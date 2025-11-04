//! Backend implementations for StreamDb.
//!
//! This module defines the `DatabaseBackend` trait and implements `MemoryBackend` and `FileBackend`.
//! `MemoryBackend` stores data in memory with a persistent Trie and WAL for crash recovery.
//! `FileBackend` provides disk-based persistence with paged storage, mmap support (non-WASM),
//! encryption, and compression. Both use `okaywal` for write-ahead logging to ensure transaction durability.

use std::any::Any;
use std::collections::{HashMap, HashSet};
use std::fs::{File, OpenOptions};
use std::io::{self, BufReader, BufWriter, Cursor, Read, Seek, SeekFrom, Write};
use std::path::Path;
use std::sync::{Arc, atomic::{AtomicBool, AtomicU64, Ordering}};
use parking_lot::{Mutex, RwLock as PlRwLock};
#[cfg(not(target_arch = "wasm32"))]
use memmap2::{MmapMut, MmapOptions};
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use uuid::Uuid;
use crc::Crc as CrcLib;
use crc::CRC_32_ISO_HDLC;
use lru::LruCache;
use snappy;
use okaywal::{WriteAheadLog, EntryId};
#[cfg(feature = "encryption")]
use ring::aead::{Aad, LessSafeKey, Nonce, AES_256_GCM, NONCE_LEN, TAG_LEN, UnboundKey};
use tokio::sync::Mutex as TokioMutex;
use bincode::{serialize, deserialize};
use log::{debug, info, warn};
use serde::{Serialize, Deserialize};

// Placeholder definitions for types from super module (adjust as needed)
#[derive(Debug, Clone)]
pub enum StreamDbError {
    Io(io::Error),
    TransactionError(String),
    EncryptionError(String),
    InvalidInput(String),
    NotFound(String),
    InvalidData(String),
    CorruptData(String),
}

#[derive(Debug, Clone)]
pub struct Config {
    pub db_path: String,
    pub wal_path: String,
    pub page_size: u64,
    pub cache_size: usize,
    pub use_compression: bool,
    #[cfg(feature = "encryption")]
    pub encryption_key: Option<[u8; 32]>,
}

#[derive(Debug, Clone)]
pub struct CacheStats {
    pub hits: u64,
    pub misses: u64,
}

#[derive(Debug, Clone)]
pub struct TrieNode<T: Clone> {
    children: HashMap<char, TrieNode<T>>,
    value: Option<T>,
    is_end: bool,
}

impl<T: Clone> TrieNode<T> {
    pub fn new() -> Self {
        TrieNode {
            children: HashMap::new(),
            value: None,
            is_end: false,
        }
    }

    pub fn insert(&self, key: &str, value: T) -> Self {
        let mut node = self.clone();
        let mut current = &mut node;
        for ch in key.chars() {
            current = current.children.entry(ch).or_insert_with(Self::new);
        }
        current.value = Some(value);
        current.is_end = true;
        node
    }

    pub fn remove(&self, key: &str) -> Self {
        let mut node = self.clone();
        if let Some(current) = Self::remove_recursive(&mut node, key, 0) {
            current.is_end = false;
            current.value = None;
        }
        node
    }

    fn remove_recursive(node: &mut TrieNode<T>, key: &str, depth: usize) -> Option<&mut TrieNode<T>> {
        if depth == key.len() {
            return Some(node);
        }
        let ch = key.chars().nth(depth)?;
        if let Some(child) = node.children.get_mut(&ch) {
            let result = Self::remove_recursive(child, key, depth + 1);
            if let Some(child_node) = result {
                if child_node.children.is_empty() && !child_node.is_end {
                    node.children.remove(&ch);
                }
            }
            Some(node)
        } else {
            None
        }
    }

    pub fn get(&self, key: &str) -> Option<T> {
        let mut current = self;
        for ch in key.chars() {
            current = current.children.get(&ch)?;
        }
        current.value.clone()
    }

    pub fn search(&self, prefix: &str, results: &mut Vec<String>, current_path: String) {
        if self.is_end {
            results.push(current_path.chars().rev().collect());
        }
        for (&ch, child) in &self.children {
            let mut new_path = current_path.clone();
            new_path.push(ch);
            child.search(prefix, results, new_path);
        }
    }
}

const MAGIC: [u8; 8] = [0x55, 0xAA, 0xFE, 0xED, 0xFA, 0xCE, 0xDA, 0x7A];
const DEFAULT_PAGE_RAW_SIZE: u64 = 4096;
const DEFAULT_PAGE_HEADER_SIZE: u64 = 32;
const FREE_LIST_HEADER_SIZE: u64 = 12;
const FREE_LIST_ENTRIES_PER_PAGE: usize = ((DEFAULT_PAGE_RAW_SIZE - DEFAULT_PAGE_HEADER_SIZE - FREE_LIST_HEADER_SIZE) / 8) as usize;
const BATCH_GROW_PAGES: u64 = 16;
const MAX_CONSECUTIVE_EMPTY_FREE_LIST: u64 = 5;

const FLAG_DATA_PAGE: u8 = 0b00000001;
const FLAG_FREE_LIST_PAGE: u8 = 0b00000100;
const FLAG_INDEX_PAGE: u8 = 0b00001000;

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
struct VersionedLink {
    page_id: i64,
    version: i32,
}

#[derive(Debug)]
struct DatabaseHeader {
    magic: [u8; 8],
    index_root: VersionedLink,
    path_lookup_root: VersionedLink,
    free_list_root: VersionedLink,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PageHeader {
    crc: u32,
    version: i32,
    prev_page_id: i64,
    next_page_id: i64,
    flags: u8,
    data_length: i32,
    padding: [u8; 3],
}

#[derive(Debug, Serialize, Deserialize)]
struct FreeListPage {
    next_free_list_page: i64,
    used_entries: i32,
    free_page_ids: Vec<i64>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct Document {
    id: Uuid,
    first_page_id: i64,
    current_version: i32,
    paths: Vec<String>,
}

pub trait DatabaseBackend: Send + Sync + Any {
    fn write_document(&mut self, data: &mut dyn Read) -> Result<Uuid, StreamDbError>;
    fn read_document(&self, id: Uuid) -> Result<Vec<u8>, StreamDbError>;
    fn read_document_quick(&self, id: Uuid, quick: bool) -> Result<Vec<u8>, StreamDbError>;
    fn delete_document(&mut self, id: Uuid) -> Result<(), StreamDbError>;
    fn bind_path_to_document(&mut self, path: &str, id: Uuid) -> Result<Uuid, StreamDbError>;
    fn get_document_id_by_path(&self, path: &str) -> Result<Uuid, StreamDbError>;
    fn search_paths(&self, prefix: &str) -> Result<Vec<String>, StreamDbError>;
    fn list_paths_for_document(&self, id: Uuid) -> Result<Vec<String>, StreamDbError>;
    fn flush(&self) -> Result<(), StreamDbError>;
    fn calculate_statistics(&self) -> Result<(i64, i64), StreamDbError>;
    fn set_quick_mode(&mut self, enabled: bool);
    fn begin_transaction(&mut self) -> Result<(), StreamDbError>;
    fn commit_transaction(&mut self) -> Result<(), StreamDbError>;
    fn rollback_transaction(&mut self) -> Result<(), StreamDbError>;
    async fn begin_async_transaction(&mut self) -> Result<(), StreamDbError>;
    async fn commit_async_transaction(&mut self) -> Result<(), StreamDbError>;
    async fn rollback_async_transaction(&mut self) -> Result<(), StreamDbError>;
    fn as_any(&self) -> &dyn Any;
}

struct MemoryBackend {
    path_trie: PlRwLock<TrieNode<Uuid>>,
    document_index: Mutex<HashMap<Uuid, Document>>,
    id_to_paths: Mutex<HashMap<Uuid, Vec<String>>>,
    data: Mutex<HashMap<Uuid, Vec<u8>>>,
    wal: WriteAheadLog,
    quick_mode: AtomicBool,
}

impl MemoryBackend {
    fn new(config: Config) -> Result<Self, StreamDbError> {
        let wal = WriteAheadLog::recover(&config.wal_path)
            .map_err(|e| StreamDbError::TransactionError(e.to_string()))?
            .unwrap_or_else(|| WriteAheadLog::open(&config.wal_path).expect("Failed to open WAL"));
        let mut backend = MemoryBackend {
            path_trie: PlRwLock::new(TrieNode::new()),
            document_index: Mutex::new(HashMap::new()),
            id_to_paths: Mutex::new(HashMap::new()),
            data: Mutex::new(HashMap::new()),
            wal,
            quick_mode: AtomicBool::new(false),
        };
        backend.recover_from_wal()?;
        Ok(backend)
    }

    fn reverse_path(path: &str) -> String {
        path.chars().rev().collect()
    }

    fn recover_from_wal(&mut self) -> Result<(), StreamDbError> {
        let mut entries = self.wal.entries();
        while let Some(entry) = entries.next().map_err(|e| StreamDbError::TransactionError(e.to_string()))? {
            let data = entry.data();
            let deserialized: (String, Uuid, Option<Vec<u8>>) = deserialize(&data)
                .map_err(|e| StreamDbError::InvalidData(e.to_string()))?;
            match deserialized.0.as_str() {
                "write" => {
                    if let Some(buffer) = deserialized.2 {
                        let id = deserialized.1;
                        self.data.lock().insert(id, buffer);
                        self.document_index.lock().insert(id, Document {
                            id,
                            first_page_id: -1,
                            current_version: 1,
                            paths: Vec::new(),
                        });
                    }
                }
                "delete" => {
                    let id = deserialized.1;
                    self.data.lock().remove(&id);
                    self.document_index.lock().remove(&id);
                    self.id_to_paths.lock().remove(&id);
                }
                "bind" => {
                    if let Some(path_bytes) = deserialized.2 {
                        let path = String::from_utf8(path_bytes)
                            .map_err(|e| StreamDbError::InvalidData(e.to_string()))?;
                        let id = deserialized.1;
                        let mut index = self.document_index.lock();
                        if let Some(document) = index.get_mut(&id) {
                            document.paths.push(path.clone());
                        } else {
                            index.insert(id, Document {
                                id,
                                first_page_id: -1,
                                current_version: 1,
                                paths: vec![path.clone()],
                            });
                        }
                        let reversed = Self::reverse_path(&path);
                        let mut trie = self.path_trie.write();
                        *trie = trie.insert(&reversed, id);
                        self.id_to_paths.lock().entry(id).or_insert_with(Vec::new).push(path);
                    }
                }
                _ => warn!("Unknown WAL entry type: {}", deserialized.0),
            }
        }
        Ok(())
    }

    fn validate_path(&self, path: &str) -> Result<(), StreamDbError> {
        if path.is_empty() || path.contains('\0') {
            Err(StreamDbError::InvalidInput("Path cannot be empty or contain null bytes".to_string()))
        } else {
            Ok(())
        }
    }
}

impl DatabaseBackend for MemoryBackend {
    fn write_document(&mut self, data: &mut dyn Read) -> Result<Uuid, StreamDbError> {
        let id = Uuid::new_v4();
        let mut buffer = Vec::new();
        data.read_to_end(&mut buffer).map_err(StreamDbError::Io)?;
        self.wal.append(&serialize(&("write", id, Some(buffer.clone())))
            .map_err(|e| StreamDbError::InvalidData(e.to_string()))?)
            .map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        self.data.lock().insert(id, buffer);
        self.document_index.lock().insert(id, Document {
            id,
            first_page_id: -1,
            current_version: 1,
            paths: Vec::new(),
        });
        Ok(id)
    }

    fn read_document(&self, id: Uuid) -> Result<Vec<u8>, StreamDbError> {
        self.data.lock()
            .get(&id)
            .cloned()
            .ok_or(StreamDbError::NotFound(format!("Document not found: {}", id)))
    }

    fn read_document_quick(&self, id: Uuid, _quick: bool) -> Result<Vec<u8>, StreamDbError> {
        self.read_document(id)
    }

    fn delete_document(&mut self, id: Uuid) -> Result<(), StreamDbError> {
        info!("Deleting document ID: {}", id);
        self.wal.append(&serialize(&("delete", id, None::<Vec<u8>>))
            .map_err(|e| StreamDbError::InvalidData(e.to_string()))?)
            .map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        let mut index = self.document_index.lock();
        index.remove(&id).ok_or(StreamDbError::NotFound(format!("Document not found: {}", id)))?;
        self.data.lock().remove(&id);
        let paths = self.id_to_paths.lock().remove(&id).unwrap_or_default();
        let mut trie = self.path_trie.write();
        for path in paths {
            let reversed = Self::reverse_path(&path);
            *trie = trie.remove(&reversed);
        }
        Ok(())
    }

    fn bind_path_to_document(&mut self, path: &str, id: Uuid) -> Result<Uuid, StreamDbError> {
        self.validate_path(path)?;
        info!("Binding path {} to ID: {}", path, id);
        let mut index = self.document_index.lock();
        let mut document = index.get(&id)
            .cloned()
            .ok_or(StreamDbError::NotFound(format!("Document not found: {}", id)))?;
        if self.get_document_id_by_path(path).is_ok() {
            return Err(StreamDbError::InvalidInput(format!("Path already bound: {}", path)));
        }
        document.paths.push(path.to_string());
        index.insert(id, document);
        let reversed = Self::reverse_path(path);
        let mut trie = self.path_trie.write();
        *trie = trie.insert(&reversed, id);
        self.wal.append(&serialize(&("bind", id, Some(path.as_bytes().to_vec())))
            .map_err(|e| StreamDbError::InvalidData(e.to_string()))?)
            .map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        self.id_to_paths.lock().entry(id).or_insert_with(Vec::new).push(path.to_string());
        Ok(id)
    }

    fn get_document_id_by_path(&self, path: &str) -> Result<Uuid, StreamDbError> {
        self.validate_path(path)?;
        debug!("Getting ID for path: {}", path);
        let reversed = Self::reverse_path(path);
        let trie = self.path_trie.read();
        trie.get(&reversed).ok_or(StreamDbError::NotFound(format!("Path not found: {}", path)))
    }

    fn search_paths(&self, prefix: &str) -> Result<Vec<String>, StreamDbError> {
        self.validate_path(prefix)?;
        debug!("Searching paths with prefix: {}", prefix);
        let reversed_prefix = Self::reverse_path(prefix);
        let trie = self.path_trie.read();
        let mut results = vec![];
        trie.search(&reversed_prefix, &mut results, String::new());
        Ok(results)
    }

    fn list_paths_for_document(&self, id: Uuid) -> Result<Vec<String>, StreamDbError> {
        debug!("Listing paths for document ID: {}", id);
        let paths = self.id_to_paths.lock()
            .get(&id)
            .cloned()
            .ok_or(StreamDbError::NotFound(format!("Document not found: {}", id)))?;
        Ok(paths)
    }

    fn flush(&self) -> Result<(), StreamDbError> {
        self.wal.checkpoint().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        Ok(())
    }

    fn calculate_statistics(&self) -> Result<(i64, i64), StreamDbError> {
        let total = self.data.lock().len() as i64;
        Ok((total, 0)) // MemoryBackend has no free pages
    }

    fn set_quick_mode(&mut self, enabled: bool) {
        self.quick_mode.store(enabled, Ordering::Relaxed);
    }

    fn begin_transaction(&mut self) -> Result<(), StreamDbError> {
        self.wal.begin().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        Ok(())
    }

    fn commit_transaction(&mut self) -> Result<(), StreamDbError> {
        self.wal.commit().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        Ok(())
    }

    fn rollback_transaction(&mut self) -> Result<(), StreamDbError> {
        self.wal.rollback().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        Ok(())
    }

    async fn begin_async_transaction(&mut self) -> Result<(), StreamDbError> {
        self.wal.begin().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        Ok(())
    }

    async fn commit_async_transaction(&mut self) -> Result<(), StreamDbError> {
        self.wal.commit().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        Ok(())
    }

    async fn rollback_async_transaction(&mut self) -> Result<(), StreamDbError> {
        self.wal.rollback().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        Ok(())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

struct FileBackend {
    config: Config,
    file: TokioMutex<File>,
    #[cfg(not(target_arch = "wasm32"))]
    mmap: PlRwLock<Option<MmapMut>>,
    header: DatabaseHeader,
    document_index: Mutex<HashMap<Uuid, Document>>,
    path_trie: PlRwLock<TrieNode<Uuid>>,
    id_to_paths: Mutex<HashMap<Uuid, Vec<String>>>,
    quick_mode: AtomicBool,
    wal: WriteAheadLog,
    page_cache: Mutex<LruCache<i64, Vec<u8>>>,
    batch_cache: Mutex<HashMap<i64, Vec<u8>>>,
    batch_lock: TokioMutex<()>,
    batch_size: AtomicU64,
    compression: bool,
    #[cfg(feature = "encryption")]
    encryption_key: Option<LessSafeKey>,
    nonce: AtomicU64,
    crc: CrcLib<u32>,
    batch_grow_threshold: u64,
    consecutive_empty_free_list: AtomicU64,
    free_list_root: Mutex<VersionedLink>,
    current_size: AtomicU64,
}

impl FileBackend {
    fn new(config: Config) -> Result<Self, StreamDbError> {
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&config.db_path)
            .map_err(StreamDbError::Io)?;
        let file_len = file.metadata().map_err(StreamDbError::Io)?.len();
        let wal = WriteAheadLog::recover(&config.wal_path)
            .map_err(|e| StreamDbError::TransactionError(e.to_string()))?
            .unwrap_or_else(|| WriteAheadLog::open(&config.wal_path).expect("Failed to open WAL"));
        let mut backend = FileBackend {
            config: config.clone(),
            file: TokioMutex::new(file),
            #[cfg(not(target_arch = "wasm32"))]
            mmap: PlRwLock::new(None),
            header: DatabaseHeader {
                magic: MAGIC,
                index_root: VersionedLink { page_id: -1, version: 0 },
                path_lookup_root: VersionedLink { page_id: -1, version: 0 },
                free_list_root: VersionedLink { page_id: -1, version: 0 },
            },
            document_index: Mutex::new(HashMap::new()),
            path_trie: PlRwLock::new(TrieNode::new()),
            id_to_paths: Mutex::new(HashMap::new()),
            quick_mode: AtomicBool::new(false),
            wal,
            page_cache: Mutex::new(LruCache::new(config.cache_size)),
            batch_cache: Mutex::new(HashMap::new()),
            batch_lock: TokioMutex::new(()),
            batch_size: AtomicU64::new(0),
            compression: config.use_compression,
            #[cfg(feature = "encryption")]
            encryption_key: None,
            nonce: AtomicU64::new(0),
            crc: CrcLib::<u32>::new(&CRC_32_ISO_HDLC),
            batch_grow_threshold: BATCH_GROW_PAGES * config.page_size,
            consecutive_empty_free_list: AtomicU64::new(0),
            free_list_root: Mutex::new(VersionedLink { page_id: -1, version: 0 }),
            current_size: AtomicU64::new(file_len),
        };
        #[cfg(not(target_arch = "wasm32"))]
        if file_len > 0 {
            let file = backend.file.blocking_lock();
            let mmap = unsafe { MmapOptions::new().map_mut(&*file).map_err(StreamDbError::Io)? };
            *backend.mmap.write() = Some(mmap);
        }
        #[cfg(feature = "encryption")]
        if let Some(key) = &config.encryption_key {
            let unbound_key = UnboundKey::new(&AES_256_GCM, key)
                .map_err(|_| StreamDbError::EncryptionError("Invalid encryption key".to_string()))?;
            backend.encryption_key = Some(LessSafeKey::new(unbound_key));
        }
        if file_len > 0 {
            backend.read_header()?;
        } else {
            backend.write_header()?;
        }
        backend.recover_from_wal()?;
        Ok(backend)
    }

    fn read_header(&mut self) -> Result<(), StreamDbError> {
        let mut file = self.file.blocking_lock();
        file.seek(SeekFrom::Start(0)).map_err(StreamDbError::Io)?;
        let mut magic = [0u8; 8];
        file.read_exact(&mut magic).map_err(StreamDbError::Io)?;
        if magic != MAGIC {
            return Err(StreamDbError::CorruptData("Invalid database magic".to_string()));
        }
        let index_root = VersionedLink {
            page_id: file.read_i64::<LittleEndian>().map_err(StreamDbError::Io)?,
            version: file.read_i32::<LittleEndian>().map_err(StreamDbError::Io)?,
        };
        let path_lookup_root = VersionedLink {
            page_id: file.read_i64::<LittleEndian>().map_err(StreamDbError::Io)?,
            version: file.read_i32::<LittleEndian>().map_err(StreamDbError::Io)?,
        };
        let free_list_root = VersionedLink {
            page_id: file.read_i64::<LittleEndian>().map_err(StreamDbError::Io)?,
            version: file.read_i32::<LittleEndian>().map_err(StreamDbError::Io)?,
        };
        self.header = DatabaseHeader {
            magic,
            index_root,
            path_lookup_root,
            free_list_root,
        };
        *self.free_list_root.lock() = free_list_root;
        Ok(())
    }

    fn write_header(&mut self) -> Result<(), StreamDbError> {
        let mut file = self.file.blocking_lock();
        file.seek(SeekFrom::Start(0)).map_err(StreamDbError::Io)?;
        file.write_all(&self.header.magic).map_err(StreamDbError::Io)?;
        file.write_i64::<LittleEndian>(self.header.index_root.page_id).map_err(StreamDbError::Io)?;
        file.write_i32::<LittleEndian>(self.header.index_root.version).map_err(StreamDbError::Io)?;
        file.write_i64::<LittleEndian>(self.header.path_lookup_root.page_id).map_err(StreamDbError::Io)?;
        file.write_i32::<LittleEndian>(self.header.path_lookup_root.version).map_err(StreamDbError::Io)?;
        file.write_i64::<LittleEndian>(self.header.free_list_root.page_id).map_err(StreamDbError::Io)?;
        file.write_i32::<LittleEndian>(self.header.free_list_root.version).map_err(StreamDbError::Io)?;
        file.flush().map_err(StreamDbError::Io)?;
        Ok(())
    }

    fn read_page_header(&self, page_id: i64) -> Result<PageHeader, StreamDbError> {
        let offset = page_id as u64 * self.config.page_size;
        let mut file = self.file.blocking_lock();
        file.seek(SeekFrom::Start(offset)).map_err(StreamDbError::Io)?;
        let crc = file.read_u32::<LittleEndian>().map_err(StreamDbError::Io)?;
        let version = file.read_i32::<LittleEndian>().map_err(StreamDbError::Io)?;
        let prev_page_id = file.read_i64::<LittleEndian>().map_err(StreamDbError::Io)?;
        let next_page_id = file.read_i64::<LittleEndian>().map_err(StreamDbError::Io)?;
        let flags = file.read_u8().map_err(StreamDbError::Io)?;
        let data_length = file.read_i32::<LittleEndian>().map_err(StreamDbError::Io)?;
        let mut padding = [0u8; 3];
        file.read_exact(&mut padding).map_err(StreamDbError::Io)?;
        Ok(PageHeader {
            crc,
            version,
            prev_page_id,
            next_page_id,
            flags,
            data_length,
            padding,
        })
    }

    fn write_page_header(&self, page_id: i64, header: &PageHeader) -> Result<(), StreamDbError> {
        let offset = page_id as u64 * self.config.page_size;
        let mut file = self.file.blocking_lock();
        file.seek(SeekFrom::Start(offset)).map_err(StreamDbError::Io)?;
        file.write_u32::<LittleEndian>(header.crc).map_err(StreamDbError::Io)?;
        file.write_i32::<LittleEndian>(header.version).map_err(StreamDbError::Io)?;
        file.write_i64::<LittleEndian>(header.prev_page_id).map_err(StreamDbError::Io)?;
        file.write_i64::<LittleEndian>(header.next_page_id).map_err(StreamDbError::Io)?;
        file.write_u8(header.flags).map_err(StreamDbError::Io)?;
        file.write_i32::<LittleEndian>(header.data_length).map_err(StreamDbError::Io)?;
        file.write_all(&header.padding).map_err(StreamDbError::Io)?;
        file.flush().map_err(StreamDbError::Io)?;
        Ok(())
    }

    fn read_raw_page(&self, page_id: i64) -> Result<Vec<u8>, StreamDbError> {
        if let Some(cached) = self.page_cache.lock().get(&page_id) {
            return Ok(cached.clone());
        }
        let header = self.read_page_header(page_id)?;
        let offset_data = page_id as u64 * self.config.page_size + DEFAULT_PAGE_HEADER_SIZE;
        let data_length = header.data_length as usize;
        let mut data = vec![0u8; data_length];
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(mmap) = self.mmap.read().as_ref() {
            // Fixed: removed mut (line 804)
            let start = offset_data as usize;
            if start + data_length <= mmap.len() {
                data.copy_from_slice(&mmap[start..start + data_length]);
            } else {
                return Err(StreamDbError::CorruptData("Mmap access out of bounds".to_string()));
            }
        } else {
            let mut file = self.file.blocking_lock();
            file.seek(SeekFrom::Start(offset_data)).map_err(StreamDbError::Io)?;
            file.read_exact(&mut data).map_err(StreamDbError::Io)?;
        }
        #[cfg(target_arch = "wasm32")]
        {
            let mut file = self.file.blocking_lock();
            file.seek(SeekFrom::Start(offset_data)).map_err(StreamDbError::Io)?;
            file.read_exact(&mut data).map_err(StreamDbError::Io)?;
        }
        #[cfg(feature = "encryption")]
        if header.flags & FLAG_DATA_PAGE != 0 {
            if let Some(key) = &self.encryption_key {
                let nonce = Nonce::try_assume_unique_for_key(&Self::derive_nonce(page_id))
                    .map_err(|e| StreamDbError::EncryptionError(e.to_string()))?;
                let mut in_out = data.clone();
                let decrypted = key.open_in_place(nonce, Aad::empty(), &mut in_out)
                    .map_err(|e| StreamDbError::EncryptionError(e.to_string()))?;
                data = decrypted[..decrypted.len() - TAG_LEN].to_vec();
            }
        }
        if self.compression && header.flags & FLAG_DATA_PAGE != 0 {
            data = snappy::uncompress(&data).map_err(|e| StreamDbError::InvalidData(e.to_string()))?;
        }
        if !self.quick_mode.load(Ordering::Relaxed) {
            let computed_crc = self.crc.checksum(&data);
            if computed_crc != header.crc {
                return Err(StreamDbError::CorruptData(format!("CRC mismatch on page {}", page_id)));
            }
        }
        self.page_cache.lock().put(page_id, data.clone());
        Ok(data)
    }

    fn write_raw_page(&mut self, page_id: i64, data: &[u8], flags: u8) -> Result<(), StreamDbError> {
        let _lock = self.batch_lock.blocking_lock();
        let mut data = data.to_vec();
        let crc = self.crc.checksum(&data);
        if self.compression && flags & FLAG_DATA_PAGE != 0 {
            data = snappy::compress(&data).map_err(|e| StreamDbError::InvalidData(e.to_string()))?;
        }
        #[cfg(feature = "encryption")]
        if flags & FLAG_DATA_PAGE != 0 {
            if let Some(key) = &self.encryption_key {
                let nonce_val = self.nonce.fetch_add(1, Ordering::SeqCst);
                let nonce = Nonce::try_assume_unique_for_key(&nonce_val.to_le_bytes()[..NONCE_LEN])
                    .map_err(|e| StreamDbError::EncryptionError(e.to_string()))?;
                key.seal_in_place_append_tag(nonce, Aad::empty(), &mut data)
                    .map_err(|e| StreamDbError::EncryptionError(e.to_string()))?;
            }
        }
        let header = PageHeader {
            crc,
            version: 1,
            prev_page_id: -1,
            next_page_id: -1,
            flags,
            data_length: data.len() as i32,
            padding: [0; 3],
        };
        self.write_page_header(page_id, &header)?;
        let offset_data = page_id as u64 * self.config.page_size + DEFAULT_PAGE_HEADER_SIZE;
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(mmap) = self.mmap.write().as_mut() {
            let start = offset_data as usize;
            if start + data.len() <= mmap.len() {
                mmap[start..start + data.len()].copy_from_slice(&data);
                mmap.flush().map_err(StreamDbError::Io)?;
            } else {
                let mut file = self.file.blocking_lock();
                file.seek(SeekFrom::Start(offset_data)).map_err(StreamDbError::Io)?;
                file.write_all(&data).map_err(StreamDbError::Io)?;
                file.flush().map_err(StreamDbError::Io)?;
            }
        } else {
            let mut file = self.file.blocking_lock();
            file.seek(SeekFrom::Start(offset_data)).map_err(StreamDbError::Io)?;
            file.write_all(&data).map_err(StreamDbError::Io)?;
            file.flush().map_err(StreamDbError::Io)?;
        }
        #[cfg(target_arch = "wasm32")]
        {
            let mut file = self.file.blocking_lock();
            file.seek(SeekFrom::Start(offset_data)).map_err(StreamDbError::Io)?;
            file.write_all(&data).map_err(StreamDbError::Io)?;
            file.flush().map_err(StreamDbError::Io)?;
        }
        self.page_cache.lock().put(page_id, data);
        self.batch_cache.lock().insert(page_id, data);
        self.batch_size.fetch_add(self.config.page_size, Ordering::Relaxed);
        if self.batch_size.load(Ordering::Relaxed) >= self.batch_grow_threshold {
            self.flush_batch()?;
        }
        Ok(())
    }

    fn flush_batch(&mut self) -> Result<(), StreamDbError> {
        let _lock = self.batch_lock.blocking_lock();
        let batch = self.batch_cache.lock().drain().collect::<Vec<_>>();
        for (page_id, data) in batch {
            self.write_raw_page(page_id, &data, FLAG_DATA_PAGE)?;
        }
        self.batch_size.store(0, Ordering::Relaxed);
        Ok(())
    }

    fn allocate_page(&mut self) -> Result<i64, StreamDbError> {
        // Fixed borrow checker issue: release lock early
        let free_root_page_id;
        let free_root_version;
        {
            let free_root = self.free_list_root.lock(); // Immutable borrow
            free_root_page_id = free_root.page_id;
            free_root_version = free_root.version;
        } // Lock released here

        if free_root_page_id != -1 {
            let mut free_list = self.read_free_list_page(free_root_page_id)?;
            if !free_list.free_page_ids.is_empty() {
                let page_id = free_list.free_page_ids.pop().unwrap();
                free_list.used_entries -= 1;
                self.write_free_list_page(free_root_page_id, &free_list)?;
                if free_list.used_entries == 0 {
                    self.consecutive_empty_free_list.fetch_add(1, Ordering::Relaxed);
                    if self.consecutive_empty_free_list.load(Ordering::Relaxed) >= MAX_CONSECUTIVE_EMPTY_FREE_LIST {
                        let new_free_root = VersionedLink {
                            page_id: free_list.next_free_list_page,
                            version: free_root_version + 1,
                        };
                        *self.free_list_root.lock() = new_free_root;
                        self.header.free_list_root = new_free_root;
                        self.write_header()?;
                    }
                }
                return Ok(page_id);
            }
        }

        self.consecutive_empty_free_list.store(0, Ordering::Relaxed);
        let mut file = self.file.blocking_lock();
        let len = file.metadata().map_err(StreamDbError::Io)?.len();
        let page_id = (len / self.config.page_size) as i64;
        file.set_len(len + self.config.page_size).map_err(StreamDbError::Io)?;
        #[cfg(not(target_arch = "wasm32"))]
        {
            *self.mmap.write() = Some(unsafe {
                MmapOptions::new().map_mut(&*file).map_err(StreamDbError::Io)?
            });
        }
        self.current_size.store(len + self.config.page_size, Ordering::Relaxed);
        Ok(page_id)
    }

    fn read_free_list_page(&self, page_id: i64) -> Result<FreeListPage, StreamDbError> {
        let data = self.read_raw_page(page_id)?;
        let mut cursor = Cursor::new(data);
        let next_free_list_page = cursor.read_i64::<LittleEndian>().map_err(StreamDbError::Io)?;
        let used_entries = cursor.read_i32::<LittleEndian>().map_err(StreamDbError::Io)?;
        let mut free_page_ids = Vec::new();
        for _ in 0..used_entries {
            free_page_ids.push(cursor.read_i64::<LittleEndian>().map_err(StreamDbError::Io)?);
        }
        Ok(FreeListPage {
            next_free_list_page,
            used_entries,
            free_page_ids,
        })
    }

    fn write_free_list_page(&mut self, page_id: i64, free_list: &FreeListPage) -> Result<(), StreamDbError> {
        let mut data = Vec::new();
        let mut cursor = Cursor::new(&mut data);
        cursor.write_i64::<LittleEndian>(free_list.next_free_list_page).map_err(StreamDbError::Io)?;
        cursor.write_i32::<LittleEndian>(free_list.used_entries).map_err(StreamDbError::Io)?;
        for &id in &free_list.free_page_ids {
            cursor.write_i64::<LittleEndian>(id).map_err(StreamDbError::Io)?;
        }
        self.write_raw_page(page_id, &data, FLAG_FREE_LIST_PAGE)?;
        Ok(())
    }

    fn free_page(&mut self, page_id: i64) -> Result<(), StreamDbError> {
        let free_root_page_id;
        let free_root_version;
        {
            let free_root = self.free_list_root.lock(); // Fixed: removed mut
            free_root_page_id = free_root.page_id;
            free_root_version = free_root.version;
        } // Lock released

        let mut free_list = if free_root_page_id != -1 {
            self.read_free_list_page(free_root_page_id)?
        } else {
            FreeListPage {
                next_free_list_page: -1,
                used_entries: 0,
                free_page_ids: Vec::new(),
            }
        };
        if free_list.used_entries as usize >= FREE_LIST_ENTRIES_PER_PAGE {
            let new_page_id = self.allocate_page()?;
            let new_free_list = FreeListPage {
                next_free_list_page: free_root_page_id,
                used_entries: 1,
                free_page_ids: vec![page_id],
            };
            self.write_free_list_page(new_page_id, &new_free_list)?;
            let new_free_root = VersionedLink {
                page_id: new_page_id,
                version: free_root_version + 1,
            };
            *self.free_list_root.lock() = new_free_root;
            self.header.free_list_root = new_free_root;
            self.write_header()?;
        } else {
            free_list.free_page_ids.push(page_id);
            free_list.used_entries += 1;
            self.write_free_list_page(free_root_page_id, &free_list)?;
        }
        self.consecutive_empty_free_list.store(0, Ordering::Relaxed);
        self.page_cache.lock().remove(&page_id);
        Ok(())
    }

    fn count_free_pages(&self) -> Result<i64, StreamDbError> {
        let mut count = 0;
        let mut current_page_id = self.free_list_root.lock().page_id;
        while current_page_id != -1 {
            let free_list = self.read_free_list_page(current_page_id)?;
            count += free_list.used_entries as i64;
            current_page_id = free_list.next_free_list_page;
        }
        Ok(count)
    }

    fn remove_from_index(&mut self, id: Uuid) -> Result<(), StreamDbError> {
        let mut trie = self.path_trie.write();
        let paths = self.id_to_paths.lock().remove(&id).unwrap_or_default();
        for path in paths {
            let reversed = MemoryBackend::reverse_path(&path);
            *trie = trie.remove(&reversed);
        }
        Ok(())
    }

    fn validate_path(&self, path: &str) -> Result<(), StreamDbError> {
        if path.is_empty() || path.contains('\0') {
            Err(StreamDbError::InvalidInput("Path cannot be empty or contain null bytes".to_string()))
        } else {
            Ok(())
        }
    }

    #[cfg(feature = "encryption")]
    fn derive_nonce(page_id: i64) -> [u8; NONCE_LEN] {
        let mut nonce = [0u8; NONCE_LEN];
        nonce[..8].copy_from_slice(&page_id.to_le_bytes());
        nonce
    }

    fn recover_from_wal(&mut self) -> Result<(), StreamDbError> {
        let mut entries = self.wal.entries();
        while let Some(entry) = entries.next().map_err(|e| StreamDbError::TransactionError(e.to_string()))? {
            let data = entry.data();
            let deserialized: (String, Uuid, Option<Vec<u8>>) = deserialize(&data)
                .map_err(|e| StreamDbError::InvalidData(e.to_string()))?;
            match deserialized.0.as_str() {
                "write" => {
                    if let Some(buffer) = deserialized.2 {
                        let id = deserialized.1;
                        let page_id = self.allocate_page()?;
                        self.write_raw_page(page_id, &buffer, FLAG_DATA_PAGE)?;
                        self.document_index.lock().insert(id, Document {
                            id,
                            first_page_id: page_id,
                            current_version: 1,
                            paths: Vec::new(),
                        });
                    }
                }
                "delete" => {
                    let id = deserialized.1;
                    let _ = self.delete_document(id); // Ignore errors during recovery
                }
                "bind" => {
                    if let Some(path_bytes) = deserialized.2 {
                        let path = String::from_utf8(path_bytes)
                            .map_err(|e| StreamDbError::InvalidData(e.to_string()))?;
                        let id = deserialized.1;
                        let _ = self.bind_path_to_document(&path, id); // Ignore errors during recovery
                    }
                }
                _ => warn!("Unknown WAL entry type: {}", deserialized.0),
            }
        }
        Ok(())
    }
}

impl DatabaseBackend for FileBackend {
    fn write_document(&mut self, data: &mut dyn Read) -> Result<Uuid, StreamDbError> {
        let id = Uuid::new_v4();
        let mut buffer = Vec::new();
        data.read_to_end(&mut buffer).map_err(StreamDbError::Io)?;
        self.wal.append(&serialize(&("write", id, Some(buffer.clone())))
            .map_err(|e| StreamDbError::InvalidData(e.to_string()))?)
            .map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        let page_id = self.allocate_page()?;
        self.write_raw_page(page_id, &buffer, FLAG_DATA_PAGE)?;
        self.document_index.lock().insert(id, Document {
            id,
            first_page_id: page_id,
            current_version: 1,
            paths: Vec::new(),
        });
        Ok(id)
    }

    fn read_document(&self, id: Uuid) -> Result<Vec<u8>, StreamDbError> {
        let quick = self.quick_mode.load(Ordering::Relaxed);
        self.read_document_quick(id, quick)
    }

    fn read_document_quick(&self, id: Uuid, quick: bool) -> Result<Vec<u8>, StreamDbError> {
        info!("Reading document ID: {}, quick: {}", id, quick);
        let orig_quick_mode = self.quick_mode.load(Ordering::Relaxed);
        self.quick_mode.store(quick, Ordering::Relaxed);
        let index = self.document_index.lock();
        let document = index.get(&id)
            .ok_or(StreamDbError::NotFound(format!("Document not found: {}", id)))?;
        let mut result = Vec::new();
        let mut current_page_id = document.first_page_id;
        while current_page_id != -1 {
            let data = self.read_raw_page(current_page_id)?;
            result.extend_from_slice(&data);
            let header = self.read_page_header(current_page_id)?;
            current_page_id = header.next_page_id;
        }
        self.quick_mode.store(orig_quick_mode, Ordering::Relaxed);
        Ok(result)
    }

    fn delete_document(&mut self, id: Uuid) -> Result<(), StreamDbError> {
        info!("Deleting document ID: {}", id);
        self.wal.append(&serialize(&("delete", id, None::<Vec<u8>>))
            .map_err(|e| StreamDbError::InvalidData(e.to_string()))?)
            .map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        let mut index = self.document_index.lock();
        let document = index.remove(&id)
            .ok_or(StreamDbError::NotFound(format!("Document not found: {}", id)))?;
        let mut current_page_id = document.first_page_id;
        while current_page_id != -1 {
            let header = self.read_page_header(current_page_id)?;
            self.free_page(current_page_id)?;
            current_page_id = header.next_page_id;
        }
        self.remove_from_index(id)?;
        Ok(())
    }

    fn bind_path_to_document(&mut self, path: &str, id: Uuid) -> Result<Uuid, StreamDbError> {
        self.validate_path(path)?;
        info!("Binding path {} to ID: {}", path, id);
        let mut index = self.document_index.lock();
        let mut document = index.get(&id)
            .cloned()
            .ok_or(StreamDbError::NotFound(format!("Document not found: {}", id)))?;
        if self.get_document_id_by_path(path).is_ok() {
            return Err(StreamDbError::InvalidInput(format!("Path already bound: {}", path)));
        }
        document.paths.push(path.to_string());
        index.insert(id, document);
        let reversed = MemoryBackend::reverse_path(path);
        let mut trie = self.path_trie.write();
        *trie = trie.insert(&reversed, id);
        self.wal.append(&serialize(&("bind", id, Some(path.as_bytes().to_vec())))
            .map_err(|e| StreamDbError::InvalidData(e.to_string()))?)
            .map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        self.id_to_paths.lock().entry(id).or_insert_with(Vec::new).push(path.to_string());
        Ok(id)
    }

    fn get_document_id_by_path(&self, path: &str) -> Result<Uuid, StreamDbError> {
        self.validate_path(path)?;
        debug!("Getting ID for path: {}", path);
        let reversed = MemoryBackend::reverse_path(path);
        let trie = self.path_trie.read();
        trie.get(&reversed).ok_or(StreamDbError::NotFound(format!("Path not found: {}", path)))
    }

    fn search_paths(&self, prefix: &str) -> Result<Vec<String>, StreamDbError> {
        self.validate_path(prefix)?;
        debug!("Searching paths with prefix: {}", prefix);
        let reversed_prefix = MemoryBackend::reverse_path(prefix);
        let trie = self.path_trie.read();
        let mut results = vec![];
        trie.search(&reversed_prefix, &mut results, String::new());
        Ok(results)
    }

    fn list_paths_for_document(&self, id: Uuid) -> Result<Vec<String>, StreamDbError> {
        debug!("Listing paths for document ID: {}", id);
        let paths = self.id_to_paths.lock()
            .get(&id)
            .cloned()
            .ok_or(StreamDbError::NotFound(format!("Document not found: {}", id)))?;
        Ok(paths)
    }

    fn flush(&self) -> Result<(), StreamDbError> {
        self.wal.checkpoint().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        let mut file = self.file.blocking_lock();
        file.flush().map_err(StreamDbError::Io)?;
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(mmap) = self.mmap.read().as_ref() {
            mmap.flush().map_err(StreamDbError::Io)?;
        }
        Ok(())
    }

    fn calculate_statistics(&self) -> Result<(i64, i64), StreamDbError> {
        let total = (self.current_size.load(Ordering::Relaxed) / self.config.page_size) as i64;
        let free = self.count_free_pages()?;
        Ok((total - free, free))
    }

    fn set_quick_mode(&mut self, enabled: bool) {
        self.quick_mode.store(enabled, Ordering::Relaxed);
    }

    fn begin_transaction(&mut self) -> Result<(), StreamDbError> {
        self.wal.begin().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        Ok(())
    }

    fn commit_transaction(&mut self) -> Result<(), StreamDbError> {
        self.wal.commit().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        self.flush()?;
        Ok(())
    }

    fn rollback_transaction(&mut self) -> Result<(), StreamDbError> {
        self.wal.rollback().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        Ok(())
    }

    async fn begin_async_transaction(&mut self) -> Result<(), StreamDbError> {
        self.wal.begin().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        Ok(())
    }

    async fn commit_async_transaction(&mut self) -> Result<(), StreamDbError> {
        self.wal.commit().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        self.flush()?;
        Ok(())
    }

    async fn rollback_async_transaction(&mut self) -> Result<(), StreamDbError> {
        self.wal.rollback().map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        Ok(())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
