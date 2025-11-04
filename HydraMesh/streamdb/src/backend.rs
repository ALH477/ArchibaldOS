//! Backend implementations for StreamDb.
//!
//! This module defines the `DatabaseBackend` trait and implements `MemoryBackend` and `FileBackend`.
//! `MemoryBackend` stores data in memory with a persistent Trie and WAL for crash recovery.
//! `FileBackend` provides disk-based persistence with paged storage, mmap support (non-WASM),
//! encryption, and compression. Both use `okaywal` for write-ahead logging to ensure transaction durability.

use super::{StreamDbError, Config, CacheStats, TrieNode};
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
use okaywal::WriteAheadLog;
#[cfg(feature = "encryption")]
use ring::aead::{Aad, LessSafeKey, Nonce, AES_256_GCM, NONCE_LEN, TAG_LEN, UnboundKey};
use tokio::sync::Mutex as TokioMutex;
use bincode::{serialize, deserialize};
use log::{debug, info, warn};
use serde::{Serialize, Deserialize};

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

#[derive(Clone, Copy, Debug)]
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

#[derive(Debug, Clone)]
struct PageHeader {
    crc: u32,
    version: i32,
    prev_page_id: i64,
    next_page_id: i64,
    flags: u8,
    data_length: i32,
    padding: [u8; 3],
}

#[derive(Debug)]
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
    fn write_document(&self, data: &mut dyn Read) -> Result<Uuid, StreamDbError>;
    fn read_document(&self, id: Uuid) -> Result<Vec<u8>, StreamDbError>;
    fn read_document_quick(&self, id: Uuid, quick: bool) -> Result<Vec<u8>, StreamDbError>;
    fn delete_document(&self, id: Uuid) -> Result<(), StreamDbError>;
    fn bind_path_to_document(&self, path: &str, id: Uuid) -> Result<Uuid, StreamDbError>;
    fn get_document_id_by_path(&self, path: &str) -> Result<Uuid, StreamDbError>;
    fn search_paths(&self, prefix: &str) -> Result<Vec<String>, StreamDbError>;
    fn list_paths_for_document(&self, id: Uuid) -> Result<Vec<String>, StreamDbError>;
    fn count_free_pages(&self) -> Result<i64, StreamDbError>;
    fn get_info(&self, id: Uuid) -> Result<String, StreamDbError>;
    fn delete_paths_for_document(&self, id: Uuid) -> Result<(), StreamDbError>;
    fn remove_from_index(&self, id: Uuid) -> Result<(), StreamDbError>;
    fn get_cache_stats(&self) -> Result<CacheStats, StreamDbError>;
    fn get_stream(&self, id: Uuid) -> Result<Box<dyn Iterator<Item = Result<Vec<u8>, StreamDbError>> + Send + Sync>, StreamDbError>;
    fn as_any(&self) -> &dyn Any;
    fn flush(&self) -> Result<(), StreamDbError>;
    fn calculate_statistics(&self) -> Result<(i64, i64), StreamDbError>;
    fn set_quick_mode(&self, enabled: bool);
    fn begin_transaction(&self) -> Result<(), StreamDbError>;
    fn commit_transaction(&self) -> Result<(), StreamDbError>;
    fn rollback_transaction(&self) -> Result<(), StreamDbError>;
    async fn begin_async_transaction(&self) -> Result<(), StreamDbError>;
    async fn commit_async_transaction(&self) -> Result<(), StreamDbError>;
    async fn rollback_async_transaction(&self) -> Result<(), StreamDbError>;
}

pub struct MemoryBackend {
    documents: Mutex<HashMap<Uuid, Vec<u8>>>,
    path_trie: PlRwLock<TrieNode>,
    id_to_paths: Mutex<HashMap<Uuid, Vec<String>>>,
    cache_stats: Mutex<CacheStats>,
    transaction: TokioMutex<Option<HashMap<Uuid, Vec<u8>>>>,
    wal: WriteAheadLog,
}

impl MemoryBackend {
    pub fn new<P: AsRef<Path>>(wal_path: P) -> Result<Self, StreamDbError> {
        let wal = WriteAheadLog::open(wal_path).map_err(|e| StreamDbError::Io(e.into()))?;
        Ok(Self {
            documents: Mutex::new(HashMap::new()),
            path_trie: PlRwLock::new(TrieNode::new()),
            id_to_paths: Mutex::new(HashMap::new()),
            cache_stats: Mutex::new(CacheStats::default()),
            transaction: TokioMutex::new(None),
            wal,
        })
    }

    fn reverse_path(path: &str) -> Vec<u8> {
        path.chars().rev().map(|c| c as u8).collect()
    }

    fn validate_path(&self, path: &str) -> Result<(), StreamDbError> {
        if path.is_empty() || path.contains('\0') || path.contains("//") || path.len() > 1024 {
            return Err(StreamDbError::InvalidInput("Invalid path".to_string()));
        }
        Ok(())
    }
}

impl DatabaseBackend for MemoryBackend {
    fn write_document(&self, data: &mut dyn Read) -> Result<Uuid, StreamDbError> {
        let mut buffer = Vec::new();
        data.read_to_end(&mut buffer).map_err(StreamDbError::Io)?;
        let id = Uuid::new_v4();

        let mut tx = self.transaction.blocking_lock();
        if let Some(ref mut tx_data) = *tx {
            tx_data.insert(id, buffer.clone());
        } else {
            self.documents.lock().insert(id, buffer.clone());
        }
        self.wal.append(&serialize(&("write", id, buffer))?)
            .map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        Ok(id)
    }

    fn read_document(&self, id: Uuid) -> Result<Vec<u8>, StreamDbError> {
        let tx = self.transaction.blocking_lock();
        if let Some(ref tx_data) = *tx {
            if let Some(data) = tx_data.get(&id) {
                return Ok(data.clone());
            }
        }
        self.documents.lock().get(&id).cloned()
            .ok_or(StreamDbError::NotFound(format!("Document not found: {}", id)))
    }

    fn read_document_quick(&self, id: Uuid, _quick: bool) -> Result<Vec<u8>, StreamDbError> {
        self.read_document(id)
    }

    fn delete_document(&self, id: Uuid) -> Result<(), StreamDbError> {
        let mut tx = self.transaction.blocking_lock();
        if let Some(ref mut tx_data) = *tx {
            tx_data.remove(&id);
        } else {
            self.documents.lock().remove(&id);
        }
        self.wal.append(&serialize(&("delete", id, vec![]))?)
            .map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        self.delete_paths_for_document(id)?;
        Ok(())
    }

    fn bind_path_to_document(&self, path: &str, id: Uuid) -> Result<Uuid, StreamDbError> {
        self.validate_path(path)?;
        let reversed = Self::reverse_path(path);
        let mut tx = self.transaction.blocking_lock();
        let mut trie = self.path_trie.write();
        if tx.is_some() {
            self.wal.append(&serialize(&("bind", id, path.as_bytes().to_vec()))?)
                .map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        } else {
            *trie = trie.insert(&reversed, id);
            self.id_to_paths.lock().entry(id).or_insert_with(Vec::new).push(path.to_string());
            self.wal.append(&serialize(&("bind", id, path.as_bytes().to_vec()))?)
                .map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        }
        Ok(id)
    }

    fn get_document_id_by_path(&self, path: &str) -> Result<Uuid, StreamDbError> {
        self.validate_path(path)?;
        let reversed = Self::reverse_path(path);
        let trie = self.path_trie.read();
        trie.get(&reversed)
            .ok_or(StreamDbError::NotFound(format!("Path not found: {}", path)))
    }

    fn search_paths(&self, prefix: &str) -> Result<Vec<String>, StreamDbError> {
        self.validate_path(prefix)?;
        let reversed = Self::reverse_path(prefix);
        let trie = self.path_trie.read();
        let mut results = Vec::new();
        trie.search(&reversed, &mut results, String::new());
        Ok(results)
    }

    fn list_paths_for_document(&self, id: Uuid) -> Result<Vec<String>, StreamDbError> {
        self.id_to_paths.lock().get(&id).cloned()
            .ok_or(StreamDbError::NotFound(format!("No paths for ID: {}", id)))
    }

    fn count_free_pages(&self) -> Result<i64, StreamDbError> { Ok(0) }

    fn get_info(&self, id: Uuid) -> Result<String, StreamDbError> {
        let data = self.read_document(id)?;
        let paths = self.list_paths_for_document(id)?;
        Ok(format!("ID: {}, Size: {} bytes, Paths: {:?}", id, data.len(), paths))
    }

    fn delete_paths_for_document(&self, id: Uuid) -> Result<(), StreamDbError> {
        let paths = self.id_to_paths.lock().remove(&id).unwrap_or_default();
        let mut trie = self.path_trie.write();
        for path in &paths {
            let reversed = Self::reverse_path(path);
            if let Some(new_trie) = trie.remove(&reversed) {
                *trie = new_trie;
            }
            self.wal.append(&serialize(&("unbind", id, path.as_bytes().to_vec()))?)
                .map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        }
        Ok(())
    }

    fn remove_from_index(&self, id: Uuid) -> Result<(), StreamDbError> {
        self.delete_paths_for_document(id)
    }

    fn get_cache_stats(&self) -> Result<CacheStats, StreamDbError> {
        Ok(self.cache_stats.lock().clone())
    }

    fn get_stream(&self, id: Uuid) -> Result<Box<dyn Iterator<Item = Result<Vec<u8>, StreamDbError>> + Send + Sync>, StreamDbError> {
        let data = self.read_document(id)?;
        Ok(Box::new(std::iter::once(Ok(data))))
    }

    fn as_any(&self) -> &dyn Any { self }

    fn flush(&self) -> Result<(), StreamDbError> {
        self.wal.checkpoint_active()
            .map_err(|e| StreamDbError::TransactionError(e.to_string()))
    }

    fn calculate_statistics(&self) -> Result<(i64, i64), StreamDbError> { Ok((0, 0)) }

    fn set_quick_mode(&self, _enabled: bool) {}

    fn begin_transaction(&self) -> Result<(), StreamDbError> {
        let mut tx = self.transaction.blocking_lock();
        if tx.is_some() {
            return Err(StreamDbError::TransactionError("Transaction in progress".into()));
        }
        *tx = Some(HashMap::new());
        Ok(())
    }

    fn commit_transaction(&self) -> Result<(), StreamDbError> {
        let mut tx = self.transaction.blocking_lock();
        if let Some(data) = tx.take() {
            let mut docs = self.documents.lock();
            docs.extend(data);
            self.wal.checkpoint_active()
                .map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        }
        Ok(())
    }

    fn rollback_transaction(&self) -> Result<(), StreamDbError> {
        self.transaction.blocking_lock().take();
        Ok(())
    }

    async fn begin_async_transaction(&self) -> Result<(), StreamDbError> {
        let mut tx = self.transaction.lock().await;
        if tx.is_some() {
            return Err(StreamDbError::TransactionError("Transaction in progress".into()));
        }
        *tx = Some(HashMap::new());
        Ok(())
    }

    async fn commit_async_transaction(&self) -> Result<(), StreamDbError> {
        let mut tx = self.transaction.lock().await;
        if let Some(data) = tx.take() {
            let mut docs = self.documents.lock();
            docs.extend(data);
            self.wal.checkpoint_active()
                .map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        }
        Ok(())
    }

    async fn rollback_async_transaction(&self) -> Result<(), StreamDbError> {
        self.transaction.lock().await.take();
        Ok(())
    }
}

pub struct FileBackend {
    config: Config,
    file: Arc<Mutex<File>>,
    #[cfg(not(target_arch = "wasm32"))]
    mmap: Arc<PlRwLock<Option<MmapMut>>>,
    current_size: Arc<AtomicU64>,
    document_index: Mutex<HashMap<Uuid, Document>>,
    path_trie: PlRwLock<TrieNode>,
    id_to_paths: Mutex<HashMap<Uuid, Vec<String>>>,
    free_list_root: Mutex<VersionedLink>,
    document_index_root: Mutex<VersionedLink>,
    path_lookup_root: Mutex<VersionedLink>,
    page_cache: Mutex<LruCache<i64, Arc<Vec<u8>>>>,
    quick_mode: AtomicBool,
    old_versions: Mutex<HashMap<Uuid, Vec<(i32, i64)>>>,
    cache_stats: Mutex<CacheStats>,
    empty_free_list_count: Mutex<u64>,
    wal: WriteAheadLog,
    batch_lock: TokioMutex<()>,
    #[cfg(feature = "encryption")]
    aead_key: Option<LessSafeKey>,
}

impl FileBackend {
    pub fn new<P: AsRef<Path>>(path: P, config: Config) -> Result<Self, StreamDbError> {
        let db_path = path.as_ref().to_path_buf();
        let wal = WriteAheadLog::open(db_path.with_extension("wal"))
            .map_err(|e| StreamDbError::Io(e.into()))?;

        let mut file = OpenOptions::new()
            .read(true).write(true).create(true)
            .open(path.as_ref())
            .map_err(StreamDbError::Io)?;

        let initial_size = file.seek(SeekFrom::End(0)).map_err(StreamDbError::Io)?;
        let mut header = DatabaseHeader {
            magic: MAGIC,
            index_root: VersionedLink { page_id: -1, version: 0 },
            path_lookup_root: VersionedLink { page_id: -1, version: 0 },
            free_list_root: VersionedLink { page_id: -1, version: 0 },
        };

        if initial_size == 0 {
            file.set_len(config.page_size).map_err(StreamDbError::Io)?;
            Self::write_header(&mut file, &header, &config)?;
        } else {
            header = Self::read_header(&mut file, &config)?;
            if header.magic != MAGIC {
                warn!("Invalid magic, reinitializing");
                header = DatabaseHeader::default();
                file.set_len(config.page_size).map_err(StreamDbError::Io)?;
                Self::write_header(&mut file, &header, &config)?;
            }
        }

        #[cfg(not(target_arch = "wasm32"))]
        let mmap = config.use_mmap.then(|| {
            unsafe { MmapOptions::new().len(initial_size as usize).map_mut(&file) }
                .map_err(StreamDbError::Io)
        }).transpose()?;

        #[cfg(feature = "encryption")]
        let aead_key = config.encryption_key.as_ref().map(|key| {
            let unbound = UnboundKey::new(&AES_256_GCM, key).expect("Invalid key");
            LessSafeKey::new(unbound)
        });

        let backend = Self {
            config,
            file: Arc::new(Mutex::new(file)),
            #[cfg(not(target_arch = "wasm32"))]
            mmap: Arc::new(PlRwLock::new(mmap)),
            current_size: Arc::new(AtomicU64::new(initial_size)),
            document_index: Mutex::new(HashMap::new()),
            path_trie: PlRwLock::new(TrieNode::new()),
            id_to_paths: Mutex::new(HashMap::new()),
            free_list_root: Mutex::new(header.free_list_root),
            document_index_root: Mutex::new(header.index_root),
            path_lookup_root: Mutex::new(header.path_lookup_root),
            page_cache: Mutex::new(LruCache::new(std::num::NonZeroUsize::new(config.page_cache_size).unwrap())),
            quick_mode: AtomicBool::new(false),
            old_versions: Mutex::new(HashMap::new()),
            cache_stats: Mutex::new(CacheStats::default()),
            empty_free_list_count: Mutex::new(0),
            wal,
            batch_lock: TokioMutex::new(()),
            #[cfg(feature = "encryption")]
            aead_key,
        };

        backend.recover()?;
        Ok(backend)
    }

    fn reverse_path(path: &str) -> Vec<u8> {
        path.chars().rev().map(|c| c as u8).collect()
    }

    fn validate_path(&self, path: &str) -> Result<(), StreamDbError> {
        if path.is_empty() || path.contains('\0') || path.contains("//") || path.len() > 1024 {
            Err(StreamDbError::InvalidInput("Invalid path".into()))
        } else {
            Ok(())
        }
    }

    fn write_header(file: &mut File, header: &DatabaseHeader, _: &Config) -> Result<(), StreamDbError> {
        file.seek(SeekFrom::Start(0)).map_err(StreamDbError::Io)?;
        let mut w = BufWriter::new(file);
        w.write_all(&header.magic).map_err(StreamDbError::Io)?;
        w.write_i64::<LittleEndian>(header.index_root.page_id).map_err(StreamDbError::Io)?;
        w.write_i32::<LittleEndian>(header.index_root.version).map_err(StreamDbError::Io)?;
        w.write_i64::<LittleEndian>(header.path_lookup_root.page_id).map_err(StreamDbError::Io)?;
        w.write_i32::<LittleEndian>(header.path_lookup_root.version).map_err(StreamDbError::Io)?;
        w.write_i64::<LittleEndian>(header.free_list_root.page_id).map_err(StreamDbError::Io)?;
        w.write_i32::<LittleEndian>(header.free_list_root.version).map_err(StreamDbError::Io)?;
        w.flush().map_err(StreamDbError::Io)
    }

    fn read_header(file: &mut File, _: &Config) -> Result<DatabaseHeader, StreamDbError> {
        file.seek(SeekFrom::Start(0)).map_err(StreamDbError::Io)?;
        let mut r = BufReader::new(file);
        let mut magic = [0u8; 8];
        r.read_exact(&mut magic).map_err(StreamDbError::Io)?;
        let index_root = VersionedLink {
            page_id: r.read_i64::<LittleEndian>().map_err(StreamDbError::Io)?,
            version: r.read_i32::<LittleEndian>().map_err(StreamDbError::Io)?,
        };
        let path_lookup_root = VersionedLink {
            page_id: r.read_i64::<LittleEndian>().map_err(StreamDbError::Io)?,
            version: r.read_i32::<LittleEndian>().map_err(StreamDbError::Io)?,
        };
        let free_list_root = VersionedLink {
            page_id: r.read_i64::<LittleEndian>().map_err(StreamDbError::Io)?,
            version: r.read_i32::<LittleEndian>().map_err(StreamDbError::Io)?,
        };
        Ok(DatabaseHeader { magic, index_root, path_lookup_root, free_list_root })
    }

    fn compute_crc(&self, data: &[u8]) -> u32 {
        CrcLib::<u32>::new(&CRC_32_ISO_HDLC).checksum(data)
    }

    #[cfg(feature = "encryption")]
    fn derive_nonce(page_id: i64) -> [u8; NONCE_LEN] {
        let mut nonce = [0u8; NONCE_LEN];
        nonce[..8].copy_from_slice(&page_id.to_le_bytes());
        nonce
    }

    fn read_raw_page(&self, page_id: i64) -> Result<Arc<Vec<u8>>, StreamDbError> {
        let mut cache = self.page_cache.lock();
        if let Some(data) = cache.get(&page_id) {
            self.cache_stats.lock().hits += 1;
            return Ok(data.clone());
        }
        self.cache_stats.lock().misses += 1;

        let header = self.read_page_header(page_id)?;
        let mut data = vec![0u8; header.data_length as usize];
        let offset = page_id as u64 * self.config.page_size + self.config.page_header_size;

        #[cfg(not(target_arch = "wasm32"))]
        if let Some(mmap) = self.mmap.read().as_ref() {
            let start = offset as usize;
            data.copy_from_slice(&mmap[start..start + data.len()]);
        } else {
            let file = self.file.lock();
            file.seek(SeekFrom::Start(offset)).map_err(StreamDbError::Io)?;
            file.read_exact(&mut data).map_err(StreamDbError::Io)?;
        }

        #[cfg(target_arch = "wasm32")]
        {
            let file = self.file.lock();
            file.seek(SeekFrom::Start(offset)).map_err(StreamDbError::Io)?;
            file.read_exact(&mut data).map_err(StreamDbError::Io)?;
        }

        #[cfg(feature = "encryption")]
        if let Some(key) = &self.aead_key {
            let nonce = Nonce::try_assume_unique_for_key(&Self::derive_nonce(page_id)).unwrap();
            key.open_in_place(nonce, Aad::empty(), &mut data)
                .map_err(|_| StreamDbError::EncryptionError("Decryption failed".into()))?;
        }

        if self.config.use_compression {
            data = snappy::uncompress(&data)
                .map_err(|_| StreamDbError::InvalidData("Decompression failed".into()))?;
        }

        if !self.quick_mode.load(Ordering::Relaxed) && self.compute_crc(&data) != header.crc {
            return Err(StreamDbError::InvalidData("CRC mismatch".into()));
        }

        let arc = Arc::new(data);
        cache.put(page_id, arc.clone());
        Ok(arc)
    }

    fn write_raw_page(&self, page_id: i64, raw_data: &[u8], version: i32) -> Result<(), StreamDbError> {
        let _guard = self.batch_lock.blocking_lock();
        let mut data = if self.config.use_compression {
            snappy::compress(raw_data)
        } else {
            raw_data.to_vec()
        };

        #[cfg(feature = "encryption")]
        if let Some(key) = &self.aead_key {
            let nonce = Nonce::try_assume_unique_for_key(&Self::derive_nonce(page_id)).unwrap();
            key.seal_in_place_separate_tag(nonce, Aad::empty(), &mut data)
                .map_err(|_| StreamDbError::EncryptionError("Encryption failed".into()))?;
        }

        let header = PageHeader {
            crc: self.compute_crc(&raw_data),
            version,
            prev_page_id: -1,
            next_page_id: -1,
            flags: FLAG_DATA_PAGE,
            data_length: data.len() as i32,
            padding: [0; 3],
        };

        self.write_page_header(page_id, &header)?;
        let offset = page_id as u64 * self.config.page_size + self.config.page_header_size;

        #[cfg(not(target_arch = "wasm32"))]
        if let Some(mmap) = self.mmap.write().as_mut() {
            let start = offset as usize;
            mmap[start..start + data.len()].copy_from_slice(&data);
            mmap.flush().map_err(StreamDbError::Io)?;
        } else {
            let mut file = self.file.lock();
            file.seek(SeekFrom::Start(offset)).map_err(StreamDbError::Io)?;
            file.write_all(&data).map_err(StreamDbError::Io)?;
        }

        #[cfg(target_arch = "wasm32")]
        {
            let mut file = self.file.lock();
            file.seek(SeekFrom::Start(offset)).map_err(StreamDbError::Io)?;
            file.write_all(&data).map_err(StreamDbError::Io)?;
        }

        self.page_cache.lock().pop(&page_id);
        Ok(())
    }

    fn write_page_header(&self, page_id: i64, header: &PageHeader) -> Result<(), StreamDbError> {
        let offset = page_id as u64 * self.config.page_size;
        let mut buf = Vec::new();
        let mut w = BufWriter::new(&mut buf);
        w.write_u32::<LittleEndian>(header.crc).map_err(StreamDbError::Io)?;
        w.write_i32::<LittleEndian>(header.version).map_err(StreamDbError::Io)?;
        w.write_i64::<LittleEndian>(header.prev_page_id).map_err(StreamDbError::Io)?;
        w.write_i64::<LittleEndian>(header.next_page_id).map_err(StreamDbError::Io)?;
        w.write_u8(header.flags).map_err(StreamDbError::Io)?;
        w.write_i32::<LittleEndian>(header.data_length).map_err(StreamDbError::Io)?;
        w.write_all(&header.padding).map_err(StreamDbError::Io)?;
        w.flush().map_err(StreamDbError::Io)?;

        #[cfg(not(target_arch = "wasm32"))]
        if let Some(mmap) = self.mmap.write().as_mut() {
            let start = offset as usize;
            mmap[start..start + buf.len()].copy_from_slice(&buf);
            mmap.flush().map_err(StreamDbError::Io)?;
        } else {
            let mut file = self.file.lock();
            file.seek(SeekFrom::Start(offset)).map_err(StreamDbError::Io)?;
            file.write_all(&buf).map_err(StreamDbError::Io)?;
        }

        #[cfg(target_arch = "wasm32")]
        {
            let mut file = self.file.lock();
            file.seek(SeekFrom::Start(offset)).map_err(StreamDbError::Io)?;
            file.write_all(&buf).map_err(StreamDbError::Io)?;
        }
        Ok(())
    }

    fn read_page_header(&self, page_id: i64) -> Result<PageHeader, StreamDbError> {
        let offset = page_id as u64 * self.config.page_size;
        let mut buf = vec![0u8; self.config.page_header_size as usize];

        #[cfg(not(target_arch = "wasm32"))]
        if let Some(mmap) = self.mmap.read().as_ref() {
            let start = offset as usize;
            buf.copy_from_slice(&mmap[start..start + buf.len()]);
        } else {
            let file = self.file.lock();
            file.seek(SeekFrom::Start(offset)).map_err(StreamDbError::Io)?;
            file.read_exact(&mut buf).map_err(StreamDbError::Io)?;
        }

        #[cfg(target_arch = "wasm32")]
        {
            let file = self.file.lock();
            file.seek(SeekFrom::Start(offset)).map_err(StreamDbError::Io)?;
            file.read_exact(&mut buf).map_err(StreamDbError::Io)?;
        }

        let mut r = Cursor::new(buf);
        Ok(PageHeader {
            crc: r.read_u32::<LittleEndian>().map_err(StreamDbError::Io)?,
            version: r.read_i32::<LittleEndian>().map_err(StreamDbError::Io)?,
            prev_page_id: r.read_i64::<LittleEndian>().map_err(StreamDbError::Io)?,
            next_page_id: r.read_i64::<LittleEndian>().map_err(StreamDbError::Io)?,
            flags: r.read_u8().map_err(StreamDbError::Io)?,
            data_length: r.read_i32::<LittleEndian>().map_err(StreamDbError::Io)?,
            padding: [0; 3],
        })
    }

    fn allocate_page(&self) -> Result<i64, StreamDbError> {
        let mut root = self.free_list_root.lock();
        let mut current = root.page_id;
        let mut empty_count = *self.empty_free_list_count.lock();

        while current != -1 {
            let mut page = self.read_free_list_page(current)?;
            if !page.free_page_ids.is_empty() {
                let id = page.free_page_ids.pop().unwrap();
                page.used_entries -= 1;
                self.write_free_list_page(current, &page)?;
                *self.empty_free_list_count.lock() = 0;
                return Ok(id);
            }
            empty_count += 1;
            current = page.next_free_list_page;
        }

        if empty_count >= MAX_CONSECUTIVE_EMPTY_FREE_LIST {
            self.grow_db(BATCH_GROW_PAGES)?;
            empty_count = 0;
        }
        *self.empty_free_list_count.lock() = empty_count;

        let new_id = self.current_size.load(Ordering::Relaxed) as i64 / self.config.page_size as i64;
        self.grow_db(1)?;
        Ok(new_id)
    }

    fn free_page(&self, page_id: i64) -> Result<(), StreamDbError> {
        let mut root = self.free_list_root.lock();
        let mut current = root.page_id;

        while current != -1 {
            let mut page = self.read_free_list_page(current)?;
            if page.used_entries as usize == FREE_LIST_ENTRIES_PER_PAGE {
                if page.next_free_list_page == -1 {
                    let new = self.allocate_page()?;
                    page.next_free_list_page = new;
                    self.write_free_list_page(current, &page)?;
                    let new_page = FreeListPage {
                        next_free_list_page: -1,
                        used_entries: 1,
                        free_page_ids: vec![page_id],
                    };
                    self.write_free_list_page(new, &new_page)?;
                    return Ok(());
                }
                current = page.next_free_list_page;
            } else {
                page.free_page_ids.push(page_id);
                page.used_entries += 1;
                self.write_free_list_page(current, &page)?;
                return Ok(());
            }
        }

        let new = self.allocate_page()?;
        root.page_id = new;
        let page = FreeListPage {
            next_free_list_page: -1,
            used_entries: 1,
            free_page_ids: vec![page_id],
        };
        self.write_free_list_page(new, &page)?;
        Ok(())
    }

    fn read_free_list_page(&self, page_id: i64) -> Result<FreeListPage, StreamDbError> {
        let data = self.read_raw_page(page_id)?;
        let mut r = Cursor::new(&*data);
        Ok(FreeListPage {
            next_free_list_page: r.read_i64::<LittleEndian>().map_err(StreamDbError::Io)?,
            used_entries: r.read_i32::<LittleEndian>().map_err(StreamDbError::Io)?,
            free_page_ids: (0..r.read_i32::<LittleEndian>().map_err(StreamDbError::Io)?)
                .map(|_| r.read_i64::<LittleEndian>().map_err(StreamDbError::Io))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    fn write_free_list_page(&self, page_id: i64, page: &FreeListPage) -> Result<(), StreamDbError> {
        let mut buf = Vec::new();
        buf.write_i64::<LittleEndian>(page.next_free_list_page).map_err(StreamDbError::Io)?;
        buf.write_i32::<LittleEndian>(page.used_entries).map_err(StreamDbError::Io)?;
        for &id in &page.free_page_ids {
            buf.write_i64::<LittleEndian>(id).map_err(StreamDbError::Io)?;
        }
        let header = PageHeader {
            crc: self.compute_crc(&buf),
            version: 1,
            prev_page_id: -1,
            next_page_id: -1,
            flags: FLAG_FREE_LIST_PAGE,
            data_length: buf.len() as i32,
            padding: [0; 3],
        };
        self.write_page_header(page_id, &header)?;
        self.write_raw_page(page_id, &buf, 1)?;
        Ok(())
    }

    fn grow_db(&self, pages: u64) -> Result<(), StreamDbError> {
        let current = self.current_size.load(Ordering::Relaxed);
        let new = current + pages * self.config.page_size;
        if new > self.config.max_db_size {
            return Err(StreamDbError::InvalidInput("DB size limit exceeded".into()));
        }
        let mut file = self.file.lock();
        file.set_len(new).map_err(StreamDbError::Io)?;
        self.current_size.store(new, Ordering::Relaxed);
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(mut mmap) = self.mmap.write().as_mut() {
            *mmap = unsafe { MmapOptions::new().len(new as usize).map_mut(&*file) }
                .map_err(StreamDbError::Io)?;
        }
        Ok(())
    }

    fn recover(&self) -> Result<(), StreamDbError> {
        let mut docs = self.document_index.lock();
        let mut paths = self.id_to_paths.lock();
        let mut trie = self.path_trie.write();

        self.wal.recover(|entry| {
            let data = entry.data();
            let (op, id, payload): (&str, Uuid, Option<Vec<u8>>) = deserialize(&data)
                .map_err(|e| StreamDbError::InvalidData(e.to_string()))?;

            match op {
                "write" => if let Some(buf) = payload {
                    let page_id = self.allocate_page()?;
                    self.write_raw_page(page_id, &buf, 1)?;
                    docs.insert(id, Document {
                        id,
                        first_page_id: page_id,
                        current_version: 1,
                        paths: vec![],
                    });
                },
                "delete" => {
                    docs.remove(&id);
                    paths.remove(&id);
                }
                "bind" => if let Some(bytes) = payload {
                    if let Ok(path) = String::from_utf8(bytes) {
                        let rev = Self::reverse_path(&path);
                        *trie = trie.insert(&rev, id);
                        paths.entry(id).or_default().push(path);
                    }
                }
                "unbind" => if let Some(bytes) = payload {
                    if let Ok(path) = String::from_utf8(bytes) {
                        let rev = Self::reverse_path(&path);
                        if let Some(new) = trie.remove(&rev) { *trie = new; }
                        if let Some(list) = paths.get_mut(&id) {
                            list.retain(|p| p != &path);
                        }
                    }
                }
                _ => {}
            }
            Ok(())
        }).map_err(|e| StreamDbError::TransactionError(e.to_string()))?;
        Ok(())
    }
}

impl DatabaseBackend for FileBackend {
    fn write_document(&self, data: &mut dyn Read) -> Result<Uuid, StreamDbError> {
        let mut buf = Vec::new();
        data.read_to_end(&mut buf).map_err(StreamDbError::Io)?;
        let id = Uuid::new_v4();
        let page_id = self.allocate_page()?;
        self.write_raw_page(page_id, &buf, 1)?;
        self.document_index.lock().insert(id, Document {
            id,
            first_page_id: page_id,
            current_version: 1,
            paths: vec![],
        });
        self.wal.append(&serialize(&("write", id, Some(buf))))?;
        Ok(id)
    }

    fn read_document(&self, id: Uuid) -> Result<Vec<u8>, StreamDbError> {
        self.read_document_quick(id, self.quick_mode.load(Ordering::Relaxed))
    }

    fn read_document_quick(&self, id: Uuid, quick: bool) -> Result<Vec<u8>, StreamDbError> {
        let old = self.quick_mode.load(Ordering::Relaxed);
        self.quick_mode.store(quick, Ordering::Relaxed);
        let doc = self.document_index.lock().get(&id)
            .ok_or(StreamDbError::NotFound("Not found".into()))?.clone();
        let mut result = Vec::new();
        let mut cur = doc.first_page_id;
        while cur != -1 {
            result.extend_from_slice(&self.read_raw_page(cur)?);
            cur = self.read_page_header(cur)?.next_page_id;
        }
        self.quick_mode.store(old, Ordering::Relaxed);
        Ok(result)
    }

    fn delete_document(&self, id: Uuid) -> Result<(), StreamDbError> {
        let doc = self.document_index.lock().remove(&id)
            .ok_or(StreamDbError::NotFound("Not found".into()))?;
        let mut cur = doc.first_page_id;
        while cur != -1 {
            let header = self.read_page_header(cur)?;
            self.free_page(cur)?;
            cur = header.next_page_id;
        }
        self.remove_from_index(id)?;
        self.wal.append(&serialize(&("delete", id, None::<Vec<u8>>)))?;
        Ok(())
    }

    fn bind_path_to_document(&self, path: &str, id: Uuid) -> Result<Uuid, StreamDbError> {
        self.validate_path(path)?;
        let mut doc = self.document_index.lock().get(&id).cloned()
            .ok_or(StreamDbError::NotFound("Not found".into()))?;
        doc.paths.push(path.to_string());
        self.document_index.lock().insert(id, doc);
        let rev = Self::reverse_path(path);
        let mut trie = self.path_trie.write();
        *trie = trie.insert(&rev, id);
        self.id_to_paths.lock().entry(id).or_default().push(path.to_string());
        self.wal.append(&serialize(&("bind", id, Some(path.as_bytes().to_vec()))))?;
        Ok(id)
    }

    fn get_document_id_by_path(&self, path: &str) -> Result<Uuid, StreamDbError> {
        self.validate_path(path)?;
        let rev = Self::reverse_path(path);
        let trie = self.path_trie.read();
        trie.get(&rev).ok_or(StreamDbError::NotFound("Path not found".into()))
    }

    fn search_paths(&self, prefix: &str) -> Result<Vec<String>, StreamDbError> {
        self.validate_path(prefix)?;
        let rev = Self::reverse_path(prefix);
        let trie = self.path_trie.read();
        let mut results = Vec::new();
        trie.search(&rev, &mut results, String::new());
        Ok(results)
    }

    fn list_paths_for_document(&self, id: Uuid) -> Result<Vec<String>, StreamDbError> {
        Ok(self.id_to_paths.lock().get(&id).cloned().unwrap_or_default())
    }

    fn count_free_pages(&self) -> Result<i64, StreamDbError> {
        let mut count = 0;
        let mut cur = self.free_list_root.lock().page_id;
        while cur != -1 {
            let page = self.read_free_list_page(cur)?;
            count += page.free_page_ids.len() as i64;
            cur = page.next_free_list_page;
        }
        Ok(count)
    }

    fn get_info(&self, id: Uuid) -> Result<String, StreamDbError> {
        let doc = self.document_index.lock().get(&id)
            .ok_or(StreamDbError::NotFound("Not found".into()))?;
        let paths = self.list_paths_for_document(id)?;
        Ok(format!("ID: {}, Version: {}, Paths: {:?}", id, doc.current_version, paths))
    }

    fn delete_paths_for_document(&self, id: Uuid) -> Result<(), StreamDbError> {
        let paths = self.id_to_paths.lock().remove(&id).unwrap_or_default();
        let mut trie = self.path_trie.write();
        for path in &paths {
            let rev = Self::reverse_path(path);
            if let Some(new) = trie.remove(&rev) {
                *trie = new;
            }
            self.wal.append(&serialize(&("unbind", id, Some(path.as_bytes().to_vec()))))?;
        }
        Ok(())
    }

    fn remove_from_index(&self, id: Uuid) -> Result<(), StreamDbError> {
        self.delete_paths_for_document(id)
    }

    fn get_cache_stats(&self) -> Result<CacheStats, StreamDbError> {
        Ok(self.cache_stats.lock().clone())
    }

    fn get_stream(&self, id: Uuid) -> Result<Box<dyn Iterator<Item = Result<Vec<u8>, StreamDbError>> + Send + Sync>, StreamDbError> {
        let doc = self.document_index.lock().get(&id).cloned()
            .ok_or(StreamDbError::NotFound("Not found".into()))?;
        let mut pages = Vec::new();
        let mut cur = doc.first_page_id;
        while cur != -1 {
            pages.push(cur);
            cur = self.read_page_header(cur)?.next_page_id;
        }
        let backend = Arc::new(self);
        let iter = pages.into_iter().map(move |pid| {
            backend.read_raw_page(pid).map(|d| d.to_vec())
        });
        Ok(Box::new(iter))
    }

    fn as_any(&self) -> &dyn Any { self }

    fn flush(&self) -> Result<(), StreamDbError> {
        self.wal.checkpoint_active()?;
        let mut file = self.file.lock();
        file.flush().map_err(StreamDbError::Io)?;
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(m) = self.mmap.read().as_ref() {
            m.flush().map_err(StreamDbError::Io)?;
        }
        Ok(())
    }

    fn calculate_statistics(&self) -> Result<(i64, i64), StreamDbError> {
        let total = self.current_size.load(Ordering::Relaxed) as i64 / self.config.page_size as i64;
        let free = self.count_free_pages()?;
        Ok((total - free, free))
    }

    fn set_quick_mode(&self, enabled: bool) {
        self.quick_mode.store(enabled, Ordering::Relaxed);
    }

    fn begin_transaction(&self) -> Result<(), StreamDbError> { Ok(()) }
    fn commit_transaction(&self) -> Result<(), StreamDbError> { Ok(()) }
    fn rollback_transaction(&self) -> Result<(), StreamDbError> { Ok(()) }

    async fn begin_async_transaction(&self) -> Result<(), StreamDbError> { Ok(()) }
    async fn commit_async_transaction(&self) -> Result<(), StreamDbError> { Ok(()) }
    async fn rollback_async_transaction(&self) -> Result<(), StreamDbError> { Ok(()) }
}

impl Default for DatabaseHeader {
    fn default() -> Self {
        Self {
            magic: MAGIC,
            index_root: VersionedLink { page_id: -1, version: 0 },
            path_lookup_root: VersionedLink { page_id: -1, version: 0 },
            free_list_root: VersionedLink { page_id: -1, version: 0 },
        }
    }
}
