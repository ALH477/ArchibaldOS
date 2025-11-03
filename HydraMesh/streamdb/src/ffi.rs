//! Foreign Function Interface (FFI) for StreamDb.
//!
//! This module provides a C-compatible interface for StreamDb, enabling integration with non-Rust systems.
//! It covers all `Database` trait methods, including async operations via callbacks, using custom structs for
//! safe memory management. The `StreamDbHandle` wraps an `Arc<StreamDb>` for thread safety, and functions like
//! `streamdb_get_async` use a Tokio runtime for robust async handling. Error codes and callbacks ensure clear
//! communication with C clients.

use super::{StreamDb, Database, StreamDbError};
use std::os::raw::{c_char, c_int, c_uint, c_void};
use std::slice;
use std::ffi::{CString, CStr};
use std::sync::Arc;
use uuid::Uuid;
use tokio::runtime::Runtime;
use lazy_static::lazy_static;
use std::panic;

#[repr(C)]
pub struct StreamDbHandle(Arc<StreamDb>);

#[repr(C)]
pub struct ByteBuffer {
    data: *mut u8,
    len: usize,
}

impl ByteBuffer {
    pub fn from_vec(vec: Vec<u8>) -> Self {
        let mut vec = vec;
        let data = vec.as_mut_ptr();
        let len = vec.len();
        std::mem::forget(vec);
        Self { data, len }
    }

    pub fn destroy(self) {
        if !self.data.is_null() {
            unsafe { drop(Vec::from_raw_parts(self.data, self.len, self.len)); }
        }
    }
}

struct FfiStr<'a>(&'a CStr);

impl<'a> FfiStr<'a> {
    fn from_raw(ptr: *const c_char) -> Self {
        Self(unsafe { CStr::from_ptr(ptr) })
    }

    fn as_str(&self) -> Result<&'a str, ()> {
        self.0.to_str().map_err(|_| ())
    }
}

fn rust_string_to_c(s: String) -> *mut c_char {
    CString::new(s).ok().map(|c| c.into_raw()).unwrap_or(std::ptr::null_mut())
}

const SUCCESS: c_int = 0;
const ERR_IO: c_int = -1;
const ERR_NOT_FOUND: c_int = -2;
const ERR_INVALID_INPUT: c_int = -3;
const ERR_PANIC: c_int = -4;
const ERR_TRANSACTION: c_int = -5;

// Callback for async operations: data, length, error code, user data
type Callback = extern "C" fn(*const u8, c_uint, c_int, *mut c_void) -> c_int;

// Streaming iterator handle for C clients
#[repr(C)]
struct StreamIterator {
    inner: Box<dyn Iterator<Item = Result<Vec<u8>, StreamDbError>> + Send + Sync>,
    handle: *mut StreamDbHandle,
}

lazy_static! {
    static ref RUNTIME: Runtime = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(1)
        .enable_all()
        .build()
        .expect("Failed to create Tokio runtime for FFI");
}

fn call_with_result<F, T>(f: F) -> Option<T>
where F: FnOnce() -> Result<T, StreamDbError> + std::panic::UnwindSafe {
    panic::catch_unwind(f).ok().and_then(|res| res.ok())
}

#[no_mangle]
pub extern "C" fn streamdb_open(path: *const c_char, out_handle: *mut *mut StreamDbHandle) -> c_int {
    let res = call_with_result(|| {
        let path_str = FfiStr::from_raw(path).as_str()
            .map_err(|_| StreamDbError::InvalidInput("Invalid path string".to_string()))?;
        let config = super::Config::default();
        let db = StreamDb::open_with_config(path_str, config)?;
        unsafe { *out_handle = Box::into_raw(Box::new(StreamDbHandle(Arc::new(db)))); }
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_IO)
}

#[no_mangle]
pub extern "C" fn streamdb_close(handle: *mut StreamDbHandle) {
    if !handle.is_null() {
        unsafe { drop(Box::from_raw(handle)); }
    }
}

#[no_mangle]
pub extern "C" fn streamdb_write_document(handle: *mut StreamDbHandle, path: *const c_char, data: *const u8, len: c_uint) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &mut (*handle).0 };
        let path_str = FfiStr::from_raw(path).as_str()
            .map_err(|_| StreamDbError::InvalidInput("Invalid path string".to_string()))?;
        let data_slice = unsafe { slice::from_raw_parts(data, len as usize) };
        let mut cursor = std::io::Cursor::new(data_slice);
        db.write_document(path_str, &mut cursor)?;
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_IO)
}

#[no_mangle]
pub extern "C" fn streamdb_get(handle: *mut StreamDbHandle, path: *const c_char, out_buffer: *mut ByteBuffer) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &(*handle).0 };
        let path_str = FfiStr::from_raw(path).as_str()
            .map_err(|_| StreamDbError::InvalidInput("Invalid path string".to_string()))?;
        let vec = db.get(path_str)?;
        unsafe { *out_buffer = ByteBuffer::from_vec(vec); }
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_NOT_FOUND)
}

#[no_mangle]
pub extern "C" fn streamdb_get_quick(handle: *mut StreamDbHandle, path: *const c_char, quick: c_int, out_buffer: *mut ByteBuffer) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &(*handle).0 };
        let path_str = FfiStr::from_raw(path).as_str()
            .map_err(|_| StreamDbError::InvalidInput("Invalid path string".to_string()))?;
        let vec = db.get_quick(path_str, quick != 0)?;
        unsafe { *out_buffer = ByteBuffer::from_vec(vec); }
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_NOT_FOUND)
}

#[no_mangle]
pub extern "C" fn streamdb_get_id_by_path(handle: *mut StreamDbHandle, path: *const c_char, out_id: *mut Uuid) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &(*handle).0 };
        let path_str = FfiStr::from_raw(path).as_str()
            .map_err(|_| StreamDbError::InvalidInput("Invalid path string".to_string()))?;
        let id = db.get_id_by_path(path_str)?.ok_or(StreamDbError::NotFound("Path not found".to_string()))?;
        unsafe { *out_id = id; }
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_NOT_FOUND)
}

#[no_mangle]
pub extern "C" fn streamdb_delete(handle: *mut StreamDbHandle, path: *const c_char) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &mut (*handle).0 };
        let path_str = FfiStr::from_raw(path).as_str()
            .map_err(|_| StreamDbError::InvalidInput("Invalid path string".to_string()))?;
        db.delete(path_str)?;
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_NOT_FOUND)
}

#[no_mangle]
pub extern "C" fn streamdb_delete_by_id(handle: *mut StreamDbHandle, id: Uuid) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &mut (*handle).0 };
        db.delete_by_id(id)?;
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_NOT_FOUND)
}

#[no_mangle]
pub extern "C" fn streamdb_bind_to_path(handle: *mut StreamDbHandle, id: Uuid, path: *const c_char) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &mut (*handle).0 };
        let path_str = FfiStr::from_raw(path).as_str()
            .map_err(|_| StreamDbError::InvalidInput("Invalid path string".to_string()))?;
        db.bind_to_path(id, path_str)?;
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_INVALID_INPUT)
}

#[no_mangle]
pub extern "C" fn streamdb_unbind_path(handle: *mut StreamDbHandle, id: Uuid, path: *const c_char) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &mut (*handle).0 };
        let path_str = FfiStr::from_raw(path).as_str()
            .map_err(|_| StreamDbError::InvalidInput("Invalid path string".to_string()))?;
        db.unbind_path(id, path_str)?;
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_NOT_FOUND)
}

#[no_mangle]
pub extern "C" fn streamdb_search(handle: *mut StreamDbHandle, prefix: *const c_char, out_array: *mut *mut *const c_char, out_len: *mut c_uint) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &(*handle).0 };
        let prefix_str = FfiStr::from_raw(prefix).as_str()
            .map_err(|_| StreamDbError::InvalidInput("Invalid prefix string".to_string()))?;
        let results = db.search(prefix_str)?;
        let mut c_strings = results.into_iter().map(|s| rust_string_to_c(s)).collect::<Vec<_>>();
        let ptr = c_strings.as_mut_ptr();
        unsafe { *out_array = ptr; *out_len = c_strings.len() as c_uint; }
        std::mem::forget(c_strings); // Freed by streamdb_free_string_array
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_IO)
}

#[no_mangle]
pub extern "C" fn streamdb_free_string_array(ptr: *mut *const c_char, len: c_uint) {
    if !ptr.is_null() {
        let slice = unsafe { slice::from_raw_parts_mut(ptr, len as usize) };
        for &p in slice.iter() {
            if !p.is_null() {
                unsafe { drop(CString::from_raw(p as *mut c_char)); }
            }
        }
        unsafe { drop(Vec::from_raw_parts(ptr, len as usize, len as usize)); }
    }
}

#[no_mangle]
pub extern "C" fn streamdb_list_paths(handle: *mut StreamDbHandle, id: Uuid, out_array: *mut *mut *const c_char, out_len: *mut c_uint) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &(*handle).0 };
        let results = db.list_paths(id)?;
        let mut c_strings = results.into_iter().map(|s| rust_string_to_c(s)).collect::<Vec<_>>();
        let ptr = c_strings.as_mut_ptr();
        unsafe { *out_array = ptr; *out_len = c_strings.len() as c_uint; }
        std::mem::forget(c_strings);
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_NOT_FOUND)
}

#[no_mangle]
pub extern "C" fn streamdb_flush(handle: *mut StreamDbHandle) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &mut (*handle).0 };
        db.flush()?;
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_IO)
}

#[no_mangle]
pub extern "C" fn streamdb_calculate_statistics(handle: *mut StreamDbHandle, out_free: *mut i64, out_total: *mut i64) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &(*handle).0 };
        let (free, total) = db.calculate_statistics()?;
        unsafe { *out_free = free; *out_total = total; }
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_IO)
}

#[no_mangle]
pub extern "C" fn streamdb_set_quick_mode(handle: *mut StreamDbHandle, enabled: c_int) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &mut (*handle).0 };
        db.set_quick_mode(enabled != 0);
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_IO)
}

#[no_mangle]
pub extern "C" fn streamdb_snapshot(handle: *mut StreamDbHandle, out_snapshot_handle: *mut *mut StreamDbHandle) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &(*handle).0 };
        let snapshot = db.snapshot()?;
        unsafe { *out_snapshot_handle = Box::into_raw(Box::new(StreamDbHandle(Arc::new(snapshot)))); }
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_IO)
}

#[no_mangle]
pub extern "C" fn streamdb_get_cache_stats(handle: *mut StreamDbHandle, out_hits: *mut usize, out_misses: *mut usize) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &(*handle).0 };
        let stats = db.get_cache_stats()?;
        unsafe { *out_hits = stats.hits; *out_misses = stats.misses; }
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_IO)
}

#[no_mangle]
pub extern "C" fn streamdb_get_stream(handle: *mut StreamDbHandle, path: *const c_char, out_iterator: *mut *mut StreamIterator) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &(*handle).0 };
        let path_str = FfiStr::from_raw(path).as_str()
            .map_err(|_| StreamDbError::InvalidInput("Invalid path string".to_string()))?;
        let iterator = db.get_stream(path_str)?;
        unsafe {
            *out_iterator = Box::into_raw(Box::new(StreamIterator {
                inner: iterator,
                handle,
            }));
        }
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_NOT_FOUND)
}

#[no_mangle]
pub extern "C" fn streamdb_stream_next(iterator: *mut StreamIterator, out_buffer: *mut ByteBuffer) -> c_int {
    let res = call_with_result(|| {
        let iter = unsafe { &mut (*iterator) };
        match iter.inner.next() {
            Some(Ok(vec)) => {
                unsafe { *out_buffer = ByteBuffer::from_vec(vec); }
                Ok(SUCCESS)
            }
            Some(Err(e)) => Err(e),
            None => Ok(-3), // End of stream
        }
    });
    res.unwrap_or(ERR_NOT_FOUND)
}

#[no_mangle]
pub extern "C" fn streamdb_free_stream(iterator: *mut StreamIterator) {
    if !iterator.is_null() {
        unsafe { drop(Box::from_raw(iterator)); }
    }
}

#[no_mangle]
pub extern "C" fn streamdb_get_async(handle: *mut StreamDbHandle, path: *const c_char, callback: Callback, user_data: *mut c_void) -> c_int {
    let db = unsafe { (*handle).0.clone() };
    let path_str = match FfiStr::from_raw(path).as_str() {
        Ok(s) => s.to_string(),
        Err(_) => {
            callback(std::ptr::null(), 0, ERR_INVALID_INPUT, user_data);
            return ERR_INVALID_INPUT;
        }
    };
    RUNTIME.spawn(async move {
        let result = db.get_async(&path_str).await;
        match result {
            Ok(vec) => {
                let buffer = ByteBuffer::from_vec(vec);
                callback(buffer.data, buffer.len as c_uint, SUCCESS, user_data);
                buffer.destroy();
            }
            Err(e) => {
                let err_code = match e {
                    StreamDbError::NotFound(_) => ERR_NOT_FOUND,
                    StreamDbError::Io(_) => ERR_IO,
                    StreamDbError::InvalidInput(_) => ERR_INVALID_INPUT,
                    StreamDbError::TransactionError(_) => ERR_TRANSACTION,
                    _ => ERR_PANIC,
                };
                callback(std::ptr::null(), 0, err_code, user_data);
            }
        }
    });
    SUCCESS
}

#[no_mangle]
pub extern "C" fn streamdb_free_byte_buffer(buffer: ByteBuffer) {
    buffer.destroy();
}

#[no_mangle]
pub extern "C" fn streamdb_begin_transaction(handle: *mut StreamDbHandle) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &mut (*handle).0 };
        db.begin_transaction()?;
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_TRANSACTION)
}

#[no_mangle]
pub extern "C" fn streamdb_commit_transaction(handle: *mut StreamDbHandle) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &mut (*handle).0 };
        db.commit_transaction()?;
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_TRANSACTION)
}

#[no_mangle]
pub extern "C" fn streamdb_rollback_transaction(handle: *mut StreamDbHandle) -> c_int {
    let res = call_with_result(|| {
        let db = unsafe { &mut (*handle).0 };
        db.rollback_transaction()?;
        Ok(SUCCESS)
    });
    res.unwrap_or(ERR_TRANSACTION)
}

#[no_mangle]
pub extern "C" fn streamdb_begin_async_transaction(handle: *mut StreamDbHandle, callback: Callback, user_data: *mut c_void) -> c_int {
    let db = unsafe { (*handle).0.clone() };
    RUNTIME.spawn(async move {
        let result = db.begin_async_transaction().await;
        let err_code = match result {
            Ok(()) => SUCCESS,
            Err(e) => match e {
                StreamDbError::TransactionError(_) => ERR_TRANSACTION,
                _ => ERR_PANIC,
            },
        };
        callback(std::ptr::null(), 0, err_code, user_data);
    });
    SUCCESS
}

#[no_mangle]
pub extern "C" fn streamdb_commit_async_transaction(handle: *mut StreamDbHandle, callback: Callback, user_data: *mut c_void) -> c_int {
    let db = unsafe { (*handle).0.clone() };
    RUNTIME.spawn(async move {
        let result = db.commit_async_transaction().await;
        let err_code = match result {
            Ok(()) => SUCCESS,
            Err(e) => match e {
                StreamDbError::TransactionError(_) => ERR_TRANSACTION,
                _ => ERR_PANIC,
            },
        };
        callback(std::ptr::null(), 0, err_code, user_data);
    });
    SUCCESS
}

#[no_mangle]
pub extern "C" fn streamdb_rollback_async_transaction(handle: *mut StreamDbHandle, callback: Callback, user_data: *mut c_void) -> c_int {
    let db = unsafe { (*handle).0.clone() };
    RUNTIME.spawn(async move {
        let result = db.rollback_async_transaction().await;
        let err_code = match result {
            Ok(()) => SUCCESS,
            Err(e) => match e {
                StreamDbError::TransactionError(_) => ERR_TRANSACTION,
                _ => ERR_PANIC,
            },
        };
        callback(std::ptr::null(), 0, err_code, user_data);
    });
    SUCCESS
}
