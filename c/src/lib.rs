#![allow(non_camel_case_types)]

use std::{ptr, sync};

#[macro_use]
mod utils;
pub mod error;
pub mod process_handle;
pub mod scanner;
pub mod system;

/// A opaque process identifier.
pub struct pts_process_id_t(pub(crate) ptscan::ProcessId);

/// A thread pool.
pub struct pts_thread_pool_t(pub(crate) sync::Arc<rayon::ThreadPool>);

/// Create a new thread pool.
///
/// If an error is raised, NULL is returned and `pts_error_last()` is updated accordingly.
#[no_mangle]
pub extern "C" fn pts_thread_pool_new<'a>() -> *mut pts_thread_pool_t {
    let thread_pool = try_last!(rayon::ThreadPoolBuilder::new().build(), ptr::null_mut());
    into_ptr!(pts_thread_pool_t(sync::Arc::new(thread_pool)))
}

/// Close and free the thread pool.
#[no_mangle]
pub extern "C" fn pts_thread_pool_free(thread_pool: *mut pts_thread_pool_t) {
    free!(thread_pool);
}
