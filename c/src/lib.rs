#![allow(non_camel_case_types)]

use std::{os::raw::c_char, ptr, sync};

#[macro_use]
mod utils;
pub mod error;
pub mod filter;
pub mod pointer;
pub mod process_handle;
pub mod scan;
pub mod string;
pub mod system;
pub mod values;
pub mod watch;

/// A opaque process identifier.
pub struct pts_process_id_t(pub(crate) ptscan::ProcessId);

/// A thread pool.
pub struct pts_thread_pool_t(pub(crate) sync::Arc<rayon::ThreadPool>);

static VERSION: &'static [u8] = b"0.1.0\x00";

/// Get the current ptscan version.
#[no_mangle]
pub extern "C" fn pts_version() -> *const c_char {
    VERSION as *const _ as *const c_char
}

/// Setup function that needs to be called to initialize the library.
#[no_mangle]
pub extern "C" fn pts_setup() {
    // NB: needed to initialize eagerly initialized data.
    failure::format_err!("dummy error");
}

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

/// A token that can be used to indicate some condition.
pub struct pts_token_t(ptscan::Token);

/// Create a new token.
#[no_mangle]
pub extern "C" fn pts_token_new<'a>() -> *mut pts_token_t {
    into_ptr!(pts_token_t(ptscan::Token::new()))
}

/// Set the token.
#[no_mangle]
pub extern "C" fn pts_token_set<'a>(token: *const pts_token_t) {
    let pts_token_t(ref token) = *null_ck!(&'a token);
    token.set();
}

/// Free the token.
#[no_mangle]
pub extern "C" fn pts_token_free<'a>(token: *mut pts_token_t) {
    free!(token);
}
