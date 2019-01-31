#![allow(non_camel_case_types)]

extern crate ptscan;

use std::{mem, os::raw::c_char, ptr, sync};

#[macro_use]
mod utils;
pub mod error;
pub mod filter;
pub mod pointer;
pub mod process_handle;
pub mod scan;
pub mod string;
pub mod system;
pub mod value;
pub mod values;
pub mod watch;

/// A thread pool.
pub struct ThreadPool(pub(crate) sync::Arc<rayon::ThreadPool>);

static VERSION: &'static [u8] = b"0.1.0\x00";

/// Get the current ptscan version.
#[no_mangle]
pub extern "C" fn pts_version() -> *const c_char {
    VERSION as *const _ as *const c_char
}

/// Setup function that needs to be called to initialize the library.
#[no_mangle]
pub extern "C" fn pts_setup() {
    assert_eq!(value::VALUE_SIZE, mem::size_of::<ptscan::Value>());
    // NB: needed to initialize eagerly initialized data.
    failure::format_err!("dummy error");
}

/// Create a new thread pool.
///
/// If an error is raised, NULL is returned and `error_last()` is updated accordingly.
#[no_mangle]
pub extern "C" fn pts_thread_pool_new<'a>() -> *mut ThreadPool {
    let thread_pool = try_last!(rayon::ThreadPoolBuilder::new().build(), ptr::null_mut());
    into_ptr!(ThreadPool(sync::Arc::new(thread_pool)))
}

/// Close and free the thread pool.
#[no_mangle]
pub extern "C" fn pts_thread_pool_free(thread_pool: *mut ThreadPool) {
    free!(thread_pool);
}

/// A token that can be used to indicate some condition.
pub struct Token(ptscan::Token);

/// Create a new token.
#[no_mangle]
pub extern "C" fn pts_token_new<'a>() -> *mut Token {
    into_ptr!(Token(ptscan::Token::new()))
}

/// Set the token.
#[no_mangle]
pub extern "C" fn pts_token_set<'a>(token: *const Token) {
    let Token(ref token) = *null_ck!(&'a token);
    token.set();
}

/// Free the token.
#[no_mangle]
pub extern "C" fn pts_token_free<'a>(token: *mut Token) {
    free!(token);
}
