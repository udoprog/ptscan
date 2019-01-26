use crate::utils;
use std::{cell::RefCell, os::raw::c_char, ptr};

thread_local!(pub(crate) static LAST_ERROR: RefCell<Option<failure::Error>> = RefCell::new(None));

pub struct pts_error_t(failure::Error);

/// Returns the last error raised in this thread.
///
/// Returns NULL if no error was raised.
#[no_mangle]
pub extern "C" fn pts_error_last<'a>() -> *const pts_error_t {
    match LAST_ERROR.with(|e| e.borrow_mut().take()) {
        Some(e) => into_ptr!(pts_error_t(e)),
        None => ptr::null(),
    }
}

/// Write the last error message to the given string.
///
/// Returns the number of bytes copied.
#[no_mangle]
pub extern "C" fn pts_error_message<'a>(
    error: *const pts_error_t,
    message: *mut c_char,
    message_len: usize,
) -> usize {
    let pts_error_t(ref e) = *null_ck!(&'a error);
    let string = e.to_string();
    utils::string(&string, message, message_len)
}
