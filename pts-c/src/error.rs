use crate::string::StringT;
use std::{cell::RefCell, ptr};

thread_local!(pub(crate) static LAST_ERROR: RefCell<Option<failure::Error>> = RefCell::new(None));

pub struct Error(failure::Error);

/// Returns the last error raised in this thread.
///
/// Returns NULL if no error was raised.
#[no_mangle]
pub extern "C" fn pts_error_last<'a>() -> *const Error {
    match LAST_ERROR.with(|e| e.borrow_mut().take()) {
        Some(e) => into_ptr!(Error(e)),
        None => ptr::null(),
    }
}

/// Write the last error message to the given string.
///
/// Returns the number of bytes copied.
#[no_mangle]
pub extern "C" fn pts_error_message<'a>(error: *const Error, message: *mut StringT) {
    use std::fmt::Write;

    let message = null_ck!(&'a mut message);
    let Error(ref e) = *null_ck!(&'a error);

    let mut m = String::new();

    let mut causes = e.iter_chain();

    if let Some(cause) = causes.next() {
        try_last!(writeln!(&mut m, "{}", cause), ());
    }

    while let Some(cause) = causes.next() {
        try_last!(writeln!(&mut m, "Caused by: {}", cause), ());
    }

    *message = StringT::new(m);
}
