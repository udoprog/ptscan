use crate::{string, utils};
use std::{os::raw::c_char, ptr};

/// A pointer.
pub struct pts_pointer_t(pub(crate) ptscan::pointer::Pointer);

/// Parse a string as a pointer.
///
/// Returns NULL and sets error accordingly on failure.
#[no_mangle]
pub extern "C" fn pts_pointer_parse<'a>(
    input: *const c_char,
    input_len: usize,
) -> *mut pts_pointer_t {
    let input = utils::lossy_string(input, input_len);
    let res = ptscan::pointer::Pointer::parse(&input);
    let pointer = try_last!(res, ptr::null_mut());
    let out = into_ptr!(pts_pointer_t(pointer));
    out
}

/// Free a pointer.
#[no_mangle]
pub extern "C" fn pts_pointer_free(pointer: *mut pts_pointer_t) {
    free!(pointer);
}

/// Find a process by name.
///
/// If a process cannot be found, *out is set to NULL.
/// If an error is raised, false is returned and `pts_error_last()` is updated accordingly.
#[no_mangle]
pub extern "C" fn pts_pointer_display<'a>(
    pointer: *const pts_pointer_t,
    display: *mut string::pts_string_t,
) {
    let pts_pointer_t(ref pointer) = *null_ck!(&'a pointer);
    let display = null_ck!(&'a mut display);
    *display = string::pts_string_t::new(pointer.to_string());
}
