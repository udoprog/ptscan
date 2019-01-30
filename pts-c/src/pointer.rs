use crate::{string::StringT, utils};
use std::{os::raw::c_char, ptr};

/// A pointer.
pub struct Pointer(pub(crate) ptscan::Pointer);

/// Parse a string as a pointer.
///
/// Returns NULL and sets error accordingly on failure.
#[no_mangle]
pub extern "C" fn pts_pointer_parse<'a>(input: *const c_char, input_len: usize) -> *mut Pointer {
    let input = utils::lossy_string(input, input_len);
    let res = ptscan::Pointer::parse(&input);
    let pointer = try_last!(res, ptr::null_mut());
    let out = into_ptr!(Pointer(pointer));
    out
}

/// Free a pointer.
#[no_mangle]
pub extern "C" fn pts_pointer_free(pointer: *mut Pointer) {
    free!(pointer);
}

/// Find a process by name.
///
/// If a process cannot be found, *out is set to NULL.
/// If an error is raised, false is returned and `error_last()` is updated accordingly.
#[no_mangle]
pub extern "C" fn pts_pointer_display<'a>(pointer: *const Pointer, display: *mut StringT) {
    let Pointer(ref pointer) = *null_ck!(&'a pointer);
    let display = null_ck!(&'a mut display);
    *display = StringT::new(pointer.to_string());
}
