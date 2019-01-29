use crate::{string, utils};
use std::{os::raw::c_char, ptr};

/// A filter.
pub struct pts_filter_t(pub(crate) Box<dyn ptscan::filter::Filter>);

/// Parse a string as a filter.
///
/// Returns NULL and sets error accordingly on failure.
#[no_mangle]
pub extern "C" fn pts_filter_parse<'a>(
    input: *const c_char,
    input_len: usize,
) -> *mut pts_filter_t {
    let input = utils::lossy_string(input, input_len);
    let res = ptscan::filter::parse(&input);
    let filter = try_last!(res, ptr::null_mut());
    let out = into_ptr!(pts_filter_t(filter));
    out
}

/// Free a filter.
#[no_mangle]
pub extern "C" fn pts_filter_free(filter: *mut pts_filter_t) {
    free!(filter);
}

/// Find a process by name.
///
/// If a process cannot be found, *out is set to NULL.
/// If an error is raised, false is returned and `pts_error_last()` is updated accordingly.
#[no_mangle]
pub extern "C" fn pts_filter_display<'a>(
    filter: *const pts_filter_t,
    display: *mut string::pts_string_t,
) {
    let pts_filter_t(ref filter) = *null_ck!(&'a filter);
    let display = null_ck!(&'a mut display);
    *display = string::pts_string_t::new(filter.to_string());
}
