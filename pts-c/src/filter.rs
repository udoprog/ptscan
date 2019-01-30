use crate::{string::StringT, utils};
use std::{os::raw::c_char, ptr};

/// A filter.
pub struct Filter(pub(crate) Box<dyn ptscan::filter::Filter>);

/// Parse a string as a filter.
///
/// Returns NULL and sets error accordingly on failure.
#[no_mangle]
pub extern "C" fn pts_filter_parse<'a>(input: *const c_char, input_len: usize) -> *mut Filter {
    let input = utils::lossy_string(input, input_len);
    let res = ptscan::filter::parse(&input);
    let filter = try_last!(res, ptr::null_mut());
    let out = into_ptr!(Filter(filter));
    out
}

/// Free a filter.
#[no_mangle]
pub extern "C" fn pts_filter_free(filter: *mut Filter) {
    free!(filter);
}

/// Find a process by name.
///
/// If a process cannot be found, *out is set to NULL.
/// If an error is raised, false is returned and `error_last()` is updated accordingly.
#[no_mangle]
pub extern "C" fn pts_filter_display<'a>(filter: *const Filter, display: *mut StringT) {
    let Filter(ref filter) = *null_ck!(&'a filter);
    let display = null_ck!(&'a mut display);
    *display = StringT::new(filter.to_string());
}
