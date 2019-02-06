use crate::{string::StringT, ty::Type, utils};
use std::{os::raw::c_char, ptr};

/// A filter.
pub struct Filter(pub(crate) ptscan::filter::Filter);

/// Parse a string as a filter.
///
/// Returns NULL and sets error accordingly on failure.
#[no_mangle]
pub extern "C" fn pts_filter_parse<'a>(
    input: *const c_char,
    input_len: usize,
    ty: Type,
) -> *mut Filter {
    let input = utils::lossy_string(input, input_len);
    let ty = immediate_ck!(ptscan::Type, ty);
    let res = ptscan::filter::parse(&input, ty);
    let filter = try_last!(res, ptr::null_mut());
    let out = into_ptr!(Filter(filter));
    out
}

/// Free a filter.
#[no_mangle]
pub extern "C" fn pts_filter_free(filter: *mut Filter) {
    free!(filter);
}

/// Display the filter as a string.
#[no_mangle]
pub extern "C" fn pts_filter_display<'a>(filter: *const Filter, display: *mut StringT) {
    let Filter(ref filter) = *null_ck!(&'a filter);
    let display = null_ck!(&'a mut display);
    *display = StringT::new(filter.matcher().to_string());
}

/// Return the type of a filter.
#[no_mangle]
pub extern "C" fn pts_filter_type<'a>(filter: *const Filter) -> Type {
    let Filter(ref filter) = *null_ck!(&'a filter);
    into_immediate!(filter.ty())
}
