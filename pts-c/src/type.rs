use crate::{string::StringT, utils};
use std::os::raw::c_char;

#[repr(C)]
pub struct Type(u8);

/// Parse a string as a filter.
///
/// Returns NULL and sets error accordingly on failure.
#[no_mangle]
pub extern "C" fn pts_type_parse<'a>(input: *const c_char, input_len: usize) -> Type {
    let input = utils::lossy_string(input, input_len);
    into_immediate!(ptscan::Type::from_string(&input))
}

/// Export the value as a string.
#[no_mangle]
pub extern "C" fn pts_type_display<'a>(ty: *const Type, out: *mut StringT) {
    let ty = immediate_ck!(ptscan::Type, &'a ty);
    let out = null_ck!(&'a mut out);
    *out = StringT::new(ty.to_string());
}

/// Export the value as a human-readable string.
#[no_mangle]
pub extern "C" fn pts_type_human_display<'a>(ty: *const Type, out: *mut StringT) {
    let ty = immediate_ck!(ptscan::Type, &'a ty);
    let out = null_ck!(&'a mut out);
    *out = StringT::new(ty.human_display().to_string());
}
