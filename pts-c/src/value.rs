//! A value in a memory location.
use crate::{r#type::Type, string::StringT};

/// NB: has to be the same size as `ptscan::Value`.
#[repr(C)]
pub struct Value([u8; 24]);

/// Export the type as a string.
#[no_mangle]
pub extern "C" fn pts_value_type<'a>(value: *const Value) -> Type {
    let value = immediate_ck!(ptscan::Value, &'a value);
    into_immediate!(value.ty())
}

/// Export the value as a string.
#[no_mangle]
pub extern "C" fn pts_value_display<'a>(value: *const Value, out: *mut StringT) {
    let value = immediate_ck!(ptscan::Value, &'a value);
    let out = null_ck!(&'a mut out);
    *out = StringT::new(value.to_string());
}
