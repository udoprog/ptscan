//! A value in a memory location.

/// NB: has to be the same size as `ptscan::Value`.
#[repr(C)]
pub struct Value([u8; 24]);

#[no_mangle]
pub extern "C" fn test_value() -> Value {
    Value([0u8; 24])
}
