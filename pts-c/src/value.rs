//! A value in a memory location.

pub const VALUE_SIZE: usize = 24;

#[repr(C)]
pub struct Value([u8; VALUE_SIZE]);

#[no_mangle]
pub extern "C" fn test_value() -> Value {
    Value([0u8; VALUE_SIZE])
}
