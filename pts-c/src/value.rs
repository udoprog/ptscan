//! A value in a memory location.

/// Free the underlying string.
#[no_mangle]
pub extern "C" fn pts_test_value<'a>() -> ptscan::Value {
    ptscan::Value::U32(32)
}

/// Free the underlying string.
#[no_mangle]
pub extern "C" fn pts_test_value2<'a>() -> ptscan::Value {
    ptscan::Value::U32(32)
}
