use crate::value::{Type, Value};

/// A collection of scan values that can be populated through e.g. scan_refresh.
pub struct Values(pub(crate) ptscan::Values);

/// Create a new values container.
#[no_mangle]
pub extern "C" fn pts_values_new<'a>() -> *mut Values {
    into_ptr!(Values(ptscan::Values::new()))
}

/// Push a value.
#[no_mangle]
pub extern "C" fn pts_values_push<'a>(values: *mut Values, value: Value) {
    let Values(ref mut values) = *null_ck!(&'a mut values);
    values.push(immediate_ck!(ptscan::Value, value));
}

/// Push a type.
/// The pushed value will be the type-default of the specified type.
#[no_mangle]
pub extern "C" fn pts_values_push_type<'a>(values: *mut Values, ty: Type) {
    let Values(ref mut values) = *null_ck!(&'a mut values);
    values.push_type(immediate_ck!(ptscan::Type, ty));
}

/// Get the value at the given position as a string.
#[no_mangle]
pub extern "C" fn pts_values_length<'a>(values: *const Values) -> usize {
    null_ck!(&'a values).0.len()
}

/// Get the value at the given position as a string.
#[no_mangle]
pub extern "C" fn pts_values_at<'a>(values: *const Values, pos: usize, out: *mut Value) -> bool {
    let Values(ref values) = *null_ck!(&'a values);
    let out = immediate_ck!(ptscan::Value, &'a mut out);

    if let Some(value) = values.get(pos) {
        *out = value;
        return true;
    }

    false
}

/// Clone the values collection.
#[no_mangle]
pub extern "C" fn pts_values_clone<'a>(values: *const Values) -> *mut Values {
    let Values(ref values) = *null_ck!(&'a values);
    into_ptr!(Values(values.clone()))
}

/// Free the scan values.
#[no_mangle]
pub extern "C" fn pts_values_free<'a>(values: *mut Values) {
    free!(values)
}
