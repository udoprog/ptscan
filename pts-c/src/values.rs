use crate::{ty::Type, value::Value};

/// A collection of scan values that can be populated through e.g. scan_refresh.
pub struct Values(pub(crate) Vec<ptscan::Value>);

/// Create a new values container.
#[no_mangle]
pub extern "C" fn pts_values_new<'a>() -> *mut Values {
    into_ptr!(Values(Vec::new()))
}

/// Push a value.
#[no_mangle]
pub extern "C" fn pts_values_push<'a>(values: *mut Values, value: Value) {
    let Values(ref mut values) = *null_ck!(&'a mut values);
    values.push(from_immediate!(ptscan::Value, value));
}

/// Push a type.
/// The pushed value will be the type-default of the specified type.
#[no_mangle]
pub extern "C" fn pts_values_push_type<'a>(values: *mut Values, ty: Type) {
    let Values(ref mut values) = *null_ck!(&'a mut values);
    let ty = from_immediate!(ptscan::Type, ty);
    values.push(ty.default());
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
    let out = from_immediate!(ptscan::Value, &'a mut out);

    if let Some(value) = values.get(pos) {
        *out = value.clone();
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
