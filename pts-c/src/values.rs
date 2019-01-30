use crate::string::StringT;

/// A collection of scan values that can be populated through e.g. scan_refresh.
pub struct Values(pub(crate) Vec<ptscan::Value>);

/// Create a new collection of scan values.
///
/// The collection will be set to the given size.
#[no_mangle]
pub extern "C" fn pts_values_new<'a>(size: usize) -> *mut Values {
    into_ptr!(Values(vec![ptscan::Value::None; size]))
}

/// Get the value at the given position as a string.
#[no_mangle]
pub extern "C" fn pts_values_length<'a>(values: *const Values) -> usize {
    null_ck!(&'a values).0.len()
}

/// Get the value at the given position as a string.
#[no_mangle]
pub extern "C" fn pts_values_value_at<'a>(values: *const Values, pos: usize, out: *mut StringT) {
    let Values(ref values) = *null_ck!(&'a values);
    let out = null_ck!(&'a mut out);

    *out = StringT::new(match values.get(pos) {
        Some(value) => value.to_string(),
        None => String::from(""),
    });
}

/// Free the scan values.
#[no_mangle]
pub extern "C" fn pts_values_free<'a>(values: *mut Values) {
    free!(values)
}
