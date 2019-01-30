use crate::string::pts_string_t;

/// A collection of scan values that can be populated through e.g. pts_scan_refresh.
pub struct pts_values_t(pub(crate) Vec<ptscan::Value>);

/// Create a new collection of scan values.
///
/// The collection will be set to the given size.
#[no_mangle]
pub extern "C" fn pts_values_new<'a>(size: usize) -> *mut pts_values_t {
    into_ptr!(pts_values_t(vec![ptscan::Value::None; size]))
}

/// Get the value at the given position as a string.
#[no_mangle]
pub extern "C" fn pts_values_length<'a>(values: *const pts_values_t) -> usize {
    null_ck!(&'a values).0.len()
}

/// Get the value at the given position as a string.
#[no_mangle]
pub extern "C" fn pts_values_value_at<'a>(
    values: *const pts_values_t,
    pos: usize,
    out: *mut pts_string_t,
) {
    let pts_values_t(ref values) = *null_ck!(&'a values);
    let out = null_ck!(&'a mut out);

    *out = pts_string_t::new(match values.get(pos) {
        Some(value) => value.to_string(),
        None => String::from(""),
    });
}

/// Free the scan values.
#[no_mangle]
pub extern "C" fn pts_values_free<'a>(values: *mut pts_values_t) {
    free!(values)
}
