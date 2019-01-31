use crate::string::StringT;

/// A collection of addresses that can be populated.
pub struct Addresses(pub(crate) Vec<ptscan::Address>);

/// Get the value at the given position as a string.
#[no_mangle]
pub extern "C" fn pts_addresses_length<'a>(addresses: *const Addresses) -> usize {
    null_ck!(&'a addresses).0.len()
}

/// Get the value at the given position as a string.
#[no_mangle]
pub extern "C" fn pts_addresses_value_at<'a>(
    addresses: *const Addresses,
    pos: usize,
    out: *mut StringT,
) {
    let Addresses(ref addresses) = *null_ck!(&'a addresses);
    let out = null_ck!(&'a mut out);

    *out = StringT::new(match addresses.get(pos) {
        Some(value) => value.to_string(),
        None => String::from(""),
    });
}

/// Free the scan addresses.
#[no_mangle]
pub extern "C" fn pts_addresses_free<'a>(addresses: *mut Addresses) {
    free!(addresses)
}
