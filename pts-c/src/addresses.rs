use crate::address::Address;

/// A collection of addresses that can be populated.
pub struct Addresses(pub(crate) Vec<ptscan::Address>);

/// Get the value at the given position as a string.
#[no_mangle]
pub extern "C" fn pts_addresses_length<'a>(addresses: *const Addresses) -> usize {
    null_ck!(&'a addresses).0.len()
}

/// Get the value at the given position as a string.
#[no_mangle]
pub extern "C" fn pts_addresses_at<'a>(
    addresses: *const Addresses,
    pos: usize,
    out: *mut Address,
) -> bool {
    let Addresses(ref addresses) = *null_ck!(&'a addresses);
    let out = immediate_ck!(ptscan::Address, &'a mut out);

    if let Some(address) = addresses.get(pos) {
        *out = *address;
        return true;
    }

    false
}

/// Free the scan addresses.
#[no_mangle]
pub extern "C" fn pts_addresses_free<'a>(addresses: *mut Addresses) {
    free!(addresses)
}
