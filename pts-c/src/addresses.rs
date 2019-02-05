use crate::address::Address;

/// A collection of addresses that can be populated.
pub struct Addresses(pub(crate) Vec<ptscan::Address>);

/// Create a new addresss container.
#[no_mangle]
pub extern "C" fn pts_addresses_new<'a>() -> *mut Addresses {
    into_ptr!(Addresses(Vec::new()))
}

/// Push a address.
#[no_mangle]
pub extern "C" fn pts_addresses_push<'a>(addresses: *mut Addresses, address: Address) {
    let Addresses(ref mut addresses) = *null_ck!(&'a mut addresses);
    addresses.push(immediate_ck!(ptscan::Address, address));
}

/// Get the address at the given position as a string.
#[no_mangle]
pub extern "C" fn pts_addresses_length<'a>(addresses: *const Addresses) -> usize {
    null_ck!(&'a addresses).0.len()
}

/// Get the address at the given position as a string.
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
