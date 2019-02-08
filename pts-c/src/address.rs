//! An address.

use crate::{process_handle::ProcessHandle, string::StringT};

/// NB: has to be the same size as `ptscan::Address`.
#[repr(C)]
pub struct Address([u8; 8]);

/// Display the address as a string.
#[no_mangle]
pub extern "C" fn pts_address_display<'a>(
    address: *const Address,
    handle: *const ProcessHandle,
    out: *mut StringT,
) {
    let address = from_immediate!(ptscan::Address, &'a address);
    let handle = null_opt!(&'a handle).map(|r| &r.0);
    let out = null_ck!(&'a mut out);

    *out = StringT::new(match handle {
        Some(handle) => address.display(handle).to_string(),
        None => address.to_string(),
    });
}
