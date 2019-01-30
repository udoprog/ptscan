use std::{ptr, vec};

pub struct SystemProcessesIter(vec::IntoIter<ptscan::ProcessId>);
/// An opaque process identifier.
pub struct ProcessId(pub(crate) ptscan::ProcessId);

/// Iterate over system processes. NULL is returned on errors.
#[no_mangle]
pub extern "C" fn pts_system_processes_iter<'a>() -> *mut SystemProcessesIter {
    let processes = try_last!(ptscan::system::processes(), ptr::null_mut());
    into_ptr!(SystemProcessesIter(processes.into_iter()))
}

/// Walk the process iterator one step.
///
/// At the end of the iteration NULL is returned.
#[no_mangle]
pub extern "C" fn pts_system_processes_next<'a>(iter: *mut SystemProcessesIter) -> *mut ProcessId {
    let SystemProcessesIter(ref mut iter) = *null_ck!(&'a mut iter);

    match iter.next() {
        Some(next) => into_ptr!(ProcessId(next)),
        None => ptr::null_mut(),
    }
}

/// Free the process iterator.
#[no_mangle]
pub extern "C" fn pts_system_processes_free<'a>(iter: *mut SystemProcessesIter) {
    free!(iter);
}
