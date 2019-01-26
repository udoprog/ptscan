use std::{ptr, vec};

pub struct pts_system_processes_iter_t(vec::IntoIter<ptscan::ProcessId>);
pub struct pts_process_id_t(ptscan::ProcessId);

/// Iterate over system processes. NULL is returned on errors.
#[no_mangle]
pub extern "C" fn pts_system_processes_iter<'a>() -> *mut pts_system_processes_iter_t {
    let processes = try_last!(ptscan::system::processes(), ptr::null_mut());
    into_ptr!(pts_system_processes_iter_t(processes.into_iter()))
}

/// Walk the process iterator one step.
///
/// At the end of the iteration NULL is returned.
#[no_mangle]
pub extern "C" fn pts_system_processes_next<'a>(
    iter: *mut pts_system_processes_iter_t,
) -> *mut pts_process_id_t {
    let pts_system_processes_iter_t(ref mut iter) = *null_ck!(&'a mut iter);

    match iter.next() {
        Some(next) => into_ptr!(pts_process_id_t(next)),
        None => ptr::null_mut(),
    }
}

/// Free the process iterator.
#[no_mangle]
pub extern "C" fn pts_system_processes_free<'a>(iter: *mut pts_system_processes_iter_t) {
    free!(iter);
}
