use crate::{pts_process_id_t, utils};
use std::{os::raw::c_char, ptr, slice};

pub struct pts_process_handle_t(ptscan::ProcessHandle);

/// Open a process handle by a pid.
///
/// If the process doesn't exist or access is denied *out is set to NULL.
/// If any error was raised, returns false and set errors appropriately.
#[no_mangle]
pub extern "C" fn pts_process_handle_open<'a>(
    pid: *const pts_process_id_t,
    out: *mut *mut pts_process_handle_t,
) -> bool {
    let pts_process_id_t(pid) = *null_ck!(&'a pid);
    let out = null_ck!(&'a mut out);

    *out = match try_last!(ptscan::ProcessHandle::open(pid), false) {
        Some(handle) => into_ptr!(pts_process_handle_t(handle)),
        None => ptr::null_mut(),
    };

    true
}

/// Find a process by name.
///
/// If a process cannot be found, *out is set to NULL.
/// If an error is raised, false is returned and `pts_error_last()` is updated accordingly.
#[no_mangle]
pub extern "C" fn pts_process_handle_open_by_name<'a>(
    name: *const c_char,
    name_len: usize,
    out: *mut *mut pts_process_handle_t,
) -> bool {
    let name = unsafe {
        let bytes = slice::from_raw_parts(name as *const u8, name_len);
        String::from_utf8_lossy(bytes)
    };

    let out = null_ck!(&'a mut out);

    if let Some(handle) = try_last!(ptscan::ProcessHandle::open_by_name(name.as_ref()), false) {
        *out = into_ptr!(pts_process_handle_t(handle));
    } else {
        *out = ptr::null_mut();
    }

    true
}

/// Access the name of the process handle.
///
/// If the process handle has no name, returns 0.
#[no_mangle]
pub extern "C" fn pts_process_handle_name<'a>(
    handle: *const pts_process_handle_t,
    name: *mut c_char,
    name_len: usize,
) -> usize {
    let pts_process_handle_t(ref handle) = *null_ck!(&'a handle);

    if let Some(n) = handle.name.as_ref() {
        utils::string(n, name, name_len)
    } else {
        0
    }
}

/// Access a readable process identifier for the handle.
#[no_mangle]
pub extern "C" fn pts_process_handle_pid<'a>(
    handle: *const pts_process_handle_t,
    pid: *mut c_char,
    pid_len: usize,
) -> usize {
    let pts_process_handle_t(ref handle) = *null_ck!(&'a handle);
    let p = handle.process.process_id().to_string();
    utils::string(&p, pid, pid_len)
}

/// Close and free the process handle.
#[no_mangle]
pub extern "C" fn pts_process_handle_free(handle: *mut pts_process_handle_t) {
    free!(handle);
}
