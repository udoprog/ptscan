use crate::{
    address::Address, addresses::Addresses, pointer::Pointer, scan::ScanProgress, string::StringT,
    system::ProcessId, utils, values::Values, ThreadPool, Token,
};
use std::{
    os::raw::{c_char, c_void},
    ptr,
};

/// Handle for a process.
pub struct ProcessHandle(pub(crate) ptscan::ProcessHandle);

/// Open a process handle by a pid.
///
/// If the process doesn't exist or access is denied *out is set to NULL.
/// If any error was raised, returns false and set errors appropriately.
#[no_mangle]
pub extern "C" fn pts_process_handle_open<'a>(
    pid: *const ProcessId,
    out: *mut *mut ProcessHandle,
) -> bool {
    let ProcessId(pid) = *null_ck!(&'a pid);
    let out = null_ck!(&'a mut out);

    *out = match try_last!(ptscan::ProcessHandle::open(pid), false) {
        Some(handle) => into_ptr!(ProcessHandle(handle)),
        None => ptr::null_mut(),
    };

    true
}

/// Find a process by name.
///
/// If a process cannot be found, *out is set to NULL.
/// If an error is raised, false is returned and `error_last()` is updated accordingly.
#[no_mangle]
pub extern "C" fn pts_process_handle_open_by_name<'a>(
    name: *const c_char,
    name_len: usize,
    out: *mut *mut ProcessHandle,
) -> bool {
    let name = utils::lossy_string(name, name_len);
    let out = null_ck!(&'a mut out);

    if let Some(handle) = try_last!(ptscan::ProcessHandle::open_by_name(name.as_ref()), false) {
        *out = into_ptr!(ProcessHandle(handle));
    } else {
        *out = ptr::null_mut();
    }

    true
}

/// Refresh known modules.
#[no_mangle]
pub extern "C" fn pts_process_handle_refresh_modules<'a>(handle: *mut ProcessHandle) -> bool {
    let ProcessHandle(ref mut handle) = *null_ck!(&'a mut handle);
    try_last!(handle.refresh_modules(), false);
    true
}

/// Refresh known threads.
#[no_mangle]
pub extern "C" fn pts_process_handle_refresh_threads<'a>(handle: *mut ProcessHandle) -> bool {
    let ProcessHandle(ref mut handle) = *null_ck!(&'a mut handle);
    try_last!(handle.refresh_threads(), false);
    true
}

/// Access the name of the process handle.
///
/// If the process handle has no name, returns 0.
#[no_mangle]
pub extern "C" fn pts_process_handle_name<'a>(handle: *const ProcessHandle, name: *mut StringT) {
    let name = null_ck!(&'a mut name);

    let ProcessHandle(ref handle) = *null_ck!(&'a handle);

    if let Some(n) = handle.name.as_ref() {
        *name = StringT::new(n.to_string());
    } else {
        *name = StringT::empty();
    }
}

/// Access a readable process identifier for the handle.
#[no_mangle]
pub extern "C" fn pts_process_handle_pid<'a>(handle: *const ProcessHandle, pid: *mut StringT) {
    let ProcessHandle(ref handle) = *null_ck!(&'a handle);
    let pid = null_ck!(&'a mut pid);
    *pid = StringT::new(handle.process.process_id().to_string());
}

/// Read the given memory locations from the process.
#[no_mangle]
pub extern "C" fn pts_process_handle_read_memory<'a>(
    handle: *const ProcessHandle,
    thread_pool: *const ThreadPool,
    addresses: *const Addresses,
    output: *mut Values,
    cancel: *const Token,
    progress: *const ScanProgress,
    data: *mut c_void,
) -> bool {
    let ProcessHandle(ref handle) = *null_ck!(&'a handle);
    let ThreadPool(ref thread_pool) = *null_ck!(&'a thread_pool);
    let Addresses(ref addresses) = *null_ck!(&'a addresses);
    let Values(ref mut output) = *null_ck!(&'a mut output);
    let cancel = null_opt!(&'a cancel).map(|t| &t.0);
    let progress = null_ck!(&'a progress).as_progress(data);

    try_last!(
        handle
            .process
            .read_memory(&*thread_pool, addresses, output, cancel, None, progress),
        false
    );

    true
}

/// Resolve a pointer.
#[no_mangle]
pub extern "C" fn pts_process_handle_read_pointer<'a>(
    handle: *const ProcessHandle,
    pointer: *const Pointer,
    out: *mut Address,
) -> bool {
    let ProcessHandle(ref handle) = *null_ck!(&'a handle);
    let Pointer(ref pointer) = *null_ck!(&'a pointer);
    let out = immediate_ck!(ptscan::Address, &'a mut out);

    match handle.read_pointer(pointer) {
        Some(address) => {
            *out = address;
            true
        }
        None => false,
    }
}

/// Close and free the process handle.
#[no_mangle]
pub extern "C" fn pts_process_handle_free(handle: *mut ProcessHandle) {
    free!(handle);
}
