use std::{os::raw::c_char, ptr, slice};

pub struct pts_process_handle_t(ptscan::ProcessHandle);

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

    if let Some(process_handle) =
        try_last!(ptscan::ProcessHandle::open_by_name(name.as_ref()), false)
    {
        *out = into_ptr!(pts_process_handle_t(process_handle));
    } else {
        *out = ptr::null_mut();
    }

    true
}

/// Close and free the process handle.
#[no_mangle]
pub extern "C" fn pts_process_handle_free(process_handle: *mut pts_process_handle_t) {
    free!(process_handle);
}
