use crate::{
    filter::pts_filter_t, process_handle::pts_process_handle_t, pts_thread_pool_t, pts_token_t,
    string::pts_string_t,
};
use std::{mem, os::raw::c_void, ptr};

/// A scan keeping track of results scanned from memory.
pub struct pts_scan_t(ptscan::scan::Scan);

/// Creates and returns a new scan.
#[no_mangle]
pub extern "C" fn pts_scan_new<'a>(thread_pool: *const pts_thread_pool_t) -> *mut pts_scan_t {
    let pts_thread_pool_t(ref thread_pool) = *null_ck!(&'a thread_pool);
    into_ptr!(pts_scan_t(ptscan::scan::Scan::new(thread_pool).aligned()))
}

/// Close and free the scan.
#[no_mangle]
pub extern "C" fn pts_scan_free(scan: *mut pts_scan_t) {
    free!(scan);
}

/// An iterator over scan results.
pub struct pts_scan_results_iter_t(std::slice::Iter<'static, ptscan::scan::ScanResult>);

/// Create an iterator over the results of a scan.
///
/// # Safety
///
/// Modifying a collection while an iterate is open results in undefined behavior.
#[no_mangle]
pub extern "C" fn pts_scan_results_iter<'a>(
    scan: *const pts_scan_t,
) -> *mut pts_scan_results_iter_t {
    let pts_scan_t(ref scan) = *null_ck!(&'a scan);

    into_ptr!(pts_scan_results_iter_t(unsafe {
        mem::transmute(scan.results.iter())
    }))
}

/// Walk the iterator one step.
///
/// If no more elements are available NULL is returned.
#[no_mangle]
pub extern "C" fn pts_scan_results_next<'a>(
    iter: *mut pts_scan_results_iter_t,
) -> *const pts_scan_result_t {
    let pts_scan_results_iter_t(ref mut iter) = *null_ck!(&'a mut iter);

    match iter.next() {
        Some(next) => next as *const _ as *const pts_scan_result_t,
        None => ptr::null_mut(),
    }
}

/// Free the scan results iterator.
#[no_mangle]
pub extern "C" fn pts_scan_results_free(scan_results: *mut pts_scan_results_iter_t) {
    free!(scan_results);
}

/// A single scan result.
pub struct pts_scan_result_t(ptscan::scan::ScanResult);

/// Access a readable process identifier for the handle.
#[no_mangle]
pub extern "C" fn pts_scan_result_address<'a>(
    result: *const pts_scan_result_t,
    handle: *const pts_process_handle_t,
    out: *mut pts_string_t,
) {
    let pts_scan_result_t(ref result) = *null_ck!(&'a result);
    let handle = null_opt!(&'a handle).map(|r| &r.0);
    let out = null_ck!(&'a mut out);

    *out = pts_string_t::new(match handle {
        Some(handle) => result.address_display(handle).to_string(),
        None => result.address.to_string(),
    });
}

/// Access a readable process identifier for the handle.
#[no_mangle]
pub extern "C" fn pts_scan_result_value<'a>(
    result: *const pts_scan_result_t,
    out: *mut pts_string_t,
) {
    let pts_scan_result_t(ref result) = *null_ck!(&'a result);
    let out = null_ck!(&'a mut out);
    *out = pts_string_t::new(result.value.to_string());
}

/// Access a readable process identifier for the handle.
#[no_mangle]
pub extern "C" fn pts_scan_result_current<'a>(
    result: *const pts_scan_result_t,
    out: *mut pts_string_t,
) {
    let pts_scan_result_t(ref result) = *null_ck!(&'a result);
    let out = null_ck!(&'a mut out);

    *out = match result.current.as_ref() {
        Some(current) => pts_string_t::new(current.to_string()),
        None => pts_string_t::empty(),
    };
}

/// Create an iterator over the results of a scan.
///
/// # Safety
///
/// Modifying a collection while an iterate is open results in undefined behavior.
#[no_mangle]
pub extern "C" fn pts_scan_count<'a>(scan: *const pts_scan_t) -> usize {
    null_ck!(&'a scan).0.results.len()
}

type pts_scan_progress_report_fn = fn(*mut c_void, usize);

#[repr(C)]
pub struct pts_scan_progress_t {
    /// Called to indicate that the process is in progress.
    report: pts_scan_progress_report_fn,
}

/// Creates and returns a new scan.
#[no_mangle]
pub extern "C" fn pts_scan_scan<'a>(
    scan: *mut pts_scan_t,
    handle: *const pts_process_handle_t,
    filter: *const pts_filter_t,
    cancel: *const pts_token_t,
    progress: *const pts_scan_progress_t,
    data: *mut c_void,
) -> bool {
    let pts_scan_t(ref mut scan) = *null_ck!(&'a mut scan);
    let pts_process_handle_t(ref handle) = *null_ck!(&'a handle);
    let pts_filter_t(ref filter) = *null_ck!(&'a filter);
    let cancel = null_opt!(&'a cancel).map(|t| &t.0);
    let pts_scan_progress_t { report } = *null_ck!(&'a progress);

    let progress = RawProgress { data, report };

    if !scan.initial {
        scan.initial = true;

        try_last!(
            scan.initial_scan(&handle.process, &**filter, cancel, progress),
            false
        );
    } else {
        try_last!(
            scan.rescan(&handle.process, &**filter, cancel, progress),
            false
        );
    }

    true
}

/// Creates and returns a new scan.
#[no_mangle]
pub extern "C" fn pts_scan_refresh<'a>(
    scan: *mut pts_scan_t,
    handle: *const pts_process_handle_t,
    limit: usize,
    cancel: *const pts_token_t,
    progress: *const pts_scan_progress_t,
    data: *mut c_void,
) -> bool {
    let pts_scan_t(ref mut scan) = *null_ck!(&'a mut scan);
    let pts_process_handle_t(ref handle) = *null_ck!(&'a handle);
    let cancel = null_opt!(&'a cancel).map(|t| &t.0);
    let pts_scan_progress_t { report } = *null_ck!(&'a progress);

    let progress = RawProgress { data, report };

    try_last!(
        scan.refresh(&handle.process, limit, cancel, progress),
        false
    );
    true
}

/// A reporter implementation that adapts a raw reporter.
pub struct RawProgress {
    data: *mut c_void,
    report: pts_scan_progress_report_fn,
}

unsafe impl Send for RawProgress {}
unsafe impl Sync for RawProgress {}

impl ptscan::scan::Progress for RawProgress {
    fn report_bytes(&mut self, _: usize) -> Result<(), failure::Error> {
        Ok(())
    }

    fn report(&mut self, percentage: usize) -> Result<(), failure::Error> {
        (self.report)(self.data, percentage);
        Ok(())
    }
}
