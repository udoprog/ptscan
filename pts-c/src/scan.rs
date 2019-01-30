use crate::{
    filter::Filter, process_handle::ProcessHandle, string::StringT, values::Values, watch::Watch,
    ThreadPool, Token,
};
use std::{mem, os::raw::c_void, ptr};

/// A scan keeping track of results scanned from memory.
pub struct Scan(ptscan::scan::Scan);

/// Creates and returns a new scan.
#[no_mangle]
pub extern "C" fn pts_scan_new<'a>(thread_pool: *const ThreadPool) -> *mut Scan {
    let ThreadPool(ref thread_pool) = *null_ck!(&'a thread_pool);
    into_ptr!(Scan(ptscan::scan::Scan::new(thread_pool).aligned()))
}

/// Close and free the scan.
#[no_mangle]
pub extern "C" fn pts_scan_free(scan: *mut Scan) {
    free!(scan);
}

/// Access the scan result at the given offset.
#[no_mangle]
pub extern "C" fn pts_scan_result_at<'a>(scan: *const Scan, offset: usize) -> *const ScanResult {
    let Scan(ref scan) = *null_ck!(&'a scan);

    match scan.results.get(offset) {
        Some(result) => result as *const _ as *const ScanResult,
        None => ptr::null(),
    }
}

/// Convert the scan result into a watch.
#[no_mangle]
pub extern "C" fn pts_scan_result_as_watch<'a>(
    result: *const ScanResult,
    handle: *const ProcessHandle,
) -> *mut Watch {
    let ScanResult(ref result) = *null_ck!(&'a result);
    let handle = null_opt!(&'a handle).map(|h| &h.0);
    let watch = try_last!(result.as_watch(handle), ptr::null_mut());
    into_ptr!(Watch(watch))
}

/// An iterator over scan results.
pub struct ScanResultsIter(std::slice::Iter<'static, ptscan::scan::ScanResult>);

/// Create an iterator over the results of a scan.
///
/// # Safety
///
/// Modifying a collection while an iterate is open results in undefined behavior.
#[no_mangle]
pub extern "C" fn pts_scan_results_iter<'a>(scan: *const Scan) -> *mut ScanResultsIter {
    let Scan(ref scan) = *null_ck!(&'a scan);

    into_ptr!(ScanResultsIter(unsafe {
        mem::transmute(scan.results.iter())
    }))
}

/// Walk the iterator one step.
///
/// If no more elements are available NULL is returned.
#[no_mangle]
pub extern "C" fn pts_scan_results_next<'a>(iter: *mut ScanResultsIter) -> *const ScanResult {
    let ScanResultsIter(ref mut iter) = *null_ck!(&'a mut iter);

    match iter.next() {
        Some(next) => next as *const _ as *const ScanResult,
        None => ptr::null_mut(),
    }
}

/// Free the scan results iterator.
#[no_mangle]
pub extern "C" fn pts_scan_results_free(pts_scan_results: *mut ScanResultsIter) {
    free!(pts_scan_results);
}

/// A single scan result.
pub struct ScanResult(ptscan::scan::ScanResult);

/// Access a readable process identifier for the handle.
#[no_mangle]
pub extern "C" fn pts_scan_result_address<'a>(
    result: *const ScanResult,
    handle: *const ProcessHandle,
    out: *mut StringT,
) {
    let ScanResult(ref result) = *null_ck!(&'a result);
    let handle = null_opt!(&'a handle).map(|r| &r.0);
    let out = null_ck!(&'a mut out);

    *out = StringT::new(match handle {
        Some(handle) => result.address_display(handle).to_string(),
        None => result.address.to_string(),
    });
}

/// Access the value for the scan result.
#[no_mangle]
pub extern "C" fn pts_scan_result_value<'a>(result: *const ScanResult, out: *mut StringT) {
    let ScanResult(ref result) = *null_ck!(&'a result);
    let out = null_ck!(&'a mut out);
    *out = StringT::new(result.value.to_string());
}

/// Create an iterator over the results of a scan.
///
/// # Safety
///
/// Modifying a collection while an iterate is open results in undefined behavior.
#[no_mangle]
pub extern "C" fn pts_scan_count<'a>(scan: *const Scan) -> usize {
    null_ck!(&'a scan).0.results.len()
}

type ScanProgressReportFn = fn(*mut c_void, usize);

#[repr(C)]
pub struct ScanProgress {
    /// Called to indicate that the process is in progress.
    report: ScanProgressReportFn,
}

/// Creates and returns a new scan.
#[no_mangle]
pub extern "C" fn pts_scan_scan<'a>(
    scan: *mut Scan,
    handle: *const ProcessHandle,
    filter: *const Filter,
    cancel: *const Token,
    progress: *const ScanProgress,
    data: *mut c_void,
) -> bool {
    let Scan(ref mut scan) = *null_ck!(&'a mut scan);
    let ProcessHandle(ref handle) = *null_ck!(&'a handle);
    let Filter(ref filter) = *null_ck!(&'a filter);
    let cancel = null_opt!(&'a cancel).map(|t| &t.0);
    let ScanProgress { report } = *null_ck!(&'a progress);

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
    scan: *const Scan,
    handle: *const ProcessHandle,
    values: *mut Values,
    cancel: *const Token,
    progress: *const ScanProgress,
    data: *mut c_void,
) -> bool {
    let Scan(ref scan) = *null_ck!(&'a scan);
    let ProcessHandle(ref handle) = *null_ck!(&'a handle);
    let Values(ref mut values) = *null_ck!(&'a mut values);
    let cancel = null_opt!(&'a cancel).map(|t| &t.0);
    let ScanProgress { report } = *null_ck!(&'a progress);

    let progress = RawProgress { data, report };

    try_last!(
        scan.refresh(&handle.process, values, cancel, progress),
        false
    );

    true
}

/// A reporter implementation that adapts a raw reporter.
pub struct RawProgress {
    data: *mut c_void,
    report: ScanProgressReportFn,
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
