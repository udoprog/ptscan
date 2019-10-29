use crate::{
    address::Address, filter::Filter, process_handle::ProcessHandle, value::Value, values::Values,
    watch::Watch, ThreadPool, Token,
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
pub extern "C" fn pts_scan_result_at<'a>(
    scan: *const Scan,
    offset: usize,
    out: *mut ScanResult,
) -> bool {
    let Scan(ref scan) = *null_ck!(&'a scan);
    let out = from_immediate!(ptscan::ScanResult, &'a mut out);

    match scan.get(offset) {
        Some(result) => {
            *out = result;
            true
        }
        None => false,
    }
}

/// An iterator over scan results.
pub struct ScanResultsIter(ptscan::scan::Iter<'static>);

/// Create an iterator over the results of a scan.
///
/// # Safety
///
/// Modifying a collection while an iterate is open results in undefined behavior.
#[no_mangle]
pub extern "C" fn pts_scan_results_iter<'a>(scan: *const Scan) -> *mut ScanResultsIter {
    let Scan(ref scan) = *null_ck!(&'a scan);

    into_ptr!(ScanResultsIter(unsafe { mem::transmute(scan.iter()) }))
}

/// Walk the iterator one step.
///
/// If no more elements are available NULL is returned.
#[no_mangle]
pub extern "C" fn pts_scan_results_next<'a>(
    iter: *mut ScanResultsIter,
    out: *mut ScanResult,
) -> bool {
    let ScanResultsIter(ref mut iter) = *null_ck!(&'a mut iter);
    let out = null_ck!(&'a mut out);

    match iter.next() {
        Some(next) => {
            *out = unsafe { mem::transmute::<_, ScanResult>(next) };
            true
        }
        None => false,
    }
}

/// Free the scan results iterator.
#[no_mangle]
pub extern "C" fn pts_scan_results_free(pts_scan_results: *mut ScanResultsIter) {
    free!(pts_scan_results);
}

/// A single scan result.
/// NB: has to be the same size as `ptscan::ScanResult`.
#[repr(C)]
pub struct ScanResult([u8; 32]);

/// Convert the scan result into a watch.
#[no_mangle]
pub extern "C" fn pts_scan_result_as_watch<'a>(
    result: *const ScanResult,
    handle: *const ProcessHandle,
) -> *mut Watch {
    let result = from_immediate!(ptscan::ScanResult, &'a result);
    let handle = null_opt!(&'a handle).map(|h| &h.0);
    let watch = try_last!(result.as_watch(handle), ptr::null_mut());
    into_ptr!(Watch(watch))
}

/// Access a readable process identifier for the handle.
#[no_mangle]
pub extern "C" fn pts_scan_result_address<'a>(result: *const ScanResult) -> Address {
    let result = from_immediate!(ptscan::ScanResult, &'a result);
    into_immediate!(result.address)
}

/// Access the value for the scan result.
#[no_mangle]
pub extern "C" fn pts_scan_result_value<'a>(result: *const ScanResult) -> Value {
    let result = from_immediate!(ptscan::ScanResult, &'a result);
    into_immediate!(result.value)
}

/// Create an iterator over the results of a scan.
///
/// # Safety
///
/// Modifying a collection while an iterate is open results in undefined behavior.
#[no_mangle]
pub extern "C" fn pts_scan_count<'a>(scan: *const Scan) -> usize {
    null_ck!(&'a scan).0.len()
}

type ScanProgressReportFn = fn(*mut c_void, usize, u64);

#[repr(C)]
pub struct ScanProgress {
    /// Called to indicate that the process is in progress.
    report: ScanProgressReportFn,
}

impl ScanProgress {
    /// Convert into a proper progress implementation.
    pub fn as_progress(&self, data: *mut c_void) -> RawProgress {
        let ScanProgress { report } = *self;
        RawProgress { data, report }
    }
}

/// Creates and returns a new scan.
#[no_mangle]
pub extern "C" fn pts_scan_initial<'a>(
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
    let progress = null_ck!(&'a progress).as_progress(data);

    try_last!(
        scan.initial_scan(&handle.process, filter, cancel, progress),
        false
    );

    true
}

/// Performs a new scan based on a previous scan.
#[no_mangle]
pub extern "C" fn pts_scan_scan<'a>(
    scan: *const Scan,
    handle: *const ProcessHandle,
    filter: *const Filter,
    cancel: *const Token,
    progress: *const ScanProgress,
    data: *mut c_void,
) -> *mut Scan {
    let Scan(ref scan) = *null_ck!(&'a scan);
    let ProcessHandle(ref handle) = *null_ck!(&'a handle);
    let Filter(ref filter) = *null_ck!(&'a filter);
    let cancel = null_opt!(&'a cancel).map(|t| &t.0);
    let progress = null_ck!(&'a progress).as_progress(data);

    let scan = try_last!(
        scan.scan(&handle.process, filter, cancel, progress),
        ptr::null_mut()
    );

    into_ptr!(Scan(scan))
}

/// Creates and returns a new scan.
/// deprecated: use `pts_process_handle_read_memory`
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
    let progress = null_ck!(&'a progress).as_progress(data);

    try_last!(
        handle.process.refresh_values(
            &*scan.thread_pool,
            &scan.addresses,
            values,
            cancel,
            progress
        ),
        false
    );

    true
}

/// Get a copy of the values contained in the scan.
#[no_mangle]
pub extern "C" fn pts_scan_values<'a>(scan: *const Scan, limit: usize) -> *mut Values {
    let Scan(ref scan) = *null_ck!(&'a scan);
    let end = usize::min(scan.values.len(), limit);
    into_ptr!(Values(scan.values.clone_slice(0, end)))
}

/// A reporter implementation that adapts a raw reporter.
pub struct RawProgress {
    data: *mut c_void,
    report: ScanProgressReportFn,
}

unsafe impl Send for RawProgress {}
unsafe impl Sync for RawProgress {}

impl ptscan::scan::Progress for RawProgress {
    fn report_bytes(&mut self, _: ptscan::Size) -> anyhow::Result<()> {
        Ok(())
    }

    fn report(&mut self, percentage: usize, count: u64) -> anyhow::Result<()> {
        (self.report)(self.data, percentage, count);
        Ok(())
    }
}
