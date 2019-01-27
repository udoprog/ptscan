use crate::{filter::pts_filter_t, process_handle::pts_process_handle_t, pts_thread_pool_t};
use std::{mem, ptr};

/// A scanner keeping track of results scanned from memory.
pub struct pts_scanner_t(ptscan::scanner::Scanner);

/// Creates and returns a new scanner.
#[no_mangle]
pub extern "C" fn pts_scanner_new<'a>(thread_pool: *const pts_thread_pool_t) -> *mut pts_scanner_t {
    let pts_thread_pool_t(ref thread_pool) = *null_ck!(&'a thread_pool);
    into_ptr!(pts_scanner_t(ptscan::scanner::Scanner::new(thread_pool)))
}

/// Close and free the scanner.
#[no_mangle]
pub extern "C" fn pts_scanner_free(scanner: *mut pts_scanner_t) {
    free!(scanner);
}

/// An iterator over scan results.
pub struct pts_scanner_results_iter_t(std::slice::Iter<'static, ptscan::scanner::ScanResult>);

/// A single scan result.
pub struct pts_scanner_result_t(*const ptscan::scanner::ScanResult);

/// Create an iterator over the results of a scan.
///
/// # Safety
///
/// Modifying a collection while an iterate is open results in undefined behavior.
#[no_mangle]
pub extern "C" fn pts_scanner_results_iter<'a>(
    scanner: *const pts_scanner_t,
) -> *mut pts_scanner_results_iter_t {
    let pts_scanner_t(ref scanner) = *null_ck!(&'a scanner);

    into_ptr!(pts_scanner_results_iter_t(unsafe {
        mem::transmute(scanner.results.iter())
    }))
}

/// Walk the iterator one step.
///
/// If no more elements are available NULL is returned.
#[no_mangle]
pub extern "C" fn pts_scanner_results_next<'a>(
    iter: *mut pts_scanner_results_iter_t,
) -> *mut pts_scanner_result_t {
    let pts_scanner_results_iter_t(ref mut iter) = *null_ck!(&'a mut iter);

    match iter.next() {
        Some(next) => into_ptr!(pts_scanner_result_t(next as *const _)),
        None => ptr::null_mut(),
    }
}

/// Free the scanner results iterator.
#[no_mangle]
pub extern "C" fn pts_scanner_results_free(scanner_results: *mut pts_scanner_results_iter_t) {
    free!(scanner_results);
}

type pts_scanner_progress_report_fn = fn(usize);
type pts_scanner_progress_done_fn = fn(bool);

#[repr(C)]
pub struct pts_scanner_progress_t {
    /// Called to indicate that the process is in progress.
    report: pts_scanner_progress_report_fn,
    /// Called to indicate that the process is done.
    /// The argument is true if the process was interrupted or failed, pts_last_error will be set appropriately.
    done: pts_scanner_progress_done_fn,
}

/// Creates and returns a new scanner.
#[no_mangle]
pub extern "C" fn pts_scanner_scan<'a>(
    scanner: *mut pts_scanner_t,
    handle: *const pts_process_handle_t,
    filter: *const pts_filter_t,
    progress: *const pts_scanner_progress_t,
) -> bool {
    let pts_scanner_t(ref mut scanner) = *null_ck!(&'a mut scanner);
    let pts_process_handle_t(ref handle) = *null_ck!(&'a handle);
    let pts_filter_t(ref filter) = *null_ck!(&'a filter);
    let pts_scanner_progress_t { report, done } = *null_ck!(&'a progress);

    let progress = RawProgress { report, done };

    if !scanner.initial {
        try_last!(
            scanner.initial_scan(&handle.process, &**filter, progress),
            false
        );
        scanner.initial = true;
    } else {
        try_last!(scanner.rescan(&handle.process, &**filter, progress), false);
    }

    true
}

/// A reporter implementation that adapts a raw reporter.
pub struct RawProgress {
    report: pts_scanner_progress_report_fn,
    done: pts_scanner_progress_done_fn,
}

impl ptscan::scanner::Progress for RawProgress {
    fn report_bytes(&mut self, _: usize) -> Result<(), failure::Error> {
        Ok(())
    }

    fn report(&mut self, percentage: usize) -> Result<(), failure::Error> {
        println!("HERE");
        (self.report)(percentage);
        Ok(())
    }

    fn done(&mut self, interrupted: bool) -> Result<(), failure::Error> {
        (self.done)(interrupted);
        Ok(())
    }
}
