use crate::pts_thread_pool_t;
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
