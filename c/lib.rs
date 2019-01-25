use std::{cell::RefCell, mem, os::raw::c_char, ptr, slice, sync};

thread_local!(static LAST_ERROR: RefCell<Option<failure::Error>> = RefCell::new(None));

fn constrain<'a, T>(value: *const T) -> &'a T {
    unsafe { &*value }
}

fn constrain_mut<'a, T>(value: *mut T) -> &'a mut T {
    unsafe { &mut *value }
}

/// NULL check the given argument and convert into a reference with a bounded lifetime.
macro_rules! null_ck {
    (&$l:lifetime mut $expr:expr) => {{
        null_ck!(@test $expr, null_mut);
        $crate::constrain_mut::<$l, _>($expr)
    }};

    (&$l:lifetime $expr:expr) => {{
        null_ck!(@test $expr, null);
        $crate::constrain::<$l, _>($expr)
    }};

    (@test $expr:expr, $m:ident) => {
        if $expr == ptr::$m() {
            LAST_ERROR.with(|last_error| {
                *last_error.borrow_mut() = Some(failure::format_err!("received null pointer"));
            });

            return false;
        }
    };
}

/// Convert the given expression into a pointer.
macro_rules! into_ptr {
    ($expr:expr) => {
        Box::into_raw(Box::new($expr))
    };
}

/// Free the given expression by taking ownership of it, causing it to drop at the end of the scope.
macro_rules! free {
    ($expr:expr) => {
        // FIXME: should we panic on NULL?
        if $expr != ptr::null_mut() {
            unsafe {
                Box::from_raw($expr);
            }
        }
    };
}

/// Helper macro to handle errors.
///
/// Any raised error will be stored in thread-local memory and can be accessed using the corresponding error_*
/// functions.
macro_rules! try_last {
    ($expr:expr) => {
        match $expr {
            Ok(value) => value,
            Err(e) => {
                LAST_ERROR.with(|last_error| {
                    *last_error.borrow_mut() = Some(failure::Error::from(e));
                });

                return false;
            }
        }
    };
}

pub struct ProcessHandle(ptscan::ProcessHandle);

/// Find a process by name.
///
/// If a process cannot be found, *out is set to NULL.
#[no_mangle]
pub extern "C" fn ptscan_process_handle_open_by_name<'a>(
    name: *const c_char,
    name_len: usize,
    out: *mut *mut ProcessHandle,
) -> bool {
    let name = unsafe {
        let bytes = slice::from_raw_parts(name as *const u8, name_len);
        String::from_utf8_lossy(bytes)
    };

    let out = null_ck!(&'a mut out);

    if let Some(process_handle) = try_last!(ptscan::ProcessHandle::open_by_name(name.as_ref())) {
        *out = into_ptr!(ProcessHandle(process_handle));
    } else {
        *out = ptr::null_mut();
    }

    true
}

/// Close and free the ProcessHandle.
#[no_mangle]
pub extern "C" fn ptscan_process_handle_free(process_handle: *mut ProcessHandle) {
    free!(process_handle);
}

pub struct ThreadPool(sync::Arc<rayon::ThreadPool>);

/// Create a new thread pool.
#[no_mangle]
pub extern "C" fn ptscan_thread_pool_new<'a>(out: *mut *mut ThreadPool) -> bool {
    let out = null_ck!(&'a mut out);
    let thread_pool = try_last!(rayon::ThreadPoolBuilder::new().build());
    *out = into_ptr!(ThreadPool(sync::Arc::new(thread_pool)));
    true
}

/// Close and free the thread pool.
#[no_mangle]
pub extern "C" fn ptscan_thread_pool_free(thread_pool: *mut ThreadPool) {
    free!(thread_pool);
}

/// A scanner keeping track of results scanned from memory.
pub struct Scanner(ptscan::scanner::Scanner);

#[no_mangle]
pub extern "C" fn ptscan_scanner_new<'a>(
    thread_pool: *const ThreadPool,
    out: *mut *mut Scanner,
) -> bool {
    let ThreadPool(ref thread_pool) = *null_ck!(&'a thread_pool);
    let out = null_ck!(&'a mut out);
    *out = into_ptr!(Scanner(ptscan::scanner::Scanner::new(thread_pool)));
    true
}

/// Close and free the scanner.
#[no_mangle]
pub extern "C" fn ptscan_scanner_free(scanner: *mut Scanner) {
    free!(scanner);
}

/// An iterator over scan results.
pub struct ScannerResultsIter(std::slice::Iter<'static, ptscan::scanner::ScanResult>);

/// A single scan result.
pub struct ScanResult(*const ptscan::scanner::ScanResult);

/// Create an iterator over the results of a scan.
///
/// # Safety
///
/// Modifying a collection while an iterate is open results in undefined behavior.
#[no_mangle]
pub extern "C" fn ptscan_scanner_results_iter<'a>(
    scanner: *mut Scanner,
    out: *mut *mut ScannerResultsIter,
) -> bool {
    let scanner = null_ck!(&'a mut scanner);
    let out = null_ck!(&'a mut out);
    *out = into_ptr!(ScannerResultsIter(unsafe {
        mem::transmute(scanner.0.results.iter())
    }));
    true
}

/// Walk the iterator one step.
///
/// If no more elements are available *out is set to NULL, otherwise it is set to point to the next element.
#[no_mangle]
pub extern "C" fn ptscan_scanner_results_next<'a>(
    scanner_results: *mut ScannerResultsIter,
    out: *mut *mut ScanResult,
) -> bool {
    let scanner_results = null_ck!(&'a mut scanner_results);
    let out = null_ck!(&'a mut out);

    *out = match scanner_results.0.next() {
        Some(next) => into_ptr!(ScanResult(next as *const _)),
        None => ptr::null_mut(),
    };

    true
}

/// Free the scanner results iterator.
#[no_mangle]
pub extern "C" fn ptscan_scanner_results_free(scanner_results: *mut ScannerResultsIter) {
    free!(scanner_results);
}
