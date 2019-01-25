use std::{cell::RefCell, ptr, slice, sync, os::raw::{c_char}};

thread_local!(static LAST_ERROR: RefCell<Option<failure::Error>> = RefCell::new(None));

macro_rules! into_ptr {
    ($expr:expr) => {
        Box::into_raw(Box::new($expr))
    };
}

macro_rules! try_last {
    ($expr:expr, $err:expr) => {
        match $expr {
            Ok(value) => value,
            Err(e) => {
                LAST_ERROR.with(|last_error| {
                    *last_error.borrow_mut() = Some(failure::Error::from(e));
                });

                return $err;
            }
        }
    };
}

pub struct ProcessHandle(ptscan::ProcessHandle);

/// Find a process by name.
///
/// If a process cannot be found, *out is left as NULL.
#[no_mangle]
pub extern "C" fn ptscan_process_handle_open_by_name(
    name: *const c_char,
    name_len: usize,
    out: *mut *mut ProcessHandle,
) -> bool {
    unsafe {
        let bytes = slice::from_raw_parts(name, name_len);
        let name = String::from_utf8_lossy(bytes);

        if let Some(process_handle) =
            try_last!(ptscan::ProcessHandle::open_by_name(name.as_ref()), false)
        {
            *out = into_ptr!(ProcessHandle(process_handle));
        } else {
            *out = ptr::null_mut();
        }

        true
    }
}

/// Free the ProcessHandle.
#[no_mangle]
pub extern "C" fn ptscan_process_handle_free(process_handle: *mut ProcessHandle) {
    unsafe {
        Box::from_raw(process_handle);
    }
}

pub struct ThreadPool(sync::Arc<rayon::ThreadPool>);

#[no_mangle]
pub extern "C" fn ptscan_thread_pool_new(out: *mut *mut ThreadPool) -> bool {
    unsafe {
        let thread_pool = try_last!(rayon::ThreadPoolBuilder::new().build(), false);
        *out = into_ptr!(ThreadPool(sync::Arc::new(thread_pool)));
        true
    }
}

#[no_mangle]
pub extern "C" fn ptscan_thread_pool_free(thread_pool: *mut ThreadPool) {
    unsafe {
        Box::from_raw(thread_pool);
    }
}

pub struct Scanner(ptscan::scanner::Scanner);

#[no_mangle]
pub extern "C" fn ptscan_scanner_new(thread_pool: *const ThreadPool, out: *mut *mut Scanner) -> bool {
    unsafe {
        let thread_pool = &*thread_pool;

        *out = into_ptr!(Scanner(ptscan::scanner::Scanner::new(&thread_pool.0,)));

        true
    }
}

#[no_mangle]
pub extern "C" fn ptscan_scanner_free(scanner: *mut Scanner) {
    unsafe {
        Box::from_raw(scanner);
    }
}

pub struct ScannerResults {
    ptr: *mut ptscan::scanner::ScanResult,
    cur: usize,
    len: usize,
}

pub struct ScanResult(*mut ptscan::scanner::ScanResult);

#[no_mangle]
pub extern "C" fn ptscan_scanner_results_iter(scanner: *mut Scanner, out: *mut *mut ScannerResults) {
    unsafe {
        let scanner = &mut *scanner;
        let len = scanner.0.results.len();

        *out = into_ptr!(ScannerResults {
            ptr: scanner.0.results.as_mut_ptr(),
            cur: 0,
            len,
        });
    };
}

#[no_mangle]
pub extern "C" fn ptscan_scanner_results_next(
    scanner_results: *mut ScannerResults,
    out: *mut *mut ScanResult,
) {
    unsafe {
        let scanner_results = &mut *scanner_results;

        if scanner_results.cur >= scanner_results.len {
            *out = ptr::null_mut();
            return;
        }

        let ptr = scanner_results.ptr.add(scanner_results.cur);
        scanner_results.cur += 1;
        *out = into_ptr!(ScanResult(ptr));
    }
}

#[no_mangle]
pub extern "C" fn ptscan_scanner_results_free(scanner_results: *mut ScannerResults) {
    unsafe {
        Box::from_raw(scanner_results);
    }
}
