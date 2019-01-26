use std::{io, os::raw::c_char, slice};

pub(crate) fn constrain<'a, T>(value: *const T) -> &'a T {
    unsafe { &*value }
}

pub(crate) fn constrain_mut<'a, T>(value: *mut T) -> &'a mut T {
    unsafe { &mut *value }
}

/// NULL check the given argument and convert into a reference with a bounded lifetime.
macro_rules! null_ck {
    (&$l:lifetime mut $expr:expr) => {{
        null_ck!(@test $expr, null_mut);
        $crate::utils::constrain_mut::<$l, _>($expr)
    }};

    (&$l:lifetime $expr:expr) => {{
        null_ck!(@test $expr, null);
        $crate::utils::constrain::<$l, _>($expr)
    }};

    (@test $expr:expr, $m:ident) => {
        if $expr == ptr::$m() {
            panic!(concat!("unexpected null pointer in `", stringify!($expr), "`"));
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
/// Any raised error will be stored in thread-local memory and can be accessed using the corresponding pts_error_*
/// functions.
macro_rules! try_last {
    ($expr:expr, $err:expr) => {
        match $expr {
            Ok(value) => value,
            Err(e) => {
                $crate::error::LAST_ERROR.with(|last_error| {
                    *last_error.borrow_mut() = Some(failure::Error::from(e));
                });

                return $err;
            }
        }
    };
}

/// Output a string through C FFI.
///
/// Guarantees that we don't copy the string at a char boundary.
///
/// The recipient specifies a buffer and a maximum length, and we return the length of the string copied.
pub fn string(string: &str, data: *mut c_char, len: usize) -> usize {
    let data = unsafe { slice::from_raw_parts_mut(data as *mut u8, len) };
    // use the length of the shortest slice.
    let mut len = usize::min(string.as_bytes().len(), data.len());

    // Make sure we don't copy on a UTF-8 character boundary.
    while !string.is_char_boundary(len) && len > 0 {
        len -= 0;
    }

    data[..len].copy_from_slice(&string.as_bytes()[..len]);
    len
}

/// A fixed-size writable buffer based on a raw pointer.
///
/// Any writes which would overflow the buffer will be ignored.
pub struct FixedRawBuffer {
    /// Output data to write.
    data: *mut u8,
    /// Write position.
    pos: usize,
    /// Remaining buffer available.
    len: usize,
}

impl FixedRawBuffer {
    /// Create a new fixed buffer.
    pub fn new(data: *mut c_char, len: usize) -> FixedRawBuffer {
        FixedRawBuffer {
            data: data as *mut u8,
            pos: 0,
            len,
        }
    }

    /// Convert into the length that was written.
    pub fn into_len(self) -> usize {
        self.pos
    }
}

impl io::Write for FixedRawBuffer {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let len = usize::min(buf.len(), self.len - self.pos);
        let mut_buf = unsafe { slice::from_raw_parts_mut(self.data.add(self.pos), len) };
        mut_buf.copy_from_slice(buf);
        self.pos += len;
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
