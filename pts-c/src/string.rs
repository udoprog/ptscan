use std::os::raw::c_char;

#[repr(C)]
pub struct pts_string_t {
    ptr: *mut c_char,
    len: usize,
    cap: usize,
}

impl pts_string_t {
    /// Output a string through C FFI.
    ///
    /// Guarantees that we don't copy the string at a char boundary.
    ///
    /// The recipient specifies a buffer and a maximum length, and we return the length of the string copied.
    pub fn new(string: String) -> pts_string_t {
        let mut b = string.into_bytes();

        let out = pts_string_t {
            ptr: b.as_mut_ptr() as *mut c_char,
            len: b.len(),
            cap: b.capacity(),
        };

        std::mem::forget(b);
        out
    }

    /// Output an empty string through C FFI.
    pub fn empty() -> pts_string_t {
        Self::new(String::from(""))
    }
}

/// Free the underlying string.
#[no_mangle]
pub extern "C" fn pts_string_free<'a>(string: *mut pts_string_t) {
    let string = null_ck!(&'a mut string);

    unsafe {
        Vec::from_raw_parts(string.ptr, string.len, string.cap);
    }
}
