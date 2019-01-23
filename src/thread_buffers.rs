//! An unsafe data structure to maintain a set of buffers across threads.

use hashbrown::HashMap;
use std::{ops, sync, thread};

/// A collection of buffers mapped to threads.
pub struct ThreadBuffers {
    map: sync::RwLock<HashMap<thread::ThreadId, Buffer>>,
}

impl ThreadBuffers {
    pub fn new() -> ThreadBuffers {
        ThreadBuffers {
            map: sync::RwLock::new(hashbrown::HashMap::new()),
        }
    }

    /// Get mutable buffer for the current thread.
    ///
    /// # Safety
    ///
    /// This function is unsafe since it could allow two mutable references to the same memory.
    /// It is up to the user to prevent this from happening.
    pub unsafe fn get_mut<'a>(&'a self, len: usize) -> Result<Guard<'a>, failure::Error> {
        let id = thread::current().id();

        loop {
            {
                let inner = self
                    .map
                    .read()
                    .map_err(|_| failure::format_err!("lock poisoned"))?;

                if let Some(b) = inner.get(&id) {
                    let ptr = b.ptr.clone();
                    let cap = b.cap.clone();

                    if len <= cap {
                        return Ok(Guard {
                            _inner: inner,
                            ptr,
                            len,
                        });
                    }
                }
            }

            let mut inner = self
                .map
                .write()
                .map_err(|_| failure::format_err!("lock poisoned"))?;

            // we already have a buffer, but it's probably too small.
            if let Some(existing) = inner.get_mut(&id) {
                if len <= existing.cap {
                    continue;
                }

                existing.reserve(len - existing.cap);
                continue;
            }

            inner.insert(id, Buffer::from_vec(Vec::with_capacity(len)));
        }
    }
}

pub struct Guard<'a> {
    _inner: sync::RwLockReadGuard<'a, hashbrown::HashMap<thread::ThreadId, Buffer>>,
    ptr: *mut u8,
    len: usize,
}

impl<'a> Guard<'a> {
    /// Treat guard as mutable buffer.
    pub fn as_ref<'b>(&'b self) -> &'b [u8] {
        unsafe { std::slice::from_raw_parts(self.ptr, self.len) }
    }

    /// Treat guard as mutable buffer.
    pub fn as_mut<'b>(&'b mut self) -> &'b mut [u8] {
        unsafe { std::slice::from_raw_parts_mut(self.ptr, self.len) }
    }
}

impl<'a> ops::Deref for Guard<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<'a> ops::DerefMut for Guard<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

struct Buffer {
    ptr: *mut u8,
    len: usize,
    cap: usize,
}

unsafe impl Sync for Buffer {}
unsafe impl Send for Buffer {}

impl Buffer {
    /// Create a buffer from a vector.
    pub fn from_vec(mut data: Vec<u8>) -> Buffer {
        use std::mem;

        let buffer = Buffer {
            ptr: data.as_mut_ptr(),
            len: data.capacity(),
            cap: data.capacity(),
        };

        mem::forget(data);
        buffer
    }

    pub fn reserve(&mut self, additional: usize) {
        use std::mem;

        let mut v = unsafe { Vec::from_raw_parts(self.ptr, self.len, self.cap) };

        v.reserve(additional);
        let old = mem::replace(self, Buffer::from_vec(v));

        // NB: need to forget the old one to avoid de-allocating it.
        mem::forget(old);
    }
}

impl Drop for Buffer {
    fn drop(&mut self) {
        unsafe {
            Vec::from_raw_parts(self.ptr, self.len, self.cap);
        }
    }
}
