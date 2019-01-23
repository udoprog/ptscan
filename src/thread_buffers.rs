//! A data structure to maintain a collection of persistent buffers across threads.

use alloc::raw_vec::RawVec;
use hashbrown::HashMap;
use std::{
    cell::{Cell, UnsafeCell},
    ops, ptr, slice,
    sync::{RwLock, RwLockReadGuard},
    thread::{self, ThreadId},
};

#[derive(Debug, failure::Fail)]
pub enum Error {
    #[fail(display = "lock poisoned")]
    LockPoisoned,
    #[fail(display = "buffer for thread {:?} is already borrowed", _0)]
    AlreadyBorrowed(ThreadId),
}

/// A collection of buffers mapped to threads.
pub struct ThreadBuffers {
    lock: RwLock<HashMap<ThreadId, UnsafeCell<Buffer>>>,
}

unsafe impl Sync for ThreadBuffers {}

impl ThreadBuffers {
    pub fn new() -> ThreadBuffers {
        ThreadBuffers {
            lock: RwLock::new(HashMap::new()),
        }
    }

    /// Access a mutable persistent buffer for the current thread.
    ///
    /// # Safety
    ///
    /// The returned slice is _not_ guaranteed to be initialized and might contain stale data.
    pub fn get_mut<'a>(&'a self, len: usize) -> Result<Guard<'a>, Error> {
        let id = thread::current().id();

        loop {
            // Read lock mode.
            // Check if we have a buffer in the map, and that it has the expected size.
            // Pointers are valid since the guard prevents modifying the map while the guards are in scope.
            {
                let guard = self.lock.read().map_err(|_| Error::LockPoisoned)?;

                if let Some(buffer) = guard.get(&id) {
                    let buf = unsafe { ptr::NonNull::new_unchecked(buffer.get()) };
                    let b = unsafe { buf.as_ref() };

                    if b.borrowed.get() {
                        return Err(Error::AlreadyBorrowed(id));
                    }

                    if len <= b.data.cap() {
                        b.borrowed.set(true);
                        return Ok(Guard { guard, len, buf });
                    }
                }
            }

            // Guaranteed unique access beyond this point since we are under a write lock.
            let mut guard = self.lock.write().map_err(|_| Error::LockPoisoned)?;

            // We already have a buffer, but it's probably too small.
            if let Some(buf) = guard.get(&id) {
                let b = unsafe { &mut *buf.get() };
                let cap = b.data.cap();

                if len <= cap {
                    continue;
                }

                b.data.reserve(cap, len - cap);
                continue;
            }

            let vec = RawVec::with_capacity(len);
            guard.insert(id, UnsafeCell::new(Buffer::from_raw_vec(vec)));
        }
    }
}

pub struct Guard<'a> {
    #[allow(unused)]
    guard: RwLockReadGuard<'a, HashMap<ThreadId, UnsafeCell<Buffer>>>,
    len: usize,
    buf: ptr::NonNull<Buffer>,
}

impl<'a> Drop for Guard<'a> {
    fn drop(&mut self) {
        unsafe {
            self.buf.as_ref().borrowed.set(false);
        }
    }
}

impl<'a> ops::Deref for Guard<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        unsafe { slice::from_raw_parts(self.buf.as_ref().data.ptr(), self.len) }
    }
}

impl<'a> ops::DerefMut for Guard<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { slice::from_raw_parts_mut(self.buf.as_ref().data.ptr(), self.len) }
    }
}

/// Helper struct for a buffer.
///
/// Keeps track of if the buffer is mutably borrowed.
struct Buffer {
    data: RawVec<u8>,
    borrowed: Cell<bool>,
}

impl Buffer {
    /// Create a buffer from a vector.
    fn from_raw_vec(data: RawVec<u8>) -> Buffer {
        Buffer {
            data: data,
            borrowed: Cell::new(false),
        }
    }
}
