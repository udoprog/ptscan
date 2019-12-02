//! Lightweight dynamic Address storage who's exact type is determined at runtime.
//!
//! This modules provides a collection which behaves remarkably similar to,
//!  Vec<T>, where T is an enum. Except, it can store the discriminator
//! separately from the data (which saves on _a lot_ of storage).
//!
//! T in this case is `Address`, and the discriminator is `PointerWidth`. Combined this
//! means we can store dynamically determined address with much less memory.

use crate::{Address, PointerWidth};
use std::{marker, mem, ptr};

/// A dynamic collection of address, with the goal of supporting efficient delete
/// operations (through swap_remove), and as much contiguous memory as possible.
pub struct Addresses {
    pub ty: PointerWidth,
    pub element_size: usize,
    data: Vec<u8>,
    len: usize,
}

impl Addresses {
    /// Construct a new empty collection which shares the same characteristics
    /// as another.
    pub fn new_of(other: &Addresses) -> Self {
        Self {
            ty: other.ty,
            element_size: other.element_size,
            data: Vec::new(),
            len: 0,
        }
    }

    /// Construct a new address collection.
    pub fn new(ty: PointerWidth) -> Self {
        Self {
            ty,
            element_size: ty.size(),
            data: Vec::new(),
            len: 0,
        }
    }

    /// Creates a address collection with the given capacity.
    pub fn with_capacity(ty: PointerWidth, cap: usize) -> Self {
        let element_size = ty.size();

        Self {
            ty,
            element_size,
            data: Vec::with_capacity(element_size * cap),
            len: 0,
        }
    }

    /// Convert the current collection in place.
    ///
    /// This allows for some neat optimizations, like avoiding an allocation in
    /// case we are shrinking the collection.
    pub fn convert_in_place(&mut self, ty: PointerWidth) {
        if self.ty == ty {
            return;
        }

        let element_size = ty.size();

        // shrinking can be done in-place.
        if element_size <= self.element_size {
            let mut dst = self.data.as_mut_ptr();
            let mut src = self.data.as_ptr();

            for _ in 0..self.len {
                unsafe {
                    let v = self.ty.read_unchecked(src);
                    ty.write_unchecked(dst, v);
                    src = src.add(self.element_size);
                    dst = dst.add(element_size);
                };
            }

            self.ty = ty;
            self.element_size = element_size;

            unsafe {
                self.data.set_len(element_size * self.len);
            }
        } else {
            let mut new = Addresses::with_capacity(ty, self.len);

            for v in self.iter() {
                new.push(v);
            }

            *self = new;
        }
    }

    /// Construct from an accessor iterator.
    pub fn extend<I>(&mut self, it: I)
    where
        I: IntoIterator<Item = Address>,
    {
        // TODO: utilize more efficient copying where possible. Accessors are
        // pointers into another Addresses, and if we assert that type type is the
        // same we can in most cases perform a plain memcpy.

        for v in it {
            self.push(v);
        }
    }

    /// Current set of address is empty.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Get the length of the collection.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Clear the data from this container.
    pub fn clear(&mut self) {
        self.len = 0;
        self.data.clear();
    }

    /// Bytes used by this collection.
    pub fn bytes(&self) -> usize {
        if self.element_size == 0 {
            return 0;
        }

        self.data.len()
    }

    /// Get the value at the given location.
    pub fn get(&self, index: usize) -> Option<Address> {
        if index >= self.len {
            return None;
        }

        let size = self.element_size;

        let value = unsafe {
            let ptr = self.data.as_ptr().add(index * size);
            self.ty.read_unchecked(ptr)
        };

        Some(value)
    }

    /// Get accessor related to the given index.
    pub fn get_accessor(&self, index: usize) -> Option<Accessor<'_>> {
        if index >= self.len {
            return None;
        }

        unsafe {
            let ptr = self.data.as_ptr().add(index * self.element_size);

            Some(Accessor {
                ty: self.ty,
                ptr,
                _marker: marker::PhantomData,
            })
        }
    }

    /// Get mutator related to the give position.
    ///
    /// # Safety
    /// The mutator allows mutation through a reference, among other things, the
    /// caller needs to make sure that:
    /// * Multiple mutators are not active to the same index.
    pub unsafe fn get_mutator_unsafe(&self, index: usize) -> Option<Mutator<'_>> {
        if index >= self.len {
            return None;
        }

        let ptr = (self.data.as_ptr() as *mut u8).add(index * self.element_size);

        Some(Mutator {
            ty: self.ty,
            ptr,
            _marker: marker::PhantomData,
        })
    }

    /// Push the given value onto the address collection.
    ///
    /// # Panics
    ///
    /// This will panic if the value has an incompatible type.
    pub fn push(&mut self, value: Address) {
        if self.element_size == 0 {
            self.len += 1;
            return;
        }

        self.data.reserve(self.element_size);
        let pos = self.len * self.element_size;

        unsafe {
            let ptr = self.data.as_mut_ptr().add(pos);
            self.ty.write_unchecked(ptr, value);
            self.data.set_len(pos + self.element_size);
        };

        self.len += 1;
    }

    /// Append address from one collection to another,
    pub fn append(&mut self, other: &mut Self) {
        if self.ty != other.ty {
            panic!(
                "trying to append incompatible collection of type {} to type {}",
                other.ty, self.ty
            );
        }

        self.len += mem::replace(&mut other.len, 0);
        self.data.append(&mut other.data);
    }

    /// Removes the element at the given index by swapping it with the element
    /// at the last place and truncating the collection.
    pub fn swap_remove(&mut self, index: usize) -> bool {
        unsafe {
            if index >= self.len {
                return false;
            }

            if self.element_size == 0 {
                self.len -= 1;
                return true;
            }

            let ptr = self.data.as_ptr();
            let hole = self.data.as_mut_ptr().add(index * self.element_size);
            self.len -= 1;

            if self.len > 1 {
                let from = ptr.add(self.len * self.element_size);
                ptr::copy_nonoverlapping(from, hole, self.element_size);
            }

            self.data.set_len(self.len * self.element_size);
            true
        }
    }

    /// Iterate mutably over the current collection.
    pub fn iter(&self) -> Iter<'_> {
        let len = self.len.checked_mul(self.element_size).expect("overflow");

        Iter {
            ty: self.ty,
            start: self.data.as_ptr(),
            end: unsafe { self.data.as_ptr().add(len) },
            element_size: self.element_size,
            _marker: marker::PhantomData,
        }
    }

    /// Iterate mutably over the current collection.
    pub fn iter_mut(&mut self) -> IterMut<'_> {
        let len = self.len.checked_mul(self.element_size).expect("overflow");

        IterMut {
            ty: self.ty,
            start: self.data.as_mut_ptr(),
            end: unsafe { self.data.as_mut_ptr().add(len) },
            element_size: self.element_size,
            _marker: marker::PhantomData,
        }
    }
}

impl Clone for Addresses {
    fn clone(&self) -> Self {
        Self {
            ty: self.ty,
            element_size: self.element_size,
            data: self.data.clone(),
            len: self.len,
        }
    }
}

pub struct Accessor<'a> {
    pub ty: PointerWidth,
    ptr: *const u8,
    _marker: marker::PhantomData<&'a ()>,
}

unsafe impl Send for Accessor<'_> {}
unsafe impl Sync for Accessor<'_> {}

impl Accessor<'_> {
    /// Read the corresponding value stored in the current accessor location.
    #[inline]
    pub fn read(&self) -> Address {
        unsafe { self.ty.read_unchecked(self.ptr) }
    }
}

pub struct Iter<'a> {
    ty: PointerWidth,
    start: *const u8,
    end: *const u8,
    element_size: usize,
    _marker: marker::PhantomData<&'a mut ()>,
}

impl<'a> Iterator for Iter<'a> {
    type Item = Address;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None;
        }

        let address = unsafe { self.ty.read_unchecked(self.start) };
        self.start = unsafe { self.start.add(self.element_size) };
        Some(address)
    }
}

pub struct Mutator<'a> {
    pub ty: PointerWidth,
    ptr: *mut u8,
    _marker: marker::PhantomData<&'a mut ()>,
}

unsafe impl Send for Mutator<'_> {}

impl Mutator<'_> {
    /// Write the corresponding value to the mutator location.
    #[inline]
    pub fn write(&mut self, value: Address) {
        unsafe { self.ty.write_unchecked(self.ptr, value) }
    }

    /// Read the corresponding value stored in the mutator location.
    #[inline]
    pub fn read(&self) -> Address {
        unsafe { self.ty.read_unchecked(self.ptr) }
    }
}

pub struct IterMut<'a> {
    ty: PointerWidth,
    start: *mut u8,
    end: *mut u8,
    element_size: usize,
    _marker: marker::PhantomData<&'a mut ()>,
}

impl<'a> Iterator for IterMut<'a> {
    type Item = Mutator<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None;
        }

        let mutator = Mutator {
            ty: self.ty,
            ptr: self.start,
            _marker: marker::PhantomData,
        };

        self.start = unsafe { self.start.add(self.element_size) };
        Some(mutator)
    }
}
