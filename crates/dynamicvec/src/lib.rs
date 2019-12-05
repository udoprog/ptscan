//! Lightweight dynamic Value storage who's exact type is determined at runtime.
//!
//! This modules provides a collection which behaves remarkably similar to,
//!  Vec<T>, where T is an enum. Except, it can store the discriminator
//! separately from the data (which saves on _a lot_ of storage).
//!
//! T in this case is something discriminated by an implementor of
//! `DynamicType`. At runtime, this determines the behavior of the `DynamicVec`.

use std::{fmt, marker, mem, ptr};

/// Convert one dynamic value into another.
///
/// We make this trait external to allow for additional data to be associated
/// with the conversion.
pub trait DynamicConverter<T>
where
    T: DynamicType,
{
    fn convert(&self, ty: T, from: T::Value) -> T::Value;
}

pub trait DynamicType: fmt::Display + Copy + PartialEq {
    type Value;

    /// Get the size of the type.
    fn element_size(self) -> usize;

    /// Clone the underlying data of a dynamic vector.
    fn clone_data(self, data: &Vec<u8>, element_size: usize, len: usize) -> Vec<u8>;

    /// Perform an unchecked read which will clone the inner value.
    unsafe fn read_unchecked(self, ptr: *const u8) -> Self::Value;

    /// Write the given value to the unchecked memory location.
    unsafe fn write_unchecked(self, ptr: *mut u8, value: Self::Value);

    /// Drop a single element correctly.
    unsafe fn drop_element(self, hole: *mut u8);

    /// Drop the underlying data.
    fn drop(self, data: &mut Vec<u8>, len: &mut usize);
}

/// A dynamic collection of values, with the goal of supporting efficient delete
/// operations (through swap_remove), and as much contiguous memory as possible.
pub struct DynamicVec<T>
where
    T: DynamicType,
{
    ty: T,
    element_size: usize,
    data: Vec<u8>,
    len: usize,
}

impl<T> DynamicVec<T>
where
    T: DynamicType,
{
    /// Construct a new values collection.
    pub fn new(ty: T) -> Self {
        Self {
            ty,
            element_size: ty.element_size(),
            data: Vec::new(),
            len: 0,
        }
    }

    /// Creates a values collection with the given capacity.
    pub fn with_capacity(ty: T, cap: usize) -> Self {
        let element_size = ty.element_size();
        let cap = element_size.checked_mul(cap).expect("capacity overflowed");

        Self {
            ty,
            element_size,
            data: Vec::with_capacity(cap),
            len: 0,
        }
    }

    /// Construct a dynamic collection from its raw components.
    #[inline]
    pub unsafe fn from_raw_parts(ty: T, element_size: usize, data: Vec<u8>, len: usize) -> Self {
        Self {
            ty,
            element_size,
            data,
            len,
        }
    }

    /// Get the type of the collection.
    #[inline]
    pub fn ty(&self) -> T {
        self.ty
    }

    /// Get the size of a single element.
    #[inline]
    pub fn element_size(&self) -> usize {
        self.element_size
    }

    /// Access the underlying bytes being stored.
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        &self.data
    }

    /// Convert the current collection in place.
    ///
    /// This allows for some neat optimizations, like avoiding an allocation in
    /// case we are shrinking the collection.
    pub fn convert_in_place<C>(&mut self, converter: &C, ty: T)
    where
        C: DynamicConverter<T>,
    {
        unsafe {
            if self.ty == ty {
                return;
            }

            let element_size = ty.element_size();

            // shrinking can be done in-place.
            if element_size <= self.element_size {
                let mut dst = self.data.as_mut_ptr();
                let mut src = self.data.as_ptr();

                let new_len = element_size
                    .checked_mul(self.len)
                    .expect("length overflowed");

                for _ in 0..self.len {
                    let v = self.ty.read_unchecked(src);
                    let v = converter.convert(ty, v);
                    ty.write_unchecked(dst, v);
                    src = src.add(self.element_size);
                    dst = dst.add(element_size);
                }

                self.ty = ty;
                self.element_size = element_size;
                self.data.set_len(new_len);
            } else {
                let mut new = DynamicVec::with_capacity(ty, self.len);

                for v in self.iter() {
                    let v = v.read();
                    let v = converter.convert(ty, v);
                    new.push(v);
                }

                *self = new;
            }
        }
    }

    /// Construct from an accessor iterator.
    pub fn extend<'a, I>(&mut self, it: I)
    where
        I: IntoIterator<Item = Accessor<'a, T>>,
    {
        // TODO: utilize more efficient copying where possible. Accessors are
        // pointers into another DynamicVec, and if we assert that type type is the
        // same we can in most cases perform a plain memcpy.

        for v in it {
            self.push(v.read());
        }
    }

    /// Current set of values is empty.
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
    pub fn get(&self, index: usize) -> Option<T::Value> {
        // Safety note: We need to make sure we don't perform an operation which
        // overflows the data pointer.
        //
        // This is safe because the operation happens before a length check - if
        // an overflow was possible, it would have happened when we appended to
        // the collection.
        unsafe {
            if index >= self.len {
                return None;
            }

            let size = self.element_size;

            let ptr = self.data.as_ptr().add(index * size);
            let value = self.ty.read_unchecked(ptr);

            Some(value)
        }
    }

    /// Get accessor related to the given index.
    pub fn get_accessor(&self, index: usize) -> Option<Accessor<'_, T>> {
        // Safety note: We need to make sure we don't perform an operation which
        // overflows the data pointer.
        //
        // This is safe because the operation happens before a length check - if
        // an overflow was possible, it would have happened when we appended to
        // the collection.
        unsafe {
            if index >= self.len {
                return None;
            }

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
    pub unsafe fn get_mutator_unsafe(&self, index: usize) -> Option<Mutator<'_, T>> {
        if index >= self.len {
            return None;
        }

        // Safety note: We need to make sure we don't perform an operation which
        // overflows the data pointer.
        //
        // This is safe because the operation happens before a length check - if
        // an overflow was possible, it would have happened when we appended to
        // the collection.
        let ptr = (self.data.as_ptr() as *mut u8).add(index * self.element_size);

        Some(Mutator {
            ty: self.ty,
            ptr,
            _marker: marker::PhantomData,
        })
    }

    /// Push the given value onto the values collection.
    ///
    /// # Panics
    ///
    /// This will panic if the value has an incompatible type.
    pub fn push(&mut self, value: T::Value) {
        unsafe {
            if self.element_size == 0 {
                self.len = self.len.checked_add(1).expect("length overflowed");
                return;
            }

            let pos = self
                .len
                .checked_mul(self.element_size)
                .expect("position overflowed");

            let new_len = pos
                .checked_add(self.element_size)
                .expect("length overflowed");

            self.data.reserve(self.element_size);

            let ptr = self.data.as_mut_ptr().add(pos);
            self.ty.write_unchecked(ptr, value);
            self.data.set_len(new_len);
            self.len += 1;
        };
    }

    /// Append values from one collection to another,
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
        // Safety note: We need to make sure we don't perform an operation which
        // overflows the data pointer.
        //
        // This is safe because the operation happens before a length check - if
        // an overflow was possible, it would have happened when we appended to
        // the collection.
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
            self.ty.drop_element(hole);

            self.len -= 1;

            if self.len > 1 {
                let from = ptr.add(self.len * self.element_size);
                ptr::copy_nonoverlapping(from, hole, self.element_size);
            }

            self.data.set_len(self.len * self.element_size);
            true
        }
    }

    /// Iterate over the current collection, providing accessors as we go.
    pub fn iter(&self) -> Iter<'_, T> {
        let len = self.len.checked_mul(self.element_size).expect("overflow");

        Iter {
            ty: self.ty,
            start: self.data.as_ptr(),
            end: unsafe { self.data.as_ptr().add(len) },
            element_size: self.element_size,
            _marker: marker::PhantomData,
        }
    }
    
    /// Iterate over the current collection, providing copies of the underlying
    /// value as we go.
    pub fn iter_copied(&self) -> IterCopied<'_, T> where T::Value: Copy {
        let len = self.len.checked_mul(self.element_size).expect("overflow");

        IterCopied {
            ty: self.ty,
            start: self.data.as_ptr(),
            end: unsafe { self.data.as_ptr().add(len) },
            element_size: self.element_size,
            _marker: marker::PhantomData,
        }
    }

    /// Iterate mutably over the current collection.
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
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

impl<T> Clone for DynamicVec<T>
where
    T: DynamicType,
{
    fn clone(&self) -> Self {
        let data = self.ty.clone_data(&self.data, self.element_size, self.len);

        Self {
            ty: self.ty,
            element_size: self.element_size,
            data,
            len: self.len,
        }
    }
}

impl<T> Drop for DynamicVec<T>
where
    T: DynamicType,
{
    fn drop(&mut self) {
        self.ty.drop(&mut self.data, &mut self.len);
    }
}

pub struct Accessor<'a, T> {
    pub ty: T,
    ptr: *const u8,
    _marker: marker::PhantomData<&'a ()>,
}

unsafe impl<T> Send for Accessor<'_, T> where T: Send {}
unsafe impl<T> Sync for Accessor<'_, T> where T: Sync {}

impl<T> Accessor<'_, T>
where
    T: DynamicType,
{
    /// Read the corresponding value stored in the current accessor location.
    #[inline]
    pub fn read(&self) -> T::Value {
        unsafe { self.ty.read_unchecked(self.ptr) }
    }
}

pub struct Iter<'a, T> {
    ty: T,
    start: *const u8,
    end: *const u8,
    element_size: usize,
    _marker: marker::PhantomData<&'a mut ()>,
}

impl<'a, T> Iterator for Iter<'a, T>
where
    T: Copy,
{
    type Item = Accessor<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None;
        }

        let accessor = Accessor {
            ty: self.ty,
            ptr: self.start,
            _marker: marker::PhantomData,
        };

        self.start = unsafe { self.start.add(self.element_size) };
        Some(accessor)
    }
}

pub struct IterCopied<'a, T> {
    ty: T,
    start: *const u8,
    end: *const u8,
    element_size: usize,
    _marker: marker::PhantomData<&'a mut ()>,
}

impl<'a, T> Iterator for IterCopied<'a, T>
where
    T: DynamicType,
    T::Value: Copy,
{
    type Item = T::Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None;
        }

        unsafe {
            let value = self.ty.read_unchecked(self.start);
            self.start = self.start.add(self.element_size);
            Some(value)
        }
    }
}

pub struct Mutator<'a, T> {
    pub ty: T,
    ptr: *mut u8,
    _marker: marker::PhantomData<&'a mut ()>,
}

unsafe impl<T> Send for Mutator<'_, T> where T: Send {}

impl<T> Mutator<'_, T>
where
    T: DynamicType,
{
    /// Write the corresponding value to the mutator location.
    #[inline]
    pub fn write(&mut self, value: T::Value) {
        unsafe { self.ty.write_unchecked(self.ptr, value) }
    }

    /// Read the corresponding value stored in the mutator location.
    #[inline]
    pub fn read(&self) -> T::Value {
        unsafe { self.ty.read_unchecked(self.ptr) }
    }
}

pub struct IterMut<'a, T> {
    ty: T,
    start: *mut u8,
    end: *mut u8,
    element_size: usize,
    _marker: marker::PhantomData<&'a mut ()>,
}

impl<'a, T> Iterator for IterMut<'a, T>
where
    T: Copy,
{
    type Item = Mutator<'a, T>;

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
