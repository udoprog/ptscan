//! Lightweight dynamic Value storage who's exact type is determined at runtime.
//!
//! This modules provides a collection which behaves remarkably similar to,
//!  Vec<T>, where T is an enum. Except, it can store the discriminator
//! separately from the data (which saves on _a lot_ of storage).
//!
//! T in this case is `Value`, and the discriminator is `Type`. Combined this
//! means we can store dynamically determined values with much less memory.

use crate::{Address, PointerInfo, Type, Value, ValueRef};
use std::{marker, mem, ptr, slice};

/// A dynamic collection of values, with the goal of supporting efficient delete
/// operations (through swap_remove), and as much contiguous memory as possible.
pub struct Values {
    pub ty: Type,
    pub element_size: usize,
    data: Vec<u8>,
    len: usize,
}

impl Values {
    /// Construct a new empty collection which shares the same characteristics
    /// as another.
    pub fn new_of(other: &Values) -> Self {
        Self {
            ty: other.ty,
            element_size: other.element_size,
            data: Vec::new(),
            len: 0,
        }
    }

    /// Construct a new values collection.
    pub fn new(ty: Type) -> Self {
        Self {
            ty,
            element_size: ty.element_size(),
            data: Vec::new(),
            len: 0,
        }
    }

    /// Creates a values collection with the given capacity.
    pub fn with_capacity(ty: Type, cap: usize) -> Self {
        let element_size = ty.element_size();
        let cap = element_size.checked_mul(cap).expect("capacity overflowed");

        Self {
            ty,
            element_size,
            data: Vec::with_capacity(cap),
            len: 0,
        }
    }

    /// Convert the current collection in place.
    ///
    /// This allows for some neat optimizations, like avoiding an allocation in
    /// case we are shrinking the collection.
    pub fn convert_in_place(&mut self, pointer: &impl PointerInfo, ty: Type) {
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
                    let v = Accessor::read_unchecked(self.ty, src);
                    let v = ty.convert(pointer, v);
                    Mutator::write_unchecked(ty, dst, v);
                    src = src.add(self.element_size);
                    dst = dst.add(element_size);
                }

                self.ty = ty;
                self.element_size = element_size;
                self.data.set_len(new_len);
            } else {
                let mut new = Values::with_capacity(ty, self.len);

                for v in self.iter() {
                    let v = v.read();
                    let v = ty.convert(pointer, v);
                    new.push(v);
                }

                *self = new;
            }
        }
    }

    /// Construct from an accessor iterator.
    pub fn extend<'a, I>(&mut self, it: I)
    where
        I: IntoIterator<Item = Accessor<'a>>,
    {
        // TODO: utilize more efficient copying where possible. Accessors are
        // pointers into another Values, and if we assert that type type is the
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
    pub fn get(&self, index: usize) -> Option<Value> {
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
            let value = Accessor::read_unchecked(self.ty, ptr);

            Some(value)
        }
    }

    /// Get accessor related to the given index.
    pub fn get_accessor(&self, index: usize) -> Option<Accessor<'_>> {
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
    pub unsafe fn get_mutator_unsafe(&self, index: usize) -> Option<Mutator<'_>> {
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

    /// Get the value as a reference at the given location.
    pub fn get_ref(&self, index: usize) -> Option<ValueRef<'_>> {
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
            let value = Accessor::as_ref_unchecked(self.ty, ptr);

            Some(value)
        }
    }

    /// Push the given value onto the values collection.
    ///
    /// # Panics
    ///
    /// This will panic if the value has an incompatible type.
    pub fn push(&mut self, value: Value) {
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
            Mutator::write_unchecked(self.ty, ptr, value);
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

            match self.ty {
                Type::String(..) => {
                    ptr::drop_in_place(hole as *mut String);
                }
                Type::Bytes(None) => {
                    ptr::drop_in_place(hole as *mut Vec<u8>);
                }
                _ => (),
            }

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

    /// Clone the internal data as type `T`.
    unsafe fn clone_data<T>(&self) -> Vec<u8>
    where
        T: Clone,
    {
        let len = self.len * self.element_size;
        let mut data = Vec::with_capacity(len);
        let mut dst = data.as_mut_ptr() as *mut T;
        let mut src = self.data.as_ptr() as *const T;

        for _ in 0..self.len {
            ptr::write(dst, T::clone(&*src));
            dst = dst.add(1);
            src = src.add(1);
        }

        data.set_len(len);
        data
    }

    /// Drop all elements in the backing vector as type `T`.
    unsafe fn drop_data<T>(&mut self) {
        let mut ptr = self.data.as_mut_ptr() as *mut T;

        for _ in 0..self.len {
            ptr::drop_in_place(ptr);
            ptr = ptr.add(1);
        }

        self.len = 0;
        self.data.set_len(0);
    }
}

impl Clone for Values {
    fn clone(&self) -> Self {
        match self.ty {
            // Special cases where we need to take care to clone each element.
            Type::String(..) => Self {
                ty: self.ty,
                element_size: self.element_size,
                data: unsafe { self.clone_data::<String>() },
                len: self.len,
            },
            Type::Bytes(None) => Self {
                ty: self.ty,
                element_size: self.element_size,
                data: unsafe { self.clone_data::<Vec<u8>>() },
                len: self.len,
            },
            _ => Self {
                ty: self.ty,
                element_size: self.element_size,
                data: self.data.clone(),
                len: self.len,
            },
        }
    }
}

impl Drop for Values {
    fn drop(&mut self) {
        match self.ty {
            Type::String(..) => unsafe { self.drop_data::<String>() },
            Type::Bytes(None) => unsafe { self.drop_data::<Vec<u8>>() },
            _ => (),
        }
    }
}

pub struct Accessor<'a> {
    pub ty: Type,
    ptr: *const u8,
    _marker: marker::PhantomData<&'a ()>,
}

unsafe impl Send for Accessor<'_> {}

unsafe impl Sync for Accessor<'_> {}

impl Accessor<'_> {
    /// Read the corresponding value stored in the current accessor location.
    #[inline]
    pub fn read(&self) -> Value {
        unsafe { Accessor::read_unchecked(self.ty, self.ptr) }
    }

    /// Get the given memory location as a reference.
    #[inline]
    pub fn as_ref(&self) -> ValueRef<'_> {
        unsafe { Self::as_ref_unchecked(self.ty, self.ptr) }
    }

    /// Read the given memory location as the specified type.
    ///
    /// This is HIGHLY unsafe, the caller must guarantee that the given memory
    /// location references a LIVE memory location with the correct memory
    /// structure.
    #[inline]
    unsafe fn read_unchecked(ty: Type, ptr: *const u8) -> Value {
        match ty {
            Type::None => Value::None,
            Type::Pointer(width) => Value::Pointer(width.read_unchecked(ptr)),
            Type::U8 => Value::U8(ptr::read(ptr as *const _)),
            Type::I8 => Value::I8(ptr::read(ptr as *const _)),
            Type::U16 => Value::U16(ptr::read(ptr as *const _)),
            Type::I16 => Value::I16(ptr::read(ptr as *const _)),
            Type::U32 => Value::U32(ptr::read(ptr as *const _)),
            Type::I32 => Value::I32(ptr::read(ptr as *const _)),
            Type::U64 => Value::U64(ptr::read(ptr as *const _)),
            Type::I64 => Value::I64(ptr::read(ptr as *const _)),
            Type::U128 => Value::U128(ptr::read(ptr as *const _)),
            Type::I128 => Value::I128(ptr::read(ptr as *const _)),
            Type::F32 => Value::F32(ptr::read(ptr as *const _)),
            Type::F64 => Value::F64(ptr::read(ptr as *const _)),
            Type::String(..) => Value::String(String::clone(&*(ptr as *const _))),
            Type::Bytes(None) => Value::Bytes(<Vec<u8>>::clone(&*(ptr as *const _))),
            Type::Bytes(Some(len)) => {
                let mut data = Vec::with_capacity(len);
                ptr::copy_nonoverlapping(ptr, data.as_mut_ptr(), len);
                data.set_len(len);
                Value::Bytes(data)
            }
        }
    }

    /// Punt the given memory location into a value reference.
    ///
    /// This is incredibuly unsafe since it punts the given memory location into
    /// appropriate type to construct a reference. It also allows you to
    /// construct the reference into any lifetime `'a`.
    #[inline]
    unsafe fn as_ref_unchecked<'a>(ty: Type, ptr: *const u8) -> ValueRef<'a> {
        macro_rules! as_ref {
            ($(($variant:ident, $variant_ty:ty)),*) => {
                match ty {
                    Type::None => ValueRef::None,
                    $(Type::$variant{..} => ValueRef::$variant(*(ptr as *const $variant_ty)),)*
                    Type::String(..) => ValueRef::String(String::as_str(&*(ptr as *const _))),
                    Type::Bytes(None) => ValueRef::Bytes(<Vec<u8>>::as_slice(&*(ptr as *const _))),
                    Type::Bytes(Some(len)) => ValueRef::Bytes(slice::from_raw_parts(ptr, len)),
                }
            }
        }

        as_ref! {
            (Pointer, Address),
            (U8, u8),
            (I8, i8),
            (U16, u16),
            (I16, i16),
            (U32, u32),
            (I32, i32),
            (U64, u64),
            (I64, i64),
            (U128, u128),
            (I128, i128),
            (F32, f32),
            (F64, f64)
        }
    }
}

pub struct Iter<'a> {
    ty: Type,
    start: *const u8,
    end: *const u8,
    element_size: usize,
    _marker: marker::PhantomData<&'a mut ()>,
}

impl<'a> Iterator for Iter<'a> {
    type Item = Accessor<'a>;

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

pub struct Mutator<'a> {
    pub ty: Type,
    ptr: *mut u8,
    _marker: marker::PhantomData<&'a mut ()>,
}

unsafe impl Send for Mutator<'_> {}

impl Mutator<'_> {
    /// Write the corresponding value to the mutator location.
    #[inline]
    pub fn write(&mut self, value: Value) {
        unsafe { Self::write_unchecked(self.ty, self.ptr, value) }
    }

    /// Read the corresponding value stored in the mutator location.
    #[inline]
    pub fn read(&self) -> Value {
        unsafe { Accessor::read_unchecked(self.ty, self.ptr) }
    }

    /// Get the given memory location as a reference.
    #[inline]
    pub fn as_ref(&self) -> ValueRef<'_> {
        unsafe { Accessor::as_ref_unchecked(self.ty, self.ptr) }
    }

    /// Write the given value to the unchecked memory location.
    unsafe fn write_unchecked(ty: Type, ptr: *mut u8, value: Value) {
        macro_rules! write {
            ($(($variant:ident, $ty:ty)),*) => {
                match (ty, value) {
                    (Type::None, ..) => (),
                    $(
                        (Type::$variant{..}, Value::$variant(value)) => {
                            ptr::write(ptr as *mut _, value);
                        },
                        (Type::$variant{..}, ..) => {
                            ptr::write(ptr as *mut _, <$ty>::default());
                        },
                    )*
                    (Type::Pointer(width), Value::Pointer(value)) => {
                        width.write_unchecked(ptr, value);
                    },
                    (Type::Pointer(..), ..) => {
                        ptr::write(ptr as *mut _, Address::default());
                    },
                    (Type::Bytes(None), Value::Bytes(b)) => {
                        ptr::write(ptr as *mut _, b);
                    },
                    (Type::Bytes(None), ..) => {
                        ptr::write(ptr as *mut _, <Vec<u8>>::default());
                    },
                    (Type::Bytes(Some(len)), Value::Bytes(b)) if b.len() >= len => {
                        ptr::copy_nonoverlapping(b.as_ptr(), ptr, len);
                    },
                    (Type::Bytes(Some(len)), ..) => {
                        ptr::write_bytes(ptr, 0u8, len);
                    },
                }
            }
        }

        write!(
            (U8, u8),
            (I8, i8),
            (U16, u16),
            (I16, i16),
            (U32, u32),
            (I32, i32),
            (U64, u64),
            (I64, i64),
            (U128, u128),
            (I128, i128),
            (F32, f32),
            (F64, f64),
            (String, String)
        );
    }
}

pub struct IterMut<'a> {
    ty: Type,
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

#[cfg(test)]
mod tests {
    use super::{Value, Values};
    use crate::{Encoding, Type};

    #[test]
    fn test_values() {
        let encoding = Encoding::default();
        let mut values = Values::new(Type::String(encoding), &8usize);
        let string = String::from("hello world");
        values.push(Value::String(string.clone()));
        assert_eq!(Some(Value::String(string)), values.get(0));
        assert_eq!(None, values.get(1));
    }

    #[test]
    fn test_iterators() {
        let mut values = Values::new(Type::U32, &8usize);
        values.push(Value::U32(42));

        assert_eq!(
            vec![Value::U32(42)],
            values.iter().map(|v| v.read()).collect::<Vec<_>>()
        );
    }
}
