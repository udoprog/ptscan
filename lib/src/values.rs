//! Lightweight dynamic Value storage who's exact type is determined at runtime.
//!
//! This modules provides a collection which behaves remarkably similar to,
//!  Vec<T>, where T is an enum. Except, it can store the discriminator
//! separately from the data (which saves on _a lot_ of storage).
//!
//! T in this case is `Value`, and the discriminator is `Type`. Combined this
//! means we can store dynamically determined values with much less memory.

use crate::{Address, PointerInfo, Type, Value};
use serde::{Deserialize, Serialize};
use std::{fmt, io, marker, mem, ptr, slice};

pub trait DynamicType: fmt::Display + Copy + PartialEq {
    type Value;

    /// Get the size of the type.
    fn element_size(self) -> usize;

    /// Convert one dynamic value into another.
    fn convert(self, pointer: &impl PointerInfo, value: Self::Value) -> Self::Value;

    /// Clone the underlying data of a dynamic vector.
    fn clone_data(self, data: &Vec<u8>, element_size: usize, len: usize) -> Vec<u8>;

    /// Perform an unchecked read which will clone the inner value.
    unsafe fn read_unchecked(self, ptr: *const u8) -> Self::Value;

    /// Write the given value to the unchecked memory location.
    unsafe fn write_unchecked(self, ptr: *mut u8, value: Self::Value);

    /// Drop a single element correctly.
    unsafe fn drop_element(self, hole: *mut u8);

    fn drop(self, data: &mut Vec<u8>, len: &mut usize);

    fn serialize<W>(self, writer: &mut W, data: &[u8], len: usize) -> anyhow::Result<()>
    where
        W: io::Write;

    unsafe fn deserialize<R>(self, reader: &mut R, len: usize) -> io::Result<Vec<u8>>
    where
        R: io::Read;
}

impl DynamicType for Type {
    type Value = Value;

    fn element_size(self) -> usize {
        Type::element_size(self)
    }

    fn convert(self, pointer: &impl PointerInfo, value: Self::Value) -> Self::Value {
        Type::convert(self, pointer, value)
    }

    fn clone_data(self, data: &Vec<u8>, element_size: usize, len: usize) -> Vec<u8> {
        match self {
            // Special cases where we need to take care to clone each element.
            Type::String(..) => unsafe { clone_data::<String>(data, element_size, len) },
            Type::Bytes(None) => unsafe { clone_data::<Vec<u8>>(data, element_size, len) },
            _ => data.clone(),
        }
    }

    /// Read the given memory location as the specified type.
    ///
    /// This is HIGHLY unsafe, the caller must guarantee that the given memory
    /// location references a LIVE memory location with the correct memory
    /// structure.
    #[inline]
    unsafe fn read_unchecked(self, ptr: *const u8) -> Self::Value {
        match self {
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

    unsafe fn write_unchecked(self, ptr: *mut u8, value: Self::Value) {
        macro_rules! write {
            ($(($variant:ident, $ty:ty)),*) => {
                match (self, value) {
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

    unsafe fn drop_element(self, hole: *mut u8) {
        match self {
            Type::String(..) => {
                ptr::drop_in_place(hole as *mut String);
            }
            Type::Bytes(None) => {
                ptr::drop_in_place(hole as *mut Vec<u8>);
            }
            _ => (),
        }
    }

    fn drop(self, data: &mut Vec<u8>, len: &mut usize) {
        match self {
            Type::String(..) => unsafe { drop_data::<String>(data, len) },
            Type::Bytes(None) => unsafe { drop_data::<Vec<u8>>(data, len) },
            _ => (),
        }
    }

    fn serialize<W>(self, writer: &mut W, data: &[u8], len: usize) -> anyhow::Result<()>
    where
        W: io::Write,
    {
        match self {
            Type::String(..) => unsafe {
                serialize_dynamic::<W, String>(writer, data.as_ptr() as *const _, len)?;
            },
            Type::Bytes(None) => unsafe {
                serialize_dynamic::<W, Vec<u8>>(writer, data.as_ptr() as *const _, len)?;
            },
            _ => {
                writer.write_all(data)?;
            }
        }

        Ok(())
    }

    unsafe fn deserialize<R>(self, reader: &mut R, len: usize) -> io::Result<Vec<u8>>
    where
        R: io::Read,
    {
        Ok(match self {
            Type::String(..) => deserialize_dynamic::<R, _, String>(reader, len, |vec| {
                String::from_utf8_unchecked(vec)
            })?,
            Type::Bytes(None) => deserialize_dynamic::<R, _, Vec<u8>>(reader, len, From::from)?,
            _ => Vec::new(),
        })
    }
}

/// A dynamic collection of values, with the goal of supporting efficient delete
/// operations (through swap_remove), and as much contiguous memory as possible.
pub struct Values<T = Type>
where
    T: DynamicType,
{
    pub ty: T,
    pub element_size: usize,
    data: Vec<u8>,
    len: usize,
}

impl<T> Values<T>
where
    T: DynamicType,
{
    /// Construct a new empty collection which shares the same characteristics
    /// as another.
    pub fn new_of(other: &Values<T>) -> Self {
        Self {
            ty: other.ty,
            element_size: other.element_size,
            data: Vec::new(),
            len: 0,
        }
    }

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

    /// Convert the current collection in place.
    ///
    /// This allows for some neat optimizations, like avoiding an allocation in
    /// case we are shrinking the collection.
    pub fn convert_in_place(&mut self, pointer: &impl PointerInfo, ty: T) {
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
                    let v = ty.convert(pointer, v);
                    ty.write_unchecked(dst, v);
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
        I: IntoIterator<Item = Accessor<'a, T>>,
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

    /// Iterate mutably over the current collection.
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

    /// Serialize to the given writer.
    pub fn serialize<W>(&self, writer: &mut W) -> anyhow::Result<()>
    where
        W: io::Write,
        T: serde::Serialize,
    {
        serde_cbor::to_writer(
            &mut *writer,
            &FileHeader {
                ty: self.ty,
                len: self.len,
                element_size: self.element_size,
            },
        )?;

        self.ty.serialize(writer, &self.data, self.len)?;
        return Ok(());

        #[derive(Serialize)]
        struct ImmediateValues<'a> {
            data: &'a [u8],
        }
    }

    /// Deserialize from the given reader.
    pub fn deserialize<R>(reader: &mut R) -> anyhow::Result<Self>
    where
        R: io::Read,
        for<'de> T: serde::Deserialize<'de>,
    {
        let header = serde_cbor::from_reader::<FileHeader<T>, _>(&mut *reader)?;
        let data = unsafe { header.ty.deserialize(reader, header.len)? };

        Ok(Self {
            ty: header.ty,
            len: header.len,
            element_size: header.element_size,
            data,
        })
    }
}

impl Clone for Values {
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

impl<T> Drop for Values<T>
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

/// Drop all elements in the backing vector as type `T`.
unsafe fn drop_data<T>(data: &mut Vec<u8>, len: &mut usize) {
    let mut ptr = data.as_mut_ptr() as *mut T;

    for _ in 0..*len {
        ptr::drop_in_place(ptr);
        ptr = ptr.add(1);
    }

    *len = 0;
    data.set_len(0);
}

/// Clone the internal data as type `T`.
unsafe fn clone_data<T>(from: &Vec<u8>, element_size: usize, len: usize) -> Vec<u8>
where
    T: Clone,
{
    let len = len * element_size;
    let mut data = Vec::with_capacity(len);
    let mut dst = data.as_mut_ptr() as *mut T;
    let mut src = from.as_ptr() as *const T;

    for _ in 0..len {
        ptr::write(dst, T::clone(&*src));
        dst = dst.add(1);
        src = src.add(1);
    }

    data.set_len(len);
    data
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct FileHeader<T> {
    ty: T,
    len: usize,
    element_size: usize,
}

struct DynamicElementIndex {
    #[allow(unused)]
    offset: usize,
    len: usize,
}

/// Serialized a dynamically sized type to the given writer.
unsafe fn serialize_dynamic<W, T>(writer: &mut W, ptr: *const T, len: usize) -> anyhow::Result<()>
where
    W: io::Write,
    T: AsRef<[u8]>,
{
    let mut offset = 0usize;

    for i in 0..len {
        let ptr = ptr.add(i);
        let r = T::as_ref(&*ptr);

        let index = DynamicElementIndex {
            offset,
            len: r.len(),
        };

        writer.write_all(slice::from_raw_parts(
            &index as *const _ as *const u8,
            mem::size_of::<DynamicElementIndex>(),
        ))?;

        offset += r.len();
    }

    for i in 0..len {
        let ptr = ptr.add(i);
        writer.write_all(T::as_ref(&*ptr))?;
    }

    return Ok(());
}

/// Deserialize a dynamically sized type from the given reader.
unsafe fn deserialize_dynamic<R, F, T>(reader: &mut R, len: usize, decode: F) -> io::Result<Vec<u8>>
where
    R: io::Read,
    F: Fn(Vec<u8>) -> T,
{
    let mut data = Vec::<u8>::with_capacity(mem::size_of::<T>() * len);
    let mut buf = vec![0u8; mem::size_of::<DynamicElementIndex>()];

    let mut indices = Vec::new();

    for _ in 0..len {
        reader.read_exact(&mut buf)?;
        indices.push(ptr::read(buf.as_ptr() as *const DynamicElementIndex));
    }

    let ptr = data.as_mut_ptr() as *mut T;

    for (i, DynamicElementIndex { len, .. }) in indices.into_iter().enumerate() {
        if buf.len() < len {
            buf.extend(std::iter::repeat(0u8).take(len - buf.len()));
        }

        reader.read_exact(&mut buf)?;
        ptr::write(ptr.add(i), decode(buf.clone()));
    }

    data.set_len(data.capacity());
    Ok(data)
}

#[cfg(test)]
mod tests {
    use super::{Value, Values};
    use crate::{Encoding, Type};

    #[test]
    fn test_values() {
        let encoding = Encoding::default();
        let mut values = Values::new(Type::String(encoding));
        let string = String::from("hello world");
        values.push(Value::String(string.clone()));
        assert_eq!(Some(Value::String(string)), values.get(0));
        assert_eq!(None, values.get(1));
    }

    #[test]
    fn test_iterators() {
        let mut values = Values::new(Type::U32);
        values.push(Value::U32(42));

        assert_eq!(
            vec![Value::U32(42)],
            values.iter().map(|v| v.read()).collect::<Vec<_>>()
        );
    }
}
