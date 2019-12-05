//! Lightweight dynamic Value storage who's exact type is determined at runtime.
//!
//! This modules provides a collection which behaves remarkably similar to,
//!  Vec<T>, where T is an enum. Except, it can store the discriminator
//! separately from the data (which saves on _a lot_ of storage).
//!
//! T in this case is `Value`, and the discriminator is `Type`. Combined this
//! means we can store dynamically determined values with much less memory.

use crate::{
    raw_serde::{RawDeserialize, RawSerialize, RawTypeDeserialize, RawTypeSerialize},
    Address, Type, Value,
};
use dynamicvec::{DynamicType, DynamicVec};
use serde::{Deserialize, Serialize};
use std::{io, mem, ptr, slice};

impl DynamicType for Type {
    type Value = Value;

    fn element_size(self) -> usize {
        Type::element_size(self)
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
}

pub type Values = DynamicVec<Type>;

impl RawTypeDeserialize for Type {
    unsafe fn raw_type_deserialize<R>(self, reader: &mut R, len: usize) -> io::Result<Vec<u8>>
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

impl RawTypeSerialize for Type {
    fn raw_type_serialize<W>(self, writer: &mut W, data: &[u8], len: usize) -> io::Result<()>
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
}

impl<T> RawSerialize for DynamicVec<T>
where
    T: DynamicType + RawTypeSerialize + serde::Serialize,
{
    /// Serialize to the given writer.
    fn raw_serialize<W>(&self, writer: &mut W) -> anyhow::Result<()>
    where
        W: io::Write,
        T: serde::Serialize,
    {
        serde_cbor::to_writer(
            &mut *writer,
            &FileHeader {
                ty: self.ty(),
                len: self.len(),
                element_size: self.element_size(),
            },
        )?;

        self.ty()
            .raw_type_serialize(writer, self.as_bytes(), self.len())?;
        return Ok(());

        #[derive(Serialize)]
        struct ImmediateValues<'a> {
            data: &'a [u8],
        }
    }
}

impl<T> RawDeserialize for DynamicVec<T>
where
    T: DynamicType + RawTypeDeserialize,
    for<'de> T: serde::Deserialize<'de>,
{
    /// Deserialize from the given reader.
    unsafe fn raw_deserialize<R>(reader: &mut R) -> anyhow::Result<Self>
    where
        R: io::Read,
        for<'de> T: serde::Deserialize<'de>,
    {
        let header = serde_cbor::from_reader::<FileHeader<T>, _>(&mut *reader)?;
        let data = header.ty.raw_type_deserialize(reader, header.len)?;

        Ok(Self::from_raw_parts(
            header.ty,
            header.element_size,
            data,
            header.len,
        ))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct FileHeader<T> {
    ty: T,
    len: usize,
    element_size: usize,
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

struct DynamicElementIndex {
    #[allow(unused)]
    offset: usize,
    len: usize,
}

/// Serialized a dynamically sized type to the given writer.
unsafe fn serialize_dynamic<W, T>(writer: &mut W, ptr: *const T, len: usize) -> io::Result<()>
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
