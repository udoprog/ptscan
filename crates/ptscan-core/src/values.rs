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
    Address, BitSet, Type, Value,
};
use dynamicvec::{DynamicType, DynamicVec};
use serde::{Deserialize, Serialize};
use std::{io, mem, ptr, slice};

impl DynamicType for Type {
    type Value = Value;

    fn element_size(self) -> usize {
        Type::element_size(self)
    }

    fn clone_data(self, data: &[u8]) -> Vec<u8> {
        match self {
            // Special cases where we need to take care to clone each element.
            Type::String(..) => unsafe { clone_data::<String>(data) },
            Type::Bytes(None) => unsafe { clone_data::<Vec<u8>>(data) },
            _ => data.to_vec(),
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
                match self {
                    Type::None => (),
                    $(
                    Type::$variant{..} => match value {
                        Value::$variant(value) => ptr::write(ptr as *mut _, value),
                        _ => ptr::write(ptr as *mut _, <$ty>::default()),
                    },
                    )*
                    Type::Pointer(width) => match value {
                        Value::Pointer(value) => width.write_unchecked(ptr, value),
                        _ => ptr::write(ptr as *mut _, Address::default()),
                    },
                    Type::Bytes(None) => match value {
                        Value::Bytes(b) => ptr::write(ptr as *mut _, b),
                        _ => ptr::write(ptr as *mut _, <Vec<u8>>::default()),
                    },
                    Type::Bytes(Some(len)) => match value {
                        Value::Bytes(b) if b.len() >= len => ptr::copy_nonoverlapping(b.as_ptr(), ptr, len),
                        Value::Bytes(b) => {
                            let copied = usize::min(b.len(), len);
                            ptr::copy_nonoverlapping(b.as_ptr(), ptr, copied);
                            let rest = b.len().saturating_sub(copied);

                            if rest > 0 {
                                ptr::write_bytes(ptr, 0u8, rest);
                            }
                        },
                        _ => ptr::write_bytes(ptr, 0u8, len),
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

    unsafe fn replace_unchecked(self, ptr: *mut u8, value: Self::Value) {
        macro_rules! write {
            ($(($variant:ident, $ty:ty)),*) => {
                match self {
                    Type::None => (),
                    $(
                    Type::$variant{..} => match value {
                        Value::$variant(value) => ptr::write(ptr as *mut _, value),
                        _ => ptr::write(ptr as *mut _, <$ty>::default()),
                    },
                    )*
                    Type::Pointer(width) => match value {
                        Value::Pointer(value) => width.write_unchecked(ptr, value),
                        _ => ptr::write(ptr as *mut _, Address::default()),
                    },
                    Type::String(..) => match value {
                        Value::String(b) => drop(ptr::replace(ptr as *mut _, b)),
                        _ => drop(ptr::replace(ptr as *mut _, <String>::default())),
                    },
                    Type::Bytes(None) => match value {
                        Value::Bytes(b) => drop(ptr::replace(ptr as *mut _, b)),
                        _ => drop(ptr::replace(ptr as *mut _, <Vec<u8>>::default())),
                    },
                    Type::Bytes(Some(len)) => match value {
                        Value::Bytes(b) if b.len() >= len => ptr::copy_nonoverlapping(b.as_ptr(), ptr, len),
                        Value::Bytes(b) => {
                            let copied = usize::min(b.len(), len);
                            ptr::copy_nonoverlapping(b.as_ptr(), ptr, copied);
                            let rest = b.len().saturating_sub(copied);

                            if rest > 0 {
                                ptr::write_bytes(ptr, 0u8, rest);
                            }
                        }
                        _ => ptr::write_bytes(ptr, 0u8, len),
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
            (F64, f64)
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

/// The values of scan results: either stored per result, or one constant
/// shared by every result, as after an exact-match initial scan.
#[derive(Clone)]
pub enum ScanValues {
    /// A value per result.
    Dense(Values),
    /// `len` results which all hold `value`, of type `ty`.
    Constant { ty: Type, value: Value, len: usize },
}

impl ScanValues {
    /// Whether values of `ty` can be stored as a constant: fixed-size
    /// integers and pointers, whose equal values are the same bytes and own
    /// no heap data, so a constant can be expanded byte by byte.
    pub fn can_be_constant(ty: Type) -> bool {
        is_plain(ty)
    }

    /// The type of the values.
    pub fn ty(&self) -> Type {
        match self {
            Self::Dense(values) => values.ty(),
            Self::Constant { ty, .. } => *ty,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Dense(values) => values.len(),
            Self::Constant { len, .. } => *len,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// The value at `index`.
    pub fn get(&self, index: usize) -> Option<Value> {
        match self {
            Self::Dense(values) => values.get(index),
            Self::Constant { value, len, .. } if index < *len => Some(value.clone()),
            Self::Constant { .. } => None,
        }
    }

    /// Iterate over the values in order.
    pub fn iter(&self) -> impl Iterator<Item = Value> + '_ {
        (0..self.len()).map_while(move |index| self.get(index))
    }

    /// Bytes used by the stored values; a constant uses none per result.
    pub fn bytes(&self) -> usize {
        match self {
            Self::Dense(values) => values.bytes(),
            Self::Constant { .. } => 0,
        }
    }

    /// Remove the values whose index is set in `marked`, keeping the order
    /// of the others.
    pub fn remove_marked(&mut self, marked: &BitSet) {
        match self {
            Self::Dense(values) => values.retain_by_index(|index| !marked.test(index)),
            Self::Constant { len, .. } => {
                *len = len.saturating_sub(marked.count_below(*len));
            }
        }
    }

    /// The values, if they are stored per result.
    pub fn as_dense(&self) -> Option<&Values> {
        match self {
            Self::Dense(values) => Some(values),
            Self::Constant { .. } => None,
        }
    }

    /// A value per result, which a constant is expanded into first.
    pub fn dense_mut(&mut self) -> Result<&mut Values, ScanValuesError> {
        if let Self::Constant { ty, value, len } = self {
            *self = Self::Dense(materialize(*ty, value, *len)?);
        }

        match self {
            Self::Dense(values) => Ok(values),
            Self::Constant { .. } => Err(ScanValuesError),
        }
    }
}

impl From<Values> for ScanValues {
    fn from(values: Values) -> Self {
        Self::Dense(values)
    }
}

/// A constant could not be expanded into a value per result.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScanValuesError;

impl std::fmt::Display for ScanValuesError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("constant values too large to expand")
    }
}

impl std::error::Error for ScanValuesError {}

/// Fixed-size primitives whose element bytes are the whole value, so a copy
/// of the bytes is a copy of the value.
fn is_plain(ty: Type) -> bool {
    matches!(
        ty,
        Type::U8
            | Type::I8
            | Type::U16
            | Type::I16
            | Type::U32
            | Type::I32
            | Type::U64
            | Type::I64
            | Type::U128
            | Type::I128
            | Type::Pointer(..)
    )
}

/// `len` copies of `value` as a value per result.
fn materialize(ty: Type, value: &Value, len: usize) -> Result<Values, ScanValuesError> {
    if !is_plain(ty) {
        return Err(ScanValuesError);
    }

    let mut one = Values::with_capacity(ty, 1);
    one.push(value.clone());
    let element_size = one.element_size();

    // NB: `repeat` panics when the size overflows, which is checked first.
    if element_size.checked_mul(len).is_none() {
        return Err(ScanValuesError);
    }

    let data = one.as_bytes().repeat(len);

    // Safety: `ty` owns no heap data, so `data` is `len` valid elements, each
    // a byte copy of the one pushed above.
    Ok(unsafe { Values::from_raw_parts(ty, element_size, data, len) })
}

impl RawTypeDeserialize for Type {
    unsafe fn raw_type_deserialize<R>(self, reader: &mut R, len: usize) -> io::Result<Vec<u8>>
    where
        R: io::Read,
    {
        Ok(match self {
            Type::String(..) => {
                deserialize_dynamic(reader, len, |vec| String::from_utf8_unchecked(vec))?
            }
            Type::Bytes(None) => deserialize_dynamic(reader, len, <Vec<u8>>::from)?,
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
        Ok(())
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
unsafe fn clone_data<T>(from: &[u8]) -> Vec<u8>
where
    T: Clone,
{
    let mut data = Vec::with_capacity(from.len());
    let mut dst = data.as_mut_ptr() as *mut T;
    let mut src = from.as_ptr() as *const T;
    let end = from.as_ptr().add(from.len()) as *const T;

    while src < end {
        ptr::write(dst, T::clone(&*src));
        dst = dst.add(1);
        src = src.add(1);
    }

    data.set_len(from.len());
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

    Ok(())
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
            buf.extend(std::iter::repeat_n(0u8, len - buf.len()));
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

    #[test]
    fn test_swap_remove() {
        let mut values = Values::new(Type::U32);

        for n in 0..4 {
            values.push(Value::U32(n));
        }

        let read = |values: &Values| values.iter().map(|v| v.read()).collect::<Vec<_>>();

        assert!(values.swap_remove(3));
        assert_eq!(read(&values), [Value::U32(0), Value::U32(1), Value::U32(2)]);
        assert!(values.swap_remove(0));
        assert_eq!(read(&values), [Value::U32(2), Value::U32(1)]);
        assert!(values.swap_remove(0));
        assert_eq!(read(&values), [Value::U32(1)]);
        assert!(values.swap_remove(0));
        assert!(values.is_empty());
        assert!(!values.swap_remove(0));
    }

    #[test]
    fn retain_by_index_keeps_order() {
        let read = |values: &Values| values.iter().map(|v| v.read()).collect::<Vec<_>>();

        let mut values = Values::new(Type::U32);

        for n in 0..10 {
            values.push(Value::U32(n));
        }

        let mut seen = Vec::new();
        values.retain_by_index(|index| {
            seen.push(index);
            index % 3 != 0
        });
        assert_eq!(seen, (0..10).collect::<Vec<_>>());
        assert_eq!(read(&values), [1, 2, 4, 5, 7, 8].map(Value::U32).to_vec());
        assert_eq!(values.bytes(), 6 * 4);

        values.retain_by_index(|_| true);
        assert_eq!(values.len(), 6);
        values.retain_by_index(|_| false);
        assert!(values.is_empty());
        assert_eq!(values.bytes(), 0);

        let mut none = Values::new(Type::None);

        for _ in 0..5 {
            none.push(Value::None);
        }

        none.retain_by_index(|index| index < 2);
        assert_eq!(none.len(), 2);
    }

    #[test]
    fn retain_by_index_drops_removed_strings() {
        let mut values = Values::new(Type::String(Encoding::default()));

        for n in 0..6 {
            values.push(Value::String(format!("value {n}")));
        }

        values.retain_by_index(|index| index == 1 || index == 4);
        assert_eq!(
            values.iter().map(|v| v.read()).collect::<Vec<_>>(),
            [
                Value::String("value 1".into()),
                Value::String("value 4".into())
            ]
        );

        // A copy owns its strings, so dropping both would double free if
        // retain had left a removed or moved string behind.
        let copy = values.clone();
        drop(values);
        assert_eq!(copy.len(), 2);
    }

    #[test]
    fn retain_by_index_survives_a_panic() {
        let mut values = Values::new(Type::String(Encoding::default()));

        for n in 0..6 {
            values.push(Value::String(format!("value {n}")));
        }

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            values.retain_by_index(|index| {
                assert!(index < 3, "stop");
                index != 1
            });
        }));
        assert!(result.is_err());

        // Visited: 0 kept, 1 dropped, 2 kept; 3.. untouched behind them.
        assert_eq!(
            values.iter().map(|v| v.read()).collect::<Vec<_>>(),
            [0, 2, 3, 4, 5]
                .map(|n| Value::String(format!("value {n}")))
                .to_vec()
        );
    }
}
