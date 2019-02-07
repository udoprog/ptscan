//! Optimized storage for in-memory values.

use crate::{Type, Value};
use std::{fmt, iter};

const MASK: u64 = 0x00_ff_ff_ff_ff_ff_ff_ffu64;

macro_rules! assert_buffer {
    ($buf:expr, $type:ty) => {
        assert!(
            $buf.len() == std::mem::size_of::<$type>(),
            "buffer size mismatch: {} (buffer) != {} (expected)",
            $buf.len(),
            std::mem::size_of::<$type>()
        );
    };
}

// Helper macro to decode a simple value.
macro_rules! decode_value {
    ($type:expr, $buf:expr) => {
        decode_value!(@inner $type, $buf,
            (u16, U16, read_u16),
            (i16, I16, read_i16),
            (u32, U32, read_u32),
            (i32, I32, read_i32),
            (u64, U64, read_u64),
            (i64, I64, read_i64),
            (u128, U128, read_u128),
            (i128, I128, read_i128),
        )
    };

    (
        @inner $type:expr, $buf:expr,
        $(($ty:ty, $member:ident, $read:ident),)*
    ) => {
        match $type {
        Type::None => Value::None,
        Type::U8 => Value::U8($buf[0]),
        Type::I8 => Value::I8($buf[0] as i8),
        $(Type::$member => {
            assert_buffer!($buf, $ty);
            let buf = $buf.as_ptr() as *const $ty;
            Value::$member(unsafe { *buf })
        },)*
        }
    };
}

/// Decode the given index.
#[inline]
fn decode_index(index: u64) -> (Type, usize) {
    let ty = Type::from_byte((index >> 56) as u8);
    let i = (index & MASK) as usize;
    (ty, i)
}

/// Encode an index.
#[inline]
fn encode_index(t: Type, i: usize) -> u64 {
    let i = i as u64;

    if i > MASK {
        panic!("index out of range");
    }

    ((t as u64) << 56) | i
}

#[derive(Clone, Default, PartialEq, Eq)]
pub struct Values {
    /// Index into the buffer.
    /// high byte = type
    /// 56 bits = index in corresponding buffer.
    index: Vec<u64>,
    /// Buffer with raw data.
    buffer: Vec<u8>,
    /// Scratch buffer for writing new values.
    scratch: [u8; 16],
}

impl Values {
    /// Create a new values.
    pub fn new() -> Values {
        Values::default()
    }

    /// Get the length of the values collection.
    pub fn len(&self) -> usize {
        self.index.len()
    }

    /// Clear the values collection.
    pub fn clear(&mut self) {
        self.index.clear();
        self.buffer.clear();
    }

    /// Clone a slice of available values.
    pub fn clone_slice(&self, start: usize, end: usize) -> Self {
        let len = self.index.len();
        assert!(start <= len, "start out of bounds `{} <= {}`", start, len);
        assert!(end <= len, "end out of bounds `{} <= {}`", end, len);

        let s = match self.index.get(start) {
            Some(s) => *s,
            None => return Values::default(),
        };

        let (start_ty, s) = decode_index(s);
        let s = s.saturating_sub(start_ty.size());

        let e = match end.checked_sub(1).and_then(|e| self.index.get(e)) {
            Some(e) => *e,
            None => return Values::default(),
        };

        let (end_ty, e) = decode_index(e);
        let e = e.saturating_add(end_ty.size());

        Values {
            index: self.index[start..end].to_vec(),
            buffer: self.buffer[s..e].to_vec(),
            scratch: Default::default(),
        }
    }

    /// Get the index.
    #[inline]
    pub fn get(&self, index: usize) -> Option<Value> {
        let index = self.index.get(index)?;
        let (ty, i) = decode_index(*index);
        Some(decode_value!(ty, &self.buffer[i..(i + ty.size())]))
    }

    /// Create an iterator over all values in collections.
    pub fn iter<'a>(&'a self) -> Iter<'a> {
        Iter {
            index: &self.index,
            buffer: &self.buffer,
        }
    }

    /// Create a mutable iterator over all values.
    pub fn iter_mut<'a>(&'a mut self) -> IterMut<'a> {
        IterMut {
            index: &mut self.index,
            buffer: &mut self.buffer,
            pos: 0,
        }
    }

    /// Push the given type onto the collection.
    /// The pushed value will be the zero-value of that type.
    pub fn push_type(&mut self, ty: Type) {
        self.push(ty.default());
    }

    /// Push a single value.
    pub fn push(&mut self, value: Value) {
        use std::mem;

        macro_rules! define {
            (
                $value:expr, $buf:expr,
                $(($ty:ty, $member:ident, $write:ident),)*
            ) => {
                match $value {
                Value::None => { self.index.push(0); return; },
                Value::U8(v) => { $buf.push(v); Type::U8 },
                Value::I8(v) => { $buf.push(v as u8); Type::I8 },
                $(
                Value::$member(v) => {
                    let o = self.scratch.as_mut_ptr() as *mut $ty;
                    unsafe { *o = v };
                    $buf.extend(&self.scratch[..mem::size_of::<$ty>()]);
                    Type::$member
                },
                )*
                }
            };
        }

        let i = self.buffer.len();

        let t = define! {
            value,
            self.buffer,
            (u16, U16, write_u16),
            (i16, I16, write_i16),
            (u32, U32, write_u32),
            (i32, I32, write_i32),
            (u64, U64, write_u64),
            (i64, I64, write_i64),
            (u128, U128, write_u128),
            (i128, I128, write_i128),
        };

        self.index.push(encode_index(t, i))
    }

    // Append the other values collection, the collection appended from will be cleared.
    pub fn append(&mut self, values: &mut Self) {
        let b = self.buffer.len();

        for i in values.index.drain(..) {
            let (t, i) = decode_index(i);

            self.index.push(match t {
                Type::None => 0,
                other => encode_index(other, b + i),
            });
        }

        self.buffer.append(&mut values.buffer);
    }
}

impl fmt::Debug for Values {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_list().entries(self.iter()).finish()
    }
}

impl iter::Extend<Value> for Values {
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = Value>,
    {
        for value in iter {
            self.push(value);
        }
    }
}

/// A single dynamic literal value.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ValueMut<'a> {
    ty: Type,
    index: &'a mut u64,
    buffer: &'a mut [u8],
}

impl<'a> ValueMut<'a> {
    /// Convert into a value.
    pub fn to_value(&self) -> Value {
        macro_rules! define {
            ($buf:expr, $(($member:ident, $ty:ty),)*) => {
                match self.ty {
                    Type::None => Value::None,
                    $(Type::$member => {
                        assert_buffer!($buf, $ty);
                        let buf = $buf.as_ptr() as *const $ty;
                        Value::$member(unsafe { *buf })
                    },)*
                }
            }
        }

        define! {
            self.buffer,
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
        }
    }

    /// Set the given position with the given value.
    pub fn set(&mut self, value: Value) -> bool {
        macro_rules! define {
            ($buf:expr, $value:expr, $(($member:ident, $ty:ty),)*) => {
                match self.ty {
                    $(Type::$member => {
                        assert_buffer!($buf, $ty);
                        match $value.cast::<$ty>() {
                            Some(val) => {
                                let buf = $buf.as_mut_ptr() as *mut $ty;
                                unsafe { *buf = val };
                                true
                            },
                            None => {
                                self.clear();
                                false
                            },
                        }
                    },)*
                    _ => {
                        false
                    }
                }
            }
        }

        define! {
            self.buffer, value,
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
        }
    }

    /// Clear the value at the given location.
    pub fn clear(&mut self) {
        *self.index = (Type::None as u64) << 56;
    }

    /// Get the type of the value.
    pub fn ty(&self) -> Type {
        self.ty
    }
}

impl<'a> IntoIterator for &'a mut Values {
    type Item = ValueMut<'a>;
    type IntoIter = IterMut<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<'a> IntoIterator for &'a Values {
    type Item = Value;
    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct Iter<'a> {
    index: &'a [u64],
    buffer: &'a [u8],
}

impl<'a> Iterator for Iter<'a> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        let (index, rest) = self.index.split_first()?;
        self.index = rest;

        let (ty, i) = decode_index(*index);
        let len = ty.size();

        let buffer = match len {
            0 => &[],
            len => &self.buffer[i..(i + len)],
        };

        Some(decode_value!(ty, buffer))
    }
}

pub struct IterMut<'a> {
    index: &'a mut [u64],
    buffer: &'a mut [u8],
    pos: usize,
}

impl<'a> Iterator for IterMut<'a> {
    type Item = ValueMut<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        use std::mem;

        let (index, rest) = mem::replace(&mut self.index, &mut []).split_first_mut()?;
        self.index = rest;

        let (ty, i) = decode_index(*index);
        let len = ty.size();

        let buffer = match len {
            0 => &mut [],
            len => {
                let offset = i - self.pos;
                let (buffer, rest) =
                    mem::replace(&mut self.buffer, &mut []).split_at_mut(offset + len);
                let buffer = &mut buffer[offset..];
                self.buffer = rest;
                self.pos += offset + len;
                buffer
            }
        };

        Some(ValueMut { index, buffer, ty })
    }
}

#[cfg(test)]
mod tests {
    use super::Values;
    use crate::value::Value;

    fn expected() -> Vec<Value> {
        vec![
            Value::None,
            Value::U8(1),
            Value::I8(2),
            Value::U16(3),
            Value::I16(4),
            Value::U32(5),
            Value::I32(6),
            Value::U64(7),
            Value::I64(8),
            Value::U128(9),
            Value::I128(10),
        ]
    }

    #[test]
    pub fn test_iter() {
        let mut a = Values::new();
        a.extend(expected());

        for (a, b) in a.iter().zip(expected()) {
            assert_eq!(a, b);
        }
    }

    #[test]
    pub fn test_extend() {
        let mut a = Values::new();

        for v in expected() {
            a.push(v);
        }

        let mut b = Values::new();
        b.extend(expected());

        assert_eq!(a, b);
        assert_eq!(expected(), a.iter().collect::<Vec<_>>());
        assert_eq!(expected(), b.iter().collect::<Vec<_>>());
    }

    #[test]
    pub fn test_append() {
        let mut a = Values::new();
        let mut expected = expected();

        a.extend(expected.iter().cloned());
        a.append(&mut a.clone());
        expected.extend(expected.iter().cloned().collect::<Vec<_>>());
        assert_eq!(expected, a.iter().collect::<Vec<_>>());
    }

    #[test]
    pub fn test_values() {
        let mut a = Values::new();
        let expected = expected();

        a.extend(expected.iter().cloned());

        for (i, v) in expected.iter().cloned().enumerate() {
            assert_eq!(Some(v), a.get(i));
        }

        assert_eq!(None, a.get(expected.len() + 1));
        assert_eq!(expected, a.iter().collect::<Vec<_>>());
        assert_eq!(a.len(), expected.len());

        let len = expected.len();

        assert_eq!(
            expected[3..8].to_vec(),
            a.clone_slice(3, 8).iter().collect::<Vec<_>>()
        );

        assert_eq!(
            expected[3..3].to_vec(),
            a.clone_slice(3, 3).iter().collect::<Vec<_>>()
        );

        assert_eq!(
            expected[0..0].to_vec(),
            a.clone_slice(0, 0).iter().collect::<Vec<_>>()
        );

        assert_eq!(expected, a.clone_slice(0, len).iter().collect::<Vec<_>>());
    }

    #[test]
    pub fn test_iter_mut() {
        let mut a = Values::new();
        let expected = expected();
        a.extend(expected.iter().cloned());

        for mut v in a.iter_mut() {
            v.clear();
        }

        let expected = expected
            .into_iter()
            .map(|_| Value::None)
            .collect::<Vec<_>>();

        assert_eq!(expected, a.iter().collect::<Vec<_>>());
    }
}
