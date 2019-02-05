//! Optimized storage for in-memory values.

use crate::value::{Type, Value};
use std::{iter, marker, ptr};

const MASK: u64 = 0x00_ff_ff_ff_ff_ff_ff_ffu64;

/// Decode the given index.
#[inline]
fn decode_index(index: u64) -> (Type, usize) {
    let ty = Type::from_byte((index >> 56) as u8);
    let i = (index & MASK) as usize;
    (ty, i)
}

#[derive(Clone, Default)]
pub struct Values {
    // high byte = type
    // 56 bits = index in corresponding buffer.
    index: Vec<u64>,
    /// buffer.
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

    /// A mutable reference to the underlying value.
    ///
    /// # Safety
    ///
    /// This is unsafe, the caller needs to make sure that the collection is not modified, and that multiple mutable
    /// references are not active.
    #[inline]
    pub fn get_mut<'a>(&'a mut self, index: usize) -> Option<ValueMut<'a>> {
        let index = match self.index.get_mut(index) {
            Some(index) => index,
            None => return None,
        };

        let idx = *index;
        let ty = Type::from_byte((idx >> 56) as u8);
        let i = (idx & MASK) as usize;
        let ptr = self.buffer[i..].as_ptr() as *mut _;

        Some(ValueMut {
            index: index as *mut _,
            ty,
            ptr,
            marker: marker::PhantomData,
        })
    }

    /// Get the index.
    #[inline]
    pub fn get(&self, index: usize) -> Option<Value> {
        macro_rules! define {
            (
                $type:expr, $buf:expr,
                $(($ty:ty, $member:ident, $read:ident),)*
            ) => {
                match $type {
                Type::None => Some(Value::None),
                Type::U8 => Some(Value::U8($buf[0])),
                Type::I8 => Some(Value::I8($buf[0] as i8)),
                $(Type::$member => {
                    Some(Value::$member(unsafe { *($buf.as_ptr() as *const $ty) }))
                },)*
                }
            };
        }

        let index = match self.index.get(index) {
            Some(index) => *index,
            None => return None,
        };

        let (ty, i) = decode_index(index);
        let buf = &self.buffer[i..];

        define!(
            ty,
            buf,
            (u16, U16, read_u16),
            (i16, I16, read_i16),
            (u32, U32, read_u32),
            (i32, I32, read_i32),
            (u64, U64, read_u64),
            (i64, I64, read_i64),
            (u128, U128, read_u128),
            (i128, I128, read_i128),
        )
    }

    /// Create an iterator over all values in collections.
    pub fn iter<'a>(&'a self) -> Iter<'a> {
        Iter {
            values: self,
            pos: 0,
        }
    }

    /// Create a mutable iterator over all values.
    pub fn iter_mut<'a>(&'a mut self) -> IterMut<'a> {
        IterMut {
            values: unsafe { ptr::NonNull::new_unchecked(self) },
            pos: 0,
            marker: marker::PhantomData,
        }
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
                Value::U8(v) => { $buf.push(v); Type::U8 as u8 },
                Value::I8(v) => { $buf.push(v as u8); Type::I8 as u8 },
                $(
                Value::$member(v) => {
                    let o = self.scratch.as_mut_ptr() as *mut $ty;
                    unsafe { *o = v };
                    $buf.extend(&self.scratch[..mem::size_of::<$ty>()]);
                    Type::$member as u8
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

        let i = i as u64;

        if i > MASK {
            panic!("index out of range");
        }

        self.index.push(((t as u64) << 56) | i)
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueMut<'a> {
    ty: Type,
    index: *mut u64,
    ptr: *mut u8,
    marker: marker::PhantomData<&'a ()>,
}

unsafe impl<'a> Send for ValueMut<'a> {}

impl<'a> ValueMut<'a> {
    /// Convert into a value.
    pub fn to_value(&self) -> Value {
        macro_rules! define {
            ($value:expr, $(($member:ident, $ty:ty),)*) => {
                match $value {
                    Type::None => Value::None,
                    $(Type::$member => {
                        Value::$member(*(self.ptr as *mut $ty))
                    },)*
                }
            }
        }

        unsafe {
            define! {
                self.ty,
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
    }

    /// Set the given position with the given value.
    pub fn set(&mut self, value: Value) -> bool {
        macro_rules! define {
            ($type:expr, $ptr:expr, $value:expr, $(($member:ident, $ty:ty),)*) => {
                match ($type, $value) {
                    $((Type::$member, Value::$member(v)) => {
                        *($ptr as *mut $ty) = v;
                    },)*
                    _ => {
                        return false;
                    }
                }
            }
        }

        unsafe {
            define! {
                self.ty, self.ptr, value,
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

        true
    }

    /// Clear the value at the given location.
    pub fn clear(&mut self) {
        unsafe {
            *self.index = (Type::None as u64) << 56;
        }
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
    values: &'a Values,
    pos: usize,
}

impl<'a> Iterator for Iter<'a> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.values.get(self.pos)?;
        self.pos += 1;
        Some(value)
    }
}

pub struct IterMut<'a> {
    values: ptr::NonNull<Values>,
    pos: usize,
    marker: marker::PhantomData<&'a mut ()>,
}

impl<'a> Iterator for IterMut<'a> {
    type Item = ValueMut<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match unsafe { &mut *self.values.as_ptr() }.get_mut(self.pos) {
            Some(value) => {
                self.pos += 1;
                Some(value)
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Values;
    use crate::value::Value;

    #[test]
    pub fn test_values() {
        let mut values = Values::new();

        let expected = vec![
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
        ];

        values.extend(expected.iter().cloned());

        for (i, v) in expected.iter().cloned().enumerate() {
            assert_eq!(Some(v), values.get(i));
        }

        assert_eq!(None, values.get(expected.len() + 1));
        assert_eq!(expected, values.iter().collect::<Vec<_>>());
        assert_eq!(values.len(), expected.len());

        let len = expected.len();

        assert_eq!(
            expected[3..8].to_vec(),
            values.clone_slice(3, 8).iter().collect::<Vec<_>>()
        );

        assert_eq!(
            expected[3..3].to_vec(),
            values.clone_slice(3, 3).iter().collect::<Vec<_>>()
        );

        assert_eq!(
            expected[0..0].to_vec(),
            values.clone_slice(0, 0).iter().collect::<Vec<_>>()
        );

        assert_eq!(
            expected,
            values.clone_slice(0, len).iter().collect::<Vec<_>>()
        );
    }
}
