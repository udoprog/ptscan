//! Optimized storage for in-memory values.

use crate::value::{Type, Value};

#[derive(Default)]
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

    /// Get the index.
    #[inline]
    pub fn get(&mut self, index: usize) -> Option<Value> {
        use byteorder::{ByteOrder, NativeEndian};
        use std::mem;

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
                    Some(Value::$member(NativeEndian::$read(&$buf[..mem::size_of::<$ty>()])))
                },)*
                }
            };
        }

        let index = match self.index.get(index) {
            Some(index) => *index,
            None => return None,
        };

        let i = (index & 0x00_ff_ff_ff_ff_ff_ff_ffu64) as usize;

        let buf = &self.buffer[i..];

        define!(
            Type::from_byte((index >> 56) as u8),
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

    /// Push a single value.
    pub fn push(&mut self, value: Value) {
        use byteorder::{ByteOrder, NativeEndian};
        use std::mem;

        macro_rules! define {
            (
                $value:expr, $buf:expr,
                $(($ty:ty, $member:ident, $write:ident),)*
            ) => {
                match $value {
                Value::None => {
                    self.index.push(0);
                    return;
                },
                Value::U8(v) => {
                    $buf.push(v);
                    Type::U8 as u8
                },
                Value::I8(v) => {
                    $buf.push(v as u8);
                    Type::I8 as u8
                },
                $(
                Value::$member(v) => {
                    let mut buf = &mut self.scratch[..mem::size_of::<$ty>()];
                    NativeEndian::$write(&mut buf, v);
                    $buf.extend(buf.iter().cloned());
                    Type::$member as u8
                },
                )*
                }
            };
        }

        let i = self.buffer.len();

        let t = define!(
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
        );

        let i = i as u64;

        if i > 0x00_ff_ff_ff_ff_ff_ff_ffu64 {
            panic!("index out of range");
        }

        self.index.push(((t as u64) << 56) | i)
    }
}

#[cfg(test)]
mod tests {
    use super::Values;
    use crate::value::Value;

    #[test]
    pub fn test_values() {
        let mut values = Values::new();

        values.push(Value::U8(1));
        values.push(Value::I8(2));
        values.push(Value::U16(3));
        values.push(Value::I16(4));
        values.push(Value::U32(5));
        values.push(Value::I32(6));
        values.push(Value::U64(7));
        values.push(Value::I64(8));
        values.push(Value::U128(9));
        values.push(Value::I128(10));

        assert_eq!(Some(Value::U8(1)), values.get(0));
        assert_eq!(Some(Value::I8(2)), values.get(1));
        assert_eq!(Some(Value::U16(3)), values.get(2));
        assert_eq!(Some(Value::I16(4)), values.get(3));
        assert_eq!(Some(Value::U32(5)), values.get(4));
        assert_eq!(Some(Value::I32(6)), values.get(5));
        assert_eq!(Some(Value::U64(7)), values.get(6));
        assert_eq!(Some(Value::I64(8)), values.get(7));
        assert_eq!(Some(Value::U128(9)), values.get(8));
        assert_eq!(Some(Value::I128(10)), values.get(9));

        assert_eq!(None, values.get(10));
    }
}
