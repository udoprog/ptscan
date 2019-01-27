//! Predicates used for matching against memory.

use crate::address::Address;
use byteorder::{ByteOrder, LittleEndian};
use std::{fmt, mem, str};

/// A single dynamic literal value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    U128(u128),
    I128(i128),
    U64(u64),
    I64(i64),
    U32(u32),
    I32(i32),
    U16(u16),
    I16(i16),
    U8(u8),
    I8(i8),
}

impl Value {
    /// Try to treat the value as an address.
    ///
    /// Returns `None` if this is not possible (e.g. value out of range).
    pub fn as_address(&self) -> Result<Address, failure::Error> {
        use self::Value::*;
        use std::convert::TryFrom;

        let out = match *self {
            U128(value) => Address::try_from(value)?,
            I128(value) => Address::try_from(value)?,
            U64(value) => Address::try_from(value)?,
            I64(value) => Address::try_from(value)?,
            U32(value) => Address::try_from(value)?,
            I32(value) => Address::try_from(value)?,
            U16(value) => Address::try_from(value)?,
            I16(value) => Address::try_from(value)?,
            U8(value) => Address::try_from(value)?,
            I8(value) => Address::try_from(value)?,
        };

        Ok(out)
    }

    /// Decode the given buffer into a value.
    pub fn encode(&self, buf: &mut [u8]) {
        use self::Value::*;

        match *self {
            U128(value) => u128::encode(buf, value),
            I128(value) => i128::encode(buf, value),
            U64(value) => u64::encode(buf, value),
            I64(value) => i64::encode(buf, value),
            U32(value) => u32::encode(buf, value),
            I32(value) => i32::encode(buf, value),
            U16(value) => u16::encode(buf, value),
            I16(value) => i16::encode(buf, value),
            U8(value) => u8::encode(buf, value),
            I8(value) => i8::encode(buf, value),
        }
    }

    /// Get the type of the value.
    pub fn ty(&self) -> Type {
        use self::Value::*;

        match *self {
            U128(..) => Type::U128,
            I128(..) => Type::I128,
            U64(..) => Type::U64,
            I64(..) => Type::I64,
            U32(..) => Type::U32,
            I32(..) => Type::I32,
            U16(..) => Type::U16,
            I16(..) => Type::I16,
            U8(..) => Type::U8,
            I8(..) => Type::I8,
        }
    }
}

impl str::FromStr for Value {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (hex, s) = if s.starts_with("0x") {
            (true, &s[2..])
        } else {
            (false, s)
        };

        let mut it = s.split(|c| c == 'i' || c == 'u');

        let base = it
            .next()
            .ok_or_else(|| failure::format_err!("missing numeric base"))?;

        let (s, ty) = match it.next() {
            Some(suffix) => {
                // NB: need to extract full suffix from original input.
                let e = s.len() - suffix.len() - 1;
                (base, str::parse::<Type>(&s[e..])?)
            }
            None => (s, Type::I32),
        };

        if hex {
            ty.parse_hex(s)
        } else {
            ty.parse(s)
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Value::U128(value) => write!(fmt, "{}u128", value),
            Value::I128(value) => write!(fmt, "{}i128", value),
            Value::U64(value) => write!(fmt, "{}u64", value),
            Value::I64(value) => write!(fmt, "{}i64", value),
            Value::U32(value) => write!(fmt, "{}u32", value),
            Value::I32(value) => write!(fmt, "{}i32", value),
            Value::U16(value) => write!(fmt, "{}u16", value),
            Value::I16(value) => write!(fmt, "{}i16", value),
            Value::U8(value) => write!(fmt, "{}u8", value),
            Value::I8(value) => write!(fmt, "{}i8", value),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    U128,
    I128,
    U64,
    I64,
    U32,
    I32,
    U16,
    I16,
    U8,
    I8,
}

impl Type {
    /// Parse a string value of the type.
    pub fn parse(&self, input: &str) -> Result<Value, failure::Error> {
        use self::Type::*;

        let value = match *self {
            U128 => Value::U128(str::parse::<u128>(input)?),
            I128 => Value::I128(str::parse::<i128>(input)?),
            U64 => Value::U64(str::parse::<u64>(input)?),
            I64 => Value::I64(str::parse::<i64>(input)?),
            U32 => Value::U32(str::parse::<u32>(input)?),
            I32 => Value::I32(str::parse::<i32>(input)?),
            U16 => Value::U16(str::parse::<u16>(input)?),
            I16 => Value::I16(str::parse::<i16>(input)?),
            U8 => Value::U8(str::parse::<u8>(input)?),
            I8 => Value::I8(str::parse::<i8>(input)?),
        };

        Ok(value)
    }

    /// Parse a string as hex.
    pub fn parse_hex(&self, input: &str) -> Result<Value, failure::Error> {
        use self::Type::*;

        let value = match *self {
            U128 => Value::U128(u128::from_str_radix(input, 16)?),
            I128 => Value::I128(i128::from_str_radix(input, 16)?),
            U64 => Value::U64(u64::from_str_radix(input, 16)?),
            I64 => Value::I64(i64::from_str_radix(input, 16)?),
            U32 => Value::U32(u32::from_str_radix(input, 16)?),
            I32 => Value::I32(i32::from_str_radix(input, 16)?),
            U16 => Value::U16(u16::from_str_radix(input, 16)?),
            I16 => Value::I16(i16::from_str_radix(input, 16)?),
            U8 => Value::U8(u8::from_str_radix(input, 16)?),
            I8 => Value::I8(i8::from_str_radix(input, 16)?),
        };

        Ok(value)
    }

    /// The size in-memory that a value has.
    pub fn size(&self) -> usize {
        use self::Type::*;

        match *self {
            U128 => mem::size_of::<u128>(),
            I128 => mem::size_of::<i128>(),
            U64 => mem::size_of::<u64>(),
            I64 => mem::size_of::<i64>(),
            U32 => mem::size_of::<u32>(),
            I32 => mem::size_of::<i32>(),
            U16 => mem::size_of::<u16>(),
            I16 => mem::size_of::<i16>(),
            U8 => mem::size_of::<u8>(),
            I8 => mem::size_of::<i8>(),
        }
    }

    /// Decode the given buffer into a value.
    pub fn decode(&self, buf: &[u8]) -> Value {
        use self::Type::*;
        use byteorder::{ByteOrder, LittleEndian};

        match *self {
            U128 => Value::U128(LittleEndian::read_u128(buf)),
            I128 => Value::I128(LittleEndian::read_i128(buf)),
            U64 => Value::U64(LittleEndian::read_u64(buf)),
            I64 => Value::I64(LittleEndian::read_i64(buf)),
            U32 => Value::U32(LittleEndian::read_u32(buf)),
            I32 => Value::I32(LittleEndian::read_i32(buf)),
            U16 => Value::U16(LittleEndian::read_u16(buf)),
            I16 => Value::I16(LittleEndian::read_i16(buf)),
            U8 => Value::U8(buf[0]),
            I8 => Value::I8(buf[0] as i8),
        }
    }
}

impl str::FromStr for Type {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ty = match s {
            "u128" => Type::U128,
            "i128" => Type::I128,
            "u64" => Type::U64,
            "i64" => Type::I64,
            "u32" => Type::U32,
            "i32" => Type::I32,
            "u16" => Type::U16,
            "i16" => Type::I16,
            "u8" => Type::U8,
            "i8" => Type::I8,
            other => failure::bail!("bad type: {}", other),
        };

        Ok(ty)
    }
}

/// A bunch of read-only zeros used for comparison.
static ZEROS: &'static [u8] = &[
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
];

/// Special matching mode to speed up scanning.
///
/// Special matchers are matchers which can look at an entire slice of memory and immediate (without decoding) decide
/// if it is a match or not.
///
/// This has the benefit of speeding up decoding of scanned memory significantly.
#[derive(Debug, Clone)]
pub enum Special {
    /// Must match the given buffer exactly.
    Exact { buffer: Vec<u8> },
    /// Must not match the given buffer exactly.
    NotExact { buffer: Vec<u8> },
    /// All bytes in the given range are expected to be zero.
    Zero,
    /// All bytes in the given range are expected to be non-zero.
    NotZero,
}

impl Special {
    /// Invert what the special matcher does.
    ///
    /// Non-zero matchers become zero matchers,
    /// Exact matchers become not-exact matchers.
    pub fn invert(self) -> Special {
        match self {
            Special::Zero => Special::NotZero,
            Special::NotZero => Special::Zero,
            Special::Exact { buffer } => Special::NotExact { buffer },
            Special::NotExact { buffer } => Special::Exact { buffer },
        }
    }

    /// Set up an exact match for the given value.
    pub fn exact<T>(value: T) -> Special
    where
        T: Encode,
    {
        let mut buffer = vec![0u8; mem::size_of::<T>()];
        T::encode(&mut buffer, value);

        Special::Exact { buffer }
    }

    /// Set up a not-exact match for the given value.
    pub fn not_exact<T>(value: T) -> Special
    where
        T: Encode,
    {
        let mut buffer = vec![0u8; mem::size_of::<T>()];
        T::encode(&mut buffer, value);
        Special::NotExact { buffer }
    }

    /// Test if this special scanner matches the given slice of memory.
    pub fn test(&self, buf: &[u8]) -> Option<bool> {
        match self {
            Special::Zero => Some(buf == &ZEROS[..buf.len()]),
            Special::NotZero => {
                if buf == &ZEROS[..buf.len()] {
                    return Some(false);
                }

                None
            }
            Special::Exact { ref buffer } => Some(buf == buffer.as_slice()),
            Special::NotExact { ref buffer } => Some(buf != buffer.as_slice()),
        }
    }
}

pub trait Encode: Sized {
    /// Encode the value into the buffer.
    fn encode(buffer: &mut [u8], value: Self);
}

impl Encode for u128 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u128(buffer, value);
    }
}

impl Encode for i128 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i128(buffer, value);
    }
}

impl Encode for u64 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u64(buffer, value);
    }
}

impl Encode for i64 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i64(buffer, value);
    }
}

impl Encode for u32 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u32(buffer, value);
    }
}

impl Encode for i32 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i32(buffer, value);
    }
}

impl Encode for u16 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u16(buffer, value);
    }
}

impl Encode for i16 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i16(buffer, value);
    }
}

impl Encode for u8 {
    fn encode(buffer: &mut [u8], value: Self) {
        buffer[0] = value;
    }
}

impl Encode for i8 {
    fn encode(buffer: &mut [u8], value: Self) {
        buffer[0] = value as u8;
    }
}
