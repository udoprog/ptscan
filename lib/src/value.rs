use crate::{address::Address, encode::Encode};
use std::{error, fmt, mem, str};

/// A single dynamic literal value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    None,
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    U64(u64),
    I64(i64),
    U128(u128),
    I128(i128),
}

impl Value {
    /// Test if this value is something.
    pub fn is_some(&self) -> bool {
        match *self {
            Value::None => false,
            _ => true,
        }
    }

    /// Try to treat the value as an address.
    ///
    /// Returns `None` if this is not possible (e.g. value out of range).
    pub fn as_address(&self) -> Result<Address, failure::Error> {
        use self::Value::*;
        use std::convert::TryFrom;

        let out = match *self {
            None => failure::bail!("nothing cannot be made into address"),
            U8(value) => Address::try_from(value)?,
            I8(value) => Address::try_from(value)?,
            U16(value) => Address::try_from(value)?,
            I16(value) => Address::try_from(value)?,
            U32(value) => Address::try_from(value)?,
            I32(value) => Address::try_from(value)?,
            U64(value) => Address::try_from(value)?,
            I64(value) => Address::try_from(value)?,
            U128(value) => Address::try_from(value)?,
            I128(value) => Address::try_from(value)?,
        };

        Ok(out)
    }

    /// Decode the given buffer into a value.
    pub fn encode(&self, buf: &mut [u8]) {
        use self::Value::*;

        match *self {
            None => {}
            U8(value) => u8::encode(buf, value),
            I8(value) => i8::encode(buf, value),
            U16(value) => u16::encode(buf, value),
            I16(value) => i16::encode(buf, value),
            U32(value) => u32::encode(buf, value),
            I32(value) => i32::encode(buf, value),
            U64(value) => u64::encode(buf, value),
            I64(value) => i64::encode(buf, value),
            U128(value) => u128::encode(buf, value),
            I128(value) => i128::encode(buf, value),
        }
    }

    /// Get the type of the value.
    pub fn ty(&self) -> Type {
        use self::Value::*;

        match *self {
            None => Type::None,
            U8(..) => Type::U8,
            I8(..) => Type::I8,
            U16(..) => Type::U16,
            I16(..) => Type::I16,
            U32(..) => Type::U32,
            I32(..) => Type::I32,
            U64(..) => Type::U64,
            I64(..) => Type::I64,
            U128(..) => Type::U128,
            I128(..) => Type::I128,
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
            Value::None => write!(fmt, "none"),
            Value::U8(value) => write!(fmt, "{}u8", value),
            Value::I8(value) => write!(fmt, "{}i8", value),
            Value::U16(value) => write!(fmt, "{}u16", value),
            Value::I16(value) => write!(fmt, "{}i16", value),
            Value::U32(value) => write!(fmt, "{}u32", value),
            Value::I32(value) => write!(fmt, "{}i32", value),
            Value::U64(value) => write!(fmt, "{}u64", value),
            Value::I64(value) => write!(fmt, "{}i64", value),
            Value::U128(value) => write!(fmt, "{}u128", value),
            Value::I128(value) => write!(fmt, "{}i128", value),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    None = 0u8,
    U8 = 1u8,
    I8 = 2u8,
    U16 = 3u8,
    I16 = 4u8,
    U32 = 5u8,
    I32 = 6u8,
    U64 = 7u8,
    I64 = 8u8,
    U128 = 9u8,
    I128 = 10u8,
}

impl Type {
    /// Convert from byte.
    pub fn from_byte(ty: u8) -> Type {
        const U8: u8 = Type::U8 as u8;
        const I8: u8 = Type::I8 as u8;
        const U16: u8 = Type::U16 as u8;
        const I16: u8 = Type::I16 as u8;
        const U32: u8 = Type::U32 as u8;
        const I32: u8 = Type::I32 as u8;
        const U64: u8 = Type::U64 as u8;
        const I64: u8 = Type::I64 as u8;
        const U128: u8 = Type::U128 as u8;
        const I128: u8 = Type::I128 as u8;

        match ty {
            U8 => Type::U8,
            I8 => Type::I8,
            U16 => Type::U16,
            I16 => Type::I16,
            U32 => Type::U32,
            I32 => Type::I32,
            U64 => Type::U64,
            I64 => Type::I64,
            U128 => Type::U128,
            I128 => Type::I128,
            _ => Type::None,
        }
    }

    /// Parse a string value of the type.
    pub fn parse(&self, input: &str) -> Result<Value, failure::Error> {
        use self::Type::*;

        let value = match *self {
            None => failure::bail!("cannot parse none type"),
            U8 => Value::U8(str::parse::<u8>(input)?),
            I8 => Value::I8(str::parse::<i8>(input)?),
            U16 => Value::U16(str::parse::<u16>(input)?),
            I16 => Value::I16(str::parse::<i16>(input)?),
            U32 => Value::U32(str::parse::<u32>(input)?),
            I32 => Value::I32(str::parse::<i32>(input)?),
            U64 => Value::U64(str::parse::<u64>(input)?),
            I64 => Value::I64(str::parse::<i64>(input)?),
            U128 => Value::U128(str::parse::<u128>(input)?),
            I128 => Value::I128(str::parse::<i128>(input)?),
        };

        Ok(value)
    }

    /// Parse a string as hex.
    pub fn parse_hex(&self, input: &str) -> Result<Value, failure::Error> {
        use self::Type::*;

        let value = match *self {
            None => failure::bail!("cannot parse none type"),
            U8 => Value::U8(u8::from_str_radix(input, 16)?),
            I8 => Value::I8(i8::from_str_radix(input, 16)?),
            U16 => Value::U16(u16::from_str_radix(input, 16)?),
            I16 => Value::I16(i16::from_str_radix(input, 16)?),
            U32 => Value::U32(u32::from_str_radix(input, 16)?),
            I32 => Value::I32(i32::from_str_radix(input, 16)?),
            U64 => Value::U64(u64::from_str_radix(input, 16)?),
            I64 => Value::I64(i64::from_str_radix(input, 16)?),
            U128 => Value::U128(u128::from_str_radix(input, 16)?),
            I128 => Value::I128(i128::from_str_radix(input, 16)?),
        };

        Ok(value)
    }

    /// The size in-memory that a value has.
    pub fn size(&self) -> usize {
        use self::Type::*;

        match *self {
            None => 0,
            U8 => mem::size_of::<u8>(),
            I8 => mem::size_of::<i8>(),
            U16 => mem::size_of::<u16>(),
            I16 => mem::size_of::<i16>(),
            U32 => mem::size_of::<u32>(),
            I32 => mem::size_of::<i32>(),
            U64 => mem::size_of::<u64>(),
            I64 => mem::size_of::<i64>(),
            U128 => mem::size_of::<u128>(),
            I128 => mem::size_of::<i128>(),
        }
    }

    /// Decode the given buffer into a value.
    pub fn decode(&self, buf: &[u8]) -> Value {
        use self::Type::*;
        use byteorder::{ByteOrder, LittleEndian};

        match *self {
            None => Value::None,
            U8 => Value::U8(buf[0]),
            I8 => Value::I8(buf[0] as i8),
            U16 => Value::U16(LittleEndian::read_u16(buf)),
            I16 => Value::I16(LittleEndian::read_i16(buf)),
            U32 => Value::U32(LittleEndian::read_u32(buf)),
            I32 => Value::I32(LittleEndian::read_i32(buf)),
            U64 => Value::U64(LittleEndian::read_u64(buf)),
            I64 => Value::I64(LittleEndian::read_i64(buf)),
            U128 => Value::U128(LittleEndian::read_u128(buf)),
            I128 => Value::I128(LittleEndian::read_i128(buf)),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Type::*;

        let o = match *self {
            None => "none",
            U8 => "u8",
            I8 => "i8",
            U16 => "u16",
            I16 => "i16",
            U32 => "u32",
            I32 => "i32",
            U64 => "u64",
            I64 => "i64",
            U128 => "u128",
            I128 => "i128",
        };

        o.fmt(fmt)
    }
}

impl str::FromStr for Type {
    type Err = ParseTypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ty = match s {
            "u8" => Type::U8,
            "i8" => Type::I8,
            "u16" => Type::U16,
            "i16" => Type::I16,
            "u32" => Type::U32,
            "i32" => Type::I32,
            "u64" => Type::U64,
            "i64" => Type::I64,
            "u128" => Type::U128,
            "i128" => Type::I128,
            other => return Err(ParseTypeError(other.to_string())),
        };

        Ok(ty)
    }
}

#[derive(Debug)]
pub struct ParseTypeError(String);

impl error::Error for ParseTypeError {}

impl fmt::Display for ParseTypeError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "`{}` is not a valid", self.0)
    }
}