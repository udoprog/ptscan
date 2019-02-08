use crate::Value;
use std::{error, fmt, mem, str};

// Decode a buffer, and depending on its width decode it differently.
macro_rules! decode_buf {
    ($buf:expr, $ty:ty) => {
        match $buf.len() {
            1 => $buf[0] as $ty,
            2 => byteorder::LittleEndian::read_u16($buf) as $ty,
            4 => byteorder::LittleEndian::read_u32($buf) as $ty,
            8 => byteorder::LittleEndian::read_u64($buf) as $ty,
            16 => byteorder::LittleEndian::read_u128($buf) as $ty,
            _ => return Value::None,
        }
    };
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
    /// Convert from a string.
    pub fn from_string(input: &str) -> Type {
        match input {
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
            _ => Type::None,
        }
    }

    /// Initialize a default value of the given type.
    pub fn default(&self) -> Value {
        use self::Type::*;

        match *self {
            None => Value::None,
            U8 => Value::U8(Default::default()),
            I8 => Value::I8(Default::default()),
            U16 => Value::U16(Default::default()),
            I16 => Value::I16(Default::default()),
            U32 => Value::U32(Default::default()),
            I32 => Value::I32(Default::default()),
            U64 => Value::U64(Default::default()),
            I64 => Value::I64(Default::default()),
            U128 => Value::U128(Default::default()),
            I128 => Value::I128(Default::default()),
        }
    }

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
    #[inline]
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
        use byteorder::ByteOrder;

        match *self {
            None => Value::None,
            U8 => Value::U8(decode_buf!(buf, u8)),
            I8 => Value::I8(decode_buf!(buf, i8)),
            U16 => Value::U16(decode_buf!(buf, u16)),
            I16 => Value::I16(decode_buf!(buf, i16)),
            U32 => Value::U32(decode_buf!(buf, u32)),
            I32 => Value::I32(decode_buf!(buf, i32)),
            U64 => Value::U64(decode_buf!(buf, u64)),
            I64 => Value::I64(decode_buf!(buf, i64)),
            U128 => Value::U128(decode_buf!(buf, u128)),
            I128 => Value::I128(decode_buf!(buf, i128)),
        }
    }

    /// Convert into a type which implements `fmt::Display` for a human-readable string.
    pub fn human_display(&self) -> HumanDisplay {
        HumanDisplay { ty: *self }
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

/// A human display of a `Type`.
pub struct HumanDisplay {
    ty: Type,
}

impl fmt::Display for HumanDisplay {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            Type::None => write!(fmt, "none"),
            Type::U8 => write!(fmt, "unsigned 8-bit number"),
            Type::I8 => write!(fmt, "signed 8-bit number"),
            Type::U16 => write!(fmt, "unsigned 16-bit number"),
            Type::I16 => write!(fmt, "signed 16-bit number"),
            Type::U32 => write!(fmt, "unsigned 32-bit number"),
            Type::I32 => write!(fmt, "signed 32-bit number"),
            Type::U64 => write!(fmt, "unsigned 64-bit number"),
            Type::I64 => write!(fmt, "signed 64-bit number"),
            Type::U128 => write!(fmt, "unsigned 128-bit number"),
            Type::I128 => write!(fmt, "signed 128-bit number"),
        }
    }
}
