use crate::{
    encode::Encode, error::Error, filter::ast::EscapeString, process::Process, Address, Type,
};
use anyhow::bail;
use serde::{Deserialize, Serialize};
use std::{fmt, mem, str};

/// A single dynamic literal value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum Value {
    #[serde(rename = "none")]
    None(Type),
    #[serde(rename = "pointer")]
    Pointer(Address),
    #[serde(rename = "u8")]
    U8(u8),
    #[serde(rename = "i8")]
    I8(i8),
    #[serde(rename = "u16")]
    U16(u16),
    #[serde(rename = "i16")]
    I16(i16),
    #[serde(rename = "u32")]
    U32(u32),
    #[serde(rename = "i32")]
    I32(i32),
    #[serde(rename = "u64")]
    U64(u64),
    #[serde(rename = "i64")]
    I64(i64),
    #[serde(rename = "u128")]
    U128(u128),
    #[serde(rename = "i128")]
    I128(i128),
    #[serde(rename = "string")]
    String(Vec<u8>),
}

impl Value {
    /// Test if this value is something.
    pub fn is_some(&self) -> bool {
        match *self {
            Value::None(..) => false,
            _ => true,
        }
    }

    /// Test if value is aligned by default or not.
    pub fn is_default_aligned(&self) -> bool {
        match self {
            Self::None(Type::String(..)) => false,
            Self::String(..) => false,
            _ => true,
        }
    }

    /// Try to treat the value as an address.
    ///
    /// Returns `None` if this is not possible (e.g. value out of range).
    pub fn as_address(&self) -> anyhow::Result<Address> {
        use std::convert::TryFrom;

        let out = match *self {
            Self::None(..) => bail!("nothing cannot be made into address"),
            Self::Pointer(address) => address,
            Self::String(..) => bail!("string cannot be made into address"),
            Self::U8(value) => Address::try_from(value)?,
            Self::I8(value) => Address::try_from(value)?,
            Self::U16(value) => Address::try_from(value)?,
            Self::I16(value) => Address::try_from(value)?,
            Self::U32(value) => Address::try_from(value)?,
            Self::I32(value) => Address::try_from(value)?,
            Self::U64(value) => Address::try_from(value)?,
            Self::I64(value) => Address::try_from(value)?,
            Self::U128(value) => Address::try_from(value)?,
            Self::I128(value) => Address::try_from(value)?,
        };

        Ok(out)
    }

    /// Encode the given buffer into a value.
    pub fn encode(&self, process: &Process, buf: &mut [u8]) -> Result<(), Error> {
        match self {
            Self::None(..) => (),
            Self::Pointer(address) => address.encode(process, buf)?,
            Self::U8(value) => value.encode(buf),
            Self::I8(value) => value.encode(buf),
            Self::U16(value) => value.encode(buf),
            Self::I16(value) => value.encode(buf),
            Self::U32(value) => value.encode(buf),
            Self::I32(value) => value.encode(buf),
            Self::U64(value) => value.encode(buf),
            Self::I64(value) => value.encode(buf),
            Self::U128(value) => value.encode(buf),
            Self::I128(value) => value.encode(buf),
            Self::String(string) => string.encode(buf),
        }

        Ok(())
    }

    /// Cast this value.
    pub fn cast<T>(&self) -> Option<T>
    where
        T: Default + num::FromPrimitive,
    {
        match self {
            Self::U8(value) => T::from_u8(*value),
            Self::I8(value) => T::from_i8(*value),
            Self::U16(value) => T::from_u16(*value),
            Self::I16(value) => T::from_i16(*value),
            Self::U32(value) => T::from_u32(*value),
            Self::I32(value) => T::from_i32(*value),
            Self::U64(value) => T::from_u64(*value),
            Self::I64(value) => T::from_i64(*value),
            Self::U128(value) => T::from_u128(*value),
            Self::I128(value) => T::from_i128(*value),
            _ => Some(T::default()),
        }
    }

    /// Get the type of the value.
    pub fn ty(&self) -> Type {
        match self {
            Self::None(ty) => *ty,
            Self::Pointer(..) => Type::Pointer,
            Self::U8(..) => Type::U8,
            Self::I8(..) => Type::I8,
            Self::U16(..) => Type::U16,
            Self::I16(..) => Type::I16,
            Self::U32(..) => Type::U32,
            Self::I32(..) => Type::I32,
            Self::U64(..) => Type::U64,
            Self::I64(..) => Type::I64,
            Self::U128(..) => Type::U128,
            Self::I128(..) => Type::I128,
            Self::String(string) => Type::String(string.len()),
        }
    }

    /// Get the size in bytes of this value.
    pub fn size(&self, process: &Process) -> usize {
        match self {
            Self::None(ty) => ty.size(process),
            Self::Pointer(..) => process.pointer_width,
            Self::U8(..) => mem::size_of::<u8>(),
            Self::I8(..) => mem::size_of::<i8>(),
            Self::U16(..) => mem::size_of::<u16>(),
            Self::I16(..) => mem::size_of::<i16>(),
            Self::U32(..) => mem::size_of::<u32>(),
            Self::I32(..) => mem::size_of::<i32>(),
            Self::U64(..) => mem::size_of::<u64>(),
            Self::I64(..) => mem::size_of::<i64>(),
            Self::U128(..) => mem::size_of::<u128>(),
            Self::I128(..) => mem::size_of::<i128>(),
            Self::String(string) => string.len(),
        }
    }

    /// Convert from a big integer.
    pub fn from_bigint(ty: Type, value: &num_bigint::BigInt) -> Result<Self, Error> {
        use num::ToPrimitive;

        let out = match ty {
            Type::U8 => value.to_u8().map(Value::U8),
            Type::I8 => value.to_i8().map(Value::I8),
            Type::U16 => value.to_u16().map(Value::U16),
            Type::I16 => value.to_i16().map(Value::I16),
            Type::U32 => value.to_u32().map(Value::U32),
            Type::I32 => value.to_i32().map(Value::I32),
            Type::U64 => value.to_u64().map(Value::U64),
            Type::I64 => value.to_i64().map(Value::I64),
            Type::U128 => value.to_u128().map(Value::U128),
            Type::I128 => value.to_i128().map(Value::I128),
            _ => return Ok(Value::None(ty)),
        };

        let out = out.ok_or_else(|| Error::ValueNumberConversion(value.clone(), ty))?;
        Ok(out)
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::None(Type::default())
    }
}

impl str::FromStr for Value {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (hex, s) = if s.starts_with("0x") {
            (true, &s[2..])
        } else {
            (false, s)
        };

        let mut it = s.split(|c| c == 'i' || c == 'u');

        let base = it.next().ok_or_else(|| Error::ValueMissingBase)?;

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
        match self {
            Value::None(ty) => write!(fmt, "none({})", ty),
            Value::Pointer(address) => write!(fmt, "{}", address),
            Value::U8(value) => write!(fmt, "{}", value),
            Value::I8(value) => write!(fmt, "{}", value),
            Value::U16(value) => write!(fmt, "{}", value),
            Value::I16(value) => write!(fmt, "{}", value),
            Value::U32(value) => write!(fmt, "{}", value),
            Value::I32(value) => write!(fmt, "{}", value),
            Value::U64(value) => write!(fmt, "{}", value),
            Value::I64(value) => write!(fmt, "{}", value),
            Value::U128(value) => write!(fmt, "{}", value),
            Value::I128(value) => write!(fmt, "{}", value),
            Value::String(string) => write!(fmt, "{}", EscapeString(&*string)),
        }
    }
}
