use crate::{encode::Encode, Address, Type};
use num_bigint::BigInt;
use std::{fmt, str};

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

    /// Convert from a big integer.
    pub fn from_big(value: BigInt, ty: Type) -> Result<Self, failure::Error> {
        use self::Type::*;
        use num::ToPrimitive;

        let out = match ty {
            U8 => value.to_u8().map(Value::U8),
            I8 => value.to_i8().map(Value::I8),
            U16 => value.to_u16().map(Value::U16),
            I16 => value.to_i16().map(Value::I16),
            U32 => value.to_u32().map(Value::U32),
            I32 => value.to_i32().map(Value::I32),
            U64 => value.to_u64().map(Value::U64),
            I64 => value.to_i64().map(Value::I64),
            U128 => value.to_u128().map(Value::U128),
            I128 => value.to_i128().map(Value::I128),
            _ => return Ok(Value::None),
        };

        let out = out.ok_or_else(|| {
            failure::format_err!(
                "failed to convert number `{}`: out-of-range for type {}",
                value,
                ty,
            )
        })?;
        Ok(out)
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
        }
    }
}
