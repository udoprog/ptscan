use crate::{encode::Encode, error::Error, filter::ast, Address, Type};
use anyhow::bail;
use std::{fmt, mem, str};

/// A single dynamic literal value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    String(String),
}

impl Value {
    /// Test if this value is something.
    pub fn is_some(&self) -> bool {
        match *self {
            Value::None => false,
            _ => true,
        }
    }

    /// Test if value is aligned by default or not.
    pub fn is_default_aligned(&self) -> bool {
        match self {
            Self::String(..) => false,
            _ => true,
        }
    }

    /// Convert from a big integer.
    pub fn from_ast(value: ast::Value, ty: Type) -> Result<Self, Error> {
        use num::ToPrimitive;

        let out = match (ty, &value) {
            (Type::U8, ast::Value::Number(value)) => value.to_u8().map(Value::U8),
            (Type::I8, ast::Value::Number(value)) => value.to_i8().map(Value::I8),
            (Type::U16, ast::Value::Number(value)) => value.to_u16().map(Value::U16),
            (Type::I16, ast::Value::Number(value)) => value.to_i16().map(Value::I16),
            (Type::U32, ast::Value::Number(value)) => value.to_u32().map(Value::U32),
            (Type::I32, ast::Value::Number(value)) => value.to_i32().map(Value::I32),
            (Type::U64, ast::Value::Number(value)) => value.to_u64().map(Value::U64),
            (Type::I64, ast::Value::Number(value)) => value.to_i64().map(Value::I64),
            (Type::U128, ast::Value::Number(value)) => value.to_u128().map(Value::U128),
            (Type::I128, ast::Value::Number(value)) => value.to_i128().map(Value::I128),
            (Type::String, ast::Value::String(string)) => Some(Value::String(string.clone())),
            _ => return Ok(Value::None),
        };

        let out = out.ok_or_else(|| Error::AstConversionError(value, ty))?;
        Ok(out)
    }

    /// Try to treat the value as an address.
    ///
    /// Returns `None` if this is not possible (e.g. value out of range).
    pub fn as_address(&self) -> anyhow::Result<Address> {
        use self::Value::*;
        use std::convert::TryFrom;

        let out = match *self {
            None => bail!("nothing cannot be made into address"),
            String(..) => bail!("string cannot be made into address"),
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
        match self {
            Self::None => {}
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
    }

    /// Cast this value.
    pub fn cast<T>(&self) -> Option<T>
    where
        T: Default + num::FromPrimitive,
    {
        use self::Value::*;

        match self {
            U8(value) => T::from_u8(*value),
            I8(value) => T::from_i8(*value),
            U16(value) => T::from_u16(*value),
            I16(value) => T::from_i16(*value),
            U32(value) => T::from_u32(*value),
            I32(value) => T::from_i32(*value),
            U64(value) => T::from_u64(*value),
            I64(value) => T::from_i64(*value),
            U128(value) => T::from_u128(*value),
            I128(value) => T::from_i128(*value),
            _ => Some(T::default()),
        }
    }

    /// Get the type of the value.
    pub fn ty(&self) -> Type {
        use self::Value::*;

        match self {
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
            String(..) => Type::String,
        }
    }

    /// Get the size in bytes of this value.
    pub fn size(&self) -> usize {
        use self::Value::*;

        match self {
            None => 0,
            U8(..) => mem::size_of::<u8>(),
            I8(..) => mem::size_of::<i8>(),
            U16(..) => mem::size_of::<u16>(),
            I16(..) => mem::size_of::<i16>(),
            U32(..) => mem::size_of::<u32>(),
            I32(..) => mem::size_of::<i32>(),
            U64(..) => mem::size_of::<u64>(),
            I64(..) => mem::size_of::<i64>(),
            U128(..) => mem::size_of::<u128>(),
            I128(..) => mem::size_of::<i128>(),
            String(string) => string.len(),
        }
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
            Value::String(string) => write!(fmt, "{}", EscapeString(string.as_str())),
        }
    }
}

struct EscapeString<'a>(&'a str);

impl fmt::Display for EscapeString<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "\"")?;

        for c in self.0.chars() {
            match c {
                '\\' => write!(fmt, "\\\\")?,
                '"' => write!(fmt, "\\\"")?,
                ' ' => write!(fmt, " ")?,
                '\t' => write!(fmt, "\\t")?,
                '\n' => write!(fmt, "\\n")?,
                '\r' => write!(fmt, "\\r")?,
                '\0' => write!(fmt, "\\0")?,
                c => write!(fmt, "{}", c)?,
            }
        }

        write!(fmt, "\"")?;
        Ok(())
    }
}
