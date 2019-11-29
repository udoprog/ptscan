use crate::{
    error::Error,
    utils::{EscapeString, Hex},
    Address, Encoding, PointerInfo, Type,
};
use anyhow::bail;
use num_bigint::Sign;
use serde::{Deserialize, Serialize};
use std::{cmp, fmt, iter, mem, str};

macro_rules! numeric_op {
    ($a:ident $op:tt $b:ident, $checked_op:ident) => {
        numeric_op!(@a $a $op $b, $checked_op, {
            [Pointer, Address],
            [U8, u8],
            [I8, i8],
            [U16, u16],
            [I16, i16],
            [U32, u32],
            [I32, i32],
            [U64, u64],
            [I64, i64],
            [U128, u128],
            [I128, i128]
        })
    };

    (@a $a:ident $op:tt $b:ident, $checked_op:ident, {$([$variant:ident, $ty:ty]),*}) => {
        match ($a, $b) {
            $((Self::$variant(lhs), Self::$variant(rhs)) => lhs.$checked_op(rhs).map(Self::$variant),)*
            (Self::None, _) | (_, Self::None) => None,
            (lhs, rhs) => bail!("bad operation: {} {} {}", lhs, stringify!($op), rhs),
        }
    };
}

#[derive(Debug, thiserror::Error)]
#[error("failed to convert bigint `{0}` into type {1}")]
pub struct FromBigIntError(num_bigint::BigInt, Type);

#[derive(Debug, thiserror::Error)]
#[error("failed to convert bigdecimal `{0}` into type {1}")]
pub struct FromBigDecimalError(bigdecimal::BigDecimal, Type);

/// A single dynamic literal value.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "type", content = "value")]
pub enum Value {
    None,
    Pointer(Address),
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
    F32(f32),
    F64(f64),
    String(String),
    Bytes(Vec<u8>),
}

impl Value {
    /// Test if this value is something.
    pub fn is_some(&self) -> bool {
        match *self {
            Value::None => false,
            _ => true,
        }
    }

    pub fn is_none(&self) -> bool {
        match *self {
            Value::None => true,
            _ => false,
        }
    }

    /// Test if value is of the given type.
    pub fn is(&self, ty: Type) -> bool {
        match (self, ty) {
            (Self::None, Type::None) => true,
            (Self::Pointer(..), Type::Pointer) => true,
            (Self::U8(..), Type::U8) => true,
            (Self::I8(..), Type::I8) => true,
            (Self::U16(..), Type::U16) => true,
            (Self::I16(..), Type::I16) => true,
            (Self::U32(..), Type::U32) => true,
            (Self::I32(..), Type::I32) => true,
            (Self::U64(..), Type::U64) => true,
            (Self::I64(..), Type::I64) => true,
            (Self::U128(..), Type::U128) => true,
            (Self::I128(..), Type::I128) => true,
            (Self::F32(..), Type::F32) => true,
            (Self::F64(..), Type::F64) => true,
            (Self::String(..), Type::String(..)) => true,
            (Self::Bytes(..), Type::Bytes(..)) => true,
            _ => false,
        }
    }

    /// Unwrap the current value, or return an alternative if this is None.
    pub fn unwrap_or(self, other: Value) -> Self {
        match self {
            Value::None => other,
            value => value,
        }
    }

    /// The currently assumed value is all zero bits in memory.
    ///
    /// This is used for optimizations.
    pub fn is_zero_bits(&self) -> Option<bool> {
        Some(match *self {
            Self::None => return None,
            Self::Pointer(address) => address.is_null(),
            Self::U8(value) => value == 0,
            Self::I8(value) => value == 0,
            Self::U16(value) => value == 0,
            Self::I16(value) => value == 0,
            Self::U32(value) => value == 0,
            Self::I32(value) => value == 0,
            Self::U64(value) => value == 0,
            Self::I64(value) => value == 0,
            Self::U128(value) => value == 0,
            Self::I128(value) => value == 0,
            Self::F32(value) => value == 0f32,
            Self::F64(value) => value == 0f64,
            Self::String(ref s) => s.as_bytes().iter().all(|c| *c == 0),
            Self::Bytes(ref bytes) => bytes.iter().all(|c| *c == 0),
        })
    }

    pub fn sign(&self) -> Sign {
        macro_rules! signed {
            ($expr:expr) => {
                match $expr.signum() {
                    -1 => Sign::Minus,
                    1 => Sign::Plus,
                    _ => Sign::NoSign,
                }
            };
        }

        macro_rules! unsigned {
            ($expr:expr) => {
                if $expr > 0 {
                    Sign::Plus
                } else {
                    Sign::NoSign
                }
            };
        }

        match *self {
            Self::Pointer(address) => {
                if address.is_null() {
                    Sign::NoSign
                } else {
                    Sign::Plus
                }
            }
            Self::U8(value) => unsigned!(value),
            Self::I8(value) => signed!(value),
            Self::U16(value) => unsigned!(value),
            Self::I16(value) => signed!(value),
            Self::U32(value) => unsigned!(value),
            Self::I32(value) => signed!(value),
            Self::U64(value) => unsigned!(value),
            Self::I64(value) => signed!(value),
            Self::U128(value) => unsigned!(value),
            Self::I128(value) => signed!(value),
            _ => Sign::NoSign,
        }
    }

    /// Test if value is aligned by default or not.
    pub fn is_default_aligned(&self) -> bool {
        match self {
            Self::String(..) => false,
            Self::Bytes(..) => false,
            _ => true,
        }
    }

    /// Try to treat the value as an address.
    ///
    /// Returns `None` if this is not possible (e.g. value out of range).
    pub fn as_address(&self) -> Option<Address> {
        use std::convert::TryFrom;

        match *self {
            Self::Pointer(address) => Some(address),
            Self::U8(value) => Address::try_from(value).ok(),
            Self::I8(value) => Address::try_from(value).ok(),
            Self::U16(value) => Address::try_from(value).ok(),
            Self::I16(value) => Address::try_from(value).ok(),
            Self::U32(value) => Address::try_from(value).ok(),
            Self::I32(value) => Address::try_from(value).ok(),
            Self::U64(value) => Address::try_from(value).ok(),
            Self::I64(value) => Address::try_from(value).ok(),
            Self::U128(value) => Address::try_from(value).ok(),
            Self::I128(value) => Address::try_from(value).ok(),
            _ => None,
        }
    }

    /// Encode the given buffer into a value.
    pub fn encode<P>(&self, process: &P, buf: &mut Vec<u8>) -> bool
    where
        P: PointerInfo,
    {
        use byteorder::ByteOrder as _;

        macro_rules! reserve {
            ($len:expr) => {{
                buf.reserve($len);
                buf.extend(iter::repeat(0u8).take($len));
                &mut buf[..$len]
            }};
        }

        match *self {
            Self::None => (),
            Self::Pointer(Address(address)) => {
                let width = process.pointer_width().size();
                return process.encode_pointer(reserve!(width), address);
            }
            Self::U8(value) => {
                buf.reserve(1);
                buf.push(value);
            }
            Self::I8(value) => {
                buf.reserve(1);
                buf.push(value as u8);
            }
            Self::U16(value) => {
                P::ByteOrder::write_u16(reserve!(2), value);
            }
            Self::I16(value) => {
                P::ByteOrder::write_u16(reserve!(2), value as u16);
            }
            Self::U32(value) => {
                P::ByteOrder::write_u32(reserve!(4), value);
            }
            Self::I32(value) => {
                reserve!(4);
                P::ByteOrder::write_u32(reserve!(4), value as u32);
            }
            Self::U64(value) => {
                P::ByteOrder::write_u64(reserve!(8), value);
            }
            Self::I64(value) => {
                P::ByteOrder::write_u64(reserve!(8), value as u64);
            }
            Self::U128(value) => {
                P::ByteOrder::write_u128(reserve!(16), value);
            }
            Self::I128(value) => {
                P::ByteOrder::write_u128(reserve!(16), value as u128);
            }
            Self::F32(value) => P::ByteOrder::write_f32(reserve!(4), value),
            Self::F64(value) => P::ByteOrder::write_f64(reserve!(4), value),
            Self::String(ref s) => {
                // TODO: use the proper encoding.
                // Currently stops encoding when reaching an unmappable character.
                return Encoding::default().stream_encode(buf, s).is_ok();
            }
            Self::Bytes(ref bytes) => {
                let buf = reserve!(bytes.len());
                buf.clone_from_slice(&bytes[..]);
            }
        }

        true
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

    /// Get the size in bytes of this value.
    pub fn size(&self, process: &impl PointerInfo) -> Option<usize> {
        Some(match self {
            Self::None => return None,
            Self::Pointer(..) => process.pointer_width().size(),
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
            Self::F32(..) => mem::size_of::<f32>(),
            Self::F64(..) => mem::size_of::<f64>(),
            Self::String(..) => return None,
            Self::Bytes(bytes) => bytes.len(),
        })
    }

    /// Convert from a big integer.
    pub fn from_bigint(ty: Type, value: &num_bigint::BigInt) -> Result<Self, FromBigIntError> {
        use num::ToPrimitive;

        let out = match ty {
            Type::Pointer => value.to_u64().map(|v| Value::Pointer(Address(v))),
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
            Type::F32 => value.to_f32().map(Value::F32),
            Type::F64 => value.to_f64().map(Value::F64),
            _ => return Ok(Value::None),
        };

        let out = out.ok_or_else(|| FromBigIntError(value.clone(), ty))?;
        Ok(out)
    }

    /// Convert from a big decimal.
    pub fn from_bigdecimal(
        ty: Type,
        value: &bigdecimal::BigDecimal,
    ) -> Result<Self, FromBigDecimalError> {
        use num_traits::cast::ToPrimitive as _;

        let out = match ty {
            Type::F32 => value.to_f32().map(Value::F32),
            Type::F64 => value.to_f64().map(Value::F64),
            _ => return Ok(Value::None),
        };

        let out = out.ok_or_else(|| FromBigDecimalError(value.clone(), ty))?;
        Ok(out)
    }

    pub fn checked_add(self, rhs: Value) -> anyhow::Result<Option<Value>> {
        let lhs = self;
        Ok(numeric_op!(lhs + rhs, checked_add))
    }

    pub fn checked_sub(self, rhs: Value) -> anyhow::Result<Option<Value>> {
        let lhs = self;
        Ok(numeric_op!(lhs - rhs, checked_sub))
    }

    pub fn checked_mul(self, rhs: Value) -> anyhow::Result<Option<Value>> {
        let lhs = self;
        Ok(numeric_op!(lhs - rhs, checked_mul))
    }

    pub fn checked_div(self, rhs: Value) -> anyhow::Result<Option<Value>> {
        let lhs = self;
        Ok(numeric_op!(lhs - rhs, checked_div))
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::None
    }
}

impl str::FromStr for Value {
    type Err = anyhow::Error;

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

        Ok(if hex { ty.parse_hex(s)? } else { ty.parse(s)? })
    }
}

impl fmt::Display for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(fmt, "none"),
            Self::Pointer(address) => write!(fmt, "{}", address),
            Self::U8(value) => write!(fmt, "{}", value),
            Self::I8(value) => write!(fmt, "{}", value),
            Self::U16(value) => write!(fmt, "{}", value),
            Self::I16(value) => write!(fmt, "{}", value),
            Self::U32(value) => write!(fmt, "{}", value),
            Self::I32(value) => write!(fmt, "{}", value),
            Self::U64(value) => write!(fmt, "{}", value),
            Self::I64(value) => write!(fmt, "{}", value),
            Self::U128(value) => write!(fmt, "{}", value),
            Self::I128(value) => write!(fmt, "{}", value),
            Self::F32(value) => write!(fmt, "{}", value),
            Self::F64(value) => write!(fmt, "{}", value),
            Self::String(string) => write!(fmt, "{}", EscapeString(&*string)),
            Self::Bytes(bytes) => write!(fmt, "{{{}}}", Hex(&*bytes)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(rename_all = "snake_case", tag = "type", content = "value")]
pub enum ValueRef<'a> {
    None,
    Pointer(Address),
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
    F32(f32),
    F64(f64),
    String(&'a str),
    Bytes(&'a [u8]),
}

impl fmt::Display for ValueRef<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(fmt, "none"),
            Self::Pointer(address) => write!(fmt, "{}", address),
            Self::U8(value) => write!(fmt, "{}", value),
            Self::I8(value) => write!(fmt, "{}", value),
            Self::U16(value) => write!(fmt, "{}", value),
            Self::I16(value) => write!(fmt, "{}", value),
            Self::U32(value) => write!(fmt, "{}", value),
            Self::I32(value) => write!(fmt, "{}", value),
            Self::U64(value) => write!(fmt, "{}", value),
            Self::I64(value) => write!(fmt, "{}", value),
            Self::U128(value) => write!(fmt, "{}", value),
            Self::I128(value) => write!(fmt, "{}", value),
            Self::F32(value) => write!(fmt, "{}", value),
            Self::F64(value) => write!(fmt, "{}", value),
            Self::String(string) => write!(fmt, "{}", EscapeString(&*string)),
            Self::Bytes(bytes) => write!(fmt, "{{{}}}", Hex(&*bytes)),
        }
    }
}

impl cmp::PartialEq<Value> for ValueRef<'_> {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Self::None, Value::None) => true,
            (Self::Pointer(lhs), Value::Pointer(rhs)) => lhs == rhs,
            (Self::U8(lhs), Value::U8(rhs)) => lhs == rhs,
            (Self::I8(lhs), Value::I8(rhs)) => lhs == rhs,
            (Self::U16(lhs), Value::U16(rhs)) => lhs == rhs,
            (Self::I16(lhs), Value::I16(rhs)) => lhs == rhs,
            (Self::U32(lhs), Value::U32(rhs)) => lhs == rhs,
            (Self::I32(lhs), Value::I32(rhs)) => lhs == rhs,
            (Self::U64(lhs), Value::U64(rhs)) => lhs == rhs,
            (Self::I64(lhs), Value::I64(rhs)) => lhs == rhs,
            (Self::U128(lhs), Value::U128(rhs)) => lhs == rhs,
            (Self::I128(lhs), Value::I128(rhs)) => lhs == rhs,
            (Self::F32(lhs), Value::F32(rhs)) => lhs == rhs,
            (Self::F64(lhs), Value::F64(rhs)) => lhs == rhs,
            (Self::String(lhs), Value::String(rhs)) => *lhs == rhs.as_str(),
            (Self::Bytes(lhs), Value::Bytes(rhs)) => *lhs == rhs.as_slice(),
            _ => false,
        }
    }
}
