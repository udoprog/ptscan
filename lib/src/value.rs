use crate::{
    error::Error,
    process::Process,
    utils::{EscapeString, Hex},
    Address, Type,
};
use anyhow::bail;
use num_bigint::Sign;
use serde::{Deserialize, Serialize};
use std::convert::TryFrom as _;
use std::{fmt, mem, str};

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
        match $a {
            $(
                Self::$variant(lhs) => numeric_op!(
                    @b lhs, $ty, $variant, $a $op $b,
                    $checked_op,
                    [Pointer, U8, I8, U16, I16, U32, I32, U64, I64, U128, I128]
                ),
            )*
            _ => bail!("bad operation: {} {} {}", $a.ty(), stringify!($op), $b.ty()),
        }
    };

    (@b $lhs:ident, $ty:ty, $variant:ident, $a:ident $op:tt $b:ident, $checked_op:ident, [$($variant2:ident),*]) => {
        match $b {
            $(
                Self::$variant2(rhs) => {
                    $lhs.$checked_op(<$ty>::try_from(rhs)?).map(Self::$variant)
                },
            )+
            _ => bail!("bad operation: {} {} {}", $a.ty(), stringify!($op), $b.ty()),
        }
    };
}

/// A single dynamic literal value.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
    #[serde(rename = "f32")]
    F32(f32),
    #[serde(rename = "f64")]
    F64(f64),
    #[serde(rename = "string")]
    String(String, usize),
    #[serde(rename = "bytes")]
    Bytes(Vec<u8>),
}

impl Value {
    /// Test if this value is something.
    pub fn is_some(&self) -> bool {
        match *self {
            Value::None(..) => false,
            _ => true,
        }
    }

    pub fn is_none(&self) -> bool {
        match *self {
            Value::None(..) => true,
            _ => false,
        }
    }

    pub fn is_zero(&self) -> Option<bool> {
        Some(match *self {
            Self::None(..) => return None,
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
            Self::String(ref s, ..) => s.as_bytes().iter().all(|c| *c == 0),
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
            Self::None(Type::String(..)) | Self::String(..) => false,
            Self::None(Type::Bytes(..)) | Self::Bytes(..) => false,
            _ => true,
        }
    }

    /// Try to treat the value as an address.
    ///
    /// Returns `None` if this is not possible (e.g. value out of range).
    pub fn as_address(&self) -> anyhow::Result<Address> {
        use std::convert::TryFrom;

        let out = match *self {
            Self::Pointer(address) => address,
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
            Self::F32(..) | Self::F64(..) => bail!("float cannot be made into address"),
            Self::None(..) => bail!("nothing cannot be made into address"),
            Self::String(..) => bail!("string cannot be made into address"),
            Self::Bytes(..) => bail!("bytes cannot be made into address"),
        };

        Ok(out)
    }

    /// Encode the given buffer into a value.
    pub fn encode(&self, process: &Process, buf: &mut [u8]) -> Result<(), Error> {
        match *self {
            Self::None(..) => (),
            Self::Pointer(address) => address.encode(process, buf)?,
            Self::U8(value) => {
                buf[0] = value;
            }
            Self::I8(value) => {
                buf[0] = value as u8;
            }
            Self::U16(value) => process.encode_u16(buf, value),
            Self::I16(value) => process.encode_u16(buf, value as u16),
            Self::U32(value) => process.encode_u32(buf, value),
            Self::I32(value) => process.encode_u32(buf, value as u32),
            Self::U64(value) => process.encode_u64(buf, value),
            Self::I64(value) => process.encode_u64(buf, value as u64),
            Self::U128(value) => process.encode_u128(buf, value),
            Self::I128(value) => process.encode_u128(buf, value as u128),
            Self::F32(value) => process.encode_f32(buf, value),
            Self::F64(value) => process.encode_f64(buf, value),
            Self::String(ref s, ..) => {
                let len = usize::min(s.len(), buf.len());
                buf[..len].clone_from_slice(&s.as_bytes()[..len]);
            }
            Self::Bytes(ref bytes) => {
                let len = usize::min(bytes.len(), buf.len());
                buf[..len].clone_from_slice(&bytes[..len]);
            }
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
        match *self {
            Self::None(ty) => ty,
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
            Self::F32(..) => Type::F32,
            Self::F64(..) => Type::F64,
            Self::String(_, len) => Type::String(len),
            Self::Bytes(ref bytes) => Type::Bytes(bytes.len()),
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
            Self::F32(..) => mem::size_of::<f32>(),
            Self::F64(..) => mem::size_of::<f64>(),
            Self::String(string, ..) => string.len(),
            Self::Bytes(bytes) => bytes.len(),
        }
    }

    /// Convert from a big integer.
    pub fn from_bigint(ty: Type, value: &num_bigint::BigInt) -> Result<Self, Error> {
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
            _ => return Ok(Value::None(ty)),
        };

        let out = out.ok_or_else(|| Error::ValueNumberConversion(value.clone(), ty))?;
        Ok(out)
    }

    /// Convert from a big decimal.
    pub fn from_bigdecimal(ty: Type, value: &bigdecimal::BigDecimal) -> Result<Self, Error> {
        use num_traits::cast::ToPrimitive as _;

        let out = match ty {
            Type::F32 => value.to_f32().map(Value::F32),
            Type::F64 => value.to_f64().map(Value::F64),
            _ => return Ok(Value::None(ty)),
        };

        let out = out.ok_or_else(|| Error::ValueDecimalConversion(value.clone(), ty))?;
        Ok(out)
    }

    pub fn add(self, rhs: Value) -> anyhow::Result<Option<Value>> {
        let lhs = self;
        Ok(numeric_op!(lhs + rhs, checked_add))
    }

    pub fn sub(self, rhs: Value) -> anyhow::Result<Option<Value>> {
        let lhs = self;
        Ok(numeric_op!(lhs - rhs, checked_sub))
    }

    pub fn mul(self, rhs: Value) -> anyhow::Result<Option<Value>> {
        let lhs = self;
        Ok(numeric_op!(lhs - rhs, checked_mul))
    }

    pub fn div(self, rhs: Value) -> anyhow::Result<Option<Value>> {
        let lhs = self;
        Ok(numeric_op!(lhs - rhs, checked_div))
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
            Value::F32(value) => write!(fmt, "{}", value),
            Value::F64(value) => write!(fmt, "{}", value),
            Value::String(string, ..) => write!(fmt, "{}", EscapeString(&*string)),
            Value::Bytes(bytes) => write!(fmt, "{{{}}}", Hex(&*bytes)),
        }
    }
}
