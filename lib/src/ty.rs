use crate::{error::Error, process::MemoryReader, Address, Encoding, ProcessInfo, Value};
use anyhow::bail;
use byteorder::ByteOrder as _;
use serde::{Deserialize, Serialize};
use std::{convert::TryFrom as _, fmt, mem, str};

macro_rules! convert {
    ($a:ident, $b:ident) => {
        convert!(@a $a, $b, {
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

    (@a $a:ident, $b:ident, {$([$variant:ident, $ty:ty]),*}) => {
        match $a {
            $(
                Self::$variant => convert!(@b $a, $b, $variant, $ty, {
                    Pointer,
                    U8,
                    I8,
                    U16,
                    I16,
                    U32,
                    I32,
                    U64,
                    I64,
                    U128,
                    I128
                }),
            )*
            _ => return None,
        }
    };

    (@b $a:ident, $b:ident, $into:ident, $ty:ty, {$($variant:ident),*}) => {
        match $b {
            $(Value::$variant(value) => Value::$into(<$ty>::try_from(value).ok()?),)*
            _ => return None,
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum Type {
    #[serde(rename = "none")]
    None,
    #[serde(rename = "pointer")]
    Pointer,
    #[serde(rename = "u8")]
    U8,
    #[serde(rename = "i8")]
    I8,
    #[serde(rename = "u16")]
    U16,
    #[serde(rename = "i16")]
    I16,
    #[serde(rename = "u32")]
    U32,
    #[serde(rename = "i32")]
    I32,
    #[serde(rename = "u64")]
    U64,
    #[serde(rename = "i64")]
    I64,
    #[serde(rename = "u128")]
    U128,
    #[serde(rename = "i128")]
    I128,
    #[serde(rename = "f32")]
    F32,
    #[serde(rename = "f64")]
    F64,
    #[serde(rename = "string")]
    String(Encoding),
    #[serde(rename = "bytes")]
    Bytes(Option<usize>),
}

impl Type {
    /// Indicates if this type is aligned by default or not.
    pub fn is_default_aligned(&self) -> bool {
        match self {
            Self::String(..) => false,
            Self::Bytes(..) => false,
            _ => true,
        }
    }

    /// Convert one value into the given type.
    pub fn convert(self, other: Value) -> Option<Value> {
        let other = match (self, other) {
            (Self::None, Value::None(..)) => return None,
            (Self::Pointer, other @ Value::Pointer(..)) => return Some(other),
            (Self::U8, other @ Value::U8(..)) => return Some(other),
            (Self::I8, other @ Value::I8(..)) => return Some(other),
            (Self::U16, other @ Value::U16(..)) => return Some(other),
            (Self::I16, other @ Value::I16(..)) => return Some(other),
            (Self::U32, other @ Value::U32(..)) => return Some(other),
            (Self::I32, other @ Value::I32(..)) => return Some(other),
            (Self::U64, other @ Value::U64(..)) => return Some(other),
            (Self::I64, other @ Value::I64(..)) => return Some(other),
            (Self::U128, other @ Value::U128(..)) => return Some(other),
            (Self::I128, other @ Value::I128(..)) => return Some(other),
            (Self::String(encoding), Value::String(_, string)) => {
                return Some(Value::String(encoding, string));
            }
            (Self::Bytes(..), other @ Value::Bytes(..)) => return Some(other),
            (Self::Bytes(None), Value::String(encoding, string)) => {
                let mut buf = Vec::new();
                encoding.stream_encode(&mut buf, string.as_str()).ok()?;
                return Some(Value::Bytes(buf));
            }
            (Self::Bytes(Some(len)), Value::String(encoding, string)) => {
                let mut buf = Vec::new();
                encoding.stream_encode(&mut buf, string.as_str()).ok()?;
                buf.resize_with(usize::min(len, buf.len()), u8::default);
                return Some(Value::Bytes(buf));
            }
            (_, other) => other,
        };

        Some(convert!(self, other))
    }

    /// Initialize a default value of the given type.
    pub fn default_value(&self) -> Value {
        match *self {
            Self::None => Value::None(Type::None),
            Self::Pointer => Value::Pointer(Default::default()),
            Self::U8 => Value::U8(Default::default()),
            Self::I8 => Value::I8(Default::default()),
            Self::U16 => Value::U16(Default::default()),
            Self::I16 => Value::I16(Default::default()),
            Self::U32 => Value::U32(Default::default()),
            Self::I32 => Value::I32(Default::default()),
            Self::U64 => Value::U64(Default::default()),
            Self::I64 => Value::I64(Default::default()),
            Self::U128 => Value::U128(Default::default()),
            Self::I128 => Value::I128(Default::default()),
            Self::F32 => Value::F32(Default::default()),
            Self::F64 => Value::F64(Default::default()),
            Self::String(..) => Value::String(Default::default(), Default::default()),
            Self::Bytes(..) => Value::Bytes(Default::default()),
        }
    }

    /// Parse a string value of the type.
    pub fn parse(&self, input: &str) -> Result<Value, Error> {
        let value = match *self {
            Self::None => return Err(Error::TypeParseNone),
            Self::Pointer => {
                return Ok(Value::Pointer(
                    str::parse::<Address>(input).map_err(|_| Error::TypeParseError)?,
                ));
            }
            Self::U8 => str::parse::<u8>(input).map(Value::U8),
            Self::I8 => str::parse::<i8>(input).map(Value::I8),
            Self::U16 => str::parse::<u16>(input).map(Value::U16),
            Self::I16 => str::parse::<i16>(input).map(Value::I16),
            Self::U32 => str::parse::<u32>(input).map(Value::U32),
            Self::I32 => str::parse::<i32>(input).map(Value::I32),
            Self::U64 => str::parse::<u64>(input).map(Value::U64),
            Self::I64 => str::parse::<i64>(input).map(Value::I64),
            Self::U128 => str::parse::<u128>(input).map(Value::U128),
            Self::I128 => str::parse::<i128>(input).map(Value::I128),
            Self::F32 => {
                return Ok(Value::F32(
                    str::parse::<f32>(input).map_err(|_| Error::TypeParseError)?,
                ))
            }
            Self::F64 => {
                return Ok(Value::F64(
                    str::parse::<f64>(input).map_err(|_| Error::TypeParseError)?,
                ))
            }
            Self::String(..) => return Err(Error::TypeParseString),
            Self::Bytes(..) => return Err(Error::TypeParseBytes),
        };

        value.map_err(|_| Error::TypeParseError)
    }

    /// Parse a string as hex.
    pub fn parse_hex(&self, input: &str) -> Result<Value, Error> {
        let value = match *self {
            Self::None => return Err(Error::TypeParseNone),
            Self::Pointer => {
                u64::from_str_radix(input, 16).map(|a| Value::Pointer(Address::new(a)))
            }
            Self::U8 => u8::from_str_radix(input, 16).map(Value::U8),
            Self::I8 => i8::from_str_radix(input, 16).map(Value::I8),
            Self::U16 => u16::from_str_radix(input, 16).map(Value::U16),
            Self::I16 => i16::from_str_radix(input, 16).map(Value::I16),
            Self::U32 => u32::from_str_radix(input, 16).map(Value::U32),
            Self::I32 => i32::from_str_radix(input, 16).map(Value::I32),
            Self::U64 => u64::from_str_radix(input, 16).map(Value::U64),
            Self::I64 => i64::from_str_radix(input, 16).map(Value::I64),
            Self::U128 => u128::from_str_radix(input, 16).map(Value::U128),
            Self::I128 => i128::from_str_radix(input, 16).map(Value::I128),
            Self::F32 => return Err(Error::TypeParseFloat),
            Self::F64 => return Err(Error::TypeParseFloat),
            Self::String(..) => return Err(Error::TypeParseString),
            Self::Bytes(..) => return Err(Error::TypeParseBytes),
        };

        value.map_err(|_| Error::TypeParseError)
    }

    /// The default alignment for every type.
    #[inline]
    pub fn alignment<P>(&self, process: &P) -> Option<usize>
    where
        P: ProcessInfo,
    {
        Some(match *self {
            Self::None => return None,
            Self::Pointer => process.pointer_width(),
            Self::U8 => mem::size_of::<u8>(),
            Self::I8 => mem::size_of::<i8>(),
            Self::U16 => mem::size_of::<u16>(),
            Self::I16 => mem::size_of::<i16>(),
            Self::U32 => mem::size_of::<u32>(),
            Self::I32 => mem::size_of::<i32>(),
            Self::U64 => mem::size_of::<u64>(),
            Self::I64 => mem::size_of::<i64>(),
            Self::U128 => mem::size_of::<u128>(),
            Self::I128 => mem::size_of::<i128>(),
            Self::F32 => mem::size_of::<f32>(),
            Self::F64 => mem::size_of::<f64>(),
            Self::Bytes(_) => 1,
            Self::String(encoding) => encoding.alignment(),
        })
    }

    /// The known in-memory size that a type has.
    #[inline]
    pub fn size<P>(&self, process: &P) -> Option<usize>
    where
        P: ProcessInfo,
    {
        Some(match *self {
            Self::None => 0,
            Self::Pointer => process.pointer_width(),
            Self::U8 => mem::size_of::<u8>(),
            Self::I8 => mem::size_of::<i8>(),
            Self::U16 => mem::size_of::<u16>(),
            Self::I16 => mem::size_of::<i16>(),
            Self::U32 => mem::size_of::<u32>(),
            Self::I32 => mem::size_of::<i32>(),
            Self::U64 => mem::size_of::<u64>(),
            Self::I64 => mem::size_of::<i64>(),
            Self::U128 => mem::size_of::<u128>(),
            Self::I128 => mem::size_of::<i128>(),
            Self::F32 => mem::size_of::<f32>(),
            Self::F64 => mem::size_of::<f64>(),
            Self::Bytes(len) => return len,
            Self::String(..) => return None,
        })
    }

    /// Decode the given buffer into a value.
    pub fn decode<R>(self, reader: &R, address: Address) -> anyhow::Result<(Value, Option<usize>)>
    where
        R: MemoryReader,
    {
        // Decode a buffer, and depending on its width decode it differently.
        macro_rules! decode_buf {
            ($buf:expr, $ty:ty, $reader:ident) => {{
                if $buf.len() != std::mem::size_of::<$ty>() {
                    return Err(
                        Error::BufferOverflow(std::mem::size_of::<$ty>(), $buf.len()).into(),
                    );
                }

                <<R as MemoryReader>::Process as ProcessInfo>::ByteOrder::$reader($buf) as $ty
            }};
        }

        macro_rules! decode_byte {
            ($buf:expr, $ty:ty) => {{
                if $buf.is_empty() {
                    return Err(Error::BufferOverflow(1, $buf.len()).into());
                }

                $buf[0] as $ty
            }};
        }

        if let Some(size) = self.size(reader.process()) {
            let mut buf = vec![0u8; size];

            let len = reader.read_memory(address, &mut buf)?;

            if len != buf.len() {
                return Ok((Value::None(self), None));
            }

            let buf = &buf[..];

            let value = match self {
                Self::None => Value::None(Type::None),
                Type::Pointer => Value::Pointer(Address::decode(reader.process(), buf)?),
                Self::U8 => Value::U8(decode_byte!(buf, u8)),
                Self::I8 => Value::I8(decode_byte!(buf, i8)),
                Self::U16 => Value::U16(decode_buf!(buf, u16, read_u16)),
                Self::I16 => Value::I16(decode_buf!(buf, i16, read_u16)),
                Self::U32 => Value::U32(decode_buf!(buf, u32, read_u32)),
                Self::I32 => Value::I32(decode_buf!(buf, i32, read_u32)),
                Self::U64 => Value::U64(decode_buf!(buf, u64, read_u64)),
                Self::I64 => Value::I64(decode_buf!(buf, i64, read_u64)),
                Self::U128 => Value::U128(decode_buf!(buf, u128, read_u128)),
                Self::I128 => Value::I128(decode_buf!(buf, i128, read_u128)),
                Self::F32 => Value::F32(decode_buf!(buf, f32, read_f32)),
                Self::F64 => Value::F64(decode_buf!(buf, f64, read_f64)),
                Self::Bytes(..) => Value::Bytes(buf.to_vec()),
                other => bail!("tried to decode unsized {} as sized", other),
            };

            return Ok((value, Some(size)));
        }

        return match self {
            Self::String(encoding) => Ok(encoding.stream_decode(reader, address)?),
            other => bail!("tried to decode sized {} as unsized", other),
        };
    }

    /// Convert into a type which implements `fmt::Display` for a human-readable string.
    pub fn human_display(&self) -> HumanDisplay {
        HumanDisplay { ty: *self }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::None
    }
}

impl fmt::Display for Type {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let o = match *self {
            Type::None => "none",
            Type::Pointer => "pointer",
            Type::U8 => "u8",
            Type::I8 => "i8",
            Type::U16 => "u16",
            Type::I16 => "i16",
            Type::U32 => "u32",
            Type::I32 => "i32",
            Type::U64 => "u64",
            Type::I64 => "i64",
            Type::U128 => "u128",
            Type::I128 => "i128",
            Type::F32 => "f32",
            Type::F64 => "f64",
            Type::String(encoding) => return write!(fmt, "string/{}", encoding),
            Type::Bytes(None) => "bytes",
            Type::Bytes(Some(len)) => return write!(fmt, "bytes/{}", len),
        };

        o.fmt(fmt)
    }
}

impl str::FromStr for Type {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut it = s.split('/');

        let first = match it.next() {
            Some(base) => base,
            None => return Err(Error::TypeBadBase.into()),
        };

        let ty = match first {
            "pointer" => Type::Pointer,
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
            "f32" => Type::F32,
            "f64" => Type::F64,
            "string" => {
                let encoding = match it.next() {
                    Some(s) => str::parse::<Encoding>(s)?,
                    None => Encoding::default(),
                };

                Type::String(encoding)
            }
            "bytes" => {
                let size = match it.next() {
                    Some(size) => Some(str::parse::<usize>(size).map_err(|_| Error::TypeBadSize)?),
                    None => None,
                };

                Type::Bytes(size)
            }
            "none" => Type::None,
            other => return Err(Error::IllegalType(other.to_string()).into()),
        };

        Ok(ty)
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
            Type::Pointer => write!(fmt, "pointer"),
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
            Type::F32 => write!(fmt, "32-bit floating point number"),
            Type::F64 => write!(fmt, "64-bit floating point number"),
            Type::String(encoding) => write!(fmt, "{} string", encoding),
            Type::Bytes(Some(len)) => write!(fmt, "a byte array of length {}", len),
            Type::Bytes(None) => write!(fmt, "an unsized byte array"),
        }
    }
}
