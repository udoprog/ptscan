use crate::{process::MemoryReader, Address, Endianness, Size, Type, Value};
use anyhow::bail;
use serde::{Deserialize, Serialize};
use std::{fmt, str};

/// The encoding for a string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Encoding {
    #[serde(rename = "utf-8")]
    Utf8,
    #[serde(rename = "utf-16")]
    Utf16,
    #[serde(rename = "utf-16-le")]
    Utf16LE,
    #[serde(rename = "utf-16-be")]
    Utf16BE,
}

impl Encoding {
    /// Get the length of the string in the specified encoding.
    pub fn len(self, s: &str) -> usize {
        match self {
            Self::Utf8 => s.len(),
            Self::Utf16 | Self::Utf16LE | Self::Utf16BE => s.encode_utf16().count() * 2,
        }
    }

    /// Decode a string with the specified encoding from memory.
    ///
    /// This returns the Value (the decoded string), and the amount of bytes
    /// that the scanner can safely advance without missing the next string.
    pub fn decode<'a>(
        self,
        reader: impl MemoryReader<'a>,
        address: Address,
    ) -> anyhow::Result<(Value, Option<usize>)> {
        let (string, len) = match self {
            Self::Utf8 => return self.decode_utf8(reader, address),
            Self::Utf16 => match reader.process().endianness {
                Endianness::LittleEndian => {
                    let string = match read_until_null(reader, address)? {
                        Some(string) => string,
                        None => return Ok((Value::None(Type::String(self)), None)),
                    };

                    (
                        decode_utf16::<byteorder::LittleEndian>(&string)?,
                        string.len() + 1,
                    )
                }
                Endianness::BigEndian => {
                    let string = match read_until_null(reader, address)? {
                        Some(string) => string,
                        None => return Ok((Value::None(Type::String(self)), None)),
                    };

                    (
                        decode_utf16::<byteorder::BigEndian>(&string)?,
                        string.len() + 1,
                    )
                }
            },
            Self::Utf16LE => {
                let string = match read_until_null(reader, address)? {
                    Some(string) => string,
                    None => return Ok((Value::None(Type::String(self)), None)),
                };

                (
                    decode_utf16::<byteorder::LittleEndian>(&string)?,
                    string.len() + 1,
                )
            }
            Self::Utf16BE => {
                let string = match read_until_null(reader, address)? {
                    Some(string) => string,
                    None => return Ok((Value::None(Type::String(self)), None)),
                };

                (
                    decode_utf16::<byteorder::BigEndian>(&string)?,
                    string.len() + 1,
                )
            }
        };

        return Ok((Value::String(self, string), Some(len)));
    }

    /// Decode an UTF-8 string efficiently.
    pub fn decode_utf8<'a>(
        self,
        reader: impl MemoryReader<'a>,
        address: Address,
    ) -> anyhow::Result<(Value, Option<usize>)> {
        let string = match read_until_null(reader, address)? {
            Some(string) => string,
            None => return Ok((Value::None(Type::String(self)), None)),
        };

        let len = string.len() + 1;

        let string = match String::from_utf8(string) {
            Err(e) => {
                let len = e.utf8_error().valid_up_to();
                let mut string = e.into_bytes();
                string.resize_with(len, u8::default);
                String::from_utf8(string)?
            }
            Ok(string) => string,
        };

        Ok((Value::String(self, string), Some(len)))
    }

    /// Default alignment for various encodings.
    pub fn alignment(self) -> usize {
        match self {
            Self::Utf8 => 1,
            Self::Utf16 => 2,
            Self::Utf16LE => 2,
            Self::Utf16BE => 2,
        }
    }
}

impl fmt::Display for Encoding {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Utf8 => "utf-8".fmt(fmt),
            Self::Utf16 => "utf-16".fmt(fmt),
            Self::Utf16LE => "utf-16-le".fmt(fmt),
            Self::Utf16BE => "utf-16-be".fmt(fmt),
        }
    }
}

impl str::FromStr for Encoding {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "utf-8" => Encoding::Utf8,
            "utf-16" => Encoding::Utf16,
            "utf-16-be" => Encoding::Utf16BE,
            "utf-16-le" => Encoding::Utf16LE,
            other => bail!("bad encodign: {}", other),
        })
    }
}

/// Read until we encounter a null.
fn read_until_null<'a>(
    reader: impl MemoryReader<'a>,
    mut address: Address,
) -> anyhow::Result<Option<Vec<u8>>> {
    let mut buf = vec![0u8; 0x100];
    let mut string = Vec::new();

    loop {
        let buf = match reader.read_memory(address, &mut buf)? {
            Some(buf) => buf,
            None => return Ok(None),
        };

        if let Some(index) = memchr::memchr(0x0, &buf) {
            string.extend(&buf[..index]);
            break;
        }

        string.extend(buf);
        address.add_assign(Size::new(0x100))?;
    }

    Ok(Some(string))
}

/// Helper function to decode UTF-16.
fn decode_utf16<B>(mut string: &[u8]) -> anyhow::Result<String>
where
    B: byteorder::ByteOrder,
{
    use std::{char, iter};

    let mut s = String::new();

    while string.len() >= 2 {
        let c = B::read_u16(&string[..2]);

        for c in char::decode_utf16(iter::once(c)) {
            s.push(c?);
        }

        string = &string[2..];
    }

    Ok(s)
}
