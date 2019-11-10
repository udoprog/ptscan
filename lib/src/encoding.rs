use crate::{
    filter_expr::special::find_first_nonzero, process::MemoryReader, Address, Endianness, Size,
    Type, Value,
};
use anyhow::bail;
use serde::{Deserialize, Serialize};
use std::{convert::TryFrom as _, fmt, str};

const READ_BUFFER_SIZE: usize = 0x100;

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
        use encoding_rs::{UTF_16BE, UTF_16LE, UTF_8};

        let decoder = match self {
            Self::Utf8 => UTF_8.new_decoder(),
            Self::Utf16 => match reader.process().endianness {
                Endianness::LittleEndian => UTF_16LE.new_decoder(),
                Endianness::BigEndian => UTF_16BE.new_decoder(),
            },
            Self::Utf16LE => UTF_16LE.new_decoder(),
            Self::Utf16BE => UTF_16BE.new_decoder(),
        };

        self.decode_string(decoder, reader, address)
    }

    /// Decode an UTF-8 string efficiently.
    pub fn decode_string<'a>(
        self,
        mut decoder: encoding_rs::Decoder,
        reader: impl MemoryReader<'a>,
        mut address: Address,
    ) -> anyhow::Result<(Value, Option<usize>)> {
        use encoding_rs::DecoderResult;

        // NB: start out as empty to quickly skip over malformed sequences.
        let mut output = String::new();
        let mut input = vec![0u8; READ_BUFFER_SIZE];
        let read_buffer_size = Size::try_from(READ_BUFFER_SIZE)?;

        let mut offset = 0;

        'outer: loop {
            let len = reader.read_memory(address, &mut input)?;

            let (mut last, mut input) = match len {
                0 => (true, &[][..]),
                len if len == input.len() => (true, &input[..]),
                len => (true, &input[..len]),
            };

            if !last {
                address.add_assign(read_buffer_size)?;
            }

            if !input.is_empty() {
                match find_first_nonzero(input) {
                    Some(0) => (),
                    Some(other) => {
                        return Ok((Value::None(Type::String(self)), Some(offset + other - 1)))
                    }
                    // NB: buffer is _all_ zeros.
                    None => {
                        return Ok((Value::None(Type::String(self)), Some(offset + input.len())))
                    }
                }

                if let Some(index) = memchr::memchr(0x0, &input) {
                    input = &input[..index];
                    last = true;
                } else {
                    offset += input.len();
                }
            }

            loop {
                let (result, read) =
                    decoder.decode_to_string_without_replacement(input, &mut output, last);

                match result {
                    DecoderResult::InputEmpty => {
                        assert_eq!(read, input.len());

                        if last {
                            break 'outer;
                        } else {
                            break;
                        }
                    }
                    DecoderResult::OutputFull => {
                        input = &input[read..];
                        output.reserve(READ_BUFFER_SIZE);
                    }
                    DecoderResult::Malformed(..) => {
                        break 'outer;
                    }
                }
            }
        }

        let len = output.len();
        Ok((Value::String(self, output), Some(len)))
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
