use crate::{
    filter_expr::special::find_first_nonzero, process::MemoryReader, Address, Size, Type, Value,
};
use anyhow::bail;
use std::{convert::TryFrom as _, fmt, str};

const READ_BUFFER_SIZE: usize = 0x100;

/// The encoding for a string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Encoding(&'static encoding_rs::Encoding);

impl serde::Serialize for Encoding {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.name().serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for Encoding {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let name = String::deserialize(deserializer)?;

        match encoding_rs::Encoding::for_label(name.as_bytes()) {
            Some(encoding) => Ok(Self(encoding)),
            None => Err(serde::de::Error::custom(format!("bad encoding: {}", name))),
        }
    }
}

impl Encoding {
    /// Get the size of the buffer needed to store the specified string.
    pub fn size(self, s: &str) -> Option<usize> {
        if self.0 == encoding_rs::UTF_8 {
            Some(s.len())
        } else {
            let encoder = self.0.new_encoder();
            encoder.max_buffer_length_from_utf8_if_no_unmappables(s.len())
        }
    }

    /// Encode the specified string into the specified buffer.
    pub fn encode(self, buf: &mut [u8], s: &str) -> anyhow::Result<()> {
        use encoding_rs::EncoderResult;

        let mut encoder = self.0.new_encoder();
        let (result, _, _) = encoder.encode_from_utf8_without_replacement(s, buf, true);

        match result {
            EncoderResult::InputEmpty => (),
            EncoderResult::OutputFull => bail!("output buffer too small"),
            EncoderResult::Unmappable(c) => bail!("encountered unmappable character: {}", c),
        }

        Ok(())
    }

    /// Stream encode.
    pub fn stream_encode(self, buf: &mut Vec<u8>, mut s: &str) -> anyhow::Result<()> {
        use encoding_rs::EncoderResult;

        loop {
            let mut encoder = self.0.new_encoder();
            let (result, read) = encoder.encode_from_utf8_to_vec_without_replacement(s, buf, false);
            s = &s[read..];

            match result {
                EncoderResult::InputEmpty => break,
                EncoderResult::OutputFull => {
                    buf.reserve(READ_BUFFER_SIZE);
                }
                EncoderResult::Unmappable(c) => bail!("encountered unmappable character: {}", c),
            }
        }

        Ok(())
    }

    /// Decode an UTF-8 string efficiently.
    pub fn stream_decode(
        self,
        reader: &impl MemoryReader,
        mut address: Address,
    ) -> anyhow::Result<(Value, Option<usize>)> {
        use encoding_rs::DecoderResult;

        let mut decoder = self.0.new_decoder();

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
                        return Ok((Value::None(Type::String(self)), Some(offset + other)));
                    }
                    // NB: buffer is _all_ zeros.
                    None => {
                        return Ok((
                            Value::None(Type::String(self)),
                            Some(offset + input.len() + 1),
                        ))
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
                        if output.is_empty() {
                            return Ok((Value::None(Type::String(self)), Some(1)));
                        }

                        break 'outer;
                    }
                }
            }
        }

        let advance = output.len() + 1;
        Ok((Value::String(self, output), Some(advance)))
    }

    /// Default alignment for various encodings.
    pub fn alignment(self) -> usize {
        use encoding_rs::*;

        if self.0 == UTF_8 {
            return 1;
        }

        if self.0 == UTF_16LE || self.0 == UTF_16BE {
            return 2;
        }

        1
    }
}

impl Default for Encoding {
    fn default() -> Self {
        Self(encoding_rs::UTF_8)
    }
}

impl fmt::Display for Encoding {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.name().fmt(fmt)
    }
}

impl str::FromStr for Encoding {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match encoding_rs::Encoding::for_label(s.as_bytes()) {
            Some(encoding) => Ok(Self(encoding)),
            None => bail!("bad encoding: {}", s),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Encoding;

    #[test]
    fn test_stream_encode() -> anyhow::Result<()> {
        let encoding = Encoding::default();

        let mut buf = Vec::new();
        encoding.stream_encode(&mut buf, "foobar")?;
        assert_eq!("foobar".as_bytes(), &buf[..]);
        Ok(())
    }
}
