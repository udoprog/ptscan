//! The extent of an ELF image's loadable segments, read from its program
//! headers with checked reads only.

use std::{convert::TryFrom, io};

/// `PT_LOAD`, a loadable segment.
const PT_LOAD: u32 = 1;

/// The most program-header bytes read; real objects have a few dozen.
const MAX_PROGRAM_HEADERS: usize = 0x10000;

/// The span of an image's `PT_LOAD` segments, in its own virtual addresses.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct LoadSpan {
    /// The lowest `p_vaddr`.
    pub(super) start: u64,
    /// The highest `p_vaddr + p_memsz`.
    pub(super) end: u64,
}

impl LoadSpan {
    /// Read the span through `read`, which fills a buffer from an offset into
    /// the image (the file, or the image as mapped from its first byte).
    pub(super) fn read(mut read: impl FnMut(u64, &mut [u8]) -> io::Result<()>) -> io::Result<Self> {
        let mut header = [0u8; 64];
        read(0, &mut header)?;
        let header = Reader::header(&header)?;

        // e_phoff, e_phentsize and e_phnum.
        let (phoff, phentsize, phnum) = if header.wide {
            (header.u64(32)?, header.u16(54)?, header.u16(56)?)
        } else {
            (u64::from(header.u32(28)?), header.u16(42)?, header.u16(44)?)
        };

        let entry = if header.wide { 56 } else { 32 };

        if usize::from(phentsize) < entry {
            return Err(bad("ELF program header entries too small"));
        }

        let len = usize::from(phentsize)
            .checked_mul(usize::from(phnum))
            .filter(|len| *len <= MAX_PROGRAM_HEADERS)
            .ok_or_else(|| bad("too many ELF program headers"))?;

        let mut table = vec![0u8; len];
        read(phoff, &mut table)?;
        let table = Reader {
            bytes: &table,
            ..header
        };

        let mut span = None::<Self>;

        for index in 0..usize::from(phnum) {
            let at = index
                .checked_mul(usize::from(phentsize))
                .ok_or_else(|| bad("ELF program header offset overflows"))?;
            let field = |offset: usize| {
                at.checked_add(offset)
                    .ok_or_else(|| bad("ELF program header offset overflows"))
            };

            // p_type, p_vaddr and p_memsz.
            let (ty, vaddr, memsz) = if table.wide {
                (
                    table.u32(at)?,
                    table.u64(field(16)?)?,
                    table.u64(field(40)?)?,
                )
            } else {
                (
                    table.u32(at)?,
                    u64::from(table.u32(field(8)?)?),
                    u64::from(table.u32(field(20)?)?),
                )
            };

            if ty != PT_LOAD {
                continue;
            }

            let end = vaddr
                .checked_add(memsz)
                .ok_or_else(|| bad("ELF segment overflows"))?;

            span = Some(match span {
                Some(s) => Self {
                    start: s.start.min(vaddr),
                    end: s.end.max(end),
                },
                None => Self { start: vaddr, end },
            });
        }

        span.ok_or_else(|| bad("no loadable ELF segments"))
    }
}

fn bad(message: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message.to_owned())
}

/// Checked fixed-width reads in an ELF object's byte order.
#[derive(Clone, Copy)]
struct Reader<'a> {
    bytes: &'a [u8],
    little: bool,
    /// `ELFCLASS64`.
    wide: bool,
}

impl<'a> Reader<'a> {
    fn header(bytes: &'a [u8]) -> io::Result<Self> {
        if bytes.get(..4) != Some(b"\x7fELF") {
            return Err(bad("not an ELF image"));
        }

        let wide = match bytes.get(4) {
            Some(1) => false,
            Some(2) => true,
            _ => return Err(bad("unknown ELF class")),
        };

        let little = match bytes.get(5) {
            Some(1) => true,
            Some(2) => false,
            _ => return Err(bad("unknown ELF data encoding")),
        };

        Ok(Self {
            bytes,
            little,
            wide,
        })
    }

    fn array<const N: usize>(&self, at: usize) -> io::Result<[u8; N]> {
        at.checked_add(N)
            .and_then(|end| self.bytes.get(at..end))
            .and_then(|b| <[u8; N]>::try_from(b).ok())
            .ok_or_else(|| bad("truncated ELF headers"))
    }

    fn u16(&self, at: usize) -> io::Result<u16> {
        let b = self.array(at)?;
        Ok(if self.little {
            u16::from_le_bytes(b)
        } else {
            u16::from_be_bytes(b)
        })
    }

    fn u32(&self, at: usize) -> io::Result<u32> {
        let b = self.array(at)?;
        Ok(if self.little {
            u32::from_le_bytes(b)
        } else {
            u32::from_be_bytes(b)
        })
    }

    fn u64(&self, at: usize) -> io::Result<u64> {
        let b = self.array(at)?;
        Ok(if self.little {
            u64::from_le_bytes(b)
        } else {
            u64::from_be_bytes(b)
        })
    }
}

#[cfg(test)]
pub(super) mod tests {
    use super::*;

    /// A program header: `(p_type, p_vaddr, p_memsz)`.
    pub(in super::super) type Segment = (u32, u64, u64);

    /// A little-endian ELF image prefix with `segments` as its program
    /// headers, 64-bit if `wide`.
    pub(in super::super) fn image(wide: bool, segments: &[Segment]) -> Vec<u8> {
        let (header, entry) = if wide { (64, 56) } else { (52, 32) };
        let mut bytes = vec![0u8; header + entry * segments.len()];
        bytes[..4].copy_from_slice(b"\x7fELF");
        bytes[4] = if wide { 2 } else { 1 };
        bytes[5] = 1;

        let mut put = |at: usize, value: &[u8]| bytes[at..at + value.len()].copy_from_slice(value);
        let count = (segments.len() as u16).to_le_bytes();
        let entry_size = (entry as u16).to_le_bytes();

        if wide {
            put(32, &(header as u64).to_le_bytes());
            put(54, &entry_size);
            put(56, &count);
        } else {
            put(28, &(header as u32).to_le_bytes());
            put(42, &entry_size);
            put(44, &count);
        }

        for (index, &(ty, vaddr, memsz)) in segments.iter().enumerate() {
            let at = header + index * entry;
            put(at, &ty.to_le_bytes());

            if wide {
                put(at + 16, &vaddr.to_le_bytes());
                put(at + 40, &memsz.to_le_bytes());
            } else {
                put(at + 8, &(vaddr as u32).to_le_bytes());
                put(at + 20, &(memsz as u32).to_le_bytes());
            }
        }

        bytes
    }

    pub(in super::super) fn read_from(
        bytes: &[u8],
    ) -> impl FnMut(u64, &mut [u8]) -> io::Result<()> + '_ {
        move |at, buf| {
            let at = usize::try_from(at).map_err(|_| bad("offset"))?;
            let src = bytes
                .get(at..)
                .and_then(|b| b.get(..buf.len()))
                .ok_or_else(|| io::Error::from(io::ErrorKind::UnexpectedEof))?;
            buf.copy_from_slice(src);
            Ok(())
        }
    }

    #[test]
    fn load_span_of_both_classes() {
        let segments = [
            (6, 0x40, 0x2a0),
            (PT_LOAD, 0, 0x1000),
            (PT_LOAD, 0x1000, 0x5000),
            (PT_LOAD, 0x8e478, 0xd38),
            (0x6474_e551, 0, 0),
        ];

        for wide in [false, true] {
            let mut bytes = image(wide, &segments);
            // Pad like a page, so the 64-byte header read fits a 32-bit image.
            bytes.resize(0x1000, 0);
            let span = LoadSpan::read(read_from(&bytes)).unwrap();
            assert_eq!(
                span,
                LoadSpan {
                    start: 0,
                    end: 0x8f1b0
                },
                "wide {}",
                wide
            );
        }
    }

    #[test]
    fn load_span_rejects_bad_images() {
        let mut bytes = image(true, &[(PT_LOAD, 0, 0x1000)]);
        assert!(LoadSpan::read(read_from(&bytes)).is_ok());

        // Truncated program headers.
        let short = &bytes[..bytes.len() - 1];
        assert!(LoadSpan::read(read_from(short)).is_err());

        // No loadable segments.
        let none = image(true, &[(6, 0, 0)]);
        assert!(LoadSpan::read(read_from(&none)).is_err());

        // A segment end past u64.
        let overflow = image(true, &[(PT_LOAD, u64::MAX, 2)]);
        assert!(LoadSpan::read(read_from(&overflow)).is_err());

        // Program header entries smaller than the class needs.
        bytes[54] = 8;
        assert!(LoadSpan::read(read_from(&bytes)).is_err());

        bytes[..4].copy_from_slice(b"\x7fELG");
        assert!(LoadSpan::read(read_from(&bytes)).is_err());
        assert!(LoadSpan::read(read_from(&[])).is_err());
    }
}
