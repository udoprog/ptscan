mod words;

use std::fmt;

use crate::{
    process::{MemoryInformation, MemoryProtect},
    AddressRange, Size,
};

pub use self::words::Words;

/// Helper macro to handle fallible operations inside of a fallible iterator.
macro_rules! try_iter {
    ($expr:expr) => {
        match $expr {
            Ok(value) => value,
            Err(e) => return Some(Err(Into::into(e))),
        }
    };
}

pub trait IteratorExtension {
    fn chunked(self, chunk_size: Size) -> Chunked<Self>
    where
        Self: Sized + Iterator<Item = anyhow::Result<MemoryInformation>>,
    {
        Chunked {
            iter: self,
            current: None,
            offset: Size::new(0),
            chunk_size,
        }
    }

    fn only_relevant(self) -> OnlyRelevant<Self>
    where
        Self: Sized + Iterator<Item = anyhow::Result<MemoryInformation>>,
    {
        OnlyRelevant { iter: self }
    }
}

impl<I> IteratorExtension for I where I: Iterator {}

/// A filter which filters out only relevant pages.
pub struct OnlyRelevant<I>
where
    I: Iterator,
{
    iter: I,
}

impl<I> Iterator for OnlyRelevant<I>
where
    I: Iterator<Item = anyhow::Result<MemoryInformation>>,
{
    type Item = anyhow::Result<MemoryInformation>;

    fn next(&mut self) -> Option<Self::Item> {
        use crate::process::{MemoryState, MemoryType};

        loop {
            let memory_info = match self.iter.next() {
                Some(memory_info) => try_iter!(memory_info),
                None => return None,
            };

            match (memory_info.state, memory_info.ty) {
                (_, Some(MemoryType::Mapped)) => continue,
                (MemoryState::Commit, _) => {
                    if memory_info.protect.contains(MemoryProtect::NoAccess) {
                        continue;
                    }

                    if memory_info.protect.contains(MemoryProtect::Guard) {
                        continue;
                    }

                    if memory_info.protect.contains(MemoryProtect::NoCache) {
                        continue;
                    }

                    if !memory_info.is_writable() {
                        continue;
                    }

                    return Some(Ok(memory_info));
                }
                _ => continue,
            }
        }
    }
}

pub struct Chunked<I>
where
    I: Iterator,
{
    iter: I,
    current: Option<MemoryInformation>,
    offset: Size,
    chunk_size: Size,
}

impl<I> Iterator for Chunked<I>
where
    I: Iterator<Item = anyhow::Result<MemoryInformation>>,
{
    type Item = anyhow::Result<(MemoryInformation, AddressRange)>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.current.is_none() {
                self.current = match self.iter.next() {
                    Some(memory_info) => Some(try_iter!(memory_info)),
                    None => return None,
                }
            }

            let memory_info = match self.current.as_ref() {
                Some(memory_info) => memory_info,
                None => return None,
            };

            if self.offset >= memory_info.range.size {
                self.offset = Size::new(0);
                self.current.take();
                continue;
            }

            let s = self.offset;
            let e = Size::min(
                self.offset.checked_add(self.chunk_size)?,
                memory_info.range.size,
            );

            self.offset = e;

            let range = AddressRange {
                base: memory_info.range.base.checked_add_size(s)?,
                size: e.checked_sub(s)?,
            };

            return Some(Ok((memory_info.clone(), range)));
        }
    }
}

pub struct Hex<'a>(pub &'a [u8]);

impl fmt::Display for Hex<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut it = self.0.iter().cloned();

        let last = it.next_back();

        for c in it {
            write!(fmt, "{:02X} ", c)?;
        }

        if let Some(c) = last {
            write!(fmt, "{:02X}", c)?;
        }

        Ok(())
    }
}

pub struct EscapeString<'a>(pub &'a str);

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
                c => write!(fmt, "{}", c)?,
            }
        }

        write!(fmt, "\"")?;
        Ok(())
    }
}
