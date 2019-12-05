//! Traits for handling raw serialization.
//!
//! Raw serialization is a _highly unsafe_ form of serialization where we try to
//! achieve as much speed as possible by mapping read data directly from and to
//! native types.

use std::io;

pub trait RawSerialize {
    /// Serialize to the given writer.
    fn raw_serialize<W>(&self, writer: &mut W) -> anyhow::Result<()>
    where
        W: io::Write;
}

pub trait RawDeserialize {
    /// Deserialize raw data from the given reader.
    unsafe fn raw_deserialize<R>(reader: &mut R) -> anyhow::Result<Self>
    where
        Self: Sized,
        R: io::Read;
}

pub trait RawTypeDeserialize {
    unsafe fn raw_type_deserialize<R>(self, reader: &mut R, len: usize) -> io::Result<Vec<u8>>
    where
        R: io::Read;
}

pub trait RawTypeSerialize {
    fn raw_type_serialize<W>(self, writer: &mut W, data: &[u8], len: usize) -> io::Result<()>
    where
        W: io::Write;
}
