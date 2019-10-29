use byteorder::{ByteOrder, LittleEndian};

pub trait Encode: Sized {
    /// Encode the self into the buffer.
    fn encode(self, buffer: &mut [u8]);
}

impl Encode for u128 {
    fn encode(self, buffer: &mut [u8]) {
        LittleEndian::write_u128(buffer, self);
    }
}

impl Encode for i128 {
    fn encode(self, buffer: &mut [u8]) {
        LittleEndian::write_i128(buffer, self);
    }
}

impl Encode for u64 {
    fn encode(self, buffer: &mut [u8]) {
        LittleEndian::write_u64(buffer, self);
    }
}

impl Encode for i64 {
    fn encode(self, buffer: &mut [u8]) {
        LittleEndian::write_i64(buffer, self);
    }
}

impl Encode for u32 {
    fn encode(self, buffer: &mut [u8]) {
        LittleEndian::write_u32(buffer, self);
    }
}

impl Encode for i32 {
    fn encode(self, buffer: &mut [u8]) {
        LittleEndian::write_i32(buffer, self);
    }
}

impl Encode for u16 {
    fn encode(self, buffer: &mut [u8]) {
        LittleEndian::write_u16(buffer, self);
    }
}

impl Encode for i16 {
    fn encode(self, buffer: &mut [u8]) {
        LittleEndian::write_i16(buffer, self);
    }
}

impl Encode for u8 {
    fn encode(self, buffer: &mut [u8]) {
        buffer[0] = self;
    }
}

impl Encode for i8 {
    fn encode(self, buffer: &mut [u8]) {
        buffer[0] = self as u8;
    }
}

impl<'a> Encode for &'a str {
    fn encode(self, buffer: &mut [u8]) {
        let source = self.as_bytes();
        let len = usize::min(buffer.len(), source.len());
        buffer[..len].clone_from_slice(&source[..len]);
    }
}
