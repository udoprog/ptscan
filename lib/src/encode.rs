use byteorder::{ByteOrder, LittleEndian};

pub trait Encode: Sized {
    /// Encode the value into the buffer.
    fn encode(buffer: &mut [u8], value: Self);
}

impl Encode for u128 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u128(buffer, value);
    }
}

impl Encode for i128 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i128(buffer, value);
    }
}

impl Encode for u64 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u64(buffer, value);
    }
}

impl Encode for i64 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i64(buffer, value);
    }
}

impl Encode for u32 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u32(buffer, value);
    }
}

impl Encode for i32 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i32(buffer, value);
    }
}

impl Encode for u16 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u16(buffer, value);
    }
}

impl Encode for i16 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i16(buffer, value);
    }
}

impl Encode for u8 {
    fn encode(buffer: &mut [u8], value: Self) {
        buffer[0] = value;
    }
}

impl Encode for i8 {
    fn encode(buffer: &mut [u8], value: Self) {
        buffer[0] = value as u8;
    }
}
