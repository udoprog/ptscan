use crate::{
    filter_expr::special::{find_first_nonzero, is_all_zeros},
    Address, Addresses, Alignment, BufferProxy, ProcessHandle, Proxy as _, Special, Test, Token,
    Type, TypedFilterExpr, Value, ValueRef, Values,
};
use anyhow::bail;
use std::{convert::TryInto as _, marker, ops::Bound};

fn align(to_align: &mut usize, alignment: usize) {
    let rem = *to_align % alignment;

    if rem > 0 {
        *to_align += alignment - rem;
    }
}

/// How many bytes [`DefaultScanner`] scans between checks for cancellation.
const CANCEL_CHECK_INTERVAL: usize = 0x1000;

/// Trait deciding how the scan is performed.
pub trait ScannerTrait {
    /// Find the first offset which satistifes the trait.
    fn find_trait_offset(&self, data: &[u8]) -> Option<usize>;

    /// Test if the found byte slice satisfies the trait.
    fn test(&self, data: &[u8]) -> bool;
}

/// Find zeroed bytes.
pub struct FindZero;

impl ScannerTrait for FindZero {
    #[inline(always)]
    fn find_trait_offset(&self, data: &[u8]) -> Option<usize> {
        memchr::memchr(0, data)
    }

    #[inline(always)]
    fn test(&self, data: &[u8]) -> bool {
        is_all_zeros(data)
    }
}

/// Find non-zeroed bytes.
pub struct FindNonZero;

impl ScannerTrait for FindNonZero {
    #[inline(always)]
    fn find_trait_offset(&self, data: &[u8]) -> Option<usize> {
        find_first_nonzero(data)
    }

    #[inline(always)]
    fn test(&self, _: &[u8]) -> bool {
        // NB: doesn't matter
        true
    }
}

/// A chunk of process memory to scan.
pub struct Chunk<'a> {
    /// Address of the first byte of `data`.
    pub base: Address,
    pub data: &'a [u8],
    pub handle: &'a ProcessHandle,
}

/// Where a scan puts what it finds.
pub struct ScanSink<'a> {
    pub addresses: &'a mut Addresses,
    pub values: &'a mut Values,
    pub hits: &'a mut u64,
}

pub trait Scanner: Send + Sync {
    fn scan(&self, chunk: Chunk<'_>, out: &mut ScanSink<'_>, cancel: &Token) -> anyhow::Result<()>;
}

/// A default scanner with a filter.
pub struct DefaultScanner<'a> {
    filter: TypedFilterExpr<'a>,
    special: Option<&'a Special>,
    alignment: Option<usize>,
    step_size: usize,
    value_type: Type,
}

impl<'a> DefaultScanner<'a> {
    pub fn new(
        filter: TypedFilterExpr<'a>,
        special: Option<&'a Special>,
        alignment: Option<usize>,
        step_size: usize,
        value_type: Type,
    ) -> Self {
        Self {
            filter,
            special,
            alignment,
            step_size,
            value_type,
        }
    }
}

impl<'a> Scanner for DefaultScanner<'a> {
    fn scan(&self, chunk: Chunk<'_>, out: &mut ScanSink<'_>, cancel: &Token) -> anyhow::Result<()> {
        let Chunk { base, data, handle } = chunk;
        let ScanSink {
            addresses,
            values,
            hits,
        } = out;
        let mut offset = match self.special {
            Some(special) => match special.test(data) {
                Some(offset) => offset,
                None => return Ok(()),
            },
            None => 0usize,
        };

        if let Some(size) = self.alignment {
            align(&mut offset, size);
        }

        let mut last_address = None;
        let mut next_cancel_check = 0usize;

        while offset < data.len() {
            if offset >= next_cancel_check {
                if cancel.test() {
                    break;
                }

                next_cancel_check = offset.saturating_add(CANCEL_CHECK_INTERVAL);
            }

            let address = match base.checked_add(offset.try_into()?) {
                Some(address) => address,
                None => bail!("base `{}` + offset `{}` out of range", base, offset),
            };

            let Some(rest) = data.get(offset..) else {
                break;
            };

            // A value running past the end of the chunk is read from the
            // process.
            let mut proxy = BufferProxy::with_fallback(address, rest, handle);

            // sanity check
            if let Some(last_address) = last_address {
                if last_address >= address {
                    bail!(
                        "BUG: address did not increase during scan: {} -> {}",
                        last_address,
                        address
                    )
                }
            }

            last_address = Some(address);

            if let Test::True = self
                .filter
                .test(ValueRef::None, ValueRef::None, &mut proxy)?
            {
                **hits += 1;
                let (value, advance) = proxy.eval(self.value_type)?;

                addresses.push(address);
                values.push(value);

                if let Some(advance) = advance {
                    if advance == 0 {
                        bail!("BUG: attempt to advance by 0 bytes");
                    }

                    offset += advance;
                } else {
                    offset += self.step_size;
                }
            } else {
                offset += self.step_size;
            }

            if let Some(special) = self.special {
                let Some(rest) = data.get(offset..) else {
                    break;
                };

                offset += match special.test(rest) {
                    Some(o) => o,
                    None => return Ok(()),
                };

                if let Some(size) = self.alignment {
                    align(&mut offset, size);
                }
            }
        }

        Ok(())
    }
}

/// A highly specialized, aligned zero scanner.
pub struct BytesScanner<A, B, T>
where
    A: Alignment,
    B: Send + Sync + byteorder::ByteOrder,
    T: Send + Sync + ScannerTrait,
{
    scanner_trait: T,
    alignment: A,
    type_size: usize,
    value_type: Type,
    marker: marker::PhantomData<B>,
}

impl<A, B, T> BytesScanner<A, B, T>
where
    A: Alignment,
    B: Send + Sync + byteorder::ByteOrder,
    T: Send + Sync + ScannerTrait,
{
    pub fn new(scanner_trait: T, alignment: A, type_size: usize, value_type: Type) -> Self {
        Self {
            scanner_trait,
            alignment,
            type_size,
            value_type,
            marker: marker::PhantomData,
        }
    }
}

impl<A, B, T> Scanner for BytesScanner<A, B, T>
where
    A: Alignment,
    B: Send + Sync + byteorder::ByteOrder,
    T: Send + Sync + ScannerTrait,
{
    fn scan(&self, chunk: Chunk<'_>, out: &mut ScanSink<'_>, cancel: &Token) -> anyhow::Result<()> {
        let Chunk { base, data, handle } = chunk;
        let ScanSink {
            addresses,
            values,
            hits,
        } = out;
        let mut offset = 0usize;

        while offset < data.len() && !cancel.test() {
            let o = match self.scanner_trait.find_trait_offset(&data[offset..]) {
                Some(o) => o,
                None => break,
            };

            offset += o;

            self.alignment.align(&mut offset);

            // The element at or past the end belongs to the next chunk.
            if offset >= data.len() {
                break;
            }

            let d = &data[offset..];

            let mut tmp;

            // special case: buffer doesn't contain enough data, so we need to
            // read using the handle.
            let (d, address) = if d.len() < self.type_size {
                tmp = vec![0u8; self.type_size];

                let address = match base.checked_add(offset.try_into()?) {
                    Some(address) => address,
                    None => break,
                };

                let read = handle.process.read_process_memory(address, &mut tmp)?;

                if read != self.type_size {
                    break;
                }

                (&tmp[..], Some(address))
            } else {
                (&d[..self.type_size], None)
            };

            if !self.scanner_trait.test(d) {
                offset += self.alignment.step();
                continue;
            }

            let value = self.value_type.decode_fixed::<B>(d);

            let address = match address {
                Some(address) => address,
                None => match base.checked_add(offset.try_into()?) {
                    Some(address) => address,
                    None => break,
                },
            };

            addresses.push(address);
            values.push(value);
            **hits += 1;
            offset += self.alignment.step();
        }

        Ok(())
    }
}

/// A highly specialized, aligned zero scanner.
pub struct BufferScanner<B>
where
    B: Send + Sync + byteorder::ByteOrder,
{
    value_type: Type,
    buffer: Vec<u8>,
    marker: marker::PhantomData<B>,
}

impl<B> BufferScanner<B>
where
    B: Send + Sync + byteorder::ByteOrder,
{
    pub fn new(value_type: Type, buffer: Vec<u8>) -> Self {
        Self {
            value_type,
            buffer,
            marker: marker::PhantomData,
        }
    }
}

impl<B> Scanner for BufferScanner<B>
where
    B: Send + Sync + byteorder::ByteOrder,
{
    fn scan(&self, chunk: Chunk<'_>, out: &mut ScanSink<'_>, cancel: &Token) -> anyhow::Result<()> {
        let Chunk {
            base,
            data,
            handle: _,
        } = chunk;
        let ScanSink {
            addresses,
            values,
            hits,
        } = out;
        let mut offset = 0usize;

        while offset < data.len() && !cancel.test() {
            let o = match memchr::memchr(self.buffer[0], &data[offset..]) {
                Some(o) => o,
                None => break,
            };

            offset += o;

            let d = &data[offset..];

            if d.len() < self.buffer.len() {
                break;
            }

            let d = &d[..self.buffer.len()];

            if d != &self.buffer[..] {
                offset += 1;
                continue;
            }

            let value = self.value_type.decode_fixed::<B>(d);
            let address = match base.checked_add(offset.try_into()?) {
                Some(address) => address,
                None => break,
            };

            addresses.push(address);
            values.push(value);
            **hits += 1;
            offset += self.buffer.len();
        }

        Ok(())
    }
}

/// A fixed-size primitive which [`CompareScanner`] decodes straight from the
/// chunk.
pub trait Primitive: Copy + PartialOrd + Send + Sync + 'static {
    /// The in-memory size of the primitive.
    const SIZE: usize;

    /// Decode the primitive from the first [`Primitive::SIZE`] bytes of
    /// `data`, or `None` if there are not that many.
    fn decode<B: byteorder::ByteOrder>(data: &[u8]) -> Option<Self>;

    /// Convert into a dynamic value.
    fn into_value(self) -> Value;
}

/// A 32-bit pointer compared as an integer.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Pointer32(pub u32);

/// A 64-bit pointer compared as an integer.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Pointer64(pub u64);

macro_rules! primitive {
    ($ty:ty, $variant:ident, $read:ident) => {
        impl Primitive for $ty {
            const SIZE: usize = std::mem::size_of::<$ty>();

            #[inline(always)]
            fn decode<B: byteorder::ByteOrder>(data: &[u8]) -> Option<Self> {
                Some(B::$read(data.get(..Self::SIZE)?) as $ty)
            }

            #[inline(always)]
            fn into_value(self) -> Value {
                Value::$variant(self)
            }
        }
    };
}

macro_rules! primitive_byte {
    ($ty:ty, $variant:ident) => {
        impl Primitive for $ty {
            const SIZE: usize = 1;

            #[inline(always)]
            fn decode<B: byteorder::ByteOrder>(data: &[u8]) -> Option<Self> {
                Some(*data.first()? as $ty)
            }

            #[inline(always)]
            fn into_value(self) -> Value {
                Value::$variant(self)
            }
        }
    };
}

primitive_byte!(u8, U8);
primitive_byte!(i8, I8);
primitive!(u16, U16, read_u16);
primitive!(i16, I16, read_u16);
primitive!(u32, U32, read_u32);
primitive!(i32, I32, read_u32);
primitive!(u64, U64, read_u64);
primitive!(i64, I64, read_u64);
primitive!(u128, U128, read_u128);
primitive!(i128, I128, read_u128);

impl Primitive for f32 {
    const SIZE: usize = 4;

    #[inline(always)]
    fn decode<B: byteorder::ByteOrder>(data: &[u8]) -> Option<Self> {
        Some(B::read_f32(data.get(..Self::SIZE)?))
    }

    #[inline(always)]
    fn into_value(self) -> Value {
        Value::F32(self)
    }
}

impl Primitive for f64 {
    const SIZE: usize = 8;

    #[inline(always)]
    fn decode<B: byteorder::ByteOrder>(data: &[u8]) -> Option<Self> {
        Some(B::read_f64(data.get(..Self::SIZE)?))
    }

    #[inline(always)]
    fn into_value(self) -> Value {
        Value::F64(self)
    }
}

impl Primitive for Pointer32 {
    const SIZE: usize = 4;

    #[inline(always)]
    fn decode<B: byteorder::ByteOrder>(data: &[u8]) -> Option<Self> {
        Some(Pointer32(B::read_u32(data.get(..Self::SIZE)?)))
    }

    #[inline(always)]
    fn into_value(self) -> Value {
        Value::Pointer(Address::from(self.0))
    }
}

impl Primitive for Pointer64 {
    const SIZE: usize = 8;

    #[inline(always)]
    fn decode<B: byteorder::ByteOrder>(data: &[u8]) -> Option<Self> {
        Some(Pointer64(B::read_u64(data.get(..Self::SIZE)?)))
    }

    #[inline(always)]
    fn into_value(self) -> Value {
        Value::Pointer(Address::from(self.0))
    }
}

/// A test on a decoded primitive.
pub trait Predicate<T>: Send + Sync {
    fn test(&self, value: T) -> bool;
}

/// Matches values inside of an interval, like `value > 10` or
/// `value >= 10 and value < 20`.
#[derive(Debug, Clone, Copy)]
pub struct Interval<T> {
    pub lower: Bound<T>,
    pub upper: Bound<T>,
}

impl<T> Interval<T>
where
    T: Primitive,
{
    /// Narrow the lower bound to also satisfy `bound`.
    pub fn narrow_lower(&mut self, bound: Bound<T>) {
        self.lower = match (self.lower, bound) {
            (Bound::Unbounded, b) | (b, Bound::Unbounded) => b,
            (Bound::Included(a), Bound::Included(b)) => Bound::Included(if b > a { b } else { a }),
            (Bound::Excluded(a), Bound::Excluded(b)) => Bound::Excluded(if b > a { b } else { a }),
            (Bound::Included(i), Bound::Excluded(e)) | (Bound::Excluded(e), Bound::Included(i)) => {
                if i > e {
                    Bound::Included(i)
                } else {
                    Bound::Excluded(e)
                }
            }
        };
    }

    /// Narrow the upper bound to also satisfy `bound`.
    pub fn narrow_upper(&mut self, bound: Bound<T>) {
        self.upper = match (self.upper, bound) {
            (Bound::Unbounded, b) | (b, Bound::Unbounded) => b,
            (Bound::Included(a), Bound::Included(b)) => Bound::Included(if b < a { b } else { a }),
            (Bound::Excluded(a), Bound::Excluded(b)) => Bound::Excluded(if b < a { b } else { a }),
            (Bound::Included(i), Bound::Excluded(e)) | (Bound::Excluded(e), Bound::Included(i)) => {
                if i < e {
                    Bound::Included(i)
                } else {
                    Bound::Excluded(e)
                }
            }
        };
    }
}

impl<T> Predicate<T> for Interval<T>
where
    T: Primitive,
{
    #[inline(always)]
    fn test(&self, value: T) -> bool {
        let lower = match self.lower {
            Bound::Included(lower) => value >= lower,
            Bound::Excluded(lower) => value > lower,
            Bound::Unbounded => true,
        };

        let upper = match self.upper {
            Bound::Included(upper) => value <= upper,
            Bound::Excluded(upper) => value < upper,
            Bound::Unbounded => true,
        };

        lower && upper
    }
}

/// Matches values not equal to a constant.
#[derive(Debug, Clone, Copy)]
pub struct NotEqual<T>(pub T);

impl<T> Predicate<T> for NotEqual<T>
where
    T: Primitive,
{
    #[inline(always)]
    fn test(&self, value: T) -> bool {
        value != self.0
    }
}

/// A scanner comparing fixed-size primitives against constants, decoding them
/// straight from the chunk and only building a [`Value`] on a hit.
///
/// It finds the same values as [`DefaultScanner`] without a special: offsets
/// step by the alignment (or a byte when unaligned), a hit advances by the
/// size of the value, and a value running past the end of the chunk is read
/// from the process.
pub struct CompareScanner<B, T, P> {
    predicate: P,
    alignment: Option<usize>,
    marker: marker::PhantomData<fn() -> (B, T)>,
}

impl<B, T, P> CompareScanner<B, T, P>
where
    B: byteorder::ByteOrder,
    T: Primitive,
    P: Predicate<T>,
{
    pub fn new(predicate: P, alignment: Option<usize>) -> Self {
        Self {
            predicate,
            alignment: alignment.filter(|a| *a > 1),
            marker: marker::PhantomData,
        }
    }

    /// Record a hit at `offset`.
    #[inline(always)]
    fn push(base: Address, offset: usize, value: T, out: &mut ScanSink<'_>) -> anyhow::Result<()> {
        let address = match base.checked_add(offset.try_into()?) {
            Some(address) => address,
            None => bail!("base `{}` + offset `{}` out of range", base, offset),
        };

        out.addresses.push(address);
        out.values.push(value.into_value());
        *out.hits = out.hits.saturating_add(1);
        Ok(())
    }

    /// Scan with each offset one value further than the last, for when the
    /// alignment is the size of the value.
    ///
    /// Returns the offset where the values stop fitting in `data`, or `None`
    /// if cancelled.
    fn scan_packed(
        &self,
        base: Address,
        data: &[u8],
        out: &mut ScanSink<'_>,
        cancel: &Token,
    ) -> anyhow::Result<Option<usize>> {
        let mut offset = 0usize;

        // NB: CANCEL_CHECK_INTERVAL is a multiple of every primitive size, so
        // each block starts on a value.
        for block in data.chunks(CANCEL_CHECK_INTERVAL) {
            if cancel.test() {
                return Ok(None);
            }

            let mut values = block.chunks_exact(T::SIZE);

            for (index, bytes) in values.by_ref().enumerate() {
                let Some(value) = T::decode::<B>(bytes) else {
                    continue;
                };

                if self.predicate.test(value) {
                    let at = offset.saturating_add(index.saturating_mul(T::SIZE));
                    Self::push(base, at, value, out)?;
                }
            }

            let rest = values.remainder().len();
            offset = offset.saturating_add(block.len());

            if rest > 0 {
                return Ok(Some(offset.saturating_sub(rest)));
            }
        }

        Ok(Some(offset))
    }

    /// Scan offsets from `offset`, reading values running past the end of
    /// `data` from the process.
    fn scan_from(
        &self,
        mut offset: usize,
        chunk: &Chunk<'_>,
        out: &mut ScanSink<'_>,
        cancel: &Token,
    ) -> anyhow::Result<()> {
        use crate::process::MemoryReader as _;

        let step = self.alignment.unwrap_or(1);
        let mut next_cancel_check = offset;

        while offset < chunk.data.len() {
            if offset >= next_cancel_check {
                if cancel.test() {
                    break;
                }

                next_cancel_check = offset.saturating_add(CANCEL_CHECK_INTERVAL);
            }

            let value = match chunk.data.get(offset..).and_then(T::decode::<B>) {
                Some(value) => Some(value),
                None => {
                    let address = match chunk.base.checked_add(offset.try_into()?) {
                        Some(address) => address,
                        None => bail!("base `{}` + offset `{}` out of range", chunk.base, offset),
                    };

                    let mut buf = [0u8; 16];

                    match buf.get_mut(..T::SIZE) {
                        Some(buf) => match chunk.handle.process.read_memory(address, buf) {
                            Ok(len) if len == buf.len() => T::decode::<B>(buf),
                            _ => None,
                        },
                        None => None,
                    }
                }
            };

            match value {
                Some(value) if self.predicate.test(value) => {
                    Self::push(chunk.base, offset, value, out)?;
                    offset = offset.saturating_add(T::SIZE);

                    if let Some(alignment) = self.alignment {
                        if let Some(rem) = offset.checked_rem(alignment).filter(|r| *r > 0) {
                            offset = offset.saturating_add(alignment.saturating_sub(rem));
                        }
                    }
                }
                _ => {
                    offset = offset.saturating_add(step);
                }
            }
        }

        Ok(())
    }
}

impl<B, T, P> Scanner for CompareScanner<B, T, P>
where
    B: byteorder::ByteOrder,
    T: Primitive,
    P: Predicate<T>,
{
    fn scan(&self, chunk: Chunk<'_>, out: &mut ScanSink<'_>, cancel: &Token) -> anyhow::Result<()> {
        let offset = if self.alignment.unwrap_or(1) == T::SIZE {
            match self.scan_packed(chunk.base, chunk.data, out, cancel)? {
                Some(offset) => offset,
                None => return Ok(()),
            }
        } else {
            0
        };

        self.scan_from(offset, &chunk, out, cancel)
    }
}

#[cfg(test)]
mod tests {
    use super::{Chunk, DefaultScanner, ScanSink, Scanner};
    use crate::{Address, Addresses, FilterExpr, PointerWidth, ProcessHandle, Token, Type, Values};

    /// Bytes holding values near the constants the tests compare against, in
    /// every width and both signs, and floats including NaN and infinities.
    fn data() -> Vec<u8> {
        let mut x = 0x2545_f491_4f6c_dd1du64;
        let mut out = Vec::new();

        for i in 0..3_000u64 {
            x ^= x << 13;
            x ^= x >> 7;
            x ^= x << 17;

            // NB: pairs of words make small 128-bit values of either sign.
            let word: u64 = match i % 12 {
                0 => x,
                2 => x % 16,
                4 => (x % 16).wrapping_neg(),
                5 => u64::MAX,
                6 => u64::from(f32::to_bits((x % 40) as f32 / 8.0 - 2.0)) << ((x % 2) * 32),
                7 => f64::to_bits((x % 40) as f64 / 8.0 - 2.0),
                8 => [
                    f64::NAN.to_bits(),
                    f64::INFINITY.to_bits(),
                    f64::NEG_INFINITY.to_bits(),
                    (-0.0f64).to_bits(),
                    u64::from(f32::NAN.to_bits()),
                    u64::from(f32::INFINITY.to_bits()) << 32,
                ][(x % 6) as usize],
                9 => x & 0x0101_0101_0101_0101,
                10 => u64::MAX - x % 8,
                _ => 0,
            };

            out.extend_from_slice(&word.to_ne_bytes());
        }

        out
    }

    /// Scan `data` (an address range of this process) in chunks of
    /// `chunk_size` with `scanner`, as an initial scan would.
    fn scan(
        handle: &ProcessHandle,
        scanner: &dyn Scanner,
        data: &[u8],
        chunk_size: usize,
        ty: Type,
    ) -> Vec<(Address, String)> {
        let mut addresses = Addresses::new(handle.process.pointer_width);
        let mut values = Values::new(ty);
        let mut hits = 0;
        let token = Token::new();

        for (index, chunk) in data.chunks(chunk_size).enumerate() {
            let base = Address::new(chunk.as_ptr() as u64);
            assert_eq!(
                base,
                Address::new((data.as_ptr() as usize + index * chunk_size) as u64)
            );

            let chunk = Chunk {
                base,
                data: chunk,
                handle,
            };

            let mut sink = ScanSink {
                addresses: &mut addresses,
                values: &mut values,
                hits: &mut hits,
            };

            scanner.scan(chunk, &mut sink, &token).unwrap();
        }

        assert_eq!(hits, addresses.len() as u64);

        addresses
            .iter_copied()
            .enumerate()
            .map(|(i, a)| (a, format!("{:?}", values.get(i))))
            .collect()
    }

    /// The compare scanner finds the same values as the generic path does
    /// without a special, for each comparison and for ranges, on signed,
    /// unsigned and floating point types, aligned and unaligned, with values
    /// straddling chunk boundaries.
    #[test]
    fn compare_scanner_matches_default_scanner() {
        let handle = ProcessHandle::open(std::process::id()).unwrap().unwrap();

        let data = data();
        // NB: start on a 16 byte boundary so aligned scans see aligned
        // addresses.
        let pad = (16 - data.as_ptr() as usize % 16) % 16;
        let data = &data[pad..data.len() - 16];

        let integers = [
            "value > 5",
            "value >= 5",
            "value < 5",
            "value <= 5",
            "value != 5",
            "value != 0",
            "5 < value",
            "5 >= value",
            "value >= 2 and value < 10",
            "value > 2 and 10 >= value",
            "value > 2 and value >= 3 and value < 9 and value <= 12",
            "value > 10 and value < 2",
            "value >= 0",
        ];

        let signed = ["value > -3", "value < -3", "value >= -8 and value <= -2"];

        let floats = [
            "value > 1.5",
            "value <= 0.75",
            "value < 0.75 and value > -1.0",
            "value != 0",
            "value != 1.5",
            "value >= 1 and value < 1.5",
            "-1.25 < value",
        ];

        let types = [
            (Type::U8, &integers[..], &[][..]),
            (Type::I8, &integers[..], &signed[..]),
            (Type::U16, &integers[..], &[][..]),
            (Type::I16, &integers[..], &signed[..]),
            (Type::U32, &integers[..], &[][..]),
            (Type::I32, &integers[..], &signed[..]),
            (Type::U64, &integers[..], &[][..]),
            (Type::I64, &integers[..], &signed[..]),
            (Type::U128, &integers[..], &[][..]),
            (Type::I128, &integers[..], &signed[..]),
            (Type::F32, &floats[..], &integers[..]),
            (Type::F64, &floats[..], &signed[..]),
            (
                Type::Pointer(PointerWidth::Pointer32),
                &integers[..],
                &[][..],
            ),
            (
                Type::Pointer(PointerWidth::Pointer64),
                &integers[..],
                &[][..],
            ),
        ];

        let mut cases = 0;

        for &(ty, a, b) in &types {
            let size = ty.size().unwrap();

            for filter in a.iter().chain(b) {
                let expr = FilterExpr::parse(filter, &handle).unwrap();
                let typed = expr
                    .type_check(&handle, Type::None, Type::None, ty)
                    .unwrap();

                // NB: half the size puts aligned values across chunk
                // boundaries.
                let alignments = [Some(size), Some(size / 2).filter(|a| *a > 1), None];

                for (index, alignment) in alignments.iter().copied().enumerate() {
                    // Skip a missing half-size alignment, but not unaligned.
                    if index == 1 && alignment.is_none() {
                        continue;
                    }

                    // NB: aligned `value != 0` has its own scanner.
                    if alignment.is_some() && *filter == "value != 0" {
                        continue;
                    }

                    let scanner = expr
                        .scanner(alignment, &handle, ty)
                        .unwrap()
                        .unwrap_or_else(|| panic!("no scanner for `{}` on {}", filter, ty));

                    let default = DefaultScanner::new(
                        typed.clone(),
                        None,
                        alignment,
                        alignment.unwrap_or(1),
                        ty,
                    );

                    // NB: chunk sizes which aren't multiples of the value
                    // size put values across chunk boundaries.
                    // Aligned scans need aligned chunks, as an initial scan
                    // gives them.
                    let chunk_sizes = match alignment {
                        Some(..) => [0x1000, 1008, 48, 16],
                        None => [0x1000, 1000, 36, 7],
                    };

                    for &chunk_size in &chunk_sizes {
                        let expected = scan(&handle, &default, data, chunk_size, ty);
                        let actual = scan(&handle, &*scanner, data, chunk_size, ty);

                        assert!(
                            !expected.is_empty() || filter.contains("> 10 and"),
                            "`{}` on {} found nothing",
                            filter,
                            ty
                        );

                        if expected != actual {
                            let first = expected
                                .iter()
                                .zip(&actual)
                                .position(|(e, a)| e != a)
                                .unwrap_or(expected.len().min(actual.len()));

                            panic!(
                                "`{}` on {}, alignment {:?}, chunks of {}: \
                                 {} expected, {} found, first difference at {}: {:?} != {:?}",
                                filter,
                                ty,
                                alignment,
                                chunk_size,
                                expected.len(),
                                actual.len(),
                                first,
                                expected.get(first),
                                actual.get(first),
                            );
                        }

                        cases += 1;
                    }
                }
            }
        }

        assert!(cases > 0);
    }
}
