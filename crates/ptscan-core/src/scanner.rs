use crate::{
    filter_expr::special::{find_first_nonzero, is_all_zeros},
    Address, Addresses, Alignment, BufferProxy, ProcessHandle, Proxy as _, Special, Test, Token,
    Type, TypedFilterExpr, Value, ValueRef, Values,
};
use anyhow::bail;
use std::{convert::TryInto as _, marker, ops::Bound};

/// Where to resume scanning after a skip found a first interesting byte
/// `skip` bytes past `offset`.
///
/// A value reaching that byte can start up to `lookbehind` bytes before it, so
/// the result is rounded *down* to the alignment, never below `offset`.
/// Rounding up would jump past a word whose first non-zero byte is not its
/// first.
fn skip_to(offset: usize, skip: usize, lookbehind: usize, alignment: Option<usize>) -> usize {
    let target = offset.saturating_add(skip.saturating_sub(lookbehind));

    let target = match alignment.and_then(|a| target.checked_rem(a)) {
        Some(rem) => target.saturating_sub(rem),
        None => target,
    };

    usize::max(target, offset)
}

/// Round `offset` up to the alignment.
fn align_up(offset: usize, alignment: Option<usize>) -> usize {
    match alignment.and_then(|a| offset.checked_rem(a).filter(|r| *r > 0).map(|r| (a, r))) {
        Some((a, r)) => offset.saturating_add(a.saturating_sub(r)),
        None => offset,
    }
}

/// Where to resume scanning `data` from `offset`, using `special`, or `None`
/// if nothing is left to scan.
///
/// When the special finds nothing, a value that starts in the last
/// `lookbehind` bytes of `data` can still reach into the next chunk, which
/// the special doesn't see, so those starts are scanned.
fn next_offset(
    special: &Special,
    data: &[u8],
    offset: usize,
    lookbehind: usize,
    alignment: Option<usize>,
) -> Option<usize> {
    let rest = data.get(offset..)?;

    match special.test(rest) {
        Some(skip) => Some(skip_to(offset, skip, lookbehind, alignment)),
        None if lookbehind == 0 => None,
        None => {
            let tail = usize::max(offset, data.len().saturating_sub(lookbehind));
            let tail = align_up(tail, alignment);
            (tail < data.len()).then_some(tail)
        }
    }
}

/// How many bytes [`DefaultScanner`] scans between checks for cancellation.
const CANCEL_CHECK_INTERVAL: usize = 0x1000;

/// Trait deciding how the scan is performed.
pub trait ScannerTrait {
    /// Find the first offset which satistifes the trait.
    fn find_trait_offset(&self, data: &[u8]) -> Option<usize>;

    /// How many bytes before the found offset an element containing it can
    /// start, for an element of `type_size` bytes.
    fn lookbehind(&self, _type_size: usize) -> usize {
        0
    }

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
    fn lookbehind(&self, type_size: usize) -> usize {
        type_size.saturating_sub(1)
    }

    #[inline(always)]
    fn test(&self, data: &[u8]) -> bool {
        // NB: the aligned element holding the found byte might not reach it
        // when the alignment is wider than the element.
        !is_all_zeros(data)
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
        let lookbehind = self.special.map(Special::lookbehind).unwrap_or_default();

        let mut offset = match self.special {
            Some(special) => match next_offset(special, data, 0, lookbehind, self.alignment) {
                Some(offset) => offset,
                None => return Ok(()),
            },
            None => 0usize,
        };

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

                    // NB: step over the value, then back onto the alignment,
                    // which is wider than the value for some types.
                    offset = align_up(offset.saturating_add(advance), self.alignment);
                } else {
                    offset = offset.saturating_add(self.step_size);
                }
            } else {
                offset = offset.saturating_add(self.step_size);
            }

            if let Some(special) = self.special {
                offset = match next_offset(special, data, offset, lookbehind, self.alignment) {
                    Some(offset) => offset,
                    None => return Ok(()),
                };
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
            let Some(rest) = data.get(offset..) else {
                break;
            };

            let lookbehind = self.scanner_trait.lookbehind(self.type_size);

            match self.scanner_trait.find_trait_offset(rest) {
                Some(o) => {
                    offset = offset.saturating_add(o.saturating_sub(lookbehind));
                    self.alignment.align(&mut offset);
                }
                None if lookbehind == 0 => break,
                // NB: an element starting in the last `lookbehind` bytes can
                // reach into the next chunk, which isn't searched.
                None => {
                    let tail = data.len().saturating_sub(lookbehind);
                    offset = self.alignment.align_up(usize::max(offset, tail));
                }
            }

            // The element at or past the end belongs to the next chunk.
            let d = match data.get(offset..) {
                Some(d) if !d.is_empty() => d,
                _ => break,
            };

            let mut tmp;

            // special case: buffer doesn't contain enough data, so we need to
            // read using the handle.
            let (d, address) = match d.get(..self.type_size) {
                Some(d) => (d, None),
                None => {
                    tmp = vec![0u8; self.type_size];

                    let address = match base.checked_add(offset.try_into()?) {
                        Some(address) => address,
                        None => break,
                    };

                    // NB: an unreadable element isn't one.
                    match handle.process.read_process_memory(address, &mut tmp) {
                        Ok(read) if read == self.type_size => (),
                        _ => break,
                    }

                    (&tmp[..], Some(address))
                }
            };

            if !self.scanner_trait.test(d) {
                offset = offset.saturating_add(self.alignment.step());
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

            // NB: a hit is stepped over, as the generic scan does.
            offset = self.alignment.advance(offset, self.type_size);
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
    use crate::{
        Address, Addresses, FilterExpr, PointerWidth, ProcessHandle, Special, Token, Type, Values,
    };

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

    /// Words that are zero, or non-zero only in a byte other than the first,
    /// so that a skip to the first non-zero byte lands inside a word.
    fn sparse_data() -> Vec<u8> {
        let mut x = 0x9e37_79b9_7f4a_7c15u64;
        let mut out = Vec::new();

        for _ in 0..2_000 {
            x ^= x << 13;
            x ^= x >> 7;
            x ^= x << 17;

            let word: u64 = match x % 8 {
                0 => 0x1234_5600,
                1 => 0x100,
                2 => 0xff00_0000_0000_0000,
                3 => x << 8,
                4 => x,
                _ => 0,
            };

            out.extend_from_slice(&word.to_ne_bytes());
        }

        out.extend_from_slice(&[0u8; 16]);
        out
    }

    /// A skip to the first non-zero byte must not jump past an aligned word
    /// whose low byte is zero, for the default scanner with a special and for
    /// the aligned `value != 0` scanner, including alignments narrower and
    /// wider than the value.
    #[test]
    fn nonzero_skip_keeps_aligned_words() {
        let handle = ProcessHandle::open(std::process::id()).unwrap().unwrap();

        let data = sparse_data();
        let pad = (16 - data.as_ptr() as usize % 16) % 16;
        let data = &data[pad..data.len() - 16];
        let base = data.as_ptr() as u64;

        for (ty, alignments) in [
            (Type::U64, &[8usize, 16, 4, 2][..]),
            (Type::U32, &[4, 8, 2][..]),
            (Type::U16, &[2, 4, 8][..]),
            (Type::Pointer(PointerWidth::Pointer64), &[8, 16, 4][..]),
        ] {
            let size = ty.size().unwrap();
            let expr = FilterExpr::parse("value != 0", &handle).unwrap();
            let typed = expr
                .type_check(&handle, Type::None, Type::None, ty)
                .unwrap();

            let special = Special::NonZero(Some(size));

            for &alignment in alignments {
                let skipping = DefaultScanner::new(
                    typed.clone(),
                    Some(&special),
                    Some(alignment),
                    alignment,
                    ty,
                );
                let bytes = expr.scanner(Some(alignment), &handle, ty).unwrap().unwrap();

                let scanners = [
                    ("default scanner with special", &skipping as &dyn Scanner),
                    ("aligned scanner", &*bytes),
                ];

                // NB: values overlapping a hit are skipped, which a chunk
                // boundary resets, so scan narrow alignments as one chunk.
                let chunk_sizes = if alignment >= size {
                    vec![0x1000, 1008, 48, 16]
                } else {
                    vec![data.len()]
                };

                // The aligned values with a non-zero byte, which the scanners
                // find, except those overlapping a hit when the alignment is
                // narrower than the value.
                let expected = (0..data.len().saturating_sub(size) + 1)
                    .step_by(alignment)
                    .filter(|a| data[*a..a.saturating_add(size)].iter().any(|b| *b != 0))
                    .map(|a| Address::new(base + a as u64))
                    .collect::<Vec<_>>();

                assert!(!expected.is_empty());

                for chunk_size in chunk_sizes {
                    for &(name, scanner) in &scanners {
                        let actual = scan(&handle, scanner, data, chunk_size, ty)
                            .into_iter()
                            .map(|(a, _)| a)
                            .collect::<Vec<_>>();

                        // Without overlap, hits are at least `size` apart.
                        let expected = if alignment >= size {
                            expected.clone()
                        } else {
                            let mut next = 0;

                            expected
                                .iter()
                                .copied()
                                .filter(|a| {
                                    if a.0 < next {
                                        return false;
                                    }

                                    next = a.0 + size as u64;
                                    true
                                })
                                .collect()
                        };

                        let first = expected
                            .iter()
                            .zip(&actual)
                            .position(|(e, a)| e != a)
                            .unwrap_or(expected.len().min(actual.len()));

                        assert!(
                            expected == actual,
                            "{} on {}, alignment {}, chunks of {}: {} expected, {} found, \
                             first difference at {}: {:?} != {:?}",
                            name,
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
                }
            }
        }
    }

    /// A value starting in the last bytes of a chunk, with all its non-zero
    /// bytes in the next chunk, is found from the chunk it starts in, and the
    /// scanners that skip find what the generic path without a special does,
    /// whatever the alignment and chunk size.
    #[test]
    fn nonzero_skip_finds_values_straddling_chunks() {
        let handle = ProcessHandle::open(std::process::id()).unwrap().unwrap();

        let data = sparse_data();
        let pad = (16 - data.as_ptr() as usize % 16) % 16;
        let data = &data[pad..data.len() - 16];

        // A single value, whose non-zero bytes start at the next chunk.
        let mut single = [0u8; 64 + 16];
        let pad = (16 - single.as_ptr() as usize % 16) % 16;
        single[pad + 16] = 1;
        let single = &single[pad..pad + 64];

        for (ty, alignments) in [
            (Type::U64, &[8usize, 4, 2][..]),
            (Type::U32, &[4, 2, 8][..]),
            (Type::U16, &[2, 4][..]),
            (Type::Pointer(PointerWidth::Pointer64), &[8, 4][..]),
            (Type::Pointer(PointerWidth::Pointer32), &[4, 2][..]),
        ] {
            let size = ty.size().unwrap();
            let expr = FilterExpr::parse("value != 0", &handle).unwrap();
            let typed = expr
                .type_check(&handle, Type::None, Type::None, ty)
                .unwrap();

            let special = Special::NonZero(Some(size));

            for &alignment in alignments {
                let generic =
                    DefaultScanner::new(typed.clone(), None, Some(alignment), alignment, ty);
                let skipping = DefaultScanner::new(
                    typed.clone(),
                    Some(&special),
                    Some(alignment),
                    alignment,
                    ty,
                );
                let bytes = expr.scanner(Some(alignment), &handle, ty).unwrap();

                let mut scanners =
                    vec![("default scanner with special", &skipping as &dyn Scanner)];

                if let Some(bytes) = bytes.as_deref() {
                    scanners.push(("aligned scanner", bytes));
                }

                for (name, scanner) in scanners {
                    for chunk_size in [16, 48, 20, 36, 1008, 4096] {
                        if chunk_size % alignment != 0 {
                            continue;
                        }

                        for data in [data, single] {
                            let expected = scan(&handle, &generic, data, chunk_size, ty);
                            let actual = scan(&handle, scanner, data, chunk_size, ty);

                            assert!(
                                expected == actual,
                                "{} on {}, alignment {}, chunks of {}, {} bytes: \
                                 {} expected, {} found",
                                name,
                                ty,
                                alignment,
                                chunk_size,
                                data.len(),
                                expected.len(),
                                actual.len(),
                            );

                            assert!(!expected.is_empty());
                        }
                    }
                }
            }
        }
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
