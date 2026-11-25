//! Block-compressed, strictly increasing address storage.
//!
//! The address space is cut into 64 KiB blocks keyed by `address >> 16`. Each
//! non-empty block has a 16 byte [`Header`] and stores its addresses in
//! whichever of two encodings is smaller:
//!
//! * [`Encoding::Offsets`]: one little-endian `u16` slot index per address.
//! * [`Encoding::Bitmap`]: one bit per slot in the block.
//!
//! A slot is `step` bytes wide, where `step` is the largest power of two
//! dividing the scan's step size (capped at the block size), so every address
//! of an aligned scan falls on a slot.
//!
//! Hot paths should walk [`BlockAddresses::blocks`]; [`BlockAddresses::get`]
//! is a binary search plus select and is meant for paging.

use std::{convert::TryFrom, fmt, iter::FusedIterator, slice};

use crate::Address;

/// Log2 of the block size.
const BLOCK_SHIFT: u32 = 16;
/// Mask for the offset of an address inside of its block.
const BLOCK_MASK: u64 = (1 << BLOCK_SHIFT) - 1;
/// Marks a header as bitmap-encoded. Block keys use at most 48 bits.
const BITMAP_FLAG: u64 = 1 << 63;

/// Errors raised when building or appending to [`BlockAddresses`].
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum BlockError {
    #[error("step must be non-zero")]
    ZeroStep,
    #[error("address {address} is not after {previous}")]
    Unsorted { previous: Address, address: Address },
    #[error("address {address} is not aligned to step {step}")]
    Misaligned { address: Address, step: u64 },
    #[error("cannot combine address sets with steps {0} and {1}")]
    StepMismatch(u64, u64),
    #[error("address set is too large to encode")]
    Overflow,
    #[error("address set is corrupt")]
    Corrupt,
}

/// The encoding of a single block.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Encoding {
    /// One `u16` slot index per address.
    Offsets,
    /// One bit per slot.
    Bitmap,
}

/// A 16 byte block header.
///
/// The number of addresses in a block is not stored: it is the difference
/// between the rank of the next block (or the total length) and its own.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
struct Header {
    /// The block key (`address >> 16`), with [`BITMAP_FLAG`] for bitmaps.
    key: u64,
    /// Index of the first address in the block.
    rank: u32,
    /// Offset of the block's data in the shared data buffer.
    data: u32,
}

impl Header {
    #[inline]
    fn block(&self) -> u64 {
        self.key & !BITMAP_FLAG
    }

    #[inline]
    fn encoding(&self) -> Encoding {
        if self.key & BITMAP_FLAG != 0 {
            Encoding::Bitmap
        } else {
            Encoding::Offsets
        }
    }
}

/// Strictly increasing addresses, compressed in 64 KiB blocks.
#[derive(Clone, PartialEq, Eq)]
pub struct BlockAddresses {
    step: u64,
    shift: u32,
    len: usize,
    headers: Vec<Header>,
    data: Vec<u8>,
}

impl BlockAddresses {
    /// Construct an empty set for the given scan step.
    pub fn new(step: u64) -> Result<Self, BlockError> {
        Ok(BlockAddressesBuilder::new(step)?.finish_unchecked())
    }

    /// Build a set from strictly increasing addresses.
    pub fn from_sorted<I>(step: u64, iter: I) -> Result<Self, BlockError>
    where
        I: IntoIterator<Item = Address>,
    {
        let mut builder = BlockAddressesBuilder::new(step)?;

        for address in iter {
            builder.push(address)?;
        }

        builder.finish()
    }

    /// The scan step this set was built for.
    #[inline]
    pub fn step(&self) -> u64 {
        self.step
    }

    /// The number of addresses.
    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    /// Test if there are no addresses.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// The number of bytes used by headers and data.
    pub fn bytes(&self) -> usize {
        self.headers
            .len()
            .saturating_mul(std::mem::size_of::<Header>())
            .saturating_add(self.data.len())
    }

    /// Get the address at index `index`.
    ///
    /// This is a binary search over blocks plus a select inside of one, so it
    /// is meant for paging. Hot paths should use [`blocks`](Self::blocks).
    pub fn get(&self, index: usize) -> Option<Address> {
        let (block, k) = self.locate(index)?;
        self.block(block)?.get(k)
    }

    /// The first address.
    pub fn first(&self) -> Option<Address> {
        self.block(0)?.get(0)
    }

    /// The last address.
    pub fn last(&self) -> Option<Address> {
        let block = self.block(self.headers.len().checked_sub(1)?)?;
        block.get(block.len().checked_sub(1)?)
    }

    /// The number of addresses below `address`, which is the index of the
    /// first address at or after it.
    ///
    /// A binary search over blocks plus a scan of at most one block.
    pub fn lower_bound(&self, address: Address) -> usize {
        let key = address.0 >> BLOCK_SHIFT;
        let block = self.headers.partition_point(|h| h.block() < key);

        let Some(b) = self.block(block) else {
            return self.len;
        };

        let below = b.iter().take_while(|a| *a < address).count();
        b.rank().saturating_add(below)
    }

    /// The index of `address`, if it is in the set.
    pub fn index_of(&self, address: Address) -> Option<usize> {
        let index = self.lower_bound(address);
        (self.get(index)? == address).then_some(index)
    }

    /// Iterate over all addresses.
    pub fn iter(&self) -> Iter<'_> {
        self.iter_from(0)
    }

    /// Iterate over addresses starting at index `index`.
    ///
    /// The iterator is empty if `index` is out of bounds.
    pub fn iter_from(&self, index: usize) -> Iter<'_> {
        let empty = Iter {
            addresses: self,
            next_block: self.headers.len(),
            current: BlockIter::empty(),
            remaining: 0,
        };

        let Some((block_index, k)) = self.locate(index) else {
            return empty;
        };

        let Some(block) = self.block(block_index) else {
            return empty;
        };

        Iter {
            addresses: self,
            next_block: block_index.saturating_add(1),
            current: block.iter_from(k),
            remaining: self.len.saturating_sub(index),
        }
    }

    /// Iterate over blocks in address order.
    pub fn blocks(&self) -> Blocks<'_> {
        Blocks {
            addresses: self,
            index: 0,
        }
    }

    /// Keep only the addresses whose index `keep` returns `true` for, in one
    /// streaming pass that re-encodes every block.
    ///
    /// `keep` is called once per index, in increasing order.
    pub fn retain_by_index<F>(&mut self, mut keep: F) -> Result<(), BlockError>
    where
        F: FnMut(usize) -> bool,
    {
        let mut builder = BlockAddressesBuilder::new(self.step)?;

        // Blocks are already sorted and distinct, so kept slots go straight
        // into the pending block without re-validating every address.
        for block in self.blocks() {
            builder.pending_block = block.base >> BLOCK_SHIFT;

            for (k, slot) in block.slots().enumerate() {
                let index = block.rank.checked_add(k).ok_or(BlockError::Overflow)?;

                if keep(index) {
                    let slot = u16::try_from(slot.0).map_err(|_| BlockError::Corrupt)?;
                    builder.pending.push(slot);
                }
            }

            builder.flush()?;
        }

        *self = builder.finish()?;
        Ok(())
    }

    /// Find the block holding `index` and the position inside of it.
    fn locate(&self, index: usize) -> Option<(usize, usize)> {
        if index >= self.len {
            return None;
        }

        let index = u64::try_from(index).ok()?;
        let block = self
            .headers
            .partition_point(|h| u64::from(h.rank) <= index)
            .checked_sub(1)?;
        let header = self.headers.get(block)?;
        let k = index.checked_sub(u64::from(header.rank))?;
        Some((block, usize::try_from(k).ok()?))
    }

    /// The number of addresses in block `index`.
    fn block_len(&self, index: usize) -> Option<usize> {
        let start = usize::try_from(self.headers.get(index)?.rank).ok()?;

        let end = match self.headers.get(index.checked_add(1)?) {
            Some(next) => usize::try_from(next.rank).ok()?,
            None => self.len,
        };

        end.checked_sub(start)
    }

    /// Access block `index`.
    fn block(&self, index: usize) -> Option<Block<'_>> {
        let header = self.headers.get(index)?;
        let len = self.block_len(index)?;
        let encoding = header.encoding();
        let size = data_size(encoding, len, self.shift)?;
        let start = usize::try_from(header.data).ok()?;
        let data = self.data.get(start..start.checked_add(size)?)?;

        Some(Block {
            base: header.block().checked_shl(BLOCK_SHIFT)?,
            rank: usize::try_from(header.rank).ok()?,
            len,
            encoding,
            shift: self.shift,
            data,
        })
    }
}

impl fmt::Debug for BlockAddresses {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BlockAddresses")
            .field("step", &self.step)
            .field("len", &self.len)
            .field("blocks", &self.headers.len())
            .field("bytes", &self.bytes())
            .finish()
    }
}

impl<'a> IntoIterator for &'a BlockAddresses {
    type Item = Address;
    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Builds [`BlockAddresses`] from strictly increasing addresses, or from
/// address sets concatenated in order.
pub struct BlockAddressesBuilder {
    step: u64,
    shift: u32,
    len: usize,
    headers: Vec<Header>,
    data: Vec<u8>,
    /// The block key of the addresses in `pending`.
    pending_block: u64,
    /// Slot indexes of the block being built.
    pending: Vec<u16>,
    last: Option<Address>,
}

impl BlockAddressesBuilder {
    /// Construct a builder for the given scan step.
    pub fn new(step: u64) -> Result<Self, BlockError> {
        if step == 0 {
            return Err(BlockError::ZeroStep);
        }

        Ok(Self {
            step,
            shift: step.trailing_zeros().min(BLOCK_SHIFT),
            len: 0,
            headers: Vec::new(),
            data: Vec::new(),
            pending_block: 0,
            pending: Vec::new(),
            last: None,
        })
    }

    /// The number of addresses pushed so far.
    pub fn len(&self) -> usize {
        self.len.saturating_add(self.pending.len())
    }

    /// Test if nothing has been pushed.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Push an address, which must be after every address pushed so far.
    pub fn push(&mut self, address: Address) -> Result<(), BlockError> {
        let value = address.0;

        if let Some(previous) = self.last {
            if address <= previous {
                return Err(BlockError::Unsorted { previous, address });
            }
        }

        if value & ((1u64 << self.shift) - 1) != 0 {
            return Err(BlockError::Misaligned {
                address,
                step: self.step,
            });
        }

        let block = value >> BLOCK_SHIFT;

        if block != self.pending_block && !self.pending.is_empty() {
            self.flush()?;
        }

        // After `append` copied blocks, the last block is encoded already.
        if self.pending.is_empty() {
            self.reopen_last(block)?;
        }

        let slot =
            u16::try_from((value & BLOCK_MASK) >> self.shift).map_err(|_| BlockError::Overflow)?;
        self.pending_block = block;
        self.pending.push(slot);
        self.last = Some(address);
        Ok(())
    }

    /// Append every address of `other`, whose first address must be after
    /// every address pushed so far. A block shared with the last pushed
    /// address is merged; every other block is copied without re-encoding.
    pub fn append(&mut self, other: &BlockAddresses) -> Result<(), BlockError> {
        if other.step != self.step {
            return Err(BlockError::StepMismatch(self.step, other.step));
        }

        let Some(first) = other.first() else {
            return Ok(());
        };

        if let Some(previous) = self.last {
            if first <= previous {
                return Err(BlockError::Unsorted {
                    previous,
                    address: first,
                });
            }
        }

        let mut start = 0;

        if self.pending.is_empty() {
            self.reopen_last(first.0 >> BLOCK_SHIFT)?;
        }

        if !self.pending.is_empty() && first.0 >> BLOCK_SHIFT == self.pending_block {
            let block = other.block(0).ok_or(BlockError::Corrupt)?;

            for address in block.iter() {
                self.push(address)?;
            }

            start = 1;
        }

        let Some(head) = other.headers.get(start) else {
            return Ok(());
        };

        self.flush()?;

        let rank_base = head.rank;
        let data_base = head.data;
        let rank_offset = u64::try_from(self.len).map_err(|_| BlockError::Overflow)?;
        let data_offset = u64::try_from(self.data.len()).map_err(|_| BlockError::Overflow)?;

        let tail = other.headers.get(start..).ok_or(BlockError::Corrupt)?;
        self.headers.reserve(tail.len());

        for h in tail {
            let rank = u64::from(h.rank.checked_sub(rank_base).ok_or(BlockError::Corrupt)?)
                .checked_add(rank_offset)
                .and_then(|r| u32::try_from(r).ok())
                .ok_or(BlockError::Overflow)?;

            let data = u64::from(h.data.checked_sub(data_base).ok_or(BlockError::Corrupt)?)
                .checked_add(data_offset)
                .and_then(|d| u32::try_from(d).ok())
                .ok_or(BlockError::Overflow)?;

            self.headers.push(Header {
                key: h.key,
                rank,
                data,
            });
        }

        let data_base = usize::try_from(data_base).map_err(|_| BlockError::Corrupt)?;
        let bytes = other.data.get(data_base..).ok_or(BlockError::Corrupt)?;
        self.data.extend_from_slice(bytes);

        let copied = usize::try_from(rank_base)
            .ok()
            .and_then(|r| other.len.checked_sub(r))
            .ok_or(BlockError::Corrupt)?;
        self.len = self.len.checked_add(copied).ok_or(BlockError::Overflow)?;
        self.last = other.last();
        Ok(())
    }

    /// Finish building.
    pub fn finish(mut self) -> Result<BlockAddresses, BlockError> {
        self.flush()?;
        Ok(self.finish_unchecked())
    }

    fn finish_unchecked(self) -> BlockAddresses {
        debug_assert!(self.pending.is_empty());
        debug_assert!(self.headers.windows(2).all(|w| match w {
            [a, b] => a.block() < b.block() && a.rank < b.rank && a.data < b.data,
            _ => true,
        }));

        BlockAddresses {
            step: self.step,
            shift: self.shift,
            len: self.len,
            headers: self.headers,
            data: self.data,
        }
    }

    /// If the last encoded block is `block`, decode it back into `pending` so
    /// that more addresses can be merged into it.
    fn reopen_last(&mut self, block: u64) -> Result<(), BlockError> {
        let Some(header) = self.headers.last().copied() else {
            return Ok(());
        };

        if header.block() != block {
            return Ok(());
        }

        let rank = usize::try_from(header.rank).map_err(|_| BlockError::Corrupt)?;
        let len = self.len.checked_sub(rank).ok_or(BlockError::Corrupt)?;
        let start = usize::try_from(header.data).map_err(|_| BlockError::Corrupt)?;
        let size = data_size(header.encoding(), len, self.shift).ok_or(BlockError::Corrupt)?;
        let data = self
            .data
            .get(start..start.checked_add(size).ok_or(BlockError::Corrupt)?)
            .ok_or(BlockError::Corrupt)?;

        let decoded = Block {
            base: 0,
            rank,
            len,
            encoding: header.encoding(),
            shift: 0,
            data,
        };

        self.pending.clear();

        for slot in decoded.iter() {
            let slot = u16::try_from(slot.0).map_err(|_| BlockError::Corrupt)?;
            self.pending.push(slot);
        }

        self.pending_block = block;
        self.headers.pop();
        self.data.truncate(start);
        self.len = rank;
        Ok(())
    }

    /// Encode the pending block.
    fn flush(&mut self) -> Result<(), BlockError> {
        if self.pending.is_empty() {
            return Ok(());
        }

        let count = self.pending.len();
        let rank = u32::try_from(self.len).map_err(|_| BlockError::Overflow)?;
        let data = u32::try_from(self.data.len()).map_err(|_| BlockError::Overflow)?;

        let offsets =
            data_size(Encoding::Offsets, count, self.shift).ok_or(BlockError::Overflow)?;
        let bitmap = data_size(Encoding::Bitmap, count, self.shift).ok_or(BlockError::Overflow)?;

        let key = if bitmap < offsets {
            let start = self.data.len();
            let end = start.checked_add(bitmap).ok_or(BlockError::Overflow)?;
            self.data.resize(end, 0);
            let bits = self.data.get_mut(start..end).ok_or(BlockError::Overflow)?;

            for &slot in &self.pending {
                let slot = usize::from(slot);
                let byte = bits.get_mut(slot / 8).ok_or(BlockError::Overflow)?;
                *byte |= 1 << (slot % 8);
            }

            self.pending_block | BITMAP_FLAG
        } else {
            self.data.reserve(offsets);

            for &slot in &self.pending {
                self.data.extend_from_slice(&slot.to_le_bytes());
            }

            self.pending_block
        };

        self.headers.push(Header { key, rank, data });
        self.len = self.len.checked_add(count).ok_or(BlockError::Overflow)?;
        self.pending.clear();
        Ok(())
    }
}

/// The size in bytes of a block's data.
fn data_size(encoding: Encoding, len: usize, shift: u32) -> Option<usize> {
    match encoding {
        Encoding::Offsets => len.checked_mul(2),
        Encoding::Bitmap => {
            let slots = 1usize.checked_shl(BLOCK_SHIFT.checked_sub(shift)?)?;
            slots.div_ceil(64).checked_mul(8)
        }
    }
}

/// Read the `index`th 64-bit word of a bitmap.
#[inline]
fn word(data: &[u8], index: usize) -> u64 {
    let start = index.saturating_mul(8);

    match data.get(start..start.saturating_add(8)) {
        Some(&[a, b, c, d, e, f, g, h]) => u64::from_le_bytes([a, b, c, d, e, f, g, h]),
        _ => 0,
    }
}

/// Clear the `k` lowest set bits of `word`.
#[inline]
fn clear_lowest(mut word: u64, k: usize) -> u64 {
    for _ in 0..k {
        word &= word.wrapping_sub(1);
    }

    word
}

/// One non-empty 64 KiB block.
#[derive(Debug, Clone, Copy)]
pub struct Block<'a> {
    base: u64,
    rank: usize,
    len: usize,
    encoding: Encoding,
    shift: u32,
    data: &'a [u8],
}

impl<'a> Block<'a> {
    /// The address of the start of the block.
    #[inline]
    pub fn base(&self) -> Address {
        Address(self.base)
    }

    /// The index of the block's first address in the whole set.
    #[inline]
    pub fn rank(&self) -> usize {
        self.rank
    }

    /// The number of addresses in the block.
    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    /// Blocks are never empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// The block's encoding.
    #[inline]
    pub fn encoding(&self) -> Encoding {
        self.encoding
    }

    /// The `k`th address of the block.
    pub fn get(&self, k: usize) -> Option<Address> {
        if k >= self.len {
            return None;
        }

        self.iter_from(k).next()
    }

    /// Iterate over the block's addresses.
    pub fn iter(&self) -> BlockIter<'a> {
        self.iter_from(0)
    }

    /// Iterate over the block's slot indexes, as addresses relative to the
    /// block in units of slots.
    fn slots(&self) -> BlockIter<'a> {
        BlockIter {
            base: 0,
            shift: 0,
            ..self.iter()
        }
    }

    /// Iterate over the block's addresses from its `k`th address.
    pub fn iter_from(&self, k: usize) -> BlockIter<'a> {
        let remaining = self.len.saturating_sub(k);

        let state = match self.encoding {
            Encoding::Offsets => {
                let data = self.data.get(k.saturating_mul(2)..).unwrap_or_default();
                State::Offsets(data.as_chunks::<2>().0.iter())
            }
            Encoding::Bitmap => {
                let words = self.data.len() / 8;
                let mut skip = k;
                let mut index = 0;
                let mut current = 0;

                while index < words {
                    let w = word(self.data, index);
                    let ones = w.count_ones() as usize;

                    if skip < ones {
                        current = clear_lowest(w, skip);
                        break;
                    }

                    skip -= ones;
                    index += 1;
                }

                State::Bitmap {
                    data: self.data,
                    index,
                    current,
                }
            }
        };

        BlockIter {
            base: self.base,
            shift: self.shift,
            remaining,
            state,
        }
    }
}

#[derive(Clone)]
enum State<'a> {
    Offsets(slice::Iter<'a, [u8; 2]>),
    Bitmap {
        data: &'a [u8],
        /// Index of the word in `current`.
        index: usize,
        /// Remaining bits of word `index`.
        current: u64,
    },
}

/// Iterator over the addresses of one [`Block`].
#[derive(Clone)]
pub struct BlockIter<'a> {
    base: u64,
    shift: u32,
    remaining: usize,
    state: State<'a>,
}

impl BlockIter<'_> {
    fn empty() -> Self {
        Self {
            base: 0,
            shift: 0,
            remaining: 0,
            state: State::Offsets([].iter()),
        }
    }
}

impl Iterator for BlockIter<'_> {
    type Item = Address;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            return None;
        }

        let slot = match &mut self.state {
            State::Offsets(chunks) => u64::from(u16::from_le_bytes(*chunks.next()?)),
            State::Bitmap {
                data,
                index,
                current,
            } => {
                let words = data.len() / 8;

                while *current == 0 {
                    *index = index.checked_add(1)?;

                    if *index >= words {
                        return None;
                    }

                    *current = word(data, *index);
                }

                let bit = u64::from(current.trailing_zeros());
                *current &= current.wrapping_sub(1);
                u64::try_from(*index)
                    .ok()?
                    .checked_mul(64)?
                    .checked_add(bit)?
            }
        };

        self.remaining -= 1;
        Some(Address(
            self.base.checked_add(slot.checked_shl(self.shift)?)?,
        ))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl ExactSizeIterator for BlockIter<'_> {}
impl FusedIterator for BlockIter<'_> {}

/// Iterator over the blocks of a [`BlockAddresses`].
#[derive(Clone)]
pub struct Blocks<'a> {
    addresses: &'a BlockAddresses,
    index: usize,
}

impl<'a> Iterator for Blocks<'a> {
    type Item = Block<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let block = self.addresses.block(self.index)?;
        self.index = self.index.saturating_add(1);
        Some(block)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.addresses.headers.len().saturating_sub(self.index);
        (n, Some(n))
    }
}

impl ExactSizeIterator for Blocks<'_> {}

/// Iterator over the addresses of a [`BlockAddresses`].
#[derive(Clone)]
pub struct Iter<'a> {
    addresses: &'a BlockAddresses,
    next_block: usize,
    current: BlockIter<'a>,
    remaining: usize,
}

impl Iterator for Iter<'_> {
    type Item = Address;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(address) = self.current.next() {
                self.remaining = self.remaining.saturating_sub(1);
                return Some(address);
            }

            let block = self.addresses.block(self.next_block)?;
            self.next_block = self.next_block.saturating_add(1);
            self.current = block.iter();
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl ExactSizeIterator for Iter<'_> {}
impl FusedIterator for Iter<'_> {}

#[cfg(test)]
mod tests;
