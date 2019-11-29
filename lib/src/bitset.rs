//! A simple, stripped down bitset implementation.

#[derive(Clone)]
#[repr(transparent)]
pub struct BitSet {
    slots: Vec<u64>,
}

impl BitSet {
    const BITS_IN_SLOT: usize = std::mem::size_of::<u64>() * 8;

    /// Construct a new bitset.
    pub fn new() -> Self {
        Self { slots: Vec::new() }
    }

    /// Test if the given bit is set.
    pub fn test(&self, index: usize) -> bool {
        let slot = index / Self::BITS_IN_SLOT;

        if let Some(slot) = self.slots.get(slot) {
            let o = index % Self::BITS_IN_SLOT;
            1 & (slot >> o) == 1
        } else {
            false
        }
    }

    /// Set the given bit.
    pub fn set(&mut self, index: usize) {
        let slot = index / Self::BITS_IN_SLOT;
        let offset = index % Self::BITS_IN_SLOT;

        if self.slots.len() < slot {
            self.slots
                .extend(std::iter::repeat(0u64).take(slot - self.slots.len()));
        }

        self.slots[slot] |= 1u64 << offset;
    }

    /// Clear the given bit.
    pub fn clear(&mut self, index: usize) {
        let slot = index / Self::BITS_IN_SLOT;
        let offset = index % Self::BITS_IN_SLOT;

        // NB: not set.
        if self.slots.len() < slot {
            return;
        }

        self.slots[slot] &= !(1u64 << offset);
    }
}
