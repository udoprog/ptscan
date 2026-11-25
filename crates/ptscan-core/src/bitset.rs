//! A simple, stripped down bitset implementation.

#[derive(Clone)]
#[repr(transparent)]
pub struct BitSet {
    slots: Vec<u64>,
}

impl Default for BitSet {
    fn default() -> Self {
        Self::new()
    }
}

impl BitSet {
    const BITS_IN_SLOT: usize = std::mem::size_of::<u64>() * 8;

    /// Construct a new bitset.
    pub fn new() -> Self {
        Self { slots: Vec::new() }
    }

    /// Construct a bitset with room for `bits` bits, all of them clear.
    pub fn with_capacity(bits: usize) -> Self {
        Self {
            slots: vec![0; bits.div_ceil(Self::BITS_IN_SLOT)],
        }
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

        if self.slots.len() <= slot {
            self.slots.resize(slot.saturating_add(1), 0);
        }

        if let Some(slot) = self.slots.get_mut(slot) {
            *slot |= 1u64 << offset;
        }
    }

    /// Clear the given bit.
    pub fn clear(&mut self, index: usize) {
        let slot = index / Self::BITS_IN_SLOT;
        let offset = index % Self::BITS_IN_SLOT;

        if let Some(slot) = self.slots.get_mut(slot) {
            *slot &= !(1u64 << offset);
        }
    }

    /// Whether no bit is set.
    pub fn is_empty(&self) -> bool {
        self.slots.iter().all(|&slot| slot == 0)
    }

    /// The number of bits set.
    pub fn count(&self) -> usize {
        self.slots.iter().fold(0usize, |n, slot| {
            n.saturating_add(slot.count_ones() as usize)
        })
    }

    /// The number of bits set below `end`.
    pub fn count_below(&self, end: usize) -> usize {
        let full = end / Self::BITS_IN_SLOT;
        let rest = end % Self::BITS_IN_SLOT;

        let n = self.slots.iter().take(full).fold(0usize, |n, slot| {
            n.saturating_add(slot.count_ones() as usize)
        });

        match self.slots.get(full) {
            Some(slot) if rest > 0 => {
                n.saturating_add((slot & ((1u64 << rest) - 1)).count_ones() as usize)
            }
            _ => n,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::BitSet;

    #[test]
    fn set_test_clear() {
        let mut set = BitSet::new();
        assert!(set.is_empty());

        // Indices at, below and above slot boundaries, set out of order.
        for index in [64, 0, 63, 200, 1] {
            set.set(index);
        }

        for index in 0..256 {
            assert_eq!(set.test(index), [0, 1, 63, 64, 200].contains(&index));
        }

        assert_eq!(set.count(), 5);
        assert_eq!(set.count_below(0), 0);
        assert_eq!(set.count_below(1), 1);
        assert_eq!(set.count_below(63), 2);
        assert_eq!(set.count_below(64), 3);
        assert_eq!(set.count_below(65), 4);
        assert_eq!(set.count_below(201), 5);
        assert_eq!(set.count_below(10_000), 5);

        set.clear(64);
        set.clear(1000);
        assert!(!set.test(64));
        assert_eq!(set.count(), 4);

        for index in [0, 1, 63, 200] {
            set.clear(index);
        }

        assert!(set.is_empty());
    }

    #[test]
    fn with_capacity_is_clear() {
        let mut set = BitSet::with_capacity(130);
        assert!(set.is_empty());
        assert!(!set.test(129));
        set.set(129);
        assert!(set.test(129));
        assert_eq!(set.count(), 1);
    }
}
