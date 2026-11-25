pub trait Alignment: 'static + Copy + Send + Sync {
    /// Align the given offset.
    fn align(self, offset: &mut usize);

    /// Get the step for the current alignment.
    fn step(self) -> usize;

    /// Round the given offset up to the alignment.
    fn align_up(self, offset: usize) -> usize;

    /// Move past a hit of `size` bytes at `offset`, to the first aligned
    /// offset at or after its end, as the generic scan does.
    fn advance(self, offset: usize, size: usize) -> usize;
}

#[derive(Clone, Copy)]
pub struct Aligned(pub usize);

impl Alignment for Aligned {
    fn align(self, offset: &mut usize) {
        if let Some(d) = offset.checked_rem(self.0).filter(|d| *d > 0) {
            *offset = offset.saturating_sub(d);
        }
    }

    fn step(self) -> usize {
        self.0
    }

    fn align_up(self, offset: usize) -> usize {
        match offset.checked_rem(self.0).filter(|d| *d > 0) {
            Some(d) => offset.saturating_add(self.0.saturating_sub(d)),
            None => offset,
        }
    }

    fn advance(self, offset: usize, size: usize) -> usize {
        // NB: a hit is at least a byte wide.
        self.align_up(offset.saturating_add(size.max(1)))
    }
}

#[derive(Clone, Copy)]
pub struct Unaligned;

impl Alignment for Unaligned {
    fn align(self, _: &mut usize) {}

    fn step(self) -> usize {
        1
    }

    fn align_up(self, offset: usize) -> usize {
        offset
    }

    fn advance(self, offset: usize, size: usize) -> usize {
        offset.saturating_add(size.max(1))
    }
}
