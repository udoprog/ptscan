pub trait Alignment: 'static + Copy + Send + Sync {
    /// Align the given offset.
    fn align(self, offset: &mut usize);

    /// Get the step for the current alignment.
    fn step(self) -> usize;
}

#[derive(Clone, Copy)]
pub struct Aligned(pub usize);

impl Alignment for Aligned {
    fn align(self, offset: &mut usize) {
        let d = *offset % self.0;

        if d > 0 {
            *offset -= d;
        }
    }

    fn step(self) -> usize {
        self.0
    }
}

#[derive(Clone, Copy)]
pub struct Unaligned;

impl Alignment for Unaligned {
    fn align(self, _: &mut usize) {}

    fn step(self) -> usize {
        1
    }
}
