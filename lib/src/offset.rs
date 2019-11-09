use crate::{Sign, Size};
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Offset(pub(crate) Sign, pub(crate) u64);

impl Offset {
    /// Construct a new offset.
    /// If `sign` is true, the offset is positive. Otherwise it is negative.
    pub fn new(sign: Sign, offset: u64) -> Self {
        Offset(sign, offset)
    }

    /// Initialize the zero offset.
    pub fn zero() -> Self {
        Offset(Sign::Pos, 0)
    }

    /// Add two offsets together in a saturating manner.
    pub fn saturating_add(self, offset: Offset) -> Offset {
        use self::Sign::*;

        match (self, offset) {
            (Offset(Pos, a), Offset(Pos, b)) => Offset(Pos, a.saturating_add(b)),
            (Offset(Pos, a), Offset(Neg, b)) => {
                if a >= b {
                    Offset(Pos, a - b)
                } else {
                    Offset(Neg, b - a)
                }
            }
            (Offset(Neg, a), Offset(Pos, b)) => {
                if a > b {
                    Offset(Neg, a - b)
                } else {
                    Offset(Pos, b - a)
                }
            }
            (Offset(Neg, a), Offset(Neg, b)) => Offset(Neg, a.saturating_add(b)),
        }
    }

    /// Return the sign which is `true` for positive, `false` for negative.
    pub fn sign(self) -> Sign {
        self.0
    }

    /// Return the absolute offset of this offset.
    pub fn abs(self) -> u64 {
        self.1
    }

    /// Check if the offset is within the given size.
    pub fn is_within(self, size: Size) -> bool {
        self.1 <= size.0
    }

    /// Convert into an absolute distance.
    pub fn distance(self) -> u64 {
        self.1
    }
}

impl fmt::Display for Offset {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Offset(Sign::Pos, o) => write!(fmt, "0x{:X}", o),
            Offset(Sign::Neg, o) => write!(fmt, "-0x{:X}", o),
        }
    }
}

impl fmt::Debug for Offset {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, fmt)
    }
}

#[cfg(test)]
mod tests {
    use super::{Offset, Sign};

    #[test]
    fn test_offsets() {
        use self::Sign::*;

        assert_eq!(
            Offset(Pos, 0),
            Offset(Neg, 0x10).saturating_add(Offset(Pos, 0x10))
        );
        assert_eq!(
            Offset(Pos, 0x10),
            Offset(Neg, 0x10).saturating_add(Offset(Pos, 0x20))
        );
        assert_eq!(
            Offset(Neg, 0x10),
            Offset(Neg, 0x20).saturating_add(Offset(Pos, 0x10))
        );

        assert_eq!(
            Offset(Pos, 0),
            Offset(Pos, 0x10).saturating_add(Offset(Neg, 0x10))
        );
        assert_eq!(
            Offset(Neg, 0x10),
            Offset(Pos, 0x10).saturating_add(Offset(Neg, 0x20))
        );
        assert_eq!(
            Offset(Pos, 0x10),
            Offset(Pos, 0x20).saturating_add(Offset(Neg, 0x10))
        );

        assert_eq!(
            Offset(Pos, 0),
            Offset(Pos, 0).saturating_add(Offset(Pos, 0))
        );
        assert_eq!(
            Offset(Pos, 0x20),
            Offset(Pos, 0x10).saturating_add(Offset(Pos, 0x10))
        );
    }
}
