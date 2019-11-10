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

    /// Construct a zero offset.
    pub fn zero() -> Self {
        Offset(Sign::NoSign, 0)
    }

    /// Add two offsets together in a saturating manner.
    pub fn saturating_add(self, offset: Offset) -> Offset {
        use self::Sign::*;

        match (self, offset) {
            (other, Offset(NoSign, _)) | (Offset(NoSign, _), other) => other,
            (Offset(Plus, a), Offset(Plus, b)) => Offset(Plus, a.saturating_add(b)),
            (Offset(Plus, a), Offset(Minus, b)) => {
                if a > b {
                    Offset(Plus, a.saturating_sub(b))
                } else if a < b {
                    Offset(Minus, b.saturating_sub(a))
                } else {
                    Offset::zero()
                }
            }
            (Offset(Minus, a), Offset(Plus, b)) => {
                if a > b {
                    Offset(Minus, a.saturating_sub(b))
                } else if a < b {
                    Offset(Plus, b.saturating_sub(a))
                } else {
                    Offset::zero()
                }
            }
            (Offset(Minus, a), Offset(Minus, b)) => Offset(Minus, a.saturating_add(b)),
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
            Offset(Sign::Plus, o) | Offset(Sign::NoSign, o) => write!(fmt, "0x{:X}", o),
            Offset(Sign::Minus, o) => write!(fmt, "-0x{:X}", o),
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
            Offset(NoSign, 0),
            Offset(Minus, 0x10).saturating_add(Offset(Plus, 0x10))
        );
        assert_eq!(
            Offset(Plus, 0x10),
            Offset(Minus, 0x10).saturating_add(Offset(Plus, 0x20))
        );
        assert_eq!(
            Offset(Minus, 0x10),
            Offset(Minus, 0x20).saturating_add(Offset(Plus, 0x10))
        );

        assert_eq!(
            Offset(NoSign, 0),
            Offset(Plus, 0x10).saturating_add(Offset(Minus, 0x10))
        );
        assert_eq!(
            Offset(Minus, 0x10),
            Offset(Plus, 0x10).saturating_add(Offset(Minus, 0x20))
        );
        assert_eq!(
            Offset(Plus, 0x10),
            Offset(Plus, 0x20).saturating_add(Offset(Minus, 0x10))
        );

        assert_eq!(
            Offset(NoSign, 0),
            Offset(NoSign, 0).saturating_add(Offset(NoSign, 0))
        );
        assert_eq!(
            Offset(Plus, 0x20),
            Offset(Plus, 0x10).saturating_add(Offset(Plus, 0x10))
        );
    }
}
