use crate::{Sign, Size};
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Offset {
    #[serde(skip_serializing_if = "Sign::is_some")]
    pub(crate) sign: Sign,
    pub(crate) offset: Size,
}

impl Offset {
    /// Construct a new offset.
    /// If `sign` is true, the offset is positive. Otherwise it is negative.
    #[inline]
    pub fn new(sign: Sign, offset: Size) -> Self {
        assert!(sign != Sign::NoSign || offset != 0u64.into());
        Offset { sign, offset }
    }

    /// Construct a zero offset.
    #[inline]
    pub fn zero() -> Self {
        Offset {
            sign: Sign::NoSign,
            offset: Size::new(0),
        }
    }

    /// Add two offsets together in a saturating manner.
    pub fn saturating_add(self, offset: Offset) -> Offset {
        use self::Sign::*;

        match (self, offset) {
            (other, Offset { sign: NoSign, .. }) | (Offset { sign: NoSign, .. }, other) => other,
            (
                Offset {
                    sign: Plus,
                    offset: a,
                },
                Offset {
                    sign: Plus,
                    offset: b,
                },
            ) => Offset {
                sign: Plus,
                offset: a.saturating_add(b),
            },
            (
                Offset {
                    sign: Plus,
                    offset: a,
                },
                Offset {
                    sign: Minus,
                    offset: b,
                },
            ) => {
                if a > b {
                    Offset {
                        sign: Plus,
                        offset: a.saturating_sub(b),
                    }
                } else if a < b {
                    Offset {
                        sign: Minus,
                        offset: b.saturating_sub(a),
                    }
                } else {
                    Offset::zero()
                }
            }
            (
                Offset {
                    sign: Minus,
                    offset: a,
                },
                Offset {
                    sign: Plus,
                    offset: b,
                },
            ) => {
                if a > b {
                    Offset {
                        sign: Minus,
                        offset: a.saturating_sub(b),
                    }
                } else if a < b {
                    Offset {
                        sign: Plus,
                        offset: b.saturating_sub(a),
                    }
                } else {
                    Offset::zero()
                }
            }
            (
                Offset {
                    sign: Minus,
                    offset: a,
                },
                Offset {
                    sign: Minus,
                    offset: b,
                },
            ) => Offset {
                sign: Minus,
                offset: a.saturating_add(b),
            },
        }
    }

    /// Return the sign which is `true` for positive, `false` for negative.
    pub fn sign(self) -> Sign {
        self.sign
    }

    /// Return the absolute offset of this offset.
    pub fn abs(self) -> Size {
        self.offset
    }

    /// Check if the offset is within the given size.
    pub fn is_within(self, size: Size) -> bool {
        self.offset <= size
    }

    /// Convert into an absolute distance.
    pub fn distance(self) -> Size {
        self.offset
    }
}

impl fmt::Display for Offset {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Offset {
                sign: Sign::Plus,
                offset,
            }
            | Offset {
                sign: Sign::NoSign,
                offset,
            } => write!(fmt, "{}", offset),
            Offset {
                sign: Sign::Minus,
                offset,
            } => write!(fmt, "-{}", offset),
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
