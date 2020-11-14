use crate::{Address, Sign, Size};
use serde::{Deserialize, Serialize};
use std::{convert::TryFrom, fmt};
use thiserror::Error;

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Offset {
    #[serde(default, skip_serializing_if = "Sign::is_none")]
    sign: Sign,
    offset: Size,
}

impl Offset {
    /// Construct a new offset.
    /// If `sign` is true, the offset is positive. Otherwise it is negative.
    #[inline]
    pub fn new(sign: Sign, offset: Size) -> Self {
        #[cfg(debug_assertions)]
        {
            match sign {
                Sign::NoSign => assert_eq!(offset, 0u64.into()),
                _ => assert_ne!(offset, 0u64.into()),
            }
        }

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

    /// Apply to the specified address in a checked manner.
    pub fn checked_apply(self, address: Address) -> Option<Address> {
        use self::Sign::*;

        Some(match self {
            Offset { sign: NoSign, .. } => address,
            Offset { sign: Plus, offset } => Address(address.0.checked_add(offset.0)?),
            Offset {
                sign: Minus,
                offset,
            } => Address(address.0.checked_sub(offset.0)?),
        })
    }

    /// Apply to the specified address.
    pub fn saturating_apply(self, address: Address) -> Address {
        use self::Sign::*;

        match self {
            Offset { sign: NoSign, .. } => address,
            Offset { sign: Plus, offset } => Address(address.0.saturating_add(offset.0)),
            Offset {
                sign: Minus,
                offset,
            } => Address(address.0.saturating_sub(offset.0)),
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

#[derive(Debug, Error)]
#[error("out of range offset to integer type conversion attempted")]
pub struct TryFromOffsetError(());

impl TryFrom<Offset> for usize {
    type Error = TryFromOffsetError;

    fn try_from(offset: Offset) -> Result<Self, Self::Error> {
        if offset.sign == Sign::Minus {
            return Err(TryFromOffsetError(()));
        }

        usize::try_from(offset.offset.0).map_err(|_| TryFromOffsetError(()))
    }
}

#[cfg(test)]
mod tests {
    use super::{Offset, Sign};
    use crate::Size;

    #[test]
    fn test_offsets() {
        use self::Sign::*;

        assert_eq!(
            Offset::new(NoSign, Size::new(0)),
            Offset::new(Minus, Size::new(0x10)).saturating_add(Offset::new(Plus, Size::new(0x10)))
        );
        assert_eq!(
            Offset::new(Plus, Size::new(0x10)),
            Offset::new(Minus, Size::new(0x10)).saturating_add(Offset::new(Plus, Size::new(0x20)))
        );
        assert_eq!(
            Offset::new(Minus, Size::new(0x10)),
            Offset::new(Minus, Size::new(0x20)).saturating_add(Offset::new(Plus, Size::new(0x10)))
        );

        assert_eq!(
            Offset::new(NoSign, Size::new(0)),
            Offset::new(Plus, Size::new(0x10)).saturating_add(Offset::new(Minus, Size::new(0x10)))
        );
        assert_eq!(
            Offset::new(Minus, Size::new(0x10)),
            Offset::new(Plus, Size::new(0x10)).saturating_add(Offset::new(Minus, Size::new(0x20)))
        );
        assert_eq!(
            Offset::new(Plus, Size::new(0x10)),
            Offset::new(Plus, Size::new(0x20)).saturating_add(Offset::new(Minus, Size::new(0x10)))
        );

        assert_eq!(
            Offset::new(NoSign, Size::new(0)),
            Offset::new(NoSign, Size::new(0)).saturating_add(Offset::new(NoSign, Size::new(0)))
        );
        assert_eq!(
            Offset::new(Plus, Size::new(0x20)),
            Offset::new(Plus, Size::new(0x10)).saturating_add(Offset::new(Plus, Size::new(0x10)))
        );
    }
}
