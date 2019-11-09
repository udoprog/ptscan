use serde::{Deserialize, Serialize};

/// The sign of a number or offset.
#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Sign {
    #[serde(rename = "plus")]
    Plus,
    #[serde(rename = "minus")]
    Minus,
    #[serde(rename = "nosign")]
    NoSign,
}

impl From<num_bigint::Sign> for Sign {
    /// Convert from bigint sign.
    fn from(sign: num_bigint::Sign) -> Self {
        match sign {
            num_bigint::Sign::Plus => Self::Plus,
            num_bigint::Sign::Minus => Self::Minus,
            num_bigint::Sign::NoSign => Self::NoSign,
        }
    }
}
