/// The sign of a number or offset.
#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Sign {
    Plus,
    Minus,
    NoSign,
}

impl Sign {
    /// Test if the sign is something other than `NoSign`.
    pub fn is_some(&self) -> bool {
        match self {
            Self::NoSign => false,
            _ => true,
        }
    }
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

impl serde::Serialize for Sign {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        match self {
            Self::Plus => serializer.serialize_some("+"),
            Self::Minus => serializer.serialize_some("-"),
            Self::NoSign => serializer.serialize_none(),
        }
    }
}

impl<'de> serde::de::Deserialize<'de> for Sign {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        Ok(
            match Option::<String>::deserialize(deserializer)?
                .as_ref()
                .map(String::as_str)
            {
                Some("+") => Sign::Plus,
                Some("-") => Sign::Minus,
                Some(value) => {
                    return Err(serde::de::Error::custom(format!(
                        "unexpected variant: {}",
                        value
                    )))
                }
                None => Sign::NoSign,
            },
        )
    }
}
