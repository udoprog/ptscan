use core::fmt;
use core::str::FromStr;

use crate::{ConfigKey, ConfigSegment};

macro_rules! fields {
    ($($variant:ident = $text:literal),* $(,)?) => {
        /// One field name in a [`ConfigKey`].
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum ConfigField {
            $($variant),*
        }

        impl ConfigField {
            /// Every field, in declaration order.
            pub const ALL: &'static [ConfigField] = &[$(ConfigField::$variant),*];

            /// The canonical text of this field.
            #[inline]
            pub const fn as_str(self) -> &'static str {
                match self {
                    $(ConfigField::$variant => $text),*
                }
            }
        }

        impl FromStr for ConfigField {
            type Err = UnknownConfigField;

            #[inline]
            fn from_str(input: &str) -> Result<Self, Self::Err> {
                match input {
                    $($text => Ok(ConfigField::$variant),)*
                    _ => Err(UnknownConfigField),
                }
            }
        }
    };
}

fields! {
    Alignment = "alignment",
    Created = "created",
    Filter = "filter",
    MaxDepth = "max-depth",
    MaxOffset = "max-offset",
    MaxPaths = "max-paths",
    ModulesOnly = "modules-only",
    Name = "name",
    Opened = "opened",
    Order = "order",
    Pointer = "pointer",
    PointerScan = "pointer-scan",
    Scan = "scan",
    Session = "session",
    Suspend = "suspend",
    Target = "target",
    Type = "type",
    Value = "value",
    Watch = "watch",
}

impl ConfigField {
    #[inline]
    pub const fn key(self) -> ConfigKey {
        ConfigKey::field(self)
    }

    /// The field a key names, when the key is this one field alone.
    #[inline]
    pub fn from_key(key: &ConfigKey) -> Option<Self> {
        match key.as_slice() {
            [ConfigSegment::Field(field)] => Some(*field),
            _ => None,
        }
    }
}

impl fmt::Display for ConfigField {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// A word that is not one of the [`ConfigField`]s.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnknownConfigField;

impl fmt::Display for UnknownConfigField {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("unknown configuration field")
    }
}

impl core::error::Error for UnknownConfigField {}

#[cfg(test)]
mod tests {
    use super::ConfigField;

    #[test]
    fn every_field_round_trips() {
        for field in ConfigField::ALL {
            assert_eq!(field.as_str().parse::<ConfigField>(), Ok(*field));
        }
    }
}
