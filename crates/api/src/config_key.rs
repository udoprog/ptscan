use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};
use core::iter::Peekable;
use core::str::{CharIndices, FromStr};

use musli::{Decode, Encode};

use crate::ConfigField;

/// How many segments a key can hold.
pub const MAX_SEGMENTS: usize = 4;

/// What fills the unused segments of a key.
const FILL: ConfigSegment = ConfigSegment::Index(0);

/// One component of a [`ConfigKey`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ConfigSegment {
    /// A named field, written `scan` or `.filter`.
    Field(ConfigField),
    /// A member by position, written `[0]`.
    Index(usize),
    /// A member by identifier, written `[0x1f]`.
    Id(u64),
}

/// A short path naming one stored setting, such as `scan.filter` or
/// `watch[0x1f].pointer`.
///
/// Its [`fmt::Display`] text is canonical and is what every encoding stores.
#[derive(Clone, Copy, Encode, Decode)]
#[musli(display)]
pub struct ConfigKey {
    segments: [ConfigSegment; MAX_SEGMENTS],
    len: usize,
}

impl ConfigKey {
    #[inline]
    pub const fn field(field: ConfigField) -> Self {
        Self {
            segments: [ConfigSegment::Field(field), FILL, FILL, FILL],
            len: 1,
        }
    }

    /// Append a field, as in `scan.filter`.
    #[inline]
    pub fn with_field(self, field: ConfigField) -> Result<Self, ConfigKeyFull> {
        self.with(ConfigSegment::Field(field))
    }

    /// Append a member by position, as in `watch[0]`.
    #[inline]
    pub fn with_index(self, index: usize) -> Result<Self, ConfigKeyFull> {
        self.with(ConfigSegment::Index(index))
    }

    /// Append a member by identifier, as in `watch[0x1f]`.
    #[inline]
    pub fn with_id(self, id: u64) -> Result<Self, ConfigKeyFull> {
        self.with(ConfigSegment::Id(id))
    }

    #[inline]
    pub fn as_slice(&self) -> &[ConfigSegment] {
        self.segments.get(..self.len).unwrap_or(&[])
    }

    fn with(mut self, segment: ConfigSegment) -> Result<Self, ConfigKeyFull> {
        self.push(segment)?;
        Ok(self)
    }

    fn push(&mut self, segment: ConfigSegment) -> Result<(), ConfigKeyFull> {
        let slot = self.segments.get_mut(self.len).ok_or(ConfigKeyFull)?;
        *slot = segment;
        self.len = self.len.saturating_add(1);
        Ok(())
    }
}

/// A key already holds [`MAX_SEGMENTS`] segments.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ConfigKeyFull;

impl fmt::Display for ConfigKeyFull {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("configuration key has too many segments")
    }
}

impl core::error::Error for ConfigKeyFull {}

impl PartialEq for ConfigKey {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl Eq for ConfigKey {}

impl PartialOrd for ConfigKey {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ConfigKey {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}

impl Hash for ConfigKey {
    #[inline]
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.as_slice().hash(state);
    }
}

impl fmt::Debug for ConfigKey {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ConfigKey({self})")
    }
}

impl fmt::Display for ConfigKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (position, segment) in self.as_slice().iter().enumerate() {
            match segment {
                ConfigSegment::Field(field) => {
                    if position > 0 {
                        f.write_str(".")?;
                    }

                    f.write_str(field.as_str())?;
                }
                ConfigSegment::Index(index) => write!(f, "[{index}]")?,
                ConfigSegment::Id(id) => write!(f, "[0x{id:x}]")?,
            }
        }

        Ok(())
    }
}

#[cfg(feature = "sqll")]
impl ::sqll::BindValue for ConfigKey {
    #[inline]
    fn bind_value(&self, stmt: &mut ::sqll::Statement, index: ::sqll::Index) -> ::sqll::Result<()> {
        self.to_string().as_str().bind_value(stmt, index)
    }
}

#[cfg(feature = "sqll")]
impl ::sqll::FromColumn<'_> for ConfigKey {
    type Type = ::sqll::ty::Text;

    #[inline]
    fn from_column(stmt: &::sqll::Statement, index: ::sqll::ty::Text) -> ::sqll::Result<Self> {
        <&str>::from_column(stmt, index)?
            .parse()
            .map_err(::sqll::Error::mismatch)
    }
}

impl FromStr for ConfigKey {
    type Err = ParseConfigKeyError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Parser::new(input).parse()
    }
}

/// Why a configuration key could not be parsed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConfigKeyErrorKind {
    InvalidCharacter(char),
    UnknownField,
    ExpectedField,
    ExpectedIndex,
    ExpectedClosingBracket,
    ExpectedSeparator,
    IndexOverflow,
    InvalidId,
    TooManySegments,
}

/// An error at a byte offset in a configuration key.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParseConfigKeyError {
    offset: usize,
    kind: ConfigKeyErrorKind,
}

impl ParseConfigKeyError {
    #[inline]
    pub fn offset(self) -> usize {
        self.offset
    }

    #[inline]
    pub fn kind(self) -> ConfigKeyErrorKind {
        self.kind
    }
}

impl fmt::Display for ParseConfigKeyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = match self.kind {
            ConfigKeyErrorKind::InvalidCharacter(c) => {
                return write!(
                    f,
                    "invalid character {c:?} at byte {} in configuration key",
                    self.offset
                );
            }
            ConfigKeyErrorKind::UnknownField => "not a known field",
            ConfigKeyErrorKind::ExpectedField => "expected a field",
            ConfigKeyErrorKind::ExpectedIndex => "expected an index or a `0x` identifier",
            ConfigKeyErrorKind::ExpectedClosingBracket => "expected `]`",
            ConfigKeyErrorKind::ExpectedSeparator => "expected `.` or `[`",
            ConfigKeyErrorKind::IndexOverflow => "index does not fit in a usize",
            ConfigKeyErrorKind::InvalidId => "expected 1 to 16 lowercase hex digits after `0x`",
            ConfigKeyErrorKind::TooManySegments => "more segments than a key can hold",
        };

        write!(f, "{message} at byte {} in configuration key", self.offset)
    }
}

impl core::error::Error for ParseConfigKeyError {}

/// Characters a word is made of; `-` lets a field read as `modules-only`.
#[inline]
fn is_word(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '-'
}

struct Parser<'a> {
    input: &'a str,
    chars: Peekable<CharIndices<'a>>,
    key: ConfigKey,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.char_indices().peekable(),
            key: ConfigKey {
                segments: [FILL; MAX_SEGMENTS],
                len: 0,
            },
        }
    }

    fn parse(mut self) -> Result<ConfigKey, ParseConfigKeyError> {
        self.field()?;

        loop {
            match self.peek() {
                None => return Ok(self.key),
                Some('.') => {
                    self.chars.next();
                    self.field()?;
                }
                Some('[') => {
                    self.chars.next();
                    self.member()?;
                }
                Some(c) => return Err(self.unexpected(c, ConfigKeyErrorKind::ExpectedSeparator)),
            }
        }
    }

    fn field(&mut self) -> Result<(), ParseConfigKeyError> {
        let start = self.offset();
        let word = self.word();

        if word.is_empty() {
            return Err(match self.peek() {
                Some(c) => self.unexpected(c, ConfigKeyErrorKind::ExpectedField),
                None => Self::error(start, ConfigKeyErrorKind::ExpectedField),
            });
        }

        let field = word
            .parse()
            .map_err(|_| Self::error(start, ConfigKeyErrorKind::UnknownField))?;

        self.push(ConfigSegment::Field(field), start)
    }

    fn member(&mut self) -> Result<(), ParseConfigKeyError> {
        let start = self.offset();
        let word = self.word();

        let segment = if let Some(hex) = word.strip_prefix("0x") {
            if hex.is_empty()
                || hex.len() > 16
                || !hex.bytes().all(|b| matches!(b, b'0'..=b'9' | b'a'..=b'f'))
            {
                return Err(Self::error(start, ConfigKeyErrorKind::InvalidId));
            }

            let id = u64::from_str_radix(hex, 16)
                .map_err(|_| Self::error(start, ConfigKeyErrorKind::InvalidId))?;

            ConfigSegment::Id(id)
        } else if !word.is_empty() && word.bytes().all(|b| b.is_ascii_digit()) {
            let index = word
                .parse()
                .map_err(|_| Self::error(start, ConfigKeyErrorKind::IndexOverflow))?;

            ConfigSegment::Index(index)
        } else if let (true, Some(c)) = (word.is_empty(), self.peek()) {
            return Err(self.unexpected(c, ConfigKeyErrorKind::ExpectedIndex));
        } else {
            return Err(Self::error(start, ConfigKeyErrorKind::ExpectedIndex));
        };

        self.push(segment, start)?;

        match self.peek() {
            Some(']') => {
                self.chars.next();
                Ok(())
            }
            Some(c) => Err(self.unexpected(c, ConfigKeyErrorKind::ExpectedClosingBracket)),
            None => Err(Self::error(
                self.offset(),
                ConfigKeyErrorKind::ExpectedClosingBracket,
            )),
        }
    }

    fn push(&mut self, segment: ConfigSegment, at: usize) -> Result<(), ParseConfigKeyError> {
        self.key
            .push(segment)
            .map_err(|_| Self::error(at, ConfigKeyErrorKind::TooManySegments))
    }

    fn word(&mut self) -> &'a str {
        let start = self.offset();

        while self.peek().is_some_and(is_word) {
            self.chars.next();
        }

        self.input.get(start..self.offset()).unwrap_or_default()
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|&(_, c)| c)
    }

    fn offset(&mut self) -> usize {
        self.chars.peek().map_or(self.input.len(), |&(at, _)| at)
    }

    /// The error for `c` where `expected` was due: one of the key's own
    /// punctuation out of place, or a character keys never contain.
    fn unexpected(&mut self, c: char, expected: ConfigKeyErrorKind) -> ParseConfigKeyError {
        let at = self.offset();

        if matches!(c, '.' | '[' | ']') {
            Self::error(at, expected)
        } else {
            Self::error(at, ConfigKeyErrorKind::InvalidCharacter(c))
        }
    }

    fn error(offset: usize, kind: ConfigKeyErrorKind) -> ParseConfigKeyError {
        ParseConfigKeyError { offset, kind }
    }
}

#[cfg(test)]
mod tests {
    use super::{ConfigField, ConfigKey, ConfigKeyErrorKind, ConfigSegment, MAX_SEGMENTS};

    #[test]
    fn round_trips_through_its_text() {
        for text in [
            "scan",
            "scan.filter",
            "modules-only",
            "watch[0]",
            "watch[0x1f].pointer",
            "watch[12].pointer[0xffffffffffffffff]",
        ] {
            let key = text.parse::<ConfigKey>().unwrap();
            assert_eq!(key.to_string(), text);
            assert_eq!(key.to_string().parse::<ConfigKey>().unwrap(), key);
        }
    }

    #[test]
    fn builds_the_same_key_it_parses() {
        let key = ConfigField::Watch
            .key()
            .with_id(0x1f)
            .unwrap()
            .with_field(ConfigField::Pointer)
            .unwrap();

        assert_eq!(key, "watch[0x1f].pointer".parse().unwrap());
        assert_eq!(
            key.as_slice(),
            [
                ConfigSegment::Field(ConfigField::Watch),
                ConfigSegment::Id(0x1f),
                ConfigSegment::Field(ConfigField::Pointer),
            ]
        );
        assert_eq!(
            ConfigField::from_key(&ConfigField::Scan.key()),
            Some(ConfigField::Scan)
        );
        assert_eq!(ConfigField::from_key(&key), None);
    }

    #[test]
    fn an_identifier_is_told_from_an_index_by_its_prefix() {
        let index = "watch[10]".parse::<ConfigKey>().unwrap();
        let id = "watch[0x10]".parse::<ConfigKey>().unwrap();

        assert_eq!(index.as_slice()[1], ConfigSegment::Index(10));
        assert_eq!(id.as_slice()[1], ConfigSegment::Id(16));
        assert_ne!(index, id);
    }

    #[test]
    fn rejects_malformed_keys() {
        let cases = [
            ("", 0, ConfigKeyErrorKind::ExpectedField),
            ("scan.", 5, ConfigKeyErrorKind::ExpectedField),
            ("scan..filter", 5, ConfigKeyErrorKind::ExpectedField),
            (".scan", 0, ConfigKeyErrorKind::ExpectedField),
            ("[0]", 0, ConfigKeyErrorKind::ExpectedField),
            ("watch[]", 6, ConfigKeyErrorKind::ExpectedIndex),
            ("watch[scan]", 6, ConfigKeyErrorKind::ExpectedIndex),
            ("watch[1", 7, ConfigKeyErrorKind::ExpectedClosingBracket),
            (
                "watch[1.scan]",
                7,
                ConfigKeyErrorKind::ExpectedClosingBracket,
            ),
            ("scan]", 4, ConfigKeyErrorKind::ExpectedSeparator),
            ("scan filter", 4, ConfigKeyErrorKind::InvalidCharacter(' ')),
            (" scan", 0, ConfigKeyErrorKind::InvalidCharacter(' ')),
            ("nonesuch", 0, ConfigKeyErrorKind::UnknownField),
            ("scan.0", 5, ConfigKeyErrorKind::UnknownField),
            ("watch[0x]", 6, ConfigKeyErrorKind::InvalidId),
            ("watch[0xAB]", 6, ConfigKeyErrorKind::InvalidId),
            ("watch[0xzz]", 6, ConfigKeyErrorKind::InvalidId),
            (
                "watch[0x11223344556677889]",
                6,
                ConfigKeyErrorKind::InvalidId,
            ),
            ("scan.💥", 5, ConfigKeyErrorKind::InvalidCharacter('💥')),
        ];

        for (input, offset, kind) in cases {
            let error = input.parse::<ConfigKey>().unwrap_err();
            assert_eq!((error.offset(), error.kind()), (offset, kind), "{input:?}");
        }
    }

    #[test]
    fn rejects_indices_that_do_not_fit_in_usize() {
        let input = format!("watch[{}0]", usize::MAX);
        let error = input.parse::<ConfigKey>().unwrap_err();

        assert_eq!(error.offset(), 6);
        assert_eq!(error.kind(), ConfigKeyErrorKind::IndexOverflow);
    }

    #[test]
    fn refuses_a_key_deeper_than_it_can_hold() {
        let key = "watch[0].watch[1]".parse::<ConfigKey>().unwrap();
        assert_eq!(key.as_slice().len(), MAX_SEGMENTS);
        assert!(key.with_field(ConfigField::Scan).is_err());

        let error = "watch[0].watch[1].scan".parse::<ConfigKey>().unwrap_err();
        assert_eq!(error.kind(), ConfigKeyErrorKind::TooManySegments);
        assert_eq!(error.offset(), 18);
    }

    #[test]
    fn encodes_as_its_text() {
        let key = "watch[0x1f].pointer".parse::<ConfigKey>().unwrap();
        let json = musli::json::to_string(&key).unwrap();

        assert_eq!(json, "\"watch[0x1f].pointer\"");
        assert_eq!(musli::json::from_str::<ConfigKey>(&json).unwrap(), key);
        assert!(musli::json::from_str::<ConfigKey>("\"watch[\"").is_err());
    }
}
