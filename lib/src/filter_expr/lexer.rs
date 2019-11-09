use crate::Type;
use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use std::{borrow::Cow, fmt, str};
use thiserror::Error;

#[derive(Debug, Error)]
#[error("{}", description)]
pub struct Error {
    /// Description of the error.
    description: Cow<'static, str>,
    /// Offset into the expression which caused the error.
    pos: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    /// And token.
    And,
    /// Or token.
    Or,
    /// value, references the value in memory.
    Value,
    /// `last` keyword.
    Last,
    /// `initial` keyword.
    Initial,
    /// `as` casting keyword.
    As,
    /// `not` keyword.
    Not,
    /// `is` keyword.
    Is,
    /// `nan` keyword,
    Nan,
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `==`.
    Eq,
    /// `!=`.
    Neq,
    /// `!~`.
    NotTilde,
    /// `<=`.
    Lte,
    /// `>=`.
    Gte,
    /// `<`.
    Lt,
    /// `>`.
    Gt,
    /// `^`.
    Caret,
    /// `~`.
    Tilde,
    /// `(`.
    OpenParen,
    /// `)`.
    CloseParen,
    /// A literal value, like `0x42`, or `128u32`.
    Number(BigInt, Option<Type>),
    /// A bigdecimal type.
    Decimal(BigDecimal, Option<Type>),
    /// A quoted string.
    String(String),
    /// A raw byte array.
    Bytes(Vec<u8>),
    /// A type literal, like `u8`.
    Type(Type),
    /// The addressof operator `&`.
    AddressOf,
}

impl fmt::Display for Token {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, fmt)
    }
}

/// Test if character qualifies as part of a number.
fn is_numeric(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false,
    }
}

/// Iterate over an input string and emits tokens.
#[derive(Debug)]
pub struct Lexer<'a> {
    /// The raw input.
    input: &'a str,
    /// Iterator over the input.
    iter: str::CharIndices<'a>,
    /// First character lookahead.
    c1: Option<(usize, char)>,
    /// Second character lookahead.
    c2: Option<(usize, char)>,
    /// Third character lookahead.
    c3: Option<(usize, char)>,
    /// Shared buffer used for buffering things.
    /// Note: remember to avoid using recursively.
    buf: String,
}

impl<'a> Lexer<'a> {
    /// Construct a new lexer for the given string.
    pub fn new(input: &'a str) -> Lexer<'a> {
        let mut iter = input.char_indices();

        let c1 = iter.next();
        let c2 = iter.next();
        let c3 = iter.next();

        Lexer {
            input,
            iter,
            c1,
            c2,
            c3,
            buf: String::with_capacity(128),
        }
    }

    /// Advance the iterator one step.
    pub fn step(&mut self) {
        self.c1 = self.c2.take();
        self.c2 = self.c3.take();
        self.c3 = self.iter.next();
    }

    /// Advance the iterate `n` number of steps.
    ///
    /// Returns the position stepped to.
    pub fn step_n(&mut self, n: usize) -> usize {
        for _ in 0..n {
            self.step();
        }

        // NB: if we don't have the first lookahead character, we are at the end of the string.
        self.c1.map(|(p, _)| p).unwrap_or_else(|| self.input.len())
    }

    /// Peek a single character.
    pub fn peek(&self) -> Option<(usize, char)> {
        let p = self.pos();
        self.c1.as_ref().map(|(_, c)| (p, *c))
    }

    /// Peek two characters.
    pub fn peek2(&self) -> Option<(usize, char, char)> {
        match (self.peek(), self.c2.as_ref().map(|(_, c)| *c)) {
            (Some((p, c1)), Some(c2)) => Some((p, c1, c2)),
            _ => None,
        }
    }

    /// Peek three characters.
    pub fn peek3(&self) -> Option<(usize, char, Option<char>, Option<char>)> {
        match (
            self.peek(),
            self.c2.as_ref().map(|(_, c)| *c),
            self.c3.as_ref().map(|(_, c)| *c),
        ) {
            (Some((p, c1)), c2, c3) => Some((p, c1, c2, c3)),
            _ => None,
        }
    }

    /// Get the current position of the iterator.
    /// Used for error handling.
    pub fn pos(&self) -> usize {
        self.c1
            .as_ref()
            .map(|(pos, _)| *pos)
            .unwrap_or_else(|| self.input.len())
    }

    /// Format an error with a correct description and position.
    pub fn err(&self, description: impl Into<Cow<'static, str>>) -> Error {
        Error {
            description: description.into(),
            pos: self.pos(),
        }
    }

    // decode a sequence of 4 unicode characters
    fn decode_unicode4(&mut self) -> Result<char, (&'static str, usize)> {
        let mut res = 0u32;

        for x in 0..4u32 {
            let c = self
                .peek()
                .ok_or_else(|| ("expected digit", x as usize))?
                .1
                .to_string();
            let c = u32::from_str_radix(&c, 16).map_err(|_| ("expected hex digit", x as usize))?;
            res += c << (4 * (3 - x));
            self.step();
        }

        Ok(::std::char::from_u32(res).ok_or_else(|| ("invalid character", 0usize))?)
    }

    fn scan_escape(&mut self) -> Result<char, Error> {
        self.step();

        let (_, escape) = self.peek().ok_or_else(|| self.err("unterminated escape"))?;

        let escaped = match escape {
            '0' => '\0',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '\"' => '\"',
            'u' => {
                let seq_start = self.step_n(1);

                let c = self
                    .decode_unicode4()
                    .map_err(|(description, offset)| Error {
                        description: description.into(),
                        pos: seq_start + offset,
                    })?;

                return Ok(c);
            }
            _ => {
                return Err(self.err(
                    "unrecognized escape, should be one of: \\\", \\n, \\r, \\t, \\\\, \
                     or \\uXXXX",
                ));
            }
        };

        self.step();
        Ok(escaped)
    }

    /// Scan a string.
    fn scan_bytes(&mut self) -> Result<Vec<u8>, Error> {
        self.buf.clear();

        while let Some((_, c)) = self.peek() {
            match c {
                ' ' => (),
                'a'..='f' => {
                    self.buf.extend(c.to_uppercase());
                }
                '0'..='9' | 'A'..='F' => {
                    self.buf.push(c);
                }
                '}' => {
                    self.step();
                    break;
                }
                c => return Err(self.err(format!("unexpected character: {}", c))),
            }

            self.step();
        }

        let result = hex::FromHex::from_hex(&self.buf);
        Ok(result.map_err(|e: hex::FromHexError| self.err(e.to_string()))?)
    }

    /// Scan a string.
    fn scan_string(&mut self) -> Result<String, Error> {
        self.buf.clear();

        self.step();

        while let Some((_, c)) = self.peek() {
            match c {
                '\\' => {
                    let c = self.scan_escape()?;
                    self.buf.push(c);
                    continue;
                }
                '"' => {
                    self.step();
                    return Ok(self.buf.clone());
                }
                c => {
                    self.buf.push(c);
                    self.step();
                }
            }
        }

        Err(self.err("unterminated string"))
    }

    /// Scan an identifier.
    pub fn scan_ident(&mut self) -> Result<String, Error> {
        self.buf.clear();

        while let Some((_, c)) = self.peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '/' => {
                    self.step();
                    self.buf.push(c);
                }
                _ => break,
            }
        }

        Ok(self.buf.to_string())
    }

    /// Scan an identifier.
    pub fn scan_literal(&mut self) -> Result<Token, Error> {
        use num::Num;
        use std::str::FromStr as _;

        self.buf.clear();

        let mut hex = false;
        let mut decimal = false;

        if let Some((_, '-')) = self.peek() {
            self.buf.push('-');
            self.step();
        }

        if let Some((_, '0', 'x')) = self.peek2() {
            hex = true;
            self.step_n(2);
        }

        let mut ty = None;

        while let Some((_, c)) = self.peek() {
            match c {
                '0'..='9' | 'a'..='f' | 'A'..='F' if hex => {
                    self.step();
                    self.buf.push(c);
                }
                '0'..='9' if !hex => {
                    self.step();
                    self.buf.push(c);
                }
                '.' if !hex => {
                    decimal = true;
                    self.step();
                    self.buf.push(c);
                }
                'i' | 'u' | 'f' => {
                    self.step();

                    let (step, inferred_ty) = match (c, self.peek3()) {
                        ('i', Some((_, '8', _, _))) => (1, Type::I8),
                        ('i', Some((_, '1', Some('6'), _))) => (2, Type::I16),
                        ('i', Some((_, '3', Some('2'), _))) => (2, Type::I32),
                        ('i', Some((_, '6', Some('4'), _))) => (2, Type::I64),
                        ('i', Some((_, '1', Some('2'), Some('8')))) => (3, Type::I128),
                        ('u', Some((_, '8', _, _))) => (1, Type::U8),
                        ('u', Some((_, '1', Some('6'), _))) => (2, Type::U16),
                        ('u', Some((_, '3', Some('2'), _))) => (2, Type::U32),
                        ('u', Some((_, '6', Some('4'), _))) => (2, Type::U64),
                        ('u', Some((_, '1', Some('2'), Some('8')))) => (3, Type::U128),
                        ('f', Some((_, '3', Some('2'), _))) => (2, Type::F32),
                        ('f', Some((_, '6', Some('4'), _))) => (2, Type::F64),
                        _ => return Err(self.err("unexpected suffix")),
                    };

                    self.step_n(step);
                    ty = Some(inferred_ty);
                    break;
                }
                _ => break,
            }
        }

        if decimal {
            let decimal = BigDecimal::from_str(&self.buf).map_err(|e| self.err(e.to_string()))?;
            Ok(Token::Decimal(decimal, ty))
        } else {
            let result = if hex {
                BigInt::from_str_radix(&self.buf, 16)
            } else {
                BigInt::from_str_radix(&self.buf, 10)
            };

            let number = result.map_err(|e| self.err(e.to_string()))?;
            Ok(Token::Number(number, ty))
        }
    }
}

macro_rules! try_iter {
    ($expr:expr) => {
        match $expr {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        }
    };
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<(usize, Token, usize), Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.peek2() {
                Some((s, '=', '=')) => {
                    let e = self.step_n(2);
                    return Some(Ok((s, Token::Eq, e)));
                }
                Some((s, '!', '=')) => {
                    let e = self.step_n(2);
                    return Some(Ok((s, Token::Neq, e)));
                }
                Some((s, '!', '~')) => {
                    let e = self.step_n(2);
                    return Some(Ok((s, Token::NotTilde, e)));
                }
                Some((s, '<', '=')) => {
                    let e = self.step_n(2);
                    return Some(Ok((s, Token::Lte, e)));
                }
                Some((s, '>', '=')) => {
                    let e = self.step_n(2);
                    return Some(Ok((s, Token::Gte, e)));
                }
                _ => {}
            }

            let (s, c) = match self.peek() {
                Some(c) => c,
                None => break,
            };

            match c {
                ' ' => {
                    self.step();
                    continue;
                }
                '<' => {
                    self.step();
                    let e = self.pos();
                    return Some(Ok((s, Token::Lt, e)));
                }
                '>' => {
                    self.step();
                    let e = self.pos();
                    return Some(Ok((s, Token::Gt, e)));
                }
                '^' => {
                    self.step();
                    let e = self.pos();
                    return Some(Ok((s, Token::Caret, e)));
                }
                '~' => {
                    self.step();
                    let e = self.pos();
                    return Some(Ok((s, Token::Tilde, e)));
                }
                '{' => {
                    self.step();
                    let bytes = try_iter!(self.scan_bytes());
                    let e = self.pos();
                    return Some(Ok((s, Token::Bytes(bytes), e)));
                }
                '(' => {
                    self.step();
                    let e = self.pos();
                    return Some(Ok((s, Token::OpenParen, e)));
                }
                ')' => {
                    self.step();
                    let e = self.pos();
                    return Some(Ok((s, Token::CloseParen, e)));
                }
                '"' => {
                    let string = try_iter!(self.scan_string());
                    let e = self.pos();
                    return Some(Ok((s, Token::String(string), e)));
                }
                '*' => {
                    self.step();
                    let e = self.pos();
                    return Some(Ok((s, Token::Star, e)));
                }
                '/' => {
                    self.step();
                    let e = self.pos();
                    return Some(Ok((s, Token::Slash, e)));
                }
                '+' => {
                    self.step();
                    let e = self.pos();
                    return Some(Ok((s, Token::Plus, e)));
                }
                // NB: special test that `-` is not immediately followed by a number which would be a numeric literal.
                '-' if !self.c2.map(|(_, c)| is_numeric(c)).unwrap_or_default() => {
                    self.step();
                    let e = self.pos();
                    return Some(Ok((s, Token::Minus, e)));
                }
                '&' => {
                    self.step();
                    let e = self.pos();
                    return Some(Ok((s, Token::AddressOf, e)));
                }
                '-' | '0'..='9' => {
                    let literal = try_iter!(self.scan_literal());
                    let e = self.pos();
                    return Some(Ok((s, literal, e)));
                }
                _ => {
                    let ident = try_iter!(self.scan_ident());
                    let e = self.pos();

                    if let Ok(ty) = str::parse::<Type>(&ident) {
                        return Some(Ok((s, Token::Type(ty), e)));
                    }

                    match ident.as_str() {
                        "value" => return Some(Ok((s, Token::Value, e))),
                        "last" => return Some(Ok((s, Token::Last, e))),
                        "initial" => return Some(Ok((s, Token::Initial, e))),
                        "not" => return Some(Ok((s, Token::Not, e))),
                        "and" => return Some(Ok((s, Token::And, e))),
                        "or" => return Some(Ok((s, Token::Or, e))),
                        "is" => return Some(Ok((s, Token::Is, e))),
                        "as" => return Some(Ok((s, Token::As, e))),
                        "nan" => return Some(Ok((s, Token::Nan, e))),
                        string => {
                            return Some(Ok((s, Token::String(string.to_string()), e)));
                        }
                    }
                }
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::{Error, Lexer, Token};
    use num_bigint::BigInt;

    fn tokenize(input: &str) -> Result<Vec<(usize, Token, usize)>, Error> {
        Lexer::new(input).collect::<Result<Vec<_>, _>>()
    }

    #[test]
    fn basic_expression() -> Result<(), Error> {
        assert_eq!(
            vec![
                (0, Token::Value, 5),
                (6, Token::Eq, 8),
                (9, Token::Literal(BigInt::from(42), None), 11)
            ],
            tokenize("value == 42")?
        );

        assert_eq!(
            vec![(0, Token::Literal(BigInt::from(0x424140), None), 8)],
            tokenize("0x424140")?
        );

        assert_eq!(
            vec![
                (0, Token::Literal(BigInt::from(0x42), None), 4),
                (5, Token::Literal(BigInt::from(83), None), 7)
            ],
            tokenize("0x42 83")?
        );

        Ok(())
    }
}
