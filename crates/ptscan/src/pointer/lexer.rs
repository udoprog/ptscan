use crate::{Address, Offset, Sign, Size};
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
pub struct Hex(i128);

impl Hex {
    /// Forcibly convert into an address.
    pub fn into_address(self) -> Address {
        Address::new(self.0 as u64)
    }

    /// Forcibly convert into an offset.
    pub fn into_offset(self) -> Offset {
        if self.0 < 0 {
            Offset::new(Sign::Minus, Size::new((-self.0) as u64))
        } else if self.0 > 0 {
            Offset::new(Sign::Plus, Size::new(self.0 as u64))
        } else {
            Offset::zero()
        }
    }

    /// Negate the hex value.
    pub fn negate(self) -> Hex {
        Hex(-self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    /// A quoted string.
    String(String),
    /// `->`
    Rocket,
    /// A numeric hex.
    Hex(Hex),
    /// `-`
    Minus,
    /// `+`
    Plus,
}

impl fmt::Display for Token {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, fmt)
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
        self.c1
            .as_ref()
            .map(|(p, _)| *p)
            .unwrap_or_else(|| self.input.len())
    }

    /// Peek a single character.
    pub fn peek(&self) -> Option<(usize, char)> {
        self.c1.as_ref().map(|(p, c)| (*p, *c))
    }

    /// Peek two characters.
    pub fn peek2(&self) -> Option<(usize, char)> {
        self.c2.as_ref().map(|(p, c)| (*p, *c))
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

    /// Scan an identifier.
    pub fn scan_hex(&mut self) -> Result<Hex, Error> {
        self.buf.clear();

        while let Some((_, c)) = self.peek() {
            match c {
                '+' | '-' | '0'..='9' | 'A'..='F' | 'a'..='f' => {
                    self.step();
                    self.buf.push(c);
                    continue;
                }
                _ => break,
            }
        }

        let value = i128::from_str_radix(&self.buf, 16).map_err(|_| self.err("illegal hex"))?;
        Ok(Hex(value))
    }

    /// Scan a quoted string.
    pub fn scan_quoted(&mut self) -> Result<String, Error> {
        self.buf.clear();

        let mut escape = false;

        while let Some((_, c)) = self.peek() {
            if escape {
                let o = match c {
                    '\\' => "\\",
                    '"' => "\"",
                    '\'' => "'",
                    't' => "\t",
                    'n' => "\n",
                    'r' => "\r",
                    '0' => "\0",
                    // TODO: support 7-bit character and unicode escapes.
                    o => return Err(self.err(format!("illegal escape character `{}`", o))),
                };

                self.step();
                self.buf.push_str(o);
                escape = false;
                continue;
            }

            match c {
                '"' => {
                    self.step();
                    break;
                }
                '\\' => {
                    self.step();
                    escape = true;
                    continue;
                }
                c => {
                    self.step();
                    self.buf.push(c);
                }
            }
        }

        if escape {
            return Err(self.err("escape sequence incomplete"));
        }

        Ok(self.buf.to_string())
    }

    /// Scan an identifier.
    pub fn scan_ident(&mut self) -> Result<String, Error> {
        self.buf.clear();

        while let Some((_, c)) = self.peek() {
            self.step();

            match c {
                ' ' => {
                    break;
                }
                c => {
                    self.buf.push(c);
                }
            }
        }

        Ok(self.buf.to_string())
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
        while let Some((s, c)) = self.peek() {
            match (c, self.peek2().map(|c| c.1)) {
                ('-', Some('>')) => {
                    let e = self.step_n(2);
                    return Some(Ok((s, Token::Rocket, e)));
                }
                ('-', _) => {
                    self.step();
                    let e = self.pos();
                    return Some(Ok((s, Token::Minus, e)));
                }
                ('+', _) => {
                    self.step();
                    let e = self.pos();
                    return Some(Ok((s, Token::Plus, e)));
                }
                ('0', Some('x')) => {
                    self.step_n(2);
                    let hex = try_iter!(self.scan_hex());
                    let e = self.pos();
                    return Some(Ok((s, Token::Hex(hex), e)));
                }
                _ => {}
            }

            match c {
                ' ' => {
                    self.step();
                    continue;
                }
                '"' => {
                    self.step();
                    let string = try_iter!(self.scan_quoted());
                    let e = self.pos();
                    return Some(Ok((s, Token::String(string), e)));
                }
                _ => {
                    let string = try_iter!(self.scan_ident());
                    let e = self.pos();
                    return Some(Ok((s, Token::String(string), e)));
                }
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::{Error, Hex, Lexer, Token};

    fn tokenize(input: &str) -> Result<Vec<(usize, Token, usize)>, Error> {
        Lexer::new(input).collect::<Result<Vec<_>, _>>()
    }

    #[test]
    fn basic_expression() -> Result<(), Error> {
        assert_eq!(
            vec![
                (0, Token::String(String::from("Steam.exe")), 11),
                (12, Token::Plus, 13),
                (14, Token::Hex(Hex(12)), 18),
                (19, Token::Rocket, 21),
                (22, Token::Minus, 23),
                (23, Token::Hex(Hex(8)), 26)
            ],
            tokenize("\"Steam.exe\" + 0x0C -> -0x8")?
        );

        Ok(())
    }
}
