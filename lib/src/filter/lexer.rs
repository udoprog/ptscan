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
    /// changed keyword.
    Changed,
    /// same keyword.
    Same,
    /// Value increased.
    Inc,
    /// Value decreased.
    Dec,
    /// `==`.
    Eq,
    /// `!=`.
    Neq,
    /// `<=`.
    Lte,
    /// `>=`.
    Gte,
    /// `<`.
    Lt,
    /// `>`.
    Gt,
    /// `(`.
    OpenParen,
    /// `)`.
    CloseParen,
    /// A literal value, like `0x42`, or `128u32`.
    Literal(BigInt),
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
            .unwrap_or(self.input.len())
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

    /// Get the current position of the iterator.
    /// Used for error handling.
    pub fn pos(&self) -> usize {
        self.c1
            .as_ref()
            .map(|(pos, _)| *pos)
            .unwrap_or(self.input.len())
    }

    /// Format an error with a correct description and position.
    pub fn err(&self, description: impl Into<Cow<'static, str>>) -> Error {
        Error {
            description: description.into(),
            pos: self.pos(),
        }
    }

    /// Scan an identifier.
    pub fn scan_ident(&mut self) -> Result<String, Error> {
        self.buf.clear();

        loop {
            let (_, c) = match self.peek() {
                Some(c) => c,
                None => break,
            };

            match c {
                'a'..='z' => {
                    self.step();
                    self.buf.push(c);
                    continue;
                }
                _ => break,
            }
        }

        Ok(self.buf.to_string())
    }

    /// Scan an identifier.
    pub fn scan_literal(&mut self) -> Result<BigInt, Error> {
        use num::Num;

        self.buf.clear();

        let mut hex = false;

        // test if we have a hex prefix
        if let Some((_, '0', 'x')) = self.peek2() {
            hex = true;
            self.step_n(2);
        }

        loop {
            let (_, c) = match self.peek() {
                Some(c) => c,
                None => break,
            };

            match c {
                ' ' => break,
                '0'..='9' => {
                    self.step();
                    self.buf.push(c);
                }
                _ => return Err(self.err("unexpected character in identifier")),
            }
        }

        let result = if hex {
            BigInt::from_str_radix(&self.buf, 16)
        } else {
            BigInt::from_str_radix(&self.buf, 10)
        };

        Ok(result.map_err(|e| self.err(e.to_string()))?)
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
                'a'..='z' => {
                    let ident = try_iter!(self.scan_ident());
                    let e = self.pos();

                    match ident.as_str() {
                        "value" => return Some(Ok((s, Token::Value, e))),
                        "and" => return Some(Ok((s, Token::And, e))),
                        "or" => return Some(Ok((s, Token::Or, e))),
                        "same" => return Some(Ok((s, Token::Same, e))),
                        "changed" => return Some(Ok((s, Token::Changed, e))),
                        "inc" => return Some(Ok((s, Token::Inc, e))),
                        "dec" => return Some(Ok((s, Token::Dec, e))),
                        other => {
                            return Some(Err(self.err(format!("unsupported keyword `{}`", other))));
                        }
                    }
                }
                '-' | '+' | '0'..='9' => {
                    let literal = try_iter!(self.scan_literal());
                    let e = self.pos();
                    return Some(Ok((s, Token::Literal(literal), e)));
                }
                _ => return Some(Err(self.err("unsupported character"))),
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
                (9, Token::Literal(BigInt::from(42)), 11)
            ],
            tokenize("value == 42")?
        );

        assert_eq!(
            vec![(0, Token::Literal(BigInt::from(66)), 4)],
            tokenize("0x42")?
        );

        assert_eq!(
            vec![
                (0, Token::Literal(BigInt::from(66)), 4),
                (5, Token::Literal(BigInt::from(83)), 7)
            ],
            tokenize("0x42 83")?
        );

        Ok(())
    }
}
