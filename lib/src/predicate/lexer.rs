use crate::scan::{Type, Value};
use std::{borrow::Cow, fmt, str};

#[derive(Debug, err_derive::Error)]
#[error(display = "{}", description)]
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
    /// $value, references the value in memory.
    Value,
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
    /// A literal value, like `0x42`, or `128u32`.
    Literal(Value),
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

    /// Peek all characters in lookahead buffers.
    pub fn peek3_chars(&self) -> (Option<char>, Option<char>, Option<char>) {
        (
            self.c1.as_ref().map(|(_, c)| *c),
            self.c2.as_ref().map(|(_, c)| *c),
            self.c3.as_ref().map(|(_, c)| *c),
        )
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

    /// Scan a literal type.
    pub fn scan_type(&mut self) -> Result<Type, Error> {
        let result = match self.peek().map(|(_, c)| c) {
            // Unsigned type.
            Some('u') => {
                self.step();

                match self.peek3_chars() {
                    (Some('8'), _, _) => Some((1, Type::U8)),
                    (Some('1'), Some('6'), _) => Some((2, Type::U16)),
                    (Some('3'), Some('2'), _) => Some((2, Type::U32)),
                    (Some('6'), Some('4'), _) => Some((2, Type::U64)),
                    (Some('1'), Some('2'), Some('8')) => Some((3, Type::U128)),
                    _ => None,
                }
            }
            // Signed type.
            Some('i') => {
                self.step();

                match self.peek3_chars() {
                    (Some('8'), _, _) => Some((1, Type::I8)),
                    (Some('1'), Some('6'), _) => Some((2, Type::I16)),
                    (Some('3'), Some('2'), _) => Some((2, Type::I32)),
                    (Some('6'), Some('4'), _) => Some((2, Type::I64)),
                    (Some('1'), Some('2'), Some('8')) => Some((3, Type::I128)),
                    _ => None,
                }
            }
            _ => None,
        };

        if let Some((w, ty)) = result {
            self.step_n(w);
            return Ok(ty);
        }

        return Err(self.err("expected type"));
    }

    /// Scan an identifier.
    pub fn scan_literal(&mut self) -> Result<Value, Error> {
        self.buf.clear();

        let mut hex = false;
        let mut ty = Type::I32;

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
                // type break.
                'u' | 'i' => {
                    ty = self.scan_type()?;
                    break;
                }
                '0'..='9' => {
                    self.step();
                    self.buf.push(c);
                }
                _ => return Err(self.err("unexpected character in identifier")),
            }
        }

        let result = if hex {
            ty.parse_hex(&self.buf)
        } else {
            ty.parse(&self.buf)
        };

        match result {
            Ok(value) => Ok(value),
            Err(_) => Err(self.err("invalid literal")),
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
                'a'..='z' => {
                    let ident = try_iter!(self.scan_ident());
                    let e = self.pos();

                    match ident.as_str() {
                        "and" => return Some(Ok((s, Token::And, e))),
                        "or" => return Some(Ok((s, Token::Or, e))),
                        other => {
                            return Some(Err(self.err(format!("unsupported keyword `{}`", other))));
                        }
                    }
                }
                '$' => {
                    self.step();
                    let ident = try_iter!(self.scan_ident());
                    let e = self.pos();

                    match ident.as_str() {
                        "value" => return Some(Ok((s, Token::Value, e))),
                        other => {
                            return Some(
                                Err(self.err(format!("unsupported identifier `{}`", other))),
                            );
                        }
                    }
                }
                '0'..='9' => {
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
    use crate::scan::Value;

    fn tokenize(input: &str) -> Result<Vec<(usize, Token, usize)>, Error> {
        Lexer::new(input).collect::<Result<Vec<_>, _>>()
    }

    #[test]
    fn basic_expression() -> Result<(), Error> {
        assert_eq!(
            vec![
                (0, Token::Value, 6),
                (7, Token::Eq, 9),
                (10, Token::Literal(Value::I32(42)), 12)
            ],
            tokenize("$value == 42")?
        );

        assert_eq!(
            vec![(0, Token::Literal(Value::I32(66)), 4)],
            tokenize("0x42")?
        );

        assert_eq!(
            vec![
                (0, Token::Literal(Value::U128(66)), 8),
                (9, Token::Literal(Value::I128(83)), 15)
            ],
            tokenize("0x42u128 83i128")?
        );

        Ok(())
    }
}
