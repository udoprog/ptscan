//! Recursive descent parser for portable pointers:
//!
//! ```text
//! pointer = base ("->" deref)*
//! base    = (hex | name) (("+" | "-") hex)*
//! deref   = ("+" | "-")? hex
//! ```

use crate::{
    pointer::{
        lexer::{Error, Hex, Lexer, Token},
        PortableBase, PortablePointer,
    },
    Offset,
};

pub fn parse(input: &str) -> Result<PortablePointer, Error> {
    let tokens = Lexer::new(input).collect::<Result<Vec<_>, _>>()?;

    let mut parser = Parser {
        tokens,
        index: 0,
        end: input.len(),
    };

    parser.pointer()
}

struct Parser {
    tokens: Vec<(usize, Token, usize)>,
    index: usize,
    end: usize,
}

impl Parser {
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index).map(|(_, t, _)| t)
    }

    fn next(&mut self) -> Option<Token> {
        let (_, token, _) = self.tokens.get(self.index)?.clone();
        self.index += 1;
        Some(token)
    }

    fn unexpected(&self, expected: &str) -> Error {
        match self.tokens.get(self.index) {
            Some((pos, token, _)) => {
                Error::new(format!("expected {}, found {}", expected, token), *pos)
            }
            None => Error::new(
                format!("expected {}, found end of input", expected),
                self.end,
            ),
        }
    }

    fn hex(&mut self) -> Result<Hex, Error> {
        if let Some((_, Token::Hex(hex), _)) = self.tokens.get(self.index) {
            let hex = hex.clone();
            self.index += 1;
            return Ok(hex);
        }

        Err(self.unexpected("hex number"))
    }

    fn pointer(&mut self) -> Result<PortablePointer, Error> {
        let base = self.base()?;
        let mut offsets = Vec::new();

        while let Some(Token::Rocket) = self.peek() {
            self.next();
            offsets.push(self.deref()?.into_offset());
        }

        if self.peek().is_some() {
            return Err(self.unexpected(if offsets.is_empty() {
                "`+`, `-`, `->` or end of input"
            } else {
                "`->` or end of input"
            }));
        }

        Ok(PortablePointer { base, offsets })
    }

    fn base(&mut self) -> Result<PortableBase, Error> {
        let base = match self.peek() {
            Some(Token::Hex(..) | Token::String(..)) => self.next(),
            _ => return Err(self.unexpected("hex address or module name")),
        };

        match base {
            Some(Token::Hex(address)) => {
                let mut address = address.into_address();

                while let Some(o) = self.base_offset()? {
                    address = address.saturating_offset(o);
                }

                Ok(PortableBase::Address { address })
            }
            Some(Token::String(name)) => {
                let mut offset = Offset::zero();

                while let Some(o) = self.base_offset()? {
                    offset = offset.saturating_add(o);
                }

                Ok(PortableBase::Module { name, offset })
            }
            _ => unreachable!(),
        }
    }

    fn base_offset(&mut self) -> Result<Option<Offset>, Error> {
        match self.peek() {
            Some(Token::Plus) => {
                self.next();
                Ok(Some(self.hex()?.into_offset()))
            }
            Some(Token::Minus) => {
                self.next();
                Ok(Some(self.hex()?.negate().into_offset()))
            }
            _ => Ok(None),
        }
    }

    fn deref(&mut self) -> Result<Hex, Error> {
        match self.peek() {
            Some(Token::Plus) => {
                self.next();
                self.hex()
            }
            Some(Token::Minus) => {
                self.next();
                Ok(self.hex()?.negate())
            }
            _ => self
                .hex()
                .map_err(|_| self.unexpected("`+`, `-` or hex number")),
        }
    }
}
