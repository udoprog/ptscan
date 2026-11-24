//! Recursive descent parser for filter and value expressions:
//!
//! ```text
//! filter  = all ("or" all)*
//! all     = test ("and" test)*
//! test    = "(" filter ")"
//!         | value "is" "not"? (type | "nan")
//!         | value ("==" | "!=" | "<=" | ">=" | "<" | ">" | "^" | "~" | "!~") value
//! value   = factor (("+" | "-") factor)*
//! factor  = cast (("*" | "/") cast)*
//! cast    = address ("as" type)*
//! address = "(" value ")" | "*" address | "&" address | term
//! term    = "value" | "last" | "initial" | number | decimal | string | bytes
//! ```

use crate::{
    filter_expr::{
        ast::FilterExpr,
        lexer::{Error, Lexer, Token},
        FilterOp,
    },
    value_expr::{ast::ValueExpr, ValueOp},
    Type,
};

/// Bounds recursion so that deeply nested input fails instead of overflowing
/// the stack.
const MAX_DEPTH: usize = 256;

pub fn parse_filter(input: &str) -> Result<FilterExpr, Error> {
    let mut parser = Parser::new(input)?;
    let expr = parser.filter()?;
    parser.end("`and`, `or` or end of input")?;
    Ok(expr)
}

pub fn parse_value(input: &str) -> Result<ValueExpr, Error> {
    let mut parser = Parser::new(input)?;
    let expr = parser.value()?;
    parser.end("`+`, `-`, `*`, `/`, `as` or end of input")?;
    Ok(expr)
}

struct Parser<'a> {
    input: &'a str,
    tokens: Vec<(usize, Token, usize)>,
    index: usize,
    depth: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Result<Self, Error> {
        Ok(Self {
            input,
            tokens: Lexer::new(input).collect::<Result<Vec<_>, _>>()?,
            index: 0,
            depth: 0,
        })
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index).map(|(_, t, _)| t)
    }

    fn next(&mut self) -> Option<Token> {
        let (_, token, _) = self.tokens.get(self.index)?.clone();
        self.index += 1;
        Some(token)
    }

    fn eat(&mut self, token: &Token) -> bool {
        if self.peek() == Some(token) {
            self.index += 1;
            true
        } else {
            false
        }
    }

    fn pos(&self) -> usize {
        self.tokens
            .get(self.index)
            .map(|(s, _, _)| *s)
            .unwrap_or(self.input.len())
    }

    fn unexpected(&self, expected: &str) -> Error {
        match self.tokens.get(self.index) {
            Some((s, _, e)) => Error::new(
                format!("expected {}, found `{}`", expected, &self.input[*s..*e]),
                *s,
            ),
            None => Error::new(
                format!("expected {}, found end of input", expected),
                self.input.len(),
            ),
        }
    }

    fn end(&self, expected: &str) -> Result<(), Error> {
        match self.peek() {
            None => Ok(()),
            Some(..) => Err(self.unexpected(expected)),
        }
    }

    fn expect(&mut self, token: &Token, expected: &str) -> Result<(), Error> {
        if self.eat(token) {
            Ok(())
        } else {
            Err(self.unexpected(expected))
        }
    }

    fn enter(&mut self) -> Result<(), Error> {
        if self.depth == MAX_DEPTH {
            return Err(Error::new("expression is nested too deeply", self.pos()));
        }

        self.depth += 1;
        Ok(())
    }

    fn ty(&mut self) -> Result<Type, Error> {
        if let Some(Token::Type(ty)) = self.peek() {
            let ty = *ty;
            self.index += 1;
            return Ok(ty);
        }

        Err(self.unexpected("type"))
    }

    fn filter(&mut self) -> Result<FilterExpr, Error> {
        let first = self.all()?;

        if self.peek() != Some(&Token::Or) {
            return Ok(first);
        }

        let mut or = vec![first];

        while self.eat(&Token::Or) {
            or.push(self.all()?);
        }

        Ok(FilterExpr::Or(or))
    }

    fn all(&mut self) -> Result<FilterExpr, Error> {
        let first = self.test()?;

        if self.peek() != Some(&Token::And) {
            return Ok(first);
        }

        let mut and = vec![first];

        while self.eat(&Token::And) {
            and.push(self.test()?);
        }

        Ok(FilterExpr::And(and))
    }

    fn test(&mut self) -> Result<FilterExpr, Error> {
        self.enter()?;
        let result = self.test_inner();
        self.depth -= 1;
        result
    }

    fn test_inner(&mut self) -> Result<FilterExpr, Error> {
        if self.peek() != Some(&Token::OpenParen) {
            return self.comparison();
        }

        // A `(` opens either a grouped filter or a grouped value. Only a filter
        // contains a comparison, so at most one of the two parses succeeds.
        let start = self.index;
        self.index += 1;

        let grouped = self.filter().and_then(|filter| {
            self.expect(&Token::CloseParen, "`and`, `or` or `)`")?;
            Ok(filter)
        });

        let grouped_error = match grouped {
            Ok(filter) => return Ok(filter),
            Err(error) => error,
        };

        self.index = start;

        match self.comparison() {
            Ok(filter) => Ok(filter),
            Err(error) if error.pos() >= grouped_error.pos() => Err(error),
            Err(..) => Err(grouped_error),
        }
    }

    fn comparison(&mut self) -> Result<FilterExpr, Error> {
        let lhs = self.value()?;

        let op = match self.peek() {
            Some(Token::Is) => {
                self.next();
                let not = self.eat(&Token::Not);

                let filter = if self.eat(&Token::Nan) {
                    FilterExpr::IsNan(lhs)
                } else {
                    let expected = if not {
                        "type or `nan`"
                    } else {
                        "type, `not` or `nan`"
                    };
                    let ty = self.ty().map_err(|_| self.unexpected(expected))?;
                    FilterExpr::IsType(lhs, ty)
                };

                return Ok(if not {
                    FilterExpr::Not(Box::new(filter))
                } else {
                    filter
                });
            }
            Some(Token::Tilde) => {
                self.next();
                return Ok(FilterExpr::Regex(lhs, self.value()?));
            }
            Some(Token::NotTilde) => {
                self.next();
                let regex = FilterExpr::Regex(lhs, self.value()?);
                return Ok(FilterExpr::Not(Box::new(regex)));
            }
            Some(Token::Eq) => FilterOp::Eq,
            Some(Token::Neq) => FilterOp::Neq,
            Some(Token::Lte) => FilterOp::Lte,
            Some(Token::Gte) => FilterOp::Gte,
            Some(Token::Lt) => FilterOp::Lt,
            Some(Token::Gt) => FilterOp::Gt,
            Some(Token::Caret) => FilterOp::StartsWith,
            _ => return Err(self.unexpected("`is` or a comparison operator")),
        };

        self.next();
        Ok(FilterExpr::Binary(op, lhs, self.value()?))
    }

    fn value(&mut self) -> Result<ValueExpr, Error> {
        let mut lhs = self.factor()?;

        loop {
            let op = match self.peek() {
                Some(Token::Plus) => ValueOp::Add,
                Some(Token::Minus) => ValueOp::Sub,
                _ => return Ok(lhs),
            };

            self.next();
            lhs = ValueExpr::Binary(op, Box::new(lhs), Box::new(self.factor()?));
        }
    }

    fn factor(&mut self) -> Result<ValueExpr, Error> {
        let mut lhs = self.cast()?;

        loop {
            let op = match self.peek() {
                Some(Token::Star) => ValueOp::Mul,
                Some(Token::Slash) => ValueOp::Div,
                _ => return Ok(lhs),
            };

            self.next();
            lhs = ValueExpr::Binary(op, Box::new(lhs), Box::new(self.cast()?));
        }
    }

    fn cast(&mut self) -> Result<ValueExpr, Error> {
        let mut expr = self.address()?;

        while self.eat(&Token::As) {
            expr = ValueExpr::Cast(Box::new(expr), self.ty()?);
        }

        Ok(expr)
    }

    fn address(&mut self) -> Result<ValueExpr, Error> {
        self.enter()?;
        let result = self.address_inner();
        self.depth -= 1;
        result
    }

    fn address_inner(&mut self) -> Result<ValueExpr, Error> {
        let expr = match self.peek() {
            Some(Token::OpenParen) => {
                self.next();
                let value = self.value()?;
                self.expect(&Token::CloseParen, "operator or `)`")?;
                return Ok(value);
            }
            Some(Token::Star) => {
                self.next();
                return Ok(ValueExpr::Deref(Box::new(self.address()?)));
            }
            Some(Token::AddressOf) => {
                self.next();
                return Ok(ValueExpr::AddressOf(Box::new(self.address()?)));
            }
            Some(Token::Value) => ValueExpr::Value,
            Some(Token::Last) => ValueExpr::Last,
            Some(Token::Initial) => ValueExpr::Initial,
            Some(Token::Number(number, ty)) => ValueExpr::Number(number.clone(), *ty),
            Some(Token::Decimal(decimal, ty)) => ValueExpr::Decimal(decimal.clone(), *ty),
            Some(Token::String(string)) => ValueExpr::String(string.clone()),
            Some(Token::Bytes(bytes)) => ValueExpr::Bytes(bytes.clone()),
            _ => return Err(self.unexpected("value")),
        };

        self.next();
        Ok(expr)
    }
}
