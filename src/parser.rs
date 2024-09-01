use crate::{Scanner, Token};
use anyhow::anyhow;
use std::fmt;
use std::fmt::Formatter;
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq)]
pub enum Ast<'a> {
    Boolean(bool),
    Number(f64),
    String(&'a str),
    Nil,
}

impl<'a> fmt::Display for Ast<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Ast::Boolean(v) => write!(f, "{}", v),
            Ast::Number(x) => {
                if x.trunc() == *x {
                    write!(f, "{:.1}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
            Ast::String(s) => write!(f, "{s}"),
            Ast::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    input: &'a str,
    scanner: Peekable<Scanner<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            scanner: Scanner::new(input).peekable(),
        }
    }

    pub fn parse(mut self) -> Result<Ast<'a>, anyhow::Error> {
        match self.scanner.next() {
            Some(Ok(Token::True)) => Ok(Ast::Boolean(true)),
            Some(Ok(Token::False)) => Ok(Ast::Boolean(false)),
            Some(Ok(Token::Nil)) => Ok(Ast::Nil),
            Some(Ok(Token::Number(s))) => Ok(Ast::Number(s.parse::<f64>().unwrap())),
            Some(Ok(Token::String(s))) => Ok(Ast::String(s)),
            Some(Err(e)) => Err(e),
            Some(Ok(t)) => Err(anyhow!("invalid token: {}", t)),
            None => Err(anyhow!("empty input")),
        }
    }
}
