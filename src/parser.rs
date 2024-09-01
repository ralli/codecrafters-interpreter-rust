use crate::{Scanner, Token};
use anyhow::anyhow;
use std::fmt;
use std::fmt::Formatter;
use std::iter::Peekable;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Ast<'a> {
    Boolean(bool),
    Number(f64),
    String(&'a str),
    Group(Rc<Ast<'a>>),
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
            Ast::Group(inner) => write!(f, "(group {inner})"),
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

    pub fn parse(&mut self) -> Result<Ast<'a>, anyhow::Error> {
        let result = self.parse_terminal_or_group()?;
        match self.scanner.peek() {
            None => Ok(result),
            Some(Ok(t)) => Err(anyhow!("invalid token: {}. end of input expected.", t)),
            _ => Err(anyhow!("end of input expected")),
        }
    }

    fn parse_terminal_or_group(&mut self) -> Result<Ast<'a>, anyhow::Error> {
        let tok = match self.scanner.peek() {
            Some(Ok(Token::True)) => {
                self.scanner.next();
                Ok(Ast::Boolean(true))
            }
            Some(Ok(Token::False)) => {
                self.scanner.next();
                Ok(Ast::Boolean(false))
            }
            Some(Ok(Token::Nil)) => {
                self.scanner.next();
                Ok(Ast::Nil)
            }
            Some(Ok(Token::Number(s))) => {
                let res = Ok(Ast::Number(s.parse::<f64>().unwrap()));
                self.scanner.next();
                res
            }
            Some(Ok(Token::String(s))) => {
                let res = Ok(Ast::String(s));
                self.scanner.next();
                res
            }
            Some(Ok(Token::LeftParen)) => {
                self.scanner.next();
                let inner = self.parse_terminal_or_group()?;
                match self.scanner.peek() {
                    Some(Ok(Token::RightParen)) => {
                        self.scanner.next();
                        Ok(Ast::Group(Rc::new(inner)))
                    }
                    _ => {
                        Err(anyhow!("Expected ')'"))
                    }
                }
            }
            Some(Ok(t)) => Err(anyhow!("invalid token: {}", t)),
            Some(Err(e)) => {
                match self.scanner.next() {
                    Some(Err(e)) => Err(e),
                    _ => panic!("bla"),
                }
            }
            None => Err(anyhow!("empty input")),
        };
        tok
    }
}
