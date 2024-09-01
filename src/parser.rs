use crate::{Scanner, Token};
use anyhow::anyhow;
use std::fmt;
use std::fmt::Formatter;
use std::iter::Peekable;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            UnaryOp::Neg => '-',
            UnaryOp::Not => '!',
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            BinaryOp::Add => '+',
            BinaryOp::Sub => '-',
            BinaryOp::Mul => '*',
            BinaryOp::Div => '/',
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ast<'a> {
    Boolean(bool),
    Number(f64),
    String(&'a str),
    Group(Rc<Ast<'a>>),
    Unary(UnaryOp, Rc<Ast<'a>>),
    Binary(BinaryOp, Rc<Ast<'a>>, Rc<Ast<'a>>),
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
            Ast::Unary(op, rhs) => write!(f, "({op} {rhs})"),
            Ast::Binary(op, lhs, rhs) => write!(f, "({} {} {})", op, lhs, rhs),
            Ast::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    // input: &'a str,
    scanner: Peekable<Scanner<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            // input,
            scanner: Scanner::new(input).peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Rc<Ast<'a>>, anyhow::Error> {
        let result = self.parse_mul()?;
        match self.scanner.peek() {
            None => Ok(result),
            Some(Ok(t)) => Err(anyhow!("invalid token: {}. end of input expected.", t)),
            _ => Err(anyhow!("end of input expected")),
        }
    }

    fn parse_mul(&mut self) -> Result<Rc<Ast<'a>>, anyhow::Error> {
        let mut lhs = self.parse_terminal_or_group()?;
        while let Some(true) = self.scanner.peek().map(|t| match t {
            Ok(tok) => {
                *tok == Token::Star || *tok == Token::Slash
            }
            _ => false
        }) {
            let op = self.scanner.next().unwrap()?;
            let rhs = self.parse_mul()?;
            let binary_op = match op {
                Token::Star => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
                _ => unreachable!()
            };
            lhs = Rc::new(Ast::Binary(binary_op, lhs, rhs));
        }
        Ok(lhs)
    }

    fn parse_terminal_or_group(&mut self) -> Result<Rc<Ast<'a>>, anyhow::Error> {
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
                let inner = self.parse_mul()?;
                match self.scanner.peek() {
                    Some(Ok(Token::RightParen)) => {
                        self.scanner.next();
                        Ok(Ast::Group(inner))
                    }
                    _ => {
                        Err(anyhow!("Expected ')'"))
                    }
                }
            }
            Some(Ok(Token::Bang)) => {
                self.scanner.next();
                let rhs = self.parse_terminal_or_group()?;
                Ok(Ast::Unary(UnaryOp::Not, rhs))
            }
            Some(Ok(Token::Minus)) => {
                self.scanner.next();
                let rhs = self.parse_terminal_or_group()?;
                Ok(Ast::Unary(UnaryOp::Neg, rhs))
            }
            Some(Ok(t)) => Err(anyhow!("invalid token: {}", t)),
            Some(Err(_)) => {
                match self.scanner.next() {
                    Some(Err(e)) => Err(e),
                    _ => panic!("bla"),
                }
            }
            None => Err(anyhow!("empty input")),
        };
        tok.map(Rc::new)
    }
}
