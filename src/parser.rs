use crate::{Scanner, ScannerError, Token};
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;
use anyhow::anyhow;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("{0}")]
    Scanner(#[from] ScannerError),

    #[error("{0}")]
    AnyError(#[from] anyhow::Error),
    #[error("end of input expected. got {0:?}")]
    EOFExpected(Option<String>),
    #[error("expression expected. got {0:?}")]
    ExpressionExpected(Option<String>),
    #[error("')' expected. got {0:?}")]
    RightParenExpected(Option<String>),
}

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
pub enum ComparisonOp {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl fmt::Display for ComparisonOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            ComparisonOp::Less => "<",
            ComparisonOp::LessEqual => "<=",
            ComparisonOp::Greater => ">",
            ComparisonOp::GreaterEqual => ">=",
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EqualityOp {
    Equal,
    NotEqual,
}

impl fmt::Display for EqualityOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            EqualityOp::Equal => "==",
            EqualityOp::NotEqual => "!=",
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
    Comparison(ComparisonOp, Rc<Ast<'a>>, Rc<Ast<'a>>),
    Equality(EqualityOp, Rc<Ast<'a>>, Rc<Ast<'a>>),
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
            Ast::Comparison(op, lhs, rhs) => write!(f, "({} {} {})", op, lhs, rhs),
            Ast::Equality(op, lhs, rhs) => write!(f, "({} {} {})", op, lhs, rhs),
            Ast::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    current: Option<Result<Token<'a>, ScannerError>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            // input,
            scanner: Scanner::new(input),
            current: None,
        }
    }

    pub fn parse(&mut self) -> Result<Rc<Ast<'a>>, ParseError> {
        self.next();
        let result = self.parse_expression()?;
        match self.peek() {
            None => Ok(result),
            Some(Ok(t)) => Err(ParseError::EOFExpected(Some(t.lexeme().to_string()))),
            Some(Err(e)) => Err(ParseError::AnyError(e.into())),
        }
    }

    fn next(&mut self) -> Option<Result<&Token<'a>, ParseError>> {
        self.current = self.scanner.next();
        self.peek()
    }

    fn peek(&self) -> Option<Result<&Token<'a>, ParseError>> {
        match self.current.as_ref() {
            Some(Ok(bla)) => Some(Ok(bla)),
            Some(Err(e)) => Some(Err(ParseError::AnyError(anyhow!("{}", e.to_string())))),
            None => None,
        }
    }

    fn parse_expression(&mut self) -> Result<Rc<Ast<'a>>, ParseError> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Result<Rc<Ast<'a>>, ParseError> {
        let mut lhs = self.parse_relational()?;
        while let Some(true) = self.peek().map(|t| match t {
            Ok(tok) => {
                *tok == Token::EqualEqual || *tok == Token::BangEqual
            }
            _ => false
        }) {
            self.next();
            let op = self.peek().unwrap().copied()?;
            let rhs = self.parse_relational()?;
            let binary_op = match op {
                Token::EqualEqual => EqualityOp::Equal,
                Token::BangEqual => EqualityOp::NotEqual,
                _ => unreachable!()
            };
            lhs = Rc::new(Ast::Equality(binary_op, lhs, rhs));
        }
        Ok(lhs)
    }

    fn parse_relational(&mut self) -> Result<Rc<Ast<'a>>, ParseError> {
        let mut lhs = self.parse_add()?;
        while let Some(true) = self.peek().map(|t| match t {
            Ok(tok) => {
                *tok == Token::Less || *tok == Token::LessEqual || *tok == Token::Greater || *tok == Token::GreaterEqual
            }
            _ => false
        }) {
            let op = self.peek().unwrap().copied()?;
            let rhs = self.parse_add()?;
            let binary_op = match op {
                Token::Less => ComparisonOp::Less,
                Token::LessEqual => ComparisonOp::LessEqual,
                Token::Greater => ComparisonOp::Greater,
                Token::GreaterEqual => ComparisonOp::GreaterEqual,
                _ => unreachable!()
            };
            lhs = Rc::new(Ast::Comparison(binary_op, lhs, rhs));
        }
        Ok(lhs)
    }


    fn parse_add(&mut self) -> Result<Rc<Ast<'a>>, ParseError> {
        let mut lhs = self.parse_mul()?;
        while let Some(true) = self.peek().map(|t| match t {
            Ok(tok) => {
                *tok == Token::Plus || *tok == Token::Minus
            }
            _ => false
        }) {
            self.next();
            let op = self.peek().unwrap().copied()?;
            let rhs = self.parse_mul()?;
            let binary_op = match op {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => unreachable!()
            };
            lhs = Rc::new(Ast::Binary(binary_op, lhs, rhs));
        }
        Ok(lhs)
    }

    fn parse_mul(&mut self) -> Result<Rc<Ast<'a>>, ParseError> {
        let mut lhs = self.parse_terminal_or_group()?;
        while let Some(true) = self.peek().map(|t| match t {
            Ok(tok) => {
                *tok == Token::Star || *tok == Token::Slash
            }
            _ => false
        }) {
            let op = self.next().unwrap().copied()?;
            let rhs = self.parse_terminal_or_group()?;
            let binary_op = match op {
                Token::Star => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
                _ => unreachable!()
            };
            lhs = Rc::new(Ast::Binary(binary_op, lhs, rhs));
        }
        Ok(lhs)
    }

    fn parse_terminal_or_group(&mut self) -> Result<Rc<Ast<'a>>, ParseError> {
        let tok = match self.peek() {
            Some(Ok(Token::True)) => {
                self.next();
                Ok(Ast::Boolean(true))
            }
            Some(Ok(Token::False)) => {
                self.next();
                Ok(Ast::Boolean(false))
            }
            Some(Ok(Token::Nil)) => {
                self.next();
                Ok(Ast::Nil)
            }
            Some(Ok(Token::Number(s))) => {
                let res = Ok(Ast::Number(s.parse::<f64>().unwrap()));
                self.next();
                res
            }
            Some(Ok(Token::String(s))) => {
                let res = Ok(Ast::String(s));
                self.next();
                res
            }
            Some(Ok(Token::LeftParen)) => {
                self.next();
                let inner = self.parse_expression()?;
                match self.peek() {
                    Some(Ok(Token::RightParen)) => {
                        self.next();
                        Ok(Ast::Group(inner))
                    }
                    Some(Ok(t)) => {
                        Err(ParseError::RightParenExpected(Some(t.lexeme().to_string())))
                    }
                    Some(Err(e)) => Err(ParseError::AnyError(e.into())),
                    None => {
                        Err(ParseError::RightParenExpected(None))
                    }
                }
            }
            Some(Ok(Token::Bang)) => {
                self.next();
                let rhs = self.parse_terminal_or_group()?;
                Ok(Ast::Unary(UnaryOp::Not, rhs))
            }
            Some(Ok(Token::Minus)) => {
                self.next();
                let rhs = self.parse_terminal_or_group()?;
                Ok(Ast::Unary(UnaryOp::Neg, rhs))
            }
            Some(Ok(t)) => Err(ParseError::ExpressionExpected(Some(t.lexeme().to_string()))),
            Some(Err(e)) =>
                Err(ParseError::AnyError(e.into())),
            None => Err(ParseError::ExpressionExpected(None)),
        };
        tok.map(Rc::new)
    }
}
