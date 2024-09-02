use crate::{Scanner, ScannerError, Token};
use anyhow::anyhow;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("{0}")]
    AnyError(anyhow::Error),

    #[error("[line {0}] end of input expected. got {1}")]
    EOFExpected(usize, String),

    #[error("[line {0}] expression expected. got {1}")]
    ExpressionExpected(usize, String),

    #[error("[line {0}] ')' expected. got {1}")]
    RightParenExpected(usize, String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOp::Neg => '-',
                UnaryOp::Not => '!',
            }
        )
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
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Add => '+',
                BinaryOp::Sub => '-',
                BinaryOp::Mul => '*',
                BinaryOp::Div => '/',
            }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ComparisonOp {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl fmt::Display for ComparisonOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ComparisonOp::Less => "<",
                ComparisonOp::LessEqual => "<=",
                ComparisonOp::Greater => ">",
                ComparisonOp::GreaterEqual => ">=",
            }
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum EqualityOp {
    Equal,
    NotEqual,
}

impl fmt::Display for EqualityOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                EqualityOp::Equal => "==",
                EqualityOp::NotEqual => "!=",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Number(f64),
    String(String),
    Nil,
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Bool(_) => "boolean",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Nil => "nil",
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{b}"),
            Value::Number(x) => write!(f, "{x}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Nil => write!(f, "nil"),
        }
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

impl<'a> Ast<'a> {
    pub fn eval(&self) -> Result<Value, anyhow::Error> {
        match self {
            Ast::Boolean(b) => Ok(Value::Bool(*b)),
            Ast::Number(x) => Ok(Value::Number(*x)),
            Ast::String(s) => Ok(Value::String(s.to_string())),
            Ast::Group(inner) => inner.eval(),
            Ast::Unary(op, right) => match op {
                UnaryOp::Not => match right.eval()? {
                    Value::Bool(b) => Ok(Value::Bool(!b)),
                    Value::Nil => Ok(Value::Bool(true)),
                    _ => Ok(Value::Bool(false)),
                },
                UnaryOp::Neg => match right.eval()? {
                    Value::Number(x) => Ok(Value::Number(-x)),
                    _ => Err(anyhow!("Operand must be a number.")),
                },
            },
            Ast::Binary(op, left, right) => match (left.eval()?, right.eval()?) {
                (Value::Number(a), Value::Number(b)) => match op {
                    BinaryOp::Add => Ok(Value::Number(a + b)),
                    BinaryOp::Sub => Ok(Value::Number(a - b)),
                    BinaryOp::Mul => Ok(Value::Number(a * b)),
                    BinaryOp::Div => Ok(Value::Number(a / b)),
                },
                (Value::String(a), Value::String(b)) => match op {
                    BinaryOp::Add => Ok(Value::String(format!("{a}{b}"))),
                    op => Err(anyhow!("operation {op} not implemented for strings")),
                },
                (l, r) => Err(anyhow!(
                    "incompatible types for {op} {} {}",
                    l.type_name(),
                    r.type_name()
                )),
            },
            Ast::Comparison(op, left, right) => match (left.eval()?, right.eval()?) {
                (Value::Number(a), Value::Number(b)) => Ast::compare(*op, a, b),
                // (Value::Bool(a), Value::Bool(b)) => Ast::compare(*op, a, b),
                // (Value::String(a), Value::String(b)) => Ast::compare(*op, a.as_str(), b.as_str()),
                _ => Err(anyhow!("Operands must be numbers.")),
            },
            Ast::Equality(op, left, right) => match (left.eval()?, right.eval()?) {
                (Value::Number(a), Value::Number(b)) => Ast::equal_to(*op, a, b),
                (Value::Bool(a), Value::Bool(b)) => Ast::equal_to(*op, a, b),
                (Value::String(a), Value::String(b)) => Ast::equal_to(*op, a.as_str(), b.as_str()),
                (Value::Nil, Value::Nil) => Ok(Value::Bool(true)),
                _ => Ok(Value::Bool(false)),
            },
            Ast::Nil => Ok(Value::Nil),
        }
    }
    fn equal_to<T: PartialEq>(op: EqualityOp, a: T, b: T) -> Result<Value, anyhow::Error> {
        match op {
            EqualityOp::Equal => Ok(Value::Bool(a == b)),
            EqualityOp::NotEqual => Ok(Value::Bool(a != b)),
        }
    }

    fn compare<T: PartialOrd>(op: ComparisonOp, a: T, b: T) -> Result<Value, anyhow::Error> {
        match op {
            ComparisonOp::Less => Ok(Value::Bool(a < b)),
            ComparisonOp::LessEqual => Ok(Value::Bool(a <= b)),
            ComparisonOp::Greater => Ok(Value::Bool(a > b)),
            ComparisonOp::GreaterEqual => Ok(Value::Bool(a >= b)),
        }
    }
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
    line: usize,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            // input,
            scanner: Scanner::new(input),
            current: None,
            line: 1,
        }
    }

    pub fn parse(&mut self) -> Result<Rc<Ast<'a>>, ParseError> {
        self.next();
        let result = self.parse_expression()?;
        match self.peek() {
            None => Ok(result),
            Some(Ok(t)) => Err(ParseError::EOFExpected(self.line, t.lexeme().into())),
            Some(Err(e)) => Err(e),
        }
    }

    fn next(&mut self) -> Option<Result<&Token<'a>, ParseError>> {
        self.line = self.scanner.current_line();
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
            Ok(tok) => *tok == Token::EqualEqual || *tok == Token::BangEqual,
            _ => false,
        }) {
            let op = self.peek().unwrap().copied()?;
            self.next();
            let rhs = self.parse_relational()?;
            let binary_op = match op {
                Token::EqualEqual => EqualityOp::Equal,
                Token::BangEqual => EqualityOp::NotEqual,
                _ => unreachable!(),
            };
            lhs = Rc::new(Ast::Equality(binary_op, lhs, rhs));
        }
        Ok(lhs)
    }

    fn parse_relational(&mut self) -> Result<Rc<Ast<'a>>, ParseError> {
        let mut lhs = self.parse_add()?;
        while let Some(true) = self.peek().map(|t| match t {
            Ok(tok) => {
                *tok == Token::Less
                    || *tok == Token::LessEqual
                    || *tok == Token::Greater
                    || *tok == Token::GreaterEqual
            }
            _ => false,
        }) {
            let op = self.peek().unwrap().copied()?;
            self.next();
            let rhs = self.parse_add()?;
            let binary_op = match op {
                Token::Less => ComparisonOp::Less,
                Token::LessEqual => ComparisonOp::LessEqual,
                Token::Greater => ComparisonOp::Greater,
                Token::GreaterEqual => ComparisonOp::GreaterEqual,
                _ => unreachable!(),
            };
            lhs = Rc::new(Ast::Comparison(binary_op, lhs, rhs));
        }
        Ok(lhs)
    }

    fn parse_add(&mut self) -> Result<Rc<Ast<'a>>, ParseError> {
        let mut lhs = self.parse_mul()?;
        while let Some(true) = self.peek().map(|t| match t {
            Ok(tok) => *tok == Token::Plus || *tok == Token::Minus,
            _ => false,
        }) {
            let op = self.peek().unwrap().copied()?;
            self.next();
            let rhs = self.parse_mul()?;
            let binary_op = match op {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            };
            lhs = Rc::new(Ast::Binary(binary_op, lhs, rhs));
        }
        Ok(lhs)
    }

    fn parse_mul(&mut self) -> Result<Rc<Ast<'a>>, ParseError> {
        let mut lhs = self.parse_terminal_or_group()?;
        while let Some(true) = self.peek().map(|t| match t {
            Ok(tok) => *tok == Token::Star || *tok == Token::Slash,
            _ => false,
        }) {
            let op = self.peek().unwrap().copied()?;
            self.next();
            let rhs = self.parse_terminal_or_group()?;
            let binary_op = match op {
                Token::Star => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
                _ => unreachable!(),
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
                        Err(ParseError::RightParenExpected(self.line, t.lexeme().into()))
                    }
                    None => Err(ParseError::RightParenExpected(self.line, "EOF".into())),
                    Some(Err(e)) => Err(e),
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
            Some(Ok(t)) => Err(ParseError::ExpressionExpected(self.line, t.lexeme().into())),
            Some(Err(e)) => Err(e),
            None => Err(ParseError::ExpressionExpected(self.line, "EOF".to_string())),
        };
        tok.map(Rc::new)
    }
}
