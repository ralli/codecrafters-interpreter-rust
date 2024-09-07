use crate::ast::*;
use crate::error::*;
use crate::{Scanner, ScannerError, Token};
use anyhow::anyhow;
use std::rc::Rc;

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

    pub fn parse_statement_list(&mut self) -> Result<Vec<Statement<'a>>, ParseError> {
        let mut result = Vec::new();
        self.next();
        loop {
            let bla = self.parse_statement();
            match bla {
                Some(Ok(statement)) => result.push(statement),
                Some(Err(err)) => return Err(err),
                None => break,
            };
        }
        Ok(result)
    }

    fn parse_statement(&mut self) -> Option<Result<Statement<'a>, ParseError>> {
        let _ = self.peek()?;

        match self.peek() {
            Some(Ok(Token::Print)) => {
                self.next();
                let expression = match self.parse_expression() {
                    Ok(x) => x,
                    Err(e) => return Some(Err(e)),
                };
                if let Some(Ok(Token::Semicolon)) = self.peek() {
                    self.next();
                    Some(Ok(Statement::PrintStatement(expression)))
                } else {
                    Some(Err(ParseError::ExpressionExpected(
                        self.line,
                        "; expected".to_string(),
                    )))
                }
            }
            Some(Ok(Token::Var)) => {
                self.next();
                Some(self.parse_assignment_statement())
            }
            _ => {
                let expression = match self.parse_expression() {
                    Ok(x) => x,
                    Err(e) => return Some(Err(e)),
                };
                if let Some(Ok(Token::Semicolon)) = self.peek() {
                    self.next();
                    Some(Ok(Statement::ExpressionStatement(expression)))
                } else {
                    Some(Err(ParseError::ExpressionExpected(
                        self.line,
                        "; expected".to_string(),
                    )))
                }
            }
        }
    }

    fn parse_assignment_statement(&mut self) -> Result<Statement<'a>, ParseError> {
        let Some(Ok(Token::Identifier(name))) = self.peek() else {
            return Err(ParseError::ExpressionExpected(
                self.line,
                "identifier expected".to_string(),
            ));
        };
        let name = name.to_string();
        if let Some(Ok(Token::Semicolon)) = self.next() {
            self.next();
            return Ok(Statement::AssignmentStatement(name, None));
        }
        let Some(Ok(Token::Equal)) = self.peek() else {
            return Err(ParseError::ExpressionExpected(
                self.line,
                "= expected".to_string(),
            ));
        };
        self.next();
        let expression = self.parse_expression()?;
        let Some(Ok(Token::Semicolon)) = self.peek() else {
            return Err(ParseError::ExpressionExpected(
                self.line,
                "; expected".to_string(),
            ));
        };
        self.next();
        Ok(Statement::AssignmentStatement(name, Some(expression)))
    }

    fn parse_expression(&mut self) -> Result<Rc<Ast<'a>>, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Rc<Ast<'a>>, ParseError> {
        let mut lhs = self.parse_equality()?;
        while let Some(Ok(Token::Equal)) = self.peek() {
            self.next();
            let rhs = self.parse_assignment()?;
            lhs = Rc::new(Ast::Assignment(lhs, rhs));
        }
        Ok(lhs)
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
            Some(Ok(Token::Identifier(s))) => {
                let res = Ok(Ast::Identifier(s));
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
