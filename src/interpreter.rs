use crate::{Parser, Statement, Value};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum InterpreterError {
    #[error("{0}")]
    ParseError(#[from] crate::error::ParseError),
    #[error("{0}")]
    RuntimeError(#[from] crate::ast::RuntimeError),
}

pub struct Interpreter<'a> {
    input: &'a str,
    variables: HashMap<String, Value>,
}

impl<'a> Interpreter<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            variables: HashMap::new(),
        }
    }

    pub fn run(&mut self) -> Result<(), InterpreterError> {
        let mut parser = Parser::new(self.input);
        let statements = parser.parse_statement_list()?;
        for statement in statements.iter() {
            match statement {
                Statement::PrintStatement(expression) => {
                    let value = expression.eval(&mut self.variables)?;
                    println!("{value}");
                }
                Statement::ExpressionStatement(expression) => {
                    let _ = expression.eval(&mut self.variables)?;
                }
                Statement::AssignmentStatement(name, expression) => {
                    if let Some(expression) = expression {
                        let value = expression.eval(&mut self.variables)?;
                        self.variables.insert(name.to_string(), value);
                    } else {
                        self.variables.insert(name.to_string(), Value::Nil);
                    }
                }
            }
        }
        Ok(())
    }
}
