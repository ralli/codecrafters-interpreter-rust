use anyhow::anyhow;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("{0}")]
    Any(#[from] anyhow::Error),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    PrintStatement(Rc<Ast<'a>>),
    ExpressionStatement(Rc<Ast<'a>>),
    AssignmentStatement(String, Option<Rc<Ast<'a>>>),
    BlockStatement(Vec<Rc<Statement<'a>>>),
}

impl<'a> fmt::Display for Statement<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::PrintStatement(expr) => write!(f, "(print {expr})"),
            Statement::ExpressionStatement(expr) => write!(f, "{expr}"),
            Statement::AssignmentStatement(variable, expr) => {
                if let Some(expr) = expr {
                    write!(f, "(assign {variable} {expr})")
                } else {
                    write!(f, "(declare {variable})")
                }
            }
            Statement::BlockStatement(inner) => {
                write!(f, "(block ")?;
                for (i, s) in inner.iter().enumerate() {
                    if i > 1 {
                        write!(f, ";")?;
                    }
                    write!(f, "{s}")?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ast<'a> {
    Boolean(bool),
    Number(f64),
    Identifier(&'a str),
    String(&'a str),
    Group(Rc<Ast<'a>>),
    Unary(UnaryOp, Rc<Ast<'a>>),
    Binary(BinaryOp, Rc<Ast<'a>>, Rc<Ast<'a>>),
    Comparison(ComparisonOp, Rc<Ast<'a>>, Rc<Ast<'a>>),
    Equality(EqualityOp, Rc<Ast<'a>>, Rc<Ast<'a>>),
    Assignment(Rc<Ast<'a>>, Rc<Ast<'a>>),
    Nil,
}

impl<'a> Ast<'a> {
    pub fn eval(&self, variables: &mut HashMap<String, Value>) -> Result<Value, RuntimeError> {
        match self {
            Ast::Boolean(b) => Ok(Value::Bool(*b)),
            Ast::Number(x) => Ok(Value::Number(*x)),
            Ast::Identifier(s) => {
                let v = s.to_string();
                match variables.get(&v) {
                    Some(value) => Ok(value.clone()),
                    None => Err(RuntimeError::Any(anyhow!("{s} not defined"))),
                }
            }
            Ast::String(s) => Ok(Value::String(s.to_string())),
            Ast::Group(inner) => inner.eval(variables),
            Ast::Unary(op, right) => match op {
                UnaryOp::Not => match right.eval(variables)? {
                    Value::Bool(b) => Ok(Value::Bool(!b)),
                    Value::Nil => Ok(Value::Bool(true)),
                    _ => Ok(Value::Bool(false)),
                },
                UnaryOp::Neg => match right.eval(variables)? {
                    Value::Number(x) => Ok(Value::Number(-x)),
                    _ => Err(RuntimeError::Any(anyhow!("Operand must be a number."))),
                },
            },
            Ast::Binary(op, left, right) => match (left.eval(variables)?, right.eval(variables)?) {
                (Value::Number(a), Value::Number(b)) => match op {
                    BinaryOp::Add => Ok(Value::Number(a + b)),
                    BinaryOp::Sub => Ok(Value::Number(a - b)),
                    BinaryOp::Mul => Ok(Value::Number(a * b)),
                    BinaryOp::Div => Ok(Value::Number(a / b)),
                },
                (Value::String(a), Value::String(b)) => match op {
                    BinaryOp::Add => Ok(Value::String(format!("{a}{b}"))),
                    op => Err(RuntimeError::Any(anyhow!(
                        "operation {op} not implemented for strings"
                    ))),
                },
                (l, r) => Err(RuntimeError::Any(anyhow!(
                    "incompatible types for {op} {} {}",
                    l.type_name(),
                    r.type_name()
                ))),
            },
            Ast::Comparison(op, left, right) => {
                match (left.eval(variables)?, right.eval(variables)?) {
                    (Value::Number(a), Value::Number(b)) => Ast::compare(*op, a, b),
                    /* would be cooler, if comparison would be implemented on numbers and strings as well */
                    // (Value::Bool(a), Value::Bool(b)) => Ast::compare(*op, a, b),
                    // (Value::String(a), Value::String(b)) => Ast::compare(*op, a.as_str(), b.as_str()),
                    _ => Err(RuntimeError::Any(anyhow!("Operands must be numbers."))),
                }
            }
            Ast::Equality(op, left, right) => match (left.eval(variables)?, right.eval(variables)?)
            {
                (Value::Number(a), Value::Number(b)) => Ast::equal_to(*op, a, b),
                (Value::Bool(a), Value::Bool(b)) => Ast::equal_to(*op, a, b),
                (Value::String(a), Value::String(b)) => Ast::equal_to(*op, a.as_str(), b.as_str()),
                (Value::Nil, Value::Nil) => Ok(Value::Bool(true)),
                _ => Ok(Value::Bool(false)),
            },
            Ast::Assignment(left, right) => match (left.as_ref(), right.eval(variables)?) {
                (Ast::Identifier(name), v) => {
                    variables.insert(name.to_string(), v.clone());
                    Ok(v)
                }
                _ => Err(RuntimeError::Any(anyhow!("Can only assign to variables"))),
            },
            Ast::Nil => Ok(Value::Nil),
        }
    }
    fn equal_to<T: PartialEq>(op: EqualityOp, a: T, b: T) -> Result<Value, RuntimeError> {
        match op {
            EqualityOp::Equal => Ok(Value::Bool(a == b)),
            EqualityOp::NotEqual => Ok(Value::Bool(a != b)),
        }
    }
    fn compare<T: PartialOrd>(op: ComparisonOp, a: T, b: T) -> Result<Value, RuntimeError> {
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
            Ast::Identifier(s) => write!(f, "${s}"),
            Ast::String(s) => write!(f, "{s}"),
            Ast::Group(inner) => write!(f, "(group {inner})"),
            Ast::Unary(op, rhs) => write!(f, "({op} {rhs})"),
            Ast::Binary(op, lhs, rhs) => write!(f, "({} {} {})", op, lhs, rhs),
            Ast::Comparison(op, lhs, rhs) => write!(f, "({} {} {})", op, lhs, rhs),
            Ast::Equality(op, lhs, rhs) => write!(f, "({} {} {})", op, lhs, rhs),
            Ast::Assignment(left, right) => write!(f, "(= {left} {right})"),
            Ast::Nil => write!(f, "nil"),
        }
    }
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
