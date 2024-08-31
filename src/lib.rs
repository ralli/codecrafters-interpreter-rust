use anyhow::anyhow;
use std::borrow::Cow;
use std::fmt;
use std::fmt::Formatter;
use std::iter::{Enumerate, Peekable};
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Bang,
    BangEqual,
    Comma,
    Dot,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    LeftBrace,
    Less,
    LessEqual,
    Minus,
    Plus,
    RightBrace,
    RightParen,
    Semicolon,
    Slash,
    Star,
    LeftParen,
    String(&'a str),
}

impl<'a> Token<'a> {
    pub fn type_name(&self) -> &'a str {
        match self {
            Token::Bang => "BANG",
            Token::BangEqual => "BANG_EQUAL",
            Token::Comma => "COMMA",
            Token::Dot => "DOT",
            Token::Equal => "EOF",
            Token::EqualEqual => "EQUAL_EQUAL",
            Token::Greater => "GREATER",
            Token::GreaterEqual => "GREATER_EQUAL",
            Token::LeftBrace => "LEFT_BRACE",
            Token::LeftParen => "LEFT_PAREN",
            Token::Less => "LESS",
            Token::LessEqual => "LESS_EQUAL",
            Token::Minus => "MINUS",
            Token::Plus => "PLUS",
            Token::RightBrace => "RIGHT_BRACE",
            Token::RightParen => "RIGHT_PAREN",
            Token::Semicolon => "SEMICOLON",
            Token::Slash => "SLASH",
            Token::Star => "STAR",
            Token::String(_) => "STRING",
        }
    }

    pub fn lexeme(&self) -> Cow<'static, str> {
        match self {
            Token::Bang => "!".into(),
            Token::BangEqual => "!=".into(),
            Token::Comma => ",".into(),
            Token::Dot => ".".into(),
            Token::Equal => "=".into(),
            Token::EqualEqual => "==".into(),
            Token::Greater => ">".into(),
            Token::GreaterEqual => ">=".into(),
            Token::LeftBrace => "{".into(),
            Token::LeftParen => "(".into(),
            Token::Less => "<".into(),
            Token::LessEqual => "<=".into(),
            Token::Minus => "-".into(),
            Token::Plus => "+".into(),
            Token::RightBrace => "}".into(),
            Token::RightParen => ")".into(),
            Token::Semicolon => ";".into(),
            Token::Slash => "/".into(),
            Token::Star => "*".into(),
            Token::String(s) => format!("{}{}{}", '"', s, '"').into(),
        }
    }

    pub fn repr(&self) -> &'a str {
        match self {
            Token::String(s) => s,
            _ => "null"
        }
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.type_name(), self.lexeme(), self.repr())
    }
}

pub struct Scanner<'a> {
    input: &'a str,
    it: Peekable<Enumerate<Chars<'a>>>,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            it: input.chars().enumerate().peekable(),
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token<'a>, anyhow::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let (pos, c) = self.it.next()?;

        let mut bla = move |with_equal: Token<'a>, without_equal: Token<'a>| {
            match self.it.peek() {
                Some((_, '=')) => {
                    self.it.next();
                    Some(Ok(with_equal))
                }
                _ => Some(Ok(without_equal))
            }
        };

        match c {
            '!' => bla(Token::BangEqual, Token::Bang),
            ',' => Some(Ok(Token::Comma)),
            '.' => Some(Ok(Token::Dot)),
            '=' => bla(Token::EqualEqual, Token::Equal),
            '>' => bla(Token::Greater, Token::GreaterEqual),
            '{' => Some(Ok(Token::LeftBrace)),
            '(' => Some(Ok(Token::LeftParen)),
            '<' => bla(Token::LessEqual, Token::Less),
            '-' => Some(Ok(Token::Minus)),
            '+' => Some(Ok(Token::Plus)),
            '}' => Some(Ok(Token::RightBrace)),
            ')' => Some(Ok(Token::RightParen)),
            ';' => Some(Ok(Token::Semicolon)),
            '/' => Some(Ok(Token::Slash)),
            '*' => Some(Ok(Token::Star)),
            _ => Some(Err(anyhow!("invalid character {} at {}", c, pos))),
        }
    }
}
