use std::borrow::Cow;
use std::fmt;
use std::fmt::Formatter;
use std::iter::{Enumerate, Peekable};
use std::str::Chars;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ScannerError {
    #[error("[line {}] Error: Unexpected character: {}", line, c)]
    InvalidCharacter { line: usize, c: char },

    #[error("[line {}] Error: Unterminated string.", line)]
    UnterminatedString { line: usize },
}
#[derive(Debug, Clone, Copy, PartialEq)]
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
    Number(&'a str),
    Identifier(&'a str),
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl<'a> crate::Token<'a> {
    pub fn type_name(&self) -> &'a str {
        match self {
            crate::Token::Bang => "BANG",
            crate::Token::BangEqual => "BANG_EQUAL",
            crate::Token::Comma => "COMMA",
            crate::Token::Dot => "DOT",
            crate::Token::Equal => "EQUAL",
            crate::Token::EqualEqual => "EQUAL_EQUAL",
            crate::Token::Greater => "GREATER",
            crate::Token::GreaterEqual => "GREATER_EQUAL",
            crate::Token::LeftBrace => "LEFT_BRACE",
            crate::Token::LeftParen => "LEFT_PAREN",
            crate::Token::Less => "LESS",
            crate::Token::LessEqual => "LESS_EQUAL",
            crate::Token::Minus => "MINUS",
            crate::Token::Plus => "PLUS",
            crate::Token::RightBrace => "RIGHT_BRACE",
            crate::Token::RightParen => "RIGHT_PAREN",
            crate::Token::Semicolon => "SEMICOLON",
            crate::Token::Slash => "SLASH",
            crate::Token::Star => "STAR",
            crate::Token::String(_) => "STRING",
            crate::Token::Number(_) => "NUMBER",
            crate::Token::Identifier(_) => "IDENTIFIER",
            crate::Token::And => "AND",
            crate::Token::Class => "CLASS",
            crate::Token::Else => "ELSE",
            crate::Token::False => "FALSE",
            crate::Token::For => "FOR",
            crate::Token::Fun => "FUN",
            crate::Token::If => "IF",
            crate::Token::Nil => "NIL",
            crate::Token::Or => "OR",
            crate::Token::Print => "PRINT",
            crate::Token::Return => "RETURN",
            crate::Token::Super => "SUPER",
            crate::Token::This => "THIS",
            crate::Token::True => "TRUE",
            crate::Token::Var => "VAR",
            crate::Token::While => "WHILE",
        }
    }

    pub fn lexeme(&self) -> Cow<'a, str> {
        match self {
            crate::Token::Bang => "!".into(),
            crate::Token::BangEqual => "!=".into(),
            crate::Token::Comma => ",".into(),
            crate::Token::Dot => ".".into(),
            crate::Token::Equal => "=".into(),
            crate::Token::EqualEqual => "==".into(),
            crate::Token::Greater => ">".into(),
            crate::Token::GreaterEqual => ">=".into(),
            crate::Token::LeftBrace => "{".into(),
            crate::Token::LeftParen => "(".into(),
            crate::Token::Less => "<".into(),
            crate::Token::LessEqual => "<=".into(),
            crate::Token::Minus => "-".into(),
            crate::Token::Plus => "+".into(),
            crate::Token::RightBrace => "}".into(),
            crate::Token::RightParen => ")".into(),
            crate::Token::Semicolon => ";".into(),
            crate::Token::Slash => "/".into(),
            crate::Token::Star => "*".into(),
            crate::Token::String(s) => format!("{}{}{}", '"', s, '"').into(),
            crate::Token::Number(s) => (*s).into(),
            crate::Token::Identifier(s) => (*s).into(),
            crate::Token::And => "and".into(),
            crate::Token::Class => "class".into(),
            crate::Token::Else => "else".into(),
            crate::Token::False => "false".into(),
            crate::Token::For => "for".into(),
            crate::Token::Fun => "fun".into(),
            crate::Token::If => "if".into(),
            crate::Token::Nil => "nil".into(),
            crate::Token::Or => "or".into(),
            crate::Token::Print => "print".into(),
            crate::Token::Return => "return".into(),
            crate::Token::Super => "super".into(),
            crate::Token::This => "this".into(),
            crate::Token::True => "true".into(),
            crate::Token::Var => "var".into(),
            crate::Token::While => "while".into(),
        }
    }

    pub fn repr(&self) -> Cow<'a, str> {
        match self {
            crate::Token::String(s) => (*s).into(),
            crate::Token::Number(s) => {
                let x = s.parse::<f64>().unwrap();
                if x.trunc() == x {
                    format!("{:.1}", x).into()
                } else {
                    format!("{}", x).into()
                }
            }
            _ => "null".into()
        }
    }
}

impl<'a> fmt::Display for crate::Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.type_name(), self.lexeme(), self.repr())
    }
}

#[derive(Debug)]
pub struct Scanner<'a> {
    input: &'a str,
    it: Peekable<Enumerate<Chars<'a>>>,
    line: usize,
}

impl<'a> crate::Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            it: input.chars().enumerate().peekable(),
            line: 1,
        }
    }

    fn skip_comment(&mut self) {
        let Some((pos, '/')) = self.it.peek().copied() else { return; };

        let mut rest = (self.input[pos..]).chars().skip(1);

        let Some('/') = rest.next() else { return; };

        while let Some(true) = self.it.peek().map(|(_, c)| *c != '\n') {
            self.it.next();
        }

        if let Some(true) = self.it.peek().map(|(_, c)| *c == '\n') {
            self.it.next();
            self.line += 1;
        }
    }

    fn scan_string(&mut self) -> Option<Result<crate::Token<'a>, ScannerError>> {
        let Some((start, _c)) = self.it.peek().copied() else {
            return Some(Err(ScannerError::UnterminatedString { line: self.line }));
        };
        while let Some(true) = self.it.peek().map(|(_, c)| *c != '"') {
            let (_, c) = self.it.next()?;
            if c == '\n' {
                self.line += 1;
            }
        }
        let Some((end, '"')) = self.it.next() else {
            return Some(Err(ScannerError::UnterminatedString { line: self.line }));
        };
        Some(Ok(crate::Token::String(&self.input[start..end])))
    }

    fn scan_number(&mut self) -> Option<Result<crate::Token<'a>, ScannerError>> {
        let (start, _c) = self.it.peek().copied()?;
        while let Some(true) = self.it.peek().map(|(_, c)| c.is_ascii_digit()) {
            self.it.next();
        }
        if let Some((_, '.')) = self.it.peek() {
            self.it.next();
            while let Some(true) = self.it.peek().map(|(_, c)| c.is_ascii_digit()) {
                self.it.next();
            }
        }
        let s =
            if let Some((end, _c)) = self.it.peek().copied() {
                &self.input[start..end]
            } else {
                &self.input[start..]
            };
        Some(Ok(crate::Token::Number(s)))
    }

    fn scan_identifier(&mut self) -> Option<Result<crate::Token<'a>, ScannerError>> {
        let (start, _c) = self.it.peek().copied()?;
        while let Some(true) = self.it.peek().map(|(_, c)| c.is_alphabetic() || c.is_ascii_digit() || *c == '_') {
            self.it.next();
        }
        let s =
            if let Some((end, _c)) = self.it.peek().copied() {
                &self.input[start..end]
            } else {
                &self.input[start..]
            };
        let tok = match s {
            "and" => crate::Token::And,
            "class" => crate::Token::Class,
            "else" => crate::Token::Else,
            "false" => crate::Token::False,
            "for" => crate::Token::For,
            "fun" => crate::Token::Fun,
            "if" => crate::Token::If,
            "nil" => crate::Token::Nil,
            "or" => crate::Token::Or,
            "print" => crate::Token::Print,
            "return" => crate::Token::Return,
            "super" => crate::Token::Super,
            "this" => crate::Token::This,
            "true" => crate::Token::True,
            "var" => crate::Token::Var,
            "while" => crate::Token::While,
            _ => crate::Token::Identifier(s),
        };
        Some(Ok(tok))
    }
}

impl<'a> Iterator for crate::Scanner<'a> {
    type Item = Result<crate::Token<'a>, ScannerError>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(true) = self.it.peek().map(|(_pos, c)| c.is_whitespace()) {
            let (_pos, c) = self.it.next()?;
            if c == '\n' {
                self.line += 1;
            }
        }

        self.skip_comment();

        if let Some(true) = self.it.peek().map(|(_, c)| c.is_ascii_digit()) {
            return self.scan_number();
        }

        if let Some(true) = self.it.peek().map(|(_, c)| c.is_alphabetic() || *c == '_') {
            return self.scan_identifier();
        }

        let (_pos, c) = self.it.next()?;

        let mut if_token_has_equal = |with_equal: crate::Token<'a>, without_equal: crate::Token<'a>| {
            match self.it.peek() {
                Some((_, '=')) => {
                    self.it.next();
                    Some(Ok(with_equal))
                }
                _ => Some(Ok(without_equal))
            }
        };


        match c {
            '!' => if_token_has_equal(crate::Token::BangEqual, crate::Token::Bang),
            ',' => Some(Ok(crate::Token::Comma)),
            '.' => Some(Ok(crate::Token::Dot)),
            '=' => if_token_has_equal(crate::Token::EqualEqual, crate::Token::Equal),
            '>' => if_token_has_equal(crate::Token::GreaterEqual, crate::Token::Greater),
            '{' => Some(Ok(crate::Token::LeftBrace)),
            '(' => Some(Ok(crate::Token::LeftParen)),
            '<' => if_token_has_equal(crate::Token::LessEqual, crate::Token::Less),
            '-' => Some(Ok(crate::Token::Minus)),
            '+' => Some(Ok(crate::Token::Plus)),
            '}' => Some(Ok(crate::Token::RightBrace)),
            ')' => Some(Ok(crate::Token::RightParen)),
            ';' => Some(Ok(crate::Token::Semicolon)),
            '/' => Some(Ok(crate::Token::Slash)),
            '*' => Some(Ok(crate::Token::Star)),
            '"' => self.scan_string(),
            _ => Some(Err(ScannerError::InvalidCharacter { line: self.line, c }))
        }
    }
}
