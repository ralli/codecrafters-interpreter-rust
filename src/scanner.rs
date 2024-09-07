use std::borrow::Cow;
use std::fmt;
use std::fmt::Formatter;
use std::iter::Peekable;
use std::str::Chars;
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
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

impl<'a> Token<'a> {
    pub fn type_name(&self) -> &'a str {
        match self {
            Token::BangEqual => "BANG_EQUAL",
            Token::Bang => "BANG",
            Token::Comma => "COMMA",
            Token::Dot => "DOT",
            Token::Equal => "EQUAL",
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
            Token::Number(_) => "NUMBER",
            Token::Identifier(_) => "IDENTIFIER",
            Token::And => "AND",
            Token::Class => "CLASS",
            Token::Else => "ELSE",
            Token::False => "FALSE",
            Token::For => "FOR",
            Token::Fun => "FUN",
            Token::If => "IF",
            Token::Nil => "NIL",
            Token::Or => "OR",
            Token::Print => "PRINT",
            Token::Return => "RETURN",
            Token::Super => "SUPER",
            Token::This => "THIS",
            Token::True => "TRUE",
            Token::Var => "VAR",
            Token::While => "WHILE",
        }
    }

    pub fn lexeme(&self) -> Cow<'a, str> {
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
            Token::Number(s) => (*s).into(),
            Token::Identifier(s) => (*s).into(),
            Token::And => "and".into(),
            Token::Class => "class".into(),
            Token::Else => "else".into(),
            Token::False => "false".into(),
            Token::For => "for".into(),
            Token::Fun => "fun".into(),
            Token::If => "if".into(),
            Token::Nil => "nil".into(),
            Token::Or => "or".into(),
            Token::Print => "print".into(),
            Token::Return => "return".into(),
            Token::Super => "super".into(),
            Token::This => "this".into(),
            Token::True => "true".into(),
            Token::Var => "var".into(),
            Token::While => "while".into(),
        }
    }

    pub fn repr(&self) -> Cow<'a, str> {
        match self {
            Token::String(s) => (*s).into(),
            Token::Number(s) => {
                let x = s.parse::<f64>().unwrap();
                if x.trunc() == x {
                    format!("{:.1}", x).into()
                } else {
                    format!("{}", x).into()
                }
            }
            _ => "null".into(),
        }
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.type_name(), self.lexeme(), self.repr())
    }
}

#[derive(Debug)]
struct UtfPosChars<'a> {
    pos: usize,
    it: Chars<'a>,
}

impl<'a> UtfPosChars<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            pos: 0,
            it: input.chars(),
        }
    }
}

impl<'a> Iterator for UtfPosChars<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        match self.it.next() {
            Some(ch) => {
                let current_pos = self.pos;
                self.pos += ch.len_utf8();
                Some((current_pos, ch))
            }
            None => None,
        }
    }
}

#[derive(Debug)]
pub struct Scanner<'a> {
    input: &'a str,
    it: Peekable<UtfPosChars<'a>>,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            it: UtfPosChars::new(input).peekable(),
            line: 1,
        }
    }

    pub fn current_line(&self) -> usize {
        self.line
    }

    fn skip_comment(&mut self) {
        let Some((pos, '/')) = self.it.peek().copied() else {
            return;
        };

        let mut rest = self.input[pos..].chars().skip(1);

        let Some('/') = rest.next() else {
            return;
        };

        while let Some(true) = self.it.peek().map(|(_, c)| *c != '\n') {
            self.it.next();
        }

        if let Some(true) = self.it.peek().map(|(_, c)| *c == '\n') {
            self.it.next();
            self.line += 1;
        }
    }

    fn scan_string(&mut self) -> Option<Result<Token<'a>, ScannerError>> {
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
        Some(Ok(Token::String(&self.input[start..end])))
    }

    fn scan_number(&mut self) -> Option<Result<Token<'a>, ScannerError>> {
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

        Some(Ok(Token::Number(self.token_str(start))))
    }

    fn token_str(&mut self, start: usize) -> &'a str {
        if let Some((end, _c)) = self.it.peek().copied() {
            &self.input[start..end]
        } else {
            &self.input[start..]
        }
    }

    fn scan_identifier(&mut self) -> Option<Result<Token<'a>, ScannerError>> {
        let (start, _c) = self.it.peek().copied()?;
        while let Some(true) = self
            .it
            .peek()
            .map(|(_, c)| c.is_alphabetic() || c.is_ascii_digit() || *c == '_')
        {
            self.it.next();
        }
        let s = self.token_str(start);
        let tok = match s {
            "and" => Token::And,
            "class" => Token::Class,
            "else" => Token::Else,
            "false" => Token::False,
            "for" => Token::For,
            "fun" => Token::Fun,
            "if" => Token::If,
            "nil" => Token::Nil,
            "or" => Token::Or,
            "print" => Token::Print,
            "return" => Token::Return,
            "super" => Token::Super,
            "this" => Token::This,
            "true" => Token::True,
            "var" => Token::Var,
            "while" => Token::While,
            _ => Token::Identifier(s),
        };
        Some(Ok(tok))
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token<'a>, ScannerError>;

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

        let mut if_token_has_equal =
            |with_equal: Token<'a>, without_equal: Token<'a>| match self.it.peek() {
                Some((_, '=')) => {
                    self.it.next();
                    Some(Ok(with_equal))
                }
                _ => Some(Ok(without_equal)),
            };

        match c {
            '!' => if_token_has_equal(Token::BangEqual, Token::Bang),
            ',' => Some(Ok(Token::Comma)),
            '.' => Some(Ok(Token::Dot)),
            '=' => if_token_has_equal(Token::EqualEqual, Token::Equal),
            '>' => if_token_has_equal(Token::GreaterEqual, Token::Greater),
            '{' => Some(Ok(Token::LeftBrace)),
            '(' => Some(Ok(Token::LeftParen)),
            '<' => if_token_has_equal(Token::LessEqual, Token::Less),
            '-' => Some(Ok(Token::Minus)),
            '+' => Some(Ok(Token::Plus)),
            '}' => Some(Ok(Token::RightBrace)),
            ')' => Some(Ok(Token::RightParen)),
            ';' => Some(Ok(Token::Semicolon)),
            '/' => Some(Ok(Token::Slash)),
            '*' => Some(Ok(Token::Star)),
            '"' => self.scan_string(),
            _ => Some(Err(ScannerError::InvalidCharacter { line: self.line, c })),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scanner_unicode_string() -> Result<(), anyhow::Error> {
        let input = r#""ॐa""#;
        let scanner = Scanner::new(input);
        let tokens: Vec<_> = scanner.collect();
        dbg!(&tokens);
        let expected = Token::String("ॐa");
        assert_eq!(Ok(expected), tokens[0]);
        Ok(())
    }

    #[test]
    fn test_chars_unicode() {
        let input = "ॐa";
        let mut it = UtfPosChars::new(input);
        let (i, ch) = it.next().unwrap();
        assert_eq!('ॐ', ch);
        assert_eq!(3, ch.len_utf8());
        assert_eq!(0, i);
        let (i, ch) = it.next().unwrap();
        assert_eq!('a', ch);
        assert_eq!(1, ch.len_utf8());
        assert_eq!(3, i);
    }
}
