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
