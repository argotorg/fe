use std::fmt;

/// Errors that can happen while emitting Yul.
#[derive(Debug)]
pub enum YulError {
    /// Raised when a function declaration lacks a body in HIR.
    MissingBody(String),
    /// Raised for features the Yul backend does not yet support.
    Unsupported(String),
}

impl fmt::Display for YulError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            YulError::MissingBody(name) => write!(f, "function `{name}` does not have a body"),
            YulError::Unsupported(msg) => write!(f, "{msg}"),
        }
    }
}

impl std::error::Error for YulError {}
