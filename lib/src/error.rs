use thiserror::Error;

use crate::typing::TypingError;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("syntax error: {description}")]
    ParserError { description: String },
    #[error("typing error: {0}")]
    TypingError(TypingError),
}
