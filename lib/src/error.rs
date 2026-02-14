use thiserror::Error;

use crate::pass::TypingError;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Syntax error: {description:?}")]
    ParserError { description: String },
    #[error("Typing error: {0:?}")]
    TypingError(TypingError),
}
