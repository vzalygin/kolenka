use thiserror::Error;

#[derive(Error, Debug)]
pub(crate) enum CompilerError {
    #[error("Syntax error: {description:?}")]
    ParserError { description: String },
}
