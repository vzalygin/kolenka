//! Модуль с логикой для описания и работы с типами.

mod fmt;
mod inference;
mod types;

pub use inference::{TypingError, infer_ast};
pub use types::Type;
