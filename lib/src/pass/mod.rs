mod type_inference;
mod typing;

pub use type_inference::{TypingError, infer_ast};
pub use typing::Type;
