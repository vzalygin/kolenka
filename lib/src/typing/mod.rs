//! Модуль с логикой для описания и работы с типами.

mod fmt;
mod inference;
mod structs;

pub use inference::TypingError;
pub use structs::{Type, StackCfg};

pub(crate) use crate::typing::{
    inference::{TypesMap, infer_definitions},
    structs::StackVar,
};
