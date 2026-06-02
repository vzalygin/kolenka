//! Модуль кодогенератора
//!
//! Кодогенератор осуществляет преобразовние high IR в код под target-платформу web assembly

use std::{collections::HashMap, ops::Deref};

use wasm_encoder::{Function, RefType, ValType};

use crate::{codegen::wasm::{WasmFunctionBundle, WasmModuleBundle}, hir::{Expr, ExprInstr, InstrKind}, id::BlockId};
pub(crate) use crate::codegen::wasm::{WasmLocalId, WasmType, WasmTypeId};
pub use crate::codegen::{wasm::WasmModule, generate::generate_bytecode};

mod wasm;
mod generate;
mod blocks_graph;
