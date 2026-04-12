use std::collections::HashMap;

use derived_deref::{Deref, DerefMut};

use crate::{
    hir::dataflow::Signature,
    id::{BlockId, InstrId, ProgramId, VarId},
    parser::Program,
};

/// Мапа для сопоставления пользовательских имен и идентификаторов
#[derive(Debug, Clone, Deref, DerefMut)]
pub(crate) struct DeclMap(
    #[target] pub(crate) HashMap<String, ProgramId>,
    pub(crate) HashMap<ProgramId, String>,
);

impl DeclMap {
    pub(crate) fn revmap(&self) -> &HashMap<ProgramId, String> {
        &self.1
    }
}

#[derive(Debug, Clone, Deref, DerefMut)]
pub(crate) struct DefMap<'p>(pub(crate) HashMap<ProgramId, &'p Program>);

#[derive(Debug, Clone, Deref, DerefMut)]
pub struct Hir(pub(crate) HashMap<ProgramId, HirFunction>);

#[derive(Debug, Clone, Deref, DerefMut)]
pub struct HirFunction {
    pub(crate) id: ProgramId,
    pub(crate) name: Option<String>,
    pub(crate) signature: Signature,
    #[target]
    pub(crate) blocks: Vec<HirBaseBlock>,
}

impl HirFunction {
    pub(crate) fn empty(id: ProgramId, name: Option<&String>, signature: Signature) -> HirFunction {
        HirFunction {
            id,
            name: name.cloned(),
            signature,
            blocks: vec![],
        }
    }
}

#[derive(Debug, Clone, Deref, DerefMut)]
pub struct HirBaseBlock {
    pub(crate) id: BlockId,
    #[target]
    pub(crate) exprs: Vec<Expr>,
}

impl HirBaseBlock {
    pub(crate) fn new() -> HirBaseBlock {
        HirBaseBlock {
            id: BlockId::new(),
            exprs: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Goto(BlockId),                 // Безусловно перейти в блок
    GotoIf(Var, BlockId, BlockId), // По условию 0 перейти в 1 или 2
    Instr(Instr),
    Call(ProgramId, Vec<Var>, Vec<Var>),
    Return,
}

#[derive(Debug, Clone, Copy)]
pub struct Var {
    pub(crate) id: VarId,
    pub(crate) kind: VarKind,
}

impl Var {
    pub(crate) fn new(id: VarId, kind: VarKind) -> Var {
        Var { id, kind }
    }

    pub(crate) fn nothing() -> Var {
        Var::new(VarId::new(), VarKind::Nothing)
    }
}

/// Инструкция трехадресного кода
#[derive(Debug, Clone)]
pub struct Instr {
    pub(crate) id: InstrId,
    pub(crate) kind: InstrKind,
    pub(crate) produces: Var,
    pub(crate) consumes: (Var, Var),
}

impl Instr {
    pub(crate) fn new(id: InstrId, kind: InstrKind, produces: Var, consumes: (Var, Var)) -> Instr {
        Instr {
            id,
            kind,
            produces,
            consumes,
        }
    }

    pub(crate) fn imm_int(produces: Var, i: i32) -> Instr {
        Instr::new(
            InstrId::new(),
            InstrKind::ConstInt(i),
            produces,
            (Var::nothing(), Var::nothing()),
        )
    }

    pub(crate) fn imm_bool(produces: Var, b: bool) -> Instr {
        Instr::new(
            InstrId::new(),
            InstrKind::ConstBool(b),
            produces,
            (Var::nothing(), Var::nothing()),
        )
    }

    pub(crate) fn op(kind: InstrKind, ret: Var, args: (Var, Var)) -> Instr {
        Instr::new(InstrId::new(), kind, ret, args)
    }
}

#[derive(Debug, Clone)]
pub enum InstrKind {
    ConstInt(i32),
    ConstBool(bool),

    Add,
    Sub,
    Mul,
    Div,
    Less,
    LessOrEq,
    Great,
    GreatOrEq,
}

#[derive(Debug, Clone, Copy)]
pub enum VarKind {
    Int,
    Bool,
    Any,
    AnonFn(ProgramId),
    Nothing,
}
