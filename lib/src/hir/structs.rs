use std::collections::HashMap;

use derived_deref::{Deref, DerefMut};

use crate::{Type, id::{BlockId, ProgramId, VarId}, parser::Program};

/// Мапа для сопоставления пользовательских имен и идентификаторов
#[derive(Debug, Clone, Deref, DerefMut)]
pub(crate) struct DeclMap(pub(crate) HashMap<String, ProgramId>);

#[derive(Debug, Clone, Deref, DerefMut)]
pub(crate) struct DefMap<'p>(pub(crate) HashMap<ProgramId, &'p Program>);

#[derive(Debug, Clone)]
pub struct Hir {
    pub(crate) functions: HashMap<String, HirFunction>,
}

impl Hir {
    pub(crate) fn new(functions: HashMap<String, HirFunction>) -> Hir {
        Hir { functions }
    }
}

#[derive(Debug, Clone)]
pub struct HirFunction(pub(crate) Vec<HirBaseBlock>);

#[derive(Debug, Clone)]
pub struct HirBaseBlock {
    pub(crate) id: BlockId,
    pub(crate) exprs: Vec<Expr>,
}

impl HirBaseBlock {
    pub(crate) fn empty() -> HirBaseBlock {
        HirBaseBlock {
            id: BlockId::new(),
            exprs: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Goto(BlockId), // Безусловно перейти в блок
    GotoIf(Var, BlockId, BlockId), // По условию 0 перейти в 1 или 2
    Instr(Var, Operation),
    Call(ProgramId, Vec<Var>, Vec<Var>),
}

#[derive(Debug, Clone)]
pub struct Var {
    pub(crate) id: VarId,
    pub(crate) t: VarType
}

impl Var {
    pub(crate) fn new(id: VarId, t: VarType) -> Var {
        Var { id, t }
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    ConstInt(i32),
    ConstBool(bool),

    Add(Var, Var),
    Sub(Var, Var),
    Mul(Var, Var),
    Div(Var, Var),
    Less(Var, Var),
    LessOrEq(Var, Var),
    Great(Var, Var),
    GreatOrEq(Var, Var),
}

#[derive(Debug, Clone, Copy)]
pub enum VarType {
    Int,
    Bool,
    Any,
    AnonFn
}
