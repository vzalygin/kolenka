use std::collections::HashMap;

use derived_deref::{Deref, DerefMut};

use crate::{
    hir::{Signature, dataflow::DataFlow},
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
pub struct Hir(#[target] pub(crate) HashMap<ProgramId, HirFunction>);

#[derive(Debug, Clone, Deref, DerefMut)]
pub struct HirFunction {
    pub(crate) id: ProgramId,
    pub(crate) name: Option<String>,
    pub(crate) dataflow: DataFlow,
    #[target]
    pub(crate) blocks: Vec<HirBaseBlock>,
    pub(crate) std: bool,
}

impl HirFunction {
    pub(crate) fn empty(id: ProgramId, name: Option<&String>, dataflow: DataFlow) -> HirFunction {
        HirFunction {
            id,
            name: name.cloned(),
            dataflow,
            blocks: vec![],
            std: false
        }
    }

    pub(crate) fn std(id: ProgramId, name: Option<&String>) -> HirFunction {
        HirFunction {
            id,
            name: name.cloned(),
            dataflow: DataFlow { signature: Signature::new([], []), nodes: HashMap::new(), vars: HashMap::new() },
            blocks: vec![],
            std: true
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
    Goto(ExprGoto),     // Безусловно перейти в блок
    GotoIf(ExprGotoIf), // По условию 0 перейти в 1 или 2
    Instr(ExprInstr),
    Call(ExprCall),
    Return,
}

#[derive(Debug, Clone)]
pub struct ExprGoto {
    pub(crate) next: BlockId,
}

#[derive(Debug, Clone)]
pub struct ExprGotoIf {
    pub(crate) cond: Var, 
    pub(crate) then_block: BlockId,
    pub(crate) else_block: BlockId,
}

#[derive(Debug, Clone)]
pub struct ExprCall {
    pub(crate) prog_id: ProgramId,
    pub(crate) args: Vec<Var>,
    pub(crate) rets: Vec<Var>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
pub struct ExprInstr {
    pub(crate) id: InstrId,
    pub(crate) kind: InstrKind,
    pub(crate) produces: Var,
    pub(crate) consumes: (Var, Var),
}

impl ExprInstr {
    pub(crate) fn new(id: InstrId, kind: InstrKind, produces: Var, consumes: (Var, Var)) -> ExprInstr {
        ExprInstr {
            id,
            kind,
            produces,
            consumes,
        }
    }

    pub(crate) fn imm_int(produces: Var, i: i32) -> ExprInstr {
        ExprInstr::new(
            InstrId::new(),
            InstrKind::ConstInt(i),
            produces,
            (Var::nothing(), Var::nothing()),
        )
    }

    pub(crate) fn imm_bool(produces: Var, b: bool) -> ExprInstr {
        ExprInstr::new(
            InstrId::new(),
            InstrKind::ConstBool(b),
            produces,
            (Var::nothing(), Var::nothing()),
        )
    }

    pub(crate) fn phi(ret: Var, args: (Var, Var)) -> ExprInstr {
        ExprInstr::new(InstrId::new(), InstrKind::Phi, ret, args)
    }

    pub(crate) fn op(kind: InstrKind, ret: Var, args: (Var, Var)) -> ExprInstr {
        ExprInstr::new(InstrId::new(), kind, ret, args)
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

    Phi,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VarKind {
    Int,
    Bool,
    Any,
    AnonFn(ProgramId),
    Nothing,
}
