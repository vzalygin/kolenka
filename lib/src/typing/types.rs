//! Модель типов.

use std::sync::atomic::{AtomicU32, Ordering};

static GLOBAL_ID: AtomicU32 = AtomicU32::new(0);

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Type {
    pub(crate) seq: Vec<StackCfg>,
}

pub(crate) type StackCfg = Vec<Term>;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Term {
    Tail(Id),
    Var(Id),

    Quote { inner: Type },

    Int(Id),
    Bool(Id),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Id(pub(crate) u32);

impl Type {
    pub(crate) fn new(seq: impl Into<Vec<StackCfg>>) -> Type {
        let seq = seq.into();
        assert!(seq.len() >= 2, "Тип должен иметь хотя бы два состояния");
        Type { seq }
    }

    pub(crate) fn append(self, rhs: impl Iterator<Item = StackCfg>) -> Type {
        Type::new(self.seq.into_iter().chain(rhs).collect::<Vec<_>>())
    }

    pub(crate) fn from_inp_out(inp: impl Into<StackCfg>, out: impl Into<StackCfg>) -> Type {
        Type::new(vec![inp.into(), out.into()])
    }

    /// Тип тривиальной программы -- программы, которая ничего не делает
    pub(crate) fn trivial() -> Type {
        let stack = Term::tail();
        Type::from_inp_out([stack.clone()], [stack])
    }

    pub(crate) fn inp_out(&self) -> (&StackCfg, &StackCfg) {
        (self.seq.first().unwrap(), self.seq.last().unwrap())
    }
}

impl Term {
    pub(crate) fn tail() -> Term {
        Term::Tail(Id::new())
    }

    pub(crate) fn var() -> Term {
        Term::Var(Id::new())
    }

    pub(crate) fn quote(inner: Type) -> Term {
        Term::Quote { inner }
    }

    pub(crate) fn int() -> Term {
        Term::Int(Id::new())
    }

    pub(crate) fn bool() -> Term {
        Term::Bool(Id::new())
    }
}

impl Id {
    fn new() -> Id {
        Id(GLOBAL_ID.fetch_add(1, Ordering::Relaxed))
    }
}
