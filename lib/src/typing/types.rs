//! Модель типов

use std::sync::atomic::{AtomicU32, Ordering};

static GLOBAL_ID: AtomicU32 = AtomicU32::new(0);

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Type {
    pub(crate) inp: StackCfg,
    pub(crate) out: StackCfg,
}

impl Type {
    pub(crate) fn new(inp: impl Into<StackCfg>, out: impl Into<StackCfg>) -> Type {
        Type {
            inp: inp.into(),
            out: out.into(),
        }
    }

    /// Тип тривиальной программы -- программы, которая ничего не делает
    pub(crate) fn trivial() -> Type {
        let stack = Term::tail();
        Type::new([stack.clone()], [stack])
    }
}

pub(crate) type StackCfg = Vec<Term>;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Term {
    Tail(Id), // Стек и переменная могут обозначать разные по сути типы,
    Var(Id),  // поэтому надо различать их при помощи идентификаторов

    Quote { inner: Type },

    Int,
    Bool,
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
        Term::Int
    }

    pub(crate) fn bool() -> Term {
        Term::Bool
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Id(u32);

impl Id {
    fn new() -> Id {
        Id(GLOBAL_ID.fetch_add(1, Ordering::Relaxed))
    }
}
