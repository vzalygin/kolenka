//! Модель типов

use std::sync::atomic::{AtomicI64, Ordering};

static GLOBAL_ID: AtomicI64 = AtomicI64::new(0);

#[derive(PartialEq, Eq, Clone)]
pub(crate) struct Type {
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
}

pub(crate) type StackCfg = Vec<Term>;

#[derive(PartialEq, Eq, Clone)]
pub(crate) enum Term {
    Stack(Id), // Стек и переменная могут обозначать разные по сути типы,
    Var(Id),   // поэтому надо различать их при помощи идентификаторов

    Quote { inner: Type },

    Int,
    Bool,
}

impl Term {
    pub(crate) fn stack() -> Term {
        Term::Stack(Id::new())
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

#[derive(PartialEq, Eq, Clone)]
struct Id(i64);

impl Id {
    fn new() -> Id {
        Id(GLOBAL_ID.fetch_add(1, Ordering::Relaxed))
    }
}
