//! Модель типов

use std::{
    collections::HashMap,
    fmt::Display,
    sync::atomic::{AtomicI64, Ordering},
};

static GLOBAL_ID: AtomicI64 = AtomicI64::new(0);

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
        let stack = Term::stack();
        Type::new([stack.clone()], [stack])
    }

    fn fmt_stack_cfg<'t>(
        f: &mut std::fmt::Formatter<'_>,
        stack_cfg: &'t StackCfg,
        term_names: &mut HashMap<&'t Term, String>,
        stack_names_it: &mut impl Iterator<Item = char>,
        var_names_it: &mut impl Iterator<Item = char>,
    ) -> std::fmt::Result {
        for (i, term) in stack_cfg.iter().enumerate() {
            match term {
                Term::Stack(_) => {
                    let name = term_names
                        .entry(term)
                        .or_insert(stack_names_it.next().unwrap().to_string());

                    write!(f, "{}", name)?;
                }
                Term::Var(_) => {
                    let name = term_names
                        .entry(term)
                        .or_insert(var_names_it.next().unwrap().to_string());

                    write!(f, "{}", name)?;
                }
                Term::Quote { inner } => {
                    write!(f, "(")?;
                    inner.fmt(f)?;
                    write!(f, ")")?;
                }
                Term::Int => write!(f, "int")?,
                Term::Bool => write!(f, "bool")?,
            }
            if i != stack_cfg.len() - 1 {
                write!(f, " ")?;
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut term_names: HashMap<&Term, String> = HashMap::new();
        // TODO Придумать, если перестанет хватать букв
        let mut stack_names_it =
            std::iter::successors(Some('A'), |&prev| Some((prev as u8 + 1) as char));
        let mut var_names_it =
            std::iter::successors(Some('a'), |&prev| Some((prev as u8 + 1) as char));

        Type::fmt_stack_cfg(
            f,
            &self.inp,
            &mut term_names,
            &mut stack_names_it,
            &mut var_names_it,
        )?;
        write!(f, " -> ")?;
        Type::fmt_stack_cfg(
            f,
            &self.out,
            &mut term_names,
            &mut stack_names_it,
            &mut var_names_it,
        )?;

        Ok(())
    }
}

pub(crate) type StackCfg = Vec<Term>;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
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

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) struct Id(i64);

impl Id {
    fn new() -> Id {
        Id(GLOBAL_ID.fetch_add(1, Ordering::Relaxed))
    }
}
