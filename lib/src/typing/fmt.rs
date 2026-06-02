//! Модуль с имплами [`std::fmt::Display`] для структур вывод типов.

use colored::Colorize;
use itertools::Itertools;
use std::collections::HashMap;

use crate::{
    Type,
    typing::{
        TypesMap,
        inference::{Constraint, Replacement},
        structs::{StackCfg, StackVar},
    },
};

struct Naming<'t> {
    term_names: HashMap<&'t StackVar, String>,
    stack_names_it: std::iter::Successors<char, fn(&char) -> Option<char>>,
    var_names_it: std::iter::Successors<char, fn(&char) -> Option<char>>,
}

impl<'t> Naming<'t> {
    fn new() -> Naming<'t> {
        // Naming {
        //     term_names: HashMap::new(),
        //     // TODO Придумать, если перестанет хватать букв
        //     stack_names_it: std::iter::successors(Some('A'), |&prev| {
        //         Some((prev as u8 + 1) as char)
        //     }),
        //     var_names_it: std::iter::successors(Some('a'), |&prev| Some((prev as u8 + 1) as char)),
        // }
        Naming {
            term_names: HashMap::new(),
            stack_names_it: std::iter::successors(Some('S'), |&prev| {Some(prev)}),
            var_names_it: std::iter::successors(Some('a'), |&prev| Some(prev)),
        }
    }
}

fn fmt_stack_cfg<'a>(
    f: &mut std::fmt::Formatter,
    stack_cfg: &'a StackCfg,
    naming: &mut Naming<'a>,
) -> std::fmt::Result {
    for (i, term) in stack_cfg.iter().enumerate() {
        term.fmt_inner(f, naming)?;
        if i != stack_cfg.len() - 1 {
            write!(f, " ")?;
        }
    }

    Ok(())
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut naming = Naming::new();

        self.fmt_inner(f, &mut naming)
    }
}

impl Type {
    fn fmt_inner<'a>(
        &'a self,
        f: &mut std::fmt::Formatter,
        naming: &mut Naming<'a>,
    ) -> std::fmt::Result {
        for (i, stack_cfg) in self.seq.iter().enumerate() {
            fmt_stack_cfg(f, stack_cfg, naming)?;
            if i != self.seq.len() - 1 {
                write!(f, "{}", " -> ".bright_white())?;
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for StackVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut naming = Naming::new();

        self.fmt_inner(f, &mut naming)
    }
}

impl StackVar {
    fn fmt_inner<'a>(
        &'a self,
        f: &mut std::fmt::Formatter,
        naming: &mut Naming<'a>,
    ) -> std::fmt::Result {
        match self {
            StackVar::Tail(id) => {
                let name = naming
                    .term_names
                    .entry(self)
                    .or_insert_with(|| naming.stack_names_it.next().unwrap().to_string());
                write!(
                    f,
                    "{}{}",
                    name.bright_green(),
                    id.to_string().bright_green()
                )?;
            }
            StackVar::Var(id) => {
                let name = naming
                    .term_names
                    .entry(self)
                    .or_insert_with(|| naming.var_names_it.next().unwrap().to_string());
                write!(f, "{}{}", name.bright_cyan(), id.to_string().bright_cyan())?;
            }
            StackVar::Quote {
                program_id: _,
                inner,
            } => {
                write!(f, "{}", "(".bright_white())?;
                inner.fmt_inner(f, naming)?;
                write!(f, "{}", ")".bright_white())?;
            }
            StackVar::Int(id) => {
                write!(f, "{}{}", "int".bright_cyan(), id.to_string().bright_cyan())?
            }
            StackVar::Bool(id) => write!(
                f,
                "{}{}",
                "bool".bright_cyan(),
                id.to_string().bright_cyan()
            )?,
        };

        Ok(())
    }
}

impl std::fmt::Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut naming = Naming::new();
        match self {
            Constraint::Unification(from, to) => {
                from.fmt_inner(f, &mut naming)?;
                write!(f, " {} ", "=".truecolor(255, 170, 0))?;
                to.fmt_inner(f, &mut naming)?;
                Ok(())
            }
            Constraint::TailExtension(from, to) => {
                fmt_stack_cfg(f, from, &mut naming)?;
                write!(f, " {} ", "=".truecolor(255, 170, 0))?;
                fmt_stack_cfg(f, to, &mut naming)?;
                Ok(())
            }
        }
    }
}

impl std::fmt::Display for Replacement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Replacement::Stack(from, to) => {
                let mut naming = Naming::new();
                fmt_stack_cfg(f, from, &mut naming)?;
                write!(f, " {} ", "=>".bright_magenta())?;
                fmt_stack_cfg(f, to, &mut naming)?;
                Ok(())
            }
            Replacement::Identity => write!(f, "() {} ()", "=>".bright_magenta()),
        }
    }
}

impl std::fmt::Display for StackCfg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", fmt_vec(self))
    }
}

pub(crate) struct FmtVec<'a, T>(&'a Vec<T>)
where
    T: std::fmt::Display;

impl<'a, T> std::fmt::Display for FmtVec<'a, T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut it = self.0.iter().peekable();
        write!(f, "[")?;
        while let Some(t) = it.next() {
            write!(f, "{}", t)?;
            if it.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")?;
        Ok(())
    }
}

pub(crate) fn fmt_vec<'a, T>(vec: &'a Vec<T>) -> FmtVec<'a, T>
where
    T: std::fmt::Display,
{
    FmtVec(vec)
}

impl std::fmt::Display for TypesMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.0
                .iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .join(", ")
        )
    }
}
