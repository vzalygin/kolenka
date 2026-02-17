//! Модуль с имплами [`std::fmt::Display`] для структур вывод типов.

use colored::Colorize;
use std::collections::HashMap;

use crate::{
    Type,
    typing::types::{StackCfg, Term},
};

fn make_naming_triple<'a>() -> (
    HashMap<&'a Term, String>,
    impl Iterator<Item = char>,
    impl Iterator<Item = char>,
) {
    let term_names: HashMap<&Term, String> = HashMap::new();
    // TODO Придумать, если перестанет хватать букв
    let stack_names_it = std::iter::successors(Some('A'), |&prev| Some((prev as u8 + 1) as char));
    let var_names_it = std::iter::successors(Some('a'), |&prev| Some((prev as u8 + 1) as char));

    (term_names, stack_names_it, var_names_it)
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let (mut term_names, mut stack_names_it, mut var_names_it) = make_naming_triple();

        self.fmt_inner(f, &mut term_names, &mut stack_names_it, &mut var_names_it)
    }
}

impl Type {
    fn fmt_stack_cfg<'a>(
        f: &mut std::fmt::Formatter,
        stack_cfg: &'a StackCfg,
        term_names: &mut HashMap<&'a Term, String>,
        stack_names_it: &mut impl Iterator<Item = char>,
        var_names_it: &mut impl Iterator<Item = char>,
    ) -> std::fmt::Result {
        for (i, term) in stack_cfg.iter().enumerate() {
            term.fmt_inner(f, term_names, stack_names_it, var_names_it)?;
            if i != stack_cfg.len() - 1 {
                write!(f, " ")?;
            }
        }

        Ok(())
    }

    fn fmt_inner<'a>(
        &'a self,
        f: &mut std::fmt::Formatter,
        term_names: &mut HashMap<&'a Term, String>,
        stack_names_it: &mut impl Iterator<Item = char>,
        var_names_it: &mut impl Iterator<Item = char>,
    ) -> std::fmt::Result {
        Type::fmt_stack_cfg(f, &self.inp, term_names, stack_names_it, var_names_it)?;
        write!(f, "{}", " -> ".bright_white())?;
        Type::fmt_stack_cfg(f, &self.out, term_names, stack_names_it, var_names_it)?;

        Ok(())
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let (mut term_names, mut stack_names_it, mut var_names_it) = make_naming_triple();

        self.fmt_inner(f, &mut term_names, &mut stack_names_it, &mut var_names_it)
    }
}

impl Term {
    fn fmt_inner<'a>(
        &'a self,
        f: &mut std::fmt::Formatter,
        term_names: &mut HashMap<&'a Term, String>,
        stack_names_it: &mut impl Iterator<Item = char>,
        var_names_it: &mut impl Iterator<Item = char>,
    ) -> std::fmt::Result {
        match self {
            Term::Tail(_) => {
                let name = term_names
                    .entry(self)
                    .or_insert_with(|| stack_names_it.next().unwrap().to_string());
                write!(f, "{}", name.bright_green())?;
            }
            Term::Var(_) => {
                let name = term_names
                    .entry(self)
                    .or_insert_with(|| var_names_it.next().unwrap().to_string());
                write!(f, "{}", name.bright_cyan())?;
            }
            Term::Quote { inner } => {
                write!(f, "{}", "(".bright_white())?;
                inner.fmt_inner(f, term_names, stack_names_it, var_names_it)?;
                write!(f, "{}", ")".bright_white())?;
            }
            Term::Int => write!(f, "{}", "Int".bright_cyan())?,
            Term::Bool => write!(f, "{}", "Bool".bright_cyan())?,
        };

        Ok(())
    }
}
