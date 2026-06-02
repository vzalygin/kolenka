//! Модель типов.
//! BIG TODO пробежаться и сделать замену term -> var

use std::collections::HashMap;

use derived_deref::{Deref, DerefMut};

use crate::{ProgramId, id::VarId};

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Type {
    pub(crate) id: VarId,
    pub(crate) seq: Vec<StackCfg>,
}

impl Type {
    pub(crate) fn new(id: VarId, seq: impl Into<Vec<StackCfg>>) -> Type {
        let seq = seq.into();
        assert!(seq.len() >= 2, "type should have at least 2 configurations");
        Type { id, seq }
    }

    pub(crate) fn append(self, rhs: impl Iterator<Item = StackCfg>) -> Type {
        Type::new(
            VarId::new(),
            self.seq.into_iter().chain(rhs).collect::<Vec<_>>(),
        )
    }

    pub(crate) fn from_inp_out(inp: impl Into<StackCfg>, out: impl Into<StackCfg>) -> Type {
        Type::new(VarId::new(), vec![inp.into(), out.into()])
    }

    pub(crate) fn from_id_inp_out(
        id: VarId,
        inp: impl Into<StackCfg>,
        out: impl Into<StackCfg>,
    ) -> Type {
        Type::new(id, vec![inp.into(), out.into()])
    }

    /// Тип тривиальной программы -- программы, которая ничего не делает
    pub(crate) fn trivial() -> Type {
        let stack = StackVar::tail();
        Type::from_inp_out(StackCfg::new([stack.clone()]), StackCfg::new([stack]))
    }

    pub(crate) fn inp_out(&self) -> (&StackCfg, &StackCfg) {
        (self.seq.first().unwrap(), self.seq.last().unwrap())
    }

    // Клонирование, но только входной и выходной конфигураций
    pub(crate) fn clone_only_inp_out(&self) -> Type {
        let (inp, out) = self.inp_out();
        Type::from_inp_out(inp.clone(), out.clone())
    }

    // Клонирование с заменой идентификаторов `VarId` 
    pub(crate) fn clone_change_id(&self) -> Type {
        let mut replacements = HashMap::new();
        self.clone_change_id_internal(&mut replacements)
    }

    /// При клонировании надо делать замены переменных (например, bool 17 -> bool 24) 
    fn clone_change_id_internal(&self, replacements: &mut HashMap<StackVar, StackVar>) -> Type {
        let new_seq: Vec<StackCfg> = self
            .seq
            .iter()
            .map(|stack_cfg| {
                StackCfg::new(
                    stack_cfg
                        .0
                        .iter()
                        .map(|var| {
                            let replacement = replacements.get(var);

                            if let Some(replacement) = replacement {
                                replacement.clone()
                            } else {
                                let replacement = var.clone_change_id_internal(replacements);
                                replacements
                                    .entry(var.clone())
                                    .or_insert(replacement)
                                    .clone()
                            }
                        })
                        .collect::<Vec<StackVar>>(),
                )
            })
            .collect();

        Type::new(VarId::new(), new_seq)
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, Deref, DerefMut)]
pub struct StackCfg(Vec<StackVar>);

impl StackCfg {
    pub(crate) fn new(v: impl Into<Vec<StackVar>>) -> StackCfg {
        StackCfg(v.into())
    }

    pub(crate) fn empty() -> StackCfg {
        StackCfg::new([])
    }
}

impl<S> From<S> for StackCfg
where
    S: Into<Vec<StackVar>>,
{
    fn from(value: S) -> Self {
        StackCfg::new(value.into())
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum StackVar {
    Tail(VarId),
    Var(VarId),

    Quote { program_id: ProgramId, inner: Type },

    Int(VarId),
    Bool(VarId),
}

impl StackVar {
    pub(crate) fn tail() -> StackVar {
        StackVar::Tail(VarId::new())
    }

    pub(crate) fn var() -> StackVar {
        StackVar::Var(VarId::new())
    }

    pub(crate) fn quote(program_id: ProgramId, inner: Type) -> StackVar {
        StackVar::Quote { program_id, inner }
    }

    pub(crate) fn int() -> StackVar {
        StackVar::Int(VarId::new())
    }

    pub(crate) fn bool() -> StackVar {
        StackVar::Bool(VarId::new())
    }

    /// Клон с сохранением инварианта, но с заменой идентификаторов VarId. ProgramId при этом сохраняется
    pub(crate) fn clone_change_id(&self) -> StackVar {
        let mut replacements = HashMap::new();
        self.clone_change_id_internal(&mut replacements)
    }

    fn clone_change_id_internal(&self, replacements: &mut HashMap<StackVar, StackVar>) -> StackVar {
        match self {
            StackVar::Tail(_) => StackVar::tail(),
            StackVar::Var(_) => StackVar::var(),
            StackVar::Quote { program_id, inner } => StackVar::quote(
                *program_id,
                inner.clone_change_id_internal(replacements),
            ),
            StackVar::Int(_) => StackVar::int(),
            StackVar::Bool(_) => StackVar::bool(),
        }
    }

}
