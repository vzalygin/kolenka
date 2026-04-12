//! Модуль построения графа потока данных

use std::collections::HashMap;

use derived_deref::{Deref, DerefMut};

use crate::{
    ProgramId, Type,
    hir::{
        DefMap,
        structs::{Var, VarType},
    },
    id::{OpId, VarId},
    parser::{AstNode, Builtin, Program},
    typing::{StackVar, TypesMap},
};

#[derive(Debug, Clone, Deref, DerefMut)]
pub(crate) struct DataFlowMap(HashMap<ProgramId, DataFlow>);

/// `DataFlow` должен давать представление об устройстве потока данных -- какие ноды и как используют какие данные из выведенных типов.
/// По сути две мапы, характеризующие двудольный граф, у которого в одной доле -- ноды, а в другой -- переменные.
#[derive(Debug, Clone)]
pub(crate) struct DataFlow {
    nodes: HashMap<OpId, DataFlowNode>,
    vars: HashMap<VarId, DataFlowVar>,
}

/// Указывает на зависимости нод от переменных
#[derive(Debug, Clone)]
pub(crate) struct DataFlowNode {
    id: OpId,
    /// Переменные, от которых зависит команда
    depends: Vec<VarId>,
    /// Переменные, произведенные командой
    produces: Vec<VarId>,
}

impl DataFlowNode {
    fn new(
        id: OpId,
        depends: impl Into<Vec<VarId>>,
        produces: impl Into<Vec<VarId>>,
    ) -> DataFlowNode {
        DataFlowNode {
            id,
            depends: depends.into(),
            produces: produces.into(),
        }
    }
}

/// Указывает на зависимости переменных от нод
#[derive(Debug, Clone)]
pub(crate) struct DataFlowVar {
    id: VarId,
    produced: Vec<OpId>,
    depends: Vec<OpId>,
}

impl DataFlowVar {
    fn new(id: VarId) -> DataFlowVar {
        DataFlowVar {
            id,
            depends: [].into(),
            produced: [].into(),
        }
    }

    /// Команды, которые зависят от переменной
    fn push_depends(&mut self, id: OpId) -> &mut Self {
        self.depends.push(id);
        self
    }

    /// Команды, которые произвели переменную
    fn push_produced(&mut self, id: OpId) -> &mut Self {
        self.produced.push(id);
        self
    }
}

pub(crate) fn analyze_dataflow<'n>(defs: &'n DefMap, types: &'n TypesMap) -> DataFlowMap {
    DataFlowMap(
        defs.iter()
            .map(|(program_id, program)| {
                let t = types.get(program_id).unwrap();
                (*program_id, analyze_program_dataflow(program, t))
            })
            .collect(),
    )
}

fn analyze_program_dataflow<'n>(program: &'n Program, t: &'n Type) -> DataFlow {
    let prog_type_merged = program.iter().zip(t.seq.iter().zip(t.seq.iter().skip(1)));

    let mut nodes: HashMap<OpId, DataFlowNode> = HashMap::new();
    let mut vars: HashMap<VarId, DataFlowVar> = HashMap::new();
    for (node, (stack_inp, stack_out)) in prog_type_merged {
        let mut stack_inp = stack_inp.iter().rev();
        let mut stack_out = stack_out.iter().rev();

        match node {
            AstNode::Int { id, value: _ } | AstNode::Bool { id, value: _ } => {
                let var = get_var(&mut stack_out);
                let node = DataFlowNode::new(*id, [], [var.id]);
                vars.entry(var.id)
                    .or_insert(DataFlowVar::new(var.id))
                    .push_produced(*id);
                nodes.insert(*id, node);
            }
            AstNode::BuiltinIdentifier { id, value } => match value {
                Builtin::Add
                | Builtin::Sub
                | Builtin::Mul
                | Builtin::Div
                | Builtin::Less
                | Builtin::LessOrEq
                | Builtin::Great
                | Builtin::GreatOrEq => {
                    let inp1 = get_var(&mut stack_inp);
                    let inp2 = get_var(&mut stack_inp);
                    let out = get_var(&mut stack_out);
                    let dataflow = DataFlowNode::new(*id, [inp2.id, inp1.id], [out.id]);
                    vars.entry(inp1.id)
                        .or_insert(DataFlowVar::new(inp1.id))
                        .push_depends(*id);
                    vars.entry(inp2.id)
                        .or_insert(DataFlowVar::new(inp2.id))
                        .push_depends(*id);
                    vars.entry(inp1.id)
                        .or_insert(DataFlowVar::new(out.id))
                        .push_produced(*id);
                    nodes.insert(*id, dataflow);
                }
                Builtin::Eval => todo!(),
                Builtin::If => todo!(),
                Builtin::While => todo!(),
                Builtin::Pop => todo!(),
                Builtin::Dup => todo!(),
                Builtin::Swap => todo!(),
                Builtin::Quote => todo!(),
                Builtin::Compose => todo!(),
            },
            AstNode::Identifier { id: _, value: _ } => todo!(),
            AstNode::Quote { id: _, value: _ } => todo!(),
            AstNode::Define {
                id: _,
                name: _,
                value: _,
            } => unreachable!("inner defines are not allowed"),
        }
    }

    DataFlow { nodes, vars }
}

fn get_var<'a>(stack_cfg: &mut impl Iterator<Item = &'a StackVar>) -> Var {
    let term = stack_cfg.next().unwrap();
    match term {
        StackVar::Int(id) => Var::new(*id, VarType::Int),
        StackVar::Bool(id) => Var::new(*id, VarType::Bool),
        StackVar::Var(id) => Var::new(*id, VarType::Any),
        StackVar::Quote { inner } => Var::new(inner.id, VarType::AnonFn),
        StackVar::Tail(_) => unreachable!("tried to get var by tail"),
    }
}
