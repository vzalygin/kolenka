//! Модуль построения графа потока данных

use std::collections::HashMap;

use derived_deref::{Deref, DerefMut};

use crate::{
    Context, ProgramId, Type,
    hir::{
        DeclMap, DefMap,
        structs::{Var, VarKind},
    },
    id::{NodeId, VarId},
    parser::{AstNode, Builtin, Program},
    typing::{StackVar, TypesMap},
};

#[derive(Debug, Clone, Deref, DerefMut)]
pub(crate) struct DataFlowMap(pub(crate) HashMap<ProgramId, DataFlow>);

/// `DataFlow` должен давать представление об устройстве потока данных -- какие ноды и как используют какие данные из выведенных типов. По сути две мапы, характеризующие двудольный граф, у которого в одной доле -- ноды, а в другой -- переменные.
#[derive(Debug, Clone)]
pub(crate) struct DataFlow {
    pub(crate) signature: Signature,
    pub(crate) nodes: HashMap<NodeId, DataFlowNode>,
    pub(crate) vars: HashMap<VarId, DataFlowVar>,
}

/// Указывает на зависимости нод от переменных
#[derive(Debug, Clone)]
pub(crate) enum DataFlowNode {
    /// Только создает переменную
    Producer(Var),
    /// Потребляет две переменные и создает одну
    Triple(VarTriple),
    /// Вызов
    Call(Signature),
    /// Вызов переменной
    CallVar(Var, Signature),
    /// Ветвление с вызовом then и else
    If(VarIf),
}

impl DataFlowNode {
    pub(crate) fn get_producer(&self) -> Option<&Var> {
        if let DataFlowNode::Producer(v) = self {
            Option::Some(v)
        } else {
            Option::None
        }
    }

    pub(crate) fn get_triple(&self) -> Option<&VarTriple> {
        if let DataFlowNode::Triple(triple) = self {
            Option::Some(triple)
        } else {
            Option::None
        }
    }

    pub(crate) fn get_call(&self) -> Option<&Signature> {
        if let DataFlowNode::Call(signature) = self {
            Option::Some(signature)
        } else {
            Option::None
        }
    }

    pub(crate) fn get_call_var(&self) -> Option<(&Var, &Signature)> {
        if let DataFlowNode::CallVar(var, signature) = self {
            Option::Some((var, signature))
        } else {
            Option::None
        }
    }

    pub(crate) fn get_if(&self) -> Option<&VarIf> {
        if let DataFlowNode::If(var_if) = self {
            Option::Some(var_if)
        } else {
            Option::None
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct VarTriple {
    pub(crate) args: (Var, Var),
    pub(crate) ret: Var,
}

#[derive(Debug, Clone)]
pub(crate) struct VarIf {
    pub(crate) condition: Var,
    pub(crate) th: (Var, Signature),
    pub(crate) el: (Var, Signature),
}

/// Указывает на зависимости переменных от нод
#[derive(Debug, Clone)]
pub(crate) struct DataFlowVar {
    pub(crate) var: Var,
    // FIXME возможны дубли?
    pub(crate) produced: Vec<NodeId>,
    pub(crate) depends: Vec<NodeId>,
}

impl DataFlowVar {
    fn new(var: Var) -> DataFlowVar {
        DataFlowVar {
            var,
            depends: [].into(),
            produced: [].into(),
        }
    }

    /// Команды, которые зависят от переменной
    fn push_depends(&mut self, id: NodeId) -> &mut Self {
        self.depends.push(id);
        self
    }

    /// Команды, которые произвели переменную
    fn push_produced(&mut self, id: NodeId) -> &mut Self {
        self.produced.push(id);
        self
    }
}

#[derive(Debug, Clone, Deref, DerefMut)]
struct SignatureMap(HashMap<ProgramId, Signature>);

#[derive(Debug, Clone)]
pub(crate) struct Signature {
    pub(crate) args: Vec<Var>,
    pub(crate) rets: Vec<Var>,
}

impl Signature {
    pub(crate) fn new(args: impl Into<Vec<Var>>, rets: impl Into<Vec<Var>>) -> Signature {
        Signature {
            args: args.into(),
            rets: rets.into(),
        }
    }
}

pub(crate) fn analyze_dataflow<'n>(
    decls: &'n DeclMap,
    defs: &'n DefMap,
    types: &'n TypesMap,
    ctx: &mut Context,
) -> DataFlowMap {
    ctx.emit_debug("prepare dataflow for program".to_string());
    assert!(
        defs.len() == types.len(),
        "definitions and types should be same size"
    );

    let signatures = SignatureMap(
        types
            .iter()
            .map(|(program_id, t)| {
                let ctx = &mut ctx.step();
                ctx.emit_debug(format!("program_id={} prepare signature", program_id));
                (*program_id, analyze_program_signature(t, &mut ctx.step()))
            })
            .collect(),
    );

    DataFlowMap(
        defs.iter()
            .map(|(program_id, program)| {
                let ctx = &mut ctx.step();
                ctx.emit_debug(format!("program_id={} prepare dataflow", program.id));
                let t = types.get(program_id).unwrap();
                (
                    *program_id,
                    analyze_program_dataflow(decls, &signatures, program, t, &mut ctx.step()),
                )
            })
            .collect(),
    )
}

fn analyze_program_signature(t: &Type, ctx: &mut Context) -> Signature {
    let args = t
        .seq
        .first()
        .unwrap()
        .iter()
        .skip(1)
        .rev()
        .map(build_var)
        .collect();
    let rets = t
        .seq
        .last()
        .unwrap()
        .iter()
        .skip(1)
        .rev()
        .map(build_var)
        .collect();
    let signature = Signature { args, rets };

    ctx.emit_debug(format!("collected signature {} for type {}", signature, t));

    signature
}

fn analyze_program_dataflow<'n>(
    decls: &'n DeclMap,
    signatures: &'n SignatureMap,
    program: &'n Program,
    t: &'n Type,
    ctx: &mut Context,
) -> DataFlow {
    let prog_type_merged = program.iter().zip(t.seq.iter().zip(t.seq.iter().skip(1)));

    let mut nodes: HashMap<NodeId, DataFlowNode> = HashMap::new();
    let mut vars: HashMap<VarId, DataFlowVar> = HashMap::new();

    let signature = signatures.get(&program.id).unwrap().clone();

    for (node, (stack_inp, stack_out)) in prog_type_merged {
        ctx.emit_debug(format!("node {}", node));

        let mut stack_inp = stack_inp.iter().rev();
        let mut stack_out = stack_out.iter().rev();

        match node {
            AstNode::Int { id, value: _ } | AstNode::Bool { id, value: _ } => {
                let var = get_var(&mut stack_out);
                let dataflow = DataFlowNode::Producer(var);
                vars.entry(var.id)
                    .or_insert(DataFlowVar::new(var))
                    .push_produced(*id);
                ctx.emit_debug(format!("const dataflow {}", dataflow));
                nodes.insert(*id, dataflow);
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
                    let dataflow = DataFlowNode::Triple(VarTriple {
                        args: (inp2, inp1),
                        ret: out,
                    });
                    vars.entry(inp1.id)
                        .or_insert(DataFlowVar::new(inp1))
                        .push_depends(*id);
                    vars.entry(inp2.id)
                        .or_insert(DataFlowVar::new(inp2))
                        .push_depends(*id);
                    vars.entry(out.id)
                        .or_insert(DataFlowVar::new(out))
                        .push_produced(*id);
                    ctx.emit_debug(format!("op dataflow {}", dataflow));
                    nodes.insert(*id, dataflow);
                }
                Builtin::Eval => {
                    let (inp, t) = get_var_fn(&mut stack_inp);

                    let args: Vec<Var> = stack_inp
                        .take(t.seq.first().unwrap().len() - 1)
                        .map(build_var)
                        .inspect(|&var| {
                            vars.entry(var.id)
                                .or_insert(DataFlowVar::new(var))
                                .push_depends(*id);
                        })
                        .collect();
                    let rets: Vec<Var> = stack_out
                        .take(t.seq.last().unwrap().len() - 1)
                        .map(build_var)
                        .inspect(|&var| {
                            vars.entry(var.id)
                                .or_insert(DataFlowVar::new(var))
                                .push_produced(*id);
                        })
                        .collect();
                    let dataflow = DataFlowNode::CallVar(inp, Signature::new(args, rets));
                    ctx.emit_debug(format!("eval dataflow {}", dataflow));
                    nodes.insert(*id, dataflow);
                }
                Builtin::If => {
                    let (else_branch_var, else_branch_type) = get_var_fn(&mut stack_inp);
                    let (then_branch_var, then_branch_type) = get_var_fn(&mut stack_inp);
                    let condition = get_var(&mut stack_inp);

                    let then_args: Vec<Var> = stack_inp
                        .clone()
                        .take(then_branch_type.seq.first().unwrap().len() - 1)
                        .map(build_var)
                        .inspect(|&var| {
                            vars.entry(var.id)
                                .or_insert(DataFlowVar::new(var))
                                .push_depends(*id);
                        })
                        .collect();
                    let then_rets: Vec<Var> = stack_out
                        .clone()
                        .take(then_branch_type.seq.last().unwrap().len() - 1)
                        .map(build_var)
                        .inspect(|&var| {
                            vars.entry(var.id)
                                .or_insert(DataFlowVar::new(var))
                                .push_produced(*id);
                        })
                        .collect();

                    let else_args: Vec<Var> = stack_inp
                        .clone()
                        .take(else_branch_type.seq.first().unwrap().len() - 1)
                        .map(build_var)
                        .inspect(|&var| {
                            vars.entry(var.id)
                                .or_insert(DataFlowVar::new(var))
                                .push_depends(*id);
                        })
                        .collect();
                    let else_rets: Vec<Var> = stack_out
                        .clone()
                        .take(else_branch_type.seq.last().unwrap().len() - 1)
                        .map(build_var)
                        .inspect(|&var| {
                            vars.entry(var.id)
                                .or_insert(DataFlowVar::new(var))
                                .push_produced(*id);
                        })
                        .collect();
                    let dataflow = DataFlowNode::If(VarIf {
                        condition,
                        th: (then_branch_var, Signature::new(then_args, then_rets)),
                        el: (else_branch_var, Signature::new(else_args, else_rets)),
                    });
                    ctx.emit_debug(format!("if dataflow {}", dataflow));
                    nodes.insert(*id, dataflow);
                }
                Builtin::While => {
                    // TODO
                }

                Builtin::Pop => {}
                Builtin::Dup => {}
                Builtin::Swap => {}
                Builtin::Quote => {}
                Builtin::Compose => {}
            },
            AstNode::Identifier { id, value } => {
                let signature = signatures.get(decls.get(value).unwrap()).unwrap();

                let args: Vec<Var> = stack_inp
                    .clone()
                    .take(signature.args.len())
                    .map(build_var)
                    .inspect(|&var| {
                        vars.entry(var.id)
                            .or_insert(DataFlowVar::new(var))
                            .push_depends(*id);
                    })
                    .collect();
                let rets: Vec<Var> = stack_out
                    .clone()
                    .take(signature.rets.len())
                    .map(build_var)
                    .inspect(|&var| {
                        vars.entry(var.id)
                            .or_insert(DataFlowVar::new(var))
                            .push_produced(*id);
                    })
                    .collect();
                let dataflow = DataFlowNode::Call(Signature::new(args, rets));
                ctx.emit_debug(format!("identifier dataflow {}", dataflow));
                nodes.insert(*id, dataflow);
            }
            AstNode::Quote { id: _, value: _ } => {
                // TODO а это вообще надо?

                // let quote = get_var(&mut stack_out);
                // let node = DataFlowNode::new(*id, [], [quote.id]);
                // vars.entry(quote.id)
                //     .or_insert(DataFlowVar::new(quote))
                //     .push_produced(*id);
                // nodes.insert(*id, node);
            }
            AstNode::Define {
                id: _,
                name: _,
                value: _,
            } => {}
        }
    }

    DataFlow {
        signature,
        nodes,
        vars,
    }
}

fn get_var<'a>(stack_cfg: &mut impl Iterator<Item = &'a StackVar>) -> Var {
    build_var(stack_cfg.next().unwrap())
}

// TODO подумать, возможно стоит убрать дублирование StackVar и Var
fn build_var(stack_var: &StackVar) -> Var {
    match stack_var {
        StackVar::Int(id) => Var::new(*id, VarKind::Int),
        StackVar::Bool(id) => Var::new(*id, VarKind::Bool),
        StackVar::Var(id) => Var::new(*id, VarKind::Any),
        StackVar::Quote { program_id, inner } => Var::new(inner.id, VarKind::AnonFn(*program_id)),
        StackVar::Tail(_) => unreachable!("tried to get var by tail"),
    }
}

fn get_var_fn<'a>(stack_cfg: &mut impl Iterator<Item = &'a StackVar>) -> (Var, &'a Type) {
    let stack_var = stack_cfg.next().unwrap();
    match stack_var {
        StackVar::Quote { program_id, inner } => {
            (Var::new(inner.id, VarKind::AnonFn(*program_id)), inner)
        }
        StackVar::Tail(_) | StackVar::Var(_) | StackVar::Int(_) | StackVar::Bool(_) => {
            unreachable!("expected quote term")
        }
    }
}
