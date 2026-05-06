//! Модуль анализа потока управления программ
//!
//! Генерирует команды трехадресного кода с разбивкой по базовым блокам.


use crate::{
    Context,
    hir::{
        DeclMap, DefMap,
        dataflow::{DataFlow, DataFlowMap},
        structs::{Expr, Hir, HirBaseBlock, HirFunction, Instr, InstrKind, VarKind},
    },
    parser::{AstNode, Builtin, Program},
};

pub(crate) fn construct_hir<'n>(
    decls: &'n DeclMap,
    defs: &'n DefMap,
    dataflow: &'n DataFlowMap,
    ctx: &mut Context,
) -> Hir {
    Hir(defs
        .iter()
        .map(|(program_id, program)| {
            ctx.emit_debug("---");
            let program = *program;
            let dataflow = dataflow.get(program_id).unwrap();

            (
                *program_id,
                build_hir_function(program, dataflow, decls, ctx),
            )
        })
        .collect())
}

pub(crate) fn build_hir_function<'p>(
    program: &'p Program,
    dataflow: &'p DataFlow,
    decls: &'p DeclMap,
    ctx: &mut Context,
) -> HirFunction {
    ctx.emit_debug(format!("building function {}", program.id));

    let mut function = HirFunction::empty(
        program.id,
        decls.revmap().get(&program.id),
        dataflow.clone(),
    );
    let mut curr_bb = HirBaseBlock::new();

    for node in program.iter() {
        let node_id = node.get_id();
        let dfn = dataflow.nodes.get(node_id);

        ctx.emit_debug(format!(
            "node {} dataflow {}",
            node,
            if let Some(dfn) = dfn {
                format!("{}", dfn)
            } else {
                "none".to_string()
            }
        ));

        match node {
            // TODO точно ли нужны эти идентификаторы?
            AstNode::Int { id: _, value } => {
                let var = *dfn.unwrap().get_producer().expect("expected int producer");
                curr_bb.push(Expr::Instr(Instr::imm_int(var, *value)));
            }
            AstNode::Bool { id: _, value } => {
                let var = *dfn.unwrap().get_producer().expect("expected int producer");
                curr_bb.push(Expr::Instr(Instr::imm_bool(var, *value)));
            }
            AstNode::Identifier { id: _, value } => {
                let program_id = *decls.get(value).unwrap();
                let local_signature = dfn.unwrap().get_call().expect("expected call");
                curr_bb.push(Expr::Call(
                    program_id,
                    local_signature.args.clone(),
                    local_signature.rets.clone(),
                ));
            }
            AstNode::BuiltinIdentifier { id: _, value } => match value {
                Builtin::Add => {
                    let triple = dfn.unwrap().get_triple().expect("expected add triple");
                    curr_bb.push(Expr::Instr(Instr::op(
                        InstrKind::Add,
                        triple.ret,
                        triple.args,
                    )));
                }
                Builtin::Sub => {
                    let triple = dfn.unwrap().get_triple().expect("expected sub triple");
                    curr_bb.push(Expr::Instr(Instr::op(
                        InstrKind::Sub,
                        triple.ret,
                        triple.args,
                    )));
                }
                Builtin::Mul => {
                    let triple = dfn.unwrap().get_triple().expect("expected mul triple");
                    curr_bb.push(Expr::Instr(Instr::op(
                        InstrKind::Mul,
                        triple.ret,
                        triple.args,
                    )));
                }
                Builtin::Div => {
                    let triple = dfn.unwrap().get_triple().expect("expected div triple");
                    curr_bb.push(Expr::Instr(Instr::op(
                        InstrKind::Div,
                        triple.ret,
                        triple.args,
                    )));
                }
                Builtin::Less => {
                    let triple = dfn.unwrap().get_triple().expect("expected less triple");
                    curr_bb.push(Expr::Instr(Instr::op(
                        InstrKind::Less,
                        triple.ret,
                        triple.args,
                    )));
                }
                Builtin::LessOrEq => {
                    let triple = dfn
                        .unwrap()
                        .get_triple()
                        .expect("expected less_or_equal triple");
                    curr_bb.push(Expr::Instr(Instr::op(
                        InstrKind::LessOrEq,
                        triple.ret,
                        triple.args,
                    )));
                }
                Builtin::Great => {
                    let triple = dfn.unwrap().get_triple().expect("expected great triple");
                    curr_bb.push(Expr::Instr(Instr::op(
                        InstrKind::Great,
                        triple.ret,
                        triple.args,
                    )));
                }
                Builtin::GreatOrEq => {
                    let triple = dfn
                        .unwrap()
                        .get_triple()
                        .expect("expected great_or_equals triple");
                    curr_bb.push(Expr::Instr(Instr::op(
                        InstrKind::GreatOrEq,
                        triple.ret,
                        triple.args,
                    )));
                }

                // Команды управления
                Builtin::Eval => {
                    let (var, local_signature) =
                        dfn.unwrap().get_call_var().expect("expected anon_fn call");
                    let program_id = if let VarKind::AnonFn(program_id) = var.kind {
                        program_id
                    } else {
                        unreachable!("eval expected anon_fn var")
                    };
                    curr_bb.push(Expr::Call(
                        program_id,
                        local_signature.args.clone(),
                        local_signature.rets.clone(),
                    ));
                }
                Builtin::If => {
                    let if_ = dfn.unwrap().get_if().expect("expected if");

                    let mut then_bb = HirBaseBlock::new();
                    let mut else_bb = HirBaseBlock::new();
                    let next_bb = HirBaseBlock::new();

                    let bool_var = if_.condition;
                    let (then_var, then_local_signature) = &if_.th;
                    let (else_var, else_local_signature) = &if_.el;
                    let VarKind::AnonFn(then_program_id) = then_var.kind else {
                        unreachable!("if then expected anon_fn var")
                    };
                    let VarKind::AnonFn(else_program_id) = else_var.kind else {
                        unreachable!("if else expected anon_fn_var")
                    };

                    curr_bb.push(Expr::GotoIf(bool_var, then_bb.id, else_bb.id));
                    then_bb.push(Expr::Call(
                        then_program_id,
                        then_local_signature.args.clone(),
                        then_local_signature.rets.clone(),
                    ));
                    then_bb.push(Expr::Goto(next_bb.id));
                    else_bb.push(Expr::Call(
                        else_program_id,
                        else_local_signature.args.clone(),
                        else_local_signature.rets.clone(),
                    ));
                    else_bb.push(Expr::Goto(next_bb.id));
                    function.push(curr_bb);
                    function.push(then_bb);
                    function.push(else_bb);

                    curr_bb = next_bb;
                }
                Builtin::While => {
                    // TODO
                }

                // Структурные команды -- ничего генерировать не надо, так как весь поток выведен на уровне типов
                Builtin::Pop => {}
                Builtin::Dup => {}
                Builtin::Swap => {}
                Builtin::Quote => {} // TODO вывод типов полностью закрывает функциональность quote, compose ?
                Builtin::Compose => {}
            },
            AstNode::Quote { id: _, value: _ } => { /* Для самой по себе цитаты ничего генерировать не надо */
            }
            AstNode::Define {
                id: _,
                name: _,
                value: _,
            } => {}
        }
    }

    curr_bb.push(Expr::Return);
    function.push(curr_bb);

    function
}
