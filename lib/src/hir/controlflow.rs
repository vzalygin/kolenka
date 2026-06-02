//! Модуль анализа потока управления программ
//!
//! Генерирует команды трехадресного кода с разбивкой по базовым блокам.


use std::collections::HashMap;

use crate::{
    Context,
    hir::{
        DeclMap, DefMap, Var, dataflow::{DataFlow, DataFlowMap}, structs::{Expr, ExprCall, ExprGoto, ExprGotoIf, ExprInstr, Hir, HirBaseBlock, HirFunction, InstrKind, VarKind}
    },
    parser::{AstNode, Builtin, Program}, prelude::{STD_PRINT_FN_NAME, STD_READ_FN_NAME},
};

pub(crate) fn construct_hir<'n>(
    decls: &'n DeclMap,
    defs: &'n DefMap,
    dataflow: &'n DataFlowMap,
    ctx: &mut Context,
) -> Hir {
    let mut hir = Hir(defs
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
        .collect());

    let std_read_id = *decls.get(STD_READ_FN_NAME).unwrap();
    let std_print_id = *decls.get(STD_PRINT_FN_NAME).unwrap();
    hir.insert(std_read_id, HirFunction::std(std_read_id, Some(&STD_READ_FN_NAME.to_string())));
    hir.insert(std_print_id, HirFunction::std(std_print_id, Some(&STD_PRINT_FN_NAME.to_string())));

    hir
}

pub(crate) fn build_hir_function<'p>(
    prog: &'p Program,
    dataflow: &'p DataFlow,
    decls: &'p DeclMap,
    ctx: &mut Context,
) -> HirFunction {
    ctx.emit_debug(format!("building function {}", prog.id));

    let mut function = HirFunction::empty(
        prog.id,
        decls.revmap().get(&prog.id),
        dataflow.clone(),
    );
    let mut curr_bb = HirBaseBlock::new();

    for node in prog.iter() {
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
                curr_bb.push(Expr::Instr(ExprInstr::imm_int(var, *value)));
            }
            AstNode::Bool { id: _, value } => {
                let var = *dfn.unwrap().get_producer().expect("expected bool producer");
                curr_bb.push(Expr::Instr(ExprInstr::imm_bool(var, *value)));
            }
            AstNode::Identifier { id: _, value } => {
                let program_id = *decls.get(value).unwrap();
                let local_signature = dfn.unwrap().get_call().expect("expected call");
                curr_bb.push(Expr::Call(ExprCall {
                    prog_id: program_id,
                    args: local_signature.args.clone(),
                    rets: local_signature.rets.clone(),
                }));
            }
            AstNode::BuiltinIdentifier { id: _, value } => match value {
                Builtin::Add => {
                    let triple = dfn.unwrap().get_triple().expect("expected add triple");
                    curr_bb.push(Expr::Instr(ExprInstr::op(
                        InstrKind::Add,
                        triple.ret,
                        triple.args,
                    )));
                }
                Builtin::Sub => {
                    let triple = dfn.unwrap().get_triple().expect("expected sub triple");
                    curr_bb.push(Expr::Instr(ExprInstr::op(
                        InstrKind::Sub,
                        triple.ret,
                        triple.args,
                    )));
                }
                Builtin::Mul => {
                    let triple = dfn.unwrap().get_triple().expect("expected mul triple");
                    curr_bb.push(Expr::Instr(ExprInstr::op(
                        InstrKind::Mul,
                        triple.ret,
                        triple.args,
                    )));
                }
                Builtin::Div => {
                    let triple = dfn.unwrap().get_triple().expect("expected div triple");
                    curr_bb.push(Expr::Instr(ExprInstr::op(
                        InstrKind::Div,
                        triple.ret,
                        triple.args,
                    )));
                }
                Builtin::Less => {
                    let triple = dfn.unwrap().get_triple().expect("expected less triple");
                    curr_bb.push(Expr::Instr(ExprInstr::op(
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
                    curr_bb.push(Expr::Instr(ExprInstr::op(
                        InstrKind::LessOrEq,
                        triple.ret,
                        triple.args,
                    )));
                }
                Builtin::Great => {
                    let triple = dfn.unwrap().get_triple().expect("expected great triple");
                    curr_bb.push(Expr::Instr(ExprInstr::op(
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
                    curr_bb.push(Expr::Instr(ExprInstr::op(
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
                    curr_bb.push(Expr::Call(ExprCall {
                        prog_id: program_id,
                        args: local_signature.args.clone(),
                        rets: local_signature.rets.clone()
                    }));
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

                    curr_bb.push(Expr::GotoIf(ExprGotoIf {
                        cond: bool_var,
                        then_block: then_bb.id,
                        else_block: else_bb.id
                    }));
                    then_bb.push(Expr::Call(ExprCall {
                        prog_id: then_program_id,
                        args: then_local_signature.args.clone(),
                        rets: then_local_signature.rets.clone(),
                    }));
                    then_bb.push(Expr::Goto(ExprGoto { next: next_bb.id }));
                    else_bb.push(Expr::Call(ExprCall {
                        prog_id: else_program_id,
                        args: else_local_signature.args.clone(),
                        rets: else_local_signature.rets.clone(),
                    }));
                    else_bb.push(Expr::Goto(ExprGoto { next: next_bb.id }));

                    function.push(curr_bb);
                    function.push(then_bb);
                    function.push(else_bb);

                    curr_bb = next_bb;
                }
                Builtin::While => {
                    let var_loop = dfn.unwrap().get_loop().expect("expected loop");

                    let cond_var = var_loop.condition_var;
                    let (cond_fn_var, cond_signature) = &var_loop.condition;
                    let (body_fn_var, body_signature) = &var_loop.body;
                    let VarKind::AnonFn(cond_program_id) = cond_fn_var.kind else {
                        unreachable!("loop cond expected anon_fn_var")
                    };
                    let VarKind::AnonFn(body_program_id) = body_fn_var.kind else {
                        unreachable!("loop body expected anon_fn_var")
                    };

                    let mut cond_block = HirBaseBlock::new();
                    let mut body_block = HirBaseBlock::new();
                    let next_bb = HirBaseBlock::new();

                    curr_bb.push(Expr::Goto(ExprGoto { next: cond_block.id }));
                    cond_block.push(Expr::Call(ExprCall { prog_id: cond_program_id, args: cond_signature.args.clone(), rets: cond_signature.rets.clone() }));
                    cond_block.push(Expr::GotoIf(ExprGotoIf { cond: cond_var, then_block: body_block.id, else_block: next_bb.id }));
                    body_block.push(Expr::Call(ExprCall { prog_id: body_program_id, args: body_signature.args.clone(), rets: body_signature.rets.clone() }));
                    body_block.push(Expr::Goto(ExprGoto { next: cond_block.id }));

                    function.push(curr_bb);
                    function.push(cond_block);
                    function.push(body_block);

                    curr_bb = next_bb;
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
