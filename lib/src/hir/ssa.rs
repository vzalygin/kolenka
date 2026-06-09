//! Модуль для приведения HIR к SSA форме

use std::collections::{HashMap, HashSet};

use crate::{Context, hir::{Expr, ExprCall, ExprInstr, Hir, HirBaseBlock, HirFunction, Var}, id::VarId};

pub(crate) fn ssaify(input_hir: &Hir, ctx: &mut Context) -> Hir {
    Hir(
        input_hir.iter()
            .map(|(id, function)| {
                ctx.emit_debug(format!("ssaifying {}", id));
                (*id, ssaify_function(function, &mut ctx.step()))
            })
            .collect()
    )
}

fn ssaify_function(input_function: &HirFunction, ctx: &mut Context) -> HirFunction {
    ctx.emit_debug("ssaifying pass 0");
    let (mut changed, mut ssa) = ssaify_pass(input_function.clone(), &mut ctx.step());
    let mut i = 1;

    while changed {
        ctx.emit_debug(format!("ssaifying pass {}", i));
        let (next_changed, ssaified) = ssaify_pass(ssa, &mut ctx.step());
        changed = next_changed;
        ssa = ssaified;
        i += 1;
    }

    ctx.emit_debug(format!("ssaified"));

    ssa
}

fn ssaify_pass(input_function: HirFunction, ctx: &mut Context) -> (bool, HirFunction) { 
    let mut changed: bool = false;
    let mut visited: HashSet<Var> = HashSet::new();
    let mut replaced: HashMap<Var, Var> = HashMap::new();
    let mut function = HirFunction::empty(input_function.id, input_function.name.as_ref(), input_function.dataflow);

    for block in input_function.blocks {
        let mut phis: Vec<Expr> = vec![];
        let mut exprs: Vec<Expr> = vec![];

        for expr in block.iter() {
            let mut expr = expr.clone();
            let (produced, consumed) = get_produced_consumed(&expr);

            for var in consumed {
                // Замена переменных, которые были разведены
                if let Some(replaced) = replaced.get(&var) {
                    let phi_var = Var::new(VarId::new(), var.kind);
                    let phi = Expr::Instr(ExprInstr::phi(phi_var, (var, *replaced)));
                    ctx.emit_debug(format!("emit {}", phi));
                    phis.push(phi);
                    expr = replace_var(expr, var, phi_var);
                    changed = true;
                }
            }

            for var in produced {
                // Замена переменных, которые встречаются повторно
                // TODO если повторы несколько раз?
                if let Some(visited) = visited.get(&var) {
                    let copy_var = Var::new(VarId::new(), var.kind);
                    ctx.emit_debug(format!("breed {} -> {} and {}", var, var, copy_var));
                    replaced.insert(var, copy_var);
                    expr = replace_var(expr, *visited, copy_var);
                    changed = true;
                } else {
                    visited.insert(var);
                }
            }

            exprs.push(expr);
        }

        function.push(HirBaseBlock { id: block.id, exprs: [phis, exprs].concat() });
    }

    (changed, function)
}

fn get_produced_consumed(expr: &Expr) -> (Vec<Var>, Vec<Var>) {
    match expr {
        Expr::Instr(instr) => (vec![instr.produces], vec![instr.consumes.0, instr.consumes.1]),
        Expr::Call(call) => (call.rets.clone(), call.args.clone()),
        Expr::Goto(_) | Expr::Return | Expr::GotoIf(_) => (vec![], vec![]),
    }
}

fn replace_var(expr: Expr, from: Var, to: Var) -> Expr {
    match expr {
        Expr::Instr(instr) => Expr::Instr(ExprInstr::new(
            instr.id,
            instr.kind,
            replace_var_var(instr.produces, from, to),
            (
                replace_var_var(instr.consumes.0, from, to),
                replace_var_var(instr.consumes.1, from, to),
            )
        )),
        Expr::Call(call) => Expr::Call(ExprCall {
            prog_id: call.prog_id,
            args: call.args.iter()
                .map(|var| replace_var_var(*var, from, to))
                .collect(),
            rets: call.rets.iter()
                .map(|var| replace_var_var(*var, from, to))
                .collect(),
        }),
        Expr::Goto(_) | Expr::Return | Expr::GotoIf(_) => expr,
    }
}

fn replace_var_var(var: Var, from: Var, to: Var) -> Var {
    if var.eq(&from) {
        to
    } else {
        var
    }
}
