//! Модуль с high IR представлением.
//!
//! High IR имеет SSA форму (?), полезную для выполнения пассов оптимизации и анализа.
//!
//! При преобразовании [`crate::parser::Ast`] в [`Hir`] для каждой команды вычисляется ее трехадресный код. Вывод типов определяет, какая команда принимает какие данные на вход за счет выведенных формы и размера стека. Преобразование в [`Hir`] также накладывает ограничения на тип главной программы -- она не должна принимать на вход дополнительных аргументов (или может принимать, но тогда это аргументы вызова?; подумать над этим).

use std::collections::HashMap;

use crate::{
    Ast, CompilerError, Context, ProgramId, hir::{
        controlflow::construct_hir,
        dataflow::analyze_dataflow, ssa::ssaify
    }, parser::AstNode, prelude::{MAIN_FN_NAME, STD_PRINT_FN_NAME, STD_READ_FN_NAME}, typing::infer_definitions
};

mod controlflow;
mod dataflow;
mod fmt;
mod structs;
mod ssa;

pub(crate) use crate::hir::{structs::{DeclMap, DefMap, Hir, Var, VarKind, HirFunction, HirBaseBlock, Expr, ExprInstr, ExprGoto, ExprGotoIf, ExprCall, InstrKind}, dataflow::{Signature, DataFlow}};

/*
Пусть у нас программа
1 dup 0 > [ 1 + ] [ 1 - ] if print

Тогда тип для нее
A0 ->
A0 i1 ->
A0 i1 i1 ->
A0 i1 i1 i6 ->
A0 i1 b10 ->
A0 i1 b10 (A0 i1 ->
           A0 i1 i13 ->
           A0 i18) ->
A0 i1 b10 (A0 i1 -> A0 i1 i 13 -> A0 i18) (A0 i1 ->
                                           A0 i1 i21 ->
                                           A0 i18) ->
A0 i18 ->
A0

Из этого можно построить такой hir
anon1:
    i13 = 1
    i18 = i1 + i13
    ret i18
anon2:
    i21 = 1
    i18 = i1 - i21
    ret i18
start:
    i1 = 1
    i1 = i1
    i6 = 0
    b10 = 1 > 0
    if b1 then goto j1 else goto j2
j1:
    i18 = anon1(i1)
j2:
    i18 = anon2(i1)
j3:
    print(i18)
*/

pub fn build_hir(
    ast: &Ast,
    typ_ctx: &mut Context,
    hir_ctx: &mut Context,
) -> Result<Hir, CompilerError> {
    typ_ctx.emit_debug("=== TYPE INFERENCE ===".to_string());
    let (mut defs, decls) = init_definitions(ast, hir_ctx)?;
    typ_ctx.emit_debug(format!("init definitions {}", defs));
    let types =
        infer_definitions(&decls, &mut defs, typ_ctx).map_err(CompilerError::TypingError)?;
    hir_ctx.emit_debug(format!("declarations {}", decls));
    hir_ctx.emit_debug(format!("definitions {}", defs));
    hir_ctx.emit_debug(format!("types {}", types));
    hir_ctx.emit_debug("=== HIR BUILDING ===".to_string());
    let dataflow = analyze_dataflow(&decls, &defs, &types, hir_ctx);
    let hir = construct_hir(&decls, &defs, &dataflow, hir_ctx);
    hir_ctx.emit_debug(format!("HIR listing\n{}", hir));
    hir_ctx.emit_debug("=== SSA BUILDING ===".to_string());
    validate_main(&hir, &decls)?;
    let ssa = ssaify(&hir, hir_ctx);
    hir_ctx.emit_debug(format!("SSA listing\n{}", ssa));
    Ok(hir)
}

fn init_definitions<'p>(ast: &'p Ast, ctx: &mut Context) -> Result<(DefMap<'p>, DeclMap), CompilerError> {
    let mut defs = DefMap(HashMap::new());
    let mut decl = DeclMap(HashMap::new(), HashMap::new());

    add_decl(&mut decl, MAIN_FN_NAME.to_string(), ast.program.id);
    defs.insert(ast.program.id, &ast.program);

    add_decl(&mut decl, STD_READ_FN_NAME.to_string(), ProgramId::new());
    add_decl(&mut decl, STD_PRINT_FN_NAME.to_string(), ProgramId::new());

    for node in &*ast.program {
        // TODO удалить id здесь?
        if let AstNode::Define {
            id: _,
            name,
            value: program,
        } = node
        {
            ctx.emit_debug(format!("define {}", name));
            if decl.0.contains_key(name) {
                return Err(CompilerError::MultipleDefinitionError { name: name.clone() });
            }
            add_decl(&mut decl, name.clone(), program.id);
            defs.insert(program.id, program);
        }
    }

    Ok((defs, decl))
}

fn add_decl(decl: &mut DeclMap, name: String, id: ProgramId) {
    decl.0.insert(name.clone(), id);
    decl.1.insert(id, name);
}

fn validate_main(hir: &Hir, decls: &DeclMap) -> Result<(), CompilerError> {
    let main_id = decls.get(&MAIN_FN_NAME.to_string()).unwrap();
    let main_function = hir.get(main_id).unwrap();

    if main_function.dataflow.signature.args.len() > 0 {
        return Err(CompilerError::LogicError { description: 
            format!("main function must has no parameters, but found {}", main_function.dataflow.signature.args.len()) 
        });
    }
    if main_function.dataflow.signature.rets.len() > 1 {
        return Err(CompilerError::LogicError { description: 
            format!("main function must return 0 or 1 vars, but found {}", main_function.dataflow.signature.rets.len()) 
        });
    }

    Ok(())
}
