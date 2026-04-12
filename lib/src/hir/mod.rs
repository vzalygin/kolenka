//! Модуль с high IR представлением.
//!
//! High IR имеет SSA форму (?), полезную для выполнения пассов оптимизации и анализа.
//!
//! При преобразовании [`crate::parser::Ast`] в [`Hir`] для каждой команды вычисляется ее трехадресный код. Вывод типов определяет, какая команда принимает какие данные на вход за счет выведенных формы и размера стека. Преобразование в [`Hir`] также накладывает ограничения на тип главной программы -- она не должна принимать на вход дополнительных аргументов (или может принимать, но тогда это аргументы вызова?; подумать над этим).

use std::collections::HashMap;

use crate::{
    Ast, CompilerError, Context, MAIN_FN_NAME, Type,
    hir::{
        dataflow::analyze_dataflow,
        structs::{Hir, HirBaseBlock, HirFunction},
    },
    parser::{AstNode, Program},
    typing::{TypesMap, infer_definitions},
};

mod dataflow;
mod structs;

pub(crate) use structs::{DeclMap, DefMap};

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

pub fn generate_hir(
    ast: &Ast,
    typing_ctx: &mut Context,
    hir_ctx: &mut Context,
) -> Result<Hir, CompilerError> {
    let (mut defs, decls) = init_definitions(ast, typing_ctx);
    hir_ctx.emit_debug(format!("declarations {:?}", decls));
    hir_ctx.emit_debug(format!("preliminary definitions {:?}", defs));
    let types =
        infer_definitions(&decls, &mut defs, typing_ctx).map_err(CompilerError::TypingError)?;
    assert!(
        defs.len() == types.len(),
        "definitions and types should be same size"
    );
    hir_ctx.emit_debug(format!("definitions {:?}", defs));
    hir_ctx.emit_debug(format!("types {:?}", types));
    let _dataflow = analyze_dataflow(&defs, &types);

    let hir = build_hir(defs, types, hir_ctx);

    Ok(hir)
}

fn init_definitions<'p>(ast: &'p Ast, ctx: &mut Context) -> (DefMap<'p>, DeclMap) {
    let mut defs = DefMap(HashMap::new());
    let mut decl = DeclMap(HashMap::new());

    decl.insert(MAIN_FN_NAME.to_string(), ast.program.id);
    defs.insert(ast.program.id, &ast.program);

    for node in &*ast.program {
        // TODO удалить id здесь?
        if let AstNode::Define {
            id: _,
            name,
            value: program,
        } = node
        {
            ctx.emit_debug(format!("define {}\n", name));
            decl.insert(name.clone(), program.id);
            defs.insert(program.id, program);
        }
    }

    (defs, decl)
}

fn build_hir<'p>(_defs: DefMap<'p>, _types: TypesMap, _ctx: &mut Context) -> Hir {
    let functions: HashMap<String, HirFunction> = HashMap::new();

    // for (name, t) in types.clone() {
    //     let program = *defs.get(&name).unwrap();
    //     let function = build_function(program, &t, &types);

    //     functions.insert(name.clone(), function);
    // }

    Hir::new(functions)
}

fn build_function(_program: &Program, _t: &Type, _types: &TypesMap) -> HirFunction {
    let bbs: Vec<HirBaseBlock> = Vec::new();
    let _bb_cur = HirBaseBlock::empty();

    HirFunction(bbs)
}
