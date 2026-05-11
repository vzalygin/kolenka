//! Модуль с алгоритмами кодогенерации wasm

use std::{collections::{HashMap, HashSet}, fmt::format};

use wasm_encoder::{Function, InstructionSink, RefType, ValType};
use wasmprinter::print_bytes;

use crate::{Context, ProgramId, WasmModule, codegen::{WasmLocalId, WasmType, blocks_graph::{self, BlocksGraph, analyze_blocks_graph}, wasm::{WasmFunctionBundle, WasmFunctionId, WasmModuleBundle}}, hir::{Expr, Hir, HirBaseBlock, HirFunction, Instr, InstrKind, Signature, Var, VarKind}, id::{BlockId, VarId}, prelude::{MAIN_FN_NAME, STD_PRINT_FN_NAME, STD_READ_FN_NAME, WASM_MAIN_FN_NAME, WASM_STD_MODULE_NAME, WASM_STD_PRINT_FN_NAME, WASM_STD_READ_FN_NAME}};

pub fn generate_bytecode(program: &Hir, ctx: &mut Context) -> Vec<u8> {
    let mut module = WasmModule::new();

    let module_bundle = make_module_bundle(&mut module, program);

    for (program_id, hir) in program.iter() {
        ctx.emit_debug(format!("generate program {}", program_id));
        let function_id = module_bundle.functions[program_id];
        let func = generate_function(hir, &module_bundle, &mut ctx.step());
        module.function_definition(function_id, func);

        if let Some(name) = &hir.name && name == MAIN_FN_NAME {
            module.function_export(function_id, WASM_MAIN_FN_NAME);
        }
    }

    let bytecode = module.finish();
    ctx.emit_debug(format!("WAT listing:\n{}", print_bytes(&bytecode).unwrap()));

    bytecode
}

fn generate_function(hir: &HirFunction, module_bundle: &WasmModuleBundle, ctx: &mut Context) -> Function {
    let WasmFunctionBundle { blocks, locals, locals_decl } = make_function_bundle(hir);
    let blocks_graph = analyze_blocks_graph(hir);
    let mut function = Function::new(locals_decl.clone());
    let mut sink = function.instructions();

    ctx.emit_debug(format!("locals {:?}", locals));
    ctx.emit_debug(format!("locals_decl {:?}", locals_decl));

    generate_blocks(
        &module_bundle.functions,
        &locals,
        &blocks,
        &blocks_graph,
        &mut sink,
        &mut HashSet::new(),
        hir.blocks.first(),
        ctx
    );

    for ret in &hir.dataflow.signature.rets {
    let ret = get_local(&locals, &ret.id);
        sink.local_get(*ret);
    }
    sink.end();

    function
}

/// По сути обход в ширину
/// 
/// Возвращает блок, который надо посетить.
fn generate_blocks(
    functions: &HashMap<ProgramId, WasmFunctionId>,
    locals: &HashMap<VarId, WasmLocalId>,
    blocks: &HashMap<BlockId, HirBaseBlock>,
    graph: &BlocksGraph,
    sink: &mut InstructionSink<'_>,
    visited: &mut HashSet<BlockId>,
    init: Option<&HirBaseBlock>,
    ctx: &mut Context
) -> Option<BlockId> {
    let mut block0: Option<&HirBaseBlock> = init;

    while let Some(block) = block0 {
        ctx.emit_debug(format!("generate block {}", block.id));
        assert!(!visited.contains(&block.id), "control flow build failed");
        visited.insert(block.id);

        let ctx = &mut ctx.step();
        for expr in &block.exprs {
            ctx.emit_debug(format!("expr {}", expr));
            match expr {
                Expr::Instr(instr) => match instr.kind {
                    InstrKind::ConstInt(i) => {
                        let res = get_produces(&locals, instr);
                        sink.i32_const(i)
                            .local_set(*res);
                    },
                    InstrKind::ConstBool(b) => {
                        let res = get_produces(&locals, instr);
                        sink.i32_const(if b { 1 } else { 0 })
                            .local_set(*res);
                    },
                    InstrKind::Add => {
                        let ((arg1, arg2), res) = get_triple(&locals, instr);
                        sink.local_get(*arg1)
                            .local_get(*arg2)
                            .i32_add()
                            .local_set(*res);
                    },
                    InstrKind::Sub => {
                        let ((arg1, arg2), res) = get_triple(&locals, instr);
                        sink.local_get(*arg1)
                            .local_get(*arg2)
                            .i32_sub()
                            .local_set(*res);
                    },
                    InstrKind::Mul => {
                        let ((arg1, arg2), res) = get_triple(&locals, instr);
                        sink.local_get(*arg1)
                            .local_get(*arg2)
                            .i32_mul()
                            .local_set(*res);
                    },
                    InstrKind::Div => {
                        let ((arg1, arg2), res) = get_triple(&locals, instr);
                        sink.local_get(*arg1)
                            .local_get(*arg2)
                            .i32_div_s()
                            .local_set(*res);
                    },
                    InstrKind::Less => {
                        let ((arg1, arg2), res) = get_triple(&locals, instr);
                        sink.local_get(*arg1)
                            .local_get(*arg2)
                            .i32_lt_s()
                            .local_set(*res);
                    },
                    InstrKind::LessOrEq => {
                        let ((arg1, arg2), res) = get_triple(&locals, instr);
                        sink.local_get(*arg1)
                            .local_get(*arg2)
                            .i32_le_s()
                            .local_set(*res);
                    },
                    InstrKind::Great => {
                        let ((arg1, arg2), res) = get_triple(&locals, instr);
                        sink.local_get(*arg1)
                            .local_get(*arg2)
                            .i32_gt_s()
                            .local_set(*res);
                    },
                    InstrKind::GreatOrEq => {
                        let ((arg1, arg2), res) = get_triple(&locals, instr);
                        sink.local_get(*arg1)
                            .local_get(*arg2)
                            .i32_ge_s()
                            .local_set(*res);
                    },
                },
                Expr::Goto(next_id) => {
                    let next_block = Option::Some(&blocks[next_id]);
                    if graph.get(next_id).unwrap().predecessors.len() > 1 {
                        // Если несколько блоков ссылаются на один, то их порядок генерации разруливается на уровне выше
                        return Option::Some(*next_id);
                    }
                    block0 = next_block;
                },
                Expr::GotoIf(cond, th, el) => {
                    let arg = get_local(&locals, &cond.id);
                    let then_block = blocks.get(th);
                    let else_block = blocks.get(el);
                    sink.local_get(*arg);
                    sink.if_(wasm_encoder::BlockType::Empty);
                    let then_next = generate_blocks(functions, locals, blocks, graph, sink, visited, then_block, ctx);
                    sink.else_();
                    let else_next = generate_blocks(functions, locals, blocks, graph, sink, visited, else_block, ctx);
                    sink.end();
                    assert!(then_next == else_next, "control flow convergence failed");
                    block0 = then_next.map(|next_id| &blocks[&next_id]);
                },
                Expr::Call(program_id, args, rets) => {
                    for arg in args {
                        sink.local_get(*get_local(&locals, &arg.id));
                    }
                    sink.call(*functions[program_id]);
                    for ret in rets {
                        sink.local_set(*get_local(&locals, &ret.id));
                    }
                },
                Expr::Return => {
                    block0 = Option::None;
                },
            }
        }
    }

    Option::None
}

fn make_module_bundle(module: &mut WasmModule, program: &Hir) -> WasmModuleBundle {
    let mut functions: HashMap<ProgramId, WasmFunctionId> = HashMap::new();

    let std_read_id = module.function_import(WASM_STD_MODULE_NAME, WASM_STD_READ_FN_NAME, WasmType::new([], [ValType::I32]));
    let std_print_id = module.function_import(WASM_STD_MODULE_NAME, WASM_STD_PRINT_FN_NAME, WasmType::new([ValType::I32], []));

    for (program_id, hir) in program.iter() {
        if hir.std {
            let name = &hir.name.clone().unwrap(); 
            if name == STD_READ_FN_NAME {
                functions.insert(*program_id, std_read_id);
            }
            if name == STD_PRINT_FN_NAME {
                functions.insert(*program_id, std_print_id);
            }
        } else {
            let function_type = make_wasm_type(&hir.dataflow.signature);
            let function_id = module.function_declaration(function_type);
            functions.insert(*program_id, function_id);
        }
    }

    WasmModuleBundle { functions }
}

fn make_function_bundle(hir: &HirFunction) -> WasmFunctionBundle {
    let mut locals: HashMap<VarId, WasmLocalId> = HashMap::new();
    let mut locals_decl: Vec<(u32, ValType)> = Vec::new();
    let mut counter: u32 = 0;

    // Сначала идут переменные-аргументы
    for arg_var in &hir.dataflow.signature.args {
        if !locals.contains_key(&arg_var.id) {
            locals.insert(arg_var.id, WasmLocalId::new(counter));
            counter += 1;
        }
    }

    // Затем назначаются остальные переменные
    for (_, var) in &hir.dataflow.vars {
        let var = &var.var;
        if !locals.contains_key(&var.id) {
            locals.insert(*&var.id, WasmLocalId::new(counter));
            locals_decl.push((1, make_val_type(var)));
            counter += 1;
        }
    }

    let blocks: HashMap<BlockId, HirBaseBlock> = hir.blocks.iter()
        .map(|block| (block.id, block.clone()))
        .collect();

    WasmFunctionBundle {
        blocks,
        locals,
        locals_decl,
    }
}

fn make_wasm_type(signature: &Signature) -> WasmType {
    let args: Vec<ValType> = signature.args.iter()
        .map(make_val_type)
        .collect();
    let rets: Vec<ValType> = signature.rets.iter()
        .map(make_val_type)
        .collect();
    WasmType { args, rets }
}

fn make_val_type(var: &Var) -> ValType {
    match var.kind {
        VarKind::Int => ValType::I32,
        VarKind::Bool => ValType::I32,
        VarKind::AnonFn(_) => ValType::Ref(RefType::FUNCREF),
        VarKind::Nothing | VarKind::Any => unreachable!(),
    }
}

fn get_produces(locals: &HashMap<VarId, WasmLocalId>, instr: &Instr) -> WasmLocalId {
    get_local(locals, &instr.produces.id)
}

fn get_triple(locals: &HashMap<VarId, WasmLocalId>, instr: &Instr) -> ((WasmLocalId, WasmLocalId), WasmLocalId) {
    (
        (
            get_local(locals, &instr.consumes.0.id),
            get_local(locals, &instr.consumes.1.id)
        ),
        get_local(locals, &instr.produces.id),
    )
}

fn get_local(locals: &HashMap<VarId, WasmLocalId>, id: &VarId) -> WasmLocalId {
    *locals.get(id).unwrap()
}
