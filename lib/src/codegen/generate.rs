//! Модуль с алгоритмами кодогенерации wasm

use std::{collections::{HashMap, HashSet}, fmt::format};

use wasm_encoder::{BlockType, Function, InstructionSink, RefType, ValType};
use wasmprinter::print_bytes;

use crate::{Context, ProgramId, WasmModule, codegen::{WasmLocalId, WasmType, blocks_graph::{self, BlocksGraph, analyze_blocks}, wasm::{WasmFunctionBundle, WasmFunctionId, WasmModuleBundle}}, hir::{Expr, ExprCall, ExprGoto, ExprGotoIf, ExprInstr, Hir, HirBaseBlock, HirFunction, InstrKind, Signature, Var, VarKind}, id::{BlockId, VarId}, prelude::{MAIN_FN_NAME, STD_PRINT_FN_NAME, STD_READ_FN_NAME, WASM_MAIN_FN_NAME, WASM_STD_MODULE_NAME, WASM_STD_PRINT_FN_NAME, WASM_STD_READ_FN_NAME}};

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
    let blocks_graph = analyze_blocks(hir);
    let mut function = Function::new(locals_decl.clone());
    let mut sink = function.instructions();
    let init_block = hir.blocks.first();

    ctx.emit_debug(format!("locals {:?}", locals));
    ctx.emit_debug(format!("locals_decl {:?}", locals_decl));

    dispatch(
        &module_bundle.functions,
        &locals,
        &blocks,
        &blocks_graph,
        &mut sink,
        init_block,
        ctx
    );

    for ret in &hir.dataflow.signature.rets {
    let ret = get_local(&locals, &ret.id);
        sink.local_get(*ret);
    }
    sink.end();

    function
}

/// Функция для каждого блока определяет, не является ли он началом какой-то конструкции нелинейного потока управления
/// 
/// Всего таких конструкций на уровне wasm пока 2:
/// 1. IF
/// 2. WHILE
/// 
/// Если встречает признак начала конструкции (переход к блоку начала конструкции), то вызывает обработчик такой конструкции
fn dispatch(
    functions: &HashMap<ProgramId, WasmFunctionId>,
    locals: &HashMap<VarId, WasmLocalId>,
    blocks: &HashMap<BlockId, HirBaseBlock>,
    graph: &BlocksGraph,
    sink: &mut InstructionSink<'_>,
    init: Option<&HirBaseBlock>,
    ctx: &mut Context
) {
    let mut block0 = init;
    while let Some(block) = block0 {
        let expr = generate_block(functions, locals, sink, block, ctx);
        let mut next_block: Option<&HirBaseBlock> = Option::None;
        
        // Признак IF -- блок заканчивается на GotoIf
        if let Expr::GotoIf(goto_if) = expr {
            let next_block_id = generate_if(functions, locals, blocks, graph, sink, goto_if, ctx);
            next_block = blocks.get(next_block_id);
        }

        // Признак LOOP -- блок заканчивается на GOTO в блок, у которого несколько предшественников и хотя бы один из них не является доминатором
        // (то есть предшественник из будущего, который обеспечивает возврат назад)
        if let Expr::Goto(goto) = expr {
            let graph_next_node = graph.get(&goto.next).unwrap();
            let next_block_id = if graph_next_node.predecessors.len() == 2 && !graph_next_node.predecessors.is_subset(&graph_next_node.dominators) {
                generate_loop(functions, locals, blocks, graph, sink, goto, ctx)
            } else {
                &goto.next
            };
            next_block = blocks.get(next_block_id);
        }

        block0 = next_block;
    }
}

fn generate_if<'a>(
    functions: &HashMap<ProgramId, WasmFunctionId>,
    locals: &HashMap<VarId, WasmLocalId>,
    blocks: &'a HashMap<BlockId, HirBaseBlock>,
    graph: &BlocksGraph,
    sink: &mut InstructionSink<'_>,
    goto_if: &ExprGotoIf,
    ctx: &mut Context
) -> &'a BlockId {
    let ExprGotoIf { cond, then_block, else_block } = goto_if;
    let arg = get_local(&locals, &cond.id);
    let then_block = blocks.get(then_block).unwrap();
    let else_block = blocks.get(else_block).unwrap();

    sink.local_get(*arg);
    sink.if_(BlockType::Empty);
    let Expr::Goto(ExprGoto { next: then_next }) = generate_block(functions, locals, sink, then_block, ctx) else {
        unreachable!("if then branch expected goto block")
    };
    sink.else_();
    let Expr::Goto(ExprGoto { next: else_next }) = generate_block(functions, locals, sink, else_block, ctx) else {
        unreachable!("if else branch expected goto block")
    };
    sink.end();
    assert!(then_next == else_next, "control flow convergence failed");
    
    then_next
}

fn generate_loop<'a>(
    functions: &HashMap<ProgramId, WasmFunctionId>,
    locals: &HashMap<VarId, WasmLocalId>,
    blocks: &'a HashMap<BlockId, HirBaseBlock>,
    graph: &BlocksGraph,
    sink: &mut InstructionSink<'_>,
    goto: &ExprGoto, // ссылка на начальный блок с условием
    ctx: &mut Context
) -> &'a BlockId {
    sink.block(BlockType::Empty)
        .loop_(BlockType::Empty);
    let Expr::GotoIf(ExprGotoIf { cond, then_block, else_block }) = generate_block(functions, locals, sink, blocks.get(&goto.next).unwrap(), ctx) else {
        unreachable!("loop cond expected goto if block")
    };
    let cond = *locals.get(&cond.id).unwrap();
    sink.local_get(*cond)
        .i32_eqz()
        .br_if(1);
    let _ = generate_block(functions, locals, sink, blocks.get(&then_block).unwrap(), ctx);
    sink.br(0)
        .end()
        .end();

    else_block
}

fn generate_block<'a>(
    functions: &HashMap<ProgramId, WasmFunctionId>,
    locals: &HashMap<VarId, WasmLocalId>,
    sink: &mut InstructionSink<'_>,
    block: &'a HirBaseBlock,
    ctx: &mut Context
) -> &'a Expr {
    for expr in &block.exprs {
        match expr {
            Expr::Instr(instr) => {
                match instr.kind {
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
                }
            },
            Expr::Call(ExprCall { prog_id: prog, args, rets } ) => {
                for arg in args {
                    sink.local_get(*get_local(&locals, &arg.id));
                }
                sink.call(**functions.get(prog).unwrap());
                for ret in rets.iter().rev() {
                    sink.local_set(*get_local(&locals, &ret.id));
                }
            },
            Expr::Return | Expr::Goto(_) | Expr::GotoIf(_) => {
            },
        }
    }

    block.exprs.last().unwrap()
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

fn get_produces(locals: &HashMap<VarId, WasmLocalId>, instr: &ExprInstr) -> WasmLocalId {
    get_local(locals, &instr.produces.id)
}

fn get_triple(locals: &HashMap<VarId, WasmLocalId>, instr: &ExprInstr) -> ((WasmLocalId, WasmLocalId), WasmLocalId) {
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
