//! Модуль графов отношений между базовыми блоками
//! 
//! TODO можно ли свести в один алгоритм с dataflow?

use std::{collections::{HashMap, HashSet}, f32::consts::E};

use derived_deref::{Deref, DerefMut};
use itertools::PeekingNext;

use crate::{codegen::blocks_graph, hir::{Expr, ExprGoto, ExprGotoIf, HirBaseBlock, HirFunction}, id::BlockId};

#[derive(Debug, Clone, Deref, DerefMut)]
pub(crate) struct BlocksGraph(HashMap<BlockId, BlocksGraphNode>);

#[derive(Debug, Clone)]
pub(crate) struct BlocksGraphNode {
    pub(crate) block_id: BlockId,
    /// Прямые предшественники блока
    pub(crate) predecessors: HashSet<BlockId>,
    /// Прямые потомки блока
    pub(crate) successors: HashSet<BlockId>,
    /// Доминаторы блока -- все блоки, идущие раньше данного по потоку управления
    pub(crate) dominators: HashSet<BlockId>,
}

impl BlocksGraphNode {
    fn new(block_id: BlockId) -> BlocksGraphNode {
        BlocksGraphNode { block_id, predecessors: HashSet::new(), successors: HashSet::new(), dominators: HashSet::new() }
    }
}

pub(crate) fn analyze_blocks(hir: &HirFunction) -> BlocksGraph {
    let mut pred_succ_graph = analyze_predecessors_successors(hir);

    analyze_dominators(hir, &mut pred_succ_graph);

    pred_succ_graph
}

fn analyze_predecessors_successors(hir: &HirFunction) -> BlocksGraph {
    let mut blocks_flow = BlocksGraph(HashMap::new());

    for block in &hir.blocks {
        let block_id = block.id;
        blocks_flow.entry(block_id)
            .or_insert_with(|| BlocksGraphNode::new(block_id));

        if let Some(expr) = get_control(block) {
            match expr {
                Expr::Goto(ExprGoto { next }) => {
                    let next = *next;
                    blocks_flow.entry(block_id)
                        .or_insert_with(|| BlocksGraphNode::new(block_id))
                        .successors
                        .insert(next);
                    blocks_flow.entry(next)
                        .or_insert_with(|| BlocksGraphNode::new(next))
                        .predecessors
                        .insert(block_id);
                },
                Expr::GotoIf(ExprGotoIf { cond: _, then_block, else_block }) => {
                    let then_block = *then_block;
                    let else_block = *else_block;

                    let node = blocks_flow.entry(block_id)
                        .or_insert_with(|| BlocksGraphNode::new(block_id));
                    node.successors.insert(then_block);
                    node.successors.insert(else_block);

                    blocks_flow.entry(then_block)
                        .or_insert_with(|| BlocksGraphNode::new(then_block))
                        .predecessors
                        .insert(block_id);
                    blocks_flow.entry(else_block)
                        .or_insert_with(|| BlocksGraphNode::new(else_block))
                        .predecessors
                        .insert(block_id);
                },
                Expr::Return => {},
                Expr::Instr(_) | Expr::Call(_) => unreachable!("wrong base block tail"),
            }
        }
    }

    blocks_flow
}

fn analyze_dominators(hir: &HirFunction, graph: &mut BlocksGraph) {
    if let Some(start) = hir.blocks.first() {
        let blocks: HashMap<BlockId, &HirBaseBlock> = hir.blocks.iter()
            .map(|block| (block.id, block))
            .collect();

        analyze_dominators_walk(&blocks, graph, start, &HashSet::new());
    }
}

fn analyze_dominators_walk(
    blocks: &HashMap<BlockId, &HirBaseBlock>,
    graph: &mut BlocksGraph,
    block: &HirBaseBlock,
    visited: &HashSet<BlockId>,
) {

    if visited.contains(&block.id) {
        return; // Уже посетили вершину
    }
    let mut visited = visited.clone();
    visited.insert(block.id);

    graph.get_mut(&block.id).unwrap().dominators = visited.clone();

    if let Some(expr) = get_control(block) {
        match expr {
            Expr::Goto(ExprGoto { next }) => {
                if should_dominators_visit(graph, next, &visited) {
                    analyze_dominators_walk(blocks, graph, blocks[next], &visited);
                }
            },
            Expr::GotoIf(ExprGotoIf { cond: _, then_block, else_block }) => {
                if should_dominators_visit(graph, then_block, &visited) {
                    analyze_dominators_walk(blocks, graph, blocks[then_block], &visited);
                }
                if should_dominators_visit(graph, else_block, &visited) {
                    analyze_dominators_walk(blocks, graph, blocks[else_block], &visited);
                }
            },
            Expr::Return => {},
            Expr::Instr(_) | Expr::Call(_) => unreachable!("wrong base block tail"),
        }
    }
}

fn get_control(block: &HirBaseBlock) -> Option<&Expr> {
    block.exprs.last()
}

fn should_dominators_visit(graph: &BlocksGraph, block_id: &BlockId, visited: &HashSet<BlockId>) -> bool {
    // Заходим в вершину только тогда, когда посетили всех ее предшественников
    graph[block_id].predecessors.is_subset(visited)
}
