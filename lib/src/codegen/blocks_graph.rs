//! Модуль графов отношений между базовыми блоками
//! 
//! TODO можно ли свести в один алгоритм с dataflow?

use std::{collections::{HashMap, HashSet}, f32::consts::E};

use derived_deref::{Deref, DerefMut};
use itertools::PeekingNext;

use crate::{codegen::blocks_graph, hir::{Expr, HirFunction}, id::BlockId};

#[derive(Debug, Clone, Deref, DerefMut)]
pub(crate) struct BlocksGraph(HashMap<BlockId, BlocksGraphNode>);

#[derive(Debug, Clone)]
pub(crate) struct BlocksGraphNode {
    pub(crate) block_id: BlockId,
    pub(crate) predecessors: HashSet<BlockId>,
    pub(crate) successors: HashSet<BlockId>,  
}

impl BlocksGraphNode {
    fn new(block_id: BlockId) -> BlocksGraphNode {
        BlocksGraphNode { block_id, predecessors: HashSet::new(), successors: HashSet::new() }
    }
}

pub(crate) fn analyze_blocks_graph(hir: &HirFunction) -> BlocksGraph {
    let mut blocks_flow = BlocksGraph(HashMap::new());

    for block in &hir.blocks {
        let block_id = block.id;
        if let Some(expr) = block.exprs.last() {
            match expr {
                Expr::Goto(next) => {
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
                Expr::GotoIf(_, th, el) => {
                    let th = *th;
                    let el = *el;

                    let node = blocks_flow.entry(block_id)
                        .or_insert_with(|| BlocksGraphNode::new(block_id));
                    node.successors.insert(th);
                    node.successors.insert(el);

                    blocks_flow.entry(th)
                        .or_insert_with(|| BlocksGraphNode::new(th))
                        .predecessors
                        .insert(block_id);
                    blocks_flow.entry(el)
                        .or_insert_with(|| BlocksGraphNode::new(el))
                        .predecessors
                        .insert(block_id);
                },
                Expr::Return => {},
                Expr::Instr(_) | Expr::Call(_, _, _) => unreachable!("wrong base block tail"),
            }
        }
    }

    blocks_flow
}
