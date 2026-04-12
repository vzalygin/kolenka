//! Набор общих компонентов

use std::sync::atomic::{AtomicU32, Ordering};

use derived_deref::{Deref, DerefMut};

static GLOBAL_PROGRAM_ID: AtomicU32 = AtomicU32::new(0);
static GLOBAL_NODE_ID: AtomicU32 = AtomicU32::new(0);
static GLOBAL_INSTR_ID: AtomicU32 = AtomicU32::new(0);
static GLOBAL_VAR_ID: AtomicU32 = AtomicU32::new(0);
static GLOBAL_BLOCK_ID: AtomicU32 = AtomicU32::new(0);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Deref, DerefMut)]
pub struct ProgramId(u32);

impl ProgramId {
    pub(crate) fn new() -> ProgramId {
        ProgramId(GLOBAL_PROGRAM_ID.fetch_add(1, Ordering::Relaxed))
    }
}

impl std::fmt::Display for ProgramId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Deref, DerefMut)]
pub struct NodeId(u32);

impl NodeId {
    pub(crate) fn new() -> NodeId {
        NodeId(GLOBAL_NODE_ID.fetch_add(1, Ordering::Relaxed))
    }
}

impl std::fmt::Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Deref, DerefMut)]
pub struct InstrId(u32);

impl InstrId {
    pub(crate) fn new() -> InstrId {
        InstrId(GLOBAL_INSTR_ID.fetch_add(1, Ordering::Relaxed))
    }
}

impl std::fmt::Display for InstrId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Deref, DerefMut)]
pub struct VarId(u32);

impl VarId {
    pub(crate) fn new() -> VarId {
        VarId(GLOBAL_VAR_ID.fetch_add(1, Ordering::Relaxed))
    }
}

impl std::fmt::Display for VarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Deref, DerefMut)]
pub struct BlockId(u32);

impl BlockId {
    pub(crate) fn new() -> BlockId {
        BlockId(GLOBAL_PROGRAM_ID.fetch_add(1, Ordering::Relaxed))
    }
}

impl std::fmt::Display for BlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
