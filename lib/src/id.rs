//! Набор общих компонентов

use std::sync::atomic::{AtomicU32, Ordering};

use derived_deref::{Deref, DerefMut};

static GLOBAL_PROGRAM_ID: AtomicU32 = AtomicU32::new(0);
static GLOBAL_OP_ID: AtomicU32 = AtomicU32::new(0);
static GLOBAL_VAR_ID: AtomicU32 = AtomicU32::new(0);
static GLOBAL_BLOCK_ID: AtomicU32 = AtomicU32::new(0);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Deref, DerefMut)]
pub struct ProgramId(u32);

impl ProgramId {
    pub(crate) fn new() -> ProgramId {
        ProgramId(GLOBAL_PROGRAM_ID.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Deref, DerefMut)]
pub struct OpId(u32);

impl OpId {
    pub(crate) fn new() -> OpId {
        OpId(GLOBAL_OP_ID.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Deref, DerefMut)]
pub struct VarId(u32);

impl VarId {
    pub(crate) fn new() -> VarId {
        VarId(GLOBAL_VAR_ID.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Deref, DerefMut)]
pub struct BlockId(u32);

impl BlockId {
    pub(crate) fn new() -> BlockId {
        BlockId(GLOBAL_PROGRAM_ID.fetch_add(1, Ordering::Relaxed))
    }
}
