use common::origin::OriginKey;
use salsa::Update;

use crate::{RuntimeInstance, runtime::RBlockId};

/// Statement index within a runtime MIR block.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct RuntimeStmtIndex(u32);

impl RuntimeStmtIndex {
    pub const fn from_u32(raw: u32) -> Self {
        Self(raw)
    }

    pub const fn as_u32(self) -> u32 {
        self.0
    }

    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

/// Block-local statement site in runtime MIR.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct RuntimeStmtSite {
    block: RBlockId,
    stmt: RuntimeStmtIndex,
}

impl RuntimeStmtSite {
    pub const fn new(block: RBlockId, stmt: RuntimeStmtIndex) -> Self {
        Self { block, stmt }
    }

    pub const fn block(self) -> RBlockId {
        self.block
    }

    pub const fn stmt(self) -> RuntimeStmtIndex {
        self.stmt
    }
}

/// Block-local terminator site in runtime MIR.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct RuntimeTerminatorSite {
    block: RBlockId,
}

impl RuntimeTerminatorSite {
    pub const fn new(block: RBlockId) -> Self {
        Self { block }
    }

    pub const fn block(self) -> RBlockId {
        self.block
    }
}

/// Origin key for a runtime MIR statement. Statement positions are only
/// meaningful inside the owning runtime instance.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimeStmtOrigin<'db> {
    key: OriginKey<RuntimeInstance<'db>, RuntimeStmtSite>,
}

impl<'db> RuntimeStmtOrigin<'db> {
    pub const fn new(instance: RuntimeInstance<'db>, site: RuntimeStmtSite) -> Self {
        Self {
            key: OriginKey::new(instance, site),
        }
    }

    pub fn instance(self) -> RuntimeInstance<'db> {
        self.key.into_parts().0
    }

    pub fn site(self) -> RuntimeStmtSite {
        self.key.into_parts().1
    }
}

/// Origin key for a runtime MIR terminator. Terminators are represented
/// separately from statements to prevent accidental statement/terminator mixes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimeTerminatorOrigin<'db> {
    key: OriginKey<RuntimeInstance<'db>, RuntimeTerminatorSite>,
}

impl<'db> RuntimeTerminatorOrigin<'db> {
    pub const fn new(instance: RuntimeInstance<'db>, site: RuntimeTerminatorSite) -> Self {
        Self {
            key: OriginKey::new(instance, site),
        }
    }

    pub fn instance(self) -> RuntimeInstance<'db> {
        self.key.into_parts().0
    }

    pub fn site(self) -> RuntimeTerminatorSite {
        self.key.into_parts().1
    }
}
