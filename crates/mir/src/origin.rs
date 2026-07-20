use common::facts::TypedFactSet;
use common::origin::{
    OriginExportKey, OriginExportLocalKey, OriginExportOwnerKey, OriginKey, OriginKeyTextError,
    validate_origin_key_text,
};
use cranelift_entity::EntityRef;
use salsa::Update;

use crate::{MirDb, RuntimeInstance, RuntimePackage, runtime::RBlockId, runtime::RLocalId};

pub const RUNTIME_LOCAL_EXPORT_KIND: &str = "runtime.local";
pub const RUNTIME_STMT_EXPORT_KIND: &str = "runtime.stmt";
pub const RUNTIME_TERMINATOR_EXPORT_KIND: &str = "runtime.terminator";
pub const RUNTIME_FUNCTION_EXPORT_KIND: &str = "runtime.function";
pub const RUNTIME_BLOCK_EXPORT_KIND: &str = "runtime.block";
pub const RUNTIME_LOOP_EXPORT_KIND: &str = "runtime.loop";
pub const RUNTIME_TYPE_EXPORT_KIND: &str = "runtime.type";

/// Stable export owner key for a runtime MIR instance.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct RuntimeInstanceOwnerKey(String);

impl RuntimeInstanceOwnerKey {
    pub fn new(value: impl Into<String>) -> Self {
        Self::try_new(value).unwrap_or_else(|err| panic!("{err}"))
    }

    pub fn for_instance<'db>(db: &'db dyn MirDb, instance: RuntimeInstance<'db>) -> Self {
        Self::new(format!(
            "runtime-instance:{}",
            crate::runtime_instance_stable_key(db, instance)
        ))
    }

    pub fn try_new(value: impl Into<String>) -> Result<Self, OriginKeyTextError> {
        let value = value.into();
        validate_origin_key_text("runtime instance owner key", &value)?;
        Ok(Self(value))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl OriginExportOwnerKey for RuntimeInstanceOwnerKey {
    fn as_str(&self) -> &str {
        &self.0
    }
}

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

impl OriginExportLocalKey for RuntimeStmtSite {
    fn to_export_local_key(&self) -> String {
        format!("block:{}:stmt:{}", self.block.index(), self.stmt.index())
    }
}

struct RuntimeLocalOriginLocalKey(RLocalId);

impl OriginExportLocalKey for RuntimeLocalOriginLocalKey {
    fn to_export_local_key(&self) -> String {
        format!("local:{}", self.0.index())
    }
}

struct RuntimeFunctionOriginLocalKey;

impl OriginExportLocalKey for RuntimeFunctionOriginLocalKey {
    fn to_export_local_key(&self) -> String {
        "function".to_string()
    }
}

/// Origin key for a runtime MIR function instance.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimeFunctionOrigin<'db> {
    instance: RuntimeInstance<'db>,
}

impl<'db> RuntimeFunctionOrigin<'db> {
    pub const fn new(instance: RuntimeInstance<'db>) -> Self {
        Self { instance }
    }

    pub const fn instance(self) -> RuntimeInstance<'db> {
        self.instance
    }

    pub fn export_key(self, stable_instance_key: &RuntimeInstanceOwnerKey) -> OriginExportKey {
        OriginExportKey::new(
            RUNTIME_FUNCTION_EXPORT_KIND,
            stable_instance_key,
            &RuntimeFunctionOriginLocalKey,
        )
    }
}

struct RuntimeBlockOriginLocalKey(RBlockId);

impl OriginExportLocalKey for RuntimeBlockOriginLocalKey {
    fn to_export_local_key(&self) -> String {
        format!("block:{}", self.0.index())
    }
}

/// Origin key for a runtime MIR block. Block IDs are scoped to the runtime
/// function instance and must not escape without the owner key.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimeBlockOrigin<'db> {
    key: OriginKey<RuntimeInstance<'db>, RBlockId>,
}

impl<'db> RuntimeBlockOrigin<'db> {
    pub const fn new(instance: RuntimeInstance<'db>, block: RBlockId) -> Self {
        Self {
            key: OriginKey::new(instance, block),
        }
    }

    pub fn instance(self) -> RuntimeInstance<'db> {
        self.key.into_parts().0
    }

    pub fn block(self) -> RBlockId {
        self.key.into_parts().1
    }

    pub fn export_key(self, stable_instance_key: &RuntimeInstanceOwnerKey) -> OriginExportKey {
        OriginExportKey::new(
            RUNTIME_BLOCK_EXPORT_KIND,
            stable_instance_key,
            &RuntimeBlockOriginLocalKey(self.block()),
        )
    }
}

/// One natural-loop candidate produced from a MIR CFG backedge.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct RuntimeLoopSite {
    header: RBlockId,
    latch: RBlockId,
}

impl RuntimeLoopSite {
    pub const fn new(header: RBlockId, latch: RBlockId) -> Self {
        Self { header, latch }
    }

    pub const fn header(self) -> RBlockId {
        self.header
    }

    pub const fn latch(self) -> RBlockId {
        self.latch
    }
}

impl OriginExportLocalKey for RuntimeLoopSite {
    fn to_export_local_key(&self) -> String {
        format!(
            "loop:header:{}:latch:{}",
            self.header.index(),
            self.latch.index()
        )
    }
}

/// Origin key for a compiler-derived MIR natural loop.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimeLoopOrigin<'db> {
    key: OriginKey<RuntimeInstance<'db>, RuntimeLoopSite>,
}

impl<'db> RuntimeLoopOrigin<'db> {
    pub const fn new(instance: RuntimeInstance<'db>, site: RuntimeLoopSite) -> Self {
        Self {
            key: OriginKey::new(instance, site),
        }
    }

    pub fn instance(self) -> RuntimeInstance<'db> {
        self.key.into_parts().0
    }

    pub fn site(self) -> RuntimeLoopSite {
        self.key.into_parts().1
    }

    pub fn export_key(self, stable_instance_key: &RuntimeInstanceOwnerKey) -> OriginExportKey {
        OriginExportKey::new(RUNTIME_LOOP_EXPORT_KIND, stable_instance_key, &self.site())
    }
}

/// Origin key for a runtime MIR local. Local IDs are scoped to a runtime
/// instance and are not stable without the owner key.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub struct RuntimeLocalOrigin<'db> {
    key: OriginKey<RuntimeInstance<'db>, RLocalId>,
}

impl<'db> RuntimeLocalOrigin<'db> {
    pub const fn new(instance: RuntimeInstance<'db>, local: RLocalId) -> Self {
        Self {
            key: OriginKey::new(instance, local),
        }
    }

    pub fn instance(self) -> RuntimeInstance<'db> {
        self.key.into_parts().0
    }

    pub fn local(self) -> RLocalId {
        self.key.into_parts().1
    }

    pub fn export_key(self, stable_instance_key: &RuntimeInstanceOwnerKey) -> OriginExportKey {
        OriginExportKey::new(
            RUNTIME_LOCAL_EXPORT_KIND,
            stable_instance_key,
            &RuntimeLocalOriginLocalKey(self.local()),
        )
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

impl OriginExportLocalKey for RuntimeTerminatorSite {
    fn to_export_local_key(&self) -> String {
        format!("block:{}:terminator", self.block.index())
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

    pub fn export_key(self, stable_instance_key: &RuntimeInstanceOwnerKey) -> OriginExportKey {
        OriginExportKey::new(RUNTIME_STMT_EXPORT_KIND, stable_instance_key, &self.site())
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

    pub fn export_key(self, stable_instance_key: &RuntimeInstanceOwnerKey) -> OriginExportKey {
        OriginExportKey::new(
            RUNTIME_TERMINATOR_EXPORT_KIND,
            stable_instance_key,
            &self.site(),
        )
    }
}

/// Legacy analyze-only origin projection.
///
/// New compiler trace emission must use `mir::trace` plus `fe-trace-facts`
/// instead of returning `common::facts` from MIR.
pub fn legacy_runtime_package_origin_facts<'db>(
    db: &'db dyn MirDb,
    package: RuntimePackage<'db>,
) -> TypedFactSet {
    let mut facts = TypedFactSet::new();
    for function in package.functions(db) {
        let instance = function.instance(db);
        let owner_key = RuntimeInstanceOwnerKey::for_instance(db, instance);
        let body = instance.body(db);
        for (block_index, runtime_block) in body.blocks.iter().enumerate() {
            let block = RBlockId::from_u32(block_index as u32);
            for (stmt_index, _) in runtime_block.stmts.iter().enumerate() {
                let site =
                    RuntimeStmtSite::new(block, RuntimeStmtIndex::from_u32(stmt_index as u32));
                facts.push_origin_node(
                    RuntimeStmtOrigin::new(instance, site).export_key(&owner_key),
                );
            }
            let terminator_site = RuntimeTerminatorSite::new(block);
            facts.push_origin_node(
                RuntimeTerminatorOrigin::new(instance, terminator_site).export_key(&owner_key),
            );
        }
    }
    facts
}
