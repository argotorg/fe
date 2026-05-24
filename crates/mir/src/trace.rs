use trace_facts::{
    CompilerEventFact, CompilerEventKind, CompilerPhase, CompilerReason, DisplayNameFact,
    DisplayNameKind, OriginNodeFact, OriginNodeKind, StorageFact, StorageLocation, StorageReason,
    TraceFact, ValueProperty, ValuePropertyFact,
};

use crate::{
    MirDb, RuntimePackage,
    origin::{
        RUNTIME_LOCAL_EXPORT_KIND, RUNTIME_STMT_EXPORT_KIND, RUNTIME_TERMINATOR_EXPORT_KIND,
        RuntimeInstanceOwnerKey, RuntimeLocalOrigin, RuntimeStmtIndex, RuntimeStmtOrigin,
        RuntimeStmtSite, RuntimeTerminatorOrigin, RuntimeTerminatorSite,
    },
    runtime::{RBlockId, RuntimeCarrier, RuntimeLocalLowering, RuntimeLocalRoot},
};
use hir::{
    analysis::{semantic::borrowck::normalize_semantic_body, ty::ty_check::LocalBinding},
    hir_def::{Partial, Pat},
};

/// Emit MIR/runtime-owned trace facts for a runtime package.
///
/// MIR owns runtime statement and terminator identity. Backend storage slots,
/// registers, final instructions, and codegen events are emitted by codegen.
pub fn emit_mir_facts<'db>(db: &'db dyn MirDb, package: RuntimePackage<'db>) -> Vec<TraceFact> {
    let mut facts = Vec::new();
    for function in package.functions(db) {
        let instance = function.instance(db);
        let owner_key = RuntimeInstanceOwnerKey::for_instance(db, instance);
        let body = instance.body(db);
        let semantic_local_info = semantic_local_trace_info(db, instance);
        for (local_index, local) in body.locals.iter().enumerate() {
            let local_key = RuntimeLocalOrigin::new(
                instance,
                crate::runtime::RLocalId::from_u32(local_index as u32),
            )
            .export_key(&owner_key);
            facts.push(origin_node(local_key.clone(), RUNTIME_LOCAL_EXPORT_KIND));
            let source_is_mut = semantic_local_info
                .get(local_index)
                .and_then(|info| info.as_ref())
                .is_some_and(|info| info.is_mut);
            if let Some(Some(info)) = semantic_local_info.get(local_index) {
                facts.push(TraceFact::DisplayName(DisplayNameFact::new(
                    local_key.clone(),
                    DisplayNameKind::SourceLocal,
                    info.name.clone(),
                )));
                if info.is_mut {
                    facts.push(TraceFact::ValueProperty(ValuePropertyFact::new(
                        local_key.clone(),
                        CompilerPhase::Mir,
                        ValueProperty::SourceMutable,
                        Some(CompilerReason::new("source binding is mutable")),
                    )));
                }
            }
            let storage_location = mir_storage_location(local);
            let storage_reason = mir_storage_reason(
                local_index,
                &body.semantic_locals,
                &storage_location,
                source_is_mut,
            );
            facts.push(TraceFact::Storage(StorageFact::new(
                local_key.clone(),
                CompilerPhase::Mir,
                storage_location.clone(),
                storage_reason,
            )));
            facts.push(TraceFact::ValueProperty(ValuePropertyFact::new(
                local_key.clone(),
                CompilerPhase::Mir,
                match storage_location {
                    StorageLocation::MemoryPlace => ValueProperty::MemoryBacked,
                    StorageLocation::SsaValue => ValueProperty::SsaValue,
                    _ => continue,
                },
                Some(CompilerReason::new("MIR storage classification")),
            )));
            let event_key = compiler_event_key(
                &owner_key,
                format!("mir:local:{local_index}:storage_classification"),
            );
            facts.push(origin_node(event_key.clone(), "compiler.event"));
            facts.push(TraceFact::CompilerEvent(CompilerEventFact::new(
                event_key,
                CompilerPhase::Mir,
                CompilerEventKind::Lowering,
                Vec::new(),
                vec![local_key],
                Some(CompilerReason::new(mir_storage_event_reason(
                    &storage_location,
                    storage_reason,
                ))),
            )));
        }
        for (block_index, runtime_block) in body.blocks.iter().enumerate() {
            let block = RBlockId::from_u32(block_index as u32);
            for (stmt_index, _) in runtime_block.stmts.iter().enumerate() {
                let site =
                    RuntimeStmtSite::new(block, RuntimeStmtIndex::from_u32(stmt_index as u32));
                facts.push(origin_node(
                    RuntimeStmtOrigin::new(instance, site).export_key(&owner_key),
                    RUNTIME_STMT_EXPORT_KIND,
                ));
            }
            let terminator_site = RuntimeTerminatorSite::new(block);
            facts.push(origin_node(
                RuntimeTerminatorOrigin::new(instance, terminator_site).export_key(&owner_key),
                RUNTIME_TERMINATOR_EXPORT_KIND,
            ));
        }
    }
    facts
}

fn mir_storage_location(local: &crate::runtime::RLocal<'_>) -> StorageLocation {
    match (&local.carrier, &local.root) {
        (
            _,
            RuntimeLocalRoot::Slot(_) | RuntimeLocalRoot::Ref(_) | RuntimeLocalRoot::Ptr { .. },
        ) => StorageLocation::MemoryPlace,
        (RuntimeCarrier::Value(_), RuntimeLocalRoot::None) => StorageLocation::SsaValue,
        _ => StorageLocation::Unknown,
    }
}

fn mir_storage_reason(
    local_index: usize,
    semantic_locals: &[RuntimeLocalLowering<'_>],
    location: &StorageLocation,
    source_is_mut: bool,
) -> StorageReason {
    if matches!(location, StorageLocation::MemoryPlace) && source_is_mut {
        return StorageReason::MutableLocalLowering;
    }
    match semantic_locals.get(local_index) {
        Some(
            RuntimeLocalLowering::PlaceCarrier { .. }
            | RuntimeLocalLowering::PlaceBoundValue { .. },
        ) => StorageReason::MutableLocalLowering,
        _ => StorageReason::Unknown,
    }
}

fn mir_storage_event_reason(location: &StorageLocation, reason: StorageReason) -> &'static str {
    match (location, reason) {
        (StorageLocation::MemoryPlace, StorageReason::MutableLocalLowering) => {
            "semantic local lowered to MIR memory place"
        }
        (StorageLocation::SsaValue, _) => "semantic local kept as MIR SSA value",
        _ => "runtime local storage classified by MIR lowering",
    }
}

#[derive(Clone, Debug)]
struct SemanticLocalTraceInfo {
    name: String,
    is_mut: bool,
}

fn semantic_local_trace_info<'db>(
    db: &'db dyn MirDb,
    instance: crate::RuntimeInstance<'db>,
) -> Vec<Option<SemanticLocalTraceInfo>> {
    let Some(semantic) = instance.key(db).semantic(db) else {
        return Vec::new();
    };
    let typed_body = semantic.key(db).typed_body(db);
    let Some(body) = typed_body.body() else {
        return Vec::new();
    };
    let Ok(normalized) = normalize_semantic_body(db, semantic) else {
        return Vec::new();
    };
    normalized
        .locals
        .iter()
        .map(|local| {
            local
                .source
                .map(|binding| local_binding_trace_info(db, body, binding))
        })
        .collect()
}

fn local_binding_trace_info<'db>(
    db: &'db dyn MirDb,
    body: hir::hir_def::Body<'db>,
    binding: LocalBinding<'db>,
) -> SemanticLocalTraceInfo {
    SemanticLocalTraceInfo {
        name: local_binding_name(db, body, binding),
        is_mut: binding.is_mut(),
    }
}

fn local_binding_name<'db>(
    db: &'db dyn MirDb,
    body: hir::hir_def::Body<'db>,
    binding: LocalBinding<'db>,
) -> String {
    match binding {
        LocalBinding::Local { pat, .. } => {
            let Partial::Present(Pat::Path(Partial::Present(path), ..)) = pat.data(db, body) else {
                return "_".to_string();
            };
            path.ident(db)
                .to_opt()
                .map(|ident| ident.data(db).to_string())
                .unwrap_or_else(|| "_".to_string())
        }
        LocalBinding::Param { idx, .. } => format!("%param{idx}"),
        LocalBinding::EffectParam {
            binding_name, idx, ..
        } => binding_name
            .data(db)
            .is_empty()
            .then(|| format!("%effect{idx}"))
            .unwrap_or_else(|| binding_name.data(db).to_string()),
    }
}

fn compiler_event_key(
    owner_key: &RuntimeInstanceOwnerKey,
    local_key: impl AsRef<str>,
) -> common::origin::OriginExportKey {
    common::origin::OriginExportKey::try_from_raw_parts(
        "compiler.event",
        owner_key.as_str(),
        local_key.as_ref(),
    )
    .expect("MIR compiler event key must be valid")
}

fn origin_node(key: common::origin::OriginExportKey, kind: &str) -> TraceFact {
    TraceFact::OriginNode(OriginNodeFact::new(key, OriginNodeKind::new(kind)))
}
