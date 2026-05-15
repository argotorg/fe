use cranelift_entity::EntityRef;
use hir::analysis::semantic::{SemanticInstance, check_semantic_borrows, check_semantic_noesc};
use salsa::Update;

use crate::{
    db::MirDb,
    runtime::{
        LowerError, LoweredRuntimeBody, RLocalId, RuntimeBody, RuntimeCallEdge, RuntimeClass,
        RuntimeExitBehavior, RuntimeInterfaceSignature, RuntimeParam, RuntimeSyntheticSpec,
        lower::{
            body::lower_to_rmir,
            call::{
                collect_referenced_code_regions, collect_referenced_const_regions,
                collect_runtime_calls as collect_runtime_calls_lowered,
            },
            interface::runtime_param_locals,
            returns::{runtime_exit_behavior, runtime_return_class},
        },
        synthetic::{lower_synthetic_runtime_body, runtime_synthetic_interface_signature},
    },
};

#[salsa::interned]
#[derive(Debug)]
pub struct RuntimeSyntheticInstance<'db> {
    pub spec: RuntimeSyntheticSpec<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum RuntimeInstanceSource<'db> {
    Semantic(SemanticInstance<'db>),
    Synthetic(RuntimeSyntheticInstance<'db>),
}

#[salsa::interned]
#[derive(Debug)]
pub struct RuntimeInstanceKey<'db> {
    pub source: RuntimeInstanceSource<'db>,
    #[return_ref]
    pub params: Vec<RuntimeClass<'db>>,
}

impl<'db> RuntimeInstanceKey<'db> {
    pub fn semantic(self, db: &'db dyn MirDb) -> Option<SemanticInstance<'db>> {
        match self.source(db) {
            RuntimeInstanceSource::Semantic(semantic) => Some(semantic),
            RuntimeInstanceSource::Synthetic(_) => None,
        }
    }
}

#[salsa::tracked]
#[derive(Debug)]
pub struct RuntimeInstance<'db> {
    pub key: RuntimeInstanceKey<'db>,
}

#[salsa::tracked]
impl<'db> RuntimeInstance<'db> {
    #[salsa::tracked]
    pub fn interface_signature(self, db: &'db dyn MirDb) -> RuntimeInterfaceSignature<'db> {
        runtime_interface_signature_for_key(db, self.key(db))
    }

    #[salsa::tracked]
    pub fn exit_behavior(self, db: &'db dyn MirDb) -> RuntimeExitBehavior {
        runtime_exit_behavior(db, self.key(db))
    }

    #[salsa::tracked]
    pub fn body(self, db: &'db dyn MirDb) -> RuntimeBody<'db> {
        expect_lowered_runtime_body(db, self).body(db)
    }

    #[salsa::tracked(return_ref)]
    pub fn calls(self, db: &'db dyn MirDb) -> Vec<RuntimeCallEdge<'db>> {
        expect_lowered_runtime_body(db, self).direct_callees(db)
    }

    #[salsa::tracked(return_ref)]
    pub fn referenced_const_regions(
        self,
        db: &'db dyn MirDb,
    ) -> Vec<crate::runtime::ConstRegionId<'db>> {
        expect_lowered_runtime_body(db, self).referenced_const_regions(db)
    }

    #[salsa::tracked(return_ref)]
    pub fn referenced_code_regions(
        self,
        db: &'db dyn MirDb,
    ) -> Vec<crate::runtime::RuntimeCodeRegion<'db>> {
        expect_lowered_runtime_body(db, self).referenced_code_regions(db)
    }

    #[salsa::tracked(return_ref)]
    pub fn source_table(
        self,
        db: &'db dyn MirDb,
    ) -> common::source_ord::FunctionSourceTable {
        expect_lowered_runtime_body(db, self).body(db).source_table.clone()
    }
}

pub(crate) fn runtime_interface_signature_for_key<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
) -> RuntimeInterfaceSignature<'db> {
    match key.source(db) {
        RuntimeInstanceSource::Semantic(semantic) => RuntimeInterfaceSignature {
            params: key
                .params(db)
                .iter()
                .zip(runtime_param_locals(db, semantic, key.params(db)))
                .map(|(class, local)| RuntimeParam {
                    local: RLocalId::from_u32(local.index() as u32),
                    class: class.clone(),
                })
                .collect(),
            ret: runtime_return_class(db, key),
        },
        RuntimeInstanceSource::Synthetic(synthetic) => {
            runtime_synthetic_interface_signature(synthetic.spec(db).clone())
        }
    }
}

#[salsa::tracked]
pub fn get_or_build_runtime_instance<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
) -> RuntimeInstance<'db> {
    RuntimeInstance::new(db, key)
}

#[salsa::tracked]
fn lower_runtime_body<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> Result<LoweredRuntimeBody<'db>, LowerError> {
    let body = match instance.key(db).source(db) {
        RuntimeInstanceSource::Semantic(semantic) => {
            if let Err(diag) = check_semantic_borrows(db, semantic) {
                return Err(LowerError::Unsupported(format!(
                    "semantic borrow checking failed for {:?}: {}",
                    semantic.key(db),
                    diag.message
                )));
            }
            if let Err(diag) = check_semantic_noesc(db, semantic) {
                return Err(LowerError::Unsupported(format!(
                    "semantic noesc checking failed for {:?}: {}",
                    semantic.key(db),
                    diag.message
                )));
            }
            lower_to_rmir(db, instance)?
        }
        RuntimeInstanceSource::Synthetic(synthetic) => {
            lower_synthetic_runtime_body(db, instance, synthetic.spec(db).clone())
        }
    };
    let direct_callees = collect_runtime_calls_lowered(&body);
    let referenced_const_regions = collect_referenced_const_regions(&body);
    let referenced_code_regions = collect_referenced_code_regions(&body);
    Ok(LoweredRuntimeBody::new(
        db,
        body,
        direct_callees,
        referenced_const_regions,
        referenced_code_regions,
    ))
}

pub(crate) fn runtime_instance_lowered_body<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> Result<LoweredRuntimeBody<'db>, LowerError> {
    lower_runtime_body(db, instance)
}

fn expect_lowered_runtime_body<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> LoweredRuntimeBody<'db> {
    lower_runtime_body(db, instance).unwrap_or_else(|err| {
        panic!(
            "runtime lowering failed for {:?}: {err}",
            instance.key(db).source(db)
        )
    })
}
