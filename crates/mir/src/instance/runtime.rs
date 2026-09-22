use hir::analysis::{
    semantic::{SemanticInstance, check_semantic_borrows, check_semantic_boundaries},
    ty::ty_check::BodyOwner,
};
use salsa::Update;

use crate::{
    db::MirDb,
    runtime::{
        LowerError, LoweredRuntimeBody, RuntimeBody, RuntimeCallEdge, RuntimeClass,
        RuntimeExitBehavior, RuntimeInterfaceSignature, RuntimeSyntheticSpec,
        lower::{
            abi::runtime_declaration_abi_plan,
            body::lower_to_rmir,
            call::{
                collect_referenced_code_regions, collect_referenced_const_regions,
                collect_runtime_calls as collect_runtime_calls_lowered,
            },
            returns::runtime_exit_behavior,
        },
        synthetic::lower_synthetic_runtime_body,
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
        if self.is_external_declaration(db) {
            return Vec::new();
        }
        expect_lowered_runtime_body(db, self).direct_callees(db)
    }

    #[salsa::tracked(return_ref)]
    pub fn referenced_const_regions(
        self,
        db: &'db dyn MirDb,
    ) -> Vec<crate::runtime::ConstRegionId<'db>> {
        if self.is_external_declaration(db) {
            return Vec::new();
        }
        expect_lowered_runtime_body(db, self).referenced_const_regions(db)
    }

    #[salsa::tracked(return_ref)]
    pub fn referenced_code_regions(
        self,
        db: &'db dyn MirDb,
    ) -> Vec<crate::runtime::RuntimeCodeRegion<'db>> {
        if self.is_external_declaration(db) {
            return Vec::new();
        }
        expect_lowered_runtime_body(db, self).referenced_code_regions(db)
    }
}

impl<'db> RuntimeInstance<'db> {
    pub fn is_external_declaration(self, db: &'db dyn MirDb) -> bool {
        self.key(db).semantic(db).is_some_and(|semantic| {
            matches!(semantic.key(db).owner(db), BodyOwner::Func(func) if func.is_extern(db))
        })
    }
}

pub(crate) fn runtime_interface_signature_for_key<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
) -> RuntimeInterfaceSignature<'db> {
    runtime_declaration_abi_plan(db, key).signature()
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
                    diag
                )));
            }
            if let Err(diag) = check_semantic_boundaries(db, semantic) {
                return Err(LowerError::Unsupported(format!(
                    "semantic boundary checking failed for {:?}: {}",
                    semantic.key(db),
                    diag
                )));
            }
            lower_to_rmir(db, instance)?
        }
        RuntimeInstanceSource::Synthetic(synthetic) => {
            lower_synthetic_runtime_body(db, instance, synthetic.spec(db).clone())?
        }
    };
    // Anchor lowering to the canonical class discipline: in debug builds,
    // verify each body as it is produced so a divergence is attributed to
    // the instance being lowered instead of surfacing later at package
    // assembly. Release builds rely on the unconditional package-level
    // verification.
    #[cfg(debug_assertions)]
    if let Err(failure) = crate::verify::verify_runtime_body_detailed(db, &db, &body) {
        panic!(
            "lowering produced an invalid runtime body for {:?}:\n{}",
            instance.key(db).source(db),
            crate::runtime::format_runtime_verify_failure(db, &body, &failure),
        );
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::analysis::{
        semantic::{get_or_build_semantic_instance, identity_semantic_instance_key},
        ty::ty_check::BodyOwner,
    };
    use url::Url;

    #[test]
    fn runtime_lowering_cannot_discharge_pending_semantic_validation() {
        for source in [
            "extern { fn opaque() }\nfn entry() { opaque() }",
            "extern { fn opaque() -> ! }\nfn entry() -> u256 { opaque() }",
            "trait Operation { fn apply() }\nfn entry<T: Operation>() { T::apply() }",
        ] {
            let mut db = DriverDataBase::default();
            let file = db.workspace().touch(
                &mut db,
                Url::parse("file:///pending_runtime_validation.fe").unwrap(),
                Some(source.into()),
            );
            let module = db.top_mod(file);
            let func = module
                .all_funcs(&db)
                .iter()
                .copied()
                .find(|func| {
                    func.name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "entry")
                })
                .unwrap();
            let semantic = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(func)),
            );
            let key =
                RuntimeInstanceKey::new(&db, RuntimeInstanceSource::Semantic(semantic), Vec::new());
            let instance = get_or_build_runtime_instance(&db, key);
            let Err(LowerError::Unsupported(message)) = lower_runtime_body(&db, instance) else {
                panic!("pending validation reached runtime lowering");
            };
            assert!(
                message.contains("requires concrete implementations"),
                "{message}"
            );
        }
    }
}
