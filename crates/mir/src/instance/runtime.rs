use hir::analysis::{
    semantic::{SemanticInstance, check_semantic_accesses},
    ty::ty_check::BodyOwner,
};
use salsa::SalsaValue;

use crate::{
    db::MirDb,
    runtime::{
        LowerError, LoweredRuntimeBody, RuntimeBody, RuntimeCallEdge, RuntimeClass,
        RuntimeExitBehavior, RuntimeInterfaceSignature, RuntimeSyntheticSpec,
        lower::{
            abi::runtime_declaration_signature,
            body::lower_to_rmir,
            call::{
                collect_referenced_code_regions, collect_referenced_const_regions,
                collect_runtime_calls as collect_runtime_calls_lowered,
            },
            inline::inline_projections,
            returns::{check_projection_result_spaces, runtime_exit_behavior},
        },
        synthetic::lower_synthetic_runtime_body,
    },
};

#[salsa::interned]
#[derive(Debug)]
pub struct RuntimeSyntheticInstance<'db> {
    pub spec: RuntimeSyntheticSpec<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum RuntimeInstanceSource<'db> {
    Semantic(SemanticInstance<'db>),
    Synthetic(RuntimeSyntheticInstance<'db>),
}

#[salsa::interned]
#[derive(Debug)]
pub struct RuntimeInstanceKey<'db> {
    #[returns(copy)]
    pub source: RuntimeInstanceSource<'db>,
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
    #[returns(copy)]
    pub key: RuntimeInstanceKey<'db>,
}

#[salsa::tracked]
impl<'db> RuntimeInstance<'db> {
    #[salsa::tracked]
    pub fn interface_signature(self, db: &'db dyn MirDb) -> RuntimeInterfaceSignature<'db> {
        runtime_declaration_signature(db, self.key(db))
    }

    #[salsa::tracked(returns(copy))]
    pub fn exit_behavior(self, db: &'db dyn MirDb) -> RuntimeExitBehavior {
        runtime_exit_behavior(db, self.key(db))
    }

    #[salsa::tracked]
    pub fn body(self, db: &'db dyn MirDb) -> RuntimeBody<'db> {
        expect_lowered_runtime_body(db, self).body(db).clone()
    }

    #[salsa::tracked]
    pub fn calls(self, db: &'db dyn MirDb) -> Vec<RuntimeCallEdge<'db>> {
        if self.is_external_declaration(db) {
            return Vec::new();
        }
        expect_lowered_runtime_body(db, self)
            .direct_callees(db)
            .clone()
    }

    #[salsa::tracked]
    pub fn referenced_const_regions(
        self,
        db: &'db dyn MirDb,
    ) -> Vec<crate::runtime::ConstRegionId<'db>> {
        if self.is_external_declaration(db) {
            return Vec::new();
        }
        expect_lowered_runtime_body(db, self)
            .referenced_const_regions(db)
            .clone()
    }

    #[salsa::tracked]
    pub fn referenced_code_regions(
        self,
        db: &'db dyn MirDb,
    ) -> Vec<crate::runtime::RuntimeCodeRegion<'db>> {
        if self.is_external_declaration(db) {
            return Vec::new();
        }
        expect_lowered_runtime_body(db, self)
            .referenced_code_regions(db)
            .clone()
    }
}

impl<'db> RuntimeInstance<'db> {
    pub fn is_external_declaration(self, db: &'db dyn MirDb) -> bool {
        self.key(db).semantic(db).is_some_and(|semantic| {
            matches!(semantic.key(db).owner(db), BodyOwner::Func(func, _) if func.is_extern(db))
        })
    }
}

#[salsa::tracked(returns(copy))]
pub fn get_or_build_runtime_instance<'db>(
    db: &'db dyn MirDb,
    key: RuntimeInstanceKey<'db>,
) -> RuntimeInstance<'db> {
    RuntimeInstance::new(db, key)
}

#[salsa::tracked(returns(clone))]
fn lower_runtime_body<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> Result<LoweredRuntimeBody<'db>, LowerError> {
    let body = match instance.key(db).source(db) {
        RuntimeInstanceSource::Semantic(semantic) => {
            if let Err(diag) = check_semantic_accesses(db, semantic) {
                return Err(LowerError::Unsupported(format!(
                    "semantic borrow checking failed for {:?}: {}",
                    semantic.key(db),
                    diag
                )));
            }
            let mut body = lower_to_rmir(db, instance)?;
            if semantic.is_projection(db) {
                check_projection_result_spaces(db, semantic, body.signature.ret.as_ref())?;
            }
            inline_projections(db, &mut body)?;
            body.elide_proven_variant_asserts();
            body
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
