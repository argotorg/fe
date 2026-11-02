use hir::analysis::ty::AddressSpace;
use hir::analysis::{
    semantic::{SemanticBody, SemanticInstance, same_owner_effect_binding},
    ty::ty_check::{BodyOwner, LocalBinding},
};
use hir::hir_def::FuncParamMode;

use crate::{
    RuntimeInstanceKey,
    db::MirDb,
    runtime::{RuntimeBoundarySpec, RuntimeParamPlan},
};

use super::{
    classify::{
        RuntimeVisibleBindingPlan, desired_runtime_param_plan, owner_effect_binding_boundary,
    },
    type_info::{RuntimeTypeEnv, top_level_class_for_ty_in_env},
};

pub(crate) fn runtime_param_locals<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    body: &SemanticBody<'db>,
    params: &[crate::runtime::RuntimeClass<'db>],
) -> Vec<hir::analysis::semantic::SLocalId> {
    let entries = runtime_visible_binding_plans(db, semantic);
    if entries.len() != params.len() {
        let owner = semantic.key(db).owner(db);
        let binding_debug = entries
            .iter()
            .map(|entry| {
                let ty = semantic
                    .binding_ty(db, entry.binding)
                    .pretty_print(db)
                    .to_string();
                format!("{:?}:{ty}:plan={:?}", entry.binding, entry.plan)
            })
            .collect::<Vec<_>>()
            .join("; ");
        panic!(
            "failed to map runtime params to semantic locals for {:?} owner={:?}: expected {} runtime-visible params, got {}; params={params:?}; visible_bindings=[{}]",
            semantic.key(db),
            owner,
            entries.len(),
            params.len(),
            binding_debug,
        );
    }
    entries
        .iter()
        .map(|entry| runtime_visible_binding_local(body, entry.binding))
        .collect()
}

/// Whether each runtime parameter of `key` is an access of its caller's
/// place: a view or `mut` parameter, an access part of a product, or an
/// effect. Fe's access rules make such a place exclusive to the call or
/// unwritten during it.
pub fn runtime_param_accesses<'db>(db: &'db dyn MirDb, key: RuntimeInstanceKey<'db>) -> Vec<bool> {
    let Some(semantic) = key.semantic(db) else {
        return Vec::new();
    };
    runtime_visible_binding_plans(db, semantic)
        .iter()
        .map(|entry| match entry.binding {
            LocalBinding::Param { mode, .. } => mode != FuncParamMode::Own,
            LocalBinding::ParamPart { access, .. } => access.is_some(),
            LocalBinding::EffectParam { .. } => true,
            LocalBinding::Local { .. } => false,
        })
        .collect()
}

#[salsa::tracked]
pub(crate) fn runtime_param_plans<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> Vec<RuntimeParamPlan<'db>> {
    let typed_body = semantic.key(db).typed_body(db);
    (0..typed_body.input_bindings().len())
        .map(|idx| desired_runtime_param_plan(db, semantic, typed_body, idx))
        .collect()
}

#[salsa::tracked]
pub(crate) fn runtime_visible_binding_plans<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> Vec<RuntimeVisibleBindingPlan<'db>> {
    let owner = semantic.key(db).owner(db);
    let typed_body = semantic.key(db).typed_body(db);
    let param_plans = runtime_param_plans(db, semantic);
    let mut entries = Vec::new();
    let mut push = |binding, plan| {
        if !matches!(plan, RuntimeParamPlan::Erased) {
            entries.push(RuntimeVisibleBindingPlan {
                binding,
                semantic_ty: semantic.binding_ty(db, binding),
                plan,
            });
        }
    };

    for (idx, &binding) in typed_body.input_bindings().iter().enumerate() {
        push(
            binding,
            param_plans
                .get(idx)
                .cloned()
                .unwrap_or(RuntimeParamPlan::Erased),
        );
    }

    if let BodyOwner::ContractRecvArm {
        contract,
        recv_idx,
        arm_idx,
    } = owner
    {
        let recv = hir::semantic::RecvView::new(db, contract, recv_idx);
        let arm = hir::semantic::RecvArmView::new(db, recv, arm_idx);
        let env = RuntimeTypeEnv::for_semantic(db, semantic);
        for arg_binding in arm.arg_bindings(db) {
            let Some(binding) = typed_body.pat_binding(arg_binding.pat) else {
                continue;
            };
            let ty = semantic.binding_ty(db, binding);
            let plan = top_level_class_for_ty_in_env(db, env, ty, AddressSpace::Memory)
                .map(RuntimeBoundarySpec::ExactTransport)
                .map(RuntimeParamPlan::Boundary)
                .unwrap_or(RuntimeParamPlan::Erased);
            push(binding, plan);
        }
    }

    for binding in semantic.effect_bindings(db) {
        let plan = owner_effect_binding_boundary(db, semantic, binding)
            .map(RuntimeParamPlan::Boundary)
            .unwrap_or(RuntimeParamPlan::Erased);
        push(binding, plan);
    }

    entries
}

pub(crate) fn runtime_visible_binding_local<'db>(
    body: &SemanticBody<'db>,
    binding: LocalBinding<'db>,
) -> hir::analysis::semantic::SLocalId {
    if let Some(local) = body.entry_locals.iter().copied().find(|local| {
        body.local(*local)
            .and_then(|local| local.source)
            .is_some_and(|candidate| {
                candidate == binding || same_owner_effect_binding(candidate, binding)
            })
    }) {
        return local;
    }
    panic!("missing semantic local for runtime-visible binding {binding:?}")
}
