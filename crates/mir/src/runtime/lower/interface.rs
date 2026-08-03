use hir::analysis::{
    semantic::{SemanticInstance, owner_effect_bindings, same_owner_effect_binding},
    ty::ty_check::{BodyOwner, LocalBinding},
};

use crate::{
    db::MirDb,
    runtime::{AddressSpaceKind, RuntimeBoundarySpec, RuntimeParamPlan},
};

use super::{
    classify::{
        RuntimeVisibleBindingPlan, desired_runtime_binding_plan, owner_effect_binding_boundary,
    },
    type_info::{RuntimeTypeEnv, top_level_class_for_ty_in_env},
};

pub(crate) fn runtime_param_locals<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
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
                format!(
                    "{:?}:{ty}:plan={:?}:local={:?}",
                    entry.binding, entry.plan, entry.local
                )
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
    entries.iter().map(|entry| entry.local).collect()
}

#[salsa::tracked(return_ref)]
pub(crate) fn runtime_param_plans<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> Vec<RuntimeParamPlan<'db>> {
    semantic
        .key(db)
        .callable_body(db)
        .param_bindings(db)
        .into_iter()
        .map(|binding| desired_runtime_binding_plan(db, semantic, binding))
        .collect()
}

#[salsa::tracked(return_ref)]
pub(crate) fn runtime_visible_binding_plans<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> Vec<RuntimeVisibleBindingPlan<'db>> {
    let owner = semantic.key(db).owner(db);
    let param_plans = runtime_param_plans(db, semantic);
    let mut entries = Vec::new();
    let mut push = |binding, plan| {
        if !matches!(plan, RuntimeParamPlan::Erased) {
            entries.push(RuntimeVisibleBindingPlan {
                binding,
                local: runtime_visible_binding_local(db, semantic, binding),
                semantic_ty: semantic.binding_ty(db, binding),
                plan,
            });
        }
    };

    for (idx, binding) in semantic
        .key(db)
        .callable_body(db)
        .param_bindings(db)
        .into_iter()
        .enumerate()
    {
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
        let typed_body = semantic.key(db).typed_body(db);
        for arg_binding in arm.arg_bindings(db) {
            let Some(binding) = typed_body.pat_binding(arg_binding.pat) else {
                continue;
            };
            let ty = semantic.binding_ty(db, binding);
            let plan = top_level_class_for_ty_in_env(db, env, ty, AddressSpaceKind::Memory)
                .map(RuntimeBoundarySpec::ExactTransport)
                .map(RuntimeParamPlan::Boundary)
                .unwrap_or(RuntimeParamPlan::Erased);
            push(binding, plan);
        }
    }

    for binding in owner_effect_bindings(db, owner) {
        let plan = owner_effect_binding_boundary(db, semantic, binding)
            .map(RuntimeParamPlan::Boundary)
            .unwrap_or(RuntimeParamPlan::Erased);
        push(binding, plan);
    }

    entries
}

fn runtime_visible_binding_local<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
) -> hir::analysis::semantic::SLocalId {
    let body = semantic.body(db);
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
