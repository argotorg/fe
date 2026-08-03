use std::collections::hash_map::Entry;

use cranelift_entity::EntityRef;
use hir::{
    analysis::{
        semantic::{
            NBorrowRoot, SBlockId, SLocalId, SemanticInstance, SemanticInstanceKey,
            borrowck::{
                NEffectArg, NEffectArgValue, NExpr, NOperand, NSPlace, NSPlaceRoot, NSStmtKind,
                NSTerminatorKind, NormalizedSemanticBody, normalize_semantic_body,
                normalized_cfg_successors, store_rebinds_capability,
            },
            get_or_build_semantic_instance, owner_effect_bindings, same_syntactic_callable_owner,
        },
        ty::{
            corelib::runtime_builtin_func_kind,
            ty_check::{BodyOwner, LocalBinding, ParamSite},
            ty_contains_borrow,
        },
    },
    hir_def::FuncParamMode,
    semantic::ProviderSource,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::db::MirDb;

use super::source::place_root_local;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, salsa::Update)]
pub(super) enum RetainedCapabilityInput {
    Param(u32),
    Effect(u32),
}

struct RetentionCall<'db> {
    callee: SemanticInstanceKey<'db>,
    actual_sources: FxHashMap<RetainedCapabilityInput, FxHashSet<RetainedCapabilityInput>>,
}

enum RetentionNode<'db> {
    Conservative(FxHashSet<RetainedCapabilityInput>),
    Body {
        direct: FxHashSet<RetainedCapabilityInput>,
        calls: Vec<RetentionCall<'db>>,
    },
}

#[salsa::tracked(return_ref)]
pub(super) fn retained_capability_inputs<'db>(
    db: &'db dyn MirDb,
    root_key: SemanticInstanceKey<'db>,
) -> Vec<RetainedCapabilityInput> {
    let root = get_or_build_semantic_instance(db, root_key);
    let mut instances = vec![root];
    let mut node_by_key = FxHashMap::default();
    node_by_key.insert(root_key, 0);
    let mut nodes = vec![None];
    let mut parents = vec![None];
    let mut pending = vec![0];

    while let Some(node_idx) = pending.pop() {
        if nodes[node_idx].is_some() {
            continue;
        }
        let instance = instances[node_idx];
        let key = instance.key(db);
        if is_known_nonretaining_builtin(db, key) {
            nodes[node_idx] = Some(RetentionNode::Body {
                direct: FxHashSet::default(),
                calls: Vec::new(),
            });
            continue;
        }
        if key.typed_body(db).has_smir_lowering_blocker(db) {
            nodes[node_idx] = Some(RetentionNode::Conservative(conservative_retained_inputs(
                db, instance,
            )));
            continue;
        }
        let Ok(body) = normalize_semantic_body(db, instance) else {
            nodes[node_idx] = Some(RetentionNode::Conservative(conservative_retained_inputs(
                db, instance,
            )));
            continue;
        };
        let Some(reachable) = reachable_blocks(db, &body) else {
            nodes[node_idx] = Some(RetentionNode::Conservative(conservative_retained_inputs(
                db, instance,
            )));
            continue;
        };
        let sources = local_input_sources(db, &body, &reachable);
        let external_inputs = external_address_inputs(db, instance);
        let mut direct = FxHashSet::default();
        let mut calls = Vec::new();
        for (block_idx, block) in body.blocks.iter().enumerate() {
            if !reachable[block_idx] {
                continue;
            }
            for stmt in &block.stmts {
                match &stmt.kind {
                    NSStmtKind::Store { dst, src }
                        if store_carries_borrow_transport(db, &body, dst, *src)
                            && place_is_external(&body, dst, &sources, &external_inputs) =>
                    {
                        direct.extend(sources[src.local.index()].iter().copied());
                    }
                    NSStmtKind::Assign {
                        expr:
                            NExpr::Call {
                                callee,
                                args,
                                effect_args,
                                ..
                            },
                        ..
                    } => calls.push(RetentionCall {
                        callee: callee.key,
                        actual_sources: call_actual_sources(&body, &sources, args, effect_args),
                    }),
                    NSStmtKind::Assign { .. } | NSStmtKind::Store { .. } => {}
                }
            }
            if let NSTerminatorKind::Return(Some(value)) = block.terminator.kind {
                let return_ty = body.locals[value.local.index()].ty;
                if return_ty.as_capability(db).is_none() && ty_contains_borrow(db, return_ty) {
                    direct.extend(sources[value.local.index()].iter().copied());
                }
            }
        }

        for callee_key in calls.iter().map(|call| call.callee) {
            let callee_idx = nodes.len();
            let Entry::Vacant(entry) = node_by_key.entry(callee_key) else {
                continue;
            };
            entry.insert(callee_idx);
            let callee = get_or_build_semantic_instance(db, callee_key);
            let callee_owner = callee_key.owner(db);
            let mut ancestor = Some(node_idx);
            let mut expands_recursive_body = false;
            while let Some(ancestor_idx) = ancestor {
                if same_syntactic_callable_owner(
                    callee_owner,
                    instances[ancestor_idx].key(db).owner(db),
                ) {
                    expands_recursive_body = true;
                    break;
                }
                ancestor = parents[ancestor_idx];
            }
            instances.push(callee);
            parents.push(Some(node_idx));
            if expands_recursive_body {
                nodes.push(Some(RetentionNode::Conservative(
                    conservative_retained_inputs(db, callee),
                )));
            } else {
                nodes.push(None);
                pending.push(callee_idx);
            }
        }
        nodes[node_idx] = Some(RetentionNode::Body { direct, calls });
    }

    let nodes = nodes
        .into_iter()
        .map(|node| node.expect("every capability-retention node must be analyzed"))
        .collect::<Vec<_>>();
    let mut retained = nodes
        .iter()
        .map(|node| match node {
            RetentionNode::Conservative(inputs) => inputs.clone(),
            RetentionNode::Body { direct, .. } => direct.clone(),
        })
        .collect::<Vec<_>>();
    loop {
        let mut changed = false;
        for (node_idx, node) in nodes.iter().enumerate() {
            let RetentionNode::Body { calls, .. } = node else {
                continue;
            };
            for call in calls {
                let callee_idx = node_by_key[&call.callee];
                let callee_retained = retained[callee_idx].clone();
                for retained_input in callee_retained {
                    let Some(sources) = call.actual_sources.get(&retained_input) else {
                        continue;
                    };
                    for source in sources {
                        changed |= retained[node_idx].insert(*source);
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }

    let mut retained = retained[node_by_key[&root_key]]
        .iter()
        .copied()
        .collect::<Vec<_>>();
    retained.sort_unstable();
    retained
}

fn is_known_nonretaining_builtin<'db>(db: &'db dyn MirDb, key: SemanticInstanceKey<'db>) -> bool {
    matches!(
        key.owner(db),
        BodyOwner::Func(func) if runtime_builtin_func_kind(db, func).is_some()
    )
}

fn conservative_retained_inputs<'db>(
    db: &'db dyn MirDb,
    instance: SemanticInstance<'db>,
) -> FxHashSet<RetainedCapabilityInput> {
    let key = instance.key(db);
    let mut inputs = FxHashSet::default();
    for (idx, binding) in key
        .callable_body(db)
        .param_bindings(db)
        .into_iter()
        .enumerate()
    {
        if binding_may_carry_borrow(db, instance, binding) {
            inputs.insert(RetainedCapabilityInput::Param(idx as u32));
        }
    }
    for binding in owner_effect_bindings(db, key.owner(db)) {
        if binding_may_carry_borrow(db, instance, binding)
            && let Some(idx) = effect_binding_index(binding)
        {
            inputs.insert(RetainedCapabilityInput::Effect(idx));
        }
    }
    inputs
}

fn binding_may_carry_borrow<'db>(
    db: &'db dyn MirDb,
    instance: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
) -> bool {
    let ty = instance.normalized_binding_ty(db, binding);
    ty_contains_borrow(db, ty) || ty.has_param(db) || ty.has_var(db) || ty.has_projection(db)
}

fn external_address_inputs<'db>(
    db: &'db dyn MirDb,
    instance: SemanticInstance<'db>,
) -> FxHashSet<RetainedCapabilityInput> {
    let key = instance.key(db);
    let mut inputs = FxHashSet::default();
    for (idx, binding) in key
        .callable_body(db)
        .param_bindings(db)
        .into_iter()
        .enumerate()
    {
        let ty = instance.normalized_binding_ty(db, binding);
        if matches!(
            binding,
            LocalBinding::Param {
                mode: FuncParamMode::View,
                ..
            }
        ) || ty.as_capability(db).is_some()
        {
            inputs.insert(RetainedCapabilityInput::Param(idx as u32));
        }
    }
    for binding in owner_effect_bindings(db, key.owner(db)) {
        if let Some(idx) = effect_binding_index(binding) {
            inputs.insert(RetainedCapabilityInput::Effect(idx));
        }
    }
    inputs
}

fn effect_binding_index(binding: LocalBinding<'_>) -> Option<u32> {
    match binding {
        LocalBinding::EffectParam { idx, .. }
        | LocalBinding::Param {
            site: ParamSite::EffectField(_),
            idx,
            ..
        } => Some(idx as u32),
        LocalBinding::Local { .. }
        | LocalBinding::Param {
            site:
                ParamSite::Func(_)
                | ParamSite::ContractInit(_)
                | ParamSite::Closure(_)
                | ParamSite::ClosureEnv(_)
                | ParamSite::ClosureArgs(_),
            ..
        } => None,
    }
}

fn reachable_blocks<'db>(
    db: &'db dyn MirDb,
    body: &NormalizedSemanticBody<'db>,
) -> Option<Vec<bool>> {
    if body.blocks.is_empty() {
        return None;
    }
    let successors = normalized_cfg_successors(db, body);
    if successors.len() != body.blocks.len() {
        return None;
    }
    let mut reachable = vec![false; body.blocks.len()];
    let mut pending = vec![SBlockId::new(0)];
    while let Some(block) = pending.pop() {
        let block_successors = successors.get(block.index())?;
        if std::mem::replace(reachable.get_mut(block.index())?, true) {
            continue;
        }
        if block_successors
            .iter()
            .any(|successor| successor.index() >= body.blocks.len())
        {
            return None;
        }
        pending.extend(block_successors.iter().copied());
    }
    Some(reachable)
}

fn local_input_sources<'db>(
    db: &'db dyn MirDb,
    body: &NormalizedSemanticBody<'db>,
    reachable: &[bool],
) -> Vec<FxHashSet<RetainedCapabilityInput>> {
    let mut seeds = vec![FxHashSet::default(); body.locals.len()];
    let mut dependencies = vec![Vec::new(); body.locals.len()];
    for root in &body.borrow_roots {
        if let NBorrowRoot::Param { local, param_idx } = root {
            seeds[local.index()].insert(RetainedCapabilityInput::Param(*param_idx));
        }
    }
    for (idx, local) in body.locals.iter().enumerate() {
        if let Some(effect) = local
            .facts
            .origin
            .root_provider()
            .and_then(provider_effect_input)
        {
            seeds[idx].insert(effect);
        }
        for source in &local.facts.layout_backing_sources {
            add_place_sources(
                body,
                &source.source,
                &mut seeds[idx],
                &mut dependencies[idx],
            );
        }
    }
    for (block_idx, block) in body.blocks.iter().enumerate() {
        if !reachable[block_idx] {
            continue;
        }
        for stmt in &block.stmts {
            match &stmt.kind {
                NSStmtKind::Assign { dst, expr } => match expr {
                    NExpr::Borrow { place, .. } | NExpr::ReadPlace { place, .. } => {
                        add_place_sources(
                            body,
                            place,
                            &mut seeds[dst.index()],
                            &mut dependencies[dst.index()],
                        );
                    }
                    NExpr::Use(value)
                    | NExpr::Cast { value, .. }
                    | NExpr::ArrayRepeat { value, .. }
                    | NExpr::ExtractEnumField { value, .. }
                    | NExpr::Unary { value, .. }
                    | NExpr::GetEnumTag { value }
                    | NExpr::IsEnumVariant { value, .. } => {
                        dependencies[dst.index()].push(value.local);
                    }
                    NExpr::Binary { lhs, rhs, .. } => {
                        dependencies[dst.index()].push(lhs.local);
                        dependencies[dst.index()].push(rhs.local);
                    }
                    NExpr::AggregateMake { fields, .. } | NExpr::EnumMake { fields, .. } => {
                        dependencies[dst.index()].extend(fields.iter().map(|field| field.local));
                    }
                    NExpr::Call {
                        args, effect_args, ..
                    } => {
                        add_conservative_call_result_sources(
                            db,
                            body,
                            *dst,
                            args,
                            effect_args,
                            &mut seeds[dst.index()],
                            &mut dependencies[dst.index()],
                        );
                    }
                    NExpr::CodeRegionRef { .. }
                    | NExpr::Const(_)
                    | NExpr::CodeRegionOffset { .. }
                    | NExpr::CodeRegionLen { .. } => {}
                },
                NSStmtKind::Store { dst, src }
                    if store_carries_borrow_transport(db, body, dst, *src) =>
                {
                    if let Some(dst) = place_root_local(body, dst) {
                        dependencies[dst.index()].push(src.local);
                    }
                }
                NSStmtKind::Store { .. } => {}
            }
        }
    }

    seeds
        .iter()
        .enumerate()
        .map(|(idx, direct)| {
            let mut sources = direct.clone();
            let mut pending = dependencies[idx].clone();
            let mut visited = FxHashSet::default();
            while let Some(source) = pending.pop() {
                if !visited.insert(source) {
                    continue;
                }
                sources.extend(seeds[source.index()].iter().copied());
                pending.extend(dependencies[source.index()].iter().copied());
            }
            sources
        })
        .collect()
}

fn add_conservative_call_result_sources<'db>(
    db: &'db dyn MirDb,
    body: &NormalizedSemanticBody<'db>,
    dst: SLocalId,
    args: &[NOperand],
    effect_args: &[NEffectArg<'db>],
    inputs: &mut FxHashSet<RetainedCapabilityInput>,
    dependencies: &mut Vec<SLocalId>,
) {
    let result = &body.locals[dst.index()];
    if !result.facts.layout_backing_sources.is_empty() || !ty_contains_borrow(db, result.ty) {
        return;
    }

    for arg in args {
        if ty_contains_borrow(db, body.locals[arg.local.index()].ty) {
            dependencies.push(arg.local);
        }
    }
    for effect in effect_args {
        match &effect.arg {
            NEffectArgValue::Value(value)
                if ty_contains_borrow(db, body.locals[value.local.index()].ty) =>
            {
                dependencies.push(value.local);
            }
            NEffectArgValue::Place(place) => {
                add_place_sources(body, place, inputs, dependencies);
            }
            NEffectArgValue::Value(_) => {}
        }
    }
}

fn provider_effect_input(
    binding: &hir::semantic::ProviderBinding<'_>,
) -> Option<RetainedCapabilityInput> {
    match binding.source {
        ProviderSource::UsesParam {
            requirement_idx, ..
        } => Some(RetainedCapabilityInput::Effect(requirement_idx)),
        ProviderSource::ContractField { .. } | ProviderSource::RootProvider { .. } => None,
    }
}

fn add_place_sources<'db>(
    body: &NormalizedSemanticBody<'db>,
    place: &NSPlace<'db>,
    inputs: &mut FxHashSet<RetainedCapabilityInput>,
    dependencies: &mut Vec<SLocalId>,
) {
    match place.root {
        NSPlaceRoot::CarrierDerefLocal(local) => dependencies.push(local),
        NSPlaceRoot::Root(root) => match body.root(root) {
            Some(NBorrowRoot::Param { local, param_idx }) => {
                dependencies.push(*local);
                inputs.insert(RetainedCapabilityInput::Param(*param_idx));
            }
            Some(NBorrowRoot::LocalSlot { local }) => dependencies.push(*local),
            Some(NBorrowRoot::Provider { binding, .. }) => {
                if let Some(effect) = provider_effect_input(binding) {
                    inputs.insert(effect);
                }
            }
            None => {}
        },
    }
}

pub(super) fn store_carries_borrow_transport<'db>(
    db: &'db dyn MirDb,
    body: &NormalizedSemanticBody<'db>,
    dst: &NSPlace<'db>,
    src: NOperand,
) -> bool {
    store_rebinds_capability(db, body, dst, src)
        || body.local(src.local).is_some_and(|local| {
            local.ty.as_capability(db).is_none() && ty_contains_borrow(db, local.ty)
        })
}

fn place_is_external<'db>(
    body: &NormalizedSemanticBody<'db>,
    place: &NSPlace<'db>,
    sources: &[FxHashSet<RetainedCapabilityInput>],
    external_inputs: &FxHashSet<RetainedCapabilityInput>,
) -> bool {
    match place.root {
        NSPlaceRoot::CarrierDerefLocal(local) => {
            !sources[local.index()].is_disjoint(external_inputs)
        }
        NSPlaceRoot::Root(root) => match body.root(root) {
            Some(NBorrowRoot::Param { param_idx, .. }) => {
                external_inputs.contains(&RetainedCapabilityInput::Param(*param_idx))
            }
            Some(NBorrowRoot::LocalSlot { .. }) => false,
            Some(NBorrowRoot::Provider { .. }) => true,
            None => false,
        },
    }
}

fn call_actual_sources<'db>(
    body: &NormalizedSemanticBody<'db>,
    sources: &[FxHashSet<RetainedCapabilityInput>],
    args: &[NOperand],
    effect_args: &[NEffectArg<'db>],
) -> FxHashMap<RetainedCapabilityInput, FxHashSet<RetainedCapabilityInput>> {
    let mut actuals = FxHashMap::default();
    for (idx, arg) in args.iter().enumerate() {
        actuals.insert(
            RetainedCapabilityInput::Param(idx as u32),
            sources[arg.local.index()].clone(),
        );
    }
    for effect in effect_args {
        let effect_sources = match &effect.arg {
            NEffectArgValue::Value(value) => sources[value.local.index()].clone(),
            NEffectArgValue::Place(place) => {
                let mut inputs = FxHashSet::default();
                let mut dependencies = Vec::new();
                add_place_sources(body, place, &mut inputs, &mut dependencies);
                for dependency in dependencies {
                    inputs.extend(sources[dependency.index()].iter().copied());
                }
                inputs
            }
        };
        actuals
            .entry(RetainedCapabilityInput::Effect(effect.binding_idx))
            .or_insert_with(FxHashSet::default)
            .extend(effect_sources);
    }
    actuals
}
