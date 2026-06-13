use std::convert::Infallible;

use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::{SparseAnalysis, solve_sparse};
use hir::{
    analysis::{
        semantic::{
            SLocalId, SemanticLocalKind,
            borrowck::{NLocalOrigin, NSLocal, NormalizedBindingLowering, NormalizedSemanticBody},
        },
        ty::{
            trait_resolution::PredicateListId, ty_check::LocalBinding, ty_def::CapabilityKind,
            ty_is_copy,
        },
    },
    hir_def::FuncParamMode,
    semantic::ProviderBinding,
};

use crate::{
    db::MirDb,
    runtime::{
        AddressSpaceKind, ArrayLayout, EnumLayoutKey, EnumVariantLayout, Layout, LayoutId,
        LayoutKey, RefKind, RefView, RuntimeCarrier, RuntimeClass, RuntimeLocalLowering,
        RuntimeLocalRoot, RuntimeProviderBinding, RuntimeProviderBindingId, StructLayout,
    },
};

use super::{
    classify::{
        AssignmentId, BodyEnv, BodyStaticFacts, InferClassCache, RuntimeBodyCx,
        carrier_value_class, provider_erases_runtime_root,
        runtime_class_for_direct_value_provider_in_context,
        runtime_class_for_effect_binding_provider_in_context, runtime_class_for_provider_binding,
    },
    conversion::RuntimeConversionPlanner,
    returns::runtime_return_class,
    source::local_read_places_extractable_from_value,
    type_info::{
        effect_handle_class_for_ty_in_context, provider_class_for_target_in_context,
        runtime_repr_ty_in_context, stored_class_for_ty_in_context,
        top_level_class_for_ty_in_context,
    },
};

#[derive(Clone, Debug)]
pub(super) struct InferenceResult<'db> {
    pub(super) carriers: Vec<RuntimeCarrier<'db>>,
    pub(super) roots: Vec<RuntimeLocalRoot<'db>>,
    pub(super) semantic_locals: Vec<RuntimeLocalLowering<'db>>,
    pub(super) provider_bindings: Vec<RuntimeProviderBinding<'db>>,
}

pub(super) struct LocalStateInferer<'a, 'db> {
    env: BodyEnv<'a, 'db>,
    carriers: Vec<RuntimeCarrier<'db>>,
    class_cache: InferClassCache<'db>,
    pending_dependents: Vec<AssignmentId>,
}

impl<'a, 'db> LocalStateInferer<'a, 'db> {
    pub(super) fn new(
        env: BodyEnv<'a, 'db>,
        params: &[RuntimeClass<'db>],
        param_locals: &[SLocalId],
    ) -> Self {
        let mut carriers = vec![RuntimeCarrier::Erased; env.body().locals.len()];
        for (class, local) in params.iter().zip(param_locals.iter().copied()) {
            carriers[local.index()] = RuntimeCarrier::Value(class.clone());
        }
        Self {
            env,
            carriers,
            class_cache: InferClassCache::new(env.body().locals.len()),
            pending_dependents: Vec::new(),
        }
    }

    pub(super) fn run(mut self) -> InferenceResult<'db> {
        seed_root_provider_carriers(self.env, &mut self.carriers);
        solve_sparse(&mut self, &mut ());
        let roots = self.infer_roots();
        let (semantic_locals, provider_bindings) =
            lower_semantic_locals(self.env.with_carriers(&self.carriers));
        InferenceResult {
            carriers: self.carriers,
            roots,
            semantic_locals,
            provider_bindings,
        }
    }

    fn set_carrier(&mut self, local: SLocalId, desired: RuntimeCarrier<'db>) -> bool {
        let current = self
            .carriers
            .get(local.index())
            .cloned()
            .unwrap_or(RuntimeCarrier::Erased);
        let desired = merge_runtime_carrier(
            self.env.db(),
            &self.env.body().locals[local.index()],
            current,
            desired,
        );
        if self.carriers[local.index()] == desired {
            return false;
        }
        self.carriers[local.index()] = desired;
        self.class_cache.note_carrier_changed(local);
        true
    }

    fn collect_local_change_dependents(&mut self, changed_local: SLocalId) {
        let mut pending = vec![changed_local];
        let mut seen = vec![false; self.env.body().locals.len()];
        let mut queued = SecondaryMap::with_default(false);
        queued.resize(self.env.assignment_count());
        self.pending_dependents.clear();
        while let Some(local) = pending.pop() {
            if std::mem::replace(&mut seen[local.index()], true) {
                continue;
            }
            self.class_cache.invalidate_local_dynamic_facts(local);
            for &assign_id in self.env.assignments_using_local(local) {
                if !queued[assign_id] {
                    queued[assign_id] = true;
                    self.pending_dependents.push(assign_id);
                }
            }
            for dependent in self.env.dynamic_dependents(local).iter().copied() {
                pending.push(dependent);
            }
        }
    }

    fn infer_roots(&mut self) -> Vec<RuntimeLocalRoot<'db>> {
        let carriers = self.carriers.clone();
        let cx = self.env.with_carriers(&carriers);
        let mut roots = Vec::with_capacity(cx.env.body().locals.len());
        for (idx, local) in cx.env.body().locals.iter().enumerate() {
            let local_id = SLocalId::from_u32(idx as u32);
            let mut carrier = carriers[idx].clone();
            let root = if !local.facts.root_demand.needs_runtime_root() {
                RuntimeLocalRoot::None
            } else if let Some(unrooted_carrier) = local_lowers_as_unrooted_read_value(
                cx.env.db(),
                cx.env.body(),
                local_id,
                local,
                &carrier,
                cx.env.scope(),
                cx.env.assumptions(),
            ) {
                carrier = unrooted_carrier;
                RuntimeLocalRoot::None
            } else {
                infer_runtime_local_root(cx, local_id, &mut carrier)
            };
            self.carriers[idx] = carrier;
            roots.push(root);
        }
        roots
    }
}

impl<'a, 'db> SparseAnalysis for LocalStateInferer<'a, 'db> {
    type Node = AssignmentId;
    type State = ();
    type Error = Infallible;

    fn node_count(&self) -> usize {
        self.env.assignment_count()
    }

    fn seed_nodes(&self) -> Vec<Self::Node> {
        self.env.assignment_ids()
    }

    fn step(&mut self, node: Self::Node, _: &mut Self::State) -> Result<bool, Self::Error> {
        self.pending_dependents.clear();
        let assign_id = node;
        let assign = self
            .env
            .assignment(assign_id)
            .unwrap_or_else(|| panic!("missing assignment facts for statement {assign_id:?}"));
        let stmt = &self.env.body().blocks[assign.block_idx].stmts[assign.stmt_idx];
        let expr = match &stmt.kind {
            hir::analysis::semantic::NSStmtKind::Assign { expr, .. } => expr,
            hir::analysis::semantic::NSStmtKind::Store { .. } => {
                panic!(
                    "assignment facts point to non-assignment statement: block={} stmt={}",
                    assign.block_idx, assign.stmt_idx
                )
            }
        };
        let local = &self.env.body().locals[assign.dst.index()];
        let mut lookup_return_class = |key| runtime_return_class(self.env.db(), key);
        let class = self.env.expr_direct_class(
            &self.carriers,
            assign.block_idx,
            assign.stmt_idx,
            expr,
            Some(&mut self.class_cache),
            &mut lookup_return_class,
        );
        let Some(class) = class else {
            return Ok(false);
        };
        let desired = desired_runtime_value_carrier(
            self.env.db(),
            local,
            class,
            self.env.scope(),
            self.env.assumptions(),
        );
        if !self.set_carrier(assign.dst, desired) {
            return Ok(false);
        }
        self.collect_local_change_dependents(assign.dst);
        Ok(true)
    }

    fn dependents(&self, _node: Self::Node, out: &mut Vec<Self::Node>) {
        out.extend(self.pending_dependents.iter().copied());
    }
}

pub(crate) fn seed_root_provider_carriers<'a, 'db>(
    env: BodyEnv<'a, 'db>,
    carriers: &mut [RuntimeCarrier<'db>],
) {
    for (idx, local) in env.body().locals.iter().enumerate() {
        if !matches!(carriers[idx], RuntimeCarrier::Erased) {
            continue;
        }
        if local.facts.origin.root_provider().is_some_and(|provider| {
            provider_erases_runtime_root(env.db(), provider, env.scope(), env.assumptions())
        }) {
            continue;
        }
        let class = match (&local.facts.interface, &local.facts.origin) {
            (SemanticLocalKind::DirectValue, NLocalOrigin::RootProvider(provider)) => env
                .actual_runtime_visible_root_provider_class(carriers, provider)
                .map(|(_, class)| class)
                .or_else(|| {
                    runtime_class_for_direct_value_provider_in_context(
                        env.db(),
                        provider,
                        env.scope(),
                        env.assumptions(),
                    )
                }),
            (SemanticLocalKind::DirectCarrier, NLocalOrigin::RootProvider(provider)) => env
                .actual_runtime_visible_root_provider_class(carriers, provider)
                .map(|(_, class)| class)
                .or_else(|| {
                    runtime_class_for_provider_binding(
                        env.db(),
                        provider,
                        env.scope(),
                        env.assumptions(),
                    )
                }),
            (SemanticLocalKind::PlaceCarrier, NLocalOrigin::RootProvider(provider)) => env
                .actual_runtime_visible_root_provider_class(carriers, provider)
                .map(|(_, class)| class)
                .or_else(|| {
                    runtime_class_for_effect_binding_provider_in_context(
                        env.db(),
                        provider,
                        env.scope(),
                        env.assumptions(),
                    )
                }),
            _ => None,
        };
        if let Some(class) = class {
            carriers[idx] = RuntimeCarrier::Value(class);
        }
    }
}

pub(crate) fn desired_runtime_value_carrier<'db>(
    db: &'db dyn MirDb,
    local: &NSLocal<'db>,
    class: RuntimeClass<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> RuntimeCarrier<'db> {
    if runtime_class_has_zero_sized_payload(db, &class) {
        return RuntimeCarrier::Erased;
    }
    if matches!(
        (&local.facts.interface, &local.facts.origin),
        (
            SemanticLocalKind::PlaceBoundValue,
            NLocalOrigin::AliasedPlace
        )
    ) {
        return RuntimeCarrier::Erased;
    }
    if let Some(transport_class) =
        effect_handle_class_for_ty_in_context(db, local.ty, scope, assumptions)
    {
        return RuntimeCarrier::Value(transport_class);
    }
    if !class.is_transport()
        && matches!(local.facts.interface, SemanticLocalKind::DirectCarrier)
        && let Some(transport_class) = fallback_root_transport_class(db, local, scope, assumptions)
    {
        return RuntimeCarrier::Value(transport_class);
    }
    match class {
        RuntimeClass::AggregateValue { layout }
            if matches!(local.facts.interface, SemanticLocalKind::DirectValue)
                && local.facts.root_demand.needs_projectable_owned_storage() =>
        {
            RuntimeCarrier::Value(RuntimeClass::object_ref(layout))
        }
        class => RuntimeCarrier::Value(class),
    }
}

fn runtime_class_has_zero_sized_payload<'db>(
    db: &'db dyn MirDb,
    class: &RuntimeClass<'db>,
) -> bool {
    match class {
        RuntimeClass::Ref {
            pointee,
            kind:
                RefKind::Provider {
                    space: AddressSpaceKind::Memory,
                    ..
                },
            ..
        } => pointee.span_words(db) == 0,
        RuntimeClass::Scalar(_)
        | RuntimeClass::AggregateValue { .. }
        | RuntimeClass::Ref { .. }
        | RuntimeClass::RawAddr { .. } => class.span_words(db) == 0,
    }
}

fn lower_semantic_locals<'db>(
    cx: RuntimeBodyCx<'_, '_, 'db>,
) -> (
    Vec<RuntimeLocalLowering<'db>>,
    Vec<RuntimeProviderBinding<'db>>,
) {
    let db = cx.env.db();
    let body = cx.env.body();
    let carriers = cx.carriers;
    let scope = cx.env.scope();
    let assumptions = cx.env.assumptions();
    let mut provider_bindings = Vec::new();
    for (idx, local) in body.locals.iter().enumerate() {
        let local_id = SLocalId::from_u32(idx as u32);
        if local
            .facts
            .origin
            .root_provider()
            .is_some_and(|provider| provider_erases_runtime_root(db, provider, scope, assumptions))
        {
            continue;
        }
        let binding = match (&local.facts.interface, &local.facts.origin) {
            (SemanticLocalKind::DirectValue, NLocalOrigin::RootProvider(provider)) => {
                let (provider_local, provider_class) = cx
                    .env
                    .actual_runtime_visible_root_provider_class(carriers, provider)
                .or_else(|| {
                        runtime_class_for_direct_value_provider_in_context(
                        db,
                        provider,
                        scope,
                        assumptions,
                    )
                    .map(|class| (local_id, class))
                })
                .unwrap_or_else(|| {
                    panic!(
                        "missing runtime provider class for root-provider direct value local {:?}: {provider:?}",
                        local.source
                    )
                });
                Some((
                    provider.clone(),
                    provider_class,
                    normalized_local_place_class(db, body, local_id, carriers)
                        .unwrap_or_else(|| {
                            panic!(
                                "missing normalized place class for root-provider direct value local {idx}"
                            )
                        }),
                    provider_local,
                ))
            }
            (SemanticLocalKind::PlaceBoundValue, NLocalOrigin::RootProvider(provider)) => {
                let (provider_local, provider_class) = cx
                    .env
                    .actual_runtime_visible_root_provider_class(carriers, provider)
                .or_else(|| {
                    runtime_class_for_effect_binding_provider_in_context(
                        db,
                        provider,
                        scope,
                        assumptions,
                    )
                    .or_else(|| {
                        runtime_class_for_direct_value_provider_in_context(
                            db,
                            provider,
                            scope,
                            assumptions,
                        )
                    })
                    .map(|class| (local_id, class))
                })
                .unwrap_or_else(|| {
                    panic!(
                        "missing runtime provider class for root-provider place-bound local {idx}: {provider:?}"
                    )
                });
                Some((
                    provider.clone(),
                    provider_class,
                    normalized_local_place_class(db, body, local_id, carriers)
                        .unwrap_or_else(|| {
                            panic!(
                                "missing normalized place class for root-provider place-bound local {idx}"
                            )
                        }),
                    provider_local,
                ))
            }
            (SemanticLocalKind::DirectCarrier, NLocalOrigin::RootProvider(provider)) => {
                let NormalizedBindingLowering::CarrierLocal { .. } = &local.lowering else {
                    panic!("direct-carrier local missing carrier lowering: {idx}");
                };
                let (provider_local, provider_class) = cx
                    .env
                    .actual_runtime_visible_root_provider_class(carriers, provider)
                    .or_else(|| {
                        runtime_class_for_provider_binding(db, provider, scope, assumptions)
                            .map(|class| (local_id, class))
                    })
                    .unwrap_or_else(|| {
                        panic!(
                            "missing direct-carrier runtime class for semantic local {idx}: {}",
                            local.ty.pretty_print(db),
                        )
                    });
                Some((
                    provider.clone(),
                    provider_class,
                    carrier_local_place_class(db, local, local_id, carriers, scope, assumptions),
                    provider_local,
                ))
            }
            (SemanticLocalKind::PlaceCarrier, NLocalOrigin::RootProvider(provider)) => {
                let NormalizedBindingLowering::CarrierLocal { .. } = &local.lowering else {
                    panic!("place-carrier local missing carrier lowering: {idx}");
                };
                let (provider_local, provider_class) = cx
                    .env
                    .actual_runtime_visible_root_provider_class(carriers, provider)
                    .or_else(|| {
                        runtime_class_for_effect_binding_provider_in_context(
                            db,
                            provider,
                            scope,
                            assumptions,
                        )
                        .map(|class| (local_id, class))
                    })
                    .unwrap_or_else(|| {
                        panic!(
                            "missing place-carrier runtime class for semantic local {idx}: {}",
                            local.ty.pretty_print(db),
                        )
                    });
                Some((
                    provider.clone(),
                    provider_class,
                    carrier_local_place_class(db, local, local_id, carriers, scope, assumptions),
                    provider_local,
                ))
            }
            _ => None,
        };
        let Some((provider, provider_class, place_class, provider_local)) = binding else {
            continue;
        };
        if runtime_provider_binding_id(&provider_bindings, &provider).is_some() {
            continue;
        }
        push_runtime_provider_binding(
            &mut provider_bindings,
            provider,
            provider_local,
            provider_class,
            place_class,
        );
    }
    let lowerings = cx
        .env
        .body()
        .locals
        .iter()
        .enumerate()
        .map(|(idx, local)| match (&local.facts.interface, &local.facts.origin) {
            (SemanticLocalKind::Erased, _) => RuntimeLocalLowering::Erased,
            (_, NLocalOrigin::RootProvider(provider))
                if provider_erases_runtime_root(db, provider, scope, assumptions) =>
            {
                RuntimeLocalLowering::Erased
            }
            (SemanticLocalKind::DirectValue, NLocalOrigin::RootProvider(provider)) => {
                let provider = runtime_provider_binding_id(&provider_bindings, provider)
                    .unwrap_or_else(|| {
                        panic!(
                            "missing runtime provider binding for root-provider direct value local {idx}: {provider:?}"
                        )
                    });
                RuntimeLocalLowering::PlaceBoundValue {
                    provider: Some(provider),
                    place_class: provider_bindings[provider.index()].place_class.clone(),
                }
            }
            (SemanticLocalKind::DirectValue, _) => RuntimeLocalLowering::DirectValue,
            (SemanticLocalKind::PlaceCarrier, _)
                if place_carrier_lowers_as_direct_value(
                    db,
                    local,
                    &carriers[idx],
                    scope,
                    assumptions,
                ) =>
            {
                RuntimeLocalLowering::DirectValue
            }
            (SemanticLocalKind::PlaceCarrier, _) => RuntimeLocalLowering::PlaceCarrier {
                place_class: carrier_local_place_class(
                    db,
                    local,
                    SLocalId::from_u32(idx as u32),
                    carriers,
                    scope,
                    assumptions,
                ),
            },
            (SemanticLocalKind::PlaceBoundValue, origin) => {
                let place_class =
                    normalized_local_place_class(db, body, SLocalId::from_u32(idx as u32), carriers)
                        .unwrap_or_else(|| {
                    panic!("missing normalized place class for place-bound semantic local {idx}")
                });
                let provider = origin.root_provider().map(|provider| {
                    runtime_provider_binding_id(&provider_bindings, provider).unwrap_or_else(|| {
                        panic!(
                            "missing runtime provider binding for place-bound semantic local {idx}: {origin:?}"
                        )
                    })
                });
                RuntimeLocalLowering::PlaceBoundValue {
                    provider,
                    place_class,
                }
            }
            (SemanticLocalKind::DirectCarrier, origin) => {
                let place_class = carrier_local_place_class(
                    db,
                    local,
                    SLocalId::from_u32(idx as u32),
                    carriers,
                    scope,
                    assumptions,
                );
                let provider = origin.root_provider().map(|provider| {
                    let provider_class = runtime_class_for_provider_binding(
                        db,
                        provider,
                        scope,
                        assumptions,
                    )
                    .unwrap_or_else(|| {
                                panic!(
                                    "missing direct-carrier runtime class for semantic local {idx}: {}",
                                    local.ty.pretty_print(db),
                                )
                            });
                    push_runtime_provider_binding(
                        &mut provider_bindings,
                        provider.clone(),
                        SLocalId::from_u32(idx as u32),
                        provider_class,
                        place_class.clone(),
                    )
                });
                RuntimeLocalLowering::DirectCarrier {
                    provider,
                    place_class,
                }
            }
        })
        .collect();
    (lowerings, provider_bindings)
}

fn place_carrier_lowers_as_direct_value<'db>(
    db: &'db dyn MirDb,
    local: &NSLocal<'db>,
    carrier: &RuntimeCarrier<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    let Some(class) = carrier.value_class().cloned() else {
        return false;
    };
    if class.is_transport() {
        return false;
    }
    if matches!(class, RuntimeClass::Scalar(_)) {
        return true;
    }
    let NormalizedBindingLowering::CarrierLocal { target_ty, .. } = &local.lowering else {
        panic!("place-carrier local missing carrier lowering");
    };
    let runtime_target_ty = runtime_repr_ty_in_context(db, *target_ty, scope, assumptions);
    matches!(
        local.ty.as_capability(db),
        Some((CapabilityKind::View, inner))
            if runtime_repr_ty_in_context(db, inner, scope, assumptions) == runtime_target_ty
    ) || matches!(
        local.source,
        Some(LocalBinding::Param {
            mode: FuncParamMode::View,
            ..
        }) if runtime_repr_ty_in_context(db, local.ty, scope, assumptions) == runtime_target_ty
    ) || scope.is_some_and(|scope| ty_is_copy(db, scope, *target_ty, assumptions))
}

fn local_lowers_as_direct_read_value<'db>(
    db: &'db dyn MirDb,
    local: &NSLocal<'db>,
    carrier: &RuntimeCarrier<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    let Some(class) = carrier.value_class().cloned() else {
        return false;
    };
    if !matches!(
        class,
        RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. }
    ) {
        return false;
    }
    match local.facts.interface {
        SemanticLocalKind::DirectValue => local_direct_value_lowers_as_unrooted(local, db, &class),
        SemanticLocalKind::PlaceCarrier => {
            local_is_read_only_view_param(local)
                && place_carrier_lowers_as_direct_value(db, local, carrier, scope, assumptions)
        }
        SemanticLocalKind::Erased
        | SemanticLocalKind::DirectCarrier
        | SemanticLocalKind::PlaceBoundValue => false,
    }
}

fn local_direct_value_lowers_as_unrooted<'db>(
    local: &NSLocal<'db>,
    db: &'db dyn MirDb,
    class: &RuntimeClass<'db>,
) -> bool {
    if local.facts.origin.root_provider().is_some() {
        return false;
    }
    if !class.contains_transport(db) {
        return true;
    }
    matches!(
        local.source,
        Some(LocalBinding::Param {
            mode: FuncParamMode::View,
            ..
        })
    )
}

fn local_is_read_only_view_param<'db>(local: &NSLocal<'db>) -> bool {
    matches!(
        local.source,
        Some(LocalBinding::Param {
            mode: FuncParamMode::View,
            ..
        })
    )
}

fn local_lowers_as_unrooted_read_value<'db>(
    db: &'db dyn MirDb,
    body: &NormalizedSemanticBody<'db>,
    local_id: SLocalId,
    local: &NSLocal<'db>,
    carrier: &RuntimeCarrier<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeCarrier<'db>> {
    let candidate = unrooted_read_value_candidate_carrier(db, local, carrier)?;
    if !local_lowers_as_direct_read_value(db, local, &candidate, scope, assumptions) {
        return None;
    }
    let demand = local.facts.root_demand;
    if !demand.permits_unrooted_value_projection_reads() {
        return None;
    }
    local_read_places_extractable_from_value(body, local_id).then_some(candidate)
}

fn unrooted_read_value_candidate_carrier<'db>(
    db: &'db dyn MirDb,
    local: &NSLocal<'db>,
    carrier: &RuntimeCarrier<'db>,
) -> Option<RuntimeCarrier<'db>> {
    let class = carrier.value_class().cloned()?;
    if matches!(
        class,
        RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. }
    ) {
        return Some(RuntimeCarrier::Value(class));
    }
    if !matches!(local.facts.interface, SemanticLocalKind::DirectValue) {
        return None;
    }
    let RuntimeClass::Ref {
        kind: RefKind::Object,
        ..
    } = class
    else {
        return None;
    };
    let aggregate = class.aggregate_value_class()?;
    (!aggregate.contains_transport(db)).then_some(RuntimeCarrier::Value(aggregate))
}

fn runtime_provider_binding_id<'db>(
    provider_bindings: &[RuntimeProviderBinding<'db>],
    provider: &ProviderBinding<'db>,
) -> Option<RuntimeProviderBindingId> {
    provider_bindings
        .iter()
        .enumerate()
        .find_map(|(idx, binding)| {
            (binding.provider == *provider).then(|| RuntimeProviderBindingId::from_u32(idx as u32))
        })
}

fn carrier_local_place_class<'db>(
    db: &'db dyn MirDb,
    local: &NSLocal<'db>,
    local_id: SLocalId,
    carriers: &[RuntimeCarrier<'db>],
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> RuntimeClass<'db> {
    let NormalizedBindingLowering::CarrierLocal { target_ty, .. } = &local.lowering else {
        panic!("carrier local missing carrier lowering: {local_id:?}");
    };
    carrier_value_class(local_id, carriers)
        .and_then(|class| class.aggregate_value_class())
        .unwrap_or_else(|| stored_class_for_ty_in_context(db, *target_ty, scope, assumptions))
}

fn normalized_local_place_class<'db>(
    db: &'db dyn MirDb,
    body: &NormalizedSemanticBody<'db>,
    local: SLocalId,
    carriers: &[RuntimeCarrier<'db>],
) -> Option<RuntimeClass<'db>> {
    normalized_local_place_class_in_context(
        db,
        body,
        local,
        carriers,
        Some(body.owner.key(db).owner(db).scope()),
        body.owner.assumptions(db),
    )
}

pub(super) fn normalized_local_place_class_in_context<'db>(
    db: &'db dyn MirDb,
    body: &NormalizedSemanticBody<'db>,
    local: SLocalId,
    carriers: &[RuntimeCarrier<'db>],
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeClass<'db>> {
    let typed_body = body.owner.key(db).typed_body(db);
    let type_env = super::type_info::RuntimeTypeEnv::new(scope, assumptions);
    let facts = BodyStaticFacts::new_in_context(db, body, typed_body, type_env);
    BodyEnv::from_parts(db, body, type_env, &facts)
        .normalized_place_class(carriers, body.locals.get(local.index())?.backing_place()?)
}

fn infer_runtime_local_root<'db>(
    cx: RuntimeBodyCx<'_, '_, 'db>,
    local: SLocalId,
    carrier: &mut RuntimeCarrier<'db>,
) -> RuntimeLocalRoot<'db> {
    let local_data = cx
        .env
        .body()
        .locals
        .get(local.index())
        .expect("normalized local exists");
    let place_class = local_place_root_class(cx, local, local_data, carrier);
    let transport_class = match carrier {
        RuntimeCarrier::Value(class) => Some(class.clone()),
        RuntimeCarrier::Erased => cx.env.root_transport_fallback_class(local),
    };
    let Some(place_class) = place_class else {
        return RuntimeLocalRoot::None;
    };
    if runtime_class_has_zero_sized_payload(cx.env.db(), &place_class) {
        return RuntimeLocalRoot::None;
    }
    let Some(transport_class) = transport_class else {
        return RuntimeLocalRoot::Slot(place_class);
    };
    if matches!(
        (&*carrier, &transport_class),
        (
            RuntimeCarrier::Erased,
            RuntimeClass::RawAddr { .. } | RuntimeClass::Ref { .. }
        )
    ) {
        *carrier = RuntimeCarrier::Value(transport_class.clone());
    }
    match transport_class {
        RuntimeClass::RawAddr { space, .. } => RuntimeLocalRoot::Ptr {
            space,
            class: place_class,
        },
        RuntimeClass::Ref {
            kind: RefKind::Provider { space, .. },
            ..
        } if space != AddressSpaceKind::Memory => RuntimeLocalRoot::Ptr {
            space,
            class: place_class,
        },
        RuntimeClass::Ref { .. } => RuntimeLocalRoot::Ref(transport_class),
        RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. } => {
            RuntimeLocalRoot::Slot(place_class)
        }
    }
}

pub(super) fn local_place_root_class<'db>(
    cx: RuntimeBodyCx<'_, '_, 'db>,
    local: SLocalId,
    local_data: &NSLocal<'db>,
    carrier: &RuntimeCarrier<'db>,
) -> Option<RuntimeClass<'db>> {
    match local_data.facts.interface {
        SemanticLocalKind::Erased => None,
        SemanticLocalKind::DirectValue => {
            if let Some(carrier_class) = carrier.value_class().cloned()
                && let Some(place_class) =
                    materialized_place_class_from_runtime_source(&carrier_class)
            {
                return Some(place_class);
            }
            cx.env.root_place_fallback_class(local)
        }
        SemanticLocalKind::PlaceCarrier => {
            if let Some(carrier_class) = carrier.value_class().cloned()
                && let Some(place_class) =
                    materialized_place_class_from_runtime_source(&carrier_class)
            {
                return Some(place_class);
            }
            cx.env.root_place_fallback_class(local)
        }
        SemanticLocalKind::PlaceBoundValue => cx
            .env
            .normalized_place_class(
                cx.carriers,
                cx.env.body().locals.get(local.index())?.backing_place()?,
            )
            .or_else(|| cx.env.root_place_fallback_class(local)),
        SemanticLocalKind::DirectCarrier => {
            if let Some(carrier_class) = carrier.value_class().cloned()
                && let Some(place_class) =
                    materialized_place_class_from_runtime_source(&carrier_class)
            {
                return Some(place_class);
            }
            cx.env.root_place_fallback_class(local)
        }
    }
}

pub(super) fn fallback_root_transport_class<'db>(
    db: &'db dyn MirDb,
    local: &NSLocal<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeClass<'db>> {
    match local.facts.interface {
        SemanticLocalKind::Erased
        | SemanticLocalKind::DirectValue
        | SemanticLocalKind::PlaceBoundValue => None,
        SemanticLocalKind::PlaceCarrier => {
            let NormalizedBindingLowering::CarrierLocal { target_ty, .. } = &local.lowering else {
                panic!("place-carrier local missing carrier lowering");
            };
            local
                .facts
                .origin
                .root_provider()
                .and_then(|provider| {
                    runtime_class_for_effect_binding_provider_in_context(
                        db,
                        provider,
                        scope,
                        assumptions,
                    )
                })
                .or_else(|| {
                    top_level_class_for_ty_in_context(
                        db,
                        local.ty,
                        AddressSpaceKind::Memory,
                        scope,
                        assumptions,
                    )
                })
                .or_else(|| {
                    Some(provider_class_for_target_in_context(
                        db,
                        Some(*target_ty),
                        AddressSpaceKind::Memory,
                        scope,
                        assumptions,
                    ))
                })
        }
        SemanticLocalKind::DirectCarrier => {
            let provider = local.facts.origin.root_provider();
            let NormalizedBindingLowering::CarrierLocal { target_ty, .. } = &local.lowering else {
                panic!("direct-carrier local missing carrier lowering");
            };
            provider
                .and_then(|provider| {
                    runtime_class_for_provider_binding(db, provider, scope, assumptions)
                })
                .or_else(|| {
                    top_level_class_for_ty_in_context(
                        db,
                        local.ty,
                        AddressSpaceKind::Memory,
                        scope,
                        assumptions,
                    )
                })
                .or_else(|| {
                    Some(provider_class_for_target_in_context(
                        db,
                        Some(*target_ty),
                        AddressSpaceKind::Memory,
                        scope,
                        assumptions,
                    ))
                })
        }
    }
}

fn materialized_place_class_from_runtime_source<'db>(
    class: &RuntimeClass<'db>,
) -> Option<RuntimeClass<'db>> {
    match class {
        RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. } => Some(class.clone()),
        RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => class.deref_target(),
    }
}

pub(crate) fn merge_runtime_carrier<'db>(
    db: &'db dyn MirDb,
    local: &NSLocal<'db>,
    current: RuntimeCarrier<'db>,
    desired: RuntimeCarrier<'db>,
) -> RuntimeCarrier<'db> {
    match (current, desired) {
        (RuntimeCarrier::Erased, desired) | (desired, RuntimeCarrier::Erased) => desired,
        (RuntimeCarrier::Value(current), RuntimeCarrier::Value(desired)) => {
            let demand = RuntimeJoinDemand::for_local(local);
            RuntimeCarrier::Value(join_runtime_class(db, demand, &current, &desired).unwrap_or_else(
                || {
                    panic!(
                        "runtime carrier classes have no common realizable join: local={local:?}; current={current:?}; desired={desired:?}"
                    )
                },
            ))
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct RuntimeJoinDemand {
    prefer_owned_object: bool,
    prefer_transport: bool,
}

impl RuntimeJoinDemand {
    fn for_local(local: &NSLocal<'_>) -> Self {
        Self {
            prefer_owned_object: local.facts.root_demand.needs_projectable_owned_storage(),
            prefer_transport: matches!(
                local.facts.interface,
                SemanticLocalKind::PlaceCarrier | SemanticLocalKind::DirectCarrier
            ) || local.facts.origin.root_provider().is_some(),
        }
    }
}

fn join_runtime_class<'db>(
    db: &'db dyn MirDb,
    demand: RuntimeJoinDemand,
    current: &RuntimeClass<'db>,
    desired: &RuntimeClass<'db>,
) -> Option<RuntimeClass<'db>> {
    if current == desired {
        return Some(current.clone());
    }
    if let Some(merged) = merge_runtime_class(db, current, desired) {
        return Some(merged);
    }

    let mut candidates = Vec::new();
    push_join_candidate(&mut candidates, current.clone());
    push_join_candidate(&mut candidates, desired.clone());
    push_materialized_join_candidates(&mut candidates, current);
    push_materialized_join_candidates(&mut candidates, desired);
    if let (Some(current_layout), Some(desired_layout)) =
        (current.aggregate_layout(), desired.aggregate_layout())
        && let Some(layout) = merge_layouts(db, current_layout, desired_layout)
    {
        push_join_candidate(&mut candidates, RuntimeClass::AggregateValue { layout });
        push_join_candidate(&mut candidates, RuntimeClass::object_ref(layout));
    }

    candidates
        .into_iter()
        .filter(|candidate| can_join_as(db, current, desired, candidate))
        .min_by_key(|candidate| join_candidate_rank(demand, candidate))
}

fn push_materialized_join_candidates<'db>(
    candidates: &mut Vec<RuntimeClass<'db>>,
    class: &RuntimeClass<'db>,
) {
    if let Some(pointee) = class.pointee() {
        push_join_candidate(candidates, pointee.clone());
    }
    if let Some(layout) = class.aggregate_layout() {
        push_join_candidate(candidates, RuntimeClass::AggregateValue { layout });
        push_join_candidate(candidates, RuntimeClass::object_ref(layout));
    }
}

fn push_join_candidate<'db>(candidates: &mut Vec<RuntimeClass<'db>>, candidate: RuntimeClass<'db>) {
    if !candidates.contains(&candidate) {
        candidates.push(candidate);
    }
}

fn can_realize_as<'db>(
    db: &'db dyn MirDb,
    source: &RuntimeClass<'db>,
    target: &RuntimeClass<'db>,
) -> bool {
    source == target || RuntimeConversionPlanner::plan(db, source.clone(), target.clone()).is_ok()
}

fn can_join_as<'db>(
    db: &'db dyn MirDb,
    current: &RuntimeClass<'db>,
    desired: &RuntimeClass<'db>,
    candidate: &RuntimeClass<'db>,
) -> bool {
    merge_runtime_class(db, current, desired).as_ref() == Some(candidate)
        || can_realize_as(db, current, candidate) && can_realize_as(db, desired, candidate)
}

fn join_candidate_rank(demand: RuntimeJoinDemand, candidate: &RuntimeClass<'_>) -> u8 {
    match candidate {
        RuntimeClass::Scalar(_) => 0,
        RuntimeClass::AggregateValue { .. }
            if !demand.prefer_owned_object && !demand.prefer_transport =>
        {
            1
        }
        RuntimeClass::Ref {
            kind: RefKind::Object,
            ..
        } if demand.prefer_owned_object => 1,
        RuntimeClass::Ref {
            kind: RefKind::Provider { .. },
            ..
        } if demand.prefer_transport => 1,
        RuntimeClass::RawAddr { .. } if demand.prefer_transport => 2,
        RuntimeClass::Ref {
            kind: RefKind::Object,
            ..
        } => 2,
        RuntimeClass::AggregateValue { .. } => 3,
        RuntimeClass::Ref {
            kind: RefKind::Provider { .. },
            ..
        } => 4,
        RuntimeClass::RawAddr { .. } => 5,
        RuntimeClass::Ref {
            kind: RefKind::Const,
            ..
        } => 6,
    }
}

pub(super) fn merge_runtime_class<'db>(
    db: &'db dyn MirDb,
    current: &RuntimeClass<'db>,
    desired: &RuntimeClass<'db>,
) -> Option<RuntimeClass<'db>> {
    if current == desired {
        return Some(current.clone());
    }
    match (current, desired) {
        (
            RuntimeClass::AggregateValue {
                layout: current_layout,
            },
            RuntimeClass::AggregateValue {
                layout: desired_layout,
            },
        ) => merge_layouts(db, *current_layout, *desired_layout)
            .map(|layout| RuntimeClass::AggregateValue { layout }),
        (
            RuntimeClass::Ref {
                pointee: current_pointee,
                kind: current_kind,
                view: current_view,
            },
            RuntimeClass::Ref {
                pointee: desired_pointee,
                kind: desired_kind,
                view: desired_view,
            },
        ) if current_view == desired_view => Some(RuntimeClass::Ref {
            pointee: Box::new(merge_runtime_class(db, current_pointee, desired_pointee)?),
            kind: merge_ref_kind(current_kind, desired_kind)?,
            view: current_view.clone(),
        }),
        (
            RuntimeClass::RawAddr {
                space: current_space,
                target: current_target,
            },
            RuntimeClass::RawAddr {
                space: desired_space,
                target: desired_target,
            },
        ) if current_target == desired_target => Some(RuntimeClass::RawAddr {
            space: preferred_address_space(*current_space, *desired_space),
            target: *current_target,
        }),
        (
            RuntimeClass::Ref {
                pointee,
                kind,
                view: RefView::Whole,
            },
            RuntimeClass::RawAddr { space, target },
        )
        | (
            RuntimeClass::RawAddr { space, target },
            RuntimeClass::Ref {
                pointee,
                kind,
                view: RefView::Whole,
            },
        ) if pointee.aggregate_layout() == *target => {
            let ref_space = ref_kind_address_space(kind);
            if ref_space == *space {
                Some(RuntimeClass::Ref {
                    pointee: pointee.clone(),
                    kind: kind.clone(),
                    view: RefView::Whole,
                })
            } else {
                Some(RuntimeClass::RawAddr {
                    space: preferred_address_space(ref_space, *space),
                    target: *target,
                })
            }
        }
        _ => None,
    }
}

fn merge_layouts<'db>(
    db: &'db dyn MirDb,
    current: LayoutId<'db>,
    desired: LayoutId<'db>,
) -> Option<LayoutId<'db>> {
    if current == desired {
        return Some(current);
    }
    match (current.data(db), desired.data(db)) {
        (Layout::Array(current), Layout::Array(desired))
            if current.source_ty == desired.source_ty && current.len == desired.len =>
        {
            Some(LayoutId::new(
                db,
                LayoutKey::Array(ArrayLayout {
                    source_ty: current.source_ty,
                    elem: merge_runtime_class(db, &current.elem, &desired.elem)?,
                    len: current.len,
                }),
            ))
        }
        (Layout::Struct(current), Layout::Struct(desired))
            if current.source_ty == desired.source_ty
                && current.fields.len() == desired.fields.len() =>
        {
            Some(LayoutId::new(
                db,
                LayoutKey::Struct(StructLayout {
                    source_ty: current.source_ty,
                    fields: current
                        .fields
                        .iter()
                        .zip(desired.fields.iter())
                        .map(|(current, desired)| merge_runtime_class(db, current, desired))
                        .collect::<Option<Vec<_>>>()?
                        .into(),
                }),
            ))
        }
        (Layout::Enum(current), Layout::Enum(desired))
            if current.source_ty == desired.source_ty
                && current.variants.len() == desired.variants.len() =>
        {
            Some(LayoutId::new(
                db,
                LayoutKey::Enum(EnumLayoutKey {
                    source_ty: current.source_ty,
                    variants: current
                        .variants
                        .iter()
                        .zip(desired.variants.iter())
                        .map(|(current, desired)| {
                            (current.fields.len() == desired.fields.len()).then_some(
                                EnumVariantLayout {
                                    name: current.name.clone(),
                                    fields: current
                                        .fields
                                        .iter()
                                        .zip(desired.fields.iter())
                                        .map(|(current, desired)| {
                                            merge_runtime_class(db, current, desired)
                                        })
                                        .collect::<Option<Vec<_>>>()?
                                        .into(),
                                },
                            )
                        })
                        .collect::<Option<Vec<_>>>()?
                        .into(),
                }),
            ))
        }
        (Layout::Struct(_) | Layout::Array(_) | Layout::Enum(_), _) => None,
    }
}

fn merge_ref_kind<'db>(current: &RefKind<'db>, desired: &RefKind<'db>) -> Option<RefKind<'db>> {
    match (current, desired) {
        (RefKind::Object, RefKind::Object) => Some(RefKind::Object),
        (RefKind::Const, RefKind::Const) => Some(RefKind::Const),
        (RefKind::Object, RefKind::Provider { provider_ty, space })
        | (RefKind::Provider { provider_ty, space }, RefKind::Object) => Some(RefKind::Provider {
            provider_ty: *provider_ty,
            space: *space,
        }),
        (
            RefKind::Provider {
                provider_ty: current_provider_ty,
                space: current_space,
            },
            RefKind::Provider {
                provider_ty: desired_provider_ty,
                space: desired_space,
            },
        ) => {
            let space = preferred_address_space(*current_space, *desired_space);
            let provider_ty = if current_provider_ty == desired_provider_ty {
                *current_provider_ty
            } else if *current_space == AddressSpaceKind::Memory
                && *desired_space != AddressSpaceKind::Memory
            {
                *desired_provider_ty
            } else if *desired_space == AddressSpaceKind::Memory
                && *current_space != AddressSpaceKind::Memory
            {
                *current_provider_ty
            } else {
                return None;
            };
            Some(RefKind::Provider { provider_ty, space })
        }
        _ => None,
    }
}

fn preferred_address_space(
    current: AddressSpaceKind,
    desired: AddressSpaceKind,
) -> AddressSpaceKind {
    match (current, desired) {
        (AddressSpaceKind::Memory, desired) => desired,
        (current, AddressSpaceKind::Memory) => current,
        (current, _) => current,
    }
}

fn ref_kind_address_space(kind: &RefKind<'_>) -> AddressSpaceKind {
    match kind {
        RefKind::Provider { space, .. } => *space,
        RefKind::Object | RefKind::Const => AddressSpaceKind::Memory,
    }
}

fn push_runtime_provider_binding<'db>(
    provider_bindings: &mut Vec<RuntimeProviderBinding<'db>>,
    provider: ProviderBinding<'db>,
    local: SLocalId,
    provider_class: RuntimeClass<'db>,
    place_class: RuntimeClass<'db>,
) -> RuntimeProviderBindingId {
    let id = RuntimeProviderBindingId::from_u32(provider_bindings.len() as u32);
    provider_bindings.push(RuntimeProviderBinding {
        provider,
        value: crate::runtime::RLocalId::from_u32(local.index() as u32),
        provider_class,
        place_class,
    });
    id
}

#[cfg(test)]
mod tests {
    use driver::DriverDataBase;
    use hir::analysis::ty::ty_def::TyId;

    use super::*;
    use crate::runtime::{ScalarClass, ScalarRepr, ScalarRole};

    fn test_enum_layout<'db>(
        db: &'db dyn MirDb,
        source_ty: TyId<'db>,
        payload: RuntimeClass<'db>,
    ) -> LayoutId<'db> {
        LayoutId::new(
            db,
            LayoutKey::Enum(EnumLayoutKey {
                source_ty,
                variants: vec![
                    EnumVariantLayout {
                        name: "Some".to_string(),
                        fields: vec![payload].into(),
                    },
                    EnumVariantLayout {
                        name: "None".to_string(),
                        fields: vec![].into(),
                    },
                ]
                .into(),
            }),
        )
    }

    fn test_pair_layout<'db>(db: &'db dyn MirDb) -> LayoutId<'db> {
        let word = RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits: 256,
                signed: false,
            },
            role: ScalarRole::Plain,
        });
        LayoutId::new(
            db,
            LayoutKey::Struct(StructLayout {
                source_ty: TyId::unit(db),
                fields: vec![word.clone(), word].into(),
            }),
        )
    }

    fn plain_value_join_demand() -> RuntimeJoinDemand {
        RuntimeJoinDemand {
            prefer_owned_object: false,
            prefer_transport: false,
        }
    }

    fn owned_object_join_demand() -> RuntimeJoinDemand {
        RuntimeJoinDemand {
            prefer_owned_object: true,
            prefer_transport: false,
        }
    }

    fn provider_enum_classes<'db>(
        db: &'db DriverDataBase,
    ) -> (RuntimeClass<'db>, RuntimeClass<'db>) {
        let source_ty = TyId::unit(db);
        let pointee = RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits: 256,
                signed: false,
            },
            role: ScalarRole::Plain,
        });
        let provider = |provider_ty, space, pointee| RuntimeClass::AggregateValue {
            layout: test_enum_layout(
                db,
                source_ty,
                RuntimeClass::Ref {
                    pointee: Box::new(pointee),
                    kind: RefKind::Provider { provider_ty, space },
                    view: RefView::Whole,
                },
            ),
        };
        (
            provider(TyId::bool(db), AddressSpaceKind::Storage, pointee.clone()),
            provider(TyId::u256(db), AddressSpaceKind::Memory, pointee),
        )
    }

    #[test]
    fn join_runtime_class_materializes_const_ref_to_aggregate_for_plain_values() {
        let db = DriverDataBase::default();
        let layout = test_pair_layout(&db);
        let aggregate = RuntimeClass::AggregateValue { layout };
        let const_ref = RuntimeClass::const_ref(layout);

        assert_eq!(
            join_runtime_class(&db, plain_value_join_demand(), &const_ref, &aggregate),
            Some(aggregate.clone())
        );
        assert_eq!(
            join_runtime_class(&db, plain_value_join_demand(), &aggregate, &const_ref),
            Some(aggregate)
        );
    }

    #[test]
    fn join_runtime_class_materializes_const_ref_to_object_for_owned_storage() {
        let db = DriverDataBase::default();
        let layout = test_pair_layout(&db);
        let aggregate = RuntimeClass::AggregateValue { layout };
        let const_ref = RuntimeClass::const_ref(layout);
        let object_ref = RuntimeClass::object_ref(layout);

        assert_eq!(
            join_runtime_class(&db, owned_object_join_demand(), &const_ref, &aggregate),
            Some(object_ref.clone())
        );
        assert_eq!(
            join_runtime_class(&db, owned_object_join_demand(), &aggregate, &const_ref),
            Some(object_ref)
        );
    }

    #[test]
    fn join_runtime_class_keeps_matching_const_refs() {
        let db = DriverDataBase::default();
        let const_ref = RuntimeClass::const_ref(test_pair_layout(&db));

        assert_eq!(
            join_runtime_class(&db, plain_value_join_demand(), &const_ref, &const_ref),
            Some(const_ref)
        );
    }

    #[test]
    fn merge_runtime_class_prefers_non_memory_provider_enum_layouts() {
        let db = DriverDataBase::default();
        let (storage_class, memory_class) = provider_enum_classes(&db);

        assert_eq!(
            merge_runtime_class(&db, &storage_class, &memory_class),
            Some(storage_class.clone())
        );
        assert_eq!(
            merge_runtime_class(&db, &memory_class, &storage_class),
            Some(storage_class)
        );
    }

    #[test]
    fn join_runtime_class_preserves_structural_aggregate_merge() {
        let db = DriverDataBase::default();
        let (storage_class, memory_class) = provider_enum_classes(&db);

        assert_eq!(
            join_runtime_class(
                &db,
                plain_value_join_demand(),
                &storage_class,
                &memory_class
            ),
            Some(storage_class.clone())
        );
        assert_eq!(
            join_runtime_class(
                &db,
                plain_value_join_demand(),
                &memory_class,
                &storage_class
            ),
            Some(storage_class)
        );
    }

    #[test]
    fn merge_runtime_class_prefers_provider_refs_over_object_refs() {
        let db = DriverDataBase::default();
        let pointee = RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits: 256,
                signed: false,
            },
            role: ScalarRole::Plain,
        });
        let object = RuntimeClass::Ref {
            pointee: Box::new(pointee.clone()),
            kind: RefKind::Object,
            view: RefView::Whole,
        };
        let provider = RuntimeClass::Ref {
            pointee: Box::new(pointee),
            kind: RefKind::Provider {
                provider_ty: TyId::u256(&db),
                space: AddressSpaceKind::Storage,
            },
            view: RefView::Whole,
        };

        assert_eq!(
            merge_runtime_class(&db, &object, &provider),
            Some(provider.clone())
        );
        assert_eq!(merge_runtime_class(&db, &provider, &object), Some(provider));
    }
}
