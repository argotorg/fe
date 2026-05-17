use cranelift_entity::EntityRef;
use hir::analysis::{
    semantic::{
        NEffectArg, NEffectArgValue, NOperand, NSPlace, ReadMode, SLocalId, SemanticLocalKind,
        borrowck::NLocalOrigin,
    },
    ty::ty_def::TyId,
};

use crate::runtime::{AddressSpaceKind, RuntimeBoundarySpec, RuntimeCarrier, RuntimeClass};

use super::{
    boundary::{
        BoundaryMatcher, BoundaryRef, RuntimeValueMaterialization, StagedBoundary,
        specialize_boundary_for_runtime_source_in_context,
    },
    call_input::{
        CompiledCallInputPlan, CompiledEffectArgPlan, CompiledEffectPlacePlan,
        CompiledEffectValuePlan, CompiledMaterializationPlan, CompiledValuePassPlan,
    },
    classify::{
        BodyEnv, InferClassCache, carrier_value_class, nonself_backing_value_place,
        provider_root_space, ref_class_for_place_result,
        runtime_class_for_direct_value_provider_in_context, snapshot_source_place,
    },
    realize::SelectedRuntimeArg,
    source::{RuntimeSourceMode, RuntimeSourceQuery, SemanticPlaceValueSource},
};

pub(super) struct RuntimeArgSelector<'a, 'carriers, 'roots, 'cache, 'db> {
    env: BodyEnv<'a, 'db>,
    carriers: &'carriers [RuntimeCarrier<'db>],
    source_mode: RuntimeSourceMode<'roots, 'db>,
    class_cache: Option<&'cache mut InferClassCache<'db>>,
}

impl<'a, 'carriers, 'roots, 'cache, 'db> RuntimeArgSelector<'a, 'carriers, 'roots, 'cache, 'db> {
    pub(super) fn new(
        env: BodyEnv<'a, 'db>,
        carriers: &'carriers [RuntimeCarrier<'db>],
        class_cache: Option<&'cache mut InferClassCache<'db>>,
    ) -> Self {
        Self {
            env,
            carriers,
            source_mode: RuntimeSourceMode::Abstract,
            class_cache,
        }
    }

    pub(super) fn with_concrete_roots(
        mut self,
        roots: &'roots [crate::runtime::RuntimeLocalRoot<'db>],
    ) -> Self {
        self.source_mode = RuntimeSourceMode::Concrete(roots);
        self
    }

    fn sources(&self) -> RuntimeSourceQuery<'a, 'carriers, 'roots, 'db> {
        RuntimeSourceQuery::new(self.env, self.carriers, self.source_mode)
    }

    pub(super) fn selected_actual_value(
        &mut self,
        local: SLocalId,
    ) -> Option<SelectedRuntimeArg<'db>> {
        self.select_actual_operand_value(local, copy_operand(local))
    }

    pub(super) fn selected_materialized_value(
        &mut self,
        local: SLocalId,
    ) -> Option<SelectedRuntimeArg<'db>> {
        self.select_materialized_operand_value(local, copy_operand(local))
    }

    pub(super) fn selected_call_inputs(
        &mut self,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
        plan: &CompiledCallInputPlan<'db>,
    ) -> Vec<SelectedRuntimeArg<'db>> {
        assert_eq!(
            args.len(),
            plan.param_plans.len(),
            "runtime call arg count mismatch during value evaluation: caller={:?} args={args:?} plans={:?}",
            self.env.body().owner.key(self.env.db()),
            plan.param_plans,
        );
        assert_eq!(
            effect_args.len(),
            plan.effect_plans.len(),
            "runtime effect arg count mismatch during value evaluation: caller={:?} effect_args={effect_args:?} plans={:?}",
            self.env.body().owner.key(self.env.db()),
            plan.effect_plans,
        );
        let mut selected = self.selected_param_inputs(args, &plan.param_plans);
        selected.extend(
            effect_args
                .iter()
                .zip(plan.effect_plans.iter())
                .filter_map(|(arg, effect_plan)| self.select_effect_arg(arg, effect_plan)),
        );
        selected
    }

    pub(super) fn selected_param_inputs(
        &mut self,
        args: &[NOperand],
        plans: &[CompiledValuePassPlan<'db>],
    ) -> Vec<SelectedRuntimeArg<'db>> {
        assert_eq!(
            args.len(),
            plans.len(),
            "runtime call arg count mismatch during param evaluation: caller={:?} args={args:?} plans={plans:?}",
            self.env.body().owner.key(self.env.db()),
        );
        args.iter()
            .zip(plans.iter())
            .filter_map(|(arg, plan)| self.selected_value_pass_plan(*arg, plan))
            .collect()
    }

    pub(super) fn selected_semantic_operand_for_boundary(
        &mut self,
        arg: NOperand,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> SelectedRuntimeArg<'db> {
        let local = arg.local;
        let boundary = self
            .env
            .specialize_boundary_for_source(self.carriers, local, boundary);
        match &boundary {
            RuntimeBoundarySpec::ExactTransport(target) => {
                self.selected_semantic_operand_for_class(arg, target)
            }
            RuntimeBoundarySpec::ExactShape(target) => self
                .select_runtime_boundary_compatible_value(local, &boundary)
                .unwrap_or_else(|| self.selected_semantic_operand_for_class(arg, target)),
            RuntimeBoundarySpec::BorrowLike { .. } => self
                .select_runtime_boundary_compatible_value(local, &boundary)
                .or_else(|| {
                    self.sources()
                        .semantic_operand_value_is_available(local)
                        .then(|| self.select_materialized_semantic_value(arg, &boundary))
                        .flatten()
                })
                .unwrap_or_else(|| {
                    panic!(
                        "semantic operand boundary has no runtime use plan: owner={:?}; arg={arg:?}; boundary={boundary:?}",
                        self.env.body().owner.key(self.env.db()).owner(self.env.db()),
                    )
                }),
        }
    }

    pub(super) fn selected_semantic_operand_for_class(
        &mut self,
        arg: NOperand,
        target: &RuntimeClass<'db>,
    ) -> SelectedRuntimeArg<'db> {
        self.try_selected_semantic_operand_for_class(arg, target)
            .unwrap_or_else(|| {
                panic!(
                    "semantic operand has no lowerable runtime source for class: owner={:?}; arg={arg:?}; target={target:?}; local={:?}",
                    self.env.body().owner.key(self.env.db()).owner(self.env.db()),
                    self.env.body().locals.get(arg.local.index()),
                )
            })
    }

    fn try_selected_semantic_operand_for_class(
        &mut self,
        arg: NOperand,
        target: &RuntimeClass<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        let local = arg.local;
        if matches!(target, RuntimeClass::AggregateValue { .. })
            && self.env.boundary_source_transport_sensitive(local)
            && self
                .env
                .actual_aggregate_class_for_source(self.carriers, local)
                .is_some()
        {
            return Some(SelectedRuntimeArg::aggregate_from_runtime_source(
                local,
                target.clone(),
            ));
        }
        if !target.is_transport()
            && carrier_value_class(local, self.carriers).is_some_and(|class| !class.is_transport())
        {
            return Some(SelectedRuntimeArg::runtime_value(local, target.clone()));
        }
        if !target.is_transport()
            && let Some(selected) = self.select_direct_value_materialization(local, target)
        {
            return Some(selected);
        }
        if target.is_transport() {
            if self
                .sources()
                .handle_like_semantic_value_is_available(local)
            {
                return Some(SelectedRuntimeArg::handle_like_value(local, target.clone()));
            }
            if let Some(selected) = self.select_semantic_place_address_for_class(arg, target) {
                return Some(selected);
            }
        } else if let Some(selected) = self.select_semantic_place_value_for_class(arg, target) {
            return Some(selected);
        }
        self.sources()
            .semantic_operand_value_is_available(local)
            .then(|| SelectedRuntimeArg::semantic_operand(arg, target.clone()))
    }

    pub(super) fn selected_value_pass_plan(
        &mut self,
        arg: NOperand,
        plan: &CompiledValuePassPlan<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        let local = arg.local;
        match plan {
            CompiledValuePassPlan::Erased => None,
            CompiledValuePassPlan::VisibleValue => {
                self.select_materialized_operand_value(local, arg)
            }
            CompiledValuePassPlan::ActualValue => self.select_actual_operand_value(local, arg),
            CompiledValuePassPlan::ExactTransport(exact) => {
                Some(self.selected_semantic_operand_for_class(arg, exact))
            }
            CompiledValuePassPlan::ExactShapeAggregate(exact) => self
                .select_actual_aggregate_value(local)
                .or_else(|| Some(self.selected_semantic_operand_for_class(arg, exact))),
            CompiledValuePassPlan::ExactShapeRefLike(boundary) => self
                .select_exact_shape_ref_like_value(local, boundary)
                .or_else(|| self.exact_shape_ref_like_placeholder(local, boundary)),
            CompiledValuePassPlan::ReadOnlyView { value, borrow } => self
                .select_free_boundary_compatible_value(local, borrow)
                .or_else(|| self.select_value_view_arg(local, arg, value))
                .or_else(|| self.select_materializable_semantic_value(arg, borrow)),
            CompiledValuePassPlan::BorrowLike(boundary) => {
                if let Some(selected) = self.select_boundary_compatible_value(local, boundary) {
                    return Some(selected);
                }
                self.select_materializable_semantic_value(arg, boundary)
            }
        }
    }

    pub(super) fn selected_value_for_local(
        &mut self,
        local: SLocalId,
        plan: &CompiledValuePassPlan<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        self.selected_value_pass_plan(copy_operand(local), plan)
    }

    fn select_value_view_arg(
        &mut self,
        local: SLocalId,
        arg: NOperand,
        value: &RuntimeClass<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        if matches!(value, RuntimeClass::AggregateValue { .. })
            && let Some(selected) = self.select_actual_aggregate_value(local)
        {
            return Some(selected);
        }
        self.try_selected_semantic_operand_for_class(arg, value)
    }

    fn select_materialized_operand_value(
        &mut self,
        local: SLocalId,
        arg: NOperand,
    ) -> Option<SelectedRuntimeArg<'db>> {
        match self.env.materialization_plan(local)? {
            CompiledMaterializationPlan::Erased => None,
            CompiledMaterializationPlan::SemanticValue => self
                .env
                .semantic_value_class(self.carriers, local)
                .and_then(|class| self.try_selected_semantic_operand_for_class(arg, &class)),
            CompiledMaterializationPlan::AggregateFromSource => {
                self.select_actual_aggregate_value(local)
            }
            CompiledMaterializationPlan::AggregateFromSourceOrFallback(fallback) => self
                .select_actual_aggregate_value(local)
                .or_else(|| Some(self.selected_semantic_operand_for_class(arg, fallback))),
        }
    }

    fn select_direct_value_materialization(
        &self,
        local: SLocalId,
        target: &RuntimeClass<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        let local_data = self.env.body().locals.get(local.index())?;
        if !matches!(
            (&local_data.facts.interface, &local_data.facts.origin),
            (
                SemanticLocalKind::DirectValue,
                NLocalOrigin::SelfRooted | NLocalOrigin::AliasedPlace
            )
        ) {
            return None;
        }
        let current = self.env.semantic_value_class(self.carriers, local)?;
        let materialized_class = self.materialized_value_class(local)?;
        if current == materialized_class
            || !self.direct_value_materialization_is_lowerable(local, &materialized_class)
        {
            return None;
        }
        Some(SelectedRuntimeArg::direct_value_materialization(
            local,
            materialized_class,
            target.clone(),
        ))
    }

    fn materialized_value_class(&self, local: SLocalId) -> Option<RuntimeClass<'db>> {
        match self.env.materialization_plan(local)? {
            CompiledMaterializationPlan::Erased => None,
            CompiledMaterializationPlan::SemanticValue => {
                self.env.semantic_value_class(self.carriers, local)
            }
            CompiledMaterializationPlan::AggregateFromSource => self
                .env
                .actual_aggregate_class_for_source(self.carriers, local),
            CompiledMaterializationPlan::AggregateFromSourceOrFallback(fallback) => self
                .env
                .actual_aggregate_class_for_source(self.carriers, local)
                .or_else(|| Some(fallback.clone())),
        }
    }

    fn direct_value_materialization_is_lowerable(
        &self,
        local: SLocalId,
        materialized_class: &RuntimeClass<'db>,
    ) -> bool {
        let Some(local_data) = self.env.body().locals.get(local.index()) else {
            return false;
        };
        local_data
            .backing_place()
            .is_some_and(|place| self.sources().place_is_lowerable(place))
            || self.direct_value_transport_place_is_lowerable(local, materialized_class)
    }

    fn direct_value_transport_place_is_lowerable(
        &self,
        local: SLocalId,
        materialized_class: &RuntimeClass<'db>,
    ) -> bool {
        match carrier_value_class(local, self.carriers) {
            Some(RuntimeClass::Ref { .. })
            | Some(RuntimeClass::RawAddr {
                target: Some(_), ..
            }) => true,
            Some(RuntimeClass::RawAddr { target: None, .. }) => {
                matches!(materialized_class, RuntimeClass::Scalar(_))
            }
            Some(RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. }) | None => false,
        }
    }

    fn select_semantic_place_value_for_class(
        &self,
        arg: NOperand,
        target: &RuntimeClass<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        match self.sources().semantic_place_value_source(arg.local)? {
            SemanticPlaceValueSource::PlaceValue { place, semantic_ty } => Some(
                SelectedRuntimeArg::place_load(place, semantic_ty, target.clone()),
            ),
            SemanticPlaceValueSource::ValueExtract { place, semantic_ty } => Some(
                SelectedRuntimeArg::value_extract(place, semantic_ty, target.clone()),
            ),
        }
    }

    fn select_semantic_place_address_for_class(
        &self,
        arg: NOperand,
        target: &RuntimeClass<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        let local_data = self.env.body().locals.get(arg.local.index())?;
        if let Some(place) = nonself_backing_value_place(self.env.body(), arg.local)
            && let Some(arg) = self.select_place_address_if_satisfies(
                place.clone(),
                local_data.ty,
                &RuntimeBoundarySpec::ExactTransport(target.clone()),
                true,
            )
        {
            return Some(arg);
        }
        if matches!(
            local_data.facts.interface,
            SemanticLocalKind::DirectValue
                | SemanticLocalKind::PlaceCarrier
                | SemanticLocalKind::PlaceBoundValue
        ) && self
            .sources()
            .semantic_operand_place_address_is_lowerable(arg.local)
        {
            return Some(SelectedRuntimeArg::semantic_place_addr(
                arg.local,
                local_data.ty,
                target.clone(),
            ));
        }
        None
    }

    fn select_actual_operand_value(
        &mut self,
        local: SLocalId,
        arg: NOperand,
    ) -> Option<SelectedRuntimeArg<'db>> {
        carrier_value_class(local, self.carriers)
            .map(|class| SelectedRuntimeArg::local_value(local, class))
            .or_else(|| {
                self.env
                    .semantic_value_class(self.carriers, local)
                    .and_then(|class| self.try_selected_semantic_operand_for_class(arg, &class))
            })
    }

    fn select_actual_aggregate_value(
        &mut self,
        local: SLocalId,
    ) -> Option<SelectedRuntimeArg<'db>> {
        self.env
            .actual_aggregate_class_for_source(self.carriers, local)
            .map(|class| SelectedRuntimeArg::aggregate_from_runtime_source(local, class))
    }

    fn select_boundary_compatible_value(
        &mut self,
        local: SLocalId,
        boundary: &StagedBoundary<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        let boundary = self.specialized_boundary(local, boundary);
        self.select_runtime_boundary_compatible_value(local, &boundary)
    }

    fn select_free_boundary_compatible_value(
        &mut self,
        local: SLocalId,
        boundary: &StagedBoundary<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        let boundary = self.specialized_boundary(local, boundary);
        self.select_runtime_boundary_compatible_value_for_places(local, &boundary, false)
    }

    fn select_runtime_boundary_compatible_value(
        &mut self,
        local: SLocalId,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        self.select_runtime_boundary_compatible_value_for_places(local, boundary, true)
    }

    fn select_runtime_boundary_compatible_value_for_places(
        &mut self,
        local: SLocalId,
        boundary: &RuntimeBoundarySpec<'db>,
        allow_new_place_address: bool,
    ) -> Option<SelectedRuntimeArg<'db>> {
        let local_data = self.env.body().locals.get(local.index())?;
        let semantic_ty = local_data.ty;
        if let Some(class) = carrier_value_class(local, self.carriers)
            && BoundaryMatcher::class_satisfies_boundary(&class, boundary)
        {
            return Some(SelectedRuntimeArg::local_value(local, class));
        }
        if let Some(value_class) = self.env.semantic_value_class(self.carriers, local) {
            if let Some(provider) = local_data.facts.origin.root_provider()
                && let Some(root_class) = self
                    .env
                    .actual_runtime_visible_root_provider_class(self.carriers, provider)
                    .map(|(_, class)| class)
                    .or_else(|| {
                        runtime_class_for_direct_value_provider_in_context(
                            self.env.db(),
                            provider,
                            self.env.scope(),
                            self.env.assumptions(),
                        )
                    })
            {
                let class = ref_class_for_place_result(
                    &root_class,
                    &value_class,
                    provider_root_space(provider, &root_class),
                    false,
                );
                if BoundaryMatcher::class_satisfies_boundary(&class, boundary) {
                    return Some(SelectedRuntimeArg::handle_like_value(local, class));
                }
            }
            let cx = self.env.with_carriers(self.carriers);
            if allow_new_place_address
                && self.sources().local_has_existing_runtime_root(local)
                && let Some(root_class) = super::infer::local_place_root_class(
                    cx,
                    local,
                    local_data,
                    self.carriers.get(local.index())?,
                )
            {
                let class = ref_class_for_place_result(
                    &root_class,
                    &value_class,
                    AddressSpaceKind::Memory,
                    false,
                );
                if BoundaryMatcher::class_satisfies_boundary(&class, boundary) {
                    return Some(SelectedRuntimeArg::handle_like_value(local, class));
                }
            }
        }

        [
            local_data.backing_place().cloned(),
            snapshot_source_place(self.env.body(), local).cloned(),
        ]
        .into_iter()
        .flatten()
        .find_map(|place| {
            self.select_place_address_if_satisfies(
                place,
                semantic_ty,
                boundary,
                allow_new_place_address,
            )
        })
    }

    fn select_place_address_if_satisfies(
        &self,
        place: NSPlace<'db>,
        semantic_ty: TyId<'db>,
        boundary: &RuntimeBoundarySpec<'db>,
        allow_new_place_address: bool,
    ) -> Option<SelectedRuntimeArg<'db>> {
        let sources = self.sources();
        if !(if allow_new_place_address {
            sources.place_is_lowerable(&place)
        } else {
            sources.place_has_existing_runtime_root(&place)
        }) {
            return None;
        }
        let class = self
            .env
            .normalized_place_address_class(self.carriers, &place)?;
        BoundaryMatcher::class_satisfies_boundary(&class, boundary)
            .then(|| SelectedRuntimeArg::place_addr(place, semantic_ty, class))
    }

    fn select_exact_shape_ref_like_value(
        &mut self,
        local: SLocalId,
        boundary: &StagedBoundary<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        let boundary = self.specialized_boundary(local, boundary);
        match &boundary {
            RuntimeBoundarySpec::ExactShape(
                RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. },
            ) => self.select_runtime_boundary_compatible_value(local, &boundary),
            RuntimeBoundarySpec::ExactShape(_) | RuntimeBoundarySpec::ExactTransport(_) => None,
            RuntimeBoundarySpec::BorrowLike { .. } => unreachable!(),
        }
    }

    fn exact_shape_ref_like_placeholder(
        &mut self,
        local: SLocalId,
        boundary: &StagedBoundary<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        let RuntimeBoundarySpec::ExactShape(class) = self.specialized_boundary(local, boundary)
        else {
            panic!(
                "exact-shape ref-like pass plan specialized to non-exact-shape boundary: owner={:?}; local={local:?}; boundary={boundary:?}",
                self.env
                    .body()
                    .owner
                    .key(self.env.db())
                    .owner(self.env.db()),
            );
        };
        Some(SelectedRuntimeArg::placeholder(
            self.env.body().locals[local.index()].ty,
            class,
        ))
    }

    fn select_place_for_boundary(
        &self,
        place: NSPlace<'db>,
        semantic_ty: TyId<'db>,
        boundary: RuntimeBoundarySpec<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        if !self.sources().place_is_lowerable(&place) {
            return self.placeholder_arg_for_unlowerable_place(place, semantic_ty, &boundary);
        }
        if let Some(class) = self
            .env
            .normalized_place_address_class(self.carriers, &place)
            && BoundaryMatcher::class_satisfies_boundary(&class, &boundary)
        {
            return Some(SelectedRuntimeArg::place_addr(place, semantic_ty, class));
        }
        match &boundary {
            RuntimeBoundarySpec::ExactTransport(target) if target.is_transport() => Some(
                SelectedRuntimeArg::place_addr(place, semantic_ty, target.clone()),
            ),
            RuntimeBoundarySpec::ExactTransport(target) => Some(SelectedRuntimeArg::place_load(
                place,
                semantic_ty,
                target.clone(),
            )),
            RuntimeBoundarySpec::ExactShape(target) if !target.is_transport() => Some(
                SelectedRuntimeArg::place_load(place, semantic_ty, target.clone()),
            ),
            RuntimeBoundarySpec::BorrowLike { .. } => {
                RuntimeValueMaterialization::for_boundary(&boundary)
                    .map(|materialization| {
                        SelectedRuntimeArg::materialized_place(
                            place.clone(),
                            semantic_ty,
                            materialization,
                        )
                    })
                    .or_else(|| {
                        self.placeholder_arg_for_unlowerable_place(place, semantic_ty, &boundary)
                    })
            }
            RuntimeBoundarySpec::ExactShape(_) => {
                self.placeholder_arg_for_unlowerable_place(place, semantic_ty, &boundary)
            }
        }
    }

    fn placeholder_arg_for_unlowerable_place(
        &self,
        place: NSPlace<'db>,
        semantic_ty: TyId<'db>,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        if !place.path.is_empty() {
            return None;
        }
        BoundaryMatcher::placeholder_class(boundary)
            .map(|class| SelectedRuntimeArg::placeholder(semantic_ty, class))
    }

    fn select_effect_arg(
        &mut self,
        arg: &NEffectArg<'db>,
        plan: &CompiledEffectArgPlan<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        match (EffectArgInput::new(arg), plan) {
            (_, CompiledEffectArgPlan::Erased) => None,
            (EffectArgInput::Value(value), CompiledEffectArgPlan::Value(plan)) => {
                self.select_value_effect_arg(value, plan)
            }
            (EffectArgInput::Place(place), CompiledEffectArgPlan::Place(plan)) => {
                Some(self.select_place_effect_arg(arg, place, plan))
            }
            (EffectArgInput::Value(_), CompiledEffectArgPlan::Place(_))
            | (EffectArgInput::Place(_), CompiledEffectArgPlan::Value(_)) => {
                panic!(
                    "compiled effect arg source kind mismatch: owner={:?}; arg={arg:?}; plan={plan:?}",
                    self.env
                        .body()
                        .owner
                        .key(self.env.db())
                        .owner(self.env.db()),
                )
            }
        }
    }

    fn select_value_effect_arg(
        &mut self,
        value: NOperand,
        plan: &CompiledEffectValuePlan<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        match plan {
            CompiledEffectValuePlan::ErasedPlainValue => {
                self.select_materialized_operand_value(value.local, value)
            }
            CompiledEffectValuePlan::ByValue(plan) => self.selected_value_pass_plan(value, plan),
            CompiledEffectValuePlan::ByValueFallback(fallback) => self
                .select_actual_operand_value(value.local, value)
                .or_else(|| {
                    Some(SelectedRuntimeArg::placeholder(
                        self.env.body().locals[value.local.index()].ty,
                        fallback.clone(),
                    ))
                }),
            CompiledEffectValuePlan::ByPlace {
                boundary,
                allow_materialize,
            } => self
                .select_boundary_compatible_value(value.local, boundary)
                .or_else(|| {
                    allow_materialize
                        .then(|| self.select_materializable_semantic_value(value, boundary))
                        .flatten()
                }),
        }
    }

    fn select_materializable_semantic_value(
        &mut self,
        arg: NOperand,
        boundary: &StagedBoundary<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        if !self
            .sources()
            .semantic_operand_value_is_available(arg.local)
        {
            return None;
        }
        let boundary = self.specialized_boundary(arg.local, boundary);
        self.select_materialized_semantic_value(arg, &boundary)
    }

    fn select_materialized_semantic_value(
        &self,
        arg: NOperand,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> Option<SelectedRuntimeArg<'db>> {
        let materialization = RuntimeValueMaterialization::for_boundary(boundary)?;
        let selected = match self.sources().semantic_place_value_source(arg.local) {
            Some(SemanticPlaceValueSource::PlaceValue { place, semantic_ty }) => {
                SelectedRuntimeArg::materialized_place(place, semantic_ty, materialization)
            }
            Some(SemanticPlaceValueSource::ValueExtract { place, semantic_ty }) => {
                let value_class = self.env.semantic_value_class(self.carriers, arg.local)?;
                SelectedRuntimeArg::materialized_value_extract(
                    place,
                    semantic_ty,
                    value_class,
                    materialization,
                )
            }
            None => SelectedRuntimeArg::materialized_semantic_operand(arg, materialization),
        };
        Some(selected)
    }

    fn select_place_effect_arg(
        &self,
        arg: &NEffectArg<'db>,
        place: &NSPlace<'db>,
        plan: &CompiledEffectPlacePlan<'db>,
    ) -> SelectedRuntimeArg<'db> {
        match plan {
            CompiledEffectPlacePlan::Boundary(boundary) => {
                self.select_effect_place_for_boundary(arg, place, boundary.boundary.clone())
            }
            CompiledEffectPlacePlan::Fallback(fallback) => self.select_effect_place_for_boundary(
                arg,
                place,
                RuntimeBoundarySpec::ExactTransport(fallback.clone()),
            ),
        }
    }

    fn select_effect_place_for_boundary(
        &self,
        arg: &NEffectArg<'db>,
        place: &NSPlace<'db>,
        boundary: RuntimeBoundarySpec<'db>,
    ) -> SelectedRuntimeArg<'db> {
        self.select_place_for_boundary(
            place.clone(),
            self.effect_arg_target_ty(arg),
            boundary.clone(),
        )
        .unwrap_or_else(|| {
            panic!(
                "effect place arg boundary has no runtime use plan: owner={:?}; arg={arg:?}; boundary={boundary:?}",
                self.env.body().owner.key(self.env.db()).owner(self.env.db()),
            )
        })
    }

    fn effect_arg_target_ty(&self, arg: &NEffectArg<'db>) -> TyId<'db> {
        arg.target_ty.unwrap_or_else(|| TyId::unit(self.env.db()))
    }

    fn specialized_boundary(
        &mut self,
        local: SLocalId,
        boundary: &StagedBoundary<'db>,
    ) -> RuntimeBoundarySpec<'db> {
        specialize_boundary_for_runtime_source_in_context(
            self.env,
            local,
            BoundaryRef::staged(boundary),
            self.carriers,
            self.class_cache.as_deref_mut(),
        )
        .boundary
        .into_owned()
    }
}

enum EffectArgInput<'arg, 'db> {
    Value(NOperand),
    Place(&'arg NSPlace<'db>),
}

impl<'arg, 'db> EffectArgInput<'arg, 'db> {
    fn new(arg: &'arg NEffectArg<'db>) -> Self {
        match &arg.arg {
            NEffectArgValue::Value(value) => Self::Value(*value),
            NEffectArgValue::Place(place) => Self::Place(place),
        }
    }
}

fn copy_operand(local: SLocalId) -> NOperand {
    NOperand {
        local,
        origin: None,
        mode: ReadMode::Copy,
    }
}
