use common::indexmap::IndexSet;
use cranelift_entity::{EntityRef, PrimaryMap};
use hir::analysis::{
    semantic::{
        FieldIndex, GenericSubst, ImplEnv, NEffectArg, ReadMode, SBlockId, SConst, SLocalId,
        SemanticCalleeRef, SemanticInstance, SemanticInstanceKey, SemanticLocalKind,
        SemanticLocalRole, ValueProvenance, VariantIndex,
        borrowck::{
            NAssignmentId, NBorrowRoot, NExpr, NLocalOrigin, NOperand, NSPlace, NSPlaceRoot,
            NormalizedBindingLowering, NormalizedBodyFacts, NormalizedSemanticBody,
        },
        get_or_build_semantic_instance,
    },
    ty::{
        ProviderKind,
        corelib::runtime_builtin_func_kind,
        normalize::normalize_ty,
        provider::registered_root_providers,
        trait_def::{TraitInstId, resolve_trait_method_instance},
        trait_resolution::{PredicateListId, TraitSolveCx},
        ty_check::{BodyOwner, EffectParamSite, EffectPassMode, LocalBinding, ParamSite},
        ty_def::{CapabilityKind, TyData, TyId, strip_derived_adt_layout_args},
    },
};
use hir::hir_def::{ArithBinOp, FuncParamMode, ItemKind};
use hir::projection::Projection;
use hir::semantic::{ProviderBinding, ProviderSource, constraints_for};
use rustc_hash::FxHashMap;
use salsa::Update;

use crate::{
    db::MirDb,
    instance::{RuntimeInstanceKey, RuntimeInstanceSource},
    runtime::{
        AddressSpaceKind, BorrowAccess, Layout, LayoutId, RefKind, RefView, RuntimeBoundarySpec,
        RuntimeCarrier, RuntimeClass, RuntimeCodeRegion, RuntimeCodeRegionKey, RuntimeParamPlan,
        SaturatingBinOp, ScalarClass, ScalarRepr, ScalarRole, VariantId,
    },
};

use super::{
    arg_selector::RuntimeArgSelector,
    boundary::{
        BoundaryRef, BoundarySiteAllocator, BoundarySpecializationCache, RuntimeClassShape,
        StagedBoundary, aggregate_transport_depends_on_runtime_source,
        boundary_source_uses_transport_sensitive_aggregate, boundary_spec_for_ty_in_env,
        default_borrow_transport_set, specialize_boundary_for_aggregate_layout,
        specialize_boundary_for_runtime_source_in_context,
    },
    call_input::{
        CompiledCallInputPlan, CompiledMaterializationPlan, compile_call_input_plan_for_semantic,
        compile_value_pass_plan,
    },
    consts::{reified_const_ref_value_for_ty, runtime_const_value_class},
    infer::{fallback_root_transport_class, local_place_root_class},
    interface::runtime_visible_binding_plans,
    layout::{
        layout_for_aggregate_instance_in_context, layout_for_enum_variant_instance_in_context,
        layout_for_ty_in_context,
    },
    place::{
        address_space_from_provider, project_field_class, project_index_class,
        project_variant_field_class,
    },
    realize::SelectedRuntimeArg,
    returns::{StaticRuntimeReturnDecision, static_runtime_return_decision},
    type_info::{
        RuntimeTypeEnv, effect_handle_class_for_ty_in_context, provider_address_space_to_runtime,
        provider_class_for_target_in_context, provider_class_for_target_in_env,
        runtime_interface_ty_in_context, runtime_repr_ty_in_context, runtime_zero_sized_ty,
        scalar_class_for_ty_in_env, stored_class_for_ty_in_context,
        top_level_class_for_ty_in_context, top_level_class_for_ty_in_env,
    },
};

#[derive(Clone)]
pub(crate) struct BodyStaticFacts<'db> {
    normalized_facts: NormalizedBodyFacts,
    local_facts: Vec<LocalStaticFacts<'db>>,
    assignments: PrimaryMap<AssignmentId, AssignStaticFacts<'db>>,
    root_provider_locals: FxHashMap<ProviderBinding<'db>, SLocalId>,
}

pub(crate) type AssignmentId = NAssignmentId;

#[derive(Clone)]
pub(super) struct AssignStaticFacts<'db> {
    pub(super) block_idx: usize,
    pub(super) stmt_idx: usize,
    pub(super) dst: SLocalId,
    expr: Option<ExprStaticFacts<'db>>,
}

#[derive(Default)]
pub(super) struct InferClassCache<'db> {
    pub(super) boundary_specializations: BoundarySpecializationCache<'db>,
    local_versions: Vec<u32>,
    local_dynamic_facts: Vec<CachedLocalDynamicFacts<'db>>,
}

impl<'db> InferClassCache<'db> {
    pub(super) fn new(local_count: usize) -> Self {
        Self {
            boundary_specializations: BoundarySpecializationCache::default(),
            local_versions: vec![0; local_count],
            local_dynamic_facts: vec![CachedLocalDynamicFacts::default(); local_count],
        }
    }

    pub(super) fn note_carrier_changed(&mut self, local: SLocalId) {
        let version = self
            .local_versions
            .get_mut(local.index())
            .unwrap_or_else(|| panic!("missing local version slot for {local:?}"));
        *version += 1;
        if let Some(entry) = self.local_dynamic_facts.get_mut(local.index()) {
            entry.facts = None;
        }
    }

    pub(super) fn invalidate_local_dynamic_facts(&mut self, local: SLocalId) {
        if let Some(entry) = self.local_dynamic_facts.get_mut(local.index()) {
            entry.facts = None;
        }
    }

    pub(super) fn local_dynamic_facts(
        &mut self,
        env: BodyEnv<'_, 'db>,
        local: SLocalId,
        carriers: &[RuntimeCarrier<'db>],
    ) -> Option<LocalDynamicFacts<'db>> {
        env.local_facts(local)?;
        let source_locals = env.facts.source_locals(local);
        let self_version = *self.local_versions.get(local.index())?;
        let entry = self
            .local_dynamic_facts
            .get_mut(local.index())
            .unwrap_or_else(|| panic!("missing dynamic fact cache entry for {local:?}"));
        if let Some(facts) = entry.facts.as_ref()
            && entry.self_version == self_version
            && source_locals
                .iter()
                .zip(entry.source_versions.iter())
                .all(|(dep, version)| self.local_versions[dep.index()] == *version)
        {
            return Some(facts.clone());
        }
        let facts = LocalDynamicFacts::compute(env, local, carriers);
        entry.self_version = self_version;
        entry.source_versions = source_locals
            .iter()
            .map(|dep| self.local_versions[dep.index()])
            .collect();
        entry.facts = facts.clone();
        facts
    }
}

#[derive(Clone, Debug)]
pub(super) struct LocalDynamicFacts<'db> {
    pub(super) exact_source_shape: Option<RuntimeClassShape<'db>>,
    pub(super) aggregate_layout: Option<LayoutId<'db>>,
}

impl<'db> LocalDynamicFacts<'db> {
    fn compute(
        env: BodyEnv<'_, 'db>,
        local: SLocalId,
        carriers: &[RuntimeCarrier<'db>],
    ) -> Option<Self> {
        let local_static = env.local_facts(local)?;
        let exact_source_shape = carrier_value_class_ref(local, carriers)
            .map(RuntimeClassShape::from_class)
            .or_else(|| {
                env.semantic_value_class(carriers, local)
                    .as_ref()
                    .map(RuntimeClassShape::from_class)
            });
        let aggregate_layout = local_static
            .boundary_source_transport_sensitive
            .then(|| env.actual_aggregate_class_for_source(carriers, local))
            .flatten()
            .and_then(|class| class.aggregate_layout());
        Some(Self {
            exact_source_shape,
            aggregate_layout,
        })
    }
}

#[derive(Clone, Debug, Default)]
struct CachedLocalDynamicFacts<'db> {
    facts: Option<LocalDynamicFacts<'db>>,
    self_version: u32,
    source_versions: Box<[u32]>,
}

#[derive(Clone)]
struct LocalStaticFacts<'db> {
    boundary_source_transport_sensitive: bool,
    semantic_fallback_class: Option<RuntimeClass<'db>>,
    root_place_fallback_class: Option<RuntimeClass<'db>>,
    root_transport_fallback_class: Option<RuntimeClass<'db>>,
    pub(super) materialization_plan: CompiledMaterializationPlan<'db>,
}

#[derive(Clone)]
enum ExprStaticFacts<'db> {
    Const(Option<RuntimeClass<'db>>),
    DirectClass(Option<RuntimeClass<'db>>),
    AggregateMake(AggregateMakeStaticFacts<'db>),
    Borrow {
        provider_fallback: Option<RuntimeClass<'db>>,
    },
    Call(CallStaticFacts<'db>),
}

#[derive(Clone)]
struct AggregateMakeStaticFacts<'db> {
    direct_class: Option<RuntimeClass<'db>>,
    ctor: AggregateCtorKind<'db>,
    fields: Vec<AggregateMakeFieldStaticFacts<'db>>,
}

#[derive(Clone)]
enum AggregateCtorKind<'db> {
    Aggregate(TyId<'db>),
    EnumVariant {
        enum_ty: TyId<'db>,
        variant: VariantIndex,
    },
}

#[derive(Clone)]
struct AggregateMakeFieldStaticFacts<'db> {
    boundary: Option<StagedBoundary<'db>>,
    stored_class: RuntimeClass<'db>,
}

#[derive(Clone)]
struct CallStaticFacts<'db> {
    semantic: SemanticInstance<'db>,
    builtin_return_class: Option<Option<RuntimeClass<'db>>>,
    return_decision: StaticRuntimeReturnDecision<'db>,
    input_plan: Option<CompiledCallInputPlan<'db>>,
}

impl<'db> BodyStaticFacts<'db> {
    pub(crate) fn new(db: &'db dyn MirDb, body: &NormalizedSemanticBody<'db>) -> Self {
        let typed_body = body.owner.key(db).typed_body(db);
        let type_env = RuntimeTypeEnv::for_semantic(db, body.owner);
        Self::new_in_context(db, body, typed_body, type_env)
    }

    pub(super) fn new_in_context(
        db: &'db dyn MirDb,
        body: &NormalizedSemanticBody<'db>,
        typed_body: &hir::analysis::ty::ty_check::TypedBody<'db>,
        type_env: RuntimeTypeEnv<'db>,
    ) -> Self {
        let mut boundary_sites = BoundarySiteAllocator::default();
        let normalized_facts = NormalizedBodyFacts::new(body);
        let expr_facts_builder = ExprStaticFactsBuilder {
            db,
            body,
            typed_body,
            type_env,
        };
        let local_facts: Vec<_> = body
            .locals
            .iter()
            .enumerate()
            .map(|(idx, local_data)| {
                let local = SLocalId::from_u32(idx as u32);
                build_local_static_facts(db, type_env, local, local_data)
            })
            .collect();
        let mut assignments = PrimaryMap::new();
        for (assign_id, structural) in normalized_facts.assignments().iter() {
            let stmt = &body.blocks[structural.block.index()].stmts[structural.stmt_idx];
            let hir::analysis::semantic::NSStmtKind::Assign { dst, expr } = &stmt.kind else {
                panic!(
                    "normalized assignment facts point to non-assignment statement: block={} stmt={}",
                    structural.block.index(),
                    structural.stmt_idx
                );
            };
            let result_ty = body
                .locals
                .get(dst.index())
                .unwrap_or_else(|| panic!("missing assignment local for {dst:?}"))
                .ty;
            let expr_facts = expr_facts_builder.build(expr, *dst, result_ty, &mut boundary_sites);
            let pushed = assignments.push(AssignStaticFacts {
                block_idx: structural.block.index(),
                stmt_idx: structural.stmt_idx,
                dst: *dst,
                expr: expr_facts,
            });
            debug_assert_eq!(pushed, assign_id);
        }
        let root_provider_locals = build_runtime_visible_root_provider_locals(db, body.owner);
        Self {
            normalized_facts,
            local_facts,
            assignments,
            root_provider_locals,
        }
    }

    fn local(&self, local: SLocalId) -> Option<&LocalStaticFacts<'db>> {
        self.local_facts.get(local.index())
    }

    fn expr(&self, block_idx: usize, stmt_idx: usize) -> Option<&ExprStaticFacts<'db>> {
        let assign_id = self
            .normalized_facts
            .stmt_assignment(SBlockId::new(block_idx), stmt_idx)?;
        self.assignments.get(assign_id)?.expr.as_ref()
    }

    pub(super) fn assignment(&self, assign_id: AssignmentId) -> Option<&AssignStaticFacts<'db>> {
        self.assignments.get(assign_id)
    }

    pub(super) fn assignments(&self) -> &PrimaryMap<AssignmentId, AssignStaticFacts<'db>> {
        &self.assignments
    }

    pub(super) fn source_locals(&self, local: SLocalId) -> &[SLocalId] {
        self.normalized_facts.local_source_uses(local)
    }

    pub(super) fn boundary_source_transport_sensitive(&self, local: SLocalId) -> bool {
        self.local(local)
            .is_some_and(|facts| facts.boundary_source_transport_sensitive)
    }

    fn assignments_using_local(&self, local: SLocalId) -> &[AssignmentId] {
        self.normalized_facts.assignments_using_local(local)
    }

    pub(super) fn assignments_defining_local(&self, local: SLocalId) -> &[AssignmentId] {
        self.normalized_facts.defs_by_local(local)
    }

    pub(super) fn assignment_uses(&self, assign_id: AssignmentId) -> &[SLocalId] {
        self.normalized_facts
            .assignment(assign_id)
            .map(|assignment| assignment.uses())
            .unwrap_or(&[])
    }

    fn dynamic_dependents(&self, local: SLocalId) -> &[SLocalId] {
        self.normalized_facts.dynamic_dependents(local)
    }

    fn root_provider_local(&self, provider: &ProviderBinding<'db>) -> Option<SLocalId> {
        self.root_provider_locals.get(provider).copied()
    }
}

#[derive(Clone, Copy)]
pub(crate) struct BodyEnv<'a, 'db> {
    db: &'db dyn MirDb,
    body: &'a NormalizedSemanticBody<'db>,
    type_env: RuntimeTypeEnv<'db>,
    facts: &'a BodyStaticFacts<'db>,
}

impl<'a, 'db> BodyEnv<'a, 'db> {
    pub(crate) fn new(
        db: &'db dyn MirDb,
        body: &'a NormalizedSemanticBody<'db>,
        facts: &'a BodyStaticFacts<'db>,
    ) -> Self {
        let type_env = RuntimeTypeEnv::for_semantic(db, body.owner);
        Self::from_parts(db, body, type_env, facts)
    }

    pub(super) fn from_parts(
        db: &'db dyn MirDb,
        body: &'a NormalizedSemanticBody<'db>,
        type_env: RuntimeTypeEnv<'db>,
        facts: &'a BodyStaticFacts<'db>,
    ) -> Self {
        Self {
            db,
            body,
            type_env,
            facts,
        }
    }

    pub(super) fn db(self) -> &'db dyn MirDb {
        self.db
    }

    pub(super) fn body(self) -> &'a NormalizedSemanticBody<'db> {
        self.body
    }

    pub(super) fn scope(self) -> Option<hir::hir_def::scope_graph::ScopeId<'db>> {
        self.type_env.scope
    }

    pub(super) fn assumptions(self) -> PredicateListId<'db> {
        self.type_env.assumptions
    }

    fn local_facts(self, local: SLocalId) -> Option<&'a LocalStaticFacts<'db>> {
        self.facts.local(local)
    }

    pub(super) fn materialization_plan(
        self,
        local: SLocalId,
    ) -> Option<&'a CompiledMaterializationPlan<'db>> {
        self.local_facts(local)
            .map(|facts| &facts.materialization_plan)
    }

    fn expr_facts(self, block_idx: usize, stmt_idx: usize) -> Option<&'a ExprStaticFacts<'db>> {
        self.facts.expr(block_idx, stmt_idx)
    }

    pub(super) fn assignment(self, assign_id: AssignmentId) -> Option<&'a AssignStaticFacts<'db>> {
        self.facts.assignment(assign_id)
    }

    pub(super) fn assignment_count(self) -> usize {
        self.facts.assignments.len()
    }

    pub(super) fn assignment_ids(self) -> Vec<AssignmentId> {
        self.facts.assignments.keys().collect()
    }

    pub(super) fn assignments_using_local(self, local: SLocalId) -> &'a [AssignmentId] {
        self.facts.assignments_using_local(local)
    }

    pub(super) fn dynamic_dependents(self, local: SLocalId) -> &'a [SLocalId] {
        self.facts.dynamic_dependents(local)
    }

    fn actual_runtime_visible_root_provider_local(
        self,
        provider: &ProviderBinding<'db>,
    ) -> Option<SLocalId> {
        self.facts.root_provider_local(provider)
    }

    pub(super) fn actual_runtime_visible_root_provider_class(
        self,
        carriers: &[RuntimeCarrier<'db>],
        provider: &ProviderBinding<'db>,
    ) -> Option<(SLocalId, RuntimeClass<'db>)> {
        let local = self.actual_runtime_visible_root_provider_local(provider)?;
        carrier_value_class(local, carriers).map(|class| (local, class))
    }

    pub(super) fn boundary_source_transport_sensitive(self, local: SLocalId) -> bool {
        self.local_facts(local)
            .is_some_and(|facts| facts.boundary_source_transport_sensitive)
    }

    pub(super) fn root_place_fallback_class(self, local: SLocalId) -> Option<RuntimeClass<'db>> {
        self.local_facts(local)
            .and_then(|facts| facts.root_place_fallback_class.clone())
    }

    pub(super) fn root_transport_fallback_class(
        self,
        local: SLocalId,
    ) -> Option<RuntimeClass<'db>> {
        self.local_facts(local)
            .and_then(|facts| facts.root_transport_fallback_class.clone())
    }

    pub(super) fn with_carriers<'carriers>(
        self,
        carriers: &'carriers [RuntimeCarrier<'db>],
    ) -> RuntimeBodyCx<'a, 'carriers, 'db> {
        RuntimeBodyCx {
            env: self,
            carriers,
        }
    }

    pub(super) fn expr_direct_class(
        self,
        carriers: &[RuntimeCarrier<'db>],
        block_idx: usize,
        stmt_idx: usize,
        expr: &NExpr<'db>,
        mut class_cache: Option<&mut InferClassCache<'db>>,
        lookup_return_class: &mut dyn FnMut(RuntimeInstanceKey<'db>) -> Option<RuntimeClass<'db>>,
    ) -> Option<RuntimeClass<'db>> {
        let expr_facts = self.expr_facts(block_idx, stmt_idx);
        Some(match expr {
            NExpr::Use(value) => {
                RuntimeArgSelector::new(self, carriers, class_cache)
                    .selected_materialized_value(value.local)?
                    .class
            }
            NExpr::Const(_)
            | NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::Cast { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. }
            | NExpr::IsEnumVariant { .. } => match expr_facts {
                Some(ExprStaticFacts::Const(class) | ExprStaticFacts::DirectClass(class)) => {
                    class.clone()?
                }
                _ => panic!(
                    "missing staged runtime class facts: owner={:?}; expr={expr:?}",
                    self.body.owner.key(self.db),
                ),
            },
            NExpr::CodeRegionRef { .. } => return None,
            NExpr::ArrayRepeat { .. } => {
                panic!(
                    "array repeat with non-concrete length reached runtime class inference: {expr:?}"
                )
            }
            NExpr::GetEnumTag { value } => {
                let enum_layout = self
                    .semantic_value_class(carriers, value.local)?
                    .aggregate_layout()
                    .expect("enum tag source should have aggregate layout");
                RuntimeClass::Scalar(ScalarClass {
                    repr: match enum_layout.data(self.db) {
                        Layout::Enum(layout) => layout.tag.repr,
                        Layout::Struct(_) | Layout::Array(_) => {
                            panic!("enum tag source should lower as enum layout: {enum_layout:?}")
                        }
                    },
                    role: ScalarRole::EnumTag { enum_layout },
                })
            }
            NExpr::AggregateMake { fields, .. } | NExpr::EnumMake { fields, .. } => {
                let Some(ExprStaticFacts::AggregateMake(facts)) = expr_facts else {
                    panic!(
                        "missing staged aggregate facts: owner={:?}; expr={expr:?}",
                        self.body.owner.key(self.db),
                    );
                };
                aggregate_make_class_from_facts(
                    self,
                    facts,
                    fields,
                    carriers,
                    class_cache.as_deref_mut(),
                )?
            }
            NExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => {
                let value_class = self.semantic_value_class(carriers, value.local)?;
                let variant = VariantId {
                    enum_layout: value_class
                        .aggregate_layout()
                        .expect("enum extract source should have aggregate layout"),
                    index: variant.0,
                };
                project_variant_field_class(self.db, value_class, variant, FieldIndex(field.0))
            }
            NExpr::ReadPlace { place, .. } => match expr_facts {
                Some(ExprStaticFacts::DirectClass(None)) => return None,
                Some(ExprStaticFacts::DirectClass(Some(_))) | None => self
                    .normalized_place_class(carriers, place)
                    .or_else(|| match expr_facts {
                        Some(ExprStaticFacts::DirectClass(class)) => class.clone(),
                        _ => None,
                    })?,
                Some(
                    ExprStaticFacts::Const(_)
                    | ExprStaticFacts::AggregateMake(_)
                    | ExprStaticFacts::Borrow { .. }
                    | ExprStaticFacts::Call(_),
                ) => panic!(
                    "unexpected staged runtime class facts for read-place expr: owner={:?}; expr={expr:?}",
                    self.body.owner.key(self.db),
                ),
            },
            NExpr::Borrow { place, .. } => self
                .normalized_place_address_class(carriers, place)
                .or_else(|| match expr_facts {
                    Some(ExprStaticFacts::Borrow { provider_fallback }) => {
                        provider_fallback.clone()
                    }
                    _ => None,
                })?,
            NExpr::Call {
                args, effect_args, ..
            } => {
                let Some(ExprStaticFacts::Call(facts)) = expr_facts else {
                    panic!(
                        "missing staged runtime call facts: owner={:?}; expr={expr:?}",
                        self.body.owner.key(self.db),
                    );
                };
                if let Some(class) = facts.builtin_return_class.clone() {
                    return class;
                }
                if let StaticRuntimeReturnDecision::Known(class) = &facts.return_decision {
                    return class.clone();
                }
                let input_plan = facts
                    .input_plan
                    .as_ref()
                    .expect("dynamic call return should keep a staged input plan");
                let param_classes: Vec<_> = RuntimeArgSelector::new(self, carriers, class_cache)
                    .selected_call_inputs(args, effect_args, input_plan)
                    .into_iter()
                    .map(|arg| arg.class)
                    .collect();
                return lookup_return_class(RuntimeInstanceKey::new(
                    self.db,
                    RuntimeInstanceSource::Semantic(facts.semantic),
                    param_classes,
                ));
            }
        })
    }
}

impl<'a, 'db> BodyEnv<'a, 'db> {
    pub(crate) fn normalized_place_class(
        self,
        carriers: &[RuntimeCarrier<'db>],
        place: &NSPlace<'db>,
    ) -> Option<RuntimeClass<'db>> {
        if let Some(class) = single_field_transport_projection_class(self, carriers, place) {
            return Some(class);
        }
        let mut current =
            normalized_place_root_class_in_context(self, place.root.clone(), carriers)?;
        let root_local = normalized_place_root_local(self.body, &place.root);
        for projection in place.path.iter() {
            current = match projection {
                Projection::Field(field) => project_field_class(
                    self.db,
                    current,
                    FieldIndex((*field).try_into().expect("field index fits")),
                ),
                Projection::Index(_) => project_index_class(self.db, current),
                Projection::Deref => {
                    if root_local.is_some_and(|local| {
                        self.body
                            .locals
                            .get(local.index())
                            .is_some_and(|local| local.ty.as_borrow(self.db).is_some())
                    }) {
                        current
                    } else {
                        current
                            .deref_target()
                            .or_else(|| {
                                pointer_pointee_class_for_local(self, root_local?, &current)
                            })
                            .expect("invalid deref projection class")
                    }
                }
                Projection::VariantField {
                    variant, field_idx, ..
                } => project_variant_field_place_class(
                    self.db,
                    current,
                    *variant,
                    FieldIndex((*field_idx).try_into().expect("field index fits")),
                ),
                Projection::Discriminant => match current {
                    RuntimeClass::Ref { pointee, .. } => match pointee.aggregate_layout() {
                        Some(layout) => match layout.data(self.db) {
                            Layout::Enum(layout) => RuntimeClass::Scalar(layout.tag),
                            Layout::Struct(_) | Layout::Array(_) => {
                                panic!("invalid discriminant projection class")
                            }
                        },
                        None => panic!("invalid discriminant projection class"),
                    },
                    RuntimeClass::AggregateValue { layout }
                    | RuntimeClass::RawAddr {
                        target: Some(layout),
                        ..
                    } => match layout.data(self.db) {
                        Layout::Enum(layout) => RuntimeClass::Scalar(layout.tag),
                        Layout::Struct(_) | Layout::Array(_) => {
                            panic!("invalid discriminant projection class")
                        }
                    },
                    RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { target: None, .. } => {
                        panic!("invalid discriminant projection class")
                    }
                },
            };
        }
        Some(current)
    }

    pub(crate) fn normalized_place_address_class(
        self,
        carriers: &[RuntimeCarrier<'db>],
        place: &NSPlace<'db>,
    ) -> Option<RuntimeClass<'db>> {
        let value_class = self.normalized_place_class(carriers, place)?;
        let root_class =
            normalized_place_root_transport_class_in_context(self, place.root.clone(), carriers)?;
        let (root_space, force_raw) = match place.root {
            NSPlaceRoot::CarrierDerefLocal(_) => (AddressSpaceKind::Memory, false),
            NSPlaceRoot::Root(root) => match self.body.root(root)? {
                NBorrowRoot::Param { .. } | NBorrowRoot::LocalSlot { .. } => {
                    (AddressSpaceKind::Memory, false)
                }
                NBorrowRoot::Provider { binding } => {
                    (provider_root_space(binding, &root_class), false)
                }
            },
        };
        Some(ref_class_for_place_result(
            &root_class,
            &value_class,
            root_space,
            force_raw,
        ))
    }

    pub(crate) fn specialize_boundary_for_source(
        self,
        carriers: &[RuntimeCarrier<'db>],
        local: SLocalId,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> RuntimeBoundarySpec<'db> {
        specialize_boundary_for_runtime_source_in_context(
            self,
            local,
            BoundaryRef::unstaged(boundary),
            carriers,
            None,
        )
        .boundary
        .into_owned()
    }

    pub(crate) fn actual_aggregate_class_for_source(
        self,
        carriers: &[RuntimeCarrier<'db>],
        local: SLocalId,
    ) -> Option<RuntimeClass<'db>> {
        snapshot_source_place(self.body, local)
            .and_then(|place| self.normalized_place_class(carriers, place))
            .and_then(|class| actual_aggregate_class_from_runtime_source(&class))
            .or_else(|| {
                self.semantic_value_class(carriers, local)
                    .and_then(|class| actual_aggregate_class_from_runtime_source(&class))
            })
    }

    pub(super) fn semantic_value_class(
        self,
        carriers: &[RuntimeCarrier<'db>],
        local: SLocalId,
    ) -> Option<RuntimeClass<'db>> {
        let local_data = self.body.locals.get(local.index())?;
        let local_facts = self.local_facts(local)?;
        match local_data.facts.interface {
            SemanticLocalKind::Erased => None,
            SemanticLocalKind::DirectValue | SemanticLocalKind::DirectCarrier => {
                carrier_value_class(local, carriers)
            }
            SemanticLocalKind::PlaceCarrier => carrier_value_class(local, carriers)
                .or_else(|| local_facts.semantic_fallback_class.clone()),
            SemanticLocalKind::PlaceBoundValue => self
                .normalized_place_class(
                    carriers,
                    self.body.locals.get(local.index())?.backing_place()?,
                )
                .or_else(|| local_facts.semantic_fallback_class.clone()),
        }
    }
}

fn normalized_place_root_local(
    body: &NormalizedSemanticBody,
    root: &NSPlaceRoot,
) -> Option<SLocalId> {
    match root {
        NSPlaceRoot::CarrierDerefLocal(local) => Some(*local),
        NSPlaceRoot::Root(root) => match body.root(*root)? {
            NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local } => Some(*local),
            NBorrowRoot::Provider { .. } => None,
        },
    }
}

fn pointer_pointee_class_for_local<'db>(
    env: BodyEnv<'_, 'db>,
    local: SLocalId,
    ptr_class: &RuntimeClass<'db>,
) -> Option<RuntimeClass<'db>> {
    let space = match ptr_class {
        RuntimeClass::RawAddr { space, .. } => *space,
        RuntimeClass::Scalar(_)
        | RuntimeClass::AggregateValue { .. }
        | RuntimeClass::Ref { .. } => AddressSpaceKind::Memory,
    };
    let local = env.body.locals.get(local.index())?;
    let scope = env.scope()?;
    let pointee = normalize_ty(env.db, local.ty, scope, env.assumptions()).as_ptr(env.db)?;
    top_level_class_for_ty_in_context(env.db, pointee, space, Some(scope), env.assumptions())
}

fn single_field_transport_projection_class<'db>(
    env: BodyEnv<'_, 'db>,
    carriers: &[RuntimeCarrier<'db>],
    place: &NSPlace<'db>,
) -> Option<RuntimeClass<'db>> {
    let mut path = place.path.iter();
    let Some(Projection::Field(field_idx)) = path.next() else {
        return None;
    };
    if *field_idx != 0 || path.next().is_some() {
        return None;
    }
    let root_local = match place.root {
        NSPlaceRoot::CarrierDerefLocal(local) => local,
        NSPlaceRoot::Root(root) => match env.body.root(root)? {
            NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local } => *local,
            NBorrowRoot::Provider { .. } => return None,
        },
    };
    let local = env.body.locals.get(root_local.index())?;
    effect_handle_class_for_ty_in_context(env.db, local.ty, env.scope(), env.assumptions())?;
    let root_class =
        normalized_place_root_transport_class_in_context(env, place.root.clone(), carriers)?;
    if !root_class.is_transport() {
        return None;
    }
    let field_ty = runtime_repr_ty_in_context(env.db, local.ty, env.scope(), env.assumptions())
        .field_types(env.db)
        .get(*field_idx)
        .copied()?;
    top_level_class_for_ty_in_context(
        env.db,
        field_ty,
        AddressSpaceKind::Memory,
        env.scope(),
        env.assumptions(),
    )
}

fn root_provider_for_runtime_visible_binding<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
) -> Option<ProviderBinding<'db>> {
    match semantic.binding_role(db, binding) {
        SemanticLocalRole::DirectValue {
            provenance: ValueProvenance::RootProvider(provider),
        }
        | SemanticLocalRole::DirectCarrier {
            provider: Some(provider),
            ..
        }
        | SemanticLocalRole::PlaceCarrier {
            provider: Some(provider),
            ..
        }
        | SemanticLocalRole::PlaceBoundValue {
            provenance: hir::analysis::semantic::PlaceProvenance::RootProvider(provider),
            ..
        } => Some(provider),
        SemanticLocalRole::Erased
        | SemanticLocalRole::DirectValue { .. }
        | SemanticLocalRole::DirectCarrier { provider: None, .. }
        | SemanticLocalRole::PlaceCarrier { provider: None, .. }
        | SemanticLocalRole::PlaceBoundValue {
            provenance: hir::analysis::semantic::PlaceProvenance::Derived(_),
            ..
        } => None,
    }
}

fn build_runtime_visible_root_provider_locals<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> FxHashMap<ProviderBinding<'db>, SLocalId> {
    let mut locals = FxHashMap::default();
    for entry in runtime_visible_binding_plans(db, semantic) {
        let Some(provider) = root_provider_for_runtime_visible_binding(db, semantic, entry.binding)
        else {
            continue;
        };
        locals.entry(provider).or_insert(entry.local);
    }
    locals
}

fn build_local_static_facts<'db>(
    db: &'db dyn MirDb,
    type_env: RuntimeTypeEnv<'db>,
    local: SLocalId,
    local_data: &hir::analysis::semantic::borrowck::NSLocal<'db>,
) -> LocalStaticFacts<'db> {
    let scope = type_env.scope;
    let assumptions = type_env.assumptions;
    let lowered_ty = lowered_place_like_ty(local, local_data);
    let semantic_fallback_class = match local_data.facts.interface {
        SemanticLocalKind::PlaceCarrier | SemanticLocalKind::PlaceBoundValue => {
            lowered_ty.map(|ty| stored_class_for_ty_in_context(db, ty, scope, assumptions))
        }
        SemanticLocalKind::Erased
        | SemanticLocalKind::DirectValue
        | SemanticLocalKind::DirectCarrier => None,
    };
    let root_place_fallback_class = match local_data.facts.interface {
        SemanticLocalKind::Erased => None,
        SemanticLocalKind::DirectValue => Some(stored_class_for_ty_in_context(
            db,
            local_data.ty,
            scope,
            assumptions,
        )),
        SemanticLocalKind::PlaceCarrier
        | SemanticLocalKind::DirectCarrier
        | SemanticLocalKind::PlaceBoundValue => {
            lowered_ty.map(|ty| stored_class_for_ty_in_context(db, ty, scope, assumptions))
        }
    };
    LocalStaticFacts {
        boundary_source_transport_sensitive: boundary_source_uses_transport_sensitive_aggregate(
            db,
            local_data.ty,
            scope,
            assumptions,
        ),
        semantic_fallback_class,
        root_place_fallback_class,
        root_transport_fallback_class: fallback_root_transport_class(
            db,
            local_data,
            scope,
            assumptions,
        ),
        materialization_plan: if matches!(local_data.facts.interface, SemanticLocalKind::Erased) {
            CompiledMaterializationPlan::Erased
        } else {
            match local_data.facts.interface {
                SemanticLocalKind::DirectValue
                    if runtime_repr_ty_in_context(db, local_data.ty, scope, assumptions)
                        .as_ptr(db)
                        .is_some() =>
                {
                    CompiledMaterializationPlan::SemanticValue
                }
                SemanticLocalKind::DirectValue => top_level_class_for_ty_in_context(
                    db,
                    local_data.ty,
                    AddressSpaceKind::Memory,
                    scope,
                    assumptions,
                )
                .map_or(
                    CompiledMaterializationPlan::AggregateFromSource,
                    CompiledMaterializationPlan::AggregateFromSourceOrFallback,
                ),
                SemanticLocalKind::PlaceCarrier
                | SemanticLocalKind::DirectCarrier
                | SemanticLocalKind::PlaceBoundValue => CompiledMaterializationPlan::SemanticValue,
                SemanticLocalKind::Erased => unreachable!(),
            }
        },
    }
}

fn lowered_place_like_ty<'db>(
    local: SLocalId,
    local_data: &hir::analysis::semantic::borrowck::NSLocal<'db>,
) -> Option<TyId<'db>> {
    match (&local_data.facts.interface, &local_data.lowering) {
        (
            SemanticLocalKind::PlaceCarrier | SemanticLocalKind::DirectCarrier,
            NormalizedBindingLowering::CarrierLocal { target_ty, .. },
        ) => Some(*target_ty),
        (
            SemanticLocalKind::PlaceBoundValue,
            NormalizedBindingLowering::PlaceBoundValue { value_ty, .. },
        ) => Some(*value_ty),
        (SemanticLocalKind::PlaceCarrier | SemanticLocalKind::DirectCarrier, _) => {
            panic!("carrier local missing carrier lowering: {local:?}")
        }
        (SemanticLocalKind::PlaceBoundValue, _) => {
            panic!("place-bound local missing place-bound lowering: {local:?}")
        }
        (SemanticLocalKind::Erased | SemanticLocalKind::DirectValue, _) => None,
    }
}

struct ExprStaticFactsBuilder<'a, 'db> {
    db: &'db dyn MirDb,
    body: &'a NormalizedSemanticBody<'db>,
    typed_body: &'a hir::analysis::ty::ty_check::TypedBody<'db>,
    type_env: RuntimeTypeEnv<'db>,
}

impl<'db> ExprStaticFactsBuilder<'_, 'db> {
    fn build(
        &self,
        expr: &NExpr<'db>,
        dst: SLocalId,
        result_ty: TyId<'db>,
        boundary_sites: &mut BoundarySiteAllocator,
    ) -> Option<ExprStaticFacts<'db>> {
        let db = self.db;
        let body = self.body;
        let typed_body = self.typed_body;
        let type_env = self.type_env;
        Some(match expr {
            NExpr::Use(_) | NExpr::CodeRegionRef { .. } => return None,
            NExpr::Const(const_) => ExprStaticFacts::Const(match const_ {
                SConst::Value(value) => runtime_const_value_class(
                    db,
                    type_env,
                    *value,
                    body.locals
                        .get(dst.index())
                        .is_none_or(|local| !local.facts.root_demand.disallows_const_ref_storage()),
                ),
                SConst::Ref(cref) => {
                    let value = reified_const_ref_value_for_ty(db, body.owner, *cref, result_ty);
                    runtime_const_value_class(
                        db,
                        type_env,
                        value,
                        body.locals.get(dst.index()).is_none_or(|local| {
                            !local.facts.root_demand.disallows_const_ref_storage()
                        }),
                    )
                }
            }),
            NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::Cast { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. } => ExprStaticFacts::DirectClass(
                scalar_class_for_ty_in_env(db, type_env, result_ty).map(RuntimeClass::Scalar),
            ),
            NExpr::ArrayRepeat { .. } => {
                panic!(
                    "array repeat with non-concrete length reached runtime class inference: {expr:?}"
                )
            }
            NExpr::GetEnumTag { .. } => return None,
            NExpr::AggregateMake { ty, fields } => {
                let direct_class =
                    top_level_class_for_ty_in_env(db, type_env, *ty, AddressSpaceKind::Memory)
                        .filter(|class| !matches!(class, RuntimeClass::AggregateValue { .. }));
                let field_tys = if ty.is_array(db) {
                    let (_, args) = ty.decompose_ty_app(db);
                    let elem_ty = args.first().copied().expect("array element type");
                    vec![elem_ty; fields.len()]
                } else {
                    ty.field_types(db)
                };
                if field_tys.len() != fields.len() {
                    return None;
                }
                let fields = field_tys
                    .into_iter()
                    .map(|field_ty| AggregateMakeFieldStaticFacts {
                        boundary: boundary_spec_for_ty_in_env(
                            db,
                            type_env,
                            field_ty,
                            AddressSpaceKind::Memory,
                        )
                        .map(|boundary| boundary_sites.stage(boundary)),
                        stored_class: stored_class_for_ty_in_context(
                            db,
                            field_ty,
                            type_env.scope,
                            type_env.assumptions,
                        ),
                    })
                    .collect();
                ExprStaticFacts::AggregateMake(AggregateMakeStaticFacts {
                    direct_class,
                    ctor: AggregateCtorKind::Aggregate(*ty),
                    fields,
                })
            }
            NExpr::EnumMake {
                enum_ty,
                variant,
                fields,
            } => {
                let enum_ = enum_ty
                    .as_enum(db)
                    .unwrap_or_else(|| panic!("enum construction reached non-enum type"));
                let args = enum_ty.generic_args(db);
                let enum_variant = enum_.variants(db).nth(variant.0 as usize)?;
                let field_tys = enum_variant
                    .field_tys(db)
                    .into_iter()
                    .map(|field| field.instantiate(db, args))
                    .collect::<Vec<_>>();
                if field_tys.len() != fields.len() {
                    return None;
                }
                let fields = field_tys
                    .into_iter()
                    .map(|field_ty| AggregateMakeFieldStaticFacts {
                        boundary: boundary_spec_for_ty_in_env(
                            db,
                            type_env,
                            field_ty,
                            AddressSpaceKind::Memory,
                        )
                        .map(|boundary| boundary_sites.stage(boundary)),
                        stored_class: stored_class_for_ty_in_context(
                            db,
                            field_ty,
                            type_env.scope,
                            type_env.assumptions,
                        ),
                    })
                    .collect();
                ExprStaticFacts::AggregateMake(AggregateMakeStaticFacts {
                    direct_class: None,
                    ctor: AggregateCtorKind::EnumVariant {
                        enum_ty: *enum_ty,
                        variant: *variant,
                    },
                    fields,
                })
            }
            NExpr::ReadPlace { .. } => ExprStaticFacts::DirectClass(top_level_class_for_ty_in_env(
                db,
                type_env,
                result_ty,
                AddressSpaceKind::Memory,
            )),
            NExpr::ExtractEnumField { .. } => return None,
            NExpr::Borrow {
                provider, place, ..
            } => {
                let provider_fallback = match place.root {
                    NSPlaceRoot::Root(root) => match body.root(root) {
                        Some(NBorrowRoot::Provider { binding })
                            if provider_erases_runtime_root(
                                db,
                                binding,
                                type_env.scope,
                                type_env.assumptions,
                            ) =>
                        {
                            None
                        }
                        _ => provider.map(|provider| RuntimeClass::RawAddr {
                            space: address_space_from_provider(provider),
                            target: None,
                        }),
                    },
                    NSPlaceRoot::CarrierDerefLocal(_) => {
                        provider.map(|provider| RuntimeClass::RawAddr {
                            space: address_space_from_provider(provider),
                            target: None,
                        })
                    }
                };
                ExprStaticFacts::Borrow { provider_fallback }
            }
            NExpr::IsEnumVariant { .. } => {
                ExprStaticFacts::DirectClass(Some(RuntimeClass::Scalar(ScalarClass {
                    repr: ScalarRepr::Bool,
                    role: ScalarRole::Plain,
                })))
            }
            NExpr::Call {
                callee,
                args,
                effect_args,
                ..
            } => {
                let caller_key = body.owner.key(db);
                let callee_key = resolve_runtime_call_key(
                    db, caller_key, typed_body, body, *callee, args,
                )
                .unwrap_or_else(|err| {
                    panic!(
                        "runtime call resolution failed during return-class inference for {:?}: {err}",
                        caller_key,
                    )
                });
                let semantic = get_or_build_semantic_instance(db, callee_key);
                let builtin_return_class = extern_builtin_return_class(db, semantic, result_ty);
                let return_decision = static_runtime_return_decision(db, semantic);
                let needs_input_plan = builtin_return_class.is_none()
                    && matches!(return_decision, StaticRuntimeReturnDecision::Dynamic);
                ExprStaticFacts::Call(CallStaticFacts {
                    semantic,
                    builtin_return_class,
                    return_decision,
                    input_plan: needs_input_plan.then(|| {
                        compile_call_input_plan_for_semantic(
                            db,
                            body,
                            semantic,
                            RuntimeTypeEnv::for_semantic(db, semantic),
                            effect_args,
                            boundary_sites,
                        )
                    }),
                })
            }
        })
    }
}

#[derive(Clone, Copy)]
pub(crate) struct RuntimeBodyCx<'a, 'carriers, 'db> {
    pub(crate) env: BodyEnv<'a, 'db>,
    pub(crate) carriers: &'carriers [RuntimeCarrier<'db>],
}

impl<'a, 'carriers, 'db> RuntimeBodyCx<'a, 'carriers, 'db> {
    pub(super) fn selected_materialized_value(
        self,
        local: SLocalId,
    ) -> Option<SelectedRuntimeArg<'db>> {
        RuntimeArgSelector::new(self.env, self.carriers, None).selected_materialized_value(local)
    }

    pub(crate) fn normalized_place_class(self, place: &NSPlace<'db>) -> Option<RuntimeClass<'db>> {
        self.env.normalized_place_class(self.carriers, place)
    }

    pub(crate) fn normalized_place_address_class(
        self,
        place: &NSPlace<'db>,
    ) -> Option<RuntimeClass<'db>> {
        self.env
            .normalized_place_address_class(self.carriers, place)
    }

    pub(crate) fn actual_aggregate_class_for_source(
        self,
        local: SLocalId,
    ) -> Option<RuntimeClass<'db>> {
        self.env
            .actual_aggregate_class_for_source(self.carriers, local)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub(crate) struct RuntimeVisibleBindingPlan<'db> {
    pub(crate) binding: LocalBinding<'db>,
    pub(crate) local: SLocalId,
    pub(crate) semantic_ty: TyId<'db>,
    pub(crate) plan: RuntimeParamPlan<'db>,
}

#[derive(Clone, Debug)]
pub(crate) struct RuntimeEffectBindingPlan<'db> {
    pub(crate) class: RuntimeClass<'db>,
    pub(crate) boundary: RuntimeBoundarySpec<'db>,
}

#[derive(Clone, Debug)]
pub(crate) enum RuntimeVisibleReturnPlan<'db> {
    Erased,
    Exact(RuntimeClass<'db>),
    Constrained(RuntimeBoundarySpec<'db>),
    PassActual,
}

pub(super) fn provider_root_space<'db>(
    binding: &ProviderBinding<'db>,
    root_class: &RuntimeClass<'db>,
) -> AddressSpaceKind {
    root_class
        .address_space()
        .unwrap_or_else(|| match binding.semantics.kind {
            ProviderKind::RootObject => AddressSpaceKind::Memory,
            ProviderKind::Handle | ProviderKind::RawAddress => address_space_from_provider(
                binding
                    .semantics
                    .address_space
                    .unwrap_or_else(|| panic!("provider binding missing resolved space")),
            ),
        })
}

pub(crate) fn ref_class_for_place_result<'db>(
    root_class: &RuntimeClass<'db>,
    value_class: &RuntimeClass<'db>,
    root_space: AddressSpaceKind,
    force_raw: bool,
) -> RuntimeClass<'db> {
    if !force_raw {
        match root_class {
            RuntimeClass::Ref { kind, .. } => {
                return RuntimeClass::Ref {
                    pointee: Box::new(value_class.clone()),
                    kind: kind.clone(),
                    view: RefView::Whole,
                };
            }
            RuntimeClass::AggregateValue { .. } => {
                return RuntimeClass::Ref {
                    pointee: Box::new(value_class.clone()),
                    kind: RefKind::Object,
                    view: RefView::Whole,
                };
            }
            RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { .. } => {}
        }
    }
    RuntimeClass::RawAddr {
        space: root_class.address_space().unwrap_or(root_space),
        target: value_class.aggregate_layout(),
    }
}

fn provider_root_place_class<'db>(
    db: &'db dyn MirDb,
    value_ty: TyId<'db>,
    provider_class: &RuntimeClass<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> RuntimeClass<'db> {
    provider_class
        .deref_target()
        .unwrap_or_else(|| stored_class_for_ty_in_context(db, value_ty, scope, assumptions))
}

fn provider_root_value_ty<'db>(binding: &ProviderBinding<'db>) -> TyId<'db> {
    binding.semantics.target_ty.unwrap_or(binding.provider_ty)
}

pub(crate) fn runtime_class_for_provider_binding<'db>(
    db: &'db dyn MirDb,
    provider: &ProviderBinding<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeClass<'db>> {
    match provider.semantics.kind {
        ProviderKind::RootObject => top_level_class_for_ty_in_context(
            db,
            provider.provider_ty,
            AddressSpaceKind::Memory,
            scope,
            assumptions,
        ),
        ProviderKind::Handle | ProviderKind::RawAddress => {
            effect_handle_class_for_ty_in_context(db, provider.provider_ty, scope, assumptions)
                .or_else(|| {
                    Some(provider_class_for_target_in_context(
                        db,
                        provider.semantics.target_ty,
                        provider_address_space_to_runtime(provider.semantics.address_space?),
                        scope,
                        assumptions,
                    ))
                })
        }
    }
}

pub(crate) fn runtime_class_for_effect_binding_provider_in_context<'db>(
    db: &'db dyn MirDb,
    provider: &ProviderBinding<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeClass<'db>> {
    match provider.semantics.kind {
        ProviderKind::RootObject => Some(provider_class_for_target_in_context(
            db,
            Some(provider.semantics.target_ty.unwrap_or(provider.provider_ty)),
            provider
                .semantics
                .address_space
                .map_or(AddressSpaceKind::Memory, provider_address_space_to_runtime),
            scope,
            assumptions,
        )),
        ProviderKind::Handle | ProviderKind::RawAddress => {
            runtime_class_for_provider_binding(db, provider, scope, assumptions)
        }
    }
}

pub(crate) fn runtime_class_for_direct_value_provider_in_context<'db>(
    db: &'db dyn MirDb,
    provider: &ProviderBinding<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeClass<'db>> {
    runtime_class_for_effect_binding_provider_in_context(db, provider, scope, assumptions)
}

fn runtime_class_for_provider_value_ty_in_context<'db>(
    db: &'db dyn MirDb,
    provider: &ProviderBinding<'db>,
    value_ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeClass<'db>> {
    let space = match provider.semantics.kind {
        ProviderKind::RootObject => provider
            .semantics
            .address_space
            .map_or(AddressSpaceKind::Memory, provider_address_space_to_runtime),
        ProviderKind::Handle | ProviderKind::RawAddress => {
            provider_address_space_to_runtime(provider.semantics.address_space?)
        }
    };
    effect_handle_class_for_ty_in_context(db, provider.provider_ty, scope, assumptions).or_else(
        || {
            Some(provider_class_for_target_in_context(
                db,
                Some(value_ty),
                space,
                scope,
                assumptions,
            ))
        },
    )
}

fn effect_binding_borrow_boundary<'db>(
    db: &'db dyn MirDb,
    binding: LocalBinding<'db>,
    pointee_ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> RuntimeBoundarySpec<'db> {
    let access = if binding.is_mut() {
        BorrowAccess::ReadWrite
    } else {
        BorrowAccess::ReadOnly
    };
    RuntimeBoundarySpec::BorrowLike {
        pointee: stored_class_for_ty_in_context(db, pointee_ty, scope, assumptions),
        access,
        allow: default_borrow_transport_set(access, AddressSpaceKind::Memory),
    }
}

fn specialize_effect_binding_boundary_for_class<'db>(
    boundary: RuntimeBoundarySpec<'db>,
    class: &RuntimeClass<'db>,
) -> RuntimeBoundarySpec<'db> {
    specialize_boundary_for_aggregate_layout(&boundary, class.aggregate_layout()).into_owned()
}

fn exact_effect_binding_plan_for_class<'db>(
    class: RuntimeClass<'db>,
) -> RuntimeEffectBindingPlan<'db> {
    RuntimeEffectBindingPlan {
        class: class.clone(),
        boundary: RuntimeBoundarySpec::default_exact_boundary_for_class(class),
    }
}

pub(crate) fn runtime_zero_sized_effect_value_is_inert<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    let ty = runtime_repr_ty_in_context(db, ty, scope, assumptions);
    if let Some((_, inner)) = ty.as_borrow(db) {
        return runtime_zero_sized_effect_value_is_inert(db, inner, scope, assumptions);
    }
    if let Some((_, inner)) = ty.as_capability(db) {
        return runtime_zero_sized_effect_value_is_inert(db, inner, scope, assumptions);
    }
    if !runtime_zero_sized_ty(db, ty, scope, assumptions) {
        return false;
    }
    if ty
        .generic_args(db)
        .iter()
        .any(|arg| matches!(arg.data(db), TyData::ConstTy(_)))
    {
        return false;
    }
    if ty.is_tuple(db) || ty.is_struct(db) {
        return ty
            .field_types(db)
            .into_iter()
            .all(|field| runtime_zero_sized_effect_value_is_inert(db, field, scope, assumptions));
    }
    ty.is_never(db)
        || matches!(
            ty.base_ty(db).data(db),
            TyData::TyBase(hir::analysis::ty::ty_def::TyBase::Func(_))
        )
}

pub(crate) fn provider_source_erases_zero_sized_effect_value<'db>(
    db: &'db dyn MirDb,
    provider: &ProviderBinding<'db>,
    value_ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    match provider.source {
        ProviderSource::RootProvider { .. } | ProviderSource::UsesParam { .. } => {
            runtime_zero_sized_effect_value_is_inert(db, value_ty, scope, assumptions)
        }
        ProviderSource::ContractField { .. } => false,
    }
}

fn provider_source_erases_zero_sized_runtime_payload<'db>(
    db: &'db dyn MirDb,
    provider: &ProviderBinding<'db>,
    value_ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    if !runtime_zero_sized_ty(db, value_ty, scope, assumptions) {
        return false;
    }
    match provider.source {
        ProviderSource::RootProvider { .. } | ProviderSource::UsesParam { .. } => true,
        ProviderSource::ContractField { .. } => false,
    }
}

pub(crate) fn provider_erases_runtime_root<'db>(
    db: &'db dyn MirDb,
    provider: &ProviderBinding<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    provider_source_erases_zero_sized_effect_value(
        db,
        provider,
        provider.semantics.target_ty.unwrap_or(provider.provider_ty),
        scope,
        assumptions,
    )
}

pub(crate) fn runtime_effect_binding_plan<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
) -> Option<RuntimeEffectBindingPlan<'db>> {
    if !matches!(binding, LocalBinding::EffectParam { .. }) {
        return None;
    }
    let env = RuntimeTypeEnv::for_semantic(db, semantic);
    let binding_ty = semantic.binding_ty(db, binding);
    match semantic.binding_role(db, binding) {
        SemanticLocalRole::Erased => None,
        SemanticLocalRole::DirectValue {
            provenance: ValueProvenance::RootProvider(provider),
        } => {
            let value_ty = provider.semantics.target_ty.unwrap_or(binding_ty);
            let class = runtime_class_for_provider_value_ty_in_context(
                db,
                &provider,
                value_ty,
                env.scope,
                env.assumptions,
            )?;
            if provider_source_erases_zero_sized_runtime_payload(
                db,
                &provider,
                value_ty,
                env.scope,
                env.assumptions,
            ) || class.span_words(db) == 0
            {
                return None;
            }
            let boundary =
                effect_binding_borrow_boundary(db, binding, value_ty, env.scope, env.assumptions);
            let boundary = specialize_effect_binding_boundary_for_class(boundary, &class);
            Some(RuntimeEffectBindingPlan { class, boundary })
        }
        SemanticLocalRole::DirectValue { .. } => {
            let class =
                runtime_class_for_explicit_root_provider_param(db, env, binding, binding_ty)
                    .or_else(|| {
                        top_level_class_for_ty_in_env(db, env, binding_ty, AddressSpaceKind::Memory)
                    })?;
            Some(RuntimeEffectBindingPlan {
                class: class.clone(),
                boundary: RuntimeBoundarySpec::default_exact_boundary_for_class(class),
            })
        }
        SemanticLocalRole::DirectCarrier {
            provider: Some(provider),
            target_ty,
        } => {
            let binding_handle_class =
                effect_handle_class_for_ty_in_context(db, binding_ty, env.scope, env.assumptions);
            if binding_handle_class.is_none()
                && runtime_zero_sized_ty(db, target_ty, env.scope, env.assumptions)
            {
                return None;
            }
            let class = match binding_handle_class {
                Some(class) => class,
                None => {
                    runtime_class_for_provider_binding(db, &provider, env.scope, env.assumptions)?
                }
            };
            if class.span_words(db) == 0 {
                return None;
            }
            Some(exact_effect_binding_plan_for_class(class))
        }
        SemanticLocalRole::DirectCarrier {
            provider: None,
            target_ty,
        } => {
            let binding_handle_class =
                effect_handle_class_for_ty_in_context(db, binding_ty, env.scope, env.assumptions);
            if binding_handle_class.is_none()
                && runtime_zero_sized_ty(db, target_ty, env.scope, env.assumptions)
            {
                return None;
            }
            let class = binding_handle_class
                .or_else(|| {
                    top_level_class_for_ty_in_env(db, env, binding_ty, AddressSpaceKind::Memory)
                })
                .or_else(|| {
                    Some(provider_class_for_target_in_env(
                        db,
                        env,
                        Some(target_ty),
                        AddressSpaceKind::Memory,
                    ))
                })?;
            if class.span_words(db) == 0 {
                return None;
            }
            Some(exact_effect_binding_plan_for_class(class))
        }
        SemanticLocalRole::PlaceCarrier {
            provider: Some(provider),
            value_ty,
        } => {
            if provider_source_erases_zero_sized_runtime_payload(
                db,
                &provider,
                value_ty,
                env.scope,
                env.assumptions,
            ) {
                return None;
            }
            let class = runtime_class_for_provider_value_ty_in_context(
                db,
                &provider,
                value_ty,
                env.scope,
                env.assumptions,
            )?;
            if class.span_words(db) == 0 {
                return None;
            }
            let boundary =
                effect_binding_borrow_boundary(db, binding, value_ty, env.scope, env.assumptions);
            let boundary = specialize_effect_binding_boundary_for_class(boundary, &class);
            Some(RuntimeEffectBindingPlan { class, boundary })
        }
        SemanticLocalRole::PlaceCarrier {
            provider: None,
            value_ty,
        } => {
            if runtime_zero_sized_ty(db, value_ty, env.scope, env.assumptions) {
                return None;
            }
            let class =
                provider_class_for_target_in_env(db, env, Some(value_ty), AddressSpaceKind::Memory);
            Some(RuntimeEffectBindingPlan {
                class: class.clone(),
                boundary: RuntimeBoundarySpec::default_exact_boundary_for_class(class),
            })
        }
        SemanticLocalRole::PlaceBoundValue {
            provenance: hir::analysis::semantic::PlaceProvenance::RootProvider(provider),
            value_ty,
        } => {
            if provider_source_erases_zero_sized_runtime_payload(
                db,
                &provider,
                value_ty,
                env.scope,
                env.assumptions,
            ) {
                return None;
            }
            let class = runtime_class_for_provider_value_ty_in_context(
                db,
                &provider,
                value_ty,
                env.scope,
                env.assumptions,
            )?;
            if class.span_words(db) == 0 {
                return None;
            }
            let boundary =
                effect_binding_borrow_boundary(db, binding, value_ty, env.scope, env.assumptions);
            let boundary = specialize_effect_binding_boundary_for_class(boundary, &class);
            Some(RuntimeEffectBindingPlan { class, boundary })
        }
        SemanticLocalRole::PlaceBoundValue {
            provenance: hir::analysis::semantic::PlaceProvenance::Derived(_),
            ..
        } => None,
    }
}

fn runtime_exact_class_for_ordinary_binding_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    binding: LocalBinding<'db>,
    binding_ty: TyId<'db>,
) -> Option<RuntimeClass<'db>> {
    runtime_class_for_explicit_root_provider_param(db, env, binding, binding_ty).or_else(|| {
        match boundary_spec_for_ty_in_env(db, env, binding_ty, AddressSpaceKind::Memory) {
            Some(RuntimeBoundarySpec::ExactTransport(class))
            | Some(RuntimeBoundarySpec::ExactShape(class)) => Some(class),
            Some(RuntimeBoundarySpec::BorrowLike { .. }) | None => None,
        }
    })
}

fn runtime_exact_class_for_visible_binding_in_env<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    env: RuntimeTypeEnv<'db>,
    binding: LocalBinding<'db>,
    binding_ty: TyId<'db>,
) -> Option<RuntimeClass<'db>> {
    match semantic.binding_role(db, binding) {
        SemanticLocalRole::Erased => None,
        SemanticLocalRole::DirectValue {
            provenance: ValueProvenance::RootProvider(provider),
        } => runtime_class_for_provider_value_ty_in_context(
            db,
            &provider,
            binding_ty,
            env.scope,
            env.assumptions,
        ),
        SemanticLocalRole::DirectValue { .. } => runtime_exact_class_for_ordinary_binding_in_env(
            db, env, binding, binding_ty,
        )
        .or_else(|| top_level_class_for_ty_in_env(db, env, binding_ty, AddressSpaceKind::Memory)),
        SemanticLocalRole::DirectCarrier {
            provider: Some(provider),
            target_ty,
        } => {
            let binding_handle_class =
                effect_handle_class_for_ty_in_context(db, binding_ty, env.scope, env.assumptions);
            if binding_handle_class.is_none()
                && runtime_zero_sized_ty(db, target_ty, env.scope, env.assumptions)
            {
                return None;
            }
            binding_handle_class.or_else(|| {
                runtime_class_for_provider_binding(db, &provider, env.scope, env.assumptions)
            })
        }
        SemanticLocalRole::DirectCarrier {
            provider: None,
            target_ty,
        } => {
            let binding_handle_class =
                effect_handle_class_for_ty_in_context(db, binding_ty, env.scope, env.assumptions);
            if binding_handle_class.is_none()
                && runtime_zero_sized_ty(db, target_ty, env.scope, env.assumptions)
            {
                return None;
            }
            binding_handle_class
                .or_else(|| {
                    top_level_class_for_ty_in_env(db, env, binding_ty, AddressSpaceKind::Memory)
                })
                .or_else(|| {
                    Some(provider_class_for_target_in_env(
                        db,
                        env,
                        Some(target_ty),
                        AddressSpaceKind::Memory,
                    ))
                })
        }
        SemanticLocalRole::PlaceCarrier {
            provider: Some(provider),
            value_ty,
        } => runtime_class_for_provider_value_ty_in_context(
            db,
            &provider,
            value_ty,
            env.scope,
            env.assumptions,
        ),
        SemanticLocalRole::PlaceCarrier {
            provider: None,
            value_ty,
        } => Some(provider_class_for_target_in_env(
            db,
            env,
            Some(value_ty),
            AddressSpaceKind::Memory,
        )),
        SemanticLocalRole::PlaceBoundValue {
            provenance: hir::analysis::semantic::PlaceProvenance::RootProvider(provider),
            value_ty,
        } => runtime_class_for_provider_value_ty_in_context(
            db,
            &provider,
            value_ty,
            env.scope,
            env.assumptions,
        ),
        SemanticLocalRole::PlaceBoundValue {
            provenance: hir::analysis::semantic::PlaceProvenance::Derived(_),
            ..
        } => None,
    }
}

pub(crate) fn runtime_effect_binding_plan_for_binding_idx<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    binding_idx: u32,
) -> Option<RuntimeEffectBindingPlan<'db>> {
    let BodyOwner::Func(func) = semantic.key(db).owner(db) else {
        return None;
    };
    let resolved = hir::semantic::EffectEnvView::new(EffectParamSite::Func(func))
        .resolved_binding(db, binding_idx as usize)?;
    runtime_effect_binding_plan(
        db,
        semantic,
        LocalBinding::EffectParam {
            site: resolved.requirement.binding_site,
            idx: resolved.requirement.binding_idx as usize,
            binding_name: resolved.requirement.binding_name,
            provider_idx: resolved.provider.provider_idx,
            key_path: resolved.requirement.binding_path,
            is_mut: resolved.requirement.is_mut,
        },
    )
}

pub(crate) fn runtime_visible_binding_class<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
) -> Option<RuntimeClass<'db>> {
    if matches!(binding, LocalBinding::EffectParam { .. }) {
        return runtime_effect_binding_plan(db, semantic, binding).map(|plan| plan.class);
    }
    if let Some(plan) = runtime_effect_binding_plan(db, semantic, binding) {
        return Some(plan.class);
    }
    let env = RuntimeTypeEnv::for_semantic(db, semantic);
    let binding_ty = semantic.binding_ty(db, binding);
    runtime_exact_class_for_visible_binding_in_env(db, semantic, env, binding, binding_ty)
}

pub(crate) fn owner_effect_binding_boundary<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
) -> Option<RuntimeBoundarySpec<'db>> {
    runtime_effect_binding_plan(db, semantic, binding).map(|plan| plan.boundary)
}

fn runtime_class_for_explicit_root_provider_param<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    binding: LocalBinding<'db>,
    binding_ty: TyId<'db>,
) -> Option<RuntimeClass<'db>> {
    let (func, idx) = root_provider_func_param(binding)?;
    let param = func.params(db).nth(idx)?;
    let original_param_ty = *param.ty_binder(db).skip_binder();
    let has_contract_host_bound =
        explicit_root_provider_param_has_contract_host_bound(db, func, original_param_ty);
    if !param.is_self_param(db) && !has_contract_host_bound {
        return None;
    }
    if has_contract_host_bound {
        return Some(provider_class_for_target_in_env(
            db,
            env,
            Some(root_provider_param_target_ty(db, binding_ty)),
            AddressSpaceKind::Memory,
        ));
    }
    runtime_class_for_root_provider_param(db, env, binding, binding_ty)
}

fn runtime_class_for_root_provider_param<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    binding: LocalBinding<'db>,
    binding_ty: TyId<'db>,
) -> Option<RuntimeClass<'db>> {
    let (func, _) = root_provider_func_param(binding)?;
    let canonical = |ty| {
        strip_derived_adt_layout_args(
            db,
            runtime_repr_ty_in_context(db, ty, env.scope, env.assumptions),
        )
    };
    let binding_ty = canonical(binding_ty);
    let binding_ty = binding_ty
        .as_capability(db)
        .map_or(binding_ty, |(_, inner)| canonical(inner));
    registered_root_providers(db, EffectParamSite::Func(func))
        .iter()
        .find(|provider| canonical(provider.provider_ty) == binding_ty)
        .map(|provider| {
            provider_class_for_target_in_env(
                db,
                env,
                Some(provider.provider_ty),
                AddressSpaceKind::Memory,
            )
        })
}

fn root_provider_func_param<'db>(
    binding: LocalBinding<'db>,
) -> Option<(hir::hir_def::Func<'db>, usize)> {
    let LocalBinding::Param {
        site: ParamSite::Func(func),
        idx,
        ..
    } = binding
    else {
        return None;
    };
    Some((func, idx))
}

fn explicit_root_provider_param_has_contract_host_bound<'db>(
    db: &'db dyn MirDb,
    func: hir::hir_def::Func<'db>,
    param_ty: TyId<'db>,
) -> bool {
    let param_ty = if let Some((_, inner)) = param_ty.as_capability(db) {
        inner
    } else if let Some((_, inner)) = param_ty.as_borrow(db) {
        inner
    } else {
        param_ty
    };
    matches!(param_ty.data(db), TyData::TyParam(_))
        && constraints_for(db, ItemKind::Func(func))
            .list(db)
            .iter()
            .any(|inst| {
                inst.args(db).first().is_some_and(|arg| *arg == param_ty)
                    && inst
                        .def(db)
                        .scope()
                        .pretty_path(db)
                        .is_some_and(|path| path.ends_with("contracts::ContractHost"))
            })
}

fn root_provider_param_target_ty<'db>(db: &'db dyn MirDb, ty: TyId<'db>) -> TyId<'db> {
    if let Some((_, inner)) = ty.as_capability(db) {
        inner
    } else if let Some((_, inner)) = ty.as_borrow(db) {
        inner
    } else {
        ty
    }
}

fn aggregate_make_class_from_facts<'db>(
    env: BodyEnv<'_, 'db>,
    facts: &AggregateMakeStaticFacts<'db>,
    fields: &[NOperand],
    carriers: &[RuntimeCarrier<'db>],
    class_cache: Option<&mut InferClassCache<'db>>,
) -> Option<RuntimeClass<'db>> {
    if let Some(class) = facts.direct_class.clone() {
        return Some(class);
    }
    if facts.fields.len() != fields.len() {
        return None;
    }
    let mut field_classes = Vec::with_capacity(fields.len());
    let mut evaluator = RuntimeArgSelector::new(env, carriers, class_cache);
    for (field, field_facts) in fields.iter().copied().zip(facts.fields.iter()) {
        let selected = if let Some(boundary) = field_facts.boundary.as_ref() {
            let mut boundary_sites = BoundarySiteAllocator::default();
            evaluator.selected_value_pass_plan(
                field,
                &compile_value_pass_plan(
                    RuntimeParamPlan::Boundary(boundary.boundary.clone()),
                    &mut boundary_sites,
                ),
            )
        } else {
            evaluator.selected_materialized_value(field.local)
        };
        let class = selected
            .map(|arg| arg.class)
            .unwrap_or_else(|| field_facts.stored_class.clone());
        field_classes.push(class);
    }
    Some(RuntimeClass::AggregateValue {
        layout: match facts.ctor {
            AggregateCtorKind::Aggregate(ty) => layout_for_aggregate_instance_in_context(
                env.db,
                ty,
                &field_classes,
                env.scope(),
                env.assumptions(),
            ),
            AggregateCtorKind::EnumVariant { enum_ty, variant } => {
                layout_for_enum_variant_instance_in_context(
                    env.db,
                    enum_ty,
                    variant.0 as usize,
                    &field_classes,
                    env.scope(),
                    env.assumptions(),
                )
            }
        },
    })
}

pub(super) fn selected_visible_return_for_local<'db>(
    env: BodyEnv<'_, 'db>,
    local: SLocalId,
    plan: &RuntimeVisibleReturnPlan<'db>,
    carriers: &[RuntimeCarrier<'db>],
) -> Option<SelectedRuntimeArg<'db>> {
    let mut evaluator = RuntimeArgSelector::new(env, carriers, None);
    match plan {
        RuntimeVisibleReturnPlan::Erased => None,
        RuntimeVisibleReturnPlan::Exact(class) => evaluator
            .selected_materialized_value(local)
            .or_else(|| Some(selected_semantic_copy(local, class.clone()))),
        RuntimeVisibleReturnPlan::Constrained(boundary) => {
            let mut boundary_sites = BoundarySiteAllocator::default();
            evaluator.selected_value_for_local(
                local,
                &compile_value_pass_plan(
                    RuntimeParamPlan::Boundary(boundary.clone()),
                    &mut boundary_sites,
                ),
            )
        }
        RuntimeVisibleReturnPlan::PassActual => evaluator.selected_actual_value(local),
    }
}

fn selected_semantic_copy<'db>(
    local: SLocalId,
    class: RuntimeClass<'db>,
) -> SelectedRuntimeArg<'db> {
    SelectedRuntimeArg::semantic_operand(
        NOperand {
            local,
            origin: None,
            mode: ReadMode::Copy,
        },
        class,
    )
}

pub(super) fn nonself_backing_value_place<'a, 'db>(
    body: &'a NormalizedSemanticBody<'db>,
    local: SLocalId,
) -> Option<&'a NSPlace<'db>> {
    let place = body.local(local)?.backing_place()?;
    (!is_self_rooted_value_place(body, local, place)).then_some(place)
}

fn is_self_rooted_value_place<'db>(
    body: &NormalizedSemanticBody<'db>,
    local: SLocalId,
    place: &NSPlace<'db>,
) -> bool {
    if !place.path.is_empty() {
        return false;
    }
    match place.root {
        NSPlaceRoot::CarrierDerefLocal(root_local) => root_local == local,
        NSPlaceRoot::Root(root) => matches!(
            body.root(root),
            Some(NBorrowRoot::Param { local: root_local, .. } | NBorrowRoot::LocalSlot { local: root_local })
                if *root_local == local
        ),
    }
}

pub(super) fn snapshot_source_place<'a, 'db>(
    body: &'a NormalizedSemanticBody<'db>,
    local: SLocalId,
) -> Option<&'a NSPlace<'db>> {
    body.local(local)?.snapshot_source_place()
}

pub(crate) fn desired_runtime_param_plan<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    typed_body: &hir::analysis::ty::ty_check::TypedBody<'db>,
    idx: usize,
) -> RuntimeParamPlan<'db> {
    let Some(binding) = typed_body.param_binding(idx) else {
        return RuntimeParamPlan::Erased;
    };
    let binding_ty = typed_body.binding_ty(db, binding);
    let env = RuntimeTypeEnv::for_semantic(db, semantic);
    let scope = env.scope;
    let assumptions = env.assumptions;
    if let Some(class) =
        runtime_class_for_explicit_root_provider_param(db, env, binding, binding_ty)
    {
        return RuntimeParamPlan::Boundary(runtime_param_boundary(
            db,
            typed_body,
            binding,
            env,
            RuntimeBoundarySpec::default_exact_boundary_for_class(class),
        ));
    }
    if runtime_zero_sized_effect_value_is_inert(db, binding_ty, scope, assumptions) {
        return RuntimeParamPlan::Erased;
    }
    let interface_ty = runtime_interface_ty_in_context(db, binding_ty, scope, assumptions);
    let repr_ty = runtime_repr_ty_in_context(db, binding_ty, scope, assumptions);
    if runtime_abstract_param_ty(db, binding_ty, scope, assumptions)
        || matches!(
            repr_ty.base_ty(db).data(db),
            TyData::TyParam(param) if param.is_effect() || param.is_effect_provider()
        )
    {
        RuntimeParamPlan::PassActual
    } else if matches!(
        binding,
        LocalBinding::Param {
            mode: FuncParamMode::View,
            ..
        }
    ) && binding_ty.as_capability(db).is_none()
    {
        desired_read_only_view_param_plan(db, typed_body, binding, env, binding_ty)
    } else if let Some((CapabilityKind::View, inner)) = interface_ty.as_capability(db) {
        desired_read_only_view_param_plan(db, typed_body, binding, env, inner)
    } else if interface_ty.as_capability(db).is_some() {
        boundary_spec_for_ty_in_env(db, env, binding_ty, AddressSpaceKind::Memory)
            .map(|boundary| {
                RuntimeParamPlan::Boundary(runtime_param_boundary(
                    db, typed_body, binding, env, boundary,
                ))
            })
            .unwrap_or(RuntimeParamPlan::Erased)
    } else if let Some(class) =
        runtime_exact_class_for_visible_binding_in_env(db, semantic, env, binding, binding_ty)
        && (!matches!(class, RuntimeClass::AggregateValue { .. })
            || !aggregate_transport_depends_on_runtime_source(db, binding_ty, scope, assumptions))
    {
        RuntimeParamPlan::Boundary(runtime_param_boundary(
            db,
            typed_body,
            binding,
            env,
            RuntimeBoundarySpec::default_exact_boundary_for_class(class),
        ))
    } else {
        let Some(boundary) =
            boundary_spec_for_ty_in_env(db, env, binding_ty, AddressSpaceKind::Memory)
        else {
            return RuntimeParamPlan::Erased;
        };
        if matches!(
            boundary,
            RuntimeBoundarySpec::ExactTransport(RuntimeClass::AggregateValue { .. })
                | RuntimeBoundarySpec::ExactShape(RuntimeClass::AggregateValue { .. })
        ) && aggregate_transport_depends_on_runtime_source(db, binding_ty, scope, assumptions)
        {
            RuntimeParamPlan::PassActual
        } else {
            RuntimeParamPlan::Boundary(runtime_param_boundary(
                db, typed_body, binding, env, boundary,
            ))
        }
    }
}

fn desired_read_only_view_param_plan<'db>(
    db: &'db dyn MirDb,
    typed_body: &hir::analysis::ty::ty_check::TypedBody<'db>,
    binding: LocalBinding<'db>,
    env: RuntimeTypeEnv<'db>,
    inner: TyId<'db>,
) -> RuntimeParamPlan<'db> {
    let Some(boundary) = boundary_spec_for_ty_in_env(
        db,
        env,
        typed_body.binding_ty(db, binding),
        AddressSpaceKind::Memory,
    ) else {
        return RuntimeParamPlan::Erased;
    };
    if binding.is_mut() {
        return RuntimeParamPlan::Boundary(runtime_param_boundary(
            db, typed_body, binding, env, boundary,
        ));
    }
    let value = stored_class_for_ty_in_context(db, inner, env.scope, env.assumptions);
    if value.aggregate_layout().is_none() {
        return RuntimeParamPlan::Boundary(runtime_param_boundary(
            db, typed_body, binding, env, boundary,
        ));
    }
    let borrow = match boundary {
        RuntimeBoundarySpec::BorrowLike { .. } => boundary,
        _ => RuntimeBoundarySpec::BorrowLike {
            pointee: value.clone(),
            access: BorrowAccess::ReadOnly,
            allow: default_borrow_transport_set(BorrowAccess::ReadOnly, AddressSpaceKind::Memory),
        },
    };
    RuntimeParamPlan::ReadOnlyView {
        value: runtime_param_class(db, typed_body, binding, env, value),
        borrow,
    }
}

pub(crate) fn resolve_runtime_call_key<'db>(
    db: &'db dyn MirDb,
    caller_key: SemanticInstanceKey<'db>,
    caller_typed_body: &hir::analysis::ty::ty_check::TypedBody<'db>,
    body: &NormalizedSemanticBody<'db>,
    callee: SemanticCalleeRef<'db>,
    args: &[NOperand],
) -> Result<SemanticInstanceKey<'db>, crate::runtime::LowerError> {
    let callee_key = callee.key;
    let callee_semantic = get_or_build_semantic_instance(db, callee_key);
    if contract_metadata_builtin(db, callee_semantic).is_some() {
        return Ok(callee_key);
    }
    let BodyOwner::Func(func) = callee_key.owner(db) else {
        return Ok(callee_key);
    };
    let Some(trait_) = func.containing_trait(db) else {
        return Ok(callee_key);
    };
    if func.body(db).is_some() {
        return Ok(callee_key);
    }
    let Some(method_name) = func.name(db).to_opt() else {
        return Err(crate::runtime::LowerError::Unsupported(format!(
            "runtime trait-call resolution reached an unnamed declaration-only method: caller={caller_key:?} callee={callee_key:?}"
        )));
    };
    let impl_env = callee_key.impl_env(db);
    let original_inst: Option<TraitInstId<'db>> = impl_env
        .witnesses(db)
        .iter()
        .find(|inst| inst.def(db) == trait_)
        .copied();
    let concrete_inst = if func
        .params(db)
        .next()
        .is_some_and(|param| param.is_self_param(db))
    {
        let Some(arg) = args.first() else {
            return Err(crate::runtime::LowerError::Unsupported(format!(
                "runtime trait-call resolution is missing a self argument: caller={caller_key:?} callee={callee_key:?}"
            )));
        };
        let Some(self_ty) = concrete_runtime_self_ty_for_call_arg(
            db,
            RuntimeTypeEnv::for_semantic(db, body.owner),
            body,
            arg.local,
        ) else {
            return Err(crate::runtime::LowerError::Unsupported(format!(
                "runtime trait-call resolution could not infer the concrete self type: caller={caller_key:?} callee={callee_key:?} local={:?}",
                arg.local,
            )));
        };
        let mut inst_args = original_inst
            .map(|inst| inst.args(db).to_vec())
            .unwrap_or_else(|| vec![self_ty]);
        let Some(first) = inst_args.first_mut() else {
            return Err(crate::runtime::LowerError::Unsupported(format!(
                "runtime trait-call resolution produced an empty trait-inst arg list: caller={caller_key:?} callee={callee_key:?}"
            )));
        };
        *first = self_ty;
        TraitInstId::new(
            db,
            trait_,
            inst_args,
            original_inst
                .map(|inst| inst.assoc_type_bindings(db).clone())
                .unwrap_or_default(),
        )
    } else {
        let Some(original_inst) = original_inst else {
            return Err(crate::runtime::LowerError::Unsupported(format!(
                "runtime trait-call resolution is missing a trait witness for a declaration-only method: caller={caller_key:?} callee={callee_key:?}"
            )));
        };
        original_inst
    };
    let assumptions = runtime_callee_assumptions(db, caller_key, caller_typed_body);
    let Some((impl_func, mut impl_args)) = resolve_trait_method_instance(
        db,
        TraitSolveCx::new(db, caller_key.impl_env(db).normalization_scope(db))
            .with_assumptions(assumptions),
        concrete_inst,
        method_name,
    ) else {
        return Err(crate::runtime::LowerError::Unsupported(format!(
            "runtime trait-call resolution failed to resolve a concrete impl body: caller={caller_key:?} decl={callee_key:?} method={} concrete_inst={} original_inst={}",
            method_name.data(db),
            concrete_inst.pretty_print(db, false),
            original_inst
                .map(|inst| inst.pretty_print(db, false))
                .unwrap_or_else(|| "<none>".to_string()),
        )));
    };
    let trait_arg_len = concrete_inst.args(db).len();
    let tail = callee_key
        .subst(db)
        .generic_args(db)
        .get(trait_arg_len..)
        .unwrap_or(callee_key.subst(db).generic_args(db).as_slice());
    impl_args.extend_from_slice(tail);
    let mut witnesses = IndexSet::new();
    witnesses.extend(caller_key.impl_env(db).witnesses(db).iter().copied());
    witnesses.extend(impl_env.witnesses(db).iter().copied());
    witnesses.insert(concrete_inst);
    Ok(SemanticInstanceKey::new(
        db,
        BodyOwner::Func(impl_func),
        GenericSubst::new(db, impl_args),
        hir::analysis::semantic::EffectProviderSubst::empty(db),
        ImplEnv::new(
            db,
            caller_key.impl_env(db).normalization_scope(db),
            assumptions,
            witnesses.into_iter().collect::<Vec<_>>(),
        ),
    ))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum GenericNumericIntrinsicKind {
    Bitcast,
    Saturating(SaturatingBinOp),
    CheckedBinary(ArithBinOp),
    CheckedNeg,
}

fn runtime_callee_assumptions<'db>(
    db: &'db dyn MirDb,
    caller_key: SemanticInstanceKey<'db>,
    caller_typed_body: &hir::analysis::ty::ty_check::TypedBody<'db>,
) -> PredicateListId<'db> {
    let impl_env = caller_key.impl_env(db);
    let mut predicates: IndexSet<_> = caller_typed_body
        .assumptions()
        .list(db)
        .iter()
        .copied()
        .collect();
    predicates.extend(impl_env.assumptions(db).list(db).iter().copied());
    predicates.extend(impl_env.witnesses(db).iter().copied());
    PredicateListId::new(db, predicates.into_iter().collect::<Vec<_>>())
}

fn concrete_runtime_self_ty_for_call_arg<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    body: &NormalizedSemanticBody<'db>,
    local: SLocalId,
) -> Option<TyId<'db>> {
    let scope = env.scope;
    let assumptions = env.assumptions;
    let normalized = |ty| normalize_runtime_self_ty(db, ty, scope, assumptions);
    let local_data = body.locals.get(local.index())?;
    match (
        &local_data.facts.interface,
        &local_data.facts.origin,
        &local_data.lowering,
    ) {
        (SemanticLocalKind::Erased, _, _) => None,
        (
            SemanticLocalKind::DirectValue | SemanticLocalKind::DirectCarrier,
            NLocalOrigin::RootProvider(provider),
            _,
        ) => Some(normalized(provider.provider_ty)),
        (
            SemanticLocalKind::PlaceBoundValue,
            NLocalOrigin::RootProvider(provider),
            NormalizedBindingLowering::PlaceBoundValue { value_ty, .. },
        ) => Some(normalized(
            provider.semantics.target_ty.unwrap_or(*value_ty),
        )),
        (
            SemanticLocalKind::PlaceBoundValue,
            NLocalOrigin::SelfRooted | NLocalOrigin::AliasedPlace,
            NormalizedBindingLowering::PlaceBoundValue { value_ty, .. },
        ) => Some(normalized(*value_ty)),
        (SemanticLocalKind::DirectValue, _, _) => Some(normalized(local_data.ty)),
        (
            SemanticLocalKind::PlaceCarrier | SemanticLocalKind::DirectCarrier,
            _,
            NormalizedBindingLowering::CarrierLocal { target_ty, .. },
        ) => Some(normalized(*target_ty)),
        _ => None,
    }
}

fn normalize_runtime_self_ty<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let ty = runtime_repr_ty_in_context(db, ty, scope, assumptions);
    if let Some((_, inner)) = ty.as_borrow(db) {
        return scope.map_or(inner, |scope| normalize_ty(db, inner, scope, assumptions));
    }
    scope.map_or(ty, |scope| normalize_ty(db, ty, scope, assumptions))
}

pub(super) fn carrier_value_class<'db>(
    local: SLocalId,
    carriers: &[RuntimeCarrier<'db>],
) -> Option<RuntimeClass<'db>> {
    carrier_value_class_ref(local, carriers).cloned()
}

pub(super) fn carrier_value_class_ref<'a, 'db>(
    local: SLocalId,
    carriers: &'a [RuntimeCarrier<'db>],
) -> Option<&'a RuntimeClass<'db>> {
    match carriers.get(local.index())? {
        RuntimeCarrier::Erased => None,
        RuntimeCarrier::Value(class) => Some(class),
    }
}

fn normalized_place_root_transport_class_in_context<'db>(
    env: BodyEnv<'_, 'db>,
    root: NSPlaceRoot,
    carriers: &[RuntimeCarrier<'db>],
) -> Option<RuntimeClass<'db>> {
    match root {
        NSPlaceRoot::CarrierDerefLocal(local) => {
            carrier_value_class(local, carriers).or_else(|| {
                env.local_facts(local)?
                    .root_transport_fallback_class
                    .clone()
            })
        }
        NSPlaceRoot::Root(root) => match env.body.root(root)? {
            NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local } => {
                carrier_value_class(*local, carriers).or_else(|| {
                    env.local_facts(*local)?
                        .root_transport_fallback_class
                        .clone()
                })
            }
            NBorrowRoot::Provider { binding } => {
                if provider_erases_runtime_root(env.db, binding, env.scope(), env.assumptions()) {
                    return None;
                }
                env.actual_runtime_visible_root_provider_class(carriers, binding)
                    .map(|(_, class)| class)
                    .or_else(|| {
                        runtime_class_for_effect_binding_provider_in_context(
                            env.db,
                            binding,
                            env.scope(),
                            env.assumptions(),
                        )
                        .or_else(|| {
                            runtime_class_for_direct_value_provider_in_context(
                                env.db,
                                binding,
                                env.scope(),
                                env.assumptions(),
                            )
                        })
                    })
            }
        },
    }
}

fn normalized_place_root_class_in_context<'db>(
    env: BodyEnv<'_, 'db>,
    root: NSPlaceRoot,
    carriers: &[RuntimeCarrier<'db>],
) -> Option<RuntimeClass<'db>> {
    let cx = env.with_carriers(carriers);
    match root {
        NSPlaceRoot::CarrierDerefLocal(local) => {
            let local_data = env.body.locals.get(local.index())?;
            local_place_root_class(cx, local, local_data, carriers.get(local.index())?)
        }
        NSPlaceRoot::Root(root) => match env.body.root(root)? {
            NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local } => {
                local_place_root_class(
                    cx,
                    *local,
                    env.body.locals.get(local.index())?,
                    carriers.get(local.index())?,
                )
            }
            NBorrowRoot::Provider { binding } => {
                if provider_erases_runtime_root(env.db, binding, env.scope(), env.assumptions()) {
                    return None;
                }
                let provider_class = env
                    .actual_runtime_visible_root_provider_class(carriers, binding)
                    .map(|(_, class)| class)
                    .or_else(|| {
                        runtime_class_for_effect_binding_provider_in_context(
                            env.db,
                            binding,
                            env.scope(),
                            env.assumptions(),
                        )
                        .or_else(|| {
                            runtime_class_for_direct_value_provider_in_context(
                                env.db,
                                binding,
                                env.scope(),
                                env.assumptions(),
                            )
                        })
                    })?;
                Some(provider_root_place_class(
                    env.db,
                    provider_root_value_ty(binding),
                    &provider_class,
                    env.scope(),
                    env.assumptions(),
                ))
            }
        },
    }
}

fn project_variant_field_place_class<'db>(
    db: &'db dyn MirDb,
    class: RuntimeClass<'db>,
    variant: VariantIndex,
    field: FieldIndex,
) -> RuntimeClass<'db> {
    let layout = class
        .aggregate_layout()
        .unwrap_or_else(|| panic!("invalid variant-field projection class"));
    match layout.data(db) {
        Layout::Enum(layout) => {
            layout.variants[variant.0 as usize].fields[field.0 as usize].clone()
        }
        Layout::Struct(_) | Layout::Array(_) => panic!("invalid variant-field projection layout"),
    }
}

pub(crate) fn desired_runtime_effect_arg_boundary<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    arg: &NEffectArg<'db>,
    plan: Option<&RuntimeEffectBindingPlan<'db>>,
    effect_space: AddressSpaceKind,
) -> Option<RuntimeBoundarySpec<'db>> {
    if let Some(plan) = plan {
        return Some(plan.boundary.clone());
    }
    let target_ty = arg.target_ty?;
    if runtime_zero_sized_ty(db, target_ty, env.scope, env.assumptions) {
        return None;
    }
    Some(match arg.pass_mode {
        EffectPassMode::ByPlace | EffectPassMode::ByTempPlace => RuntimeBoundarySpec::BorrowLike {
            pointee: stored_class_for_ty_in_context(db, target_ty, env.scope, env.assumptions),
            access: BorrowAccess::ReadWrite,
            allow: default_borrow_transport_set(BorrowAccess::ReadWrite, effect_space),
        },
        EffectPassMode::ByValue | EffectPassMode::Unknown => boundary_spec_for_ty_in_env(
            db,
            env,
            target_ty,
            effect_space,
        )
        .unwrap_or(RuntimeBoundarySpec::ExactShape(
            provider_class_for_target_in_env(db, env, Some(target_ty), effect_space),
        )),
    })
}

pub(crate) enum ContractMetadataBuiltin<'db> {
    InitCodeOffset(RuntimeCodeRegion<'db>),
    InitCodeLen(RuntimeCodeRegion<'db>),
}

pub(crate) fn contract_metadata_builtin<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> Option<ContractMetadataBuiltin<'db>> {
    let BodyOwner::Func(func) = semantic.key(db).owner(db) else {
        return None;
    };
    let name = func.name(db).to_opt()?.data(db);
    let trait_ = func.containing_trait(db)?;
    if trait_.name(db).to_opt()?.data(db) != "Contract" {
        return None;
    }
    let contract = semantic
        .key(db)
        .subst(db)
        .generic_args(db)
        .iter()
        .find_map(|ty| ty.as_contract(db))?;
    let region = RuntimeCodeRegion::new(db, RuntimeCodeRegionKey::ContractInit { contract });
    match name.as_str() {
        "init_code_offset" => Some(ContractMetadataBuiltin::InitCodeOffset(region)),
        "init_code_len" => Some(ContractMetadataBuiltin::InitCodeLen(region)),
        _ => None,
    }
}

#[salsa::tracked]
fn runtime_extern_builtin_return_class<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    result_ty: TyId<'db>,
) -> Option<Option<RuntimeClass<'db>>> {
    let env = RuntimeTypeEnv::for_semantic(db, semantic);
    if contract_metadata_builtin(db, semantic).is_some() {
        return Some(top_level_class_for_ty_in_env(
            db,
            env,
            result_ty,
            AddressSpaceKind::Memory,
        ));
    }
    let hir::analysis::ty::ty_check::BodyOwner::Func(func) = semantic.key(db).owner(db) else {
        return None;
    };
    if func.body(db).is_none()
        && func
            .name(db)
            .to_opt()
            .is_some_and(|name| is_runtime_intrinsic_name(name.data(db).as_str()))
    {
        return Some(top_level_class_for_ty_in_env(
            db,
            env,
            result_ty,
            AddressSpaceKind::Memory,
        ));
    }
    runtime_builtin_func_kind(db, func)
        .is_some()
        .then(|| top_level_class_for_ty_in_env(db, env, result_ty, AddressSpaceKind::Memory))
}

fn extern_builtin_return_class<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    result_ty: TyId<'db>,
) -> Option<Option<RuntimeClass<'db>>> {
    runtime_extern_builtin_return_class(db, semantic, result_ty)
}

fn is_runtime_intrinsic_name(name: &str) -> bool {
    if matches!(name, "alloc") || generic_numeric_intrinsic_kind(name).is_some() {
        return true;
    }
    intrinsic_numeric_name_parts(name).is_some()
}

pub(super) fn generic_numeric_intrinsic_kind(name: &str) -> Option<GenericNumericIntrinsicKind> {
    Some(match name {
        "__bitcast" => GenericNumericIntrinsicKind::Bitcast,
        "__saturating_add" => GenericNumericIntrinsicKind::Saturating(SaturatingBinOp::Add),
        "__saturating_sub" => GenericNumericIntrinsicKind::Saturating(SaturatingBinOp::Sub),
        "__saturating_mul" => GenericNumericIntrinsicKind::Saturating(SaturatingBinOp::Mul),
        "__checked_add" => GenericNumericIntrinsicKind::CheckedBinary(ArithBinOp::Add),
        "__checked_sub" => GenericNumericIntrinsicKind::CheckedBinary(ArithBinOp::Sub),
        "__checked_mul" => GenericNumericIntrinsicKind::CheckedBinary(ArithBinOp::Mul),
        "__checked_div" => GenericNumericIntrinsicKind::CheckedBinary(ArithBinOp::Div),
        "__checked_rem" => GenericNumericIntrinsicKind::CheckedBinary(ArithBinOp::Rem),
        "__checked_pow" => GenericNumericIntrinsicKind::CheckedBinary(ArithBinOp::Pow),
        "__checked_neg" => GenericNumericIntrinsicKind::CheckedNeg,
        _ => return None,
    })
}

fn intrinsic_numeric_name_parts(name: &str) -> Option<(&str, &str)> {
    let op = name.strip_prefix("__")?;
    [
        "_u8", "_u16", "_u32", "_u64", "_u128", "_u256", "_usize", "_i8", "_i16", "_i32", "_i64",
        "_i128", "_i256", "_isize", "_bool",
    ]
    .iter()
    .find_map(|suffix| op.strip_suffix(suffix).map(|prefix| (prefix, *suffix)))
}

pub(crate) fn runtime_param_class<'db>(
    db: &'db dyn MirDb,
    typed_body: &hir::analysis::ty::ty_check::TypedBody<'db>,
    binding: hir::analysis::ty::ty_check::LocalBinding<'db>,
    env: RuntimeTypeEnv<'db>,
    actual: RuntimeClass<'db>,
) -> RuntimeClass<'db> {
    let ty = runtime_repr_ty_in_context(
        db,
        typed_body.binding_ty(db, binding),
        env.scope,
        env.assumptions,
    );
    if runtime_abstract_param_ty(
        db,
        typed_body.binding_ty(db, binding),
        env.scope,
        env.assumptions,
    ) || matches!(
        ty.base_ty(db).data(db),
        TyData::TyParam(param) if param.is_effect() || param.is_effect_provider()
    ) {
        return actual;
    }
    if binding.is_mut() && ty.as_enum(db).is_some() {
        return RuntimeClass::object_ref(layout_for_ty_in_context(
            db,
            ty,
            env.scope,
            env.assumptions,
        ));
    }
    actual
}

pub(crate) fn runtime_param_boundary<'db>(
    db: &'db dyn MirDb,
    typed_body: &hir::analysis::ty::ty_check::TypedBody<'db>,
    binding: hir::analysis::ty::ty_check::LocalBinding<'db>,
    env: RuntimeTypeEnv<'db>,
    boundary: RuntimeBoundarySpec<'db>,
) -> RuntimeBoundarySpec<'db> {
    match boundary {
        RuntimeBoundarySpec::ExactTransport(actual) => RuntimeBoundarySpec::ExactTransport(
            runtime_param_class(db, typed_body, binding, env, actual),
        ),
        RuntimeBoundarySpec::ExactShape(actual) => RuntimeBoundarySpec::ExactShape(
            runtime_param_class(db, typed_body, binding, env, actual),
        ),
        RuntimeBoundarySpec::BorrowLike {
            pointee,
            access,
            allow,
        } => {
            let ty = runtime_repr_ty_in_context(
                db,
                typed_body.binding_ty(db, binding),
                env.scope,
                env.assumptions,
            );
            if binding.is_mut() && ty.as_enum(db).is_some() {
                return RuntimeBoundarySpec::ExactTransport(RuntimeClass::object_ref(
                    layout_for_ty_in_context(db, ty, env.scope, env.assumptions),
                ));
            }
            RuntimeBoundarySpec::BorrowLike {
                pointee,
                access,
                allow,
            }
        }
    }
}

pub(crate) fn semantic_return_ty<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> TyId<'db> {
    semantic.key(db).typed_body(db).result_ty()
}

pub(crate) fn default_return_class<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> Option<RuntimeClass<'db>> {
    let typed_body = semantic.key(db).typed_body(db);
    let env = RuntimeTypeEnv::for_semantic(db, semantic);
    let return_borrow_provider = typed_body
        .result_ty()
        .as_borrow(db)
        .and(typed_body.return_borrow_provider());
    let default_space =
        return_borrow_provider.map_or(AddressSpaceKind::Memory, address_space_from_provider);
    if return_borrow_provider.is_some() {
        return Some(provider_class_for_target_in_env(
            db,
            env,
            Some(typed_body.result_ty()),
            default_space,
        ));
    }
    top_level_class_for_ty_in_env(db, env, typed_body.result_ty(), default_space)
}

pub(crate) fn desired_runtime_return_plan<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
) -> RuntimeVisibleReturnPlan<'db> {
    let typed_body = semantic.key(db).typed_body(db);
    let env = RuntimeTypeEnv::for_semantic(db, semantic);
    let return_borrow_provider = typed_body
        .result_ty()
        .as_borrow(db)
        .and(typed_body.return_borrow_provider());
    let default_space =
        return_borrow_provider.map_or(AddressSpaceKind::Memory, address_space_from_provider);
    let ty = typed_body.result_ty();
    if return_borrow_provider.is_some() {
        return RuntimeVisibleReturnPlan::PassActual;
    }
    let repr_ty = runtime_repr_ty_in_context(db, ty, env.scope, env.assumptions);
    if runtime_abstract_param_ty(db, ty, env.scope, env.assumptions)
        || matches!(
            repr_ty.base_ty(db).data(db),
            TyData::TyParam(param) if param.is_effect() || param.is_effect_provider()
        )
    {
        return RuntimeVisibleReturnPlan::PassActual;
    }
    let Some(boundary) = boundary_spec_for_ty_in_env(db, env, ty, default_space) else {
        return RuntimeVisibleReturnPlan::Erased;
    };
    match &boundary {
        RuntimeBoundarySpec::ExactTransport(class @ RuntimeClass::Scalar(_))
        | RuntimeBoundarySpec::ExactTransport(class @ RuntimeClass::Ref { .. })
        | RuntimeBoundarySpec::ExactTransport(class @ RuntimeClass::RawAddr { .. }) => {
            RuntimeVisibleReturnPlan::Exact(class.clone())
        }
        RuntimeBoundarySpec::ExactTransport(class @ RuntimeClass::AggregateValue { .. })
            if !aggregate_transport_depends_on_runtime_source(
                db,
                ty,
                env.scope,
                env.assumptions,
            ) =>
        {
            RuntimeVisibleReturnPlan::Exact(class.clone())
        }
        RuntimeBoundarySpec::ExactTransport(RuntimeClass::AggregateValue { .. })
        | RuntimeBoundarySpec::ExactShape(_)
        | RuntimeBoundarySpec::BorrowLike { .. } => RuntimeVisibleReturnPlan::Constrained(boundary),
    }
}

pub(crate) fn actual_aggregate_class_from_runtime_source<'db>(
    class: &RuntimeClass<'db>,
) -> Option<RuntimeClass<'db>> {
    match class {
        RuntimeClass::AggregateValue { .. } => Some(class.clone()),
        RuntimeClass::Ref { pointee, .. } => pointee
            .aggregate_layout()
            .map(|layout| RuntimeClass::AggregateValue { layout }),
        RuntimeClass::RawAddr {
            target: Some(layout),
            ..
        } => Some(RuntimeClass::AggregateValue { layout: *layout }),
        RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { target: None, .. } => None,
    }
}

fn runtime_abstract_param_ty<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    let ty = runtime_repr_ty_in_context(db, ty, scope, assumptions);
    if ty.as_ptr(db).is_some() {
        return false;
    }
    ty.has_param(db) || ty.contains_assoc_ty_of_param(db)
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::{
        analysis::semantic::{
            NEffectArg, SemanticInstance,
            borrowck::{NSTerminatorKind, NormalizedSemanticBody, normalize_semantic_body},
            get_or_build_semantic_instance, owner_effect_bindings,
            resolved_provider_binding_for_instance_effect, root_semantic_instance_key,
        },
        analysis::ty::ty_check::BodyOwner,
    };
    use url::Url;

    use super::super::{
        arg_selector::RuntimeArgSelector,
        boundary::BoundarySiteAllocator,
        call_input::{CompiledCallInputPlan, compile_call_input_plan_for_semantic},
    };
    use super::*;
    use crate::runtime::lower::boundary::BoundaryMatcher;
    use crate::runtime::{
        RuntimeInterfaceSignature,
        lower::{
            infer::LocalStateInferer,
            interface::{runtime_param_locals, runtime_visible_binding_plans},
            returns::runtime_return_class,
        },
        package::runtime_instance_for_semantic,
        package::runtime_instance_for_semantic_with_visible_param_overrides,
    };

    fn call_input_plan_for_test<'db>(
        db: &'db DriverDataBase,
        body: &NormalizedSemanticBody<'db>,
        call_facts: &CallStaticFacts<'db>,
        effect_args: &[NEffectArg<'db>],
    ) -> CompiledCallInputPlan<'db> {
        call_facts.input_plan.clone().unwrap_or_else(|| {
            let mut boundary_sites = BoundarySiteAllocator::default();
            compile_call_input_plan_for_semantic(
                db,
                body,
                call_facts.semantic,
                RuntimeTypeEnv::for_semantic(db, call_facts.semantic),
                effect_args,
                &mut boundary_sites,
            )
        })
    }

    fn runtime_signature_for_named_func<'db>(
        db: &'db DriverDataBase,
        top_mod: hir::hir_def::TopLevelMod<'db>,
        name: &str,
    ) -> RuntimeInterfaceSignature<'db> {
        runtime_instance_for_semantic(db, semantic_instance_for_named_func(db, top_mod, name))
            .interface_signature(db)
    }

    fn func_by_name<'db>(
        db: &'db DriverDataBase,
        top_mod: hir::hir_def::TopLevelMod<'db>,
        name: &str,
    ) -> hir::core::hir_def::item::Func<'db> {
        top_mod
            .all_funcs(db)
            .iter()
            .copied()
            .find(|func| {
                func.name(db)
                    .to_opt()
                    .is_some_and(|func_name| func_name.data(db) == name)
            })
            .unwrap_or_else(|| panic!("missing function `{name}`"))
    }

    fn semantic_instance_for_named_func<'db>(
        db: &'db DriverDataBase,
        top_mod: hir::hir_def::TopLevelMod<'db>,
        name: &str,
    ) -> SemanticInstance<'db> {
        let func = func_by_name(db, top_mod, name);
        let key = root_semantic_instance_key(db, BodyOwner::Func(func)).unwrap_or_else(|err| {
            panic!("failed to build root semantic key for `{name}`: {err:?}")
        });
        get_or_build_semantic_instance(db, key)
    }

    fn contract_by_name<'db>(
        db: &'db DriverDataBase,
        top_mod: hir::hir_def::TopLevelMod<'db>,
        name: &str,
    ) -> hir::hir_def::item::Contract<'db> {
        top_mod
            .all_contracts(db)
            .iter()
            .copied()
            .find(|contract| {
                contract
                    .name(db)
                    .to_opt()
                    .is_some_and(|contract_name| contract_name.data(db) == name)
            })
            .unwrap_or_else(|| panic!("missing contract `{name}`"))
    }

    #[test]
    fn poseidon_helpers_keep_visible_by_value_array_returns() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///poseidon_helpers_keep_visible_by_value_array_returns.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!("../../../../fe/tests/fixtures/fe_test/poseidon_mock.fe").to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let signatures = ["ark", "sigma_full", "mix"]
            .into_iter()
            .map(|name| (name, runtime_signature_for_named_func(&db, top_mod, name)))
            .collect::<Vec<_>>();

        assert!(
            signatures.iter().all(|(_, signature)| matches!(
                signature.ret,
                Some(RuntimeClass::AggregateValue { .. })
            )),
            "Poseidon helpers should keep by-value aggregate return signatures:\n{}",
            signatures
                .iter()
                .map(|(name, signature)| format!("{name}: {signature:#?}"))
                .collect::<Vec<_>>()
                .join("\n\n")
        );
        assert!(
            signatures.iter().all(|(_, signature)| !matches!(
                signature.ret,
                Some(RuntimeClass::Ref {
                    kind: RefKind::Object,
                    ..
                })
            )),
            "Poseidon helpers must not leak internal object-backed carriers into visible return contracts:\n{}",
            signatures
                .iter()
                .map(|(name, signature)| format!("{name}: {signature:#?}"))
                .collect::<Vec<_>>()
                .join("\n\n")
        );
    }

    #[test]
    fn transport_shaped_returns_remain_visible_transport_returns() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///transport_shaped_returns_remain_visible_transport_returns.fe")
                .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!(
                    "../../../../fe/tests/fixtures/fe_test/mut_self_storage_receiver_regression.fe"
                )
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let signature = runtime_signature_for_named_func(&db, top_mod, "value_mut");

        assert!(
            matches!(
                signature.ret,
                Some(RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. })
            ),
            "transport-shaped returns must remain visible transport returns, not be normalized to by-value aggregates:\n{signature:#?}"
        );
        assert!(
            !matches!(signature.ret, Some(RuntimeClass::AggregateValue { .. })),
            "transport-shaped returns must not be normalized to by-value aggregate contracts:\n{signature:#?}"
        );
    }

    #[test]
    fn visible_return_selection_preserves_transport_return_source() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///visible_return_selection_preserves_transport_return_source.fe")
                .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!(
                    "../../../../fe/tests/fixtures/fe_test/contract_field_mut_borrow_matrix.fe"
                )
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, "pick_ac_mut");
        let instance = runtime_instance_for_semantic(&db, semantic);
        let normalized = normalize_semantic_body(&db, semantic)
            .unwrap_or_else(|err| panic!("failed to normalize pick_ac_mut: {err:?}"));
        let facts = BodyStaticFacts::new(&db, &normalized);
        let env = BodyEnv::new(&db, &normalized, &facts);
        let params = instance.key(&db).params(&db);
        let inferred =
            LocalStateInferer::new(env, params, &runtime_param_locals(&db, semantic, params)).run();
        let return_plan = desired_runtime_return_plan(&db, semantic);
        let selected_returns = normalized
            .blocks
            .iter()
            .filter_map(|block| match block.terminator.kind {
                NSTerminatorKind::Return(Some(value)) => Some(value.local),
                NSTerminatorKind::Goto(_)
                | NSTerminatorKind::Branch { .. }
                | NSTerminatorKind::MatchEnum { .. }
                | NSTerminatorKind::Assert { .. }
                | NSTerminatorKind::Return(None) => None,
            })
            .map(|local| {
                selected_visible_return_for_local(env, local, &return_plan, &inferred.carriers)
                    .unwrap_or_else(|| {
                        panic!("pick_ac_mut return local should stay runtime-visible: {local:?}")
                    })
            })
            .collect::<Vec<_>>();

        assert!(
            matches!(
                return_plan,
                RuntimeVisibleReturnPlan::Constrained(RuntimeBoundarySpec::BorrowLike { .. })
            ),
            "pick_ac_mut should use a constrained borrow-like visible return plan:\n{return_plan:#?}",
        );
        assert!(
            !selected_returns.is_empty(),
            "pick_ac_mut should expose at least one normalized return local",
        );
        assert!(
            selected_returns.iter().all(|selected| matches!(
                selected.class,
                RuntimeClass::Ref {
                    kind: RefKind::Provider { .. },
                    ..
                } | RuntimeClass::RawAddr { .. }
            )),
            "visible return selection must preserve transport, not only the semantic pointee class:\nselected={selected_returns:#?}",
        );
    }

    #[test]
    fn specialized_grant_callee_keeps_self_runtime_visible() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///specialized_grant_callee_keeps_self_runtime_visible.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(include_str!("../../../../codegen/tests/fixtures/erc20.fe").to_string()),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let contract = top_mod
            .all_contracts(&db)
            .first()
            .copied()
            .expect("erc20 fixture should define a contract");
        let init_key = root_semantic_instance_key(&db, BodyOwner::ContractInit { contract })
            .unwrap_or_else(|err| panic!("failed to build root init semantic key: {err:?}"));
        let init =
            runtime_instance_for_semantic(&db, get_or_build_semantic_instance(&db, init_key));
        let grant = init
            .calls(&db)
            .iter()
            .find_map(|call| {
                let semantic = call.callee.key(&db).semantic(&db)?;
                match semantic.key(&db).owner(&db) {
                    BodyOwner::Func(func)
                        if func
                            .name(&db)
                            .to_opt()
                            .is_some_and(|name| name.data(&db) == "grant") =>
                    {
                        Some((semantic, call.callee))
                    }
                    _ => None,
                }
            })
            .expect("init should call grant");
        let (semantic, callee) = grant;
        let typed_body = semantic.key(&db).instantiate_typed_body(&db);
        let self_binding = typed_body
            .param_binding(0)
            .expect("grant typed body should keep self as the first param binding");
        let self_role = semantic.binding_role(&db, self_binding);
        let plans = runtime_visible_binding_plans(&db, semantic);
        let signature = callee.interface_signature(&db);

        assert_eq!(
            plans.len(),
            3,
            "specialized grant callee should keep self + 2 explicit args runtime-visible:\nself_role={self_role:#?}\nplans={plans:#?}\nsignature={signature:#?}"
        );
        assert!(
            matches!(
                plans[0].plan,
                RuntimeParamPlan::Boundary(RuntimeBoundarySpec::ExactShape(RuntimeClass::Ref {
                    kind: RefKind::Provider { .. },
                    ..
                }))
            ),
            "specialized grant receiver should remain a visible provider transport:\nself_role={self_role:#?}\nplans={plans:#?}\nsignature={signature:#?}"
        );
        assert_eq!(
            signature.params.len(),
            3,
            "specialized grant runtime signature should keep self + 2 explicit args:\nself_role={self_role:#?}\nplans={plans:#?}\nsignature={signature:#?}"
        );
    }

    #[test]
    fn runtime_instance_overrides_cannot_resurrect_zero_width_params() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///runtime_instance_overrides_cannot_resurrect_zero_width_params.fe")
                .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
struct Empty {}

fn takes_empty(_ host: Empty, value: u256) -> u256 {
    value
}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, "takes_empty");
        let forced_host_class = RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits: 256,
                signed: false,
            },
            role: ScalarRole::Plain,
        });
        let mut override_calls = 0;
        let instance =
            runtime_instance_for_semantic_with_visible_param_overrides(&db, semantic, |entry| {
                if matches!(entry.binding, LocalBinding::Param { idx: 0, .. }) {
                    override_calls += 1;
                    Some(forced_host_class.clone())
                } else {
                    None
                }
            });
        let params = instance.key(&db).params(&db);

        assert_eq!(
            override_calls, 0,
            "override hooks should only see runtime-visible bindings, not zero-width params"
        );
        assert_eq!(
            params.len(),
            1,
            "the zero-width Empty host param must not be reintroduced by the override path:\n{params:#?}"
        );
        assert_eq!(
            runtime_param_locals(&db, semantic, params).len(),
            params.len(),
            "runtime params should remain aligned with visible semantic bindings"
        );
    }

    #[test]
    fn generic_zero_sized_uses_provider_erases_runtime_root() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///generic_zero_sized_uses_provider_erases_runtime_root.fe").unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                r#"
struct Slot<T> {}

fn main() -> u256
uses (slot: Slot<u256>)
{
    1
}
"#
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let semantic = semantic_instance_for_named_func(&db, top_mod, "main");
        let binding = owner_effect_bindings(&db, semantic.key(&db).owner(&db))
            .into_iter()
            .next()
            .expect("main should have one uses binding");
        let provider = resolved_provider_binding_for_instance_effect(&db, semantic, binding)
            .expect("main uses binding should resolve to a provider");
        let env = RuntimeTypeEnv::for_semantic(&db, semantic);

        assert!(
            runtime_effect_binding_plan(&db, semantic, binding).is_none(),
            "generic zero-sized uses providers should have no runtime payload"
        );
        assert!(
            provider_erases_runtime_root(&db, &provider, env.scope, env.assumptions),
            "root effect planning must erase the same generic zero-sized provider"
        );
    }

    #[test]
    fn provider_backed_effect_bindings_keep_actualized_borrow_boundaries() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(
            "file:///provider_backed_effect_bindings_keep_actualized_borrow_boundaries.fe",
        )
        .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!(
                    "../../../../fe/tests/fixtures/fe_test/reentrancy_mutex_storage_map.fe"
                )
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let contract = contract_by_name(&db, top_mod, "B");
        for arm_idx in [1, 2, 4] {
            let semantic = get_or_build_semantic_instance(
                &db,
                root_semantic_instance_key(
                    &db,
                    BodyOwner::ContractRecvArm {
                        contract,
                        recv_idx: 0,
                        arm_idx,
                    },
                )
                .unwrap_or_else(|err| panic!("failed to build recv-arm semantic key: {err:?}")),
            );
            let binding = owner_effect_bindings(&db, semantic.key(&db).owner(&db))
                .into_iter()
                .next()
                .expect("Protected arm should keep one owner effect binding");
            let binding_ty = semantic.binding_ty(&db, binding);
            let plan = runtime_effect_binding_plan(&db, semantic, binding)
                .expect("guarded_balances should lower to a runtime effect binding plan");
            let RuntimeClass::Ref {
                kind: RefKind::Provider { space, .. },
                pointee,
                ..
            } = &plan.class
            else {
                panic!(
                    "guarded_balances should lower as a provider ref for arm {arm_idx}:\n{:#?}",
                    plan.class
                );
            };
            assert_eq!(*space, AddressSpaceKind::Storage);
            let RuntimeClass::AggregateValue { layout } = **pointee else {
                panic!(
                    "guarded_balances provider ref should point at its semantic value aggregate for arm {arm_idx}:\n{:#?}",
                    plan.class
                );
            };
            let Layout::Struct(layout_data) = layout.data(&db) else {
                panic!(
                    "guarded_balances provider pointee should use struct layout for arm {arm_idx}:\n{:#?}",
                    layout.data(&db)
                );
            };
            assert_eq!(layout_data.source_ty, binding_ty);
            assert_eq!(
                layout_data.fields.len(),
                1,
                "guarded_balances Mutex layout should expose the wrapped value field for arm {arm_idx}"
            );

            assert!(
                BoundaryMatcher::class_satisfies_boundary(&plan.class, &plan.boundary),
                "provider-backed effect binding plan should keep an actualized boundary matching its chosen runtime class:\nplan={plan:#?}"
            );
            let _ = runtime_instance_for_semantic(&db, semantic).body(&db);
        }
    }

    #[test]
    fn provider_backed_method_call_inputs_keep_storage_receiver_transport() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(
            "file:///provider_backed_method_call_inputs_keep_storage_receiver_transport.fe",
        )
        .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!(
                    "../../../../fe/tests/fixtures/fe_test/reentrancy_mutex_storage_map.fe"
                )
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let contract = contract_by_name(&db, top_mod, "B");
        let semantic = get_or_build_semantic_instance(
            &db,
            root_semantic_instance_key(
                &db,
                BodyOwner::ContractRecvArm {
                    contract,
                    recv_idx: 0,
                    arm_idx: 4,
                },
            )
            .unwrap_or_else(|err| panic!("failed to build recv-arm semantic key: {err:?}")),
        );
        let instance = runtime_instance_for_semantic(&db, semantic);
        let normalized = normalize_semantic_body(&db, semantic)
            .unwrap_or_else(|err| panic!("failed to normalize LockAndCheck: {err:?}"));
        let facts = BodyStaticFacts::new(&db, &normalized);
        let env = BodyEnv::new(&db, &normalized, &facts);
        let params = instance.key(&db).params(&db);
        let inferred =
            LocalStateInferer::new(env, params, &runtime_param_locals(&db, semantic, params)).run();
        let mut checked_calls = Vec::new();
        for (block_idx, block) in normalized.blocks.iter().enumerate() {
            for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
                let hir::analysis::semantic::NSStmtKind::Assign { expr, .. } = &stmt.kind else {
                    continue;
                };
                let NExpr::Call {
                    callee,
                    args,
                    effect_args,
                    ..
                } = expr
                else {
                    continue;
                };
                let BodyOwner::Func(func) = callee.key.owner(&db) else {
                    continue;
                };
                let Some(name) = func.name(&db).to_opt().map(|name| name.data(&db)) else {
                    continue;
                };
                if !matches!(name.as_str(), "lock" | "is_locked" | "unlock") {
                    continue;
                }
                let ExprStaticFacts::Call(call_facts) =
                    facts.expr(block_idx, stmt_idx).unwrap_or_else(|| {
                        panic!("missing staged call facts for {block_idx}:{stmt_idx}")
                    })
                else {
                    panic!("{name} expression should keep staged call facts");
                };
                let receiver = args.first().map(|arg| arg.local);
                let (receiver_actual, receiver_materialized, selected, selected_classes) = {
                    let mut class_cache = InferClassCache::new(normalized.locals.len());
                    let mut evaluator =
                        RuntimeArgSelector::new(env, &inferred.carriers, Some(&mut class_cache));
                    let receiver_actual =
                        receiver.and_then(|local| evaluator.selected_actual_value(local));
                    let receiver_materialized =
                        receiver.and_then(|local| evaluator.selected_materialized_value(local));
                    let input_plan =
                        call_input_plan_for_test(&db, &normalized, call_facts, effect_args);
                    let selected = evaluator.selected_call_inputs(args, effect_args, &input_plan);
                    let selected_classes = selected
                        .iter()
                        .map(|arg| arg.class.clone())
                        .collect::<Vec<_>>();
                    (
                        receiver_actual,
                        receiver_materialized,
                        selected,
                        selected_classes,
                    )
                };
                let selected_return = runtime_return_class(
                    &db,
                    RuntimeInstanceKey::new(
                        &db,
                        RuntimeInstanceSource::Semantic(call_facts.semantic),
                        selected_classes,
                    ),
                );
                if !selected.is_empty() {
                    assert!(
                        matches!(
                            selected.first().map(|arg| &arg.class),
                            Some(
                                RuntimeClass::Ref {
                                    kind: RefKind::Provider {
                                        space: AddressSpaceKind::Storage,
                                        ..
                                    },
                                    ..
                                } | RuntimeClass::RawAddr {
                                    space: AddressSpaceKind::Storage,
                                    ..
                                }
                            )
                        ),
                        "provider-backed mutex receiver call input should preserve storage transport for `{name}`:\nreceiver_actual={receiver_actual:#?}\nreceiver_materialized={receiver_materialized:#?}\nselected_return={selected_return:#?}\nselected={selected:#?}",
                    );
                }
                if name == "lock" {
                    assert!(
                        matches!(
                            selected_return,
                            Some(
                                RuntimeClass::Ref {
                                    kind: RefKind::Provider {
                                        space: AddressSpaceKind::Storage,
                                        ..
                                    },
                                    ..
                                } | RuntimeClass::RawAddr {
                                    space: AddressSpaceKind::Storage,
                                    ..
                                }
                            )
                        ),
                        "storage-specialized `lock` should keep a storage transport return:\nselected_return={selected_return:#?}\nselected={selected:#?}",
                    );
                }
                checked_calls.push(name.to_string());
            }
        }

        assert_eq!(
            checked_calls,
            ["lock", "is_locked", "unlock"],
            "LockAndCheck should contain the expected mutex method calls",
        );
        let _ = instance.body(&db);
    }

    #[test]
    fn inner_take_call_uses_the_same_runtime_key_and_return_class_as_lowering() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(
            "file:///inner_take_call_uses_the_same_runtime_key_and_return_class_as_lowering.fe",
        )
        .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!(
                    "../../../../fe/tests/fixtures/fe_test/view_param_local_ref_take_reverse.fe"
                )
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let sum_last4 = semantic_instance_for_named_func(&db, top_mod, "sum_last4");
        let (semantic, instance) = runtime_instance_for_semantic(&db, sum_last4)
            .calls(&db)
            .iter()
            .find_map(|call| {
                let semantic = call.callee.key(&db).semantic(&db)?;
                match semantic.key(&db).owner(&db) {
                    BodyOwner::Func(func)
                        if func
                            .name(&db)
                            .to_opt()
                            .is_some_and(|name| name.data(&db) == "take_u256") =>
                    {
                        Some((semantic, call.callee))
                    }
                    _ => None,
                }
            })
            .expect("sum_last4 should call specialized take_u256");
        let normalized = normalize_semantic_body(&db, semantic)
            .unwrap_or_else(|err| panic!("failed to normalize specialized take_u256: {err:?}"));
        let facts = BodyStaticFacts::new(&db, &normalized);
        let env = BodyEnv::new(&db, &normalized, &facts);
        let params = instance.key(&db).params(&db);
        let inferred =
            LocalStateInferer::new(env, params, &runtime_param_locals(&db, semantic, params)).run();
        let (call_dst, args, effect_args, call_facts) = normalized
            .blocks
            .iter()
            .enumerate()
            .find_map(|(block_idx, block)| {
                block.stmts.iter().enumerate().find_map(|(stmt_idx, stmt)| {
                    let hir::analysis::semantic::NSStmtKind::Assign { dst, expr } = &stmt.kind
                    else {
                        return None;
                    };
                    let NExpr::Call {
                        callee,
                        args,
                        effect_args,
                        ..
                    } = expr
                    else {
                        return None;
                    };
                    let BodyOwner::Func(func) = callee.key.owner(&db) else {
                        return None;
                    };
                    if func
                        .name(&db)
                        .to_opt()
                        .is_none_or(|name| name.data(&db) != "take")
                    {
                        return None;
                    }
                    let ExprStaticFacts::Call(call_facts) =
                        facts.expr(block_idx, stmt_idx).unwrap_or_else(|| {
                            panic!("missing staged call facts for {block_idx}:{stmt_idx}")
                        })
                    else {
                        panic!("inner take expression should keep staged call facts");
                    };
                    Some((*dst, args.clone(), effect_args.clone(), call_facts.clone()))
                })
            })
            .expect("specialized take_u256 should contain an inner call to take");
        let mut class_cache = InferClassCache::new(normalized.locals.len());
        let input_plan = call_input_plan_for_test(&db, &normalized, &call_facts, &effect_args);
        let inferred_param_classes =
            RuntimeArgSelector::new(env, &inferred.carriers, Some(&mut class_cache))
                .selected_call_inputs(&args, &effect_args, &input_plan)
                .into_iter()
                .map(|arg| arg.class)
                .collect::<Vec<_>>();
        let lowered_take = instance
            .calls(&db)
            .iter()
            .find_map(|call| {
                let semantic = call.callee.key(&db).semantic(&db)?;
                match semantic.key(&db).owner(&db) {
                    BodyOwner::Func(func)
                        if func
                            .name(&db)
                            .to_opt()
                            .is_some_and(|name| name.data(&db) == "take") =>
                    {
                        Some(call.callee)
                    }
                    _ => None,
                }
            })
            .expect("specialized take_u256 should lower an inner call to take");
        let inferred_dst_class = match inferred.carriers.get(call_dst.index()) {
            Some(RuntimeCarrier::Value(class)) => Some(class.clone()),
            Some(RuntimeCarrier::Erased) | None => None,
        };
        let lowered_dst_class = match instance.body(&db).locals.get(call_dst.index()) {
            Some(local) => match &local.carrier {
                RuntimeCarrier::Value(class) => Some(class.clone()),
                RuntimeCarrier::Erased => None,
            },
            None => None,
        };
        let lowered_return_class = runtime_return_class(&db, lowered_take.key(&db));

        assert_eq!(
            inferred_param_classes,
            *lowered_take.key(&db).params(&db),
            "infer-time call classification should build the same runtime key as lowering for take_u256 -> take:\ninferred_param_classes={inferred_param_classes:#?}\nlowered_key={:#?}",
            lowered_take.key(&db),
        );
        assert_eq!(
            inferred_dst_class, lowered_dst_class,
            "infer-time dst carrier should match the lowered call-result carrier for take_u256 -> take:\ninferred_dst_class={inferred_dst_class:#?}\nlowered_dst_class={lowered_dst_class:#?}",
        );
        assert_eq!(
            inferred_dst_class, lowered_return_class,
            "infer-time dst carrier should match the specialized callee return class for take_u256 -> take:\ninferred_dst_class={inferred_dst_class:#?}\nlowered_return_class={lowered_return_class:#?}",
        );
    }

    #[test]
    fn own_scalar_call_inputs_materialize_provider_backed_direct_values() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(
            "file:///own_scalar_call_inputs_materialize_provider_backed_direct_values.fe",
        )
        .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!(
                    "../../../../fe/tests/fixtures/fe_test/contract_field_mut_borrow_matrix.fe"
                )
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let contract = contract_by_name(&db, top_mod, "C");
        let semantic = get_or_build_semantic_instance(
            &db,
            root_semantic_instance_key(
                &db,
                BodyOwner::ContractRecvArm {
                    contract,
                    recv_idx: 0,
                    arm_idx: 2,
                },
            )
            .unwrap_or_else(|err| panic!("failed to build recv-arm semantic key: {err:?}")),
        );
        let instance = runtime_instance_for_semantic(&db, semantic);
        let normalized = normalize_semantic_body(&db, semantic)
            .unwrap_or_else(|err| panic!("failed to normalize SelectAndMutate: {err:?}"));
        let facts = BodyStaticFacts::new(&db, &normalized);
        let env = BodyEnv::new(&db, &normalized, &facts);
        let params = instance.key(&db).params(&db);
        let inferred =
            LocalStateInferer::new(env, params, &runtime_param_locals(&db, semantic, params)).run();
        let (args, effect_args, call_facts) = normalized
            .blocks
            .iter()
            .enumerate()
            .find_map(|(block_idx, block)| {
                block.stmts.iter().enumerate().find_map(|(stmt_idx, stmt)| {
                    let hir::analysis::semantic::NSStmtKind::Assign { expr, .. } = &stmt.kind
                    else {
                        return None;
                    };
                    let NExpr::Call {
                        callee,
                        args,
                        effect_args,
                        ..
                    } = expr
                    else {
                        return None;
                    };
                    let BodyOwner::Func(func) = callee.key.owner(&db) else {
                        return None;
                    };
                    if func
                        .name(&db)
                        .to_opt()
                        .is_none_or(|name| name.data(&db) != "set_scaled")
                    {
                        return None;
                    }
                    let ExprStaticFacts::Call(call_facts) =
                        facts.expr(block_idx, stmt_idx).unwrap_or_else(|| {
                            panic!("missing staged call facts for {block_idx}:{stmt_idx}")
                        })
                    else {
                        panic!("set_scaled expression should keep staged call facts");
                    };
                    Some((args.clone(), effect_args.clone(), call_facts.clone()))
                })
            })
            .expect("SelectAndMutate should call set_scaled");
        let mut class_cache = InferClassCache::new(normalized.locals.len());
        let input_plan = call_input_plan_for_test(&db, &normalized, &call_facts, &effect_args);
        let inferred_param_classes =
            RuntimeArgSelector::new(env, &inferred.carriers, Some(&mut class_cache))
                .selected_call_inputs(&args, &effect_args, &input_plan)
                .into_iter()
                .map(|arg| arg.class)
                .collect::<Vec<_>>();
        let lowered_set_scaled = instance
            .calls(&db)
            .iter()
            .find_map(|call| {
                let semantic = call.callee.key(&db).semantic(&db)?;
                match semantic.key(&db).owner(&db) {
                    BodyOwner::Func(func)
                        if func
                            .name(&db)
                            .to_opt()
                            .is_some_and(|name| name.data(&db) == "set_scaled") =>
                    {
                        Some(call.callee)
                    }
                    _ => None,
                }
            })
            .expect("SelectAndMutate should lower a call to set_scaled");

        assert!(
            matches!(
                inferred_param_classes.first(),
                Some(RuntimeClass::Scalar(_))
            ),
            "provider-backed direct values passed to own u256 params must materialize by value:\nclasses={inferred_param_classes:#?}",
        );
        assert!(
            matches!(
                lowered_set_scaled.key(&db).params(&db).first(),
                Some(RuntimeClass::Scalar(_))
            ),
            "specialized set_scaled runtime key must keep its first param by-value:\nkey={:#?}",
            lowered_set_scaled.key(&db),
        );
    }

    #[test]
    fn transport_shaped_method_return_keeps_runtime_visible_signature() {
        let mut db = DriverDataBase::default();
        let file_url =
            Url::parse("file:///transport_shaped_method_return_keeps_runtime_visible_signature.fe")
                .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!(
                    "../../../../fe/tests/fixtures/fe_test/contract_field_mut_borrow_matrix.fe"
                )
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let signature = runtime_signature_for_named_func(&db, top_mod, "pick_ac_mut");

        assert!(
            matches!(
                signature.ret,
                Some(
                    RuntimeClass::Ref {
                        kind: RefKind::Provider { .. },
                        ..
                    } | RuntimeClass::RawAddr {
                        space: AddressSpaceKind::Storage,
                        ..
                    }
                )
            ),
            "mut-returning method signatures must keep provider/storage transport, not degrade to object refs:\n{signature:#?}"
        );
    }
}
