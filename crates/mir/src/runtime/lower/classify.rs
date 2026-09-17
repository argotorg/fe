use common::indexmap::IndexSet;
use cranelift_entity::{EntityRef, PrimaryMap, entity_impl};
use hir::analysis::{
    semantic::{
        FieldIndex, GenericSubst, ImplEnv, Mutability, SConst, SLocal, SLocalId, SemanticCalleeRef,
        SemanticInstance, SemanticInstanceKey, SemanticLocalKind, SemanticLocalRole,
        ValueProvenance, VariantIndex, get_or_build_semantic_instance,
        normalized::{
            NDataProjection, NEffectArg, NExpr, NIndex, NOperand, NPlace, NPlaceBase, NRootKind,
            NStatementKind, NValueDefinition, NValueId, ReadMode,
        },
    },
    ty::{
        ProviderKind,
        corelib::runtime_builtin_func_kind,
        normalize::normalize_ty,
        provider::registered_root_providers,
        trait_def::{
            TraitInstId, complete_resolved_trait_method_args, resolve_trait_method_instance,
        },
        trait_resolution::{PredicateListId, TraitSolveCx},
        ty_check::{BodyOwner, EffectParamSite, EffectPassMode, LocalBinding, ParamSite},
        ty_def::{CapabilityKind, TyData, TyId},
    },
};
use hir::hir_def::{ArithBinOp, FuncParamMode, ItemKind};
use hir::semantic::{ProviderBinding, ProviderSource, constraints_for};
use rustc_hash::FxHashMap;
use salsa::Update;

use crate::{
    db::MirDb,
    instance::{RuntimeInstanceKey, RuntimeInstanceSource},
    runtime::place::{project_field_class, project_index_class, ref_class_for_place_result},
    runtime::{
        AddressSpaceKind, BorrowAccess, Layout, LayoutId, RefKind, RuntimeBoundarySpec,
        RuntimeCarrier, RuntimeClass, RuntimeCodeRegion, RuntimeCodeRegionKey, RuntimeParamPlan,
        SaturatingBinOp, ScalarClass, ScalarRepr, ScalarRole,
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
    interface::{runtime_visible_binding_local, runtime_visible_binding_plans},
    layout::{
        layout_for_aggregate_instance_in_env, layout_for_enum_variant_instance_in_env,
        layout_for_ty_in_env,
    },
    provider_space::address_space_from_provider,
    realize::SelectedRuntimeArg,
    returns::{StaticRuntimeReturnDecision, static_runtime_return_decision},
    semantic_body::{RuntimeOperand, RuntimeSemanticBody},
    type_info::{
        RuntimeTypeEnv, effect_handle_transport_class_for_ty_in_env,
        provider_address_space_to_runtime, provider_class_for_target_in_env,
        runtime_interface_ty_in_env, runtime_repr_ty_in_env, runtime_zero_sized_transport_ty,
        runtime_zero_sized_ty, scalar_class_for_ty_in_env, stored_class_for_ty_in_env,
        top_level_class_for_ty_in_env,
    },
};

#[derive(Clone)]
pub(crate) struct BodyStaticFacts<'db> {
    local_facts: Vec<LocalStaticFacts<'db>>,
    assignments: PrimaryMap<AssignmentId, AssignStaticFacts<'db>>,
    statement_assignments: Vec<Vec<Option<AssignmentId>>>,
    source_locals: Vec<Vec<SLocalId>>,
    assignments_using_local: Vec<Vec<AssignmentId>>,
    assignments_defining_local: Vec<Vec<AssignmentId>>,
    dynamic_dependents: Vec<Vec<SLocalId>>,
    root_provider_locals: FxHashMap<ProviderBinding<'db>, SLocalId>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct AssignmentId(u32);
entity_impl!(AssignmentId);

#[derive(Clone)]
pub(super) struct AssignStaticFacts<'db> {
    pub(super) block_idx: usize,
    pub(super) stmt_idx: usize,
    pub(super) dst: SLocalId,
    uses: Vec<SLocalId>,
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
    pub(crate) fn new(db: &'db dyn MirDb, body: &RuntimeSemanticBody<'db>) -> Self {
        let typed_body = body.owner().key(db).typed_body(db);
        let type_env = RuntimeTypeEnv::for_semantic(db, body.owner());
        Self::new_in_context(db, body, typed_body, type_env)
    }

    pub(super) fn new_in_context(
        db: &'db dyn MirDb,
        body: &RuntimeSemanticBody<'db>,
        typed_body: &hir::analysis::ty::ty_check::TypedBody<'db>,
        type_env: RuntimeTypeEnv<'db>,
    ) -> Self {
        let mut boundary_sites = BoundarySiteAllocator::default();
        let expr_facts_builder = ExprStaticFactsBuilder {
            db,
            body,
            typed_body,
            type_env,
        };
        let local_facts: Vec<_> = body
            .locals
            .iter()
            .map(|local_data| build_local_static_facts(db, type_env, body, local_data))
            .collect();
        let mut assignments = PrimaryMap::new();
        let local_count = body.locals.len();
        let mut statement_assignments = body
            .normalized
            .blocks
            .iter()
            .map(|block| vec![None; block.statements.len()])
            .collect::<Vec<_>>();
        let mut source_locals = vec![Vec::new(); local_count];
        let mut assignments_using_local = vec![Vec::new(); local_count];
        let mut assignments_defining_local = vec![Vec::new(); local_count];
        let mut dynamic_dependents = vec![Vec::new(); local_count];
        for (block_idx, block) in body.normalized.blocks.iter().enumerate() {
            for (stmt_idx, statement) in block.statements.iter().enumerate() {
                let NStatementKind::Define { result, expr } = &statement.kind else {
                    continue;
                };
                let dst = body.value_local(*result).unwrap_or_else(|| {
                    panic!("missing runtime representation for normalized value {result:?}")
                });
                let result_ty = body
                    .normalized
                    .value(*result)
                    .expect("normalized definition result must exist")
                    .ty;
                let uses = runtime_expr_source_locals(body, expr);
                let dynamic_indices = runtime_expr_dynamic_index_locals(body, expr);
                let expr_facts =
                    expr_facts_builder.build(expr, dst, result_ty, &mut boundary_sites);
                let assignment = assignments.push(AssignStaticFacts {
                    block_idx,
                    stmt_idx,
                    dst,
                    uses: uses.clone(),
                    expr: expr_facts,
                });
                statement_assignments[block_idx][stmt_idx] = Some(assignment);
                assignments_defining_local[dst.index()].push(assignment);
                for source in uses {
                    push_unique(&mut source_locals[dst.index()], source);
                    assignments_using_local[source.index()].push(assignment);
                }
                for index in dynamic_indices {
                    push_unique(&mut dynamic_dependents[index.index()], dst);
                }
            }
        }
        let root_provider_locals = build_runtime_visible_root_provider_locals(db, body);
        Self {
            local_facts,
            assignments,
            statement_assignments,
            source_locals,
            assignments_using_local,
            assignments_defining_local,
            dynamic_dependents,
            root_provider_locals,
        }
    }

    fn local(&self, local: SLocalId) -> Option<&LocalStaticFacts<'db>> {
        self.local_facts.get(local.index())
    }

    fn expr(&self, block_idx: usize, stmt_idx: usize) -> Option<&ExprStaticFacts<'db>> {
        let assign_id = *self
            .statement_assignments
            .get(block_idx)?
            .get(stmt_idx)?
            .as_ref()?;
        self.assignments.get(assign_id)?.expr.as_ref()
    }

    pub(super) fn assignment(&self, assign_id: AssignmentId) -> Option<&AssignStaticFacts<'db>> {
        self.assignments.get(assign_id)
    }

    pub(super) fn assignments(&self) -> &PrimaryMap<AssignmentId, AssignStaticFacts<'db>> {
        &self.assignments
    }

    pub(super) fn source_locals(&self, local: SLocalId) -> &[SLocalId] {
        self.source_locals
            .get(local.index())
            .map(Vec::as_slice)
            .unwrap_or_default()
    }

    fn assignments_using_local(&self, local: SLocalId) -> &[AssignmentId] {
        self.assignments_using_local
            .get(local.index())
            .map(Vec::as_slice)
            .unwrap_or_default()
    }

    pub(super) fn assignments_defining_local(&self, local: SLocalId) -> &[AssignmentId] {
        self.assignments_defining_local
            .get(local.index())
            .map(Vec::as_slice)
            .unwrap_or_default()
    }

    pub(super) fn assignment_uses(&self, assign_id: AssignmentId) -> &[SLocalId] {
        self.assignments
            .get(assign_id)
            .map(|assignment| assignment.uses.as_slice())
            .unwrap_or(&[])
    }

    fn dynamic_dependents(&self, local: SLocalId) -> &[SLocalId] {
        self.dynamic_dependents
            .get(local.index())
            .map(Vec::as_slice)
            .unwrap_or_default()
    }

    pub(super) fn root_provider_local(&self, provider: &ProviderBinding<'db>) -> Option<SLocalId> {
        self.root_provider_locals.get(provider).copied()
    }
}

fn runtime_expr_source_locals(body: &RuntimeSemanticBody<'_>, expr: &NExpr<'_>) -> Vec<SLocalId> {
    let mut sources = Vec::new();
    expr.for_each_value_operand(|operand| {
        if let Some(local) = body.operand_local(operand) {
            push_unique(&mut sources, local);
        }
    });
    expr.for_each_place_operand(|place| {
        match place.base {
            NPlaceBase::Root(root) => {
                if let Some(local) = body.root_local(root) {
                    push_unique(&mut sources, local);
                }
            }
            NPlaceBase::CapabilityTarget { carrier } => {
                if let Some(local) = body.value_local(carrier) {
                    push_unique(&mut sources, local);
                }
            }
        }
        for projection in place.path.iter() {
            if let NDataProjection::Index(NIndex::Value(value)) = projection
                && let Some(local) = body.value_local(*value)
            {
                push_unique(&mut sources, local);
            }
        }
    });
    sources
}

fn runtime_expr_dynamic_index_locals(
    body: &RuntimeSemanticBody<'_>,
    expr: &NExpr<'_>,
) -> Vec<SLocalId> {
    let mut indices = Vec::new();
    expr.for_each_place_operand(|place| {
        for projection in place.path.iter() {
            if let NDataProjection::Index(NIndex::Value(value)) = projection
                && let Some(local) = body.value_local(*value)
            {
                push_unique(&mut indices, local);
            }
        }
    });
    if let NExpr::ProjectValue { path, .. } = expr {
        for projection in path.0.iter() {
            if let NDataProjection::Index(NIndex::Value(value)) = projection
                && let Some(local) = body.value_local(*value)
            {
                push_unique(&mut indices, local);
            }
        }
    }
    indices
}

fn push_unique<T: Copy + PartialEq>(values: &mut Vec<T>, value: T) {
    if !values.contains(&value) {
        values.push(value);
    }
}

#[derive(Clone, Copy)]
pub(crate) struct BodyEnv<'a, 'db> {
    db: &'db dyn MirDb,
    body: &'a RuntimeSemanticBody<'db>,
    type_env: RuntimeTypeEnv<'db>,
    facts: &'a BodyStaticFacts<'db>,
}

impl<'a, 'db> BodyEnv<'a, 'db> {
    pub(crate) fn new(
        db: &'db dyn MirDb,
        body: &'a RuntimeSemanticBody<'db>,
        facts: &'a BodyStaticFacts<'db>,
    ) -> Self {
        let type_env = RuntimeTypeEnv::for_semantic(db, body.owner());
        Self::from_parts(db, body, type_env, facts)
    }

    pub(super) fn from_parts(
        db: &'db dyn MirDb,
        body: &'a RuntimeSemanticBody<'db>,
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

    pub(super) fn body(self) -> &'a RuntimeSemanticBody<'db> {
        self.body
    }

    pub(super) fn local(self, local: SLocalId) -> Option<&'a SLocal<'db>> {
        self.body.local(local)
    }

    pub(super) fn value_local(self, value: NValueId) -> Option<SLocalId> {
        self.body.value_local(value)
    }

    pub(super) fn type_env(self) -> RuntimeTypeEnv<'db> {
        self.type_env
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

    pub(super) fn source_locals(self, local: SLocalId) -> &'a [SLocalId] {
        self.facts.source_locals(local)
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
            NExpr::Forward { src } | NExpr::StructuralRepack { value: src, .. } => {
                let operand = self.body.runtime_operand(*src)?;
                RuntimeArgSelector::new(self, carriers, class_cache)
                    .selected_materialized_operand(operand)?
                    .class
            }
            NExpr::Const(_)
            | NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::ScalarCast { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. }
            | NExpr::IsEnumVariant { .. } => match expr_facts {
                Some(ExprStaticFacts::Const(class) | ExprStaticFacts::DirectClass(class)) => {
                    class.clone()?
                }
                _ => panic!(
                    "missing staged runtime class facts: owner={:?}; expr={expr:?}",
                    self.body.owner().key(self.db),
                ),
            },
            NExpr::CodeRegionRef { .. } => return None,
            NExpr::ArrayRepeat { ty, value } => {
                let Some(ExprStaticFacts::AggregateMake(facts)) = expr_facts else {
                    panic!(
                        "missing staged array-repeat facts: owner={:?}; expr={expr:?}",
                        self.body.owner().key(self.db),
                    );
                };
                let len = ty.array_len(self.db).unwrap_or_else(|| {
                    panic!(
                        "array repeat with non-concrete length reached runtime class inference: \
                         {expr:?}"
                    )
                });
                aggregate_make_class_from_facts(
                    self,
                    facts,
                    &vec![*value; len],
                    carriers,
                    class_cache.as_deref_mut(),
                )?
            }
            NExpr::GetEnumTag { value } => {
                let local = self.value_local(value.value)?;
                let enum_layout = self
                    .semantic_value_class(carriers, local)?
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
                        self.body.owner().key(self.db),
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
            NExpr::ProjectValue { value, path } => {
                let value_class = self.normalized_value_structural_class(carriers, value.value)?;
                self.walk_data_path_class(value_class, &path.0)
            }
            NExpr::Load { place, .. } => match expr_facts {
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
                    self.body.owner().key(self.db),
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
                        self.body.owner().key(self.db),
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
        place: &NPlace<'db>,
    ) -> Option<RuntimeClass<'db>> {
        let root = normalized_place_root_class_in_context(self, place.base, carriers)?;
        Some(self.walk_data_path_class(root, &place.path))
    }

    fn walk_data_path_class(
        self,
        mut current: RuntimeClass<'db>,
        path: &hir::analysis::semantic::normalized::NDataPath,
    ) -> RuntimeClass<'db> {
        for (index, projection) in path.iter().enumerate() {
            current = match projection {
                NDataProjection::Field(field) => project_field_class(self.db, current, *field),
                NDataProjection::Index(_) => project_index_class(self.db, current),
                NDataProjection::VariantField { variant, field } => {
                    project_variant_field_place_class(self.db, current, *variant, *field)
                }
            };
            if index + 1 < path.len()
                && let Some(target) = current.deref_target()
            {
                current = target;
            }
        }
        current
    }

    pub(crate) fn normalized_place_address_class(
        self,
        carriers: &[RuntimeCarrier<'db>],
        place: &NPlace<'db>,
    ) -> Option<RuntimeClass<'db>> {
        let value_class = self.normalized_place_class(carriers, place)?;
        let root_class =
            normalized_place_root_transport_class_in_context(self, place.base, carriers)?;
        let (root_space, force_raw) = match place.base {
            NPlaceBase::CapabilityTarget { .. } => (
                root_class
                    .address_space()
                    .unwrap_or(AddressSpaceKind::Memory),
                matches!(&root_class, RuntimeClass::RawAddr { .. }),
            ),
            NPlaceBase::Root(root) => match &self.body.normalized.root(root)?.kind {
                NRootKind::LocalSlot { .. } | NRootKind::ParamPlace { .. } => {
                    (AddressSpaceKind::Memory, false)
                }
                NRootKind::Provider { binding } => {
                    (provider_root_space(binding, &root_class), false)
                }
                NRootKind::CapabilityRepresentation { .. } => (
                    root_class
                        .address_space()
                        .unwrap_or(AddressSpaceKind::Memory),
                    matches!(&root_class, RuntimeClass::RawAddr { .. }),
                ),
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
        self.semantic_value_class(carriers, local)
            .and_then(|class| class.aggregate_value_class())
    }

    pub(crate) fn runtime_operand_value_class(
        self,
        carriers: &[RuntimeCarrier<'db>],
        operand: RuntimeOperand,
    ) -> Option<RuntimeClass<'db>> {
        operand
            .value
            .and_then(|value| normalized_value_runtime_class(self, value, carriers))
            .or_else(|| self.semantic_value_class(carriers, operand.local))
    }

    pub(crate) fn normalized_value_structural_class(
        self,
        carriers: &[RuntimeCarrier<'db>],
        value: NValueId,
    ) -> Option<RuntimeClass<'db>> {
        let ty = self.body.normalized.value(value)?.ty;
        let ordinary = stored_class_for_ty_in_env(self.db, self.type_env(), ty);
        if effect_handle_transport_class_for_ty_in_env(self.db, self.type_env(), ty).is_some() {
            return Some(ordinary);
        }
        self.value_local(value)
            .and_then(|local| self.semantic_value_class(carriers, local))
            .or(Some(ordinary))
    }

    pub(crate) fn actual_aggregate_class_for_operand(
        self,
        carriers: &[RuntimeCarrier<'db>],
        operand: RuntimeOperand,
    ) -> Option<RuntimeClass<'db>> {
        self.runtime_operand_value_class(carriers, operand)
            .and_then(|class| class.aggregate_value_class())
    }

    pub(super) fn semantic_value_class(
        self,
        carriers: &[RuntimeCarrier<'db>],
        local: SLocalId,
    ) -> Option<RuntimeClass<'db>> {
        let local_data = self.local(local)?;
        let local_facts = self.local_facts(local)?;
        match local_data.role.kind() {
            SemanticLocalKind::Erased => None,
            SemanticLocalKind::DirectValue => {
                if local_data.role.root_provider(&self.body.locals).is_some()
                    && matches!(
                        local_facts.root_place_fallback_class,
                        Some(RuntimeClass::Scalar(_))
                    )
                {
                    local_facts.root_place_fallback_class.clone()
                } else {
                    carrier_value_class(local, carriers)
                }
            }
            SemanticLocalKind::DirectCarrier => carrier_value_class(local, carriers),
            SemanticLocalKind::PlaceCarrier => carrier_value_class(local, carriers)
                .or_else(|| local_facts.semantic_fallback_class.clone()),
            SemanticLocalKind::PlaceBoundValue => {
                if matches!(
                    local_data.role,
                    SemanticLocalRole::PlaceBoundValue {
                        provenance: hir::analysis::semantic::PlaceProvenance::RootProvider(_),
                        ..
                    }
                ) && matches!(
                    local_facts.semantic_fallback_class,
                    Some(RuntimeClass::Scalar(_))
                ) {
                    local_facts.semantic_fallback_class.clone()
                } else {
                    carrier_value_class(local, carriers)
                        .or_else(|| local_facts.semantic_fallback_class.clone())
                }
            }
        }
    }
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
    body: &RuntimeSemanticBody<'db>,
) -> FxHashMap<ProviderBinding<'db>, SLocalId> {
    let semantic = body.owner();
    let mut locals = FxHashMap::default();
    for entry in runtime_visible_binding_plans(db, semantic) {
        let Some(provider) = root_provider_for_runtime_visible_binding(db, semantic, entry.binding)
        else {
            continue;
        };
        let local = runtime_visible_binding_local(&body.source, entry.binding);
        locals.entry(provider).or_insert(local);
    }
    locals
}

fn build_local_static_facts<'db>(
    db: &'db dyn MirDb,
    type_env: RuntimeTypeEnv<'db>,
    body: &RuntimeSemanticBody<'db>,
    local_data: &SLocal<'db>,
) -> LocalStaticFacts<'db> {
    let scope = type_env.scope;
    let assumptions = type_env.assumptions;
    let lowered_ty = lowered_place_like_ty(local_data);
    let local_is_effect_handle =
        effect_handle_transport_class_for_ty_in_env(db, type_env, local_data.ty).is_some();
    let zero_sized_transport = if local_is_effect_handle
        && !local_uses_effect_handle_transport(local_data, &body.locals)
    {
        runtime_zero_sized_ty(db, local_data.ty, scope, assumptions)
    } else {
        runtime_zero_sized_transport_ty(db, local_data.ty, scope, assumptions)
            || (!local_is_effect_handle
                && lowered_ty
                    .is_some_and(|ty| runtime_zero_sized_transport_ty(db, ty, scope, assumptions)))
    };
    let interface = local_data.role.kind();
    let semantic_fallback_class = match interface {
        SemanticLocalKind::PlaceCarrier | SemanticLocalKind::PlaceBoundValue
            if !zero_sized_transport =>
        {
            lowered_ty.map(|ty| stored_class_for_ty_in_env(db, type_env, ty))
        }
        SemanticLocalKind::Erased
        | SemanticLocalKind::DirectValue
        | SemanticLocalKind::DirectCarrier
        | SemanticLocalKind::PlaceCarrier
        | SemanticLocalKind::PlaceBoundValue => None,
    };
    let root_place_fallback_class = match interface {
        SemanticLocalKind::Erased => None,
        SemanticLocalKind::DirectValue if !zero_sized_transport => {
            Some(stored_class_for_ty_in_env(db, type_env, local_data.ty))
        }
        SemanticLocalKind::PlaceCarrier
        | SemanticLocalKind::DirectCarrier
        | SemanticLocalKind::PlaceBoundValue
            if !zero_sized_transport =>
        {
            lowered_ty.map(|ty| stored_class_for_ty_in_env(db, type_env, ty))
        }
        SemanticLocalKind::DirectValue
        | SemanticLocalKind::PlaceCarrier
        | SemanticLocalKind::DirectCarrier
        | SemanticLocalKind::PlaceBoundValue => None,
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
            body,
            local_data,
            scope,
            assumptions,
        ),
        materialization_plan: if matches!(interface, SemanticLocalKind::Erased)
            || zero_sized_transport
        {
            CompiledMaterializationPlan::Erased
        } else {
            match interface {
                SemanticLocalKind::DirectValue => top_level_class_for_ty_in_env(
                    db,
                    type_env,
                    local_data.ty,
                    AddressSpaceKind::Memory,
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

pub(super) fn local_uses_effect_handle_transport(
    local: &SLocal<'_>,
    locals: &[SLocal<'_>],
) -> bool {
    local.role.root_provider(locals).is_some()
        || matches!(local.source, Some(LocalBinding::EffectParam { .. }))
}

pub(super) fn lowered_place_like_ty<'db>(local_data: &SLocal<'db>) -> Option<TyId<'db>> {
    match &local_data.role {
        SemanticLocalRole::PlaceCarrier { value_ty, .. }
        | SemanticLocalRole::PlaceBoundValue { value_ty, .. } => Some(*value_ty),
        SemanticLocalRole::DirectCarrier { target_ty, .. } => Some(*target_ty),
        SemanticLocalRole::Erased | SemanticLocalRole::DirectValue { .. } => None,
    }
}

fn local_disallows_const_ref_storage(body: &RuntimeSemanticBody<'_>, local: SLocalId) -> bool {
    let place_uses_local = |place: &NPlace<'_>| match place.base {
        NPlaceBase::Root(root) => body.root_local(root) == Some(local),
        NPlaceBase::CapabilityTarget { carrier } => body.value_local(carrier) == Some(local),
    };
    let mutable_place_uses_local = |place: &NPlace<'_>| {
        place_uses_local(place)
            && match place.base {
                NPlaceBase::Root(root) => body
                    .normalized
                    .root(root)
                    .is_some_and(|root| matches!(root.mutability, Mutability::Mutable)),
                NPlaceBase::CapabilityTarget { .. } => true,
            }
    };
    body.normalized.blocks.iter().any(|block| {
        block
            .statements
            .iter()
            .any(|statement| match &statement.kind {
                NStatementKind::Store { destination, .. } => mutable_place_uses_local(destination),
                NStatementKind::Define {
                    expr: NExpr::Borrow { place, kind, .. },
                    ..
                } => {
                    matches!(kind, hir::analysis::ty::ty_def::BorrowKind::Mut)
                        && place_uses_local(place)
                }
                NStatementKind::Define {
                    expr: NExpr::Call { effect_args, .. },
                    ..
                } => effect_args.iter().any(|arg| {
                    arg.required_mut
                        && match &arg.arg {
                            hir::analysis::semantic::normalized::NEffectArgValue::Place(place) => {
                                place_uses_local(place)
                            }
                            hir::analysis::semantic::normalized::NEffectArgValue::Value(value) => {
                                body.operand_local(*value) == Some(local)
                            }
                        }
                }),
                NStatementKind::Define { .. } => false,
            })
    })
}

struct ExprStaticFactsBuilder<'a, 'db> {
    db: &'db dyn MirDb,
    body: &'a RuntimeSemanticBody<'db>,
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
            NExpr::Forward { .. }
            | NExpr::ProjectValue { .. }
            | NExpr::StructuralRepack { .. }
            | NExpr::CodeRegionRef { .. } => return None,
            NExpr::Const(const_) => ExprStaticFacts::Const(match const_ {
                SConst::Value(value) => runtime_const_value_class(
                    db,
                    type_env,
                    *value,
                    !local_disallows_const_ref_storage(body, dst),
                ),
                SConst::Ref(cref) => {
                    let value = reified_const_ref_value_for_ty(db, body.owner(), *cref, result_ty);
                    runtime_const_value_class(
                        db,
                        type_env,
                        value,
                        !local_disallows_const_ref_storage(body, dst),
                    )
                }
            }),
            NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::ScalarCast { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. } => ExprStaticFacts::DirectClass(
                scalar_class_for_ty_in_env(db, type_env, result_ty).map(RuntimeClass::Scalar),
            ),
            NExpr::ArrayRepeat { ty, .. } => {
                let len = ty.array_len(db).unwrap_or_else(|| {
                    panic!(
                        "array repeat with non-concrete length reached runtime class inference: \
                         {expr:?}"
                    )
                });
                let elem_ty = ty
                    .decompose_ty_app(db)
                    .1
                    .first()
                    .copied()
                    .expect("array element type");
                let field = AggregateMakeFieldStaticFacts {
                    boundary: boundary_spec_for_ty_in_env(
                        db,
                        type_env,
                        elem_ty,
                        AddressSpaceKind::Memory,
                    )
                    .map(|boundary| boundary_sites.stage(boundary)),
                    stored_class: stored_class_for_ty_in_env(db, type_env, elem_ty),
                };
                ExprStaticFacts::AggregateMake(AggregateMakeStaticFacts {
                    direct_class: top_level_class_for_ty_in_env(
                        db,
                        type_env,
                        *ty,
                        AddressSpaceKind::Memory,
                    )
                    .filter(|class| !matches!(class, RuntimeClass::AggregateValue { .. })),
                    ctor: AggregateCtorKind::Aggregate(*ty),
                    fields: vec![field; len],
                })
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
                        stored_class: stored_class_for_ty_in_env(db, type_env, field_ty),
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
                        stored_class: stored_class_for_ty_in_env(db, type_env, field_ty),
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
            NExpr::Load { .. } => ExprStaticFacts::DirectClass(top_level_class_for_ty_in_env(
                db,
                type_env,
                result_ty,
                AddressSpaceKind::Memory,
            )),
            NExpr::Borrow {
                provider, place, ..
            } => {
                let provider_fallback = if runtime_zero_sized_transport_ty(
                    db,
                    result_ty,
                    type_env.scope,
                    type_env.assumptions,
                ) {
                    None
                } else {
                    match place.base {
                        NPlaceBase::Root(root) => match body.normalized.root(root) {
                            Some(hir::analysis::semantic::normalized::NRoot {
                                kind: NRootKind::Provider { binding },
                                ..
                            }) if provider_erases_runtime_root(
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
                        NPlaceBase::CapabilityTarget { .. } => {
                            provider.map(|provider| RuntimeClass::RawAddr {
                                space: address_space_from_provider(provider),
                                target: None,
                            })
                        }
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
                let caller_key = body.owner().key(db);
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub(crate) struct RuntimeVisibleBindingPlan<'db> {
    pub(crate) binding: LocalBinding<'db>,
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
            ProviderKind::InvalidHandle => {
                panic!("invalid effect-handle provider reached MIR lowering")
            }
        })
}

fn provider_root_place_class<'db>(
    db: &'db dyn MirDb,
    value_ty: TyId<'db>,
    provider_class: &RuntimeClass<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> RuntimeClass<'db> {
    provider_class.deref_target().unwrap_or_else(|| {
        stored_class_for_ty_in_env(db, RuntimeTypeEnv::new(scope, assumptions), value_ty)
    })
}

pub(crate) fn runtime_class_for_provider_binding<'db>(
    db: &'db dyn MirDb,
    provider: &ProviderBinding<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeClass<'db>> {
    match provider.semantics.kind {
        ProviderKind::RootObject => top_level_class_for_ty_in_env(
            db,
            RuntimeTypeEnv::new(scope, assumptions),
            provider.provider_ty,
            AddressSpaceKind::Memory,
        ),
        ProviderKind::Handle | ProviderKind::RawAddress => {
            effect_handle_transport_class_for_ty_in_env(
                db,
                RuntimeTypeEnv::new(scope, assumptions),
                provider.provider_ty,
            )
            .or_else(|| {
                Some(provider_class_for_target_in_env(
                    db,
                    RuntimeTypeEnv::new(scope, assumptions),
                    provider.semantics.target_ty,
                    provider_address_space_to_runtime(provider.semantics.address_space?),
                ))
            })
        }
        ProviderKind::InvalidHandle => None,
    }
}

pub(crate) fn runtime_class_for_effect_binding_provider_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    provider: &ProviderBinding<'db>,
) -> Option<RuntimeClass<'db>> {
    match provider.semantics.kind {
        ProviderKind::RootObject => Some(provider_class_for_target_in_env(
            db,
            env,
            Some(provider.semantics.target_ty.unwrap_or(provider.provider_ty)),
            provider
                .semantics
                .address_space
                .map_or(AddressSpaceKind::Memory, provider_address_space_to_runtime),
        )),
        ProviderKind::Handle | ProviderKind::RawAddress => {
            runtime_class_for_provider_binding(db, provider, env.scope, env.assumptions)
        }
        ProviderKind::InvalidHandle => None,
    }
}

pub(crate) fn runtime_class_for_direct_value_provider_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    provider: &ProviderBinding<'db>,
) -> Option<RuntimeClass<'db>> {
    runtime_class_for_effect_binding_provider_in_env(db, env, provider)
}

fn runtime_class_for_provider_value_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    provider: &ProviderBinding<'db>,
    value_ty: TyId<'db>,
) -> Option<RuntimeClass<'db>> {
    if matches!(provider.semantics.kind, ProviderKind::InvalidHandle) {
        return None;
    }
    let space = match provider.semantics.kind {
        ProviderKind::RootObject => provider
            .semantics
            .address_space
            .map_or(AddressSpaceKind::Memory, provider_address_space_to_runtime),
        ProviderKind::Handle | ProviderKind::RawAddress => {
            provider_address_space_to_runtime(provider.semantics.address_space?)
        }
        ProviderKind::InvalidHandle => unreachable!(),
    };
    effect_handle_transport_class_for_ty_in_env(db, env, provider.provider_ty).or_else(|| {
        Some(provider_class_for_target_in_env(
            db,
            env,
            Some(value_ty),
            space,
        ))
    })
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
        pointee: stored_class_for_ty_in_env(
            db,
            RuntimeTypeEnv::new(scope, assumptions),
            pointee_ty,
        ),
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
    let ty = runtime_repr_ty_in_env(db, RuntimeTypeEnv::new(scope, assumptions), ty);
    if let Some((_, inner)) = ty.as_borrow(db) {
        return runtime_zero_sized_effect_value_is_inert(db, inner, scope, assumptions);
    }
    if let Some((_, inner)) = ty.as_capability(db) {
        return runtime_zero_sized_effect_value_is_inert(db, inner, scope, assumptions);
    }
    if !runtime_zero_sized_ty(db, ty, scope, assumptions) {
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

pub(crate) fn provider_erases_runtime_root<'db>(
    db: &'db dyn MirDb,
    provider: &ProviderBinding<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    if let ProviderSource::ContractField { field: field_id } = provider.source {
        return field_id
            .contract
            .storage_layout(db)
            .values()
            .find(|field| field.field == field_id)
            .is_none_or(|field| {
                field.inline_span == 0
                    && field.cells.iter().all(|cell| cell.allocation.is_none())
                    && field
                        .families
                        .iter()
                        .all(|family| family.allocation.is_none())
            });
    }

    let value_ty = provider.semantics.target_ty.unwrap_or(provider.provider_ty);
    runtime_zero_sized_transport_ty(db, value_ty, scope, assumptions)
        || provider_source_erases_zero_sized_effect_value(
            db,
            provider,
            value_ty,
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
            if provider_erases_runtime_root(db, &provider, env.scope, env.assumptions) {
                return None;
            }
            let class = runtime_class_for_provider_value_ty_in_env(db, env, &provider, value_ty)?;
            if class.span_words(db) == 0 {
                return None;
            }
            let boundary =
                effect_binding_borrow_boundary(db, binding, value_ty, env.scope, env.assumptions);
            let boundary = specialize_effect_binding_boundary_for_class(boundary, &class);
            Some(RuntimeEffectBindingPlan { class, boundary })
        }
        SemanticLocalRole::DirectValue { .. } => {
            if runtime_zero_sized_transport_ty(db, binding_ty, env.scope, env.assumptions) {
                return None;
            }
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
                effect_handle_transport_class_for_ty_in_env(db, env, binding_ty);
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
                effect_handle_transport_class_for_ty_in_env(db, env, binding_ty);
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
            if provider_erases_runtime_root(db, &provider, env.scope, env.assumptions) {
                return None;
            }
            let class = runtime_class_for_provider_value_ty_in_env(db, env, &provider, value_ty)?;
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
            if provider_erases_runtime_root(db, &provider, env.scope, env.assumptions) {
                return None;
            }
            let class = runtime_class_for_provider_value_ty_in_env(db, env, &provider, value_ty)?;
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
    if !matches!(binding, LocalBinding::EffectParam { .. })
        && effect_handle_transport_class_for_ty_in_env(db, env, binding_ty).is_some()
    {
        return top_level_class_for_ty_in_env(db, env, binding_ty, AddressSpaceKind::Memory);
    }
    match semantic.binding_role(db, binding) {
        SemanticLocalRole::Erased => None,
        SemanticLocalRole::DirectValue {
            provenance: ValueProvenance::RootProvider(provider),
        } => runtime_class_for_provider_value_ty_in_env(db, env, &provider, binding_ty),
        SemanticLocalRole::DirectValue { .. } => runtime_exact_class_for_ordinary_binding_in_env(
            db, env, binding, binding_ty,
        )
        .or_else(|| top_level_class_for_ty_in_env(db, env, binding_ty, AddressSpaceKind::Memory)),
        SemanticLocalRole::DirectCarrier {
            provider: Some(provider),
            target_ty,
        } => {
            let binding_handle_class =
                effect_handle_transport_class_for_ty_in_env(db, env, binding_ty);
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
                effect_handle_transport_class_for_ty_in_env(db, env, binding_ty);
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
        } => runtime_class_for_provider_value_ty_in_env(db, env, &provider, value_ty),
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
        } => runtime_class_for_provider_value_ty_in_env(db, env, &provider, value_ty),
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
    if effect_handle_transport_class_for_ty_in_env(db, env, binding_ty).is_some() {
        return None;
    }
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
    let canonical = |ty| runtime_repr_ty_in_env(db, env, ty);
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
        let field = env.body.runtime_operand(field)?;
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
            evaluator.selected_materialized_operand(field)
        };
        let class = selected
            .map(|arg| arg.class)
            .unwrap_or_else(|| field_facts.stored_class.clone());
        field_classes.push(class);
    }
    Some(RuntimeClass::AggregateValue {
        layout: match facts.ctor {
            AggregateCtorKind::Aggregate(ty) => {
                layout_for_aggregate_instance_in_env(env.db, env.type_env(), ty, &field_classes)
            }
            AggregateCtorKind::EnumVariant { enum_ty, variant } => {
                layout_for_enum_variant_instance_in_env(
                    env.db,
                    env.type_env(),
                    enum_ty,
                    variant.0 as usize,
                    &field_classes,
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
        RuntimeVisibleReturnPlan::Exact(class) => {
            Some(evaluator.selected_semantic_operand_for_class(copy_operand(local), class))
        }
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

fn copy_operand(local: SLocalId) -> RuntimeOperand {
    RuntimeOperand {
        local,
        value: None,
        origin: None,
        mode: ReadMode::Copy,
    }
}

fn binding_forwards_runtime_transport<'db>(
    db: &'db dyn MirDb,
    semantic: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
) -> bool {
    let Some(origin) = binding.callable_input_origin(db) else {
        return false;
    };
    if !default_return_class(db, semantic).is_some_and(|class| class.contains_transport(db)) {
        return false;
    }
    semantic
        .key(db)
        .typed_body(db)
        .forwarded_return_sources(db)
        .iter()
        .any(|source| source.origin == origin)
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
    let semantic_binding_ty = semantic.binding_ty(db, binding);
    if effect_handle_transport_class_for_ty_in_env(db, env, semantic_binding_ty).is_some() {
        return boundary_spec_for_ty_in_env(db, env, semantic_binding_ty, AddressSpaceKind::Memory)
            .map(|boundary| match boundary {
                RuntimeBoundarySpec::BorrowLike {
                    pointee,
                    access,
                    mut allow,
                } => {
                    allow.provider_spaces = Box::default();
                    allow.allow_raw_addr = false;
                    RuntimeBoundarySpec::BorrowLike {
                        pointee,
                        access,
                        allow,
                    }
                }
                boundary => boundary,
            })
            .map(|boundary| {
                RuntimeParamPlan::Boundary(runtime_param_boundary(
                    db, typed_body, binding, env, boundary,
                ))
            })
            .unwrap_or(RuntimeParamPlan::Erased);
    }
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
    if runtime_zero_sized_effect_value_is_inert(db, binding_ty, scope, assumptions)
        && !binding_forwards_runtime_transport(db, semantic, binding)
    {
        return RuntimeParamPlan::Erased;
    }
    let interface_ty = runtime_interface_ty_in_env(db, env, binding_ty);
    let repr_ty = runtime_repr_ty_in_env(db, env, binding_ty);
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
    let value = stored_class_for_ty_in_env(db, env, inner);
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
    body: &RuntimeSemanticBody<'db>,
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
            RuntimeTypeEnv::for_semantic(db, body.owner()),
            body,
            body.operand_local(*arg).ok_or_else(|| {
                crate::runtime::LowerError::Unsupported(format!(
                    "runtime trait-call self argument has no representation: caller={caller_key:?} callee={callee_key:?} value={:?}",
                    arg.value,
                ))
            })?,
        ) else {
            return Err(crate::runtime::LowerError::Unsupported(format!(
                "runtime trait-call resolution could not infer the concrete self type: caller={caller_key:?} callee={callee_key:?} local={:?}",
                arg.value,
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
    let Some((impl_func, impl_args)) = resolve_trait_method_instance(
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
    let impl_args = complete_resolved_trait_method_args(
        db,
        impl_func,
        impl_args,
        callee_key.subst(db).generic_args(db),
        concrete_inst.args(db).len(),
    );
    let owner = BodyOwner::Func(impl_func);
    Ok(SemanticInstanceKey::new(
        db,
        owner,
        GenericSubst::new(db, impl_args),
        hir::analysis::semantic::EffectProviderSubst::empty(db),
        ImplEnv::for_resolved_trait_method(db, owner, concrete_inst),
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
    body: &RuntimeSemanticBody<'db>,
    local: SLocalId,
) -> Option<TyId<'db>> {
    let scope = env.scope;
    let assumptions = env.assumptions;
    let normalized = |ty| normalize_runtime_self_ty(db, ty, scope, assumptions);
    let local_data = body.local(local)?;
    let provider = local_data.role.root_provider(&body.locals);
    match &local_data.role {
        SemanticLocalRole::Erased => None,
        SemanticLocalRole::DirectValue { .. } | SemanticLocalRole::DirectCarrier { .. }
            if provider.is_some() =>
        {
            Some(normalized(provider?.provider_ty))
        }
        SemanticLocalRole::PlaceBoundValue { value_ty, .. } => Some(normalized(
            provider
                .and_then(|provider| provider.semantics.target_ty)
                .unwrap_or(*value_ty),
        )),
        SemanticLocalRole::DirectValue { .. } => Some(normalized(local_data.ty)),
        SemanticLocalRole::PlaceCarrier { value_ty, .. } => Some(normalized(*value_ty)),
        SemanticLocalRole::DirectCarrier { target_ty, .. } => Some(normalized(*target_ty)),
    }
}

fn normalize_runtime_self_ty<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let ty = runtime_repr_ty_in_env(db, RuntimeTypeEnv::new(scope, assumptions), ty);
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
    carriers.get(local.index())?.value_class()
}

pub(super) fn local_slot_uses_transport_class(
    mutability: Mutability,
    transport: Option<&RuntimeClass<'_>>,
) -> bool {
    mutability == Mutability::Immutable
        && matches!(
            transport,
            Some(RuntimeClass::Ref {
                kind: RefKind::Const,
                ..
            })
        )
}

fn normalized_place_root_transport_class_in_context<'db>(
    env: BodyEnv<'_, 'db>,
    base: NPlaceBase,
    carriers: &[RuntimeCarrier<'db>],
) -> Option<RuntimeClass<'db>> {
    match base {
        NPlaceBase::CapabilityTarget { carrier } => {
            normalized_value_runtime_class(env, carrier, carriers)
        }
        NPlaceBase::Root(root) => match &env.body.normalized.root(root)?.kind {
            NRootKind::LocalSlot { .. } => {
                let local = env.body.root_local(root)?;
                let root = env.body.normalized.root(root)?;
                let transport = carrier_value_class(local, carriers).or_else(|| {
                    env.local_facts(local)?
                        .root_transport_fallback_class
                        .clone()
                });
                if local_slot_uses_transport_class(root.mutability, transport.as_ref()) {
                    transport
                } else {
                    local_place_root_class(
                        env.with_carriers(carriers),
                        local,
                        env.local(local)?,
                        carriers.get(local.index())?,
                    )
                }
            }
            NRootKind::ParamPlace { .. } => {
                let local = env.body.root_local(root)?;
                carrier_value_class(local, carriers).or_else(|| {
                    env.local_facts(local)?
                        .root_transport_fallback_class
                        .clone()
                })
            }
            NRootKind::Provider { binding } => {
                let actual = env.actual_runtime_visible_root_provider_class(carriers, binding);
                if actual.is_none()
                    && provider_erases_runtime_root(env.db, binding, env.scope(), env.assumptions())
                {
                    return None;
                }
                actual.map(|(_, class)| class).or_else(|| {
                    runtime_class_for_effect_binding_provider_in_env(
                        env.db,
                        env.type_env(),
                        binding,
                    )
                    .or_else(|| {
                        runtime_class_for_direct_value_provider_in_env(
                            env.db,
                            env.type_env(),
                            binding,
                        )
                    })
                })
            }
            NRootKind::CapabilityRepresentation { carrier } => {
                normalized_value_runtime_class(env, *carrier, carriers)
            }
        },
    }
}

fn normalized_place_root_class_in_context<'db>(
    env: BodyEnv<'_, 'db>,
    base: NPlaceBase,
    carriers: &[RuntimeCarrier<'db>],
) -> Option<RuntimeClass<'db>> {
    let cx = env.with_carriers(carriers);
    match base {
        NPlaceBase::CapabilityTarget { carrier } => {
            let carrier_ty = env.body.normalized.value(carrier)?.ty;
            let (_, target_ty) = carrier_ty.as_capability(env.db)?;
            runtime_source_place_class(
                env,
                normalized_value_runtime_class(env, carrier, carriers)?,
                target_ty,
            )
        }
        NPlaceBase::Root(root) => match &env.body.normalized.root(root)?.kind {
            NRootKind::LocalSlot { .. } | NRootKind::ParamPlace { .. } => {
                let local = env.body.root_local(root)?;
                local_place_root_class(cx, local, env.local(local)?, carriers.get(local.index())?)
            }
            NRootKind::Provider { binding } => {
                let value_ty = env.body.normalized.root(root)?.ty;
                let actual = env.actual_runtime_visible_root_provider_class(carriers, binding);
                if actual.is_none()
                    && provider_erases_runtime_root(env.db, binding, env.scope(), env.assumptions())
                {
                    return None;
                }
                if let Some((local, _)) = &actual
                    && let Some(local) = env.local(*local)
                    && local.ty == binding.provider_ty
                    && effect_handle_transport_class_for_ty_in_env(env.db, env.type_env(), local.ty)
                        .is_some()
                {
                    return Some(stored_class_for_ty_in_env(env.db, env.type_env(), local.ty));
                }
                let provider_class = actual.map(|(_, class)| class).or_else(|| {
                    runtime_class_for_effect_binding_provider_in_env(
                        env.db,
                        env.type_env(),
                        binding,
                    )
                    .or_else(|| {
                        runtime_class_for_direct_value_provider_in_env(
                            env.db,
                            env.type_env(),
                            binding,
                        )
                    })
                })?;
                Some(provider_root_place_class(
                    env.db,
                    value_ty,
                    &provider_class,
                    env.scope(),
                    env.assumptions(),
                ))
            }
            NRootKind::CapabilityRepresentation { carrier } => runtime_source_place_class(
                env,
                normalized_value_runtime_class(env, *carrier, carriers)?,
                env.body.normalized.root(root)?.ty,
            ),
        },
    }
}

fn runtime_source_place_class<'db>(
    env: BodyEnv<'_, 'db>,
    class: RuntimeClass<'db>,
    target_ty: TyId<'db>,
) -> Option<RuntimeClass<'db>> {
    match class {
        class @ (RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. }) => Some(class),
        RuntimeClass::Ref { .. }
        | RuntimeClass::RawAddr {
            target: Some(_), ..
        } => class.deref_target(),
        RuntimeClass::RawAddr {
            space,
            target: None,
        } => top_level_class_for_ty_in_env(env.db, env.type_env(), target_ty, space),
    }
}

fn normalized_value_runtime_class<'db>(
    env: BodyEnv<'_, 'db>,
    value: NValueId,
    carriers: &[RuntimeCarrier<'db>],
) -> Option<RuntimeClass<'db>> {
    let value_data = env.body.normalized.value(value)?;
    let local = env.value_local(value)?;
    let fallback = || {
        carrier_value_class(local, carriers).or_else(|| {
            env.local_facts(local)?
                .root_transport_fallback_class
                .clone()
        })
    };
    let NValueDefinition::Statement { block, statement } = value_data.definition else {
        return fallback();
    };
    let NStatementKind::Define { expr, .. } = &env
        .body
        .normalized
        .block(block)?
        .statements
        .get(statement as usize)?
        .kind
    else {
        return fallback();
    };
    match expr {
        NExpr::Forward { src } | NExpr::StructuralRepack { value: src, .. } => {
            normalized_value_runtime_class(env, src.value, carriers)
        }
        NExpr::ProjectValue { value, path } => Some(env.walk_data_path_class(
            env.normalized_value_structural_class(carriers, value.value)?,
            &path.0,
        )),
        NExpr::Load { place, .. } => env.normalized_place_class(carriers, place),
        NExpr::Borrow { place, .. } => env.normalized_place_address_class(carriers, place),
        NExpr::CodeRegionRef { .. }
        | NExpr::Const(_)
        | NExpr::Unary { .. }
        | NExpr::Binary { .. }
        | NExpr::ScalarCast { .. }
        | NExpr::ArrayRepeat { .. }
        | NExpr::AggregateMake { .. }
        | NExpr::EnumMake { .. }
        | NExpr::GetEnumTag { .. }
        | NExpr::IsEnumVariant { .. }
        | NExpr::Call { .. }
        | NExpr::CodeRegionOffset { .. }
        | NExpr::CodeRegionLen { .. } => fallback(),
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
            pointee: stored_class_for_ty_in_env(db, env, target_ty),
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
    let ty = runtime_repr_ty_in_env(db, env, typed_body.binding_ty(db, binding));
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
        return RuntimeClass::object_ref(layout_for_ty_in_env(db, env, ty));
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
            let ty = runtime_repr_ty_in_env(db, env, typed_body.binding_ty(db, binding));
            if binding.is_mut() && ty.as_enum(db).is_some() {
                return RuntimeBoundarySpec::ExactTransport(RuntimeClass::object_ref(
                    layout_for_ty_in_env(db, env, ty),
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
    let repr_ty = runtime_repr_ty_in_env(db, env, ty);
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

fn runtime_abstract_param_ty<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    let ty = runtime_repr_ty_in_env(db, RuntimeTypeEnv::new(scope, assumptions), ty);
    ty.has_param(db) || ty.contains_assoc_ty_of_param(db)
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::{
        analysis::semantic::{
            NEffectArg, NPlace, NPlaceBase, NRootKind, NStatementKind, NTerminatorKind,
            SemanticInstance, SemanticNormalizationFailure, get_or_build_semantic_instance,
            owner_effect_bindings, resolved_provider_binding_for_instance_effect,
            root_semantic_instance_key,
        },
        analysis::ty::ty_check::{BodyOwner, LocalBinding},
    };
    use url::Url;

    use super::super::{
        abi::runtime_declaration_abi_plan,
        arg_selector::RuntimeArgSelector,
        boundary::BoundarySiteAllocator,
        call_input::{
            CompiledCallInputPlan, CompiledEffectArgPlan, compile_call_input_plan_for_semantic,
        },
        realize::{RuntimeArgSource, SelectedRuntimeArg},
    };
    use super::*;
    use crate::runtime::lower::boundary::BoundaryMatcher;
    use crate::runtime::{
        RefKind, RuntimeInterfaceSignature, RuntimeLocalRoot,
        lower::{
            infer::LocalStateInferer,
            interface::{runtime_param_locals, runtime_param_plans, runtime_visible_binding_plans},
            returns::declaration_runtime_return_class,
            semantic_body::RuntimeSemanticBody,
        },
        package::runtime_instance_for_semantic,
        package::runtime_instance_for_semantic_with_visible_param_overrides,
    };

    fn normalize_semantic_body<'db>(
        db: &'db DriverDataBase,
        instance: SemanticInstance<'db>,
    ) -> Result<RuntimeSemanticBody<'db>, SemanticNormalizationFailure<'db>> {
        RuntimeSemanticBody::admitted(db, instance)
    }

    fn call_input_plan_for_test<'db>(
        db: &'db DriverDataBase,
        body: &RuntimeSemanticBody<'db>,
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
    fn runtime_handle_preservation_keeps_mut_owned_aggregate_param_by_value() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(
            "file:///runtime_handle_preservation_keeps_mut_owned_aggregate_param_by_value.fe",
        )
        .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!(
                    "../../../../fe/tests/fixtures/fe_test/mut_owned_aggregate_param_slot_carrier.fe"
                )
                .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);

        // `permute` takes `mut _ s: own [u256; 3]`, projects it (`s[0]`), reassigns it whole
        // (`s = [..]`), and returns it. That demands projectable owned storage, which must be
        // supplied by a slot root rather than by widening the by-value parameter into an
        // object reference. A "fix" that widened the ABI would keep the behavioral fe_test
        // fixture green while silently changing the calling convention; these assertions fail
        // loudly instead.
        let semantic = semantic_instance_for_named_func(&db, top_mod, "permute");
        let instance = runtime_instance_for_semantic(&db, semantic);
        let signature = instance.interface_signature(&db);
        let body = instance.body(&db);

        let param = signature
            .params
            .first()
            .expect("permute has one runtime-visible parameter");

        // The interface contract stays a by-value aggregate: callers pass `[u256; 3]`
        // directly, never an object handle.
        assert!(
            matches!(param.class, RuntimeClass::AggregateValue { .. }),
            "mut-owned aggregate param must keep a by-value aggregate ABI class, got:\n{:#?}",
            param.class,
        );

        // The parameter local's carrier is the calling-convention handle; it must stay
        // exactly equal to the signature class (the equality the runtime verifier enforces,
        // whose failure surfaces as SlotCarrierMismatch).
        let carrier = body
            .value_class(param.local)
            .unwrap_or_else(|| panic!("param local {:?} should keep a value carrier", param.local));
        assert_eq!(
            carrier, &param.class,
            "param carrier must equal the signature param class, not be upgraded to an object ref",
        );

        // Owned storage for the mutation/reassignment comes from a slot root over the same
        // aggregate, confirming the demand is met without touching the carrier.
        let root = &body.local(param.local).expect("param local exists").root;
        assert!(
            matches!(
                root,
                RuntimeLocalRoot::Slot(RuntimeClass::AggregateValue { .. })
            ),
            "projectable owned storage must come from an aggregate slot root, got:\n{root:#?}",
        );
    }

    #[test]
    fn runtime_handle_preservation_keeps_mut_owned_enum_param_as_object_ref() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(
            "file:///runtime_handle_preservation_keeps_mut_owned_enum_param_as_object_ref.fe",
        )
        .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!("../../../../fe/tests/fixtures/fe_test/if_let_while_let.fe")
                    .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);

        // Mutable enum parameters deliberately use an object-reference ABI class. The
        // unrooted-read optimization must not rewrite that signature-pinned carrier to an
        // aggregate value merely because the `while let` pattern reads it by place.
        let semantic = semantic_instance_for_named_func(&db, top_mod, "sum_descending_while_let");
        let instance = runtime_instance_for_semantic(&db, semantic);
        let signature = instance.interface_signature(&db);
        let body = instance.body(&db);
        let param = signature
            .params
            .first()
            .expect("sum_descending_while_let has one runtime-visible parameter");

        assert!(
            matches!(
                param.class,
                RuntimeClass::Ref {
                    kind: RefKind::Object,
                    ..
                }
            ),
            "mutable enum parameter must keep its object-reference ABI class, got:\n{:#?}",
            param.class,
        );

        let carrier = body
            .value_class(param.local)
            .unwrap_or_else(|| panic!("param local {:?} should keep a value carrier", param.local));
        assert_eq!(
            carrier, &param.class,
            "param carrier must equal the signature param class, not be rewritten as an aggregate value",
        );

        let root = &body.local(param.local).expect("param local exists").root;
        assert!(
            matches!(
                root,
                RuntimeLocalRoot::Ref(RuntimeClass::Ref {
                    kind: RefKind::Object,
                    ..
                })
            ),
            "mutable enum parameter must retain an object-reference root, got:\n{root:#?}",
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
        let inferred = LocalStateInferer::new(
            env,
            params,
            &runtime_param_locals(&db, semantic, &normalized.source, params),
        )
        .run();
        let return_plan = desired_runtime_return_plan(&db, semantic);
        let selected_returns = normalized
            .normalized
            .blocks
            .iter()
            .filter_map(|block| match &block.terminator.kind {
                NTerminatorKind::Return(Some(value)) => normalized.operand_local(*value),
                NTerminatorKind::Goto(_)
                | NTerminatorKind::Branch { .. }
                | NTerminatorKind::MatchEnum { .. }
                | NTerminatorKind::Assert { .. }
                | NTerminatorKind::Return(None) => None,
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
    fn specialized_grant_callee_erases_self_after_root_assignment() {
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
        let param_plans = runtime_param_plans(&db, semantic);
        let plans = runtime_visible_binding_plans(&db, semantic);
        let abi = runtime_declaration_abi_plan(&db, callee.key(&db));
        let signature = callee.interface_signature(&db);

        assert_eq!(
            plans.len(),
            2,
            "specialized grant callee should expose only its two non-ZST arguments after the assigned root becomes concrete:\nself_role={self_role:#?}\nparam_plans={param_plans:#?}\nplans={plans:#?}\nsignature={signature:#?}"
        );
        assert!(
            matches!(param_plans.first(), Some(RuntimeParamPlan::Erased)),
            "specialized grant receiver should erase once its nested layout root is a concrete assigned literal:\nself_role={self_role:#?}\nparam_plans={param_plans:#?}\nplans={plans:#?}\nsignature={signature:#?}"
        );
        assert_eq!(
            abi.visible_params.len(),
            2,
            "specialized grant visible ABI should omit the inert receiver:\nself_role={self_role:#?}\nparam_plans={param_plans:#?}\nplans={plans:#?}\nabi={abi:#?}"
        );
        assert_eq!(
            abi.evidence_params.len(),
            1,
            "the declared receiver layout must remain an explicit evidence parameter:\nabi={abi:#?}"
        );
        assert_eq!(signature, abi.signature());
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
        let normalized = normalize_semantic_body(&db, semantic)
            .unwrap_or_else(|err| panic!("failed to normalize takes_empty: {err:?}"));

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
            runtime_param_locals(&db, semantic, &normalized.source, params).len(),
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
            // The wrapped value plus the zero-sized `TSlot<bool>` lock field.
            assert_eq!(
                layout_data.fields.len(),
                2,
                "guarded_balances Mutex layout should expose the wrapped value and lock fields for arm {arm_idx}; binding_ty={}",
                binding_ty.pretty_print(&db),
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
        let protected = get_or_build_semantic_instance(
            &db,
            root_semantic_instance_key(
                &db,
                BodyOwner::ContractRecvArm {
                    contract,
                    recv_idx: 0,
                    arm_idx: 1,
                },
            )
            .unwrap_or_else(|err| panic!("failed to build Protected semantic key: {err:?}")),
        );
        let protected_body = normalize_semantic_body(&db, protected)
            .unwrap_or_else(|err| panic!("failed to normalize Protected: {err:?}"));
        let try_lock = protected_body
            .normalized
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .find_map(|stmt| {
                let NStatementKind::Define {
                    expr: NExpr::Call { callee, .. },
                    ..
                } = &stmt.kind
                else {
                    return None;
                };
                let BodyOwner::Func(func) = callee.key.owner(&db) else {
                    return None;
                };
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "try_lock")
                    .then(|| get_or_build_semantic_instance(&db, callee.key))
            })
            .expect("Protected should call Mutex::try_lock");
        let try_lock_return_sources = try_lock
            .key(&db)
            .typed_body(&db)
            .forwarded_return_sources(&db);
        assert!(
            try_lock_return_sources.iter().any(|source| {
                source.origin
                    == hir::analysis::ty::const_ty::CallableInputLayoutHoleOrigin::Receiver
            }),
            "try_lock must retain its receiver among partial forwarded return sources:\n{try_lock_return_sources:#?}",
        );
        assert!(
            runtime_param_plans(&db, try_lock)
                .first()
                .is_some_and(|plan| !matches!(plan, RuntimeParamPlan::Erased)),
            "try_lock must keep its receiver runtime-visible for the Some borrow payload:\nplans={:#?}",
            runtime_param_plans(&db, try_lock),
        );
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
        let inferred = LocalStateInferer::new(
            env,
            params,
            &runtime_param_locals(&db, semantic, &normalized.source, params),
        )
        .run();
        let mut checked_calls = Vec::new();
        for (block_idx, block) in normalized.normalized.blocks.iter().enumerate() {
            for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                let NStatementKind::Define { expr, .. } = &stmt.kind else {
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
                let receiver_plan = runtime_param_plans(&db, call_facts.semantic).first();
                if name == "lock" {
                    assert!(
                        receiver_plan
                            .is_some_and(|plan| { !matches!(plan, RuntimeParamPlan::Erased) }),
                        "borrow-returning `lock` must keep its forwarded receiver transport runtime-visible:\nplans={:#?}",
                        runtime_param_plans(&db, call_facts.semantic),
                    );
                } else {
                    assert!(
                        matches!(receiver_plan, Some(RuntimeParamPlan::Erased)),
                        "non-forwarding `{name}` must erase its zero-width receiver and use only hidden layout roots:\nplans={:#?}",
                        runtime_param_plans(&db, call_facts.semantic),
                    );
                }
                let receiver = args.first().and_then(|arg| normalized.operand_local(*arg));
                let (receiver_actual, receiver_materialized, selected, selected_classes) = {
                    let mut class_cache = InferClassCache::new(normalized.locals.len());
                    let mut evaluator =
                        RuntimeArgSelector::new(env, &inferred.carriers, Some(&mut class_cache));
                    let receiver_actual =
                        receiver.and_then(|local| evaluator.selected_actual_value(local));
                    let receiver_materialized = receiver.and_then(|local| {
                        evaluator.selected_materialized_operand(copy_operand(local))
                    });
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
                let selected_return = declaration_runtime_return_class(
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
                        "storage-specialized `lock` should keep a storage transport return:\nreceiver_actual={receiver_actual:#?}\nreceiver_materialized={receiver_materialized:#?}\nselected_return={selected_return:#?}\nselected={selected:#?}",
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
    fn concrete_zero_width_storage_map_effect_args_erase_from_runtime_calls() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(
            "file:///concrete_zero_width_storage_map_effect_args_erase_from_runtime_calls.fe",
        )
        .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!("../../../../codegen/tests/fixtures/storage_map_contract.fe")
                    .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let runtime = semantic_instance_for_named_func(&db, top_mod, "runtime");
        let mut owners = vec![("runtime".to_string(), runtime)];
        let mut checked_calls = 0;
        let mut owner_idx = 0;

        while owner_idx < owners.len() {
            let (owner_name, semantic) = owners[owner_idx].clone();
            owner_idx += 1;
            let instance = runtime_instance_for_semantic(&db, semantic);
            let normalized = normalize_semantic_body(&db, semantic)
                .unwrap_or_else(|err| panic!("failed to normalize {owner_name}: {err:?}"));
            let facts = BodyStaticFacts::new(&db, &normalized);
            let env = BodyEnv::new(&db, &normalized, &facts);
            let params = instance.key(&db).params(&db);
            let inferred = LocalStateInferer::new(
                env,
                params,
                &runtime_param_locals(&db, semantic, &normalized.source, params),
            )
            .run();

            for (block_idx, block) in normalized.normalized.blocks.iter().enumerate() {
                for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                    let NStatementKind::Define { expr, .. } = &stmt.kind else {
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
                    let is_storage_map_call = matches!(
                        name.as_str(),
                        "new"
                            | "get"
                            | "set"
                            | "get_unchecked"
                            | "set_unchecked"
                            | "get_balance"
                            | "set_balance"
                            | "get_allowance"
                            | "set_allowance"
                            | "transfer"
                    );
                    if !is_storage_map_call {
                        continue;
                    }
                    let ExprStaticFacts::Call(call_facts) =
                        facts.expr(block_idx, stmt_idx).unwrap_or_else(|| {
                            panic!("missing staged call facts for {block_idx}:{stmt_idx}")
                        })
                    else {
                        panic!("{name} expression should keep staged call facts");
                    };
                    if func.is_method(&db) {
                        assert!(
                            matches!(
                                runtime_param_plans(&db, call_facts.semantic).first(),
                                Some(RuntimeParamPlan::Erased)
                            ),
                            "zero-width StorageMap receiver should erase independently of its explicit runtime layout-root ABI for `{owner_name}` -> `{name}`:\nplans={:#?}",
                            runtime_param_plans(&db, call_facts.semantic),
                        );
                    }
                    let input_plan =
                        call_input_plan_for_test(&db, &normalized, call_facts, effect_args);

                    assert!(
                        input_plan
                            .effect_plans
                            .iter()
                            .all(|plan| matches!(plan, CompiledEffectArgPlan::Erased)),
                        "StorageMap effect arg should erase for `{owner_name}` -> `{name}`:\nargs={args:#?}\neffect_args={effect_args:#?}\ninput_plan={input_plan:#?}",
                    );

                    let mut class_cache = InferClassCache::new(normalized.locals.len());
                    let selected =
                        RuntimeArgSelector::new(env, &inferred.carriers, Some(&mut class_cache))
                            .with_concrete_roots(&inferred.roots)
                            .selected_call_inputs(args, effect_args, &input_plan);
                    for selected_arg in &selected {
                        let erased_place = selected_erased_place_root(
                            &normalized,
                            &inferred.carriers,
                            &inferred.roots,
                            selected_arg,
                        );
                        assert!(
                            erased_place.is_none(),
                            "selected runtime input for `{owner_name}` -> `{name}` would lower an erased place root:\nerased_place={erased_place:#?}\nselected={selected:#?}",
                        );
                    }
                    if !owners
                        .iter()
                        .any(|(_, existing)| existing.key(&db) == call_facts.semantic.key(&db))
                    {
                        owners.push((name.to_string(), call_facts.semantic));
                    }
                    checked_calls += 1;
                }
            }
        }

        assert!(
            checked_calls >= 5,
            "runtime should contain all StorageMap helper calls; checked {checked_calls}"
        );
    }

    #[test]
    fn concrete_zero_width_storage_packed_array_effect_args_erase_from_runtime_calls() {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse(
            "file:///concrete_zero_width_storage_packed_array_effect_args_erase_from_runtime_calls.fe",
        )
        .unwrap();
        db.workspace().touch(
            &mut db,
            file_url.clone(),
            Some(
                include_str!("../../../../codegen/tests/fixtures/storage_packed_array.fe")
                    .to_string(),
            ),
        );
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        let owners = [
            ("get_status", 1usize),
            ("set_status", 2usize),
            ("search_status", 3usize),
        ];
        let mut checked_calls = 0;

        for (owner_name, expected_params) in owners {
            let semantic = semantic_instance_for_named_func(&db, top_mod, owner_name);
            let instance = runtime_instance_for_semantic(&db, semantic);
            let signature = instance.interface_signature(&db);
            assert_eq!(
                signature.params.len(),
                expected_params,
                "`{owner_name}` should not expose its StoragePackedArray effect as a runtime param:\n{signature:#?}",
            );
            assert!(
                runtime_visible_binding_plans(&db, semantic)
                    .iter()
                    .all(|entry| !matches!(entry.binding, LocalBinding::EffectParam { .. })),
                "`{owner_name}` should not keep zero-width StoragePackedArray effects runtime-visible",
            );

            let normalized = normalize_semantic_body(&db, semantic)
                .unwrap_or_else(|err| panic!("failed to normalize {owner_name}: {err:?}"));
            let facts = BodyStaticFacts::new(&db, &normalized);
            let env = BodyEnv::new(&db, &normalized, &facts);
            let params = instance.key(&db).params(&db);
            let inferred = LocalStateInferer::new(
                env,
                params,
                &runtime_param_locals(&db, semantic, &normalized.source, params),
            )
            .run();

            for (block_idx, block) in normalized.normalized.blocks.iter().enumerate() {
                for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                    let NStatementKind::Define { expr, .. } = &stmt.kind else {
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
                    if !matches!(name.as_str(), "get" | "set" | "search") {
                        continue;
                    }
                    let ExprStaticFacts::Call(call_facts) =
                        facts.expr(block_idx, stmt_idx).unwrap_or_else(|| {
                            panic!("missing staged call facts for {block_idx}:{stmt_idx}")
                        })
                    else {
                        panic!("{name} expression should keep staged call facts");
                    };
                    let input_plan =
                        call_input_plan_for_test(&db, &normalized, call_facts, effect_args);
                    assert!(
                        input_plan
                            .effect_plans
                            .iter()
                            .all(|plan| matches!(plan, CompiledEffectArgPlan::Erased)),
                        "StoragePackedArray effect arg should erase for `{owner_name}` -> `{name}`:\nargs={args:#?}\neffect_args={effect_args:#?}\ninput_plan={input_plan:#?}",
                    );

                    let mut class_cache = InferClassCache::new(normalized.locals.len());
                    let selected =
                        RuntimeArgSelector::new(env, &inferred.carriers, Some(&mut class_cache))
                            .with_concrete_roots(&inferred.roots)
                            .selected_call_inputs(args, effect_args, &input_plan);
                    for selected_arg in &selected {
                        let erased_place = selected_erased_place_root(
                            &normalized,
                            &inferred.carriers,
                            &inferred.roots,
                            selected_arg,
                        );
                        assert!(
                            erased_place.is_none(),
                            "selected runtime input for `{owner_name}` -> `{name}` would lower an erased place root:\nerased_place={erased_place:#?}\nselected={selected:#?}",
                        );
                    }
                    checked_calls += 1;
                }
            }

            let _ = instance.body(&db);
        }

        assert_eq!(
            checked_calls, 3,
            "StoragePackedArray helpers should contain get/set/search calls; checked {checked_calls}",
        );
    }

    fn selected_erased_place_root<'db>(
        body: &RuntimeSemanticBody<'db>,
        carriers: &[RuntimeCarrier<'db>],
        roots: &[RuntimeLocalRoot<'db>],
        selected: &SelectedRuntimeArg<'db>,
    ) -> Option<SLocalId> {
        let local = match &selected.source {
            RuntimeArgSource::PlaceAddress(place, _)
            | RuntimeArgSource::PlaceValue(place, _)
            | RuntimeArgSource::ValueExtract { place, .. } => place_root_local(body, place)?,
            RuntimeArgSource::SemanticPlaceAddress(local, _) => *local,
            RuntimeArgSource::SemanticOperand(_)
            | RuntimeArgSource::DirectValueMaterialization { .. }
            | RuntimeArgSource::RuntimeValue(_)
            | RuntimeArgSource::HandleLikeValue(_)
            | RuntimeArgSource::AggregateFromRuntimeSource(_)
            | RuntimeArgSource::Placeholder(_) => return None,
        };
        matches!(carriers.get(local.index()), Some(RuntimeCarrier::Erased))
            .then_some(local)
            .filter(|local| {
                matches!(
                    roots.get(local.index()),
                    Some(RuntimeLocalRoot::None) | None
                )
            })
    }

    fn place_root_local<'db>(
        body: &RuntimeSemanticBody<'db>,
        place: &NPlace<'db>,
    ) -> Option<SLocalId> {
        match place.base {
            NPlaceBase::CapabilityTarget { carrier } => body.value_local(carrier),
            NPlaceBase::Root(root) => match body.normalized.root(root).map(|root| &root.kind) {
                Some(NRootKind::ParamPlace { .. } | NRootKind::LocalSlot { .. }) => {
                    body.root_local(root)
                }
                Some(NRootKind::Provider { .. } | NRootKind::CapabilityRepresentation { .. })
                | None => None,
            },
        }
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
        let inferred = LocalStateInferer::new(
            env,
            params,
            &runtime_param_locals(&db, semantic, &normalized.source, params),
        )
        .run();
        let (call_dst, args, effect_args, call_facts) = normalized
            .normalized
            .blocks
            .iter()
            .enumerate()
            .find_map(|(block_idx, block)| {
                block
                    .statements
                    .iter()
                    .enumerate()
                    .find_map(|(stmt_idx, stmt)| {
                        let NStatementKind::Define { result, expr } = &stmt.kind else {
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
                        Some((
                            normalized.value_local(*result)?,
                            args.clone(),
                            effect_args.clone(),
                            call_facts.clone(),
                        ))
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
        let lowered_return_class = declaration_runtime_return_class(&db, lowered_take.key(&db));

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
        let inferred = LocalStateInferer::new(
            env,
            params,
            &runtime_param_locals(&db, semantic, &normalized.source, params),
        )
        .run();
        let (args, effect_args, call_facts) = normalized
            .normalized
            .blocks
            .iter()
            .enumerate()
            .find_map(|(block_idx, block)| {
                block
                    .statements
                    .iter()
                    .enumerate()
                    .find_map(|(stmt_idx, stmt)| {
                        let NStatementKind::Define { expr, .. } = &stmt.kind else {
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
