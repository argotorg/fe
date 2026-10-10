//! One fixed point for structural holders, storage contents, and loan relations.
use super::validity::NativeValidity;
use crate::analysis::semantic::diagnostics::{
    BlockedSemanticBody, SemanticDiagnostic, SemanticDiagnosticKind, SemanticDiagnosticSpan,
    SemanticNormalizationFailure, normalized_body_internal_diag,
};
#[cfg(feature = "borrowck-profile")]
use crate::analysis::{semantic::capability::profile::ProfileScope, ty::ty_check::BodyOwner};
use std::{
    cell::{OnceCell, RefCell},
    collections::BTreeMap,
    slice,
};

use cranelift_entity::EntityRef;
use num_traits::ToPrimitive;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        FieldIndex, SConst, SemConstScalar, SemConstValue, SemOrigin, SemanticInstance,
        capability::{
            birth::AllocationBirth,
            external::{ExternalSource, MemoryOffset},
            guard::{ChoiceKey, Guard, ValueOccurrence},
            handle::{AddressOccurrence, OpaqueHandleContract, OpaqueHandleRef, OpaqueWriteSite},
            index::{BinderScope, IndexExpr},
            loan::{CapabilityRef, LoanRef},
            opaque::OpaqueWrite,
            path::{Projection, RegionPath, StructuralPath},
            region::{ProviderRegionId, RegionRoot, RegionSet, SymbolicPlace},
            repack::ReferentRepackId,
            shape::{ShapeChildren, ShapeError, ShapeId, capability_shape},
            source::SourceExpr,
            state::{BorrowState, CapabilityValue},
            value::Guarded,
        },
        ctfe::eval_const_ref,
        definite_assignment::literal_bool_cond,
        get_or_build_semantic_instance,
        normalized::{
            HandleOrigin, NBlockId, NDataPath, NDataProjection, NExpr, NIndex, NOperand, NPlace,
            NPlaceBase, NRootKind, NStatement, NStatementId, NStatementKind, NSuccessor,
            NTerminatorKind, NValueDefinition, NValueId, NormalizedBody, ReadMode,
            literal_allocation, normalize_semantic_body,
        },
    },
    ty::ty_def::TyId,
};

use super::{
    access::ResolvedOperation,
    boundary::resolve_boundary_requirements,
    events::ConflictAnalysis,
    inventory::{Inventory, unbounded_referents_diag, unresolved_effect_handle_diag},
    ir::{BoundaryRequirement, ExecutableBlock, ExecutableControlFlow, PendingSemanticValidation},
    loop_certificate::{FrontierCandidate, PrefixCertificate},
    scalar::{CONDITION_BUDGET, ScalarDemand},
    summary::CallSummary,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum BorrowSummaryMode {
    Final,
    Provisional,
}

#[derive(Clone, Debug)]
pub(super) struct Resolution<'db> {
    pub invalidated: NativeValidity<'db>,
    pub region: RegionSet<'db>,
    pub parents: Vec<Guarded<'db, LoanRef<'db>>>,
    /// Permissions used to traverse containers, distinct from the final referent's parents.
    pub traversed: Vec<Guarded<'db, LoanRef<'db>>>,
}

impl<'db> Resolution<'db> {
    pub fn empty(scope: &BinderScope) -> Self {
        Self {
            invalidated: NativeValidity::default(),
            region: RegionSet::empty(scope),
            parents: Vec::new(),
            traversed: Vec::new(),
        }
    }
}

pub(super) struct Borrowck<'db> {
    pub db: &'db dyn HirAnalysisDb,
    pub instance: SemanticInstance<'db>,
    pub body: NormalizedBody<'db>,
    pub(super) scalar: ScalarDemand<'db>,
    /// Loops whose frontier shape can support a certified range.
    pub(super) frontiers: Vec<FrontierCandidate>,
    pub(super) prefix_certificates: Vec<PrefixCertificate<'db>>,
    pub(super) failed_prefix_certificates: bool,
    pub inventory: Inventory<'db>,
    pub summary_mode: BorrowSummaryMode,
    pub calls: BTreeMap<NValueId, CallSummary<'db>>,
    pub(super) recursive_calls: FxHashSet<NValueId>,
    /// Published only after the joint structural/loan fixed point converges.
    pub before: Vec<Vec<BorrowState<'db>>>,
    pub terminal: Vec<Option<BorrowState<'db>>>,
    pub operations: Vec<Vec<ResolvedOperation<'db>>>,
    pub boundary_requirements:
        Option<Result<Vec<BoundaryRequirement<'db>>, SemanticDiagnostic<'db>>>,
    /// Published with the converged fixed point, like `boundary_requirements`.
    pub(super) conflicts: Option<ConflictAnalysis<'db>>,
    pub blocked: Option<BlockedSemanticBody<'db>>,
    pub pending: PendingSemanticValidation<'db>,
    pub validation_dependencies: Vec<Vec<bool>>,
    pub loan_facts_changed: bool,
    pub storage_facts_changed: bool,
    /// Invalidates call-local source resolutions when their inventory changes.
    pub(super) source_generation: usize,
    /// Guarded capability regions at one `source_generation`; loans change only
    /// with the generation.
    capability_regions: RefCell<(
        usize,
        FxHashMap<Guarded<'db, CapabilityRef<'db>>, RegionSet<'db>>,
    )>,
    /// The availability diagnostic of the published states, which the summary
    /// and the local check would otherwise each recompute.
    pub(super) availability_diagnostic: OnceCell<Option<SemanticDiagnostic<'db>>>,
}

impl<'db> Borrowck<'db> {
    pub fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
    ) -> Result<Self, SemanticNormalizationFailure<'db>> {
        let body = normalize_semantic_body(db, instance)?.body;
        Self::new_with_body(db, instance, body, BorrowSummaryMode::Final).map_err(Into::into)
    }

    pub fn new_with_body(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: NormalizedBody<'db>,
        summary_mode: BorrowSummaryMode,
    ) -> Result<Self, SemanticDiagnostic<'db>> {
        let inventory = Inventory::new(db, &body).map_err(|error| {
            let message = match error {
                ShapeError::UnboundedReferents(head) => {
                    return unbounded_referents_diag(db, instance, body.template_owner, head);
                }
                ShapeError::UnresolvedCapability(ty) => {
                    return unresolved_effect_handle_diag(db, instance, body.template_owner, ty);
                }
                error => format!("invalid capability inventory: {error:?}"),
            };
            normalized_body_internal_diag(
                db,
                instance,
                &body,
                SemOrigin::Body(body.template_owner),
                message,
            )
        })?;
        Ok(Self {
            db,
            instance,
            before: vec![Vec::new(); body.blocks.len()],
            terminal: vec![None; body.blocks.len()],
            operations: vec![Vec::new(); body.blocks.len()],
            boundary_requirements: None,
            conflicts: None,
            validation_dependencies: body
                .blocks
                .iter()
                .map(|block| vec![false; block.statements.len() + 1])
                .collect(),
            body,
            scalar: ScalarDemand::default(),
            frontiers: Vec::new(),
            prefix_certificates: Vec::new(),
            failed_prefix_certificates: false,
            inventory,
            summary_mode,
            calls: BTreeMap::new(),
            recursive_calls: FxHashSet::default(),
            blocked: None,
            pending: PendingSemanticValidation::default(),
            loan_facts_changed: false,
            storage_facts_changed: false,
            source_generation: 0,
            capability_regions: RefCell::default(),
            availability_diagnostic: OnceCell::new(),
        })
    }

    pub fn shape(&self, ty: TyId<'db>) -> Result<ShapeId<'db>, SemanticDiagnostic<'db>> {
        capability_shape(
            self.db,
            self.instance
                .key(self.db)
                .impl_env(self.db)
                .normalization_scope(self.db),
            self.instance.assumptions(self.db),
            ty,
        )
        .map_err(|error| {
            self.internal_diag(
                SemOrigin::Body(self.body.template_owner),
                format!("invalid capability shape: {error:?}"),
            )
        })
    }

    pub fn path(&self, path: &NDataPath) -> RegionPath<IndexExpr<'db>> {
        RegionPath::new(
            path.iter()
                .map(|projection| match projection {
                    NDataProjection::Field(field) => Projection::Field(*field),
                    NDataProjection::VariantField { variant, field } => Projection::VariantField {
                        variant: *variant,
                        field: *field,
                    },
                    NDataProjection::Index(NIndex::Const(index)) => {
                        Projection::Index(IndexExpr::Const(*index))
                    }
                    NDataProjection::Index(NIndex::Value(value)) => {
                        Projection::Index(self.index(*value))
                    }
                })
                .collect::<Vec<_>>(),
        )
    }

    pub fn index(&self, value: NValueId) -> IndexExpr<'db> {
        let Some((_, expr)) = self.body.defining_expr(value) else {
            return IndexExpr::Runtime(value);
        };
        match expr {
            NExpr::Const(constant) => {
                let constant = match constant {
                    SConst::Value(value) => Some(value.value()),
                    SConst::Ref(reference) => eval_const_ref(self.db, *reference).into_ready(),
                    SConst::Description(_) | SConst::Evidence(_) | SConst::Invalid(_) => None,
                };
                if let Some(constant) = constant
                    && let SemConstValue::Scalar {
                        value: SemConstScalar::Int { value: integer },
                        ..
                    } = constant.value(self.db)
                    && let Some(integer) = integer.to_usize()
                {
                    return IndexExpr::Const(integer);
                }
                IndexExpr::Runtime(value)
            }
            NExpr::Forward { src } => self.index(src.value),
            NExpr::ScalarCast { value: source, to }
                if self.lossless_scalar_cast(source.value, *to) =>
            {
                self.index(source.value)
            }
            NExpr::Load { place, .. } if place.path.is_empty() && place.ty.is_integral(self.db) => {
                match place.base {
                    NPlaceBase::CapabilityTarget { carrier }
                        if self.body.values[carrier.index()]
                            .ty
                            .as_view(self.db)
                            .is_some()
                            && matches!(
                                self.body.values[carrier.index()].definition,
                                NValueDefinition::EntryParam { .. }
                            ) =>
                    {
                        self.index(carrier)
                    }
                    _ => IndexExpr::Runtime(value),
                }
            }
            _ => IndexExpr::Runtime(value),
        }
    }

    pub fn resolve_region(&self, state: &BorrowState<'db>, place: &NPlace<'db>) -> RegionSet<'db> {
        let path = self.path(&place.path);
        match place.base {
            NPlaceBase::Root(root) => RegionSet::singleton(
                &BinderScope::default(),
                self.inventory.roots[root.index()].clone(),
                path,
            )
            .with_guard(state.guard()),
            NPlaceBase::CapabilityTarget { carrier } => {
                state.referent_region(self.db, carrier, &path, &self.inventory.loans)
            }
        }
    }

    /// The region a capability entry designates under its guard.
    pub(super) fn capability_region(
        &self,
        entry: &Guarded<'db, CapabilityRef<'db>>,
    ) -> RegionSet<'db> {
        let mut cache = self.capability_regions.borrow_mut();
        let (generation, regions) = &mut *cache;
        if *generation != self.source_generation {
            *generation = self.source_generation;
            regions.clear();
        }
        regions
            .entry(entry.clone())
            .or_insert_with(|| {
                let guards = self.inventory.values.guards();
                entry
                    .payload
                    .region(self.db, &self.inventory.loans, entry.guard.scope())
                    .with_guard(&entry.guard)
                    .share_guards(|guard| guards.borrow_mut().share(guard))
            })
            .clone()
    }

    pub fn resolve_capability(&self, value: &CapabilityValue<'db>) -> Resolution<'db> {
        let mut result = Resolution::empty(value.scope());
        result.region = RegionSet::union_all(
            value.scope(),
            value.direct().iter().filter_map(|entry| {
                let region = self.capability_region(entry);
                if matches!(entry.payload, CapabilityRef::Invalidated { .. }) {
                    result.invalidated |= NativeValidity::from_region(&region);
                    return None;
                }
                result.parents.extend(entry.payload.authority(&entry.guard));
                Some(region.close_existentials(value.scope()))
            }),
        );
        result
    }

    pub fn resolve_place(&self, state: &BorrowState<'db>, place: &NPlace<'db>) -> Resolution<'db> {
        let mut resolved = match place.base {
            NPlaceBase::Root(_) => Resolution::empty(state.guard().scope()),
            NPlaceBase::CapabilityTarget { carrier } => {
                self.resolve_capability(state.value(carrier))
            }
        };
        resolved.region = self.resolve_region(state, place);
        resolved
    }

    pub fn read_region(
        &mut self,
        state: &BorrowState<'db>,
        region: &RegionSet<'db>,
        shape: ShapeId<'db>,
        occurrence: ValueOccurrence,
        origin: SemOrigin<'db>,
    ) -> Result<CapabilityValue<'db>, SemanticDiagnostic<'db>> {
        // A callee summary can follow a typed cell before its pointer becomes
        // a caller SSA value. Discover that storage on demand as well as when
        // transferring values; the fixed point then replays all earlier writes.
        let sources: Vec<_> = region
            .clauses()
            .iter()
            .filter_map(|clause| {
                let RegionRoot::External(source) = &clause.payload.root else {
                    return None;
                };
                (!state
                    .storage_coverage(&clause.payload.root, clause.guard.scope())
                    .complete(&clause.guard))
                .then(|| (source.clone(), clause.guard.scope().clone()))
            })
            .collect();
        let completed = if shape.contains_capability(self.db) && !sources.is_empty() {
            let mut completed = state.clone();
            self.ensure_storage(&mut completed, sources, origin)?;
            Some(completed)
        } else {
            None
        };
        let state = completed.as_ref().unwrap_or(state);
        state
            .read_region(
                self.db,
                &mut self.inventory.values,
                region,
                shape,
                occurrence,
            )
            .map_err(|error| {
                self.internal_diag(origin, format!("unresolved capability contents: {error:?}"))
            })
    }

    pub fn opaque_write(&self, statement: NStatementId) -> OpaqueWrite<'db> {
        OpaqueWrite {
            site: OpaqueWriteSite::Operation {
                instance: self.instance,
                statement,
            },
            scope: self
                .instance
                .key(self.db)
                .impl_env(self.db)
                .normalization_scope(self.db),
            assumptions: self.instance.assumptions(self.db),
        }
    }

    pub fn write_region(
        &mut self,
        state: &mut BorrowState<'db>,
        region: &RegionSet<'db>,
        value: &CapabilityValue<'db>,
        statement: &NStatement<'db>,
    ) -> Result<(), SemanticDiagnostic<'db>> {
        let overwrite = self.opaque_write(statement.id);
        state
            .write_region(overwrite, &mut self.inventory.values, region, value)
            .map_err(|error| {
                self.internal_diag(
                    statement.origin,
                    format!("unresolved capability write: {error:?}"),
                )
            })
    }

    fn statement_diverges(&self, statement: &NStatement<'db>) -> bool {
        matches!(&statement.kind,
            NStatementKind::Define { result, expr: NExpr::Call { .. } }
                if self.calls.get(result).is_some_and(|call| !call.summary.may_return))
    }

    /// The converged reachability: a reached block either stops at the
    /// diverging call it recorded last or runs its terminator, whose edges are
    /// feasible under the block's terminal guard.
    pub(super) fn executable_control_flow(&self) -> ExecutableControlFlow {
        let blocks = self
            .body
            .blocks
            .iter()
            .zip(self.before.iter().zip(&self.terminal))
            .enumerate()
            .map(|(index, (block, (before, terminal)))| match terminal {
                Some(state) => ExecutableBlock::Continues(
                    block
                        .terminator
                        .kind
                        .successors()
                        .into_iter()
                        .map(|successor| {
                            self.edge_guard(NBlockId::new(index), successor)
                                .is_some_and(|guard| state.guard().and(&guard).is_some())
                        })
                        .collect(),
                ),
                None if before.is_empty() => ExecutableBlock::Unreachable,
                None => ExecutableBlock::Diverges(
                    block.statements[before.len() - 1]
                        .source
                        .expect("a diverging call is a source statement"),
                ),
            })
            .collect();
        ExecutableControlFlow(blocks)
    }

    pub(super) fn forwarded_value(&self, mut value: NValueId) -> NValueId {
        while let Some((_, NExpr::Forward { src })) = self.body.defining_expr(value) {
            value = src.value;
        }
        value
    }

    pub(super) fn boolean_choice(&self, value: NValueId) -> ChoiceKey<'db> {
        ChoiceKey::new(
            ValueOccurrence::Value(self.forwarded_value(value)),
            StructuralPath::default(),
        )
    }

    pub(super) fn recursive_call_choice(&self, occurrence: ValueOccurrence) -> bool {
        matches!(
            occurrence,
            ValueOccurrence::CallChoice { result, .. }
                if self.recursive_calls.contains(&result)
        )
    }

    fn prepare_recursive_calls(&mut self) {
        let mut reaches_self = FxHashMap::default();
        for (result, call) in &self.calls {
            let recursive = *reaches_self.entry(call.instance).or_insert_with(|| {
                let mut seen = FxHashSet::default();
                let mut pending = vec![call.instance];
                while let Some(instance) = pending.pop() {
                    if instance == self.instance {
                        return true;
                    }
                    if seen.len() >= 1024 {
                        // Unknown reachability leaves the call's choices intact.
                        return false;
                    }
                    if seen.insert(instance) {
                        pending.extend(
                            instance
                                .provisional_callees(self.db)
                                .iter()
                                .map(|callee| get_or_build_semantic_instance(self.db, callee.key)),
                        );
                    }
                }
                false
            });
            if recursive {
                self.recursive_calls.insert(*result);
            }
        }
    }

    pub fn edge_guard(&self, from: NBlockId, successor: &NSuccessor) -> Option<Guard<'db>> {
        let block = &self.body.blocks[from.index()];
        let always = Guard::always(&BinderScope::default());
        let mut selected = match &block.terminator.kind {
            NTerminatorKind::Branch {
                cond,
                then_target,
                else_target,
            } => {
                let choice = self.boolean_choice(cond.value);
                let constant = literal_bool_cond(self.db, &self.body, cond.value);
                let mut selected: Option<Guard<'db>> = None;
                for (target, value) in [(then_target, true), (else_target, false)] {
                    if target == successor && constant.is_none_or(|known| known == value) {
                        let guard = if constant.is_some() {
                            always.clone()
                        } else {
                            always
                                .with_boolean(choice.clone(), value)
                                .expect("boolean alternative is feasible")
                        };
                        let include_bounds = self.scalar_bounds_enabled(cond.value);
                        let guard = guard.and(&self.condition_guard(
                            cond.value,
                            value,
                            include_bounds,
                            CONDITION_BUDGET,
                        )?)?;
                        selected =
                            Some(selected.map_or_else(|| guard.clone(), |old| old.or(&guard)));
                    }
                }
                selected
            }
            NTerminatorKind::MatchEnum {
                value,
                cases,
                default,
                ..
            } => {
                let choice = ChoiceKey::new(
                    ValueOccurrence::Value(self.forwarded_value(value.value)),
                    StructuralPath::default(),
                );
                let mut selected: Option<Guard<'db>> = None;
                let mut remaining = Some(always.clone());
                for (variant, target) in cases {
                    let guard = always
                        .with_variant(choice.clone(), *variant)
                        .expect("enum alternative is feasible");
                    if *target == *successor {
                        selected =
                            Some(selected.map_or_else(|| guard.clone(), |old| old.or(&guard)));
                    }
                    remaining = remaining.and_then(|old| old.difference(&guard));
                }
                if default.as_ref() == Some(successor)
                    && let Some(remaining) = remaining
                {
                    selected =
                        Some(selected.map_or_else(|| remaining.clone(), |old| old.or(&remaining)));
                }
                selected
            }
            _ => Some(always.clone()),
        }?;
        // A feedback edge binds the next iteration's header parameters while
        // the state still describes this iteration's values of those names.
        if self
            .inventory
            .loops
            .feedback(from, successor.block)
            .is_some()
        {
            return Some(selected);
        }
        for (parameter, argument) in self.body.blocks[successor.block.index()]
            .params
            .iter()
            .zip(&successor.args)
        {
            if self.body.values[parameter.index()].ty.is_bool(self.db) {
                let choice = self.boolean_choice(*parameter);
                let equal = if let Some(value) =
                    literal_bool_cond(self.db, &self.body, argument.value)
                {
                    always.with_boolean(choice, value)?
                } else {
                    [true, false]
                        .into_iter()
                        .filter_map(|value| {
                            let argument = if self.scalar.live.contains(parameter) {
                                self.condition_guard(
                                    argument.value,
                                    value,
                                    self.scalar_bounds_enabled(argument.value),
                                    CONDITION_BUDGET,
                                )?
                            } else {
                                always.with_boolean(self.boolean_choice(argument.value), value)?
                            };
                            argument.with_boolean(choice.clone(), value)
                        })
                        .reduce(|left, right| left.or(&right))?
                };
                selected = selected.and(&equal)?;
            } else if self.scalar.values.contains(parameter)
                && (self.scalar.selectors.contains(&self.index(*parameter))
                    || self.scalar.phis.contains(parameter))
                && self.body.values[parameter.index()].ty.is_integral(self.db)
                && self.body.values[parameter.index()].ty
                    == self.body.values[argument.value.index()].ty
            {
                selected =
                    selected.with_equality(self.index(*parameter), self.index(argument.value))?;
            }
        }
        Some(selected)
    }

    pub fn extend_loan(
        &mut self,
        result: NValueId,
        reference: &LoanRef<'db>,
        region: &RegionSet<'db>,
        parents: Vec<Guarded<'db, LoanRef<'db>>>,
    ) {
        let (region, parents) =
            // One definition serves every execution of its site, so it keeps
            // no choice that any enclosing iteration can renew.
            if let Some(iteration) = self
                .inventory
                .loops
                .for_value(&self.body, result)
                .map(|region| self.inventory.loops.outermost(region))
            {
                let repeated = |occurrence| {
                    self.inventory
                        .loops
                        .repeats_occurrence(iteration, occurrence)
                };
                (
                    region.forget_occurrences(repeated),
                    parents
                        .into_iter()
                        .map(|parent| Guarded {
                            guard: parent.guard.forget_occurrences(repeated),
                            payload: parent.payload,
                        })
                        .collect(),
                )
            } else {
                (region.clone(), parents)
            };
        if self.inventory.loans[reference.id.0]
            .extend_occurrence(self.db, reference, &region, parents)
        {
            self.loan_facts_changed = true;
            self.source_generation += 1;
        }
    }

    /// Whether [`Self::solve`] has published a converged fixed point.
    pub fn is_solved(&self) -> bool {
        self.boundary_requirements.is_some()
    }

    #[cfg(feature = "borrowck-profile")]
    pub(super) fn profile_scope(&self, phase: &str) -> ProfileScope {
        ProfileScope::new(|| {
            format!(
                "{phase} {:?} owner={:?} name={} blocks={}",
                self.summary_mode,
                self.instance.key(self.db).owner(self.db),
                match self.instance.key(self.db).owner(self.db) {
                    BodyOwner::Func(func) => func
                        .name(self.db)
                        .to_opt()
                        .map(|name| name.data(self.db).as_str())
                        .unwrap_or("anonymous"),
                    _ => "generated",
                },
                self.body.blocks.len(),
            )
        })
    }

    fn initial_state(&mut self) -> BorrowState<'db> {
        let mut state = self
            .inventory
            .entry
            .before_allocations(&mut self.inventory.values);
        if let Some(guard) = self.inventory.loops.entry_guard(None, self.body.entry) {
            state.constrain(&guard, &mut self.inventory.values);
        }
        state
    }

    pub fn solve(&mut self) -> Result<(), SemanticDiagnostic<'db>> {
        #[cfg(feature = "borrowck-profile")]
        let profile = self.profile_scope("solve");
        self.prepare_calls()?;
        self.prepare_scalar_demand()?;
        if self
            .calls
            .values()
            .any(|call| !call.summary.certified_ranges.is_empty())
        {
            self.enable_bounded_readers();
        }
        self.prepare_recursive_calls();
        self.prepare_validation_dependencies();
        // Normalization can allocate joins before their predecessors. Use the
        // existing DFS order so forward facts propagate within each sweep.
        let order = self.inventory.loops.reverse_postorder().to_vec();
        loop {
            self.before.fill(Vec::new());
            self.terminal.fill(None);
            self.operations.fill(Vec::new());
            self.boundary_requirements = None;
            self.conflicts = None;
            let mut incoming = vec![None; self.body.blocks.len()];
            incoming[self.body.entry.index()] = Some(self.initial_state());
            let mut certificate_application_failed = false;
            // Every sweep keeps its per-statement states. The sweep that finds
            // the fixed point ran with unchanged facts, so its states are the
            // published snapshots when each block ran from its converged state.
            let mut before = vec![Vec::new(); self.body.blocks.len()];
            let mut terminal = vec![None; self.body.blocks.len()];
            loop {
                #[cfg(feature = "borrowck-profile")]
                profile.sweep(self.inventory.loans.len(), self.source_generation);
                let previous_incoming = incoming.clone();
                self.loan_facts_changed = false;
                self.storage_facts_changed = false;
                before.fill(Vec::new());
                terminal.fill(None);
                for &index in &order {
                    let Some(mut state) = incoming[index].clone() else {
                        continue;
                    };
                    state.extend_storage(&self.inventory.entry);
                    let block = self.body.blocks[index].clone();
                    let mut snapshots = Vec::with_capacity(block.statements.len());
                    let mut returns = true;
                    for statement in &block.statements {
                        #[cfg(feature = "borrowck-profile")]
                        profile.point("transfer", index, snapshots.len());
                        snapshots.push(state.clone());
                        if self.statement_diverges(statement) {
                            returns = false;
                            break;
                        }
                        self.transfer(&mut state, statement)?;
                    }
                    before[index] = snapshots;
                    if !returns {
                        continue;
                    }
                    for (successor_index, successor) in
                        block.terminator.kind.successors().into_iter().enumerate()
                    {
                        let mut edge = state.clone();
                        let edge_guard = self.edge_guard(NBlockId::new(index), successor);
                        let Some(edge_guard) = edge_guard else {
                            continue;
                        };
                        #[cfg(feature = "borrowck-profile")]
                        profile.point("constrain", index, successor_index);
                        if !edge.constrain(&edge_guard, &mut self.inventory.values) {
                            continue;
                        }
                        for certificate in self.prefix_certificates.iter().filter(|certificate| {
                            certificate.exit.from == NBlockId::new(index)
                                && certificate.exit.successor == successor_index
                                && certificate.exit.to == successor.block
                        }) {
                            let Some(coverage) = certificate
                                .coverage
                                .and(&edge.guard().in_scope(&certificate.family_scope))
                            else {
                                continue;
                            };
                            if !edge.certify_family_contents(
                                &mut self.inventory.values,
                                &certificate.family,
                                &certificate.family_scope,
                                &coverage,
                                &certificate.contents,
                            ) {
                                certificate_application_failed = true;
                            }
                        }

                        let arguments: Vec<_> = successor
                            .args
                            .iter()
                            .map(|arg| edge.value(arg.value).clone())
                            .collect();
                        for operand in &successor.args {
                            self.consume(&mut edge, *operand);
                        }
                        for (parameter, argument) in self.body.blocks[successor.block.index()]
                            .params
                            .iter()
                            .zip(arguments)
                        {
                            edge.set_value(*parameter, argument);
                        }
                        if let Some(iteration) = self
                            .inventory
                            .loops
                            .feedback(NBlockId::new(index), successor.block)
                        {
                            #[cfg(feature = "borrowck-profile")]
                            profile.point("forget_iteration", index, successor_index);
                            let loops = &self.inventory.loops;
                            edge.forget_iteration(
                                &mut self.inventory.values,
                                incoming[successor.block.index()].as_ref(),
                                |index| loops.repeats_index(iteration, index),
                                |index| loops.drops_fact(iteration, index),
                                |occurrence| loops.repeats_occurrence(iteration, occurrence),
                            );
                        }
                        if let Some(guard) = self
                            .inventory
                            .loops
                            .entry_guard(Some(NBlockId::new(index)), successor.block)
                            && !edge.constrain(&guard, &mut self.inventory.values)
                        {
                            continue;
                        }
                        #[cfg(feature = "borrowck-profile")]
                        profile.point("join", index, successor_index);
                        if let Some(previous) = &mut incoming[successor.block.index()] {
                            previous.extend_storage(&self.inventory.entry);
                            previous.join(&edge, &mut self.inventory.values);
                        } else {
                            incoming[successor.block.index()] = Some(edge);
                        }
                    }
                    terminal[index] = Some(state);
                }
                if self.storage_facts_changed {
                    self.prefix_certificates.clear();
                    self.failed_prefix_certificates = false;
                    certificate_application_failed = false;
                    self.inventory.reset_epoch_loans();
                    self.source_generation += 1;
                    incoming.fill(None);
                    incoming[self.body.entry.index()] = Some(self.initial_state());
                    continue;
                }
                if incoming == previous_incoming && !self.loan_facts_changed {
                    break;
                }
            }
            // A certificate that no longer fits the replayed state is dropped
            // for this body; the conservative analysis is replayed without it.
            if certificate_application_failed {
                self.prefix_certificates.clear();
                self.failed_prefix_certificates = true;
                continue;
            }
            // A block joined again after this sweep visited it, such as a loop
            // header reached through its latch, may have run from another
            // representation of its converged state. Replay the blocks from
            // their converged states then.
            let settled =
                incoming
                    .iter()
                    .zip(&before)
                    .zip(&terminal)
                    .all(|((entry, before), terminal)| {
                        entry.is_none() || before.first().or(terminal.as_ref()) == entry.as_ref()
                    });
            if !settled {
                for (index, entry) in incoming.into_iter().enumerate() {
                    let Some(mut state) = entry else { continue };
                    let statements = self.body.blocks[index].statements.clone();
                    let mut snapshots = Vec::with_capacity(statements.len());
                    let mut returns = true;
                    for statement in &statements {
                        snapshots.push(state.clone());
                        if self.statement_diverges(statement) {
                            returns = false;
                            break;
                        }
                        self.transfer(&mut state, statement)?;
                    }
                    before[index] = snapshots;
                    terminal[index] = returns.then_some(state);
                }
            }
            self.before = before;
            self.terminal = terminal;
            self.availability_diagnostic = OnceCell::new();
            #[cfg(feature = "borrowck-profile")]
            profile.point("resolve_operations", 0, 0);
            self.resolve_operations()?;
            #[cfg(feature = "borrowck-profile")]
            profile.point("boundary", 0, 0);
            let boundary_requirements = resolve_boundary_requirements(self);
            if !self.loan_facts_changed && !self.storage_facts_changed {
                if self.prefix_certificates.is_empty() && !self.failed_prefix_certificates {
                    let certificates = self.prove_prefix_certificates();
                    if !certificates.is_empty() {
                        self.prefix_certificates = certificates;
                        self.enable_bounded_readers();
                        // The proved contents retract earlier possibilities.
                        // Rebuild loans derived before that refinement as well.
                        self.inventory.reset_epoch_loans();
                        self.source_generation += 1;
                        continue;
                    }
                }
                self.boundary_requirements = Some(boundary_requirements);
                self.conflicts = Some(self.analyze_conflicts());
                return Ok(());
            }
            self.prefix_certificates.clear();
            self.failed_prefix_certificates = false;
            if self.storage_facts_changed {
                self.inventory.reset_epoch_loans();
                self.source_generation += 1;
            }
            // Resolving call effects, held referents, or boundary requirements
            // can discover typed cells.
            // Replay before any consumer observes snapshots or resolved actions.
        }
    }

    pub fn memory_region(
        &self,
        region: &RegionSet<'db>,
        target_ty: TyId<'db>,
        offset: MemoryOffset<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<RegionSet<'db>, SemanticDiagnostic<'db>> {
        let mut clauses = Vec::new();
        for clause in region.clauses() {
            let source = SourceExpr::from_place(&clause.payload).ok_or_else(|| {
                self.internal_diag(origin, "raw pointer has no address provenance".into())
            })?;
            clauses.push(Guarded {
                guard: clause.guard.clone(),
                payload: SymbolicPlace {
                    root: RegionRoot::External(ExternalSource::memory(
                        self.db, source, target_ty, offset,
                    )),
                    path: RegionPath::default(),
                    views: Default::default(),
                },
            });
        }
        Ok(RegionSet::new(region.scope(), clauses))
    }

    pub fn ensure_storage(
        &mut self,
        state: &mut BorrowState<'db>,
        sources: impl IntoIterator<Item = (ExternalSource<'db>, BinderScope)>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticDiagnostic<'db>> {
        let sources: Vec<_> = sources
            .into_iter()
            .filter(|(source, scope)| {
                !self
                    .inventory
                    .entry
                    .storage_coverage(&RegionRoot::External(source.clone()), scope)
                    .complete(&Guard::always(scope))
            })
            .collect();
        if !sources.is_empty() {
            self.source_generation += 1;
            let changed = self
                .inventory
                .add_external_sources(self.db, self.instance, sources.iter().cloned())
                .map_err(|error| self.storage_error(origin, error, "invalid raw memory storage"))?;
            if !changed {
                let (source, scope) = &sources[0];
                let root = RegionRoot::External(source.clone());
                let uncovered = self
                    .inventory
                    .entry
                    .storage_coverage(&root, scope)
                    .uncovered(&Guard::always(scope));
                return Err(self.internal_diag(
                    origin,
                    format!(
                        "typed storage registration made no progress for {root:?}; uncovered: {uncovered:?}"
                    ),
                ));
            }
            self.storage_facts_changed = true;
        }
        state.extend_storage(&self.inventory.entry);
        Ok(())
    }

    fn consume(&mut self, state: &mut BorrowState<'db>, operand: NOperand) {
        if operand.mode == ReadMode::Move
            && self.body.values[operand.value.index()]
                .ty
                .as_capability(self.db)
                .is_none()
        {
            let value = state.value(operand.value);
            let empty = self.inventory.values.empty(value.shape(), value.scope());
            state.set_value(operand.value, empty);
        }
    }

    /// Recognized allocating constants share one source identity between
    /// provenance transfer and resolved lifetime actions, including on replay.
    pub(super) fn literal_birth(
        &self,
        result: NValueId,
        constant: &SConst<'db>,
    ) -> Option<(FieldIndex, AllocationBirth<'db>)> {
        let (field, contract) =
            literal_allocation(self.db, self.body.values[result.index()].ty, constant)?;
        Some((
            field,
            AllocationBirth {
                allocation: OpaqueHandleRef {
                    contract,
                    occurrence: AddressOccurrence::Value {
                        instance: self.instance,
                        value: result,
                        choice: 0,
                    },
                    arguments: self
                        .inventory
                        .loops
                        .for_value(&self.body, result)
                        .map(IndexExpr::Iteration)
                        .into_iter()
                        .collect(),
                },
                guard: Guard::always(&BinderScope::default()),
            },
        ))
    }

    fn transfer(
        &mut self,
        state: &mut BorrowState<'db>,
        statement: &NStatement<'db>,
    ) -> Result<(), SemanticDiagnostic<'db>> {
        let scope = BinderScope::default();
        let NStatementKind::Define { result, expr } = &statement.kind else {
            let NStatementKind::Store { destination, value } = &statement.kind else {
                unreachable!()
            };
            let replacement = state.value(value.value).clone();
            let region = self.resolve_region(state, destination);
            let scalar = self
                .scalar_cell(destination, &region, state)
                .map(|root| (root, self.index(value.value)));
            self.consume(state, *value);
            self.write_region(state, &region, &replacement, statement)?;
            if let Some((root, value)) = scalar {
                state.store_scalar(root, value);
            }
            return Ok(());
        };
        let shape = self.inventory.shapes[result.index()];
        let occurrence = ValueOccurrence::Value(*result);
        let mut value = match expr {
            NExpr::Forward { src } => state.value(src.value).clone(),
            NExpr::ProjectValue { value, path } => {
                let path = StructuralPath::new(self.path(&path.0).as_slice());
                let source = state.value(value.value).clone();
                let selected = self
                    .inventory
                    .values
                    .project(&source, &path, ValueOccurrence::Value(value.value))
                    .unwrap_or_else(|| self.inventory.values.empty(shape, &scope));
                if value.mode == ReadMode::Move
                    && self.body.values[value.value.index()]
                        .ty
                        .as_capability(self.db)
                        .is_none()
                {
                    let empty = self
                        .inventory
                        .values
                        .empty(selected.shape(), selected.scope());
                    let changed = self.inventory.values.replace(&source, &path, &empty);
                    state.set_value(value.value, changed);
                }
                selected
            }
            NExpr::Load { place, mode } => {
                let region = self.resolve_region(state, place);
                let value =
                    self.read_region(state, &region, shape, occurrence, statement.origin)?;
                if let Some(root) = self.scalar_cell(place, &region, state) {
                    state.load_scalar(root, *result, &mut self.inventory.values);
                }
                if *mode == ReadMode::Move && place.ty.as_capability(self.db).is_none() {
                    state
                        .move_out(&mut self.inventory.values, &region, shape)
                        .map_err(|error| {
                            self.internal_diag(
                                statement.origin,
                                format!("unresolved capability move: {error:?}"),
                            )
                        })?;
                }
                value
            }
            NExpr::Borrow { place, .. } => {
                let value = self.inventory.definitions[result].clone();
                let resolved = self.resolve_place(state, place);
                if resolved.invalidated.invalid {
                    self.inventory.values.with_direct(
                        &value,
                        vec![Guarded {
                            guard: state.guard().clone(),
                            payload: CapabilityRef::Invalidated {
                                class: shape.direct(self.db).expect("borrow shape").class,
                                region: resolved.invalidated.requirements,
                            },
                        }],
                    )
                } else {
                    for entry in value.direct() {
                        let reference = entry.payload.loan().expect("borrow template");
                        self.extend_loan(
                            *result,
                            reference,
                            &resolved.region,
                            resolved.parents.clone(),
                        );
                    }
                    value
                }
            }
            NExpr::MakeView { place, .. } => {
                let resolved = self.resolve_place(state, place);
                let region = resolved.region;
                let contents_shape = self.shape(place.ty)?;
                let contents =
                    self.read_region(state, &region, contents_shape, occurrence, statement.origin)?;
                // The view's direct reference names the representation place; a
                // handle contained there remains in the separately tracked contents.
                let children = self.inventory.values.with_direct(&contents, Vec::new());
                let view = self.inventory.values.repack(&children, shape);
                self.inventory.values.with_direct(
                    &view,
                    vec![Guarded {
                        guard: Guard::always(&scope),
                        payload: CapabilityRef::view(region, resolved.parents),
                    }],
                )
            }
            NExpr::PointerCast { value, to } => {
                let empty = self.inventory.values.empty(shape, &scope);
                if let Some(target_ty) = to.as_ptr(self.db) {
                    let source = state.value(value.value).clone();
                    let region = if self.body.values[value.value.index()]
                        .ty
                        .as_ptr(self.db)
                        .is_some()
                    {
                        let region = self.resolve_capability(&source).region;
                        self.memory_region(
                            &region,
                            target_ty,
                            MemoryOffset::Zero,
                            statement.origin,
                        )?
                    } else {
                        let contract = OpaqueHandleContract::for_ty(
                            self.db,
                            self.instance
                                .key(self.db)
                                .impl_env(self.db)
                                .normalization_scope(self.db),
                            self.instance.assumptions(self.db),
                            *to,
                        )
                        .expect("verified pointer cast contract")
                        .expect("pointer contract");
                        RegionSet::singleton(
                            &scope,
                            RegionRoot::External(ExternalSource::opaque(
                                self.db,
                                OpaqueHandleRef {
                                    contract,
                                    occurrence: AddressOccurrence::Value {
                                        instance: self.instance,
                                        value: *result,
                                        choice: 0,
                                    },
                                    arguments: self
                                        .inventory
                                        .loops
                                        .arguments(&self.body, *result)
                                        .into_boxed_slice(),
                                },
                            )),
                            RegionPath::default(),
                        )
                    };
                    self.inventory.values.with_direct(
                        &empty,
                        vec![Guarded {
                            guard: Guard::always(&scope),
                            payload: CapabilityRef::Address(region),
                        }],
                    )
                } else {
                    empty
                }
            }
            NExpr::StructuralRepack { value, .. } => {
                let repack = ReferentRepackId::new(
                    self.db,
                    self.body.values[value.value.index()].ty,
                    self.body.values[result.index()].ty,
                    self.instance
                        .key(self.db)
                        .impl_env(self.db)
                        .normalization_scope(self.db),
                    self.instance.assumptions(self.db),
                );
                repack
                    .apply(
                        self.db,
                        &mut self.inventory.values,
                        state.value(value.value),
                        &[],
                    )
                    .map_err(|error| {
                        self.internal_diag(
                            statement.origin,
                            format!("invalid referent conversion: {error:?}"),
                        )
                    })?
            }
            NExpr::ArrayRepeat { value, .. } => self
                .inventory
                .values
                .array_repeat(shape, state.value(value.value)),
            NExpr::AggregateMake { fields, .. }
            | NExpr::MakeHandle {
                fields,
                variant: None,
                ..
            } => match shape.children(self.db) {
                ShapeChildren::Array { .. } | ShapeChildren::EmptyArray => {
                    let mut array = self.inventory.values.empty(shape, &scope);
                    for (index, field) in fields.iter().enumerate() {
                        array = self.inventory.values.replace(
                            &array,
                            &StructuralPath::new([Projection::Index(IndexExpr::Const(index))]),
                            state.value(field.value),
                        );
                    }
                    array
                }
                ShapeChildren::Product(_) => self.inventory.values.product(
                    shape,
                    &scope,
                    fields.iter().enumerate().map(|(index, field)| {
                        (
                            FieldIndex(index.try_into().expect("verified field count")),
                            state.value(field.value).clone(),
                        )
                    }),
                ),
                ShapeChildren::None | ShapeChildren::Sum(_) => {
                    return Err(self.internal_diag(
                        statement.origin,
                        "aggregate constructor lacks a structural product".into(),
                    ));
                }
            },
            NExpr::EnumMake {
                variant, fields, ..
            }
            | NExpr::MakeHandle {
                variant: Some(variant),
                fields,
                ..
            } => {
                let ShapeChildren::Sum(variants) = shape.children(self.db) else {
                    unreachable!("verified enum")
                };
                let variant_shape = variants
                    .iter()
                    .find(|(key, _)| key == variant)
                    .expect("verified variant")
                    .1;
                let contents = self.inventory.values.product(
                    variant_shape,
                    &scope,
                    fields.iter().enumerate().map(|(index, field)| {
                        (
                            FieldIndex(index.try_into().expect("verified field count")),
                            state.value(field.value).clone(),
                        )
                    }),
                );
                self.inventory
                    .values
                    .sum(shape, &scope, [(*variant, contents)])
            }
            NExpr::Call { .. } => self.transfer_call(state, *result, statement)?,
            NExpr::Const(constant) => {
                let empty = self.inventory.values.empty(shape, &scope);
                if let Some((field, birth)) = self.literal_birth(*result, constant) {
                    state.birth_allocations(
                        &mut self.inventory.values,
                        &self.inventory.entry,
                        &self.inventory.allocation_cells,
                        slice::from_ref(&birth),
                    );
                    let pointer_shape = self.shape(birth.allocation.contract.handle_ty)?;
                    let pointer = self.inventory.values.empty(pointer_shape, &scope);
                    let source = ExternalSource::allocation(self.db, birth.allocation);
                    let pointer = self.inventory.values.with_direct(
                        &pointer,
                        vec![Guarded {
                            guard: Guard::always(&scope),
                            payload: CapabilityRef::Address(RegionSet::singleton(
                                &scope,
                                RegionRoot::External(source),
                                RegionPath::default(),
                            )),
                        }],
                    );
                    self.inventory.values.replace(
                        &empty,
                        &StructuralPath::new([Projection::Field(field)]),
                        &pointer,
                    )
                } else if shape.contains_capability(self.db) {
                    return Err(self.internal_diag(
                        statement.origin,
                        "constant creates a capability without an explicit source".into(),
                    ));
                } else {
                    empty
                }
            }
            NExpr::CodeRegionRef { .. }
            | NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::ScalarCast { .. }
            | NExpr::GetEnumTag { .. }
            | NExpr::IsEnumVariant { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. } => {
                if shape.contains_capability(self.db) {
                    return Err(self.internal_diag(
                        statement.origin,
                        "scalar expression creates a capability without an explicit source".into(),
                    ));
                }
                self.inventory.values.empty(shape, &scope)
            }
        };
        if let NExpr::MakeHandle { origin, .. } = expr {
            let root = match origin {
                HandleOrigin::Provider(binding) => RegionRoot::External(ExternalSource::provider(
                    self.db,
                    ProviderRegionId::new(self.db, binding.clone()),
                    shape.direct(self.db).expect("declared handle").target_ty,
                )),
                HandleOrigin::Opaque(contract) => RegionRoot::External(ExternalSource::opaque(
                    self.db,
                    OpaqueHandleRef {
                        contract: *contract,
                        occurrence: AddressOccurrence::Value {
                            instance: self.instance,
                            value: *result,
                            choice: 0,
                        },
                        arguments: self
                            .inventory
                            .loops
                            .for_value(&self.body, *result)
                            .map(IndexExpr::Iteration)
                            .into_iter()
                            .collect(),
                    },
                )),
            };
            value = self.inventory.values.with_direct(
                &value,
                vec![Guarded {
                    guard: Guard::always(&scope),
                    payload: CapabilityRef::Address(RegionSet::singleton(
                        &scope,
                        root,
                        RegionPath::default(),
                    )),
                }],
            );
        }
        let sources: Vec<_> = self
            .inventory
            .values
            .leaves(&value, occurrence)
            .into_iter()
            .flat_map(|leaf| {
                leaf.payload
                    .region(self.db, &self.inventory.loans, leaf.guard.scope())
                    .clauses()
                    .to_vec()
            })
            .filter_map(|clause| match clause.payload.root {
                RegionRoot::External(source) => Some((source, clause.guard.scope().clone())),
                _ => None,
            })
            .collect();
        self.ensure_storage(state, sources, statement.origin)?;
        if !matches!(expr, NExpr::ProjectValue { .. }) {
            expr.for_each_value_operand(|operand| self.consume(state, operand));
        }
        for (index, root) in self.body.roots.iter().enumerate() {
            if matches!(root.kind, NRootKind::Temporary { value } if value == *result) {
                let region = RegionSet::singleton(
                    &scope,
                    self.inventory.roots[index].clone(),
                    RegionPath::default(),
                );
                let overwrite = self.opaque_write(statement.id);
                state
                    .write_region(overwrite, &mut self.inventory.values, &region, &value)
                    .expect("temporary root matches its initializer");
            }
        }
        if value.shape() != shape {
            return Err(self.internal_diag(statement.origin, format!("transfer shape mismatch for {result:?} ({}): {expr:?}; expected={:?}; actual={:?}", self.body.values[result.index()].ty.pretty_print(self.db), (shape.direct(self.db), shape.children(self.db)), (value.shape().direct(self.db), value.shape().children(self.db)))));
        }
        let value = self.inventory.values.with_guard(&value, state.guard());
        state.set_value(*result, value);
        Ok(())
    }

    pub fn diag(
        &self,
        kind: SemanticDiagnosticKind,
        origin: SemOrigin<'db>,
        message: String,
    ) -> SemanticDiagnostic<'db> {
        SemanticDiagnostic::new(
            self.instance,
            kind,
            message,
            SemanticDiagnosticSpan::OriginWithTemplateFallback {
                owner: self.instance.key(self.db).owner(self.db),
                template_owner: self.body.template_owner,
                origin,
            },
        )
    }

    pub fn internal_diag(
        &self,
        origin: SemOrigin<'db>,
        message: String,
    ) -> SemanticDiagnostic<'db> {
        self.diag(SemanticDiagnosticKind::Internal, origin, message)
    }

    /// A storage discovery failure. Reaching too many referent types is the
    /// program's error; any other failure is internal.
    pub(super) fn storage_error(
        &self,
        origin: SemOrigin<'db>,
        error: ShapeError<'db>,
        context: &str,
    ) -> SemanticDiagnostic<'db> {
        match error {
            ShapeError::UnboundedReferents(head) => {
                unbounded_referents_diag(self.db, self.instance, self.body.template_owner, head)
            }
            error => self.internal_diag(origin, format!("{context}: {error:?}")),
        }
    }
}
