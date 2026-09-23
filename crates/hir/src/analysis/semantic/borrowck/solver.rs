//! One fixed point for structural holders, storage contents, and loan relations.
use super::validity::NativeValidity;
use crate::analysis::semantic::diagnostics::{
    BlockedSemanticBody, SemanticDiagnostic, SemanticDiagnosticKind, SemanticDiagnosticSpan,
    SemanticNormalizationFailure, normalized_body_internal_diag,
};
use std::{collections::BTreeMap, slice};

use cranelift_entity::EntityRef;
use num_traits::ToPrimitive;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        FieldIndex, SConst, SemConstScalar, SemConstValue, SemOrigin, SemanticInstance,
        capability::{
            birth::AllocationBirth,
            external::ExternalSource,
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
        normalized::{
            HandleOrigin, NBlock, NBlockId, NDataPath, NDataProjection, NExpr, NIndex, NOperand,
            NPlace, NPlaceBase, NRootKind, NStatement, NStatementId, NStatementKind, NSuccessor,
            NTerminatorKind, NValueDefinition, NValueId, NormalizedBody, ReadMode,
            literal_allocation, normalize_semantic_body,
        },
    },
    ty::ty_def::TyId,
};

use super::{
    access::ResolvedOperation,
    boundary::resolve_boundary_requirements,
    inventory::Inventory,
    ir::{BoundaryRequirement, PendingSemanticValidation},
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
    pub inventory: Inventory<'db>,
    pub summary_mode: BorrowSummaryMode,
    pub calls: BTreeMap<NValueId, CallSummary<'db>>,
    /// Published only after the joint structural/loan fixed point converges.
    pub before: Vec<Vec<BorrowState<'db>>>,
    pub terminal: Vec<Option<BorrowState<'db>>>,
    pub operations: Vec<Vec<ResolvedOperation<'db>>>,
    pub boundary_requirements:
        Option<Result<Vec<BoundaryRequirement<'db>>, SemanticDiagnostic<'db>>>,
    pub blocked: Option<BlockedSemanticBody<'db>>,
    pub pending: PendingSemanticValidation<'db>,
    pub validation_dependencies: Vec<Vec<bool>>,
    pub loan_facts_changed: bool,
    pub storage_facts_changed: bool,
    /// Invalidates call-local source resolutions when their inventory changes.
    pub(super) source_generation: usize,
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
            normalized_body_internal_diag(
                db,
                instance,
                &body,
                SemOrigin::Body(body.template_owner),
                match error {
                    ShapeError::UnresolvedCapability(ty) => format!(
                        "unresolved capability inventory for `{}`: {ty:?}",
                        ty.pretty_print(db)
                    ),
                    error => format!("invalid capability inventory: {error:?}"),
                },
            )
        })?;
        Ok(Self {
            db,
            instance,
            before: vec![Vec::new(); body.blocks.len()],
            terminal: vec![None; body.blocks.len()],
            operations: vec![Vec::new(); body.blocks.len()],
            boundary_requirements: None,
            validation_dependencies: body
                .blocks
                .iter()
                .map(|block| vec![false; block.statements.len() + 1])
                .collect(),
            body,
            inventory,
            summary_mode,
            calls: BTreeMap::new(),
            blocked: None,
            pending: PendingSemanticValidation::default(),
            loan_facts_changed: false,
            storage_facts_changed: false,
            source_generation: 0,
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
        let NValueDefinition::Statement { block, statement } =
            self.body.values[value.index()].definition
        else {
            return IndexExpr::Runtime(value);
        };
        match &self.body.blocks[block.index()].statements[statement as usize].kind {
            NStatementKind::Define {
                expr: NExpr::Const(SConst::Value(constant)),
                ..
            } => {
                if let SemConstValue::Scalar {
                    value: SemConstScalar::Int { value: integer },
                    ..
                } = constant.value(self.db)
                    && let Some(integer) = integer.to_usize()
                {
                    return IndexExpr::Const(integer);
                }
                IndexExpr::Runtime(value)
            }
            NStatementKind::Define {
                expr: NExpr::Forward { src },
                ..
            } => self.index(src.value),
            NStatementKind::Define {
                expr: NExpr::Load { place, .. },
                ..
            } if place.path.is_empty() && place.ty.is_integral(self.db) => match place.base {
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
            },

            NStatementKind::Define { .. } | NStatementKind::Store { .. } => {
                IndexExpr::Runtime(value)
            }
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

    pub fn resolve_capability(&self, value: &CapabilityValue<'db>) -> Resolution<'db> {
        let mut result = Resolution::empty(value.scope());
        result.region = RegionSet::union_all(
            value.scope(),
            value.direct().iter().filter_map(|entry| {
                let region = entry
                    .payload
                    .region(self.db, &self.inventory.loans, entry.guard.scope())
                    .with_guard(&entry.guard);
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
                (!state.has_storage(self.db, &clause.payload.root, clause.guard.scope()))
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

    pub fn edge_guard(&self, block: &NBlock<'db>, successor: &NSuccessor) -> Option<Guard<'db>> {
        let always = Guard::always(&BinderScope::default());
        match &block.terminator.kind {
            NTerminatorKind::MatchEnum {
                value,
                cases,
                default,
                ..
            } => {
                let choice = ChoiceKey::new(
                    ValueOccurrence::Value(value.value),
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
            _ => Some(always),
        }
    }

    pub fn extend_loan(
        &mut self,
        result: NValueId,
        reference: &LoanRef<'db>,
        region: &RegionSet<'db>,
        parents: Vec<Guarded<'db, LoanRef<'db>>>,
    ) {
        let (region, parents) =
            if let Some(iteration) = self.inventory.loops.for_value(&self.body, result) {
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

    pub fn solve(&mut self) -> Result<(), SemanticDiagnostic<'db>> {
        self.prepare_calls()?;
        self.prepare_validation_dependencies();
        // Normalization can allocate joins before their predecessors. Use the
        // existing DFS order so forward facts propagate within each sweep.
        let order = self.inventory.loops.reverse_postorder().to_vec();
        loop {
            self.before.fill(Vec::new());
            self.terminal.fill(None);
            self.operations.fill(Vec::new());
            self.boundary_requirements = None;
            let mut incoming = vec![None; self.body.blocks.len()];
            incoming[self.body.entry.index()] = Some(self.inventory.entry.clone());
            loop {
                self.loan_facts_changed = false;
                self.storage_facts_changed = false;
                let mut state_changed = false;
                for &index in &order {
                    let Some(mut state) = incoming[index].clone() else {
                        continue;
                    };
                    state.extend_storage(&self.inventory.entry);
                    let block = self.body.blocks[index].clone();
                    let mut returns = true;
                    for statement in &block.statements {
                        if self.statement_diverges(statement) {
                            returns = false;
                            break;
                        }
                        self.transfer(&mut state, statement)?;
                    }
                    if !returns {
                        continue;
                    }
                    for successor in block.terminator.kind.successors() {
                        let mut edge = state.clone();
                        let edge_guard = self.edge_guard(&block, successor);
                        let Some(edge_guard) = edge_guard else {
                            continue;
                        };
                        if !edge.constrain(&edge_guard, &mut self.inventory.values) {
                            continue;
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
                            let repeated = self.inventory.loops.repeated(iteration);
                            edge.forget_iteration(&mut self.inventory.values,
                            |index| matches!(index, IndexExpr::Iteration(region) if region == iteration) || matches!(index, IndexExpr::Runtime(value) if repeated.contains(&value)),
                            |occurrence| self.inventory.loops.repeats_occurrence(iteration, occurrence));
                        }
                        if let Some(previous) = &mut incoming[successor.block.index()] {
                            previous.extend_storage(&self.inventory.entry);
                            state_changed |= previous.join(&edge, &mut self.inventory.values);
                        } else {
                            incoming[successor.block.index()] = Some(edge);
                            state_changed = true;
                        }
                    }
                }
                if self.storage_facts_changed {
                    incoming.fill(None);
                    incoming[self.body.entry.index()] = Some(self.inventory.entry.clone());
                    continue;
                }
                if !state_changed && !self.loan_facts_changed {
                    break;
                }
            }
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
                self.before[index] = snapshots;
                self.terminal[index] = returns.then_some(state);
            }
            self.resolve_operations()?;
            let boundary_requirements = resolve_boundary_requirements(self);
            if !self.loan_facts_changed && !self.storage_facts_changed {
                self.boundary_requirements = Some(boundary_requirements);
                return Ok(());
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
        element: Option<(TyId<'db>, IndexExpr<'db>)>,
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
                        self.db, source, target_ty, element,
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
                !self.inventory.entry.has_storage(
                    self.db,
                    &RegionRoot::External(source.clone()),
                    scope,
                )
            })
            .collect();
        if !sources.is_empty() {
            self.source_generation += 1;
            self.inventory
                .add_external_sources(self.db, self.instance, sources)
                .map_err(|error| {
                    self.internal_diag(origin, format!("invalid raw memory storage: {error:?}"))
                })?;
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
            self.consume(state, *value);
            return self.write_region(state, &region, &replacement, statement);
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
                if *mode == ReadMode::Move && place.ty.as_capability(self.db).is_none() {
                    let empty = self.inventory.values.empty(shape, &scope);
                    self.write_region(state, &region, &empty, statement)?;
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
                        self.memory_region(&region, target_ty, None, statement.origin)?
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
}
