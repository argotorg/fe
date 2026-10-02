//! Ownership dataflow shared by diagnostics and summary construction.
use super::validity::NativeValidity;
use crate::analysis::semantic::diagnostics::{
    SemanticDiagnostic, SemanticDiagnosticKind, SemanticDiagnosticSpan,
};
use std::collections::{BTreeMap, BTreeSet};

use cranelift_entity::EntityRef;

use super::{
    access::ResolvedAccess,
    ir::{AvailabilityRequirement, AvailabilitySummary},
    solver::Borrowck,
    summary::{CallInputs, SourceInstantiations},
};
use crate::analysis::{
    semantic::{
        SemOrigin,
        capability::{
            birth::AllocationBirth,
            external::ExternalOrigin,
            footprint::{AccessExtent, AccessFootprint},
            guard::{Guard, ValueOccurrence},
            handle::AddressOccurrence,
            index::{BinderScope, IndexExpr},
            path::RegionPath,
            region::{OverlapResult, RegionRoot, RegionSet},
            shape::ShapeId,
            source::{InputOrigin, SourceExpr},
            state::BorrowState,
            value::Guarded,
        },
        normalized::{NBlockId, NStatementKind, NTerminatorKind, NValueId, access::AccessPhase},
    },
    ty::{corelib::MemoryAccessKind, ty_is_copy},
};

impl<'db> AvailabilitySummary<'db> {
    pub fn empty() -> Self {
        Self {
            incoming: Vec::new(),
            reinitialized: RegionSet::empty(&BinderScope::default()),
            unavailable: RegionSet::empty(&BinderScope::default()),
        }
    }
}

#[derive(Clone)]
pub(super) struct ResolvedAvailability<'db> {
    pub incoming: Vec<(AvailabilityRequirement<'db>, NativeValidity<'db>)>,
    pub reinitialized: RegionSet<'db>,
    pub unavailable: RegionSet<'db>,
    /// Typed groups of unavailable memory, for retiring capabilities without
    /// replaying an ownership effect as a physical byte write.
    pub consumed: Vec<(RegionSet<'db>, ShapeId<'db>)>,
}

pub(super) struct AvailabilityAnalysis<'db> {
    pub summary: AvailabilitySummary<'db>,
    pub native_validity: NativeValidity<'db>,
    pub diagnostic: Option<SemanticDiagnostic<'db>>,
    report_errors: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct MoveFact<'db> {
    origin: SemOrigin<'db>,
    region: RegionSet<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct AvailabilityState<'db> {
    moved: BTreeMap<(usize, usize, usize), MoveFact<'db>>,
    // Candidate sites only: cleared facts may leave an entry, and every lookup
    // rechecks the current region with the shared physical-family selector.
    families: BTreeMap<AddressOccurrence<'db>, BTreeSet<(usize, usize, usize)>>,
    initialized: RegionSet<'db>,
    guard: Guard<'db>,
}

impl<'db> AvailabilityState<'db> {
    fn new() -> Self {
        let scope = BinderScope::default();
        Self {
            moved: BTreeMap::new(),
            families: BTreeMap::new(),
            initialized: RegionSet::empty(&scope),
            guard: Guard::always(&scope),
        }
    }

    fn restrict(&mut self, guard: &Guard<'db>) -> Option<()> {
        self.guard = self.guard.and(guard)?;
        self.initialized = self.initialized.with_guard(&self.guard);
        for fact in self.moved.values_mut() {
            fact.region = fact.region.with_guard(&self.guard);
        }
        Some(())
    }

    fn join(&mut self, other: Self) {
        // Initialization is a must fact. Preserve alternatives only where their
        // path guards are disjoint; overlapping paths require both guarantees.
        let mut initialized = self.initialized.proven_intersection(&other.initialized);
        if let Some(guard) = self.guard.difference(&other.guard) {
            initialized = initialized.union(&self.initialized.with_guard(&guard));
        }
        if let Some(guard) = other.guard.difference(&self.guard) {
            initialized = initialized.union(&other.initialized.with_guard(&guard));
        }
        self.initialized = initialized;
        self.guard = self.guard.or(&other.guard);
        for (family, sites) in other.families {
            self.families.entry(family).or_default().extend(sites);
        }
        for (site, fact) in other.moved {
            self.moved
                .entry(site)
                .and_modify(|old| old.region = old.region.union(&fact.region))
                .or_insert(fact);
        }
    }

    fn initialize(&mut self, written: &RegionSet<'db>, exported: &RegionSet<'db>) {
        for fact in self.moved.values_mut() {
            fact.region = fact.region.remove_covered(written);
        }
        self.moved.retain(|_, fact| !fact.region.is_empty());
        self.initialized = self.initialized.union(exported);
    }

    fn birth_allocations(&mut self, births: &[AllocationBirth<'db>]) {
        for birth in births {
            let Some(sites) = self.families.get(&birth.allocation.occurrence) else {
                continue;
            };
            for site in sites {
                let Some(fact) = self.moved.get_mut(site) else {
                    continue;
                };
                fact.region = RegionSet::new(
                    fact.region.scope(),
                    fact.region.clauses().iter().filter_map(|clause| {
                        let guard = if let Some(born) =
                            birth.selector(&clause.payload.root, clause.guard.scope())
                        {
                            clause.guard.difference(&born)?
                        } else {
                            clause.guard.clone()
                        };
                        Some(Guarded {
                            guard,
                            payload: clause.payload.clone(),
                        })
                    }),
                );
            }
        }
    }

    fn consume(
        &mut self,
        site: (usize, usize, usize),
        region: RegionSet<'db>,
        origin: SemOrigin<'db>,
    ) {
        let region = region.with_guard(&self.guard);
        if !region.is_empty() {
            for clause in region.clauses() {
                if let RegionRoot::External(source) = &clause.payload.root
                    && let Some(allocation) = source.fresh_allocation()
                {
                    self.families
                        .entry(allocation.occurrence)
                        .or_default()
                        .insert(site);
                }
            }
            self.moved
                .entry(site)
                .and_modify(|fact| fact.region = fact.region.union(&region))
                .or_insert(MoveFact { origin, region });
        }
    }
}

impl<'db> Borrowck<'db> {
    pub fn call_availability(
        &mut self,
        state: &BorrowState<'db>,
        result: NValueId,
        inputs: CallInputs<'_, 'db>,
    ) -> Result<Option<ResolvedAvailability<'db>>, SemanticDiagnostic<'db>> {
        let Some(call) = self.calls.get(&result).cloned() else {
            return Ok(None);
        };
        let scope = BinderScope::default();
        let mut resolved = ResolvedAvailability {
            incoming: Vec::new(),
            reinitialized: RegionSet::empty(&scope),
            unavailable: RegionSet::empty(&scope),
            consumed: Vec::new(),
        };
        let cache = self.inventory.values.guard_cache();
        let mut instantiations = SourceInstantiations::new(state, result, inputs);
        // Every source is substituted against the same pre-call snapshot. A
        // guaranteed callee destination can still be ambiguous in its caller.
        for (region, kind, definite, extent) in call
            .summary
            .availability
            .incoming
            .iter()
            .map(|requirement| {
                (
                    &requirement.region,
                    Some(requirement.kind),
                    false,
                    requirement.extent,
                )
            })
            .chain([
                (
                    &call.summary.availability.reinitialized,
                    None,
                    true,
                    AccessExtent::Typed,
                ),
                (
                    &call.summary.availability.unavailable,
                    None,
                    false,
                    AccessExtent::Typed,
                ),
            ])
        {
            let mut target_clauses = Vec::new();
            let mut invalidated = NativeValidity::default();
            for clause in region.clauses() {
                let Some(guard) = instantiations.guard(self, &clause.guard)? else {
                    continue;
                };
                let source =
                    SourceExpr::from_place(&clause.payload).expect("verified availability source");
                if let ExternalOrigin::Input(input) = &source.source.origin
                    && matches!(input.origin(), InputOrigin::Place(_))
                    && input.dereferences().is_empty()
                    && source.source.dereferences().is_empty()
                    && let Some(arg) = inputs.args.get(input.param() as usize)
                    && state.value(arg.value).shape().direct(self.db).is_none()
                {
                    continue;
                }
                let mut target = instantiations.resolve(self, &source, guard.scope())?;
                // A by-value effect provider can expose its representation as
                // an input place. Its logical holder was already validated and
                // transferred by the caller's operand access. Only addressable
                // referents survive as callee memory pre/postconditions.
                target.region = RegionSet::new(
                    target.region.scope(),
                    target
                        .region
                        .clauses()
                        .iter()
                        .filter(|clause| !matches!(clause.payload.root, RegionRoot::Value(_)))
                        .cloned(),
                );
                if target.region.is_empty() {
                    continue;
                }
                invalidated |= target.invalidated;
                if kind.is_some_and(|kind| kind != MemoryAccessKind::Write) {
                    let ty = source.referent_ty(self.db, call.instance).ok_or_else(|| {
                        self.internal_diag(
                            inputs.origin,
                            "availability has no typed referent".into(),
                        )
                    })?;
                    let shape = self.shape(ty)?;
                    if shape.contains_capability(self.db) {
                        let contents = self.read_region(
                            state,
                            &target.region,
                            shape,
                            ValueOccurrence::Value(result),
                            inputs.origin,
                        )?;
                        invalidated |=
                            self.value_validity(&contents, ValueOccurrence::Value(result));
                    }
                }
                let target = target
                    .region
                    .restricted(&guard, |left, right| cache.borrow_mut().and(left, right))
                    .close_existentials(&scope);
                if kind.is_none() && !definite {
                    let ty = source.referent_ty(self.db, call.instance).ok_or_else(|| {
                        self.internal_diag(
                            inputs.origin,
                            "consumed memory has no typed referent".into(),
                        )
                    })?;
                    let shape = self.shape(ty)?;
                    if shape.contains_capability(self.db) {
                        resolved.consumed.push((target.clone(), shape));
                    }
                }
                if !definite || target.definite_write().is_some() {
                    target_clauses.extend(target.clauses().iter().cloned());
                }
            }
            let target_region = RegionSet::new(&scope, target_clauses);
            if let Some(kind) = kind {
                resolved.incoming.push((
                    AvailabilityRequirement {
                        kind,
                        extent: self.instantiate_extent(extent, inputs),
                        region: target_region,
                    },
                    invalidated,
                ));
            } else if definite {
                resolved.reinitialized = target_region;
            } else {
                resolved.unavailable = target_region;
            }
        }
        Ok(Some(resolved))
    }

    /// The availability diagnostic of the published states, computed once.
    pub(super) fn availability_diagnostic(&self) -> Option<SemanticDiagnostic<'db>> {
        self.availability_diagnostic
            .get_or_init(|| self.analyze_availability().diagnostic)
            .clone()
    }

    pub fn analyze_availability(&self) -> AvailabilityAnalysis<'db> {
        #[cfg(feature = "borrowck-profile")]
        let profile = self.profile_scope("availability");
        let mut entries = vec![None; self.body.blocks.len()];
        entries[self.body.entry.index()] = Some(AvailabilityState::new());
        // Revisit only blocks whose entry state changed: an unchanged block
        // would rejoin the same edges into its successors.
        let mut dirty = vec![false; self.body.blocks.len()];
        dirty[self.body.entry.index()] = true;
        while dirty.contains(&true) {
            #[cfg(feature = "borrowck-profile")]
            profile.sweep(self.inventory.loans.len(), self.source_generation);
            for (block_index, block) in self.body.blocks.iter().enumerate() {
                if !std::mem::take(&mut dirty[block_index]) {
                    continue;
                }
                let Some(mut state) = entries[block_index].clone() else {
                    continue;
                };
                for (index, _) in self.before[block_index].iter().enumerate() {
                    #[cfg(feature = "borrowck-profile")]
                    profile.point("evaluate", block_index, index);
                    self.evaluate_availability(&mut state, block_index, index, None);
                }
                let Some(terminal) = &self.terminal[block_index] else {
                    continue;
                };
                for successor in block.terminator.kind.successors() {
                    let Some(guard) = self.edge_guard(NBlockId::new(block_index), successor) else {
                        continue;
                    };
                    let mut edge = state.clone();
                    if edge.restrict(&guard).is_none() {
                        continue;
                    }
                    for (index, access) in block
                        .terminator
                        .kind
                        .access(self.db, &self.body)
                        .into_iter()
                        .chain(successor.accesses(self.db, &self.body))
                        .enumerate()
                    {
                        let access = self.resolve_access(terminal, access, block.terminator.origin);
                        if access.kind == MemoryAccessKind::Move {
                            edge.consume(
                                (block_index, block.statements.len(), index),
                                access.region,
                                access.origin,
                            );
                        }
                    }
                    for (parameter, argument) in self.body.blocks[successor.block.index()]
                        .params
                        .iter()
                        .zip(&successor.args)
                    {
                        self.initialize_availability(
                            &mut edge,
                            &RegionSet::singleton(
                                &BinderScope::default(),
                                RegionRoot::Value(*parameter),
                                RegionPath::default(),
                            ),
                        );
                        for (site, fact) in &state.moved {
                            let region = RegionSet::new(
                                fact.region.scope(),
                                fact.region
                                    .clauses()
                                    .iter()
                                    .filter(|clause| {
                                        clause.payload.root == RegionRoot::Value(argument.value)
                                    })
                                    .cloned()
                                    .map(|mut clause| {
                                        clause.payload.root = RegionRoot::Value(*parameter);
                                        clause
                                    }),
                            );
                            edge.consume(*site, region, fact.origin);
                        }
                    }
                    if let Some(iteration) = self
                        .inventory
                        .loops
                        .feedback(NBlockId::new(block_index), successor.block)
                    {
                        let repeats_index =
                            |index| self.inventory.loops.repeats_index(iteration, index);
                        let repeats_occurrence = |occurrence| {
                            self.inventory
                                .loops
                                .repeats_occurrence(iteration, occurrence)
                        };
                        for fact in edge.moved.values_mut() {
                            fact.region = fact.region.forget_iteration(
                                self.db,
                                repeats_index,
                                repeats_occurrence,
                            );
                        }
                        // Forgetting a witness is safe for may facts, but cannot
                        // turn a previous iteration's write into a must fact.
                        edge.initialized = RegionSet::new(
                            edge.initialized.scope(),
                            edge.initialized
                                .clauses()
                                .iter()
                                .filter(|clause| {
                                    !clause
                                        .guard
                                        .indices()
                                        .into_iter()
                                        .chain(clause.payload.root.indices())
                                        .chain(clause.payload.path.indices())
                                        .any(repeats_index)
                                        && !clause
                                            .guard
                                            .occurrences()
                                            .into_iter()
                                            .any(repeats_occurrence)
                                })
                                .cloned(),
                        );
                        edge.guard = edge
                            .guard
                            .forget_indices(|index| {
                                self.inventory.loops.drops_fact(iteration, index)
                            })
                            .forget_occurrences(repeats_occurrence);
                    }
                    edge.moved.retain(|_, fact| !fact.region.is_empty());
                    let entry = &mut entries[successor.block.index()];
                    if let Some(previous) = entry {
                        let old = previous.clone();
                        #[cfg(feature = "borrowck-profile")]
                        profile.point("join", block_index, 0);
                        previous.join(edge);
                        dirty[successor.block.index()] |= *previous != old;
                    } else {
                        *entry = Some(edge);
                        dirty[successor.block.index()] = true;
                    }
                }
            }
        }
        let mut analysis = AvailabilityAnalysis {
            summary: AvailabilitySummary::empty(),
            native_validity: NativeValidity::default(),
            diagnostic: None,
            report_errors: true,
        };
        let mut returned: Option<AvailabilityState<'db>> = None;
        for (block_index, block) in self.body.blocks.iter().enumerate() {
            let Some(mut state) = entries[block_index].clone() else {
                continue;
            };
            for (index, _) in self.before[block_index].iter().enumerate() {
                analysis.report_errors = !self.validation_dependencies[block_index][index];
                self.evaluate_availability(&mut state, block_index, index, Some(&mut analysis));
            }
            analysis.report_errors =
                !self.validation_dependencies[block_index][block.statements.len()];
            let Some(terminal) = &self.terminal[block_index] else {
                continue;
            };
            if let Some(access) = block.terminator.kind.access(self.db, &self.body) {
                let access = self.resolve_access(terminal, access, block.terminator.origin);
                self.require_access(&mut analysis, &state, terminal, &access);
                if access.kind == MemoryAccessKind::Move {
                    state.consume(
                        (block_index, block.statements.len(), 0),
                        access.region,
                        access.origin,
                    );
                }
            }
            for successor in block.terminator.kind.successors() {
                let Some(guard) = self.edge_guard(NBlockId::new(block_index), successor) else {
                    continue;
                };
                let mut edge = state.clone();
                if edge.restrict(&guard).is_none() {
                    continue;
                }
                for (index, access) in successor.accesses(self.db, &self.body).enumerate() {
                    let access = self.resolve_access(terminal, access, block.terminator.origin);
                    self.require_access(&mut analysis, &edge, terminal, &access);
                    if access.kind == MemoryAccessKind::Move {
                        edge.consume(
                            (block_index, block.statements.len(), index + 1),
                            access.region,
                            access.origin,
                        );
                    }
                }
            }
            if matches!(block.terminator.kind, NTerminatorKind::Return(_)) {
                if let Some(previous) = &mut returned {
                    previous.join(state);
                } else {
                    returned = Some(state);
                }
            }
        }
        if let Some(returned) = returned {
            // Call poststates apply only after a normal return. A write whose
            // guard covers every returning path is therefore unconditional at
            // the caller, even when other paths loop or do not return.
            analysis.summary.reinitialized = RegionSet::new(
                returned.initialized.scope(),
                returned.initialized.clauses().iter().map(|clause| {
                    let return_guard = returned.guard.in_scope(clause.guard.scope());
                    Guarded {
                        guard: if return_guard.implies(&clause.guard) {
                            Guard::always(clause.guard.scope())
                        } else {
                            clause.guard.clone()
                        },
                        payload: clause.payload.clone(),
                    }
                }),
            );
            for fact in returned.moved.values() {
                analysis.summary.unavailable = analysis.summary.unavailable.union(&fact.region);
            }
        }
        analysis.summary.incoming.sort();
        analysis.summary.incoming.dedup();
        analysis
    }

    fn initialize_availability(
        &self,
        state: &mut AvailabilityState<'db>,
        written: &RegionSet<'db>,
    ) {
        // Capability-free Copy storage cannot acquire its own moved fact. Byte
        // writes cannot reinitialize differently typed moved aggregates either.
        // Native slots still need restoration facts even when their handles are
        // Copy, so calls do not require validity before a guaranteed typed store.
        let exported = RegionSet::new(
            written.scope(),
            written
                .clauses()
                .iter()
                .filter(|clause| {
                    let RegionRoot::External(source) = &clause.payload.root else {
                        return false;
                    };
                    !matches!(source.origin, ExternalOrigin::Local(_))
                        && !source.is_fresh_allocation()
                        && (!ty_is_copy(
                            self.db,
                            self.instance
                                .key(self.db)
                                .impl_env(self.db)
                                .normalization_scope(self.db),
                            source.contract.ty,
                            self.instance.assumptions(self.db),
                        ) || self
                            .shape(source.contract.ty)
                            .expect("resolved storage has an admitted capability shape")
                            .contains_capability(self.db))
                })
                .cloned(),
        );
        state.initialize(written, &exported);
    }

    /// One transfer for fixed-point propagation and diagnostic/summary replay.
    /// Provenance remains a pre-operation snapshot, while ownership consumption
    /// precedes callee entry requirements and all writes/result initialization.
    fn evaluate_availability(
        &self,
        state: &mut AvailabilityState<'db>,
        block: usize,
        index: usize,
        mut analysis: Option<&mut AvailabilityAnalysis<'db>>,
    ) {
        let statement = &self.body.blocks[block].statements[index];
        let operation = &self.operations[block][index];
        let borrow_state = &self.before[block][index];
        if let Some(analysis) = analysis.as_deref_mut() {
            analysis.native_validity |= operation.native_validity.clone();
            for call in &operation.calls {
                analysis.native_validity |= call.invalidated.clone();
            }
            if analysis.report_errors
                && !self.call_validation_pending(block, index)
                && (operation.native_validity.invalid
                    || operation.calls.iter().any(|call| call.invalidated.invalid))
                && analysis.diagnostic.is_none()
            {
                analysis.diagnostic = Some(self.invalidated_diag(statement.origin));
            }
            // Address evaluation and nonconsuming operand uses see the incoming
            // state. Synthetic reads need not precede operands in the vector.
            for phase in [AccessPhase::Address, AccessPhase::Operand] {
                for access in &operation.accesses {
                    if access.phase == phase && access.kind != MemoryAccessKind::Move {
                        self.require_access(analysis, state, borrow_state, access);
                    }
                }
            }
        }
        // Only consumption mutates this atomic batch. Each next move is checked
        // against prior consuming regions, preserving guards and selected paths.
        // This establishes nonduplication without imposing an order on address
        // computation, reads, or writes.
        for (access_index, access) in operation.accesses.iter().enumerate() {
            if access.kind != MemoryAccessKind::Move {
                continue;
            }
            debug_assert_eq!(access.phase, AccessPhase::Operand);
            if let Some(analysis) = analysis.as_deref_mut() {
                self.require_access(analysis, state, borrow_state, access);
            }
            state.consume(
                (block, index, access_index),
                access.region.clone(),
                access.origin,
            );
        }
        if let Some(analysis) = analysis.as_deref_mut()
            && self.call_validation_pending(block, index)
        {
            analysis.report_errors = false;
        }
        if let Some(analysis) = analysis.as_deref_mut()
            && let Some(call) = &operation.availability
        {
            for (requirement, invalidated) in &call.incoming {
                self.require_available(
                    analysis,
                    state,
                    borrow_state,
                    requirement.kind,
                    requirement.footprint(),
                    statement.origin,
                );
                analysis.native_validity |= invalidated.clone();
                if analysis.report_errors && invalidated.invalid && analysis.diagnostic.is_none() {
                    analysis.diagnostic = Some(self.invalidated_diag(statement.origin));
                }
            }
        }
        state.birth_allocations(&operation.births);
        for access in &operation.accesses {
            if access.phase == AccessPhase::Write {
                debug_assert_eq!(access.kind, MemoryAccessKind::Write);
                if let Some(analysis) = analysis.as_deref_mut() {
                    self.require_access(analysis, state, borrow_state, access);
                }
                if let Some(write) = access.region.definite_write() {
                    self.initialize_availability(state, write.region());
                }
            }
        }
        if let Some(call) = &operation.availability {
            self.initialize_availability(state, &call.reinitialized);
            state.consume(
                (block, index, operation.accesses.len()),
                call.unavailable.clone(),
                statement.origin,
            );
        }
        if let NStatementKind::Define { result, .. } = statement.kind {
            self.initialize_availability(
                state,
                &RegionSet::singleton(
                    &BinderScope::default(),
                    RegionRoot::Value(result),
                    RegionPath::default(),
                ),
            );
        }
    }

    fn require_access(
        &self,
        analysis: &mut AvailabilityAnalysis<'db>,
        state: &AvailabilityState<'db>,
        borrow_state: &BorrowState<'db>,
        access: &ResolvedAccess<'db>,
    ) {
        analysis.native_validity |= access.invalidated.clone();
        self.require_available(
            analysis,
            state,
            borrow_state,
            access.kind,
            AccessFootprint::typed(&access.region),
            access.origin,
        );
        if analysis.diagnostic.is_none() {
            if access.forbidden_move {
                analysis.diagnostic = Some(self.diag(
                    SemanticDiagnosticKind::MoveConflict,
                    access.origin,
                    "cannot move out of a view parameter or through a borrow handle".into(),
                ));
            } else if analysis.report_errors && access.invalidated.invalid {
                analysis.diagnostic = Some(self.invalidated_diag(access.origin));
            }
        }
    }

    fn require_available(
        &self,
        analysis: &mut AvailabilityAnalysis<'db>,
        state: &AvailabilityState<'db>,
        borrow_state: &BorrowState<'db>,
        kind: MemoryAccessKind,
        footprint: AccessFootprint<'_, 'db>,
        origin: SemOrigin<'db>,
    ) {
        if footprint.extent == AccessExtent::Bytes(IndexExpr::Const(0)) {
            return;
        }
        let region = footprint.region.with_guard(&state.guard);
        let certified = if footprint.extent == AccessExtent::Typed {
            borrow_state.certified_initialized_region(&region)
        } else {
            RegionSet::empty(region.scope())
        };
        // Logical SSA holders have no address. Unknown memory effects cannot
        // change their ownership, so these checks never depend on specialization.
        let independent = region
            .clauses()
            .iter()
            .all(|clause| matches!(clause.payload.root, RegionRoot::Value(_)));
        if (analysis.report_errors || independent)
            && analysis.diagnostic.is_none()
            && let Some(fact) = state.moved.values().find(|fact| {
                if kind != MemoryAccessKind::Write {
                    // Ownership still exists for an empty representation.
                    // Exact structural overlap is a logical conflict even when
                    // the corresponding physical footprint touches no bytes.
                    return (footprint.extent == AccessExtent::Typed
                        && !region.proven_intersection(&fact.region).is_empty())
                        || !matches!(
                            AccessFootprint {
                                region: &region,
                                extent: footprint.extent
                            }
                            .overlap(self.db, AccessFootprint::typed(&fact.region)),
                            OverlapResult::Disjoint
                        );
                }
                // Requirements are conjunctive. A whole-value write elsewhere
                // cannot authorize an earlier field write through a moved owner.
                region.clauses().iter().any(|write| {
                    let written = RegionSet::new(region.scope(), [write.clone()]);
                    fact.region.clauses().iter().any(|moved| {
                        let mut unavailable = RegionSet::new(fact.region.scope(), [moved.clone()]);
                        if write.guard.scope() == region.scope() {
                            unavailable = unavailable.with_guard(&write.guard);
                        }
                        ((footprint.extent == AccessExtent::Typed
                            && !written.proven_intersection(&unavailable).is_empty())
                            || !matches!(
                                AccessFootprint {
                                    region: &written,
                                    extent: footprint.extent
                                }
                                .overlap(self.db, AccessFootprint::typed(&unavailable)),
                                OverlapResult::Disjoint
                            ))
                            && !written.provably_covers(&unavailable)
                    })
                })
            })
        {
            let message = if kind == MemoryAccessKind::Write {
                "cannot write through a moved value"
            } else {
                "cannot use a value after it was moved"
            };
            let mut diagnostic =
                self.diag(SemanticDiagnosticKind::MoveConflict, origin, message.into());
            diagnostic.push_secondary(
                "value is moved here".into(),
                SemanticDiagnosticSpan::OriginWithTemplateFallback {
                    owner: self.instance.key(self.db).owner(self.db),
                    template_owner: self.body.template_owner,
                    origin: fact.origin,
                },
            );
            analysis.diagnostic = Some(diagnostic);
        }
        // Local reads still need diagnostics, but cannot be caller preconditions.
        // Fresh allocations cannot impose incoming obligations either. Filtering
        // them here also avoids subtracting unrelated byte-offset writes.
        let region = RegionSet::new(region.scope(), region.clauses().iter().filter(|clause| {
            matches!(&clause.payload.root, RegionRoot::External(source) if !matches!(source.origin, ExternalOrigin::Local(_)) && !source.is_fresh_allocation())
        }).cloned());
        let region = if footprint.extent == AccessExtent::Typed {
            region
                .remove_covered(&state.initialized)
                .remove_covered(&certified)
        } else {
            region
        };
        if !region.is_empty() {
            analysis.summary.incoming.push(AvailabilityRequirement {
                kind: if kind == MemoryAccessKind::Write {
                    kind
                } else {
                    MemoryAccessKind::Read
                },
                region,
                extent: footprint.extent,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::{
            semantic::{
                FieldIndex, VariantIndex,
                borrowck::solver::BorrowSummaryMode,
                capability::{
                    external::{AddressProvenance, ExternalSource},
                    guard::ChoiceKey,
                    handle::{
                        AddressOccurrence, HandleAddressSpace, OpaqueHandleContract,
                        OpaqueHandleRef,
                    },
                    path::{Projection, StructuralPath},
                    source::InputSource,
                    test_roots,
                },
                get_or_build_semantic_instance, identity_semantic_instance_key,
                normalized::{NExpr, normalize_semantic_body, verify_normalized_body},
            },
            ty::{ProviderAddressSpace, ty_check::BodyOwner, ty_def::TyId},
        },
        test_db::{HirAnalysisTestDb, find_func},
    };
    use std::slice;

    #[test]
    fn birth_composition_preserves_histories_choices_and_exit_moves() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("birth_composition.fe".into(), "fn anchor() {}");
        let (module, _) = db.top_mod(file);
        let origin = SemOrigin::Body(BodyOwner::Func(find_func(&db, module, "anchor")));
        let scope = BinderScope::default();
        let generation = IndexExpr::Runtime(NValueId::new(0));
        let word = TyId::u256(&db);
        for outer in 0..2 {
            for choice in 0..2 {
                for variant in 0..2 {
                    let allocation = OpaqueHandleRef {
                        contract: OpaqueHandleContract {
                            handle_ty: TyId::ptr_to(&db, word),
                            target_ty: word,
                            address_space: HandleAddressSpace::Known(ProviderAddressSpace::Memory),
                        },
                        occurrence: AddressOccurrence::Summary(choice),
                        arguments: Box::new([IndexExpr::Const(outer), generation]),
                        provenance: AddressProvenance::Raw,
                    };
                    let guard = Guard::always(&scope)
                        .with_variant(
                            ChoiceKey::new(ValueOccurrence::Summary, StructuralPath::default()),
                            VariantIndex(variant),
                        )
                        .unwrap();
                    let source = ExternalSource::allocation(&db, allocation.clone());
                    let current = RegionSet::singleton(
                        &scope,
                        RegionRoot::External(source.clone()),
                        RegionPath::default(),
                    )
                    .with_guard(&guard);
                    let previous =
                        current.forget_iteration(&db, |index| index == generation, |_| false);
                    let birth = AllocationBirth::from_source(&source, guard.clone()).unwrap();
                    let mut different_choice = allocation.clone();
                    different_choice.occurrence = AddressOccurrence::Summary(1 - choice);
                    let mut different_outer = allocation;
                    different_outer.arguments[0] = IndexExpr::Const(1 - outer);
                    let others: Vec<_> = [different_choice, different_outer]
                        .into_iter()
                        .map(|allocation| {
                            RegionSet::singleton(
                                &scope,
                                RegionRoot::External(ExternalSource::allocation(&db, allocation)),
                                RegionPath::default(),
                            )
                            .with_guard(&guard)
                        })
                        .collect();
                    let mut state = AvailabilityState::new();
                    state.consume((0, 0, 0), previous.clone(), origin);
                    for (index, region) in others.iter().enumerate() {
                        state.consume((0, index + 1, 0), region.clone(), origin);
                    }
                    state.birth_allocations(slice::from_ref(&birth));
                    assert_eq!(
                        state.moved[&(0, 0, 0)].region.overlap(&db, &current),
                        OverlapResult::Disjoint
                    );
                    assert_ne!(
                        state.moved[&(0, 0, 0)].region.overlap(&db, &previous),
                        OverlapResult::Disjoint
                    );
                    for (index, region) in others.iter().enumerate() {
                        assert_eq!(&state.moved[&(0, index + 1, 0)].region, region);
                    }
                    let once = state.clone();
                    state.birth_allocations(slice::from_ref(&birth));
                    assert_eq!(state, once);
                    assert!(state.initialized.is_empty());
                    assert_eq!(
                        state.moved[&(0, 0, 0)].region.forget_iteration(
                            &db,
                            |index| index == generation,
                            |_| false
                        ),
                        previous
                    );
                    state.consume((1, 0, 0), current.clone(), origin);
                    assert_ne!(
                        state.moved[&(1, 0, 0)].region.overlap(&db, &current),
                        OverlapResult::Disjoint
                    );
                }
            }
        }
    }

    #[test]
    fn allocation_feedback_can_overlap_the_next_current_member() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "allocation_feedback.fe".into(),
            "struct Item { n: u256 }\nfn factory() -> *Item { core::ptr::alloc<Item>() }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "factory"))),
        );
        let body = normalize_semantic_body(&db, instance).unwrap().body;
        let result = body
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .find_map(|statement| {
                if let NStatementKind::Define {
                    result,
                    expr: NExpr::Call { .. },
                } = statement.kind
                {
                    Some(result)
                } else {
                    None
                }
            })
            .unwrap();
        let mut checker = Borrowck::new(&db, instance).unwrap();
        checker.solve().unwrap();
        assert!(
            checker.inventory.allocation_cells.is_empty(),
            "capability-free Item has no inventoried capability contents"
        );
        assert!(
            checker
                .operations
                .iter()
                .flatten()
                .any(|operation| !operation.births.is_empty()),
            "births exist independently of capability-cell inventory"
        );
        let iteration = IndexExpr::Iteration(body.entry);
        let pointer = instance.normalized_result_ty(&db);
        let allocation = ExternalSource::allocation(
            &db,
            OpaqueHandleRef {
                contract: OpaqueHandleContract {
                    handle_ty: pointer,
                    target_ty: pointer.as_ptr(&db).unwrap(),
                    address_space: HandleAddressSpace::Known(ProviderAddressSpace::Memory),
                },
                occurrence: AddressOccurrence::Value {
                    instance,
                    value: result,
                    choice: 0,
                },
                arguments: Box::new([iteration]),
                provenance: AddressProvenance::Raw,
            },
        );
        let current = RegionSet::singleton(
            &BinderScope::default(),
            RegionRoot::External(allocation.clone()),
            RegionPath::default(),
        );
        let previous = current.forget_iteration(&db, |index| index == iteration, |_| true);
        assert_ne!(previous, current);
        assert!(!matches!(
            previous.overlap(&db, &current),
            OverlapResult::Disjoint
        ));
        let birth =
            AllocationBirth::from_source(&allocation, Guard::always(current.scope())).unwrap();
        let mut state = AvailabilityState::new();
        state.consume(
            (0, 0, 0),
            previous.clone(),
            SemOrigin::Body(body.template_owner),
        );
        state.birth_allocations(std::slice::from_ref(&birth));
        let remaining = &state.moved[&(0, 0, 0)].region;
        assert!(matches!(
            remaining.overlap(&db, &current),
            OverlapResult::Disjoint
        ));
        assert!(!matches!(
            remaining.overlap(&db, &previous),
            OverlapResult::Disjoint
        ));
        assert!(
            state.initialized.is_empty(),
            "birth is not typed initialization"
        );
        let once = state.clone();
        state.birth_allocations(std::slice::from_ref(&birth));
        assert_eq!(state, once, "reapplying a birth is idempotent");
        // Feedback projects the now-unobserved previous current-generation
        // witness, rather than accumulating a longer disequality history.
        let forgotten = state.moved[&(0, 0, 0)].region.forget_iteration(
            &db,
            |index| index == iteration,
            |_| true,
        );
        assert_eq!(forgotten, previous);
        state.consume(
            (0, 1, 0),
            current.clone(),
            SemOrigin::Body(body.template_owner),
        );
        assert!(
            !matches!(
                state.moved[&(0, 1, 0)].region.overlap(&db, &current),
                OverlapResult::Disjoint
            ),
            "callee exit consumption must survive birth"
        );
    }

    #[test]
    fn operation_operands_reject_duplicate_moves_in_verified_normalized_ir() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "operation_consumption.fe".into(),
            "struct Item { n: u256 }\n\
             fn pair(first: own Item, second: own Item) -> (Item, Item) { (first, second) }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "pair"))),
        );
        let body = normalize_semantic_body(&db, instance).unwrap().body;
        let mut checker =
            Borrowck::new_with_body(&db, instance, body, BorrowSummaryMode::Final).unwrap();
        checker.solve().unwrap();
        assert!(checker.availability_diagnostic().is_none());
        assert!(checker.availability_diagnostic().is_none());
        let (block, index) =
            checker
                .body
                .blocks
                .iter()
                .enumerate()
                .find_map(|(block, body)| {
                    body.statements.iter().position(|statement| matches!(
                    &statement.kind,
                    NStatementKind::Define { expr: NExpr::AggregateMake { fields, .. }, .. }
                        if fields.len() == 2
                )).map(|index| (block, index))
                })
                .unwrap();
        let NStatementKind::Define {
            expr: NExpr::AggregateMake { fields, .. },
            ..
        } = &mut checker.body.blocks[block].statements[index].kind
        else {
            unreachable!();
        };
        let original = fields[1].value;
        fields[1].value = fields[0].value;
        verify_normalized_body(&db, &checker.body).unwrap();
        checker.solve().unwrap();
        let diagnostic = checker.availability_diagnostic().unwrap();
        assert_eq!(diagnostic.kind, SemanticDiagnosticKind::MoveConflict);
        assert_eq!(diagnostic.secondaries.len(), 1);
        assert_ne!(diagnostic.primary.span, diagnostic.secondaries[0].span);
        assert!(checker.build_summary().is_err());
        let NStatementKind::Define {
            expr: NExpr::AggregateMake { fields, .. },
            ..
        } = &mut checker.body.blocks[block].statements[index].kind
        else {
            unreachable!();
        };
        fields[1].value = original;
        checker.solve().unwrap();
        assert!(checker.availability_diagnostic().is_none());
        assert!(checker.build_summary().is_ok());
    }

    #[test]
    fn operation_operands_respect_guarded_regions_and_availability_phases() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "guarded_operation_consumption.fe".into(),
            "struct Item { n: u256 }\nstruct Pair { left: Item, right: Item }\n\
             fn pair(first: own Pair, second: own Pair) -> (Pair, Pair) { (first, second) }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "pair"))),
        );
        let mut checker = Borrowck::new(&db, instance).unwrap();
        checker.solve().unwrap();
        let (block, index) = checker.body.blocks.iter().enumerate().find_map(|(block, body)| {
            body.statements.iter().position(|statement| matches!(&statement.kind,
                NStatementKind::Define { expr: NExpr::AggregateMake { fields, .. }, .. } if fields.len() == 2
            )).map(|index| (block, index))
        }).unwrap();
        let original = checker.operations[block][index].clone();
        let scope = BinderScope::default();
        let whole = original.accesses[0].region.clone();
        let field = |index| whole.project(&RegionPath::new([Projection::Field(FieldIndex(index))]));
        let selector = IndexExpr::Runtime(NValueId::new(0));
        let selected = |index| {
            whole.with_guard(
                &Guard::always(&scope)
                    .with_equality(selector, IndexExpr::Const(index))
                    .unwrap(),
            )
        };
        for (first, second, conflict) in [
            (whole.clone(), whole.clone(), true),
            (whole.clone(), field(0), true),
            (field(0), field(0), true),
            (field(0), field(1), false),
            (selected(0), selected(1), false),
        ] {
            let operation = &mut checker.operations[block][index];
            *operation = original.clone();
            operation.accesses[0].region = first;
            operation.accesses[1].region = second;
            assert_eq!(
                checker.analyze_availability().diagnostic.is_some(),
                conflict
            );
            assert_eq!(checker.build_summary().is_err(), conflict);
        }

        let operation = &mut checker.operations[block][index];
        *operation = original;
        let mut carrier = operation.accesses[0].clone();
        carrier.kind = MemoryAccessKind::Read;
        carrier.phase = AccessPhase::Address;
        operation.accesses.push(carrier);
        assert!(checker.analyze_availability().diagnostic.is_none());
        // Callee entry requirements see argument ownership already consumed,
        // while the appended synthetic address read above must see it available.
        checker.operations[block][index].availability = Some(ResolvedAvailability {
            incoming: vec![(
                AvailabilityRequirement {
                    kind: MemoryAccessKind::Read,
                    extent: AccessExtent::Typed,
                    region: whole,
                },
                NativeValidity::default(),
            )],
            reinitialized: RegionSet::empty(&scope),
            unavailable: RegionSet::empty(&scope),
            consumed: Vec::new(),
        });
        assert!(checker.analyze_availability().diagnostic.is_some());
    }

    #[test]
    fn ownership_join_and_composition_cover_two_cell_executions() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("availability_model.fe".into(), "fn inspect() {}");
        let (module, _) = db.top_mod(file);
        let origin = SemOrigin::Body(BodyOwner::Func(find_func(&db, module, "inspect")));
        let scope = BinderScope::default();
        let cells = [0, 1].map(|id| {
            RegionSet::singleton(
                &scope,
                RegionRoot::External(test_roots::input(&db, InputSource::place(id))),
                RegionPath::default(),
            )
        });
        let subset = |mask: u32| {
            cells
                .iter()
                .enumerate()
                .filter(|(index, _)| mask & (1 << index) != 0)
                .fold(RegionSet::empty(&scope), |set, (_, cell)| set.union(cell))
        };
        let programs: Vec<_> = [0_u32, 1, 2, 3]
            .into_iter()
            .flat_map(|written| {
                [0_u32, 1, 2, 3].map(|moved| {
                    let mut state = AvailabilityState::new();
                    for (index, cell) in cells.iter().enumerate() {
                        if written & (1 << index) != 0 {
                            state.initialize(cell, cell);
                        }
                        if moved & (1 << index) != 0 {
                            state.consume((0, 0, index), cell.clone(), origin);
                        }
                    }
                    (written, moved, state)
                })
            })
            .collect();
        for (left_written, left_moved, left) in &programs {
            for (right_written, right_moved, right) in &programs {
                let mut joined = left.clone();
                joined.join(right.clone());
                for before in [0_u32, 1, 2, 3] {
                    let mut abstract_join = subset(before).remove_covered(&joined.initialized);
                    for fact in joined.moved.values() {
                        abstract_join = abstract_join.union(&fact.region);
                    }
                    let concrete_left = (before & !left_written) | left_moved;
                    let concrete_right = (before & !right_written) | right_moved;
                    assert!(abstract_join.provably_covers(&subset(concrete_left | concrete_right)));

                    let mut composed = subset(before).remove_covered(&left.initialized);
                    for fact in left.moved.values() {
                        composed = composed.union(&fact.region);
                    }
                    composed = composed.remove_covered(&right.initialized);
                    for fact in right.moved.values() {
                        composed = composed.union(&fact.region);
                    }
                    let concrete_composed = (concrete_left & !right_written) | right_moved;
                    assert_eq!(composed, subset(concrete_composed));
                }
            }
        }
    }
}
