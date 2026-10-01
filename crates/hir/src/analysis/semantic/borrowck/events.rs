//! Access checking over the converged structural state and shared regions.
use super::validity::NativeValidity;
use crate::analysis::semantic::diagnostics::{
    BlockedSemanticBody, SemanticDiagnostic, SemanticDiagnosticId, SemanticDiagnosticKind,
    SemanticDiagnosticSpan,
};
use std::{
    cell::OnceCell,
    collections::{BTreeMap, BTreeSet},
};

use cranelift_entity::EntityRef;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        BorrowActivation, SemOrigin,
        capability::{
            external::{ExternalOrigin, ExternalSource, ReferentContract},
            footprint::{AccessExtent, AccessFootprint, FootprintPair},
            guard::{Guard, ValueOccurrence},
            handle::HandleAddressSpace,
            index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
            loan::{CapabilityRef, LoanDef, LoanRef},
            path::{Projection, RegionPath},
            region::{OverlapResult, RegionRoot, RegionSet, SymbolicPlace},
            semantics::{CapabilityClass, CapabilitySemantics},
            separation::{SEPARATION_PAIR_LIMIT, Separation, SeparationSet},
            source::SourceExpr,
            state::{BorrowState, CapabilityValue},
            value::{Guarded, IndexPayload},
        },
        normalized::{
            NEffectArgValue, NExpr, NPlace, NPlaceBase, NRootId, NStatementKind, NValueDefinition,
            NValueId, NormalizedBody, access::AccessTarget,
        },
    },
    ty::{
        corelib::MemoryAccessKind,
        ty_def::{BorrowKind, TyId},
    },
};
use crate::semantic::ProviderSource;

use super::{
    access::effect_occurrence,
    ir::{LocalBorrowCheck, SemanticBorrowCheckResult, SeparationOrigin},
    memory::ResolvedSeparation,
    solver::{Borrowck, Resolution},
    summary::CallInputs,
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum OccurrenceStep<'db> {
    Data(Projection<IndexExpr<'db>>),
    Target,
}

#[derive(Clone, Copy)]
pub(super) enum CapabilityTraversal {
    Held,
    Reachable,
    Effect(BorrowKind),
}

#[derive(Clone)]
pub(super) struct CapabilityOccurrence<'db> {
    pub semantics: CapabilitySemantics<'db>,
    pub access: Option<BorrowKind>,
    path: Vec<OccurrenceStep<'db>>,
    guard: Guard<'db>,
    payload: CapabilityRef<'db>,
    pub region: RegionSet<'db>,
    suspended: RegionSet<'db>,
}

impl<'db> CapabilityOccurrence<'db> {
    pub(super) fn authorizer(&self) -> Option<Guarded<'db, LoanRef<'db>>> {
        Some(Guarded {
            guard: self.guard.clone(),
            payload: self.payload.loan()?.clone(),
        })
    }

    fn substitute(&self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        Self {
            semantics: self.semantics,
            access: self.access,
            path: self
                .path
                .iter()
                .map(|step| match step {
                    OccurrenceStep::Target => OccurrenceStep::Target,
                    OccurrenceStep::Data(Projection::Index(index)) => {
                        OccurrenceStep::Data(Projection::Index(subst.apply(*index)))
                    }
                    OccurrenceStep::Data(step) => OccurrenceStep::Data(*step),
                })
                .collect(),
            guard: self.guard.substitute(subst).expect("fresh occurrence"),
            payload: self.payload.substitute(db, subst),
            region: self.region.substitute(db, subst),
            suspended: self.suspended.substitute(db, subst),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Live {
    values: BTreeSet<NValueId>,
    roots: BTreeSet<NRootId>,
}

impl Live {
    fn join(&mut self, other: &Self) {
        self.values.extend(&other.values);
        self.roots.extend(&other.roots);
    }
    fn statement(
        &mut self,
        db: &dyn HirAnalysisDb,
        body: &NormalizedBody<'_>,
        statement: &NStatementKind<'_>,
    ) {
        if let NStatementKind::Define { result, .. } = statement {
            self.values.remove(result);
        }
        for access in statement.accesses(db, body) {
            match access.target {
                AccessTarget::Value { operand, .. } => {
                    self.values.insert(operand.value);
                }
                AccessTarget::Place(place) => match place.base {
                    NPlaceBase::Root(root) => {
                        if access.kind == MemoryAccessKind::Write && place.path.is_empty() {
                            self.roots.remove(&root);
                        } else {
                            self.roots.insert(root);
                        }
                    }
                    NPlaceBase::CapabilityTarget { carrier } => {
                        self.values.insert(carrier);
                    }
                },
            }
        }
    }
}

impl<'db> Borrowck<'db> {
    pub fn check(&mut self) -> Result<Option<BlockedSemanticBody<'db>>, SemanticDiagnostic<'db>> {
        // The final summary may already have solved this body.
        if !self.is_solved() {
            self.solve()?;
        }
        if self.blocked.is_some() {
            return Ok(self.blocked.clone());
        }
        if self
            .instance
            .key(self.db)
            .owner(self.db)
            .body(self.db)
            .is_none()
            && self.intrinsic_summary()?.is_none()
        {
            self.pending.callees.insert(self.instance.key(self.db));
        }
        if let Some(diagnostic) = &self.conflicts().diagnostic {
            return Err(diagnostic.clone());
        }
        if let Some(diagnostic) = self.availability_diagnostic() {
            return Err(diagnostic);
        }
        Ok(None)
    }

    pub(super) fn local_check(&mut self) -> LocalBorrowCheck<'db> {
        let result = match self.check() {
            Ok(Some(blocked)) => SemanticBorrowCheckResult::Blocked(blocked),
            Err(diag) => SemanticBorrowCheckResult::Err(SemanticDiagnosticId::new(self.db, diag)),
            Ok(None) if self.pending.callees.is_empty() => SemanticBorrowCheckResult::Ok,
            Ok(None) => SemanticBorrowCheckResult::Pending(self.pending.clone()),
        };
        LocalBorrowCheck {
            result,
            callees: self
                .calls
                .values()
                .filter(|call| !call.pending)
                .map(|call| call.instance)
                .collect(),
        }
    }

    fn live_before(&self) -> Vec<Vec<Live>> {
        let mut entry = vec![Live::default(); self.body.blocks.len()];
        let mut before: Vec<Vec<_>> = self
            .body
            .blocks
            .iter()
            .map(|block| vec![Live::default(); block.statements.len()])
            .collect();
        loop {
            let mut changed = false;
            for (index, block) in self.body.blocks.iter().enumerate().rev() {
                let mut live = Live::default();
                for successor in block
                    .terminator
                    .kind
                    .successors()
                    .into_iter()
                    .filter(|_| self.terminal[index].is_some())
                {
                    let mut edge = entry[successor.block.index()].clone();
                    let parameters = &self.body.blocks[successor.block.index()].params;
                    let arguments: Vec<_> = parameters
                        .iter()
                        .zip(&successor.args)
                        .filter_map(|(param, arg)| edge.values.contains(param).then_some(arg.value))
                        .collect();
                    for parameter in parameters {
                        edge.values.remove(parameter);
                    }
                    edge.values.extend(arguments);
                    live.join(&edge);
                }
                if self.terminal[index].is_some()
                    && let Some(access) = block.terminator.kind.access(self.db, &self.body)
                    && let AccessTarget::Value { operand, .. } = access.target
                {
                    live.values.insert(operand.value);
                }
                for (position, statement) in block
                    .statements
                    .iter()
                    .take(self.before[index].len())
                    .enumerate()
                    .rev()
                {
                    live.statement(self.db, &self.body, &statement.kind);
                    before[index][position] = live.clone();
                }
                changed |= entry[index] != live;
                entry[index] = live;
            }
            if !changed {
                return before;
            }
        }
    }

    pub(super) fn capabilities(
        &mut self,
        state: &BorrowState<'db>,
        value: &CapabilityValue<'db>,
        occurrence: ValueOccurrence,
        origin: SemOrigin<'db>,
        traversal: CapabilityTraversal,
    ) -> Result<Vec<CapabilityOccurrence<'db>>, SemanticDiagnostic<'db>> {
        let mut result = Vec::new();
        let mut pending = vec![(value.clone(), Vec::new(), Vec::new(), traversal)];
        let mut reached = BTreeSet::new();
        while let Some((value, prefix, ancestry, traversal)) = pending.pop() {
            let leaves = self.inventory.values.leaves(&value, occurrence);
            for leaf in &leaves {
                if leaves.iter().any(|outer| {
                    outer.semantics.class == CapabilityClass::View
                        && outer.path != leaf.path
                        && leaf.path.as_slice().starts_with(outer.path.as_slice())
                }) {
                    continue;
                }
                let path: Vec<_> = prefix
                    .iter()
                    .cloned()
                    .chain(
                        leaf.path
                            .as_slice()
                            .iter()
                            .copied()
                            .map(OccurrenceStep::Data),
                    )
                    .collect();
                let region = self.capability_region(&Guarded {
                    guard: leaf.guard.clone(),
                    payload: leaf.payload.clone(),
                });
                let invalidated = matches!(leaf.payload, CapabilityRef::Invalidated { .. });
                let access = if invalidated || leaf.semantics.target_ty.is_zero_sized(self.db) {
                    None
                } else {
                    match leaf.semantics.class {
                        CapabilityClass::Borrow(kind) => Some(kind),
                        CapabilityClass::View => Some(BorrowKind::Ref),
                        CapabilityClass::Handle | CapabilityClass::Pointer => match traversal {
                            CapabilityTraversal::Effect(kind)
                                if leaf.path.as_slice().is_empty() =>
                            {
                                Some(kind)
                            }
                            _ => None,
                        },
                    }
                };
                result.push(CapabilityOccurrence {
                    semantics: leaf.semantics,
                    access,
                    path: path.clone(),
                    guard: leaf.guard.clone(),
                    payload: leaf.payload.clone(),
                    region: region.clone(),
                    suspended: RegionSet::empty(region.scope()),
                });
                // Holding an ordinary handle does not access or hold its referent.
                // Effect arguments explicitly access their target; summary reachability
                // follows every capability using the same structural traversal.
                if invalidated
                    || (access.is_none() && !matches!(traversal, CapabilityTraversal::Reachable))
                {
                    continue;
                }
                let target = self.shape(leaf.semantics.target_ty)?;
                // Reads introduce fresh witnesses, so a recursive referent
                // returns as an alpha-variant of an ancestor region. Compare
                // regions with their witnesses closed and renumbered.
                let visited = region.close_existentials(&region.scope().without_existentials());
                if !target.contains_capability(self.db)
                    || region.is_empty()
                    || ancestry.contains(&(visited.clone(), target))
                {
                    continue;
                }
                // Reachability unions the capabilities every path reaches, and
                // contents depend only on the region read. Expand each region
                // once rather than once per path through a dense referent graph.
                if matches!(traversal, CapabilityTraversal::Reachable)
                    && !reached.insert((visited.clone(), target))
                {
                    continue;
                }
                let mut ancestry = ancestry.clone();
                ancestry.push((visited, target));
                let contents = self.read_region(state, &region, target, occurrence, origin)?;
                let mut path = path;
                path.push(OccurrenceStep::Target);
                pending.push((
                    contents,
                    path,
                    ancestry,
                    match traversal {
                        CapabilityTraversal::Reachable => CapabilityTraversal::Reachable,
                        CapabilityTraversal::Held | CapabilityTraversal::Effect(_) => {
                            CapabilityTraversal::Held
                        }
                    },
                ));
            }
        }
        Ok(result)
    }

    pub fn reachable_input(
        &mut self,
        state: &BorrowState<'db>,
        param: u32,
        target: ReferentContract<'db>,
        path: &RegionPath<IndexExpr<'db>>,
        scope: &BinderScope,
        inputs: CallInputs<'_, 'db>,
    ) -> Result<Resolution<'db>, SemanticDiagnostic<'db>> {
        let CallInputs {
            args,
            effects,
            origin,
        } = inputs;
        let occurrence = inputs.occurrence(param).ok_or_else(|| {
            self.internal_diag(origin, "reachable source has no caller occurrence".into())
        })?;
        let value = if let Some(arg) = args.get(param as usize) {
            state.value(arg.value).clone()
        } else {
            let effect = effects
                .iter()
                .find(|effect| effect.binding_idx as usize + args.len() == param as usize)
                .ok_or_else(|| {
                    self.internal_diag(origin, "reachable source has no input".into())
                })?;
            match &effect.arg {
                NEffectArgValue::Value(value) => state.value(value.value).clone(),
                NEffectArgValue::Place(place) => {
                    let region = self.resolve_region(state, place);
                    let shape = self.shape(place.ty)?;
                    self.read_region(state, &region, shape, occurrence, origin)?
                }
            }
        };
        let mut resolved = Resolution::empty(scope);
        let capabilities = self.capabilities(
            state,
            &value,
            occurrence,
            origin,
            CapabilityTraversal::Reachable,
        )?;
        self.fold_reachable(capabilities, target, Some(path), scope, &mut resolved);
        Ok(resolved)
    }

    pub fn reachable_region(
        &mut self,
        state: &BorrowState<'db>,
        region: &RegionSet<'db>,
        ty: TyId<'db>,
        target: ReferentContract<'db>,
        scope: &BinderScope,
        origin: SemOrigin<'db>,
    ) -> Result<Resolution<'db>, SemanticDiagnostic<'db>> {
        let shape = self.shape(ty)?;
        let contents = self.read_region(state, region, shape, ValueOccurrence::Summary, origin)?;
        let mut resolved = Resolution::empty(scope);
        if ty == target.ty {
            resolved.region = region.clone();
        }
        let capabilities = self.capabilities(
            state,
            &contents,
            ValueOccurrence::Summary,
            origin,
            CapabilityTraversal::Reachable,
        )?;
        self.fold_reachable(capabilities, target, None, scope, &mut resolved);
        Ok(resolved)
    }

    /// Accumulate reachable capabilities into `resolved`, retargeting abstract
    /// contract roots at `target` and projecting each region through `path`.
    fn fold_reachable(
        &self,
        capabilities: Vec<CapabilityOccurrence<'db>>,
        target: ReferentContract<'db>,
        path: Option<&RegionPath<IndexExpr<'db>>>,
        scope: &BinderScope,
        resolved: &mut Resolution<'db>,
    ) {
        for capability in capabilities {
            let subst = capability.guard.scope().freshening(scope);
            let mut capability = capability.substitute(self.db, &subst);
            if matches!(capability.payload, CapabilityRef::Invalidated { .. }) {
                resolved.invalidated |= NativeValidity::from_region(&capability.region);
                continue;
            }
            if capability.semantics.target_ty != target.ty {
                let clauses = capability.region.clauses().iter().filter_map(|clause| {
                    let original = clause.payload.root.contract()?;
                    if !original.is_abstract(self.db) {
                        return None;
                    }
                    let mut clause = clause.clone();
                    clause.payload.root = RegionRoot::External(ExternalSource::abstract_target(
                        &clause.payload.root,
                        ReferentContract::new(
                            self.db,
                            target.ty,
                            if target.address_space == HandleAddressSpace::Unspecified {
                                original.address_space
                            } else {
                                target.address_space
                            },
                        ),
                    ));
                    clause.payload.path = RegionPath::default();
                    clause.payload.views = Default::default();
                    Some(clause)
                });
                capability.region = RegionSet::new(capability.region.scope(), clauses);
            }
            let projected = path.map(|path| capability.region.project(path));
            let region = projected.as_ref().unwrap_or(&capability.region);
            resolved.region = resolved.region.union(&region.close_existentials(scope));
            if let Some(reference) = capability.payload.loan() {
                resolved.parents.push(Guarded {
                    guard: capability.guard,
                    payload: reference.clone(),
                });
            }
        }
    }

    pub(super) fn ancestors(
        &self,
        seeds: impl IntoIterator<Item = Guarded<'db, LoanRef<'db>>>,
    ) -> Vec<Guarded<'db, LoanRef<'db>>> {
        let mut found = BTreeSet::new();
        let mut pending: Vec<_> = seeds
            .into_iter()
            .map(|seed| (seed, BTreeSet::new()))
            .collect();
        while let Some((entry, mut path)) = pending.pop() {
            if !path.insert(entry.payload.id) || !found.insert(entry.clone()) {
                continue;
            }
            for parent in self.inventory.loans[entry.payload.id.0]
                .parents(&entry.payload, entry.guard.scope())
            {
                if let Some(guard) = parent
                    .guard
                    .and(&entry.guard.in_scope(parent.guard.scope()))
                {
                    pending.push((
                        Guarded {
                            guard,
                            payload: parent.payload,
                        },
                        path.clone(),
                    ));
                }
            }
        }
        found.into_iter().collect()
    }

    pub(super) fn authority(
        &self,
        state: &BorrowState<'db>,
        place: &NPlace<'db>,
    ) -> Vec<Guarded<'db, LoanRef<'db>>> {
        match place.base {
            NPlaceBase::Root(_) => Vec::new(),
            NPlaceBase::CapabilityTarget { carrier } => self.ancestors(
                state
                    .value(carrier)
                    .direct()
                    .iter()
                    .flat_map(|entry| entry.payload.authority(&entry.guard)),
            ),
        }
    }

    fn suspend_parents(&self, active: &mut [CapabilityOccurrence<'db>]) {
        let children = active.to_vec();
        for parent in active {
            let reference = parent.payload.loan().expect("active parent");
            for child in &children {
                let child_reference = child.payload.loan().expect("active child");
                if child_reference.id == reference.id {
                    continue;
                }
                for ancestor in self.ancestors(child.payload.authority(&child.guard)) {
                    if ancestor.payload.id != reference.id {
                        continue;
                    }
                    // Bind the child's parent-family arguments to this parent occurrence.
                    // Other child indices remain independent, owned witnesses.
                    let mut scope = parent.guard.scope().clone();
                    let mut bindings = BTreeMap::new();
                    for (source, target) in ancestor.payload.args.iter().zip(&reference.args) {
                        if matches!(source, IndexExpr::Bound(_)) {
                            bindings.entry(*source).or_insert(*target);
                        }
                    }
                    for index in ancestor.guard.scope().variables() {
                        bindings.entry(index).or_insert_with(|| {
                            let (nested, witness) = scope.bind(IndexNamespace::Existential);
                            scope = nested;
                            witness
                        });
                    }
                    let subst = IndexSubst::new(ancestor.guard.scope(), &scope, bindings)
                        .expect("parent occurrence scope");
                    let Some(guard) = ancestor
                        .guard
                        .substitute(&subst)
                        .and_then(|guard| guard.and(&parent.guard.in_scope(&scope)))
                        .and_then(|guard| {
                            reference.matching_guard(&ancestor.payload.substitute(&subst), guard)
                        })
                    else {
                        continue;
                    };
                    let lift = IndexSubst::new(child.region.scope(), ancestor.guard.scope(), [])
                        .expect("child ancestor witness scope");
                    let suspended = child
                        .region
                        .substitute(self.db, &lift)
                        .substitute(self.db, &subst)
                        .with_guard(&guard)
                        .close_existentials(parent.guard.scope());
                    parent.suspended = parent.suspended.union(&suspended);
                }
            }
        }
    }

    pub(super) fn resolve_conflict_facts(&mut self) -> Result<(), SemanticDiagnostic<'db>> {
        let live = self.live_before();
        for (block_index, block_live) in live.iter().enumerate() {
            let statements = self.body.blocks[block_index].statements.clone();
            for (index, statement) in statements
                .iter()
                .take(self.before[block_index].len())
                .enumerate()
            {
                let state = self.before[block_index][index].clone();
                let mut active = Vec::new();
                for holder in &block_live[index].values {
                    active.extend(self.capabilities(
                        &state,
                        state.value(*holder),
                        ValueOccurrence::Value(*holder),
                        statement.origin,
                        CapabilityTraversal::Held,
                    )?);
                }
                for root in &block_live[index].roots {
                    let region = &self.inventory.roots[root.index()];
                    if let Some((_, value)) = state.storage().find(|(key, _)| *key == region) {
                        active.extend(self.capabilities(
                            &state,
                            value,
                            ValueOccurrence::Root(*root),
                            statement.origin,
                            CapabilityTraversal::Held,
                        )?);
                    }
                }
                active.retain(|capability| {
                    let Some(reference) = capability.payload.loan() else { return false };
                    match self.inventory.loans[reference.id.0].activation() {
                        BorrowActivation::Immediate => true,
                        BorrowActivation::AtCall { call_site, callee } => matches!(
                            &statement.kind,
                            NStatementKind::Define { expr: NExpr::Call { call_site: actual_site, callee: actual_callee, .. }, .. }
                            if *actual_site == call_site && *actual_callee == callee
                        ),
                    }
                });
                self.suspend_parents(&mut active);
                let arguments = if let NStatementKind::Define {
                    expr:
                        NExpr::Call {
                            args, effect_args, ..
                        },
                    ..
                } = &statement.kind
                {
                    let mut groups = Vec::new();
                    for (argument, operand) in args.iter().enumerate() {
                        groups.push((
                            argument,
                            self.capabilities(
                                &state,
                                state.value(operand.value),
                                ValueOccurrence::Value(operand.value),
                                statement.origin,
                                CapabilityTraversal::Held,
                            )?,
                        ));
                    }
                    for effect in effect_args {
                        let value = match &effect.arg {
                            NEffectArgValue::Value(value) => state.value(value.value).clone(),
                            NEffectArgValue::Place(place) => {
                                let region = self.resolve_region(&state, place);
                                let shape = self.shape(place.ty)?;
                                self.read_region(
                                    &state,
                                    &region,
                                    shape,
                                    effect_occurrence(&effect.arg),
                                    statement.origin,
                                )?
                            }
                        };
                        groups.push((
                            args.len() + effect.binding_idx as usize,
                            self.capabilities(
                                &state,
                                &value,
                                effect_occurrence(&effect.arg),
                                statement.origin,
                                if matches!(effect.arg, NEffectArgValue::Value(_)) {
                                    CapabilityTraversal::Effect(if effect.required_mut {
                                        BorrowKind::Mut
                                    } else {
                                        BorrowKind::Ref
                                    })
                                } else {
                                    CapabilityTraversal::Held
                                },
                            )?,
                        ));
                    }
                    let all: Vec<_> = groups
                        .into_iter()
                        .flat_map(|(group, members)| {
                            members.into_iter().map(move |member| (group, member))
                        })
                        .collect();
                    all
                } else {
                    Vec::new()
                };
                self.operations[block_index][index].active = active;
                self.operations[block_index][index].arguments = arguments;
            }
        }
        Ok(())
    }

    /// The analysis published with the solved fixed point.
    pub(super) fn conflicts(&self) -> &ConflictAnalysis<'db> {
        self.conflicts.as_ref().expect("solved conflict analysis")
    }

    /// Compare every settled operation with the borrows live across it. This
    /// reads only published facts, so the check and the summary share it.
    pub(super) fn analyze_conflicts(&self) -> ConflictAnalysis<'db> {
        let mut analysis = ConflictFacts::default();
        self.collect_conflicts(&mut analysis);
        ConflictAnalysis {
            diagnostic: analysis.diagnostic,
            exhausted: analysis.exhausted,
            deferred: analysis.deferred,
            validity: analysis.validity,
        }
    }

    /// Stop at the first conflict: a body with a conflict never validates, and
    /// neither does any caller, whose check includes this body's.
    fn collect_conflicts(&self, analysis: &mut ConflictFacts<'db>) {
        for (block, operations) in self.operations.iter().enumerate() {
            for (index, operation) in operations.iter().enumerate() {
                if self.validation_dependencies[block][index] {
                    continue;
                }
                let origin = self.body.blocks[block].statements[index].origin;
                for access in &operation.accesses {
                    if access.invalidated.invalid {
                        return analysis.report(self.invalidated_diag(access.origin));
                    }
                    self.compare_access(
                        &operation.active,
                        access.conflict_kind,
                        AccessFootprint::typed(&access.region),
                        &access.authority,
                        access.origin,
                        analysis,
                    );
                    if analysis.diagnostic.is_some() {
                        return;
                    }
                }
                let pending = self.call_validation_pending(block, index);
                for resolved in operation.calls.iter().filter(|_| !pending) {
                    if resolved.invalidated.invalid {
                        return analysis.report(self.invalidated_diag(origin));
                    }
                    self.compare_access(
                        &operation.active,
                        resolved.access.kind.borrow_kind(),
                        resolved.access.footprint(),
                        &resolved.authority,
                        origin,
                        analysis,
                    );
                    if analysis.diagnostic.is_some() {
                        return;
                    }
                }
                for (position, (group, member)) in operation.arguments.iter().enumerate() {
                    for (other_group, other) in operation.arguments.iter().skip(position) {
                        if let Err(diagnostic) =
                            self.check_call_pair(*group == *other_group, member, other, origin)
                        {
                            return analysis.report(diagnostic);
                        }
                    }
                    if let Some(kind) = member.access {
                        let authority = self.ancestors(member.payload.authority(&member.guard));
                        self.compare_access(
                            &operation.active,
                            kind,
                            AccessFootprint::typed(&member.region),
                            &authority,
                            origin,
                            analysis,
                        );
                        if analysis.diagnostic.is_some() {
                            return;
                        }
                    }
                }
                // Argument conflicts explain an aliased call more directly.
                if pending {
                    continue;
                }
                if operation.separation_validity.invalid {
                    return analysis.report(self.invalidated_diag(origin));
                }
                analysis.validity |= operation.separation_validity.clone();
                for requirement in &operation.requirements {
                    self.discharge(requirement, origin, analysis);
                    if analysis.diagnostic.is_some() {
                        return;
                    }
                }
            }
        }
    }

    pub(super) fn invalidated_diag(&self, origin: SemOrigin<'db>) -> SemanticDiagnostic<'db> {
        self.diag(
            SemanticDiagnosticKind::BorrowConflict,
            origin,
            "cannot use a native borrow that is uninitialized or invalidated by a raw write".into(),
        )
    }

    fn check_call_pair(
        &self,
        same_group: bool,
        left: &CapabilityOccurrence<'db>,
        right: &CapabilityOccurrence<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticDiagnostic<'db>> {
        let (Some(left_kind), Some(right_kind)) = (left.access, right.access) else {
            return Ok(());
        };
        // An access excludes another access only through a native loan.
        // Nominal effect providers can alias each other, but their accesses
        // must still respect every native loan passed or held across the call.
        if (left_kind == BorrowKind::Ref && right_kind == BorrowKind::Ref)
            || (left.payload.loan().is_none() && right.payload.loan().is_none())
        {
            return Ok(());
        }
        let (left, right) = independent(self.db, left, right);
        let Some(mut guard) = left.guard.and(&right.guard) else {
            return Ok(());
        };
        if same_group && left.path.len() == right.path.len() {
            let mut same = Some(guard.clone());
            for (left, right) in left.path.iter().zip(&right.path) {
                same = match (left, right) {
                    (
                        OccurrenceStep::Data(Projection::Index(left)),
                        OccurrenceStep::Data(Projection::Index(right)),
                    ) => same.and_then(|guard| guard.with_equality(*left, *right)),
                    (left, right) if left == right => same,
                    _ => None,
                };
            }
            if let Some(same) = same {
                let Some(different) = guard.difference(&same) else {
                    return Ok(());
                };
                guard = different;
            }
        }
        if !matches!(
            left.region
                .with_guard(&guard)
                .overlap(self.db, &right.region.with_guard(&guard)),
            OverlapResult::Disjoint
        ) {
            return Err(self.diag(
                SemanticDiagnosticKind::BorrowConflict,
                origin,
                "call arguments require conflicting access to mutable borrows".into(),
            ));
        }
        Ok(())
    }

    /// Compare one access with every active loan. Other loans are checked
    /// conservatively. An input loan's unresolved remainder, without the entry
    /// assumption that distinct inputs are separate, becomes a separation
    /// requirement on callers.
    fn compare_access(
        &self,
        active: &[CapabilityOccurrence<'db>],
        kind: BorrowKind,
        footprint: AccessFootprint<'_, 'db>,
        authority: &[Guarded<'db, LoanRef<'db>>],
        origin: SemOrigin<'db>,
        analysis: &mut ConflictFacts<'db>,
    ) {
        let region = footprint.region;
        for loan in active {
            if loan.semantics.target_ty.is_zero_sized(self.db) {
                continue;
            }
            let reference = loan.payload.loan().expect("active loan");
            let definition = &self.inventory.loans[reference.id.0];
            if kind == BorrowKind::Ref && definition.kind() == BorrowKind::Ref {
                continue;
            }
            let fresh = loan.guard.scope().freshening(region.scope());
            let loan = loan.substitute(self.db, &fresh);
            let lift = IndexSubst::new(region.scope(), fresh.destination(), [])
                .expect("access comparison scope");
            let footprint = AccessFootprint {
                region: &region.substitute(self.db, &lift),
                extent: footprint.extent.substitute(&lift),
            };
            // Authority matching is costly; compute it only for an overlap.
            let permitted = OnceCell::new();
            let permitted = || {
                permitted
                    .get_or_init(|| self.permitted(&loan, region, &lift, authority))
                    .as_ref()
            };
            // A reborrow of an input still names caller-supplied memory.
            // Forward its unresolved separation just as for the original loan;
            // this creates an obligation, never authority to discharge one.
            let input_backed = self.inventory.input_loans.contains(&reference.id)
                || self
                    .ancestors(loan.payload.authority(&loan.guard))
                    .iter()
                    .any(|parent| self.inventory.input_loans.contains(&parent.payload.id));
            if !input_backed {
                let (overlap, uncertain) =
                    footprint.intersect(self.db, AccessFootprint::typed(&loan.region));
                if !uncertain && (overlap.is_empty() || loan.suspended.provably_covers(&overlap)) {
                    continue;
                }
                // Exact occurrence authority remains valid when its target is opaque.
                // Unknown overlap alone never establishes that authority.
                if !overlap.is_empty()
                    && let Some(permitted) = permitted()
                    && overlap.clauses().iter().all(|clause| {
                        clause
                            .guard
                            .implies(&permitted.in_scope(clause.guard.scope()))
                    })
                {
                    continue;
                }
                return analysis.report(self.conflict_diag(kind, definition, origin));
            }
            if footprint.region.clauses().len() * loan.region.clauses().len()
                > SEPARATION_PAIR_LIMIT
            {
                return analysis.exhaust(self.separation_limit_diag(origin));
            }
            let mut clauses = Vec::new();
            for pair in footprint.physical_pairs(self.db, &loan.region) {
                let Some(guard) = (match permitted() {
                    Some(permitted) => pair
                        .possible
                        .difference(&permitted.in_scope(pair.possible.scope())),
                    None => Some(pair.possible.clone()),
                }) else {
                    continue;
                };
                let lift = IndexSubst::new(loan.suspended.scope(), guard.scope(), [])
                    .expect("suspension pair scope");
                let suspended = loan.suspended.substitute(self.db, &lift);
                let Some(unsuspended) = remove_suspended(&suspended, &pair, guard) else {
                    continue;
                };
                let guard = match unsuspended {
                    Unsuspended::Possible(guard) => guard,
                    Unsuspended::Definite => {
                        return analysis.report(self.conflict_diag(kind, definition, origin));
                    }
                };
                if !representable(&pair.access) || !representable(&pair.borrowed) {
                    // A caller cannot name this endpoint.
                    return analysis.report(self.conflict_diag(kind, definition, origin));
                }
                let suspended = Separation::suspension_slices(&pair.borrowed, &suspended, &guard);
                clauses.push(Guarded {
                    guard,
                    payload: Separation {
                        protected: pair.borrowed,
                        protected_kind: definition.kind(),
                        access: pair.access,
                        access_kind: kind,
                        extent: pair.extent,
                        suspended,
                    },
                });
            }
            let origin = SeparationOrigin {
                owner: self.instance.key(self.db).owner(self.db),
                template_owner: self.body.template_owner,
                borrow: definition.origin(),
                access: origin,
            };
            analysis.deferred.extend(
                SeparationSet::new(self.db, region.scope(), clauses)
                    .quantify_into(self.db, &BinderScope::default())
                    .clauses()
                    .iter()
                    .map(|clause| (clause.clone(), origin)),
            );
        }
    }

    /// Settle a callee's separation clause at this call. Only physical
    /// separation or the callee's frozen suspension discharges it; authority
    /// found while resolving an endpoint never does. What callers can still
    /// refine is forwarded, and the rest is a conflict here.
    fn discharge(
        &self,
        requirement: &ResolvedSeparation<'db>,
        origin: SemOrigin<'db>,
        analysis: &mut ConflictFacts<'db>,
    ) {
        let scope = requirement.guard.scope();
        if requirement.access.clauses().len() * requirement.protected.clauses().len()
            > SEPARATION_PAIR_LIMIT
        {
            return analysis.exhaust(self.separation_limit_diag(origin));
        }
        let footprint = AccessFootprint {
            region: &requirement.access.with_guard(&requirement.guard),
            extent: requirement.extent,
        };
        let mut clauses = Vec::new();
        for pair in footprint.physical_pairs(
            self.db,
            &requirement.protected.with_guard(&requirement.guard),
        ) {
            let lift =
                IndexSubst::new(scope, pair.possible.scope(), []).expect("separation pair scope");
            let slices: Vec<_> = requirement
                .suspended
                .iter()
                .map(|slice| Guarded {
                    guard: slice
                        .guard
                        .substitute(&lift)
                        .expect("scope extension preserves feasibility"),
                    payload: slice.payload.clone(),
                })
                .collect();
            // The callee's certified slices of whichever place it borrowed.
            let suspended = RegionSet::new(
                pair.possible.scope(),
                slices.iter().map(|slice| Guarded {
                    guard: slice.guard.clone(),
                    payload: SymbolicPlace {
                        root: pair.borrowed.root.clone(),
                        path: pair.borrowed.path.concat(&slice.payload),
                        views: pair.borrowed.views.clone(),
                    },
                }),
            );
            let guard = match remove_suspended(&suspended, &pair, pair.possible.clone()) {
                None => continue,
                Some(Unsuspended::Definite) => {
                    return analysis.report(self.separation_diag(
                        "this call accesses memory that the callee keeps borrowed across the access",
                        origin,
                        requirement.origin,
                    ));
                }
                Some(Unsuspended::Possible(guard)) => guard,
            };
            if !representable(&pair.access)
                || !representable(&pair.borrowed)
                || !(self.refinable_place(&pair.access)
                    || self.refinable_place(&pair.borrowed)
                    || pair
                        .extent
                        .indices()
                        .any(|index| self.refinable_index(index))
                    || self.refinable_guard(&guard)
                    || slices.iter().any(|slice| {
                        slice
                            .payload
                            .indices()
                            .any(|index| self.refinable_index(index))
                            || self.refinable_guard(&slice.guard)
                    }))
            {
                return analysis.report(self.separation_diag(
                    "cannot prove that this call keeps memory the callee holds borrowed separate from the callee's access",
                    origin,
                    requirement.origin,
                ));
            }
            clauses.push(Guarded {
                guard,
                payload: Separation {
                    protected: pair.borrowed,
                    protected_kind: requirement.protected_kind,
                    access: pair.access,
                    access_kind: requirement.access_kind,
                    extent: pair.extent,
                    suspended: slices.into(),
                },
            });
        }
        analysis.deferred.extend(
            SeparationSet::new(self.db, scope, clauses)
                .quantify_into(self.db, &BinderScope::default())
                .clauses()
                .iter()
                .map(|clause| (clause.clone(), requirement.origin)),
        );
    }

    pub(super) fn separation_limit_diag(&self, origin: SemOrigin<'db>) -> SemanticDiagnostic<'db> {
        self.diag(
            SemanticDiagnosticKind::BorrowConflict,
            origin,
            "borrow separation requirements exceed the analysis limits".into(),
        )
    }

    /// A conflict at a call, with the borrow and access the requirement began
    /// with, wherever it was first deferred.
    fn separation_diag(
        &self,
        message: &str,
        origin: SemOrigin<'db>,
        separation: SeparationOrigin<'db>,
    ) -> SemanticDiagnostic<'db> {
        let mut diagnostic = self.diag(
            SemanticDiagnosticKind::BorrowConflict,
            origin,
            message.into(),
        );
        for (message, origin) in [
            ("borrow held across the access", separation.borrow),
            ("access that must stay separate from it", separation.access),
        ] {
            diagnostic.push_secondary(
                message.into(),
                SemanticDiagnosticSpan::OriginWithTemplateFallback {
                    owner: separation.owner,
                    template_owner: separation.template_owner,
                    origin,
                },
            );
        }
        diagnostic
    }

    /// Whether a caller supplies part of this place's identity.
    fn refinable_place(&self, place: &SymbolicPlace<'db>) -> bool {
        place
            .path
            .indices()
            .any(|index| self.refinable_index(index))
            || matches!(&place.root, RegionRoot::External(source) if self.refinable_source(source))
    }

    fn refinable_source(&self, source: &ExternalSource<'db>) -> bool {
        source.indices().any(|index| self.refinable_index(index))
            || match &source.origin {
                ExternalOrigin::Input(_) => true,
                ExternalOrigin::Provider { provider, .. } => matches!(
                    provider.binding(self.db).source,
                    ProviderSource::UsesParam { .. }
                ),
                ExternalOrigin::Memory { base, .. } => self.refinable_source(&base.source),
                _ => false,
            }
            || source.clobber.as_ref().is_some_and(|clobber| {
                self.refinable_source(&clobber.target.source)
                    || self.refinable_source(&clobber.written.source)
            })
    }

    /// A scalar that becomes a formal parameter in the summary. Type-level
    /// constants are fixed by this instance and never refined by a caller.
    fn refinable_index(&self, index: IndexExpr<'db>) -> bool {
        match index {
            IndexExpr::Runtime(value) => matches!(self.index(value),
                IndexExpr::Runtime(actual)
                    if matches!(self.body.values[actual.index()].definition,
                        NValueDefinition::EntryParam { .. })),
            IndexExpr::FormalValue(_) => true,
            IndexExpr::Const(_)
            | IndexExpr::TypeConst(_)
            | IndexExpr::Bound(_)
            | IndexExpr::Iteration(_) => false,
        }
    }

    fn refinable_guard(&self, guard: &Guard<'db>) -> bool {
        guard
            .indices()
            .into_iter()
            .any(|index| self.refinable_index(index))
            || guard
                .occurrences()
                .into_iter()
                .any(|occurrence| match occurrence {
                    ValueOccurrence::Value(value) => matches!(
                        self.body.values[value.index()].definition,
                        NValueDefinition::EntryParam { .. }
                    ),
                    ValueOccurrence::Argument(_) => true,
                    _ => false,
                })
    }

    /// Where the access's authority descends from exactly this loan occurrence,
    /// in the occurrence's freshened scope.
    fn permitted(
        &self,
        loan: &CapabilityOccurrence<'db>,
        region: &RegionSet<'db>,
        lift: &IndexSubst<'db>,
        authority: &[Guarded<'db, LoanRef<'db>>],
    ) -> Option<Guard<'db>> {
        let mut permitted = None;
        for parent in self.ancestors(authority.iter().cloned()) {
            // Access offsets can introduce witnesses unused by the authority.
            // Drop only those unused binders before comparing exact loan occurrences.
            let canonical = parent
                .guard
                .scope()
                .canonical_existentials(region.scope(), || {
                    parent
                        .guard
                        .indices()
                        .into_iter()
                        .chain(parent.payload.args.iter().copied())
                });
            let parent = Guarded {
                guard: parent
                    .guard
                    .substitute(&canonical)
                    .expect("authority normalization"),
                payload: parent.payload.substitute(&canonical),
            };
            let subst = lift.under_existentials(parent.guard.scope());
            let parent = Guarded {
                guard: parent.guard.substitute(&subst).expect("authority scope"),
                payload: parent.payload.substitute(&subst),
            };
            if parent.guard.scope() != lift.destination() {
                continue;
            }
            if let Some(guard) = loan
                .payload
                .loan()
                .expect("loan occurrence")
                .matching_guard(&parent.payload, parent.guard)
            {
                permitted =
                    Some(permitted.map_or_else(|| guard.clone(), |old: Guard<'db>| old.or(&guard)));
            }
        }
        permitted
    }

    fn conflict_diag(
        &self,
        kind: BorrowKind,
        definition: &LoanDef<'db>,
        origin: SemOrigin<'db>,
    ) -> SemanticDiagnostic<'db> {
        let mut diagnostic = self.diag(
            SemanticDiagnosticKind::BorrowConflict,
            origin,
            match (kind, definition.kind()) {
                (BorrowKind::Mut, BorrowKind::Mut) => {
                    "cannot mutably borrow this place while a mut borrow is active"
                }
                (BorrowKind::Mut, BorrowKind::Ref) => {
                    "cannot mutably borrow this place while an immutable borrow is active"
                }
                (BorrowKind::Ref, BorrowKind::Mut) => {
                    "cannot immutably borrow this place while a mutable borrow is active"
                }
                (BorrowKind::Ref, BorrowKind::Ref) => unreachable!(),
            }
            .into(),
        );
        diagnostic.push_secondary(
            "borrow created here".into(),
            SemanticDiagnosticSpan::OriginWithTemplateFallback {
                owner: self.instance.key(self.db).owner(self.db),
                template_owner: self.body.template_owner,
                origin: definition.origin(),
            },
        );
        diagnostic
    }
}

/// Local conflicts and the separation left to callers, from one read-only
/// pass over the settled operations.
#[derive(Clone)]
pub(super) struct ConflictAnalysis<'db> {
    /// The first conflict in operation order, where the analysis stopped.
    pub diagnostic: Option<SemanticDiagnostic<'db>>,
    /// The analysis exceeded a limit, so no summary may use `deferred`.
    pub exhausted: bool,
    /// Unresolved separation, in the default owner scope and body order. It
    /// is complete only without a diagnostic, which prevents validation.
    pub deferred: Vec<(Guarded<'db, Separation<'db>>, SeparationOrigin<'db>)>,
    /// Native validity of the accesses callees' relations name, deferred to
    /// callers on the same physical basis, complete under the same condition.
    pub validity: NativeValidity<'db>,
}

#[derive(Default)]
pub(super) struct ConflictFacts<'db> {
    diagnostic: Option<SemanticDiagnostic<'db>>,
    exhausted: bool,
    deferred: Vec<(Guarded<'db, Separation<'db>>, SeparationOrigin<'db>)>,
    validity: NativeValidity<'db>,
}

impl<'db> ConflictFacts<'db> {
    fn report(&mut self, diagnostic: SemanticDiagnostic<'db>) {
        self.diagnostic.get_or_insert(diagnostic);
    }

    fn exhaust(&mut self, diagnostic: SemanticDiagnostic<'db>) {
        self.exhausted = true;
        self.report(diagnostic);
    }
}

/// What remains of a pair's overlap outside a certified suspension.
enum Unsuspended<'db> {
    /// An uncovered certain overlap.
    Definite,
    /// Only a possible overlap, on this guard.
    Possible(Guard<'db>),
}

/// Remove the parts of `guard` whose overlap lies in `suspended`, which is in
/// the pair's scope. Suspension of the whole borrowed place covers any access;
/// otherwise only a typed access's exact intersection can be covered.
fn remove_suspended<'db>(
    suspended: &RegionSet<'db>,
    pair: &FootprintPair<'db>,
    guard: Guard<'db>,
) -> Option<Unsuspended<'db>> {
    let guard = match suspended.covering_guard(&pair.borrowed, &guard) {
        Some(whole) => guard.difference(&whole)?,
        None => guard,
    };
    let Some(definite) = pair
        .definite
        .as_ref()
        .and_then(|definite| definite.and(&guard))
    else {
        return Some(Unsuspended::Possible(guard));
    };
    let intersection = if pair.extent == AccessExtent::Typed
        && pair.access.path.as_slice().len() > pair.borrowed.path.as_slice().len()
    {
        &pair.access
    } else {
        &pair.borrowed
    };
    let covered = suspended.covering_guard(intersection, &definite);
    if covered.is_none_or(|covered| definite.difference(&covered).is_some()) {
        return Some(Unsuspended::Definite);
    }
    guard.difference(&definite).map(Unsuspended::Possible)
}

/// Whether a caller can name this place: an external route without local
/// storage anywhere in it.
fn representable(place: &SymbolicPlace<'_>) -> bool {
    SourceExpr::from_place(place).is_some_and(|source| !source.source.names_local_storage())
}

fn independent<'db>(
    db: &'db dyn HirAnalysisDb,
    left: &CapabilityOccurrence<'db>,
    right: &CapabilityOccurrence<'db>,
) -> (CapabilityOccurrence<'db>, CapabilityOccurrence<'db>) {
    let left_subst = left.guard.scope().freshening(&BinderScope::default());
    let right_subst = right.guard.scope().freshening(left_subst.destination());
    let left_subst = left_subst
        .then(
            &IndexSubst::new(left_subst.destination(), right_subst.destination(), [])
                .expect("combined occurrence scope"),
        )
        .expect("independent occurrences");
    (
        left.substitute(db, &left_subst),
        right.substitute(db, &right_subst),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::semantic::{
            FieldIndex,
            capability::{source::InputSource, test_roots},
        },
        test_db::HirAnalysisTestDb,
    };

    #[test]
    fn suspension_removes_only_overlap_it_certainly_contains() {
        let db = HirAnalysisTestDb::default();
        let scope = BinderScope::default();
        let root = |param| RegionRoot::External(test_roots::input(&db, InputSource::place(param)));
        let field = RegionPath::new([Projection::Field(FieldIndex(0))]);
        let borrowed = RegionSet::singleton(&scope, root(0), RegionPath::default());
        fn suspension<'db>(
            pair: &FootprintPair<'db>,
            path: &RegionPath<IndexExpr<'db>>,
        ) -> RegionSet<'db> {
            RegionSet::new(
                pair.possible.scope(),
                [Guarded {
                    guard: Guard::always(pair.possible.scope()),
                    payload: SymbolicPlace {
                        path: pair.borrowed.path.concat(path),
                        ..pair.borrowed.clone()
                    },
                }],
            )
        }
        // An access of unknown extent through another input may overlap only.
        let other = RegionSet::singleton(&scope, root(1), RegionPath::default());
        let [pair] = <[_; 1]>::try_from(
            AccessFootprint {
                region: &other,
                extent: AccessExtent::Unknown,
            }
            .physical_pairs(&db, &borrowed),
        )
        .ok()
        .unwrap();
        assert!(pair.definite.is_none());
        let whole = suspension(&pair, &RegionPath::default());
        assert!(remove_suspended(&whole, &pair, pair.possible.clone()).is_none());
        let partial = suspension(&pair, &field);
        assert!(matches!(
            remove_suspended(&partial, &pair, pair.possible.clone()),
            Some(Unsuspended::Possible(_))
        ));
        // A byte span starting in a suspended field may reach past it.
        let start = RegionSet::singleton(&scope, root(0), field.clone());
        let [span] = <[_; 1]>::try_from(
            AccessFootprint {
                region: &start,
                extent: AccessExtent::Bytes(IndexExpr::Const(64)),
            }
            .physical_pairs(&db, &borrowed),
        )
        .ok()
        .unwrap();
        let partial = suspension(&span, &field);
        assert!(remove_suspended(&partial, &span, span.possible.clone()).is_some());
        // A typed access within the suspended field is covered exactly.
        let [typed] =
            <[_; 1]>::try_from(AccessFootprint::typed(&start).physical_pairs(&db, &borrowed))
                .ok()
                .unwrap();
        let partial = suspension(&typed, &field);
        assert!(remove_suspended(&partial, &typed, typed.possible.clone()).is_none());
    }
}
