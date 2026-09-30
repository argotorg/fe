//! Access checking over the converged structural state and shared regions.
use super::validity::NativeValidity;
use crate::analysis::semantic::diagnostics::{
    BlockedSemanticBody, SemanticDiagnostic, SemanticDiagnosticId, SemanticDiagnosticKind,
    SemanticDiagnosticSpan,
};
use std::collections::{BTreeMap, BTreeSet};

use cranelift_entity::EntityRef;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        BorrowActivation, SemOrigin,
        capability::{
            external::{ExternalSource, ReferentContract},
            footprint::AccessFootprint,
            guard::{Guard, ValueOccurrence},
            handle::HandleAddressSpace,
            index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
            loan::{CapabilityRef, LoanRef},
            path::{Projection, RegionPath},
            region::{OverlapResult, RegionRoot, RegionSet},
            semantics::{CapabilityClass, CapabilitySemantics},
            state::{BorrowState, CapabilityValue},
            value::{Guarded, IndexPayload},
        },
        normalized::{
            NEffectArgValue, NExpr, NPlace, NPlaceBase, NRootId, NStatementKind, NValueId,
            NormalizedBody, access::AccessTarget,
        },
    },
    ty::{
        corelib::MemoryAccessKind,
        ty_def::{BorrowKind, TyId},
    },
};

use super::{
    access::effect_occurrence,
    ir::{LocalBorrowCheck, SemanticBorrowCheckResult},
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
        self.check_conflicts()?;
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

    pub(super) fn check_conflicts(&self) -> Result<(), SemanticDiagnostic<'db>> {
        for (block, operations) in self.operations.iter().enumerate() {
            for (index, operation) in operations.iter().enumerate() {
                if self.validation_dependencies[block][index] {
                    continue;
                }
                let origin = self.body.blocks[block].statements[index].origin;
                for access in &operation.accesses {
                    if access.invalidated.invalid {
                        return Err(self.invalidated_diag(access.origin));
                    }
                    self.check_access(
                        &operation.active,
                        access.conflict_kind,
                        AccessFootprint::typed(&access.region),
                        &access.authority,
                        access.origin,
                    )?;
                }
                for resolved in &operation.calls {
                    if self.call_validation_pending(block, index) {
                        break;
                    }
                    if resolved.invalidated.invalid {
                        return Err(self.invalidated_diag(origin));
                    }
                    self.check_access(
                        &operation.active,
                        resolved.access.kind.borrow_kind(),
                        resolved.access.footprint(),
                        &resolved.authority,
                        origin,
                    )?;
                }
                for (position, (group, member)) in operation.arguments.iter().enumerate() {
                    for (other_group, other) in operation.arguments.iter().skip(position) {
                        self.check_call_pair(*group == *other_group, member, other, origin)?;
                    }
                    if let Some(kind) = member.access {
                        let authority = self.ancestors(member.payload.authority(&member.guard));
                        self.check_access(
                            &operation.active,
                            kind,
                            AccessFootprint::typed(&member.region),
                            &authority,
                            origin,
                        )?;
                    }
                }
            }
        }
        Ok(())
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

    fn check_access(
        &self,
        active: &[CapabilityOccurrence<'db>],
        kind: BorrowKind,
        footprint: AccessFootprint<'_, 'db>,
        authority: &[Guarded<'db, LoanRef<'db>>],
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticDiagnostic<'db>> {
        let region = footprint.region;
        for loan in active {
            if loan.semantics.target_ty.is_zero_sized(self.db) {
                continue;
            }
            let reference = loan.payload.loan().expect("active loan");
            let definition = &self.inventory.loans[reference.id.0];
            let active_kind = definition.kind();
            if kind == BorrowKind::Ref && active_kind == BorrowKind::Ref {
                continue;
            }
            let fresh = loan.guard.scope().freshening(region.scope());
            let loan = loan.substitute(self.db, &fresh);
            let lift = IndexSubst::new(region.scope(), fresh.destination(), [])
                .expect("access comparison scope");
            let accessed = region.substitute(self.db, &lift);
            // Input exclusivity is a precondition checked at each call. Keep
            // exact alias checks here; unresolved accesses remain in the summary
            // so a caller cannot use this assumption to hide an actual conflict.
            let (overlap, uncertain) = if self.inventory.input_loans.contains(&reference.id) {
                (accessed.proven_intersection(&loan.region), false)
            } else {
                AccessFootprint {
                    region: &accessed,
                    extent: footprint.extent.substitute(&lift),
                }
                .intersect(self.db, AccessFootprint::typed(&loan.region))
            };
            if !uncertain && (overlap.is_empty() || loan.suspended.provably_covers(&overlap)) {
                continue;
            }
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
                if parent.guard.scope() != overlap.scope() {
                    continue;
                }
                if let Some(guard) = loan
                    .payload
                    .loan()
                    .expect("loan occurrence")
                    .matching_guard(&parent.payload, parent.guard)
                {
                    permitted = Some(
                        permitted.map_or_else(|| guard.clone(), |old: Guard<'db>| old.or(&guard)),
                    );
                }
            }
            // Exact occurrence authority remains valid when its target is opaque.
            // Unknown overlap alone never establishes that authority.
            if !overlap.is_empty()
                && let Some(permitted) = permitted
                && overlap.clauses().iter().all(|clause| {
                    clause
                        .guard
                        .implies(&permitted.in_scope(clause.guard.scope()))
                })
            {
                continue;
            }
            let mut diagnostic = self.diag(
                SemanticDiagnosticKind::BorrowConflict,
                origin,
                match (kind, active_kind) {
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
            return Err(diagnostic);
        }
        Ok(())
    }
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
