//! Access checking over the converged structural state and shared regions.
use std::collections::{BTreeMap, BTreeSet};

use cranelift_entity::EntityRef;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        BorrowActivation, SemOrigin,
        capability::{
            external::{ExternalSource, ReferentContract},
            guard::{Guard, ValueOccurrence},
            index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
            loan::{CapabilityRef, LoanRef},
            path::{Projection, RegionPath},
            region::{OverlapResult, RegionRoot, RegionSet},
            semantics::{CapabilityClass, CapabilitySemantics},
            state::{BorrowState, CapabilityValue},
            value::{Guarded, IndexPayload},
        },
        normalized::{
            NBlockId, NDataProjection, NEffectArgValue, NExpr, NIndex, NOperand, NPlace,
            NPlaceBase, NRootId, NStatementKind, NTerminatorKind, NValueId, ReadMode,
        },
    },
    ty::ty_def::{BorrowKind, TyId},
};

use super::{
    diagnostics::operand_origin,
    ir::{
        BlockedSemanticBody, SemanticBorrowDiagKind, SemanticBorrowDiagnostic,
        SemanticBorrowDiagnosticSpan,
    },
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
    fn place(&mut self, place: &NPlace<'_>) {
        match place.base {
            NPlaceBase::Root(root) => {
                self.roots.insert(root);
            }
            NPlaceBase::CapabilityTarget { carrier } => {
                self.values.insert(carrier);
            }
        }
        self.path(&place.path);
    }
    fn path(&mut self, path: &crate::analysis::semantic::normalized::NDataPath) {
        self.values
            .extend(path.iter().filter_map(|step| match step {
                NDataProjection::Index(NIndex::Value(value)) => Some(*value),
                _ => None,
            }));
    }
    fn statement(&mut self, statement: &NStatementKind<'_>) {
        match statement {
            NStatementKind::Define { result, expr } => {
                self.values.remove(result);
                expr.for_each_value_operand(|operand| {
                    self.values.insert(operand.value);
                });
                expr.for_each_place_operand(|place| self.place(place));
                if let NExpr::ProjectValue { path, .. } = expr {
                    self.path(&path.0);
                }
            }
            NStatementKind::Store { destination, value } => {
                if let NPlaceBase::Root(root) = destination.base
                    && destination.path.is_empty()
                {
                    self.roots.remove(&root);
                    self.path(&destination.path);
                } else {
                    self.place(destination);
                }
                self.values.insert(value.value);
            }
        }
    }
}

impl<'db> Borrowck<'db> {
    pub fn check(
        mut self,
    ) -> Result<Option<BlockedSemanticBody<'db>>, SemanticBorrowDiagnostic<'db>> {
        self.solve()?;
        if self.blocked.is_some() {
            return Ok(self.blocked);
        }
        self.check_moves()?;
        self.check_conflicts()?;
        Ok(None)
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
                if self.terminal[index].is_some() {
                    match block.terminator.kind {
                        NTerminatorKind::Branch { cond, .. }
                        | NTerminatorKind::MatchEnum { value: cond, .. }
                        | NTerminatorKind::Return(Some(cond)) => {
                            live.values.insert(cond.value);
                        }
                        NTerminatorKind::Goto(_)
                        | NTerminatorKind::Assert { .. }
                        | NTerminatorKind::Return(None) => {}
                    }
                }
                for (position, statement) in block
                    .statements
                    .iter()
                    .take(self.before[index].len())
                    .enumerate()
                    .rev()
                {
                    live.statement(&statement.kind);
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
    ) -> Result<Vec<CapabilityOccurrence<'db>>, SemanticBorrowDiagnostic<'db>> {
        let mut result = Vec::new();
        let mut pending = vec![(value.clone(), Vec::new(), Vec::new(), traversal)];
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
                let region = leaf
                    .payload
                    .region(self.db, &self.inventory.loans, leaf.guard.scope())
                    .with_guard(&leaf.guard);
                let access = match leaf.semantics.class {
                    CapabilityClass::Borrow(kind) => Some(kind),
                    CapabilityClass::View => Some(BorrowKind::Ref),
                    CapabilityClass::Handle => match traversal {
                        CapabilityTraversal::Effect(kind) if leaf.path.as_slice().is_empty() => {
                            Some(kind)
                        }
                        _ => None,
                    },
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
                if access.is_none() && !matches!(traversal, CapabilityTraversal::Reachable) {
                    continue;
                }
                let target = self.shape(leaf.semantics.target_ty)?;
                if !target.contains_capability(self.db)
                    || region.is_empty()
                    || ancestry.contains(&(region.clone(), target))
                {
                    continue;
                }
                let mut ancestry = ancestry.clone();
                ancestry.push((region.clone(), target));
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
        target_ty: TyId<'db>,
        path: &RegionPath<IndexExpr<'db>>,
        scope: &BinderScope,
        inputs: CallInputs<'_, 'db>,
    ) -> Result<Resolution<'db>, SemanticBorrowDiagnostic<'db>> {
        let CallInputs {
            args,
            effects,
            origin,
        } = inputs;
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
                    self.read_region(
                        state,
                        &region,
                        shape,
                        ValueOccurrence::Argument(param),
                        origin,
                    )?
                }
            }
        };
        let mut resolved = Resolution::empty(scope);
        for capability in self.capabilities(
            state,
            &value,
            ValueOccurrence::Argument(param),
            origin,
            CapabilityTraversal::Reachable,
        )? {
            let subst = capability.guard.scope().freshening(scope);
            let mut capability = capability.substitute(self.db, &subst);
            if capability.semantics.target_ty != target_ty {
                let clauses = capability.region.clauses().iter().filter_map(|clause| {
                    let original = clause.payload.root.contract()?;
                    if !original.is_abstract(self.db) {
                        return None;
                    }
                    let mut clause = clause.clone();
                    clause.payload.root = RegionRoot::External(ExternalSource::abstract_target(
                        &clause.payload.root,
                        ReferentContract::new(self.db, target_ty, original.address_space),
                    ));
                    clause.payload.path = RegionPath::default();
                    clause.payload.views = Default::default();
                    Some(clause)
                });
                capability.region = RegionSet::new(capability.region.scope(), clauses);
            }
            resolved.region = resolved
                .region
                .union(&capability.region.project(path).close_existentials(scope));
            if let Some(reference) = capability.payload.loan() {
                resolved.parents.push(Guarded {
                    guard: capability.guard,
                    payload: reference.clone(),
                });
            }
        }
        Ok(resolved)
    }

    pub fn reachable_region(
        &mut self,
        state: &BorrowState<'db>,
        region: &RegionSet<'db>,
        ty: TyId<'db>,
        target_ty: TyId<'db>,
        scope: &BinderScope,
        origin: SemOrigin<'db>,
    ) -> Result<Resolution<'db>, SemanticBorrowDiagnostic<'db>> {
        let shape = self.shape(ty)?;
        let contents = self.read_region(state, region, shape, ValueOccurrence::Summary, origin)?;
        let mut resolved = Resolution::empty(scope);
        if ty == target_ty {
            resolved.region = region.clone();
        }
        for capability in self.capabilities(
            state,
            &contents,
            ValueOccurrence::Summary,
            origin,
            CapabilityTraversal::Reachable,
        )? {
            let subst = capability.guard.scope().freshening(scope);
            let mut capability = capability.substitute(self.db, &subst);
            if capability.semantics.target_ty != target_ty {
                let clauses = capability.region.clauses().iter().filter_map(|clause| {
                    let original = clause.payload.root.contract()?;
                    if !original.is_abstract(self.db) {
                        return None;
                    }
                    let mut clause = clause.clone();
                    clause.payload.root = RegionRoot::External(ExternalSource::abstract_target(
                        &clause.payload.root,
                        ReferentContract::new(self.db, target_ty, original.address_space),
                    ));
                    clause.payload.path = RegionPath::default();
                    clause.payload.views = Default::default();
                    Some(clause)
                });
                capability.region = RegionSet::new(capability.region.scope(), clauses);
            }
            resolved.region = resolved
                .region
                .union(&capability.region.close_existentials(scope));
            if let Some(reference) = capability.payload.loan() {
                resolved.parents.push(Guarded {
                    guard: capability.guard,
                    payload: reference.clone(),
                });
            }
        }
        Ok(resolved)
    }

    fn ancestors(
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

    fn authority(
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

    fn check_conflicts(&mut self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
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
                match &statement.kind {
                    NStatementKind::Define {
                        expr:
                            NExpr::Borrow {
                                place,
                                kind,
                                activation,
                                ..
                            },
                        ..
                    } => {
                        let kind = if matches!(activation, BorrowActivation::AtCall { .. }) {
                            BorrowKind::Ref
                        } else {
                            *kind
                        };
                        self.check_access(
                            &active,
                            kind,
                            &self.resolve_region(&state, place),
                            &self.authority(&state, place),
                            statement.origin,
                        )?;
                    }
                    NStatementKind::Define {
                        expr: NExpr::Load { place, .. } | NExpr::MakeView { place, .. },
                        ..
                    } => {
                        self.check_access(
                            &active,
                            BorrowKind::Ref,
                            &self.resolve_region(&state, place),
                            &self.authority(&state, place),
                            statement.origin,
                        )?;
                    }
                    NStatementKind::Store { destination, value } => {
                        let mut authority = self.authority(&state, destination);
                        if value.mode == ReadMode::Move {
                            authority.extend(
                                self.capabilities(
                                    &state,
                                    state.value(value.value),
                                    ValueOccurrence::Value(value.value),
                                    statement.origin,
                                    CapabilityTraversal::Held,
                                )?
                                .into_iter()
                                .filter_map(|capability| {
                                    Some(Guarded {
                                        guard: capability.guard,
                                        payload: capability.payload.loan()?.clone(),
                                    })
                                }),
                            );
                        }
                        self.check_access(
                            &active,
                            BorrowKind::Mut,
                            &self.resolve_region(&state, destination),
                            &authority,
                            statement.origin,
                        )?;
                    }
                    NStatementKind::Define {
                        expr:
                            NExpr::Call {
                                args, effect_args, ..
                            },
                        ..
                    } => {
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
                                    self.check_access(
                                        &active,
                                        if effect.required_mut {
                                            BorrowKind::Mut
                                        } else {
                                            BorrowKind::Ref
                                        },
                                        &self.resolve_region(&state, place),
                                        &self.authority(&state, place),
                                        statement.origin,
                                    )?;
                                    let region = self.resolve_region(&state, place);
                                    let shape = self.shape(place.ty)?;
                                    self.read_region(
                                        &state,
                                        &region,
                                        shape,
                                        ValueOccurrence::Argument(effect.binding_idx),
                                        statement.origin,
                                    )?
                                }
                            };
                            groups.push((
                                args.len() + effect.binding_idx as usize,
                                self.capabilities(
                                    &state,
                                    &value,
                                    ValueOccurrence::Argument(effect.binding_idx),
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
                        for (position, (group, member)) in all.iter().enumerate() {
                            for (other_group, other) in all.iter().skip(position) {
                                self.check_call_pair(
                                    *group == *other_group,
                                    member,
                                    other,
                                    statement.origin,
                                )?;
                            }
                            if let Some(kind) = member.access {
                                let authority =
                                    self.ancestors(member.payload.authority(&member.guard));
                                self.check_access(
                                    &active,
                                    kind,
                                    &member.region,
                                    &authority,
                                    statement.origin,
                                )?;
                            }
                        }
                    }
                    NStatementKind::Define { .. } => {}
                }
            }
        }
        Ok(())
    }

    fn check_call_pair(
        &self,
        same_group: bool,
        left: &CapabilityOccurrence<'db>,
        right: &CapabilityOccurrence<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
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
                .overlap(&right.region.with_guard(&guard)),
            OverlapResult::Disjoint
        ) {
            return Err(self.diag(
                SemanticBorrowDiagKind::BorrowConflict,
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
        region: &RegionSet<'db>,
        authority: &[Guarded<'db, LoanRef<'db>>],
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        for loan in active {
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
            let (overlap, uncertain) = accessed.intersect(&loan.region);
            if !uncertain && (overlap.is_empty() || loan.suspended.provably_covers(&overlap)) {
                continue;
            }
            let mut permitted = None;
            for parent in self.ancestors(authority.iter().cloned()) {
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
                SemanticBorrowDiagKind::BorrowConflict,
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
                SemanticBorrowDiagnosticSpan::OriginWithTemplateFallback {
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

#[derive(Clone, Debug, PartialEq, Eq)]
struct MoveFact<'db> {
    origin: SemOrigin<'db>,
    region: RegionSet<'db>,
}
type MoveState<'db> = BTreeMap<(usize, usize, usize), MoveFact<'db>>;

impl<'db> Borrowck<'db> {
    fn check_moves(&self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let mut entries: Vec<Option<MoveState<'db>>> = vec![None; self.body.blocks.len()];
        entries[self.body.entry.index()] = Some(BTreeMap::new());
        loop {
            let mut changed = false;
            for (block_index, block) in self.body.blocks.iter().enumerate() {
                let Some(mut moved) = entries[block_index].clone() else {
                    continue;
                };
                for (index, statement) in block
                    .statements
                    .iter()
                    .take(self.before[block_index].len())
                    .enumerate()
                {
                    self.transfer_moves(
                        &mut moved,
                        &self.before[block_index][index],
                        block_index,
                        index,
                        &statement.kind,
                        statement.origin,
                    );
                }
                for successor in block
                    .terminator
                    .kind
                    .successors()
                    .into_iter()
                    .filter(|_| self.terminal[block_index].is_some())
                {
                    let Some(guard) = self.edge_guard(block, successor) else {
                        continue;
                    };
                    let mut edge = moved.clone();
                    for fact in edge.values_mut() {
                        fact.region = fact.region.with_guard(&guard);
                    }

                    for (parameter, argument) in self.body.blocks[successor.block.index()]
                        .params
                        .iter()
                        .zip(&successor.args)
                    {
                        let destination = RegionSet::singleton(
                            &BinderScope::default(),
                            RegionRoot::Value(*parameter),
                            RegionPath::default(),
                        );
                        for fact in edge.values_mut() {
                            fact.region = fact.region.remove_covered(&destination);
                        }
                        for (site, fact) in &moved {
                            let clauses = fact
                                .region
                                .clauses()
                                .iter()
                                .filter(|clause| {
                                    clause.payload.root == RegionRoot::Value(argument.value)
                                })
                                .map(|clause| {
                                    let mut clause = clause.clone();
                                    clause.payload.root = RegionRoot::Value(*parameter);
                                    clause
                                });
                            let region = RegionSet::new(fact.region.scope(), clauses);
                            if !region.is_empty() {
                                edge.entry(*site)
                                    .and_modify(|fact| fact.region = fact.region.union(&region))
                                    .or_insert(MoveFact {
                                        origin: fact.origin,
                                        region,
                                    });
                            }
                        }
                    }
                    if let Some(iteration) = self
                        .inventory
                        .loops
                        .feedback(NBlockId::new(block_index), successor.block)
                    {
                        let repeated = self.inventory.loops.repeated(iteration);
                        for fact in edge.values_mut() {
                            fact.region = fact.region.forget_iteration(self.db,
                                |index| matches!(index, IndexExpr::Iteration(region) if region == iteration) || matches!(index, IndexExpr::Runtime(value) if repeated.contains(&value)),
                                |occurrence| self.inventory.loops.repeats_occurrence(iteration, occurrence));
                        }
                    }
                    edge.retain(|_, fact| !fact.region.is_empty());
                    if let Some(previous) = &mut entries[successor.block.index()] {
                        for (site, fact) in edge {
                            if let Some(old) = previous.get_mut(&site) {
                                let region = old.region.union(&fact.region);
                                changed |= region != old.region;
                                old.region = region;
                            } else {
                                previous.insert(site, fact);
                                changed = true;
                            }
                        }
                    } else {
                        entries[successor.block.index()] = Some(edge);
                        changed = true;
                    }
                }
            }
            if !changed {
                break;
            }
        }
        for (block_index, block) in self.body.blocks.iter().enumerate() {
            let Some(mut moved) = entries[block_index].clone() else {
                continue;
            };
            for (index, statement) in block
                .statements
                .iter()
                .take(self.before[block_index].len())
                .enumerate()
            {
                let state = &self.before[block_index][index];
                match &statement.kind {
                    NStatementKind::Define {
                        expr: NExpr::Load { place, .. } | NExpr::Borrow { place, .. },
                        ..
                    } => {
                        let region = self.resolve_region(state, place);
                        self.check_moved(&moved, &region, statement.origin)?;
                        if matches!(
                            &statement.kind,
                            NStatementKind::Define {
                                expr: NExpr::Load {
                                    mode: ReadMode::Move,
                                    ..
                                },
                                ..
                            }
                        ) && matches!(place.base, NPlaceBase::CapabilityTarget { .. })
                        {
                            return Err(self.diag(
                                SemanticBorrowDiagKind::MoveConflict,
                                statement.origin,
                                "cannot move out of a view parameter or through a borrow handle"
                                    .into(),
                            ));
                        }
                    }
                    NStatementKind::Define {
                        expr: NExpr::ProjectValue { value, path },
                        ..
                    } => {
                        let region = RegionSet::singleton(
                            &BinderScope::default(),
                            RegionRoot::Value(value.value),
                            self.path(&path.0),
                        );
                        self.check_moved(&moved, &region, statement.origin)?;
                        if value.mode == ReadMode::Move
                            && self.body.values[value.value.index()]
                                .ty
                                .as_view(self.db)
                                .is_some()
                        {
                            return Err(self.diag(
                                SemanticBorrowDiagKind::MoveConflict,
                                statement.origin,
                                "cannot move out of a view parameter".into(),
                            ));
                        }
                    }
                    NStatementKind::Define { expr, .. } => {
                        expr.try_for_each_value_operand(|operand| {
                            self.check_moved_operand(&moved, operand, statement.origin)
                        })?;
                    }
                    NStatementKind::Store { destination, value } => {
                        self.check_moved_operand(&moved, *value, statement.origin)?;
                        let region = self.resolve_region(state, destination);
                        for fact in moved.values() {
                            if !matches!(region.overlap(&fact.region), OverlapResult::Disjoint)
                                && !region.provably_covers(&fact.region)
                            {
                                return Err(self.diag(
                                    SemanticBorrowDiagKind::MoveConflict,
                                    statement.origin,
                                    "cannot write through a moved value".into(),
                                ));
                            }
                        }
                    }
                }
                self.transfer_moves(
                    &mut moved,
                    state,
                    block_index,
                    index,
                    &statement.kind,
                    statement.origin,
                );
            }
            if self.terminal[block_index].is_none() {
                continue;
            }
            match block.terminator.kind {
                NTerminatorKind::Branch { cond, .. }
                | NTerminatorKind::MatchEnum { value: cond, .. }
                | NTerminatorKind::Return(Some(cond)) => {
                    self.check_moved_operand(&moved, cond, block.terminator.origin)?
                }
                NTerminatorKind::Goto(_)
                | NTerminatorKind::Assert { .. }
                | NTerminatorKind::Return(None) => {}
            }
            for successor in block.terminator.kind.successors() {
                for operand in &successor.args {
                    self.check_moved_operand(&moved, *operand, block.terminator.origin)?;
                }
            }
        }
        Ok(())
    }

    fn transfer_moves(
        &self,
        moved: &mut MoveState<'db>,
        state: &BorrowState<'db>,
        block: usize,
        statement: usize,
        kind: &NStatementKind<'db>,
        origin: SemOrigin<'db>,
    ) {
        let mut sources = Vec::new();
        match kind {
            NStatementKind::Define { result, expr } => {
                let destination = RegionSet::singleton(
                    &BinderScope::default(),
                    RegionRoot::Value(*result),
                    RegionPath::default(),
                );
                for fact in moved.values_mut() {
                    fact.region = fact.region.remove_covered(&destination);
                }
                match expr {
                    NExpr::Load {
                        place,
                        mode: ReadMode::Move,
                    } if place.ty.as_capability(self.db).is_none() => {
                        sources.push((self.resolve_region(state, place), origin))
                    }
                    NExpr::ProjectValue { value, path }
                        if value.mode == ReadMode::Move
                            && self.body.values[value.value.index()]
                                .ty
                                .as_capability(self.db)
                                .is_none() =>
                    {
                        sources.push((
                            RegionSet::singleton(
                                &BinderScope::default(),
                                RegionRoot::Value(value.value),
                                self.path(&path.0),
                            ),
                            operand_origin(*value, origin),
                        ));
                    }
                    NExpr::ProjectValue { .. } => {}
                    _ => expr.for_each_value_operand(|operand| {
                        if operand.mode == ReadMode::Move
                            && self.body.values[operand.value.index()]
                                .ty
                                .as_capability(self.db)
                                .is_none()
                        {
                            sources.push((
                                RegionSet::singleton(
                                    &BinderScope::default(),
                                    RegionRoot::Value(operand.value),
                                    RegionPath::default(),
                                ),
                                operand_origin(operand, origin),
                            ));
                        }
                    }),
                }
            }
            NStatementKind::Store { destination, value } => {
                let written = self.resolve_region(state, destination);
                for fact in moved.values_mut() {
                    fact.region = fact.region.remove_covered(&written);
                }
                if value.mode == ReadMode::Move
                    && self.body.values[value.value.index()]
                        .ty
                        .as_capability(self.db)
                        .is_none()
                {
                    sources.push((
                        RegionSet::singleton(
                            &BinderScope::default(),
                            RegionRoot::Value(value.value),
                            RegionPath::default(),
                        ),
                        operand_origin(*value, origin),
                    ));
                }
            }
        }
        moved.retain(|_, fact| !fact.region.is_empty());
        for (index, (region, origin)) in sources.into_iter().enumerate() {
            let region = region.with_guard(state.guard());
            moved
                .entry((block, statement, index))
                .and_modify(|fact| fact.region = fact.region.union(&region))
                .or_insert(MoveFact { origin, region });
        }
    }

    fn check_moved_operand(
        &self,
        moved: &MoveState<'db>,
        operand: NOperand,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        self.check_moved(
            moved,
            &RegionSet::singleton(
                &BinderScope::default(),
                RegionRoot::Value(operand.value),
                RegionPath::default(),
            ),
            operand_origin(operand, origin),
        )
    }

    fn check_moved(
        &self,
        moved: &MoveState<'db>,
        region: &RegionSet<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let Some(fact) = moved
            .values()
            .find(|fact| !matches!(fact.region.overlap(region), OverlapResult::Disjoint))
        {
            let mut diagnostic = self.diag(
                SemanticBorrowDiagKind::MoveConflict,
                origin,
                "cannot use a value after it was moved".into(),
            );
            diagnostic.push_secondary(
                "value is moved here".into(),
                SemanticBorrowDiagnosticSpan::OriginWithTemplateFallback {
                    owner: self.instance.key(self.db).owner(self.db),
                    template_owner: self.body.template_owner,
                    origin: fact.origin,
                },
            );
            return Err(diagnostic);
        }
        Ok(())
    }
}
