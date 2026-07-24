use std::mem;

use cranelift_entity::SecondaryMap;
use dataflow::JoinSemiLattice;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            SBlockId, SConst, SLocalId, SemOrigin, SemanticInstance, VariantIndex,
            borrowck::ir::{NExpr, NSStmtKind},
            consts::{SemConstValue, scalar_int},
        },
        ty::{
            provider::{ProviderAddressSpace, ProviderKind},
            ty_def::{BorrowKind, TyId},
        },
    },
    projection::{Aliasing, IndexSource, Projection},
};
use num_traits::ToPrimitive;

use super::{
    diagnostics::normalized_body_internal_diag,
    facts::NormalizedBodyFacts,
    ir::{
        FreshAllocSite, NBorrowRoot, NBorrowRootId, NSPlace, NSPlaceRoot, NSProjectionPath,
        NormalizedBindingLowering, NormalizedSemanticBody, PointerAddressSpaces,
        SemanticBorrowDiagnostic,
    },
    pointer::{
        mem_array_carrier_suffix, path_with_projection, projection_result_ty,
        raw_pointer_pointee_suffix,
    },
};

mod summary;
mod transfer;

pub(super) use summary::BorrowSummaryResolver;
pub(super) use transfer::BorrowStateTransferCx;

pub(super) fn address_spaces_for_borrow_root<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedSemanticBody<'db>,
    root: &BorrowRoot<'db>,
    origin: SemOrigin<'db>,
) -> Result<Vec<ProviderAddressSpace>, SemanticBorrowDiagnostic<'db>> {
    match root {
        BorrowRoot::Param(_)
        | BorrowRoot::Local(_)
        | BorrowRoot::FreshAllocation {
            address_space: ProviderAddressSpace::Memory,
            ..
        }
        | BorrowRoot::UnknownMemory(PointerAddressSpaces::One(ProviderAddressSpace::Memory)) => {
            Ok(vec![ProviderAddressSpace::Memory])
        }
        BorrowRoot::FreshAllocation { address_space, .. } => Ok(vec![*address_space]),
        BorrowRoot::UnknownMemory(spaces) => Ok(spaces.spaces()),
        BorrowRoot::Provider(binding) => match binding.semantics.address_space {
            Some(space) => Ok(vec![space]),
            None if matches!(binding.semantics.kind, ProviderKind::RootObject) => {
                Ok(vec![ProviderAddressSpace::Memory])
            }
            None => Err(normalized_body_internal_diag(
                db,
                instance,
                body,
                origin,
                format!(
                    "provider `{}` has no address space",
                    binding.provider_ty.pretty_print(db)
                ),
            )),
        },
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct LoanId(pub(super) u32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum BorrowRoot<'db> {
    Param(u32),
    Local(SLocalId),
    Provider(crate::semantic::ProviderBinding<'db>),
    FreshAllocation {
        site: FreshAllocSite<'db>,
        address_space: ProviderAddressSpace,
    },
    UnknownMemory(PointerAddressSpaces),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct CanonPlace<'db> {
    pub(super) root: BorrowRoot<'db>,
    pub(super) proj: NSProjectionPath<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct PointerSlotPlace<'db> {
    root: BorrowRoot<'db>,
    proj: NSProjectionPath<'db>,
}

impl<'db> PointerSlotPlace<'db> {
    fn new(root: BorrowRoot<'db>, proj: NSProjectionPath<'db>) -> Self {
        Self { root, proj }
    }

    fn is_unknown_memory_slot(&self) -> bool {
        matches!(self.root, BorrowRoot::UnknownMemory(_))
    }

    fn contains_wildcard_index(&self) -> bool {
        self.proj.iter().any(|projection| {
            matches!(
                projection,
                Projection::Index(IndexSource::Dynamic(_) | IndexSource::Any)
            )
        })
    }

    fn is_precise_slot(&self) -> bool {
        !self.is_unknown_memory_slot() && !self.contains_wildcard_index()
    }

    fn may_name_multiple_slots(&self) -> bool {
        !self.is_precise_slot()
    }
}

#[derive(Clone, Debug)]
pub(super) struct Loan<'db> {
    pub(super) kind: BorrowKind,
    pub(super) targets: FxHashSet<CanonPlace<'db>>,
    pub(super) parents: FxHashSet<LoanId>,
    pub(super) origin: SemOrigin<'db>,
}

#[derive(Clone, Debug)]
pub(super) struct MoveSite<'db> {
    pub(super) origin: SemOrigin<'db>,
    pub(super) note: String,
}

pub(super) type MovedPlaces<'db> = FxHashMap<CanonPlace<'db>, MoveSite<'db>>;
pub(super) type BlockAdjacency = SmallVec<SBlockId, 2>;
pub(super) type CfgAdjacency = SecondaryMap<SBlockId, BlockAdjacency>;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct PointerTargets<'db> {
    known: FxHashSet<CanonPlace<'db>>,
    unknown: Option<PointerAddressSpaces>,
}

impl<'db> PointerTargets<'db> {
    pub(super) fn known(targets: FxHashSet<CanonPlace<'db>>) -> Self {
        Self {
            known: targets,
            unknown: None,
        }
    }

    pub(super) fn unknown(address_spaces: PointerAddressSpaces) -> Self {
        Self {
            known: FxHashSet::default(),
            unknown: Some(address_spaces),
        }
    }

    pub(super) fn places(&self) -> FxHashSet<CanonPlace<'db>> {
        let mut places = self.known.clone();
        if let Some(spaces) = self.unknown {
            places.insert(CanonPlace {
                root: BorrowRoot::UnknownMemory(spaces),
                proj: NSProjectionPath::default(),
            });
        }
        places
    }

    fn join_into(&mut self, other: &Self) -> bool {
        let before = self.known.len();
        self.known.extend(other.known.iter().cloned());
        let mut changed = before != self.known.len();
        match (&mut self.unknown, other.unknown) {
            (Some(lhs), Some(rhs)) => changed |= lhs.join(rhs),
            (None, Some(rhs)) => {
                self.unknown = Some(rhs);
                changed = true;
            }
            (Some(_), None) | (None, None) => {}
        }
        changed
    }

    fn is_empty(&self) -> bool {
        self.known.is_empty() && self.unknown.is_none()
    }

    fn has_unknown(&self) -> bool {
        self.unknown.is_some()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum WritePrecision {
    Strong,
    Weak,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PointerStrongCoverage {
    None,
    Complete,
    Partial,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct PointerStrongLookup<'db> {
    targets: PointerTargets<'db>,
    coverage: PointerStrongCoverage,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct PointerTargetState<'db> {
    strong: FxHashMap<PointerSlotPlace<'db>, PointerTargets<'db>>,
    weak: FxHashMap<PointerSlotPlace<'db>, PointerTargets<'db>>,
}

impl<'db> PointerTargetState<'db> {
    // Lattice invariants:
    // - Strong facts are definite writes. They replace structural defaults only
    //   when lookup proves the fact fully covers the requested slot.
    // - Weak facts are may-writes. They are always joined with the strong or
    //   structural default result.
    // - IndexSource::Any is analysis-only wildcard state. Concrete strong facts
    //   override a covering Any default for exact reads; dynamic reads join both.
    // - UnknownMemory facts overlap roots whose address-space sets intersect.
    // - Nested pointer slots in fresh allocations default to unknown until a
    //   definite write initializes them.
    fn assign_strong(&mut self, key: PointerSlotPlace<'db>, targets: PointerTargets<'db>) {
        self.strong
            .retain(|stored_key, _| !pointer_slot_write_may_affect(&key, stored_key));
        self.weak
            .retain(|stored_key, _| !pointer_slot_write_may_affect(&key, stored_key));
        if targets.is_empty() {
            return;
        }
        self.strong.insert(key, targets);
    }

    fn assign_weak(&mut self, key: PointerSlotPlace<'db>, targets: PointerTargets<'db>) -> bool {
        if targets.is_empty() {
            return false;
        }
        match self.weak.entry(key) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                entry.get_mut().join_into(&targets)
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(targets);
                true
            }
        }
    }

    fn clear_strong(&mut self, key: &PointerSlotPlace<'db>) {
        self.strong.remove(key);
    }

    fn invalidate_strong(&mut self, key: PointerSlotPlace<'db>, targets: PointerTargets<'db>) {
        self.strong
            .retain(|stored_key, _| !pointer_slot_write_may_affect(&key, stored_key));
        self.weak
            .retain(|stored_key, _| !pointer_slot_write_may_affect(&key, stored_key));
        self.strong.insert(key, targets);
    }

    fn strong_lookup(&self, requested: &PointerSlotPlace<'db>) -> PointerStrongLookup<'db> {
        if requested.is_precise_slot()
            && let Some(targets) = self.strong.get(requested)
        {
            return PointerStrongLookup {
                targets: targets.clone(),
                coverage: PointerStrongCoverage::Complete,
            };
        }

        let mut targets = PointerTargets::default();
        let mut complete = false;
        let mut partial = false;
        for (stored_key, stored_targets) in &self.strong {
            if !pointer_slots_read_may_alias(stored_key, stored_targets, requested) {
                continue;
            }
            targets.join_into(stored_targets);
            if pointer_slot_covers(stored_key, requested) {
                complete = true;
            } else {
                partial = true;
            }
        }

        let coverage = if complete {
            PointerStrongCoverage::Complete
        } else if partial {
            PointerStrongCoverage::Partial
        } else {
            PointerStrongCoverage::None
        };
        PointerStrongLookup { targets, coverage }
    }

    fn weak_targets_for_request(&self, requested: &PointerSlotPlace<'db>) -> PointerTargets<'db> {
        pointer_targets_for_request(&self.weak, requested)
    }

    fn join_reachable(&mut self, other: &Self) -> bool {
        let mut changed = false;
        let self_strong = mem::take(&mut self.strong);
        let mut joined_strong = FxHashMap::default();
        let mut keys = FxHashSet::default();
        keys.extend(self_strong.keys().cloned());
        keys.extend(other.strong.keys().cloned());

        for key in keys {
            match (self_strong.get(&key), other.strong.get(&key)) {
                (Some(lhs), Some(rhs)) => {
                    let mut joined = lhs.clone();
                    joined.join_into(rhs);
                    if !joined.is_empty() {
                        changed |= self_strong.get(&key) != Some(&joined);
                        joined_strong.insert(key, joined);
                    } else {
                        changed |= self_strong.contains_key(&key);
                    }
                }
                (Some(targets), None) => {
                    changed = true;
                    changed |= self.assign_weak(key, targets.clone());
                }
                (None, Some(targets)) => {
                    changed |= self.assign_weak(key, targets.clone());
                }
                (None, None) => {}
            }
        }

        self.strong = joined_strong;
        for (key, targets) in &other.weak {
            changed |= self.assign_weak(key.clone(), targets.clone());
        }

        changed
    }

    fn writes_since(
        &self,
        initial: &Self,
    ) -> Vec<(PointerSlotPlace<'db>, PointerTargets<'db>, WritePrecision)> {
        let mut keys = FxHashSet::default();
        keys.extend(self.strong.keys().cloned());
        keys.extend(self.weak.keys().cloned());
        keys.into_iter()
            .filter_map(|key| {
                let strong = self.strong.get(&key);
                let weak = self.weak.get(&key);
                if strong == initial.strong.get(&key) && weak == initial.weak.get(&key) {
                    return None;
                }
                let mut targets = PointerTargets::default();
                if let Some(strong) = strong {
                    targets.join_into(strong);
                }
                if let Some(weak) = weak {
                    targets.join_into(weak);
                }
                (!targets.is_empty()).then_some((
                    key,
                    targets,
                    if strong.is_some() {
                        WritePrecision::Strong
                    } else {
                        WritePrecision::Weak
                    },
                ))
            })
            .collect()
    }
}

fn pointer_targets_for_request<'db>(
    facts: &FxHashMap<PointerSlotPlace<'db>, PointerTargets<'db>>,
    requested: &PointerSlotPlace<'db>,
) -> PointerTargets<'db> {
    let mut out = PointerTargets::default();
    for (stored_key, targets) in facts {
        if pointer_slots_read_may_alias(stored_key, targets, requested) {
            out.join_into(targets);
        }
    }
    out
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct State<'db> {
    reachable: bool,
    pub(super) local_loans: FxHashMap<SLocalId, FxHashSet<LoanId>>,
    pub(super) pointer_targets: PointerTargetState<'db>,
}

impl<'db> State<'db> {
    pub(super) fn mark_reachable(&mut self) {
        self.reachable = true;
    }

    pub(super) fn is_reachable(&self) -> bool {
        self.reachable
    }

    fn mark_unreachable(&mut self) {
        *self = Self::default();
    }

    pub(super) fn loans_in(&self, local: SLocalId) -> FxHashSet<LoanId> {
        self.local_loans.get(&local).cloned().unwrap_or_default()
    }

    pub(super) fn assign_loans(&mut self, local: SLocalId, loans: FxHashSet<LoanId>) {
        if loans.is_empty() {
            self.local_loans.remove(&local);
        } else {
            self.local_loans.insert(local, loans);
        }
    }

    pub(super) fn assign_pointer_targets(
        &mut self,
        key: PointerSlotPlace<'db>,
        targets: PointerTargets<'db>,
    ) {
        self.pointer_targets.assign_strong(key, targets);
    }

    pub(super) fn update_pointer_targets(
        &mut self,
        key: PointerSlotPlace<'db>,
        targets: PointerTargets<'db>,
        weak: bool,
    ) {
        if weak {
            self.pointer_targets.assign_weak(key, targets);
        } else {
            self.assign_pointer_targets(key, targets);
        }
    }

    pub(super) fn clear_pointer_target(&mut self, key: &PointerSlotPlace<'db>) {
        self.pointer_targets.clear_strong(key);
    }

    fn invalidate_pointer_targets(
        &mut self,
        key: PointerSlotPlace<'db>,
        targets: PointerTargets<'db>,
        weak: bool,
    ) {
        if weak {
            self.pointer_targets.assign_weak(key, targets);
        } else {
            self.pointer_targets.invalidate_strong(key, targets);
        }
    }

    pub(super) fn pointer_writes_since(
        &self,
        initial: &Self,
    ) -> Vec<(CanonPlace<'db>, PointerTargets<'db>, WritePrecision)> {
        self.pointer_targets
            .writes_since(&initial.pointer_targets)
            .into_iter()
            .map(|(place, targets, precision)| {
                (
                    CanonPlace {
                        root: place.root,
                        proj: place.proj,
                    },
                    targets,
                    precision,
                )
            })
            .collect()
    }
}

impl JoinSemiLattice for State<'_> {
    fn join_into(&mut self, other: &Self) -> bool {
        if !other.reachable {
            return false;
        }
        if !self.reachable {
            *self = other.clone();
            return true;
        }
        let mut changed = false;
        for (local, loans) in &other.local_loans {
            let entry = self.local_loans.entry(*local).or_default();
            let before = entry.len();
            entry.extend(loans.iter().copied());
            changed |= before != entry.len();
        }
        changed |= self.pointer_targets.join_reachable(&other.pointer_targets);
        changed
    }
}

#[derive(Clone, Copy)]
pub(super) struct BorrowCanonCx<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &'a NormalizedSemanticBody<'db>,
    facts: &'a NormalizedBodyFacts,
    loans: &'a [Loan<'db>],
}

impl<'a, 'db> BorrowCanonCx<'a, 'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: &'a NormalizedSemanticBody<'db>,
        facts: &'a NormalizedBodyFacts,
        loans: &'a [Loan<'db>],
    ) -> Self {
        Self {
            db,
            instance,
            body,
            facts,
            loans,
        }
    }

    pub(super) fn canonicalize_value_base(
        &self,
        state: &State<'db>,
        local: SLocalId,
    ) -> FxHashSet<CanonPlace<'db>> {
        if self
            .body
            .local(local)
            .is_some_and(|local| local.ty.as_borrow(self.db).is_some())
        {
            return self.borrow_local_targets(state, local);
        }

        let Some(local_data) = self.body.local(local) else {
            return FxHashSet::default();
        };
        if let Some(place) = local_data.lowering.place() {
            return self.canonicalize_place_targets(state, place);
        }
        let root = match &local_data.lowering {
            NormalizedBindingLowering::CarrierLocal { root, provider, .. } => provider
                .clone()
                .map(BorrowRoot::Provider)
                .or_else(|| root.and_then(|root| self.root_to_borrow_root(root))),
            NormalizedBindingLowering::Erased => None,
            NormalizedBindingLowering::ValueLocal { .. }
            | NormalizedBindingLowering::PlaceBoundValue { .. } => unreachable!(),
        };
        root.into_iter()
            .map(|root| CanonPlace {
                root,
                proj: NSProjectionPath::default(),
            })
            .collect()
    }

    pub(super) fn borrow_local_targets(
        &self,
        state: &State<'db>,
        local: SLocalId,
    ) -> FxHashSet<CanonPlace<'db>> {
        let mut out = FxHashSet::default();
        for loan in state.loans_in(local) {
            out.extend(self.loans[loan.0 as usize].targets.iter().cloned());
        }
        if !out.is_empty() {
            return out;
        }

        let Some(local_data) = self.body.local(local) else {
            return FxHashSet::default();
        };
        if let Some(place) = local_data.lowering.place() {
            return self.canonicalize_place_targets(state, place);
        }
        match &local_data.lowering {
            NormalizedBindingLowering::CarrierLocal { root, provider, .. } => provider
                .clone()
                .map(BorrowRoot::Provider)
                .or_else(|| root.and_then(|root| self.root_to_borrow_root(root)))
                .into_iter()
                .map(|root| CanonPlace {
                    root,
                    proj: NSProjectionPath::default(),
                })
                .collect(),
            NormalizedBindingLowering::Erased => FxHashSet::default(),
            NormalizedBindingLowering::ValueLocal { .. }
            | NormalizedBindingLowering::PlaceBoundValue { .. } => FxHashSet::default(),
        }
    }

    pub(super) fn canonicalize_place(
        &self,
        state: &State<'db>,
        place: &NSPlace<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<FxHashSet<CanonPlace<'db>>, SemanticBorrowDiagnostic<'db>> {
        let bases = match place.root {
            NSPlaceRoot::Root(root) => FxHashSet::from_iter([CanonPlace {
                root: self
                    .root_to_borrow_root(root)
                    .expect("normalized borrow root"),
                proj: NSProjectionPath::default(),
            }]),
            NSPlaceRoot::CarrierDerefLocal(local) => {
                let suffix = self.carrier_deref_suffix(local, &place.path);
                let mut out = FxHashSet::default();
                let loans = state.loans_in(local);
                for loan in &loans {
                    out.extend(self.loans[loan.0 as usize].targets.iter().cloned());
                }
                if out.is_empty() && !loans.is_empty() {
                    out.insert(CanonPlace {
                        root: BorrowRoot::UnknownMemory(PointerAddressSpaces::Any),
                        proj: NSProjectionPath::default(),
                    });
                } else if out.is_empty()
                    && let Some(local_data) = self.body.local(local)
                {
                    match &local_data.lowering {
                        NormalizedBindingLowering::CarrierLocal { root, provider, .. } => {
                            if let Some(provider) = provider {
                                out.insert(CanonPlace {
                                    root: BorrowRoot::Provider(provider.clone()),
                                    proj: NSProjectionPath::default(),
                                });
                            } else if let Some(root) =
                                root.and_then(|root| self.root_to_borrow_root(root))
                            {
                                out.insert(CanonPlace {
                                    root,
                                    proj: NSProjectionPath::default(),
                                });
                            }
                        }
                        NormalizedBindingLowering::ValueLocal { place }
                        | NormalizedBindingLowering::PlaceBoundValue { place, .. } => {
                            out.extend(self.canonicalize_place(state, place, origin)?);
                        }
                        NormalizedBindingLowering::Erased => {}
                    }
                }
                if out.is_empty() {
                    return Err(self.internal_diag(
                        origin,
                        "cannot canonicalize carrier-rooted place".to_string(),
                    ));
                }
                return self.canonicalize_path_from_places(&out, &suffix, state, origin);
            }
        };
        self.canonicalize_path_from_places(&bases, &place.path, state, origin)
    }

    fn canonicalize_place_targets(
        &self,
        state: &State<'db>,
        place: &NSPlace<'db>,
    ) -> FxHashSet<CanonPlace<'db>> {
        match place.root {
            NSPlaceRoot::Root(root) => FxHashSet::from_iter([CanonPlace {
                root: self
                    .root_to_borrow_root(root)
                    .expect("normalized borrow root"),
                proj: place.path.clone(),
            }]),
            NSPlaceRoot::CarrierDerefLocal(local) => {
                let suffix = self.carrier_deref_suffix(local, &place.path);
                let mut out = FxHashSet::default();
                let mut resolved = false;
                for loan in state.loans_in(local) {
                    resolved = true;
                    for target in &self.loans[loan.0 as usize].targets {
                        out.insert(CanonPlace {
                            root: target.root.clone(),
                            proj: target.proj.concat(&suffix),
                        });
                    }
                }
                if !resolved
                    && let Some(NormalizedBindingLowering::CarrierLocal { root, provider, .. }) =
                        self.body.local(local).map(|local| &local.lowering)
                {
                    if let Some(provider) = provider {
                        out.insert(CanonPlace {
                            root: BorrowRoot::Provider(provider.clone()),
                            proj: suffix.clone(),
                        });
                    } else if let Some(root) = root.and_then(|root| self.root_to_borrow_root(root))
                    {
                        out.insert(CanonPlace { root, proj: suffix });
                    }
                }
                out
            }
        }
    }

    pub(super) fn canonicalize_value_path(
        &self,
        state: &State<'db>,
        local: SLocalId,
        path: &NSProjectionPath<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<FxHashSet<CanonPlace<'db>>, SemanticBorrowDiagnostic<'db>> {
        let bases = self.canonicalize_value_base(state, local);
        self.canonicalize_path_from_places(&bases, path, state, origin)
    }

    fn canonicalize_path_from_places(
        &self,
        bases: &FxHashSet<CanonPlace<'db>>,
        path: &NSProjectionPath<'db>,
        state: &State<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<FxHashSet<CanonPlace<'db>>, SemanticBorrowDiagnostic<'db>> {
        let mut places = bases.clone();
        for projection in path.iter() {
            if matches!(projection, crate::projection::Projection::Deref) {
                places = self.resolve_pointer_deref(state, &places, origin)?;
            } else {
                places = places
                    .into_iter()
                    .map(|place| CanonPlace {
                        root: place.root,
                        proj: path_with_projection(&place.proj, projection.clone()),
                    })
                    .collect();
            }
        }
        Ok(places)
    }

    fn resolve_pointer_deref(
        &self,
        state: &State<'db>,
        keys: &FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<FxHashSet<CanonPlace<'db>>, SemanticBorrowDiagnostic<'db>> {
        Ok(self
            .pointer_targets_for_keys(state, keys.clone(), origin)?
            .places())
    }

    pub(super) fn pointer_targets_for_value_path(
        &self,
        state: &State<'db>,
        local: SLocalId,
        path: &NSProjectionPath<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<PointerTargets<'db>, SemanticBorrowDiagnostic<'db>> {
        let bases = self.canonicalize_value_base(state, local);
        let keys = self.canonicalize_path_from_places(&bases, path, state, origin)?;
        self.pointer_targets_for_keys(state, keys, origin)
    }

    fn pointer_targets_for_keys(
        &self,
        state: &State<'db>,
        keys: FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<PointerTargets<'db>, SemanticBorrowDiagnostic<'db>> {
        let mut out: Option<PointerTargets<'db>> = None;
        for key in keys {
            let targets = self.pointer_targets_for_key(state, &key, origin)?;
            if let Some(out) = &mut out {
                out.join_into(&targets);
            } else {
                out = Some(targets);
            }
        }
        Ok(out.unwrap_or_else(|| {
            PointerTargets::unknown(PointerAddressSpaces::one(ProviderAddressSpace::Memory))
        }))
    }

    fn pointer_targets_for_key(
        &self,
        state: &State<'db>,
        key: &CanonPlace<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<PointerTargets<'db>, SemanticBorrowDiagnostic<'db>> {
        let requested = self.pointer_slot_place(key.root.clone(), key.proj.clone());
        let strong = state.pointer_targets.strong_lookup(&requested);
        let mut out = match strong.coverage {
            PointerStrongCoverage::Complete => strong.targets,
            PointerStrongCoverage::Partial => {
                let mut out = self.default_pointer_targets_for_key(key, origin)?;
                out.join_into(&strong.targets);
                out
            }
            PointerStrongCoverage::None => self.default_pointer_targets_for_key(key, origin)?,
        };
        out.join_into(&state.pointer_targets.weak_targets_for_request(&requested));
        Ok(out)
    }

    fn default_pointer_targets_for_key(
        &self,
        key: &CanonPlace<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<PointerTargets<'db>, SemanticBorrowDiagnostic<'db>> {
        match key.root {
            BorrowRoot::UnknownMemory(spaces) => Ok(PointerTargets::unknown(spaces)),
            BorrowRoot::FreshAllocation { .. } => Ok(PointerTargets::unknown(
                PointerAddressSpaces::one(ProviderAddressSpace::Memory),
            )),
            BorrowRoot::Param(_) | BorrowRoot::Local(_) | BorrowRoot::Provider(_) => {
                if matches!(key.root, BorrowRoot::Param(_) | BorrowRoot::Provider(_))
                    && let Some(suffix) = self.default_pointer_target_suffix(key)
                {
                    Ok(PointerTargets::known(FxHashSet::from_iter([CanonPlace {
                        root: key.root.clone(),
                        proj: key.proj.concat(&suffix),
                    }])))
                } else if self.type_of_canon_place(key, origin).is_some()
                    || matches!(key.root, BorrowRoot::Param(_) | BorrowRoot::Local(_))
                {
                    Ok(PointerTargets::unknown(PointerAddressSpaces::one(
                        ProviderAddressSpace::Memory,
                    )))
                } else if matches!(key.root, BorrowRoot::Provider(_)) {
                    Ok(PointerTargets::known(FxHashSet::from_iter([CanonPlace {
                        root: key.root.clone(),
                        proj: path_with_projection(&key.proj, Projection::Deref),
                    }])))
                } else {
                    Err(self.internal_diag(
                        origin,
                        "cannot resolve pointer dereference target".to_string(),
                    ))
                }
            }
        }
    }

    fn carrier_deref_suffix(
        &self,
        local: SLocalId,
        suffix: &NSProjectionPath<'db>,
    ) -> NSProjectionPath<'db> {
        if self
            .body
            .local(local)
            .is_some_and(|local| local.ty.as_ptr(self.db).is_some())
        {
            let mut path = NSProjectionPath::from_projection(crate::projection::Projection::Deref);
            path = path.concat(suffix);
            path
        } else {
            suffix.clone()
        }
    }

    fn default_pointer_target_suffix(
        &self,
        key: &CanonPlace<'db>,
    ) -> Option<NSProjectionPath<'db>> {
        let ty = self.type_of_canon_place(key, SemOrigin::Synthetic)?;
        raw_pointer_pointee_suffix(self.db, ty).or_else(|| mem_array_carrier_suffix(self.db, ty))
    }

    fn type_of_canon_place(
        &self,
        place: &CanonPlace<'db>,
        _origin: SemOrigin<'db>,
    ) -> Option<TyId<'db>> {
        let mut ty = match &place.root {
            BorrowRoot::Param(idx) => {
                let local = self.param_local(*idx)?;
                self.storage_ty_for_local(local)?
            }
            BorrowRoot::Local(local) => self.storage_ty_for_local(*local)?,
            BorrowRoot::Provider(binding) => binding.effective_target_ty(),
            BorrowRoot::FreshAllocation { .. } | BorrowRoot::UnknownMemory(_) => return None,
        };
        for projection in place.proj.iter() {
            ty = projection_result_ty(self.db, ty, projection)?;
        }
        Some(ty)
    }

    pub(super) fn param_local(&self, idx: u32) -> Option<SLocalId> {
        self.body.borrow_roots.iter().find_map(|root| match root {
            NBorrowRoot::Param { local, param_idx } if *param_idx == idx => Some(*local),
            NBorrowRoot::Param { .. }
            | NBorrowRoot::LocalSlot { .. }
            | NBorrowRoot::Provider { .. } => None,
        })
    }

    fn storage_ty_for_local(&self, local: SLocalId) -> Option<TyId<'db>> {
        let local = self.body.local(local)?;
        Some(match &local.lowering {
            NormalizedBindingLowering::CarrierLocal { target_ty, .. } => target_ty,
            NormalizedBindingLowering::Erased
            | NormalizedBindingLowering::ValueLocal { .. }
            | NormalizedBindingLowering::PlaceBoundValue { .. } => &local.ty,
        })
        .copied()
    }

    fn pointer_slot_place(
        &self,
        root: BorrowRoot<'db>,
        proj: NSProjectionPath<'db>,
    ) -> PointerSlotPlace<'db> {
        PointerSlotPlace::new(root, self.normalize_pointer_slot_path(&proj))
    }

    fn normalize_pointer_slot_path(&self, path: &NSProjectionPath<'db>) -> NSProjectionPath<'db> {
        let mut out = NSProjectionPath::default();
        for projection in path.iter() {
            out.push(self.normalize_pointer_slot_projection(projection.clone()));
        }
        out
    }

    fn normalize_pointer_slot_projection(
        &self,
        projection: Projection<TyId<'db>, VariantIndex, SLocalId>,
    ) -> Projection<TyId<'db>, VariantIndex, SLocalId> {
        match projection {
            Projection::Index(IndexSource::Dynamic(index)) => self
                .constant_index_value(index)
                .map(|index| Projection::Index(IndexSource::Constant(index)))
                .unwrap_or(Projection::Index(IndexSource::Dynamic(index))),
            projection => projection,
        }
    }

    fn constant_index_value(&self, local: SLocalId) -> Option<usize> {
        // Normalization represents direct literal indices as value locals.
        // Borrowck consumes the normalized def table instead of rediscovering
        // def-use facts by scanning statements: a local is constant only when
        // it has one constant definition. Reassigned or computed indices remain
        // dynamic and therefore imprecise. Definite-assignment validation
        // guarantees that the sole definition dominates valid uses.
        let [assignment] = self.facts.defs_by_local(local) else {
            return None;
        };
        let assignment = self.facts.assignment(*assignment)?;
        let stmt = self
            .body
            .block(assignment.block)?
            .stmts
            .get(assignment.stmt_idx)?;
        let NSStmtKind::Assign { dst, expr } = &stmt.kind else {
            return None;
        };
        if *dst != local {
            return None;
        }
        let NExpr::Const(SConst::Value(value)) = expr else {
            return None;
        };
        let SemConstValue::Scalar { value, .. } = value.value(self.db) else {
            return None;
        };
        scalar_int(&value).and_then(|value| value.to_usize())
    }

    fn structural_local_places(&self, local: SLocalId) -> FxHashSet<CanonPlace<'db>> {
        let Some(local_data) = self.body.local(local) else {
            return FxHashSet::default();
        };
        if let Some(place) = local_data.lowering.place() {
            return place
                .root
                .borrow_root()
                .and_then(|root| self.root_to_borrow_root(root))
                .into_iter()
                .map(|root| CanonPlace {
                    root,
                    proj: place.path.clone(),
                })
                .collect();
        }
        match &local_data.lowering {
            NormalizedBindingLowering::CarrierLocal { root, provider, .. } => provider
                .clone()
                .map(BorrowRoot::Provider)
                .or_else(|| root.and_then(|root| self.root_to_borrow_root(root)))
                .into_iter()
                .map(|root| CanonPlace {
                    root,
                    proj: NSProjectionPath::default(),
                })
                .collect(),
            NormalizedBindingLowering::Erased => FxHashSet::default(),
            NormalizedBindingLowering::ValueLocal { .. }
            | NormalizedBindingLowering::PlaceBoundValue { .. } => unreachable!(),
        }
    }

    pub(super) fn root_to_borrow_root(&self, root: NBorrowRootId) -> Option<BorrowRoot<'db>> {
        match self.body.root(root)? {
            NBorrowRoot::Param { param_idx, .. } => Some(BorrowRoot::Param(*param_idx)),
            NBorrowRoot::LocalSlot { local } => Some(BorrowRoot::Local(*local)),
            NBorrowRoot::Provider { binding, .. } => Some(BorrowRoot::Provider(binding.clone())),
        }
    }

    pub(super) fn mut_loans_for_place(
        &self,
        state: &State<'db>,
        place: &NSPlace<'db>,
    ) -> FxHashSet<LoanId> {
        self.mut_loans(self.loans_for_place(state, place))
    }

    fn loans_for_place(&self, state: &State<'db>, place: &NSPlace<'db>) -> FxHashSet<LoanId> {
        match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => state.loans_in(local),
            NSPlaceRoot::Root(root) => self
                .body
                .root(root)
                .map(|root| state.loans_in(root.authorization_local()))
                .unwrap_or_default(),
        }
    }

    pub(super) fn mut_loans_for_value(
        &self,
        state: &State<'db>,
        local: SLocalId,
    ) -> FxHashSet<LoanId> {
        self.mut_loans(self.loans_for_value(state, local))
    }

    fn loans_for_value(&self, state: &State<'db>, local: SLocalId) -> FxHashSet<LoanId> {
        state.loans_in(local)
    }

    fn mut_loans(&self, loans: FxHashSet<LoanId>) -> FxHashSet<LoanId> {
        loans
            .into_iter()
            .filter(|loan| self.loans[loan.0 as usize].kind == BorrowKind::Mut)
            .collect()
    }

    fn internal_diag(
        &self,
        origin: SemOrigin<'db>,
        message: String,
    ) -> SemanticBorrowDiagnostic<'db> {
        normalized_body_internal_diag(self.db, self.instance, self.body, origin, message)
    }
}

pub(super) fn places_overlap<'db>(lhs: &CanonPlace<'db>, rhs: &CanonPlace<'db>) -> bool {
    if param_pointer_pointees_overlap(lhs, rhs) {
        return true;
    }
    if roots_overlap_conservatively(&lhs.root, &rhs.root) {
        return true;
    }
    lhs.root == rhs.root && place_paths_overlap(&lhs.proj, &rhs.proj)
}

fn place_paths_overlap<'db>(lhs: &NSProjectionPath<'db>, rhs: &NSProjectionPath<'db>) -> bool {
    if matches!(lhs.may_alias(rhs), Aliasing::No) {
        return false;
    }
    let shorter_len = lhs.len().min(rhs.len());
    let longer = if lhs.len() < rhs.len() { rhs } else { lhs };
    lhs.len() == rhs.len()
        || !longer
            .iter()
            .skip(shorter_len)
            .any(|projection| projection == &Projection::Deref)
}

fn pointer_slots_may_alias<'db>(lhs: &PointerSlotPlace<'db>, rhs: &PointerSlotPlace<'db>) -> bool {
    if roots_overlap_conservatively(&lhs.root, &rhs.root) {
        return true;
    }
    lhs.root == rhs.root && pointer_slot_paths_may_alias(&lhs.proj, &rhs.proj)
}

fn pointer_slots_read_may_alias<'db>(
    stored: &PointerSlotPlace<'db>,
    stored_targets: &PointerTargets<'db>,
    requested: &PointerSlotPlace<'db>,
) -> bool {
    pointer_slots_may_alias(stored, requested)
        || (stored_targets.has_unknown() && pointer_slot_write_may_affect(stored, requested))
}

fn pointer_slot_write_may_affect<'db>(
    written: &PointerSlotPlace<'db>,
    slot: &PointerSlotPlace<'db>,
) -> bool {
    if roots_overlap_conservatively(&written.root, &slot.root) {
        return true;
    }
    written.root == slot.root && pointer_slot_path_write_may_affect(&written.proj, &slot.proj)
}

fn pointer_slot_covers<'db>(
    stored: &PointerSlotPlace<'db>,
    requested: &PointerSlotPlace<'db>,
) -> bool {
    stored.root == requested.root && pointer_slot_path_covers(&stored.proj, &requested.proj)
}

fn pointer_slot_path_covers<'db>(
    stored: &NSProjectionPath<'db>,
    requested: &NSProjectionPath<'db>,
) -> bool {
    stored.len() == requested.len()
        && stored
            .iter()
            .zip(requested.iter())
            .all(|(stored, requested)| pointer_slot_projection_covers(stored, requested))
}

fn pointer_slot_projection_covers<'db>(
    stored: &Projection<TyId<'db>, crate::analysis::semantic::VariantIndex, SLocalId>,
    requested: &Projection<TyId<'db>, crate::analysis::semantic::VariantIndex, SLocalId>,
) -> bool {
    stored == requested
        || matches!(
            (stored, requested),
            (
                Projection::Index(IndexSource::Any),
                Projection::Index(
                    IndexSource::Constant(_) | IndexSource::Dynamic(_) | IndexSource::Any
                )
            )
        )
}

fn pointer_slot_paths_may_alias<'db>(
    lhs: &NSProjectionPath<'db>,
    rhs: &NSProjectionPath<'db>,
) -> bool {
    lhs.len() == rhs.len()
        && lhs
            .iter()
            .zip(rhs.iter())
            .all(|(lhs, rhs)| pointer_slot_projection_may_alias(lhs, rhs))
}

fn pointer_slot_path_write_may_affect<'db>(
    written: &NSProjectionPath<'db>,
    slot: &NSProjectionPath<'db>,
) -> bool {
    let mut written = written.iter();
    let mut slot = slot.iter();
    loop {
        match (written.next(), slot.next()) {
            (Some(written), Some(slot)) if pointer_slot_projection_may_alias(written, slot) => {}
            (Some(_), Some(_)) => return false,
            (None, _) => return true,
            (Some(Projection::Deref), None) => return false,
            (Some(_), None) => return true,
        }
    }
}

fn pointer_slot_projection_may_alias<'db>(
    lhs: &Projection<TyId<'db>, crate::analysis::semantic::VariantIndex, SLocalId>,
    rhs: &Projection<TyId<'db>, crate::analysis::semantic::VariantIndex, SLocalId>,
) -> bool {
    match (lhs, rhs) {
        (Projection::Field(lhs), Projection::Field(rhs)) => lhs == rhs,
        (
            Projection::VariantField {
                variant: lhs_variant,
                field_idx: lhs_field,
                ..
            },
            Projection::VariantField {
                variant: rhs_variant,
                field_idx: rhs_field,
                ..
            },
        ) => lhs_variant == rhs_variant && lhs_field == rhs_field,
        (Projection::Index(lhs), Projection::Index(rhs)) => pointer_indices_may_alias(lhs, rhs),
        (Projection::Deref, Projection::Deref) => true,
        (Projection::Discriminant, Projection::Discriminant) => true,
        _ => false,
    }
}

fn pointer_indices_may_alias<Idx>(lhs: &IndexSource<Idx>, rhs: &IndexSource<Idx>) -> bool {
    match (lhs, rhs) {
        (IndexSource::Constant(lhs), IndexSource::Constant(rhs)) => lhs == rhs,
        (IndexSource::Dynamic(_), IndexSource::Dynamic(_)) => true,
        (IndexSource::Constant(_), IndexSource::Dynamic(_))
        | (IndexSource::Dynamic(_), IndexSource::Constant(_))
        | (IndexSource::Any, _)
        | (_, IndexSource::Any) => true,
    }
}

fn roots_overlap_conservatively<'db>(lhs: &BorrowRoot<'db>, rhs: &BorrowRoot<'db>) -> bool {
    match (lhs, rhs) {
        (BorrowRoot::UnknownMemory(lhs), BorrowRoot::UnknownMemory(rhs)) => {
            unknown_spaces_overlap(*lhs, *rhs)
        }
        (BorrowRoot::UnknownMemory(spaces), other) | (other, BorrowRoot::UnknownMemory(spaces)) => {
            unknown_overlaps_root(*spaces, other)
        }
        _ => false,
    }
}

fn param_pointer_pointees_overlap<'db>(lhs: &CanonPlace<'db>, rhs: &CanonPlace<'db>) -> bool {
    let (BorrowRoot::Param(lhs_param), BorrowRoot::Param(rhs_param)) = (&lhs.root, &rhs.root)
    else {
        return false;
    };
    lhs_param != rhs_param
        && path_reaches_pointer_pointee(&lhs.proj)
        && path_reaches_pointer_pointee(&rhs.proj)
}

fn path_reaches_pointer_pointee<'db>(path: &NSProjectionPath<'db>) -> bool {
    path.iter()
        .any(|projection| matches!(projection, Projection::Deref))
}

fn unknown_spaces_overlap(lhs: PointerAddressSpaces, rhs: PointerAddressSpaces) -> bool {
    matches!(
        (lhs, rhs),
        (PointerAddressSpaces::Any, _) | (_, PointerAddressSpaces::Any)
    ) || lhs == rhs
}

fn unknown_overlaps_root<'db>(spaces: PointerAddressSpaces, root: &BorrowRoot<'db>) -> bool {
    unknown_spaces_overlap(spaces, address_spaces_for_root_conservative(root))
}

fn address_spaces_for_root_conservative<'db>(root: &BorrowRoot<'db>) -> PointerAddressSpaces {
    match root {
        BorrowRoot::Param(_) | BorrowRoot::Local(_) => {
            PointerAddressSpaces::one(ProviderAddressSpace::Memory)
        }
        BorrowRoot::FreshAllocation { address_space, .. } => {
            PointerAddressSpaces::one(*address_space)
        }
        BorrowRoot::UnknownMemory(spaces) => *spaces,
        BorrowRoot::Provider(binding) => binding
            .semantics
            .address_space
            .map(PointerAddressSpaces::one)
            .unwrap_or_else(|| {
                if matches!(binding.semantics.kind, ProviderKind::RootObject) {
                    PointerAddressSpaces::one(ProviderAddressSpace::Memory)
                } else {
                    PointerAddressSpaces::Any
                }
            }),
    }
}
