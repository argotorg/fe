use std::mem;

use cranelift_entity::SecondaryMap;
use dataflow::JoinSemiLattice;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            FieldIndex, SBlockId, SConst, SLocalId, SemOrigin, SemanticCalleeRef, SemanticInstance,
            VariantIndex,
            consts::{SemConstValue, scalar_int},
            get_or_build_semantic_instance,
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
    analyses::BorrowSummaryMode,
    check::{provisional_memory_summary_voucher, semantic_memory_summary_voucher},
    diagnostics::normalized_body_internal_diag,
    facts::NormalizedBodyFacts,
    ir::{
        FreshAllocSite, MemoryAccessKind, MemorySummaryId, MemorySummaryItem, MemorySummaryTarget,
        NBorrowRoot, NBorrowRootId, NEffectArg, NEffectArgValue, NExpr, NSPlace, NSPlaceRoot,
        NSProjectionPath, NSStmt, NSStmtKind, NormalizedBindingLowering, NormalizedSemanticBody,
        PointerAddressSpaces, SemanticBorrowDiagnostic,
    },
    pointer::{
        is_pointer_bearing_type, mem_array_carrier_suffix, path_with_projection, pointer_slots,
        projection_result_ty, raw_pointer_pointee_suffix,
    },
};

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

pub(super) struct ResolvedMemoryAccess<'db> {
    pub(super) kind: MemoryAccessKind,
    pub(super) targets: FxHashSet<CanonPlace<'db>>,
    pub(super) authorized: FxHashSet<LoanId>,
    pub(super) authorizer_targets: FxHashSet<CanonPlace<'db>>,
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

pub(super) struct BorrowCanonCx<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &'a NormalizedSemanticBody<'db>,
    facts: &'a NormalizedBodyFacts,
    loans: &'a [Loan<'db>],
    loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
    summary_mode: BorrowSummaryMode,
}

impl<'a, 'db> BorrowCanonCx<'a, 'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: &'a NormalizedSemanticBody<'db>,
        facts: &'a NormalizedBodyFacts,
        loans: &'a [Loan<'db>],
        loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
        summary_mode: BorrowSummaryMode,
    ) -> Self {
        Self {
            db,
            instance,
            body,
            facts,
            loans,
            loan_for_local,
            summary_mode,
        }
    }

    fn assign_pointer_targets_for_expr(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        expr: &NExpr<'db>,
        call_summary: Option<MemorySummaryId<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let Some(dst_ty) = self.body.local(dst).map(|local| local.ty) else {
            return Ok(());
        };
        if dst_ty.as_borrow(self.db).is_some() || !is_pointer_bearing_type(self.db, dst_ty) {
            return Ok(());
        }
        match expr {
            NExpr::Use(src) => {
                self.copy_pointer_targets_from_value(state, dst, src.local, dst_ty, origin)
            }
            NExpr::ReadPlace { place, .. } => {
                self.copy_pointer_targets_from_place(state, dst, place, dst_ty, origin)
            }
            NExpr::AggregateMake { ty, fields } => {
                self.assign_aggregate_pointer_targets(state, dst, *ty, fields, origin)
            }
            NExpr::ArrayRepeat { ty, value } => {
                self.assign_array_repeat_pointer_targets(state, dst, *ty, *value, origin)
            }
            NExpr::EnumMake {
                enum_ty,
                variant,
                fields,
            } => self.assign_enum_pointer_targets(state, dst, *enum_ty, *variant, fields, origin),
            NExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => self
                .copy_pointer_targets_from_enum_field(state, dst, *value, *variant, *field, origin),
            NExpr::Cast { value, to } if is_pointer_bearing_type(self.db, *to) => {
                let src_ty = self.body.local(value.local).map(|local| local.ty);
                if src_ty.is_some_and(|ty| is_pointer_bearing_type(self.db, ty)) {
                    self.copy_pointer_targets_from_value(state, dst, value.local, dst_ty, origin)
                } else {
                    // Non-pointer-to-pointer casts model raw address construction
                    // such as integer-to-pointer. Address-of or borrow-to-pointer
                    // lowering must preserve Local/Provider provenance before it
                    // reaches this fallback.
                    self.assign_unknown_pointer_targets(state, dst, dst_ty);
                    Ok(())
                }
            }
            NExpr::Call {
                args, effect_args, ..
            } => {
                let summary = call_summary.expect("call expression must have a memory summary");
                let dst_bases = self.canonicalize_value_base(state, dst);
                let mut by_output: FxHashMap<NSProjectionPath<'db>, PointerTargets<'db>> =
                    FxHashMap::default();
                for item in summary.items(self.db) {
                    let MemorySummaryItem::ReturnPointer { output, targets } = item else {
                        continue;
                    };
                    let item_targets =
                        self.resolve_summary_targets(state, args, effect_args, targets, origin)?;
                    by_output
                        .entry(output.clone())
                        .and_modify(|existing| {
                            existing.join_into(&item_targets);
                        })
                        .or_insert(item_targets);
                }
                for (output, targets) in by_output {
                    for base in &dst_bases {
                        let key =
                            self.pointer_slot_place(base.root.clone(), base.proj.concat(&output));
                        state.assign_pointer_targets(key, targets.clone());
                    }
                }
                Ok(())
            }
            _ => {
                self.assign_unknown_pointer_targets(state, dst, dst_ty);
                Ok(())
            }
        }
    }

    fn resolve_summary_targets(
        &self,
        state: &State<'db>,
        args: &[super::ir::NOperand],
        effect_args: &[NEffectArg<'db>],
        summary: &[MemorySummaryTarget<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<PointerTargets<'db>, SemanticBorrowDiagnostic<'db>> {
        let mut out = PointerTargets::default();
        for target in summary {
            out.join_into(
                &self
                    .resolve_summary_target(state, args, effect_args, target, origin)?
                    .0,
            );
        }
        Ok(out)
    }

    fn resolve_summary_target(
        &self,
        state: &State<'db>,
        args: &[super::ir::NOperand],
        effect_args: &[NEffectArg<'db>],
        target: &MemorySummaryTarget<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(PointerTargets<'db>, FxHashSet<LoanId>), SemanticBorrowDiagnostic<'db>> {
        Ok(match target {
            MemorySummaryTarget::Input { input, proj } => {
                let super::ir::BorrowInputRef::Param(idx) = input;
                let Some(arg) = args.get(*idx as usize) else {
                    return Err(self.internal_diag(
                        origin,
                        format!("memory summary refers to missing parameter {idx}"),
                    ));
                };
                (
                    PointerTargets::known(
                        self.canonicalize_value_path(state, arg.local, proj, origin)?,
                    ),
                    self.loans_for_value(state, arg.local),
                )
            }
            MemorySummaryTarget::Effect { binding_idx, proj } => {
                let Some(arg) = effect_args
                    .iter()
                    .find(|arg| arg.binding_idx == *binding_idx)
                else {
                    return Err(self.internal_diag(
                        origin,
                        format!("memory summary refers to missing effect argument {binding_idx}"),
                    ));
                };
                match &arg.arg {
                    NEffectArgValue::Place(place) => {
                        let bases = self.canonicalize_place(state, place, origin)?;
                        (
                            PointerTargets::known(
                                self.canonicalize_path_from_places(&bases, proj, state, origin)?,
                            ),
                            self.loans_for_place(state, place),
                        )
                    }
                    NEffectArgValue::Value(value) => (
                        PointerTargets::known(self.canonicalize_value_path(
                            state,
                            value.local,
                            proj,
                            origin,
                        )?),
                        self.loans_for_value(state, value.local),
                    ),
                }
            }
            MemorySummaryTarget::FreshAllocation {
                site,
                address_space,
            } => (
                PointerTargets::known(FxHashSet::from_iter([CanonPlace {
                    root: BorrowRoot::FreshAllocation {
                        site: FreshAllocSite::Call {
                            call: origin,
                            callee: match site {
                                FreshAllocSite::Direct(origin)
                                | FreshAllocSite::Call { call: origin, .. } => *origin,
                            },
                        },
                        address_space: *address_space,
                    },
                    proj: NSProjectionPath::default(),
                }])),
                FxHashSet::default(),
            ),
            MemorySummaryTarget::Unknown { address_spaces } => (
                PointerTargets::unknown(*address_spaces),
                FxHashSet::default(),
            ),
        })
    }

    fn callee_memory_summary(
        &self,
        callee: &SemanticCalleeRef<'db>,
    ) -> Result<MemorySummaryId<'db>, SemanticBorrowDiagnostic<'db>> {
        let callee_instance = get_or_build_semantic_instance(self.db, callee.key);
        match self.summary_mode.for_callee(self.db, callee_instance) {
            BorrowSummaryMode::Final => semantic_memory_summary_voucher(self.db, callee_instance),
            BorrowSummaryMode::Provisional => {
                provisional_memory_summary_voucher(self.db, callee_instance)
            }
        }
    }

    pub(super) fn resolve_call_memory_accesses(
        &self,
        state: &State<'db>,
        callee: &SemanticCalleeRef<'db>,
        args: &[super::ir::NOperand],
        effect_args: &[NEffectArg<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<Vec<ResolvedMemoryAccess<'db>>, SemanticBorrowDiagnostic<'db>> {
        let summary = self.callee_memory_summary(callee)?;
        let mut out = Vec::new();
        for item in summary.items(self.db) {
            let MemorySummaryItem::Access {
                target,
                kind,
                authorizers,
            } = item
            else {
                continue;
            };
            let (targets, loans) =
                self.resolve_summary_target(state, args, effect_args, target, origin)?;
            // Direct accesses are authorized only by mutable loans. Explicit
            // authorizers identify the receiver/capability through which an
            // otherwise unknown effect occurs, so their immutable loans must
            // also be excluded from self-conflict checks.
            let mut authorized = self.mut_loans(loans);
            let mut authorizer_targets = FxHashSet::default();
            for authorizer in authorizers {
                let (targets, loans) =
                    self.resolve_summary_target(state, args, effect_args, authorizer, origin)?;
                authorized.extend(loans);
                authorizer_targets.extend(targets.places());
            }
            out.push(ResolvedMemoryAccess {
                kind: *kind,
                targets: targets.places(),
                authorized,
                authorizer_targets,
            });
        }
        Ok(out)
    }

    fn apply_call_memory_summary(
        &self,
        state: &mut State<'db>,
        args: &[super::ir::NOperand],
        effect_args: &[NEffectArg<'db>],
        summary: MemorySummaryId<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<bool, SemanticBorrowDiagnostic<'db>> {
        let mut resolved = Vec::new();
        for item in summary.items(self.db) {
            let MemorySummaryItem::StorePointer {
                target,
                targets,
                weak,
            } = item
            else {
                continue;
            };
            let written = self
                .resolve_summary_target(state, args, effect_args, target, origin)?
                .0
                .places();
            let targets =
                self.resolve_summary_targets(state, args, effect_args, targets, origin)?;
            resolved.push((written, targets, *weak));
        }
        for (written, targets, weak) in resolved {
            let weak = weak || self.write_precision(&written) == WritePrecision::Weak;
            for place in written {
                state.update_pointer_targets(
                    self.pointer_slot_place(place.root, place.proj),
                    targets.clone(),
                    weak,
                );
            }
        }
        Ok(summary.may_return(self.db))
    }

    fn clear_pointer_targets_for_local(&self, state: &mut State<'db>, local: SLocalId) {
        let Some(local_data) = self.body.local(local) else {
            return;
        };
        for base in self.structural_local_places(local) {
            self.clear_physical_pointer_slots(state, &base, local_data.ty);
        }
    }

    fn copy_pointer_targets_from_value(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        src: SLocalId,
        dst_ty: TyId<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let dst_bases = self.canonicalize_value_base(state, dst);
        for slot in pointer_slots(self.db, dst_ty) {
            let targets = self.pointer_targets_for_value_path(state, src, &slot.path, origin)?;
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    targets.clone(),
                );
            }
        }
        Ok(())
    }

    fn copy_pointer_targets_from_place(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        src: &NSPlace<'db>,
        dst_ty: TyId<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let dst_bases = self.canonicalize_value_base(state, dst);
        let src_bases = self.canonicalize_place(state, src, origin)?;
        for slot in pointer_slots(self.db, dst_ty) {
            let keys = self.canonicalize_path_from_places(&src_bases, &slot.path, state, origin)?;
            let targets = self.pointer_targets_for_keys(state, keys, origin)?;
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    targets.clone(),
                );
            }
        }
        Ok(())
    }

    fn assign_array_repeat_pointer_targets(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        ty: TyId<'db>,
        value: super::ir::NOperand,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let dst_bases = self.canonicalize_value_base(state, dst);
        for slot in pointer_slots(self.db, ty) {
            let Some(suffix) = split_array_slot(&slot.path, false) else {
                continue;
            };
            let targets =
                self.pointer_targets_for_value_path(state, value.local, &suffix, origin)?;
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    targets.clone(),
                );
            }
        }
        Ok(())
    }

    fn assign_aggregate_pointer_targets(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        ty: TyId<'db>,
        fields: &[super::ir::NOperand],
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let dst_bases = self.canonicalize_value_base(state, dst);
        for slot in pointer_slots(self.db, ty) {
            let targets = if let Some((field_idx, suffix)) = split_aggregate_slot(&slot.path) {
                let Some(field) = fields.get(field_idx) else {
                    continue;
                };
                self.pointer_targets_for_value_path(state, field.local, &suffix, origin)?
            } else if let Some(suffix) = split_array_slot(&slot.path, true) {
                let mut targets = PointerTargets::default();
                for field in fields {
                    let field_targets =
                        self.pointer_targets_for_value_path(state, field.local, &suffix, origin)?;
                    targets.join_into(&field_targets);
                }
                targets
            } else {
                continue;
            };
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    targets.clone(),
                );
            }
        }
        Ok(())
    }

    fn assign_enum_pointer_targets(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        enum_ty: TyId<'db>,
        variant: VariantIndex,
        fields: &[super::ir::NOperand],
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let dst_bases = self.canonicalize_value_base(state, dst);
        for slot in pointer_slots(self.db, enum_ty) {
            let Some((field_idx, suffix)) = split_enum_slot(&slot.path, variant) else {
                continue;
            };
            let Some(field) = fields.get(field_idx) else {
                continue;
            };
            let targets =
                self.pointer_targets_for_value_path(state, field.local, &suffix, origin)?;
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    targets.clone(),
                );
            }
        }
        Ok(())
    }

    fn copy_pointer_targets_from_enum_field(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        value: super::ir::NOperand,
        variant: VariantIndex,
        field: FieldIndex,
        origin: SemOrigin<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let Some(enum_ty) = self.body.local(value.local).map(|local| local.ty) else {
            return Ok(());
        };
        let Some(dst_ty) = self.body.local(dst).map(|local| local.ty) else {
            return Ok(());
        };
        let field_path = NSProjectionPath::from_projection(Projection::VariantField {
            enum_ty,
            variant,
            field_idx: field.0 as usize,
        });
        let dst_bases = self.canonicalize_value_base(state, dst);
        for slot in pointer_slots(self.db, dst_ty) {
            let path = field_path.concat(&slot.path);
            let targets = self.pointer_targets_for_value_path(state, value.local, &path, origin)?;
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    targets.clone(),
                );
            }
        }
        Ok(())
    }

    fn assign_unknown_pointer_targets(
        &self,
        state: &mut State<'db>,
        dst: SLocalId,
        dst_ty: TyId<'db>,
    ) {
        let dst_bases = self.canonicalize_value_base(state, dst);
        for slot in pointer_slots(self.db, dst_ty) {
            for base in &dst_bases {
                state.assign_pointer_targets(
                    self.pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path)),
                    PointerTargets::unknown(PointerAddressSpaces::one(
                        ProviderAddressSpace::Memory,
                    )),
                );
            }
        }
    }

    pub(super) fn seed_param_pointer_targets(
        &self,
        state: &mut State<'db>,
        local: SLocalId,
        param_idx: u32,
    ) {
        let Some(local_data) = self.body.local(local) else {
            return;
        };
        let base = CanonPlace {
            root: BorrowRoot::Param(param_idx),
            proj: NSProjectionPath::default(),
        };
        self.seed_structural_pointer_targets(state, base, local_data.ty);
    }

    fn seed_structural_pointer_targets(
        &self,
        state: &mut State<'db>,
        base: CanonPlace<'db>,
        ty: TyId<'db>,
    ) {
        for slot in pointer_slots(self.db, ty) {
            let key = self.pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path));
            let target = CanonPlace {
                root: base.root.clone(),
                proj: base.proj.concat(&slot.path).concat(&slot.target_suffix),
            };
            state
                .assign_pointer_targets(key, PointerTargets::known(FxHashSet::from_iter([target])));
        }
    }

    pub(super) fn apply_stmt_state(
        &self,
        state: &mut State<'db>,
        stmt: &NSStmt<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => {
                let call_summary = if let NExpr::Call {
                    callee,
                    args,
                    effect_args,
                    ..
                } = expr
                {
                    let summary = self.callee_memory_summary(callee)?;
                    if !self.apply_call_memory_summary(
                        state,
                        args,
                        effect_args,
                        summary,
                        stmt.origin,
                    )? {
                        state.mark_unreachable();
                        return Ok(());
                    }
                    Some(summary)
                } else {
                    None
                };
                if self
                    .body
                    .local(*dst)
                    .is_some_and(|local| local.ty.as_borrow(self.db).is_none())
                {
                    self.clear_pointer_targets_for_local(state, *dst);
                }
                let loans = match expr {
                    NExpr::Use(src) => {
                        let loans = state.loans_in(src.local);
                        if loans.is_empty() {
                            self.loan_for_local
                                .get(dst)
                                .copied()
                                .map(|loan| FxHashSet::from_iter([loan]))
                                .unwrap_or_default()
                        } else {
                            loans
                        }
                    }
                    NExpr::Borrow { .. } | NExpr::Call { .. } => self
                        .loan_for_local
                        .get(dst)
                        .copied()
                        .map(|loan| FxHashSet::from_iter([loan]))
                        .unwrap_or_default(),
                    _ => FxHashSet::default(),
                };
                state.assign_loans(*dst, loans);
                self.assign_pointer_targets_for_expr(state, *dst, expr, call_summary, stmt.origin)
            }
            NSStmtKind::Store { dst, src } => {
                let Some(src_ty) = self.body.local(src.local).map(|local| local.ty) else {
                    return Ok(());
                };
                let written = self.canonicalize_place(state, dst, stmt.origin)?;
                let precision = self.write_precision(&written);
                if !is_pointer_bearing_type(self.db, src_ty) {
                    self.invalidate_scalar_store_pointer_targets(state, &written, precision);
                    return Ok(());
                }
                if precision == WritePrecision::Strong {
                    for place in &written {
                        self.clear_physical_pointer_slots(state, place, src_ty);
                    }
                }
                for slot in pointer_slots(self.db, src_ty) {
                    let targets = self.pointer_targets_for_value_path(
                        state,
                        src.local,
                        &slot.path,
                        stmt.origin,
                    )?;
                    for base in &written {
                        let key = self
                            .pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path));
                        state.update_pointer_targets(
                            key,
                            targets.clone(),
                            precision == WritePrecision::Weak,
                        );
                    }
                }
                Ok(())
            }
        }
    }

    fn invalidate_scalar_store_pointer_targets(
        &self,
        state: &mut State<'db>,
        written: &FxHashSet<CanonPlace<'db>>,
        precision: WritePrecision,
    ) {
        for place in written {
            let key = self.pointer_slot_place(place.root.clone(), place.proj.clone());
            state.invalidate_pointer_targets(
                key,
                PointerTargets::unknown(PointerAddressSpaces::one(ProviderAddressSpace::Memory)),
                precision == WritePrecision::Weak,
            );
        }
    }

    fn write_precision(&self, written: &FxHashSet<CanonPlace<'db>>) -> WritePrecision {
        if written.len() == 1
            && written.iter().all(|place| {
                self.pointer_slot_place(place.root.clone(), place.proj.clone())
                    .is_precise_slot()
            })
        {
            WritePrecision::Strong
        } else {
            WritePrecision::Weak
        }
    }

    fn clear_physical_pointer_slots(
        &self,
        state: &mut State<'db>,
        base: &CanonPlace<'db>,
        ty: TyId<'db>,
    ) {
        for slot in pointer_slots(self.db, ty) {
            let key = self.pointer_slot_place(base.root.clone(), base.proj.concat(&slot.path));
            if !key.may_name_multiple_slots() {
                state.clear_pointer_target(&key);
            }
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

fn split_aggregate_slot<'db>(
    path: &NSProjectionPath<'db>,
) -> Option<(usize, NSProjectionPath<'db>)> {
    let mut iter = path.iter();
    let field_idx = match iter.next()? {
        Projection::Field(field_idx) => *field_idx,
        Projection::Index(IndexSource::Constant(idx)) => *idx,
        _ => return None,
    };
    let mut suffix = NSProjectionPath::default();
    for projection in iter {
        suffix.push(projection.clone());
    }
    Some((field_idx, suffix))
}

fn split_array_slot<'db>(
    path: &NSProjectionPath<'db>,
    wildcard_only: bool,
) -> Option<NSProjectionPath<'db>> {
    let mut iter = path.iter();
    let is_array_slot = match iter.next()? {
        Projection::Index(IndexSource::Any) => true,
        Projection::Index(_) => !wildcard_only,
        _ => false,
    };
    if !is_array_slot {
        return None;
    }
    let mut suffix = NSProjectionPath::default();
    for projection in iter {
        suffix.push(projection.clone());
    }
    Some(suffix)
}

fn split_enum_slot<'db>(
    path: &NSProjectionPath<'db>,
    expected_variant: VariantIndex,
) -> Option<(usize, NSProjectionPath<'db>)> {
    let mut iter = path.iter();
    let field_idx = match iter.next()? {
        Projection::VariantField {
            variant, field_idx, ..
        } if *variant == expected_variant => *field_idx,
        _ => return None,
    };
    let mut suffix = NSProjectionPath::default();
    for projection in iter {
        suffix.push(projection.clone());
    }
    Some((field_idx, suffix))
}
