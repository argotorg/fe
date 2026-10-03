//! Typed external storage identities, including followed and widened referents.
use std::collections::{BTreeMap, BTreeSet};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::normalized::NRootId,
        ty::{
            ProviderAddressSpace,
            adt_def::instantiate_adt_field_shape,
            fold::TyFoldable,
            provider::{ProviderKind, ProviderLayoutEvidence},
            ty_def::{TyData, TyId},
        },
    },
    semantic::{ContractFieldId, LayoutViewKind, ProviderSource},
};

use super::{
    footprint::AccessExtent,
    guard::{Guard, ValueOccurrence},
    handle::{AddressOccurrence, HandleAddressSpace, OpaqueHandleRef},
    index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
    path::{RegionPath, aligned_index_pairs},
    region::{ProviderRegionId, RegionRoot},
    source::{InputSource, SourceExpr},
    value::{Guarded, IndexPayload},
};

/// What an alias comparison may treat as separation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AliasBasis {
    /// Also the entry assumption that distinct certain sources are separate,
    /// which call-boundary checks establish for the loans a body is given.
    Assumed,
    /// Only physical separation: address spaces, fresh objects against older
    /// ones, and field, element and range separation within one object.
    Physical,
}

/// Physical referent typing is independent of a capability's conversion views.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferentContract<'db> {
    pub ty: TyId<'db>,
    pub address_space: HandleAddressSpace<'db>,
    pub addressable: bool,
}

impl<'db> ReferentContract<'db> {
    pub fn new(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        address_space: HandleAddressSpace<'db>,
    ) -> Self {
        Self {
            ty,
            address_space,
            addressable: !ty.is_zero_sized(db),
        }
    }

    pub fn memory(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Self {
        Self::new(
            db,
            ty,
            HandleAddressSpace::Known(ProviderAddressSpace::Memory),
        )
    }

    pub fn substitute(self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        Self::new(
            db,
            self.ty.fold_with(db, &mut subst.clone()),
            self.address_space.substitute(db, subst),
        )
    }

    pub fn is_abstract(self, db: &'db dyn HirAnalysisDb) -> bool {
        let ty = self.ty.as_view(db).unwrap_or(self.ty);
        // Referents behind a pointer or native borrow have their own storage.
        // Their type parameters do not hide fields in this representation.
        if ty.as_ptr(db).is_some() || ty.as_borrow(db).is_some() {
            return false;
        }
        if matches!(
            ty.base_ty(db).data(db),
            TyData::TyParam(_) | TyData::AssocTy(_) | TyData::QualifiedTy(_)
        ) {
            return true;
        }
        let fields = if ty.is_array(db) {
            vec![ty.generic_args(db)[0]]
        } else if let Some(adt) = ty.adt_def(db) {
            adt.fields(db)
                .iter()
                .enumerate()
                .flat_map(|(variant, fields)| {
                    (0..fields.num_types()).map(move |field| {
                        instantiate_adt_field_shape(db, adt, variant, field, ty.generic_args(db))
                    })
                })
                .collect()
        } else {
            ty.field_types(db)
        };
        fields
            .into_iter()
            .any(|ty| Self { ty, ..self }.is_abstract(db))
    }

    pub fn may_alias(self, other: Self) -> bool {
        // Addresses survive casts even when their original pointee is empty.
        // Whether an access touches storage belongs to its current footprint.
        self.address_space.may_alias(other.address_space)
    }
}

/// Where a numeric address came from.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AddressProvenance {
    Raw,
    /// Exactly a `StorageMap` preimage hash (`keccak256(key_encoding ++ salt)`).
    /// Under the collision-resistance premise it never equals a
    /// compiler-allocated contract-field slot, which lies below 2^64. It grants
    /// no authority, validity, or initialization.
    HashedStorageSlot,
}

/// Whether a provider's target is the compiler-allocated storage of a
/// contract field, and of which field. This is a function of the interned
/// binding, cached where a database is available so that aliasing stays
/// database-free.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ProviderStorage<'db> {
    AllocatedField(ContractFieldId<'db>),
    Other,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExternalOrigin<'db> {
    /// A typed, conservatively reachable part of an abstract local value.
    Local(NRootId),
    Input(InputSource<'db>),
    Provider {
        provider: ProviderRegionId<'db>,
        target_ty: TyId<'db>,
        storage: ProviderStorage<'db>,
    },
    OpaqueHandle(OpaqueHandleRef<'db>),
    /// All addresses of this contract, without a distinguished object or loan.
    /// Arbitrary contents close over this finite family instead of naming a
    /// new cell for every pointer loaded from unknown bytes.
    OpaqueMemory,
    /// An allocator-created object. Its identity is distinct from every older input.
    Allocation(OpaqueHandleRef<'db>),
    /// An address admitted by an explicit unknown-call or intrinsic contract.
    /// Unlike a manufactured handle, it has no source-language carrier type.
    Unknown {
        contract: ReferentContract<'db>,
        occurrence: AddressOccurrence<'db>,
        arguments: Box<[IndexExpr<'db>]>,
        provenance: AddressProvenance,
    },
    /// A typed interpretation of raw memory at an element-scaled offset.
    /// This is a memory location, never a structural Index on a scalar type.
    Memory {
        base: Box<SourceExpr<'db>>,
        offset: MemoryOffset<'db>,
        target_ty: TyId<'db>,
    },
}

/// An unknown displacement retains its physical base, but names no definite cell.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MemoryOffset<'db> {
    Zero,
    Element(TyId<'db>, IndexExpr<'db>),
    Unknown,
}

impl<'db> MemoryOffset<'db> {
    pub fn index(self) -> Option<IndexExpr<'db>> {
        match self {
            Self::Zero => Some(IndexExpr::Const(0)),
            Self::Element(_, index) => Some(index),
            Self::Unknown => None,
        }
    }
}

/// A source names storage, not the representation of the handle used to reach it.
/// Its final type remains explicit after recursive widening.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExternalSource<'db> {
    pub origin: ExternalOrigin<'db>,
    pub contract: ReferentContract<'db>,
    /// This arbitrary replacement is present only when these locations overlap.
    /// The condition carries addresses, never replacement contents or authority.
    pub clobber: Option<Box<ClobberCondition<'db>>>,
    dereferences: Box<[RegionPath<IndexExpr<'db>>]>,
    reachable: bool,
    uncertain: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClobberCondition<'db> {
    pub target: SourceExpr<'db>,
    pub written: SourceExpr<'db>,
    pub extent: AccessExtent<'db>,
}

/// Checked read and write projections of one structural typed-cell relation.
pub struct StorageMatch<'db> {
    pub substitution: IndexSubst<'db>,
    pub guard: Guard<'db>,
    /// Family members reached as the same typed cell, in the family scope.
    /// Absent when another selection could overlap a member physically.
    pub typed: Option<Guard<'db>>,
    /// The definite-update embedding; a widened representation has none.
    pub write: Option<Guarded<'db, BTreeMap<IndexExpr<'db>, IndexExpr<'db>>>>,
}

/// Transport a definite write into family coordinates. Request binders map to
/// family positions; a repeated binder or a fixed selector constrains the
/// replaced members. Clobber-only binders must map one-to-one.
fn write_embedding<'db>(
    scope: &BinderScope,
    location: &[(IndexExpr<'db>, IndexExpr<'db>)],
    metadata: &[(IndexExpr<'db>, IndexExpr<'db>)],
) -> Option<Guarded<'db, BTreeMap<IndexExpr<'db>, IndexExpr<'db>>>> {
    let mut guard = Guard::always(scope);
    let mut bindings = BTreeMap::new();
    for &(formal, actual) in location {
        guard = match (actual, bindings.get(&actual)) {
            (IndexExpr::Bound(_), None) => {
                bindings.insert(actual, formal);
                guard
            }
            (IndexExpr::Bound(_), Some(&previous)) => guard.with_equality(previous, formal)?,
            _ => guard.with_equality(formal, actual)?,
        };
    }
    for &(formal, actual) in metadata {
        if matches!(actual, IndexExpr::Bound(_))
            && (!matches!(formal, IndexExpr::Bound(_))
                || *bindings.entry(actual).or_insert(formal) != formal)
        {
            return None;
        }
    }
    Some(Guarded {
        guard,
        payload: bindings,
    })
}

/// Places held in one structural slot before a loop feedback edge, with the
/// leaf-domain and clause guards under which each is held.
pub(super) type FeedbackPlaces<'db> = FxHashMap<SourceExpr<'db>, BTreeSet<Guard<'db>>>;

/// A prior slot that may be the current leaf's own, with the alignment of the
/// current (left) and prior (right) element selectors of the two slots.
pub(super) struct FeedbackSlot<'a, 'db> {
    pub pairs: Vec<(IndexExpr<'db>, IndexExpr<'db>)>,
    pub places: &'a FeedbackPlaces<'db>,
}

/// The facts a loop feedback edge forgets: choices and selectors that a new
/// execution may take differently. Facts fixed before the loop, such as a
/// choice or runtime value computed outside it, still separate clauses.
#[derive(Clone, Copy)]
pub(super) struct FeedbackRepeats<'a, 'db> {
    pub index: &'a dyn Fn(IndexExpr<'db>) -> bool,
    pub occurrence: &'a dyn Fn(ValueOccurrence) -> bool,
}

/// Only facts that hold in every execution separate a recomputed base from a
/// derived one. Forgetting the repeated ones can only admit more.
fn feedback_guard<'db>(guard: &Guard<'db>, repeats: FeedbackRepeats<'_, 'db>) -> Guard<'db> {
    guard
        .forget_occurrences(|occurrence| (repeats.occurrence)(occurrence))
        .forget_indices(|index| (repeats.index)(index))
}

/// A clause guard restricted to the domain of its structural leaf, such as
/// the disequalities excluding exact members from an array default. `None`
/// if the clause cannot hold there. A domain outside the clause's scope
/// adds nothing, which can only admit more.
pub(super) fn feedback_clause_guard<'db>(
    clause: &Guard<'db>,
    domain: &Guard<'db>,
    repeats: FeedbackRepeats<'_, 'db>,
) -> Option<Guard<'db>> {
    let guard = if clause
        .scope()
        .existential_extension_of(domain.scope())
        .is_some()
    {
        clause.and(&domain.in_scope(clause.scope()))?
    } else {
        clause.clone()
    };
    Some(feedback_guard(&guard, repeats))
}

/// Whether aligned selectors of the current clause (left) and an ancestor's
/// clause (right) can be equal under both guards, with the ancestor's
/// binders renamed apart. A selector bound outside its guard's scope
/// constrains nothing.
fn selectors_may_agree<'db>(
    current: &Guard<'db>,
    previous: &Guard<'db>,
    pairs: impl IntoIterator<Item = (IndexExpr<'db>, IndexExpr<'db>)>,
) -> bool {
    let fresh = previous.scope().freshening(current.scope());
    let pairs: Vec<_> = pairs
        .into_iter()
        .filter(|(left, right)| {
            current.scope().validate(*left).is_ok() && previous.scope().validate(*right).is_ok()
        })
        .map(|(left, right)| (left, fresh.apply(right)))
        .collect();
    let Some(previous) = previous.substitute(&fresh) else {
        return false;
    };
    current
        .in_scope(previous.scope())
        .and(&previous)
        .and_then(|guard| guard.with_equalities(pairs))
        .is_some()
}

/// Whether a prior place is itself an offset or load chain over an earlier
/// value of the same prior slot and element, under selectors that can agree.
/// Growth passes through its own slot again; a sibling slot holding the base
/// of a fixed offset is no evidence of growth.
fn derived_from_prior<'db>(
    place: &SourceExpr<'db>,
    guards: &BTreeSet<Guard<'db>>,
    slot: &FeedbackSlot<'_, 'db>,
) -> bool {
    // Both places are held in `slot`: equate their element selectors.
    let elements: Vec<_> = slot
        .pairs
        .iter()
        .map(|(_, prior)| (*prior, *prior))
        .collect();
    let mut pairs = Vec::new();
    let mut source = &place.source;
    while let ExternalOrigin::Memory { base, .. } = &source.origin {
        if slot.places.iter().any(|(ancestor, ancestor_guards)| {
            pairs.clear();
            place_correspondence(base, ancestor, &mut pairs).is_some()
                && guards.iter().any(|guard| {
                    ancestor_guards.iter().any(|ancestor| {
                        selectors_may_agree(guard, ancestor, pairs.iter().chain(&elements).copied())
                    })
                })
        }) {
            return true;
        }
        source = &base.source;
    }
    false
}

/// Whether aligned index pairs rename existential binders one-to-one and
/// agree on every other index: each binder maps to one binder, and no two
/// binders map to the same one.
pub(super) fn is_existential_renaming<'db>(pairs: &[(IndexExpr<'db>, IndexExpr<'db>)]) -> bool {
    let existential =
        |index: &IndexExpr<'db>| index.bound_namespace() == Some(IndexNamespace::Existential);
    let mut forward = FxHashMap::default();
    let mut backward = FxHashMap::default();
    pairs.iter().all(
        |(left, right)| match (existential(left), existential(right)) {
            (true, true) => {
                *forward.entry(*left).or_insert(*right) == *right
                    && *backward.entry(*right).or_insert(*left) == *left
            }
            (false, false) => left == right,
            _ => false,
        },
    )
}

/// Align a current base with a prior place: the same storage expression up
/// to its selectors, including referent paths and views.
fn place_correspondence<'db>(
    base: &SourceExpr<'db>,
    previous: &SourceExpr<'db>,
    pairs: &mut Vec<(IndexExpr<'db>, IndexExpr<'db>)>,
) -> Option<()> {
    if base.views != previous.views {
        return None;
    }
    base.source.correspondence(&previous.source, pairs)?;
    aligned_index_pairs(base.path.as_slice(), previous.path.as_slice(), pairs)
}

impl<'db> ClobberCondition<'db> {
    pub fn new(
        mut target: SourceExpr<'db>,
        mut written: SourceExpr<'db>,
        extent: AccessExtent<'db>,
    ) -> Self {
        // A write through an earlier arbitrary replacement can happen only if
        // that replacement's corruption condition held. Keep that prerequisite
        // rather than making a loop's later writes unconditionally arbitrary.
        // Dropping the additional overlap test is conservative and keeps the
        // condition depth bounded across repeated writes and summary calls.
        if let Some(condition) = written.source.clobber_dependency() {
            return condition.clone();
        }
        target.source.erase_clobber_conditions();
        written.source.erase_clobber_conditions();
        // Bound raw write selectors by their containing object, but retain
        // the clobbered cell: a callee's separation precondition may protect
        // that cell without protecting every field in its containing object.
        // If the written source is unchanged, its extent stays meaningful too.
        // A widened written source loses its original offset and extent.
        let object = |place: &SourceExpr<'db>| {
            let mut place = place.clone();
            while place.source.dereferences.is_empty()
                && !place.source.reachable
                && let ExternalOrigin::Memory { base, .. } = &place.source.origin
            {
                place = (**base).clone();
            }
            place.path = RegionPath::default();
            place.views = Default::default();
            place
        };
        let (target_object, written_object) = (object(&target), object(&written));
        if written_object.source.in_raw_memory() && !target_object.source.in_raw_memory() {
            return Self {
                target,
                extent: if written_object == written {
                    extent
                } else {
                    AccessExtent::Unknown
                },
                written: written_object,
            };
        }
        Self {
            target,
            written,
            extent,
        }
    }
}

impl<'db> ExternalSource<'db> {
    pub fn opaque_memory(contract: ReferentContract<'db>) -> Self {
        Self {
            origin: ExternalOrigin::OpaqueMemory,
            contract,
            clobber: None,
            dereferences: Box::new([]),
            reachable: true,
            uncertain: true,
        }
    }

    /// Reading or corrupting arbitrary bytes cannot refine their identity.
    pub fn is_arbitrary(&self) -> bool {
        self.clobber.is_some()
            || match &self.origin {
                ExternalOrigin::OpaqueMemory => true,
                ExternalOrigin::OpaqueHandle(handle) => {
                    matches!(handle.occurrence, AddressOccurrence::Overwrite(_))
                }
                ExternalOrigin::Memory { base, .. } => base.source.is_arbitrary(),
                _ => false,
            }
    }

    pub fn unknown(
        contract: ReferentContract<'db>,
        occurrence: AddressOccurrence<'db>,
        arguments: Box<[IndexExpr<'db>]>,
        provenance: AddressProvenance,
    ) -> Self {
        Self {
            origin: ExternalOrigin::Unknown {
                contract,
                occurrence,
                arguments,
                provenance,
            },
            contract,
            clobber: None,
            dereferences: Box::new([]),
            reachable: false,
            uncertain: true,
        }
    }

    /// The original address before following stored capabilities or widening.
    pub fn address_base(&self, db: &'db dyn HirAnalysisDb) -> Option<Self> {
        match &self.origin {
            ExternalOrigin::OpaqueHandle(handle) => Some(Self::opaque(db, handle.clone())),
            ExternalOrigin::Allocation(handle) => Some(Self::allocation(db, handle.clone())),
            ExternalOrigin::Unknown {
                contract,
                occurrence,
                arguments,
                provenance,
            } => Some(Self::unknown(
                *contract,
                *occurrence,
                arguments.clone(),
                *provenance,
            )),
            _ => None,
        }
    }

    /// The corruption condition under which this address, or the base it
    /// is offset from, exists.
    fn clobber_dependency(&self) -> Option<&ClobberCondition<'db>> {
        let mut dependency = self;
        loop {
            if let Some(condition) = &dependency.clobber {
                return Some(condition);
            }
            let ExternalOrigin::Memory { base, .. } = &dependency.origin else {
                return None;
            };
            dependency = &base.source;
        }
    }

    pub(super) fn has_clobber_dependency(&self) -> bool {
        self.clobber_dependency().is_some()
    }

    fn erase_clobber_conditions(&mut self) {
        self.clobber = None;
        if let ExternalOrigin::Memory { base, .. } = &mut self.origin {
            base.source.erase_clobber_conditions();
        }
    }

    pub fn abstract_target(root: &RegionRoot<'db>, contract: ReferentContract<'db>) -> Self {
        let mut source = match root {
            RegionRoot::External(source) => source.clone().widen(),
            RegionRoot::Root { root, .. } => Self {
                origin: ExternalOrigin::Local(*root),
                contract,
                clobber: None,
                dereferences: Box::new([]),
                reachable: true,
                uncertain: true,
            },
            RegionRoot::Value(_) => unreachable!("an abstract referent has storage"),
        };
        source.contract = contract;
        source
    }

    pub fn input(
        source: InputSource<'db>,
        contract: ReferentContract<'db>,
        uncertain: bool,
    ) -> Self {
        let reachable = source.is_reachable();
        Self {
            origin: ExternalOrigin::Input(source),
            contract,
            clobber: None,
            dereferences: Box::new([]),
            reachable,
            uncertain,
        }
    }

    pub fn provider(
        db: &'db dyn HirAnalysisDb,
        provider: ProviderRegionId<'db>,
        target_ty: TyId<'db>,
    ) -> Self {
        let binding = provider.binding(db);
        let space = binding.semantics.address_space.map_or_else(
            || {
                if binding.semantics.kind == ProviderKind::RootObject {
                    HandleAddressSpace::Known(ProviderAddressSpace::Memory)
                } else {
                    HandleAddressSpace::Unspecified
                }
            },
            HandleAddressSpace::Known,
        );
        // Only a contract field's own persistent target, placed at the slots its
        // layout allocated, qualifies. Allocated slots are checked `usize`
        // offsets; the explicit bound keeps the hash premise independent of the
        // host.
        let storage = if space == HandleAddressSpace::Known(ProviderAddressSpace::Storage)
            && binding.semantics.evidence == ProviderLayoutEvidence::ContractField
            && let ProviderSource::ContractField { field } = &binding.source
            && binding
                .layout_env
                .is_some_and(|env| env.field == *field && env.view == LayoutViewKind::Target)
            && binding
                .assigned_field_layout(db)
                .is_some_and(|(layout, _)| {
                    layout
                        .slot_offset
                        .checked_add(layout.slot_count)
                        .is_some_and(|end| u64::try_from(end).is_ok())
                }) {
            ProviderStorage::AllocatedField(*field)
        } else {
            ProviderStorage::Other
        };
        Self {
            origin: ExternalOrigin::Provider {
                provider,
                target_ty,
                storage,
            },
            clobber: None,
            contract: ReferentContract::new(db, target_ty, space),
            dereferences: Box::new([]),
            reachable: false,
            uncertain: false,
        }
    }

    pub fn opaque(db: &'db dyn HirAnalysisDb, handle: OpaqueHandleRef<'db>) -> Self {
        Self {
            contract: ReferentContract::new(
                db,
                handle.contract.target_ty,
                handle.contract.address_space,
            ),
            origin: ExternalOrigin::OpaqueHandle(handle),
            clobber: None,
            dereferences: Box::new([]),
            reachable: false,
            uncertain: true,
        }
    }

    pub fn allocation(db: &'db dyn HirAnalysisDb, allocation: OpaqueHandleRef<'db>) -> Self {
        let mut source = Self::opaque(db, allocation.clone());
        source.origin = ExternalOrigin::Allocation(allocation);
        source.uncertain = false;
        source
    }

    pub fn memory(
        db: &'db dyn HirAnalysisDb,
        mut base: SourceExpr<'db>,
        target_ty: TyId<'db>,
        mut offset: MemoryOffset<'db>,
    ) -> Self {
        if matches!(base.source.origin, ExternalOrigin::OpaqueMemory) {
            // An offset into a conditional replacement exists under its condition.
            let mut source = Self::opaque_memory(ReferentContract::new(
                db,
                target_ty,
                base.source.contract.address_space,
            ));
            source.clobber = base.source.clobber;
            return source;
        }
        if matches!(offset, MemoryOffset::Element(_, IndexExpr::Const(0))) {
            offset = MemoryOffset::Zero;
        }
        // Casts and unknown displacements retain one physical base. An offset
        // of an unknown offset stays unknown, without following stored pointers.
        while base.path.is_empty()
            && base.views.iter().next().is_none()
            && base.source.dereferences.is_empty()
            && !base.source.reachable
            && let ExternalOrigin::Memory {
                base: original,
                offset: old,
                ..
            } = &base.source.origin
            && (offset == MemoryOffset::Zero
                || *old == MemoryOffset::Zero
                || offset == MemoryOffset::Unknown
                || *old == MemoryOffset::Unknown)
        {
            offset = if offset == MemoryOffset::Unknown || *old == MemoryOffset::Unknown {
                MemoryOffset::Unknown
            } else if offset == MemoryOffset::Zero {
                *old
            } else {
                offset
            };
            base = *original.clone();
        }
        if offset == MemoryOffset::Zero
            && base.path.is_empty()
            && base.views.iter().next().is_none()
            && base.source.contract.ty == target_ty
        {
            return base.source;
        }
        Self {
            contract: ReferentContract::new(db, target_ty, base.source.contract.address_space),
            uncertain: base.source.uncertain() || offset == MemoryOffset::Unknown,
            origin: ExternalOrigin::Memory {
                base: Box::new(base),
                offset,
                target_ty,
            },
            clobber: None,
            dereferences: Box::new([]),
            reachable: false,
        }
    }

    /// Storage reached through a raw address. The referent of a native borrow
    /// or view parameter is memory too, but its caller passes a native place
    /// whose overlap with raw writes it can usually refute, so it is not raw.
    pub(super) fn in_raw_memory(&self) -> bool {
        self.contract.address_space == HandleAddressSpace::Known(ProviderAddressSpace::Memory)
            && !(matches!(self.origin, ExternalOrigin::Input(_)) && !self.uncertain())
    }

    /// A conditional replacement of a pointer stored in raw memory, or an
    /// offset into one. Its overlap condition lets a caller refute the
    /// overwrite, but every loop iteration would add replacements of its own.
    pub(super) fn is_raw_memory_replacement(&self) -> bool {
        match &self.clobber {
            Some(clobber) => clobber.target.source.in_raw_memory(),
            None => matches!(&self.origin, ExternalOrigin::Memory { base, .. }
                if self.dereferences.is_empty() && base.source.is_raw_memory_replacement()),
        }
    }

    /// Whether `self`, the replacement root of `offset`, was held before the
    /// loop. The root itself must be one exactly. An offset embeds its root in
    /// its own clause, which renumbers the root's clause-local witnesses, so
    /// there a pure renaming of existential binders suffices.
    fn is_invariant_replacement(&self, offset: &Self, invariant: &FxHashSet<Self>) -> bool {
        if invariant.contains(self) {
            return true;
        }
        if std::ptr::eq(self, offset) {
            return false;
        }
        let mut pairs = Vec::new();
        invariant.iter().any(|replacement| {
            pairs.clear();
            self.correspondence(replacement, &mut pairs).is_some()
                && self
                    .metadata_correspondence(replacement, &mut pairs)
                    .is_some()
                && is_existential_renaming(&pairs)
        })
    }

    /// The conditional replacement an offset or cast of one is based on.
    pub(super) fn replacement_root(&self) -> Option<&Self> {
        let mut source = self;
        loop {
            if source.clobber.is_some() {
                return Some(source);
            }
            match &source.origin {
                ExternalOrigin::Memory { base, .. } if source.dereferences.is_empty() => {
                    source = &base.source;
                }
                _ => return None,
            }
        }
    }

    fn has_unknown_offset(&self) -> bool {
        matches!(&self.origin, ExternalOrigin::Memory { base, offset, .. }
            if *offset == MemoryOffset::Unknown || base.source.has_unknown_offset())
    }

    /// A family cannot justify an exact alias or a definite storage update.
    pub fn is_widened(&self) -> bool {
        self.reachable
            || matches!(&self.origin, ExternalOrigin::Memory { base, offset, .. }
                if *offset == MemoryOffset::Unknown || base.source.is_widened())
    }

    /// Only feedback that adds provenance needs widening. An invariant source
    /// retains its exact offset and caller-dischargeable overwrite condition.
    /// `place` is the current clause's place rooted at `self` and `guard` its
    /// feedback guard; `previous` holds the prior slots that may be its own.
    pub(super) fn widen_feedback(
        &self,
        db: &'db dyn HirAnalysisDb,
        place: &SourceExpr<'db>,
        guard: &Guard<'db>,
        previous: &[FeedbackSlot<'_, 'db>],
        invariant_replacements: &FxHashSet<Self>,
    ) -> Option<Self> {
        if previous.iter().any(|slot| slot.places.contains_key(place))
            || invariant_replacements.contains(self)
        {
            return None;
        }
        // A replacement created in the loop would add alternatives on every
        // iteration. One held before the loop is invariant, and so is its
        // condition; an offset of it is widened only if it grows.
        if self.is_raw_memory_replacement()
            && !self
                .replacement_root()
                .is_some_and(|root| root.is_invariant_replacement(self, invariant_replacements))
        {
            return Some(Self::opaque_memory(self.contract));
        }
        let mut source = self;
        let mut followed = false;
        let mut pairs = Vec::new();
        while let ExternalOrigin::Memory { base, .. } = &source.origin {
            followed |= !source.dereferences.is_empty() || source.reachable;
            // Forgetting an iteration renumbers existential selectors. Match
            // the ancestor's structure, not its binder numbers, to recognize
            // growth. This proves no equality: the widened result retains the
            // current base and its arguments, and only forgets displacement.
            // A base whose aligned selectors contradict every guard of the
            // ancestor, such as `offset(base, 2)` against `offset(base, 1)`,
            // or whose slot cannot be the ancestor's, is recomputed rather
            // than derived from it.
            // An ancestor that is not itself derived from another prior place
            // may be an invariant base the loop recomputes from, as in
            // `p = offset(offset(base, 32), 32)` after `p = offset(base, 32)`.
            // Growth nests prior values in each other, so it is widened one
            // feedback later, when its ancestor is derived as well.
            if previous.iter().any(|slot| {
                slot.places.iter().any(|(previous_place, guards)| {
                    pairs.clear();
                    place_correspondence(base, previous_place, &mut pairs).is_some()
                        && guards.iter().any(|previous| {
                            selectors_may_agree(
                                guard,
                                previous,
                                pairs.iter().chain(&slot.pairs).copied(),
                            )
                        })
                        && derived_from_prior(previous_place, guards, slot)
                })
            }) {
                // Offsets stay within the same physical allocation. Loading a
                // pointer may instead reach another object, so retain no such
                // allocation guarantee for a growing offset/load chain.
                return Some(if followed {
                    Self::opaque_memory(self.contract)
                } else {
                    Self::memory(
                        db,
                        SourceExpr::whole(self.clone()),
                        self.contract.ty,
                        MemoryOffset::Unknown,
                    )
                });
            }
            source = &base.source;
        }
        None
    }

    /// Rewrites construction occurrences through every nested memory base.
    pub fn map_occurrences(
        &mut self,
        f: &mut impl FnMut(&mut AddressOccurrence<'db>, &mut Box<[IndexExpr<'db>]>),
    ) {
        if let Some(clobber) = &mut self.clobber {
            clobber.target.source.map_occurrences(f);
            clobber.written.source.map_occurrences(f);
        }
        match &mut self.origin {
            ExternalOrigin::OpaqueHandle(source) | ExternalOrigin::Allocation(source) => {
                f(&mut source.occurrence, &mut source.arguments)
            }
            ExternalOrigin::Unknown {
                occurrence,
                arguments,
                ..
            } => f(occurrence, arguments),
            ExternalOrigin::Memory { base, .. } => base.source.map_occurrences(f),
            _ => {}
        }
    }

    pub fn follow(
        &self,
        path: RegionPath<IndexExpr<'db>>,
        contract: ReferentContract<'db>,
        uncertain: bool,
    ) -> Self {
        let mut source = self.clone();
        source.contract = contract;
        source.uncertain |= uncertain;
        if source.reachable || source.dereferences.len() >= InputSource::MAX_DEREFERENCES {
            return source.widen();
        }
        let mut dereferences = source.dereferences.to_vec();
        dereferences.push(path);
        source.dereferences = dereferences.into();
        source
    }

    pub fn widen(mut self) -> Self {
        if let ExternalOrigin::Input(input) = &self.origin {
            self.origin = ExternalOrigin::Input(InputSource::reachable(input.param()));
        }
        self.dereferences = Box::new([]);
        self.reachable = true;
        self.uncertain = true;
        self
    }

    /// Direct bytes in an allocation created by this invocation cannot alias
    /// any caller loan. Following a pointer stored there loses that guarantee.
    /// Raw accesses must stay within the allocation's complete valid extent;
    /// this provenance query does not prove bounds for a cast or offset.
    pub fn is_fresh_allocation(&self) -> bool {
        self.fresh_allocation().is_some()
    }

    pub fn fresh_allocation(&self) -> Option<&OpaqueHandleRef<'db>> {
        if self.is_reachable() || !self.dereferences.is_empty() {
            return None;
        }
        match &self.origin {
            ExternalOrigin::Allocation(handle) => Some(handle),
            ExternalOrigin::Memory { base, .. } => base.source.fresh_allocation(),
            _ => None,
        }
    }

    pub fn is_reachable(&self) -> bool {
        self.reachable
    }
    pub fn uncertain(&self) -> bool {
        self.uncertain || self.reachable
    }
    pub fn dereferences(&self) -> &[RegionPath<IndexExpr<'db>>] {
        &self.dereferences
    }
    /// Entry pointers cannot refer to storage first created in the callee's
    /// frame. Once overwritten, their explicit replacement regions take over.
    pub fn is_incoming(&self) -> bool {
        match &self.origin {
            ExternalOrigin::Input(_) => true,
            ExternalOrigin::Memory { base, .. } => base.source.is_incoming(),
            _ => false,
        }
    }

    /// How many nodes this source stores: itself, its dereference steps and
    /// index arguments, and every Memory base and clobber dependency with their
    /// projection steps and conversion views.
    pub fn size(&self) -> usize {
        let origin = match &self.origin {
            ExternalOrigin::Input(input) => input.size(),
            ExternalOrigin::OpaqueHandle(handle) | ExternalOrigin::Allocation(handle) => {
                handle.arguments.len()
            }
            ExternalOrigin::Unknown { arguments, .. } => arguments.len(),
            ExternalOrigin::Memory { base, offset, .. } => {
                base.size() + usize::from(*offset != MemoryOffset::Zero)
            }
            ExternalOrigin::Provider { .. }
            | ExternalOrigin::Local(_)
            | ExternalOrigin::OpaqueMemory => 0,
        };
        let clobber = self.clobber.as_ref().map_or(0, |clobber| {
            clobber.target.size() + clobber.written.size() + clobber.extent.indices().count()
        });
        let dereferences = self
            .dereferences
            .iter()
            .map(|path| 1 + path.as_slice().len())
            .sum::<usize>();
        1 + origin + clobber + dereferences
    }

    /// Whether any part of this route, including Memory bases and clobber
    /// dependencies, names storage local to the analyzed body.
    pub fn names_local_storage(&self) -> bool {
        matches!(self.origin, ExternalOrigin::Local(_))
            || matches!(&self.origin, ExternalOrigin::Memory { base, .. } if base.source.names_local_storage())
            || self.clobber.as_ref().is_some_and(|clobber| {
                clobber.target.source.names_local_storage()
                    || clobber.written.source.names_local_storage()
            })
    }

    pub fn param(&self) -> Option<u32> {
        match &self.origin {
            ExternalOrigin::Input(input) => Some(input.param()),
            ExternalOrigin::Memory { base, .. } => base.source.param(),
            _ => None,
        }
    }

    pub fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>> + '_ {
        let mut indices: Vec<_> = match &self.origin {
            ExternalOrigin::Input(input) => input.indices().collect(),
            ExternalOrigin::OpaqueHandle(handle) | ExternalOrigin::Allocation(handle) => {
                handle.arguments.to_vec()
            }
            ExternalOrigin::Unknown { arguments, .. } => arguments.to_vec(),
            ExternalOrigin::Memory { base, offset, .. } => {
                let index = match offset {
                    MemoryOffset::Element(_, index) => Some(*index),
                    _ => None,
                };
                base.indices().chain(index).collect()
            }
            ExternalOrigin::Provider { .. }
            | ExternalOrigin::Local(_)
            | ExternalOrigin::OpaqueMemory => Vec::new(),
        };
        indices.extend(self.clobber.iter().flat_map(|clobber| {
            clobber
                .target
                .indices()
                .chain(clobber.written.indices())
                .chain(clobber.extent.indices())
        }));
        indices
            .into_iter()
            .chain(self.dereferences.iter().flat_map(RegionPath::indices))
    }

    pub fn substitute(&self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        let mut result = self.rename_indices(subst);
        result.contract = self.contract.substitute(db, subst);
        result.clobber = self.clobber.as_ref().map(|clobber| {
            Box::new(ClobberCondition {
                target: clobber.target.substitute(db, subst),
                written: clobber.written.substitute(db, subst),
                extent: clobber.extent.substitute(subst),
            })
        });
        match &self.origin {
            ExternalOrigin::Unknown {
                contract,
                occurrence,
                arguments,
                provenance,
            } => {
                result.origin = ExternalOrigin::Unknown {
                    contract: contract.substitute(db, subst),
                    occurrence: *occurrence,
                    arguments: arguments.iter().map(|index| subst.apply(*index)).collect(),
                    provenance: *provenance,
                };
            }
            ExternalOrigin::OpaqueHandle(handle) => {
                result.origin = ExternalOrigin::OpaqueHandle(handle.substitute(db, subst))
            }
            // The interned binding, and so its storage classification, is unchanged.
            ExternalOrigin::Provider {
                provider,
                target_ty,
                storage,
            } => {
                result.origin = ExternalOrigin::Provider {
                    provider: *provider,
                    target_ty: target_ty.fold_with(db, &mut subst.clone()),
                    storage: *storage,
                }
            }
            ExternalOrigin::Allocation(handle) => {
                result.origin = ExternalOrigin::Allocation(handle.substitute(db, subst));
            }
            ExternalOrigin::Memory {
                base,
                offset,
                target_ty,
            } => {
                result.origin = ExternalOrigin::Memory {
                    target_ty: target_ty.fold_with(db, &mut subst.clone()),
                    base: Box::new(base.substitute(db, subst)),
                    offset: match *offset {
                        MemoryOffset::Element(ty, index) => MemoryOffset::Element(
                            ty.fold_with(db, &mut subst.clone()),
                            subst.apply(index),
                        ),
                        offset => offset,
                    },
                };
            }
            ExternalOrigin::Input(_) | ExternalOrigin::Local(_) | ExternalOrigin::OpaqueMemory => {}
        }
        result
    }

    pub(super) fn rename_indices(&self, subst: &IndexSubst<'db>) -> Self {
        let origin = match &self.origin {
            ExternalOrigin::Unknown {
                contract,
                occurrence,
                arguments,
                provenance,
            } => ExternalOrigin::Unknown {
                contract: *contract,
                occurrence: *occurrence,
                arguments: arguments.iter().map(|index| subst.apply(*index)).collect(),
                provenance: *provenance,
            },
            ExternalOrigin::Local(root) => ExternalOrigin::Local(*root),
            ExternalOrigin::OpaqueMemory => ExternalOrigin::OpaqueMemory,
            ExternalOrigin::Input(input) => ExternalOrigin::Input(input.substitute(subst)),
            ExternalOrigin::Provider { .. } => self.origin.clone(),
            ExternalOrigin::Memory {
                base,
                offset,
                target_ty,
            } => ExternalOrigin::Memory {
                target_ty: *target_ty,
                base: Box::new(SourceExpr {
                    invalidated: base.invalidated,
                    source: base.source.rename_indices(subst),
                    path: base.path.substitute(subst),
                    views: base.views.clone(),
                }),
                offset: match *offset {
                    MemoryOffset::Element(ty, index) => {
                        MemoryOffset::Element(ty, subst.apply(index))
                    }
                    offset => offset,
                },
            },
            ExternalOrigin::Allocation(handle) => ExternalOrigin::Allocation(OpaqueHandleRef {
                arguments: handle
                    .arguments
                    .iter()
                    .map(|index| subst.apply(*index))
                    .collect(),
                ..handle.clone()
            }),
            ExternalOrigin::OpaqueHandle(handle) => ExternalOrigin::OpaqueHandle(OpaqueHandleRef {
                arguments: handle
                    .arguments
                    .iter()
                    .map(|index| subst.apply(*index))
                    .collect(),
                ..handle.clone()
            }),
        };
        Self {
            origin,
            contract: self.contract,
            clobber: self.clobber.as_ref().map(|clobber| {
                let rename = |source: &SourceExpr<'db>| SourceExpr {
                    source: source.source.rename_indices(subst),
                    path: source.path.substitute(subst),
                    ..source.clone()
                };
                Box::new(ClobberCondition {
                    target: rename(&clobber.target),
                    written: rename(&clobber.written),
                    extent: clobber.extent.substitute(subst),
                })
            }),
            dereferences: self
                .dereferences
                .iter()
                .map(|path| path.substitute(subst))
                .collect(),
            reachable: self.reachable,
            uncertain: self.uncertain,
        }
    }

    /// Storage-family matching is distinct from a semantic coverage proof.
    /// One structural correspondence supplies the read substitution, its
    /// identity guard, and the optional write embedding. A widened typed cell
    /// can be read, but never supports a strong update.
    pub fn match_instance(
        &self,
        scope: &BinderScope,
        instance: &Self,
        instance_scope: &BinderScope,
    ) -> Option<StorageMatch<'db>> {
        let mut location = Vec::new();
        self.correspondence(instance, &mut location)?;
        let mut metadata = Vec::new();
        self.metadata_correspondence(instance, &mut metadata)?;
        let mut bindings = BTreeMap::new();
        for &(formal, actual) in location.iter().chain(&metadata) {
            scope.validate(formal).ok()?;
            instance_scope.validate(actual).ok()?;
            if matches!(formal, IndexExpr::Bound(_)) {
                bindings.entry(formal).or_insert(actual);
            }
        }
        let substitution = IndexSubst::new(scope, instance_scope, bindings).ok()?;
        let guard = Guard::always(instance_scope).with_equalities(
            location
                .iter()
                .map(|&(formal, actual)| (substitution.apply(formal), actual)),
        )?;
        let embedding = write_embedding(scope, &location, &metadata);
        // Unequal selections of an uncertain object or of a loaded pointer can
        // overlap a member at another byte offset.
        let typed = embedding
            .as_ref()
            .filter(|_| {
                !matches!(self.origin, ExternalOrigin::OpaqueMemory)
                    && !self.has_unknown_offset()
                    && !instance
                        .overlapping_object_indices()
                        .iter()
                        .any(|index| matches!(index, IndexExpr::Bound(_)))
            })
            .map(|embedding| embedding.guard.clone());
        let write = embedding.filter(|_| !self.is_widened() && !instance.is_widened());
        Some(StorageMatch {
            substitution,
            guard,
            typed,
            write,
        })
    }

    /// Indices choosing an object that another choice may overlap at an
    /// unknown offset: uncertain handle arguments and loaded pointers.
    fn overlapping_object_indices(&self) -> Vec<IndexExpr<'db>> {
        let mut indices = match &self.origin {
            ExternalOrigin::Unknown { arguments, .. } => arguments.to_vec(),
            ExternalOrigin::OpaqueHandle(handle) => handle.arguments.to_vec(),
            ExternalOrigin::Input(input) => input
                .dereferences()
                .iter()
                .flat_map(|path| path.indices())
                .collect(),
            ExternalOrigin::Memory { base, .. } => base.source.overlapping_object_indices(),
            _ => Vec::new(),
        };
        indices.extend(self.dereferences.iter().flat_map(|path| path.indices()));
        indices
    }

    fn zero_wrapper_base(&self) -> Option<(&SourceExpr<'db>, IndexExpr<'db>)> {
        let ExternalOrigin::Memory {
            base,
            offset,
            target_ty,
        } = &self.origin
        else {
            return None;
        };
        (self.dereferences.is_empty()
            && !self.reachable
            && base.path.is_empty()
            && base.views.iter().next().is_none()
            && base.source.dereferences.is_empty()
            && !base.source.reachable
            && base.source.contract.ty == *target_ty)
            .then_some((base.as_ref(), offset.index()?))
    }

    /// Align the index roles of two structurally identical typed locations.
    /// A same-type zero-offset
    /// wrapper corresponds to its base. Exact identity is the conjunction of
    /// the aligned index equalities.
    fn correspondence(
        &self,
        other: &Self,
        pairs: &mut Vec<(IndexExpr<'db>, IndexExpr<'db>)>,
    ) -> Option<()> {
        if self.contract != other.contract
            || self.reachable != other.reachable
            || self.dereferences.len() != other.dereferences.len()
        {
            return None;
        }
        match (&self.origin, &other.origin) {
            (
                ExternalOrigin::Memory {
                    base: left,
                    offset: left_offset,
                    target_ty: left_ty,
                },
                ExternalOrigin::Memory {
                    base: right,
                    offset: right_offset,
                    target_ty: right_ty,
                },
            ) => {
                if left_ty != right_ty
                    || left.views != right.views
                    || matches!((left_offset, right_offset),
                        (MemoryOffset::Element(left, _), MemoryOffset::Element(right, _)) if left != right)
                {
                    return None;
                }
                left.source.correspondence(&right.source, pairs)?;
                aligned_index_pairs(left.path.as_slice(), right.path.as_slice(), pairs)?;
                match (left_offset.index(), right_offset.index()) {
                    (Some(left), Some(right)) => pairs.push((left, right)),
                    (None, None) => {}
                    _ => return None,
                }
            }
            (ExternalOrigin::Memory { .. }, _) => {
                let (base, selector) = self.zero_wrapper_base()?;
                base.source.correspondence(other, pairs)?;
                pairs.push((selector, IndexExpr::Const(0)));
            }
            (_, ExternalOrigin::Memory { .. }) => {
                let (base, selector) = other.zero_wrapper_base()?;
                self.correspondence(&base.source, pairs)?;
                pairs.push((IndexExpr::Const(0), selector));
            }
            // Occurrence numbers are not provenance tags: a raw and a hashed
            // address with equal metadata remain distinct locations.
            (
                ExternalOrigin::Unknown {
                    contract: left_contract,
                    occurrence: left,
                    arguments: left_args,
                    provenance: left_provenance,
                },
                ExternalOrigin::Unknown {
                    contract: right_contract,
                    occurrence: right,
                    arguments: right_args,
                    provenance: right_provenance,
                },
            ) if left_contract == right_contract
                && left == right
                && left_provenance == right_provenance
                && left_args.len() == right_args.len() =>
            {
                pairs.extend(left_args.iter().copied().zip(right_args.iter().copied()));
            }
            (ExternalOrigin::OpaqueHandle(left), ExternalOrigin::OpaqueHandle(right))
            | (ExternalOrigin::Allocation(left), ExternalOrigin::Allocation(right))
                if left.occurrence == right.occurrence
                    && left.contract == right.contract
                    && left.arguments.len() == right.arguments.len() =>
            {
                pairs.extend(
                    left.arguments
                        .iter()
                        .copied()
                        .zip(right.arguments.iter().copied()),
                );
            }
            (ExternalOrigin::Local(left), ExternalOrigin::Local(right)) if left == right => {}
            (ExternalOrigin::OpaqueMemory, ExternalOrigin::OpaqueMemory) => {}
            // Comparing the cached storage classification too makes an
            // inconsistent representation fail closed.
            (ExternalOrigin::Provider { .. }, ExternalOrigin::Provider { .. })
                if self.origin == other.origin => {}
            (ExternalOrigin::Input(left), ExternalOrigin::Input(right)) => {
                left.correspondence(right, pairs)?;
            }
            _ => return None,
        }
        for (left, right) in self.dereferences.iter().zip(&other.dereferences) {
            aligned_index_pairs(left.as_slice(), right.as_slice(), pairs)?;
        }
        Some(())
    }

    /// Bind clobber dependencies for read substitution without treating them as
    /// selectors that restrict a definite typed-cell write.
    fn metadata_correspondence(
        &self,
        other: &Self,
        pairs: &mut Vec<(IndexExpr<'db>, IndexExpr<'db>)>,
    ) -> Option<()> {
        match (&self.origin, &other.origin) {
            (
                ExternalOrigin::Memory { base: left, .. },
                ExternalOrigin::Memory { base: right, .. },
            ) => left.source.metadata_correspondence(&right.source, pairs)?,
            (ExternalOrigin::Memory { .. }, _) => {
                self.zero_wrapper_base()?
                    .0
                    .source
                    .metadata_correspondence(other, pairs)?;
            }
            (_, ExternalOrigin::Memory { .. }) => {
                self.metadata_correspondence(&other.zero_wrapper_base()?.0.source, pairs)?;
            }
            _ => {}
        }
        match (&self.clobber, &other.clobber) {
            (None, None) => {}
            (Some(left), Some(right)) => {
                for (left, right) in [
                    (&left.target, &right.target),
                    (&left.written, &right.written),
                ] {
                    if left.invalidated != right.invalidated || left.views != right.views {
                        return None;
                    }
                    left.source.correspondence(&right.source, pairs)?;
                    left.source.metadata_correspondence(&right.source, pairs)?;
                    aligned_index_pairs(left.path.as_slice(), right.path.as_slice(), pairs)?;
                }
                match (left.extent, right.extent) {
                    (AccessExtent::Bytes(left), AccessExtent::Bytes(right)) => {
                        pairs.push((left, right));
                    }
                    (AccessExtent::Typed, AccessExtent::Typed)
                    | (AccessExtent::Unknown, AccessExtent::Unknown) => {}
                    _ => return None,
                }
            }
            _ => return None,
        }
        Some(())
    }

    /// A typed cell over a certain base: an element of its own type, or the
    /// base's first cell. A widened or uncertain base may name several objects.
    fn typed_cell(&self) -> Option<(&SourceExpr<'db>, TyId<'db>)> {
        match &self.origin {
            ExternalOrigin::Memory {
                base,
                offset,
                target_ty,
            } if (*offset == MemoryOffset::Zero
                || matches!(offset, MemoryOffset::Element(stride, _) if stride == target_ty))
                && self.dereferences.is_empty()
                && !self.reachable
                && !base.source.uncertain() =>
            {
                Some((base, *target_ty))
            }
            _ => None,
        }
    }

    /// The guard under which two cells of one element type index the same base.
    fn same_base_cells(&self, other: &Self, guard: &Guard<'db>) -> Option<Guard<'db>> {
        let ((left, left_ty), (right, right_ty)) = (self.typed_cell()?, other.typed_cell()?);
        if self.contract != other.contract || left_ty != right_ty || left.views != right.views {
            return None;
        }
        let mut pairs = Vec::new();
        left.source.correspondence(&right.source, &mut pairs)?;
        aligned_index_pairs(left.path.as_slice(), right.path.as_slice(), &mut pairs)?;
        guard.with_equalities(pairs)
    }

    /// Under the hash premise, a `StorageMap` slot, and the value stored from
    /// it onwards, is not the target of a compiler-allocated contract field.
    /// Following or widening either source forfeits this: a stored pointer can
    /// name any slot.
    fn is_hashed_slot_beside_allocated_field(&self, field: &Self) -> bool {
        (matches!(
            &self.origin,
            ExternalOrigin::Unknown {
                provenance: AddressProvenance::HashedStorageSlot,
                ..
            }
        ) || matches!(
            &self.origin,
            ExternalOrigin::OpaqueHandle(handle)
                if handle.provenance == AddressProvenance::HashedStorageSlot
        )) && matches!(
            field.origin,
            ExternalOrigin::Provider {
                storage: ProviderStorage::AllocatedField(_),
                ..
            }
        ) && [self, field]
            .iter()
            .all(|source| source.dereferences.is_empty() && !source.reachable)
    }

    /// The layout gives each field of a contract its own block of slots, which
    /// holds the field's typed contents. Storage named through the field's
    /// layout roots, such as a `StorageMap` entry, is a different source. A raw
    /// span may cross into the next block, and following or widening either
    /// source forfeits the block.
    fn is_allocated_field_beside(&self, other: &Self) -> bool {
        matches!(
            (&self.origin, &other.origin),
            (
                ExternalOrigin::Provider {
                    storage: ProviderStorage::AllocatedField(left),
                    ..
                },
                ExternalOrigin::Provider {
                    storage: ProviderStorage::AllocatedField(right),
                    ..
                },
            ) if left.contract == right.contract && left.index != right.index
        ) && [self, other]
            .iter()
            .all(|source| source.dereferences.is_empty() && !source.uncertain())
    }

    pub(super) fn alias_guard(
        &self,
        other: &Self,
        guard: Guard<'db>,
        allow_unknown: bool,
    ) -> Option<Guard<'db>> {
        self.alias_guard_in(other, guard, allow_unknown, true, AliasBasis::Assumed)
    }

    /// A raw byte span may cross from one cell into the next, so cells of one
    /// base are never separated by their elements.
    pub(super) fn byte_alias_guard(&self, other: &Self, guard: Guard<'db>) -> Option<Guard<'db>> {
        self.alias_guard_in(other, guard, true, false, AliasBasis::Assumed)
    }

    /// Possible overlap without the entry assumption; `typed` compares typed
    /// cells rather than byte spans.
    pub(super) fn physical_alias_guard(
        &self,
        other: &Self,
        guard: Guard<'db>,
        typed: bool,
    ) -> Option<Guard<'db>> {
        self.alias_guard_in(other, guard, true, typed, AliasBasis::Physical)
    }

    fn alias_guard_in(
        &self,
        other: &Self,
        guard: Guard<'db>,
        allow_unknown: bool,
        typed: bool,
        basis: AliasBasis,
    ) -> Option<Guard<'db>> {
        let mut pairs = Vec::new();
        let exact = if !self.is_widened() && !other.is_widened() {
            self.correspondence(other, &mut pairs)
                .and_then(|()| guard.with_equalities(pairs))
        } else {
            None
        };
        if !allow_unknown {
            return exact;
        }
        if exact.as_ref().is_some_and(|exact| guard.implies(exact)) {
            return exact;
        }
        let possible = if self.dereferences.is_empty()
            && !self.reachable
            && let ExternalOrigin::Memory { base: left, .. } = &self.origin
        {
            let other_base = match &other.origin {
                ExternalOrigin::Memory { base, .. }
                    if other.dereferences.is_empty() && !other.reachable =>
                {
                    Some(&**base)
                }
                _ => None,
            };
            let right = other_base.map_or(other, |base| &base.source);
            // Offsets compose: distinct intermediate cells can reach one final
            // address, so bases are compared without cell separation.
            let possible = left
                .source
                .alias_guard_in(right, guard.clone(), true, false, basis);
            // Where the bases are one object, the accessed typed cells of one
            // layout overlap only at an equal element, which `exact` states.
            match (
                possible,
                typed.then(|| self.same_base_cells(other, &guard)).flatten(),
            ) {
                (Some(possible), Some(same)) => possible.difference(&same),
                (possible, _) => possible,
            }
        } else if other.dereferences.is_empty()
            && !other.reachable
            && let ExternalOrigin::Memory { base: right, .. } = &other.origin
        {
            self.alias_guard_in(&right.source, guard, true, false, basis)
        } else {
            // Distinct fresh allocations and incoming pointers cannot identify the
            // same object. Unknown manufactured addresses remain conservative.
            // This assumes each raw operation's entire footprint is within its
            // allocated object. Allocation identity is not a raw bounds certificate.
            let disjoint = (matches!(
                (&self.origin, &other.origin),
                (
                    ExternalOrigin::Allocation(_),
                    ExternalOrigin::Input(_) | ExternalOrigin::Allocation(_)
                ) | (ExternalOrigin::Input(_), ExternalOrigin::Allocation(_))
            ) && self.dereferences.is_empty()
                && other.dereferences.is_empty())
                || self.is_hashed_slot_beside_allocated_field(other)
                || other.is_hashed_slot_beside_allocated_field(self)
                || (typed && self.is_allocated_field_beside(other));
            // Distinct certain sources are separate only by the entry assumption.
            (!disjoint && (self.uncertain() || other.uncertain() || basis == AliasBasis::Physical))
                .then_some(guard)
                .filter(|_| self.contract.may_alias(other.contract))
        };
        match (exact, possible) {
            (Some(exact), Some(possible)) => Some(exact.or(&possible)),
            (Some(exact), None) => Some(exact),
            (None, possible) => possible,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::semantic::capability::path::{Projection, StructuralPath},
        test_db::HirAnalysisTestDb,
    };

    #[test]
    fn clobber_coarsening_keeps_the_target_and_unchanged_write_extent() {
        let db = HirAnalysisTestDb::default();
        let memory = HandleAddressSpace::Known(ProviderAddressSpace::Memory);
        let mut target = SourceExpr::whole(ExternalSource::input(
            InputSource::slot(0, StructuralPath::default()),
            ReferentContract::new(&db, TyId::array_with_len(&db, TyId::u256(&db), 2), memory),
            false,
        ));
        target.path = RegionPath::new([Projection::Index(IndexExpr::Const(1))]);
        let written = SourceExpr::whole(ExternalSource::input(
            InputSource::slot(1, StructuralPath::default()),
            ReferentContract::new(&db, TyId::u8(&db), memory),
            true,
        ));
        for extent in [
            AccessExtent::Typed,
            AccessExtent::Bytes(IndexExpr::Const(8)),
            AccessExtent::Unknown,
        ] {
            let direct = ClobberCondition::new(target.clone(), written.clone(), extent);
            assert_eq!(direct.target, target);
            assert_eq!(direct.written, written);
            assert_eq!(direct.extent, extent);

            let offset = SourceExpr::whole(ExternalSource::memory(
                &db,
                written.clone(),
                TyId::u8(&db),
                MemoryOffset::Element(TyId::u8(&db), IndexExpr::Const(1)),
            ));
            let widened = ClobberCondition::new(target.clone(), offset, extent);
            assert_eq!(widened.target, target);
            assert_eq!(widened.written, written);
            assert_eq!(widened.extent, AccessExtent::Unknown);
        }
    }
}
