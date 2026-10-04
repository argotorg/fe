//! Structural holders and the contents of explicitly inventoried storage.
//!
//! A carrier describes its referent region. Loading that region reads a separate
//! structural value; updating it never changes the carrier or its loan identity.
use rustc_hash::{FxHashMap, FxHashSet};
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
};

use super::{
    birth::AllocationBirth,
    external::{
        FeedbackPlaces, FeedbackRepeats, FeedbackSlot, StorageMatch, feedback_clause_guard,
    },
    footprint::AccessFootprint,
    guard::{Guard, ValueOccurrence},
    handle::AddressOccurrence,
    index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
    loan::{CapabilityRef, LoanDef},
    opaque::OpaqueWrite,
    path::{RegionPath, StructuralPath, aligned_index_pairs},
    region::{OverlapResult, RegionRoot, RegionSet, SymbolicPlace},
    repack::RepackError,
    semantics::UnresolvedCapability,
    shape::ShapeId,
    source::SourceExpr,
    value::{Guarded, IndexPayload, ValueId, ValueInterner, ValueLimits},
};
use crate::analysis::{HirAnalysisDb, semantic::normalized::NValueId};

pub type CapabilityValue<'db> = ValueId<'db, CapabilityRef<'db>>;
pub type CapabilityValues<'db> = ValueInterner<'db, CapabilityRef<'db>>;

/// Typed-cell matches for one request, whose guards are in the request's
/// binder scope. Their union is the represented part of a demand.
pub struct StorageCoverage<'a, 'db> {
    pub matches: Vec<(
        &'a RegionRoot<'db>,
        &'a CapabilityValue<'db>,
        StorageMatch<'db>,
    )>,
}

impl<'db> StorageCoverage<'_, 'db> {
    fn covered(&self) -> Option<Guard<'db>> {
        self.matches
            .iter()
            .map(|(_, _, matched)| matched.guard.clone())
            .reduce(|left, right| left.or(&right))
    }

    pub fn complete(&self, demand: &Guard<'db>) -> bool {
        self.covered()
            .is_some_and(|covered| demand.implies(&covered))
    }

    pub fn uncovered(&self, demand: &Guard<'db>) -> Option<Guard<'db>> {
        self.covered().map_or_else(
            || Some(demand.clone()),
            |covered| demand.difference(&covered),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StateError<'db> {
    OpaqueContents(UnresolvedCapability<'db>),
    MissingStorage(Box<RegionRoot<'db>>),
    Repack(RepackError<'db>),
    UnrepresentableWrite(Box<RegionRoot<'db>>),
}

/// The keys and shapes are the immutable inventory shared by every block state.
/// Keeping empty holders in the inventory makes all updates shape-checked and
/// prevents a missing referent from being mistaken for capability-free storage.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BorrowState<'db> {
    guard: Guard<'db>,
    values: BTreeMap<NValueId, CapabilityValue<'db>>,
    contents: BTreeMap<RegionRoot<'db>, CapabilityValue<'db>>,
    certified_contents: Vec<CertifiedContents<'db>>,
    scalar_cells: BTreeMap<RegionRoot<'db>, Vec<Guarded<'db, Option<IndexExpr<'db>>>>>,
}

/// A verified must range over a typed family and its possible contents.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CertifiedContents<'db> {
    pub family: RegionRoot<'db>,
    pub scope: BinderScope,
    pub coverage: Guard<'db>,
    pub contents: CapabilityValue<'db>,
}

/// Prior feedback places of one holder, per structural leaf path.
type SlotSources<'db> = Vec<(StructuralPath<IndexExpr<'db>>, FeedbackPlaces<'db>)>;

/// Align a current leaf path with a prior one. Constant element indices name
/// distinct slots; other selectors are left to the feedback guards, which also
/// carry the leaf domains, such as an array default excluding exact members.
fn slot_alignment<'db>(
    current: &StructuralPath<IndexExpr<'db>>,
    previous: &StructuralPath<IndexExpr<'db>>,
) -> Option<Vec<(IndexExpr<'db>, IndexExpr<'db>)>> {
    let mut pairs = Vec::new();
    aligned_index_pairs(current.as_slice(), previous.as_slice(), &mut pairs)?;
    if pairs.iter().any(
        |pair| matches!(pair, (IndexExpr::Const(left), IndexExpr::Const(right)) if left != right),
    ) {
        return None;
    }
    pairs.retain(|pair| !matches!(pair, (IndexExpr::Const(_), IndexExpr::Const(_))));
    Some(pairs)
}

impl<'db> BorrowState<'db> {
    pub fn new(
        values: &mut CapabilityValues<'db>,
        holders: impl IntoIterator<Item = (NValueId, ShapeId<'db>)>,
        storage: impl IntoIterator<Item = (RegionRoot<'db>, CapabilityValue<'db>)>,
    ) -> Self {
        let scope = BinderScope::default();
        let mut state = Self {
            guard: Guard::always(&scope),
            values: BTreeMap::new(),
            contents: BTreeMap::new(),
            certified_contents: Vec::new(),
            scalar_cells: BTreeMap::new(),
        };
        for (id, shape) in holders {
            assert!(
                state
                    .values
                    .insert(id, values.empty(shape, &scope))
                    .is_none()
            );
        }
        for (root, value) in storage {
            assert!(
                !matches!(root, RegionRoot::Value(_)),
                "an SSA value is not an inventoried storage root"
            );
            assert!(
                value
                    .scope()
                    .variables()
                    .all(|variable| root.indices().any(|index| index == variable)),
                "storage binders must occur in its root"
            );
            for index in root.indices() {
                value.scope().validate(index).expect("free storage binder");
            }
            assert!(state.contents.insert(root, value).is_none());
        }
        state
    }

    /// Allocation inventory is a template for future births, not live storage.
    /// An allocation's unknown bytes become possible only when it is born.
    pub fn before_allocations(&self, values: &mut CapabilityValues<'db>) -> Self {
        let mut state = self.clone();
        for (root, contents) in &mut state.contents {
            if matches!(root, RegionRoot::External(source) if source.is_fresh_allocation()) {
                *contents = values.empty(contents.shape(), contents.scope());
            }
        }
        state
    }

    pub fn extend_storage(&mut self, inventory: &Self) {
        for (root, initial) in &inventory.contents {
            self.contents
                .entry(root.clone())
                .or_insert_with(|| initial.clone());
        }
    }

    /// Inventory can precede execution. Each allocation occurrence starts with
    /// its byte seed again; older loop-family members retain their own contents.
    /// A call's typed poststates are applied only after this birth.
    pub fn birth_allocations(
        &mut self,
        values: &mut CapabilityValues<'db>,
        inventory: &Self,
        families: &BTreeMap<AddressOccurrence<'db>, Vec<RegionRoot<'db>>>,
        births: &[AllocationBirth<'db>],
    ) {
        for birth in births {
            self.certified_contents.retain(|certificate| {
                birth
                    .selector(&certificate.family, &certificate.scope)
                    .is_none()
            });
            let Some(roots) = families.get(&birth.allocation.occurrence) else {
                continue;
            };
            for root in roots {
                let initial = &inventory.contents[root];
                let Some(born) = birth.selector(root, initial.scope()) else {
                    continue;
                };
                let old = self.contents.get_mut(root).expect("inventoried allocation");
                let kept =
                    values.map_guards(old, |guard| guard.difference(&born.in_scope(guard.scope())));
                let initial = values.with_guard(initial, &born);
                *old = values.join(&kept, &initial);
            }
        }
    }

    /// SSA representation roots are covered by their structural value.
    pub fn storage_coverage<'a>(
        &'a self,
        root: &'a RegionRoot<'db>,
        scope: &BinderScope,
    ) -> StorageCoverage<'a, 'db> {
        let representation = match root {
            RegionRoot::Value(value) => Some((root, self.value(*value))),
            _ => None,
        };
        StorageCoverage {
            matches: self
                .contents
                .iter()
                .chain(representation)
                .filter_map(|(candidate, contents)| {
                    let matched = storage_instance(candidate, contents.scope(), root, scope)?;
                    Some((candidate, contents, matched))
                })
                .collect(),
        }
    }

    pub fn guard(&self) -> &Guard<'db> {
        &self.guard
    }

    /// Restrict every holder and storage alternative to a feasible control-flow edge.
    pub fn constrain(&mut self, guard: &Guard<'db>, values: &mut CapabilityValues<'db>) -> bool {
        let Some(guard) = self.guard.and(guard) else {
            return false;
        };
        for value in self.values.values_mut().chain(self.contents.values_mut()) {
            *value = values.with_guard(value, &guard.in_scope(value.scope()));
        }
        for alternatives in self.scalar_cells.values_mut() {
            *alternatives = alternatives
                .iter()
                .filter_map(|entry| {
                    Some(Guarded {
                        guard: values.guards().borrow_mut().and(&entry.guard, &guard)?,
                        payload: entry.payload,
                    })
                })
                .collect();
        }
        self.scalar_cells
            .retain(|_, alternatives| !alternatives.is_empty());
        self.guard = guard;
        true
    }

    pub fn load_scalar(
        &mut self,
        root: RegionRoot<'db>,
        result: NValueId,
        values: &mut CapabilityValues<'db>,
    ) {
        if let Some(alternatives) = self.scalar_cells.get(&root) {
            let relation = alternatives
                .iter()
                .filter_map(|entry| {
                    let guard = self.guard.and(&entry.guard)?;
                    entry.payload.map_or(Some(guard.clone()), |stored| {
                        guard.with_equality(IndexExpr::Runtime(result), stored)
                    })
                })
                .reduce(|left, right| left.or(&right));
            if let Some(relation) = relation
                && relation != self.guard
            {
                self.constrain(&relation, values);
            }
        }
        self.scalar_cells.insert(
            root,
            vec![Guarded {
                guard: self.guard.clone(),
                payload: Some(IndexExpr::Runtime(result)),
            }],
        );
    }

    pub fn store_scalar(&mut self, root: RegionRoot<'db>, value: IndexExpr<'db>) {
        self.scalar_cells.insert(
            root,
            vec![Guarded {
                guard: self.guard.clone(),
                payload: Some(value),
            }],
        );
    }

    pub fn scalar_constant(&self, root: &RegionRoot<'db>) -> Option<usize> {
        let alternatives = self.scalar_cells.get(root)?;
        let Some(IndexExpr::Const(value)) = alternatives.first()?.payload else {
            return None;
        };
        let mut covered: Option<Guard<'db>> = None;
        for entry in alternatives {
            if entry.payload != Some(IndexExpr::Const(value)) {
                return None;
            }
            covered = Some(covered.map_or_else(|| entry.guard.clone(), |old| old.or(&entry.guard)));
        }
        self.guard.implies(&covered?).then_some(value)
    }

    fn invalidate_scalar_memory(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        footprint: AccessFootprint<'_, 'db>,
    ) {
        self.scalar_cells.retain(|root, _| {
            let region = RegionSet::singleton(
                footprint.region.scope(),
                root.clone(),
                RegionPath::default(),
            );
            matches!(
                AccessFootprint::typed(&region).overlap(db, footprint),
                OverlapResult::Disjoint
            )
        });
    }

    fn invalidate_certified_contents(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        footprint: AccessFootprint<'_, 'db>,
    ) {
        self.certified_contents.retain(|certificate| {
            let family = RegionSet::singleton(
                &certificate.scope,
                certificate.family.clone(),
                RegionPath::default(),
            )
            .substitute(db, &certificate.scope.freshening(footprint.region.scope()))
            .close_existentials(footprint.region.scope());
            matches!(
                AccessFootprint::typed(&family).overlap(db, footprint),
                OverlapResult::Disjoint
            )
        });
    }

    /// Cross a feedback edge. `renewed` indices get a new instance in the next
    /// iteration, so identities naming them become previous-iteration
    /// witnesses. Facts about `dropped` indices, which include the renewed
    /// ones, are dropped from guards and scalar cells. A caller can forget more
    /// facts than it renews, since dropping a fact only weakens what is known.
    pub fn forget_iteration(
        &mut self,
        values: &mut CapabilityValues<'db>,
        previous: Option<&Self>,
        renewed: impl Fn(IndexExpr<'db>) -> bool + Copy,
        dropped: impl Fn(IndexExpr<'db>) -> bool + Copy,
        occurrence: impl Fn(ValueOccurrence) -> bool + Copy,
    ) {
        self.certified_contents.retain(|certificate| {
            let family = RegionSet::singleton(
                &certificate.scope,
                certificate.family.clone(),
                RegionPath::default(),
            );
            !certificate.family.indices().any(renewed)
                && family.forget_occurrences(occurrence) == family
                && !certificate.coverage.indices().into_iter().any(renewed)
                && !certificate
                    .coverage
                    .occurrences()
                    .into_iter()
                    .any(occurrence)
                && !values
                    .leaves(&certificate.contents, ValueOccurrence::Summary)
                    .iter()
                    .any(|leaf| {
                        leaf.guard.indices().into_iter().any(renewed)
                            || leaf.guard.occurrences().into_iter().any(occurrence)
                            || leaf.payload.indices().any(renewed)
                            || leaf.payload.forget_occurrences(occurrence) != leaf.payload
                    })
        });
        // Existential projections commute; scalar projection first avoids
        // joining branch alternatives whose scalar facts will be discarded.
        self.guard = self
            .guard
            .forget_indices(dropped)
            .forget_occurrences(occurrence);
        for alternatives in self.scalar_cells.values_mut() {
            for entry in alternatives.iter_mut() {
                entry.guard = entry
                    .guard
                    .forget_indices(dropped)
                    .forget_occurrences(occurrence);
                if entry.payload.is_some_and(dropped) {
                    entry.payload = None;
                }
            }
        }
        // Each iteration forgets the same guards again, and values share clauses.
        // The temporary interner shares one cache with this one, so reuse survives.
        let mut forgotten = FxHashMap::default();
        let mut destination = CapabilityValues::sharing(values, ValueLimits::default());
        // Prior places per structural leaf: a leaf grows only from slots that
        // may be its own, not from a sibling pointer of the same aggregate.
        // A leaf with no possible counterpart before, such as a new enum
        // variant or a newly populated element, is not widened on this edge:
        // the header then holds its slot, so growth through it is found on
        // the next feedback within its own history. Leaf paths come from the
        // holder's shape, so only finitely many such edges occur.
        let mut sources: FxHashMap<CapabilityValue<'db>, SlotSources<'db>> = FxHashMap::default();
        let mut invariant_replacements = FxHashSet::default();
        let feedback_guards = RefCell::default();
        let repeats = FeedbackRepeats {
            guards: &feedback_guards,
            index: &renewed,
            occurrence: &occurrence,
        };
        if let Some(previous) = previous {
            for value in previous.values.values().chain(previous.contents.values()) {
                sources.entry(value.clone()).or_insert_with(|| {
                    let mut slots = FxHashMap::<_, FeedbackPlaces<'db>>::default();
                    for leaf in values.leaves(value, ValueOccurrence::Summary).iter() {
                        let CapabilityRef::Address(region) = &leaf.payload else {
                            continue;
                        };
                        let slot = slots.entry(leaf.path.clone()).or_default();
                        for clause in region.clauses() {
                            let RegionRoot::External(source) = &clause.payload.root else {
                                continue;
                            };
                            let Some(guard) =
                                feedback_clause_guard(&clause.guard, &leaf.guard, repeats)
                            else {
                                continue;
                            };
                            let place = SourceExpr {
                                source: source.clone(),
                                path: clause.payload.path.clone(),
                                views: clause.payload.views.clone(),
                                invalidated: false,
                            };
                            slot.entry(place).or_default().insert(guard);
                        }
                    }
                    // Copying a pointer between holders does not introduce a
                    // new overwrite. Preserve conditions already in the state.
                    // The replacement an offset of one is based on is held too.
                    invariant_replacements.extend(
                        slots
                            .values()
                            .flat_map(FeedbackPlaces::keys)
                            .map(|place| &place.source)
                            .filter(|source| source.is_raw_memory_replacement())
                            .flat_map(|source| [Some(source), source.replacement_root()])
                            .flatten()
                            .cloned(),
                    );
                    slots.into_iter().collect()
                });
            }
        }
        let holders = self
            .values
            .iter_mut()
            .map(|(id, value)| (value, previous.and_then(|previous| previous.values.get(id))));
        let storage = self.contents.iter_mut().map(|(root, value)| {
            (
                value,
                previous.and_then(|previous| previous.contents.get(root)),
            )
        });
        for (value, prior) in holders.chain(storage) {
            let prior_sources = prior.and_then(|prior| sources.get(prior));
            let mapped = values.map_payloads(value, &mut destination, |_, path, entry, domain| {
                forgotten
                    .entry((
                        entry.payload.clone(),
                        domain.clone(),
                        prior.cloned(),
                        path.clone(),
                    ))
                    .or_insert_with(|| {
                        let previous_slots: Vec<_> = prior_sources
                            .into_iter()
                            .flatten()
                            .filter_map(|(prior, places)| {
                                slot_alignment(path, prior)
                                    .map(|pairs| FeedbackSlot { pairs, places })
                            })
                            .collect();
                        let payload = entry
                            .payload
                            .widen_feedback(
                                values.db,
                                domain,
                                repeats,
                                &previous_slots,
                                &invariant_replacements,
                            )
                            .unwrap_or_else(|| entry.payload.clone())
                            .forget_occurrences(occurrence);
                        // Guard-only selectors become unobserved existential
                        // witnesses below. Project them before joining forgotten
                        // choices, which can otherwise build a large scalar
                        // disjunction only to project it during normalization.
                        let observed: BTreeSet<_> =
                            payload.indices().filter(|index| renewed(*index)).collect();
                        let guard = domain
                            .project_witnesses(|index| dropped(index) && !observed.contains(&index))
                            .forget_occurrences(occurrence);
                        let mut scope = guard.scope().clone();
                        let indices: BTreeSet<_> = guard
                            .indices()
                            .into_iter()
                            .filter(|index| dropped(*index))
                            .chain(payload.indices().filter(|index| renewed(*index)))
                            .collect();
                        let bindings: Vec<_> = indices
                            .into_iter()
                            .map(|index| {
                                let (nested, witness) = scope.bind(IndexNamespace::Existential);
                                scope = nested;
                                (index, witness)
                            })
                            .collect();
                        let identities = IndexSubst::new(
                            guard.scope(),
                            &scope,
                            bindings
                                .iter()
                                .copied()
                                .filter(|(index, _)| renewed(*index)),
                        )
                        .expect("previous value occurrence identities");
                        let subst = IndexSubst::new(guard.scope(), &scope, bindings)
                            .expect("previous value occurrence witnesses");
                        let substituted = values.guards().borrow_mut().substitute(&guard, &subst);
                        substituted.map(|guard| Guarded {
                            guard,
                            payload: payload.substitute(values.db, &identities),
                        })
                    })
                    .clone()
                    .into_iter()
                    .collect()
            });
            *value = destination.widen(&mapped);
        }
    }

    pub fn value(&self, id: NValueId) -> &CapabilityValue<'db> {
        self.values.get(&id).expect("inventoried SSA value")
    }

    pub fn set_value(&mut self, id: NValueId, value: CapabilityValue<'db>) {
        let old = self.values.get_mut(&id).expect("inventoried SSA value");
        assert_eq!(old.shape(), value.shape(), "SSA capability shape mismatch");
        assert_eq!(old.scope(), value.scope(), "SSA capability scope mismatch");
        *old = value;
    }

    pub fn holders(&self) -> impl Iterator<Item = (NValueId, &CapabilityValue<'db>)> {
        self.values.iter().map(|(id, value)| (*id, value))
    }

    pub fn storage(&self) -> impl Iterator<Item = (&RegionRoot<'db>, &CapabilityValue<'db>)> {
        self.contents.iter()
    }

    pub fn certified(&self) -> &[CertifiedContents<'db>] {
        &self.certified_contents
    }

    fn matching_certified_contents<'a>(
        &'a self,
        clause: &Guarded<'db, SymbolicPlace<'db>>,
    ) -> Vec<(&'a CertifiedContents<'db>, IndexSubst<'db>, Guard<'db>)> {
        self.certified_contents
            .iter()
            .filter_map(|certificate| {
                let (RegionRoot::External(family), RegionRoot::External(instance)) =
                    (&certificate.family, &clause.payload.root)
                else {
                    return None;
                };
                if instance.uncertain() || instance.is_reachable() {
                    return None;
                }
                let matched =
                    family.match_instance(&certificate.scope, instance, clause.guard.scope())?;
                let write = matched.write?;
                let guard = certificate
                    .coverage
                    .substitute(&matched.substitution)?
                    .and(&matched.guard)?
                    .and(&write.guard.substitute(&matched.substitution)?)?
                    .and(&clause.guard)?
                    .and(&self.guard.in_scope(clause.guard.scope()))?;
                Some((certificate, matched.substitution, guard))
            })
            .collect()
    }

    /// Definite typed initialization supplied by a verified whole-cell range.
    /// The selected request path is retained, so this is also valid for a leaf.
    pub fn certified_initialized_region(&self, region: &RegionSet<'db>) -> RegionSet<'db> {
        RegionSet::new(
            region.scope(),
            region.clauses().iter().flat_map(|clause| {
                self.matching_certified_contents(clause)
                    .into_iter()
                    .map(|(_, _, guard)| Guarded {
                        guard,
                        payload: clause.payload.clone(),
                    })
            }),
        )
    }

    /// Keep a proved range separate from possible contents, whose join may
    /// widen guards. Reads use this must fact to exclude old possibilities only
    /// on covered members of the same typed storage family.
    pub fn certify_family_contents(
        &mut self,
        family: &RegionRoot<'db>,
        family_scope: &BinderScope,
        coverage: &Guard<'db>,
        replacement: &CapabilityValue<'db>,
    ) -> bool {
        assert_eq!(coverage.scope(), family_scope, "certificate guard scope");
        assert_eq!(
            replacement.scope(),
            family_scope,
            "certificate content scope"
        );
        assert!(
            coverage.implies(&self.guard.in_scope(family_scope)),
            "certificate must be restricted to its state"
        );
        if !matches!(family, RegionRoot::External(source) if !source.uncertain() && !source.is_reachable())
            || !self.contents.get(family).is_some_and(|old| {
                old.scope() == family_scope && old.shape() == replacement.shape()
            })
        {
            return false;
        }
        self.certified_contents.push(CertifiedContents {
            family: family.clone(),
            scope: family_scope.clone(),
            coverage: coverage.clone(),
            contents: replacement.clone(),
        });
        true
    }

    pub fn join(&mut self, other: &Self, values: &mut CapabilityValues<'db>) -> bool {
        assert!(
            self.values.keys().eq(other.values.keys()),
            "holder inventory mismatch"
        );
        assert!(
            self.contents.keys().eq(other.contents.keys()),
            "storage inventory mismatch"
        );
        let previous_guard = self.guard.clone();
        let joined_guard = self.guard.or(&other.guard);
        let mut changed = joined_guard != self.guard;
        self.guard = joined_guard;
        let mut certified = Vec::new();
        for prior in &self.certified_contents {
            let matching = other.certified_contents.iter().find(|incoming| {
                incoming.family == prior.family
                    && incoming.scope == prior.scope
                    && incoming.contents == prior.contents
            });
            let old_only = prior
                .coverage
                .difference(&other.guard.in_scope(&prior.scope));
            let covered = matching.and_then(|incoming| prior.coverage.and(&incoming.coverage));
            let new_only = matching.and_then(|incoming| {
                incoming
                    .coverage
                    .difference(&previous_guard.in_scope(&incoming.scope))
            });
            if let Some(coverage) = old_only
                .into_iter()
                .chain(covered)
                .chain(new_only)
                .reduce(|left, right| left.or(&right))
            {
                let mut retained = prior.clone();
                retained.coverage = coverage;
                certified.push(retained);
            }
        }
        for incoming in &other.certified_contents {
            if self.certified_contents.iter().any(|prior| {
                prior.family == incoming.family
                    && prior.scope == incoming.scope
                    && prior.contents == incoming.contents
            }) {
                continue;
            }
            if let Some(coverage) = incoming
                .coverage
                .difference(&previous_guard.in_scope(&incoming.scope))
            {
                let mut retained = incoming.clone();
                retained.coverage = coverage;
                certified.push(retained);
            }
        }
        changed |= certified != self.certified_contents;
        self.certified_contents = certified;
        let mut scalar_cells = BTreeMap::new();
        for root in self
            .scalar_cells
            .keys()
            .chain(other.scalar_cells.keys())
            .cloned()
            .collect::<BTreeSet<_>>()
        {
            let mut alternatives = self.scalar_cells.get(&root).cloned().unwrap_or_else(|| {
                vec![Guarded {
                    guard: previous_guard.clone(),
                    payload: None,
                }]
            });
            alternatives.extend(other.scalar_cells.get(&root).cloned().unwrap_or_else(|| {
                vec![Guarded {
                    guard: other.guard.clone(),
                    payload: None,
                }]
            }));
            let mut by_value = BTreeMap::new();
            for entry in alternatives {
                by_value
                    .entry(entry.payload)
                    .and_modify(|guard: &mut Guard<'db>| *guard = guard.or(&entry.guard))
                    .or_insert(entry.guard);
            }
            scalar_cells.insert(
                root,
                by_value
                    .into_iter()
                    .map(|(payload, guard)| Guarded { guard, payload })
                    .collect(),
            );
        }
        changed |= scalar_cells != self.scalar_cells;
        self.scalar_cells = scalar_cells;
        for (old, incoming) in self
            .values
            .values_mut()
            .zip(other.values.values())
            .chain(self.contents.values_mut().zip(other.contents.values()))
        {
            let joined = values.join(old, incoming);
            let joined = values.widen(&joined);
            changed |= joined != *old;
            *old = joined;
        }
        changed
    }

    /// Resolve only the direct carrier. Nested handles are contents of its target,
    /// not additional destinations of the outer borrow.
    pub fn referent_region(
        &self,
        db: &'db dyn HirAnalysisDb,
        carrier: NValueId,
        path: &RegionPath<IndexExpr<'db>>,
        loans: &[LoanDef<'db>],
    ) -> RegionSet<'db> {
        let value = self.value(carrier);
        RegionSet::union_all(
            value.scope(),
            value
                .direct()
                .iter()
                .filter(|entry| !matches!(entry.payload, CapabilityRef::Invalidated { .. }))
                .map(|entry| {
                    entry
                        .payload
                        .region(db, loans, entry.guard.scope())
                        .with_guard(&entry.guard)
                        .close_existentials(value.scope())
                }),
        )
        .project(path)
    }

    /// Load structural contents from the selected storage, preserving guards and
    /// array-family arguments. The caller supplies the snapshot's enum identity.
    pub fn read_region(
        &self,
        db: &'db dyn HirAnalysisDb,
        values: &mut CapabilityValues<'db>,
        region: &RegionSet<'db>,
        shape: ShapeId<'db>,
        occurrence: ValueOccurrence,
    ) -> Result<CapabilityValue<'db>, StateError<'db>> {
        let mut result = values.empty(shape, region.scope());
        if !shape.contains_capability(db) {
            return Ok(result);
        }
        for clause in region.clauses() {
            let coverage = self.storage_coverage(&clause.payload.root, clause.guard.scope());
            if !coverage.complete(&clause.guard) {
                return Err(StateError::MissingStorage(Box::new(
                    clause.payload.root.clone(),
                )));
            }
            // A certified range replaces every older possibility on its members.
            let certified = self.matching_certified_contents(clause);
            let certified_coverage = certified
                .iter()
                .map(|(_, _, guard)| guard.clone())
                .reduce(|left, right| left.or(&right));
            let possible = coverage
                .matches
                .into_iter()
                .filter_map(|(_, contents, matched)| {
                    let guard = matched.guard.and(&clause.guard)?;
                    let guard = match &certified_coverage {
                        Some(covered) => guard.difference(covered)?,
                        None => guard,
                    };
                    Some((contents, matched.substitution, guard))
                });
            let certified = certified
                .into_iter()
                .map(|(certificate, substitution, guard)| {
                    (&certificate.contents, substitution, guard)
                });
            for (contents, substitution, guard) in possible.chain(certified) {
                let path = StructuralPath::new(clause.payload.path.as_slice());
                let Some(selected) =
                    values.project_substituted(contents, &substitution, &path, occurrence)
                else {
                    continue;
                };
                let selected = clause
                    .payload
                    .views
                    .apply(db, values, &selected, clause.payload.path.as_slice(), false)
                    .map_err(StateError::Repack)?;
                assert_eq!(selected.shape(), shape, "referent load shape mismatch");
                let selected = values.with_guard(&selected, &guard);
                let selected = values.close_existentials(&selected, region.scope());
                result = values.join(&result, &selected);
            }
        }
        Ok(result)
    }

    /// A singleton destination replaces its selected member under its guard.
    /// Ambiguous alternatives retain old contents and weakly add the new ones.
    /// All replacements are prepared before changing state, so failure is atomic.
    pub fn write_region(
        &mut self,
        overwrite: OpaqueWrite<'db>,
        values: &mut CapabilityValues<'db>,
        region: &RegionSet<'db>,
        replacement: &CapabilityValue<'db>,
    ) -> Result<(), StateError<'db>> {
        self.write_regions(overwrite, values, &[(region, replacement)])
    }

    /// Consuming a value retires its modeled capabilities without writing bytes
    /// or manufacturing clobbers in possibly overlapping cells. Availability
    /// separately prevents subsequent uses through every alias.
    pub fn move_out(
        &mut self,
        values: &mut CapabilityValues<'db>,
        region: &RegionSet<'db>,
        shape: ShapeId<'db>,
    ) -> Result<(), StateError<'db>> {
        let empty = values.empty(shape, region.scope());
        self.update_regions(None, values, &[(region, &empty)])?;
        self.invalidate_certified_contents(values.db, AccessFootprint::typed(region));
        self.invalidate_scalar_memory(values.db, AccessFootprint::typed(region));
        Ok(())
    }

    /// Byte writes may destroy every overlapping typed interpretation, including
    /// an exact cell when an intrinsic supplies no structural poststate.
    pub fn invalidate_memory(
        &mut self,
        values: &mut CapabilityValues<'db>,
        footprint: AccessFootprint<'_, 'db>,
        overwrite: OpaqueWrite<'db>,
    ) -> Result<(), StateError<'db>> {
        let region = footprint.region;
        let mut updates = BTreeMap::new();
        for (root, contents) in &self.contents {
            if !contents.shape().contains_capability(values.db) {
                continue;
            }
            let candidate =
                RegionSet::singleton(contents.scope(), root.clone(), RegionPath::default())
                    .substitute(values.db, &contents.scope().freshening(region.scope()))
                    .close_existentials(region.scope());
            if !matches!(
                AccessFootprint::typed(&candidate).overlap(values.db, footprint),
                OverlapResult::Disjoint
            ) {
                let unknown = overwrite
                    .contents(
                        values,
                        contents.shape(),
                        contents.scope(),
                        Some(root),
                        Some(footprint),
                    )
                    .map_err(StateError::OpaqueContents)?;
                updates.insert(root.clone(), values.join(contents, &unknown));
            }
        }
        self.contents.extend(updates);
        self.invalidate_certified_contents(values.db, footprint);
        self.invalidate_scalar_memory(values.db, footprint);
        Ok(())
    }

    /// Apply one call's complete poststate without imposing an order on aliased
    /// inputs. Independent destinations replace exactly; possibly overlapping
    /// destinations weakly retain every candidate. Failure leaves state intact.
    pub fn write_regions(
        &mut self,
        overwrite: OpaqueWrite<'db>,
        values: &mut CapabilityValues<'db>,
        replacements: &[(&RegionSet<'db>, &CapabilityValue<'db>)],
    ) -> Result<(), StateError<'db>> {
        self.update_regions(Some(overwrite), values, replacements)
    }

    fn update_regions(
        &mut self,
        overwrite: Option<OpaqueWrite<'db>>,
        values: &mut CapabilityValues<'db>,
        replacements: &[(&RegionSet<'db>, &CapabilityValue<'db>)],
    ) -> Result<(), StateError<'db>> {
        let scope = BinderScope::default();
        let independent: Vec<_> = replacements
            .iter()
            .map(|(region, replacement)| {
                assert_eq!(region.scope(), replacement.scope(), "store scope mismatch");
                region
                    .substitute(values.db, &region.scope().freshening(&scope))
                    .close_existentials(&scope)
            })
            .collect();
        let mut updates = BTreeMap::new();
        // Family members each destination clause reaches as the same typed cell,
        // and those among them the typed update wrote.
        let mut typed = Vec::new();
        let mut written = Vec::new();
        for (index, (region, replacement)) in replacements.iter().enumerate() {
            let interferes = independent.iter().enumerate().any(|(other, region)| {
                index != other
                    && !matches!(
                        independent[index].overlap(values.db, region),
                        OverlapResult::Disjoint
                    )
            });
            for clause in region.clauses() {
                let coverage = self.storage_coverage(&clause.payload.root, clause.guard.scope());
                if matches!(clause.payload.root, RegionRoot::Value(_))
                    || !coverage.complete(&clause.guard)
                {
                    return Err(StateError::MissingStorage(Box::new(
                        clause.payload.root.clone(),
                    )));
                }
                let mut domains = BTreeMap::new();
                let mut applied = BTreeMap::new();
                for (root, contents, matched) in coverage.matches {
                    if let Some(domain) = &matched.typed {
                        domains.insert(root, domain.clone());
                    }
                    let (Some(write_guard), Some(embedding)) =
                        (clause.guard.and(&matched.guard), matched.write)
                    else {
                        continue;
                    };
                    if let Some(domain) = matched.typed {
                        applied.insert(root, domain);
                    }
                    let path = StructuralPath::new(clause.payload.path.as_slice());
                    let old = updates.get(root).unwrap_or(contents);
                    let lift = IndexSubst::new(replacement.scope(), clause.guard.scope(), [])
                        .expect("store witness scope");
                    let replacement = values.substitute(replacement, &lift);
                    let replacement = clause
                        .payload
                        .views
                        .apply(
                            values.db,
                            values,
                            &replacement,
                            clause.payload.path.as_slice(),
                            true,
                        )
                        .map_err(StateError::Repack)?;
                    let changed = values
                        .replace_family(old, &path, &replacement, &write_guard, &embedding)
                        .map_err(|_| {
                            StateError::UnrepresentableWrite(Box::new(clause.payload.root.clone()))
                        })?;
                    let updated = if !interferes && region.definite_write().is_some() {
                        changed
                    } else {
                        values.join(old, &changed)
                    };
                    updates.insert(root.clone(), updated);
                }
                typed.push(domains);
                written.push(applied);
            }
        }
        let Some(overwrite) = overwrite else {
            self.contents.extend(updates);
            return Ok(());
        };
        let clauses = replacements
            .iter()
            .flat_map(|(region, _)| region.clauses().iter().map(move |clause| (*region, clause)));
        for ((region, clause), domains) in clauses.zip(&typed) {
            for (root, contents) in &self.contents {
                if !contents.shape().contains_capability(values.db)
                    || !root.may_alias_unknown(&clause.payload.root)
                {
                    continue;
                }
                // The typed update accounts for members reached as the same
                // typed cell. Other members may still share written bytes.
                let residual = match domains.get(root) {
                    Some(domain) => Guard::always(contents.scope()).difference(domain),
                    None => Some(Guard::always(contents.scope())),
                };
                let Some(residual) = residual else {
                    continue;
                };
                let candidate =
                    RegionSet::singleton(contents.scope(), root.clone(), RegionPath::default())
                        .with_guard(&residual)
                        .substitute(values.db, &contents.scope().freshening(region.scope()))
                        .close_existentials(region.scope());
                if matches!(
                    candidate.overlap(values.db, region),
                    OverlapResult::Disjoint
                ) {
                    continue;
                }
                // Unknown bases may overlap at a byte offset, even when
                // their typed contents have the same shape. A partial store
                // can corrupt a native capability rather than copy it intact.
                let old = updates.get(root).unwrap_or(contents);
                let unknown = overwrite
                    .contents(
                        values,
                        contents.shape(),
                        contents.scope(),
                        Some(root),
                        Some(AccessFootprint::typed(region)),
                    )
                    .map_err(StateError::OpaqueContents)?;
                let unknown = values.with_guard(&unknown, &residual);
                updates.insert(root.clone(), values.join(old, &unknown));
            }
        }
        // Unknown bases may overlap at different physical offsets. Retain every
        // compatible stored capability, with independent witnesses for the source
        // and destination families; an unknown write never removes prior contents.
        // Members the typed update wrote already hold the replacement.
        let mut written = written.into_iter();
        for (region, replacement) in replacements {
            for clause in region.clauses() {
                let applied = written.next().expect("typed domains for every clause");
                let lift = IndexSubst::new(replacement.scope(), clause.guard.scope(), [])
                    .expect("unknown store witness scope");
                let replacement = values.substitute(replacement, &lift);
                let replacement = clause
                    .payload
                    .views
                    .apply(
                        values.db,
                        values,
                        &replacement,
                        clause.payload.path.as_slice(),
                        true,
                    )
                    .map_err(StateError::Repack)?;
                let replacement = values.with_guard(&replacement, &clause.guard);
                let leaves = values.leaves(&replacement, ValueOccurrence::Summary);
                if leaves.is_empty() {
                    continue;
                }
                let db = values.db;
                for (root, contents) in &self.contents {
                    if !root.may_alias_unknown(&clause.payload.root)
                        || (root == &clause.payload.root
                            && !root.is_widened()
                            && root.indices().next().is_none())
                    {
                        continue;
                    }
                    let residual = match applied.get(root) {
                        Some(domain) => Guard::always(contents.scope()).difference(domain),
                        None => Some(Guard::always(contents.scope())),
                    };
                    let Some(residual) = residual else {
                        continue;
                    };
                    let candidate =
                        RegionSet::singleton(contents.scope(), root.clone(), RegionPath::default())
                            .with_guard(&residual)
                            .substitute(values.db, &contents.scope().freshening(region.scope()))
                            .close_existentials(region.scope());
                    if matches!(
                        candidate.overlap(values.db, region),
                        OverlapResult::Disjoint
                    ) {
                        continue;
                    }
                    let added = values.from_shape(
                        contents.shape(),
                        contents.scope(),
                        |semantics, _, scope| {
                            leaves
                                .iter()
                                .filter(|leaf| leaf.semantics == semantics)
                                .filter_map(|leaf| {
                                    let subst = leaf.guard.scope().freshening(scope);
                                    Some(Guarded {
                                        guard: leaf.guard.substitute(&subst)?,
                                        payload: leaf.payload.substitute(db, &subst),
                                    })
                                })
                                .collect()
                        },
                    );
                    let added = values.with_guard(&added, &residual);
                    let old = updates.get(root).unwrap_or(contents);
                    updates.insert(root.clone(), values.join(old, &added));
                }
            }
        }
        self.contents.extend(updates);
        for (region, _) in replacements {
            self.invalidate_certified_contents(values.db, AccessFootprint::typed(region));
            self.invalidate_scalar_memory(values.db, AccessFootprint::typed(region));
        }
        Ok(())
    }
}

fn storage_instance<'db>(
    root: &RegionRoot<'db>,
    scope: &BinderScope,
    instance: &RegionRoot<'db>,
    instance_scope: &BinderScope,
) -> Option<StorageMatch<'db>> {
    match (root, instance) {
        (RegionRoot::External(root), RegionRoot::External(instance)) => {
            root.match_instance(scope, instance, instance_scope)
        }
        _ if root == instance => Some(StorageMatch {
            substitution: IndexSubst::new(scope, instance_scope, []).ok()?,
            guard: Guard::always(instance_scope),
            typed: Some(Guard::always(scope)),
            write: Some(Guarded {
                guard: Guard::always(scope),
                payload: BTreeMap::new(),
            }),
        }),
        _ => None,
    }
}

#[cfg(test)]
mod tests;
