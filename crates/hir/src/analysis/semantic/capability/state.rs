//! Structural holders and the contents of explicitly inventoried storage.
//!
//! A carrier describes its referent region. Loading that region reads a separate
//! structural value; updating it never changes the carrier or its loan identity.
use std::collections::{BTreeMap, BTreeSet};

use super::{
    birth::AllocationBirth,
    footprint::AccessFootprint,
    guard::{Guard, ValueOccurrence},
    handle::AddressOccurrence,
    index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
    loan::{CapabilityRef, LoanDef},
    opaque::OpaqueWrite,
    path::{RegionPath, StructuralPath},
    region::{OverlapResult, RegionRoot, RegionSet},
    repack::RepackError,
    semantics::UnresolvedCapability,
    shape::ShapeId,
    value::{Guarded, IndexPayload, ValueId, ValueInterner, ValueLimits},
};
use crate::analysis::{HirAnalysisDb, semantic::normalized::NValueId};

pub type CapabilityValue<'db> = ValueId<'db, CapabilityRef<'db>>;
pub type CapabilityValues<'db> = ValueInterner<'db, CapabilityRef<'db>>;

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

    pub fn has_storage(
        &self,
        db: &'db dyn HirAnalysisDb,
        root: &RegionRoot<'db>,
        scope: &BinderScope,
    ) -> bool {
        self.contents.iter().any(|(candidate, contents)| {
            storage_instance(db, candidate, contents.scope(), root, scope).is_some()
        })
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
        self.guard = guard;
        true
    }

    pub fn forget_iteration(
        &mut self,
        values: &mut CapabilityValues<'db>,
        repeated: impl Fn(IndexExpr<'db>) -> bool + Copy,
        occurrence: impl Fn(ValueOccurrence) -> bool + Copy,
    ) {
        self.guard = self
            .guard
            .forget_occurrences(occurrence)
            .forget_indices(repeated);
        let mut destination = CapabilityValues::new(values.db, ValueLimits::default());
        for value in self.values.values_mut().chain(self.contents.values_mut()) {
            let mapped = values.map_payloads(value, &mut destination, |_, _, entry, domain| {
                let guard = domain.forget_occurrences(occurrence);
                let payload = entry.payload.forget_occurrences(occurrence);
                let mut scope = guard.scope().clone();
                let indices: BTreeSet<_> = guard
                    .indices()
                    .into_iter()
                    .chain(payload.indices())
                    .filter(|index| repeated(*index))
                    .collect();
                let bindings: Vec<_> = indices
                    .into_iter()
                    .map(|index| {
                        let (nested, witness) = scope.bind(IndexNamespace::Existential);
                        scope = nested;
                        (index, witness)
                    })
                    .collect();
                let subst = IndexSubst::new(guard.scope(), &scope, bindings)
                    .expect("previous value occurrence witnesses");
                guard
                    .substitute(&subst)
                    .map(|guard| Guarded {
                        guard,
                        payload: payload.substitute(values.db, &subst),
                    })
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

    pub fn join(&mut self, other: &Self, values: &mut CapabilityValues<'db>) -> bool {
        assert!(
            self.values.keys().eq(other.values.keys()),
            "holder inventory mismatch"
        );
        assert!(
            self.contents.keys().eq(other.contents.keys()),
            "storage inventory mismatch"
        );
        let joined_guard = self.guard.or(&other.guard);
        let mut changed = joined_guard != self.guard;
        self.guard = joined_guard;
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
            let mut covered: Option<Guard<'db>> = None;
            let representation = if let RegionRoot::Value(value) = clause.payload.root {
                Some((&clause.payload.root, self.value(value)))
            } else {
                None
            };
            for (root, contents) in self.contents.iter().chain(representation) {
                let Some((substitution, guard)) = storage_instance(
                    db,
                    root,
                    contents.scope(),
                    &clause.payload.root,
                    clause.guard.scope(),
                ) else {
                    continue;
                };
                covered = Some(covered.map_or_else(|| guard.clone(), |old| old.or(&guard)));
                let Some(guard) = guard.and(&clause.guard) else {
                    continue;
                };
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
            if covered.is_none_or(|guard| !clause.guard.implies(&guard)) {
                return Err(StateError::MissingStorage(Box::new(
                    clause.payload.root.clone(),
                )));
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
                        Some((root, footprint)),
                    )
                    .map_err(StateError::OpaqueContents)?;
                updates.insert(root.clone(), values.join(contents, &unknown));
            }
        }
        self.contents.extend(updates);
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
        for (index, (region, replacement)) in replacements.iter().enumerate() {
            let interferes = independent.iter().enumerate().any(|(other, region)| {
                index != other
                    && !matches!(
                        independent[index].overlap(values.db, region),
                        OverlapResult::Disjoint
                    )
            });
            for clause in region.clauses() {
                let mut covered: Option<Guard<'db>> = None;
                for (root, contents) in &self.contents {
                    let Some((_, match_guard)) = storage_instance(
                        values.db,
                        root,
                        contents.scope(),
                        &clause.payload.root,
                        clause.guard.scope(),
                    ) else {
                        continue;
                    };
                    covered = Some(
                        covered.map_or_else(|| match_guard.clone(), |old| old.or(&match_guard)),
                    );
                    let Some(write_guard) = clause.guard.and(&match_guard) else {
                        continue;
                    };
                    // Bind a selected symbolic occurrence back to its storage family.
                    // Constants and runtime selectors stay free, so a write to one
                    // member is guarded by equality with the family's parameter.
                    let mut bindings = BTreeMap::new();
                    for (formal, actual) in root.indices().zip(clause.payload.root.indices()) {
                        if matches!(actual, IndexExpr::Bound(_)) {
                            bindings.entry(actual).or_insert(formal);
                        }
                    }
                    let mut family_guard = Some(Guard::always(contents.scope()));
                    for (formal, actual) in root.indices().zip(clause.payload.root.indices()) {
                        let actual = bindings.get(&actual).copied().unwrap_or(actual);
                        family_guard =
                            family_guard.and_then(|guard| guard.with_equality(formal, actual));
                    }
                    let Some(family_guard) = family_guard else {
                        continue;
                    };
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
                        .replace_family(
                            old,
                            &path,
                            &replacement,
                            &write_guard,
                            &Guarded {
                                guard: family_guard,
                                payload: bindings,
                            },
                        )
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
                if covered.is_none_or(|guard| !clause.guard.implies(&guard)) {
                    return Err(StateError::MissingStorage(Box::new(
                        clause.payload.root.clone(),
                    )));
                }
            }
        }
        for (region, _) in replacements {
            for clause in region.clauses() {
                for (root, contents) in &self.contents {
                    if !contents.shape().contains_capability(values.db)
                        || storage_instance(
                            values.db,
                            root,
                            contents.scope(),
                            &clause.payload.root,
                            clause.guard.scope(),
                        )
                        .is_some()
                        || !root.may_alias_unknown(&clause.payload.root)
                    {
                        continue;
                    }
                    let candidate =
                        RegionSet::singleton(contents.scope(), root.clone(), RegionPath::default());
                    let candidate = candidate
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
                            Some((root, AccessFootprint::typed(region))),
                        )
                        .map_err(StateError::OpaqueContents)?;
                    updates.insert(root.clone(), values.join(old, &unknown));
                }
            }
        }
        // Unknown bases may overlap at different physical offsets. Retain every
        // compatible stored capability, with independent witnesses for the source
        // and destination families; an unknown write never removes prior contents.
        for (region, replacement) in replacements {
            for clause in region.clauses() {
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
                            && !root.is_reachable()
                            && root.indices().next().is_none())
                    {
                        continue;
                    }
                    let candidate =
                        RegionSet::singleton(contents.scope(), root.clone(), RegionPath::default())
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
                    let old = updates.get(root).unwrap_or(contents);
                    updates.insert(root.clone(), values.join(old, &added));
                }
            }
        }
        self.contents.extend(updates);
        Ok(())
    }
}

fn storage_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    root: &RegionRoot<'db>,
    scope: &BinderScope,
    instance: &RegionRoot<'db>,
    instance_scope: &BinderScope,
) -> Option<(IndexSubst<'db>, Guard<'db>)> {
    match (root, instance) {
        (RegionRoot::External(root), RegionRoot::External(instance)) => {
            root.match_instance(db, scope, instance, instance_scope)
        }
        _ if root == instance => Some((
            IndexSubst::new(scope, instance_scope, []).ok()?,
            Guard::always(instance_scope),
        )),
        _ => None,
    }
}

#[cfg(test)]
mod tests;
