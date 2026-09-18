//! Structural holders and the contents of explicitly inventoried storage.
//!
//! A carrier describes its referent region. Loading that region reads a separate
//! structural value; updating it never changes the carrier or its loan identity.
use std::collections::BTreeMap;

use super::{
    guard::{Guard, ValueOccurrence},
    index::{BinderScope, IndexExpr, IndexSubst},
    loan::{CapabilityRef, LoanDef},
    path::{RegionPath, StructuralPath},
    region::{RegionRoot, RegionSet},
    shape::ShapeId,
    value::{Guarded, ValueId, ValueInterner},
};
use crate::analysis::{HirAnalysisDb, semantic::normalized::NValueId};

pub type CapabilityValue<'db> = ValueId<'db, CapabilityRef<'db>>;
pub type CapabilityValues<'db> = ValueInterner<'db, CapabilityRef<'db>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StateError<'db> {
    MissingStorage(RegionRoot<'db>),
    UnrepresentableWrite(RegionRoot<'db>),
}

/// The keys and shapes are the immutable inventory shared by every block state.
/// Keeping empty holders in the inventory makes all updates shape-checked and
/// prevents a missing referent from being mistaken for capability-free storage.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BorrowState<'db> {
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
        let mut changed = false;
        for (old, incoming) in self
            .values
            .values_mut()
            .zip(other.values.values())
            .chain(self.contents.values_mut().zip(other.contents.values()))
        {
            let joined = values.join(old, incoming);
            changed |= joined != *old;
            *old = joined;
        }
        changed
    }

    /// Resolve only the direct carrier. Nested handles are contents of its target,
    /// not additional destinations of the outer borrow.
    pub fn referent_region(
        &self,
        carrier: NValueId,
        path: &RegionPath<IndexExpr<'db>>,
        loans: &[LoanDef<'db>],
    ) -> RegionSet<'db> {
        let value = self.value(carrier);
        value
            .direct()
            .iter()
            .fold(RegionSet::empty(value.scope()), |region, entry| {
                region.union(
                    &entry
                        .payload
                        .region(loans, value.scope())
                        .with_guard(&entry.guard),
                )
            })
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
            for (root, contents) in &self.contents {
                let Some((substitution, guard)) =
                    storage_instance(root, contents.scope(), &clause.payload.root, region.scope())
                else {
                    continue;
                };
                covered = Some(covered.map_or_else(|| guard.clone(), |old| old.or(&guard)));
                let Some(guard) = guard.and(&clause.guard) else {
                    continue;
                };
                let contents = values.substitute(contents, &substitution);
                let path = StructuralPath::new(clause.payload.path.as_slice());
                let selected = values.project(&contents, &path, occurrence);
                assert_eq!(selected.shape(), shape, "referent load shape mismatch");
                let selected = values.with_guard(&selected, &guard);
                result = values.join(&result, &selected);
            }
            if covered.is_none_or(|guard| !clause.guard.implies(&guard)) {
                return Err(StateError::MissingStorage(clause.payload.root.clone()));
            }
        }
        Ok(result)
    }

    /// A singleton destination replaces its selected member under its guard.
    /// Ambiguous alternatives retain old contents and weakly add the new ones.
    /// All replacements are prepared before changing state, so failure is atomic.
    pub fn write_region(
        &mut self,
        values: &mut CapabilityValues<'db>,
        region: &RegionSet<'db>,
        replacement: &CapabilityValue<'db>,
    ) -> Result<(), StateError<'db>> {
        assert_eq!(region.scope(), replacement.scope(), "store scope mismatch");
        let mut updates = BTreeMap::new();
        for clause in region.clauses() {
            let mut covered: Option<Guard<'db>> = None;
            for (root, contents) in &self.contents {
                let Some((_, match_guard)) =
                    storage_instance(root, contents.scope(), &clause.payload.root, region.scope())
                else {
                    continue;
                };
                covered =
                    Some(covered.map_or_else(|| match_guard.clone(), |old| old.or(&match_guard)));
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
                let changed = values
                    .replace_family(
                        old,
                        &path,
                        replacement,
                        &write_guard,
                        &Guarded {
                            guard: family_guard,
                            payload: bindings,
                        },
                    )
                    .map_err(|_| StateError::UnrepresentableWrite(clause.payload.root.clone()))?;
                let updated = if region.clauses().len() == 1 {
                    changed
                } else {
                    values.join(old, &changed)
                };
                updates.insert(root.clone(), updated);
            }
            if covered.is_none_or(|guard| !clause.guard.implies(&guard)) {
                return Err(StateError::MissingStorage(clause.payload.root.clone()));
            }
        }
        self.contents.extend(updates);
        Ok(())
    }
}

fn storage_instance<'db>(
    root: &RegionRoot<'db>,
    scope: &BinderScope,
    instance: &RegionRoot<'db>,
    instance_scope: &BinderScope,
) -> Option<(IndexSubst<'db>, Guard<'db>)> {
    match (root, instance) {
        (RegionRoot::Input(root), RegionRoot::Input(instance)) => {
            root.match_instance(scope, instance, instance_scope)
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
