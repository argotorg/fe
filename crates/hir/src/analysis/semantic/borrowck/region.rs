use crate::{
    analysis::semantic::{FieldIndex, SLocalId, VariantIndex},
    semantic::ProviderBinding,
};

use super::{
    guard::{ExistentialId, Guard, IndexExpr, IndexSubst},
    shape::{SlotPath, SlotProjection},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum RegionRoot<'db> {
    ParamPlace(u32),
    ParamCapability {
        param: u32,
        slot: SlotPath<IndexExpr>,
    },
    Local(SLocalId),
    Provider(ProviderBinding<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum RegionProjection {
    Field(FieldIndex),
    VariantField {
        variant: VariantIndex,
        field: FieldIndex,
    },
    Index(IndexExpr),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct SymbolicPlace<'db> {
    root: RegionRoot<'db>,
    projection: Vec<RegionProjection>,
}

impl<'db> SymbolicPlace<'db> {
    pub(crate) fn new(
        root: RegionRoot<'db>,
        projection: impl IntoIterator<Item = RegionProjection>,
    ) -> Self {
        Self {
            root,
            projection: projection.into_iter().collect(),
        }
    }

    pub(crate) fn root(&self) -> &RegionRoot<'db> {
        &self.root
    }

    pub(crate) fn projection(&self) -> &[RegionProjection] {
        &self.projection
    }

    fn project(&self, projection: &[RegionProjection]) -> Self {
        Self {
            root: self.root.clone(),
            projection: self.projection.iter().chain(projection).cloned().collect(),
        }
    }

    pub(crate) fn substitute(&self, subst: &IndexSubst) -> Self {
        Self {
            root: match &self.root {
                RegionRoot::ParamCapability { param, slot } => RegionRoot::ParamCapability {
                    param: *param,
                    slot: substitute_slot_path(slot, subst),
                },
                root => root.clone(),
            },
            projection: self
                .projection
                .iter()
                .map(|projection| match projection {
                    RegionProjection::Field(field) => RegionProjection::Field(*field),
                    RegionProjection::VariantField { variant, field } => {
                        RegionProjection::VariantField {
                            variant: *variant,
                            field: *field,
                        }
                    }
                    RegionProjection::Index(index) => RegionProjection::Index(subst.apply(*index)),
                })
                .collect(),
        }
    }

    pub(crate) fn index_exprs(&self) -> Vec<IndexExpr> {
        let root = match &self.root {
            RegionRoot::ParamCapability { slot, .. } => slot
                .as_slice()
                .iter()
                .filter_map(|projection| match projection {
                    SlotProjection::Index(index) => Some(*index),
                    SlotProjection::Field(_) | SlotProjection::VariantField { .. } => None,
                })
                .collect::<Vec<_>>(),
            RegionRoot::ParamPlace(_) | RegionRoot::Local(_) | RegionRoot::Provider(_) => {
                Vec::new()
            }
        };
        root.into_iter()
            .chain(self.projection.iter().filter_map(|projection| {
                if let RegionProjection::Index(index) = projection {
                    Some(*index)
                } else {
                    None
                }
            }))
            .collect()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct RegionClause<'db> {
    guard: Guard,
    place: SymbolicPlace<'db>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub(crate) struct RegionSet<'db> {
    clauses: Vec<RegionClause<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct OverlapWitness<'db> {
    pub(crate) left: SymbolicPlace<'db>,
    pub(crate) right: SymbolicPlace<'db>,
    pub(crate) model: Guard,
}

impl<'db> RegionSet<'db> {
    pub(crate) fn empty() -> Self {
        Self::default()
    }

    pub(crate) fn singleton(place: SymbolicPlace<'db>) -> Self {
        Self::from_clause(Guard::always(), place)
    }

    pub(crate) fn from_clause(guard: Guard, place: SymbolicPlace<'db>) -> Self {
        Self::normalize(vec![RegionClause { guard, place }])
    }

    pub(crate) fn union(&self, other: &Self) -> Self {
        Self::normalize(self.clauses.iter().chain(&other.clauses).cloned().collect())
    }

    pub(crate) fn with_guard(&self, guard: &Guard) -> Self {
        Self::normalize(
            self.clauses
                .iter()
                .filter_map(|clause| {
                    Some(RegionClause {
                        guard: clause.guard.and(guard)?,
                        place: clause.place.clone(),
                    })
                })
                .collect(),
        )
    }

    pub(crate) fn substitute(&self, subst: &IndexSubst) -> Self {
        Self::normalize(
            self.clauses
                .iter()
                .filter_map(|clause| {
                    Some(RegionClause {
                        guard: clause.guard.substitute(subst)?,
                        place: clause.place.substitute(subst),
                    })
                })
                .collect(),
        )
    }

    pub(crate) fn project(&self, projection: &[RegionProjection]) -> Self {
        Self::normalize(
            self.clauses
                .iter()
                .map(|clause| RegionClause {
                    guard: clause.guard.clone(),
                    place: clause.place.project(projection),
                })
                .collect(),
        )
    }

    pub(crate) fn may_overlap(&self, other: &Self) -> Option<OverlapWitness<'db>> {
        for lhs in &self.clauses {
            for rhs in &other.clauses {
                let rhs = freshen_against(rhs, lhs);
                let Some(mut model) = lhs.guard.and(&rhs.guard) else {
                    continue;
                };
                if !roots_match(&lhs.place.root, &rhs.place.root)
                    || !projections_may_overlap(
                        &lhs.place.projection,
                        &rhs.place.projection,
                        &mut model,
                    )
                {
                    continue;
                }
                return Some(OverlapWitness {
                    left: lhs.place.clone(),
                    right: rhs.place.clone(),
                    model,
                });
            }
        }
        None
    }

    pub(crate) fn intersection(&self, other: &Self) -> Self {
        let mut clauses = Vec::new();
        for lhs in &self.clauses {
            for rhs in &other.clauses {
                let rhs = freshen_against(rhs, lhs);
                let Some(mut guard) = lhs.guard.and(&rhs.guard) else {
                    continue;
                };
                if !roots_match(&lhs.place.root, &rhs.place.root)
                    || !projections_may_overlap(
                        &lhs.place.projection,
                        &rhs.place.projection,
                        &mut guard,
                    )
                {
                    continue;
                }
                clauses.push(RegionClause {
                    guard,
                    place: if lhs.place.projection.len() >= rhs.place.projection.len() {
                        lhs.place.clone()
                    } else {
                        rhs.place.clone()
                    },
                });
            }
        }
        Self::normalize(clauses)
    }

    pub(crate) fn provably_covers(&self, other: &Self) -> bool {
        other.clauses.iter().all(|target| {
            self.clauses.iter().any(|container| {
                roots_match(&container.place.root, &target.place.root)
                    && projection_covers(
                        &container.place.projection,
                        &target.place.projection,
                        &target.guard,
                    )
                    && target.guard.implies(&container.guard)
            })
        })
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.clauses.is_empty()
    }

    pub(crate) fn guarded_places(&self) -> impl Iterator<Item = (&Guard, &SymbolicPlace<'db>)> {
        self.clauses
            .iter()
            .map(|clause| (&clause.guard, &clause.place))
    }

    pub(crate) fn clauses(&self) -> impl Iterator<Item = Self> + '_ {
        self.clauses.iter().cloned().map(|clause| Self {
            clauses: vec![clause],
        })
    }

    pub(crate) fn has_root(&self, root: &RegionRoot<'db>) -> bool {
        self.clauses.iter().any(|clause| &clause.place.root == root)
    }

    fn normalize(clauses: Vec<RegionClause<'db>>) -> Self {
        let mut clauses = clauses
            .into_iter()
            .filter(|clause| clause.guard.satisfiable())
            .map(alpha_normalize_clause)
            .collect::<Vec<_>>();
        clauses.sort_by(|lhs, rhs| {
            root_sort_key(&lhs.place.root)
                .cmp(&root_sort_key(&rhs.place.root))
                .then_with(|| lhs.place.projection.cmp(&rhs.place.projection))
                .then_with(|| lhs.guard.cmp(&rhs.guard))
        });
        clauses.dedup();

        let mut normalized: Vec<RegionClause<'db>> = Vec::new();
        for clause in clauses {
            if normalized.iter().any(|existing| {
                existing.place == clause.place && clause.guard.implies(&existing.guard)
            }) {
                continue;
            }
            normalized.retain(|existing| {
                existing.place != clause.place || !existing.guard.implies(&clause.guard)
            });
            normalized.push(clause);
        }
        Self {
            clauses: normalized,
        }
    }
}

fn freshen_against<'db>(
    clause: &RegionClause<'db>,
    other: &RegionClause<'db>,
) -> RegionClause<'db> {
    let offset = existential_ids(other)
        .into_iter()
        .map(|id| id.0)
        .max()
        .and_then(|id| id.checked_add(1))
        .unwrap_or(0);
    let mut subst = IndexSubst::new();
    for id in existential_ids(clause) {
        subst.insert(
            IndexExpr::Existential(id),
            IndexExpr::Existential(ExistentialId(
                offset.checked_add(id.0).expect("existential id overflow"),
            )),
        );
    }
    RegionClause {
        guard: clause
            .guard
            .substitute(&subst)
            .expect("alpha-renaming preserves satisfiability"),
        place: clause.place.substitute(&subst),
    }
}

fn existential_ids(clause: &RegionClause<'_>) -> Vec<ExistentialId> {
    let mut ordered = Vec::new();
    if let RegionRoot::ParamCapability { slot, .. } = &clause.place.root {
        collect_slot_existentials(slot, &mut ordered);
    }
    for projection in &clause.place.projection {
        if let RegionProjection::Index(IndexExpr::Existential(id)) = projection
            && !ordered.contains(id)
        {
            ordered.push(*id);
        }
    }
    for id in clause.guard.existential_ids() {
        if !ordered.contains(&id) {
            ordered.push(id);
        }
    }
    ordered
}

fn roots_match(lhs: &RegionRoot<'_>, rhs: &RegionRoot<'_>) -> bool {
    lhs == rhs
}

fn projections_may_overlap(
    lhs: &[RegionProjection],
    rhs: &[RegionProjection],
    guard: &mut Guard,
) -> bool {
    for (lhs, rhs) in lhs.iter().zip(rhs) {
        match (lhs, rhs) {
            (RegionProjection::Field(lhs), RegionProjection::Field(rhs)) => {
                if lhs != rhs {
                    return false;
                }
            }
            (
                RegionProjection::VariantField {
                    variant: lhs_variant,
                    field: lhs_field,
                },
                RegionProjection::VariantField {
                    variant: rhs_variant,
                    field: rhs_field,
                },
            ) => {
                if lhs_variant != rhs_variant || lhs_field != rhs_field {
                    return false;
                }
            }
            (RegionProjection::Index(lhs), RegionProjection::Index(rhs)) => {
                let Some(combined) = guard.with_equality(*lhs, *rhs) else {
                    return false;
                };
                *guard = combined;
            }
            _ => return false,
        }
    }
    true
}

fn projection_covers(
    container: &[RegionProjection],
    target: &[RegionProjection],
    guard: &Guard,
) -> bool {
    container.len() <= target.len()
        && container
            .iter()
            .zip(target)
            .all(|(container, target)| match (container, target) {
                (RegionProjection::Field(lhs), RegionProjection::Field(rhs)) => lhs == rhs,
                (
                    RegionProjection::VariantField {
                        variant: lhs_variant,
                        field: lhs_field,
                    },
                    RegionProjection::VariantField {
                        variant: rhs_variant,
                        field: rhs_field,
                    },
                ) => lhs_variant == rhs_variant && lhs_field == rhs_field,
                (RegionProjection::Index(lhs), RegionProjection::Index(rhs)) => {
                    guard.proves_equal(*lhs, *rhs)
                }
                _ => false,
            })
}

fn alpha_normalize_clause<'db>(clause: RegionClause<'db>) -> RegionClause<'db> {
    let mut subst = IndexSubst::new();
    for (next, old) in existential_ids(&clause).into_iter().enumerate() {
        subst.insert(
            IndexExpr::Existential(old),
            IndexExpr::Existential(ExistentialId(next as u32)),
        );
    }
    RegionClause {
        guard: clause
            .guard
            .substitute(&subst)
            .expect("alpha-renaming preserves satisfiability"),
        place: clause.place.substitute(&subst),
    }
}

fn collect_slot_existentials(path: &SlotPath<IndexExpr>, out: &mut Vec<ExistentialId>) {
    for projection in path.as_slice() {
        if let SlotProjection::Index(IndexExpr::Existential(id)) = projection
            && !out.contains(id)
        {
            out.push(*id);
        }
    }
}

fn substitute_slot_path(path: &SlotPath<IndexExpr>, subst: &IndexSubst) -> SlotPath<IndexExpr> {
    SlotPath::from_steps(path.as_slice().iter().map(|projection| match projection {
        SlotProjection::Field(field) => SlotProjection::Field(*field),
        SlotProjection::VariantField { variant, field } => SlotProjection::VariantField {
            variant: *variant,
            field: *field,
        },
        SlotProjection::Index(index) => SlotProjection::Index(subst.apply(*index)),
    }))
}

fn root_sort_key(root: &RegionRoot<'_>) -> (u8, u32) {
    match root {
        RegionRoot::ParamPlace(param) => (0, *param),
        RegionRoot::ParamCapability { param, .. } => (1, *param),
        RegionRoot::Local(local) => (2, local.as_u32()),
        RegionRoot::Provider(provider) => (3, provider.provider_idx),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn local(
        index: u32,
        projection: impl IntoIterator<Item = RegionProjection>,
    ) -> RegionSet<'static> {
        RegionSet::singleton(SymbolicPlace::new(
            RegionRoot::Local(SLocalId::from_u32(index)),
            projection,
        ))
    }

    #[test]
    fn distinct_constant_indices_are_disjoint() {
        let left = local(0, [RegionProjection::Index(IndexExpr::Const(0))]);
        let right = local(0, [RegionProjection::Index(IndexExpr::Const(1))]);

        assert!(left.may_overlap(&right).is_none());
    }

    #[test]
    fn dynamic_index_overlap_produces_an_equality_model() {
        let dynamic = IndexExpr::Runtime(SLocalId::from_u32(1));
        let left = local(0, [RegionProjection::Index(dynamic)]);
        let right = local(0, [RegionProjection::Index(IndexExpr::Const(1))]);
        let witness = left.may_overlap(&right).expect("indices may be equal");

        assert!(witness.model.proves_equal(dynamic, IndexExpr::Const(1)));
    }

    #[test]
    fn incompatible_guards_prove_regions_disjoint() {
        let index = IndexExpr::Runtime(SLocalId::from_u32(1));
        let place = SymbolicPlace::new(
            RegionRoot::Local(SLocalId::from_u32(0)),
            [RegionProjection::Index(index)],
        );
        let left = RegionSet::from_clause(
            Guard::equal(index, IndexExpr::Const(0)).expect("valid guard"),
            place.clone(),
        );
        let right = RegionSet::from_clause(
            Guard::equal(index, IndexExpr::Const(1)).expect("valid guard"),
            place,
        );

        assert!(left.may_overlap(&right).is_none());
    }

    #[test]
    fn coverage_requires_proof_of_the_index_relation() {
        let index = IndexExpr::Runtime(SLocalId::from_u32(1));
        let symbolic = local(0, [RegionProjection::Index(index)]);
        let exact = local(0, [RegionProjection::Index(IndexExpr::Const(0))]);
        let constrained =
            exact.with_guard(&Guard::equal(index, IndexExpr::Const(0)).expect("valid equality"));

        assert!(!symbolic.provably_covers(&exact));
        assert!(symbolic.provably_covers(&constrained));
    }

    #[test]
    fn guarded_intersection_can_be_covered_by_partial_suspension() {
        let index = IndexExpr::LoanParam(super::super::guard::IndexParamId(0));
        let base = RegionSet::from_clause(
            Guard::bounded(index, 2).expect("nonempty array"),
            SymbolicPlace::new(RegionRoot::Local(SLocalId::from_u32(0)), []),
        );
        let access = local(0, []);
        let suspended =
            base.with_guard(&Guard::equal(index, IndexExpr::Const(0)).expect("valid member"));
        let overlap = base.intersection(&access);

        assert!(!overlap.is_empty());
        assert!(!suspended.provably_covers(&overlap));
        assert!(suspended.provably_covers(
            &overlap.with_guard(&Guard::equal(index, IndexExpr::Const(0)).expect("valid member"))
        ));
    }

    #[test]
    fn distinct_enum_variants_are_disjoint() {
        let left = local(
            0,
            [RegionProjection::VariantField {
                variant: VariantIndex(0),
                field: FieldIndex(0),
            }],
        );
        let right = local(
            0,
            [RegionProjection::VariantField {
                variant: VariantIndex(1),
                field: FieldIndex(0),
            }],
        );

        assert!(left.intersection(&right).is_empty());
        assert!(left.may_overlap(&right).is_none());
    }

    #[test]
    fn existential_normalization_uses_place_occurrence_order() {
        let left = local(
            0,
            [RegionProjection::Index(IndexExpr::Existential(
                ExistentialId(9),
            ))],
        );
        let right = local(
            0,
            [RegionProjection::Index(IndexExpr::Existential(
                ExistentialId(2),
            ))],
        );

        assert_eq!(left, right);
    }

    #[test]
    fn separate_clauses_do_not_share_existential_constraints() {
        let existential = IndexExpr::Existential(ExistentialId(0));
        let place = SymbolicPlace::new(RegionRoot::Local(SLocalId::from_u32(0)), []);
        let left = RegionSet::from_clause(
            Guard::equal(existential, IndexExpr::Const(0)).expect("valid guard"),
            place.clone(),
        );
        let right = RegionSet::from_clause(
            Guard::equal(existential, IndexExpr::Const(1)).expect("valid guard"),
            place,
        );

        assert!(left.may_overlap(&right).is_some());
    }
}
