//! Canonical guarded referent regions. Structural slots and storage paths are distinct.
use std::{
    cmp::{Ordering, min},
    collections::{BTreeMap, BTreeSet},
};

use super::{
    external::{ExternalOrigin, ExternalSource, ReferentContract},
    guard::{Guard, ValueOccurrence},
    handle::HandleAddressSpace,
    index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
    path::{Projection, RegionPath},
    repack::{ReferentRepackId, ReferentViews},
    value::Guarded,
};
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::normalized::{NRootId, NValueId},
    },
    semantic::ProviderBinding,
};

/// Interning uses the complete binding, including its source and semantic contract.
#[salsa::interned]
#[derive(Debug)]
pub struct ProviderRegionId<'db> {
    #[return_ref]
    pub binding: ProviderBinding<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RegionRoot<'db> {
    External(ExternalSource<'db>),
    Root {
        root: NRootId,
        contract: ReferentContract<'db>,
    },
    /// An SSA holder is a logical representation and move location, never
    /// addressable storage. Copies can read its structural fields directly.
    Value(NValueId),
}

impl<'db> RegionRoot<'db> {
    pub fn address_space(&self) -> HandleAddressSpace<'db> {
        self.contract()
            .map_or(HandleAddressSpace::Unspecified, |contract| {
                contract.address_space
            })
    }

    pub fn contract(&self) -> Option<ReferentContract<'db>> {
        match self {
            Self::External(source) => Some(source.contract),
            Self::Root { contract, .. } => Some(*contract),
            Self::Value(_) => None,
        }
    }

    pub fn is_reachable(&self) -> bool {
        matches!(self, Self::External(source) if source.is_reachable())
    }

    pub fn may_alias_unknown(&self, other: &Self) -> bool {
        if matches!((self, other), (Self::Root { .. }, Self::External(source)) | (Self::External(source), Self::Root { .. }) if source.is_incoming())
        {
            return false;
        }
        (matches!(self, Self::External(source) if source.uncertain() || (matches!(other, Self::External(_)) && matches!(source.origin, ExternalOrigin::Memory { .. })))
            || matches!(other, Self::External(source) if source.uncertain() || (matches!(self, Self::External(_)) && matches!(source.origin, ExternalOrigin::Memory { .. }))))
            && self
                .contract()
                .zip(other.contract())
                .is_some_and(|(left, right)| left.may_alias(right))
    }

    pub fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>> + '_ {
        match self {
            Self::External(source) => Some(source),
            _ => None,
        }
        .into_iter()
        .flat_map(ExternalSource::indices)
    }

    pub fn substitute(&self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        match self {
            Self::External(source) => Self::External(source.substitute(db, subst)),
            Self::Root { root, contract } => Self::Root {
                root: *root,
                contract: contract.substitute(db, subst),
            },
            Self::Value(_) => self.clone(),
        }
    }

    fn rename_indices(&self, subst: &IndexSubst<'db>) -> Self {
        match self {
            Self::External(source) => Self::External(source.rename_indices(subst)),
            Self::Root { .. } | Self::Value(_) => self.clone(),
        }
    }

    fn alias_guard(
        &self,
        other: &Self,
        guard: Guard<'db>,
        allow_unknown: bool,
    ) -> Option<Guard<'db>> {
        match (self, other) {
            (Self::External(left), Self::External(right)) => {
                left.alias_guard(right, guard, allow_unknown)
            }
            _ => {
                (self == other || (allow_unknown && self.may_alias_unknown(other))).then_some(guard)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolicPlace<'db> {
    pub root: RegionRoot<'db>,
    pub path: RegionPath<IndexExpr<'db>>,
    pub views: ReferentViews<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegionSet<'db> {
    scope: BinderScope,
    clauses: Box<[Guarded<'db, SymbolicPlace<'db>>]>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OverlapResult<'db> {
    Disjoint,
    Overlap(RegionSet<'db>),
    Unknown,
}

impl<'db> RegionSet<'db> {
    pub fn empty(scope: &BinderScope) -> Self {
        Self {
            scope: scope.clone(),
            clauses: Box::new([]),
        }
    }

    pub fn singleton(
        scope: &BinderScope,
        root: RegionRoot<'db>,
        path: RegionPath<IndexExpr<'db>>,
    ) -> Self {
        Self::new(
            scope,
            [Guarded {
                guard: Guard::always(scope),
                payload: SymbolicPlace {
                    root,
                    path,
                    views: ReferentViews::default(),
                },
            }],
        )
    }

    pub fn new(
        scope: &BinderScope,
        clauses: impl IntoIterator<Item = Guarded<'db, SymbolicPlace<'db>>>,
    ) -> Self {
        let mut canonical = BTreeMap::<(BinderScope, SymbolicPlace<'db>), Guard<'db>>::new();
        for mut clause in clauses {
            let substitution = clause.guard.scope().canonical_existentials(
                scope,
                clause
                    .guard
                    .indices()
                    .into_iter()
                    .chain(clause.payload.path.indices())
                    .chain(clause.payload.root.indices()),
            );
            clause.guard = clause
                .guard
                .substitute(&substitution)
                .expect("clause alpha normalization");
            clause.payload.root = clause.payload.root.rename_indices(&substitution);
            clause.payload.path = clause.payload.path.substitute(&substitution);
            for index in clause
                .payload
                .path
                .indices()
                .chain(clause.payload.root.indices())
            {
                clause
                    .guard
                    .scope()
                    .validate(index)
                    .expect("free region binder");
            }
            canonical
                .entry((clause.guard.scope().clone(), clause.payload))
                .and_modify(|guard| *guard = guard.or(&clause.guard))
                .or_insert(clause.guard);
        }
        Self {
            scope: scope.clone(),
            clauses: canonical
                .into_iter()
                .map(|((_, payload), guard)| Guarded { guard, payload })
                .collect(),
        }
    }

    pub fn scope(&self) -> &BinderScope {
        &self.scope
    }
    pub fn clauses(&self) -> &[Guarded<'db, SymbolicPlace<'db>>] {
        &self.clauses
    }
    pub fn forget_occurrences(&self, repeated: impl Fn(ValueOccurrence) -> bool + Copy) -> Self {
        Self::new(
            &self.scope,
            self.clauses.iter().map(|clause| Guarded {
                guard: clause.guard.forget_occurrences(repeated),
                payload: clause.payload.clone(),
            }),
        )
    }

    /// Previous executions own fresh witnesses, independent of the next execution.
    pub fn forget_iteration(
        &self,
        db: &'db dyn HirAnalysisDb,
        repeated: impl Fn(IndexExpr<'db>) -> bool + Copy,
        occurrence: impl Fn(ValueOccurrence) -> bool + Copy,
    ) -> Self {
        Self::new(
            &self.scope,
            self.clauses.iter().filter_map(|clause| {
                let guard = clause.guard.forget_occurrences(occurrence);
                let mut scope = guard.scope().clone();
                let indices: BTreeSet<_> = guard
                    .indices()
                    .into_iter()
                    .chain(clause.payload.root.indices())
                    .chain(clause.payload.path.indices())
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
                    .expect("previous iteration witnesses");
                Some(Guarded {
                    guard: guard.substitute(&subst)?,
                    payload: SymbolicPlace {
                        root: clause.payload.root.substitute(db, &subst),
                        path: clause.payload.path.substitute(&subst),
                        views: clause.payload.views.substitute(db, &subst),
                    },
                })
            }),
        )
    }

    pub fn is_empty(&self) -> bool {
        self.clauses.is_empty()
    }

    pub fn indices(&self) -> BTreeSet<IndexExpr<'db>> {
        self.clauses
            .iter()
            .flat_map(|clause| {
                clause
                    .guard
                    .indices()
                    .into_iter()
                    .chain(clause.payload.path.indices())
                    .chain(clause.payload.root.indices())
            })
            .filter(|index| self.scope.validate(*index).is_ok())
            .collect()
    }

    pub fn union(&self, other: &Self) -> Self {
        assert_eq!(self.scope, other.scope, "region scopes must match");
        Self::new(
            &self.scope,
            self.clauses.iter().chain(other.clauses.iter()).cloned(),
        )
    }

    pub fn with_guard(&self, guard: &Guard<'db>) -> Self {
        Self::new(
            &self.scope,
            self.clauses.iter().filter_map(|clause| {
                Some(Guarded {
                    guard: clause.guard.and(&guard.in_scope(clause.guard.scope()))?,
                    payload: clause.payload.clone(),
                })
            }),
        )
    }

    pub fn repack(&self, db: &'db dyn HirAnalysisDb, repack: ReferentRepackId<'db>) -> Self {
        Self::new(
            &self.scope,
            self.clauses.iter().map(|clause| {
                let mut clause = clause.clone();
                clause
                    .payload
                    .views
                    .append(db, clause.payload.path.as_slice().len(), repack);
                clause
            }),
        )
    }

    pub fn with_relative_views(
        &self,
        db: &'db dyn HirAnalysisDb,
        views: &ReferentViews<'db>,
        source_path_len: usize,
    ) -> Self {
        Self::new(
            &self.scope,
            self.clauses.iter().map(|clause| {
                let mut clause = clause.clone();
                let offset = clause
                    .payload
                    .path
                    .as_slice()
                    .len()
                    .checked_sub(source_path_len)
                    .expect("instantiated source retains its final projection");
                clause.payload.views.extend_at(db, views, offset);
                clause
            }),
        )
    }

    pub fn project(&self, path: &RegionPath<IndexExpr<'db>>) -> Self {
        Self::new(
            &self.scope,
            self.clauses.iter().map(|clause| Guarded {
                guard: clause.guard.clone(),
                payload: SymbolicPlace {
                    root: clause.payload.root.clone(),
                    views: clause.payload.views.clone(),
                    path: RegionPath::new(
                        clause
                            .payload
                            .path
                            .as_slice()
                            .iter()
                            .chain(path.as_slice())
                            .copied()
                            .collect::<Vec<_>>(),
                    ),
                },
            }),
        )
    }

    pub fn substitute(&self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        assert_eq!(
            self.scope(),
            subst.source(),
            "region substitution scope mismatch"
        );
        Self::new(
            subst.destination(),
            self.clauses.iter().filter_map(|clause| {
                let subst = subst.under_existentials(clause.guard.scope());
                Some(Guarded {
                    guard: clause.guard.substitute(&subst)?,
                    payload: SymbolicPlace {
                        root: clause.payload.root.substitute(db, &subst),
                        views: clause.payload.views.substitute(db, &subst),
                        path: clause.payload.path.substitute(&subst),
                    },
                })
            }),
        )
    }

    pub fn close_existentials(&self, scope: &BinderScope) -> Self {
        Self::new(scope, self.clauses.iter().cloned())
    }

    /// Conservatively intersect regions. Unknown enum overlays retain both paths:
    /// choosing one could later turn uncertainty into a false coverage proof.
    pub fn intersection(&self, other: &Self) -> Self {
        self.intersect(other).0
    }

    pub fn intersect(&self, other: &Self) -> (Self, bool) {
        self.intersect_with_unknown(other, true)
    }

    pub fn proven_intersection(&self, other: &Self) -> Self {
        self.intersect_with_unknown(other, false).0
    }

    fn intersect_with_unknown(&self, other: &Self, allow_unknown: bool) -> (Self, bool) {
        assert_eq!(self.scope, other.scope, "region scopes must match");
        let mut clauses = Vec::new();
        let mut uncertain = false;
        for left in &self.clauses {
            for right in &other.clauses {
                let left_subst = left
                    .guard
                    .scope()
                    .open_existentials(&self.scope, &self.scope);
                let right_subst = right
                    .guard
                    .scope()
                    .open_existentials(&self.scope, left_subst.destination());
                let left_subst = left_subst
                    .then(
                        &IndexSubst::new(left_subst.destination(), right_subst.destination(), [])
                            .expect("combined witness scope"),
                    )
                    .expect("fresh witnesses");
                let left = substitute_clause(left, &left_subst);
                let right = substitute_clause(right, &right_subst);
                // Distinct raw-handle occurrences can name overlapping bases.
                // Their field paths cannot prove disjointness without base identity.
                if allow_unknown
                    && left.payload.root.may_alias_unknown(&right.payload.root)
                    && (left.payload.root != right.payload.root || left.payload.root.is_reachable())
                {
                    if let Some(guard) = left.guard.and(&right.guard).and_then(|guard| {
                        left.payload
                            .root
                            .alias_guard(&right.payload.root, guard, allow_unknown)
                    }) {
                        uncertain = true;
                        clauses.push(Guarded {
                            guard: guard.clone(),
                            payload: left.payload.clone(),
                        });
                        clauses.push(Guarded {
                            guard,
                            payload: right.payload.clone(),
                        });
                    }
                    continue;
                }
                let Some(guard) = left
                    .guard
                    .and(&right.guard)
                    .and_then(|guard| {
                        left.payload
                            .root
                            .alias_guard(&right.payload.root, guard, allow_unknown)
                    })
                    .and_then(|guard| {
                        path_alias_guard(
                            left.payload.path.as_slice(),
                            right.payload.path.as_slice(),
                            guard,
                            allow_unknown,
                        )
                    })
                else {
                    continue;
                };
                let exact = left
                    .payload
                    .root
                    .alias_guard(&right.payload.root, guard.clone(), false)
                    .is_some()
                    && path_alias_guard(
                        left.payload.path.as_slice(),
                        right.payload.path.as_slice(),
                        guard.clone(),
                        false,
                    )
                    .is_some();
                if exact {
                    let left_len = left.payload.path.as_slice().len();
                    let right_len = right.payload.path.as_slice().len();
                    let payload = match left_len.cmp(&right_len) {
                        Ordering::Less => &right.payload,
                        Ordering::Greater => &left.payload,
                        Ordering::Equal => min(&left.payload, &right.payload),
                    };
                    clauses.push(Guarded {
                        guard,
                        payload: payload.clone(),
                    });
                } else {
                    uncertain = true;
                    clauses.push(Guarded {
                        guard: guard.clone(),
                        payload: left.payload.clone(),
                    });
                    clauses.push(Guarded {
                        guard,
                        payload: right.payload.clone(),
                    });
                }
            }
        }
        (Self::new(&self.scope, clauses), uncertain)
    }

    pub fn overlap(&self, other: &Self) -> OverlapResult<'db> {
        let (overlap, uncertain) = self.intersect(other);
        if uncertain {
            OverlapResult::Unknown
        } else if overlap.is_empty() {
            OverlapResult::Disjoint
        } else {
            OverlapResult::Overlap(overlap)
        }
    }

    /// Coverage requires an exact root and a proven prefix for every clause.
    /// Separate guards for the same covering region may collectively cover a clause.
    pub fn provably_covers(&self, other: &Self) -> bool {
        assert_eq!(self.scope, other.scope, "region scopes must match");
        other.clauses.iter().all(|right| {
            let coverage = self
                .clauses
                .iter()
                .filter_map(|left| {
                    // An existential witness establishes possible overlap, not
                    // universal coverage of another occurrence.
                    if left.guard.scope() != &self.scope {
                        return None;
                    }
                    let guard = left.guard.in_scope(right.guard.scope()).and(&right.guard)?;
                    let guard = left
                        .payload
                        .root
                        .alias_guard(&right.payload.root, guard, false)?;
                    if left.payload.path.as_slice().len() > right.payload.path.as_slice().len() {
                        return None;
                    }
                    path_alias_guard(
                        left.payload.path.as_slice(),
                        right.payload.path.as_slice(),
                        guard,
                        false,
                    )
                })
                .reduce(|left, right| left.or(&right));
            coverage.is_some_and(|coverage| right.guard.implies(&coverage))
        })
    }

    /// Forget covered portions of a moved region after a definite write. A partial
    /// field write cannot reinitialize an entire moved aggregate.
    pub fn remove_covered(&self, written: &Self) -> Self {
        assert_eq!(self.scope, written.scope, "region scopes must match");
        Self::new(
            &self.scope,
            self.clauses.iter().filter_map(|moved| {
                let mut remaining = Some(moved.guard.clone());
                for write in &written.clauses {
                    if write.guard.scope() != &self.scope
                        || write.payload.path.as_slice().len() > moved.payload.path.as_slice().len()
                    {
                        continue;
                    }
                    let guard = write
                        .payload
                        .root
                        .alias_guard(
                            &moved.payload.root,
                            write.guard.in_scope(moved.guard.scope()),
                            false,
                        )
                        .and_then(|guard| {
                            path_alias_guard(
                                write.payload.path.as_slice(),
                                moved.payload.path.as_slice(),
                                guard,
                                false,
                            )
                        });
                    if let Some(guard) = guard {
                        remaining = remaining.and_then(|remaining| remaining.difference(&guard));
                    }
                }
                Some(Guarded {
                    guard: remaining?,
                    payload: moved.payload.clone(),
                })
            }),
        )
    }
}

fn substitute_clause<'db>(
    clause: &Guarded<'db, SymbolicPlace<'db>>,
    subst: &IndexSubst<'db>,
) -> Guarded<'db, SymbolicPlace<'db>> {
    Guarded {
        guard: clause
            .guard
            .substitute(subst)
            .expect("fresh witnesses preserve satisfiability"),
        payload: SymbolicPlace {
            root: clause.payload.root.rename_indices(subst),
            views: clause.payload.views.clone(),
            path: clause.payload.path.substitute(subst),
        },
    }
}

pub(super) fn path_alias_guard<'db>(
    left: &[Projection<IndexExpr<'db>>],
    right: &[Projection<IndexExpr<'db>>],
    mut guard: Guard<'db>,
    allow_overlay: bool,
) -> Option<Guard<'db>> {
    for (left, right) in left.iter().zip(right) {
        match (left, right) {
            (Projection::Index(left), Projection::Index(right)) => {
                guard = guard.with_equality(*left, *right)?
            }
            (Projection::Field(left), Projection::Field(right)) if left != right => return None,
            (
                Projection::VariantField {
                    variant: left_variant,
                    field: left_field,
                },
                Projection::VariantField {
                    variant: right_variant,
                    field: right_field,
                },
            ) => {
                if left_variant != right_variant {
                    return allow_overlay.then_some(guard);
                }
                if left_field != right_field {
                    return None;
                }
            }
            (left, right) if left != right => return allow_overlay.then_some(guard),
            _ => {}
        }
    }
    Some(guard)
}

#[cfg(test)]
mod tests {
    use crate::analysis::semantic::capability::external::ExternalSource;
    use crate::analysis::semantic::capability::source::InputSource;
    use crate::analysis::semantic::capability::test_roots;
    use crate::analysis::ty::ProviderAddressSpace;

    use super::*;
    use crate::analysis::semantic::capability::path::StructuralPath;
    use crate::{
        analysis::{
            semantic::{FieldIndex, VariantIndex},
            ty::{
                provider::{
                    ProviderKind, ProviderLayoutEvidence, ProviderSemantics, ProviderTransport,
                },
                ty_check::EffectParamSite,
                ty_def::TyId,
            },
        },
        hir_def::ItemKind,
        semantic::ProviderSource,
        test_db::HirAnalysisTestDb,
    };

    fn index<'db>(value: u32) -> IndexExpr<'db> {
        IndexExpr::Runtime(NValueId::from_u32(value))
    }
    fn path<'db>(index: IndexExpr<'db>) -> RegionPath<IndexExpr<'db>> {
        RegionPath::new([Projection::Index(index)])
    }
    fn region<'db>(root: RegionRoot<'db>, path: RegionPath<IndexExpr<'db>>) -> RegionSet<'db> {
        RegionSet::singleton(&BinderScope::default(), root, path)
    }

    #[test]
    fn region_unions_use_complete_root_identity_and_obey_lattice_laws() {
        let db = HirAnalysisTestDb::default();
        let roots = [
            RegionRoot::External(test_roots::input(&db, InputSource::place(0))),
            RegionRoot::External(test_roots::input(&db, InputSource::place(1))),
            RegionRoot::External(test_roots::input(
                &db,
                InputSource::slot(0, StructuralPath::new([Projection::Field(FieldIndex(0))])),
            )),
            RegionRoot::External(test_roots::input(
                &db,
                InputSource::slot(0, StructuralPath::new([Projection::Field(FieldIndex(1))])),
            )),
            test_roots::local(&db, NRootId::from_u32(0)),
            RegionRoot::Value(NValueId::from_u32(0)),
        ];
        let regions: Vec<_> = roots
            .iter()
            .flat_map(|root| {
                [
                    region(root.clone(), RegionPath::default()),
                    region(root.clone(), path(index(0))),
                ]
            })
            .collect();
        for left in &regions {
            assert_eq!(left.union(left), *left);
            for right in &regions {
                assert_eq!(left.union(right), right.union(left));
                assert_eq!(
                    left.intersection(right).is_empty(),
                    right.intersection(left).is_empty()
                );
                for third in &regions {
                    assert_eq!(
                        left.union(right).union(third),
                        left.union(&right.union(third))
                    );
                }
            }
        }
    }

    #[test]
    fn symbolic_slot_and_referent_indices_share_one_constraint_solver() {
        let db = HirAnalysisTestDb::default();
        let root = |selector| {
            RegionRoot::External(test_roots::input(
                &db,
                InputSource::slot(0, StructuralPath::new([Projection::Index(selector)])),
            ))
        };
        let left = region(root(index(0)), path(index(1)));
        let right = region(root(index(1)), path(IndexExpr::Const(0)));
        let guard = Guard::always(&BinderScope::default())
            .with_disequality(index(0), IndexExpr::Const(0))
            .unwrap();
        assert!(left.with_guard(&guard).intersection(&right).is_empty());
        let overlapping = left.intersection(&right);
        assert!(!overlapping.is_empty());
        assert!(
            overlapping
                .clauses()
                .iter()
                .all(|clause| clause.guard.proves_equal(index(0), IndexExpr::Const(0)))
        );
    }

    #[test]
    fn coverage_and_reinitialization_preserve_disjoint_members() {
        let db = HirAnalysisTestDb::default();
        let root = test_roots::local(&db, NRootId::from_u32(0));
        let all = region(root.clone(), RegionPath::default());
        let zero = region(root.clone(), path(IndexExpr::Const(0)));
        let one = region(root.clone(), path(IndexExpr::Const(1)));
        let dynamic = region(root, path(index(0)));
        assert!(all.provably_covers(&dynamic));
        assert!(!zero.provably_covers(&dynamic));
        assert!(!zero.provably_covers(&one));
        assert_eq!(zero.union(&one).remove_covered(&zero), one);
        let remaining = dynamic.remove_covered(&zero);
        assert!(remaining.intersection(&zero).is_empty());
        assert!(!remaining.intersection(&one).is_empty());
        assert_eq!(all.remove_covered(&zero), all);
        for selected in [0, 1, 2] {
            let subst = IndexSubst::new(
                &BinderScope::default(),
                &BinderScope::default(),
                [(index(0), IndexExpr::Const(selected))],
            )
            .unwrap();
            assert_eq!(remaining.substitute(&db, &subst).is_empty(), selected == 0);
        }
    }

    #[test]
    fn enum_storage_overlap_never_establishes_coverage() {
        let db = HirAnalysisTestDb::default();
        let root = test_roots::local(&db, NRootId::from_u32(0));
        let field = |variant| {
            region(
                root.clone(),
                RegionPath::new([Projection::VariantField {
                    variant: VariantIndex(variant),
                    field: FieldIndex(0),
                }]),
            )
        };
        let uncertain = field(0).intersection(&field(1));
        assert_eq!(field(0).overlap(&field(1)), OverlapResult::Unknown);
        assert_eq!(uncertain, field(1).intersection(&field(0)));
        assert!(!uncertain.is_empty());
        assert!(!field(0).provably_covers(&uncertain));
        assert!(!field(1).provably_covers(&uncertain));
        assert!(!field(0).provably_covers(&field(1)));
        assert_eq!(field(0).remove_covered(&field(1)), field(0));
    }

    #[test]
    fn provider_regions_distinguish_complete_bindings_with_the_same_provider_index() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("provider_roots.fe".into(), "fn inspect() {}");
        let (top_mod, _) = db.top_mod(file);
        let func = top_mod
            .all_items(&db)
            .iter()
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(*func),
                _ => None,
            })
            .unwrap();
        let ty = TyId::u256(&db);
        let binding = ProviderBinding {
            provider_idx: 0,
            provider_ty: ty,
            is_mut: true,
            source: ProviderSource::UsesParam {
                site: EffectParamSite::Func(func),
                requirement_idx: 0,
            },
            semantics: ProviderSemantics {
                provider_ty: ty,
                kind: ProviderKind::RootObject,
                address_space: Some(ProviderAddressSpace::Storage),
                target_ty: Some(ty),
                transport: ProviderTransport::ByPlace,
                evidence: ProviderLayoutEvidence::NotHandle,
            },
            layout_env: None,
        };
        let mut distinct_source = binding.clone();
        distinct_source.source = ProviderSource::UsesParam {
            site: EffectParamSite::Func(func),
            requirement_idx: 1,
        };
        let mut distinct_contract = binding.clone();
        distinct_contract.semantics.address_space = Some(ProviderAddressSpace::Transient);
        let regions: Vec<_> = [binding, distinct_source, distinct_contract]
            .into_iter()
            .map(|binding| {
                region(
                    RegionRoot::External(ExternalSource::provider(
                        &db,
                        ProviderRegionId::new(&db, binding),
                        TyId::u256(&db),
                    )),
                    RegionPath::default(),
                )
            })
            .collect();
        let union = regions[0].union(&regions[1]).union(&regions[2]);
        assert_eq!(union.clauses().len(), 3);
        assert_eq!(union, regions[2].union(&regions[1]).union(&regions[0]));
        for (index, left) in regions.iter().enumerate() {
            for right in regions.iter().skip(index + 1) {
                assert_ne!(left, right);
                assert!(!left.provably_covers(right));
            }
        }
    }
}
