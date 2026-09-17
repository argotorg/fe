//! Canonical guarded referent regions. Structural slots and storage paths are distinct.
use std::{
    cmp::{Ordering, min},
    collections::{BTreeMap, BTreeSet},
};

use super::{
    guard::Guard,
    index::{BinderScope, IndexExpr, IndexSubst},
    path::{Projection, RegionPath, StructuralPath},
    value::Guarded,
};
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::normalized::{NRootId, NValueId},
        ty::ProviderAddressSpace,
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
    ParamPlace(u32),
    ParamSlot {
        param: u32,
        slot: StructuralPath<IndexExpr<'db>>,
    },
    Root(NRootId),
    Value(NValueId),
    Provider(ProviderRegionId<'db>),
}

impl<'db> RegionRoot<'db> {
    pub fn address_space(&self, db: &'db dyn HirAnalysisDb) -> ProviderAddressSpace {
        match self {
            Self::Provider(provider) => provider
                .binding(db)
                .semantics
                .address_space
                .unwrap_or(ProviderAddressSpace::Memory),
            Self::ParamPlace(_) | Self::ParamSlot { .. } | Self::Root(_) | Self::Value(_) => {
                ProviderAddressSpace::Memory
            }
        }
    }

    fn substitute(&self, subst: &IndexSubst<'db>) -> Self {
        match self {
            Self::ParamSlot { param, slot } => Self::ParamSlot {
                param: *param,
                slot: slot.substitute(subst),
            },
            Self::ParamPlace(_) | Self::Root(_) | Self::Value(_) | Self::Provider(_) => {
                self.clone()
            }
        }
    }

    fn alias_guard(&self, other: &Self, guard: Guard<'db>) -> Option<Guard<'db>> {
        match (self, other) {
            (
                Self::ParamSlot {
                    param: left,
                    slot: left_slot,
                },
                Self::ParamSlot {
                    param: right,
                    slot: right_slot,
                },
            ) if left == right && left_slot.as_slice().len() == right_slot.as_slice().len() => {
                path_alias_guard(left_slot.as_slice(), right_slot.as_slice(), guard, false)
            }
            _ => (self == other).then_some(guard),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolicPlace<'db> {
    pub root: RegionRoot<'db>,
    pub path: RegionPath<IndexExpr<'db>>,
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
                payload: SymbolicPlace { root, path },
            }],
        )
    }

    pub fn new(
        scope: &BinderScope,
        clauses: impl IntoIterator<Item = Guarded<'db, SymbolicPlace<'db>>>,
    ) -> Self {
        let mut canonical = BTreeMap::<SymbolicPlace<'db>, Guard<'db>>::new();
        for clause in clauses {
            assert_eq!(clause.guard.scope(), scope, "region guard scope mismatch");
            for index in clause
                .payload
                .path
                .indices()
                .chain(match &clause.payload.root {
                    RegionRoot::ParamSlot { slot, .. } => slot.indices().collect::<Vec<_>>(),
                    _ => Vec::new(),
                })
            {
                scope.validate(index).expect("free region binder");
            }
            canonical
                .entry(clause.payload)
                .and_modify(|guard| *guard = guard.or(&clause.guard))
                .or_insert(clause.guard);
        }
        Self {
            scope: scope.clone(),
            clauses: canonical
                .into_iter()
                .map(|(payload, guard)| Guarded { guard, payload })
                .collect(),
        }
    }

    pub fn scope(&self) -> &BinderScope {
        &self.scope
    }
    pub fn clauses(&self) -> &[Guarded<'db, SymbolicPlace<'db>>] {
        &self.clauses
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
                    .chain(match &clause.payload.root {
                        RegionRoot::ParamSlot { slot, .. } => slot.indices().collect::<Vec<_>>(),
                        _ => Vec::new(),
                    })
            })
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
                    guard: clause.guard.and(guard)?,
                    payload: clause.payload.clone(),
                })
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

    pub fn substitute(&self, subst: &IndexSubst<'db>) -> Self {
        assert_eq!(
            self.scope(),
            subst.source(),
            "region substitution scope mismatch"
        );
        Self::new(
            subst.destination(),
            self.clauses.iter().filter_map(|clause| {
                Some(Guarded {
                    guard: clause.guard.substitute(subst)?,
                    payload: SymbolicPlace {
                        root: clause.payload.root.substitute(subst),
                        path: clause.payload.path.substitute(subst),
                    },
                })
            }),
        )
    }

    /// Conservatively intersect regions. Unknown enum overlays retain both paths:
    /// choosing one could later turn uncertainty into a false coverage proof.
    pub fn intersection(&self, other: &Self) -> Self {
        self.intersect(other).0
    }

    fn intersect(&self, other: &Self) -> (Self, bool) {
        assert_eq!(self.scope, other.scope, "region scopes must match");
        let mut clauses = Vec::new();
        let mut uncertain = false;
        for left in &self.clauses {
            for right in &other.clauses {
                let Some(guard) = left
                    .guard
                    .and(&right.guard)
                    .and_then(|guard| left.payload.root.alias_guard(&right.payload.root, guard))
                    .and_then(|guard| {
                        path_alias_guard(
                            left.payload.path.as_slice(),
                            right.payload.path.as_slice(),
                            guard,
                            true,
                        )
                    })
                else {
                    continue;
                };
                let exact = path_alias_guard(
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
                    let guard = left.guard.and(&right.guard)?;
                    let guard = left.payload.root.alias_guard(&right.payload.root, guard)?;
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
                    if write.payload.path.as_slice().len() > moved.payload.path.as_slice().len() {
                        continue;
                    }
                    let guard = write
                        .payload
                        .root
                        .alias_guard(&moved.payload.root, write.guard.clone())
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

fn path_alias_guard<'db>(
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
    use super::*;
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
        let roots = [
            RegionRoot::ParamPlace(0),
            RegionRoot::ParamPlace(1),
            RegionRoot::ParamSlot {
                param: 0,
                slot: StructuralPath::new([Projection::Field(FieldIndex(0))]),
            },
            RegionRoot::ParamSlot {
                param: 0,
                slot: StructuralPath::new([Projection::Field(FieldIndex(1))]),
            },
            RegionRoot::Root(NRootId::from_u32(0)),
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
        let root = |selector| RegionRoot::ParamSlot {
            param: 0,
            slot: StructuralPath::new([Projection::Index(selector)]),
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
        let root = RegionRoot::Root(NRootId::from_u32(0));
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
            assert_eq!(remaining.substitute(&subst).is_empty(), selected == 0);
        }
    }

    #[test]
    fn enum_storage_overlap_never_establishes_coverage() {
        let root = RegionRoot::Root(NRootId::from_u32(0));
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
                    RegionRoot::Provider(ProviderRegionId::new(&db, binding)),
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
