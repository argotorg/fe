//! Separation a borrow's holder owes to its callers, kept as one relation.
//!
//! A body compares its input loans with unresolved accesses only by exact
//! aliasing. The separation it could not prove is owed by every caller, which
//! must establish it physically rather than through authority. The relation
//! keeps the protected referent, the access, and their shared witnesses in one
//! clause: two independently quantified regions would pair alternatives that
//! never occurred together.

use std::collections::{BTreeMap, BTreeSet, btree_map::Entry};

use crate::analysis::{HirAnalysisDb, ty::ty_def::BorrowKind};

use super::{
    footprint::AccessExtent,
    guard::{Guard, ValueOccurrence},
    index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
    path::RegionPath,
    region::{RegionRoot, RegionSet, SymbolicPlace},
    value::Guarded,
};

// Limits are well above the largest measured on the repository workspace and
// the `fe_test` fixtures: 8 pairs per comparison, 26 relations per summary,
// 1287 guard nodes, 6 witnesses and 16 stored nodes per clause. A loop nest
// three deep, with a child borrow live across a read through a stored
// borrow, needs 7432 guard nodes and 7 witnesses.

/// Most jointly opened pairs one comparison may materialize.
pub const SEPARATION_PAIR_LIMIT: usize = 256;
/// Most relations one summary may export.
pub const SEPARATION_CLAUSE_LIMIT: usize = 1024;
/// Most guard nodes in one clause, counting its suspension conditions. A
/// bounded 256-bit index costs up to about a thousand nodes, so this admits as
/// many bounded witnesses as a clause may own.
pub const SEPARATION_GUARD_NODE_LIMIT: usize = 32768;
/// Most witnesses one clause may own.
pub const SEPARATION_WITNESS_LIMIT: u32 = 32;
/// Most nodes one relation may store apart from its guards: the sources of
/// both endpoints with every Memory base and clobber dependency, their
/// projection steps, conversion views and index arguments, the extent and
/// the suspended paths.
pub const SEPARATION_SIZE_LIMIT: usize = 256;

/// On every valuation of its clause guard, the access footprint touches no part
/// of `protected` outside the suspended projections.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Separation<'db> {
    /// A selected referent of a borrow that is live across the access.
    pub protected: SymbolicPlace<'db>,
    pub protected_kind: BorrowKind,
    pub access: SymbolicPlace<'db>,
    pub access_kind: BorrowKind,
    pub extent: AccessExtent<'db>,
    /// Projections of `protected`, relative to it, that the holder's own live
    /// reborrows definitely suspended. The mask is a union, kept as one slice
    /// per path in path order. Guards are must conditions, so no operation may
    /// widen them. Their scope is a prefix of the clause scope: canonically the
    /// relation's own, the owner plus the witnesses the relation observes.
    pub suspended: Box<[Guarded<'db, RegionPath<IndexExpr<'db>>>]>,
}

impl<'db> Separation<'db> {
    /// Certified suspended projections of `protected` under `guard`.
    ///
    /// `suspended` must already be the well-typed suspension recorded for the
    /// active loan occurrence that `protected` selects, lifted with the
    /// substitution that opened `protected`. This certifies only the relative
    /// containment of that evidence, not its origin. Scopes must match without
    /// re-owning: promoting a suspension's own witnesses into the clause scope
    /// would turn a possible suspension into a definite one.
    ///
    /// A suspension clause certifies a slice only when it has no witnesses of
    /// its own, names the same root and conversion views, and its path
    /// syntactically extends the protected path or covers it. A covering
    /// suspension clips to the whole protected place, encoded as an empty
    /// suffix. Everything else is dropped, including equal places spelled
    /// differently (`P[i]` against `P[j]` under `i == j`), which only shrinks
    /// the exclusion.
    pub fn suspension_slices(
        protected: &SymbolicPlace<'db>,
        suspended: &RegionSet<'db>,
        guard: &Guard<'db>,
    ) -> Box<[Guarded<'db, RegionPath<IndexExpr<'db>>>]> {
        if suspended.scope() != guard.scope() {
            return Box::new([]);
        }
        let protected_path = protected.path.as_slice();
        canonical_mask(
            suspended
                .clauses()
                .iter()
                .filter(|clause| {
                    clause.guard.scope() == guard.scope()
                        && clause.payload.root == protected.root
                        && clause.payload.views == protected.views
                })
                .filter_map(|clause| {
                    let path = clause.payload.path.as_slice();
                    let suffix = if let Some(suffix) = path.strip_prefix(protected_path) {
                        RegionPath::new(suffix)
                    } else if protected_path.starts_with(path) {
                        RegionPath::default()
                    } else {
                        return None;
                    };
                    Some(Guarded {
                        guard: clause.guard.and(guard)?,
                        payload: suffix,
                    })
                }),
        )
    }

    /// Every index that some part of the relation observes.
    pub fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>> + '_ {
        [&self.protected, &self.access]
            .into_iter()
            .flat_map(|place| place.root.indices().chain(place.path.indices()))
            .chain(self.extent.indices())
            .chain(self.suspended.iter().flat_map(|slice| {
                slice
                    .payload
                    .indices()
                    .chain(slice.guard.indices())
                    .collect::<Vec<_>>()
            }))
    }

    /// The owner plus the witnesses this relation observes. A canonical
    /// relation observes exactly the leading witnesses of its clause.
    fn scope(&self, owner: &BinderScope) -> BinderScope {
        let witnesses: BTreeSet<_> = self
            .indices()
            .filter(|index| owner.validate(*index).is_err())
            .collect();
        witnesses.iter().fold(owner.clone(), |scope, _| {
            scope.bind(IndexNamespace::Existential).0
        })
    }

    /// Rename every part with one substitution, preserving shared witnesses.
    pub fn substitute(&self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        let place = |place: &SymbolicPlace<'db>| SymbolicPlace {
            root: place.root.substitute(db, subst),
            path: place.path.substitute(subst),
            views: place.views.substitute(db, subst),
        };
        Self {
            protected: place(&self.protected),
            protected_kind: self.protected_kind,
            access: place(&self.access),
            access_kind: self.access_kind,
            extent: self.extent.substitute(subst),
            // A must slice whose condition becomes unsatisfiable excludes
            // nothing. Substitution can reorder paths or identify them.
            suspended: canonical_mask(self.suspended.iter().filter_map(|slice| {
                Some(Guarded {
                    guard: slice.guard.in_scope(subst.source()).substitute(subst)?,
                    payload: slice.payload.substitute(subst),
                })
            })),
        }
    }

    /// Canonicalize one clause under `owner`. Witnesses are renamed in one
    /// substitution across every part, the relation's own before those only
    /// the guard observes. Suspension conditions then move to the relation's
    /// scope, so the relation alone, never the guard, determines the key, and
    /// the guard's own witnesses are projected or compacted.
    ///
    /// A witness that no endpoint, extent, or suspended path names selects no
    /// part of the relation. The clause applies on some value of it, and a
    /// slice stays suspended only where it holds on every value the clause
    /// admits, so eliminating it never weakens the obligation. A slice is read
    /// only where the clause applies, so one that holds there is unconditional.
    fn canonical(
        self,
        db: &'db dyn HirAnalysisDb,
        owner: &BinderScope,
        guard: &Guard<'db>,
    ) -> (Self, Guard<'db>) {
        let named: BTreeSet<_> = [&self.protected, &self.access]
            .into_iter()
            .flat_map(|place| place.root.indices().chain(place.path.indices()))
            .chain(self.extent.indices())
            .chain(
                self.suspended
                    .iter()
                    .flat_map(|slice| slice.payload.indices()),
            )
            .collect();
        let hidden =
            |index: IndexExpr<'db>| owner.validate(index).is_err() && !named.contains(&index);
        // Merging slices can drop a slice guard's last use of a witness.
        let mask = canonical_mask(self.suspended.iter().map(|slice| Guarded {
            guard: slice.guard.in_scope(guard.scope()),
            payload: slice.payload.clone(),
        }));
        let relation = Self {
            suspended: mask
                .iter()
                .filter_map(|slice| {
                    // Values the clause admits on which the slice fails. A slice
                    // that holds wherever the clause does is unconditional.
                    let guard = match guard.difference(&slice.guard) {
                        None => Guard::always(guard.scope()),
                        Some(_) if !slice.guard.indices().into_iter().any(hidden) => {
                            slice.guard.clone()
                        }
                        Some(refuted) => Guard::always(guard.scope())
                            .difference(&refuted.project_witnesses(hidden))?,
                    };
                    Some(Guarded {
                        guard,
                        payload: slice.payload.clone(),
                    })
                })
                .collect(),
            ..self
        };
        let observed: BTreeSet<_> = relation.indices().collect();
        let used = guard.indices();
        let substitution = guard.scope().ranked_existentials(owner, |index| {
            let observed = observed.contains(&index);
            (observed || used.contains(&index)).then_some(!observed)
        });
        let relation = relation.substitute(db, &substitution);
        let scope = relation.scope(owner);
        // Suspension conditions observe no witness beyond the relation's.
        let narrow = substitution
            .destination()
            .canonical_existentials(&scope, std::iter::empty);
        let relation = Self {
            suspended: relation
                .suspended
                .iter()
                .map(|slice| Guarded {
                    guard: slice
                        .guard
                        .substitute(&narrow)
                        .expect("suspension conditions observe relation witnesses"),
                    payload: slice.payload.clone(),
                })
                .collect(),
            ..relation
        };
        for index in relation.indices() {
            scope.validate(index).expect("free separation binder");
        }
        let guard = guard
            .substitute(&substitution)
            .expect("separation alpha normalization");
        (relation, compact_guard(&scope, guard))
    }
}

/// Project and compact the witnesses of a clause guard beyond `relation`, the
/// relation's scope, which keeps the relation and its suspension conditions.
/// Renaming re-canonicalizes the guard, which can pick another equality
/// representative and expose a projectable witness, so this repeats until
/// the guard is stable.
fn compact_guard<'db>(relation: &BinderScope, mut guard: Guard<'db>) -> Guard<'db> {
    loop {
        let projected = guard.project_witnesses(|index| relation.validate(index).is_err());
        let compact = projected
            .scope()
            .canonical_existentials(relation, || projected.indices());
        let next = projected
            .substitute(&compact)
            .expect("witness compaction preserves the guard");
        if next == guard {
            return guard;
        }
        guard = next;
    }
}

/// A mask excludes the union of its slices, so one slice per path is suspended
/// whenever any of its conditions holds. This neither widens nor narrows it.
fn canonical_mask<'db>(
    slices: impl IntoIterator<Item = Guarded<'db, RegionPath<IndexExpr<'db>>>>,
) -> Box<[Guarded<'db, RegionPath<IndexExpr<'db>>>]> {
    let mut mask = BTreeMap::<_, Guard<'db>>::new();
    for slice in slices {
        mask.entry(slice.payload)
            .and_modify(|guard| *guard = guard.or(&slice.guard))
            .or_insert(slice.guard);
    }
    mask.into_iter()
        .map(|(payload, guard)| Guarded { guard, payload })
        .collect()
}

/// Whether a clause over `owner` fits the representation limits. Exceeding one
/// is a deterministic analysis failure, never a truncated set of requirements.
pub fn within_limits<'db>(clause: &Guarded<'db, Separation<'db>>, owner: &BinderScope) -> bool {
    let relation = &clause.payload;
    let nodes = clause.guard.node_count()
        + relation
            .suspended
            .iter()
            .map(|slice| slice.guard.node_count())
            .sum::<usize>();
    let size = place_size(&relation.protected)
        + place_size(&relation.access)
        + relation.extent.indices().count()
        + relation
            .suspended
            .iter()
            .map(|slice| 1 + slice.payload.as_slice().len())
            .sum::<usize>();
    size <= SEPARATION_SIZE_LIMIT && guard_within_limits(&clause.guard, nodes, owner)
}

/// Whether the native validity of a related access, a clause over `owner`,
/// fits the same limits as the relation.
pub fn validity_within_limits<'db>(
    clause: &Guarded<'db, SymbolicPlace<'db>>,
    owner: &BinderScope,
) -> bool {
    place_size(&clause.payload) <= SEPARATION_SIZE_LIMIT
        && guard_within_limits(&clause.guard, clause.guard.node_count(), owner)
}

/// How many nodes a place stores; see `ExternalSource::size`.
fn place_size(place: &SymbolicPlace<'_>) -> usize {
    let root = match &place.root {
        RegionRoot::External(source) => source.size(),
        RegionRoot::Root { .. } | RegionRoot::Value(_) => 1,
    };
    root + place.path.as_slice().len() + place.views.iter().count()
}

/// Whether `nodes` guard nodes fit, with at most the witness limit beyond `owner`.
fn guard_within_limits(guard: &Guard<'_>, nodes: usize, owner: &BinderScope) -> bool {
    nodes <= SEPARATION_GUARD_NODE_LIMIT
        && guard
            .scope()
            .existential_extension_of(owner)
            .is_some_and(|witnesses| witnesses <= SEPARATION_WITNESS_LIMIT)
}

/// Guarded separation clauses over one owner scope, one per relation. A
/// clause's guard owns the witnesses shared by all of its parts.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SeparationSet<'db> {
    scope: BinderScope,
    clauses: Box<[Guarded<'db, Separation<'db>>]>,
}

impl<'db> SeparationSet<'db> {
    pub fn empty(scope: &BinderScope) -> Self {
        Self {
            scope: scope.clone(),
            clauses: Box::new([]),
        }
    }

    /// Canonicalize each clause as a whole and merge the guards of equal
    /// relations. The result is a fixed point: rebuilding it, or its union
    /// with an empty set, changes nothing.
    pub fn new(
        db: &'db dyn HirAnalysisDb,
        scope: &BinderScope,
        clauses: impl IntoIterator<Item = Guarded<'db, Separation<'db>>>,
    ) -> Self {
        let mut relations = BTreeMap::<Separation<'db>, Guard<'db>>::new();
        for clause in clauses {
            let (relation, guard) = clause.payload.canonical(db, scope, &clause.guard);
            match relations.entry(relation) {
                Entry::Vacant(entry) => {
                    entry.insert(guard);
                }
                Entry::Occupied(mut entry) => {
                    // Witnesses only the guards observe are existential in the
                    // obligation, and existentials distribute over a
                    // disjunction, so merged guards may share them by position.
                    let wide = entry.get().scope().max(guard.scope()).clone();
                    let merged = entry.get().in_scope(&wide).or(&guard.in_scope(&wide));
                    // The merge can leave such a witness unobservable.
                    let merged = compact_guard(&entry.key().scope(scope), merged);
                    entry.insert(merged);
                }
            }
        }
        Self {
            scope: scope.clone(),
            clauses: relations
                .into_iter()
                .map(|(payload, guard)| Guarded { guard, payload })
                .collect(),
        }
    }

    pub fn scope(&self) -> &BinderScope {
        &self.scope
    }

    pub fn clauses(&self) -> &[Guarded<'db, Separation<'db>>] {
        &self.clauses
    }

    pub fn is_empty(&self) -> bool {
        self.clauses.is_empty()
    }

    /// Rebuilding and the empty union are exact. Grouping can change the
    /// representation, never the meaning, of a relation whose guards keep two
    /// or more witnesses only they observe, since merges share those by
    /// position.
    /// Express this set in `owner`, a lexical ancestor of its scope. The
    /// binders `owner` lacks become each clause's own witnesses.
    pub fn quantify_into(&self, db: &'db dyn HirAnalysisDb, owner: &BinderScope) -> Self {
        if &self.scope == owner {
            return self.clone();
        }
        Self::new(
            db,
            owner,
            self.clauses.iter().map(|clause| {
                let subst = clause.guard.scope().quantifying(owner);
                Guarded {
                    guard: clause
                        .guard
                        .substitute(&subst)
                        .expect("renaming preserves feasibility"),
                    payload: clause.payload.substitute(db, &subst),
                }
            }),
        )
    }

    pub fn union(&self, db: &'db dyn HirAnalysisDb, other: &Self) -> Self {
        assert_eq!(self.scope, other.scope, "separation scopes must match");
        Self::new(
            db,
            &self.scope,
            self.clauses.iter().chain(other.clauses.iter()).cloned(),
        )
    }

    /// Apply one substitution to every part of every clause, lifting it over
    /// each clause's own witnesses.
    pub fn substitute(&self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        assert_eq!(
            self.scope(),
            subst.source(),
            "separation substitution scope mismatch"
        );
        if subst.is_identity() {
            return self.clone();
        }
        Self::new(
            db,
            subst.destination(),
            self.clauses.iter().filter_map(|clause| {
                let subst = subst.under_existentials(clause.guard.scope());
                Some(Guarded {
                    guard: clause.guard.substitute(&subst)?,
                    payload: clause.payload.substitute(db, &subst),
                })
            }),
        )
    }

    /// Project choices out of a precondition. The access can run on any
    /// admitted value of a hidden choice, but a slice is suspended only if it
    /// stays suspended on every admitted value. Projecting a slice's guard
    /// existentially would incorrectly turn a possible suspension into a fact.
    pub fn forget_occurrences(
        &self,
        db: &'db dyn HirAnalysisDb,
        hidden: impl Fn(ValueOccurrence) -> bool,
    ) -> Self {
        Self::new(
            db,
            &self.scope,
            self.clauses.iter().map(|clause| {
                let projected = clause.guard.forget_occurrences(&hidden);
                let suspended = clause
                    .payload
                    .suspended
                    .iter()
                    .filter_map(|slice| {
                        Some(Guarded {
                            guard: slice
                                .guard
                                .in_scope(clause.guard.scope())
                                .forget_occurrences_universally(&clause.guard, &hidden)?,
                            payload: slice.payload.clone(),
                        })
                    })
                    .collect();
                Guarded {
                    guard: projected,
                    payload: Separation {
                        suspended,
                        ..clause.payload.clone()
                    },
                }
            }),
        )
    }

    /// Rename choice occurrences in every clause and suspension guard at once.
    /// `rename` must be injective over all occurrences in the set, so every
    /// guard keeps its meaning and its relation to the others. Forgetting a
    /// choice would widen must conditions and needs separate treatment.
    pub fn map_occurrences(
        &self,
        db: &'db dyn HirAnalysisDb,
        rename: impl Fn(ValueOccurrence) -> ValueOccurrence,
    ) -> Self {
        let map = |guard: &Guard<'db>| {
            guard
                .map_occurrences(&rename)
                .expect("injective occurrence renaming preserves feasibility")
        };
        Self::new(
            db,
            &self.scope,
            self.clauses.iter().map(|clause| Guarded {
                guard: map(&clause.guard),
                payload: Separation {
                    suspended: clause
                        .payload
                        .suspended
                        .iter()
                        .map(|slice| Guarded {
                            guard: map(&slice.guard),
                            payload: slice.payload.clone(),
                        })
                        .collect(),
                    ..clause.payload.clone()
                },
            }),
        )
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::{
        analysis::{
            semantic::{
                FieldIndex, VariantIndex,
                capability::{
                    external::{ClobberCondition, ExternalSource, MemoryOffset},
                    guard::ChoiceKey,
                    index::IndexNamespace,
                    path::{Projection, StructuralPath},
                    region::RegionRoot,
                    repack::{ReferentRepackId, ReferentViews},
                    source::{InputSource, SourceExpr},
                    test_roots,
                },
            },
            ty::{trait_resolution::PredicateListId, ty_def::TyId},
        },
        test_db::HirAnalysisTestDb,
    };

    fn input<'db>(db: &'db HirAnalysisTestDb, param: u32) -> RegionRoot<'db> {
        RegionRoot::External(test_roots::input(db, InputSource::place(param)))
    }

    fn place<'db>(
        root: RegionRoot<'db>,
        path: impl Into<Arc<[Projection<IndexExpr<'db>>]>>,
    ) -> SymbolicPlace<'db> {
        SymbolicPlace {
            root,
            path: RegionPath::new(path),
            views: Default::default(),
        }
    }

    fn separation<'db>(
        protected: SymbolicPlace<'db>,
        access: SymbolicPlace<'db>,
        extent: AccessExtent<'db>,
        suspended: impl Into<Box<[Guarded<'db, RegionPath<IndexExpr<'db>>>]>>,
    ) -> Separation<'db> {
        Separation {
            protected,
            protected_kind: BorrowKind::Mut,
            access,
            access_kind: BorrowKind::Ref,
            extent,
            suspended: suspended.into(),
        }
    }

    #[test]
    fn alpha_equivalent_relations_merge_but_distinct_masks_do_not() {
        let db = HirAnalysisTestDb::default();
        let owner = BinderScope::default();
        let (unused, _) = owner.bind(IndexNamespace::Existential);
        let (two, late) = unused.bind(IndexNamespace::Existential);
        let (one, early) = owner.bind(IndexNamespace::Existential);
        let clause = |scope: &BinderScope, witness, suspended: Vec<_>| Guarded {
            guard: Guard::always(scope)
                .with_disequality(witness, IndexExpr::Const(3))
                .unwrap(),
            payload: separation(
                place(input(&db, 0), [Projection::Index(witness)]),
                place(input(&db, 1), [Projection::Index(witness)]),
                AccessExtent::Typed,
                suspended,
            ),
        };
        let merged = SeparationSet::new(
            &db,
            &owner,
            [
                clause(&two, late, Vec::new()),
                clause(&one, early, Vec::new()),
            ],
        );
        assert_eq!(merged.clauses().len(), 1, "{merged:#?}");
        let slice = Guarded {
            guard: Guard::always(&one),
            payload: RegionPath::default(),
        };
        let distinct = merged.union(
            &db,
            &SeparationSet::new(&db, &owner, [clause(&one, early, vec![slice])]),
        );
        assert_eq!(distinct.clauses().len(), 2, "{distinct:#?}");
    }

    #[test]
    fn shared_witnesses_survive_and_guard_only_witnesses_are_projected() {
        let db = HirAnalysisTestDb::default();
        let owner = BinderScope::default();
        let (scope, shared) = owner.bind(IndexNamespace::Existential);
        let (scope, hidden) = scope.bind(IndexNamespace::Existential);
        let set = SeparationSet::new(
            &db,
            &owner,
            [Guarded {
                guard: Guard::always(&scope)
                    .with_disequality(hidden, IndexExpr::Const(1))
                    .unwrap(),
                payload: separation(
                    place(input(&db, 0), [Projection::Index(shared)]),
                    place(input(&db, 1), [Projection::Index(shared)]),
                    AccessExtent::Bytes(shared),
                    [],
                ),
            }],
        );
        let [clause] = set.clauses() else {
            panic!("{set:#?}");
        };
        assert_eq!(
            clause.guard.scope().existential_extension_of(&owner),
            Some(1)
        );
        let separation = &clause.payload;
        assert_eq!(separation.protected.path, separation.access.path);
        assert_eq!(
            separation.extent,
            AccessExtent::Bytes(separation.access.path.indices().next().unwrap())
        );
    }

    #[test]
    fn suspension_slices_certify_only_definite_projections() {
        let db = HirAnalysisTestDb::default();
        let scope = BinderScope::default();
        let field = |index| Projection::Field(FieldIndex(index));
        let (witnessed, witness) = scope.bind(IndexNamespace::Existential);
        let suspended = RegionSet::new(
            &scope,
            [
                (input(&db, 0), vec![field(0), field(1)]),
                (input(&db, 0), vec![]),
                (input(&db, 0), vec![field(1)]),
                (input(&db, 1), vec![field(0)]),
            ]
            .into_iter()
            .map(|(root, path)| Guarded {
                guard: Guard::always(&scope),
                payload: place(root, path),
            })
            .chain([Guarded {
                // A witnessed clause is a possible suspension only.
                guard: Guard::always(&witnessed),
                payload: place(input(&db, 0), [field(0), Projection::Index(witness)]),
            }]),
        );
        let protected = place(input(&db, 0), [field(0)]);
        let slices: BTreeSet<_> =
            Separation::suspension_slices(&protected, &suspended, &Guard::always(&scope))
                .iter()
                .map(|slice| slice.payload.clone())
                .collect();
        assert_eq!(
            slices,
            [RegionPath::new([field(1)]), RegionPath::default()].into()
        );
    }

    #[test]
    fn substitution_renames_every_part_and_drops_infeasible_slices() {
        let db = HirAnalysisTestDb::default();
        let (owner, value) = BinderScope::default().bind(IndexNamespace::Value);
        let set = SeparationSet::new(
            &db,
            &owner,
            [Guarded {
                guard: Guard::always(&owner),
                payload: separation(
                    place(input(&db, 0), [Projection::Index(value)]),
                    place(input(&db, 1), [Projection::Index(value)]),
                    AccessExtent::Bytes(value),
                    [Guarded {
                        guard: Guard::always(&owner)
                            .with_disequality(value, IndexExpr::Const(2))
                            .unwrap(),
                        payload: RegionPath::new([Projection::Index(value)]),
                    }],
                ),
            }],
        );
        for (constant, slices) in [(2, 0), (5, 1)] {
            let subst = IndexSubst::new(
                &owner,
                &BinderScope::default(),
                [(value, IndexExpr::Const(constant))],
            )
            .unwrap();
            let substituted = set.substitute(&db, &subst);
            let [clause] = substituted.clauses() else {
                panic!("{substituted:#?}");
            };
            let index = Projection::Index(IndexExpr::Const(constant));
            assert_eq!(clause.payload.protected.path, RegionPath::new([index]));
            assert_eq!(clause.payload.access.path, RegionPath::new([index]));
            assert_eq!(
                clause.payload.extent,
                AccessExtent::Bytes(IndexExpr::Const(constant))
            );
            assert_eq!(clause.payload.suspended.len(), slices);
        }
    }

    #[test]
    fn hidden_choices_keep_argument_guards_and_only_definite_suspensions() {
        let db = HirAnalysisTestDb::default();
        let owner = BinderScope::default();
        let argument = ChoiceKey::new(ValueOccurrence::Argument(0), StructuralPath::default());
        let private = ChoiceKey::new(ValueOccurrence::SummaryChoice(0), StructuralPath::default());
        let outer = Guard::always(&owner).with_boolean(argument, true).unwrap();
        for branch in [None, Some(false), Some(true)] {
            let guard = branch.map_or_else(
                || outer.clone(),
                |value| outer.with_boolean(private.clone(), value).unwrap(),
            );
            let slices = [
                Guard::always(&owner)
                    .with_boolean(private.clone(), true)
                    .unwrap(),
                Guard::always(&owner)
                    .with_boolean(private.clone(), false)
                    .unwrap(),
                outer.clone(),
            ]
            .into_iter()
            .enumerate()
            .map(|(field, guard)| Guarded {
                guard,
                payload: RegionPath::new([Projection::Field(FieldIndex(field as u16))]),
            })
            .collect::<Vec<_>>();
            let before = SeparationSet::new(
                &db,
                &owner,
                [Guarded {
                    guard,
                    payload: separation(
                        place(input(&db, 0), []),
                        place(input(&db, 1), []),
                        AccessExtent::Typed,
                        slices,
                    ),
                }],
            );
            let after = before.forget_occurrences(&db, |choice| {
                matches!(choice, ValueOccurrence::SummaryChoice(_))
            });
            let [clause] = after.clauses() else {
                panic!("one relation")
            };
            assert_eq!(clause.guard, outer);
            let paths = clause
                .payload
                .suspended
                .iter()
                .map(|slice| {
                    assert_eq!(slice.guard, Guard::always(&owner));
                    slice.payload.clone()
                })
                .collect::<BTreeSet<_>>();
            let expected = [Some(2), branch.map(|value| if value { 0 } else { 1 })]
                .into_iter()
                .flatten()
                .map(|field| RegionPath::new([Projection::Field(FieldIndex(field))]))
                .collect();
            assert_eq!(paths, expected, "{branch:?}");
        }
    }

    #[test]
    fn normalization_is_a_fixed_point_after_guard_or() {
        let db = HirAnalysisTestDb::default();
        let owner = BinderScope::default();
        let (scope, witness) = owner.bind(IndexNamespace::Existential);
        let payload = separation(
            place(input(&db, 0), []),
            place(input(&db, 1), []),
            AccessExtent::Typed,
            [],
        );
        let choice = ChoiceKey::new(
            ValueOccurrence::SummaryChoice(0),
            StructuralPath::new([Projection::Index(witness)]),
        );
        let set = |guard: Option<_>| {
            SeparationSet::new(
                &db,
                &owner,
                [Guarded {
                    guard: guard.unwrap(),
                    payload: payload.clone(),
                }],
            )
        };
        let [some, none] = [true, false]
            .map(|value| set(Guard::always(&scope).with_boolean(choice.clone(), value)));
        // An indexed choice keeps its witness while it is observable.
        assert_eq!(some.clauses()[0].guard.scope(), &scope);
        let restricted = set(Guard::always(&scope)
            .with_boolean(choice.clone(), true)
            .and_then(|guard| guard.with_disequality(witness, IndexExpr::Const(3))));
        let merged = some.union(&db, &none);
        let [clause] = merged.clauses() else {
            panic!("{merged:#?}");
        };
        assert_eq!(clause.guard, Guard::always(&owner));
        assert_eq!(
            SeparationSet::new(&db, &owner, merged.clauses().iter().cloned()),
            merged
        );
        assert_eq!(merged.union(&db, &SeparationSet::empty(&owner)), merged);
        assert_eq!(merged.union(&db, &restricted), merged);
        assert_eq!(some.union(&db, &none.union(&db, &restricted)), merged);
    }

    #[test]
    fn guard_merges_keep_suspension_conditions_in_the_relation_scope() {
        let db = HirAnalysisTestDb::default();
        let owner = BinderScope::default();
        let (scope, witness) = owner.bind(IndexNamespace::Existential);
        let choice =
            |occurrence, path| ChoiceKey::new(ValueOccurrence::SummaryChoice(occurrence), path);
        let indexed = choice(0, StructuralPath::new([Projection::Index(witness)]));
        let suspension = choice(1, StructuralPath::default());
        // The whole referent is suspended under a condition that does not
        // observe the outer guard's witness.
        let relation = |scope: &BinderScope, guard| {
            let slice = Guard::always(scope)
                .with_boolean(suspension.clone(), true)
                .unwrap();
            Guarded {
                guard,
                payload: separation(
                    place(input(&db, 0), []),
                    place(input(&db, 1), []),
                    AccessExtent::Typed,
                    [Guarded {
                        guard: slice,
                        payload: RegionPath::default(),
                    }],
                ),
            }
        };
        let [some, none] = [true, false].map(|value| {
            relation(
                &scope,
                Guard::always(&scope)
                    .with_boolean(indexed.clone(), value)
                    .unwrap(),
            )
        });
        let aligned = |set: &SeparationSet<'_>| {
            for clause in set.clauses() {
                for slice in &clause.payload.suspended {
                    assert_eq!(slice.guard.scope(), &clause.payload.scope(set.scope()));
                }
            }
        };
        // A relation is keyed apart from witnesses only its guard observes.
        let conditional = SeparationSet::new(&db, &owner, [some.clone()]);
        aligned(&conditional);
        let [clause] = conditional.clauses() else {
            panic!("{conditional:#?}");
        };
        assert_eq!(clause.guard.scope(), &scope);

        let merged = SeparationSet::new(&db, &owner, [some.clone(), none.clone()]);
        aligned(&merged);
        let [clause] = merged.clauses() else {
            panic!("{merged:#?}");
        };
        assert_eq!(clause.guard, Guard::always(&owner));
        assert_eq!(
            &*clause.payload.suspended,
            &[Guarded {
                guard: Guard::always(&owner)
                    .with_boolean(suspension.clone(), true)
                    .unwrap(),
                payload: RegionPath::default(),
            }]
        );
        assert_eq!(
            SeparationSet::new(&db, &owner, merged.clauses().iter().cloned()),
            merged
        );
        assert_eq!(merged.union(&db, &SeparationSet::empty(&owner)), merged);

        // It merges with an already compacted relation.
        let compacted = SeparationSet::new(
            &db,
            &owner,
            [relation(
                &owner,
                Guard::always(&owner)
                    .with_boolean(choice(2, StructuralPath::default()), true)
                    .unwrap(),
            )],
        );
        let joined = conditional.union(&db, &compacted);
        aligned(&joined);
        let [clause] = joined.clauses() else {
            panic!("{joined:#?}");
        };
        assert_eq!(clause.guard.scope(), &scope);
        assert_eq!(merged.union(&db, &compacted), merged);
        let none = SeparationSet::new(&db, &owner, [none]);
        assert_eq!(joined.union(&db, &none), merged);
    }

    #[test]
    fn guard_only_witnesses_do_not_split_a_relation() {
        let db = HirAnalysisTestDb::default();
        let owner = BinderScope::default();
        let clause = |selector, guard| Guarded {
            guard,
            payload: separation(
                place(input(&db, 0), [Projection::Index(selector)]),
                place(input(&db, 1), []),
                AccessExtent::Typed,
                [],
            ),
        };
        // The guard's witness precedes the relation's own lexically.
        let (scope, guarded) = owner.bind(IndexNamespace::Existential);
        let (scope, selector) = scope.bind(IndexNamespace::Existential);
        let choice = ChoiceKey::new(
            ValueOccurrence::SummaryChoice(0),
            StructuralPath::new([Projection::Index(guarded)]),
        );
        let conditional = clause(
            selector,
            Guard::always(&scope).with_boolean(choice, true).unwrap(),
        );
        let (scope, selector) = owner.bind(IndexNamespace::Existential);
        let unconditional = clause(selector, Guard::always(&scope));
        let set = SeparationSet::new(&db, &owner, [conditional, unconditional.clone()]);
        assert_eq!(set.clauses(), [unconditional]);
    }

    #[test]
    fn ranking_leaves_no_projectable_guard_witness() {
        let db = HirAnalysisTestDb::default();
        let owner = BinderScope::default();
        // The guard's witness precedes the relation's, and an equality makes
        // the relation's witness its representative once ranked first.
        let (scope, hidden) = owner.bind(IndexNamespace::Existential);
        let (scope, observed) = scope.bind(IndexNamespace::Existential);
        let indexed = |index| {
            ChoiceKey::new(
                ValueOccurrence::SummaryChoice(0),
                StructuralPath::new([Projection::Index(index)]),
            )
        };
        let guard = Guard::always(&scope)
            .with_equality(hidden, observed)
            .and_then(|guard| guard.with_boolean(indexed(hidden), true))
            .unwrap();
        let (relation, witness) = owner.bind(IndexNamespace::Existential);
        let condition = |scope: &BinderScope| {
            Guard::always(scope)
                .with_boolean(
                    ChoiceKey::new(ValueOccurrence::SummaryChoice(1), StructuralPath::default()),
                    true,
                )
                .unwrap()
        };
        for suspended in [
            Vec::new(),
            vec![Guarded {
                guard: condition(&scope),
                payload: RegionPath::default(),
            }],
        ] {
            let original = Guarded {
                guard: guard.clone(),
                payload: separation(
                    place(input(&db, 0), []),
                    place(input(&db, 1), []),
                    AccessExtent::Bytes(observed),
                    suspended.clone(),
                ),
            };
            let once = SeparationSet::new(&db, &owner, [original.clone()]);
            assert_eq!(
                SeparationSet::new(&db, &owner, once.clauses().iter().cloned()),
                once
            );
            assert_eq!(once.union(&db, &SeparationSet::empty(&owner)), once);
            assert_eq!(once.union(&db, &once), once);
            assert_eq!(
                SeparationSet::new(&db, &owner, [original.clone(), original]),
                once
            );
            let [clause] = once.clauses() else {
                panic!("{once:#?}");
            };
            assert_eq!(
                clause.guard,
                Guard::always(&relation)
                    .with_boolean(indexed(witness), true)
                    .unwrap()
            );
            assert_eq!(clause.payload.extent, AccessExtent::Bytes(witness));
            let expected: Vec<_> = suspended
                .iter()
                .map(|slice| Guarded {
                    guard: condition(&relation),
                    payload: slice.payload.clone(),
                })
                .collect();
            assert_eq!(*clause.payload.suspended, *expected);
        }
    }

    #[test]
    fn projection_repeats_until_no_witness_is_exposed() {
        let db = HirAnalysisTestDb::default();
        let owner = BinderScope::default();
        let (scope, indexed) = owner.bind(IndexNamespace::Existential);
        let (scope, scalar) = scope.bind(IndexNamespace::Existential);
        let choice = ChoiceKey::new(
            ValueOccurrence::SummaryChoice(0),
            StructuralPath::new([Projection::Index(indexed)]),
        );
        // Projecting the scalar witness makes both branches of the choice equal,
        // so the indexed witness is then observable only by a scalar constraint.
        let branch = |value, constant| {
            Guard::always(&scope)
                .with_boolean(choice.clone(), value)
                .and_then(|guard| guard.with_equality(scalar, IndexExpr::Const(constant)))
                .and_then(|guard| guard.with_disequality(indexed, IndexExpr::Const(0)))
                .unwrap()
        };
        let set = SeparationSet::new(
            &db,
            &owner,
            [Guarded {
                guard: branch(true, 1).or(&branch(false, 2)),
                payload: separation(
                    place(input(&db, 0), []),
                    place(input(&db, 1), []),
                    AccessExtent::Typed,
                    [],
                ),
            }],
        );
        let [clause] = set.clauses() else {
            panic!("{set:#?}");
        };
        assert_eq!(clause.guard, Guard::always(&owner));
    }

    #[test]
    fn suspension_masks_are_sets() {
        let db = HirAnalysisTestDb::default();
        let (owner, first) = BinderScope::default().bind(IndexNamespace::Value);
        let (owner, second) = owner.bind(IndexNamespace::Value);
        let always = Guard::always(&owner);
        let slice = |index, guard| Guarded {
            guard,
            payload: RegionPath::new([Projection::Index(index)]),
        };
        let set = |suspended: Vec<_>| {
            SeparationSet::new(
                &db,
                &owner,
                [Guarded {
                    guard: always.clone(),
                    payload: separation(
                        place(input(&db, 0), []),
                        place(input(&db, 1), []),
                        AccessExtent::Typed,
                        suspended,
                    ),
                }],
            )
        };
        let forward = set(vec![
            slice(first, always.clone()),
            slice(second, always.clone()),
        ]);
        let reverse = set(vec![
            slice(second, always.clone()),
            slice(first, always.clone()),
        ]);
        assert_eq!(forward, reverse);
        assert_eq!(
            forward,
            set(vec![
                slice(first, always.clone()),
                slice(first, always.clone()),
                slice(second, always.clone()),
            ])
        );
        assert_eq!(forward.union(&db, &reverse), forward);
        // The conditions of one path merge exactly into one must condition.
        let [one, two] = [1, 2].map(|constant| {
            always
                .with_equality(first, IndexExpr::Const(constant))
                .unwrap()
        });
        let conditional = set(vec![slice(second, one.clone()), slice(second, two.clone())]);
        assert_eq!(
            &*conditional.clauses()[0].payload.suspended,
            &[slice(second, one.or(&two))]
        );
        // Substitution can reorder selectors or identify them.
        let swap = IndexSubst::new(&owner, &owner, [(first, second), (second, first)]).unwrap();
        assert_eq!(forward.substitute(&db, &swap), forward);
        let identify = IndexSubst::new(&owner, &owner, [(second, first)]).unwrap();
        assert_eq!(
            forward.substitute(&db, &identify),
            set(vec![slice(first, always.clone())])
        );
    }

    #[test]
    fn witnesses_observed_by_any_part_survive_jointly() {
        let db = HirAnalysisTestDb::default();
        let owner = BinderScope::default();
        let field = |index| Projection::Field(FieldIndex(index));
        // One witness shared by both endpoints, the extent and a slice, after
        // an unused binder and a witness only the guard observes.
        let (scope, _) = owner.bind(IndexNamespace::Existential);
        let (scope, hidden) = scope.bind(IndexNamespace::Existential);
        let (scope, shared) = scope.bind(IndexNamespace::Existential);
        let set = SeparationSet::new(
            &db,
            &owner,
            [Guarded {
                guard: Guard::always(&scope)
                    .with_disequality(hidden, IndexExpr::Const(1))
                    .unwrap(),
                payload: separation(
                    place(input(&db, 0), [Projection::Index(shared)]),
                    place(input(&db, 1), [Projection::Index(shared)]),
                    AccessExtent::Bytes(shared),
                    [Guarded {
                        guard: Guard::always(&scope)
                            .with_disequality(shared, IndexExpr::Const(2))
                            .unwrap(),
                        payload: RegionPath::new([field(0)]),
                    }],
                ),
            }],
        );
        let [clause] = set.clauses() else {
            panic!("{set:#?}");
        };
        let (single, witness) = owner.bind(IndexNamespace::Existential);
        assert_eq!(clause.guard, Guard::always(&single));
        let relation = &clause.payload;
        assert_eq!(
            relation.protected.path,
            RegionPath::new([Projection::Index(witness)])
        );
        assert_eq!(relation.access.path, relation.protected.path);
        assert_eq!(relation.extent, AccessExtent::Bytes(witness));
        assert_eq!(
            &*relation.suspended,
            &[Guarded {
                guard: Guard::always(&single)
                    .with_disequality(witness, IndexExpr::Const(2))
                    .unwrap(),
                payload: RegionPath::new([field(0)]),
            }]
        );
    }

    #[test]
    fn witnesses_only_guards_observe_are_eliminated() {
        let db = HirAnalysisTestDb::default();
        let owner = BinderScope::default();
        let field = |index| Projection::Field(FieldIndex(index));
        let (scope, witness) = owner.bind(IndexNamespace::Existential);
        fn canonical<'db>(
            db: &'db HirAnalysisTestDb,
            guard: Guard<'db>,
            slice: Guard<'db>,
        ) -> SeparationSet<'db> {
            SeparationSet::new(
                db,
                &BinderScope::default(),
                [Guarded {
                    guard,
                    payload: separation(
                        place(input(db, 0), []),
                        place(input(db, 1), []),
                        AccessExtent::Typed,
                        [Guarded {
                            guard: slice,
                            payload: RegionPath::new([Projection::Field(FieldIndex(0))]),
                        }],
                    ),
                }],
            )
        }
        let unequal = |scope: &BinderScope, index| {
            Guard::always(scope)
                .with_disequality(index, IndexExpr::Const(1))
                .unwrap()
        };
        // The clause admits a value on which the slice fails, so no part of
        // the protected place is certainly suspended.
        let refuted = canonical(&db, Guard::always(&scope), unequal(&scope, witness));
        let [clause] = refuted.clauses() else {
            panic!("{refuted:#?}");
        };
        assert_eq!(clause.guard, Guard::always(&owner));
        assert!(clause.payload.suspended.is_empty());

        // On every value the clause admits, the slice holds.
        let implied = canonical(&db, unequal(&scope, witness), unequal(&scope, witness));
        let [clause] = implied.clauses() else {
            panic!("{implied:#?}");
        };
        assert_eq!(clause.guard, Guard::always(&owner));
        assert_eq!(
            &*clause.payload.suspended,
            &[Guarded {
                guard: Guard::always(&owner),
                payload: RegionPath::new([field(0)]),
            }]
        );

        // A named index keeps its correlation with the eliminated witness:
        // the slice holds where every admitted value of the witness agrees.
        let (named_scope, named) = owner.bind(IndexNamespace::Existential);
        let (scope, hidden) = named_scope.bind(IndexNamespace::Existential);
        let correlated = SeparationSet::new(
            &db,
            &owner,
            [Guarded {
                guard: Guard::always(&scope)
                    .with_equality(hidden, IndexExpr::Const(3))
                    .unwrap(),
                payload: separation(
                    place(input(&db, 0), [Projection::Index(named)]),
                    place(input(&db, 1), []),
                    AccessExtent::Typed,
                    [Guarded {
                        guard: Guard::always(&scope).with_equality(hidden, named).unwrap(),
                        payload: RegionPath::new([field(0)]),
                    }],
                ),
            }],
        );
        let [clause] = correlated.clauses() else {
            panic!("{correlated:#?}");
        };
        assert_eq!(clause.guard, Guard::always(&named_scope));
        assert_eq!(
            &*clause.payload.suspended,
            &[Guarded {
                guard: Guard::always(&named_scope)
                    .with_equality(named, IndexExpr::Const(3))
                    .unwrap(),
                payload: RegionPath::new([field(0)]),
            }]
        );

        // A witness indexing a choice cannot be quantified alone, so the
        // relation keeps it.
        let (scope, witness) = owner.bind(IndexNamespace::Existential);
        let indexed = Guard::always(&scope)
            .with_boolean(
                ChoiceKey::new(
                    ValueOccurrence::SummaryChoice(0),
                    StructuralPath::new([Projection::Index(witness)]),
                ),
                true,
            )
            .unwrap();
        let kept = canonical(&db, Guard::always(&scope), indexed.clone());
        let [clause] = kept.clauses() else {
            panic!("{kept:#?}");
        };
        assert_eq!(clause.guard.scope(), &scope);
        assert_eq!(clause.payload.suspended[0].guard, indexed);
        // Canonical sets are fixed points.
        for set in [refuted, implied, correlated, kept] {
            assert_eq!(SeparationSet::new(&db, &owner, set.clauses().to_vec()), set);
        }
    }

    #[test]
    fn owner_substitution_does_not_capture_clause_witnesses() {
        let db = HirAnalysisTestDb::default();
        let (owner, value) = BinderScope::default().bind(IndexNamespace::Value);
        let (scope, witness) = owner.bind(IndexNamespace::Existential);
        let (scope, guarded) = scope.bind(IndexNamespace::Existential);
        let choice = |index| {
            ChoiceKey::new(
                ValueOccurrence::SummaryChoice(0),
                StructuralPath::new([Projection::Index(index)]),
            )
        };
        let field = RegionPath::new([Projection::Field(FieldIndex(0))]);
        let set = SeparationSet::new(
            &db,
            &owner,
            [Guarded {
                guard: Guard::always(&scope)
                    .with_disequality(witness, value)
                    .and_then(|guard| guard.with_boolean(choice(guarded), true))
                    .unwrap(),
                payload: separation(
                    place(
                        input(&db, 0),
                        [Projection::Index(value), Projection::Index(witness)],
                    ),
                    place(input(&db, 1), [Projection::Index(witness)]),
                    AccessExtent::Typed,
                    // A suspension condition in a strict prefix of the clause.
                    [Guarded {
                        guard: Guard::always(&owner)
                            .with_disequality(value, IndexExpr::Const(1))
                            .unwrap(),
                        payload: field.clone(),
                    }],
                ),
            }],
        );
        // The destination already binds the witness's lexical level.
        let (destination, occupied) = BinderScope::default().bind(IndexNamespace::Existential);
        let subst = IndexSubst::new(&owner, &destination, [(value, occupied)]).unwrap();
        let substituted = set.substitute(&db, &subst);
        let [clause] = substituted.clauses() else {
            panic!("{substituted:#?}");
        };
        let (relation, fresh) = destination.bind(IndexNamespace::Existential);
        let (scope, guarded) = relation.bind(IndexNamespace::Existential);
        assert_ne!(fresh, occupied);
        assert_eq!(
            clause.guard,
            Guard::always(&scope)
                .with_disequality(fresh, occupied)
                .and_then(|guard| guard.with_boolean(choice(guarded), true))
                .unwrap()
        );
        assert_eq!(
            clause.payload.protected.path,
            RegionPath::new([Projection::Index(occupied), Projection::Index(fresh)])
        );
        assert_eq!(
            clause.payload.access.path,
            RegionPath::new([Projection::Index(fresh)])
        );
        assert_eq!(
            &*clause.payload.suspended,
            &[Guarded {
                guard: Guard::always(&relation)
                    .with_disequality(occupied, IndexExpr::Const(1))
                    .unwrap(),
                payload: field,
            }]
        );
    }

    #[test]
    fn occurrence_renaming_keeps_conditional_suspensions() {
        let db = HirAnalysisTestDb::default();
        let owner = BinderScope::default();
        let choice = |occurrence| {
            ChoiceKey::new(
                ValueOccurrence::SummaryChoice(occurrence),
                StructuralPath::default(),
            )
        };
        let set = |outer, inner| {
            let guard = Guard::always(&owner)
                .with_boolean(choice(outer), true)
                .unwrap();
            SeparationSet::new(
                &db,
                &owner,
                [Guarded {
                    guard: guard.clone(),
                    payload: separation(
                        place(input(&db, 0), []),
                        place(input(&db, 1), []),
                        AccessExtent::Typed,
                        [Guarded {
                            // Only the suspension observes the inner choice.
                            guard: guard.with_boolean(choice(inner), false).unwrap(),
                            payload: RegionPath::default(),
                        }],
                    ),
                }],
            )
        };
        let swap = |occurrence| match occurrence {
            ValueOccurrence::SummaryChoice(0) => ValueOccurrence::SummaryChoice(1),
            ValueOccurrence::SummaryChoice(1) => ValueOccurrence::SummaryChoice(0),
            _ => occurrence,
        };
        let original = set(0, 1);
        let renamed = original.map_occurrences(&db, swap);
        assert_eq!(renamed, set(1, 0));
        assert_eq!(renamed.map_occurrences(&db, swap), original);
    }

    #[test]
    fn size_limit_counts_every_stored_projection_and_view() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("separation_size.fe".into(), "");
        let (module, _) = db.top_mod(file);
        let db = &db;
        let owner = BinderScope::default();
        let u256 = TyId::u256(db);
        let fields = |count: usize| vec![Projection::Field(FieldIndex(0)); count];
        let views = |count: usize| {
            let mut views = ReferentViews::default();
            for depth in 0..count {
                let (from, to) = if depth % 2 == 0 {
                    (u256, TyId::u8(db))
                } else {
                    (TyId::u8(db), u256)
                };
                views.append(
                    db,
                    depth,
                    ReferentRepackId::new(
                        db,
                        from,
                        to,
                        module.scope(),
                        PredicateListId::empty_list(db),
                    ),
                );
            }
            views
        };
        let accessed = |source| SymbolicPlace {
            root: RegionRoot::External(source),
            path: RegionPath::default(),
            views: Default::default(),
        };
        let clobbered = |target, written| {
            let mut source = test_roots::input(db, InputSource::place(1));
            source.clobber = Some(Box::new(ClobberCondition::new(
                target,
                written,
                AccessExtent::Typed,
            )));
            source
        };
        // Each relation stores `count` steps or views in one component.
        let relation = |component: usize, count: usize| {
            let input = |param| test_roots::input(db, InputSource::place(param));
            let mut protected = accessed(input(0));
            let mut access = accessed(input(1));
            let mut suspended = Vec::new();
            match component {
                0 => protected.path = RegionPath::new(fields(count)),
                1 => access.path = RegionPath::new(fields(count)),
                2 => access.views = views(count),
                3 => {
                    access = accessed(test_roots::input(
                        db,
                        InputSource::slot(1, StructuralPath::new(fields(count))),
                    ))
                }
                4 => {
                    access = accessed(test_roots::input(
                        db,
                        InputSource::place(1).follow(RegionPath::new(fields(count))),
                    ))
                }
                5 => {
                    let base = SourceExpr {
                        path: RegionPath::new(fields(count)),
                        ..SourceExpr::whole(input(1))
                    };
                    access = accessed(ExternalSource::memory(
                        db,
                        base,
                        u256,
                        MemoryOffset::Element(u256, IndexExpr::Const(1)),
                    ))
                }
                6 => {
                    let base = SourceExpr {
                        views: views(count),
                        ..SourceExpr::whole(input(1))
                    };
                    access = accessed(ExternalSource::memory(
                        db,
                        base,
                        u256,
                        MemoryOffset::Element(u256, IndexExpr::Const(1)),
                    ))
                }
                7 => {
                    let target = SourceExpr {
                        path: RegionPath::new(fields(count)),
                        ..SourceExpr::whole(input(0))
                    };
                    access = accessed(clobbered(target, SourceExpr::whole(input(2))))
                }
                8 => {
                    let written = SourceExpr {
                        path: RegionPath::new(fields(count)),
                        ..SourceExpr::whole(input(2))
                    };
                    access = accessed(clobbered(SourceExpr::whole(input(0)), written))
                }
                _ => suspended.push(Guarded {
                    guard: Guard::always(&owner),
                    payload: RegionPath::new(fields(count)),
                }),
            }
            Guarded {
                guard: Guard::always(&owner),
                payload: separation(protected, access, AccessExtent::Typed, suspended),
            }
        };
        for component in 0..10 {
            assert!(
                within_limits(&relation(component, 1), &owner),
                "{component}"
            );
            assert!(
                !within_limits(&relation(component, SEPARATION_SIZE_LIMIT), &owner),
                "{component}"
            );
        }
    }

    #[test]
    fn suspension_slices_keep_indexed_enum_and_viewed_projections() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("separation_views.fe".into(), "");
        let (module, _) = db.top_mod(file);
        let [viewed, other] = [TyId::u8(&db), TyId::bool(&db)].map(|target| {
            let mut views = ReferentViews::default();
            views.append(
                &db,
                1,
                ReferentRepackId::new(
                    &db,
                    TyId::u256(&db),
                    target,
                    module.scope(),
                    PredicateListId::empty_list(&db),
                ),
            );
            views
        });
        let (scope, selector) = BinderScope::default().bind(IndexNamespace::Value);
        let field = Projection::Field(FieldIndex(0));
        let variant = Projection::VariantField {
            variant: VariantIndex(1),
            field: FieldIndex(0),
        };
        let at = |path: Vec<_>, views| SymbolicPlace {
            root: input(&db, 0),
            path: RegionPath::new(path),
            views,
        };
        let selected = Projection::Index(selector);
        let restricted = Guard::always(&scope)
            .with_disequality(selector, IndexExpr::Const(2))
            .unwrap();
        let suspended = RegionSet::new(
            &scope,
            [
                (
                    Guard::always(&scope),
                    at(vec![selected, field], viewed.clone()),
                ),
                (
                    restricted.clone(),
                    at(vec![selected, variant], viewed.clone()),
                ),
                // Other views, and a place equal only under a guard, are dropped.
                (
                    Guard::always(&scope),
                    at(vec![selected, field], other.clone()),
                ),
                (
                    Guard::always(&scope),
                    at(vec![Projection::Index(IndexExpr::Const(3))], viewed.clone()),
                ),
            ]
            .map(|(guard, payload)| Guarded { guard, payload }),
        );
        let protected = at(vec![selected], viewed.clone());
        let guard = Guard::always(&scope)
            .with_disequality(selector, IndexExpr::Const(5))
            .unwrap();
        let slices = Separation::suspension_slices(&protected, &suspended, &guard);
        assert_eq!(
            &*slices,
            &[
                Guarded {
                    guard: guard.clone(),
                    payload: RegionPath::new([field]),
                },
                Guarded {
                    guard: restricted.and(&guard).unwrap(),
                    payload: RegionPath::new([variant]),
                },
            ]
        );
        // Views survive substitution with the rest of the relation.
        let set = SeparationSet::new(
            &db,
            &scope,
            [Guarded {
                guard,
                payload: separation(
                    protected,
                    place(input(&db, 1), []),
                    AccessExtent::Typed,
                    slices,
                ),
            }],
        );
        let subst = IndexSubst::new(
            &scope,
            &BinderScope::default(),
            [(selector, IndexExpr::Const(4))],
        )
        .unwrap();
        let substituted = set.substitute(&db, &subst);
        let [clause] = substituted.clauses() else {
            panic!("{substituted:#?}");
        };
        assert_eq!(clause.payload.protected.views, viewed);
        assert_eq!(
            clause.payload.protected.path,
            RegionPath::new([Projection::Index(IndexExpr::Const(4))])
        );
        assert_eq!(clause.payload.suspended.len(), 2);
    }

    #[test]
    fn guards_merge_only_for_equal_relations() {
        let db = HirAnalysisTestDb::default();
        let (owner, value) = BinderScope::default().bind(IndexNamespace::Value);
        let [first, second, third] = [1, 2, 3].map(|constant| {
            Guard::always(&owner)
                .with_equality(value, IndexExpr::Const(constant))
                .unwrap()
        });
        let [held, other] = [0, 1].map(|param| place(input(&db, param), []));
        let forward = separation(held.clone(), other.clone(), AccessExtent::Typed, []);
        let backward = separation(other, held, AccessExtent::Typed, []);
        let set = SeparationSet::new(
            &db,
            &owner,
            [
                (first.clone(), forward.clone()),
                (second.clone(), forward.clone()),
                (third.clone(), backward.clone()),
            ]
            .map(|(guard, payload)| Guarded { guard, payload }),
        );
        let clauses: BTreeSet<_> = set.clauses().iter().cloned().collect();
        assert_eq!(
            clauses,
            [
                Guarded {
                    guard: first.or(&second),
                    payload: forward,
                },
                Guarded {
                    guard: third,
                    payload: backward,
                },
            ]
            .into()
        );
    }

    #[test]
    fn a_partly_permitted_pair_owes_only_its_remainder() {
        let db = HirAnalysisTestDb::default();
        let (owner, value) = BinderScope::default().bind(IndexNamespace::Value);
        let pair = Guard::always(&owner)
            .with_disequality(value, IndexExpr::Const(0))
            .unwrap();
        let permitted = Guard::always(&owner)
            .with_equality(value, IndexExpr::Const(1))
            .unwrap();
        let remainder = pair.difference(&permitted).unwrap();
        let set = SeparationSet::new(
            &db,
            &owner,
            [Guarded {
                guard: remainder.clone(),
                payload: separation(
                    place(input(&db, 0), [Projection::Index(value)]),
                    place(input(&db, 1), []),
                    AccessExtent::Typed,
                    [],
                ),
            }],
        );
        let [clause] = set.clauses() else {
            panic!("{set:#?}");
        };
        assert_eq!(clause.guard, remainder);
        assert!(clause.guard.and(&permitted).is_none());
        let covered = clause.guard.or(&permitted);
        assert!(pair.implies(&covered) && covered.implies(&pair));
    }
}
