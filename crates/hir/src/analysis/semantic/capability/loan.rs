//! Static borrow definitions and exact, parameterized occurrences held by values.
use std::collections::{BTreeMap, BTreeSet};

use super::{
    guard::{Guard, ValueOccurrence},
    index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
    region::RegionSet,
    repack::{ReferentRepackId, ReferentViews, RepackPayload},
    semantics::CapabilityClass,
    value::{Guarded, IndexPayload},
};
use crate::analysis::{
    HirAnalysisDb,
    semantic::{BorrowActivation, SemOrigin},
    ty::ty_def::BorrowKind,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LoanId(pub usize);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LoanRef<'db> {
    pub id: LoanId,
    pub args: Box<[IndexExpr<'db>]>,
}

impl<'db> LoanRef<'db> {
    pub fn substitute(&self, subst: &IndexSubst<'db>) -> Self {
        Self {
            id: self.id,
            args: self.args.iter().map(|index| subst.apply(*index)).collect(),
        }
    }

    /// Matching a static definition is insufficient: suspension and authority
    /// require all arguments of the represented occurrences to agree.
    pub fn matching_guard(&self, other: &Self, mut guard: Guard<'db>) -> Option<Guard<'db>> {
        if self.id != other.id || self.args.len() != other.args.len() {
            return None;
        }
        for (left, right) in self.args.iter().zip(&other.args) {
            guard = guard.with_equality(*left, *right)?;
        }
        Some(guard)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CapabilityRef<'db> {
    Shared {
        reference: LoanRef<'db>,
        views: ReferentViews<'db>,
    },
    Mutable {
        reference: LoanRef<'db>,
        views: ReferentViews<'db>,
    },
    View {
        region: RegionSet<'db>,
        authority: Vec<Guarded<'db, LoanRef<'db>>>,
    },
    Address(RegionSet<'db>),
}

impl<'db> CapabilityRef<'db> {
    pub fn view(region: RegionSet<'db>, mut authority: Vec<Guarded<'db, LoanRef<'db>>>) -> Self {
        authority.sort();
        authority.dedup();
        Self::View { region, authority }
    }

    /// Views retain the permission used to create them without becoming active loans.
    pub fn authority(&self, guard: &Guard<'db>) -> Vec<Guarded<'db, LoanRef<'db>>> {
        match self {
            Self::Shared { reference, .. } | Self::Mutable { reference, .. } => vec![Guarded {
                guard: guard.clone(),
                payload: reference.clone(),
            }],
            Self::View { region, authority } => {
                let lift = IndexSubst::new(region.scope(), guard.scope(), [])
                    .expect("view authority scope");
                authority
                    .iter()
                    .filter_map(|entry| {
                        let subst = lift.under_existentials(entry.guard.scope());
                        let entry_guard = entry.guard.substitute(&subst)?;
                        Some(Guarded {
                            guard: entry_guard.and(&guard.in_scope(entry_guard.scope()))?,
                            payload: entry.payload.substitute(&subst),
                        })
                    })
                    .collect()
            }
            Self::Address(_) => Vec::new(),
        }
    }

    pub fn forget_occurrences(&self, repeated: impl Fn(ValueOccurrence) -> bool + Copy) -> Self {
        match self {
            Self::Shared { .. } | Self::Mutable { .. } => self.clone(),
            Self::Address(region) => Self::Address(region.forget_occurrences(repeated)),
            Self::View { region, authority } => Self::view(
                region.forget_occurrences(repeated),
                authority
                    .iter()
                    .map(|entry| Guarded {
                        guard: entry.guard.forget_occurrences(repeated),
                        payload: entry.payload.clone(),
                    })
                    .collect(),
            ),
        }
    }

    pub fn borrow(kind: BorrowKind, reference: LoanRef<'db>) -> Self {
        match kind {
            BorrowKind::Mut => Self::Mutable {
                reference,
                views: ReferentViews::default(),
            },
            BorrowKind::Ref => Self::Shared {
                reference,
                views: ReferentViews::default(),
            },
        }
    }
    pub fn loan(&self) -> Option<&LoanRef<'db>> {
        match self {
            Self::Shared { reference, .. } | Self::Mutable { reference, .. } => Some(reference),
            Self::View { .. } | Self::Address(_) => None,
        }
    }
    pub fn region(
        &self,
        db: &'db dyn HirAnalysisDb,
        loans: &[LoanDef<'db>],
        scope: &BinderScope,
    ) -> RegionSet<'db> {
        match self {
            Self::Shared { reference, views } | Self::Mutable { reference, views } => loans
                [reference.id.0]
                .region(db, reference, scope)
                .with_relative_views(db, views, 0),
            Self::View { region, .. } | Self::Address(region) => {
                let lift =
                    IndexSubst::new(region.scope(), scope, []).expect("capability witness scope");
                region.substitute(db, &lift)
            }
        }
    }
}

impl<'db> IndexPayload<'db> for CapabilityRef<'db> {
    fn accepts_class(&self, class: CapabilityClass) -> bool {
        let expected = match self {
            Self::Shared { .. } => CapabilityClass::Borrow(BorrowKind::Ref),
            Self::Mutable { .. } => CapabilityClass::Borrow(BorrowKind::Mut),
            Self::View { .. } => CapabilityClass::View,
            Self::Address(_) => {
                return matches!(class, CapabilityClass::Handle | CapabilityClass::Pointer);
            }
        };
        class == expected
    }
    fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>> {
        match self {
            Self::Shared { reference, .. } | Self::Mutable { reference, .. } => {
                reference.args.iter().copied().collect::<BTreeSet<_>>()
            }
            Self::View { region, authority } => {
                let mut indices = region.indices();
                for entry in authority {
                    indices.extend(
                        entry
                            .payload
                            .args
                            .iter()
                            .copied()
                            .chain(entry.guard.indices())
                            .filter(|index| region.scope().validate(*index).is_ok()),
                    );
                }
                indices
            }
            Self::Address(region) => region.indices(),
        }
        .into_iter()
    }
    fn substitute(&self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        match self {
            Self::Shared { reference, views } => Self::Shared {
                reference: reference.substitute(subst),
                views: views.substitute(db, subst),
            },
            Self::Mutable { reference, views } => Self::Mutable {
                reference: reference.substitute(subst),
                views: views.substitute(db, subst),
            },
            Self::View { region, authority } => {
                let lift =
                    IndexSubst::new(region.scope(), subst.source(), []).expect("view guard scope");
                let authority = authority
                    .iter()
                    .filter_map(|entry| {
                        let lift = lift.under_existentials(entry.guard.scope());
                        let guard = entry.guard.substitute(&lift)?;
                        let payload = entry.payload.substitute(&lift);
                        let subst = subst.under_existentials(guard.scope());
                        Some(Guarded {
                            guard: guard.substitute(&subst)?,
                            payload: payload.substitute(&subst),
                        })
                    })
                    .collect();
                Self::view(
                    region.substitute(db, &lift).substitute(db, subst),
                    authority,
                )
            }
            Self::Address(region) => {
                let lift = IndexSubst::new(region.scope(), subst.source(), [])
                    .expect("handle guard scope");
                Self::Address(region.substitute(db, &lift).substitute(db, subst))
            }
        }
    }
}

impl<'db> RepackPayload<'db> for CapabilityRef<'db> {
    fn repack_referent(&self, db: &'db dyn HirAnalysisDb, repack: ReferentRepackId<'db>) -> Self {
        let mut payload = self.clone();
        match &mut payload {
            Self::Shared { views, .. } | Self::Mutable { views, .. } => views.append(db, 0, repack),
            Self::View { region, .. } | Self::Address(region) => {
                *region = region.repack(db, repack)
            }
        }
        payload
    }
}

#[derive(Clone, Debug)]
pub struct LoanDef<'db> {
    kind: BorrowKind,
    activation: BorrowActivation<'db>,
    origin: SemOrigin<'db>,
    parameters: BinderScope,
    region: RegionSet<'db>,
    parents: BTreeMap<(BinderScope, LoanRef<'db>), Guard<'db>>,
}

impl<'db> LoanDef<'db> {
    /// Inventory immutable metadata before solving. Abstract all lexical family
    /// binders without capturing runtime values or type-level const expressions.
    pub fn new(
        kind: BorrowKind,
        activation: BorrowActivation<'db>,
        origin: SemOrigin<'db>,
        source: &BinderScope,
    ) -> (Self, Box<[IndexExpr<'db>]>, IndexSubst<'db>) {
        Self::with_occurrence_arguments(kind, activation, origin, source, [])
    }

    /// Loop executions contribute explicit occurrence parameters alongside lexical families.
    pub fn with_occurrence_arguments(
        kind: BorrowKind,
        activation: BorrowActivation<'db>,
        origin: SemOrigin<'db>,
        source: &BinderScope,
        occurrence: impl IntoIterator<Item = IndexExpr<'db>>,
    ) -> (Self, Box<[IndexExpr<'db>]>, IndexSubst<'db>) {
        let mut parameters = BinderScope::default();
        let arguments: Box<_> = source.variables().chain(occurrence).collect();
        let entries: Vec<_> = arguments
            .iter()
            .map(|argument| {
                let (nested, parameter) = parameters.bind(IndexNamespace::Loan);
                parameters = nested;
                (*argument, parameter)
            })
            .collect();
        let substitution =
            IndexSubst::new(source, &parameters, entries).expect("loan binder abstraction");
        let definition = Self {
            kind,
            activation,
            origin,
            region: RegionSet::empty(&parameters),
            parents: BTreeMap::new(),
            parameters,
        };
        (definition, arguments, substitution)
    }

    pub fn kind(&self) -> BorrowKind {
        self.kind
    }
    pub fn activation(&self) -> BorrowActivation<'db> {
        self.activation
    }
    pub fn origin(&self) -> SemOrigin<'db> {
        self.origin
    }
    pub fn parameters(&self) -> &BinderScope {
        &self.parameters
    }

    pub fn extend(
        &mut self,
        region: &RegionSet<'db>,
        parents: impl IntoIterator<Item = Guarded<'db, LoanRef<'db>>>,
    ) -> bool {
        assert_eq!(
            region.scope(),
            &self.parameters,
            "loan region scope mismatch"
        );
        let joined = self.region.union(region);
        let mut changed = joined != self.region;
        self.region = joined;
        for mut parent in parents {
            let subst = parent.guard.scope().canonical_existentials(
                &self.parameters,
                parent
                    .guard
                    .indices()
                    .into_iter()
                    .chain(parent.payload.args.iter().copied()),
            );
            parent.guard = parent
                .guard
                .substitute(&subst)
                .expect("parent witness normalization");
            parent.payload = parent.payload.substitute(&subst);
            for argument in &parent.payload.args {
                parent
                    .guard
                    .scope()
                    .validate(*argument)
                    .expect("free loan parent argument");
            }
            let guard = self
                .parents
                .entry((parent.guard.scope().clone(), parent.payload))
                .or_insert_with(|| {
                    changed = true;
                    parent.guard.clone()
                });
            let joined = guard.or(&parent.guard);
            changed |= joined != *guard;
            *guard = joined;
        }
        changed
    }

    /// Abstract an exact result occurrence into this definition's formal family.
    /// Unrelated source selectors remain owned existential witnesses; constants
    /// constrain the result family instead of disappearing during abstraction.
    pub fn extend_occurrence(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        reference: &LoanRef<'db>,
        region: &RegionSet<'db>,
        parents: impl IntoIterator<Item = Guarded<'db, LoanRef<'db>>>,
    ) -> bool {
        let parameters: Vec<_> = self.parameters.variables().collect();
        assert_eq!(
            parameters.len(),
            reference.args.len(),
            "loan argument arity mismatch"
        );
        let mut scope = self.parameters.clone();
        let mut bindings = BTreeMap::new();
        for (argument, parameter) in reference.args.iter().zip(&parameters) {
            if matches!(
                argument,
                IndexExpr::Bound(_) | IndexExpr::Runtime(_) | IndexExpr::Iteration(_)
            ) {
                bindings.entry(*argument).or_insert(*parameter);
            }
        }
        for source in region.scope().variables() {
            bindings.entry(source).or_insert_with(|| {
                let (nested, witness) = scope.bind(IndexNamespace::Existential);
                scope = nested;
                witness
            });
        }
        let subst =
            IndexSubst::new(region.scope(), &scope, bindings).expect("loan occurrence abstraction");
        let mut guard = Some(Guard::always(&scope));
        for (argument, parameter) in reference.args.iter().zip(parameters) {
            guard = guard.and_then(|guard| guard.with_equality(parameter, subst.apply(*argument)));
        }
        let Some(guard) = guard else { return false };
        let region = region
            .substitute(db, &subst)
            .with_guard(&guard)
            .close_existentials(&self.parameters);
        let parents = parents.into_iter().filter_map(|parent| {
            let subst = subst.under_existentials(parent.guard.scope());
            let guard = parent
                .guard
                .substitute(&subst)?
                .and(&guard.in_scope(subst.destination()))?;
            Some(Guarded {
                guard,
                payload: parent.payload.substitute(&subst),
            })
        });
        self.extend(&region, parents)
    }

    fn substitution(&self, reference: &LoanRef<'db>, scope: &BinderScope) -> IndexSubst<'db> {
        let parameters: Vec<_> = self.parameters.variables().collect();
        assert_eq!(
            parameters.len(),
            reference.args.len(),
            "loan argument arity mismatch"
        );
        IndexSubst::new(
            &self.parameters,
            scope,
            parameters.into_iter().zip(reference.args.iter().copied()),
        )
        .expect("loan arguments must be in scope")
    }

    pub fn region(
        &self,
        db: &'db dyn HirAnalysisDb,
        reference: &LoanRef<'db>,
        scope: &BinderScope,
    ) -> RegionSet<'db> {
        self.region
            .substitute(db, &self.substitution(reference, scope))
    }

    pub fn parents(
        &self,
        reference: &LoanRef<'db>,
        scope: &BinderScope,
    ) -> Vec<Guarded<'db, LoanRef<'db>>> {
        let subst = self.substitution(reference, scope);
        self.parents
            .iter()
            .filter_map(|((_, parent), guard)| {
                let subst = subst.under_existentials(guard.scope());
                Some(Guarded {
                    guard: guard.substitute(&subst)?,
                    payload: parent.substitute(&subst),
                })
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::semantic::capability::test_roots;

    use super::*;
    use crate::analysis::semantic::{
        capability::path::{Projection, RegionPath},
        capability::region::{OverlapResult, RegionRoot},
        capability::source::InputSource,
        normalized::{NRootId, NValueId},
    };
    use crate::test_db::HirAnalysisTestDb;

    #[test]
    fn loan_families_abstract_and_instantiate_regions_and_guarded_parent_occurrences() {
        let db = HirAnalysisTestDb::default();
        let empty = BinderScope::default();
        let (scope, outer) = empty.bind(IndexNamespace::Value);
        let (scope, inner) = scope.bind(IndexNamespace::Value);
        let (mut definition, args, abstraction) = LoanDef::new(
            BorrowKind::Mut,
            BorrowActivation::Immediate,
            SemOrigin::Synthetic,
            &scope,
        );
        assert_eq!(args.as_ref(), &[outer, inner]);
        assert_ne!(abstraction.apply(outer), outer);
        let region = RegionSet::singleton(
            &scope,
            RegionRoot::External(test_roots::input(&db, InputSource::place(0))),
            RegionPath::new([Projection::Index(outer), Projection::Index(inner)]),
        );
        let guard = Guard::always(&scope)
            .with_bound(outer, 4)
            .unwrap()
            .with_equality(outer, inner)
            .unwrap();
        let parent = LoanRef {
            id: LoanId(7),
            args: [outer].into(),
        };
        let parent = Guarded {
            guard: guard.substitute(&abstraction).unwrap(),
            payload: parent.substitute(&abstraction),
        };
        let region = region.substitute(&db, &abstraction);
        assert!(definition.extend(&region, [parent.clone()]));
        assert!(!definition.extend(&region, [parent]));
        let reference = LoanRef {
            id: LoanId(8),
            args: [IndexExpr::Const(2), IndexExpr::Const(2)].into(),
        };
        let actual = definition.region(&db, &reference, &empty);
        assert_eq!(
            actual,
            RegionSet::singleton(
                &empty,
                RegionRoot::External(test_roots::input(&db, InputSource::place(0))),
                RegionPath::new([Projection::Index(2.into()), Projection::Index(2.into())])
            )
        );
        assert_eq!(
            definition.parents(&reference, &empty),
            vec![Guarded {
                guard: Guard::always(&empty),
                payload: LoanRef {
                    id: LoanId(7),
                    args: [2.into()].into()
                }
            }]
        );
        let sibling = LoanRef {
            id: LoanId(8),
            args: [2.into(), 3.into()].into(),
        };
        assert!(definition.parents(&sibling, &empty).is_empty());
        assert_eq!(
            actual.overlap(&definition.region(&db, &sibling, &empty)),
            OverlapResult::Disjoint
        );
        let out_of_bound = LoanRef {
            id: LoanId(8),
            args: [4.into(), 4.into()].into(),
        };
        assert!(definition.parents(&out_of_bound, &empty).is_empty());
    }

    #[test]
    fn parent_matching_requires_exact_family_arguments_and_holder_guards() {
        let scope = BinderScope::default();
        let index = IndexExpr::Runtime(NValueId::from_u32(0));
        let reference = |argument| LoanRef {
            id: LoanId(0),
            args: [argument].into(),
        };
        let zero = reference(0.into());
        let one = reference(1.into());
        assert!(zero.matching_guard(&one, Guard::always(&scope)).is_none());
        let conditional = zero
            .matching_guard(&reference(index), Guard::always(&scope))
            .unwrap();
        assert!(conditional.proves_equal(index, 0.into()));
        let other = Guard::always(&scope)
            .with_disequality(index, 0.into())
            .unwrap();
        assert!(zero.matching_guard(&reference(index), other).is_none());
        let different = LoanRef {
            id: LoanId(1),
            args: zero.args.clone(),
        };
        assert!(
            zero.matching_guard(&different, Guard::always(&scope))
                .is_none()
        );
    }

    #[test]
    fn loan_facts_grow_when_only_parent_relations_change() {
        let db = HirAnalysisTestDb::default();
        let scope = BinderScope::default();
        let (mut definition, args, _) = LoanDef::new(
            BorrowKind::Mut,
            BorrowActivation::Immediate,
            SemOrigin::Synthetic,
            &scope,
        );
        let reference = LoanRef {
            id: LoanId(0),
            args,
        };
        let region = RegionSet::singleton(
            &scope,
            test_roots::local(&db, NRootId::from_u32(0)),
            RegionPath::default(),
        );
        assert!(definition.extend(&region, []));
        let index = IndexExpr::Runtime(NValueId::from_u32(0));
        let first = Guarded {
            guard: Guard::always(&scope)
                .with_equality(index, 0.into())
                .unwrap(),
            payload: reference.clone(),
        };
        assert!(definition.extend(&region, [first.clone()]));
        assert!(!definition.extend(&region, [first]));
        let remainder = Guarded {
            guard: Guard::always(&scope)
                .with_disequality(index, 0.into())
                .unwrap(),
            payload: reference.clone(),
        };
        assert!(definition.extend(&region, [remainder]));
        assert_eq!(
            definition.parents(&reference, &scope),
            vec![Guarded {
                guard: Guard::always(&scope),
                payload: reference
            }]
        );
        assert_eq!(definition.kind(), BorrowKind::Mut);
        assert_eq!(definition.activation(), BorrowActivation::Immediate);
    }

    #[test]
    fn views_preserve_guarded_authority_without_becoming_active_loans() {
        let db = HirAnalysisTestDb::default();
        let empty = BinderScope::default();
        let (scope, member) = empty.bind(IndexNamespace::Value);
        let (owned, witness) = scope.bind(IndexNamespace::Existential);
        let selector = IndexExpr::Runtime(NValueId::from_u32(3));
        let region = RegionSet::singleton(
            &scope,
            RegionRoot::External(test_roots::input(&db, InputSource::place(0))),
            RegionPath::new([Projection::Index(member)]),
        );
        let view = CapabilityRef::view(
            region,
            vec![Guarded {
                guard: Guard::always(&owned)
                    .with_bound(witness, 4)
                    .unwrap()
                    .with_equality(selector, 0.into())
                    .unwrap(),
                payload: LoanRef {
                    id: LoanId(7),
                    args: [member, witness].into(),
                },
            }],
        );
        assert!(view.loan().is_none());
        assert!(!view.indices().any(|index| index == witness));
        let subst = IndexSubst::new(&scope, &empty, [(member, 2.into())]).unwrap();
        let selected = view.substitute(&db, &subst);
        let authority = selected.authority(&Guard::always(&empty));
        assert_eq!(authority.len(), 1);
        assert_eq!(authority[0].payload.args[0], 2.into());
        assert!(
            authority[0]
                .guard
                .scope()
                .existential_extension_of(&empty)
                .is_some()
        );
        assert!(
            authority[0]
                .guard
                .scope()
                .validate(authority[0].payload.args[1])
                .is_ok()
        );
        assert!(
            selected
                .authority(
                    &Guard::always(&empty)
                        .with_disequality(selector, 0.into())
                        .unwrap()
                )
                .is_empty()
        );
        assert_eq!(
            selected.region(&db, &[], &empty),
            RegionSet::singleton(
                &empty,
                RegionRoot::External(test_roots::input(&db, InputSource::place(0))),
                RegionPath::new([Projection::Index(2.into())])
            )
        );
    }
}
