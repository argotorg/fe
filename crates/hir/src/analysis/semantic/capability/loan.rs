//! Static borrow definitions and exact, parameterized occurrences held by values.
use std::collections::{BTreeMap, BTreeSet};

use super::{
    guard::Guard,
    index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
    region::RegionSet,
    semantics::CapabilityClass,
    value::{Guarded, IndexPayload},
};
use crate::analysis::{
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
    Shared(LoanRef<'db>),
    Mutable(LoanRef<'db>),
    View(RegionSet<'db>),
    Handle(RegionSet<'db>),
}

impl<'db> CapabilityRef<'db> {
    pub fn borrow(kind: BorrowKind, reference: LoanRef<'db>) -> Self {
        match kind {
            BorrowKind::Mut => Self::Mutable(reference),
            BorrowKind::Ref => Self::Shared(reference),
        }
    }
    pub fn loan(&self) -> Option<&LoanRef<'db>> {
        match self {
            Self::Shared(reference) | Self::Mutable(reference) => Some(reference),
            Self::View(_) | Self::Handle(_) => None,
        }
    }
    pub fn region(&self, loans: &[LoanDef<'db>], scope: &BinderScope) -> RegionSet<'db> {
        match self {
            Self::Shared(reference) | Self::Mutable(reference) => {
                loans[reference.id.0].region(reference, scope)
            }
            Self::View(region) | Self::Handle(region) => {
                assert_eq!(region.scope(), scope);
                region.clone()
            }
        }
    }
}

impl<'db> IndexPayload<'db> for CapabilityRef<'db> {
    fn accepts_class(&self, class: CapabilityClass) -> bool {
        let expected = match self {
            Self::Shared(_) => CapabilityClass::Borrow(BorrowKind::Ref),
            Self::Mutable(_) => CapabilityClass::Borrow(BorrowKind::Mut),
            Self::View(_) => CapabilityClass::View,
            Self::Handle(_) => CapabilityClass::Handle,
        };
        class == expected
    }
    fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>> {
        match self {
            Self::Shared(reference) | Self::Mutable(reference) => {
                reference.args.iter().copied().collect::<BTreeSet<_>>()
            }
            Self::View(region) | Self::Handle(region) => region.indices(),
        }
        .into_iter()
    }
    fn substitute(&self, subst: &IndexSubst<'db>) -> Self {
        match self {
            Self::Shared(reference) => Self::Shared(reference.substitute(subst)),
            Self::Mutable(reference) => Self::Mutable(reference.substitute(subst)),
            Self::View(region) => Self::View(region.substitute(subst)),
            Self::Handle(region) => Self::Handle(region.substitute(subst)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct LoanDef<'db> {
    kind: BorrowKind,
    activation: BorrowActivation<'db>,
    origin: SemOrigin<'db>,
    parameters: BinderScope,
    region: RegionSet<'db>,
    parents: BTreeMap<LoanRef<'db>, Guard<'db>>,
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
        let mut parameters = BinderScope::default();
        let arguments: Box<_> = source.variables().collect();
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
        for parent in parents {
            assert_eq!(
                parent.guard.scope(),
                &self.parameters,
                "loan parent scope mismatch"
            );
            for argument in &parent.payload.args {
                self.parameters
                    .validate(*argument)
                    .expect("free loan parent argument");
            }
            let guard = self.parents.entry(parent.payload).or_insert_with(|| {
                changed = true;
                parent.guard.clone()
            });
            let joined = guard.or(&parent.guard);
            changed |= joined != *guard;
            *guard = joined;
        }
        changed
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

    pub fn region(&self, reference: &LoanRef<'db>, scope: &BinderScope) -> RegionSet<'db> {
        self.region.substitute(&self.substitution(reference, scope))
    }

    pub fn parents(
        &self,
        reference: &LoanRef<'db>,
        scope: &BinderScope,
    ) -> Vec<Guarded<'db, LoanRef<'db>>> {
        let subst = self.substitution(reference, scope);
        self.parents
            .iter()
            .filter_map(|(parent, guard)| {
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
    use super::*;
    use crate::analysis::semantic::{
        capability::path::{Projection, RegionPath},
        capability::region::{OverlapResult, RegionRoot},
        capability::source::InputSource,
        normalized::{NRootId, NValueId},
    };

    #[test]
    fn loan_families_abstract_and_instantiate_regions_and_guarded_parent_occurrences() {
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
            RegionRoot::Input(InputSource::place(0)),
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
        let region = region.substitute(&abstraction);
        assert!(definition.extend(&region, [parent.clone()]));
        assert!(!definition.extend(&region, [parent]));
        let reference = LoanRef {
            id: LoanId(8),
            args: [IndexExpr::Const(2), IndexExpr::Const(2)].into(),
        };
        let actual = definition.region(&reference, &empty);
        assert_eq!(
            actual,
            RegionSet::singleton(
                &empty,
                RegionRoot::Input(InputSource::place(0)),
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
            actual.overlap(&definition.region(&sibling, &empty)),
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
            RegionRoot::Root(NRootId::from_u32(0)),
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
}
