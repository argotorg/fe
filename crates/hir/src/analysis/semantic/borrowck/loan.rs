use crate::analysis::{
    semantic::{BorrowActivation, SemOrigin},
    ty::ty_def::BorrowKind,
};

use super::{
    guard::{Guard, IndexExpr, IndexParamId, IndexSubst, ResultIndexId},
    region::RegionSet,
    shape::SlotPath,
    summary::{SummaryPath, SummaryProjection},
    value::IndexPayload,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct LoanId(pub(super) u32);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct ParentClause {
    guard: Guard,
    reference: LoanRef,
}

impl ParentClause {
    pub(crate) fn new(guard: Guard, reference: LoanRef) -> Self {
        Self { guard, reference }
    }

    pub(crate) fn guard(&self) -> &Guard {
        &self.guard
    }

    pub(crate) fn reference(&self) -> &LoanRef {
        &self.reference
    }

    fn substitute(&self, subst: &IndexSubst) -> Option<Self> {
        Some(Self {
            guard: self.guard.substitute(subst)?,
            reference: self.reference.substitute(subst),
        })
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct ParentSet {
    clauses: Vec<ParentClause>,
}

impl ParentSet {
    pub(crate) fn from_guarded_references(
        references: impl IntoIterator<Item = (Guard, LoanRef)>,
    ) -> Self {
        Self::from_clauses(
            references
                .into_iter()
                .map(|(guard, reference)| ParentClause::new(guard, reference)),
        )
    }

    fn from_clauses(clauses: impl IntoIterator<Item = ParentClause>) -> Self {
        let mut clauses = clauses.into_iter().collect::<Vec<_>>();
        clauses.sort_unstable();
        clauses.dedup();
        Self { clauses }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &ParentClause> {
        self.clauses.iter()
    }

    pub(crate) fn with_guard(&self, guard: &Guard) -> Self {
        Self::from_clauses(self.clauses.iter().filter_map(|clause| {
            Some(ParentClause {
                guard: clause.guard.and(guard)?,
                reference: clause.reference.clone(),
            })
        }))
    }

    pub(crate) fn substitute(&self, subst: &IndexSubst) -> Self {
        Self::from_clauses(
            self.clauses
                .iter()
                .filter_map(|clause| clause.substitute(subst)),
        )
    }

    pub(crate) fn union(&mut self, other: Self) -> bool {
        let before = self.clauses.len();
        self.clauses.extend(other.clauses);
        self.clauses.sort_unstable();
        self.clauses.dedup();
        self.clauses.len() != before
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct AuthoritySet(ParentSet);

impl AuthoritySet {
    pub(crate) fn from_parents(parents: ParentSet) -> Self {
        Self(parents)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &ParentClause> {
        self.0.iter()
    }

    pub(crate) fn union(&mut self, other: Self) {
        self.0.union(other.0);
    }

    pub(crate) fn matches(&self, reference: &LoanRef, holder_guard: &Guard) -> bool {
        self.iter().any(|authority| {
            holder_guard
                .and(authority.guard())
                .and_then(|guard| reference.unify(authority.reference(), &guard))
                .is_some()
        })
    }
}

#[derive(Clone, Debug)]
pub(crate) struct LoanDef<'db> {
    kind: BorrowKind,
    activation: BorrowActivation,
    binders: Vec<IndexParamId>,
    region: RegionSet<'db>,
    parents: ParentSet,
    origin: SemOrigin<'db>,
}

impl<'db> LoanDef<'db> {
    pub(crate) fn plain(
        kind: BorrowKind,
        activation: BorrowActivation,
        origin: SemOrigin<'db>,
    ) -> Self {
        Self::new(kind, activation, Vec::new(), origin)
    }

    pub(crate) fn for_slot(
        kind: BorrowKind,
        path: &SlotPath<IndexParamId>,
        activation: BorrowActivation,
        origin: SemOrigin<'db>,
    ) -> Self {
        Self::new(kind, activation, slot_binders(path), origin)
    }

    pub(crate) fn for_summary(
        kind: BorrowKind,
        path: &SummaryPath,
        activation: BorrowActivation,
        origin: SemOrigin<'db>,
    ) -> Self {
        let mut binders = result_params(path)
            .map(|result| IndexParamId(result.0))
            .collect::<Vec<_>>();
        binders.sort_unstable();
        binders.dedup();
        Self::new(kind, activation, binders, origin)
    }

    fn new(
        kind: BorrowKind,
        activation: BorrowActivation,
        binders: Vec<IndexParamId>,
        origin: SemOrigin<'db>,
    ) -> Self {
        Self {
            kind,
            activation,
            binders,
            region: RegionSet::empty(),
            parents: ParentSet::default(),
            origin,
        }
    }

    pub(crate) fn kind(&self) -> BorrowKind {
        self.kind
    }

    pub(crate) fn activation(&self) -> BorrowActivation {
        self.activation
    }

    pub(crate) fn origin(&self) -> SemOrigin<'db> {
        self.origin
    }

    pub(crate) fn parents(&self) -> &ParentSet {
        &self.parents
    }

    pub(crate) fn extend(&mut self, region: RegionSet<'db>, parents: ParentSet) -> bool {
        let subst = self.result_parameter_subst();
        let region = region.substitute(&subst);
        let parents = parents.substitute(&subst);
        let joined = self.region.union(&region);
        let changed = joined != self.region;
        self.region = joined;
        self.parents.union(parents) || changed
    }

    pub(crate) fn instantiate(&self, reference: &LoanRef) -> RegionSet<'db> {
        self.region.substitute(&self.instantiation_subst(reference))
    }

    pub(crate) fn instantiate_parents(
        &self,
        reference: &LoanRef,
        holder_guard: &Guard,
    ) -> ParentSet {
        self.parents
            .substitute(&self.instantiation_subst(reference))
            .with_guard(holder_guard)
    }

    fn instantiation_subst(&self, reference: &LoanRef) -> IndexSubst {
        let mut subst = IndexSubst::new();
        for binder in &self.binders {
            if let Some(value) = reference.binding_for_param(*binder) {
                subst.insert(IndexExpr::LoanParam(*binder), value);
            }
        }
        subst
    }

    fn result_parameter_subst(&self) -> IndexSubst {
        let mut subst = IndexSubst::new();
        for binder in &self.binders {
            subst.insert(
                IndexExpr::ResultParam(ResultIndexId(binder.0)),
                IndexExpr::LoanParam(*binder),
            );
        }
        subst
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct LoanRef {
    pub(crate) id: LoanId,
    args: Vec<(IndexParamId, IndexExpr)>,
}

impl LoanRef {
    pub(crate) fn new(id: LoanId) -> Self {
        Self {
            id,
            args: Vec::new(),
        }
    }

    pub(crate) fn for_slot(id: LoanId, path: &SlotPath<IndexParamId>) -> Self {
        Self {
            id,
            args: slot_binders(path)
                .into_iter()
                .map(|param| (param, IndexExpr::LoanParam(param)))
                .collect(),
        }
    }

    pub(crate) fn for_summary(id: LoanId, path: &SummaryPath) -> Self {
        Self {
            id,
            args: result_params(path)
                .map(|result| (IndexParamId(result.0), IndexExpr::ResultParam(result)))
                .collect(),
        }
    }

    fn binding_for_param(&self, param: IndexParamId) -> Option<IndexExpr> {
        self.args
            .iter()
            .find_map(|(candidate, value)| (*candidate == param).then_some(*value))
    }

    pub(crate) fn substitute(&self, subst: &IndexSubst) -> Self {
        self.substitute_indices(subst)
    }

    pub(crate) fn unify(&self, other: &Self, guard: &Guard) -> Option<Guard> {
        if self.id != other.id || self.args.len() != other.args.len() {
            return None;
        }
        self.args.iter().zip(&other.args).try_fold(
            guard.clone(),
            |guard, ((lhs_param, lhs), (rhs_param, rhs))| {
                (lhs_param == rhs_param)
                    .then(|| guard.with_equality(*lhs, *rhs))
                    .flatten()
            },
        )
    }
}

impl IndexPayload for LoanRef {
    fn substitute_indices(&self, subst: &IndexSubst) -> Self {
        Self {
            id: self.id,
            args: self
                .args
                .iter()
                .map(|(param, value)| (*param, subst.apply(*value)))
                .collect(),
        }
    }
}

fn slot_binders(path: &SlotPath<IndexParamId>) -> Vec<IndexParamId> {
    let mut binders = path
        .as_slice()
        .iter()
        .filter_map(|projection| match projection {
            super::shape::SlotProjection::Index(index) => Some(*index),
            super::shape::SlotProjection::Field(_)
            | super::shape::SlotProjection::VariantField { .. } => None,
        })
        .collect::<Vec<_>>();
    binders.sort_unstable();
    binders.dedup();
    binders
}

fn result_params(path: &SummaryPath) -> impl Iterator<Item = ResultIndexId> + '_ {
    path.as_slice()
        .iter()
        .filter_map(|projection| match projection {
            SummaryProjection::Index(IndexExpr::ResultParam(result)) => Some(*result),
            SummaryProjection::Field(_)
            | SummaryProjection::VariantField { .. }
            | SummaryProjection::Index(_) => None,
        })
}
