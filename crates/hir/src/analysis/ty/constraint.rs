#![allow(dead_code)]

use common::indexmap::IndexSet;

use crate::analysis::{
    HirAnalysisDb,
    ty::{
        fold::{TyFoldable, TyFolder},
        trait_def::{ImplementorId, TraitInstId},
        trait_resolution::PredicateListId,
        ty_def::TyId,
        visitor::{TyVisitable, TyVisitor},
    },
};
use crate::hir_def::{Body, WhereClauseOwner, scope_graph::ScopeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub(crate) enum ConstraintKind<'db> {
    Trait(TraitInstId<'db>),
    ConstPredicate(ConstPredicateInstId<'db>),
}

impl<'db> From<TraitInstId<'db>> for ConstraintKind<'db> {
    fn from(inst: TraitInstId<'db>) -> Self {
        Self::Trait(inst)
    }
}

#[salsa::interned]
#[derive(Debug)]
pub(crate) struct ConstraintId<'db> {
    pub(crate) kind: ConstraintKind<'db>,
}

impl<'db> ConstraintId<'db> {
    pub(crate) fn from_trait(db: &'db dyn HirAnalysisDb, inst: TraitInstId<'db>) -> Self {
        Self::new(db, ConstraintKind::Trait(inst))
    }
}

#[salsa::interned]
#[derive(Debug)]
pub(crate) struct ConstraintListId<'db> {
    #[return_ref]
    pub(crate) list: Vec<ConstraintId<'db>>,
}

impl<'db> ConstraintListId<'db> {
    pub(crate) fn empty(db: &'db dyn HirAnalysisDb) -> Self {
        Self::new(db, Vec::new())
    }

    pub(crate) fn from_trait_predicates(
        db: &'db dyn HirAnalysisDb,
        predicates: PredicateListId<'db>,
    ) -> Self {
        Self::new(
            db,
            predicates
                .list(db)
                .iter()
                .map(|pred| ConstraintId::from_trait(db, *pred))
                .collect::<Vec<_>>(),
        )
    }

    pub(crate) fn merge(self, db: &'db dyn HirAnalysisDb, other: Self) -> Self {
        let mut constraints = self.list(db).clone();
        constraints.extend(other.list(db));
        Self::new(db, constraints)
    }

    pub(crate) fn is_empty(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.list(db).is_empty()
    }

    pub(crate) fn trait_predicates(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        PredicateListId::new(
            db,
            self.list(db)
                .iter()
                .filter_map(|constraint| match constraint.kind(db) {
                    ConstraintKind::Trait(inst) => Some(inst),
                    ConstraintKind::ConstPredicate(_) => None,
                })
                .collect::<Vec<_>>(),
        )
    }

    pub(crate) fn const_predicates(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<ConstPredicateInstId<'db>> {
        self.list(db)
            .iter()
            .filter_map(|constraint| match constraint.kind(db) {
                ConstraintKind::Trait(_) => None,
                ConstraintKind::ConstPredicate(pred) => Some(pred),
            })
            .collect()
    }

    pub(crate) fn extend_all_trait_bounds(self, db: &'db dyn HirAnalysisDb) -> Self {
        let mut constraints: IndexSet<ConstraintId<'db>> = self.list(db).iter().copied().collect();
        for &trait_pred in self.trait_predicates(db).extend_all_bounds(db).list(db) {
            constraints.insert(ConstraintId::from_trait(db, trait_pred));
        }
        Self::new(db, constraints.into_iter().collect::<Vec<_>>())
    }

    pub(crate) fn pretty_print(self, db: &'db dyn HirAnalysisDb) -> String {
        format!(
            "{{{}}}",
            self.list(db)
                .iter()
                .map(|constraint| constraint.pretty_print(db))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl<'db> ConstraintId<'db> {
    pub(crate) fn pretty_print(self, db: &'db dyn HirAnalysisDb) -> String {
        match self.kind(db) {
            ConstraintKind::Trait(inst) => inst.pretty_print(db, true),
            ConstraintKind::ConstPredicate(pred) => pred.pretty_print(db),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub(crate) struct ConstPredicateRef<'db> {
    pub(crate) owner: WhereClauseOwner<'db>,
    pub(crate) index: u32,
}

impl<'db> ConstPredicateRef<'db> {
    pub(crate) fn body(self, db: &'db dyn HirAnalysisDb) -> Body<'db> {
        self.owner
            .where_clause(db)
            .const_predicates(db)
            .get(self.index as usize)
            .copied()
            .expect("const predicate ref index should resolve to a where-clause predicate")
    }
}

#[salsa::interned]
#[derive(Debug)]
pub(crate) struct ConstPredicateInstId<'db> {
    pub(crate) predicate: ConstPredicateRef<'db>,

    #[return_ref]
    pub(crate) args: Vec<TyId<'db>>,
}

impl<'db> ConstPredicateInstId<'db> {
    pub(crate) fn body(self, db: &'db dyn HirAnalysisDb) -> Body<'db> {
        self.predicate(db).body(db)
    }

    pub(crate) fn pretty_print(self, db: &'db dyn HirAnalysisDb) -> String {
        let body = self.body(db);
        format!(
            "const predicate #{} on {}",
            self.predicate(db).index,
            body.top_mod(db).name(db).data(db).as_str()
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub(crate) struct ConstProofId<'db> {
    pub(crate) predicate: ConstPredicateInstId<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub(crate) enum EvidenceKind<'db> {
    Impl(ImplementorId<'db>),
    Assumption(ConstraintId<'db>),
    ConstProof(ConstProofId<'db>),
}

#[salsa::interned]
#[derive(Debug)]
pub(crate) struct EvidenceId<'db> {
    pub(crate) kind: EvidenceKind<'db>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ParamEnv<'db> {
    pub(crate) scope: ScopeId<'db>,
    pub(crate) constraints: ConstraintListId<'db>,
}

impl<'db> ParamEnv<'db> {
    pub(crate) fn trait_assumptions(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        self.constraints.trait_predicates(db)
    }

    pub(crate) fn const_assumptions(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<ConstPredicateInstId<'db>> {
        self.constraints.const_predicates(db)
    }
}

impl<'db> TyVisitable<'db> for ConstraintKind<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        match self {
            Self::Trait(inst) => inst.visit_with(visitor),
            Self::ConstPredicate(pred) => pred.visit_with(visitor),
        }
    }
}

impl<'db> TyFoldable<'db> for ConstraintKind<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        match self {
            Self::Trait(inst) => Self::Trait(inst.fold_with(db, folder)),
            Self::ConstPredicate(pred) => Self::ConstPredicate(pred.fold_with(db, folder)),
        }
    }
}

impl<'db> TyVisitable<'db> for ConstraintId<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.kind(visitor.db()).visit_with(visitor);
    }
}

impl<'db> TyFoldable<'db> for ConstraintId<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self::new(db, self.kind(db).fold_with(db, folder))
    }
}

impl<'db> TyVisitable<'db> for ConstraintListId<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.list(visitor.db()).visit_with(visitor);
    }
}

impl<'db> TyFoldable<'db> for ConstraintListId<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self::new(
            db,
            self.list(db)
                .iter()
                .map(|constraint| constraint.fold_with(db, folder))
                .collect::<Vec<_>>(),
        )
    }
}

impl<'db> TyVisitable<'db> for ConstPredicateInstId<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.args(visitor.db()).visit_with(visitor);
    }
}

impl<'db> TyFoldable<'db> for ConstPredicateInstId<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let args: Vec<_> = self
            .args(db)
            .iter()
            .map(|arg| arg.fold_with(db, folder))
            .collect();
        Self::new(db, self.predicate(db), args)
    }
}

impl<'db> TyVisitable<'db> for ConstProofId<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.predicate.visit_with(visitor);
    }
}

impl<'db> TyFoldable<'db> for ConstProofId<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            predicate: self.predicate.fold_with(db, folder),
        }
    }
}

impl<'db> TyVisitable<'db> for EvidenceKind<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        match self {
            Self::Impl(implementor) => implementor.visit_with(visitor),
            Self::Assumption(constraint) => constraint.visit_with(visitor),
            Self::ConstProof(proof) => proof.visit_with(visitor),
        }
    }
}

impl<'db> TyFoldable<'db> for EvidenceKind<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        match self {
            Self::Impl(implementor) => Self::Impl(implementor.fold_with(db, folder)),
            Self::Assumption(constraint) => Self::Assumption(constraint.fold_with(db, folder)),
            Self::ConstProof(proof) => Self::ConstProof(proof.fold_with(db, folder)),
        }
    }
}

impl<'db> TyVisitable<'db> for EvidenceId<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.kind(visitor.db()).visit_with(visitor);
    }
}

impl<'db> TyFoldable<'db> for EvidenceId<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self::new(db, self.kind(db).fold_with(db, folder))
    }
}
