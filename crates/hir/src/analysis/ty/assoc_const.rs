use salsa::Update;

use super::ty_def::TyId;
use super::{
    fold::{TyFoldable, TyFolder},
    trait_def::TraitInstId,
    trait_resolution::PredicateListId,
    visitor::{TyVisitable, TyVisitor},
};
use crate::{
    analysis::HirAnalysisDb,
    hir_def::{IdentId, Impl, scope_graph::ScopeId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct AssocConstUse<'db> {
    origin_scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    inst: TraitInstId<'db>,
    name: IdentId<'db>,
}

impl<'db> AssocConstUse<'db> {
    pub fn new(
        origin_scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        inst: TraitInstId<'db>,
        name: IdentId<'db>,
    ) -> Self {
        Self {
            origin_scope,
            assumptions,
            inst,
            name,
        }
    }

    pub fn origin_scope(self) -> ScopeId<'db> {
        self.origin_scope
    }

    pub fn assumptions(self) -> PredicateListId<'db> {
        self.assumptions
    }

    pub fn inst(self) -> TraitInstId<'db> {
        self.inst
    }

    pub fn with_inst(self, inst: TraitInstId<'db>) -> Self {
        Self { inst, ..self }
    }

    pub fn with_env(self, origin_scope: ScopeId<'db>, assumptions: PredicateListId<'db>) -> Self {
        Self {
            origin_scope,
            assumptions,
            ..self
        }
    }

    pub fn name(self) -> IdentId<'db> {
        self.name
    }
}

impl<'db> TyVisitable<'db> for AssocConstUse<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.assumptions.visit_with(visitor);
        self.inst.visit_with(visitor);
    }
}

impl<'db> TyFoldable<'db> for AssocConstUse<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            origin_scope: self.origin_scope,
            assumptions: self.assumptions.fold_with(db, folder),
            inst: self.inst.fold_with(db, folder),
            name: self.name,
        }
    }
}

/// A use of an associated const defined in an inherent `impl` block,
/// e.g. `Foo::<7>::SIZE` or `Self::SIZE` inside the impl.
///
/// Carries the receiver type so the impl's generic parameters can be
/// recovered by unifying the impl self type against it once the receiver
/// is concrete (after typed-body instantiation).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct InherentConstUse<'db> {
    origin_scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    impl_: Impl<'db>,
    receiver_ty: TyId<'db>,
    name: IdentId<'db>,
}

impl<'db> InherentConstUse<'db> {
    pub fn new(
        origin_scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        impl_: Impl<'db>,
        receiver_ty: TyId<'db>,
        name: IdentId<'db>,
    ) -> Self {
        Self {
            origin_scope,
            assumptions,
            impl_,
            receiver_ty,
            name,
        }
    }

    pub fn origin_scope(self) -> ScopeId<'db> {
        self.origin_scope
    }

    pub fn assumptions(self) -> PredicateListId<'db> {
        self.assumptions
    }

    pub fn impl_(self) -> Impl<'db> {
        self.impl_
    }

    pub fn receiver_ty(self) -> TyId<'db> {
        self.receiver_ty
    }

    pub fn name(self) -> IdentId<'db> {
        self.name
    }
}

impl<'db> TyVisitable<'db> for InherentConstUse<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.assumptions.visit_with(visitor);
        self.receiver_ty.visit_with(visitor);
    }
}

impl<'db> TyFoldable<'db> for InherentConstUse<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            origin_scope: self.origin_scope,
            assumptions: self.assumptions.fold_with(db, folder),
            impl_: self.impl_,
            receiver_ty: self.receiver_ty.fold_with(db, folder),
            name: self.name,
        }
    }
}
