use std::hash::Hash;

use common::indexmap::{IndexMap, IndexSet};
use hir::hir_def::IdentId;

use super::{
    trait_def::{Implementor, TraitInstId},
    trait_resolution::PredicateListId,
    ty_check::ExprProp,
    ty_def::{TyData, TyId},
    visitor::TyVisitable,
};
use crate::{
    HirAnalysisDb,
    ty::const_ty::{ConstTyData, ConstTyId},
};

pub trait TyFoldable<'db>
where
    Self: Sized + TyVisitable<'db>,
{
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>;

    fn fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        self.super_fold_with(folder)
    }
}

pub trait TyFolder<'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb;
    fn fold_ty(&mut self, ty: TyId<'db>) -> TyId<'db>;
}

impl<'db> TyFoldable<'db> for TyId<'db> {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        use TyData::*;

        let db = folder.db();
        match self.data(db) {
            TyApp(abs, arg) => {
                let abs = folder.fold_ty(*abs);
                let arg = folder.fold_ty(*arg);

                TyId::app(db, abs, arg)
            }

            ConstTy(cty) => {
                use ConstTyData::*;
                let cty_data = match cty.data(db) {
                    TyVar(var, ty) => {
                        let ty = folder.fold_ty(*ty);
                        TyVar(var.clone(), ty)
                    }
                    TyParam(param, ty) => {
                        let ty = folder.fold_ty(*ty);
                        TyParam(param.clone(), ty)
                    }
                    Evaluated(val, ty) => {
                        let ty = folder.fold_ty(*ty);
                        Evaluated(val.clone(), ty)
                    }
                    UnEvaluated(body) => UnEvaluated(*body),
                };

                let const_ty = ConstTyId::new(db, cty_data);
                TyId::const_ty(db, const_ty)
            }

            AssocTy(assoc) => {
                let folded_trait = assoc.trait_.fold_with(folder);

                TyId::assoc_ty(folder.db(), folded_trait, assoc.name)
            }

            QualifiedTy(trait_inst) => {
                let folded_trait = trait_inst.fold_with(folder);
                TyId::qualified_ty(folder.db(), folded_trait)
            }

            TyVar(_) | TyParam(_) | TyBase(_) | Never | Invalid(_) => self,
        }
    }

    fn fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        folder.fold_ty(self)
    }
}

impl<'db, T> TyFoldable<'db> for Vec<T>
where
    T: TyFoldable<'db>,
{
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        self.into_iter()
            .map(|inner| inner.fold_with(folder))
            .collect()
    }
}

impl<'db, T> TyFoldable<'db> for IndexSet<T>
where
    T: TyFoldable<'db> + Hash + Eq,
{
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        self.into_iter().map(|ty| ty.fold_with(folder)).collect()
    }
}

impl<'db> TyFoldable<'db> for TraitInstId<'db> {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let db = folder.db();
        let def = self.def(db);
        let args = self
            .args(db)
            .iter()
            .map(|ty| ty.fold_with(folder))
            .collect::<Vec<_>>();

        let assoc_type_bindings: IndexMap<IdentId<'db>, TyId<'db>> = self
            .assoc_type_bindings(db)
            .iter()
            .map(|(name, ty)| (*name, ty.fold_with(folder)))
            .collect();

        TraitInstId::new(db, def, args, assoc_type_bindings)
    }
}

impl<'db> TyFoldable<'db> for Implementor<'db> {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let db = folder.db();
        let trait_inst = self.trait_(db).fold_with(folder);
        let params = self
            .params(db)
            .iter()
            .map(|ty| ty.fold_with(folder))
            .collect::<Vec<_>>();
        let hir_impl_trait = self.hir_impl_trait(db);

        let types = self
            .types(db)
            .iter()
            .map(|(ident, ty)| (*ident, ty.fold_with(folder)))
            .collect::<IndexMap<_, _>>();

        Implementor::new(db, trait_inst, params, types, hir_impl_trait)
    }
}

impl<'db> TyFoldable<'db> for PredicateListId<'db> {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let predicates = self
            .list(folder.db())
            .iter()
            .map(|pred| pred.fold_with(folder))
            .collect::<Vec<_>>();

        Self::new(folder.db(), predicates)
    }
}

impl<'db> TyFoldable<'db> for ExprProp<'db> {
    fn super_fold_with<F>(self, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let ty = self.ty.fold_with(folder);
        Self { ty, ..self }
    }
}

/// A type folder that substitutes associated types based on a trait instance's bindings
pub struct AssocTySubst<'db> {
    db: &'db dyn HirAnalysisDb,
    trait_inst: TraitInstId<'db>,
}

impl<'db> AssocTySubst<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb, trait_inst: TraitInstId<'db>) -> Self {
        Self { db, trait_inst }
    }
}

impl<'db> TyFolder<'db> for AssocTySubst<'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.db
    }

    fn fold_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(self.db) {
            TyData::TyParam(param) => {
                // If this is a trait self parameter, substitute with the trait instance's self type
                if param.is_trait_self() {
                    return self.trait_inst.self_ty(self.db).fold_with(self);
                }
                ty.super_fold_with(self)
            }
            TyData::AssocTy(assoc_ty) => {
                // First fold the trait instance to handle any Self substitutions
                let folded_trait = assoc_ty.trait_.fold_with(self);

                // Check if this associated type belongs to our trait instance
                if assoc_ty.trait_.def(self.db) == self.trait_inst.def(self.db) {
                    // Check if we have a binding for this associated type
                    if let Some(&bound_ty) = self
                        .trait_inst
                        .assoc_type_bindings(self.db)
                        .get(&assoc_ty.name)
                    {
                        return bound_ty.fold_with(self);
                    }
                }

                // If the trait instance changed due to Self substitution, create a new associated type
                if folded_trait != assoc_ty.trait_ {
                    return TyId::assoc_ty(self.db, folded_trait, assoc_ty.name);
                }

                // Continue with default folding
                ty.super_fold_with(self)
            }
            _ => ty.super_fold_with(self),
        }
    }
}
