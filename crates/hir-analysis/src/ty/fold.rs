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
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>;

    fn fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        self.super_fold_with(db, folder)
    }
}

pub trait TyFolder<'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db>;
}

impl<'db> TyFoldable<'db> for TyId<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        use TyData::*;

        match self.data(db) {
            TyApp(abs, arg) => {
                let abs = folder.fold_ty(db, *abs);
                let arg = folder.fold_ty(db, *arg);

                TyId::app(db, abs, arg)
            }

            ConstTy(cty) => {
                use ConstTyData::*;
                let cty_data = match cty.data(db) {
                    TyVar(var, ty) => {
                        let ty = folder.fold_ty(db, *ty);
                        TyVar(var.clone(), ty)
                    }
                    TyParam(param, ty) => {
                        let ty = folder.fold_ty(db, *ty);
                        TyParam(param.clone(), ty)
                    }
                    Evaluated(val, ty) => {
                        let ty = folder.fold_ty(db, *ty);
                        Evaluated(val.clone(), ty)
                    }
                    UnEvaluated {
                        body,
                        ty,
                        const_def,
                    } => {
                        let ty = ty.map(|t| folder.fold_ty(db, t));
                        UnEvaluated {
                            body: *body,
                            ty,
                            const_def: *const_def,
                        }
                    }
                };

                let const_ty = ConstTyId::new(db, cty_data);
                TyId::const_ty(db, const_ty)
            }

            AssocTy(assoc) => {
                let folded_trait = assoc.trait_.fold_with(db, folder);

                TyId::assoc_ty(db, folded_trait, assoc.name)
            }

            QualifiedTy(trait_inst) => {
                let folded_trait = trait_inst.fold_with(db, folder);
                TyId::qualified_ty(db, folded_trait)
            }

            TyVar(_) | TyParam(_) | TyBase(_) | Never | Invalid(_) => self,
        }
    }

    fn fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        folder.fold_ty(db, self)
    }
}

impl<'db, T> TyFoldable<'db> for Vec<T>
where
    T: TyFoldable<'db>,
{
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        self.into_iter()
            .map(|inner| inner.fold_with(db, folder))
            .collect()
    }
}

impl<'db, T> TyFoldable<'db> for IndexSet<T>
where
    T: TyFoldable<'db> + Hash + Eq,
{
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        self.into_iter()
            .map(|ty| ty.fold_with(db, folder))
            .collect()
    }
}

impl<'db> TyFoldable<'db> for TraitInstId<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let def = self.def(db);
        let args = self
            .args(db)
            .iter()
            .map(|ty| ty.fold_with(db, folder))
            .collect::<Vec<_>>();

        let assoc_type_bindings: IndexMap<IdentId<'db>, TyId<'db>> = self
            .assoc_type_bindings(db)
            .iter()
            .map(|(name, ty)| (*name, ty.fold_with(db, folder)))
            .collect();

        TraitInstId::new(db, def, args, assoc_type_bindings)
    }
}

impl<'db> TyFoldable<'db> for Implementor<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let trait_inst = self.trait_(db).fold_with(db, folder);
        let params = self
            .params(db)
            .iter()
            .map(|ty| ty.fold_with(db, folder))
            .collect::<Vec<_>>();
        let hir_impl_trait = self.hir_impl_trait(db);

        let types = self
            .types(db)
            .iter()
            .map(|(ident, ty)| (*ident, ty.fold_with(db, folder)))
            .collect::<IndexMap<_, _>>();

        Implementor::new(db, trait_inst, params, types, hir_impl_trait)
    }
}

impl<'db> TyFoldable<'db> for PredicateListId<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let predicates = self
            .list(db)
            .iter()
            .map(|pred| pred.fold_with(db, folder))
            .collect::<Vec<_>>();

        Self::new(db, predicates)
    }
}

impl<'db> TyFoldable<'db> for ExprProp<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let ty = self.ty.fold_with(db, folder);
        Self { ty, ..self }
    }
}

/// A type folder that substitutes associated types based on a trait instance's bindings
pub struct AssocTySubst<'db> {
    trait_inst: TraitInstId<'db>,
}

impl<'db> AssocTySubst<'db> {
    pub fn new(trait_inst: TraitInstId<'db>) -> Self {
        Self { trait_inst }
    }
}

impl<'db> TyFolder<'db> for AssocTySubst<'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(db) {
            TyData::TyParam(param) => {
                // If this is a trait self parameter, substitute with the trait instance's self type
                if param.is_trait_self() {
                    return self.trait_inst.self_ty(db).fold_with(db, self);
                }
                ty.super_fold_with(db, self)
            }
            TyData::AssocTy(assoc_ty) => {
                // First fold the trait instance to handle any Self substitutions
                let folded_trait = assoc_ty.trait_.fold_with(db, self);

                // Check if this associated type belongs to our trait instance
                if assoc_ty.trait_.def(db) == self.trait_inst.def(db) {
                    // Check if we have a binding for this associated type
                    if let Some(&bound_ty) =
                        self.trait_inst.assoc_type_bindings(db).get(&assoc_ty.name)
                    {
                        return bound_ty.fold_with(db, self);
                    }
                }

                // If the trait instance changed due to Self substitution, create a new associated type
                if folded_trait != assoc_ty.trait_ {
                    return TyId::assoc_ty(db, folded_trait, assoc_ty.name);
                }

                // Continue with default folding
                ty.super_fold_with(db, self)
            }
            _ => ty.super_fold_with(db, self),
        }
    }
}
