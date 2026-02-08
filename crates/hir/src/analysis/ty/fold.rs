use std::hash::Hash;

use crate::core::hir_def::IdentId;
use crate::hir_def::{ItemKind, Trait};
use common::indexmap::{IndexMap, IndexSet};

use super::{
    trait_def::{ImplementorId, TraitInstId},
    trait_resolution::{PredicateListId, TraitGoalSolution},
    ty_check::ExprProp,
    ty_def::{TyData, TyId},
    visitor::TyVisitable,
};
use crate::analysis::{
    HirAnalysisDb,
    ty::const_expr::{ConstExpr, ConstExprId},
    ty::const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
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
                        let val = match val {
                            EvaluatedConstTy::Tuple(elems) => EvaluatedConstTy::Tuple(
                                elems
                                    .iter()
                                    .copied()
                                    .map(|elem| folder.fold_ty(db, elem))
                                    .collect(),
                            ),
                            EvaluatedConstTy::Array(elems) => EvaluatedConstTy::Array(
                                elems
                                    .iter()
                                    .copied()
                                    .map(|elem| folder.fold_ty(db, elem))
                                    .collect(),
                            ),
                            EvaluatedConstTy::Record(fields) => EvaluatedConstTy::Record(
                                fields
                                    .iter()
                                    .copied()
                                    .map(|field| folder.fold_ty(db, field))
                                    .collect(),
                            ),
                            _ => val.clone(),
                        };
                        Evaluated(val, ty)
                    }
                    Abstract(expr, ty) => {
                        let ty = folder.fold_ty(db, *ty);
                        let expr = fold_const_expr_id(db, folder, *expr);
                        Abstract(expr, ty)
                    }
                    UnEvaluated {
                        body,
                        ty,
                        const_def,
                        generic_args,
                    } => {
                        let ty = ty.map(|t| folder.fold_ty(db, t));
                        let generic_args = generic_args
                            .iter()
                            .copied()
                            .map(|arg| folder.fold_ty(db, arg))
                            .collect();
                        UnEvaluated {
                            body: *body,
                            ty,
                            const_def: *const_def,
                            generic_args,
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

fn fold_const_expr_id<'db, F>(
    db: &'db dyn HirAnalysisDb,
    folder: &mut F,
    expr: ConstExprId<'db>,
) -> ConstExprId<'db>
where
    F: TyFolder<'db>,
{
    match expr.data(db) {
        ConstExpr::ExternConstFnCall {
            func,
            generic_args,
            args,
        } => {
            let generic_args = generic_args
                .iter()
                .copied()
                .map(|arg| folder.fold_ty(db, arg))
                .collect();
            let args = args
                .iter()
                .copied()
                .map(|arg| folder.fold_ty(db, arg))
                .collect();
            ConstExprId::new(
                db,
                ConstExpr::ExternConstFnCall {
                    func: *func,
                    generic_args,
                    args,
                },
            )
        }
        ConstExpr::UserConstFnCall {
            func,
            generic_args,
            args,
        } => {
            let generic_args = generic_args
                .iter()
                .copied()
                .map(|arg| folder.fold_ty(db, arg))
                .collect();
            let args = args
                .iter()
                .copied()
                .map(|arg| folder.fold_ty(db, arg))
                .collect();
            ConstExprId::new(
                db,
                ConstExpr::UserConstFnCall {
                    func: *func,
                    generic_args,
                    args,
                },
            )
        }
        ConstExpr::ArithBinOp { op, lhs, rhs } => {
            let lhs = folder.fold_ty(db, *lhs);
            let rhs = folder.fold_ty(db, *rhs);
            ConstExprId::new(db, ConstExpr::ArithBinOp { op: *op, lhs, rhs })
        }
        ConstExpr::UnOp { op, expr } => {
            let expr = folder.fold_ty(db, *expr);
            ConstExprId::new(db, ConstExpr::UnOp { op: *op, expr })
        }
        ConstExpr::Cast { expr, to } => {
            let expr = folder.fold_ty(db, *expr);
            let to = folder.fold_ty(db, *to);
            ConstExprId::new(db, ConstExpr::Cast { expr, to })
        }
        ConstExpr::TraitConst { inst, name } => {
            let inst = inst.fold_with(db, folder);
            ConstExprId::new(db, ConstExpr::TraitConst { inst, name: *name })
        }
        ConstExpr::LocalBinding(binding) => ConstExprId::new(db, ConstExpr::LocalBinding(*binding)),
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

impl<'db> TyFoldable<'db> for ImplementorId<'db> {
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
        let origin = self.origin(db);

        let types = self
            .types(db)
            .iter()
            .map(|(ident, ty)| (*ident, ty.fold_with(db, folder)))
            .collect::<IndexMap<_, _>>();

        ImplementorId::new(db, trait_inst, params, types, origin)
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

impl<'db> TyFoldable<'db> for TraitGoalSolution<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            inst: self.inst.fold_with(db, folder),
            implementor: self.implementor.fold_with(db, folder),
        }
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
                    let owner_trait = param.owner.resolve_to::<Trait>(db).or_else(|| {
                        match param.owner.parent_item(db)? {
                            ItemKind::Trait(trait_) => Some(trait_),
                            _ => None,
                        }
                    });
                    if owner_trait.is_some_and(|trait_def| trait_def == self.trait_inst.def(db)) {
                        let self_ty = self.trait_inst.self_ty(db);
                        // Avoid infinite recursion when the instance `Self` is the same param.
                        if self_ty == ty {
                            return ty;
                        }
                        return self_ty.fold_with(db, self);
                    }
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
