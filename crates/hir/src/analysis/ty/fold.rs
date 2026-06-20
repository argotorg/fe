use std::hash::Hash;

use crate::core::hir_def::IdentId;
use crate::hir_def::{ItemKind, Trait};
use common::indexmap::{IndexMap, IndexSet};

use super::{
    trait_def::{ImplementorId, TraitInstId},
    trait_resolution::{PredicateListId, TraitGoalSolution, TraitSolverQuery},
    ty_check::{EffectArg, ExprProp, LocalBinding, ResolvedEffectArg},
    ty_def::{TyData, TyId},
    visitor::TyVisitable,
};
use crate::analysis::{
    HirAnalysisDb,
    place::{Place, PlaceBase, PlaceProjection},
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

    fn fold_ty_app(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        abs: TyId<'db>,
        arg: TyId<'db>,
    ) -> TyId<'db> {
        TyId::app(db, abs, arg)
    }
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

                folder.fold_ty_app(db, abs, arg)
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
                    Hole(ty, hole_id) => {
                        let ty = folder.fold_ty(db, *ty);
                        Hole(ty, *hole_id)
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
                        preserve_unevaluated,
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
                            preserve_unevaluated: *preserve_unevaluated,
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
        ConstExpr::TraitConst(assoc) => {
            let assoc = assoc.fold_with(db, folder);
            ConstExprId::new(db, ConstExpr::TraitConst(assoc))
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

impl<'db> TyFoldable<'db> for TraitSolverQuery<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            goal: self.goal.fold_with(db, folder),
            assumptions: self.assumptions.fold_with(db, folder),
        }
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

impl<'db> TyFoldable<'db> for LocalBinding<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        match self {
            LocalBinding::Local { .. } | LocalBinding::EffectParam { .. } => self,
            LocalBinding::Param {
                site,
                idx,
                mode,
                ty,
                is_mut,
            } => LocalBinding::Param {
                site,
                idx,
                mode,
                ty: ty.fold_with(db, folder),
                is_mut,
            },
        }
    }
}

impl<'db> TyFoldable<'db> for ExprProp<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let ty = self.ty.fold_with(db, folder);
        let binding = self.binding.map(|binding| binding.fold_with(db, folder));
        Self {
            ty,
            binding,
            ..self
        }
    }
}

impl<'db> TyFoldable<'db> for Place<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let base = self.base.fold_with(db, folder);
        let projections = self
            .projections
            .into_iter()
            .map(|projection| projection.fold_with(db, folder))
            .collect();
        Self { base, projections }
    }
}

impl<'db> TyFoldable<'db> for PlaceProjection<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        match self {
            PlaceProjection::Field { index, result_ty } => PlaceProjection::Field {
                index,
                result_ty: result_ty.fold_with(db, folder),
            },
            PlaceProjection::Index {
                index_expr,
                result_ty,
            } => PlaceProjection::Index {
                index_expr,
                result_ty: result_ty.fold_with(db, folder),
            },
        }
    }
}

impl<'db> TyFoldable<'db> for PlaceBase<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        match self {
            PlaceBase::Binding(binding) => PlaceBase::Binding(binding.fold_with(db, folder)),
        }
    }
}

impl<'db> TyFoldable<'db> for EffectArg<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        match self {
            EffectArg::Place(place) => EffectArg::Place(place.fold_with(db, folder)),
            EffectArg::Binding(binding) => EffectArg::Binding(binding.fold_with(db, folder)),
            EffectArg::Value(_) | EffectArg::Unknown => self,
        }
    }
}

impl<'db> TyFoldable<'db> for ResolvedEffectArg<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            param_idx: self.param_idx,
            binding_idx: self.binding_idx,
            key: self.key,
            arg: self.arg.fold_with(db, folder),
            pass_mode: self.pass_mode,
            required_mut: self.required_mut,
            key_kind: self.key_kind,
            instantiated_key_ty: self.instantiated_key_ty.map(|ty| ty.fold_with(db, folder)),
            provider_target_ty: self.provider_target_ty.map(|ty| ty.fold_with(db, folder)),
            provider: self.provider,
        }
    }
}

/// Returns true if `needle` occurs anywhere within `haystack`.
fn ty_contains<'db>(db: &'db dyn HirAnalysisDb, haystack: TyId<'db>, needle: TyId<'db>) -> bool {
    use super::visitor::{TyVisitor, walk_ty};

    struct ContainsVisitor<'db> {
        db: &'db dyn HirAnalysisDb,
        needle: TyId<'db>,
        found: bool,
    }

    impl<'db> TyVisitor<'db> for ContainsVisitor<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.found {
                return;
            }
            if ty == self.needle {
                self.found = true;
                return;
            }
            walk_ty(self, ty);
        }
    }

    let mut visitor = ContainsVisitor {
        db,
        needle,
        found: false,
    };
    haystack.visit_with(&mut visitor);
    visitor.found
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
                        // Occurs check: when the replacement itself contains this `Self`
                        // param (e.g. the instance's self type is a projection rooted at
                        // `Self`, as in a `Self::Item: Trait` assumption), re-folding it
                        // would substitute forever. The replacement is already expressed
                        // in the outer context, so return it as-is.
                        if ty_contains(db, self_ty, ty) {
                            return self_ty;
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
