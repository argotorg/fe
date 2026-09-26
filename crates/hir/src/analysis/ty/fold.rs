use std::hash::Hash;

use crate::core::hir_def::IdentId;
use crate::hir_def::scope_graph::ScopeId;
use common::indexmap::{IndexMap, IndexSet};

use super::{
    trait_def::{ImplementorId, TraitInstId, TraitRefId},
    trait_resolution::{PredicateListId, TraitGoalSolution, TraitSolverQuery},
    ty_check::{EffectArg, ExprProp, LocalBinding, ResolvedEffectArg},
    ty_def::{TyData, TyId},
    visitor::TyVisitable,
};
use crate::analysis::{
    HirAnalysisDb,
    place::{Place, PlaceBase, PlaceProjection},
    semantic::{
        EffectProviderSubst, GenericSubst, ImplEnv, SemConstId, SemConstValue, SemanticInstanceKey,
        sem_const_from_ty,
    },
    ty::const_expr::{ConstExpr, ConstExprId, ConstInvocation},
    ty::const_ty::{ConstCaptureEnv, ConstTyData, ConstTyId, const_ty_from_sem_const},
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

    fn fold_scope(&mut self, scope: ScopeId<'db>) -> ScopeId<'db> {
        scope
    }

    fn fold_const_capture(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        capture: &ConstCaptureEnv<'db>,
    ) -> ConstCaptureEnv<'db>
    where
        Self: Sized,
    {
        capture.fold_ranges(db, self)
    }

    fn fold_ty_app(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        abs: TyId<'db>,
        arg: TyId<'db>,
    ) -> TyId<'db> {
        TyId::app_structural(db, abs, arg)
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
                    Value(value) => {
                        const_ty_from_sem_const(db, fold_sem_const(db, folder, value.value()))
                            .data(db)
                            .clone()
                    }
                    Description(value) => {
                        const_ty_from_sem_const(db, fold_sem_const(db, folder, *value))
                            .data(db)
                            .clone()
                    }
                    Computation {
                        description,
                        source,
                    } => Computation {
                        description: Box::new(description.as_ref().clone().fold_with(db, folder)),
                        source: source.fold_with(db, folder),
                    },
                    Invalid(ty) => Invalid(folder.fold_ty(db, *ty)),
                    Abstract(expr, ty) => {
                        let ty = folder.fold_ty(db, *ty);
                        let expr = fold_const_expr_id(db, folder, *expr);
                        Abstract(expr, ty)
                    }
                    UnEvaluated {
                        body,
                        ty,
                        template_ty,
                        const_def,
                        capture,
                        policy,
                    } => {
                        let ty = ty.map(|t| folder.fold_ty(db, t));
                        let capture = folder.fold_const_capture(db, capture);
                        UnEvaluated {
                            body: *body,
                            ty,
                            template_ty: *template_ty,
                            const_def: *const_def,
                            capture,
                            policy: *policy,
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

fn fold_sem_const<'db, F>(
    db: &'db dyn HirAnalysisDb,
    folder: &mut F,
    value: SemConstId<'db>,
) -> SemConstId<'db>
where
    F: TyFolder<'db>,
{
    if let SemConstValue::Description(term) = value.value(db) {
        let folded = folder.fold_ty(db, TyId::const_ty(db, term));
        let TyData::ConstTy(term) = folded.data(db) else {
            unreachable!("folding a dependent description lost its constant representation")
        };
        return sem_const_from_ty(db, folded)
            .unwrap_or_else(|| SemConstId::new(db, SemConstValue::Description(*term)));
    }
    let value = match value.value(db) {
        SemConstValue::Unit => SemConstValue::Unit,
        SemConstValue::Scalar { ty, value } => SemConstValue::Scalar {
            ty: folder.fold_ty(db, ty),
            value,
        },
        SemConstValue::Description(..) => unreachable!(),
        SemConstValue::Tuple { ty, elems } => SemConstValue::Tuple {
            ty: folder.fold_ty(db, ty),
            elems: elems
                .iter()
                .copied()
                .map(|elem| fold_sem_const(db, folder, elem))
                .collect(),
        },
        SemConstValue::Struct { ty, fields } => SemConstValue::Struct {
            ty: folder.fold_ty(db, ty),
            fields: fields
                .iter()
                .copied()
                .map(|field| fold_sem_const(db, folder, field))
                .collect(),
        },
        SemConstValue::Array { ty, elems } => SemConstValue::Array {
            ty: folder.fold_ty(db, ty),
            elems: elems
                .iter()
                .copied()
                .map(|elem| fold_sem_const(db, folder, elem))
                .collect(),
        },
        SemConstValue::Enum {
            ty,
            variant,
            fields,
        } => SemConstValue::Enum {
            ty: folder.fold_ty(db, ty),
            variant,
            fields: fields
                .iter()
                .copied()
                .map(|field| fold_sem_const(db, folder, field))
                .collect(),
        },
    };
    SemConstId::new(db, value)
}

impl<'db> TyFoldable<'db> for SemanticInstanceKey<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let owner = match self.owner(db) {
            super::ty_check::BodyOwner::AnonConstBody { body, expected } => {
                super::ty_check::BodyOwner::AnonConstBody {
                    body,
                    expected: expected.fold_with(db, folder),
                }
            }
            owner => owner,
        };
        let env = self.impl_env(db);
        Self::new(
            db,
            owner,
            GenericSubst::new(
                db,
                self.subst(db)
                    .mapping(db)
                    .as_ref()
                    .map(|mapping| mapping.map_values(|value| value.fold_with(db, folder))),
            ),
            EffectProviderSubst::new(
                db,
                self.effect_providers(db)
                    .providers(db)
                    .clone()
                    .fold_with(db, folder),
            ),
            ImplEnv::new(
                db,
                folder.fold_scope(env.normalization_scope(db)),
                env.assumptions(db).fold_with(db, folder),
                env.witnesses(db).clone().fold_with(db, folder),
            ),
        )
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
        ConstExpr::Invocation(invocation) => ConstExprId::new(
            db,
            ConstExpr::Invocation(ConstInvocation {
                key: invocation.key.fold_with(db, folder),
                args: invocation.args.clone().fold_with(db, folder),
                parameter_owner: folder.fold_scope(invocation.parameter_owner),
            }),
        ),
        ConstExpr::ArithBinOp { op, mode, lhs, rhs } => {
            let lhs = folder.fold_ty(db, *lhs);
            let rhs = folder.fold_ty(db, *rhs);
            ConstExprId::new(
                db,
                ConstExpr::ArithBinOp {
                    op: *op,
                    mode: *mode,
                    lhs,
                    rhs,
                },
            )
        }
        ConstExpr::UnOp { op, mode, expr } => {
            let expr = folder.fold_ty(db, *expr);
            ConstExprId::new(
                db,
                ConstExpr::UnOp {
                    op: *op,
                    mode: *mode,
                    expr,
                },
            )
        }
        ConstExpr::Cast { expr, to } => {
            let expr = folder.fold_ty(db, *expr);
            let to = folder.fold_ty(db, *to);
            ConstExprId::new(db, ConstExpr::Cast { expr, to })
        }
        ConstExpr::ArrayRepeat { value, len } => ConstExprId::new(
            db,
            ConstExpr::ArrayRepeat {
                value: folder.fold_ty(db, *value),
                len: folder.fold_ty(db, *len),
            },
        ),
        ConstExpr::ArrayIndex { array, index } => ConstExprId::new(
            db,
            ConstExpr::ArrayIndex {
                array: folder.fold_ty(db, *array),
                index: folder.fold_ty(db, *index),
            },
        ),
        ConstExpr::Field { value, index } => ConstExprId::new(
            db,
            ConstExpr::Field {
                value: folder.fold_ty(db, *value),
                index: *index,
            },
        ),
        ConstExpr::TraitConst(assoc) => {
            let assoc = assoc.fold_with(db, folder);
            ConstExprId::new(db, ConstExpr::TraitConst(assoc))
        }
        ConstExpr::InherentConst(use_) => {
            let use_ = use_.fold_with(db, folder);
            ConstExprId::new(db, ConstExpr::InherentConst(use_))
        }
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

impl<'db> TyFoldable<'db> for TraitRefId<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        TraitRefId::new(
            db,
            self.def(db),
            self.args(db)
                .iter()
                .map(|ty| ty.fold_with(db, folder))
                .collect::<Vec<_>>(),
        )
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
            require_impl: self.require_impl,
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
            PlaceProjection::Deref { result_ty } => PlaceProjection::Deref {
                result_ty: result_ty.fold_with(db, folder),
            },
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
            with_source: self.with_source,
            pass_mode: self.pass_mode,
            layout_view: self.layout_view,
            required_mut: self.required_mut,
            key_kind: self.key_kind,
            instantiated_key_ty: self.instantiated_key_ty.map(|ty| ty.fold_with(db, folder)),
            provider_target_ty: self.provider_target_ty.map(|ty| ty.fold_with(db, folder)),
            provider: self.provider,
        }
    }
}
