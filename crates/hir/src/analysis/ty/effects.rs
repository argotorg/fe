use crate::analysis::HirAnalysisDb;
use crate::analysis::name_resolution::PathRes;
use crate::analysis::ty::const_ty::{
    ConstTyData, HoleId, LocalFrameId, StructuralHoleOrigin, const_ty_from_trait_const,
    normalize_const_tys_for_comparison,
};
use crate::analysis::ty::fold::{AssocTySubst, TyFoldable, TyFolder};
use crate::analysis::ty::layout_holes::layout_hole_with_fallback_ty;
use crate::analysis::ty::normalize::normalize_ty;
use crate::analysis::ty::trait_def::TraitInstId;
use crate::analysis::ty::trait_resolution::PredicateListId;
use crate::analysis::ty::trait_resolution::TraitSolveCx;
use crate::analysis::ty::ty_def::{InvalidCause, TyBase, TyData, TyId};
use crate::analysis::ty::ty_lower::{collect_generic_params, func_implicit_param_plan};
use crate::core::hir_def::GenericParamOwner;
use crate::hir_def::scope_graph::ScopeId;
use crate::hir_def::{CallableDef, Func, PathId};
use common::indexmap::IndexMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EffectKeyKind {
    Type,
    Trait,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum ResolvedEffectKey<'db> {
    Type(TyId<'db>),
    Trait(TraitInstId<'db>),
    Other,
}

impl<'db> ResolvedEffectKey<'db> {
    pub(crate) fn into_parts(self) -> (EffectKeyKind, Option<TyId<'db>>, Option<TraitInstId<'db>>) {
        match self {
            Self::Type(ty) => (EffectKeyKind::Type, Some(ty), None),
            Self::Trait(trait_inst) => (EffectKeyKind::Trait, None, Some(trait_inst)),
            Self::Other => (EffectKeyKind::Other, None, None),
        }
    }
}

/// Returns a per-effect mapping from effect index → hidden provider generic-arg index.
#[salsa::tracked(return_ref)]
pub fn place_effect_provider_param_index_map<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<Option<usize>> {
    func_implicit_param_plan(db, func)
        .provider_param_index_by_effect
        .clone()
}

/// Resolves a type effect key path and applies effect-key normalization.
///
/// Normalization currently means existentializing omitted trailing const args only
/// when the omitted const parameter defaults to a layout hole (`_`).
pub(crate) fn resolve_normalized_type_effect_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key_path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<TyId<'db>> {
    match crate::analysis::name_resolution::resolve_path(db, key_path, scope, assumptions, false) {
        Ok(PathRes::Ty(ty)) if ty.is_star_kind(db) => Some(
            existentialize_omitted_const_args_in_effect_key(db, key_path, ty),
        ),
        Ok(PathRes::TyAlias(_, ty)) if ty.is_star_kind(db) => Some(ty),
        _ => None,
    }
}

pub(crate) fn resolve_effect_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key_path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> ResolvedEffectKey<'db> {
    if let Some(ty) = resolve_normalized_type_effect_key(db, key_path, scope, assumptions) {
        return ResolvedEffectKey::Type(ty);
    }

    match crate::analysis::name_resolution::resolve_path(db, key_path, scope, assumptions, false) {
        Ok(PathRes::Trait(trait_inst)) => ResolvedEffectKey::Trait(trait_inst),
        _ => ResolvedEffectKey::Other,
    }
}

/// Replaces omitted trailing const generic arguments in a type effect key with typed holes.
///
/// Example: for `uses (map: StorageMap<K, V>)`, where `StorageMap` has
/// `const SALT: u256 = ...`, this returns `StorageMap<K, V, _>` so later lowering can
/// bind that const as an effect-specific inference variable.
pub(crate) fn existentialize_omitted_const_args_in_effect_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key_path: PathId<'db>,
    ty: TyId<'db>,
) -> TyId<'db> {
    let (base, args) = ty.decompose_ty_app(db);
    let TyData::TyBase(base_ty) = base.data(db) else {
        return ty;
    };

    let (param_set, offset, owner) = match base_ty {
        TyBase::Adt(adt) => {
            let set = *adt.param_set(db);
            (
                set,
                set.offset_to_explicit_params_position(db),
                GenericParamOwner::from_item_opt(adt.scope(db).item()),
            )
        }
        TyBase::Func(func) => match *func {
            CallableDef::Func(def) => {
                let set = collect_generic_params(db, def.into());
                (
                    set,
                    set.offset_to_explicit_params_position(db),
                    Some(def.into()),
                )
            }
            CallableDef::VariantCtor(_) => return ty,
        },
        _ => return ty,
    };
    let explicit_param_count = param_set.explicit_param_count(db);
    if explicit_param_count == 0 {
        return ty;
    }
    let Some(owner) = owner else {
        return ty;
    };

    let provided_explicit_len = key_path
        .generic_args(db)
        .data(db)
        .len()
        .min(explicit_param_count);
    if provided_explicit_len >= explicit_param_count {
        return ty;
    }

    let mut completed_args = args.to_vec();
    let mut changed = false;
    for explicit_idx in provided_explicit_len..explicit_param_count {
        let Some(const_ty_ty) = param_set.explicit_const_param_default_hole_ty(db, explicit_idx)
        else {
            continue;
        };

        let arg_idx = offset + explicit_idx;
        if arg_idx >= completed_args.len() {
            continue;
        }
        let hole = layout_hole_with_fallback_ty(
            db,
            const_ty_ty,
            HoleId::structural(
                db,
                const_ty_ty,
                StructuralHoleOrigin::EffectKeyExistential {
                    path: key_path,
                    arg_idx,
                    owner,
                    param_idx: explicit_idx,
                },
                LocalFrameId::root_path(db, key_path),
            ),
        );
        if completed_args[arg_idx] != hole {
            completed_args[arg_idx] = hole;
            changed = true;
        }
    }

    if !changed {
        return ty;
    }

    TyId::foldl(db, base, &completed_args)
}

pub(crate) fn instantiate_trait_effect_key<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_key: TraitInstId<'db>,
    callee_generic_args: &[TyId<'db>],
    assoc_ty_subst: Option<TraitInstId<'db>>,
) -> TraitInstId<'db> {
    let mut trait_key = instantiate_effect_key_value(db, trait_key, callee_generic_args, false);

    if let Some(inst) = assoc_ty_subst {
        let mut subst = AssocTySubst::new(inst);
        trait_key = trait_key.fold_with(db, &mut subst);
    }

    trait_key
}

pub(crate) fn instantiate_trait_effect_lookup_key<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_key: TraitInstId<'db>,
    callee_generic_args: &[TyId<'db>],
    assoc_ty_subst: Option<TraitInstId<'db>>,
) -> TraitInstId<'db> {
    let mut trait_key = instantiate_effect_key_value(db, trait_key, callee_generic_args, true);

    if let Some(inst) = assoc_ty_subst {
        let mut subst = AssocTySubst::new(inst);
        trait_key = trait_key.fold_with(db, &mut subst);
    }

    trait_key
}

pub(crate) fn instantiate_type_effect_lookup_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key_ty: TyId<'db>,
    callee_generic_args: &[TyId<'db>],
) -> TyId<'db> {
    instantiate_effect_key_value(db, key_ty, callee_generic_args, true)
}

pub(crate) fn instantiate_type_effect_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key_ty: TyId<'db>,
    callee_generic_args: &[TyId<'db>],
) -> TyId<'db> {
    instantiate_effect_key_value(db, key_ty, callee_generic_args, false)
}

fn instantiate_effect_key_value<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    callee_generic_args: &[TyId<'db>],
    preserve_implicit_const_params: bool,
) -> T
where
    T: TyFoldable<'db>,
{
    struct InstantiateCalleeArgs<'db, 'a> {
        args: &'a [TyId<'db>],
        preserve_implicit_const_params: bool,
    }

    impl<'db> TyFolder<'db> for InstantiateCalleeArgs<'db, '_> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            match ty.data(db) {
                TyData::TyParam(param)
                    if !param.is_effect() && !param.is_trait_self() && !param.is_implicit() =>
                {
                    self.args.get(param.idx).copied().unwrap_or(ty)
                }
                TyData::ConstTy(const_ty) => {
                    if let ConstTyData::TyParam(param, _) = const_ty.data(db)
                        && !param.is_effect()
                        && !param.is_trait_self()
                        && (!self.preserve_implicit_const_params || !param.is_implicit())
                        && let Some(arg) = self.args.get(param.idx)
                    {
                        *arg
                    } else {
                        ty.super_fold_with(db, self)
                    }
                }
                _ => ty.super_fold_with(db, self),
            }
        }
    }

    value.fold_with(
        db,
        &mut InstantiateCalleeArgs {
            args: callee_generic_args,
            preserve_implicit_const_params,
        },
    )
}

pub(crate) fn normalize_effect_identity_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    assoc_ty_subst: Option<TraitInstId<'db>>,
) -> TyId<'db> {
    let ty = if let Some(inst) = assoc_ty_subst {
        let mut substituter = AssocTySubst::new(inst);
        ty.fold_with(db, &mut substituter)
    } else {
        ty
    };
    let ty = normalize_ty(db, ty, scope, assumptions);
    normalize_effect_identity_const_tys(
        db,
        ty,
        TraitSolveCx::new(db, scope).with_assumptions(assumptions),
    )
}

pub(crate) fn normalize_effect_identity_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_key: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    assoc_ty_subst: Option<TraitInstId<'db>>,
) -> TraitInstId<'db> {
    let original_self = trait_key.self_ty(db);
    let preserve_self = assoc_ty_subst.is_some_and(|inst| inst.def(db) == trait_key.def(db));
    let trait_key = if let Some(inst) = assoc_ty_subst {
        let mut substituter = AssocTySubst::new(inst);
        trait_key.fold_with(db, &mut substituter)
    } else {
        trait_key
    };
    let mut args: Vec<TyId<'db>> = trait_key
        .args(db)
        .iter()
        .copied()
        .map(|ty| normalize_effect_identity_ty(db, ty, scope, assumptions, None))
        .collect();
    if preserve_self && let Some(self_ty) = args.first_mut() {
        *self_ty = normalize_effect_identity_ty(db, original_self, scope, assumptions, None);
    }
    let mut assoc_type_bindings: Vec<_> = trait_key
        .assoc_type_bindings(db)
        .iter()
        .map(|(name, &ty)| {
            (
                *name,
                normalize_effect_identity_ty(db, ty, scope, assumptions, None),
            )
        })
        .collect();
    assoc_type_bindings.sort_by(|(lhs, _), (rhs, _)| lhs.data(db).cmp(rhs.data(db)));
    let assoc_type_bindings: IndexMap<_, _> = assoc_type_bindings.into_iter().collect();
    TraitInstId::new(db, trait_key.def(db), args, assoc_type_bindings)
}

fn normalize_effect_identity_const_tys<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    solve_cx: TraitSolveCx<'db>,
) -> TyId<'db> {
    let ty = normalize_const_tys_for_comparison(db, ty);

    struct ConstFolder<'db> {
        solve_cx: TraitSolveCx<'db>,
    }

    impl<'db> TyFolder<'db> for ConstFolder<'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            let TyData::ConstTy(const_ty) = ty.data(db) else {
                return ty.super_fold_with(db, self);
            };

            match const_ty.data(db) {
                ConstTyData::Abstract(expr, expected_ty) => {
                    let crate::analysis::ty::const_expr::ConstExpr::TraitConst { inst, name } =
                        expr.data(db)
                    else {
                        return ty.super_fold_with(db, self);
                    };

                    let Some(const_ty) = const_ty_from_trait_const(db, self.solve_cx, *inst, *name)
                    else {
                        return ty.super_fold_with(db, self);
                    };

                    let evaluated = const_ty.evaluate(db, Some(*expected_ty));
                    if matches!(
                        evaluated.ty(db).invalid_cause(db),
                        Some(InvalidCause::ConstEvalUnsupported { .. })
                    ) {
                        ty.super_fold_with(db, self)
                    } else {
                        TyId::const_ty(db, evaluated)
                    }
                }
                _ => ty.super_fold_with(db, self),
            }
        }
    }

    ty.fold_with(db, &mut ConstFolder { solve_cx })
}

pub(crate) fn instantiate_trait_effect_goal<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_key: TraitInstId<'db>,
    provided_ty: TyId<'db>,
) -> TraitInstId<'db> {
    struct SelfSubst<'db> {
        self_subst: TyId<'db>,
    }

    impl<'db> TyFolder<'db> for SelfSubst<'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            match ty.data(db) {
                TyData::TyParam(p) if p.is_trait_self() => self.self_subst,
                _ => ty.super_fold_with(db, self),
            }
        }
    }

    trait_key.fold_with(
        db,
        &mut SelfSubst {
            self_subst: provided_ty,
        },
    )
}
