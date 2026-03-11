use crate::analysis::HirAnalysisDb;
use crate::analysis::name_resolution::PathRes;
use crate::analysis::ty::const_ty::{
    CallableInputHoleCx, ConstTyData, LayoutHoleArgSite, LayoutHoleId,
};
use crate::analysis::ty::fold::{AssocTySubst, TyFoldable, TyFolder};
use crate::analysis::ty::layout_holes::layout_hole_with_fallback_ty;
use crate::analysis::ty::trait_def::TraitInstId;
use crate::analysis::ty::trait_resolution::PredicateListId;
use crate::analysis::ty::ty_def::{TyBase, TyData, TyId};
use crate::analysis::ty::ty_lower::{collect_generic_params, func_implicit_param_plan};
use crate::hir_def::scope_graph::ScopeId;
use crate::hir_def::{CallableDef, Func, PathId};

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
    resolve_normalized_type_effect_key_with_callable_holes(db, key_path, scope, assumptions, None)
}

pub(crate) fn resolve_normalized_type_effect_key_with_callable_holes<'db>(
    db: &'db dyn HirAnalysisDb,
    key_path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    callable_holes: Option<&CallableInputHoleCx<'db>>,
) -> Option<TyId<'db>> {
    match crate::analysis::name_resolution::resolve_path_with_callable_holes(
        db,
        key_path,
        scope,
        assumptions,
        false,
        callable_holes,
    ) {
        Ok(PathRes::Ty(ty)) if ty.is_star_kind(db) => Some(
            existentialize_omitted_const_args_in_effect_key(db, key_path, ty, callable_holes),
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
    resolve_effect_key_with_callable_holes(db, key_path, scope, assumptions, None)
}

pub(crate) fn resolve_effect_key_with_callable_holes<'db>(
    db: &'db dyn HirAnalysisDb,
    key_path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    callable_holes: Option<&CallableInputHoleCx<'db>>,
) -> ResolvedEffectKey<'db> {
    if let Some(ty) = resolve_normalized_type_effect_key_with_callable_holes(
        db,
        key_path,
        scope,
        assumptions,
        callable_holes,
    ) {
        return ResolvedEffectKey::Type(ty);
    }

    match crate::analysis::name_resolution::resolve_path_with_callable_holes(
        db,
        key_path,
        scope,
        assumptions,
        false,
        callable_holes,
    ) {
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
    callable_holes: Option<&CallableInputHoleCx<'db>>,
) -> TyId<'db> {
    let (base, args) = ty.decompose_ty_app(db);
    let TyData::TyBase(base_ty) = base.data(db) else {
        return ty;
    };

    let (param_set, offset) = match base_ty {
        TyBase::Adt(adt) => {
            let set = *adt.param_set(db);
            (set, set.offset_to_explicit_params_position(db))
        }
        TyBase::Func(func) => match *func {
            CallableDef::Func(def) => {
                let set = collect_generic_params(db, def.into());
                (set, set.offset_to_explicit_params_position(db))
            }
            CallableDef::VariantCtor(_) => return ty,
        },
        _ => return ty,
    };
    let explicit_param_count = param_set.explicit_param_count(db);
    if explicit_param_count == 0 {
        return ty;
    }

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
            callable_holes
                .map(CallableInputHoleCx::fresh_hole_id)
                .unwrap_or(LayoutHoleId::ExplicitArg {
                    site: LayoutHoleArgSite::Path(key_path),
                    arg_idx,
                }),
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

pub(crate) fn instantiate_trait_effect_requirement<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_inst: TraitInstId<'db>,
    callee_generic_args: &[TyId<'db>],
    provided_ty: TyId<'db>,
    assoc_ty_subst: Option<TraitInstId<'db>>,
) -> TraitInstId<'db> {
    struct InstantiateCalleeArgs<'db, 'a> {
        args: &'a [TyId<'db>],
    }

    impl<'db> TyFolder<'db> for InstantiateCalleeArgs<'db, '_> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            match ty.data(db) {
                TyData::TyParam(param) if !param.is_effect() && !param.is_trait_self() => {
                    self.args.get(param.idx).copied().unwrap_or(ty)
                }
                TyData::ConstTy(const_ty) => {
                    if let ConstTyData::TyParam(param, _) = const_ty.data(db)
                        && !param.is_effect()
                        && !param.is_trait_self()
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

    let mut instantiation = InstantiateCalleeArgs {
        args: callee_generic_args,
    };
    let trait_inst = trait_inst.fold_with(db, &mut instantiation);

    let mut self_subst = SelfSubst {
        self_subst: provided_ty,
    };
    let mut trait_req = trait_inst.fold_with(db, &mut self_subst);

    if let Some(inst) = assoc_ty_subst {
        let mut subst = AssocTySubst::new(inst);
        trait_req = trait_req.fold_with(db, &mut subst);
    }

    trait_req
}
