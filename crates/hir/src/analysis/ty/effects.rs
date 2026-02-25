use crate::analysis::HirAnalysisDb;
use crate::analysis::name_resolution::{
    NameDomain, PathRes, resolve_ident_to_bucket, resolve_path,
};
use crate::analysis::ty::const_ty::{ConstTyData, ConstTyId};
use crate::analysis::ty::fold::{AssocTySubst, TyFoldable, TyFolder};
use crate::analysis::ty::trait_def::TraitInstId;
use crate::analysis::ty::trait_resolution::PredicateListId;
use crate::analysis::ty::ty_def::{TyBase, TyData, TyId};
use crate::analysis::ty::ty_lower::collect_generic_params;
use crate::hir_def::scope_graph::ScopeId;
use crate::hir_def::{CallableDef, Func, PathId};
use salsa::Update;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum EffectKeyKind {
    Type,
    Trait,
    Other,
}

/// Classifies an effect key path without inspecting its generic arguments.
///
/// This is cached and cycle-recoverable because effect-key classification can be queried while
/// lowering generic arguments (and vice versa).
#[salsa::tracked(cycle_fn=effect_key_kind_cycle_recover, cycle_initial=effect_key_kind_cycle_initial)]
pub(crate) fn effect_key_kind<'db>(
    db: &'db dyn HirAnalysisDb,
    key_path: PathId<'db>,
    scope: ScopeId<'db>,
) -> EffectKeyKind {
    let assumptions = PredicateListId::empty_list(db);
    let stripped_path = key_path.strip_generic_args(db);

    let classify = |res| match res {
        Ok(PathRes::Ty(_) | PathRes::TyAlias(_, _)) => EffectKeyKind::Type,
        Ok(PathRes::Trait(_) | PathRes::TraitMethod(..)) => EffectKeyKind::Trait,
        _ => EffectKeyKind::Other,
    };
    let classify_bucket = |lookup_scope, ident| {
        let path = PathId::from_ident(db, ident);
        if let Ok(res) = resolve_ident_to_bucket(db, path, lookup_scope).pick(NameDomain::TYPE) {
            if res.is_trait() {
                return EffectKeyKind::Trait;
            }
            if res.is_type() {
                return EffectKeyKind::Type;
            }
        }
        EffectKeyKind::Other
    };

    // Prefer classifying the key without lowering generic args to avoid recursive generic-arg
    // lowering cycles.
    let kind = classify(resolve_path(db, stripped_path, scope, assumptions, false));
    if kind != EffectKeyKind::Other {
        return kind;
    }

    let Some(ident) = stripped_path.ident(db).to_opt() else {
        return EffectKeyKind::Other;
    };

    // `resolve_path` rejects generic paths after stripping args (e.g. `Storage<T>` -> `Storage`).
    // Resolve the parent path, then classify the leaf identifier in that scope without touching
    // generic args.
    if let Some(parent_path) = stripped_path.parent(db) {
        if let Ok(parent_res) = resolve_path(db, parent_path, scope, assumptions, false)
            && let Some(parent_scope) = parent_res.as_scope(db)
        {
            return classify_bucket(parent_scope, ident);
        }
        return EffectKeyKind::Other;
    }

    classify_bucket(scope, ident)
}

fn effect_key_kind_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _key_path: PathId<'db>,
    _scope: ScopeId<'db>,
) -> EffectKeyKind {
    EffectKeyKind::Other
}

fn effect_key_kind_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &EffectKeyKind,
    _count: u32,
    _key_path: PathId<'db>,
    _scope: ScopeId<'db>,
) -> salsa::CycleRecoveryAction<EffectKeyKind> {
    salsa::CycleRecoveryAction::Iterate
}

/// Returns a per-effect mapping from effect index → hidden provider generic-arg index.
///
/// Effects whose keys are neither a type nor a trait have `None` entries.
#[salsa::tracked(return_ref)]
pub fn place_effect_provider_param_index_map<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<Option<usize>> {
    let effect_count = func.effects(db).data(db).len();
    let mut out = vec![None; effect_count];

    let provider_param_indices: Vec<usize> = CallableDef::Func(func)
        .params(db)
        .iter()
        .enumerate()
        .filter_map(|(idx, ty)| match ty.data(db) {
            TyData::TyParam(param) if param.is_effect_provider() => Some(idx),
            _ => None,
        })
        .collect();

    let mut ord = 0usize;
    for effect in func.effect_params(db) {
        let Some(key_path) = effect.key_path(db) else {
            continue;
        };
        if !matches!(
            effect_key_kind(db, key_path, func.scope()),
            EffectKeyKind::Type | EffectKeyKind::Trait
        ) {
            continue;
        }
        let Some(provider_idx) = provider_param_indices.get(ord).copied() else {
            break;
        };
        ord += 1;
        out[effect.index()] = Some(provider_idx);
    }

    out
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
    match resolve_path(db, key_path, scope, assumptions, false) {
        Ok(PathRes::Ty(ty)) if ty.is_star_kind(db) => Some(
            existentialize_omitted_const_args_in_effect_key(db, key_path, ty),
        ),
        Ok(PathRes::TyAlias(_, ty)) if ty.is_star_kind(db) => Some(ty),
        _ => None,
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

    let (param_set, params, offset) = match base_ty {
        TyBase::Adt(adt) => {
            let set = *adt.param_set(db);
            (
                set,
                set.explicit_params(db),
                set.offset_to_explicit_params_position(db),
            )
        }
        TyBase::Func(func) => match *func {
            CallableDef::Func(def) => {
                let set = collect_generic_params(db, def.into());
                (
                    set,
                    set.explicit_params(db),
                    set.offset_to_explicit_params_position(db),
                )
            }
            CallableDef::VariantCtor(_) => return ty,
        },
        _ => return ty,
    };
    if params.is_empty() {
        return ty;
    }

    let provided_explicit_len = key_path.generic_args(db).data(db).len().min(params.len());
    if provided_explicit_len >= params.len() {
        return ty;
    }

    let mut completed_args = args.to_vec();
    let mut changed = false;
    for (explicit_idx, param) in params.iter().enumerate().skip(provided_explicit_len) {
        if !param_set.explicit_const_param_default_is_hole(db, explicit_idx) {
            continue;
        }
        let Some(const_ty_ty) = param.const_ty_ty(db) else {
            continue;
        };

        let arg_idx = offset + explicit_idx;
        if arg_idx >= completed_args.len() {
            continue;
        }
        let hole_ty = if const_ty_ty.has_invalid(db) {
            TyId::u256(db)
        } else {
            const_ty_ty
        };
        let hole = TyId::const_ty(db, ConstTyId::hole_with_ty(db, hole_ty));
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
