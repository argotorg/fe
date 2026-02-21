use crate::analysis::HirAnalysisDb;
use crate::analysis::name_resolution::{
    NameDomain, PathRes, resolve_ident_to_bucket, resolve_path,
};
use crate::analysis::ty::const_ty::ConstTyData;
use crate::analysis::ty::fold::{AssocTySubst, TyFoldable, TyFolder};
use crate::analysis::ty::trait_def::TraitInstId;
use crate::analysis::ty::trait_resolution::PredicateListId;
use crate::analysis::ty::ty_def::{TyData, TyId};
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

    // Prefer classifying the key without lowering generic args to avoid recursive generic-arg
    // lowering cycles. If that fails (e.g., generic traits/types that require args), fall back to
    // resolving the full key path.
    let kind = classify(resolve_path(db, stripped_path, scope, assumptions, false));
    if kind != EffectKeyKind::Other {
        return kind;
    }

    // `resolve_path` rejects generic trait paths when the generic args are stripped, which can
    // create a cycle when this classification runs during generic-param collection. For bare
    // identifiers, consult the name-resolution bucket to classify without lowering args.
    if stripped_path.parent(db).is_none()
        && let Ok(res) = resolve_ident_to_bucket(db, stripped_path, scope).pick(NameDomain::TYPE)
    {
        if res.is_trait() {
            return EffectKeyKind::Trait;
        }
        if res.is_type() {
            return EffectKeyKind::Type;
        }
    }

    classify(resolve_path(db, key_path, scope, assumptions, false))
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
