use crate::analysis::HirAnalysisDb;
use crate::analysis::name_resolution::{PathRes, resolve_path};
use crate::analysis::ty::trait_resolution::PredicateListId;
use crate::analysis::ty::ty_def::TyData;
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
    // Avoid recursive generic-arg lowering cycles by resolving the key without its generic args.
    let assumptions = PredicateListId::empty_list(db);
    let key_path = key_path.strip_generic_args(db);

    match resolve_path(db, key_path, scope, assumptions, false) {
        Ok(PathRes::Ty(_) | PathRes::TyAlias(_, _)) => EffectKeyKind::Type,
        Ok(PathRes::Trait(_)) => EffectKeyKind::Trait,
        _ => EffectKeyKind::Other,
    }
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
/// For non-type effects (trait effects) the entry is `None`.
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
            EffectKeyKind::Type
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
