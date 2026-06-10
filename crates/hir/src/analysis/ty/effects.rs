use crate::analysis::HirAnalysisDb;
use crate::analysis::name_resolution::PathRes;
use crate::analysis::ty::const_ty::{
    ConstCanonEnv, ConstCanonMode, ConstTyData, HoleId, LexSite, ProvenanceId, ProvenanceSite,
    StructuralHoleOrigin, canonicalize_trait_inst_for_mode, canonicalize_ty_for_mode,
};
use crate::analysis::ty::fold::{AssocTySubst, TyFoldable, TyFolder};
use crate::analysis::ty::layout_holes::layout_hole_with_fallback_ty;
use crate::analysis::ty::trait_def::TraitInstId;
use crate::analysis::ty::trait_resolution::PredicateListId;
use crate::analysis::ty::ty_def::{TyBase, TyData, TyId};
use crate::analysis::ty::ty_lower::{collect_generic_params, func_implicit_param_plan};
use crate::core::hir_def::GenericParamOwner;
use crate::hir_def::scope_graph::ScopeId;
use crate::hir_def::{CallableDef, Func, PathId};

pub mod elaborate;
pub mod match_;
pub mod model;

pub use model::{
    BarrierReason, EffectBarrier, EffectFamily, EffectForwarder, EffectPatternKey, EffectQuery,
    EffectQueryMode, EffectRequirementDecl, EffectRequirementKey, EffectWitness,
    ForwardedEffectKey, ForwardedTraitKey, ForwardedTypeKey, KeyedEffectEntry, PatternSlot,
    PatternSlotId, PatternSlotKind, PatternSlots, StoredEffectKey, StoredTraitKey, StoredTypeKey,
    TraitKeySchema, TraitPatternKey, TypeKeySchema, TypePatternKey, WitnessTransport,
    effect_family_for_trait, effect_family_for_type, forwarded_trait_key_is_well_formed,
    forwarded_type_key_is_well_formed, stored_trait_key_is_rigid, stored_type_key_is_rigid,
    stored_value_contains_implicit_layout_params, stored_value_contains_out_of_scope_params,
    stored_value_is_storage_rigid, trait_key_schema_is_well_formed, type_key_schema_is_well_formed,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EffectKeyKind {
    Type,
    Trait,
    Other,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ResolvedEffectKey<'db> {
    Type(TypeKeySchema<'db>),
    Trait(TraitKeySchema<'db>),
    Invalid,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EffectKeyCanonMode {
    Stored,
    Solver,
    Compare,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CanonicalEffectIdentity<'db> {
    pub key_kind: EffectKeyKind,
    pub key_ty: Option<TyId<'db>>,
    pub key_trait: Option<TraitInstId<'db>>,
    pub key_path: PathId<'db>,
    pub is_mut: bool,
}

impl<'db> ResolvedEffectKey<'db> {
    pub(crate) fn into_parts(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> (EffectKeyKind, Option<TyId<'db>>, Option<TraitInstId<'db>>) {
        match self {
            Self::Type(schema) => (EffectKeyKind::Type, Some(schema.carrier), None),
            Self::Trait(schema) => (EffectKeyKind::Trait, None, Some(schema.into_trait_inst(db))),
            Self::Invalid | Self::Other => (EffectKeyKind::Other, None, None),
        }
    }
}

pub(crate) fn canonicalize_effect_type_key<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    assoc_ty_subst: Option<TraitInstId<'db>>,
    mode: EffectKeyCanonMode,
) -> TyId<'db> {
    match mode {
        EffectKeyCanonMode::Stored => canonicalize_ty_for_mode(
            db,
            ty,
            ConstCanonEnv::new(scope, assumptions, assoc_ty_subst),
            ConstCanonMode::Stored,
        ),
        EffectKeyCanonMode::Solver | EffectKeyCanonMode::Compare => canonicalize_ty_for_mode(
            db,
            ty,
            ConstCanonEnv::new(scope, assumptions, assoc_ty_subst),
            ConstCanonMode::Identity,
        ),
    }
}

pub(crate) fn canonicalize_effect_trait_key<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_key: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    assoc_ty_subst: Option<TraitInstId<'db>>,
    mode: EffectKeyCanonMode,
) -> TraitInstId<'db> {
    match mode {
        EffectKeyCanonMode::Stored => canonicalize_trait_inst_for_mode(
            db,
            trait_key,
            ConstCanonEnv::new(scope, assumptions, assoc_ty_subst),
            ConstCanonMode::Stored,
        ),
        EffectKeyCanonMode::Solver | EffectKeyCanonMode::Compare => {
            canonicalize_trait_inst_for_mode(
                db,
                trait_key,
                ConstCanonEnv::new(scope, assumptions, assoc_ty_subst),
                ConstCanonMode::Identity,
            )
        }
    }
}

pub(crate) fn canonical_effect_identity_for_binding<'db>(
    db: &'db dyn HirAnalysisDb,
    binding: &crate::core::semantic::EffectRequirement<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    assoc_ty_subst: Option<TraitInstId<'db>>,
    mode: EffectKeyCanonMode,
) -> CanonicalEffectIdentity<'db> {
    CanonicalEffectIdentity {
        key_kind: binding.key.kind(),
        key_ty: binding.key.key_ty().map(|ty| {
            canonicalize_effect_type_key(db, ty, scope, assumptions, assoc_ty_subst, mode)
        }),
        key_trait: binding.key.key_trait().map(|trait_key| {
            canonicalize_effect_trait_key(db, trait_key, scope, assumptions, assoc_ty_subst, mode)
        }),
        key_path: binding.binding_path,
        is_mut: binding.is_mut,
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

pub(crate) fn resolve_effect_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key_path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> ResolvedEffectKey<'db> {
    match crate::analysis::name_resolution::resolve_path(db, key_path, scope, assumptions, false) {
        Ok(PathRes::Ty(ty)) if ty.is_star_kind(db) => {
            let schema = TypeKeySchema {
                carrier: existentialize_omitted_const_args_in_effect_key(db, key_path, ty),
            };
            if type_key_schema_is_well_formed(db, schema) {
                ResolvedEffectKey::Type(schema)
            } else {
                ResolvedEffectKey::Invalid
            }
        }
        Ok(PathRes::TyAlias(_, ty)) if ty.is_star_kind(db) => {
            let schema = TypeKeySchema { carrier: ty };
            if type_key_schema_is_well_formed(db, schema) {
                ResolvedEffectKey::Type(schema)
            } else {
                ResolvedEffectKey::Invalid
            }
        }
        Ok(PathRes::Trait(trait_inst)) => {
            let schema = TraitKeySchema::from_canonical_trait_binding(db, trait_inst);
            if trait_key_schema_is_well_formed(db, &schema) {
                ResolvedEffectKey::Trait(schema)
            } else {
                ResolvedEffectKey::Invalid
            }
        }
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
                ProvenanceId::root(db, ProvenanceSite::Lex(LexSite::RootPath(key_path))),
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
    preserve_implicit_const_params: bool,
    assoc_ty_subst: Option<TraitInstId<'db>>,
) -> TraitInstId<'db> {
    let mut trait_key = instantiate_effect_key_value(
        db,
        trait_key,
        callee_generic_args,
        preserve_implicit_const_params,
    );

    if let Some(inst) = assoc_ty_subst {
        let mut subst = AssocTySubst::new(inst);
        trait_key = trait_key.fold_with(db, &mut subst);
    }

    trait_key
}

pub(crate) fn instantiate_type_effect_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key_ty: TyId<'db>,
    callee_generic_args: &[TyId<'db>],
) -> TyId<'db> {
    instantiate_effect_key_value(db, key_ty, callee_generic_args, true)
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
    canonicalize_ty_for_mode(
        db,
        ty,
        ConstCanonEnv::new(scope, assumptions, assoc_ty_subst),
        ConstCanonMode::Identity,
    )
}

pub(crate) fn normalize_effect_identity_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_key: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    assoc_ty_subst: Option<TraitInstId<'db>>,
) -> TraitInstId<'db> {
    canonicalize_trait_inst_for_mode(
        db,
        trait_key,
        ConstCanonEnv::new(scope, assumptions, assoc_ty_subst),
        ConstCanonMode::Identity,
    )
}
