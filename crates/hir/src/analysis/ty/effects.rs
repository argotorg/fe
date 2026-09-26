use crate::analysis::HirAnalysisDb;
use crate::analysis::name_resolution::{PathRes, resolve_path_with_minter};
use crate::analysis::ty::const_ty::{
    ConstCanonEnv, ConstCanonMode, HoleAnchor, HoleId, LayoutHoleArgSite, LayoutIntroSite,
    LoweringContext, StructuralHoleOrigin, canonicalize_trait_inst_for_mode,
    canonicalize_ty_for_mode,
};
use crate::analysis::ty::fold::TyFoldable;
use crate::analysis::ty::layout_holes::layout_hole_with_fallback_ty;
use crate::analysis::ty::normalize::normalize_from_assumptions;
use crate::analysis::ty::subst::substitute_complete;
use crate::analysis::ty::trait_def::TraitInstId;
use crate::analysis::ty::trait_resolution::PredicateListId;
use crate::analysis::ty::ty_check::Callable;
use crate::analysis::ty::ty_def::{TyBase, TyData, TyId};
use crate::analysis::ty::ty_lower::{
    CompleteSubst, LoweredSlot, ParamDomainId, ParamSchemaId, collect_generic_params,
    func_implicit_param_plan, lower_hir_ty_with_minter,
};
use crate::core::hir_def::GenericParamOwner;
use crate::hir_def::scope_graph::ScopeId;
use crate::hir_def::{CallableDef, Func, Partial, PathId, TypeId as HirTypeId, TypeKind};

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
    pub key_syntax: HirTypeId<'db>,
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
    assoc_evidence: Option<TraitInstId<'db>>,
    mode: EffectKeyCanonMode,
) -> TyId<'db> {
    match mode {
        EffectKeyCanonMode::Stored => canonicalize_ty_for_mode(
            db,
            ty,
            ConstCanonEnv::new(scope, assumptions, assoc_evidence),
            ConstCanonMode::Stored,
        ),
        EffectKeyCanonMode::Solver | EffectKeyCanonMode::Compare => canonicalize_ty_for_mode(
            db,
            ty,
            ConstCanonEnv::new(scope, assumptions, assoc_evidence),
            ConstCanonMode::Identity,
        ),
    }
}

pub(crate) fn canonicalize_effect_trait_key<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_key: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    assoc_evidence: Option<TraitInstId<'db>>,
    mode: EffectKeyCanonMode,
) -> TraitInstId<'db> {
    match mode {
        EffectKeyCanonMode::Stored => canonicalize_trait_inst_for_mode(
            db,
            trait_key,
            ConstCanonEnv::new(scope, assumptions, assoc_evidence),
            ConstCanonMode::Stored,
        ),
        EffectKeyCanonMode::Solver | EffectKeyCanonMode::Compare => {
            canonicalize_trait_inst_for_mode(
                db,
                trait_key,
                ConstCanonEnv::new(scope, assumptions, assoc_evidence),
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
    assoc_evidence: Option<TraitInstId<'db>>,
    mode: EffectKeyCanonMode,
) -> CanonicalEffectIdentity<'db> {
    CanonicalEffectIdentity {
        key_kind: binding.key.kind(),
        key_ty: binding.key.key_ty().map(|ty| {
            canonicalize_effect_type_key(db, ty, scope, assumptions, assoc_evidence, mode)
        }),
        key_trait: binding.key.key_trait().map(|trait_key| {
            canonicalize_effect_trait_key(db, trait_key, scope, assumptions, assoc_evidence, mode)
        }),
        key_syntax: binding.binding_ty,
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
    key_ty: HirTypeId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> ResolvedEffectKey<'db> {
    let minter = LoweringContext::new(HoleAnchor::TemplateTy {
        ty: key_ty,
        scope,
        assumptions,
    });
    match lower_effect_key_schema(db, key_ty, scope, assumptions, &minter) {
        ResolvedEffectKey::Type(schema) if !type_key_schema_is_well_formed(db, schema) => {
            ResolvedEffectKey::Invalid
        }
        ResolvedEffectKey::Trait(schema) if !trait_key_schema_is_well_formed(db, &schema) => {
            ResolvedEffectKey::Invalid
        }
        key => key,
    }
}

pub(crate) fn resolve_effect_path<'db>(
    db: &'db dyn HirAnalysisDb,
    key_path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> ResolvedEffectKey<'db> {
    let key_ty = HirTypeId::new(db, TypeKind::Path(Partial::Present(key_path)));
    resolve_effect_key(db, key_ty, scope, assumptions)
}

/// Lower a key's declaration shape without requiring it to be a valid effect.
/// A deferred minter lets slot planning retain layout holes before const bodies
/// and bounds are checked using the completed callable parameter list.
pub(crate) fn lower_effect_key_schema<'db>(
    db: &'db dyn HirAnalysisDb,
    key_ty: HirTypeId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    minter: &LoweringContext<'db>,
) -> ResolvedEffectKey<'db> {
    let TypeKind::Path(path) = key_ty.data(db) else {
        let carrier = normalize_from_assumptions(
            db,
            lower_hir_ty_with_minter(db, key_ty, scope, assumptions, minter),
            scope,
            assumptions,
        );
        return if carrier.is_star_kind(db) {
            ResolvedEffectKey::Type(TypeKeySchema { carrier })
        } else {
            ResolvedEffectKey::Invalid
        };
    };
    let Some(key_path) = path.to_opt() else {
        return ResolvedEffectKey::Other;
    };
    match resolve_path_with_minter(db, key_path, scope, assumptions, false, minter) {
        Ok(PathRes::Ty(ty)) if ty.is_star_kind(db) => {
            let ty = normalize_from_assumptions(db, ty, scope, assumptions);
            let schema = TypeKeySchema {
                carrier: existentialize_omitted_const_args_in_effect_key(
                    db,
                    key_path,
                    scope,
                    assumptions,
                    ty,
                ),
            };
            ResolvedEffectKey::Type(schema)
        }
        Ok(PathRes::TyAlias(_, ty)) if ty.is_star_kind(db) => {
            let schema = TypeKeySchema {
                carrier: normalize_from_assumptions(db, ty, scope, assumptions),
            };
            ResolvedEffectKey::Type(schema)
        }
        Ok(PathRes::Trait(trait_inst)) => {
            let schema = TraitKeySchema::from_canonical_trait_binding(db, trait_inst);
            ResolvedEffectKey::Trait(schema)
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
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
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

    let minter = LoweringContext::new(HoleAnchor::TemplatePath {
        path: key_path,
        scope,
        assumptions,
    });
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
                LayoutIntroSite::lowering(LayoutHoleArgSite::Path(key_path), arg_idx),
                minter.holes().mint(db),
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
    callable: &Callable<'db>,
) -> TraitInstId<'db> {
    instantiate_effect_key_value(db, trait_key, callable)
}

pub(crate) fn instantiate_type_effect_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key_ty: TyId<'db>,
    callable: &Callable<'db>,
) -> TyId<'db> {
    instantiate_effect_key_value(db, key_ty, callable)
}

fn instantiate_effect_key_value<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    callable: &Callable<'db>,
) -> T
where
    T: TyFoldable<'db>,
{
    let schema = ParamSchemaId::callable(db, callable.callable_def());
    let mut args = callable.generic_args().to_vec();
    for (index, arg) in args.iter_mut().enumerate() {
        let formal = schema
            .declared_formal_at(db, LoweredSlot(index))
            .expect("effect key slot has a formal");
        if formal
            .as_generic_param(db)
            .is_some_and(|param| param.is_effect() || param.is_implicit())
        {
            *arg = formal;
        }
    }
    let domain = ParamDomainId::full(db, schema);
    let subst = CompleteSubst::new(domain, db, args)
        .expect("effect key callable arguments must cover their schema");
    substitute_complete(db, value, &subst)
        .expect("effect key must use its callable declaration schema")
}

pub(crate) fn normalize_effect_identity_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    assoc_evidence: Option<TraitInstId<'db>>,
) -> TyId<'db> {
    canonicalize_ty_for_mode(
        db,
        ty,
        ConstCanonEnv::new(scope, assumptions, assoc_evidence),
        ConstCanonMode::Identity,
    )
}

pub(crate) fn normalize_effect_identity_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_key: TraitInstId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    assoc_evidence: Option<TraitInstId<'db>>,
) -> TraitInstId<'db> {
    canonicalize_trait_inst_for_mode(
        db,
        trait_key,
        ConstCanonEnv::new(scope, assumptions, assoc_evidence),
        ConstCanonMode::Identity,
    )
}
