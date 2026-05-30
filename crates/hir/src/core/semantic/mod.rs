//! Semantic traversal surface for HIR items.
//!
//! This module hosts the externally‑facing, semantic methods that callers
//! should use when walking the HIR. Keep raw, syntactic accessors and
//! #[salsa::tracked] implementations in `item.rs`; provide ergonomic,
//! context‑aware helpers here that compose the internal lowering and
//! resolution layers.
//!
//! Design notes
//! - Prefer returning semantic IDs (`TyId`, `TraitInstId`, etc.) or diagnostic
//!   collections from here; do not expose raw HIR nodes.
//! - Avoid env/engine types (`PredicateListId`, solver tables, etc.) in the
//!   public API. Keep environment plumbing internal to semantic helpers or
//!   the analysis layer; callers should ask items/views for semantic answers
//!   instead of pushing assumption lists around.
//! - Keep methods small and capability‑oriented (e.g., generic params,
//!   where‑clauses, signature types). Push per‑node, context‑rich logic into
//!   views when a single method signature becomes unwieldy.
//! - Let the compiler guide additions: ablate public syntactic accessors in
//!   `item.rs` and replace call sites by adding only the minimal semantic
//!   method(s) here.

pub mod index;
pub mod reference;
pub mod symbol;
use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::corelib::{resolve_core_trait, resolve_lib_func_path};
use crate::analysis::ty::diagnostics::{ImplDiag, TyLowerDiag};
use crate::analysis::ty::fold::TyFoldable;
use crate::analysis::ty::normalize::normalize_ty;
use crate::analysis::ty::ty_def::Kind;
use crate::analysis::ty::ty_error::collect_hir_ty_diags;
use crate::hir_def::params::KindBound as HirKindBound;
use crate::hir_def::scope_graph::ScopeId;
use crate::{HirDb, SpannedHirDb};
pub use reference::{
    FieldAccessView, HasReferences, MethodCallView, PathView, ReferenceView, Target, UsePathView,
};
use rustc_hash::{FxHashMap, FxHashSet};
pub use symbol::{
    IndexedReference, ReferenceIndex, SignatureWithSpan, SourceLocation, SymbolKind, SymbolView,
    item_kind_to_url_suffix, qualify_path_with_ingot_name, scope_to_doc_path,
};

pub fn lower_hir_kind_local(k: &HirKindBound) -> Kind {
    use crate::hir_def::Partial;
    match k {
        HirKindBound::Mono => Kind::Star,
        HirKindBound::Abs(lhs, rhs) => {
            let lhs_k = match lhs {
                Partial::Present(inner) => lower_hir_kind_local(inner),
                Partial::Absent => Kind::Any,
            };
            let rhs_k = match rhs {
                Partial::Present(inner) => lower_hir_kind_local(inner),
                Partial::Absent => Kind::Any,
            };
            Kind::Abs(Box::new((lhs_k, rhs_k)))
        }
    }
}
use crate::analysis::ty::binder::Binder;
use crate::hir_def::*;
// When adding real methods, prefer calling internal lowering/normalization here
// rather than exposing raw syntax.
use crate::analysis::ty::adt_def::{AdtCycleMember, AdtDef, AdtField, AdtRef};
use crate::analysis::ty::const_ty::{
    CallableInputLayoutHoleOrigin, ConstTyData, ConstTyId, EvaluatedConstTy,
};
use crate::analysis::ty::effects::{
    EffectKeyKind, place_effect_provider_param_index_map, resolve_effect_key,
};
use crate::analysis::ty::layout_holes::{
    LayoutPlaceholderPolicy, callable_input_layout_bindings_by_origin,
    collect_layout_hole_tys_in_order, collect_unique_layout_placeholders_in_order_with_policy,
    layout_hole_fallback_ty, substitute_layout_holes_by_identity,
    substitute_layout_holes_by_identity_in, substitute_layout_placeholders_by_identity,
};
use crate::analysis::ty::trait_def::{
    ImplementorId, ImplementorOrigin, TraitInstId, does_impl_trait_conflict, ingot_trait_env,
};
use crate::analysis::ty::trait_lower::{TraitRefLowerError, lower_trait_ref};
use crate::analysis::ty::trait_resolution::constraint::{
    collect_adt_constraints, collect_constraints, collect_func_decl_constraints,
    collect_func_def_constraints,
};
use crate::analysis::ty::ty_def::{TyBase, TyData, TyParam, strip_derived_adt_layout_args};
use crate::analysis::ty::ty_lower::{GenericParamTypeSet, collect_generic_params};
use crate::analysis::ty::visitor::{TyVisitable, TyVisitor, walk_ty};
use crate::analysis::ty::{
    diagnostics::{TraitConstraintDiag, TyDiagCollection},
    provider::{
        ProviderSemantics, RootProviderRegistration, provider_semantics, registered_root_providers,
    },
    trait_resolution::{
        GoalSatisfiability, PredicateListId, TraitSolveCx, WellFormedness, check_ty_wf,
        is_goal_satisfiable,
    },
    ty_check::EffectParamSite,
    ty_contains_const_hole,
    ty_def::{InvalidCause, PrimTy, TyId, instantiate_adt_field_ty},
    ty_error::collect_ty_lower_errors,
    ty_lower::{
        TyAlias, lower_callable_input_param_ty, lower_hir_ty, lower_opt_hir_ty, lower_type_alias,
        lower_type_alias_from_hir, resolve_callable_input_effect_key,
    },
};
use crate::core::adt_lower::{lower_adt, lower_contract_fields};
use common::indexmap::IndexMap;
use indexmap::IndexSet;
use num_bigint::BigUint;
use num_traits::ToPrimitive;
use salsa::Update;
// Re-export from crate root for backwards compatibility
pub use crate::diagnosable as diagnostics;

/// Core-exposed entry point for alias lowering. Reads the HIR type_ref (core-visible)
/// and delegates to the analysis helper to keep visibility tight without shims.
pub(crate) fn lower_type_alias_body<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: TypeAlias<'db>,
) -> TyAlias<'db> {
    let hir_ty_opt = alias.type_ref(db).to_opt();
    lower_type_alias_from_hir(db, alias, hir_ty_opt)
}

/// The trait's implicit `Self: Trait` predicate.
pub(crate) fn trait_self_predicate<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db>,
) -> TraitInstId<'db> {
    TraitInstId::new(db, trait_, trait_.params(db).to_vec(), IndexMap::new())
}

/// Appends `pred` to `preds` unless it is already present.
fn push_predicate<'db>(
    db: &'db dyn HirAnalysisDb,
    preds: PredicateListId<'db>,
    pred: TraitInstId<'db>,
) -> PredicateListId<'db> {
    if preds.list(db).contains(&pred) {
        return preds;
    }
    let mut merged = preds.list(db).to_vec();
    merged.push(pred);
    PredicateListId::new(db, merged)
}

/// Consolidated assumptions for any item kind.
pub fn constraints_for<'db>(
    db: &'db dyn HirAnalysisDb,
    item: ItemKind<'db>,
) -> PredicateListId<'db> {
    match item {
        ItemKind::Struct(s) => collect_adt_constraints(db, s.as_adt(db)).instantiate_identity(),
        ItemKind::Enum(e) => collect_adt_constraints(db, e.as_adt(db)).instantiate_identity(),
        // Contracts have no generic parameters, so no constraints
        ItemKind::Contract(_) => PredicateListId::empty_list(db),
        ItemKind::Func(f) => {
            collect_func_def_constraints(db, f.into(), true).instantiate_identity()
        }
        ItemKind::Impl(i) => collect_constraints(db, i.into()).instantiate_identity(),
        ItemKind::Trait(t) => push_predicate(
            db,
            collect_constraints(db, t.into()).instantiate_identity(),
            trait_self_predicate(db, t),
        ),
        ItemKind::ImplTrait(i) => collect_constraints(db, i.into()).instantiate_identity(),
        _ => PredicateListId::empty_list(db),
    }
}

/// The full assumption set in scope for proving obligations *inside* `item`:
/// the item's declared constraints (including its parents'), the implicit
/// `Self: Trait` predicate for traits and trait items, and all implied bounds
/// (super-trait closure and associated-type bounds).
///
/// Use this for well-formedness and satisfiability *checks*. Lowering an
/// item's own header (super-trait refs, where-clause subjects, generic-param
/// bounds) must use [`header_constraints_for`] instead: assuming `Self: Trait`
/// while the trait's own interface is still being lowered can recurse through
/// the in-progress definition.
pub fn param_env<'db>(db: &'db dyn HirAnalysisDb, item: ItemKind<'db>) -> PredicateListId<'db> {
    let preds = constraints_for(db, item);
    let preds = match item {
        // The trait's own self-predicate is added by `constraints_for`.
        ItemKind::Trait(_) => preds,
        _ => match item.scope().parent_item(db) {
            Some(ItemKind::Trait(trait_)) => {
                push_predicate(db, preds, trait_self_predicate(db, trait_))
            }
            _ => preds,
        },
    };
    preds.extend_all_bounds(db)
}

/// Unelaborated assumptions for checking `func`'s body: the declared
/// constraints (including the parent item's) plus the implicit `Self: Trait`
/// predicate for trait methods. Effect-derived bounds are layered on
/// separately by the body checker.
pub(crate) fn func_body_assumptions<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> PredicateListId<'db> {
    let preds = collect_func_decl_constraints(db, func.into(), true).instantiate_identity();
    match func.scope().parent_item(db) {
        Some(ItemKind::Trait(trait_)) => {
            push_predicate(db, preds, trait_self_predicate(db, trait_))
        }
        _ => preds,
    }
}

/// Trait header validation must not assume `Self: Trait` yet.
///
/// The synthetic self-predicate from [`constraints_for`] is only valid once the trait
/// interface has been established. Using it while lowering the trait's own super-traits or
/// where-predicate headers can make projection resolution recurse through the in-progress trait
/// definition.
pub(crate) fn header_constraints_for<'db>(
    db: &'db dyn HirAnalysisDb,
    item: ItemKind<'db>,
) -> PredicateListId<'db> {
    match item {
        ItemKind::Trait(trait_) => collect_constraints(db, trait_.into()).instantiate_identity(),
        _ => constraints_for(db, item),
    }
}

type CallableInputLayoutArgs<'db> =
    FxHashMap<CallableInputLayoutHoleOrigin, FxHashMap<TyId<'db>, TyId<'db>>>;

fn callable_input_layout_args<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> CallableInputLayoutArgs<'db> {
    callable_input_layout_bindings_by_origin(db, CallableDef::Func(func))
        .into_iter()
        .map(|(origin, bindings)| (origin, bindings.into_iter().collect()))
        .collect()
}

fn canonicalize_effect_binding_trait_inst<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_inst: TraitInstId<'db>,
) -> TraitInstId<'db> {
    let mut assoc = trait_inst
        .assoc_ty_bindings(db)
        .into_iter()
        .collect::<Vec<_>>();
    assoc.sort_by_key(|(lhs, _)| *lhs);
    TraitInstId::new(
        db,
        trait_inst.def(db),
        trait_inst.args(db).to_vec(),
        assoc.into_iter().collect::<IndexMap<_, _>>(),
    )
}

fn contract_effect_hidden_param_scope<'db>(
    _db: &'db dyn HirDb,
    site: EffectParamSite<'db>,
) -> ScopeId<'db> {
    match site {
        EffectParamSite::Contract(contract)
        | EffectParamSite::ContractInit { contract }
        | EffectParamSite::ContractRecvArm { contract, .. } => contract.scope(),
        EffectParamSite::Func(_) => {
            unreachable!("contract effect hidden params must use a contract-scoped site")
        }
    }
}

fn contract_effect_layout_param_name<'db>(
    db: &'db dyn HirDb,
    site: EffectParamSite<'db>,
    binding_idx: u32,
    layout_idx: usize,
) -> IdentId<'db> {
    let site_name = |ident: Option<IdentId<'db>>, fallback: &str| {
        ident
            .map(|ident| ident.data(db).to_string())
            .unwrap_or_else(|| fallback.to_string())
    };
    let prefix = match site {
        EffectParamSite::Contract(contract) => {
            format!(
                "__contract{}_efflayout",
                site_name(contract.name(db).to_opt(), "contract")
            )
        }
        EffectParamSite::ContractInit { contract } => {
            format!(
                "__init{}_efflayout",
                site_name(contract.name(db).to_opt(), "contract")
            )
        }
        EffectParamSite::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => format!(
            "__recv{}_{}_{}_efflayout",
            site_name(contract.name(db).to_opt(), "contract"),
            recv_idx,
            arm_idx
        ),
        EffectParamSite::Func(_) => {
            unreachable!("contract effect hidden params must use a contract-scoped site")
        }
    };
    IdentId::new(db, format!("{prefix}{binding_idx}_{layout_idx}"))
}

fn contract_effect_hidden_param_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
    binding_idx: u32,
    layout_idx: usize,
    placeholder: TyId<'db>,
) -> TyId<'db> {
    let TyData::ConstTy(const_ty) = placeholder.data(db) else {
        return placeholder;
    };
    let fallback_ty = match const_ty.data(db) {
        ConstTyData::Hole(hole_ty, _) => layout_hole_fallback_ty(db, *hole_ty),
        ConstTyData::TyParam(_, fallback_ty) => *fallback_ty,
        _ => return placeholder,
    };
    let param = TyParam::implicit_param(
        contract_effect_layout_param_name(db, site, binding_idx, layout_idx),
        layout_idx,
        fallback_ty.kind(db).clone(),
        contract_effect_hidden_param_scope(db, site),
    );
    TyId::new(
        db,
        TyData::ConstTy(ConstTyId::new(db, ConstTyData::TyParam(param, fallback_ty))),
    )
}

fn canonicalize_contract_effect_key_value<'db, T>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
    binding_idx: u32,
    value: T,
) -> T
where
    T: TyFoldable<'db> + TyVisitable<'db> + Copy,
{
    let placeholders = collect_unique_layout_placeholders_in_order_with_policy(
        db,
        value,
        LayoutPlaceholderPolicy::HolesAndImplicitParams,
    );
    if placeholders.is_empty() {
        return value;
    }

    let layout_args = placeholders
        .into_iter()
        .enumerate()
        .map(|(layout_idx, placeholder)| {
            (
                placeholder,
                contract_effect_hidden_param_ty(db, site, binding_idx, layout_idx, placeholder),
            )
        })
        .collect::<FxHashMap<_, _>>();
    substitute_layout_placeholders_by_identity(
        db,
        value,
        &layout_args,
        LayoutPlaceholderPolicy::HolesAndImplicitParams,
    )
}

fn canonicalize_contract_effect_key<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
    binding_idx: u32,
    key_ty: Option<TyId<'db>>,
    key_trait: Option<TraitInstId<'db>>,
) -> (Option<TyId<'db>>, Option<TraitInstId<'db>>) {
    let key_ty = key_ty.map(|ty| canonicalize_contract_effect_key_value(db, site, binding_idx, ty));
    let key_trait = key_trait
        .map(|trait_inst| canonicalize_contract_effect_key_value(db, site, binding_idx, trait_inst))
        .map(|trait_inst| canonicalize_effect_binding_trait_inst(db, trait_inst));
    (key_ty, key_trait)
}

fn func_effect_requirements_canonical<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<EffectRequirement<'db>> {
    let assumptions = collect_func_decl_constraints(db, func.into(), true).instantiate_identity();
    let layout_args = callable_input_layout_args(db, func);
    func.effects(db)
        .data(db)
        .iter()
        .enumerate()
        .filter_map(|(idx, effect)| {
            let key_path = effect.key_path.to_opt()?;
            let binding_name = effect
                .name
                .or_else(|| key_path.ident(db).to_opt())
                .unwrap_or_else(|| IdentId::new(db, "_effect".to_string()));
            let (key_kind, key_ty, key_trait) =
                resolve_callable_input_effect_key(db, func, idx, key_path, assumptions)
                    .into_parts(db);
            let effect_layout_args = layout_args.get(&CallableInputLayoutHoleOrigin::Effect(idx));
            let key_ty = key_ty.map(|ty| {
                if !ty_contains_const_hole(db, ty) {
                    return ty;
                }
                let Some(effect_layout_args) = effect_layout_args else {
                    return ty;
                };
                let ty = substitute_layout_holes_by_identity(db, ty, effect_layout_args);
                debug_assert!(
                    !ty_contains_const_hole(db, ty) || ty.has_invalid(db),
                    "unelaborated layout hole remained in callable effect key type"
                );
                ty
            });
            let key_trait = key_trait.map(|trait_inst| {
                if collect_layout_hole_tys_in_order(db, trait_inst).is_empty() {
                    return canonicalize_effect_binding_trait_inst(db, trait_inst);
                }
                let Some(effect_layout_args) = effect_layout_args else {
                    return canonicalize_effect_binding_trait_inst(db, trait_inst);
                };
                canonicalize_effect_binding_trait_inst(
                    db,
                    substitute_layout_holes_by_identity_in(db, trait_inst, effect_layout_args),
                )
            });
            Some(EffectRequirement {
                binding_name,
                key: EffectRequirementKey::from_parts(key_kind, key_ty, key_trait),
                is_mut: effect.is_mut,
                binding_site: EffectParamSite::Func(func),
                binding_idx: idx as u32,
                binding_path: key_path,
            })
        })
        .collect()
}

fn func_provider_bindings_canonical<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<ProviderBinding<'db>> {
    let requirements = func_effect_requirements_canonical(db, func);
    let assumptions = collect_func_decl_constraints(db, func.into(), true).instantiate_identity();
    let scope = func.scope();
    let provider_map = place_effect_provider_param_index_map(db, func);
    let provider_params = CallableDef::Func(func).params(db);
    let mut providers = requirements
        .iter()
        .filter(|requirement| {
            matches!(
                requirement.key.kind(),
                EffectKeyKind::Type | EffectKeyKind::Trait
            )
        })
        .filter_map(|requirement| {
            let provider_param_idx = provider_map
                .get(requirement.binding_idx as usize)
                .copied()
                .flatten()?;
            let provider_ty = *provider_params.get(provider_param_idx)?;
            Some(ProviderBinding {
                provider_idx: requirement.binding_idx,
                provider_ty,
                is_mut: requirement.is_mut,
                source: ProviderSource::UsesParam {
                    site: EffectParamSite::Func(func),
                    requirement_idx: requirement.binding_idx,
                },
                semantics: provider_semantics(db, scope, assumptions, provider_ty),
            })
        })
        .collect::<Vec<_>>();

    let base_provider_idx = providers.len();
    providers.extend(
        registered_root_providers(db, EffectParamSite::Func(func))
            .iter()
            .cloned()
            .enumerate()
            .map(|(idx, registration)| {
                let provider_ty = registration.provider_ty;
                ProviderBinding {
                    provider_idx: (base_provider_idx + idx) as u32,
                    provider_ty,
                    is_mut: true,
                    source: ProviderSource::RootProvider {
                        site: EffectParamSite::Func(func),
                        registration,
                    },
                    semantics: provider_semantics(db, scope, assumptions, provider_ty),
                }
            }),
    );
    providers
}

fn func_effect_resolutions_canonical<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<ResolvedEffectBinding> {
    let providers = provider_bindings_for_site(db, EffectParamSite::Func(func));
    let explicit_provider_idx = providers
        .iter()
        .filter_map(|provider| match provider.source {
            ProviderSource::UsesParam {
                requirement_idx, ..
            } => Some((
                requirement_idx,
                (provider.provider_idx, provider.semantics.kind),
            )),
            ProviderSource::RootProvider { .. } | ProviderSource::ContractField { .. } => None,
        })
        .collect::<FxHashMap<_, _>>();
    let root_provider_idx = providers.iter().find_map(|provider| match provider.source {
        ProviderSource::RootProvider { .. } => Some(provider.provider_idx),
        ProviderSource::UsesParam { .. } | ProviderSource::ContractField { .. } => None,
    });

    func_effect_requirements_canonical(db, func)
        .into_iter()
        .filter(|requirement| {
            matches!(
                requirement.key.kind(),
                EffectKeyKind::Type | EffectKeyKind::Trait
            )
        })
        .filter_map(|requirement| {
            explicit_provider_idx
                .get(&requirement.binding_idx)
                .map(|(provider_idx, _)| *provider_idx)
                .or(root_provider_idx)
                .map(|provider_idx| ResolvedEffectBinding {
                    requirement_idx: requirement.binding_idx,
                    provider_idx,
                })
        })
        .collect()
}

fn contract_effect_requirements_canonical<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> Vec<EffectRequirement<'db>> {
    let assumptions = PredicateListId::empty_list(db);
    let site = EffectParamSite::Contract(contract);
    contract
        .effects(db)
        .data(db)
        .iter()
        .enumerate()
        .filter_map(|(idx, effect)| {
            let key_path = effect.key_path.to_opt()?;
            let binding_name = effect
                .name
                .or_else(|| key_path.ident(db).to_opt())
                .unwrap_or_else(|| IdentId::new(db, "_effect".to_string()));
            let (key_kind, key_ty, key_trait) =
                resolve_effect_key(db, key_path, contract.scope(), assumptions).into_parts(db);
            let (key_ty, key_trait) =
                canonicalize_contract_effect_key(db, site, idx as u32, key_ty, key_trait);
            Some(EffectRequirement {
                binding_name,
                key: EffectRequirementKey::from_parts(key_kind, key_ty, key_trait),
                is_mut: effect.is_mut,
                binding_site: site,
                binding_idx: idx as u32,
                binding_path: key_path,
            })
        })
        .collect()
}

fn lower_self_fallback_param_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    hir_ty: TypeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    match hir_ty.data(db) {
        TypeKind::Path(path) if path.to_opt().is_some_and(|path| path.is_self_ty(db)) => func
            .expected_self_ty(db)
            .unwrap_or_else(|| lower_hir_ty(db, hir_ty, func.scope(), assumptions)),
        TypeKind::Mode(mode, inner) => {
            let Some(inner) = inner.to_opt() else {
                return TyId::invalid(db, InvalidCause::ParseError);
            };

            let inner = lower_self_fallback_param_ty(db, func, inner, assumptions);
            match mode {
                TypeMode::Mut => TyId::borrow_mut_of(db, inner),
                TypeMode::Ref => TyId::borrow_ref_of(db, inner),
                TypeMode::Own => inner,
            }
        }
        _ => lower_hir_ty(db, hir_ty, func.scope(), assumptions),
    }
}

fn elaborate_func_param_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    assumptions: PredicateListId<'db>,
    layout_args: &CallableInputLayoutArgs<'db>,
    param_idx: usize,
    param: &FuncParam<'db>,
    apply_view: bool,
) -> TyId<'db> {
    let mut ty = match (
        param.ty.to_opt(),
        param.is_self_param(db),
        param.self_ty_fallback,
    ) {
        (Some(hir_ty), true, true) => lower_self_fallback_param_ty(db, func, hir_ty, assumptions),
        (Some(hir_ty), true, false) => lower_callable_input_param_ty(
            db,
            func,
            CallableInputLayoutHoleOrigin::Receiver,
            hir_ty,
            assumptions,
        ),
        (Some(hir_ty), false, _) => lower_callable_input_param_ty(
            db,
            func,
            CallableInputLayoutHoleOrigin::ValueParam(param_idx),
            hir_ty,
            assumptions,
        ),
        (None, _, _) => TyId::invalid(db, InvalidCause::ParseError),
    };
    let had_layout_hole = ty_contains_const_hole(db, ty);

    if param.is_self_param(db)
        && had_layout_hole
        && let Some(receiver_layout_args) =
            layout_args.get(&CallableInputLayoutHoleOrigin::Receiver)
    {
        ty = substitute_layout_holes_by_identity(db, ty, receiver_layout_args);
    } else if had_layout_hole
        && let Some(param_layout_args) =
            layout_args.get(&CallableInputLayoutHoleOrigin::ValueParam(param_idx))
    {
        ty = substitute_layout_holes_by_identity(db, ty, param_layout_args);
    }

    let ty = if apply_view
        && param.mode == crate::hir_def::params::FuncParamMode::View
        && ty.as_capability(db).is_none()
    {
        TyId::view_of(db, ty)
    } else {
        ty
    };

    if had_layout_hole {
        let func_name = func
            .name(db)
            .to_opt()
            .map(|name| name.data(db).to_string())
            .unwrap_or_else(|| "<anonymous>".to_string());
        debug_assert!(
            !ty_contains_const_hole(db, ty) || ty.has_invalid(db),
            "unelaborated layout hole remained in callable parameter type for {func_name} param {param_idx}: {}",
            ty.pretty_print(db),
        );
    }
    ty
}
// Top‑level module items ----------------------------------------------------

impl<'db> TopLevelMod<'db> {
    // Note: callers currently use `scope_graph`-based traversal for modules.
    // If we find repetition in analysis or diagnostics, consider adding
    // semantic child-iteration helpers here instead of reaching into HIR.
}

impl<'db> Mod<'db> {
    // Note: semantic child iteration and module-scoped diagnostics can be
    // added here if direct `scope_graph` traversal in analysis becomes noisy.
    pub fn arithmetic_mode(self, db: &'db dyn HirDb) -> Option<ArithmeticMode> {
        self.attributes(db).arithmetic_mode(db)
    }
}

// Function items ------------------------------------------------------------

impl<'db> Func<'db> {
    pub fn arithmetic_mode(self, db: &'db dyn HirDb) -> ArithmeticMode {
        if let Some(mode) = self.attributes(db).arithmetic_mode(db) {
            return mode;
        }

        let mut scope = self.scope().parent_module(db);
        while let Some(module_scope) = scope {
            if let ItemKind::Mod(mod_) = module_scope.item()
                && let Some(mode) = mod_.attributes(db).arithmetic_mode(db)
            {
                return mode;
            }
            scope = module_scope.parent_module(db);
        }

        if let Some(mode) = self.top_mod(db).attributes(db).arithmetic_mode(db) {
            return mode;
        }

        if let Some(mode) = self.top_mod(db).ingot(db).arithmetic_mode(db) {
            return match mode {
                common::config::ArithmeticMode::Checked => ArithmeticMode::Checked,
                common::config::ArithmeticMode::Unchecked => ArithmeticMode::Unchecked,
            };
        }

        ArithmeticMode::Checked
    }

    /// Semantic predicate list (assumptions) for this function.
    pub(crate) fn assumptions(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        constraints_for(db, self.into())
    }

    /// Returns true if this function declares an explicit return type.
    pub fn has_explicit_return_ty(self, db: &'db dyn HirDb) -> bool {
        self.ret_type_ref(db).is_some()
    }

    /// Explicit return type if annotated in source; `None` when the
    /// function has no explicit return type.
    fn explicit_return_ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let assumptions = self.assumptions(db);
        let hir = self.ret_type_ref(db)?;
        Some(lower_hir_ty(db, hir, self.scope(), assumptions))
    }

    /// Semantic return type. When absent in source, this is `unit`.
    pub fn return_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.explicit_return_ty(db)
            .unwrap_or_else(|| TyId::unit(db))
    }

    /// Semantic argument types bound to identity parameters.
    pub fn arg_tys(self, db: &'db dyn HirAnalysisDb) -> Vec<Binder<TyId<'db>>> {
        let assumptions = self.assumptions(db);
        let layout_args = callable_input_layout_args(db, self);
        match self.params_list(db).to_opt() {
            Some(params) => params
                .data(db)
                .iter()
                .enumerate()
                .map(|(param_idx, param)| {
                    let ty = elaborate_func_param_ty(
                        db,
                        self,
                        assumptions,
                        &layout_args,
                        param_idx,
                        param,
                        true,
                    );
                    debug_assert!(
                        !ty_contains_const_hole(db, ty) || ty.has_invalid(db),
                        "unelaborated layout hole remained in Func::arg_tys"
                    );
                    Binder::bind(ty)
                })
                .collect(),
            None => Vec::new(),
        }
    }

    /// Semantic receiver type if this is a method (first argument), else None.
    pub fn receiver_ty(self, db: &'db dyn HirAnalysisDb) -> Option<Binder<TyId<'db>>> {
        self.is_method(db)
            .then(|| self.arg_tys(db).into_iter().next())
            .flatten()
    }

    /// Expected `Self` type for this function when it is an associated method.
    /// - In trait: the trait's `Self` parameter (identity instantiation)
    /// - In impl/impl_trait: the implementor type
    pub fn expected_self_ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        match self.scope().parent(db)? {
            ScopeId::Item(ItemKind::Trait(tr)) => Some(tr.self_param(db)),
            ScopeId::Item(ItemKind::ImplTrait(it)) => Some(it.ty(db)),
            ScopeId::Item(ItemKind::Impl(im)) => Some(im.ty(db)),
            _ => None,
        }
    }

    /// Return type lowering errors for functions with explicit return types.
    pub fn ret_ty_errors(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let Some(hir_ty) = self.ret_type_ref(db) else {
            return Vec::new();
        };
        let assumptions = self.assumptions(db);
        collect_ty_lower_errors(
            db,
            self.scope(),
            hir_ty,
            self.span().sig().ret_ty(),
            assumptions,
        )
    }

    /// Returns the containing `impl Trait` block if this function is a method
    /// inside an impl trait block.
    pub fn containing_impl_trait(self, db: &'db dyn HirDb) -> Option<ImplTrait<'db>> {
        match self.scope().parent(db)? {
            ScopeId::Item(ItemKind::ImplTrait(impl_trait)) => Some(impl_trait),
            _ => None,
        }
    }

    /// Returns the containing trait if this function is a method inside a trait definition.
    pub fn containing_trait(self, db: &'db dyn HirDb) -> Option<Trait<'db>> {
        match self.scope().parent(db)? {
            ScopeId::Item(ItemKind::Trait(trait_)) => Some(trait_),
            _ => None,
        }
    }

    /// Returns the containing inherent `impl` block if this function is a method
    /// inside an `impl` block (not an `impl Trait` block).
    pub fn containing_impl(self, db: &'db dyn HirDb) -> Option<Impl<'db>> {
        match self.scope().parent(db)? {
            ScopeId::Item(ItemKind::Impl(impl_)) => Some(impl_),
            _ => None,
        }
    }

    /// If this function is a method inside an `impl Trait` block, returns the
    /// corresponding trait method definition.
    ///
    /// Returns `None` if:
    /// - This function is not inside an impl trait block
    /// - The impl trait block's trait cannot be resolved
    /// - No matching method exists in the trait definition
    pub fn trait_method_def(self, db: &'db dyn HirAnalysisDb) -> Option<Func<'db>> {
        let impl_trait = self.containing_impl_trait(db)?;
        let trait_ = impl_trait.trait_def(db)?;
        let method_name = self.name(db).to_opt()?;

        trait_
            .methods(db)
            .find(|m| m.name(db).to_opt() == Some(method_name))
    }
}

// Call site analysis --------------------------------------------------------

/// A call site found inside a function body.
#[derive(Clone, Debug)]
pub struct CallSiteView<'db> {
    pub body: Body<'db>,
    pub expr_id: ExprId,
    pub kind: CallSiteKind<'db>,
}

/// Discriminant for the shape of a call expression.
#[derive(Clone, Debug)]
pub enum CallSiteKind<'db> {
    FnCall,
    MethodCall { method_name: Partial<IdentId<'db>> },
}

impl<'db> CallSiteView<'db> {
    /// Resolve the callee of this call site via type checking.
    pub fn target(&self, db: &'db dyn HirAnalysisDb) -> Option<CallableDef<'db>> {
        use crate::analysis::ty::ty_check::check_func_body;
        let func = self.body.containing_func(db)?;
        let (_, typed_body) = check_func_body(db, func);
        typed_body
            .callable_expr(self.expr_id)
            .map(|c| c.callable_def)
    }

    /// Span of the callee name at the call site (function path or method name).
    pub fn callee_span(&self) -> crate::span::DynLazySpan<'db> {
        let expr_span = self.expr_id.span(self.body);
        match &self.kind {
            CallSiteKind::FnCall => expr_span.into_call_expr().callee().into(),
            CallSiteKind::MethodCall { .. } => {
                expr_span.into_method_call_expr().method_name().into()
            }
        }
    }

    /// Span of the entire call expression.
    pub fn call_span(&self) -> crate::span::DynLazySpan<'db> {
        self.expr_id.span(self.body).into()
    }

    /// The function containing this call site.
    pub fn containing_func(&self, db: &'db dyn HirDb) -> Option<Func<'db>> {
        self.body.containing_func(db)
    }
}

impl<'db> Body<'db> {
    /// Enumerate all call sites (function calls and method calls) in this body.
    pub fn call_sites(self, db: &'db dyn HirDb) -> Vec<CallSiteView<'db>> {
        let mut sites = Vec::new();
        for (expr_id, partial_expr) in self.exprs(db).iter() {
            let Partial::Present(expr) = partial_expr else {
                continue;
            };
            match expr {
                Expr::Call(_, _) => {
                    sites.push(CallSiteView {
                        body: self,
                        expr_id,
                        kind: CallSiteKind::FnCall,
                    });
                }
                Expr::MethodCall(_, method_name, _, _) => {
                    sites.push(CallSiteView {
                        body: self,
                        expr_id,
                        kind: CallSiteKind::MethodCall {
                            method_name: *method_name,
                        },
                    });
                }
                _ => {}
            }
        }
        sites
    }
}

impl<'db> CallableDef<'db> {
    pub fn name_span(self) -> crate::span::DynLazySpan<'db> {
        match self {
            Self::Func(func) => func.span().name().into(),
            Self::VariantCtor(v) => v.span().name().into(),
        }
    }

    pub fn is_method(self, db: &dyn HirDb) -> bool {
        match self {
            Self::Func(func) => func.is_method(db),
            Self::VariantCtor(..) => false,
        }
    }

    pub fn has_body(self, db: &dyn HirDb) -> bool {
        match self {
            Self::Func(func) => func.body(db).is_some(),
            Self::VariantCtor(..) => false,
        }
    }

    pub fn is_must_use(self, db: &'db dyn HirDb) -> bool {
        match self {
            Self::Func(func) => func.is_must_use(db),
            Self::VariantCtor(..) => false,
        }
    }

    pub fn ingot(self, db: &'db dyn HirDb) -> common::ingot::Ingot<'db> {
        match self {
            Self::Func(func) => func.top_mod(db).ingot(db),
            Self::VariantCtor(v) => v.enum_.top_mod(db).ingot(db),
        }
    }

    pub fn scope(self) -> ScopeId<'db> {
        match self {
            Self::Func(func) => func.scope(),
            Self::VariantCtor(v) => ScopeId::Variant(v),
        }
    }

    pub fn param_list_span(self) -> crate::span::DynLazySpan<'db> {
        match self {
            Self::Func(func) => func.span().params().into(),
            Self::VariantCtor(v) => v.span().tuple_type().into(),
        }
    }

    pub fn param_span(self, idx: usize) -> crate::span::DynLazySpan<'db> {
        match self {
            Self::Func(func) => func.span().params().param(idx).into(),
            Self::VariantCtor(var) => var.span().tuple_type().elem_ty(idx).into(),
        }
    }

    pub fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        match self {
            Self::Func(func) => collect_generic_params(db, func.into()).params(db),
            Self::VariantCtor(var) => {
                let adt = var.enum_.as_adt(db);
                adt.params(db)
            }
        }
    }

    pub fn explicit_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        match self {
            Self::Func(func) => collect_generic_params(db, func.into()).explicit_params(db),
            Self::VariantCtor(var) => {
                let adt = var.enum_.as_adt(db);
                adt.params(db)
            }
        }
    }

    pub fn offset_to_explicit_params_position(self, db: &'db dyn HirAnalysisDb) -> usize {
        match self {
            Self::Func(func) => {
                collect_generic_params(db, func.into()).offset_to_explicit_params_position(db)
            }
            Self::VariantCtor(_) => 0, // Variant constructors don't have implicit self parameters
        }
    }

    /// Callable name (if present). Variant ctors may be absent when the name is elided.
    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        match self {
            Self::Func(func) => func.name(db).to_opt(),
            Self::VariantCtor(var) => var.ident(db),
        }
    }

    pub fn param_label(self, db: &'db dyn HirDb, idx: usize) -> Option<IdentId<'db>> {
        match self {
            Self::Func(func) => func.param_label(db, idx),
            Self::VariantCtor(_) => None,
        }
    }

    pub fn param_label_or_name(self, db: &'db dyn HirDb, idx: usize) -> Option<FuncParamName<'db>> {
        match self {
            Self::Func(func) => func.param_label_or_name(db, idx),
            Self::VariantCtor(_) => None,
        }
    }

    pub fn arg_tys(self, db: &'db dyn HirAnalysisDb) -> Vec<Binder<TyId<'db>>> {
        match self {
            Self::Func(func) => func.arg_tys(db),
            Self::VariantCtor(var) => {
                let adt = var.enum_.as_adt(db);
                let field = &adt.fields(db)[var.idx as usize];
                field.iter_types(db).collect()
            }
        }
    }

    pub fn ret_ty(self, db: &'db dyn HirAnalysisDb) -> Binder<TyId<'db>> {
        match self {
            Self::Func(func) => Binder::bind(func.return_ty(db)),
            Self::VariantCtor(var) => {
                let adt = var.enum_.as_adt(db);
                let mut ty = TyId::adt(db, adt);
                for &param in adt.params(db) {
                    ty = TyId::app(db, ty, param);
                }
                Binder::bind(ty)
            }
        }
    }

    pub fn receiver_ty(self, db: &'db dyn HirAnalysisDb) -> Option<Binder<TyId<'db>>> {
        match self {
            Self::Func(func) if func.is_method(db) => func.arg_tys(db).into_iter().next(),
            _ => None,
        }
    }
}

// ADT items -----------------------------------------------------------------

#[derive(Clone, Copy, Debug)]
pub struct FuncParamView<'db> {
    func: Func<'db>,
    idx: usize,
}

impl<'db> FuncParamView<'db> {
    pub(crate) fn hir_ty(self, db: &'db dyn HirDb) -> Option<TypeId<'db>> {
        self.func
            .params_list(db)
            .to_opt()
            .and_then(|l| l.data(db).get(self.idx))
            .and_then(|param| param.ty.to_opt())
    }

    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        let list = self.func.params_list(db).to_opt()?;
        list.data(db).get(self.idx)?.name()
    }

    pub fn label(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        let list = self.func.params_list(db).to_opt()?;
        let param = list.data(db).get(self.idx)?;
        (!param.is_label_suppressed && !param.is_self_param(db))
            .then(|| param.name())
            .flatten()
    }

    pub fn is_label_suppressed(self, db: &'db dyn HirDb) -> bool {
        let list = self.func.params_list(db).to_opt();
        match list.and_then(|l| l.data(db).get(self.idx)) {
            Some(p) => p.is_label_suppressed,
            None => false,
        }
    }

    pub fn label_eagerly(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        let list = self.func.params_list(db).to_opt()?;
        list.data(db).get(self.idx)?.label_eagerly()
    }

    pub fn is_self_param(self, db: &'db dyn HirDb) -> bool {
        let list = self.func.params_list(db).to_opt();
        match list.and_then(|l| l.data(db).get(self.idx)) {
            Some(p) => p.is_self_param(db),
            None => false,
        }
    }

    pub fn is_mut(self, db: &'db dyn HirDb) -> bool {
        let list = self.func.params_list(db).to_opt();
        match list.and_then(|l| l.data(db).get(self.idx)) {
            Some(p) => p.is_mut,
            None => false,
        }
    }

    pub fn mode(self, db: &'db dyn HirDb) -> crate::hir_def::params::FuncParamMode {
        let list = self.func.params_list(db).to_opt();
        match list.and_then(|l| l.data(db).get(self.idx)) {
            Some(p) => p.mode,
            None => crate::hir_def::params::FuncParamMode::View,
        }
    }

    pub fn span(self) -> crate::span::params::LazyFuncParamSpan<'db> {
        self.func.span().params().param(self.idx)
    }

    pub fn self_ty_fallback(self, db: &'db dyn HirDb) -> bool {
        let list = self.func.params_list(db).to_opt();
        match list.and_then(|l| l.data(db).get(self.idx)) {
            Some(p) => p.self_ty_fallback,
            None => false,
        }
    }

    /// Semantic type of this parameter, bound to identity parameters.
    pub fn ty_binder(self, db: &'db dyn HirAnalysisDb) -> Binder<TyId<'db>> {
        // Delegate to the function-level lowering to keep behavior consistent.
        // Indexing is safe as long as `idx` was derived from the function's own
        // parameter list.
        self.func.arg_tys(db)[self.idx]
    }

    /// Semantic type of this parameter (binder removed).
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        *self.ty_binder(db).skip_binder()
    }

    /// Returns the owning function.
    pub fn func(self) -> Func<'db> {
        self.func
    }

    /// Returns the parameter index.
    pub fn index(self) -> usize {
        self.idx
    }

    /// Returns the lazy span for this parameter's type in `LazyTySpan` form.
    /// Handles self-parameter fallback span correctly.
    pub fn lazy_ty_span(self, db: &'db dyn HirDb) -> crate::span::types::LazyTySpan<'db> {
        if self.is_self_param(db) && self.self_ty_fallback(db) {
            self.span().fallback_self_ty()
        } else {
            self.span().ty()
        }
    }

    /// Returns the span for error reporting on this parameter's type.
    /// Handles self-parameter fallback span correctly.
    pub fn ty_span(self, db: &'db dyn HirDb) -> crate::span::DynLazySpan<'db> {
        if self.is_self_param(db) && self.self_ty_fallback(db) {
            self.span().name().into()
        } else {
            self.span().ty().into()
        }
    }

    /// All type-related diagnostics for this parameter.
    pub fn ty_diags(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let func = self.func;
        let assumptions = func.assumptions(db);

        let Some(param) = self
            .func
            .params_list(db)
            .to_opt()
            .and_then(|l| l.data(db).get(self.idx))
        else {
            return Vec::new();
        };
        let Some(hir_ty) = param.ty.to_opt() else {
            return Vec::new();
        };

        if self.is_self_param(db) && !param.self_ty_fallback {
            if param.has_ref_prefix {
                return vec![
                    TyLowerDiag::MixedRefSelfPrefixWithExplicitType {
                        span: self.span().ref_kw().into(),
                    }
                    .into(),
                ];
            }

            if param.has_own_prefix {
                return vec![
                    TyLowerDiag::MixedOwnSelfPrefixWithExplicitType {
                        span: self.span().own_kw().into(),
                    }
                    .into(),
                ];
            }

            if param.is_mut && !allows_mut_self_prefix_with_explicit_ty(db, hir_ty) {
                let span = self.span().mut_kw().into();
                return vec![TyLowerDiag::InvalidMutSelfPrefixWithExplicitType { span }.into()];
            }
        }

        if !self.is_self_param(db)
            && param.is_mut
            && !matches!(hir_ty.data(db), TypeKind::Mode(TypeMode::Own, _))
        {
            let span = self.span().mut_kw().into();
            return vec![TyLowerDiag::InvalidMutParamPrefixWithoutOwnType { span }.into()];
        }

        // Surface name-resolution errors for the parameter type first
        let errs =
            collect_hir_ty_diags(db, func.scope(), hir_ty, self.lazy_ty_span(db), assumptions);
        if !errs.is_empty() {
            return errs;
        }

        let semantic_ty = self.ty(db);
        let ty = if semantic_ty.has_invalid(db) {
            let layout_args = callable_input_layout_args(db, func);
            elaborate_func_param_ty(db, func, assumptions, &layout_args, self.idx, param, false)
        } else if self.mode(db) == crate::hir_def::params::FuncParamMode::View {
            semantic_ty.as_view(db).unwrap_or(semantic_ty)
        } else {
            semantic_ty
        };
        let ty_span = self.ty_span(db);

        let mut out = Vec::new();

        if !ty.has_star_kind(db) {
            out.push(TyDiagCollection::from(TyLowerDiag::ExpectedStarKind(
                ty_span.clone(),
            )));
            return out;
        }
        if ty.is_const_ty(db) {
            out.push(
                TyLowerDiag::NormalTypeExpected {
                    span: ty_span.clone(),
                    given: ty,
                }
                .into(),
            );
            return out;
        }

        if self.mode(db) == crate::hir_def::params::FuncParamMode::Own && ty.as_borrow(db).is_some()
        {
            out.push(
                TyLowerDiag::OwnParamCannotBeBorrow {
                    span: ty_span.clone(),
                    ty,
                }
                .into(),
            );
        }

        // Well-formedness / trait-bound satisfaction for parameter type
        if let WellFormedness::IllFormed { goal, subgoal } = check_ty_wf(
            db,
            TraitSolveCx::new(db, func.scope()).with_assumptions(param_env(db, func.into())),
            ty,
        ) {
            out.push(
                TraitConstraintDiag::TraitBoundNotSat {
                    span: ty_span.clone(),
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                    required_by: None,
                }
                .into(),
            );
        }

        // Self-parameter type shape check
        if self.is_self_param(db)
            && let Some(mut expected) = func.expected_self_ty(db)
            && !ty.has_invalid(db)
            && !expected.has_invalid(db)
        {
            if ty_contains_const_hole(db, expected) {
                let layout_args = callable_input_layout_args(db, func);
                if let Some(receiver_layout_args) =
                    layout_args.get(&CallableInputLayoutHoleOrigin::Receiver)
                {
                    expected =
                        substitute_layout_holes_by_identity(db, expected, receiver_layout_args);
                }
            }
            let ty_norm = normalize_ty(db, ty, func.scope(), assumptions);

            let matches_expected = |candidate: TyId<'db>| {
                let (exp_base, exp_args) = expected.decompose_ty_app(db);
                let (cand_base, cand_args) = candidate.decompose_ty_app(db);
                cand_base == exp_base
                    && cand_args.len() >= exp_args.len()
                    && exp_args.iter().zip(cand_args.iter()).all(|(a, b)| a == b)
            };

            let is_allowed_self_ty = matches_expected(ty_norm)
                || ty_norm
                    .as_capability(db)
                    .is_some_and(|(_, inner)| matches_expected(inner));

            if !is_allowed_self_ty {
                out.push(
                    ImplDiag::InvalidSelfType {
                        span: ty_span,
                        expected,
                        given: ty_norm,
                    }
                    .into(),
                );
            }
        }

        out
    }
}

fn allows_mut_self_prefix_with_explicit_ty<'db>(db: &'db dyn HirDb, hir_ty: TypeId<'db>) -> bool {
    if let TypeKind::Mode(TypeMode::Own, inner) = hir_ty.data(db)
        && let Some(inner) = inner.to_opt()
    {
        !is_bare_self_ty(db, inner)
    } else {
        false
    }
}

fn is_bare_self_ty<'db>(db: &'db dyn HirDb, hir_ty: TypeId<'db>) -> bool {
    if let TypeKind::Path(path) = hir_ty.data(db)
        && let Some(path) = path.to_opt()
    {
        path.is_self_ty(db) && path.generic_args(db).is_empty(db)
    } else {
        false
    }
}

// Effect param views --------------------------------------------------------

#[salsa::interned]
#[derive(Debug)]
pub struct RecvView<'db> {
    pub contract: Contract<'db>,
    pub recv_idx: u32,
}

impl<'db> RecvView<'db> {
    pub fn index(self, db: &'db dyn HirDb) -> u32 {
        self.recv_idx(db)
    }

    pub fn msg_path(self, db: &'db dyn HirDb) -> Option<PathId<'db>> {
        self.contract(db)
            .recvs(db)
            .data(db)
            .get(self.recv_idx(db) as usize)
            .and_then(|r| r.msg_path)
    }

    pub fn arm(self, db: &'db dyn HirDb, arm_idx: u32) -> Option<RecvArmView<'db>> {
        self.contract(db)
            .recv_arm(db, self.recv_idx(db) as usize, arm_idx as usize)
            .map(|_| RecvArmView::new(db, self, arm_idx))
    }

    pub fn arms(self, db: &'db dyn HirDb) -> impl Iterator<Item = RecvArmView<'db>> + 'db {
        let len = self
            .contract(db)
            .recvs(db)
            .data(db)
            .get(self.recv_idx(db) as usize)
            .map(|r| r.arms.data(db).len())
            .unwrap_or(0);
        (0..len).map(move |arm_idx| RecvArmView::new(db, self, arm_idx as u32))
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct RecvArmView<'db> {
    pub recv: RecvView<'db>,
    pub arm_idx: u32,
}

impl<'db> RecvArmView<'db> {
    pub fn index(self, db: &'db dyn HirDb) -> u32 {
        self.arm_idx(db)
    }

    pub fn contract(self, db: &'db dyn HirDb) -> Contract<'db> {
        self.recv(db).contract(db)
    }

    pub fn arm(self, db: &'db dyn HirDb) -> Option<ContractRecvArm<'db>> {
        let recv = self.recv(db);
        recv.contract(db)
            .recv_arm(db, recv.recv_idx(db) as usize, self.arm_idx(db) as usize)
    }

    pub fn is_fallback(self, db: &'db dyn HirDb) -> bool {
        self.arm(db).is_some_and(|arm| arm.is_fallback(db))
    }

    pub fn effects(self, db: &'db dyn HirDb) -> EffectParamListId<'db> {
        self.arm(db)
            .map(|a| a.effects)
            .unwrap_or_else(|| EffectParamListId::new(db, Vec::new()))
    }

    pub fn effective_effect_env(self, db: &'db dyn HirAnalysisDb) -> EffectEnvView<'db> {
        let contract = self.contract(db);
        let recv_idx = self.recv(db).recv_idx(db);
        let arm_idx = self.arm_idx(db);
        EffectEnvView {
            site: EffectParamSite::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            },
        }
    }
}

#[salsa::tracked]
impl<'db> RecvArmView<'db> {
    #[salsa::tracked]
    pub fn variant_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let assumptions = PredicateListId::empty_list(db);
        let recv = self.recv(db);
        let contract = recv.contract(db);

        let recv_is_bare = recv.msg_path(db).is_none();
        let msg_mod = recv
            .msg_path(db)
            .and_then(|p| resolve_msg_mod(db, contract, p, assumptions));

        let Some(hir_arm) = self.arm(db) else {
            return TyId::invalid(db, InvalidCause::Other);
        };

        let (_variant_struct, variant_ty) =
            resolve_recv_variant(db, contract, msg_mod, recv_is_bare, hir_arm, assumptions);

        variant_ty.unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }

    #[salsa::tracked]
    pub fn abi_info(self, db: &'db dyn HirAnalysisDb, abi: TyId<'db>) -> RecvArmAbiInfo<'db> {
        if self.is_fallback(db) {
            return RecvArmAbiInfo {
                is_fallback: true,
                selector_value: None,
                selector_signature: None,
                args_ty: TyId::unit(db),
                ret_ty: None,
            };
        }

        let assumptions = PredicateListId::empty_list(db);
        let recv = self.recv(db);
        let contract = recv.contract(db);

        let variant_ty = self.variant_ty(db);
        let selector_info = get_variant_selector_info(db, variant_ty, contract.scope());

        let Some(msg_variant_trait) =
            resolve_core_trait(db, contract.scope(), &["message", "MsgVariant"])
        else {
            return RecvArmAbiInfo {
                is_fallback: false,
                selector_value: selector_info.value,
                selector_signature: selector_info.signature,
                args_ty: variant_ty,
                ret_ty: None,
            };
        };
        let return_ident = IdentId::new(db, "Return".to_string());

        let variant_ret_ty = if !variant_ty.has_invalid(db) && !abi.has_invalid(db) {
            let inst = TraitInstId::new(
                db,
                msg_variant_trait,
                vec![variant_ty, abi],
                IndexMap::new(),
            );
            let return_proj = TyId::assoc_ty(db, inst, return_ident);
            normalize_ty(db, return_proj, contract.scope(), assumptions)
        } else {
            TyId::invalid(db, InvalidCause::Other)
        };

        let args_ty = variant_ty;
        let ret_ty = (variant_ret_ty != TyId::unit(db)).then_some(variant_ret_ty);

        RecvArmAbiInfo {
            is_fallback: false,
            selector_value: selector_info.value,
            selector_signature: selector_info.signature,
            args_ty,
            ret_ty,
        }
    }

    #[salsa::tracked(return_ref)]
    pub fn arg_bindings(self, db: &'db dyn HirAnalysisDb) -> Vec<ArgBinding<'db>> {
        if self.is_fallback(db) {
            return Vec::new();
        }

        let assumptions = PredicateListId::empty_list(db);
        let recv = self.recv(db);
        let contract = recv.contract(db);

        let Some(sol_ty) = resolve_sol_abi_ty(db, contract.scope(), assumptions) else {
            return Vec::new();
        };

        let variant_ty = self.variant_ty(db);
        let Some(variant_struct) = variant_struct_from_ty(db, variant_ty) else {
            return Vec::new();
        };

        let Some(hir_arm) = self.arm(db) else {
            return Vec::new();
        };

        let abi_info = self.abi_info(db, sol_ty);
        compute_arg_bindings(
            db,
            variant_struct,
            abi_info.args_ty,
            hir_arm.pat,
            hir_arm.body,
        )
        .unwrap_or_default()
    }

    #[salsa::tracked(return_ref)]
    pub fn effective_effect_requirements(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<EffectRequirement<'db>> {
        let contract = self.contract(db);
        let recv_idx = self.recv(db).recv_idx(db);
        let arm_idx = self.arm_idx(db);
        let Some(arm) = self.arm(db) else {
            return Vec::new();
        };

        let site = EffectParamSite::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        };
        contract_scoped_effect_requirements_canonical(db, contract, site, arm.effects)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ContractFieldInfo<'db> {
    pub index: u32,
    pub name: IdentId<'db>,
    pub declared_ty: TyId<'db>,
    pub is_mut: bool,
    pub is_provider: bool,
    pub target_ty: TyId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ContractFieldLayoutInfo<'db> {
    pub index: u32,
    pub name: IdentId<'db>,
    pub declared_ty: TyId<'db>,
    pub is_mut: bool,
    pub is_provider: bool,
    pub target_ty: TyId<'db>,
    /// Semantic address space in which this field is allocated.
    pub address_space: crate::analysis::ty::ProviderAddressSpace,
    /// Slot offset from the start of `address_space`.
    pub slot_offset: usize,
    /// Total number of slots consumed by this field.
    pub slot_count: usize,
    /// True when this field embeds a `StaticSlot` type whose `SPACE` could not
    /// be resolved to a concrete address space. The layout is unsound and the
    /// field is reported as an error by `FieldView::ty_diags`.
    pub static_slot_space_unresolved: bool,
    /// True when this field carries an explicit `_` const argument (e.g.
    /// `String<_>`); a layout hole must come from a `= _` parameter default. The
    /// field is reported as an error.
    pub explicit_const_hole: bool,
    /// True when this field has an unresolved const hole (`_`) that is not a
    /// storage-slot index (e.g. a defaulted `const SP: AddressSpace = _`). The
    /// hole is left unnumbered; the field is reported as an error.
    pub non_slot_const_hole: bool,
    /// True when this field is a selected `EffectHandle` whose `SPACE` did not
    /// resolve to a concrete address space; the field is reported as an error.
    pub handle_space_unresolved: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ArgBinding<'db> {
    pub pat: PatId,
    pub tuple_index: u32,
    pub ty: TyId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum EffectRequirementKey<'db> {
    Type(TyId<'db>),
    Trait(TraitInstId<'db>),
    Other,
}

impl<'db> EffectRequirementKey<'db> {
    pub fn from_parts(
        key_kind: EffectKeyKind,
        key_ty: Option<TyId<'db>>,
        key_trait: Option<TraitInstId<'db>>,
    ) -> Self {
        match key_kind {
            EffectKeyKind::Type => key_ty.map_or(Self::Other, Self::Type),
            EffectKeyKind::Trait => key_trait.map_or(Self::Other, Self::Trait),
            EffectKeyKind::Other => Self::Other,
        }
    }

    pub fn kind(&self) -> EffectKeyKind {
        match self {
            Self::Type(_) => EffectKeyKind::Type,
            Self::Trait(_) => EffectKeyKind::Trait,
            Self::Other => EffectKeyKind::Other,
        }
    }

    pub fn key_ty(&self) -> Option<TyId<'db>> {
        match self {
            Self::Type(ty) => Some(*ty),
            Self::Trait(_) | Self::Other => None,
        }
    }

    pub fn key_trait(&self) -> Option<TraitInstId<'db>> {
        match self {
            Self::Trait(trait_inst) => Some(*trait_inst),
            Self::Type(_) | Self::Other => None,
        }
    }

    pub fn binding_ty(&self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        match self {
            Self::Type(ty) => Some(*ty),
            Self::Trait(trait_inst) => Some(trait_inst.self_ty(db)),
            Self::Other => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct EffectRequirement<'db> {
    pub binding_name: IdentId<'db>,
    pub key: EffectRequirementKey<'db>,
    pub is_mut: bool,
    pub binding_site: EffectParamSite<'db>,
    pub binding_idx: u32,
    /// The path written at the binding site (e.g. `uses (ctx)` or `uses (mut store)`).
    ///
    /// Note: this is not necessarily the semantic "key path" that resolves to a type/trait; for
    /// contract-scoped named imports, this is the import name, while the resolved key is captured
    /// by `key`.
    pub binding_path: PathId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum ProviderSource<'db> {
    UsesParam {
        site: EffectParamSite<'db>,
        requirement_idx: u32,
    },
    ContractField {
        contract: Contract<'db>,
        field_idx: u32,
    },
    RootProvider {
        site: EffectParamSite<'db>,
        registration: RootProviderRegistration<'db>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ProviderBinding<'db> {
    pub provider_idx: u32,
    pub provider_ty: TyId<'db>,
    pub is_mut: bool,
    pub source: ProviderSource<'db>,
    pub semantics: ProviderSemantics<'db>,
}

impl<'db> ProviderBinding<'db> {
    pub fn effective_target_ty(&self) -> TyId<'db> {
        self.semantics.target_ty.unwrap_or(self.provider_ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ResolvedEffectBinding {
    pub requirement_idx: u32,
    pub provider_idx: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ResolvedEffectBindingInfo<'db> {
    pub requirement: EffectRequirement<'db>,
    pub provider: ProviderBinding<'db>,
}

#[salsa::interned]
pub struct EffectEnvSite<'db> {
    pub site: EffectParamSite<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct EffectEnvView<'db> {
    site: EffectParamSite<'db>,
}

impl<'db> EffectEnvView<'db> {
    pub fn new(site: EffectParamSite<'db>) -> Self {
        Self { site }
    }

    pub fn site(self) -> EffectParamSite<'db> {
        self.site
    }

    pub fn requirements(self, db: &'db dyn HirAnalysisDb) -> Vec<EffectRequirement<'db>> {
        effect_requirements_for_site(db, self.site)
    }

    pub fn resolved_binding(
        self,
        db: &'db dyn HirAnalysisDb,
        idx: usize,
    ) -> Option<ResolvedEffectBindingInfo<'db>> {
        resolved_effect_binding_infos_for_site(db, EffectEnvSite::new(db, self.site))
            .get(idx)
            .and_then(|binding| binding.clone())
    }

    /// Returns the type of the effect binding as it is visible in the body.
    ///
    /// For a type-key effect like `uses (evm: Evm)`, this is the visible key
    /// type `Evm`. For a trait-key effect, the binding's value has the hidden
    /// provider type that implements the trait key.
    pub fn visible_effect_binding_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        idx: usize,
    ) -> Option<TyId<'db>> {
        let requirement = self
            .requirements(db)
            .into_iter()
            .find(|binding| binding.binding_idx as usize == idx)?;
        let provider = self
            .resolved_binding(db, idx)
            .map(|binding| binding.provider);

        match requirement.key {
            EffectRequirementKey::Trait(_) => provider
                .map(|binding| binding.provider_ty)
                .or_else(|| requirement.key.binding_ty(db)),
            EffectRequirementKey::Type(_) | EffectRequirementKey::Other => requirement
                .key
                .binding_ty(db)
                .or_else(|| provider.map(|binding| binding.provider_ty)),
        }
    }

    /// Returns the hidden provider type associated with an effect binding.
    pub fn hidden_provider_binding_ty(
        self,
        db: &'db dyn HirAnalysisDb,
        idx: usize,
    ) -> Option<TyId<'db>> {
        self.resolved_binding(db, idx)
            .map(|binding| binding.provider.provider_ty)
    }

    pub fn providers(self, db: &'db dyn HirAnalysisDb) -> Vec<ProviderBinding<'db>> {
        provider_bindings_for_site(db, self.site).to_vec()
    }

    pub fn resolutions(self, db: &'db dyn HirAnalysisDb) -> Vec<ResolvedEffectBinding> {
        effect_resolutions_for_site(db, self.site).to_vec()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct RecvArmAbiInfo<'db> {
    pub is_fallback: bool,
    pub selector_value: Option<u32>,
    pub selector_signature: Option<String>,
    pub args_ty: TyId<'db>,
    pub ret_ty: Option<TyId<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
struct VariantSelectorInfo {
    value: Option<u32>,
    signature: Option<String>,
}

fn variant_struct_from_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<Struct<'db>> {
    match ty.base_ty(db).data(db) {
        TyData::TyBase(TyBase::Adt(adt)) => match adt.adt_ref(db) {
            AdtRef::Struct(struct_) => Some(struct_),
            _ => None,
        },
        _ => None,
    }
}

fn slot_const_ty<'db>(db: &'db dyn HirAnalysisDb, value: usize, ty: TyId<'db>) -> TyId<'db> {
    let int = IntegerId::new(db, BigUint::from(value));
    let const_ty = ConstTyId::new(
        db,
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int), ty),
    );
    TyId::new(db, TyData::ConstTy(const_ty))
}

fn const_ty_to_usize<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<usize> {
    let TyData::ConstTy(const_ty) = ty.data(db) else {
        return None;
    };
    match const_ty.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => int_id.data(db).to_usize(),
        _ => None,
    }
}

/// Returns the number of *inline* storage slots `ty` occupies.
///
/// Layout placeholders contribute zero width: a hole's value is the root of
/// out-of-line storage, not an inline position, so hole roots are allocated
/// after the field's inline span (see
/// [`contract_field_layout_slot_assignments`]). This makes the layout immune
/// to overlay aliasing: enum variant payloads share inline slots, and a hole
/// root past every variant's inline span can never collide with inline data
/// in any variant, regardless of variant order or nesting.
fn contract_field_inline_slot_span<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    placeholders: &FxHashSet<TyId<'db>>,
    visiting: &mut FxHashSet<TyId<'db>>,
) -> usize {
    if placeholders.contains(&ty) {
        return 0;
    }

    if !visiting.insert(ty) {
        return 1;
    }

    let slots = if let Some((_, inner_ty)) = ty.as_capability(db) {
        contract_field_inline_slot_span(db, inner_ty, placeholders, visiting)
    } else if let TyData::ConstTy(const_ty) = ty.data(db) {
        contract_field_inline_slot_span(db, const_ty.ty(db), placeholders, visiting)
    } else if ty.adt_def(db).is_none() && (ty.is_never(db) || ty.is_zero_sized(db)) {
        0
    } else if let TyData::TyParam(param) = ty.data(db)
        && (param.is_effect() || param.is_effect_provider() || param.is_trait_self())
    {
        0
    } else if let TyData::TyBase(TyBase::Func(_) | TyBase::Contract(_)) = ty.base_ty(db).data(db) {
        0
    } else if ty.is_tuple(db) {
        ty.field_types(db)
            .into_iter()
            .map(|field_ty| contract_field_inline_slot_span(db, field_ty, placeholders, visiting))
            .fold(0usize, usize::saturating_add)
    } else if ty.is_array(db) {
        let (_, args) = ty.decompose_ty_app(db);
        let elem_slots = args
            .first()
            .copied()
            .map(|elem_ty| contract_field_inline_slot_span(db, elem_ty, placeholders, visiting))
            .unwrap_or(1);
        let len = args
            .get(1)
            .copied()
            .and_then(|len_ty| const_ty_to_usize(db, len_ty))
            .unwrap_or(1);
        elem_slots.saturating_mul(len)
    } else if let Some(adt_def) = ty.adt_def(db) {
        let args = ty.generic_args(db);

        match adt_def.adt_ref(db) {
            AdtRef::Struct(_) => ty
                .field_types(db)
                .into_iter()
                .map(|field_ty| {
                    contract_field_inline_slot_span(db, field_ty, placeholders, visiting)
                })
                .fold(0usize, usize::saturating_add),
            AdtRef::Enum(_) => {
                // Variant payloads overlay each other; the enum's inline span
                // is the tag slot plus the widest variant payload.
                let max_payload = adt_def
                    .fields(db)
                    .iter()
                    .enumerate()
                    .map(|(variant_idx, variant)| {
                        variant
                            .iter_types(db)
                            .enumerate()
                            .map(|(field_idx, _)| {
                                let field_ty = instantiate_adt_field_ty(
                                    db,
                                    adt_def,
                                    variant_idx,
                                    field_idx,
                                    args,
                                );
                                contract_field_inline_slot_span(
                                    db,
                                    field_ty,
                                    placeholders,
                                    visiting,
                                )
                            })
                            .fold(0usize, usize::saturating_add)
                    })
                    .max()
                    .unwrap_or(0);
                1usize.saturating_add(max_payload)
            }
        }
    } else {
        1
    };

    visiting.remove(&ty);
    slots
}

struct ContractFieldLayoutPlan<'db> {
    declared_ty: TyId<'db>,
    is_provider: bool,
    address_space: crate::analysis::ty::ProviderAddressSpace,
    target_ty: TyId<'db>,
    slot_basis_ty: TyId<'db>,
    slot_placeholders: Vec<TyId<'db>>,
    materialization_placeholders: Vec<TyId<'db>>,
    /// Placeholders whose declaring param indexes a different address space
    /// (`core::effect_ref::StaticSlot`); assigned from that space's contract
    /// counter instead of the field's, and excluded from the field's span.
    space_overrides: Vec<(crate::analysis::ty::ProviderAddressSpace, TyId<'db>)>,
    /// True when a placeholder's owner implements `StaticSlot` but its `SPACE`
    /// could not be resolved to a concrete address space. Such a field cannot
    /// be laid out soundly (a silent fallback to the field's own counter risks
    /// a cross-space slot collision), so it is rejected with a diagnostic.
    static_slot_space_unresolved: bool,
    /// True when the field carries an explicit `_` const argument (e.g.
    /// `String<_>`). A layout hole must come from a `= _` parameter default, not
    /// an explicit argument; the field is rejected.
    explicit_const_hole: bool,
    /// True when the field carries an unresolved const hole (`_`) whose type is
    /// not a storage-slot index (e.g. a defaulted `const SP: AddressSpace = _`).
    /// Such a hole is not a slot and must not be numbered; the field is rejected.
    non_slot_const_hole: bool,
    /// True when the field is a selected `EffectHandle` whose `const SPACE` did
    /// not resolve to a concrete address space (so its slot space is unknown).
    handle_space_unresolved: bool,
}

/// Result of probing a contract field's `EffectHandle` implementation.
struct ContractFieldMetadata<'db> {
    is_provider: bool,
    address_space: crate::analysis::ty::ProviderAddressSpace,
    target_ty: TyId<'db>,
    handle_space_unresolved: bool,
}

/// Whether a layout placeholder's slot is governed by a `StaticSlot`
/// implementation, and if so which address space it indexes.
enum StaticSlotSpace {
    /// The placeholder's owner does not implement `StaticSlot`; it is an
    /// ordinary layout hole numbered from the field's own address space.
    NotStaticSlot,
    /// The owner implements `StaticSlot` and `SPACE` resolved to this space.
    Resolved(crate::analysis::ty::ProviderAddressSpace),
    /// The owner implements `StaticSlot` but `SPACE` could not be evaluated to a
    /// concrete address space.
    Unresolvable,
}

#[derive(Clone, Copy)]
struct ContractFieldEffectHandleCx<'db> {
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    effect_handle: Trait<'db>,
    static_slot: Option<Trait<'db>>,
    target_ident: IdentId<'db>,
    fallback_space: crate::analysis::ty::ProviderAddressSpace,
}

impl<'db> ContractFieldEffectHandleCx<'db> {
    fn metadata(
        self,
        db: &'db dyn HirAnalysisDb,
        field_ty: TyId<'db>,
    ) -> ContractFieldMetadata<'db> {
        let inst = TraitInstId::new(db, self.effect_handle, vec![field_ty], IndexMap::new());
        match is_goal_satisfiable(db, TraitSolveCx::new(db, self.scope), inst) {
            GoalSatisfiability::ContainsInvalid | GoalSatisfiability::UnSat(_) => {
                ContractFieldMetadata {
                    is_provider: false,
                    address_space: self.fallback_space,
                    target_ty: field_ty,
                    handle_space_unresolved: false,
                }
            }
            GoalSatisfiability::Satisfied(_) | GoalSatisfiability::NeedsConfirmation(_) => {
                // A selected handle must name a concrete address space via its
                // `const SPACE`. If it does not, record it: the field is rejected
                // rather than silently classified into the fallback space.
                let resolved = crate::analysis::ty::effect_space_from_trait_const(
                    db,
                    self.scope,
                    self.assumptions,
                    inst,
                );
                let target_ty = inst
                    .assoc_ty(db, self.target_ident)
                    .map(|assoc| normalize_ty(db, assoc, self.scope, self.assumptions))
                    .filter(|ty| !ty.has_invalid(db))
                    .unwrap_or(field_ty);

                ContractFieldMetadata {
                    is_provider: true,
                    address_space: resolved.unwrap_or(self.fallback_space),
                    target_ty,
                    handle_space_unresolved: resolved.is_none(),
                }
            }
        }
    }

    /// The address space a placeholder's slot indexes, when the generic
    /// param that declared the hole belongs to an ADT implementing
    /// `core::effect_ref::StaticSlot` (e.g. `TSlot`'s slot param indexes
    /// transient storage regardless of where the `TSlot` is embedded).
    ///
    /// Distinguishes "not a `StaticSlot` hole" (an ordinary layout hole) from
    /// "a `StaticSlot` hole whose `SPACE` is unresolvable": the latter must be
    /// rejected rather than silently numbered from the field's own counter,
    /// which would risk a cross-space slot collision.
    fn placeholder_space_resolution(
        self,
        db: &'db dyn HirAnalysisDb,
        placeholder: TyId<'db>,
    ) -> StaticSlotSpace {
        use crate::analysis::name_resolution::PathRes;
        use crate::analysis::ty::const_ty::{LayoutHoleArgSite, StructuralHoleOrigin};
        use crate::analysis::ty::layout_holes::structural_hole_origin;

        let Some(static_slot) = self.static_slot else {
            return StaticSlotSpace::NotStaticSlot;
        };
        // Recover the ADT that declared the hole. The `StaticSlot::SPACE` of a
        // slot type is a property of the type's impl, so it is read from the
        // owner's generic form (uniform across explicit `_` args and defaulted
        // holes); `SPACE` must therefore not depend on the impl's parameters.
        let owner_item = match structural_hole_origin(db, placeholder) {
            Some(StructuralHoleOrigin::DefaultHoleParam { owner, .. }) => owner.scope().item(),
            Some(StructuralHoleOrigin::ExplicitWildcard {
                site: LayoutHoleArgSite::Path(path),
                ..
            }) => {
                let Ok(res) = crate::analysis::name_resolution::resolve_path(
                    db,
                    path,
                    self.scope,
                    self.assumptions,
                    false,
                ) else {
                    return StaticSlotSpace::NotStaticSlot;
                };
                let (PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) = res else {
                    return StaticSlotSpace::NotStaticSlot;
                };
                let Some(adt) = ty.adt_def(db) else {
                    return StaticSlotSpace::NotStaticSlot;
                };
                adt.adt_ref(db).as_item()
            }
            _ => return StaticSlotSpace::NotStaticSlot,
        };

        let Some(adt_ref) = AdtRef::try_from_item(owner_item) else {
            return StaticSlotSpace::NotStaticSlot;
        };
        let adt = adt_ref.as_adt(db);
        let mut self_ty = TyId::adt(db, adt);
        for &param in adt.params(db) {
            self_ty = TyId::app(db, self_ty, param);
        }

        let inst = TraitInstId::new(db, static_slot, vec![self_ty], IndexMap::new());
        let solve_cx = TraitSolveCx::new(db, self.scope).with_assumptions(self.assumptions);
        match is_goal_satisfiable(db, solve_cx, inst) {
            // The owner does not implement `StaticSlot`: an ordinary layout hole.
            GoalSatisfiability::ContainsInvalid | GoalSatisfiability::UnSat(_) => {
                StaticSlotSpace::NotStaticSlot
            }
            // The owner implements `StaticSlot`, so `SPACE` must resolve.
            GoalSatisfiability::Satisfied(_) | GoalSatisfiability::NeedsConfirmation(_) => {
                match crate::analysis::ty::effect_space_from_trait_const(
                    db,
                    self.scope,
                    self.assumptions,
                    inst,
                ) {
                    Some(space) => StaticSlotSpace::Resolved(space),
                    None => StaticSlotSpace::Unresolvable,
                }
            }
        }
    }

    fn layout_plan(
        self,
        db: &'db dyn HirAnalysisDb,
        field_ty: TyId<'db>,
    ) -> ContractFieldLayoutPlan<'db> {
        let ContractFieldMetadata {
            is_provider,
            address_space,
            target_ty,
            handle_space_unresolved,
        } = self.metadata(db, field_ty);
        // Provider slot order is defined by the normalized Target type. Contract
        // layout must not synthesize a positional equivalence with the wrapper.
        let slot_basis_ty = if is_provider { target_ty } else { field_ty };
        let mut slot_placeholders = collect_unique_layout_placeholders_in_order_with_policy(
            db,
            slot_basis_ty,
            LayoutPlaceholderPolicy::HolesAndImplicitParams,
        );
        let other_placeholders = if is_provider {
            collect_unique_layout_placeholders_in_order_with_policy(
                db,
                field_ty,
                LayoutPlaceholderPolicy::HolesAndImplicitParams,
            )
        } else {
            collect_unique_layout_placeholders_in_order_with_policy(
                db,
                target_ty,
                LayoutPlaceholderPolicy::HolesAndImplicitParams,
            )
        };
        let mut seen = slot_placeholders.iter().copied().collect::<FxHashSet<_>>();
        let mut materialization_placeholders = slot_placeholders.clone();
        materialization_placeholders.extend(
            other_placeholders
                .into_iter()
                .filter(|placeholder| seen.insert(*placeholder)),
        );

        let mut space_overrides = Vec::new();
        let mut overridden = FxHashSet::default();
        let mut rejected_holes = FxHashSet::default();
        let mut static_slot_space_unresolved = false;
        let mut explicit_const_hole = false;
        let mut non_slot_const_hole = false;
        for &placeholder in &materialization_placeholders {
            // A storage-slot layout hole must be declared by the type author via
            // a `= _` parameter default. An explicit `_` argument (e.g.
            // `String<_>`) is not a way to request a slot: reject it.
            if placeholder_is_explicit_hole(db, placeholder) {
                explicit_const_hole = true;
                rejected_holes.insert(placeholder);
                continue;
            }
            // A defaulted hole is a storage slot only if its const type is a slot
            // index (`u256`/`usize`). A hole of any other type (e.g.
            // `AddressSpace`, or a narrower integer) is an under-specified const,
            // not a slot. Rejected holes are dropped from the slot lists so they
            // are neither counted nor numbered, and the field is flagged.
            if !placeholder_is_slot_hole(db, placeholder) {
                non_slot_const_hole = true;
                rejected_holes.insert(placeholder);
                continue;
            }
            match self.placeholder_space_resolution(db, placeholder) {
                StaticSlotSpace::Resolved(space) if space != address_space => {
                    space_overrides.push((space, placeholder));
                    overridden.insert(placeholder);
                }
                // `Resolved` matching the field's own space already draws from
                // the correct counter; non-`StaticSlot` holes are ordinary.
                StaticSlotSpace::Resolved(_) | StaticSlotSpace::NotStaticSlot => {}
                // Keep the hole in the field's counter so the layout stays
                // materializable; the recorded flag makes the field a hard error
                // so this unsound layout never reaches codegen.
                StaticSlotSpace::Unresolvable => static_slot_space_unresolved = true,
            }
        }
        if !overridden.is_empty() || !rejected_holes.is_empty() {
            slot_placeholders.retain(|placeholder| {
                !overridden.contains(placeholder) && !rejected_holes.contains(placeholder)
            });
            materialization_placeholders.retain(|placeholder| {
                !overridden.contains(placeholder) && !rejected_holes.contains(placeholder)
            });
        }

        ContractFieldLayoutPlan {
            declared_ty: field_ty,
            is_provider,
            address_space,
            target_ty,
            slot_basis_ty,
            slot_placeholders,
            materialization_placeholders,
            space_overrides,
            static_slot_space_unresolved,
            explicit_const_hole,
            non_slot_const_hole,
            handle_space_unresolved,
        }
    }
}

/// Whether a contract-field layout placeholder is a genuine storage slot, i.e.
/// its const type is a storage-slot index type (`u256` or `usize`). A hole of
/// any other type — including a narrower integer such as `u8` — is an
/// under-specified const, not a slot, and must not be numbered. An invalid hole
/// type is treated as a `u256` slot (it is diagnosed elsewhere), matching
/// `layout_hole_fallback_ty`.
fn placeholder_is_slot_hole<'db>(db: &'db dyn HirAnalysisDb, placeholder: TyId<'db>) -> bool {
    let TyData::ConstTy(const_ty) = placeholder.data(db) else {
        return false;
    };
    let hole_ty = match const_ty.data(db) {
        ConstTyData::Hole(hole_ty, _) => *hole_ty,
        ConstTyData::TyParam(param, hole_ty) if param.is_implicit() => *hole_ty,
        _ => return false,
    };
    matches!(
        layout_hole_fallback_ty(db, hole_ty).data(db),
        TyData::TyBase(TyBase::Prim(PrimTy::U256 | PrimTy::Usize))
    )
}

/// Whether a contract-field layout placeholder came from an explicit `_`
/// argument (e.g. `String<_>`) rather than a `= _` parameter default. A storage
/// slot must be declared by the type author via a parameter default; an explicit
/// `_` is not a valid way to request one.
fn placeholder_is_explicit_hole<'db>(db: &'db dyn HirAnalysisDb, placeholder: TyId<'db>) -> bool {
    use crate::analysis::ty::const_ty::StructuralHoleOrigin;
    use crate::analysis::ty::layout_holes::structural_hole_origin;
    matches!(
        structural_hole_origin(db, placeholder),
        Some(StructuralHoleOrigin::ExplicitWildcard { .. })
    )
}

/// Assigns a slot value to every layout placeholder of a contract field and
/// returns the assignments along with the field's total slot count.
///
/// Hole roots are allocated *after* the field's inline span, in
/// `materialization_placeholders` order (basis placeholders first, in their
/// collection order over `slot_basis_ty`). Only basis placeholders contribute
/// to the slot count; placeholders that occur solely in the declared or
/// target type get slots past the counted span.
fn contract_field_layout_slot_assignments<'db>(
    db: &'db dyn HirAnalysisDb,
    slot_basis_ty: TyId<'db>,
    slot_placeholders: &[TyId<'db>],
    materialization_placeholders: &[TyId<'db>],
    start_slot: usize,
) -> (FxHashMap<TyId<'db>, TyId<'db>>, usize) {
    debug_assert!(materialization_placeholders.starts_with(slot_placeholders));

    let placeholder_set = slot_placeholders.iter().copied().collect::<FxHashSet<_>>();
    let span = contract_field_inline_slot_span(
        db,
        slot_basis_ty,
        &placeholder_set,
        &mut FxHashSet::default(),
    );

    let mut slot_count = span;
    let mut next_offset = span;
    let mut assignments = FxHashMap::default();
    for &placeholder in materialization_placeholders {
        let TyData::ConstTy(const_ty) = placeholder.data(db) else {
            continue;
        };
        let hole_ty = match const_ty.data(db) {
            ConstTyData::Hole(hole_ty, _) => *hole_ty,
            ConstTyData::TyParam(param, hole_ty) if param.is_implicit() => *hole_ty,
            _ => continue,
        };
        let offset = next_offset;
        next_offset = next_offset.saturating_add(1);
        if placeholder_set.contains(&placeholder) {
            slot_count = slot_count.saturating_add(1);
        }
        assignments.insert(
            placeholder,
            slot_const_ty(
                db,
                start_slot.saturating_add(offset),
                layout_hole_fallback_ty(db, hole_ty),
            ),
        );
    }

    (assignments, slot_count)
}

fn materialize_contract_layout_holes<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    slot_assignments: &FxHashMap<TyId<'db>, TyId<'db>>,
) -> TyId<'db> {
    substitute_layout_placeholders_by_identity(
        db,
        ty,
        slot_assignments,
        LayoutPlaceholderPolicy::HolesAndImplicitParams,
    )
}

#[salsa::tracked]
impl<'db> Contract<'db> {
    pub fn recv(self, db: &'db dyn HirDb, recv_idx: u32) -> Option<RecvView<'db>> {
        self.recvs(db)
            .data(db)
            .get(recv_idx as usize)
            .map(|_| RecvView::new(db, self, recv_idx))
    }

    pub fn init_effect_env(self, db: &'db dyn HirDb) -> Option<EffectEnvView<'db>> {
        self.init(db).map(|_| EffectEnvView {
            site: EffectParamSite::ContractInit { contract: self },
        })
    }

    pub fn recv_views(self, db: &'db dyn HirDb) -> impl Iterator<Item = RecvView<'db>> + 'db {
        let len = self.recvs(db).data(db).len();
        (0..len).map(move |idx| RecvView::new(db, self, idx as u32))
    }

    pub fn effect_params(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = EffectParamView<'db>> + 'db {
        let len = self.effects(db).data(db).len();
        let owner = EffectParamOwner::Contract(self);
        (0..len).map(move |idx| EffectParamView { owner, idx })
    }

    /// Contract field layout for semantic consumers.
    ///
    /// User-visible layout behavior:
    /// - Slot counters are maintained independently per address space.
    /// - No packing is performed.
    /// - Each non-zero-sized primitive consumes one slot.
    /// - Aggregate types consume the sum of component slots.
    /// - Enum fields consume one discriminant slot plus the max payload slots.
    /// - Each const layout hole (`_`) also consumes one slot, at its
    ///   structural position within the field's layout.
    #[salsa::tracked(return_ref)]
    pub fn field_layout(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> IndexMap<IdentId<'db>, ContractFieldLayoutInfo<'db>> {
        let scope = self.top_mod(db).scope();
        let assumptions = PredicateListId::empty_list(db);

        let effect_handle = resolve_core_trait(db, scope, &["EffectHandle"])
            .expect("missing required core trait `core::EffectHandle`");
        let static_slot = resolve_core_trait(db, scope, &["StaticSlot"]);
        let target_ident = IdentId::new(db, "Target".to_string());
        let effect_handle_cx_base = ContractFieldEffectHandleCx {
            scope,
            assumptions,
            effect_handle,
            static_slot,
            target_ident,
            fallback_space: crate::analysis::ty::ProviderAddressSpace::Storage,
        };

        let hir_fields = self.hir_fields(db).data(db);
        let mut next_slot_by_address_space: FxHashMap<
            crate::analysis::ty::ProviderAddressSpace,
            usize,
        > = FxHashMap::default();
        let mut layout = IndexMap::new();

        for (idx, field) in hir_fields
            .iter()
            .filter(|field| field.name.is_present())
            .enumerate()
        {
            let lowered_ty = lower_opt_hir_ty(db, field.type_ref(), scope, assumptions);
            let effect_handle_cx = ContractFieldEffectHandleCx {
                fallback_space: if field.is_mut {
                    crate::analysis::ty::ProviderAddressSpace::Storage
                } else {
                    crate::analysis::ty::ProviderAddressSpace::Code
                },
                ..effect_handle_cx_base
            };
            let plan = effect_handle_cx.layout_plan(db, lowered_ty);
            let slot_offset = *next_slot_by_address_space
                .entry(plan.address_space)
                .or_insert(0);
            let (mut slot_assignments, slot_count) = contract_field_layout_slot_assignments(
                db,
                plan.slot_basis_ty,
                &plan.slot_placeholders,
                &plan.materialization_placeholders,
                slot_offset,
            );
            // Placeholders indexing a different address space draw from that
            // space's counter (shared with that space's fields), so e.g. a
            // `TSlot` lock bit can never collide with `TStorPtr` state.
            for &(space, placeholder) in &plan.space_overrides {
                let TyData::ConstTy(const_ty) = placeholder.data(db) else {
                    continue;
                };
                let hole_ty = match const_ty.data(db) {
                    ConstTyData::Hole(hole_ty, _) => *hole_ty,
                    ConstTyData::TyParam(param, hole_ty) if param.is_implicit() => *hole_ty,
                    _ => continue,
                };
                let counter = next_slot_by_address_space.entry(space).or_insert(0);
                let slot = *counter;
                *counter = counter.saturating_add(1);
                slot_assignments.insert(
                    placeholder,
                    slot_const_ty(db, slot, layout_hole_fallback_ty(db, hole_ty)),
                );
            }
            let declared_ty =
                materialize_contract_layout_holes(db, plan.declared_ty, &slot_assignments);
            let target_ty =
                materialize_contract_layout_holes(db, plan.target_ty, &slot_assignments);
            let slot_basis_ty =
                materialize_contract_layout_holes(db, plan.slot_basis_ty, &slot_assignments);
            // A field with a rejected const hole (explicit `_`, or a non-slot
            // type) is reported by `ty_diags`; its hole is intentionally left
            // unnumbered (not materialized as a bogus slot), so it legitimately
            // remains here.
            debug_assert!(
                plan.explicit_const_hole
                    || plan.non_slot_const_hole
                    || (!ty_contains_const_hole(db, declared_ty)
                        && !ty_contains_const_hole(db, target_ty)
                        && !ty_contains_const_hole(db, slot_basis_ty)),
                "contract field layout materialization left unresolved holes"
            );
            *next_slot_by_address_space
                .entry(plan.address_space)
                .or_insert(0) = slot_offset.saturating_add(slot_count);

            let name = field.name.unwrap();
            layout.insert(
                name,
                ContractFieldLayoutInfo {
                    index: idx as u32,
                    name,
                    declared_ty,
                    is_mut: field.is_mut,
                    is_provider: plan.is_provider,
                    target_ty,
                    address_space: plan.address_space,
                    slot_offset,
                    slot_count,
                    static_slot_space_unresolved: plan.static_slot_space_unresolved,
                    explicit_const_hole: plan.explicit_const_hole,
                    non_slot_const_hole: plan.non_slot_const_hole,
                    handle_space_unresolved: plan.handle_space_unresolved,
                },
            );
        }

        layout
    }

    /// Total number of code-address-space slots occupied by this contract's
    /// immutable fields. The init wrapper's immutables buffer size and the
    /// end-of-code-relative offsets of code-backed field reads are both
    /// derived from this value and must agree byte-for-byte.
    pub fn code_address_space_slot_count(self, db: &'db dyn HirAnalysisDb) -> usize {
        self.field_layout(db)
            .values()
            .filter(|field| field.address_space == crate::analysis::ty::ProviderAddressSpace::Code)
            .map(|field| field.slot_offset.saturating_add(field.slot_count))
            .max()
            .unwrap_or(0)
    }

    #[salsa::tracked(return_ref)]
    pub fn fields(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> IndexMap<IdentId<'db>, ContractFieldInfo<'db>> {
        self.field_layout(db)
            .iter()
            .map(|(name, field)| {
                (
                    *name,
                    ContractFieldInfo {
                        index: field.index,
                        name: field.name,
                        declared_ty: strip_derived_adt_layout_args(db, field.declared_ty),
                        is_mut: field.is_mut,
                        is_provider: field.is_provider,
                        target_ty: strip_derived_adt_layout_args(db, field.target_ty),
                    },
                )
            })
            .collect()
    }

    #[salsa::tracked]
    pub fn init_args_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let Some(init) = self.init(db) else {
            return TyId::unit(db);
        };

        let assumptions = PredicateListId::empty_list(db);
        let param_tys: Vec<TyId<'db>> = init
            .params(db)
            .data(db)
            .iter()
            .map(|p| match p.ty.to_opt() {
                Some(hir_ty) => {
                    lower_opt_hir_ty(db, Partial::Present(hir_ty), self.scope(), assumptions)
                }
                None => TyId::invalid(db, InvalidCause::ParseError),
            })
            .collect();

        let base = TyId::new(
            db,
            TyData::TyBase(TyBase::Prim(PrimTy::Tuple(param_tys.len()))),
        );
        param_tys
            .into_iter()
            .fold(base, |acc, elem| TyId::app(db, acc, elem))
    }

    #[salsa::tracked(return_ref)]
    pub fn effect_requirements(self, db: &'db dyn HirAnalysisDb) -> Vec<EffectRequirement<'db>> {
        contract_effect_requirements_canonical(db, self)
    }

    #[salsa::tracked(return_ref)]
    pub fn init_effect_requirements(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<EffectRequirement<'db>> {
        let Some(init) = self.init(db) else {
            return Vec::new();
        };
        contract_scoped_effect_requirements_canonical(
            db,
            self,
            EffectParamSite::ContractInit { contract: self },
            init.effects(db),
        )
    }
}

#[salsa::tracked]
impl<'db> Func<'db> {
    #[salsa::tracked(return_ref)]
    pub fn effect_requirements(self, db: &'db dyn HirAnalysisDb) -> Vec<EffectRequirement<'db>> {
        func_effect_requirements_canonical(db, self)
    }

    #[salsa::tracked(return_ref)]
    pub fn effective_effect_requirements(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<EffectRequirement<'db>> {
        func_effect_requirements_canonical(db, self)
    }
}

fn contract_effect_decl_map<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> FxHashMap<IdentId<'db>, EffectRequirement<'db>> {
    contract
        .effect_requirements(db)
        .iter()
        .cloned()
        .map(|binding| (binding.binding_name, binding))
        .collect()
}

#[derive(Clone, Copy)]
enum ContractResolutionSource {
    RootProvider,
    Field(u32),
    Unresolved,
}

pub fn effect_requirements_for_site<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
) -> Vec<EffectRequirement<'db>> {
    match site {
        EffectParamSite::Func(func) => func.effective_effect_requirements(db).clone(),
        EffectParamSite::Contract(contract) => contract.effect_requirements(db).clone(),
        EffectParamSite::ContractInit { contract } => contract.init_effect_requirements(db).clone(),
        EffectParamSite::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => {
            let recv = RecvView::new(db, contract, recv_idx);
            let arm = RecvArmView::new(db, recv, arm_idx);
            arm.effective_effect_requirements(db).clone()
        }
    }
}

pub fn provider_bindings_for_site<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
) -> &'db [ProviderBinding<'db>] {
    provider_bindings_for_site_query(db, EffectEnvSite::new(db, site)).as_slice()
}

#[salsa::tracked(return_ref)]
fn provider_bindings_for_site_query<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectEnvSite<'db>,
) -> Vec<ProviderBinding<'db>> {
    let site = site.site(db);
    match site {
        EffectParamSite::Func(func) => func_provider_bindings_canonical(db, func),
        EffectParamSite::Contract(contract)
        | EffectParamSite::ContractInit { contract }
        | EffectParamSite::ContractRecvArm { contract, .. } => {
            contract_provider_bindings_canonical(db, contract, site)
        }
    }
}

pub fn effect_resolutions_for_site<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
) -> &'db [ResolvedEffectBinding] {
    effect_resolutions_for_site_query(db, EffectEnvSite::new(db, site)).as_slice()
}

#[salsa::tracked(return_ref)]
fn effect_resolutions_for_site_query<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectEnvSite<'db>,
) -> Vec<ResolvedEffectBinding> {
    let site = site.site(db);
    match site {
        EffectParamSite::Func(func) => func_effect_resolutions_canonical(db, func),
        EffectParamSite::Contract(contract) => {
            contract_effect_resolutions_canonical(db, contract, site, contract.effects(db))
        }
        EffectParamSite::ContractInit { contract } => {
            let Some(init) = contract.init(db) else {
                return Vec::new();
            };
            contract_effect_resolutions_canonical(db, contract, site, init.effects(db))
        }
        EffectParamSite::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => {
            let Some(arm) = contract.recv_arm(db, recv_idx as usize, arm_idx as usize) else {
                return Vec::new();
            };
            contract_effect_resolutions_canonical(db, contract, site, arm.effects)
        }
    }
}

#[salsa::tracked(return_ref)]
pub fn resolved_effect_binding_infos_for_site<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectEnvSite<'db>,
) -> Vec<Option<ResolvedEffectBindingInfo<'db>>> {
    let site = site.site(db);
    let requirements = effect_requirements_for_site(db, site);
    let resolutions = effect_resolutions_for_site(db, site);
    let providers = provider_bindings_for_site(db, site);
    let max_idx = requirements
        .iter()
        .map(|requirement| requirement.binding_idx as usize)
        .max()
        .map_or(0, |idx| idx + 1);
    let mut requirements_by_idx = vec![None; max_idx];
    for requirement in requirements {
        let idx = requirement.binding_idx as usize;
        requirements_by_idx[idx] = Some(requirement);
    }
    let providers_by_idx = providers
        .iter()
        .cloned()
        .map(|provider| (provider.provider_idx, provider))
        .collect::<FxHashMap<_, _>>();
    let mut resolved = vec![None; requirements_by_idx.len()];
    for resolution in resolutions {
        let idx = resolution.requirement_idx as usize;
        let Some(requirement) = requirements_by_idx
            .get(idx)
            .and_then(|requirement| requirement.clone())
        else {
            continue;
        };
        let Some(provider) = providers_by_idx.get(&resolution.provider_idx).cloned() else {
            continue;
        };
        resolved[idx] = Some(ResolvedEffectBindingInfo {
            requirement,
            provider,
        });
    }
    resolved
}

fn contract_scoped_effect_requirements_canonical<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    list_site: EffectParamSite<'db>,
    list: EffectParamListId<'db>,
) -> Vec<EffectRequirement<'db>> {
    if matches!(list_site, EffectParamSite::Func(_)) {
        unreachable!("contract-scoped effect bindings require a contract init/recv/decl site");
    }

    let fields = contract.fields(db);
    let contract_named_effects = contract_effect_decl_map(db, contract);
    let assumptions = PredicateListId::empty_list(db);

    let mut out = Vec::new();
    for (idx, effect) in list.data(db).iter().enumerate() {
        let Some(key_path) = effect.key_path.to_opt() else {
            continue;
        };

        if let Some(binding_name) = effect.name {
            let forwarded_key = if key_path.len(db) == 1 {
                key_path
                    .ident(db)
                    .to_opt()
                    .and_then(|name| contract_named_effects.get(&name))
                    .map(|binding| binding.key.clone())
            } else {
                None
            };
            let (key_kind, key_ty, key_trait) = forwarded_key.as_ref().map_or_else(
                || resolve_effect_key(db, key_path, contract.scope(), assumptions).into_parts(db),
                |key| (key.kind(), key.key_ty(), key.key_trait()),
            );
            let (key_ty, key_trait) = forwarded_key.as_ref().map_or_else(
                || canonicalize_contract_effect_key(db, list_site, idx as u32, key_ty, key_trait),
                |_| (key_ty, key_trait),
            );

            out.push(EffectRequirement {
                binding_name,
                key: EffectRequirementKey::from_parts(key_kind, key_ty, key_trait),
                is_mut: effect.is_mut,
                binding_site: list_site,
                binding_idx: idx as u32,
                binding_path: key_path,
            });
            continue;
        }

        if key_path.len(db) == 1
            && let Some(name) = key_path.ident(db).to_opt()
            && let Some(field) = fields.get(&name)
        {
            let target_ty = field.target_ty;

            out.push(EffectRequirement {
                binding_name: name,
                key: EffectRequirementKey::Type(target_ty),
                is_mut: effect.is_mut,
                binding_site: list_site,
                binding_idx: idx as u32,
                binding_path: key_path,
            });
            continue;
        }

        if key_path.len(db) == 1
            && let Some(name) = key_path.ident(db).to_opt()
            && let Some(binding) = contract_named_effects.get(&name)
        {
            out.push(EffectRequirement {
                binding_name: name,
                key: binding.key.clone(),
                is_mut: binding.is_mut,
                binding_site: list_site,
                binding_idx: idx as u32,
                binding_path: key_path,
            });
            continue;
        }

        // Unlabeled contract-scoped effects are either references (field/contract effect) or
        // invalid. Diagnostics are emitted by the type checker; keep lowering conservative.
        let binding_name = key_path
            .ident(db)
            .to_opt()
            .unwrap_or_else(|| IdentId::new(db, "_effect".to_string()));

        out.push(EffectRequirement {
            binding_name,
            key: EffectRequirementKey::Other,
            is_mut: effect.is_mut,
            binding_site: list_site,
            binding_idx: idx as u32,
            binding_path: key_path,
        });
    }

    out
}

fn contract_provider_bindings_canonical<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    site: EffectParamSite<'db>,
) -> Vec<ProviderBinding<'db>> {
    let scope = contract.scope();
    let assumptions = PredicateListId::empty_list(db);
    let is_init_site = matches!(site, EffectParamSite::ContractInit { .. });
    let mut providers = contract
        .field_layout(db)
        .values()
        .cloned()
        .enumerate()
        .map(|(idx, field)| ProviderBinding {
            provider_idx: idx as u32,
            provider_ty: field.target_ty,
            // Immutable contract fields (i.e. code-backed) are writable during `init`.
            // They are materialized into memory during initialization and later embedded into code.
            is_mut: field.is_mut
                || (is_init_site
                    && field.address_space == crate::analysis::ty::ProviderAddressSpace::Code),
            source: ProviderSource::ContractField {
                contract,
                field_idx: field.index,
            },
            semantics: ProviderSemantics {
                provider_ty: field.target_ty,
                kind: if field.target_ty.is_struct(db)
                    || field.target_ty.is_array(db)
                    || field.target_ty.is_tuple(db)
                    || field.target_ty.as_enum(db).is_some()
                {
                    crate::analysis::ty::ProviderKind::Handle
                } else {
                    crate::analysis::ty::ProviderKind::RawAddress
                },
                address_space: Some(field.address_space).map(|space| {
                    if is_init_site && space == crate::analysis::ty::ProviderAddressSpace::Code {
                        crate::analysis::ty::ProviderAddressSpace::Memory
                    } else {
                        space
                    }
                }),
                target_ty: Some(field.target_ty),
                transport: crate::analysis::ty::ProviderTransport::ByValue,
            },
        })
        .collect::<Vec<_>>();

    let base_provider_idx = providers.len();
    providers.extend(
        registered_root_providers(db, site)
            .iter()
            .cloned()
            .enumerate()
            .map(|(idx, registration)| {
                let provider_ty = registration.provider_ty;
                ProviderBinding {
                    provider_idx: (base_provider_idx + idx) as u32,
                    provider_ty,
                    is_mut: true,
                    source: ProviderSource::RootProvider { site, registration },
                    semantics: provider_semantics(db, scope, assumptions, provider_ty),
                }
            }),
    );
    providers
}

fn contract_effect_decl_resolution_sources<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> FxHashMap<IdentId<'db>, ContractResolutionSource> {
    contract
        .effect_requirements(db)
        .iter()
        .map(|requirement| {
            (
                requirement.binding_name,
                if matches!(requirement.key, EffectRequirementKey::Other) {
                    ContractResolutionSource::Unresolved
                } else {
                    ContractResolutionSource::RootProvider
                },
            )
        })
        .collect()
}

fn contract_effect_resolutions_canonical<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    site: EffectParamSite<'db>,
    list: EffectParamListId<'db>,
) -> Vec<ResolvedEffectBinding> {
    let providers = provider_bindings_for_site(db, site);
    let root_provider_idx = providers.iter().find_map(|provider| {
        matches!(provider.source, ProviderSource::RootProvider { .. })
            .then_some(provider.provider_idx)
    });
    let field_provider_idx = providers
        .iter()
        .filter_map(|provider| match provider.source {
            ProviderSource::ContractField { field_idx, .. } => {
                Some((field_idx, provider.provider_idx))
            }
            ProviderSource::UsesParam { .. } | ProviderSource::RootProvider { .. } => None,
        })
        .collect::<FxHashMap<_, _>>();
    let contract_named_sources = contract_effect_decl_resolution_sources(db, contract);
    let fields = contract.fields(db);

    effect_requirements_for_site(db, site)
        .iter()
        .filter_map(|requirement| {
            let effect = list.data(db).get(requirement.binding_idx as usize)?;
            let key_path = effect.key_path.to_opt()?;
            let source = if effect.name.is_some() {
                ContractResolutionSource::RootProvider
            } else if key_path.len(db) == 1
                && let Some(name) = key_path.ident(db).to_opt()
                && let Some(field) = fields.get(&name)
            {
                ContractResolutionSource::Field(field.index)
            } else if key_path.len(db) == 1
                && let Some(name) = key_path.ident(db).to_opt()
            {
                contract_named_sources
                    .get(&name)
                    .copied()
                    .unwrap_or(ContractResolutionSource::Unresolved)
            } else {
                ContractResolutionSource::Unresolved
            };

            let provider_idx = match source {
                ContractResolutionSource::RootProvider => root_provider_idx?,
                ContractResolutionSource::Field(field_idx) => {
                    *field_provider_idx.get(&field_idx)?
                }
                ContractResolutionSource::Unresolved => return None,
            };
            Some(ResolvedEffectBinding {
                requirement_idx: requirement.binding_idx,
                provider_idx,
            })
        })
        .collect()
}

fn compute_arg_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    variant_struct: Struct<'db>,
    args_ty: TyId<'db>,
    arm_pat: PatId,
    arm_body: Body<'db>,
) -> Option<Vec<ArgBinding<'db>>> {
    let Partial::Present(pat) = arm_pat.data(db, arm_body) else {
        return None;
    };

    let elem_tys = args_ty.field_types(db);

    let bindings = match pat {
        Pat::Record(_, fields) => {
            let mut out = Vec::new();
            for f in fields {
                let Some(label) = f.label(db, arm_body) else {
                    continue;
                };
                let Some(field_idx) = variant_struct.hir_fields(db).field_idx(db, label) else {
                    continue;
                };
                let Some(elem_ty) = elem_tys.get(field_idx).copied() else {
                    continue;
                };
                out.push(ArgBinding {
                    pat: f.pat,
                    tuple_index: field_idx as u32,
                    ty: elem_ty,
                });
            }
            out
        }
        Pat::PathTuple(_, pats) | Pat::Tuple(pats) => pats
            .iter()
            .enumerate()
            .filter_map(|(idx, pat)| {
                let ty = elem_tys.get(idx).copied()?;
                Some(ArgBinding {
                    pat: *pat,
                    tuple_index: idx as u32,
                    ty,
                })
            })
            .collect(),
        _ => Vec::new(),
    };

    Some(bindings)
}

fn resolve_msg_mod<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    msg_path: PathId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<Mod<'db>> {
    use crate::analysis::name_resolution::{PathRes, resolve_path};
    use crate::hir_def::ItemKind;

    if let PathRes::Mod(ScopeId::Item(ItemKind::Mod(m))) =
        resolve_path(db, msg_path, contract.scope(), assumptions, false).ok()?
    {
        Some(m)
    } else {
        None
    }
}

fn resolve_recv_variant<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    msg_mod: Option<Mod<'db>>,
    recv_is_bare: bool,
    arm: ContractRecvArm<'db>,
    assumptions: PredicateListId<'db>,
) -> (Option<Struct<'db>>, Option<TyId<'db>>) {
    let Some(variant_path) = arm.variant_path(db) else {
        return (None, None);
    };

    if let Some(msg_mod) = msg_mod {
        match crate::analysis::ty::ty_check::resolve_variant_in_msg(
            db,
            msg_mod,
            variant_path,
            assumptions,
        ) {
            Ok(resolved) => (Some(resolved.variant_struct), Some(resolved.ty)),
            _ => (None, None),
        }
    } else if recv_is_bare {
        match crate::analysis::ty::ty_check::resolve_variant_bare(
            db,
            contract,
            variant_path,
            assumptions,
        ) {
            Ok(resolved) => (Some(resolved.variant_struct), Some(resolved.ty)),
            _ => (None, None),
        }
    } else {
        (None, None)
    }
}

fn resolve_sol_abi_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<TyId<'db>> {
    use crate::analysis::name_resolution::{PathRes, resolve_path};
    use common::ingot::IngotKind;

    let ingot = scope.ingot(db);
    let std_root = if ingot.kind(db) == IngotKind::Std {
        IdentId::make_ingot(db)
    } else {
        IdentId::new(db, "std".to_string())
    };

    let sol_path = PathId::from_ident(db, std_root)
        .push_ident(db, IdentId::new(db, "abi".to_string()))
        .push_ident(db, IdentId::new(db, "Sol".to_string()));

    match resolve_path(db, sol_path, scope, assumptions, false).ok()? {
        PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => Some(ty),
        _ => None,
    }
}

fn get_variant_selector_info<'db>(
    db: &'db dyn HirAnalysisDb,
    variant_ty: TyId<'db>,
    scope: ScopeId<'db>,
) -> VariantSelectorInfo {
    use crate::analysis::semantic::{SemConstScalar, SemConstValue, eval_body_owner_const};
    use crate::analysis::ty::{
        canonical::Canonical,
        corelib::resolve_core_trait,
        trait_def::impls_for_ty,
        ty_def::{PrimTy, TyBase, TyData},
    };
    use num_traits::ToPrimitive;

    let Some(msg_variant_trait) = resolve_core_trait(db, scope, &["message", "MsgVariant"]) else {
        return VariantSelectorInfo {
            value: None,
            signature: None,
        };
    };
    let canonical_ty = Canonical::new(db, variant_ty);
    let scope_ingot = scope.ingot(db);
    let search_ingots = [
        Some(scope_ingot),
        variant_ty.ingot(db).filter(|&ingot| ingot != scope_ingot),
    ];

    let Some(implementor) = search_ingots.into_iter().flatten().find_map(|ingot| {
        impls_for_ty(db, ingot, canonical_ty)
            .iter()
            .find(|impl_| impl_.skip_binder().trait_def(db).eq(&msg_variant_trait))
            .copied()
    }) else {
        return VariantSelectorInfo {
            value: None,
            signature: None,
        };
    };
    let impl_ = implementor.skip_binder();

    let selector_name = IdentId::new(db, "SELECTOR".to_string());
    let hir_impl = impl_.hir_impl_trait(db);
    let Some(selector_const) = hir_impl
        .hir_consts(db)
        .iter()
        .find(|c| c.name.to_opt() == Some(selector_name))
    else {
        return VariantSelectorInfo {
            value: None,
            signature: None,
        };
    };

    let Some(body) = selector_const.value.to_opt() else {
        return VariantSelectorInfo {
            value: None,
            signature: None,
        };
    };
    let signature = selector_signature_from_body(db, body, hir_impl.scope());
    let expected_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U32)));
    let value = match eval_body_owner_const(
        db,
        crate::analysis::ty::ty_check::BodyOwner::AnonConstBody {
            body,
            expected: expected_ty,
        },
        Vec::new(),
    ) {
        Ok(value) => match value.value(db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } => value.to_u32(),
            _ => None,
        },
        Err(_) => None,
    };

    VariantSelectorInfo { value, signature }
}

fn selector_signature_from_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    scope: ScopeId<'db>,
) -> Option<String> {
    use crate::analysis::ty::ty_def::{PrimTy, TyBase, TyData};

    let expected_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U32)));
    let (_, typed_body) =
        crate::analysis::ty::ty_check::check_anon_const_body(db, body, expected_ty);
    let mut visited = SelectorSearchVisited::new();

    selector_signature_from_typed_body(db, body, scope, typed_body, &mut visited)
}

/// Tracks already-visited items to prevent cycles when following const/fn references.
struct SelectorSearchVisited<'db> {
    consts: FxHashSet<Const<'db>>,
    inherent_consts: FxHashSet<Body<'db>>,
    funcs: FxHashSet<Func<'db>>,
    locals: FxHashSet<PatId>,
}

impl<'db> SelectorSearchVisited<'db> {
    fn new() -> Self {
        Self {
            consts: FxHashSet::default(),
            inherent_consts: FxHashSet::default(),
            funcs: FxHashSet::default(),
            locals: FxHashSet::default(),
        }
    }
}

fn selector_signature_from_typed_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    scope: ScopeId<'db>,
    typed_body: &crate::analysis::ty::ty_check::TypedBody<'db>,
    visited: &mut SelectorSearchVisited<'db>,
) -> Option<String> {
    let expr_id = body.expr(db);
    selector_signature_from_expr(db, body, scope, typed_body, visited, expr_id)
}

fn selector_signature_from_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    scope: ScopeId<'db>,
    typed_body: &crate::analysis::ty::ty_check::TypedBody<'db>,
    visited: &mut SelectorSearchVisited<'db>,
    expr_id: ExprId,
) -> Option<String> {
    match expr_id.data(db, body) {
        Partial::Present(Expr::Call(_callee, args)) => {
            if args.len() == 1
                && args[0].label.is_none()
                && let Partial::Present(Expr::Lit(LitKind::String(signature))) =
                    args[0].expr.data(db, body)
            {
                let resolved_sol = resolve_lib_func_path(db, scope, "std::abi::sol")?;
                if let crate::hir_def::CallableDef::Func(func) =
                    typed_body.callable_expr(expr_id)?.callable_def
                    && func == resolved_sol
                {
                    return Some(signature.data(db).to_string());
                }
            }

            // Follow const fn calls
            if let Some(callable) = typed_body.callable_expr(expr_id)
                && let crate::hir_def::CallableDef::Func(func) = callable.callable_def
                && func.is_const(db)
                && visited.funcs.insert(func)
            {
                let (_, func_typed_body) = crate::analysis::ty::ty_check::check_func_body(db, func);
                if let Some(func_body) = func_typed_body.body() {
                    return selector_signature_from_typed_body(
                        db,
                        func_body,
                        func.scope(),
                        func_typed_body,
                        visited,
                    );
                }
            }
        }
        Partial::Present(Expr::Block(stmts)) => {
            if let Some(last_stmt) = stmts.last()
                && let Partial::Present(Stmt::Expr(inner_expr)) = last_stmt.data(db, body)
            {
                return selector_signature_from_expr(
                    db,
                    body,
                    scope,
                    typed_body,
                    visited,
                    *inner_expr,
                );
            }
        }
        _ => {}
    }

    if let Some(signature) =
        selector_signature_from_local_binding(db, body, scope, typed_body, visited, expr_id)
    {
        return Some(signature);
    }

    match typed_body.expr_const_ref(expr_id)? {
        crate::analysis::ty::ty_check::ConstRef::Const(const_) => {
            if !visited.consts.insert(const_) {
                return None;
            }
            let (_, const_typed_body) = crate::analysis::ty::ty_check::check_const_body(db, const_);
            let const_body = const_typed_body.body()?;
            selector_signature_from_typed_body(
                db,
                const_body,
                const_.scope(),
                const_typed_body,
                visited,
            )
        }
        crate::analysis::ty::ty_check::ConstRef::InherentConst(use_) => {
            // Follow the inherent const's value body so a selector that
            // delegates to `Selectors::TRANSFER` resolves the underlying
            // `sol("...")` signature, mirroring the top-level `Const` branch.
            let (const_body, _) = crate::analysis::ty::const_ty::inherent_const_body_and_impl_args(
                db,
                use_.impl_(),
                use_.receiver_ty(),
                use_.name(),
            )?;
            if !visited.inherent_consts.insert(const_body) {
                return None;
            }
            let expected = crate::analysis::ty::const_ty::inherent_const_expected_ty(
                db,
                use_.impl_(),
                use_.receiver_ty(),
                use_.name(),
            )?;
            let (_, const_typed_body) =
                crate::analysis::ty::ty_check::check_anon_const_body(db, const_body, expected);
            selector_signature_from_typed_body(
                db,
                const_body,
                use_.impl_().scope(),
                const_typed_body,
                visited,
            )
        }
        crate::analysis::ty::ty_check::ConstRef::TraitConst { .. } => None,
    }
}

fn selector_signature_from_local_binding<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    scope: ScopeId<'db>,
    typed_body: &crate::analysis::ty::ty_check::TypedBody<'db>,
    visited: &mut SelectorSearchVisited<'db>,
    expr_id: ExprId,
) -> Option<String> {
    let crate::analysis::ty::ty_check::LocalBinding::Local { pat, .. } =
        typed_body.expr_binding(expr_id)?
    else {
        return None;
    };

    if !visited.locals.insert(pat) {
        return None;
    }

    let init_expr = selector_local_binding_init_expr(db, body, pat)?;
    selector_signature_from_expr(db, body, scope, typed_body, visited, init_expr)
}

fn selector_local_binding_init_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    pat: PatId,
) -> Option<ExprId> {
    for (_, stmt) in body.stmts(db).iter() {
        if let Partial::Present(Stmt::Let(stmt_pat, _, Some(init_expr))) = stmt
            && *stmt_pat == pat
        {
            return Some(*init_expr);
        }
    }

    None
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum EffectParamOwner<'db> {
    Func(Func<'db>),
    Contract(Contract<'db>),
    RecvArm(RecvArmView<'db>),
}
impl<'db> EffectParamOwner<'db> {
    pub fn scope(self, db: &'db dyn HirDb) -> ScopeId<'db> {
        match self {
            EffectParamOwner::Func(func) => func.scope(),
            EffectParamOwner::Contract(contract) => contract.scope(),
            EffectParamOwner::RecvArm(arm) => arm.contract(db).scope(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct EffectParamView<'db> {
    pub owner: EffectParamOwner<'db>,
    idx: usize,
}

impl<'db> EffectParamView<'db> {
    fn effect(self, db: &'db dyn HirDb) -> &'db crate::core::hir_def::EffectParam<'db> {
        match self.owner {
            EffectParamOwner::Func(func) => &func.effects(db).data(db)[self.idx],
            EffectParamOwner::Contract(contract) => &contract.effects(db).data(db)[self.idx],
            EffectParamOwner::RecvArm(arm) => &arm.effects(db).data(db)[self.idx],
        }
    }

    /// Optional name for this effect parameter.
    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.effect(db).name
    }

    /// The path identifying the effect key (trait or type).
    pub fn key_path(self, db: &'db dyn HirDb) -> Option<PathId<'db>> {
        self.effect(db)
            .key_path
            .to_opt()
            .filter(|path| path.ident(db).is_present())
    }

    /// Whether this effect requires mutation.
    pub fn is_mut(self, db: &'db dyn HirDb) -> bool {
        self.effect(db).is_mut
    }

    /// Index of this effect in the function's effect list.
    pub fn index(self) -> usize {
        self.idx
    }
}

impl<'db> Func<'db> {
    /// Iterate parameters as contextual views (semantic traversal helper).
    pub fn params(self, db: &'db dyn HirDb) -> impl Iterator<Item = FuncParamView<'db>> + 'db {
        let len = self
            .params_list(db)
            .to_opt()
            .map(|l| l.data(db).len())
            .unwrap_or(0);
        (0..len).map(move |idx| FuncParamView { func: self, idx })
    }

    /// Iterate effect parameters as contextual views.
    pub fn effect_params(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = EffectParamView<'db>> + 'db {
        let len = self.effects(db).data(db).len();
        let owner = EffectParamOwner::Func(self);
        (0..len).map(move |idx| EffectParamView { owner, idx })
    }

    /// Returns true if this function has any effect parameters.
    pub fn has_effects(self, db: &'db dyn HirDb) -> bool {
        !self.effects(db).data(db).is_empty()
    }
}

/// Helper to check if a type's base matches a given ADT.
fn matches_adt<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, adt: AdtDef<'db>) -> bool {
    match ty.base_ty(db).data(db) {
        TyData::TyBase(TyBase::Adt(ty_adt)) => *ty_adt == adt,
        _ => false,
    }
}

impl<'db> Enum<'db> {
    pub fn len_variants(&self, db: &'db dyn HirDb) -> usize {
        self.variants_list(db).data(db).len()
    }

    /// Iterates variants as contextual views (structural traversal helper).
    pub fn variants(self, db: &'db dyn HirDb) -> impl Iterator<Item = VariantView<'db>> + 'db {
        let list = self.variants_list(db);
        list.data(db)
            .iter()
            .enumerate()
            .map(move |(idx, _)| VariantView { owner: self, idx })
    }

    /// Semantic ADT definition for this enum (cached via tracked query).
    pub fn as_adt(self, db: &'db dyn HirAnalysisDb) -> AdtDef<'db> {
        lower_adt(db, AdtRef::from(self))
    }

    /// Returns all inherent `impl` blocks for this enum within the same ingot.
    pub fn all_impls(self, db: &'db dyn HirAnalysisDb) -> Vec<Impl<'db>> {
        let adt = self.as_adt(db);
        self.top_mod(db)
            .ingot(db)
            .all_impls(db)
            .iter()
            .copied()
            .filter(|impl_| matches_adt(db, impl_.ty(db), adt))
            .collect()
    }

    /// Returns all `impl Trait for Enum` blocks for this enum within the same ingot.
    pub fn all_impl_traits(self, db: &'db dyn HirAnalysisDb) -> Vec<ImplTrait<'db>> {
        let adt = self.as_adt(db);
        self.top_mod(db)
            .ingot(db)
            .all_impl_traits(db)
            .iter()
            .copied()
            .filter(|impl_trait| matches_adt(db, impl_trait.ty(db), adt))
            .collect()
    }
}

impl<'db> Struct<'db> {
    /// Returns semantic types of all fields, bound to identity parameters.
    pub fn field_tys(self, db: &'db dyn HirAnalysisDb) -> Vec<Binder<TyId<'db>>> {
        use crate::analysis::ty::ty_def::{InvalidCause, TyId};
        use crate::analysis::ty::ty_lower::lower_hir_ty;

        let scope = self.scope();
        let assumptions =
            collect_constraints(db, GenericParamOwner::Struct(self)).instantiate_identity();
        let fields = self.fields(db);

        fields
            .data(db)
            .iter()
            .map(|field| {
                let ty = match field.type_ref.to_opt() {
                    Some(hir_ty) => lower_hir_ty(db, hir_ty, scope, assumptions),
                    None => TyId::invalid(db, InvalidCause::ParseError),
                };
                Binder::bind(ty)
            })
            .collect()
    }

    /// Semantic ADT definition for this struct (cached via tracked query).
    pub fn as_adt(self, db: &'db dyn HirAnalysisDb) -> AdtDef<'db> {
        lower_adt(db, AdtRef::from(self))
    }

    /// Returns all inherent `impl` blocks for this struct within the same ingot.
    pub fn all_impls(self, db: &'db dyn HirAnalysisDb) -> Vec<Impl<'db>> {
        let adt = self.as_adt(db);
        self.top_mod(db)
            .ingot(db)
            .all_impls(db)
            .iter()
            .copied()
            .filter(|impl_| matches_adt(db, impl_.ty(db), adt))
            .collect()
    }

    /// Returns all `impl Trait for Struct` blocks for this struct within the same ingot.
    pub fn all_impl_traits(self, db: &'db dyn HirAnalysisDb) -> Vec<ImplTrait<'db>> {
        let adt = self.as_adt(db);
        self.top_mod(db)
            .ingot(db)
            .all_impl_traits(db)
            .iter()
            .copied()
            .filter(|impl_trait| matches_adt(db, impl_trait.ty(db), adt))
            .collect()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct WhereClauseView<'db> {
    pub owner: WhereClauseOwner<'db>,
    pub id: WhereClauseId<'db>,
}

#[derive(Clone, Copy, Debug)]
pub struct WherePredicateView<'db> {
    pub clause: WhereClauseView<'db>,
    pub idx: usize,
}

impl<'db> WhereClauseOwner<'db> {
    /// Semantic where-clause view for this owner.
    pub fn clause(self, db: &'db dyn HirDb) -> WhereClauseView<'db> {
        WhereClauseView {
            owner: self,
            id: self.where_clause(db),
        }
    }
}

impl<'db> WhereClauseView<'db> {
    pub fn predicates(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = WherePredicateView<'db>> + 'db {
        let len = self.id.data(db).len();
        (0..len).map(move |idx| WherePredicateView { clause: self, idx })
    }

    pub fn span(self) -> crate::span::params::LazyWhereClauseSpan<'db> {
        match self.owner {
            WhereClauseOwner::Func(f) => f.span().where_clause(),
            WhereClauseOwner::Struct(s) => s.span().where_clause(),
            WhereClauseOwner::Enum(e) => e.span().where_clause(),
            WhereClauseOwner::Impl(i) => i.span().where_clause(),
            WhereClauseOwner::Trait(t) => t.span().where_clause(),
            WhereClauseOwner::ImplTrait(i) => i.span().where_clause(),
        }
    }
}

impl<'db> WherePredicateView<'db> {
    pub(in crate::core) fn hir_pred(self, db: &'db dyn HirDb) -> &'db WherePredicate<'db> {
        &self.clause.id.data(db)[self.idx]
    }

    fn owner_item(self) -> ItemKind<'db> {
        ItemKind::from(self.clause.owner)
    }

    /// If this predicate's subject is one of the owner's generic parameters,
    /// returns its original index (0-based within the owner).
    pub fn param_original_index(self, db: &'db dyn HirDb) -> Option<usize> {
        use crate::core::hir_def::types::TypeKind as HirTyKind;
        let hir_ty = self.hir_pred(db).ty.to_opt()?;
        let path = match hir_ty.data(db) {
            HirTyKind::Path(p) => p.to_opt()?,
            _ => return None,
        };

        let ident = path.as_ident(db)?;
        let owner = GenericParamOwner::from_item_opt(self.owner_item())?;
        let params = owner.params_list(db).data(db);
        params.iter().position(|p| match p {
            GenericParam::Type(t) => t.name.to_opt() == Some(ident),
            GenericParam::Const(c) => c.name.to_opt() == Some(ident),
        })
    }

    /// Lowered kind bound sourced from the where-clause, if present.
    pub fn kind(self, db: &'db dyn HirAnalysisDb) -> Option<Kind> {
        use crate::hir_def::Partial;
        for b in &self.hir_pred(db).bounds {
            if let TypeBound::Kind(Partial::Present(kb)) = b {
                return Some(lower_hir_kind_local(kb));
            }
        }
        None
    }

    pub fn span(self) -> crate::span::params::LazyWherePredicateSpan<'db> {
        self.clause.span().predicate(self.idx)
    }

    /// Iterate trait bounds as per-bound semantic views.
    pub fn bounds(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = WherePredicateBoundView<'db>> + 'db {
        let idxs: Vec<usize> = self
            .hir_pred(db)
            .bounds
            .iter()
            .enumerate()
            .filter_map(|(i, b)| matches!(b, TypeBound::Trait(_)).then_some(i))
            .collect();
        idxs.into_iter()
            .map(move |idx| WherePredicateBoundView { pred: self, idx })
    }

    /// True if this predicate's subject type is `Self` (within a trait).
    pub fn is_self_subject(self, db: &'db dyn HirDb) -> bool {
        self.hir_pred(db)
            .ty
            .to_opt()
            .map(|ty| ty.is_self_ty(db))
            .unwrap_or_default()
    }

    /// Lower the subject type of this where-predicate into a semantic `TyId`.
    /// Returns `None` if the HIR type is missing or invalid.
    pub fn subject_ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let hir_ty = self.hir_pred(db).ty.to_opt()?;
        let owner_item = ItemKind::from(self.clause.owner);
        let assumptions = header_constraints_for(db, owner_item);
        Some(lower_hir_ty(db, hir_ty, owner_item.scope(), assumptions))
    }

    /// True if the lowered subject type is a const type.
    pub fn subject_is_const(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.subject_ty(db)
            .map(|t| t.is_const_ty(db))
            .unwrap_or(false)
    }

    /// True if the lowered subject type is concrete (no generic params) and not invalid.
    pub fn subject_is_concrete(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.subject_ty(db)
            .map(|t| !t.has_invalid(db) && !t.has_param(db))
            .unwrap_or(false)
    }

    /// Diagnostics for all bounds (trait and kind) of this predicate.
    pub fn bound_diags(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: TyId<'db>,
    ) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();
        let hir = &self.clause.id.data(db)[self.idx];

        for (i, bound) in hir.bounds.iter().enumerate() {
            match bound {
                TypeBound::Trait(_) => {
                    let bview = WherePredicateBoundView::new(self, i);
                    out.extend(bview.diags(db));
                }
                TypeBound::Kind(crate::hir_def::Partial::Present(kb)) => {
                    let expected = lower_hir_kind_local(kb);
                    let actual = subject.kind(db);
                    if !actual.does_match(&expected) {
                        let span = self.span().bounds().bound(i).kind_bound();
                        out.push(
                            TyLowerDiag::InconsistentKindBound {
                                span: span.into(),
                                ty: subject,
                                bound: expected,
                            }
                            .into(),
                        );
                    }
                }
                _ => {}
            }
        }
        out
    }
}

#[derive(Clone, Copy, Debug)]
pub struct WherePredicateBoundView<'db> {
    pub pred: WherePredicateView<'db>,
    pub idx: usize,
}

impl<'db> WherePredicateBoundView<'db> {
    pub fn new(pred: WherePredicateView<'db>, idx: usize) -> Self {
        Self { pred, idx }
    }
    pub fn trait_ref(self, db: &'db dyn HirDb) -> TraitRefId<'db> {
        match &self.pred.hir_pred(db).bounds[self.idx] {
            TypeBound::Trait(tr) => *tr,
            _ => unreachable!(),
        }
    }

    pub fn span(self) -> crate::span::params::LazyTypeBoundSpan<'db> {
        self.pred.span().bounds().bound(self.idx)
    }

    pub fn trait_ref_span(self) -> crate::span::params::LazyTraitRefSpan<'db> {
        self.span().trait_bound()
    }

    /// Lower this bound into a semantic trait instance using the predicate's subject type.
    pub(in crate::core) fn as_trait_inst(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<TraitInstId<'db>> {
        let subject = self.pred.subject_ty(db)?;
        let owner_item = ItemKind::from(self.pred.clause.owner);
        let assumptions = header_constraints_for(db, owner_item);
        let scope = owner_item.scope();
        lower_trait_ref(
            db,
            subject,
            self.trait_ref(db),
            scope,
            assumptions,
            crate::analysis::ty::trait_resolution::constraint::enclosing_trait_self_ty(db, scope),
        )
        .ok()
    }
}

impl<'db> TypeAlias<'db> {
    /// Semantic alias target type (convenience over lower_type_alias).
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let ta = lower_type_alias(db, self);
        *ta.alias_to.skip_binder()
    }

    /// Type lowering errors for the alias target type.
    pub fn ty_errors(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let Some(hir_ty) = self.type_ref(db).to_opt() else {
            return Vec::new();
        };
        let assumptions = constraints_for(db, self.into());
        let ty = lower_hir_ty(db, hir_ty, self.scope(), assumptions);
        if ty.has_invalid(db) {
            collect_ty_lower_errors(db, self.scope(), hir_ty, self.span().ty(), assumptions)
        } else {
            Vec::new()
        }
    }

    /// Well-formedness errors for the alias target type.
    pub fn ty_wf_errors(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let Some(hir_ty) = self.type_ref(db).to_opt() else {
            return Vec::new();
        };
        let assumptions = constraints_for(db, self.into());
        let ty = lower_hir_ty(db, hir_ty, self.scope(), assumptions);
        if let WellFormedness::IllFormed { goal, subgoal } = check_ty_wf(
            db,
            TraitSolveCx::new(db, self.scope()).with_assumptions(param_env(db, self.into())),
            ty,
        ) {
            vec![
                TraitConstraintDiag::TraitBoundNotSat {
                    span: self.span().ty().into(),
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                    required_by: None,
                }
                .into(),
            ]
        } else {
            Vec::new()
        }
    }
}

// Trait / Impl items --------------------------------------------------------

impl<'db> Trait<'db> {
    pub fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        collect_generic_params(db, self.into()).params(db)
    }

    pub fn param_set(self, db: &'db dyn HirAnalysisDb) -> GenericParamTypeSet<'db> {
        collect_generic_params(db, self.into())
    }

    pub fn self_param(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        collect_generic_params(db, self.into())
            .trait_self(db)
            .unwrap()
    }

    pub fn original_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        collect_generic_params(db, self.into()).explicit_params(db)
    }

    pub fn method_defs(self, db: &'db dyn HirAnalysisDb) -> IndexMap<IdentId<'db>, Func<'db>> {
        let mut methods = IndexMap::default();
        for method in self.methods(db) {
            if let Some(name) = method.name(db).to_opt() {
                methods.insert(name, method);
            }
        }
        methods
    }

    pub fn ingot(self, db: &'db dyn HirDb) -> common::ingot::Ingot<'db> {
        self.top_mod(db).ingot(db)
    }

    /// Iterate associated types as contextual views.
    pub fn assoc_types(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = TraitAssocTypeView<'db>> + 'db {
        let len = self.types(db).len();
        (0..len).map(move |idx| TraitAssocTypeView { owner: self, idx })
    }

    /// Iterate associated consts as contextual views.
    pub fn assoc_consts(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = TraitAssocConstView<'db>> + 'db {
        let len = self.consts(db).len();
        (0..len).map(move |idx| TraitAssocConstView { owner: self, idx })
    }

    /// Get an associated const view by name, if it exists.
    pub fn const_(
        self,
        db: &'db dyn HirDb,
        name: IdentId<'db>,
    ) -> Option<TraitAssocConstView<'db>> {
        self.assoc_consts(db).find(|c| c.name(db) == Some(name))
    }

    /// Get the index of an associated const by name, if it exists.
    pub fn const_index(self, db: &'db dyn HirDb, name: IdentId<'db>) -> Option<usize> {
        self.assoc_consts(db)
            .enumerate()
            .find_map(|(idx, c)| (c.name(db) == Some(name)).then_some(idx))
    }

    /// Get an associated const view by its index.
    pub fn const_by_index(self, idx: usize) -> TraitAssocConstView<'db> {
        TraitAssocConstView { owner: self, idx }
    }

    /// Iterate declared super-trait references as contextual views.
    pub fn super_trait_refs(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = SuperTraitRefView<'db>> + 'db {
        let len = self.super_traits_refs(db).len();
        (0..len).map(move |idx| SuperTraitRefView { owner: self, idx })
    }

    /// Semantic super-trait bounds using the trait's own `Self` as subject.
    pub fn super_trait_bounds(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = TraitInstId<'db>> + 'db {
        let self_param = self.self_param(db);
        let scope = self.scope();
        let assumptions = collect_constraints(db, self.into()).instantiate_identity();

        let mut super_traits = IndexSet::new();
        for view in self.super_trait_refs(db) {
            let super_ref = view.trait_ref(db);
            if let Ok(inst) = lower_trait_ref(db, self_param, super_ref, scope, assumptions, None) {
                super_traits.insert(inst);
            }
        }

        for pred in WhereClauseOwner::Trait(self).clause(db).predicates(db) {
            if !pred.is_self_subject(db) {
                continue;
            }
            for bound in pred.bounds(db) {
                if let Some(inst) = bound.as_trait_inst(db) {
                    super_traits.insert(inst);
                }
            }
        }

        super_traits.into_iter()
    }

    pub(crate) fn super_traits(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> IndexSet<Binder<TraitInstId<'db>>> {
        self.super_trait_bounds(db).map(Binder::bind).collect()
    }

    /// Returns all `impl Trait for Type` blocks that implement this trait
    /// within the same ingot.
    pub fn all_impl_traits(self, db: &'db dyn HirAnalysisDb) -> Vec<ImplTrait<'db>> {
        self.ingot(db)
            .all_impl_traits(db)
            .iter()
            .copied()
            .filter(|impl_trait| impl_trait.trait_def(db) == Some(self))
            .collect()
    }

    /// Returns all implementations of a specific method from this trait.
    ///
    /// Given a method name, finds all `impl Trait for Type` blocks that implement
    /// this trait and returns the corresponding method implementations.
    pub fn method_implementations(
        self,
        db: &'db dyn HirAnalysisDb,
        method_name: IdentId<'db>,
    ) -> Vec<Func<'db>> {
        self.all_impl_traits(db)
            .into_iter()
            .filter_map(|impl_trait| {
                impl_trait
                    .methods(db)
                    .find(|m| m.name(db).to_opt() == Some(method_name))
            })
            .collect()
    }

    /// Returns the method definition in this trait with the given name.
    pub fn method(self, db: &'db dyn HirDb, name: IdentId<'db>) -> Option<Func<'db>> {
        self.methods(db).find(|m| m.name(db).to_opt() == Some(name))
    }
}

// ADT recursion (semantic) --------------------------------------------------

impl<'db> AdtDef<'db> {
    /// Detects a recursive ADT cycle that is not guarded by an indirect wrapper
    /// (e.g., pointer/reference). Returns the cycle members if the ADT is part
    /// of a cycle; otherwise returns None.
    pub fn recursive_cycle(self, db: &'db dyn HirAnalysisDb) -> Option<Vec<AdtCycleMember<'db>>> {
        fn impl_check<'db>(
            db: &'db dyn HirAnalysisDb,
            adt: AdtDef<'db>,
            chain: &[AdtCycleMember<'db>],
        ) -> Option<Vec<AdtCycleMember<'db>>> {
            if chain.iter().any(|m| m.adt == adt) {
                return Some(chain.to_vec());
            } else if adt.fields(db).is_empty() {
                return None;
            }

            let mut chain = chain.to_vec();
            for (field_idx, field) in adt.fields(db).iter().enumerate() {
                for (ty_idx, ty) in field.iter_types(db).enumerate() {
                    for field_adt_ref in collect_direct_adts(db, ty.instantiate_identity()) {
                        chain.push(AdtCycleMember {
                            adt,
                            field_idx: field_idx as u16,
                            ty_idx: ty_idx as u16,
                        });

                        if let Some(cycle) = impl_check(db, lower_adt(db, field_adt_ref), &chain)
                            && cycle.iter().any(|m| m.adt == adt)
                        {
                            return Some(cycle);
                        }
                        chain.pop();
                    }
                }
            }
            None
        }

        impl_check(db, self, &[])
    }
}

/// Collect all ADTs directly appearing inside the given type without
/// traversing through indirect wrappers like pointers or references.
fn collect_direct_adts<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> rustc_hash::FxHashSet<AdtRef<'db>> {
    use crate::analysis::ty::ty_def::{PrimTy, TyBase};
    use crate::analysis::ty::visitor::TyVisitable;
    use rustc_hash::FxHashSet;

    struct AdtCollector<'db> {
        db: &'db dyn HirAnalysisDb,
        adts: FxHashSet<AdtRef<'db>>,
    }
    impl<'db> TyVisitor<'db> for AdtCollector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }
        fn visit_app(&mut self, abs: TyId<'db>, arg: TyId<'db>) {
            let is_indirect = match abs.data(self.db) {
                TyData::TyBase(TyBase::Prim(PrimTy::Ptr)) => true,
                // Future: handle Ref when introduced.
                _ => false,
            };
            if !is_indirect {
                walk_ty(self, arg)
            }
        }
        fn visit_adt(&mut self, adt: AdtDef<'db>) {
            self.adts.insert(adt.adt_ref(self.db));
        }
    }

    let mut collector = AdtCollector {
        db,
        adts: FxHashSet::default(),
    };
    ty.visit_with(&mut collector);
    collector.adts
}

#[derive(Clone, Copy, Debug)]
pub struct SuperTraitRefView<'db> {
    pub owner: Trait<'db>,
    pub idx: usize,
}

impl<'db> SuperTraitRefView<'db> {
    pub fn span(self) -> crate::span::params::LazyTraitRefSpan<'db> {
        self.owner.span().super_traits().super_trait(self.idx)
    }

    pub(crate) fn trait_ref(self, db: &'db dyn HirDb) -> TraitRefId<'db> {
        self.owner.super_traits_refs(db)[self.idx]
    }

    pub fn subject_self(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        collect_generic_params(db, self.owner.into())
            .trait_self(db)
            .unwrap()
    }

    pub fn assumptions(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        header_constraints_for(db, self.owner.into())
    }

    /// Lower this super-trait reference to a semantic trait instance using the trait's
    /// `Self` and constraints.
    /// Semantic trait instance for this super-trait reference lowered in the owner's
    /// context. Returns an error value; does not emit diagnostics.
    pub fn trait_inst(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<TraitInstId<'db>, SuperTraitLowerError> {
        use crate::analysis::ty::trait_lower::TraitRefLowerError;
        let subject = self.subject_self(db);
        let tr = self.trait_ref(db);
        let scope = self.owner.scope();
        let assumptions = self.assumptions(db);
        match crate::analysis::ty::trait_lower::lower_trait_ref(
            db,
            subject,
            tr,
            scope,
            assumptions,
            None,
        ) {
            Ok(v) => Ok(v),
            Err(TraitRefLowerError::PathResError(_)) => Err(SuperTraitLowerError::PathResolution),
            Err(TraitRefLowerError::InvalidDomain(_)) => Err(SuperTraitLowerError::InvalidDomain),
            Err(TraitRefLowerError::Cycle) => Err(SuperTraitLowerError::Cycle),
            Err(TraitRefLowerError::UnsafeLocalBoundBlanketImpl | TraitRefLowerError::Ignored) => {
                Err(SuperTraitLowerError::Ignored)
            }
        }
    }

    /// Returns a tuple of (expected_kind, actual_self) when the owner's `Self` kind
    /// does not match the super-trait's expected implementor kind. Returns None when
    /// kinds are compatible or `Self` is invalid.
    pub fn kind_mismatch_for_self(self, db: &'db dyn HirAnalysisDb) -> Option<(Kind, TyId<'db>)> {
        use crate::analysis::ty::trait_lower::{TraitRefLowerError, lower_trait_ref};
        let subject = self.subject_self(db);
        let scope = self.owner.scope();
        let assumptions = self.assumptions(db);
        let tr = self.trait_ref(db);
        let expected = match lower_trait_ref(db, subject, tr, scope, assumptions, None) {
            Ok(inst) => inst.def(db).self_param(db).kind(db).clone(),
            // If we cannot lower, defer to other diagnostics; do not emit mismatch here.
            Err(
                TraitRefLowerError::PathResError(_)
                | TraitRefLowerError::InvalidDomain(_)
                | TraitRefLowerError::Cycle
                | TraitRefLowerError::UnsafeLocalBoundBlanketImpl
                | TraitRefLowerError::Ignored,
            ) => return None,
        };
        let actual = subject;
        (!expected.does_match(actual.kind(db))).then_some((expected, actual))
    }

    // Note: callers that want an Option can map the error to None explicitly.
}

/// Semantic error for lowering a super-trait reference in its owner's context.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SuperTraitLowerError {
    PathResolution,
    InvalidDomain,
    Cycle,
    Ignored,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum InherentImplAdmissibility<'db> {
    Admissible {
        ty: TyId<'db>,
    },
    NotAllowed {
        ty: TyId<'db>,
        is_nominal: bool,
    },
    InvalidTy {
        ty: TyId<'db>,
    },
    IllFormed {
        ty: TyId<'db>,
        goal: TraitInstId<'db>,
        subgoal: Option<TraitInstId<'db>>,
    },
}

impl<'db> Impl<'db> {
    /// Semantic implementor type of this inherent impl.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let assumptions = constraints_for(db, self.into());
        self.type_ref(db)
            .to_opt()
            .map(|hir_ty| lower_hir_ty(db, hir_ty, self.scope(), assumptions))
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::ParseError))
    }

    /// Type lowering errors for the implementor type.
    pub fn ty_errors(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let Some(hir_ty) = self.type_ref(db).to_opt() else {
            return Vec::new();
        };
        let assumptions = constraints_for(db, self.into());
        collect_ty_lower_errors(
            db,
            self.scope(),
            hir_ty,
            self.span().target_ty(),
            assumptions,
        )
    }

    /// Iterate associated const definitions in this inherent impl block as views.
    pub fn assoc_consts(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = InherentAssocConstView<'db>> + 'db {
        let len = self.hir_consts(db).len();
        (0..len).map(move |idx| InherentAssocConstView { owner: self, idx })
    }

    /// Get an associated const view by name, if it exists in this impl block.
    pub fn const_(
        self,
        db: &'db dyn HirDb,
        name: IdentId<'db>,
    ) -> Option<InherentAssocConstView<'db>> {
        self.assoc_consts(db).find(|c| c.name(db) == Some(name))
    }

    /// Get the index of an associated const by name, if it exists. Mirrors
    /// `Trait::const_index` so `ScopeId::ImplConst` indices are computed in one
    /// place instead of open-coded `hir_consts().position()` scans.
    pub fn const_index(self, db: &'db dyn HirDb, name: IdentId<'db>) -> Option<usize> {
        self.assoc_consts(db)
            .enumerate()
            .find_map(|(idx, c)| (c.name(db) == Some(name)).then_some(idx))
    }

    /// Get an associated const view by its index. Mirrors `Trait::const_by_index`
    /// so `ScopeId::ImplConst` resolution routes through one helper instead of an
    /// open-coded `hir_consts()[idx]` slice access.
    pub fn const_by_index(self, idx: usize) -> InherentAssocConstView<'db> {
        InherentAssocConstView { owner: self, idx }
    }

    pub(crate) fn inherent_impl_admissibility(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> InherentImplAdmissibility<'db> {
        let ty = self.ty(db);
        let ingot = self.top_mod(db).ingot(db);
        if !ty.is_inherent_impl_allowed(db, ingot) {
            let base = ty.base_ty(db);
            return InherentImplAdmissibility::NotAllowed {
                ty,
                is_nominal: !base.is_param(db),
            };
        }
        if ty.has_invalid(db) {
            return InherentImplAdmissibility::InvalidTy { ty };
        }

        match check_ty_wf(
            db,
            TraitSolveCx::new(db, self.scope()).with_assumptions(param_env(db, self.into())),
            ty,
        ) {
            WellFormedness::WellFormed => InherentImplAdmissibility::Admissible { ty },
            WellFormedness::IllFormed { goal, subgoal } => {
                InherentImplAdmissibility::IllFormed { ty, goal, subgoal }
            }
        }
    }

    pub(crate) fn admissible_inherent_impl_ty(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<TyId<'db>> {
        match self.inherent_impl_admissibility(db) {
            InherentImplAdmissibility::Admissible { ty } => Some(ty),
            InherentImplAdmissibility::NotAllowed { .. }
            | InherentImplAdmissibility::InvalidTy { .. }
            | InherentImplAdmissibility::IllFormed { .. } => None,
        }
    }

    /// Returns the pretty-printed target type name from this impl block.
    /// This is useful for documentation generation.
    /// Returns None if the type reference is missing or malformed.
    pub fn target_type_name(self, db: &dyn HirDb) -> Option<String> {
        self.type_ref(db).to_opt().map(|ty| ty.pretty_print(db))
    }
}

/// Qualified path for a scope, resolving inherent-impl consts to their target
/// type's actual path. For `ScopeId::ImplConst` this yields e.g. `a::Foo::X`
/// even when the impl is written in module `b` via `use a::Foo` — something
/// `ScopeId::pretty_path` (which only has `HirDb`) cannot do, because the
/// target type needs name resolution. Tooling (goto, LSIF monikers, SCIP) must
/// use this instead of `scope.pretty_path` for inherent-const scopes.
pub fn scope_qualified_path<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
) -> Option<String> {
    if let ScopeId::ImplConst(impl_, _) = scope {
        let self_ty = impl_.ty(db);
        let name = scope.name(db)?;
        let (_, args) = self_ty.decompose_ty_app(db);
        // Build the target type's path. Named types keep their module-qualified
        // path; specialized impls (`impl Foo<1>`) include their type arguments
        // so distinct definitions don't collapse to the same path; primitives
        // (`impl u256`) have no scope, so fall back to the type print.
        let type_path = match self_ty.as_scope(db).and_then(|s| s.pretty_path(db)) {
            Some(base) if args.is_empty() => base,
            Some(base) => {
                let arg_list = args
                    .iter()
                    .map(|arg| arg.pretty_print(db).to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{base}<{arg_list}>")
            }
            None => self_ty.pretty_print(db).to_string(),
        };
        return Some(format!("{type_path}::{}", name.data(db)));
    }
    scope.pretty_path(db)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ImplTraitLowerError<'db> {
    ParseError,
    TraitRef(TraitRefLowerError<'db>),
    Conflict {
        primary: ImplTrait<'db>,
        conflict: ImplTrait<'db>,
    },
    KindMismatch {
        expected: Kind,
        actual: TyId<'db>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TraitImplAdmissibility {
    Admissible,
    ExternalTraitForExternalType,
    UnsafeLocalBoundBlanketImpl,
}

impl<'db> ImplTrait<'db> {
    /// Semantic self type of this impl-trait block.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let assumptions = constraints_for(db, self.into());
        self.type_ref(db)
            .to_opt()
            .map(|hir_ty| lower_hir_ty(db, hir_ty, self.scope(), assumptions))
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::ParseError))
    }

    /// Type lowering errors for the implementor type.
    pub fn ty_errors(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let Some(hir_ty) = self.type_ref(db).to_opt() else {
            return Vec::new();
        };
        let assumptions = constraints_for(db, self.into());
        collect_ty_lower_errors(db, self.scope(), hir_ty, self.span().ty(), assumptions)
    }

    /// Lowers this impl-trait to a semantic implementor view, performing
    /// conflict detection and kind checks.
    pub(crate) fn lowered_implementor(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<Binder<ImplementorId<'db>>, ImplTraitLowerError<'db>> {
        // Early return if the implementor type is syntactically missing or invalid.
        if matches!(
            self.ty(db).data(db),
            TyData::Invalid(InvalidCause::ParseError)
        ) {
            return Err(ImplTraitLowerError::ParseError);
        }
        // Note: we do NOT check ty.has_invalid(db) here universally, because
        // we want to proceed with lowering (and potentially report unrelated
        // errors) unless it's a hard parse error. Diagnostics code will check
        // validity separately.

        // Lower trait inst
        let trait_inst = match self.trait_inst_result(db) {
            Ok(inst) => inst,
            Err(err) => return Err(ImplTraitLowerError::TraitRef(err)),
        };

        // Build implementor view
        let params = self.impl_params(db);
        let types = self.assoc_type_bindings_for_trait_inst(db, trait_inst);
        let implementor = Binder::bind(ImplementorId::new(
            db,
            trait_inst,
            params,
            types,
            ImplementorOrigin::Hir(self),
        ));

        // Conflict check
        let trait_ = implementor.skip_binder().trait_(db);
        let env = ingot_trait_env(db, self.top_mod(db).ingot(db));
        if let Some(impls) = env.impls.get(&trait_.def(db)) {
            for &cand_view in impls {
                let cand_impl_trait = cand_view.skip_binder().hir_impl_trait(db);
                if cand_impl_trait == self {
                    continue;
                }
                if does_impl_trait_conflict(db, cand_view, implementor)
                    && !Self::impl_trait_conflict_is_sealed_marker_disjoint(
                        db,
                        cand_impl_trait,
                        self,
                    )
                {
                    return Err(ImplTraitLowerError::Conflict {
                        primary: cand_impl_trait,
                        conflict: self,
                    });
                }
            }
        }

        // Kind check
        let expected_kind = implementor
            .instantiate_identity()
            .trait_def(db)
            .self_param(db)
            .kind(db);

        let self_ty = self.ty(db);
        if self_ty.kind(db) != expected_kind {
            return Err(ImplTraitLowerError::KindMismatch {
                expected: expected_kind.clone(),
                actual: implementor.instantiate_identity().self_ty(db),
            });
        }

        Ok(implementor)
    }

    /// Internal helper that lowers the trait reference of this `impl trait`
    /// block to a semantic trait instance, preserving detailed error
    /// information.
    ///
    /// This is the canonical entry point for trait‑ref lowering from
    /// `impl trait` items. All callers that care about diagnostics should
    /// prefer this over re‑invoking `lower_trait_ref` directly.
    pub(crate) fn trait_inst_result(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<TraitInstId<'db>, TraitRefLowerError<'db>> {
        let ty = self.ty(db);

        // Preserve the existing "parse error / invalid type -> early return
        // with no diags from this helper" behavior.
        if matches!(ty.data(db), TyData::Invalid(InvalidCause::ParseError)) {
            return Err(TraitRefLowerError::Ignored);
        }
        if ty.has_invalid(db) {
            return Err(TraitRefLowerError::Ignored);
        }

        // No trait-ref in source: nothing to report here.
        let Some(trait_ref) = self.trait_ref(db).to_opt() else {
            return Err(TraitRefLowerError::Ignored);
        };

        // Assumptions derived from this impl-trait item, shared with other
        // semantic helpers.
        let assumptions = constraints_for(db, self.into());

        let trait_inst = lower_trait_ref(db, ty, trait_ref, self.scope(), assumptions, None)?;

        match self.trait_impl_admissibility(db, ty, trait_inst, assumptions) {
            TraitImplAdmissibility::Admissible => {}
            TraitImplAdmissibility::ExternalTraitForExternalType => {
                return Err(TraitRefLowerError::Ignored);
            }
            TraitImplAdmissibility::UnsafeLocalBoundBlanketImpl => {
                return Err(TraitRefLowerError::UnsafeLocalBoundBlanketImpl);
            }
        }

        Ok(trait_inst)
    }

    fn trait_impl_admissibility(
        self,
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        trait_inst: TraitInstId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> TraitImplAdmissibility {
        let impl_trait_ingot = self.top_mod(db).ingot(db);
        if Some(impl_trait_ingot) == ty.ingot(db)
            || impl_trait_ingot == trait_inst.def(db).ingot(db)
        {
            return TraitImplAdmissibility::Admissible;
        }

        if !matches!(ty.data(db), TyData::TyParam(_)) {
            return TraitImplAdmissibility::ExternalTraitForExternalType;
        }

        if assumptions.list(db).iter().any(|pred| {
            pred.self_ty(db) == ty
                && Self::is_sealed_local_marker_anchor(db, pred.def(db), impl_trait_ingot)
        }) {
            TraitImplAdmissibility::Admissible
        } else {
            TraitImplAdmissibility::UnsafeLocalBoundBlanketImpl
        }
    }

    fn is_sealed_local_marker_anchor(
        db: &'db dyn HirAnalysisDb,
        marker: Trait<'db>,
        impl_trait_ingot: common::ingot::Ingot<'db>,
    ) -> bool {
        if marker.ingot(db) != impl_trait_ingot
            || marker.scope().data(db).vis.is_pub()
            || !marker.original_params(db).is_empty()
            || marker.methods(db).next().is_some()
            || !marker.types(db).is_empty()
            || !marker.consts(db).is_empty()
            || marker.super_trait_bounds(db).next().is_some()
        {
            return false;
        }

        marker
            .ingot(db)
            .all_impl_traits(db)
            .iter()
            .copied()
            .filter(|&impl_| Self::impl_trait_implements_marker(db, impl_, marker))
            .all(|impl_| Self::impl_trait_targets_local_nominal_root(db, impl_, impl_trait_ingot))
    }

    fn impl_trait_implements_marker(
        db: &'db dyn HirAnalysisDb,
        impl_trait: ImplTrait<'db>,
        marker: Trait<'db>,
    ) -> bool {
        let Some(trait_ref) = impl_trait.trait_ref(db).to_opt() else {
            return false;
        };
        let assumptions = constraints_for(db, impl_trait.into());
        lower_trait_ref(
            db,
            impl_trait.ty(db),
            trait_ref,
            impl_trait.scope(),
            assumptions,
            None,
        )
        .is_ok_and(|inst| inst.def(db) == marker)
    }

    fn impl_trait_conflict_is_sealed_marker_disjoint(
        db: &'db dyn HirAnalysisDb,
        a: ImplTrait<'db>,
        b: ImplTrait<'db>,
    ) -> bool {
        let a_markers = Self::sealed_local_marker_anchors(db, a);
        let b_markers = Self::sealed_local_marker_anchors(db, b);
        a_markers.iter().any(|&a_marker| {
            let a_roots = Self::sealed_marker_target_roots(db, a_marker);
            b_markers.iter().any(|&b_marker| {
                a_roots.is_disjoint(&Self::sealed_marker_target_roots(db, b_marker))
            })
        })
    }

    fn sealed_local_marker_anchors(
        db: &'db dyn HirAnalysisDb,
        impl_trait: ImplTrait<'db>,
    ) -> Vec<Trait<'db>> {
        let impl_trait_ingot = impl_trait.top_mod(db).ingot(db);
        let ty = impl_trait.ty(db);
        constraints_for(db, impl_trait.into())
            .list(db)
            .iter()
            .filter_map(|pred| {
                let marker = pred.def(db);
                (pred.self_ty(db) == ty
                    && Self::is_sealed_local_marker_anchor(db, marker, impl_trait_ingot))
                .then_some(marker)
            })
            .collect()
    }

    fn sealed_marker_target_roots(
        db: &'db dyn HirAnalysisDb,
        marker: Trait<'db>,
    ) -> FxHashSet<TyBase<'db>> {
        marker
            .ingot(db)
            .all_impl_traits(db)
            .iter()
            .copied()
            .filter(|&impl_| Self::impl_trait_implements_marker(db, impl_, marker))
            .filter_map(|impl_| Self::impl_trait_local_nominal_root(db, impl_))
            .collect()
    }

    fn impl_trait_targets_local_nominal_root(
        db: &'db dyn HirAnalysisDb,
        impl_trait: ImplTrait<'db>,
        impl_trait_ingot: common::ingot::Ingot<'db>,
    ) -> bool {
        Self::impl_trait_local_nominal_root(db, impl_trait).is_some_and(|root| match root {
            TyBase::Adt(adt) => adt.ingot(db) == impl_trait_ingot,
            TyBase::Contract(contract) => contract.top_mod(db).ingot(db) == impl_trait_ingot,
            TyBase::Prim(_) | TyBase::Func(_) => false,
        })
    }

    fn impl_trait_local_nominal_root(
        db: &'db dyn HirAnalysisDb,
        impl_trait: ImplTrait<'db>,
    ) -> Option<TyBase<'db>> {
        match impl_trait.ty(db).base_ty(db).data(db) {
            TyData::TyBase(root @ (TyBase::Adt(_) | TyBase::Contract(_))) => Some(*root),
            _ => None,
        }
    }

    /// Semantic generic parameter types for this `impl trait` block, in
    /// definition order (including `Self` when present).
    ///
    /// This is a thin wrapper over the generic-param collector used elsewhere
    /// and keeps the param‑set semantics rooted on the item.
    pub(crate) fn impl_params(self, db: &'db dyn HirAnalysisDb) -> Vec<TyId<'db>> {
        collect_generic_params(db, self.into()).params(db).to_vec()
    }

    /// Semantic associated-type bindings for this `impl trait` block, given the
    /// lowered trait instance.
    ///
    /// This mirrors the logic used in `lower_impl_trait`:
    /// - start from the explicitly provided associated types in the impl block;
    /// - then merge in defaults from the trait definition, instantiated with the
    ///   concrete generic arguments of `trait_inst` (including `Self`).
    ///
    /// Kept crate‑internal so that engine code (trait env, implementor IR) can
    /// reuse the same semantics without re‑implementing the merge.
    pub(crate) fn assoc_type_bindings_for_trait_inst(
        self,
        db: &'db dyn HirAnalysisDb,
        trait_inst: TraitInstId<'db>,
    ) -> IndexMap<IdentId<'db>, TyId<'db>> {
        // Semantic associated type implementations in this impl-trait block.
        let mut types: IndexMap<_, _> = self
            .assoc_types(db)
            .filter_map(|v| v.name(db).zip(v.ty(db)))
            .collect();

        // Merge trait associated type defaults into the implementor, but evaluated in
        // the trait's own scope and then instantiated with this impl's concrete args
        // (including Self). This ensures defaults like `type Output = Self` resolve
        // to the implementor's concrete self type rather than remaining as `Self`.
        let trait_def = trait_inst.def(db);
        for view in trait_def.assoc_types(db) {
            let (Some(name), Some(default)) = (view.name(db), view.default_ty(db)) else {
                continue;
            };

            types
                .entry(name)
                .or_insert_with(|| Binder::bind(default).instantiate(db, trait_inst.args(db)));
        }

        types
    }

    /// Semantic trait instance implemented by this `impl trait` block, if well-formed.
    ///
    /// This delegates to [`ImplTrait::trait_inst_result`], which preserves
    /// detailed error information for diagnostics while providing a
    /// traversal‑friendly entry point rooted on the HIR item.
    pub fn trait_inst(self, db: &'db dyn HirAnalysisDb) -> Option<TraitInstId<'db>> {
        self.trait_inst_result(db).ok()
    }

    /// Trait definition implemented by this `impl trait` block, if well-formed.
    pub fn trait_def(self, db: &'db dyn HirAnalysisDb) -> Option<Trait<'db>> {
        self.trait_inst(db).map(|inst| inst.def(db))
    }

    /// Returns the ADT (struct or enum) that this `impl Trait` block implements for,
    /// if the implementor type resolves to a concrete ADT.
    pub fn implementing_adt(self, db: &'db dyn HirAnalysisDb) -> Option<AdtRef<'db>> {
        self.ty(db).adt_ref(db)
    }

    /// Iterate associated type definitions in this impl-trait block as views.
    pub fn assoc_types(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = ImplAssocTypeView<'db>> + 'db {
        let len = self.types(db).len();
        (0..len).map(move |idx| ImplAssocTypeView { owner: self, idx })
    }

    /// Iterate associated const definitions in this impl-trait block as views.
    pub fn assoc_consts(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = ImplAssocConstView<'db>> + 'db {
        let len = self.consts(db).len();
        (0..len).map(move |idx| ImplAssocConstView { owner: self, idx })
    }

    /// Get an associated const view by name, if it exists in this impl-trait block.
    pub fn const_(self, db: &'db dyn HirDb, name: IdentId<'db>) -> Option<ImplAssocConstView<'db>> {
        self.assoc_consts(db).find(|c| c.name(db) == Some(name))
    }

    /// Diagnostics for implementor lowering failures.
    /// Returns `(Some(implementor), [])` on success, or `(None, diags)` on failure.
    pub(crate) fn implementor_with_errors(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> (
        Option<Binder<ImplementorId<'db>>>,
        Vec<TyDiagCollection<'db>>,
    ) {
        use crate::analysis::name_resolution::{ExpectedPathKind, diagnostics::PathResDiag};
        use crate::analysis::ty::diagnostics::TraitLowerDiag;

        // First check implementor type
        let ty = self.ty(db);
        if ty.has_invalid(db) {
            let diags = self.ty_errors(db);
            if !diags.is_empty() {
                return (None, diags);
            }
            if let Some(diag) = ty.emit_diag(db, self.span().ty().into()) {
                return (None, vec![diag]);
            }
            return (None, Vec::new());
        }

        if let WellFormedness::IllFormed { goal, subgoal } = check_ty_wf(
            db,
            TraitSolveCx::new(db, self.scope()).with_assumptions(param_env(db, self.into())),
            ty,
        ) {
            return (
                None,
                vec![
                    TraitConstraintDiag::TraitBoundNotSat {
                        span: self.span().ty().into(),
                        primary_goal: goal,
                        unsat_subgoal: subgoal,
                        required_by: None,
                    }
                    .into(),
                ],
            );
        }

        match self.lowered_implementor(db) {
            Ok(implementor) => (Some(implementor), Vec::new()),
            Err(err) => {
                let mut diags = Vec::new();
                match err {
                    ImplTraitLowerError::ParseError => {}
                    ImplTraitLowerError::TraitRef(lower_err) => match lower_err {
                        TraitRefLowerError::PathResError(err) => {
                            if let Some(trait_ref) = self.trait_ref(db).to_opt() {
                                let path = trait_ref.path(db).unwrap();
                                if let Some(diag) = err.into_diag(
                                    db,
                                    path,
                                    self.span().trait_ref().path(),
                                    ExpectedPathKind::Trait,
                                ) {
                                    diags.push(diag.into());
                                }
                            }
                        }
                        TraitRefLowerError::InvalidDomain(res) => {
                            if let Some(trait_ref) = self.trait_ref(db).to_opt() {
                                diags.push(
                                    PathResDiag::ExpectedTrait(
                                        self.span().trait_ref().path().into(),
                                        trait_ref.path(db).unwrap().ident(db).unwrap(),
                                        res.kind_name(),
                                    )
                                    .into(),
                                );
                            }
                        }
                        TraitRefLowerError::Cycle => {
                            diags.push(TraitLowerDiag::CyclicTraitRef(self).into());
                        }
                        TraitRefLowerError::UnsafeLocalBoundBlanketImpl => {
                            diags.push(TraitLowerDiag::UnsafeLocalBoundBlanketImpl(self).into());
                        }
                        TraitRefLowerError::Ignored => {
                            diags.push(TraitLowerDiag::ExternalTraitForExternalType(self).into());
                        }
                    },
                    ImplTraitLowerError::Conflict { primary, conflict } => {
                        let local_impls = self.top_mod(db).ingot(db).all_impl_traits(db);
                        let primary_idx = local_impls.iter().position(|&item| item == primary);
                        let conflict_idx = local_impls.iter().position(|&item| item == conflict);
                        if !matches!(
                            (primary_idx, conflict_idx),
                            (Some(primary_idx), Some(conflict_idx)) if primary_idx > conflict_idx
                        ) {
                            diags.push(
                                TraitLowerDiag::ConflictTraitImpl {
                                    primary,
                                    conflict_with: conflict,
                                }
                                .into(),
                            );
                        }
                    }
                    ImplTraitLowerError::KindMismatch { expected, actual } => {
                        diags.push(
                            TraitConstraintDiag::TraitArgKindMismatch {
                                span: self.span().trait_ref(),
                                expected,
                                actual,
                            }
                            .into(),
                        );
                    }
                }
                (None, diags)
            }
        }
    }

    /// Returns the pretty-printed trait name from this impl trait block.
    /// This is useful for documentation generation.
    /// Returns None if the trait reference is missing or malformed.
    pub fn trait_name(self, db: &dyn HirDb) -> Option<String> {
        self.trait_ref(db).to_opt().map(|tr| tr.pretty_print(db))
    }

    /// Returns the pretty-printed target type name from this impl trait block.
    /// This is useful for documentation generation.
    /// Returns None if the type reference is missing or malformed.
    pub fn target_type_name(self, db: &dyn HirDb) -> Option<String> {
        self.type_ref(db).to_opt().map(|ty| ty.pretty_print(db))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ImplAssocTypeView<'db> {
    owner: ImplTrait<'db>,
    idx: usize,
}

impl<'db> ImplAssocTypeView<'db> {
    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.owner.types(db)[self.idx].name.to_opt()
    }

    pub fn span(self) -> crate::span::item::LazyTraitTypeSpan<'db> {
        self.owner.span().associated_type(self.idx)
    }

    /// The owning impl-trait block.
    pub fn owner(self) -> ImplTrait<'db> {
        self.owner
    }

    /// Semantic type of this associated type implementation.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let hir = self.owner.types(db)[self.idx].type_ref.to_opt()?;
        let assumptions = constraints_for(db, self.owner.into());
        Some(lower_hir_ty(db, hir, self.owner.scope(), assumptions))
    }

    /// All type-related diagnostics for this associated type.
    pub fn ty_diags(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let Some(hir) = self.owner.types(db)[self.idx].type_ref.to_opt() else {
            return Vec::new();
        };

        let ty_span = self.span().ty();
        let assumptions = constraints_for(db, self.owner.into());

        let errs =
            collect_ty_lower_errors(db, self.owner.scope(), hir, ty_span.clone(), assumptions);
        if !errs.is_empty() {
            return errs;
        }

        let ty = lower_hir_ty(db, hir, self.owner.scope(), assumptions);
        if let WellFormedness::IllFormed { goal, subgoal } = check_ty_wf(
            db,
            TraitSolveCx::new(db, self.owner.scope())
                .with_assumptions(param_env(db, self.owner.into())),
            ty,
        ) {
            return vec![
                TraitConstraintDiag::TraitBoundNotSat {
                    span: ty_span.into(),
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                    required_by: None,
                }
                .into(),
            ];
        }

        Vec::new()
    }
}

// Const / Use ---------------------------------------------------------------

impl<'db> Const<'db> {
    // Planned semantic surface:
    // - additional const semantics/diags as needed

    /// Semantic type of this const definition.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let Some(hir_ty) = self.type_ref(db).to_opt() else {
            return TyId::invalid(db, InvalidCause::ParseError);
        };
        lower_hir_ty(db, hir_ty, self.scope(), PredicateListId::empty_list(db))
    }
}

impl<'db> Use<'db> {
    // Planned semantic surface:
    // - resolved target summary / simple diags (optional)
}

// Shared capability hints ---------------------------------------------------

impl<'db> ItemKind<'db> {
    // Planned semantic surface:
    // - semantic helpers to opt into capabilities (HasGenericParams/HasWhereClause)
}

// Note:
// Avoid adding tracked methods here. Keep tracked queries in lowering and call
// them from small, semantic helpers only. This module should remain the
// ergonomic public surface for traversal without leaking syntax.

impl<'db> GenericParamOwner<'db> {
    pub fn param_view(self, db: &'db dyn HirDb, idx: usize) -> GenericParamView<'db> {
        self.params(db)
            .nth(idx)
            .expect("failed to get the generic param")
    }

    pub fn params(self, db: &'db dyn HirDb) -> impl Iterator<Item = GenericParamView<'db>> + 'db {
        self.params_list(db)
            .data(db)
            .iter()
            .enumerate()
            .map(move |(idx, param)| GenericParamView {
                owner: self,
                param,
                idx,
            })
    }
}

impl<'db> GenericParamView<'db> {
    /// Lazy span of this generic parameter in its owner's parameter list.
    ///
    /// Exposes a context-free handle that can be resolved to a concrete
    /// source span via `SpannedHirDb`, without requiring callers to manually
    /// cross-link list spans with indices.
    pub fn span(&self) -> crate::span::params::LazyGenericParamSpan<'db> {
        self.owner.params_span().param(self.idx)
    }

    /// Lazy span atom for the parameter's name token.
    ///
    /// Returns the span for the ident of a type or const generic param,
    /// depending on the underlying parameter kind.
    pub fn name_span(&self) -> crate::span::LazySpanAtom<'db> {
        match self.param {
            GenericParam::Type(_) => self.span().into_type_param().name(),
            GenericParam::Const(_) => self.span().into_const_param().name(),
        }
    }
}

// Associated type views -----------------------------------------------------

#[derive(Clone, Copy, Debug)]
pub struct TraitAssocTypeView<'db> {
    owner: Trait<'db>,
    idx: usize,
}

impl<'db> TraitAssocTypeView<'db> {
    fn decl(self, db: &'db dyn HirDb) -> &'db crate::core::hir_def::AssocTyDecl<'db> {
        &self.owner.types(db)[self.idx]
    }

    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.decl(db).name.to_opt()
    }

    pub fn span(self) -> crate::span::item::LazyTraitTypeSpan<'db> {
        self.owner.span().item_list().assoc_type(self.idx)
    }

    /// Raw bounds for this associated type (HIR). Prefer semantic checks where possible.
    pub(in crate::core) fn bounds_raw(self, db: &'db dyn HirDb) -> &'db [TypeBound<'db>] {
        &self.decl(db).bounds
    }

    /// Semantic default type for this associated type, lowered in the trait's
    /// scope using the trait's own constraints. Returns None if no default.
    pub fn default_ty(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<crate::analysis::ty::ty_def::TyId<'db>> {
        let hir = self.decl(db).default?;
        let trait_ = self.owner;
        let assumptions = constraints_for(db, trait_.into());
        Some(lower_hir_ty(db, hir, trait_.scope(), assumptions))
    }

    /// Semantic trait bounds using the trait's own `Self` as the subject.
    pub fn bounds_on_self(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = TraitInstId<'db>> + 'db {
        let self_ty = self.owner.self_param(db);
        self.bounds_on_subject(db, self_ty)
    }

    /// Semantic trait bounds for an explicit subject, using the owner's `Self`
    /// in the trait arguments.
    pub fn bounds_on_subject(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: TyId<'db>,
    ) -> impl Iterator<Item = TraitInstId<'db>> + 'db {
        let owner_self = self.owner.self_param(db);
        AssocTypeBounds {
            base: self,
            subject,
            owner_self,
        }
        .bounds(db)
    }

    /// Semantic trait bounds for an explicit subject with an explicit owner
    /// `Self` override (e.g., impl-trait analysis).
    pub fn bounds_on_subject_with_owner(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: TyId<'db>,
        owner_self: TyId<'db>,
    ) -> impl Iterator<Item = TraitInstId<'db>> + 'db {
        AssocTypeBounds {
            base: self,
            subject,
            owner_self,
        }
        .bounds(db)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct AssocTypeBoundView<'db> {
    owner: TraitAssocTypeView<'db>,
    idx: usize,
}

impl<'db> TraitAssocTypeView<'db> {
    /// Iterate trait bounds as per-bound semantic views.
    pub fn bounds(self, db: &'db dyn HirDb) -> impl Iterator<Item = AssocTypeBoundView<'db>> + 'db {
        let len = self.bounds_raw(db).len();
        let idxs: Vec<usize> = (0..len)
            .filter(|&i| matches!(self.bounds_raw(db)[i], TypeBound::Trait(_)))
            .collect();
        idxs.into_iter()
            .map(move |idx| AssocTypeBoundView { owner: self, idx })
    }
}

impl<'db> AssocTypeBoundView<'db> {
    fn trait_ref(self, db: &'db dyn HirDb) -> TraitRefId<'db> {
        match self.owner.bounds_raw(db)[self.idx] {
            TypeBound::Trait(tr) => tr,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct AssocTypeBounds<'db> {
    base: TraitAssocTypeView<'db>,
    subject: TyId<'db>,
    owner_self: TyId<'db>,
}

impl<'db> AssocTypeBounds<'db> {
    fn bounds(self, db: &'db dyn HirAnalysisDb) -> impl Iterator<Item = TraitInstId<'db>> + 'db {
        let owner_trait = self.base.owner;
        let scope = owner_trait.scope();
        let assumptions = constraints_for(db, owner_trait.into());
        self.base.bounds(db).filter_map(move |b| {
            b.to_trait_inst(db, self.subject, self.owner_self, scope, assumptions)
        })
    }
}

impl<'db> AssocTypeBoundView<'db> {
    /// Lower this bound to a trait instance for the given subject and owner `Self`.
    fn to_trait_inst(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: TyId<'db>,
        owner_self: TyId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Option<TraitInstId<'db>> {
        lower_trait_ref(
            db,
            subject,
            self.trait_ref(db),
            scope,
            assumptions,
            Some(owner_self),
        )
        .ok()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ImplementorAssocTypeView<'db> {
    implementor: ImplementorId<'db>,
    assoc: TraitAssocTypeView<'db>,
    impl_ty: TyId<'db>,
}

impl<'db> ImplementorAssocTypeView<'db> {
    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.assoc.name(db)
    }

    pub fn bounds(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = TraitInstId<'db>> + 'db {
        self.assoc
            .bounds_on_subject_with_owner(db, self.impl_ty, self.implementor.self_ty(db))
    }

    pub fn impl_ty(self) -> TyId<'db> {
        self.impl_ty
    }
}

impl<'db> ImplementorId<'db> {
    /// Contextual assoc-type views for this implementor, pairing each trait associated
    /// type with the implemented type (if provided).
    pub fn assoc_type_views(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = ImplementorAssocTypeView<'db>> + 'db {
        let trait_hir = self.trait_def(db);
        let impl_types = self.types(db);
        trait_hir.assoc_types(db).filter_map(move |assoc| {
            let name = assoc.name(db)?;
            let &impl_ty = impl_types.get(&name)?;
            Some(ImplementorAssocTypeView {
                implementor: self,
                assoc,
                impl_ty,
            })
        })
    }
}

impl<'db> TyId<'db> {
    /// Type-rooted entry to associated-type bounds: attach this type as subject.
    pub fn assoc_type_bounds(
        self,
        db: &'db dyn HirAnalysisDb,
        assoc: TraitAssocTypeView<'db>,
    ) -> impl Iterator<Item = TraitInstId<'db>> + 'db {
        assoc.bounds_on_subject(db, self)
    }
}

// Associated const views ----------------------------------------------------

#[derive(Clone, Copy, Debug)]
pub struct TraitAssocConstView<'db> {
    owner: Trait<'db>,
    idx: usize,
}

impl<'db> TraitAssocConstView<'db> {
    fn decl(self, db: &'db dyn HirDb) -> &'db crate::core::hir_def::AssocConstDecl<'db> {
        &self.owner.consts(db)[self.idx]
    }

    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.decl(db).name.to_opt()
    }

    pub fn span(self) -> crate::span::item::LazyTraitConstSpan<'db> {
        self.owner.span().item_list().assoc_const(self.idx)
    }

    /// Returns true if this associated const has a default value in the trait.
    pub fn has_default(self, db: &'db dyn HirDb) -> bool {
        self.decl(db).default.is_some()
    }

    pub fn default_body(self, db: &'db dyn HirDb) -> Option<crate::core::hir_def::Body<'db>> {
        self.decl(db).default.and_then(|body| body.to_opt())
    }

    /// Semantic type of this associated const, lowered in the trait's scope.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let hir = self.decl(db).ty.to_opt()?;
        let trait_ = self.owner;
        let assumptions = constraints_for(db, trait_.into());
        Some(lower_hir_ty(db, hir, trait_.scope(), assumptions))
    }

    /// Semantic type of this associated const as a Binder, suitable for
    /// instantiation with trait args.
    pub fn ty_binder(self, db: &'db dyn HirAnalysisDb) -> Option<Binder<TyId<'db>>> {
        self.ty(db).map(Binder::bind)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ImplAssocConstView<'db> {
    owner: ImplTrait<'db>,
    idx: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct InherentAssocConstView<'db> {
    owner: Impl<'db>,
    idx: usize,
}

impl<'db> InherentAssocConstView<'db> {
    fn def(self, db: &'db dyn HirDb) -> &'db crate::core::hir_def::AssocConstDef<'db> {
        &self.owner.hir_consts(db)[self.idx]
    }

    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.def(db).name.to_opt()
    }

    pub fn span(self) -> crate::span::item::LazyTraitConstSpan<'db> {
        self.owner.span().associated_const(self.idx)
    }

    /// Returns true if this associated const has a value defined.
    pub fn has_value(self, db: &'db dyn HirDb) -> bool {
        self.def(db).value.to_opt().is_some()
    }

    pub fn value_body(self, db: &'db dyn HirDb) -> Option<crate::core::hir_def::Body<'db>> {
        self.def(db).value.to_opt()
    }

    /// The HIR type annotation of this associated const.
    pub fn hir_ty(self, db: &'db dyn HirDb) -> Option<crate::core::hir_def::TypeId<'db>> {
        self.def(db).ty.to_opt()
    }

    /// Semantic type of this associated const, referencing the impl's own
    /// generic parameters.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let hir = self.def(db).ty.to_opt()?;
        let assumptions = constraints_for(db, self.owner.into());
        Some(lower_hir_ty(db, hir, self.owner.scope(), assumptions))
    }
}

impl<'db> ImplAssocConstView<'db> {
    fn def(self, db: &'db dyn HirDb) -> &'db crate::core::hir_def::AssocConstDef<'db> {
        &self.owner.consts(db)[self.idx]
    }

    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.def(db).name.to_opt()
    }

    pub fn span(self) -> crate::span::item::LazyTraitConstSpan<'db> {
        self.owner.span().associated_const(self.idx)
    }

    pub fn docs(self, db: &'db dyn HirDb) -> Option<String> {
        symbol::docs_from_attrs(db, self.def(db).attributes)
    }

    pub fn signature_with_span(self, db: &'db dyn SpannedHirDb) -> Option<SignatureWithSpan> {
        symbol::get_assoc_const_signature_with_span(db, self.span())
    }

    /// Returns true if this associated const has a value defined.
    pub fn has_value(self, db: &'db dyn HirDb) -> bool {
        self.def(db).value.to_opt().is_some()
    }

    pub fn value_body(self, db: &'db dyn HirDb) -> Option<crate::core::hir_def::Body<'db>> {
        self.def(db).value.to_opt()
    }

    /// Semantic type of this associated const implementation.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let hir = self.def(db).ty.to_opt()?;
        let assumptions = constraints_for(db, self.owner.into());
        Some(lower_hir_ty(db, hir, self.owner.scope(), assumptions))
    }

    /// HIR type annotation of this associated const implementation.
    pub fn hir_ty(self, db: &'db dyn HirDb) -> Option<TypeId<'db>> {
        self.def(db).ty.to_opt()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct VariantView<'db> {
    pub owner: Enum<'db>,
    pub idx: usize,
}

impl<'db> VariantView<'db> {
    pub fn kind(self, db: &'db dyn HirDb) -> VariantKind<'db> {
        self.owner.variants_list(db).data(db)[self.idx].kind
    }

    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        self.owner.variants_list(db).data(db)[self.idx]
            .name
            .to_opt()
    }

    pub fn span(self) -> crate::span::item::LazyVariantDefSpan<'db> {
        self.owner.span().variants().variant(self.idx)
    }

    /// Returns semantic types of this variant's fields (empty for unit variants).
    pub fn field_tys(self, db: &'db dyn HirAnalysisDb) -> Vec<Binder<TyId<'db>>> {
        use crate::analysis::ty::ty_def::{InvalidCause, TyId};
        use crate::analysis::ty::ty_lower::lower_hir_ty;

        let enum_ = self.owner;
        let var = EnumVariant::new(enum_, self.idx);
        let scope = var.scope();
        let assumptions =
            collect_constraints(db, GenericParamOwner::Enum(enum_)).instantiate_identity();

        match self.kind(db) {
            VariantKind::Unit => Vec::new(),
            VariantKind::Record(_) => {
                let parent = FieldParent::Variant(var);
                let fields = parent.fields_list(db);
                fields
                    .data(db)
                    .iter()
                    .map(|field| {
                        let ty = match field.type_ref.to_opt() {
                            Some(hir_ty) => lower_hir_ty(db, hir_ty, scope, assumptions),
                            None => TyId::invalid(db, InvalidCause::ParseError),
                        };
                        Binder::bind(ty)
                    })
                    .collect()
            }
            VariantKind::Tuple(tuple_id) => tuple_id
                .data(db)
                .iter()
                .map(|p| {
                    let ty = match p.to_opt() {
                        Some(hir_ty) => lower_hir_ty(db, hir_ty, scope, assumptions),
                        None => TyId::invalid(db, InvalidCause::ParseError),
                    };
                    Binder::bind(ty)
                })
                .collect(),
        }
    }

    /// Semantic field-set for this variant.
    pub fn as_adt_fields(self, db: &'db dyn HirAnalysisDb) -> &'db AdtField<'db> {
        let def = lower_adt(db, AdtRef::from(self.owner));
        &def.fields(db)[self.idx]
    }

    /// Diagnostics for tuple-variant element types: star-kind and non-const checks.
    /// Returns an empty list if this is not a tuple variant.
    /// Iterates record fields (empty for non-record variants) as contextual views.
    pub fn fields(self, db: &'db dyn HirDb) -> impl Iterator<Item = FieldView<'db>> + 'db {
        let parent = FieldParent::Variant(EnumVariant::new(self.owner, self.idx));
        let len = match self.kind(db) {
            VariantKind::Record(_) => parent.fields_list(db).data(db).len(),
            _ => 0,
        };
        (0..len).map(move |idx| FieldView { parent, idx })
    }
}

// Field views --------------------------------------------------------------

#[derive(Clone, Copy, Debug)]
pub struct FieldView<'db> {
    pub parent: FieldParent<'db>,
    pub idx: usize,
}

impl<'db> FieldView<'db> {
    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        let list = self.parent.fields_list(db);
        list.data(db)[self.idx].name.to_opt()
    }

    /// Returns the semantic type of this field.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let (adt_field, idx) = self.as_adt_field(db);
        *adt_field.ty(db, idx).skip_binder()
    }

    /// Returns the semantic ADT field-set and index for this field.
    pub fn as_adt_field(self, db: &'db dyn HirAnalysisDb) -> (AdtField<'db>, usize) {
        (self.parent.as_adt_fields(db), self.idx)
    }

    pub fn ty_span(self) -> crate::span::DynLazySpan<'db> {
        match self.parent {
            FieldParent::Struct(s) => s.span().fields().field(self.idx).ty().into(),
            FieldParent::Contract(c) => c.span().fields().field(self.idx).ty().into(),
            FieldParent::Variant(v) => v.span().fields().field(self.idx).ty().into(),
        }
    }

    /// Returns the lazy span for this field's type in `LazyTySpan` form.
    pub fn lazy_ty_span(self) -> crate::span::types::LazyTySpan<'db> {
        match self.parent {
            FieldParent::Struct(s) => s.span().fields().field(self.idx).ty(),
            FieldParent::Contract(c) => c.span().fields().field(self.idx).ty(),
            FieldParent::Variant(v) => v.span().fields().field(self.idx).ty(),
        }
    }

    /// Returns the scope for type resolution in this field.
    pub fn scope(self) -> ScopeId<'db> {
        match self.parent {
            FieldParent::Struct(s) => s.scope(),
            FieldParent::Contract(c) => c.scope(),
            FieldParent::Variant(v) => v.enum_.scope(),
        }
    }

    /// Returns the owning item for constraint collection.
    pub fn owner_item(self) -> ItemKind<'db> {
        match self.parent {
            FieldParent::Struct(s) => ItemKind::Struct(s),
            FieldParent::Contract(c) => ItemKind::Contract(c),
            FieldParent::Variant(v) => ItemKind::Enum(v.enum_),
        }
    }

    /// All type-related diagnostics for this field.
    pub fn ty_diags(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::analysis::name_resolution::{PathRes, resolve_path};
        use crate::analysis::ty::ty_def::TyData;
        use crate::analysis::ty::ty_error::collect_hir_ty_diags;

        let mut out = Vec::new();

        // First, surface name-resolution errors for the field's HIR type path.
        let hir_ty = self.parent.fields_list(db).data(db)[self.idx].type_ref;
        if let Some(hir_ty) = hir_ty.to_opt() {
            let assumptions = constraints_for(db, self.owner_item());
            let errs =
                collect_hir_ty_diags(db, self.scope(), hir_ty, self.lazy_ty_span(), assumptions);
            if !errs.is_empty() {
                return errs;
            }
        }

        let ty = self.ty(db);
        let span = self.ty_span();

        if !ty.has_star_kind(db) {
            out.push(TyLowerDiag::ExpectedStarKind(span.clone()).into());
            return out;
        }
        if ty.is_const_ty(db) {
            out.push(
                TyLowerDiag::NormalTypeExpected {
                    span: span.clone(),
                    given: ty,
                }
                .into(),
            );
            return out;
        }

        // Trait-bound well-formedness for field type.
        let owner_item = self.owner_item();
        if let WellFormedness::IllFormed { goal, subgoal } = check_ty_wf(
            db,
            TraitSolveCx::new(db, owner_item.scope()).with_assumptions(param_env(db, owner_item)),
            ty,
        ) {
            out.push(
                TraitConstraintDiag::TraitBoundNotSat {
                    span: span.clone(),
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                    required_by: None,
                }
                .into(),
            );
            return out;
        }

        // Contract-field layout rejections: a field whose storage placement
        // cannot be determined is an error rather than a silent mis-assignment.
        // At most one fires per field.
        if let FieldParent::Contract(contract) = self.parent
            && let Some(name) = self.name(db)
            && let Some(info) = contract.field_layout(db).get(&name)
        {
            // An explicit `_` const argument (layout holes must be parameter
            // defaults).
            if info.explicit_const_hole {
                out.push(TyLowerDiag::ContractFieldExplicitConstHole { span, ty }.into());
                return out;
            }
            // A selected `EffectHandle` whose `SPACE` did not resolve.
            if info.handle_space_unresolved {
                out.push(TyLowerDiag::ContractFieldHandleSpaceUnresolved { span, ty }.into());
                return out;
            }
            // A `StaticSlot` hole whose `SPACE` did not resolve.
            if info.static_slot_space_unresolved {
                out.push(TyLowerDiag::StaticSlotSpaceUnresolved { span, ty }.into());
                return out;
            }
            // A defaulted const `_` that is not a storage slot.
            if info.non_slot_const_hole {
                out.push(TyLowerDiag::ContractFieldNonSlotConstHole { span, ty }.into());
                return out;
            }
        }

        // Const type parameter mismatch check: if field name matches a const type parameter.
        if let Some(name) = self.name(db)
            && let Ok(PathRes::Ty(t)) = resolve_path(
                db,
                PathId::from_ident(db, name),
                crate::hir_def::scope_graph::ScopeId::Field(self.parent, self.idx as u16),
                PredicateListId::empty_list(db),
                true,
            )
            && let TyData::ConstTy(const_ty) = t.data(db)
        {
            let expected = *const_ty;
            let expected_ty = expected.ty(db);
            if !expected_ty.has_invalid(db) && !ty.has_invalid(db) && ty != expected_ty {
                out.push(
                    TyLowerDiag::ConstTyMismatch {
                        span,
                        expected: expected_ty,
                        given: ty,
                    }
                    .into(),
                );
                return out;
            }
        }

        out
    }
}

impl<'db> FieldParent<'db> {
    /// Iterates fields as contextual views. For variants, only record variants have fields.
    pub fn fields(self, db: &'db dyn HirDb) -> impl Iterator<Item = FieldView<'db>> + 'db {
        let len = self.fields_list(db).data(db).len();
        (0..len).map(move |idx| FieldView { parent: self, idx })
    }

    /// Semantic field-set for this parent.
    pub fn as_adt_fields(self, db: &'db dyn HirAnalysisDb) -> AdtField<'db> {
        match self {
            FieldParent::Struct(s) => s.as_adt(db).fields(db)[0].clone(),
            FieldParent::Contract(c) => lower_contract_fields(db, c),
            FieldParent::Variant(v) => v.as_adt_fields(db).clone(),
        }
    }
}

impl<'db> EnumVariant<'db> {
    /// Semantic field-set for this variant.
    pub fn as_adt_fields(self, db: &'db dyn HirAnalysisDb) -> &'db AdtField<'db> {
        let def = lower_adt(db, AdtRef::from(self.enum_));
        &def.fields(db)[self.idx as usize]
    }
}

// Type traversal helpers ----------------------------------------------------

impl<'db> TyId<'db> {
    /// Returns the field parent for this type if it's a struct or contract.
    /// This provides access to fields via `field_parent.fields(db)`.
    pub fn field_parent(self, db: &'db dyn HirAnalysisDb) -> Option<FieldParent<'db>> {
        // Check for contract first
        if let Some(contract) = self.as_contract(db) {
            return Some(FieldParent::Contract(contract));
        }
        // Check for struct
        match self.adt_ref(db)? {
            AdtRef::Struct(s) => Some(FieldParent::Struct(s)),
            AdtRef::Enum(_) => None, // Enums don't have direct field access
        }
    }
}
