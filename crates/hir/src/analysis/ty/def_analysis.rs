//! Definition analysis entrypoints that delegate to the semantic traversal API.
//! The legacy visitor-based analyzer has been removed. Keep only slim helpers
//! and public entrypoints that compose semantic diagnostics.

use common::indexmap::IndexSet;
use rustc_hash::FxHashMap;
use smallvec1::SmallVec;

use crate::analysis::HirAnalysisDb;
use crate::analysis::name_resolution::{ExpectedPathKind, diagnostics::PathResDiag};
use crate::analysis::ty::binder::Binder;
use crate::hir_def::{
    Func, GenericParamOwner, IdentId, Impl as HirImpl, ImplTrait, Trait, TraitRefId,
};

use super::{
    adt_def::{AdtDef, AdtRef},
    diagnostics::{ImplDiag, TraitConstraintDiag, TraitLowerDiag, TyDiagCollection, TyLowerDiag},
    method_cmp::compare_impl_method,
    trait_def::{Implementor, TraitDef, does_impl_trait_conflict, ingot_trait_env},
    trait_lower::{TraitRefLowerError, lower_impl_trait, lower_trait_ref},
    trait_resolution::{PredicateListId, constraint::collect_constraints},
    ty_def::{InvalidCause, TyData},
};

/// Analyze structs/enums/contracts via semantic views.
#[salsa::tracked(return_ref)]
pub fn analyze_adt<'db>(
    db: &'db dyn HirAnalysisDb,
    adt_ref: AdtRef<'db>,
) -> Vec<TyDiagCollection<'db>> {
    match adt_ref {
        AdtRef::Struct(s) => s.analyze(db),
        AdtRef::Enum(e) => e.analyze(db),
        AdtRef::Contract(c) => c.analyze(db),
    }
}

/// Analyze trait definition via semantic views.
#[salsa::tracked(return_ref)]
pub fn analyze_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db>,
) -> Vec<TyDiagCollection<'db>> {
    trait_.analyze(db)
}

/// Analyze inherent impl via semantic views.
#[salsa::tracked(return_ref)]
pub fn analyze_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: HirImpl<'db>,
) -> Vec<TyDiagCollection<'db>> {
    impl_.analyze(db)
}

/// Analyze function definition via semantic views + generic-param diags.
#[salsa::tracked(return_ref)]
pub fn analyze_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let mut diags = func.analyze(db);
    let owner = GenericParamOwner::Func(func);
    diags.extend(owner.diags_const_param_types(db));
    diags.extend(owner.diags_params_defined_in_parent(db));
    diags.extend(owner.diags_kind_bounds(db));
    diags.extend(owner.diags_trait_bounds(db));
    diags.extend(owner.diags_non_trailing_defaults(db));
    diags.extend(owner.diags_default_forward_refs(db));
    diags
}

/// Analyze trait impl definition. Keeps early error handling and method conformance
/// glue while most checks are performed by semantic views.
#[salsa::tracked(return_ref)]
pub fn analyze_impl_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Vec<TyDiagCollection<'db>> {
    // Early path/domain/WF checks; bail out on errors to avoid noisy follow-ups
    let implementor = match analyze_impl_trait_specific_error(db, impl_trait) {
        Ok(implementor) => implementor,
        Err(diags) => return diags,
    };

    let mut diags = Vec::new();

    // Method conformance diagnostics
    diags.extend(analyze_impl_trait_method(
        db,
        implementor.instantiate_identity(),
    ));

    // Trait-ref WF and super-trait constraints
    diags.extend(impl_trait.diags_trait_ref_and_wf(db));

    // Associated types diagnostics (WF + presence + bounds)
    diags.extend(impl_trait.diags_assoc_types_wf(db));
    diags.extend(impl_trait.diags_missing_assoc_types(db));
    diags.extend(impl_trait.diags_assoc_types_bounds(db));

    // Generic parameter diagnostics
    let owner = GenericParamOwner::ImplTrait(impl_trait);
    diags.extend(owner.diags_params_defined_in_parent(db));
    diags.extend(owner.diags_kind_bounds(db));
    diags.extend(owner.diags_non_trailing_defaults(db));
    diags.extend(owner.diags_default_forward_refs(db));
    diags.extend(owner.diags_trait_bounds(db));

    diags
}

/// Shared helper to detect early impl-trait errors and return a lowered Implementor.
fn analyze_impl_trait_specific_error<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Result<Binder<Implementor<'db>>, Vec<TyDiagCollection<'db>>> {
    let mut diags = vec![];
    // No need to report parser errors here.
    let Some(trait_ref) = impl_trait.trait_ref(db).to_opt() else {
        return Err(diags);
    };
    // Early return if the implementor type is syntactically missing.
    if matches!(
        impl_trait.ty(db).data(db),
        TyData::Invalid(InvalidCause::ParseError)
    ) {
        return Err(diags);
    }

    // 1) Implementor type WF diagnostics at type span.
    let assumptions = collect_constraints(db, impl_trait.into()).instantiate_identity();
    let ty = impl_trait.ty(db);
    if let Some(diag) = ty.emit_diag(db, impl_trait.span().ty().into()) {
        diags.push(diag);
    }
    if !diags.is_empty() || ty.has_invalid(db) {
        return Err(diags);
    }

    // Lower the trait ref; map domain/path errors precisely.
    let trait_inst = match lower_trait_ref(db, ty, trait_ref, impl_trait.scope(), assumptions) {
        Ok(trait_inst) => trait_inst,
        Err(TraitRefLowerError::PathResError(err)) => {
            let trait_path_span = impl_trait.span().trait_ref().path();
            if let Some(diag) = err.into_diag(
                db,
                trait_ref.path(db).unwrap(),
                trait_path_span,
                ExpectedPathKind::Trait,
            ) {
                diags.push(diag.into());
            }
            return Err(diags);
        }
        Err(TraitRefLowerError::InvalidDomain(res)) => {
            diags.push(
                PathResDiag::ExpectedTrait(
                    impl_trait.span().trait_ref().path().into(),
                    trait_ref.path(db).unwrap().ident(db).unwrap(),
                    res.kind_name(),
                )
                .into(),
            );
            return Err(diags);
        }
        Err(TraitRefLowerError::Ignored) => return Err(diags),
    };

    // Ingot checks: impl must reside with type or trait.
    let impl_trait_ingot = impl_trait.top_mod(db).ingot(db);
    if Some(impl_trait_ingot) != ty.ingot(db) && impl_trait_ingot != trait_inst.def(db).ingot(db) {
        diags.push(TraitLowerDiag::ExternalTraitForExternalType(impl_trait).into());
        return Err(diags);
    }

    // Conflict check against existing implementors.
    let trait_env = ingot_trait_env(db, impl_trait_ingot);
    let Some(implementor) = trait_env.map_impl_trait(impl_trait) else {
        let current_impl = lower_impl_trait(db, impl_trait).unwrap();
        analyze_conflict_impl(db, current_impl, &mut diags);
        return Err(diags);
    };

    fn analyze_conflict_impl<'db>(
        db: &'db dyn HirAnalysisDb,
        implementor: Binder<Implementor<'db>>,
        diags: &mut Vec<TyDiagCollection<'db>>,
    ) {
        let trait_ = implementor.skip_binder().trait_(db);
        let env = ingot_trait_env(db, trait_.ingot(db));
        let Some(impls) = env.impls.get(&trait_.def(db)) else {
            return;
        };
        for cand in impls {
            if does_impl_trait_conflict(db, *cand, implementor) {
                diags.push(
                    TraitLowerDiag::ConflictTraitImpl {
                        primary: cand.skip_binder().hir_impl_trait(db),
                        conflict_with: implementor.skip_binder().hir_impl_trait(db),
                    }
                    .into(),
                );
                return;
            }
        }
    }

    // Kind-bound mismatch is reported explicitly.
    let expected_kind = implementor
        .instantiate_identity()
        .trait_def(db)
        .expected_implementor_kind(db);
    if ty.kind(db) != expected_kind {
        diags.push(
            TraitConstraintDiag::TraitArgKindMismatch {
                span: impl_trait.span().trait_ref(),
                expected: expected_kind.clone(),
                actual: implementor.instantiate_identity().self_ty(db),
            }
            .into(),
        );
        return Err(diags);
    }

    // Associated type presence/bounds are validated elsewhere; we add them if missing for completeness.
    diags.extend(impl_trait.diags_missing_assoc_types(db));
    diags.extend(impl_trait.diags_assoc_types_bounds(db));

    if diags.is_empty() {
        Ok(implementor)
    } else {
        Err(diags)
    }
}

/// Compare impl methods vs. trait methods and report missing/mismatched ones.
fn analyze_impl_trait_method<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: Implementor<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let mut diags = vec![];
    let impl_methods = implementor.methods(db);
    let hir_trait = implementor.trait_def(db).trait_(db);
    let trait_methods = implementor.trait_def(db).methods(db);
    let mut required_methods: IndexSet<_> = trait_methods
        .iter()
        .filter_map(|(name, &trait_method)| (!trait_method.has_default_impl(db)).then_some(*name))
        .collect();

    for (name, impl_m) in impl_methods {
        let Some(trait_m) = trait_methods.get(name) else {
            diags.push(
                ImplDiag::MethodNotDefinedInTrait {
                    primary: implementor.hir_impl_trait(db).span().trait_ref().into(),
                    method_name: *name,
                    trait_: hir_trait,
                }
                .into(),
            );
            continue;
        };
        compare_impl_method(db, *impl_m, *trait_m, implementor.trait_(db), &mut diags);
        required_methods.remove(name);
    }

    if !required_methods.is_empty() {
        diags.push(
            ImplDiag::NotAllTraitItemsImplemented {
                primary: implementor.hir_impl_trait(db).span().ty().into(),
                not_implemented: required_methods.into_iter().collect(),
            }
            .into(),
        );
    }

    diags
}

/// Shared helper for duplicate name diagnostics used by semantic views.
pub(crate) fn check_duplicate_names<'db, F>(
    names: impl Iterator<Item = Option<IdentId<'db>>>,
    create_diag: F,
) -> SmallVec<[TyDiagCollection<'db>; 2]>
where
    F: Fn(SmallVec<[u16; 4]>) -> TyDiagCollection<'db>,
{
    let mut defs = FxHashMap::<IdentId<'db>, SmallVec<[u16; 4]>>::default();
    for (i, name) in names.enumerate() {
        if let Some(name) = name {
            defs.entry(name).or_default().push(i as u16);
        }
    }
    defs.into_iter()
        .filter_map(|(_name, idxs)| (idxs.len() > 1).then_some(create_diag(idxs)))
        .collect()
}

// Minimal public struct retained for downstream diagnostics that refer to cycle members.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AdtCycleMember<'db> {
    pub adt: AdtDef<'db>,
    pub field_idx: u16,
    pub ty_idx: u16,
}
