//! Semantic traversal surface for HIR items.
//!
//! This module hosts the externally‑facing, semantic methods that callers
//! should use when walking the HIR. Keep raw, syntactic accessors and
//! #[salsa::tracked] implementations in `item.rs`; provide ergonomic,
//! context‑aware helpers here that compose the internal lowering and
//! resolution layers.
//!
//! Design notes
//! - Prefer returning semantic IDs (TyId, TraitInstId, etc.) or diagnostics
//!   collections from here; do not expose raw HIR nodes.
//! - Keep methods small and capability‑oriented (e.g., generic params,
//!   where‑clauses, signature types). Push per‑node, context‑rich logic into
//!   views when a single method signature becomes unwieldy.
//! - Let the compiler guide additions: ablate public syntactic accessors in
//!   `item.rs` and replace call sites by adding only the minimal semantic
//!   method(s) here.

use crate::analysis::HirAnalysisDb;
use crate::analysis::name_resolution::{PathRes, resolve_path};
use crate::analysis::ty::diagnostics::{TyDiagCollection, TyLowerDiag};
use crate::hir_def::*;
// When adding real methods, prefer calling internal lowering/normalization here
// rather than exposing raw syntax.
use crate::analysis::ty::trait_resolution::constraint::collect_func_def_constraints;
use crate::analysis::ty::{
    trait_resolution::{PredicateListId, constraint::collect_constraints},
    ty_def::{InvalidCause, TyId},
    ty_lower::{lower_hir_ty, lower_type_alias},
};

// Top‑level module items ----------------------------------------------------

impl<'db> TopLevelMod<'db> {
    // Planned semantic surface:
    // - Walk child items (semantic iteration helpers)
    // - Aggregate diagnostics for contained items (pure glue)
    // - Ingot/visibility helpers already live in `mod.rs`/`item.rs`
}

impl<'db> Mod<'db> {
    // Planned semantic surface:
    // - Semantic child iteration (filter by kind)
    // - Optional module‑scoped diagnostics aggregation
}

// Function items ------------------------------------------------------------

impl<'db> Func<'db> {
    // Planned semantic surface:
    // - arg_tys(db) -> Vec<Binder<TyId>> (delegates to internal lowering)
    // - receiver_ty(db) -> Option<Binder<TyId>>
    // - generic_params_diags(db) -> Vec<TyDiagCollection>
    // - where_clause_diags(db) -> Vec<TyDiagCollection>
    // - analyze(db) -> Vec<TyDiagCollection> (aggregate only)

    /// Returns true if this function declares an explicit return type.
    pub fn has_explicit_return_ty(self, db: &'db dyn HirDb) -> bool {
        self.ret_type_ref(db).is_some()
    }

    /// Explicit return type if annotated in source; `None` when the
    /// function has no explicit return type.
    pub fn explicit_return_ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let assumptions =
            collect_func_def_constraints(db, self.into(), true).instantiate_identity();
        let hir = self.ret_type_ref(db)?;
        Some(lower_hir_ty(db, hir, self.scope(), assumptions))
    }

    /// Semantic return type. When absent in source, this is `unit`.
    pub fn return_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        self.explicit_return_ty(db)
            .unwrap_or_else(|| TyId::unit(db))
    }
}

// ADT items -----------------------------------------------------------------

impl<'db> Struct<'db> {
    // Planned semantic surface:
    // - ty_fields(db) -> AdtField (semantic field tys)
    // - validate_fields(db) -> Vec<TyDiagCollection>
    // - generic_params_diags(db), where_clause_diags(db)
    // - analyze(db) -> Vec<TyDiagCollection>
}

impl<'db> Enum<'db> {
    // Planned semantic surface:
    // - variant arg types (semantic)
    // - validate_variants(db) -> Vec<TyDiagCollection>
    // - generic_params_diags(db), where_clause_diags(db)
    // - analyze(db) -> Vec<TyDiagCollection>
}

impl<'db> Contract<'db> {
    // Planned semantic surface:
    // - ty_fields(db) -> AdtField
    // - validate_fields(db) -> Vec<TyDiagCollection>
    // - analyze(db) -> Vec<TyDiagCollection>
}

// Type alias ---------------------------------------------------------------

impl<'db> TypeAlias<'db> {
    // Planned semantic surface:
    // - generic_params_diags(db) -> Vec<TyDiagCollection>

    /// Semantic alias target type (convenience over lower_type_alias).
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let ta = lower_type_alias(db, self);
        *ta.alias_to.skip_binder()
    }
}

// Trait / Impl items --------------------------------------------------------

impl<'db> Trait<'db> {
    // Planned semantic surface:
    // - assoc types default bounds (per‑bound views + diags)
    // - generic_params_diags(db), where_clause_diags(db)
    // - analyze(db) -> Vec<TyDiagCollection>
}

impl<'db> Impl<'db> {
    // Planned semantic surface:
    // - analyze_preconditions(db) -> Vec<TyDiagCollection>
    // - generic_params_diags(db), where_clause_diags(db)
    // - analyze(db) -> Vec<TyDiagCollection>

    /// Semantic implementor type of this inherent impl.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let assumptions = collect_constraints(db, self.into()).instantiate_identity();
        self.type_ref(db)
            .to_opt()
            .map(|hir_ty| lower_hir_ty(db, hir_ty, self.scope(), assumptions))
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::ParseError))
    }
}

impl<'db> ImplTrait<'db> {
    // Planned semantic surface:
    // - trait_inst(db) -> Result<TraitInstId, TraitRefLowerError>
    // - associated type values/defaults (semantic map)
    // - associated types diags (WF + lowering + satisfiability)
    // - generic_params_diags(db), where_clause_diags(db)
    // - analyze(db) -> Vec<TyDiagCollection>

    /// Semantic self type of this impl-trait block.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let assumptions = collect_constraints(db, self.into()).instantiate_identity();
        self.type_ref(db)
            .to_opt()
            .map(|hir_ty| lower_hir_ty(db, hir_ty, self.scope(), assumptions))
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::ParseError))
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

    pub fn diags_params_defined_in_parent(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = TyDiagCollection<'db>> + 'db {
        self.params(db).filter_map(|param| {
            param
                .diag_param_defined_in_parent(db)
                .map(TyDiagCollection::from)
        })
    }
}

impl<'db> GenericParamView<'db> {
    pub fn diag_param_defined_in_parent(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<TyLowerDiag<'db>> {
        let name = self.param.name().to_opt()?;
        let parent_scope = self.owner.scope().parent_item(db)?.scope();
        let path = PathId::from_ident(db, name);
        let span = self.owner.params_span().param(self.idx);

        match resolve_path(
            db,
            path,
            parent_scope,
            PredicateListId::empty_list(db),
            false,
        ) {
            Ok(r @ PathRes::Ty(ty)) if ty.is_param(db) => {
                Some(TyLowerDiag::GenericParamAlreadyDefinedInParent {
                    span,
                    conflict_with: r.name_span(db).unwrap(),
                    name,
                })
            }
            _ => None,
        }
    }
}
