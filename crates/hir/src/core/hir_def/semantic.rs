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
use crate::analysis::ty::def_analysis::check_duplicate_names;
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
    pub fn as_adt(self, db: &'db dyn HirAnalysisDb) -> crate::analysis::ty::adt_def::AdtDef<'db> {
        crate::analysis::ty::adt_def::lower_adt(
            db,
            crate::analysis::ty::adt_def::AdtRef::from(self),
        )
    }
}

impl<'db> Struct<'db> {
    /// Returns semantic types of all fields, bound to identity parameters.
    pub fn field_tys(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<crate::analysis::ty::binder::Binder<crate::analysis::ty::ty_def::TyId<'db>>> {
        use crate::analysis::ty::binder::Binder;
        FieldParent::Struct(self)
            .fields(db)
            .map(|v| Binder::bind(v.ty(db)))
            .collect()
    }

    /// Semantic ADT definition for this struct (cached via tracked query).
    pub fn as_adt(self, db: &'db dyn HirAnalysisDb) -> crate::analysis::ty::adt_def::AdtDef<'db> {
        crate::analysis::ty::adt_def::lower_adt(
            db,
            crate::analysis::ty::adt_def::AdtRef::from(self),
        )
    }
}

impl<'db> Contract<'db> {
    /// Returns semantic types of all fields, bound to identity parameters.
    pub fn field_tys(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<crate::analysis::ty::binder::Binder<crate::analysis::ty::ty_def::TyId<'db>>> {
        use crate::analysis::ty::binder::Binder;
        FieldParent::Contract(self)
            .fields(db)
            .map(|v| Binder::bind(v.ty(db)))
            .collect()
    }

    /// Semantic ADT definition for this contract (cached via tracked query).
    pub fn as_adt(self, db: &'db dyn HirAnalysisDb) -> crate::analysis::ty::adt_def::AdtDef<'db> {
        crate::analysis::ty::adt_def::lower_adt(
            db,
            crate::analysis::ty::adt_def::AdtRef::from(self),
        )
    }
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
    pub fn diags_check_duplicate_names(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = TyDiagCollection<'db>> + 'db {
        let params_iter = self
            .params_list(db)
            .data(db)
            .iter()
            .map(|p| p.name().to_opt());
        check_duplicate_names(params_iter, |idxs| {
            TyDiagCollection::from(TyLowerDiag::DuplicateGenericParamName(self, idxs))
        })
        .into_iter()
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

#[derive(Clone, Copy, Debug)]
pub struct VariantView<'db> {
    pub owner: Enum<'db>,
    pub idx: usize,
}

impl<'db> VariantView<'db> {
    pub fn kind(self, db: &'db dyn HirDb) -> VariantKind<'db> {
        self.owner.variants_list(db).data(db)[self.idx].kind
    }

    pub fn name(self, db: &'db dyn HirDb) -> Partial<IdentId<'db>> {
        self.owner.variants_list(db).data(db)[self.idx].name
    }

    pub fn span(self) -> crate::span::item::LazyVariantDefSpan<'db> {
        self.owner.span().variants().variant(self.idx)
    }

    /// Returns semantic types of this variant's fields (empty for unit variants).
    pub fn field_tys(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<crate::analysis::ty::binder::Binder<crate::analysis::ty::ty_def::TyId<'db>>> {
        use crate::analysis::ty::{
            adt_def::{AdtRef, lower_adt},
            binder::Binder,
            trait_resolution::constraint::collect_adt_constraints,
            ty_def::{InvalidCause, TyId},
            ty_lower::lower_hir_ty,
        };

        match self.kind(db) {
            VariantKind::Unit => Vec::new(),
            VariantKind::Record(_) => {
                let parent = FieldParent::Variant(EnumVariant::new(self.owner, self.idx));
                parent.fields(db).map(|v| Binder::bind(v.ty(db))).collect()
            }
            VariantKind::Tuple(tuple_id) => {
                let var = EnumVariant::new(self.owner, self.idx);
                let scope = var.scope();
                let adt = self.owner.as_adt(db);
                let assumptions = collect_adt_constraints(db, adt).instantiate_identity();
                tuple_id
                    .data(db)
                    .iter()
                    .map(|p| {
                        let ty = match p.to_opt() {
                            Some(hir_ty) => lower_hir_ty(db, hir_ty, scope, assumptions),
                            None => TyId::invalid(db, InvalidCause::ParseError),
                        };
                        Binder::bind(ty)
                    })
                    .collect()
            }
        }
    }

    /// Semantic field-set for this variant.
    pub fn as_adt_fields(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> &'db crate::analysis::ty::adt_def::AdtField<'db> {
        let def = crate::analysis::ty::adt_def::lower_adt(
            db,
            crate::analysis::ty::adt_def::AdtRef::from(self.owner),
        );
        &def.fields(db)[self.idx]
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

    /// Returns the HIR type reference (syntactic) for this field.
    /// Prefer using `ty` when the semantic type is needed.
    /// Kept private to avoid exposing raw HIR outside `core`.
    fn hir_type_ref(self, db: &'db dyn HirDb) -> Partial<TypeId<'db>> {
        let list = self.parent.fields_list(db);
        list.data(db)[self.idx].type_ref
    }

    /// Returns the semantic type of this field.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let (adt_field, idx) = self.as_adt_field(db);
        *adt_field.ty(db, idx).skip_binder()
    }

    /// Returns the semantic ADT field-set and index for this field.
    pub fn as_adt_field(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> (&'db crate::analysis::ty::adt_def::AdtField<'db>, usize) {
        (self.parent.as_adt_fields(db), self.idx)
    }
}

impl<'db> FieldParent<'db> {
    /// Iterates fields as contextual views. For variants, only record variants have fields.
    pub fn fields(self, db: &'db dyn HirDb) -> impl Iterator<Item = FieldView<'db>> + 'db {
        let len = self.fields_list(db).data(db).len();
        (0..len).map(move |idx| FieldView { parent: self, idx })
    }

    /// Semantic field-set for this parent.
    pub fn as_adt_fields(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> &'db crate::analysis::ty::adt_def::AdtField<'db> {
        match self {
            FieldParent::Struct(s) => &s.as_adt(db).fields(db)[0],
            FieldParent::Contract(c) => &c.as_adt(db).fields(db)[0],
            FieldParent::Variant(v) => v.as_adt_fields(db),
        }
    }
}

impl<'db> EnumVariant<'db> {
    /// Semantic field-set for this variant.
    pub fn as_adt_fields(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> &'db crate::analysis::ty::adt_def::AdtField<'db> {
        let def = crate::analysis::ty::adt_def::lower_adt(
            db,
            crate::analysis::ty::adt_def::AdtRef::from(self.enum_),
        );
        &def.fields(db)[self.idx as usize]
    }
}
