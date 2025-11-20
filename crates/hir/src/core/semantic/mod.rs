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

use crate::HirDb;
use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::ty_def::Kind;
use crate::hir_def::params::KindBound as HirKindBound;
use crate::hir_def::scope_graph::ScopeId;

fn lower_hir_kind_local(k: &HirKindBound) -> Kind {
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
use crate::analysis::ty::def_analysis;
use crate::analysis::ty::diagnostics::TyDiagCollection;
use crate::hir_def::*;
// When adding real methods, prefer calling internal lowering/normalization here
// rather than exposing raw syntax.
use crate::analysis::ty::adt_def::{AdtDef, AdtField, AdtRef};
use crate::analysis::ty::trait_def::{
    ImplementorId, TraitInstId, does_impl_trait_conflict, ingot_trait_env,
};
use crate::analysis::ty::trait_lower::{
    TraitRefLowerError, lower_trait_ref, lower_trait_ref_with_owner_self,
};
use crate::analysis::ty::trait_resolution::constraint::{
    collect_adt_constraints, collect_constraints, collect_func_def_constraints,
    collect_super_traits, super_trait_cycle,
};
use crate::analysis::ty::ty_def::TyData;
use crate::analysis::ty::ty_lower::{GenericParamTypeSet, collect_generic_params};
use crate::analysis::ty::visitor::{TyVisitor, walk_ty};
use crate::analysis::ty::{
    trait_resolution::PredicateListId,
    ty_def::{InvalidCause, TyId},
    ty_lower::{TyAlias, lower_hir_ty, lower_type_alias, lower_type_alias_from_hir},
};
use crate::core::adt_lower::lower_adt;
use common::indexmap::IndexMap;
use either::Either;
// (kind-lowering helpers intentionally omitted; see WherePredicateView::kind)

// (Additional view types appear below; keep layering semantic.)

pub mod diagnostics;

// Note: generic trait-ref diagnostics are handled via context-rooted views (e.g.,
// WherePredicateBoundView and SuperTraitRefView). Avoid subject-taking public APIs.

/// Core-exposed entry point for alias lowering. Reads the HIR type_ref (core-visible)
/// and delegates to the analysis helper to keep visibility tight without shims.
pub(crate) fn lower_type_alias_body<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: TypeAlias<'db>,
) -> TyAlias<'db> {
    let hir_ty_opt = alias.type_ref(db).to_opt();
    lower_type_alias_from_hir(db, alias, hir_ty_opt)
}

/// Consolidated assumptions for any item kind.
pub(in crate::core) fn constraints_for<'db>(
    db: &'db dyn HirAnalysisDb,
    item: ItemKind<'db>,
) -> PredicateListId<'db> {
    match item {
        ItemKind::Struct(s) => collect_adt_constraints(db, s.as_adt(db)).instantiate_identity(),
        ItemKind::Enum(e) => collect_adt_constraints(db, e.as_adt(db)).instantiate_identity(),
        ItemKind::Contract(c) => collect_adt_constraints(db, c.as_adt(db)).instantiate_identity(),
        ItemKind::Func(f) => {
            collect_func_def_constraints(db, f.into(), true).instantiate_identity()
        }
        ItemKind::Impl(i) => collect_constraints(db, i.into()).instantiate_identity(),
        ItemKind::Trait(t) => collect_constraints(db, t.into()).instantiate_identity(),
        ItemKind::ImplTrait(i) => collect_constraints(db, i.into()).instantiate_identity(),
        _ => PredicateListId::empty_list(db),
    }
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
}

// Function items ------------------------------------------------------------

impl<'db> Func<'db> {
    /// Semantic predicate list (assumptions) for this function.
    pub fn assumptions(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        constraints_for(db, self.into())
    }

    /// Semantic predicate list extended with inherited trait bounds (super traits, assoc types).
    pub fn assumptions_with_bounds(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        self.assumptions(db).extend_all_bounds(db)
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
        use crate::analysis::ty::ty_def::{InvalidCause, TyId};
        let assumptions = self.assumptions(db);
        match self.params(db).to_opt() {
            Some(params) => params
                .data(db)
                .iter()
                .map(|p| match p.ty.to_opt() {
                    Some(hir_ty) => {
                        Binder::bind(lower_hir_ty(db, hir_ty, self.scope(), assumptions))
                    }
                    None => Binder::bind(TyId::invalid(db, InvalidCause::ParseError)),
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
    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        let list = self.func.params(db).to_opt()?;
        list.data(db).get(self.idx)?.name()
    }

    pub fn label(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        let list = self.func.params(db).to_opt()?;
        match list.data(db).get(self.idx)?.label {
            Some(FuncParamName::Ident(id)) => Some(id),
            _ => None,
        }
    }

    pub fn label_eagerly(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        let list = self.func.params(db).to_opt()?;
        list.data(db).get(self.idx)?.label_eagerly()
    }

    pub fn is_self_param(self, db: &'db dyn HirDb) -> bool {
        let list = self.func.params(db).to_opt();
        match list.and_then(|l| l.data(db).get(self.idx)) {
            Some(p) => p.is_self_param(db),
            None => false,
        }
    }

    pub fn is_mut(self, db: &'db dyn HirDb) -> bool {
        let list = self.func.params(db).to_opt();
        match list.and_then(|l| l.data(db).get(self.idx)) {
            Some(p) => p.is_mut,
            None => false,
        }
    }

    pub fn span(self) -> crate::span::params::LazyFuncParamSpan<'db> {
        self.func.span().params().param(self.idx)
    }

    fn hir_ty(self, db: &'db dyn HirDb) -> Option<TypeId<'db>> {
        let list = self.func.params(db).to_opt()?;
        list.data(db).get(self.idx)?.ty.to_opt()
    }

    pub fn self_ty_fallback(self, db: &'db dyn HirDb) -> bool {
        let list = self.func.params(db).to_opt();
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
        self.func.arg_tys(db)[self.idx].clone()
    }

    /// Semantic type of this parameter (binder removed).
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        *self.ty_binder(db).skip_binder()
    }
}

impl<'db> Func<'db> {
    /// Iterate parameters as contextual views (semantic traversal helper).
    pub fn param_views(self, db: &'db dyn HirDb) -> impl Iterator<Item = FuncParamView<'db>> + 'db {
        let len = self
            .params(db)
            .to_opt()
            .map(|l| l.data(db).len())
            .unwrap_or(0);
        (0..len).map(move |idx| FuncParamView { func: self, idx })
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
}

impl<'db> Contract<'db> {
    /// Returns semantic types of all fields, bound to identity parameters.
    pub fn field_tys(self, db: &'db dyn HirAnalysisDb) -> Vec<Binder<TyId<'db>>> {
        use crate::analysis::ty::ty_def::{InvalidCause, TyId};
        use crate::analysis::ty::ty_lower::lower_hir_ty;

        let scope = self.scope();
        // Contracts currently have no generic params/where-clause; use empty assumptions.
        let assumptions = PredicateListId::empty_list(db);
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

    /// Semantic ADT definition for this contract (cached via tracked query).
    pub fn as_adt(self, db: &'db dyn HirAnalysisDb) -> AdtDef<'db> {
        lower_adt(db, AdtRef::from(self))
    }
}

// Where-clause traversal (scaffold) -----------------------------------------

#[derive(Clone, Copy, Debug)]
pub struct WhereClauseView<'db> {
    owner: WhereClauseOwner<'db>,
    id: WhereClauseId<'db>,
}

#[derive(Clone, Copy, Debug)]
pub struct WherePredicateView<'db> {
    clause: WhereClauseView<'db>,
    idx: usize,
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
    /// Crate-local accessor for the underlying HIR predicate.
    /// Keep call sites inside this module; external users should rely on the
    /// higher-level semantic helpers on this view instead.
    fn hir_pred(self, db: &'db dyn HirDb) -> &'db WherePredicate<'db> {
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
        if !path.is_bare_ident(db) {
            return None;
        }
        let ident = path.as_ident(db)?;
        let Some(owner) = GenericParamOwner::from_item_opt(self.owner_item()) else {
            return None;
        };
        let params = owner.params_list(db).data(db);
        params.iter().position(|p| match p {
            GenericParam::Type(t) => t.name.to_opt() == Some(ident),
            GenericParam::Const(c) => c.name.to_opt() == Some(ident),
        })
    }

    /// Lowered kind bound sourced from the where-clause, if present.
    ///
    /// This inspects HIR kind-bounds on the predicate and maps them into the
    /// semantic `Kind` domain using a local lowering helper.
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
        let assumptions = constraints_for(db, owner_item);
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
}

#[derive(Clone, Copy, Debug)]
pub struct WherePredicateBoundView<'db> {
    pred: WherePredicateView<'db>,
    idx: usize,
}

impl<'db> WherePredicateBoundView<'db> {
    pub(crate) fn new(pred: WherePredicateView<'db>, idx: usize) -> Self {
        Self { pred, idx }
    }
    fn trait_ref(self, db: &'db dyn HirDb) -> TraitRefId<'db> {
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
    pub fn as_trait_inst(self, db: &'db dyn HirAnalysisDb) -> Option<TraitInstId<'db>> {
        let subject = self.pred.subject_ty(db)?;
        let owner_item = ItemKind::from(self.pred.clause.owner);
        let assumptions = constraints_for(db, owner_item);
        let scope = owner_item.scope();
        lower_trait_ref(db, subject, self.trait_ref(db), scope, assumptions).ok()
    }

    /// Lower this bound against an explicit subject type (useful for `Self` in trait contexts).
    pub fn as_trait_inst_with_subject(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: TyId<'db>,
    ) -> Option<TraitInstId<'db>> {
        let owner_item = ItemKind::from(self.pred.clause.owner);
        let assumptions = constraints_for(db, owner_item);
        let scope = owner_item.scope();
        lower_trait_ref(db, subject, self.trait_ref(db), scope, assumptions).ok()
    }
}

impl<'db> Contract<'db> {}

// Type alias ---------------------------------------------------------------

impl<'db> TypeAlias<'db> {
    /// Semantic alias target type (convenience over lower_type_alias).
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let ta = lower_type_alias(db, self);
        *ta.alias_to.skip_binder()
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

    pub fn method_defs(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> IndexMap<IdentId<'db>, CallableDef<'db>> {
        let mut methods = IndexMap::default();
        for method in self.methods(db) {
            if let Some(func) = method.as_callable(db) {
                if let Some(name) = func.name(db) {
                    methods.insert(name, func);
                }
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

    /// Semantic super-trait bounds of this trait, instantiated over the trait's own parameters.
    /// Returns an iterator of binders for each declared or implied super-trait.
    pub fn super_trait_insts(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = Binder<TraitInstId<'db>>> + 'db {
        if super_trait_cycle(db, self).is_some() {
            return Either::Left(std::iter::empty());
        }
        Either::Right(collect_super_traits(db, self).iter().copied())
    }

    /// Iterate declared super-trait references as contextual views.
    pub fn super_trait_refs(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = SuperTraitRefView<'db>> + 'db {
        let len = self.super_traits(db).len();
        (0..len).map(move |idx| SuperTraitRefView { owner: self, idx })
    }

    /// Aggregate trait definition diagnostics using semantic views.
    /// Includes:
    /// - Associated type default bound satisfaction
    /// - Super-trait constraint satisfaction
    /// - Generic parameter diagnostics (duplicates, defined-in-parent, kind/trait bounds,
    ///   non-trailing defaults, default forward references)
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();
        out.extend(self.diags_assoc_defaults(db));
        out.extend(self.diags_super_traits(db));

        let owner = GenericParamOwner::Trait(self);
        out.extend(owner.diags_params_defined_in_parent(db));
        out.extend(owner.diags_check_duplicate_names(db));
        out.extend(owner.diags_kind_bounds(db));
        out.extend(owner.diags_non_trailing_defaults(db));
        out.extend(owner.diags_default_forward_refs(db));
        out.extend(owner.diags_trait_bounds(db));
        // where-clause kind/trait bound diagnostics
        {
            let clause = WhereClauseView {
                owner: WhereClauseOwner::Trait(self),
                id: self.where_clause(db),
            };
            for pred in clause.predicates(db) {
                out.extend(pred.diags(db));
            }
        }
        out
    }
}

// ADT recursion (semantic) --------------------------------------------------

impl<'db> AdtDef<'db> {
    /// Detects a recursive ADT cycle that is not guarded by an indirect wrapper
    /// (e.g., pointer/reference). Returns the cycle members if the ADT is part
    /// of a cycle; otherwise returns None.
    pub fn recursive_cycle(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<Vec<def_analysis::AdtCycleMember<'db>>> {
        fn impl_check<'db>(
            db: &'db dyn HirAnalysisDb,
            adt: AdtDef<'db>,
            chain: &[def_analysis::AdtCycleMember<'db>],
        ) -> Option<Vec<def_analysis::AdtCycleMember<'db>>> {
            use def_analysis::AdtCycleMember;

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
    owner: Trait<'db>,
    idx: usize,
}

impl<'db> SuperTraitRefView<'db> {
    pub fn span(self) -> crate::span::params::LazyTraitRefSpan<'db> {
        self.owner.span().super_traits().super_trait(self.idx)
    }

    pub(in crate::core) fn trait_ref_id(self, db: &'db dyn HirDb) -> TraitRefId<'db> {
        self.owner.super_traits(db)[self.idx]
    }

    pub fn subject_self(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        collect_generic_params(db, self.owner.into())
            .trait_self(db)
            .unwrap()
    }

    pub(in crate::core) fn assumptions(self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        constraints_for(db, self.owner.into())
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
        let tr = self.trait_ref_id(db);
        let scope = self.owner.scope();
        let assumptions = self.assumptions(db);
        match crate::analysis::ty::trait_lower::lower_trait_ref(db, subject, tr, scope, assumptions)
        {
            Ok(v) => Ok(v),
            Err(TraitRefLowerError::PathResError(_)) => Err(SuperTraitLowerError::PathResolution),
            Err(TraitRefLowerError::InvalidDomain(_)) => Err(SuperTraitLowerError::InvalidDomain),
            Err(TraitRefLowerError::Ignored) => Err(SuperTraitLowerError::Ignored),
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
        let tr = self.trait_ref_id(db);
        let expected = match lower_trait_ref(db, subject, tr, scope, assumptions) {
            Ok(inst) => inst.def(db).self_param(db).kind(db).clone(),
            // If we cannot lower, defer to other diagnostics; do not emit mismatch here.
            Err(
                TraitRefLowerError::PathResError(_)
                | TraitRefLowerError::InvalidDomain(_)
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
    Ignored,
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

    /// Aggregate impl definition diagnostics using semantic views.
    /// Includes:
    /// - Preconditions and implementor type checks
    /// - Generic parameter diagnostics (duplicates, defined-in-parent, kind/trait bounds,
    ///   non-trailing defaults, default forward references)
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = self.diags_preconditions(db);
        let owner = GenericParamOwner::Impl(self);
        out.extend(owner.diags_params_defined_in_parent(db));
        out
    }
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

impl<'db> ImplTrait<'db> {
    /// Semantic self type of this impl-trait block.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let assumptions = constraints_for(db, self.into());
        self.type_ref(db)
            .to_opt()
            .map(|hir_ty| lower_hir_ty(db, hir_ty, self.scope(), assumptions))
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::ParseError))
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
        let implementor = Binder::bind(ImplementorId::new(db, trait_inst, params, types, self));

        // Conflict check
        let trait_ = implementor.skip_binder().trait_(db);
        let env = ingot_trait_env(db, trait_.ingot(db));
        if let Some(impls) = env.impls.get(&trait_.def(db)) {
            for &cand_view in impls {
                let cand_impl_trait = cand_view.skip_binder().hir_impl_trait(db);
                if cand_impl_trait == self {
                    continue;
                }
                if does_impl_trait_conflict(db, cand_view, implementor) {
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

        let trait_inst = lower_trait_ref(db, ty, trait_ref, self.scope(), assumptions)?;

        // Preserve ingot check used when lowering impl traits: an impl is
        // only valid if it lives in the same ingot as either its
        // implementor type or the trait itself.
        let impl_trait_ingot = self.top_mod(db).ingot(db);
        if Some(impl_trait_ingot) != ty.ingot(db)
            && impl_trait_ingot != trait_inst.def(db).ingot(db)
        {
            return Err(TraitRefLowerError::Ignored);
        }

        Ok(trait_inst)
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
            .filter_map(|v| v.name(db).and_then(|name| v.ty(db).map(|ty| (name, ty))))
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

    /// Iterate associated type definitions in this impl-trait block as views.
    pub fn assoc_types(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = ImplAssocTypeView<'db>> + 'db {
        let len = self.types(db).len();
        (0..len).map(move |idx| ImplAssocTypeView { owner: self, idx })
    }

    /// Diagnostics for missing associated types (required by the trait).
    /// Aggregate impl-trait definition diagnostics using semantic views.
    /// Includes:
    /// - Early trait-ref/implementor type checks and ingot rules
    /// - Method conformance and missing-items checks
    /// - Associated type checks (presence + bound satisfaction)
    /// - Generic parameter diagnostics (duplicates, defined-in-parent, kind/trait bounds,
    ///   non-trailing defaults, default forward references)
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut out = Vec::new();

        // Early trait-ref presence check: if there is no trait reference at
        // all, we do not report diagnostics from this entrypoint.
        let Some(trait_ref) = self.trait_ref(db).to_opt() else {
            return out;
        };

        // Implementor type diagnostics at the type span, keeping the existing
        // "parse error -> early return" behavior.
        let ty = self.ty(db);
        if matches!(ty.data(db), TyData::Invalid(InvalidCause::ParseError)) {
            return out;
        }
        if let Some(diag) =
            crate::analysis::ty::ty_error::emit_invalid_ty_error(db, ty, self.span().ty().into())
        {
            out.push(diag);
        }
        if ty.has_invalid(db) {
            return out;
        }

        // Early trait-ref path/domain checks via the shared lowering helper.
        use crate::analysis::name_resolution::{ExpectedPathKind, diagnostics::PathResDiag};
        match self.trait_inst_result(db) {
            Ok(_) => {}
            Err(TraitRefLowerError::PathResError(e)) => {
                if let Some(diag) = e.into_diag(
                    db,
                    trait_ref.path(db).unwrap(),
                    self.span().trait_ref().path(),
                    ExpectedPathKind::Trait,
                ) {
                    out.push(diag.into());
                }
                return out;
            }
            Err(TraitRefLowerError::InvalidDomain(res)) => {
                out.push(
                    PathResDiag::ExpectedTrait(
                        self.span().trait_ref().path().into(),
                        trait_ref.path(db).unwrap().ident(db).unwrap(),
                        res.kind_name(),
                    )
                    .into(),
                );
                return out;
            }
            Err(TraitRefLowerError::Ignored) => {
                // Either the trait-ref is intentionally ignored (e.g., for
                // diagnostics reported elsewhere) or the ingot check failed.
                // In both cases we return without emitting additional diags
                // from this entrypoint.
                return out;
            }
        }

        // Trait-ref WF and super-trait constraints
        out.extend(self.diags_trait_ref_and_wf(db));
        // Associated types (presence + bound satisfaction)
        out.extend(self.diags_missing_assoc_types(db));
        out.extend(self.diags_assoc_types_bounds(db));
        // Generic parameter diagnostics
        let owner = GenericParamOwner::ImplTrait(self);
        out.extend(owner.diags_params_defined_in_parent(db));
        out.extend(owner.diags_check_duplicate_names(db));
        out.extend(owner.diags_kind_bounds(db));
        out.extend(owner.diags_non_trailing_defaults(db));
        out.extend(owner.diags_default_forward_refs(db));
        out.extend(owner.diags_trait_bounds(db));
        out
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

    /// Semantic type of this associated type implementation.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let hir = self.owner.types(db)[self.idx].type_ref.to_opt()?;
        let assumptions = constraints_for(db, self.owner.into());
        Some(lower_hir_ty(db, hir, self.owner.scope(), assumptions))
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

    // Note: assoc-type bound evaluation is provided via AssocTypeOnSubjectView.
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

    /// Lower this associated-type bound to a trait instance for a given subject,
    /// using the owner-trait's `Self` inside generic arguments.
    pub fn as_trait_inst_with_subject(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: TyId<'db>,
    ) -> Option<TraitInstId<'db>> {
        let owner_trait = self.owner.owner;
        let owner_self = collect_generic_params(db, owner_trait.into())
            .trait_self(db)
            .unwrap();
        let scope = owner_trait.scope();
        let assumptions = constraints_for(db, owner_trait.into());
        lower_trait_ref_with_owner_self(
            db,
            subject,
            self.trait_ref(db),
            scope,
            assumptions,
            owner_self,
        )
        .ok()
    }

    /// Lower this bound with explicit owner `Self` for contexts like impl trait analysis.
    pub fn as_trait_inst_with_subject_and_owner(
        self,
        db: &'db dyn HirAnalysisDb,
        subject: TyId<'db>,
        owner_self: TyId<'db>,
    ) -> Option<TraitInstId<'db>> {
        let owner_trait = self.owner.owner;
        let scope = owner_trait.scope();
        let assumptions = constraints_for(db, owner_trait.into());
        lower_trait_ref_with_owner_self(
            db,
            subject,
            self.trait_ref(db),
            scope,
            assumptions,
            owner_self,
        )
        .ok()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct VariantView<'db> {
    owner: Enum<'db>,
    idx: usize,
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
    parent: FieldParent<'db>,
    idx: usize,
}

impl<'db> FieldView<'db> {
    pub fn name(self, db: &'db dyn HirDb) -> Option<IdentId<'db>> {
        let list = self.parent.fields_list(db);
        list.data(db)[self.idx].name.to_opt()
    }

    /// Crate-local helper returning the HIR type reference (syntactic) for
    /// this field. Prefer using `ty` when the semantic type is needed.
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
    pub fn as_adt_field(self, db: &'db dyn HirAnalysisDb) -> (&'db AdtField<'db>, usize) {
        (self.parent.as_adt_fields(db), self.idx)
    }

    pub fn ty_span(self) -> crate::span::DynLazySpan<'db> {
        match self.parent {
            FieldParent::Struct(s) => s.span().fields().field(self.idx).ty().into(),
            FieldParent::Contract(c) => c.span().fields().field(self.idx).ty().into(),
            FieldParent::Variant(v) => v.span().fields().field(self.idx).ty().into(),
        }
    }
}

impl<'db> FieldParent<'db> {
    /// Iterates fields as contextual views. For variants, only record variants have fields.
    pub fn fields(self, db: &'db dyn HirDb) -> impl Iterator<Item = FieldView<'db>> + 'db {
        let len = self.fields_list(db).data(db).len();
        (0..len).map(move |idx| FieldView { parent: self, idx })
    }

    /// Semantic field-set for this parent.
    pub fn as_adt_fields(self, db: &'db dyn HirAnalysisDb) -> &'db AdtField<'db> {
        match self {
            FieldParent::Struct(s) => &s.as_adt(db).fields(db)[0],
            FieldParent::Contract(c) => &c.as_adt(db).fields(db)[0],
            FieldParent::Variant(v) => v.as_adt_fields(db),
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
#[derive(Clone, Copy, Debug)]
pub struct AssocTypeOnSubjectView<'db> {
    base: TraitAssocTypeView<'db>,
    subject: TyId<'db>,
}

impl<'db> TraitAssocTypeView<'db> {
    /// Attach a subject type to this associated type for semantic bound evaluation.
    pub fn with_subject(self, subject: TyId<'db>) -> AssocTypeOnSubjectView<'db> {
        AssocTypeOnSubjectView {
            base: self,
            subject,
        }
    }
}

impl<'db> AssocTypeOnSubjectView<'db> {
    /// Semantic trait bounds for this associated type on `subject` using the owner's context.
    pub fn bounds(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = TraitInstId<'db>> + 'db {
        self.base
            .bounds(db)
            .filter_map(move |b| b.as_trait_inst_with_subject(db, self.subject))
    }
}
