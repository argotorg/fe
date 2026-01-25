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

pub mod reference;
pub use reference::{
    FieldAccessView, HasReferences, MethodCallView, PathView, ReferenceView, Target, UsePathView,
};

use crate::HirDb;
use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::canonical::Canonicalized;
use crate::analysis::ty::corelib::resolve_core_trait;
use crate::analysis::ty::diagnostics::{ImplDiag, TyLowerDiag};
use crate::analysis::ty::normalize::normalize_ty;
use crate::analysis::ty::ty_def::Kind;
use crate::analysis::ty::ty_error::collect_hir_ty_diags;
use crate::hir_def::params::KindBound as HirKindBound;
use crate::hir_def::scope_graph::ScopeId;
use rustc_hash::FxHashMap;

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
use crate::analysis::ty::effects::EffectKeyKind;
use crate::analysis::ty::trait_def::{
    ImplementorId, ImplementorOrigin, TraitInstId, does_impl_trait_conflict, ingot_trait_env,
};
use crate::analysis::ty::trait_lower::{TraitRefLowerError, lower_trait_ref};
use crate::analysis::ty::trait_resolution::constraint::{
    collect_adt_constraints, collect_constraints, collect_func_def_constraints,
};
use crate::analysis::ty::ty_def::{TyBase, TyData};
use crate::analysis::ty::ty_lower::{GenericParamTypeSet, collect_generic_params};
use crate::analysis::ty::visitor::{TyVisitor, walk_ty};
use crate::analysis::ty::{
    diagnostics::{TraitConstraintDiag, TyDiagCollection},
    trait_resolution::{
        GoalSatisfiability, PredicateListId, WellFormedness, check_ty_wf, is_goal_satisfiable,
    },
    ty_check::EffectParamSite,
    ty_def::{InvalidCause, PrimTy, TyId},
    ty_error::collect_ty_lower_errors,
    ty_lower::{
        TyAlias, lower_hir_ty, lower_opt_hir_ty, lower_type_alias, lower_type_alias_from_hir,
    },
};
use crate::core::adt_lower::{lower_adt, lower_contract_fields};
use common::indexmap::IndexMap;
use indexmap::IndexSet;
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
        use crate::analysis::ty::ty_def::{InvalidCause, TyId};
        let assumptions = self.assumptions(db);
        match self.params_list(db).to_opt() {
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

        let Some(hir_ty) = self
            .func
            .params_list(db)
            .to_opt()
            .and_then(|l| l.data(db).get(self.idx))
            .and_then(|p| p.ty.to_opt())
        else {
            return Vec::new();
        };

        // Surface name-resolution errors for the parameter type first
        let errs =
            collect_hir_ty_diags(db, func.scope(), hir_ty, self.lazy_ty_span(db), assumptions);
        if !errs.is_empty() {
            return errs;
        }

        let ty = lower_hir_ty(db, hir_ty, func.scope(), assumptions);
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

        // Well-formedness / trait-bound satisfaction for parameter type
        if let WellFormedness::IllFormed { goal, subgoal } =
            check_ty_wf(db, func.top_mod(db).ingot(db), ty, assumptions)
        {
            out.push(
                TraitConstraintDiag::TraitBoundNotSat {
                    span: ty_span.clone(),
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                }
                .into(),
            );
        }

        // Self-parameter type shape check
        if self.is_self_param(db)
            && let Some(expected) = func.expected_self_ty(db)
            && !ty.has_invalid(db)
            && !expected.has_invalid(db)
        {
            let (exp_base, exp_args) = expected.decompose_ty_app(db);
            let ty_norm = normalize_ty(db, ty, func.scope(), assumptions);
            let (ty_base, ty_args) = ty_norm.decompose_ty_app(db);
            let same_base = ty_base == exp_base;
            let same_args = exp_args.iter().zip(ty_args.iter()).all(|(a, b)| a == b);
            if !(same_base && same_args) {
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
        let assumptions = PredicateListId::empty_list(db);
        let recv = self.recv(db);
        let contract = recv.contract(db);

        let variant_ty = self.variant_ty(db);
        let selector_value = variant_struct_from_ty(db, variant_ty)
            .and_then(|s| get_variant_selector(db, s))
            .unwrap_or_default();

        let msg_variant_trait =
            resolve_core_trait(db, contract.scope(), &["message", "MsgVariant"]);
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
            selector_value,
            args_ty,
            ret_ty,
        }
    }

    #[salsa::tracked(return_ref)]
    pub fn arg_bindings(self, db: &'db dyn HirAnalysisDb) -> Vec<ArgBinding<'db>> {
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
    pub fn effective_effect_bindings(self, db: &'db dyn HirAnalysisDb) -> Vec<EffectBinding<'db>> {
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
        contract_scoped_effect_bindings(db, contract, site, arm.effects)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ContractFieldInfo<'db> {
    pub index: u32,
    pub name: IdentId<'db>,
    pub declared_ty: TyId<'db>,
    pub is_provider: bool,
    pub target_ty: TyId<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ArgBinding<'db> {
    pub pat: PatId,
    pub tuple_index: u32,
    pub ty: TyId<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum EffectSource {
    Root,
    Field(u32),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct EffectBinding<'db> {
    pub binding_name: IdentId<'db>,
    pub key_kind: EffectKeyKind,
    pub key_ty: Option<TyId<'db>>,
    pub key_trait: Option<TraitInstId<'db>>,
    pub is_mut: bool,
    pub source: EffectSource,
    pub binding_site: EffectParamSite<'db>,
    pub binding_idx: u32,
    pub binding_key_path: PathId<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct EffectEnvView<'db> {
    site: EffectParamSite<'db>,
}

impl<'db> EffectEnvView<'db> {
    pub fn site(self) -> EffectParamSite<'db> {
        self.site
    }

    pub fn bindings(self, db: &'db dyn HirAnalysisDb) -> &'db [EffectBinding<'db>] {
        match self.site {
            EffectParamSite::Contract(contract) => contract.effect_bindings(db).as_slice(),
            EffectParamSite::ContractInit { contract } => {
                contract.init_effect_bindings(db).as_slice()
            }
            EffectParamSite::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => {
                let recv = RecvView::new(db, contract, recv_idx);
                let arm = RecvArmView::new(db, recv, arm_idx);
                arm.effective_effect_bindings(db).as_slice()
            }
            EffectParamSite::Func(func) => func.effect_bindings(db).as_slice(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct RecvArmAbiInfo<'db> {
    pub selector_value: u32,
    pub args_ty: TyId<'db>,
    pub ret_ty: Option<TyId<'db>>,
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

    #[salsa::tracked(return_ref)]
    pub fn fields(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> IndexMap<IdentId<'db>, ContractFieldInfo<'db>> {
        let scope = self.top_mod(db).scope();
        let assumptions = PredicateListId::empty_list(db);
        let ingot = self.top_mod(db).ingot(db);

        let effect_ref = resolve_core_trait(db, scope, &["effect_ref", "EffectRef"]);
        let target_ident = IdentId::new(db, "Target".to_string());

        let hir_fields = self.hir_fields(db).data(db);

        hir_fields
            .iter()
            .filter(|field| field.name.is_present())
            .enumerate()
            .map(|(idx, field)| {
                let declared_ty = lower_opt_hir_ty(db, field.type_ref(), scope, assumptions);

                let inst = TraitInstId::new(db, effect_ref, vec![declared_ty], IndexMap::new());
                let goal = Canonicalized::new(db, inst).value;
                let (is_provider, target_ty) =
                    match is_goal_satisfiable(db, ingot, goal, assumptions) {
                        GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid => {
                            (false, None)
                        }
                        GoalSatisfiability::Satisfied(_)
                        | GoalSatisfiability::NeedsConfirmation(_) => (
                            true,
                            inst.assoc_ty(db, target_ident)
                                .map(|assoc| normalize_ty(db, assoc, scope, assumptions)),
                        ),
                    };

                let name = field.name.unwrap();
                (
                    name,
                    ContractFieldInfo {
                        index: idx as u32,
                        name,
                        declared_ty,
                        is_provider,
                        target_ty: target_ty.unwrap_or(declared_ty),
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
    pub fn effect_bindings(self, db: &'db dyn HirAnalysisDb) -> Vec<EffectBinding<'db>> {
        let assumptions = PredicateListId::empty_list(db);
        let contract_site = EffectParamSite::Contract(self);
        self.effects(db)
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
                    resolve_effect_key(db, key_path, self.scope(), assumptions);
                Some(EffectBinding {
                    binding_name,
                    key_kind,
                    key_ty,
                    key_trait,
                    is_mut: effect.is_mut,
                    source: EffectSource::Root,
                    binding_site: contract_site,
                    binding_idx: idx as u32,
                    binding_key_path: key_path,
                })
            })
            .collect()
    }

    #[salsa::tracked(return_ref)]
    pub fn init_effect_bindings(self, db: &'db dyn HirAnalysisDb) -> Vec<EffectBinding<'db>> {
        let Some(init) = self.init(db) else {
            return Vec::new();
        };
        contract_scoped_effect_bindings(
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
    pub fn effect_bindings(self, db: &'db dyn HirAnalysisDb) -> Vec<EffectBinding<'db>> {
        let assumptions = PredicateListId::empty_list(db);
        self.effects(db)
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
                    resolve_effect_key(db, key_path, self.scope(), assumptions);
                Some(EffectBinding {
                    binding_name,
                    key_kind,
                    key_ty,
                    key_trait,
                    is_mut: effect.is_mut,
                    source: EffectSource::Root,
                    binding_site: EffectParamSite::Func(self),
                    binding_idx: idx as u32,
                    binding_key_path: key_path,
                })
            })
            .collect()
    }
}

fn contract_effect_decl_map<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> FxHashMap<IdentId<'db>, (u32, PathId<'db>, bool)> {
    let mut out = FxHashMap::default();
    for (idx, effect) in contract.effects(db).data(db).iter().enumerate() {
        if let Some(name) = effect.name
            && let Some(key_path) = effect.key_path.to_opt()
        {
            out.insert(name, (idx as u32, key_path, effect.is_mut));
        }
    }
    out
}

fn contract_scoped_effect_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    list_site: EffectParamSite<'db>,
    list: EffectParamListId<'db>,
) -> Vec<EffectBinding<'db>> {
    let fields = contract.fields(db);
    let contract_named_effects = contract_effect_decl_map(db, contract);
    let assumptions = PredicateListId::empty_list(db);

    let mut out = Vec::new();
    for (idx, effect) in list.data(db).iter().enumerate() {
        let Some(key_path) = effect.key_path.to_opt() else {
            continue;
        };

        if let Some(binding_name) = effect.name {
            let (key_kind, key_ty, key_trait) =
                resolve_effect_key(db, key_path, contract.scope(), assumptions);
            out.push(EffectBinding {
                binding_name,
                key_kind,
                key_ty,
                key_trait,
                is_mut: effect.is_mut,
                source: EffectSource::Root,
                binding_site: list_site,
                binding_idx: idx as u32,
                binding_key_path: key_path,
            });
            continue;
        }

        if key_path.len(db) == 1
            && let Some(name) = key_path.ident(db).to_opt()
            && let Some(field) = fields.get(&name)
        {
            let field_idx = field.index;
            let target_ty = field.target_ty;

            out.push(EffectBinding {
                binding_name: name,
                key_kind: EffectKeyKind::Type,
                key_ty: Some(target_ty),
                key_trait: None,
                is_mut: effect.is_mut,
                source: EffectSource::Field(field_idx),
                binding_site: list_site,
                binding_idx: idx as u32,
                binding_key_path: key_path,
            });
            continue;
        }

        if key_path.len(db) == 1
            && let Some(name) = key_path.ident(db).to_opt()
            && let Some((binding_idx, referenced_key, is_mut)) =
                contract_named_effects.get(&name).copied()
        {
            let (key_kind, key_ty, key_trait) =
                resolve_effect_key(db, referenced_key, contract.scope(), assumptions);

            out.push(EffectBinding {
                binding_name: name,
                key_kind,
                key_ty,
                key_trait,
                is_mut,
                source: EffectSource::Root,
                binding_site: EffectParamSite::Contract(contract),
                binding_idx,
                binding_key_path: referenced_key,
            });
            continue;
        }

        // Unlabeled contract-scoped effects are either references (field/contract effect) or
        // invalid. Diagnostics are emitted by the type checker; keep lowering conservative.
        let binding_name = key_path
            .ident(db)
            .to_opt()
            .unwrap_or_else(|| IdentId::new(db, "_effect".to_string()));

        out.push(EffectBinding {
            binding_name,
            key_kind: EffectKeyKind::Other,
            key_ty: None,
            key_trait: None,
            is_mut: effect.is_mut,
            source: EffectSource::Root,
            binding_site: list_site,
            binding_idx: idx as u32,
            binding_key_path: key_path,
        });
    }

    out
}

fn resolve_effect_key<'db>(
    db: &'db dyn HirAnalysisDb,
    key_path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> (EffectKeyKind, Option<TyId<'db>>, Option<TraitInstId<'db>>) {
    use crate::analysis::name_resolution::{PathRes, resolve_path};

    match resolve_path(db, key_path, scope, assumptions, false) {
        Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) => (EffectKeyKind::Type, Some(ty), None),
        Ok(PathRes::Trait(inst)) => (EffectKeyKind::Trait, None, Some(inst)),
        _ => (EffectKeyKind::Other, None, None),
    }
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

fn get_variant_selector<'db>(db: &'db dyn HirAnalysisDb, struct_: Struct<'db>) -> Option<u32> {
    use crate::hir_def::{Expr, LitKind};
    use num_traits::ToPrimitive;

    let msg_variant_trait = resolve_core_trait(db, struct_.scope(), &["message", "MsgVariant"]);

    let adt_def = crate::analysis::ty::adt_def::AdtRef::from(struct_).as_adt(db);
    let ty = TyId::adt(db, adt_def);
    let canonical_ty = crate::analysis::ty::canonical::Canonical::new(db, ty);
    let ingot = struct_.top_mod(db).ingot(db);

    let impl_ = crate::analysis::ty::trait_def::impls_for_ty(db, ingot, canonical_ty)
        .iter()
        .find(|impl_| impl_.skip_binder().trait_def(db).eq(&msg_variant_trait))?
        .skip_binder();

    let selector_name = IdentId::new(db, "SELECTOR".to_string());
    let hir_impl = impl_.hir_impl_trait(db);
    let selector_const = hir_impl
        .hir_consts(db)
        .iter()
        .find(|c| c.name.to_opt() == Some(selector_name))?;

    let body = selector_const.value.to_opt()?;
    let root_expr = body.expr(db);
    let expr = body.exprs(db).get(root_expr)?.clone().to_opt()?;

    match expr {
        Expr::Lit(LitKind::Int(int_id)) => int_id.data(db).to_u32(),
        _ => None,
    }
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
        self.effect(db).key_path.to_opt()
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
        let assumptions = constraints_for(db, owner_item);
        let scope = owner_item.scope();
        lower_trait_ref(db, subject, self.trait_ref(db), scope, assumptions, None).ok()
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
        if let WellFormedness::IllFormed { goal, subgoal } =
            check_ty_wf(db, self.top_mod(db).ingot(db), ty, assumptions)
        {
            vec![
                TraitConstraintDiag::TraitBoundNotSat {
                    span: self.span().ty().into(),
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
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
        let tr = self.trait_ref(db);
        let expected = match lower_trait_ref(db, subject, tr, scope, assumptions, None) {
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
        let implementor = Binder::bind(ImplementorId::new(
            db,
            trait_inst,
            params,
            types,
            ImplementorOrigin::Hir(self),
        ));

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

        let trait_inst = lower_trait_ref(db, ty, trait_ref, self.scope(), assumptions, None)?;

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
        if let Some(diag) = ty.emit_diag(db, self.span().ty().into()) {
            return (None, vec![diag]);
        }
        if ty.has_invalid(db) {
            return (None, Vec::new());
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
                        TraitRefLowerError::Ignored => {
                            diags.push(TraitLowerDiag::ExternalTraitForExternalType(self).into());
                        }
                    },
                    ImplTraitLowerError::Conflict { primary, conflict } => {
                        diags.push(
                            TraitLowerDiag::ConflictTraitImpl {
                                primary,
                                conflict_with: conflict,
                            }
                            .into(),
                        );
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
        let ingot = self.owner.top_mod(db).ingot(db);
        if let WellFormedness::IllFormed { goal, subgoal } = check_ty_wf(db, ingot, ty, assumptions)
        {
            return vec![
                TraitConstraintDiag::TraitBoundNotSat {
                    span: ty_span.into(),
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
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

    /// Returns true if this associated const has a value defined.
    pub fn has_value(self, db: &'db dyn HirDb) -> bool {
        self.def(db).value.to_opt().is_some()
    }

    /// Semantic type of this associated const implementation.
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let hir = self.def(db).ty.to_opt()?;
        let assumptions = constraints_for(db, self.owner.into());
        Some(lower_hir_ty(db, hir, self.owner.scope(), assumptions))
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
        let assumptions = constraints_for(db, owner_item);
        if let WellFormedness::IllFormed { goal, subgoal } =
            check_ty_wf(db, owner_item.top_mod(db).ingot(db), ty, assumptions)
        {
            out.push(
                TraitConstraintDiag::TraitBoundNotSat {
                    span: span.clone(),
                    primary_goal: goal,
                    unsat_subgoal: subgoal,
                }
                .into(),
            );
            return out;
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
