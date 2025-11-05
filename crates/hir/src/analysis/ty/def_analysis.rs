//! This module contains analysis for the definition of the type/trait.
//! This module is the only module in `ty` module which is allowed to emit
//! diagnostics.

use crate::{
    hir_def::{
        EnumVariant, FieldParent, Func, GenericParam, GenericParamListId, GenericParamOwner,
        IdentId, Impl as HirImpl, Impl, ImplTrait, ItemKind, PathId, Trait, TraitRefId, TypeAlias,
        TypeBound, TypeId as HirTyId, VariantKind, WhereClauseOwner, scope_graph::ScopeId,
    },
    visitor::prelude::*,
};
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec1::SmallVec;

use super::{
    adt_def::AdtRef,
    canonical::Canonical,
    diagnostics::{ImplDiag, TraitConstraintDiag, TraitLowerDiag, TyDiagCollection, TyLowerDiag},
    func_def::CallableDef,
    method_cmp::compare_impl_method,
    method_table::probe_method,
    normalize::normalize_ty,
    trait_lower::{TraitRefLowerError, collect_implementor_methods, lower_trait_ref},
    trait_resolution::{
        PredicateListId,
        constraint::{collect_adt_constraints, collect_func_def_constraints},
    },
    ty_def::{InvalidCause, TyData, TyId, TyParam},
    ty_error::collect_ty_lower_errors,
    ty_lower::{collect_generic_params, lower_kind},
    visitor::{TyVisitor, walk_ty},
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{ExpectedPathKind, PathRes, diagnostics::PathResDiag, resolve_path},
    ty::{
        binder::Binder,
        canonical::Canonicalized,
        trait_def::TraitInstId,
        trait_resolution::{
            GoalSatisfiability, constraint::super_trait_cycle, is_goal_satisfiable,
        },
        ty_lower::lower_hir_ty,
        visitor::TyVisitable,
    },
};

//

fn assumptions_for_owner<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> PredicateListId<'db> {
    // Single source of truth: derive assumptions from the owner's scope
    // using the centralized ScopeId::constraints helper.
    owner.scope().constraints(db)
}

pub(crate) fn validate_generic_params_for_owner<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> Vec<TyDiagCollection<'db>> {
    use crate::span::params::{LazyGenericParamListSpan, LazyGenericParamSpan};

    let mut diags = Vec::new();
    let params = owner.params(db);
    let scope = ItemKind::from(owner).scope();
    let gp_span: LazyGenericParamListSpan<'db> = owner.params_span();
    let assumptions = assumptions_for_owner(db, owner);

    // Duplicate generic parameter names
    diags.extend(check_duplicate_names(
        params.data(db).iter().map(|p| p.name().to_opt()),
        |idxs| TyLowerDiag::DuplicateGenericParamName(owner, idxs).into(),
    ));

    // Non-trailing default type parameters
    let mut default_idxs: SmallVec<[usize; 4]> = SmallVec::new();
    for (i, p) in params.data(db).iter().enumerate() {
        let is_defaulted_type = matches!(p, GenericParam::Type(tp) if tp.default_ty.is_some());
        if is_defaulted_type {
            default_idxs.push(i);
        } else if !default_idxs.is_empty() {
            for &idx in &default_idxs {
                let span = gp_span.clone().param(idx);
                diags.push(TyLowerDiag::NonTrailingDefaultGenericParam(span).into());
            }
            break;
        }
    }

    // Parent conflict + per-param checks
    for (i, param) in params.data(db).iter().enumerate() {
        let pspan: LazyGenericParamSpan<'db> = gp_span.clone().param(i);

        if let Some(diag) = check_param_defined_in_parent(db, scope, param, pspan.clone()) {
            diags.push(diag.into());
            continue;
        }

        match param {
            GenericParam::Type(tp) => {
                if let Some(default_ty) = tp.default_ty {
                    let lowered = lower_hir_ty(db, default_ty, scope, assumptions);

                    // Collect referenced generic params belonging to the same owner.
                    struct Collector<'db> {
                        db: &'db dyn HirAnalysisDb,
                        scope: ScopeId<'db>,
                        out: Vec<usize>,
                    }
                    impl<'db> TyVisitor<'db> for Collector<'db> {
                        fn db(&self) -> &'db dyn HirAnalysisDb {
                            self.db
                        }
                        fn visit_param(&mut self, tp: &TyParam<'db>) {
                            if !tp.is_trait_self() && tp.owner == self.scope {
                                self.out.push(tp.original_idx(self.db));
                            }
                        }
                        fn visit_const_param(&mut self, tp: &TyParam<'db>, _ty: TyId<'db>) {
                            if tp.owner == self.scope {
                                self.out.push(tp.original_idx(self.db));
                            }
                        }
                    }

                    let mut collector = Collector {
                        db,
                        scope,
                        out: Vec::new(),
                    };
                    lowered.visit_with(&mut collector);

                    // Forward reference check: cannot reference a param not yet declared.
                    for j in collector.out.iter().filter(|j| **j >= i) {
                        if let Some(name) = owner.param(db, *j).name().to_opt() {
                            diags.push(
                                TyLowerDiag::GenericDefaultForwardRef {
                                    span: pspan.clone(),
                                    name,
                                }
                                .into(),
                            );
                        }
                    }
                }
            }
            GenericParam::Const(_) => {
                // Validate const param type via the original TyId
                let param_set = collect_generic_params(db, owner);
                let original = param_set.param_by_original_idx(db, i);
                if let Some(ty) = original {
                    if let Some(const_ty_param) = ty.const_ty_param(db) {
                        if let Some(diag) =
                            const_ty_param.emit_diag(db, pspan.into_const_param().ty().into())
                        {
                            diags.push(diag);
                        }
                    }
                }
            }
        }
    }

    diags
}

/// Shared helpers for ADT field/variant validations
pub(crate) fn diag_term_or_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    hir_ty: HirTyId<'db>,
    span: crate::span::types::LazyTySpan<'db>,
) -> Option<TyDiagCollection<'db>> {
    let ty = lower_hir_ty(db, hir_ty, scope, assumptions);
    if !ty.has_star_kind(db) {
        return Some(TyLowerDiag::ExpectedStarKind(span.into()).into());
    }
    if ty.is_const_ty(db) {
        return Some(
            TyLowerDiag::NormalTypeExpected {
                span: span.into(),
                given: ty,
            }
            .into(),
        );
    }
    None
}

pub(crate) fn diag_const_param_mismatch<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    field_name: Option<IdentId<'db>>,
    actual_ty: TyId<'db>,
    span: crate::span::types::LazyTySpan<'db>,
) -> Option<TyDiagCollection<'db>> {
    let Some(name) = field_name else { return None };
    let path = PathId::from_ident(db, name);
    let Ok(PathRes::Ty(ty)) = resolve_path(db, path, scope, PredicateListId::empty_list(db), true)
    else {
        return None;
    };
    if let TyData::ConstTy(const_ty) = ty.data(db) {
        let expected = const_ty.ty(db);
        if !expected.has_invalid(db) && !actual_ty.has_invalid(db) && actual_ty != expected {
            return Some(
                TyLowerDiag::ConstTyMismatch {
                    span: span.into(),
                    expected,
                    given: actual_ty,
                }
                .into(),
            );
        }
    }
    None
}

/// Shared where-clause analyzer for any owner that implements `WhereClauseOwner`.
/// Callers provide the assumptions policy; this helper performs the common
/// lowering and bound checks and returns diagnostics.
pub(crate) fn analyze_where_clause_for_owner<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: WhereClauseOwner<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<TyDiagCollection<'db>> {
    use crate::span::params::{LazyTraitRefSpan, LazyWhereClauseSpan, LazyWherePredicateSpan};

    let mut diags = Vec::new();
    let scope = owner.scope();

    let where_clause = owner.where_clause(db);
    let wc_span: LazyWhereClauseSpan<'db> = owner.where_clause_span();

    for (i, pred) in where_clause.data(db).iter().enumerate() {
        let pred_span: LazyWherePredicateSpan<'db> = wc_span.clone().predicate(i);

        let Some(hir_ty) = pred.type_ref.to_opt() else {
            continue;
        };
        let ty = lower_hir_ty(db, hir_ty, scope, assumptions);

        // Surface type-lowering errors for the predicate type itself (e.g.,
        // unresolved names, wrong domain like trait/value where a type is expected).
        if ty.has_invalid(db) {
            let errs =
                collect_ty_lower_errors(db, scope, hir_ty, pred_span.clone().ty(), assumptions);
            if !errs.is_empty() {
                diags.extend(errs);
                continue;
            }
        }

        // Reject const types in where-clause bounds
        if ty.is_const_ty(db) {
            diags.push(TraitConstraintDiag::ConstTyBound(pred_span.clone().ty().into(), ty).into());
            continue;
        }

        // Reject fully concrete non-param types (no generics) in where-clauses
        if !ty.has_invalid(db) && !ty.has_param(db) {
            diags.push(
                TraitConstraintDiag::ConcreteTypeBound(pred_span.clone().ty().into(), ty).into(),
            );
            continue;
        }

        // Check bounds on the predicate
        for (j, bound) in pred.bounds.iter().enumerate() {
            match bound {
                TypeBound::Trait(trait_ref) => {
                    let tr_span: LazyTraitRefSpan<'db> =
                        pred_span.clone().bounds().bound(j).trait_bound();

                    // Surface type-lowering errors for generic type args inside the trait ref,
                    // e.g., `MyTWithGenerics<MyT>` where `MyT` is a trait (not a type).
                    if let Some(gargs) = trait_ref.generic_args(db) {
                        if let Some(path_id) = trait_ref.path(db).to_opt() {
                            let seg_idx = path_id.segment_index(db);
                            let ga_span = tr_span.clone().path().segment(seg_idx).generic_args();
                            for (k, garg) in gargs.data(db).iter().enumerate() {
                                if let crate::hir_def::GenericArg::Type(targ) = garg {
                                    if let Some(arg_ty) = targ.ty.to_opt() {
                                        let arg_span = ga_span.clone().arg(k).into_type_arg().ty();
                                        let errs = collect_ty_lower_errors(
                                            db,
                                            scope,
                                            arg_ty,
                                            arg_span,
                                            assumptions,
                                        );
                                        if !errs.is_empty() {
                                            diags.extend(errs);
                                            // Continue checking other args to gather all issues
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if let Some(diag) =
                        analyze_trait_ref(db, ty, *trait_ref, scope, assumptions, tr_span)
                    {
                        diags.push(diag);
                    }
                }
                TypeBound::Kind(kind_bound) => {
                    if let crate::hir_def::Partial::Present(k) = kind_bound {
                        let bound_kind = lower_kind(k);
                        let former_kind = ty.kind(db);
                        if !former_kind.does_match(&bound_kind) {
                            diags.push(
                                TyLowerDiag::InconsistentKindBound {
                                    span: pred_span.clone().bounds().bound(j).kind_bound().into(),
                                    ty,
                                    bound: bound_kind,
                                }
                                .into(),
                            );
                        }
                    }
                }
            }
        }
    }

    diags
}

fn check_duplicate_field_names<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: FieldParent<'db>,
) -> SmallVec<[TyDiagCollection<'db>; 2]> {
    check_duplicate_names(
        owner.fields(db).data(db).iter().map(|f| f.name.to_opt()),
        |idxs| TyLowerDiag::DuplicateFieldName(owner, idxs).into(),
    )
}

fn check_duplicate_variant_names<'db>(
    db: &'db dyn HirAnalysisDb,
    enum_: crate::hir_def::Enum<'db>,
) -> SmallVec<[TyDiagCollection<'db>; 2]> {
    check_duplicate_names(
        enum_.variants(db).data(db).iter().map(|v| v.name.to_opt()),
        |idxs| TyLowerDiag::DuplicateVariantName(enum_, idxs).into(),
    )
}

fn check_duplicate_names<'db, F>(
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

#[salsa::tracked]
impl<'db> Trait<'db> {
    // Private helper to build an analyzer with correct assumptions for this item.
    fn def_analyzer(self, db: &'db dyn HirAnalysisDb) -> DefAnalyzer<'db> {
        DefAnalyzer::for_def(db, DefKind::Trait(self))
    }
    /// This function implements analysis for the trait definition.
    /// The analysis includes the following:
    /// - Check if the types appear in the trait is well-formed.
    /// - Check if the trait instantiation appears in the trait is well-formed.
    /// - Check if the types in the trait satisfy the constraints which is required
    ///   in type application.
    /// - Check if the trait instantiations in the trait satisfies the constraints.
    #[salsa::tracked(return_ref)]
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let analyzer = self.def_analyzer(db);
        let mut diags = analyzer.analyze();

        // Check associated type defaults satisfy their bounds
        let assumptions = self.constraints(db);
        for assoc_type in self.types(db) {
            if let Some(default_ty) = self.assoc_type_default_ty(db, assoc_type) {
                // Check each bound on the associated type
                for bound in &assoc_type.bounds {
                    let TypeBound::Trait(trait_ref) = bound else {
                        continue;
                    };
                    // Lower the trait bound
                    let Ok(trait_inst) =
                        lower_trait_ref(db, default_ty, *trait_ref, self.scope(), assumptions)
                    else {
                        // Trait ref lowering error - will be reported elsewhere
                        continue;
                    };

                    // Check if the default type satisfies the trait bound
                    let canonical_inst = Canonical::new(db, trait_inst);
                    match is_goal_satisfiable(
                        db,
                        self.top_mod(db).ingot(db),
                        canonical_inst,
                        assumptions,
                    ) {
                        GoalSatisfiability::Satisfied(_) => continue,
                        GoalSatisfiability::UnSat(subgoal) => {
                            // Report error: default type doesn't satisfy the bound
                            // TODO: Get a better span for the default type
                            diags.push(
                                TraitConstraintDiag::TraitBoundNotSat {
                                    span: self.span().into(),
                                    primary_goal: trait_inst,
                                    unsat_subgoal: subgoal.map(|s| s.value),
                                }
                                .into(),
                            );
                        }
                        _ => {
                            // Other cases: NeedsConfirmation or ContainsInvalid
                            // These might warrant errors but we'll treat them as ok for now
                        }
                    }
                }
            }
        }

        // Analyze where-clause predicates owned by this trait
        diags.extend(self.analyze_where_clause(db).iter().cloned());
        // Generic parameter validation
        diags.extend(validate_generic_params_for_owner(
            db,
            GenericParamOwner::Trait(self),
        ));

        diags
    }

    /// Analyze the trait's where-clause using the shared helper.
    #[salsa::tracked(return_ref)]
    pub fn analyze_where_clause(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let assumptions = self.constraints(db);
        analyze_where_clause_for_owner(db, WhereClauseOwner::Trait(self), assumptions)
    }
}

// TODO: where does this documentation belong?  this functionality got factored out and moved into methods... find home(s) for it!
/// This function implements analysis for the trait implementation definition.
/// The analysis include the following:
/// - Check if the types appear in the trait impl is well-formed.
/// - Check if the trait instantiation appears in the trait impl is well-formed.
/// - Check if the types in the trait impl satisfy the constraints which is
///   required in type application.
/// - Check if the trait instantiations in the trait impl satisfies the
///   constraints.
/// - Check if the conflict doesn't occur.
/// - Check if the trait or type is included in the ingot which contains the
///   impl trait.

pub struct DefAnalyzer<'db> {
    db: &'db dyn HirAnalysisDb,
    def: DefKind<'db>,
    diags: Vec<TyDiagCollection<'db>>,
    assumptions: PredicateListId<'db>,
    current_ty: Option<(TyId<'db>, DynLazySpan<'db>)>,
}

impl<'db> DefAnalyzer<'db> {
    // Central constructor: derive assumptions from the def kind.
    fn for_def(db: &'db dyn HirAnalysisDb, def: DefKind<'db>) -> Self {
        let assumptions = match def {
            DefKind::Adt(adt) => collect_adt_constraints(db, adt).instantiate_identity(),
            _ => def.scope(db).constraints(db),
        };
        Self {
            db,
            def,
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    fn scope(&self) -> ScopeId<'db> {
        self.def.scope(self.db)
    }

    pub(crate) fn analyze(mut self) -> Vec<TyDiagCollection<'db>> {
        match self.def {
            DefKind::Adt(def) => match def {
                AdtRef::Struct(struct_) => {
                    let mut ctxt = VisitorCtxt::with_struct(self.db, struct_);
                    self.visit_struct(&mut ctxt, struct_);
                }

                AdtRef::Enum(enum_) => {
                    let mut ctxt = VisitorCtxt::with_enum(self.db, enum_);
                    self.visit_enum(&mut ctxt, enum_);
                }

                AdtRef::Contract(contract) => {
                    let mut ctxt = VisitorCtxt::with_contract(self.db, contract);
                    self.visit_contract(&mut ctxt, contract);
                }
            },

            DefKind::Trait(trait_) => {
                let mut ctxt = VisitorCtxt::with_trait(self.db, trait_);
                self.visit_trait(&mut ctxt, trait_);
            }

            DefKind::ImplTrait(impl_trait) => {
                let mut ctxt = VisitorCtxt::with_impl_trait(self.db, impl_trait);
                self.visit_impl_trait(&mut ctxt, impl_trait);
            }

            DefKind::Impl(hir_impl) => {
                let mut ctxt = VisitorCtxt::with_impl(self.db, hir_impl);
                self.visit_impl(&mut ctxt, hir_impl)
            }

            DefKind::Func(func) => {
                if let CallableDef::Func(hir_func) = func {
                    let mut ctxt = VisitorCtxt::with_func(self.db, hir_func);
                    self.visit_func(&mut ctxt, hir_func);
                }
            }

            DefKind::TypeAlias(type_alias) => {
                let mut ctxt = VisitorCtxt::with_type_alias(self.db, type_alias);
                self.visit_type_alias(&mut ctxt, type_alias);
            }
        }

        self.diags
    }
}

// Check if the same generic parameter is already defined in the parent item.
// Other name conflict check is done in the name resolution.
//
// This check is necessary because the conflict rule
// for the generic parameter is the exceptional case where shadowing shouldn't
// occur.
fn check_param_defined_in_parent<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    param: &GenericParam<'db>,
    span: LazyGenericParamSpan<'db>,
) -> Option<TyLowerDiag<'db>> {
    let name = param.name().to_opt()?;
    let parent_scope = scope.parent_item(db)?.scope();
    let path = PathId::from_ident(db, name);

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

impl<'db> Visitor<'db> for DefAnalyzer<'db> {
    // We don't need to traverse the nested item, each item kinds are explicitly
    // handled(e.g, `visit_trait` or `visit_enum`).
    fn visit_item(&mut self, _ctxt: &mut VisitorCtxt<'db, LazyItemSpan>, _item: ItemKind<'db>) {}

    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'db, LazyTySpan<'db>>, hir_ty: HirTyId<'db>) {
        let ty = lower_hir_ty(self.db, hir_ty, self.scope(), self.assumptions);
        let span = ctxt.span().unwrap();

        if ty.has_invalid(self.db) {
            let diags = collect_ty_lower_errors(
                self.db,
                ctxt.scope(),
                hir_ty,
                span.clone(),
                self.assumptions,
            );
            if !diags.is_empty() {
                self.diags.extend(diags);
                return;
            }
        }
        if let Some(diag) = ty.emit_wf_diag(self.db, ctxt.ingot(), self.assumptions, span.into()) {
            self.diags.push(diag);
        }
    }

    fn visit_where_predicate(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyWherePredicateSpan<'db>>,
        pred: &crate::hir_def::WherePredicate<'db>,
    ) {
        // Where-clause analysis for Trait/ImplTrait/Func/Impl/Struct/Enum has been moved to
        // item methods (analyze_where_clause). Skip visitor-based emission to
        // avoid duplicate diagnostics.
        match self.def {
            DefKind::Trait(_)
            | DefKind::ImplTrait(_)
            | DefKind::Func(_)
            | DefKind::Impl(_)
            | DefKind::Adt(AdtRef::Struct(_))
            | DefKind::Adt(AdtRef::Enum(_))
            | DefKind::Adt(AdtRef::Contract(_)) => return,
            _ => {}
        }
        walk_where_predicate(self, ctxt, pred);
    }

    fn visit_generic_param_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyGenericParamListSpan<'db>>,
        params: GenericParamListId<'db>,
    ) {
        // For function owners (including nested functions), keep visitor-based
        // validations (duplicates, non-trailing defaults). For other owners,
        // item methods own these diagnostics; only walk to visit nested nodes.
        let owner = GenericParamOwner::from_item_opt(self.scope().item()).unwrap();
        if matches!(owner, GenericParamOwner::Func(_)) {
            self.diags.extend(check_duplicate_names(
                params.data(self.db).iter().map(|p| p.name().to_opt()),
                |idxs| TyLowerDiag::DuplicateGenericParamName(owner, idxs).into(),
            ));

            let mut default_idxs: SmallVec<[usize; 4]> = SmallVec::new();
            for (i, p) in params.data(self.db).iter().enumerate() {
                let is_defaulted_type =
                    matches!(p, GenericParam::Type(tp) if tp.default_ty.is_some());
                if is_defaulted_type {
                    default_idxs.push(i);
                } else if !default_idxs.is_empty() {
                    for &idx in &default_idxs {
                        let span = ctxt.span().unwrap().clone().param(idx);
                        self.diags
                            .push(TyLowerDiag::NonTrailingDefaultGenericParam(span).into());
                    }
                    break;
                }
            }
        }
        walk_generic_param_list(self, ctxt, params);
    }

    fn visit_generic_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyGenericParamSpan<'db>>,
        param: &GenericParam<'db>,
    ) {
        let ScopeId::GenericParam(_, idx) = ctxt.scope() else {
            unreachable!()
        };

        // Owner-level param validations (parent conflict, defaults/forward-refs, const param type)
        // are handled in item methods to avoid duplicates. Keep walking to visit kind bounds.
        let skip_param_validations = matches!(
            self.def,
            DefKind::Adt(_)
                | DefKind::Trait(_)
                | DefKind::ImplTrait(_)
                | DefKind::Impl(_)
                | DefKind::TypeAlias(_)
        );

        match param {
            GenericParam::Type(tp) => {
                if !skip_param_validations {
                    if let Some(default_ty) = tp.default_ty {
                        let lowered =
                            lower_hir_ty(self.db, default_ty, self.scope(), self.assumptions);

                        // Collect referenced generic params belonging to the same owner.
                        struct Collector<'db> {
                            db: &'db dyn HirAnalysisDb,
                            scope: ScopeId<'db>,
                            out: Vec<usize>,
                        }
                        impl<'db> TyVisitor<'db> for Collector<'db> {
                            fn db(&self) -> &'db dyn HirAnalysisDb {
                                self.db
                            }
                            fn visit_param(&mut self, tp: &TyParam<'db>) {
                                if !tp.is_trait_self() && tp.owner == self.scope {
                                    self.out.push(tp.original_idx(self.db));
                                }
                            }
                            fn visit_const_param(&mut self, tp: &TyParam<'db>, _ty: TyId<'db>) {
                                if tp.owner == self.scope {
                                    self.out.push(tp.original_idx(self.db));
                                }
                            }
                        }

                        let mut collector = Collector {
                            db: self.db,
                            scope: self.scope(),
                            out: Vec::new(),
                        };
                        lowered.visit_with(&mut collector);

                        let owner = GenericParamOwner::from_item_opt(self.scope().item()).unwrap();

                        // Forward reference check: cannot reference a param not yet declared.
                        for j in collector.out.iter().filter(|j| **j >= idx as usize) {
                            if let Some(name) = owner.param(self.db, *j).name().to_opt() {
                                let span = ctxt.span().unwrap();
                                self.diags.push(
                                    TyLowerDiag::GenericDefaultForwardRef { span, name }.into(),
                                );
                            }
                        }
                    }
                }

                self.current_ty = Some((
                    self.def.original_params(self.db)[idx as usize],
                    ctxt.span().unwrap().into_type_param().name().into(),
                ));
            }
            GenericParam::Const(_) => {
                if !skip_param_validations {
                    let ty = self.def.original_params(self.db)[idx as usize];
                    let Some(const_ty_param) = ty.const_ty_param(self.db) else {
                        return;
                    };

                    if let Some(diag) = const_ty_param
                        .emit_diag(self.db, ctxt.span().unwrap().into_const_param().ty().into())
                    {
                        self.diags.push(diag)
                    }
                }
            }
        }
        walk_generic_param(self, ctxt, param)
    }

    fn visit_kind_bound(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyKindBoundSpan<'db>>,
        bound: &crate::hir_def::KindBound,
    ) {
        let Some((ty, _)) = self.current_ty else {
            return;
        };

        let kind = lower_kind(bound);
        let former_kind = ty.kind(self.db);
        if !former_kind.does_match(&kind) {
            self.diags.push(
                TyLowerDiag::InconsistentKindBound {
                    span: ctxt.span().unwrap().into(),
                    ty,
                    bound: kind,
                }
                .into(),
            );
        }
    }

    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTraitRefSpan<'db>>,
        trait_ref: TraitRefId<'db>,
    ) {
        let current_ty = self
            .current_ty
            .as_ref()
            .map(|(ty, _)| *ty)
            .unwrap_or(TyId::invalid(self.db, InvalidCause::Other));

        if current_ty.is_trait_self(self.db) && self.def.super_trait_cycle(self.db).is_some() {
            // Skip analysis of traits involved in cycles.
            return;
        }

        if let (Some((ty, _)), Ok(trait_inst)) = (
            &self.current_ty,
            lower_trait_ref(
                self.db,
                current_ty,
                trait_ref,
                self.scope(),
                self.assumptions,
            ),
        ) {
            let expected_kind = trait_inst.def(self.db).expected_implementor_kind(self.db);
            if !expected_kind.does_match(ty.kind(self.db)) {
                self.diags.push(
                    TraitConstraintDiag::TraitArgKindMismatch {
                        span: ctxt.span().unwrap(),
                        expected: expected_kind.clone(),
                        actual: *ty,
                    }
                    .into(),
                );
            }
        }

        if let Some(diag) = analyze_trait_ref(
            self.db,
            current_ty,
            trait_ref,
            self.scope(),
            self.assumptions,
            ctxt.span().unwrap(),
        ) {
            self.diags.push(diag);
        } else {
            walk_trait_ref(self, ctxt, trait_ref);
        }
    }

    fn visit_super_trait_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, crate::span::item::LazySuperTraitListSpan<'db>>,
        super_traits: &[TraitRefId<'db>],
    ) {
        let DefKind::Trait(def) = self.def else {
            unreachable!()
        };
        let name_span = def.span().name().into();
        self.current_ty = Some((self.def.trait_self_param(self.db), name_span));
        walk_super_trait_list(self, ctxt, super_traits);
    }

    fn visit_func(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFuncSpan<'db>>,
        hir_func: crate::hir_def::Func<'db>,
    ) {
        let Some(_name) = hir_func.name(self.db).to_opt() else {
            return;
        };
        let func = CallableDef::Func(hir_func);

        // Skip the rest of the analysis if any param names conflict with a parent's param
        let span = hir_func.span().generic_params();
        let params = hir_func.generic_params(self.db).data(self.db);
        let mut is_conflict = false;
        for (i, param) in params.iter().enumerate() {
            if let Some(diag) =
                check_param_defined_in_parent(self.db, self.scope(), param, span.clone().param(i))
            {
                self.diags.push(diag.into());
                is_conflict = true;
            }
        }
        if is_conflict {
            return;
        }

        let def = std::mem::replace(&mut self.def, func.into());
        let constraints =
            std::mem::replace(&mut self.assumptions, hir_func.scope().constraints(self.db));

        walk_func(self, ctxt, hir_func);

        self.assumptions = constraints;
        self.def = def;
    }

    fn visit_body(
        &mut self,
        _ctxt: &mut VisitorCtxt<'_, LazyBodySpan>,
        _body: crate::hir_def::Body,
    ) {
    }
}

#[salsa::tracked(return_ref)]
pub(crate) fn check_recursive_adt<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtRef<'db>,
) -> Option<Vec<AdtCycleMember<'db>>> {
    check_recursive_adt_impl(db, adt, &[])
}

pub(crate) fn check_recursive_adt_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtRef<'db>,
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
            for field_adt_ref in ty.instantiate_identity().collect_direct_adts(db) {
                chain.push(AdtCycleMember {
                    adt,
                    field_idx: field_idx as u16,
                    ty_idx: ty_idx as u16,
                });

                if let Some(cycle) = check_recursive_adt_impl(db, field_adt_ref, &chain)
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AdtCycleMember<'db> {
    pub adt: AdtRef<'db>,
    pub field_idx: u16,
    pub ty_idx: u16,
}

impl<'db> TyId<'db> {
    /// Collect all adts inside types which are not wrapped by indirect type
    /// wrapper like pointer or reference.
    fn collect_direct_adts(self, db: &'db dyn HirAnalysisDb) -> FxHashSet<AdtRef<'db>> {
        struct AdtCollector<'db> {
            db: &'db dyn HirAnalysisDb,
            adts: FxHashSet<AdtRef<'db>>,
        }

        impl<'db> TyVisitor<'db> for AdtCollector<'db> {
            fn db(&self) -> &'db dyn HirAnalysisDb {
                self.db
            }

            fn visit_app(&mut self, abs: TyId<'db>, arg: TyId<'db>) {
                if !abs.is_indirect(self.db) {
                    walk_ty(self, arg)
                }
            }

            fn visit_adt(&mut self, adt: AdtRef<'db>) {
                self.adts.insert(adt);
            }
        }

        let mut collector = AdtCollector {
            db,
            adts: FxHashSet::default(),
        };

        self.visit_with(&mut collector);
        collector.adts
    }
}

#[salsa::tracked]
impl<'db> ImplTrait<'db> {
    /// Validate `impl trait` preconditions (lowering, scope/ingot checks, and conflict/kind checks)
    /// and return the binder-wrapped implementor on success, or diagnostics on failure.
    pub fn analyze_preconditions(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<Binder<ImplTrait<'db>>, Vec<TyDiagCollection<'db>>> {
        let mut diags = vec![];
        let ty = self.ty(db);

        // 1) Implementor well-formedness (except satisfiability)
        if let Some(diag) = ty.emit_diag(db, self.span().ty().into()) {
            diags.push(diag);
        }
        if !diags.is_empty() || ty.has_invalid(db) {
            return Err(diags);
        }

        // 2) Lower trait ref with errors mapped to diags
        let trait_inst = match self.trait_inst(db) {
            Ok(trait_inst) => trait_inst,
            Err(TraitRefLowerError::PathResError(err)) => {
                let trait_path_span = self.span().trait_ref().path();
                let Some(trait_ref) = self.trait_ref(db).to_opt() else {
                    return Err(diags);
                };
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
            Err(TraitRefLowerError::InvalidDomain(_)) | Err(TraitRefLowerError::Ignored) => {
                return Err(diags);
            }
        };

        // 3) Ingot ownership checks
        let impl_trait_ingot = self.top_mod(db).ingot(db);
        if Some(impl_trait_ingot) != ty.ingot(db)
            && impl_trait_ingot != trait_inst.def(db).ingot(db)
        {
            diags.push(TraitLowerDiag::ExternalTraitForExternalType(self).into());
            return Err(diags);
        }

        // 4) Conflict check
        let current_impl = Binder::bind(self);
        analyze_conflict_impl(db, current_impl, &mut diags);

        // 5) Implementor kind check
        let expected_kind = current_impl
            .skip_binder()
            .trait_def(db)
            .unwrap()
            .expected_implementor_kind(db);
        if ty.kind(db) != expected_kind {
            let actual_ty = current_impl.skip_binder().ty(db);
            diags.push(
                TraitConstraintDiag::TraitArgKindMismatch {
                    span: self.span().trait_ref(),
                    expected: expected_kind.clone(),
                    actual: actual_ty,
                }
                .into(),
            );
            return Err(diags);
        }

        // 6) WF + super trait constraints satisfiability
        let trait_def = trait_inst.def(db);
        let trait_constraints =
            Binder::bind(trait_def.scope().constraints(db)).instantiate(db, trait_inst.args(db));
        let assumptions = current_impl.skip_binder().constraints(db);
        let is_satisfied = |goal: TraitInstId<'db>, span: DynLazySpan<'db>, diags: &mut Vec<_>| {
            let canonical_goal = Canonicalized::new(db, goal);
            match is_goal_satisfiable(db, impl_trait_ingot, canonical_goal.value, assumptions) {
                GoalSatisfiability::Satisfied(_) | GoalSatisfiability::ContainsInvalid => {}
                GoalSatisfiability::NeedsConfirmation(_) => unreachable!(),
                GoalSatisfiability::UnSat(subgoal) => {
                    diags.push(
                        TraitConstraintDiag::TraitBoundNotSat {
                            span,
                            primary_goal: goal,
                            unsat_subgoal: subgoal.map(|subgoal| subgoal.value),
                        }
                        .into(),
                    );
                }
            }
        };

        let trait_ref_span: DynLazySpan = self.span().trait_ref().into();
        for &goal in trait_constraints.list(db) {
            is_satisfied(goal, trait_ref_span.clone(), &mut diags);
        }
        let target_ty_span: DynLazySpan = self.span().ty().into();
        for &super_trait in trait_def.super_trait_insts(db) {
            let super_trait = super_trait.instantiate(db, trait_inst.args(db));
            is_satisfied(super_trait, target_ty_span.clone(), &mut diags)
        }

        // 7) Associated types presence + bounds
        let impl_types = self.assoc_types(db);
        for assoc_type in trait_def.types(db) {
            let Some(name) = assoc_type.name.to_opt() else {
                continue;
            };
            let impl_ty = impl_types.get(&name);
            if impl_ty.is_none() && assoc_type.default.is_none() {
                diags.push(
                    ImplDiag::MissingAssociatedType {
                        primary: self.span().ty().into(),
                        type_name: name,
                        trait_: trait_def,
                    }
                    .into(),
                );
            }
            let Some(&impl_ty) = impl_ty else {
                continue;
            };

            for bound in &assoc_type.bounds {
                if let TypeBound::Trait(trait_ref) = bound {
                    match lower_trait_ref(db, impl_ty, *trait_ref, self.scope(), assumptions) {
                        Ok(bound_inst) => {
                            let canonical_bound = Canonical::new(db, bound_inst);
                            if let GoalSatisfiability::UnSat(subgoal) = is_goal_satisfiable(
                                db,
                                self.top_mod(db).ingot(db),
                                canonical_bound,
                                assumptions,
                            ) {
                                let assoc_ty_span = self
                                    .associated_type_span(db, name)
                                    .map(|s| s.ty().into())
                                    .unwrap_or_else(|| trait_ref_span.clone());
                                diags.push(
                                    TraitConstraintDiag::TraitBoundNotSat {
                                        span: assoc_ty_span,
                                        primary_goal: bound_inst,
                                        unsat_subgoal: subgoal.map(|s| s.value),
                                    }
                                    .into(),
                                );
                            }
                        }
                        Err(_) => {}
                    }
                }
            }
        }

        if diags.is_empty() {
            Ok(current_impl)
        } else {
            Err(diags)
        }
    }
    /// Analyze the `where`-clause of this `impl trait`, producing diagnostics
    /// for invalid predicate types and trait bounds.
    #[salsa::tracked(return_ref)]
    pub fn analyze_where_clause(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let assumptions = self.constraints(db);
        analyze_where_clause_for_owner(db, WhereClauseOwner::ImplTrait(self), assumptions)
    }

    /// Compare the methods in this `impl trait` with the trait methods and
    /// emit diagnostics for any mismatches or missing methods.
    #[salsa::tracked(return_ref)]
    pub fn analyze_methods(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use common::indexmap::IndexSet;
        let mut diags = vec![];

        // If the trait ref is invalid, skip method comparison
        let Some(hir_trait) = self.trait_def(db) else {
            return diags;
        };

        let impl_methods = collect_implementor_methods(db, self);
        let trait_methods = hir_trait.methods(db);

        // Required = trait methods without default impl
        let mut required_methods: IndexSet<_> = trait_methods
            .iter()
            .filter_map(|(name, trait_m)| (!trait_m.has_default_impl(db)).then_some(*name))
            .collect();

        for (name, impl_m) in impl_methods.iter() {
            let Some(trait_m) = trait_methods.get(name) else {
                diags.push(
                    ImplDiag::MethodNotDefinedInTrait {
                        primary: self.span().trait_ref().into(),
                        method_name: *name,
                        trait_: hir_trait,
                    }
                    .into(),
                );
                continue;
            };

            let Some(trait_inst) = self.trait_inst(db).ok() else {
                continue;
            };
            compare_impl_method(db, *impl_m, *trait_m, trait_inst, &mut diags);
            required_methods.remove(name);
        }

        if !required_methods.is_empty() {
            diags.push(
                ImplDiag::NotAllTraitItemsImplemented {
                    primary: self.span().ty().into(),
                    not_implemented: required_methods.into_iter().collect(),
                }
                .into(),
            );
        }

        diags
    }

    /// Analyze associated types defined in this impl-trait: emit lowering errors
    /// and WF diagnostics for each associated type value.
    #[salsa::tracked(return_ref)]
    pub fn analyze_associated_types(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TyDiagCollection<'db>> {
        let mut diags = vec![];
        let assumptions = self.constraints(db);
        for assoc_type in self.types(db) {
            if let Some(ty) = assoc_type.type_ref.to_opt() {
                let ty_span = assoc_type
                    .name
                    .to_opt()
                    .and_then(|name| self.associated_type_span(db, name))
                    .map(|s| s.ty())
                    .unwrap_or_else(|| self.span().ty());

                let lowered_ty = lower_hir_ty(db, ty, self.scope(), assumptions);

                if lowered_ty.has_invalid(db) {
                    let errs =
                        collect_ty_lower_errors(db, self.scope(), ty, ty_span.clone(), assumptions);
                    if !errs.is_empty() {
                        diags.extend(errs);
                    }
                }

                if let Some(diag) = lowered_ty.emit_wf_diag(
                    db,
                    self.top_mod(db).ingot(db),
                    assumptions,
                    ty_span.into(),
                ) {
                    diags.push(diag);
                }
            }
        }
        diags
    }

    /// Full analysis entry for an impl-trait item.
    #[salsa::tracked(return_ref)]
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let implementor = match self.analyze_preconditions(db) {
            Ok(implementor) => implementor,
            Err(diags) => {
                return diags;
            }
        };

        let mut diags = implementor
            .skip_binder()
            .analyze_methods(db)
            .iter()
            .cloned()
            .collect::<Vec<_>>();

        let analyzer =
            DefAnalyzer::for_def(db, DefKind::ImplTrait(implementor.instantiate_identity()));
        diags.extend(analyzer.analyze());

        diags.extend(self.analyze_where_clause(db).iter().cloned());
        diags.extend(self.analyze_associated_types(db).iter().cloned());
        diags.extend(validate_generic_params_for_owner(
            db,
            GenericParamOwner::ImplTrait(self),
        ));
        diags
    }
}

#[salsa::tracked]
impl<'db> Impl<'db> {
    // Private helper to build an analyzer with correct assumptions for this item.
    fn def_analyzer(self, db: &'db dyn HirAnalysisDb) -> DefAnalyzer<'db> {
        DefAnalyzer::for_def(db, DefKind::Impl(self))
    }
    /// Analyze the impl's where-clause predicates using the impl's scope
    /// and constraints.
    #[salsa::tracked(return_ref)]
    pub fn analyze_where_clause(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let assumptions = self.scope().constraints(db);
        analyze_where_clause_for_owner(db, WhereClauseOwner::Impl(self), assumptions)
    }

    /// Full analysis entry for an inherent impl item.
    #[salsa::tracked(return_ref)]
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let analyzer = self.def_analyzer(db);
        let mut diags = analyzer.analyze();
        diags.extend(self.analyze_preconditions(db).iter().cloned());
        diags.extend(validate_generic_params_for_owner(
            db,
            GenericParamOwner::Impl(self),
        ));
        diags.extend(self.analyze_where_clause(db).iter().cloned());
        diags
    }
}

#[salsa::tracked]
impl<'db> crate::hir_def::Struct<'db> {
    fn def_analyzer(self, db: &'db dyn HirAnalysisDb) -> DefAnalyzer<'db> {
        DefAnalyzer::for_def(db, AdtRef::Struct(self).into())
    }
    /// Full analysis entry for a struct definition.
    #[salsa::tracked(return_ref)]
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut diags = Vec::new();
        // Visitor-based checks
        diags.extend(self.def_analyzer(db).analyze());
        // Item-owned checks
        diags.extend(self.validate_fields(db).iter().cloned());
        diags.extend(self.analyze_where_clause(db).iter().cloned());
        diags.extend(validate_generic_params_for_owner(
            db,
            GenericParamOwner::Struct(self),
        ));
        // Duplicate field names
        diags.extend(check_duplicate_field_names(db, FieldParent::Struct(self)));
        diags
    }
    #[salsa::tracked(return_ref)]
    pub fn analyze_where_clause(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let assumptions = self.scope().constraints(db);
        analyze_where_clause_for_owner(db, WhereClauseOwner::Struct(self), assumptions)
    }
}

#[salsa::tracked]
impl<'db> crate::hir_def::Enum<'db> {
    fn def_analyzer(self, db: &'db dyn HirAnalysisDb) -> DefAnalyzer<'db> {
        DefAnalyzer::for_def(db, AdtRef::Enum(self).into())
    }
    /// Full analysis entry for an enum definition.
    #[salsa::tracked(return_ref)]
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut diags = Vec::new();
        // Visitor-based checks
        diags.extend(self.def_analyzer(db).analyze());
        // Item-owned checks
        diags.extend(self.validate_variants(db).iter().cloned());
        diags.extend(self.analyze_where_clause(db).iter().cloned());
        diags.extend(validate_generic_params_for_owner(
            db,
            GenericParamOwner::Enum(self),
        ));
        // Duplicate variant names
        diags.extend(check_duplicate_variant_names(db, self));
        // Duplicate field names for record variants
        for (idx, var) in self.variants(db).data(db).iter().enumerate() {
            if matches!(var.kind, VariantKind::Record(..)) {
                diags.extend(check_duplicate_field_names(
                    db,
                    FieldParent::Variant(EnumVariant::new(self, idx)),
                ));
            }
        }
        diags
    }
    #[salsa::tracked(return_ref)]
    pub fn analyze_where_clause(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let assumptions = self.scope().constraints(db);
        analyze_where_clause_for_owner(db, WhereClauseOwner::Enum(self), assumptions)
    }
}

#[salsa::tracked]
impl<'db> crate::hir_def::Contract<'db> {
    fn def_analyzer(self, db: &'db dyn HirAnalysisDb) -> DefAnalyzer<'db> {
        DefAnalyzer::for_def(db, AdtRef::Contract(self).into())
    }
    /// Full analysis entry for a contract definition.
    #[salsa::tracked(return_ref)]
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let mut diags = Vec::new();
        diags.extend(self.def_analyzer(db).analyze());
        diags.extend(self.validate_fields(db).iter().cloned());
        diags.extend(self.analyze_where_clause(db).iter().cloned());
        // Contracts have no generic params (yet)
        diags.extend(check_duplicate_field_names(db, FieldParent::Contract(self)));
        diags
    }

    #[salsa::tracked(return_ref)]
    pub fn analyze_where_clause(self, _db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        Vec::new()
    }
}

#[salsa::tracked]
impl<'db> crate::hir_def::TypeAlias<'db> {
    // Crate-visible helper to build an analyzer with correct assumptions for this item.
    pub(crate) fn def_analyzer(self, db: &'db dyn HirAnalysisDb) -> DefAnalyzer<'db> {
        DefAnalyzer::for_def(db, DefKind::TypeAlias(self))
    }
}

fn analyze_trait_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    self_ty: TyId<'db>,
    trait_ref: TraitRefId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    span: LazyTraitRefSpan<'db>,
) -> Option<TyDiagCollection<'db>> {
    let trait_inst = match lower_trait_ref(db, self_ty, trait_ref, scope, assumptions) {
        Ok(trait_ref) => trait_ref,

        Err(TraitRefLowerError::PathResError(err)) => {
            let trait_path_span = span.path();
            return Some(
                err.into_diag(
                    db,
                    trait_ref.path(db).unwrap(),
                    trait_path_span,
                    ExpectedPathKind::Trait,
                )?
                .into(),
            );
        }

        Err(TraitRefLowerError::InvalidDomain(res)) => {
            return Some(
                PathResDiag::ExpectedTrait(
                    span.path().into(),
                    trait_ref.path(db).unwrap().ident(db).unwrap(),
                    res.kind_name(),
                )
                .into(),
            );
        }

        Err(TraitRefLowerError::Ignored) => {
            return None;
        }
    };

    // Skip checking trait constraints that involve associated types of generic parameters
    // These will be checked when the function is actually called with concrete types
    if trait_inst.self_ty(db).contains_assoc_ty_of_param(db) {
        return None;
    }

    trait_inst.emit_sat_diag(db, scope.ingot(db), assumptions, span.into())
}

#[derive(Clone, Copy, Debug, derive_more::From)]
enum DefKind<'db> {
    Adt(AdtRef<'db>),
    Trait(Trait<'db>),
    ImplTrait(crate::hir_def::ImplTrait<'db>),
    Impl(HirImpl<'db>),
    Func(CallableDef<'db>),
    TypeAlias(TypeAlias<'db>),
}

impl<'db> DefKind<'db> {
    fn original_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        match self {
            Self::Adt(def) => def.original_params(db),
            Self::Trait(def) => def.original_params(db),
            Self::ImplTrait(def) => collect_generic_params(db, def.into()).params(db),
            Self::Impl(hir_impl) => collect_generic_params(db, hir_impl.into()).params(db),
            Self::Func(def) => def.explicit_params(db),
            Self::TypeAlias(alias) => collect_generic_params(db, alias.into()).explicit_params(db),
        }
    }

    fn trait_self_param(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        if let Self::Trait(def) = self {
            def.self_param(db)
        } else {
            panic!()
        }
    }

    fn super_trait_cycle(self, db: &'db dyn HirAnalysisDb) -> Option<&'db Vec<Trait<'db>>> {
        if let Self::Trait(def) = self {
            super_trait_cycle(db, def).as_ref()
        } else {
            None
        }
    }

    fn scope(self, _db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        match self {
            Self::Adt(def) => def.scope(),
            Self::Trait(def) => def.scope(),
            Self::ImplTrait(def) => def.scope(),
            Self::Impl(hir_impl) => hir_impl.scope(),
            Self::Func(def) => def.scope(),
            Self::TypeAlias(alias) => alias.scope(),
        }
    }
}

/// This function analyzes the trait impl specific error.
/// 1. If the trait ref is well-formed except for the satisfiability.
/// 2. If implementor type is well-formed except for the satisfiability.
/// 3. If the ingot contains impl trait is the same as the ingot which contains
///    either the type or trait.
/// 4. If conflict occurs.
/// 5. If implementor type satisfies the required kind bound.
/// 6. If implementor type satisfies the required trait bound.
fn analyze_conflict_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: Binder<ImplTrait<'db>>,
    diags: &mut Vec<TyDiagCollection<'db>>,
) {
    use crate::analysis::ty::trait_def::{does_impl_trait_conflict, ingot_trait_env};

    let Some(trait_def) = impl_trait.skip_binder().trait_def(db) else {
        return;
    };

    let env = ingot_trait_env(db, trait_def.ingot(db));
    let Some(impls) = env.impls.get(&trait_def) else {
        return;
    };

    for cand in impls {
        // Skip checking an impl against itself
        if cand.skip_binder() == impl_trait.skip_binder() {
            continue;
        }

        if does_impl_trait_conflict(db, *cand, impl_trait) {
            diags.push(
                TraitLowerDiag::ConflictTraitImpl {
                    primary: *cand.skip_binder(),
                    conflict_with: *impl_trait.skip_binder(),
                }
                .into(),
            );

            return;
        }
    }
}

#[salsa::tracked]
impl<'db> Func<'db> {
    fn def_analyzer(self, db: &'db dyn HirAnalysisDb) -> DefAnalyzer<'db> {
        DefAnalyzer::for_def(db, DefKind::Func(CallableDef::Func(self)))
    }
    /// Full analysis entry for a function item. Consolidates the prior
    /// free-function `analyze_func` pattern into an item method while
    /// still using the DefAnalyzer visitor for raw traversal where needed.
    #[salsa::tracked(return_ref)]
    pub fn analyze(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        let Some(_name) = self.name(db).to_opt() else {
            return Vec::new();
        };

        let analyzer = self.def_analyzer(db);
        let mut diags = analyzer.analyze();

        // Item-owned checks
        diags.extend(self.analyze_where_clause(db).iter().cloned());
        diags.extend(self.analyze_signature(db).iter().cloned());
        diags
    }
    /// Analyze the function's where-clause predicates using the function's
    /// own scope and constraints.
    #[salsa::tracked(return_ref)]
    pub fn analyze_where_clause(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        // Functions include parent constraints (trait/impl) for signature checks
        let assumptions = self.scope().constraints(db);
        analyze_where_clause_for_owner(db, WhereClauseOwner::Func(self), assumptions)
    }

    /// Analyze signature-level concerns for a function:
    /// - Method conflict in impl blocks (inherent method overlap)
    /// - Duplicate argument names and labels
    /// - Param type kind checks and const-type disallow, with type-lowering errors
    /// - Self type verification for `self` params
    /// - Return type kind checks and type-lowering errors
    #[salsa::tracked(return_ref)]
    pub fn analyze_signature(self, db: &'db dyn HirAnalysisDb) -> Vec<TyDiagCollection<'db>> {
        use crate::span::params::LazyFuncParamListSpan;

        let mut diags = Vec::new();
        let scope = self.scope();
        let assumptions =
            collect_func_def_constraints(db, self.into(), true).instantiate_identity();

        let callable = CallableDef::Func(self);

        // Check inherent method conflicts only for functions under `impl` blocks.
        if matches!(scope.parent_item(db).unwrap(), ItemKind::Impl(_)) {
            if let Some(self_ty) = callable
                .receiver_ty(db)
                .map(|b| b.instantiate_identity())
                .or_else(|| callable.self_ty(db))
            {
                if !self_ty.has_invalid(db) {
                    for &cand in probe_method(
                        db,
                        scope.ingot(db),
                        Canonical::new(db, self_ty),
                        callable.name(db),
                    ) {
                        if cand != callable {
                            diags.push(
                                ImplDiag::ConflictMethodImpl {
                                    primary: callable,
                                    conflict_with: cand,
                                }
                                .into(),
                            );
                            break;
                        }
                    }
                }
            }
        }

        // Duplicate argument name/label checks
        if let Some(params) = self.params(db).to_opt() {
            let list = params.data(db);

            let dupes = check_duplicate_names(list.iter().map(|p| p.name()), |idxs| {
                TyLowerDiag::DuplicateArgName(self, idxs).into()
            });
            let found_dupes = !dupes.is_empty();
            diags.extend(dupes);

            if !found_dupes {
                diags.extend(check_duplicate_names(
                    list.iter().map(|p| p.label_eagerly()),
                    |idxs| TyLowerDiag::DuplicateArgLabel(self, idxs).into(),
                ));
            }

            // Per-parameter type checks
            let param_list_span: LazyFuncParamListSpan<'db> = self.span().params();
            for (i, param) in list.iter().enumerate() {
                let Some(hir_ty) = param.ty.to_opt() else {
                    continue;
                };

                let pspan = param_list_span.clone().param(i);
                let name_span_node = pspan.clone().name();
                let ty_span_node = pspan.clone().ty();
                let ty_span: DynLazySpan<'db> = if param.is_self_param(db) && param.self_ty_fallback
                {
                    name_span_node.into()
                } else {
                    ty_span_node.clone().into()
                };

                let ty = lower_hir_ty(db, hir_ty, scope, assumptions);
                if !ty.has_star_kind(db) {
                    diags.push(TyLowerDiag::ExpectedStarKind(ty_span.clone()).into());
                    continue;
                }
                if ty.is_const_ty(db) {
                    diags.push(
                        TyLowerDiag::NormalTypeExpected {
                            span: ty_span.clone(),
                            given: ty,
                        }
                        .into(),
                    );
                    continue;
                }

                // Self type verification
                if param.is_self_param(db) {
                    if let Some(expected_ty) = callable.self_ty(db) {
                        let param_ty = normalize_ty(db, ty, scope, assumptions);
                        if !param_ty.has_invalid(db) && !expected_ty.has_invalid(db) {
                            let (expected_base, expected_args) = expected_ty.decompose_ty_app(db);
                            let (param_base, param_args) = param_ty.decompose_ty_app(db);
                            if param_base != expected_base
                                || expected_args
                                    .iter()
                                    .zip(param_args.iter())
                                    .any(|(e, p)| e != p)
                            {
                                diags.push(
                                    ImplDiag::InvalidSelfType {
                                        span: ty_span,
                                        expected: expected_ty,
                                        given: param_ty,
                                    }
                                    .into(),
                                );
                            }
                        }
                    }
                }
            }
        }

        // Return type checks
        if let Some(hir_ret) = self.ret_ty(db) {
            let rspan = self.span().ret_ty();
            let ty = lower_hir_ty(db, hir_ret, scope, assumptions);
            if !ty.has_star_kind(db) {
                diags.push(TyLowerDiag::ExpectedStarKind(rspan.into()).into());
            } else if ty.is_const_ty(db) {
                diags.push(
                    TyLowerDiag::NormalTypeExpected {
                        span: rspan.into(),
                        given: ty,
                    }
                    .into(),
                );
            }
        }

        diags
    }
}
