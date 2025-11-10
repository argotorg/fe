//! This module contains analysis for the definition of the type/trait.
//! This module is the only module in `ty` module which is allowed to emit
//! diagnostics.

use crate::{
    hir_def::{
        EnumVariant, FieldDef, FieldParent, Func, GenericParam, GenericParamListId,
        GenericParamOwner, GenericParamView, IdentId, Impl as HirImpl, ImplTrait, ItemKind, PathId,
        Trait, TraitRefId, TypeAlias, TypeBound, TypeId as HirTyId, VariantKind,
        scope_graph::ScopeId,
    },
    visitor::prelude::*,
};
use common::indexmap::IndexSet;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec1::SmallVec;

use super::{
    adt_def::{AdtRef, lower_adt},
    canonical::Canonical,
    const_ty::ConstTyId,
    diagnostics::{ImplDiag, TraitConstraintDiag, TraitLowerDiag, TyDiagCollection, TyLowerDiag},
    func_def::FuncDef,
    method_cmp::compare_impl_method,
    method_table::probe_method,
    normalize::normalize_ty,
    trait_def::{Implementor, TraitDef, ingot_trait_env},
    trait_lower::{TraitRefLowerError, lower_trait, lower_trait_ref},
    trait_resolution::{
        PredicateListId,
        constraint::{collect_adt_constraints, collect_constraints, collect_func_def_constraints},
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
        adt_def::AdtDef,
        binder::Binder,
        canonical::Canonicalized,
        func_def::lower_func,
        trait_def::{TraitInstId, does_impl_trait_conflict},
        trait_lower::lower_impl_trait,
        trait_resolution::{
            GoalSatisfiability, constraint::super_trait_cycle, is_goal_satisfiable,
        },
        ty_lower::lower_hir_ty,
        visitor::TyVisitable,
    },
};

/// This function implements analysis for the ADT definition.
/// The analysis includes the following:
/// - Check if the types in the ADT is well-formed.
/// - Check if the trait instantiation appears in the ADT is well-formed.
/// - Check if the field types are fully applied(i.e., these types should have
///   `*` kind).
/// - Check if the types in the ADT satisfies the constraints which is required
///   in type application.
/// - Check if the trait instantiations in the ADT satisfies the constraints.
/// - Check if the recursive types has indirect type wrapper like pointer.
#[salsa::tracked(return_ref)]
pub fn analyze_adt<'db>(
    db: &'db dyn HirAnalysisDb,
    adt_ref: AdtRef<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let dupes = match adt_ref {
        AdtRef::Struct(x) => check_duplicate_field_names(db, FieldParent::Struct(x)),
        AdtRef::Contract(x) => check_duplicate_field_names(db, FieldParent::Contract(x)),
        AdtRef::Enum(enum_) => {
            let mut dupes = check_duplicate_variant_names(db, enum_);

            for (idx, var) in enum_.variants(db).enumerate() {
                if matches!(var.kind(db), VariantKind::Record(..)) {
                    dupes.extend(check_duplicate_field_names(
                        db,
                        FieldParent::Variant(EnumVariant::new(enum_, idx)),
                    ))
                }
            }
            dupes
        }
    };

    let analyzer = DefAnalyzer::for_adt(db, adt_ref);
    let mut diags = analyzer.analyze();
    diags.extend(dupes);
    diags
}

fn check_duplicate_field_names<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: FieldParent<'db>,
) -> SmallVec<[TyDiagCollection<'db>; 2]> {
    check_duplicate_names(
        owner.fields(db).map(|v| v.name(db)),
        |idxs| TyLowerDiag::DuplicateFieldName(owner, idxs).into(),
    )
}

fn check_duplicate_variant_names<'db>(
    db: &'db dyn HirAnalysisDb,
    enum_: crate::core::hir_def::Enum<'db>,
) -> SmallVec<[TyDiagCollection<'db>; 2]> {
    check_duplicate_names(enum_.variants(db).map(|v| v.name(db).to_opt()), |idxs| {
        TyLowerDiag::DuplicateVariantName(enum_, idxs).into()
    })
}

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

/// This function implements analysis for the trait definition.
/// The analysis includes the following:
/// - Check if the types appear in the trait is well-formed.
/// - Check if the trait instantiation appears in the trait is well-formed.
/// - Check if the types in the trait satisfy the constraints which is required
///   in type application.
/// - Check if the trait instantiations in the trait satisfies the constraints.
#[salsa::tracked(return_ref)]
pub fn analyze_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let analyzer = DefAnalyzer::for_trait(db, trait_);
    let mut diags = analyzer.analyze();

    // Check associated type defaults satisfy their bounds
    let _trait_def = lower_trait(db, trait_);
    let assumptions = collect_constraints(db, trait_.into()).instantiate_identity();

    for assoc in trait_.assoc_types(db) {
        if let Some(default_ty) = assoc.default_ty(db) {
            // Check each bound on the associated type
            for bound in assoc.bounds(db) {
                let TypeBound::Trait(trait_ref) = bound else {
                    continue;
                };
                // Lower the trait bound
                let Ok(trait_inst) =
                    lower_trait_ref(db, default_ty, *trait_ref, trait_.scope(), assumptions)
                else {
                    // Trait ref lowering error - will be reported elsewhere
                    continue;
                };

                // Check if the default type satisfies the trait bound
                let canonical_inst = Canonical::new(db, trait_inst);
                match is_goal_satisfiable(
                    db,
                    trait_.top_mod(db).ingot(db),
                    canonical_inst,
                    assumptions,
                ) {
                    GoalSatisfiability::Satisfied(_) => continue,
                    GoalSatisfiability::UnSat(subgoal) => {
                        // Report error: default type doesn't satisfy the bound
                        // TODO: Get a better span for the default type
                        diags.push(
                            TraitConstraintDiag::TraitBoundNotSat {
                                span: trait_.span().into(),
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

    diags
}

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
#[salsa::tracked(return_ref)]
pub fn analyze_impl_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let implementor = match analyze_impl_trait_specific_error(db, impl_trait) {
        Ok(implementor) => implementor,
        Err(diags) => {
            return diags;
        }
    };

    let mut diags = analyze_impl_trait_method(db, implementor.instantiate_identity());

    let analyzer = DefAnalyzer::for_trait_impl(db, implementor.instantiate_identity());
    let def_diags = analyzer.analyze();

    diags.extend(def_diags);
    diags
}

#[salsa::tracked(return_ref)]
pub fn analyze_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: HirImpl<'db>,
) -> Vec<TyDiagCollection<'db>> {
    if impl_.type_ref___tmp(db).to_opt().is_none() {
        return Vec::new();
    }

    let ty = impl_.ty(db);

    let analyzer = DefAnalyzer::for_impl(db, impl_, ty);
    analyzer.analyze()
}

#[salsa::tracked(return_ref)]
pub fn analyze_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<TyDiagCollection<'db>> {
    let Some(func_def) = lower_func(db, func) else {
        return Vec::new();
    };

    let assumptions = collect_func_def_constraints(db, func.into(), true).instantiate_identity();
    let analyzer = DefAnalyzer::for_func(db, func_def, assumptions);
    analyzer.analyze()
}

pub struct DefAnalyzer<'db> {
    db: &'db dyn HirAnalysisDb,
    def: DefKind<'db>,
    self_ty: Option<TyId<'db>>,
    diags: Vec<TyDiagCollection<'db>>,
    assumptions: PredicateListId<'db>,
    current_ty: Option<(TyId<'db>, DynLazySpan<'db>)>,
}

impl<'db> DefAnalyzer<'db> {
    fn for_adt(db: &'db dyn HirAnalysisDb, adt: AdtRef<'db>) -> Self {
        let def = lower_adt(db, adt);
        let assumptions = collect_adt_constraints(db, def).instantiate_identity();
        Self {
            db,
            def: def.into(),
            self_ty: None,
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    fn for_trait(db: &'db dyn HirAnalysisDb, trait_: Trait<'db>) -> Self {
        let def = lower_trait(db, trait_);
        let assumptions = collect_constraints(db, trait_.into()).instantiate_identity();
        Self {
            db,
            def: def.into(),
            self_ty: def.self_param(db).into(),
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    fn for_impl(db: &'db dyn HirAnalysisDb, impl_: HirImpl<'db>, ty: TyId<'db>) -> Self {
        let assumptions = collect_constraints(db, impl_.into()).instantiate_identity();
        let def = DefKind::Impl(impl_);
        Self {
            db,
            def,
            self_ty: ty.into(),
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    fn for_trait_impl(db: &'db dyn HirAnalysisDb, implementor: Implementor<'db>) -> Self {
        let assumptions = implementor.constraints(db);
        Self {
            db,
            def: implementor.into(),
            self_ty: implementor.self_ty(db).into(),
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    fn for_func(
        db: &'db dyn HirAnalysisDb,
        func: FuncDef<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        let self_ty = match func.hir_func_def(db).unwrap().scope().parent(db).unwrap() {
            ScopeId::Item(ItemKind::Trait(trait_)) => lower_trait(db, trait_).self_param(db).into(),
            ScopeId::Item(ItemKind::ImplTrait(impl_trait)) => impl_trait.ty(db).into(),
            ScopeId::Item(ItemKind::Impl(impl_)) => impl_.ty(db).into(),
            _ => None,
        };

        Self {
            db,
            def: func.into(),
            self_ty,
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    pub(crate) fn for_type_alias(
        db: &'db dyn HirAnalysisDb,
        type_alias: crate::core::hir_def::TypeAlias<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        Self {
            db,
            def: DefKind::TypeAlias(type_alias),
            self_ty: None,
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    /// This method verifies if
    /// 1. the given `ty` has `*` kind.
    /// 2. the given `ty` is not const type
    ///
    /// TODO: This method is a stop-gap implementation until we design a true
    /// const type system.
    fn verify_term_type_kind(&mut self, ty: HirTyId<'db>, span: DynLazySpan<'db>) -> bool {
        let ty = lower_hir_ty(self.db, ty, self.scope(), self.assumptions);
        if !ty.has_star_kind(self.db) {
            self.diags.push(TyLowerDiag::ExpectedStarKind(span).into());
            false
        } else if ty.is_const_ty(self.db) {
            self.diags
                .push(TyLowerDiag::NormalTypeExpected { span, given: ty }.into());
            false
        } else {
            true
        }
    }

    fn verify_self_type(&mut self, self_ty: HirTyId<'db>, span: DynLazySpan<'db>) -> bool {
        let Some(expected_ty) = self.self_ty else {
            return false;
        };

        let param_ty = lower_hir_ty(self.db, self_ty, self.def.scope(self.db), self.assumptions);
        let param_ty = normalize_ty(self.db, param_ty, self.def.scope(self.db), self.assumptions);
        if !param_ty.has_invalid(self.db) && !expected_ty.has_invalid(self.db) {
            let (expected_base_ty, expected_param_ty_args) = expected_ty.decompose_ty_app(self.db);
            let (param_base_ty, param_ty_args) = param_ty.decompose_ty_app(self.db);

            if param_base_ty != expected_base_ty {
                self.diags.push(
                    ImplDiag::InvalidSelfType {
                        span: span.clone(),
                        expected: expected_ty,
                        given: param_ty,
                    }
                    .into(),
                );
                return false;
            }

            for (expected_arg, param_arg) in expected_param_ty_args.iter().zip(param_ty_args.iter())
            {
                if expected_arg != param_arg {
                    self.diags.push(
                        ImplDiag::InvalidSelfType {
                            span,
                            expected: expected_ty,
                            given: param_ty,
                        }
                        .into(),
                    );
                    return false;
                }
            }
        }

        true
    }

    fn check_method_conflict(&mut self, func: FuncDef<'db>) -> bool {
        let self_ty = func
            .receiver_ty(self.db)
            .map_or_else(|| self.self_ty.unwrap(), |ty| ty.instantiate_identity());

        if self_ty.has_invalid(self.db) {
            return true;
        }

        for &cand in probe_method(
            self.db,
            self.scope().ingot(self.db),
            Canonical::new(self.db, self_ty),
            func.name(self.db),
        ) {
            if cand != func {
                self.diags.push(
                    ImplDiag::ConflictMethodImpl {
                        primary: func,
                        conflict_with: cand,
                    }
                    .into(),
                );
                return false;
            }
        }

        true
    }

    fn scope(&self) -> ScopeId<'db> {
        self.def.scope(self.db)
    }

    pub(crate) fn analyze(mut self) -> Vec<TyDiagCollection<'db>> {
        match self.def {
            DefKind::Adt(def) => match def.adt_ref(self.db) {
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
                let trait_ = trait_.trait_(self.db);
                let mut ctxt = VisitorCtxt::with_trait(self.db, trait_);
                self.visit_trait(&mut ctxt, trait_);
            }

            DefKind::ImplTrait(implementor) => {
                let impl_trait = implementor.hir_impl_trait(self.db);
                let mut ctxt = VisitorCtxt::with_impl_trait(self.db, impl_trait);
                self.visit_impl_trait(&mut ctxt, impl_trait);
            }

            DefKind::Impl(hir_impl) => {
                let mut ctxt = VisitorCtxt::with_impl(self.db, hir_impl);
                self.visit_impl(&mut ctxt, hir_impl)
            }

            DefKind::Func(func) => {
                let hir_func = func.hir_func_def(self.db).unwrap();
                let mut ctxt = VisitorCtxt::with_func(self.db, hir_func);
                self.visit_func(&mut ctxt, hir_func);
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
#[deprecated]
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
        pred: &crate::core::hir_def::WherePredicate<'db>,
    ) {
        let Some(hir_ty) = pred.ty.to_opt() else {
            return;
        };

        let ty = lower_hir_ty(self.db, hir_ty, self.scope(), self.assumptions);

        if ty.is_const_ty(self.db) {
            let diag =
                TraitConstraintDiag::ConstTyBound(ctxt.span().unwrap().ty().into(), ty).into();
            self.diags.push(diag);
            return;
        }

        if !ty.has_invalid(self.db) && !ty.has_param(self.db) {
            let diag =
                TraitConstraintDiag::ConcreteTypeBound(ctxt.span().unwrap().ty().into(), ty).into();
            self.diags.push(diag);
            return;
        }

        self.current_ty = Some((ty, ctxt.span().unwrap().ty().into()));
        walk_where_predicate(self, ctxt, pred);
    }

    fn visit_field_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFieldDefSpan<'db>>,
        field: &FieldDef<'db>,
    ) {
        let Some(ty) = field.field_type_ref___tmp().to_opt() else {
            return;
        };

        if !self.verify_term_type_kind(ty, ctxt.span().unwrap().ty().into()) {
            return;
        }

        let Some(name) = field.name.to_opt() else {
            return;
        };

        // Checks if the field type is the same as the type of const type parameter.
        if let Some(const_ty) = find_const_ty_param(self.db, name, ctxt.scope()) {
            let const_ty_ty = const_ty.ty(self.db);
            let field_ty = lower_hir_ty(self.db, ty, ctxt.scope(), self.assumptions);
            if !const_ty_ty.has_invalid(self.db)
                && !field_ty.has_invalid(self.db)
                && field_ty != const_ty_ty
            {
                self.diags.push(
                    TyLowerDiag::ConstTyMismatch {
                        span: ctxt.span().unwrap().ty().into(),
                        expected: const_ty_ty,
                        given: field_ty,
                    }
                    .into(),
                );
                return;
            }
        }

        walk_field_def(self, ctxt, field);
    }

    fn visit_variant_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyVariantDefSpan<'db>>,
        variant: &crate::core::hir_def::VariantDef<'db>,
    ) {
        if let VariantKind::Tuple(tuple_id) = variant.kind {
            let span = ctxt.span().unwrap().tuple_type();
            for (i, elem_ty) in tuple_id.data(self.db).iter().enumerate() {
                let Some(elem_ty) = elem_ty.to_opt() else {
                    continue;
                };

                self.verify_term_type_kind(elem_ty, span.clone().elem_ty(i).into());
            }
        }
        walk_variant_def(self, ctxt, variant);
    }

    fn visit_generic_param_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyGenericParamListSpan<'db>>,
        params: GenericParamListId<'db>,
    ) {
        let owner = GenericParamOwner::from_item_opt(self.scope().item()).unwrap();

        self.diags.extend(
            owner
                .diags_check_duplicate_names(self.db)
                .collect::<Vec<_>>(),
        );

        let mut default_idxs: SmallVec<[usize; 4]> = SmallVec::new();
        for (i, p) in params.data(self.db).iter().enumerate() {
            let is_defaulted_type = matches!(p, GenericParam::Type(tp) if tp.default_ty.is_some());
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

        if let Some(diag) =
            check_param_defined_in_parent(self.db, self.scope(), param, ctxt.span().unwrap())
        {
            self.diags.push(diag.into());
            return;
        }

        match param {
            GenericParam::Type(tp) => {
                if let Some(default_ty) = tp.default_ty {
                    let lowered = lower_hir_ty(self.db, default_ty, self.scope(), self.assumptions);

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
                        if let Some(name) = owner.param_view(self.db, *j).param.name().to_opt() {
                            let span = ctxt.span().unwrap();
                            self.diags
                                .push(TyLowerDiag::GenericDefaultForwardRef { span, name }.into());
                        }
                    }
                }

                self.current_ty = Some((
                    self.def.original_params(self.db)[idx as usize],
                    ctxt.span().unwrap().into_type_param().name().into(),
                ));
            }
            GenericParam::Const(_) => {
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
        walk_generic_param(self, ctxt, param)
    }

    fn visit_kind_bound(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyKindBoundSpan<'db>>,
        bound: &crate::core::hir_def::KindBound,
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
        ctxt: &mut VisitorCtxt<'db, crate::core::span::item::LazySuperTraitListSpan<'db>>,
        super_traits: &[TraitRefId<'db>],
    ) {
        let DefKind::Trait(def) = self.def else {
            unreachable!()
        };
        let name_span = def.trait_(self.db).span().name().into();
        self.current_ty = Some((self.def.trait_self_param(self.db), name_span));
        walk_super_trait_list(self, ctxt, super_traits);
    }

    fn visit_impl(&mut self, ctxt: &mut VisitorCtxt<'db, LazyImplSpan<'db>>, impl_: HirImpl<'db>) {
        let impl_ty = impl_.ty(self.db);
        if !impl_ty.is_inherent_impl_allowed(self.db, self.scope().ingot(self.db)) {
            let base = impl_ty.base_ty(self.db);
            let diag = ImplDiag::InherentImplIsNotAllowed {
                primary: ctxt.span().unwrap().target_ty().into(),
                ty: base.pretty_print(self.db).to_string(),
                is_nominal: !base.is_param(self.db),
            };

            self.diags.push(diag.into());
        }

        if let Some(ty) = impl_ty.emit_diag(self.db, ctxt.span().unwrap().target_ty().into()) {
            self.diags.push(ty);
        } else {
            walk_impl(self, ctxt, impl_);
        }
    }

    fn visit_impl_trait(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyImplTraitSpan<'db>>,
        impl_trait: ImplTrait<'db>,
    ) {
        for assoc_type in impl_trait.types(self.db) {
            if let Some(ty) = assoc_type.type_ref___tmp().to_opt() {
                let ty_span = assoc_type
                    .name
                    .to_opt()
                    .and_then(|name| impl_trait.associated_type_span(self.db, name))
                    .map(|s| s.ty())
                    .unwrap_or_else(|| ctxt.span().unwrap().ty());

                let lowered_ty = lower_hir_ty(self.db, ty, impl_trait.scope(), self.assumptions);

                if lowered_ty.has_invalid(self.db) {
                    let diags = collect_ty_lower_errors(
                        self.db,
                        impl_trait.scope(),
                        ty,
                        ty_span.clone(),
                        self.assumptions,
                    );
                    if !diags.is_empty() {
                        self.diags.extend(diags);
                    }
                }

                if let Some(diag) =
                    lowered_ty.emit_wf_diag(self.db, ctxt.ingot(), self.assumptions, ty_span.into())
                {
                    self.diags.push(diag);
                }
            }
        }

        walk_impl_trait(self, ctxt, impl_trait);
    }

    fn visit_func(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFuncSpan<'db>>,
        hir_func: crate::core::hir_def::Func<'db>,
    ) {
        let Some(func) = lower_func(self.db, hir_func) else {
            return;
        };

        // We need to check the conflict only when the function is defined in the `impl`
        // block since this check requires the ingot-wide method table(i.e., which is
        // not performed in name resolution phase).
        if matches!(
            ctxt.scope().parent_item(self.db).unwrap(),
            ItemKind::Impl(_)
        ) && !self.check_method_conflict(func)
        {
            return;
        }

        // Skip the rest of the analysis if any param names conflict with a parent's param
        // let params = .generic_params(self.db);
        let mut is_conflict = false;
        for diag in GenericParamOwner::Func(hir_func).diags_params_defined_in_parent(self.db) {
            self.diags.push(diag);
            is_conflict = true;
        }
        if is_conflict {
            return;
        }

        let def = std::mem::replace(&mut self.def, func.into());
        let constraints = std::mem::replace(
            &mut self.assumptions,
            collect_func_def_constraints(self.db, hir_func.into(), true).instantiate_identity(),
        );

        {
            // Duplicate parameter names
            let dupes = check_duplicate_names(
                hir_func
                    .param_views(self.db)
                    .map(|v| v.name(self.db)),
                |idxs| TyLowerDiag::DuplicateArgName(hir_func, idxs).into(),
            );
            let found_dupes = !dupes.is_empty();
            self.diags.extend(dupes);

            // Duplicate labels (only if names were unique)
            if !found_dupes {
                self.diags.extend(check_duplicate_names(
                    hir_func
                        .param_views(self.db)
                        .map(|v| v.label_eagerly(self.db)),
                    |idxs| TyLowerDiag::DuplicateArgLabel(hir_func, idxs).into(),
                ));
            }
        }

        walk_func(self, ctxt, hir_func);

        if let Some(ret_ty) = hir_func.ret_type_ref___tmp(self.db) {
            self.verify_term_type_kind(ret_ty, hir_func.span().ret_ty().into());
        }

        self.assumptions = constraints;
        self.def = def;
    }

    fn visit_body(
        &mut self,
        _ctxt: &mut VisitorCtxt<'_, LazyBodySpan>,
        _body: crate::core::hir_def::Body,
    ) {
    }

    fn visit_func_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFuncParamSpan<'db>>,
        param: &crate::core::hir_def::FuncParam<'db>,
    ) {
        let Some(hir_ty) = param.ty.to_opt() else {
            return;
        };

        let ty_span: DynLazySpan = if param.is_self_param(ctxt.db()) && param.self_ty_fallback {
            ctxt.span().unwrap().name().into()
        } else {
            ctxt.span().unwrap().ty().into()
        };

        if param.is_self_param(ctxt.db()) {
            self.verify_self_type(hir_ty, ty_span.clone());
        }

        if !self.verify_term_type_kind(hir_ty, ty_span) {
            return;
        }

        walk_func_param(self, ctxt, param);
    }
}

#[salsa::tracked(return_ref)]
pub(crate) fn check_recursive_adt<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
) -> Option<Vec<AdtCycleMember<'db>>> {
    check_recursive_adt_impl(db, adt, &[])
}

pub(crate) fn check_recursive_adt_impl<'db>(
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
            for field_adt_ref in ty.instantiate_identity().collect_direct_adts(db) {
                chain.push(AdtCycleMember {
                    adt,
                    field_idx: field_idx as u16,
                    ty_idx: ty_idx as u16,
                });

                if let Some(cycle) =
                    check_recursive_adt_impl(db, lower_adt(db, field_adt_ref), &chain)
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
    pub adt: AdtDef<'db>,
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

            fn visit_adt(&mut self, adt: AdtDef<'db>) {
                self.adts.insert(adt.adt_ref(self.db));
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
    Adt(AdtDef<'db>),
    Trait(TraitDef<'db>),
    ImplTrait(Implementor<'db>),
    Impl(HirImpl<'db>),
    Func(FuncDef<'db>),
    TypeAlias(TypeAlias<'db>),
}

impl<'db> DefKind<'db> {
    fn original_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        match self {
            Self::Adt(def) => def.original_params(db),
            Self::Trait(def) => def.original_params(db),
            Self::ImplTrait(def) => def.original_params(db),
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

    fn super_trait_cycle(self, db: &'db dyn HirAnalysisDb) -> Option<&'db Vec<TraitDef<'db>>> {
        if let Self::Trait(def) = self {
            super_trait_cycle(db, def).as_ref()
        } else {
            None
        }
    }

    fn scope(self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        match self {
            Self::Adt(def) => def.adt_ref(db).scope(),
            Self::Trait(def) => def.trait_(db).scope(),
            Self::ImplTrait(def) => def.hir_impl_trait(db).scope(),
            Self::Impl(hir_impl) => hir_impl.scope(),
            Self::Func(def) => def.scope(db),
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
fn analyze_impl_trait_specific_error<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Result<Binder<Implementor<'db>>, Vec<TyDiagCollection<'db>>> {
    let mut diags = vec![];
    // We don't need to report error because it should be reported from the parser.
    let Some(trait_ref) = impl_trait.trait_ref(db).to_opt() else {
        return Err(diags);
    };
    if impl_trait.type_ref___tmp(db).to_opt().is_none() {
        return Err(diags);
    }

    // 1. Checks if implementor type is well-formed except for the satisfiability.
    let assumptions = collect_constraints(db, impl_trait.into()).instantiate_identity();
    let ty = impl_trait.ty(db);
    if let Some(diag) = ty.emit_diag(db, impl_trait.span().ty().into()) {
        diags.push(diag);
    }

    // 2. Trait ref well-formedness is checked during trait lowering below

    // If there is any error at the point, it means that `Implementor` is not
    // well-formed and no more analysis is needed to reduce the amount of error
    // messages.
    if !diags.is_empty() || ty.has_invalid(db) {
        return Err(diags);
    }

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

    // 3. Check if the ingot containing impl trait is the same as the ingot which
    //    contains either the type or trait.
    let impl_trait_ingot = impl_trait.top_mod(db).ingot(db);
    if Some(impl_trait_ingot) != ty.ingot(db) && impl_trait_ingot != trait_inst.def(db).ingot(db) {
        diags.push(TraitLowerDiag::ExternalTraitForExternalType(impl_trait).into());
        return Err(diags);
    }

    let trait_env = ingot_trait_env(db, impl_trait_ingot);
    let Some(implementor) = trait_env.map_impl_trait(impl_trait) else {
        // Lower impl trait never fails if the trait ref and implementor type is
        // well-formed.
        let current_impl = lower_impl_trait(db, impl_trait).unwrap();

        // 4. Checks if conflict occurs.
        // If there is no implementor type even if the trait ref and implementor type is
        // well-formed, it means that the conflict does occur.
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

    // 5. Checks if implementor type satisfies the kind bound which is required by
    //    the trait.
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

    let trait_def = trait_inst.def(db);
    let trait_constraints =
        collect_constraints(db, trait_def.trait_(db).into()).instantiate(db, trait_inst.args(db));
    let assumptions = implementor.instantiate_identity().constraints(db);

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

    // 6. Checks if the trait inst is WF.
    let trait_ref_span: DynLazySpan = impl_trait.span().trait_ref().into();

    for &goal in trait_constraints.list(db) {
        is_satisfied(goal, trait_ref_span.clone(), &mut diags);
    }

    // 7. Checks if the implementor ty satisfies the super trait constraints.
    let target_ty_span: DynLazySpan = impl_trait.span().ty().into();

    for &super_trait in trait_def.super_traits(db) {
        let super_trait = super_trait.instantiate(db, trait_inst.args(db));
        is_satisfied(super_trait, target_ty_span.clone(), &mut diags)
    }

    // 8. Check that all required associated types are implemented,
    //    and that they satisfy their bounds
    let trait_hir = trait_def.trait_(db);
    let impl_types = implementor.instantiate_identity().types(db);
    for assoc_type in trait_hir.assoc_types(db) {
        let Some(name) = assoc_type.name(db) else {
            continue;
        };

        let impl_ty = impl_types.get(&name);
        if impl_ty.is_none() && assoc_type.default_ty(db).is_none() {
            diags.push(
                ImplDiag::MissingAssociatedType {
                    primary: impl_trait.span().ty().into(),
                    type_name: name,
                    trait_: trait_hir,
                }
                .into(),
            );
        }
        let Some(&impl_ty) = impl_ty else {
            continue;
        };

        // Check that the implemented associated type satisfies its bounds
        for bound in assoc_type.bounds(db) {
            if let TypeBound::Trait(trait_ref) = bound {
                match lower_trait_ref(db, impl_ty, *trait_ref, impl_trait.scope(), assumptions) {
                    Ok(bound_inst) => {
                        let canonical_bound = Canonical::new(db, bound_inst);
                        if let GoalSatisfiability::UnSat(subgoal) = is_goal_satisfiable(
                            db,
                            impl_trait.top_mod(db).ingot(db),
                            canonical_bound,
                            assumptions,
                        ) {
                            // Find the span for this specific associated type implementation
                            let assoc_ty_span = impl_trait
                                .associated_type_span(db, name)
                                .map(|s| s.ty().into())
                                .unwrap_or_else(|| impl_trait.span().ty().into());

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
                    Err(_) => {
                        // Error lowering the trait bound - this will be reported elsewhere
                    }
                }
            }
        }
    }

    if diags.is_empty() {
        Ok(implementor)
    } else {
        Err(diags)
    }
}

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
        .filter_map(|(name, &trait_method)| {
            if !trait_method.has_default_impl(db) {
                Some(*name)
            } else {
                None
            }
        })
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

fn find_const_ty_param<'db>(
    db: &'db dyn HirAnalysisDb,
    ident: IdentId<'db>,
    scope: ScopeId<'db>,
) -> Option<ConstTyId<'db>> {
    let path = PathId::from_ident(db, ident);
    let Ok(PathRes::Ty(ty)) = resolve_path(db, path, scope, PredicateListId::empty_list(db), true)
    else {
        return None;
    };
    match ty.data(db) {
        TyData::ConstTy(const_ty) => Some(*const_ty),
        _ => None,
    }
}
