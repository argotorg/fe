//! This module contains analysis for the definition of the type/trait.
//! This module is the only module in `ty` module which is allowed to emit
//! diagnostics.

use std::collections::{hash_map::Entry, BTreeSet};

use hir::{
    hir_def::{
        scope_graph::ScopeId, FieldDef, Func, FuncParamListId, GenericParam, IdentId,
        Impl as HirImpl, ImplTrait, ItemKind, PathId, Trait, TraitRefId, TypeAlias,
        TypeId as HirTyId, VariantKind,
    },
    visitor::prelude::*,
};
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::function::Configuration;

use super::{
    const_ty::ConstTyId,
    constraint::{
        collect_impl_block_constraints, collect_super_traits, AssumptionListId, SuperTraitCycle,
    },
    constraint_solver::{is_goal_satisfiable, GoalSatisfiability},
    diagnostics::{ImplDiag, TraitConstraintDiag, TraitLowerDiag, TyDiagCollection, TyLowerDiag},
    trait_def::{ingot_trait_env, Implementor, TraitDef, TraitMethod},
    trait_lower::{lower_trait, lower_trait_ref, TraitRefLowerError},
    ty_def::{AdtDef, AdtRef, AdtRefId, FuncDef, InvalidCause, TyData, TyId},
    ty_lower::{collect_generic_params, lower_adt, lower_kind, GenericParamOwnerId},
    visitor::{walk_ty, TyVisitor},
};
use crate::{
    name_resolution::{resolve_path_early, EarlyResolvedPath, NameDomain, NameResKind},
    ty::{
        diagnostics::{
            AdtDefDiagAccumulator, FuncDefDiagAccumulator, ImplDefDiagAccumulator,
            ImplTraitDefDiagAccumulator, TraitDefDiagAccumulator, TypeAliasDefDiagAccumulator,
        },
        method_table::collect_methods,
        trait_lower::lower_impl_trait,
        ty_lower::{lower_func, lower_hir_ty, lower_type_alias},
        unify::UnificationTable,
    },
    HirAnalysisDb,
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
#[salsa::tracked]
pub fn analyze_adt(db: &dyn HirAnalysisDb, adt_ref: AdtRefId) {
    let analyzer = DefAnalyzer::for_adt(db, adt_ref);
    let diags = analyzer.analyze();

    for diag in diags {
        AdtDefDiagAccumulator::push(db, diag);
    }

    if let Some(diag) = check_recursive_adt(db, adt_ref) {
        AdtDefDiagAccumulator::push(db, diag);
    }
}

/// This function implements analysis for the trait definition.
/// The analysis includes the following:
/// - Check if the types appear in the trait is well-formed.
/// - Check if the trait instantiation appears in the trait is well-formed.
/// - Check if the types in the trait satisfy the constraints which is required
///   in type application.
/// - Check if the trait instantiations in the trait satisfies the constraints.
#[salsa::tracked]
pub fn analyze_trait(db: &dyn HirAnalysisDb, trait_: Trait) {
    let analyzer = DefAnalyzer::for_trait(db, trait_);
    let diags = analyzer.analyze();

    for diag in diags {
        TraitDefDiagAccumulator::push(db, diag);
    }
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
#[salsa::tracked]
pub fn analyze_impl_trait(db: &dyn HirAnalysisDb, impl_trait: ImplTrait) {
    let implementor = match analyze_impl_trait_specific_error(db, impl_trait) {
        Ok(implementor) => implementor,
        Err(diags) => {
            for diag in diags {
                ImplTraitDefDiagAccumulator::push(db, diag);
            }
            return;
        }
    };

    let method_diags = ImplTraitMethodAnalyzer::new(db, implementor).analyze();

    let analyzer = DefAnalyzer::for_trait_impl(db, implementor);
    let diags = analyzer.analyze();
    for diag in method_diags.into_iter().chain(diags) {
        ImplTraitDefDiagAccumulator::push(db, diag);
    }
}

#[salsa::tracked]
pub fn analyze_impl(db: &dyn HirAnalysisDb, impl_: HirImpl) {
    let Some(hir_ty) = impl_.ty(db.as_hir_db()).to_opt() else {
        return;
    };
    let ty = lower_hir_ty(db, hir_ty, impl_.scope());

    let analyzer = DefAnalyzer::for_impl(db, impl_, ty);
    let diags = analyzer.analyze();

    for diag in diags {
        ImplDefDiagAccumulator::push(db, diag);
    }
}

#[salsa::tracked]
pub fn analyze_func(db: &dyn HirAnalysisDb, func: Func) {
    let Some(func_def) = lower_func(db, func) else {
        return;
    };

    let analyzer = DefAnalyzer::for_func(db, func_def);
    let diags = analyzer.analyze();
    for diag in diags {
        FuncDefDiagAccumulator::push(db, diag);
    }
}

/// This function implements analysis for the type alias definition.
/// The analysis includes the following:
/// - Check if the type alias is not recursive.
/// - Check if the type in the type alias is well-formed.
///
/// NOTE: This function doesn't check the satisfiability of the type since our
/// type system treats the alias as kind of macro, meaning type alias doesn't
/// included in the type system. Satisfiability is checked where the type alias
/// is used.
#[salsa::tracked]
pub fn analyze_type_alias(db: &dyn HirAnalysisDb, alias: TypeAlias) {
    let Some(hir_ty) = alias.ty(db.as_hir_db()).to_opt() else {
        return;
    };

    let ty = lower_hir_ty(db, hir_ty, alias.scope());

    if let Err(cycle) = lower_type_alias(db, alias) {
        if cycle.representative() == alias {
            TypeAliasDefDiagAccumulator::push(
                db,
                TyLowerDiag::TypeAliasCycle {
                    primary: alias.lazy_span().ty().into(),
                    cycle: cycle.participants().collect(),
                }
                .into(),
            );
        }
        return;
    }

    // We don't need to check for bound satisfiability here because type alias
    // doesn't have trait bound, it will be checked where the type alias is used.
    if let Some(diag) = ty.emit_diag(db, alias.lazy_span().ty().into()) {
        TypeAliasDefDiagAccumulator::push(db, diag);
    }
}

pub struct DefAnalyzer<'db> {
    db: &'db dyn HirAnalysisDb,
    def: DefKind,
    self_ty: Option<TyId>,
    diags: Vec<TyDiagCollection>,
    assumptions: AssumptionListId,
    current_ty: Option<(TyId, DynLazySpan)>,
}

impl<'db> DefAnalyzer<'db> {
    fn for_adt(db: &'db dyn HirAnalysisDb, adt: AdtRefId) -> Self {
        let def = lower_adt(db, adt);
        let assumptions = def.constraints(db);
        Self {
            db,
            def: def.into(),
            self_ty: None,
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    fn for_trait(db: &'db dyn HirAnalysisDb, trait_: Trait) -> Self {
        let def = lower_trait(db, trait_);
        let assumptions = def.constraints(db);
        Self {
            db,
            def: def.into(),
            self_ty: def.self_param(db).into(),
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    fn for_impl(db: &'db dyn HirAnalysisDb, impl_: HirImpl, ty: TyId) -> Self {
        let assumptions = collect_impl_block_constraints(db, impl_);
        let def = DefKind::Impl(impl_, ty);
        Self {
            db,
            def,
            self_ty: ty.into(),
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    fn for_trait_impl(db: &'db dyn HirAnalysisDb, implementor: Implementor) -> Self {
        let assumptions = implementor.constraints(db);
        Self {
            db,
            def: implementor.into(),
            self_ty: implementor.ty(db).into(),
            diags: vec![],
            assumptions,
            current_ty: None,
        }
    }

    fn for_func(db: &'db dyn HirAnalysisDb, func: FuncDef) -> Self {
        let hir_db = db.as_hir_db();
        let assumptions = func.constraints(db);
        let self_ty = match func.hir_func(db).scope().parent(hir_db).unwrap() {
            ScopeId::Item(ItemKind::Trait(trait_)) => lower_trait(db, trait_).self_param(db).into(),
            ScopeId::Item(ItemKind::ImplTrait(impl_trait)) => {
                match impl_trait.ty(hir_db).to_opt() {
                    Some(hir_ty) => lower_hir_ty(db, hir_ty, impl_trait.scope()).into(),
                    _ => TyId::invalid(db, InvalidCause::Other).into(),
                }
            }
            ScopeId::Item(ItemKind::Impl(impl_)) => match impl_.ty(hir_db).to_opt() {
                Some(hir_ty) => lower_hir_ty(db, hir_ty, impl_.scope()).into(),
                None => TyId::invalid(db, InvalidCause::Other).into(),
            },
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

    /// This method verifies if
    /// 1. the given `ty` has `*` kind(i.e, concrete type)
    /// 2. the given `ty` is not const type
    /// TODo: This method is a stop-gap implementation until we design a true
    /// const type system.
    fn verify_normal_star_type(&mut self, ty: HirTyId, span: DynLazySpan) -> bool {
        let ty = lower_hir_ty(self.db, ty, self.scope());
        if !ty.has_star_kind(self.db) {
            self.diags
                .push(TyLowerDiag::expected_star_kind_ty(span).into());
            false
        } else if ty.is_const_ty(self.db) {
            self.diags
                .push(TyLowerDiag::normal_type_expected(self.db, span, ty).into());
            false
        } else {
            true
        }
    }

    fn verify_self_type(&mut self, self_ty: HirTyId, span: DynLazySpan) -> bool {
        let expected_ty = self.self_ty.unwrap();

        let param_ty = lower_hir_ty(self.db, self_ty, self.def.scope(self.db));
        if !param_ty.contains_invalid(self.db) && !expected_ty.contains_invalid(self.db) {
            let (expected_base_ty, expected_param_ty_args) = expected_ty.decompose_ty_app(self.db);
            let (param_base_ty, param_ty_args) = param_ty.decompose_ty_app(self.db);

            if param_base_ty != expected_base_ty {
                self.diags.push(
                    ImplDiag::invalid_self_ty(self.db, span.clone(), expected_ty, param_ty).into(),
                );
                return false;
            }

            for (expected_arg, param_arg) in expected_param_ty_args.iter().zip(param_ty_args.iter())
            {
                if expected_arg != param_arg {
                    self.diags.push(
                        ImplDiag::invalid_self_ty(self.db, span, expected_ty, param_ty).into(),
                    );
                    return false;
                }
            }
        }

        true
    }

    fn verify_method_conflict(&mut self, func: FuncDef) -> bool {
        let self_ty = func
            .receiver_ty(self.db)
            .unwrap_or_else(|| self.self_ty.unwrap());

        if self_ty.contains_invalid(self.db) {
            return true;
        }

        let method_table = collect_methods(self.db, func.ingot(self.db));
        let maybe_conflict = match method_table.prove_eager(self.db, self_ty, func.name(self.db)) {
            Some(func_def) => func_def,
            None => unreachable!(),
        };

        if maybe_conflict != func {
            self.diags.push(
                ImplDiag::conflict_method_impl(
                    func.hir_func(self.db).lazy_span().name().into(),
                    maybe_conflict.hir_func(self.db).lazy_span().name().into(),
                )
                .into(),
            );
            false
        } else {
            true
        }
    }

    fn scope(&self) -> ScopeId {
        self.def.scope(self.db)
    }

    fn analyze(mut self) -> Vec<TyDiagCollection> {
        match self.def {
            DefKind::Adt(def) => match def.adt_ref(self.db).data(self.db) {
                AdtRef::Struct(struct_) => {
                    let mut ctxt = VisitorCtxt::with_struct(self.db.as_hir_db(), struct_);
                    self.visit_struct(&mut ctxt, struct_);
                }

                AdtRef::Enum(enum_) => {
                    let mut ctxt = VisitorCtxt::with_enum(self.db.as_hir_db(), enum_);
                    self.visit_enum(&mut ctxt, enum_);
                }

                AdtRef::Contract(contract) => {
                    let mut ctxt = VisitorCtxt::with_contract(self.db.as_hir_db(), contract);
                    self.visit_contract(&mut ctxt, contract);
                }
            },

            DefKind::Trait(trait_) => {
                let trait_ = trait_.trait_(self.db);
                let mut ctxt = VisitorCtxt::with_trait(self.db.as_hir_db(), trait_);
                self.visit_trait(&mut ctxt, trait_);
            }

            DefKind::ImplTrait(implementor) => {
                let impl_trait = implementor.hir_impl_trait(self.db);
                let mut ctxt = VisitorCtxt::with_impl_trait(self.db.as_hir_db(), impl_trait);
                self.visit_impl_trait(&mut ctxt, impl_trait);
            }

            DefKind::Impl(hir_impl, _) => {
                let mut ctxt = VisitorCtxt::with_impl(self.db.as_hir_db(), hir_impl);
                self.visit_impl(&mut ctxt, hir_impl)
            }

            DefKind::Func(func) => {
                let func = func.hir_func(self.db);
                let mut ctxt = VisitorCtxt::with_func(self.db.as_hir_db(), func);
                self.visit_func(&mut ctxt, func);
            }
        }

        self.diags
    }
}

impl<'db> Visitor for DefAnalyzer<'db> {
    // We don't need to traverse the nested item, each item kinds are explicitly
    // handled(e.g, `visit_trait` or `visit_enum`).
    fn visit_item(&mut self, _ctxt: &mut VisitorCtxt<'_, LazyItemSpan>, _item: ItemKind) {}

    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'_, LazyTySpan>, hir_ty: HirTyId) {
        let ty = lower_hir_ty(self.db, hir_ty, self.scope());
        let span = ctxt.span().unwrap();
        if let Some(diag) = ty.emit_diag(self.db, span.clone().into()) {
            self.diags.push(diag)
        } else if let Some(diag) = ty.emit_sat_diag(self.db, self.assumptions, span.into()) {
            self.diags.push(diag)
        }
    }

    fn visit_where_predicate(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyWherePredicateSpan>,
        pred: &hir::hir_def::WherePredicate,
    ) {
        let Some(hir_ty) = pred.ty.to_opt() else {
            return;
        };

        let ty = lower_hir_ty(self.db, hir_ty, self.scope());

        if !ty.contains_invalid(self.db) && !ty.contains_ty_param(self.db) {
            let diag = TraitConstraintDiag::concrete_type_bound(
                self.db,
                ctxt.span().unwrap().ty().into(),
                ty,
            )
            .into();
            self.diags.push(diag);
            return;
        }

        if ty.is_const_ty(self.db) {
            let diag =
                TraitConstraintDiag::const_ty_bound(self.db, ty, ctxt.span().unwrap().ty().into())
                    .into();
            self.diags.push(diag);
            return;
        }

        self.current_ty = Some((ty, ctxt.span().unwrap().ty().into()));
        walk_where_predicate(self, ctxt, pred);
    }

    fn visit_field_def(&mut self, ctxt: &mut VisitorCtxt<'_, LazyFieldDefSpan>, field: &FieldDef) {
        let Some(ty) = field.ty.to_opt() else {
            return;
        };

        if !self.verify_normal_star_type(ty, ctxt.span().unwrap().ty().into()) {
            return;
        }

        let Some(name) = field.name.to_opt() else {
            return;
        };

        // Checks if the field type is the same as the type of const type parameter.
        if let Some(const_ty) = find_const_ty_param(self.db, name, ctxt.scope()) {
            let const_ty_ty = const_ty.ty(self.db);
            let field_ty = lower_hir_ty(self.db, ty, ctxt.scope());
            if !const_ty_ty.contains_invalid(self.db)
                && !field_ty.contains_invalid(self.db)
                && field_ty != const_ty_ty
            {
                self.diags.push(
                    TyLowerDiag::const_ty_mismatch(
                        self.db,
                        ctxt.span().unwrap().ty().into(),
                        const_ty_ty,
                        field_ty,
                    )
                    .into(),
                );
                return;
            }
        }

        walk_field_def(self, ctxt, field);
    }

    fn visit_variant_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyVariantDefSpan>,
        variant: &hir::hir_def::VariantDef,
    ) {
        if let VariantKind::Tuple(tuple_id) = variant.kind {
            let span = ctxt.span().unwrap().tuple_type_moved();
            for (i, elem_ty) in tuple_id.data(self.db.as_hir_db()).iter().enumerate() {
                let Some(elem_ty) = elem_ty.to_opt() else {
                    continue;
                };

                self.verify_normal_star_type(elem_ty, span.elem_ty(i).into());
            }
        }
        walk_variant_def(self, ctxt, variant);
    }

    fn visit_generic_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyGenericParamSpan>,
        param: &hir::hir_def::GenericParam,
    ) {
        let ScopeId::GenericParam(_, idx) = ctxt.scope() else {
            unreachable!()
        };

        // Check if the same generic parameter is already defined in the parent item.
        // Other name conflict check is done in the name resolution.
        //
        // This check is necessary because the conflict rule
        // for the generic parameter is the exceptional case where shadowing shouldn't
        // occur.
        if let Some(name) = param.name().to_opt() {
            let scope = self.scope();
            let parent_scope = scope.parent_item(self.db.as_hir_db()).unwrap().scope();
            let path = PathId::from_ident(self.db.as_hir_db(), name);
            if let EarlyResolvedPath::Full(bucket) = resolve_path_early(self.db, path, parent_scope)
            {
                if let Ok(res) = bucket.pick(NameDomain::Type) {
                    if let NameResKind::Scope(conflict_with @ ScopeId::GenericParam(..)) = res.kind
                    {
                        self.diags.push(
                            TyLowerDiag::generic_param_conflict(
                                ctxt.span().unwrap().into(),
                                conflict_with.name_span(self.db.as_hir_db()).unwrap(),
                                name,
                            )
                            .into(),
                        );

                        return;
                    }
                }
            }
        }

        match param {
            GenericParam::Type(_) => {
                self.current_ty = Some((
                    self.def.params(self.db)[idx],
                    ctxt.span().unwrap().into_type_param().name().into(),
                ));
                walk_generic_param(self, ctxt, param)
            }
            GenericParam::Const(_) => {
                let ty = self.def.params(self.db)[idx];
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

    fn visit_kind_bound(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyKindBoundSpan>,
        bound: &hir::hir_def::KindBound,
    ) {
        let Some((ty, _)) = self.current_ty else {
            return;
        };

        let kind = lower_kind(bound);
        let former_kind = ty.kind(self.db);
        if !former_kind.does_match(&kind) {
            self.diags.push(
                TyLowerDiag::inconsistent_kind_bound(
                    self.db,
                    ctxt.span().unwrap().into(),
                    ty,
                    former_kind,
                    &kind,
                )
                .into(),
            );
        }
    }

    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyTraitRefSpan>,
        trait_ref: TraitRefId,
    ) {
        if self
            .current_ty
            .as_ref()
            .map(|(ty, _)| ty.is_trait_self(self.db))
            .unwrap_or_default()
        {
            if let Some(cycle) = self.def.collect_super_trait_cycle(self.db) {
                if let Ok(trait_inst) = lower_trait_ref(self.db, trait_ref, self.scope()) {
                    if cycle.contains(trait_inst.def(self.db)) {
                        self.diags.push(
                            TraitLowerDiag::CyclicSuperTraits(ctxt.span().unwrap().path().into())
                                .into(),
                        );
                        return;
                    }
                }
            }
        }

        if let (Some((ty, span)), Ok(trait_inst)) = (
            &self.current_ty,
            lower_trait_ref(self.db, trait_ref, self.scope()),
        ) {
            let expected_kind = trait_inst.def(self.db).expected_implementor_kind(self.db);
            if !expected_kind.does_match(ty.kind(self.db)) {
                self.diags.push(
                    TraitConstraintDiag::kind_mismatch(self.db, span.clone(), expected_kind, *ty)
                        .into(),
                );
            }
        }

        if let Some(diag) = analyze_trait_ref(
            self.db,
            trait_ref,
            self.scope(),
            Some(self.assumptions),
            ctxt.span().unwrap().into(),
        ) {
            self.diags.push(diag);
        } else {
            walk_trait_ref(self, ctxt, trait_ref);
        }
    }

    fn visit_super_trait_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, hir::span::item::LazySuperTraitListSpan>,
        super_traits: &[TraitRefId],
    ) {
        let DefKind::Trait(def) = self.def else {
            unreachable!()
        };
        let name_span = def.trait_(self.db).lazy_span().name().into();
        self.current_ty = Some((self.def.trait_self_param(self.db), name_span));
        walk_super_trait_list(self, ctxt, super_traits);
    }

    fn visit_impl(&mut self, ctxt: &mut VisitorCtxt<'_, LazyImplSpan>, impl_: HirImpl) {
        let Some(impl_ty) = impl_.ty(self.db.as_hir_db()).to_opt() else {
            return;
        };

        let impl_ty = lower_hir_ty(self.db, impl_ty, impl_.scope());
        if let Some(ty) = impl_ty.emit_diag(self.db, ctxt.span().unwrap().target_ty().into()) {
            self.diags.push(ty);
        } else {
            walk_impl(self, ctxt, impl_);
        }
    }

    fn visit_func(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyFuncSpan>,
        hir_func: hir::hir_def::Func,
    ) {
        let Some(func) = lower_func(self.db, hir_func) else {
            return;
        };

        // We need to check the conflict only when the function is defined in the `impl`
        // block since this check requires the ingot-wide method table(i.e., which is
        // not performed in name resolution phase).
        if matches!(
            ctxt.scope().parent_item(self.db.as_hir_db()).unwrap(),
            ItemKind::Impl(_)
        ) && !self.verify_method_conflict(func)
        {
            return;
        }

        let def = std::mem::replace(&mut self.def, func.into());
        let constraints = std::mem::replace(&mut self.assumptions, func.constraints(self.db));

        walk_func(self, ctxt, hir_func);

        if let Some(ret_ty) = hir_func.ret_ty(self.db.as_hir_db()) {
            self.verify_normal_star_type(ret_ty, hir_func.lazy_span().ret_ty().into());
        }

        self.assumptions = constraints;
        self.def = def;
    }

    fn visit_func_param_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyFuncParamListSpan>,
        params: FuncParamListId,
    ) {
        // Checks if the argument names are not duplicated.
        let mut already_seen: FxHashMap<IdentId, usize> = FxHashMap::default();

        for (i, param) in params.data(self.db.as_hir_db()).iter().enumerate() {
            let Some(name) = param.name.to_opt().and_then(|name| name.ident()) else {
                continue;
            };

            match already_seen.entry(name) {
                Entry::Occupied(entry) => {
                    let diag = TyLowerDiag::duplicated_arg_name(
                        ctxt.span().unwrap().param(i).name().into(),
                        ctxt.span().unwrap().param(*entry.get()).name().into(),
                        name,
                    )
                    .into();
                    self.diags.push(diag);
                }

                Entry::Vacant(entry) => {
                    entry.insert(i);
                }
            }
        }

        walk_func_param_list(self, ctxt, params)
    }

    fn visit_func_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'_, LazyFuncParamSpan>,
        param: &hir::hir_def::FuncParam,
    ) {
        let Some(hir_ty) = param.ty.to_opt() else {
            return;
        };

        let ty_span: DynLazySpan = if param.is_self_param() && param.self_ty_fallback {
            ctxt.span().unwrap().name().into()
        } else {
            ctxt.span().unwrap().ty().into()
        };

        if param.is_self_param() {
            self.verify_self_type(hir_ty, ty_span.clone());
        }

        if !self.verify_normal_star_type(hir_ty, ty_span) {
            return;
        }

        walk_func_param(self, ctxt, param);
    }
}

#[salsa::tracked(recovery_fn = check_recursive_adt_impl)]
pub(crate) fn check_recursive_adt(
    db: &dyn HirAnalysisDb,
    adt: AdtRefId,
) -> Option<TyDiagCollection> {
    let adt_def = lower_adt(db, adt);
    for field in adt_def.fields(db) {
        for ty in field.iter_types(db) {
            for adt_ref in ty.collect_direct_adts(db) {
                check_recursive_adt(db, adt_ref);
            }
        }
    }

    None
}

fn check_recursive_adt_impl(
    db: &dyn HirAnalysisDb,
    cycle: &salsa::Cycle,
    adt: AdtRefId,
) -> Option<TyDiagCollection> {
    let participants: FxHashSet<_> = cycle
        .participant_keys()
        .map(|key| check_recursive_adt::key_from_id(key.key_index()))
        .collect();

    let adt_def = lower_adt(db, adt);
    for (field_idx, field) in adt_def.fields(db).iter().enumerate() {
        for (ty_idx, ty) in field.iter_types(db).enumerate() {
            for field_adt_ref in ty.collect_direct_adts(db) {
                if participants.contains(&field_adt_ref) && participants.contains(&adt) {
                    let diag = TyLowerDiag::recursive_type(
                        adt.name_span(db),
                        adt_def.variant_ty_span(db, field_idx, ty_idx),
                    );
                    return Some(diag.into());
                }
            }
        }
    }

    None
}

impl TyId {
    /// Collect all adts inside types which are not wrapped by indirect type
    /// wrapper like pointer or reference.
    fn collect_direct_adts(self, db: &dyn HirAnalysisDb) -> FxHashSet<AdtRefId> {
        struct AdtCollector {
            adts: FxHashSet<AdtRefId>,
        }

        impl TyVisitor for AdtCollector {
            fn visit_app(&mut self, db: &dyn HirAnalysisDb, abs: TyId, arg: TyId) {
                if !abs.is_indirect(db) {
                    walk_ty(self, db, arg)
                }
            }

            fn visit_adt(&mut self, db: &dyn HirAnalysisDb, adt: AdtDef) {
                self.adts.insert(adt.adt_ref(db));
            }
        }

        let mut collector = AdtCollector {
            adts: FxHashSet::default(),
        };

        walk_ty(&mut collector, db, self);
        collector.adts
    }
}

fn analyze_trait_ref(
    db: &dyn HirAnalysisDb,
    trait_ref: TraitRefId,
    scope: ScopeId,
    assumptions: Option<AssumptionListId>,
    span: DynLazySpan,
) -> Option<TyDiagCollection> {
    let trait_inst = match lower_trait_ref(db, trait_ref, scope) {
        Ok(trait_ref) => trait_ref,

        Err(TraitRefLowerError::ArgNumMismatch { expected, given }) => {
            return Some(TraitConstraintDiag::trait_arg_num_mismatch(span, expected, given).into());
        }

        Err(TraitRefLowerError::ArgKindMisMatch { expected, given }) => {
            return Some(TraitConstraintDiag::kind_mismatch(db, span, &expected, given).into());
        }

        Err(TraitRefLowerError::AssocTy(_)) => {
            return Some(TyLowerDiag::assoc_ty(span).into());
        }

        Err(TraitRefLowerError::ArgTypeMismatch { expected, given }) => match (expected, given) {
            (Some(expected), Some(given)) => {
                return Some(TyLowerDiag::const_ty_mismatch(db, span, expected, given).into())
            }

            (Some(expected), None) => {
                return Some(TyLowerDiag::const_ty_expected(db, span, expected).into())
            }

            (None, Some(given)) => {
                return Some(TyLowerDiag::normal_type_expected(db, span, given).into())
            }

            (None, None) => unreachable!(),
        },

        Err(TraitRefLowerError::Other) => {
            return None;
        }
    };

    if let Some(assumptions) = assumptions {
        trait_inst.emit_sat_diag(db, assumptions, span)
    } else {
        None
    }
}

#[derive(Clone, Copy, Debug, derive_more::From)]
enum DefKind {
    Adt(AdtDef),
    Trait(TraitDef),
    ImplTrait(Implementor),
    Impl(HirImpl, TyId),
    Func(FuncDef),
}

impl DefKind {
    fn params(self, db: &dyn HirAnalysisDb) -> &[TyId] {
        match self {
            Self::Adt(def) => def.params(db),
            Self::Trait(def) => def.params(db),
            Self::ImplTrait(def) => def.params(db),
            Self::Impl(hir_impl, _) => {
                collect_generic_params(db, GenericParamOwnerId::new(db, hir_impl.into())).params(db)
            }
            Self::Func(def) => def.params(db),
        }
    }

    fn trait_self_param(self, db: &dyn HirAnalysisDb) -> TyId {
        if let Self::Trait(def) = self {
            def.self_param(db)
        } else {
            panic!()
        }
    }

    fn collect_super_trait_cycle(self, db: &dyn HirAnalysisDb) -> Option<&SuperTraitCycle> {
        if let Self::Trait(def) = self {
            collect_super_traits(db, def).as_ref().err()
        } else {
            None
        }
    }

    fn scope(self, db: &dyn HirAnalysisDb) -> ScopeId {
        match self {
            Self::Adt(def) => def.adt_ref(db).scope(db),
            Self::Trait(def) => def.trait_(db).scope(),
            Self::ImplTrait(def) => def.hir_impl_trait(db).scope(),
            Self::Impl(hir_impl, _) => hir_impl.scope(),
            Self::Func(def) => def.hir_func(db).scope(),
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
fn analyze_impl_trait_specific_error(
    db: &dyn HirAnalysisDb,
    impl_trait: ImplTrait,
) -> Result<Implementor, Vec<TyDiagCollection>> {
    let mut diags = vec![];
    let hir_db = db.as_hir_db();
    // We don't need to report error because it should be reported from the parser.
    let (Some(trait_ref), Some(ty)) = (
        impl_trait.trait_ref(hir_db).to_opt(),
        impl_trait.ty(hir_db).to_opt(),
    ) else {
        return Err(diags);
    };

    // 1. Checks if the trait ref is well-formed except for the satisfiability.
    if let Some(diag) = analyze_trait_ref(
        db,
        trait_ref,
        impl_trait.scope(),
        None,
        impl_trait.lazy_span().trait_ref().into(),
    ) {
        diags.push(diag);
    }

    // 2. Checks if implementor type is well-formed except for the satisfiability.
    let ty = lower_hir_ty(db, ty, impl_trait.scope());
    if let Some(diag) = ty.emit_diag(db, impl_trait.lazy_span().ty().into()) {
        diags.push(diag);
    }

    // If there is any error at the point, it means that `Implementor` is not
    // well-formed and no more analysis is needed to reduce the amount of error
    // messages.
    if !diags.is_empty() || ty.contains_invalid(db) {
        return Err(diags);
    }

    let trait_inst = match lower_trait_ref(db, trait_ref, impl_trait.scope()) {
        Ok(trait_inst) => trait_inst,
        Err(_) => return Err(vec![]),
    };

    // 3. Check if the ingot contains impl trait is the same as the ingot which
    //    contains either the type or trait.
    let impl_trait_ingot = impl_trait.top_mod(hir_db).ingot(hir_db);
    if Some(impl_trait_ingot) != ty.ingot(db) && impl_trait_ingot != trait_inst.def(db).ingot(db) {
        diags.push(TraitLowerDiag::external_trait_for_external_type(impl_trait).into());
        return Err(diags);
    }

    let trait_env = ingot_trait_env(db, impl_trait.top_mod(hir_db).ingot(hir_db));
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

    fn analyze_conflict_impl(
        db: &dyn HirAnalysisDb,
        implementor: Implementor,
        diags: &mut Vec<TyDiagCollection>,
    ) {
        let trait_ = implementor.trait_(db);
        let env = ingot_trait_env(db, trait_.ingot(db));
        let Some(impls) = env.impls.get(&trait_.def(db)) else {
            return;
        };

        for cand in impls {
            if cand.does_conflict(db, implementor, &mut UnificationTable::new(db)) {
                diags.push(
                    TraitLowerDiag::conflict_impl(
                        cand.hir_impl_trait(db),
                        implementor.hir_impl_trait(db),
                    )
                    .into(),
                );

                return;
            }
        }
    }

    // 5. Checks if implementor type satisfies the kind bound which is required by
    //    the trait.
    let expected_kind = implementor.trait_def(db).expected_implementor_kind(db);
    if ty.kind(db) != expected_kind {
        diags.push(
            TraitConstraintDiag::kind_mismatch(
                db,
                impl_trait.lazy_span().ty().into(),
                expected_kind,
                implementor.ty(db),
            )
            .into(),
        );
        return Err(diags);
    }

    // 6. Checks if the implementor ty satisfies the trait constraints required by
    //    the trait.
    let mut subst = trait_inst.subst_table(db);
    let trait_def = trait_inst.def(db);
    subst.insert(trait_def.self_param(db), ty);
    let trait_constraints = trait_def.constraints(db);
    let assumptions = implementor.constraints(db);

    for goal in trait_constraints.predicates(db) {
        if !goal.ty(db).contains_trait_self(db) {
            continue;
        }
        let goal = goal.apply_subst(db, &mut subst);
        match is_goal_satisfiable(db, goal, assumptions) {
            GoalSatisfiability::Satisfied => {}
            GoalSatisfiability::NotSatisfied(_) => {
                diags.push(
                    TraitConstraintDiag::trait_bound_not_satisfied(
                        db,
                        impl_trait.lazy_span().ty().into(),
                        goal,
                    )
                    .into(),
                );
                return Err(diags);
            }
            GoalSatisfiability::InfiniteRecursion(_) => {
                diags.push(
                    TraitConstraintDiag::infinite_bound_recursion(
                        db,
                        impl_trait.lazy_span().ty().into(),
                        goal,
                    )
                    .into(),
                );
                return Err(diags);
            }
        }
    }

    Ok(implementor)
}

struct ImplTraitMethodAnalyzer<'db> {
    db: &'db dyn HirAnalysisDb,
    diags: Vec<TyDiagCollection>,
    implementor: Implementor,
}

impl<'db> ImplTraitMethodAnalyzer<'db> {
    fn new(db: &'db dyn HirAnalysisDb, implementor: Implementor) -> Self {
        Self {
            db,
            diags: vec![],
            implementor,
        }
    }

    fn analyze(mut self) -> Vec<TyDiagCollection> {
        let impl_methods = self.implementor.methods(self.db);
        let hir_trait = self.implementor.trait_def(self.db).trait_(self.db);
        let trait_methods = self.implementor.trait_def(self.db).methods(self.db);
        let mut required_methods: BTreeSet<_> = trait_methods
            .iter()
            .filter_map(|(name, trait_method)| {
                if !trait_method.has_default_impl(self.db) {
                    Some(*name)
                } else {
                    None
                }
            })
            .collect();

        for (name, impl_method) in impl_methods {
            let Some(trait_method) = trait_methods.get(name) else {
                self.diags.push(
                    ImplDiag::method_not_defined_in_trait(
                        self.implementor
                            .hir_impl_trait(self.db)
                            .lazy_span()
                            .trait_ref()
                            .into(),
                        hir_trait,
                        *name,
                    )
                    .into(),
                );
                continue;
            };

            required_methods.remove(name);
            self.analyze_method(*impl_method, *trait_method);
        }

        if !required_methods.is_empty() {
            self.diags.push(
                ImplDiag::not_all_trait_items_implemented(
                    self.implementor
                        .hir_impl_trait(self.db)
                        .lazy_span()
                        .ty_moved()
                        .into(),
                    required_methods,
                )
                .into(),
            );
        }

        self.diags
    }

    fn analyze_method(&mut self, impl_method: FuncDef, expected_method: TraitMethod) {
        // TODO: We need to check label integrity.
        let mut subst = self.implementor.subst_table(self.db);

        let mut is_err = false;
        let hir_impl_method = impl_method.hir_func(self.db);
        let hir_expected_method = expected_method.0.hir_func(self.db);

        // Checks if the number of parameters are the same.
        let method_params = impl_method.params(self.db);
        let expected_params = expected_method.0.params(self.db);
        if method_params.len() != expected_params.len() {
            self.diags.push(
                ImplDiag::method_param_num_mismatch(
                    hir_impl_method.lazy_span().name().into(),
                    expected_params.len(),
                    method_params.len(),
                )
                .into(),
            );
            is_err = true;
        };

        if is_err {
            return;
        }

        // Checks if the parameter kinds are the same.
        for (idx, (&expected_param, &method_param)) in
            expected_params.iter().zip(method_params).enumerate()
        {
            let expected_kind = expected_param.kind(self.db);
            let given_kind = method_param.kind(self.db);

            if expected_kind != given_kind {
                let span = hir_impl_method
                    .lazy_span()
                    .generic_params()
                    .param(idx)
                    .into();
                self.diags.push(
                    ImplDiag::method_param_kind_mismatch(span, expected_kind, given_kind).into(),
                );
                is_err = true;
            }

            subst.insert(expected_param, method_param);
        }

        if is_err {
            return;
        }

        let expected_arg_tys = expected_method.0.arg_tys(self.db);
        let method_arg_tys = impl_method.arg_tys(self.db);

        // Checks if the arity are the same.
        if expected_arg_tys.len() != method_arg_tys.len() {
            self.diags.push(
                ImplDiag::method_arg_num_mismatch(
                    hir_impl_method.lazy_span().params_moved().into(),
                    expected_arg_tys.len(),
                    method_arg_tys.len(),
                )
                .into(),
            );
            is_err = true;
        }

        if is_err {
            return;
        }

        // Checks if the argument labels are the same.
        for (idx, (expected_param, method_param)) in expected_method
            .0
            .hir_params(self.db)
            .iter()
            .zip(impl_method.hir_params(self.db))
            .enumerate()
        {
            let Some(expected_label) = expected_param
                .label
                .or_else(|| expected_param.name.to_opt())
            else {
                continue;
            };

            let Some(method_label) = method_param.label.or_else(|| method_param.name.to_opt())
            else {
                continue;
            };

            if expected_label != method_label {
                let primary = hir_impl_method.lazy_span().params_moved().param(idx).into();
                let sub = hir_expected_method
                    .lazy_span()
                    .params_moved()
                    .param(idx)
                    .into();

                self.diags.push(
                    ImplDiag::method_arg_label_mismatch(
                        self.db,
                        primary,
                        sub,
                        expected_label,
                        method_label,
                    )
                    .into(),
                );
                is_err = true;
            }
        }

        // Checks if the argument types are the same.
        for (idx, (expected_arg_ty, &method_arg_ty)) in
            expected_arg_tys.iter().zip(method_arg_tys).enumerate()
        {
            let expected_arg_ty = expected_arg_ty.apply_subst(self.db, &mut subst);
            if !method_arg_ty.contains_invalid(self.db) && expected_arg_ty != method_arg_ty {
                let span = impl_method
                    .hir_func(self.db)
                    .lazy_span()
                    .params_moved()
                    .param(idx)
                    .into();
                self.diags.push(
                    ImplDiag::method_arg_ty_mismatch(self.db, span, expected_arg_ty, method_arg_ty)
                        .into(),
                );
                is_err = true;
            }
        }

        // Checks if the return type is the same.
        let expected_ret_ty = expected_method
            .0
            .ret_ty(self.db)
            .apply_subst(self.db, &mut subst);
        let method_ret_ty = impl_method.ret_ty(self.db);
        if !method_ret_ty.contains_invalid(self.db) && expected_ret_ty != method_ret_ty {
            self.diags.push(
                ImplDiag::method_ret_type_mismatch(
                    self.db,
                    impl_method.hir_func(self.db).lazy_span().ret_ty().into(),
                    expected_ret_ty,
                    method_ret_ty,
                )
                .into(),
            );

            is_err = true;
        }

        if is_err {
            return;
        }

        // Check if the method constraints are stricter than the trait constraints.
        // This check can be performed to check if the `impl_method` constraints are
        // satisfied under the assumptions that is obtained from the `expected_method`
        // constraints.
        let expected_constraints = expected_method
            .0
            .constraints(self.db)
            .apply_subst(self.db, &mut subst);
        let method_constraints = impl_method.constraints(self.db);
        let mut unsatisfied_goals = vec![];
        for &goal in method_constraints.predicates(self.db) {
            if !matches!(
                is_goal_satisfiable(self.db, goal, expected_constraints),
                GoalSatisfiability::Satisfied
            ) {
                unsatisfied_goals.push(goal);
            }
        }

        if !unsatisfied_goals.is_empty() {
            self.diags.push(
                ImplDiag::method_stricter_bound(
                    self.db,
                    impl_method.hir_func(self.db).lazy_span().name().into(),
                    &unsatisfied_goals,
                )
                .into(),
            );
        }
    }
}

fn find_const_ty_param(
    db: &dyn HirAnalysisDb,
    ident: IdentId,
    scope: ScopeId,
) -> Option<ConstTyId> {
    let path = PathId::from_ident(db.as_hir_db(), ident);
    let EarlyResolvedPath::Full(bucket) = resolve_path_early(db, path, scope) else {
        return None;
    };

    let res = bucket.pick(NameDomain::Value).as_ref().ok()?;
    let NameResKind::Scope(scope) = res.kind else {
        return None;
    };

    let (item, idx) = match scope {
        ScopeId::GenericParam(item, idx) => (item, idx),
        _ => return None,
    };

    let owner = GenericParamOwnerId::from_item_opt(db, item).unwrap();
    let param_set = collect_generic_params(db, owner);
    let ty = param_set.params(db).get(idx)?;
    match ty.data(db) {
        TyData::ConstTy(const_ty) => Some(*const_ty),
        _ => None,
    }
}
