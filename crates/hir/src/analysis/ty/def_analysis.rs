//! This module contains analysis for the definition of the type/trait.
//! This module is the only module in `ty` module which is allowed to emit
//! diagnostics.

use crate::{
    hir_def::{
        EnumVariant, FieldDef, FieldParent, Func, GenericParam, GenericParamListId,
        GenericParamOwner, IdentId, Impl as HirImpl, ImplTrait, ItemKind, PathId, Trait,
        TraitRefId, TypeAlias, TypeId as HirTyId, VariantKind, WhereClauseOwner,
        scope_graph::ScopeId,
    },
    hir_def::semantic::WherePredicateView,
    visitor::prelude::*,
};
use std::ptr;
use common::indexmap::IndexSet;
use rustc_hash::FxHashMap;
use smallvec1::SmallVec;

use super::{
    adt_def::{AdtRef, lower_adt},
    canonical::Canonical,
    const_ty::ConstTyId,
    diagnostics::{ImplDiag, TraitConstraintDiag, TraitLowerDiag, TyDiagCollection, TyLowerDiag},
    func_def::FuncDef,
    method_cmp::compare_impl_method,
    method_table::probe_method,
    trait_def::{Implementor, TraitDef, ingot_trait_env},
    trait_lower::{TraitRefLowerError, lower_trait, lower_trait_ref},
    trait_resolution::{
        PredicateListId,
        constraint::{collect_adt_constraints, collect_constraints, collect_func_def_constraints},
    },
    ty_def::{InvalidCause, TyData, TyId},
    ty_error::collect_ty_lower_errors,
    ty_lower::{collect_generic_params, lower_hir_ty},
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{ExpectedPathKind, PathRes, diagnostics::PathResDiag, resolve_path},
    ty::{
        adt_def::AdtDef,
        binder::Binder,
        func_def::lower_func,
        trait_def::{does_impl_trait_conflict},
        trait_lower::lower_impl_trait,
        trait_resolution::{
            constraint::super_trait_cycle,
        },
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
    // Non-trailing default generic params via semantic traversal
    match adt_ref {
        AdtRef::Struct(s) => {
            diags.extend(GenericParamOwner::Struct(s).diags_params_defined_in_parent(db));
            diags.extend(GenericParamOwner::Struct(s).diags_kind_bounds(db));
            diags.extend(GenericParamOwner::Struct(s).diags_trait_bounds(db));
            diags.extend(GenericParamOwner::Struct(s).diags_non_trailing_defaults(db));
            diags.extend(GenericParamOwner::Struct(s).diags_default_forward_refs(db));
        }
        AdtRef::Contract(_c) => {/* contracts are not GenericParamOwner */}
        AdtRef::Enum(e) => {
            diags.extend(GenericParamOwner::Enum(e).diags_params_defined_in_parent(db));
            diags.extend(GenericParamOwner::Enum(e).diags_kind_bounds(db));
            diags.extend(GenericParamOwner::Enum(e).diags_trait_bounds(db));
            diags.extend(GenericParamOwner::Enum(e).diags_non_trailing_defaults(db));
            diags.extend(GenericParamOwner::Enum(e).diags_default_forward_refs(db));
        }
    }
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

    // Semantic traversal aggregations
    diags.extend(trait_.diags_assoc_defaults(db));
    diags.extend(trait_.diags_super_traits(db));
    diags.extend(GenericParamOwner::Trait(trait_).diags_params_defined_in_parent(db));
    diags.extend(GenericParamOwner::Trait(trait_).diags_kind_bounds(db));
    diags.extend(GenericParamOwner::Trait(trait_).diags_trait_bounds(db));
    diags.extend(GenericParamOwner::Trait(trait_).diags_non_trailing_defaults(db));
    diags.extend(GenericParamOwner::Trait(trait_).diags_default_forward_refs(db));

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
    diags.extend(GenericParamOwner::ImplTrait(impl_trait).diags_params_defined_in_parent(db));
    diags.extend(GenericParamOwner::ImplTrait(impl_trait).diags_kind_bounds(db));
    diags.extend(GenericParamOwner::ImplTrait(impl_trait).diags_trait_bounds(db));
    diags.extend(GenericParamOwner::ImplTrait(impl_trait).diags_non_trailing_defaults(db));
    diags.extend(GenericParamOwner::ImplTrait(impl_trait).diags_default_forward_refs(db));
    diags
}

#[salsa::tracked(return_ref)]
pub fn analyze_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: HirImpl<'db>,
) -> Vec<TyDiagCollection<'db>> {
    // Prefer semantic implementor type; early‑exit only for parse‑missing cases
    let ty = impl_.ty(db);
    if matches!(ty.data(db), TyData::Invalid(InvalidCause::ParseError)) {
        return Vec::new();
    }

    let analyzer = DefAnalyzer::for_impl(db, impl_, ty);
    let mut diags = analyzer.analyze();
    diags.extend(GenericParamOwner::Impl(impl_).diags_params_defined_in_parent(db));
    diags.extend(GenericParamOwner::Impl(impl_).diags_kind_bounds(db));
    diags.extend(GenericParamOwner::Impl(impl_).diags_trait_bounds(db));
    diags.extend(GenericParamOwner::Impl(impl_).diags_non_trailing_defaults(db));
    diags.extend(GenericParamOwner::Impl(impl_).diags_default_forward_refs(db));
    diags
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
    let mut diags = analyzer.analyze();
    diags.extend(GenericParamOwner::Func(func).diags_kind_bounds(db));
    diags.extend(GenericParamOwner::Func(func).diags_trait_bounds(db));
    diags.extend(GenericParamOwner::Func(func).diags_non_trailing_defaults(db));
    diags.extend(GenericParamOwner::Func(func).diags_default_forward_refs(db));
    diags
}

pub struct DefAnalyzer<'db> {
    db: &'db dyn HirAnalysisDb,
    def: DefKind<'db>,
    self_ty: Option<TyId<'db>>,
    diags: Vec<TyDiagCollection<'db>>,
    assumptions: PredicateListId<'db>,
}

impl<'db> DefAnalyzer<'db> {
    #[inline]
    fn lower_ty(&self, hir_ty: HirTyId<'db>) -> TyId<'db> {
        lower_hir_ty(self.db, hir_ty, self.scope(), self.assumptions)
    }


    fn for_adt(db: &'db dyn HirAnalysisDb, adt: AdtRef<'db>) -> Self {
        let def = lower_adt(db, adt);
        let assumptions = collect_adt_constraints(db, def).instantiate_identity();
        Self {
            db,
            def: def.into(),
            self_ty: None,
            diags: vec![],
            assumptions,
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
        }
    }

    // Removed: term type kind checks and self-type shape checks are now handled
    // semantically via Func::diags_param_types.

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
// Note: legacy check_param_defined_in_parent has been replaced by
// GenericParamOwner::diags_params_defined_in_parent. See analyze_* functions.

impl<'db> Visitor<'db> for DefAnalyzer<'db> {
    // We don't need to traverse the nested item, each item kinds are explicitly
    // handled(e.g, `visit_trait` or `visit_enum`).
    fn visit_item(&mut self, _ctxt: &mut VisitorCtxt<'db, LazyItemSpan>, _item: ItemKind<'db>) {}

    fn visit_type_alias(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTypeAliasSpan<'db>>,
        alias: TypeAlias<'db>,
    ) {
        // Delegate all alias diagnostics to semantic helper and avoid walking types redundantly.
        let diags = alias.diags(self.db);
        if !diags.is_empty() {
            self.diags.extend(diags);
            return;
        }
        walk_type_alias(self, ctxt, alias)
    }

    // Removed: generic visit_ty diagnostics are handled by semantic, context-aware helpers.

    fn visit_where_predicate(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyWherePredicateSpan<'db>>,
        pred: &crate::core::hir_def::WherePredicate<'db>,
    ) {
        let Some(owner) = WhereClauseOwner::from_item_opt(self.scope().item()) else {
            walk_where_predicate(self, ctxt, pred);
            return;
        };
        let clause = owner.clause(self.db);
        let clause_data = clause.id.data(self.db);
        let Some(idx) = clause_data
            .iter()
            .position(|candidate| ptr::eq(candidate, pred))
        else {
            walk_where_predicate(self, ctxt, pred);
            return;
        };

        let view = WherePredicateView { clause, idx };
        let diags = view.diags(self.db);
        if !diags.is_empty() {
            self.diags.extend(diags);
        }
        // Do not walk bounds; semantic diags already emitted them precisely.
    }

    fn visit_field_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFieldDefSpan<'db>>,
        field: &FieldDef<'db>,
    ) {
        // Derive FieldView from the field scope and delegate diagnostics to semantic helper
        let (parent, idx) = match ctxt.scope() {
            ScopeId::Field(parent, idx) => (parent, idx as usize),
            _ => {
                walk_field_def(self, ctxt, field);
                return;
            }
        };
        let view = crate::hir_def::semantic::FieldView { parent, idx };
        let diags = view.diags_wf(self.db);
        if !diags.is_empty() {
            self.diags.extend(diags);
            return;
        }

        walk_field_def(self, ctxt, field);
    }

    fn visit_variant_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyVariantDefSpan<'db>>,
        variant: &crate::core::hir_def::VariantDef<'db>,
    ) {
        // Derive VariantView from scope and delegate tuple-element diags to semantics
        if let ScopeId::Variant(v) = ctxt.scope() {
            let view = crate::hir_def::semantic::VariantView { owner: v.enum_, idx: v.idx as usize };
            let diags = view.diags_tuple_elems_wf(self.db);
            if !diags.is_empty() {
                self.diags.extend(diags);
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

        // Emit non-trailing default generic param diag from the visitor only for TypeAlias.
        // Other owners aggregate this via semantic traversal to avoid duplicates.
        if let GenericParamOwner::TypeAlias(_alias) = owner {
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

        // TODO(migrate): Replace legacy "defined in parent" emission with
        // GenericParamOwner::diags_params_defined_in_parent once we ensure this
        // doesn’t duplicate diagnostics for all owners. For now, keep visitor
        // path to preserve snapshots.
        // Note: "defined in parent" diagnostics are now emitted via
        // GenericParamOwner::<...>::diags_params_defined_in_parent at the
        // analyze_* entry points. Avoid emitting them here to prevent
        // duplicates.

        match param {
            GenericParam::Type(_tp) => {
                // All trait/kind diagnostics for type params are emitted via semantic
                // traversal at analyze_* entrypoints; nothing to do here.
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

    // Kind-bound checks for generic params are emitted via semantic traversal
    // (GenericParamOwner::diags_kind_bounds). No visitor emission here.

    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTraitRefSpan<'db>>,
        trait_ref: TraitRefId<'db>,
    ) {
        // All contexts that care about trait-ref diagnostics (where-predicates,
        // super-traits, generic param bounds, impl-trait) emit diagnostics via
        // semantic traversal. Keep the default walk to visit nested paths only.
        walk_trait_ref(self, ctxt, trait_ref);
    }

    fn visit_super_trait_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, crate::core::span::item::LazySuperTraitListSpan<'db>>,
        super_traits: &[TraitRefId<'db>],
    ) {
        let DefKind::Trait(_def) = self.def else {
            unreachable!()
        };
        // Emit super-trait diagnostics via semantic views to avoid duplicate visitor emissions
        let owner = match self.def { DefKind::Trait(def) => def.trait_(self.db), _ => unreachable!() };
        for (idx, _) in super_traits.iter().enumerate() {
            let view = crate::hir_def::semantic::SuperTraitRefView { owner, idx };
            if let Some(diag) = view.diags(self.db) {
                self.diags.push(diag);
            }
        }
    }

    fn visit_impl(&mut self, ctxt: &mut VisitorCtxt<'db, LazyImplSpan<'db>>, impl_: HirImpl<'db>) {
        // Delegate implementor preconditions to semantic helper; skip walking when
        // we already emitted an implementor error to avoid duplicates.
        let diags = impl_.diags_preconditions(self.db);
        if diags.is_empty() {
            walk_impl(self, ctxt, impl_);
        } else {
            self.diags.extend(diags);
        }
    }

    fn visit_impl_trait(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyImplTraitSpan<'db>>,
        impl_trait: ImplTrait<'db>,
    ) {
        // Delegate assoc-type WF + invalid diagnostics and other checks to semantic helpers
        self.diags.extend(impl_trait.diags_assoc_types_wf(self.db));
        self.diags.extend(impl_trait.diags_missing_assoc_types(self.db));
        self.diags.extend(impl_trait.diags_assoc_types_bounds(self.db));
        self.diags.extend(impl_trait.diags_trait_ref_and_wf(self.db));

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

        // Parameter diagnostics via semantic traversal (duplicate names/labels)
        self.diags.extend(hir_func.diags_parameters(self.db));
        // Parameter type diagnostics (star kind/const type/self type shape)
        self.diags.extend(hir_func.diags_param_types(self.db));

        walk_func(self, ctxt, hir_func);

        // Return type diagnostics via semantic traversal (kind/const checks)
        self.diags.extend(hir_func.diags_return(self.db));

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
        // Semantic diags for param types are aggregated at visit_func; we still walk
        // to allow general type error/WF diagnostics from visit_ty.
        walk_func_param(self, ctxt, param);
    }
}


#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AdtCycleMember<'db> {
    pub adt: AdtDef<'db>,
    pub field_idx: u16,
    pub ty_idx: u16,
}

// Adt recursion detection moved to semantic layer (AdtDef::recursive_cycle)

// analyze_trait_ref moved into semantic view (TraitRefOnSubjectView::diags)

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

    // Removed: trait_self_param and super_trait_cycle, now handled via semantic views.

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
    // Early return if the implementor type is syntactically missing
    if matches!(impl_trait.ty(db).data(db), TyData::Invalid(InvalidCause::ParseError)) {
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
    let _trait_constraints =
        collect_constraints(db, trait_def.trait_(db).into()).instantiate(db, trait_inst.args(db));
    let _assumptions = implementor.instantiate_identity().constraints(db);

    // 6–7. Trait-ref WF and super-trait constraints via semantic traversal
    diags.extend(impl_trait.diags_trait_ref_and_wf(db));

    // 8. Check associated types via semantic traversal
    diags.extend(impl_trait.diags_missing_assoc_types(db));
    diags.extend(impl_trait.diags_assoc_types_bounds(db));

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
