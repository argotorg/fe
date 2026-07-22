//! This module implements the trait and impl trait lowering process.

use crate::{
    core::hir_def::{
        AssocTypeGenericArg, ConstGenericArgValue, HirIngot, IdentId, ImplTrait, ItemKind, Partial,
        PathId, Trait, TraitRefId, TypeId as HirTyId, TypeKind as HirTyKind, TypeMode,
        params::GenericArg, scope_graph::ScopeId,
    },
    hir_def::Func,
};
use common::{indexmap::IndexMap, ingot::Ingot};
use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    binder::Binder,
    const_ty::{ConstBodyLowering, ConstTyId, HoleAnchor, HoleMinter},
    fold::{TyFoldable, TyFolder},
    trait_def::{ImplementorId, ImplementorOrigin, TraitInstId},
    trait_resolution::PredicateListId,
    ty_def::{InvalidCause, PrimTy, TyBase, TyId},
    ty_lower::{ConstDefaultCompletion, lower_hir_ty_with_minter, lower_opt_hir_ty_with_minter},
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        NameDomain, NameResKind, PathRes, PathResError, resolve_ident_to_bucket,
        resolve_name_res_with_minter, resolve_path_with_minter, resolve_type_path_definition,
    },
    ty::ty_def::{Kind, TyData},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub(crate) enum ImplSelfKey<'db> {
    Prim(PrimTy),
    Item(ScopeId<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Update)]
pub(crate) struct TraitImplTable<'db> {
    pub(crate) by_trait: FxHashMap<Trait<'db>, Vec<ImplTrait<'db>>>,
    pub(crate) by_self: FxHashMap<ImplSelfKey<'db>, Vec<ImplTrait<'db>>>,
    pub(crate) unindexed_self: Vec<ImplTrait<'db>>,
}

fn raw_impl_trait_def<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Option<Trait<'db>> {
    let path = impl_trait.hir_trait_ref(db).to_opt()?.path(db).to_opt()?;
    match resolve_type_path_definition(db, path, impl_trait.scope())? {
        NameResKind::Scope(ScopeId::Item(ItemKind::Trait(trait_))) => Some(trait_),
        NameResKind::Scope(_) | NameResKind::Prim(_) => None,
    }
}

fn raw_impl_self_key<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Option<ImplSelfKey<'db>> {
    fn lower<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: HirTyId<'db>,
        scope: ScopeId<'db>,
    ) -> Option<ImplSelfKey<'db>> {
        match ty.data(db) {
            HirTyKind::Ptr(_) => Some(ImplSelfKey::Prim(PrimTy::Ptr)),
            HirTyKind::Mode(TypeMode::Mut, _) => Some(ImplSelfKey::Prim(PrimTy::BorrowMut)),
            HirTyKind::Mode(TypeMode::Ref, _) => Some(ImplSelfKey::Prim(PrimTy::BorrowRef)),
            HirTyKind::Mode(TypeMode::Own, inner) => lower(db, inner.to_opt()?, scope),
            HirTyKind::Path(path) => match resolve_type_path_definition(db, path.to_opt()?, scope)?
            {
                NameResKind::Prim(prim) => match TyBase::from(prim) {
                    TyBase::Prim(prim) => Some(ImplSelfKey::Prim(prim)),
                    TyBase::Adt(_) | TyBase::Contract(_) | TyBase::Func(_) => unreachable!(),
                },
                NameResKind::Scope(
                    scope @ ScopeId::Item(
                        ItemKind::Struct(_) | ItemKind::Enum(_) | ItemKind::Contract(_),
                    ),
                ) => Some(ImplSelfKey::Item(scope)),
                NameResKind::Scope(_) => None,
            },
            HirTyKind::Tuple(tuple) => Some(ImplSelfKey::Prim(PrimTy::Tuple(tuple.data(db).len()))),
            HirTyKind::Array(..) => Some(ImplSelfKey::Prim(PrimTy::Array)),
            HirTyKind::Never => None,
        }
    }

    lower(db, impl_trait.type_ref(db).to_opt()?, impl_trait.scope())
}

/// Collect all local trait implementors in the ingot.
///
/// This query is intentionally a syntax-only indexing pass:
/// - it only records impls defined in `ingot`
/// - it does not pull in external ingot impls
/// - it does not lower self types, trait arguments, associated types, or
///   perform overlap/conflict checking
///
/// Generic const expressions in any of those positions can type-check
/// operators through the trait environment. Keeping collection entirely free
/// of semantic type lowering is therefore the boundary that prevents query
/// cycles. Visible candidates are semantically lowered only after this index
/// has been assembled by [`TraitEnv`](super::trait_def::TraitEnv).
#[salsa::tracked(return_ref)]
pub(crate) fn collect_trait_impls<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
) -> TraitImplTable<'db> {
    let mut impl_table = TraitImplTable::default();
    for &impl_ in ingot.all_impl_traits(db).iter() {
        if let Some(trait_def) = raw_impl_trait_def(db, impl_) {
            impl_table
                .by_trait
                .entry(trait_def)
                .or_default()
                .push(impl_);
        }
        if let Some(self_key) = raw_impl_self_key(db, impl_) {
            impl_table.by_self.entry(self_key).or_default().push(impl_);
        } else {
            impl_table.unindexed_self.push(impl_);
        }
    }

    impl_table
}

/// Returns the corresponding implementors for the given [`ImplTrait`].
/// If the implementor type or the trait reference is ill-formed, returns
/// `None`.
pub(crate) fn lower_impl_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Option<Binder<ImplementorId<'db>>> {
    let trait_inst = impl_trait.trait_inst_result(db).ok()?;
    let params = impl_trait.impl_params(db);

    // This is the checked item-signature path used after candidate lookup or by
    // diagnostics. Raw candidate enumeration uses the deferred path below.
    let types = impl_trait.assoc_type_bindings_for_trait_inst(db, trait_inst);

    Some(Binder::bind(ImplementorId::new(
        db,
        trait_inst,
        params,
        types,
        ImplementorOrigin::Hir(impl_trait),
    )))
}

pub(crate) fn complete_selected_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    selected: ImplementorId<'db>,
) -> Option<ImplementorId<'db>> {
    match selected.origin(db) {
        ImplementorOrigin::Hir(impl_trait) => {
            Some(lower_impl_trait(db, impl_trait)?.instantiate_identity())
        }
        ImplementorOrigin::VirtualContract(_) | ImplementorOrigin::Assumption => Some(selected),
    }
}

/// Re-lowers one associated-type binding after candidate enumeration has
/// finished. This is deliberately narrower than completing the whole impl:
/// normalizing one projection must not validate unrelated impl headers or
/// constraints while a trait-solver query is active.
pub(crate) fn complete_impl_assoc_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: ImplementorId<'db>,
    name: IdentId<'db>,
) -> Option<ImplementorId<'db>> {
    let ImplementorOrigin::Hir(impl_trait) = implementor.origin(db) else {
        return implementor.assoc_ty(db, name).map(|_| implementor);
    };
    let ty = lower_checked_impl_assoc_ty(db, impl_trait, name)?;
    Some(with_impl_assoc_ty(db, implementor, name, ty))
}

#[salsa::tracked(
    cycle_fn=lower_impl_assoc_ty_cycle_recover,
    cycle_initial=lower_impl_assoc_ty_cycle_initial
)]
pub(crate) fn lower_checked_impl_assoc_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
    name: IdentId<'db>,
) -> Option<TyId<'db>> {
    if let Some(assoc) = impl_trait
        .assoc_types(db)
        .find(|assoc| assoc.name(db) == Some(name))
    {
        return assoc.ty(db);
    }

    let trait_inst = impl_trait.trait_inst_result(db).ok()?;
    let default = trait_inst
        .def(db)
        .assoc_types(db)
        .find(|assoc| assoc.name(db) == Some(name))?
        .default_ty(db)?;
    Some(Binder::bind(default).instantiate(db, trait_inst.args(db)))
}

pub(crate) fn complete_candidate_impl_assoc_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: ImplementorId<'db>,
    name: IdentId<'db>,
) -> Option<ImplementorId<'db>> {
    let ImplementorOrigin::Hir(impl_trait) = implementor.origin(db) else {
        return implementor.assoc_ty(db, name).map(|_| implementor);
    };
    let ty = lower_candidate_impl_assoc_ty(db, impl_trait, name)?;
    Some(with_impl_assoc_ty(db, implementor, name, ty))
}

fn with_impl_assoc_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: ImplementorId<'db>,
    name: IdentId<'db>,
    ty: TyId<'db>,
) -> ImplementorId<'db> {
    let mut types = implementor.types(db).clone();
    types.insert(name, ty);
    ImplementorId::new(
        db,
        implementor.trait_(db),
        implementor.params(db).to_vec(),
        types,
        implementor.origin(db),
    )
}

fn lower_impl_assoc_ty_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _impl_trait: ImplTrait<'db>,
    _name: IdentId<'db>,
) -> Option<TyId<'db>> {
    Some(TyId::invalid(db, InvalidCause::TypeLoweringCycle))
}

fn lower_impl_assoc_ty_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Option<TyId<'db>>,
    _count: u32,
    _impl_trait: ImplTrait<'db>,
    _name: IdentId<'db>,
) -> salsa::CycleRecoveryAction<Option<TyId<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

/// Lowers one associated-type definition for candidate use without checking
/// anonymous const bodies. Keeping this query per binding lets sibling
/// references such as `type B = Self::A` resolve locally without completing
/// every associated type or recursively re-entering the trait environment.
#[salsa::tracked(
    cycle_fn=lower_impl_assoc_ty_cycle_recover,
    cycle_initial=lower_impl_assoc_ty_cycle_initial
)]
pub(crate) fn lower_candidate_impl_assoc_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
    name: IdentId<'db>,
) -> Option<TyId<'db>> {
    if let Some(assoc) = impl_trait
        .assoc_types(db)
        .find(|assoc| assoc.name(db) == Some(name))
    {
        return assoc.candidate_ty(db);
    }

    let trait_inst = impl_trait.candidate_trait_inst_result(db).ok()?;
    let default = trait_inst
        .def(db)
        .assoc_types(db)
        .find(|assoc| assoc.name(db) == Some(name))?
        .candidate_default_ty(db)?;
    Some(Binder::bind(default).instantiate(db, trait_inst.args(db)))
}

/// Complete a semantically lowered candidate header with its associated type
/// definitions after the syntax-only impl index has been assembled.
pub(crate) fn complete_impl_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: Binder<ImplementorId<'db>>,
) -> Binder<ImplementorId<'db>> {
    match implementor.skip_binder().origin(db) {
        ImplementorOrigin::Hir(impl_trait) => lower_impl_trait_candidate(db, impl_trait)
            .expect("a collected impl header must remain lowerable"),
        ImplementorOrigin::VirtualContract(_) | ImplementorOrigin::Assumption => implementor,
    }
}

fn lower_impl_trait_candidate<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Option<Binder<ImplementorId<'db>>> {
    let implementor = lower_impl_trait_header(db, impl_trait)?.instantiate_identity();
    let types = impl_trait.candidate_assoc_type_bindings_for_trait_inst(db, implementor.trait_(db));
    Some(Binder::bind(ImplementorId::new(
        db,
        implementor.trait_(db),
        implementor.params(db).to_vec(),
        types,
        implementor.origin(db),
    )))
}

/// Lower only the part of an impl needed to index and select it.
///
/// This phase must stay independent of trait solving. In particular, associated
/// type definitions are not lowered here: a definition such as
/// `type Target = Slot<{ ROOT + 1 }>` type-checks `+` through `core::ops::Add`,
/// which needs the completed trait environment.
pub(crate) fn lower_impl_trait_header<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Option<Binder<ImplementorId<'db>>> {
    // Delegate trait-ref lowering and ingot checks to the semantic helper on
    // `ImplTrait`. If lowering fails or the ingot rule is violated, this
    // returns `None`.
    let trait_inst = impl_trait.candidate_trait_inst_result(db).ok()?;

    // Semantic generic parameters for this impl-trait block.
    let params = impl_trait.impl_params(db);

    let implementor = ImplementorId::new(
        db,
        trait_inst,
        params,
        IndexMap::new(),
        ImplementorOrigin::Hir(impl_trait),
    );

    Some(Binder::bind(implementor))
}

/// Lower a trait reference to a trait instance.
///
/// When `owner_self` is provided, it is used for substituting `Self` references in generic
/// arguments and associated type bindings, while `self_ty` is used as the implementor (args[0]).
/// This is needed for associated type bounds like `type Assoc: Encode<Self>` where `Self`
/// refers to the owner trait's Self, not the associated type.
#[salsa::tracked(
    cycle_fn=lower_trait_ref_cycle_recover,
    cycle_initial=lower_trait_ref_cycle_initial
)]
pub(crate) fn lower_trait_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    self_ty: TyId<'db>,
    trait_ref: TraitRefId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    owner_self: Option<TyId<'db>>,
) -> Result<TraitInstId<'db>, TraitRefLowerError<'db>> {
    lower_trait_ref_inner(
        db,
        self_ty,
        trait_ref,
        scope,
        assumptions,
        owner_self,
        ConstBodyLowering::Eager,
    )
}

pub(crate) fn lower_trait_ref_deferred<'db>(
    db: &'db dyn HirAnalysisDb,
    self_ty: TyId<'db>,
    trait_ref: TraitRefId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    owner_self: Option<TyId<'db>>,
) -> Result<TraitInstId<'db>, TraitRefLowerError<'db>> {
    lower_trait_ref_inner(
        db,
        self_ty,
        trait_ref,
        scope,
        assumptions,
        owner_self,
        ConstBodyLowering::Deferred,
    )
}

fn lower_trait_ref_inner<'db>(
    db: &'db (dyn HirAnalysisDb + 'static),
    self_ty: TyId<'db>,
    trait_ref: TraitRefId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    owner_self: Option<TyId<'db>>,
    const_bodies: ConstBodyLowering,
) -> Result<TraitInstId<'db>, TraitRefLowerError<'db>> {
    let Partial::Present(path) = trait_ref.path(db) else {
        return Err(TraitRefLowerError::Ignored);
    };

    let self_subst = owner_self.unwrap_or(self_ty);
    let minter = match const_bodies {
        ConstBodyLowering::Eager => HoleMinter::new(HoleAnchor::TemplatePath {
            path,
            scope,
            assumptions,
        }),
        ConstBodyLowering::Deferred => HoleMinter::deferred(HoleAnchor::TemplatePath {
            path,
            scope,
            assumptions,
        }),
    };

    let resolved = match resolve_path_with_minter(db, path, scope, assumptions, false, &minter) {
        Ok(res @ PathRes::Ty(_)) => {
            match resolve_shadowed_trait_ref(db, &res, path, scope, assumptions, &minter) {
                Some(trait_res) => Ok(trait_res),
                None => Ok(res),
            }
        }
        other => other,
    };

    match resolved {
        Ok(PathRes::Trait(t)) => {
            let mut args = t.args(db).clone();

            // Substitute all occurrences of `Self` with `self_subst`
            // TODO: this shouldn't be necessary; Self should resolve to self_ty in a later stage,
            //  but something seems to be broken.
            struct SelfSubst<'db> {
                db: &'db dyn HirAnalysisDb,
                self_subst: TyId<'db>,
            }
            impl<'db> TyFolder<'db> for SelfSubst<'db> {
                fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
                    match ty.data(self.db) {
                        TyData::TyParam(p) if p.is_trait_self() => self.self_subst,
                        _ => ty.super_fold_with(db, self),
                    }
                }
            }

            let mut folder = SelfSubst { db, self_subst };
            args[0] = self_ty;
            args.iter_mut()
                .skip(1)
                .for_each(|a| *a = a.fold_with(db, &mut folder));

            let mut assoc_bindings = t.assoc_type_bindings(db).clone();
            assoc_bindings
                .iter_mut()
                .for_each(|(_, ty)| *ty = (*ty).fold_with(db, &mut folder));

            Ok(TraitInstId::new(db, t.key(db), args, assoc_bindings))
        }
        Ok(res) => Err(TraitRefLowerError::InvalidDomain(res)),
        Err(e) => Err(TraitRefLowerError::PathResError(e)),
    }
}

/// A trait reference can never resolve to an associated type, but an
/// associated type of an enclosing trait lexically shadows a same-named trait
/// (`trait Abi { type Encoder: Encoder }`). When a bare trait-ref ident
/// resolves to such an associated type, retry the ident lookup from outside
/// the shadowing trait's scope, while still lowering the path's generic args
/// in the original scope so they can reference the trait's generic params.
fn resolve_shadowed_trait_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    res: &PathRes<'db>,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    minter: &HoleMinter<'db>,
) -> Option<PathRes<'db>> {
    if path.parent(db).is_some() {
        return None;
    }
    let ident = path.ident(db).to_opt()?;
    let PathRes::Ty(ty) = res else {
        return None;
    };
    let TyData::AssocTy(assoc) = ty.data(db) else {
        return None;
    };
    if assoc.name != ident {
        return None;
    }

    // The shadow can only come from a trait that encloses the use site.
    let shadowing_trait = assoc.trait_.def(db);
    let mut item = Some(scope.item());
    let enclosing = loop {
        match item {
            Some(ItemKind::Trait(trait_)) if trait_ == shadowing_trait => break trait_,
            Some(current) => item = current.scope().parent_item(db),
            None => return None,
        }
    };

    let retry_scope = enclosing.scope().parent_item(db)?.scope();
    let bucket = resolve_ident_to_bucket(db, path, retry_scope);
    let nameres = bucket.pick(NameDomain::TYPE).as_ref().ok()?;
    if !matches!(
        nameres.kind,
        NameResKind::Scope(ScopeId::Item(ItemKind::Trait(_)))
    ) {
        return None;
    }

    resolve_name_res_with_minter(db, nameres, None, path, scope, assumptions, minter).ok()
}

fn lower_trait_ref_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _self_ty: TyId<'db>,
    _trait_ref: TraitRefId<'db>,
    _scope: ScopeId<'db>,
    _assumptions: PredicateListId<'db>,
    _owner_self: Option<TyId<'db>>,
) -> Result<TraitInstId<'db>, TraitRefLowerError<'db>> {
    Err(TraitRefLowerError::Cycle)
}

#[allow(clippy::too_many_arguments)]
fn lower_trait_ref_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Result<TraitInstId<'db>, TraitRefLowerError<'db>>,
    _count: u32,
    _self_ty: TyId<'db>,
    _trait_ref: TraitRefId<'db>,
    _scope: ScopeId<'db>,
    _assumptions: PredicateListId<'db>,
    _owner_self: Option<TyId<'db>>,
) -> salsa::CycleRecoveryAction<Result<TraitInstId<'db>, TraitRefLowerError<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

pub(crate) enum TraitArgError<'db> {
    ArgNumMismatch {
        expected: usize,
        given: usize,
    },
    ArgKindMisMatch {
        // TODO: add index, improve diag display
        expected: Kind,
        given: TyId<'db>,
    },
    ArgTypeMismatch {
        expected: Option<TyId<'db>>,
        given: Option<TyId<'db>>,
    },
    ConstHoleNotAllowed {
        arg_idx: usize,
    },
    Ignored,
}

#[cfg(test)]
pub(crate) fn lower_trait_ref_impl<'db>(
    db: &'db (dyn HirAnalysisDb + 'static),
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    t: Trait<'db>,
) -> Result<TraitInstId<'db>, TraitArgError<'db>> {
    let minter = HoleMinter::new(HoleAnchor::TemplatePath {
        path,
        scope,
        assumptions,
    });
    lower_trait_ref_impl_with_minter(db, path, scope, assumptions, t, &minter)
}

pub(crate) fn lower_trait_ref_impl_with_minter<'db>(
    db: &'db (dyn HirAnalysisDb + 'static),
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    t: Trait<'db>,
    minter: &HoleMinter<'db>,
) -> Result<TraitInstId<'db>, TraitArgError<'db>> {
    let trait_params: &[TyId<'db>] = t.params(db);
    let args = path.generic_args(db).data(db);
    // Lower provided explicit args (excluding Self)
    let mut provided_explicit = Vec::new();
    let mut assoc_bindings = IndexMap::new();
    for (arg_idx, arg) in args.iter().enumerate() {
        match arg {
            GenericArg::Type(ty_arg) => {
                let ty = lower_opt_hir_ty_with_minter(db, ty_arg.ty, scope, assumptions, minter);
                provided_explicit.push(ty);
            }
            GenericArg::Const(const_arg) => match const_arg.value {
                ConstGenericArgValue::Expr(body) => {
                    let const_ty = match minter.const_bodies() {
                        ConstBodyLowering::Eager => ConstTyId::from_opt_body(db, body),
                        ConstBodyLowering::Deferred => {
                            ConstTyId::from_opt_body_deferred(db, body, None, Vec::new())
                        }
                    };
                    provided_explicit.push(TyId::const_ty(db, const_ty));
                }
                ConstGenericArgValue::Hole => {
                    return Err(TraitArgError::ConstHoleNotAllowed { arg_idx });
                }
            },
            GenericArg::AssocType(AssocTypeGenericArg { name, ty }) => {
                if let (Some(name), Some(ty)) = (name.to_opt(), ty.to_opt()) {
                    let ty = lower_hir_ty_with_minter(db, ty, scope, assumptions, minter);
                    assoc_bindings.insert(name, ty);
                }
            }
        }
    }

    // Fill trailing defaults using the trait's param set. Bind Self (idx 0).
    let non_self_completed = t.param_set(db).complete_explicit_args(
        db,
        Some(t.self_param(db)),
        &provided_explicit,
        assumptions,
        match minter.const_bodies() {
            ConstBodyLowering::Eager => ConstDefaultCompletion::evaluate(Some(path)),
            ConstBodyLowering::Deferred => ConstDefaultCompletion::metadata(Some(path)),
        },
        Some(minter),
    );

    if non_self_completed.len() != trait_params.len() - 1 {
        return Err(TraitArgError::ArgNumMismatch {
            expected: trait_params.len() - 1,
            given: non_self_completed.len(),
        });
    }

    let mut final_args: Vec<TyId<'db>> = Vec::with_capacity(trait_params.len());
    final_args.push(t.self_param(db));
    final_args.extend(non_self_completed);

    for (expected_ty, actual_ty) in trait_params.iter().zip(final_args.iter_mut()).skip(1) {
        if !expected_ty.kind(db).does_match(actual_ty.kind(db)) {
            return Err(TraitArgError::ArgKindMisMatch {
                expected: expected_ty.kind(db).clone(),
                given: *actual_ty,
            });
        }

        let expected_const_ty = match expected_ty.data(db) {
            TyData::ConstTy(expected_ty) => expected_ty.ty(db).into(),
            _ => None,
        };

        let checked = match minter.const_bodies() {
            ConstBodyLowering::Eager => actual_ty.evaluate_const_ty(db, expected_const_ty),
            ConstBodyLowering::Deferred => {
                actual_ty.check_const_ty_without_eval(db, expected_const_ty)
            }
        };
        match checked {
            Ok(evaluated_ty) => *actual_ty = evaluated_ty,
            Err(InvalidCause::ConstTyMismatch { expected, given }) => {
                return Err(TraitArgError::ArgTypeMismatch {
                    expected: Some(expected),
                    given: Some(given),
                });
            }
            Err(InvalidCause::ConstTyExpected { expected }) => {
                return Err(TraitArgError::ArgTypeMismatch {
                    expected: Some(expected),
                    given: None,
                });
            }
            Err(InvalidCause::NormalTypeExpected { given }) => {
                return Err(TraitArgError::ArgTypeMismatch {
                    expected: None,
                    given: Some(given),
                });
            }
            _ => return Err(TraitArgError::Ignored),
        }
    }

    Ok(TraitInstId::new(db, t, final_args, assoc_bindings))
}

#[cfg(test)]
mod layout_hole_tests {
    use camino::Utf8PathBuf;

    use super::lower_trait_ref_impl;
    use crate::analysis::ty::{
        const_ty::{ConstTyData, HoleId},
        trait_resolution::PredicateListId,
        ty_def::TyData,
    };
    use crate::hir_def::{ItemKind, PathId};
    use crate::test_db::HirAnalysisTestDb;

    #[test]
    fn omitted_trait_hole_defaults_keep_distinct_path_arg_identity() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("omitted_trait_hole_defaults_keep_distinct_path_arg_identity.fe"),
            r#"
trait Cap<const LEFT: u256 = _, const RIGHT: u256 = _> {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);

        let trait_ = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Trait(trait_)
                    if trait_
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "Cap") =>
                {
                    Some(trait_)
                }
                _ => None,
            })
            .expect("missing `Cap` trait");
        let name = trait_.name(&db).to_opt().expect("trait must have a name");
        let path = PathId::from_ident(&db, name);
        let inst = match lower_trait_ref_impl(
            &db,
            path,
            trait_.scope(),
            PredicateListId::empty_list(&db),
            trait_,
        ) {
            Ok(inst) => inst,
            Err(_) => panic!("failed to lower trait ref"),
        };
        let args = inst.args(&db);

        assert_eq!(args.len(), 3);
        let left = args[1];
        let right = args[2];
        assert_ne!(left, right);

        let TyData::ConstTy(left) = left.data(&db) else {
            panic!("expected left arg to be a const hole");
        };
        let TyData::ConstTy(right) = right.data(&db) else {
            panic!("expected right arg to be a const hole");
        };

        assert!(matches!(
            left.data(&db),
            ConstTyData::Hole(_, HoleId::Structural(_),)
        ));
        assert!(matches!(
            right.data(&db),
            ConstTyData::Hole(_, HoleId::Structural(_),)
        ));
    }
}

#[salsa::tracked(return_ref)]
pub(crate) fn collect_implementor_methods<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: ImplementorId<'db>,
) -> IndexMap<IdentId<'db>, Func<'db>> {
    let mut methods = IndexMap::default();
    let impl_trait = match implementor.origin(db) {
        super::trait_def::ImplementorOrigin::Hir(impl_trait) => impl_trait,
        super::trait_def::ImplementorOrigin::VirtualContract(_)
        | super::trait_def::ImplementorOrigin::Assumption => return methods,
    };
    let scope = impl_trait.scope();
    let graph = scope.scope_graph(db);
    for method in graph.child_items(scope).filter_map(|item| match item {
        ItemKind::Func(func) => Some(func),
        _ => None,
    }) {
        let Some(name) = method.name(db).to_opt() else {
            continue;
        };
        methods.insert(name, method);
    }

    methods
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub(crate) enum TraitRefLowerError<'db> {
    PathResError(PathResError<'db>),
    InvalidDomain(PathRes<'db>),
    Cycle,
    UnsafeLocalBoundBlanketImpl,
    /// Error is expected to be reported elsewhere.
    Ignored,
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;

    use super::*;
    use crate::{
        analysis::ty::ty_def::InvalidCause, core::hir_def::Partial, test_db::HirAnalysisTestDb,
    };

    #[test]
    fn lower_trait_ref_cycle_initial_returns_cycle() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("cycle_initial.fe"), "");
        let (top_mod, _) = db.top_mod(file);
        let scope = ScopeId::from_item(top_mod.into());

        let result = lower_trait_ref_cycle_initial(
            &db,
            TyId::invalid(&db, InvalidCause::Other),
            TraitRefId::new(&db, Partial::Absent),
            scope,
            PredicateListId::empty_list(&db),
            None,
        );

        assert_eq!(result, Err(TraitRefLowerError::Cycle));
    }
}
