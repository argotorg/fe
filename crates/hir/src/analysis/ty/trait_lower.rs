//! This module implements the trait and impl trait lowering process.

use crate::hir_def::ModeMarker;
use crate::{
    core::hir_def::{
        AssocTypeGenericArg, HirIngot, IdentId, ImplTrait, ItemKind, Partial, PathId, Trait,
        TraitRefId, TypeId as HirTyId, TypeKind as HirTyKind, TypeMode,
        params::{FuncParamMode, GenericArg},
        scope_graph::ScopeId,
    },
    hir_def::Func,
};
use common::{indexmap::IndexMap, ingot::Ingot};
use rustc_hash::FxHashMap;
use salsa::SalsaValue;

use super::{
    binder::Binder,
    const_ty::{ConstBodyLowering, LoweringContext},
    fold::{TyFoldable, TyFolder},
    generic_defaults::DefaultApplication,
    provider::AddressSpace,
    shape::{Shape, lower_shape_value},
    trait_def::{AssocBinding, ImplementorId, ImplementorOrigin, TraitInstId},
    trait_resolution::PredicateListId,
    ty_def::{BorrowKind, InvalidCause, PrimTy, ShapeMode, TyBase, TyId},
    ty_error::first_invalid_ty_cause,
    ty_lower::{lower_generic_arg_list, lower_hir_ty_with_minter},
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        NameResKind, PathRes, PathResError, PathResErrorKind,
        resolve_trait_path_with_observer_and_minter, resolve_type_path_definition,
    },
    ty::ty_def::{Kind, TyData},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub(crate) enum ImplSelfKey<'db> {
    Prim(PrimTy),
    Item(ScopeId<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Default, SalsaValue)]
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
            HirTyKind::Ptr(_) => Some(ImplSelfKey::Prim(PrimTy::Ptr(AddressSpace::Memory))),
            HirTyKind::Mode(TypeMode::Mut, _) => Some(ImplSelfKey::Prim(PrimTy::BorrowMut)),
            HirTyKind::Mode(TypeMode::Ref, _) => Some(ImplSelfKey::Prim(PrimTy::BorrowRef)),
            HirTyKind::Mode(TypeMode::MaybeMut, _) => None,
            HirTyKind::Mode(TypeMode::Own, inner) => lower(db, inner.to_opt()?, scope),
            HirTyKind::Path(path) => match resolve_type_path_definition(db, path.to_opt()?, scope)?
            {
                NameResKind::Prim(prim) => match TyBase::from(prim) {
                    TyBase::Prim(prim) => Some(ImplSelfKey::Prim(prim)),
                    TyBase::Adt(_)
                    | TyBase::Contract(_)
                    | TyBase::Func(_)
                    | TyBase::Closure(_)
                    | TyBase::Shape(_) => unreachable!(),
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
            HirTyKind::Primitive(_) | HirTyKind::Never => None,
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
#[salsa::tracked]
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
) -> Option<ImplementorId<'db>> {
    let trait_inst = impl_trait.trait_inst_result(db).ok()?;
    let params = impl_trait.impl_params(db);

    // This is the checked item-signature path used after candidate lookup or by
    // diagnostics. Raw candidate enumeration uses the deferred path below.
    let types = impl_trait.assoc_type_bindings_for_trait_inst(db, trait_inst);

    Some(ImplementorId::new(
        db,
        trait_inst,
        params,
        types,
        ImplementorOrigin::Hir(impl_trait),
    ))
}

pub(crate) fn complete_selected_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    selected: ImplementorId<'db>,
) -> Option<ImplementorId<'db>> {
    match selected.origin(db) {
        ImplementorOrigin::Hir(impl_trait) => lower_impl_trait(db, impl_trait),
        ImplementorOrigin::VirtualContract(_)
        | ImplementorOrigin::Assumption
        | ImplementorOrigin::Callable
        | ImplementorOrigin::Structural => Some(selected),
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

#[salsa::tracked(returns(copy), cycle_initial=lower_impl_assoc_ty_cycle_initial)]
pub(crate) fn lower_checked_impl_assoc_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
    name: IdentId<'db>,
) -> Option<TyId<'db>> {
    if let Some(assoc) = impl_trait
        .assoc_types(db)
        .find(|assoc| assoc.name(db) == Some(name))
    {
        return assoc.ty(db).map(|ty| without_cycle(db, ty));
    }

    let trait_inst = impl_trait.trait_inst_result(db).ok()?;
    let default = trait_inst
        .def(db)
        .assoc_types(db)
        .find(|assoc| assoc.name(db) == Some(name))?
        .default_ty(db)?;
    let ty = Binder::bind(trait_inst.def(db).into(), default).instantiate(db, trait_inst.args(db));
    Some(without_cycle(db, ty))
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
    _: salsa::Id,
    _impl_trait: ImplTrait<'db>,
    _name: IdentId<'db>,
) -> Option<TyId<'db>> {
    Some(TyId::invalid(db, InvalidCause::TypeLoweringCycle))
}

/// `ty`, an associated type's definition, unless it holds its own cycle's
/// placeholder: one naming itself inside another type, `type A = (u8,
/// Self::A)`, would grow each round, so it is the cycle, which then
/// converges.
fn without_cycle<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
    match first_invalid_ty_cause(db, ty) {
        Some(InvalidCause::TypeLoweringCycle) => TyId::invalid(db, InvalidCause::TypeLoweringCycle),
        _ => ty,
    }
}

/// Lowers one associated-type definition for candidate use without checking
/// anonymous const bodies. Keeping this query per binding lets sibling
/// references such as `type B = Self::A` resolve locally without completing
/// every associated type or recursively re-entering the trait environment.
#[salsa::tracked(returns(copy), cycle_initial=lower_impl_assoc_ty_cycle_initial)]
pub(crate) fn lower_candidate_impl_assoc_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
    name: IdentId<'db>,
) -> Option<TyId<'db>> {
    if let Some(assoc) = impl_trait
        .assoc_types(db)
        .find(|assoc| assoc.name(db) == Some(name))
    {
        return assoc.candidate_ty(db).map(|ty| without_cycle(db, ty));
    }

    let trait_inst = impl_trait.candidate_trait_inst_result(db).ok()?;
    let default = trait_inst
        .def(db)
        .assoc_types(db)
        .find(|assoc| assoc.name(db) == Some(name))?
        .candidate_default_ty(db)?;
    let ty = Binder::bind(trait_inst.def(db).into(), default).instantiate(db, trait_inst.args(db));
    Some(without_cycle(db, ty))
}

/// Complete a semantically lowered candidate header with its associated type
/// definitions after the syntax-only impl index has been assembled.
pub(crate) fn complete_impl_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: ImplementorId<'db>,
) -> ImplementorId<'db> {
    match implementor.origin(db) {
        ImplementorOrigin::Hir(impl_trait) => lower_impl_trait_candidate(db, impl_trait)
            .expect("a collected impl header must remain lowerable"),
        ImplementorOrigin::VirtualContract(_)
        | ImplementorOrigin::Assumption
        | ImplementorOrigin::Callable
        | ImplementorOrigin::Structural => implementor,
    }
}

fn lower_impl_trait_candidate<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Option<ImplementorId<'db>> {
    let implementor = lower_impl_trait_header(db, impl_trait)?;
    let types = impl_trait.candidate_assoc_type_bindings_for_trait_inst(db, implementor.trait_(db));
    Some(ImplementorId::new(
        db,
        implementor.trait_(db),
        implementor.params(db).to_vec(),
        types,
        implementor.origin(db),
    ))
}

/// Lower only the part of an impl needed to index and select it.
///
/// This phase must stay independent of trait solving. In particular, associated
/// type definitions are not lowered here: a definition such as
/// `type Target = Slot<{ ROOT + 1 }>` type-checks `+` through `core::ops::Add`,
/// which needs the completed trait environment.
///
/// Every trait-environment lookup visits the headers of all candidate impls,
/// so the lowered header is cached per impl. The sealed-marker admissibility
/// check lowers marker impls eagerly; if that re-enters this header, the impl
/// is not a candidate until the cycle converges.
#[salsa::tracked(returns(copy), cycle_initial=lower_impl_trait_header_cycle_initial)]
pub(crate) fn lower_impl_trait_header<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Option<ImplementorId<'db>> {
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

    Some(implementor)
}

fn lower_impl_trait_header_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _impl_trait: ImplTrait<'db>,
) -> Option<ImplementorId<'db>> {
    None
}

/// Lower a trait reference to a trait instance implemented by `self_ty`.
///
/// `Self` in the written generic arguments and associated type bindings is the
/// use site's `Self`. Inside a trait, that is the trait's own `Self`; when
/// `owner_self` is provided, it is instantiated as `owner_self`, as when the
/// bounds of `type Assoc: Encode<Self>` are read for an implementation.
/// Defaults omitted from the reference, such as `Rhs = Self`, are completed
/// against `self_ty`.
#[salsa::tracked(returns(clone), cycle_initial=lower_trait_ref_cycle_initial)]
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
    if !trait_ref.path(db).is_present() {
        return Err(TraitRefLowerError::Ignored);
    }

    let lowering_cx = LoweringContext::for_const_bodies(const_bodies);

    lower_trait_ref_with_minter(
        db,
        self_ty,
        trait_ref,
        scope,
        assumptions,
        owner_self,
        &lowering_cx,
    )
}

pub(crate) fn lower_trait_ref_with_minter<'db>(
    db: &'db dyn HirAnalysisDb,
    self_ty: TyId<'db>,
    trait_ref: TraitRefId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    owner_self: Option<TyId<'db>>,
    lowering_cx: &LoweringContext<'db>,
) -> Result<TraitInstId<'db>, TraitRefLowerError<'db>> {
    let Partial::Present(path) = trait_ref.path(db) else {
        return Err(TraitRefLowerError::Ignored);
    };
    match resolve_trait_path_with_observer_and_minter(
        db,
        path,
        scope,
        assumptions,
        &mut |_, _| {},
        lowering_cx,
    ) {
        Ok(PathRes::Trait(t)) => {
            // Instantiates the use site's trait `Self` as `owner_self`.
            struct OwnerSelf<'db>(TyId<'db>);
            impl<'db> TyFolder<'db> for OwnerSelf<'db> {
                fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
                    match ty.data(db) {
                        TyData::TyParam(p) if p.is_trait_self() => self.0,
                        _ => ty.super_fold_with(db, self),
                    }
                }
            }
            let written_self = |ty: TyId<'db>| {
                owner_self.map_or(ty, |owner| ty.fold_with(db, &mut OwnerSelf(owner)))
            };

            // The resolved instance completes its defaults against the trait's
            // own `Self`. Keep only the written arguments and complete the
            // defaults again against the implementor.
            let written = path
                .generic_args(db)
                .data(db)
                .iter()
                .filter(|arg| !matches!(arg, GenericArg::AssocType(_)))
                .count();
            let explicit: Vec<_> = t.args(db)[1..=written]
                .iter()
                .map(|&ty| written_self(ty))
                .collect();
            let args = complete_trait_args(db, t.def(db), self_ty, &explicit, lowering_cx)
                .map_err(|err| {
                    TraitRefLowerError::PathResError(PathResError::new(err.into(), path))
                })?;
            let assoc_bindings: IndexMap<_, _> = t
                .assoc_type_bindings(db)
                .iter()
                .map(|(&name, &ty)| (name, written_self(ty)))
                .collect();

            Ok(TraitInstId::new(
                db,
                t.def(db),
                args,
                assoc_bindings,
                t.mode(db),
            ))
        }
        Ok(res) => Err(TraitRefLowerError::InvalidDomain(res)),
        Err(e) => Err(TraitRefLowerError::PathResError(e)),
    }
}

fn lower_trait_ref_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _self_ty: TyId<'db>,
    _trait_ref: TraitRefId<'db>,
    _scope: ScopeId<'db>,
    _assumptions: PredicateListId<'db>,
    _owner_self: Option<TyId<'db>>,
) -> Result<TraitInstId<'db>, TraitRefLowerError<'db>> {
    Err(TraitRefLowerError::Cycle)
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
    /// An argument failed compile-time evaluation, which no other pass
    /// reports for a trait argument. `applied_default` marks a fault that only
    /// applying a valid default at this reference reveals.
    InvalidArg {
        arg: TyId<'db>,
        applied_default: bool,
    },
    Ignored,
}

impl<'db> From<TraitArgError<'db>> for PathResErrorKind<'db> {
    fn from(err: TraitArgError<'db>) -> Self {
        match err {
            TraitArgError::ArgNumMismatch { expected, given } => {
                PathResErrorKind::ArgNumMismatch { expected, given }
            }
            TraitArgError::ArgKindMisMatch { expected, given } => {
                PathResErrorKind::ArgKindMisMatch { expected, given }
            }
            TraitArgError::ArgTypeMismatch { expected, given } => {
                PathResErrorKind::ArgTypeMismatch { expected, given }
            }
            TraitArgError::InvalidArg {
                arg,
                applied_default,
            } => PathResErrorKind::InvalidTraitArg {
                arg,
                applied_default,
            },
            TraitArgError::Ignored => PathResErrorKind::ParseError,
        }
    }
}

pub(crate) fn lower_trait_ref_impl_with_minter<'db>(
    db: &'db (dyn HirAnalysisDb + 'static),
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    t: Trait<'db>,
    lowering_cx: &LoweringContext<'db>,
) -> Result<TraitInstId<'db>, TraitArgError<'db>> {
    let generic_args = path.generic_args(db);
    // Lower provided explicit args (excluding Self) as a type's generic args
    // are: a path among them is the constant it names, if it names one.
    let lowered = lower_generic_arg_list(db, generic_args, scope, assumptions, lowering_cx);
    let mut provided_explicit = Vec::new();
    let mut assoc_bindings = IndexMap::new();
    for (arg, lowered) in generic_args.data(db).iter().zip(lowered) {
        match arg {
            GenericArg::Type(_) | GenericArg::Const(_) => provided_explicit.push(lowered),
            // A trait takes no mode, which is reported where it is written.
            GenericArg::Mode(_) => return Err(TraitArgError::Ignored),
            GenericArg::AssocType(AssocTypeGenericArg { name, mode, ty }) => {
                if let (Some(name), Some(ty)) = (name.to_opt(), ty.to_opt()) {
                    // An associated shape is bound to a shape, at a mode.
                    let ty = if mode.is_some()
                        || t.assoc_ty(db, name).is_some_and(|decl| decl.is_shape())
                    {
                        lower_shape_value(db, ty, scope, assumptions, lowering_cx)
                    } else {
                        lower_hir_ty_with_minter(db, ty, scope, assumptions, lowering_cx)
                    };
                    let mode = mode.map(|mode| match mode {
                        TypeMode::Mut => ShapeMode::Mut,
                        _ => ShapeMode::Ref,
                    });
                    // A second binding is reported where it is written.
                    assoc_bindings
                        .entry(AssocBinding { name, mode })
                        .or_insert(ty);
                }
            }
        }
    }

    let args = complete_trait_args(db, t, t.self_param(db), &provided_explicit, lowering_cx)?;
    Ok(TraitInstId::new(
        db,
        t,
        args,
        assoc_bindings,
        ModeMarker::None,
    ))
}

/// Completes the arguments of `trait_` implemented by `self_ty` from the
/// `explicit` ones, filling omitted trailing parameters with their defaults.
fn complete_trait_args<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db>,
    self_ty: TyId<'db>,
    explicit: &[TyId<'db>],
    lowering_cx: &LoweringContext<'db>,
) -> Result<Vec<TyId<'db>>, TraitArgError<'db>> {
    let trait_params: &[TyId<'db>] = trait_.params(db);
    let application = || match lowering_cx.const_bodies() {
        ConstBodyLowering::Eager => DefaultApplication::Evaluate(lowering_cx),
        ConstBodyLowering::Deferred => DefaultApplication::StructuralMetadata(lowering_cx),
    };
    let complete = |self_ty, provided: &[TyId<'db>]| {
        trait_
            .param_set(db)
            .complete_args(db, &[self_ty], provided, application())
    };
    let non_self_completed = complete(self_ty, explicit).map_err(|error| match error.cause {
        // A surplus argument is one too many whatever it is.
        InvalidCause::TooManyGenericArgs { expected, given } => {
            TraitArgError::ArgNumMismatch { expected, given }
        }
        // A type argument whose own lowering failed is reported where it is
        // written, with the reference's argument diagnostics.
        _ if explicit
            .get(error.index)
            .is_some_and(|arg| !arg.is_const_ty(db) && arg.has_invalid(db)) =>
        {
            TraitArgError::Ignored
        }
        // A default that faults for the trait's own parameters faults at
        // its declaration, which reports it; otherwise only applying it
        // to this reference's arguments reveals the fault.
        cause if cause.const_eval_fault().is_some() => TraitArgError::InvalidArg {
            arg: TyId::invalid(db, cause),
            applied_default: error.from_default
                && complete(trait_.self_param(db), &trait_params[1..=explicit.len()]).is_ok(),
        },
        InvalidCause::KindMismatch {
            expected: Some(expected),
            given,
        } => TraitArgError::ArgKindMisMatch { expected, given },
        InvalidCause::ConstTyMismatch { expected, given } => TraitArgError::ArgTypeMismatch {
            expected: Some(expected),
            given: Some(given),
        },
        InvalidCause::ConstTyExpected { expected } => TraitArgError::ArgTypeMismatch {
            expected: Some(expected),
            given: None,
        },
        InvalidCause::NormalTypeExpected { given } => TraitArgError::ArgTypeMismatch {
            expected: None,
            given: Some(given),
        },
        _ => TraitArgError::Ignored,
    })?;

    if non_self_completed.len() != trait_params.len() - 1 {
        return Err(TraitArgError::ArgNumMismatch {
            expected: trait_params.len() - 1,
            given: non_self_completed.len(),
        });
    }

    let mut args = Vec::with_capacity(trait_params.len());
    args.push(self_ty);
    args.extend(non_self_completed);
    Ok(args)
}

/// An implementation's methods, by name. A `mut?` trait method may have one
/// body per mode: two methods of its name, the one that takes the `mut`
/// mode where the trait method takes `mut?` being its `mut` body.
#[derive(Debug, Clone, Default, PartialEq, Eq, SalsaValue)]
pub(crate) struct ImplementorMethods<'db> {
    /// Each name's method, or its `ref` body.
    pub methods: IndexMap<IdentId<'db>, Func<'db>>,
    /// The `mut` bodies of the methods with one body per mode.
    pub mut_bodies: IndexMap<IdentId<'db>, Func<'db>>,
    /// Methods of a name taken, and the ones of that name left over.
    pub conflicts: Vec<(Func<'db>, Func<'db>)>,
}

#[salsa::tracked]
pub(crate) fn collect_implementor_methods<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: ImplementorId<'db>,
) -> ImplementorMethods<'db> {
    let mut methods = ImplementorMethods::default();
    let impl_trait = match implementor.origin(db) {
        super::trait_def::ImplementorOrigin::Hir(impl_trait) => impl_trait,
        super::trait_def::ImplementorOrigin::VirtualContract(_)
        | super::trait_def::ImplementorOrigin::Assumption
        | super::trait_def::ImplementorOrigin::Callable
        | super::trait_def::ImplementorOrigin::Structural => return methods,
    };
    let trait_methods = implementor.trait_def(db).method_defs(db);
    let scope = impl_trait.scope();
    let graph = scope.scope_graph(db);
    for method in graph.child_items(scope).filter_map(|item| match item {
        ItemKind::Func(func) => Some(func),
        _ => None,
    }) {
        let Some(name) = method.name(db).to_opt() else {
            continue;
        };
        let Some(&first) = methods.methods.get(&name) else {
            methods.methods.insert(name, method);
            continue;
        };
        // A second method of a `mut?` trait method's name is its other body.
        let is_mut_body = |func| {
            trait_methods
                .get(&name)
                .is_some_and(|&trait_m| takes_mut_body(db, func, trait_m))
        };
        if methods.mut_bodies.contains_key(&name) || is_mut_body(first) == is_mut_body(method) {
            methods.conflicts.push((first, method));
        } else if is_mut_body(first) {
            methods.methods.insert(name, method);
            methods.mut_bodies.insert(name, first);
        } else {
            methods.mut_bodies.insert(name, method);
        }
    }

    methods
}

/// Whether `func`, a method of a `mut?` trait method `trait_m`'s name, is
/// its `mut` body: it takes `mut` where `trait_m` takes `mut?`, as a
/// parameter, a product parameter's part or a result component.
fn takes_mut_body<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>, trait_m: Func<'db>) -> bool {
    /// Whether `shape` is `mut` where the trait method's shape, `at_ref` and
    /// `at_mut` at its two modes, is `mut?`.
    fn mut_where_maybe_mut(shape: &Shape, at_ref: &Shape, at_mut: &Shape) -> bool {
        match (shape, at_ref, at_mut) {
            (Shape::Access(kind, _), Shape::Access(at_ref, _), Shape::Access(at_mut, _)) => {
                at_ref != at_mut && *kind == BorrowKind::Mut
            }
            (Shape::Tuple(elems), Shape::Tuple(at_ref), Shape::Tuple(at_mut)) => elems
                .iter()
                .zip(at_ref)
                .zip(at_mut)
                .any(|((elem, at_ref), at_mut)| mut_where_maybe_mut(elem, at_ref, at_mut)),
            (
                Shape::Sum { payload, .. },
                Shape::Sum {
                    payload: at_ref, ..
                },
                Shape::Sum {
                    payload: at_mut, ..
                },
            ) => mut_where_maybe_mut(payload, at_ref, at_mut),
            _ => false,
        }
    }
    trait_m.is_maybe_mut(db)
        && (trait_m
            .params(db)
            .zip(func.params(db))
            .any(|(trait_param, param)| {
                trait_param.is_maybe_mut(db) && param.mode_at(db, None) == FuncParamMode::Mut
                    || matches!(
                        (
                            param.product(db, None),
                            trait_param.product(db, Some(BorrowKind::Ref)),
                            trait_param.product(db, Some(BorrowKind::Mut)),
                        ),
                        (Some(shape), Some(at_ref), Some(at_mut))
                            if mut_where_maybe_mut(shape, at_ref, at_mut)
                    )
            })
            || matches!(
                (
                    func.return_shape(db, None),
                    trait_m.return_shape(db, Some(BorrowKind::Ref)),
                    trait_m.return_shape(db, Some(BorrowKind::Mut)),
                ),
                (Some(shape), Some(at_ref), Some(at_mut))
                    if mut_where_maybe_mut(shape, at_ref, at_mut)
            ))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
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
    use salsa::plumbing::AsId;

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
            top_mod.as_id(),
            TyId::invalid(&db, InvalidCause::Other),
            TraitRefId::new(&db, Partial::Absent),
            scope,
            PredicateListId::empty_list(&db),
            None,
        );

        assert_eq!(result, Err(TraitRefLowerError::Cycle));
    }
}
