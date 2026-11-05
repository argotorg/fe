use crate::hir_def::{
    Enum, EnumVariant, FieldParent, GenericParamOwner, IdentId, ItemKind, Partial, Struct,
    TypeId as HirTyId, VariantKind, scope_graph::ScopeId,
};
use crate::span::DynLazySpan;
use common::ingot::Ingot;
use salsa::SalsaValue;

use super::{
    binder::Binder,
    const_ty::ConstBodyLowering,
    corelib::resolve_core_trait,
    fold::TyFoldable,
    normalize::normalize_ty,
    provider::{AddressSpace, is_state_pin, pinned_space},
    trait_def::{ImplementorOrigin, impls_for_trait_def},
    trait_resolution::{
        GoalSatisfiability, PredicateListId, TraitSolveCx, constraint::collect_constraints,
        is_goal_satisfiable,
    },
    ty_def::{InvalidCause, PrimTy, TyBase, TyData, TyId},
    ty_lower::{
        CompleteSubst, GenericParamTypeSet, ParamDomainId, ParamSchemaId, lower_hir_ty_in_mode,
    },
    unify::UnificationTable,
};
use crate::analysis::HirAnalysisDb;
use crate::semantic::adt_grows;

/// Represents a ADT type definition.
#[salsa::tracked]
#[derive(Debug)]
pub struct AdtDef<'db> {
    #[returns(copy)]
    pub adt_ref: AdtRef<'db>,

    /// Type parameters of the ADT.
    pub param_set: GenericParamTypeSet<'db>,

    /// Fields of the ADT, if the ADT is an enum, this represents variants.
    /// Otherwise, `fields[0]` represents all fields of the struct.
    pub fields: Vec<AdtField<'db>>,
}

/// Where a type is pinned: to the state space its `core::marker::Pinned`
/// implementation names as `Space`. Implementing the trait is the pinning
/// declaration.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum Pin {
    /// No implementation pins the type, or any instance of it.
    Unpinned,
    Pinned(AddressSpace),
    /// Whether the type is pinned depends on its parameters: separate
    /// implementations, or a space forwarded from them, pin some instances.
    Undecided,
}

/// The pin of `ty`: the space of the `Pinned` implementation that trait
/// selection picks for it, bounds checked, as lowering resolves it. A type
/// no implementation is selected for is undecided while one could still
/// apply to an instance of it (its parameters, or bounds they leave open),
/// and unpinned otherwise.
#[salsa::tracked(returns(copy), cycle_initial = ty_pin_cycle_initial)]
pub(crate) fn ty_pin<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Pin {
    let Some(adt) = ty.adt_def(db) else {
        return Pin::Unpinned;
    };
    // A type still being inferred stands for any instance of its ADT.
    let ty = if ty.has_var(db) {
        TyId::foldl(db, TyId::adt(db, adt), adt.params(db))
    } else {
        ty
    };
    let scope = adt.scope(db);
    let Some(pinned) = resolve_core_trait(db, scope, &["marker", "Pinned"]) else {
        return Pin::Unpinned;
    };
    if let Some(space) = pinned_space(db, scope, PredicateListId::empty_list(db), ty) {
        return Pin::Pinned(space);
    }
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(PredicateListId::empty_list(db));
    let may_apply = impls_for_trait_def(db, adt.ingot(db), pinned)
        .iter()
        .any(|implementor| {
            // A pin to another space is an error at the implementation.
            if let ImplementorOrigin::Hir(impl_trait) = implementor.origin(db)
                && !is_state_pin(db, impl_trait)
            {
                return false;
            }
            let mut table = UnificationTable::new(db);
            let instance = table.instantiate_with_fresh_vars(*implementor);
            let ty = table.instantiate_with_fresh_vars(ty);
            table.unify(instance.self_ty(db), ty).is_ok()
                && instance.constraints(db).list(db).iter().all(|constraint| {
                    !matches!(
                        is_goal_satisfiable(db, solve_cx, constraint.fold_with(db, &mut table)),
                        GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid
                    )
                })
        });
    if may_apply {
        Pin::Undecided
    } else {
        Pin::Unpinned
    }
}

fn ty_pin_cycle_initial<'db>(_: &'db dyn HirAnalysisDb, _: salsa::Id, _: TyId<'db>) -> Pin {
    Pin::Unpinned
}

/// The type that is or may be pinned that `ty` is or holds in a field, or
/// in a tuple or array element; see `TyId::pinned_part`.
#[salsa::tracked(returns(copy), cycle_initial = ty_pinned_part_cycle_initial)]
pub(crate) fn ty_pinned_part<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<TyId<'db>> {
    match ty.base_ty(db).data(db) {
        TyData::TyBase(TyBase::Adt(adt)) => {
            if ty.may_be_pinned(db) {
                return Some(ty);
            }
            // A type not applied to all its arguments has no values. A
            // growing type, rejected where it is defined, reaches a new type
            // through each pointer, so its fields are never searched.
            let args = ty.generic_args(db);
            if args.len() != adt.params(db).len() || adt_grows(db, *adt) {
                return None;
            }
            // A field of an associated type holds the type it names.
            adt.fields(db)
                .iter()
                .flat_map(|variant| variant.iter_types(db))
                .find_map(|field| {
                    normalize_ty(
                        db,
                        field.instantiate(db, args),
                        adt.scope(db),
                        PredicateListId::empty_list(db),
                    )
                    .pinned_part(db)
                })
        }
        TyData::TyBase(TyBase::Prim(PrimTy::Tuple(_) | PrimTy::Array)) => ty
            .generic_args(db)
            .iter()
            .find_map(|elem| elem.pinned_part(db)),
        // A memory pointer's target lies in memory, where a pinned type never
        // does; a state pointer is a handle to a place in state.
        TyData::TyBase(TyBase::Prim(PrimTy::Ptr(
            AddressSpace::Memory | AddressSpace::Calldata | AddressSpace::Code,
        ))) => ty.as_ptr(db)?.0.pinned_part(db),
        _ => None,
    }
}

fn ty_pinned_part_cycle_initial<'db>(
    _: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _: TyId<'db>,
) -> Option<TyId<'db>> {
    None
}

/// The `#[view]` type `ty` holds as a part: an argument of a tuple, an array
/// or a pointer, or of an ADT parameter its fields mention, at any depth; see
/// `TyId::view_part`.
pub(crate) fn ty_view_part<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<TyId<'db>> {
    let held = match ty.base_ty(db).data(db) {
        TyData::TyBase(TyBase::Adt(adt)) => adt_field_params(db, *adt).as_slice(),
        TyData::TyBase(TyBase::Prim(PrimTy::Tuple(_) | PrimTy::Array | PrimTy::Ptr(_))) => &[],
        _ => return None,
    };
    ty.generic_args(db)
        .iter()
        .enumerate()
        .filter(|(idx, _)| held.get(*idx).copied().unwrap_or(true))
        .find_map(|(_, &arg)| arg.is_view(db).then_some(arg).or_else(|| arg.view_part(db)))
}

/// Whether some field of `adt` holds a value of each of its parameters: an
/// argument for one that none does only distinguishes the type. Type lowering
/// asks this of every ADT it names, impl headers included, so the fields'
/// const bodies stay unevaluated: evaluating one proves bounds through those
/// impls, and a const holds no value anyway.
#[salsa::tracked(cycle_initial = adt_field_params_cycle_initial)]
pub(crate) fn adt_field_params<'db>(db: &'db dyn HirAnalysisDb, adt: AdtDef<'db>) -> Vec<bool> {
    let params = adt.params(db);
    if params.is_empty() {
        return Vec::new();
    }
    let fields: Vec<_> = adt
        .fields(db)
        .iter()
        .flat_map(|variant| {
            (0..variant.num_types()).map(|i| variant.ty_in_mode(db, i, ConstBodyLowering::Deferred))
        })
        .map(|field| field.instantiate_identity())
        .collect();
    params
        .iter()
        .map(|&param| fields.iter().any(|&field| holds(db, field, param)))
        .collect()
}

/// Whether a value of `ty` holds a value of `param`: it is one, or holds one
/// through an argument of a tuple, an array or a pointer, or of an ADT
/// parameter that its fields hold. A projection may be any type, an
/// equality's among them, so it holds every parameter.
fn holds<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, param: TyId<'db>) -> bool {
    if ty == param {
        return true;
    }
    match ty.base_ty(db).data(db) {
        TyData::AssocTy(_) | TyData::QualifiedTy(_) => true,
        TyData::TyBase(TyBase::Adt(adt)) => {
            let held = adt_field_params(db, *adt);
            ty.generic_args(db)
                .iter()
                .enumerate()
                .any(|(idx, &arg)| held.get(idx).copied().unwrap_or(true) && holds(db, arg, param))
        }
        _ => ty.generic_args(db).iter().any(|&arg| holds(db, arg, param)),
    }
}

// Until the fields are known, every parameter may be held.
fn adt_field_params_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    adt: AdtDef<'db>,
) -> Vec<bool> {
    vec![true; adt.params(db).len()]
}

impl<'db> AdtDef<'db> {
    pub(crate) fn name(self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        self.adt_ref(db).name(db)
    }

    pub fn name_span(self, db: &'db dyn HirAnalysisDb) -> DynLazySpan<'db> {
        self.adt_ref(db).name_span(db)
    }

    pub(crate) fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set(db).params(db)
    }

    pub(crate) fn is_struct(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.adt_ref(db), AdtRef::Struct(_))
    }

    /// Whether some field of some variant is an `unsafe` field, whose value
    /// carries an invariant of the type: the all-zero representation need
    /// not satisfy it, so zero is no value of the type.
    pub(crate) fn has_unsafe_field(self, db: &'db dyn HirAnalysisDb) -> bool {
        let parents: Vec<_> = match self.adt_ref(db) {
            AdtRef::Struct(struct_) => vec![FieldParent::Struct(struct_)],
            // Only a record variant names its fields, and so marks them.
            AdtRef::Enum(enum_) => (0..self.fields(db).len())
                .map(|idx| EnumVariant::new(enum_, idx))
                .filter(|variant| matches!(variant.kind(db), VariantKind::Record(_)))
                .map(FieldParent::Variant)
                .collect(),
        };
        parents
            .into_iter()
            .any(|parent| parent.fields(db).any(|field| field.is_unsafe(db)))
    }

    pub fn scope(self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        self.adt_ref(db).scope()
    }

    pub(crate) fn variant_ty_span(
        self,
        db: &'db dyn HirAnalysisDb,
        field_idx: usize,
        ty_idx: usize,
    ) -> DynLazySpan<'db> {
        match self.adt_ref(db) {
            AdtRef::Enum(e) => {
                let span = e.variant_span(field_idx);
                match e
                    .variants(db)
                    .nth(field_idx)
                    .expect("variant not found")
                    .kind(db)
                {
                    VariantKind::Tuple(_) => span.tuple_type().elem_ty(ty_idx).into(),
                    VariantKind::Record(_) => span.fields().field(ty_idx).ty().into(),
                    VariantKind::Unit => unreachable!(),
                }
            }

            AdtRef::Struct(s) => s.span().fields().field(ty_idx).ty().into(),
        }
    }

    pub(crate) fn ingot(self, db: &'db dyn HirAnalysisDb) -> Ingot<'db> {
        match self.adt_ref(db) {
            AdtRef::Enum(e) => e.top_mod(db).ingot(db),
            AdtRef::Struct(s) => s.top_mod(db).ingot(db),
        }
    }

    pub(crate) fn as_generic_param_owner(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> GenericParamOwner<'db> {
        self.adt_ref(db).generic_owner()
    }
}

/// This struct represents a field of an ADT. If the ADT is an enum, this
/// represents a variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct AdtField<'db> {
    /// Field types as HIR type refs. To allow recursive types, these are kept
    /// at the HIR level and lowered on demand.
    tys: Vec<Partial<HirTyId<'db>>>,

    /// Scope of the containing ADT item.
    scope: ScopeId<'db>,
}
impl<'db> AdtField<'db> {
    fn assumptions(&self, db: &'db dyn HirAnalysisDb) -> PredicateListId<'db> {
        match self.scope {
            ScopeId::Item(ItemKind::Struct(struct_)) => {
                collect_constraints(db, GenericParamOwner::Struct(struct_)).instantiate_identity()
            }
            ScopeId::Item(ItemKind::Enum(enum_)) => {
                collect_constraints(db, GenericParamOwner::Enum(enum_)).instantiate_identity()
            }
            _ => PredicateListId::empty_list(db),
        }
    }

    pub fn ty(&self, db: &'db dyn HirAnalysisDb, i: usize) -> Binder<'db, TyId<'db>> {
        self.ty_in_mode(db, i, ConstBodyLowering::Eager)
    }

    /// Deferred const bodies keep anonymous bodies and their captures for
    /// concrete-demand diagnostics.
    fn ty_in_mode(
        &self,
        db: &'db dyn HirAnalysisDb,
        i: usize,
        const_bodies: ConstBodyLowering,
    ) -> Binder<'db, TyId<'db>> {
        let ty = if let Some(hir_ty) = self.tys[i].to_opt() {
            lower_hir_ty_in_mode(db, hir_ty, self.scope, self.assumptions(db), const_bodies)
        } else {
            TyId::invalid(db, InvalidCause::ParseError)
        };

        match GenericParamOwner::from_item_opt(self.scope.item()) {
            Some(owner) => Binder::bind(owner, ty),
            None => Binder::closed(ty),
        }
    }

    /// Iterates all field types of this variant.
    pub fn iter_types<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = Binder<'db, TyId<'db>>> + 'a {
        (0..self.num_types()).map(move |i| self.ty(db, i))
    }

    pub fn num_types(&self) -> usize {
        self.tys.len()
    }

    pub(crate) fn new(tys: Vec<Partial<HirTyId<'db>>>, scope: ScopeId<'db>) -> Self {
        Self { tys, scope }
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From, salsa::Supertype, SalsaValue,
)]
pub enum AdtRef<'db> {
    Enum(Enum<'db>),
    Struct(Struct<'db>),
}

impl<'db> AdtRef<'db> {
    pub fn try_from_item(item: ItemKind<'db>) -> Option<Self> {
        match item {
            ItemKind::Enum(x) => Some(x.into()),
            ItemKind::Struct(x) => Some(x.into()),
            _ => None,
        }
    }

    pub fn scope(self) -> ScopeId<'db> {
        match self {
            Self::Enum(e) => e.scope(),
            Self::Struct(s) => s.scope(),
        }
    }

    pub fn as_item(self) -> ItemKind<'db> {
        match self {
            AdtRef::Enum(e) => e.into(),
            AdtRef::Struct(s) => s.into(),
        }
    }

    pub fn name(self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        match self {
            AdtRef::Enum(e) => e.name(db),
            AdtRef::Struct(s) => s.name(db),
        }
        .to_opt()
    }

    pub fn kind_name(self) -> &'static str {
        self.as_item().kind_name()
    }

    pub fn name_span(self, db: &'db dyn HirAnalysisDb) -> DynLazySpan<'db> {
        self.scope()
            .name_span(db)
            .unwrap_or_else(DynLazySpan::invalid)
    }

    pub fn is_must_use(self, db: &'db dyn HirAnalysisDb) -> bool {
        match self {
            AdtRef::Enum(enum_) => enum_.is_must_use(db),
            AdtRef::Struct(struct_) => struct_.is_must_use(db),
        }
    }

    pub fn is_view(self, db: &'db dyn HirAnalysisDb) -> bool {
        match self {
            AdtRef::Enum(enum_) => enum_.is_view(db),
            AdtRef::Struct(struct_) => struct_.is_view(db),
        }
    }

    /// Returns the semantic ADT definition for this reference.
    /// Thin wrapper over the tracked `lower_adt` query for ergonomic use at call sites.
    pub fn as_adt(self, db: &'db dyn HirAnalysisDb) -> AdtDef<'db> {
        crate::core::adt_lower::lower_adt(db, self)
    }

    pub(crate) fn generic_owner(self) -> GenericParamOwner<'db> {
        match self {
            AdtRef::Enum(e) => e.into(),
            AdtRef::Struct(s) => s.into(),
        }
    }
}

/// Struct for downstream diagnostics that refer to cycle members.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct AdtCycleMember<'db> {
    pub adt: AdtDef<'db>,
    pub field_idx: usize,
    pub ty_idx: usize,
}

/// Instantiates an ADT field's type with `explicit_args`; omitted trailing
/// arguments keep their declaration formals.
pub fn instantiate_adt_field_shape<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
    variant_idx: usize,
    field_idx: usize,
    explicit_args: &[TyId<'db>],
) -> TyId<'db> {
    instantiate_adt_field_shape_in_mode(
        db,
        adt,
        variant_idx,
        field_idx,
        explicit_args,
        ConstBodyLowering::Eager,
    )
}

pub(crate) fn instantiate_adt_field_for_concrete_demand<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
    variant_idx: usize,
    field_idx: usize,
    canonical_args: &[TyId<'db>],
    source_args: &[TyId<'db>],
) -> ConcreteTypeView<'db> {
    ConcreteTypeView {
        canonical: instantiate_adt_field_shape_in_mode(
            db,
            adt,
            variant_idx,
            field_idx,
            canonical_args,
            ConstBodyLowering::Eager,
        ),
        source: instantiate_adt_field_source_for_concrete_demand(
            db,
            adt,
            variant_idx,
            field_idx,
            source_args,
        ),
    }
}

/// The canonical field type supplies layout identity; the source view keeps
/// anonymous const bodies and their captures available for concrete errors.
#[derive(Clone, Copy)]
pub(crate) struct ConcreteTypeView<'db> {
    pub canonical: TyId<'db>,
    pub source: TyId<'db>,
}

impl<'db> ConcreteTypeView<'db> {
    pub(crate) fn new(canonical: TyId<'db>, source: TyId<'db>) -> Self {
        Self { canonical, source }
    }

    pub(crate) fn identity(ty: TyId<'db>) -> Self {
        Self::new(ty, ty)
    }
}

pub(crate) fn instantiate_adt_field_source_for_concrete_demand<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
    variant_idx: usize,
    field_idx: usize,
    source_args: &[TyId<'db>],
) -> TyId<'db> {
    instantiate_adt_field_shape_in_mode(
        db,
        adt,
        variant_idx,
        field_idx,
        source_args,
        ConstBodyLowering::Deferred,
    )
}

fn instantiate_adt_field_shape_in_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
    variant_idx: usize,
    field_idx: usize,
    explicit_args: &[TyId<'db>],
    const_bodies: ConstBodyLowering,
) -> TyId<'db> {
    let domain = ParamDomainId::full(db, ParamSchemaId::full(db, adt.as_generic_param_owner(db)));
    adt.fields(db)
        .get(variant_idx)
        .filter(|variant| field_idx < variant.num_types() && explicit_args.len() <= domain.len(db))
        .map_or_else(
            || TyId::invalid(db, InvalidCause::Other),
            |variant| {
                // Omitted trailing arguments keep their declaration formals.
                let subst = CompleteSubst::with_prefix(db, domain, explicit_args);
                variant
                    .ty_in_mode(db, field_idx, const_bodies)
                    .instantiate_subst(db, &subst)
                    .expect("ADT field uses its declaration domain")
            },
        )
}
