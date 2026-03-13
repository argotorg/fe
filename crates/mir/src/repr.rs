//! Runtime representation queries for MIR lowering and codegen.
//!
//! Fe exposes rich structural types (structs/tuples/arrays/enums), but MIR uses a smaller set of
//! runtime representation categories:
//! - word-like values (`ValueRepr::Word`)
//! - opaque pointers (`ValueRepr::Ptr`)
//! - by-reference aggregates (`ValueRepr::Ref`)
//!
//! This module centralizes the logic for computing those representation categories, including
//! recursive elimination of "newtype" wrappers (single-field structs and single-element tuples).
//! Newtypes are treated as transparent wrappers: their runtime representation is the same as their
//! (recursively unwrapped) single field, while preserving the logical type (`TyId`) elsewhere in
//! MIR.

use hir::analysis::HirAnalysisDb;
use hir::analysis::ty::adt_def::AdtRef;
use hir::analysis::ty::normalize::normalize_ty;
use hir::analysis::ty::ty_def::{TyBase, TyData, TyId};
use hir::analysis::ty::{
    canonical::Canonicalized,
    corelib::resolve_core_trait,
    trait_def::TraitInstId,
    trait_resolution::{GoalSatisfiability, PredicateListId, TraitSolveCx, is_goal_satisfiable},
    ty_def::CapabilityKind,
};
use hir::hir_def::{EnumVariant, IdentId};
use hir::projection::{Projection, ProjectionPath};

use crate::core_lib::CoreLib;
use crate::ir::{
    AddressSpaceKind, LocalData, MirProjection, Place, PointerInfo, RuntimeShape, RuntimeWordKind,
    ValueData, ValueId, try_value_pointer_info_in,
};
use crate::layout;
use common::indexmap::IndexMap;

/// Canonical representation categories for a type after transparent-newtype peeling.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReprKind {
    /// No runtime representation (zero-sized).
    Zst,
    /// A direct EVM word value.
    Word,
    /// An opaque pointer-like EVM word value in an address space.
    Ptr(AddressSpaceKind),
    /// A by-reference aggregate value (pointer into an address space).
    Ref,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlaceAccessKind {
    Value,
    Location,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PlaceState<'db> {
    pub ty: TyId<'db>,
    pub access_kind: PlaceAccessKind,
    pub address_space: Option<AddressSpaceKind>,
    pub pointer_info: Option<PointerInfo<'db>>,
}

impl<'db> PlaceState<'db> {
    pub fn location_address_space(self) -> Option<AddressSpaceKind> {
        matches!(self.access_kind, PlaceAccessKind::Location)
            .then_some(self.address_space)
            .flatten()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DerefStepKind {
    ReuseLocation,
    UseBaseValue,
    LoadLocationValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedPlaceProjection<'db> {
    pub projection: MirProjection<'db>,
    pub owner: PlaceState<'db>,
    pub result: PlaceState<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedPlaceSegment<'db> {
    pub start_kind: Option<DerefStepKind>,
    pub before: PlaceState<'db>,
    pub base: PlaceState<'db>,
    pub projections: Vec<ResolvedPlaceProjection<'db>>,
}

impl<'db> ResolvedPlaceSegment<'db> {
    pub fn terminal_state(&self) -> PlaceState<'db> {
        self.projections
            .last()
            .map(|projection| projection.result)
            .unwrap_or(self.base)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedPlace<'db> {
    pub base: PlaceState<'db>,
    pub segments: Vec<ResolvedPlaceSegment<'db>>,
}

impl<'db> ResolvedPlace<'db> {
    pub fn final_state(&self) -> PlaceState<'db> {
        self.segments
            .last()
            .map(ResolvedPlaceSegment::terminal_state)
            .unwrap_or(self.base)
    }
}

/// Returns the single field type if `ty` is a transparent single-field wrapper.
///
/// Transparent wrappers include:
/// - single-field `struct` ADTs
/// - single-element tuples `(T,)`
///
/// This intentionally does *not* look through nested wrappers. Callers that want to peel multiple
/// levels should loop over this helper.
pub fn transparent_newtype_field_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<TyId<'db>> {
    if ty.is_tuple(db) {
        let field_tys = ty.field_types(db);
        return (field_tys.len() == 1).then(|| field_tys[0]);
    }

    let base_ty = ty.base_ty(db);
    let TyData::TyBase(TyBase::Adt(adt_def)) = base_ty.data(db) else {
        return None;
    };
    if !matches!(adt_def.adt_ref(db), AdtRef::Struct(_)) {
        return None;
    }
    let field_tys = ty.field_types(db);
    (field_tys.len() == 1).then(|| field_tys[0])
}

/// Returns the field type for a transparent field-0 projection step.
pub fn transparent_field0_inner_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    owner_ty: TyId<'db>,
    field_idx: usize,
) -> Option<TyId<'db>> {
    (field_idx == 0)
        .then(|| transparent_newtype_field_ty(db, owner_ty))
        .flatten()
}

/// Returns the next type for a transparent field-0 projection step.
pub fn transparent_field0_projection_step_ty<'db, Idx>(
    db: &'db dyn HirAnalysisDb,
    owner_ty: TyId<'db>,
    proj: &Projection<TyId<'db>, EnumVariant<'db>, Idx>,
) -> Option<TyId<'db>> {
    let Projection::Field(field_idx) = proj else {
        return None;
    };
    transparent_field0_inner_ty(db, owner_ty, *field_idx)
}

/// Peels a projection path that must consist entirely of transparent field-0 steps.
pub fn peel_transparent_field0_projection_path<'db, Idx>(
    db: &'db dyn HirAnalysisDb,
    mut base_ty: TyId<'db>,
    path: &ProjectionPath<TyId<'db>, EnumVariant<'db>, Idx>,
) -> Option<TyId<'db>> {
    for proj in path.iter() {
        base_ty = transparent_field0_projection_step_ty(db, base_ty, proj)?;
    }
    Some(base_ty)
}

/// Peel all transparent newtype layers from `ty`, returning the first non-newtype type.
pub fn peel_transparent_newtypes<'db>(db: &'db dyn HirAnalysisDb, mut ty: TyId<'db>) -> TyId<'db> {
    while let Some(inner) = transparent_newtype_field_ty(db, ty) {
        ty = inner;
    }
    ty
}

/// Returns the effect provider's address space for a type, looking through transparent newtype wrappers.
pub fn effect_provider_space_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
) -> Option<AddressSpaceKind> {
    if let Some((_, inner)) = ty.as_capability(db) {
        return effect_provider_space_for_ty(db, core, inner);
    }

    if let Some(space) = effect_provider_space_via_domain_trait(db, core, ty) {
        return Some(space);
    }

    transparent_newtype_field_ty(db, ty)
        .and_then(|inner| effect_provider_space_for_ty(db, core, inner))
}

/// Returns the normalized `EffectHandle::Target` type for an effect provider, looking through
/// transparent newtype wrappers.
pub fn effect_provider_target_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
) -> Option<TyId<'db>> {
    if let Some(target) = direct_deref_target_ty(db, core, ty) {
        return Some(target);
    }

    transparent_newtype_field_ty(db, ty)
        .and_then(|inner| effect_provider_target_ty(db, core, inner))
}

pub fn direct_deref_target_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
) -> Option<TyId<'db>> {
    if let Some((_, inner)) = ty.as_capability(db) {
        return Some(inner);
    }

    let effect_handle = resolve_core_trait(db, core.scope, &["effect_ref", "EffectHandle"])
        .expect("missing required core trait `core::effect_ref::EffectHandle`");
    let assumptions = PredicateListId::empty_list(db);
    let target_ident = IdentId::new(db, "Target".to_string());
    let inst = TraitInstId::new(db, effect_handle, vec![ty], IndexMap::new());
    let goal = Canonicalized::new(db, inst).value;
    match is_goal_satisfiable(
        db,
        TraitSolveCx::new(db, core.scope).with_assumptions(assumptions),
        goal,
    ) {
        GoalSatisfiability::Satisfied(_) => inst
            .assoc_ty(db, target_ident)
            .map(|assoc| normalize_ty(db, assoc, core.scope, assumptions))
            .filter(|target| !target.has_invalid(db)),
        GoalSatisfiability::NeedsConfirmation(_)
        | GoalSatisfiability::ContainsInvalid
        | GoalSatisfiability::UnSat(_) => None,
    }
}

pub fn deref_target_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
) -> Option<TyId<'db>> {
    direct_deref_target_ty(db, core, ty).or_else(|| {
        transparent_newtype_field_ty(db, ty).and_then(|inner| deref_target_ty(db, core, inner))
    })
}

pub fn pointer_info_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
    default_ref_space: AddressSpaceKind,
) -> Option<PointerInfo<'db>> {
    if let Some((capability, inner)) = ty.as_capability(db) {
        return match capability {
            CapabilityKind::Mut => Some(PointerInfo {
                address_space: default_ref_space,
                target_ty: Some(inner),
            }),
            CapabilityKind::Ref => match repr_kind_for_ref_inner(db, core, inner) {
                ReprKind::Ptr(AddressSpaceKind::Memory) => Some(PointerInfo {
                    address_space: default_ref_space,
                    target_ty: Some(inner),
                }),
                ReprKind::Zst | ReprKind::Word | ReprKind::Ptr(_) | ReprKind::Ref => None,
            },
            CapabilityKind::View => pointer_info_for_ty(db, core, inner, default_ref_space),
        };
    }

    if let Some(space) = effect_provider_space_for_ty(db, core, ty) {
        return Some(PointerInfo {
            address_space: space,
            target_ty: effect_provider_target_ty(db, core, ty),
        });
    }

    transparent_newtype_field_ty(db, ty)
        .and_then(|inner| pointer_info_for_ty(db, core, inner, default_ref_space))
}

pub fn handle_pointer_info_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
    default_ref_space: AddressSpaceKind,
) -> Option<PointerInfo<'db>> {
    if let Some((capability, inner)) = ty.as_capability(db) {
        return match capability {
            CapabilityKind::Mut | CapabilityKind::Ref => Some(PointerInfo {
                address_space: default_ref_space,
                target_ty: Some(inner),
            }),
            CapabilityKind::View => handle_pointer_info_for_ty(db, core, inner, default_ref_space),
        };
    }

    if let Some(address_space) = effect_provider_space_for_ty(db, core, ty) {
        return Some(PointerInfo {
            address_space,
            target_ty: effect_provider_target_ty(db, core, ty),
        });
    }

    transparent_newtype_field_ty(db, ty)
        .and_then(|inner| handle_pointer_info_for_ty(db, core, inner, default_ref_space))
}

pub fn runtime_pointer_info_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
    default_ref_space: AddressSpaceKind,
) -> Option<PointerInfo<'db>> {
    if let Some(info) = pointer_info_for_ty(db, core, ty, default_ref_space) {
        return Some(info);
    }

    matches!(repr_kind_for_ty(db, core, ty), ReprKind::Ref).then_some(PointerInfo {
        address_space: default_ref_space,
        target_ty: Some(ty),
    })
}

pub fn runtime_value_pointer_info_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
    address_space: AddressSpaceKind,
) -> Option<PointerInfo<'db>> {
    runtime_pointer_info_for_ty(db, core, ty, address_space)
        .or_else(|| handle_pointer_info_for_ty(db, core, ty, address_space))
}

pub(crate) fn infer_value_pointer_info<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    values: &[ValueData<'db>],
    locals: &[LocalData<'db>],
    value: ValueId,
) -> Option<PointerInfo<'db>> {
    let value_data = values.get(value.index())?;
    let typed_info_for_space =
        |address_space| runtime_value_pointer_info_for_ty(db, core, value_data.ty, address_space);
    let place_value_pointer_info = |place: &crate::ir::Place<'db>| {
        crate::ir::try_place_pointer_info_in(values, locals, place)
            .or_else(|| {
                crate::ir::try_place_address_space_in(values, locals, place).map(|address_space| {
                    PointerInfo {
                        address_space,
                        target_ty: None,
                    }
                })
            })
            .and_then(|info| typed_info_for_space(info.address_space))
    };

    match &value_data.origin {
        crate::ir::ValueOrigin::Local(local) | crate::ir::ValueOrigin::PlaceRoot(local) => {
            locals.get(local.index()).and_then(|local_data| {
                crate::ir::lookup_local_pointer_leaf_info(
                    locals,
                    *local,
                    &crate::MirProjectionPath::new(),
                )
                .or_else(|| typed_info_for_space(local_data.address_space))
                .or(value_data.pointer_info)
            })
        }
        crate::ir::ValueOrigin::TransparentCast { value: inner } => {
            value_data.pointer_info.or_else(|| {
                crate::ir::try_value_address_space_in(values, locals, *inner)
                    .and_then(typed_info_for_space)
                    .or_else(|| {
                        infer_value_pointer_info(db, core, values, locals, *inner)
                            .and_then(|info| typed_info_for_space(info.address_space))
                    })
                    .or_else(|| {
                        value_data
                            .repr
                            .address_space()
                            .and_then(typed_info_for_space)
                    })
            })
        }
        crate::ir::ValueOrigin::FieldPtr(field_ptr) => value_data
            .pointer_info
            .or_else(|| {
                runtime_value_pointer_info_for_ty(db, core, value_data.ty, field_ptr.addr_space)
            })
            .or(Some(PointerInfo {
                address_space: field_ptr.addr_space,
                target_ty: None,
            })),
        crate::ir::ValueOrigin::PlaceRef(place) | crate::ir::ValueOrigin::MoveOut { place } => {
            place_value_pointer_info(place)
                .or_else(|| {
                    value_data
                        .repr
                        .address_space()
                        .and_then(typed_info_for_space)
                })
                .or(value_data.pointer_info)
        }
        _ => value_data.pointer_info.or_else(|| {
            value_data
                .repr
                .address_space()
                .and_then(typed_info_for_space)
        }),
    }
}

fn place_base_access_kind<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    value: &ValueData<'db>,
    pointer_info: Option<PointerInfo<'db>>,
    place: &Place<'db>,
) -> PlaceAccessKind {
    if (value.repr.is_ref() && matches!(repr_kind_for_ty(db, core, value.ty), ReprKind::Ref))
        || pointer_info.is_some_and(|info| info.target_ty == Some(value.ty))
        || (place.projection.is_empty()
            && pointer_info.is_some_and(|info| info.target_ty.is_none()))
        || matches!(
            value.origin,
            crate::ir::ValueOrigin::PlaceRoot(_)
                | crate::ir::ValueOrigin::PlaceRef(_)
                | crate::ir::ValueOrigin::FieldPtr(_)
        )
    {
        return PlaceAccessKind::Location;
    }

    PlaceAccessKind::Value
}

fn projection_result_ty<'db, Idx>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    current: PlaceState<'db>,
    proj: &Projection<TyId<'db>, EnumVariant<'db>, Idx>,
) -> Option<TyId<'db>> {
    match proj {
        Projection::Field(field_idx) => current.ty.field_types(db).get(*field_idx).copied(),
        Projection::VariantField {
            variant,
            enum_ty,
            field_idx,
        } => {
            let ctor =
                hir::analysis::ty::simplified_pattern::ConstructorKind::Variant(*variant, *enum_ty);
            ctor.field_types(db).get(*field_idx).copied()
        }
        Projection::Discriminant => Some(TyId::new(
            db,
            TyData::TyBase(TyBase::Prim(hir::analysis::ty::ty_def::PrimTy::U256)),
        )),
        Projection::Index(_) => {
            let (base, args) = current.ty.decompose_ty_app(db);
            (base.is_array(db) && !args.is_empty()).then_some(args[0])
        }
        Projection::Deref => current
            .ty
            .as_capability(db)
            .map(|(_, inner)| inner)
            .or_else(|| effect_provider_target_ty(db, core, current.ty))
            .or_else(|| current.pointer_info.and_then(|info| info.target_ty)),
    }
}

fn state_pointer_info<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    locals: &[LocalData<'db>],
    local_root: Option<&(
        crate::LocalId,
        ProjectionPath<TyId<'db>, EnumVariant<'db>, ValueId>,
    )>,
    ty: TyId<'db>,
    address_space: Option<AddressSpaceKind>,
) -> Option<PointerInfo<'db>> {
    if let Some((local, path)) = local_root
        && let Some(info) = crate::ir::lookup_local_pointer_leaf_info(locals, *local, path)
    {
        return Some(info);
    }

    address_space.and_then(|space| runtime_pointer_info_for_ty(db, core, ty, space))
}

fn resolve_deref_step<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    locals: &[LocalData<'db>],
    local_root: &mut Option<(
        crate::LocalId,
        ProjectionPath<TyId<'db>, EnumVariant<'db>, ValueId>,
    )>,
    owner: PlaceState<'db>,
) -> Option<(DerefStepKind, PlaceState<'db>)> {
    let deref = Projection::<TyId<'db>, EnumVariant<'db>, ValueId>::Deref;
    let ty = projection_result_ty(db, core, owner, &deref)?;
    if matches!(owner.access_kind, PlaceAccessKind::Location)
        && matches!(repr_kind_for_ty(db, core, owner.ty), ReprKind::Ref)
    {
        return Some((
            DerefStepKind::ReuseLocation,
            PlaceState {
                ty,
                access_kind: PlaceAccessKind::Location,
                address_space: owner.address_space,
                pointer_info: state_pointer_info(
                    db,
                    core,
                    locals,
                    local_root.as_ref(),
                    ty,
                    owner.address_space,
                ),
            },
        ));
    }

    let info = owner.pointer_info?;
    *local_root = None;
    let address_space = Some(info.address_space);
    let start_kind = if matches!(owner.access_kind, PlaceAccessKind::Value) {
        DerefStepKind::UseBaseValue
    } else {
        DerefStepKind::LoadLocationValue
    };
    Some((
        start_kind,
        PlaceState {
            ty,
            access_kind: PlaceAccessKind::Location,
            address_space,
            pointer_info: runtime_pointer_info_for_ty(db, core, ty, info.address_space),
        },
    ))
}

pub fn resolve_place<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    values: &[ValueData<'db>],
    locals: &[LocalData<'db>],
    place: &Place<'db>,
) -> Option<ResolvedPlace<'db>> {
    let base_value = values.get(place.base.index())?;
    let mut local_root = crate::ir::resolve_local_projection_root(values, place.base);
    let root_address_space = crate::ir::try_value_address_space_in(values, locals, place.base);
    let base_pointer_info = state_pointer_info(
        db,
        core,
        locals,
        local_root.as_ref(),
        base_value.ty,
        root_address_space,
    )
    .or_else(|| crate::ir::try_value_pointer_info_in(values, locals, place.base));
    let base_access_kind = place_base_access_kind(db, core, base_value, base_pointer_info, place);
    let base_address_space = matches!(base_access_kind, PlaceAccessKind::Location)
        .then_some(root_address_space)
        .flatten();
    let base_state = PlaceState {
        ty: base_value.ty,
        access_kind: base_access_kind,
        address_space: base_address_space,
        pointer_info: base_pointer_info,
    };

    let mut current = base_state;
    let mut segments = Vec::new();
    let mut segment = ResolvedPlaceSegment {
        start_kind: None,
        before: base_state,
        base: base_state,
        projections: Vec::new(),
    };
    for proj in place.projection.iter().cloned() {
        let owner = current;
        current = match proj {
            Projection::Deref => {
                let (start_kind, next_state) =
                    resolve_deref_step(db, core, locals, &mut local_root, owner)?;

                if !segment.projections.is_empty() {
                    segments.push(segment);
                }
                segment = ResolvedPlaceSegment {
                    start_kind: Some(start_kind),
                    before: owner,
                    base: next_state,
                    projections: Vec::new(),
                };
                next_state
            }
            _ => {
                if !matches!(owner.access_kind, PlaceAccessKind::Location) {
                    return None;
                }
                let ty = projection_result_ty(db, core, owner, &proj)?;
                if let Some((_, path)) = &mut local_root {
                    path.push(proj.clone());
                }
                let address_space = owner.address_space;
                let next_state = PlaceState {
                    ty,
                    access_kind: PlaceAccessKind::Location,
                    address_space,
                    pointer_info: state_pointer_info(
                        db,
                        core,
                        locals,
                        local_root.as_ref(),
                        ty,
                        address_space,
                    ),
                };
                segment.projections.push(ResolvedPlaceProjection {
                    projection: proj,
                    owner,
                    result: next_state,
                });
                next_state
            }
        };
    }

    if !segment.projections.is_empty()
        || segment.start_kind.is_some()
        || place.projection.is_empty()
    {
        segments.push(segment);
    }

    Some(ResolvedPlace {
        base: base_state,
        segments,
    })
}

pub fn place_yields_location_value<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    values: &[ValueData<'db>],
    locals: &[LocalData<'db>],
    place: &Place<'db>,
    value_ty: TyId<'db>,
    pointer_info: Option<PointerInfo<'db>>,
) -> Option<bool> {
    let assumptions = PredicateListId::empty_list(db);
    let final_state = resolve_place(db, core, values, locals, place)?.final_state();
    if final_state.location_address_space().is_none() {
        return Some(false);
    }
    let final_ty = normalize_ty(db, final_state.ty, core.scope, assumptions);
    let value_ty = normalize_ty(db, value_ty, core.scope, assumptions);
    let value_repr = repr_kind_for_ty(db, core, value_ty);

    if matches!(value_repr, ReprKind::Ref)
        && final_state.location_address_space() == Some(AddressSpaceKind::Memory)
        && runtime_ty_matches(db, final_ty, value_ty)
    {
        return Some(true);
    }

    if matches!(value_repr, ReprKind::Ptr(_)) && runtime_ty_matches(db, final_ty, value_ty) {
        return Some(false);
    }

    if direct_deref_target_ty(db, core, value_ty)
        .map(|target| normalize_ty(db, target, core.scope, assumptions))
        .is_some_and(|target| runtime_ty_matches(db, target, final_ty))
    {
        return Some(true);
    }

    if pointer_info
        .and_then(|info| info.target_ty)
        .is_some_and(|target| {
            runtime_ty_matches(
                db,
                normalize_ty(db, target, core.scope, assumptions),
                final_ty,
            )
        })
    {
        return Some(true);
    }

    Some(false)
}

fn runtime_ty_matches<'db>(db: &'db dyn HirAnalysisDb, lhs: TyId<'db>, rhs: TyId<'db>) -> bool {
    let lhs = peel_transparent_newtypes(db, lhs);
    let rhs = peel_transparent_newtypes(db, rhs);
    lhs == rhs || lhs.pretty_print(db) == rhs.pretty_print(db)
}

fn effect_provider_space_via_domain_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
) -> Option<AddressSpaceKind> {
    let effect_handle = resolve_core_trait(db, core.scope, &["effect_ref", "EffectHandle"])
        .expect("missing required core trait `core::effect_ref::EffectHandle`");
    let assumptions = PredicateListId::empty_list(db);

    let address_space_ident = IdentId::new(db, "AddressSpace".to_string());

    // First, determine whether `ty` is an effect provider at all.
    let inst = TraitInstId::new(db, effect_handle, vec![ty], IndexMap::new());
    let goal = Canonicalized::new(db, inst).value;
    match is_goal_satisfiable(
        db,
        TraitSolveCx::new(db, core.scope).with_assumptions(assumptions),
        goal,
    ) {
        GoalSatisfiability::Satisfied(_) => {}
        GoalSatisfiability::NeedsConfirmation(_) => return None,
        GoalSatisfiability::ContainsInvalid | GoalSatisfiability::UnSat(_) => return None,
    }

    for (space_ty, space_kind) in [
        (core.addr_space_mem, AddressSpaceKind::Memory),
        (core.addr_space_calldata, AddressSpaceKind::Calldata),
        (core.addr_space_code, AddressSpaceKind::ImmutableCode),
        (
            core.addr_space_transient,
            AddressSpaceKind::TransientStorage,
        ),
        (core.addr_space_stor, AddressSpaceKind::Storage),
    ] {
        let mut assoc = IndexMap::new();
        assoc.insert(address_space_ident, space_ty);
        let inst = TraitInstId::new(db, effect_handle, vec![ty], assoc);
        let goal = Canonicalized::new(db, inst).value;
        match is_goal_satisfiable(
            db,
            TraitSolveCx::new(db, core.scope).with_assumptions(assumptions),
            goal,
        ) {
            GoalSatisfiability::Satisfied(_) => return Some(space_kind),
            GoalSatisfiability::NeedsConfirmation(_) => return None,
            GoalSatisfiability::ContainsInvalid | GoalSatisfiability::UnSat(_) => {}
        }
    }

    panic!(
        "`{}` implements `EffectHandle` but `AddressSpace` is not one of: core::effect_ref::Memory | Calldata | Code | Storage | TransientStorage",
        ty.pretty_print(db)
    )
}

/// Computes the canonical MIR representation kind for `ty`.
///
/// This is the single source of truth used by MIR lowering and post-processing. In particular,
/// transparent newtypes are recursively unwrapped so `struct A { b: Foo }` inherits the runtime
/// representation of `Foo`, and so on.
pub fn repr_kind_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
) -> ReprKind {
    if layout::is_zero_sized_ty(db, ty) {
        return ReprKind::Zst;
    }

    if let Some((capability, inner)) = ty.as_capability(db) {
        return match capability {
            CapabilityKind::Mut => ReprKind::Ptr(AddressSpaceKind::Memory),
            CapabilityKind::Ref => repr_kind_for_ref_inner(db, core, inner),
            CapabilityKind::View => repr_kind_for_ty(db, core, inner),
        };
    }

    if let Some(space) = effect_provider_space_for_ty(db, core, ty) {
        return ReprKind::Ptr(space);
    }

    if let Some(inner) = transparent_newtype_field_ty(db, ty) {
        return repr_kind_for_ty(db, core, inner);
    }

    if ty.is_array(db) || ty.is_tuple(db) {
        return ReprKind::Ref;
    }

    if ty
        .adt_ref(db)
        .is_some_and(|adt| matches!(adt, AdtRef::Struct(_) | AdtRef::Enum(_)))
    {
        return ReprKind::Ref;
    }

    ReprKind::Word
}

fn repr_kind_for_ref_inner<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    inner: TyId<'db>,
) -> ReprKind {
    match repr_kind_for_ty(db, core, inner) {
        ReprKind::Word | ReprKind::Zst | ReprKind::Ptr(_) => ReprKind::Word,
        ReprKind::Ref => ReprKind::Ptr(AddressSpaceKind::Memory),
    }
}

/// Returns the leaf type that should drive word conversion (`WordRepr::{from_word,to_word}`).
///
/// This peels transparent newtypes and view wrappers so `struct WrapU8 { inner: u8 }` and
/// `view u8` are treated like `u8` for the purposes of masking/sign-extension.
pub fn word_conversion_leaf_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
    let mut ty = ty;
    loop {
        if let Some((CapabilityKind::View, inner)) = ty.as_capability(db) {
            ty = inner;
            continue;
        }
        if let Some(inner) = transparent_newtype_field_ty(db, ty) {
            ty = inner;
            continue;
        }
        return ty;
    }
}

pub fn runtime_word_kind_for_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> RuntimeWordKind {
    let leaf_ty = word_conversion_leaf_ty(db, ty);
    if let TyData::TyBase(TyBase::Prim(prim)) = leaf_ty.base_ty(db).data(db) {
        return match prim {
            hir::analysis::ty::ty_def::PrimTy::Bool => RuntimeWordKind::I1,
            hir::analysis::ty::ty_def::PrimTy::U8 | hir::analysis::ty::ty_def::PrimTy::I8 => {
                RuntimeWordKind::I8
            }
            hir::analysis::ty::ty_def::PrimTy::U16 | hir::analysis::ty::ty_def::PrimTy::I16 => {
                RuntimeWordKind::I16
            }
            hir::analysis::ty::ty_def::PrimTy::U32 | hir::analysis::ty::ty_def::PrimTy::I32 => {
                RuntimeWordKind::I32
            }
            hir::analysis::ty::ty_def::PrimTy::U64 | hir::analysis::ty::ty_def::PrimTy::I64 => {
                RuntimeWordKind::I64
            }
            hir::analysis::ty::ty_def::PrimTy::U128 | hir::analysis::ty::ty_def::PrimTy::I128 => {
                RuntimeWordKind::I128
            }
            hir::analysis::ty::ty_def::PrimTy::U256
            | hir::analysis::ty::ty_def::PrimTy::I256
            | hir::analysis::ty::ty_def::PrimTy::Usize
            | hir::analysis::ty::ty_def::PrimTy::Isize
            | hir::analysis::ty::ty_def::PrimTy::String
            | hir::analysis::ty::ty_def::PrimTy::Array
            | hir::analysis::ty::ty_def::PrimTy::Tuple(_)
            | hir::analysis::ty::ty_def::PrimTy::Ptr
            | hir::analysis::ty::ty_def::PrimTy::View
            | hir::analysis::ty::ty_def::PrimTy::BorrowMut
            | hir::analysis::ty::ty_def::PrimTy::BorrowRef => RuntimeWordKind::I256,
        };
    }

    RuntimeWordKind::I256
}

pub fn runtime_shape_for_pointer_info<'db>(info: PointerInfo<'db>) -> RuntimeShape<'db> {
    if info.address_space == AddressSpaceKind::Memory {
        RuntimeShape::MemoryPtr {
            target_ty: info.target_ty,
        }
    } else {
        RuntimeShape::AddressWord(info)
    }
}

fn runtime_shape_needs_dynamic_address_space<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> bool {
    if let Some((capability, inner)) = ty.as_capability(db) {
        return match capability {
            CapabilityKind::Mut | CapabilityKind::Ref => true,
            CapabilityKind::View => runtime_shape_needs_dynamic_address_space(db, inner),
        };
    }

    transparent_newtype_field_ty(db, ty)
        .is_some_and(|inner| runtime_shape_needs_dynamic_address_space(db, inner))
}

pub fn runtime_shape_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
    default_ref_space: AddressSpaceKind,
) -> RuntimeShape<'db> {
    if layout::is_zero_sized_ty(db, ty) {
        return RuntimeShape::Erased;
    }

    if let Some(info) = runtime_pointer_info_for_ty(db, core, ty, default_ref_space) {
        return runtime_shape_for_pointer_info(info);
    }

    RuntimeShape::Word(runtime_word_kind_for_ty(db, ty))
}

pub fn runtime_return_shape_seed_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
) -> RuntimeShape<'db> {
    if layout::is_zero_sized_ty(db, ty) {
        return RuntimeShape::Erased;
    }

    if runtime_shape_needs_dynamic_address_space(db, ty) {
        return RuntimeShape::Unresolved;
    }

    runtime_shape_for_ty(db, core, ty, AddressSpaceKind::Memory)
}

pub fn runtime_shape_for_local<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    local: &LocalData<'db>,
) -> RuntimeShape<'db> {
    if let Some((_, info)) = local
        .pointer_leaf_infos
        .iter()
        .find(|(path, _)| path.is_empty())
    {
        return runtime_shape_for_pointer_info(*info);
    }

    runtime_shape_for_ty(db, core, local.ty, local.address_space)
}

pub fn runtime_shape_for_value<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    values: &[ValueData<'db>],
    locals: &[LocalData<'db>],
    value: ValueId,
) -> Option<RuntimeShape<'db>> {
    let value_data = values.get(value.index())?;
    if layout::is_zero_sized_ty(db, value_data.ty) {
        return Some(RuntimeShape::Erased);
    }

    match value_data.repr {
        crate::ir::ValueRepr::Word => Some(RuntimeShape::Word(runtime_word_kind_for_ty(
            db,
            value_data.ty,
        ))),
        crate::ir::ValueRepr::Ref(AddressSpaceKind::Memory) => Some(RuntimeShape::MemoryPtr {
            target_ty: try_value_pointer_info_in(values, locals, value)
                .and_then(|info| info.target_ty)
                .or(Some(value_data.ty)),
        }),
        crate::ir::ValueRepr::Ref(space) => Some(RuntimeShape::AddressWord(PointerInfo {
            address_space: space,
            target_ty: try_value_pointer_info_in(values, locals, value)
                .and_then(|info| info.target_ty)
                .or(Some(value_data.ty)),
        })),
        crate::ir::ValueRepr::Ptr(space) => {
            let info = try_value_pointer_info_in(values, locals, value)
                .or_else(|| runtime_value_pointer_info_for_ty(db, core, value_data.ty, space))
                .unwrap_or(PointerInfo {
                    address_space: space,
                    target_ty: None,
                });
            Some(runtime_shape_for_pointer_info(info))
        }
    }
}
