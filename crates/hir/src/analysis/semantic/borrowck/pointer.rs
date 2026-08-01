use rustc_hash::FxHashSet;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::VariantIndex,
        ty::{
            adt_def::{AdtRef, instantiate_adt_field_shape},
            ty_def::{TyData, TyId},
        },
    },
    projection::{IndexSource, Projection},
};
use common::ingot::IngotKind;

use super::ir::NSProjectionPath;

const POINTER_ARRAY_ENUMERATION_LIMIT: usize = 32;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct PointerSlot<'db> {
    pub(super) path: NSProjectionPath<'db>,
    pub(super) target_suffix: NSProjectionPath<'db>,
}

#[derive(Default)]
pub(super) struct PointerReachability<'db> {
    pub(super) targets: Vec<NSProjectionPath<'db>>,
    pub(super) writable_slots: Vec<NSProjectionPath<'db>>,
    pub(super) unbounded: bool,
}

pub(super) fn pointer_reachability<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    root_mutable: bool,
) -> PointerReachability<'db> {
    let mut out = PointerReachability::default();
    collect_pointer_reachability(
        db,
        strip_borrow(db, ty),
        NSProjectionPath::default(),
        root_mutable,
        &mut FxHashSet::default(),
        &mut out,
    );
    out
}

pub(super) fn raw_pointer_pointee_suffix<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<NSProjectionPath<'db>> {
    strip_borrow(db, ty)
        .as_ptr(db)
        .is_some()
        .then(|| NSProjectionPath::from_projection(Projection::Deref))
}

pub(super) fn mem_array_carrier_suffix<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<NSProjectionPath<'db>> {
    ty_is_core_mem_array(db, strip_borrow(db, ty)).then(|| {
        let mut path = NSProjectionPath::from_projection(Projection::Field(0));
        path.push(Projection::Deref);
        path
    })
}

pub(super) fn is_pointer_bearing_type<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    is_pointer_bearing_type_inner(db, strip_borrow(db, ty), &mut FxHashSet::default())
}

pub(super) fn pointer_slots<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Vec<PointerSlot<'db>> {
    let mut out = Vec::new();
    collect_pointer_slots(
        db,
        strip_borrow(db, ty),
        NSProjectionPath::default(),
        &mut FxHashSet::default(),
        &mut out,
    );
    out
}

pub(super) fn ty_is_core_mem_array<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    let Some(adt_def) = strip_borrow(db, ty).adt_def(db) else {
        return false;
    };
    let AdtRef::Struct(struct_) = adt_def.adt_ref(db) else {
        return false;
    };
    if struct_.top_mod(db).ingot(db).kind(db) != IngotKind::Core {
        return false;
    }
    let Some(path) = struct_.scope().pretty_path(db) else {
        return false;
    };
    path.split_once("::")
        .is_some_and(|(_, suffix)| suffix == "ptr::MemArray")
}

pub(super) fn path_with_projection<'db>(
    path: &NSProjectionPath<'db>,
    projection: Projection<
        TyId<'db>,
        crate::analysis::semantic::VariantIndex,
        crate::analysis::semantic::SLocalId,
    >,
) -> NSProjectionPath<'db> {
    let mut out = path.clone();
    out.push(projection);
    out
}

fn collect_pointer_slots<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    prefix: NSProjectionPath<'db>,
    seen: &mut FxHashSet<TyId<'db>>,
    out: &mut Vec<PointerSlot<'db>>,
) {
    let ty = strip_borrow(db, ty);
    if ty.as_ptr(db).is_some() {
        out.push(PointerSlot {
            path: prefix,
            target_suffix: NSProjectionPath::from_projection(Projection::Deref),
        });
        return;
    }

    if !seen.insert(ty) {
        return;
    }

    if let Some((elem_ty, len)) = array_parts(db, ty) {
        if len <= POINTER_ARRAY_ENUMERATION_LIMIT {
            for index in 0..len {
                collect_pointer_slots(
                    db,
                    elem_ty,
                    path_with_projection(&prefix, Projection::Index(IndexSource::Constant(index))),
                    seen,
                    out,
                );
            }
        } else {
            collect_pointer_slots(
                db,
                elem_ty,
                path_with_projection(&prefix, Projection::Index(IndexSource::Any)),
                seen,
                out,
            );
        }
        seen.remove(&ty);
        return;
    }

    if let Some(variants) = enum_variant_field_tys(db, ty) {
        for (variant, fields) in variants {
            for (field_idx, field_ty) in fields.into_iter().enumerate() {
                collect_pointer_slots(
                    db,
                    field_ty,
                    path_with_projection(
                        &prefix,
                        Projection::VariantField {
                            enum_ty: ty,
                            variant,
                            field_idx,
                        },
                    ),
                    seen,
                    out,
                );
            }
        }
        seen.remove(&ty);
        return;
    }

    for (field_idx, field_ty) in ty.field_types(db).into_iter().enumerate() {
        collect_pointer_slots(
            db,
            field_ty,
            path_with_projection(&prefix, Projection::Field(field_idx)),
            seen,
            out,
        );
    }
    seen.remove(&ty);
}

fn collect_pointer_reachability<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    base: NSProjectionPath<'db>,
    mutable: bool,
    seen: &mut FxHashSet<TyId<'db>>,
    out: &mut PointerReachability<'db>,
) {
    let ty = strip_borrow(db, ty);
    if matches!(
        ty.base_ty(db).data(db),
        TyData::TyVar(_) | TyData::TyParam(_) | TyData::AssocTy(_) | TyData::QualifiedTy(_)
    ) {
        out.unbounded = true;
        return;
    }
    if !seen.insert(ty) {
        out.unbounded = true;
        return;
    }
    let slots = pointer_slots(db, ty);
    if mutable {
        out.writable_slots
            .extend(slots.iter().map(|slot| base.concat(&slot.path)));
    }
    for slot in slots {
        let Some(pointer_ty) = slot.path.iter().try_fold(ty, |ty, projection| {
            projection_result_ty(db, ty, projection)
        }) else {
            continue;
        };
        let Some(pointee_ty) = strip_borrow(db, pointer_ty).as_ptr(db) else {
            continue;
        };
        let target = base.concat(&slot.path).concat(&slot.target_suffix);
        out.targets.push(target.clone());
        collect_pointer_reachability(db, pointee_ty, target, true, seen, out);
    }
    seen.remove(&ty);
}

fn is_pointer_bearing_type_inner<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    seen: &mut FxHashSet<TyId<'db>>,
) -> bool {
    let ty = strip_borrow(db, ty);
    if ty.as_ptr(db).is_some() || ty_is_core_mem_array(db, ty) {
        return true;
    }
    if !seen.insert(ty) {
        return false;
    }
    let fields_contain_pointer = ty
        .field_types(db)
        .into_iter()
        .any(|field_ty| is_pointer_bearing_type_inner(db, field_ty, seen));
    if fields_contain_pointer {
        seen.remove(&ty);
        return true;
    }
    let enum_contains_pointer = enum_variant_field_tys(db, ty).is_some_and(|variants| {
        variants.into_iter().any(|(_, fields)| {
            fields
                .into_iter()
                .any(|field_ty| is_pointer_bearing_type_inner(db, field_ty, seen))
        })
    });
    if enum_contains_pointer {
        seen.remove(&ty);
        return true;
    }
    let array_contains_pointer = array_parts(db, ty)
        .is_some_and(|(elem_ty, _)| is_pointer_bearing_type_inner(db, elem_ty, seen));
    seen.remove(&ty);
    array_contains_pointer
}

fn strip_borrow<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
    ty.as_capability(db).map_or(ty, |(_, inner)| inner)
}

fn array_parts<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<(TyId<'db>, usize)> {
    if !ty.is_array(db) {
        return None;
    }
    let (_, args) = ty.decompose_ty_app(db);
    let elem_ty = args.first().copied()?;
    let len = ty.array_len(db)?;
    Some((elem_ty, len))
}

fn enum_variant_field_tys<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<Vec<(VariantIndex, Vec<TyId<'db>>)>> {
    let adt_def = ty.adt_def(db)?;
    let AdtRef::Enum(enum_) = adt_def.adt_ref(db) else {
        return None;
    };
    let args = ty.generic_args(db);
    Some(
        enum_
            .variants(db)
            .enumerate()
            .map(|(variant_idx, _)| {
                let fields = adt_def.fields(db)[variant_idx]
                    .iter_types(db)
                    .enumerate()
                    .map(|(field_idx, _)| {
                        instantiate_adt_field_shape(db, adt_def, variant_idx, field_idx, args)
                    })
                    .collect();
                (VariantIndex(variant_idx as u16), fields)
            })
            .collect(),
    )
}

pub(super) fn projection_result_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    projection: &Projection<
        TyId<'db>,
        crate::analysis::semantic::VariantIndex,
        crate::analysis::semantic::SLocalId,
    >,
) -> Option<TyId<'db>> {
    match projection {
        Projection::Field(idx) => ty.field_types(db).get(*idx).copied(),
        Projection::VariantField {
            enum_ty,
            variant,
            field_idx,
        } => {
            let adt_def = enum_ty.adt_def(db)?;
            let args = enum_ty.generic_args(db);
            Some(instantiate_adt_field_shape(
                db,
                adt_def,
                variant.0 as usize,
                *field_idx,
                args,
            ))
        }
        Projection::Index(_) => array_parts(db, ty).map(|(elem_ty, _)| elem_ty),
        Projection::Deref => strip_borrow(db, ty).as_ptr(db),
        Projection::Discriminant => Some(TyId::u8(db)),
    }
}
