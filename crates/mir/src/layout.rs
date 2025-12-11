//! Type layout computation for Fe's memory model.
//!
//! This module provides the canonical source of truth for type sizes and field
//! offsets. Both MIR lowering and codegen should use these functions to ensure
//! consistent layout computation across the compiler.
//!
//! # Memory Model
//!
//! Fe uses a packed byte layout (not Solidity's 32-byte slot per field):
//! - Primitives use their natural byte size (u8 = 1 byte, u256 = 32 bytes)
//! - Structs/tuples pack fields contiguously
//! - Enums have a 32-byte discriminant followed by payload fields

use hir::{
    analysis::{
        HirAnalysisDb,
        ty::{
            adt_def::AdtRef,
            const_ty::{ConstTyData, EvaluatedConstTy},
            simplified_pattern::ConstructorKind,
            ty_def::{PrimTy, TyBase, TyData, TyId, prim_int_bits},
        },
    },
    hir_def::EnumVariant,
};
use num_traits::ToPrimitive;

/// Size of an EVM word in bytes (256 bits).
pub const WORD_SIZE_BYTES: usize = 32;
pub const DISCRIMINANT_SIZE_BYTES: usize = WORD_SIZE_BYTES;

/// Computes the byte size of a type.
///
/// Returns `None` for unsupported types (unsized, unresolved, enums).
/// For enum sizes, use [`enum_size_bytes`] which handles discriminant + max payload.
///
/// # Supported Types
/// - Primitives: bool (1), u8-u256/i8-i256 (1-32 bytes)
/// - Tuples: sum of field sizes
/// - Structs: sum of field sizes
pub fn ty_size_bytes(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<usize> {
    // Handle tuples first (check base type for TyApp cases)
    if ty.is_tuple(db) {
        let mut size = 0;
        for field_ty in ty.field_types(db) {
            size += ty_size_bytes(db, field_ty)?;
        }
        return Some(size);
    }

    // Handle primitives
    if let TyData::TyBase(TyBase::Prim(prim)) = ty.base_ty(db).data(db) {
        if *prim == PrimTy::Bool {
            return Some(1);
        }
        if let Some(bits) = prim_int_bits(*prim) {
            return Some(bits / 8);
        }
    }

    // Handle ADT types (structs) - use adt_def() which handles TyApp
    if let Some(adt_def) = ty.adt_def(db)
        && matches!(adt_def.adt_ref(db), AdtRef::Struct(_))
    {
        let mut size = 0;
        for field_ty in ty.field_types(db) {
            size += ty_size_bytes(db, field_ty)?;
        }
        return Some(size);
    }

    None
}

/// Returns the element type for a fixed-size array.
pub fn array_elem_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<TyId<'db>> {
    let (base, args) = ty.decompose_ty_app(db);
    if !base.is_array(db) || args.is_empty() {
        return None;
    }
    Some(args[0])
}

/// Returns the constant length for a fixed-size array, if available.
pub fn array_len(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<usize> {
    let (base, args) = ty.decompose_ty_app(db);
    if !base.is_array(db) || args.len() < 2 {
        return None;
    }
    let len_ty = args[1];
    let TyData::ConstTy(const_ty) = len_ty.data(db) else {
        return None;
    };
    match const_ty.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(value), _) => value.data(db).to_usize(),
        _ => None,
    }
}

/// Returns the byte stride for an array element, falling back to word alignment.
pub fn array_elem_stride_bytes(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<usize> {
    let elem_ty = array_elem_ty(db, ty)?;
    Some(ty_size_bytes(db, elem_ty).unwrap_or(WORD_SIZE_BYTES))
}

/// Returns the slot stride for an array element in storage.
pub fn array_elem_stride_slots(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<usize> {
    let elem_ty = array_elem_ty(db, ty)?;
    Some(ty_size_slots(db, elem_ty))
}

/// Best-effort slot size computation for types in storage.
pub fn ty_storage_slots<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<usize> {
    // Handle tuples first (check base type for TyApp cases)
    if ty.is_tuple(db) {
        let mut size = 0;
        for field_ty in ty.field_types(db) {
            size += ty_storage_slots(db, field_ty)?;
        }
        return Some(size);
    }

    // Handle primitives
    if let TyData::TyBase(TyBase::Prim(prim)) = ty.base_ty(db).data(db)
        && matches!(
            prim,
            PrimTy::Bool
                | PrimTy::U8
                | PrimTy::U16
                | PrimTy::U32
                | PrimTy::U64
                | PrimTy::U128
                | PrimTy::U256
                | PrimTy::I8
                | PrimTy::I16
                | PrimTy::I32
                | PrimTy::I64
                | PrimTy::I128
                | PrimTy::I256
                | PrimTy::Usize
                | PrimTy::Isize
        )
    {
        return Some(1);
    }

    // Handle ADT types (structs) - use adt_def() which handles TyApp
    if let Some(adt_def) = ty.adt_def(db)
        && matches!(adt_def.adt_ref(db), AdtRef::Struct(_))
    {
        let mut size = 0;
        for field_ty in ty.field_types(db) {
            size += ty_storage_slots(db, field_ty)?;
        }
        return Some(size);
    }

    // Handle enums: discriminant (1 slot) + max variant payload
    if let Some(adt_def) = ty.adt_def(db)
        && let AdtRef::Enum(enm) = adt_def.adt_ref(db)
    {
        let mut max_payload = 0;
        for variant in enm.variants(db) {
            let ev = EnumVariant::new(enm, variant.idx);
            let ctor = ConstructorKind::Variant(ev, ty);
            let mut payload = 0;
            for field_ty in ctor.field_types(db) {
                payload += ty_storage_slots(db, field_ty)?;
            }
            max_payload = max_payload.max(payload);
        }
        return Some(1 + max_payload); // 1 slot discriminant
    }

    None
}

/// Computes the byte offset to a field within a struct or tuple.
///
/// The offset is the sum of sizes of all fields before `field_idx`.
///
/// # Returns
/// - `Some(offset)` if the type has fields and offset can be computed
/// - `None` if field_idx is out of bounds or type has no fields
pub fn field_offset_bytes(db: &dyn HirAnalysisDb, ty: TyId<'_>, field_idx: usize) -> Option<usize> {
    let field_types = ty.field_types(db);
    if field_idx >= field_types.len() {
        return None;
    }

    let mut offset = 0;
    for field_ty in field_types.iter().take(field_idx) {
        offset += ty_size_bytes(db, *field_ty)?;
    }
    Some(offset)
}

/// Like [`field_offset_bytes`], but falls back to word-aligned offset for unknown layouts.
///
/// Returns `field_idx * WORD_SIZE_BYTES` when the precise offset cannot be computed.
/// This matches Fe's EVM convention where unknown types occupy 32-byte slots.
pub fn field_offset_bytes_or_word_aligned(
    db: &dyn HirAnalysisDb,
    ty: TyId<'_>,
    field_idx: usize,
) -> usize {
    field_offset_bytes(db, ty, field_idx).unwrap_or(WORD_SIZE_BYTES * field_idx)
}

/// Computes the byte offset to a field within an enum variant's payload.
///
/// This is the offset **relative to the payload start** (i.e., after the
/// discriminant). To get the absolute offset from the enum base, add
/// `DISCRIMINANT_SIZE_BYTES`.
///
/// # Returns
/// - `Some(offset)` if the variant and field exist
/// - `None` if field_idx is out of bounds or variant has no fields
pub fn variant_field_offset_bytes(
    db: &dyn HirAnalysisDb,
    enum_ty: TyId<'_>,
    variant: EnumVariant<'_>,
    field_idx: usize,
) -> Option<usize> {
    let ctor = ConstructorKind::Variant(variant, enum_ty);
    let field_types = ctor.field_types(db);

    if field_idx >= field_types.len() {
        return None;
    }

    let mut offset = 0;
    for field_ty in field_types.iter().take(field_idx) {
        offset += ty_size_bytes(db, *field_ty)?;
    }
    Some(offset)
}

/// Like [`variant_field_offset_bytes`], but falls back to word-aligned offset for unknown layouts.
///
/// Returns `field_idx * WORD_SIZE_BYTES` when the precise offset cannot be computed.
pub fn variant_field_offset_bytes_or_word_aligned(
    db: &dyn HirAnalysisDb,
    enum_ty: TyId<'_>,
    variant: EnumVariant<'_>,
    field_idx: usize,
) -> usize {
    variant_field_offset_bytes(db, enum_ty, variant, field_idx)
        .unwrap_or(WORD_SIZE_BYTES * field_idx)
}

/// Computes the byte size of a variant's payload (sum of field sizes).
///
/// # Returns
/// - `Some(size)` if all field sizes can be computed
/// - `None` if any field has unknown size
pub fn variant_payload_size_bytes(
    db: &dyn HirAnalysisDb,
    enum_ty: TyId<'_>,
    variant: EnumVariant<'_>,
) -> Option<usize> {
    let ctor = ConstructorKind::Variant(variant, enum_ty);
    let field_types = ctor.field_types(db);

    let mut size = 0;
    for field_ty in field_types.iter() {
        size += ty_size_bytes(db, *field_ty)?;
    }
    Some(size)
}

// ============================================================================
// Storage Layout (Slot-Based)
// ============================================================================
//
// EVM storage uses 256-bit slots. Fe's storage model allocates one slot per
// primitive field, regardless of the primitive's byte size. This differs from
// memory layout which packs bytes contiguously.

/// Computes the slot offset to a field within a struct or tuple for storage.
///
/// In storage, each primitive field occupies one slot, so field N is at slot N.
/// For nested structs/tuples, we recursively count the total slots of preceding fields.
///
/// # Returns
/// - Slot offset for the field
pub fn field_offset_slots(db: &dyn HirAnalysisDb, ty: TyId<'_>, field_idx: usize) -> usize {
    let field_types = ty.field_types(db);
    if field_idx >= field_types.len() {
        return field_idx; // Fallback for out of bounds
    }

    let mut offset = 0;
    for field_ty in field_types.iter().take(field_idx) {
        offset += ty_size_slots(db, *field_ty);
    }
    offset
}

/// Computes the slot offset to a field within an enum variant's payload for storage.
///
/// This is the offset **relative to the payload start** (i.e., after the
/// discriminant slot). To get the absolute offset from the enum base, add 1
/// for the discriminant slot.
pub fn variant_field_offset_slots(
    db: &dyn HirAnalysisDb,
    enum_ty: TyId<'_>,
    variant: EnumVariant<'_>,
    field_idx: usize,
) -> usize {
    let ctor = ConstructorKind::Variant(variant, enum_ty);
    let field_types = ctor.field_types(db);

    if field_idx >= field_types.len() {
        return field_idx; // Fallback for out of bounds
    }

    let mut offset = 0;
    for field_ty in field_types.iter().take(field_idx) {
        offset += ty_size_slots(db, *field_ty);
    }
    offset
}

/// Computes the number of storage slots a type occupies.
///
/// - Primitives: 1 slot each (regardless of byte size)
/// - Structs/tuples: sum of field slot counts
/// - Unknown types: 1 slot (conservative fallback)
pub fn ty_size_slots(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> usize {
    // Handle tuples
    if ty.is_tuple(db) {
        let mut size = 0;
        for field_ty in ty.field_types(db) {
            size += ty_size_slots(db, field_ty);
        }
        return size;
    }

    // Handle primitives - each primitive takes 1 slot
    if let TyData::TyBase(TyBase::Prim(prim)) = ty.base_ty(db).data(db)
        && (*prim == PrimTy::Bool || prim_int_bits(*prim).is_some())
    {
        return 1;
    }

    // Handle ADT types (structs)
    if let Some(adt_def) = ty.adt_def(db)
        && matches!(adt_def.adt_ref(db), AdtRef::Struct(_))
    {
        let mut size = 0;
        for field_ty in ty.field_types(db) {
            size += ty_size_slots(db, field_ty);
        }
        return size;
    }

    // Unknown types default to 1 slot
    1
}
