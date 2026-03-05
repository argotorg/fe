//! Type layout computation for Fe's memory model.
//!
//! This module provides the canonical source of truth for type sizes and field
//! offsets. Both MIR lowering and codegen should use these functions to ensure
//! consistent layout computation across the compiler.
//!
//! # Layout Models
//!
//! **Packed byte layout** (`ty_size_bytes_in`, `field_offset_bytes_in`):
//! - Primitives use their natural byte size (bool = 1, u8 = 1, u256 = 32)
//! - Structs/tuples pack fields contiguously
//! - Preserved for future packed storage and ABI-level size decisions
//!
//! **Hybrid memory layout** (`ty_memory_size_in`, `field_offset_memory_in`):
//! - Struct/tuple/enum fields stay word-padded for safe word loads/stores
//! - Fixed arrays of packed element types (`u8`, `bool`) use byte stride
//! - Required because EVM `mload`/`mstore` are word-oriented, while packed arrays
//!   rely on byte-oriented lowering (`mstore8` and byte extraction)
//!
//! **Storage slot layout** (`ty_size_slots`, `field_offset_slots`):
//! - Each primitive occupies one 256-bit slot regardless of byte size
//! - Enums have a 1-slot discriminant followed by payload

use hir::{
    analysis::{
        HirAnalysisDb,
        ty::{
            adt_def::AdtRef,
            const_ty::{ConstTyData, EvaluatedConstTy},
            normalize::normalize_ty,
            simplified_pattern::ConstructorKind,
            trait_resolution::PredicateListId,
            ty_def::{PrimTy, TyBase, TyData, TyId, prim_int_bits},
            visitor::{TyVisitable, TyVisitor, walk_ty},
        },
    },
    hir_def::{EnumVariant, scope_graph::ScopeId},
    projection::Projection,
};
use num_traits::ToPrimitive;

use crate::ir::{AddressSpaceKind, MirBody, Place};

#[derive(Clone, Copy, Debug)]
pub struct TargetDataLayout {
    pub word_size_bytes: usize,
    pub discriminant_size_bytes: usize,
}

impl TargetDataLayout {
    pub const fn evm() -> Self {
        Self {
            word_size_bytes: 32,
            discriminant_size_bytes: 32,
        }
    }
}

pub const EVM_LAYOUT: TargetDataLayout = TargetDataLayout::evm();

/// Size of an EVM word in bytes (256 bits).
pub const WORD_SIZE_BYTES: usize = EVM_LAYOUT.word_size_bytes;
pub const DISCRIMINANT_SIZE_BYTES: usize = EVM_LAYOUT.discriminant_size_bytes;

fn normalize_ty_for_layout<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<TyId<'db>> {
    if ty.has_invalid(db) || ty.has_param(db) || ty.has_var(db) {
        return None;
    }

    let scope = first_assoc_scope(db, ty)?;
    let normalized = normalize_ty(db, ty, scope, PredicateListId::empty_list(db));
    if normalized == ty || normalized.has_invalid(db) {
        return None;
    }
    Some(normalized)
}

fn first_assoc_scope<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<ScopeId<'db>> {
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        scope: Option<ScopeId<'db>>,
    }

    impl<'db> TyVisitor<'db> for Finder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.scope.is_some() {
                return;
            }
            if let TyData::AssocTy(assoc) = ty.data(self.db) {
                self.scope = assoc.scope(self.db);
                return;
            }
            walk_ty(self, ty);
        }
    }

    let mut finder = Finder { db, scope: None };
    ty.visit_with(&mut finder);
    finder.scope
}

/// Returns `true` when the type is known to have zero runtime size.
///
/// This is used to avoid emitting allocations, loads, and stores for types that
/// do not have a runtime representation.
pub fn is_zero_sized_ty(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> bool {
    is_zero_sized_ty_in(db, &EVM_LAYOUT, ty)
}

pub fn is_zero_sized_ty_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    ty: TyId<'_>,
) -> bool {
    if ty.is_never(db) {
        return true;
    }
    ty_size_bytes_in(db, layout, ty).is_some_and(|size| size == 0)
}

/// Computes the byte size of a type.
///
/// Returns `None` for unsupported/unsized types.
///
/// # Supported Types
/// - Primitives: bool (1), u8-u256/i8-i256 (1-32 bytes), pointers/strings (32 bytes)
/// - Tuples/structs: sum of field sizes
/// - Fixed-size arrays: `len * stride`
/// - Enums: 32-byte discriminant + max variant payload
pub fn ty_size_bytes(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<usize> {
    ty_size_bytes_in(db, &EVM_LAYOUT, ty)
}

pub fn ty_size_bytes_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    ty: TyId<'_>,
) -> Option<usize> {
    if let Some(normalized) = normalize_ty_for_layout(db, ty) {
        return ty_size_bytes_in(db, layout, normalized);
    }

    // Effect-related and trait-self type parameters are compile-time only and have no runtime
    // representation.
    if let TyData::TyParam(param) = ty.data(db)
        && (param.is_effect() || param.is_effect_provider() || param.is_trait_self())
    {
        return Some(0);
    }

    // Handle tuples first (check base type for TyApp cases)
    if ty.is_tuple(db) {
        let mut size = 0;
        for field_ty in ty.field_types(db) {
            size += ty_size_bytes_in(db, layout, field_ty)?;
        }
        return Some(size);
    }

    // Function items are compile-time only and have no runtime representation.
    if let TyData::TyBase(TyBase::Func(_)) = ty.base_ty(db).data(db) {
        return Some(0);
    }

    // Contract types are compile-time only and have no runtime representation.
    if let TyData::TyBase(TyBase::Contract(_)) = ty.base_ty(db).data(db) {
        return Some(0);
    }

    // Handle primitives
    if let TyData::TyBase(TyBase::Prim(prim)) = ty.base_ty(db).data(db) {
        if *prim == PrimTy::Bool {
            return Some(1);
        }
        if let Some(bits) = prim_int_bits(*prim) {
            return Some(bits / 8);
        }
        if matches!(prim, PrimTy::String | PrimTy::Ptr) {
            return Some(layout.word_size_bytes);
        }
    }

    // Handle fixed-size arrays
    if ty.is_array(db) {
        let len = array_len(db, ty)?;
        let stride = array_elem_stride_bytes_in(db, layout, ty)?;
        return Some(len * stride);
    }

    // Handle ADT types (structs) - use adt_def() which handles TyApp
    if let Some(adt_def) = ty.adt_def(db)
        && matches!(adt_def.adt_ref(db), AdtRef::Struct(_))
    {
        let mut size = 0;
        for field_ty in ty.field_types(db) {
            size += ty_size_bytes_in(db, layout, field_ty)?;
        }
        return Some(size);
    }

    // Handle enums: discriminant + max variant payload
    if let Some(adt_def) = ty.adt_def(db)
        && let AdtRef::Enum(enm) = adt_def.adt_ref(db)
    {
        let mut max_payload = 0;
        for variant in enm.variants(db) {
            let ev = EnumVariant::new(enm, variant.idx);
            let ctor = ConstructorKind::Variant(ev, ty);
            let mut payload = 0;
            for field_ty in ctor.field_types(db) {
                payload += ty_size_bytes_in(db, layout, field_ty)?;
            }
            max_payload = max_payload.max(payload);
        }
        return Some(layout.discriminant_size_bytes + max_payload);
    }

    None
}

/// Computes the byte size of a type, falling back to word alignment for unknown layouts.
///
/// This is useful when the compiler needs a conservative allocation size even when precise
/// layout information is unavailable (e.g. generic type parameters). Unknown leaf types are
/// treated as occupying a single 32-byte word.
///
/// Returns `None` for types that require a concrete size but cannot be computed yet (for example,
/// arrays with non-literal lengths).
pub fn ty_size_bytes_or_word_aligned(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<usize> {
    ty_size_bytes_or_word_aligned_in(db, &EVM_LAYOUT, ty)
}

pub fn ty_size_bytes_or_word_aligned_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    ty: TyId<'_>,
) -> Option<usize> {
    ty_size_bytes_in(db, layout, ty)
        .or_else(|| ty_size_bytes_word_aligned_fallback_in(db, layout, ty))
}

fn ty_size_bytes_word_aligned_fallback_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    ty: TyId<'_>,
) -> Option<usize> {
    if ty.is_tuple(db) {
        return ty
            .field_types(db)
            .iter()
            .copied()
            .try_fold(0usize, |acc, field_ty| {
                Some(acc + ty_size_bytes_or_word_aligned_in(db, layout, field_ty)?)
            });
    }

    if ty.is_array(db) {
        let len = array_len(db, ty)?;
        let stride = array_elem_stride_bytes_in(db, layout, ty).unwrap_or(layout.word_size_bytes);
        return Some(len * stride);
    }

    if let Some(adt_def) = ty.adt_def(db)
        && matches!(adt_def.adt_ref(db), AdtRef::Struct(_))
    {
        return ty
            .field_types(db)
            .iter()
            .copied()
            .try_fold(0usize, |acc, field_ty| {
                Some(acc + ty_size_bytes_or_word_aligned_in(db, layout, field_ty)?)
            });
    }

    if let Some(adt_def) = ty.adt_def(db)
        && let AdtRef::Enum(enm) = adt_def.adt_ref(db)
    {
        let mut max_payload = 0;
        for variant in enm.variants(db) {
            let ev = EnumVariant::new(enm, variant.idx);
            let ctor = ConstructorKind::Variant(ev, ty);
            let mut payload = 0;
            for field_ty in ctor.field_types(db) {
                payload += ty_size_bytes_or_word_aligned_in(db, layout, field_ty)?;
            }
            max_payload = max_payload.max(payload);
        }
        return Some(layout.discriminant_size_bytes + max_payload);
    }

    Some(layout.word_size_bytes)
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
    array_len_with_generic_args(db, ty, &[])
}

/// Returns the constant length for a fixed-size array, using generic arguments
/// to resolve const parameters when needed.
pub fn array_len_with_generic_args<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    generic_args: &[TyId<'db>],
) -> Option<usize> {
    let (base, args) = ty.decompose_ty_app(db);
    if !base.is_array(db) || args.len() < 2 {
        return None;
    }
    let len_ty = args[1];
    const_ty_to_usize_with_generic_args(db, len_ty, generic_args)
}

fn const_ty_to_usize_with_generic_args<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    generic_args: &[TyId<'db>],
) -> Option<usize> {
    let TyData::ConstTy(const_ty) = ty.data(db) else {
        return None;
    };
    match const_ty.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(value), _) => value.data(db).to_usize(),
        ConstTyData::TyParam(param, _) => {
            let arg = *generic_args.get(param.idx)?;
            if arg == ty {
                return None;
            }
            const_ty_to_usize_with_generic_args(db, arg, generic_args)
        }
        _ => None,
    }
}

/// Returns the byte stride for an array element, falling back to word alignment.
pub fn array_elem_stride_bytes(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<usize> {
    array_elem_stride_bytes_in(db, &EVM_LAYOUT, ty)
}

pub fn array_elem_stride_bytes_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    ty: TyId<'_>,
) -> Option<usize> {
    let elem_ty = array_elem_ty(db, ty)?;
    Some(ty_size_bytes_in(db, layout, elem_ty).unwrap_or(layout.word_size_bytes))
}

/// Returns the slot stride for an array element in storage.
pub fn array_elem_stride_slots(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<usize> {
    let elem_ty = array_elem_ty(db, ty)?;
    Some(ty_size_slots(db, elem_ty))
}

/// Best-effort slot size computation for types in storage.
pub fn ty_storage_slots<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<usize> {
    if let Some(normalized) = normalize_ty_for_layout(db, ty) {
        return ty_storage_slots(db, normalized);
    }

    // Handle tuples first (check base type for TyApp cases)
    if ty.is_tuple(db) {
        let mut size = 0;
        for field_ty in ty.field_types(db) {
            size += ty_storage_slots(db, field_ty)?;
        }
        return Some(size);
    }

    // Effect-related and trait-self type parameters are compile-time only.
    if let TyData::TyParam(param) = ty.data(db)
        && (param.is_effect() || param.is_effect_provider() || param.is_trait_self())
    {
        return Some(0);
    }

    // Function items are compile-time only and do not occupy storage.
    if let TyData::TyBase(TyBase::Func(_)) = ty.base_ty(db).data(db) {
        return Some(0);
    }

    // Contract types are compile-time only and do not occupy storage.
    if let TyData::TyBase(TyBase::Contract(_)) = ty.base_ty(db).data(db) {
        return Some(0);
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

    // Handle fixed-size arrays.
    if ty.is_array(db) {
        let len = array_len(db, ty)?;
        let elem_ty = array_elem_ty(db, ty)?;
        return Some(len * ty_storage_slots(db, elem_ty)?);
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
    field_offset_bytes_in(db, &EVM_LAYOUT, ty, field_idx)
}

pub fn field_offset_bytes_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    ty: TyId<'_>,
    field_idx: usize,
) -> Option<usize> {
    let field_types = ty.field_types(db);
    if field_idx >= field_types.len() {
        return None;
    }

    let mut offset = 0;
    for field_ty in field_types.iter().take(field_idx) {
        offset += ty_size_bytes_in(db, layout, *field_ty)?;
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
    field_offset_bytes_or_word_aligned_in(db, &EVM_LAYOUT, ty, field_idx)
}

pub fn field_offset_bytes_or_word_aligned_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    ty: TyId<'_>,
    field_idx: usize,
) -> usize {
    field_offset_bytes_in(db, layout, ty, field_idx).unwrap_or(layout.word_size_bytes * field_idx)
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
    variant_field_offset_bytes_in(db, &EVM_LAYOUT, enum_ty, variant, field_idx)
}

pub fn variant_field_offset_bytes_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
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
        offset += ty_size_bytes_in(db, layout, *field_ty)?;
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
    variant_field_offset_bytes_or_word_aligned_in(db, &EVM_LAYOUT, enum_ty, variant, field_idx)
}

pub fn variant_field_offset_bytes_or_word_aligned_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    enum_ty: TyId<'_>,
    variant: EnumVariant<'_>,
    field_idx: usize,
) -> usize {
    variant_field_offset_bytes_in(db, layout, enum_ty, variant, field_idx)
        .unwrap_or(layout.word_size_bytes * field_idx)
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
    variant_payload_size_bytes_in(db, &EVM_LAYOUT, enum_ty, variant)
}

pub fn variant_payload_size_bytes_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    enum_ty: TyId<'_>,
    variant: EnumVariant<'_>,
) -> Option<usize> {
    let ctor = ConstructorKind::Variant(variant, enum_ty);
    let field_types = ctor.field_types(db);

    let mut size = 0;
    for field_ty in field_types.iter() {
        size += ty_size_bytes_in(db, layout, *field_ty)?;
    }
    Some(size)
}

// ============================================================================
// Word-Padded Memory Layout
// ============================================================================
//
// EVM `mload`/`mstore` operate on 32-byte words. To avoid overlap bugs, struct-like
// field layout remains word-padded. Fixed arrays can still opt into packed element
// stride when lowering uses byte-oriented operations (`mstore8` + byte extraction).

/// Rounds `size` up to the nearest multiple of `word_size`. Returns 0 for zero-sized types.
fn round_up_to_word(size: usize, word_size: usize) -> usize {
    if size == 0 {
        return 0;
    }
    size.div_ceil(word_size) * word_size
}

/// Computes the allocation size for a type in memory.
///
/// Struct-like fields are word-padded. Packed arrays use byte stride for elements,
/// then round up the whole array allocation to a word for stable aggregate offsets.
pub fn ty_memory_size(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<usize> {
    ty_memory_size_in(db, &EVM_LAYOUT, ty)
}

pub fn ty_memory_size_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    ty: TyId<'_>,
) -> Option<usize> {
    if let Some(normalized) = normalize_ty_for_layout(db, ty) {
        return ty_memory_size_in(db, layout, normalized);
    }

    // Zero-sized compile-time-only types.
    if let TyData::TyParam(param) = ty.data(db)
        && (param.is_effect() || param.is_effect_provider() || param.is_trait_self())
    {
        return Some(0);
    }

    // Tuples: sum of word-padded field sizes.
    if ty.is_tuple(db) {
        let mut size = 0;
        for field_ty in ty.field_types(db) {
            size += ty_memory_size_in(db, layout, field_ty)?;
        }
        return Some(size);
    }

    if let TyData::TyBase(TyBase::Func(_)) = ty.base_ty(db).data(db) {
        return Some(0);
    }
    if let TyData::TyBase(TyBase::Contract(_)) = ty.base_ty(db).data(db) {
        return Some(0);
    }

    // Primitives: round up to word size.
    if let TyData::TyBase(TyBase::Prim(prim)) = ty.base_ty(db).data(db) {
        if *prim == PrimTy::Bool {
            return Some(layout.word_size_bytes);
        }
        if let Some(bits) = prim_int_bits(*prim) {
            return Some(round_up_to_word(bits / 8, layout.word_size_bytes));
        }
        if matches!(prim, PrimTy::String | PrimTy::Ptr) {
            return Some(layout.word_size_bytes);
        }
    }

    // Arrays: total size is word-padded.
    if ty.is_array(db) {
        let len = array_len(db, ty)?;
        let stride = array_elem_stride_memory_in(db, layout, ty)?;
        return Some(round_up_to_word(len * stride, layout.word_size_bytes));
    }

    // Structs: sum of word-padded field sizes.
    if let Some(adt_def) = ty.adt_def(db)
        && matches!(adt_def.adt_ref(db), AdtRef::Struct(_))
    {
        let mut size = 0;
        for field_ty in ty.field_types(db) {
            size += ty_memory_size_in(db, layout, field_ty)?;
        }
        return Some(size);
    }

    // Enums: word-padded discriminant + max word-padded variant payload.
    if let Some(adt_def) = ty.adt_def(db)
        && let AdtRef::Enum(enm) = adt_def.adt_ref(db)
    {
        let mut max_payload = 0;
        for variant in enm.variants(db) {
            let ev = EnumVariant::new(enm, variant.idx);
            let ctor = ConstructorKind::Variant(ev, ty);
            let mut payload = 0;
            for field_ty in ctor.field_types(db) {
                payload += ty_memory_size_in(db, layout, field_ty)?;
            }
            max_payload = max_payload.max(payload);
        }
        return Some(layout.discriminant_size_bytes + max_payload);
    }

    None
}

/// Like [`ty_memory_size`], but falls back to word size for unknown layouts.
pub fn ty_memory_size_or_word(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<usize> {
    ty_memory_size_or_word_in(db, &EVM_LAYOUT, ty)
}

pub fn ty_memory_size_or_word_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    ty: TyId<'_>,
) -> Option<usize> {
    ty_memory_size_in(db, layout, ty)
        .or_else(|| ty_size_bytes_word_aligned_fallback_in(db, layout, ty))
}

/// Computes the word-padded byte offset to a field for memory access.
///
/// Each preceding field contributes its word-padded size to the offset.
pub fn field_offset_memory(db: &dyn HirAnalysisDb, ty: TyId<'_>, field_idx: usize) -> usize {
    field_offset_memory_in(db, &EVM_LAYOUT, ty, field_idx)
}

pub fn field_offset_memory_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    ty: TyId<'_>,
    field_idx: usize,
) -> usize {
    let field_types = ty.field_types(db);
    if field_idx >= field_types.len() {
        // Fallback: assume each field is one word.
        return layout.word_size_bytes * field_idx;
    }

    let mut offset = 0;
    for field_ty in field_types.iter().take(field_idx) {
        offset += ty_memory_size_in(db, layout, *field_ty).unwrap_or(layout.word_size_bytes);
    }
    offset
}

/// Computes the word-padded byte offset to a field within an enum variant's
/// payload for memory access.
///
/// Offset is relative to the payload start (after the discriminant).
pub fn variant_field_offset_memory(
    db: &dyn HirAnalysisDb,
    enum_ty: TyId<'_>,
    variant: EnumVariant<'_>,
    field_idx: usize,
) -> usize {
    variant_field_offset_memory_in(db, &EVM_LAYOUT, enum_ty, variant, field_idx)
}

pub fn variant_field_offset_memory_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    enum_ty: TyId<'_>,
    variant: EnumVariant<'_>,
    field_idx: usize,
) -> usize {
    let ctor = ConstructorKind::Variant(variant, enum_ty);
    let field_types = ctor.field_types(db);

    if field_idx >= field_types.len() {
        return layout.word_size_bytes * field_idx;
    }

    let mut offset = 0;
    for field_ty in field_types.iter().take(field_idx) {
        offset += ty_memory_size_in(db, layout, *field_ty).unwrap_or(layout.word_size_bytes);
    }
    offset
}

/// Returns the memory stride for an array element.
///
/// Byte-sized logical elements (`u8`, `bool`) use packed stride (`1`).
pub fn array_elem_stride_memory(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<usize> {
    array_elem_stride_memory_in(db, &EVM_LAYOUT, ty)
}

pub fn array_elem_stride_memory_in(
    db: &dyn HirAnalysisDb,
    layout: &TargetDataLayout,
    ty: TyId<'_>,
) -> Option<usize> {
    let elem_ty = array_elem_ty(db, ty)?;
    if is_packed_memory_array_elem_ty(db, elem_ty) {
        return Some(1);
    }
    Some(ty_memory_size_in(db, layout, elem_ty).unwrap_or(layout.word_size_bytes))
}

/// Returns `true` if array elements of `elem_ty` should use packed byte stride in memory.
///
/// EVM currently packs only byte-sized logical elements in arrays.
pub fn is_packed_memory_array_elem_ty(db: &dyn HirAnalysisDb, elem_ty: TyId<'_>) -> bool {
    matches!(
        elem_ty.base_ty(db).data(db),
        TyData::TyBase(TyBase::Prim(PrimTy::U8 | PrimTy::Bool))
    )
}

pub fn ty_contains_packed_memory_array(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> bool {
    if ty.is_array(db) {
        if let Some(elem_ty) = array_elem_ty(db, ty) {
            return is_packed_memory_array_elem_ty(db, elem_ty)
                || ty_contains_packed_memory_array(db, elem_ty);
        }
        return false;
    }

    if ty.field_count(db) > 0 {
        return ty
            .field_types(db)
            .iter()
            .copied()
            .any(|field_ty| ty_contains_packed_memory_array(db, field_ty));
    }

    false
}

pub fn place_requires_packed_layout_arithmetic<'db>(
    db: &'db dyn HirAnalysisDb,
    base_ty: TyId<'db>,
    place: &Place<'db>,
) -> Result<bool, String> {
    let mut current_ty = base_ty;
    for proj in place.projection.iter() {
        match proj {
            Projection::Field(field_idx) => {
                let field_types = current_ty.field_types(db);
                if field_types
                    .iter()
                    .take(*field_idx)
                    .copied()
                    .any(|field_ty| ty_contains_packed_memory_array(db, field_ty))
                {
                    return Ok(true);
                }
                current_ty = *field_types
                    .get(*field_idx)
                    .ok_or_else(|| format!("projection: field {field_idx} out of bounds"))?;
            }
            Projection::VariantField {
                variant,
                enum_ty,
                field_idx,
            } => {
                let ctor = ConstructorKind::Variant(*variant, *enum_ty);
                let field_types = ctor.field_types(db);
                current_ty = *field_types.get(*field_idx).ok_or_else(|| {
                    format!("projection: variant field {field_idx} out of bounds")
                })?;
            }
            Projection::Discriminant => {
                current_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
            }
            Projection::Index(_) => {
                let elem_ty = array_elem_ty(db, current_ty)
                    .ok_or_else(|| "projection: array index on non-array type".to_string())?;
                if is_packed_memory_array_elem_ty(db, elem_ty)
                    || ty_contains_packed_memory_array(db, elem_ty)
                {
                    return Ok(true);
                }
                current_ty = elem_ty;
            }
            Projection::Deref => {
                return Err("projection: pointer dereference not implemented".to_string());
            }
        }
    }
    Ok(false)
}

pub fn is_packed_scalar_array_access<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &MirBody<'db>,
    place: &Place<'db>,
    scalar_ty: TyId<'db>,
) -> Result<bool, String> {
    let space = body.place_address_space(place);
    if !matches!(space, AddressSpaceKind::Memory | AddressSpaceKind::Code) {
        return Ok(false);
    }

    let scalar_ty = crate::repr::word_conversion_leaf_ty(db, scalar_ty);
    if !is_packed_memory_array_elem_ty(db, scalar_ty) {
        return Ok(false);
    }

    let base_ty = body
        .values
        .get(place.base.index())
        .ok_or_else(|| format!("unknown MIR place base value {}", place.base.index()))?
        .ty;
    let mut current_ty = base_ty;
    let Some(last_idx) = place.projection.len().checked_sub(1) else {
        return Ok(false);
    };

    for (idx, proj) in place.projection.iter().enumerate() {
        match proj {
            Projection::Field(field_idx) => {
                let field_types = current_ty.field_types(db);
                current_ty = *field_types
                    .get(*field_idx)
                    .ok_or_else(|| format!("projection: field {field_idx} out of bounds"))?;
            }
            Projection::VariantField {
                variant,
                enum_ty,
                field_idx,
            } => {
                let ctor = ConstructorKind::Variant(*variant, *enum_ty);
                let field_types = ctor.field_types(db);
                current_ty = *field_types.get(*field_idx).ok_or_else(|| {
                    format!("projection: variant field {field_idx} out of bounds")
                })?;
            }
            Projection::Discriminant => {
                current_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
            }
            Projection::Index(_) => {
                let elem_ty = array_elem_ty(db, current_ty)
                    .ok_or_else(|| "projection: array index on non-array type".to_string())?;
                if idx == last_idx && is_packed_memory_array_elem_ty(db, elem_ty) {
                    return Ok(true);
                }
                current_ty = elem_ty;
            }
            Projection::Deref => {
                return Err("projection: pointer dereference not implemented".to_string());
            }
        }
    }

    Ok(false)
}

// ============================================================================
// Storage Layout (Slot-Based)
// ============================================================================
//
// EVM storage uses 256-bit slots. Fe's storage model allocates one slot per
// primitive field, regardless of the primitive's byte size.

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
/// - Fixed-size arrays: `len * element_slots`
/// - Structs/tuples: sum of field slot counts
/// - Enums: 1-slot discriminant + max variant payload
/// - Unknown types: 1 slot (conservative fallback)
pub fn ty_size_slots(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> usize {
    ty_storage_slots(db, ty).unwrap_or(1)
}
