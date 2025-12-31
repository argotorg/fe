//! Runtime representation queries for MIR lowering and codegen.
//!
//! Fe exposes rich structural types (structs/tuples/arrays/enums), but MIR uses a smaller set of
//! runtime representation categories:
//! - word-like values (`ValueRepr::Word`)
//! - opaque pointers (`ValueRepr::Ptr`)
//! - by-reference aggregates (`ValueRepr::Ref`)
//!
//! This module centralizes the logic for computing those representation categories, including
//! recursive elimination of "newtype" structs (single-field structs). Newtypes are treated as
//! transparent wrappers: their runtime representation is the same as their (recursively unwrapped)
//! single field, while preserving the logical type (`TyId`) elsewhere in MIR.

use hir::analysis::HirAnalysisDb;
use hir::analysis::ty::adt_def::AdtRef;
use hir::analysis::ty::ty_def::{TyBase, TyData, TyId};

use crate::core_lib::{CoreHelperTy, CoreLib};
use crate::ir::{AddressSpaceKind, EffectProviderKind};
use crate::layout;

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

/// Returns the single field type if `ty` is a single-field `struct` ADT.
///
/// This intentionally does *not* look through nested wrappers. Callers that want to peel multiple
/// levels should loop over this helper.
pub fn transparent_newtype_field_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<TyId<'db>> {
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

/// Peel all transparent newtype layers from `ty`, returning the first non-newtype type.
pub fn peel_transparent_newtypes<'db>(db: &'db dyn HirAnalysisDb, mut ty: TyId<'db>) -> TyId<'db> {
    while let Some(inner) = transparent_newtype_field_ty(db, ty) {
        ty = inner;
    }
    ty
}

/// Returns the effect provider kind for a type, looking through transparent newtype wrappers.
pub fn effect_provider_kind_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    core: &CoreLib<'db>,
    ty: TyId<'db>,
) -> Option<EffectProviderKind> {
    let base_ty = ty.base_ty(db);

    if let Some(mem_base) = core
        .helper_ty(CoreHelperTy::EffectMemPtr)
        .map(|ty| ty.base_ty(db))
        && base_ty == mem_base
    {
        return Some(EffectProviderKind::Memory);
    }

    if let Some(stor_base) = core
        .helper_ty(CoreHelperTy::EffectStorPtr)
        .map(|ty| ty.base_ty(db))
        && base_ty == stor_base
    {
        return Some(EffectProviderKind::Storage);
    }

    if let Some(calldata_base) = core
        .helper_ty(CoreHelperTy::EffectCalldataPtr)
        .map(|ty| ty.base_ty(db))
        && base_ty == calldata_base
    {
        return Some(EffectProviderKind::Calldata);
    }

    transparent_newtype_field_ty(db, ty)
        .and_then(|inner| effect_provider_kind_for_ty(db, core, inner))
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

    if let Some(kind) = effect_provider_kind_for_ty(db, core, ty) {
        let space = match kind {
            EffectProviderKind::Memory => AddressSpaceKind::Memory,
            EffectProviderKind::Storage => AddressSpaceKind::Storage,
            EffectProviderKind::Calldata => AddressSpaceKind::Memory,
        };
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

/// Returns the leaf type that should drive word conversion (`WordRepr::{from_word,to_word}`).
///
/// This peels transparent newtypes so `struct WrapU8 { inner: u8 }` is treated like `u8` for the
/// purposes of masking/sign-extension.
pub fn word_conversion_leaf_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
    peel_transparent_newtypes(db, ty)
}
