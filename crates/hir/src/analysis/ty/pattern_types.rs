use crate::{analysis::HirAnalysisDb, core::hir_def::EnumVariant};

use super::{
    adt_def::instantiate_adt_field_shape,
    ty_def::{BorrowKind, CapabilityKind, InvalidCause, TyId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatternDestructureMode {
    Owned,
    Borrow(BorrowKind),
}

#[derive(Debug, Clone, Copy)]
pub enum PatternProjectionStep<'db> {
    Field(usize),
    VariantField {
        variant: EnumVariant<'db>,
        field_idx: usize,
    },
}

pub fn destructure_pattern_source<'db>(
    db: &'db dyn HirAnalysisDb,
    source_ty: TyId<'db>,
) -> (TyId<'db>, PatternDestructureMode) {
    if let Some((kind, inner)) = source_ty.as_capability(db) {
        let borrow_kind = match kind {
            CapabilityKind::Mut => BorrowKind::Mut,
            CapabilityKind::Ref | CapabilityKind::View => BorrowKind::Ref,
        };
        (inner, PatternDestructureMode::Borrow(borrow_kind))
    } else {
        (source_ty, PatternDestructureMode::Owned)
    }
}

pub fn pattern_match_expected_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    source_ty: TyId<'db>,
) -> TyId<'db> {
    destructure_pattern_source(db, source_ty).0
}

pub fn apply_pattern_borrow_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    mode: PatternDestructureMode,
    child_source_ty: TyId<'db>,
) -> TyId<'db> {
    match mode {
        PatternDestructureMode::Owned => child_source_ty,
        PatternDestructureMode::Borrow(_) if child_source_ty.as_capability(db).is_some() => {
            child_source_ty
        }
        PatternDestructureMode::Borrow(BorrowKind::Mut) => TyId::borrow_mut_of(db, child_source_ty),
        PatternDestructureMode::Borrow(BorrowKind::Ref) => TyId::borrow_ref_of(db, child_source_ty),
    }
}

pub fn project_pattern_child_source_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    parent_match_container_ty: TyId<'db>,
    projection: PatternProjectionStep<'db>,
) -> TyId<'db> {
    match projection {
        PatternProjectionStep::Field(field_idx) => parent_match_container_ty
            .field_types(db)
            .get(field_idx)
            .copied()
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other)),
        PatternProjectionStep::VariantField { variant, field_idx } => parent_match_container_ty
            .adt_def(db)
            .filter(|adt_def| (variant.idx as usize) < adt_def.fields(db).len())
            .and_then(|adt_def| {
                adt_def
                    .fields(db)
                    .get(variant.idx as usize)
                    .filter(|fields| field_idx < fields.num_types())
                    .map(|_| {
                        instantiate_adt_field_shape(
                            db,
                            adt_def,
                            variant.idx as usize,
                            field_idx,
                            parent_match_container_ty.generic_args(db),
                        )
                    })
            })
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other)),
    }
}

pub fn project_pattern_child_carrier_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    parent_carrier_ty: TyId<'db>,
    projection: PatternProjectionStep<'db>,
) -> TyId<'db> {
    let (container_match_ty, mode) = destructure_pattern_source(db, parent_carrier_ty);
    let child_source_ty = project_pattern_child_source_ty(db, container_match_ty, projection);
    apply_pattern_borrow_mode(db, mode, child_source_ty)
}
