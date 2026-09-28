//! Field checks for the ABI records whose impls lowering generates: `msg`
//! variants, `#[error]` structs and `#[event]` data fields.
//!
//! One check decides, per field, whether the field type has what the record's
//! generated impls use. The record's field analysis reports the result, and the
//! generated metadata constants (`AssocConstBodyCheckPolicy::AbiRecordFields`)
//! are checked only when every field passes, so a field type without ABI
//! support is reported once, by one owner.

use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::abi_ty::{
    AbiTypeError, semantic_ty_to_abi_desc, semantic_ty_to_abi_desc_with_source,
};
use crate::analysis::ty::adt_def::{
    AdtRef, ConcreteTypeView, instantiate_adt_field_source_for_concrete_demand,
};
use crate::analysis::ty::corelib::{resolve_core_trait, resolve_lib_type_path};
use crate::analysis::ty::diagnostics::FuncBodyDiag;
use crate::analysis::ty::trait_def::TraitInstId;
use crate::analysis::ty::trait_resolution::{
    GoalSatisfiability, TraitSolveCx, is_goal_satisfiable,
};
use crate::analysis::ty::ty_def::TyId;
use crate::analysis::ty::ty_error::diag_from_invalid_cause;
use crate::hir_def::{AbiRecordKind, Struct};

/// An ABI problem with one field of a generated ABI record.
pub(crate) enum FieldAbiIssue<'db> {
    /// The field type has no Solidity ABI type. Checked for `msg` variants,
    /// whose ABI description lists each field's type.
    Unsupported { ty: TyId<'db>, reason: String },
    /// The field type lacks traits that the record's generated impls use.
    MissingTraits {
        ty: TyId<'db>,
        traits: Vec<&'static str>,
    },
    /// An invalid constant in the field type, reported as a type diagnostic.
    Ty(FuncBodyDiag<'db>),
}

/// Invalid field types and recursive ones are diagnosed by the analyses that
/// own them, so record field analysis reports nothing more for them.
pub(crate) fn field_abi_reported_elsewhere<'db>(
    db: &'db dyn HirAnalysisDb,
    field_ty: TyId<'db>,
) -> bool {
    field_ty.has_invalid(db)
        || matches!(
            semantic_ty_to_abi_desc(db, field_ty),
            Err(AbiTypeError::Recursive(_))
        )
}

/// The fields of `record` that its generated ABI impls cover, in declaration
/// order, with their indices among all of the record's fields.
fn abi_record_field_tys<'db>(
    db: &'db dyn HirAnalysisDb,
    record: Struct<'db>,
    kind: AbiRecordKind,
) -> Vec<(usize, TyId<'db>)> {
    let hir_fields = record.hir_fields(db).data(db);
    record
        .field_tys(db)
        .into_iter()
        .map(|ty| ty.instantiate_identity())
        .enumerate()
        .filter(|(idx, _)| {
            kind != AbiRecordKind::Event
                || !hir_fields
                    .get(*idx)
                    .is_some_and(|field| field.is_event_indexed)
        })
        .collect()
}

/// The ABI problems of the fields of `record`, a record of `kind`. Fields
/// whose problem another analysis owns are skipped
/// (`field_abi_reported_elsewhere`).
pub(crate) fn abi_record_field_issues<'db>(
    db: &'db dyn HirAnalysisDb,
    record: Struct<'db>,
    kind: AbiRecordKind,
) -> Vec<(usize, FieldAbiIssue<'db>)> {
    let (Some(sol_ty), Some(abi_size_trait), Some(encode_trait), Some(decode_trait)) = (
        resolve_lib_type_path(db, record.scope(), "std::abi::Sol"),
        resolve_core_trait(db, record.scope(), &["abi", "AbiSize"]),
        resolve_core_trait(db, record.scope(), &["abi", "Encode"]),
        resolve_core_trait(db, record.scope(), &["abi", "Decode"]),
    ) else {
        return Vec::new();
    };

    let solve_cx = TraitSolveCx::new(db, record.scope());
    let adt = AdtRef::from(record).as_adt(db);
    let mut issues = Vec::new();
    for (idx, field_ty) in abi_record_field_tys(db, record, kind) {
        if field_abi_reported_elsewhere(db, field_ty) {
            continue;
        }

        if kind == AbiRecordKind::MsgVariant {
            let source = instantiate_adt_field_source_for_concrete_demand(db, adt, 0, idx, &[]);
            match semantic_ty_to_abi_desc_with_source(db, ConcreteTypeView::new(field_ty, source)) {
                Err(AbiTypeError::Recursive(_)) => continue,
                Err(AbiTypeError::InvalidConst { cause, message }) => {
                    let span = record.span().fields().field(idx).ty().into();
                    let issue = match diag_from_invalid_cause(span, &cause) {
                        Some(diag) => FieldAbiIssue::Ty(diag.into()),
                        None => FieldAbiIssue::Unsupported {
                            ty: field_ty,
                            reason: message,
                        },
                    };
                    issues.push((idx, issue));
                    continue;
                }
                Err(AbiTypeError::Unsupported(reason)) => {
                    issues.push((
                        idx,
                        FieldAbiIssue::Unsupported {
                            ty: field_ty,
                            reason,
                        },
                    ));
                    continue;
                }
                Ok(_) => {}
            }
        }

        let mut requirements = vec![
            ("AbiSize", abi_size_trait, vec![field_ty]),
            ("Encode<Sol>", encode_trait, vec![field_ty, sol_ty]),
        ];
        if kind == AbiRecordKind::MsgVariant {
            requirements.push(("Decode<Sol>", decode_trait, vec![field_ty, sol_ty]));
        }
        let traits = requirements
            .into_iter()
            .filter(|(_, trait_, args)| {
                let goal = TraitInstId::new_simple(db, *trait_, args.clone());
                matches!(
                    is_goal_satisfiable(db, solve_cx, goal),
                    GoalSatisfiability::UnSat(_) | GoalSatisfiability::NeedsConfirmation { .. }
                )
            })
            .map(|(name, _, _)| name)
            .collect::<Vec<_>>();
        if !traits.is_empty() {
            issues.push((
                idx,
                FieldAbiIssue::MissingTraits {
                    ty: field_ty,
                    traits,
                },
            ));
        }
    }
    issues
}

/// Whether every field of `record` passes record field analysis, so that the
/// bodies of its generated ABI metadata constants report their own failures.
/// A field whose problem another analysis owns does not pass either: the
/// record is already rejected, and its metadata failures would repeat that.
pub(crate) fn abi_record_fields_pass<'db>(
    db: &'db dyn HirAnalysisDb,
    record: Struct<'db>,
    kind: AbiRecordKind,
) -> bool {
    abi_record_field_tys(db, record, kind)
        .into_iter()
        .all(|(_, field_ty)| !field_abi_reported_elsewhere(db, field_ty))
        && abi_record_field_issues(db, record, kind).is_empty()
}
