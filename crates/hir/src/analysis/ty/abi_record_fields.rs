//! Field checks for the ABI records whose impls lowering generates: `msg`
//! variants, `#[error]` structs and `#[event]` data fields.
//!
//! One check decides, per field, whether the field type has what the record's
//! generated impls use. The record's field analysis reports the result, and the
//! generated metadata constants (`AssocConstBodyCheckPolicy::AbiRecordFields`)
//! are checked only when every field passes, so a field type without ABI
//! support is reported once, by one owner.

use crate::AbiFieldContext;
use crate::analysis::HirAnalysisDb;
use crate::analysis::diagnostics::DiagnosticVoucher;
use crate::analysis::ty::abi_ty::{
    AbiTypeError, semantic_ty_to_abi_desc, semantic_ty_to_abi_desc_with_source,
};
use crate::analysis::ty::adt_def::{
    AdtRef, ConcreteTypeView, instantiate_adt_field_source_for_concrete_demand,
};
use crate::analysis::ty::corelib::{resolve_core_trait, resolve_lib_type_path};
use crate::analysis::ty::diagnostics::FuncBodyDiag;
use crate::analysis::ty::trait_def::TraitInstId;
use crate::analysis::ty::trait_lower::lower_impl_trait;
use crate::analysis::ty::trait_resolution::{
    GoalSatisfiability, TraitSolveCx, is_goal_satisfiable,
};
use crate::analysis::ty::ty_def::TyId;
use crate::analysis::ty::ty_error::diag_from_invalid_cause;
use crate::hir_def::{AbiRecordKind, ImplTrait, Struct, TopLevelMod};
use crate::lower::top_mod_ast;
use crate::span::{DesugaredOrigin, HirOrigin};
use common::indexmap::IndexSet;
use parser::{
    TextRange,
    ast::{self, AstPtr, prelude::*},
};
use rustc_hash::FxHashSet;

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

/// A field of an `#[error]` struct or an `#[event]` data field whose type
/// cannot be used in the record's ABI. `msg` variants report theirs through
/// msg field analysis.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AbiRecordFieldDiag<'db> {
    pub context: AbiFieldContext,
    pub file: common::file::File,
    /// The field's type in the record declaration.
    pub primary_range: parser::TextRange,
    pub ty: TyId<'db>,
    /// The traits the field type lacks.
    pub traits: Vec<&'static str>,
}

/// Invalid field types and recursive ones are diagnosed by the analyses that
/// own them, and so is a tuple field of an `#[error]` or `#[event]` struct
/// (`AbiFieldDiagnostic`), so record field analysis reports nothing more for
/// them.
fn field_abi_reported_elsewhere<'db>(
    db: &'db dyn HirAnalysisDb,
    kind: AbiRecordKind,
    field_ty: TyId<'db>,
) -> bool {
    field_ty.has_invalid(db)
        || (kind != AbiRecordKind::MsgVariant && field_ty.is_tuple(db))
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
        if field_abi_reported_elsewhere(db, kind, field_ty) {
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
        .all(|(_, field_ty)| !field_abi_reported_elsewhere(db, kind, field_ty))
        && abi_record_field_issues(db, record, kind).is_empty()
}

/// The range of the declared type of field `idx` of the `#[event]` or
/// `#[error]` struct `record` in `top_mod`: of the field when it has no type,
/// and empty when the declaration cannot be found. Generated items carry the
/// whole declaration as their origin, so a field's own span resolves to the
/// struct; diagnostics about one field use this range instead.
pub(crate) fn abi_record_field_ty_range<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    record: &AstPtr<ast::Struct>,
    idx: usize,
) -> TextRange {
    let root = top_mod_ast(db, top_mod).syntax().clone();
    record
        .syntax_node_ptr()
        .try_to_node(&root)
        .and_then(ast::Struct::cast)
        .and_then(|record| record.fields())
        .and_then(|fields| fields.into_iter().nth(idx))
        .map_or_else(
            || TextRange::empty(0.into()),
            |field| {
                field
                    .ty()
                    .map_or(field.syntax().text_range(), |ty| ty.syntax().text_range())
            },
        )
}

/// The record, and its kind, whose ABI impl `impl_trait` lowering generated.
pub(crate) fn generated_abi_record<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Option<(Struct<'db>, AbiRecordKind)> {
    let kind = match impl_trait.origin(db) {
        HirOrigin::Desugared(DesugaredOrigin::Msg(_)) => AbiRecordKind::MsgVariant,
        HirOrigin::Desugared(DesugaredOrigin::Error(_)) => AbiRecordKind::Error,
        HirOrigin::Desugared(DesugaredOrigin::Event(_)) => AbiRecordKind::Event,
        _ => return None,
    };
    match lower_impl_trait(db, impl_trait)?.self_ty(db).adt_ref(db)? {
        AdtRef::Struct(record) => Some((record, kind)),
        _ => None,
    }
}

/// Record field analysis for the `#[error]` structs and `#[event]` structs of
/// `top_mod`: one diagnostic per field whose type cannot be used in the
/// record's ABI. Also returns the records whose fields do not pass, whose
/// generated function bodies are not checked, since their failures would
/// repeat these.
pub(crate) fn encoded_record_field_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> (
    Vec<Box<dyn DiagnosticVoucher + 'db>>,
    FxHashSet<Struct<'db>>,
) {
    let records = top_mod
        .all_impl_traits(db)
        .iter()
        .filter_map(|&impl_trait| {
            let (context, ast) = match impl_trait.origin(db) {
                HirOrigin::Desugared(DesugaredOrigin::Error(error)) => {
                    (AbiFieldContext::Error, error.error_struct.clone())
                }
                HirOrigin::Desugared(DesugaredOrigin::Event(event)) => {
                    (AbiFieldContext::Event, event.event_struct.clone())
                }
                _ => return None,
            };
            let (record, kind) = generated_abi_record(db, impl_trait)?;
            Some((record, kind, context, ast))
        })
        .collect::<IndexSet<_>>();

    let file = top_mod.file(db);
    let mut diags: Vec<Box<dyn DiagnosticVoucher + 'db>> = Vec::new();
    let mut failing = FxHashSet::default();
    for (record, kind, context, ast) in records {
        if !abi_record_fields_pass(db, record, kind) {
            failing.insert(record);
        }
        for (idx, issue) in abi_record_field_issues(db, record, kind) {
            match issue {
                FieldAbiIssue::MissingTraits { ty, traits } => {
                    diags.push(Box::new(AbiRecordFieldDiag {
                        context,
                        file,
                        primary_range: abi_record_field_ty_range(db, top_mod, &ast, idx),
                        ty,
                        traits,
                    }))
                }
                FieldAbiIssue::Unsupported { .. } => {
                    unreachable!("only msg variant fields are checked for a Solidity ABI type")
                }
                FieldAbiIssue::Ty(diag) => diags.push(diag.to_voucher()),
            }
        }
    }
    (diags, failing)
}
