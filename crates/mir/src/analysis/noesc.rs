//! MIR validation for NoEsc (stack-only) values.
//!
//! NoEsc values (e.g. `mut T` / `ref T`) may live in locals/temps, but must not be stored into
//! non-memory address spaces such as storage.

use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use hir::analysis::{HirAnalysisDb, ty::ty_is_noesc};

use crate::{
    MirFunction, MirInst, ValueId,
    ir::SourceInfoId,
    ir::{AddressSpaceKind, Place},
};

pub fn check_noesc_escapes<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
) -> Option<CompleteDiagnostic> {
    for block in &func.body.blocks {
        for inst in &block.insts {
            match inst {
                MirInst::Store {
                    source,
                    place,
                    value,
                    ..
                } => {
                    if let Some(err) = check_store(db, func, *source, place, *value) {
                        return Some(err);
                    }
                }
                MirInst::InitAggregate {
                    source,
                    place,
                    inits,
                    ..
                } => {
                    for (_, value) in inits {
                        if let Some(err) = check_store(db, func, *source, place, *value) {
                            return Some(err);
                        }
                    }
                }
                _ => {}
            }
        }
    }
    None
}

fn check_store<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    source: SourceInfoId,
    place: &Place<'db>,
    value: ValueId,
) -> Option<CompleteDiagnostic> {
    let space = func.body.place_address_space(place);
    if matches!(space, AddressSpaceKind::Memory) {
        return None;
    }

    let ty = func.body.value(value).ty;
    if !ty_is_noesc(db, ty) {
        return None;
    }

    let span = func
        .body
        .source_span(source)
        .or_else(|| {
            func.body
                .source_infos
                .iter()
                .find_map(|info| info.span.clone())
        })
        .expect("NoEsc diagnostic missing a span");
    Some(CompleteDiagnostic::new(
        Severity::Error,
        format!(
            "cannot store NoEsc value `{}` into `{space:?}`",
            ty.pretty_print(db)
        ),
        vec![SubDiagnostic::new(
            LabelStyle::Primary,
            "NoEsc value escapes here".to_string(),
            Some(span),
        )],
        Vec::new(),
        GlobalErrorCode::new(DiagnosticPass::Mir, 1),
    ))
}
