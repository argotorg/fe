//! MIR validation for stack-only values.
//!
//! Stack-only values (e.g. `mut T` / `ref T`) may live in locals/temps, but must not be stored
//! into non-memory address spaces such as storage.

use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use hir::analysis::{
    HirAnalysisDb,
    ty::{ty_is_borrow, ty_is_noesc},
};

use crate::{
    CallOrigin, MirFunction, MirInst, ValueId,
    ir::SourceInfoId,
    ir::{AddressSpaceKind, Place, Rvalue, TerminatingCall, Terminator, ValueOrigin},
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
                MirInst::Assign {
                    source,
                    rvalue: Rvalue::Call(call),
                    ..
                } => {
                    if let Some(err) = check_call_args(db, func, *source, call) {
                        return Some(err);
                    }
                }
                _ => {}
            }
        }

        if let Terminator::TerminatingCall {
            source,
            call: TerminatingCall::Call(call),
        } = &block.terminator
            && let Some(err) = check_call_args(db, func, *source, call)
        {
            return Some(err);
        }
    }
    None
}

fn diagnostic_span<'db>(
    func: &MirFunction<'db>,
    source: SourceInfoId,
) -> common::diagnostics::Span {
    func.body
        .source_span(source)
        .or_else(|| {
            func.body
                .source_infos
                .iter()
                .find_map(|info| info.span.clone())
        })
        .expect("escape diagnostic missing a span")
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

    let span = diagnostic_span(func, source);

    if matches!(space, AddressSpaceKind::Calldata) {
        return Some(CompleteDiagnostic::new(
            Severity::Error,
            "cannot write to `Calldata`".to_string(),
            vec![SubDiagnostic::new(
                LabelStyle::Primary,
                "`Calldata` is read-only".to_string(),
                Some(span),
            )],
            vec!["note: writes must target `Memory`, `Storage`, or `TransientStorage`".to_string()],
            GlobalErrorCode::new(DiagnosticPass::Mir, 1),
        ));
    }

    let ty = func.body.value(value).ty;
    if !ty_is_noesc(db, ty) {
        return None;
    }

    let reason = if ty_is_borrow(db, ty).is_some() {
        "note: borrow handles (`mut`/`ref`) cannot be stored".to_string()
    } else {
        "note: this value contains a borrow handle (`mut`/`ref`)".to_string()
    };

    Some(CompleteDiagnostic::new(
        Severity::Error,
        format!("cannot store `{}` in `{space:?}`", ty.pretty_print(db)),
        vec![SubDiagnostic::new(
            LabelStyle::Primary,
            format!("this value cannot be written to `{space:?}`"),
            Some(span),
        )],
        vec![reason],
        GlobalErrorCode::new(DiagnosticPass::Mir, 1),
    ))
}

fn check_call_args<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    source: SourceInfoId,
    call: &CallOrigin<'db>,
) -> Option<CompleteDiagnostic> {
    let receiver_arg_count = call
        .hir_target
        .as_ref()
        .and_then(|target| target.callable_def.receiver_ty(db))
        .map(|_| 1usize)
        .unwrap_or(0);

    for (arg_idx, arg) in call
        .args
        .iter()
        .copied()
        .enumerate()
        .skip(receiver_arg_count)
    {
        if let Some(err) = check_call_arg(db, func, source, arg_idx + 1 - receiver_arg_count, arg) {
            return Some(err);
        }
    }
    None
}

fn check_call_arg<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    source: SourceInfoId,
    arg_idx: usize,
    arg: ValueId,
) -> Option<CompleteDiagnostic> {
    let ty = func.body.value(arg).ty;
    if ty.as_borrow(db).is_none() {
        return None;
    }

    let Some(space) = non_memory_borrow_origin_space(func, arg) else {
        return None;
    };

    let span = diagnostic_span(func, source);
    Some(CompleteDiagnostic::new(
        Severity::Error,
        format!(
            "cannot pass `{}` from `{space:?}` as function argument",
            ty.pretty_print(db)
        ),
        vec![SubDiagnostic::new(
            LabelStyle::Primary,
            format!("argument {arg_idx} is a `{space:?}` borrow handle"),
            Some(span),
        )],
        vec![
            "note: non-memory borrow handles (`mut`/`ref`) cannot be passed as regular function arguments"
                .to_string(),
        ],
        GlobalErrorCode::new(DiagnosticPass::Mir, 1),
    ))
}

fn non_memory_borrow_origin_space<'db>(
    func: &MirFunction<'db>,
    value: ValueId,
) -> Option<AddressSpaceKind> {
    fn visit<'db>(func: &MirFunction<'db>, value: ValueId) -> Option<AddressSpaceKind> {
        match &func.body.value(value).origin {
            ValueOrigin::TransparentCast { value } => visit(func, *value),
            ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
                let space = func.body.place_address_space(place);
                (!matches!(space, AddressSpaceKind::Memory)).then_some(space)
            }
            _ => None,
        }
    }

    visit(func, value)
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::analysis::ty::ty_def::{PrimTy, TyBase, TyData, TyId};
    use url::Url;

    use crate::{
        MirInst, analysis::noesc::check_noesc_escapes, ir::AddressSpaceKind, ir::BasicBlock,
        ir::LocalData, ir::Place, ir::SourceInfoId, ir::Terminator, ir::ValueData, ir::ValueOrigin,
        ir::ValueRepr,
    };

    #[test]
    fn store_to_calldata_is_rejected_even_for_esc_values() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///noesc_store_to_calldata.fe").unwrap();
        let src = "pub fn noesc_store_to_calldata() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "noesc_store_to_calldata")
            .expect("function should exist");

        let u256_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        func.body.locals.clear();
        func.body.values.clear();
        func.body.blocks.clear();
        let base_local = func.body.alloc_local(LocalData {
            name: "cd".to_string(),
            ty: u256_ty,
            is_mut: false,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Calldata,
            capability_spaces: Vec::new(),
        });
        let base_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(base_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ref(AddressSpaceKind::Calldata),
        });
        let store_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Synthetic(crate::ir::SyntheticValue::Int(1u8.into())),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
        });
        func.body.blocks.push(BasicBlock {
            insts: vec![MirInst::Store {
                source: SourceInfoId::SYNTHETIC,
                place: Place::new(base_value, crate::MirProjectionPath::new()),
                value: store_value,
            }],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: None,
            },
        });

        let diag = check_noesc_escapes(&db, func);
        assert!(
            diag.is_some(),
            "store to calldata should be rejected before codegen"
        );
    }
}
