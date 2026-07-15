use rustc_hash::FxHashSet;

use crate::{
    db::MirDb,
    instance::RuntimeInstance,
    runtime::{
        RExpr, RStmt, RTerminator, RuntimeBody, RuntimeBuiltin, RuntimeCarrier, RuntimeClass,
        RuntimeExitBehavior, RuntimeInterfaceSignature, RuntimeLocalRoot, RuntimeProgramView,
        ScalarClass, ScalarRepr, ScalarRole,
        class::{expr_result_class, verify_address_operand, verify_word_value},
    },
    verify::VerifyError,
};

use super::{
    RuntimeVerifyFailure, RuntimeVerifySite,
    layout::verify_class_layouts,
    place::{
        runtime_value_class, scalar_class_from_const, verify_enum_handle,
        verify_enum_write_variant, verify_value_enum_variant_ref,
    },
};

pub fn verify_runtime_body<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &RuntimeBody<'db>,
) -> Result<(), VerifyError<'db>> {
    verify_runtime_body_detailed(db, program, body).map_err(|failure| failure.error)
}

pub fn verify_runtime_body_detailed<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &RuntimeBody<'db>,
) -> Result<(), RuntimeVerifyFailure<'db>> {
    verify_signature(body).map_err(|error| RuntimeVerifyFailure {
        error,
        site: RuntimeVerifySite::Body,
    })?;

    let mut visited_layouts = FxHashSet::default();
    for (idx, local) in body.locals.iter().enumerate() {
        let local_id = crate::runtime::RLocalId::from_u32(idx as u32);
        match &local.root {
            RuntimeLocalRoot::None => {}
            RuntimeLocalRoot::Slot(class) | RuntimeLocalRoot::Ref(class) => {
                verify_class_layouts(db, program, class, &mut visited_layouts).map_err(
                    |error| RuntimeVerifyFailure {
                        error,
                        site: RuntimeVerifySite::LocalRoot(local_id),
                    },
                )?;
            }
            RuntimeLocalRoot::Ptr { class, .. } => {
                verify_class_layouts(db, program, class, &mut visited_layouts).map_err(
                    |error| RuntimeVerifyFailure {
                        error,
                        site: RuntimeVerifySite::LocalRoot(local_id),
                    },
                )?;
            }
        }

        if let RuntimeCarrier::Value(class) = &local.carrier {
            verify_class_layouts(db, program, class, &mut visited_layouts).map_err(|error| {
                RuntimeVerifyFailure {
                    error,
                    site: RuntimeVerifySite::LocalCarrier(local_id),
                }
            })?;
        }
    }

    for (block_idx, block) in body.blocks.iter().enumerate() {
        let block_id = crate::runtime::RBlockId::from_u32(block_idx as u32);
        for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
            verify_stmt(db, program, body, block, stmt_idx, stmt).map_err(|error| {
                RuntimeVerifyFailure {
                    error,
                    site: RuntimeVerifySite::Stmt {
                        block: block_id,
                        stmt: stmt_idx,
                    },
                }
            })?;
        }

        verify_terminator(db, program, body, &block.terminator).map_err(|error| {
            RuntimeVerifyFailure {
                error,
                site: RuntimeVerifySite::Terminator { block: block_id },
            }
        })?;
    }

    Ok(())
}

fn verify_stmt<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &RuntimeBody<'db>,
    block: &crate::runtime::RBlock<'db>,
    stmt_idx: usize,
    stmt: &RStmt<'db>,
) -> Result<(), VerifyError<'db>> {
    match stmt {
        RStmt::Assign { dst, expr } => {
            verify_assign(db, program, body, block, stmt_idx, *dst, expr)
        }
        RStmt::AssertIndexInBounds { index, .. } => {
            if let hir::projection::IndexSource::Dynamic(index) = index
                && !matches!(
                    runtime_value_class(body, *index)?,
                    RuntimeClass::Scalar(crate::runtime::ScalarClass {
                        repr: crate::runtime::ScalarRepr::Int { signed: false, .. },
                        role: crate::runtime::ScalarRole::Plain,
                    })
                )
            {
                return Err(VerifyError::InvalidIndexClass(*index));
            }
            Ok(())
        }
        RStmt::EnumAssertVariant { value, variant } => {
            verify_value_enum_variant_ref(
                program,
                runtime_value_class(body, *value)?.clone(),
                *variant,
            )?;
            Ok(())
        }
        RStmt::Store { dst, src } => verify_store(db, program, body, dst, *src),
        RStmt::CopyInto { dst, src } => verify_copy_into(db, program, body, dst, *src),
        RStmt::EnumSetTag { root, variant } => {
            verify_enum_handle(body, *root, *variant, program).map(|_| ())
        }
        RStmt::EnumWriteVariant {
            root,
            variant,
            fields,
        } => verify_enum_write_variant(program, body, *root, *variant, fields),
    }
}

fn verify_signature<'db>(body: &RuntimeBody<'db>) -> Result<(), VerifyError<'db>> {
    for param in &body.signature.params {
        let local = body
            .local(param.local)
            .ok_or(VerifyError::MissingRuntimeLocal(param.local))?;
        if local.carrier != RuntimeCarrier::Value(param.class.clone()) {
            return Err(VerifyError::SlotCarrierMismatch(param.local));
        }
    }

    Ok(())
}

fn verify_call<'db>(
    _db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &RuntimeBody<'db>,
    callee: RuntimeInstance<'db>,
    args: &[crate::runtime::RValueId],
    kind: RuntimeCallKind,
) -> Result<(), VerifyError<'db>> {
    let RuntimeInterfaceSignature { params, .. } = program.interface_signature(callee);
    if kind == RuntimeCallKind::Terminal
        && program.exit_behavior(callee) != RuntimeExitBehavior::NeverReturns
    {
        return Err(VerifyError::InvalidTerminalCall(callee));
    }
    if params.len() != args.len() {
        return Err(VerifyError::CallArgCountMismatch(callee));
    }

    for (idx, (arg, param)) in args.iter().zip(params.iter()).enumerate() {
        let Some(class) = body.value_class(*arg) else {
            return Err(VerifyError::ErasedRuntimeValue(*arg));
        };
        if class != &param.class {
            return Err(VerifyError::CallArgClassMismatch(callee, idx));
        }
    }

    Ok(())
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RuntimeCallKind {
    Normal,
    Terminal,
}

fn verify_assign<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &RuntimeBody<'db>,
    block: &crate::runtime::RBlock<'db>,
    stmt_idx: usize,
    dst: crate::runtime::RLocalId,
    expr: &RExpr<'db>,
) -> Result<(), VerifyError<'db>> {
    let local = body
        .local(dst)
        .ok_or(VerifyError::MissingRuntimeLocal(dst))?;
    let dst_class = match &local.carrier {
        RuntimeCarrier::Erased => None,
        RuntimeCarrier::Value(class) => Some(class.clone()),
    };

    if let RExpr::Call { callee, args } = expr {
        verify_call(db, program, body, *callee, args, RuntimeCallKind::Normal)?;
    }

    let expr_class = expr_result_class(db, program, body, dst, dst_class.clone(), expr)?;

    if let RExpr::EnumExtract { value, variant, .. } = expr
        && !same_block_dominating_enum_assert(block, stmt_idx, *value, *variant)
    {
        return Err(VerifyError::MissingEnumVariantProof(*value));
    }

    if let Some(dst_class) = &dst_class
        && !expr_class
            .as_ref()
            .is_some_and(|expr_class| expr_class.shares_runtime_rep_with(db, dst_class))
    {
        return Err(VerifyError::InvalidExprClass(dst));
    }
    if dst_class.is_none()
        && matches!(
            expr,
            RExpr::Call { .. }
                | RExpr::Builtin(
                    RuntimeBuiltin::CallDataCopy { .. } | RuntimeBuiltin::CodeCopy { .. }
                )
        )
    {
        return Ok(());
    }
    if dst_class.is_none() && expr_class.is_some() {
        return Err(VerifyError::InvalidExprClass(dst));
    }
    Ok(())
}

fn same_block_dominating_enum_assert<'db>(
    block: &crate::runtime::RBlock<'db>,
    stmt_idx: usize,
    value: crate::runtime::RValueId,
    variant: crate::runtime::VariantId<'db>,
) -> bool {
    let mut proven = false;
    for stmt in block.stmts.iter().take(stmt_idx) {
        match stmt {
            RStmt::Assign { dst, expr } if *dst == value => {
                proven = matches!(
                    expr,
                    RExpr::EnumMake {
                        variant: proven_variant,
                        ..
                    } if *proven_variant == variant
                );
            }
            RStmt::EnumAssertVariant {
                value: proven_value,
                variant: proven_variant,
            } if *proven_value == value => {
                proven = *proven_variant == variant;
            }
            RStmt::EnumAssertVariant { .. }
            | RStmt::Assign { .. }
            | RStmt::AssertIndexInBounds { .. }
            | RStmt::Store { .. }
            | RStmt::CopyInto { .. }
            | RStmt::EnumSetTag { .. }
            | RStmt::EnumWriteVariant { .. } => {}
        }
    }
    proven
}

fn verify_store<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &RuntimeBody<'db>,
    dst: &crate::runtime::RuntimePlace<'db>,
    src: crate::runtime::RValueId,
) -> Result<(), VerifyError<'db>> {
    let target = super::place::project_place(db, program, body, dst)?;
    let source = runtime_value_class(body, src)?;
    if &target != source {
        return Err(VerifyError::InvalidStoreClass);
    }
    Ok(())
}

fn verify_copy_into<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &RuntimeBody<'db>,
    dst: &crate::runtime::RuntimePlace<'db>,
    src: crate::runtime::RValueId,
) -> Result<(), VerifyError<'db>> {
    let target = super::place::project_place(db, program, body, dst)?;
    let source = runtime_value_class(body, src)?;
    if &target != source && !source.shares_runtime_rep_with(db, &target) {
        return Err(VerifyError::InvalidCopyClass);
    }
    Ok(())
}

fn verify_terminator<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &RuntimeBody<'db>,
    terminator: &RTerminator<'db>,
) -> Result<(), VerifyError<'db>> {
    match terminator {
        RTerminator::Goto(_) => Ok(()),
        RTerminator::Branch { cond, .. } => {
            let RuntimeClass::Scalar(ScalarClass {
                repr: ScalarRepr::Bool,
                role: ScalarRole::Plain,
            }) = runtime_value_class(body, *cond)?
            else {
                return Err(VerifyError::InvalidExprClass(*cond));
            };
            Ok(())
        }
        RTerminator::SwitchScalar { discr, cases, .. } => {
            let RuntimeClass::Scalar(discr_class) = runtime_value_class(body, *discr)? else {
                return Err(VerifyError::InvalidExprClass(*discr));
            };
            for (value, _) in cases {
                if scalar_class_from_const(value) != *discr_class {
                    return Err(VerifyError::InvalidExprClass(*discr));
                }
            }
            Ok(())
        }
        RTerminator::MatchEnumTag {
            tag,
            enum_layout,
            cases,
            ..
        } => {
            let class = runtime_value_class(body, *tag)?;
            let RuntimeClass::Scalar(scalar) = class else {
                return Err(VerifyError::InvalidEnumTag(*enum_layout));
            };
            if !matches!(
                scalar.role,
                ScalarRole::EnumTag { enum_layout: tag_layout } if tag_layout == *enum_layout
            ) {
                return Err(VerifyError::InvalidEnumTag(*enum_layout));
            }
            let mut seen = FxHashSet::default();
            for (variant, _) in cases {
                if variant.enum_layout != *enum_layout || !seen.insert(variant.index) {
                    return Err(VerifyError::InvalidVariant(*enum_layout, variant.index));
                }
            }
            Ok(())
        }
        RTerminator::TerminalCall { callee, args } => {
            verify_call(db, program, body, *callee, args, RuntimeCallKind::Terminal)
        }
        RTerminator::ReturnData { offset, len } | RTerminator::Revert { offset, len } => {
            verify_address_operand(body, *offset, crate::runtime::AddressSpaceKind::Memory)?;
            verify_word_value(body, *len)?;
            Ok(())
        }
        RTerminator::SelfDestruct { beneficiary } => verify_word_value(body, *beneficiary),
        RTerminator::Trap => Ok(()),
        RTerminator::Return(value) => {
            let class = value
                .map(|value| runtime_value_class(body, value).cloned())
                .transpose()?;
            if class != body.signature.ret {
                return Err(VerifyError::InvalidReturnClass);
            }
            Ok(())
        }
        RTerminator::Stop => Ok(()),
    }
}
