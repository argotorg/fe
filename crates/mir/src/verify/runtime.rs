use rustc_hash::FxHashSet;

use crate::{
    db::MirDb,
    instance::RuntimeInstance,
    runtime::{
        AddressSpaceKind, Layout, RExpr, RStmt, RTerminator, RuntimeBody, RuntimeBuiltin,
        RuntimeCarrier, RuntimeClass, RuntimeExitBehavior, RuntimeInterfaceSignature,
        RuntimeLocalRoot, RuntimeProgramView, ScalarClass, ScalarRepr, ScalarRole,
    },
    verify::VerifyError,
};

use super::{
    RuntimeVerifyFailure, RuntimeVerifySite,
    consts::verify_const_region,
    layout::verify_class_layouts,
    place::{
        enum_extract_class, enum_tag_class, enum_tag_class_from_value, project_place,
        resolve_runtime_place_address_class, runtime_value_class, scalar_class_from_const,
        verify_enum_handle, verify_enum_write_variant, verify_value_enum_variant,
        verify_value_enum_variant_ref,
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

    let expr_class = match expr {
        RExpr::Use(value) => Some(runtime_value_class(body, *value)?.clone()),
        RExpr::ConstScalar(value) => match (value, &dst_class) {
            (
                crate::runtime::ConstScalar::FixedBytes(bytes),
                Some(RuntimeClass::Scalar(ScalarClass {
                    repr: ScalarRepr::FixedBytes { len },
                    role: ScalarRole::Plain,
                })),
            ) if bytes.len() <= usize::from(*len) => dst_class.clone(),
            _ => Some(RuntimeClass::Scalar(scalar_class_from_const(value))),
        },
        RExpr::Placeholder { class } => Some(class.clone()),
        RExpr::Builtin(builtin) => verify_builtin(program, body, builtin)?,
        RExpr::Unary { value, .. } => {
            if !matches!(
                (runtime_value_class(body, *value)?, &dst_class),
                (RuntimeClass::Scalar(_), Some(RuntimeClass::Scalar(_)))
            ) {
                return Err(VerifyError::InvalidExprClass(dst));
            }
            dst_class.clone()
        }
        RExpr::Binary { lhs, rhs, .. } => {
            if !matches!(
                (
                    runtime_value_class(body, *lhs)?,
                    runtime_value_class(body, *rhs)?,
                    &dst_class,
                ),
                (
                    RuntimeClass::Scalar(_),
                    RuntimeClass::Scalar(_),
                    Some(RuntimeClass::Scalar(_)),
                )
            ) {
                return Err(VerifyError::InvalidExprClass(dst));
            }
            dst_class.clone()
        }
        RExpr::Cast { value, to } => {
            let _ = runtime_value_class(body, *value)?;
            Some(RuntimeClass::Scalar(to.clone()))
        }
        RExpr::ConstRef { region, layout } => {
            let region_id = *region;
            let region = program.const_region(region_id);
            verify_const_region(db, program, region.clone())?;
            if region.layout != *layout {
                return Err(VerifyError::InvalidConstRegion(region_id));
            }
            Some(RuntimeClass::const_ref(*layout))
        }
        RExpr::AllocObject { layout } => Some(RuntimeClass::object_ref(*layout)),
        RExpr::MaterializeToObject { src } => {
            let src_class = runtime_value_class(body, *src)?;
            let Some(RuntimeClass::Ref {
                pointee,
                kind: crate::runtime::RefKind::Object,
                view: crate::runtime::RefView::Whole,
            }) = &dst_class
            else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            let RuntimeClass::AggregateValue { layout } = &**pointee else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            match src_class {
                RuntimeClass::AggregateValue { layout: src_layout } if *src_layout == *layout => {}
                RuntimeClass::Ref {
                    pointee: src_pointee,
                    kind: crate::runtime::RefKind::Const,
                    view: crate::runtime::RefView::Whole,
                } if **src_pointee == RuntimeClass::AggregateValue { layout: *layout } => {}
                _ => return Err(VerifyError::InvalidExprClass(dst)),
            }
            dst_class.clone()
        }
        RExpr::MaterializePlaceToObject { place } => {
            let Some(RuntimeClass::Ref {
                pointee,
                kind: crate::runtime::RefKind::Object,
                view: crate::runtime::RefView::Whole,
            }) = &dst_class
            else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            let RuntimeClass::AggregateValue { layout } = &**pointee else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            if project_place(db, program, body, place)?
                != (RuntimeClass::AggregateValue { layout: *layout })
            {
                return Err(VerifyError::InvalidExprClass(dst));
            }
            dst_class.clone()
        }
        RExpr::ProviderFromRaw {
            raw,
            provider_ty,
            space,
            target,
        } => {
            let RuntimeClass::RawAddr {
                space: raw_space, ..
            } = runtime_value_class(body, *raw)?
            else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            if *raw_space != *space {
                return Err(VerifyError::InvalidExprClass(dst));
            }
            match &dst_class {
                Some(RuntimeClass::Ref {
                    pointee,
                    kind:
                        crate::runtime::RefKind::Provider {
                            provider_ty: actual_provider_ty,
                            space: actual_space,
                        },
                    view: crate::runtime::RefView::Whole,
                }) if pointee.aggregate_layout() == *target
                    && actual_provider_ty == provider_ty
                    && *actual_space == *space =>
                {
                    dst_class.clone()
                }
                _ => return Err(VerifyError::InvalidExprClass(dst)),
            }
        }
        RExpr::WordToRawAddr {
            value,
            space,
            target,
        } => {
            let RuntimeClass::Scalar(ScalarClass {
                repr:
                    ScalarRepr::Int {
                        bits: 256,
                        signed: false,
                    },
                role: ScalarRole::Plain,
            }) = runtime_value_class(body, *value)?
            else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            Some(RuntimeClass::RawAddr {
                space: *space,
                target: *target,
            })
        }
        RExpr::AddrOf { place } => {
            let expected = resolve_runtime_place_address_class(db, program, body, place)?;
            if dst_class.as_ref() != Some(&expected) {
                return Err(VerifyError::InvalidExprClass(dst));
            }
            Some(expected)
        }
        RExpr::Load { place } => Some(project_place(db, program, body, place)?),
        RExpr::Call { callee, args } => {
            verify_call(db, program, body, *callee, args, RuntimeCallKind::Normal)?;
            program.interface_signature(*callee).ret.clone()
        }
        RExpr::ProviderToRaw { value } => {
            if !matches!(
                (runtime_value_class(body, *value)?, &dst_class),
                (
                    RuntimeClass::Ref {
                        kind: crate::runtime::RefKind::Provider { .. },
                        ..
                    },
                    Some(RuntimeClass::RawAddr { .. }),
                )
            ) {
                return Err(VerifyError::InvalidExprClass(dst));
            }
            dst_class.clone()
        }
        RExpr::RetagRef { value } => {
            let Some(RuntimeClass::Ref {
                pointee: dst_pointee,
                kind: dst_kind,
                view: dst_view,
            }) = &dst_class
            else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            let RuntimeClass::Ref {
                pointee: src_pointee,
                kind: src_kind,
                view: src_view,
            } = runtime_value_class(body, *value)?.clone()
            else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            let src_view_matches = src_view == *dst_view;
            let src_class = RuntimeClass::Ref {
                pointee: src_pointee,
                kind: src_kind,
                view: src_view,
            };
            if !src_view_matches
                || !src_class.shares_runtime_rep_with(
                    db,
                    &RuntimeClass::Ref {
                        pointee: dst_pointee.clone(),
                        kind: dst_kind.clone(),
                        view: dst_view.clone(),
                    },
                )
            {
                return Err(VerifyError::InvalidExprClass(dst));
            }
            dst_class.clone()
        }
        RExpr::EnumMake {
            layout,
            variant,
            fields,
        } => {
            let expected = RuntimeClass::AggregateValue { layout: *layout };
            verify_value_enum_variant(program, body, expected.clone(), *variant, fields)?;
            Some(expected)
        }
        RExpr::EnumTagOfValue { value } => Some(enum_tag_class_from_value(db, body, *value)?),
        RExpr::EnumIsVariant { value, variant } => {
            verify_value_enum_variant_ref(
                program,
                runtime_value_class(body, *value)?.clone(),
                *variant,
            )?;
            Some(RuntimeClass::Scalar(ScalarClass {
                repr: ScalarRepr::Bool,
                role: ScalarRole::Plain,
            }))
        }
        RExpr::EnumExtract {
            value,
            variant,
            field,
        } => {
            let class = enum_extract_class(db, body, *value, *variant, *field)?;
            if !same_block_dominating_enum_assert(block, stmt_idx, *value, *variant) {
                return Err(VerifyError::MissingEnumVariantProof(*value));
            }
            Some(class)
        }
        RExpr::EnumGetTag { root } => {
            let RuntimeClass::Ref { pointee, .. } = runtime_value_class(body, *root)?.clone()
            else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            let RuntimeClass::AggregateValue { layout } = *pointee else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            if !matches!(program.layout(layout), Layout::Enum(_)) {
                return Err(VerifyError::InvalidEnumTag(layout));
            }
            Some(RuntimeClass::Scalar(enum_tag_class(layout, program)))
        }
        RExpr::EnumAssertVariantRef { root, variant } => {
            let class = verify_enum_handle(body, *root, *variant, program)?;
            let RuntimeClass::Ref { pointee, kind, .. } = class else {
                unreachable!();
            };
            Some(RuntimeClass::Ref {
                pointee,
                kind,
                view: crate::runtime::RefView::EnumVariant(*variant),
            })
        }
    };

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
            | RStmt::Store { .. }
            | RStmt::CopyInto { .. }
            | RStmt::EnumSetTag { .. }
            | RStmt::EnumWriteVariant { .. } => {}
        }
    }
    proven
}

fn verify_builtin<'db>(
    program: &impl RuntimeProgramView<'db>,
    body: &RuntimeBody<'db>,
    builtin: &RuntimeBuiltin<'db>,
) -> Result<Option<RuntimeClass<'db>>, VerifyError<'db>> {
    match builtin {
        RuntimeBuiltin::Mload { addr } => {
            verify_address_operand(body, *addr, AddressSpaceKind::Memory)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::Mstore { addr, value } => {
            verify_address_operand(body, *addr, AddressSpaceKind::Memory)?;
            verify_word_value(body, *value)?;
            Ok(None)
        }
        RuntimeBuiltin::Mstore8 { addr, value } => {
            verify_address_operand(body, *addr, AddressSpaceKind::Memory)?;
            let RuntimeClass::Scalar(ScalarClass {
                repr:
                    ScalarRepr::Int {
                        bits: 8,
                        signed: false,
                    },
                ..
            }) = runtime_value_class(body, *value)?
            else {
                return Err(VerifyError::InvalidExprClass(*value));
            };
            Ok(None)
        }
        RuntimeBuiltin::Mcopy { dst, src, len } => {
            verify_address_operand(body, *dst, AddressSpaceKind::Memory)?;
            verify_address_operand(body, *src, AddressSpaceKind::Memory)?;
            verify_word_value(body, *len)?;
            Ok(None)
        }
        RuntimeBuiltin::Msize
        | RuntimeBuiltin::CallValue
        | RuntimeBuiltin::ReturnDataSize
        | RuntimeBuiltin::CallDataSize
        | RuntimeBuiltin::CodeSize
        | RuntimeBuiltin::Address
        | RuntimeBuiltin::Caller
        | RuntimeBuiltin::Origin
        | RuntimeBuiltin::GasPrice
        | RuntimeBuiltin::CoinBase
        | RuntimeBuiltin::Timestamp
        | RuntimeBuiltin::Number
        | RuntimeBuiltin::PrevRandao
        | RuntimeBuiltin::GasLimit
        | RuntimeBuiltin::ChainId
        | RuntimeBuiltin::BaseFee
        | RuntimeBuiltin::SelfBalance
        | RuntimeBuiltin::Gas => Ok(Some(RuntimeClass::Scalar(word_scalar_class()))),
        RuntimeBuiltin::Sload { slot } => {
            verify_address_operand(body, *slot, AddressSpaceKind::Storage)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::Sstore { slot, value } => {
            verify_address_operand(body, *slot, AddressSpaceKind::Storage)?;
            verify_word_value(body, *value)?;
            Ok(None)
        }
        RuntimeBuiltin::CallDataLoad { offset } => {
            verify_word_value(body, *offset)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::ReturnDataCopy { dst, offset, len }
        | RuntimeBuiltin::CallDataCopy { dst, offset, len }
        | RuntimeBuiltin::CodeCopy { dst, offset, len } => {
            verify_address_operand(body, *dst, AddressSpaceKind::Memory)?;
            verify_word_value(body, *offset)?;
            verify_word_value(body, *len)?;
            Ok(None)
        }
        RuntimeBuiltin::Keccak256 { offset, len } => {
            verify_address_operand(body, *offset, AddressSpaceKind::Memory)?;
            verify_word_value(body, *len)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::AddMod { lhs, rhs, modulus }
        | RuntimeBuiltin::MulMod { lhs, rhs, modulus } => {
            verify_word_value(body, *lhs)?;
            verify_word_value(body, *rhs)?;
            verify_word_value(body, *modulus)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::SignExtend { byte, value } => {
            verify_word_value(body, *byte)?;
            verify_word_value(body, *value)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::IntrinsicArith {
            lhs, rhs, class, ..
        } => {
            let expected = RuntimeClass::Scalar(class.clone());
            if runtime_value_class(body, *lhs)? != &expected
                || runtime_value_class(body, *rhs)? != &expected
            {
                return Err(VerifyError::InvalidExprClass(*lhs));
            }
            Ok(Some(expected))
        }
        RuntimeBuiltin::Saturating {
            lhs, rhs, class, ..
        } => {
            let expected = RuntimeClass::Scalar(class.clone());
            if runtime_value_class(body, *lhs)? != &expected
                || runtime_value_class(body, *rhs)? != &expected
            {
                return Err(VerifyError::InvalidExprClass(*lhs));
            }
            Ok(Some(expected))
        }
        RuntimeBuiltin::BlockHash { block } => {
            verify_word_value(body, *block)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::CurrentCodeRegionLen => Ok(Some(RuntimeClass::Scalar(word_scalar_class()))),
        RuntimeBuiltin::CodeRegionOffset { region } | RuntimeBuiltin::CodeRegionLen { region } => {
            let _ = program.code_region(*region);
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::Malloc { size } => {
            verify_word_value(body, *size)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::Call {
            gas,
            addr,
            value,
            args_offset,
            args_len,
            ret_offset,
            ret_len,
        } => {
            verify_word_value(body, *gas)?;
            verify_word_value(body, *addr)?;
            verify_word_value(body, *value)?;
            verify_address_operand(body, *args_offset, AddressSpaceKind::Memory)?;
            verify_word_value(body, *args_len)?;
            verify_address_operand(body, *ret_offset, AddressSpaceKind::Memory)?;
            verify_word_value(body, *ret_len)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::StaticCall {
            gas,
            addr,
            args_offset,
            args_len,
            ret_offset,
            ret_len,
        }
        | RuntimeBuiltin::DelegateCall {
            gas,
            addr,
            args_offset,
            args_len,
            ret_offset,
            ret_len,
        } => {
            verify_word_value(body, *gas)?;
            verify_word_value(body, *addr)?;
            verify_address_operand(body, *args_offset, AddressSpaceKind::Memory)?;
            verify_word_value(body, *args_len)?;
            verify_address_operand(body, *ret_offset, AddressSpaceKind::Memory)?;
            verify_word_value(body, *ret_len)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::Create { value, offset, len } => {
            verify_word_value(body, *value)?;
            verify_address_operand(body, *offset, AddressSpaceKind::Memory)?;
            verify_word_value(body, *len)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::Create2 {
            value,
            offset,
            len,
            salt,
        } => {
            verify_word_value(body, *value)?;
            verify_address_operand(body, *offset, AddressSpaceKind::Memory)?;
            verify_word_value(body, *len)?;
            verify_word_value(body, *salt)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::Log0 { offset, len } => {
            verify_address_operand(body, *offset, AddressSpaceKind::Memory)?;
            verify_word_value(body, *len)?;
            Ok(None)
        }
        RuntimeBuiltin::Log1 {
            offset,
            len,
            topic0,
        } => {
            verify_address_operand(body, *offset, AddressSpaceKind::Memory)?;
            verify_word_value(body, *len)?;
            verify_word_value(body, *topic0)?;
            Ok(None)
        }
        RuntimeBuiltin::Log2 {
            offset,
            len,
            topic0,
            topic1,
        } => {
            verify_address_operand(body, *offset, AddressSpaceKind::Memory)?;
            verify_word_value(body, *len)?;
            verify_word_value(body, *topic0)?;
            verify_word_value(body, *topic1)?;
            Ok(None)
        }
        RuntimeBuiltin::Log3 {
            offset,
            len,
            topic0,
            topic1,
            topic2,
        } => {
            verify_address_operand(body, *offset, AddressSpaceKind::Memory)?;
            verify_word_value(body, *len)?;
            verify_word_value(body, *topic0)?;
            verify_word_value(body, *topic1)?;
            verify_word_value(body, *topic2)?;
            Ok(None)
        }
        RuntimeBuiltin::Log4 {
            offset,
            len,
            topic0,
            topic1,
            topic2,
            topic3,
        } => {
            verify_address_operand(body, *offset, AddressSpaceKind::Memory)?;
            verify_word_value(body, *len)?;
            verify_word_value(body, *topic0)?;
            verify_word_value(body, *topic1)?;
            verify_word_value(body, *topic2)?;
            verify_word_value(body, *topic3)?;
            Ok(None)
        }
        RuntimeBuiltin::CallDataSelector => Ok(Some(RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits: 32,
                signed: false,
            },
            role: ScalarRole::Plain,
        }))),
        RuntimeBuiltin::MakeContractFieldRef { class, kind, .. } => {
            if let RuntimeClass::Ref {
                kind: actual_kind,
                view: crate::runtime::RefView::Whole,
                ..
            } = class
                && actual_kind != kind
            {
                return Err(VerifyError::InvalidPlace(class.clone()));
            }
            Ok(Some(class.clone()))
        }
    }
}

fn verify_store<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &RuntimeBody<'db>,
    dst: &crate::runtime::RuntimePlace<'db>,
    src: crate::runtime::RValueId,
) -> Result<(), VerifyError<'db>> {
    let target = project_place(db, program, body, dst)?;
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
    let target = project_place(db, program, body, dst)?;
    let source = runtime_value_class(body, src)?;
    if &target != source {
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
            verify_address_operand(body, *offset, AddressSpaceKind::Memory)?;
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

fn verify_word_value<'db>(
    body: &RuntimeBody<'db>,
    value: crate::runtime::RValueId,
) -> Result<(), VerifyError<'db>> {
    let RuntimeClass::Scalar(ScalarClass {
        repr: ScalarRepr::Int {
            bits: 256,
            signed: false,
        },
        ..
    }) = runtime_value_class(body, value)?
    else {
        return Err(VerifyError::InvalidExprClass(value));
    };
    Ok(())
}

fn verify_address_operand<'db>(
    body: &RuntimeBody<'db>,
    value: crate::runtime::RValueId,
    space: AddressSpaceKind,
) -> Result<(), VerifyError<'db>> {
    match runtime_value_class(body, value)? {
        RuntimeClass::RawAddr {
            space: actual_space,
            ..
        } if *actual_space == space => Ok(()),
        RuntimeClass::Scalar(ScalarClass {
            repr:
                ScalarRepr::Int {
                    bits: 256,
                    signed: false,
                },
            ..
        }) => Ok(()),
        _ => Err(VerifyError::InvalidExprClass(value)),
    }
}

fn word_scalar_class<'db>() -> ScalarClass<'db> {
    ScalarClass {
        repr: ScalarRepr::Int {
            bits: 256,
            signed: false,
        },
        role: ScalarRole::Plain,
    }
}
