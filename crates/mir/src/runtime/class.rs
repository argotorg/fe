//! Canonical derivation of runtime expression result classes.
//!
//! `expr_result_class` is the single source of truth for "what
//! [`RuntimeClass`] does this [`RExpr`] produce". The verifier checks every
//! assignment against it, and body lowering anchors its carrier stamping to
//! it (see `lower_runtime_body`), so a body whose carriers disagree with
//! this derivation cannot survive lowering in debug builds and cannot pass
//! package verification in any build.
//!
//! Some instructions (`RetagRef`, `ProviderToRaw`, arithmetic) are
//! destination-directed: their result class is chosen by the destination
//! carrier, subject to representation compatibility. For those, this module
//! validates the destination against the operands and echoes it back; the
//! destination class is an input, not just a check target.
//!
//! TODO(P1c): place resolution (`project_place` and friends) still lives in
//! `crate::verify::place`; relocate it under `runtime/` and drop the verify
//! imports here.

use crate::{
    db::MirDb,
    runtime::{
        AddressSpaceKind, Layout, RExpr, RLocalId, RValueId, RuntimeBody, RuntimeBuiltin,
        RuntimeClass, RuntimeLayoutMap, RuntimeProgramView, ScalarClass, ScalarRepr, ScalarRole,
    },
    verify::{
        VerifyError,
        place::{
            enum_extract_class, enum_tag_class, enum_tag_class_from_value, project_place,
            resolve_runtime_place_address_class, runtime_value_class, scalar_class_from_const,
            verify_enum_handle, verify_value_enum_variant, verify_value_enum_variant_ref,
        },
        verify_const_region,
    },
};

/// Derive the result class of `expr` when assigned to `dst` (whose declared
/// class is `dst_class`; `None` for erased destinations).
///
/// Returns `Ok(None)` for expressions with no runtime value (e.g. effectful
/// builtins). Errors report malformed operands or destination-directed
/// instructions whose destination is incompatible with their operands. The
/// final representation-compatibility decision between the returned class
/// and the destination carrier is the caller's.
pub fn expr_result_class<'db>(
    db: &'db dyn MirDb,
    program: &impl RuntimeProgramView<'db>,
    body: &RuntimeBody<'db>,
    dst: RLocalId,
    dst_class: Option<RuntimeClass<'db>>,
    expr: &RExpr<'db>,
) -> Result<Option<RuntimeClass<'db>>, VerifyError<'db>> {
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
        RExpr::Builtin(builtin) => builtin_result_class(program, body, builtin)?,
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
        RExpr::AggregateExtract { value, index } => {
            let RuntimeClass::AggregateValue { layout } = runtime_value_class(body, *value)? else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            let field = aggregate_index_class(program, *layout, *index as usize)
                .ok_or(VerifyError::InvalidExprClass(dst))?;
            Some(field)
        }
        RExpr::AggregateMake { layout, fields } => {
            let expected_len = aggregate_field_count(program, *layout)
                .ok_or(VerifyError::InvalidExprClass(dst))?;
            if fields.len() != expected_len {
                return Err(VerifyError::InvalidExprClass(dst));
            }
            for (idx, field) in fields.iter().enumerate() {
                let expected = aggregate_index_class(program, *layout, idx)
                    .ok_or(VerifyError::InvalidExprClass(dst))?;
                body.local(*field)
                    .ok_or(VerifyError::MissingRuntimeLocal(*field))?;
                match body.value_class(*field) {
                    Some(actual) if actual.shares_runtime_rep_with(db, &expected) => {}
                    None if expected.span_words(db) == 0 => {}
                    Some(_) | None => return Err(VerifyError::InvalidExprClass(dst)),
                }
            }
            Some(RuntimeClass::AggregateValue { layout: *layout })
        }
        RExpr::LayoutMapAffine { map, base, strides } => {
            if !valid_ranked_layout_map(map)
                || strides.len() != map.rank()
                || runtime_value_class(body, *base)? != &RuntimeClass::Scalar(map.scalar().clone())
                || strides.iter().any(|stride| {
                    runtime_value_class(body, *stride).ok()
                        != Some(&RuntimeClass::Scalar(map.scalar().clone()))
                })
            {
                return Err(VerifyError::InvalidExprClass(dst));
            }
            Some(map.class())
        }
        RExpr::LayoutMapDense { map, elements } => {
            let Some(child) = map.projected() else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            if !valid_ranked_layout_map(map)
                || map.dimensions().first().copied() != Some(elements.len())
                || elements.is_empty()
                || elements
                    .iter()
                    .any(|element| runtime_value_class(body, *element).ok() != Some(&child.class()))
            {
                return Err(VerifyError::InvalidExprClass(dst));
            }
            Some(map.class())
        }
        RExpr::LayoutMapRepeat { map, element } => {
            let Some(child) = map.projected() else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            if !valid_ranked_layout_map(map)
                || runtime_value_class(body, *element)? != &child.class()
            {
                return Err(VerifyError::InvalidExprClass(dst));
            }
            Some(map.class())
        }
        RExpr::LayoutMapProject { map, source, index } => {
            let Some(child) = map.projected() else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            if !valid_ranked_layout_map(map)
                || runtime_value_class(body, *source)? != &map.class()
                || !valid_layout_map_index(body, *index)
            {
                return Err(VerifyError::InvalidExprClass(dst));
            }
            Some(child.class())
        }
        RExpr::LayoutMapPatch {
            map,
            source,
            index,
            replacement,
        } => {
            let Some(child) = map.projected() else {
                return Err(VerifyError::InvalidExprClass(dst));
            };
            if !valid_ranked_layout_map(map)
                || runtime_value_class(body, *source)? != &map.class()
                || !valid_layout_map_index(body, *index)
                || runtime_value_class(body, *replacement)? != &child.class()
            {
                return Err(VerifyError::InvalidExprClass(dst));
            }
            Some(map.class())
        }
        RExpr::Call { callee, .. } => program.interface_signature(*callee).ret.clone(),
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
        } => Some(enum_extract_class(db, body, *value, *variant, *field)?),
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
    Ok(expr_class)
}

fn valid_ranked_layout_map(map: &RuntimeLayoutMap<'_>) -> bool {
    !map.dimensions().is_empty()
        && map.dimensions().iter().all(|dimension| *dimension > 0)
        && matches!(
            map.scalar(),
            ScalarClass {
                repr: ScalarRepr::Int {
                    bits: 256,
                    signed: false,
                },
                role: ScalarRole::Plain,
            }
        )
}

fn valid_layout_map_index(body: &RuntimeBody<'_>, index: RValueId) -> bool {
    matches!(
        runtime_value_class(body, index),
        Ok(RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int { signed: false, .. },
            role: ScalarRole::Plain,
        }))
    )
}

fn builtin_result_class<'db>(
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
        | RuntimeBuiltin::BlobBaseFee
        | RuntimeBuiltin::SelfBalance
        | RuntimeBuiltin::Gas => Ok(Some(RuntimeClass::Scalar(word_scalar_class()))),
        RuntimeBuiltin::Balance { addr } => {
            verify_word_value(body, *addr)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::ExtCodeSize { addr } | RuntimeBuiltin::ExtCodeHash { addr } => {
            verify_word_value(body, *addr)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
        RuntimeBuiltin::BlobHash { index } => {
            verify_word_value(body, *index)?;
            Ok(Some(RuntimeClass::Scalar(word_scalar_class())))
        }
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
        RuntimeBuiltin::ExtCodeCopy {
            addr,
            dst,
            offset,
            len,
        } => {
            verify_word_value(body, *addr)?;
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
        RuntimeBuiltin::Byte { pos, value } => {
            verify_word_value(body, *pos)?;
            verify_word_value(body, *value)?;
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

pub(crate) fn verify_word_value<'db>(
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

pub(crate) fn verify_address_operand<'db>(
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

fn aggregate_index_class<'db>(
    program: &impl RuntimeProgramView<'db>,
    layout: crate::runtime::LayoutId<'db>,
    index: usize,
) -> Option<RuntimeClass<'db>> {
    match program.layout(layout) {
        Layout::Struct(data) => data.fields.get(index).cloned(),
        Layout::Array(data) => (index < data.len as usize).then(|| data.elem.clone()),
        Layout::Enum(_) => None,
    }
}

fn aggregate_field_count<'db>(
    program: &impl RuntimeProgramView<'db>,
    layout: crate::runtime::LayoutId<'db>,
) -> Option<usize> {
    match program.layout(layout) {
        Layout::Struct(data) => Some(data.fields.len()),
        Layout::Array(data) => Some(data.len as usize),
        Layout::Enum(_) => None,
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
