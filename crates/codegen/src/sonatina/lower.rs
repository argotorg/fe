//! Instruction-level lowering from MIR to Sonatina IR.
//!
//! Contains all `lower_*` free functions that operate on `LowerCtx`.

use driver::DriverDataBase;
use hir::analysis::ty::adt_def::AdtRef;
use hir::analysis::ty::ty_def::{PrimTy, TyBase, TyData};
use hir::hir_def::expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp};
use hir::projection::{IndexSource, Projection};
use mir::ir::{AddressSpaceKind, IntrinsicOp, Place, SyntheticValue};
use mir::layout;
use mir::layout::TargetDataLayout;
use num_bigint::BigUint;
use rustc_hash::FxHashMap;
use smallvec1::SmallVec;
use sonatina_ir::{
    BlockId, GlobalVariableData, I256, Immediate, Linkage, Type, ValueId,
    global_variable::GvInitializer,
    inst::{
        arith::{Add, Mul, Neg, Shl, Shr, Sub},
        cast::{IntToPtr, PtrToInt, Sext, Trunc, Zext},
        cmp::{Eq, Gt, IsZero, Lt, Ne},
        control_flow::{Br, Call, Jump, Return},
        data::{Gep, Mload, Mstore, SymAddr, SymSize, SymbolRef},
        evm::{
            EvmAddMod, EvmAddress, EvmBaseFee, EvmBlockHash, EvmCall, EvmCallValue,
            EvmCalldataCopy, EvmCalldataLoad, EvmCalldataSize, EvmCaller, EvmChainId, EvmCodeCopy,
            EvmCodeSize, EvmCoinBase, EvmCreate, EvmCreate2, EvmDelegateCall, EvmExp, EvmGas,
            EvmGasLimit, EvmInvalid, EvmKeccak256, EvmLog0, EvmLog1, EvmLog2, EvmLog3, EvmLog4,
            EvmMalloc, EvmMsize, EvmMstore8, EvmMulMod, EvmNumber, EvmOrigin, EvmPrevRandao,
            EvmReturn, EvmReturnDataCopy, EvmReturnDataSize, EvmRevert, EvmSelfBalance,
            EvmSelfDestruct, EvmSload, EvmSstore, EvmStaticCall, EvmStop, EvmTimestamp, EvmTload,
            EvmTstore, EvmUdiv, EvmUmod,
        },
        logic::{And, Not, Or, Xor},
    },
    module::FuncRef,
    object::EmbedSymbol,
};

use super::{LowerCtx, LowerError, is_erased_runtime_ty, types};

/// Lower a MIR instruction.
pub(super) fn lower_instruction<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    inst: &mir::MirInst<'db>,
) -> Result<(), LowerError> {
    use mir::MirInst;

    match inst {
        MirInst::Assign { dest, rvalue, .. } => {
            if let mir::Rvalue::Alloc { address_space } = rvalue {
                let Some(dest_local) = dest else {
                    return Err(LowerError::Internal(
                        "alloc rvalue without destination local".to_string(),
                    ));
                };
                let value = lower_alloc(ctx, *dest_local, *address_space)?;
                let dest_var = ctx.local_vars.get(dest_local).copied().ok_or_else(|| {
                    LowerError::Internal(format!("missing SSA variable for local {dest_local:?}"))
                })?;
                ctx.fb.def_var(dest_var, value);
                return Ok(());
            }

            if let mir::Rvalue::ConstAggregate { data, .. } = rvalue {
                let Some(dest_local) = dest else {
                    return Err(LowerError::Internal(
                        "ConstAggregate without destination local".to_string(),
                    ));
                };
                let value = lower_const_aggregate(ctx, data)?;
                let dest_var = ctx.local_vars.get(dest_local).copied().ok_or_else(|| {
                    LowerError::Internal(format!("missing SSA variable for local {dest_local:?}"))
                })?;
                ctx.fb.def_var(dest_var, value);
                return Ok(());
            }

            let result = lower_rvalue(ctx, rvalue)?;
            if let (Some(dest_local), Some(result_val)) = (dest, result) {
                let dest_var = ctx.local_vars.get(dest_local).copied().ok_or_else(|| {
                    LowerError::Internal(format!("missing SSA variable for local {dest_local:?}"))
                })?;
                // Apply from_word conversion for Load operations
                let converted = if matches!(rvalue, mir::Rvalue::Load { .. }) {
                    let dest_ty = ctx
                        .body
                        .locals
                        .get(dest_local.index())
                        .map(|l| l.ty)
                        .ok_or_else(|| {
                            LowerError::Internal(format!("missing local type for {dest_local:?}"))
                        })?;
                    apply_from_word(ctx.fb, ctx.db, result_val, dest_ty, ctx.is)
                } else {
                    result_val
                };
                ctx.fb.def_var(dest_var, converted);
            }
        }
        MirInst::Store { place, value, .. } => {
            lower_store_inst(ctx, place, *value)?;
        }
        MirInst::InitAggregate { place, inits, .. } => {
            for (path, value) in inits {
                let mut target = place.clone();
                for proj in path.iter() {
                    target.projection.push(proj.clone());
                }
                lower_store_inst(ctx, &target, *value)?;
            }
        }
        MirInst::SetDiscriminant { place, variant, .. } => {
            let val = ctx.fb.make_imm_value(I256::from(variant.idx as u64));
            store_word_to_place(ctx, place, val)?;
        }
        MirInst::BindValue { value, .. } => {
            // Ensure the value is lowered and cached
            let _ = lower_value(ctx, *value)?;
        }
    }

    Ok(())
}

/// Lower a MIR rvalue to a Sonatina value.
fn lower_rvalue<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    rvalue: &mir::Rvalue<'db>,
) -> Result<Option<ValueId>, LowerError> {
    use mir::Rvalue;

    match rvalue {
        Rvalue::ZeroInit => {
            // Create a zero constant
            let zero = ctx.fb.make_imm_value(I256::zero());
            Ok(Some(zero))
        }
        Rvalue::Value(value_id) => {
            let val = lower_value(ctx, *value_id)?;
            Ok(Some(val))
        }
        Rvalue::Call(call) => {
            // Get the callee function reference
            let callee_name = call.resolved_name.as_ref().ok_or_else(|| {
                LowerError::Unsupported("call without resolved symbol name".to_string())
            })?;

            if call.effect_args.is_empty() {
                // `std::evm::ops` externs (Yul builtins).
                //
                // These are declared in Fe as `extern`, so they do not have MIR bodies. The Yul
                // backend emits them as builtins; the Sonatina backend must lower them directly.
                match callee_name.as_str() {
                    // Logs
                    "log0" | "log1" | "log2" | "log3" | "log4" => {
                        let mut args = Vec::with_capacity(call.args.len());
                        for &arg in &call.args {
                            args.push(lower_value(ctx, arg)?);
                        }
                        match (callee_name.as_str(), args.as_slice()) {
                            ("log0", [offset, len]) => {
                                ctx.fb
                                    .insert_inst_no_result(EvmLog0::new(ctx.is, *offset, *len));
                                return Ok(None);
                            }
                            ("log1", [offset, len, topic0]) => {
                                ctx.fb.insert_inst_no_result(EvmLog1::new(
                                    ctx.is, *offset, *len, *topic0,
                                ));
                                return Ok(None);
                            }
                            ("log2", [offset, len, topic0, topic1]) => {
                                ctx.fb.insert_inst_no_result(EvmLog2::new(
                                    ctx.is, *offset, *len, *topic0, *topic1,
                                ));
                                return Ok(None);
                            }
                            ("log3", [offset, len, topic0, topic1, topic2]) => {
                                ctx.fb.insert_inst_no_result(EvmLog3::new(
                                    ctx.is, *offset, *len, *topic0, *topic1, *topic2,
                                ));
                                return Ok(None);
                            }
                            ("log4", [offset, len, topic0, topic1, topic2, topic3]) => {
                                ctx.fb.insert_inst_no_result(EvmLog4::new(
                                    ctx.is, *offset, *len, *topic0, *topic1, *topic2, *topic3,
                                ));
                                return Ok(None);
                            }
                            _ => {
                                return Err(LowerError::Internal(format!(
                                    "{callee_name} expects {} args, got {}",
                                    match callee_name.as_str() {
                                        "log0" => 2,
                                        "log1" => 3,
                                        "log2" => 4,
                                        "log3" => 5,
                                        "log4" => 6,
                                        _ => unreachable!(),
                                    },
                                    args.len()
                                )));
                            }
                        }
                    }

                    // Environment
                    "address" => {
                        return Ok(Some(
                            ctx.fb.insert_inst(EvmAddress::new(ctx.is), Type::I256),
                        ));
                    }
                    "callvalue" => {
                        return Ok(Some(
                            ctx.fb.insert_inst(EvmCallValue::new(ctx.is), Type::I256),
                        ));
                    }
                    "origin" => {
                        return Ok(Some(ctx.fb.insert_inst(EvmOrigin::new(ctx.is), Type::I256)));
                    }
                    "gasprice" => {
                        return Err(LowerError::Unsupported(
                            "gasprice is not supported by the Sonatina backend".to_string(),
                        ));
                    }
                    "coinbase" => {
                        return Ok(Some(
                            ctx.fb.insert_inst(EvmCoinBase::new(ctx.is), Type::I256),
                        ));
                    }
                    "timestamp" => {
                        return Ok(Some(
                            ctx.fb.insert_inst(EvmTimestamp::new(ctx.is), Type::I256),
                        ));
                    }
                    "number" => {
                        return Ok(Some(ctx.fb.insert_inst(EvmNumber::new(ctx.is), Type::I256)));
                    }
                    "prevrandao" => {
                        return Ok(Some(
                            ctx.fb.insert_inst(EvmPrevRandao::new(ctx.is), Type::I256),
                        ));
                    }
                    "gaslimit" => {
                        return Ok(Some(
                            ctx.fb.insert_inst(EvmGasLimit::new(ctx.is), Type::I256),
                        ));
                    }
                    "chainid" => {
                        return Ok(Some(
                            ctx.fb.insert_inst(EvmChainId::new(ctx.is), Type::I256),
                        ));
                    }
                    "basefee" => {
                        return Ok(Some(
                            ctx.fb.insert_inst(EvmBaseFee::new(ctx.is), Type::I256),
                        ));
                    }
                    "selfbalance" => {
                        return Ok(Some(
                            ctx.fb.insert_inst(EvmSelfBalance::new(ctx.is), Type::I256),
                        ));
                    }
                    "blockhash" => {
                        let [block] = call.args.as_slice() else {
                            return Err(LowerError::Internal(
                                "blockhash requires 1 argument".to_string(),
                            ));
                        };
                        let block = lower_value(ctx, *block)?;
                        return Ok(Some(
                            ctx.fb
                                .insert_inst(EvmBlockHash::new(ctx.is, block), Type::I256),
                        ));
                    }
                    "gas" => return Ok(Some(ctx.fb.insert_inst(EvmGas::new(ctx.is), Type::I256))),

                    // Memory size
                    "msize" => {
                        return Ok(Some(ctx.fb.insert_inst(EvmMsize::new(ctx.is), Type::I256)));
                    }

                    // Calls / create
                    "create" => {
                        let [val, offset, len] = call.args.as_slice() else {
                            return Err(LowerError::Internal(
                                "create requires 3 arguments".to_string(),
                            ));
                        };
                        let val = lower_value(ctx, *val)?;
                        let offset = lower_value(ctx, *offset)?;
                        let len = lower_value(ctx, *len)?;
                        return Ok(Some(ctx.fb.insert_inst(
                            EvmCreate::new(ctx.is, val, offset, len),
                            Type::I256,
                        )));
                    }
                    "create2" => {
                        let [val, offset, len, salt] = call.args.as_slice() else {
                            return Err(LowerError::Internal(
                                "create2 requires 4 arguments".to_string(),
                            ));
                        };
                        let val = lower_value(ctx, *val)?;
                        let offset = lower_value(ctx, *offset)?;
                        let len = lower_value(ctx, *len)?;
                        let salt = lower_value(ctx, *salt)?;
                        return Ok(Some(ctx.fb.insert_inst(
                            EvmCreate2::new(ctx.is, val, offset, len, salt),
                            Type::I256,
                        )));
                    }
                    "call" => {
                        let [gas, addr, val, arg_offset, arg_len, ret_offset, ret_len] =
                            call.args.as_slice()
                        else {
                            return Err(LowerError::Internal(
                                "call requires 7 arguments".to_string(),
                            ));
                        };
                        let gas = lower_value(ctx, *gas)?;
                        let addr = lower_value(ctx, *addr)?;
                        let val = lower_value(ctx, *val)?;
                        let arg_offset = lower_value(ctx, *arg_offset)?;
                        let arg_len = lower_value(ctx, *arg_len)?;
                        let ret_offset = lower_value(ctx, *ret_offset)?;
                        let ret_len = lower_value(ctx, *ret_len)?;
                        return Ok(Some(ctx.fb.insert_inst(
                            EvmCall::new(
                                ctx.is, gas, addr, val, arg_offset, arg_len, ret_offset, ret_len,
                            ),
                            Type::I256,
                        )));
                    }
                    "staticcall" => {
                        let [gas, addr, arg_offset, arg_len, ret_offset, ret_len] =
                            call.args.as_slice()
                        else {
                            return Err(LowerError::Internal(
                                "staticcall requires 6 arguments".to_string(),
                            ));
                        };
                        let gas = lower_value(ctx, *gas)?;
                        let addr = lower_value(ctx, *addr)?;
                        let arg_offset = lower_value(ctx, *arg_offset)?;
                        let arg_len = lower_value(ctx, *arg_len)?;
                        let ret_offset = lower_value(ctx, *ret_offset)?;
                        let ret_len = lower_value(ctx, *ret_len)?;
                        return Ok(Some(ctx.fb.insert_inst(
                            EvmStaticCall::new(
                                ctx.is, gas, addr, arg_offset, arg_len, ret_offset, ret_len,
                            ),
                            Type::I256,
                        )));
                    }
                    "delegatecall" => {
                        let [gas, addr, arg_offset, arg_len, ret_offset, ret_len] =
                            call.args.as_slice()
                        else {
                            return Err(LowerError::Internal(
                                "delegatecall requires 6 arguments".to_string(),
                            ));
                        };
                        let gas = lower_value(ctx, *gas)?;
                        let addr = lower_value(ctx, *addr)?;
                        let arg_offset = lower_value(ctx, *arg_offset)?;
                        let arg_len = lower_value(ctx, *arg_len)?;
                        let ret_offset = lower_value(ctx, *ret_offset)?;
                        let ret_len = lower_value(ctx, *ret_len)?;
                        return Ok(Some(ctx.fb.insert_inst(
                            EvmDelegateCall::new(
                                ctx.is, gas, addr, arg_offset, arg_len, ret_offset, ret_len,
                            ),
                            Type::I256,
                        )));
                    }
                    _ => {}
                }
            }

            // Special-case a few thin std wrappers that are semantically EVM opcodes.
            //
            // These wrappers show up as regular MIR functions (not `extern`), but in the Sonatina
            // backend we prefer to lower them directly to opcodes to avoid depending on internal
            // call return-value plumbing for correctness.
            if call.effect_args.is_empty() {
                match callee_name.as_str() {
                    "alloc" => {
                        let [size] = call.args.as_slice() else {
                            return Err(LowerError::Internal(
                                "alloc expects 1 argument (size)".to_string(),
                            ));
                        };
                        let size_ty = ctx
                            .body
                            .values
                            .get(size.index())
                            .ok_or_else(|| {
                                LowerError::Internal("unknown call argument".to_string())
                            })?
                            .ty;
                        if is_erased_runtime_ty(ctx.db, ctx.target_layout, size_ty) {
                            return Err(LowerError::Internal(
                                "alloc size argument unexpectedly erased".to_string(),
                            ));
                        }
                        let size = lower_value(ctx, *size)?;
                        return Ok(Some(emit_evm_malloc_word_addr(ctx.fb, size, ctx.is)));
                    }
                    "evm_create_create_raw" => {
                        let mut lowered = Vec::new();
                        for &arg in &call.args {
                            let arg_ty = ctx
                                .body
                                .values
                                .get(arg.index())
                                .ok_or_else(|| {
                                    LowerError::Internal("unknown call argument".to_string())
                                })?
                                .ty;
                            if is_erased_runtime_ty(ctx.db, ctx.target_layout, arg_ty) {
                                continue;
                            }
                            lowered.push(lower_value(ctx, arg)?);
                        }

                        let [val, offset, len] = lowered.as_slice() else {
                            return Err(LowerError::Internal(format!(
                                "{callee_name} expects 3 args (value, offset, len) after ZST erasure, got {}",
                                lowered.len()
                            )));
                        };
                        return Ok(Some(ctx.fb.insert_inst(
                            EvmCreate::new(ctx.is, *val, *offset, *len),
                            Type::I256,
                        )));
                    }
                    "evm_create_create2_raw" => {
                        let mut lowered = Vec::new();
                        for &arg in &call.args {
                            let arg_ty = ctx
                                .body
                                .values
                                .get(arg.index())
                                .ok_or_else(|| {
                                    LowerError::Internal("unknown call argument".to_string())
                                })?
                                .ty;
                            if is_erased_runtime_ty(ctx.db, ctx.target_layout, arg_ty) {
                                continue;
                            }
                            lowered.push(lower_value(ctx, arg)?);
                        }

                        let [val, offset, len, salt] = lowered.as_slice() else {
                            return Err(LowerError::Internal(format!(
                                "{callee_name} expects 4 args (value, offset, len, salt) after ZST erasure, got {}",
                                lowered.len()
                            )));
                        };
                        return Ok(Some(ctx.fb.insert_inst(
                            EvmCreate2::new(ctx.is, *val, *offset, *len, *salt),
                            Type::I256,
                        )));
                    }
                    _ => {}
                }
            }

            let func_ref = ctx
                .name_map
                .get(callee_name)
                .ok_or_else(|| LowerError::Internal(format!("unknown function: {callee_name}")))?;

            let args = lower_call_args(
                ctx,
                callee_name,
                &call.args,
                &call.effect_args,
                *func_ref,
                "call",
            )?;

            // Emit call instruction with proper return type
            let call_inst = Call::new(ctx.is, *func_ref, args.into());
            let callee_returns =
                ctx.returns_value_map
                    .get(callee_name)
                    .copied()
                    .ok_or_else(|| {
                        LowerError::Internal(format!(
                            "missing return type metadata for function: {callee_name}"
                        ))
                    })?;
            if callee_returns {
                let result = ctx.fb.insert_inst(call_inst, types::word_type());
                Ok(Some(result))
            } else {
                // Unit-returning calls don't produce a value
                ctx.fb.insert_inst_no_result(call_inst);
                Ok(None)
            }
        }
        Rvalue::Intrinsic { op, args } => lower_intrinsic(ctx, *op, args),
        Rvalue::Load { place } => {
            let addr = lower_place_address(ctx, place)?;
            let addr_space = ctx.body.place_address_space(place);

            let result = match addr_space {
                AddressSpaceKind::Memory => {
                    let load = Mload::new(ctx.is, addr, Type::I256);
                    ctx.fb.insert_inst(load, Type::I256)
                }
                AddressSpaceKind::Storage => {
                    let load = EvmSload::new(ctx.is, addr);
                    ctx.fb.insert_inst(load, Type::I256)
                }
                AddressSpaceKind::TransientStorage => {
                    let load = EvmTload::new(ctx.is, addr);
                    ctx.fb.insert_inst(load, Type::I256)
                }
                AddressSpaceKind::Calldata => {
                    let load = EvmCalldataLoad::new(ctx.is, addr);
                    ctx.fb.insert_inst(load, Type::I256)
                }
            };
            Ok(Some(result))
        }
        Rvalue::Alloc { .. } => Err(LowerError::Internal(
            "Alloc rvalue should be handled directly in Assign lowering".to_string(),
        )),
        Rvalue::ConstAggregate { .. } => Err(LowerError::Unsupported(
            "ConstAggregate not yet supported in Sonatina backend".to_string(),
        )),
    }
}

/// Lower call arguments (regular + effect), applying the runtime parameter mask or
/// filtering erased types, then zero-padding to match the callee signature width.
fn lower_call_args<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    callee_name: &str,
    regular_args: &[mir::ValueId],
    effect_args: &[mir::ValueId],
    func_ref: FuncRef,
    context: &str,
) -> Result<Vec<ValueId>, LowerError> {
    let mut args = Vec::with_capacity(regular_args.len() + effect_args.len());
    if let Some(mask) = ctx.runtime_param_masks.get(callee_name) {
        let all_args: Vec<_> = regular_args
            .iter()
            .chain(effect_args.iter())
            .copied()
            .collect();
        if mask.len() != all_args.len() {
            return Err(LowerError::Internal(format!(
                "{context} to `{callee_name}` has mismatched arg mask length (mask={}, call_args={})",
                mask.len(),
                all_args.len()
            )));
        }
        for (keep, arg) in mask.iter().zip(all_args) {
            if !*keep {
                continue;
            }
            args.push(lower_value(ctx, arg)?);
        }
    } else {
        // Fallback for callees without a declared signature/mask (e.g. externs).
        for &arg in regular_args {
            let arg_ty = ctx
                .body
                .values
                .get(arg.index())
                .ok_or_else(|| LowerError::Internal("unknown call argument".to_string()))?
                .ty;
            if is_erased_runtime_ty(ctx.db, ctx.target_layout, arg_ty) {
                continue;
            }
            args.push(lower_value(ctx, arg)?);
        }
        for &effect_arg in effect_args {
            let arg_ty = ctx
                .body
                .values
                .get(effect_arg.index())
                .ok_or_else(|| LowerError::Internal("unknown call effect argument".to_string()))?
                .ty;
            if is_erased_runtime_ty(ctx.db, ctx.target_layout, arg_ty) {
                continue;
            }
            args.push(lower_value(ctx, effect_arg)?);
        }
    }

    let expected_argc = ctx
        .fb
        .module_builder
        .ctx
        .func_sig(func_ref, |sig| sig.args().len());
    if args.len() > expected_argc {
        return Err(LowerError::Internal(format!(
            "{context} to `{callee_name}` has too many args (got {}, expected {expected_argc})",
            args.len()
        )));
    }
    while args.len() < expected_argc {
        args.push(ctx.fb.make_imm_value(I256::zero()));
    }

    Ok(args)
}

/// Lower a MIR value to a Sonatina value.
fn lower_value<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    value_id: mir::ValueId,
) -> Result<ValueId, LowerError> {
    let value_data = ctx.body.values.get(value_id.index()).ok_or_else(|| {
        LowerError::Internal(format!("unknown MIR value id {}", value_id.index()))
    })?;

    // Note: We intentionally avoid caching lowered MIR values across the function.
    //
    // Sonatina's SSA builder may rewrite/remove placeholder `phi`s during sealing; caching a value
    // that (transitively) comes from `use_var` can leave us holding a `ValueId` whose defining
    // instruction was removed from the layout, producing malformed IR.
    //
    // This can be revisited once we have a robust notion of which MIR values are stable across
    // blocks after SSA sealing.
    lower_value_origin(ctx, value_data)
}

/// Lower a MIR value origin to a Sonatina value.
fn lower_value_origin<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    value_data: &mir::ValueData<'db>,
) -> Result<ValueId, LowerError> {
    use mir::ValueOrigin;
    let origin = &value_data.origin;

    match origin {
        ValueOrigin::Synthetic(syn) => match syn {
            SyntheticValue::Int(n) => {
                let i256_val = biguint_to_i256(n);
                Ok(ctx.fb.make_imm_value(i256_val))
            }
            SyntheticValue::Bool(b) => {
                let val = if *b { I256::one() } else { I256::zero() };
                Ok(ctx.fb.make_imm_value(val))
            }
            SyntheticValue::Bytes(bytes) => {
                // Convert bytes to I256 (left-padded to 32 bytes)
                let i256_val = bytes_to_i256(bytes);
                Ok(ctx.fb.make_imm_value(i256_val))
            }
        },
        ValueOrigin::Local(local_id) | ValueOrigin::PlaceRoot(local_id) => {
            let var = ctx.local_vars.get(local_id).copied().ok_or_else(|| {
                LowerError::Internal(format!("SSA variable not found for local {local_id:?}"))
            })?;
            Ok(ctx.fb.use_var(var))
        }
        ValueOrigin::Unit => {
            // Unit is represented as 0
            Ok(ctx.fb.make_imm_value(I256::zero()))
        }
        ValueOrigin::Unary { op, inner } => {
            let inner_val = lower_value(ctx, *inner)?;
            lower_unary_op(ctx.fb, *op, inner_val, ctx.is)
        }
        ValueOrigin::Binary { op, lhs, rhs } => {
            let lhs_val = lower_value(ctx, *lhs)?;
            let rhs_val = lower_value(ctx, *rhs)?;
            lower_binary_op(ctx.fb, *op, lhs_val, rhs_val, ctx.is)
        }
        ValueOrigin::TransparentCast { value } => {
            // Transparent cast just passes through the inner value
            lower_value(ctx, *value)
        }
        ValueOrigin::ControlFlowResult { expr } => {
            // ControlFlowResult values should be converted to Local values during MIR lowering.
            // If we reach here, it means MIR lowering didn't properly handle this case.
            Err(LowerError::Internal(format!(
                "ControlFlowResult value reached codegen without being converted to Local (expr={expr:?})"
            )))
        }
        ValueOrigin::PlaceRef(place) => {
            if value_data.repr.address_space().is_none()
                && let Some((_, inner_ty)) = value_data.ty.as_capability(ctx.db)
            {
                return load_place_typed(ctx, place, inner_ty);
            }
            lower_place_address(ctx, place)
        }
        ValueOrigin::MoveOut { place } => {
            if value_data.repr.address_space().is_some() {
                lower_place_address(ctx, place)
            } else {
                load_place_typed(ctx, place, value_data.ty)
            }
        }
        ValueOrigin::FieldPtr(field_ptr) => {
            let base = lower_value(ctx, field_ptr.base)?;
            if field_ptr.offset_bytes == 0 {
                Ok(base)
            } else {
                let offset = match field_ptr.addr_space {
                    AddressSpaceKind::Memory | AddressSpaceKind::Calldata => field_ptr.offset_bytes,
                    AddressSpaceKind::Storage | AddressSpaceKind::TransientStorage => {
                        field_ptr.offset_bytes / 32
                    }
                };
                let offset_val = ctx.fb.make_imm_value(I256::from(offset as u64));
                Ok(ctx
                    .fb
                    .insert_inst(Add::new(ctx.is, base, offset_val), Type::I256))
            }
        }
        ValueOrigin::FuncItem(_) => {
            // Function items are zero-sized and should never be used as runtime values.
            // If we reach here, MIR lowering failed to eliminate this usage.
            Err(LowerError::Internal(
                "FuncItem value reached codegen - should be zero-sized".to_string(),
            ))
        }
        ValueOrigin::Expr(_) => {
            // Unlowered expressions shouldn't reach codegen
            Err(LowerError::Internal(
                "unlowered expression in codegen".to_string(),
            ))
        }
    }
}

/// Lower a unary operation.
fn lower_unary_op<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    op: UnOp,
    inner: ValueId,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    match op {
        UnOp::Not => {
            // Logical not: normalize to i1, then keep Fe's word-level bool representation.
            let is_zero = fb.insert_inst(IsZero::new(is, inner), Type::I1);
            let result = fb.insert_inst(Zext::new(is, is_zero, Type::I256), Type::I256);
            Ok(result)
        }
        UnOp::Minus => {
            // Arithmetic negation
            let result = fb.insert_inst(Neg::new(is, inner), Type::I256);
            Ok(result)
        }
        UnOp::BitNot => {
            // Bitwise not
            let result = fb.insert_inst(Not::new(is, inner), Type::I256);
            Ok(result)
        }
        UnOp::Plus => {
            // Unary plus is a no-op
            Ok(inner)
        }
        UnOp::Mut | UnOp::Ref => Ok(inner),
    }
}

/// Lower a binary operation.
fn lower_binary_op<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    op: BinOp,
    lhs: ValueId,
    rhs: ValueId,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    match op {
        BinOp::Arith(arith_op) => lower_arith_op(fb, arith_op, lhs, rhs, is),
        BinOp::Comp(comp_op) => lower_comp_op(fb, comp_op, lhs, rhs, is),
        BinOp::Logical(log_op) => lower_logical_op(fb, log_op, lhs, rhs, is),
        BinOp::Index => {
            // Index operations are handled via projections, not as binary ops
            Err(LowerError::Unsupported("index binary op".to_string()))
        }
    }
}

/// Lower an arithmetic binary operation.
fn lower_arith_op<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    op: ArithBinOp,
    lhs: ValueId,
    rhs: ValueId,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    let result = match op {
        ArithBinOp::Add => fb.insert_inst(Add::new(is, lhs, rhs), Type::I256),
        ArithBinOp::Sub => fb.insert_inst(Sub::new(is, lhs, rhs), Type::I256),
        ArithBinOp::Mul => fb.insert_inst(Mul::new(is, lhs, rhs), Type::I256),
        ArithBinOp::Div => fb.insert_inst(EvmUdiv::new(is, lhs, rhs), Type::I256),
        ArithBinOp::Rem => fb.insert_inst(EvmUmod::new(is, lhs, rhs), Type::I256),
        ArithBinOp::Pow => fb.insert_inst(EvmExp::new(is, lhs, rhs), Type::I256),
        // Shl/Shr take (bits, value).
        ArithBinOp::LShift => fb.insert_inst(Shl::new(is, rhs, lhs), Type::I256),
        ArithBinOp::RShift => fb.insert_inst(Shr::new(is, rhs, lhs), Type::I256),
        ArithBinOp::BitOr => fb.insert_inst(Or::new(is, lhs, rhs), Type::I256),
        ArithBinOp::BitXor => fb.insert_inst(Xor::new(is, lhs, rhs), Type::I256),
        ArithBinOp::BitAnd => fb.insert_inst(And::new(is, lhs, rhs), Type::I256),
        ArithBinOp::Range => {
            // Range is handled at HIR level, shouldn't reach MIR binary ops
            return Err(LowerError::Unsupported("range operator".to_string()));
        }
    };
    Ok(result)
}

/// Lower a comparison binary operation.
fn lower_comp_op<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    op: CompBinOp,
    lhs: ValueId,
    rhs: ValueId,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    let result = match op {
        CompBinOp::Eq => {
            let eq = fb.insert_inst(Eq::new(is, lhs, rhs), Type::I1);
            fb.insert_inst(Zext::new(is, eq, Type::I256), Type::I256)
        }
        CompBinOp::NotEq => {
            // neq = iszero(eq(lhs, rhs))
            let eq_result = fb.insert_inst(Eq::new(is, lhs, rhs), Type::I1);
            let neq_i1 = fb.insert_inst(IsZero::new(is, eq_result), Type::I1);
            fb.insert_inst(Zext::new(is, neq_i1, Type::I256), Type::I256)
        }
        CompBinOp::Lt => {
            let lt = fb.insert_inst(Lt::new(is, lhs, rhs), Type::I1);
            fb.insert_inst(Zext::new(is, lt, Type::I256), Type::I256)
        }
        CompBinOp::LtEq => {
            // lhs <= rhs  <==>  !(lhs > rhs)
            let gt_result = fb.insert_inst(Gt::new(is, lhs, rhs), Type::I1);
            let lte_i1 = fb.insert_inst(IsZero::new(is, gt_result), Type::I1);
            fb.insert_inst(Zext::new(is, lte_i1, Type::I256), Type::I256)
        }
        CompBinOp::Gt => {
            let gt = fb.insert_inst(Gt::new(is, lhs, rhs), Type::I1);
            fb.insert_inst(Zext::new(is, gt, Type::I256), Type::I256)
        }
        CompBinOp::GtEq => {
            // lhs >= rhs  <==>  !(lhs < rhs)
            let lt_result = fb.insert_inst(Lt::new(is, lhs, rhs), Type::I1);
            let gte_i1 = fb.insert_inst(IsZero::new(is, lt_result), Type::I1);
            fb.insert_inst(Zext::new(is, gte_i1, Type::I256), Type::I256)
        }
    };
    Ok(result)
}

/// Lower a logical binary operation.
fn lower_logical_op<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    op: LogicalBinOp,
    lhs: ValueId,
    rhs: ValueId,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    // Logical ops work on booleans (I1), but we use I256 for EVM
    let result = match op {
        LogicalBinOp::And => fb.insert_inst(And::new(is, lhs, rhs), Type::I256),
        LogicalBinOp::Or => fb.insert_inst(Or::new(is, lhs, rhs), Type::I256),
    };
    Ok(result)
}

/// Lower a MIR intrinsic operation.
fn lower_intrinsic<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    op: IntrinsicOp,
    args: &[mir::ValueId],
) -> Result<Option<ValueId>, LowerError> {
    if matches!(op, IntrinsicOp::CurrentCodeRegionLen) {
        if !args.is_empty() {
            return Err(LowerError::Internal(
                "current_code_region_len requires 0 arguments".to_string(),
            ));
        }
        return Ok(Some(ctx.fb.insert_inst(
            SymSize::new(ctx.is, SymbolRef::CurrentSection),
            Type::I256,
        )));
    }

    if matches!(
        op,
        IntrinsicOp::CodeRegionOffset | IntrinsicOp::CodeRegionLen
    ) {
        let [func_item] = args else {
            return Err(LowerError::Internal(
                "code region intrinsics require 1 argument".to_string(),
            ));
        };
        let value_data = ctx
            .body
            .values
            .get(func_item.index())
            .ok_or_else(|| LowerError::Internal("unknown code region argument".to_string()))?;
        let symbol = match &value_data.origin {
            mir::ValueOrigin::FuncItem(root) => root.symbol.as_deref().ok_or_else(|| {
                LowerError::Unsupported(
                    "code region function item is missing a resolved symbol".to_string(),
                )
            })?,
            _ => {
                return Err(LowerError::Unsupported(
                    "code region intrinsic argument must be a function item".to_string(),
                ));
            }
        };

        let embed_sym = EmbedSymbol::from(symbol.to_string());
        let sym = SymbolRef::Embed(embed_sym);
        return match op {
            IntrinsicOp::CodeRegionOffset => Ok(Some(
                ctx.fb.insert_inst(SymAddr::new(ctx.is, sym), Type::I256),
            )),
            IntrinsicOp::CodeRegionLen => Ok(Some(
                ctx.fb.insert_inst(SymSize::new(ctx.is, sym), Type::I256),
            )),
            _ => unreachable!(),
        };
    }

    // Lower all arguments first
    let mut lowered_args = Vec::with_capacity(args.len());
    for &arg in args {
        let val = lower_value(ctx, arg)?;
        lowered_args.push(val);
    }

    match op {
        IntrinsicOp::AddrOf => {
            let Some(&arg) = lowered_args.first() else {
                return Err(LowerError::Internal(
                    "addr_of requires 1 argument".to_string(),
                ));
            };
            Ok(Some(arg))
        }
        IntrinsicOp::Alloc => {
            let [size] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "alloc requires 1 argument (size)".to_string(),
                ));
            };
            Ok(Some(emit_evm_malloc_word_addr(ctx.fb, *size, ctx.is)))
        }
        IntrinsicOp::Mload => {
            let Some(&addr) = lowered_args.first() else {
                return Err(LowerError::Internal(
                    "mload requires address argument".to_string(),
                ));
            };
            Ok(Some(ctx.fb.insert_inst(
                Mload::new(ctx.is, addr, Type::I256),
                Type::I256,
            )))
        }
        IntrinsicOp::Mstore => {
            let [addr, val] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "mstore requires 2 arguments".to_string(),
                ));
            };
            ctx.fb
                .insert_inst_no_result(Mstore::new(ctx.is, *addr, *val, Type::I256));
            Ok(None)
        }
        IntrinsicOp::Mstore8 => {
            let [addr, val] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "mstore8 requires 2 arguments".to_string(),
                ));
            };
            ctx.fb
                .insert_inst_no_result(EvmMstore8::new(ctx.is, *addr, *val));
            Ok(None)
        }
        IntrinsicOp::Sload => {
            let Some(&key) = lowered_args.first() else {
                return Err(LowerError::Internal(
                    "sload requires 1 argument".to_string(),
                ));
            };
            Ok(Some(
                ctx.fb.insert_inst(EvmSload::new(ctx.is, key), Type::I256),
            ))
        }
        IntrinsicOp::Sstore => {
            let [key, val] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "sstore requires 2 arguments".to_string(),
                ));
            };
            ctx.fb
                .insert_inst_no_result(EvmSstore::new(ctx.is, *key, *val));
            Ok(None)
        }
        IntrinsicOp::Calldataload => {
            let Some(&offset) = lowered_args.first() else {
                return Err(LowerError::Internal(
                    "calldataload requires 1 argument".to_string(),
                ));
            };
            Ok(Some(ctx.fb.insert_inst(
                EvmCalldataLoad::new(ctx.is, offset),
                Type::I256,
            )))
        }
        IntrinsicOp::Calldatasize => Ok(Some(
            ctx.fb.insert_inst(EvmCalldataSize::new(ctx.is), Type::I256),
        )),
        IntrinsicOp::Calldatacopy => {
            let [dst, offset, len] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "calldatacopy requires 3 arguments".to_string(),
                ));
            };
            ctx.fb
                .insert_inst_no_result(EvmCalldataCopy::new(ctx.is, *dst, *offset, *len));
            Ok(None)
        }
        IntrinsicOp::Returndatasize => Ok(Some(
            ctx.fb
                .insert_inst(EvmReturnDataSize::new(ctx.is), Type::I256),
        )),
        IntrinsicOp::Returndatacopy => {
            let [dst, offset, len] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "returndatacopy requires 3 arguments".to_string(),
                ));
            };
            ctx.fb
                .insert_inst_no_result(EvmReturnDataCopy::new(ctx.is, *dst, *offset, *len));
            Ok(None)
        }
        IntrinsicOp::Codesize => Ok(Some(
            ctx.fb.insert_inst(EvmCodeSize::new(ctx.is), Type::I256),
        )),
        IntrinsicOp::Codecopy => {
            let [dst, offset, len] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "codecopy requires 3 arguments".to_string(),
                ));
            };
            ctx.fb
                .insert_inst_no_result(EvmCodeCopy::new(ctx.is, *dst, *offset, *len));
            Ok(None)
        }
        IntrinsicOp::CodeRegionOffset
        | IntrinsicOp::CodeRegionLen
        | IntrinsicOp::CurrentCodeRegionLen => {
            unreachable!("code region intrinsics are handled in the early return above")
        }
        IntrinsicOp::Keccak => {
            let [addr, len] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "keccak requires 2 arguments".to_string(),
                ));
            };
            Ok(Some(ctx.fb.insert_inst(
                EvmKeccak256::new(ctx.is, *addr, *len),
                Type::I256,
            )))
        }
        IntrinsicOp::Addmod => {
            let [a, b, m] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "addmod requires 3 arguments".to_string(),
                ));
            };
            Ok(Some(ctx.fb.insert_inst(
                EvmAddMod::new(ctx.is, *a, *b, *m),
                Type::I256,
            )))
        }
        IntrinsicOp::Mulmod => {
            let [a, b, m] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "mulmod requires 3 arguments".to_string(),
                ));
            };
            Ok(Some(ctx.fb.insert_inst(
                EvmMulMod::new(ctx.is, *a, *b, *m),
                Type::I256,
            )))
        }
        IntrinsicOp::Caller => Ok(Some(ctx.fb.insert_inst(EvmCaller::new(ctx.is), Type::I256))),
        IntrinsicOp::ReturnData | IntrinsicOp::Revert => Err(LowerError::Internal(
            "terminating intrinsic must be lowered as Terminator::TerminatingCall".to_string(),
        )),
    }
}

/// Convert a BigUint to I256.
fn biguint_to_i256(n: &BigUint) -> I256 {
    // Convert to bytes and then to I256
    let bytes = n.to_bytes_be();
    if bytes.is_empty() {
        return I256::zero();
    }
    // Pad to 32 bytes (right-aligned for big-endian)
    let mut padded = [0u8; 32];
    let start = 32usize.saturating_sub(bytes.len());
    let copy_len = bytes.len().min(32);
    padded[start..start + copy_len].copy_from_slice(&bytes[bytes.len() - copy_len..]);
    I256::from_be_bytes(&padded)
}

/// Convert bytes to I256.
///
/// Matches Yul's `0x...` literal semantics by interpreting the bytes as a big-endian integer.
fn bytes_to_i256(bytes: &[u8]) -> I256 {
    let mut padded = [0u8; 32];
    let copy_len = bytes.len().min(32);
    let start = 32 - copy_len;
    padded[start..start + copy_len].copy_from_slice(&bytes[bytes.len() - copy_len..]);
    I256::from_be_bytes(&padded)
}

/// Returns the Sonatina Type for a Fe primitive type, or None if not a sub-word type.
fn prim_to_sonatina_type(prim: PrimTy) -> Option<Type> {
    match prim {
        PrimTy::Bool => Some(Type::I1),
        PrimTy::U8 | PrimTy::I8 => Some(Type::I8),
        PrimTy::U16 | PrimTy::I16 => Some(Type::I16),
        PrimTy::U32 | PrimTy::I32 => Some(Type::I32),
        PrimTy::U64 | PrimTy::I64 => Some(Type::I64),
        PrimTy::U128 | PrimTy::I128 => Some(Type::I128),
        // Full-width types don't need conversion
        PrimTy::U256
        | PrimTy::I256
        | PrimTy::Usize
        | PrimTy::Isize
        | PrimTy::View
        | PrimTy::BorrowMut
        | PrimTy::BorrowRef => None,
        // Non-scalar types
        PrimTy::String | PrimTy::Array | PrimTy::Tuple(_) | PrimTy::Ptr => None,
    }
}

/// Returns true if the primitive type is signed.
fn prim_is_signed(prim: PrimTy) -> bool {
    matches!(
        prim,
        PrimTy::I8
            | PrimTy::I16
            | PrimTy::I32
            | PrimTy::I64
            | PrimTy::I128
            | PrimTy::I256
            | PrimTy::Isize
    )
}

/// Applies `from_word` conversion after loading a value.
///
/// This mirrors the stdlib `WordRepr::from_word` semantics:
/// - bool: convert to 0 or 1
/// - unsigned sub-word: mask to appropriate width
/// - signed sub-word: mask then sign-extend
fn apply_from_word<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    raw_value: ValueId,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> ValueId {
    let ty = mir::repr::word_conversion_leaf_ty(db, ty);
    let base_ty = ty.base_ty(db);

    if let TyData::TyBase(TyBase::Prim(prim)) = base_ty.data(db) {
        match prim {
            PrimTy::Bool => {
                // bool: value != 0 → 0 or 1
                let zero = fb.make_imm_value(I256::zero());
                let cmp = Ne::new(is, raw_value, zero);
                let bool_val = fb.insert_inst(cmp, Type::I1);
                // Extend back to I256
                let ext = Zext::new(is, bool_val, Type::I256);
                fb.insert_inst(ext, Type::I256)
            }
            _ => {
                if let Some(small_ty) = prim_to_sonatina_type(*prim) {
                    // Truncate to small type then extend back
                    let trunc = Trunc::new(is, raw_value, small_ty);
                    let truncated = fb.insert_inst(trunc, small_ty);

                    if prim_is_signed(*prim) {
                        let ext = Sext::new(is, truncated, Type::I256);
                        fb.insert_inst(ext, Type::I256)
                    } else {
                        let ext = Zext::new(is, truncated, Type::I256);
                        fb.insert_inst(ext, Type::I256)
                    }
                } else {
                    // Full-width type, no conversion needed
                    raw_value
                }
            }
        }
    } else {
        // Non-primitive type, no conversion
        raw_value
    }
}

/// Applies `to_word` conversion before storing a value.
///
/// This mirrors the stdlib `WordRepr::to_word` semantics:
/// - bool: convert to 0 or 1
/// - unsigned sub-word: mask to appropriate width
/// - signed: no conversion needed (already sign-extended)
fn apply_to_word<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    value: ValueId,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> ValueId {
    let ty = mir::repr::word_conversion_leaf_ty(db, ty);
    let base_ty = ty.base_ty(db);

    if let TyData::TyBase(TyBase::Prim(prim)) = base_ty.data(db) {
        match prim {
            PrimTy::Bool => {
                // bool: value != 0 → 0 or 1
                let zero = fb.make_imm_value(I256::zero());
                let cmp = Ne::new(is, value, zero);
                let bool_val = fb.insert_inst(cmp, Type::I1);
                let ext = Zext::new(is, bool_val, Type::I256);
                fb.insert_inst(ext, Type::I256)
            }
            PrimTy::U8 | PrimTy::U16 | PrimTy::U32 | PrimTy::U64 | PrimTy::U128 => {
                // Unsigned: truncate then zero-extend to mask high bits
                if let Some(small_ty) = prim_to_sonatina_type(*prim) {
                    let trunc = Trunc::new(is, value, small_ty);
                    let truncated = fb.insert_inst(trunc, small_ty);
                    let ext = Zext::new(is, truncated, Type::I256);
                    fb.insert_inst(ext, Type::I256)
                } else {
                    value
                }
            }
            // Signed types and full-width types don't need conversion
            _ => value,
        }
    } else {
        // Non-primitive type, no conversion
        value
    }
}

/// Maps a Fe type to a sonatina struct/array type for GEP-based addressing.
///
/// Returns `Some(Type)` for types that can be represented as sonatina compound types
/// (structs, tuples, arrays of such). Returns `None` for types where we should fall
/// back to manual offset arithmetic (enums, zero-sized types, plain scalars).
///
/// Uses a cache to avoid creating duplicate struct type definitions. The cache is keyed
/// by the Fe `TyId` debug representation (salsa-interned, so stable within a session).
fn fe_ty_to_sonatina<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
    cache: &mut FxHashMap<String, Option<Type>>,
    name_counter: &mut usize,
) -> Option<Type> {
    let cache_key = format!("{ty:?}");
    if let Some(cached) = cache.get(&cache_key) {
        return *cached;
    }

    let result = fe_ty_to_sonatina_inner(fb, db, target_layout, ty, cache, name_counter);
    cache.insert(cache_key, result);
    result
}

fn fe_ty_to_sonatina_inner<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
    cache: &mut FxHashMap<String, Option<Type>>,
    name_counter: &mut usize,
) -> Option<Type> {
    if is_erased_runtime_ty(db, target_layout, ty) {
        return Some(Type::Unit);
    }

    let base_ty = ty.base_ty(db);
    match base_ty.data(db) {
        TyData::TyBase(TyBase::Prim(prim)) => match prim {
            // Scalars: all map to I256 on EVM
            PrimTy::Bool
            | PrimTy::U8
            | PrimTy::I8
            | PrimTy::U16
            | PrimTy::I16
            | PrimTy::U32
            | PrimTy::I32
            | PrimTy::U64
            | PrimTy::I64
            | PrimTy::U128
            | PrimTy::I128
            | PrimTy::U256
            | PrimTy::I256
            | PrimTy::Usize
            | PrimTy::Isize
            | PrimTy::Ptr
            | PrimTy::View
            | PrimTy::BorrowMut
            | PrimTy::BorrowRef => Some(Type::I256),
            PrimTy::String => None,
            PrimTy::Tuple(_) => {
                let field_tys = ty.field_types(db);
                if field_tys.is_empty() {
                    return Some(Type::Unit);
                }
                let mut sonatina_fields = Vec::with_capacity(field_tys.len());
                for ft in &field_tys {
                    sonatina_fields.push(fe_ty_to_sonatina(
                        fb,
                        db,
                        target_layout,
                        *ft,
                        cache,
                        name_counter,
                    )?);
                }
                let id = *name_counter;
                *name_counter += 1;
                Some(fb.declare_struct_type(&format!("__fe_tuple_{id}"), &sonatina_fields, false))
            }
            PrimTy::Array => {
                let elem_ty = layout::array_elem_ty(db, ty)?;
                let len = layout::array_len(db, ty)?;
                let sonatina_elem =
                    fe_ty_to_sonatina(fb, db, target_layout, elem_ty, cache, name_counter)?;
                Some(fb.declare_array_type(sonatina_elem, len))
            }
        },
        TyData::TyBase(TyBase::Adt(adt_def)) => {
            match adt_def.adt_ref(db) {
                AdtRef::Struct(_) => {
                    let field_tys = ty.field_types(db);
                    let mut sonatina_fields = Vec::with_capacity(field_tys.len());
                    for ft in &field_tys {
                        sonatina_fields.push(fe_ty_to_sonatina(
                            fb,
                            db,
                            target_layout,
                            *ft,
                            cache,
                            name_counter,
                        )?);
                    }
                    let name = adt_def
                        .adt_ref(db)
                        .name(db)
                        .map(|id| id.data(db).to_string())
                        .unwrap_or_else(|| "anon".to_string());
                    let id = *name_counter;
                    *name_counter += 1;
                    Some(fb.declare_struct_type(
                        &format!("__fe_{name}_{id}"),
                        &sonatina_fields,
                        false,
                    ))
                }
                // Enums: fall back to manual arithmetic
                AdtRef::Enum(_) => None,
            }
        }
        TyData::TyBase(TyBase::Contract(_)) | TyData::TyBase(TyBase::Func(_)) => Some(Type::Unit),
        _ => None,
    }
}

/// Checks whether a projection chain is eligible for GEP-based addressing.
///
/// Returns true when all projections are Field or Index (no VariantField, Discriminant, or Deref).
fn projections_eligible_for_gep(place: &Place<'_>) -> bool {
    place
        .projection
        .iter()
        .all(|p| matches!(p, Projection::Field(_) | Projection::Index(_)))
}

/// Computes the address for a place by walking the projection path.
///
/// For memory, computes byte offsets. For storage, computes slot offsets.
/// Returns a Sonatina ValueId representing the final address.
fn lower_place_address<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
) -> Result<ValueId, LowerError> {
    let base_val = lower_value(ctx, place.base)?;

    if place.projection.is_empty() {
        return Ok(base_val);
    }

    // Get the base value's type to navigate projections
    let base_value = ctx.body.values.get(place.base.index()).ok_or_else(|| {
        LowerError::Internal(format!(
            "unknown MIR place base value {}",
            place.base.index()
        ))
    })?;
    let current_ty = base_value.ty;
    if is_erased_runtime_ty(ctx.db, ctx.target_layout, current_ty) {
        return Ok(base_val);
    }

    let is_slot_addressed = matches!(
        ctx.body.place_address_space(place),
        AddressSpaceKind::Storage | AddressSpaceKind::TransientStorage
    );

    // Use GEP for memory-addressed places where all projections are Field or Index
    if !is_slot_addressed
        && projections_eligible_for_gep(place)
        && let Some(sonatina_ty) = fe_ty_to_sonatina(
            ctx.fb,
            ctx.db,
            ctx.target_layout,
            current_ty,
            ctx.gep_type_cache,
            ctx.gep_name_counter,
        )
    {
        return lower_place_address_gep(ctx, place, base_val, current_ty, sonatina_ty);
    }

    // Fall back to manual offset arithmetic
    lower_place_address_arithmetic(ctx, place, base_val, current_ty, is_slot_addressed)
}

/// GEP-based place address computation for memory-addressed struct/array paths.
fn lower_place_address_gep<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
    base_val: ValueId,
    base_fe_ty: hir::analysis::ty::ty_def::TyId<'db>,
    base_sonatina_ty: Type,
) -> Result<ValueId, LowerError> {
    let ptr_ty = ctx.fb.ptr_type(base_sonatina_ty);

    // IntToPtr: cast I256 base address to typed pointer
    let typed_ptr = ctx
        .fb
        .insert_inst(IntToPtr::new(ctx.is, base_val, ptr_ty), ptr_ty);

    // Build GEP index list, tracking types through the chain
    let mut gep_values: SmallVec<[ValueId; 8]> = SmallVec::new();
    gep_values.push(typed_ptr);

    // Initial dereference index (standard GEP convention: index 0 dereferences the pointer)
    let zero = ctx.fb.make_imm_value(I256::zero());
    gep_values.push(zero);

    let mut current_fe_ty = base_fe_ty;
    let mut current_sonatina_ty = base_sonatina_ty;

    for proj in place.projection.iter() {
        match proj {
            Projection::Field(field_idx) => {
                let idx_val = ctx.fb.make_imm_value(I256::from(*field_idx as u64));
                gep_values.push(idx_val);

                // Navigate Fe type
                let field_types = current_fe_ty.field_types(ctx.db);
                current_fe_ty = *field_types.get(*field_idx).ok_or_else(|| {
                    LowerError::Unsupported(format!("gep: field {field_idx} out of bounds"))
                })?;

                // Navigate sonatina type to the field's type
                current_sonatina_ty =
                    sonatina_struct_field_ty(ctx, current_sonatina_ty, *field_idx)?;
            }
            Projection::Index(idx_source) => {
                let idx_val = match idx_source {
                    IndexSource::Constant(idx) => ctx.fb.make_imm_value(I256::from(*idx as u64)),
                    IndexSource::Dynamic(value_id) => lower_value(ctx, *value_id)?,
                };
                gep_values.push(idx_val);

                // Navigate Fe type
                current_fe_ty = layout::array_elem_ty(ctx.db, current_fe_ty).ok_or_else(|| {
                    LowerError::Unsupported("gep: array index on non-array type".to_string())
                })?;

                // Navigate sonatina type to array element
                current_sonatina_ty = sonatina_array_elem_ty(ctx, current_sonatina_ty)?;
            }
            _ => unreachable!("projections_eligible_for_gep ensures only Field/Index"),
        }
    }

    // The GEP result is a pointer to the final element type
    let result_ptr_ty = ctx.fb.ptr_type(current_sonatina_ty);
    let gep_result = ctx
        .fb
        .insert_inst(Gep::new(ctx.is, gep_values), result_ptr_ty);

    // PtrToInt: cast back to I256 for mload/mstore
    let result = ctx
        .fb
        .insert_inst(PtrToInt::new(ctx.is, gep_result, Type::I256), Type::I256);

    Ok(result)
}

/// Resolves the sonatina type of a struct field by index.
fn sonatina_struct_field_ty<C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, '_, C>,
    struct_ty: Type,
    field_idx: usize,
) -> Result<Type, LowerError> {
    let fields = ctx
        .fb
        .module_builder
        .ctx
        .with_ty_store(|s| s.struct_def(struct_ty).map(|sd| sd.fields.clone()));
    match fields {
        Some(f) => f.get(field_idx).copied().ok_or_else(|| {
            LowerError::Internal(format!(
                "gep: sonatina struct field {field_idx} out of bounds"
            ))
        }),
        None => Err(LowerError::Internal(
            "gep: expected sonatina struct type for Field projection".to_string(),
        )),
    }
}

/// Resolves the sonatina element type of an array type.
fn sonatina_array_elem_ty<C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, '_, C>,
    array_ty: Type,
) -> Result<Type, LowerError> {
    let elem = ctx
        .fb
        .module_builder
        .ctx
        .with_ty_store(|s| s.array_def(array_ty).map(|(elem, _len)| elem));
    match elem {
        Some(e) => Ok(e),
        None => Err(LowerError::Internal(
            "gep: expected sonatina array type for Index projection".to_string(),
        )),
    }
}

/// Manual offset arithmetic path for place address computation.
/// Used for storage-addressed places and any memory path with enum projections.
fn lower_place_address_arithmetic<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
    mut base_val: ValueId,
    mut current_ty: hir::analysis::ty::ty_def::TyId<'db>,
    is_slot_addressed: bool,
) -> Result<ValueId, LowerError> {
    let mut total_offset: usize = 0;

    for proj in place.projection.iter() {
        match proj {
            Projection::Field(field_idx) => {
                // Use slot-based offsets for storage, byte-based for memory
                total_offset += if is_slot_addressed {
                    layout::field_offset_slots(ctx.db, current_ty, *field_idx)
                } else {
                    layout::field_offset_memory_in(
                        ctx.db,
                        ctx.target_layout,
                        current_ty,
                        *field_idx,
                    )
                };
                // Update current type to the field's type
                let field_types = current_ty.field_types(ctx.db);
                current_ty = *field_types.get(*field_idx).ok_or_else(|| {
                    LowerError::Unsupported(format!("projection: field {field_idx} out of bounds"))
                })?;
            }
            Projection::VariantField {
                variant,
                enum_ty,
                field_idx,
            } => {
                // Skip discriminant then compute field offset
                if is_slot_addressed {
                    total_offset += 1; // discriminant takes one slot
                    total_offset +=
                        layout::variant_field_offset_slots(ctx.db, *enum_ty, *variant, *field_idx);
                } else {
                    total_offset += ctx.target_layout.discriminant_size_bytes;
                    total_offset += layout::variant_field_offset_memory_in(
                        ctx.db,
                        ctx.target_layout,
                        *enum_ty,
                        *variant,
                        *field_idx,
                    );
                }
                // Update current type to the field's type
                let ctor = hir::analysis::ty::simplified_pattern::ConstructorKind::Variant(
                    *variant, *enum_ty,
                );
                let field_types = ctor.field_types(ctx.db);
                current_ty = *field_types.get(*field_idx).ok_or_else(|| {
                    LowerError::Unsupported(format!(
                        "projection: variant field {field_idx} out of bounds"
                    ))
                })?;
            }
            Projection::Discriminant => {
                // Discriminant is at offset 0, just update the type
                current_ty = hir::analysis::ty::ty_def::TyId::new(
                    ctx.db,
                    hir::analysis::ty::ty_def::TyData::TyBase(
                        hir::analysis::ty::ty_def::TyBase::Prim(
                            hir::analysis::ty::ty_def::PrimTy::U256,
                        ),
                    ),
                );
            }
            Projection::Index(idx_source) => {
                let stride = if is_slot_addressed {
                    layout::array_elem_stride_slots(ctx.db, current_ty)
                } else {
                    layout::array_elem_stride_memory_in(ctx.db, ctx.target_layout, current_ty)
                }
                .ok_or_else(|| {
                    LowerError::Unsupported("projection: array index on non-array type".to_string())
                })?;

                match idx_source {
                    IndexSource::Constant(idx) => {
                        total_offset += idx * stride;
                    }
                    IndexSource::Dynamic(value_id) => {
                        // Flush accumulated offset first
                        if total_offset != 0 {
                            let offset_val = ctx.fb.make_imm_value(I256::from(total_offset as u64));
                            base_val = ctx
                                .fb
                                .insert_inst(Add::new(ctx.is, base_val, offset_val), Type::I256);
                            total_offset = 0;
                        }
                        // Compute dynamic index offset: idx * stride
                        let idx_val = lower_value(ctx, *value_id)?;
                        let offset_val = if stride == 1 {
                            idx_val
                        } else {
                            let stride_val = ctx.fb.make_imm_value(I256::from(stride as u64));
                            ctx.fb
                                .insert_inst(Mul::new(ctx.is, idx_val, stride_val), Type::I256)
                        };
                        base_val = ctx
                            .fb
                            .insert_inst(Add::new(ctx.is, base_val, offset_val), Type::I256);
                    }
                }

                // Update current type to element type
                let elem_ty = layout::array_elem_ty(ctx.db, current_ty).ok_or_else(|| {
                    LowerError::Unsupported("projection: array index on non-array type".to_string())
                })?;
                current_ty = elem_ty;
            }
            Projection::Deref => {
                return Err(LowerError::Unsupported(
                    "projection: pointer dereference not implemented".to_string(),
                ));
            }
        }
    }

    // Add any remaining accumulated offset
    if total_offset != 0 {
        let offset_val = ctx.fb.make_imm_value(I256::from(total_offset as u64));
        base_val = ctx
            .fb
            .insert_inst(Add::new(ctx.is, base_val, offset_val), Type::I256);
    }

    Ok(base_val)
}

/// Lower a block terminator.
///
/// If `is_entry` is true, `Return` terminators emit `evm_stop` instead of internal `Return`,
/// since the entry function is executed directly by the EVM and must halt with an EVM opcode.
pub(super) fn lower_terminator<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    term: &mir::Terminator<'db>,
) -> Result<(), LowerError> {
    use mir::Terminator;

    match term {
        Terminator::Return { value: ret_val, .. } => {
            if ctx.is_entry {
                // Entry function: emit evm_stop to halt EVM execution.
                // Any return value is ignored since the entry function should have
                // already written return data via evm_return if needed.
                ctx.fb.insert_inst_no_result(EvmStop::new(ctx.is));
            } else {
                // Non-entry function: emit internal Return for function call semantics.
                let ret_sonatina = if let Some(v) = ret_val {
                    Some(lower_value(ctx, *v)?)
                } else {
                    None
                };
                ctx.fb
                    .insert_inst_no_result(Return::new(ctx.is, ret_sonatina));
            }
        }
        Terminator::Goto { target, .. } => {
            let target_block = ctx.block_map[target];
            ctx.fb
                .insert_inst_no_result(Jump::new(ctx.is, target_block));
        }
        Terminator::Branch {
            cond,
            then_bb,
            else_bb,
            ..
        } => {
            let cond_val = lower_value(ctx, *cond)?;
            let cond_i1 = condition_to_i1(ctx.fb, cond_val, ctx.is);
            let then_block = ctx.block_map[then_bb];
            let else_block = ctx.block_map[else_bb];
            // Br: cond, nz_dest (then), z_dest (else)
            ctx.fb
                .insert_inst_no_result(Br::new(ctx.is, cond_i1, then_block, else_block));
        }
        Terminator::Switch {
            discr,
            targets,
            default,
            ..
        } => {
            let discr_val = lower_value(ctx, *discr)?;
            let default_block = ctx.block_map[default];

            // NOTE: Sonatina's current EVM backend `BrTable` lowering is broken (it does not
            // compare against the scrutinee). Lower to a chain of `Eq` + `Br` instead.
            if targets.is_empty() {
                ctx.fb
                    .insert_inst_no_result(Jump::new(ctx.is, default_block));
                return Ok(());
            }

            let mut cases = Vec::with_capacity(targets.len());
            for target in targets {
                let value = ctx
                    .fb
                    .make_imm_value(biguint_to_i256(&target.value.as_biguint()));
                let dest = ctx.block_map[&target.block];
                cases.push((value, dest));
            }

            // Create additional compare blocks as needed.
            let mut compare_blocks = Vec::with_capacity(cases.len().saturating_sub(1));
            for _ in 0..cases.len().saturating_sub(1) {
                compare_blocks.push(ctx.fb.append_block());
            }

            for (case_idx, (case_value, case_dest)) in cases.into_iter().enumerate() {
                if case_idx > 0 {
                    ctx.fb.switch_to_block(compare_blocks[case_idx - 1]);
                }

                let else_dest = if case_idx + 1 < compare_blocks.len() + 1 {
                    compare_blocks[case_idx]
                } else {
                    default_block
                };
                let cond = ctx
                    .fb
                    .insert_inst(Eq::new(ctx.is, discr_val, case_value), Type::I1);
                ctx.fb
                    .insert_inst_no_result(Br::new(ctx.is, cond, case_dest, else_dest));
            }
        }
        Terminator::TerminatingCall { call, .. } => match call {
            mir::TerminatingCall::Call(call) => {
                let callee_name = call.resolved_name.as_ref().ok_or_else(|| {
                    LowerError::Unsupported("terminating call without resolved name".to_string())
                })?;

                if call.effect_args.is_empty() {
                    match callee_name.as_str() {
                        "stop" => {
                            if !call.args.is_empty() {
                                return Err(LowerError::Internal(
                                    "stop takes no arguments".to_string(),
                                ));
                            }
                            ctx.fb.insert_inst_no_result(EvmStop::new(ctx.is));
                            return Ok(());
                        }
                        "selfdestruct" => {
                            let [addr] = call.args.as_slice() else {
                                return Err(LowerError::Internal(
                                    "selfdestruct requires 1 argument".to_string(),
                                ));
                            };
                            let addr = lower_value(ctx, *addr)?;
                            ctx.fb
                                .insert_inst_no_result(EvmSelfDestruct::new(ctx.is, addr));
                            return Ok(());
                        }
                        _ => {}
                    }
                }

                let func_ref = ctx.name_map.get(callee_name).ok_or_else(|| {
                    LowerError::Internal(format!("unknown function: {callee_name}"))
                })?;

                let args = lower_call_args(
                    ctx,
                    callee_name,
                    &call.args,
                    &call.effect_args,
                    *func_ref,
                    "terminating call",
                )?;

                ctx.fb
                    .insert_inst_no_result(Call::new(ctx.is, *func_ref, args.into()));
                ctx.fb.insert_inst_no_result(EvmInvalid::new(ctx.is));
            }
            mir::TerminatingCall::Intrinsic { op, args } => {
                let mut lowered_args = Vec::with_capacity(args.len());
                for &arg in args {
                    lowered_args.push(lower_value(ctx, arg)?);
                }
                match op {
                    IntrinsicOp::ReturnData => {
                        let [addr, len] = lowered_args.as_slice() else {
                            return Err(LowerError::Internal(
                                "return_data requires 2 arguments".to_string(),
                            ));
                        };
                        ctx.fb
                            .insert_inst_no_result(EvmReturn::new(ctx.is, *addr, *len));
                    }
                    IntrinsicOp::Revert => {
                        let [addr, len] = lowered_args.as_slice() else {
                            return Err(LowerError::Internal(
                                "revert requires 2 arguments".to_string(),
                            ));
                        };
                        ctx.fb
                            .insert_inst_no_result(EvmRevert::new(ctx.is, *addr, *len));
                    }
                    _ => {
                        return Err(LowerError::Unsupported(format!(
                            "terminating intrinsic: {:?}",
                            op
                        )));
                    }
                }
            }
        },
        Terminator::Unreachable { .. } => {
            // Emit INVALID opcode (0xFE) - this consumes all gas and reverts
            ctx.fb.insert_inst_no_result(EvmInvalid::new(ctx.is));
        }
    }

    Ok(())
}

fn lower_alloc<C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, '_, C>,
    dest: mir::LocalId,
    address_space: AddressSpaceKind,
) -> Result<ValueId, LowerError> {
    if !matches!(address_space, AddressSpaceKind::Memory) {
        return Err(LowerError::Unsupported(
            "alloc is only supported for memory".to_string(),
        ));
    }

    let alloc_ty = ctx
        .body
        .locals
        .get(dest.index())
        .ok_or_else(|| LowerError::Internal(format!("unknown local: {dest:?}")))?
        .ty;

    let Some(size_bytes) = layout::ty_memory_size_or_word_in(ctx.db, ctx.target_layout, alloc_ty)
    else {
        return Err(LowerError::Unsupported(format!(
            "cannot determine allocation size for `{}`",
            alloc_ty.pretty_print(ctx.db)
        )));
    };

    if size_bytes == 0 {
        return Ok(ctx.fb.make_imm_value(I256::zero()));
    }

    // Use Sonatina's EvmMalloc to allocate memory. This delegates memory management
    // to Sonatina's codegen, avoiding conflicts with its stack frame handling.
    let size_val = ctx.fb.make_imm_value(I256::from(size_bytes as u64));
    let ptr = emit_evm_malloc_word_addr(ctx.fb, size_val, ctx.is);

    Ok(ptr)
}

fn lower_store_inst<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
    value: mir::ValueId,
) -> Result<(), LowerError> {
    let value_data = ctx
        .body
        .values
        .get(value.index())
        .ok_or_else(|| LowerError::Internal(format!("unknown value: {value:?}")))?;
    let value_ty = value_data.ty;
    if is_erased_runtime_ty(ctx.db, ctx.target_layout, value_ty) {
        return Ok(());
    }

    if value_data.repr.is_ref() {
        let src_place = mir::ir::Place::new(value, mir::ir::MirProjectionPath::new());
        deep_copy_from_places(ctx, place, &src_place, value_ty)?;
        return Ok(());
    }

    let raw_val = lower_value(ctx, value)?;
    let val = apply_to_word(ctx.fb, ctx.db, raw_val, value_ty, ctx.is);
    store_word_to_place(ctx, place, val)
}

fn store_word_to_place<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
    val: ValueId,
) -> Result<(), LowerError> {
    let addr = lower_place_address(ctx, place)?;
    match ctx.body.place_address_space(place) {
        AddressSpaceKind::Memory => {
            ctx.fb
                .insert_inst_no_result(Mstore::new(ctx.is, addr, val, Type::I256));
        }
        AddressSpaceKind::Storage => {
            ctx.fb
                .insert_inst_no_result(EvmSstore::new(ctx.is, addr, val));
        }
        AddressSpaceKind::TransientStorage => {
            ctx.fb
                .insert_inst_no_result(EvmTstore::new(ctx.is, addr, val));
        }
        AddressSpaceKind::Calldata => {
            return Err(LowerError::Unsupported("store to calldata".to_string()));
        }
    }
    Ok(())
}

fn load_place_typed<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
    loaded_ty: hir::analysis::ty::ty_def::TyId<'db>,
) -> Result<ValueId, LowerError> {
    if is_erased_runtime_ty(ctx.db, ctx.target_layout, loaded_ty) {
        return Ok(ctx.fb.make_imm_value(I256::zero()));
    }

    let addr = lower_place_address(ctx, place)?;
    let raw = match ctx.body.place_address_space(place) {
        AddressSpaceKind::Memory => ctx
            .fb
            .insert_inst(Mload::new(ctx.is, addr, Type::I256), Type::I256),
        AddressSpaceKind::Storage => ctx.fb.insert_inst(EvmSload::new(ctx.is, addr), Type::I256),
        AddressSpaceKind::TransientStorage => {
            ctx.fb.insert_inst(EvmTload::new(ctx.is, addr), Type::I256)
        }
        AddressSpaceKind::Calldata => ctx
            .fb
            .insert_inst(EvmCalldataLoad::new(ctx.is, addr), Type::I256),
    };
    Ok(apply_from_word(ctx.fb, ctx.db, raw, loaded_ty, ctx.is))
}

fn deep_copy_from_places<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    dst_place: &Place<'db>,
    src_place: &Place<'db>,
    value_ty: hir::analysis::ty::ty_def::TyId<'db>,
) -> Result<(), LowerError> {
    if is_erased_runtime_ty(ctx.db, ctx.target_layout, value_ty) {
        return Ok(());
    }

    if value_ty.is_array(ctx.db) {
        let Some(len) = layout::array_len(ctx.db, value_ty) else {
            return Err(LowerError::Unsupported(
                "array store requires a constant length".into(),
            ));
        };
        let elem_ty = layout::array_elem_ty(ctx.db, value_ty)
            .ok_or_else(|| LowerError::Unsupported("array store requires element type".into()))?;
        for idx in 0..len {
            let dst_elem = extend_place(dst_place, Projection::Index(IndexSource::Constant(idx)));
            let src_elem = extend_place(src_place, Projection::Index(IndexSource::Constant(idx)));
            deep_copy_from_places(ctx, &dst_elem, &src_elem, elem_ty)?;
        }
        return Ok(());
    }

    if value_ty.field_count(ctx.db) > 0 {
        for (field_idx, field_ty) in value_ty.field_types(ctx.db).iter().copied().enumerate() {
            let dst_field = extend_place(dst_place, Projection::Field(field_idx));
            let src_field = extend_place(src_place, Projection::Field(field_idx));
            deep_copy_from_places(ctx, &dst_field, &src_field, field_ty)?;
        }
        return Ok(());
    }

    if value_ty
        .adt_ref(ctx.db)
        .is_some_and(|adt| matches!(adt, AdtRef::Enum(_)))
    {
        return deep_copy_enum_from_places(ctx, dst_place, src_place, value_ty);
    }

    let loaded = load_place_typed(ctx, src_place, value_ty)?;
    let stored = apply_to_word(ctx.fb, ctx.db, loaded, value_ty, ctx.is);
    store_word_to_place(ctx, dst_place, stored)
}

fn deep_copy_enum_from_places<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    dst_place: &Place<'db>,
    src_place: &Place<'db>,
    enum_ty: hir::analysis::ty::ty_def::TyId<'db>,
) -> Result<(), LowerError> {
    let Some(adt_def) = enum_ty.adt_def(ctx.db) else {
        return Err(LowerError::Unsupported(
            "enum store requires enum adt".into(),
        ));
    };
    let AdtRef::Enum(enm) = adt_def.adt_ref(ctx.db) else {
        return Err(LowerError::Unsupported(
            "enum store requires enum adt".into(),
        ));
    };

    // Copy discriminant first.
    let discr_ty =
        hir::analysis::ty::ty_def::TyId::new(ctx.db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
    let discr = load_place_typed(ctx, src_place, discr_ty)?;
    store_word_to_place(ctx, dst_place, discr)?;

    let origin_block = ctx
        .fb
        .current_block()
        .ok_or_else(|| LowerError::Internal("missing current block".to_string()))?;
    let cont_block = ctx.fb.append_block();

    let variants = adt_def.fields(ctx.db);
    let mut cases: Vec<(ValueId, BlockId)> = Vec::with_capacity(variants.len());
    let mut case_blocks = Vec::with_capacity(variants.len());
    for (idx, _) in variants.iter().enumerate() {
        let case_block = ctx.fb.append_block();
        case_blocks.push(case_block);
        cases.push((ctx.fb.make_imm_value(I256::from(idx as u64)), case_block));
    }

    ctx.fb.switch_to_block(origin_block);
    if cases.is_empty() {
        ctx.fb.insert_inst_no_result(Jump::new(ctx.is, cont_block));
    } else {
        let mut compare_blocks = Vec::with_capacity(cases.len().saturating_sub(1));
        for _ in 0..cases.len().saturating_sub(1) {
            compare_blocks.push(ctx.fb.append_block());
        }

        for (case_idx, (case_value, case_dest)) in cases.into_iter().enumerate() {
            if case_idx > 0 {
                ctx.fb.switch_to_block(compare_blocks[case_idx - 1]);
            }

            let else_dest = if case_idx + 1 < compare_blocks.len() + 1 {
                compare_blocks[case_idx]
            } else {
                cont_block
            };
            let cond = ctx
                .fb
                .insert_inst(Eq::new(ctx.is, discr, case_value), Type::I1);
            ctx.fb
                .insert_inst_no_result(Br::new(ctx.is, cond, case_dest, else_dest));
        }
    }

    for (idx, case_block) in case_blocks.into_iter().enumerate() {
        ctx.fb.switch_to_block(case_block);
        let enum_variant = hir::hir_def::EnumVariant::new(enm, idx);
        let ctor =
            hir::analysis::ty::simplified_pattern::ConstructorKind::Variant(enum_variant, enum_ty);
        for (field_idx, field_ty) in ctor.field_types(ctx.db).iter().copied().enumerate() {
            let proj = Projection::VariantField {
                variant: enum_variant,
                enum_ty,
                field_idx,
            };
            let dst_field = extend_place(dst_place, proj.clone());
            let src_field = extend_place(src_place, proj);
            deep_copy_from_places(ctx, &dst_field, &src_field, field_ty)?;
        }
        ctx.fb.insert_inst_no_result(Jump::new(ctx.is, cont_block));
    }

    ctx.fb.switch_to_block(cont_block);
    Ok(())
}

fn extend_place<'db>(place: &Place<'db>, proj: mir::ir::MirProjection<'db>) -> Place<'db> {
    let mut path = place.projection.clone();
    path.push(proj);
    Place::new(place.base, path)
}

fn condition_to_i1<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    cond: ValueId,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> ValueId {
    if fb.type_of(cond) == Type::I1 {
        cond
    } else {
        let zero = fb.make_imm_value(I256::zero());
        fb.insert_inst(Ne::new(is, cond, zero), Type::I1)
    }
}

fn emit_evm_malloc_word_addr<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    size: ValueId,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> ValueId {
    let ptr_ty = fb.ptr_type(Type::I8);
    let ptr = fb.insert_inst(EvmMalloc::new(is, size), ptr_ty);
    fb.insert_inst(PtrToInt::new(is, ptr, Type::I256), Type::I256)
}

/// Lower a `ConstAggregate` by registering a global data section and using CODECOPY.
///
/// Registers the constant bytes as a Sonatina global variable (data section),
/// then emits: malloc → symaddr → symsize → codecopy. This is the Sonatina
/// equivalent of Yul's datacopy/dataoffset/datasize pattern.
fn lower_const_aggregate<C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, '_, C>,
    data: &[u8],
) -> Result<ValueId, LowerError> {
    let gv_ref = if let Some(&existing) = ctx.const_data_globals.get(data) {
        existing
    } else {
        // Build array initializer from raw bytes (each element is one byte)
        let elems: Vec<GvInitializer> = data
            .iter()
            .map(|&b| GvInitializer::make_imm(Immediate::I8(b as i8)))
            .collect();
        let array_init = GvInitializer::make_array(elems);

        // Register as a const global variable
        let label = format!("__fe_const_data_{}", *ctx.data_global_counter);
        *ctx.data_global_counter += 1;
        let array_ty = ctx
            .fb
            .module_builder
            .declare_array_type(Type::I8, data.len());
        let gv_data = GlobalVariableData::constant(label, array_ty, Linkage::Private, array_init);
        let gv_ref = ctx.fb.module_builder.declare_gv(gv_data);
        ctx.const_data_globals.insert(data.to_vec(), gv_ref);
        ctx.data_globals.push(gv_ref);
        gv_ref
    };

    // Emit: malloc + codecopy
    let size_val = ctx.fb.make_imm_value(I256::from(data.len() as u64));
    let ptr = emit_evm_malloc_word_addr(ctx.fb, size_val, ctx.is);
    let sym = SymbolRef::Global(gv_ref);
    let code_offset = ctx
        .fb
        .insert_inst(SymAddr::new(ctx.is, sym.clone()), Type::I256);
    let code_size = ctx.fb.insert_inst(SymSize::new(ctx.is, sym), Type::I256);
    ctx.fb
        .insert_inst_no_result(EvmCodeCopy::new(ctx.is, ptr, code_offset, code_size));

    Ok(ptr)
}
