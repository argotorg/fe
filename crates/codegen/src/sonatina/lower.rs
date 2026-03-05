//! Instruction-level lowering from MIR to Sonatina IR.
//!
//! Contains all `lower_*` free functions that operate on `LowerCtx`.

use driver::DriverDataBase;
use hir::analysis::ty::adt_def::AdtRef;
use hir::analysis::ty::ty_def::{CapabilityKind, PrimTy, TyBase, TyData, TyId};
use hir::hir_def::expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp};
use hir::projection::{IndexSource, Projection};
use mir::ir::{
    AddressSpaceKind, BuiltinTerminatorKind, CheckedArithmeticOp, CheckedIntrinsic, IntrinsicOp,
    Place, SyntheticValue,
};
use mir::layout;
use mir::layout::TargetDataLayout;
use mir::repr::{PlaceState, ResolvedPlace, ResolvedPlaceProjection, ResolvedPlaceSegment};
use num_bigint::BigUint;
use rustc_hash::FxHashMap;
use smallvec1::SmallVec;
use sonatina_ir::{
    BlockId, GlobalVariableData, GlobalVariableRef, I256, Immediate, Linkage, Type, Value, ValueId,
    global_variable::GvInitializer,
    inst::{
        arith::{Add, Mul, Neg, Sar, Shl, Shr, Sub},
        cast::{Bitcast, IntToPtr, PtrToInt, Sext, Trunc, Zext},
        cmp::{Eq, Gt, IsZero, Lt, Ne, Slt},
        control_flow::{Br, BrTable, Call, Jump, Return},
        data::{Alloca, Gep, Mload, Mstore, SymAddr, SymSize, SymbolRef},
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

use super::{
    LowerCtx, LowerError, is_erased_runtime_ty, runtime_type_for_pointer_info,
    runtime_type_for_shape, types, zero_value_for_type,
};

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
                let dest_ty = ctx
                    .body
                    .locals
                    .get(dest_local.index())
                    .ok_or_else(|| {
                        LowerError::Internal(format!("missing local type for {dest_local:?}"))
                    })?
                    .ty;
                let dest_var = ctx.local_vars.get(dest_local).copied().ok_or_else(|| {
                    LowerError::Internal(format!("missing SSA variable for local {dest_local:?}"))
                })?;
                let value = coerce_value_to_runtime_ty(
                    ctx,
                    value,
                    dest_ty,
                    ctx.local_runtime_types[dest_local.index()],
                );
                ctx.fb.def_var(dest_var, value);
                ctx.initialized_locals.insert(*dest_local);
                if ctx.local_place_roots.contains_key(dest_local) {
                    store_runtime_value_to_local_slot(ctx, *dest_local, value)?;
                }
                return Ok(());
            }

            let result = lower_rvalue(ctx, rvalue, *dest)?;
            if let (Some(dest_local), Some(result_val)) = (dest, result) {
                let dest_ty = ctx
                    .body
                    .locals
                    .get(dest_local.index())
                    .ok_or_else(|| {
                        LowerError::Internal(format!("missing local type for {dest_local:?}"))
                    })?
                    .ty;
                let dest_var = ctx.local_vars.get(dest_local).copied().ok_or_else(|| {
                    LowerError::Internal(format!("missing SSA variable for local {dest_local:?}"))
                })?;
                let converted = coerce_value_to_runtime_ty(
                    ctx,
                    result_val,
                    dest_ty,
                    ctx.local_runtime_types[dest_local.index()],
                );
                ctx.fb.def_var(dest_var, converted);
                ctx.initialized_locals.insert(*dest_local);
                if ctx.local_place_roots.contains_key(dest_local) {
                    store_runtime_value_to_local_slot(ctx, *dest_local, converted)?;
                }
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
            let discr_ty = TyId::new(ctx.db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
            store_typed_to_place(ctx, place, val, discr_ty)?;
        }
        MirInst::BindValue { value, .. } => {
            // Ensure the value is lowered and cached
            let _ = lower_value(ctx, *value)?;
        }
    }

    Ok(())
}

fn allocate_local_place_root_slot<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    local: mir::LocalId,
) -> Result<ValueId, LowerError> {
    let local_ty = ctx
        .body
        .locals
        .get(local.index())
        .ok_or_else(|| LowerError::Internal(format!("unknown local: {local:?}")))?
        .ty;
    let Some(size_bytes) = layout::ty_memory_size_or_word_in(ctx.db, ctx.target_layout, local_ty)
    else {
        return Err(LowerError::Unsupported(format!(
            "cannot determine addressable slot size for `{}`",
            local_ty.pretty_print(ctx.db)
        )));
    };
    let opaque_ptr_ty = ctx.fb.ptr_type(Type::I8);
    if size_bytes == 0 {
        let zero = types::zero_value(ctx.fb, Type::I256);
        return Ok(ctx
            .fb
            .insert_inst(IntToPtr::new(ctx.is, zero, opaque_ptr_ty), opaque_ptr_ty));
    }
    if local_place_root_slot_may_escape(ctx, local) {
        let size_val = ctx.fb.make_imm_value(I256::from(size_bytes as u64));
        return Ok(emit_evm_malloc_ptr(ctx.fb, size_val, ctx.is));
    }
    let alloca_ty = ctx.fb.declare_array_type(Type::I8, size_bytes);
    Ok(emit_alloca_ptr(ctx.fb, alloca_ty, ctx.is))
}

fn store_runtime_value_to_local_slot_ptr<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    local: mir::LocalId,
    slot_ptr: ValueId,
    value: ValueId,
) -> Result<(), LowerError> {
    let local_ty = ctx
        .body
        .locals
        .get(local.index())
        .ok_or_else(|| LowerError::Internal(format!("unknown local: {local:?}")))?
        .ty;
    if is_erased_runtime_ty(ctx.db, ctx.target_layout, local_ty) {
        return Ok(());
    }
    let ptr_ty = ctx.fb.ptr_type(Type::I8);
    let slot_ptr = bitcast_ptr(ctx, slot_ptr, ptr_ty);
    let value_ty = ctx.fb.type_of(value);
    if value_ty.is_pointer(&ctx.fb.module_builder.ctx) {
        ctx.fb
            .insert_inst_no_result(Mstore::new(ctx.is, slot_ptr, value, value_ty));
        return Ok(());
    }
    let word = apply_to_word(ctx.fb, ctx.db, value, local_ty, ctx.is);
    ctx.fb
        .insert_inst_no_result(Mstore::new(ctx.is, slot_ptr, word, Type::I256));
    Ok(())
}

pub(super) fn ensure_local_place_root_slot<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    local: mir::LocalId,
) -> Result<ValueId, LowerError> {
    if let Some(slot_ptr) = ctx.local_place_roots.get(&local).copied() {
        return Ok(slot_ptr);
    }

    let slot_ptr = allocate_local_place_root_slot(ctx, local)?;
    if ctx.initialized_locals.contains(&local) {
        let var = ctx.local_vars.get(&local).copied().ok_or_else(|| {
            LowerError::Internal(format!("missing SSA variable for local {local:?}"))
        })?;
        let current = ctx.fb.use_var(var);
        store_runtime_value_to_local_slot_ptr(ctx, local, slot_ptr, current)?;
    }
    ctx.local_place_roots.insert(local, slot_ptr);
    Ok(slot_ptr)
}

fn store_runtime_value_to_local_slot<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    local: mir::LocalId,
    value: ValueId,
) -> Result<(), LowerError> {
    let slot_ptr = ensure_local_place_root_slot(ctx, local)?;
    store_runtime_value_to_local_slot_ptr(ctx, local, slot_ptr, value)
}

fn load_runtime_value_from_local_slot<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    local: mir::LocalId,
    expected_runtime_ty: Type,
) -> Result<ValueId, LowerError> {
    let slot_ptr = ensure_local_place_root_slot(ctx, local)?;
    let local_ty = ctx
        .body
        .locals
        .get(local.index())
        .ok_or_else(|| LowerError::Internal(format!("unknown local: {local:?}")))?
        .ty;
    if expected_runtime_ty.is_pointer(&ctx.fb.module_builder.ctx) {
        return Ok(ctx.fb.insert_inst(
            Mload::new(ctx.is, slot_ptr, expected_runtime_ty),
            expected_runtime_ty,
        ));
    }
    let raw = ctx
        .fb
        .insert_inst(Mload::new(ctx.is, slot_ptr, Type::I256), Type::I256);
    let loaded = apply_from_word(ctx.fb, ctx.db, raw, local_ty, ctx.is);
    Ok(coerce_value_to_type(ctx, loaded, expected_runtime_ty))
}

/// Lower a MIR rvalue to a Sonatina value.
fn lower_rvalue<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    rvalue: &mir::Rvalue<'db>,
    dest_local: Option<mir::LocalId>,
) -> Result<Option<ValueId>, LowerError> {
    use mir::Rvalue;

    match rvalue {
        Rvalue::ZeroInit => {
            let ty = dest_local
                .map(|local| ctx.local_runtime_types[local.index()])
                .unwrap_or(Type::I256);
            Ok(Some(zero_value_for_type(ctx.fb, ty, ctx.is)))
        }
        Rvalue::Value(value_id) => {
            let val = lower_value(ctx, *value_id)?;
            Ok(Some(val))
        }
        Rvalue::Call(call) => {
            if let Some(checked) = call.checked_intrinsic {
                if !call.effect_args.is_empty() {
                    return Err(LowerError::Internal(format!(
                        "checked intrinsic call unexpectedly has effect args: {checked:?}"
                    )));
                }
                let result = lower_checked_intrinsic(ctx, checked, &call.args)?;
                return Ok(Some(result));
            }

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
                                let offset = coerce_value_to_word(ctx, *offset);
                                ctx.fb
                                    .insert_inst_no_result(EvmLog0::new(ctx.is, offset, *len));
                                return Ok(None);
                            }
                            ("log1", [offset, len, topic0]) => {
                                let offset = coerce_value_to_word(ctx, *offset);
                                ctx.fb.insert_inst_no_result(EvmLog1::new(
                                    ctx.is, offset, *len, *topic0,
                                ));
                                return Ok(None);
                            }
                            ("log2", [offset, len, topic0, topic1]) => {
                                let offset = coerce_value_to_word(ctx, *offset);
                                ctx.fb.insert_inst_no_result(EvmLog2::new(
                                    ctx.is, offset, *len, *topic0, *topic1,
                                ));
                                return Ok(None);
                            }
                            ("log3", [offset, len, topic0, topic1, topic2]) => {
                                let offset = coerce_value_to_word(ctx, *offset);
                                ctx.fb.insert_inst_no_result(EvmLog3::new(
                                    ctx.is, offset, *len, *topic0, *topic1, *topic2,
                                ));
                                return Ok(None);
                            }
                            ("log4", [offset, len, topic0, topic1, topic2, topic3]) => {
                                let offset = coerce_value_to_word(ctx, *offset);
                                ctx.fb.insert_inst_no_result(EvmLog4::new(
                                    ctx.is, offset, *len, *topic0, *topic1, *topic2, *topic3,
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
                        let offset = coerce_value_to_word(ctx, offset);
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
                        let offset = coerce_value_to_word(ctx, offset);
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
                        let arg_offset = coerce_value_to_word(ctx, arg_offset);
                        let ret_offset = coerce_value_to_word(ctx, ret_offset);
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
                        let arg_offset = coerce_value_to_word(ctx, arg_offset);
                        let ret_offset = coerce_value_to_word(ctx, ret_offset);
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
                        let arg_offset = coerce_value_to_word(ctx, arg_offset);
                        let ret_offset = coerce_value_to_word(ctx, ret_offset);
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
                        if dest_local.is_some_and(|local| {
                            let local_ty = ctx.body.local(local).ty;
                            !local_ty.is_array(ctx.db) && !local_may_escape(ctx, local)
                        }) && let Some(size_bytes) = const_usize_value(ctx.fb, size)
                        {
                            if size_bytes == 0 {
                                return Ok(Some(ctx.fb.make_imm_value(I256::zero())));
                            }
                            let alloca_ty = ctx.fb.declare_array_type(Type::I8, size_bytes);
                            return Ok(Some(emit_alloca_word_addr(ctx.fb, alloca_ty, ctx.is)));
                        }
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
                        let offset = coerce_value_to_word(ctx, *offset);
                        return Ok(Some(ctx.fb.insert_inst(
                            EvmCreate::new(ctx.is, *val, offset, *len),
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
                        let offset = coerce_value_to_word(ctx, *offset);
                        return Ok(Some(ctx.fb.insert_inst(
                            EvmCreate2::new(ctx.is, *val, offset, *len, *salt),
                            Type::I256,
                        )));
                    }
                    _ => {}
                }
            }

            // Core numeric intrinsics (extern functions from `core::num`).
            if call.effect_args.is_empty()
                && let Some(result) = try_lower_numeric_intrinsic(ctx, callee_name, &call.args)?
            {
                return Ok(Some(result));
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
            let callee_metadata =
                ctx.runtime_function_metadata
                    .get(callee_name)
                    .ok_or_else(|| {
                        LowerError::Internal(format!(
                            "missing runtime type metadata for function: {callee_name}"
                        ))
                    })?;
            if let Some(ret_ty) = callee_metadata.ret {
                let result = ctx.fb.insert_inst(call_inst, ret_ty);
                Ok(Some(result))
            } else {
                // Unit-returning calls don't produce a value
                ctx.fb.insert_inst_no_result(call_inst);
                Ok(None)
            }
        }
        Rvalue::Intrinsic { op, args } => lower_intrinsic(ctx, *op, args),
        Rvalue::Load { place } => {
            let expected_runtime_ty = dest_local
                .map(|local| ctx.local_runtime_types[local.index()])
                .unwrap_or(Type::I256);
            let loaded_ty = dest_local
                .and_then(|local| ctx.body.locals.get(local.index()))
                .map(|local| local.ty)
                .ok_or_else(|| {
                    LowerError::Internal("load rvalue without a destination local type".to_string())
                })?;
            Ok(Some(load_place_runtime(
                ctx,
                place,
                loaded_ty,
                expected_runtime_ty,
            )?))
        }
        Rvalue::Alloc { .. } => Err(LowerError::Internal(
            "Alloc rvalue should be handled directly in Assign lowering".to_string(),
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
    let mut arg_tys = Vec::with_capacity(regular_args.len() + effect_args.len());
    if let Some(metadata) = ctx.runtime_function_metadata.get(callee_name) {
        let all_args: Vec<_> = regular_args
            .iter()
            .chain(effect_args.iter())
            .copied()
            .collect();
        if metadata.params.len() != all_args.len() {
            return Err(LowerError::Internal(format!(
                "{context} to `{callee_name}` has mismatched arg metadata length (params={}, call_args={})",
                metadata.params.len(),
                all_args.len()
            )));
        }
        for (expected_ty, arg) in metadata.params.iter().copied().zip(all_args) {
            let arg_ty = ctx
                .body
                .values
                .get(arg.index())
                .ok_or_else(|| LowerError::Internal("unknown call argument".to_string()))?
                .ty;
            let lowered = lower_value(ctx, arg)?;
            args.push(coerce_value_to_runtime_ty(
                ctx,
                lowered,
                arg_ty,
                expected_ty,
            ));
            arg_tys.push(arg_ty);
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
            arg_tys.push(arg_ty);
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
            arg_tys.push(arg_ty);
        }
    }

    let expected_arg_tys = ctx
        .fb
        .module_builder
        .ctx
        .func_sig(func_ref, |sig| sig.args().to_vec());
    let expected_argc = expected_arg_tys.len();
    if args.len() > expected_argc {
        return Err(LowerError::Internal(format!(
            "{context} to `{callee_name}` has too many args (got {}, expected {expected_argc})",
            args.len()
        )));
    }
    for ((arg, arg_ty), expected_ty) in args
        .iter_mut()
        .zip(arg_tys.iter().copied())
        .zip(expected_arg_tys.iter().copied())
    {
        *arg = coerce_runtime_value(ctx.fb, ctx.db, *arg, arg_ty, expected_ty, ctx.is);
    }
    for ty in expected_arg_tys.into_iter().skip(args.len()) {
        args.push(types::zero_value(ctx.fb, ty));
    }
    while args.len() < expected_argc {
        let arg_ty = ctx
            .fb
            .module_builder
            .ctx
            .func_sig(func_ref, |sig| sig.args()[args.len()]);
        args.push(zero_value_for_type(ctx.fb, arg_ty, ctx.is));
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
    lower_value_origin(ctx, value_id, value_data)
}

/// Lower a MIR value origin to a Sonatina value.
fn lower_value_origin<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    value_id: mir::ValueId,
    value_data: &mir::ValueData<'db>,
) -> Result<ValueId, LowerError> {
    use mir::ValueOrigin;
    let origin = &value_data.origin;
    let result_ty = runtime_type_for_shape(
        &ctx.fb.module_builder,
        ctx.db,
        ctx.core,
        ctx.target_layout,
        value_data.runtime_shape,
        &mut *ctx.gep_type_cache,
        &mut *ctx.gep_name_counter,
    );

    match origin {
        ValueOrigin::Synthetic(syn) => match syn {
            SyntheticValue::Int(n) => {
                let i256_val = biguint_to_i256(n);
                Ok(ctx
                    .fb
                    .make_imm_value(Immediate::from_i256(i256_val, result_ty)))
            }
            SyntheticValue::Bool(b) => {
                if result_ty == Type::I1 {
                    Ok(ctx.fb.make_imm_value(*b))
                } else {
                    let val = if *b { I256::one() } else { I256::zero() };
                    Ok(ctx.fb.make_imm_value(Immediate::from_i256(val, result_ty)))
                }
            }
            SyntheticValue::Bytes(bytes) => {
                if bytes.len() > 32 {
                    return Err(LowerError::Unsupported(format!(
                        "SyntheticValue::Bytes must fit in one EVM word, got {} bytes",
                        bytes.len()
                    )));
                }
                // Convert bytes to I256 (left-padded to 32 bytes)
                let i256_val = bytes_to_i256(bytes);
                Ok(ctx.fb.make_imm_value(i256_val))
            }
        },
        ValueOrigin::Local(local_id) => {
            if ctx.local_place_roots.contains_key(local_id) {
                return load_runtime_value_from_local_slot(ctx, *local_id, result_ty);
            }
            let var = ctx.local_vars.get(local_id).copied().ok_or_else(|| {
                LowerError::Internal(format!("SSA variable not found for local {local_id:?}"))
            })?;
            Ok(ctx.fb.use_var(var))
        }
        ValueOrigin::PlaceRoot(local_id) => {
            let slot_ptr = ensure_local_place_root_slot(ctx, *local_id)?;
            Ok(coerce_value_to_type(ctx, slot_ptr, result_ty))
        }
        ValueOrigin::Unit => Ok(types::zero_value(ctx.fb, result_ty)),
        ValueOrigin::Unary { op, inner } => {
            let inner_val = lower_value(ctx, *inner)?;
            lower_unary_op(ctx.fb, ctx.db, *op, inner_val, value_data.ty, ctx.is)
        }
        ValueOrigin::Binary { op, lhs, rhs } => {
            let lhs_val = lower_value(ctx, *lhs)?;
            let rhs_val = lower_value(ctx, *rhs)?;
            let lhs_ty = ctx
                .body
                .values
                .get(lhs.index())
                .ok_or_else(|| {
                    LowerError::Internal(format!("unknown MIR value id {}", lhs.index()))
                })?
                .ty;
            lower_binary_op(ctx.fb, ctx.db, *op, lhs_val, rhs_val, lhs_ty, ctx.is)
        }
        ValueOrigin::TransparentCast { value } => {
            let inner_val = lower_value(ctx, *value)?;
            let inner_ty = ctx
                .body
                .values
                .get(value.index())
                .ok_or_else(|| {
                    LowerError::Internal(format!("unknown MIR value id {}", value.index()))
                })?
                .ty;
            Ok(coerce_runtime_value(
                ctx.fb, ctx.db, inner_val, inner_ty, result_ty, ctx.is,
            ))
        }
        ValueOrigin::ControlFlowResult { expr } => {
            // ControlFlowResult values should be converted to Local values during MIR lowering.
            // If we reach here, it means MIR lowering didn't properly handle this case.
            Err(LowerError::Internal(format!(
                "ControlFlowResult value reached codegen without being converted to Local (expr={expr:?})"
            )))
        }
        ValueOrigin::PlaceRef(place) => {
            if !value_data.repr.is_ref() {
                let loaded_ty = value_data
                    .ty
                    .as_capability(ctx.db)
                    .map(|(_, inner_ty)| inner_ty)
                    .unwrap_or(value_data.ty);
                let runtime_ty = runtime_type_for_shape(
                    &ctx.fb.module_builder,
                    ctx.db,
                    ctx.core,
                    ctx.target_layout,
                    value_data.runtime_shape,
                    &mut *ctx.gep_type_cache,
                    &mut *ctx.gep_name_counter,
                );
                if place_yields_location_value(
                    ctx,
                    place,
                    value_data.ty,
                    ctx.body.value_pointer_info(value_id),
                )? {
                    return lower_place_runtime_location_value(ctx, place, runtime_ty);
                }
                return load_place_runtime(ctx, place, loaded_ty, runtime_ty);
            }
            if value_data.repr.address_space() == Some(AddressSpaceKind::Memory) {
                return lower_place_memory_ptr(ctx, place);
            }
            lower_place_address(ctx, place)
        }
        ValueOrigin::MoveOut { place } => {
            if place.projection.is_empty()
                && move_out_uses_root_local_alias(&ctx.body.values, place.base)
                && place_yields_location_value(
                    ctx,
                    place,
                    value_data.ty,
                    ctx.body.value_pointer_info(value_id),
                )?
            {
                return lower_value(ctx, place.base);
            }

            if value_data.repr.is_ref()
                && value_data.repr.address_space() == Some(AddressSpaceKind::Memory)
            {
                lower_place_memory_ptr(ctx, place)
            } else if matches!(value_data.repr, mir::ValueRepr::Ptr(_)) {
                let runtime_ty = runtime_type_for_shape(
                    &ctx.fb.module_builder,
                    ctx.db,
                    ctx.core,
                    ctx.target_layout,
                    value_data.runtime_shape,
                    &mut *ctx.gep_type_cache,
                    &mut *ctx.gep_name_counter,
                );
                if place_yields_location_value(
                    ctx,
                    place,
                    value_data.ty,
                    ctx.body.value_pointer_info(value_id),
                )? {
                    return lower_place_runtime_location_value(ctx, place, runtime_ty);
                }
                load_place_runtime(ctx, place, value_data.ty, runtime_ty)
            } else if value_data.repr.address_space().is_some() {
                lower_place_address(ctx, place)
            } else {
                load_place_runtime(
                    ctx,
                    place,
                    value_data.ty,
                    types::value_type(ctx.db, value_data.ty),
                )
            }
        }
        ValueOrigin::ConstRegion(region_id) => {
            let region = ctx.body.const_region(*region_id);
            let gv_ref = ensure_const_data_global(ctx, &region.bytes);
            Ok(ctx
                .fb
                .insert_inst(SymAddr::new(ctx.is, SymbolRef::Global(gv_ref)), Type::I256))
        }
        ValueOrigin::FieldPtr(field_ptr) => {
            let base = lower_value(ctx, field_ptr.base)?;
            if field_ptr.offset_bytes == 0 {
                Ok(base)
            } else if field_ptr.addr_space == AddressSpaceKind::Memory {
                let i8_ptr_ty = ctx.fb.ptr_type(Type::I8);
                let base_ptr = coerce_word_addr_to_ptr(ctx, base, i8_ptr_ty);
                let zero = ctx.fb.make_imm_value(I256::zero());
                let offset = ctx
                    .fb
                    .make_imm_value(I256::from(field_ptr.offset_bytes as u64));
                Ok(ctx.fb.insert_inst(
                    Gep::new(ctx.is, smallvec1::smallvec![base_ptr, zero, offset]),
                    i8_ptr_ty,
                ))
            } else {
                let offset = match field_ptr.addr_space {
                    AddressSpaceKind::Calldata => field_ptr.offset_bytes,
                    AddressSpaceKind::Code => field_ptr.offset_bytes,
                    AddressSpaceKind::Storage | AddressSpaceKind::TransientStorage => {
                        field_ptr.offset_bytes / 32
                    }
                    AddressSpaceKind::Memory => unreachable!(),
                };
                let offset_val = ctx.fb.make_imm_value(I256::from(offset as u64));
                let base = coerce_value_to_word(ctx, base);
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

fn move_out_uses_root_local_alias<'db>(
    values: &[mir::ValueData<'db>],
    mut value: mir::ValueId,
) -> bool {
    loop {
        let Some(value_data) = values.get(value.index()) else {
            return false;
        };
        match value_data.origin {
            mir::ValueOrigin::TransparentCast { value: inner } => value = inner,
            mir::ValueOrigin::Local(_) | mir::ValueOrigin::PlaceRoot(_) => return true,
            _ => return false,
        }
    }
}

/// Lower a unary operation.
fn lower_unary_op<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    op: UnOp,
    inner: ValueId,
    result_ty: hir::analysis::ty::ty_def::TyId<'db>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    let op_ty = types::value_type(db, result_ty);
    let prim = prim_for_runtime_value(db, result_ty);
    let inner = prim
        .map(|prim| coerce_scalar_value(fb, is, inner, op_ty, prim_is_signed(prim)))
        .unwrap_or(inner);
    match op {
        UnOp::Not => {
            let cond = condition_to_i1(fb, inner, is);
            Ok(fb.insert_inst(IsZero::new(is, cond), Type::I1))
        }
        UnOp::Minus => {
            // Arithmetic negation
            let result = fb.insert_inst(Neg::new(is, inner), op_ty);
            Ok(result)
        }
        UnOp::BitNot => {
            // Bitwise not
            let result = fb.insert_inst(Not::new(is, inner), op_ty);
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
fn lower_binary_op<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    op: BinOp,
    lhs: ValueId,
    rhs: ValueId,
    lhs_ty: hir::analysis::ty::ty_def::TyId<'db>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    match op {
        BinOp::Arith(arith_op) => lower_arith_op(fb, db, arith_op, lhs, rhs, lhs_ty, is),
        BinOp::Comp(comp_op) => lower_comp_op(fb, db, comp_op, lhs, rhs, lhs_ty, is),
        BinOp::Logical(log_op) => lower_logical_op(fb, log_op, lhs, rhs, is),
        BinOp::Index => {
            // Index operations are handled via projections, not as binary ops
            Err(LowerError::Unsupported("index binary op".to_string()))
        }
    }
}

/// Lower an arithmetic binary operation.
fn lower_arith_op<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    op: ArithBinOp,
    lhs: ValueId,
    rhs: ValueId,
    operand_ty: hir::analysis::ty::ty_def::TyId<'db>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    let operand_ty = mir::repr::word_conversion_leaf_ty(db, operand_ty);
    let op_ty = types::value_type(db, operand_ty);
    let signed = if let TyData::TyBase(TyBase::Prim(prim)) = operand_ty.base_ty(db).data(db) {
        prim_is_signed(*prim)
    } else {
        false
    };
    let lhs = coerce_scalar_value(fb, is, lhs, op_ty, signed);
    let rhs = coerce_scalar_value(fb, is, rhs, op_ty, signed);
    let result = match op {
        ArithBinOp::Add => fb.insert_inst(Add::new(is, lhs, rhs), op_ty),
        ArithBinOp::Sub => fb.insert_inst(Sub::new(is, lhs, rhs), op_ty),
        ArithBinOp::Mul => fb.insert_inst(Mul::new(is, lhs, rhs), op_ty),
        ArithBinOp::Div => {
            if signed {
                fb.insert_evm_sdivo(lhs, rhs)[0]
            } else {
                fb.insert_inst(EvmUdiv::new(is, lhs, rhs), op_ty)
            }
        }
        ArithBinOp::Rem => {
            if signed {
                fb.insert_evm_smodo(lhs, rhs)[0]
            } else {
                fb.insert_inst(EvmUmod::new(is, lhs, rhs), op_ty)
            }
        }
        ArithBinOp::Pow => fb.insert_inst(EvmExp::new(is, lhs, rhs), op_ty),
        // Shl/Shr take (bits, value).
        ArithBinOp::LShift => fb.insert_inst(Shl::new(is, rhs, lhs), op_ty),
        ArithBinOp::RShift => {
            if signed {
                fb.insert_inst(Sar::new(is, rhs, lhs), op_ty)
            } else {
                fb.insert_inst(Shr::new(is, rhs, lhs), op_ty)
            }
        }
        ArithBinOp::BitOr => fb.insert_inst(Or::new(is, lhs, rhs), op_ty),
        ArithBinOp::BitXor => fb.insert_inst(Xor::new(is, lhs, rhs), op_ty),
        ArithBinOp::BitAnd => fb.insert_inst(And::new(is, lhs, rhs), op_ty),
        ArithBinOp::Range => {
            // Range is handled at HIR level, shouldn't reach MIR binary ops
            return Err(LowerError::Unsupported("range operator".to_string()));
        }
    };
    Ok(result)
}

/// Lower a comparison binary operation.
fn lower_comp_op<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    op: CompBinOp,
    lhs: ValueId,
    rhs: ValueId,
    operand_ty: hir::analysis::ty::ty_def::TyId<'db>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    let operand_ty = mir::repr::word_conversion_leaf_ty(db, operand_ty);
    let signed = if let TyData::TyBase(TyBase::Prim(prim)) = operand_ty.base_ty(db).data(db) {
        prim_is_signed(*prim)
    } else {
        false
    };
    let op_ty = types::value_type(db, operand_ty);
    let lhs = coerce_scalar_value(fb, is, lhs, op_ty, signed);
    let rhs = coerce_scalar_value(fb, is, rhs, op_ty, signed);
    let result = match op {
        CompBinOp::Eq => fb.insert_inst(Eq::new(is, lhs, rhs), Type::I1),
        CompBinOp::NotEq => {
            // neq = iszero(eq(lhs, rhs))
            let eq_result = fb.insert_inst(Eq::new(is, lhs, rhs), Type::I1);
            fb.insert_inst(IsZero::new(is, eq_result), Type::I1)
        }
        CompBinOp::Lt => {
            if signed {
                fb.insert_inst(Slt::new(is, lhs, rhs), Type::I1)
            } else {
                fb.insert_inst(Lt::new(is, lhs, rhs), Type::I1)
            }
        }
        CompBinOp::LtEq => {
            // lhs <= rhs  <==>  !(lhs > rhs)
            let gt_result = if signed {
                fb.insert_inst(Slt::new(is, rhs, lhs), Type::I1)
            } else {
                fb.insert_inst(Gt::new(is, lhs, rhs), Type::I1)
            };
            fb.insert_inst(IsZero::new(is, gt_result), Type::I1)
        }
        CompBinOp::Gt => {
            if signed {
                fb.insert_inst(Slt::new(is, rhs, lhs), Type::I1)
            } else {
                fb.insert_inst(Gt::new(is, lhs, rhs), Type::I1)
            }
        }
        CompBinOp::GtEq => {
            // lhs >= rhs  <==>  !(lhs < rhs)
            let lt_result = if signed {
                fb.insert_inst(Slt::new(is, lhs, rhs), Type::I1)
            } else {
                fb.insert_inst(Lt::new(is, lhs, rhs), Type::I1)
            };
            fb.insert_inst(IsZero::new(is, lt_result), Type::I1)
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
    let result = match op {
        LogicalBinOp::And => fb.insert_inst(And::new(is, lhs, rhs), Type::I1),
        LogicalBinOp::Or => fb.insert_inst(Or::new(is, lhs, rhs), Type::I1),
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
            Ok(Some(coerce_value_to_word(ctx, arg)))
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
            let addr = coerce_value_to_word(ctx, addr);
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
            let addr = coerce_value_to_word(ctx, *addr);
            let val = coerce_value_to_word(ctx, *val);
            ctx.fb
                .insert_inst_no_result(Mstore::new(ctx.is, addr, val, Type::I256));
            Ok(None)
        }
        IntrinsicOp::Mstore8 => {
            let [addr, val] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "mstore8 requires 2 arguments".to_string(),
                ));
            };
            let addr = coerce_value_to_word(ctx, *addr);
            let val = coerce_value_to_word(ctx, *val);
            ctx.fb
                .insert_inst_no_result(EvmMstore8::new(ctx.is, addr, val));
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
            let dst = coerce_value_to_word(ctx, *dst);
            ctx.fb
                .insert_inst_no_result(EvmCalldataCopy::new(ctx.is, dst, *offset, *len));
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
            let dst = coerce_value_to_word(ctx, *dst);
            ctx.fb
                .insert_inst_no_result(EvmReturnDataCopy::new(ctx.is, dst, *offset, *len));
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
            let dst = coerce_value_to_word(ctx, *dst);
            ctx.fb
                .insert_inst_no_result(EvmCodeCopy::new(ctx.is, dst, *offset, *len));
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
            let addr = coerce_value_to_word(ctx, *addr);
            Ok(Some(ctx.fb.insert_inst(
                EvmKeccak256::new(ctx.is, addr, *len),
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
    types::prim_scalar_type(prim).filter(|ty| *ty != Type::I256)
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

fn int_type_bits(ty: Type) -> Option<u16> {
    match ty {
        Type::I1 => Some(1),
        Type::I8 => Some(8),
        Type::I16 => Some(16),
        Type::I32 => Some(32),
        Type::I64 => Some(64),
        Type::I128 => Some(128),
        Type::I256 => Some(256),
        _ => None,
    }
}

fn cast_int_value<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
    value: ValueId,
    target_ty: Type,
    signed: bool,
) -> ValueId {
    let current_ty = fb.type_of(value);
    if current_ty == target_ty {
        return value;
    }

    let Some(current_bits) = int_type_bits(current_ty) else {
        return value;
    };
    let Some(target_bits) = int_type_bits(target_ty) else {
        return value;
    };

    if current_bits > target_bits {
        fb.insert_inst(Trunc::new(is, value, target_ty), target_ty)
    } else if current_bits < target_bits {
        if signed && current_ty != Type::I1 {
            fb.insert_inst(Sext::new(is, value, target_ty), target_ty)
        } else {
            fb.insert_inst(Zext::new(is, value, target_ty), target_ty)
        }
    } else {
        value
    }
}

fn prim_for_runtime_value<'db>(
    db: &'db DriverDataBase,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
) -> Option<PrimTy> {
    let ty = match ty.as_capability(db) {
        Some((CapabilityKind::View, inner)) => inner,
        _ => ty,
    };
    let ty = mir::repr::word_conversion_leaf_ty(db, ty);
    let TyData::TyBase(TyBase::Prim(prim)) = ty.base_ty(db).data(db) else {
        return None;
    };
    Some(*prim)
}

fn coerce_scalar_value<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
    value: ValueId,
    target_ty: Type,
    signed: bool,
) -> ValueId {
    if target_ty == Type::I1 {
        condition_to_i1(fb, value, is)
    } else {
        cast_int_value(fb, is, value, target_ty, signed)
    }
}

fn coerce_runtime_value<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    value: ValueId,
    value_ty: hir::analysis::ty::ty_def::TyId<'db>,
    target_ty: Type,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> ValueId {
    let current_ty = fb.type_of(value);
    if current_ty == target_ty {
        return value;
    }
    if target_ty == Type::Unit {
        return types::zero_value(fb, Type::Unit);
    }
    if current_ty == Type::Unit {
        return types::zero_value(fb, target_ty);
    }

    match prim_for_runtime_value(db, value_ty) {
        Some(PrimTy::Bool) => coerce_scalar_value(fb, is, value, target_ty, false),
        Some(prim) => coerce_scalar_value(fb, is, value, target_ty, prim_is_signed(prim)),
        None => cast_int_value(fb, is, value, target_ty, false),
    }
}

fn make_int_immediate<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    value: I256,
    ty: Type,
) -> ValueId {
    fb.make_imm_value(Immediate::from_i256(value, ty))
}

/// Applies `from_word` conversion after loading a value.
///
/// This mirrors the stdlib `WordRepr::from_word` semantics, but returns the
/// loaded value at its natural Sonatina type.
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
            PrimTy::Bool => condition_to_i1(fb, raw_value, is),
            _ => prim_to_sonatina_type(*prim)
                .map(|small_ty| cast_int_value(fb, is, raw_value, small_ty, prim_is_signed(*prim)))
                .unwrap_or(raw_value),
        }
    } else {
        // Non-primitive type, no conversion
        raw_value
    }
}

/// Applies `to_word` conversion before storing a value.
///
/// This mirrors the stdlib `WordRepr::to_word` semantics, converting a natural-width
/// Sonatina integer back into an EVM word.
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
                let bool_val = condition_to_i1(fb, value, is);
                fb.insert_inst(Zext::new(is, bool_val, Type::I256), Type::I256)
            }
            _ => prim_to_sonatina_type(*prim)
                .map(|small_ty| {
                    let cast = cast_int_value(fb, is, value, small_ty, prim_is_signed(*prim));
                    if prim_is_signed(*prim) {
                        fb.insert_inst(Sext::new(is, cast, Type::I256), Type::I256)
                    } else {
                        fb.insert_inst(Zext::new(is, cast, Type::I256), Type::I256)
                    }
                })
                .unwrap_or(value),
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
pub(super) fn fe_ty_to_sonatina<'db>(
    builder: &sonatina_ir::builder::ModuleBuilder,
    db: &'db DriverDataBase,
    core: &mir::CoreLib<'db>,
    target_layout: &TargetDataLayout,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
    cache: &mut FxHashMap<String, Option<Type>>,
    name_counter: &mut usize,
) -> Option<Type> {
    let cache_key = format!("{ty:?}");
    if let Some(cached) = cache.get(&cache_key) {
        return *cached;
    }

    let result = fe_ty_to_sonatina_inner(builder, db, core, target_layout, ty, cache, name_counter);
    cache.insert(cache_key, result);
    result
}

fn pointer_like_sonatina_ty<'db>(
    builder: &sonatina_ir::builder::ModuleBuilder,
    db: &'db DriverDataBase,
    core: &mir::CoreLib<'db>,
    target_layout: &TargetDataLayout,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
    cache: &mut FxHashMap<String, Option<Type>>,
    name_counter: &mut usize,
) -> Type {
    let pointee = fe_ty_to_sonatina(builder, db, core, target_layout, ty, cache, name_counter)
        .unwrap_or(Type::I8);
    builder.ptr_type(if pointee == Type::Unit {
        Type::I8
    } else {
        pointee
    })
}

fn memory_effect_pointer_target_ty<'db>(
    db: &'db DriverDataBase,
    core: &mir::CoreLib<'db>,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
) -> Option<TyId<'db>> {
    (mir::repr::effect_provider_space_for_ty(db, core, ty) == Some(AddressSpaceKind::Memory))
        .then(|| mir::repr::effect_provider_target_ty(db, core, ty))
        .flatten()
}

fn lowers_by_ref_layout<'db>(
    db: &'db DriverDataBase,
    core: &mir::CoreLib<'db>,
    mut ty: hir::analysis::ty::ty_def::TyId<'db>,
) -> bool {
    loop {
        if let Some((capability, inner)) = ty.as_capability(db) {
            if matches!(capability, CapabilityKind::View) {
                ty = inner;
                continue;
            }
            return false;
        }

        if memory_effect_pointer_target_ty(db, core, ty).is_some() {
            return false;
        }

        if let Some(inner) = mir::repr::transparent_newtype_field_ty(db, ty) {
            ty = inner;
            continue;
        }

        return ty.is_array(db)
            || ty.is_tuple(db)
            || ty
                .adt_ref(db)
                .is_some_and(|adt| matches!(adt, AdtRef::Struct(_) | AdtRef::Enum(_)));
    }
}

fn fe_ty_to_sonatina_inner<'db>(
    builder: &sonatina_ir::builder::ModuleBuilder,
    db: &'db DriverDataBase,
    core: &mir::CoreLib<'db>,
    target_layout: &TargetDataLayout,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
    cache: &mut FxHashMap<String, Option<Type>>,
    name_counter: &mut usize,
) -> Option<Type> {
    if is_erased_runtime_ty(db, target_layout, ty) {
        return Some(Type::Unit);
    }

    if let Some((capability, inner)) = ty.as_capability(db) {
        return match capability {
            CapabilityKind::View => {
                fe_ty_to_sonatina(builder, db, core, target_layout, inner, cache, name_counter)
            }
            CapabilityKind::Mut => Some(pointer_like_sonatina_ty(
                builder,
                db,
                core,
                target_layout,
                inner,
                cache,
                name_counter,
            )),
            CapabilityKind::Ref if lowers_by_ref_layout(db, core, inner) => {
                Some(pointer_like_sonatina_ty(
                    builder,
                    db,
                    core,
                    target_layout,
                    inner,
                    cache,
                    name_counter,
                ))
            }
            CapabilityKind::Ref => Some(Type::I256),
        };
    }

    if let Some(target_ty) = memory_effect_pointer_target_ty(db, core, ty) {
        return Some(pointer_like_sonatina_ty(
            builder,
            db,
            core,
            target_layout,
            target_ty,
            cache,
            name_counter,
        ));
    }

    if let Some(inner) = mir::repr::transparent_newtype_field_ty(db, ty) {
        return fe_ty_to_sonatina(builder, db, core, target_layout, inner, cache, name_counter);
    }

    let base_ty = ty.base_ty(db);
    match base_ty.data(db) {
        TyData::TyBase(TyBase::Prim(prim)) => match prim {
            PrimTy::String => None,
            PrimTy::Tuple(_) => {
                let field_tys = ty.field_types(db);
                if field_tys.is_empty() {
                    return Some(Type::Unit);
                }
                let mut sonatina_fields = Vec::with_capacity(field_tys.len());
                for ft in &field_tys {
                    sonatina_fields.push(fe_ty_to_sonatina(
                        builder,
                        db,
                        core,
                        target_layout,
                        *ft,
                        cache,
                        name_counter,
                    )?);
                }
                let id = *name_counter;
                *name_counter += 1;
                Some(builder.declare_struct_type(
                    &format!("__fe_tuple_{id}"),
                    &sonatina_fields,
                    false,
                ))
            }
            PrimTy::Array => {
                let elem_ty = layout::array_elem_ty(db, ty)?;
                let len = layout::array_len(db, ty)?;
                let sonatina_elem = fe_ty_to_sonatina(
                    builder,
                    db,
                    core,
                    target_layout,
                    elem_ty,
                    cache,
                    name_counter,
                )?;
                Some(builder.declare_array_type(sonatina_elem, len))
            }
            _ => Some(types::value_type(db, ty)),
        },
        TyData::TyBase(TyBase::Adt(adt_def)) => match adt_def.adt_ref(db) {
            AdtRef::Struct(_) => {
                let field_tys = ty.field_types(db);
                let mut sonatina_fields = Vec::with_capacity(field_tys.len());
                for ft in &field_tys {
                    sonatina_fields.push(fe_ty_to_sonatina(
                        builder,
                        db,
                        core,
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
                Some(builder.declare_struct_type(
                    &format!("__fe_{name}_{id}"),
                    &sonatina_fields,
                    false,
                ))
            }
            AdtRef::Enum(_) => {
                let payload_bytes = layout::ty_memory_size_in(db, target_layout, ty)?
                    .saturating_sub(target_layout.discriminant_size_bytes);
                let payload_words = payload_bytes / target_layout.word_size_bytes;
                let mut fields = vec![Type::I256];
                if payload_words != 0 {
                    fields.push(builder.declare_array_type(Type::I256, payload_words));
                }
                let name = adt_def
                    .adt_ref(db)
                    .name(db)
                    .map(|id| id.data(db).to_string())
                    .unwrap_or_else(|| "anon".to_string());
                let id = *name_counter;
                *name_counter += 1;
                Some(builder.declare_struct_type(&format!("__fe_{name}_{id}"), &fields, false))
            }
        },
        TyData::TyBase(TyBase::Contract(_)) | TyData::TyBase(TyBase::Func(_)) => Some(Type::Unit),
        _ => None,
    }
}

/// Checks whether a projection chain is eligible for GEP-based addressing.
///
/// Returns true when all projections are Field or Index (no VariantField, Discriminant, or Deref).
fn projections_eligible_for_gep(projections: &[ResolvedPlaceProjection<'_>]) -> bool {
    projections
        .iter()
        .all(|step| matches!(step.projection, Projection::Field(_) | Projection::Index(_)))
}

fn place_supports_gep<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    mut current_sonatina_ty: Type,
    projections: &[ResolvedPlaceProjection<'db>],
) -> bool {
    for step in projections {
        match &step.projection {
            Projection::Field(field_idx) => {
                if mir::repr::transparent_field0_inner_ty(ctx.db, step.owner.ty, *field_idx)
                    == Some(step.result.ty)
                {
                    continue;
                }

                let Some(field_sonatina_ty) = ctx.fb.module_builder.ctx.with_ty_store(|s| {
                    s.struct_def(current_sonatina_ty)
                        .and_then(|sd| sd.fields.get(*field_idx).copied())
                }) else {
                    return false;
                };
                current_sonatina_ty = field_sonatina_ty;
            }
            Projection::Index(_) => {
                let Some(elem_sonatina_ty) = ctx
                    .fb
                    .module_builder
                    .ctx
                    .with_ty_store(|s| s.array_def(current_sonatina_ty).map(|(elem, _)| elem))
                else {
                    return false;
                };
                current_sonatina_ty = elem_sonatina_ty;
            }
            _ => return false,
        }
    }

    true
}

#[derive(Debug, Clone, Copy)]
enum LoweredPlaceAddr {
    Word(ValueId),
    MemoryPtr(ValueId),
}

#[derive(Debug, Clone, Copy)]
struct LoweredPlaceTerminal<'db> {
    state: PlaceState<'db>,
    addr: LoweredPlaceAddr,
}

impl<'db> LoweredPlaceTerminal<'db> {
    fn word_addr<C: sonatina_ir::func_cursor::FuncCursor>(
        self,
        ctx: &mut LowerCtx<'_, 'db, C>,
    ) -> ValueId {
        match self.addr {
            LoweredPlaceAddr::Word(addr) => addr,
            LoweredPlaceAddr::MemoryPtr(ptr) => coerce_value_to_word(ctx, ptr),
        }
    }

    fn memory_ptr<C: sonatina_ir::func_cursor::FuncCursor>(
        self,
        ctx: &mut LowerCtx<'_, 'db, C>,
        expected_ty: Type,
    ) -> Option<ValueId> {
        match self.addr {
            LoweredPlaceAddr::MemoryPtr(ptr) => Some(coerce_value_to_type(ctx, ptr, expected_ty)),
            LoweredPlaceAddr::Word(_) => None,
        }
    }

    fn runtime_addr(self) -> ValueId {
        match self.addr {
            LoweredPlaceAddr::Word(addr) | LoweredPlaceAddr::MemoryPtr(addr) => addr,
        }
    }

    fn native_memory_ptr(self) -> Option<ValueId> {
        match self.addr {
            LoweredPlaceAddr::MemoryPtr(ptr) => Some(ptr),
            LoweredPlaceAddr::Word(_) => None,
        }
    }
}

fn lower_place_state_terminal<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    debug_place: &Place<'db>,
    state: PlaceState<'db>,
    runtime: ValueId,
) -> Result<LoweredPlaceTerminal<'db>, LowerError> {
    let address_space = state.location_address_space().ok_or_else(|| {
        LowerError::Internal(format!("non-location segment base for {debug_place:?}"))
    })?;
    Ok(LoweredPlaceTerminal {
        state,
        addr: if matches!(address_space, AddressSpaceKind::Memory) {
            LoweredPlaceAddr::MemoryPtr(
                if ctx
                    .fb
                    .type_of(runtime)
                    .is_pointer(&ctx.fb.module_builder.ctx)
                {
                    runtime
                } else {
                    let opaque_ptr_ty = ctx.fb.ptr_type(Type::I8);
                    coerce_word_addr_to_ptr(ctx, runtime, opaque_ptr_ty)
                },
            )
        } else {
            LoweredPlaceAddr::Word(coerce_value_to_word(ctx, runtime))
        },
    })
}

fn resolve_place<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
) -> Result<ResolvedPlace<'db>, LowerError> {
    mir::repr::resolve_place(ctx.db, ctx.core, &ctx.body.values, &ctx.body.locals, place).ok_or_else(
        || {
            let base = ctx.body.value(place.base);
            LowerError::Internal(format!(
                "failed to resolve MIR place {place:?} (base ty={}, repr={:?}, origin={:?}, pointer_info={:?})",
                base.ty.pretty_print(ctx.db),
                base.repr,
                base.origin,
                base.pointer_info,
            ))
        },
    )
}

fn place_yields_location_value<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
    value_ty: TyId<'db>,
    pointer_info: Option<mir::ir::PointerInfo<'db>>,
) -> Result<bool, LowerError> {
    mir::repr::place_yields_location_value(
        ctx.db,
        ctx.core,
        &ctx.body.values,
        &ctx.body.locals,
        place,
        value_ty,
        pointer_info,
    )
    .ok_or_else(|| LowerError::Internal(format!("failed to resolve place for {place:?}")))
}

fn lower_place_runtime_location_value<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
    expected_runtime_ty: Type,
) -> Result<ValueId, LowerError> {
    let terminal = lower_place_terminal(ctx, place)?;
    if expected_runtime_ty.is_pointer(&ctx.fb.module_builder.ctx)
        && let Some(memory_ptr) = terminal.memory_ptr(ctx, expected_runtime_ty)
    {
        return Ok(coerce_value_to_type(ctx, memory_ptr, expected_runtime_ty));
    }
    let word_addr = terminal.word_addr(ctx);
    Ok(coerce_value_to_type(ctx, word_addr, expected_runtime_ty))
}

fn load_from_terminal<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    terminal: LoweredPlaceTerminal<'db>,
    loaded_ty: TyId<'db>,
    expected_runtime_ty: Type,
    packed: bool,
) -> Result<ValueId, LowerError> {
    match terminal
        .state
        .location_address_space()
        .ok_or_else(|| LowerError::Internal("place terminal is not a location".to_string()))?
    {
        AddressSpaceKind::Memory => {
            let ptr_addr = terminal.native_memory_ptr().ok_or_else(|| {
                LowerError::Internal("memory place terminal missing pointer".to_string())
            })?;
            if expected_runtime_ty.is_pointer(&ctx.fb.module_builder.ctx) && !packed {
                return Ok(ctx.fb.insert_inst(
                    Mload::new(ctx.is, ptr_addr, expected_runtime_ty),
                    expected_runtime_ty,
                ));
            }
            let word = ctx
                .fb
                .insert_inst(Mload::new(ctx.is, ptr_addr, Type::I256), Type::I256);
            let raw = if packed {
                let shift = ctx.fb.make_imm_value(I256::from(248));
                ctx.fb
                    .insert_inst(Shr::new(ctx.is, shift, word), Type::I256)
            } else {
                word
            };
            Ok(apply_from_word(ctx.fb, ctx.db, raw, loaded_ty, ctx.is))
        }
        AddressSpaceKind::Storage => {
            let word_addr = terminal.word_addr(ctx);
            let raw = ctx
                .fb
                .insert_inst(EvmSload::new(ctx.is, word_addr), Type::I256);
            let loaded = apply_from_word(ctx.fb, ctx.db, raw, loaded_ty, ctx.is);
            Ok(coerce_value_to_type(ctx, loaded, expected_runtime_ty))
        }
        AddressSpaceKind::TransientStorage => {
            let word_addr = terminal.word_addr(ctx);
            let raw = ctx
                .fb
                .insert_inst(EvmTload::new(ctx.is, word_addr), Type::I256);
            let loaded = apply_from_word(ctx.fb, ctx.db, raw, loaded_ty, ctx.is);
            Ok(coerce_value_to_type(ctx, loaded, expected_runtime_ty))
        }
        AddressSpaceKind::Calldata => {
            let word_addr = terminal.word_addr(ctx);
            let raw = ctx
                .fb
                .insert_inst(EvmCalldataLoad::new(ctx.is, word_addr), Type::I256);
            let loaded = apply_from_word(ctx.fb, ctx.db, raw, loaded_ty, ctx.is);
            Ok(coerce_value_to_type(ctx, loaded, expected_runtime_ty))
        }
        AddressSpaceKind::Code => {
            let word_addr = terminal.word_addr(ctx);
            let copy_size = if packed { 1 } else { 32 };
            let size_val = ctx.fb.make_imm_value(I256::from(copy_size));
            let scratch = emit_evm_malloc_word_addr(ctx.fb, size_val, ctx.is);
            ctx.fb
                .insert_inst_no_result(EvmCodeCopy::new(ctx.is, scratch, word_addr, size_val));
            let word = ctx
                .fb
                .insert_inst(Mload::new(ctx.is, scratch, Type::I256), Type::I256);
            let raw = if packed {
                let shift = ctx.fb.make_imm_value(I256::from(248));
                ctx.fb
                    .insert_inst(Shr::new(ctx.is, shift, word), Type::I256)
            } else {
                word
            };
            let loaded = apply_from_word(ctx.fb, ctx.db, raw, loaded_ty, ctx.is);
            Ok(coerce_value_to_type(ctx, loaded, expected_runtime_ty))
        }
    }
}

fn lower_place_segment_terminal<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    debug_place: &Place<'db>,
    segment_base: ValueId,
    segment: &ResolvedPlaceSegment<'db>,
) -> Result<LoweredPlaceTerminal<'db>, LowerError> {
    let address_space = segment.base.location_address_space().ok_or_else(|| {
        LowerError::Internal(format!("non-location segment base for {debug_place:?}"))
    })?;
    if segment.projections.is_empty() {
        return lower_place_state_terminal(
            ctx,
            debug_place,
            segment.terminal_state(),
            segment_base,
        );
    }

    let is_slot_addressed = matches!(
        address_space,
        AddressSpaceKind::Storage | AddressSpaceKind::TransientStorage
    );
    if !is_slot_addressed
        && projections_eligible_for_gep(&segment.projections)
        && !layout::resolved_place_requires_packed_layout_arithmetic(
            ctx.db,
            segment.base.ty,
            &segment.projections,
        )
        .map_err(LowerError::Unsupported)?
        && let Some(sonatina_ty) = fe_ty_to_sonatina(
            &ctx.fb.module_builder,
            ctx.db,
            ctx.core,
            ctx.target_layout,
            segment.base.ty,
            ctx.gep_type_cache,
            ctx.gep_name_counter,
        )
        && place_supports_gep(ctx, sonatina_ty, &segment.projections)
    {
        let gep_ptr =
            lower_place_address_gep(ctx, &segment.projections, segment_base, sonatina_ty)?;
        return Ok(LoweredPlaceTerminal {
            state: segment.terminal_state(),
            addr: LoweredPlaceAddr::MemoryPtr(gep_ptr),
        });
    }

    let base_word = coerce_value_to_word(ctx, segment_base);
    let word_addr =
        lower_place_address_arithmetic(ctx, &segment.projections, base_word, is_slot_addressed)?;
    Ok(LoweredPlaceTerminal {
        state: segment.terminal_state(),
        addr: if matches!(address_space, AddressSpaceKind::Memory) {
            {
                let opaque_ptr_ty = ctx.fb.ptr_type(Type::I8);
                LoweredPlaceAddr::MemoryPtr(coerce_word_addr_to_ptr(ctx, word_addr, opaque_ptr_ty))
            }
        } else {
            LoweredPlaceAddr::Word(word_addr)
        },
    })
}

fn terminal_runtime_value<'db>(terminal: LoweredPlaceTerminal<'db>) -> ValueId {
    terminal.runtime_addr()
}

fn lower_place_terminal<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
) -> Result<LoweredPlaceTerminal<'db>, LowerError> {
    let resolved = resolve_place(ctx, place)?;
    let root_runtime = match ctx.body.value(place.base).origin {
        mir::ValueOrigin::Local(local) => {
            let root_local = ctx.body.spill_slots.get(&local).copied().unwrap_or(local);
            let var = ctx.local_vars.get(&root_local).copied().ok_or_else(|| {
                LowerError::Internal(format!(
                    "SSA variable not found for place root {root_local:?}"
                ))
            })?;
            ctx.fb.use_var(var)
        }
        _ => lower_value(ctx, place.base)?,
    };
    let mut current_terminal: Option<LoweredPlaceTerminal<'db>> = None;

    for (idx, segment) in resolved.segments.iter().enumerate() {
        let segment_base_runtime = match (idx == 0, segment.start_kind, current_terminal.take()) {
            (true, None, None)
            | (true, Some(mir::repr::DerefStepKind::ReuseLocation), None)
            | (true, Some(mir::repr::DerefStepKind::UseBaseValue), None) => root_runtime,
            (true, Some(mir::repr::DerefStepKind::LoadLocationValue), None) => {
                let base_terminal =
                    lower_place_state_terminal(ctx, place, segment.before, root_runtime)?;
                let runtime_ty = runtime_type_for_pointer_info(
                    &ctx.fb.module_builder,
                    ctx.db,
                    ctx.core,
                    ctx.target_layout,
                    segment.before.pointer_info.ok_or_else(|| {
                        LowerError::Internal(format!(
                            "missing pointer metadata at deref boundary for {place:?}"
                        ))
                    })?,
                    &mut *ctx.gep_type_cache,
                    &mut *ctx.gep_name_counter,
                );
                load_from_terminal(ctx, base_terminal, segment.before.ty, runtime_ty, false)?
            }
            (false, Some(mir::repr::DerefStepKind::ReuseLocation), Some(terminal)) => {
                terminal_runtime_value(terminal)
            }
            (false, Some(mir::repr::DerefStepKind::LoadLocationValue), Some(terminal)) => {
                let runtime_ty = runtime_type_for_pointer_info(
                    &ctx.fb.module_builder,
                    ctx.db,
                    ctx.core,
                    ctx.target_layout,
                    segment.before.pointer_info.ok_or_else(|| {
                        LowerError::Internal(format!(
                            "missing pointer metadata at deref boundary for {place:?}"
                        ))
                    })?,
                    &mut *ctx.gep_type_cache,
                    &mut *ctx.gep_name_counter,
                );
                load_from_terminal(ctx, terminal, segment.before.ty, runtime_ty, false)?
            }
            (false, None, Some(_))
            | (false, Some(mir::repr::DerefStepKind::UseBaseValue), Some(_))
            | (true, None, Some(_))
            | (true, Some(_), Some(_))
            | (false, _, None) => {
                return Err(LowerError::Internal(format!(
                    "invalid resolved place segment sequence for {place:?}"
                )));
            }
        };
        current_terminal = Some(lower_place_segment_terminal(
            ctx,
            place,
            segment_base_runtime,
            segment,
        )?);
    }

    current_terminal.ok_or_else(|| {
        LowerError::Internal(format!("resolved place produced no segments for {place:?}"))
    })
}

/// Computes the address for a place by walking the projection path.
///
/// For memory, computes byte offsets. For storage, computes slot offsets.
/// Returns a Sonatina ValueId representing the final address.
fn lower_place_address<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
) -> Result<ValueId, LowerError> {
    Ok(lower_place_terminal(ctx, place)?.word_addr(ctx))
}

/// GEP-based place address computation for memory-addressed struct/array paths.
fn lower_place_address_gep<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    projections: &[ResolvedPlaceProjection<'db>],
    base_val: ValueId,
    base_sonatina_ty: Type,
) -> Result<ValueId, LowerError> {
    let ptr_ty = ctx.fb.ptr_type(base_sonatina_ty);

    // Reuse pointer sources from `ptr_to_int` when possible to avoid pointless
    // `ptr_to_int -> int_to_ptr` round-trips before GEP.
    let typed_ptr = coerce_word_addr_to_ptr(ctx, base_val, ptr_ty);

    // Build GEP index list, tracking types through the chain
    let mut gep_values: SmallVec<[ValueId; 8]> = SmallVec::new();
    gep_values.push(typed_ptr);

    // Initial dereference index (standard GEP convention: index 0 dereferences the pointer)
    let zero = ctx.fb.make_imm_value(I256::zero());
    gep_values.push(zero);

    let mut current_sonatina_ty = base_sonatina_ty;

    for step in projections {
        match &step.projection {
            Projection::Field(field_idx) => {
                let owner_fe_ty = step.owner.ty;
                if let Some(inner_ty) =
                    mir::repr::transparent_field0_inner_ty(ctx.db, owner_fe_ty, *field_idx)
                    && inner_ty == step.result.ty
                {
                    continue;
                }

                let idx_val = ctx.fb.make_imm_value(I256::from(*field_idx as u64));
                gep_values.push(idx_val);

                // Navigate Fe type
                // Navigate sonatina type to the field's type
                current_sonatina_ty = sonatina_struct_field_ty(
                    ctx,
                    current_sonatina_ty,
                    *field_idx,
                    owner_fe_ty,
                    step.result.ty,
                )?;
            }
            Projection::Index(idx_source) => {
                let idx_val = match idx_source {
                    IndexSource::Constant(idx) => ctx.fb.make_imm_value(I256::from(*idx as u64)),
                    IndexSource::Dynamic(value_id) => lower_value(ctx, *value_id)?,
                };
                gep_values.push(idx_val);

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

    Ok(gep_result)
}

fn lower_place_memory_ptr<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
) -> Result<ValueId, LowerError> {
    let lowered = lower_place_terminal(ctx, place)?;
    let Some(ptr) = lowered.native_memory_ptr() else {
        return Err(LowerError::Internal(format!(
            "requested memory pointer for non-memory place: {place:?}"
        )));
    };
    Ok(ptr)
}

/// Resolves the sonatina type of a struct field by index.
fn sonatina_struct_field_ty<C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, '_, C>,
    struct_ty: Type,
    field_idx: usize,
    owner_fe_ty: hir::analysis::ty::ty_def::TyId<'_>,
    field_fe_ty: hir::analysis::ty::ty_def::TyId<'_>,
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
        None => Err(LowerError::Internal(format!(
            "gep: expected sonatina struct type for Field projection, got {struct_ty:?} while projecting field {field_idx} of {} (field type {})",
            owner_fe_ty.pretty_print(ctx.db),
            field_fe_ty.pretty_print(ctx.db)
        ))),
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
    projections: &[ResolvedPlaceProjection<'db>],
    mut base_val: ValueId,
    is_slot_addressed: bool,
) -> Result<ValueId, LowerError> {
    let mut total_offset: usize = 0;

    for step in projections {
        match &step.projection {
            Projection::Field(field_idx) => {
                if mir::repr::transparent_field0_inner_ty(ctx.db, step.owner.ty, *field_idx)
                    == Some(step.result.ty)
                {
                    continue;
                }
                // Use slot-based offsets for storage, byte-based for memory
                total_offset += if is_slot_addressed {
                    layout::field_offset_slots(ctx.db, step.owner.ty, *field_idx)
                } else {
                    layout::field_offset_memory_in(
                        ctx.db,
                        ctx.target_layout,
                        step.owner.ty,
                        *field_idx,
                    )
                };
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
            }
            Projection::Discriminant => {
                // Discriminant is always at offset 0.
            }
            Projection::Index(idx_source) => {
                let stride = if is_slot_addressed {
                    layout::array_elem_stride_slots(ctx.db, step.owner.ty)
                } else {
                    layout::array_elem_stride_memory_in(ctx.db, ctx.target_layout, step.owner.ty)
                }
                .ok_or_else(|| {
                    LowerError::Unsupported(format!(
                        "projection: array index on non-array type `{}`",
                        step.owner.ty.pretty_print(ctx.db),
                    ))
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
                    let ret_ty = ctx.current_function_metadata.ret.ok_or_else(|| {
                        LowerError::Internal(
                            "return value present but current function has no runtime return type"
                                .to_string(),
                        )
                    })?;
                    let value_ty = ctx
                        .body
                        .values
                        .get(v.index())
                        .ok_or_else(|| LowerError::Internal("unknown return value".to_string()))?
                        .ty;
                    let value = lower_value(ctx, *v)?;
                    Some(coerce_value_to_runtime_ty(ctx, value, value_ty, ret_ty))
                } else {
                    None
                };
                let ret_args = ret_sonatina
                    .map(|value| smallvec1::smallvec![value].into())
                    .unwrap_or_default();
                ctx.fb.insert_inst_no_result(Return::new(ctx.is, ret_args));
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

            if targets.is_empty() {
                ctx.fb
                    .insert_inst_no_result(Jump::new(ctx.is, default_block));
                return Ok(());
            }

            let discr_ty = ctx.fb.type_of(discr_val);
            let mut cases = Vec::with_capacity(targets.len());
            for target in targets {
                let value = make_int_immediate(
                    ctx.fb,
                    biguint_to_i256(&target.value.as_biguint()),
                    discr_ty,
                );
                let dest = ctx.block_map[&target.block];
                cases.push((value, dest));
            }

            ctx.fb.insert_inst_no_result(BrTable::new(
                ctx.is,
                discr_val,
                Some(default_block),
                cases,
            ));
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
                            let addr = coerce_value_to_word(ctx, addr);
                            ctx.fb
                                .insert_inst_no_result(EvmSelfDestruct::new(ctx.is, addr));
                            return Ok(());
                        }
                        _ => {}
                    }
                }

                if let Some(builtin) = call.builtin_terminator {
                    match builtin {
                        BuiltinTerminatorKind::Abort => {
                            let zero = ctx.fb.make_imm_value(I256::zero());
                            ctx.fb
                                .insert_inst_no_result(EvmRevert::new(ctx.is, zero, zero));
                            return Ok(());
                        }
                    }
                }

                let Some(func_ref) = ctx.name_map.get(callee_name) else {
                    return Err(LowerError::Internal(format!(
                        "missing sonatina callee for terminating call: {callee_name}"
                    )));
                };

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
                        let addr = coerce_value_to_word(ctx, *addr);
                        ctx.fb
                            .insert_inst_no_result(EvmReturn::new(ctx.is, addr, *len));
                    }
                    IntrinsicOp::Revert => {
                        let [addr, len] = lowered_args.as_slice() else {
                            return Err(LowerError::Internal(
                                "revert requires 2 arguments".to_string(),
                            ));
                        };
                        let addr = coerce_value_to_word(ctx, *addr);
                        ctx.fb
                            .insert_inst_no_result(EvmRevert::new(ctx.is, addr, *len));
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
        return Ok(zero_value_for_type(
            ctx.fb,
            ctx.local_runtime_types[dest.index()],
            ctx.is,
        ));
    }

    // TODO: Remove this fallback once array stack allocations are proven safe across
    // ref/reborrow call paths. Without this guard, `view_param_local_ref_take_reverse`
    // can regress at runtime when `[i256; 8]` lowers to `alloca`.
    if alloc_ty.is_array(ctx.db) {
        let size_val = ctx.fb.make_imm_value(I256::from(size_bytes as u64));
        return Ok(emit_evm_malloc_ptr(ctx.fb, size_val, ctx.is));
    }

    if local_may_escape(ctx, dest) {
        let size_val = ctx.fb.make_imm_value(I256::from(size_bytes as u64));
        return Ok(emit_evm_malloc_ptr(ctx.fb, size_val, ctx.is));
    }

    let alloca_ty = fe_ty_to_sonatina(
        &ctx.fb.module_builder,
        ctx.db,
        ctx.core,
        ctx.target_layout,
        alloc_ty,
        ctx.gep_type_cache,
        ctx.gep_name_counter,
    )
    .unwrap_or_else(|| ctx.fb.declare_array_type(Type::I8, size_bytes));

    Ok(emit_alloca_ptr(ctx.fb, alloca_ty, ctx.is))
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

    let val = lower_value(ctx, value)?;
    store_runtime_value_to_place(ctx, place, value_ty, val)
}

fn store_typed_to_place<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
    val: ValueId,
    stored_ty: TyId<'db>,
) -> Result<(), LowerError> {
    let lowered = lower_place_terminal(ctx, place)?;
    match lowered
        .state
        .location_address_space()
        .ok_or_else(|| LowerError::Internal(format!("store target is not a location: {place:?}")))?
    {
        AddressSpaceKind::Memory => {
            if layout::is_packed_scalar_array_access(ctx.db, ctx.body, place, stored_ty)
                .map_err(LowerError::Unsupported)?
            {
                let addr = lowered.word_addr(ctx);
                ctx.fb
                    .insert_inst_no_result(EvmMstore8::new(ctx.is, addr, val));
            } else {
                let ptr_addr = lowered.native_memory_ptr().ok_or_else(|| {
                    LowerError::Internal("memory place terminal missing pointer".to_string())
                })?;
                ctx.fb
                    .insert_inst_no_result(Mstore::new(ctx.is, ptr_addr, val, Type::I256));
            }
        }
        AddressSpaceKind::Storage => {
            let word_addr = lowered.word_addr(ctx);
            ctx.fb
                .insert_inst_no_result(EvmSstore::new(ctx.is, word_addr, val));
        }
        AddressSpaceKind::TransientStorage => {
            let word_addr = lowered.word_addr(ctx);
            ctx.fb
                .insert_inst_no_result(EvmTstore::new(ctx.is, word_addr, val));
        }
        AddressSpaceKind::Calldata => {
            return Err(LowerError::Unsupported("store to calldata".to_string()));
        }
        AddressSpaceKind::Code => {
            return Err(LowerError::Unsupported(
                "cannot store to code space".to_string(),
            ));
        }
    }
    Ok(())
}

fn store_runtime_value_to_place<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
    stored_ty: hir::analysis::ty::ty_def::TyId<'db>,
    value: ValueId,
) -> Result<(), LowerError> {
    if is_transparent_field0_place(ctx, place) {
        let base_place = Place::new(place.base, mir::MirProjectionPath::new());
        return store_runtime_value_to_place(ctx, &base_place, stored_ty, value);
    }

    let lowered = lower_place_terminal(ctx, place)?;
    match lowered
        .state
        .location_address_space()
        .ok_or_else(|| LowerError::Internal(format!("store target is not a location: {place:?}")))?
    {
        AddressSpaceKind::Memory => {
            if layout::is_packed_scalar_array_access(ctx.db, ctx.body, place, stored_ty)
                .map_err(LowerError::Unsupported)?
            {
                let addr = lowered.word_addr(ctx);
                let val = apply_to_word(ctx.fb, ctx.db, value, stored_ty, ctx.is);
                ctx.fb
                    .insert_inst_no_result(EvmMstore8::new(ctx.is, addr, val));
                return Ok(());
            }
            let ptr_addr = lowered.native_memory_ptr().ok_or_else(|| {
                LowerError::Internal("memory place terminal missing pointer".to_string())
            })?;
            let value_ty = ctx.fb.type_of(value);
            if value_ty.is_pointer(&ctx.fb.module_builder.ctx) {
                ctx.fb
                    .insert_inst_no_result(Mstore::new(ctx.is, ptr_addr, value, value_ty));
                return Ok(());
            }

            let val = apply_to_word(ctx.fb, ctx.db, value, stored_ty, ctx.is);
            ctx.fb
                .insert_inst_no_result(Mstore::new(ctx.is, ptr_addr, val, Type::I256));
            Ok(())
        }
        AddressSpaceKind::Storage => {
            let raw = coerce_value_to_word(ctx, value);
            let val = apply_to_word(ctx.fb, ctx.db, raw, stored_ty, ctx.is);
            let word_addr = lowered.word_addr(ctx);
            ctx.fb
                .insert_inst_no_result(EvmSstore::new(ctx.is, word_addr, val));
            Ok(())
        }
        AddressSpaceKind::TransientStorage => {
            let raw = coerce_value_to_word(ctx, value);
            let val = apply_to_word(ctx.fb, ctx.db, raw, stored_ty, ctx.is);
            let word_addr = lowered.word_addr(ctx);
            ctx.fb
                .insert_inst_no_result(EvmTstore::new(ctx.is, word_addr, val));
            Ok(())
        }
        AddressSpaceKind::Calldata => Err(LowerError::Unsupported("store to calldata".to_string())),
        AddressSpaceKind::Code => Err(LowerError::Unsupported(
            "cannot store to code space".to_string(),
        )),
    }
}

fn load_place_runtime<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
    loaded_ty: hir::analysis::ty::ty_def::TyId<'db>,
    expected_runtime_ty: Type,
) -> Result<ValueId, LowerError> {
    if is_erased_runtime_ty(ctx.db, ctx.target_layout, loaded_ty) {
        return Ok(zero_value_for_type(ctx.fb, expected_runtime_ty, ctx.is));
    }

    if is_transparent_field0_place(ctx, place) {
        let base = lower_value(ctx, place.base)?;
        if expected_runtime_ty.is_pointer(&ctx.fb.module_builder.ctx) {
            return Ok(coerce_value_to_type(ctx, base, expected_runtime_ty));
        }
        let raw = coerce_value_to_word(ctx, base);
        let loaded = apply_from_word(ctx.fb, ctx.db, raw, loaded_ty, ctx.is);
        return Ok(coerce_value_to_type(ctx, loaded, expected_runtime_ty));
    }

    let packed = layout::is_packed_scalar_array_access(ctx.db, ctx.body, place, loaded_ty)
        .map_err(LowerError::Unsupported)?;
    let lowered = lower_place_terminal(ctx, place)?;
    load_from_terminal(ctx, lowered, loaded_ty, expected_runtime_ty, packed)
}

fn is_transparent_field0_place<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    place: &Place<'db>,
) -> bool {
    if place.projection.is_empty() {
        return false;
    }

    let Some(base_value) = ctx.body.values.get(place.base.index()) else {
        return false;
    };
    if !matches!(base_value.repr, mir::ValueRepr::Word) {
        return false;
    }

    mir::repr::peel_transparent_field0_projection_path(ctx.db, base_value.ty, &place.projection)
        .is_some()
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

    if let Some(info) =
        mir::repr::pointer_info_for_ty(ctx.db, ctx.core, value_ty, AddressSpaceKind::Memory)
    {
        let runtime_ty = runtime_type_for_pointer_info(
            &ctx.fb.module_builder,
            ctx.db,
            ctx.core,
            ctx.target_layout,
            info,
            &mut *ctx.gep_type_cache,
            &mut *ctx.gep_name_counter,
        );
        let loaded = load_place_runtime(ctx, src_place, value_ty, runtime_ty)?;
        return store_runtime_value_to_place(ctx, dst_place, value_ty, loaded);
    }

    let dst_space = ctx.body.place_address_space(dst_place);
    let src_space = ctx.body.place_address_space(src_place);

    if value_ty.is_array(ctx.db) {
        if dst_space == AddressSpaceKind::Memory
            && src_space == AddressSpaceKind::Code
            && src_place.projection.is_empty()
        {
            let copy_size = if let Some(mir::ValueData {
                origin: mir::ValueOrigin::ConstRegion(region_id),
                ..
            }) = ctx.body.values.get(src_place.base.index())
            {
                Some(ctx.body.const_region(*region_id).bytes.len())
            } else {
                layout::ty_memory_size_in(ctx.db, ctx.target_layout, value_ty)
            };
            if let Some(size) = copy_size
                && size > 0
            {
                let dst_addr = lower_place_address(ctx, dst_place)?;
                let src_addr = lower_place_address(ctx, src_place)?;
                let size_val = ctx.fb.make_imm_value(I256::from(size as u64));
                ctx.fb
                    .insert_inst_no_result(EvmCodeCopy::new(ctx.is, dst_addr, src_addr, size_val));
            }
            return Ok(());
        }

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

    let loaded = load_place_runtime(
        ctx,
        src_place,
        value_ty,
        types::value_type(ctx.db, value_ty),
    )?;
    store_runtime_value_to_place(ctx, dst_place, value_ty, loaded)
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
    let discr = load_place_runtime(
        ctx,
        src_place,
        discr_ty,
        types::value_type(ctx.db, discr_ty),
    )?;
    store_typed_to_place(ctx, dst_place, discr, discr_ty)?;

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
    let cond_ty = fb.type_of(cond);
    if cond_ty == Type::I1 {
        cond
    } else {
        let zero = types::zero_value(fb, cond_ty);
        fb.insert_inst(Ne::new(is, cond, zero), Type::I1)
    }
}

fn emit_evm_malloc_ptr<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    size: ValueId,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> ValueId {
    let ptr_ty = fb.ptr_type(Type::I8);
    fb.insert_inst(EvmMalloc::new(is, size), ptr_ty)
}

fn emit_evm_malloc_word_addr<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    size: ValueId,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> ValueId {
    let ptr = emit_evm_malloc_ptr(fb, size, is);
    fb.insert_inst(PtrToInt::new(is, ptr, Type::I256), Type::I256)
}

fn ensure_const_data_global<C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, '_, C>,
    data: &[u8],
) -> GlobalVariableRef {
    if let Some(&existing) = ctx.const_data_globals.get(data) {
        return existing;
    }

    let elems: Vec<GvInitializer> = data
        .iter()
        .map(|&b| GvInitializer::make_imm(Immediate::I8(b as i8)))
        .collect();
    let array_init = GvInitializer::make_array(elems);

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
}

fn emit_alloca_ptr<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    alloca_ty: Type,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> ValueId {
    let ptr_ty = fb.ptr_type(alloca_ty);
    fb.insert_inst(Alloca::new(is, alloca_ty), ptr_ty)
}

fn emit_alloca_word_addr<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    alloca_ty: Type,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> ValueId {
    let ptr = emit_alloca_ptr(fb, alloca_ty, is);
    fb.insert_inst(PtrToInt::new(is, ptr, Type::I256), Type::I256)
}

fn const_usize_value<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &sonatina_ir::builder::FunctionBuilder<C>,
    value: ValueId,
) -> Option<usize> {
    let imm = fb.func.dfg.value_imm(value)?;
    if imm.is_negative() {
        return None;
    }
    Some(imm.as_usize())
}

fn local_may_escape<C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &LowerCtx<'_, '_, C>,
    local: mir::LocalId,
) -> bool {
    ctx.ptr_escape_summary
        .and_then(|summary| summary.local_alloc_may_escape.get(local.index()))
        .copied()
        .unwrap_or(true)
}

fn local_place_root_slot_may_escape<C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &LowerCtx<'_, '_, C>,
    local: mir::LocalId,
) -> bool {
    let Some(summary) = ctx.ptr_escape_summary else {
        return true;
    };
    let mut arg_locals = ctx
        .body
        .param_locals
        .iter()
        .chain(ctx.body.effect_param_locals.iter())
        .copied();
    let Some(arg_idx) = arg_locals.position(|arg_local| arg_local == local) else {
        return true;
    };
    summary
        .arg_value_may_escape
        .get(arg_idx)
        .copied()
        .unwrap_or(true)
}

fn coerce_value_to_word<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    value: ValueId,
) -> ValueId {
    if ctx.fb.type_of(value).is_pointer(&ctx.fb.module_builder.ctx) {
        return ctx
            .fb
            .insert_inst(PtrToInt::new(ctx.is, value, Type::I256), Type::I256);
    }

    value
}

fn coerce_value_to_type<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    value: ValueId,
    expected_ty: Type,
) -> ValueId {
    let actual_ty = ctx.fb.type_of(value);
    if actual_ty == expected_ty {
        return value;
    }

    let actual_is_ptr = actual_ty.is_pointer(&ctx.fb.module_builder.ctx);
    let expected_is_ptr = expected_ty.is_pointer(&ctx.fb.module_builder.ctx);
    match (actual_is_ptr, expected_is_ptr) {
        (true, false) => ctx
            .fb
            .insert_inst(PtrToInt::new(ctx.is, value, expected_ty), expected_ty),
        (false, true) => coerce_word_addr_to_ptr(ctx, value, expected_ty),
        (true, true) => bitcast_ptr(ctx, value, expected_ty),
        (false, false) => value,
    }
}

fn coerce_value_to_runtime_ty<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    value: ValueId,
    value_ty: TyId<'db>,
    expected_ty: Type,
) -> ValueId {
    let actual_ty = ctx.fb.type_of(value);
    let actual_is_ptr = actual_ty.is_pointer(&ctx.fb.module_builder.ctx);
    let expected_is_ptr = expected_ty.is_pointer(&ctx.fb.module_builder.ctx);
    if actual_is_ptr || expected_is_ptr {
        return coerce_value_to_type(ctx, value, expected_ty);
    }

    coerce_runtime_value(ctx.fb, ctx.db, value, value_ty, expected_ty, ctx.is)
}

fn coerce_word_addr_to_ptr<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    addr: ValueId,
    ptr_ty: Type,
) -> ValueId {
    if ctx.fb.type_of(addr).is_pointer(&ctx.fb.module_builder.ctx) {
        return bitcast_ptr(ctx, addr, ptr_ty);
    }

    if let Some(from_ptr) = ptr_source_from_word_addr(ctx, addr) {
        return bitcast_ptr(ctx, from_ptr, ptr_ty);
    }

    ctx.fb
        .insert_inst(IntToPtr::new(ctx.is, addr, ptr_ty), ptr_ty)
}

fn ptr_source_from_word_addr<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    value: ValueId,
) -> Option<ValueId> {
    let Value::Inst { inst, .. } = ctx.fb.func.dfg.value(value) else {
        return None;
    };
    let ptr_to_int = sonatina_ir::inst::downcast::<&PtrToInt>(ctx.is, ctx.fb.func.dfg.inst(*inst))?;
    Some(*ptr_to_int.from())
}

fn bitcast_ptr<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    ptr: ValueId,
    ptr_ty: Type,
) -> ValueId {
    if ctx.fb.type_of(ptr) == ptr_ty {
        return ptr;
    }
    ctx.fb
        .insert_inst(Bitcast::new(ctx.is, ptr, ptr_ty), ptr_ty)
}

fn ensure_overflow_revert_block<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
) -> Result<BlockId, LowerError> {
    if let Some(block) = *ctx.overflow_revert_block {
        return Ok(block);
    }

    let origin_block = ctx
        .fb
        .current_block()
        .ok_or_else(|| LowerError::Internal("missing current block".to_string()))?;
    let revert_block = ctx.fb.append_block();
    ctx.fb.switch_to_block(revert_block);
    let zero = ctx.fb.make_imm_value(I256::zero());
    ctx.fb
        .insert_inst_no_result(EvmRevert::new(ctx.is, zero, zero));
    ctx.fb.switch_to_block(origin_block);
    *ctx.overflow_revert_block = Some(revert_block);
    Ok(revert_block)
}

/// Emit a conditional branch to the shared overflow revert block if `overflow_flag` (I1) is true.
fn emit_overflow_revert<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    overflow_flag: ValueId,
) -> Result<(), LowerError> {
    let revert_block = ensure_overflow_revert_block(ctx)?;
    let continue_block = ctx.fb.append_block();
    ctx.fb
        .insert_inst_no_result(Br::new(ctx.is, overflow_flag, revert_block, continue_block));
    ctx.fb.switch_to_block(continue_block);
    Ok(())
}

fn checked_intrinsic_prim<'db>(
    db: &'db DriverDataBase,
    ty: hir::analysis::ty::ty_def::TyId<'db>,
) -> Result<PrimTy, LowerError> {
    let base_ty = ty.base_ty(db);
    let TyData::TyBase(TyBase::Prim(prim)) = base_ty.data(db) else {
        return Err(LowerError::Internal(format!(
            "checked intrinsic type must be a primitive integer, got `{}`",
            ty.pretty_print(db)
        )));
    };
    if !prim.is_integral() {
        return Err(LowerError::Internal(format!(
            "checked intrinsic type must be integral, got `{}`",
            ty.pretty_print(db)
        )));
    }
    Ok(*prim)
}

fn checked_intrinsic_value_type(prim: PrimTy) -> Result<Type, LowerError> {
    types::prim_scalar_type(prim).ok_or_else(|| {
        LowerError::Internal(format!(
            "checked intrinsic type must be integral, got `{prim:?}`"
        ))
    })
}

fn lower_checked_operand<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
    value: ValueId,
    op_ty: Type,
    signed: bool,
) -> ValueId {
    cast_int_value(fb, is, value, op_ty, signed)
}

fn lower_checked_intrinsic<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, 'db, C>,
    checked: CheckedIntrinsic<'db>,
    args: &[mir::ir::ValueId],
) -> Result<ValueId, LowerError> {
    let prim = checked_intrinsic_prim(ctx.db, checked.ty)?;
    let op_ty = checked_intrinsic_value_type(prim)?;
    let signed = prim_is_signed(prim);

    match checked.op {
        CheckedArithmeticOp::Add => {
            let [a, b] = args else {
                return Err(LowerError::Internal(format!(
                    "checked add expects 2 arguments, got {}",
                    args.len()
                )));
            };
            let lhs_word = lower_value(ctx, *a)?;
            let rhs_word = lower_value(ctx, *b)?;
            let lhs = lower_checked_operand(ctx.fb, ctx.is, lhs_word, op_ty, signed);
            let rhs = lower_checked_operand(ctx.fb, ctx.is, rhs_word, op_ty, signed);
            let [raw, overflow] = if signed {
                ctx.fb.insert_saddo(lhs, rhs)
            } else {
                ctx.fb.insert_uaddo(lhs, rhs)
            };
            emit_overflow_revert(ctx, overflow)?;
            Ok(raw)
        }
        CheckedArithmeticOp::Sub => {
            let [a, b] = args else {
                return Err(LowerError::Internal(format!(
                    "checked sub expects 2 arguments, got {}",
                    args.len()
                )));
            };
            let lhs_word = lower_value(ctx, *a)?;
            let rhs_word = lower_value(ctx, *b)?;
            let lhs = lower_checked_operand(ctx.fb, ctx.is, lhs_word, op_ty, signed);
            let rhs = lower_checked_operand(ctx.fb, ctx.is, rhs_word, op_ty, signed);
            let [raw, overflow] = if signed {
                ctx.fb.insert_ssubo(lhs, rhs)
            } else {
                ctx.fb.insert_usubo(lhs, rhs)
            };
            emit_overflow_revert(ctx, overflow)?;
            Ok(raw)
        }
        CheckedArithmeticOp::Mul => {
            let [a, b] = args else {
                return Err(LowerError::Internal(format!(
                    "checked mul expects 2 arguments, got {}",
                    args.len()
                )));
            };
            let lhs_word = lower_value(ctx, *a)?;
            let rhs_word = lower_value(ctx, *b)?;
            let lhs = lower_checked_operand(ctx.fb, ctx.is, lhs_word, op_ty, signed);
            let rhs = lower_checked_operand(ctx.fb, ctx.is, rhs_word, op_ty, signed);
            let [raw, overflow] = if signed {
                ctx.fb.insert_smulo(lhs, rhs)
            } else {
                ctx.fb.insert_umulo(lhs, rhs)
            };
            emit_overflow_revert(ctx, overflow)?;
            Ok(raw)
        }
        CheckedArithmeticOp::Div => {
            let [a, b] = args else {
                return Err(LowerError::Internal(format!(
                    "checked div expects 2 arguments, got {}",
                    args.len()
                )));
            };
            let lhs_word = lower_value(ctx, *a)?;
            let rhs_word = lower_value(ctx, *b)?;
            let lhs = lower_checked_operand(ctx.fb, ctx.is, lhs_word, op_ty, signed);
            let rhs = lower_checked_operand(ctx.fb, ctx.is, rhs_word, op_ty, signed);
            let [raw, overflow] = if signed {
                ctx.fb.insert_evm_sdivo(lhs, rhs)
            } else {
                ctx.fb.insert_evm_udivo(lhs, rhs)
            };
            emit_overflow_revert(ctx, overflow)?;
            Ok(raw)
        }
        CheckedArithmeticOp::Rem => {
            let [a, b] = args else {
                return Err(LowerError::Internal(format!(
                    "checked rem expects 2 arguments, got {}",
                    args.len()
                )));
            };
            let lhs_word = lower_value(ctx, *a)?;
            let rhs_word = lower_value(ctx, *b)?;
            let lhs = lower_checked_operand(ctx.fb, ctx.is, lhs_word, op_ty, signed);
            let rhs = lower_checked_operand(ctx.fb, ctx.is, rhs_word, op_ty, signed);
            let [raw, overflow] = if signed {
                ctx.fb.insert_evm_smodo(lhs, rhs)
            } else {
                ctx.fb.insert_evm_umodo(lhs, rhs)
            };
            emit_overflow_revert(ctx, overflow)?;
            Ok(raw)
        }
        CheckedArithmeticOp::Neg => {
            let [arg] = args else {
                return Err(LowerError::Internal(format!(
                    "checked neg expects 1 argument, got {}",
                    args.len()
                )));
            };
            if !signed {
                return Err(LowerError::Internal(format!(
                    "checked neg is not defined for unsigned type `{}`",
                    checked.ty.pretty_print(ctx.db)
                )));
            }
            let val_word = lower_value(ctx, *arg)?;
            let val = lower_checked_operand(ctx.fb, ctx.is, val_word, op_ty, true);
            let [raw, overflow] = ctx.fb.insert_snego(val);
            emit_overflow_revert(ctx, overflow)?;
            Ok(raw)
        }
    }
}

/// Maps a raw core intrinsic suffix to the primitive type it operates on.
const INTRINSIC_SUFFIX_TYPES: &[(&str, PrimTy)] = &[
    ("_u8", PrimTy::U8),
    ("_u16", PrimTy::U16),
    ("_u32", PrimTy::U32),
    ("_u64", PrimTy::U64),
    ("_u128", PrimTy::U128),
    ("_u256", PrimTy::U256),
    ("_usize", PrimTy::Usize),
    ("_i8", PrimTy::I8),
    ("_i16", PrimTy::I16),
    ("_i32", PrimTy::I32),
    ("_i64", PrimTy::I64),
    ("_i128", PrimTy::I128),
    ("_i256", PrimTy::I256),
    ("_isize", PrimTy::Isize),
    ("_bool", PrimTy::Bool),
];

fn intrinsic_name_parts(callee_name: &str) -> Option<(&str, PrimTy)> {
    INTRINSIC_SUFFIX_TYPES.iter().find_map(|(suffix, prim)| {
        callee_name
            .strip_suffix(suffix)
            .and_then(|prefix| prefix.strip_prefix("__"))
            .map(|op| (op, *prim))
    })
}

fn intrinsic_value_type(prim: PrimTy) -> Type {
    types::prim_scalar_type(prim).unwrap_or(Type::I256)
}

fn lower_intrinsic_operand<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
    value: ValueId,
    prim: PrimTy,
    op_ty: Type,
) -> ValueId {
    if prim == PrimTy::Bool {
        condition_to_i1(fb, value, is)
    } else {
        cast_int_value(fb, is, value, op_ty, prim_is_signed(prim))
    }
}

/// Try to lower a `__<op>_<type>` core numeric intrinsic to native Sonatina
/// instructions. Returns `Ok(Some(value))` if the intrinsic was handled,
/// `Ok(None)` if the callee name is not a recognized intrinsic.
fn try_lower_numeric_intrinsic<C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, '_, C>,
    callee_name: &str,
    args: &[mir::ir::ValueId],
) -> Result<Option<ValueId>, LowerError> {
    if !callee_name.starts_with("__") {
        return Ok(None);
    }

    // Strip the type suffix to get the operation name.
    let Some((op, prim)) = intrinsic_name_parts(callee_name) else {
        return Ok(None);
    };
    let op_ty = intrinsic_value_type(prim);
    let signed = prim_is_signed(prim);

    if op.starts_with("checked_") {
        return Err(LowerError::Internal(format!(
            "checked arithmetic intrinsic `{callee_name}` must be lowered via typed checked metadata"
        )));
    }

    match op {
        // Emit Sonatina ops at the intrinsic's actual width and let the EVM
        // legalizer normalize masks/sign extension for sub-word integers.
        "lt" | "gt" | "eq" | "ne" | "le" | "ge" => {
            let (lhs_word, rhs_word) = lower_binary_args(ctx, callee_name, args)?;
            let lhs = lower_intrinsic_operand(ctx.fb, ctx.is, lhs_word, prim, op_ty);
            let rhs = lower_intrinsic_operand(ctx.fb, ctx.is, rhs_word, prim, op_ty);
            let cmp = match op {
                "lt" => {
                    if signed {
                        ctx.fb.insert_inst(Slt::new(ctx.is, lhs, rhs), Type::I1)
                    } else {
                        ctx.fb.insert_inst(Lt::new(ctx.is, lhs, rhs), Type::I1)
                    }
                }
                "gt" => {
                    if signed {
                        ctx.fb.insert_inst(Slt::new(ctx.is, rhs, lhs), Type::I1)
                    } else {
                        ctx.fb.insert_inst(Gt::new(ctx.is, lhs, rhs), Type::I1)
                    }
                }
                "eq" => ctx.fb.insert_inst(Eq::new(ctx.is, lhs, rhs), Type::I1),
                "ne" => {
                    let eq = ctx.fb.insert_inst(Eq::new(ctx.is, lhs, rhs), Type::I1);
                    ctx.fb.insert_inst(IsZero::new(ctx.is, eq), Type::I1)
                }
                "le" => {
                    let gt = if signed {
                        ctx.fb.insert_inst(Slt::new(ctx.is, rhs, lhs), Type::I1)
                    } else {
                        ctx.fb.insert_inst(Gt::new(ctx.is, lhs, rhs), Type::I1)
                    };
                    ctx.fb.insert_inst(IsZero::new(ctx.is, gt), Type::I1)
                }
                "ge" => {
                    let lt = if signed {
                        ctx.fb.insert_inst(Slt::new(ctx.is, lhs, rhs), Type::I1)
                    } else {
                        ctx.fb.insert_inst(Lt::new(ctx.is, lhs, rhs), Type::I1)
                    };
                    ctx.fb.insert_inst(IsZero::new(ctx.is, lt), Type::I1)
                }
                _ => unreachable!(),
            };
            Ok(Some(cmp))
        }

        "add" | "sub" | "mul" | "pow" | "shl" => {
            let (lhs_word, rhs_word) = lower_binary_args(ctx, callee_name, args)?;
            let lhs = lower_intrinsic_operand(ctx.fb, ctx.is, lhs_word, prim, op_ty);
            let rhs = lower_intrinsic_operand(ctx.fb, ctx.is, rhs_word, prim, op_ty);
            let raw = match op {
                "add" => ctx.fb.insert_inst(Add::new(ctx.is, lhs, rhs), op_ty),
                "sub" => ctx.fb.insert_inst(Sub::new(ctx.is, lhs, rhs), op_ty),
                "mul" => ctx.fb.insert_inst(Mul::new(ctx.is, lhs, rhs), op_ty),
                "pow" => ctx.fb.insert_inst(EvmExp::new(ctx.is, lhs, rhs), op_ty),
                "shl" => ctx.fb.insert_inst(Shl::new(ctx.is, rhs, lhs), op_ty),
                _ => unreachable!(),
            };
            Ok(Some(raw))
        }
        "div" => {
            let (lhs_word, rhs_word) = lower_binary_args(ctx, callee_name, args)?;
            let lhs = lower_intrinsic_operand(ctx.fb, ctx.is, lhs_word, prim, op_ty);
            let rhs = lower_intrinsic_operand(ctx.fb, ctx.is, rhs_word, prim, op_ty);
            let [raw, _overflow] = if signed {
                ctx.fb.insert_evm_sdivo(lhs, rhs)
            } else {
                ctx.fb.insert_evm_udivo(lhs, rhs)
            };
            Ok(Some(raw))
        }
        "rem" => {
            let (lhs_word, rhs_word) = lower_binary_args(ctx, callee_name, args)?;
            let lhs = lower_intrinsic_operand(ctx.fb, ctx.is, lhs_word, prim, op_ty);
            let rhs = lower_intrinsic_operand(ctx.fb, ctx.is, rhs_word, prim, op_ty);
            let [raw, _overflow] = if signed {
                ctx.fb.insert_evm_smodo(lhs, rhs)
            } else {
                ctx.fb.insert_evm_umodo(lhs, rhs)
            };
            Ok(Some(raw))
        }
        "shr" => {
            let (lhs_word, rhs_word) = lower_binary_args(ctx, callee_name, args)?;
            let lhs = lower_intrinsic_operand(ctx.fb, ctx.is, lhs_word, prim, op_ty);
            let rhs = lower_intrinsic_operand(ctx.fb, ctx.is, rhs_word, prim, op_ty);
            let raw = if signed {
                ctx.fb.insert_inst(Sar::new(ctx.is, rhs, lhs), op_ty)
            } else {
                ctx.fb.insert_inst(Shr::new(ctx.is, rhs, lhs), op_ty)
            };
            Ok(Some(raw))
        }

        "bitand" => {
            let (lhs_word, rhs_word) = lower_binary_args(ctx, callee_name, args)?;
            let lhs = lower_intrinsic_operand(ctx.fb, ctx.is, lhs_word, prim, op_ty);
            let rhs = lower_intrinsic_operand(ctx.fb, ctx.is, rhs_word, prim, op_ty);
            let raw = ctx.fb.insert_inst(And::new(ctx.is, lhs, rhs), op_ty);
            Ok(Some(raw))
        }
        "bitor" => {
            let (lhs_word, rhs_word) = lower_binary_args(ctx, callee_name, args)?;
            let lhs = lower_intrinsic_operand(ctx.fb, ctx.is, lhs_word, prim, op_ty);
            let rhs = lower_intrinsic_operand(ctx.fb, ctx.is, rhs_word, prim, op_ty);
            let raw = ctx.fb.insert_inst(Or::new(ctx.is, lhs, rhs), op_ty);
            Ok(Some(raw))
        }
        "bitxor" => {
            let (lhs_word, rhs_word) = lower_binary_args(ctx, callee_name, args)?;
            let lhs = lower_intrinsic_operand(ctx.fb, ctx.is, lhs_word, prim, op_ty);
            let rhs = lower_intrinsic_operand(ctx.fb, ctx.is, rhs_word, prim, op_ty);
            let raw = ctx.fb.insert_inst(Xor::new(ctx.is, lhs, rhs), op_ty);
            Ok(Some(raw))
        }

        "bitnot" => {
            let val_word = lower_unary_arg(ctx, callee_name, args)?;
            let val = lower_intrinsic_operand(ctx.fb, ctx.is, val_word, prim, op_ty);
            let raw = ctx.fb.insert_inst(Not::new(ctx.is, val), op_ty);
            Ok(Some(raw))
        }
        "not" => {
            if prim != PrimTy::Bool {
                return Err(LowerError::Internal(format!(
                    "logical not intrinsic must be bool-typed, got `{callee_name}`"
                )));
            }
            let val_word = lower_unary_arg(ctx, callee_name, args)?;
            let val = lower_intrinsic_operand(ctx.fb, ctx.is, val_word, prim, op_ty);
            let is_zero = ctx.fb.insert_inst(IsZero::new(ctx.is, val), Type::I1);
            Ok(Some(is_zero))
        }
        "neg" => {
            let val_word = lower_unary_arg(ctx, callee_name, args)?;
            let val = lower_intrinsic_operand(ctx.fb, ctx.is, val_word, prim, op_ty);
            let raw = ctx.fb.insert_inst(Neg::new(ctx.is, val), op_ty);
            Ok(Some(raw))
        }

        // Not a recognized intrinsic operation.
        _ => Ok(None),
    }
}

fn lower_binary_args<C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, '_, C>,
    callee_name: &str,
    args: &[mir::ir::ValueId],
) -> Result<(ValueId, ValueId), LowerError> {
    let [a, b] = args else {
        return Err(LowerError::Internal(format!(
            "{callee_name} requires 2 arguments"
        )));
    };
    let lhs = lower_value(ctx, *a)?;
    let rhs = lower_value(ctx, *b)?;
    Ok((lhs, rhs))
}

fn lower_unary_arg<C: sonatina_ir::func_cursor::FuncCursor>(
    ctx: &mut LowerCtx<'_, '_, C>,
    callee_name: &str,
    args: &[mir::ir::ValueId],
) -> Result<ValueId, LowerError> {
    let [a] = args else {
        return Err(LowerError::Internal(format!(
            "{callee_name} requires 1 argument"
        )));
    };
    lower_value(ctx, *a)
}
