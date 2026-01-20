//! Sonatina backend for direct EVM bytecode generation.
//!
//! This module translates Fe MIR to Sonatina IR, which is then compiled
//! to EVM bytecode without going through Yul/solc.

mod types;

use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use hir::hir_def::expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp};
use mir::{MirModule, layout::TargetDataLayout, lower_module};
use mir::ir::{AddressSpaceKind, IntrinsicOp, SyntheticValue};
use num_bigint::BigUint;
use rustc_hash::FxHashMap;
use sonatina_ir::{
    BlockId, I256, Module, Signature, Type, ValueId,
    builder::ModuleBuilder,
    func_cursor::InstInserter,
    inst::{
        arith::{Add, Mul, Neg, Shl, Shr, Sub},
        cmp::{Eq, Gt, IsZero, Lt},
        control_flow::{Br, Call, Jump, Return},
        data::{Mload, Mstore},
        evm::{EvmCalldataLoad, EvmExp, EvmSload, EvmSstore, EvmTload, EvmTstore, EvmUdiv, EvmUmod},
        logic::{And, Not, Or, Xor},
    },
    isa::{Isa, evm::Evm},
    module::{FuncRef, ModuleCtx},
};
use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

use crate::BackendError;

/// Error type for Sonatina lowering failures.
#[derive(Debug)]
pub enum LowerError {
    /// MIR lowering failed.
    MirLower(mir::MirLowerError),
    /// Unsupported MIR construct.
    Unsupported(String),
    /// Internal error.
    Internal(String),
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LowerError::MirLower(e) => write!(f, "MIR lowering failed: {e}"),
            LowerError::Unsupported(msg) => write!(f, "unsupported: {msg}"),
            LowerError::Internal(msg) => write!(f, "internal error: {msg}"),
        }
    }
}

impl std::error::Error for LowerError {}

impl From<mir::MirLowerError> for LowerError {
    fn from(e: mir::MirLowerError) -> Self {
        LowerError::MirLower(e)
    }
}

impl From<LowerError> for BackendError {
    fn from(e: LowerError) -> Self {
        BackendError::Sonatina(e.to_string())
    }
}

/// Creates a Sonatina EVM ISA for the target.
fn create_evm_isa() -> Evm {
    let triple = TargetTriple::new(
        Architecture::Evm,
        Vendor::Ethereum,
        OperatingSystem::Evm(EvmVersion::Osaka),
    );
    Evm::new(triple)
}

/// Compiles a Fe module to Sonatina IR.
pub fn compile_module(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    _layout: TargetDataLayout,
) -> Result<Module, LowerError> {
    // Lower HIR to MIR
    let mir_module = lower_module(db, top_mod)?;

    // Create Sonatina module
    let isa = create_evm_isa();
    let ctx = ModuleCtx::new(&isa);
    let module_builder = ModuleBuilder::new(ctx);

    // Create lowerer and process module
    let mut lowerer = ModuleLowerer::new(db, module_builder, &mir_module, &isa);
    lowerer.lower()?;

    Ok(lowerer.finish())
}

/// Lowers an entire MIR module to Sonatina IR.
struct ModuleLowerer<'db, 'a> {
    #[allow(unused)]
    db: &'db DriverDataBase,
    builder: ModuleBuilder,
    mir: &'a MirModule<'db>,
    isa: &'a Evm,
    /// Maps function indices to Sonatina function references.
    func_map: FxHashMap<usize, FuncRef>,
    /// Maps function symbol names to Sonatina function references.
    name_map: FxHashMap<String, FuncRef>,
}

impl<'db, 'a> ModuleLowerer<'db, 'a> {
    fn new(
        db: &'db DriverDataBase,
        builder: ModuleBuilder,
        mir: &'a MirModule<'db>,
        isa: &'a Evm,
    ) -> Self {
        Self {
            db,
            builder,
            mir,
            isa,
            func_map: FxHashMap::default(),
            name_map: FxHashMap::default(),
        }
    }

    /// Lower the entire module.
    fn lower(&mut self) -> Result<(), LowerError> {
        // First pass: declare all functions
        self.declare_functions()?;

        // Second pass: lower function bodies
        self.lower_functions()?;

        Ok(())
    }

    /// Consume the lowerer and return the built module.
    fn finish(self) -> Module {
        self.builder.build()
    }

    /// Declare all functions in the module.
    fn declare_functions(&mut self) -> Result<(), LowerError> {
        for (idx, func) in self.mir.functions.iter().enumerate() {
            let name = &func.symbol_name;
            let sig = self.lower_signature(func)?;

            let func_ref = self.builder.declare_function(sig).map_err(|e| {
                LowerError::Internal(format!("failed to declare function {name}: {e}"))
            })?;

            self.func_map.insert(idx, func_ref);
            self.name_map.insert(name.clone(), func_ref);
        }
        Ok(())
    }

    /// Lower function signatures.
    fn lower_signature(&self, func: &mir::MirFunction<'db>) -> Result<Signature, LowerError> {
        let name = &func.symbol_name;
        let linkage = sonatina_ir::Linkage::Public; // TODO: proper linkage

        // Convert parameter types - all EVM parameters are 256-bit words
        let mut params = Vec::new();
        for _ in func.body.param_locals.iter() {
            params.push(types::word_type());
        }

        // Convert return type - use Unit if function doesn't return a value
        let ret_ty = if func.returns_value {
            types::word_type() // TODO: proper return type lowering
        } else {
            types::unit_type()
        };

        Ok(Signature::new(name, linkage, &params, ret_ty))
    }

    /// Lower all function bodies.
    fn lower_functions(&mut self) -> Result<(), LowerError> {
        for (idx, func) in self.mir.functions.iter().enumerate() {
            let func_ref = self.func_map[&idx];
            self.lower_function(func_ref, func)?;
        }
        Ok(())
    }

    /// Lower a single function body.
    fn lower_function(
        &self,
        func_ref: FuncRef,
        func: &mir::MirFunction<'db>,
    ) -> Result<(), LowerError> {
        let mut fb = self.builder.func_builder::<InstInserter>(func_ref);
        let is = self.isa.inst_set();

        // Maps MIR block IDs to Sonatina block IDs
        let mut block_map: FxHashMap<mir::BasicBlockId, BlockId> = FxHashMap::default();

        // Maps MIR value IDs to Sonatina value IDs
        let mut value_map: FxHashMap<mir::ValueId, ValueId> = FxHashMap::default();

        // Maps MIR local IDs to Sonatina value IDs (for SSA)
        let mut local_map: FxHashMap<mir::LocalId, ValueId> = FxHashMap::default();

        // Create blocks
        for (idx, _block) in func.body.blocks.iter().enumerate() {
            let block_id = mir::BasicBlockId(idx as u32);
            let sonatina_block = fb.append_block();
            block_map.insert(block_id, sonatina_block);
        }

        // Get the entry block and its Sonatina equivalent
        let entry_block = func.body.entry;
        let _sonatina_entry = block_map[&entry_block];

        // Map function arguments to parameter locals
        let args = fb.args().to_vec();
        for (i, &local_id) in func.body.param_locals.iter().enumerate() {
            if i < args.len() {
                local_map.insert(local_id, args[i]);
            }
        }

        // Lower each block
        for (idx, block) in func.body.blocks.iter().enumerate() {
            let block_id = mir::BasicBlockId(idx as u32);
            let sonatina_block = block_map[&block_id];
            fb.switch_to_block(sonatina_block);

            // Lower instructions in this block
            for inst in block.insts.iter() {
                lower_instruction(
                    &mut fb,
                    inst,
                    &func.body,
                    &mut value_map,
                    &mut local_map,
                    &self.name_map,
                    is,
                )?;
            }

            // Lower terminator
            lower_terminator(
                &mut fb,
                &block.terminator,
                &block_map,
                &func.body,
                &mut value_map,
                &mut local_map,
                is,
            )?;
        }

        // Seal all blocks and finalize
        fb.seal_all();
        fb.finish();

        Ok(())
    }
}

/// Lower a MIR instruction.
fn lower_instruction<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    inst: &mir::MirInst<'db>,
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_map: &mut FxHashMap<mir::LocalId, ValueId>,
    name_map: &FxHashMap<String, FuncRef>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<(), LowerError> {
    use mir::MirInst;

    match inst {
        MirInst::Assign { dest, rvalue, .. } => {
            let result = lower_rvalue(fb, rvalue, body, value_map, local_map, name_map, is)?;
            if let (Some(dest_local), Some(result_val)) = (dest, result) {
                local_map.insert(*dest_local, result_val);
            }
        }
        MirInst::Store { place, value } => {
            // For now, only handle places without projections
            if !place.projection.is_empty() {
                return Err(LowerError::Unsupported("store with projections".to_string()));
            }

            let addr = lower_value(fb, place.base, body, value_map, local_map, is)?;
            let val = lower_value(fb, *value, body, value_map, local_map, is)?;
            let addr_space = get_place_address_space(place, body);

            match addr_space {
                AddressSpaceKind::Memory => {
                    let store = Mstore::new(is, addr, val, Type::I256);
                    fb.insert_inst_no_result(store);
                }
                AddressSpaceKind::Storage => {
                    let store = EvmSstore::new(is, addr, val);
                    fb.insert_inst_no_result(store);
                }
                AddressSpaceKind::TransientStorage => {
                    let store = EvmTstore::new(is, addr, val);
                    fb.insert_inst_no_result(store);
                }
                AddressSpaceKind::Calldata => {
                    // Calldata is read-only, cannot store to it
                    return Err(LowerError::Unsupported("store to calldata".to_string()));
                }
            }
        }
        MirInst::InitAggregate { .. } => {
            // TODO: implement aggregate initialization
        }
        MirInst::SetDiscriminant { .. } => {
            // TODO: implement discriminant setting
        }
        MirInst::BindValue { value } => {
            // Ensure the value is lowered and cached
            let _ = lower_value(fb, *value, body, value_map, local_map, is)?;
        }
    }

    Ok(())
}

/// Lower a MIR rvalue to a Sonatina value.
fn lower_rvalue<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    rvalue: &mir::Rvalue<'db>,
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_map: &mut FxHashMap<mir::LocalId, ValueId>,
    name_map: &FxHashMap<String, FuncRef>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<Option<ValueId>, LowerError> {
    use mir::Rvalue;

    match rvalue {
        Rvalue::ZeroInit => {
            // Create a zero constant
            let zero = fb.make_imm_value(I256::zero());
            Ok(Some(zero))
        }
        Rvalue::Value(value_id) => {
            let val = lower_value(fb, *value_id, body, value_map, local_map, is)?;
            Ok(Some(val))
        }
        Rvalue::Call(call) => {
            // Get the callee function reference
            let callee_name = call.resolved_name.as_ref().ok_or_else(|| {
                LowerError::Unsupported("call without resolved symbol name".to_string())
            })?;

            let func_ref = name_map.get(callee_name).ok_or_else(|| {
                LowerError::Internal(format!("unknown function: {callee_name}"))
            })?;

            // Lower arguments (regular args + effect args)
            let mut args = Vec::with_capacity(call.args.len() + call.effect_args.len());
            for &arg in &call.args {
                let val = lower_value(fb, arg, body, value_map, local_map, is)?;
                args.push(val);
            }
            for &effect_arg in &call.effect_args {
                let val = lower_value(fb, effect_arg, body, value_map, local_map, is)?;
                args.push(val);
            }

            // Emit call instruction
            let call_inst = Call::new(is, *func_ref, args.into());
            let result = fb.insert_inst(call_inst, Type::I256);
            Ok(Some(result))
        }
        Rvalue::Intrinsic { op, args } => {
            lower_intrinsic(fb, *op, args, body, value_map, local_map, is)
        }
        Rvalue::Load { place } => {
            // For now, only handle places without projections
            if !place.projection.is_empty() {
                return Err(LowerError::Unsupported("load with projections".to_string()));
            }

            let addr = lower_value(fb, place.base, body, value_map, local_map, is)?;
            let addr_space = get_place_address_space(place, body);

            let result = match addr_space {
                AddressSpaceKind::Memory => {
                    let load = Mload::new(is, addr, Type::I256);
                    fb.insert_inst(load, Type::I256)
                }
                AddressSpaceKind::Storage => {
                    let load = EvmSload::new(is, addr);
                    fb.insert_inst(load, Type::I256)
                }
                AddressSpaceKind::TransientStorage => {
                    let load = EvmTload::new(is, addr);
                    fb.insert_inst(load, Type::I256)
                }
                AddressSpaceKind::Calldata => {
                    let load = EvmCalldataLoad::new(is, addr);
                    fb.insert_inst(load, Type::I256)
                }
            };
            Ok(Some(result))
        }
        Rvalue::Alloc { .. } => {
            // TODO: implement memory allocation
            Err(LowerError::Unsupported("memory allocation".to_string()))
        }
    }
}

/// Lower a MIR value to a Sonatina value.
fn lower_value<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    value_id: mir::ValueId,
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_map: &mut FxHashMap<mir::LocalId, ValueId>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    // Check if already lowered
    if let Some(&val) = value_map.get(&value_id) {
        return Ok(val);
    }

    let value_data = &body.values[value_id.index()];
    let result = lower_value_origin(fb, &value_data.origin, body, value_map, local_map, is)?;

    value_map.insert(value_id, result);
    Ok(result)
}

/// Lower a MIR value origin to a Sonatina value.
fn lower_value_origin<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    origin: &mir::ValueOrigin<'db>,
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_map: &mut FxHashMap<mir::LocalId, ValueId>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    use mir::ValueOrigin;

    match origin {
        ValueOrigin::Synthetic(syn) => match syn {
            SyntheticValue::Int(n) => {
                let i256_val = biguint_to_i256(n);
                Ok(fb.make_imm_value(i256_val))
            }
            SyntheticValue::Bool(b) => {
                let val = if *b { I256::one() } else { I256::zero() };
                Ok(fb.make_imm_value(val))
            }
            SyntheticValue::Bytes(bytes) => {
                // Convert bytes to I256 (right-padded to 32 bytes)
                let i256_val = bytes_to_i256(bytes);
                Ok(fb.make_imm_value(i256_val))
            }
        },
        ValueOrigin::Local(local_id) => {
            local_map.get(local_id).copied().ok_or_else(|| {
                LowerError::Internal(format!("local {:?} not found", local_id))
            })
        }
        ValueOrigin::Unit => {
            // Unit is represented as 0
            Ok(fb.make_imm_value(I256::zero()))
        }
        ValueOrigin::Unary { op, inner } => {
            let inner_val = lower_value(fb, *inner, body, value_map, local_map, is)?;
            lower_unary_op(fb, *op, inner_val, is)
        }
        ValueOrigin::Binary { op, lhs, rhs } => {
            let lhs_val = lower_value(fb, *lhs, body, value_map, local_map, is)?;
            let rhs_val = lower_value(fb, *rhs, body, value_map, local_map, is)?;
            lower_binary_op(fb, *op, lhs_val, rhs_val, is)
        }
        ValueOrigin::TransparentCast { value } => {
            // Transparent cast just passes through the inner value
            lower_value(fb, *value, body, value_map, local_map, is)
        }
        ValueOrigin::ControlFlowResult { .. } => {
            // Control flow results need phi nodes - for now return zero
            // TODO: proper SSA phi handling
            Ok(fb.make_imm_value(I256::zero()))
        }
        ValueOrigin::PlaceRef(place) => {
            // Lower the base value - the place ref is a pointer
            lower_value(fb, place.base, body, value_map, local_map, is)
        }
        ValueOrigin::FieldPtr(_) => {
            // TODO: field pointer arithmetic
            Err(LowerError::Unsupported("field pointer".to_string()))
        }
        ValueOrigin::FuncItem(_) => {
            // Function items are compile-time only - return zero
            Ok(fb.make_imm_value(I256::zero()))
        }
        ValueOrigin::Expr(_) => {
            // Unlowered expressions shouldn't reach codegen
            Err(LowerError::Internal("unlowered expression in codegen".to_string()))
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
            // Logical not: iszero
            let result = fb.insert_inst(IsZero::new(is, inner), Type::I256);
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
        CompBinOp::Eq => fb.insert_inst(Eq::new(is, lhs, rhs), Type::I256),
        CompBinOp::NotEq => {
            // neq = iszero(eq(lhs, rhs))
            let eq_result = fb.insert_inst(Eq::new(is, lhs, rhs), Type::I256);
            fb.insert_inst(IsZero::new(is, eq_result), Type::I256)
        }
        CompBinOp::Lt => fb.insert_inst(Lt::new(is, lhs, rhs), Type::I256),
        CompBinOp::LtEq => {
            // lhs <= rhs  <==>  !(lhs > rhs)
            let gt_result = fb.insert_inst(Gt::new(is, lhs, rhs), Type::I256);
            fb.insert_inst(IsZero::new(is, gt_result), Type::I256)
        }
        CompBinOp::Gt => fb.insert_inst(Gt::new(is, lhs, rhs), Type::I256),
        CompBinOp::GtEq => {
            // lhs >= rhs  <==>  !(lhs < rhs)
            let lt_result = fb.insert_inst(Lt::new(is, lhs, rhs), Type::I256);
            fb.insert_inst(IsZero::new(is, lt_result), Type::I256)
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
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    op: IntrinsicOp,
    args: &[mir::ValueId],
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_map: &mut FxHashMap<mir::LocalId, ValueId>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<Option<ValueId>, LowerError> {
    // Lower all arguments first
    let mut lowered_args = Vec::with_capacity(args.len());
    for &arg in args {
        let val = lower_value(fb, arg, body, value_map, local_map, is)?;
        lowered_args.push(val);
    }

    match op {
        IntrinsicOp::Mload => {
            if let Some(&addr) = lowered_args.first() {
                let load = Mload::new(is, addr, Type::I256);
                let result = fb.insert_inst(load, Type::I256);
                Ok(Some(result))
            } else {
                Err(LowerError::Internal("mload requires address argument".to_string()))
            }
        }
        // TODO: Add more intrinsics as needed
        _ => Err(LowerError::Unsupported(format!("intrinsic {:?}", op))),
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

/// Determine the address space for a place's base value.
///
/// Returns `AddressSpaceKind::Memory` as the default if the address space cannot be determined.
fn get_place_address_space<'db>(
    place: &mir::ir::Place<'db>,
    body: &mir::MirBody<'db>,
) -> AddressSpaceKind {
    // First try to get address space from the value's repr
    let value_data = &body.values[place.base.index()];
    if let Some(space) = value_data.repr.address_space() {
        return space;
    }

    // Fall back to checking the value origin
    match &value_data.origin {
        mir::ValueOrigin::Local(local_id) => {
            if let Some(local) = body.locals.get(local_id.index()) {
                return local.address_space;
            }
        }
        mir::ValueOrigin::PlaceRef(inner_place) => {
            // Recursively check the inner place
            return get_place_address_space(inner_place, body);
        }
        mir::ValueOrigin::FieldPtr(field_ptr) => {
            return field_ptr.addr_space;
        }
        _ => {}
    }

    // Default to memory for unknown cases
    AddressSpaceKind::Memory
}

/// Lower a block terminator.
fn lower_terminator<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    term: &mir::Terminator<'db>,
    block_map: &FxHashMap<mir::BasicBlockId, BlockId>,
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_map: &mut FxHashMap<mir::LocalId, ValueId>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<(), LowerError> {
    use mir::Terminator;

    match term {
        Terminator::Return(ret_val) => {
            let ret_sonatina = if let Some(v) = ret_val {
                Some(lower_value(fb, *v, body, value_map, local_map, is)?)
            } else {
                None
            };
            fb.insert_inst_no_result(Return::new(is, ret_sonatina));
        }
        Terminator::Goto { target } => {
            let target_block = block_map[target];
            fb.insert_inst_no_result(Jump::new(is, target_block));
        }
        Terminator::Branch { cond, then_bb, else_bb } => {
            let cond_val = lower_value(fb, *cond, body, value_map, local_map, is)?;
            let then_block = block_map[then_bb];
            let else_block = block_map[else_bb];
            // Br: cond, nz_dest (then), z_dest (else)
            fb.insert_inst_no_result(Br::new(is, cond_val, then_block, else_block));
        }
        Terminator::Switch { .. } => {
            return Err(LowerError::Unsupported("switch terminator".to_string()));
        }
        Terminator::TerminatingCall(_) => {
            return Err(LowerError::Unsupported("terminating call".to_string()));
        }
        Terminator::Unreachable => {
            // For now, just return unit - TODO: proper trap
            fb.insert_inst_no_result(Return::new(is, None));
        }
    }

    Ok(())
}
