//! Sonatina backend for direct EVM bytecode generation.
//!
//! This module translates Fe MIR to Sonatina IR, which is then compiled
//! to EVM bytecode without going through Yul/solc.

mod types;

use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use hir::hir_def::expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp};
use hir::projection::{IndexSource, Projection};
use mir::{MirModule, layout, layout::TargetDataLayout, lower_module};
use mir::ir::{AddressSpaceKind, IntrinsicOp, Place, SyntheticValue};
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
        evm::{
            EvmCalldataLoad, EvmExp, EvmInvalid, EvmSload, EvmSstore,
            EvmTload, EvmTstore, EvmUdiv, EvmUmod,
        },
        logic::{And, Not, Or, Xor},
    },
    isa::{Isa, evm::Evm},
    module::{FuncRef, ModuleCtx},
    object::{Directive, Object, ObjectName, Section, SectionName},
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
    layout: TargetDataLayout,
) -> Result<Module, LowerError> {
    // Lower HIR to MIR
    let mir_module = lower_module(db, top_mod)?;

    // Create Sonatina module
    let isa = create_evm_isa();
    let ctx = ModuleCtx::new(&isa);
    let module_builder = ModuleBuilder::new(ctx);

    // Create lowerer and process module
    let mut lowerer = ModuleLowerer::new(db, module_builder, &mir_module, &isa, layout);
    lowerer.lower()?;

    Ok(lowerer.finish())
}

/// Lowers an entire MIR module to Sonatina IR.
struct ModuleLowerer<'db, 'a> {
    db: &'db DriverDataBase,
    builder: ModuleBuilder,
    mir: &'a MirModule<'db>,
    isa: &'a Evm,
    target_layout: TargetDataLayout,
    /// Maps function indices to Sonatina function references.
    func_map: FxHashMap<usize, FuncRef>,
    /// Maps function symbol names to Sonatina function references.
    name_map: FxHashMap<String, FuncRef>,
    /// Maps function symbol names to whether they return a value.
    returns_value_map: FxHashMap<String, bool>,
}

impl<'db, 'a> ModuleLowerer<'db, 'a> {
    fn new(
        db: &'db DriverDataBase,
        builder: ModuleBuilder,
        mir: &'a MirModule<'db>,
        isa: &'a Evm,
        target_layout: TargetDataLayout,
    ) -> Self {
        Self {
            db,
            builder,
            mir,
            isa,
            target_layout,
            func_map: FxHashMap::default(),
            name_map: FxHashMap::default(),
            returns_value_map: FxHashMap::default(),
        }
    }

    /// Lower the entire module.
    fn lower(&mut self) -> Result<(), LowerError> {
        // First pass: declare all functions
        self.declare_functions()?;

        // Second pass: lower function bodies
        self.lower_functions()?;

        // Third pass: create objects for codegen
        self.create_objects()?;

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
            self.returns_value_map.insert(name.clone(), func.returns_value);
        }
        Ok(())
    }

    /// Lower function signatures.
    fn lower_signature(&self, func: &mir::MirFunction<'db>) -> Result<Signature, LowerError> {
        let name = &func.symbol_name;
        let linkage = sonatina_ir::Linkage::Public; // TODO: proper linkage

        // Convert parameter types - all EVM parameters are 256-bit words
        // Include both regular params and effect params (e.g., storage/context bindings)
        let mut params = Vec::new();
        for _ in func.body.param_locals.iter() {
            params.push(types::word_type());
        }
        for _ in func.body.effect_param_locals.iter() {
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

    /// Create Sonatina objects for the module.
    ///
    /// Objects define how code is organized for compilation. Each object
    /// has sections (like "runtime" and "init") that contain function entries.
    ///
    /// TODO: Entry/object semantics need work:
    /// 1. The entry function should use EvmStop/EvmReturn to halt execution,
    ///    not internal Return which is for function call/return semantics.
    /// 2. We currently return runtime section bytes, but BackendOutput::Bytecode
    ///    is documented as "init code". Need to either generate proper init code
    ///    (that deploys runtime) or clarify the documentation.
    /// 3. Entry detection is naive (first non-empty symbol name).
    fn create_objects(&mut self) -> Result<(), LowerError> {
        // Find the entry function - typically the first public function
        // For Fe contracts, this is usually the dispatcher function
        let entry_func = self.mir.functions.iter().enumerate().find(|(_, f)| {
            // Use the first function as entry for now
            // TODO: detect actual entry point (dispatcher)
            !f.symbol_name.is_empty()
        });

        let Some((entry_idx, _entry_mir_func)) = entry_func else {
            // No functions to compile - this is valid for empty modules
            return Ok(());
        };

        let entry_ref = self.func_map[&entry_idx];

        // Create runtime section with entry and all other functions as includes
        let mut directives = vec![Directive::Entry(entry_ref)];

        // Include all other functions
        for (idx, _func) in self.mir.functions.iter().enumerate() {
            if idx != entry_idx {
                let func_ref = self.func_map[&idx];
                directives.push(Directive::Include(func_ref));
            }
        }

        let runtime_section = Section {
            name: SectionName::from("runtime"),
            directives,
        };

        // Create the contract object
        let object = Object {
            name: ObjectName::from("Contract"),
            sections: vec![runtime_section],
        };

        // Add object to module
        self.builder.declare_object(object).map_err(|e| {
            LowerError::Internal(format!("failed to declare object: {e}"))
        })?;

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
        // TODO: This is NOT proper SSA construction. local_map is updated linearly per block,
        // so any local assigned in multiple predecessors needs phi/merge logic. Currently
        // "last lowered block wins" which is incorrect for diamond/loop CFGs. Sonatina's
        // FunctionBuilder should handle phi insertion if we seal blocks correctly, but we
        // may need to use block arguments or explicit phi nodes for locals that merge.
        let mut local_map: FxHashMap<mir::LocalId, ValueId> = FxHashMap::default();

        // Create blocks
        for (idx, _block) in func.body.blocks.iter().enumerate() {
            let block_id = mir::BasicBlockId(idx as u32);
            let sonatina_block = fb.append_block();
            block_map.insert(block_id, sonatina_block);
        }

        // Get the entry block and its Sonatina equivalent
        // TODO: Verify that Sonatina infers entry from the first appended block or
        // explicitly set it. Currently we assume MIR block 0 is entry and that
        // Sonatina treats the first appended block as entry.
        let entry_block = func.body.entry;
        let _sonatina_entry = block_map[&entry_block];

        // Map function arguments to parameter locals (regular params + effect params)
        let args = fb.args().to_vec();
        let all_param_locals = func
            .body
            .param_locals
            .iter()
            .chain(func.body.effect_param_locals.iter());
        for (i, &local_id) in all_param_locals.enumerate() {
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
                    self.db,
                    &self.target_layout,
                    inst,
                    &func.body,
                    &mut value_map,
                    &mut local_map,
                    &self.name_map,
                    &self.returns_value_map,
                    is,
                )?;
            }

            // Lower terminator
            lower_terminator(
                &mut fb,
                self.db,
                &self.target_layout,
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
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
    inst: &mir::MirInst<'db>,
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_map: &mut FxHashMap<mir::LocalId, ValueId>,
    name_map: &FxHashMap<String, FuncRef>,
    returns_value_map: &FxHashMap<String, bool>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<(), LowerError> {
    use mir::MirInst;

    match inst {
        MirInst::Assign { dest, rvalue, .. } => {
            let result = lower_rvalue(fb, db, target_layout, rvalue, body, value_map, local_map, name_map, returns_value_map, is)?;
            if let (Some(dest_local), Some(result_val)) = (dest, result) {
                local_map.insert(*dest_local, result_val);
            }
        }
        MirInst::Store { place, value } => {
            let addr = lower_place_address(fb, db, target_layout, place, body, value_map, local_map, is)?;
            let val = lower_value(fb, db, target_layout, *value, body, value_map, local_map, is)?;
            let addr_space = body.place_address_space(place);

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
            return Err(LowerError::Unsupported(
                "aggregate initialization not yet implemented".to_string(),
            ));
        }
        MirInst::SetDiscriminant { .. } => {
            return Err(LowerError::Unsupported(
                "discriminant setting not yet implemented".to_string(),
            ));
        }
        MirInst::BindValue { value } => {
            // Ensure the value is lowered and cached
            let _ = lower_value(fb, db, target_layout, *value, body, value_map, local_map, is)?;
        }
    }

    Ok(())
}

/// Lower a MIR rvalue to a Sonatina value.
fn lower_rvalue<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
    rvalue: &mir::Rvalue<'db>,
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_map: &mut FxHashMap<mir::LocalId, ValueId>,
    name_map: &FxHashMap<String, FuncRef>,
    returns_value_map: &FxHashMap<String, bool>,
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
            let val = lower_value(fb, db, target_layout, *value_id, body, value_map, local_map, is)?;
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
                let val = lower_value(fb, db, target_layout, arg, body, value_map, local_map, is)?;
                args.push(val);
            }
            for &effect_arg in &call.effect_args {
                let val = lower_value(fb, db, target_layout, effect_arg, body, value_map, local_map, is)?;
                args.push(val);
            }

            // Emit call instruction with proper return type
            let call_inst = Call::new(is, *func_ref, args.into());
            let callee_returns = returns_value_map
                .get(callee_name)
                .copied()
                .unwrap_or(true); // Default to returning value if unknown
            if callee_returns {
                let result = fb.insert_inst(call_inst, types::word_type());
                Ok(Some(result))
            } else {
                // Unit-returning calls don't produce a value
                fb.insert_inst_no_result(call_inst);
                Ok(None)
            }
        }
        Rvalue::Intrinsic { op, args } => {
            lower_intrinsic(fb, db, target_layout, *op, args, body, value_map, local_map, is)
        }
        Rvalue::Load { place } => {
            let addr = lower_place_address(fb, db, target_layout, place, body, value_map, local_map, is)?;
            let addr_space = body.place_address_space(place);

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
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
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
    let result = lower_value_origin(fb, db, target_layout, &value_data.origin, body, value_map, local_map, is)?;

    value_map.insert(value_id, result);
    Ok(result)
}

/// Lower a MIR value origin to a Sonatina value.
fn lower_value_origin<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
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
            let inner_val = lower_value(fb, db, target_layout, *inner, body, value_map, local_map, is)?;
            lower_unary_op(fb, *op, inner_val, is)
        }
        ValueOrigin::Binary { op, lhs, rhs } => {
            let lhs_val = lower_value(fb, db, target_layout, *lhs, body, value_map, local_map, is)?;
            let rhs_val = lower_value(fb, db, target_layout, *rhs, body, value_map, local_map, is)?;
            lower_binary_op(fb, *op, lhs_val, rhs_val, is)
        }
        ValueOrigin::TransparentCast { value } => {
            // Transparent cast just passes through the inner value
            lower_value(fb, db, target_layout, *value, body, value_map, local_map, is)
        }
        ValueOrigin::ControlFlowResult { expr } => {
            // ControlFlowResult values should be converted to Local values during MIR lowering.
            // If we reach here, it means MIR lowering didn't properly handle this case.
            Err(LowerError::Internal(format!(
                "ControlFlowResult value reached codegen without being converted to Local (expr={expr:?})"
            )))
        }
        ValueOrigin::PlaceRef(place) => {
            // Compute the full address including projections
            lower_place_address(fb, db, target_layout, place, body, value_map, local_map, is)
        }
        ValueOrigin::FieldPtr(_) => {
            // TODO: field pointer arithmetic
            Err(LowerError::Unsupported("field pointer".to_string()))
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
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
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
        let val = lower_value(fb, db, target_layout, arg, body, value_map, local_map, is)?;
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

/// Computes the address for a place by walking the projection path.
///
/// For memory, computes byte offsets. For storage, computes slot offsets.
/// Returns a Sonatina ValueId representing the final address.
fn lower_place_address<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
    place: &Place<'db>,
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_map: &mut FxHashMap<mir::LocalId, ValueId>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    let mut base_val = lower_value(fb, db, target_layout, place.base, body, value_map, local_map, is)?;

    if place.projection.is_empty() {
        return Ok(base_val);
    }

    // Get the base value's type to navigate projections
    let base_value = &body.values[place.base.index()];
    let mut current_ty = base_value.ty;
    let mut total_offset: usize = 0;
    let is_slot_addressed = matches!(
        body.place_address_space(place),
        AddressSpaceKind::Storage | AddressSpaceKind::TransientStorage
    );

    for proj in place.projection.iter() {
        match proj {
            Projection::Field(field_idx) => {
                // Use slot-based offsets for storage, byte-based for memory
                total_offset += if is_slot_addressed {
                    layout::field_offset_slots(db, current_ty, *field_idx)
                } else {
                    layout::field_offset_bytes_or_word_aligned_in(
                        db,
                        target_layout,
                        current_ty,
                        *field_idx,
                    )
                };
                // Update current type to the field's type
                let field_types = current_ty.field_types(db);
                current_ty = *field_types.get(*field_idx).ok_or_else(|| {
                    LowerError::Unsupported(format!(
                        "projection: field {field_idx} out of bounds"
                    ))
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
                    total_offset += layout::variant_field_offset_slots(
                        db, *enum_ty, *variant, *field_idx,
                    );
                } else {
                    total_offset += target_layout.discriminant_size_bytes;
                    total_offset += layout::variant_field_offset_bytes_or_word_aligned_in(
                        db,
                        target_layout,
                        *enum_ty,
                        *variant,
                        *field_idx,
                    );
                }
                // Update current type to the field's type
                let ctor = hir::analysis::ty::simplified_pattern::ConstructorKind::Variant(*variant, *enum_ty);
                let field_types = ctor.field_types(db);
                current_ty = *field_types.get(*field_idx).ok_or_else(|| {
                    LowerError::Unsupported(format!(
                        "projection: variant field {field_idx} out of bounds"
                    ))
                })?;
            }
            Projection::Discriminant => {
                // Discriminant is at offset 0, just update the type
                current_ty = hir::analysis::ty::ty_def::TyId::new(
                    db,
                    hir::analysis::ty::ty_def::TyData::TyBase(
                        hir::analysis::ty::ty_def::TyBase::Prim(hir::analysis::ty::ty_def::PrimTy::U256)
                    )
                );
            }
            Projection::Index(idx_source) => {
                let stride = if is_slot_addressed {
                    layout::array_elem_stride_slots(db, current_ty)
                } else {
                    layout::array_elem_stride_bytes_in(db, target_layout, current_ty)
                }
                .ok_or_else(|| {
                    LowerError::Unsupported(
                        "projection: array index on non-array type".to_string(),
                    )
                })?;

                match idx_source {
                    IndexSource::Constant(idx) => {
                        total_offset += idx * stride;
                    }
                    IndexSource::Dynamic(value_id) => {
                        // Flush accumulated offset first
                        if total_offset != 0 {
                            let offset_val = fb.make_imm_value(I256::from(total_offset as u64));
                            base_val = fb.insert_inst(Add::new(is, base_val, offset_val), Type::I256);
                            total_offset = 0;
                        }
                        // Compute dynamic index offset: idx * stride
                        let idx_val = lower_value(fb, db, target_layout, *value_id, body, value_map, local_map, is)?;
                        let offset_val = if stride == 1 {
                            idx_val
                        } else {
                            let stride_val = fb.make_imm_value(I256::from(stride as u64));
                            fb.insert_inst(Mul::new(is, idx_val, stride_val), Type::I256)
                        };
                        base_val = fb.insert_inst(Add::new(is, base_val, offset_val), Type::I256);
                    }
                }

                // Update current type to element type
                let elem_ty = layout::array_elem_ty(db, current_ty).ok_or_else(|| {
                    LowerError::Unsupported(
                        "projection: array index on non-array type".to_string(),
                    )
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
        let offset_val = fb.make_imm_value(I256::from(total_offset as u64));
        base_val = fb.insert_inst(Add::new(is, base_val, offset_val), Type::I256);
    }

    Ok(base_val)
}

/// Lower a block terminator.
fn lower_terminator<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
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
                Some(lower_value(fb, db, target_layout, *v, body, value_map, local_map, is)?)
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
            let cond_val = lower_value(fb, db, target_layout, *cond, body, value_map, local_map, is)?;
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
            // Emit INVALID opcode (0xFE) - this consumes all gas and reverts
            fb.insert_inst_no_result(EvmInvalid::new(is));
        }
    }

    Ok(())
}
