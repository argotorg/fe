//! Sonatina backend for direct EVM bytecode generation.
//!
//! This module translates Fe MIR to Sonatina IR, which is then compiled
//! to EVM bytecode without going through Yul/solc.

mod types;

use driver::DriverDataBase;
use hir::analysis::ty::adt_def::AdtRef;
use hir::hir_def::TopLevelMod;
use hir::hir_def::expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp};
use hir::projection::{IndexSource, Projection};
use mir::{MirModule, layout, layout::TargetDataLayout, lower_module};
use mir::ir::{AddressSpaceKind, IntrinsicOp, Place, SyntheticValue};
use num_bigint::BigUint;
use rustc_hash::FxHashMap;
use hir::analysis::ty::ty_def::{PrimTy, TyBase, TyData};
use sonatina_ir::{
    BlockId, I256, Module, Signature, Type, ValueId,
    builder::{ModuleBuilder, Variable},
    func_cursor::InstInserter,
    inst::{
        arith::{Add, Mul, Neg, Shl, Shr, Sub},
        cast::{Sext, Trunc, Zext},
        cmp::{Eq, Gt, IsZero, Lt, Ne},
        control_flow::{Br, BrTable, Call, Jump, Return},
        data::{Mload, Mstore},
        evm::{
            EvmCalldataCopy, EvmCalldataLoad, EvmCalldataSize,
            EvmCaller, EvmCodeCopy, EvmCodeSize,
            EvmExp, EvmInvalid, EvmKeccak256,
            EvmMstore8,
            EvmRevert, EvmReturn,
            EvmReturnDataCopy, EvmReturnDataSize,
            EvmSload, EvmSstore,
            EvmStop,
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
    fn is_excluded_from_runtime(func: &mir::MirFunction<'db>) -> bool {
        use mir::ir::{ContractFunctionKind, MirFunctionOrigin, SyntheticId};

        if let Some(contract_fn) = func.contract_function.as_ref()
            && contract_fn.kind == ContractFunctionKind::Init
        {
            return true;
        }

        matches!(
            func.origin,
            MirFunctionOrigin::Synthetic(
                SyntheticId::ContractInitEntrypoint(_)
                    | SyntheticId::ContractInitHandler(_)
                    | SyntheticId::ContractInitCodeOffset(_)
                    | SyntheticId::ContractInitCodeLen(_)
            )
        )
    }

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
        // First pass: declare runtime-relevant functions.
        self.declare_functions()?;

        // Second pass: create objects for codegen (select entry + section layout).
        self.create_objects()?;

        // Third pass: lower function bodies that are actually declared/included.
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
            if func.symbol_name.is_empty() || Self::is_excluded_from_runtime(func) {
                continue;
            }
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
            let Some(&func_ref) = self.func_map.get(&idx) else {
                continue;
            };
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
        use mir::ir::{ContractFunctionKind, MirFunctionOrigin, SyntheticId};

        // Prefer a contract runtime entrypoint when present.
        let entry_func = self
            .mir
            .functions
            .iter()
            .enumerate()
            .filter(|(idx, _)| self.func_map.contains_key(idx))
            .find(|(_, f)| {
                f.contract_function
                    .as_ref()
                    .is_some_and(|cf| cf.kind == ContractFunctionKind::Runtime)
                    || matches!(
                        f.origin,
                        MirFunctionOrigin::Synthetic(SyntheticId::ContractRuntimeEntrypoint(_))
                    )
            })
            // Fallback to "first declared function".
            .or_else(|| {
                self.mir
                    .functions
                    .iter()
                    .enumerate()
                    .filter(|(idx, _)| self.func_map.contains_key(idx))
                    .next()
            });

        let Some((entry_idx, _entry_mir_func)) = entry_func else {
            // No functions to compile - this is valid for empty modules
            return Ok(());
        };

        let entry_ref = self.func_map[&entry_idx];
        let entry_mir_func = &self.mir.functions[entry_idx];

        // Create a wrapper entrypoint that halts with `STOP` instead of relying on internal
        // `Return` (which is only valid for intra-module calls with a return PC).
        let wrapper_ref = self.create_entry_wrapper(entry_ref, entry_mir_func)?;

        // Create runtime section with entry wrapper and all declared functions as includes.
        // Note: we must include the wrapped entry function explicitly; Sonatina objects only
        // compile functions referenced by directives.
        let mut directives = vec![Directive::Entry(wrapper_ref), Directive::Include(entry_ref)];

        // Include all other declared functions
        for (idx, _func) in self.mir.functions.iter().enumerate() {
            if idx == entry_idx {
                continue;
            }
            if let Some(&func_ref) = self.func_map.get(&idx) {
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

    fn create_entry_wrapper(
        &mut self,
        entry_ref: FuncRef,
        entry_mir_func: &mir::MirFunction<'db>,
    ) -> Result<FuncRef, LowerError> {
        const WRAPPER_NAME: &str = "__fe_sonatina_entry";
        if self.name_map.contains_key(WRAPPER_NAME) {
            return Err(LowerError::Internal(format!(
                "entry wrapper name collision: `{WRAPPER_NAME}`"
            )));
        }

        let sig = Signature::new(
            WRAPPER_NAME,
            sonatina_ir::Linkage::Public,
            &[],
            types::unit_type(),
        );
        let func_ref = self.builder.declare_function(sig).map_err(|e| {
            LowerError::Internal(format!("failed to declare entry wrapper `{WRAPPER_NAME}`: {e}"))
        })?;

        let mut fb = self.builder.func_builder::<InstInserter>(func_ref);
        let is = self.isa.inst_set();

        let entry_block = fb.append_block();
        fb.switch_to_block(entry_block);

        // Pass zero for all arguments (regular + effect params). Runtime entrypoints
        // generally ignore these "effect root" placeholders and use EVM intrinsics directly.
        let argc = entry_mir_func.body.param_locals.len() + entry_mir_func.body.effect_param_locals.len();
        let mut args = Vec::with_capacity(argc);
        for _ in 0..argc {
            args.push(fb.make_imm_value(I256::zero()));
        }

        let call_inst = Call::new(is, entry_ref, args.into());
        if entry_mir_func.returns_value {
            let _ = fb.insert_inst(call_inst, types::word_type());
        } else {
            fb.insert_inst_no_result(call_inst);
        }

        fb.insert_inst_no_result(EvmStop::new(is));
        fb.seal_all();
        fb.finish();

        Ok(func_ref)
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

        // Maps MIR local IDs to Sonatina SSA variables.
        let mut local_vars: FxHashMap<mir::LocalId, Variable> = FxHashMap::default();
        for (idx, _local) in func.body.locals.iter().enumerate() {
            let local_id = mir::LocalId(idx as u32);
            let var = fb.declare_var(types::word_type());
            local_vars.insert(local_id, var);
        }

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

        // Map function arguments to parameter locals (regular params + effect params).
        fb.switch_to_block(_sonatina_entry);
        let args = fb.args().to_vec();
        let all_param_locals = func
            .body
            .param_locals
            .iter()
            .chain(func.body.effect_param_locals.iter());
        for (i, &local_id) in all_param_locals.enumerate() {
            let Some(&arg_val) = args.get(i) else { break };
            let var = local_vars.get(&local_id).copied().ok_or_else(|| {
                LowerError::Internal(format!("missing SSA variable for param local {local_id:?}"))
            })?;
            fb.def_var(var, arg_val);
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
                    &local_vars,
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
                &local_vars,
                &self.name_map,
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
    local_vars: &FxHashMap<mir::LocalId, Variable>,
    name_map: &FxHashMap<String, FuncRef>,
    returns_value_map: &FxHashMap<String, bool>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
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
                let value = lower_alloc(
                    fb,
                    db,
                    target_layout,
                    *dest_local,
                    *address_space,
                    body,
                    is,
                )?;
                let dest_var = local_vars.get(dest_local).copied().ok_or_else(|| {
                    LowerError::Internal(format!("missing SSA variable for local {dest_local:?}"))
                })?;
                fb.def_var(dest_var, value);
                return Ok(());
            }

            let result = lower_rvalue(
                fb,
                db,
                target_layout,
                rvalue,
                body,
                value_map,
                local_vars,
                name_map,
                returns_value_map,
                is,
            )?;
            if let (Some(dest_local), Some(result_val)) = (dest, result) {
                let dest_var = local_vars.get(dest_local).copied().ok_or_else(|| {
                    LowerError::Internal(format!("missing SSA variable for local {dest_local:?}"))
                })?;
                // Apply from_word conversion for Load operations
                let converted = if matches!(rvalue, mir::Rvalue::Load { .. }) {
                    let dest_ty = body.locals.get(dest_local.index())
                        .map(|l| l.ty)
                        .unwrap_or_else(|| body.values[0].ty); // fallback shouldn't happen
                    apply_from_word(fb, db, result_val, dest_ty, is)
                } else {
                    result_val
                };
                fb.def_var(dest_var, converted);
            }
        }
        MirInst::Store { place, value } => {
            lower_store_inst(
                fb,
                db,
                target_layout,
                place,
                *value,
                body,
                value_map,
                local_vars,
                is,
            )?;
        }
        MirInst::InitAggregate { place, inits } => {
            for (path, value) in inits {
                let mut target = place.clone();
                for proj in path.iter() {
                    target.projection.push(proj.clone());
                }
                lower_store_inst(
                    fb,
                    db,
                    target_layout,
                    &target,
                    *value,
                    body,
                    value_map,
                    local_vars,
                    is,
                )?;
            }
        }
        MirInst::SetDiscriminant { place, variant } => {
            let val = fb.make_imm_value(I256::from(variant.idx as u64));
            store_word_to_place(
                fb,
                db,
                target_layout,
                place,
                val,
                body,
                value_map,
                local_vars,
                is,
            )?;
        }
        MirInst::BindValue { value } => {
            // Ensure the value is lowered and cached
            let _ = lower_value(fb, db, target_layout, *value, body, value_map, local_vars, is)?;
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
    local_vars: &FxHashMap<mir::LocalId, Variable>,
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
            let val = lower_value(fb, db, target_layout, *value_id, body, value_map, local_vars, is)?;
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
                let val = lower_value(fb, db, target_layout, arg, body, value_map, local_vars, is)?;
                args.push(val);
            }
            for &effect_arg in &call.effect_args {
                let val = lower_value(
                    fb,
                    db,
                    target_layout,
                    effect_arg,
                    body,
                    value_map,
                    local_vars,
                    is,
                )?;
                args.push(val);
            }

            // Emit call instruction with proper return type
            let call_inst = Call::new(is, *func_ref, args.into());
            let callee_returns = returns_value_map
                .get(callee_name)
                .copied()
                .ok_or_else(|| {
                    LowerError::Internal(format!(
                        "missing return type metadata for function: {callee_name}"
                    ))
                })?;
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
            lower_intrinsic(fb, db, target_layout, *op, args, body, value_map, local_vars, is)
        }
        Rvalue::Load { place } => {
            let addr =
                lower_place_address(fb, db, target_layout, place, body, value_map, local_vars, is)?;
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
            Err(LowerError::Internal(
                "Alloc rvalue should be handled directly in Assign lowering".to_string(),
            ))
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
    local_vars: &FxHashMap<mir::LocalId, Variable>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    let value_data = &body.values[value_id.index()];

    // Some origins depend on the current SSA state / current block; avoid caching them
    // across the whole function.
    let cacheable = !matches!(
        value_data.origin,
        mir::ValueOrigin::Local(_) | mir::ValueOrigin::PlaceRef(_)
    );
    if cacheable {
        if let Some(&val) = value_map.get(&value_id) {
            return Ok(val);
        }
    }

    let result = lower_value_origin(
        fb,
        db,
        target_layout,
        &value_data.origin,
        body,
        value_map,
        local_vars,
        is,
    )?;

    if cacheable {
        value_map.insert(value_id, result);
    }
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
    local_vars: &FxHashMap<mir::LocalId, Variable>,
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
            let var = local_vars.get(local_id).copied().ok_or_else(|| {
                LowerError::Internal(format!("SSA variable not found for local {local_id:?}"))
            })?;
            Ok(fb.use_var(var))
        }
        ValueOrigin::Unit => {
            // Unit is represented as 0
            Ok(fb.make_imm_value(I256::zero()))
        }
        ValueOrigin::Unary { op, inner } => {
            let inner_val =
                lower_value(fb, db, target_layout, *inner, body, value_map, local_vars, is)?;
            lower_unary_op(fb, *op, inner_val, is)
        }
        ValueOrigin::Binary { op, lhs, rhs } => {
            let lhs_val =
                lower_value(fb, db, target_layout, *lhs, body, value_map, local_vars, is)?;
            let rhs_val =
                lower_value(fb, db, target_layout, *rhs, body, value_map, local_vars, is)?;
            lower_binary_op(fb, *op, lhs_val, rhs_val, is)
        }
        ValueOrigin::TransparentCast { value } => {
            // Transparent cast just passes through the inner value
            lower_value(fb, db, target_layout, *value, body, value_map, local_vars, is)
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
            lower_place_address(fb, db, target_layout, place, body, value_map, local_vars, is)
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
    local_vars: &FxHashMap<mir::LocalId, Variable>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<Option<ValueId>, LowerError> {
    // Lower all arguments first
    let mut lowered_args = Vec::with_capacity(args.len());
    for &arg in args {
        let val = lower_value(fb, db, target_layout, arg, body, value_map, local_vars, is)?;
        lowered_args.push(val);
    }

    match op {
        IntrinsicOp::AddrOf => {
            let Some(&arg) = lowered_args.first() else {
                return Err(LowerError::Internal("addr_of requires 1 argument".to_string()));
            };
            Ok(Some(arg))
        }
        IntrinsicOp::Mload => {
            let Some(&addr) = lowered_args.first() else {
                return Err(LowerError::Internal("mload requires address argument".to_string()));
            };
            Ok(Some(fb.insert_inst(
                Mload::new(is, addr, Type::I256),
                Type::I256,
            )))
        }
        IntrinsicOp::Mstore => {
            let [addr, val] = lowered_args.as_slice() else {
                return Err(LowerError::Internal("mstore requires 2 arguments".to_string()));
            };
            fb.insert_inst_no_result(Mstore::new(is, *addr, *val, Type::I256));
            Ok(None)
        }
        IntrinsicOp::Mstore8 => {
            let [addr, val] = lowered_args.as_slice() else {
                return Err(LowerError::Internal("mstore8 requires 2 arguments".to_string()));
            };
            fb.insert_inst_no_result(EvmMstore8::new(is, *addr, *val));
            Ok(None)
        }
        IntrinsicOp::Sload => {
            let Some(&key) = lowered_args.first() else {
                return Err(LowerError::Internal("sload requires 1 argument".to_string()));
            };
            Ok(Some(fb.insert_inst(EvmSload::new(is, key), Type::I256)))
        }
        IntrinsicOp::Sstore => {
            let [key, val] = lowered_args.as_slice() else {
                return Err(LowerError::Internal("sstore requires 2 arguments".to_string()));
            };
            fb.insert_inst_no_result(EvmSstore::new(is, *key, *val));
            Ok(None)
        }
        IntrinsicOp::Calldataload => {
            let Some(&offset) = lowered_args.first() else {
                return Err(LowerError::Internal(
                    "calldataload requires 1 argument".to_string(),
                ));
            };
            Ok(Some(fb.insert_inst(
                EvmCalldataLoad::new(is, offset),
                Type::I256,
            )))
        }
        IntrinsicOp::Calldatasize => Ok(Some(fb.insert_inst(EvmCalldataSize::new(is), Type::I256))),
        IntrinsicOp::Calldatacopy => {
            let [dst, offset, len] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "calldatacopy requires 3 arguments".to_string(),
                ));
            };
            fb.insert_inst_no_result(EvmCalldataCopy::new(is, *dst, *offset, *len));
            Ok(None)
        }
        IntrinsicOp::Returndatasize => Ok(Some(
            fb.insert_inst(EvmReturnDataSize::new(is), Type::I256),
        )),
        IntrinsicOp::Returndatacopy => {
            let [dst, offset, len] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "returndatacopy requires 3 arguments".to_string(),
                ));
            };
            fb.insert_inst_no_result(EvmReturnDataCopy::new(is, *dst, *offset, *len));
            Ok(None)
        }
        IntrinsicOp::Codesize => Ok(Some(fb.insert_inst(EvmCodeSize::new(is), Type::I256))),
        IntrinsicOp::Codecopy => {
            let [dst, offset, len] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "codecopy requires 3 arguments".to_string(),
                ));
            };
            fb.insert_inst_no_result(EvmCodeCopy::new(is, *dst, *offset, *len));
            Ok(None)
        }
        IntrinsicOp::CodeRegionOffset | IntrinsicOp::CodeRegionLen => Err(LowerError::Unsupported(
            "code region intrinsics are not yet supported by the Sonatina backend".to_string(),
        )),
        IntrinsicOp::Keccak => {
            let [addr, len] = lowered_args.as_slice() else {
                return Err(LowerError::Internal("keccak requires 2 arguments".to_string()));
            };
            Ok(Some(fb.insert_inst(
                EvmKeccak256::new(is, *addr, *len),
                Type::I256,
            )))
        }
        IntrinsicOp::Caller => Ok(Some(fb.insert_inst(EvmCaller::new(is), Type::I256))),
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
        PrimTy::U256 | PrimTy::I256 | PrimTy::Usize | PrimTy::Isize => None,
        // Non-scalar types
        PrimTy::String | PrimTy::Array | PrimTy::Tuple(_) | PrimTy::Ptr => None,
    }
}

/// Returns true if the primitive type is signed.
fn prim_is_signed(prim: PrimTy) -> bool {
    matches!(
        prim,
        PrimTy::I8 | PrimTy::I16 | PrimTy::I32 | PrimTy::I64 | PrimTy::I128 | PrimTy::I256 | PrimTy::Isize
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
                // bool: iszero(iszero(value)) → 0 or 1
                let is_zero1 = IsZero::new(is, value);
                let z1 = fb.insert_inst(is_zero1, Type::I256);
                let is_zero2 = IsZero::new(is, z1);
                fb.insert_inst(is_zero2, Type::I256)
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
    local_vars: &FxHashMap<mir::LocalId, Variable>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    let mut base_val =
        lower_value(fb, db, target_layout, place.base, body, value_map, local_vars, is)?;

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
                        let idx_val = lower_value(
                            fb,
                            db,
                            target_layout,
                            *value_id,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
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
    local_vars: &FxHashMap<mir::LocalId, Variable>,
    name_map: &FxHashMap<String, FuncRef>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<(), LowerError> {
    use mir::Terminator;

    match term {
        Terminator::Return(ret_val) => {
            let ret_sonatina = if let Some(v) = ret_val {
                Some(lower_value(fb, db, target_layout, *v, body, value_map, local_vars, is)?)
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
            let cond_val =
                lower_value(fb, db, target_layout, *cond, body, value_map, local_vars, is)?;
            let then_block = block_map[then_bb];
            let else_block = block_map[else_bb];
            // Br: cond, nz_dest (then), z_dest (else)
            fb.insert_inst_no_result(Br::new(is, cond_val, then_block, else_block));
        }
        Terminator::Switch {
            discr,
            targets,
            default,
        } => {
            let discr_val =
                lower_value(fb, db, target_layout, *discr, body, value_map, local_vars, is)?;
            let default_block = block_map[default];
            let mut table = Vec::with_capacity(targets.len());
            for target in targets {
                let value = fb.make_imm_value(biguint_to_i256(&target.value.as_biguint()));
                let dest = block_map[&target.block];
                table.push((value, dest));
            }
            fb.insert_inst_no_result(BrTable::new(is, discr_val, Some(default_block), table));
        }
        Terminator::TerminatingCall(call) => match call {
            mir::TerminatingCall::Call(call) => {
                let callee_name = call.resolved_name.as_ref().ok_or_else(|| {
                    LowerError::Unsupported("terminating call without resolved name".to_string())
                })?;
                let func_ref = name_map.get(callee_name).ok_or_else(|| {
                    LowerError::Internal(format!("unknown function: {callee_name}"))
                })?;

                let mut args = Vec::with_capacity(call.args.len() + call.effect_args.len());
                for &arg in &call.args {
                    args.push(lower_value(
                        fb,
                        db,
                        target_layout,
                        arg,
                        body,
                        value_map,
                        local_vars,
                        is,
                    )?);
                }
                for &arg in &call.effect_args {
                    args.push(lower_value(
                        fb,
                        db,
                        target_layout,
                        arg,
                        body,
                        value_map,
                        local_vars,
                        is,
                    )?);
                }

                fb.insert_inst_no_result(Call::new(is, *func_ref, args.into()));
                fb.insert_inst_no_result(EvmInvalid::new(is));
            }
            mir::TerminatingCall::Intrinsic { op, args } => {
                let mut lowered_args = Vec::with_capacity(args.len());
                for &arg in args {
                    lowered_args.push(lower_value(
                        fb,
                        db,
                        target_layout,
                        arg,
                        body,
                        value_map,
                        local_vars,
                        is,
                    )?);
                }
                match op {
                    IntrinsicOp::ReturnData => {
                        let [addr, len] = lowered_args.as_slice() else {
                            return Err(LowerError::Internal(
                                "return_data requires 2 arguments".to_string(),
                            ));
                        };
                        fb.insert_inst_no_result(EvmReturn::new(is, *addr, *len));
                    }
                    IntrinsicOp::Revert => {
                        let [addr, len] = lowered_args.as_slice() else {
                            return Err(LowerError::Internal(
                                "revert requires 2 arguments".to_string(),
                            ));
                        };
                        fb.insert_inst_no_result(EvmRevert::new(is, *addr, *len));
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
        Terminator::Unreachable => {
            // Emit INVALID opcode (0xFE) - this consumes all gas and reverts
            fb.insert_inst_no_result(EvmInvalid::new(is));
        }
    }

    Ok(())
}

fn lower_alloc<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &DriverDataBase,
    target_layout: &TargetDataLayout,
    dest: mir::LocalId,
    address_space: AddressSpaceKind,
    body: &mir::MirBody<'_>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    if !matches!(address_space, AddressSpaceKind::Memory) {
        return Err(LowerError::Unsupported(
            "alloc is only supported for memory".to_string(),
        ));
    }

    let alloc_ty = body
        .locals
        .get(dest.index())
        .ok_or_else(|| LowerError::Internal(format!("unknown local: {dest:?}")))?
        .ty;

    let size_bytes = layout::ty_size_bytes_or_word_aligned_in(db, target_layout, alloc_ty);
    if size_bytes == 0 {
        return Ok(fb.make_imm_value(I256::zero()));
    }

    // Allocate from the free memory pointer at 0x40 (defaulting to 0x80 if unset).
    let free_ptr_addr = fb.make_imm_value(I256::from(0x40u64));
    let raw_ptr = fb.insert_inst(Mload::new(is, free_ptr_addr, Type::I256), Type::I256);
    let is_zero = fb.insert_inst(IsZero::new(is, raw_ptr), Type::I256);
    let default_ptr = fb.make_imm_value(I256::from(0x80u64));
    let fallback = fb.insert_inst(
        Mul::new(is, is_zero, default_ptr),
        Type::I256,
    );
    let ptr = fb.insert_inst(Or::new(is, raw_ptr, fallback), Type::I256);

    let size_val = fb.make_imm_value(I256::from(size_bytes as u64));
    let new_free = fb.insert_inst(
        Add::new(is, ptr, size_val),
        Type::I256,
    );
    fb.insert_inst_no_result(Mstore::new(is, free_ptr_addr, new_free, Type::I256));

    Ok(ptr)
}

fn lower_store_inst<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
    place: &Place<'db>,
    value: mir::ValueId,
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_vars: &FxHashMap<mir::LocalId, Variable>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<(), LowerError> {
    let value_data = body
        .values
        .get(value.index())
        .ok_or_else(|| LowerError::Internal(format!("unknown value: {value:?}")))?;
    let value_ty = value_data.ty;
    if layout::ty_size_bytes_in(db, target_layout, value_ty).is_some_and(|s| s == 0) {
        return Ok(());
    }

    if value_data.repr.is_ref() {
        let src_place = mir::ir::Place::new(value, mir::ir::MirProjectionPath::new());
        deep_copy_from_places(
            fb,
            db,
            target_layout,
            place,
            &src_place,
            value_ty,
            body,
            value_map,
            local_vars,
            is,
        )?;
        return Ok(());
    }

    let raw_val = lower_value(fb, db, target_layout, value, body, value_map, local_vars, is)?;
    let val = apply_to_word(fb, db, raw_val, value_ty, is);
    store_word_to_place(fb, db, target_layout, place, val, body, value_map, local_vars, is)
}

fn store_word_to_place<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
    place: &Place<'db>,
    val: ValueId,
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_vars: &FxHashMap<mir::LocalId, Variable>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<(), LowerError> {
    let addr = lower_place_address(fb, db, target_layout, place, body, value_map, local_vars, is)?;
    match body.place_address_space(place) {
        AddressSpaceKind::Memory => {
            fb.insert_inst_no_result(Mstore::new(is, addr, val, Type::I256));
        }
        AddressSpaceKind::Storage => {
            fb.insert_inst_no_result(EvmSstore::new(is, addr, val));
        }
        AddressSpaceKind::TransientStorage => {
            fb.insert_inst_no_result(EvmTstore::new(is, addr, val));
        }
        AddressSpaceKind::Calldata => {
            return Err(LowerError::Unsupported("store to calldata".to_string()));
        }
    }
    Ok(())
}

fn load_place_typed<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
    place: &Place<'db>,
    loaded_ty: hir::analysis::ty::ty_def::TyId<'db>,
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_vars: &FxHashMap<mir::LocalId, Variable>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<ValueId, LowerError> {
    if layout::ty_size_bytes_in(db, target_layout, loaded_ty).is_some_and(|s| s == 0) {
        return Ok(fb.make_imm_value(I256::zero()));
    }

    let addr = lower_place_address(fb, db, target_layout, place, body, value_map, local_vars, is)?;
    let raw = match body.place_address_space(place) {
        AddressSpaceKind::Memory => fb.insert_inst(Mload::new(is, addr, Type::I256), Type::I256),
        AddressSpaceKind::Storage => fb.insert_inst(EvmSload::new(is, addr), Type::I256),
        AddressSpaceKind::TransientStorage => fb.insert_inst(EvmTload::new(is, addr), Type::I256),
        AddressSpaceKind::Calldata => fb.insert_inst(EvmCalldataLoad::new(is, addr), Type::I256),
    };
    Ok(apply_from_word(fb, db, raw, loaded_ty, is))
}

fn deep_copy_from_places<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
    dst_place: &Place<'db>,
    src_place: &Place<'db>,
    value_ty: hir::analysis::ty::ty_def::TyId<'db>,
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_vars: &FxHashMap<mir::LocalId, Variable>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<(), LowerError> {
    if layout::ty_size_bytes_in(db, target_layout, value_ty).is_some_and(|s| s == 0) {
        return Ok(());
    }

    if value_ty.is_array(db) {
        let Some(len) = layout::array_len(db, value_ty) else {
            return Err(LowerError::Unsupported(
                "array store requires a constant length".into(),
            ));
        };
        let elem_ty = layout::array_elem_ty(db, value_ty)
            .ok_or_else(|| LowerError::Unsupported("array store requires element type".into()))?;
        for idx in 0..len {
            let dst_elem =
                extend_place(dst_place, Projection::Index(IndexSource::Constant(idx)));
            let src_elem =
                extend_place(src_place, Projection::Index(IndexSource::Constant(idx)));
            deep_copy_from_places(
                fb,
                db,
                target_layout,
                &dst_elem,
                &src_elem,
                elem_ty,
                body,
                value_map,
                local_vars,
                is,
            )?;
        }
        return Ok(());
    }

    if value_ty.field_count(db) > 0 {
        for (field_idx, field_ty) in value_ty.field_types(db).iter().copied().enumerate() {
            let dst_field = extend_place(dst_place, Projection::Field(field_idx));
            let src_field = extend_place(src_place, Projection::Field(field_idx));
            deep_copy_from_places(
                fb,
                db,
                target_layout,
                &dst_field,
                &src_field,
                field_ty,
                body,
                value_map,
                local_vars,
                is,
            )?;
        }
        return Ok(());
    }

    if value_ty
        .adt_ref(db)
        .is_some_and(|adt| matches!(adt, AdtRef::Enum(_)))
    {
        return deep_copy_enum_from_places(
            fb,
            db,
            target_layout,
            dst_place,
            src_place,
            value_ty,
            body,
            value_map,
            local_vars,
            is,
        );
    }

    let loaded = load_place_typed(
        fb,
        db,
        target_layout,
        src_place,
        value_ty,
        body,
        value_map,
        local_vars,
        is,
    )?;
    let stored = apply_to_word(fb, db, loaded, value_ty, is);
    store_word_to_place(
        fb,
        db,
        target_layout,
        dst_place,
        stored,
        body,
        value_map,
        local_vars,
        is,
    )
}

fn deep_copy_enum_from_places<'db, C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    db: &'db DriverDataBase,
    target_layout: &TargetDataLayout,
    dst_place: &Place<'db>,
    src_place: &Place<'db>,
    enum_ty: hir::analysis::ty::ty_def::TyId<'db>,
    body: &mir::MirBody<'db>,
    value_map: &mut FxHashMap<mir::ValueId, ValueId>,
    local_vars: &FxHashMap<mir::LocalId, Variable>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<(), LowerError> {
    let Some(adt_def) = enum_ty.adt_def(db) else {
        return Err(LowerError::Unsupported("enum store requires enum adt".into()));
    };
    let AdtRef::Enum(enm) = adt_def.adt_ref(db) else {
        return Err(LowerError::Unsupported("enum store requires enum adt".into()));
    };

    // Copy discriminant first.
    let discr_ty = hir::analysis::ty::ty_def::TyId::new(
        db,
        TyData::TyBase(TyBase::Prim(PrimTy::U256)),
    );
    let discr = load_place_typed(
        fb,
        db,
        target_layout,
        src_place,
        discr_ty,
        body,
        value_map,
        local_vars,
        is,
    )?;
    store_word_to_place(
        fb,
        db,
        target_layout,
        dst_place,
        discr,
        body,
        value_map,
        local_vars,
        is,
    )?;

    let origin_block = fb
        .current_block()
        .ok_or_else(|| LowerError::Internal("missing current block".to_string()))?;
    let cont_block = fb.append_block();

    let variants = adt_def.fields(db);
    let mut table: Vec<(ValueId, BlockId)> = Vec::with_capacity(variants.len());
    let mut case_blocks = Vec::with_capacity(variants.len());
    for (idx, _) in variants.iter().enumerate() {
        let case_block = fb.append_block();
        case_blocks.push(case_block);
        table.push((fb.make_imm_value(I256::from(idx as u64)), case_block));
    }

    fb.switch_to_block(origin_block);
    fb.insert_inst_no_result(BrTable::new(is, discr, Some(cont_block), table));

    for (idx, case_block) in case_blocks.into_iter().enumerate() {
        fb.switch_to_block(case_block);
        let enum_variant = hir::hir_def::EnumVariant::new(enm, idx);
        let ctor =
            hir::analysis::ty::simplified_pattern::ConstructorKind::Variant(enum_variant, enum_ty);
        for (field_idx, field_ty) in ctor.field_types(db).iter().copied().enumerate() {
            let proj = Projection::VariantField {
                variant: enum_variant,
                enum_ty,
                field_idx,
            };
            let dst_field = extend_place(dst_place, proj.clone());
            let src_field = extend_place(src_place, proj);
            deep_copy_from_places(
                fb,
                db,
                target_layout,
                &dst_field,
                &src_field,
                field_ty,
                body,
                value_map,
                local_vars,
                is,
            )?;
        }
        fb.insert_inst_no_result(Jump::new(is, cont_block));
    }

    fb.switch_to_block(cont_block);
    Ok(())
}

fn extend_place<'db>(place: &Place<'db>, proj: mir::ir::MirProjection<'db>) -> Place<'db> {
    let mut path = place.projection.clone();
    path.push(proj);
    Place::new(place.base, path)
}
