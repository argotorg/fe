//! Sonatina backend for direct EVM bytecode generation.
//!
//! This module translates Fe MIR to Sonatina IR, which is then compiled
//! to EVM bytecode without going through Yul/solc.

mod types;

use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use mir::{MirModule, layout::TargetDataLayout, lower_module};
use rustc_hash::FxHashMap;
use sonatina_ir::{
    BlockId, Module, Signature, ValueId,
    builder::ModuleBuilder,
    func_cursor::InstInserter,
    inst::control_flow::{Br, Jump, Return},
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
        OperatingSystem::Evm(EvmVersion::Cancun),
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

        // Create blocks
        for (idx, _block) in func.body.blocks.iter().enumerate() {
            let block_id = mir::BasicBlockId(idx as u32);
            let sonatina_block = fb.append_block();
            block_map.insert(block_id, sonatina_block);
        }

        // Get the entry block and its Sonatina equivalent
        let entry_block = func.body.entry;
        let _sonatina_entry = block_map[&entry_block];

        // Map function arguments
        let args = fb.args();
        for (i, _local_id) in func.body.param_locals.iter().enumerate() {
            // Find the value ID for this parameter local
            // Parameters are typically the first values in the value array
            let value_id = mir::ValueId(i as u32);
            if i < args.len() {
                value_map.insert(value_id, args[i]);
            }
        }

        // Lower each block
        for (idx, block) in func.body.blocks.iter().enumerate() {
            let block_id = mir::BasicBlockId(idx as u32);
            let sonatina_block = block_map[&block_id];
            fb.switch_to_block(sonatina_block);

            // TODO: Lower instructions in this block
            for _inst in block.insts.iter() {
                // Instruction lowering will be implemented in Phase 2
            }

            // Lower terminator
            lower_terminator(&mut fb, &block.terminator, &block_map, &value_map, is)?;
        }

        // Seal all blocks and finalize
        fb.seal_all();
        fb.finish();

        Ok(())
    }
}

/// Lower a block terminator.
fn lower_terminator<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    term: &mir::Terminator<'_>,
    block_map: &FxHashMap<mir::BasicBlockId, BlockId>,
    value_map: &FxHashMap<mir::ValueId, ValueId>,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> Result<(), LowerError> {
    use mir::Terminator;

    match term {
        Terminator::Return(ret_val) => {
            let ret_sonatina = ret_val.and_then(|v| value_map.get(&v).copied());
            fb.insert_inst_no_result(Return::new(is, ret_sonatina));
        }
        Terminator::Goto { target } => {
            let target_block = block_map[target];
            fb.insert_inst_no_result(Jump::new(is, target_block));
        }
        Terminator::Branch { cond, then_bb, else_bb } => {
            let cond_val = value_map.get(cond).copied().ok_or_else(|| {
                LowerError::Internal(format!("branch condition {:?} not found", cond))
            })?;
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
