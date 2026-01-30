//! Sonatina backend for direct EVM bytecode generation.
//!
//! This module translates Fe MIR to Sonatina IR, which is then compiled
//! to EVM bytecode without going through Yul/solc.

mod tests;
mod types;

use driver::DriverDataBase;
use hir::analysis::ty::adt_def::AdtRef;
use hir::analysis::ty::ty_def::{PrimTy, TyBase, TyData};
use hir::hir_def::TopLevelMod;
use hir::hir_def::expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp};
use hir::projection::{IndexSource, Projection};
use mir::ir::{AddressSpaceKind, IntrinsicOp, Place, SyntheticValue};
use mir::{MirModule, layout, layout::TargetDataLayout, lower_module};
use num_bigint::BigUint;
use rustc_hash::{FxHashMap, FxHashSet};
use sonatina_ir::{
    BlockId, I256, Module, Signature, Type, ValueId,
    builder::{ModuleBuilder, Variable},
    func_cursor::InstInserter,
    inst::{
        arith::{Add, Mul, Neg, Shl, Shr, Sub},
        cast::{Sext, Trunc, Zext},
        cmp::{Eq, Gt, IsZero, Lt, Ne},
        control_flow::{Br, Call, Jump, Return},
        data::{Mload, Mstore, SymAddr, SymSize, SymbolRef},
        evm::{
            EvmAddress, EvmBaseFee, EvmBlockHash, EvmCall, EvmCallValue, EvmCalldataCopy,
            EvmCalldataLoad, EvmCalldataSize, EvmCaller, EvmChainId, EvmCodeCopy, EvmCodeSize,
            EvmCoinBase, EvmCreate, EvmCreate2, EvmDelegateCall, EvmExp, EvmGas, EvmGasLimit,
            EvmInvalid, EvmKeccak256, EvmLog0, EvmLog1, EvmLog2, EvmLog3, EvmLog4, EvmMalloc,
            EvmMsize, EvmMstore8, EvmNumber, EvmOrigin, EvmPrevRandao, EvmReturn,
            EvmReturnDataCopy, EvmReturnDataSize, EvmRevert, EvmSelfBalance, EvmSelfDestruct,
            EvmSload, EvmSstore, EvmStaticCall, EvmStop, EvmTimestamp, EvmTload, EvmTstore,
            EvmUdiv, EvmUmod,
        },
        logic::{And, Not, Or, Xor},
    },
    ir_writer::ModuleWriter,
    isa::{Isa, evm::Evm},
    module::{FuncRef, ModuleCtx},
    object::{Directive, Embed, EmbedSymbol, Object, ObjectName, Section, SectionName, SectionRef},
};
use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

use crate::BackendError;
pub use tests::emit_test_module_sonatina;

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

fn is_erased_runtime_ty(
    db: &DriverDataBase,
    target_layout: &TargetDataLayout,
    ty: hir::analysis::ty::ty_def::TyId<'_>,
) -> bool {
    layout::ty_size_bytes_in(db, target_layout, ty).is_some_and(|s| s == 0)
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

/// Compiles a Fe module to Sonatina IR and returns the human-readable text representation.
///
/// This is useful for snapshot testing and debugging the IR output.
pub fn emit_module_sonatina_ir(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
) -> Result<String, LowerError> {
    let layout = layout::EVM_LAYOUT;
    let module = compile_module(db, top_mod, layout)?;
    let mut writer = ModuleWriter::new(&module);
    Ok(writer.dump_string())
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
    /// Indices of functions executed directly by the EVM (empty stack).
    ///
    /// These entry functions emit `evm_stop` instead of internal `Return`.
    entry_func_idxs: FxHashSet<usize>,
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
            entry_func_idxs: FxHashSet::default(),
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
            if func.symbol_name.is_empty() {
                continue;
            }
            let name = &func.symbol_name;
            let sig = self.lower_signature(func)?;

            let func_ref = self.builder.declare_function(sig).map_err(|e| {
                LowerError::Internal(format!("failed to declare function {name}: {e}"))
            })?;

            self.func_map.insert(idx, func_ref);
            self.name_map.insert(name.clone(), func_ref);
            self.returns_value_map
                .insert(name.clone(), func.returns_value);
        }
        Ok(())
    }

    /// Lower function signatures.
    fn lower_signature(&self, func: &mir::MirFunction<'db>) -> Result<Signature, LowerError> {
        use mir::ir::{ContractFunctionKind, MirFunctionOrigin, SyntheticId};

        let name = &func.symbol_name;
        let linkage = sonatina_ir::Linkage::Public; // TODO: proper linkage

        // Contract init/runtime entrypoints are executed directly by the EVM with an empty stack.
        // Even though MIR models them as taking effect args (e.g. `StorPtr<Evm>`), we cannot
        // expose those as real EVM stack parameters at entry.
        let is_contract_entry = func.contract_function.as_ref().is_some_and(|cf| {
            matches!(
                cf.kind,
                ContractFunctionKind::Init | ContractFunctionKind::Runtime
            )
        }) || matches!(
            func.origin,
            MirFunctionOrigin::Synthetic(
                SyntheticId::ContractInitEntrypoint(_) | SyntheticId::ContractRuntimeEntrypoint(_)
            )
        );

        // Convert parameter types - all EVM parameters are 256-bit words.
        // Entry functions have no parameters since the EVM starts with an empty stack.
        let mut params = Vec::new();
        if !is_contract_entry {
            for local_id in func.body.param_locals.iter().copied() {
                let local_ty = func
                    .body
                    .locals
                    .get(local_id.index())
                    .ok_or_else(|| {
                        LowerError::Internal(format!("unknown param local: {local_id:?}"))
                    })?
                    .ty;
                if is_erased_runtime_ty(self.db, &self.target_layout, local_ty) {
                    continue;
                }
                params.push(types::word_type());
            }
            for local_id in func.body.effect_param_locals.iter().copied() {
                let local_ty = func
                    .body
                    .locals
                    .get(local_id.index())
                    .ok_or_else(|| {
                        LowerError::Internal(format!("unknown effect param local: {local_id:?}"))
                    })?
                    .ty;
                if is_erased_runtime_ty(self.db, &self.target_layout, local_ty) {
                    continue;
                }
                params.push(types::word_type());
            }
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
            let is_entry = self.entry_func_idxs.contains(&idx);
            self.lower_function(func_ref, func, is_entry)?;
        }
        Ok(())
    }

    /// Create Sonatina objects for the module.
    ///
    /// Objects define how code is organized for compilation. Each object
    /// has sections (like "runtime" and "init") that contain function entries.
    ///
    /// For contract runtime entrypoints: The entry function emits `evm_stop` instead of
    /// internal `Return` since contract dispatchers handle return data via `evm_return`.
    ///
    /// For simple test files (no explicit contract): A wrapper function calls the entry
    /// and then does `evm_stop`, preserving internal function call semantics.
    fn create_objects(&mut self) -> Result<(), LowerError> {
        use mir::analysis::build_contract_graph;

        let contract_graph = build_contract_graph(&self.mir.functions);
        if !contract_graph.contracts.is_empty() {
            return self.create_contract_objects(&contract_graph);
        }

        // No contract annotations: fall back to compiling a single "main" entry. This is used by
        // snapshot tests for simple files and debugging.
        let Some((entry_idx, entry_mir_func)) = self
            .mir
            .functions
            .iter()
            .enumerate()
            .find(|(idx, _)| self.func_map.contains_key(idx))
        else {
            // No functions to compile - this is valid for empty modules.
            return Ok(());
        };

        let entry_ref = self.func_map[&entry_idx];
        let wrapper_ref = self.create_entry_wrapper(entry_ref, entry_mir_func)?;
        let directives = vec![Directive::Entry(wrapper_ref)];

        let object = Object {
            name: ObjectName::from("Contract"),
            sections: vec![Section {
                name: SectionName::from("runtime"),
                directives,
            }],
        };

        self.builder
            .declare_object(object)
            .map_err(|e| LowerError::Internal(format!("failed to declare object: {e}")))?;

        Ok(())
    }

    fn create_contract_objects(
        &mut self,
        contract_graph: &mir::analysis::ContractGraph,
    ) -> Result<(), LowerError> {
        use mir::analysis::{ContractRegion, ContractRegionKind};
        use std::collections::VecDeque;

        let mut func_idx_by_symbol: FxHashMap<&str, usize> = FxHashMap::default();
        for (idx, func) in self.mir.functions.iter().enumerate() {
            if !self.func_map.contains_key(&idx) {
                continue;
            }
            func_idx_by_symbol.insert(func.symbol_name.as_str(), idx);
        }

        // Pick a primary contract to compile:
        // - prefer a root contract (not referenced by others)
        // - otherwise fall back to the first contract name (deterministic sort)
        let mut referenced_contracts: FxHashSet<String> = FxHashSet::default();
        for (from_region, deps) in &contract_graph.region_deps {
            for dep in deps {
                if dep.contract_name != from_region.contract_name {
                    referenced_contracts.insert(dep.contract_name.clone());
                }
            }
        }

        let mut root_contracts: Vec<String> = contract_graph
            .contracts
            .keys()
            .filter(|name| !referenced_contracts.contains(*name))
            .cloned()
            .collect();
        root_contracts.sort();

        let primary_contract = root_contracts
            .into_iter()
            .next()
            .or_else(|| {
                let mut names: Vec<String> = contract_graph.contracts.keys().cloned().collect();
                names.sort();
                names.into_iter().next()
            })
            .ok_or_else(|| {
                LowerError::Internal("contract graph is unexpectedly empty".to_string())
            })?;

        // Collect the transitive set of contracts needed by the primary contract.
        let mut needed_contracts: FxHashSet<String> = FxHashSet::default();
        let mut queue = VecDeque::new();
        queue.push_back(primary_contract.clone());
        while let Some(contract_name) = queue.pop_front() {
            if !needed_contracts.insert(contract_name.clone()) {
                continue;
            }

            for kind in [ContractRegionKind::Init, ContractRegionKind::Deployed] {
                let region = ContractRegion {
                    contract_name: contract_name.clone(),
                    kind,
                };
                let Some(deps) = contract_graph.region_deps.get(&region) else {
                    continue;
                };
                for dep in deps {
                    if dep.contract_name != contract_name {
                        queue.push_back(dep.contract_name.clone());
                    }
                }
            }
        }

        // Assign stable object names for each needed contract.
        let mut contract_object_names: FxHashMap<String, ObjectName> = FxHashMap::default();
        let mut ordered_contracts: Vec<String> = needed_contracts.into_iter().collect();
        ordered_contracts.sort();
        for contract in &ordered_contracts {
            let object_name = ObjectName::from(contract.clone());
            contract_object_names.insert(contract.clone(), object_name);
        }

        // Emit the primary object first for readability, then all remaining contracts.
        ordered_contracts.retain(|c| c != &primary_contract);
        ordered_contracts.insert(0, primary_contract.clone());

        for contract_name in ordered_contracts {
            let object_name = contract_object_names
                .get(&contract_name)
                .cloned()
                .ok_or_else(|| {
                    LowerError::Internal(format!("missing object name for `{contract_name}`"))
                })?;

            let Some(info) = contract_graph.contracts.get(&contract_name) else {
                return Err(LowerError::Internal(format!(
                    "missing contract info for `{contract_name}`"
                )));
            };

            let init_section_name = SectionName::from("init");
            let runtime_section_name = SectionName::from("runtime");

            let mut sections = Vec::new();

            let runtime_symbol = info.deployed_symbol.as_deref();
            if let Some(runtime_symbol) = runtime_symbol {
                let runtime_ref = *self.name_map.get(runtime_symbol).ok_or_else(|| {
                    LowerError::Internal(format!(
                        "unknown contract runtime entrypoint symbol: `{runtime_symbol}`"
                    ))
                })?;
                let runtime_idx = *func_idx_by_symbol.get(runtime_symbol).ok_or_else(|| {
                    LowerError::Internal(format!(
                        "unknown contract runtime entrypoint index: `{runtime_symbol}`"
                    ))
                })?;
                self.entry_func_idxs.insert(runtime_idx);

                let region = ContractRegion {
                    contract_name: contract_name.clone(),
                    kind: ContractRegionKind::Deployed,
                };
                let deps = contract_graph
                    .region_deps
                    .get(&region)
                    .cloned()
                    .unwrap_or_default();
                let mut directives = vec![Directive::Entry(runtime_ref)];
                directives.extend(Self::build_embed_directives(
                    &contract_name,
                    ContractRegionKind::Deployed,
                    &deps,
                    &contract_object_names,
                    contract_graph,
                    &init_section_name,
                    &runtime_section_name,
                )?);

                sections.push(Section {
                    name: runtime_section_name.clone(),
                    directives,
                });
            }

            if let Some(init_symbol) = info.init_symbol.as_deref() {
                let init_ref = *self.name_map.get(init_symbol).ok_or_else(|| {
                    LowerError::Internal(format!(
                        "unknown contract init entrypoint symbol: `{init_symbol}`"
                    ))
                })?;
                let init_idx = *func_idx_by_symbol.get(init_symbol).ok_or_else(|| {
                    LowerError::Internal(format!(
                        "unknown contract init entrypoint index: `{init_symbol}`"
                    ))
                })?;
                self.entry_func_idxs.insert(init_idx);

                let region = ContractRegion {
                    contract_name: contract_name.clone(),
                    kind: ContractRegionKind::Init,
                };
                let mut deps = contract_graph
                    .region_deps
                    .get(&region)
                    .cloned()
                    .unwrap_or_default();

                // The init section must embed the runtime section so `code_region_offset/len`
                // for the runtime root can be lowered via `symaddr/symsize`.
                if info.deployed_symbol.is_some() {
                    deps.insert(ContractRegion {
                        contract_name: contract_name.clone(),
                        kind: ContractRegionKind::Deployed,
                    });
                }

                let mut directives = vec![Directive::Entry(init_ref)];
                directives.extend(Self::build_embed_directives(
                    &contract_name,
                    ContractRegionKind::Init,
                    &deps,
                    &contract_object_names,
                    contract_graph,
                    &init_section_name,
                    &runtime_section_name,
                )?);

                sections.push(Section {
                    name: init_section_name,
                    directives,
                });
            }

            // Ensure section order is stable (init before runtime).
            sections.sort_by(|a, b| a.name.0.cmp(&b.name.0));

            self.builder
                .declare_object(Object {
                    name: object_name,
                    sections,
                })
                .map_err(|e| LowerError::Internal(format!("failed to declare object: {e}")))?;
        }

        Ok(())
    }

    fn build_embed_directives(
        current_contract: &str,
        current_kind: mir::analysis::ContractRegionKind,
        deps: &FxHashSet<mir::analysis::ContractRegion>,
        contract_object_names: &FxHashMap<String, ObjectName>,
        contract_graph: &mir::analysis::ContractGraph,
        init_section_name: &SectionName,
        runtime_section_name: &SectionName,
    ) -> Result<Vec<Directive>, LowerError> {
        use mir::analysis::{ContractRegion, ContractRegionKind};

        let mut deps: Vec<ContractRegion> = deps.iter().cloned().collect();
        deps.sort();
        deps.dedup();

        let mut directives = Vec::new();
        for dep in deps {
            if dep.contract_name == current_contract && dep.kind == current_kind {
                continue;
            }

            let Some(dep_info) = contract_graph.contracts.get(&dep.contract_name) else {
                return Err(LowerError::Internal(format!(
                    "code region dep refers to unknown contract `{}`",
                    dep.contract_name
                )));
            };

            let (dep_symbol, dep_section) = match dep.kind {
                ContractRegionKind::Init => (
                    dep_info.init_symbol.as_ref().ok_or_else(|| {
                        LowerError::Internal(format!(
                            "contract `{}` has no init entrypoint symbol",
                            dep.contract_name
                        ))
                    })?,
                    init_section_name.clone(),
                ),
                ContractRegionKind::Deployed => (
                    dep_info.deployed_symbol.as_ref().ok_or_else(|| {
                        LowerError::Internal(format!(
                            "contract `{}` has no runtime entrypoint symbol",
                            dep.contract_name
                        ))
                    })?,
                    runtime_section_name.clone(),
                ),
            };

            let source = if dep.contract_name == current_contract {
                SectionRef::Local(dep_section)
            } else {
                let object = contract_object_names
                    .get(&dep.contract_name)
                    .cloned()
                    .ok_or_else(|| {
                        LowerError::Internal(format!(
                            "missing object name for dependent contract `{}`",
                            dep.contract_name
                        ))
                    })?;
                SectionRef::External {
                    object,
                    section: dep_section,
                }
            };

            directives.push(Directive::Embed(Embed {
                source,
                as_symbol: EmbedSymbol::from(dep_symbol.clone()),
            }));
        }

        Ok(directives)
    }

    /// Create a wrapper entrypoint for simple test files (non-contract modules).
    ///
    /// The wrapper calls the actual entry function and then halts with `evm_stop`.
    /// This is needed because the entry function uses internal `Return` which requires
    /// a return address on the stack (pushed by `Call`).
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
            LowerError::Internal(format!(
                "failed to declare entry wrapper `{WRAPPER_NAME}`: {e}"
            ))
        })?;

        let mut fb = self.builder.func_builder::<InstInserter>(func_ref);
        let is = self.isa.inst_set();

        let entry_block = fb.append_block();
        fb.switch_to_block(entry_block);

        // Pass zero for all arguments (regular + effect params). Test entry functions
        // generally don't use effect params meaningfully.
        let argc = entry_mir_func
            .body
            .param_locals
            .iter()
            .chain(entry_mir_func.body.effect_param_locals.iter())
            .copied()
            .filter(|local_id| {
                let local_ty = entry_mir_func
                    .body
                    .locals
                    .get(local_id.index())
                    .map(|l| l.ty);
                let Some(local_ty) = local_ty else {
                    return true;
                };
                !is_erased_runtime_ty(self.db, &self.target_layout, local_ty)
            })
            .count();
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
    ///
    /// If `is_entry` is true, this is the entry function executed directly by the EVM.
    /// Entry functions emit `evm_stop` instead of internal `Return` for their terminators.
    fn lower_function(
        &self,
        func_ref: FuncRef,
        func: &mir::MirFunction<'db>,
        is_entry: bool,
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
        let all_param_locals: Vec<_> = func
            .body
            .param_locals
            .iter()
            .chain(func.body.effect_param_locals.iter())
            .copied()
            .collect();

        if is_entry {
            // Entry functions have no Sonatina parameters (EVM starts with empty stack).
            // Initialize all param locals to zero - effect params are erased at runtime
            // and regular params shouldn't exist for entry functions.
            for local_id in all_param_locals {
                let var = local_vars.get(&local_id).copied().ok_or_else(|| {
                    LowerError::Internal(format!(
                        "missing SSA variable for param local {local_id:?}"
                    ))
                })?;
                let zero = fb.make_imm_value(I256::zero());
                fb.def_var(var, zero);
            }
        } else {
            // Non-entry functions: map actual arguments to param locals.
            let args = fb.args().to_vec();
            let mut arg_iter = args.into_iter();
            let zero = fb.make_imm_value(I256::zero());
            for local_id in all_param_locals {
                let var = local_vars.get(&local_id).copied().ok_or_else(|| {
                    LowerError::Internal(format!(
                        "missing SSA variable for param local {local_id:?}"
                    ))
                })?;

                let local_ty = func
                    .body
                    .locals
                    .get(local_id.index())
                    .ok_or_else(|| {
                        LowerError::Internal(format!("unknown param local: {local_id:?}"))
                    })?
                    .ty;
                if is_erased_runtime_ty(self.db, &self.target_layout, local_ty) {
                    fb.def_var(var, zero);
                    continue;
                }

                let arg_val = arg_iter.next().unwrap_or(zero);
                fb.def_var(var, arg_val);
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
                is_entry,
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
                let value =
                    lower_alloc(fb, db, target_layout, *dest_local, *address_space, body, is)?;
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
                    let dest_ty = body
                        .locals
                        .get(dest_local.index())
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
            let _ = lower_value(
                fb,
                db,
                target_layout,
                *value,
                body,
                value_map,
                local_vars,
                is,
            )?;
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
            let val = lower_value(
                fb,
                db,
                target_layout,
                *value_id,
                body,
                value_map,
                local_vars,
                is,
            )?;
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
                        match (callee_name.as_str(), args.as_slice()) {
                            ("log0", [offset, len]) => {
                                fb.insert_inst_no_result(EvmLog0::new(is, *offset, *len));
                                return Ok(None);
                            }
                            ("log1", [offset, len, topic0]) => {
                                fb.insert_inst_no_result(EvmLog1::new(is, *offset, *len, *topic0));
                                return Ok(None);
                            }
                            ("log2", [offset, len, topic0, topic1]) => {
                                fb.insert_inst_no_result(EvmLog2::new(
                                    is, *offset, *len, *topic0, *topic1,
                                ));
                                return Ok(None);
                            }
                            ("log3", [offset, len, topic0, topic1, topic2]) => {
                                fb.insert_inst_no_result(EvmLog3::new(
                                    is, *offset, *len, *topic0, *topic1, *topic2,
                                ));
                                return Ok(None);
                            }
                            ("log4", [offset, len, topic0, topic1, topic2, topic3]) => {
                                fb.insert_inst_no_result(EvmLog4::new(
                                    is, *offset, *len, *topic0, *topic1, *topic2, *topic3,
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
                    "address" => return Ok(Some(fb.insert_inst(EvmAddress::new(is), Type::I256))),
                    "callvalue" => {
                        return Ok(Some(fb.insert_inst(EvmCallValue::new(is), Type::I256)));
                    }
                    "origin" => return Ok(Some(fb.insert_inst(EvmOrigin::new(is), Type::I256))),
                    "gasprice" => {
                        return Err(LowerError::Unsupported(
                            "gasprice is not supported by the Sonatina backend".to_string(),
                        ));
                    }
                    "coinbase" => {
                        return Ok(Some(fb.insert_inst(EvmCoinBase::new(is), Type::I256)));
                    }
                    "timestamp" => {
                        return Ok(Some(fb.insert_inst(EvmTimestamp::new(is), Type::I256)));
                    }
                    "number" => return Ok(Some(fb.insert_inst(EvmNumber::new(is), Type::I256))),
                    "prevrandao" => {
                        return Ok(Some(fb.insert_inst(EvmPrevRandao::new(is), Type::I256)));
                    }
                    "gaslimit" => {
                        return Ok(Some(fb.insert_inst(EvmGasLimit::new(is), Type::I256)));
                    }
                    "chainid" => return Ok(Some(fb.insert_inst(EvmChainId::new(is), Type::I256))),
                    "basefee" => return Ok(Some(fb.insert_inst(EvmBaseFee::new(is), Type::I256))),
                    "selfbalance" => {
                        return Ok(Some(fb.insert_inst(EvmSelfBalance::new(is), Type::I256)));
                    }
                    "blockhash" => {
                        let [block] = call.args.as_slice() else {
                            return Err(LowerError::Internal(
                                "blockhash requires 1 argument".to_string(),
                            ));
                        };
                        let block = lower_value(
                            fb,
                            db,
                            target_layout,
                            *block,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        return Ok(Some(
                            fb.insert_inst(EvmBlockHash::new(is, block), Type::I256),
                        ));
                    }
                    "gas" => return Ok(Some(fb.insert_inst(EvmGas::new(is), Type::I256))),

                    // Memory size
                    "msize" => return Ok(Some(fb.insert_inst(EvmMsize::new(is), Type::I256))),

                    // Calls / create
                    "create" => {
                        let [val, offset, len] = call.args.as_slice() else {
                            return Err(LowerError::Internal(
                                "create requires 3 arguments".to_string(),
                            ));
                        };
                        let val = lower_value(
                            fb,
                            db,
                            target_layout,
                            *val,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let offset = lower_value(
                            fb,
                            db,
                            target_layout,
                            *offset,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let len = lower_value(
                            fb,
                            db,
                            target_layout,
                            *len,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        return Ok(Some(
                            fb.insert_inst(EvmCreate::new(is, val, offset, len), Type::I256),
                        ));
                    }
                    "create2" => {
                        let [val, offset, len, salt] = call.args.as_slice() else {
                            return Err(LowerError::Internal(
                                "create2 requires 4 arguments".to_string(),
                            ));
                        };
                        let val = lower_value(
                            fb,
                            db,
                            target_layout,
                            *val,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let offset = lower_value(
                            fb,
                            db,
                            target_layout,
                            *offset,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let len = lower_value(
                            fb,
                            db,
                            target_layout,
                            *len,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let salt = lower_value(
                            fb,
                            db,
                            target_layout,
                            *salt,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        return Ok(Some(fb.insert_inst(
                            EvmCreate2::new(is, val, offset, len, salt),
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
                        let gas = lower_value(
                            fb,
                            db,
                            target_layout,
                            *gas,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let addr = lower_value(
                            fb,
                            db,
                            target_layout,
                            *addr,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let val = lower_value(
                            fb,
                            db,
                            target_layout,
                            *val,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let arg_offset = lower_value(
                            fb,
                            db,
                            target_layout,
                            *arg_offset,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let arg_len = lower_value(
                            fb,
                            db,
                            target_layout,
                            *arg_len,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let ret_offset = lower_value(
                            fb,
                            db,
                            target_layout,
                            *ret_offset,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let ret_len = lower_value(
                            fb,
                            db,
                            target_layout,
                            *ret_len,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        return Ok(Some(fb.insert_inst(
                            EvmCall::new(
                                is, gas, addr, val, arg_offset, arg_len, ret_offset, ret_len,
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
                        let gas = lower_value(
                            fb,
                            db,
                            target_layout,
                            *gas,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let addr = lower_value(
                            fb,
                            db,
                            target_layout,
                            *addr,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let arg_offset = lower_value(
                            fb,
                            db,
                            target_layout,
                            *arg_offset,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let arg_len = lower_value(
                            fb,
                            db,
                            target_layout,
                            *arg_len,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let ret_offset = lower_value(
                            fb,
                            db,
                            target_layout,
                            *ret_offset,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let ret_len = lower_value(
                            fb,
                            db,
                            target_layout,
                            *ret_len,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        return Ok(Some(fb.insert_inst(
                            EvmStaticCall::new(
                                is, gas, addr, arg_offset, arg_len, ret_offset, ret_len,
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
                        let gas = lower_value(
                            fb,
                            db,
                            target_layout,
                            *gas,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let addr = lower_value(
                            fb,
                            db,
                            target_layout,
                            *addr,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let arg_offset = lower_value(
                            fb,
                            db,
                            target_layout,
                            *arg_offset,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let arg_len = lower_value(
                            fb,
                            db,
                            target_layout,
                            *arg_len,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let ret_offset = lower_value(
                            fb,
                            db,
                            target_layout,
                            *ret_offset,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        let ret_len = lower_value(
                            fb,
                            db,
                            target_layout,
                            *ret_len,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        return Ok(Some(fb.insert_inst(
                            EvmDelegateCall::new(
                                is, gas, addr, arg_offset, arg_len, ret_offset, ret_len,
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
                        let size_ty = body
                            .values
                            .get(size.index())
                            .ok_or_else(|| {
                                LowerError::Internal("unknown call argument".to_string())
                            })?
                            .ty;
                        if is_erased_runtime_ty(db, target_layout, size_ty) {
                            return Err(LowerError::Internal(
                                "alloc size argument unexpectedly erased".to_string(),
                            ));
                        }
                        let size = lower_value(
                            fb,
                            db,
                            target_layout,
                            *size,
                            body,
                            value_map,
                            local_vars,
                            is,
                        )?;
                        return Ok(Some(fb.insert_inst(EvmMalloc::new(is, size), Type::I256)));
                    }
                    "evm_create_create_raw" => {
                        let mut lowered = Vec::new();
                        for &arg in &call.args {
                            let arg_ty = body
                                .values
                                .get(arg.index())
                                .ok_or_else(|| {
                                    LowerError::Internal("unknown call argument".to_string())
                                })?
                                .ty;
                            if is_erased_runtime_ty(db, target_layout, arg_ty) {
                                continue;
                            }
                            lowered.push(lower_value(
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

                        let [val, offset, len] = lowered.as_slice() else {
                            return Err(LowerError::Internal(format!(
                                "{callee_name} expects 3 args (value, offset, len) after ZST erasure, got {}",
                                lowered.len()
                            )));
                        };
                        return Ok(Some(
                            fb.insert_inst(EvmCreate::new(is, *val, *offset, *len), Type::I256),
                        ));
                    }
                    "evm_create_create2_raw" => {
                        let mut lowered = Vec::new();
                        for &arg in &call.args {
                            let arg_ty = body
                                .values
                                .get(arg.index())
                                .ok_or_else(|| {
                                    LowerError::Internal("unknown call argument".to_string())
                                })?
                                .ty;
                            if is_erased_runtime_ty(db, target_layout, arg_ty) {
                                continue;
                            }
                            lowered.push(lower_value(
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

                        let [val, offset, len, salt] = lowered.as_slice() else {
                            return Err(LowerError::Internal(format!(
                                "{callee_name} expects 4 args (value, offset, len, salt) after ZST erasure, got {}",
                                lowered.len()
                            )));
                        };
                        return Ok(Some(fb.insert_inst(
                            EvmCreate2::new(is, *val, *offset, *len, *salt),
                            Type::I256,
                        )));
                    }
                    _ => {}
                }
            }

            let func_ref = name_map
                .get(callee_name)
                .ok_or_else(|| LowerError::Internal(format!("unknown function: {callee_name}")))?;

            // Lower arguments (regular args + effect args)
            let mut args = Vec::with_capacity(call.args.len() + call.effect_args.len());
            for &arg in &call.args {
                let arg_ty = body
                    .values
                    .get(arg.index())
                    .ok_or_else(|| LowerError::Internal("unknown call argument".to_string()))?
                    .ty;
                if is_erased_runtime_ty(db, target_layout, arg_ty) {
                    continue;
                }
                let val = lower_value(fb, db, target_layout, arg, body, value_map, local_vars, is)?;
                args.push(val);
            }
            for &effect_arg in &call.effect_args {
                let arg_ty = body
                    .values
                    .get(effect_arg.index())
                    .ok_or_else(|| {
                        LowerError::Internal("unknown call effect argument".to_string())
                    })?
                    .ty;
                if is_erased_runtime_ty(db, target_layout, arg_ty) {
                    continue;
                }
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

            // If the caller erased some compile-time-only arguments but the callee signature still
            // expects stack words for them, pad with zeroes to keep the internal call ABI aligned.
            let expected_argc = fb
                .module_builder
                .ctx
                .func_sig(*func_ref, |sig| sig.args().len());
            if args.len() > expected_argc {
                return Err(LowerError::Internal(format!(
                    "call to `{callee_name}` has too many args (got {}, expected {expected_argc})",
                    args.len()
                )));
            }
            while args.len() < expected_argc {
                args.push(fb.make_imm_value(I256::zero()));
            }

            // Emit call instruction with proper return type
            let call_inst = Call::new(is, *func_ref, args.into());
            let callee_returns = returns_value_map.get(callee_name).copied().ok_or_else(|| {
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
        Rvalue::Intrinsic { op, args } => lower_intrinsic(
            fb,
            db,
            target_layout,
            *op,
            args,
            body,
            value_map,
            local_vars,
            is,
        ),
        Rvalue::Load { place } => {
            let addr = lower_place_address(
                fb,
                db,
                target_layout,
                place,
                body,
                value_map,
                local_vars,
                is,
            )?;
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
        Rvalue::Alloc { .. } => Err(LowerError::Internal(
            "Alloc rvalue should be handled directly in Assign lowering".to_string(),
        )),
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
    // Avoid caching immediates: some backends treat operand lists as a set rather than a multiset,
    // so reusing the same `ValueId` for repeated immediates can lead to missing stack items when an
    // instruction needs multiple copies (e.g., `create2` with `value=0` and `salt=0`).
    let cacheable = !matches!(
        value_data.origin,
        mir::ValueOrigin::Local(_)
            | mir::ValueOrigin::PlaceRef(_)
            | mir::ValueOrigin::Synthetic(_)
            | mir::ValueOrigin::Unit
    );
    if cacheable && let Some(&val) = value_map.get(&value_id) {
        return Ok(val);
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
            let inner_val = lower_value(
                fb,
                db,
                target_layout,
                *inner,
                body,
                value_map,
                local_vars,
                is,
            )?;
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
            lower_value(
                fb,
                db,
                target_layout,
                *value,
                body,
                value_map,
                local_vars,
                is,
            )
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
            lower_place_address(
                fb,
                db,
                target_layout,
                place,
                body,
                value_map,
                local_vars,
                is,
            )
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
    if matches!(
        op,
        IntrinsicOp::CodeRegionOffset | IntrinsicOp::CodeRegionLen
    ) {
        let [func_item] = args else {
            return Err(LowerError::Internal(
                "code region intrinsics require 1 argument".to_string(),
            ));
        };
        let value_data = body
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
            IntrinsicOp::CodeRegionOffset => {
                Ok(Some(fb.insert_inst(SymAddr::new(is, sym), Type::I256)))
            }
            IntrinsicOp::CodeRegionLen => {
                Ok(Some(fb.insert_inst(SymSize::new(is, sym), Type::I256)))
            }
            _ => unreachable!(),
        };
    }

    // Lower all arguments first
    let mut lowered_args = Vec::with_capacity(args.len());
    for &arg in args {
        let val = lower_value(fb, db, target_layout, arg, body, value_map, local_vars, is)?;
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
        IntrinsicOp::Mload => {
            let Some(&addr) = lowered_args.first() else {
                return Err(LowerError::Internal(
                    "mload requires address argument".to_string(),
                ));
            };
            Ok(Some(
                fb.insert_inst(Mload::new(is, addr, Type::I256), Type::I256),
            ))
        }
        IntrinsicOp::Mstore => {
            let [addr, val] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "mstore requires 2 arguments".to_string(),
                ));
            };
            fb.insert_inst_no_result(Mstore::new(is, *addr, *val, Type::I256));
            Ok(None)
        }
        IntrinsicOp::Mstore8 => {
            let [addr, val] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "mstore8 requires 2 arguments".to_string(),
                ));
            };
            fb.insert_inst_no_result(EvmMstore8::new(is, *addr, *val));
            Ok(None)
        }
        IntrinsicOp::Sload => {
            let Some(&key) = lowered_args.first() else {
                return Err(LowerError::Internal(
                    "sload requires 1 argument".to_string(),
                ));
            };
            Ok(Some(fb.insert_inst(EvmSload::new(is, key), Type::I256)))
        }
        IntrinsicOp::Sstore => {
            let [key, val] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "sstore requires 2 arguments".to_string(),
                ));
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
            Ok(Some(
                fb.insert_inst(EvmCalldataLoad::new(is, offset), Type::I256),
            ))
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
        IntrinsicOp::Returndatasize => {
            Ok(Some(fb.insert_inst(EvmReturnDataSize::new(is), Type::I256)))
        }
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
        IntrinsicOp::CodeRegionOffset | IntrinsicOp::CodeRegionLen => {
            unreachable!("code region intrinsics are handled in the early return above")
        }
        IntrinsicOp::Keccak => {
            let [addr, len] = lowered_args.as_slice() else {
                return Err(LowerError::Internal(
                    "keccak requires 2 arguments".to_string(),
                ));
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
    let mut base_val = lower_value(
        fb,
        db,
        target_layout,
        place.base,
        body,
        value_map,
        local_vars,
        is,
    )?;

    if place.projection.is_empty() {
        return Ok(base_val);
    }

    // Get the base value's type to navigate projections
    let base_value = &body.values[place.base.index()];
    let mut current_ty = base_value.ty;
    if is_erased_runtime_ty(db, target_layout, current_ty) {
        return Ok(base_val);
    }
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
                        layout::variant_field_offset_slots(db, *enum_ty, *variant, *field_idx);
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
                let ctor = hir::analysis::ty::simplified_pattern::ConstructorKind::Variant(
                    *variant, *enum_ty,
                );
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
                        hir::analysis::ty::ty_def::TyBase::Prim(
                            hir::analysis::ty::ty_def::PrimTy::U256,
                        ),
                    ),
                );
            }
            Projection::Index(idx_source) => {
                let stride = if is_slot_addressed {
                    layout::array_elem_stride_slots(db, current_ty)
                } else {
                    layout::array_elem_stride_bytes_in(db, target_layout, current_ty)
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
                            let offset_val = fb.make_imm_value(I256::from(total_offset as u64));
                            base_val =
                                fb.insert_inst(Add::new(is, base_val, offset_val), Type::I256);
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
        let offset_val = fb.make_imm_value(I256::from(total_offset as u64));
        base_val = fb.insert_inst(Add::new(is, base_val, offset_val), Type::I256);
    }

    Ok(base_val)
}

/// Lower a block terminator.
///
/// If `is_entry` is true, `Return` terminators emit `evm_stop` instead of internal `Return`,
/// since the entry function is executed directly by the EVM and must halt with an EVM opcode.
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
    is_entry: bool,
) -> Result<(), LowerError> {
    use mir::Terminator;

    match term {
        Terminator::Return(ret_val) => {
            if is_entry {
                // Entry function: emit evm_stop to halt EVM execution.
                // Any return value is ignored since the entry function should have
                // already written return data via evm_return if needed.
                fb.insert_inst_no_result(EvmStop::new(is));
            } else {
                // Non-entry function: emit internal Return for function call semantics.
                let ret_sonatina = if let Some(v) = ret_val {
                    Some(lower_value(
                        fb,
                        db,
                        target_layout,
                        *v,
                        body,
                        value_map,
                        local_vars,
                        is,
                    )?)
                } else {
                    None
                };
                fb.insert_inst_no_result(Return::new(is, ret_sonatina));
            }
        }
        Terminator::Goto { target } => {
            let target_block = block_map[target];
            fb.insert_inst_no_result(Jump::new(is, target_block));
        }
        Terminator::Branch {
            cond,
            then_bb,
            else_bb,
        } => {
            let cond_val = lower_value(
                fb,
                db,
                target_layout,
                *cond,
                body,
                value_map,
                local_vars,
                is,
            )?;
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
            let discr_val = lower_value(
                fb,
                db,
                target_layout,
                *discr,
                body,
                value_map,
                local_vars,
                is,
            )?;
            let default_block = block_map[default];

            // NOTE: Sonatina's current EVM backend `BrTable` lowering is broken (it does not
            // compare against the scrutinee). Lower to a chain of `Eq` + `Br` instead.
            if targets.is_empty() {
                fb.insert_inst_no_result(Jump::new(is, default_block));
                return Ok(());
            }

            let mut cases = Vec::with_capacity(targets.len());
            for target in targets {
                let value = fb.make_imm_value(biguint_to_i256(&target.value.as_biguint()));
                let dest = block_map[&target.block];
                cases.push((value, dest));
            }

            // Create additional compare blocks as needed.
            let mut compare_blocks = Vec::with_capacity(cases.len().saturating_sub(1));
            for _ in 0..cases.len().saturating_sub(1) {
                compare_blocks.push(fb.append_block());
            }

            for (case_idx, (case_value, case_dest)) in cases.into_iter().enumerate() {
                if case_idx > 0 {
                    fb.switch_to_block(compare_blocks[case_idx - 1]);
                }

                let else_dest = if case_idx + 1 < compare_blocks.len() + 1 {
                    compare_blocks[case_idx]
                } else {
                    default_block
                };
                let cond = fb.insert_inst(Eq::new(is, discr_val, case_value), Type::I256);
                fb.insert_inst_no_result(Br::new(is, cond, case_dest, else_dest));
            }
        }
        Terminator::TerminatingCall(call) => match call {
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
                            fb.insert_inst_no_result(EvmStop::new(is));
                            return Ok(());
                        }
                        "selfdestruct" => {
                            let [addr] = call.args.as_slice() else {
                                return Err(LowerError::Internal(
                                    "selfdestruct requires 1 argument".to_string(),
                                ));
                            };
                            let addr = lower_value(
                                fb,
                                db,
                                target_layout,
                                *addr,
                                body,
                                value_map,
                                local_vars,
                                is,
                            )?;
                            fb.insert_inst_no_result(EvmSelfDestruct::new(is, addr));
                            return Ok(());
                        }
                        _ => {}
                    }
                }

                let func_ref = name_map.get(callee_name).ok_or_else(|| {
                    LowerError::Internal(format!("unknown function: {callee_name}"))
                })?;

                let mut args = Vec::with_capacity(call.args.len() + call.effect_args.len());
                for &arg in &call.args {
                    let arg_ty = body
                        .values
                        .get(arg.index())
                        .ok_or_else(|| LowerError::Internal("unknown call argument".to_string()))?
                        .ty;
                    if is_erased_runtime_ty(db, target_layout, arg_ty) {
                        continue;
                    }
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
                    let arg_ty = body
                        .values
                        .get(arg.index())
                        .ok_or_else(|| {
                            LowerError::Internal("unknown call effect argument".to_string())
                        })?
                        .ty;
                    if is_erased_runtime_ty(db, target_layout, arg_ty) {
                        continue;
                    }
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

                let expected_argc = fb
                    .module_builder
                    .ctx
                    .func_sig(*func_ref, |sig| sig.args().len());
                if args.len() > expected_argc {
                    return Err(LowerError::Internal(format!(
                        "terminating call to `{callee_name}` has too many args (got {}, expected {expected_argc})",
                        args.len()
                    )));
                }
                while args.len() < expected_argc {
                    args.push(fb.make_imm_value(I256::zero()));
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

    // Use Sonatina's EvmMalloc to allocate memory. This delegates memory management
    // to Sonatina's codegen, avoiding conflicts with its stack frame handling.
    let size_val = fb.make_imm_value(I256::from(size_bytes as u64));
    let ptr = fb.insert_inst(EvmMalloc::new(is, size_val), Type::I256);

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
    if is_erased_runtime_ty(db, target_layout, value_ty) {
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

    let raw_val = lower_value(
        fb,
        db,
        target_layout,
        value,
        body,
        value_map,
        local_vars,
        is,
    )?;
    let val = apply_to_word(fb, db, raw_val, value_ty, is);
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
    )
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
    let addr = lower_place_address(
        fb,
        db,
        target_layout,
        place,
        body,
        value_map,
        local_vars,
        is,
    )?;
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
    if is_erased_runtime_ty(db, target_layout, loaded_ty) {
        return Ok(fb.make_imm_value(I256::zero()));
    }

    let addr = lower_place_address(
        fb,
        db,
        target_layout,
        place,
        body,
        value_map,
        local_vars,
        is,
    )?;
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
    if is_erased_runtime_ty(db, target_layout, value_ty) {
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
            let dst_elem = extend_place(dst_place, Projection::Index(IndexSource::Constant(idx)));
            let src_elem = extend_place(src_place, Projection::Index(IndexSource::Constant(idx)));
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
        return Err(LowerError::Unsupported(
            "enum store requires enum adt".into(),
        ));
    };
    let AdtRef::Enum(enm) = adt_def.adt_ref(db) else {
        return Err(LowerError::Unsupported(
            "enum store requires enum adt".into(),
        ));
    };

    // Copy discriminant first.
    let discr_ty =
        hir::analysis::ty::ty_def::TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
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
    let mut cases: Vec<(ValueId, BlockId)> = Vec::with_capacity(variants.len());
    let mut case_blocks = Vec::with_capacity(variants.len());
    for (idx, _) in variants.iter().enumerate() {
        let case_block = fb.append_block();
        case_blocks.push(case_block);
        cases.push((fb.make_imm_value(I256::from(idx as u64)), case_block));
    }

    fb.switch_to_block(origin_block);
    if cases.is_empty() {
        fb.insert_inst_no_result(Jump::new(is, cont_block));
    } else {
        let mut compare_blocks = Vec::with_capacity(cases.len().saturating_sub(1));
        for _ in 0..cases.len().saturating_sub(1) {
            compare_blocks.push(fb.append_block());
        }

        for (case_idx, (case_value, case_dest)) in cases.into_iter().enumerate() {
            if case_idx > 0 {
                fb.switch_to_block(compare_blocks[case_idx - 1]);
            }

            let else_dest = if case_idx + 1 < compare_blocks.len() + 1 {
                compare_blocks[case_idx]
            } else {
                cont_block
            };
            let cond = fb.insert_inst(Eq::new(is, discr, case_value), Type::I256);
            fb.insert_inst_no_result(Br::new(is, cond, case_dest, else_dest));
        }
    }

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
