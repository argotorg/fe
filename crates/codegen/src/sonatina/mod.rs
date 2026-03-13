//! Sonatina backend for direct EVM bytecode generation.
//!
//! This module translates Fe MIR to Sonatina IR, which is then compiled
//! to EVM bytecode without going through Yul/solc.

mod lower;
mod tests;
mod types;

use crate::{BackendError, OptLevel};
use common::ingot::Ingot;
use driver::DriverDataBase;
use hir::analysis::ty::ty_def::TyId;
use hir::hir_def::TopLevelMod;
use mir::ir::PointerInfo;
use mir::{CoreLib, MirModule, layout, layout::TargetDataLayout, lower_ingot, lower_module};
use rustc_hash::{FxHashMap, FxHashSet};
use sonatina_ir::{
    BlockId, GlobalVariableRef, Module, Signature, Type, ValueId,
    builder::{ModuleBuilder, Variable},
    func_cursor::InstInserter,
    inst::{control_flow::Call, evm::EvmStop},
    ir_writer::ModuleWriter,
    isa::{Isa, evm::Evm},
    module::{FuncRef, ModuleCtx},
    object::{Directive, Embed, EmbedSymbol, Object, ObjectName, Section, SectionName, SectionRef},
};
use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};
use std::collections::BTreeMap;
pub use tests::{DebugOutputSink, SonatinaTestDebugConfig, emit_test_module_sonatina};

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
pub(crate) fn create_evm_isa() -> Evm {
    let triple = TargetTriple::new(
        Architecture::Evm,
        Vendor::Ethereum,
        OperatingSystem::Evm(EvmVersion::Osaka),
    );
    Evm::new(triple)
}

/// Contract bytecode output produced by the Sonatina backend.
#[derive(Debug, Clone)]
pub struct SonatinaContractBytecode {
    pub deploy: Vec<u8>,
    pub runtime: Vec<u8>,
}

pub(super) fn is_erased_runtime_ty(
    db: &DriverDataBase,
    target_layout: &TargetDataLayout,
    ty: hir::analysis::ty::ty_def::TyId<'_>,
) -> bool {
    layout::ty_size_bytes_in(db, target_layout, ty).is_some_and(|s| s == 0)
}

#[derive(Debug, Clone)]
pub(super) struct RuntimeFunctionMetadata {
    pub(super) params: Vec<Type>,
    pub(super) ret: Option<Type>,
}

impl RuntimeFunctionMetadata {
    fn signature(&self, name: &str, linkage: sonatina_ir::Linkage) -> Signature {
        let ret_tys: Vec<_> = self.ret.into_iter().collect();
        Signature::new(name, linkage, &self.params, &ret_tys)
    }
}

fn runtime_pointer_type_from_target(
    builder: &ModuleBuilder,
    db: &DriverDataBase,
    core: &CoreLib<'_>,
    target_layout: &TargetDataLayout,
    target_ty: TyId<'_>,
    cache: &mut FxHashMap<String, Option<Type>>,
    name_counter: &mut usize,
) -> Type {
    let pointee = lower::fe_ty_to_sonatina(
        builder,
        db,
        core,
        target_layout,
        target_ty,
        cache,
        name_counter,
    )
    .unwrap_or(Type::I8);
    if pointee == Type::Unit {
        return builder.ptr_type(Type::I8);
    }
    builder.ptr_type(pointee)
}

fn function_core_lib<'db>(db: &'db DriverDataBase, func: &mir::MirFunction<'db>) -> CoreLib<'db> {
    let scope = match func.origin {
        mir::ir::MirFunctionOrigin::Hir(hir_func) => hir_func.scope(),
        mir::ir::MirFunctionOrigin::Synthetic(synth) => synth.contract().scope(),
    };
    CoreLib::new(db, scope)
}

pub(super) fn runtime_type_for_pointer_info(
    builder: &ModuleBuilder,
    db: &DriverDataBase,
    core: &CoreLib<'_>,
    target_layout: &TargetDataLayout,
    info: PointerInfo<'_>,
    cache: &mut FxHashMap<String, Option<Type>>,
    name_counter: &mut usize,
) -> Type {
    runtime_type_for_shape(
        builder,
        db,
        core,
        target_layout,
        mir::repr::runtime_shape_for_pointer_info(info),
        cache,
        name_counter,
    )
}

pub(super) fn runtime_type_for_shape(
    builder: &ModuleBuilder,
    db: &DriverDataBase,
    core: &CoreLib<'_>,
    target_layout: &TargetDataLayout,
    shape: mir::ir::RuntimeShape<'_>,
    cache: &mut FxHashMap<String, Option<Type>>,
    name_counter: &mut usize,
) -> Type {
    match shape {
        mir::ir::RuntimeShape::Unresolved => {
            panic!("unresolved MIR runtime shape reached Sonatina codegen")
        }
        mir::ir::RuntimeShape::Erased => Type::Unit,
        mir::ir::RuntimeShape::Word(kind) => types::runtime_word_type(kind),
        mir::ir::RuntimeShape::MemoryPtr { target_ty } => target_ty
            .map(|target_ty| {
                runtime_pointer_type_from_target(
                    builder,
                    db,
                    core,
                    target_layout,
                    target_ty,
                    cache,
                    name_counter,
                )
            })
            .unwrap_or_else(|| builder.ptr_type(Type::I8)),
        mir::ir::RuntimeShape::AddressWord(_) => Type::I256,
    }
}

pub(super) fn zero_value_for_type<C: sonatina_ir::func_cursor::FuncCursor>(
    fb: &mut sonatina_ir::builder::FunctionBuilder<C>,
    ty: Type,
    is: &sonatina_ir::inst::evm::inst_set::EvmInstSet,
) -> ValueId {
    if ty == Type::Unit {
        return types::zero_value(fb, ty);
    }
    let zero = types::zero_value(fb, Type::I256);
    if ty.is_pointer(&fb.module_builder.ctx) {
        return fb.insert_inst(sonatina_ir::inst::cast::IntToPtr::new(is, zero, ty), ty);
    }
    types::zero_value(fb, ty)
}

#[derive(Debug, Clone)]
enum ContractObjectSelection {
    PrimaryRootAndDeps,
    RootAndDeps(String),
    All,
}

/// Compiles a Fe module to Sonatina IR.
pub fn compile_module(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    layout: TargetDataLayout,
) -> Result<Module, LowerError> {
    // Lower HIR to MIR
    let mir_module = lower_module(db, top_mod)?;
    compile_mir_module(
        db,
        &mir_module,
        layout,
        ContractObjectSelection::PrimaryRootAndDeps,
    )
}

fn compile_mir_module<'db>(
    db: &'db DriverDataBase,
    mir_module: &MirModule<'db>,
    layout: TargetDataLayout,
    selection: ContractObjectSelection,
) -> Result<Module, LowerError> {
    // Create Sonatina module
    let isa = create_evm_isa();
    let ctx = ModuleCtx::new(&isa);
    let module_builder = ModuleBuilder::new(ctx);

    // Create lowerer and process module
    let mut lowerer = ModuleLowerer::new(db, module_builder, mir_module, &isa, layout, selection);
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

/// Compiles a Fe module to optimized Sonatina IR and returns the human-readable text representation.
///
/// Optimization level and contract selection match [`emit_module_sonatina_bytecode`].
pub fn emit_module_sonatina_ir_optimized(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    opt_level: OptLevel,
    contract: Option<&str>,
) -> Result<String, LowerError> {
    let mir_module = lower_module(db, top_mod)?;
    emit_mir_module_sonatina_ir_optimized(db, &mir_module, opt_level, contract)
}

/// Compiles a Fe module to Sonatina IR and returns a validation report.
///
/// This is intended for debugging malformed IR: it checks that every `ValueId` used as an operand
/// refers to a defining instruction that is still present in the function layout.
pub fn validate_module_sonatina_ir(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
) -> Result<String, LowerError> {
    let layout = layout::EVM_LAYOUT;
    let module = compile_module(db, top_mod, layout)?;
    Ok(validate_sonatina_module_layout(&module))
}

/// Compiles a Fe module to EVM bytecode using the Sonatina backend.
///
/// When `contract` is `Some`, only that contract is compiled (along with any transitive contract
/// dependencies needed to resolve `code_region_offset/len`). When `None`, all contracts are
/// compiled.
pub fn emit_module_sonatina_bytecode(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    opt_level: OptLevel,
    contract: Option<&str>,
) -> Result<BTreeMap<String, SonatinaContractBytecode>, LowerError> {
    let mir_module = lower_module(db, top_mod)?;
    emit_mir_module_sonatina_bytecode(db, &mir_module, opt_level, contract)
}

/// Compiles an entire ingot (all source files) to EVM bytecode using the Sonatina backend.
///
/// See [`emit_module_sonatina_bytecode`] for contract selection semantics.
pub fn emit_ingot_sonatina_bytecode(
    db: &DriverDataBase,
    ingot: Ingot<'_>,
    opt_level: OptLevel,
    contract: Option<&str>,
) -> Result<BTreeMap<String, SonatinaContractBytecode>, LowerError> {
    let mir_module = lower_ingot(db, ingot)?;
    emit_mir_module_sonatina_bytecode(db, &mir_module, opt_level, contract)
}

fn emit_mir_module_sonatina_bytecode<'db>(
    db: &'db DriverDataBase,
    mir_module: &MirModule<'db>,
    opt_level: OptLevel,
    contract: Option<&str>,
) -> Result<BTreeMap<String, SonatinaContractBytecode>, LowerError> {
    use sonatina_codegen::isa::evm::EvmBackend;
    use sonatina_codegen::object::{CompileOptions, compile_object};

    let (module, target_contracts) =
        compile_mir_module_for_sonatina_output(db, mir_module, opt_level, contract)?;

    let isa = create_evm_isa();
    let backend = EvmBackend::new(isa);
    let opts: CompileOptions<_> = CompileOptions::default();

    let init_section_name = SectionName::from("init");
    let runtime_section_name = SectionName::from("runtime");

    let mut out = BTreeMap::new();
    for contract_name in target_contracts {
        let artifact =
            compile_object(&module, &backend, &contract_name, &opts).map_err(|errors| {
                let msg = errors
                    .iter()
                    .map(|e| format!("{:?}", e))
                    .collect::<Vec<_>>()
                    .join("; ");
                LowerError::Internal(msg)
            })?;

        let deploy = artifact
            .sections
            .get(&init_section_name)
            .ok_or_else(|| {
                LowerError::Internal(format!(
                    "compiled object `{contract_name}` has no init section"
                ))
            })?
            .bytes
            .clone();

        let runtime = artifact
            .sections
            .get(&runtime_section_name)
            .ok_or_else(|| {
                LowerError::Internal(format!(
                    "compiled object `{contract_name}` has no runtime section"
                ))
            })?
            .bytes
            .clone();

        out.insert(contract_name, SonatinaContractBytecode { deploy, runtime });
    }

    Ok(out)
}

/// Compiles a lowered MIR module to optimized Sonatina IR.
pub fn emit_mir_module_sonatina_ir_optimized<'db>(
    db: &'db DriverDataBase,
    mir_module: &MirModule<'db>,
    opt_level: OptLevel,
    contract: Option<&str>,
) -> Result<String, LowerError> {
    let (module, _) = compile_mir_module_for_sonatina_output(db, mir_module, opt_level, contract)?;
    let mut writer = ModuleWriter::new(&module);
    Ok(writer.dump_string())
}

fn compile_mir_module_for_sonatina_output<'db>(
    db: &'db DriverDataBase,
    mir_module: &MirModule<'db>,
    opt_level: OptLevel,
    contract: Option<&str>,
) -> Result<(Module, Vec<String>), LowerError> {
    use mir::analysis::build_contract_graph;

    let contract_graph = build_contract_graph(db, &mir_module.functions);
    let mut contract_names: Vec<String> = contract_graph.contracts.keys().cloned().collect();
    contract_names.sort();

    if let Some(contract) = contract
        && !contract_graph.contracts.contains_key(contract)
    {
        return Err(LowerError::Internal(format!(
            "contract `{contract}` not found"
        )));
    }

    let target_contracts: Vec<String> = match contract {
        Some(name) => vec![name.to_string()],
        None => contract_names,
    };

    let selection = match contract {
        Some(name) => ContractObjectSelection::RootAndDeps(name.to_string()),
        None => ContractObjectSelection::All,
    };

    let mut module = compile_mir_module(db, mir_module, layout::EVM_LAYOUT, selection)?;
    ensure_module_sonatina_ir_valid(&module)?;

    match opt_level {
        OptLevel::O0 => {}
        OptLevel::Os => sonatina_codegen::optim::Pipeline::size().run(&mut module),
        OptLevel::O2 => sonatina_codegen::optim::Pipeline::speed().run(&mut module),
    }
    if opt_level != OptLevel::O0 {
        ensure_module_sonatina_ir_valid(&module)?;
    }

    Ok((module, target_contracts))
}

pub(crate) fn ensure_module_sonatina_ir_valid(module: &Module) -> Result<(), LowerError> {
    let report = validate_sonatina_module_layout(module);
    if report.trim() == "ok" {
        return Ok(());
    }

    Err(LowerError::Internal(format!(
        "invalid Sonatina IR emitted by Fe:\n{report}"
    )))
}

fn validate_sonatina_module_layout(module: &Module) -> String {
    use std::fmt::Write as _;

    let mut out = String::new();
    let mut any = false;

    for func in module.funcs() {
        let name = module.ctx.func_sig(func, |sig| sig.name().to_string());

        let (dangling, total) = module.func_store.view(func, |function| {
            use sonatina_ir::ir_writer::{FuncWriteCtx, IrWrite as _};

            let write_inst = |inst: sonatina_ir::InstId| -> String {
                let ctx = FuncWriteCtx::new(function, func);
                let mut buf = Vec::new();
                let _ = inst.write(&mut buf, &ctx);
                String::from_utf8_lossy(&buf).into_owned()
            };

            let mut total_operands: usize = 0;
            let mut dangling_operands: Vec<(
                ValueId,
                sonatina_ir::InstId,
                String,
                sonatina_ir::InstId,
                String,
            )> = Vec::new();

            for block in function.layout.iter_block() {
                for inst in function.layout.iter_inst(block) {
                    let user_dbg = write_inst(inst);
                    function.dfg.inst(inst).for_each_value(&mut |operand| {
                        total_operands += 1;
                        let sonatina_ir::Value::Inst { inst: def_inst, .. } =
                            function.dfg.value(operand)
                        else {
                            return;
                        };
                        if !function.layout.is_inst_inserted(*def_inst) {
                            let def_dbg = write_inst(*def_inst);
                            dangling_operands.push((
                                operand,
                                *def_inst,
                                def_dbg,
                                inst,
                                user_dbg.clone(),
                            ));
                        }
                    });
                }
            }

            (dangling_operands, total_operands)
        });

        if dangling.is_empty() {
            continue;
        }
        any = true;
        let _ = writeln!(
            &mut out,
            "=== {name} ===\ninvalid_operands: {}/{}\n",
            dangling.len(),
            total
        );
        for (operand, def_inst, def_dbg, user_inst, user_dbg) in dangling {
            let _ = writeln!(
                &mut out,
                "- operand={operand:?} def_inst={def_inst:?} def={def_dbg} used_by={user_inst:?} user={user_dbg}"
            );
        }
        out.push('\n');
    }

    if !any {
        out.push_str("ok\n");
    }
    out
}

/// Lowers an entire MIR module to Sonatina IR.
struct ModuleLowerer<'db, 'a> {
    db: &'db DriverDataBase,
    builder: ModuleBuilder,
    mir: &'a MirModule<'db>,
    isa: &'a Evm,
    target_layout: TargetDataLayout,
    contract_selection: ContractObjectSelection,
    /// Maps function indices to Sonatina function references.
    func_map: FxHashMap<usize, FuncRef>,
    /// Maps function symbol names to Sonatina function references.
    name_map: FxHashMap<String, FuncRef>,
    /// Runtime Sonatina signature metadata keyed by lowered symbol name.
    runtime_function_metadata: FxHashMap<String, RuntimeFunctionMetadata>,
    /// Indices of functions executed directly by the EVM (empty stack).
    ///
    /// These entry functions emit `evm_stop` instead of internal `Return`.
    entry_func_idxs: FxHashSet<usize>,
    /// Cache for Fe type → sonatina type mapping (GEP support).
    gep_type_cache: FxHashMap<String, Option<Type>>,
    /// Counter for generating unique sonatina struct type names.
    gep_name_counter: usize,
    /// Global variables registered for constant aggregate data sections.
    data_globals: Vec<GlobalVariableRef>,
    /// Data globals keyed by function symbol that introduced them.
    ///
    /// Used to scope section directives to region-reachable functions only.
    data_globals_by_symbol: FxHashMap<String, Vec<GlobalVariableRef>>,
    /// Counter for generating unique data global names.
    data_global_counter: usize,
    /// Interprocedural pointer escape summaries keyed by lowered symbol name.
    ptr_escape_summaries: mir::analysis::escape::MirPtrEscapeSummaryMap,
}

impl<'db, 'a> ModuleLowerer<'db, 'a> {
    fn new(
        db: &'db DriverDataBase,
        builder: ModuleBuilder,
        mir: &'a MirModule<'db>,
        isa: &'a Evm,
        target_layout: TargetDataLayout,
        contract_selection: ContractObjectSelection,
    ) -> Self {
        Self {
            db,
            builder,
            mir,
            isa,
            target_layout,
            contract_selection,
            func_map: FxHashMap::default(),
            name_map: FxHashMap::default(),
            runtime_function_metadata: FxHashMap::default(),
            entry_func_idxs: FxHashSet::default(),
            gep_type_cache: FxHashMap::default(),
            gep_name_counter: 0,
            data_globals: Vec::new(),
            data_globals_by_symbol: FxHashMap::default(),
            data_global_counter: 0,
            ptr_escape_summaries: mir::analysis::escape::compute_ptr_escape_summaries(db, mir),
        }
    }

    /// Lower the entire module.
    fn lower(&mut self) -> Result<(), LowerError> {
        // First pass: declare runtime-relevant functions.
        self.declare_functions()?;

        // Identify entry functions so lowering can emit evm_stop for entries.
        self.identify_entry_functions()?;

        // Second pass: lower function bodies (populates data_globals).
        self.lower_functions()?;

        // Third pass: create objects with data directives (needs data_globals).
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
            if func.symbol_name.is_empty() {
                continue;
            }
            let name = &func.symbol_name;
            let (sig, metadata) = self.lower_signature_and_metadata(func)?;

            let func_ref = self.builder.declare_function(sig).map_err(|e| {
                LowerError::Internal(format!("failed to declare function {name}: {e}"))
            })?;

            self.func_map.insert(idx, func_ref);
            self.name_map.insert(name.clone(), func_ref);
            self.runtime_function_metadata
                .insert(name.clone(), metadata);
        }
        Ok(())
    }

    /// Lower function signatures.
    fn lower_signature_and_metadata(
        &mut self,
        func: &mir::MirFunction<'db>,
    ) -> Result<(Signature, RuntimeFunctionMetadata), LowerError> {
        let name = &func.symbol_name;
        // Keep lowered functions private so Sonatina DFE can eliminate dead functions.
        // Reachability roots are selected via object section entries, not linkage visibility.
        let linkage = sonatina_ir::Linkage::Private;
        let core = function_core_lib(self.db, func);
        let runtime_param_locals: Vec<_> = func
            .runtime_param_locals()
            .into_iter()
            .chain(func.runtime_effect_param_locals())
            .collect();

        let mut params = Vec::with_capacity(runtime_param_locals.len());
        for local_id in runtime_param_locals {
            let local = func.body.locals.get(local_id.index()).ok_or_else(|| {
                LowerError::Internal(format!("unknown param local: {local_id:?}"))
            })?;
            params.push(runtime_type_for_shape(
                &self.builder,
                self.db,
                &core,
                &self.target_layout,
                local.runtime_shape,
                &mut self.gep_type_cache,
                &mut self.gep_name_counter,
            ));
        }

        let ret = (!func.runtime_return_shape.is_erased()).then(|| {
            runtime_type_for_shape(
                &self.builder,
                self.db,
                &core,
                &self.target_layout,
                func.runtime_return_shape,
                &mut self.gep_type_cache,
                &mut self.gep_name_counter,
            )
        });

        let metadata = RuntimeFunctionMetadata { params, ret };
        Ok((metadata.signature(name, linkage), metadata))
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

    /// Compute which contracts are needed for compilation: the primary contract
    /// and its transitive dependencies. Returns `(primary_name, needed_set)`.
    fn needed_contracts(
        &self,
        contract_graph: &mir::analysis::ContractGraph,
    ) -> Result<(String, FxHashSet<String>), LowerError> {
        use mir::analysis::{ContractRegion, ContractRegionKind};
        use std::collections::VecDeque;

        let select_primary = || -> Result<String, LowerError> {
            let mut referenced: FxHashSet<String> = FxHashSet::default();
            for (from_region, deps) in &contract_graph.region_deps {
                for dep in deps {
                    if dep.contract_name != from_region.contract_name {
                        referenced.insert(dep.contract_name.clone());
                    }
                }
            }
            let mut roots: Vec<String> = contract_graph
                .contracts
                .keys()
                .filter(|n| !referenced.contains(*n))
                .cloned()
                .collect();
            roots.sort();
            roots
                .into_iter()
                .next()
                .or_else(|| {
                    let mut names: Vec<String> = contract_graph.contracts.keys().cloned().collect();
                    names.sort();
                    names.into_iter().next()
                })
                .ok_or_else(|| {
                    LowerError::Internal("contract graph is unexpectedly empty".to_string())
                })
        };

        let primary = match &self.contract_selection {
            ContractObjectSelection::PrimaryRootAndDeps => select_primary()?,
            ContractObjectSelection::RootAndDeps(root) => {
                if !contract_graph.contracts.contains_key(root.as_str()) {
                    return Err(LowerError::Internal(format!("unknown contract `{root}`")));
                }
                root.clone()
            }
            ContractObjectSelection::All => select_primary()?,
        };

        let needed: FxHashSet<String> = match &self.contract_selection {
            ContractObjectSelection::All => contract_graph.contracts.keys().cloned().collect(),
            _ => {
                let mut needed = FxHashSet::default();
                let mut queue = VecDeque::new();
                queue.push_back(primary.clone());
                while let Some(name) = queue.pop_front() {
                    if !needed.insert(name.clone()) {
                        continue;
                    }
                    for kind in [ContractRegionKind::Init, ContractRegionKind::Deployed] {
                        let region = ContractRegion {
                            contract_name: name.clone(),
                            kind,
                        };
                        if let Some(deps) = contract_graph.region_deps.get(&region) {
                            for dep in deps {
                                if dep.contract_name != name {
                                    queue.push_back(dep.contract_name.clone());
                                }
                            }
                        }
                    }
                }
                needed
            }
        };

        Ok((primary, needed))
    }

    /// Identify which functions are contract entry points so their Return
    /// terminators can be lowered as `evm_stop`. Must run before `lower_functions`.
    fn identify_entry_functions(&mut self) -> Result<(), LowerError> {
        use mir::analysis::build_contract_graph;

        let contract_graph = build_contract_graph(self.db, &self.mir.functions);
        if contract_graph.contracts.is_empty() {
            return Ok(());
        }

        let (_primary, needed_contracts) = self.needed_contracts(&contract_graph)?;

        let mut func_idx_by_symbol: FxHashMap<&str, usize> = FxHashMap::default();
        for (idx, func) in self.mir.functions.iter().enumerate() {
            if self.func_map.contains_key(&idx) {
                func_idx_by_symbol.insert(func.symbol_name.as_str(), idx);
            }
        }

        for (contract_name, info) in &contract_graph.contracts {
            if !needed_contracts.contains(contract_name) {
                continue;
            }
            if let Some(symbol) = info.deployed_symbol.as_deref()
                && let Some(&idx) = func_idx_by_symbol.get(symbol)
            {
                self.entry_func_idxs.insert(idx);
            }
            if let Some(symbol) = info.init_symbol.as_deref()
                && let Some(&idx) = func_idx_by_symbol.get(symbol)
            {
                self.entry_func_idxs.insert(idx);
            }
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

        let contract_graph = build_contract_graph(self.db, &self.mir.functions);
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
        let mut directives = vec![Directive::Entry(wrapper_ref)];
        for &gv in &self.data_globals {
            directives.push(Directive::Data(gv));
        }

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

        let (primary_contract, needed_contracts) = self.needed_contracts(contract_graph)?;
        let mut needed_contracts = needed_contracts;

        // Assign stable object names for each needed contract.
        let mut contract_object_names: FxHashMap<String, ObjectName> = FxHashMap::default();
        let mut ordered_contracts: Vec<String> = needed_contracts.drain().collect();
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
                for gv in self.reachable_data_globals_for_region(contract_graph, &region) {
                    directives.push(Directive::Data(gv));
                }
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
                for gv in self.reachable_data_globals_for_region(contract_graph, &region) {
                    directives.push(Directive::Data(gv));
                }
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

    /// Collects constant-data globals used by functions reachable from `region`.
    fn reachable_data_globals_for_region(
        &self,
        contract_graph: &mir::analysis::ContractGraph,
        region: &mir::analysis::ContractRegion,
    ) -> Vec<GlobalVariableRef> {
        let Some(reachable_symbols) = contract_graph.region_reachable.get(region) else {
            return Vec::new();
        };

        let mut symbols: Vec<_> = reachable_symbols.iter().cloned().collect();
        symbols.sort();

        let mut globals = Vec::new();
        let mut seen = FxHashSet::default();
        for symbol in symbols {
            if let Some(symbol_globals) = self.data_globals_by_symbol.get(&symbol) {
                for &gv in symbol_globals {
                    if seen.insert(gv) {
                        globals.push(gv);
                    }
                }
            }
        }
        globals
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

        let sig = Signature::new(WRAPPER_NAME, sonatina_ir::Linkage::Private, &[], &[]);
        let func_ref = self.builder.declare_function(sig).map_err(|e| {
            LowerError::Internal(format!(
                "failed to declare entry wrapper `{WRAPPER_NAME}`: {e}"
            ))
        })?;

        let mut fb = self.builder.func_builder::<InstInserter>(func_ref);
        let is = self.isa.inst_set();

        let entry_block = fb.append_block();
        fb.switch_to_block(entry_block);

        let runtime_metadata = self
            .runtime_function_metadata
            .get(&entry_mir_func.symbol_name)
            .ok_or_else(|| {
                LowerError::Internal(format!(
                    "missing runtime type metadata for `{}`",
                    entry_mir_func.symbol_name
                ))
            })?;
        // Pass zero for all arguments (regular + effect params). Entry wrappers do not supply
        // source-level arguments, but the internal call still needs type-correct placeholders.
        let mut args = Vec::with_capacity(runtime_metadata.params.len());
        for expected_ty in runtime_metadata.params.iter().copied() {
            args.push(zero_value_for_type(&mut fb, expected_ty, is));
        }
        let call_inst = Call::new(is, entry_ref, args.into());
        if let Some(ret_ty) = runtime_metadata.ret {
            let _ = fb.insert_inst(call_inst, ret_ty);
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
        &mut self,
        func_ref: FuncRef,
        func: &mir::MirFunction<'db>,
        is_entry: bool,
    ) -> Result<(), LowerError> {
        let data_globals_before = self.data_globals.len();
        let func_metadata = self
            .runtime_function_metadata
            .get(&func.symbol_name)
            .cloned()
            .ok_or_else(|| {
                LowerError::Internal(format!(
                    "missing runtime type metadata for `{}`",
                    func.symbol_name
                ))
            })?;

        let mut fb = self.builder.func_builder::<InstInserter>(func_ref);
        let is = self.isa.inst_set();
        let core = function_core_lib(self.db, func);
        let local_runtime_types: Vec<_> = func
            .body
            .locals
            .iter()
            .map(|local| {
                runtime_type_for_shape(
                    &self.builder,
                    self.db,
                    &core,
                    &self.target_layout,
                    local.runtime_shape,
                    &mut self.gep_type_cache,
                    &mut self.gep_name_counter,
                )
            })
            .collect();

        // Maps MIR block IDs to Sonatina block IDs
        let mut block_map: FxHashMap<mir::BasicBlockId, BlockId> = FxHashMap::default();

        // Maps MIR local IDs to Sonatina SSA variables.
        let mut local_vars: FxHashMap<mir::LocalId, Variable> = FxHashMap::default();
        for (idx, _local) in func.body.locals.iter().enumerate() {
            let local_id = mir::LocalId(idx as u32);
            let var = fb.declare_var(local_runtime_types[idx]);
            local_vars.insert(local_id, var);
        }
        let mut local_place_roots = FxHashMap::default();
        let mut initialized_locals = FxHashSet::default();

        // Create blocks
        for (idx, _block) in func.body.blocks.iter().enumerate() {
            let block_id = mir::BasicBlockId(idx as u32);
            let sonatina_block = fb.append_block();
            block_map.insert(block_id, sonatina_block);
        }

        // Get the entry block and its Sonatina equivalent.
        // Sonatina's Layout::append_block sets `entry_block` to the first block appended,
        // so the iteration order above (MIR block 0 first) guarantees the entry is correct.
        let entry_block = func.body.entry;
        let sonatina_entry = block_map[&entry_block];

        // Map function arguments to parameter locals (regular params + effect params).
        fb.switch_to_block(sonatina_entry);
        let all_param_locals: Vec<_> = func
            .body
            .param_locals
            .iter()
            .chain(func.body.effect_param_locals.iter())
            .copied()
            .collect();
        let runtime_param_locals: FxHashSet<_> = func
            .runtime_param_locals()
            .into_iter()
            .chain(func.runtime_effect_param_locals())
            .collect();

        if is_entry {
            // Entry functions have no Sonatina parameters (EVM starts with empty stack).
            // Initialize all param locals to zero - effect params are erased at runtime
            // and regular params shouldn't exist for entry functions.
            for local_id in all_param_locals.iter().copied() {
                let var = local_vars.get(&local_id).copied().ok_or_else(|| {
                    LowerError::Internal(format!(
                        "missing SSA variable for param local {local_id:?}"
                    ))
                })?;
                let zero = zero_value_for_type(&mut fb, local_runtime_types[local_id.index()], is);
                fb.def_var(var, zero);
            }
        } else {
            // Non-entry functions: map actual arguments to param locals.
            let args = fb.args().to_vec();
            let mut arg_iter = args.into_iter();
            for local_id in all_param_locals.iter().copied() {
                let var = local_vars.get(&local_id).copied().ok_or_else(|| {
                    LowerError::Internal(format!(
                        "missing SSA variable for param local {local_id:?}"
                    ))
                })?;

                let arg_val = if runtime_param_locals.contains(&local_id) {
                    arg_iter.next().unwrap_or_else(|| {
                        zero_value_for_type(&mut fb, local_runtime_types[local_id.index()], is)
                    })
                } else {
                    zero_value_for_type(&mut fb, local_runtime_types[local_id.index()], is)
                };
                fb.def_var(var, arg_val);
            }
        }
        initialized_locals.extend(all_param_locals.iter().copied());

        {
            let mut const_data_globals = FxHashMap::default();
            let mut overflow_revert_block = None;
            let mut code_load_scratch = None;
            let ptr_escape_summary = self.ptr_escape_summaries.get(&func.symbol_name);
            let mut ctx = LowerCtx {
                fb: &mut fb,
                db: self.db,
                core: &core,
                target_layout: &self.target_layout,
                body: &func.body,
                local_vars: &local_vars,
                local_place_roots: &mut local_place_roots,
                initialized_locals: &mut initialized_locals,
                name_map: &self.name_map,
                runtime_function_metadata: &self.runtime_function_metadata,
                current_function_metadata: &func_metadata,
                local_runtime_types: &local_runtime_types,
                block_map: &block_map,
                is,
                is_entry,
                gep_type_cache: &mut self.gep_type_cache,
                gep_name_counter: &mut self.gep_name_counter,
                data_globals: &mut self.data_globals,
                data_global_counter: &mut self.data_global_counter,
                const_data_globals: &mut const_data_globals,
                overflow_revert_block: &mut overflow_revert_block,
                code_load_scratch: &mut code_load_scratch,
                ptr_escape_summary,
            };
            for (idx, block) in ctx.body.blocks.iter().enumerate() {
                let block_id = mir::BasicBlockId(idx as u32);
                let sonatina_block = ctx.block_map[&block_id];
                ctx.fb.switch_to_block(sonatina_block);

                for inst in block.insts.iter() {
                    lower::lower_instruction(&mut ctx, inst)?;
                }

                lower::lower_terminator(&mut ctx, &block.terminator)?;
            }

            ctx.fb.seal_all();
        }
        fb.finish();

        if self.data_globals.len() > data_globals_before {
            self.data_globals_by_symbol.insert(
                func.symbol_name.clone(),
                self.data_globals[data_globals_before..].to_vec(),
            );
        }

        Ok(())
    }
}
/// Shared context threaded through all lowering functions.
pub(super) struct LowerCtx<'a, 'db, C: sonatina_ir::func_cursor::FuncCursor> {
    pub(super) fb: &'a mut sonatina_ir::builder::FunctionBuilder<C>,
    pub(super) db: &'db DriverDataBase,
    pub(super) core: &'a CoreLib<'db>,
    pub(super) target_layout: &'a TargetDataLayout,
    pub(super) body: &'a mir::MirBody<'db>,
    pub(super) local_vars: &'a FxHashMap<mir::LocalId, Variable>,
    pub(super) local_place_roots: &'a mut FxHashMap<mir::LocalId, ValueId>,
    pub(super) initialized_locals: &'a mut FxHashSet<mir::LocalId>,
    pub(super) name_map: &'a FxHashMap<String, FuncRef>,
    pub(super) runtime_function_metadata: &'a FxHashMap<String, RuntimeFunctionMetadata>,
    pub(super) current_function_metadata: &'a RuntimeFunctionMetadata,
    pub(super) local_runtime_types: &'a [Type],
    pub(super) block_map: &'a FxHashMap<mir::BasicBlockId, BlockId>,
    pub(super) is: &'a sonatina_ir::inst::evm::inst_set::EvmInstSet,
    pub(super) is_entry: bool,
    /// Cache for Fe type → sonatina type mapping (GEP support).
    pub(super) gep_type_cache: &'a mut FxHashMap<String, Option<Type>>,
    /// Counter for generating unique sonatina struct type names.
    pub(super) gep_name_counter: &'a mut usize,
    /// Collected global variable refs for constant aggregate data sections.
    pub(super) data_globals: &'a mut Vec<GlobalVariableRef>,
    /// Counter for generating unique data global names.
    pub(super) data_global_counter: &'a mut usize,
    /// Per-function dedupe for constant aggregate payloads.
    pub(super) const_data_globals: &'a mut FxHashMap<Vec<u8>, GlobalVariableRef>,
    /// Lazily-created shared overflow trap block for checked arithmetic in this function.
    pub(super) overflow_revert_block: &'a mut Option<BlockId>,
    /// Lazily-created shared scratch word used for immutable code reads in this function.
    pub(super) code_load_scratch: &'a mut Option<ValueId>,
    /// Escape summary for the current function.
    pub(super) ptr_escape_summary: Option<&'a mir::analysis::escape::MirPtrEscapeSummary>,
}
