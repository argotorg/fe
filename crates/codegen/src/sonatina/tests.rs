use driver::DriverDataBase;
use hir::hir_def::{ItemKind, TopLevelMod};
use mir::analysis::{CallGraph, build_call_graph, reachable_functions};
use mir::{
    MirFunction, MirInst, Rvalue,
    ir::{IntrinsicOp, MirFunctionOrigin},
    layout, lower_ingot,
};
use rustc_hash::{FxHashMap, FxHashSet};
use sonatina_codegen::{
    domtree::DomTree,
    isa::evm::EvmBackend,
    liveness::Liveness,
    machinst::lower::{LowerBackend, SectionLoweringCtx},
    object::{CompileOptions, SymbolId, compile_object},
    stackalloc::StackifyBuilder,
};
use sonatina_ir::{
    I256, Module, Signature, Type,
    builder::ModuleBuilder,
    cfg::ControlFlowGraph,
    func_cursor::InstInserter,
    inst::{control_flow::Call, evm::EvmStop},
    ir_writer::{FuncWriteCtx, FunctionSignature, IrWrite},
    isa::Isa,
    module::{FuncRef, ModuleCtx},
    object::{Directive, Embed, EmbedSymbol, Object, ObjectName, Section, SectionName, SectionRef},
};
use sonatina_verifier::{VerificationLevel, VerifierConfig};
use std::io::Write as _;

use crate::{ExpectedRevert, OptLevel, TestMetadata, TestModuleOutput};

use super::{ContractObjectSelection, LowerError, ModuleLowerer};

#[derive(Debug, Clone)]
pub struct SonatinaTestDebugConfig {
    pub symtab_output: Option<DebugOutputSink>,
    pub evm_debug_output: Option<DebugOutputSink>,
    pub emit_observability: bool,
    pub runtime_byte_offsets: Vec<usize>,
    pub stackify_reach_depth: u8,
}

impl Default for SonatinaTestDebugConfig {
    fn default() -> Self {
        Self {
            symtab_output: None,
            evm_debug_output: None,
            emit_observability: false,
            runtime_byte_offsets: Vec::new(),
            stackify_reach_depth: 16,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DebugOutputSink {
    pub path: Option<std::path::PathBuf>,
    pub write_stderr: bool,
}

impl Default for DebugOutputSink {
    fn default() -> Self {
        Self {
            path: None,
            write_stderr: true,
        }
    }
}

pub fn emit_test_module_sonatina(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    opt_level: OptLevel,
    debug: &SonatinaTestDebugConfig,
) -> Result<TestModuleOutput, LowerError> {
    let ingot = top_mod.ingot(db);
    let mir_module = lower_ingot(db, ingot)?;
    let tests = collect_tests(db, &mir_module.functions);

    if tests.is_empty() {
        return Ok(TestModuleOutput { tests: Vec::new() });
    }

    let call_graph = build_call_graph(&mir_module.functions);
    let funcs_by_symbol = build_funcs_by_symbol(&mir_module.functions);

    let code_region_roots = collect_code_region_roots(&mir_module.functions);
    let code_region_sections = code_region_roots
        .iter()
        .map(|sym| (sym.clone(), code_region_section_name(sym)))
        .collect::<FxHashMap<_, _>>();

    // Validate roots exist in the lowered module.
    for root in &code_region_roots {
        if !funcs_by_symbol.contains_key(root.as_str()) {
            return Err(LowerError::Internal(format!(
                "code region root `{root}` has no MIR function"
            )));
        }
    }

    // Precompute region reachability + region deps (for nested embeds).
    let mut region_reachable: FxHashMap<String, FxHashSet<String>> = FxHashMap::default();
    let mut region_deps: FxHashMap<String, FxHashSet<String>> = FxHashMap::default();
    for root in &code_region_roots {
        let reachable = reachable_functions(&call_graph, root);
        let deps = collect_code_region_deps(&reachable, &funcs_by_symbol);
        region_reachable.insert(root.clone(), reachable);
        region_deps.insert(root.clone(), deps);
    }

    // Detect cycles among region embeds early (Sonatina's embed mechanism expands bytes).
    detect_code_region_cycles(&region_deps)?;

    let mut module = compile_test_objects(
        db,
        &mir_module,
        &tests,
        &call_graph,
        &funcs_by_symbol,
        &code_region_roots,
        &code_region_sections,
        &region_reachable,
        &region_deps,
    )?;
    super::ensure_module_sonatina_ir_valid(&module)?;
    run_sonatina_optimization_pipeline(&mut module, opt_level);
    super::ensure_module_sonatina_ir_valid(&module)?;

    let mut output_tests = Vec::with_capacity(tests.len());
    for test in tests {
        let runtime = compile_runtime_section(&module, &test.object_name, debug)?;
        let init_bytecode = wrap_as_init_code(&runtime.bytes);

        output_tests.push(TestMetadata {
            display_name: test.display_name,
            hir_name: test.hir_name,
            symbol_name: test.symbol_name,
            object_name: test.object_name,
            yul: String::new(),
            bytecode: init_bytecode,
            sonatina_observability_text: runtime.observability_text,
            sonatina_observability_json: runtime.observability_json,
            value_param_count: test.value_param_count,
            effect_param_count: test.effect_param_count,
            expected_revert: test.expected_revert.clone(),
        });
    }

    Ok(TestModuleOutput {
        tests: output_tests,
    })
}

fn run_sonatina_optimization_pipeline(module: &mut Module, opt_level: OptLevel) {
    match opt_level {
        OptLevel::O0 => { /* no optimization */ }
        OptLevel::O1 => sonatina_codegen::optim::Pipeline::balanced().run(module),
        OptLevel::O2 => sonatina_codegen::optim::Pipeline::aggressive().run(module),
    }
}

#[derive(Debug, Clone)]
struct TestInfo {
    hir_name: String,
    display_name: String,
    symbol_name: String,
    object_name: String,
    value_param_count: usize,
    effect_param_count: usize,
    expected_revert: Option<ExpectedRevert>,
}

fn collect_tests(db: &DriverDataBase, functions: &[MirFunction<'_>]) -> Vec<TestInfo> {
    let mut tests: Vec<TestInfo> = functions
        .iter()
        .filter_map(|mir_func| {
            let MirFunctionOrigin::Hir(hir_func) = mir_func.origin else {
                return None;
            };
            let attrs = ItemKind::from(hir_func).attrs(db)?;
            let test_attr = attrs.get_attr(db, "test")?;

            let expected_revert = if test_attr.has_arg(db, "should_revert") {
                Some(ExpectedRevert::Any)
            } else {
                None
            };

            let hir_name = hir_func
                .name(db)
                .to_opt()
                .map(|n| n.data(db).to_string())
                .unwrap_or_else(|| "<anonymous>".to_string());
            let value_param_count = mir_func.body.param_locals.len();
            let effect_param_count = mir_func.body.effect_param_locals.len();
            Some(TestInfo {
                hir_name,
                display_name: String::new(),
                symbol_name: mir_func.symbol_name.clone(),
                object_name: String::new(),
                value_param_count,
                effect_param_count,
                expected_revert,
            })
        })
        .collect();

    assign_test_display_names(&mut tests);
    assign_test_object_names(&mut tests);
    tests
}

fn assign_test_display_names(tests: &mut [TestInfo]) {
    let mut name_counts: FxHashMap<String, usize> = FxHashMap::default();
    for test in tests.iter() {
        *name_counts.entry(test.hir_name.clone()).or_insert(0) += 1;
    }
    for test in tests.iter_mut() {
        let count = name_counts.get(&test.hir_name).copied().unwrap_or(0);
        if count > 1 {
            test.display_name = format!("{} [{}]", test.hir_name, test.symbol_name);
        } else {
            test.display_name = test.hir_name.clone();
        }
    }
}

fn assign_test_object_names(tests: &mut [TestInfo]) {
    let mut groups: FxHashMap<String, Vec<usize>> = FxHashMap::default();
    for (idx, test) in tests.iter().enumerate() {
        let base = format!("test_{}", sanitize_symbol(&test.display_name));
        groups.entry(base).or_default().push(idx);
    }
    for (base, mut indices) in groups {
        if indices.len() == 1 {
            let idx = indices[0];
            tests[idx].object_name = base;
            continue;
        }
        indices.sort_by(|a, b| tests[*a].display_name.cmp(&tests[*b].display_name));
        for (suffix, idx) in indices.into_iter().enumerate() {
            tests[idx].object_name = format!("{base}_{}", suffix + 1);
        }
    }
}

fn sanitize_symbol(component: &str) -> String {
    component
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}

fn build_funcs_by_symbol<'a>(
    functions: &'a [MirFunction<'a>],
) -> FxHashMap<&'a str, &'a MirFunction<'a>> {
    functions
        .iter()
        .map(|func| (func.symbol_name.as_str(), func))
        .collect()
}

fn runtime_argc(db: &DriverDataBase, func: &MirFunction<'_>) -> usize {
    func.body
        .param_locals
        .iter()
        .chain(func.body.effect_param_locals.iter())
        .copied()
        .filter(|local_id| {
            let local_ty = func.body.locals.get(local_id.index()).map(|l| l.ty);
            let Some(local_ty) = local_ty else {
                return true;
            };
            layout::ty_size_bytes_in(db, &layout::EVM_LAYOUT, local_ty).is_none_or(|s| s != 0)
        })
        .count()
}

fn collect_code_region_roots(functions: &[MirFunction<'_>]) -> Vec<String> {
    let mut roots = FxHashSet::default();
    for func in functions {
        if func.contract_function.is_some() {
            roots.insert(func.symbol_name.clone());
        }
        for block in &func.body.blocks {
            for inst in &block.insts {
                let MirInst::Assign {
                    rvalue:
                        Rvalue::Intrinsic {
                            op: IntrinsicOp::CodeRegionOffset | IntrinsicOp::CodeRegionLen,
                            args,
                        },
                    ..
                } = inst
                else {
                    continue;
                };
                let Some(arg) = args.first().copied() else {
                    continue;
                };
                let mir::ValueOrigin::FuncItem(target) = &func.body.value(arg).origin else {
                    continue;
                };
                let Some(symbol) = &target.symbol else {
                    continue;
                };
                roots.insert(symbol.clone());
            }
        }
    }
    let mut out: Vec<_> = roots.into_iter().collect();
    out.sort();
    out
}

fn collect_code_region_deps(
    reachable: &FxHashSet<String>,
    funcs_by_symbol: &FxHashMap<&str, &MirFunction<'_>>,
) -> FxHashSet<String> {
    let mut deps = FxHashSet::default();
    for symbol in reachable {
        let Some(func) = funcs_by_symbol.get(symbol.as_str()).copied() else {
            continue;
        };
        for block in &func.body.blocks {
            for inst in &block.insts {
                let MirInst::Assign {
                    rvalue:
                        Rvalue::Intrinsic {
                            op: IntrinsicOp::CodeRegionOffset | IntrinsicOp::CodeRegionLen,
                            args,
                        },
                    ..
                } = inst
                else {
                    continue;
                };
                let Some(arg) = args.first().copied() else {
                    continue;
                };
                let mir::ValueOrigin::FuncItem(target) = &func.body.value(arg).origin else {
                    continue;
                };
                let Some(target_symbol) = &target.symbol else {
                    continue;
                };
                deps.insert(target_symbol.clone());
            }
        }
    }
    deps
}

fn detect_code_region_cycles(
    graph: &FxHashMap<String, FxHashSet<String>>,
) -> Result<(), LowerError> {
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Mark {
        Visiting,
        Visited,
    }

    fn dfs(
        node: &str,
        graph: &FxHashMap<String, FxHashSet<String>>,
        marks: &mut FxHashMap<String, Mark>,
        stack: &mut Vec<String>,
    ) -> Result<(), LowerError> {
        match marks.get(node).copied() {
            Some(Mark::Visited) => return Ok(()),
            Some(Mark::Visiting) => {
                let start = stack.iter().position(|n| n == node).unwrap_or(0);
                let mut cycle = stack[start..].to_vec();
                cycle.push(node.to_string());
                return Err(LowerError::Unsupported(format!(
                    "cycle detected in code region graph: {}",
                    cycle.join(" -> ")
                )));
            }
            None => {}
        }

        marks.insert(node.to_string(), Mark::Visiting);
        stack.push(node.to_string());
        if let Some(deps) = graph.get(node) {
            let mut deps: Vec<String> = deps.iter().cloned().collect();
            deps.sort();
            for dep in deps {
                if dep == node {
                    continue;
                }
                dfs(&dep, graph, marks, stack)?;
            }
        }
        stack.pop();
        marks.insert(node.to_string(), Mark::Visited);
        Ok(())
    }

    let mut marks = FxHashMap::default();
    let mut stack = Vec::new();
    let mut nodes: Vec<String> = graph.keys().cloned().collect();
    nodes.sort();
    for node in nodes {
        dfs(&node, graph, &mut marks, &mut stack)?;
    }
    Ok(())
}

fn code_region_section_name(symbol: &str) -> SectionName {
    SectionName::from(format!("code_region_{}", sanitize_symbol(symbol)))
}

#[allow(clippy::too_many_arguments)]
fn compile_test_objects(
    db: &DriverDataBase,
    mir_module: &mir::MirModule<'_>,
    tests: &[TestInfo],
    call_graph: &CallGraph,
    funcs_by_symbol: &FxHashMap<&str, &MirFunction<'_>>,
    code_region_roots: &[String],
    code_region_sections: &FxHashMap<String, SectionName>,
    region_reachable: &FxHashMap<String, FxHashSet<String>>,
    region_deps: &FxHashMap<String, FxHashSet<String>>,
) -> Result<Module, LowerError> {
    let isa = super::create_evm_isa();
    let ctx = ModuleCtx::new(&isa);
    let builder = ModuleBuilder::new(ctx);

    let mut lowerer = ModuleLowerer::new(
        db,
        builder,
        mir_module,
        &isa,
        layout::EVM_LAYOUT,
        ContractObjectSelection::All,
    );
    lowerer.declare_all_functions_for_tests()?;

    let code_regions_object = create_code_regions_object(
        &mut lowerer,
        funcs_by_symbol,
        code_region_roots,
        code_region_sections,
        region_reachable,
        region_deps,
    )?;
    lowerer
        .builder
        .declare_object(code_regions_object)
        .map_err(|e| LowerError::Internal(format!("failed to declare CodeRegions object: {e}")))?;

    for test in tests {
        let object = create_test_object(
            &mut lowerer,
            funcs_by_symbol,
            call_graph,
            test,
            code_region_sections,
        )?;
        lowerer.builder.declare_object(object).map_err(|e| {
            LowerError::Internal(format!(
                "failed to declare test object `{}`: {e}",
                test.object_name
            ))
        })?;
    }

    lowerer.lower_functions()?;
    Ok(lowerer.finish())
}

fn create_code_regions_object(
    lowerer: &mut ModuleLowerer<'_, '_>,
    funcs_by_symbol: &FxHashMap<&str, &MirFunction<'_>>,
    code_region_roots: &[String],
    code_region_sections: &FxHashMap<String, SectionName>,
    _region_reachable: &FxHashMap<String, FxHashSet<String>>,
    region_deps: &FxHashMap<String, FxHashSet<String>>,
) -> Result<Object, LowerError> {
    let object_name = ObjectName::from("CodeRegions");
    let mut sections = Vec::with_capacity(code_region_roots.len());

    for root in code_region_roots {
        let Some(&root_func) = funcs_by_symbol.get(root.as_str()) else {
            return Err(LowerError::Internal(format!(
                "missing MIR function for code region root `{root}`"
            )));
        };
        let root_ref = *lowerer
            .name_map
            .get(root)
            .ok_or_else(|| LowerError::Internal(format!("unknown function: {root}")))?;

        let wrapper_name = format!("__fe_sonatina_code_region_entry_{}", sanitize_symbol(root));
        let argc = runtime_argc(lowerer.db, root_func);
        let wrapper_ref = lowerer.create_call_and_stop_wrapper(
            &wrapper_name,
            root_ref,
            argc,
            root_func.returns_value,
        )?;

        let section_name = code_region_sections
            .get(root)
            .cloned()
            .ok_or_else(|| LowerError::Internal(format!("missing section name for `{root}`")))?;

        let mut directives = vec![Directive::Entry(wrapper_ref), Directive::Include(root_ref)];

        let deps = region_deps.get(root).cloned().unwrap_or_default();
        let mut deps: Vec<String> = deps.into_iter().collect();
        deps.sort();
        for dep in deps {
            if dep == *root {
                continue;
            }
            let dep_section = code_region_sections.get(&dep).cloned().ok_or_else(|| {
                LowerError::Internal(format!(
                    "code region `{root}` depends on `{dep}`, but `{dep}` is not a known region root"
                ))
            })?;
            directives.push(Directive::Embed(Embed {
                source: SectionRef::Local(dep_section),
                as_symbol: EmbedSymbol::from(dep),
            }));
        }

        sections.push(Section {
            name: section_name,
            directives,
        });
    }

    Ok(Object {
        name: object_name,
        sections,
    })
}

fn create_test_object(
    lowerer: &mut ModuleLowerer<'_, '_>,
    funcs_by_symbol: &FxHashMap<&str, &MirFunction<'_>>,
    call_graph: &CallGraph,
    test: &TestInfo,
    code_region_sections: &FxHashMap<String, SectionName>,
) -> Result<Object, LowerError> {
    let Some(&test_func) = funcs_by_symbol.get(test.symbol_name.as_str()) else {
        return Err(LowerError::Internal(format!(
            "missing MIR function for test `{}`",
            test.symbol_name
        )));
    };

    let test_ref = *lowerer
        .name_map
        .get(&test.symbol_name)
        .ok_or_else(|| LowerError::Internal(format!("unknown function: {}", test.symbol_name)))?;

    let wrapper_name = format!("__fe_sonatina_test_entry_{}", test.object_name);
    let argc = runtime_argc(lowerer.db, test_func);
    let wrapper_ref = lowerer.create_call_and_stop_wrapper(
        &wrapper_name,
        test_ref,
        argc,
        test_func.returns_value,
    )?;

    let reachable = reachable_functions(call_graph, &test.symbol_name);
    let deps = collect_code_region_deps(&reachable, funcs_by_symbol);

    let mut directives = vec![Directive::Entry(wrapper_ref), Directive::Include(test_ref)];

    let code_regions_obj = ObjectName::from("CodeRegions");
    let mut deps: Vec<String> = deps.into_iter().collect();
    deps.sort();
    for dep in deps {
        let dep_section = code_region_sections.get(&dep).cloned().ok_or_else(|| {
            LowerError::Internal(format!(
                "test `{}` depends on code region `{dep}`, but `{dep}` is not a known region root",
                test.symbol_name
            ))
        })?;
        directives.push(Directive::Embed(Embed {
            source: SectionRef::External {
                object: code_regions_obj.clone(),
                section: dep_section,
            },
            as_symbol: EmbedSymbol::from(dep),
        }));
    }

    Ok(Object {
        name: ObjectName::from(test.object_name.clone()),
        sections: vec![Section {
            name: SectionName::from("runtime"),
            directives,
        }],
    })
}

impl<'db, 'a> ModuleLowerer<'db, 'a> {
    fn declare_all_functions_for_tests(&mut self) -> Result<(), LowerError> {
        for (idx, func) in self.mir.functions.iter().enumerate() {
            if func.symbol_name.is_empty() {
                continue;
            }

            let name = &func.symbol_name;
            let linkage = sonatina_ir::Linkage::Public;

            let mut params = Vec::new();
            let mut mask = Vec::new();
            for local_id in func.body.param_locals.iter().copied() {
                let local_ty = func
                    .body
                    .locals
                    .get(local_id.index())
                    .ok_or_else(|| {
                        LowerError::Internal(format!("unknown param local: {local_id:?}"))
                    })?
                    .ty;
                if layout::ty_size_bytes_in(self.db, &layout::EVM_LAYOUT, local_ty)
                    .is_some_and(|s| s == 0)
                {
                    mask.push(false);
                    continue;
                }
                mask.push(true);
                params.push(super::types::word_type());
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
                if layout::ty_size_bytes_in(self.db, &layout::EVM_LAYOUT, local_ty)
                    .is_some_and(|s| s == 0)
                {
                    mask.push(false);
                    continue;
                }
                mask.push(true);
                params.push(super::types::word_type());
            }

            let ret_ty = if func.returns_value {
                super::types::word_type()
            } else {
                super::types::unit_type()
            };

            let sig = Signature::new(name, linkage, &params, ret_ty);
            let func_ref = self.builder.declare_function(sig).map_err(|e| {
                LowerError::Internal(format!("failed to declare function {name}: {e}"))
            })?;

            self.func_map.insert(idx, func_ref);
            self.name_map.insert(name.clone(), func_ref);
            self.returns_value_map
                .insert(name.clone(), func.returns_value);
            self.runtime_param_masks.insert(name.clone(), mask);
        }
        Ok(())
    }

    fn create_call_and_stop_wrapper(
        &mut self,
        wrapper_name: &str,
        callee_ref: sonatina_ir::module::FuncRef,
        callee_argc: usize,
        callee_returns_value: bool,
    ) -> Result<sonatina_ir::module::FuncRef, LowerError> {
        if self.name_map.contains_key(wrapper_name) {
            return Err(LowerError::Internal(format!(
                "wrapper name collision: `{wrapper_name}`"
            )));
        }

        let sig = Signature::new(
            wrapper_name,
            sonatina_ir::Linkage::Public,
            &[],
            super::types::unit_type(),
        );
        let func_ref = self.builder.declare_function(sig).map_err(|e| {
            LowerError::Internal(format!("failed to declare wrapper `{wrapper_name}`: {e}"))
        })?;

        let mut fb = self.builder.func_builder::<InstInserter>(func_ref);
        let is = self.isa.inst_set();

        let entry_block = fb.append_block();
        fb.switch_to_block(entry_block);

        let mut args = Vec::with_capacity(callee_argc);
        for _ in 0..callee_argc {
            args.push(fb.make_imm_value(I256::zero()));
        }

        let call_inst = Call::new(is, callee_ref, args.into());
        if callee_returns_value {
            let _ = fb.insert_inst(call_inst, Type::I256);
        } else {
            fb.insert_inst_no_result(call_inst);
        }

        fb.insert_inst_no_result(EvmStop::new(is));
        fb.seal_all();
        fb.finish();

        Ok(func_ref)
    }
}

struct RuntimeCompileOutput {
    bytes: Vec<u8>,
    observability_text: Option<String>,
    observability_json: Option<String>,
}

fn compile_runtime_section(
    module: &Module,
    object_name: &str,
    debug: &SonatinaTestDebugConfig,
) -> Result<RuntimeCompileOutput, LowerError> {
    let isa = super::create_evm_isa();
    let backend = EvmBackend::new(isa);

    let mut opts: CompileOptions<_> = CompileOptions::default();
    let mut verifier_cfg = VerifierConfig::for_level(VerificationLevel::Full);
    // Sonatina SSA currently keeps removed trivial-phi instructions detached from layout.
    // Keep full verifier checks enabled while tolerating detached entities until SSA cleanup lands.
    verifier_cfg.allow_detached_entities = true;
    opts.verifier_cfg = verifier_cfg;
    opts.emit_observability = debug.emit_observability;
    let artifact = compile_object(module, &backend, object_name, &opts).map_err(|errors| {
        let msg = errors
            .iter()
            .map(|e| format!("{:?}", e))
            .collect::<Vec<_>>()
            .join("; ");
        LowerError::Internal(msg)
    })?;

    let section_name = SectionName::from("runtime");
    let runtime_section = artifact.sections.get(&section_name).ok_or_else(|| {
        LowerError::Internal(format!(
            "compiled object `{object_name}` has no runtime section"
        ))
    })?;

    let mut runtime_funcs: Vec<(u32, FuncRef)> = Vec::new();
    for (sym, def) in &runtime_section.symtab {
        if let SymbolId::Func(func_ref) = sym {
            runtime_funcs.push((def.offset, *func_ref));
        }
    }
    runtime_funcs.sort_by_key(|(offset, _)| *offset);
    let mut ordered_runtime_funcs = Vec::with_capacity(runtime_funcs.len());
    let mut seen_runtime_funcs: FxHashSet<FuncRef> = FxHashSet::default();
    for (_, func_ref) in runtime_funcs {
        if seen_runtime_funcs.insert(func_ref) {
            ordered_runtime_funcs.push(func_ref);
        }
    }

    if let Some(sink) = &debug.symtab_output {
        let mut defs: Vec<(u32, u32, String)> = Vec::new();
        for (sym, def) in &runtime_section.symtab {
            let name = match sym {
                SymbolId::Func(func_ref) => {
                    module.ctx.func_sig(*func_ref, |sig| sig.name().to_string())
                }
                SymbolId::Global(gv) => format!("{gv:?}"),
                SymbolId::Embed(embed) => format!("&{}", embed.0.as_str()),
                SymbolId::CurrentSection => "<current_section>".to_string(),
            };
            defs.push((def.offset, def.size, name));
        }
        defs.sort_by_key(|(offset, _, _)| *offset);

        let mut out = String::new();
        out.push_str(&format!(
            "SONATINA SYMTAB object={object_name} section=runtime bytes={}\n",
            runtime_section.bytes.len()
        ));
        for (offset, size, name) in defs {
            out.push_str(&format!("  off={offset:>6} size={size:>6} {name}\n"));
        }
        emit_debug_output(sink, &out);
    }

    if let Some(sink) = &debug.evm_debug_output
        && let Err(err) =
            emit_runtime_evm_debug(module, object_name, &ordered_runtime_funcs, debug, sink)
    {
        tracing::warn!("failed to write Sonatina EVM debug output for `{object_name}`: {err}",);
    }

    for &offset in &debug.runtime_byte_offsets {
        match runtime_section.bytes.get(offset) {
            Some(byte) => tracing::debug!(
                "SONATINA BYTE object={object_name} section=runtime off={offset} byte=0x{byte:02x}"
            ),
            None => tracing::warn!(
                "SONATINA BYTE object={object_name} section=runtime off={offset} (out of bounds, len={})",
                runtime_section.bytes.len()
            ),
        }
    }

    let observability_text = artifact.observability_text();
    let observability_json = artifact.observability_json();

    Ok(RuntimeCompileOutput {
        bytes: runtime_section.bytes.clone(),
        observability_text,
        observability_json,
    })
}

fn emit_runtime_evm_debug(
    module: &Module,
    object_name: &str,
    funcs: &[FuncRef],
    debug: &SonatinaTestDebugConfig,
    sink: &DebugOutputSink,
) -> Result<(), LowerError> {
    let (object_name_ref, section_name, embed_symbols) =
        runtime_section_lowering_inputs(module, object_name)?;
    if funcs.is_empty() {
        return Err(LowerError::Internal(format!(
            "runtime section for `{object_name}` has no lowered functions in compiled artifact"
        )));
    }

    let isa = super::create_evm_isa();
    let backend = EvmBackend::new(isa);

    let section_ctx = SectionLoweringCtx {
        object: &object_name_ref,
        section: &section_name,
        embed_symbols: &embed_symbols,
    };
    backend.prepare_section(module, funcs, &section_ctx);

    let mut out = Vec::new();
    writeln!(
        &mut out,
        "SONATINA EVM DEBUG object={} section={}",
        object_name_ref.0.as_str(),
        section_name.0.as_str(),
    )
    .unwrap();
    writeln!(&mut out).unwrap();

    for &func in funcs {
        let lowered = backend
            .lower_function(module, func, &section_ctx)
            .map_err(|err| {
                let func_name = module.ctx.func_sig(func, |sig| sig.name().to_string());
                LowerError::Internal(format!(
                    "failed to lower `{func_name}` for Sonatina EVM debug output: {err}"
                ))
            })?;

        let reach_depth = debug.stackify_reach_depth.clamp(1, 16);
        let (stackify_dump, lowered_dump) =
            module
                .func_store
                .view(func, |function| -> Result<(String, String), LowerError> {
                    let mut cfg = ControlFlowGraph::new();
                    cfg.compute(function);

                    let mut liveness = Liveness::new();
                    liveness.compute(function, &cfg);
                    let mut dom = DomTree::new();
                    dom.compute(&cfg);

                    let (_alloc, stackify_trace) =
                        StackifyBuilder::new(function, &cfg, &dom, &liveness, reach_depth)
                            .compute_with_trace();

                    let ctx = FuncWriteCtx::new(function, func);

                    let mut stackify_buf = Vec::new();
                    write!(&mut stackify_buf, "// ").unwrap();
                    FunctionSignature
                        .write(&mut stackify_buf, &ctx)
                        .map_err(|err| {
                            LowerError::Internal(format!(
                                "failed to render stackify signature for `{object_name}`: {err}"
                            ))
                        })?;
                    writeln!(&mut stackify_buf).unwrap();
                    writeln!(&mut stackify_buf, "{stackify_trace}").unwrap();

                    let mut lowered_buf = Vec::new();
                    lowered.vcode.write(&mut lowered_buf, &ctx).map_err(|err| {
                        LowerError::Internal(format!(
                            "failed to render lowered EVM vcode for `{object_name}`: {err}"
                        ))
                    })?;
                    writeln!(&mut lowered_buf).unwrap();

                    let stackify_dump = String::from_utf8(stackify_buf).map_err(|err| {
                    LowerError::Internal(format!(
                        "invalid UTF-8 while rendering stackify trace for `{object_name}`: {err}"
                    ))
                })?;
                    let lowered_dump = String::from_utf8(lowered_buf).map_err(|err| {
                    LowerError::Internal(format!(
                        "invalid UTF-8 while rendering lowered EVM vcode for `{object_name}`: {err}"
                    ))
                })?;

                    Ok((stackify_dump, lowered_dump))
                })?;

        out.extend_from_slice(stackify_dump.as_bytes());
        out.extend_from_slice(lowered_dump.as_bytes());
    }

    let rendered = String::from_utf8(out).map_err(|err| {
        LowerError::Internal(format!(
            "invalid UTF-8 while rendering Sonatina EVM debug output for `{object_name}`: {err}"
        ))
    })?;
    emit_debug_output(sink, &rendered);

    Ok(())
}

fn runtime_section_lowering_inputs(
    module: &Module,
    object_name: &str,
) -> Result<(ObjectName, SectionName, Vec<EmbedSymbol>), LowerError> {
    let Some(object) = module.objects.get(object_name) else {
        return Err(LowerError::Internal(format!(
            "missing Sonatina object `{object_name}` while preparing EVM debug output"
        )));
    };

    let Some(runtime_section) = object
        .sections
        .iter()
        .find(|section| section.name.0.as_str() == "runtime")
    else {
        return Err(LowerError::Internal(format!(
            "object `{object_name}` has no runtime section while preparing EVM debug output"
        )));
    };

    let mut embed_symbols = Vec::new();
    for directive in &runtime_section.directives {
        match directive {
            Directive::Entry(_) | Directive::Include(_) => {}
            Directive::Embed(embed) => embed_symbols.push(embed.as_symbol.clone()),
            Directive::Data(_) => {}
        }
    }

    Ok((
        object.name.clone(),
        runtime_section.name.clone(),
        embed_symbols,
    ))
}

fn emit_debug_output(sink: &DebugOutputSink, contents: &str) {
    if let Some(path) = &sink.path {
        match std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .and_then(|mut f| f.write_all(contents.as_bytes()))
        {
            Ok(()) => {}
            Err(err) => {
                tracing::error!(
                    "failed to write Sonatina debug output `{}`: {err}",
                    path.display()
                );
                tracing::debug!("{contents}");
                return;
            }
        }
    }

    if sink.write_stderr {
        tracing::debug!("{contents}");
    }
}

fn wrap_as_init_code(runtime: &[u8]) -> Vec<u8> {
    // Minimal initcode:
    //   CODECOPY(0, <off>, <len>)
    //   RETURN(0, <len>)
    // with the runtime appended after the init.
    //
    // PUSHn <len>
    // PUSH2 <off>
    // PUSH1 0
    // CODECOPY
    // PUSHn <len>
    // PUSH1 0
    // RETURN
    fn push_u256(mut value: usize) -> Vec<u8> {
        let mut bytes = Vec::new();
        while value > 0 {
            bytes.push((value & 0xff) as u8);
            value >>= 8;
        }
        if bytes.is_empty() {
            bytes.push(0);
        }
        bytes.reverse();
        let n = bytes.len();
        debug_assert!((1..=32).contains(&n));
        let opcode = 0x5f + (n as u8);
        let mut out = Vec::with_capacity(1 + n);
        out.push(opcode);
        out.extend(bytes);
        out
    }

    let len_push = push_u256(runtime.len());

    let mut init = Vec::with_capacity(32 + runtime.len());
    init.extend(len_push.clone());
    init.push(0x61); // PUSH2
    let off_pos = init.len();
    init.extend([0u8, 0u8]); // filled in later
    init.extend([0x60, 0x00]); // PUSH1 0
    init.push(0x39); // CODECOPY
    init.extend(len_push);
    init.extend([0x60, 0x00]); // PUSH1 0
    init.push(0xf3); // RETURN

    let off = init.len();
    init[off_pos] = ((off >> 8) & 0xff) as u8;
    init[off_pos + 1] = (off & 0xff) as u8;

    init.extend_from_slice(runtime);
    init
}
