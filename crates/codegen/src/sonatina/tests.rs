use driver::DriverDataBase;
use hir::hir_def::{ItemKind, TopLevelMod};
use mir::analysis::{CallGraph, build_call_graph, reachable_functions};
use mir::{
    MirFunction, MirInst, Rvalue,
    ir::{IntrinsicOp, MirFunctionOrigin},
    layout,
    lower_module,
};
use rustc_hash::{FxHashMap, FxHashSet};
use sonatina_ir::{
    I256, Module, Signature, Type,
    builder::ModuleBuilder,
    func_cursor::InstInserter,
    inst::{control_flow::Call, evm::EvmStop},
    isa::Isa,
    module::ModuleCtx,
    object::{Directive, Embed, EmbedSymbol, Object, ObjectName, Section, SectionName, SectionRef},
};
use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

use crate::{TestMetadata, TestModuleOutput};

use super::{LowerError, ModuleLowerer};

pub fn emit_test_module_sonatina(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
) -> Result<TestModuleOutput, LowerError> {
    let mir_module = lower_module(db, top_mod)?;
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

    let module = compile_test_objects(
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

    let mut output_tests = Vec::with_capacity(tests.len());
    for test in tests {
        let runtime = compile_runtime_section(&module, &test.object_name)?;
        let init_bytecode = wrap_as_init_code(&runtime);

        output_tests.push(TestMetadata {
            display_name: test.display_name,
            hir_name: test.hir_name,
            symbol_name: test.symbol_name,
            object_name: test.object_name,
            yul: String::new(),
            bytecode: init_bytecode,
            value_param_count: test.value_param_count,
            effect_param_count: test.effect_param_count,
        });
    }

    Ok(TestModuleOutput { tests: output_tests })
}

#[derive(Debug, Clone)]
struct TestInfo {
    hir_name: String,
    display_name: String,
    symbol_name: String,
    object_name: String,
    value_param_count: usize,
    effect_param_count: usize,
}

fn collect_tests(db: &DriverDataBase, functions: &[MirFunction<'_>]) -> Vec<TestInfo> {
    let mut tests: Vec<TestInfo> = functions
        .iter()
        .filter_map(|mir_func| {
            let MirFunctionOrigin::Hir(hir_func) = mir_func.origin else {
                return None;
            };
            if !ItemKind::from(hir_func)
                .attrs(db)
                .is_some_and(|attrs| attrs.has_attr(db, "test"))
            {
                return None;
            }

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
            let Some(local_ty) = local_ty else { return true };
            !layout::ty_size_bytes_in(db, &layout::EVM_LAYOUT, local_ty).is_some_and(|s| s == 0)
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
                let Some(arg) = args.first().copied() else { continue };
                let mir::ValueOrigin::FuncItem(target) = &func.body.value(arg).origin else {
                    continue;
                };
                let Some(symbol) = &target.symbol else { continue };
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
                let Some(arg) = args.first().copied() else { continue };
                let mir::ValueOrigin::FuncItem(target) = &func.body.value(arg).origin else {
                    continue;
                };
                let Some(target_symbol) = &target.symbol else { continue };
                deps.insert(target_symbol.clone());
            }
        }
    }
    deps
}

fn detect_code_region_cycles(graph: &FxHashMap<String, FxHashSet<String>>) -> Result<(), LowerError> {
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
    let triple = TargetTriple::new(
        Architecture::Evm,
        Vendor::Ethereum,
        OperatingSystem::Evm(EvmVersion::Osaka),
    );
    let isa = sonatina_ir::isa::evm::Evm::new(triple);
    let ctx = ModuleCtx::new(&isa);
    let builder = ModuleBuilder::new(ctx);

    let mut lowerer = ModuleLowerer::new(db, builder, mir_module, &isa, layout::EVM_LAYOUT);
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
            LowerError::Internal(format!("failed to declare test object `{}`: {e}", test.object_name))
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
    region_reachable: &FxHashMap<String, FxHashSet<String>>,
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

        let reachable = region_reachable.get(root).ok_or_else(|| {
            LowerError::Internal(format!("missing reachability for code region `{root}`"))
        })?;
        let mut reachable: Vec<&String> = reachable.iter().collect();
        reachable.sort();
        for symbol in reachable {
            if symbol == root {
                continue;
            }
            if let Some(&func_ref) = lowerer.name_map.get(symbol) {
                directives.push(Directive::Include(func_ref));
            }
        }

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
    let wrapper_ref =
        lowerer.create_call_and_stop_wrapper(&wrapper_name, test_ref, argc, test_func.returns_value)?;

    let reachable = reachable_functions(call_graph, &test.symbol_name);
    let deps = collect_code_region_deps(&reachable, funcs_by_symbol);

    let mut directives = vec![Directive::Entry(wrapper_ref), Directive::Include(test_ref)];
    let mut reachable: Vec<String> = reachable.into_iter().collect();
    reachable.sort();
    for symbol in reachable {
        if symbol == test.symbol_name {
            continue;
        }
        if let Some(&func_ref) = lowerer.name_map.get(symbol.as_str()) {
            directives.push(Directive::Include(func_ref));
        }
    }

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
                    continue;
                }
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
                    continue;
                }
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
            LowerError::Internal(format!(
                "failed to declare wrapper `{wrapper_name}`: {e}"
            ))
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

fn compile_runtime_section(module: &Module, object_name: &str) -> Result<Vec<u8>, LowerError> {
    use sonatina_codegen::isa::evm::EvmBackend;
    use sonatina_codegen::object::{CompileOptions, compile_object};
    use sonatina_codegen::object::SymbolId;
    use sonatina_ir::isa::evm::Evm;

    let triple = TargetTriple::new(
        Architecture::Evm,
        Vendor::Ethereum,
        OperatingSystem::Evm(EvmVersion::Osaka),
    );
    let isa = Evm::new(triple);
    let backend = EvmBackend::new(isa);

    let opts: CompileOptions<_> = CompileOptions::default();
    let artifact =
        compile_object(module, &backend, object_name, &opts).map_err(|errors| {
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

    if std::env::var("FE_SONATINA_DUMP_SYMTAB")
        .map(|v| v != "0" && !v.is_empty())
        .unwrap_or(false)
    {
        let mut defs: Vec<(u32, u32, String)> = Vec::new();
        for (sym, def) in &runtime_section.symtab {
            let name = match sym {
                SymbolId::Func(func_ref) => module
                    .ctx
                    .func_sig(*func_ref, |sig| sig.name().to_string()),
                SymbolId::Global(gv) => format!("{gv:?}"),
                SymbolId::Embed(embed) => format!("&{}", embed.0.as_str()),
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
        emit_debug_output(
            "FE_SONATINA_DUMP_SYMTAB_OUT",
            "FE_SONATINA_DUMP_SYMTAB_STDERR",
            &out,
        );
    }

    if let Ok(offsets) = std::env::var("FE_SONATINA_DUMP_BYTE_AT") {
        for raw in offsets.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
            let parsed = raw
                .strip_prefix("0x")
                .map(|hex| usize::from_str_radix(hex, 16))
                .unwrap_or_else(|| raw.parse());
            let offset = match parsed {
                Ok(v) => v,
                Err(_) => {
                    eprintln!("FE_SONATINA_DUMP_BYTE_AT: invalid offset `{raw}`");
                    continue;
                }
            };
            match runtime_section.bytes.get(offset) {
                Some(byte) => eprintln!(
                    "SONATINA BYTE object={object_name} section=runtime off={offset} byte=0x{byte:02x}"
                ),
                None => eprintln!(
                    "SONATINA BYTE object={object_name} section=runtime off={offset} (out of bounds, len={})",
                    runtime_section.bytes.len()
                ),
            }
        }
    }

    Ok(runtime_section.bytes.clone())
}

fn emit_debug_output(out_path_env: &str, stderr_env: &str, contents: &str) {
    use std::io::Write;

    let out_path = std::env::var_os(out_path_env).map(std::path::PathBuf::from);
    let write_stderr = out_path.is_none()
        || std::env::var(stderr_env)
            .map(|v| v != "0" && !v.is_empty())
            .unwrap_or(false);

    if let Some(path) = out_path {
        match std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&path)
            .and_then(|mut f| f.write_all(contents.as_bytes()))
        {
            Ok(()) => {}
            Err(err) => {
                eprintln!("{out_path_env}: failed to write `{}`: {err}", path.display());
                eprintln!("{contents}");
                return;
            }
        }
    }

    if write_stderr {
        eprint!("{contents}");
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
