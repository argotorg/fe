//! Module-level Yul emission helpers (functions + code regions).

use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use mir::ir::ContractFunctionKind;
use mir::{MirFunction, lower_module};
use rustc_hash::{FxHashMap, FxHashSet};
use std::{collections::VecDeque, sync::Arc};

use crate::yul::doc::{YulDoc, render_docs};

use super::{EmitModuleError, function::FunctionEmitter};

/// Emits Yul for every function in the lowered MIR module.
///
/// * `db` - Driver database used to query compiler facts.
/// * `top_mod` - Root module to lower.
///
/// Returns a single Yul string containing all lowered functions followed by any
/// auto-generated code regions, or [`EmitModuleError`] if MIR lowering or Yul
/// emission fails.
pub fn emit_module_yul(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
) -> Result<String, EmitModuleError> {
    let module = lower_module(db, top_mod).map_err(EmitModuleError::MirLower)?;

    let call_graph = build_call_graph(&module.functions);
    let contracts = collect_contracts(&module.functions);
    let symbol_to_contract = contract_symbol_map(&contracts);

    let mut code_regions = FxHashMap::default();
    for (name, entry) in &contracts {
        if let Some(init) = &entry.init_symbol {
            code_regions.insert(init.clone(), name.clone());
        }
        if let Some(runtime) = &entry.runtime_symbol {
            code_regions.insert(runtime.clone(), format!("{name}_deployed"));
        }
    }
    let code_region_roots = collect_code_region_roots(&module.functions);
    for root in &code_region_roots {
        if code_regions.contains_key(root) {
            continue;
        }
        code_regions
            .entry(root.clone())
            .or_insert_with(|| format!("code_region_{}", sanitize_symbol(root)));
    }
    let code_regions = Arc::new(code_regions);

    // Emit Yul docs for each function
    let mut function_docs: Vec<Vec<YulDoc>> = Vec::with_capacity(module.functions.len());
    for func in &module.functions {
        let emitter =
            FunctionEmitter::new(db, func, &code_regions).map_err(EmitModuleError::Yul)?;
        let docs = emitter.emit_doc().map_err(EmitModuleError::Yul)?;
        function_docs.push(docs);
    }

    // Index function docs by symbol for region assembly.
    let mut docs_by_symbol = FxHashMap::default();
    for (idx, func) in module.functions.iter().enumerate() {
        docs_by_symbol.insert(func.symbol_name.clone(), function_docs[idx].clone());
    }

    // Build contract dependency graph from code_region references inside contract entrypoints.
    let mut contract_edges: FxHashMap<String, FxHashSet<String>> = FxHashMap::default();
    let mut referenced_contracts = FxHashSet::default();
    for func in &module.functions {
        let Some(contract_fn) = &func.contract_function else {
            continue;
        };
        for value in &func.body.values {
            if let mir::ValueOrigin::Intrinsic(intr) = &value.origin
                && matches!(
                    intr.op,
                    mir::ir::IntrinsicOp::CodeRegionOffset | mir::ir::IntrinsicOp::CodeRegionLen
                )
                && let Some(target) = &intr.code_region
                && let Some(symbol) = &target.symbol
                && let Some(child_name) = symbol_to_contract.get(symbol)
                && *child_name != contract_fn.contract_name
            {
                contract_edges
                    .entry(contract_fn.contract_name.clone())
                    .or_default()
                    .insert(child_name.clone());
                referenced_contracts.insert(child_name.clone());
            }
        }
    }

    let root_contracts: Vec<_> = contracts
        .keys()
        .filter(|name| !referenced_contracts.contains(*name))
        .cloned()
        .collect();

    let mut docs = Vec::new();
    let mut visited_contracts = FxHashSet::default();
    for name in root_contracts {
        if let Some(contract_doc) = emit_contract_docs(
            &name,
            &contracts,
            &contract_edges,
            &call_graph,
            &docs_by_symbol,
            &mut visited_contracts,
        ) {
            docs.push(contract_doc);
        }
    }

    // Free-function code regions not tied to contract entrypoints.
    for root in code_region_roots {
        if contracts.values().any(|entry| {
            entry.init_symbol.as_ref() == Some(&root)
                || entry.runtime_symbol.as_ref() == Some(&root)
        }) {
            continue;
        }
        let Some(label) = code_regions.get(&root) else {
            continue;
        };
        let reachable = reachable_functions(&call_graph, &root);
        let mut region_docs = Vec::new();
        for symbol in &reachable {
            if let Some(func_docs) = docs_by_symbol.get(symbol) {
                region_docs.extend(func_docs.clone());
            }
        }
        docs.push(YulDoc::block(
            format!("object \"{label}\" "),
            vec![YulDoc::block("code ", region_docs)],
        ));
    }

    // If nothing was emitted (no regions), fall back to top-level functions.
    if docs.is_empty() {
        for func_docs in function_docs {
            docs.extend(func_docs);
        }
    }

    let mut lines = Vec::new();
    render_docs(&docs, 0, &mut lines);
    Ok(join_lines(lines))
}

/// Joins rendered lines while trimming trailing whitespace-only entries.
///
/// * `lines` - Vector of rendered Yul lines.
///
/// Returns the normalized Yul output string.
fn join_lines(mut lines: Vec<String>) -> String {
    while lines.last().is_some_and(|line| line.is_empty()) {
        lines.pop();
    }
    lines.join("\n")
}

/// Collects all function symbols referenced by `code_region` intrinsics and all contract
/// entrypoints.
fn collect_code_region_roots(functions: &[MirFunction<'_>]) -> Vec<String> {
    let mut roots = FxHashSet::default();
    for func in functions {
        if func.contract_function.is_some() {
            roots.insert(func.symbol_name.clone());
        }
    }
    roots.into_iter().collect()
}

/// Builds an adjacency list of calls between lowered functions keyed by their symbol name.
fn build_call_graph(functions: &[MirFunction<'_>]) -> FxHashMap<String, Vec<String>> {
    let mut graph = FxHashMap::default();
    let known: FxHashSet<_> = functions
        .iter()
        .map(|func| func.symbol_name.clone())
        .collect();

    for func in functions {
        let mut callees = FxHashSet::default();
        for value in &func.body.values {
            if let mir::ValueOrigin::Call(call) = &value.origin
                && let Some(target) = &call.resolved_name
                && known.contains(target)
            {
                callees.insert(target.clone());
            }
        }
        graph.insert(func.symbol_name.clone(), callees.into_iter().collect());
    }

    graph
}

/// Walks the call graph from `root` and returns all reachable symbols (including the root).
fn reachable_functions(graph: &FxHashMap<String, Vec<String>>, root: &str) -> FxHashSet<String> {
    let mut visited = FxHashSet::default();
    let mut stack = VecDeque::new();
    stack.push_back(root.to_string());
    while let Some(symbol) = stack.pop_back() {
        if !visited.insert(symbol.clone()) {
            continue;
        }
        if let Some(children) = graph.get(&symbol) {
            for child in children {
                stack.push_back(child.clone());
            }
        }
    }
    visited
}

/// Replace any non-alphanumeric characters with `_` so the label is a valid Yul identifier.
fn sanitize_symbol(component: &str) -> String {
    component
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}

#[derive(Default)]
struct ContractEntry {
    init_symbol: Option<String>,
    runtime_symbol: Option<String>,
}

fn collect_contracts(functions: &[MirFunction<'_>]) -> FxHashMap<String, ContractEntry> {
    let mut contracts = FxHashMap::default();
    for func in functions {
        let Some(contract_fn) = &func.contract_function else {
            continue;
        };
        let entry: &mut ContractEntry = contracts
            .entry(contract_fn.contract_name.clone())
            .or_default();
        match contract_fn.kind {
            ContractFunctionKind::Init => entry.init_symbol = Some(func.symbol_name.clone()),
            ContractFunctionKind::Runtime => entry.runtime_symbol = Some(func.symbol_name.clone()),
        }
    }
    contracts
}

fn contract_symbol_map(contracts: &FxHashMap<String, ContractEntry>) -> FxHashMap<String, String> {
    let mut map = FxHashMap::default();
    for (name, entry) in contracts {
        if let Some(sym) = &entry.init_symbol {
            map.insert(sym.clone(), name.clone());
        }
        if let Some(sym) = &entry.runtime_symbol {
            map.insert(sym.clone(), name.clone());
        }
    }
    map
}

fn emit_contract_docs(
    name: &str,
    contracts: &FxHashMap<String, ContractEntry>,
    edges: &FxHashMap<String, FxHashSet<String>>,
    call_graph: &FxHashMap<String, Vec<String>>,
    docs_by_symbol: &FxHashMap<String, Vec<YulDoc>>,
    visited: &mut FxHashSet<String>,
) -> Option<YulDoc> {
    if !visited.insert(name.to_string()) {
        return None;
    }
    let entry = contracts.get(name)?;

    let mut components = Vec::new();

    let mut init_docs = Vec::new();
    if let Some(symbol) = &entry.init_symbol {
        init_docs.push(YulDoc::line(format!("{symbol}()")));
        init_docs.extend(reachable_docs(symbol, call_graph, docs_by_symbol));
    }
    components.push(YulDoc::block("code ", init_docs));

    if let Some(symbol) = &entry.runtime_symbol {
        let mut runtime_docs = Vec::new();
        runtime_docs.push(YulDoc::line(format!("{symbol}()")));
        runtime_docs.extend(reachable_docs(symbol, call_graph, docs_by_symbol));
        components.push(YulDoc::line(String::new()));
        components.push(YulDoc::block(
            format!("object \"{name}_deployed\" "),
            vec![YulDoc::block("code ", runtime_docs)],
        ));
    }

    if let Some(children) = edges.get(name) {
        let mut names: Vec<_> = children.iter().cloned().collect();
        names.sort();
        for child in names {
            if let Some(doc) = emit_contract_docs(
                &child,
                contracts,
                edges,
                call_graph,
                docs_by_symbol,
                visited,
            ) {
                components.push(doc);
            }
        }
    }

    Some(YulDoc::block(format!("object \"{name}\" "), components))
}

fn reachable_docs(
    root: &str,
    call_graph: &FxHashMap<String, Vec<String>>,
    docs_by_symbol: &FxHashMap<String, Vec<YulDoc>>,
) -> Vec<YulDoc> {
    let mut docs = Vec::new();
    let reachable = reachable_functions(call_graph, root);
    let mut symbols: Vec<_> = reachable.into_iter().collect();
    symbols.sort();
    for symbol in symbols {
        if let Some(func_docs) = docs_by_symbol.get(&symbol) {
            docs.extend(func_docs.clone());
        }
    }
    docs
}
