use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;

use hir::{
    analysis::HirAnalysisDb,
    hir_def::{CallableDef, Func},
};

use crate::{CallOrigin, MirFunction, MirInst, Rvalue, Terminator, ir::MirFunctionOrigin};

pub type CallGraph = FxHashMap<String, Vec<String>>;
pub type FunctionSymbolMap<'db> = FxHashMap<Func<'db>, Vec<String>>;

pub fn build_function_symbol_map<'db>(functions: &[MirFunction<'db>]) -> FunctionSymbolMap<'db> {
    let mut symbols = FxHashMap::default();
    for func in functions {
        let MirFunctionOrigin::Hir(hir_func) = func.origin else {
            continue;
        };
        symbols
            .entry(hir_func)
            .or_insert_with(Vec::new)
            .push(func.symbol_name.clone());
    }

    for entries in symbols.values_mut() {
        entries.sort();
        entries.dedup();
    }

    symbols
}

/// Builds an adjacency list of calls between lowered functions keyed by their symbol name.
pub fn build_call_graph(db: &dyn HirAnalysisDb, functions: &[MirFunction<'_>]) -> CallGraph {
    let mut graph = FxHashMap::default();
    let known: FxHashSet<_> = functions
        .iter()
        .map(|func| func.symbol_name.clone())
        .collect();
    let symbol_by_func = build_function_symbol_map(functions);

    for func in functions {
        let mut callees = FxHashSet::default();
        for block in &func.body.blocks {
            for inst in &block.insts {
                if let MirInst::Assign {
                    rvalue: Rvalue::Call(call),
                    ..
                } = inst
                    && let Some(target) = call_target_symbol(db, call, &symbol_by_func)
                    && known.contains(&target)
                {
                    callees.insert(target);
                }
            }

            if let Terminator::TerminatingCall {
                call: crate::TerminatingCall::Call(call),
                ..
            } = &block.terminator
                && let Some(target) = call_target_symbol(db, call, &symbol_by_func)
                && known.contains(&target)
            {
                callees.insert(target);
            }
        }
        graph.insert(func.symbol_name.clone(), callees.into_iter().collect());
    }

    graph
}

pub fn call_target_symbol(
    db: &dyn HirAnalysisDb,
    call: &CallOrigin<'_>,
    symbol_by_func: &FunctionSymbolMap<'_>,
) -> Option<String> {
    if let Some(resolved) = &call.resolved_name {
        return Some(resolved.clone());
    }

    if let Some(hir_target) = call.hir_target.as_ref()
        && let CallableDef::Func(func) = hir_target.callable_def
    {
        func.body(db)?;
        if let Some(symbols) = symbol_by_func.get(&func)
            && let [symbol] = symbols.as_slice()
        {
            return Some(symbol.clone());
        }
    }

    None
}

/// Walks the call graph from `root` and returns all reachable symbols (including the root).
pub fn reachable_functions(graph: &CallGraph, root: &str) -> FxHashSet<String> {
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
