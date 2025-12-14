use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;

use crate::{MirFunction, MirInst, Rvalue, Terminator};

pub type CallGraph = FxHashMap<String, Vec<String>>;

/// Builds an adjacency list of calls between lowered functions keyed by their symbol name.
pub fn build_call_graph(functions: &[MirFunction<'_>]) -> CallGraph {
    let mut graph = FxHashMap::default();
    let known: FxHashSet<_> = functions
        .iter()
        .map(|func| func.symbol_name.clone())
        .collect();

    for func in functions {
        let mut callees = FxHashSet::default();
        for block in &func.body.blocks {
            for inst in &block.insts {
                if let MirInst::Assign {
                    rvalue: Rvalue::Call(call),
                    ..
                } = inst
                    && let Some(target) = &call.resolved_name
                    && known.contains(target)
                {
                    callees.insert(target.clone());
                }
            }

            if let Terminator::TerminatingCall(crate::TerminatingCall::Call(call)) =
                &block.terminator
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
