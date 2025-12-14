//! Deduplicate monomorphized MIR bodies so runtime helpers only appear once.
//!
//! After monomorphization every instantiation of helpers such as `to_word`
//! or `store_field` produces a fresh MIR function, often with identical
//! bodies. This module canonicalises those copies by hashing their MIR
//! structure (recursively hashing callees) and re-writing all call sites to
//! refer to a single representative symbol. A second pass explicitly groups
//! known helper roots (e.g. `store_field__*`) so they share a stable
//! `__deduped` symbol name.

use std::collections::VecDeque;

use common::ingot::IngotKind;
use hir::analysis::HirAnalysisDb;
use rustc_hash::FxHashMap;

use crate::{MirFunction, hash::hash_function};

// FIXME: We should not have to hardcode these roots here.
const HELPER_ROOTS: &[&str] = &["store_field", "to_word"];

/// Runs both structural and helper-specific deduplication on the given MIR set and
/// returns the pruned/canonicalized function list.
pub(crate) fn deduplicate_mir<'db>(
    db: &'db dyn HirAnalysisDb,
    functions: Vec<MirFunction<'db>>,
) -> Vec<MirFunction<'db>> {
    let functions = deduplicate_functions(db, functions);
    dedup_runtime_helpers(functions)
}

/// Structural deduplication: hashes MIR bodies (including callees) so any two
/// functions that are semantically identical reuse the same symbol and returns the
/// surviving representatives.
fn deduplicate_functions<'db>(
    db: &'db dyn HirAnalysisDb,
    functions: Vec<MirFunction<'db>>,
) -> Vec<MirFunction<'db>> {
    if functions.len() <= 1 {
        return functions;
    }

    let mut symbol_to_idx = FxHashMap::default();
    for (idx, func) in functions.iter().enumerate() {
        symbol_to_idx.insert(func.symbol_name.clone(), idx);
    }

    let edges = build_call_edges(&functions, &symbol_to_idx);
    let order = topo_order(&edges);

    let mut canonical_idx: Vec<Option<usize>> = vec![None; functions.len()];
    let mut canonical_symbol: Vec<Option<String>> = vec![None; functions.len()];
    let mut hash_to_idx: FxHashMap<u64, usize> = FxHashMap::default();

    for idx in order {
        if !is_dedup_candidate(db, &functions[idx]) {
            canonical_idx[idx] = Some(idx);
            canonical_symbol[idx] = Some(functions[idx].symbol_name.clone());
            continue;
        }
        let hash = hash_function(db, &functions[idx], &symbol_to_idx, &canonical_symbol);
        if let Some(&existing) = hash_to_idx.get(&hash) {
            canonical_idx[idx] = Some(existing);
            if let Some(sym) = canonical_symbol[existing].clone() {
                canonical_symbol[idx] = Some(sym);
            }
        } else {
            hash_to_idx.insert(hash, idx);
            canonical_idx[idx] = Some(idx);
            canonical_symbol[idx] = Some(functions[idx].symbol_name.clone());
        }
    }

    for idx in 0..functions.len() {
        if canonical_symbol[idx].is_none()
            && let Some(canon) = canonical_idx[idx]
        {
            canonical_symbol[idx] = Some(functions[canon].symbol_name.clone());
        }
    }

    let mut keep = vec![false; functions.len()];
    for idx in 0..functions.len() {
        if canonical_idx[idx] == Some(idx) {
            keep[idx] = true;
        }
    }

    let mut kept = Vec::new();
    for (idx, func) in functions.iter().enumerate() {
        if keep[idx] {
            kept.push(func.clone());
        }
    }

    let mut symbol_lookup = FxHashMap::default();
    for func in &kept {
        symbol_lookup.insert(func.symbol_name.clone(), func.symbol_name.clone());
    }
    for (idx, func) in functions.iter().enumerate() {
        if !keep[idx]
            && let Some(canon) = canonical_idx[idx]
        {
            let name = functions[canon].symbol_name.clone();
            symbol_lookup.insert(func.symbol_name.clone(), name);
        }
    }

    rewrite_call_targets(&mut kept, &symbol_lookup);

    kept
}

/// Collapses known helper roots (`store_field`, `to_word`) to a single stable name and
/// returns the rewritten function list.
fn dedup_runtime_helpers<'db>(functions: Vec<MirFunction<'db>>) -> Vec<MirFunction<'db>> {
    let mut root_counts: FxHashMap<String, usize> = FxHashMap::default();
    for func in &functions {
        let root = func
            .symbol_name
            .split("__")
            .next()
            .unwrap_or_default()
            .to_string();
        if HELPER_ROOTS.contains(&root.as_str()) {
            *root_counts.entry(root).or_default() += 1;
        }
    }

    let mut alias_map: FxHashMap<String, String> = FxHashMap::default();
    let mut kept = Vec::new();

    for mut func in functions {
        let root = func
            .symbol_name
            .split("__")
            .next()
            .unwrap_or_default()
            .to_string();
        if HELPER_ROOTS.contains(&root.as_str())
            && root_counts.get(&root).copied().unwrap_or(0) == 1
        {
            let canonical = format!("{root}__deduped");
            alias_map.insert(func.symbol_name.clone(), canonical.clone());
            func.symbol_name = canonical;
        } else {
            alias_map
                .entry(func.symbol_name.clone())
                .or_insert(func.symbol_name.clone());
        }
        kept.push(func);
    }

    rewrite_call_targets(&mut kept, &alias_map);

    kept
}

/// Only dedup compiler-owned helpers (core/external ingots) to avoid altering user ABI,
/// returning `true` when the function qualifies for deduplication.
fn is_dedup_candidate<'db>(db: &'db dyn HirAnalysisDb, func: &MirFunction<'db>) -> bool {
    let hir_func = match func.origin {
        crate::ir::MirFunctionOrigin::Hir(func) => func,
        crate::ir::MirFunctionOrigin::Synthetic(_) => return false,
    };
    matches!(
        hir_func.top_mod(db).ingot(db).kind(db),
        IngotKind::Core | IngotKind::External
    )
}

/// Builds adjacency lists for the call graph so we can hash in dependency order and
/// returns one vector of callee indices per function.
fn build_call_edges<'db>(
    functions: &[MirFunction<'db>],
    symbol_to_idx: &FxHashMap<String, usize>,
) -> Vec<Vec<usize>> {
    functions
        .iter()
        .map(|func| call_edge_targets(func, symbol_to_idx))
        .collect()
}

/// Returns a reverse topological order of the call graph (leaf functions first).
fn topo_order(edges: &[Vec<usize>]) -> Vec<usize> {
    let n = edges.len();
    let mut indegree = vec![0usize; n];
    for targets in edges {
        for &target in targets {
            indegree[target] += 1;
        }
    }

    let mut queue = VecDeque::new();
    for (idx, &deg) in indegree.iter().enumerate() {
        if deg == 0 {
            queue.push_back(idx);
        }
    }

    let mut order = Vec::with_capacity(n);
    let mut seen = vec![false; n];
    while let Some(node) = queue.pop_front() {
        if seen[node] {
            continue;
        }
        seen[node] = true;
        order.push(node);
        for &target in &edges[node] {
            indegree[target] -= 1;
            if indegree[target] == 0 {
                queue.push_back(target);
            }
        }
    }

    if order.len() != n {
        for (idx, was_seen) in seen.iter().enumerate() {
            if !was_seen {
                order.push(idx);
            }
        }
    }

    order
}

/// Collect all callee indices referenced by `func` and return them as a new vector.
fn call_edge_targets<'db>(
    func: &MirFunction<'db>,
    symbol_to_idx: &FxHashMap<String, usize>,
) -> Vec<usize> {
    let mut callees = Vec::new();
    for block in &func.body.blocks {
        for inst in &block.insts {
            if let crate::MirInst::Assign {
                rvalue: crate::ir::Rvalue::Call(call),
                ..
            } = inst
                && let Some(name) = &call.resolved_name
                && let Some(&idx) = symbol_to_idx.get(name)
            {
                callees.push(idx);
            }
        }

        if let crate::Terminator::TerminatingCall(crate::ir::TerminatingCall::Call(call)) =
            &block.terminator
            && let Some(name) = &call.resolved_name
            && let Some(&idx) = symbol_to_idx.get(name)
        {
            callees.push(idx);
        }
    }
    callees
}

/// Applies the canonical call name mapping to every MIR call origin in-place.
fn rewrite_call_targets<'db>(
    functions: &mut [MirFunction<'db>],
    aliases: &FxHashMap<String, String>,
) {
    for func in functions {
        for block in &mut func.body.blocks {
            for inst in &mut block.insts {
                if let crate::MirInst::Assign {
                    rvalue: crate::ir::Rvalue::Call(call),
                    ..
                } = inst
                    && let Some(alias) = canonical_call_name(&call.resolved_name, aliases)
                {
                    call.resolved_name = Some(alias);
                }
            }

            if let crate::Terminator::TerminatingCall(crate::ir::TerminatingCall::Call(call)) =
                &mut block.terminator
                && let Some(alias) = canonical_call_name(&call.resolved_name, aliases)
            {
                call.resolved_name = Some(alias);
            }
        }
    }
}

/// Computes the canonical call name, returning the alias when one exists.
fn canonical_call_name(
    name: &Option<String>,
    aliases: &FxHashMap<String, String>,
) -> Option<String> {
    name.as_ref().and_then(|curr| {
        aliases.get(curr).and_then(|alias| {
            if alias != curr {
                Some(alias.clone())
            } else {
                None
            }
        })
    })
}
