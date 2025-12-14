use rustc_hash::{FxHashMap, FxHashSet};

use super::call_graph::{build_call_graph, reachable_functions};
use crate::{
    MirFunction, MirInst, Rvalue, ValueOrigin,
    ir::{ContractFunctionKind, IntrinsicOp},
};

/// Which contract code object is being analyzed/emitted.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ContractRegionKind {
    Init,
    Deployed,
}

/// Identifies a specific code region of a contract.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ContractRegion {
    pub contract_name: String,
    pub kind: ContractRegionKind,
}

#[derive(Debug, Clone, Default)]
pub struct ContractInfo {
    pub init_symbol: Option<String>,
    pub deployed_symbol: Option<String>,
}

/// Program-level contract/code-region dependency information derived from monomorphized MIR.
#[derive(Debug, Clone, Default)]
pub struct ContractGraph {
    /// Per-contract entrypoint symbols.
    pub contracts: FxHashMap<String, ContractInfo>,
    /// Maps monomorphized contract entrypoint symbols to their region identity.
    pub symbol_to_region: FxHashMap<String, ContractRegion>,
    /// The set of reachable function symbols per contract region (includes the root).
    pub region_reachable: FxHashMap<ContractRegion, FxHashSet<String>>,
    /// Contract region dependencies induced by `code_region_offset/len` usage in reachable code.
    pub region_deps: FxHashMap<ContractRegion, FxHashSet<ContractRegion>>,
}

/// Builds a program-level contract graph by:
/// - collecting contract init/runtime roots
/// - computing call-graph reachability for each region root
/// - scanning reachable functions for `code_region_offset/len` references
pub fn build_contract_graph(functions: &[MirFunction<'_>]) -> ContractGraph {
    let call_graph = build_call_graph(functions);
    let funcs_by_symbol: FxHashMap<_, _> = functions
        .iter()
        .map(|func| (func.symbol_name.as_str(), func))
        .collect();

    let mut graph = ContractGraph::default();
    for func in functions {
        let Some(contract_fn) = &func.contract_function else {
            continue;
        };
        let entry = graph
            .contracts
            .entry(contract_fn.contract_name.clone())
            .or_default();
        match contract_fn.kind {
            ContractFunctionKind::Init => {
                if let Some(existing) = &entry.init_symbol
                    && existing != &func.symbol_name
                {
                    panic!(
                        "multiple contract init entrypoints for `{}`: `{}` and `{}`",
                        contract_fn.contract_name, existing, func.symbol_name
                    );
                }
                entry.init_symbol = Some(func.symbol_name.clone());
            }
            ContractFunctionKind::Runtime => {
                if let Some(existing) = &entry.deployed_symbol
                    && existing != &func.symbol_name
                {
                    panic!(
                        "multiple contract runtime entrypoints for `{}`: `{}` and `{}`",
                        contract_fn.contract_name, existing, func.symbol_name
                    );
                }
                entry.deployed_symbol = Some(func.symbol_name.clone());
            }
        }
    }

    for (contract_name, entry) in &graph.contracts {
        if let Some(sym) = &entry.init_symbol {
            graph.symbol_to_region.insert(
                sym.clone(),
                ContractRegion {
                    contract_name: contract_name.clone(),
                    kind: ContractRegionKind::Init,
                },
            );
        }
        if let Some(sym) = &entry.deployed_symbol {
            graph.symbol_to_region.insert(
                sym.clone(),
                ContractRegion {
                    contract_name: contract_name.clone(),
                    kind: ContractRegionKind::Deployed,
                },
            );
        }
    }

    // Compute reachability + region deps for each contract region root.
    for (contract_name, entry) in &graph.contracts {
        if let Some(root) = &entry.init_symbol {
            let region = ContractRegion {
                contract_name: contract_name.clone(),
                kind: ContractRegionKind::Init,
            };
            let reachable = reachable_functions(&call_graph, root);
            graph
                .region_reachable
                .insert(region.clone(), reachable.clone());
            graph.region_deps.insert(
                region.clone(),
                collect_region_deps(
                    &reachable,
                    &funcs_by_symbol,
                    &graph.symbol_to_region,
                    &region,
                ),
            );
        }

        if let Some(root) = &entry.deployed_symbol {
            let region = ContractRegion {
                contract_name: contract_name.clone(),
                kind: ContractRegionKind::Deployed,
            };
            let reachable = reachable_functions(&call_graph, root);
            graph
                .region_reachable
                .insert(region.clone(), reachable.clone());
            graph.region_deps.insert(
                region.clone(),
                collect_region_deps(
                    &reachable,
                    &funcs_by_symbol,
                    &graph.symbol_to_region,
                    &region,
                ),
            );
        }
    }

    graph
}

fn collect_region_deps(
    reachable: &FxHashSet<String>,
    funcs_by_symbol: &FxHashMap<&str, &MirFunction<'_>>,
    symbol_to_region: &FxHashMap<String, ContractRegion>,
    current_region: &ContractRegion,
) -> FxHashSet<ContractRegion> {
    let mut deps = FxHashSet::default();
    for symbol in reachable {
        let Some(func) = funcs_by_symbol.get(symbol.as_str()).copied() else {
            continue;
        };
        for block in &func.body.blocks {
            for inst in &block.insts {
                match inst {
                    MirInst::Assign {
                        rvalue:
                            Rvalue::Intrinsic {
                                op: IntrinsicOp::CodeRegionLen | IntrinsicOp::CodeRegionOffset,
                                args,
                            },
                        ..
                    } => {
                        let arg = args.first().unwrap();
                        let ValueOrigin::FuncItem(target) = &func.body.value(*arg).origin else {
                            continue;
                        };
                        let Some(target_symbol) = &target.symbol else {
                            continue;
                        };
                        let Some(dep_region) = symbol_to_region.get(target_symbol) else {
                            continue;
                        };
                        if dep_region == current_region {
                            continue;
                        }
                        deps.insert(dep_region.clone());
                    }
                    _ => continue,
                }
            }
        }
    }
    deps
}
