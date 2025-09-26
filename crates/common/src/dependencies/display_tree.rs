use petgraph::graph::{DiGraph, NodeIndex};
use std::collections::{HashMap, HashSet};
use url::Url;

use super::DependencyArguments;
use crate::config::Config;
use smol_str::SmolStr;

#[derive(Clone)]
enum TreePrefix {
    Root,
    Fork(String),
    Last(String),
}

impl TreePrefix {
    fn new_prefix(&self) -> String {
        match self {
            TreePrefix::Root => "".to_string(),
            TreePrefix::Fork(p) => format!("{p}├── "),
            TreePrefix::Last(p) => format!("{p}└── "),
        }
    }

    fn child_indent(&self) -> String {
        match self {
            TreePrefix::Root => "".to_string(),
            TreePrefix::Fork(p) => format!("{p}│   "),
            TreePrefix::Last(p) => format!("{p}    "),
        }
    }
}

pub fn display_tree(
    graph: &DiGraph<Url, (SmolStr, DependencyArguments)>,
    root_url: &Url,
    configs: &HashMap<Url, Config>,
) -> String {
    let mut output = String::new();

    let cycle_nodes = find_cycle_nodes(graph);

    if let Some(root_idx) = graph.node_indices().find(|i| graph[*i] == *root_url) {
        let context = TreeContext {
            graph,
            configs,
            cycle_nodes: &cycle_nodes,
        };
        let mut seen = HashSet::new();
        print_node_with_alias(
            &context,
            root_idx,
            TreePrefix::Root,
            &mut output,
            &mut seen,
            None,
        );
    } else {
        output.push_str("[error: root node not found]\n");
    }

    output
}

struct TreeContext<'a> {
    graph: &'a DiGraph<Url, (SmolStr, DependencyArguments)>,
    configs: &'a HashMap<Url, Config>,
    cycle_nodes: &'a HashSet<NodeIndex>,
}

fn print_node_with_alias(
    context: &TreeContext,
    node: NodeIndex,
    prefix: TreePrefix,
    output: &mut String,
    seen: &mut HashSet<NodeIndex>,
    alias: Option<&str>,
) {
    let ingot_path = &context.graph[node];

    // Build the label with alias support
    let base_label = if let Some(config) = context.configs.get(ingot_path) {
        let ingot_name = config.metadata.name.as_deref().unwrap_or("null");
        let version = config
            .metadata
            .version
            .as_ref()
            .map(ToString::to_string)
            .unwrap_or_else(|| "null".to_string());

        // Show "ingot_name as alias" if alias differs from ingot name
        match alias {
            Some(alias_str) if alias_str != ingot_name => {
                format!("➖ {ingot_name} as {alias_str} v{version}")
            }
            _ => format!("➖ {ingot_name} v{version}"),
        }
    } else {
        "➖ [invalid fe.toml]".to_string()
    };

    let is_in_cycle = context.cycle_nodes.contains(&node);
    let will_close_cycle = seen.contains(&node);

    let mut label = base_label;
    if will_close_cycle {
        label = format!("{label} 🔄 [cycle]");
    }

    if is_in_cycle {
        output.push_str(&format!("{}{}\n", prefix.new_prefix(), red(&label)));
    } else {
        output.push_str(&format!("{}{}\n", prefix.new_prefix(), label));
    }

    if will_close_cycle {
        return;
    }

    seen.insert(node);

    // Process children with alias information from edges
    use petgraph::visit::EdgeRef;
    let children: Vec<_> = context
        .graph
        .edges_directed(node, petgraph::Direction::Outgoing)
        .collect();

    for (i, edge) in children.iter().enumerate() {
        let child_prefix = if i == children.len() - 1 {
            TreePrefix::Last(prefix.child_indent())
        } else {
            TreePrefix::Fork(prefix.child_indent())
        };

        print_node_with_alias(
            context,
            edge.target(),
            child_prefix,
            output,
            seen,
            Some(&edge.weight().0),
        );
    }

    seen.remove(&node);
}

fn red(s: &str) -> String {
    format!("\x1b[31m{s}\x1b[0m")
}

fn find_cycle_nodes<N, E>(graph: &DiGraph<N, E>) -> HashSet<NodeIndex> {
    use petgraph::algo::kosaraju_scc;

    let mut cycles = HashSet::new();
    for scc in kosaraju_scc(graph) {
        if scc.len() > 1 {
            cycles.extend(scc);
        } else {
            let node = scc[0];
            if graph
                .neighbors_directed(node, petgraph::Direction::Outgoing)
                .any(|n| n == node)
            {
                cycles.insert(node); // self-loop
            }
        }
    }
    cycles
}
