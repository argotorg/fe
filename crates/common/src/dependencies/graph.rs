use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::{Dfs, EdgeRef};
use salsa::Setter;
use smol_str::SmolStr;
use std::collections::HashMap;
use url::Url;

use super::DependencyArguments;
use crate::InputDb;

#[salsa::input]
#[derive(Debug)]
pub struct DependencyGraph {
    graph: DiGraph<Url, (SmolStr, DependencyArguments)>,
    node_map: HashMap<Url, NodeIndex>,
}

#[salsa::tracked]
impl DependencyGraph {
    pub fn default(db: &dyn InputDb) -> Self {
        DependencyGraph::new(db, DiGraph::new(), HashMap::new())
    }

    /// Returns true if the given URL exists as a node in the dependency graph.
    pub fn contains_url(&self, db: &dyn InputDb, url: &Url) -> bool {
        self.node_map(db).contains_key(url)
    }

    /// Returns all URLs in the dependency graph.
    pub fn all_urls(&self, db: &dyn InputDb) -> Vec<Url> {
        self.node_map(db).keys().cloned().collect()
    }

    /// Returns a subgraph containing all cyclic nodes and all nodes that lead to cycles.
    ///
    /// This method identifies strongly connected components (SCCs) in the graph and returns
    /// a subgraph that includes:
    /// - All nodes that are part of multi-node cycles
    /// - All nodes that have paths leading to cyclic nodes
    ///
    /// Returns an empty graph if no cycles are detected.
    pub fn cyclic_subgraph(
        &self,
        db: &dyn InputDb,
    ) -> DiGraph<Url, (SmolStr, DependencyArguments)> {
        use petgraph::algo::tarjan_scc;
        use std::collections::VecDeque;

        let graph = self.graph(db);
        let sccs = tarjan_scc(&graph);

        // Find actual cyclic nodes (multi-node SCCs only)
        let mut cyclic_nodes = std::collections::HashSet::new();
        for scc in sccs {
            if scc.len() > 1 {
                // Multi-node SCC = actual cycle
                for node_idx in scc {
                    cyclic_nodes.insert(node_idx);
                }
            }
        }

        // If no cycles, return empty graph
        if cyclic_nodes.is_empty() {
            return DiGraph::new();
        }

        // Use reverse DFS from cyclic nodes to find all predecessors
        let mut nodes_to_include = cyclic_nodes.clone();
        let mut visited = std::collections::HashSet::new();
        let mut queue = VecDeque::new();

        // Start from all cyclic nodes and work backwards
        for &cyclic_node in &cyclic_nodes {
            queue.push_back(cyclic_node);
        }

        while let Some(current) = queue.pop_front() {
            if visited.contains(&current) {
                continue;
            }
            visited.insert(current);
            nodes_to_include.insert(current);

            // Add all predecessors (nodes that point to current)
            for pred in graph.node_indices() {
                if graph.find_edge(pred, current).is_some() && !visited.contains(&pred) {
                    queue.push_back(pred);
                }
            }
        }

        // Build subgraph with all nodes that lead to cycles
        let mut subgraph = DiGraph::new();
        let mut node_map = HashMap::new();

        // Add nodes
        for &node_idx in &nodes_to_include {
            let url = &graph[node_idx];
            let new_idx = subgraph.add_node(url.clone());
            node_map.insert(node_idx, new_idx);
        }

        // Add edges between included nodes
        for edge in graph.edge_references() {
            if let (Some(&from_new), Some(&to_new)) =
                (node_map.get(&edge.source()), node_map.get(&edge.target()))
            {
                subgraph.add_edge(from_new, to_new, edge.weight().clone());
            }
        }

        subgraph
    }

    /// Adds a dependency edge from one URL to another.
    ///
    /// Creates nodes for both URLs if they don't already exist in the graph,
    /// then adds a directed edge from `source` to `target` with the given
    /// alias and arguments.
    pub fn add_dependency(
        &self,
        db: &mut dyn InputDb,
        source: &Url,
        target: &Url,
        alias: SmolStr,
        arguments: DependencyArguments,
    ) {
        let mut graph = self.graph(db);
        let mut node_map = self.node_map(db);

        // Add nodes if they don't exist
        let source_node = if let Some(&node) = node_map.get(source) {
            node
        } else {
            let node = graph.add_node(source.clone());
            node_map.insert(source.clone(), node);
            node
        };

        let target_node = if let Some(&node) = node_map.get(target) {
            node
        } else {
            let node = graph.add_node(target.clone());
            node_map.insert(target.clone(), node);
            node
        };

        // Add the edge
        graph.add_edge(source_node, target_node, (alias, arguments));

        // Update the graph state
        self.set_graph(db).to(graph);
        self.set_node_map(db).to(node_map);
    }

    /// Returns all transitive dependencies of the given URL.
    ///
    /// This performs a depth-first search to find all URLs that are reachable
    /// from the given URL through the dependency graph, excluding the URL itself.
    pub fn dependency_urls(&self, db: &dyn InputDb, url: &Url) -> Vec<Url> {
        let node_map = self.node_map(db);
        let graph = self.graph(db);

        if let Some(&root) = node_map.get(url) {
            let mut dfs = Dfs::new(&graph, root);
            let mut visited = Vec::new();

            while let Some(node) = dfs.next(&graph) {
                if node != root {
                    visited.push(graph[node].clone());
                }
            }

            visited
        } else {
            Vec::new()
        }
    }
}
