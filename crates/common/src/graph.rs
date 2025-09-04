use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::{Dfs, EdgeRef};
use salsa::Setter;
use smol_str::SmolStr;
use std::collections::HashMap;
use url::Url;

use crate::{InputDb, config::DependencyArguments};

#[salsa::input]
#[derive(Debug)]
pub struct Graph {
    graph: DiGraph<Url, EdgeWeight>,
    node_map: HashMap<Url, NodeIndex>,
}

#[derive(Debug, Clone)]
pub struct JoinEdge {
    pub origin: Url,
    pub destination: Url,
    pub weight: EdgeWeight,
}

#[derive(Clone, Debug)]
pub struct EdgeWeight {
    pub alias: SmolStr,
    pub arguments: DependencyArguments,
}

#[salsa::tracked]
impl Graph {
    pub fn default(db: &dyn InputDb) -> Self {
        Graph::new(db, DiGraph::new(), HashMap::new())
    }

    pub fn contains_url(&self, db: &dyn InputDb, url: &Url) -> bool {
        self.node_map(db).contains_key(url)
    }

    /// Returns a subgraph containing all cyclic nodes and all nodes that lead to cycles.
    ///
    /// This method identifies strongly connected components (SCCs) in the graph and returns
    /// a subgraph that includes:
    /// - All nodes that are part of multi-node cycles
    /// - All nodes that have paths leading to cyclic nodes
    ///
    /// Returns an empty graph if no cycles are detected.
    pub fn cyclic_subgraph(&self, db: &dyn InputDb) -> DiGraph<Url, EdgeWeight> {
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

    pub fn join_graph(
        &self,
        db: &mut dyn InputDb,
        other: DiGraph<Url, EdgeWeight>,
        join_edges: Vec<JoinEdge>,
    ) {
        let current_graph = self.graph(db);
        let combined_graph = join_graphs(&current_graph, &other, &join_edges);

        // Build new node map
        let mut new_node_map = HashMap::new();
        for node_idx in combined_graph.node_indices() {
            let url = &combined_graph[node_idx];
            new_node_map.insert(url.clone(), node_idx);
        }

        self.set_graph(db).to(combined_graph);
        self.set_node_map(db).to(new_node_map);
    }

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

    pub fn add_node(&self, db: &mut dyn InputDb, url: Url) -> NodeIndex {
        let mut graph = self.graph(db);
        let mut node_map = self.node_map(db);

        if let Some(&existing_idx) = node_map.get(&url) {
            return existing_idx;
        }

        let new_idx = graph.add_node(url.clone());
        node_map.insert(url, new_idx);

        self.set_graph(db).to(graph);
        self.set_node_map(db).to(node_map);

        new_idx
    }

    pub fn add_edge(&self, db: &mut dyn InputDb, from: &Url, to: &Url, weight: EdgeWeight) -> bool {
        let node_map = self.node_map(db);

        if let (Some(&from_idx), Some(&to_idx)) = (node_map.get(from), node_map.get(to)) {
            let mut graph = self.graph(db);
            graph.add_edge(from_idx, to_idx, weight);
            self.set_graph(db).to(graph);
            true
        } else {
            false
        }
    }
}

fn join_graphs(
    graph1: &DiGraph<Url, EdgeWeight>,
    graph2: &DiGraph<Url, EdgeWeight>,
    join_edges: &[JoinEdge],
) -> DiGraph<Url, EdgeWeight> {
    let mut combined = DiGraph::new();
    let mut node_map = HashMap::<Url, NodeIndex>::new();

    // Insert graph1 nodes
    for idx in graph1.node_indices() {
        let url = &graph1[idx];
        if !node_map.contains_key(url) {
            let new_idx = combined.add_node(url.clone());
            node_map.insert(url.clone(), new_idx);
        }
    }

    // Insert graph2 nodes (avoid duplicates)
    for idx in graph2.node_indices() {
        let url = &graph2[idx];
        if !node_map.contains_key(url) {
            let new_idx = combined.add_node(url.clone());
            node_map.insert(url.clone(), new_idx);
        }
    }

    // Insert graph1 edges
    for edge in graph1.edge_references() {
        let source_url = &graph1[edge.source()];
        let target_url = &graph1[edge.target()];
        if let (Some(&from), Some(&to)) = (node_map.get(source_url), node_map.get(target_url)) {
            combined.add_edge(from, to, edge.weight().clone());
        }
    }

    // Insert graph2 edges
    for edge in graph2.edge_references() {
        let source_url = &graph2[edge.source()];
        let target_url = &graph2[edge.target()];
        if let (Some(&from), Some(&to)) = (node_map.get(source_url), node_map.get(target_url)) {
            combined.add_edge(from, to, edge.weight().clone());
        }
    }

    // Insert join edges (connecting graph1 and graph2)
    for join_edge in join_edges {
        if let (Some(&from), Some(&to)) = (
            node_map.get(&join_edge.origin),
            node_map.get(&join_edge.destination),
        ) {
            combined.add_edge(from, to, join_edge.weight.clone());
        }
    }

    combined
}
