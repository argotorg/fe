#![allow(clippy::too_many_arguments)] // salsa-generated input constructor takes multiple fields

use std::collections::{HashMap, HashSet};

use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::{Dfs, EdgeRef};
use salsa::Setter;
use url::Url;

use super::{DependencyAlias, DependencyArguments, RemoteFiles};
use crate::InputDb;

type EdgeWeight = (DependencyAlias, DependencyArguments);

#[salsa::input]
#[derive(Debug)]
pub struct DependencyGraph {
    graph: DiGraph<Url, EdgeWeight>,
    node_map: HashMap<Url, NodeIndex>,
    git_locations: HashMap<Url, RemoteFiles>,
    reverse_git_map: HashMap<RemoteFiles, Url>,
}

#[salsa::tracked]
impl DependencyGraph {
    pub fn default(db: &dyn InputDb) -> Self {
        DependencyGraph::new(
            db,
            DiGraph::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        )
    }

    fn allocate_node(
        graph: &mut DiGraph<Url, EdgeWeight>,
        node_map: &mut HashMap<Url, NodeIndex>,
        url: &Url,
    ) -> NodeIndex {
        if let Some(&idx) = node_map.get(url) {
            idx
        } else {
            let idx = graph.add_node(url.clone());
            node_map.insert(url.clone(), idx);
            idx
        }
    }

    pub fn ensure_node(&self, db: &mut dyn InputDb, url: &Url) {
        if self.node_map(db).contains_key(url) {
            return;
        }
        let mut graph = self.graph(db);
        let mut node_map = self.node_map(db);
        Self::allocate_node(&mut graph, &mut node_map, url);
        self.set_graph(db).to(graph);
        self.set_node_map(db).to(node_map);
    }

    pub fn contains_url(&self, db: &dyn InputDb, url: &Url) -> bool {
        self.node_map(db).contains_key(url)
    }

    pub fn add_dependency(
        &self,
        db: &mut dyn InputDb,
        source: &Url,
        target: &Url,
        alias: DependencyAlias,
        arguments: DependencyArguments,
    ) {
        let mut graph = self.graph(db);
        let mut node_map = self.node_map(db);
        let source_idx = Self::allocate_node(&mut graph, &mut node_map, source);
        let target_idx = Self::allocate_node(&mut graph, &mut node_map, target);
        graph.add_edge(source_idx, target_idx, (alias, arguments));
        self.set_graph(db).to(graph);
        self.set_node_map(db).to(node_map);
    }

    pub fn petgraph(&self, db: &dyn InputDb) -> DiGraph<Url, EdgeWeight> {
        self.graph(db)
    }

    pub fn cyclic_subgraph(&self, db: &dyn InputDb) -> DiGraph<Url, EdgeWeight> {
        use petgraph::algo::tarjan_scc;

        let graph = self.graph(db);
        let sccs = tarjan_scc(&graph);

        let mut cyclic_nodes = HashSet::new();
        for scc in sccs {
            if scc.len() > 1 {
                for node_idx in scc {
                    cyclic_nodes.insert(node_idx);
                }
            }
        }

        if cyclic_nodes.is_empty() {
            return DiGraph::new();
        }

        let mut nodes_to_include = cyclic_nodes.clone();
        let mut visited = HashSet::new();
        let mut queue: Vec<NodeIndex> = cyclic_nodes.iter().copied().collect();

        while let Some(current) = queue.pop() {
            if !visited.insert(current) {
                continue;
            }
            nodes_to_include.insert(current);

            for pred in graph.node_indices() {
                if graph.find_edge(pred, current).is_some() && !visited.contains(&pred) {
                    queue.push(pred);
                }
            }
        }

        let mut subgraph = DiGraph::new();
        let mut node_map = HashMap::new();

        for &node_idx in &nodes_to_include {
            let url = &graph[node_idx];
            let new_idx = subgraph.add_node(url.clone());
            node_map.insert(node_idx, new_idx);
        }

        for edge in graph.edge_references() {
            if let (Some(&from_new), Some(&to_new)) =
                (node_map.get(&edge.source()), node_map.get(&edge.target()))
            {
                subgraph.add_edge(from_new, to_new, edge.weight().clone());
            }
        }

        subgraph
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

    pub fn register_remote_checkout(
        &self,
        db: &mut dyn InputDb,
        local_url: Url,
        remote: RemoteFiles,
    ) {
        let mut git_map = self.git_locations(db);
        git_map.insert(local_url.clone(), remote.clone());
        self.set_git_locations(db).to(git_map);

        let mut reverse = self.reverse_git_map(db);
        reverse.insert(remote, local_url);
        self.set_reverse_git_map(db).to(reverse);
    }

    pub fn remote_git_for_local(&self, db: &dyn InputDb, local_url: &Url) -> Option<RemoteFiles> {
        self.git_locations(db).get(local_url).cloned()
    }

    pub fn local_for_remote_git(&self, db: &dyn InputDb, remote: &RemoteFiles) -> Option<Url> {
        self.reverse_git_map(db).get(remote).cloned()
    }
}
