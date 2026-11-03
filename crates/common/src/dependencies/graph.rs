#![allow(clippy::too_many_arguments)] // salsa-generated input constructor takes multiple fields

use std::collections::{HashMap, HashSet};

use petgraph::Direction::Incoming;
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::{Dfs, EdgeRef};
use salsa::Setter;
use smol_str::SmolStr;
use url::Url;

use super::{DependencyAlias, DependencyArguments, RemoteFiles, WorkspaceMemberRecord};
use crate::{InputDb, config::ArithmeticMode, ingot::Version};

type EdgeWeight = (DependencyAlias, DependencyArguments);

#[salsa::input]
#[derive(Debug)]
pub struct DependencyGraph {
    graph: DiGraph<Url, EdgeWeight>,
    node_map: HashMap<Url, NodeIndex>,
    git_locations: HashMap<Url, RemoteFiles>,
    reverse_git_map: HashMap<RemoteFiles, Url>,
    ingots_by_metadata: HashMap<(SmolStr, Version), Url>,
    workspace_members: HashMap<Url, Vec<WorkspaceMemberRecord>>,
    workspace_root_by_member: HashMap<Url, Url>,
    expected_member_metadata: HashMap<Url, (SmolStr, Version)>,
    forced_dependency_arithmetic: HashMap<Url, ArithmeticMode>,
    /// The ingots and workspaces a build was asked for, not reached as
    /// dependencies.
    root_ingots: HashSet<Url>,
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
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashSet::new(),
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
        let mut graph = self.graph(db).clone();
        let mut node_map = self.node_map(db).clone();
        Self::allocate_node(&mut graph, &mut node_map, url);
        self.set_graph(db).to(graph);
        self.set_node_map(db).to(node_map);
    }

    pub fn register_ingot_metadata(
        &self,
        db: &mut dyn InputDb,
        url: &Url,
        name: SmolStr,
        version: Version,
    ) {
        let mut map = self.ingots_by_metadata(db).clone();
        map.entry((name, version)).or_insert_with(|| url.clone());
        self.set_ingots_by_metadata(db).to(map);
    }

    pub fn ingot_by_name_version(
        &self,
        db: &dyn InputDb,
        name: &SmolStr,
        version: &Version,
    ) -> Option<Url> {
        self.ingots_by_metadata(db)
            .get(&(name.clone(), version.clone()))
            .cloned()
    }

    pub fn register_workspace_member(
        &self,
        db: &mut dyn InputDb,
        workspace_root: &Url,
        member: WorkspaceMemberRecord,
    ) {
        let mut members = self.workspace_members(db).clone();
        let entry = members.entry(workspace_root.clone()).or_default();
        if let Some(existing) = entry.iter_mut().find(|record| record.url == member.url) {
            *existing = member.clone();
        } else {
            entry.push(member.clone());
        }
        self.set_workspace_members(db).to(members);

        self.register_workspace_member_root(db, workspace_root, &member.url);
    }

    pub fn register_workspace_member_root(
        &self,
        db: &mut dyn InputDb,
        workspace_root: &Url,
        member_url: &Url,
    ) {
        let mut roots = self.workspace_root_by_member(db).clone();
        roots.insert(member_url.clone(), workspace_root.clone());
        self.set_workspace_root_by_member(db).to(roots);
    }

    pub fn workspace_member_records(
        &self,
        db: &dyn InputDb,
        workspace_root: &Url,
    ) -> Vec<WorkspaceMemberRecord> {
        self.workspace_members(db)
            .get(workspace_root)
            .cloned()
            .unwrap_or_default()
    }

    pub fn workspace_roots(&self, db: &dyn InputDb) -> Vec<Url> {
        self.workspace_members(db).keys().cloned().collect()
    }

    /// Ensure the workspace root has an entry in the members map even when
    /// it has zero named members (e.g. a workspace with only glob patterns
    /// that match nothing yet).  Without this the root would be invisible to
    /// `workspace_roots()`.
    pub fn ensure_workspace_root(&self, db: &mut dyn InputDb, workspace_root: &Url) {
        let mut members = self.workspace_members(db).clone();
        if members.contains_key(workspace_root) {
            return;
        }
        members.entry(workspace_root.clone()).or_default();
        self.set_workspace_members(db).to(members);
    }

    pub fn workspace_members_by_name(
        &self,
        db: &dyn InputDb,
        workspace_root: &Url,
        name: &SmolStr,
    ) -> Vec<WorkspaceMemberRecord> {
        self.workspace_members(db)
            .get(workspace_root)
            .map(|members| {
                members
                    .iter()
                    .filter(|member| member.name == *name)
                    .cloned()
                    .collect()
            })
            .unwrap_or_default()
    }

    pub fn workspace_root_for_member(&self, db: &dyn InputDb, url: &Url) -> Option<Url> {
        self.workspace_root_by_member(db).get(url).cloned()
    }

    pub fn register_expected_member_metadata(
        &self,
        db: &mut dyn InputDb,
        url: &Url,
        name: SmolStr,
        version: Version,
    ) {
        let mut map = self.expected_member_metadata(db).clone();
        map.insert(url.clone(), (name, version));
        self.set_expected_member_metadata(db).to(map);
    }

    pub fn expected_member_metadata_for(
        &self,
        db: &dyn InputDb,
        url: &Url,
    ) -> Option<(SmolStr, Version)> {
        self.expected_member_metadata(db).get(url).cloned()
    }

    pub fn contains_url(&self, db: &dyn InputDb, url: &Url) -> bool {
        self.node_map(db).contains_key(url)
    }

    /// Records `url` as an ingot or workspace a build was asked for.
    pub fn register_root_ingot(&self, db: &mut dyn InputDb, url: &Url) {
        let mut roots = self.root_ingots(db).clone();
        if roots.insert(url.clone()) {
            self.set_root_ingots(db).to(roots);
        }
    }

    /// Whether `url` is the user's own code rather than a dependency: an
    /// ingot a build was asked for or a member of its workspace, an ingot
    /// nothing in the graph depends on, or one the graph does not know (a
    /// standalone file). A member of a dependency's own workspace is a
    /// dependency.
    pub fn is_trust_root(&self, db: &dyn InputDb, url: &Url) -> bool {
        let Some(&idx) = self.node_map(db).get(url) else {
            return true;
        };
        let members = self.workspace_root_by_member(db);
        let workspace = |url: &Url| members.get(url).unwrap_or(url).clone();
        self.root_ingots(db)
            .iter()
            .any(|root| root == url || workspace(root) == workspace(url))
            || self
                .graph(db)
                .edges_directed(idx, Incoming)
                .next()
                .is_none()
    }

    /// The ingots that depend on `url`, each with whether it declares the
    /// dependency with `allow_unsafe = true`.
    pub fn dependents(&self, db: &dyn InputDb, url: &Url) -> Vec<(Url, bool)> {
        let Some(&idx) = self.node_map(db).get(url) else {
            return Vec::new();
        };
        let graph = self.graph(db);
        graph
            .edges_directed(idx, Incoming)
            .map(|edge| (graph[edge.source()].clone(), edge.weight().1.allow_unsafe))
            .collect()
    }

    /// The ingots whose unsafe code trusted code may use: the trust roots,
    /// `core` and `std`, and every dependency that a trusted ingot declares
    /// with `allow_unsafe = true`. An untrusted ingot's `allow_unsafe` grants
    /// nothing.
    pub fn trusted_ingots(&self, db: &dyn InputDb) -> HashSet<Url> {
        let graph = self.graph(db);
        let mut pending: Vec<_> = graph
            .node_indices()
            .filter(|&idx| {
                matches!(graph[idx].scheme(), "builtin-core" | "builtin-std")
                    || self.is_trust_root(db, &graph[idx])
            })
            .collect();
        let mut trusted = HashSet::new();
        while let Some(idx) = pending.pop() {
            if trusted.insert(graph[idx].clone()) {
                pending.extend(
                    graph
                        .edges(idx)
                        .filter(|edge| edge.weight().1.allow_unsafe)
                        .map(|edge| edge.target()),
                );
            }
        }
        trusted
    }

    pub fn force_dependency_arithmetic(
        &self,
        db: &mut dyn InputDb,
        url: &Url,
        arithmetic: ArithmeticMode,
    ) {
        let mut forced = self.forced_dependency_arithmetic(db).clone();
        forced.insert(url.clone(), arithmetic);
        self.set_forced_dependency_arithmetic(db).to(forced);
    }

    pub fn forced_dependency_arithmetic_for(
        &self,
        db: &dyn InputDb,
        url: &Url,
    ) -> Option<ArithmeticMode> {
        self.forced_dependency_arithmetic(db).get(url).copied()
    }

    pub fn add_dependency(
        &self,
        db: &mut dyn InputDb,
        source: &Url,
        target: &Url,
        alias: DependencyAlias,
        arguments: DependencyArguments,
    ) {
        let mut graph = self.graph(db).clone();
        let mut node_map = self.node_map(db).clone();
        let source_idx = Self::allocate_node(&mut graph, &mut node_map, source);
        let target_idx = Self::allocate_node(&mut graph, &mut node_map, target);

        // Avoid duplicate edges when re-resolving (e.g. workspace re-init after
        // a new member ingot appears).  If an edge with the same alias already
        // exists, update its arguments in case they changed.
        let existing = graph
            .edges(source_idx)
            .find(|e| e.target() == target_idx && e.weight().0 == alias)
            .map(|e| e.id());
        if let Some(edge_id) = existing {
            if graph[edge_id].1 != arguments {
                graph[edge_id] = (alias, arguments);
            }
        } else {
            graph.add_edge(source_idx, target_idx, (alias, arguments));
        }

        self.set_graph(db).to(graph);
        self.set_node_map(db).to(node_map);
    }

    pub fn petgraph(&self, db: &dyn InputDb) -> DiGraph<Url, EdgeWeight> {
        self.graph(db).clone()
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

    pub fn direct_dependencies(&self, db: &dyn InputDb, url: &Url) -> Vec<(DependencyAlias, Url)> {
        let node_map = self.node_map(db);
        let graph = self.graph(db);

        let Some(&root) = node_map.get(url) else {
            return Vec::new();
        };

        graph
            .edges(root)
            .map(|edge| {
                let (alias, _arguments) = edge.weight();
                (alias.clone(), graph[edge.target()].clone())
            })
            .collect()
    }

    pub fn register_remote_checkout(
        &self,
        db: &mut dyn InputDb,
        local_url: Url,
        remote: RemoteFiles,
    ) {
        let mut git_map = self.git_locations(db).clone();
        git_map.insert(local_url.clone(), remote.clone());
        self.set_git_locations(db).to(git_map);

        let mut reverse = self.reverse_git_map(db).clone();
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::define_input_db;

    define_input_db!(TestDatabase);

    #[test]
    fn finds_ingot_by_metadata() {
        let mut db = TestDatabase::default();
        let graph = DependencyGraph::default(&db);

        let url = Url::parse("file:///workspace/ingot/").unwrap();
        let version = Version::parse("0.1.0").unwrap();
        graph.register_ingot_metadata(&mut db, &url, "foo".into(), version.clone());

        let found = graph.ingot_by_name_version(&db, &"foo".into(), &version);
        assert_eq!(found, Some(url));
    }

    #[test]
    fn trust_flows_only_from_trusted_ingots() {
        let mut db = TestDatabase::default();
        let graph = DependencyGraph::default(&db);
        let url = |name: &str| Url::parse(&format!("file:///workspace/{name}/")).unwrap();
        let depend = |db: &mut TestDatabase, from: &str, to: &str, allow_unsafe: bool| {
            let arguments = DependencyArguments {
                allow_unsafe,
                ..Default::default()
            };
            graph.add_dependency(db, &url(from), &url(to), to.into(), arguments);
        };
        // root trusts a, which vouches for b; root does not trust c, whose
        // vouching for d therefore grants nothing.
        depend(&mut db, "root", "a", true);
        depend(&mut db, "a", "b", true);
        depend(&mut db, "root", "c", false);
        depend(&mut db, "c", "d", true);

        let trusted = graph.trusted_ingots(&db);
        for name in ["root", "a", "b"] {
            assert!(trusted.contains(&url(name)), "{name} should be trusted");
        }
        for name in ["c", "d"] {
            assert!(
                !trusted.contains(&url(name)),
                "{name} should not be trusted"
            );
        }
        assert!(graph.is_trust_root(&db, &url("root")));
        assert!(!graph.is_trust_root(&db, &url("a")));
        assert_eq!(graph.dependents(&db, &url("d")), vec![(url("c"), true)]);
    }
}
