use std::collections::{HashMap, HashSet};

use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
use smol_str::SmolStr;
use url::Url;

use super::{DependencyAlias, DependencyArguments};
use crate::{
    InputDb,
    config::{Config, IngotConfig},
    ingot::Version,
};

type TreeEdge = (DependencyAlias, DependencyArguments);

pub struct DependencyTree {
    root: Url,
    graph: DiGraph<Url, TreeEdge>,
    configs: HashMap<Url, IngotConfig>,
    remote_edges: HashSet<(Url, Url)>,
}

impl DependencyTree {
    pub fn build(db: &dyn InputDb, root: &Url) -> Self {
        let graph = db.dependency_graph().petgraph(db);
        let configs = collect_configs(db, &graph);
        let remote_edges = collect_remote_edges(db, &graph);

        Self::from_parts(graph, root.clone(), configs, remote_edges)
    }

    pub fn from_parts(
        graph: DiGraph<Url, TreeEdge>,
        root: Url,
        configs: HashMap<Url, IngotConfig>,
        remote_edges: HashSet<(Url, Url)>,
    ) -> Self {
        Self {
            root,
            graph,
            configs,
            remote_edges,
        }
    }

    pub fn display(&self) -> String {
        display_tree(&self.graph, &self.root, &self.configs, &self.remote_edges)
    }
}

fn collect_configs(db: &dyn InputDb, graph: &DiGraph<Url, TreeEdge>) -> HashMap<Url, IngotConfig> {
    let mut configs = HashMap::new();
    let mut workspace_versions: HashMap<Url, Option<Version>> = HashMap::new();
    for node_idx in graph.node_indices() {
        let url = &graph[node_idx];
        if let Some(ingot) = db.workspace().containing_ingot(db, url.clone())
            && let Some(mut config) = ingot.config(db)
        {
            if config.metadata.version.is_none()
                && let Some(workspace_url) =
                    db.dependency_graph().workspace_root_for_member(db, url)
            {
                let version = workspace_versions
                    .entry(workspace_url.clone())
                    .or_insert_with(|| workspace_version(db, &workspace_url));
                if let Some(version) = version.clone() {
                    config.metadata.version = Some(version);
                }
            }
            configs.insert(url.clone(), config);
        }
    }
    configs
}

fn workspace_version(db: &dyn InputDb, workspace_url: &Url) -> Option<Version> {
    let config_url = workspace_url.join("fe.toml").ok()?;
    let file = db.workspace().get(db, &config_url)?;
    let parsed = Config::parse(file.text(db)).ok()?;
    match parsed {
        Config::Workspace(config) => config.workspace.version,
        Config::Ingot(_) => None,
    }
}

fn collect_remote_edges(db: &dyn InputDb, graph: &DiGraph<Url, TreeEdge>) -> HashSet<(Url, Url)> {
    let mut edges = HashSet::new();
    for edge in graph.edge_references() {
        let from = &graph[edge.source()];
        let to = &graph[edge.target()];
        let to_is_remote = db.dependency_graph().remote_git_for_local(db, to).is_some();
        if to_is_remote
            && db
                .dependency_graph()
                .remote_git_for_local(db, from)
                .is_none()
        {
            edges.insert((from.clone(), to.clone()));
        }
    }
    edges
}

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

fn display_tree(
    graph: &DiGraph<Url, (SmolStr, DependencyArguments)>,
    root_url: &Url,
    configs: &HashMap<Url, IngotConfig>,
    remote_edges: &HashSet<(Url, Url)>,
) -> String {
    let mut output = String::new();

    let cycle_nodes = find_cycle_nodes(graph);

    if let Some(root_idx) = graph.node_indices().find(|i| graph[*i] == *root_url) {
        let context = TreeContext {
            graph,
            configs,
            cycle_nodes: &cycle_nodes,
            remote_edges,
        };
        let mut seen = HashSet::new();
        print_node_with_alias(
            &context,
            root_idx,
            TreePrefix::Root,
            &mut output,
            &mut seen,
            None,
            None,
        );
    } else {
        output.push_str("[error: root node not found]\n");
    }

    output
}

struct TreeContext<'a> {
    graph: &'a DiGraph<Url, (SmolStr, DependencyArguments)>,
    configs: &'a HashMap<Url, IngotConfig>,
    cycle_nodes: &'a HashSet<NodeIndex>,
    remote_edges: &'a HashSet<(Url, Url)>,
}

fn print_node_with_alias(
    context: &TreeContext,
    node: NodeIndex,
    prefix: TreePrefix,
    output: &mut String,
    seen: &mut HashSet<NodeIndex>,
    alias: Option<&str>,
    parent_url: Option<&Url>,
) {
    let ingot_path = &context.graph[node];

    // Build the label with alias support (no leading marker; remote marker is appended for local→remote edges)
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
                format!("{ingot_name} as {alias_str} v{version}")
            }
            _ => format!("{ingot_name} v{version}"),
        }
    } else {
        "[invalid fe.toml]".to_string()
    };

    let is_in_cycle = context.cycle_nodes.contains(&node);
    let will_close_cycle = seen.contains(&node);

    let is_remote_edge = parent_url
        .map(|parent| {
            context
                .remote_edges
                .contains(&(parent.clone(), ingot_path.clone()))
        })
        .unwrap_or(false);

    let mut label = base_label;

    if will_close_cycle {
        label = format!("{label} 🔄 [cycle]");
    }

    if is_remote_edge {
        label = format!("{label} 🌐 [remote]");
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
            Some(ingot_path),
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
