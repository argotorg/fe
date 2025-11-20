use std::{collections::HashMap, fs, time::Duration};

use camino::{Utf8Path, Utf8PathBuf};
use common::{
    cache::remote_git_cache_dir,
    config::Config,
    dependencies::{
        DependencyArguments, DependencyLocation, ExternalDependencyEdge, display_tree::display_tree,
    },
};
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use resolver::{
    ResolutionHandler, Resolver,
    files::{FilesResolver, FilesResource},
    git::{GitDependencyDescription, GitResolver, GitResource},
    graph::{
        DiGraph, GraphResolutionHandler, GraphResolver, GraphResolverImpl, petgraph::visit::EdgeRef,
    },
};
use smol_str::SmolStr;
use url::Url;

pub fn print_tree(path: &Utf8PathBuf) {
    let mut resolver = tree_resolver();
    let mut handler = TreeHandler::default();

    let canonical_path = match path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            eprintln!("Error: Invalid or non-existent directory path: {path}");
            return;
        }
    };

    let ingot_url = match Url::from_directory_path(canonical_path.as_str()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid directory path: {path}");
            return;
        }
    };

    match resolver.graph_resolve(&mut handler, &ingot_url) {
        Ok(tree_output) => {
            // Print graph resolver diagnostics (unresolvable nodes)
            for unresolvable_node in resolver.take_diagnostics() {
                eprintln!(
                    "❌ Failed to resolve ingot dependency '{}': {}",
                    unresolvable_node.0, unresolvable_node.1
                );
            }

            // Print files resolver diagnostics
            for diagnostic in resolver.node_resolver.take_diagnostics() {
                eprintln!("❌ File resolution error: {diagnostic}");
            }

            // Print the tree
            print!("{tree_output}");

            let remote_requests = handler.take_remote_dependencies();
            if let Some(remote_output) = resolve_remote_tree(&remote_requests, &ingot_url) {
                print!("{remote_output}");
            }
        }
        Err(err) => {
            // Print diagnostics even on failure
            for unresolvable_node in resolver.take_diagnostics() {
                eprintln!(
                    "❌ Failed to resolve ingot dependency '{}': {}",
                    unresolvable_node.0, unresolvable_node.1
                );
            }

            for diagnostic in resolver.node_resolver.take_diagnostics() {
                eprintln!("❌ File resolution error: {diagnostic}");
            }

            println!("❌ Failed to resolve dependency tree: {err}");

            let remote_requests = handler.take_remote_dependencies();
            if let Some(remote_output) = resolve_remote_tree(&remote_requests, &ingot_url) {
                print!("{remote_output}");
            }
        }
    }
}

pub type IngotGraphResolver =
    resolver::graph::GraphResolverImpl<FilesResolver, TreeHandler, (SmolStr, DependencyArguments)>;

pub fn tree_resolver() -> IngotGraphResolver {
    let files_resolver = FilesResolver::new().with_required_file("fe.toml");
    resolver::graph::GraphResolverImpl::new(files_resolver)
}

#[derive(Default)]
pub struct TreeHandler {
    pub configs: HashMap<Url, Config>,
    pub remote_dependencies: Vec<ExternalDependencyEdge>,
}

impl TreeHandler {
    fn take_remote_dependencies(&mut self) -> Vec<ExternalDependencyEdge> {
        std::mem::take(&mut self.remote_dependencies)
    }
}

impl ResolutionHandler<FilesResolver> for TreeHandler {
    type Item = Vec<(Url, (SmolStr, DependencyArguments))>;

    fn handle_resolution(&mut self, ingot_url: &Url, resource: FilesResource) -> Self::Item {
        tracing::info!(target: "resolver", "Handling ingot resolution for: {}", ingot_url);

        // Look for fe.toml file
        if let Some(config_file) = resource
            .files
            .iter()
            .find(|f| f.path.file_name() == Some("fe.toml"))
        {
            match Config::parse(&config_file.content) {
                Ok(config) => {
                    tracing::info!(target: "resolver", "Successfully parsed config for ingot: {}", ingot_url);

                    // Report config validation diagnostics
                    for diagnostic in &config.diagnostics {
                        eprintln!("❌ Config validation error at {ingot_url}: {diagnostic}");
                    }

                    self.configs.insert(ingot_url.clone(), config.clone());
                    let mut edges = Vec::new();
                    for dependency in config.dependencies(ingot_url) {
                        let alias = dependency.alias;
                        let arguments = dependency.arguments;
                        match dependency.location {
                            DependencyLocation::Local(local) => {
                                tracing::info!(target: "resolver", "Found dependency: {} -> {}", ingot_url, local.url);
                                edges.push((local.url, (alias, arguments)));
                            }
                            DependencyLocation::Remote(remote) => {
                                self.remote_dependencies.push(ExternalDependencyEdge {
                                    parent: ingot_url.clone(),
                                    alias,
                                    arguments,
                                    remote,
                                });
                            }
                        }
                    }
                    edges
                }
                Err(err) => {
                    tracing::warn!(target: "resolver", "Failed to parse config for ingot {}: {}", ingot_url, err);
                    eprintln!("❌ Invalid fe.toml file at {ingot_url}: {err}");
                    vec![]
                }
            }
        } else {
            // This case should not happen since we require fe.toml, but handle it gracefully
            vec![]
        }
    }
}

impl GraphResolutionHandler<Url, DiGraph<Url, (SmolStr, DependencyArguments)>> for TreeHandler {
    type Item = String;

    fn handle_graph_resolution(
        &mut self,
        ingot_url: &Url,
        graph: DiGraph<Url, (SmolStr, DependencyArguments)>,
    ) -> Self::Item {
        display_tree(&graph, ingot_url, &self.configs)
    }
}

fn resolve_remote_tree(requests: &[ExternalDependencyEdge], ingot_url: &Url) -> Option<String> {
    if requests.is_empty() {
        return None;
    }

    let checkout_root = tree_remote_checkout_root(ingot_url);
    let git_resolver = GitResolver::new(checkout_root);
    let mut graph_resolver: GraphResolverImpl<
        GitResolver,
        TreeRemoteHandler,
        (SmolStr, DependencyArguments),
    > = GraphResolverImpl::new(git_resolver);
    let mut handler = TreeRemoteHandler::new();
    let mut output = String::new();
    let multi = MultiProgress::new();
    let spinner_style = ProgressStyle::with_template("{spinner} {msg}")
        .unwrap()
        .tick_strings(&["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]);

    for request in requests {
        let description = tree_git_description_from_request(request);
        let spinner = multi.add(ProgressBar::new_spinner());
        spinner.set_style(spinner_style.clone());
        spinner.enable_steady_tick(Duration::from_millis(80));
        spinner.set_message(format!(
            "Resolving remote dependency {} ({})",
            request.alias, description
        ));
        handler.register_spinner(description.clone(), spinner.clone());
        match graph_resolver.graph_resolve(&mut handler, &description) {
            Ok(graph) => {
                if let Some(root_url) = handler.ingot_url(&description) {
                    let tree = display_tree(&graph, root_url, &handler.configs);
                    output.push_str(&format!(
                        "\nRemote dependency tree for {} ({}):\n{tree}\n",
                        request.alias, description
                    ));
                }
            }
            Err(err) => {
                handler.fail_spinner(&description, &err.to_string());
                output.push_str(&format!(
                    "\n❌ Failed to resolve remote dependency {}: {err}\n",
                    description
                ));
            }
        }
    }
    let _ = multi.clear();

    for diagnostic in handler.take_diagnostics() {
        output.push_str(&format!("\n❌ {diagnostic}\n"));
    }

    for diagnostic in graph_resolver.take_diagnostics() {
        output.push_str(&format!(
            "\n❌ Remote dependency error for {}: {}\n",
            diagnostic.0, diagnostic.1
        ));
    }

    for diagnostic in graph_resolver.node_resolver.take_diagnostics() {
        output.push_str(&format!("\nℹ️ {diagnostic}\n"));
    }

    if output.is_empty() {
        None
    } else {
        Some(output)
    }
}

fn tree_remote_checkout_root(ingot_url: &Url) -> Utf8PathBuf {
    if let Some(root) = remote_git_cache_dir() {
        return root;
    }

    let mut ingot_path = Utf8PathBuf::from_path_buf(
        ingot_url
            .to_file_path()
            .expect("ingot URL should map to a local path"),
    )
    .expect("ingot path should be valid UTF-8");
    ingot_path.push(".fe");
    ingot_path.push("git");
    ingot_path
}

fn tree_git_description_from_request(request: &ExternalDependencyEdge) -> GitDependencyDescription {
    let mut description = GitDependencyDescription::new(
        request.remote.source.clone(),
        request.remote.rev.to_string(),
    );
    if let Some(path) = request.remote.path.clone() {
        description = description.with_path(path);
    }
    description
}

struct TreeRemoteHandler {
    configs: HashMap<Url, Config>,
    ingot_urls: HashMap<GitDependencyDescription, Url>,
    diagnostics: Vec<String>,
    spinners: HashMap<GitDependencyDescription, ProgressBar>,
}

impl TreeRemoteHandler {
    fn new() -> Self {
        Self {
            configs: HashMap::new(),
            ingot_urls: HashMap::new(),
            diagnostics: Vec::new(),
            spinners: HashMap::new(),
        }
    }

    fn ingot_url(&self, description: &GitDependencyDescription) -> Option<&Url> {
        self.ingot_urls.get(description)
    }

    fn take_diagnostics(&mut self) -> Vec<String> {
        std::mem::take(&mut self.diagnostics)
    }

    fn register_spinner(&mut self, description: GitDependencyDescription, spinner: ProgressBar) {
        self.spinners.insert(description, spinner);
    }

    fn finish_spinner_success(&mut self, description: &GitDependencyDescription, ingot_url: &Url) {
        if let Some(spinner) = self.spinners.remove(description) {
            spinner.finish_with_message(format!("✅ Resolved {ingot_url}"));
        }
    }

    fn fail_spinner(&mut self, description: &GitDependencyDescription, message: &str) {
        if let Some(spinner) = self.spinners.remove(description) {
            spinner.abandon_with_message(format!("❌ Failed to resolve {description}: {message}"));
        }
    }
}

impl ResolutionHandler<GitResolver> for TreeRemoteHandler {
    type Item = Vec<(GitDependencyDescription, (SmolStr, DependencyArguments))>;

    fn handle_resolution(
        &mut self,
        description: &GitDependencyDescription,
        resource: GitResource,
    ) -> Self::Item {
        self.ingot_urls
            .insert(description.clone(), resource.ingot_url.clone());

        let config_path = resource.ingot_path.join("fe.toml");
        let config_content = match fs::read_to_string(&config_path) {
            Ok(content) => content,
            Err(error) => {
                self.fail_spinner(description, "missing fe.toml");
                self.diagnostics.push(format!(
                    "Remote file error at {}: {error}",
                    resource.ingot_url
                ));
                return vec![];
            }
        };

        let config = match Config::parse(&config_content) {
            Ok(config) => config,
            Err(error) => {
                self.fail_spinner(description, "invalid fe.toml");
                self.diagnostics.push(format!(
                    "Invalid remote fe.toml file in {}: {error}",
                    resource.ingot_url
                ));
                return vec![];
            }
        };

        if !config.diagnostics.is_empty() {
            for diagnostic in &config.diagnostics {
                self.diagnostics.push(format!(
                    "Erroneous remote fe.toml file in {}: {diagnostic}",
                    resource.ingot_url
                ));
            }
        }

        self.configs
            .insert(resource.ingot_url.clone(), config.clone());
        self.finish_spinner_success(description, &resource.ingot_url);

        let mut dependencies = Vec::new();
        for dependency in config.dependencies(&resource.ingot_url) {
            let alias = dependency.alias;
            let arguments = dependency.arguments;
            match dependency.location {
                DependencyLocation::Local(local) => match tree_relative_path_within_checkout(
                    resource.checkout_path.as_path(),
                    &local.url,
                ) {
                    Ok(relative_path) => {
                        let mut next_description = GitDependencyDescription::new(
                            description.source.clone(),
                            description.rev.clone(),
                        );
                        if let Some(path) = relative_path {
                            next_description = next_description.with_path(path);
                        }
                        dependencies.push((next_description, (alias, arguments)));
                    }
                    Err(error) => {
                        self.diagnostics.push(format!(
                            "Remote dependency '{}' in {} points outside the repository: {error}",
                            alias, resource.ingot_url
                        ));
                    }
                },
                DependencyLocation::Remote(remote) => {
                    let mut next_description = GitDependencyDescription::new(
                        remote.source.clone(),
                        remote.rev.to_string(),
                    );
                    if let Some(path) = remote.path.clone() {
                        next_description = next_description.with_path(path);
                    }
                    dependencies.push((next_description, (alias, arguments)));
                }
            }
        }

        dependencies
    }
}

impl
    GraphResolutionHandler<
        GitDependencyDescription,
        DiGraph<GitDependencyDescription, (SmolStr, DependencyArguments)>,
    > for TreeRemoteHandler
{
    type Item = DiGraph<Url, (SmolStr, DependencyArguments)>;

    fn handle_graph_resolution(
        &mut self,
        _description: &GitDependencyDescription,
        graph: DiGraph<GitDependencyDescription, (SmolStr, DependencyArguments)>,
    ) -> Self::Item {
        let mut converted = DiGraph::new();
        let mut node_map = HashMap::new();

        for node_idx in graph.node_indices() {
            if let Some(url) = self.ingot_urls.get(&graph[node_idx]) {
                let new_idx = converted.add_node(url.clone());
                node_map.insert(node_idx, new_idx);
            }
        }

        for edge in graph.edge_references() {
            if let (Some(&from_idx), Some(&to_idx)) =
                (node_map.get(&edge.source()), node_map.get(&edge.target()))
            {
                converted.add_edge(from_idx, to_idx, edge.weight().clone());
            }
        }

        converted
    }
}

fn tree_relative_path_within_checkout(
    checkout_path: &Utf8Path,
    target_url: &Url,
) -> Result<Option<Utf8PathBuf>, String> {
    let path_buf = target_url
        .to_file_path()
        .map_err(|_| "target URL is not a file URL".to_string())?;
    let utf8_path = Utf8PathBuf::from_path_buf(path_buf)
        .map_err(|_| "non UTF-8 path encountered in remote dependency".to_string())?;
    let relative = utf8_path
        .strip_prefix(checkout_path)
        .map_err(|_| "path escapes the checked-out repository".to_string())?;
    if relative.as_str().is_empty() {
        Ok(None)
    } else {
        Ok(Some(relative.to_owned()))
    }
}
