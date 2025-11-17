#![allow(clippy::print_stderr)]

pub mod db;
pub mod diagnostics;
pub mod files;

use std::{collections::HashMap, fs};

use camino::{Utf8Path, Utf8PathBuf};
use common::{
    InputDb,
    config::Config,
    dependencies::{
        DependencyArguments, DependencyLocation, RemoteDependencyRequest,
        display_tree::display_tree,
    },
};
pub use db::DriverDataBase;
use smol_str::SmolStr;

use hir::hir_def::TopLevelMod;
use resolver::{
    ResolutionHandler, Resolver,
    files::{FilesResolutionDiagnostic, FilesResolutionError, FilesResolver, FilesResource},
    git::{
        GitDependencyDescription, GitResolutionDiagnostic, GitResolutionError, GitResolver,
        GitResource,
    },
    graph::{
        DiGraph, GraphResolutionHandler, GraphResolver, GraphResolverImpl,
        petgraph::{algo::is_cyclic_directed, visit::EdgeRef},
    },
};
use url::Url;

pub type IngotGraphResolver<'a> =
    GraphResolverImpl<FilesResolver, InputHandler<'a>, (SmolStr, DependencyArguments)>;

pub fn ingot_graph_resolver<'a>() -> IngotGraphResolver<'a> {
    let files_resolver = FilesResolver::new()
        .with_required_file("fe.toml")
        .with_required_directory("src")
        .with_required_file("src/lib.fe")
        .with_pattern("src/**/*.fe");
    GraphResolverImpl::new(files_resolver)
}

pub fn init_ingot(db: &mut DriverDataBase, ingot_url: &Url) -> Vec<IngotInitDiagnostics> {
    tracing::info!(target: "resolver", "Starting workspace ingot resolution for: {}", ingot_url);
    let remote_requests;
    let mut diagnostics: Vec<IngotInitDiagnostics> = {
        let mut handler = InputHandler::from_db(db, ingot_url.clone());
        let mut ingot_graph_resolver = ingot_graph_resolver();

        // Root ingot resolution should never fail since directory existence is validated earlier.
        // If it fails, it indicates a bug in the resolver or an unexpected system condition.
        if let Err(err) = ingot_graph_resolver.graph_resolve(&mut handler, ingot_url) {
            panic!(
                "Unexpected failure resolving root ingot at {ingot_url}: {err:?}. This indicates a bug in the resolver since directory existence is validated before calling init_ingot."
            );
        }

        // Collect diagnostics from all sources
        let mut all_diagnostics = Vec::new();

        // Add handler diagnostics (missing fe.toml)
        all_diagnostics.extend(std::mem::take(&mut handler.diagnostics));

        // Add graph resolver diagnostics (unresolvable dependencies)
        all_diagnostics.extend(ingot_graph_resolver.take_diagnostics().into_iter().map(
            |diagnostic| IngotInitDiagnostics::UnresolvableIngotDependency {
                target: diagnostic.0,
                error: diagnostic.1,
            },
        ));

        // Add files resolver diagnostics (file errors)
        all_diagnostics.extend(
            ingot_graph_resolver
                .node_resolver
                .take_diagnostics()
                .into_iter()
                .map(|diagnostic| IngotInitDiagnostics::FileError { diagnostic }),
        );

        remote_requests = handler.take_remote_dependencies();

        all_diagnostics
    };

    diagnostics.extend(resolve_remote_dependencies(db, ingot_url, &remote_requests));

    // Check for cycles after graph resolution (now that handler is dropped)
    let cyclic_subgraph = db.graph().cyclic_subgraph(db);

    // Add cycle diagnostics - single comprehensive diagnostic if any cycles exist
    if cyclic_subgraph.node_count() > 0 {
        // Get configs for all nodes in the cyclic subgraph
        let mut configs = HashMap::new();
        for node_idx in cyclic_subgraph.node_indices() {
            let url = &cyclic_subgraph[node_idx];
            if let Some(ingot) = db.workspace().containing_ingot(db, url.clone())
                && let Some(config) = ingot.config(db)
            {
                configs.insert(url.clone(), config);
            }
        }

        // The root ingot should be part of any detected cycles since we're analyzing its dependencies
        if !cyclic_subgraph
            .node_indices()
            .any(|idx| cyclic_subgraph[idx] == *ingot_url)
        {
            panic!(
                "Root ingot {ingot_url} not found in cyclic subgraph. This indicates a bug in cycle detection logic."
            );
        }
        let tree_root = ingot_url.clone();

        // Generate the tree display string
        let tree_display = display_tree(&cyclic_subgraph, &tree_root, &configs);

        diagnostics.push(IngotInitDiagnostics::IngotDependencyCycle { tree_display });
    }

    if diagnostics.is_empty() {
        tracing::info!(target: "resolver", "Ingot resolution completed successfully for: {}", ingot_url);
    } else {
        tracing::warn!(target: "resolver", "Ingot resolution completed with {} diagnostics for: {}", diagnostics.len(), ingot_url);
    }

    diagnostics
}

fn _dump_scope_graph(db: &DriverDataBase, top_mod: TopLevelMod) -> String {
    let mut s = vec![];
    top_mod.scope_graph(db).write_as_dot(db, &mut s).unwrap();
    String::from_utf8(s).unwrap()
}

// Maybe the driver should eventually only support WASI?

#[derive(Debug)]
pub enum IngotInitDiagnostics {
    UnresolvableIngotDependency {
        target: Url,
        error: FilesResolutionError,
    },
    IngotDependencyCycle {
        tree_display: String,
    },
    FileError {
        diagnostic: FilesResolutionDiagnostic,
    },
    ConfigParseError {
        ingot_url: Url,
        error: String,
    },
    ConfigDiagnostics {
        ingot_url: Url,
        diagnostics: Vec<common::config::ConfigDiagnostic>,
    },
    RemoteFileError {
        ingot_url: Url,
        error: String,
    },
    RemoteConfigParseError {
        ingot_url: Url,
        error: String,
    },
    RemoteConfigDiagnostics {
        ingot_url: Url,
        diagnostics: Vec<common::config::ConfigDiagnostic>,
    },
    UnresolvableRemoteDependency {
        target: GitDependencyDescription,
        error: GitResolutionError,
    },
    RemoteDependencyCycle {
        root_ingot: Url,
    },
    RemoteGitDiagnostic {
        diagnostic: GitResolutionDiagnostic,
    },
    RemotePathResolutionError {
        ingot_url: Url,
        dependency: SmolStr,
        error: String,
    },
}

impl std::fmt::Display for IngotInitDiagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IngotInitDiagnostics::UnresolvableIngotDependency { target, error } => {
                write!(f, "Failed to resolve ingot dependency '{target}': {error}")
            }
            IngotInitDiagnostics::IngotDependencyCycle { tree_display } => {
                write!(
                    f,
                    "Detected cycle(s) in ingot dependencies:\n\n{tree_display}"
                )
            }
            IngotInitDiagnostics::FileError { diagnostic } => {
                write!(f, "File resolution error: {diagnostic}")
            }
            IngotInitDiagnostics::ConfigParseError { ingot_url, error } => {
                write!(f, "Invalid fe.toml file in ingot {ingot_url}: {error}")
            }
            IngotInitDiagnostics::ConfigDiagnostics {
                ingot_url,
                diagnostics,
            } => {
                if diagnostics.len() == 1 {
                    write!(
                        f,
                        "Erroneous fe.toml file in {ingot_url}: {}",
                        diagnostics[0]
                    )
                } else {
                    writeln!(f, "Erroneous fe.toml file in {ingot_url}:")?;
                    for diagnostic in diagnostics {
                        writeln!(f, "  • {diagnostic}")?;
                    }
                    Ok(())
                }
            }
            IngotInitDiagnostics::RemoteFileError { ingot_url, error } => {
                write!(f, "Remote file error at {ingot_url}: {error}")
            }
            IngotInitDiagnostics::RemoteConfigParseError { ingot_url, error } => {
                write!(f, "Invalid remote fe.toml file in {ingot_url}: {error}")
            }
            IngotInitDiagnostics::RemoteConfigDiagnostics {
                ingot_url,
                diagnostics,
            } => {
                if diagnostics.len() == 1 {
                    write!(
                        f,
                        "Erroneous remote fe.toml file in {ingot_url}: {}",
                        diagnostics[0]
                    )
                } else {
                    writeln!(f, "Erroneous remote fe.toml file in {ingot_url}:")?;
                    for diagnostic in diagnostics {
                        writeln!(f, "  • {diagnostic}")?;
                    }
                    Ok(())
                }
            }
            IngotInitDiagnostics::UnresolvableRemoteDependency { target, error } => {
                write!(f, "Failed to resolve remote dependency '{target}': {error}")
            }
            IngotInitDiagnostics::RemoteDependencyCycle { root_ingot } => {
                write!(
                    f,
                    "Remote dependencies rooted at {root_ingot} contain a cycle"
                )
            }
            IngotInitDiagnostics::RemoteGitDiagnostic { diagnostic } => {
                write!(f, "Remote git diagnostic: {diagnostic}")
            }
            IngotInitDiagnostics::RemotePathResolutionError {
                ingot_url,
                dependency,
                error,
            } => {
                write!(
                    f,
                    "Remote dependency '{dependency}' in {ingot_url} points outside the repository: {error}"
                )
            }
        }
    }
}

pub struct InputHandler<'a> {
    pub db: &'a mut dyn InputDb,
    pub diagnostics: Vec<IngotInitDiagnostics>,
    pub main_ingot_url: Url,
    pub remote_dependencies: Vec<RemoteDependencyRequest>,
}

impl<'a> InputHandler<'a> {
    pub fn from_db(db: &'a mut dyn InputDb, main_ingot_url: Url) -> Self {
        Self {
            db,
            diagnostics: vec![],
            main_ingot_url,
            remote_dependencies: vec![],
        }
    }

    pub fn take_remote_dependencies(&mut self) -> Vec<RemoteDependencyRequest> {
        std::mem::take(&mut self.remote_dependencies)
    }
}

impl<'a> ResolutionHandler<FilesResolver> for InputHandler<'a> {
    type Item = Vec<(Url, (SmolStr, DependencyArguments))>;

    fn handle_resolution(&mut self, ingot_url: &Url, resource: FilesResource) -> Self::Item {
        let mut config = None;

        for file in resource.files {
            if file.path.ends_with("fe.toml") {
                self.db.workspace().touch(
                    self.db,
                    Url::from_file_path(file.path).unwrap(),
                    Some(file.content.clone()),
                );
                config = Some(file.content.clone());
            } else {
                self.db.workspace().touch(
                    self.db,
                    Url::from_file_path(file.path).unwrap(),
                    Some(file.content),
                );
            }
        }

        if config.is_some() {
            if let Some(ingot) = self
                .db
                .workspace()
                .containing_ingot(self.db, ingot_url.clone())
            {
                // Check for config parse errors first
                if let Some(parse_error) = ingot.config_parse_error(self.db) {
                    self.diagnostics
                        .push(IngotInitDiagnostics::ConfigParseError {
                            ingot_url: ingot_url.clone(),
                            error: parse_error,
                        });
                    vec![]
                } else if let Some(parsed_config) = ingot.config(self.db) {
                    // Check for config validation diagnostics (invalid names, versions, etc.)
                    if !parsed_config.diagnostics.is_empty() {
                        self.diagnostics
                            .push(IngotInitDiagnostics::ConfigDiagnostics {
                                ingot_url: ingot_url.clone(),
                                diagnostics: parsed_config.diagnostics.clone(),
                            });
                    }

                    let mut pending = Vec::new();
                    for dependency in parsed_config.dependencies(ingot_url) {
                        let alias = dependency.alias;
                        let arguments = dependency.arguments;
                        match dependency.location {
                            DependencyLocation::Local(local) => {
                                let url = local.url;

                                if self.db.graph().contains_url(self.db, &url) {
                                    self.db
                                        .graph()
                                        .add_dependency(self.db, ingot_url, &url, alias, arguments);
                                } else {
                                    pending.push((url, (alias, arguments)));
                                }
                            }
                            DependencyLocation::Git(git) => {
                                self.remote_dependencies.push(RemoteDependencyRequest {
                                    parent: ingot_url.clone(),
                                    alias,
                                    arguments,
                                    git,
                                });
                            }
                        }
                    }
                    pending
                } else {
                    // No config file found at this ingot
                    vec![]
                }
            } else {
                // This shouldn't happen since we found a config file
                tracing::error!("Unable to locate ingot for config: {}", ingot_url);
                vec![]
            }
        } else {
            // No fe.toml file found - this will be reported by the FilesResolver as a diagnostic
            vec![]
        }
    }
}

impl<'a> GraphResolutionHandler<Url, DiGraph<Url, (SmolStr, DependencyArguments)>>
    for InputHandler<'a>
{
    type Item = ();

    fn handle_graph_resolution(
        &mut self,
        _ingot_url: &Url,
        graph: DiGraph<Url, (SmolStr, DependencyArguments)>,
    ) -> Self::Item {
        let dependency_graph = self.db.graph();

        // Add edges from the resolved graph to the database dependency graph
        // Note: edges to existing URLs are already added in handle_resolution
        for edge in graph.edge_references() {
            let from_url = &graph[edge.source()];
            let to_url = &graph[edge.target()];
            let (alias, arguments) = edge.weight();
            dependency_graph.add_dependency(
                self.db,
                from_url,
                to_url,
                alias.clone(),
                arguments.clone(),
            );
        }
    }
}

fn resolve_remote_dependencies(
    db: &mut dyn InputDb,
    ingot_url: &Url,
    requests: &[RemoteDependencyRequest],
) -> Vec<IngotInitDiagnostics> {
    if requests.is_empty() {
        return vec![];
    }

    let checkout_root = remote_checkout_root(ingot_url);
    let git_resolver = GitResolver::new(checkout_root);
    let mut graph_resolver: GraphResolverImpl<
        GitResolver,
        DriverRemoteHandler<'_>,
        (SmolStr, DependencyArguments),
    > = GraphResolverImpl::new(git_resolver);
    let mut handler = DriverRemoteHandler::new(db);
    let mut diagnostics = Vec::new();

    for request in requests {
        let description = git_description_from_request(request);
        if let Ok(graph) = graph_resolver.graph_resolve(&mut handler, &description) {
            if is_cyclic_directed(&graph) {
                if let Some(root_url) = handler.ingot_url(&description) {
                    diagnostics.push(IngotInitDiagnostics::RemoteDependencyCycle {
                        root_ingot: root_url.clone(),
                    });
                }
            }
        }
    }

    diagnostics.extend(handler.take_diagnostics());
    diagnostics.extend(
        graph_resolver
            .take_diagnostics()
            .into_iter()
            .map(
                |diagnostic| IngotInitDiagnostics::UnresolvableRemoteDependency {
                    target: diagnostic.0,
                    error: diagnostic.1,
                },
            ),
    );
    diagnostics.extend(
        graph_resolver
            .node_resolver
            .take_diagnostics()
            .into_iter()
            .map(|diagnostic| IngotInitDiagnostics::RemoteGitDiagnostic { diagnostic }),
    );

    diagnostics
}

fn remote_checkout_root(ingot_url: &Url) -> Utf8PathBuf {
    let mut path = Utf8PathBuf::from_path_buf(
        ingot_url
            .to_file_path()
            .expect("ingot URL should map to a local path"),
    )
    .expect("ingot path should be valid UTF-8");
    path.push(".fe");
    path.push("git");
    path
}

fn git_description_from_request(request: &RemoteDependencyRequest) -> GitDependencyDescription {
    let mut description =
        GitDependencyDescription::new(request.git.source.clone(), request.git.rev.to_string());
    if let Some(path) = request.git.path.clone() {
        description = description.with_path(path);
    }
    description
}

struct DriverRemoteHandler<'a> {
    db: &'a mut dyn InputDb,
    ingot_urls: HashMap<GitDependencyDescription, Url>,
    configs: HashMap<Url, Config>,
    diagnostics: Vec<IngotInitDiagnostics>,
}

impl<'a> DriverRemoteHandler<'a> {
    fn new(db: &'a mut dyn InputDb) -> Self {
        Self {
            db,
            ingot_urls: HashMap::new(),
            configs: HashMap::new(),
            diagnostics: Vec::new(),
        }
    }

    fn ingot_url(&self, description: &GitDependencyDescription) -> Option<&Url> {
        self.ingot_urls.get(description)
    }

    fn take_diagnostics(&mut self) -> Vec<IngotInitDiagnostics> {
        std::mem::take(&mut self.diagnostics)
    }
}

impl<'a> ResolutionHandler<GitResolver> for DriverRemoteHandler<'a> {
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
                self.diagnostics
                    .push(IngotInitDiagnostics::RemoteFileError {
                        ingot_url: resource.ingot_url.clone(),
                        error: error.to_string(),
                    });
                return vec![];
            }
        };

        let config = match Config::parse(&config_content) {
            Ok(config) => config,
            Err(error) => {
                self.diagnostics
                    .push(IngotInitDiagnostics::RemoteConfigParseError {
                        ingot_url: resource.ingot_url.clone(),
                        error,
                    });
                return vec![];
            }
        };

        if !config.diagnostics.is_empty() {
            self.diagnostics
                .push(IngotInitDiagnostics::RemoteConfigDiagnostics {
                    ingot_url: resource.ingot_url.clone(),
                    diagnostics: config.diagnostics.clone(),
                });
        }

        self.configs
            .insert(resource.ingot_url.clone(), config.clone());

        let mut dependencies = Vec::new();

        for dependency in config.dependencies(&resource.ingot_url) {
            let alias = dependency.alias;
            let arguments = dependency.arguments;
            match dependency.location {
                DependencyLocation::Local(local) => match relative_path_within_checkout(
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
                        self.diagnostics
                            .push(IngotInitDiagnostics::RemotePathResolutionError {
                                ingot_url: resource.ingot_url.clone(),
                                dependency: alias.clone(),
                                error,
                            });
                    }
                },
                DependencyLocation::Git(git) => {
                    let mut next_description =
                        GitDependencyDescription::new(git.source.clone(), git.rev.to_string());
                    if let Some(path) = git.path.clone() {
                        next_description = next_description.with_path(path);
                    }
                    dependencies.push((next_description, (alias, arguments)));
                }
            }
        }

        dependencies
    }
}

impl<'a>
    GraphResolutionHandler<
        GitDependencyDescription,
        DiGraph<GitDependencyDescription, (SmolStr, DependencyArguments)>,
    > for DriverRemoteHandler<'a>
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
                let weight = edge.weight().clone();
                let from_url = converted[from_idx].clone();
                let to_url = converted[to_idx].clone();
                converted.add_edge(from_idx, to_idx, weight.clone());
                self.db
                    .remote_graph()
                    .add_dependency(self.db, &from_url, &to_url, weight.0, weight.1);
            }
        }

        converted
    }
}

fn relative_path_within_checkout(
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
