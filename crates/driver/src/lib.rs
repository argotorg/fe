#![allow(clippy::print_stderr)]

pub mod db;
pub mod diagnostics;
pub mod files;

use std::collections::HashMap;

use common::{
    InputDb,
    dependencies::{DependencyArguments, display_tree::display_tree},
};
pub use db::DriverDataBase;
use smol_str::SmolStr;

use hir::hir_def::TopLevelMod;
use resolver::{
    ResolutionHandler, Resolver,
    files::{FilesResolutionDiagnostic, FilesResolutionError, FilesResolver, FilesResource},
    graph::{
        DiGraph, GraphResolutionHandler, GraphResolver, GraphResolverImpl, petgraph::visit::EdgeRef,
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
    // Check if ingot is already initialized
    if db.graph().contains_url(db, ingot_url) {
        tracing::info!(target: "resolver", "Ingot already initialized: {}", ingot_url);
        return Vec::new();
    }

    tracing::info!(target: "resolver", "Starting workspace ingot resolution for: {}", ingot_url);

    let mut diagnostics: Vec<IngotInitDiagnostics> = {
        let mut handler = InputHandler::from_db(db, ingot_url.clone());
        let mut ingot_graph_resolver = ingot_graph_resolver();

        // Always enable search mode for the initial URL
        // It will auto-disable after the first resolution so dependencies use exact paths
        ingot_graph_resolver.node_resolver.search_mode = true;

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
        all_diagnostics.extend(handler.diagnostics);

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

        all_diagnostics
    };

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
        }
    }
}

pub struct InputHandler<'a> {
    pub db: &'a mut dyn InputDb,
    pub diagnostics: Vec<IngotInitDiagnostics>,
    pub main_ingot_url: Url,
}

impl<'a> InputHandler<'a> {
    pub fn from_db(db: &'a mut dyn InputDb, main_ingot_url: Url) -> Self {
        Self {
            db,
            diagnostics: vec![],
            main_ingot_url,
        }
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

                    parsed_config
                        .dependencies(ingot_url)
                        .into_iter()
                        .filter_map(|dependency| {
                            if self.db.graph().contains_url(self.db, &dependency.url) {
                                // URL already exists in graph - add edge immediately
                                self.db.graph().add_dependency(
                                    self.db,
                                    ingot_url,
                                    &dependency.url,
                                    dependency.alias,
                                    dependency.arguments,
                                );
                                None
                            } else {
                                // URL doesn't exist - needs to be resolved
                                Some((dependency.url, (dependency.alias, dependency.arguments)))
                            }
                        })
                        .collect()
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

        // Log total ingot count after resolution
        let total_ingots = dependency_graph.all_urls(self.db).len();
        if total_ingots > 0 {
            tracing::info!(target: "resolver", "Total ingots in workspace: {}", total_ingots);
        }
    }
}
