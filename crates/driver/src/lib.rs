#![allow(clippy::print_stderr)]

pub mod db;
pub mod diagnostics;
pub mod files;

use std::{collections::HashMap, mem::take};

use common::{
    InputDb,
    graph::{EdgeWeight, JoinEdge},
    tree::display_dependency_tree,
};
pub use db::DriverDataBase;

use common::config::Config;
use hir::hir_def::TopLevelMod;
use resolver::{
    ResolutionHandler, Resolver,
    files::{FilesResolutionDiagnostic, FilesResolutionError, FilesResolver, FilesResource},
    graph::{DiGraph, GraphResolutionHandler, GraphResolver, GraphResolverImpl},
};
use url::Url;

pub type IngotGraphResolver<'a> = GraphResolverImpl<FilesResolver, InputHandler<'a>, EdgeWeight>;

pub fn ingot_graph_resolver<'a>() -> IngotGraphResolver<'a> {
    let files_resolver = FilesResolver::new()
        .with_required_file("fe.toml")
        .with_required_directory("src")
        .with_required_file("src/lib.fe")
        .with_pattern("src/**/*.fe");
    GraphResolverImpl::new(files_resolver)
}

pub fn init_ingot(db: &mut DriverDataBase, ingot_url: &Url) -> Vec<IngotInitDiagnostics> {
    tracing::trace!(target: "resolver", "Starting workspace ingot resolution for: {}", ingot_url);
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
        let tree_display = display_dependency_tree(&cyclic_subgraph, &tree_root, &configs);

        diagnostics.push(IngotInitDiagnostics::IngotDependencyCycle { tree_display });
    }

    if diagnostics.is_empty() {
        tracing::trace!(target: "resolver", "Ingot resolution completed successfully for: {}", ingot_url);
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
    MissingFeToml {
        ingot_url: Url,
    },
    InvalidToml {
        ingot_url: Url,
        error: String,
    },
    ConfigValidation {
        ingot_url: Url,
        diagnostic: common::config::ConfigDiagnostic,
    },
    MissingRootFile {
        ingot_url: Url,
        is_main_ingot: bool,
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
            IngotInitDiagnostics::MissingFeToml { ingot_url } => {
                write!(f, "Missing fe.toml file in ingot: {ingot_url}")
            }
            IngotInitDiagnostics::InvalidToml { ingot_url, error } => {
                write!(f, "Invalid fe.toml file in ingot {ingot_url}: {error}")
            }
            IngotInitDiagnostics::ConfigValidation {
                ingot_url,
                diagnostic,
            } => {
                write!(f, "Config validation error at {ingot_url}: {diagnostic}")
            }
            IngotInitDiagnostics::MissingRootFile {
                ingot_url,
                is_main_ingot,
            } => {
                if *is_main_ingot {
                    write!(
                        f,
                        "Missing root file (src/lib.fe) in current ingot: {ingot_url}"
                    )
                } else {
                    write!(
                        f,
                        "Missing root file (src/lib.fe) in dependency: {ingot_url}"
                    )
                }
            }
        }
    }
}

pub struct InputHandler<'a> {
    pub db: &'a mut dyn InputDb,
    pub join_edges: Vec<JoinEdge>,
    pub diagnostics: Vec<IngotInitDiagnostics>,
    pub main_ingot_url: Url,
}

impl<'a> InputHandler<'a> {
    pub fn from_db(db: &'a mut dyn InputDb, main_ingot_url: Url) -> Self {
        Self {
            db,
            join_edges: vec![],
            diagnostics: vec![],
            main_ingot_url,
        }
    }
}

impl<'a> ResolutionHandler<FilesResolver> for InputHandler<'a> {
    type Item = Vec<(Url, EdgeWeight)>;

    fn handle_resolution(&mut self, ingot_url: &Url, resource: FilesResource) -> Self::Item {
        let mut config = None;

        for file in resource.files {
            if file.path.ends_with("fe.toml") {
                self.db.workspace().touch(
                    self.db,
                    Url::from_file_path(file.path).unwrap(),
                    Some(file.content.clone()),
                );
                config = Some(file.content);
            } else {
                self.db.workspace().touch(
                    self.db,
                    Url::from_file_path(file.path).unwrap(),
                    Some(file.content),
                );
            }
        }

        if let Some(content) = config {
            let config = match Config::parse(&content) {
                Ok(config) => config,
                Err(err) => {
                    // Add invalid config as a diagnostic
                    tracing::error!(target: "resolver", "Failed to parse fe.toml: {:?}", err);
                    self.diagnostics.push(IngotInitDiagnostics::InvalidToml {
                        ingot_url: ingot_url.clone(),
                        error: err.to_string(),
                    });
                    return vec![];
                }
            };

            // Check for config validation diagnostics (invalid names, versions, etc.)
            for diagnostic in &config.diagnostics {
                self.diagnostics
                    .push(IngotInitDiagnostics::ConfigValidation {
                        ingot_url: ingot_url.clone(),
                        diagnostic: diagnostic.clone(),
                    });
            }

            // Missing src/lib.fe file is now handled by the FilesResolver diagnostics

            // let weights: HashSet<Url> = self.db.graph().node_weights().cloned().collect();

            config
                .forward_edges(ingot_url)
                .into_iter()
                .filter_map(|(url, weight)| {
                    if self.db.graph().contains_url(self.db, &url) {
                        self.join_edges.push(JoinEdge {
                            origin: ingot_url.clone(),
                            destination: url,
                            weight,
                        });
                        None
                    } else {
                        Some((url, weight))
                    }
                })
                .collect()
        } else {
            // No fe.toml file found - this will be reported by the FilesResolver as a diagnostic
            vec![]
        }
    }
}

impl<'a> GraphResolutionHandler<Url, DiGraph<Url, EdgeWeight>> for InputHandler<'a> {
    type Item = ();

    fn handle_graph_resolution(
        &mut self,
        _ingot_url: &Url,
        graph: DiGraph<Url, EdgeWeight>,
    ) -> Self::Item {
        // Update the graph singleton in the database with the resolved graph
        self.db
            .graph()
            .join_graph(self.db, graph, take(&mut self.join_edges));
    }
}
