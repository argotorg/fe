#![allow(clippy::print_stderr)]

pub mod db;
pub mod diagnostics;
pub mod files;
mod ingot_handler;

pub use common::dependencies::DependencyTree;

use std::collections::{HashMap, HashSet};

use camino::Utf8PathBuf;
use common::{InputDb, cache::remote_git_cache_dir};
pub use db::DriverDataBase;
use ingot_handler::IngotHandler;
use smol_str::SmolStr;

use hir::hir_def::TopLevelMod;
use resolver::{
    files::FilesResolutionDiagnostic,
    git::{GitDescription, GitResolver},
    graph::{GraphResolver, GraphResolverImpl},
    ingot::{
        IngotDescriptor, IngotResolutionError, IngotResolver, RemoteProgress,
        project_files_resolver,
    },
};
use url::Url;

struct LoggingProgress;

impl RemoteProgress for LoggingProgress {
    fn start(&mut self, description: &GitDescription) {
        tracing::info!(target: "resolver", "Resolving remote dependency {}", description);
    }

    fn success(&mut self, _description: &GitDescription, ingot_url: &Url) {
        tracing::info!(target: "resolver", "✅ Resolved {}", ingot_url);
    }

    fn error(&mut self, description: &GitDescription, error: &IngotResolutionError) {
        tracing::warn!(
            target: "resolver",
            "❌ Failed to resolve {}: {}",
            description,
            error
        );
    }
}

fn ingot_resolver(remote_checkout_root: Utf8PathBuf) -> IngotResolver {
    let files_resolver = project_files_resolver();
    let git_resolver = GitResolver::new(remote_checkout_root);
    IngotResolver::new(files_resolver, git_resolver).with_progress(Box::new(LoggingProgress))
}

pub fn init_ingot(db: &mut DriverDataBase, ingot_url: &Url) -> bool {
    tracing::info!(target: "resolver", "Starting workspace ingot resolution for: {}", ingot_url);
    let mut handler = IngotHandler::new(db).with_stdout(true);
    let mut ingot_graph_resolver =
        GraphResolverImpl::new(ingot_resolver(remote_checkout_root(ingot_url)));

    // Root ingot resolution should never fail since directory existence is validated earlier.
    // If it fails, it indicates a bug in the resolver or an unexpected system condition.
    if let Err(err) =
        ingot_graph_resolver.graph_resolve(&mut handler, &IngotDescriptor::Local(ingot_url.clone()))
    {
        panic!(
            "Unexpected failure resolving root ingot at {ingot_url}: {err:?}. This indicates a bug in the resolver since directory existence is validated before calling init_ingot."
        );
    }

    let (trace_enabled, stdout_enabled) = handler.logging_modes();
    let mut had_diagnostics = handler.had_diagnostics();

    // Check for cycles after graph resolution (now that handler is dropped)
    let cyclic_subgraph = db.dependency_graph().cyclic_subgraph(db);

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

        // Generate the tree display string
        let tree_display =
            DependencyTree::from_parts(cyclic_subgraph, ingot_url.clone(), configs, HashSet::new())
                .display();

        let diag = IngotInitDiagnostics::IngotDependencyCycle { tree_display };
        if trace_enabled {
            tracing::warn!(target: "resolver", "{diag}");
        }
        if stdout_enabled {
            eprintln!("❌ {diag}");
        }
        had_diagnostics = true;
    }

    if !had_diagnostics {
        tracing::info!(target: "resolver", "Ingot resolution completed successfully for: {}", ingot_url);
    } else {
        tracing::warn!(target: "resolver", "Ingot resolution completed with diagnostics for: {}", ingot_url);
    }

    had_diagnostics
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
        error: IngotResolutionError,
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
        target: GitDescription,
        error: IngotResolutionError,
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

fn remote_checkout_root(ingot_url: &Url) -> Utf8PathBuf {
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
