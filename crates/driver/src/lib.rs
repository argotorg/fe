#![allow(clippy::print_stderr)]

pub mod db;
pub mod diagnostics;
pub mod files;

mod handlers;

use std::{collections::HashMap, time::Duration};

use camino::Utf8PathBuf;
use common::{
    InputDb,
    cache::remote_git_cache_dir,
    dependencies::{DependencyArguments, ExternalDependencyEdge, display_tree::display_tree},
};
pub use db::DriverDataBase;
use handlers::{LocalIngotHandler, RemoteIngotHandler};
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use smol_str::SmolStr;

use hir::hir_def::TopLevelMod;
use resolver::{
    Resolver,
    files::{FilesResolutionDiagnostic, FilesResolutionError, FilesResolver},
    git::{GitDependencyDescription, GitResolutionDiagnostic, GitResolutionError, GitResolver},
    graph::{GraphResolver, GraphResolverImpl, petgraph::algo::is_cyclic_directed},
};
use url::Url;

pub type IngotGraphResolver<'a> =
    GraphResolverImpl<FilesResolver, LocalIngotHandler<'a>, (SmolStr, DependencyArguments)>;

fn ingot_files_resolver() -> FilesResolver {
    FilesResolver::new()
        .with_required_file("fe.toml")
        .with_required_directory("src")
        .with_required_file("src/lib.fe")
        .with_pattern("src/**/*.fe")
}

pub fn ingot_graph_resolver<'a>() -> IngotGraphResolver<'a> {
    GraphResolverImpl::new(ingot_files_resolver())
}

pub fn init_ingot(db: &mut DriverDataBase, ingot_url: &Url) -> Vec<IngotInitDiagnostics> {
    tracing::info!(target: "resolver", "Starting workspace ingot resolution for: {}", ingot_url);
    let remote_requests;
    let mut diagnostics: Vec<IngotInitDiagnostics> = {
        let mut handler = LocalIngotHandler::new(db, ingot_url.clone());
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
    let cyclic_subgraph = db.local_graph().cyclic_subgraph(db);

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

fn resolve_remote_dependencies(
    db: &mut dyn InputDb,
    ingot_url: &Url,
    requests: &[ExternalDependencyEdge],
) -> Vec<IngotInitDiagnostics> {
    if requests.is_empty() {
        return vec![];
    }

    let checkout_root = remote_checkout_root(ingot_url);
    let git_resolver = GitResolver::new(checkout_root);
    let mut graph_resolver: GraphResolverImpl<
        GitResolver,
        RemoteIngotHandler<'_>,
        (SmolStr, DependencyArguments),
    > = GraphResolverImpl::new(git_resolver);
    let mut handler = RemoteIngotHandler::new(db);
    let mut diagnostics = Vec::new();
    let multi = MultiProgress::new();
    let spinner_style = ProgressStyle::with_template("{spinner} {msg}")
        .unwrap()
        .tick_strings(&["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]);

    for request in requests {
        let description = git_description_from_request(request);
        let spinner = multi.add(ProgressBar::new_spinner());
        spinner.set_style(spinner_style.clone());
        spinner.enable_steady_tick(Duration::from_millis(80));
        spinner.set_message(format!(
            "Resolving remote dependency {} ({})",
            request.alias, description
        ));

        match graph_resolver.graph_resolve(&mut handler, &description) {
            Ok(graph) => {
                if let Some(root_url) = handler.ingot_url(&description) {
                    spinner.finish_with_message(format!("✅ Resolved {root_url}"));
                } else {
                    spinner.finish_and_clear();
                }
                if is_cyclic_directed(&graph) {
                    if let Some(root_url) = handler.ingot_url(&description) {
                        diagnostics.push(IngotInitDiagnostics::RemoteDependencyCycle {
                            root_ingot: root_url.clone(),
                        });
                    }
                }
            }
            Err(err) => {
                spinner.abandon_with_message(format!("❌ Failed to resolve {description}: {err}"));
            }
        }
    }
    let _ = multi.clear();

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
    for diagnostic in graph_resolver.node_resolver.take_diagnostics() {
        match diagnostic {
            GitResolutionDiagnostic::ReusedCheckout(path) => {
                tracing::info!(target: "resolver", "Reused existing remote checkout at {}", path);
            }
        }
    }

    diagnostics
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

fn git_description_from_request(request: &ExternalDependencyEdge) -> GitDependencyDescription {
    let mut description = GitDependencyDescription::new(
        request.remote.source.clone(),
        request.remote.rev.to_string(),
    );
    if let Some(path) = request.remote.path.clone() {
        description = description.with_path(path);
    }
    description
}
