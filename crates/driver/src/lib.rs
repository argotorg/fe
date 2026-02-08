#![allow(clippy::print_stderr)]

pub mod db;
pub mod diagnostics;
pub mod files;
mod ingot_handler;

pub use common::dependencies::DependencyTree;

use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicBool, Ordering};

use camino::Utf8PathBuf;
use common::{
    InputDb,
    cache::remote_git_cache_dir,
    config::WorkspaceMemberSelection,
    ingot::Version,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
pub use db::DriverDataBase;
use ingot_handler::IngotHandler;
use smol_str::SmolStr;

use hir::analysis::core_requirements;
use hir::hir_def::TopLevelMod;
pub use resolver::workspace::{ExpandedWorkspaceMember, expand_workspace_members};
use resolver::{
    files::FilesResolutionDiagnostic,
    git::{GitDescription, GitResolver},
    graph::{AsyncGraphResolverImpl, GraphResolver},
    ingot::{IngotDescriptor, IngotResolutionError, IngotResolverImpl, RemoteProgress},
};
use url::Url;

static RESOLVER_VERBOSE: AtomicBool = AtomicBool::new(false);

pub fn set_resolver_verbose(enabled: bool) {
    RESOLVER_VERBOSE.store(enabled, Ordering::Relaxed);
}

fn resolver_verbose() -> bool {
    RESOLVER_VERBOSE.load(Ordering::Relaxed)
}

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

fn ingot_resolver(remote_checkout_root: Utf8PathBuf) -> IngotResolverImpl {
    let git_resolver = GitResolver::new(remote_checkout_root);
    IngotResolverImpl::new(git_resolver).with_progress(Box::new(LoggingProgress))
}

pub fn init_ingot(db: &mut DriverDataBase, ingot_url: &Url) -> bool {
    init_ingot_graph(db, ingot_url)
}

pub fn check_library_requirements(db: &DriverDataBase) -> Vec<String> {
    let mut missing = Vec::new();

    let core = db.builtin_core();
    if let Ok(core_file) = core.root_file(db) {
        let top_mod = db.top_mod(core_file);
        missing.extend(
            core_requirements::check_core_requirements(db, top_mod.scope(), core.kind(db))
                .into_iter()
                .map(|req| req.to_string()),
        );
    } else {
        missing.push("missing required core ingot".to_string());
    }

    let std_ingot = db.builtin_std();
    if let Ok(std_file) = std_ingot.root_file(db) {
        let top_mod = db.top_mod(std_file);
        missing.extend(
            core_requirements::check_std_type_requirements(db, top_mod.scope(), std_ingot.kind(db))
                .into_iter()
                .map(|req| req.to_string()),
        );
    } else {
        missing.push("missing required std ingot".to_string());
    }

    missing
}

fn init_ingot_graph(db: &mut DriverDataBase, ingot_url: &Url) -> bool {
    tracing::info!(target: "resolver", "Starting ingot resolution for: {}", ingot_url);
    let checkout_root = remote_checkout_root(ingot_url);
    let mut handler = IngotHandler::new(db)
        .with_stdout(true)
        .with_verbose(resolver_verbose());
    let checkout_root_clone = checkout_root.clone();
    let mut ingot_graph_resolver =
        AsyncGraphResolverImpl::new(move || ingot_resolver(checkout_root_clone.clone()));

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

pub fn init_workspace(db: &mut DriverDataBase, workspace_url: &Url) -> bool {
    init_ingot_graph(db, workspace_url)
}

pub fn find_ingot_by_metadata(db: &DriverDataBase, name: &str, version: &Version) -> Option<Url> {
    db.dependency_graph()
        .ingot_by_name_version(db, &SmolStr::new(name), version)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkspaceMember {
    pub url: Url,
    pub path: Utf8PathBuf,
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

fn _dump_scope_graph(db: &DriverDataBase, top_mod: TopLevelMod) -> String {
    let mut s = vec![];
    top_mod.scope_graph(db).write_as_dot(db, &mut s).unwrap();
    String::from_utf8(s).unwrap()
}

pub fn workspace_member_urls(
    workspace: &common::config::WorkspaceSettings,
    workspace_url: &Url,
) -> Result<Vec<Url>, String> {
    let members = workspace_members(workspace, workspace_url)?;
    Ok(members
        .into_iter()
        .filter(|member| member.url != *workspace_url)
        .map(|member| member.url)
        .collect())
}

pub fn workspace_members(
    workspace: &common::config::WorkspaceSettings,
    workspace_url: &Url,
) -> Result<Vec<WorkspaceMember>, String> {
    let selection = if workspace.default_members.is_some() {
        WorkspaceMemberSelection::DefaultOnly
    } else {
        WorkspaceMemberSelection::All
    };
    let members = expand_workspace_members(workspace, workspace_url, selection)?;
    Ok(members
        .into_iter()
        .filter(|member| member.url != *workspace_url)
        .map(|member| WorkspaceMember {
            url: member.url,
            path: member.path,
            name: member.name,
            version: member.version,
        })
        .collect())
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
    WorkspaceConfigParseError {
        workspace_url: Url,
        error: String,
    },
    WorkspaceDiagnostics {
        workspace_url: Url,
        diagnostics: Vec<common::config::ConfigDiagnostic>,
    },
    WorkspaceMembersError {
        workspace_url: Url,
        error: String,
    },
    WorkspaceMemberDuplicate {
        workspace_url: Url,
        name: SmolStr,
        version: Option<Version>,
    },
    WorkspaceMemberMetadataMismatch {
        ingot_url: Url,
        expected_name: SmolStr,
        expected_version: Version,
        found_name: Option<SmolStr>,
        found_version: Option<Version>,
    },
    WorkspaceDependencyAliasConflict {
        workspace_url: Url,
        alias: SmolStr,
    },
    WorkspacePathRequiresSelection {
        ingot_url: Url,
        dependency: SmolStr,
        workspace_url: Url,
    },
    WorkspaceNameLookupUnavailable {
        ingot_url: Url,
        dependency: SmolStr,
    },
    DependencyMetadataMismatch {
        ingot_url: Url,
        dependency: SmolStr,
        dependency_url: Url,
        expected_name: SmolStr,
        expected_version: Option<Version>,
        found_name: Option<SmolStr>,
        found_version: Option<Version>,
    },
    WorkspaceMemberResolutionFailed {
        ingot_url: Url,
        dependency: SmolStr,
        error: String,
    },
    IngotByNameResolutionFailed {
        ingot_url: Url,
        dependency: SmolStr,
        name: SmolStr,
        version: Version,
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

fn format_metadata(name: &Option<SmolStr>, version: &Option<Version>) -> String {
    let name = name
        .as_ref()
        .map(ToString::to_string)
        .unwrap_or_else(|| "<missing name>".to_string());
    let version = version
        .as_ref()
        .map(ToString::to_string)
        .unwrap_or_else(|| "<missing version>".to_string());
    format!("{name}@{version}")
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
            IngotInitDiagnostics::WorkspaceConfigParseError {
                workspace_url,
                error,
            } => {
                write!(f, "Invalid workspace fe.toml in {workspace_url}: {error}")
            }
            IngotInitDiagnostics::WorkspaceDiagnostics {
                workspace_url,
                diagnostics,
            } => {
                if diagnostics.len() == 1 {
                    write!(
                        f,
                        "Erroneous workspace fe.toml in {workspace_url}: {}",
                        diagnostics[0]
                    )
                } else {
                    writeln!(f, "Erroneous workspace fe.toml in {workspace_url}:")?;
                    for diagnostic in diagnostics {
                        writeln!(f, "  • {diagnostic}")?;
                    }
                    Ok(())
                }
            }
            IngotInitDiagnostics::WorkspaceMembersError {
                workspace_url,
                error,
            } => {
                write!(f, "Workspace members error in {workspace_url}: {error}")
            }
            IngotInitDiagnostics::WorkspaceMemberDuplicate {
                workspace_url,
                name,
                version,
            } => {
                let _ = version;
                write!(
                    f,
                    "Workspace member {name} is duplicated in {workspace_url}"
                )
            }
            IngotInitDiagnostics::WorkspaceMemberMetadataMismatch {
                ingot_url,
                expected_name,
                expected_version,
                found_name,
                found_version,
            } => {
                write!(
                    f,
                    "Workspace member {expected_name}@{expected_version} in {ingot_url} has mismatched metadata (found {})",
                    format_metadata(found_name, found_version)
                )
            }
            IngotInitDiagnostics::WorkspaceDependencyAliasConflict {
                workspace_url,
                alias,
            } => {
                write!(
                    f,
                    "Workspace dependency alias '{alias}' conflicts with a workspace member name in {workspace_url}"
                )
            }
            IngotInitDiagnostics::WorkspacePathRequiresSelection {
                ingot_url,
                dependency,
                workspace_url,
            } => {
                write!(
                    f,
                    "Dependency '{dependency}' in {ingot_url} points to a workspace at {workspace_url}; provide an ingot path or a name/version"
                )
            }
            IngotInitDiagnostics::WorkspaceNameLookupUnavailable {
                ingot_url,
                dependency,
            } => {
                write!(
                    f,
                    "Dependency '{dependency}' in {ingot_url} uses name-only lookup outside a workspace"
                )
            }
            IngotInitDiagnostics::DependencyMetadataMismatch {
                ingot_url,
                dependency,
                dependency_url,
                expected_name,
                expected_version,
                found_name,
                found_version,
            } => {
                if let Some(expected_version) = expected_version {
                    write!(
                        f,
                        "Dependency '{dependency}' in {ingot_url} expected {expected_name}@{expected_version} at {dependency_url} but found {}",
                        format_metadata(found_name, found_version)
                    )
                } else {
                    write!(
                        f,
                        "Dependency '{dependency}' in {ingot_url} expected {expected_name} at {dependency_url} but found {}",
                        format_metadata(found_name, found_version)
                    )
                }
            }
            IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                ingot_url,
                dependency,
                error,
            } => {
                write!(
                    f,
                    "Failed to resolve workspace member for '{dependency}' in {ingot_url}: {error}"
                )
            }
            IngotInitDiagnostics::IngotByNameResolutionFailed {
                ingot_url,
                dependency,
                name,
                version,
            } => {
                write!(
                    f,
                    "Dependency '{dependency}' in {ingot_url} requested ingot {name}@{version} but it was not found in the workspace registry"
                )
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

pub(crate) fn remote_checkout_root(ingot_url: &Url) -> Utf8PathBuf {
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
