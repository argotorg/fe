#![allow(clippy::print_stderr)]

pub mod cli_target;
pub mod db;
pub mod diagnostics;
pub mod files;
mod ingot_handler;

pub use common::dependencies::DependencyTree;

use std::collections::{HashMap, HashSet};

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
pub use mir::{MirDiagnosticsMode, MirDiagnosticsOutput};
use smol_str::SmolStr;

use hir::analysis::core_requirements;
use hir::hir_def::TopLevelMod;
pub use resolver::workspace::{ExpandedWorkspaceMember, expand_workspace_members};
use resolver::{
    files::FilesResolutionDiagnostic,
    git::{GitDescription, GitResolver},
    graph::{GraphResolver, GraphResolverImpl},
    ingot::{IngotDescriptor, IngotResolutionError, IngotResolverImpl, RemoteProgress},
};
use url::Url;

struct LoggingProgress;

impl RemoteProgress for LoggingProgress {
    fn start(&mut self, description: &GitDescription) {
        tracing::info!(target: "resolver", "Resolving remote dependency {}", description);
    }

    fn success(&mut self, _description: &GitDescription, ingot_url: &Url) {
        tracing::info!(target: "resolver", "Resolved {}", ingot_url);
    }

    fn error(&mut self, description: &GitDescription, error: &IngotResolutionError) {
        tracing::warn!(
            target: "resolver",
            "Failed to resolve {}: {}",
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

/// Result of discovering and initializing ingots/files from a root directory.
pub struct DiscoveredProject {
    /// Ingot URLs that were loaded into the DB.
    pub ingot_urls: Vec<Url>,
    /// Standalone `.fe` file URLs that were loaded into the DB.
    pub standalone_files: Vec<Url>,
}

/// Discover ingots/workspaces from `root_url` and init them into `db`.
///
/// 1. Runs `discover_context` (walks up looking for fe.toml).
/// 2. If nothing found AND root has no fe.toml, scans downward for child
///    ingots/workspaces and standalone `.fe` files (e.g. a workbook with
///    scattered lessons).
pub fn discover_and_init(db: &mut DriverDataBase, root_url: &Url) -> DiscoveredProject {
    use resolver::workspace::discover_context;
    use std::collections::BTreeSet;

    let mut result = DiscoveredProject {
        ingot_urls: Vec::new(),
        standalone_files: Vec::new(),
    };

    let Ok(discovery) = discover_context(root_url) else {
        return result;
    };

    // Init workspace root if present
    if let Some(ws_root) = &discovery.workspace_root {
        init_ingot(db, ws_root);
        result.ingot_urls.push(ws_root.clone());
    }

    // Init all discovered ingots (workspace members)
    for url in &discovery.ingot_roots {
        init_ingot(db, url);
        result.ingot_urls.push(url.clone());
    }

    // If discover_context found actual ingot roots (not just a workspace root
    // with no members), we're done.  A workspace with `members = []` should
    // still fall through to the downward scan.
    if !discovery.ingot_roots.is_empty() {
        return result;
    }

    // Fallback: if root itself has fe.toml, init it directly
    let root_has_config = root_url
        .to_file_path()
        .map(|p| p.join("fe.toml").is_file())
        .unwrap_or(false);
    if root_has_config {
        init_ingot(db, root_url);
        result.ingot_urls.push(root_url.clone());
        return result;
    }

    // Nothing discovered upward and no root config — scan downward for
    // child ingots/workspaces (e.g. a workbook with scattered lessons).
    let Ok(root_path) = root_url.to_file_path() else {
        return result;
    };

    // Collect fe.toml locations and standalone .fe files via recursive scan
    let mut config_dirs = BTreeSet::new();
    let mut fe_files = Vec::new();
    fn scan_dir(
        dir: &std::path::Path,
        configs: &mut BTreeSet<std::path::PathBuf>,
        files: &mut Vec<std::path::PathBuf>,
    ) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                // Skip hidden dirs like .solutions
                if path.file_name().is_some_and(|n| n.to_string_lossy().starts_with('.')) {
                    continue;
                }
                // Skip symlinks to avoid infinite recursion on cyclic links
                if path.symlink_metadata().is_ok_and(|m| m.file_type().is_symlink()) {
                    continue;
                }
                if path.join("fe.toml").is_file() {
                    configs.insert(path.clone());
                }
                scan_dir(&path, configs, files);
            } else if path.extension().is_some_and(|ext| ext == "fe") {
                files.push(path);
            }
        }
    }
    scan_dir(&root_path, &mut config_dirs, &mut fe_files);

    // Filter out dirs that are inside another discovered dir (workspace members
    // will be expanded by discover_context, so we only want top-level configs).
    let top_level_dirs: Vec<_> = config_dirs
        .iter()
        .filter(|dir| {
            !config_dirs
                .iter()
                .any(|other| other != *dir && dir.starts_with(other))
        })
        .cloned()
        .collect();

    // Collect all ingot src/ directories to filter out .fe files that belong to ingots
    let mut ingot_src_dirs: Vec<std::path::PathBuf> = Vec::new();

    for dir in top_level_dirs {
        let Ok(dir_url) = Url::from_directory_path(&dir) else {
            continue;
        };
        ingot_src_dirs.push(dir.clone());
        // Run discover_context on each top-level config dir to properly
        // handle workspaces (expands members) vs plain ingots.
        if let Ok(sub_discovery) = discover_context(&dir_url) {
            if let Some(ws_root) = &sub_discovery.workspace_root {
                init_ingot(db, ws_root);
                result.ingot_urls.push(ws_root.clone());
            }
            for url in &sub_discovery.ingot_roots {
                if !result.ingot_urls.contains(url) {
                    init_ingot(db, url);
                    result.ingot_urls.push(url.clone());
                    if let Ok(p) = url.to_file_path() {
                        ingot_src_dirs.push(p);
                    }
                }
            }
            if sub_discovery.workspace_root.is_none() && sub_discovery.ingot_roots.is_empty() {
                init_ingot(db, &dir_url);
                result.ingot_urls.push(dir_url);
            }
        }
    }

    // Load standalone .fe files that aren't under any discovered ingot
    for fe_path in &fe_files {
        let under_ingot = ingot_src_dirs.iter().any(|d| fe_path.starts_with(d));
        if under_ingot {
            continue;
        }
        if let Ok(content) = std::fs::read_to_string(fe_path)
            && let Ok(url) = Url::from_file_path(fe_path)
        {
            db.workspace().touch(db, url.clone(), Some(content));
            result.standalone_files.push(url);
        }
    }

    result
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
    let mut handler = IngotHandler::new(db);
    let mut ingot_graph_resolver = GraphResolverImpl::new(ingot_resolver(checkout_root));

    // Root ingot resolution should never fail since directory existence is validated earlier.
    // If it fails, it indicates a bug in the resolver or an unexpected system condition.
    if let Err(err) =
        ingot_graph_resolver.graph_resolve(&mut handler, &IngotDescriptor::Local(ingot_url.clone()))
    {
        panic!(
            "Unexpected failure resolving root ingot at {ingot_url}: {err:?}. This indicates a bug in the resolver since directory existence is validated before calling init_ingot."
        );
    }

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
                .display_to(common::color::ColorTarget::Stderr);

        let diag = IngotInitDiagnostics::IngotDependencyCycle { tree_display };
        tracing::warn!(target: "resolver", "{diag}");
        eprintln!("Error: {diag}");
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
                write!(f, "File resolution failed: {diagnostic}")
            }
            IngotInitDiagnostics::ConfigParseError { ingot_url, error } => {
                write!(f, "Invalid fe.toml in ingot {ingot_url}: {error}")
            }
            IngotInitDiagnostics::ConfigDiagnostics {
                ingot_url,
                diagnostics,
            } => {
                if diagnostics.len() == 1 {
                    write!(
                        f,
                        "Invalid fe.toml in ingot {ingot_url}: {}",
                        diagnostics[0]
                    )
                } else {
                    writeln!(f, "Invalid fe.toml in ingot {ingot_url}:")?;
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
                        "Invalid workspace fe.toml in {workspace_url}: {}",
                        diagnostics[0]
                    )
                } else {
                    writeln!(f, "Invalid workspace fe.toml in {workspace_url}:")?;
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
                write!(
                    f,
                    "Failed to resolve workspace members in {workspace_url}: {error}"
                )
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
                write!(f, "Remote file operation failed at {ingot_url}: {error}")
            }
            IngotInitDiagnostics::RemoteConfigParseError { ingot_url, error } => {
                write!(f, "Invalid remote fe.toml in {ingot_url}: {error}")
            }
            IngotInitDiagnostics::RemoteConfigDiagnostics {
                ingot_url,
                diagnostics,
            } => {
                if diagnostics.len() == 1 {
                    write!(
                        f,
                        "Invalid remote fe.toml in {ingot_url}: {}",
                        diagnostics[0]
                    )
                } else {
                    writeln!(f, "Invalid remote fe.toml in {ingot_url}:")?;
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

#[cfg(test)]
mod tests {
    use super::*;

    /// discover_and_init should not panic on a directory with no fe.toml.
    #[test]
    fn discover_and_init_empty_dir() {
        let tmp = tempfile::tempdir().unwrap();
        let url = Url::from_directory_path(tmp.path()).unwrap();
        let mut db = DriverDataBase::default();
        let result = discover_and_init(&mut db, &url);
        assert!(result.ingot_urls.is_empty());
        assert!(result.standalone_files.is_empty());
    }

    /// discover_and_init should find a single ingot at root.
    #[test]
    fn discover_and_init_single_ingot() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();
        std::fs::write(
            root.join("fe.toml"),
            "[ingot]\nname = \"test\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::create_dir_all(root.join("src")).unwrap();
        std::fs::write(root.join("src").join("lib.fe"), "pub fn foo() {}").unwrap();

        let url = Url::from_directory_path(root).unwrap();
        let mut db = DriverDataBase::default();
        let result = discover_and_init(&mut db, &url);
        assert_eq!(result.ingot_urls.len(), 1);
    }

    /// discover_and_init should scan downward and find child ingots when
    /// the root has no fe.toml.
    #[test]
    fn discover_and_init_scattered_ingots() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();

        // Create two child ingots under lessons/
        for name in &["alpha", "beta"] {
            let ingot_dir = root.join("lessons").join(name);
            std::fs::create_dir_all(ingot_dir.join("src")).unwrap();
            std::fs::write(
                ingot_dir.join("fe.toml"),
                format!("[ingot]\nname = \"{name}\"\nversion = \"0.1.0\"\n"),
            )
            .unwrap();
            std::fs::write(ingot_dir.join("src").join("lib.fe"), "pub fn f() {}").unwrap();
        }

        let url = Url::from_directory_path(root).unwrap();
        let mut db = DriverDataBase::default();
        let result = discover_and_init(&mut db, &url);
        assert_eq!(result.ingot_urls.len(), 2, "should find both child ingots");
    }

    /// discover_and_init should still scan downward when a workspace root
    /// is found but has no member ingots (e.g. `members = []`).
    #[test]
    fn discover_and_init_empty_workspace_scans_children() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();

        // Create a workspace with empty members
        std::fs::write(
            root.join("fe.toml"),
            "[workspace]\nmembers = []\n",
        )
        .unwrap();

        // Create a child ingot that should be discovered by downward scan
        let child = root.join("packages").join("child");
        std::fs::create_dir_all(child.join("src")).unwrap();
        std::fs::write(
            child.join("fe.toml"),
            "[ingot]\nname = \"child\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(child.join("src").join("lib.fe"), "pub fn f() {}").unwrap();

        let url = Url::from_directory_path(root).unwrap();
        let mut db = DriverDataBase::default();
        let result = discover_and_init(&mut db, &url);
        // Workspace root + the child ingot discovered by downward scan
        assert!(
            result.ingot_urls.len() >= 2,
            "should find workspace root and child ingot, got {}",
            result.ingot_urls.len()
        );
    }

    /// discover_and_init should not follow directory symlinks (cyclic links
    /// would cause infinite recursion).
    #[cfg(unix)]
    #[test]
    fn discover_and_init_skips_symlinks() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();

        // Create a directory with a cyclic symlink
        let subdir = root.join("lessons");
        std::fs::create_dir_all(&subdir).unwrap();
        std::os::unix::fs::symlink(&root, subdir.join("loop")).unwrap();

        // Create a standalone .fe file to prove we still scan non-symlink entries
        std::fs::write(root.join("hello.fe"), "pub fn hello() {}").unwrap();

        let url = Url::from_directory_path(root).unwrap();
        let mut db = DriverDataBase::default();
        // This would hang/overflow before the symlink fix
        let result = discover_and_init(&mut db, &url);
        assert_eq!(
            result.standalone_files.len(),
            1,
            "should find the standalone file without infinite recursion"
        );
    }

    /// discover_and_init should find standalone .fe files not under any ingot.
    #[test]
    fn discover_and_init_standalone_files() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();

        // Create a standalone .fe file
        let lessons = root.join("lessons").join("basics");
        std::fs::create_dir_all(&lessons).unwrap();
        std::fs::write(lessons.join("hello.fe"), "pub fn hello() {}").unwrap();

        // And an ingot alongside it
        let ingot_dir = root.join("lessons").join("myingot");
        std::fs::create_dir_all(ingot_dir.join("src")).unwrap();
        std::fs::write(
            ingot_dir.join("fe.toml"),
            "[ingot]\nname = \"myingot\"\nversion = \"0.1.0\"\n",
        )
        .unwrap();
        std::fs::write(ingot_dir.join("src").join("lib.fe"), "pub fn f() {}").unwrap();

        let url = Url::from_directory_path(root).unwrap();
        let mut db = DriverDataBase::default();
        let result = discover_and_init(&mut db, &url);
        assert_eq!(result.ingot_urls.len(), 1, "should find the ingot");
        assert_eq!(
            result.standalone_files.len(),
            1,
            "should find the standalone file"
        );
    }
}
