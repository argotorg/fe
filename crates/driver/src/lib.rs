#![allow(clippy::print_stderr)]

pub mod db;
pub mod diagnostics;
pub mod files;
mod ingot_handler;

pub use common::dependencies::DependencyTree;

use std::collections::{HashMap, HashSet};
use std::fs;

use camino::Utf8PathBuf;
use common::{
    InputDb,
    cache::remote_git_cache_dir,
    config::{Config, WorkspaceMemberSelection},
    ingot::Version,
};
pub use db::DriverDataBase;
use glob::glob;
use ingot_handler::IngotHandler;
use smol_str::SmolStr;

use hir::hir_def::TopLevelMod;
use resolver::{
    Resolver,
    files::FilesResolutionDiagnostic,
    git::{GitDescription, GitResolver},
    graph::{GraphResolver, GraphResolverImpl},
    ingot::{
        IngotDescriptor, IngotResolutionError, IngotResolver, RemoteProgress,
        WorkspaceResolutionDiagnostic, WorkspaceResolutionError, WorkspaceResolver,
        WorkspaceResource, project_files_resolver, workspace_files_resolver,
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

fn workspace_resolver() -> WorkspaceResolver {
    let files_resolver = workspace_files_resolver();
    WorkspaceResolver::new(files_resolver)
}

pub fn init_ingot(db: &mut DriverDataBase, ingot_url: &Url) -> bool {
    if should_use_workspace(ingot_url) {
        return init_workspace(db, ingot_url);
    }
    init_ingot_graph(db, ingot_url)
}

fn init_ingot_graph(db: &mut DriverDataBase, ingot_url: &Url) -> bool {
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

fn should_use_workspace(ingot_url: &Url) -> bool {
    manifest_config(ingot_url).is_some_and(|config| {
        config.workspace.is_some()
            && config.metadata.name.is_none()
            && config.metadata.version.is_none()
    })
}

fn manifest_config(ingot_url: &Url) -> Option<Config> {
    let mut path = ingot_url.to_file_path().ok()?;
    path.push("fe.toml");
    fs::read_to_string(path)
        .ok()
        .and_then(|content| Config::parse(&content).ok())
}

pub fn init_workspace(db: &mut DriverDataBase, workspace_url: &Url) -> bool {
    tracing::info!(target: "resolver", "Starting workspace resolution for: {}", workspace_url);
    let mut handler = WorkspaceHandler::new(db).with_stdout(true);
    let mut resolver = workspace_resolver();
    let resource = match resolver.resolve(&mut handler, workspace_url) {
        Ok(resource) => resource,
        Err(error) => {
            handler.report_error(IngotInitDiagnostics::WorkspaceResolutionError {
                workspace_url: workspace_url.clone(),
                error,
            });
            return true;
        }
    };

    let mut had_diagnostics = handler.had_diagnostics();

    let manifest_file = match resource
        .files
        .files
        .iter()
        .find(|file| file.path.as_str().ends_with("fe.toml"))
    {
        Some(file) => file,
        None => {
            handler.report_error(IngotInitDiagnostics::WorkspaceMembersError {
                workspace_url: workspace_url.clone(),
                error: "workspace manifest fe.toml not found".to_string(),
            });
            return true;
        }
    };

    let config = match Config::parse(&manifest_file.content) {
        Ok(config) => config,
        Err(error) => {
            handler.report_error(IngotInitDiagnostics::WorkspaceConfigParseError {
                workspace_url: workspace_url.clone(),
                error,
            });
            return true;
        }
    };

    if !config.diagnostics.is_empty() {
        handler.report_warn(IngotInitDiagnostics::WorkspaceDiagnostics {
            workspace_url: workspace_url.clone(),
            diagnostics: config.diagnostics.clone(),
        });
        had_diagnostics = true;
    }

    let Some(workspace) = config.workspace.clone() else {
        handler.report_error(IngotInitDiagnostics::WorkspaceMembersError {
            workspace_url: workspace_url.clone(),
            error: "missing [workspace] section in manifest".to_string(),
        });
        return true;
    };

    let member_selection = if workspace.default_members.is_some() {
        WorkspaceMemberSelection::DefaultOnly
    } else {
        WorkspaceMemberSelection::All
    };
    let members = match expand_workspace_members(&workspace, workspace_url, member_selection) {
        Ok(members) => members,
        Err(error) => {
            handler.report_error(IngotInitDiagnostics::WorkspaceMembersError {
                workspace_url: workspace_url.clone(),
                error,
            });
            return true;
        }
    };

    for member in &members {
        if let (Some(name), Some(version)) = (&member.name, &member.version) {
            db.dependency_graph().register_ingot_metadata(
                db,
                &member.url,
                name.clone(),
                version.clone(),
            );
        }
    }

    for member in members {
        if &member.url == workspace_url {
            continue;
        }
        let member_had_diagnostics = init_ingot_graph(db, &member.url);
        had_diagnostics |= member_had_diagnostics;
    }

    had_diagnostics
}

pub fn find_ingot_by_metadata(db: &DriverDataBase, name: &str, version: &Version) -> Option<Url> {
    db.dependency_graph()
        .ingot_by_name_version(db, &SmolStr::new(name), version)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ExpandedWorkspaceMember {
    url: Url,
    name: Option<SmolStr>,
    version: Option<Version>,
}

fn _dump_scope_graph(db: &DriverDataBase, top_mod: TopLevelMod) -> String {
    let mut s = vec![];
    top_mod.scope_graph(db).write_as_dot(db, &mut s).unwrap();
    String::from_utf8(s).unwrap()
}

struct WorkspaceHandler<'a> {
    db: &'a mut dyn InputDb,
    had_diagnostics: bool,
    trace_enabled: bool,
    stdout_enabled: bool,
}

impl<'a> WorkspaceHandler<'a> {
    fn new(db: &'a mut dyn InputDb) -> Self {
        Self {
            db,
            had_diagnostics: false,
            trace_enabled: true,
            stdout_enabled: false,
        }
    }

    fn with_stdout(mut self, stdout_enabled: bool) -> Self {
        self.stdout_enabled = stdout_enabled;
        self
    }

    fn had_diagnostics(&self) -> bool {
        self.had_diagnostics
    }

    fn report_warn(&mut self, diagnostic: IngotInitDiagnostics) {
        self.had_diagnostics = true;
        if self.trace_enabled {
            tracing::warn!(target: "resolver", "{diagnostic}");
        }
        if self.stdout_enabled {
            eprintln!("❌ {diagnostic}");
        }
    }

    fn report_error(&mut self, diagnostic: IngotInitDiagnostics) {
        self.had_diagnostics = true;
        if self.trace_enabled {
            tracing::error!(target: "resolver", "{diagnostic}");
        }
        if self.stdout_enabled {
            eprintln!("❌ {diagnostic}");
        }
    }
}

impl<'a> resolver::ResolutionHandler<WorkspaceResolver> for WorkspaceHandler<'a> {
    type Item = WorkspaceResource;

    fn on_resolution_diagnostic(&mut self, diagnostic: WorkspaceResolutionDiagnostic) {
        match diagnostic {
            WorkspaceResolutionDiagnostic::Files(diagnostic) => {
                self.report_warn(IngotInitDiagnostics::WorkspaceFileError { diagnostic });
            }
        }
    }

    fn on_resolution_error(&mut self, description: &Url, error: WorkspaceResolutionError) {
        self.report_error(IngotInitDiagnostics::WorkspaceResolutionError {
            workspace_url: description.clone(),
            error,
        });
    }

    fn handle_resolution(&mut self, _description: &Url, resource: WorkspaceResource) -> Self::Item {
        for file in &resource.files.files {
            let file_url =
                Url::from_file_path(file.path.as_std_path()).expect("resolved path to url");
            self.db
                .workspace()
                .touch(self.db, file_url, Some(file.content.clone()));
        }
        resource
    }
}

fn expand_workspace_members(
    workspace: &common::config::WorkspaceConfig,
    base_url: &Url,
    selection: WorkspaceMemberSelection,
) -> Result<Vec<ExpandedWorkspaceMember>, String> {
    let base_path_buf = base_url
        .to_file_path()
        .map_err(|_| "workspace URL is not a file URL".to_string())?;
    let base_path = Utf8PathBuf::from_path_buf(base_path_buf)
        .map_err(|_| "workspace path is not UTF-8".to_string())?;

    let mut excluded = HashSet::new();
    for pattern in &workspace.exclude {
        let pattern_path = base_path.join(pattern.as_str());
        let entries = glob(pattern_path.as_str())
            .map_err(|err| format!("Invalid exclude pattern \"{pattern}\": {err}"))?;
        for entry in entries {
            let path = entry
                .map_err(|err| format!("Glob error for exclude pattern \"{pattern}\": {err}"))?;
            if !path.starts_with(&base_path) {
                return Err(format!(
                    "Exclude pattern \"{pattern}\" escapes workspace root {base_path}"
                ));
            }
            if let Ok(path) = Utf8PathBuf::from_path_buf(path) {
                excluded.insert(path);
            }
        }
    }

    let mut members = Vec::new();
    let mut seen = HashSet::new();
    for spec in workspace.members_for_selection(selection) {
        let pattern = spec.path.as_str();
        if spec.name.is_some() || spec.version.is_some() {
            if pattern.contains(['*', '?', '[']) {
                return Err(format!(
                    "Member path \"{pattern}\" with name/version cannot contain glob patterns"
                ));
            }
            let path = base_path.join(pattern);
            if !path.starts_with(&base_path) {
                return Err(format!(
                    "Member path \"{pattern}\" escapes workspace root {base_path}"
                ));
            }
            if !path.is_dir() {
                continue;
            }
            if excluded.contains(&path) {
                continue;
            }
            let url = Url::from_directory_path(path.as_std_path())
                .map_err(|_| "failed to convert member path to URL".to_string())?;
            if seen.insert(url.clone()) {
                members.push(ExpandedWorkspaceMember {
                    url,
                    name: spec.name.clone(),
                    version: spec.version.clone(),
                });
            }
            continue;
        }

        let pattern_path = base_path.join(pattern);
        let entries = glob(pattern_path.as_str())
            .map_err(|err| format!("Invalid member pattern \"{pattern}\": {err}"))?;
        for entry in entries {
            let path = entry
                .map_err(|err| format!("Glob error for member pattern \"{pattern}\": {err}"))?;
            if !path.starts_with(&base_path) {
                return Err(format!(
                    "Member pattern \"{pattern}\" escapes workspace root {base_path}"
                ));
            }
            if !path.is_dir() {
                continue;
            }
            let utf8_path = Utf8PathBuf::from_path_buf(path)
                .map_err(|_| "member path is not UTF-8".to_string())?;
            if excluded.contains(&utf8_path) {
                continue;
            }
            let url = Url::from_directory_path(utf8_path.as_std_path())
                .map_err(|_| "failed to convert member path to URL".to_string())?;
            if seen.insert(url.clone()) {
                members.push(ExpandedWorkspaceMember {
                    url,
                    name: None,
                    version: None,
                });
            }
        }
    }

    Ok(members)
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
    WorkspaceResolutionError {
        workspace_url: Url,
        error: WorkspaceResolutionError,
    },
    WorkspaceFileError {
        diagnostic: FilesResolutionDiagnostic,
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
            IngotInitDiagnostics::WorkspaceResolutionError {
                workspace_url,
                error,
            } => {
                write!(f, "Workspace resolution error at {workspace_url}: {error}")
            }
            IngotInitDiagnostics::WorkspaceFileError { diagnostic } => {
                write!(f, "Workspace file resolution error: {diagnostic}")
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
