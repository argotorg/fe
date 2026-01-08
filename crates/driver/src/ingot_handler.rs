use std::{collections::HashMap, fs};

use camino::{Utf8Path, Utf8PathBuf};
use common::{
    InputDb,
    config::{ConfigDiagnostic, Manifest},
    dependencies::{DependencyAlias, DependencyArguments, DependencyLocation, RemoteFiles},
};
use resolver::{
    ResolutionHandler,
    git::{GitDescription, GitResolver},
    graph::{DiGraph, GraphResolutionHandler, UnresolvedNode, petgraph::visit::EdgeRef},
    ingot::{IngotDescriptor, IngotOrigin, IngotPriority, IngotResolver, IngotResource},
};
use smol_str::SmolStr;
use url::Url;

use crate::{
    IngotInitDiagnostics, remote_checkout_root,
    workspace_lookup::{resolve_local_workspace_member, resolve_remote_workspace_member},
};

pub struct IngotHandler<'a> {
    pub db: &'a mut dyn InputDb,
    ingot_urls: HashMap<IngotDescriptor, Url>,
    root_ingot_url: Option<Url>,
    had_diagnostics: bool,
    trace_enabled: bool,
    stdout_enabled: bool,
}

fn workspace_version_for_member(
    db: &dyn InputDb,
    ingot_url: &Url,
) -> Option<common::ingot::Version> {
    let workspace_url = db
        .dependency_graph()
        .workspace_root_for_member(db, ingot_url)?;
    let mut path = workspace_url.to_file_path().ok()?;
    path.push("fe.toml");
    let content = fs::read_to_string(path).ok()?;
    let manifest = Manifest::parse(&content).ok()?;
    match manifest {
        Manifest::Workspace(workspace_manifest) => workspace_manifest.workspace.version,
        Manifest::Ingot(_) => None,
    }
}

impl<'a> IngotHandler<'a> {
    pub fn new(db: &'a mut dyn InputDb) -> Self {
        Self {
            db,
            ingot_urls: HashMap::new(),
            root_ingot_url: None,
            had_diagnostics: false,
            trace_enabled: true,
            stdout_enabled: false,
        }
    }

    pub fn with_stdout(mut self, stdout_enabled: bool) -> Self {
        self.stdout_enabled = stdout_enabled;
        self
    }

    pub fn had_diagnostics(&self) -> bool {
        self.had_diagnostics
    }

    pub fn logging_modes(&self) -> (bool, bool) {
        (self.trace_enabled, self.stdout_enabled)
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

    fn record_files(&mut self, resource: &IngotResource) -> Option<()> {
        let mut has_config = false;
        for file in &resource.files.files {
            let file_url =
                Url::from_file_path(file.path.as_std_path()).expect("resolved path to url");
            has_config |= file.path.as_str().ends_with("fe.toml");
            self.db
                .workspace()
                .touch(self.db, file_url, Some(file.content.clone()));
        }
        has_config.then_some(())
    }

    fn register_remote_mapping(&mut self, ingot_url: &Url, origin: &IngotOrigin) {
        if let IngotOrigin::Remote { description, .. } = origin {
            let remote = RemoteFiles {
                source: description.source.clone(),
                rev: SmolStr::new(description.rev.clone()),
                path: description.path.clone(),
            };
            self.db
                .dependency_graph()
                .register_remote_checkout(self.db, ingot_url.clone(), remote);
        }
    }

    fn convert_dependency(
        &mut self,
        ingot_url: &Url,
        origin: &IngotOrigin,
        dependency: common::dependencies::Dependency,
    ) -> Option<(IngotDescriptor, (DependencyAlias, DependencyArguments))> {
        if let Some(name) = dependency.arguments.name.clone() {
            if let Some(result) = self.resolve_workspace_dependency(ingot_url, &dependency, &name) {
                return Some((result, (dependency.alias, dependency.arguments)));
            }

            if let Some(version) = dependency.arguments.version.clone() {
                if let Some(url) = self
                    .db
                    .dependency_graph()
                    .ingot_by_name_version(self.db, &name, &version)
                {
                    return Some((
                        IngotDescriptor::Local(url),
                        (dependency.alias, dependency.arguments),
                    ));
                }
                self.report_error(IngotInitDiagnostics::IngotByNameResolutionFailed {
                    ingot_url: ingot_url.clone(),
                    dependency: dependency.alias,
                    name,
                    version,
                });
                return None;
            }

            if matches!(dependency.location, DependencyLocation::WorkspaceCurrent) {
                self.report_error(IngotInitDiagnostics::WorkspaceNameLookupUnavailable {
                    ingot_url: ingot_url.clone(),
                    dependency: dependency.alias,
                });
                return None;
            }
        }

        match dependency.location {
            DependencyLocation::Local(local) => match origin {
                IngotOrigin::Local => Some((
                    IngotDescriptor::Local(local.url),
                    (dependency.alias, dependency.arguments),
                )),
                IngotOrigin::Remote {
                    description,
                    checkout_path,
                    ..
                } => match relative_path_within_checkout(checkout_path.as_path(), &local.url) {
                    Ok(relative_path) => {
                        let mut next_description = GitDescription::new(
                            description.source.clone(),
                            description.rev.clone(),
                        );
                        if let Some(path) = relative_path {
                            next_description = next_description.with_path(path);
                        }
                        Some((
                            IngotDescriptor::Remote(next_description),
                            (dependency.alias, dependency.arguments),
                        ))
                    }
                    Err(error) => {
                        self.report_error(IngotInitDiagnostics::RemotePathResolutionError {
                            ingot_url: ingot_url.clone(),
                            dependency: dependency.alias,
                            error,
                        });
                        None
                    }
                },
            },
            DependencyLocation::Remote(remote) => {
                let mut next_description =
                    GitDescription::new(remote.source.clone(), remote.rev.to_string());
                if let Some(path) = remote.path.clone() {
                    next_description = next_description.with_path(path);
                }
                Some((
                    IngotDescriptor::Remote(next_description),
                    (dependency.alias, dependency.arguments),
                ))
            }
            DependencyLocation::WorkspaceCurrent => None,
        }
    }

    fn resolve_workspace_dependency(
        &mut self,
        ingot_url: &Url,
        dependency: &common::dependencies::Dependency,
        name: &SmolStr,
    ) -> Option<IngotDescriptor> {
        let version = dependency.arguments.version.as_ref();
        let path_hint = match &dependency.location {
            DependencyLocation::Remote(remote) => remote.path.as_ref(),
            _ => None,
        };

        match &dependency.location {
            DependencyLocation::WorkspaceCurrent => {
                let Some(workspace_root) = self
                    .db
                    .dependency_graph()
                    .workspace_root_for_member(self.db, ingot_url)
                else {
                    self.report_error(IngotInitDiagnostics::WorkspaceNameLookupUnavailable {
                        ingot_url: ingot_url.clone(),
                        dependency: dependency.alias.clone(),
                    });
                    return None;
                };
                match resolve_local_workspace_member(self.db, &workspace_root, name, version, None)
                {
                    Ok(result) => Some(IngotDescriptor::Local(result.member.url)),
                    Err(err) => {
                        self.report_error(IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                            ingot_url: ingot_url.clone(),
                            dependency: dependency.alias.clone(),
                            error: err.to_string(),
                        });
                        None
                    }
                }
            }
            DependencyLocation::Local(local) => {
                match resolve_local_workspace_member(self.db, &local.url, name, version, None) {
                    Ok(result) => Some(IngotDescriptor::Local(result.member.url)),
                    Err(err) => match err {
                        crate::workspace_lookup::WorkspaceLookupError::NotWorkspace => None,
                        _ => {
                            self.report_error(
                                IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                                    ingot_url: ingot_url.clone(),
                                    dependency: dependency.alias.clone(),
                                    error: err.to_string(),
                                },
                            );
                            None
                        }
                    },
                }
            }
            DependencyLocation::Remote(remote) => {
                let git = GitResolver::new(remote_checkout_root(ingot_url));
                match resolve_remote_workspace_member(
                    self.db,
                    &git,
                    &GitDescription::new(remote.source.clone(), remote.rev.to_string()),
                    name,
                    version,
                    path_hint,
                ) {
                    Ok(result) => {
                        let member_path = result.member.path.clone();
                        let descriptor =
                            GitDescription::new(remote.source.clone(), remote.rev.to_string())
                                .with_path(member_path);
                        Some(IngotDescriptor::Remote(descriptor))
                    }
                    Err(err) => match err {
                        crate::workspace_lookup::WorkspaceLookupError::NotWorkspace => None,
                        _ => {
                            self.report_error(
                                IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                                    ingot_url: ingot_url.clone(),
                                    dependency: dependency.alias.clone(),
                                    error: err.to_string(),
                                },
                            );
                            None
                        }
                    },
                }
            }
        }
    }
}

impl<'a> ResolutionHandler<IngotResolver> for IngotHandler<'a> {
    type Item =
        Vec<UnresolvedNode<IngotPriority, IngotDescriptor, (DependencyAlias, DependencyArguments)>>;

    fn on_resolution_start(&mut self, description: &IngotDescriptor) {
        if self.root_ingot_url.is_none()
            && let IngotDescriptor::Local(url) = description
        {
            self.root_ingot_url = Some(url.clone());
        }
        if matches!(description, IngotDescriptor::Remote(_)) {
            if self.trace_enabled {
                tracing::info!(target: "resolver", "Checking out {description}");
            }
            if self.stdout_enabled {
                eprintln!("Checking out {description}");
            }
        }
    }

    fn on_resolution_diagnostic(&mut self, diagnostic: resolver::ingot::IngotResolutionDiagnostic) {
        match diagnostic {
            resolver::ingot::IngotResolutionDiagnostic::Files(diagnostic) => {
                self.report_warn(IngotInitDiagnostics::FileError { diagnostic });
            }
            resolver::ingot::IngotResolutionDiagnostic::Git(diagnostic) => {
                if let Some(ingot_url) = self.root_ingot_url.clone() {
                    self.report_error(IngotInitDiagnostics::RemoteFileError {
                        ingot_url,
                        error: diagnostic.to_string(),
                    });
                }
            }
        }
    }

    fn on_resolution_error(
        &mut self,
        description: &IngotDescriptor,
        error: resolver::ingot::IngotResolutionError,
    ) {
        if matches!(description, IngotDescriptor::Remote(_)) {
            if self.trace_enabled {
                tracing::error!(
                    target: "resolver",
                    "❌ Failed to check out {description}: {error}"
                );
            }
            if self.stdout_enabled {
                eprintln!("❌ Failed to check out {description}: {error}");
            }
        }
        match description {
            IngotDescriptor::Local(target) => {
                self.report_error(IngotInitDiagnostics::UnresolvableIngotDependency {
                    target: target.clone(),
                    error,
                })
            }
            IngotDescriptor::Remote(target) => {
                self.report_error(IngotInitDiagnostics::UnresolvableRemoteDependency {
                    target: target.clone(),
                    error,
                })
            }
        };
    }

    fn handle_resolution(
        &mut self,
        descriptor: &IngotDescriptor,
        resource: IngotResource,
    ) -> Self::Item {
        if let IngotOrigin::Remote {
            reused_checkout, ..
        } = &resource.origin
        {
            if *reused_checkout {
                // Skip noisy checkout logs when using cached repositories.
            } else {
                if self.trace_enabled {
                    tracing::info!(target: "resolver", "✅ Checked out {}", resource.ingot_url);
                }
                if self.stdout_enabled {
                    eprintln!("✅ Checked out {}", resource.ingot_url);
                }
            }
        } else if matches!(descriptor, IngotDescriptor::Remote(_)) {
            if self.trace_enabled {
                tracing::info!(target: "resolver", "✅ Checked out {}", resource.ingot_url);
            }
            if self.stdout_enabled {
                eprintln!("✅ Checked out {}", resource.ingot_url);
            }
        }
        self.ingot_urls
            .insert(descriptor.clone(), resource.ingot_url.clone());
        self.register_remote_mapping(&resource.ingot_url, &resource.origin);

        if self.record_files(&resource).is_none() {
            match &resource.origin {
                IngotOrigin::Local => {}
                IngotOrigin::Remote { .. } => {
                    self.report_error(IngotInitDiagnostics::RemoteFileError {
                        ingot_url: resource.ingot_url.clone(),
                        error: "Remote ingot is missing fe.toml".into(),
                    })
                }
            }
            return Vec::new();
        }

        let Some(ingot) = self
            .db
            .workspace()
            .containing_ingot(self.db, resource.ingot_url.clone())
        else {
            if self.trace_enabled {
                tracing::error!("Unable to find ingot for {}", resource.ingot_url);
            }
            return Vec::new();
        };

        if let Some(error) = ingot.config_parse_error(self.db) {
            match &resource.origin {
                IngotOrigin::Local => self.report_error(IngotInitDiagnostics::ConfigParseError {
                    ingot_url: resource.ingot_url.clone(),
                    error,
                }),
                IngotOrigin::Remote { .. } => {
                    self.report_error(IngotInitDiagnostics::RemoteConfigParseError {
                        ingot_url: resource.ingot_url.clone(),
                        error,
                    })
                }
            };
            return Vec::new();
        }

        let Some(config) = ingot.config(self.db) else {
            return Vec::new();
        };

        let mut config = config.clone();
        let mut diagnostics = config.diagnostics.clone();

        if config.metadata.version.is_none()
            && let Some(version) = workspace_version_for_member(self.db, &resource.ingot_url)
        {
            config.metadata.version = Some(version);
            diagnostics.retain(|diag| !matches!(diag, ConfigDiagnostic::MissingVersion));
        }

        if !diagnostics.is_empty() {
            match &resource.origin {
                IngotOrigin::Local => self.report_warn(IngotInitDiagnostics::ConfigDiagnostics {
                    ingot_url: resource.ingot_url.clone(),
                    diagnostics,
                }),
                IngotOrigin::Remote { .. } => {
                    self.report_warn(IngotInitDiagnostics::RemoteConfigDiagnostics {
                        ingot_url: resource.ingot_url.clone(),
                        diagnostics,
                    })
                }
            };
        }

        self.db
            .dependency_graph()
            .ensure_node(self.db, &resource.ingot_url);

        if let Some((expected_name, expected_version)) = self
            .db
            .dependency_graph()
            .expected_member_metadata_for(self.db, &resource.ingot_url)
            && (config.metadata.name.as_ref() != Some(&expected_name)
                || config.metadata.version.as_ref() != Some(&expected_version))
        {
            self.report_error(IngotInitDiagnostics::WorkspaceMemberMetadataMismatch {
                ingot_url: resource.ingot_url.clone(),
                expected_name,
                expected_version,
                found_name: config.metadata.name.clone(),
                found_version: config.metadata.version.clone(),
            });
            return Vec::new();
        }

        if let (Some(name), Some(version)) = (
            config.metadata.name.clone(),
            config.metadata.version.clone(),
        ) {
            self.db.dependency_graph().register_ingot_metadata(
                self.db,
                &resource.ingot_url,
                name,
                version,
            );
        }

        let mut dependencies = Vec::new();
        for dependency in config.dependencies(&resource.ingot_url) {
            if let Some(converted) =
                self.convert_dependency(&resource.ingot_url, &resource.origin, dependency)
            {
                let priority = match converted.0 {
                    IngotDescriptor::Local(_) => IngotPriority::local(),
                    IngotDescriptor::Remote(_) => IngotPriority::remote(),
                };
                dependencies.push(UnresolvedNode {
                    priority,
                    description: converted.0,
                    edge: converted.1,
                });
            }
        }

        dependencies
    }
}

impl<'a>
    GraphResolutionHandler<
        IngotDescriptor,
        DiGraph<IngotDescriptor, (DependencyAlias, DependencyArguments)>,
    > for IngotHandler<'a>
{
    type Item = ();

    fn handle_graph_resolution(
        &mut self,
        _descriptor: &IngotDescriptor,
        graph: DiGraph<IngotDescriptor, (DependencyAlias, DependencyArguments)>,
    ) -> Self::Item {
        for node_idx in graph.node_indices() {
            if let Some(url) = self.ingot_urls.get(&graph[node_idx]) {
                self.db.dependency_graph().ensure_node(self.db, url);
            }
        }

        for edge in graph.edge_references() {
            if let (Some(from_url), Some(to_url)) = (
                self.ingot_urls.get(&graph[edge.source()]),
                self.ingot_urls.get(&graph[edge.target()]),
            ) {
                let (alias, arguments) = edge.weight();
                self.db.dependency_graph().add_dependency(
                    self.db,
                    from_url,
                    to_url,
                    alias.clone(),
                    arguments.clone(),
                );
            }
        }
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
