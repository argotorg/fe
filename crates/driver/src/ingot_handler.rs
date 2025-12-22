use std::{
    collections::{HashMap, HashSet},
    fs,
};

use camino::{Utf8Path, Utf8PathBuf};
use common::{
    InputDb,
    config::{Config, ConfigDiagnostic, WorkspaceMemberSelection},
    dependencies::{
        DependencyAlias, DependencyArguments, DependencyLocation, RemoteFiles,
        WorkspaceMemberRecord,
    },
};
use resolver::{
    ResolutionHandler,
    files::FilesResource,
    git::{GitDescription, GitResolver},
    graph::{DiGraph, GraphResolutionHandler, UnresolvedNode, petgraph::visit::EdgeRef},
    ingot::{IngotDescriptor, IngotOrigin, IngotPriority, IngotResolver, IngotResource},
};
use smol_str::SmolStr;
use url::Url;

use crate::{IngotInitDiagnostics, remote_checkout_root};

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
    let mut config_url = workspace_url.clone();
    config_url.set_path(&format!("{}fe.toml", workspace_url.path()));
    let file = db.workspace().get(db, &config_url)?;
    let config_file = Config::parse(file.text(db)).ok()?;
    match config_file {
        Config::Workspace(workspace_config) => workspace_config.workspace.version,
        Config::Ingot(_) => None,
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

            if matches!(dependency.location, DependencyLocation::WorkspaceCurrent) {
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
                } else {
                    self.report_error(IngotInitDiagnostics::WorkspaceNameLookupUnavailable {
                        ingot_url: ingot_url.clone(),
                        dependency: dependency.alias,
                    });
                }
                return None;
            }
        }

        match dependency.location {
            DependencyLocation::Local(local) => match origin {
                IngotOrigin::Local => {
                    if let Ok(config) = config_at_url(&local.url)
                        && matches!(config, Config::Workspace(_))
                    {
                        self.report_error(IngotInitDiagnostics::WorkspacePathRequiresSelection {
                            ingot_url: ingot_url.clone(),
                            dependency: dependency.alias.clone(),
                            workspace_url: local.url.clone(),
                        });
                        return None;
                    }
                    Some((
                        IngotDescriptor::Local(local.url),
                        (dependency.alias, dependency.arguments),
                    ))
                }
                IngotOrigin::Remote {
                    description,
                    checkout_path,
                    ..
                } => match relative_path_within_checkout(checkout_path.as_path(), &local.url) {
                    Ok(relative_path) => {
                        if let Ok(config) = config_at_url(&local.url)
                            && matches!(config, Config::Workspace(_))
                        {
                            self.report_error(
                                IngotInitDiagnostics::WorkspacePathRequiresSelection {
                                    ingot_url: ingot_url.clone(),
                                    dependency: dependency.alias.clone(),
                                    workspace_url: local.url.clone(),
                                },
                            );
                            return None;
                        }
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
                if let Some((config, path_url)) = self.config_at_remote_path(ingot_url, &remote)
                    && matches!(config, Config::Workspace(_))
                {
                    self.report_error(IngotInitDiagnostics::WorkspacePathRequiresSelection {
                        ingot_url: ingot_url.clone(),
                        dependency: dependency.alias.clone(),
                        workspace_url: path_url,
                    });
                    return None;
                }
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
                match self.select_workspace_member(&workspace_root, name, version) {
                    Ok(member) => Some(IngotDescriptor::Local(member.url)),
                    Err(error) => {
                        self.report_error(IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                            ingot_url: ingot_url.clone(),
                            dependency: dependency.alias.clone(),
                            error,
                        });
                        None
                    }
                }
            }
            DependencyLocation::Local(local) => {
                if let Ok(config) = config_at_url(&local.url) {
                    match config {
                        Config::Ingot(ingot) => {
                            if self.verify_dependency_metadata(
                                ingot_url,
                                &dependency.alias,
                                &local.url,
                                &ingot,
                                name,
                                version,
                            ) {
                                return Some(IngotDescriptor::Local(local.url.clone()));
                            }
                            return None;
                        }
                        Config::Workspace(_) => {}
                    }
                }
                match self.select_workspace_member(&local.url, name, version) {
                    Ok(member) => Some(IngotDescriptor::Local(member.url)),
                    Err(error) => {
                        if error == "target is not a workspace" {
                            None
                        } else {
                            self.report_error(
                                IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                                    ingot_url: ingot_url.clone(),
                                    dependency: dependency.alias.clone(),
                                    error,
                                },
                            );
                            None
                        }
                    }
                }
            }
            DependencyLocation::Remote(remote) => {
                let git = GitResolver::new(remote_checkout_root(ingot_url));
                let description =
                    GitDescription::new(remote.source.clone(), remote.rev.to_string());
                let checkout = match git.ensure_checkout_resource(&description) {
                    Ok(checkout) => checkout,
                    Err(err) => {
                        self.report_error(IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                            ingot_url: ingot_url.clone(),
                            dependency: dependency.alias.clone(),
                            error: err.to_string(),
                        });
                        return None;
                    }
                };
                let workspace_root = match &remote.path {
                    Some(path) => checkout.checkout_path.join(path.as_str()),
                    None => checkout.checkout_path.clone(),
                };
                if !workspace_root.starts_with(&checkout.checkout_path) {
                    self.report_error(IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                        ingot_url: ingot_url.clone(),
                        dependency: dependency.alias.clone(),
                        error: "workspace path escapes checkout root".to_string(),
                    });
                    None
                } else if let Ok(config) = config_at_path(&workspace_root) {
                    match config {
                        Config::Ingot(ingot) => {
                            let url =
                                Url::from_directory_path(workspace_root.as_std_path()).ok()?;
                            if self.verify_dependency_metadata(
                                ingot_url,
                                &dependency.alias,
                                &url,
                                &ingot,
                                name,
                                version,
                            ) {
                                let mut descriptor = description.clone();
                                if let Some(path) = remote.path.clone() {
                                    descriptor = descriptor.with_path(path);
                                }
                                return Some(IngotDescriptor::Remote(descriptor));
                            }
                            None
                        }
                        Config::Workspace(_) => {
                            match self.select_workspace_member_at_path(
                                &workspace_root,
                                name,
                                version,
                            ) {
                                Ok(member) => {
                                    let member_path = if let Some(root) = remote.path.clone() {
                                        root.join(member.path.as_str())
                                    } else {
                                        member.path.clone()
                                    };
                                    let descriptor = description.with_path(member_path);
                                    Some(IngotDescriptor::Remote(descriptor))
                                }
                                Err(error) => {
                                    if error == "target is not a workspace" {
                                        None
                                    } else {
                                        self.report_error(
                                            IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                                                ingot_url: ingot_url.clone(),
                                                dependency: dependency.alias.clone(),
                                                error,
                                            },
                                        );
                                        None
                                    }
                                }
                            }
                        }
                    }
                } else {
                    match self.select_workspace_member_at_path(&workspace_root, name, version) {
                        Ok(member) => {
                            let member_path = if let Some(root) = remote.path.clone() {
                                root.join(member.path.as_str())
                            } else {
                                member.path.clone()
                            };
                            let descriptor = description.with_path(member_path);
                            Some(IngotDescriptor::Remote(descriptor))
                        }
                        Err(error) => {
                            if error == "target is not a workspace" {
                                None
                            } else {
                                self.report_error(
                                    IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                                        ingot_url: ingot_url.clone(),
                                        dependency: dependency.alias.clone(),
                                        error,
                                    },
                                );
                                None
                            }
                        }
                    }
                }
            }
        }
    }

    fn verify_dependency_metadata(
        &mut self,
        ingot_url: &Url,
        dependency: &SmolStr,
        dependency_url: &Url,
        config: &common::config::IngotConfig,
        expected_name: &SmolStr,
        expected_version: Option<&common::ingot::Version>,
    ) -> bool {
        let name_matches = config.metadata.name.as_ref() == Some(expected_name);
        let version_matches = match expected_version {
            Some(version) => config.metadata.version.as_ref() == Some(version),
            None => true,
        };

        if name_matches && version_matches {
            return true;
        }

        self.report_error(IngotInitDiagnostics::DependencyMetadataMismatch {
            ingot_url: ingot_url.clone(),
            dependency: dependency.clone(),
            dependency_url: dependency_url.clone(),
            expected_name: expected_name.clone(),
            expected_version: expected_version.cloned(),
            found_name: config.metadata.name.clone(),
            found_version: config.metadata.version.clone(),
        });
        false
    }

    fn workspace_member_metadata(
        &self,
        member: &crate::ExpandedWorkspaceMember,
    ) -> Result<(Option<SmolStr>, Option<common::ingot::Version>), String> {
        if member.name.is_some() && member.version.is_some() {
            return Ok((member.name.clone(), member.version.clone()));
        }
        let config = config_at_url(&member.url)?;
        let Config::Ingot(ingot) = config else {
            return Err(format!("Expected ingot config at {}", member.url));
        };
        let name = member.name.clone().or(ingot.metadata.name.clone());
        let version = member.version.clone().or(ingot.metadata.version.clone());
        Ok((name, version))
    }

    fn ensure_workspace_registry(&mut self, workspace_root: &Url) -> Result<(), String> {
        if !self
            .db
            .dependency_graph()
            .workspace_member_records(self.db, workspace_root)
            .is_empty()
        {
            return Ok(());
        }

        let config = config_at_url(workspace_root)?;
        let Config::Workspace(workspace_config) = config else {
            return Err("target is not a workspace".to_string());
        };
        let members = crate::expand_workspace_members(
            &workspace_config.workspace,
            workspace_root,
            WorkspaceMemberSelection::All,
        )?;

        for mut member in members {
            let explicit_name = member.name.clone();
            let explicit_version = member.version.clone();
            let (name, version) = self.workspace_member_metadata(&member)?;
            member.name = name;
            member.version = version;
            self.db.dependency_graph().register_workspace_member_root(
                self.db,
                workspace_root,
                &member.url,
            );
            if let (Some(name), Some(version)) = (explicit_name, explicit_version) {
                self.db
                    .dependency_graph()
                    .register_expected_member_metadata(self.db, &member.url, name, version);
            }
            if let Some(name) = &member.name {
                let existing = self.db.dependency_graph().workspace_members_by_name(
                    self.db,
                    workspace_root,
                    name,
                );
                if existing.iter().any(|other| other.version == member.version) {
                    return Err(format!(
                        "workspace member {name} has duplicate version in {workspace_root}"
                    ));
                }
                let record = WorkspaceMemberRecord {
                    name: name.clone(),
                    version: member.version.clone(),
                    path: member.path.clone(),
                    url: member.url.clone(),
                };
                self.db.dependency_graph().register_workspace_member(
                    self.db,
                    workspace_root,
                    record,
                );
            }
        }

        Ok(())
    }

    fn select_workspace_member(
        &mut self,
        workspace_root: &Url,
        name: &SmolStr,
        version: Option<&common::ingot::Version>,
    ) -> Result<WorkspaceMemberRecord, String> {
        self.ensure_workspace_registry(workspace_root)?;
        let candidates =
            self.db
                .dependency_graph()
                .workspace_members_by_name(self.db, workspace_root, name);
        if candidates.is_empty() {
            return Err(format!(
                "No workspace member named \"{name}\" found in {workspace_root}"
            ));
        }

        if let Some(version) = version {
            let exact: Vec<_> = candidates
                .iter()
                .filter(|member| member.version.as_ref() == Some(version))
                .cloned()
                .collect();
            if exact.len() == 1 {
                return Ok(exact[0].clone());
            }
            if exact.len() > 1 {
                return Err(format!(
                    "Multiple workspace members named \"{name}\" with version {version}"
                ));
            }
            return Err(format!(
                "No workspace member named \"{name}\" with version {version}"
            ));
        }

        if candidates.len() == 1 {
            return Ok(candidates[0].clone());
        }

        Err(format!(
            "Multiple workspace members named \"{name}\"; specify a version"
        ))
    }

    fn select_workspace_member_at_path(
        &mut self,
        workspace_root: &Utf8PathBuf,
        name: &SmolStr,
        version: Option<&common::ingot::Version>,
    ) -> Result<WorkspaceMemberRecord, String> {
        let workspace_url = Url::from_directory_path(workspace_root.as_std_path())
            .map_err(|_| "failed to convert workspace checkout path to URL".to_string())?;
        self.select_workspace_member(&workspace_url, name, version)
    }

    fn handle_workspace_config(
        &mut self,
        resource: &IngotResource,
        workspace_config: common::config::WorkspaceConfig,
    ) -> Vec<UnresolvedNode<IngotPriority, IngotDescriptor, (DependencyAlias, DependencyArguments)>>
    {
        if !workspace_config.diagnostics.is_empty() {
            self.report_warn(IngotInitDiagnostics::WorkspaceDiagnostics {
                workspace_url: resource.ingot_url.clone(),
                diagnostics: workspace_config.diagnostics.clone(),
            });
        }

        let workspace = workspace_config.workspace.clone();
        let selection = if workspace.default_members.is_some() {
            WorkspaceMemberSelection::DefaultOnly
        } else {
            WorkspaceMemberSelection::All
        };
        let mut members =
            match crate::expand_workspace_members(&workspace, &resource.ingot_url, selection) {
                Ok(members) => members,
                Err(error) => {
                    self.report_error(IngotInitDiagnostics::WorkspaceMembersError {
                        workspace_url: resource.ingot_url.clone(),
                        error,
                    });
                    return Vec::new();
                }
            };

        for member in &mut members {
            let explicit_name = member.name.clone();
            let explicit_version = member.version.clone();
            let (name, version) = match self.workspace_member_metadata(member) {
                Ok(metadata) => metadata,
                Err(error) => {
                    self.report_error(IngotInitDiagnostics::WorkspaceMembersError {
                        workspace_url: resource.ingot_url.clone(),
                        error,
                    });
                    return Vec::new();
                }
            };
            member.name = name;
            member.version = version;
            self.db.dependency_graph().register_workspace_member_root(
                self.db,
                &resource.ingot_url,
                &member.url,
            );
            if let (Some(name), Some(version)) = (explicit_name, explicit_version) {
                self.db
                    .dependency_graph()
                    .register_expected_member_metadata(
                        self.db,
                        &member.url,
                        name.clone(),
                        version.clone(),
                    );
            }

            if let Some(name) = &member.name {
                let existing = self.db.dependency_graph().workspace_members_by_name(
                    self.db,
                    &resource.ingot_url,
                    name,
                );
                if existing.iter().any(|other| other.version == member.version) {
                    self.report_error(IngotInitDiagnostics::WorkspaceMemberDuplicate {
                        workspace_url: resource.ingot_url.clone(),
                        name: name.clone(),
                        version: member.version.clone(),
                    });
                    return Vec::new();
                }
                let record = WorkspaceMemberRecord {
                    name: name.clone(),
                    version: member.version.clone(),
                    path: member.path.clone(),
                    url: member.url.clone(),
                };
                self.db.dependency_graph().register_workspace_member(
                    self.db,
                    &resource.ingot_url,
                    record,
                );
            }
        }

        let mut dependencies = Vec::new();
        for member in members {
            if member.url == resource.ingot_url {
                continue;
            }
            let arguments = DependencyArguments {
                name: member.name.clone(),
                version: member.version.clone(),
            };
            let alias = member
                .name
                .clone()
                .unwrap_or_else(|| SmolStr::new(member.path.as_str()));
            let descriptor = match &resource.origin {
                IngotOrigin::Local => IngotDescriptor::Local(member.url.clone()),
                IngotOrigin::Remote { description, .. } => {
                    let mut member_path = member.path.clone();
                    if let Some(root_path) = &description.path {
                        member_path = root_path.join(member_path.as_str());
                    }
                    let next_description =
                        GitDescription::new(description.source.clone(), description.rev.clone())
                            .with_path(member_path);
                    IngotDescriptor::Remote(next_description)
                }
            };
            let priority = match descriptor {
                IngotDescriptor::Local(_) => IngotPriority::local(),
                IngotDescriptor::Remote(_) => IngotPriority::remote(),
            };
            dependencies.push(UnresolvedNode {
                priority,
                description: descriptor,
                edge: (alias, arguments),
            });
        }

        dependencies
    }

    fn config_at_remote_path(
        &mut self,
        ingot_url: &Url,
        remote: &RemoteFiles,
    ) -> Option<(Config, Url)> {
        let git = GitResolver::new(remote_checkout_root(ingot_url));
        let description = GitDescription::new(remote.source.clone(), remote.rev.to_string());
        let checkout = git.ensure_checkout_resource(&description).ok()?;
        let base_path = match &remote.path {
            Some(path) => checkout.checkout_path.join(path.as_str()),
            None => checkout.checkout_path.clone(),
        };
        let url = Url::from_directory_path(base_path.as_std_path()).ok()?;
        let config = config_at_path(&base_path).ok()?;
        Some((config, url))
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

        let config = match config_at_url(&resource.ingot_url) {
            Ok(config) => config,
            Err(error) => {
                match &resource.origin {
                    IngotOrigin::Local => {
                        if looks_like_workspace_config(&resource.files, &resource.ingot_url) {
                            self.report_error(IngotInitDiagnostics::WorkspaceConfigParseError {
                                workspace_url: resource.ingot_url.clone(),
                                error,
                            });
                        } else {
                            self.report_error(IngotInitDiagnostics::ConfigParseError {
                                ingot_url: resource.ingot_url.clone(),
                                error,
                            });
                        }
                    }
                    IngotOrigin::Remote { .. } => {
                        self.report_error(IngotInitDiagnostics::RemoteConfigParseError {
                            ingot_url: resource.ingot_url.clone(),
                            error,
                        });
                    }
                }
                return Vec::new();
            }
        };

        if let Config::Workspace(workspace_config) = config {
            return self.handle_workspace_config(&resource, *workspace_config);
        }

        let Config::Ingot(mut config) = config else {
            return Vec::new();
        };

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
        let mut registered_nodes = HashSet::new();
        for node_idx in graph.node_indices() {
            if let Some(url) = self.ingot_urls.get(&graph[node_idx])
                && registered_nodes.insert(url.clone())
            {
                self.db.dependency_graph().ensure_node(self.db, url);
            }
        }

        let mut registered_edges = HashSet::new();
        for edge in graph.edge_references() {
            if let (Some(from_url), Some(to_url)) = (
                self.ingot_urls.get(&graph[edge.source()]),
                self.ingot_urls.get(&graph[edge.target()]),
            ) {
                let (alias, arguments) = edge.weight();
                if registered_edges.insert((
                    from_url.clone(),
                    to_url.clone(),
                    alias.clone(),
                    arguments.clone(),
                )) {
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
}

fn config_at_url(url: &Url) -> Result<Config, String> {
    let path = url
        .to_file_path()
        .map_err(|_| "dependency URL is not a file path".to_string())?;
    let path =
        Utf8PathBuf::from_path_buf(path).map_err(|_| "dependency path is not UTF-8".to_string())?;
    config_at_path(&path)
}

fn config_at_path(path: &Utf8Path) -> Result<Config, String> {
    let config_path = path.join("fe.toml");
    let content = fs::read_to_string(config_path.as_std_path())
        .map_err(|err| format!("Failed to read {}: {err}", config_path))?;
    Config::parse(&content).map_err(|err| format!("Failed to parse {config_path}: {err}"))
}

fn looks_like_workspace_config(files: &FilesResource, root_url: &Url) -> bool {
    let root_path = Utf8PathBuf::from(root_url.path());
    let config_path = root_path.join("fe.toml");
    let Some(file) = files.files.iter().find(|file| file.path == config_path) else {
        return false;
    };
    for line in file.content.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("[workspace]") {
            return true;
        }
        if trimmed.starts_with("members")
            || trimmed.starts_with("default-members")
            || trimmed.starts_with("exclude")
        {
            return true;
        }
    }
    false
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
