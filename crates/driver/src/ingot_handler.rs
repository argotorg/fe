use std::collections::{HashMap, HashSet};

use camino::{Utf8Path, Utf8PathBuf};
use common::{
    InputDb,
    config::{Config, ConfigDiagnostic, WorkspaceMemberSelection},
    dependencies::{
        DependencyAlias, DependencyArguments, DependencyLocation, LocalFiles, RemoteFiles,
        WorkspaceMemberRecord,
    },
    urlext::UrlExt,
};
use resolver::{
    ResolutionHandler,
    git::GitDescription,
    graph::{
        DiGraph, GraphNodeOutcome, GraphResolutionHandler, UnresolvedNode, petgraph::visit::EdgeRef,
    },
    ingot::{
        ConfigKind, IngotDescriptor, IngotOrigin, IngotPriority, IngotResolutionDiagnostic,
        IngotResolutionEvent, IngotResolverImpl, IngotResource,
    },
};
use smol_str::SmolStr;
use url::Url;

use crate::IngotInitDiagnostics;

pub struct IngotHandler<'a> {
    pub db: &'a mut dyn InputDb,
    ingot_urls: HashMap<IngotDescriptor, Url>,
    had_diagnostics: bool,
    trace_enabled: bool,
    stdout_enabled: bool,
    verbose_enabled: bool,
    reported_checkouts: HashSet<GitDescription>,
    dependency_contexts: HashMap<IngotDescriptor, Vec<DependencyContext>>,
    emitted_diagnostics: HashSet<String>,
}

#[derive(Clone, Debug)]
struct DependencyContext {
    from_ingot_url: Url,
    dependency: SmolStr,
}

fn workspace_version_for_member(
    db: &dyn InputDb,
    ingot_url: &Url,
) -> Option<common::ingot::Version> {
    let workspace_url = db
        .dependency_graph()
        .workspace_root_for_member(db, ingot_url)?;
    let config_url = workspace_url.join("fe.toml").ok()?;
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
            had_diagnostics: false,
            trace_enabled: true,
            stdout_enabled: false,
            verbose_enabled: false,
            reported_checkouts: HashSet::new(),
            dependency_contexts: HashMap::new(),
            emitted_diagnostics: HashSet::new(),
        }
    }

    pub fn with_stdout(mut self, stdout_enabled: bool) -> Self {
        self.stdout_enabled = stdout_enabled;
        self
    }

    pub fn with_verbose(mut self, verbose_enabled: bool) -> Self {
        self.verbose_enabled = verbose_enabled;
        self
    }

    pub fn had_diagnostics(&self) -> bool {
        self.had_diagnostics
    }

    pub fn logging_modes(&self) -> (bool, bool) {
        (self.trace_enabled, self.stdout_enabled)
    }

    fn record_dependency_context(
        &mut self,
        descriptor: &IngotDescriptor,
        from_ingot_url: &Url,
        dependency: &SmolStr,
    ) {
        let contexts = self
            .dependency_contexts
            .entry(descriptor.clone())
            .or_default();
        if contexts.iter().any(|context| {
            context.from_ingot_url == *from_ingot_url && context.dependency == *dependency
        }) {
            return;
        }
        contexts.push(DependencyContext {
            from_ingot_url: from_ingot_url.clone(),
            dependency: dependency.clone(),
        });
    }

    fn report_warn(&mut self, diagnostic: IngotInitDiagnostics) {
        let diagnostic_string = diagnostic.to_string();
        if !self.emitted_diagnostics.insert(diagnostic_string.clone()) {
            return;
        }
        self.had_diagnostics = true;
        if self.trace_enabled {
            tracing::warn!(target: "resolver", "{diagnostic_string}");
        }
        if self.stdout_enabled {
            eprintln!("Error: {diagnostic_string}");
        }
    }

    fn report_error(&mut self, diagnostic: IngotInitDiagnostics) {
        let diagnostic_string = diagnostic.to_string();
        if !self.emitted_diagnostics.insert(diagnostic_string.clone()) {
            return;
        }
        self.had_diagnostics = true;
        if self.trace_enabled {
            tracing::error!(target: "resolver", "{diagnostic_string}");
        }
        if self.stdout_enabled {
            eprintln!("Error: {diagnostic_string}");
        }
    }

    fn record_files(&mut self, files: &[resolver::files::File]) {
        for file in files {
            let file_url =
                Url::from_file_path(file.path.as_std_path()).expect("resolved path to url");
            self.db
                .workspace()
                .touch(self.db, file_url, Some(file.content.clone()));
        }
    }

    fn record_files_owned(&mut self, files: Vec<resolver::files::File>) {
        for file in files {
            let file_url =
                Url::from_file_path(file.path.as_std_path()).expect("resolved path to url");
            self.db
                .workspace()
                .touch(self.db, file_url, Some(file.content));
        }
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
        workspace_root: Option<&Url>,
        dependency: common::dependencies::Dependency,
    ) -> Option<(IngotDescriptor, (DependencyAlias, DependencyArguments))> {
        let common::dependencies::Dependency {
            alias,
            location,
            arguments,
        } = dependency;

        match location {
            DependencyLocation::WorkspaceCurrent => {
                let name = arguments.name.clone().unwrap_or_else(|| alias.clone());

                let Some(workspace_root) = workspace_root else {
                    self.report_error(IngotInitDiagnostics::WorkspaceNameLookupUnavailable {
                        ingot_url: ingot_url.clone(),
                        dependency: alias.clone(),
                    });
                    return None;
                };

                let workspace_current = common::dependencies::Dependency {
                    alias: alias.clone(),
                    location: DependencyLocation::WorkspaceCurrent,
                    arguments: arguments.clone(),
                };
                match self.workspace_dependency_for_alias(
                    ingot_url,
                    origin,
                    workspace_root,
                    &workspace_current,
                ) {
                    Ok(Some(descriptor)) => {
                        return Some((descriptor, (alias, arguments)));
                    }
                    Ok(None) => {}
                    Err(error) => {
                        self.report_error(IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                            ingot_url: ingot_url.clone(),
                            dependency: alias.clone(),
                            error,
                        });
                        return None;
                    }
                }

                match origin {
                    IngotOrigin::Local => {
                        let descriptor = IngotDescriptor::LocalByName {
                            base: workspace_root.clone(),
                            name,
                        };
                        self.record_dependency_context(&descriptor, ingot_url, &alias);
                        Some((descriptor, (alias, arguments)))
                    }
                    IngotOrigin::Remote {
                        description,
                        checkout_path,
                        ..
                    } => {
                        match relative_path_within_checkout(checkout_path.as_path(), workspace_root)
                        {
                            Ok(relative_path) => {
                                let mut base = GitDescription::new(
                                    description.source.clone(),
                                    description.rev.clone(),
                                );
                                if let Some(path) = relative_path {
                                    base = base.with_path(path);
                                }
                                let descriptor = IngotDescriptor::RemoteByName { base, name };
                                self.record_dependency_context(&descriptor, ingot_url, &alias);
                                Some((descriptor, (alias, arguments)))
                            }
                            Err(error) => {
                                self.report_error(
                                    IngotInitDiagnostics::RemotePathResolutionError {
                                        ingot_url: ingot_url.clone(),
                                        dependency: alias,
                                        error,
                                    },
                                );
                                None
                            }
                        }
                    }
                }
            }
            DependencyLocation::Local(local) => {
                if let Some(name) = arguments.name.clone() {
                    match origin {
                        IngotOrigin::Local => {
                            let descriptor = IngotDescriptor::LocalByName {
                                base: local.url,
                                name,
                            };
                            self.record_dependency_context(&descriptor, ingot_url, &alias);
                            Some((descriptor, (alias, arguments)))
                        }
                        IngotOrigin::Remote {
                            description,
                            checkout_path,
                            ..
                        } => {
                            match relative_path_within_checkout(checkout_path.as_path(), &local.url)
                            {
                                Ok(relative_path) => {
                                    let mut base = GitDescription::new(
                                        description.source.clone(),
                                        description.rev.clone(),
                                    );
                                    if let Some(path) = relative_path {
                                        base = base.with_path(path);
                                    }
                                    let descriptor = IngotDescriptor::RemoteByName { base, name };
                                    self.record_dependency_context(&descriptor, ingot_url, &alias);
                                    Some((descriptor, (alias, arguments)))
                                }
                                Err(error) => {
                                    self.report_error(
                                        IngotInitDiagnostics::RemotePathResolutionError {
                                            ingot_url: ingot_url.clone(),
                                            dependency: alias,
                                            error,
                                        },
                                    );
                                    None
                                }
                            }
                        }
                    }
                } else {
                    match origin {
                        IngotOrigin::Local => {
                            let descriptor = IngotDescriptor::Local(local.url);
                            self.record_dependency_context(&descriptor, ingot_url, &alias);
                            Some((descriptor, (alias, arguments)))
                        }
                        IngotOrigin::Remote {
                            description,
                            checkout_path,
                            ..
                        } => {
                            match relative_path_within_checkout(checkout_path.as_path(), &local.url)
                            {
                                Ok(relative_path) => {
                                    let mut next_description = GitDescription::new(
                                        description.source.clone(),
                                        description.rev.clone(),
                                    );
                                    if let Some(path) = relative_path {
                                        next_description = next_description.with_path(path);
                                    }
                                    let descriptor = IngotDescriptor::Remote(next_description);
                                    self.record_dependency_context(&descriptor, ingot_url, &alias);
                                    Some((descriptor, (alias, arguments)))
                                }
                                Err(error) => {
                                    self.report_error(
                                        IngotInitDiagnostics::RemotePathResolutionError {
                                            ingot_url: ingot_url.clone(),
                                            dependency: alias,
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
            DependencyLocation::Remote(remote) => {
                if let Some(name) = arguments.name.clone() {
                    let mut base =
                        GitDescription::new(remote.source.clone(), remote.rev.to_string());
                    if let Some(path) = remote.path.clone() {
                        base = base.with_path(path);
                    }
                    let descriptor = IngotDescriptor::RemoteByName { base, name };
                    self.record_dependency_context(&descriptor, ingot_url, &alias);
                    Some((descriptor, (alias, arguments)))
                } else {
                    let mut next_description =
                        GitDescription::new(remote.source.clone(), remote.rev.to_string());
                    if let Some(path) = remote.path.clone() {
                        next_description = next_description.with_path(path);
                    }
                    let descriptor = IngotDescriptor::Remote(next_description);
                    self.record_dependency_context(&descriptor, ingot_url, &alias);
                    Some((descriptor, (alias, arguments)))
                }
            }
        }
    }

    fn workspace_dependency_for_alias(
        &mut self,
        ingot_url: &Url,
        origin: &IngotOrigin,
        workspace_root: &Url,
        dependency: &common::dependencies::Dependency,
    ) -> Result<Option<IngotDescriptor>, String> {
        let config = self.config_at_url(workspace_root)?;
        let Config::Workspace(workspace_config) = config else {
            return Ok(None);
        };

        let Some(entry) = workspace_config
            .workspace
            .dependencies
            .iter()
            .find(|entry| entry.alias == dependency.alias)
        else {
            return Ok(None);
        };

        let location = match &entry.location {
            common::config::DependencyEntryLocation::RelativePath(path) => {
                let url = workspace_root
                    .join_directory(path)
                    .map_err(|_| format!("Failed to join workspace dependency path {path}"))?;
                DependencyLocation::Local(LocalFiles {
                    path: path.clone(),
                    url,
                })
            }
            common::config::DependencyEntryLocation::Remote(remote) => {
                DependencyLocation::Remote(remote.clone())
            }
            common::config::DependencyEntryLocation::WorkspaceCurrent => {
                return Err(format!(
                    "Workspace dependency '{}' must specify a path or a source",
                    entry.alias
                ));
            }
        };

        let mut arguments = entry.arguments.clone();
        if arguments.name.is_none() {
            arguments.name = dependency.arguments.name.clone();
        }
        if dependency.arguments.version.is_some() {
            arguments.version = dependency.arguments.version.clone();
        }

        let workspace_dependency = common::dependencies::Dependency {
            alias: entry.alias.clone(),
            location,
            arguments,
        };

        Ok(self
            .convert_dependency(
                ingot_url,
                origin,
                Some(workspace_root),
                workspace_dependency,
            )
            .map(|(descriptor, _)| descriptor))
    }

    fn workspace_member_metadata(
        &mut self,
        member: &crate::ExpandedWorkspaceMember,
    ) -> Result<(Option<SmolStr>, Option<common::ingot::Version>), String> {
        let config = self.config_at_url(&member.url)?;
        let Config::Ingot(ingot) = config else {
            return Err(format!("Expected ingot config at {}", member.url));
        };

        if let Some(expected_name) = member.name.as_ref()
            && ingot.metadata.name.as_ref() != Some(expected_name)
        {
            return Err(format!(
                "Workspace member {} has mismatched metadata: name expected {expected_name} but found {}",
                member.url,
                ingot.metadata.name.as_deref().unwrap_or("<missing name>")
            ));
        }

        if let Some(expected_version) = member.version.as_ref()
            && ingot.metadata.version.as_ref() != Some(expected_version)
        {
            return Err(format!(
                "Workspace member {} has mismatched metadata: version expected {expected_version} but found {}",
                member.url,
                ingot
                    .metadata
                    .version
                    .as_ref()
                    .map(ToString::to_string)
                    .unwrap_or_else(|| "<missing version>".to_string())
            ));
        }

        let name = member.name.clone().or(ingot.metadata.name.clone());
        let version = member.version.clone().or(ingot.metadata.version.clone());
        Ok((name, version))
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
        let workspace_dependency_aliases: HashSet<_> = workspace
            .dependencies
            .iter()
            .map(|dependency| dependency.alias.clone())
            .collect();
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
                if workspace_dependency_aliases.contains(name) {
                    self.report_error(IngotInitDiagnostics::WorkspaceDependencyAliasConflict {
                        workspace_url: resource.ingot_url.clone(),
                        alias: name.clone(),
                    });
                    return Vec::new();
                }
                let existing = self.db.dependency_graph().workspace_members_by_name(
                    self.db,
                    &resource.ingot_url,
                    name,
                );
                if existing.iter().any(|other| other.url != member.url) {
                    self.report_error(IngotInitDiagnostics::WorkspaceMemberDuplicate {
                        workspace_url: resource.ingot_url.clone(),
                        name: name.clone(),
                        version: None,
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
            let priority = match &descriptor {
                IngotDescriptor::Local(_) => IngotPriority::local(),
                IngotDescriptor::Remote(_) => IngotPriority::remote(),
                _ => unreachable!("workspace members must resolve to local or remote descriptors"),
            };
            dependencies.push(UnresolvedNode {
                priority,
                description: descriptor,
                edge: (alias, arguments),
            });
        }

        dependencies
    }

    fn config_at_url(&mut self, url: &Url) -> Result<Config, String> {
        let config_url = url
            .join("fe.toml")
            .map_err(|_| "failed to locate fe.toml for dependency".to_string())?;

        let file = self
            .db
            .workspace()
            .get(self.db, &config_url)
            .ok_or_else(|| format!("Missing config at {config_url}"))?;
        Config::parse(file.text(self.db))
            .map_err(|err| format!("Failed to parse {config_url}: {err}"))
    }

    fn effective_metadata_for_ingot(
        &mut self,
        ingot_url: &Url,
    ) -> Option<(Option<SmolStr>, Option<common::ingot::Version>)> {
        let config = self.config_at_url(ingot_url).ok()?;
        match config {
            Config::Ingot(ingot) => {
                let name = ingot.metadata.name.clone();
                let version = ingot
                    .metadata
                    .version
                    .clone()
                    .or_else(|| workspace_version_for_member(self.db, ingot_url));
                Some((name, version))
            }
            Config::Workspace(_) => None,
        }
    }
}

impl<'a> ResolutionHandler<IngotResolverImpl> for IngotHandler<'a> {
    type Item = Result<
        GraphNodeOutcome<IngotPriority, IngotDescriptor, (DependencyAlias, DependencyArguments)>,
        resolver::ingot::IngotResolutionError,
    >;

    fn on_resolution_diagnostic(&mut self, diagnostic: IngotResolutionDiagnostic) {
        match diagnostic {
            IngotResolutionDiagnostic::Files(diagnostic) => {
                self.report_warn(IngotInitDiagnostics::FileError { diagnostic });
            }
        }
    }

    fn on_resolution_event(&mut self, event: IngotResolutionEvent) {
        match event {
            IngotResolutionEvent::FilesResolved { files } => {
                self.record_files_owned(files);
            }
            IngotResolutionEvent::RemoteCheckoutStart { description } => {
                if self.trace_enabled {
                    tracing::info!(target: "resolver", "Checking out {description}");
                }
                if self.stdout_enabled {
                    eprintln!("Checking out {description}");
                }
            }
            IngotResolutionEvent::RemoteCheckoutComplete {
                ingot_url,
                reused_checkout,
                description,
                ..
            } => {
                if reused_checkout {
                    if self.reported_checkouts.contains(&description) {
                        return;
                    }
                    if self.trace_enabled {
                        tracing::debug!(target: "resolver", "Using cached checkout {}", ingot_url);
                    }
                    if self.stdout_enabled && self.verbose_enabled {
                        eprintln!("Using cached checkout {}", ingot_url);
                    }
                    return;
                }
                if self.trace_enabled {
                    tracing::info!(target: "resolver", "Checked out {}", ingot_url);
                }
                if self.stdout_enabled {
                    eprintln!("Checked out {}", ingot_url);
                }
            }
        }
    }

    fn on_resolution_error(
        &mut self,
        description: &IngotDescriptor,
        error: resolver::ingot::IngotResolutionError,
    ) {
        if matches!(
            description,
            IngotDescriptor::Remote(_) | IngotDescriptor::RemoteByName { .. }
        ) {
            if self.trace_enabled {
                tracing::error!(
                    target: "resolver",
                    "Failed to check out {description}: {error}"
                );
            }
            if self.stdout_enabled {
                eprintln!("Error: Failed to check out {description}: {error}");
            }
        }

        if let resolver::ingot::IngotResolutionError::Selection(selection) = &error
            && let Some(contexts) = self.dependency_contexts.get(description).cloned()
        {
            use resolver::ingot::IngotSelectionError;

            for context in contexts {
                match selection.as_ref() {
                    IngotSelectionError::NoResolvedIngotByMetadata { name, version } => {
                        self.report_error(IngotInitDiagnostics::IngotByNameResolutionFailed {
                            ingot_url: context.from_ingot_url.clone(),
                            dependency: context.dependency.clone(),
                            name: name.clone(),
                            version: version.clone(),
                        });
                    }
                    IngotSelectionError::WorkspacePathRequiresSelection { workspace_url } => {
                        self.report_error(IngotInitDiagnostics::WorkspacePathRequiresSelection {
                            ingot_url: context.from_ingot_url.clone(),
                            dependency: context.dependency.clone(),
                            workspace_url: workspace_url.clone(),
                        });
                    }
                    IngotSelectionError::WorkspaceMemberNotFound {
                        workspace_url,
                        name,
                    }
                    | IngotSelectionError::WorkspaceMemberDuplicate {
                        workspace_url,
                        name,
                    } => {
                        self.report_error(IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                            ingot_url: context.from_ingot_url.clone(),
                            dependency: context.dependency.clone(),
                            error: format!(
                                "No workspace member named \"{name}\" found in {workspace_url}"
                            ),
                        });
                    }
                    IngotSelectionError::DependencyMetadataMismatch {
                        dependency_url,
                        expected_name,
                        found_name,
                        found_version,
                    } => {
                        self.report_error(IngotInitDiagnostics::DependencyMetadataMismatch {
                            ingot_url: context.from_ingot_url.clone(),
                            dependency: context.dependency.clone(),
                            dependency_url: dependency_url.clone(),
                            expected_name: expected_name.clone(),
                            expected_version: None,
                            found_name: found_name.clone(),
                            found_version: found_version.clone(),
                        });
                    }
                    IngotSelectionError::MissingConfig { config_url } => {
                        self.report_error(IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                            ingot_url: context.from_ingot_url.clone(),
                            dependency: context.dependency.clone(),
                            error: format!("Missing config at {config_url}"),
                        });
                    }
                    IngotSelectionError::ConfigParseError { config_url, error } => {
                        self.report_error(IngotInitDiagnostics::WorkspaceMemberResolutionFailed {
                            ingot_url: context.from_ingot_url.clone(),
                            dependency: context.dependency.clone(),
                            error: format!("Failed to parse {config_url}: {error}"),
                        });
                    }
                }
            }
            return;
        }

        match description {
            IngotDescriptor::Local(target) => {
                self.report_error(IngotInitDiagnostics::UnresolvableIngotDependency {
                    target: target.clone(),
                    error,
                })
            }
            IngotDescriptor::LocalByName { base, .. } => {
                self.report_error(IngotInitDiagnostics::UnresolvableIngotDependency {
                    target: base.clone(),
                    error,
                })
            }
            IngotDescriptor::ByNameVersion { .. } => {
                if self.trace_enabled {
                    tracing::error!(
                        target: "resolver",
                        "Unhandled ByNameVersion resolution error for {description}: {error}"
                    );
                }
                if self.stdout_enabled {
                    eprintln!(
                        "Error: Unhandled name-only dependency resolution error for {description}: {error}"
                    );
                }
            }
            IngotDescriptor::Remote(target) => {
                self.report_error(IngotInitDiagnostics::UnresolvableRemoteDependency {
                    target: target.clone(),
                    error,
                })
            }
            IngotDescriptor::RemoteByName { base, .. } => {
                self.report_error(IngotInitDiagnostics::UnresolvableRemoteDependency {
                    target: base.clone(),
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
        let IngotResource {
            ingot_url,
            origin,
            workspace_root,
            config_probe,
            files_error,
        } = resource;

        if let Some(workspace_root) = &workspace_root {
            self.db.dependency_graph().register_workspace_member_root(
                self.db,
                workspace_root,
                &ingot_url,
            );
        }

        self.register_remote_mapping(&ingot_url, &origin);

        if let Some(error) = files_error {
            match &origin {
                IngotOrigin::Local => {
                    self.report_error(IngotInitDiagnostics::UnresolvableIngotDependency {
                        target: ingot_url.clone(),
                        error: resolver::ingot::IngotResolutionError::Files(error),
                    });
                }
                IngotOrigin::Remote { .. } => {
                    self.report_error(IngotInitDiagnostics::RemoteFileError {
                        ingot_url: ingot_url.clone(),
                        error: error.to_string(),
                    });
                }
            }
            self.ingot_urls
                .insert(descriptor.clone(), ingot_url.clone());
            return Ok(GraphNodeOutcome {
                canonical_description: descriptor.clone(),
                aliases: Vec::new(),
                forward_nodes: Vec::new(),
            });
        }

        if !config_probe.has_config() {
            if matches!(&origin, IngotOrigin::Remote { .. }) {
                self.report_error(IngotInitDiagnostics::RemoteFileError {
                    ingot_url: ingot_url.clone(),
                    error: "Remote ingot is missing fe.toml".into(),
                });
            }
            self.ingot_urls
                .insert(descriptor.clone(), ingot_url.clone());
            return Ok(GraphNodeOutcome {
                canonical_description: descriptor.clone(),
                aliases: Vec::new(),
                forward_nodes: Vec::new(),
            });
        }

        let config = match self.config_at_url(&ingot_url) {
            Ok(config) => config,
            Err(error) => {
                match &origin {
                    IngotOrigin::Local => {
                        if config_probe.kind_hint() == Some(ConfigKind::Workspace) {
                            self.report_error(IngotInitDiagnostics::WorkspaceConfigParseError {
                                workspace_url: ingot_url.clone(),
                                error,
                            });
                        } else {
                            self.report_error(IngotInitDiagnostics::ConfigParseError {
                                ingot_url: ingot_url.clone(),
                                error,
                            });
                        }
                    }
                    IngotOrigin::Remote { .. } => {
                        self.report_error(IngotInitDiagnostics::RemoteConfigParseError {
                            ingot_url: ingot_url.clone(),
                            error,
                        });
                    }
                }
                self.ingot_urls
                    .insert(descriptor.clone(), ingot_url.clone());
                return Ok(GraphNodeOutcome {
                    canonical_description: descriptor.clone(),
                    aliases: Vec::new(),
                    forward_nodes: Vec::new(),
                });
            }
        };

        if let Config::Workspace(workspace_config) = config {
            if self.dependency_contexts.contains_key(descriptor) {
                return Err(resolver::ingot::IngotResolutionError::Selection(Box::new(
                    resolver::ingot::IngotSelectionError::WorkspacePathRequiresSelection {
                        workspace_url: ingot_url.clone(),
                    },
                )));
            }

            self.ingot_urls
                .insert(descriptor.clone(), ingot_url.clone());
            return Ok(GraphNodeOutcome {
                canonical_description: descriptor.clone(),
                aliases: Vec::new(),
                forward_nodes: self.handle_workspace_config(
                    &IngotResource {
                        ingot_url,
                        origin,
                        workspace_root,
                        config_probe,
                        files_error: None,
                    },
                    *workspace_config,
                ),
            });
        }

        let Config::Ingot(mut config) = config else {
            self.ingot_urls
                .insert(descriptor.clone(), ingot_url.clone());
            return Ok(GraphNodeOutcome {
                canonical_description: descriptor.clone(),
                aliases: Vec::new(),
                forward_nodes: Vec::new(),
            });
        };

        let mut diagnostics = config.diagnostics.clone();

        if config.metadata.version.is_none()
            && let Some(version) = workspace_version_for_member(self.db, &ingot_url)
        {
            config.metadata.version = Some(version);
            diagnostics.retain(|diag| !matches!(diag, ConfigDiagnostic::MissingVersion));
        }

        if !diagnostics.is_empty() {
            match &origin {
                IngotOrigin::Local => self.report_warn(IngotInitDiagnostics::ConfigDiagnostics {
                    ingot_url: ingot_url.clone(),
                    diagnostics,
                }),
                IngotOrigin::Remote { .. } => {
                    self.report_warn(IngotInitDiagnostics::RemoteConfigDiagnostics {
                        ingot_url: ingot_url.clone(),
                        diagnostics,
                    })
                }
            };
        }

        self.db.dependency_graph().ensure_node(self.db, &ingot_url);

        if let Some((expected_name, expected_version)) = self
            .db
            .dependency_graph()
            .expected_member_metadata_for(self.db, &ingot_url)
            && (config.metadata.name.as_ref() != Some(&expected_name)
                || config.metadata.version.as_ref() != Some(&expected_version))
        {
            self.report_error(IngotInitDiagnostics::WorkspaceMemberMetadataMismatch {
                ingot_url: ingot_url.clone(),
                expected_name,
                expected_version,
                found_name: config.metadata.name.clone(),
                found_version: config.metadata.version.clone(),
            });
            self.ingot_urls
                .insert(descriptor.clone(), ingot_url.clone());
            return Ok(GraphNodeOutcome {
                canonical_description: descriptor.clone(),
                aliases: Vec::new(),
                forward_nodes: Vec::new(),
            });
        }

        let mut aliases = Vec::new();
        if let (Some(name), Some(version)) = (
            config.metadata.name.clone(),
            config.metadata.version.clone(),
        ) {
            self.db.dependency_graph().register_ingot_metadata(
                self.db,
                &ingot_url,
                name.clone(),
                version.clone(),
            );
            aliases.push(IngotDescriptor::ByNameVersion { name, version });
        }

        let workspace_member_alias = workspace_root.as_ref().and_then(|workspace_root| {
            config.metadata.name.clone().map(|name| match &origin {
                IngotOrigin::Local => IngotDescriptor::LocalByName {
                    base: workspace_root.clone(),
                    name,
                },
                IngotOrigin::Remote {
                    description,
                    checkout_path,
                    ..
                } => {
                    let mut base =
                        GitDescription::new(description.source.clone(), description.rev.clone());
                    if let Ok(relative) =
                        relative_path_within_checkout(checkout_path.as_path(), workspace_root)
                        && let Some(path) = relative
                    {
                        base = base.with_path(path);
                    }
                    IngotDescriptor::RemoteByName { base, name }
                }
            })
        });

        let canonical_description = workspace_member_alias
            .clone()
            .unwrap_or_else(|| descriptor.clone());

        let concrete_description = match &origin {
            IngotOrigin::Local => IngotDescriptor::Local(ingot_url.clone()),
            IngotOrigin::Remote { description, .. } => IngotDescriptor::Remote(description.clone()),
        };
        if concrete_description != canonical_description {
            aliases.push(concrete_description);
        }

        let mut dependencies = Vec::new();
        for dependency in config.dependencies(&ingot_url) {
            if let Some(converted) =
                self.convert_dependency(&ingot_url, &origin, workspace_root.as_ref(), dependency)
            {
                let priority = match &converted.0 {
                    IngotDescriptor::Local(_) | IngotDescriptor::LocalByName { .. } => {
                        IngotPriority::local()
                    }
                    IngotDescriptor::Remote(_)
                    | IngotDescriptor::RemoteByName { .. }
                    | IngotDescriptor::ByNameVersion { .. } => IngotPriority::remote(),
                };
                dependencies.push(UnresolvedNode {
                    priority,
                    description: converted.0,
                    edge: converted.1,
                });
            }
        }

        self.ingot_urls
            .insert(canonical_description.clone(), ingot_url.clone());

        Ok(GraphNodeOutcome {
            canonical_description,
            aliases,
            forward_nodes: dependencies,
        })
    }
}

impl<'a> ResolutionHandler<resolver::files::FilesResolver> for IngotHandler<'a> {
    type Item = resolver::files::FilesResource;

    fn on_resolution_diagnostic(&mut self, diagnostic: resolver::files::FilesResolutionDiagnostic) {
        self.report_warn(IngotInitDiagnostics::FileError { diagnostic });
    }

    fn handle_resolution(
        &mut self,
        _description: &Url,
        resource: resolver::files::FilesResource,
    ) -> Self::Item {
        self.record_files(&resource.files);
        resource
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
                let from_url = from_url.clone();
                let to_url = to_url.clone();
                let (alias, arguments) = edge.weight();
                if registered_edges.insert((
                    from_url.clone(),
                    to_url.clone(),
                    alias.clone(),
                    arguments.clone(),
                )) {
                    if let Some(expected_version) = arguments.version.clone()
                        && let Some((found_name, found_version)) =
                            self.effective_metadata_for_ingot(&to_url)
                        && found_version.as_ref() != Some(&expected_version)
                    {
                        let expected_name = arguments
                            .name
                            .clone()
                            .or_else(|| found_name.clone())
                            .unwrap_or_else(|| alias.clone());
                        self.report_error(IngotInitDiagnostics::DependencyMetadataMismatch {
                            ingot_url: from_url.clone(),
                            dependency: alias.clone(),
                            dependency_url: to_url.clone(),
                            expected_name,
                            expected_version: Some(expected_version),
                            found_name,
                            found_version,
                        });
                    }
                    self.db.dependency_graph().add_dependency(
                        self.db,
                        &from_url,
                        &to_url,
                        alias.clone(),
                        arguments.clone(),
                    );
                }
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
