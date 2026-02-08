use std::fs;

use camino::{Utf8Path, Utf8PathBuf};
use smol_str::SmolStr;
use toml::Value;
use url::Url;

use common::config::{Config, WorkspaceMemberSelection};
use common::ingot::Version;

use crate::{
    ResolutionHandler, Resolver,
    files::{File, FilesResolutionDiagnostic, FilesResolutionError, FilesResolver, FilesResource},
    git::{GitDescription, GitResolutionError, GitResolver},
    graph::GraphResolverImpl,
    workspace::{ExpandedWorkspaceMember, expand_workspace_members},
};

/// Files resolver used for basic ingot discovery. Requires only `fe.toml`.
pub fn minimal_files_resolver() -> FilesResolver {
    FilesResolver::new().with_required_file("fe.toml")
}

/// Files resolver used for project ingots. Requires a `src/lib.fe` entrypoint.
pub fn project_files_resolver() -> FilesResolver {
    FilesResolver::new()
        .with_required_directory("src")
        .with_required_file("src/lib.fe")
        .with_pattern("src/**/*.fe")
}

/// Files resolver used for workspace roots. Gathers member configs by pattern.
pub fn workspace_files_resolver() -> FilesResolver {
    FilesResolver::with_patterns(&["**/fe.toml"])
}

/// Convenience alias for the standard local ingot graph resolver.
pub type LocalGraphResolver<H, E, P> = GraphResolverImpl<FilesResolver, H, E, P>;

/// Convenience alias for graph resolvers that walk remote git dependencies.
pub type RemoteGraphResolver<H, E, P> = GraphResolverImpl<GitResolver, H, E, P>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IngotDescriptor {
    Local(Url),
    Remote(GitDescription),
    /// Reference an ingot purely by its metadata.
    ///
    /// This is intended to be canonicalized to a concrete [`Local`] or [`Remote`]
    /// descriptor during graph resolution (e.g. when the ingot at a path has been
    /// resolved and its metadata is known).
    ByNameVersion {
        name: SmolStr,
        version: Version,
    },
    /// Resolve an ingot by name (and optional version) within a local directory.
    ///
    /// The target directory may be either an ingot itself, or a workspace root.
    LocalByName {
        base: Url,
        name: SmolStr,
    },
    /// Resolve an ingot by name (and optional version) within a remote git checkout.
    ///
    /// The `base` path may be either an ingot itself, or a workspace root.
    RemoteByName {
        base: GitDescription,
        name: SmolStr,
    },
}

impl std::fmt::Display for IngotDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IngotDescriptor::Local(url) => write!(f, "{url}"),
            IngotDescriptor::Remote(description) => write!(f, "{description}"),
            IngotDescriptor::ByNameVersion { name, version } => write!(f, "{name}@{version}"),
            IngotDescriptor::LocalByName { base, name } => write!(f, "{base} (name={name})"),
            IngotDescriptor::RemoteByName { base, name } => write!(f, "{base} (name={name})"),
        }
    }
}

#[derive(Debug)]
pub enum IngotOrigin {
    Local,
    Remote {
        description: GitDescription,
        checkout_path: Utf8PathBuf,
        reused_checkout: bool,
    },
}

#[derive(Debug)]
pub struct IngotResource {
    pub ingot_url: Url,
    pub origin: IngotOrigin,
    pub workspace_root: Option<Url>,
    pub config_probe: FeTomlProbe,
    pub files_error: Option<FilesResolutionError>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ConfigKind {
    Ingot,
    Workspace,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FeTomlProbe {
    Missing,
    Present { kind_hint: Option<ConfigKind> },
}

impl FeTomlProbe {
    pub fn has_config(self) -> bool {
        matches!(self, Self::Present { .. })
    }

    pub fn kind_hint(self) -> Option<ConfigKind> {
        match self {
            Self::Missing => None,
            Self::Present { kind_hint } => kind_hint,
        }
    }
}

pub fn infer_config_kind(content: &str) -> Option<ConfigKind> {
    let parsed: Value = content.parse().ok()?;
    let table = parsed.as_table()?;
    if table.contains_key("workspace") || common::config::looks_like_workspace(&parsed) {
        return Some(ConfigKind::Workspace);
    }
    if table.contains_key("ingot") {
        return Some(ConfigKind::Ingot);
    }
    Some(ConfigKind::Ingot)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Default)]
pub enum IngotPriority {
    #[default]
    Local,
    Remote,
}

impl IngotPriority {
    pub fn local() -> Self {
        Self::Local
    }

    pub fn remote() -> Self {
        Self::Remote
    }
}

impl Ord for IngotPriority {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;
        match (self, other) {
            (Self::Local, Self::Local) => Equal,
            (Self::Remote, Self::Remote) => Equal,
            (Self::Local, Self::Remote) => Greater,
            (Self::Remote, Self::Local) => Less,
        }
    }
}

impl PartialOrd for IngotPriority {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub enum IngotResolutionError {
    Files(FilesResolutionError),
    Git(GitResolutionError),
    Selection(Box<IngotSelectionError>),
}

#[derive(Debug)]
pub enum IngotResolutionDiagnostic {
    Files(FilesResolutionDiagnostic),
}

#[derive(Debug, Clone)]
pub enum IngotSelectionError {
    NoResolvedIngotByMetadata {
        name: SmolStr,
        version: Version,
    },
    WorkspacePathRequiresSelection {
        workspace_url: Url,
    },
    MissingConfig {
        config_url: Url,
    },
    ConfigParseError {
        config_url: Url,
        error: String,
    },
    WorkspaceMemberNotFound {
        workspace_url: Url,
        name: SmolStr,
    },
    WorkspaceMemberDuplicate {
        workspace_url: Url,
        name: SmolStr,
    },
    DependencyMetadataMismatch {
        dependency_url: Url,
        expected_name: SmolStr,
        found_name: Option<SmolStr>,
        found_version: Option<Version>,
    },
}

type SelectionResult<T> = Result<T, Box<IngotSelectionError>>;

pub trait RemoteProgress {
    fn start(&mut self, description: &GitDescription);
    fn success(&mut self, description: &GitDescription, ingot_url: &Url);
    fn error(&mut self, description: &GitDescription, error: &IngotResolutionError);
}

#[derive(Default)]
struct NoopProgress;

impl RemoteProgress for NoopProgress {
    fn start(&mut self, _description: &GitDescription) {}

    fn success(&mut self, _description: &GitDescription, _ingot_url: &Url) {}

    fn error(&mut self, _description: &GitDescription, _error: &IngotResolutionError) {}
}

impl std::fmt::Display for IngotResolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IngotResolutionError::Files(err) => err.fmt(f),
            IngotResolutionError::Git(err) => err.fmt(f),
            IngotResolutionError::Selection(err) => err.fmt(f),
        }
    }
}

impl std::fmt::Display for IngotSelectionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IngotSelectionError::NoResolvedIngotByMetadata { name, version } => write!(
                f,
                "No ingot with metadata {name}@{version} has been resolved"
            ),
            IngotSelectionError::WorkspacePathRequiresSelection { workspace_url } => write!(
                f,
                "Dependency points to a workspace at {workspace_url}; provide an ingot path or a name"
            ),
            IngotSelectionError::MissingConfig { config_url } => {
                write!(f, "Missing config at {config_url}")
            }
            IngotSelectionError::ConfigParseError { config_url, error } => {
                write!(f, "Failed to parse {config_url}: {error}")
            }
            IngotSelectionError::WorkspaceMemberNotFound {
                workspace_url,
                name,
            } => write!(
                f,
                "No workspace member named \"{name}\" found in {workspace_url}"
            ),
            IngotSelectionError::WorkspaceMemberDuplicate {
                workspace_url,
                name,
            } => write!(
                f,
                "Multiple workspace members named \"{name}\" found in {workspace_url}"
            ),
            IngotSelectionError::DependencyMetadataMismatch {
                dependency_url,
                expected_name,
                found_name: _,
                found_version: _,
            } => write!(
                f,
                "Metadata mismatch at {dependency_url}: expected {expected_name}"
            ),
        }
    }
}

impl std::error::Error for IngotResolutionError {}

#[derive(Debug)]
pub enum IngotResolutionEvent {
    FilesResolved {
        files: Vec<File>,
    },
    RemoteCheckoutStart {
        description: GitDescription,
    },
    RemoteCheckoutComplete {
        description: GitDescription,
        ingot_url: Url,
        reused_checkout: bool,
    },
}

type RemoteCheckoutEvent = (GitDescription, Url, bool, bool);
type ResolvedIngotLocation = (Url, IngotOrigin, Option<RemoteCheckoutEvent>);

pub struct IngotResolverImpl {
    git: GitResolver,
    progress: Box<dyn RemoteProgress>,
}

impl IngotResolverImpl {
    pub fn new(git: GitResolver) -> Self {
        Self {
            git,
            progress: Box::new(NoopProgress),
        }
    }

    pub fn with_progress(mut self, progress: Box<dyn RemoteProgress>) -> Self {
        self.progress = progress;
        self
    }

    fn resolve_config(url: &Url) -> SelectionResult<Config> {
        let config_url = url.join("fe.toml").map_err(|_| {
            Box::new(IngotSelectionError::MissingConfig {
                config_url: url.clone(),
            })
        })?;
        let path = config_url.to_file_path().map_err(|_| {
            Box::new(IngotSelectionError::MissingConfig {
                config_url: config_url.clone(),
            })
        })?;
        if !path.is_file() {
            return Err(Box::new(IngotSelectionError::MissingConfig {
                config_url: config_url.clone(),
            }));
        }
        let content = fs::read_to_string(&path).map_err(|err| {
            Box::new(IngotSelectionError::ConfigParseError {
                config_url: config_url.clone(),
                error: err.to_string(),
            })
        })?;
        Config::parse(&content).map_err(|err| {
            Box::new(IngotSelectionError::ConfigParseError {
                config_url,
                error: err.to_string(),
            })
        })
    }

    fn resolve_member_by_name(
        workspace_url: &Url,
        workspace_config: &common::config::WorkspaceConfig,
        name: &SmolStr,
    ) -> SelectionResult<ExpandedWorkspaceMember> {
        let members = expand_workspace_members(
            &workspace_config.workspace,
            workspace_url,
            WorkspaceMemberSelection::All,
        )
        .map_err(|error| {
            Box::new(IngotSelectionError::ConfigParseError {
                config_url: workspace_url.clone(),
                error,
            })
        })?;

        let mut matches = Vec::new();
        for member in members {
            let member_name = member.name.clone().or_else(|| {
                let Config::Ingot(config) = Self::resolve_config(&member.url).ok()? else {
                    return None;
                };
                config.metadata.name.clone()
            });
            if member_name.as_ref() == Some(name) {
                matches.push(member);
            }
        }

        if matches.is_empty() {
            return Err(Box::new(IngotSelectionError::WorkspaceMemberNotFound {
                workspace_url: workspace_url.clone(),
                name: name.clone(),
            }));
        }
        if matches.len() > 1 {
            return Err(Box::new(IngotSelectionError::WorkspaceMemberDuplicate {
                workspace_url: workspace_url.clone(),
                name: name.clone(),
            }));
        }

        Ok(matches.remove(0))
    }

    fn ensure_local_dir(url: &Url) -> Result<(), IngotResolutionError> {
        let path = url.to_file_path().map_err(|_| {
            IngotResolutionError::Files(FilesResolutionError::DirectoryDoesNotExist(url.clone()))
        })?;
        if !path.is_dir() {
            return Err(IngotResolutionError::Files(
                FilesResolutionError::DirectoryDoesNotExist(url.clone()),
            ));
        }
        Ok(())
    }

    fn remote_dir_url(
        description: &GitDescription,
        checkout_path: &Utf8Path,
    ) -> Result<Url, IngotResolutionError> {
        let ingot_path = description
            .path
            .as_ref()
            .map(|relative| checkout_path.join(relative))
            .unwrap_or_else(|| checkout_path.to_owned());
        if !ingot_path.exists() || !ingot_path.is_dir() {
            let url = Url::from_directory_path(ingot_path.as_std_path())
                .or_else(|_| Url::from_file_path(ingot_path.as_std_path()))
                .unwrap_or_else(|_| description.source.clone());
            return Err(IngotResolutionError::Files(
                FilesResolutionError::DirectoryDoesNotExist(url),
            ));
        }
        Ok(Url::from_directory_path(ingot_path.as_std_path())
            .expect("Failed to convert ingot path to URL"))
    }

    fn ensure_remote_checkout<H>(
        &mut self,
        handler: &mut H,
        description: &GitDescription,
    ) -> Result<(Utf8PathBuf, bool, bool), IngotResolutionError>
    where
        H: ResolutionHandler<Self>,
    {
        let checkout_path = self.git.checkout_path(description);
        if self.git.has_valid_cached_checkout(description) {
            if let Err(error) = self.git.enforce_readonly(checkout_path.as_path()) {
                tracing::warn!(
                    target: "resolver",
                    "Failed to mark git checkout read-only at {}: {}",
                    checkout_path,
                    error
                );
            }
            return Ok((checkout_path, true, false));
        }

        handler.on_resolution_event(IngotResolutionEvent::RemoteCheckoutStart {
            description: description.clone(),
        });
        self.progress.start(description);
        let git_resource = match self.git.ensure_checkout_resource(description) {
            Ok(resource) => resource,
            Err(err) => {
                let wrapped = IngotResolutionError::Git(err);
                self.progress.error(description, &wrapped);
                return Err(wrapped);
            }
        };
        Ok((
            git_resource.checkout_path,
            git_resource.reused_checkout,
            true,
        ))
    }

    fn resolve_ingot_location<H>(
        &mut self,
        handler: &mut H,
        description: &IngotDescriptor,
    ) -> Result<ResolvedIngotLocation, IngotResolutionError>
    where
        H: ResolutionHandler<Self>,
    {
        match description {
            IngotDescriptor::Local(url) => {
                Self::ensure_local_dir(url)?;
                Ok((url.clone(), IngotOrigin::Local, None))
            }
            IngotDescriptor::Remote(desc) => {
                let (checkout_path, reused_checkout, hit_network) =
                    self.ensure_remote_checkout(handler, desc)?;
                let resolved_url = Self::remote_dir_url(desc, checkout_path.as_path())?;
                Ok((
                    resolved_url.clone(),
                    IngotOrigin::Remote {
                        description: desc.clone(),
                        checkout_path,
                        reused_checkout,
                    },
                    Some((desc.clone(), resolved_url, reused_checkout, hit_network)),
                ))
            }
            IngotDescriptor::ByNameVersion { name, version } => {
                Err(IngotResolutionError::Selection(Box::new(
                    IngotSelectionError::NoResolvedIngotByMetadata {
                        name: name.clone(),
                        version: version.clone(),
                    },
                )))
            }
            IngotDescriptor::LocalByName { base, name } => {
                Self::ensure_local_dir(base)?;
                match Self::resolve_config(base).map_err(IngotResolutionError::Selection)? {
                    Config::Ingot(ingot_config) => {
                        if ingot_config.metadata.name.as_ref() != Some(name) {
                            return Err(IngotResolutionError::Selection(Box::new(
                                IngotSelectionError::DependencyMetadataMismatch {
                                    dependency_url: base.clone(),
                                    expected_name: name.clone(),
                                    found_name: ingot_config.metadata.name.clone(),
                                    found_version: ingot_config.metadata.version.clone(),
                                },
                            )));
                        }
                        Ok((base.clone(), IngotOrigin::Local, None))
                    }
                    Config::Workspace(workspace_config) => {
                        let member = Self::resolve_member_by_name(base, &workspace_config, name)
                            .map_err(IngotResolutionError::Selection)?;
                        Ok((member.url, IngotOrigin::Local, None))
                    }
                }
            }
            IngotDescriptor::RemoteByName { base, name } => {
                let (checkout_path, reused_checkout, hit_network) =
                    self.ensure_remote_checkout(handler, base)?;
                let base_url = Self::remote_dir_url(base, checkout_path.as_path())?;

                let (ingot_url, origin) = match Self::resolve_config(&base_url)
                    .map_err(IngotResolutionError::Selection)?
                {
                    Config::Ingot(ingot_config) => {
                        if ingot_config.metadata.name.as_ref() != Some(name) {
                            return Err(IngotResolutionError::Selection(Box::new(
                                IngotSelectionError::DependencyMetadataMismatch {
                                    dependency_url: base_url.clone(),
                                    expected_name: name.clone(),
                                    found_name: ingot_config.metadata.name.clone(),
                                    found_version: ingot_config.metadata.version.clone(),
                                },
                            )));
                        }
                        (
                            base_url.clone(),
                            IngotOrigin::Remote {
                                description: base.clone(),
                                checkout_path: checkout_path.clone(),
                                reused_checkout,
                            },
                        )
                    }
                    Config::Workspace(workspace_config) => {
                        let member =
                            Self::resolve_member_by_name(&base_url, &workspace_config, name)
                                .map_err(IngotResolutionError::Selection)?;
                        let mut member_path = member.path.clone();
                        if let Some(root_path) = &base.path {
                            member_path = root_path.join(member_path.as_str());
                        }
                        let member_description =
                            GitDescription::new(base.source.clone(), base.rev.clone())
                                .with_path(member_path);
                        (
                            member.url.clone(),
                            IngotOrigin::Remote {
                                description: member_description,
                                checkout_path: checkout_path.clone(),
                                reused_checkout,
                            },
                        )
                    }
                };

                Ok((
                    ingot_url,
                    origin,
                    Some((base.clone(), base_url, reused_checkout, hit_network)),
                ))
            }
        }
    }

    fn find_workspace_root(ingot_url: &Url, stop_at: Option<&Utf8Path>) -> Option<Url> {
        let path_buf = ingot_url.to_file_path().ok()?;
        let mut ingot_dir = Utf8PathBuf::from_path_buf(path_buf).ok()?;
        if ingot_dir.is_file() {
            ingot_dir = ingot_dir.parent()?.to_owned();
        }
        let ingot_dir_url = Url::from_directory_path(ingot_dir.as_std_path()).ok()?;

        let mut current = ingot_dir.clone();
        loop {
            let candidate = current.join("fe.toml");
            if candidate.is_file()
                && let Ok(content) = fs::read_to_string(candidate.as_std_path())
                && let Ok(Config::Workspace(config)) = Config::parse(&content)
                && let Ok(url) = Url::from_directory_path(current.as_std_path())
            {
                if current == ingot_dir {
                    return Some(url);
                }

                if let Ok(members) =
                    expand_workspace_members(&config.workspace, &url, WorkspaceMemberSelection::All)
                    && members.iter().any(|member| member.url == ingot_dir_url)
                {
                    return Some(url);
                }
            }

            if stop_at.is_some_and(|stop_at| stop_at == current) {
                break;
            }

            let Some(parent) = current.parent() else {
                break;
            };
            current = parent.to_owned();
        }

        None
    }

    fn collect_files(
        handler: &mut impl ResolutionHandler<Self>,
        url: &Url,
        mut resolver: FilesResolver,
    ) -> Result<FeTomlProbe, FilesResolutionError> {
        struct Forwarder<'a, H> {
            handler: &'a mut H,
        }

        impl<'a, H> ResolutionHandler<FilesResolver> for Forwarder<'a, H>
        where
            H: ResolutionHandler<IngotResolverImpl>,
        {
            type Item = FeTomlProbe;

            fn on_resolution_diagnostic(&mut self, diagnostic: FilesResolutionDiagnostic) {
                <H as ResolutionHandler<IngotResolverImpl>>::on_resolution_diagnostic(
                    self.handler,
                    IngotResolutionDiagnostic::Files(diagnostic),
                );
            }

            fn handle_resolution(
                &mut self,
                _description: &Url,
                resource: FilesResource,
            ) -> Self::Item {
                let probe = resource
                    .files
                    .iter()
                    .find(|file| file.path.as_str().ends_with("fe.toml"))
                    .map(|config_file| FeTomlProbe::Present {
                        kind_hint: infer_config_kind(&config_file.content),
                    })
                    .unwrap_or(FeTomlProbe::Missing);

                <H as ResolutionHandler<IngotResolverImpl>>::on_resolution_event(
                    self.handler,
                    IngotResolutionEvent::FilesResolved {
                        files: resource.files,
                    },
                );

                probe
            }
        }

        let mut forwarder = Forwarder { handler };
        resolver.resolve(&mut forwarder, url)
    }
}

impl Resolver for IngotResolverImpl {
    type Description = IngotDescriptor;
    type Resource = IngotResource;
    type Error = IngotResolutionError;
    type Diagnostic = IngotResolutionDiagnostic;
    type Event = IngotResolutionEvent;

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        description: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: ResolutionHandler<Self>,
    {
        let (ingot_url, origin, remote_event) =
            self.resolve_ingot_location(handler, description)?;

        let stop_at = match &origin {
            IngotOrigin::Remote { checkout_path, .. } => Some(checkout_path.as_path()),
            IngotOrigin::Local => None,
        };
        let workspace_root = Self::find_workspace_root(&ingot_url, stop_at);

        if let Some(workspace_root) = &workspace_root
            && workspace_root != &ingot_url
        {
            let _ = Self::collect_files(handler, workspace_root, minimal_files_resolver());
        }

        let mut config_probe = FeTomlProbe::Missing;
        let mut files_error = None;

        match Self::collect_files(handler, &ingot_url, minimal_files_resolver()) {
            Ok(probe) => config_probe = probe,
            Err(error) => files_error = Some(error),
        }

        if files_error.is_none() && config_probe.has_config() {
            match config_probe.kind_hint() {
                Some(ConfigKind::Workspace) => {
                    let _ = Self::collect_files(handler, &ingot_url, workspace_files_resolver());
                }
                Some(ConfigKind::Ingot) => {
                    let _ = Self::collect_files(handler, &ingot_url, project_files_resolver());
                }
                None => {}
            }
        }

        let resource = IngotResource {
            ingot_url: ingot_url.clone(),
            origin,
            workspace_root,
            config_probe,
            files_error,
        };

        let result = handler.handle_resolution(description, resource);

        if let Some((checkout_description, checkout_url, reused_checkout, hit_network)) =
            remote_event
        {
            handler.on_resolution_event(IngotResolutionEvent::RemoteCheckoutComplete {
                description: checkout_description.clone(),
                ingot_url: checkout_url.clone(),
                reused_checkout,
            });
            if hit_network {
                self.progress.success(&checkout_description, &checkout_url);
            }
        }

        Ok(result)
    }
}
