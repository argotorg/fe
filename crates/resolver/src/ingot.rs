use camino::{Utf8Path, Utf8PathBuf};
use url::Url;

use crate::{
    ResolutionHandler, Resolver,
    files::{FilesResolutionDiagnostic, FilesResolutionError, FilesResolver, FilesResource},
    git::{GitDescription, GitResolutionDiagnostic, GitResolutionError, GitResolver, GitResource},
    graph::GraphResolverImpl,
};

/// Files resolver used for basic ingot discovery. Requires only `fe.toml`.
pub fn minimal_files_resolver() -> FilesResolver {
    FilesResolver::new().with_required_file("fe.toml")
}

/// Files resolver used for project ingots. Requires a `src/lib.fe` entrypoint.
pub fn project_files_resolver() -> FilesResolver {
    minimal_files_resolver()
        .with_required_directory("src")
        .with_required_file("src/lib.fe")
        .with_pattern("src/**/*.fe")
}

/// Convenience alias for the standard local ingot graph resolver.
pub type LocalGraphResolver<H, E, P> = GraphResolverImpl<FilesResolver, H, E, P>;

/// Convenience alias for graph resolvers that walk remote git dependencies.
pub type RemoteGraphResolver<H, E, P> = GraphResolverImpl<GitResolver, H, E, P>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IngotDescriptor {
    Local(Url),
    Remote(GitDescription),
}

impl std::fmt::Display for IngotDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IngotDescriptor::Local(url) => write!(f, "{url}"),
            IngotDescriptor::Remote(description) => write!(f, "{description}"),
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
    pub files: FilesResource,
    pub origin: IngotOrigin,
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
}

#[derive(Debug)]
pub enum IngotResolutionDiagnostic {
    Files(FilesResolutionDiagnostic),
    Git(GitResolutionDiagnostic),
}

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
        }
    }
}

impl std::error::Error for IngotResolutionError {}

impl std::fmt::Display for IngotResolutionDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IngotResolutionDiagnostic::Files(diag) => diag.fmt(f),
            IngotResolutionDiagnostic::Git(diag) => diag.fmt(f),
        }
    }
}

pub struct IngotResolver {
    files: FilesResolver,
    git: GitResolver,
    progress: Box<dyn RemoteProgress>,
}

impl IngotResolver {
    pub fn new(files: FilesResolver, git: GitResolver) -> Self {
        Self {
            files,
            git,
            progress: Box::new(NoopProgress),
        }
    }

    pub fn with_progress(mut self, progress: Box<dyn RemoteProgress>) -> Self {
        self.progress = progress;
        self
    }

    fn load_remote_files<H>(
        &mut self,
        handler: &mut H,
        description: &GitDescription,
        checkout_path: &Utf8Path,
        reused_checkout: bool,
    ) -> Result<(H::Item, Url), IngotResolutionError>
    where
        H: ResolutionHandler<Self>,
    {
        let ingot_path = description
            .path
            .as_ref()
            .map(|relative| checkout_path.join(relative))
            .unwrap_or_else(|| checkout_path.to_owned());
        if !ingot_path.exists() {
            return Err(IngotResolutionError::Files(
                FilesResolutionError::DirectoryDoesNotExist(
                    Url::from_directory_path(ingot_path.as_std_path())
                        .expect("valid url for checkout path"),
                ),
            ));
        }
        let ingot_url = Url::from_directory_path(ingot_path.as_std_path())
            .expect("Failed to convert ingot path to URL");
        let files = self
            .resolve_files(handler, &ingot_url)
            .map_err(IngotResolutionError::Files)?;

        Ok((
            handler.handle_resolution(
                &IngotDescriptor::Remote(description.clone()),
                IngotResource {
                    ingot_url: ingot_url.clone(),
                    files,
                    origin: IngotOrigin::Remote {
                        description: description.clone(),
                        checkout_path: checkout_path.to_owned(),
                        reused_checkout,
                    },
                },
            ),
            ingot_url,
        ))
    }

    fn resolve_files<H>(
        &mut self,
        handler: &mut H,
        ingot_url: &Url,
    ) -> Result<FilesResource, FilesResolutionError>
    where
        H: ResolutionHandler<Self>,
    {
        struct ForwardDiagnostics<'a, H> {
            ingot_handler: &'a mut H,
        }
        impl<'a, H> ResolutionHandler<FilesResolver> for ForwardDiagnostics<'a, H>
        where
            H: ResolutionHandler<IngotResolver>,
        {
            type Item = FilesResource;

            fn on_resolution_diagnostic(&mut self, diagnostic: FilesResolutionDiagnostic) {
                self.ingot_handler
                    .on_resolution_diagnostic(IngotResolutionDiagnostic::Files(diagnostic));
            }

            fn handle_resolution(
                &mut self,
                _description: &Url,
                resource: FilesResource,
            ) -> Self::Item {
                resource
            }
        }

        let mut handler = ForwardDiagnostics {
            ingot_handler: handler,
        };
        let files = self.files.resolve(&mut handler, ingot_url)?;
        Ok(files)
    }

    fn resolve_git<H>(
        &mut self,
        handler: &mut H,
        description: &GitDescription,
    ) -> Result<GitResource, GitResolutionError>
    where
        H: ResolutionHandler<Self>,
    {
        struct ForwardDiagnostics<'a, H> {
            ingot_handler: &'a mut H,
        }
        impl<'a, H> ResolutionHandler<GitResolver> for ForwardDiagnostics<'a, H>
        where
            H: ResolutionHandler<IngotResolver>,
        {
            type Item = GitResource;

            fn on_resolution_diagnostic(&mut self, diagnostic: GitResolutionDiagnostic) {
                self.ingot_handler
                    .on_resolution_diagnostic(IngotResolutionDiagnostic::Git(diagnostic));
            }

            fn handle_resolution(
                &mut self,
                _description: &GitDescription,
                resource: GitResource,
            ) -> Self::Item {
                resource
            }
        }

        let mut handler = ForwardDiagnostics {
            ingot_handler: handler,
        };
        self.git.resolve(&mut handler, description)
    }

    fn resolve_local<H>(
        &mut self,
        handler: &mut H,
        url: &Url,
    ) -> Result<H::Item, IngotResolutionError>
    where
        H: ResolutionHandler<Self>,
    {
        handler.on_resolution_start(&IngotDescriptor::Local(url.clone()));
        let files = self
            .resolve_files(handler, url)
            .map_err(IngotResolutionError::Files)?;
        Ok(handler.handle_resolution(
            &IngotDescriptor::Local(url.clone()),
            IngotResource {
                ingot_url: url.clone(),
                files,
                origin: IngotOrigin::Local,
            },
        ))
    }

    fn resolve_remote<H>(
        &mut self,
        handler: &mut H,
        description: &GitDescription,
    ) -> Result<H::Item, IngotResolutionError>
    where
        H: ResolutionHandler<Self>,
    {
        let checkout_path = self.git.checkout_path(description);

        // Try to use an existing valid checkout without hitting the network.
        if self.git.has_valid_cached_checkout(description)
            && let Ok((result, _)) =
                self.load_remote_files(handler, description, checkout_path.as_path(), true)
        {
            return Ok(result);
        }

        // Fallback to fetching/refreshing the checkout and then reading files.
        handler.on_resolution_start(&IngotDescriptor::Remote(description.clone()));
        self.progress.start(description);
        let git_resource = match self.resolve_git(handler, description) {
            Ok(resource) => resource,
            Err(err) => {
                let wrapped = IngotResolutionError::Git(err);
                self.progress.error(description, &wrapped);
                return Err(wrapped);
            }
        };
        let (result, ingot_url) = self.load_remote_files(
            handler,
            description,
            git_resource.checkout_path.as_path(),
            git_resource.reused_checkout,
        )?;
        self.progress.success(description, &ingot_url);
        Ok(result)
    }
}

impl Resolver for IngotResolver {
    type Description = IngotDescriptor;
    type Resource = IngotResource;
    type Error = IngotResolutionError;
    type Diagnostic = IngotResolutionDiagnostic;

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        description: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: ResolutionHandler<Self>,
    {
        match description {
            IngotDescriptor::Local(url) => self.resolve_local(handler, url),
            IngotDescriptor::Remote(desc) => self.resolve_remote(handler, desc),
        }
    }
}
