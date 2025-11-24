use std::{error::Error, fmt};

use camino::Utf8PathBuf;
use url::Url;

use crate::{ResolutionHandler, Resolver};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GitDependencyDescription {
    pub source: Url,
    pub rev: String,
    pub path: Option<Utf8PathBuf>,
}

impl GitDependencyDescription {
    pub fn new(source: Url, rev: String) -> Self {
        Self {
            source,
            rev,
            path: None,
        }
    }

    pub fn with_path(mut self, path: Utf8PathBuf) -> Self {
        self.path = Some(path);
        self
    }
}

impl fmt::Display for GitDependencyDescription {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.path {
            Some(path) => write!(f, "{}#{} ({})", self.source, self.rev, path),
            None => write!(f, "{}#{}", self.source, self.rev),
        }
    }
}

#[derive(Clone, Debug)]
pub struct GitResource {
    pub source: Url,
    pub rev: String,
    pub ingot_url: Url,
    pub ingot_path: Utf8PathBuf,
    pub checkout_path: Utf8PathBuf,
}

#[derive(Debug)]
pub enum GitResolutionError {
    UnsupportedTarget,
}

impl fmt::Display for GitResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GitResolutionError::UnsupportedTarget => {
                write!(f, "git resolution is unsupported on wasm targets")
            }
        }
    }
}

impl Error for GitResolutionError {}

#[derive(Debug)]
pub enum GitResolutionDiagnostic {
    UnsupportedTarget(Url),
    ReusedCheckout(Utf8PathBuf),
}

impl fmt::Display for GitResolutionDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GitResolutionDiagnostic::UnsupportedTarget(url) => {
                write!(f, "git resolution unsupported for {url}")
            }
            GitResolutionDiagnostic::ReusedCheckout(path) => {
                write!(f, "Reused existing checkout at {}", path)
            }
        }
    }
}

#[derive(Default)]
pub struct GitResolver;

impl GitResolver {
    pub fn new(_checkout_root: Utf8PathBuf) -> Self {
        Self
    }
}

impl Resolver for GitResolver {
    type Description = GitDependencyDescription;
    type Resource = GitResource;
    type Error = GitResolutionError;
    type Diagnostic = GitResolutionDiagnostic;

    fn resolve<H>(
        &mut self,
        _handler: &mut H,
        description: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: ResolutionHandler<Self>,
    {
        let resource = GitResource {
            source: description.source.clone(),
            rev: description.rev.clone(),
            ingot_url: description.source.clone(),
            ingot_path: Utf8PathBuf::from("unsupported-wasm"),
            checkout_path: Utf8PathBuf::from("unsupported-wasm"),
        };
        // Return an error so graph resolution will record an unresolvable node.
        let _ = resource;
        Err(GitResolutionError::UnsupportedTarget)
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        Vec::new()
    }
}
