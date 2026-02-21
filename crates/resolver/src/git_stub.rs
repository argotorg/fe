use std::{error::Error, fmt};

use camino::Utf8PathBuf;
use url::Url;

use crate::{ResolutionHandler, Resolver};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GitDescription {
    pub source: Url,
    pub rev: String,
    pub path: Option<Utf8PathBuf>,
}

impl GitDescription {
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

impl fmt::Display for GitDescription {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.path {
            Some(path) => write!(f, "{}#{} ({})", self.source, self.rev, path),
            None => write!(f, "{}#{}", self.source, self.rev),
        }
    }
}

#[derive(Clone, Debug)]
pub struct GitResource {
    pub reused_checkout: bool,
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
}

impl fmt::Display for GitResolutionDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GitResolutionDiagnostic::UnsupportedTarget(url) => {
                write!(f, "git resolution unsupported for {url}")
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

    pub fn checkout_path(&self, _description: &GitDescription) -> Utf8PathBuf {
        Utf8PathBuf::from("unsupported-wasm")
    }

    pub fn has_valid_cached_checkout(&self, _description: &GitDescription) -> bool {
        false
    }

    pub fn ensure_checkout_resource(
        &self,
        _description: &GitDescription,
    ) -> Result<GitResource, GitResolutionError> {
        Err(GitResolutionError::UnsupportedTarget)
    }
}

#[derive(Debug, Clone)]
pub enum GitResolutionEvent {
    CheckoutStart {
        description: GitDescription,
    },
    CheckoutComplete {
        description: GitDescription,
        checkout_path: Utf8PathBuf,
        reused_checkout: bool,
    },
}

impl Resolver for GitResolver {
    type Description = GitDescription;
    type Resource = GitResource;
    type Error = GitResolutionError;
    type Diagnostic = GitResolutionDiagnostic;
    type Event = GitResolutionEvent;

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        description: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: ResolutionHandler<Self>,
    {
        handler.on_resolution_event(GitResolutionEvent::CheckoutStart {
            description: description.clone(),
        });
        let resource = GitResource {
            reused_checkout: false,
            checkout_path: Utf8PathBuf::from("unsupported-wasm"),
        };
        handler.on_resolution_event(GitResolutionEvent::CheckoutComplete {
            description: description.clone(),
            checkout_path: resource.checkout_path.clone(),
            reused_checkout: resource.reused_checkout,
        });
        // Return an error so graph resolution will record an unresolvable node.
        let _ = resource;
        Err(GitResolutionError::UnsupportedTarget)
    }
}
