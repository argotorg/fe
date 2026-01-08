pub mod graph;
pub mod tree;

use crate::ingot::Version;
use camino::Utf8PathBuf;
use smol_str::SmolStr;
use url::Url;

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct DependencyArguments {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

pub type DependencyAlias = SmolStr;

/// Metadata describing a git checkout on disk. This can later become an enum if
/// we support multiple remote transport mechanisms.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RemoteFiles {
    pub source: Url,
    pub rev: SmolStr,
    pub path: Option<Utf8PathBuf>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LocalFiles {
    pub path: Utf8PathBuf,
    pub url: Url,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DependencyLocation {
    Local(LocalFiles),
    Remote(RemoteFiles),
    WorkspaceCurrent,
}

#[derive(Clone, Debug)]
pub struct Dependency {
    pub alias: SmolStr,
    pub location: DependencyLocation,
    pub arguments: DependencyArguments,
}

impl Dependency {
    pub fn url(&self) -> &Url {
        match &self.location {
            DependencyLocation::Local(local) => &local.url,
            DependencyLocation::Remote(remote) => &remote.source,
            DependencyLocation::WorkspaceCurrent => panic!("workspace current has no URL"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExternalDependencyEdge {
    pub parent: Url,
    pub alias: SmolStr,
    pub arguments: DependencyArguments,
    pub remote: RemoteFiles,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WorkspaceMemberRecord {
    pub name: SmolStr,
    pub version: Option<Version>,
    pub path: Utf8PathBuf,
    pub url: Url,
}

pub use graph::DependencyGraph;
pub use tree::DependencyTree;
