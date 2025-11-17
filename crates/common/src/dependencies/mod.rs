pub mod display_tree;
pub mod graph;

use crate::ingot::Version;
use camino::Utf8PathBuf;
use smol_str::SmolStr;
use url::Url;

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct DependencyArguments {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GitDependency {
    pub source: Url,
    pub rev: SmolStr,
    pub path: Option<Utf8PathBuf>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LocalDependency {
    pub path: Utf8PathBuf,
    pub url: Url,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DependencyLocation {
    Local(LocalDependency),
    Git(GitDependency),
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
            DependencyLocation::Git(git) => &git.source,
        }
    }
}

#[derive(Clone, Debug)]
pub struct RemoteDependencyRequest {
    pub parent: Url,
    pub alias: SmolStr,
    pub arguments: DependencyArguments,
    pub git: GitDependency,
}

pub use display_tree::display_tree;
pub use graph::DependencyGraph;
