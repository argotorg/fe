pub mod display_tree;
pub mod graph;

use crate::ingot::Version;
use smol_str::SmolStr;
use url::Url;

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct DependencyArguments {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

#[derive(Clone, Debug)]
pub struct Dependency {
    pub url: Url,
    pub alias: SmolStr,
    pub arguments: DependencyArguments,
}

pub use display_tree::display_tree;
pub use graph::DependencyGraph;
