use crate::{files::FilesResolver, git::GitResolver, graph::GraphResolverImpl};

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
pub type LocalGraphResolver<H, E> = GraphResolverImpl<FilesResolver, H, E>;

/// Convenience alias for graph resolvers that walk remote git dependencies.
pub type RemoteGraphResolver<H, E> = GraphResolverImpl<GitResolver, H, E>;
