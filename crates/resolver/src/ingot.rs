use camino::{Utf8Path, Utf8PathBuf};
use common::config::looks_like_workspace;
use indexmap::IndexMap;
use std::marker::PhantomData;
use toml::Value;
use url::Url;

use crate::{
    ResolutionHandler, Resolver,
    files::{FilesResolutionDiagnostic, FilesResolutionError, FilesResolver, FilesResource},
    git::{GitDescription, GitResolutionDiagnostic, GitResolutionError, GitResolver, GitResource},
    graph::{
        DiGraph, GraphResolutionHandler, GraphResolverImpl, NodeIndex, UnresolvableRootNode,
        UnresolvedNode,
    },
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

#[derive(Debug, Clone)]
pub enum IngotResolutionEvent {
    RemoteCheckoutStart {
        description: GitDescription,
    },
    RemoteCheckoutComplete {
        description: GitDescription,
        ingot_url: Url,
        reused_checkout: bool,
    },
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

pub trait IngotResolutionHandler<R>:
    ResolutionHandler<R>
    + ResolutionHandler<FilesResolver, Item = FilesResource>
    + IngotResolutionEventHandler
where
    R: Resolver,
{
}

impl<R, T> IngotResolutionHandler<R> for T
where
    R: Resolver,
    T: ResolutionHandler<R>
        + ResolutionHandler<FilesResolver, Item = FilesResource>
        + IngotResolutionEventHandler,
{
}

pub trait IngotResolutionEventHandler {
    fn on_ingot_event(&mut self, _event: IngotResolutionEvent) {}
}

pub trait IngotResolver:
    Resolver<
        Description = IngotDescriptor,
        Resource = IngotResource,
        Error = IngotResolutionError,
        Diagnostic = IngotResolutionDiagnostic,
    >
{
    fn resolve_ingot<H>(
        &mut self,
        handler: &mut H,
        description: &IngotDescriptor,
    ) -> Result<<H as ResolutionHandler<Self>>::Item, IngotResolutionError>
    where
        H: IngotResolutionHandler<Self>;
}

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

    fn load_remote_files<H>(
        &mut self,
        handler: &mut H,
        description: &GitDescription,
        checkout_path: &Utf8Path,
        reused_checkout: bool,
    ) -> Result<(<H as ResolutionHandler<Self>>::Item, Url), IngotResolutionError>
    where
        H: IngotResolutionHandler<Self>,
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
            .resolve_ingot_files(handler, &ingot_url)
            .map_err(IngotResolutionError::Files)?;

        Ok((
            <H as ResolutionHandler<Self>>::handle_resolution(
                handler,
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

    fn resolve_files_with<H>(
        &mut self,
        handler: &mut H,
        resolver: &mut FilesResolver,
        ingot_url: &Url,
    ) -> Result<FilesResource, FilesResolutionError>
    where
        H: ResolutionHandler<FilesResolver, Item = FilesResource>,
    {
        resolver.resolve(handler, ingot_url)
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
            H: ResolutionHandler<IngotResolverImpl>,
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
    ) -> Result<<H as ResolutionHandler<Self>>::Item, IngotResolutionError>
    where
        H: IngotResolutionHandler<Self>,
    {
        <H as ResolutionHandler<Self>>::on_resolution_start(
            handler,
            &IngotDescriptor::Local(url.clone()),
        );
        let files = self
            .resolve_ingot_files(handler, url)
            .map_err(IngotResolutionError::Files)?;
        Ok(<H as ResolutionHandler<Self>>::handle_resolution(
            handler,
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
    ) -> Result<<H as ResolutionHandler<Self>>::Item, IngotResolutionError>
    where
        H: IngotResolutionHandler<Self>,
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
        <H as ResolutionHandler<Self>>::on_resolution_start(
            handler,
            &IngotDescriptor::Remote(description.clone()),
        );
        IngotResolutionEventHandler::on_ingot_event(
            handler,
            IngotResolutionEvent::RemoteCheckoutStart {
                description: description.clone(),
            },
        );
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
        IngotResolutionEventHandler::on_ingot_event(
            handler,
            IngotResolutionEvent::RemoteCheckoutComplete {
                description: description.clone(),
                ingot_url: ingot_url.clone(),
                reused_checkout: git_resource.reused_checkout,
            },
        );
        self.progress.success(description, &ingot_url);
        Ok(result)
    }

    fn resolve_ingot_files<H>(
        &mut self,
        handler: &mut H,
        ingot_url: &Url,
    ) -> Result<FilesResource, FilesResolutionError>
    where
        H: ResolutionHandler<FilesResolver, Item = FilesResource>,
    {
        let mut config_resolver = minimal_files_resolver();
        let config_files = self.resolve_files_with(handler, &mut config_resolver, ingot_url)?;
        let ingot_path = Utf8PathBuf::from(ingot_url.path());
        let config_kind = config_kind_from_files(&ingot_path, &config_files);

        let mut extra_resolver = match config_kind {
            Some(ConfigKind::Workspace) => workspace_files_resolver(),
            Some(ConfigKind::Ingot) => project_files_resolver(),
            None => return Ok(config_files),
        };

        let extra_files = self.resolve_files_with(handler, &mut extra_resolver, ingot_url)?;
        Ok(merge_files(config_files, extra_files))
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ConfigKind {
    Ingot,
    Workspace,
}

fn config_kind_from_files(root: &Utf8PathBuf, files: &FilesResource) -> Option<ConfigKind> {
    let config_path = root.join("fe.toml");
    let file = files.files.iter().find(|file| file.path == config_path)?;
    config_kind_from_content(&file.content)
}

fn config_kind_from_content(content: &str) -> Option<ConfigKind> {
    let parsed: Value = content.parse().ok()?;
    let table = parsed.as_table()?;
    if table.contains_key("workspace") || looks_like_workspace(&parsed) {
        return Some(ConfigKind::Workspace);
    }
    if table.contains_key("ingot") {
        return Some(ConfigKind::Ingot);
    }
    Some(ConfigKind::Ingot)
}

fn merge_files(mut base: FilesResource, extra: FilesResource) -> FilesResource {
    let mut seen = std::collections::HashSet::new();
    for file in base.files.iter() {
        seen.insert(file.path.clone());
    }
    for file in extra.files {
        if seen.insert(file.path.clone()) {
            base.files.push(file);
        }
    }
    base
}

impl IngotResolver for IngotResolverImpl {
    fn resolve_ingot<H>(
        &mut self,
        handler: &mut H,
        description: &IngotDescriptor,
    ) -> Result<<H as ResolutionHandler<Self>>::Item, IngotResolutionError>
    where
        H: IngotResolutionHandler<Self>,
    {
        match description {
            IngotDescriptor::Local(url) => self.resolve_local(handler, url),
            IngotDescriptor::Remote(desc) => self.resolve_remote(handler, desc),
        }
    }
}

struct ForwardingHandler<'a, H> {
    ingot_handler: &'a mut H,
}

impl<'a, H> ResolutionHandler<IngotResolverImpl> for ForwardingHandler<'a, H>
where
    H: ResolutionHandler<IngotResolverImpl>,
{
    type Item = H::Item;

    fn on_resolution_start(&mut self, description: &IngotDescriptor) {
        self.ingot_handler.on_resolution_start(description);
    }

    fn on_resolution_diagnostic(&mut self, diagnostic: IngotResolutionDiagnostic) {
        self.ingot_handler.on_resolution_diagnostic(diagnostic);
    }

    fn on_resolution_error(&mut self, description: &IngotDescriptor, error: IngotResolutionError) {
        self.ingot_handler.on_resolution_error(description, error);
    }

    fn handle_resolution(
        &mut self,
        description: &IngotDescriptor,
        resource: IngotResource,
    ) -> Self::Item {
        self.ingot_handler.handle_resolution(description, resource)
    }
}

impl<'a, H> ResolutionHandler<FilesResolver> for ForwardingHandler<'a, H>
where
    H: ResolutionHandler<IngotResolverImpl>,
{
    type Item = FilesResource;

    fn on_resolution_diagnostic(&mut self, diagnostic: FilesResolutionDiagnostic) {
        self.ingot_handler
            .on_resolution_diagnostic(IngotResolutionDiagnostic::Files(diagnostic));
    }

    fn handle_resolution(&mut self, _description: &Url, resource: FilesResource) -> Self::Item {
        resource
    }
}

impl<'a, H> IngotResolutionEventHandler for ForwardingHandler<'a, H> {}

impl Resolver for IngotResolverImpl {
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
        let mut forwarding = ForwardingHandler {
            ingot_handler: handler,
        };
        self.resolve_ingot(&mut forwarding, description)
    }
}

type BackEdges<E> = Vec<(NodeIndex, E)>;
type UnresolvedMap<D, E> = IndexMap<D, (IngotPriority, BackEdges<E>)>;

pub struct IngotGraphResolverImpl<IR, H, E> {
    pub node_resolver: IR,
    pub _handler: PhantomData<H>,
    pub _edge: PhantomData<E>,
}

impl<IR, H, E> IngotGraphResolverImpl<IR, H, E>
where
    IR: IngotResolver,
{
    pub fn new(node_resolver: IR) -> Self {
        Self {
            node_resolver,
            _handler: PhantomData,
            _edge: PhantomData,
        }
    }
}

impl<IR, H, E> IngotGraphResolverImpl<IR, H, E>
where
    IR: IngotResolver,
    H: GraphResolutionHandler<IngotDescriptor, DiGraph<IngotDescriptor, E>>
        + IngotResolutionHandler<IR>,
    <H as ResolutionHandler<IR>>::Item:
        IntoIterator<Item = UnresolvedNode<IngotPriority, IngotDescriptor, E>>,
    E: Clone,
{
    pub fn graph_resolve(
        &mut self,
        handler: &mut H,
        root_node: &IngotDescriptor,
    ) -> Result<
        <H as GraphResolutionHandler<IngotDescriptor, DiGraph<IngotDescriptor, E>>>::Item,
        UnresolvableRootNode,
    > {
        tracing::info!(target: "resolver", "Starting ingot graph resolution");

        let mut graph = DiGraph::default();
        let mut nodes: IndexMap<IngotDescriptor, NodeIndex> = IndexMap::new();
        let mut unresolved_nodes: UnresolvedMap<IngotDescriptor, E> = IndexMap::new();
        let mut unresolvable_nodes: IndexMap<IngotDescriptor, BackEdges<E>> = IndexMap::new();

        unresolved_nodes
            .entry(root_node.clone())
            .or_insert_with(|| (IngotPriority::default(), Vec::new()));

        while let Some((unresolved_node_description, back_nodes)) =
            take_highest_priority(&mut unresolved_nodes)
        {
            tracing::info!(target: "resolver", "Resolving node");
            match self
                .node_resolver
                .resolve_ingot(handler, &unresolved_node_description)
            {
                Ok(forward_nodes) => {
                    tracing::info!(target: "resolver", "Successfully resolved node");
                    let resolved_node_description = unresolved_node_description;

                    let resolved_node_index = graph.add_node(resolved_node_description.clone());
                    nodes.insert(resolved_node_description.clone(), resolved_node_index);

                    for (back_node_index, back_edge) in &back_nodes {
                        graph.add_edge(*back_node_index, resolved_node_index, back_edge.clone());
                    }

                    for UnresolvedNode {
                        priority,
                        description: forward_node_description,
                        edge: forward_edge,
                    } in forward_nodes
                    {
                        if unresolvable_nodes.contains_key(&forward_node_description) {
                            unresolvable_nodes
                                .entry(forward_node_description)
                                .or_default()
                                .push((resolved_node_index, forward_edge));
                        } else if !nodes.contains_key(&forward_node_description) {
                            unresolved_nodes
                                .entry(forward_node_description)
                                .and_modify(|(existing_priority, back_edges)| {
                                    if priority > *existing_priority {
                                        *existing_priority = priority;
                                    }
                                    back_edges.push((resolved_node_index, forward_edge.clone()));
                                })
                                .or_insert_with(|| {
                                    (priority, vec![(resolved_node_index, forward_edge)])
                                });
                        } else if let Some(&existing_index) = nodes.get(&forward_node_description) {
                            graph.add_edge(resolved_node_index, existing_index, forward_edge);
                        }
                    }
                }
                Err(error) => {
                    tracing::warn!(target: "resolver", "Failed to resolve node");
                    <H as ResolutionHandler<IR>>::on_resolution_error(
                        handler,
                        &unresolved_node_description,
                        error,
                    );
                    unresolvable_nodes
                        .entry(unresolved_node_description)
                        .or_default()
                        .extend(back_nodes);
                }
            }
        }

        if graph.node_count() == 0 {
            tracing::warn!(
                target: "resolver",
                "Graph resolution failed: root node is unresolvable"
            );
            Err(UnresolvableRootNode)
        } else {
            tracing::info!(
                target: "resolver",
                "Graph resolution completed successfully with {} nodes",
                graph.node_count()
            );
            let result = handler.handle_graph_resolution(root_node, graph);
            Ok(result)
        }
    }
}

fn take_highest_priority<D, E>(
    unresolved_nodes: &mut UnresolvedMap<D, E>,
) -> Option<(D, BackEdges<E>)>
where
    D: Eq + std::hash::Hash + Clone,
{
    let mut best_index = None;
    let mut best_priority: Option<IngotPriority> = None;

    for (index, (_description, (priority, _))) in unresolved_nodes.iter().enumerate() {
        let should_replace = match &best_priority {
            None => true,
            Some(current_best) => priority > current_best,
        };

        if should_replace {
            best_priority = Some(*priority);
            best_index = Some(index);
        }
    }

    best_index.and_then(|index| {
        unresolved_nodes
            .shift_remove_index(index)
            .map(|(description, (_priority, back_nodes))| (description, back_nodes))
    })
}
