use std::{fmt, marker::PhantomData, mem::take};

use indexmap::IndexMap;
pub use petgraph::graph::{DiGraph, NodeIndex};

pub use petgraph;

use crate::Resolver;

pub trait GraphResolutionHandler<D, R> {
    type Item;

    fn handle_graph_resolution(&mut self, description: &D, resource: R) -> Self::Item;
}

pub trait GraphResolver<NR, H, E>: Sized
where
    NR: Resolver,
    H: GraphResolutionHandler<NR::Description, DiGraph<NR::Description, E>>
        + crate::ResolutionHandler<NR>,
    <H as crate::ResolutionHandler<NR>>::Item: IntoIterator<Item = (NR::Description, E)>,
    NR::Description: Eq + std::hash::Hash + Clone + fmt::Display,
    E: Clone,
{
    #[allow(clippy::type_complexity)]
    fn graph_resolve(
        &mut self,
        handler: &mut H,
        root_node: &NR::Description,
    ) -> Result<
        <H as GraphResolutionHandler<NR::Description, DiGraph<NR::Description, E>>>::Item,
        UnresolvableRootNode,
    >;

    fn take_diagnostics(&mut self) -> Vec<UnresolvableNode<NR::Description, NR::Error>>;
}

impl<NR, H, E> GraphResolver<NR, H, E> for GraphResolverImpl<NR, H, E>
where
    NR: Resolver,
    H: GraphResolutionHandler<NR::Description, DiGraph<NR::Description, E>>
        + crate::ResolutionHandler<NR>,
    <H as crate::ResolutionHandler<NR>>::Item: IntoIterator<Item = (NR::Description, E)>,
    NR::Description: Eq + std::hash::Hash + Clone + fmt::Display,
    E: Clone,
{
    fn graph_resolve(
        &mut self,
        handler: &mut H,
        root_node: &NR::Description,
    ) -> Result<
        <H as GraphResolutionHandler<NR::Description, DiGraph<NR::Description, E>>>::Item,
        UnresolvableRootNode,
    > {
        tracing::info!(target: "resolver", "Starting graph resolution for root node: {}", root_node);

        let mut graph = DiGraph::default();
        let mut nodes: IndexMap<NR::Description, NodeIndex> = IndexMap::new();
        let mut unresolved_nodes: IndexMap<NR::Description, Vec<(NodeIndex, E)>> = IndexMap::new();
        let mut unresolvable_nodes: IndexMap<NR::Description, Vec<(NodeIndex, E)>> =
            IndexMap::new();

        unresolved_nodes.entry(root_node.clone()).or_default();

        while let Some((unresolved_node_description, back_nodes)) = unresolved_nodes.pop() {
            tracing::info!(target: "resolver", "Resolving node: {}", unresolved_node_description);
            match self
                .node_resolver
                .resolve(handler, &unresolved_node_description)
            {
                Ok(forward_nodes) => {
                    tracing::info!(target: "resolver", "Successfully resolved node: {}", unresolved_node_description);
                    let resolved_node_description = unresolved_node_description;

                    let resolved_node_index = graph.add_node(resolved_node_description.clone());
                    nodes.insert(resolved_node_description.clone(), resolved_node_index);

                    for (back_node_index, back_edge) in &back_nodes {
                        graph.add_edge(*back_node_index, resolved_node_index, back_edge.clone());
                    }

                    for (forward_node_description, forward_edge) in forward_nodes {
                        if unresolvable_nodes.contains_key(&forward_node_description) {
                            unresolvable_nodes
                                .entry(forward_node_description)
                                .or_default()
                                .push((resolved_node_index, forward_edge));
                        } else if !nodes.contains_key(&forward_node_description) {
                            unresolved_nodes
                                .entry(forward_node_description)
                                .or_default()
                                .push((resolved_node_index, forward_edge));
                        } else if let Some(&existing_index) = nodes.get(&forward_node_description) {
                            graph.add_edge(resolved_node_index, existing_index, forward_edge);
                        }
                    }
                }
                Err(error) => {
                    tracing::warn!(target: "resolver", "Failed to resolve node: {}", unresolved_node_description);
                    self.diagnostics
                        .push(UnresolvableNode(unresolved_node_description.clone(), error));
                    unresolvable_nodes
                        .entry(unresolved_node_description)
                        .or_default()
                        .extend(back_nodes);
                }
            }
        }

        if graph.node_count() == 0 {
            tracing::warn!(target: "resolver", "Graph resolution failed: root node is unresolvable");
            Err(UnresolvableRootNode)
        } else {
            tracing::info!(target: "resolver", "Graph resolution completed successfully with {} nodes", graph.node_count());
            let result = handler.handle_graph_resolution(root_node, graph);
            Ok(result)
        }
    }

    fn take_diagnostics(&mut self) -> Vec<UnresolvableNode<NR::Description, NR::Error>> {
        take(&mut self.diagnostics)
    }
}

impl<NR, H, E> Default for GraphResolverImpl<NR, H, E>
where
    NR: Resolver + Default,
{
    fn default() -> Self {
        Self {
            node_resolver: NR::default(),
            diagnostics: vec![],
            _handler: PhantomData,
            _edge: PhantomData,
        }
    }
}

pub struct GraphResolverImpl<NR: Resolver, H, E> {
    pub node_resolver: NR,
    pub diagnostics: Vec<UnresolvableNode<NR::Description, NR::Error>>,
    // These phantom data fields are necessary because H and E are used in the trait implementation
    // but are not stored as fields. They ensure correct type inference for the GraphResolver trait.
    pub _handler: PhantomData<H>,
    pub _edge: PhantomData<E>,
}

impl<NR, H, E> GraphResolverImpl<NR, H, E>
where
    NR: Resolver,
{
    pub fn new(node_resolver: NR) -> Self {
        Self {
            node_resolver,
            diagnostics: vec![],
            _handler: PhantomData,
            _edge: PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct UnresolvableNode<N, E>(pub N, pub E);

#[derive(Debug)]
pub struct UnresolvableRootNode;

impl<N, E> fmt::Display for UnresolvableNode<N, E>
where
    N: fmt::Display,
    E: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Unresolvable node '{}': {}", self.0, self.1)
    }
}

impl<N, E> std::error::Error for UnresolvableNode<N, E>
where
    N: fmt::Debug + fmt::Display,
    E: fmt::Debug + fmt::Display + std::error::Error + 'static,
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.1)
    }
}

impl fmt::Display for UnresolvableRootNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Root node is unresolvable")
    }
}

impl std::error::Error for UnresolvableRootNode {}
