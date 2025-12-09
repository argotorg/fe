use std::{fmt, marker::PhantomData};

use indexmap::IndexMap;
pub use petgraph::graph::{DiGraph, NodeIndex};

pub use petgraph;

use crate::Resolver;

type BackEdges<E> = Vec<(NodeIndex, E)>;
type UnresolvedMap<D, P, E> = IndexMap<D, (P, BackEdges<E>)>;

pub trait GraphResolutionHandler<D, R> {
    type Item;

    fn handle_graph_resolution(&mut self, description: &D, resource: R) -> Self::Item;
}

pub trait GraphResolver<NR, H, E>: Sized
where
    NR: Resolver,
    H: GraphResolutionHandler<NR::Description, DiGraph<NR::Description, E>>
        + crate::ResolutionHandler<NR>,
    <H as crate::ResolutionHandler<NR>>::Item:
        IntoIterator<Item = UnresolvedNode<Self::Priority, NR::Description, E>>,
    NR::Description: Eq + std::hash::Hash + Clone,
    E: Clone,
{
    type Priority: Ord + Clone + Default;

    #[allow(clippy::type_complexity)]
    fn graph_resolve(
        &mut self,
        handler: &mut H,
        root_node: &NR::Description,
    ) -> Result<
        <H as GraphResolutionHandler<NR::Description, DiGraph<NR::Description, E>>>::Item,
        UnresolvableRootNode,
    >;
}

impl<NR, H, E, P> GraphResolver<NR, H, E> for GraphResolverImpl<NR, H, E, P>
where
    NR: Resolver,
    H: GraphResolutionHandler<NR::Description, DiGraph<NR::Description, E>>
        + crate::ResolutionHandler<NR>,
    <H as crate::ResolutionHandler<NR>>::Item:
        IntoIterator<Item = UnresolvedNode<P, NR::Description, E>>,
    NR::Description: Eq + std::hash::Hash + Clone,
    E: Clone,
    P: Ord + Clone + Default,
{
    type Priority = P;

    fn graph_resolve(
        &mut self,
        handler: &mut H,
        root_node: &NR::Description,
    ) -> Result<
        <H as GraphResolutionHandler<NR::Description, DiGraph<NR::Description, E>>>::Item,
        UnresolvableRootNode,
    > {
        tracing::info!(target: "resolver", "Starting graph resolution");

        let mut graph = DiGraph::default();
        let mut nodes: IndexMap<NR::Description, NodeIndex> = IndexMap::new();
        let mut unresolved_nodes: UnresolvedMap<NR::Description, P, E> = IndexMap::new();
        let mut unresolvable_nodes: IndexMap<NR::Description, BackEdges<E>> = IndexMap::new();

        unresolved_nodes
            .entry(root_node.clone())
            .or_insert_with(|| (P::default(), Vec::new()));

        while let Some((unresolved_node_description, back_nodes)) =
            take_highest_priority(&mut unresolved_nodes)
        {
            tracing::info!(target: "resolver", "Resolving node");
            match self
                .node_resolver
                .resolve(handler, &unresolved_node_description)
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
                                        *existing_priority = priority.clone();
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
                    handler.on_resolution_error(&unresolved_node_description, error);
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
}

impl<NR, H, E, P> Default for GraphResolverImpl<NR, H, E, P>
where
    NR: Resolver + Default,
{
    fn default() -> Self {
        Self {
            node_resolver: NR::default(),
            _handler: PhantomData,
            _edge: PhantomData,
            _priority: PhantomData,
        }
    }
}

pub struct GraphResolverImpl<NR: Resolver, H, E, P> {
    pub node_resolver: NR,
    // These phantom data fields are necessary because H and E are used in the trait implementation
    // but are not stored as fields. They ensure correct type inference for the GraphResolver trait.
    pub _handler: PhantomData<H>,
    pub _edge: PhantomData<E>,
    pub _priority: PhantomData<P>,
}

impl<NR, H, E, P> GraphResolverImpl<NR, H, E, P>
where
    NR: Resolver,
{
    pub fn new(node_resolver: NR) -> Self {
        Self {
            node_resolver,
            _handler: PhantomData,
            _edge: PhantomData,
            _priority: PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct UnresolvableNode<N, E>(pub N, pub E);

#[derive(Debug)]
pub struct UnresolvableRootNode;

#[derive(Debug, Clone)]
pub struct UnresolvedNode<P, D, E> {
    pub priority: P,
    pub description: D,
    pub edge: E,
}

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

fn take_highest_priority<D, E, P>(
    unresolved_nodes: &mut UnresolvedMap<D, P, E>,
) -> Option<(D, BackEdges<E>)>
where
    P: Ord + Clone,
{
    let mut best_index = None;
    let mut best_priority: Option<P> = None;

    for (index, (_description, (priority, _))) in unresolved_nodes.iter().enumerate() {
        let should_replace = match &best_priority {
            None => true,
            Some(current_best) => priority > current_best,
        };

        if should_replace {
            best_priority = Some(priority.clone());
            best_index = Some(index);
        }
    }

    best_index.and_then(|index| {
        unresolved_nodes
            .shift_remove_index(index)
            .map(|(description, (_priority, back_nodes))| (description, back_nodes))
    })
}
