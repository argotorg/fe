use std::{
    collections::{HashMap, VecDeque},
    fmt,
    marker::PhantomData,
    sync::{Arc, Condvar, Mutex, mpsc},
    thread,
};

use indexmap::IndexMap;
pub use petgraph::graph::{DiGraph, NodeIndex};

pub use petgraph;

use crate::Resolver;

type BackEdges<E> = Vec<(NodeIndex, E)>;
type UnresolvedMap<D, P, E> = IndexMap<D, (P, BackEdges<E>)>;

#[derive(Debug, Clone)]
pub struct GraphNodeOutcome<P, D, E> {
    /// The canonical node description to use in the resolved graph.
    pub canonical_description: D,
    /// Any additional descriptions that should be treated as aliases of `canonical_description`.
    pub aliases: Vec<D>,
    /// The forward edges for the node.
    pub forward_nodes: Vec<UnresolvedNode<P, D, E>>,
}

pub trait GraphNodeResolution<P, D, E, Err> {
    fn into_outcome(self, original: &D) -> Result<GraphNodeOutcome<P, D, E>, Err>;
}

impl<P, D, E, Err> GraphNodeResolution<P, D, E, Err> for Vec<UnresolvedNode<P, D, E>>
where
    D: Clone,
{
    fn into_outcome(self, original: &D) -> Result<GraphNodeOutcome<P, D, E>, Err> {
        Ok(GraphNodeOutcome {
            canonical_description: original.clone(),
            aliases: Vec::new(),
            forward_nodes: self,
        })
    }
}

impl<P, D, E, Err> GraphNodeResolution<P, D, E, Err> for GraphNodeOutcome<P, D, E> {
    fn into_outcome(self, _original: &D) -> Result<GraphNodeOutcome<P, D, E>, Err> {
        Ok(self)
    }
}

impl<P, D, E, Err> GraphNodeResolution<P, D, E, Err> for Result<GraphNodeOutcome<P, D, E>, Err> {
    fn into_outcome(self, _original: &D) -> Result<GraphNodeOutcome<P, D, E>, Err> {
        self
    }
}

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
        GraphNodeResolution<Self::Priority, NR::Description, E, NR::Error>,
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

#[derive(Clone)]
pub struct AsyncGraphResolverImpl<NR: Resolver, H, E, P> {
    make_resolver: Arc<dyn Fn() -> NR + Send + Sync>,
    max_concurrency: usize,
    pub _handler: PhantomData<H>,
    pub _edge: PhantomData<E>,
    pub _priority: PhantomData<P>,
}

impl<NR: Resolver, H, E, P> AsyncGraphResolverImpl<NR, H, E, P> {
    pub fn new(make_resolver: impl Fn() -> NR + Send + Sync + 'static) -> Self {
        let max_concurrency = thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1);
        Self {
            make_resolver: Arc::new(make_resolver),
            max_concurrency: max_concurrency.max(1),
            _handler: PhantomData,
            _edge: PhantomData,
            _priority: PhantomData,
        }
    }

    pub fn with_max_concurrency(mut self, max_concurrency: usize) -> Self {
        self.max_concurrency = max_concurrency.max(1);
        self
    }
}

enum CapturedAction<D, E> {
    Diagnostic(D),
    Event(E),
}

struct CaptureHandler<NR: Resolver> {
    before: Vec<CapturedAction<NR::Diagnostic, NR::Event>>,
    after: Vec<CapturedAction<NR::Diagnostic, NR::Event>>,
    in_after: bool,
}

impl<NR: Resolver> Default for CaptureHandler<NR> {
    fn default() -> Self {
        Self {
            before: Vec::new(),
            after: Vec::new(),
            in_after: false,
        }
    }
}

impl<NR> crate::ResolutionHandler<NR> for CaptureHandler<NR>
where
    NR: Resolver,
{
    type Item = NR::Resource;

    fn on_resolution_diagnostic(&mut self, diagnostic: NR::Diagnostic) {
        if self.in_after {
            self.after.push(CapturedAction::Diagnostic(diagnostic));
        } else {
            self.before.push(CapturedAction::Diagnostic(diagnostic));
        }
    }

    fn on_resolution_event(&mut self, event: NR::Event) {
        if self.in_after {
            self.after.push(CapturedAction::Event(event));
        } else {
            self.before.push(CapturedAction::Event(event));
        }
    }

    fn handle_resolution(
        &mut self,
        _description: &NR::Description,
        resource: NR::Resource,
    ) -> Self::Item {
        self.in_after = true;
        resource
    }
}

struct WorkQueue<T> {
    state: Mutex<WorkQueueState<T>>,
    available: Condvar,
}

struct WorkQueueState<T> {
    queue: VecDeque<T>,
    closed: bool,
}

impl<T> WorkQueue<T> {
    fn new() -> Self {
        Self {
            state: Mutex::new(WorkQueueState {
                queue: VecDeque::new(),
                closed: false,
            }),
            available: Condvar::new(),
        }
    }

    fn push(&self, task: T) {
        let mut state = self.state.lock().expect("work queue lock");
        if state.closed {
            return;
        }
        state.queue.push_back(task);
        self.available.notify_one();
    }

    fn pop(&self) -> Option<T> {
        let mut state = self.state.lock().expect("work queue lock");
        loop {
            if let Some(task) = state.queue.pop_front() {
                return Some(task);
            }
            if state.closed {
                return None;
            }
            state = self.available.wait(state).expect("work queue wait");
        }
    }

    fn close(&self) {
        let mut state = self.state.lock().expect("work queue lock");
        state.closed = true;
        self.available.notify_all();
    }
}

struct NodeTask<D> {
    description: D,
}

struct NodeResult<D, R, Err, Diag, Ev> {
    description: D,
    resolution: Result<R, Err>,
    before: Vec<CapturedAction<Diag, Ev>>,
    after: Vec<CapturedAction<Diag, Ev>>,
}

type NodeResultFor<NR> = NodeResult<
    <NR as Resolver>::Description,
    <NR as Resolver>::Resource,
    <NR as Resolver>::Error,
    <NR as Resolver>::Diagnostic,
    <NR as Resolver>::Event,
>;

impl<NR, H, E, P> GraphResolver<NR, H, E> for GraphResolverImpl<NR, H, E, P>
where
    NR: Resolver,
    H: GraphResolutionHandler<NR::Description, DiGraph<NR::Description, E>>
        + crate::ResolutionHandler<NR>,
    <H as crate::ResolutionHandler<NR>>::Item:
        GraphNodeResolution<P, NR::Description, E, NR::Error>,
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
        let mut aliases: HashMap<NR::Description, NR::Description> = HashMap::new();

        unresolved_nodes
            .entry(root_node.clone())
            .or_insert_with(|| (P::default(), Vec::new()));

        while let Some((unresolved_node_description, priority, back_nodes)) =
            take_highest_priority(&mut unresolved_nodes)
        {
            let canonical = canonicalize_description(&unresolved_node_description, &mut aliases);
            if canonical != unresolved_node_description {
                attach_back_edges(
                    &mut graph,
                    &nodes,
                    &mut unresolved_nodes,
                    &mut unresolvable_nodes,
                    canonical,
                    priority,
                    back_nodes,
                );
                continue;
            }

            if let Some(&resolved_node_index) = nodes.get(&canonical) {
                for (back_node_index, back_edge) in back_nodes {
                    graph.add_edge(back_node_index, resolved_node_index, back_edge);
                }
                continue;
            }

            tracing::info!(target: "resolver", "Resolving node");
            match self
                .node_resolver
                .resolve(handler, &unresolved_node_description)
            {
                Ok(item) => match item.into_outcome(&unresolved_node_description) {
                    Ok(outcome) => {
                        tracing::info!(target: "resolver", "Successfully resolved node");

                        let canonical_description =
                            canonicalize_description(&outcome.canonical_description, &mut aliases);
                        if canonical_description != unresolved_node_description {
                            aliases.insert(
                                unresolved_node_description.clone(),
                                canonical_description.clone(),
                            );
                        }
                        for alias in outcome.aliases {
                            aliases.insert(alias, canonical_description.clone());
                        }

                        let resolved_node_index =
                            if let Some(&existing) = nodes.get(&canonical_description) {
                                existing
                            } else {
                                let index = graph.add_node(canonical_description.clone());
                                nodes.insert(canonical_description.clone(), index);
                                index
                            };

                        for (back_node_index, back_edge) in back_nodes {
                            graph.add_edge(back_node_index, resolved_node_index, back_edge);
                        }

                        for UnresolvedNode {
                            priority,
                            description: forward_node_description,
                            edge: forward_edge,
                        } in outcome.forward_nodes
                        {
                            let forward_node_description =
                                canonicalize_description(&forward_node_description, &mut aliases);
                            attach_back_edges(
                                &mut graph,
                                &nodes,
                                &mut unresolved_nodes,
                                &mut unresolvable_nodes,
                                forward_node_description,
                                priority,
                                vec![(resolved_node_index, forward_edge)],
                            );
                        }
                    }
                    Err(error) => {
                        tracing::warn!(target: "resolver", "Node handler reported failure");
                        handler.on_resolution_error(&unresolved_node_description, error);
                        unresolvable_nodes
                            .entry(unresolved_node_description)
                            .or_default()
                            .extend(back_nodes);
                    }
                },
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
            tracing::info!(
                target: "resolver",
                "Graph resolution completed successfully with {} nodes",
                graph.node_count()
            );
            let root_node = canonicalize_description(root_node, &mut aliases);
            let result = handler.handle_graph_resolution(&root_node, graph);
            Ok(result)
        }
    }
}

impl<NR, H, E, P> GraphResolver<NR, H, E> for AsyncGraphResolverImpl<NR, H, E, P>
where
    NR: Resolver + Send + 'static,
    NR::Description: Eq + std::hash::Hash + Clone + Send + 'static,
    NR::Resource: Send + 'static,
    NR::Error: Send + 'static,
    NR::Diagnostic: Send + 'static,
    NR::Event: Send + 'static,
    H: GraphResolutionHandler<NR::Description, DiGraph<NR::Description, E>>
        + crate::ResolutionHandler<NR>,
    <H as crate::ResolutionHandler<NR>>::Item:
        GraphNodeResolution<P, NR::Description, E, NR::Error>,
    E: Clone + Send + 'static,
    P: Ord + Clone + Default + Send + 'static,
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
        tracing::info!(target: "resolver", "Starting async graph resolution");

        let mut graph = DiGraph::default();
        let mut nodes: IndexMap<NR::Description, NodeIndex> = IndexMap::new();
        let mut unresolved_nodes: UnresolvedMap<NR::Description, P, E> = IndexMap::new();
        let mut in_flight: HashMap<NR::Description, (P, BackEdges<E>)> = HashMap::new();
        let mut in_flight_order: VecDeque<NR::Description> = VecDeque::new();
        let mut completed: HashMap<NR::Description, NodeResultFor<NR>> = HashMap::new();
        let mut unresolvable_nodes: IndexMap<NR::Description, BackEdges<E>> = IndexMap::new();
        let mut aliases: HashMap<NR::Description, NR::Description> = HashMap::new();

        unresolved_nodes
            .entry(root_node.clone())
            .or_insert_with(|| (P::default(), Vec::new()));

        let max_concurrency = self.max_concurrency.max(1);
        let work_queue = Arc::new(WorkQueue::<NodeTask<NR::Description>>::new());
        let (result_tx, result_rx) = mpsc::channel::<NodeResultFor<NR>>();

        let mut worker_handles = Vec::new();
        for _ in 0..max_concurrency {
            let queue = work_queue.clone();
            let tx = result_tx.clone();
            let make_resolver = self.make_resolver.clone();
            worker_handles.push(thread::spawn(move || {
                let mut resolver = (make_resolver)();
                while let Some(task) = queue.pop() {
                    let mut capture = CaptureHandler::<NR>::default();
                    let resolution = resolver.resolve(&mut capture, &task.description);
                    let result = NodeResult {
                        description: task.description,
                        resolution,
                        before: capture.before,
                        after: capture.after,
                    };
                    if tx.send(result).is_err() {
                        break;
                    }
                }
            }));
        }
        drop(result_tx);

        while !unresolved_nodes.is_empty() || !in_flight.is_empty() || !completed.is_empty() {
            let max_unresolved_priority = unresolved_nodes
                .values()
                .map(|(priority, _)| priority.clone())
                .max();
            let max_in_flight_priority = in_flight
                .values()
                .map(|(priority, _)| priority.clone())
                .max();
            let active_priority = match (
                max_unresolved_priority.clone(),
                max_in_flight_priority.clone(),
            ) {
                (Some(a), Some(b)) => Some(if a > b { a } else { b }),
                (Some(a), None) => Some(a),
                (None, Some(b)) => Some(b),
                (None, None) => None,
            };

            // Dispatch new work, but only for the current highest-priority class.
            while in_flight.len() < max_concurrency
                && max_unresolved_priority.is_some()
                && active_priority.as_ref() == max_unresolved_priority.as_ref()
            {
                let Some((unresolved_description, priority, back_edges)) =
                    take_highest_priority(&mut unresolved_nodes)
                else {
                    break;
                };

                let canonical = canonicalize_description(&unresolved_description, &mut aliases);
                if canonical != unresolved_description {
                    {
                        let mut context = AsyncBackEdgeContext {
                            graph: &mut graph,
                            nodes: &nodes,
                            unresolved_nodes: &mut unresolved_nodes,
                            in_flight: &mut in_flight,
                            unresolvable_nodes: &mut unresolvable_nodes,
                        };
                        context.attach(canonical, priority, back_edges);
                    }
                    continue;
                }

                in_flight_order.push_back(unresolved_description.clone());
                in_flight.insert(unresolved_description.clone(), (priority, back_edges));
                work_queue.push(NodeTask {
                    description: unresolved_description,
                });
            }

            // Process completed nodes in dispatch order (to keep resolution deterministic).
            while let Some(front) = in_flight_order.front().cloned() {
                let Some(result) = completed.remove(&front) else {
                    break;
                };
                in_flight_order.pop_front();

                let Some((_priority, back_nodes)) = in_flight.remove(&result.description) else {
                    continue;
                };

                for action in result.before {
                    match action {
                        CapturedAction::Diagnostic(diagnostic) => {
                            handler.on_resolution_diagnostic(diagnostic);
                        }
                        CapturedAction::Event(event) => {
                            handler.on_resolution_event(event);
                        }
                    }
                }

                match result.resolution {
                    Ok(resource) => {
                        let handler_item = handler.handle_resolution(&result.description, resource);

                        for action in result.after {
                            match action {
                                CapturedAction::Diagnostic(diagnostic) => {
                                    handler.on_resolution_diagnostic(diagnostic);
                                }
                                CapturedAction::Event(event) => {
                                    handler.on_resolution_event(event);
                                }
                            }
                        }

                        match handler_item.into_outcome(&result.description) {
                            Ok(outcome) => {
                                let canonical_description = canonicalize_description(
                                    &outcome.canonical_description,
                                    &mut aliases,
                                );
                                if canonical_description != outcome.canonical_description {
                                    aliases.insert(
                                        outcome.canonical_description,
                                        canonical_description.clone(),
                                    );
                                }
                                if canonical_description != result.description {
                                    aliases.insert(
                                        result.description.clone(),
                                        canonical_description.clone(),
                                    );
                                }
                                for alias in outcome.aliases {
                                    aliases.insert(alias, canonical_description.clone());
                                }

                                let resolved_node_index =
                                    if let Some(&existing) = nodes.get(&canonical_description) {
                                        existing
                                    } else {
                                        let index = graph.add_node(canonical_description.clone());
                                        nodes.insert(canonical_description.clone(), index);
                                        index
                                    };

                                for (back_node_index, back_edge) in back_nodes {
                                    graph.add_edge(back_node_index, resolved_node_index, back_edge);
                                }

                                for UnresolvedNode {
                                    priority,
                                    description: forward_node_description,
                                    edge: forward_edge,
                                } in outcome.forward_nodes
                                {
                                    let forward_node_description = canonicalize_description(
                                        &forward_node_description,
                                        &mut aliases,
                                    );
                                    {
                                        let mut context = AsyncBackEdgeContext {
                                            graph: &mut graph,
                                            nodes: &nodes,
                                            unresolved_nodes: &mut unresolved_nodes,
                                            in_flight: &mut in_flight,
                                            unresolvable_nodes: &mut unresolvable_nodes,
                                        };
                                        context.attach(
                                            forward_node_description,
                                            priority,
                                            vec![(resolved_node_index, forward_edge)],
                                        );
                                    }
                                }
                            }
                            Err(error) => {
                                handler.on_resolution_error(&result.description, error);
                                unresolvable_nodes
                                    .entry(result.description)
                                    .or_default()
                                    .extend(back_nodes);
                            }
                        }
                    }
                    Err(error) => {
                        for action in result.after {
                            match action {
                                CapturedAction::Diagnostic(diagnostic) => {
                                    handler.on_resolution_diagnostic(diagnostic);
                                }
                                CapturedAction::Event(event) => {
                                    handler.on_resolution_event(event);
                                }
                            }
                        }
                        handler.on_resolution_error(&result.description, error);
                        unresolvable_nodes
                            .entry(result.description)
                            .or_default()
                            .extend(back_nodes);
                    }
                }
            }

            if in_flight.is_empty() {
                continue;
            }

            match result_rx.recv() {
                Ok(result) => {
                    completed.insert(result.description.clone(), result);
                }
                Err(_) => break,
            }
        }

        work_queue.close();
        for handle in worker_handles {
            let _ = handle.join();
        }

        if graph.node_count() == 0 {
            tracing::warn!(target: "resolver", "Async graph resolution failed: root node is unresolvable");
            Err(UnresolvableRootNode)
        } else {
            tracing::info!(
                target: "resolver",
                "Async graph resolution completed successfully with {} nodes",
                graph.node_count()
            );
            let root_node = canonicalize_description(root_node, &mut aliases);
            let result = handler.handle_graph_resolution(&root_node, graph);
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
) -> Option<(D, P, BackEdges<E>)>
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
            .map(|(description, (priority, back_nodes))| (description, priority, back_nodes))
    })
}

fn canonicalize_description<D>(description: &D, aliases: &mut HashMap<D, D>) -> D
where
    D: Eq + std::hash::Hash + Clone,
{
    let mut current = description.clone();
    let mut chain = Vec::new();
    while let Some(next) = aliases.get(&current).cloned() {
        if next == current {
            break;
        }
        chain.push(current);
        current = next;
    }
    for link in chain {
        aliases.insert(link, current.clone());
    }
    current
}

fn attach_back_edges<D, E, P>(
    graph: &mut DiGraph<D, E>,
    nodes: &IndexMap<D, NodeIndex>,
    unresolved_nodes: &mut UnresolvedMap<D, P, E>,
    unresolvable_nodes: &mut IndexMap<D, BackEdges<E>>,
    description: D,
    priority: P,
    back_edges: BackEdges<E>,
) where
    D: Eq + std::hash::Hash + Clone,
    E: Clone,
    P: Ord + Clone,
{
    if let Some(&existing_index) = nodes.get(&description) {
        for (back_node_index, back_edge) in back_edges {
            graph.add_edge(back_node_index, existing_index, back_edge);
        }
        return;
    }

    if unresolvable_nodes.contains_key(&description) {
        unresolvable_nodes
            .entry(description)
            .or_default()
            .extend(back_edges);
        return;
    }

    if let Some((existing_priority, existing_back_edges)) = unresolved_nodes.get_mut(&description) {
        if priority > *existing_priority {
            *existing_priority = priority;
        }
        existing_back_edges.extend(back_edges);
    } else {
        unresolved_nodes.insert(description, (priority, back_edges));
    }
}

struct AsyncBackEdgeContext<'a, D, E, P> {
    graph: &'a mut DiGraph<D, E>,
    nodes: &'a IndexMap<D, NodeIndex>,
    unresolved_nodes: &'a mut UnresolvedMap<D, P, E>,
    in_flight: &'a mut HashMap<D, (P, BackEdges<E>)>,
    unresolvable_nodes: &'a mut IndexMap<D, BackEdges<E>>,
}

impl<'a, D, E, P> AsyncBackEdgeContext<'a, D, E, P>
where
    D: Eq + std::hash::Hash + Clone,
    E: Clone,
    P: Ord + Clone,
{
    fn attach(&mut self, description: D, priority: P, back_edges: BackEdges<E>) {
        if let Some(&existing_index) = self.nodes.get(&description) {
            for (back_node_index, back_edge) in back_edges {
                self.graph
                    .add_edge(back_node_index, existing_index, back_edge);
            }
            return;
        }

        if let Some((existing_priority, existing_back_edges)) = self.in_flight.get_mut(&description)
        {
            if priority > *existing_priority {
                *existing_priority = priority;
            }
            existing_back_edges.extend(back_edges);
            return;
        }

        if self.unresolvable_nodes.contains_key(&description) {
            self.unresolvable_nodes
                .entry(description)
                .or_default()
                .extend(back_edges);
            return;
        }

        if let Some((existing_priority, existing_back_edges)) =
            self.unresolved_nodes.get_mut(&description)
        {
            if priority > *existing_priority {
                *existing_priority = priority;
            }
            existing_back_edges.extend(back_edges);
        } else {
            self.unresolved_nodes
                .insert(description, (priority, back_edges));
        }
    }
}
