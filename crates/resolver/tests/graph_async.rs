use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    marker::PhantomData,
    sync::Arc,
};

use dir_test::{Fixture, dir_test};
use fe_resolver::{
    ResolutionHandler, Resolver,
    graph::{
        AsyncGraphResolverImpl, GraphResolutionHandler, GraphResolver, GraphResolverImpl,
        UnresolvedNode,
    },
};

#[derive(Clone)]
struct MapEntryResolver<K, V>(Arc<HashMap<K, V>>);

#[derive(Debug)]
struct EntryDoesNotExist;

impl<K: Eq + Hash, V: Clone> Resolver for MapEntryResolver<K, V> {
    type Description = K;
    type Resource = V;
    type Error = EntryDoesNotExist;
    type Diagnostic = ();
    type Event = ();

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        description: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: ResolutionHandler<Self>,
    {
        if let Some(value) = self.0.get(description) {
            Ok(handler.handle_resolution(description, value.clone()))
        } else {
            Err(EntryDoesNotExist)
        }
    }
}

#[derive(Default)]
pub struct MockNodeHandler {
    diagnostics: Vec<fe_resolver::graph::UnresolvableNode<String, EntryDoesNotExist>>,
}

impl ResolutionHandler<MapEntryResolver<String, Vec<String>>> for MockNodeHandler {
    type Item = Vec<UnresolvedNode<usize, String, ()>>;

    fn on_resolution_error(&mut self, description: &String, error: EntryDoesNotExist) {
        self.diagnostics.push(fe_resolver::graph::UnresolvableNode(
            description.clone(),
            error,
        ));
    }

    fn handle_resolution(&mut self, _source: &String, targets: Vec<String>) -> Self::Item {
        targets
            .into_iter()
            .enumerate()
            .map(|(priority, target)| UnresolvedNode {
                priority,
                description: target,
                edge: (),
            })
            .collect()
    }
}

impl GraphResolutionHandler<String, petgraph::Graph<String, ()>> for MockNodeHandler {
    type Item = petgraph::Graph<String, ()>;

    fn handle_graph_resolution(
        &mut self,
        _source: &String,
        graph: petgraph::Graph<String, ()>,
    ) -> Self::Item {
        graph
    }
}

fn load_toml(path: &str) -> HashMap<String, Vec<String>> {
    let text = std::fs::read_to_string(path).expect("Failed to read TOML file");
    toml::from_str(&text).expect("Invalid TOML")
}

type MapEntryGraphResolver<K, V> =
    GraphResolverImpl<MapEntryResolver<K, V>, MockNodeHandler, (), usize>;
type AsyncMapEntryGraphResolver<K, V> =
    AsyncGraphResolverImpl<MapEntryResolver<K, V>, MockNodeHandler, (), usize>;

fn fixture_resolver<K, V>(map: Arc<HashMap<K, V>>) -> MapEntryGraphResolver<K, V>
where
    K: Eq + Hash + Clone,
    V: Eq + Hash + Clone,
{
    GraphResolverImpl {
        node_resolver: MapEntryResolver(map),
        _handler: PhantomData,
        _edge: PhantomData,
        _priority: PhantomData,
    }
}

fn fixture_async_resolver<K, V>(map: Arc<HashMap<K, V>>) -> AsyncMapEntryGraphResolver<K, V>
where
    K: Eq + Hash + Clone + Send + Sync + 'static,
    V: Eq + Hash + Clone + Send + Sync + 'static,
{
    AsyncGraphResolverImpl::new(move || MapEntryResolver(map.clone())).with_max_concurrency(4)
}

fn node_set(graph: &petgraph::Graph<String, ()>) -> HashSet<String> {
    graph.node_indices().map(|idx| graph[idx].clone()).collect()
}

fn edge_set(graph: &petgraph::Graph<String, ()>) -> HashSet<(String, String)> {
    graph
        .edge_indices()
        .map(|idx| graph.edge_endpoints(idx).expect("edge endpoints"))
        .map(|(a, b)| (graph[a].clone(), graph[b].clone()))
        .collect()
}

fn diagnostic_set(handler: &MockNodeHandler) -> HashSet<String> {
    handler
        .diagnostics
        .iter()
        .map(|diag| diag.0.clone())
        .collect()
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/graph",
    glob: "*.toml",
    loader: load_toml,
)]
fn async_graph_resolution_matches_sync(fixture: Fixture<HashMap<String, Vec<String>>>) {
    let map = Arc::new(fixture.content().clone());

    let mut sync_resolver = fixture_resolver(map.clone());
    let mut sync_handler = MockNodeHandler::default();
    let sync_graph = sync_resolver
        .graph_resolve(&mut sync_handler, &"a".to_string())
        .unwrap();

    let mut async_resolver = fixture_async_resolver(map);
    let mut async_handler = MockNodeHandler::default();
    let async_graph = async_resolver
        .graph_resolve(&mut async_handler, &"a".to_string())
        .unwrap();

    assert_eq!(node_set(&async_graph), node_set(&sync_graph));
    assert_eq!(edge_set(&async_graph), edge_set(&sync_graph));
    assert_eq!(
        diagnostic_set(&async_handler),
        diagnostic_set(&sync_handler)
    );
}
