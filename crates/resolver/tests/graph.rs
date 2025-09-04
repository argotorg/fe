use core::panic;
use std::{collections::HashMap, hash::Hash, marker::PhantomData};

use dir_test::{Fixture, dir_test};
use fe_resolver::{
    ResolutionHandler, Resolver,
    graph::{GraphResolutionHandler, GraphResolver, GraphResolverImpl},
};
use test_utils::snap_test;

struct FixtureEntryResolver<K, V>(pub Fixture<HashMap<K, V>>);

impl<K, V> FixtureEntryResolver<K, V> {}

#[derive(Debug)]
struct EntryDoesNotExist;

impl<K: Eq + Hash, V: Clone> Resolver for FixtureEntryResolver<K, V> {
    type Description = K;
    type Resource = V;
    type Error = EntryDoesNotExist;
    type Diagnostic = ();

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        description: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: ResolutionHandler<Self>,
    {
        if let Some(value) = self.0.content().get(description) {
            Ok(handler.handle_resolution(description, value.clone()))
        } else {
            Err(EntryDoesNotExist)
        }
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        panic!()
    }
}

#[derive(Default)]
pub struct MockNodeHandler;

impl ResolutionHandler<FixtureEntryResolver<String, Vec<String>>> for MockNodeHandler {
    type Item = Vec<(String, ())>;

    fn handle_resolution(&mut self, _source: &String, targets: Vec<String>) -> Self::Item {
        targets.into_iter().map(|target| (target, ())).collect()
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

type FixtureEntryGraphResolver<K, V> =
    GraphResolverImpl<FixtureEntryResolver<K, V>, MockNodeHandler, ()>;

fn fixture_resolver<K, V>(fixture: Fixture<HashMap<K, V>>) -> FixtureEntryGraphResolver<K, V>
where
    K: Eq + Hash + Clone,
    V: Eq + Hash + Clone,
{
    GraphResolverImpl {
        node_resolver: FixtureEntryResolver(fixture),
        _handler: PhantomData,
        diagnostics: vec![],
        _edge: PhantomData,
    }
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/fixtures/graph",
    glob: "*.toml",
    loader: load_toml,
)]
fn graph_resolution(fixture: Fixture<HashMap<String, Vec<String>>>) {
    let fixture_path = fixture.path();
    let mut resolver = fixture_resolver(fixture);
    let mut handler = MockNodeHandler;
    let graph = resolver
        .graph_resolve(&mut handler, &"a".to_string())
        .unwrap();
    snap_test!(
        format!("{:#?}\n{:#?}", graph, resolver.take_diagnostics()),
        fixture_path
    );
}
