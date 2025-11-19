use common::{
    InputDb,
    dependencies::{DependencyArguments, RemoteDependencyRequest},
};
use resolver::{
    ResolutionHandler,
    files::{FilesResolver, FilesResource},
    graph::{DiGraph, GraphResolutionHandler, petgraph::visit::EdgeRef},
};
use smol_str::SmolStr;
use url::Url;

use super::{DiagnosticKind, ProcessedDependency, process_ingot_resource};
use crate::IngotInitDiagnostics;

pub struct LocalIngotHandler<'a> {
    pub db: &'a mut dyn InputDb,
    pub diagnostics: Vec<IngotInitDiagnostics>,
    pub main_ingot_url: Url,
    pub remote_dependencies: Vec<RemoteDependencyRequest>,
}

impl<'a> LocalIngotHandler<'a> {
    pub fn new(db: &'a mut dyn InputDb, main_ingot_url: Url) -> Self {
        Self {
            db,
            diagnostics: vec![],
            main_ingot_url,
            remote_dependencies: vec![],
        }
    }

    pub fn take_remote_dependencies(&mut self) -> Vec<RemoteDependencyRequest> {
        std::mem::take(&mut self.remote_dependencies)
    }

    fn handle_processed_dependencies(
        &mut self,
        ingot_url: &Url,
        dependencies: Vec<ProcessedDependency>,
    ) -> Vec<(Url, (SmolStr, DependencyArguments))> {
        let mut pending = Vec::new();
        for dependency in dependencies {
            match dependency {
                ProcessedDependency::Local {
                    alias,
                    arguments,
                    url,
                } => {
                    if self.db.graph().contains_url(self.db, &url) {
                        self.db
                            .graph()
                            .add_dependency(self.db, ingot_url, &url, alias, arguments);
                    } else {
                        pending.push((url, (alias, arguments)));
                    }
                }
                ProcessedDependency::Git {
                    alias,
                    arguments,
                    git,
                } => {
                    self.remote_dependencies.push(RemoteDependencyRequest {
                        parent: ingot_url.clone(),
                        alias,
                        arguments,
                        git,
                    });
                }
            }
        }
        pending
    }
}

impl<'a> ResolutionHandler<FilesResolver> for LocalIngotHandler<'a> {
    type Item = Vec<(Url, (SmolStr, DependencyArguments))>;

    fn handle_resolution(&mut self, ingot_url: &Url, resource: FilesResource) -> Self::Item {
        match process_ingot_resource(
            self.db,
            ingot_url,
            resource,
            &mut self.diagnostics,
            DiagnosticKind::Local,
        ) {
            Ok(dependencies) => self.handle_processed_dependencies(ingot_url, dependencies),
            Err(_) => Vec::new(),
        }
    }
}

impl<'a> GraphResolutionHandler<Url, DiGraph<Url, (SmolStr, DependencyArguments)>>
    for LocalIngotHandler<'a>
{
    type Item = ();

    fn handle_graph_resolution(
        &mut self,
        _ingot_url: &Url,
        graph: DiGraph<Url, (SmolStr, DependencyArguments)>,
    ) -> Self::Item {
        let dependency_graph = self.db.graph();

        for edge in graph.edge_references() {
            let from_url = &graph[edge.source()];
            let to_url = &graph[edge.target()];
            let (alias, arguments) = edge.weight();
            dependency_graph.add_dependency(
                self.db,
                from_url,
                to_url,
                alias.clone(),
                arguments.clone(),
            );
        }
    }
}
