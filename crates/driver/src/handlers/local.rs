use common::{
    InputDb,
    dependencies::{DependencyArguments, DependencyLocation, ExternalDependencyEdge},
};
use resolver::{
    ResolutionHandler,
    files::{FilesResolver, FilesResource},
    graph::{DiGraph, GraphResolutionHandler, petgraph::visit::EdgeRef},
};
use smol_str::SmolStr;
use url::Url;

use crate::IngotInitDiagnostics;

pub struct LocalIngotHandler<'a> {
    pub db: &'a mut dyn InputDb,
    pub diagnostics: Vec<IngotInitDiagnostics>,
    pub main_ingot_url: Url,
    pub remote_dependencies: Vec<ExternalDependencyEdge>,
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

    pub fn take_remote_dependencies(&mut self) -> Vec<ExternalDependencyEdge> {
        std::mem::take(&mut self.remote_dependencies)
    }

    fn record_files(&mut self, resource: FilesResource) -> Option<()> {
        let mut has_config = false;
        for file in resource.files {
            let file_url =
                Url::from_file_path(file.path.as_std_path()).expect("resolved path to url");
            if file.path.as_str().ends_with("fe.toml") {
                has_config = true;
                self.db
                    .workspace()
                    .touch(self.db, file_url, Some(file.content.clone()));
            } else {
                self.db
                    .workspace()
                    .touch(self.db, file_url, Some(file.content));
            }
        }
        has_config.then_some(())
    }
}

impl<'a> ResolutionHandler<FilesResolver> for LocalIngotHandler<'a> {
    type Item = Vec<(Url, (SmolStr, DependencyArguments))>;

    fn handle_resolution(&mut self, ingot_url: &Url, resource: FilesResource) -> Self::Item {
        if self.record_files(resource).is_none() {
            return Vec::new();
        }

        let Some(ingot) = self
            .db
            .workspace()
            .containing_ingot(self.db, ingot_url.clone())
        else {
            tracing::error!("Unable to find ingot for {}", ingot_url);
            return Vec::new();
        };

        if let Some(error) = ingot.config_parse_error(self.db) {
            self.diagnostics
                .push(IngotInitDiagnostics::ConfigParseError {
                    ingot_url: ingot_url.clone(),
                    error,
                });
            return Vec::new();
        }

        let Some(config) = ingot.config(self.db) else {
            return Vec::new();
        };

        if !config.diagnostics.is_empty() {
            self.diagnostics
                .push(IngotInitDiagnostics::ConfigDiagnostics {
                    ingot_url: ingot_url.clone(),
                    diagnostics: config.diagnostics.clone(),
                });
        }

        let mut pending = Vec::new();
        for dependency in config.dependencies(ingot_url) {
            match dependency.location {
                DependencyLocation::Local(local) => {
                    if self.db.local_graph().contains_url(self.db, &local.url) {
                        self.db.local_graph().add_dependency(
                            self.db,
                            ingot_url,
                            &local.url,
                            dependency.alias.clone(),
                            dependency.arguments.clone(),
                        );
                    } else {
                        pending.push((
                            local.url,
                            (dependency.alias.clone(), dependency.arguments.clone()),
                        ));
                    }
                }
                DependencyLocation::Remote(remote) => {
                    self.remote_dependencies.push(ExternalDependencyEdge {
                        parent: ingot_url.clone(),
                        alias: dependency.alias.clone(),
                        arguments: dependency.arguments.clone(),
                        remote,
                    });
                }
            }
        }

        pending
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
        let dependency_graph = self.db.local_graph();
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
