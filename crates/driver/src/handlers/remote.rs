use std::collections::HashMap;

use camino::{Utf8Path, Utf8PathBuf};
use common::{
    InputDb,
    dependencies::{DependencyArguments, DependencyLocation, RemoteFiles},
};
use resolver::{
    ResolutionHandler, Resolver,
    files::{FilesResolver, FilesResource},
    git::{GitDependencyDescription, GitResolver, GitResource},
    graph::{DiGraph, GraphResolutionHandler, petgraph::visit::EdgeRef},
};
use smol_str::SmolStr;
use url::Url;

use crate::{IngotInitDiagnostics, ingot_files_resolver};

#[derive(Clone)]
struct RemoteFilesContext {
    description: GitDependencyDescription,
    checkout_path: Utf8PathBuf,
}

pub struct RemoteIngotHandler<'a> {
    pub db: &'a mut dyn InputDb,
    ingot_urls: HashMap<GitDependencyDescription, Url>,
    pub diagnostics: Vec<IngotInitDiagnostics>,
    current_context: Option<RemoteFilesContext>,
}

impl<'a> RemoteIngotHandler<'a> {
    pub fn new(db: &'a mut dyn InputDb) -> Self {
        Self {
            db,
            ingot_urls: HashMap::new(),
            diagnostics: Vec::new(),
            current_context: None,
        }
    }

    pub fn ingot_url(&self, description: &GitDependencyDescription) -> Option<&Url> {
        self.ingot_urls.get(description)
    }

    pub fn take_diagnostics(&mut self) -> Vec<IngotInitDiagnostics> {
        std::mem::take(&mut self.diagnostics)
    }

    fn touch_files(&mut self, resource: FilesResource) -> Option<()> {
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

    fn register_remote_mapping(&mut self, ingot_url: &Url, description: &GitDependencyDescription) {
        let remote = RemoteFiles {
            source: description.source.clone(),
            rev: SmolStr::new(description.rev.clone()),
            path: description.path.clone(),
        };
        self.db
            .workspace()
            .register_remote_git(self.db, ingot_url.clone(), remote);
    }

    fn build_dependency_descriptions(
        &mut self,
        ingot_url: &Url,
        context: &RemoteFilesContext,
        dependency: DependencyLocation,
        alias: SmolStr,
        arguments: DependencyArguments,
    ) -> Option<(GitDependencyDescription, (SmolStr, DependencyArguments))> {
        match dependency {
            DependencyLocation::Local(local) => {
                match relative_path_within_checkout(context.checkout_path.as_path(), &local.url) {
                    Ok(relative_path) => {
                        let mut next_description = GitDependencyDescription::new(
                            context.description.source.clone(),
                            context.description.rev.clone(),
                        );
                        if let Some(path) = relative_path {
                            next_description = next_description.with_path(path);
                        }
                        Some((next_description, (alias, arguments)))
                    }
                    Err(error) => {
                        self.diagnostics
                            .push(IngotInitDiagnostics::RemotePathResolutionError {
                                ingot_url: ingot_url.clone(),
                                dependency: alias.clone(),
                                error,
                            });
                        None
                    }
                }
            }
            DependencyLocation::Remote(remote) => {
                let mut next_description =
                    GitDependencyDescription::new(remote.source.clone(), remote.rev.to_string());
                if let Some(path) = remote.path.clone() {
                    next_description = next_description.with_path(path);
                }
                Some((next_description, (alias, arguments)))
            }
        }
    }
}

impl<'a> ResolutionHandler<GitResolver> for RemoteIngotHandler<'a> {
    type Item = Vec<(GitDependencyDescription, (SmolStr, DependencyArguments))>;

    fn handle_resolution(
        &mut self,
        description: &GitDependencyDescription,
        resource: GitResource,
    ) -> Self::Item {
        self.ingot_urls
            .insert(description.clone(), resource.ingot_url.clone());

        let mut files_resolver = ingot_files_resolver();
        self.current_context = Some(RemoteFilesContext {
            description: description.clone(),
            checkout_path: resource.checkout_path.clone(),
        });
        let files_result = files_resolver.resolve(self, &resource.ingot_url);
        self.current_context = None;

        for diagnostic in files_resolver.take_diagnostics() {
            let target_url = diagnostic
                .url()
                .cloned()
                .unwrap_or_else(|| resource.ingot_url.clone());
            self.diagnostics
                .push(IngotInitDiagnostics::RemoteFileError {
                    ingot_url: target_url,
                    error: diagnostic.to_string(),
                });
        }

        match files_result {
            Ok(dependencies) => dependencies,
            Err(error) => {
                self.diagnostics
                    .push(IngotInitDiagnostics::RemoteFileError {
                        ingot_url: resource.ingot_url.clone(),
                        error: error.to_string(),
                    });
                Vec::new()
            }
        }
    }
}

impl<'a> ResolutionHandler<FilesResolver> for RemoteIngotHandler<'a> {
    type Item = Vec<(GitDependencyDescription, (SmolStr, DependencyArguments))>;

    fn handle_resolution(&mut self, ingot_url: &Url, resource: FilesResource) -> Self::Item {
        let context = self
            .current_context
            .as_ref()
            .expect("missing remote resolution context")
            .clone();

        if self.touch_files(resource).is_none() {
            self.diagnostics
                .push(IngotInitDiagnostics::RemoteFileError {
                    ingot_url: ingot_url.clone(),
                    error: "Remote ingot is missing fe.toml".into(),
                });
            return Vec::new();
        }

        let Some(ingot) = self
            .db
            .workspace()
            .containing_ingot(self.db, ingot_url.clone())
        else {
            tracing::error!("Unable to locate remote ingot {}", ingot_url);
            return Vec::new();
        };

        if let Some(error) = ingot.config_parse_error(self.db) {
            self.diagnostics
                .push(IngotInitDiagnostics::RemoteConfigParseError {
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
                .push(IngotInitDiagnostics::RemoteConfigDiagnostics {
                    ingot_url: ingot_url.clone(),
                    diagnostics: config.diagnostics.clone(),
                });
        }

        self.register_remote_mapping(ingot_url, &context.description);

        let mut dependencies = Vec::new();
        for dependency in config.dependencies(ingot_url) {
            if let Some(description) = self.build_dependency_descriptions(
                ingot_url,
                &context,
                dependency.location,
                dependency.alias,
                dependency.arguments,
            ) {
                dependencies.push(description);
            }
        }

        dependencies
    }
}

impl<'a>
    GraphResolutionHandler<
        GitDependencyDescription,
        DiGraph<GitDependencyDescription, (SmolStr, DependencyArguments)>,
    > for RemoteIngotHandler<'a>
{
    type Item = DiGraph<Url, (SmolStr, DependencyArguments)>;

    fn handle_graph_resolution(
        &mut self,
        _description: &GitDependencyDescription,
        graph: DiGraph<GitDependencyDescription, (SmolStr, DependencyArguments)>,
    ) -> Self::Item {
        let mut converted = DiGraph::new();
        let mut node_map = HashMap::new();

        for node_idx in graph.node_indices() {
            if let Some(url) = self.ingot_urls.get(&graph[node_idx]) {
                let new_idx = converted.add_node(url.clone());
                node_map.insert(node_idx, new_idx);
            }
        }

        for edge in graph.edge_references() {
            if let (Some(&from_idx), Some(&to_idx)) =
                (node_map.get(&edge.source()), node_map.get(&edge.target()))
            {
                let weight = edge.weight().clone();
                let from_url = converted[from_idx].clone();
                let to_url = converted[to_idx].clone();
                converted.add_edge(from_idx, to_idx, weight.clone());
                self.db
                    .remote_graph()
                    .add_dependency(self.db, &from_url, &to_url, weight.0, weight.1);
            }
        }

        converted
    }
}

fn relative_path_within_checkout(
    checkout_path: &Utf8Path,
    target_url: &Url,
) -> Result<Option<Utf8PathBuf>, String> {
    let path_buf = target_url
        .to_file_path()
        .map_err(|_| "target URL is not a file URL".to_string())?;
    let utf8_path = Utf8PathBuf::from_path_buf(path_buf)
        .map_err(|_| "non UTF-8 path encountered in remote dependency".to_string())?;
    let relative = utf8_path
        .strip_prefix(checkout_path)
        .map_err(|_| "path escapes the checked-out repository".to_string())?;
    if relative.as_str().is_empty() {
        Ok(None)
    } else {
        Ok(Some(relative.to_owned()))
    }
}
