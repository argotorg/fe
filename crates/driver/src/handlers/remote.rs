use std::collections::HashMap;

use camino::{Utf8Path, Utf8PathBuf};
use common::{
    InputDb,
    dependencies::{DependencyArguments, GitDependency},
};
use indicatif::ProgressBar;
use resolver::{
    ResolutionHandler, Resolver,
    files::{FilesResolver, FilesResource},
    git::{GitDependencyDescription, GitResolver, GitResource},
    graph::{DiGraph, GraphResolutionHandler, petgraph::visit::EdgeRef},
};
use smol_str::SmolStr;
use url::Url;

use super::{DiagnosticKind, ProcessedDependency, ProcessingError, process_ingot_resource};
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
    spinners: HashMap<GitDependencyDescription, ProgressBar>,
    current_context: Option<RemoteFilesContext>,
}

impl<'a> RemoteIngotHandler<'a> {
    pub fn new(db: &'a mut dyn InputDb) -> Self {
        Self {
            db,
            ingot_urls: HashMap::new(),
            diagnostics: Vec::new(),
            spinners: HashMap::new(),
            current_context: None,
        }
    }

    pub fn ingot_url(&self, description: &GitDependencyDescription) -> Option<&Url> {
        self.ingot_urls.get(description)
    }

    pub fn take_diagnostics(&mut self) -> Vec<IngotInitDiagnostics> {
        std::mem::take(&mut self.diagnostics)
    }

    pub fn register_spinner(
        &mut self,
        description: GitDependencyDescription,
        spinner: ProgressBar,
    ) {
        self.spinners.insert(description, spinner);
    }

    pub fn finish_spinner_success(
        &mut self,
        description: &GitDependencyDescription,
        ingot_url: &Url,
    ) {
        if let Some(spinner) = self.spinners.remove(description) {
            spinner.finish_with_message(format!("✅ Resolved {ingot_url}"));
        }
    }

    pub fn fail_spinner(&mut self, description: &GitDependencyDescription, message: &str) {
        if let Some(spinner) = self.spinners.remove(description) {
            spinner.abandon_with_message(format!("❌ Failed to resolve {description}: {message}"));
        }
    }

    fn map_dependency_to_description(
        &mut self,
        ingot_url: &Url,
        context: &RemoteFilesContext,
        dependency: ProcessedDependency,
    ) -> Option<(GitDependencyDescription, (SmolStr, DependencyArguments))> {
        match dependency {
            ProcessedDependency::Local {
                alias,
                arguments,
                url,
            } => match relative_path_within_checkout(context.checkout_path.as_path(), &url) {
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
            },
            ProcessedDependency::Git {
                alias,
                arguments,
                git,
            } => {
                let mut next_description =
                    GitDependencyDescription::new(git.source.clone(), git.rev.to_string());
                if let Some(path) = git.path.clone() {
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
                self.fail_spinner(description, "file resolution failed");
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
            .expect("remote files resolver invoked without context")
            .clone();

        match process_ingot_resource(
            self.db,
            ingot_url,
            resource,
            &mut self.diagnostics,
            DiagnosticKind::Remote,
        ) {
            Ok(dependencies) => {
                self.finish_spinner_success(&context.description, ingot_url);
                let git_dependency = git_dependency_from_description(&context.description);
                self.db
                    .workspace()
                    .register_remote_git(self.db, ingot_url.clone(), git_dependency);
                dependencies
                    .into_iter()
                    .filter_map(|dependency| {
                        self.map_dependency_to_description(ingot_url, &context, dependency)
                    })
                    .collect()
            }
            Err(error) => {
                let message = match error {
                    ProcessingError::MissingConfigFile => "missing fe.toml",
                    ProcessingError::ConfigParseError => "invalid fe.toml",
                    ProcessingError::MissingWorkspaceIngot
                    | ProcessingError::MissingParsedConfig => "failed to process ingot",
                };
                self.fail_spinner(&context.description, message);
                Vec::new()
            }
        }
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

fn git_dependency_from_description(description: &GitDependencyDescription) -> GitDependency {
    GitDependency {
        source: description.source.clone(),
        rev: SmolStr::new(description.rev.clone()),
        path: description.path.clone(),
    }
}
