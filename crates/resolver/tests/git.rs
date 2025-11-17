use std::fs;

use camino::Utf8PathBuf;
use fe_resolver::{
    ResolutionHandler, Resolver,
    git::{GitDependencyDescription, GitResolver, GitResource},
    graph::{
        DiGraph, GraphResolutionHandler, GraphResolver, GraphResolverImpl, petgraph::visit::EdgeRef,
    },
};
use git2::{IndexAddOption, Repository, Signature};
use tempfile::TempDir;
use toml::Value;
use url::Url;

struct GitGraphHandler;

impl ResolutionHandler<GitResolver> for GitGraphHandler {
    type Item = Vec<(GitDependencyDescription, ())>;

    fn handle_resolution(
        &mut self,
        _description: &GitDependencyDescription,
        resource: GitResource,
    ) -> Self::Item {
        let config_path = resource.ingot_path.join("fe.toml");
        let config = fs::read_to_string(config_path.as_std_path()).expect("failed to read fe.toml");
        let parsed: Value = config.parse().expect("invalid fe.toml");

        parsed
            .get("dependencies")
            .and_then(|value| value.as_table())
            .map(|table| {
                table
                    .values()
                    .filter_map(|value| parse_dependency(value))
                    .map(|desc| (desc, ()))
                    .collect()
            })
            .unwrap_or_default()
    }
}

impl GraphResolutionHandler<GitDependencyDescription, DiGraph<GitDependencyDescription, ()>>
    for GitGraphHandler
{
    type Item = DiGraph<GitDependencyDescription, ()>;

    fn handle_graph_resolution(
        &mut self,
        _description: &GitDependencyDescription,
        graph: DiGraph<GitDependencyDescription, ()>,
    ) -> Self::Item {
        graph
    }
}

fn parse_dependency(value: &Value) -> Option<GitDependencyDescription> {
    let table = value.as_table()?;
    let source = table.get("source")?.as_str()?;
    let rev = table.get("rev")?.as_str()?;
    let mut description = GitDependencyDescription::new(Url::parse(source).ok()?, rev.to_string());
    if let Some(path) = table
        .get("path")
        .and_then(|value| value.as_str())
        .map(Utf8PathBuf::from)
    {
        description.path = Some(path);
    }
    Some(description)
}

#[derive(Clone)]
struct RepoInfo {
    url: Url,
    rev: String,
}

impl RepoInfo {
    fn description(&self) -> GitDependencyDescription {
        GitDependencyDescription::new(self.url.clone(), self.rev.clone())
    }
}

#[test]
fn graph_resolution_with_git_repositories() {
    let tmp_dir = TempDir::new().expect("failed to create temp dir");
    let workspace =
        Utf8PathBuf::from_path_buf(tmp_dir.path().to_path_buf()).expect("non UTF-8 temp dir");

    let leaf = create_repo(&workspace.join("leaf"), "leaf", &[]);
    let mid = create_repo(&workspace.join("mid"), "mid", &[("leaf", &leaf)]);
    let root = create_repo(
        &workspace.join("root"),
        "root",
        &[("leaf", &leaf), ("mid", &mid)],
    );

    let checkout_root = workspace.join("checkouts");
    let git_resolver = GitResolver::new(checkout_root);
    let mut graph_resolver: GraphResolverImpl<GitResolver, GitGraphHandler, ()> =
        GraphResolverImpl::new(git_resolver);

    let mut handler = GitGraphHandler;
    let graph = graph_resolver
        .graph_resolve(&mut handler, &root.description())
        .expect("graph resolution should succeed");

    assert_eq!(graph.node_count(), 3, "expected three ingots in graph");

    assert!(
        graph_resolver.take_diagnostics().is_empty(),
        "graph resolver reported unexpected diagnostics"
    );
    assert!(
        graph_resolver.node_resolver.take_diagnostics().is_empty(),
        "git resolver diagnostics should be empty"
    );

    let mut edges = graph
        .edge_references()
        .map(|edge| {
            (
                graph[edge.source()].source.as_str().to_string(),
                graph[edge.target()].source.as_str().to_string(),
            )
        })
        .collect::<Vec<_>>();
    edges.sort();

    let mut expected = vec![
        (root.url.as_str().to_string(), leaf.url.as_str().to_string()),
        (root.url.as_str().to_string(), mid.url.as_str().to_string()),
        (mid.url.as_str().to_string(), leaf.url.as_str().to_string()),
    ];
    expected.sort();
    assert_eq!(edges, expected);
}

fn create_repo(root: &Utf8PathBuf, name: &str, dependencies: &[(&str, &RepoInfo)]) -> RepoInfo {
    if root.exists() {
        fs::remove_dir_all(root.as_std_path()).unwrap();
    }
    fs::create_dir_all(root.join("src").as_std_path()).unwrap();

    fs::write(
        root.join("src/lib.fe").as_std_path(),
        format!("pub fn {name}_value() -> u256 {{ 1 }}"),
    )
    .unwrap();

    let mut config = format!("[ingot]\nname = \"{name}\"\nversion = \"1.0.0\"\n\n",);
    if !dependencies.is_empty() {
        config.push_str("[dependencies]\n");
        for (alias, repo) in dependencies {
            config.push_str(&format!(
                "{alias} = {{ source = \"{}\", rev = \"{}\" }}\n",
                repo.url, repo.rev
            ));
        }
    }
    fs::write(root.join("fe.toml").as_std_path(), config).unwrap();

    let repo = Repository::init(root.as_std_path()).unwrap();
    let mut index = repo.index().unwrap();
    index
        .add_all(["*"], IndexAddOption::DEFAULT, None)
        .expect("failed to add files");
    index.write().unwrap();
    let tree_id = index.write_tree().unwrap();
    let tree = repo.find_tree(tree_id).unwrap();
    let signature = Signature::now("Fe Tests", "tests@fe-lang.org").unwrap();
    let commit = repo
        .commit(Some("HEAD"), &signature, &signature, "initial", &tree, &[])
        .expect("failed to create commit");

    RepoInfo {
        url: Url::from_directory_path(root.as_std_path()).expect("invalid repo url"),
        rev: commit.to_string(),
    }
}
