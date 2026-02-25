use camino::Utf8PathBuf;
use common::config::{Config, WorkspaceMemberSelection, WorkspaceSettings};
use common::ingot::Version;
use common::urlext::UrlExt;
use glob::glob;
use smol_str::SmolStr;
use std::path::{Path, PathBuf};
use url::Url;

use crate::{
    ResolutionHandler, Resolver,
    files::{FilesResolutionDiagnostic, FilesResolutionError, FilesResolver, FilesResource},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExpandedWorkspaceMember {
    pub url: Url,
    pub path: Utf8PathBuf,
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

#[derive(Debug, Default)]
pub struct ContextDiscovery {
    pub workspace_root: Option<Url>,
    pub ingot_roots: Vec<Url>,
    pub standalone_files: Vec<Url>,
    pub diagnostics: Vec<FilesResolutionDiagnostic>,
}

enum DiscoveredConfig {
    Workspace(Box<common::config::WorkspaceConfig>),
    Ingot,
    Invalid,
}

struct ContextProbe;

impl ResolutionHandler<FilesResolver> for ContextProbe {
    type Item = Option<DiscoveredConfig>;

    fn handle_resolution(&mut self, _description: &Url, resource: FilesResource) -> Self::Item {
        let config = resource
            .files
            .iter()
            .find(|file| file.path.as_str().ends_with("fe.toml"))
            .map(|file| file.content.as_str())?;

        match Config::parse(config) {
            Ok(Config::Workspace(config)) => Some(DiscoveredConfig::Workspace(config)),
            Ok(Config::Ingot(_)) => Some(DiscoveredConfig::Ingot),
            Err(_) => Some(DiscoveredConfig::Invalid),
        }
    }
}

pub fn expand_workspace_members(
    workspace: &WorkspaceSettings,
    base_url: &Url,
    selection: WorkspaceMemberSelection,
) -> Result<Vec<ExpandedWorkspaceMember>, String> {
    let base_path_buf = base_url
        .to_file_path()
        .map_err(|_| "workspace URL is not a file URL".to_string())?;
    let base_path = Utf8PathBuf::from_path_buf(base_path_buf)
        .map_err(|_| "workspace path is not UTF-8".to_string())?;
    let base_canonical = base_path
        .as_std_path()
        .canonicalize()
        .map_err(|err| format!("failed to canonicalize workspace root {base_path}: {err}"))?;
    let base_canonical = Utf8PathBuf::from_path_buf(base_canonical)
        .map_err(|_| "workspace path is not UTF-8".to_string())?;

    let mut excluded = std::collections::HashSet::new();
    for pattern in &workspace.exclude {
        let pattern_path = base_path.join(pattern.as_str());
        let entries = glob(pattern_path.as_str())
            .map_err(|err| format!("Invalid exclude pattern \"{pattern}\": {err}"))?;
        for entry in entries {
            let path = entry
                .map_err(|err| format!("Glob error for exclude pattern \"{pattern}\": {err}"))?;
            let canonical = path
                .canonicalize()
                .map_err(|err| format!("Glob error for exclude pattern \"{pattern}\": {err}"))?;
            let canonical = Utf8PathBuf::from_path_buf(canonical)
                .map_err(|_| "exclude path is not UTF-8".to_string())?;
            if !canonical.starts_with(&base_canonical) {
                return Err(format!(
                    "Exclude pattern \"{pattern}\" escapes workspace root {base_path}"
                ));
            }
            excluded.insert(canonical);
        }
    }

    let mut members = Vec::new();
    let mut seen = std::collections::HashSet::new();
    let spec_selection = match selection {
        WorkspaceMemberSelection::DefaultOnly if workspace.default_members.is_some() => {
            WorkspaceMemberSelection::All
        }
        WorkspaceMemberSelection::DefaultOnly => WorkspaceMemberSelection::PrimaryOnly,
        selection => selection,
    };
    for spec in workspace.members_for_selection(spec_selection) {
        let pattern = spec.path.as_str();
        if spec.name.is_some() || spec.version.is_some() {
            if pattern.contains(['*', '?', '[']) {
                return Err(format!(
                    "Member path \"{pattern}\" with name/version cannot contain glob patterns"
                ));
            }
            let path = base_path.join(pattern);
            if !path.is_dir() {
                continue;
            }
            let canonical = path.as_std_path().canonicalize().map_err(|err| {
                format!("failed to canonicalize member path \"{pattern}\": {err}")
            })?;
            let canonical = Utf8PathBuf::from_path_buf(canonical)
                .map_err(|_| "member path is not UTF-8".to_string())?;
            if !canonical.starts_with(&base_canonical) {
                return Err(format!(
                    "Member path \"{pattern}\" escapes workspace root {base_path}"
                ));
            }
            if excluded.contains(&canonical) {
                continue;
            }
            if seen.insert(canonical.clone()) {
                let relative = canonical
                    .strip_prefix(&base_canonical)
                    .map_err(|_| "member path escaped workspace root".to_string())?
                    .to_owned();
                let url = base_url
                    .join_directory(&relative)
                    .map_err(|_| "failed to convert member path to URL".to_string())?;
                members.push(ExpandedWorkspaceMember {
                    url,
                    path: relative,
                    name: spec.name.clone(),
                    version: spec.version.clone(),
                });
            }
            continue;
        }

        let pattern_path = base_path.join(pattern);
        let entries = glob(pattern_path.as_str())
            .map_err(|err| format!("Invalid member pattern \"{pattern}\": {err}"))?;

        for entry in entries {
            let path = entry
                .map_err(|err| format!("Glob error for member pattern \"{pattern}\": {err}"))?;
            let canonical = path
                .canonicalize()
                .map_err(|err| format!("Glob error for member pattern \"{pattern}\": {err}"))?;
            let canonical = Utf8PathBuf::from_path_buf(canonical)
                .map_err(|_| "member path is not UTF-8".to_string())?;
            if !canonical.starts_with(&base_canonical) {
                return Err(format!(
                    "Member pattern \"{pattern}\" escapes workspace root {base_path}"
                ));
            }
            if !canonical.is_dir() {
                continue;
            }
            if excluded.contains(&canonical) {
                continue;
            }
            if seen.insert(canonical.clone()) {
                let relative = canonical
                    .strip_prefix(&base_canonical)
                    .map_err(|_| "member path escaped workspace root".to_string())?;
                let url = base_url
                    .join_directory(&relative.to_owned())
                    .map_err(|_| "failed to convert member path to URL".to_string())?;
                members.push(ExpandedWorkspaceMember {
                    url,
                    path: relative.to_owned(),
                    name: None,
                    version: None,
                });
            }
        }
    }

    if matches!(selection, WorkspaceMemberSelection::DefaultOnly)
        && let Some(default_members) = &workspace.default_members
    {
        let defaults: std::collections::HashSet<&str> =
            default_members.iter().map(|m| m.as_str()).collect();
        members.retain(|member| defaults.contains(member.path.as_str()));
    }

    Ok(members)
}

pub fn discover_context(url: &Url) -> Result<ContextDiscovery, FilesResolutionError> {
    let path = url
        .to_file_path()
        .map_err(|_| FilesResolutionError::DirectoryDoesNotExist(url.clone()))?;
    let mut discovery = ContextDiscovery::default();
    let mut first_ingot_root: Option<Url> = None;

    let is_file = path.is_file();
    let start_dir = if is_file {
        path.parent().map(PathBuf::from).unwrap_or(path.clone())
    } else {
        path.clone()
    };

    if !start_dir.exists() {
        return Err(FilesResolutionError::DirectoryDoesNotExist(url.clone()));
    }

    let mut current = Some(start_dir.as_path());
    let mut resolver = FilesResolver::new().with_required_file("fe.toml");
    while let Some(dir) = current {
        let dir_url = Url::from_directory_path(dir)
            .map_err(|_| FilesResolutionError::DirectoryDoesNotExist(url.clone()))?;
        let mut handler = ContextProbe;
        let probe = resolver.resolve(&mut handler, &dir_url)?;
        match probe {
            Some(DiscoveredConfig::Workspace(config)) => {
                discovery.workspace_root = Some(dir_url.clone());
                if let Ok(members) = expand_workspace_members(
                    &config.workspace,
                    &dir_url,
                    WorkspaceMemberSelection::All,
                ) {
                    for member in members {
                        if !discovery.ingot_roots.contains(&member.url) {
                            discovery.ingot_roots.push(member.url);
                        }
                    }
                }
                if let Some(ingot_root) = first_ingot_root
                    && ingot_root != dir_url
                    && !discovery.ingot_roots.contains(&ingot_root)
                {
                    discovery.ingot_roots.push(ingot_root);
                }
                return Ok(discovery);
            }
            Some(DiscoveredConfig::Ingot) | Some(DiscoveredConfig::Invalid) => {
                if first_ingot_root.is_none() {
                    first_ingot_root = Some(dir_url);
                }
            }
            None => {}
        }

        current = dir.parent();
    }

    if let Some(ingot_root) = first_ingot_root {
        discovery.ingot_roots.push(ingot_root);
        return Ok(discovery);
    }

    if is_file {
        if let Some(root) = ancestor_root_for_src(&path) {
            let ingot_url = Url::from_directory_path(&root)
                .map_err(|_| FilesResolutionError::DirectoryDoesNotExist(url.clone()))?;
            discovery
                .diagnostics
                .push(FilesResolutionDiagnostic::RequiredFileMissing(
                    ingot_url.clone(),
                    "fe.toml".to_string(),
                ));
            discovery.ingot_roots.push(ingot_url);
            return Ok(discovery);
        }

        if path.extension().and_then(|ext| ext.to_str()) == Some("fe")
            && let Ok(file_url) = Url::from_file_path(&path)
        {
            discovery.standalone_files.push(file_url);
        }

        return Ok(discovery);
    }

    // Upward walk found nothing and this is a directory without fe.toml.
    // Scan downward for fe.toml files to discover nested workspaces/ingots
    // (e.g. editor opened at a parent directory like a monorepo or workbook).
    discover_nested_configs(&start_dir, url, &mut resolver, &mut discovery)?;

    Ok(discovery)
}

/// Recursively scan subdirectories for `fe.toml` files to discover nested
/// workspaces and ingots. Used when the editor root is a parent directory
/// that doesn't itself contain a Fe project (e.g. a monorepo or workbook).
fn discover_nested_configs(
    root: &Path,
    root_url: &Url,
    resolver: &mut FilesResolver,
    discovery: &mut ContextDiscovery,
) -> Result<(), FilesResolutionError> {
    const MAX_DEPTH: usize = 8;
    const SKIP_DIRS: &[&str] = &[
        ".git",
        ".hg",
        "node_modules",
        "target",
        "out",
        ".claude",
        "__pycache__",
    ];

    fn walk(
        dir: &Path,
        depth: usize,
        resolver: &mut FilesResolver,
        discovery: &mut ContextDiscovery,
        root_url: &Url,
    ) -> Result<(), FilesResolutionError> {
        if depth > MAX_DEPTH {
            return Ok(());
        }

        let dir_url = match Url::from_directory_path(dir) {
            Ok(u) => u,
            Err(_) => return Ok(()),
        };

        let mut handler = ContextProbe;
        let probe = resolver.resolve(&mut handler, &dir_url)?;
        match probe {
            Some(DiscoveredConfig::Workspace(config)) => {
                // Use the first discovered workspace as the primary root;
                // additional workspaces are still loaded via ingot_roots.
                if discovery.workspace_root.is_none() {
                    discovery.workspace_root = Some(dir_url.clone());
                }
                // Always add the workspace root itself so init_ingot processes it
                if !discovery.ingot_roots.contains(&dir_url) {
                    discovery.ingot_roots.push(dir_url.clone());
                }
                if let Ok(members) = expand_workspace_members(
                    &config.workspace,
                    &dir_url,
                    WorkspaceMemberSelection::All,
                ) {
                    for member in members {
                        if !discovery.ingot_roots.contains(&member.url) {
                            discovery.ingot_roots.push(member.url);
                        }
                    }
                }
                // Don't recurse into a workspace — its members are already expanded
                return Ok(());
            }
            Some(DiscoveredConfig::Ingot) => {
                if !discovery.ingot_roots.contains(&dir_url) {
                    discovery.ingot_roots.push(dir_url);
                }
                // Don't recurse into an ingot
                return Ok(());
            }
            Some(DiscoveredConfig::Invalid) | None => {}
        }

        // Recurse into subdirectories (sorted for deterministic discovery order)
        let entries = match std::fs::read_dir(dir) {
            Ok(entries) => entries,
            Err(_) => return Ok(()),
        };
        let mut subdirs: Vec<PathBuf> = entries
            .filter_map(|e| e.ok())
            .filter(|e| e.path().is_dir())
            .filter(|e| {
                let name = e.file_name();
                let name_str = name.to_string_lossy();
                !SKIP_DIRS.contains(&name_str.as_ref()) && !name_str.starts_with('.')
            })
            .map(|e| e.path())
            .collect();
        subdirs.sort();
        for subdir in subdirs {
            walk(&subdir, depth + 1, resolver, discovery, root_url)?;
        }

        Ok(())
    }

    walk(root, 0, resolver, discovery, root_url)
}

fn ancestor_root_for_src(path: &Path) -> Option<PathBuf> {
    let mut current = path.parent();
    while let Some(dir) = current {
        if dir.file_name() == Some(std::ffi::OsStr::new("src")) {
            return dir.parent().map(PathBuf::from);
        }
        current = dir.parent();
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::fs;

    fn parse_workspace_settings(toml: &str) -> WorkspaceSettings {
        let config = Config::parse(toml).expect("workspace config parses");
        let Config::Workspace(workspace_config) = config else {
            panic!("expected workspace config");
        };
        workspace_config.workspace.clone()
    }

    fn write_file(path: &Path, contents: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("create parent dirs");
        }
        fs::write(path, contents).expect("write file");
    }

    #[test]
    fn discovers_workspace_root_above_member_ingot() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let root = tmp.path();

        write_file(
            &root.join("fe.toml"),
            r#"
[workspace]
members = ["ingots/*"]
"#,
        );
        write_file(
            &root.join("ingots/app/fe.toml"),
            r#"
[ingot]
name = "app"
version = "0.1.0"
"#,
        );
        fs::create_dir_all(root.join("ingots/app/src")).expect("create src dir");

        let member_url = Url::from_directory_path(root.join("ingots/app")).expect("member url");
        let discovery = discover_context(&member_url).expect("discover context");

        let root_url = Url::from_directory_path(root).expect("root url");
        assert_eq!(discovery.workspace_root, Some(root_url));
        assert!(discovery.ingot_roots.contains(&member_url));
    }

    #[test]
    fn discovers_workspace_root_even_if_nearest_config_is_invalid() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let root = tmp.path();

        write_file(
            &root.join("fe.toml"),
            r#"
[workspace]
members = ["ingots/*"]
"#,
        );
        write_file(&root.join("ingots/bad/fe.toml"), "not toml at all");
        fs::create_dir_all(root.join("ingots/bad/src")).expect("create src dir");

        let bad_url = Url::from_directory_path(root.join("ingots/bad")).expect("bad url");
        let discovery = discover_context(&bad_url).expect("discover context");

        let root_url = Url::from_directory_path(root).expect("root url");
        assert_eq!(discovery.workspace_root, Some(root_url));
    }

    #[test]
    fn expand_members_applies_exclude_patterns() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let root = tmp.path();
        fs::create_dir_all(root.join("ingots/app")).expect("create app dir");
        fs::create_dir_all(root.join("ingots/bad")).expect("create bad dir");

        let workspace = parse_workspace_settings(
            r#"
[workspace]
members = ["ingots/*"]
exclude = ["ingots/bad"]
"#,
        );
        let root_url = Url::from_directory_path(root).expect("root url");
        let members =
            expand_workspace_members(&workspace, &root_url, WorkspaceMemberSelection::All)
                .expect("expand members");

        assert_eq!(members.len(), 1);
        assert!(members[0].url.as_str().ends_with("/ingots/app/"));
    }

    #[test]
    fn expand_members_deduplicates_overlapping_specs() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let root = tmp.path();
        fs::create_dir_all(root.join("ingots/app")).expect("create app dir");

        let workspace = parse_workspace_settings(
            r#"
[workspace]
members = ["ingots/*", "ingots/app"]
"#,
        );
        let root_url = Url::from_directory_path(root).expect("root url");
        let members =
            expand_workspace_members(&workspace, &root_url, WorkspaceMemberSelection::All)
                .expect("expand members");

        assert_eq!(members.len(), 1);
        assert!(members[0].url.as_str().ends_with("/ingots/app/"));
    }

    #[test]
    fn expand_members_default_only_filters_after_glob_expansion() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let root = tmp.path();
        fs::create_dir_all(root.join("ingots/app")).expect("create app dir");
        fs::create_dir_all(root.join("ingots/lib")).expect("create lib dir");

        let workspace = parse_workspace_settings(
            r#"
[workspace]
members = ["ingots/*"]
default-members = ["ingots/app"]
"#,
        );
        let root_url = Url::from_directory_path(root).expect("root url");
        let members =
            expand_workspace_members(&workspace, &root_url, WorkspaceMemberSelection::DefaultOnly)
                .expect("expand members");

        assert_eq!(members.len(), 1);
        assert_eq!(members[0].path.as_str(), "ingots/app");
    }

    #[test]
    fn expand_members_rejects_glob_when_name_is_specified() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let root_url = Url::from_directory_path(tmp.path()).expect("root url");

        let workspace = parse_workspace_settings(
            r#"
[workspace]
members = [{ path = "ingots/*", name = "app" }]
"#,
        );

        let err = expand_workspace_members(&workspace, &root_url, WorkspaceMemberSelection::All)
            .expect_err("expected error");
        assert!(err.contains("cannot contain glob patterns"));
    }

    #[test]
    fn expand_members_rejects_member_pattern_escaping_root() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let root = tmp.path();
        let outside = tempfile::tempdir_in(root.parent().expect("temp root has parent"))
            .expect("outside tempdir");
        let outside_name = outside
            .path()
            .file_name()
            .expect("outside name")
            .to_string_lossy()
            .to_string();

        let workspace = parse_workspace_settings(&format!(
            r#"
[workspace]
members = ["../{outside_name}"]
"#
        ));
        let root_url = Url::from_directory_path(root).expect("root url");
        let err = expand_workspace_members(&workspace, &root_url, WorkspaceMemberSelection::All)
            .expect_err("expected error");
        assert!(err.contains("escapes workspace root"));
    }

    #[test]
    fn expand_members_rejects_exclude_pattern_escaping_root() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let root = tmp.path();
        let outside = tempfile::tempdir_in(root.parent().expect("temp root has parent"))
            .expect("outside tempdir");
        let outside_name = outside
            .path()
            .file_name()
            .expect("outside name")
            .to_string_lossy()
            .to_string();

        let workspace = parse_workspace_settings(&format!(
            r#"
[workspace]
members = ["ingots/*"]
exclude = ["../{outside_name}"]
"#
        ));
        let root_url = Url::from_directory_path(root).expect("root url");
        let err = expand_workspace_members(&workspace, &root_url, WorkspaceMemberSelection::All)
            .expect_err("expected error");
        assert!(err.contains("escapes workspace root"));
    }

    #[test]
    fn discovers_nested_workspace_from_parent_directory() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let root = tmp.path();

        // Create a nested workspace: root/lessons/lesson1/fe.toml
        write_file(
            &root.join("lessons/lesson1/fe.toml"),
            r#"
[workspace]
members = ["counter", "counter_user"]
"#,
        );
        fs::create_dir_all(root.join("lessons/lesson1/counter/src")).expect("create counter");
        write_file(
            &root.join("lessons/lesson1/counter/fe.toml"),
            r#"
[ingot]
name = "counter"
version = "0.1.0"
"#,
        );
        fs::create_dir_all(root.join("lessons/lesson1/counter_user/src"))
            .expect("create counter_user");
        write_file(
            &root.join("lessons/lesson1/counter_user/fe.toml"),
            r#"
[ingot]
name = "counter_user"
version = "0.1.0"
"#,
        );

        // Discover from the parent root (no fe.toml here)
        let root_url = Url::from_directory_path(root).expect("root url");
        let discovery = discover_context(&root_url).expect("discover context");

        let ws_url = Url::from_directory_path(root.join("lessons/lesson1")).expect("workspace url");
        assert_eq!(discovery.workspace_root, Some(ws_url.clone()));

        let counter_url =
            Url::from_directory_path(root.join("lessons/lesson1/counter")).expect("counter url");
        let counter_user_url = Url::from_directory_path(root.join("lessons/lesson1/counter_user"))
            .expect("counter_user url");
        assert!(
            discovery.ingot_roots.contains(&ws_url),
            "workspace root should be in ingot_roots"
        );
        assert!(
            discovery.ingot_roots.contains(&counter_url),
            "counter should be in ingot_roots"
        );
        assert!(
            discovery.ingot_roots.contains(&counter_user_url),
            "counter_user should be in ingot_roots"
        );
    }

    #[test]
    fn discovers_standalone_ingot_from_parent_directory() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let root = tmp.path();

        // Create a nested ingot without a workspace
        fs::create_dir_all(root.join("projects/mylib/src")).expect("create mylib");
        write_file(
            &root.join("projects/mylib/fe.toml"),
            r#"
[ingot]
name = "mylib"
version = "0.1.0"
"#,
        );

        let root_url = Url::from_directory_path(root).expect("root url");
        let discovery = discover_context(&root_url).expect("discover context");

        let mylib_url = Url::from_directory_path(root.join("projects/mylib")).expect("mylib url");
        assert!(
            discovery.ingot_roots.contains(&mylib_url),
            "standalone ingot should be discovered"
        );
        assert!(
            discovery.workspace_root.is_none(),
            "no workspace root expected"
        );
    }

    #[test]
    fn discover_context_skips_noise_directories() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let root = tmp.path();

        // Put fe.toml in target/ and node_modules/ — should be skipped
        write_file(
            &root.join("target/fe.toml"),
            "[ingot]\nname = \"bad\"\nversion = \"0.1.0\"",
        );
        write_file(
            &root.join("node_modules/fe.toml"),
            "[ingot]\nname = \"bad2\"\nversion = \"0.1.0\"",
        );
        write_file(
            &root.join(".hidden/fe.toml"),
            "[ingot]\nname = \"bad3\"\nversion = \"0.1.0\"",
        );

        // Real project
        fs::create_dir_all(root.join("mylib/src")).expect("create mylib");
        write_file(
            &root.join("mylib/fe.toml"),
            "[ingot]\nname = \"mylib\"\nversion = \"0.1.0\"",
        );

        let root_url = Url::from_directory_path(root).expect("root url");
        let discovery = discover_context(&root_url).expect("discover context");

        assert_eq!(discovery.ingot_roots.len(), 1, "only mylib should be found");
        let mylib_url = Url::from_directory_path(root.join("mylib")).expect("mylib url");
        assert!(discovery.ingot_roots.contains(&mylib_url));
    }

    #[test]
    fn discover_context_multiple_workspaces_under_parent() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let root = tmp.path();

        // Two sibling workspaces under the root
        write_file(
            &root.join("ws_a/fe.toml"),
            "[workspace]\nmembers = [\"app\"]",
        );
        fs::create_dir_all(root.join("ws_a/app")).expect("create ws_a/app");
        write_file(
            &root.join("ws_b/fe.toml"),
            "[workspace]\nmembers = [\"lib\"]",
        );
        fs::create_dir_all(root.join("ws_b/lib")).expect("create ws_b/lib");

        let root_url = Url::from_directory_path(root).expect("root url");
        let discovery = discover_context(&root_url).expect("discover context");

        // First workspace alphabetically should be primary
        let ws_a_url = Url::from_directory_path(root.join("ws_a")).expect("ws_a url");
        assert_eq!(
            discovery.workspace_root,
            Some(ws_a_url.clone()),
            "first workspace alphabetically should be primary"
        );

        // Both workspaces and their members should be in ingot_roots
        let ws_b_url = Url::from_directory_path(root.join("ws_b")).expect("ws_b url");
        let app_url = Url::from_directory_path(root.join("ws_a/app")).expect("app url");
        let lib_url = Url::from_directory_path(root.join("ws_b/lib")).expect("lib url");
        assert!(discovery.ingot_roots.contains(&ws_a_url));
        assert!(discovery.ingot_roots.contains(&app_url));
        assert!(discovery.ingot_roots.contains(&ws_b_url));
        assert!(discovery.ingot_roots.contains(&lib_url));
    }

    #[test]
    fn discover_context_upward_takes_precedence_over_downward() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let root = tmp.path();

        // Workspace at root level
        write_file(&root.join("fe.toml"), "[workspace]\nmembers = [\"app\"]");
        fs::create_dir_all(root.join("app")).expect("create app");

        // Nested workspace that should NOT be discovered (upward walk finds root first)
        write_file(
            &root.join("nested/fe.toml"),
            "[workspace]\nmembers = [\"other\"]",
        );
        fs::create_dir_all(root.join("nested/other")).expect("create nested/other");

        let root_url = Url::from_directory_path(root).expect("root url");
        let discovery = discover_context(&root_url).expect("discover context");

        // Upward walk finds workspace at root — downward scan should NOT run
        assert_eq!(discovery.workspace_root, Some(root_url));
        let nested_url = Url::from_directory_path(root.join("nested")).expect("nested url");
        assert!(
            !discovery.ingot_roots.contains(&nested_url),
            "nested workspace should not be discovered when upward walk succeeds"
        );
    }
}
