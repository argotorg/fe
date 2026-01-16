use camino::Utf8PathBuf;
use common::config::{Config, WorkspaceMemberSelection, WorkspaceSettings};
use common::ingot::Version;
use glob::glob;
use smol_str::SmolStr;
use std::path::{Path, PathBuf};
use url::Url;

use crate::files::{FilesResolutionDiagnostic, FilesResolutionError, read_file_text};

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

    let mut excluded = std::collections::HashSet::new();
    for pattern in &workspace.exclude {
        let pattern_path = base_path.join(pattern.as_str());
        let entries = glob(pattern_path.as_str())
            .map_err(|err| format!("Invalid exclude pattern \"{pattern}\": {err}"))?;
        for entry in entries {
            let path = entry
                .map_err(|err| format!("Glob error for exclude pattern \"{pattern}\": {err}"))?;
            if !path.starts_with(&base_path) {
                return Err(format!(
                    "Exclude pattern \"{pattern}\" escapes workspace root {base_path}"
                ));
            }
            if let Ok(path) = Utf8PathBuf::from_path_buf(path) {
                excluded.insert(path);
            }
        }
    }

    let mut members = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for spec in workspace.members_for_selection(selection) {
        let pattern = spec.path.as_str();
        if spec.name.is_some() || spec.version.is_some() {
            if pattern.contains(['*', '?', '[']) {
                return Err(format!(
                    "Member path \"{pattern}\" with name/version cannot contain glob patterns"
                ));
            }
            let path = base_path.join(pattern);
            if !path.starts_with(&base_path) {
                return Err(format!(
                    "Member path \"{pattern}\" escapes workspace root {base_path}"
                ));
            }
            if !path.is_dir() {
                continue;
            }
            if excluded.contains(&path) {
                continue;
            }
            let url = Url::from_directory_path(path.as_std_path())
                .map_err(|_| "failed to convert member path to URL".to_string())?;
            if seen.insert(url.clone()) {
                members.push(ExpandedWorkspaceMember {
                    url,
                    path: Utf8PathBuf::from(pattern),
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
            if !path.starts_with(&base_path) {
                return Err(format!(
                    "Member pattern \"{pattern}\" escapes workspace root {base_path}"
                ));
            }
            if !path.is_dir() {
                continue;
            }
            let utf8_path = Utf8PathBuf::from_path_buf(path)
                .map_err(|_| "member path is not UTF-8".to_string())?;
            if excluded.contains(&utf8_path) {
                continue;
            }
            let url = Url::from_directory_path(utf8_path.as_std_path())
                .map_err(|_| "failed to convert member path to URL".to_string())?;
            if seen.insert(url.clone()) {
                let relative = utf8_path
                    .strip_prefix(&base_path)
                    .map_err(|_| "member path escaped workspace root".to_string())?;
                members.push(ExpandedWorkspaceMember {
                    url,
                    path: relative.to_owned(),
                    name: None,
                    version: None,
                });
            }
        }
    }

    Ok(members)
}

pub fn discover_context(url: &Url) -> Result<ContextDiscovery, FilesResolutionError> {
    let path = url
        .to_file_path()
        .map_err(|_| FilesResolutionError::DirectoryDoesNotExist(url.clone()))?;
    let mut discovery = ContextDiscovery::default();

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
    while let Some(dir) = current {
        let config_path = dir.join("fe.toml");
        if config_path.exists() {
            match read_file_text(&config_path) {
                Ok(content) => match Config::parse(&content) {
                    Ok(Config::Workspace(config)) => {
                        let root_url = Url::from_directory_path(dir).map_err(|_| {
                            FilesResolutionError::DirectoryDoesNotExist(url.clone())
                        })?;
                        discovery.workspace_root = Some(root_url.clone());
                        if let Ok(members) = expand_workspace_members(
                            &config.workspace,
                            &root_url,
                            WorkspaceMemberSelection::All,
                        ) {
                            discovery
                                .ingot_roots
                                .extend(members.into_iter().map(|member| member.url));
                        }
                        return Ok(discovery);
                    }
                    Ok(Config::Ingot(_)) => {
                        let ingot_url = Url::from_directory_path(dir).map_err(|_| {
                            FilesResolutionError::DirectoryDoesNotExist(url.clone())
                        })?;
                        discovery.ingot_roots.push(ingot_url);
                        return Ok(discovery);
                    }
                    Err(_) => {
                        let ingot_url = Url::from_directory_path(dir).map_err(|_| {
                            FilesResolutionError::DirectoryDoesNotExist(url.clone())
                        })?;
                        discovery.ingot_roots.push(ingot_url);
                        return Ok(discovery);
                    }
                },
                Err(error) => return Err(FilesResolutionError::IoError(error)),
            }
        }

        current = dir.parent();
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
    }

    Ok(discovery)
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
