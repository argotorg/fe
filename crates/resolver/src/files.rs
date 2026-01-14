use camino::Utf8PathBuf;
use common::config::Config;
use common::config::{WorkspaceMemberSelection, WorkspaceSettings};
use common::ingot::Version;
use glob::glob;
use smol_str::SmolStr;
use std::io;
use std::path::{Path, PathBuf};
use std::{fmt, fs};
use url::Url;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExpandedWorkspaceMember {
    pub url: Url,
    pub path: Utf8PathBuf,
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

use crate::Resolver;

#[derive(Clone)]
pub struct FilesResolver {
    pub file_patterns: Vec<String>,
    pub required_files: Vec<RequiredFile>,
    pub required_directories: Vec<RequiredDirectory>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RequiredFile {
    pub path: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RequiredDirectory {
    pub path: String,
}

#[derive(Debug)]
pub struct File {
    pub path: Utf8PathBuf,
    pub content: String,
}

#[derive(Debug)]
pub struct FilesResource {
    pub files: Vec<File>,
}

#[derive(Debug, Default)]
pub struct ContextDiscovery {
    pub workspace_root: Option<Url>,
    pub ingot_roots: Vec<Url>,
    pub standalone_files: Vec<Url>,
    pub diagnostics: Vec<FilesResolutionDiagnostic>,
}

#[derive(Debug)]
pub enum FilesResolutionError {
    DirectoryDoesNotExist(Url),
    GlobError(glob::GlobError),
    PatternError(glob::PatternError),
    IoError(io::Error),
}

#[derive(Debug)]
pub enum FilesResolutionDiagnostic {
    SkippedNonUtf8(PathBuf),
    FileIoError(Utf8PathBuf, io::Error),
    RequiredFileMissing(Url, String),
    RequiredDirectoryMissing(Url, String),
}

impl fmt::Display for FilesResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FilesResolutionError::DirectoryDoesNotExist(url) => {
                write!(f, "Directory does not exist: {url}")
            }
            FilesResolutionError::GlobError(err) => {
                write!(f, "Glob pattern error: {err}")
            }
            FilesResolutionError::PatternError(err) => {
                write!(f, "Pattern error: {err}")
            }
            FilesResolutionError::IoError(err) => {
                write!(f, "IO error: {err}")
            }
        }
    }
}

impl std::error::Error for FilesResolutionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            FilesResolutionError::GlobError(err) => Some(err),
            FilesResolutionError::PatternError(err) => Some(err),
            FilesResolutionError::IoError(err) => Some(err),
            _ => None,
        }
    }
}

impl fmt::Display for FilesResolutionDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FilesResolutionDiagnostic::SkippedNonUtf8(path) => {
                write!(f, "Skipped non-UTF8 file: {}", path.display())
            }
            FilesResolutionDiagnostic::FileIoError(path, err) => {
                write!(f, "IO error reading file {path}: {err}")
            }
            FilesResolutionDiagnostic::RequiredFileMissing(url, path) => {
                write!(f, "Missing required file '{path}' in ingot at {url}")
            }
            FilesResolutionDiagnostic::RequiredDirectoryMissing(url, path) => {
                write!(f, "Missing required directory '{path}' in ingot at {url}")
            }
        }
    }
}

impl FilesResolutionDiagnostic {
    pub fn url(&self) -> Option<&Url> {
        match self {
            FilesResolutionDiagnostic::SkippedNonUtf8(_) => None,
            FilesResolutionDiagnostic::FileIoError(_, _) => None,
            FilesResolutionDiagnostic::RequiredFileMissing(url, _) => Some(url),
            FilesResolutionDiagnostic::RequiredDirectoryMissing(url, _) => Some(url),
        }
    }
}

impl Default for FilesResolver {
    fn default() -> Self {
        Self::new()
    }
}

impl FilesResolver {
    pub fn new() -> Self {
        Self {
            file_patterns: vec![],
            required_files: vec![],
            required_directories: vec![],
        }
    }

    pub fn with_patterns(patterns: &[&str]) -> Self {
        Self {
            file_patterns: patterns.iter().map(|p| p.to_string()).collect(),
            required_files: vec![],
            required_directories: vec![],
        }
    }

    pub fn with_required_file(mut self, path: &str) -> Self {
        self.required_files.push(RequiredFile {
            path: path.to_string(),
        });
        self
    }

    pub fn with_required_directory(mut self, path: &str) -> Self {
        self.required_directories.push(RequiredDirectory {
            path: path.to_string(),
        });
        self
    }

    pub fn with_pattern(mut self, pattern: &str) -> Self {
        self.file_patterns.push(pattern.to_string());
        self
    }
}

pub fn read_file_text(path: &Path) -> Result<String, io::Error> {
    fs::read_to_string(path)
}

pub fn path_exists(path: &Path) -> bool {
    fs::metadata(path).is_ok()
}

pub fn find_fe_toml_paths(root: &Path) -> Result<Vec<PathBuf>, FilesResolutionError> {
    let pattern = format!("{}/**/fe.toml", root.to_string_lossy());
    let entries = glob(&pattern).map_err(FilesResolutionError::PatternError)?;
    let mut paths = Vec::new();
    for entry in entries {
        match entry {
            Ok(path) => paths.push(path),
            Err(error) => return Err(FilesResolutionError::GlobError(error)),
        }
    }
    Ok(paths)
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

impl Resolver for FilesResolver {
    type Description = Url;
    type Resource = FilesResource;
    type Error = FilesResolutionError;
    type Diagnostic = FilesResolutionDiagnostic;

    fn resolve<H>(&mut self, handler: &mut H, url: &Url) -> Result<H::Item, Self::Error>
    where
        H: crate::ResolutionHandler<Self>,
    {
        tracing::info!(target: "resolver", "Starting file resolution for URL: {}", url);
        handler.on_resolution_start(url);
        let mut files = vec![];

        let ingot_path = Utf8PathBuf::from(url.path());
        tracing::info!(target: "resolver", "Resolving files in path: {}", ingot_path);

        // Check if the directory exists
        if !ingot_path.exists() || !ingot_path.is_dir() {
            return Err(FilesResolutionError::DirectoryDoesNotExist(url.clone()));
        }

        for required_dir in self.required_directories.clone() {
            let required_dir_path = ingot_path.join(&required_dir.path);
            if !required_dir_path.exists() || !required_dir_path.is_dir() {
                handler.on_resolution_diagnostic(
                    FilesResolutionDiagnostic::RequiredDirectoryMissing(
                        url.clone(),
                        required_dir.path.clone(),
                    ),
                );
            }
        }

        for required_file in self.required_files.clone() {
            let required_path = ingot_path.join(&required_file.path);
            if !required_path.exists() {
                handler.on_resolution_diagnostic(FilesResolutionDiagnostic::RequiredFileMissing(
                    url.clone(),
                    required_file.path.clone(),
                ));
                continue;
            }

            match fs::read_to_string(&required_path) {
                Ok(content) => {
                    tracing::info!(target: "resolver", "Successfully read required file: {}", required_path);
                    files.push(File {
                        path: required_path,
                        content,
                    });
                }
                Err(error) => {
                    tracing::warn!(target: "resolver", "Failed to read required file {}: {}", required_path, error);
                    handler.on_resolution_diagnostic(FilesResolutionDiagnostic::FileIoError(
                        required_path,
                        error,
                    ));
                }
            }
        }

        for pattern in self.file_patterns.clone() {
            let pattern_path = ingot_path.join(&pattern);
            let entries =
                glob(pattern_path.as_str()).map_err(FilesResolutionError::PatternError)?;

            for entry in entries {
                match entry {
                    Ok(path) => {
                        if path.is_file() {
                            match Utf8PathBuf::from_path_buf(path) {
                                Ok(path) => {
                                    // Skip if this file was already loaded as a required file
                                    if files.iter().any(|f| f.path == path) {
                                        continue;
                                    }

                                    match fs::read_to_string(&path) {
                                        Ok(content) => {
                                            tracing::info!(target: "resolver", "Successfully read file: {}", path);
                                            files.push(File { path, content });
                                        }
                                        Err(error) => {
                                            tracing::warn!(target: "resolver", "Failed to read file {}: {}", path, error);
                                            handler.on_resolution_diagnostic(
                                                FilesResolutionDiagnostic::FileIoError(path, error),
                                            );
                                        }
                                    }
                                }
                                Err(error) => {
                                    handler.on_resolution_diagnostic(
                                        FilesResolutionDiagnostic::SkippedNonUtf8(error),
                                    );
                                }
                            }
                        }
                    }
                    Err(e) => return Err(FilesResolutionError::GlobError(e)),
                }
            }
        }

        tracing::info!(target: "resolver", "File resolution completed successfully, found {} files", files.len());
        let resource = FilesResource { files };
        Ok(handler.handle_resolution(url, resource))
    }
}
