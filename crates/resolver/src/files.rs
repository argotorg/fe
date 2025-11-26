use camino::Utf8PathBuf;
use glob::glob;
use std::io;
use std::path::PathBuf;
use std::{fmt, fs};
use url::Url;

use crate::Resolver;

pub struct FilesResolver {
    pub file_patterns: Vec<String>,
    pub required_files: Vec<RequiredFile>,
    pub required_directories: Vec<RequiredDirectory>,
    pub search_mode: bool,
    diagnostics: Vec<FilesResolutionDiagnostic>,
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
            search_mode: false,
            diagnostics: vec![],
        }
    }

    pub fn with_patterns(patterns: &[&str]) -> Self {
        Self {
            file_patterns: patterns.iter().map(|p| p.to_string()).collect(),
            required_files: vec![],
            required_directories: vec![],
            search_mode: false,
            diagnostics: vec![],
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

    pub fn with_search_mode(mut self) -> Self {
        self.search_mode = true;
        self
    }

    /// Check if a directory satisfies all required files and directories
    fn check_requirements(&self, dir: &Utf8PathBuf) -> bool {
        // Check required files
        for required_file in &self.required_files {
            let required_path = dir.join(&required_file.path);
            if !required_path.exists() {
                return false;
            }
        }

        // Check required directories
        for required_dir in &self.required_directories {
            let required_dir_path = dir.join(&required_dir.path);
            if !required_dir_path.exists() || !required_dir_path.is_dir() {
                return false;
            }
        }

        true
    }
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
        let mut files = vec![];

        let mut path = Utf8PathBuf::from(url.path());
        let mut url = url.clone();

        // If search mode is enabled, walk up to find the directory satisfying requirements
        // Search mode is automatically disabled after first use to ensure only the root
        // is searched for, not dependencies
        if self.search_mode {
            tracing::info!(target: "resolver", "Search mode enabled, looking for root from: {}", path);

            // Disable search mode for subsequent resolutions (dependencies)
            self.search_mode = false;

            // If the path is a file, start from its parent directory
            if path.is_file()
                && let Some(parent) = path.parent()
            {
                path = parent.to_path_buf();
            }

            // Walk up directory tree to find root matching requirements
            loop {
                if self.check_requirements(&path) {
                    // Found the root
                    url = Url::from_file_path(&path)
                        .map_err(|_| FilesResolutionError::DirectoryDoesNotExist(url.clone()))?;
                    tracing::info!(target: "resolver", "Found root at: {}", path);
                    break;
                }

                // Move to parent directory
                match path.parent() {
                    Some(parent) => {
                        path = parent.to_path_buf();
                    }
                    None => {
                        // Reached filesystem root without finding matching directory
                        return Err(FilesResolutionError::DirectoryDoesNotExist(url.clone()));
                    }
                }
            }
        }

        tracing::info!(target: "resolver", "Resolving files in path: {}", path);

        // Check if the directory exists
        if !path.exists() || !path.is_dir() {
            return Err(FilesResolutionError::DirectoryDoesNotExist(
                url.clone(),
            ));
        }

        // Check for required directories first
        for required_dir in &self.required_directories {
            let required_dir_path = path.join(&required_dir.path);
            if !required_dir_path.exists() || !required_dir_path.is_dir() {
                self.diagnostics
                    .push(FilesResolutionDiagnostic::RequiredDirectoryMissing(
                        url.clone(),
                        required_dir.path.clone(),
                    ));
            }
        }

        // Check for required files
        for required_file in &self.required_files {
            let required_path = path.join(&required_file.path);
            if !required_path.exists() {
                self.diagnostics
                    .push(FilesResolutionDiagnostic::RequiredFileMissing(
                        url.clone(),
                        required_file.path.clone(),
                    ));
            } else {
                // If required file exists, load it
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
                        self.diagnostics
                            .push(FilesResolutionDiagnostic::FileIoError(required_path, error));
                    }
                }
            }
        }

        // Process file patterns
        for pattern in &self.file_patterns {
            let pattern_path = path.join(pattern);
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
                                            self.diagnostics.push(
                                                FilesResolutionDiagnostic::FileIoError(path, error),
                                            );
                                        }
                                    }
                                }
                                Err(error) => {
                                    self.diagnostics
                                        .push(FilesResolutionDiagnostic::SkippedNonUtf8(error));
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
        Ok(handler.handle_resolution(&url, resource))
    }

    fn take_diagnostics(&mut self) -> Vec<Self::Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }
}
