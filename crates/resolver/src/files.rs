use camino::{Utf8Path, Utf8PathBuf};
use glob::glob;
use std::io;
use std::path::PathBuf;
use std::{fmt, fs};
use url::Url;

use crate::Resolver;

pub const FE_TOML: &str = "fe.toml";

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
    pub ingot_url: Url,
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

/// Walks up from the provided path to find the ingot root (directory containing fe.toml).
fn find_ingot_root(start: &Utf8Path) -> Option<Utf8PathBuf> {
    let mut path = if start.is_file() {
        start.parent()?.to_path_buf()
    } else {
        start.to_path_buf()
    };

    loop {
        if path.join(FE_TOML).is_file() {
            return Some(path);
        }

        if !path.pop() {
            break;
        }
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

        let requested_path = Utf8PathBuf::from(url.path());
        let (ingot_path, ingot_url) = if requested_path.is_file() {
            let ingot_path = find_ingot_root(&requested_path)
                .ok_or_else(|| FilesResolutionError::DirectoryDoesNotExist(url.clone()))?;
            let ingot_url = Url::from_directory_path(ingot_path.as_std_path())
                .expect("ingot path should be representable as URL");
            (ingot_path, ingot_url)
        } else if requested_path.is_dir() {
            let ingot_url = Url::from_directory_path(requested_path.as_std_path())
                .expect("ingot path should be representable as URL");
            (requested_path, ingot_url)
        } else {
            return Err(FilesResolutionError::DirectoryDoesNotExist(url.clone()));
        };

        handler.on_resolution_start(&ingot_url);
        let mut files = vec![];

        tracing::info!(target: "resolver", "Resolving files in path: {}", ingot_path);

        // Check if the directory exists
        if !ingot_path.exists() || !ingot_path.is_dir() {
            return Err(FilesResolutionError::DirectoryDoesNotExist(
                ingot_url.clone(),
            ));
        }

        // Check for required directories first
        for required_dir in &self.required_directories {
            let required_dir_path = ingot_path.join(&required_dir.path);
            if !required_dir_path.exists() || !required_dir_path.is_dir() {
                handler.on_resolution_diagnostic(
                    FilesResolutionDiagnostic::RequiredDirectoryMissing(
                        ingot_url.clone(),
                        required_dir.path.clone(),
                    ),
                );
            }
        }

        // Check for required files
        for required_file in &self.required_files {
            let required_path = ingot_path.join(&required_file.path);
            if !required_path.exists() {
                handler.on_resolution_diagnostic(FilesResolutionDiagnostic::RequiredFileMissing(
                    ingot_url.clone(),
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
                        handler.on_resolution_diagnostic(FilesResolutionDiagnostic::FileIoError(
                            required_path,
                            error,
                        ));
                    }
                }
            }
        }

        // Process file patterns
        for pattern in &self.file_patterns {
            let pattern_path = ingot_path.join(pattern);
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
        let resource = FilesResource {
            ingot_url: ingot_url.clone(),
            files,
        };
        Ok(handler.handle_resolution(&ingot_url, resource))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ResolutionHandler;
    use std::fs;
    use tempfile::tempdir;

    struct TestHandler;

    impl ResolutionHandler<FilesResolver> for TestHandler {
        type Item = FilesResource;

        fn on_resolution_start(&mut self, _description: &Url) {}

        fn on_resolution_diagnostic(&mut self, _diagnostic: FilesResolutionDiagnostic) {}

        fn handle_resolution(&mut self, _description: &Url, resource: FilesResource) -> Self::Item {
            resource
        }
    }

    #[test]
    fn resolves_from_file_path_within_ingot() {
        let temp = tempdir().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        let fe_toml = root.join(FE_TOML);
        fs::write(fe_toml.as_std_path(), r#"name = "demo""#).unwrap();

        let src_dir = root.join("src");
        fs::create_dir_all(src_dir.as_std_path()).unwrap();
        let lib_path = src_dir.join("lib.fe");
        fs::write(lib_path.as_std_path(), "contract Foo {}\n").unwrap();
        let nested_dir = src_dir.join("nested");
        fs::create_dir_all(nested_dir.as_std_path()).unwrap();
        let nested_file = nested_dir.join("bar.fe");
        fs::write(nested_file.as_std_path(), "contract Bar {}\n").unwrap();

        let mut resolver = FilesResolver::new()
            .with_required_directory("src")
            .with_required_file("src/lib.fe")
            .with_pattern("src/**/*.fe");

        let mut handler = TestHandler;
        let url = Url::from_file_path(nested_file.as_std_path()).unwrap();
        let resource = resolver.resolve(&mut handler, &url).unwrap();

        assert_eq!(resource.files.len(), 2);
        assert!(resource.files.iter().any(|f| f.path == lib_path));
        assert!(resource.files.iter().any(|f| f.path == nested_file));
    }
}
