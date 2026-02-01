use std::{
    fmt, fs, io,
    process::{Command, ExitStatus},
};

use camino::{Utf8Path, Utf8PathBuf};
use git2::{Repository, build::CheckoutBuilder};
use sha2::{Digest, Sha256};
use url::Url;

use crate::{ResolutionHandler, Resolver};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GitDescription {
    pub source: Url,
    pub rev: String,
    pub path: Option<Utf8PathBuf>,
}

impl GitDescription {
    pub fn new(source: Url, rev: impl Into<String>) -> Self {
        Self {
            source,
            rev: rev.into(),
            path: None,
        }
    }

    pub fn with_path(mut self, path: impl Into<Utf8PathBuf>) -> Self {
        self.path = Some(path.into());
        self
    }
}

impl fmt::Display for GitDescription {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.path {
            Some(path) => write!(f, "{}#{} ({})", self.source, self.rev, path),
            None => write!(f, "{}#{}", self.source, self.rev),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GitResource {
    pub reused_checkout: bool,
    pub checkout_path: Utf8PathBuf,
}

#[derive(Debug)]
pub struct GitResolver {
    pub checkout_root: Utf8PathBuf,
}

#[derive(Debug, Clone, Copy)]
pub enum SparseCheckoutMode {
    Cone,
    NoCone,
}

impl GitResolver {
    pub fn new(checkout_root: impl Into<Utf8PathBuf>) -> Self {
        Self {
            checkout_root: checkout_root.into(),
        }
    }

    pub fn has_valid_cached_checkout(&self, description: &GitDescription) -> bool {
        let checkout_path = self.checkout_path(description);
        if !checkout_path.exists() {
            return false;
        }
        if let Some(path) = &description.path
            && !checkout_path.join(path).exists()
        {
            return false;
        }
        let repo = match Repository::open(checkout_path.as_std_path()) {
            Ok(repo) => repo,
            Err(_) => return false,
        };
        let oid = match git2::Oid::from_str(&description.rev) {
            Ok(oid) => oid,
            Err(_) => return false,
        };
        repo.find_object(oid, None).is_ok()
    }

    pub fn checkout_path(&self, description: &GitDescription) -> Utf8PathBuf {
        let mut hasher = Sha256::new();
        hasher.update(description.source.as_str().as_bytes());
        hasher.update(b"@");
        hasher.update(description.rev.as_bytes());
        let digest = hasher.finalize();
        let mut encoded = String::with_capacity(digest.len() * 2);
        for byte in digest {
            encoded.push_str(&format!("{byte:02x}"));
        }
        self.checkout_root.join(encoded)
    }

    pub fn ensure_checkout_resource(
        &self,
        description: &GitDescription,
    ) -> Result<GitResource, GitResolutionError> {
        if let Some(path) = &description.path {
            return self.ensure_sparse_checkout(
                description,
                &[path.as_str().to_string()],
                SparseCheckoutMode::Cone,
            );
        }
        self.ensure_checkout_root()?;
        let checkout_path = self.checkout_path(description);
        let status = self.ensure_checkout(description, &checkout_path)?;
        Ok(GitResource {
            reused_checkout: matches!(status, CheckoutStatus::Existing),
            checkout_path,
        })
    }

    fn ensure_checkout_root(&self) -> Result<(), GitResolutionError> {
        if !self.checkout_root.exists() {
            fs::create_dir_all(self.checkout_root.as_std_path()).map_err(|source| {
                GitResolutionError::PrepareCheckoutDirectory {
                    path: self.checkout_root.clone(),
                    source,
                }
            })?;
        }
        Ok(())
    }

    fn ensure_checkout(
        &self,
        description: &GitDescription,
        checkout_path: &Utf8Path,
    ) -> Result<CheckoutStatus, GitResolutionError> {
        if checkout_path.exists() {
            let repo = Repository::open(checkout_path.as_std_path()).map_err(|error| {
                GitResolutionError::OpenRepository {
                    path: checkout_path.to_owned(),
                    error,
                }
            })?;
            self.checkout_revision(&repo, description)?;
            return Ok(CheckoutStatus::Existing);
        }

        if let Some(parent) = checkout_path.parent() {
            fs::create_dir_all(parent.as_std_path()).map_err(|source| {
                GitResolutionError::PrepareCheckoutDirectory {
                    path: parent.to_owned(),
                    source,
                }
            })?;
        }

        let remote = Self::source_for_clone(description)?;
        let repo = Repository::clone(&remote, checkout_path.as_std_path()).map_err(|error| {
            GitResolutionError::CloneRepository {
                source: description.source.clone(),
                error,
            }
        })?;
        self.checkout_revision(&repo, description)?;
        Ok(CheckoutStatus::Fresh)
    }

    fn checkout_revision(
        &self,
        repo: &Repository,
        description: &GitDescription,
    ) -> Result<(), GitResolutionError> {
        let oid = git2::Oid::from_str(&description.rev).map_err(|error| {
            GitResolutionError::InvalidRevision {
                rev: description.rev.clone(),
                error,
            }
        })?;
        let object = match repo.find_object(oid, None) {
            Ok(object) => object,
            Err(error) if description.source.scheme() == "file" => {
                return Err(GitResolutionError::RevisionLookup {
                    rev: description.rev.clone(),
                    error,
                });
            }
            Err(_) => {
                self.fetch_default_remote(repo, description)?;
                repo.find_object(oid, None)
                    .map_err(|error| GitResolutionError::RevisionLookup {
                        rev: description.rev.clone(),
                        error,
                    })?
            }
        };
        let mut builder = CheckoutBuilder::new();
        builder.force();
        repo.checkout_tree(&object, Some(&mut builder))
            .map_err(|error| GitResolutionError::Checkout {
                rev: description.rev.clone(),
                error,
            })?;
        repo.set_head_detached(oid)
            .map_err(|error| GitResolutionError::Checkout {
                rev: description.rev.clone(),
                error,
            })?;
        Ok(())
    }

    fn fetch_default_remote(
        &self,
        repo: &Repository,
        description: &GitDescription,
    ) -> Result<(), GitResolutionError> {
        let mut remote =
            repo.find_remote("origin")
                .map_err(|error| GitResolutionError::FetchRepository {
                    source: description.source.clone(),
                    error,
                })?;
        remote.fetch::<&str>(&[], None, None).map_err(|error| {
            GitResolutionError::FetchRepository {
                source: description.source.clone(),
                error,
            }
        })?;
        Ok(())
    }

    fn source_for_clone(description: &GitDescription) -> Result<String, GitResolutionError> {
        if description.source.scheme() == "file" {
            let path = description.source.to_file_path().map_err(|_| {
                GitResolutionError::SourcePathConversion {
                    source: description.source.clone(),
                }
            })?;
            Ok(path.to_string_lossy().into_owned())
        } else {
            Ok(description.source.as_str().to_owned())
        }
    }

    pub fn ensure_sparse_checkout(
        &self,
        description: &GitDescription,
        patterns: &[String],
        mode: SparseCheckoutMode,
    ) -> Result<GitResource, GitResolutionError> {
        self.ensure_checkout_root()?;
        let checkout_path = self.checkout_path(description);
        let existed = checkout_path.exists();

        if !checkout_path.exists() {
            if let Some(parent) = checkout_path.parent() {
                fs::create_dir_all(parent.as_std_path()).map_err(|source| {
                    GitResolutionError::PrepareCheckoutDirectory {
                        path: parent.to_owned(),
                        source,
                    }
                })?;
            }
            let source = Self::source_for_clone(description)?;
            let output = Command::new("git")
                .arg("clone")
                .arg("--filter=blob:none")
                .arg("--no-checkout")
                .arg(source)
                .arg(checkout_path.as_std_path())
                .output()
                .map_err(|error| GitResolutionError::GitCommand {
                    command: "git clone".to_string(),
                    error,
                })?;
            if !output.status.success() {
                return Err(GitResolutionError::GitCommandFailure {
                    command: "git clone".to_string(),
                    status: output.status,
                    stderr: String::from_utf8_lossy(&output.stderr).to_string(),
                });
            }
        }

        let mut combined_patterns = self.list_sparse_patterns(&checkout_path)?;
        combined_patterns.extend(patterns.iter().cloned());
        combined_patterns = normalize_sparse_patterns(combined_patterns);

        let mode_arg = match mode {
            SparseCheckoutMode::Cone => "--cone",
            SparseCheckoutMode::NoCone => "--no-cone",
        };
        self.run_git(
            &checkout_path,
            &["config", "core.sparseCheckout", "true"],
            &[],
        )?;
        let cone_value = match mode {
            SparseCheckoutMode::Cone => "true",
            SparseCheckoutMode::NoCone => "false",
        };
        self.run_git(
            &checkout_path,
            &["config", "core.sparseCheckoutCone", cone_value],
            &[],
        )?;
        self.run_git_allow_failure(
            &checkout_path,
            &["sparse-checkout", "init", mode_arg],
            &[],
            &["already initialized", "already enabled", "already exists"],
        )?;
        let combined_pattern_args: Vec<&str> = combined_patterns
            .iter()
            .map(|pattern| pattern.as_str())
            .collect();
        self.run_git(
            &checkout_path,
            &["sparse-checkout", "set", mode_arg],
            &combined_pattern_args,
        )?;

        self.ensure_revision_present_sparse(&checkout_path, description)?;
        self.run_git(
            &checkout_path,
            &["checkout", "--detach", &description.rev],
            &[],
        )?;

        Ok(GitResource {
            reused_checkout: existed,
            checkout_path,
        })
    }

    fn run_git(
        &self,
        checkout_path: &Utf8Path,
        args: &[&str],
        extra_args: &[&str],
    ) -> Result<(), GitResolutionError> {
        let output = self.run_git_command(checkout_path, args, extra_args)?;
        if output.status.success() {
            Ok(())
        } else {
            Err(GitResolutionError::GitCommandFailure {
                command: format!("git {}", args.join(" ")),
                status: output.status,
                stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            })
        }
    }

    fn run_git_allow_failure(
        &self,
        checkout_path: &Utf8Path,
        args: &[&str],
        extra_args: &[&str],
        allowed_errors: &[&str],
    ) -> Result<(), GitResolutionError> {
        let output = self.run_git_command(checkout_path, args, extra_args)?;
        if output.status.success() {
            return Ok(());
        }

        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        if allowed_errors
            .iter()
            .any(|allowed| stderr.contains(allowed))
        {
            return Ok(());
        }
        Err(GitResolutionError::GitCommandFailure {
            command: format!("git {}", args.join(" ")),
            status: output.status,
            stderr,
        })
    }

    fn run_git_command(
        &self,
        checkout_path: &Utf8Path,
        args: &[&str],
        extra_args: &[&str],
    ) -> Result<std::process::Output, GitResolutionError> {
        let mut command = Command::new("git");
        command.arg("-C").arg(checkout_path.as_std_path());
        command.args(args);
        command.args(extra_args);
        command
            .output()
            .map_err(|error| GitResolutionError::GitCommand {
                command: format!("git {}", args.join(" ")),
                error,
            })
    }

    fn list_sparse_patterns(
        &self,
        checkout_path: &Utf8Path,
    ) -> Result<Vec<String>, GitResolutionError> {
        let output = self.run_git_command(checkout_path, &["sparse-checkout", "list"], &[])?;
        if !output.status.success() {
            return Ok(Vec::new());
        }
        let stdout = String::from_utf8_lossy(&output.stdout);
        Ok(stdout
            .lines()
            .map(str::trim)
            .filter(|line| !line.is_empty())
            .map(ToOwned::to_owned)
            .collect())
    }

    fn ensure_revision_present_sparse(
        &self,
        checkout_path: &Utf8Path,
        description: &GitDescription,
    ) -> Result<(), GitResolutionError> {
        if self
            .run_git_command(checkout_path, &["cat-file", "-e", &description.rev], &[])
            .is_ok_and(|output| output.status.success())
        {
            return Ok(());
        }

        if self
            .run_git(
                checkout_path,
                &["fetch", "--depth=1", "origin", &description.rev],
                &[],
            )
            .is_ok()
        {
            return Ok(());
        }
        self.run_git(checkout_path, &["fetch", "origin", &description.rev], &[])
    }
}

enum CheckoutStatus {
    Fresh,
    Existing,
}

#[derive(Debug, Clone)]
pub enum GitResolutionEvent {
    CheckoutStart {
        description: GitDescription,
    },
    CheckoutComplete {
        description: GitDescription,
        checkout_path: Utf8PathBuf,
        reused_checkout: bool,
    },
}

impl Resolver for GitResolver {
    type Description = GitDescription;
    type Resource = GitResource;
    type Error = GitResolutionError;
    type Diagnostic = GitResolutionDiagnostic;
    type Event = GitResolutionEvent;

    fn resolve<H>(
        &mut self,
        handler: &mut H,
        description: &Self::Description,
    ) -> Result<H::Item, Self::Error>
    where
        H: ResolutionHandler<Self>,
    {
        handler.on_resolution_event(GitResolutionEvent::CheckoutStart {
            description: description.clone(),
        });
        let resource = self.ensure_checkout_resource(description)?;
        handler.on_resolution_event(GitResolutionEvent::CheckoutComplete {
            description: description.clone(),
            checkout_path: resource.checkout_path.clone(),
            reused_checkout: resource.reused_checkout,
        });
        Ok(handler.handle_resolution(description, resource))
    }
}

#[derive(Debug)]
pub enum GitResolutionError {
    PrepareCheckoutDirectory {
        path: Utf8PathBuf,
        source: io::Error,
    },
    CloneRepository {
        source: Url,
        error: git2::Error,
    },
    OpenRepository {
        path: Utf8PathBuf,
        error: git2::Error,
    },
    InvalidRevision {
        rev: String,
        error: git2::Error,
    },
    RevisionLookup {
        rev: String,
        error: git2::Error,
    },
    Checkout {
        rev: String,
        error: git2::Error,
    },
    GitCommand {
        command: String,
        error: io::Error,
    },
    GitCommandFailure {
        command: String,
        status: ExitStatus,
        stderr: String,
    },
    FetchRepository {
        source: Url,
        error: git2::Error,
    },
    MissingSubdirectory {
        repo_path: Utf8PathBuf,
        subdirectory: Utf8PathBuf,
    },
    SourcePathConversion {
        source: Url,
    },
}

impl fmt::Display for GitResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GitResolutionError::PrepareCheckoutDirectory { path, source } => {
                write!(f, "Failed to prepare checkout directory {}: {source}", path)
            }
            GitResolutionError::CloneRepository { source, error } => {
                write!(f, "Failed to clone repository {source}: {error}")
            }
            GitResolutionError::OpenRepository { path, error } => {
                write!(f, "Failed to open existing checkout at {}: {error}", path)
            }
            GitResolutionError::InvalidRevision { rev, error } => write!(
                f,
                "Revision '{rev}' is not a valid commit identifier: {error}"
            ),
            GitResolutionError::RevisionLookup { rev, error } => {
                write!(
                    f,
                    "Revision '{rev}' was not found in the repository: {error}"
                )
            }
            GitResolutionError::Checkout { rev, error } => {
                write!(f, "Failed to checkout revision '{rev}': {error}")
            }
            GitResolutionError::GitCommand { command, error } => {
                write!(f, "Failed to run {command}: {error}")
            }
            GitResolutionError::GitCommandFailure {
                command,
                status,
                stderr,
            } => {
                write!(f, "Command {command} failed ({status}): {stderr}")
            }
            GitResolutionError::FetchRepository { source, error } => {
                write!(
                    f,
                    "Failed to fetch updates for repository {source}: {error}"
                )
            }
            GitResolutionError::MissingSubdirectory {
                repo_path,
                subdirectory,
            } => write!(
                f,
                "Missing subdirectory '{}' inside checkout '{}'",
                subdirectory, repo_path
            ),
            GitResolutionError::SourcePathConversion { source } => write!(
                f,
                "Failed to convert git source URL '{}' into a local path",
                source
            ),
        }
    }
}

impl std::error::Error for GitResolutionError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            GitResolutionError::PrepareCheckoutDirectory { source, .. } => Some(source),
            GitResolutionError::CloneRepository { error, .. } => Some(error),
            GitResolutionError::OpenRepository { error, .. } => Some(error),
            GitResolutionError::InvalidRevision { error, .. } => Some(error),
            GitResolutionError::RevisionLookup { error, .. } => Some(error),
            GitResolutionError::Checkout { error, .. } => Some(error),
            GitResolutionError::GitCommand { error, .. } => Some(error),
            GitResolutionError::FetchRepository { error, .. } => Some(error),
            GitResolutionError::MissingSubdirectory { .. } => None,
            GitResolutionError::SourcePathConversion { .. } => None,
            GitResolutionError::GitCommandFailure { .. } => None,
        }
    }
}

#[derive(Debug)]
pub enum GitResolutionDiagnostic {}

impl fmt::Display for GitResolutionDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let _ = f;
        unreachable!("GitResolutionDiagnostic has no variants")
    }
}

fn normalize_sparse_patterns(mut patterns: Vec<String>) -> Vec<String> {
    patterns = patterns
        .into_iter()
        .map(|pattern| pattern.trim().to_owned())
        .filter(|pattern| !pattern.is_empty())
        .collect();
    patterns.sort();
    patterns.dedup();
    patterns
}
