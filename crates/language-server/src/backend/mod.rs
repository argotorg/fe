use async_lsp::ClientSocket;
use common::InputDb;
use common::cache::remote_git_cache_dir;
use driver::DriverDataBase;
use rustc_hash::FxHashSet;
use std::path::{Path, PathBuf};
use url::Url;

use crate::builtin_files::BuiltinFiles;

pub struct Backend {
    pub(super) client: ClientSocket,
    pub(super) db: DriverDataBase,
    #[allow(dead_code)] // TODO: salsa3-compatible parallelism
    pub(super) workers: tokio::runtime::Runtime,
    pub(super) builtin_files: Option<BuiltinFiles>,
    pub(super) git_cache_root: Option<PathBuf>,
    pub(super) git_cache_uris: FxHashSet<Url>,
    pub(super) non_git_cache_uris: FxHashSet<Url>,
    pub(super) readonly_warnings: FxHashSet<Url>,
}

impl Backend {
    pub fn new(client: ClientSocket) -> Self {
        let db = DriverDataBase::default();
        let builtin_files = BuiltinFiles::new(&db).ok();
        let git_cache_root = remote_git_cache_dir().map(|path| path.as_std_path().to_path_buf());

        let workers = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(1)
            .enable_all()
            .build()
            .unwrap();
        Self {
            client,
            db,
            workers,
            builtin_files,
            git_cache_root,
            git_cache_uris: FxHashSet::default(),
            non_git_cache_uris: FxHashSet::default(),
            readonly_warnings: FxHashSet::default(),
        }
    }

    pub fn map_internal_uri_to_client(&self, uri: Url) -> Url {
        if let Some(builtins) = self.builtin_files.as_ref() {
            return builtins.map_internal_to_client(uri);
        }
        uri
    }

    pub fn map_client_uri_to_internal(&self, uri: Url) -> Url {
        if let Some(builtins) = self.builtin_files.as_ref() {
            return builtins.map_client_to_internal(uri);
        }
        uri
    }

    pub fn is_builtin_tmp_uri(&self, uri: &Url) -> bool {
        self.builtin_files
            .as_ref()
            .is_some_and(|builtins| builtins.is_tmp_uri(uri))
    }

    pub fn is_git_cache_uri(&self, uri: &Url) -> bool {
        if self.git_cache_uris.contains(uri) {
            return true;
        }
        if self.non_git_cache_uris.contains(uri) {
            return false;
        }
        if uri.scheme() != "file" {
            return false;
        }
        let Ok(path) = uri.to_file_path() else {
            return false;
        };
        self.is_git_cache_path(&path) || self.is_remote_dependency_uri(uri)
    }

    pub fn is_git_cache_path(&self, path: &Path) -> bool {
        if let Some(root) = self.git_cache_root.as_deref() {
            if path.starts_with(root) {
                return true;
            }

            // If the cache dir (or file path) is accessed via a symlinked alias (e.g. `/tmp`
            // vs `/private/tmp`), a simple `starts_with` check can fail. Best-effort
            // canonicalization keeps the resolver + editor paths aligned.
            if let Ok(canonical_root) = std::fs::canonicalize(root)
                && let Ok(canonical_path) = std::fs::canonicalize(path)
                && canonical_path.starts_with(&canonical_root)
            {
                return true;
            }
        }
        is_local_git_cache_path(path)
    }

    pub fn classify_git_cache_path(&mut self, uri: &Url, path: &Path) -> bool {
        if self.git_cache_uris.contains(uri) {
            return true;
        }
        if self.non_git_cache_uris.contains(uri) {
            return false;
        }

        let is_git_cache = self.is_git_cache_path(path) || self.is_remote_dependency_uri(uri);
        if is_git_cache {
            self.git_cache_uris.insert(uri.clone());
        } else {
            self.non_git_cache_uris.insert(uri.clone());
        }
        is_git_cache
    }

    fn is_remote_dependency_uri(&self, uri: &Url) -> bool {
        let Some(ingot) = self.db.workspace().containing_ingot(&self.db, uri.clone()) else {
            return false;
        };
        let base = ingot.base(&self.db);
        self.db
            .dependency_graph()
            .remote_git_for_local(&self.db, &base)
            .is_some()
    }
}

fn is_local_git_cache_path(path: &Path) -> bool {
    let mut components = path.components().peekable();
    while let Some(component) = components.next() {
        if component.as_os_str() == ".fe"
            && components
                .peek()
                .is_some_and(|next| next.as_os_str() == "git")
        {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detects_local_git_cache_dir() {
        let path = PathBuf::from("workspace")
            .join(".fe")
            .join("git")
            .join("checkout")
            .join("src")
            .join("lib.fe");
        assert!(is_local_git_cache_path(&path));
    }

    #[test]
    fn does_not_match_non_git_dirs() {
        let path = PathBuf::from("workspace")
            .join(".fe")
            .join("not-git")
            .join("checkout")
            .join("src")
            .join("lib.fe");
        assert!(!is_local_git_cache_path(&path));
    }
}
