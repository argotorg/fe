use anyhow::Context;
use common::stdlib::{BUILTIN_CORE_BASE_URL, BUILTIN_STD_BASE_URL, HasBuiltinCore, HasBuiltinStd};
use driver::DriverDataBase;
use rustc_hash::FxHashMap;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use tempfile::NamedTempFile;
use tracing::warn;
use url::Url;

use crate::logging::user_cache_dirs;

/// Files the server writes for the editor to open: the builtin `core` and `std` ingots, and
/// codegen views.
///
/// They live in the user's cache dir, shared by every server process, so a server that exits
/// or is killed leaves nothing behind. Builtins go to
/// `<cache>/builtins/<content hash>/{core,std}`, written once for each distinct content.
/// Codegen views go to `<cache>/codegen/<source hash>/<name>`, replaced by each new view of the
/// same source.
pub struct VirtualFiles {
    cache: PathBuf,
    builtins: PathBuf,
    /// internal URI → client file URI (e.g. builtin-core:/src/lib.fe → file:///…/core/src/lib.fe)
    internal_to_client: FxHashMap<Url, Url>,
    /// client file URI → internal URI
    client_to_internal: FxHashMap<Url, Url>,
}

impl VirtualFiles {
    /// Write the builtin ingots to the first usable dir of [`user_cache_dirs`]. A shared temp dir
    /// is no fallback: another user could create the fixed path first and plant symlinks.
    pub fn new(db: &DriverDataBase) -> Option<Self> {
        user_cache_dirs().find_map(|cache| {
            Self::in_cache(&cache, db)
                .inspect_err(|e| {
                    warn!(
                        "failed to write builtin files to {}: {e:#}",
                        cache.display()
                    )
                })
                .ok()
        })
    }

    fn in_cache(cache: &Path, db: &DriverDataBase) -> anyhow::Result<Self> {
        let mut files = Vec::new();
        for (name, ingot, base) in [
            ("core", db.builtin_core(), BUILTIN_CORE_BASE_URL),
            ("std", db.builtin_std(), BUILTIN_STD_BASE_URL),
        ] {
            let scheme = Url::parse(base)
                .expect("builtin url parse")
                .scheme()
                .to_string();
            for (url, file) in ingot.files(db).iter() {
                let relative = url.path().trim_start_matches('/').to_string();
                if url.scheme() == scheme && !relative.is_empty() {
                    files.push((url, name, relative, file.text(db)));
                }
            }
        }
        files.sort_by(|a, b| a.0.cmp(&b.0));

        let mut hasher = blake3::Hasher::new();
        for (url, _, _, text) in &files {
            for part in [url.as_str(), text.as_str()] {
                hasher.update(&(part.len() as u64).to_le_bytes());
                hasher.update(part.as_bytes());
            }
        }
        let builtins_dir = cache.join("builtins");
        let builtins = builtins_dir.join(&hasher.finalize().to_hex()[..32]);

        if !builtins.exists() {
            fs::create_dir_all(&builtins_dir)
                .with_context(|| format!("create dir {}", builtins_dir.display()))?;
            // Write a staging dir and rename it into place, so no server sees a partial copy.
            // The rename fails if another server published the same files first; then `staging`
            // is removed on drop. After a successful rename its drop does nothing.
            let staging = tempfile::Builder::new()
                .prefix(".staging-")
                .tempdir_in(&builtins_dir)
                .with_context(|| format!("create staging dir in {}", builtins_dir.display()))?;
            for (_, name, relative, text) in &files {
                let path = join_url_path(&staging.path().join(name), relative);
                if let Some(parent) = path.parent() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("create dir {}", parent.display()))?;
                }
                fs::write(&path, text).with_context(|| format!("write file {}", path.display()))?;
                set_readonly(&path)?;
            }
            if let Err(e) = fs::rename(staging.path(), &builtins)
                && !builtins.exists()
            {
                return Err(e).with_context(|| format!("rename to {}", builtins.display()));
            }
        }

        let mut internal_to_client = FxHashMap::default();
        let mut client_to_internal = FxHashMap::default();
        for (url, name, relative, _) in files {
            let path = join_url_path(&builtins.join(name), &relative);
            let client_url =
                Url::from_file_path(&path).map_err(|_| anyhow::anyhow!("bad path url"))?;
            internal_to_client.insert(url.clone(), client_url.clone());
            client_to_internal.insert(client_url, url);
        }

        Ok(Self {
            cache: cache.to_path_buf(),
            builtins,
            internal_to_client,
            client_to_internal,
        })
    }

    /// Write a codegen view of `source` named `file_name`, replacing an earlier view of the same
    /// name, and return its URL.
    pub fn write_codegen(
        &self,
        source: &Url,
        file_name: &str,
        content: &str,
    ) -> anyhow::Result<Url> {
        let dir = self
            .cache
            .join("codegen")
            .join(&blake3::hash(source.as_str().as_bytes()).to_hex()[..16]);
        fs::create_dir_all(&dir).with_context(|| format!("create dir {}", dir.display()))?;
        let path = dir.join(file_name);
        // Write a temp file (mode 0600 on Unix) and rename it over the earlier view, so another
        // server writing a view of the same source at once can't interleave its output with ours.
        let mut file = NamedTempFile::new_in(&dir)
            .with_context(|| format!("create temp file in {}", dir.display()))?;
        file.write_all(content.as_bytes())
            .with_context(|| format!("write temp file in {}", dir.display()))?;
        file.persist(&path)
            .with_context(|| format!("replace {}", path.display()))?;
        Url::from_file_path(&path).map_err(|_| anyhow::anyhow!("bad path url"))
    }

    pub fn map_internal_to_client(&self, uri: Url) -> Url {
        self.internal_to_client.get(&uri).cloned().unwrap_or(uri)
    }

    pub fn map_client_to_internal(&self, uri: Url) -> Url {
        if let Some(mapped) = self.client_to_internal.get(&uri) {
            return mapped.clone();
        }

        // Best-effort fallback: if a client normalized the file URI differently, map by path.
        if uri.scheme() == "file"
            && let Ok(path) = uri.to_file_path()
            && let Ok(rel) = path.strip_prefix(&self.builtins)
        {
            let mut components = rel.components();
            let Some(root_dir) = components.next() else {
                return uri;
            };
            let name = root_dir.as_os_str().to_string_lossy();
            let base = match name.as_ref() {
                "core" => BUILTIN_CORE_BASE_URL,
                "std" => BUILTIN_STD_BASE_URL,
                _ => return uri,
            };

            let mut rel_url_path = String::new();
            for component in components {
                if !rel_url_path.is_empty() {
                    rel_url_path.push('/');
                }
                rel_url_path.push_str(&component.as_os_str().to_string_lossy());
            }

            if let Ok(base) = Url::parse(base)
                && let Ok(builtin) = base.join(&rel_url_path)
            {
                return builtin;
            }
        }

        uri
    }

    /// Whether `uri` is a file the server wrote, including builtins of other Fe versions.
    pub fn is_virtual_uri(&self, uri: &Url) -> bool {
        if self.client_to_internal.contains_key(uri) {
            return true;
        }
        uri.scheme() == "file"
            && uri
                .to_file_path()
                .is_ok_and(|path| path.starts_with(&self.cache))
    }
}

fn join_url_path(base: &Path, url_path: &str) -> PathBuf {
    url_path
        .split('/')
        .filter(|part| !part.is_empty())
        .fold(base.to_path_buf(), |acc, part| acc.join(part))
}

#[cfg(unix)]
fn set_readonly(path: &Path) -> anyhow::Result<()> {
    use std::os::unix::fs::PermissionsExt;
    let mut perms = fs::metadata(path)
        .with_context(|| format!("read metadata {}", path.display()))?
        .permissions();
    perms.set_mode(0o444);
    fs::set_permissions(path, perms)
        .with_context(|| format!("set readonly perms {}", path.display()))?;
    Ok(())
}

#[cfg(not(unix))]
fn set_readonly(path: &Path) -> anyhow::Result<()> {
    let mut perms = fs::metadata(path)
        .with_context(|| format!("read metadata {}", path.display()))?
        .permissions();
    perms.set_readonly(true);
    fs::set_permissions(path, perms)
        .with_context(|| format!("set readonly perms {}", path.display()))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(unix)]
    use std::os::unix::fs::PermissionsExt;
    use std::sync::Barrier;
    use std::thread;

    fn core_lib() -> Url {
        Url::parse(BUILTIN_CORE_BASE_URL)
            .unwrap()
            .join("src/lib.fe")
            .unwrap()
    }

    /// Servers started at once share one copy of the builtins, and no staging dir is left over.
    #[test]
    fn concurrent_servers_share_builtin_files() {
        let cache = tempfile::tempdir().unwrap();
        let barrier = Barrier::new(4);
        let roots: Vec<PathBuf> = thread::scope(|scope| {
            let handles: Vec<_> = (0..4)
                .map(|_| {
                    scope.spawn(|| {
                        let db = DriverDataBase::default();
                        barrier.wait();
                        VirtualFiles::in_cache(cache.path(), &db).unwrap().builtins
                    })
                })
                .collect();
            handles.into_iter().map(|h| h.join().unwrap()).collect()
        });
        assert!(roots.iter().all(|root| *root == roots[0]));

        let entries: Vec<_> = fs::read_dir(cache.path().join("builtins"))
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .collect();
        assert_eq!(entries, [roots[0].clone()]);

        let db = DriverDataBase::default();
        let vfs = VirtualFiles::in_cache(cache.path(), &db).unwrap();
        let client = vfs.map_internal_to_client(core_lib());
        let path = client.to_file_path().unwrap();
        assert_eq!(path, roots[0].join("core").join("src").join("lib.fe"));
        assert!(fs::metadata(&path).unwrap().permissions().readonly());
        assert!(vfs.is_virtual_uri(&client));
        assert_eq!(vfs.map_client_to_internal(client), core_lib());
    }

    /// A codegen view replaces the earlier view of its source without leaving a temp file, but
    /// not views of other sources with the same file name. Only the user can read it.
    #[test]
    fn codegen_views_replace_views_of_the_same_source() {
        let cache = tempfile::tempdir().unwrap();
        let db = DriverDataBase::default();
        let vfs = VirtualFiles::in_cache(cache.path(), &db).unwrap();
        let a = Url::parse("file:///a/src/lib.fe").unwrap();
        let b = Url::parse("file:///b/src/lib.fe").unwrap();

        let first = vfs.write_codegen(&a, "lib.mir", "first").unwrap();
        let second = vfs.write_codegen(&a, "lib.mir", "second").unwrap();
        let other = vfs.write_codegen(&b, "lib.mir", "other").unwrap();
        assert_eq!(first, second);
        assert_ne!(first, other);
        let path = first.to_file_path().unwrap();
        assert_eq!(fs::read_to_string(&path).unwrap(), "second");
        let views: Vec<_> = fs::read_dir(path.parent().unwrap())
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .collect();
        assert_eq!(views, [path.as_path()]);
        #[cfg(unix)]
        assert_eq!(
            fs::metadata(&path).unwrap().permissions().mode() & 0o777,
            0o600
        );
        assert!(vfs.is_virtual_uri(&other));
    }
}
