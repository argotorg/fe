use anyhow::Context;
use common::stdlib::{BUILTIN_CORE_BASE_URL, BUILTIN_STD_BASE_URL, HasBuiltinCore, HasBuiltinStd};
use driver::DriverDataBase;
use rustc_hash::FxHashMap;
use std::path::{Path, PathBuf};
use tempfile::TempDir;
use url::Url;

pub struct VirtualFiles {
    root: TempDir,
    /// internal URI → tmp file URI (e.g. fe-builtin://core/... → file:///tmp/...)
    internal_to_tmp: FxHashMap<Url, Url>,
    /// tmp file URI → internal URI
    tmp_to_internal: FxHashMap<Url, Url>,
}

impl VirtualFiles {
    pub fn new(prefix: &str) -> anyhow::Result<Self> {
        let root = tempfile::Builder::new()
            .prefix(prefix)
            .tempdir()
            .context("create virtual files temp dir")?;

        Ok(Self {
            root,
            internal_to_tmp: FxHashMap::default(),
            tmp_to_internal: FxHashMap::default(),
        })
    }

    /// Write content to `root/<category>/<relative_path>`, return the tmp file URL.
    pub fn write_file(
        &mut self,
        category: &str,
        relative_path: &str,
        content: &str,
        readonly: bool,
    ) -> anyhow::Result<Url> {
        let out_path = join_url_path(&self.root.path().join(category), relative_path);
        if let Some(parent) = out_path.parent() {
            std::fs::create_dir_all(parent)
                .with_context(|| format!("create dir {}", parent.display()))?;
        }

        // If the file already exists as readonly, make it writable before overwriting
        if out_path.exists() {
            let mut perms = std::fs::metadata(&out_path)
                .with_context(|| format!("read metadata {}", out_path.display()))?
                .permissions();
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                perms.set_mode(0o644);
            }
            #[cfg(not(unix))]
            {
                perms.set_readonly(false);
            }
            std::fs::set_permissions(&out_path, perms)
                .with_context(|| format!("make writable {}", out_path.display()))?;
        }

        std::fs::write(&out_path, content)
            .with_context(|| format!("write file {}", out_path.display()))?;

        if readonly {
            set_readonly(&out_path)?;
        }

        Url::from_file_path(&out_path).map_err(|_| anyhow::anyhow!("bad path url"))
    }

    /// Register a bidirectional URI mapping.
    pub fn register_mapping(&mut self, internal_url: Url, tmp_url: Url) {
        self.internal_to_tmp
            .insert(internal_url.clone(), tmp_url.clone());
        self.tmp_to_internal.insert(tmp_url, internal_url);
    }

    pub fn map_internal_to_client(&self, uri: Url) -> Url {
        self.internal_to_tmp.get(&uri).cloned().unwrap_or(uri)
    }

    pub fn map_client_to_internal(&self, uri: Url) -> Url {
        if let Some(mapped) = self.tmp_to_internal.get(&uri) {
            return mapped.clone();
        }

        // Best-effort fallback: if a client normalized the file URI differently, map by path.
        if uri.scheme() == "file"
            && let Ok(path) = uri.to_file_path()
            && let Ok(rel) = path.strip_prefix(self.root.path())
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

    pub fn is_virtual_uri(&self, uri: &Url) -> bool {
        if self.tmp_to_internal.contains_key(uri) {
            return true;
        }
        uri.scheme() == "file"
            && uri
                .to_file_path()
                .is_ok_and(|path| path.starts_with(self.root.path()))
    }

    #[allow(dead_code)]
    pub fn root_path(&self) -> &Path {
        self.root.path()
    }
}

/// Materialize the built-in core/std ingots into the virtual files directory.
pub fn materialize_builtins(vfs: &mut VirtualFiles, db: &DriverDataBase) -> anyhow::Result<()> {
    let core = db.builtin_core();
    materialize_ingot(vfs, db, core, "core").context("materialize builtin core")?;

    let std_ingot = db.builtin_std();
    materialize_ingot(vfs, db, std_ingot, "std").context("materialize builtin std")?;

    Ok(())
}

fn materialize_ingot<'db>(
    vfs: &mut VirtualFiles,
    db: &'db DriverDataBase,
    ingot: common::ingot::Ingot<'db>,
    name: &str,
) -> anyhow::Result<()> {
    let base = match name {
        "core" => Url::parse(BUILTIN_CORE_BASE_URL).expect("builtin core url parse"),
        "std" => Url::parse(BUILTIN_STD_BASE_URL).expect("builtin std url parse"),
        _ => unreachable!("unknown builtin ingot {name}"),
    };

    for (url, file) in ingot.files(db).iter() {
        if url.scheme() != base.scheme() {
            continue;
        }

        let relative = url.path().trim_start_matches('/');
        if relative.is_empty() {
            continue;
        }

        let tmp_url = vfs.write_file(name, relative, file.text(db), true)?;
        vfs.register_mapping(url.clone(), tmp_url);
    }

    Ok(())
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
    let mut perms = std::fs::metadata(path)
        .with_context(|| format!("read metadata {}", path.display()))?
        .permissions();
    perms.set_mode(0o444);
    std::fs::set_permissions(path, perms)
        .with_context(|| format!("set readonly perms {}", path.display()))?;
    Ok(())
}

#[cfg(not(unix))]
fn set_readonly(path: &Path) -> anyhow::Result<()> {
    let mut perms = std::fs::metadata(path)
        .with_context(|| format!("read metadata {}", path.display()))?
        .permissions();
    perms.set_readonly(true);
    std::fs::set_permissions(path, perms)
        .with_context(|| format!("set readonly perms {}", path.display()))?;
    Ok(())
}
