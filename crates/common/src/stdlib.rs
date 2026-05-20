use std::fs;

use camino::{Utf8Path, Utf8PathBuf};
use rust_embed::Embed;
use url::Url;

use crate::{
    InputDb,
    ingot::{Ingot, IngotBaseUrl},
};

// Use the canonical single-slash form for custom-scheme base URLs.
// `Url::join` normalizes children under `builtin-core:/...`, so lookups must
// use the same base form or builtin ingots appear empty.
pub static BUILTIN_CORE_BASE_URL: &str = "builtin-core:/";
pub static BUILTIN_STD_BASE_URL: &str = "builtin-std:/";

fn is_library_file(path: &Utf8Path) -> bool {
    matches!(path.file_name(), Some("fe.toml")) || matches!(path.extension(), Some("fe"))
}

fn initialize_builtin<E: Embed>(db: &mut dyn InputDb, base_url: &str) {
    let base = Url::parse(base_url).unwrap();

    // Ensure deterministic file insertion order across platforms/builds.
    // This matters because downstream iteration over workspace tries is depth-first.
    let mut paths = E::iter()
        .map(|path| Utf8PathBuf::from(path.to_string()))
        .collect::<Vec<_>>();
    paths.sort();
    for path in paths {
        if !is_library_file(&path) {
            continue;
        }

        let contents = String::from_utf8(
            E::get(path.as_str())
                .unwrap_or_else(|| panic!("missing embedded builtin `{path}`"))
                .data
                .into_owned(),
        )
        .unwrap_or_else(|_| panic!("embedded builtin `{path}` must be UTF-8"));
        base.touch(db, path, contents.into());
    }
}

fn load_library_dir(db: &mut dyn InputDb, base_url: &str, root: &Utf8Path) -> Result<(), String> {
    let base = Url::parse(base_url).map_err(|_| "invalid base url".to_string())?;
    let mut stack = vec![root.to_path_buf()];

    while let Some(dir) = stack.pop() {
        let entries = fs::read_dir(dir.as_std_path())
            .map_err(|err| format!("Failed to read {}: {err}", dir))?
            .map(|entry| {
                let entry = entry.map_err(|err| format!("Failed to read entry: {err}"))?;
                let path = Utf8PathBuf::from_path_buf(entry.path())
                    .map_err(|_| "Library path is not UTF-8".to_string())?;
                let file_type = entry
                    .file_type()
                    .map_err(|err| format!("Failed to read file type: {err}"))?;
                Ok((path, file_type))
            })
            .collect::<Result<Vec<_>, String>>()?;

        // Ensure deterministic traversal order across platforms/filesystems.
        let mut entries = entries;
        entries.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));
        for (path, file_type) in entries {
            if file_type.is_dir() {
                stack.push(path);
                continue;
            }
            if !is_library_file(&path) {
                continue;
            }
            let relative = path
                .strip_prefix(root)
                .map_err(|_| "Library path escaped root".to_string())?;
            let url = base
                .join(relative.as_str())
                .map_err(|_| "Failed to join library path".to_string())?;
            let content = fs::read_to_string(path.as_std_path())
                .map_err(|err| format!("Failed to read {}: {err}", path))?;
            db.workspace().update(db, url, content);
        }
    }

    Ok(())
}

pub fn load_library_from_path(db: &mut dyn InputDb, library_root: &Utf8Path) -> Result<(), String> {
    let core_root = library_root.join("core");
    let std_root = library_root.join("std");

    // Clear embedded builtins first so stale files from the embedded version
    // don't leak into the index when the on-disk version has fewer files.
    clear_library(db, BUILTIN_CORE_BASE_URL);
    clear_library(db, BUILTIN_STD_BASE_URL);

    load_library_dir(db, BUILTIN_CORE_BASE_URL, &core_root)?;
    load_library_dir(db, BUILTIN_STD_BASE_URL, &std_root)?;
    Ok(())
}

fn clear_library(db: &mut dyn InputDb, base_url: &str) {
    let base = Url::parse(base_url).unwrap();
    let workspace = db.workspace();
    let urls: Vec<Url> = workspace
        .items_at_base(db, base)
        .iter()
        .map(|(url, _)| url.clone())
        .collect();
    for url in urls {
        workspace.remove(db, &url);
    }
}

#[derive(Embed)]
#[folder = "../../ingots/core"]
pub struct Core;

pub trait HasBuiltinCore: InputDb {
    fn initialize_builtin_core(&mut self);
    fn builtin_core(&self) -> Ingot<'_>;
}

impl<T: InputDb> HasBuiltinCore for T {
    fn initialize_builtin_core(&mut self) {
        initialize_builtin::<Core>(self, BUILTIN_CORE_BASE_URL);
    }

    fn builtin_core(&self) -> Ingot<'_> {
        let core = self
            .workspace()
            .containing_ingot(self, Url::parse(BUILTIN_CORE_BASE_URL).unwrap());
        core.expect("Built-in core ingot failed to initialize")
    }
}

#[derive(Embed)]
#[folder = "../../ingots/std"]
pub struct Std;

pub trait HasBuiltinStd: InputDb {
    fn initialize_builtin_std(&mut self);
    fn builtin_std(&self) -> Ingot<'_>;
}

impl<T: InputDb> HasBuiltinStd for T {
    fn initialize_builtin_std(&mut self) {
        initialize_builtin::<Std>(self, BUILTIN_STD_BASE_URL);
    }

    fn builtin_std(&self) -> Ingot<'_> {
        let std = self
            .workspace()
            .containing_ingot(self, Url::parse(BUILTIN_STD_BASE_URL).unwrap());
        std.expect("Built-in std ingot failed to initialize")
    }
}

#[cfg(test)]
mod tests {
    use camino::Utf8Path;

    use super::{HasBuiltinCore, HasBuiltinStd, is_library_file};
    use crate::define_input_db;

    define_input_db!(TestDb);

    #[test]
    fn library_loader_filters_non_fe_files() {
        assert!(is_library_file(Utf8Path::new("fe.toml")));
        assert!(is_library_file(Utf8Path::new("src/lib.fe")));
        assert!(!is_library_file(Utf8Path::new(".DS_Store")));
        assert!(!is_library_file(Utf8Path::new("src/lib.rs")));
        assert!(!is_library_file(Utf8Path::new("README.md")));
    }

    #[test]
    fn builtin_ingots_are_indexed_under_their_lookup_urls() {
        let db = TestDb::default();
        let core = db.builtin_core();
        let std = db.builtin_std();

        assert!(
            core.files(&db).iter().next().is_some(),
            "builtin core ingot should contain indexed files"
        );
        assert!(
            std.files(&db).iter().next().is_some(),
            "builtin std ingot should contain indexed files"
        );
    }
}
