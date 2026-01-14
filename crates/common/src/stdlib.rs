use std::fs;

use camino::{Utf8Path, Utf8PathBuf};
use rust_embed::Embed;
use url::Url;

use crate::{
    InputDb,
    ingot::{Ingot, IngotBaseUrl},
};

pub static BUILTIN_CORE_BASE_URL: &str = "builtin-core:///";
pub static BUILTIN_STD_BASE_URL: &str = "builtin-std:///";

fn initialize_builtin<E: Embed>(db: &mut dyn InputDb, base_url: &str) {
    let base = Url::parse(base_url).unwrap();

    for (path, contents) in E::iter().filter_map(|path| {
        E::get(&path).map(|content| {
            let contents = String::from_utf8(content.data.into_owned()).unwrap();
            (Utf8PathBuf::from(path.to_string()), contents)
        })
    }) {
        base.touch(db, path, contents.into());
    }
}

fn load_library_dir(db: &mut dyn InputDb, base_url: &str, root: &Utf8Path) -> Result<(), String> {
    let base = Url::parse(base_url).map_err(|_| "invalid base url".to_string())?;
    let mut stack = vec![root.to_path_buf()];

    while let Some(dir) = stack.pop() {
        let entries = fs::read_dir(dir.as_std_path())
            .map_err(|err| format!("Failed to read {}: {err}", dir))?;
        for entry in entries {
            let entry = entry.map_err(|err| format!("Failed to read entry: {err}"))?;
            let path = Utf8PathBuf::from_path_buf(entry.path())
                .map_err(|_| "Library path is not UTF-8".to_string())?;
            let file_type = entry
                .file_type()
                .map_err(|err| format!("Failed to read file type: {err}"))?;
            if file_type.is_dir() {
                stack.push(path);
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

    load_library_dir(db, BUILTIN_CORE_BASE_URL, &core_root)?;
    load_library_dir(db, BUILTIN_STD_BASE_URL, &std_root)?;
    Ok(())
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
