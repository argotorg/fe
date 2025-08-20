use core::panic;

use camino::Utf8PathBuf;
pub use radix_immutable::StringPrefixView;
use smol_str::SmolStr;
use url::Url;

use crate::config::{Config, ConfigDiagnostic};
use crate::core::BUILTIN_CORE_BASE_URL;
use crate::file::{File, Workspace};
use crate::urlext::UrlExt;
use crate::InputDb;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IngotKind {
    /// A standalone ingot is a dummy ingot when the compiler is invoked
    /// directly on a file.
    StandAlone,

    /// A local ingot which is the current ingot being compiled.
    Local,

    /// An external ingot which is depended on by the current ingot.
    External,

    /// Core library ingot.
    Core,
}

pub trait IngotBaseUrl {
    fn touch(
        &self,
        db: &mut dyn InputDb,
        path: Utf8PathBuf,
        initial_content: Option<String>,
    ) -> File;
    fn ingot<'db>(&self, db: &'db dyn InputDb) -> Option<Ingot<'db>>;
}

impl IngotBaseUrl for Url {
    fn touch(
        &self,
        db: &mut dyn InputDb,
        relative_path: Utf8PathBuf,
        initial_content: Option<String>,
    ) -> File {
        if relative_path.is_absolute() {
            panic!("Expected relative path, got absolute path: {relative_path}");
        }
        let path = self
            .directory()
            .expect("failed to parse directory")
            .join(relative_path.as_str())
            .expect("failed to parse path");
        db.workspace().touch(db, path, initial_content)
    }
    fn ingot<'db>(&self, db: &'db dyn InputDb) -> Option<Ingot<'db>> {
        db.workspace().containing_ingot(db, self.clone())
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct Ingot<'db> {
    pub base: Url,
    pub standalone_file: Option<File>,
    pub kind: IngotKind,
}

#[derive(Debug)]
pub enum IngotError {
    RootFileNotFound,
}

#[salsa::tracked]
impl<'db> Ingot<'db> {
    pub fn root_file(&self, db: &dyn InputDb) -> Result<File, IngotError> {
        if let Some(root_file) = self.standalone_file(db) {
            Ok(root_file)
        } else {
            let path = self
                .base(db)
                .join("src/lib.fe")
                .expect("failed to join path");
            db.workspace()
                .get(db, &path)
                .ok_or(IngotError::RootFileNotFound)
        }
    }

    #[salsa::tracked]
    pub fn files(self, db: &'db dyn InputDb) -> StringPrefixView<'db, Url, File> {
        if let Some(standalone_file) = self.standalone_file(db) {
            // For standalone ingots, use the standalone file URL as the base
            db.workspace().items_at_base(
                db,
                standalone_file
                    .url(db)
                    .expect("file should be registered in the index"),
            )
        } else {
            // For regular ingots, use the ingot base URL
            db.workspace().items_at_base(db, self.base(db))
        }
    }

    #[salsa::tracked]
    pub fn config(self, db: &'db dyn InputDb) -> Option<Config> {
        db.workspace()
            .containing_ingot_config(db, self.base(db))
            .map(|config_file| match Config::parse(config_file.text(db)) {
                Ok(config) => config,
                Err(err) => {
                    // Create a Config with just the parse error as a diagnostic
                    let diagnostics = vec![ConfigDiagnostic::InvalidTomlSyntax(err.to_string())];
                    Config {
                        metadata: Default::default(),
                        diagnostics,
                        dependencies: Vec::new(),
                    }
                }
            })
    }

    #[salsa::tracked]
    pub fn version(self, db: &'db dyn InputDb) -> Option<Version> {
        self.config(db).and_then(|config| config.metadata.version)
    }

    #[salsa::tracked]
    pub fn dependencies(self, db: &'db dyn InputDb) -> Vec<(SmolStr, Url)> {
        let base_url = self.base(db);
        let mut deps = match self.config(db) {
            Some(config) => config
                .forward_edges(&base_url)
                .into_iter()
                .map(|(url, weight)| (weight.alias, url))
                .collect(),
            None => vec![],
        };

        if self.kind(db) != IngotKind::Core {
            deps.push((
                "core".into(),
                Url::parse(BUILTIN_CORE_BASE_URL).expect("couldn't parse core ingot URL"),
            ))
        }
        deps
    }
}

pub type Version = serde_semver::semver::Version;

#[salsa::tracked]
impl Workspace {
    /// Recursively search for a local ingot configuration file
    #[salsa::tracked]
    pub fn containing_ingot_config(self, db: &dyn InputDb, file: Url) -> Option<File> {
        tracing::debug!(target: "ingot_config", "containing_ingot_config called with file: {}", file);
        let dir = match file.directory() {
            Some(d) => d,
            None => {
                tracing::debug!(target: "ingot_config", "Could not get directory for: {}", file);
                return None;
            }
        };
        tracing::debug!(target: "ingot_config", "Search directory: {}", dir);

        let config_url = match dir.join("fe.toml") {
            Ok(url) => url,
            Err(_) => {
                tracing::debug!(target: "ingot_config", "Could not join 'fe.toml' to dir: {}", dir);
                return None;
            }
        };
        tracing::debug!(target: "ingot_config", "Looking for config file at: {}", config_url);

        if let Some(file_obj) = self.get(db, &config_url) {
            tracing::debug!(target: "ingot_config", "Found config file in index: {}", config_url);
            Some(file_obj)
        } else {
            tracing::debug!(target: "ingot_config", "Config file NOT found in index: {}. Checking parent.", config_url);
            if let Some(parent_dir_url) = dir.parent() {
                tracing::debug!(target: "ingot_config", "Recursively calling containing_ingot_config for parent: {}", parent_dir_url);
                self.containing_ingot_config(db, parent_dir_url)
            } else {
                tracing::debug!(target: "ingot_config", "No parent directory for {}, stopping search.", dir);
                None
            }
        }
    }

    #[salsa::tracked]
    pub fn containing_ingot(self, db: &dyn InputDb, location: Url) -> Option<Ingot<'_>> {
        // Try to find a config file to determine if this is part of a structured ingot
        if let Some(config_file) = db.workspace().containing_ingot_config(db, location.clone()) {
            // Extract base URL from config file location
            let base_url = config_file
                .url(db)
                .expect("Config file should be indexed")
                .directory()
                .expect("Config URL should have a directory");

            let kind = if base_url.scheme().contains("core") {
                IngotKind::Core
            } else {
                IngotKind::Local
            };

            Some(Ingot::new(db, base_url.clone(), None, kind))
        } else {
            // Make a standalone ingot if no config is found
            let base = location.directory().unwrap_or_else(|| location.clone());
            let specific_root_file = if location.path().ends_with(".fe") {
                db.workspace().get(db, &location)
            } else {
                None
            };
            Some(Ingot::new(
                db,
                base,
                specific_root_file,
                IngotKind::StandAlone,
            ))
        }
    }

    pub fn touch_ingot<'db>(
        self,
        db: &'db mut dyn InputDb,
        base_url: &Url,
        config_content: Option<String>,
    ) -> Option<Ingot<'db>> {
        let base_dir = base_url
            .directory()
            .expect("Base URL should have a directory");
        let config_file = base_dir
            .join("fe.toml")
            .expect("Config file should be indexed");
        let config = self.touch(db, config_file, config_content);

        config.containing_ingot(db)
    }
}

#[cfg(test)]
mod tests {
    use crate::file::File;

    use super::*;

    use crate::define_input_db;

    define_input_db!(TestDatabase);

    #[test]
    fn test_locate_config() {
        let mut db = TestDatabase::default();
        let index = db.workspace();

        // Create our test files - a library file, a config file, and a standalone file
        let url_lib = Url::parse("file:///foo/src/lib.fe").unwrap();
        let lib = File::__new_impl(&db, "lib".to_string());

        let url_config = Url::parse("file:///foo/fe.toml").unwrap();
        let config = File::__new_impl(&db, "config".to_string());

        let url_standalone = Url::parse("file:///bar/standalone.fe").unwrap();
        let standalone = File::__new_impl(&db, "standalone".to_string());

        // Add the files to the index
        index
            .set(&mut db, url_lib.clone(), lib)
            .expect("Failed to set lib file");
        index
            .set(&mut db, url_config.clone(), config)
            .expect("Failed to set config file");
        index
            .set(&mut db, url_standalone.clone(), standalone)
            .expect("Failed to set standalone file");

        // Test recursive search: lib.fe is in /foo/src/ but config is in /foo/
        // This tests that we correctly search up the directory tree
        let found_config = index.containing_ingot_config(&db, url_lib);
        assert!(found_config.is_some());
        assert_eq!(found_config.and_then(|c| c.url(&db)).unwrap(), url_config);

        // Test that standalone file without a config returns None
        let no_config = index.containing_ingot_config(&db, url_standalone);
        assert!(no_config.is_none());
    }

    #[test]
    fn test_same_ingot_for_nested_paths() {
        let mut db = TestDatabase::default();
        let index = db.workspace();

        // Create an ingot structure
        let url_config = Url::parse("file:///project/fe.toml").unwrap();
        let config = File::__new_impl(&db, "[ingot]\nname = \"test\"".to_string());

        let url_lib = Url::parse("file:///project/src/lib.fe").unwrap();
        let lib = File::__new_impl(&db, "pub fn main() {}".to_string());

        let url_mod = Url::parse("file:///project/src/module.fe").unwrap();
        let module = File::__new_impl(&db, "pub fn helper() {}".to_string());

        let url_nested = Url::parse("file:///project/src/nested/deep.fe").unwrap();
        let nested = File::__new_impl(&db, "pub fn deep_fn() {}".to_string());

        // Add all files to the index
        index
            .set(&mut db, url_config.clone(), config)
            .expect("Failed to set config file");
        index
            .set(&mut db, url_lib.clone(), lib)
            .expect("Failed to set lib file");
        index
            .set(&mut db, url_mod.clone(), module)
            .expect("Failed to set module file");
        index
            .set(&mut db, url_nested.clone(), nested)
            .expect("Failed to set nested file");

        // Get ingots for different files in the same project
        let ingot_lib = index.containing_ingot(&db, url_lib);
        let ingot_mod = index.containing_ingot(&db, url_mod);
        let ingot_nested = index.containing_ingot(&db, url_nested);

        // All should return Some
        assert!(ingot_lib.is_some());
        assert!(ingot_mod.is_some());
        assert!(ingot_nested.is_some());

        let ingot_lib = ingot_lib.unwrap();
        let ingot_mod = ingot_mod.unwrap();
        let ingot_nested = ingot_nested.unwrap();

        // Critical test: All files in the same logical ingot should return the SAME Salsa instance
        // This ensures we don't have infinite loops due to different ingot IDs
        assert_eq!(
            ingot_lib, ingot_mod,
            "lib.fe and module.fe should have the same ingot"
        );
        assert_eq!(
            ingot_lib, ingot_nested,
            "lib.fe and nested/deep.fe should have the same ingot"
        );
        assert_eq!(
            ingot_mod, ingot_nested,
            "module.fe and nested/deep.fe should have the same ingot"
        );

        // Verify they all have the same base URL
        assert_eq!(ingot_lib.base(&db), ingot_mod.base(&db));
        assert_eq!(ingot_lib.base(&db), ingot_nested.base(&db));

        let expected_base = Url::parse("file:///project/").unwrap();
        assert_eq!(ingot_lib.base(&db), expected_base);
    }

    #[test]
    fn test_ingot_files_updates_when_new_files_added() {
        let mut db = TestDatabase::default();
        let index = db.workspace();

        // Create initial files for an ingot
        let config_url = Url::parse("file:///project/fe.toml").unwrap();
        let config_file = File::__new_impl(&db, "[ingot]\nname = \"test\"".to_string());

        let lib_url = Url::parse("file:///project/src/lib.fe").unwrap();
        let lib_file = File::__new_impl(&db, "pub use S".to_string());

        // Add initial files to the index
        index
            .set(&mut db, config_url.clone(), config_file)
            .expect("Failed to set config file");
        index
            .set(&mut db, lib_url.clone(), lib_file)
            .expect("Failed to set lib file");

        // Get the ingot and its initial files, then drop the reference
        let initial_count = {
            let ingot = index
                .containing_ingot(&db, lib_url.clone())
                .expect("Should find ingot");
            let initial_files = ingot.files(&db);
            initial_files.iter().count()
        };

        // Should have 2 files initially (config + lib)
        assert_eq!(initial_count, 2, "Should have 2 initial files");

        // Add a new source file to the same ingot
        let mod_url = Url::parse("file:///project/src/module.fe").unwrap();
        let mod_file = File::__new_impl(&db, "pub struct NewStruct;".to_string());

        index
            .set(&mut db, mod_url.clone(), mod_file)
            .expect("Failed to set module file");

        // Get the updated files list - this tests that Salsa correctly invalidates
        // and recomputes the files list when new files are added
        let ingot = index
            .containing_ingot(&db, lib_url.clone())
            .expect("Should find ingot");
        let updated_files = ingot.files(&db);
        let updated_count = updated_files.iter().count();

        // Should now have 3 files (config + lib + module)
        assert_eq!(updated_count, 3, "Should have 3 files after adding module");

        // Verify the new file is in the list
        let file_urls: Vec<Url> = updated_files.iter().map(|(url, _)| url).collect();
        assert!(
            file_urls.contains(&mod_url),
            "New module file should be in the files list"
        );
        assert!(
            file_urls.contains(&lib_url),
            "Original lib file should still be in the files list"
        );
        assert!(
            file_urls.contains(&config_url),
            "Config file should still be in the files list"
        );
    }

    #[test]
    fn test_file_containing_ingot_establishes_dependency() {
        let mut db = TestDatabase::default();
        let index = db.workspace();

        // Create a regular ingot with config file
        let config_url = Url::parse("file:///project/fe.toml").unwrap();
        let config_file = File::__new_impl(&db, "[ingot]\nname = \"test\"".to_string());

        let main_url = Url::parse("file:///project/src/main.fe").unwrap();
        let main_file = File::__new_impl(&db, "use foo::*\npub use S".to_string());

        index
            .set(&mut db, config_url.clone(), config_file)
            .expect("Failed to set config file");
        index
            .set(&mut db, main_url.clone(), main_file)
            .expect("Failed to set main file");

        // Call containing_ingot, which should trigger the side effect of calling ingot.files()
        let ingot_option = main_file.containing_ingot(&db);
        assert!(ingot_option.is_some(), "Should find ingot for main file");

        // Drop the ingot reference before mutating the database
        let _ = ingot_option;

        // Add another file to the same ingot
        let other_url = Url::parse("file:///project/src/other.fe").unwrap();
        let other_file = File::__new_impl(&db, "pub struct OtherStruct;".to_string());

        index
            .set(&mut db, other_url.clone(), other_file)
            .expect("Failed to set other file");

        // Get the ingot again and check that the dependency established by the containing_ingot
        // call ensures the files list is correctly updated
        let ingot = main_file.containing_ingot(&db).expect("Should find ingot");
        let files = ingot.files(&db);
        let file_count = files.iter().count();

        // Should have all files now (config + main + other)
        assert_eq!(file_count, 3, "Should have 3 files in the ingot");

        let file_urls: Vec<Url> = files.iter().map(|(url, _)| url).collect();
        assert!(
            file_urls.contains(&config_url),
            "Should contain config file"
        );
        assert!(file_urls.contains(&main_url), "Should contain main file");
        assert!(file_urls.contains(&other_url), "Should contain other file");
    }
}
