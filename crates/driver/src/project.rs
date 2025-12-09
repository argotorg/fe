use std::fs;

use camino::{Utf8Path, Utf8PathBuf};
use common::{InputDb, file::File};
use hir::Ingot;
use resolver::{
    ResolutionHandler,
    files::{FilesResolutionDiagnostic, FilesResolver, FilesResource},
    ingot::minimal_files_resolver,
    Resolver,
};
use url::Url;

use crate::{DriverDataBase, db::DiagnosticsCollection, init_ingot};

pub enum Target {
    SingleFile(SingleFileTarget),
    Ingot(IngotTarget),
}

pub struct SingleFileTarget {
    file_url: Url,
    file: File,
    ingot_url: Option<Url>,
}

impl SingleFileTarget {
    pub fn file_url(&self) -> &Url {
        &self.file_url
    }

    pub fn file(&self) -> File {
        self.file
    }

    pub fn ingot_url(&self) -> Option<&Url> {
        self.ingot_url.as_ref()
    }
}

pub struct IngotTarget {
    ingot_url: Url,
}

impl IngotTarget {
    pub fn from_url(ingot_url: Url) -> Self {
        Self { ingot_url }
    }

    pub fn url(&self) -> &Url {
        &self.ingot_url
    }

    pub fn ingot<'db>(&self, db: &'db DriverDataBase) -> Option<Ingot<'db>> {
        db.workspace().containing_ingot(db, self.ingot_url.clone())
    }
}

#[allow(clippy::result_unit_err)]
pub fn resolve_target(db: &mut DriverDataBase, path: &Utf8PathBuf) -> Result<Target, ()> {
    let canonical = path.canonicalize_utf8().map_err(|_| {
        eprintln!("❌ Error: Invalid path: {path}");
    })?;

    if canonical.is_file() && canonical.extension() == Some("fe") {
        return resolve_single_file(db, &canonical).map(Target::SingleFile);
    }

    if canonical.is_dir() {
        return resolve_ingot(db, &canonical).map(Target::Ingot);
    }

    eprintln!("❌ Error: Path must be either a .fe file or a directory containing fe.toml");
    Err(())
}

fn resolve_single_file(
    db: &mut DriverDataBase,
    file_path: &Utf8PathBuf,
) -> Result<SingleFileTarget, ()> {
    let canonical = file_path.canonicalize_utf8().map_err(|_| {
        eprintln!("❌ Error: Invalid file path: {file_path}");
    })?;

    let file_url = Url::from_file_path(&canonical).map_err(|_| {
        eprintln!("❌ Error: Invalid file path: {file_path}");
    })?;

    let ingot_url = detect_ingot_url(&canonical);

    if let Some(root_url) = &ingot_url {
        // Load the entire ingot so the file can resolve imports across the ingot.
        if init_ingot(db, root_url) {
            return Err(());
        }
    } else {
        let content = fs::read_to_string(&canonical).map_err(|err| {
            eprintln!("Error reading file {file_path}: {err}");
        })?;

        db.workspace().touch(db, file_url.clone(), Some(content));
    }

    let Some(file) = db.workspace().get(db, &file_url) else {
        eprintln!("❌ Error: Could not process file {file_path}");
        return Err(());
    };

    Ok(SingleFileTarget {
        file_url,
        file,
        ingot_url,
    })
}

fn resolve_ingot(db: &mut DriverDataBase, dir_path: &Utf8PathBuf) -> Result<IngotTarget, ()> {
    let canonical = dir_path.canonicalize_utf8().map_err(|_| {
        eprintln!("Error: Invalid or non-existent directory path: {dir_path}");
        eprintln!("       Make sure the directory exists and is accessible");
    })?;

    let Some(ingot_url) = detect_ingot_url(&canonical) else {
        eprintln!("❌ Error: No fe.toml file found in the provided path or its parents");
        eprintln!("       Expected fe.toml somewhere above: {canonical}");
        return Err(());
    };

    if handle_init_diagnostics(db, &ingot_url) {
        return Err(());
    }

    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        report_missing_ingot(db, &ingot_url);
        return Err(());
    };

    if ingot.root_file(db).is_err() {
        eprintln!(
            "source files resolution error: `src` folder does not exist in the ingot directory"
        );
        return Err(());
    }

    Ok(IngotTarget { ingot_url })
}

fn handle_init_diagnostics(db: &mut DriverDataBase, ingot_url: &Url) -> bool {
    init_ingot(db, ingot_url)
}

fn report_missing_ingot(db: &DriverDataBase, ingot_url: &Url) {
    let config_url = match ingot_url.join("fe.toml") {
        Ok(url) => url,
        Err(_) => {
            eprintln!("❌ Error: Invalid ingot directory path");
            return;
        }
    };

    if db.workspace().get(db, &config_url).is_none() {
        eprintln!("❌ Error: No fe.toml file found in the root directory");
        eprintln!("       Expected fe.toml at: {config_url}");
        eprintln!("       Make sure you're in an fe project directory or create a fe.toml file");
    } else {
        eprintln!("❌ Error: Could not resolve ingot from directory");
    }
}

pub fn collect_dependency_errors<'db>(
    db: &'db DriverDataBase,
    ingot_target: &IngotTarget,
) -> Vec<(Url, DiagnosticsCollection<'db>)> {
    let mut dependency_errors = Vec::new();
    for dependency_url in db
        .dependency_graph()
        .dependency_urls(db, ingot_target.url())
    {
        let Some(ingot) = db.workspace().containing_ingot(db, dependency_url.clone()) else {
            continue;
        };
        let diags = db.run_on_ingot(ingot);
        if !diags.is_empty() {
            dependency_errors.push((dependency_url, diags));
        }
    }
    dependency_errors
}

pub fn report_dependency_errors<'db>(
    db: &'db DriverDataBase,
    dependency_errors: Vec<(Url, DiagnosticsCollection<'db>)>,
    show_details: bool,
) -> bool {
    if dependency_errors.is_empty() {
        return false;
    }

    if show_details {
        if dependency_errors.len() == 1 {
            eprintln!("❌ Error in downstream ingot");
        } else {
            eprintln!("❌ Errors in downstream ingots");
        }

        for (dependency_url, diags) in dependency_errors {
            print_dependency_info(db, &dependency_url);
            diags.emit(db);
        }
    } else {
        let count = dependency_errors.len();
        eprintln!(
            "❌ Downstream ingots reported {count} error{} (rerun with --show-downstream for details)",
            if count == 1 { "" } else { "s" }
        );
    }

    true
}

fn print_dependency_info(db: &DriverDataBase, dependency_url: &Url) {
    eprintln!();

    if let Some(ingot) = db.workspace().containing_ingot(db, dependency_url.clone()) {
        if let Some(config) = ingot.config(db) {
            let name = config.metadata.name.as_deref().unwrap_or("unknown");
            if let Some(version) = &config.metadata.version {
                eprintln!("➖ {name} (version: {version})");
            } else {
                eprintln!("➖ {name}");
            }
        } else {
            eprintln!("➖ Unknown dependency");
        }
    } else {
        eprintln!("➖ Unknown dependency");
    }

    eprintln!("🔗 {dependency_url}");
    eprintln!();
}

fn detect_ingot_url(path: &Utf8Path) -> Option<Url> {
    let url = if path.is_dir() {
        Url::from_directory_path(path.as_std_path()).ok()?
    } else {
        Url::from_file_path(path.as_std_path()).ok()?
    };

    let mut resolver = minimal_files_resolver();
    let mut handler = IngotProbe;
    resolver
        .resolve(&mut handler, &url)
        .ok()
        .map(|res| res.ingot_url)
}

struct IngotProbe;

impl ResolutionHandler<FilesResolver> for IngotProbe {
    type Item = FilesResource;

    fn on_resolution_start(&mut self, _description: &Url) {}

    fn on_resolution_diagnostic(&mut self, _diagnostic: FilesResolutionDiagnostic) {}

    fn handle_resolution(&mut self, _description: &Url, resource: FilesResource) -> Self::Item {
        resource
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::DriverDataBase;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn resolves_ingot_when_given_inner_file() {
        let temp = tempdir().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        fs::write(root.join("fe.toml"), r#"name = "demo""#).unwrap();
        let src_dir = root.join("src");
        fs::create_dir_all(src_dir.as_std_path()).unwrap();
        let lib = src_dir.join("lib.fe");
        fs::write(&lib, "contract Foo {}\n").unwrap();

        let mut db = DriverDataBase::default();
        let target = resolve_target(&mut db, &lib).unwrap();
        match target {
            Target::Ingot(ingot) => {
                let expected =
                    Url::from_directory_path(root.as_std_path()).expect("valid ingot url");
                assert_eq!(ingot.url(), &expected);
            }
            Target::SingleFile(_) => panic!("expected ingot target"),
        }
    }
}
