use std::fs;

use camino::Utf8PathBuf;
use common::{InputDb, file::File};
use hir::Ingot;
use url::Url;

use crate::{DriverDataBase, db::DiagnosticsCollection, init_ingot};

pub enum Target {
    SingleFile(SingleFileTarget),
    Ingot(IngotTarget),
}

pub struct SingleFileTarget {
    file_url: Url,
    file: File,
}

impl SingleFileTarget {
    pub fn file_url(&self) -> &Url {
        &self.file_url
    }

    pub fn file(&self) -> File {
        self.file
    }
}

pub struct IngotTarget {
    ingot_url: Url,
}

impl IngotTarget {
    pub fn url(&self) -> &Url {
        &self.ingot_url
    }

    pub fn ingot<'db>(&self, db: &'db DriverDataBase) -> Option<Ingot<'db>> {
        db.workspace().containing_ingot(db, self.ingot_url.clone())
    }
}

pub fn resolve_target(db: &mut DriverDataBase, path: &Utf8PathBuf) -> Result<Target, ()> {
    if path.is_file() && path.extension() == Some("fe") {
        resolve_single_file(db, path).map(Target::SingleFile)
    } else if path.is_dir() {
        resolve_ingot(db, path).map(Target::Ingot)
    } else {
        eprintln!("❌ Error: Path must be either a .fe file or a directory containing fe.toml");
        Err(())
    }
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

    let content = fs::read_to_string(file_path).map_err(|err| {
        eprintln!("Error reading file {file_path}: {err}");
    })?;

    db.workspace().touch(db, file_url.clone(), Some(content));
    let Some(file) = db.workspace().get(db, &file_url) else {
        eprintln!("❌ Error: Could not process file {file_path}");
        return Err(());
    };

    Ok(SingleFileTarget { file_url, file })
}

fn resolve_ingot(db: &mut DriverDataBase, dir_path: &Utf8PathBuf) -> Result<IngotTarget, ()> {
    let canonical_path = dir_path.canonicalize_utf8().map_err(|_| {
        eprintln!("Error: Invalid or non-existent directory path: {dir_path}");
        eprintln!("       Make sure the directory exists and is accessible");
    })?;

    let ingot_url = Url::from_directory_path(canonical_path.as_str()).map_err(|_| {
        eprintln!("❌ Error: Invalid directory path: {dir_path}");
    })?;

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
    let init_diagnostics = init_ingot(db, ingot_url);
    if init_diagnostics.is_empty() {
        return false;
    }

    let mut has_init_issues = false;
    for diagnostic in &init_diagnostics {
        eprintln!("❌ {diagnostic}");
        has_init_issues = true;
    }
    has_init_issues
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
    for dependency_url in db.graph().dependency_urls(db, ingot_target.url()) {
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
) -> bool {
    if dependency_errors.is_empty() {
        return false;
    }

    if dependency_errors.len() == 1 {
        eprintln!("❌ Error in downstream ingot");
    } else {
        eprintln!("❌ Errors in downstream ingots");
    }

    for (dependency_url, diags) in dependency_errors {
        print_dependency_info(db, &dependency_url);
        diags.emit(db);
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
