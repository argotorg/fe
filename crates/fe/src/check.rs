use std::collections::HashSet;

use camino::Utf8PathBuf;
use codegen::emit_module_yul;
use common::{
    InputDb,
    config::{Config, WorkspaceConfig},
    file::IngotFileKind,
};
use driver::DriverDataBase;
use driver::cli_target::{CliTarget, resolve_cli_target};
use hir::hir_def::{HirIngot, TopLevelMod};
use mir::lower_module;
use resolver::ResolutionHandler;
use resolver::Resolver;
use url::Url;

pub fn check(path: &Utf8PathBuf, dump_mir: bool, emit_yul_min: bool) {
    let mut db = DriverDataBase::default();

    let target = match resolve_cli_target(&mut db, path) {
        Ok(target) => target,
        Err(message) => {
            eprintln!("Error: {message}");
            std::process::exit(1);
        }
    };

    let has_errors = match target {
        CliTarget::StandaloneFile(file_path) => {
            check_single_file(&mut db, &file_path, dump_mir, emit_yul_min)
        }
        CliTarget::Directory(dir_path) => {
            check_directory(&mut db, &dir_path, dump_mir, emit_yul_min)
        }
    };

    if has_errors {
        std::process::exit(1);
    }
}

fn check_directory(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    dump_mir: bool,
    emit_yul_min: bool,
) -> bool {
    let ingot_url = match dir_url(dir_path) {
        Ok(url) => url,
        Err(message) => {
            eprintln!("Error: {message}");
            return true;
        }
    };

    let had_init_diagnostics = driver::init_ingot(db, &ingot_url);
    if had_init_diagnostics {
        return true;
    }

    let config = match config_from_db(db, dir_path) {
        Ok(Some(config)) => config,
        Ok(None) => {
            eprintln!("Error: No fe.toml file found in the root directory");
            return true;
        }
        Err(err) => {
            eprintln!("Error: {err}");
            return true;
        }
    };

    match config {
        Config::Workspace(workspace) => {
            check_workspace(db, dir_path, *workspace, dump_mir, emit_yul_min)
        }
        Config::Ingot(_) => check_ingot_url(db, &ingot_url, dump_mir, emit_yul_min),
    }
}

fn config_from_db(db: &DriverDataBase, dir_path: &Utf8PathBuf) -> Result<Option<Config>, String> {
    let config_path = if dir_path.is_absolute() {
        dir_path.join("fe.toml")
    } else {
        let cwd = std::env::current_dir()
            .map_err(|err| format!("Failed to read current directory: {err}"))?;
        let cwd = Utf8PathBuf::from_path_buf(cwd)
            .map_err(|_| "Current directory is not valid UTF-8".to_string())?;
        cwd.join(dir_path).join("fe.toml")
    };
    if !config_path.is_file() {
        return Ok(None);
    }
    let config_url = Url::from_file_path(config_path.as_std_path())
        .map_err(|_| format!("Invalid config path: {config_path}"))?;
    let content = db
        .workspace()
        .get(db, &config_url)
        .ok_or_else(|| format!("Config file not loaded by resolver: {config_path}"))?
        .text(db)
        .to_string();
    let config_file =
        Config::parse(&content).map_err(|err| format!("Failed to parse {config_path}: {err}"))?;
    Ok(Some(config_file))
}

fn dir_url(path: &Utf8PathBuf) -> Result<Url, String> {
    let canonical_path = match path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            let cwd = std::env::current_dir()
                .map_err(|err| format!("Failed to read current directory: {err}"))?;
            let cwd = Utf8PathBuf::from_path_buf(cwd)
                .map_err(|_| "Current directory is not valid UTF-8".to_string())?;
            cwd.join(path)
        }
    };
    Url::from_directory_path(canonical_path.as_str())
        .map_err(|_| format!("Invalid or non-existent directory path: {path}"))
}

fn check_single_file(
    db: &mut DriverDataBase,
    file_path: &Utf8PathBuf,
    dump_mir: bool,
    emit_yul_min: bool,
) -> bool {
    let file_url = match Url::from_file_path(file_path.canonicalize_utf8().unwrap()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid file path: {file_path}");
            return true;
        }
    };

    struct StandaloneFileLoader<'a> {
        db: &'a mut DriverDataBase,
    }

    impl<'a> ResolutionHandler<resolver::files::FilesResolver> for StandaloneFileLoader<'a> {
        type Item = ();

        fn handle_resolution(
            &mut self,
            _description: &Url,
            resource: resolver::files::FilesResource,
        ) -> Self::Item {
            for file in resource.files {
                let file_url =
                    Url::from_file_path(file.path.as_std_path()).expect("valid file URL");
                self.db
                    .workspace()
                    .touch(self.db, file_url, Some(file.content));
            }
        }
    }

    let mut resolver = resolver::files::FilesResolver::new();
    if let Err(err) = resolver.resolve(&mut StandaloneFileLoader { db }, &file_url) {
        eprintln!("Error: Failed to read file {file_path}: {err}");
        return true;
    }

    // Try to get the file and check it for errors
    if let Some(file) = db.workspace().get(db, &file_url) {
        let top_mod = db.top_mod(file);
        let diags = db.run_on_top_mod(top_mod);
        if !diags.is_empty() {
            eprintln!("errors in {file_url}");
            eprintln!();
            diags.emit(db);
            return true;
        }
        if dump_mir {
            dump_module_mir(db, top_mod);
        }
        if emit_yul_min {
            emit_yul(db, top_mod);
        }
    } else {
        eprintln!("Error: Could not process file {file_path}");
        return true;
    }

    false
}

fn check_ingot_url(
    db: &mut DriverDataBase,
    ingot_url: &Url,
    dump_mir: bool,
    emit_yul_min: bool,
) -> bool {
    if db
        .workspace()
        .containing_ingot(db, ingot_url.clone())
        .is_none()
    {
        // Check if the issue is a missing fe.toml file
        let config_url = match ingot_url.join("fe.toml") {
            Ok(url) => url,
            Err(_) => {
                eprintln!("Error: Invalid ingot directory path");
                return true;
            }
        };

        if db.workspace().get(db, &config_url).is_none() {
            eprintln!("Error: No fe.toml file found in the root directory");
            eprintln!("       Expected fe.toml at: {config_url}");
            eprintln!(
                "       Make sure you're in an fe project directory or create a fe.toml file"
            );
        } else {
            eprintln!("Error: Could not resolve ingot from directory");
        }
        return true;
    }

    let mut seen = HashSet::new();
    check_ingot_and_dependencies(db, ingot_url, dump_mir, emit_yul_min, &mut seen)
}

fn check_workspace(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    workspace_config: WorkspaceConfig,
    dump_mir: bool,
    emit_yul_min: bool,
) -> bool {
    let workspace_url = match dir_url(dir_path) {
        Ok(url) => url,
        Err(message) => {
            eprintln!("Error: {message}");
            return true;
        }
    };

    let members = match driver::workspace_members(&workspace_config.workspace, &workspace_url) {
        Ok(members) => members,
        Err(err) => {
            eprintln!("Error: Failed to resolve workspace members: {err}");
            return true;
        }
    };

    if members.is_empty() {
        eprintln!("Warning: No workspace members found");
        return false;
    }

    let mut seen = HashSet::new();
    let mut has_errors = false;
    for member in members {
        let member_url = member.url;
        let member_has_errors =
            check_ingot_and_dependencies(db, &member_url, dump_mir, emit_yul_min, &mut seen);
        has_errors |= member_has_errors;
    }

    has_errors
}

fn check_ingot_and_dependencies(
    db: &mut DriverDataBase,
    ingot_url: &Url,
    dump_mir: bool,
    emit_yul_min: bool,
    seen: &mut HashSet<Url>,
) -> bool {
    if !seen.insert(ingot_url.clone()) {
        return false;
    }

    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        eprintln!("Error: Could not resolve ingot {ingot_url}");
        return true;
    };

    if ingot.root_file(db).is_err() {
        eprintln!("Error: `src` folder does not exist in the ingot directory");
        return true;
    }

    if !ingot_has_source_files(db, ingot) {
        eprintln!("Error: Could not find source files for ingot {ingot_url}");
        return true;
    }

    let diags = db.run_on_ingot(ingot);
    let mut has_errors = false;

    if !diags.is_empty() {
        diags.emit(db);
        has_errors = true;
    } else {
        let root_mod = ingot.root_mod(db);
        if dump_mir {
            dump_module_mir(db, root_mod);
        }
        if emit_yul_min {
            emit_yul(db, root_mod);
        }
    }

    let mut dependency_errors = Vec::new();
    for dependency_url in db.dependency_graph().dependency_urls(db, ingot_url) {
        if !seen.insert(dependency_url.clone()) {
            continue;
        }
        let Some(ingot) = db.workspace().containing_ingot(db, dependency_url.clone()) else {
            continue;
        };
        if !ingot_has_source_files(db, ingot) {
            eprintln!("Error: Could not find source files for ingot {dependency_url}");
            has_errors = true;
            continue;
        }
        let diags = db.run_on_ingot(ingot);
        if !diags.is_empty() {
            dependency_errors.push((dependency_url, diags));
        }
    }

    if !dependency_errors.is_empty() {
        has_errors = true;
        if dependency_errors.len() == 1 {
            eprintln!("Error: Downstream ingot has errors");
        } else {
            eprintln!("Error: Downstream ingots have errors");
        }

        for (dependency_url, diags) in dependency_errors {
            print_dependency_info(db, &dependency_url);
            diags.emit(db);
        }
    }

    has_errors
}

fn ingot_has_source_files(db: &DriverDataBase, ingot: hir::Ingot<'_>) -> bool {
    ingot
        .files(db)
        .iter()
        .any(|(_, file)| matches!(file.kind(db), Some(IngotFileKind::Source)))
}

fn print_dependency_info(db: &DriverDataBase, dependency_url: &Url) {
    eprintln!();

    // Get the ingot for this dependency URL to access its config
    if let Some(ingot) = db.workspace().containing_ingot(db, dependency_url.clone()) {
        if let Some(config) = ingot.config(db) {
            let name = config.metadata.name.as_deref().unwrap_or("unknown");
            if let Some(version) = &config.metadata.version {
                eprintln!("- {name} (version: {version})");
            } else {
                eprintln!("- {name}");
            }
        } else {
            eprintln!("- Unknown dependency");
        }
    } else {
        eprintln!("- Unknown dependency");
    }

    eprintln!("  {dependency_url}");
    eprintln!();
}

fn emit_yul(db: &DriverDataBase, top_mod: TopLevelMod<'_>) {
    match emit_module_yul(db, top_mod) {
        Ok(yul) => {
            println!("=== Yul ===");
            println!("{yul}");
        }
        Err(err) => eprintln!("Warning: failed to emit Yul: {err}"),
    }
}

fn dump_module_mir(db: &DriverDataBase, top_mod: TopLevelMod<'_>) {
    match lower_module(db, top_mod) {
        Ok(mir_module) => {
            println!("=== MIR for module ===");
            print!("{}", mir::fmt::format_module(db, &mir_module));
        }
        Err(err) => eprintln!("failed to lower MIR: {err}"),
    }
}
