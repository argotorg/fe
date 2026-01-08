use std::collections::HashSet;

use camino::Utf8PathBuf;
use codegen::emit_module_yul;
use common::{InputDb, config::Manifest};
use driver::DriverDataBase;
use hir::hir_def::{HirIngot, TopLevelMod};
use mir::lower_module;
use url::Url;

pub fn check(path: &Utf8PathBuf, dump_mir: bool, emit_yul_min: bool) {
    let mut db = DriverDataBase::default();

    // Determine if we're dealing with a single file or an ingot directory
    let has_errors = if path.is_file() && path.extension() == Some("fe") {
        check_single_file(&mut db, path, dump_mir, emit_yul_min)
    } else if path.is_dir() {
        match manifest_at_path(path) {
            Ok(Some(Manifest::Workspace(_))) => {
                check_workspace(&mut db, path, dump_mir, emit_yul_min)
            }
            Ok(_) => check_ingot(&mut db, path, dump_mir, emit_yul_min),
            Err(err) => {
                eprintln!("❌ Error: {err}");
                true
            }
        }
    } else {
        eprintln!("❌ Error: Path must be either a .fe file or a directory containing fe.toml");
        std::process::exit(1);
    };

    if has_errors {
        std::process::exit(1);
    }
}

fn check_single_file(
    db: &mut DriverDataBase,
    file_path: &Utf8PathBuf,
    dump_mir: bool,
    emit_yul_min: bool,
) -> bool {
    // Create a file URL for the single .fe file
    let file_url = match Url::from_file_path(file_path.canonicalize_utf8().unwrap()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("❌ Error: Invalid file path: {file_path}");
            return true;
        }
    };

    // Read the file content
    let content = match std::fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file {file_path}: {err}");
            return true;
        }
    };

    // Add the file to the workspace
    db.workspace().touch(db, file_url.clone(), Some(content));

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
        eprintln!("❌ Error: Could not process file {file_path}");
        return true;
    }

    false
}

fn check_ingot(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    dump_mir: bool,
    emit_yul_min: bool,
) -> bool {
    let canonical_path = match dir_path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            eprintln!("Error: Invalid or non-existent directory path: {dir_path}");
            eprintln!("       Make sure the directory exists and is accessible");
            return true;
        }
    };

    let ingot_url = match Url::from_directory_path(canonical_path.as_str()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("❌ Error: Invalid directory path: {dir_path}");
            return true;
        }
    };
    let had_init_diagnostics = driver::init_ingot(db, &ingot_url);
    if had_init_diagnostics {
        return true;
    }

    if db
        .workspace()
        .containing_ingot(db, ingot_url.clone())
        .is_none()
    {
        // Check if the issue is a missing fe.toml file
        let config_url = match ingot_url.join("fe.toml") {
            Ok(url) => url,
            Err(_) => {
                eprintln!("❌ Error: Invalid ingot directory path");
                return true;
            }
        };

        if db.workspace().get(db, &config_url).is_none() {
            eprintln!("❌ Error: No fe.toml file found in the root directory");
            eprintln!("       Expected fe.toml at: {config_url}");
            eprintln!(
                "       Make sure you're in an fe project directory or create a fe.toml file"
            );
        } else {
            eprintln!("❌ Error: Could not resolve ingot from directory");
        }
        return true;
    }

    let mut seen = HashSet::new();
    check_ingot_and_dependencies(db, &ingot_url, dump_mir, emit_yul_min, &mut seen)
}

fn check_workspace(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    dump_mir: bool,
    emit_yul_min: bool,
) -> bool {
    let canonical_path = match dir_path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            eprintln!("Error: Invalid or non-existent directory path: {dir_path}");
            eprintln!("       Make sure the directory exists and is accessible");
            return true;
        }
    };

    let workspace_url = match Url::from_directory_path(canonical_path.as_str()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("❌ Error: Invalid directory path: {dir_path}");
            return true;
        }
    };

    let had_init_diagnostics = driver::init_ingot(db, &workspace_url);
    if had_init_diagnostics {
        return true;
    }

    let manifest = match manifest_at_path(dir_path) {
        Ok(Some(Manifest::Workspace(workspace_manifest))) => workspace_manifest,
        Ok(Some(Manifest::Ingot(_))) => {
            eprintln!("❌ Error: Expected a workspace manifest at {dir_path}");
            return true;
        }
        Ok(None) => {
            eprintln!("❌ Error: No fe.toml file found in the workspace root");
            return true;
        }
        Err(err) => {
            eprintln!("❌ Error: {err}");
            return true;
        }
    };

    let member_urls = match driver::workspace_member_urls(&manifest.workspace, &workspace_url) {
        Ok(members) => members,
        Err(err) => {
            eprintln!("❌ Error resolving workspace members: {err}");
            return true;
        }
    };

    if member_urls.is_empty() {
        eprintln!("⚠️  No workspace members found");
        return false;
    }

    let mut seen = HashSet::new();
    let mut has_errors = false;
    for member_url in member_urls {
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
        eprintln!("❌ Error: Could not resolve ingot {ingot_url}");
        return true;
    };

    if ingot.root_file(db).is_err() {
        eprintln!(
            "source files resolution error: `src` folder does not exist in the ingot directory"
        );
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
        let diags = db.run_on_ingot(ingot);
        if !diags.is_empty() {
            dependency_errors.push((dependency_url, diags));
        }
    }

    if !dependency_errors.is_empty() {
        has_errors = true;
        if dependency_errors.len() == 1 {
            eprintln!("❌ Error in downstream ingot");
        } else {
            eprintln!("❌ Errors in downstream ingots");
        }

        for (dependency_url, diags) in dependency_errors {
            print_dependency_info(db, &dependency_url);
            diags.emit(db);
        }
    }

    has_errors
}

fn manifest_at_path(path: &Utf8PathBuf) -> Result<Option<Manifest>, String> {
    let manifest_path = path.join("fe.toml");
    if !manifest_path.is_file() {
        return Ok(None);
    }
    let content = std::fs::read_to_string(manifest_path.as_std_path())
        .map_err(|err| format!("Failed to read {}: {err}", manifest_path))?;
    let manifest = Manifest::parse(&content)
        .map_err(|err| format!("Failed to parse {}: {err}", manifest_path))?;
    Ok(Some(manifest))
}

fn print_dependency_info(db: &DriverDataBase, dependency_url: &Url) {
    eprintln!();

    // Get the ingot for this dependency URL to access its config
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

fn emit_yul(db: &DriverDataBase, top_mod: TopLevelMod<'_>) {
    match emit_module_yul(db, top_mod) {
        Ok(yul) => {
            println!("=== Yul ===");
            println!("{yul}");
        }
        Err(err) => eprintln!("⚠️  failed to emit Yul: {err}"),
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
