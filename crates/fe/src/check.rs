use camino::Utf8PathBuf;
use codegen::emit_module_yul;
use common::InputDb;
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
        check_ingot(&mut db, path, dump_mir, emit_yul_min)
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

    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
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
    };

    // Check if the ingot has source files before trying to analyze
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

    // Collect all dependencies with errors
    let mut dependency_errors = Vec::new();
    for dependency_url in db.dependency_graph().dependency_urls(db, &ingot_url) {
        let Some(ingot) = db.workspace().containing_ingot(db, dependency_url.clone()) else {
            // Skip dependencies that can't be resolved
            continue;
        };
        let diags = db.run_on_ingot(ingot);
        if !diags.is_empty() {
            dependency_errors.push((dependency_url, diags));
        }
    }

    // Print dependency errors if any exist
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
