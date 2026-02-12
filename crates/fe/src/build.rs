use std::{collections::BTreeMap, fs};

use camino::{Utf8Path, Utf8PathBuf};
use common::{
    InputDb,
    config::{Config, WorkspaceConfig},
    dependencies::WorkspaceMemberRecord,
    file::IngotFileKind,
};
use driver::DriverDataBase;
use driver::cli_target::{CliTarget, resolve_cli_target};
use hir::hir_def::{HirIngot, TopLevelMod};
use mir::{analysis::build_contract_graph, lower_module};
use smol_str::SmolStr;
use solc_runner::compile_single_contract_with_solc;
use url::Url;

#[derive(Debug, Default, Clone, Copy)]
struct BuildSummary {
    had_errors: bool,
}

pub fn build(
    path: &Utf8PathBuf,
    contract: Option<&str>,
    optimize: bool,
    out_dir: Option<&Utf8PathBuf>,
    solc: Option<&str>,
) {
    let mut db = DriverDataBase::default();

    let target = match resolve_cli_target(&mut db, path) {
        Ok(target) => target,
        Err(message) => {
            eprintln!("Error: {message}");
            std::process::exit(1);
        }
    };

    let had_errors = match target {
        CliTarget::StandaloneFile(file_path) => {
            build_file(&mut db, &file_path, contract, optimize, out_dir, solc)
        }
        CliTarget::Directory(dir_path) => {
            build_directory(&mut db, &dir_path, contract, optimize, out_dir, solc)
        }
    };

    if had_errors {
        std::process::exit(1);
    }
}

fn build_file(
    db: &mut DriverDataBase,
    file_path: &Utf8PathBuf,
    contract: Option<&str>,
    optimize: bool,
    out_dir: Option<&Utf8PathBuf>,
    solc: Option<&str>,
) -> bool {
    let canonical = match file_path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            eprintln!("Error: Invalid file path: {file_path}");
            return true;
        }
    };

    let url = match Url::from_file_path(canonical.as_std_path()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid file path: {file_path}");
            return true;
        }
    };

    let content = match fs::read_to_string(&canonical) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error: Failed to read file {file_path}: {err}");
            return true;
        }
    };

    db.workspace().touch(db, url.clone(), Some(content));

    let Some(file) = db.workspace().get(db, &url) else {
        eprintln!("Error: Could not process file {file_path}");
        return true;
    };

    let top_mod = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    if !diags.is_empty() {
        diags.emit(db);
        return true;
    }

    let default_out_dir = canonical
        .parent()
        .map(|parent| parent.join("out"))
        .unwrap_or_else(|| Utf8PathBuf::from("out"));
    let out_dir = out_dir.cloned().unwrap_or(default_out_dir);
    build_top_mod(db, top_mod, contract, optimize, &out_dir, true, solc).had_errors
}

fn build_directory(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    contract: Option<&str>,
    optimize: bool,
    out_dir: Option<&Utf8PathBuf>,
    solc: Option<&str>,
) -> bool {
    let canonical = match dir_path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            eprintln!("Error: Invalid or non-existent directory path: {dir_path}");
            return true;
        }
    };

    if !canonical.join("fe.toml").is_file() {
        eprintln!("Error: No fe.toml file found in the provided directory: {canonical}");
        return true;
    }

    let url = match Url::from_directory_path(canonical.as_str()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid directory path: {dir_path}");
            return true;
        }
    };

    if driver::init_ingot(db, &url) {
        return true;
    }

    let config = match fs::read_to_string(canonical.join("fe.toml")) {
        Ok(content) => match Config::parse(&content) {
            Ok(config) => config,
            Err(err) => {
                eprintln!("Error: Failed to parse {}/fe.toml: {err}", canonical);
                return true;
            }
        },
        Err(err) => {
            eprintln!("Error: Failed to read {}/fe.toml: {err}", canonical);
            return true;
        }
    };

    match config {
        Config::Workspace(workspace) => build_workspace(
            db, &canonical, url, *workspace, contract, optimize, out_dir, solc,
        ),
        Config::Ingot(_) => {
            let default_out_dir = canonical.join("out");
            let out_dir = out_dir.cloned().unwrap_or(default_out_dir);
            build_ingot_url(db, &url, contract, optimize, &out_dir, true, solc).had_errors
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn build_workspace(
    db: &mut DriverDataBase,
    workspace_root: &Utf8PathBuf,
    workspace_url: Url,
    _workspace_config: WorkspaceConfig,
    contract: Option<&str>,
    optimize: bool,
    out_dir: Option<&Utf8PathBuf>,
    solc: Option<&str>,
) -> bool {
    let mut members = db
        .dependency_graph()
        .workspace_member_records(db, &workspace_url);
    members.sort_by(|a, b| a.path.cmp(&b.path));

    if members.is_empty() {
        eprintln!("Warning: No workspace members found");
        return false;
    }

    let out_dir = out_dir
        .cloned()
        .unwrap_or_else(|| workspace_root.join("out"));

    let mut contract_names_by_member = Vec::with_capacity(members.len());
    for member in members {
        let contract_names = match analyze_ingot_contract_names(db, &member.url) {
            Ok(names) => names,
            Err(()) => return true,
        };
        contract_names_by_member.push((member, contract_names));
    }

    if let Some(contract) = contract {
        let matches: Vec<_> = contract_names_by_member
            .iter()
            .filter(|(_, names)| names.iter().any(|name| name == contract))
            .map(|(member, _)| member)
            .collect();

        match matches.len() {
            0 => {
                eprintln!("Error: No workspace members contained the requested contract");
                return true;
            }
            1 => {
                let summary = build_ingot_url(
                    db,
                    &matches[0].url,
                    Some(contract),
                    optimize,
                    &out_dir,
                    true,
                    solc,
                );
                return summary.had_errors;
            }
            _ => {
                eprintln!(
                    "Error: Contract \"{contract}\" is defined in multiple workspace members"
                );
                eprintln!("Matches:");
                for member in matches {
                    eprintln!("  - {} ({})", member.name, member.path);
                }
                eprintln!("Hint: build a specific member by name or path instead.");
                return true;
            }
        }
    }

    if let Err(()) = check_workspace_artifact_name_collisions(&contract_names_by_member) {
        return true;
    }

    let mut had_errors = false;
    for (member, _) in contract_names_by_member {
        let summary = build_ingot_url(db, &member.url, None, optimize, &out_dir, true, solc);
        had_errors |= summary.had_errors;
    }

    had_errors
}

fn analyze_ingot_contract_names(
    db: &mut DriverDataBase,
    ingot_url: &Url,
) -> Result<Vec<String>, ()> {
    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        eprintln!("Error: Could not resolve ingot from directory");
        return Err(());
    };

    if ingot.root_file(db).is_err() {
        eprintln!("Error: `src` folder does not exist in the ingot directory");
        return Err(());
    }

    if !ingot_has_source_files(db, ingot) {
        eprintln!("Error: Could not find source files for ingot {ingot_url}");
        return Err(());
    }

    let diags = db.run_on_ingot(ingot);
    if !diags.is_empty() {
        diags.emit(db);
        return Err(());
    }

    let root_mod = ingot.root_mod(db);
    let contract_names = collect_contract_names(db, root_mod).map_err(|err| {
        eprintln!("Error: Failed to analyze contracts: {err}");
    })?;

    Ok(contract_names)
}

fn check_workspace_artifact_name_collisions(
    contract_names_by_member: &[(WorkspaceMemberRecord, Vec<String>)],
) -> Result<(), ()> {
    let mut collisions: BTreeMap<String, Vec<(SmolStr, Utf8PathBuf, String)>> = BTreeMap::new();
    for (member, contract_names) in contract_names_by_member {
        for name in contract_names {
            let artifact = sanitize_filename(name);
            collisions.entry(artifact).or_default().push((
                member.name.clone(),
                member.path.clone(),
                name.clone(),
            ));
        }
    }

    let duplicates: Vec<_> = collisions
        .into_iter()
        .filter(|(_, entries)| entries.len() > 1)
        .collect();

    if duplicates.is_empty() {
        return Ok(());
    }

    eprintln!("Error: Contract names collide in a flat workspace output directory");
    eprintln!("Conflicts:");
    for (artifact, entries) in duplicates {
        let mut labels: Vec<String> = entries
            .into_iter()
            .map(|(member_name, member_path, contract_name)| {
                format!("{contract_name} in {member_name} ({member_path})")
            })
            .collect();
        labels.sort();
        eprintln!("  - {artifact}");
        for label in labels {
            eprintln!("    - {label}");
        }
    }
    eprintln!("Hint: build a specific member by name or path instead.");
    Err(())
}

fn build_ingot_url(
    db: &mut DriverDataBase,
    ingot_url: &Url,
    contract: Option<&str>,
    optimize: bool,
    out_dir: &Utf8Path,
    missing_contract_is_error: bool,
    solc: Option<&str>,
) -> BuildSummary {
    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        eprintln!("Error: Could not resolve ingot from directory");
        return BuildSummary { had_errors: true };
    };

    if ingot.root_file(db).is_err() {
        eprintln!("Error: `src` folder does not exist in the ingot directory");
        return BuildSummary { had_errors: true };
    }

    if !ingot_has_source_files(db, ingot) {
        eprintln!("Error: Could not find source files for ingot {ingot_url}");
        return BuildSummary { had_errors: true };
    }

    let diags = db.run_on_ingot(ingot);
    if !diags.is_empty() {
        diags.emit(db);
        return BuildSummary { had_errors: true };
    }

    let root_mod = ingot.root_mod(db);
    build_top_mod(
        db,
        root_mod,
        contract,
        optimize,
        out_dir,
        missing_contract_is_error,
        solc,
    )
}

fn build_top_mod(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    contract: Option<&str>,
    optimize: bool,
    out_dir: &Utf8Path,
    missing_contract_is_error: bool,
    solc: Option<&str>,
) -> BuildSummary {
    let contract_names = match collect_contract_names(db, top_mod) {
        Ok(names) => names,
        Err(err) => {
            eprintln!("Error: Failed to analyze contracts: {err}");
            return BuildSummary { had_errors: true };
        }
    };

    if contract_names.is_empty() {
        eprintln!("Error: No contracts found to build");
        return BuildSummary { had_errors: true };
    }

    let names_to_build = if let Some(name) = contract {
        if contract_names.iter().any(|candidate| candidate == name) {
            vec![name.to_string()]
        } else {
            if missing_contract_is_error {
                eprintln!("Error: Contract \"{name}\" not found");
                eprintln!("Available contracts:");
                for candidate in &contract_names {
                    eprintln!("  - {candidate}");
                }
                return BuildSummary { had_errors: true };
            }
            return BuildSummary { had_errors: false };
        }
    } else {
        contract_names
    };

    let yul = match codegen::emit_module_yul(db, top_mod) {
        Ok(yul) => yul,
        Err(err) => {
            eprintln!("Error: Failed to emit Yul: {err}");
            return BuildSummary { had_errors: true };
        }
    };

    if let Err(err) = fs::create_dir_all(out_dir.as_std_path()) {
        eprintln!("Error: Failed to create output directory {out_dir}: {err}");
        return BuildSummary { had_errors: true };
    }

    let mut had_errors = false;
    for name in &names_to_build {
        match compile_single_contract_with_solc(name, &yul, optimize, true, solc) {
            Ok(bytecode) => {
                if let Err(err) = write_artifacts(
                    out_dir,
                    name,
                    &bytecode.bytecode,
                    &bytecode.runtime_bytecode,
                ) {
                    eprintln!("Error: {err}");
                    had_errors = true;
                } else {
                    println!("Wrote {}/{}.bin", out_dir, sanitize_filename(name));
                    println!("Wrote {}/{}.runtime.bin", out_dir, sanitize_filename(name));
                }
            }
            Err(err) => {
                eprintln!("Error: solc failed for contract \"{name}\": {}", err.0);
                eprintln!("Hint: install solc, set FE_SOLC_PATH, or pass --solc <path>.");
                had_errors = true;
            }
        }
    }

    BuildSummary { had_errors }
}

fn collect_contract_names(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
) -> Result<Vec<String>, String> {
    let module = lower_module(db, top_mod).map_err(|err| err.to_string())?;
    let graph = build_contract_graph(&module.functions);
    let mut names: Vec<_> = graph.contracts.keys().cloned().collect();
    names.sort();
    Ok(names)
}

fn write_artifacts(
    out_dir: &Utf8Path,
    contract_name: &str,
    bytecode: &str,
    runtime_bytecode: &str,
) -> Result<(), String> {
    let base = sanitize_filename(contract_name);
    let deploy_path = out_dir.join(format!("{base}.bin"));
    let runtime_path = out_dir.join(format!("{base}.runtime.bin"));
    fs::write(deploy_path.as_std_path(), format!("{bytecode}\n"))
        .map_err(|err| format!("Failed to write {deploy_path}: {err}"))?;
    fs::write(runtime_path.as_std_path(), format!("{runtime_bytecode}\n"))
        .map_err(|err| format!("Failed to write {runtime_path}: {err}"))?;
    Ok(())
}

fn sanitize_filename(name: &str) -> String {
    let sanitized: String = name
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' || c == '-' {
                c
            } else {
                '_'
            }
        })
        .collect();

    if sanitized.is_empty() {
        "contract".into()
    } else {
        sanitized
    }
}

fn ingot_has_source_files(db: &DriverDataBase, ingot: hir::Ingot<'_>) -> bool {
    ingot
        .files(db)
        .iter()
        .any(|(_, file)| matches!(file.kind(db), Some(IngotFileKind::Source)))
}
