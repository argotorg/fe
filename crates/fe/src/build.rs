use std::fs;

use camino::{Utf8Path, Utf8PathBuf};
use common::{
    InputDb,
    config::{Config, WorkspaceConfig},
    file::IngotFileKind,
};
use driver::DriverDataBase;
use hir::hir_def::{HirIngot, TopLevelMod};
use mir::{analysis::build_contract_graph, lower_module};
use resolver::files::ancestor_fe_toml_dirs;
use solc_runner::compile_single_contract;
use url::Url;

#[derive(Debug, Default, Clone, Copy)]
struct BuildSummary {
    had_errors: bool,
    built_contracts: usize,
}

pub fn build(
    path: &Utf8PathBuf,
    contract: Option<&str>,
    optimize: bool,
    out_dir: Option<&Utf8PathBuf>,
) {
    let mut db = DriverDataBase::default();

    let had_errors = if path.is_file() && path.extension() == Some("fe") {
        build_file(&mut db, path, contract, optimize, out_dir)
    } else if path.is_dir() {
        build_directory(&mut db, path, contract, optimize, out_dir)
    } else {
        eprintln!("❌ Error: Path must be either a .fe file or a directory containing fe.toml");
        true
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
) -> bool {
    let canonical = match file_path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            eprintln!("❌ Error: Invalid file path: {file_path}");
            return true;
        }
    };

    // If the file lives under an ingot (i.e. the nearest `fe.toml` is an `[ingot]` config),
    // build from that directory so imports resolve in context. For workspace roots, prefer treating
    // the file as standalone unless the user explicitly targets the workspace.
    if let Some(root) = ancestor_fe_toml_dirs(canonical.as_std_path())
        .first()
        .and_then(|root| Utf8PathBuf::from_path_buf(root.to_path_buf()).ok())
    {
        let config_path = root.join("fe.toml");
        if let Ok(content) = fs::read_to_string(&config_path)
            && matches!(Config::parse(&content), Ok(Config::Ingot(_)))
        {
            return build_directory(db, &root, contract, optimize, out_dir);
        }
    }

    let url = match Url::from_file_path(canonical.as_std_path()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("❌ Error: Invalid file path: {file_path}");
            return true;
        }
    };

    let content = match fs::read_to_string(&canonical) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("❌ Error reading file {file_path}: {err}");
            return true;
        }
    };

    db.workspace().touch(db, url.clone(), Some(content));

    let Some(file) = db.workspace().get(db, &url) else {
        eprintln!("❌ Error: Could not process file {file_path}");
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
    build_top_mod(db, top_mod, contract, optimize, &out_dir, true).had_errors
}

fn build_directory(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    contract: Option<&str>,
    optimize: bool,
    out_dir: Option<&Utf8PathBuf>,
) -> bool {
    let canonical = match dir_path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            eprintln!("❌ Error: Invalid or non-existent directory path: {dir_path}");
            return true;
        }
    };

    if !canonical.join("fe.toml").is_file() {
        eprintln!("❌ Error: No fe.toml file found in the provided directory: {canonical}");
        return true;
    }

    let url = match Url::from_directory_path(canonical.as_str()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("❌ Error: Invalid directory path: {dir_path}");
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
                eprintln!("❌ Error: Failed to parse {}/fe.toml: {err}", canonical);
                return true;
            }
        },
        Err(err) => {
            eprintln!("❌ Error: Failed to read {}/fe.toml: {err}", canonical);
            return true;
        }
    };

    match config {
        Config::Workspace(workspace) => {
            build_workspace(db, &canonical, url, *workspace, contract, optimize, out_dir)
        }
        Config::Ingot(_) => {
            let default_out_dir = canonical.join("out");
            let out_dir = out_dir.cloned().unwrap_or(default_out_dir);
            build_ingot_url(db, &url, contract, optimize, &out_dir, true).had_errors
        }
    }
}

fn build_workspace(
    db: &mut DriverDataBase,
    workspace_root: &Utf8PathBuf,
    workspace_url: Url,
    workspace_config: WorkspaceConfig,
    contract: Option<&str>,
    optimize: bool,
    out_dir: Option<&Utf8PathBuf>,
) -> bool {
    let members = match driver::workspace_members(&workspace_config.workspace, &workspace_url) {
        Ok(members) => members,
        Err(err) => {
            eprintln!("❌ Error resolving workspace members: {err}");
            return true;
        }
    };

    if members.is_empty() {
        eprintln!("⚠️  No workspace members found");
        return false;
    }

    let mut had_errors = false;
    let mut built_any = false;

    for member in members {
        if driver::init_ingot(db, &member.url) {
            had_errors = true;
            continue;
        }

        let member_out_dir = if let Some(base) = out_dir {
            base.join(&member.path)
        } else {
            member_root_dir(workspace_root, &member.path).join("out")
        };

        let summary = build_ingot_url(db, &member.url, contract, optimize, &member_out_dir, false);
        built_any |= summary.built_contracts > 0;
        had_errors |= summary.had_errors;
    }

    if contract.is_some() && !built_any {
        eprintln!("❌ Error: No workspace members contained the requested contract");
        return true;
    }

    had_errors
}

fn member_root_dir(workspace_root: &Utf8Path, member_path: &Utf8Path) -> Utf8PathBuf {
    if member_path.is_absolute() {
        member_path.to_path_buf()
    } else {
        workspace_root.join(member_path)
    }
}

fn build_ingot_url(
    db: &mut DriverDataBase,
    ingot_url: &Url,
    contract: Option<&str>,
    optimize: bool,
    out_dir: &Utf8Path,
    missing_contract_is_error: bool,
) -> BuildSummary {
    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        eprintln!("❌ Error: Could not resolve ingot from directory");
        return BuildSummary {
            had_errors: true,
            built_contracts: 0,
        };
    };

    if ingot.root_file(db).is_err() {
        eprintln!(
            "source files resolution error: `src` folder does not exist in the ingot directory"
        );
        return BuildSummary {
            had_errors: true,
            built_contracts: 0,
        };
    }

    if !ingot_has_source_files(db, ingot) {
        eprintln!("❌ Error: Could not find source files for ingot {ingot_url}");
        return BuildSummary {
            had_errors: true,
            built_contracts: 0,
        };
    }

    let diags = db.run_on_ingot(ingot);
    if !diags.is_empty() {
        diags.emit(db);
        return BuildSummary {
            had_errors: true,
            built_contracts: 0,
        };
    }

    let root_mod = ingot.root_mod(db);
    build_top_mod(
        db,
        root_mod,
        contract,
        optimize,
        out_dir,
        missing_contract_is_error,
    )
}

fn build_top_mod(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    contract: Option<&str>,
    optimize: bool,
    out_dir: &Utf8Path,
    missing_contract_is_error: bool,
) -> BuildSummary {
    let contract_names = match collect_contract_names(db, top_mod) {
        Ok(names) => names,
        Err(err) => {
            eprintln!("❌ Error: Failed to analyze contracts: {err}");
            return BuildSummary {
                had_errors: true,
                built_contracts: 0,
            };
        }
    };

    if contract_names.is_empty() {
        eprintln!("❌ Error: No contracts found to build");
        return BuildSummary {
            had_errors: true,
            built_contracts: 0,
        };
    }

    let names_to_build = if let Some(name) = contract {
        if contract_names.iter().any(|candidate| candidate == name) {
            vec![name.to_string()]
        } else {
            if missing_contract_is_error {
                eprintln!("❌ Error: Contract \"{name}\" not found");
                eprintln!("Available contracts:");
                for candidate in &contract_names {
                    eprintln!("  - {candidate}");
                }
                return BuildSummary {
                    had_errors: true,
                    built_contracts: 0,
                };
            }
            return BuildSummary {
                had_errors: false,
                built_contracts: 0,
            };
        }
    } else {
        contract_names
    };

    let yul = match codegen::emit_module_yul(db, top_mod) {
        Ok(yul) => yul,
        Err(err) => {
            eprintln!("❌ Error: Failed to emit Yul: {err}");
            return BuildSummary {
                had_errors: true,
                built_contracts: 0,
            };
        }
    };

    if let Err(err) = fs::create_dir_all(out_dir.as_std_path()) {
        eprintln!("❌ Error: Failed to create output directory {out_dir}: {err}");
        return BuildSummary {
            had_errors: true,
            built_contracts: 0,
        };
    }

    let mut had_errors = false;
    let mut built_contracts = 0;
    for name in &names_to_build {
        match compile_single_contract(name, &yul, optimize, true) {
            Ok(bytecode) => {
                if let Err(err) = write_artifacts(
                    out_dir,
                    name,
                    &bytecode.bytecode,
                    &bytecode.runtime_bytecode,
                ) {
                    eprintln!("❌ Error: {err}");
                    had_errors = true;
                } else {
                    println!("Wrote {}/{}.bin", out_dir, sanitize_filename(name));
                    println!("Wrote {}/{}.runtime.bin", out_dir, sanitize_filename(name));
                    built_contracts += 1;
                }
            }
            Err(err) => {
                eprintln!("❌ solc failed for contract \"{name}\": {}", err.0);
                eprintln!("Hint: install solc or set FE_SOLC_PATH to a solc binary.");
                had_errors = true;
            }
        }
    }

    BuildSummary {
        had_errors,
        built_contracts,
    }
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
