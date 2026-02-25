use std::collections::HashSet;

use camino::Utf8PathBuf;
use common::{
    InputDb,
    config::{Config, WorkspaceConfig},
    file::IngotFileKind,
};
use driver::DriverDataBase;
use driver::cli_target::{CliTarget, resolve_cli_target};
use hir::hir_def::{HirIngot, TopLevelMod};
use mir::{MirDiagnosticsMode, collect_mir_diagnostics, fmt as mir_fmt, lower_module};
use url::Url;

use crate::report::{
    copy_input_into_report, create_dir_all_utf8, create_report_staging_dir, enable_panic_report,
    normalize_report_out_path, tar_gz_dir, write_report_meta,
};
use crate::workspace_ingot::{
    INGOT_REQUIRES_WORKSPACE_ROOT, WorkspaceMemberRef, select_workspace_member_paths,
};

#[derive(Debug, Clone)]
struct ReportContext {
    root_dir: Utf8PathBuf,
}

fn write_report_file(report: &ReportContext, rel: &str, contents: &str) {
    let path = report.root_dir.join(rel);
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    let _ = std::fs::write(path, contents);
}

#[allow(clippy::too_many_arguments)]
pub fn check(
    path: &Utf8PathBuf,
    ingot: Option<&str>,
    force_standalone: bool,
    dump_mir: bool,
    report_out: Option<&Utf8PathBuf>,
    report_failed_only: bool,
) -> Result<bool, String> {
    let mut db = DriverDataBase::default();

    let report_root = report_out
        .map(|out| -> Result<_, String> {
            let staging = create_report_staging_dir("target/fe-check-report-staging")?;
            let out = normalize_report_out_path(out)?;
            Ok((out, staging))
        })
        .transpose()?;

    let report_ctx = report_root
        .as_ref()
        .map(|(_, staging)| -> Result<_, String> {
            let inputs_dir = staging.join("inputs");
            create_dir_all_utf8(&inputs_dir)?;
            create_dir_all_utf8(&staging.join("artifacts"))?;
            create_dir_all_utf8(&staging.join("errors"))?;
            write_report_meta(staging, "fe check report", None);
            Ok(ReportContext {
                root_dir: staging.clone(),
            })
        })
        .transpose()?;

    let target = match resolve_cli_target(&mut db, path, force_standalone) {
        Ok(target) => target,
        Err(message) => {
            if let Some(report) = report_ctx.as_ref() {
                write_report_file(report, "errors/cli_target.txt", &format!("{message}\n"));
            }

            if let Some((out, staging)) = report_root {
                let has_errors = true;
                let should_write = !report_failed_only || has_errors;
                if should_write {
                    write_check_manifest(&staging, path, dump_mir, has_errors);
                    if let Err(err) = tar_gz_dir(&staging, &out) {
                        eprintln!("Error: failed to write report `{out}`: {err}");
                        eprintln!("Report staging directory left at `{staging}`");
                    } else {
                        let _ = std::fs::remove_dir_all(&staging);
                        println!("wrote report: {out}");
                    }
                } else {
                    let _ = std::fs::remove_dir_all(&staging);
                }
            }

            return Err(message);
        }
    };

    if let Some(report) = report_ctx.as_ref() {
        let inputs_dir = report.root_dir.join("inputs");
        let source = match &target {
            CliTarget::StandaloneFile(file) => file,
            CliTarget::Directory(dir) => dir,
        };
        if let Err(err) = copy_input_into_report(source, &inputs_dir) {
            write_report_file(report, "errors/report_inputs.txt", &format!("{err}\n"));
        }
    }

    let _panic_guard = report_ctx
        .as_ref()
        .map(|report| enable_panic_report(report.root_dir.join("errors/panic_full.txt")));

    let has_errors = match target {
        CliTarget::StandaloneFile(file_path) => {
            if ingot.is_some() {
                eprintln!("Error: {INGOT_REQUIRES_WORKSPACE_ROOT}");
                true
            } else {
                check_single_file(&mut db, &file_path, dump_mir, report_ctx.as_ref())
            }
        }
        CliTarget::Directory(dir_path) => {
            check_directory(&mut db, &dir_path, ingot, dump_mir, report_ctx.as_ref())
        }
    };

    if let Some((out, staging)) = report_root {
        let should_write = !report_failed_only || has_errors;
        if should_write {
            write_check_manifest(&staging, path, dump_mir, has_errors);
            if let Err(err) = tar_gz_dir(&staging, &out) {
                eprintln!("Error: failed to write report `{out}`: {err}");
                eprintln!("Report staging directory left at `{staging}`");
            } else {
                let _ = std::fs::remove_dir_all(&staging);
                println!("wrote report: {out}");
            }
        } else {
            let _ = std::fs::remove_dir_all(&staging);
        }
    }

    Ok(has_errors)
}

#[allow(clippy::too_many_arguments)]
fn check_directory(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    ingot: Option<&str>,
    dump_mir: bool,
    report: Option<&ReportContext>,
) -> bool {
    let ingot_url = match dir_url(dir_path) {
        Ok(url) => url,
        Err(message) => {
            eprintln!("{message}");
            return true;
        }
    };

    let had_init_diagnostics = driver::init_ingot(db, &ingot_url);
    if had_init_diagnostics {
        if let Some(report) = report {
            write_report_file(
                report,
                "errors/diagnostics.txt",
                "compilation errors while initializing ingot",
            );
        }
        return true;
    }

    let config = match config_from_db(db, &ingot_url) {
        Ok(Some(config)) => config,
        Ok(None) => {
            if ingot.is_some() {
                eprintln!("Error: {INGOT_REQUIRES_WORKSPACE_ROOT}");
                return true;
            }
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
            check_workspace(db, dir_path, *workspace, ingot, dump_mir, report)
        }
        Config::Ingot(_) => {
            if ingot.is_some() {
                eprintln!("Error: {INGOT_REQUIRES_WORKSPACE_ROOT}");
                return true;
            }
            check_ingot_url(db, &ingot_url, dump_mir, report)
        }
    }
}

fn config_from_db(db: &DriverDataBase, ingot_url: &Url) -> Result<Option<Config>, String> {
    let config_url = ingot_url
        .join("fe.toml")
        .map_err(|_| format!("Failed to locate fe.toml for {ingot_url}"))?;
    let Some(file) = db.workspace().get(db, &config_url) else {
        return Ok(None);
    };
    let config = Config::parse(file.text(db))
        .map_err(|err| format!("Failed to parse {config_url}: {err}"))?;
    Ok(Some(config))
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
        .map_err(|_| format!("Error: invalid or non-existent directory path: {path}"))
}

#[allow(clippy::too_many_arguments)]
fn check_ingot_url(
    db: &mut DriverDataBase,
    ingot_url: &Url,
    dump_mir: bool,
    report: Option<&ReportContext>,
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
    check_ingot_and_dependencies(db, ingot_url, dump_mir, report, &mut seen)
}

#[allow(clippy::too_many_arguments)]
fn check_workspace(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    workspace_config: WorkspaceConfig,
    ingot: Option<&str>,
    dump_mir: bool,
    report: Option<&ReportContext>,
) -> bool {
    let workspace_url = match dir_url(dir_path) {
        Ok(url) => url,
        Err(message) => {
            eprintln!("{message}");
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

    let selected_member_paths = match select_workspace_member_paths(
        dir_path,
        dir_path,
        members
            .iter()
            .map(|member| WorkspaceMemberRef::new(member.path.as_path(), member.name.as_deref())),
        ingot,
    ) {
        Ok(paths) => paths,
        Err(err) => {
            eprintln!("Error: {err}");
            return true;
        }
    };
    let selected_member_paths: HashSet<Utf8PathBuf> = selected_member_paths.into_iter().collect();

    let mut seen = HashSet::new();
    let mut has_errors = false;
    for member in members {
        let member_path = dir_path.join(member.path.as_str());
        if !selected_member_paths.contains(&member_path) {
            continue;
        }
        let member_url = member.url;
        let member_has_errors =
            check_ingot_and_dependencies(db, &member_url, dump_mir, report, &mut seen);
        has_errors |= member_has_errors;
    }

    has_errors
}

#[allow(clippy::too_many_arguments)]
fn check_ingot_and_dependencies(
    db: &mut DriverDataBase,
    ingot_url: &Url,
    dump_mir: bool,
    report: Option<&ReportContext>,
    seen: &mut HashSet<Url>,
) -> bool {
    if !seen.insert(ingot_url.clone()) {
        return false;
    }

    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        eprintln!("Error: Could not resolve ingot {ingot_url}");
        return true;
    };

    if !ingot_has_source_files(db, ingot) {
        eprintln!("Error: Could not find source files for ingot {ingot_url}");
        return true;
    }

    let hir_diags = db.run_on_ingot(ingot);
    let mut has_errors = false;

    if !hir_diags.is_empty() {
        hir_diags.emit(db);
        if let Some(report) = report {
            let formatted = hir_diags.format_diags(db);
            write_report_file(report, "errors/diagnostics.txt", &formatted);
        }
        has_errors = true;
    }

    let mir_diags = db.mir_diagnostics_for_ingot(ingot, MirDiagnosticsMode::CompilerParity);
    if !mir_diags.is_empty() {
        db.emit_complete_diagnostics(&mir_diags);
        has_errors = true;
    }

    if !has_errors {
        let root_mod = ingot.root_mod(db);
        if dump_mir {
            dump_module_mir(db, root_mod);
        }
        if let Some(report) = report {
            write_check_artifacts(db, root_mod, report);
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
        let hir_diags = db.run_on_ingot(ingot);
        let mir_diags = db.mir_diagnostics_for_ingot(ingot, MirDiagnosticsMode::CompilerParity);
        if !hir_diags.is_empty() || !mir_diags.is_empty() {
            dependency_errors.push((dependency_url, hir_diags, mir_diags));
        }
    }

    if !dependency_errors.is_empty() {
        has_errors = true;
        if dependency_errors.len() == 1 {
            eprintln!("Error: Downstream ingot has errors");
        } else {
            eprintln!("Error: Downstream ingots have errors");
        }

        if let Some(report) = report {
            let mut out = String::new();
            for (dependency_url, hir_diags, mir_diags) in &dependency_errors {
                out.push_str(&format!("dependency: {dependency_url}\n"));
                if !hir_diags.is_empty() {
                    out.push_str(&hir_diags.format_diags(db));
                }
                if !mir_diags.is_empty() {
                    out.push_str(&format!(
                        "MIR diagnostics: {} emitted to stderr\n",
                        mir_diags.len()
                    ));
                }
                out.push('\n');
            }
            write_report_file(report, "errors/dependency_diagnostics.txt", &out);
        }

        for (dependency_url, hir_diags, mir_diags) in dependency_errors {
            print_dependency_info(db, &dependency_url);
            if !hir_diags.is_empty() {
                hir_diags.emit(db);
            }
            if !mir_diags.is_empty() {
                db.emit_complete_diagnostics(&mir_diags);
            }
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

#[allow(clippy::too_many_arguments)]
fn check_single_file(
    db: &mut DriverDataBase,
    file_path: &Utf8PathBuf,
    dump_mir: bool,
    report: Option<&ReportContext>,
) -> bool {
    // Create a file URL for the single .fe file
    let canonical = match file_path.canonicalize_utf8() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Error: Cannot canonicalize {file_path}: {e}");
            return true;
        }
    };
    let file_url = match Url::from_file_path(&canonical) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid file path: {file_path}");
            return true;
        }
    };

    // Read the file content
    let content = match std::fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error: Failed to read file {file_path}: {err}");
            return true;
        }
    };

    // Add the file to the workspace
    db.workspace().touch(db, file_url.clone(), Some(content));

    // Try to get the file and check it for errors
    if let Some(file) = db.workspace().get(db, &file_url) {
        let top_mod = db.top_mod(file);
        let hir_diags = db.run_on_top_mod(top_mod);
        let mut has_errors = false;

        if !hir_diags.is_empty() {
            eprintln!("errors in {file_url}");
            eprintln!();
            hir_diags.emit(db);
            if let Some(report) = report {
                let formatted = hir_diags.format_diags(db);
                write_report_file(report, "errors/diagnostics.txt", &formatted);
            }
            has_errors = true;
        }

        let mir_output = collect_mir_diagnostics(db, top_mod, MirDiagnosticsMode::CompilerParity);
        if !mir_output.diagnostics.is_empty() {
            if !has_errors {
                eprintln!("errors in {file_url}");
                eprintln!();
            }
            db.emit_complete_diagnostics(&mir_output.diagnostics);
            has_errors = true;
        }

        if has_errors {
            return true;
        }
        if dump_mir {
            dump_module_mir(db, top_mod);
        }
        if let Some(report) = report {
            write_check_artifacts(db, top_mod, report);
        }
    } else {
        eprintln!("Error: Could not process file {file_path}");
        return true;
    }

    false
}

fn print_dependency_info(db: &DriverDataBase, dependency_url: &Url) {
    eprintln!();

    // Get the ingot for this dependency URL to access its config
    if let Some(ingot) = db.workspace().containing_ingot(db, dependency_url.clone()) {
        if let Some(config) = ingot.config(db) {
            let name = config.metadata.name.as_deref().unwrap_or("unknown");
            if let Some(version) = &config.metadata.version {
                eprintln!("Dependency: {name} (version: {version})");
            } else {
                eprintln!("Dependency: {name}");
            }
        } else {
            eprintln!("Dependency: <unknown>");
        }
    } else {
        eprintln!("Dependency: <unknown>");
    }

    eprintln!("URL: {dependency_url}");
    eprintln!();
}

fn dump_module_mir(db: &DriverDataBase, top_mod: TopLevelMod<'_>) {
    match lower_module(db, top_mod) {
        Ok(mir_module) => {
            println!("=== MIR for module ===");
            print!("{}", mir_fmt::format_module(db, &mir_module));
        }
        Err(err) => eprintln!("failed to lower MIR: {err}"),
    }
}

fn write_check_manifest(
    staging: &Utf8PathBuf,
    path: &Utf8PathBuf,
    dump_mir: bool,
    has_errors: bool,
) {
    let mut out = String::new();
    out.push_str("fe check report\n");
    out.push_str(&format!("path: {path}\n"));
    out.push_str(&format!("dump_mir: {dump_mir}\n"));
    out.push_str(&format!(
        "status: {}\n",
        if has_errors { "failed" } else { "ok" }
    ));
    out.push_str(&format!("fe_version: {}\n", env!("CARGO_PKG_VERSION")));
    let _ = std::fs::write(staging.join("manifest.txt"), out);
}

fn write_check_artifacts(db: &DriverDataBase, top_mod: TopLevelMod<'_>, report: &ReportContext) {
    match lower_module(db, top_mod) {
        Ok(mir) => {
            write_report_file(
                report,
                "artifacts/mir.txt",
                &mir_fmt::format_module(db, &mir),
            );
        }
        Err(err) => {
            write_report_file(report, "artifacts/mir_error.txt", &format!("{err}"));
        }
    }
}
