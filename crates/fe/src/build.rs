use std::{collections::BTreeMap, fs};

use camino::{Utf8Path, Utf8PathBuf};
use codegen::{BackendKind, OptLevel, SonatinaContractBytecode};
use common::{
    InputDb,
    config::{Config, WorkspaceConfig},
    dependencies::WorkspaceMemberRecord,
    file::IngotFileKind,
};
use driver::DriverDataBase;
use driver::cli_target::{CliTarget, resolve_cli_target};
use hir::hir_def::TopLevelMod;
use mir::{analysis::build_contract_graph, fmt as mir_fmt, lower_ingot, lower_module};
use smol_str::SmolStr;
use solc_runner::compile_single_contract_with_solc;
use url::Url;

use crate::{
    BuildEmit,
    report::{
        ReportStaging, copy_input_into_report, create_dir_all_utf8, create_report_staging_root,
        enable_panic_report, normalize_report_out_path, tar_gz_dir, write_report_meta,
    },
};

#[derive(Debug, Default, Clone, Copy)]
struct BuildSummary {
    had_errors: bool,
}

#[derive(Debug, Clone)]
struct BuildReportContext {
    root_dir: Utf8PathBuf,
}

#[derive(Debug, Clone, Copy)]
struct EmitSelection {
    bytecode: bool,
    runtime_bytecode: bool,
    ir: bool,
}

impl EmitSelection {
    fn from_requested(requested: &[BuildEmit]) -> Self {
        let mut selection = Self {
            bytecode: false,
            runtime_bytecode: false,
            ir: false,
        };
        for emit in requested {
            match emit {
                BuildEmit::Bytecode => selection.bytecode = true,
                BuildEmit::RuntimeBytecode => selection.runtime_bytecode = true,
                BuildEmit::Ir => selection.ir = true,
            }
        }
        selection
    }

    fn writes_any_bytecode(self) -> bool {
        self.bytecode || self.runtime_bytecode
    }
}

fn create_build_report_staging() -> Result<ReportStaging, String> {
    create_report_staging_root("target/fe-build-report-staging", "fe-build-report")
}

fn write_report_file(report: &BuildReportContext, rel: &str, contents: &str) {
    let path = report.root_dir.join(rel);
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent.as_std_path());
    }
    let _ = std::fs::write(path.as_std_path(), contents);
}

#[allow(clippy::too_many_arguments)]
fn write_build_manifest(
    report: &BuildReportContext,
    path: &Utf8PathBuf,
    force_standalone: bool,
    contract: Option<&str>,
    backend_kind: BackendKind,
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: Option<&Utf8PathBuf>,
    solc: Option<&str>,
    has_errors: bool,
) {
    let mut out = String::new();
    out.push_str("fe build report\n");
    out.push_str(&format!("path: {path}\n"));
    out.push_str(&format!("standalone: {force_standalone}\n"));
    out.push_str(&format!("contract: {}\n", contract.unwrap_or("<all>")));
    out.push_str(&format!("backend: {}\n", backend_kind.name()));
    out.push_str(&format!("opt_level: {opt_level}\n"));
    out.push_str(&format!("emit: {}\n", describe_emit_selection(emit)));
    out.push_str(&format!(
        "out_dir: {}\n",
        out_dir.map(|p| p.as_str()).unwrap_or("<default>")
    ));
    out.push_str(&format!("solc: {}\n", solc.unwrap_or("<default>")));
    out.push_str(&format!(
        "status: {}\n",
        if has_errors { "failed" } else { "ok" }
    ));
    out.push_str(&format!("fe_version: {}\n", env!("CARGO_PKG_VERSION")));
    write_report_file(report, "manifest.txt", &out);
}

fn report_scope_dir(report: Option<&BuildReportContext>, scope: &str) -> Option<Utf8PathBuf> {
    let report = report?;
    let dir = report
        .root_dir
        .join("artifacts")
        .join(sanitize_filename(scope));
    let _ = create_dir_all_utf8(&dir);
    Some(dir)
}

#[allow(clippy::too_many_arguments)]
pub fn build(
    path: &Utf8PathBuf,
    force_standalone: bool,
    contract: Option<&str>,
    backend_kind: BackendKind,
    opt_level: OptLevel,
    emit: &[BuildEmit],
    out_dir: Option<&Utf8PathBuf>,
    solc: Option<&str>,
    report_out: Option<&Utf8PathBuf>,
    report_failed_only: bool,
) {
    let emit = EmitSelection::from_requested(emit);
    let mut db = DriverDataBase::default();

    let report_root = match report_out
        .map(|out| -> Result<_, String> {
            let staging = create_build_report_staging()?;
            let out = normalize_report_out_path(out)?;
            Ok((out, staging))
        })
        .transpose()
    {
        Ok(v) => v,
        Err(err) => {
            eprintln!("Error: {err}");
            std::process::exit(1);
        }
    };

    let report_ctx = match report_root
        .as_ref()
        .map(|(_, staging)| -> Result<_, String> {
            let root = &staging.root_dir;
            create_dir_all_utf8(&root.join("inputs"))?;
            create_dir_all_utf8(&root.join("artifacts"))?;
            create_dir_all_utf8(&root.join("errors"))?;
            write_report_meta(root, "fe build report", None);
            Ok(BuildReportContext {
                root_dir: root.clone(),
            })
        })
        .transpose()
    {
        Ok(v) => v,
        Err(err) => {
            eprintln!("Error: {err}");
            std::process::exit(1);
        }
    };

    let _panic_guard = report_ctx
        .as_ref()
        .map(|report| enable_panic_report(report.root_dir.join("errors/panic_full.txt")));

    let target = match resolve_cli_target(&mut db, path, force_standalone) {
        Ok(target) => target,
        Err(message) => {
            eprintln!("Error: {message}");
            if let Some(report) = report_ctx.as_ref() {
                write_report_file(report, "errors/cli_target.txt", &format!("{message}\n"));
            }
            if let Some((out, staging)) = report_root {
                let has_errors = true;
                if report_ctx.as_ref().is_some() && (!report_failed_only || has_errors) {
                    write_build_manifest(
                        report_ctx.as_ref().expect("report ctx"),
                        path,
                        force_standalone,
                        contract,
                        backend_kind,
                        opt_level,
                        emit,
                        out_dir,
                        solc,
                        has_errors,
                    );
                    if let Err(err) = tar_gz_dir(&staging.root_dir, &out) {
                        eprintln!("Error: failed to write report `{out}`: {err}");
                        eprintln!("Report staging directory left at `{}`", staging.temp_dir);
                    } else {
                        let _ = std::fs::remove_dir_all(&staging.temp_dir);
                        println!("wrote report: {out}");
                    }
                } else {
                    let _ = std::fs::remove_dir_all(&staging.temp_dir);
                }
            }
            std::process::exit(1);
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

    let had_errors = match target {
        CliTarget::StandaloneFile(file_path) => build_file(
            &mut db,
            &file_path,
            contract,
            backend_kind,
            opt_level,
            emit,
            out_dir,
            solc,
            report_ctx.as_ref(),
        ),
        CliTarget::Directory(dir_path) => build_directory(
            &mut db,
            &dir_path,
            contract,
            backend_kind,
            opt_level,
            emit,
            out_dir,
            solc,
            report_ctx.as_ref(),
        ),
    };

    if let Some((out, staging)) = report_root {
        let should_write = !report_failed_only || had_errors;
        if should_write {
            write_build_manifest(
                report_ctx.as_ref().expect("report ctx"),
                path,
                force_standalone,
                contract,
                backend_kind,
                opt_level,
                emit,
                out_dir,
                solc,
                had_errors,
            );
            if let Err(err) = tar_gz_dir(&staging.root_dir, &out) {
                eprintln!("Error: failed to write report `{out}`: {err}");
                eprintln!("Report staging directory left at `{}`", staging.temp_dir);
            } else {
                // Best-effort cleanup.
                let _ = std::fs::remove_dir_all(&staging.temp_dir);
                println!("wrote report: {out}");
            }
        } else {
            let _ = std::fs::remove_dir_all(&staging.temp_dir);
        }
    }

    if had_errors {
        std::process::exit(1);
    }
}

#[allow(clippy::too_many_arguments)]
fn build_file(
    db: &mut DriverDataBase,
    file_path: &Utf8PathBuf,
    contract: Option<&str>,
    backend_kind: BackendKind,
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: Option<&Utf8PathBuf>,
    solc: Option<&str>,
    report: Option<&BuildReportContext>,
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
    let ir_file_stem = canonical
        .file_stem()
        .map(|stem| sanitize_name_with_default(stem, "module"))
        .unwrap_or_else(|| "module".to_string());
    let report_dir = report_scope_dir(
        report,
        &format!(
            "file-{}",
            canonical
                .file_stem()
                .map(|s| s.to_string())
                .unwrap_or_else(|| "build".to_string())
        ),
    );
    build_top_mod(
        db,
        top_mod,
        contract,
        backend_kind,
        opt_level,
        emit,
        &out_dir,
        &out_dir,
        ir_file_stem.as_str(),
        true,
        solc,
        report_dir.as_ref(),
    )
    .had_errors
}

#[allow(clippy::too_many_arguments)]
fn build_directory(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    contract: Option<&str>,
    backend_kind: BackendKind,
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: Option<&Utf8PathBuf>,
    solc: Option<&str>,
    report: Option<&BuildReportContext>,
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
            db,
            &canonical,
            url,
            *workspace,
            contract,
            backend_kind,
            opt_level,
            emit,
            out_dir,
            solc,
            report,
        ),
        Config::Ingot(_) => {
            let default_out_dir = canonical.join("out");
            let out_dir = out_dir.cloned().unwrap_or(default_out_dir);
            let report_dir = report_scope_dir(
                report,
                &format!(
                    "ingot-{}",
                    canonical
                        .file_name()
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| "build".to_string())
                ),
            );
            build_ingot_url(
                db,
                &url,
                contract,
                backend_kind,
                opt_level,
                emit,
                &out_dir,
                None,
                None,
                true,
                solc,
                report_dir.as_ref(),
            )
            .had_errors
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
    backend_kind: BackendKind,
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: Option<&Utf8PathBuf>,
    solc: Option<&str>,
    report: Option<&BuildReportContext>,
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
                eprintln!("Error: Contract \"{contract}\" not found in any workspace member");
                let mut available: Vec<String> = contract_names_by_member
                    .iter()
                    .flat_map(|(_, names)| names.iter().cloned())
                    .collect();
                available.sort();
                available.dedup();
                if !available.is_empty() {
                    eprintln!("Available contracts:");
                    const MAX: usize = 50;
                    for name in available.iter().take(MAX) {
                        eprintln!("  - {name}");
                    }
                    if available.len() > MAX {
                        eprintln!("  ... and {} more", available.len() - MAX);
                    }
                }
                return true;
            }
            1 => {
                let report_dir =
                    report_scope_dir(report, &format!("member-{}", matches[0].name.as_str()));
                let summary = build_ingot_url(
                    db,
                    &matches[0].url,
                    Some(contract),
                    backend_kind,
                    opt_level,
                    emit,
                    &out_dir,
                    workspace_member_ir_out_dir(emit, &out_dir, matches[0].name.as_str()),
                    Some(matches[0].name.as_str()),
                    true,
                    solc,
                    report_dir.as_ref(),
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

    if emit.writes_any_bytecode()
        && let Err(()) = check_workspace_artifact_name_collisions(&contract_names_by_member)
    {
        return true;
    }
    if emit.ir
        && let Err(()) = check_workspace_ir_output_name_collisions(&contract_names_by_member)
    {
        return true;
    }

    let mut had_errors = false;
    let mut any_contracts = false;
    for (member, contract_names) in contract_names_by_member {
        if contract_names.is_empty() {
            continue;
        }
        any_contracts = true;
        let report_dir = report_scope_dir(report, &format!("member-{}", member.name.as_str()));
        let summary = build_ingot_url(
            db,
            &member.url,
            None,
            backend_kind,
            opt_level,
            emit,
            &out_dir,
            workspace_member_ir_out_dir(emit, &out_dir, member.name.as_str()),
            Some(member.name.as_str()),
            true,
            solc,
            report_dir.as_ref(),
        );
        had_errors |= summary.had_errors;
    }

    if !any_contracts {
        eprintln!("Error: No contracts found to build");
        return true;
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

    if !ingot_has_source_files(db, ingot) {
        eprintln!("Error: Could not find source files for ingot {ingot_url}");
        return Err(());
    }

    let diags = db.run_on_ingot(ingot);
    if !diags.is_empty() {
        diags.emit(db);
        return Err(());
    }

    let contract_names = collect_ingot_contract_names(db, ingot).map_err(|err| {
        eprintln!("Error: Failed to analyze contracts: {err}");
    })?;

    Ok(contract_names)
}

fn check_workspace_artifact_name_collisions(
    contract_names_by_member: &[(WorkspaceMemberRecord, Vec<String>)],
) -> Result<(), ()> {
    struct CollisionEntry {
        member_name: SmolStr,
        member_path: Utf8PathBuf,
        contract_name: String,
        artifact: String,
    }

    // Use a case-insensitive key to avoid filesystem-dependent artifact collisions
    // (e.g. macOS default case-insensitive APFS).
    let mut collisions: BTreeMap<String, Vec<CollisionEntry>> = BTreeMap::new();
    for (member, contract_names) in contract_names_by_member {
        for name in contract_names {
            let artifact = sanitize_filename(name);
            let key = artifact.to_ascii_lowercase();
            collisions.entry(key).or_default().push(CollisionEntry {
                member_name: member.name.clone(),
                member_path: member.path.clone(),
                contract_name: name.clone(),
                artifact,
            });
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
    for (key, entries) in duplicates {
        let mut artifacts: Vec<String> = entries.iter().map(|e| e.artifact.clone()).collect();
        artifacts.sort();
        artifacts.dedup();
        let header = if artifacts.len() == 1 {
            artifacts[0].clone()
        } else {
            format!("{key} (case-insensitive)")
        };
        let mut labels: Vec<String> = entries
            .into_iter()
            .map(|entry| {
                format!(
                    "{} in {} ({})",
                    entry.contract_name, entry.member_name, entry.member_path
                )
            })
            .collect();
        labels.sort();
        eprintln!("  - {header}");
        for label in labels {
            eprintln!("    - {label}");
        }
    }
    eprintln!("Hint: build a specific member by name or path instead.");
    Err(())
}

fn check_workspace_ir_output_name_collisions(
    contract_names_by_member: &[(WorkspaceMemberRecord, Vec<String>)],
) -> Result<(), ()> {
    struct CollisionEntry {
        member_name: SmolStr,
        member_path: Utf8PathBuf,
        artifact: String,
    }

    // Use a case-insensitive key to avoid filesystem-dependent artifact collisions
    // (e.g. macOS default case-insensitive APFS).
    let mut collisions: BTreeMap<String, Vec<CollisionEntry>> = BTreeMap::new();
    for (member, contract_names) in contract_names_by_member {
        if contract_names.is_empty() {
            continue;
        }
        let artifact = sanitize_filename(member.name.as_str());
        let key = artifact.to_ascii_lowercase();
        collisions.entry(key).or_default().push(CollisionEntry {
            member_name: member.name.clone(),
            member_path: member.path.clone(),
            artifact,
        });
    }

    let duplicates: Vec<_> = collisions
        .into_iter()
        .filter(|(_, entries)| entries.len() > 1)
        .collect();

    if duplicates.is_empty() {
        return Ok(());
    }

    eprintln!("Error: Workspace member names collide in IR output directories");
    eprintln!("Conflicts:");
    for (key, entries) in duplicates {
        let mut artifacts: Vec<String> = entries.iter().map(|e| e.artifact.clone()).collect();
        artifacts.sort();
        artifacts.dedup();
        let header = if artifacts.len() == 1 {
            artifacts[0].clone()
        } else {
            format!("{key} (case-insensitive)")
        };
        let mut labels: Vec<String> = entries
            .into_iter()
            .map(|entry| format!("{} ({})", entry.member_name, entry.member_path))
            .collect();
        labels.sort();
        eprintln!("  - {header}");
        for label in labels {
            eprintln!("    - {label}");
        }
    }
    eprintln!("Hint: build a specific member by name or path instead.");
    Err(())
}

#[allow(clippy::too_many_arguments)]
fn build_ingot_url(
    db: &mut DriverDataBase,
    ingot_url: &Url,
    contract: Option<&str>,
    backend_kind: BackendKind,
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: &Utf8Path,
    ir_out_dir: Option<Utf8PathBuf>,
    ir_file_stem: Option<&str>,
    missing_contract_is_error: bool,
    solc: Option<&str>,
    report_dir: Option<&Utf8PathBuf>,
) -> BuildSummary {
    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        eprintln!("Error: Could not resolve ingot from directory");
        return BuildSummary { had_errors: true };
    };

    if !ingot_has_source_files(db, ingot) {
        eprintln!("Error: Could not find source files for ingot {ingot_url}");
        return BuildSummary { had_errors: true };
    }

    let diags = db.run_on_ingot(ingot);
    if !diags.is_empty() {
        diags.emit(db);
        return BuildSummary { had_errors: true };
    }

    let ir_file_stem = ir_file_stem
        .map(|name| sanitize_name_with_default(name, "module"))
        .unwrap_or_else(|| derive_ingot_ir_file_stem(db, ingot));

    build_ingot(
        db,
        ingot,
        contract,
        backend_kind,
        opt_level,
        emit,
        out_dir,
        ir_out_dir.as_ref().map_or(out_dir, Utf8PathBuf::as_path),
        ir_file_stem.as_str(),
        missing_contract_is_error,
        solc,
        report_dir,
    )
}

#[allow(clippy::too_many_arguments)]
fn build_ingot(
    db: &DriverDataBase,
    ingot: hir::Ingot<'_>,
    contract: Option<&str>,
    backend_kind: BackendKind,
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: &Utf8Path,
    ir_out_dir: &Utf8Path,
    ir_file_stem: &str,
    missing_contract_is_error: bool,
    solc: Option<&str>,
    report_dir: Option<&Utf8PathBuf>,
) -> BuildSummary {
    let contract_names = match collect_ingot_contract_names(db, ingot) {
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

    let names_to_build =
        match resolve_names_to_build(contract_names, contract, missing_contract_is_error) {
            Ok(names) => names,
            Err(summary) => return summary,
        };
    if let Err(err) = ensure_output_dirs(emit, out_dir, ir_out_dir) {
        eprintln!("Error: {err}");
        return BuildSummary { had_errors: true };
    }
    let report_dir = report_dir.map(Utf8PathBuf::as_path);

    let mut had_errors = false;
    match backend_kind {
        BackendKind::Yul => {
            let optimize = opt_level.yul_optimize();
            let yul = match codegen::emit_ingot_yul(db, ingot) {
                Ok(yul) => yul,
                Err(err) => {
                    eprintln!("Error: Failed to emit Yul: {err}");
                    return BuildSummary { had_errors: true };
                }
            };
            if let Some(dir) = report_dir {
                let path = dir.join("ingot.yul");
                let _ = std::fs::write(path.as_std_path(), &yul);
                match lower_ingot(db, ingot) {
                    Ok(mir) => {
                        let path = dir.join("mir.txt");
                        let _ =
                            std::fs::write(path.as_std_path(), mir_fmt::format_module(db, &mir));
                    }
                    Err(err) => {
                        let path = dir.join("mir_error.txt");
                        let _ = std::fs::write(path.as_std_path(), format!("{err}"));
                    }
                }
            }
            if emit.ir
                && let Err(err) =
                    write_named_ir_artifact(ir_out_dir, report_dir, ir_file_stem, "yul", &yul)
            {
                eprintln!("Error: {err}");
                had_errors = true;
            }
            if emit.writes_any_bytecode() {
                had_errors |= write_yul_bytecode_artifacts(
                    &names_to_build,
                    &yul,
                    optimize,
                    out_dir,
                    report_dir,
                    emit,
                    solc,
                );
            }
        }
        BackendKind::Sonatina => {
            if emit.ir {
                let mir_module = match lower_ingot(db, ingot) {
                    Ok(mir_module) => mir_module,
                    Err(err) => {
                        eprintln!("Error: Failed to compile Sonatina IR: {err}");
                        return BuildSummary { had_errors: true };
                    }
                };
                let ir = match codegen::emit_mir_module_sonatina_ir_optimized(
                    db,
                    &mir_module,
                    opt_level,
                    contract,
                ) {
                    Ok(ir) => ir,
                    Err(err) => {
                        eprintln!("Error: Failed to compile Sonatina IR: {err}");
                        return BuildSummary { had_errors: true };
                    }
                };
                if let Err(err) =
                    write_named_ir_artifact(ir_out_dir, report_dir, ir_file_stem, "sona", &ir)
                {
                    eprintln!("Error: {err}");
                    had_errors = true;
                }
            }
            if emit.writes_any_bytecode() {
                let bytecode =
                    match codegen::emit_ingot_sonatina_bytecode(db, ingot, opt_level, contract) {
                        Ok(bytecode) => bytecode,
                        Err(err) => {
                            eprintln!("Error: Failed to compile Sonatina bytecode: {err}");
                            return BuildSummary { had_errors: true };
                        }
                    };
                had_errors |= write_sonatina_bytecode_artifacts(
                    &names_to_build,
                    &bytecode,
                    out_dir,
                    report_dir,
                    emit,
                );
            }
        }
    };

    BuildSummary { had_errors }
}

#[allow(clippy::too_many_arguments)]
fn build_top_mod(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    contract: Option<&str>,
    backend_kind: BackendKind,
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: &Utf8Path,
    ir_out_dir: &Utf8Path,
    ir_file_stem: &str,
    missing_contract_is_error: bool,
    solc: Option<&str>,
    report_dir: Option<&Utf8PathBuf>,
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

    let names_to_build =
        match resolve_names_to_build(contract_names, contract, missing_contract_is_error) {
            Ok(names) => names,
            Err(summary) => return summary,
        };
    if let Err(err) = ensure_output_dirs(emit, out_dir, ir_out_dir) {
        eprintln!("Error: {err}");
        return BuildSummary { had_errors: true };
    }
    let report_dir = report_dir.map(Utf8PathBuf::as_path);

    let mut had_errors = false;
    match backend_kind {
        BackendKind::Yul => {
            let optimize = opt_level.yul_optimize();
            let yul = match codegen::emit_module_yul(db, top_mod) {
                Ok(yul) => yul,
                Err(err) => {
                    eprintln!("Error: Failed to emit Yul: {err}");
                    return BuildSummary { had_errors: true };
                }
            };
            if let Some(dir) = report_dir {
                let path = dir.join("module.yul");
                let _ = std::fs::write(path.as_std_path(), &yul);
                match lower_module(db, top_mod) {
                    Ok(mir) => {
                        let path = dir.join("mir.txt");
                        let _ =
                            std::fs::write(path.as_std_path(), mir_fmt::format_module(db, &mir));
                    }
                    Err(err) => {
                        let path = dir.join("mir_error.txt");
                        let _ = std::fs::write(path.as_std_path(), format!("{err}"));
                    }
                }
            }
            if emit.ir
                && let Err(err) =
                    write_named_ir_artifact(ir_out_dir, report_dir, ir_file_stem, "yul", &yul)
            {
                eprintln!("Error: {err}");
                had_errors = true;
            }
            if emit.writes_any_bytecode() {
                had_errors |= write_yul_bytecode_artifacts(
                    &names_to_build,
                    &yul,
                    optimize,
                    out_dir,
                    report_dir,
                    emit,
                    solc,
                );
            }
        }
        BackendKind::Sonatina => {
            if emit.ir {
                let ir = match codegen::emit_module_sonatina_ir_optimized(
                    db, top_mod, opt_level, contract,
                ) {
                    Ok(ir) => ir,
                    Err(err) => {
                        eprintln!("Error: Failed to compile Sonatina IR: {err}");
                        return BuildSummary { had_errors: true };
                    }
                };
                if let Err(err) =
                    write_named_ir_artifact(ir_out_dir, report_dir, ir_file_stem, "sona", &ir)
                {
                    eprintln!("Error: {err}");
                    had_errors = true;
                }
            }
            if emit.writes_any_bytecode() {
                let bytecode = match codegen::emit_module_sonatina_bytecode(
                    db, top_mod, opt_level, contract,
                ) {
                    Ok(bytecode) => bytecode,
                    Err(err) => {
                        eprintln!("Error: Failed to compile Sonatina bytecode: {err}");
                        return BuildSummary { had_errors: true };
                    }
                };
                had_errors |= write_sonatina_bytecode_artifacts(
                    &names_to_build,
                    &bytecode,
                    out_dir,
                    report_dir,
                    emit,
                );
            }
        }
    };

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

fn collect_ingot_contract_names(
    db: &DriverDataBase,
    ingot: hir::Ingot<'_>,
) -> Result<Vec<String>, String> {
    let module = lower_ingot(db, ingot).map_err(|err| err.to_string())?;
    let graph = build_contract_graph(&module.functions);
    let mut names: Vec<_> = graph.contracts.keys().cloned().collect();
    names.sort();
    Ok(names)
}

fn workspace_member_ir_out_dir(
    emit: EmitSelection,
    out_dir: &Utf8Path,
    member_name: &str,
) -> Option<Utf8PathBuf> {
    emit.ir
        .then(|| out_dir.join(sanitize_filename(member_name)))
}

fn resolve_names_to_build(
    contract_names: Vec<String>,
    contract: Option<&str>,
    missing_contract_is_error: bool,
) -> Result<Vec<String>, BuildSummary> {
    let Some(name) = contract else {
        return Ok(contract_names);
    };

    if contract_names.iter().any(|candidate| candidate == name) {
        return Ok(vec![name.to_string()]);
    }
    if missing_contract_is_error {
        eprintln!("Error: Contract \"{name}\" not found");
        eprintln!("Available contracts:");
        for candidate in &contract_names {
            eprintln!("  - {candidate}");
        }
        Err(BuildSummary { had_errors: true })
    } else {
        Err(BuildSummary { had_errors: false })
    }
}

fn ensure_output_dirs(
    emit: EmitSelection,
    out_dir: &Utf8Path,
    ir_out_dir: &Utf8Path,
) -> Result<(), String> {
    if emit.writes_any_bytecode() {
        fs::create_dir_all(out_dir.as_std_path())
            .map_err(|err| format!("Failed to create output directory {out_dir}: {err}"))?;
    }
    if emit.ir {
        fs::create_dir_all(ir_out_dir.as_std_path())
            .map_err(|err| format!("Failed to create output directory {ir_out_dir}: {err}"))?;
    }
    Ok(())
}

fn write_named_ir_artifact(
    out_dir: &Utf8Path,
    report_dir: Option<&Utf8Path>,
    ir_file_stem: &str,
    extension: &str,
    ir: &str,
) -> Result<(), String> {
    let file_name = format!("{ir_file_stem}.{extension}");
    let path = out_dir.join(&file_name);
    let ir_with_newline = if ir.ends_with('\n') {
        ir.to_string()
    } else {
        format!("{ir}\n")
    };
    fs::write(path.as_std_path(), &ir_with_newline)
        .map_err(|err| format!("Failed to write {path}: {err}"))?;
    if let Some(dir) = report_dir {
        let _ = fs::write(dir.join(&file_name).as_std_path(), ir_with_newline);
    }
    println!("Wrote {out_dir}/{file_name}");
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn write_yul_bytecode_artifacts(
    names_to_build: &[String],
    yul: &str,
    optimize: bool,
    out_dir: &Utf8Path,
    report_dir: Option<&Utf8Path>,
    emit: EmitSelection,
    solc: Option<&str>,
) -> bool {
    let mut had_errors = false;
    for name in names_to_build {
        match compile_single_contract_with_solc(name, yul, optimize, true, solc) {
            Ok(bytecode) => {
                if let Err(err) = write_contract_artifacts(
                    out_dir,
                    report_dir,
                    name,
                    &bytecode.bytecode,
                    &bytecode.runtime_bytecode,
                    emit,
                ) {
                    eprintln!("Error: {err}");
                    had_errors = true;
                } else {
                    print_contract_artifact_paths(out_dir, name, emit);
                }
            }
            Err(err) => {
                eprintln!("Error: solc failed for contract \"{name}\": {}", err.0);
                eprintln!("Hint: install solc, set FE_SOLC_PATH, or pass --solc <path>.");
                had_errors = true;
            }
        }
    }
    had_errors
}

fn write_sonatina_bytecode_artifacts(
    names_to_build: &[String],
    bytecode: &BTreeMap<String, SonatinaContractBytecode>,
    out_dir: &Utf8Path,
    report_dir: Option<&Utf8Path>,
    emit: EmitSelection,
) -> bool {
    let mut had_errors = false;
    for name in names_to_build {
        let Some(SonatinaContractBytecode { deploy, runtime }) = bytecode.get(name) else {
            eprintln!("Error: Sonatina did not emit bytecode for contract \"{name}\"");
            had_errors = true;
            continue;
        };
        let deploy_hex = hex::encode(deploy);
        let runtime_hex = hex::encode(runtime);
        if let Err(err) =
            write_contract_artifacts(out_dir, report_dir, name, &deploy_hex, &runtime_hex, emit)
        {
            eprintln!("Error: {err}");
            had_errors = true;
        } else {
            print_contract_artifact_paths(out_dir, name, emit);
        }
    }
    had_errors
}

fn derive_ingot_ir_file_stem(db: &DriverDataBase, ingot: hir::Ingot<'_>) -> String {
    if let Some(name) = ingot
        .config(db)
        .and_then(|config| config.metadata.name)
        .map(|name| name.to_string())
    {
        return sanitize_name_with_default(&name, "module");
    }

    let ingot_base = ingot.base(db);
    let fallback = ingot_base
        .path_segments()
        .and_then(|mut segments| segments.rfind(|segment| !segment.is_empty()))
        .unwrap_or("module");
    sanitize_name_with_default(fallback, "module")
}

fn describe_emit_selection(emit: EmitSelection) -> String {
    let mut parts = Vec::new();
    if emit.bytecode {
        parts.push("bytecode");
    }
    if emit.runtime_bytecode {
        parts.push("runtime-bytecode");
    }
    if emit.ir {
        parts.push("ir");
    }
    parts.join(",")
}

fn write_contract_artifacts(
    out_dir: &Utf8Path,
    report_dir: Option<&Utf8Path>,
    contract_name: &str,
    bytecode: &str,
    runtime_bytecode: &str,
    emit: EmitSelection,
) -> Result<(), String> {
    let base = sanitize_filename(contract_name);
    if emit.bytecode {
        let deploy_path = out_dir.join(format!("{base}.bin"));
        fs::write(deploy_path.as_std_path(), format!("{bytecode}\n"))
            .map_err(|err| format!("Failed to write {deploy_path}: {err}"))?;
        if let Some(dir) = report_dir {
            let deploy_path = dir.join(format!("{base}.bin"));
            let _ = fs::write(deploy_path.as_std_path(), format!("{bytecode}\n"));
        }
    }
    if emit.runtime_bytecode {
        let runtime_path = out_dir.join(format!("{base}.runtime.bin"));
        fs::write(runtime_path.as_std_path(), format!("{runtime_bytecode}\n"))
            .map_err(|err| format!("Failed to write {runtime_path}: {err}"))?;
        if let Some(dir) = report_dir {
            let runtime_path = dir.join(format!("{base}.runtime.bin"));
            let _ = fs::write(runtime_path.as_std_path(), format!("{runtime_bytecode}\n"));
        }
    }
    Ok(())
}

fn print_contract_artifact_paths(out_dir: &Utf8Path, contract_name: &str, emit: EmitSelection) {
    let base = sanitize_filename(contract_name);
    if emit.bytecode {
        println!("Wrote {out_dir}/{base}.bin");
    }
    if emit.runtime_bytecode {
        println!("Wrote {out_dir}/{base}.runtime.bin");
    }
}

fn sanitize_filename(name: &str) -> String {
    sanitize_name_with_default(name, "contract")
}

fn sanitize_name_with_default(name: &str, default_name: &str) -> String {
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
        default_name.into()
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
