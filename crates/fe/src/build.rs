use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fs,
};

use camino::{Utf8Path, Utf8PathBuf};
use codegen::{OptLevel, SonatinaContractBytecode};
use common::{
    InputDb,
    config::{
        ArithmeticMode, Config, DependencyArithmeticMode, resolve_dependency_arithmetic_mode,
    },
    dependencies::WorkspaceMemberRecord,
    file::IngotFileKind,
    ingot::{IngotBaseUrl, IngotKind},
};
use driver::DriverDataBase;
use driver::cli_target::{CliTarget, resolve_cli_target};
use hir::hir_def::{HirIngot, ManualContractRootAttr, TopLevelMod};
use hir::lower::map_file_to_mod;
use mir::build_runtime_package;
use salsa::Setter;
use smol_str::SmolStr;
use tiny_keccak::{Hasher, Keccak};
use url::Url;

use crate::{
    BuildEmit,
    report::{
        ReportStaging, copy_input_into_report, create_dir_all_utf8, create_report_staging_root,
        enable_panic_report, normalize_report_out_path, tar_gz_dir, write_report_meta,
    },
    workspace_ingot::{
        INGOT_REQUIRES_WORKSPACE_ROOT, WorkspaceMemberRef, select_workspace_member_paths,
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

#[derive(Debug, Default)]
struct IngotBuildAnalysis {
    contract_names: Vec<String>,
    abi_artifact_names: Vec<String>,
}

#[derive(Debug, Clone, Copy)]
struct EmitSelection {
    bytecode: bool,
    runtime_bytecode: bool,
    ir: bool,
    abi: bool,
    metadata: bool,
}

impl EmitSelection {
    fn from_requested(requested: &[BuildEmit]) -> Self {
        let mut selection = Self {
            bytecode: false,
            runtime_bytecode: false,
            ir: false,
            abi: false,
            metadata: false,
        };
        for emit in requested {
            match emit {
                BuildEmit::Bytecode => selection.bytecode = true,
                BuildEmit::RuntimeBytecode => selection.runtime_bytecode = true,
                BuildEmit::Ir => selection.ir = true,
                BuildEmit::Abi => selection.abi = true,
                BuildEmit::Metadata => selection.metadata = true,
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
    ingot: Option<&str>,
    force_standalone: bool,
    contract: Option<&str>,
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: Option<&Utf8PathBuf>,
    has_errors: bool,
) {
    let mut out = String::new();
    out.push_str("fe build report\n");
    out.push_str(&format!("path: {path}\n"));
    out.push_str(&format!("ingot: {}\n", ingot.unwrap_or("<all>")));
    out.push_str(&format!("standalone: {force_standalone}\n"));
    out.push_str(&format!("contract: {}\n", contract.unwrap_or("<all>")));
    out.push_str("backend: sonatina\n");
    out.push_str(&format!("opt_level: {opt_level}\n"));
    out.push_str(&format!("emit: {}\n", describe_emit_selection(emit)));
    out.push_str(&format!(
        "out_dir: {}\n",
        out_dir.map(|p| p.as_str()).unwrap_or("<default>")
    ));
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
    ingot: Option<&str>,
    force_standalone: bool,
    contract: Option<&str>,
    opt_level: OptLevel,
    emit: &[BuildEmit],
    out_dir: Option<&Utf8PathBuf>,
    profile: &str,
    report_out: Option<&Utf8PathBuf>,
    report_failed_only: bool,
    use_recovery_mode: bool,
) {
    let emit = EmitSelection::from_requested(emit);
    let mut db = DriverDataBase::default();
    db.compiler_options()
        .set_recovery_mode(&mut db)
        .to(use_recovery_mode);
    db.compilation_settings()
        .set_profile(&mut db)
        .to(profile.into());

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
                        ingot,
                        force_standalone,
                        contract,
                        opt_level,
                        emit,
                        out_dir,
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
            ingot,
            contract,
            opt_level,
            emit,
            out_dir,
            report_ctx.as_ref(),
        ),
        CliTarget::Directory(dir_path) => build_directory(
            &mut db,
            &dir_path,
            ingot,
            contract,
            opt_level,
            emit,
            out_dir,
            report_ctx.as_ref(),
        ),
    };

    if let Some((out, staging)) = report_root {
        let should_write = !report_failed_only || had_errors;
        if should_write {
            write_build_manifest(
                report_ctx.as_ref().expect("report ctx"),
                path,
                ingot,
                force_standalone,
                contract,
                opt_level,
                emit,
                out_dir,
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
    ingot: Option<&str>,
    contract: Option<&str>,
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: Option<&Utf8PathBuf>,
    report: Option<&BuildReportContext>,
) -> bool {
    if ingot.is_some() {
        eprintln!("Error: {INGOT_REQUIRES_WORKSPACE_ROOT}");
        return true;
    }

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
    let hir_diags = db.run_on_top_mod(top_mod);
    let mut has_errors = false;
    let hir_has_errors = hir_diags.has_errors(db);
    if !hir_diags.is_empty() {
        hir_diags.emit(db);
        has_errors = true;
    }
    let mir_diags = if hir_has_errors {
        Vec::new()
    } else {
        db.mir_diagnostics_for_top_mod(top_mod)
    };
    if !mir_diags.is_empty() {
        db.emit_complete_diagnostics(&mir_diags);
        has_errors = true;
    }
    if has_errors {
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
        opt_level,
        emit,
        &out_dir,
        &out_dir,
        ir_file_stem.as_str(),
        true,
        report_dir.as_ref(),
    )
    .had_errors
}

#[allow(clippy::too_many_arguments)]
fn build_directory(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    ingot: Option<&str>,
    contract: Option<&str>,
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: Option<&Utf8PathBuf>,
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
        if ingot.is_some() {
            eprintln!("Error: {INGOT_REQUIRES_WORKSPACE_ROOT}");
            return true;
        }
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
        Config::Workspace(_) => build_workspace(
            db, &canonical, url, ingot, contract, opt_level, emit, out_dir, report,
        ),
        Config::Ingot(_) => {
            if ingot.is_some() {
                eprintln!("Error: {INGOT_REQUIRES_WORKSPACE_ROOT}");
                return true;
            }
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
                opt_level,
                emit,
                &out_dir,
                None,
                None,
                true,
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
    ingot: Option<&str>,
    contract: Option<&str>,
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: Option<&Utf8PathBuf>,
    report: Option<&BuildReportContext>,
) -> bool {
    let mut members = db
        .dependency_graph()
        .workspace_member_records(db, &workspace_url);
    members.sort_by(|a, b| a.path.cmp(&b.path));

    if members.is_empty() {
        eprintln!(
            "Warning: No workspace members found. Check that member paths in fe.toml exist on disk."
        );
        return false;
    }

    let selected_member_paths = match select_workspace_member_paths(
        workspace_root,
        workspace_root,
        members.iter().map(|member| {
            WorkspaceMemberRef::new(member.path.as_path(), Some(member.name.as_str()))
        }),
        ingot,
    ) {
        Ok(paths) => paths,
        Err(err) => {
            eprintln!("Error: {err}");
            return true;
        }
    };
    let selected_member_paths: HashSet<Utf8PathBuf> = selected_member_paths.into_iter().collect();

    let out_dir = out_dir
        .cloned()
        .unwrap_or_else(|| workspace_root.join("out"));

    let abi_collision_check_needs_filtering =
        emit.abi && !emit.writes_any_bytecode() && contract.is_none();
    let mut contract_names_by_member = Vec::with_capacity(selected_member_paths.len());
    let mut abi_artifact_names_by_member = Vec::with_capacity(selected_member_paths.len());
    for member in members {
        let member_path = workspace_root.join(member.path.as_str());
        if !selected_member_paths.contains(&member_path) {
            continue;
        }
        let analysis = match analyze_ingot_build_artifacts(
            db,
            &member.url,
            abi_collision_check_needs_filtering,
        ) {
            Ok(analysis) => analysis,
            Err(()) => return true,
        };
        contract_names_by_member.push((member.clone(), analysis.contract_names));
        if abi_collision_check_needs_filtering {
            abi_artifact_names_by_member.push((member, analysis.abi_artifact_names));
        }
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
                    opt_level,
                    emit,
                    &out_dir,
                    workspace_member_ir_out_dir(emit, &out_dir, matches[0].name.as_str()),
                    Some(matches[0].name.as_str()),
                    true,
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

    if (emit.writes_any_bytecode() || emit.metadata)
        && let Err(()) = check_workspace_artifact_name_collisions(&contract_names_by_member)
    {
        return true;
    }
    if abi_collision_check_needs_filtering
        && let Err(()) = check_workspace_artifact_name_collisions(&abi_artifact_names_by_member)
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
            opt_level,
            emit,
            &out_dir,
            workspace_member_ir_out_dir(emit, &out_dir, member.name.as_str()),
            Some(member.name.as_str()),
            true,
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

fn analyze_ingot_build_artifacts(
    db: &mut DriverDataBase,
    ingot_url: &Url,
    include_abi_artifact_names: bool,
) -> Result<IngotBuildAnalysis, ()> {
    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        eprintln!("Error: Could not resolve ingot from directory");
        return Err(());
    };

    if !ingot_has_source_files(db, ingot) {
        eprintln!("Error: Could not find source files for ingot {ingot_url}");
        return Err(());
    }

    let hir_diags = db.run_on_ingot(ingot);
    let mut has_errors = false;
    let hir_has_errors = hir_diags.has_errors(db);
    if !hir_diags.is_empty() {
        hir_diags.emit(db);
        has_errors = true;
    }
    let mir_diags = if hir_has_errors {
        Vec::new()
    } else {
        db.mir_diagnostics_for_ingot(ingot)
    };
    if !mir_diags.is_empty() {
        db.emit_complete_diagnostics(&mir_diags);
        has_errors = true;
    }
    if has_errors {
        return Err(());
    }

    let contract_names = collect_workspace_contract_names(db, ingot);

    let abi_artifact_names = if include_abi_artifact_names {
        collect_ingot_abi_artifact_names(db, ingot).map_err(|err| {
            eprintln!("Error: Failed to analyze ABI artifacts: {err}");
        })?
    } else {
        Vec::new()
    };

    Ok(IngotBuildAnalysis {
        contract_names,
        abi_artifact_names,
    })
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
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: &Utf8Path,
    ir_out_dir: Option<Utf8PathBuf>,
    ir_file_stem: Option<&str>,
    missing_contract_is_error: bool,
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

    let hir_diags = db.run_on_ingot(ingot);
    let mut has_errors = false;
    let hir_has_errors = hir_diags.has_errors(db);
    if !hir_diags.is_empty() {
        hir_diags.emit(db);
        has_errors = true;
    }
    let mir_diags = if hir_has_errors {
        Vec::new()
    } else {
        db.mir_diagnostics_for_ingot(ingot)
    };
    if !mir_diags.is_empty() {
        db.emit_complete_diagnostics(&mir_diags);
        has_errors = true;
    }
    if has_errors {
        return BuildSummary { had_errors: true };
    }

    let ir_file_stem = ir_file_stem
        .map(|name| sanitize_name_with_default(name, "module"))
        .unwrap_or_else(|| derive_ingot_ir_file_stem(db, ingot));

    build_ingot(
        db,
        ingot,
        contract,
        opt_level,
        emit,
        out_dir,
        ir_out_dir.as_ref().map_or(out_dir, Utf8PathBuf::as_path),
        ir_file_stem.as_str(),
        missing_contract_is_error,
        report_dir,
    )
}

#[allow(clippy::too_many_arguments)]
fn build_ingot(
    db: &DriverDataBase,
    ingot: hir::Ingot<'_>,
    contract: Option<&str>,
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: &Utf8Path,
    ir_out_dir: &Utf8Path,
    ir_file_stem: &str,
    missing_contract_is_error: bool,
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
    if emit.ir || emit.writes_any_bytecode() {
        if emit.ir {
            let ir = match codegen::emit_ingot_sonatina_ir_optimized(db, ingot, opt_level, contract)
            {
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

    if emit.abi {
        // For ingot builds, generate ABI from each top-level module
        use hir::hir_def::HirIngot;
        for top_mod in ingot.all_modules(db) {
            had_errors |= write_abi_artifacts(db, *top_mod, &names_to_build, out_dir, report_dir);
        }
    }

    if emit.metadata {
        had_errors |=
            write_metadata_artifacts(db, ingot, &names_to_build, opt_level, out_dir, report_dir);
    }

    BuildSummary { had_errors }
}

#[allow(clippy::too_many_arguments)]
fn build_top_mod(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    contract: Option<&str>,
    opt_level: OptLevel,
    emit: EmitSelection,
    out_dir: &Utf8Path,
    ir_out_dir: &Utf8Path,
    ir_file_stem: &str,
    missing_contract_is_error: bool,
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
    if emit.ir || emit.writes_any_bytecode() {
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
            let bytecode =
                match codegen::emit_module_sonatina_bytecode(db, top_mod, opt_level, contract) {
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

    if emit.abi {
        had_errors |= write_abi_artifacts(db, top_mod, &names_to_build, out_dir, report_dir);
    }

    if emit.metadata {
        let ingot = top_mod.ingot(db);
        had_errors |=
            write_metadata_artifacts(db, ingot, &names_to_build, opt_level, out_dir, report_dir);
    }

    BuildSummary { had_errors }
}

fn collect_contract_names(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
) -> Result<Vec<String>, String> {
    let package = build_runtime_package(db, top_mod).map_err(|err| err.to_string())?;
    let mut names: Vec<_> = package
        .root_objects(db)
        .iter()
        .map(|object| object.name(db).clone())
        .collect();
    names.sort();
    names.dedup();
    Ok(names)
}

fn collect_ingot_contract_names(
    db: &DriverDataBase,
    ingot: hir::Ingot<'_>,
) -> Result<Vec<String>, String> {
    let mut names = BTreeSet::new();
    for &top_mod in ingot.all_modules(db) {
        let package = build_runtime_package(db, top_mod).map_err(|err| err.to_string())?;
        for object in package.root_objects(db) {
            names.insert(object.name(db).clone());
        }
    }
    let mut names: Vec<_> = names.into_iter().collect();
    names.sort();
    Ok(names)
}

fn collect_workspace_contract_names(db: &DriverDataBase, ingot: hir::Ingot<'_>) -> Vec<String> {
    let mut names = BTreeSet::new();
    for &top_mod in ingot.all_modules(db) {
        for contract in top_mod.all_contracts(db) {
            if let Some(name) = contract.name(db).to_opt() {
                names.insert(name.data(db).to_string());
            }
        }
        for &func in top_mod.all_funcs(db) {
            if func.top_mod(db) != top_mod {
                continue;
            }
            match func.manual_contract_root_attr(db) {
                Some(ManualContractRootAttr::Init { contract_name })
                | Some(ManualContractRootAttr::Runtime { contract_name }) => {
                    names.insert(contract_name.data(db).to_string());
                }
                Some(ManualContractRootAttr::Error(_)) | None => {}
            }
        }
    }
    names.into_iter().collect()
}

fn collect_ingot_abi_artifact_names(
    db: &DriverDataBase,
    ingot: hir::Ingot<'_>,
) -> Result<Vec<String>, String> {
    use hir::hir_def::HirIngot;

    let mut names = BTreeSet::new();
    for top_mod in ingot.all_modules(db) {
        for contract in top_mod.all_contracts(db) {
            let Some(name) = contract
                .name(db)
                .to_opt()
                .map(|name| name.data(db).to_string())
            else {
                continue;
            };
            let Some(result) = crate::abi::generate_contract_abi(db, *top_mod, &name)? else {
                continue;
            };
            if result.entry_count > 0 {
                names.insert(name);
            }
        }
    }

    Ok(names.into_iter().collect())
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
    if emit.writes_any_bytecode() || emit.abi || emit.metadata {
        fs::create_dir_all(out_dir.as_std_path())
            .map_err(|err| format!("Failed to create output directory {out_dir}: {err}"))?;
    }
    if emit.ir {
        fs::create_dir_all(ir_out_dir.as_std_path())
            .map_err(|err| format!("Failed to create output directory {ir_out_dir}: {err}"))?;
    }
    Ok(())
}

fn write_abi_artifacts(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    names_to_build: &[String],
    out_dir: &Utf8Path,
    report_dir: Option<&Utf8Path>,
) -> bool {
    let mut had_errors = false;
    for name in names_to_build {
        match crate::abi::generate_contract_abi(db, top_mod, name) {
            Ok(Some(result)) => {
                for warning in &result.warnings {
                    eprintln!("Warning: {warning}");
                }
                let base = sanitize_filename(name);
                let abi_path = out_dir.join(format!("{base}.abi.json"));
                let report_path = report_dir.map(|dir| dir.join(format!("{base}.abi.json")));
                if result.entry_count == 0 {
                    if let Err(err) = remove_file_if_exists(&abi_path) {
                        eprintln!("Error: Failed to remove stale ABI: {err}");
                        had_errors = true;
                    }
                    if let Some(path) = report_path.as_ref()
                        && let Err(err) = remove_file_if_exists(path)
                    {
                        eprintln!("Error: Failed to remove stale ABI from report dir: {err}");
                        had_errors = true;
                    }
                    continue;
                }
                if let Err(err) = fs::write(abi_path.as_std_path(), &result.json) {
                    eprintln!("Error: Failed to write ABI: {err}");
                    had_errors = true;
                } else {
                    if let Some(path) = report_path.as_ref() {
                        let _ = fs::write(path.as_std_path(), &result.json);
                    }
                    println!("Wrote {out_dir}/{base}.abi.json");
                }
            }
            Ok(None) => {} // Contract not in this module, skip
            Err(err) => {
                eprintln!("Error: Failed to generate ABI for {name}: {err}");
                had_errors = true;
            }
        }
    }
    had_errors
}

/// keccak256 of `bytes` as a `0x`-prefixed lowercase hex string (matches Solidity's
/// `sources[].keccak256`).
fn keccak256_hex(bytes: &[u8]) -> String {
    let mut hasher = Keccak::v256();
    hasher.update(bytes);
    let mut out = [0u8; 32];
    hasher.finalize(&mut out);
    let mut s = String::with_capacity(2 + 64);
    s.push_str("0x");
    for byte in out {
        s.push_str(&format!("{byte:02x}"));
    }
    s
}

fn arithmetic_str(mode: Option<ArithmeticMode>) -> &'static str {
    match mode {
        Some(ArithmeticMode::Unchecked) => "unchecked",
        // `checked` is the default when unset.
        Some(ArithmeticMode::Checked) | None => "checked",
    }
}

fn dependency_arithmetic_str(mode: DependencyArithmeticMode) -> &'static str {
    match mode {
        DependencyArithmeticMode::Defer => "defer",
        DependencyArithmeticMode::Checked => "checked",
        DependencyArithmeticMode::Unchecked => "unchecked",
    }
}

/// The ingot's declared name (falling back to the last segment of its base URL). This is the
/// human-facing `name` and the *candidate* namespace; see [`assign_namespaces`] for the unique
/// namespace actually used as a path prefix and dependency-map target.
fn ingot_namespace(db: &DriverDataBase, ingot: hir::Ingot<'_>) -> String {
    if let Some(name) = ingot.config(db).and_then(|config| config.metadata.name) {
        return name.to_string();
    }
    ingot
        .base(db)
        .path()
        .trim_end_matches('/')
        .rsplit('/')
        .next()
        .filter(|segment| !segment.is_empty())
        .unwrap_or("dep")
        .to_string()
}

/// Collect one ingot's `.fe` source files keyed by ingot-relative path (basename for standalone),
/// optionally namespaced with `prefix`, into `out`.
fn collect_ingot_sources_prefixed<'db>(
    db: &'db DriverDataBase,
    ingot: hir::Ingot<'db>,
    prefix: Option<&str>,
    out: &mut BTreeMap<String, (TopLevelMod<'db>, String)>,
) {
    let standalone = ingot.standalone_file(db).is_some();
    for (url, file) in ingot.files(db).iter() {
        if file.kind(db) != Some(IngotFileKind::Source) {
            continue;
        }
        let rel = if standalone {
            url.path()
                .rsplit('/')
                .next()
                .filter(|s| !s.is_empty())
                .unwrap_or("input.fe")
                .to_string()
        } else {
            match file.path(db).clone() {
                Some(p) if !p.as_str().is_empty() => p.into_string(),
                _ => continue,
            }
        };
        let key = match prefix {
            Some(ns) => format!("{ns}/{rel}"),
            None => rel,
        };
        let top_mod = map_file_to_mod(db, file);
        out.insert(key, (top_mod, file.text(db).clone()));
    }
}

/// The non-builtin ingots needed to reproduce a build: the root ingot followed by its transitive
/// dependencies, excluding the bundled `std`/`core` (pinned to `compiler.version`).
fn non_builtin_closure<'db>(
    db: &'db DriverDataBase,
    root: hir::Ingot<'db>,
) -> Vec<hir::Ingot<'db>> {
    let mut closure = vec![root];
    for dep_url in db.dependency_graph().dependency_urls(db, &root.base(db)) {
        let Some(dep) = dep_url.ingot(db) else {
            continue;
        };
        if matches!(dep.kind(db), IngotKind::Core | IngotKind::Std) {
            continue;
        }
        closure.push(dep);
    }
    closure
}

/// Assign each non-builtin ingot in the closure a unique, deterministic namespace, used
/// consistently for source-key prefixes, `settings.ingots[].namespace`, and the dependency map.
/// The root ingot gets the empty namespace; others use their name, qualified by version (then an
/// index) when two *distinct* ingots share a name (e.g. two versions of the same package). Without
/// this, same-named-different-version dependencies would collide in the flat `sources` map.
fn assign_namespaces(
    db: &DriverDataBase,
    root: hir::Ingot<'_>,
    closure: &[hir::Ingot<'_>],
    root_source_keys: &BTreeSet<String>,
) -> HashMap<Url, String> {
    let root_base = root.base(db);

    // How many non-root ingots share each bare name?
    let mut name_counts: HashMap<String, usize> = HashMap::new();
    for &ing in closure {
        if ing.base(db) == root_base {
            continue;
        }
        *name_counts.entry(ingot_namespace(db, ing)).or_default() += 1;
    }

    let mut map: HashMap<Url, String> = HashMap::new();
    map.insert(root_base.clone(), String::new());

    // Reserve the root namespace and the builtin markers so a user ingot can never shadow them.
    let mut used: HashSet<String> =
        HashSet::from([String::new(), "std".to_string(), "core".to_string()]);

    // Deterministic assignment order, independent of graph traversal.
    let mut others: Vec<hir::Ingot<'_>> = closure
        .iter()
        .copied()
        .filter(|ing| ing.base(db) != root_base)
        .collect();
    others.sort_by_key(|ing| ing.base(db).to_string());

    for ing in others {
        let name = ingot_namespace(db, ing);
        // Bare name when unique; otherwise qualify by version (then index) for stable uniqueness.
        let mut candidate = if name_counts.get(&name).copied().unwrap_or(0) > 1 {
            match ing.version(db) {
                Some(version) => format!("{name}-{}", sanitize_filename(&version.to_string())),
                None => name.clone(),
            }
        } else {
            name.clone()
        };
        let stem = candidate.clone();
        let mut n = 2;
        while used.contains(&candidate)
            || ing.files(db).iter().any(|(_, file)| {
                file.kind(db) == Some(IngotFileKind::Source)
                    && file.path(db).as_ref().is_some_and(|path| {
                        root_source_keys.contains(&format!("{candidate}/{path}"))
                    })
            })
        {
            candidate = format!("{stem}-{n}");
            n += 1;
        }
        used.insert(candidate.clone());
        map.insert(ing.base(db), candidate);
    }
    map
}

/// `{ alias: namespace }` for an ingot's direct dependencies; builtin `std`/`core` map to the
/// literal markers `"std"`/`"core"` (the verifier's compiler provides them). Non-builtin targets
/// resolve to the same unique namespace assigned in [`assign_namespaces`].
fn ingot_dependencies_json(
    db: &DriverDataBase,
    ingot: hir::Ingot<'_>,
    namespaces: &HashMap<Url, String>,
) -> serde_json::Map<String, serde_json::Value> {
    let mut map = serde_json::Map::new();
    for (alias, url) in ingot.dependencies(db) {
        let value = match url.ingot(db) {
            Some(dep) if matches!(dep.kind(db), IngotKind::Std) => "std".to_string(),
            Some(dep) if matches!(dep.kind(db), IngotKind::Core) => "core".to_string(),
            Some(dep) => namespaces
                .get(&dep.base(db))
                .cloned()
                .unwrap_or_else(|| ingot_namespace(db, dep)),
            None => alias.to_string(),
        };
        map.insert(alias.to_string(), serde_json::Value::String(value));
    }
    map
}

/// Write one `<out>/<Contract>.metadata.json` per built contract, conforming to the Solidity
/// Contract Metadata schema (`version: 1`) adapted to Fe. The artifact is a sufficient,
/// deterministic recompilation input (see `openspec/changes/add-fe-metadata-json`).
fn write_metadata_artifacts(
    db: &DriverDataBase,
    ingot: hir::Ingot<'_>,
    names_to_build: &[String],
    opt_level: OptLevel,
    out_dir: &Utf8Path,
    report_dir: Option<&Utf8Path>,
) -> bool {
    let mut had_errors = false;
    let profile = db.compilation_settings().profile(db);
    let profile = profile.as_str();

    // Root sources (unprefixed) drive contract -> path/top_mod resolution.
    let mut root_sources: BTreeMap<String, (TopLevelMod<'_>, String)> = BTreeMap::new();
    collect_ingot_sources_prefixed(db, ingot, None, &mut root_sources);

    // Full source set = root + transitive non-builtin dependencies, each under its unique namespace.
    let closure = non_builtin_closure(db, ingot);
    let root_source_keys = root_sources.keys().cloned().collect();
    let namespaces = assign_namespaces(db, ingot, &closure, &root_source_keys);
    let mut all_sources = root_sources.clone();
    for &dep in &closure {
        if dep.base(db) == ingot.base(db) {
            continue;
        }
        let ns = namespaces.get(&dep.base(db)).cloned().unwrap_or_default();
        collect_ingot_sources_prefixed(db, dep, Some(&ns), &mut all_sources);
    }

    // Map each root contract name to its source path and defining module.
    let mut name_to_loc: BTreeMap<String, (String, TopLevelMod<'_>)> = BTreeMap::new();
    for (path_key, (top_mod, _)) in &root_sources {
        for contract in top_mod.all_contracts(db) {
            if let Some(name) = contract.name(db).to_opt() {
                name_to_loc
                    .entry(name.data(db).to_string())
                    .or_insert_with(|| (path_key.clone(), *top_mod));
            }
        }
        for &func in top_mod.all_funcs(db) {
            if func.top_mod(db) != *top_mod {
                continue;
            }
            match func.manual_contract_root_attr(db) {
                Some(ManualContractRootAttr::Init { contract_name })
                | Some(ManualContractRootAttr::Runtime { contract_name }) => {
                    name_to_loc
                        .entry(contract_name.data(db).to_string())
                        .or_insert_with(|| (path_key.clone(), *top_mod));
                }
                Some(ManualContractRootAttr::Error(_)) | None => {}
            }
        }
    }

    // `sources` object (shared across this ingot's contracts), with keccak256 + content.
    let mut sources_obj = serde_json::Map::new();
    for (key, (_, contents)) in &all_sources {
        sources_obj.insert(
            key.clone(),
            serde_json::json!({
                "keccak256": keccak256_hex(contents.as_bytes()),
                "content": contents,
            }),
        );
    }

    // `settings.ingots` (shared): one entry per non-builtin ingot, with resolved effective values.
    //
    // `arithmetic` is the *effective* per-ingot value from `Ingot::arithmetic_mode`, which already
    // folds in `dependency-arithmetic` forcing (forcing is applied only to external/workspace
    // edges; see `forced_dependency_arithmetic_for`). Because the post-forcing result is recorded
    // here, the `dependencies` map deliberately keeps only alias -> namespace and drops the
    // internal-vs-external edge type: a verifier reproduces the bytecode by pinning each ingot's
    // `arithmetic` and reconstructing dependencies as plain path edges WITHOUT re-applying
    // `dependency-arithmetic`. Restoring faithful workspace edges + re-forcing would re-derive the
    // same values, but only the pin-and-don't-force procedure is guaranteed sufficient. See
    // `test_cli_build_emit_metadata_preserves_dependency_arithmetic_across_edge_types`.
    let ingots_arr: Vec<serde_json::Value> = closure
        .iter()
        .map(|&ing| {
            let namespace = namespaces.get(&ing.base(db)).cloned().unwrap_or_default();
            let dep_arith = resolve_dependency_arithmetic_mode(
                ing.config(db).as_ref(),
                ing.workspace_config(db).as_ref(),
                profile,
            );
            serde_json::json!({
                "name": ingot_namespace(db, ing),
                "version": ing.version(db).map(|v| v.to_string()),
                "namespace": namespace,
                "arithmetic": arithmetic_str(ing.arithmetic_mode(db)),
                "dependencyArithmetic": dependency_arithmetic_str(dep_arith),
                "dependencies": serde_json::Value::Object(ingot_dependencies_json(db, ing, &namespaces)),
            })
        })
        .collect();

    let root_arithmetic = arithmetic_str(ingot.arithmetic_mode(db));
    let root_dep_arithmetic = dependency_arithmetic_str(resolve_dependency_arithmetic_mode(
        ingot.config(db).as_ref(),
        ingot.workspace_config(db).as_ref(),
        profile,
    ));

    // Compiler identity (shared across contracts). `version` is the release SemVer; `commit` pins
    // the exact source revision when available, disambiguating non-release builds that still carry
    // the previous release's version number.
    let mut compiler = serde_json::Map::new();
    compiler.insert(
        "version".to_string(),
        serde_json::Value::String(env!("CARGO_PKG_VERSION").to_string()),
    );
    if let Some(hash) = option_env!("FE_GIT_HASH").filter(|hash| !hash.is_empty()) {
        compiler.insert(
            "commit".to_string(),
            serde_json::Value::String(hash.to_string()),
        );
    }
    let compiler = serde_json::Value::Object(compiler);

    for name in names_to_build {
        let Some((source_path, top_mod)) = name_to_loc.get(name) else {
            continue;
        };

        let abi_value = match crate::abi::generate_contract_abi(db, *top_mod, name) {
            Ok(Some(result)) => {
                for warning in &result.warnings {
                    eprintln!("Warning: {warning}");
                }
                serde_json::from_str::<serde_json::Value>(&result.json)
                    .unwrap_or_else(|_| serde_json::json!([]))
            }
            Ok(None) => serde_json::json!([]),
            Err(err) => {
                eprintln!("Error: Failed to generate ABI for {name}: {err}");
                had_errors = true;
                serde_json::json!([])
            }
        };

        let mut compilation_target = serde_json::Map::new();
        compilation_target.insert(source_path.clone(), serde_json::Value::String(name.clone()));

        let value = serde_json::json!({
            "version": 1,
            "language": "Fe",
            "compiler": compiler.clone(),
            "sources": serde_json::Value::Object(sources_obj.clone()),
            "settings": {
                "compilationTarget": serde_json::Value::Object(compilation_target),
                "optimizer": { "level": opt_level.to_string() },
                "arithmetic": root_arithmetic,
                "dependencyArithmetic": root_dep_arithmetic,
                "evmVersion": "osaka",
                "ingots": ingots_arr.clone(),
            },
            "output": { "abi": abi_value },
        });

        let serialized = match serde_json::to_string_pretty(&value) {
            Ok(s) => s,
            Err(err) => {
                eprintln!("Error: Failed to serialize metadata for {name}: {err}");
                had_errors = true;
                continue;
            }
        };
        let base = sanitize_filename(name);
        let path = out_dir.join(format!("{base}.metadata.json"));
        let report_path = report_dir.map(|dir| dir.join(format!("{base}.metadata.json")));
        if let Err(err) = fs::write(path.as_std_path(), &serialized) {
            eprintln!("Error: Failed to write metadata: {err}");
            had_errors = true;
            continue;
        }
        if let Some(p) = report_path.as_ref() {
            let _ = fs::write(p.as_std_path(), &serialized);
        }
        println!("Wrote {out_dir}/{base}.metadata.json");
    }
    had_errors
}

fn remove_file_if_exists(path: &Utf8Path) -> std::io::Result<()> {
    match fs::remove_file(path.as_std_path()) {
        Ok(()) => Ok(()),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(err) => Err(err),
    }
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
    if emit.abi {
        parts.push("abi");
    }
    if emit.metadata {
        parts.push("metadata");
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
        fs::write(deploy_path.as_std_path(), bytecode)
            .map_err(|err| format!("Failed to write {deploy_path}: {err}"))?;
        if let Some(dir) = report_dir {
            let deploy_path = dir.join(format!("{base}.bin"));
            let _ = fs::write(deploy_path.as_std_path(), bytecode);
        }
    }
    if emit.runtime_bytecode {
        let runtime_path = out_dir.join(format!("{base}.runtime.bin"));
        fs::write(runtime_path.as_std_path(), runtime_bytecode)
            .map_err(|err| format!("Failed to write {runtime_path}: {err}"))?;
        if let Some(dir) = report_dir {
            let runtime_path = dir.join(format!("{base}.runtime.bin"));
            let _ = fs::write(runtime_path.as_std_path(), runtime_bytecode);
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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;
    use tempfile::tempdir;

    #[test]
    fn describe_emit_selection_lists_abi() {
        let emit = EmitSelection {
            bytecode: false,
            runtime_bytecode: false,
            ir: false,
            abi: true,
            metadata: false,
        };
        assert_eq!(describe_emit_selection(emit), "abi");
    }

    #[test]
    fn write_abi_artifacts_copies_into_report_dir() {
        let code = include_str!("../tests/fixtures/cli_output/emit_abi/abi_contract.fe");

        let temp_input = tempdir().expect("tempdir");
        let file_path = temp_input.path().join("test.fe");
        let url = Url::from_file_path(&file_path).expect("file path to url");
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(&mut db, url, Some(code.to_string()));
        let top_mod = db.top_mod(file);
        let temp = tempdir().expect("tempdir");
        let out_dir =
            Utf8PathBuf::from_path_buf(temp.path().join("out")).expect("utf8 output path");
        let report_dir =
            Utf8PathBuf::from_path_buf(temp.path().join("report")).expect("utf8 report path");
        fs::create_dir_all(out_dir.as_std_path()).expect("create output dir");
        fs::create_dir_all(report_dir.as_std_path()).expect("create report dir");

        let names = vec!["Foo".to_string()];
        let had_errors = write_abi_artifacts(&db, top_mod, &names, &out_dir, Some(&report_dir));
        assert!(!had_errors, "unexpected ABI write error");

        let out_abi = out_dir.join("Foo.abi.json");
        let report_abi = report_dir.join("Foo.abi.json");
        assert!(out_abi.exists(), "missing output ABI");
        assert!(report_abi.exists(), "missing report ABI");

        let out_json = fs::read_to_string(out_abi.as_std_path()).expect("read output ABI");
        let report_json = fs::read_to_string(report_abi.as_std_path()).expect("read report ABI");
        assert_eq!(out_json, report_json);

        let abi: Value = serde_json::from_str(&out_json).expect("parse output ABI");
        let function = abi
            .as_array()
            .expect("abi array")
            .iter()
            .find(|entry| entry["type"] == "function")
            .expect("function entry");
        assert_eq!(function["name"], "ping");
    }

    #[test]
    fn write_abi_artifacts_removes_stale_empty_outputs() {
        let temp_input = tempdir().expect("tempdir");
        let file_path = temp_input.path().join("test.fe");
        let url = Url::from_file_path(&file_path).expect("file path to url");
        let temp = tempdir().expect("tempdir");
        let out_dir =
            Utf8PathBuf::from_path_buf(temp.path().join("out")).expect("utf8 output path");
        let report_dir =
            Utf8PathBuf::from_path_buf(temp.path().join("report")).expect("utf8 report path");
        fs::create_dir_all(out_dir.as_std_path()).expect("create output dir");
        fs::create_dir_all(report_dir.as_std_path()).expect("create report dir");

        let mut first_db = DriverDataBase::default();
        let first_code = include_str!("../tests/fixtures/cli_output/emit_abi/abi_contract.fe");
        let first_file =
            first_db
                .workspace()
                .touch(&mut first_db, url.clone(), Some(first_code.to_string()));
        let first_top_mod = first_db.top_mod(first_file);
        let names = vec!["Foo".to_string()];
        let had_errors = write_abi_artifacts(
            &first_db,
            first_top_mod,
            &names,
            &out_dir,
            Some(&report_dir),
        );
        assert!(!had_errors, "unexpected ABI write error");
        assert!(out_dir.join("Foo.abi.json").exists(), "missing output ABI");
        assert!(
            report_dir.join("Foo.abi.json").exists(),
            "missing report ABI"
        );

        let mut second_db = DriverDataBase::default();
        let second_code = r#"
msg FooMsg {
    #[selector = 0x12345678]
    Ping -> u256,
}

pub contract Foo {
    recv FooMsg {
        Ping -> u256 {
            return 1
        }
    }
}
"#;
        let second_file =
            second_db
                .workspace()
                .touch(&mut second_db, url, Some(second_code.to_string()));
        let second_top_mod = second_db.top_mod(second_file);
        let had_errors = write_abi_artifacts(
            &second_db,
            second_top_mod,
            &names,
            &out_dir,
            Some(&report_dir),
        );
        assert!(!had_errors, "unexpected ABI cleanup error");
        assert!(
            !out_dir.join("Foo.abi.json").exists(),
            "stale output ABI should be removed"
        );
        assert!(
            !report_dir.join("Foo.abi.json").exists(),
            "stale report ABI should be removed"
        );
    }
}
