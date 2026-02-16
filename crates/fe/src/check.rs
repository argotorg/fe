use std::collections::HashSet;

use camino::Utf8PathBuf;
use codegen::{Backend, BackendKind, OptLevel};
use common::{
    InputDb,
    config::{Config, WorkspaceConfig},
    file::IngotFileKind,
};
use driver::DriverDataBase;
use hir::hir_def::{HirIngot, TopLevelMod};
use mir::{fmt as mir_fmt, layout, lower_module};
use resolver::ResolutionHandler;
use resolver::ingot::{FeTomlProbe, infer_config_kind};
use resolver::{Resolver, files::ancestor_fe_toml_dirs};
use url::Url;

use crate::report::{
    copy_input_into_report, create_dir_all_utf8, create_report_staging_dir, enable_panic_report,
    is_verifier_error_text, normalize_report_out_path, panic_payload_to_string, tar_gz_dir,
    write_report_meta,
};

#[derive(Debug, Clone)]
struct ReportContext {
    root_dir: Utf8PathBuf,
}

struct ResolvedMember {
    path: Utf8PathBuf,
    url: Url,
}

enum CheckTarget {
    StandaloneFile(Utf8PathBuf),
    Directory(Utf8PathBuf),
    WorkspaceMember(Utf8PathBuf),
}

struct ConfigProbe;

impl ResolutionHandler<resolver::files::FilesResolver> for ConfigProbe {
    type Item = FeTomlProbe;

    fn handle_resolution(
        &mut self,
        _description: &Url,
        resource: resolver::files::FilesResource,
    ) -> Self::Item {
        for file in &resource.files {
            if file.path.as_str().ends_with("fe.toml") {
                return FeTomlProbe::Present {
                    kind_hint: infer_config_kind(&file.content),
                };
            }
        }
        FeTomlProbe::Missing
    }
}

fn write_report_file(report: &ReportContext, rel: &str, contents: &str) {
    let path = report.root_dir.join(rel);
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    let _ = std::fs::write(path, contents);
}

fn write_codegen_report_error(report: &ReportContext, contents: &str) {
    write_report_file(report, "errors/codegen_error.txt", contents);
    if is_verifier_error_text(contents) {
        write_report_file(report, "errors/verifier_error.txt", contents);
    }
}

pub fn check(
    path: &Utf8PathBuf,
    dump_mir: bool,
    emit_yul_min: bool,
    backend_name: &str,
    opt_level: OptLevel,
    report_out: Option<&Utf8PathBuf>,
    report_failed_only: bool,
) -> Result<bool, String> {
    let backend_kind: BackendKind = backend_name.parse()?;
    let backend = backend_kind.create();
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
            copy_input_into_report(path, &inputs_dir)?;
            create_dir_all_utf8(&staging.join("artifacts"))?;
            create_dir_all_utf8(&staging.join("errors"))?;
            write_report_meta(staging, "fe check report", None);
            Ok(ReportContext {
                root_dir: staging.clone(),
            })
        })
        .transpose()?;

    let target = resolve_check_target(&mut db, path)?;

    let has_errors = match target {
        CheckTarget::StandaloneFile(file_path) => check_single_file(
            &mut db,
            &file_path,
            dump_mir,
            emit_yul_min,
            backend_kind,
            backend.as_ref(),
            opt_level,
            report_ctx.as_ref(),
        ),
        CheckTarget::WorkspaceMember(dir_path) => check_ingot(
            &mut db,
            &dir_path,
            dump_mir,
            emit_yul_min,
            backend_kind,
            backend.as_ref(),
            opt_level,
            report_ctx.as_ref(),
        ),
        CheckTarget::Directory(dir_path) => check_directory(
            &mut db,
            &dir_path,
            dump_mir,
            emit_yul_min,
            backend_kind,
            backend.as_ref(),
            opt_level,
            report_ctx.as_ref(),
        ),
    };

    if let Some((out, staging)) = report_root {
        let should_write = !report_failed_only || has_errors;
        if should_write {
            write_check_manifest(
                &staging,
                path,
                dump_mir,
                emit_yul_min,
                backend_name,
                opt_level,
                has_errors,
            );
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

fn resolve_check_target(
    db: &mut DriverDataBase,
    path: &Utf8PathBuf,
) -> Result<CheckTarget, String> {
    let arg = path.as_str();
    let is_name = is_name_candidate(arg);
    let path_exists = path.exists();

    if path.is_file() {
        if path.extension() == Some("fe") {
            return Ok(CheckTarget::StandaloneFile(path.clone()));
        }
        return Err("Path must be either a .fe file or a directory containing fe.toml".into());
    }

    let name_match = if is_name {
        resolve_member_by_name(db, arg)?
    } else {
        None
    };

    let path_member = if is_name && path_exists {
        resolve_member_by_path(db, path)?
    } else {
        None
    };

    if path_exists && name_match.is_some() {
        match (&name_match, &path_member) {
            (Some(name_member), Some(path_member)) => {
                if name_member.url == path_member.url {
                    return Ok(CheckTarget::WorkspaceMember(path_member.path.clone()));
                }
                return Err(format!(
                    "Argument \"{arg}\" matches a workspace member name but does not match the provided path"
                ));
            }
            (Some(_), None) => {
                return Err(format!(
                    "Argument \"{arg}\" matches a workspace member name but does not match the provided path"
                ));
            }
            _ => {}
        }
    }

    if let Some(name_member) = name_match {
        return Ok(CheckTarget::WorkspaceMember(name_member.path));
    }

    if path_exists {
        if path.is_dir() && path.join("fe.toml").is_file() {
            return Ok(CheckTarget::Directory(path.clone()));
        }
        return Err("Path must be either a .fe file or a directory containing fe.toml".into());
    }

    Err("Path must be either a .fe file or a directory containing fe.toml".into())
}

#[allow(clippy::too_many_arguments)]
fn check_directory(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    dump_mir: bool,
    emit_yul_min: bool,
    backend_kind: BackendKind,
    backend: &dyn Backend,
    opt_level: OptLevel,
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

    let config = match config_from_db(db, dir_path) {
        Ok(Some(config)) => config,
        Ok(None) => {
            eprintln!("❌ Error: No fe.toml file found in the root directory");
            return true;
        }
        Err(err) => {
            eprintln!("❌ Error: {err}");
            return true;
        }
    };

    match config {
        Config::Workspace(workspace) => check_workspace(
            db,
            dir_path,
            *workspace,
            dump_mir,
            emit_yul_min,
            backend_kind,
            backend,
            opt_level,
            report,
        ),
        Config::Ingot(_) => check_ingot_url(
            db,
            &ingot_url,
            dump_mir,
            emit_yul_min,
            backend_kind,
            backend,
            opt_level,
            report,
        ),
    }
}

fn is_name_candidate(value: &str) -> bool {
    !value.is_empty() && value.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn resolve_member_by_name(
    db: &mut DriverDataBase,
    name: &str,
) -> Result<Option<ResolvedMember>, String> {
    let cwd = std::env::current_dir()
        .map_err(|err| format!("Failed to read current directory: {err}"))?;
    let cwd = Utf8PathBuf::from_path_buf(cwd)
        .map_err(|_| "Current directory is not valid UTF-8".to_string())?;
    let workspace_root = find_workspace_root(db, &cwd)?;
    let Some(workspace_root) = workspace_root else {
        return Ok(None);
    };
    let workspace_url = dir_url(&workspace_root)?;
    let mut matches = db.dependency_graph().workspace_members_by_name(
        db,
        &workspace_url,
        &smol_str::SmolStr::new(name),
    );
    if matches.is_empty() {
        return Ok(None);
    }
    if matches.len() > 1 {
        return Err(format!(
            "Multiple workspace members named \"{name}\"; specify a path instead"
        ));
    }
    let member = matches.pop().map(|member| ResolvedMember {
        path: workspace_root.join(member.path.as_str()),
        url: member.url,
    });
    Ok(member)
}

fn resolve_member_by_path(
    db: &mut DriverDataBase,
    path: &Utf8PathBuf,
) -> Result<Option<ResolvedMember>, String> {
    if !path.is_dir() {
        return Ok(None);
    }
    let workspace_root = find_workspace_root(db, path)?;
    let Some(workspace_root) = workspace_root else {
        return Ok(None);
    };
    let workspace_url = dir_url(&workspace_root)?;
    let members = db
        .dependency_graph()
        .workspace_member_records(db, &workspace_url);
    let canonical = path
        .canonicalize_utf8()
        .map_err(|_| format!("Error: invalid or non-existent directory path: {path}"))?;
    let target_url = Url::from_directory_path(canonical.as_str())
        .map_err(|_| format!("Error: invalid directory path: {path}"))?;

    Ok(members
        .into_iter()
        .find(|member| member.url == target_url)
        .map(|member| ResolvedMember {
            path: workspace_root.join(member.path.as_str()),
            url: member.url,
        }))
}

fn find_workspace_root(
    db: &mut DriverDataBase,
    start: &Utf8PathBuf,
) -> Result<Option<Utf8PathBuf>, String> {
    let dirs = ancestor_fe_toml_dirs(start.as_std_path());
    for dir in dirs {
        let dir = Utf8PathBuf::from_path_buf(dir)
            .map_err(|_| "Encountered non UTF-8 workspace path".to_string())?;
        let url = dir_url(&dir)?;
        let mut resolver = resolver::ingot::minimal_files_resolver();
        let summary = resolver
            .resolve(&mut ConfigProbe, &url)
            .map_err(|err| err.to_string())?;
        if summary.kind_hint() == Some(resolver::ingot::ConfigKind::Workspace) {
            if db
                .dependency_graph()
                .workspace_member_records(db, &url)
                .is_empty()
            {
                let _ = driver::init_ingot(db, &url);
            }
            return Ok(Some(dir));
        }
    }
    Ok(None)
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
        .map_err(|_| format!("Error: invalid or non-existent directory path: {path}"))
}

#[allow(clippy::too_many_arguments)]
fn check_ingot_url(
    db: &mut DriverDataBase,
    ingot_url: &Url,
    dump_mir: bool,
    emit_yul_min: bool,
    backend_kind: BackendKind,
    backend: &dyn Backend,
    opt_level: OptLevel,
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
    check_ingot_and_dependencies(
        db,
        ingot_url,
        dump_mir,
        emit_yul_min,
        backend_kind,
        backend,
        opt_level,
        report,
        &mut seen,
    )
}

#[allow(clippy::too_many_arguments)]
fn check_workspace(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    workspace_config: WorkspaceConfig,
    dump_mir: bool,
    emit_yul_min: bool,
    backend_kind: BackendKind,
    backend: &dyn Backend,
    opt_level: OptLevel,
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
            eprintln!("❌ Error resolving workspace members: {err}");
            return true;
        }
    };

    if members.is_empty() {
        eprintln!("⚠️  No workspace members found");
        return false;
    }

    let mut seen = HashSet::new();
    let mut has_errors = false;
    for member in members {
        let member_url = member.url;
        let member_has_errors = check_ingot_and_dependencies(
            db,
            &member_url,
            dump_mir,
            emit_yul_min,
            backend_kind,
            backend,
            opt_level,
            report,
            &mut seen,
        );
        has_errors |= member_has_errors;
    }

    has_errors
}

#[allow(clippy::too_many_arguments)]
fn check_ingot_and_dependencies(
    db: &mut DriverDataBase,
    ingot_url: &Url,
    dump_mir: bool,
    emit_yul_min: bool,
    backend_kind: BackendKind,
    backend: &dyn Backend,
    opt_level: OptLevel,
    report: Option<&ReportContext>,
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

    if !ingot_has_source_files(db, ingot) {
        eprintln!("❌ Error: Could not find source files for ingot {ingot_url}");
        return true;
    }

    let diags = db.run_on_ingot(ingot);
    let mut has_errors = false;

    if !diags.is_empty() {
        diags.emit(db);
        if let Some(report) = report {
            let formatted = diags.format_diags(db);
            write_report_file(report, "errors/diagnostics.txt", &formatted);
        }
        has_errors = true;
    } else {
        let root_mod = ingot.root_mod(db);
        if dump_mir {
            dump_module_mir(db, root_mod);
        }
        if emit_yul_min {
            emit_codegen(db, root_mod, backend, opt_level);
        }
        if let Some(report) = report {
            has_errors |=
                write_check_artifacts(db, root_mod, backend_kind, backend, opt_level, report);
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
            eprintln!("❌ Error: Could not find source files for ingot {dependency_url}");
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
            eprintln!("❌ Error in downstream ingot");
        } else {
            eprintln!("❌ Errors in downstream ingots");
        }

        if let Some(report) = report {
            let mut out = String::new();
            for (dependency_url, diags) in &dependency_errors {
                out.push_str(&format!("dependency: {dependency_url}\n"));
                out.push_str(&diags.format_diags(db));
                out.push('\n');
            }
            write_report_file(report, "errors/dependency_diagnostics.txt", &out);
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

#[allow(clippy::too_many_arguments)]
fn check_single_file(
    db: &mut DriverDataBase,
    file_path: &Utf8PathBuf,
    dump_mir: bool,
    emit_yul_min: bool,
    backend_kind: BackendKind,
    backend: &dyn Backend,
    opt_level: OptLevel,
    report: Option<&ReportContext>,
) -> bool {
    // Create a file URL for the single .fe file
    let canonical = match file_path.canonicalize_utf8() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("❌ Error: Cannot canonicalize {file_path}: {e}");
            return true;
        }
    };
    let file_url = match Url::from_file_path(&canonical) {
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
            if let Some(report) = report {
                let formatted = diags.format_diags(db);
                write_report_file(report, "errors/diagnostics.txt", &formatted);
            }
            return true;
        }
        if dump_mir {
            dump_module_mir(db, top_mod);
        }
        if emit_yul_min {
            emit_codegen(db, top_mod, backend, opt_level);
        }
        if let Some(report) = report
            && write_check_artifacts(db, top_mod, backend_kind, backend, opt_level, report)
        {
            return true;
        }
    } else {
        eprintln!("❌ Error: Could not process file {file_path}");
        return true;
    }

    false
}

#[allow(clippy::too_many_arguments)]
fn check_ingot(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    dump_mir: bool,
    emit_yul_min: bool,
    backend_kind: BackendKind,
    backend: &dyn Backend,
    opt_level: OptLevel,
    report: Option<&ReportContext>,
) -> bool {
    let mut seen = HashSet::new();
    check_ingot_inner(
        db,
        dir_path,
        dump_mir,
        emit_yul_min,
        backend_kind,
        backend,
        opt_level,
        report,
        &mut seen,
    )
}

#[allow(clippy::too_many_arguments)]
fn check_ingot_inner(
    db: &mut DriverDataBase,
    dir_path: &Utf8PathBuf,
    dump_mir: bool,
    emit_yul_min: bool,
    backend_kind: BackendKind,
    backend: &dyn Backend,
    opt_level: OptLevel,
    report: Option<&ReportContext>,
    seen: &mut HashSet<Url>,
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
    if !seen.insert(ingot_url.clone()) {
        return false;
    }
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
        if let Some(report) = report {
            let formatted = diags.format_diags(db);
            write_report_file(report, "errors/diagnostics.txt", &formatted);
        }
        has_errors = true;
    } else {
        let root_mod = ingot.root_mod(db);
        if dump_mir {
            dump_module_mir(db, root_mod);
        }
        if emit_yul_min {
            emit_codegen(db, root_mod, backend, opt_level);
        }
        if let Some(report) = report {
            has_errors |=
                write_check_artifacts(db, root_mod, backend_kind, backend, opt_level, report);
        }
    }

    // Collect all dependencies with errors
    let mut dependency_errors = Vec::new();
    for dependency_url in db.dependency_graph().dependency_urls(db, &ingot_url) {
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

    // Print dependency errors if any exist
    if !dependency_errors.is_empty() {
        has_errors = true;
        if dependency_errors.len() == 1 {
            eprintln!("❌ Error in downstream ingot");
        } else {
            eprintln!("❌ Errors in downstream ingots");
        }

        if let Some(report) = report {
            let mut out = String::new();
            for (dependency_url, diags) in &dependency_errors {
                out.push_str(&format!("dependency: {dependency_url}\n"));
                out.push_str(&diags.format_diags(db));
                out.push('\n');
            }
            write_report_file(report, "errors/dependency_diagnostics.txt", &out);
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

fn emit_codegen(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    backend: &dyn Backend,
    opt_level: OptLevel,
) {
    println!("=== {} backend ===", backend.name());
    match backend.compile(db, top_mod, layout::EVM_LAYOUT, opt_level) {
        Ok(output) => match output {
            codegen::BackendOutput::Yul {
                source,
                solc_optimize,
            } => {
                println!("// solc optimizer enabled: {solc_optimize}");
                println!("{source}");
            }
            codegen::BackendOutput::Bytecode(bytes) => {
                println!("bytecode ({} bytes):", bytes.len());
                println!("{}", hex::encode(&bytes));
            }
        },
        Err(err) => eprintln!("⚠️  failed to compile: {err}"),
    }
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
    emit_yul_min: bool,
    backend: &str,
    opt_level: OptLevel,
    has_errors: bool,
) {
    let mut out = String::new();
    out.push_str("fe check report\n");
    out.push_str(&format!("path: {path}\n"));
    out.push_str(&format!("backend: {backend}\n"));
    out.push_str(&format!("opt_level: {opt_level}\n"));
    out.push_str(&format!("dump_mir: {dump_mir}\n"));
    out.push_str(&format!("emit_yul_min: {emit_yul_min}\n"));
    out.push_str(&format!(
        "status: {}\n",
        if has_errors { "failed" } else { "ok" }
    ));
    out.push_str(&format!("fe_version: {}\n", env!("CARGO_PKG_VERSION")));
    let _ = std::fs::write(staging.join("manifest.txt"), out);
}

fn write_check_artifacts(
    db: &DriverDataBase,
    top_mod: TopLevelMod<'_>,
    backend_kind: BackendKind,
    backend: &dyn Backend,
    opt_level: OptLevel,
    report: &ReportContext,
) -> bool {
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

    match backend_kind {
        BackendKind::Yul => match codegen::emit_module_yul(db, top_mod) {
            Ok(yul) => write_report_file(report, "artifacts/yul_module.yul", &yul),
            Err(err) => {
                write_report_file(report, "artifacts/yul_module_error.txt", &format!("{err}"))
            }
        },
        BackendKind::Sonatina => {
            match codegen::emit_module_sonatina_ir(db, top_mod) {
                Ok(ir) => write_report_file(report, "artifacts/sonatina_ir.txt", &ir),
                Err(err) => {
                    write_report_file(report, "artifacts/sonatina_ir_error.txt", &format!("{err}"))
                }
            }
            match codegen::validate_module_sonatina_ir(db, top_mod) {
                Ok(v) => write_report_file(report, "artifacts/sonatina_validate.txt", &v),
                Err(err) => write_report_file(
                    report,
                    "artifacts/sonatina_validate_error.txt",
                    &format!("{err}"),
                ),
            }
        }
    }

    let _hook = enable_panic_report(
        report
            .root_dir
            .join("errors")
            .join("codegen_panic_full.txt"),
    );

    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        backend.compile(db, top_mod, layout::EVM_LAYOUT, opt_level)
    })) {
        Ok(Ok(output)) => match output {
            codegen::BackendOutput::Yul {
                source,
                solc_optimize,
            } => {
                write_report_file(report, "artifacts/backend_output.yul", &source);
                write_report_file(
                    report,
                    "artifacts/backend_output_yul_solc_optimize.txt",
                    &format!("{solc_optimize}\n"),
                );
                false
            }
            codegen::BackendOutput::Bytecode(bytes) => {
                write_report_file(
                    report,
                    "artifacts/backend_bytecode.hex",
                    &hex::encode(&bytes),
                );
                write_report_file(
                    report,
                    "artifacts/backend_bytecode_len.txt",
                    &format!("{}\n", bytes.len()),
                );
                false
            }
        },
        Ok(Err(err)) => {
            let err = format!("{err}");
            write_codegen_report_error(report, &err);
            true
        }
        Err(payload) => {
            write_report_file(
                report,
                "errors/codegen_panic.txt",
                &format!(
                    "backend panicked while compiling: {}",
                    panic_payload_to_string(payload.as_ref())
                ),
            );
            true
        }
    }
}
