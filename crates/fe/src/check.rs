use camino::Utf8PathBuf;
use codegen::{Backend, BackendKind};
use common::InputDb;
use driver::DriverDataBase;
use hir::hir_def::{HirIngot, TopLevelMod};
use mir::{fmt as mir_fmt, layout, lower_module};
use url::Url;

use crate::report::{
    copy_input_into_report, create_dir_all_utf8, create_report_staging_dir, enable_panic_report,
    normalize_report_out_path, panic_payload_to_string, tar_gz_dir, write_report_meta,
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

pub fn check(
    path: &Utf8PathBuf,
    dump_mir: bool,
    emit_yul_min: bool,
    backend_name: &str,
    report_out: Option<&Utf8PathBuf>,
    report_failed_only: bool,
) {
    // Parse backend selection
    let backend_kind: BackendKind = match backend_name.parse() {
        Ok(kind) => kind,
        Err(err) => {
            eprintln!("❌ Error: {err}");
            std::process::exit(1);
        }
    };
    let backend = backend_kind.create();
    let mut db = DriverDataBase::default();

    let report_root = report_out.map(|out| {
        let staging = create_report_staging_dir("target/fe-check-report-staging");
        let out = normalize_report_out_path(out);
        (out, staging)
    });

    let report_ctx = report_root.as_ref().map(|(_, staging)| {
        let inputs_dir = staging.join("inputs");
        create_dir_all_utf8(&inputs_dir);
        copy_input_into_report(path, &inputs_dir);
        create_dir_all_utf8(&staging.join("artifacts"));
        create_dir_all_utf8(&staging.join("errors"));
        write_report_meta(staging, "fe check report", None);
        ReportContext {
            root_dir: staging.clone(),
        }
    });

    // Determine if we're dealing with a single file or an ingot directory
    let has_errors = if path.is_file() && path.extension() == Some("fe") {
        check_single_file(
            &mut db,
            path,
            dump_mir,
            emit_yul_min,
            backend_kind,
            backend.as_ref(),
            report_ctx.as_ref(),
        )
    } else if path.is_dir() {
        check_ingot(
            &mut db,
            path,
            dump_mir,
            emit_yul_min,
            backend_kind,
            backend.as_ref(),
            report_ctx.as_ref(),
        )
    } else {
        eprintln!("❌ Error: Path must be either a .fe file or a directory containing fe.toml");
        std::process::exit(1);
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

    if has_errors {
        std::process::exit(1);
    }
}

fn check_single_file(
    db: &mut DriverDataBase,
    file_path: &Utf8PathBuf,
    dump_mir: bool,
    emit_yul_min: bool,
    backend_kind: BackendKind,
    backend: &dyn Backend,
    report: Option<&ReportContext>,
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
            emit_codegen(db, top_mod, backend);
        }
        if let Some(report) = report {
            write_check_artifacts(db, top_mod, backend_kind, backend, report);
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
    backend_kind: BackendKind,
    backend: &dyn Backend,
    report: Option<&ReportContext>,
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
            emit_codegen(db, root_mod, backend);
        }
        if let Some(report) = report {
            write_check_artifacts(db, root_mod, backend_kind, backend, report);
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

fn emit_codegen(db: &DriverDataBase, top_mod: TopLevelMod<'_>, backend: &dyn Backend) {
    println!("=== {} backend ===", backend.name());
    match backend.compile(db, top_mod, layout::EVM_LAYOUT) {
        Ok(output) => match output {
            codegen::BackendOutput::Yul(yul) => {
                println!("{yul}");
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
    has_errors: bool,
) {
    let mut out = String::new();
    out.push_str("fe check report\n");
    out.push_str(&format!("path: {path}\n"));
    out.push_str(&format!("backend: {backend}\n"));
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
    report: &ReportContext,
) {
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
        backend.compile(db, top_mod, layout::EVM_LAYOUT)
    })) {
        Ok(Ok(output)) => match output {
            codegen::BackendOutput::Yul(yul) => {
                write_report_file(report, "artifacts/backend_output.yul", &yul);
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
            }
        },
        Ok(Err(err)) => {
            write_report_file(report, "errors/codegen_error.txt", &format!("{err}"));
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
        }
    }
}
