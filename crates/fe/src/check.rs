use camino::Utf8PathBuf;
use common::InputDb;
use cranelift_entity::EntityRef;
use driver::DriverDataBase;
use hir::hir_def::{ExprId, HirIngot, PatId, StmtId, TopLevelMod};
use mir::{lower_module, MirInst, Terminator, ValueId};
use url::Url;

pub fn check(path: &Utf8PathBuf, dump_mir: bool) {
    let mut db = DriverDataBase::default();

    // Determine if we're dealing with a single file or an ingot directory
    let has_errors = if path.is_file() && path.extension() == Some("fe") {
        check_single_file(&mut db, path, dump_mir)
    } else if path.is_dir() {
        check_ingot(&mut db, path, dump_mir)
    } else {
        eprintln!("❌ Error: Path must be either a .fe file or a directory containing fe.toml");
        std::process::exit(1);
    };

    if has_errors {
        std::process::exit(1);
    }
}

fn check_single_file(db: &mut DriverDataBase, file_path: &Utf8PathBuf, dump_mir: bool) -> bool {
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
    } else {
        eprintln!("❌ Error: Could not process file {file_path}");
        return true;
    }

    false
}

fn check_ingot(db: &mut DriverDataBase, dir_path: &Utf8PathBuf, dump_mir: bool) -> bool {
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
    let init_diagnostics = driver::init_ingot(db, &ingot_url);

    // Handle workspace setup diagnostics if any
    if !init_diagnostics.is_empty() {
        let mut has_init_issues = false;
        for diagnostic in &init_diagnostics {
            eprintln!("❌ {diagnostic}");
            has_init_issues = true;
        }
        if has_init_issues {
            return true;
        }
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
    } else if dump_mir {
        let root_mod = ingot.root_mod(db);
        dump_module_mir(db, root_mod);
    }

    // Collect all dependencies with errors
    let mut dependency_errors = Vec::new();
    for dependency_url in db.graph().dependency_urls(db, &ingot_url) {
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

fn dump_module_mir(db: &DriverDataBase, top_mod: TopLevelMod<'_>) {
    match lower_module(db, top_mod) {
        Ok(mir_module) => {
            println!("=== MIR for module ===");
            for func in mir_module.functions {
                let name = func
                    .func
                    .name(db)
                    .to_opt()
                    .map(|id| id.data(db).to_string())
                    .unwrap_or_else(|| "<anonymous>".into());
                println!("fn {name}:");
                for (idx, block) in func.body.blocks.iter().enumerate() {
                    println!("  bb{idx}:");
                    for inst in &block.insts {
                        println!("    {}", format_inst(db, inst));
                    }
                    println!("    terminator: {}", format_terminator(&block.terminator));
                }
                println!();
            }
        }
        Err(err) => eprintln!("failed to lower MIR: {err}"),
    }
}

fn format_inst(db: &DriverDataBase, inst: &MirInst<'_>) -> String {
    match inst {
        MirInst::Let { stmt, pat, ty, value } => {
            let value_str = value
                .as_ref()
                .map(|val| value_label(*val))
                .unwrap_or_else(|| "_".into());
            if let Some(ty) = ty.as_ref() {
                format!(
                    "{}: let {}: {} = {}",
                    stmt_label(*stmt),
                    pat_label(*pat),
                    ty.pretty_print(db),
                    value_str
                )
            } else {
                format!(
                    "{}: let {} = {}",
                    stmt_label(*stmt),
                    pat_label(*pat),
                    value_str
                )
            }
        }
        MirInst::Assign { stmt, target, value } => format!(
            "{}: assign {} = {}",
            stmt_label(*stmt),
            expr_label(*target),
            value_label(*value)
        ),
        MirInst::AugAssign {
            stmt,
            target,
            value,
            op,
        } => format!(
            "{}: {:?} {} {}",
            stmt_label(*stmt),
            op,
            expr_label(*target),
            value_label(*value)
        ),
        MirInst::Eval { stmt, value } => {
            format!("{}: eval {}", stmt_label(*stmt), value_label(*value))
        }
        MirInst::ForLoop { stmt, pat, iter, body } => format!(
            "{}: for {} in {} -> {}",
            stmt_label(*stmt),
            pat_label(*pat),
            value_label(*iter),
            expr_label(*body)
        ),
        MirInst::WhileLoop { stmt, cond, body } => format!(
            "{}: while {} -> {}",
            stmt_label(*stmt),
            value_label(*cond),
            expr_label(*body)
        ),
        MirInst::Break { stmt } => format!("{}: break", stmt_label(*stmt)),
        MirInst::Continue { stmt } => format!("{}: continue", stmt_label(*stmt)),
        MirInst::Return { stmt, value } => match value {
            Some(val) => format!("{}: return {}", stmt_label(*stmt), value_label(*val)),
            None => format!("{}: return", stmt_label(*stmt)),
        },
    }
}

fn format_terminator(term: &Terminator) -> String {
    match term {
        Terminator::Return(Some(val)) => format!("return {}", value_label(*val)),
        Terminator::Return(None) => "return".into(),
        Terminator::Goto { target } => format!("goto bb{}", target.index()),
        Terminator::Branch { cond, then_bb, else_bb } => format!(
            "if {} -> bb{}, bb{}",
            value_label(*cond),
            then_bb.index(),
            else_bb.index()
        ),
        Terminator::Switch {
            discr,
            targets,
            default,
        } => {
            let parts = targets
                .iter()
                .map(|t| format!("{}: bb{}", t.value, t.block.index()))
                .collect::<Vec<_>>();
            let arms = parts.join(", ");
            format!(
                "switch {} [{arms}] default bb{}",
                value_label(*discr),
                default.index()
            )
        }
        Terminator::Unreachable => "unreachable".into(),
    }
}

fn stmt_label(stmt: StmtId) -> String {
    format!("s{}", stmt.index())
}

fn pat_label(pat: PatId) -> String {
    format!("p{}", pat.index())
}

fn expr_label(expr: ExprId) -> String {
    format!("e{}", expr.index())
}

fn value_label(val: ValueId) -> String {
    format!("v{}", val.index())
}
