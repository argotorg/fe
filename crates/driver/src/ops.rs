use camino::Utf8PathBuf;
use cranelift_entity::EntityRef;
use hir::hir_def::{ExprId, HirIngot, PatId, StmtId, TopLevelMod};
use mir::{MirInst, Terminator, ValueId, lower_module};

use crate::{
    DriverDataBase,
    project::{Target, collect_dependency_errors, report_dependency_errors, resolve_target},
};

pub struct BuildOptions<'a> {
    pub dump_mir: bool,
    pub emit:
        &'a (dyn for<'db> Fn(&'db DriverDataBase, TopLevelMod<'db>) -> Result<(), String> + 'a),
}

pub fn check_path(path: &Utf8PathBuf, show_downstream: bool) -> bool {
    let mut db = DriverDataBase::default();
    let target = match resolve_target(&mut db, path) {
        Ok(target) => target,
        Err(_) => return true,
    };

    let mut has_errors = false;
    match target {
        Target::SingleFile(single) => {
            let top_mod = db.top_mod(single.file());
            let diags = db.run_on_top_mod(top_mod);
            if !diags.is_empty() {
                eprintln!("errors in {}", single.file_url());
                eprintln!();
                diags.emit(&db);
                has_errors = true;
            }

            // Only report downstream issues for single-file mode when explicitly requested.
            if show_downstream && let Some(ingot_url) = single.ingot_url() {
                let ingot_target = crate::project::IngotTarget::from_url(ingot_url.clone());
                let dependency_errors = collect_dependency_errors(&db, &ingot_target);
                if report_dependency_errors(&db, dependency_errors, show_downstream) {
                    has_errors = true;
                }
            }
        }
        Target::Ingot(ingot_target) => {
            if let Some(ingot) = ingot_target.ingot(&db) {
                let diags = db.run_on_ingot(ingot);
                if !diags.is_empty() {
                    diags.emit(&db);
                    has_errors = true;
                } else {
                    let dependency_errors = collect_dependency_errors(&db, &ingot_target);
                    if report_dependency_errors(&db, dependency_errors, show_downstream) {
                        has_errors = true;
                    }
                }
            } else {
                eprintln!("❌ Error: Could not resolve ingot from directory");
                has_errors = true;
            }
        }
    }

    has_errors
}

pub fn build_path(path: &Utf8PathBuf, options: BuildOptions<'_>) -> bool {
    let mut db = DriverDataBase::default();
    let target = match resolve_target(&mut db, path) {
        Ok(target) => target,
        Err(_) => return true,
    };

    let mut has_errors = false;
    match target {
        Target::SingleFile(single) => {
            let top_mod = db.top_mod(single.file());
            let diags = db.run_on_top_mod(top_mod);
            if !diags.is_empty() {
                eprintln!("errors in {}", single.file_url());
                eprintln!();
                diags.emit(&db);
                has_errors = true;
            } else {
                if options.dump_mir {
                    dump_module_mir(&db, top_mod);
                }
                if let Err(err) = (options.emit)(&db, top_mod) {
                    eprintln!("⚠️  failed to emit build artifact: {err}");
                    has_errors = true;
                }
            }
        }
        Target::Ingot(ingot_target) => {
            if let Some(ingot) = ingot_target.ingot(&db) {
                let diags = db.run_on_ingot(ingot);
                if !diags.is_empty() {
                    diags.emit(&db);
                    has_errors = true;
                } else {
                    let root_mod = ingot.root_mod(&db);
                    if options.dump_mir {
                        dump_module_mir(&db, root_mod);
                    }
                    if let Err(err) = (options.emit)(&db, root_mod) {
                        eprintln!("⚠️  failed to emit build artifact: {err}");
                        has_errors = true;
                    }

                    let dependency_errors = collect_dependency_errors(&db, &ingot_target);
                    if report_dependency_errors(&db, dependency_errors, true) {
                        has_errors = true;
                    }
                }
            } else {
                eprintln!("❌ Error: Could not resolve ingot from directory");
                has_errors = true;
            }
        }
    }

    has_errors
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
        MirInst::Let {
            stmt,
            pat,
            ty,
            value,
        } => {
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
        MirInst::Assign {
            stmt,
            target,
            value,
        } => format!(
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
        MirInst::EvalExpr {
            expr,
            value,
            bind_value,
        } => {
            let bind_suffix = if *bind_value { " (bind)" } else { "" };
            format!(
                "{}: eval_expr{} {}",
                expr_label(*expr),
                bind_suffix,
                value_label(*value)
            )
        }
        MirInst::IntrinsicStmt { expr, op, args } => {
            let args = args
                .iter()
                .map(|arg| value_label(*arg))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}: intrinsic {:?}({args})", expr_label(*expr), op)
        }
    }
}

fn format_terminator(term: &Terminator) -> String {
    match term {
        Terminator::Return(Some(val)) => format!("return {}", value_label(*val)),
        Terminator::Return(None) => "return".into(),
        Terminator::Goto { target } => format!("goto bb{}", target.index()),
        Terminator::Branch {
            cond,
            then_bb,
            else_bb,
        } => format!(
            "if {} -> bb{}, bb{}",
            value_label(*cond),
            then_bb.index(),
            else_bb.index()
        ),
        Terminator::ReturnData { offset, size } => format!(
            "return_data {}, {}",
            value_label(*offset),
            value_label(*size)
        ),
        Terminator::Switch {
            discr,
            targets,
            default,
            ..
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
