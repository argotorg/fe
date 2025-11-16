use camino::Utf8PathBuf;
use codegen::emit_module_yul;
use cranelift_entity::EntityRef;
use driver::{
    DriverDataBase,
    project::{Target, collect_dependency_errors, report_dependency_errors, resolve_target},
};
use hir::hir_def::{ExprId, HirIngot, PatId, StmtId, TopLevelMod};
use mir::{MirInst, Terminator, ValueId, lower_module};

pub fn build(path: &Utf8PathBuf, dump_mir: bool) {
    let mut db = DriverDataBase::default();
    let target = match resolve_target(&mut db, path) {
        Ok(target) => target,
        Err(_) => std::process::exit(1),
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
                if dump_mir {
                    dump_module_mir(&db, top_mod);
                }
                emit_yul(&db, top_mod);
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
                    if dump_mir {
                        dump_module_mir(&db, root_mod);
                    }
                    emit_yul(&db, root_mod);
                }

                let dependency_errors = collect_dependency_errors(&db, &ingot_target);
                if report_dependency_errors(&db, dependency_errors) {
                    has_errors = true;
                }
            } else {
                eprintln!("❌ Error: Could not resolve ingot from directory");
                has_errors = true;
            }
        }
    }

    if has_errors {
        std::process::exit(1);
    }
}

fn emit_yul(db: &DriverDataBase, top_mod: TopLevelMod<'_>) {
    match emit_module_yul(db, top_mod) {
        Ok(results) => {
            for result in results {
                match result {
                    Ok(yul) => {
                        println!("=== Yul ===");
                        println!("{yul}");
                        println!();
                    }
                    Err(err) => eprintln!("⚠️  yul emission skipped: {err}"),
                }
            }
        }
        Err(err) => eprintln!("⚠️  failed to lower MIR for yul emission: {err}"),
    }
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
