use camino::Utf8PathBuf;
use driver::{
    DriverDataBase,
    project::{Target, collect_dependency_errors, report_dependency_errors, resolve_target},
};

pub fn check(path: &Utf8PathBuf) {
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
            }
        }
        Target::Ingot(ingot_target) => {
            if let Some(ingot) = ingot_target.ingot(&db) {
                let diags = db.run_on_ingot(ingot);
                if !diags.is_empty() {
                    diags.emit(&db);
                    has_errors = true;
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
