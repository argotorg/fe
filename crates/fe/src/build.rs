use camino::Utf8PathBuf;
use codegen::emit_module_yul;
use driver::{BuildOptions, DriverDataBase, build_path};
use hir::hir_def::TopLevelMod;

pub fn build(path: &Utf8PathBuf, dump_mir: bool) {
    let had_errors = build_path(
        path,
        BuildOptions {
            dump_mir,
            emit: &emit_yul,
        },
    );
    if had_errors {
        std::process::exit(1);
    }
}

fn emit_yul(db: &DriverDataBase, top_mod: TopLevelMod<'_>) -> Result<(), String> {
    emit_module_yul(db, top_mod)
        .map(|yul| {
            println!("=== Yul ===");
            println!("{yul}");
            println!();
        })
        .map_err(|err| err.to_string())
}
