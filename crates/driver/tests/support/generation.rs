use common::{InputDb, file::File};
use fe_driver::{DriverDataBase, generation::prepare_file};

/// Check the whole generation path from a written file to its diagnostics.
pub(super) fn program_errors(db: &DriverDataBase, file: File) -> String {
    match prepare_file(db, file) {
        Err(error) => error.cause,
        Ok(prepared) => {
            let url = file.url(db).expect("indexed input file");
            let view = prepared.as_ref().unwrap_or(db);
            let file = view
                .workspace()
                .get(view, &url)
                .expect("file in output view");
            let diagnostics = view.run_on_top_mod(view.top_mod(file));
            let errors = diagnostics.format_diags(view);
            assert!(diagnostics.has_errors(view), "expected an invalid program");
            errors
        }
    }
}
