use common::{InputDb, file::File};
use driver::{DriverDataBase, generation::prepare_file};
use fe_hir::test_db::HirAnalysisTestDb;
use salsa::Setter;

/// Keep the fixture's HIR database and transfer only the driver's frozen output.
pub fn prepare_generation(db: &mut HirAnalysisTestDb, file: File) {
    let url = file.url(db).expect("fixture has a source URL");
    let mut driver = DriverDataBase::default();
    driver
        .compiler_options()
        .set_recovery_mode(&mut driver)
        .to(db.compiler_options().recovery_mode(db));
    driver
        .compilation_settings()
        .set_profile(&mut driver)
        .to(db.compilation_settings().profile(db).clone());
    driver
        .compilation_settings()
        .set_target(&mut driver)
        .to(db.compilation_settings().target(db));
    driver
        .compilation_settings()
        .set_platform(&mut driver)
        .to(db.compilation_settings().platform(db));
    let driver_file = driver
        .workspace()
        .touch(&mut driver, url, Some(file.text(db).clone()));
    let prepared = prepare_file(&driver, driver_file).expect("fixture generation should succeed");
    let prepared = prepared.as_ref().unwrap_or(&driver);
    let prepared_inputs = prepared.generation_inputs();
    let inputs = db.generation_inputs();
    inputs
        .set_requests(db)
        .to(prepared_inputs.requests(prepared).clone());
    inputs
        .set_functions(db)
        .to(prepared_inputs.functions(prepared).clone());
}
