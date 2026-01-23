use common::InputDb;
use driver::DriverDataBase;
use fe_codegen::{Backend, SonatinaBackend};
use mir::layout::EVM_LAYOUT;
use std::path::PathBuf;
use url::Url;

#[test]
fn sonatina_compiles_selected_fixtures() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let fixtures = [
        "tests/fixtures/literal_add.fe",
        "tests/fixtures/struct_init.fe",
        "tests/fixtures/match_literal.fe",
        "tests/fixtures/match_enum_with_data.fe",
        "tests/fixtures/intrinsic_ops.fe",
        "tests/fixtures/revert.fe",
        "tests/fixtures/caller.fe",
        "tests/fixtures/keccak_intrinsic.fe",
    ];

    for rel_path in fixtures {
        let path = manifest_dir.join(rel_path);
        let content = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read fixture {path:?}: {e}"));

        let mut db = DriverDataBase::default();
        let file_url = Url::from_file_path(&path).expect("fixture path should be absolute");
        db.workspace()
            .touch(&mut db, file_url.clone(), Some(content));
        let file = db
            .workspace()
            .get(&db, &file_url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);

        let output = SonatinaBackend
            .compile(&db, top_mod, EVM_LAYOUT)
            .unwrap_or_else(|e| panic!("sonatina failed to compile {path:?}: {e}"));

        let bytecode = output
            .as_bytecode()
            .unwrap_or_else(|| panic!("sonatina output is not bytecode for {path:?}"));
        assert!(
            !bytecode.is_empty(),
            "sonatina produced empty bytecode for {path:?}"
        );
    }
}
