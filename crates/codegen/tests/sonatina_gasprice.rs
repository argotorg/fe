use common::{InputDb, ingot::IngotBaseUrl, stdlib::BUILTIN_STD_BASE_URL};
use driver::DriverDataBase;
use fe_codegen::emit_module_sonatina_ir;
use url::Url;

#[test]
fn sonatina_lower_runtime_supports_gasprice() {
    let mut db = DriverDataBase::default();

    // `std::evm::ops::gasprice` exists but is not exported publicly. For this test, expose a
    // small wrapper in the in-memory builtin stdlib so we can exercise the lowering path.
    let std_base = Url::parse(BUILTIN_STD_BASE_URL).expect("builtin std base url should parse");
    std_base.touch(
        &mut db,
        "src/evm/gasprice_test.fe".into(),
        Some(
            r#"
use ingot::evm::ops

pub fn gasprice() -> u256 {
    ops::gasprice()
}
"#
            .to_string(),
        ),
    );

    let evm_url = std_base
        .join("src/evm.fe")
        .expect("builtin std evm module url should join");
    let evm_file = db
        .workspace()
        .get(&db, &evm_url)
        .expect("builtin std evm module should exist");
    let mut evm_text = evm_file.text(&db).to_string();
    if !evm_text.contains("gasprice_test") {
        evm_text.push_str("\npub use gasprice_test::{self, *}\n");
        db.workspace().update(&mut db, evm_url, evm_text);
    }

    let module = r#"
use std::evm::gasprice

pub fn main() -> u256 {
    gasprice()
}
"#;

    let file_url = Url::parse("file:///sonatina_gasprice.fe").expect("test URL should parse");
    db.workspace()
        .touch(&mut db, file_url.clone(), Some(module.to_string()));
    let file = db
        .workspace()
        .get(&db, &file_url)
        .expect("test file should be loaded");
    let top_mod = db.top_mod(file);

    let ir = emit_module_sonatina_ir(&db, top_mod).expect("Sonatina IR should emit");
    assert!(
        ir.contains("evm_gas_price"),
        "expected Sonatina IR to contain evm_gas_price instruction:\n{ir}"
    );
}
