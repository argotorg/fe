use common::InputDb;
use driver::DriverDataBase;
use fe_mir::{ValueOrigin, layout, lower_module};
use url::Url;

#[test]
fn trait_default_ne_preserves_receiver() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///trait_default_ne.fe").unwrap();
    let src = r#"
use std::evm::Address

fn main(a: Address, b: Address) -> bool {
    a != b
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let module = lower_module(&db, top_mod).expect("lowered MIR");

    // `Address` implements `core::ops::Eq::eq` but relies on the trait-default `ne`.
    // If monomorphization forgets to substitute `Self`, it is treated as ZST and the
    // receiver gets canonicalized to `()`, producing `other != 0`-style codegen.
    let ne_fn = module
        .functions
        .iter()
        .find(|func| func.symbol_name.starts_with("ne__Address"))
        .expect("expected monomorphized `Address::ne`");

    let self_local = *ne_fn
        .body
        .param_locals
        .first()
        .expect("`ne` should have a receiver param");

    let self_ty = ne_fn.body.local(self_local).ty;
    let self_size = layout::ty_size_bytes(&db, self_ty).expect("receiver layout known");
    assert_ne!(
        self_size, 0,
        "`ne` receiver should not be zero-sized after monomorphization"
    );

    let self_used = ne_fn
        .body
        .values
        .iter()
        .any(|value| matches!(value.origin, ValueOrigin::Local(local) if local == self_local));
    assert!(self_used, "`ne` should use its receiver param");
}
