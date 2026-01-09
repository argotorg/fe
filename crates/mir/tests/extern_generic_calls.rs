use common::InputDb;
use driver::DriverDataBase;
use fe_mir::{MirInst, Rvalue, lower_module};
use url::Url;

#[test]
fn extern_generics_get_mangled_names() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///extern_generics.fe").unwrap();
    let src = r#"
extern {
    fn id<T>(value: T) -> T
}

fn main() {
    let a: u32 = 1
    let _ = id<u32>(a)
    let _ = id<bool>(true)
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let module = lower_module(&db, top_mod).expect("lowered MIR");

    let main_fn = module
        .functions
        .iter()
        .find(|func| func.symbol_name == "main")
        .expect("main function lowered");

    let mut call_names = Vec::new();
    for block in &main_fn.body.blocks {
        for inst in &block.insts {
            if let MirInst::Assign {
                rvalue: Rvalue::Call(call),
                ..
            } = inst
            {
                call_names.push(
                    call.resolved_name
                        .clone()
                        .expect("extern generic calls should be named"),
                );
            }
        }
    }

    assert_eq!(call_names.len(), 2);
    assert!(call_names.iter().all(|name| name.starts_with("id")));
    assert_ne!(call_names[0], call_names[1]);
}
