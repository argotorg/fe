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

#[test]
fn extern_generics_normalize_assoc_type_args_for_decl_calls() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///extern_generics_assoc_normalize.fe").unwrap();
    let src = r#"
use core::abi::Abi
use std::abi::Sol

extern {
    fn id<T>(value: T) -> T
}

fn call_selector<A>(value: A::Selector) -> A::Selector
    where A: Abi
{
    id<A::Selector>(value)
}

fn main(x: u32) -> u32 {
    let a = id<u32>(x)
    let b = call_selector<Sol>(x)
    a + b
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let module = lower_module(&db, top_mod).expect("lowered MIR");

    let mut id_call_names = Vec::new();
    for func in &module.functions {
        for block in &func.body.blocks {
            for inst in &block.insts {
                if let MirInst::Assign {
                    rvalue: Rvalue::Call(call),
                    ..
                } = inst
                    && let Some(name) = call.resolved_name.as_ref()
                    && name.starts_with("id")
                {
                    id_call_names.push(name.clone());
                }
            }
        }
    }

    assert_eq!(id_call_names.len(), 2, "expected two calls to extern `id`");
    assert_eq!(
        id_call_names[0], id_call_names[1],
        "associated-type and concrete-type decl calls should mangle identically"
    );
}
