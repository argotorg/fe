use common::InputDb;
use driver::DriverDataBase;
use fe_semantic_query::Api;
use hir::lower::map_file_to_mod;
use url::Url;

fn touch(db: &mut DriverDataBase, path: &std::path::Path, content: &str) -> common::file::File {
    db.workspace()
        .touch(db, Url::from_file_path(path).unwrap(), Some(content.to_string()))
}

#[test]
fn goto_shape_scalar_for_unambiguous() {
    let mut db = DriverDataBase::default();
    let tmp = std::env::temp_dir().join("goto_shape_scalar.fe");
    let content = r#"
mod m { pub struct Foo {} }
fn f() { let _x: m::Foo }
"#;
    let file = touch(&mut db, &tmp, content);
    let top_mod = map_file_to_mod(&db, file);
    let cursor = content.find("Foo }").unwrap() as u32;
    let api = Api::new(&db);
    let candidates = api.goto_candidates_at_cursor(top_mod, parser::TextSize::from(cursor));
    assert_eq!(candidates.len(), 1, "expected scalar goto for unambiguous target");
}

#[test]
fn goto_shape_array_for_ambiguous_imports() {
    let mut db = DriverDataBase::default();
    let tmp = std::env::temp_dir().join("goto_shape_ambiguous.fe");
    // Two types with the same name T imported into the same scope, then used in type position.
    let content = r#"
mod a { pub struct T {} }
mod b { pub struct T {} }
use a::T
use b::T
fn f() { let _x: T }
"#;
    let file = touch(&mut db, &tmp, content);
    let top_mod = map_file_to_mod(&db, file);
    let cursor = content.rfind("T }").unwrap() as u32;
    let api = Api::new(&db);
    let candidates = api.goto_candidates_at_cursor(top_mod, parser::TextSize::from(cursor));
    assert!(candidates.len() >= 2, "expected array goto for ambiguous target; got {}", candidates.len());
}
