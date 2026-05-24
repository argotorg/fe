use common::{InputDb, facts::OriginFactIndex};
use driver::DriverDataBase;
use fe_mir::{RuntimePackage, build_runtime_package, runtime_package_origin_facts};
use hir::hir_def::{Func, TopLevelMod};
use url::Url;

fn find_func<'db>(db: &'db DriverDataBase, top_mod: TopLevelMod<'db>, name: &str) -> Func<'db> {
    top_mod
        .all_funcs(db)
        .iter()
        .copied()
        .find(|func| {
            func.name(db)
                .to_opt()
                .is_some_and(|ident| ident.data(db) == name)
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"))
}

fn package_statement_and_terminator_count<'db>(
    db: &'db DriverDataBase,
    package: RuntimePackage<'db>,
) -> usize {
    package
        .functions(db)
        .iter()
        .map(|function| {
            let body = function.instance(db).body(db);
            body.blocks
                .iter()
                .map(|block| block.stmts.len() + 1)
                .sum::<usize>()
        })
        .sum()
}

#[test]
fn runtime_package_origin_facts_cover_statements_and_terminators() {
    let mut db = DriverDataBase::default();
    let file_url = Url::parse("file:///runtime_origin_facts.fe").unwrap();
    let file = db.workspace().touch(
        &mut db,
        file_url,
        Some(
            r#"
fn main() -> u256 {
    let x: u256 = 1
    x
}
"#
            .to_string(),
        ),
    );
    let top_mod = db.top_mod(file);
    let _ = find_func(&db, top_mod, "main");
    let package = build_runtime_package(&db, top_mod).expect("runtime package should build");
    let facts = runtime_package_origin_facts(&db, package);
    let index = OriginFactIndex::from_facts(&facts);

    assert_eq!(
        facts.origin_node_count(),
        package_statement_and_terminator_count(&db, package)
    );
    assert_eq!(index.node_count(), facts.origin_node_count());
    assert_eq!(facts.origin_link_count(), 0);
}
