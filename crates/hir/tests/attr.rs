mod test_db;

use fe_hir::{SpannedHirDb, span::LazySpan};
use test_db::HirAnalysisTestDb;

#[test]
fn lang_attr_value_span_resolves() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("lang_attr.fe".into(), "#[lang = \"alloc\"]\nfn foo() {}");
    let (top_mod, _) = db.top_mod(file);

    let func = top_mod.all_funcs(&db)[0];
    let value_span = func.span().attributes().attr(0).into_normal_attr().value();

    let resolved = value_span.resolve(&db).expect("resolve value span");
    let text = resolved.file.text(&db);
    let start: usize = resolved.range.start().into();
    let end: usize = resolved.range.end().into();

    assert_eq!(&text[start..end], "\"alloc\"");
}
