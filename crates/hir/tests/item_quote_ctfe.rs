use fe_hir::{
    analysis::{
        semantic::{
            EvalOutcome, GenericSubst, SemConstId, SemConstScalar, SemConstValue,
            eval_body_owner_const, eval_body_owner_const_with_args,
        },
        ty::{corelib::resolve_lib_type_path, ty_check::BodyOwner},
    },
    core::meta::{MetaValue, RecordInput},
    test_db::{HirAnalysisTestDb, find_func},
};
use parser::ast::prelude::*;

#[test]
fn reflected_count_becomes_an_item_template_in_the_existing_ctfe_instance() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("item-provider.fe".into(),
        "use core::meta::{Code, Item, Record, field_count}\nstruct Pair { first: u256, second: bool }\nconst fn make(_ record: Record) -> Code<Item> { let n = field_count(record)\n quote { fn generated_count() -> usize { ${n} } } }\n");
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let func = find_func(&db, module, "make");
    let owner = BodyOwner::Func(func, None);
    let carrier = resolve_lib_type_path(&db, func.scope(), "core::meta::Record").unwrap();
    let target = resolve_lib_type_path(&db, func.scope(), "Pair").unwrap();
    let record = RecordInput::for_authored_type(&db, target).unwrap();
    let argument = SemConstId::new(
        &db,
        SemConstValue::Meta {
            ty: carrier,
            value: MetaValue::Record(record),
        },
    );
    let outcome = eval_body_owner_const_with_args(
        &db,
        owner,
        GenericSubst::for_body_owner(&db, owner, vec![]),
        vec![argument],
    );
    let EvalOutcome::Ready(value) = outcome else {
        panic!("{outcome:?}")
    };
    let SemConstValue::ItemCode { code, .. } = value.value(&db) else {
        panic!("{value:?}")
    };
    assert_eq!(code.producer().owner(&db), owner);
    assert_eq!(code.holes().len(), 1);
    assert!(
        matches!(code.holes()[0].value(&db), SemConstValue::Scalar { value: SemConstScalar::Int { value }, .. } if value.to_string() == "2")
    );
    let quote = code.syntax(&db).unwrap();
    assert!(
        quote
            .syntax()
            .text()
            .to_string()
            .contains("generated_count")
    );
    assert!(
        !module.all_funcs(&db).iter().any(|f| f
            .name(&db)
            .to_opt()
            .is_some_and(|name| name.data(&db) == "generated_count")),
        "quoted output must not enter the authored input view"
    );
}

#[test]
fn item_constructor_refuses_statement_fragments_and_over_budget_templates() {
    for (template, cause) in [
        (
            "quote { 42 }".to_string(),
            "item quotation requires item declarations",
        ),
        (
            format!(
                "quote {{ fn generated() -> u256 {{ {}\n 42 }} }}",
                (0..800)
                    .map(|n| format!("let value_{n} = 0\n"))
                    .collect::<String>()
            ),
            "construction node limit of 4096",
        ),
        (
            format!(
                "quote {{ fn generated() -> u256 {{ // {}\n 42 }} }}",
                "x".repeat(4100)
            ),
            "template byte limit of 4096",
        ),
        (
            format!(
                "quote {{ fn generated() -> u256 {{ {} 0 }} }}",
                (0..65).map(|_| "${1} + ").collect::<String>()
            ),
            "hole limit of 64",
        ),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let source = format!(
            "use core::meta::{{Code, Item}}\nconst fn make() -> Code<Item> {{ {template} }}\n"
        );
        let file = db.new_stand_alone("bad-item-provider.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let owner = BodyOwner::Func(find_func(&db, module, "make"), None);
        let outcome =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
        assert!(matches!(outcome, EvalOutcome::Failed(_)), "{outcome:?}");
        assert!(format!("{outcome:?}").contains(cause), "{outcome:?}");
    }
}

#[test]
fn exponential_quote_splice_dag_is_refused_by_expanded_node_budget() {
    let mut db = HirAnalysisTestDb::default();
    let source = r#"use core::meta::{Code, Expr, Item}
const fn explode() -> Code<Item> {
    var q: Code<Expr> = quote { 1 + 1 }
    var i: usize = 0
    while i < 12 {
        q = quote { ${q} + ${q} }
        i += 1
    }
    quote {
        fn big() -> u256 {
            ${q}
        }
    }
}
"#;
    let file = db.new_stand_alone("dag-budget.fe".into(), source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "explode"), None);
    let outcome =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    assert!(matches!(outcome, EvalOutcome::Failed(_)), "{outcome:?}");
    assert!(
        format!("{outcome:?}").contains("expanded AST node limit of 4096"),
        "{outcome:?}"
    );
}
