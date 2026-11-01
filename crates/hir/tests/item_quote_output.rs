use common::{InputDb, generation::FrozenFunctionTemplate};
use fe_hir::{
    analysis::{
        semantic::{
            EvalOutcome, GenericSubst, SemConstId, SemConstScalar, SemConstValue,
            eval_body_owner_const, eval_body_owner_const_with_args,
        },
        ty::{
            corelib::{resolve_lib_func_path, resolve_lib_type_path},
            ty_check::BodyOwner,
        },
    },
    core::meta::{MetaValue, RecordInput},
    hir_def::ItemKind,
    test_db::{HirAnalysisTestDb, find_func},
};
use salsa::Setter;

const SOURCE: &str = "use core::meta::{Code, Item, Record, field_count}\nmod author {\n use core::meta::{Code, Item, Record, field_count}\n const fn boost(_ n: usize) -> u256 { n as u256 + 40 }\n pub const fn make(_ record: Record) -> Code<Item> { let n = field_count(record)\n quote { const fn generated_count() -> u256 { boost(${n}) } } }\n}\nfn boost(_ n: usize) -> u256 { 999 }\nstruct Pair { first: u256, second: bool }\n";

fn collect(source: &str) -> FrozenFunctionTemplate {
    let mut input = HirAnalysisTestDb::default();
    let file = input.new_stand_alone("quote-output.fe".into(), source);
    let (module, _) = input.top_mod(file);
    input.assert_no_diags(module);
    let provider = resolve_lib_func_path(&input, module.scope(), "author::make").unwrap();
    let target = resolve_lib_type_path(&input, module.scope(), "Pair").unwrap();
    let carrier = resolve_lib_type_path(&input, module.scope(), "core::meta::Record").unwrap();
    let value = SemConstId::new(
        &input,
        SemConstValue::Meta {
            ty: carrier,
            value: MetaValue::Record(RecordInput::for_authored_type(&input, target).unwrap()),
        },
    );
    let owner = BodyOwner::Func(provider, None);
    let outcome = eval_body_owner_const_with_args(
        &input,
        owner,
        GenericSubst::for_body_owner(&input, owner, vec![]),
        vec![value],
    );
    let EvalOutcome::Ready(value) = outcome else {
        panic!("{outcome:?}")
    };
    let SemConstValue::ItemCode { code, .. } = value.value(&input) else {
        panic!("{value:?}")
    };
    assert!(!module.all_funcs(&input).iter().any(|f| {
        f.name(&input)
            .to_opt()
            .is_some_and(|n| n.data(&input) == "generated_count")
    }));
    code.freeze_function(&input, file).unwrap()
}

#[test]
fn one_output_view_checks_original_ast_and_retains_private_producer_hygiene() {
    let plan = collect(SOURCE);
    let mut output = HirAnalysisTestDb::default();
    let file = output.new_stand_alone("quote-output.fe".into(), SOURCE);
    output
        .generation_inputs()
        .set_functions(&mut output)
        .to(vec![plan]);
    let (module, _) = output.top_mod(file);
    output.assert_no_diags(module);
    let function = find_func(&output, module, "generated_count");
    assert!(
        fe_hir::analysis::name_resolution::is_scope_visible_from(
            &output,
            function.scope(),
            module.scope()
        ),
        "published private function must be visible from target"
    );
    assert_eq!(
        function.scope().parent(&output),
        Some(module.scope()),
        "published function retains target parent"
    );
    assert_eq!(
        function.body(&output).unwrap().containing_func(&output),
        Some(function),
        "generated body must retain its containing function"
    );
    let owner = BodyOwner::Func(function, None);
    let result = eval_body_owner_const(
        &output,
        owner,
        GenericSubst::for_body_owner(&output, owner, vec![]),
    );
    let EvalOutcome::Ready(value) = result else {
        panic!("{result:?}")
    };
    assert!(
        matches!(value.value(&output), SemConstValue::Scalar { value: SemConstScalar::Int { value }, .. } if value.to_string() == "42")
    );
}

#[test]
fn generated_signature_and_body_share_producer_type_identity() {
    let source = SOURCE
        .replace(
            "mod author {",
            "mod author { pub struct Stamp { value: usize }",
        )
        .replace(
            "quote { const fn generated_count() -> u256 { boost(${n}) } }",
            "quote { const fn generated_count() -> Stamp { Stamp { value: ${n} } } }",
        )
        .replace("struct Pair", "struct Stamp { value: bool }\nstruct Pair");
    let plan = collect(&source);
    let mut output = HirAnalysisTestDb::default();
    let file = output.new_stand_alone("quote-output.fe".into(), &source);
    output
        .generation_inputs()
        .set_functions(&mut output)
        .to(vec![plan]);
    let (module, _) = output.top_mod(file);
    output.assert_no_diags(module);
    let function = find_func(&output, module, "generated_count");
    assert!(
        fe_hir::analysis::name_resolution::is_scope_visible_from(
            &output,
            function.scope(),
            module.scope()
        ),
        "published private function must be visible from target"
    );
    assert_eq!(
        function.scope().parent(&output),
        Some(module.scope()),
        "published function retains target parent"
    );
    let producer_ty = resolve_lib_type_path(&output, module.scope(), "author::Stamp").unwrap();
    let target_ty = resolve_lib_type_path(&output, module.scope(), "Stamp").unwrap();
    assert_ne!(producer_ty, target_ty);
    assert_eq!(function.return_ty(&output), producer_ty);
    assert_eq!(
        function.body(&output).unwrap().containing_func(&output),
        Some(function)
    );
    let owner = BodyOwner::Func(function, None);
    let result = eval_body_owner_const(
        &output,
        owner,
        GenericSubst::for_body_owner(&output, owner, vec![]),
    );
    let EvalOutcome::Ready(value) = result else {
        panic!("{result:?}")
    };
    let SemConstValue::Struct { ty, fields } = value.value(&output) else {
        panic!("{value:?}")
    };
    assert_eq!(*ty, producer_ty);
    assert_eq!(fields.len(), 1);
    assert!(
        matches!(fields[0].value(&output), SemConstValue::Scalar { value: SemConstScalar::Int { value }, .. } if value.to_string() == "2")
    );
}

#[test]
fn generated_negative_signed_scalar_keeps_minimum_value() {
    let source = SOURCE
        .replace(
            "let n = field_count(record)",
            "let _ = field_count(record)\n let n: i8 = -128",
        )
        .replace(
            "const fn generated_count() -> u256 { boost(${n}) }",
            "const fn generated_count() -> i8 { ${n} }",
        );
    let plan = collect(&source);
    let mut output = HirAnalysisTestDb::default();
    let file = output.new_stand_alone("quote-output.fe".into(), &source);
    output
        .generation_inputs()
        .set_functions(&mut output)
        .to(vec![plan]);
    let (module, _) = output.top_mod(file);
    output.assert_no_diags(module);
    let function = find_func(&output, module, "generated_count");
    assert!(
        fe_hir::analysis::name_resolution::is_scope_visible_from(
            &output,
            function.scope(),
            module.scope()
        ),
        "published private function must be visible from target"
    );
    assert_eq!(
        function.scope().parent(&output),
        Some(module.scope()),
        "published function retains target parent"
    );
    let owner = BodyOwner::Func(function, None);
    let result = eval_body_owner_const(
        &output,
        owner,
        GenericSubst::for_body_owner(&output, owner, vec![]),
    );
    let EvalOutcome::Ready(value) = result else {
        panic!("{result:?}")
    };
    assert!(
        matches!(value.value(&output), SemConstValue::Scalar { value: SemConstScalar::Int { value }, .. } if value.to_string() == "-128")
    );
}

#[test]
fn frozen_item_request_publishes_const() {
    let source = r#"use core::meta::{Code, Item}
const fn make() -> Code<Item> {
    quote {
        const GENERATED: u256 = 7
    }
}
"#;
    let mut input = HirAnalysisTestDb::default();
    let file = input.new_stand_alone("generated-const.fe".into(), source);
    let (module, _) = input.top_mod(file);
    input.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&input, module, "make"), None);
    let EvalOutcome::Ready(value) = eval_body_owner_const(
        &input,
        owner,
        GenericSubst::for_body_owner(&input, owner, vec![]),
    ) else {
        panic!("item quote evaluation failed");
    };
    let SemConstValue::ItemCode { code, .. } = value.value(&input) else {
        panic!("provider did not return item code");
    };
    let request = code.freeze_request(&input, file).unwrap();

    let mut output = HirAnalysisTestDb::default();
    let file = output.new_stand_alone("generated-const.fe".into(), source);
    output
        .generation_inputs()
        .set_requests(&mut output)
        .to(vec![request]);
    let (module, _) = output.top_mod(file);
    output.assert_no_diags(module);

    let generated_const = module
        .children_non_nested(&output)
        .find(|item| {
            matches!(item, ItemKind::Const(_))
                && item
                    .name(&output)
                    .is_some_and(|name| name.data(&output) == "GENERATED")
        })
        .expect("generated const was not published");
    let ItemKind::Const(generated_const) = generated_const else {
        unreachable!()
    };
    let result = eval_body_owner_const(
        &output,
        BodyOwner::Const(generated_const),
        GenericSubst::for_body_owner(&output, BodyOwner::Const(generated_const), vec![]),
    );
    let EvalOutcome::Ready(value) = result else {
        panic!("generated const did not evaluate: {result:?}");
    };
    assert!(
        matches!(value.value(&output), SemConstValue::Scalar { value: SemConstScalar::Int { value }, .. } if value.to_string() == "7")
    );
}

#[test]
fn quoted_type_alias_is_explicitly_rejected_before_freezing() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "unsupported-quoted-alias.fe".into(),
        "use core::meta::{Code, Item}\nconst fn make() -> Code<Item> { quote { type Generated = u256 } }\n",
    );
    let (module, _) = db.top_mod(file);
    let rendered = fe_hir::test_db::format_diagnostics(&db, &db.run_on_top_mod(module));
    assert!(
        rendered.contains("only functions, constants and impls can be quoted"),
        "{rendered}"
    );
}
