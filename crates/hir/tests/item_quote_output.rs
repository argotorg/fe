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

/// A quoted `type` is an associated type member. Spliced at module level it is
/// reported where it lands, never dropped or published as a module alias.
#[test]
fn quoted_type_alias_is_refused_as_a_module_item() {
    let source = "use core::meta::{Code, Item}\nconst fn make() -> Code<Item> { quote { type Generated = u256 } }\n";
    let mut input = HirAnalysisTestDb::default();
    let file = input.new_stand_alone("quoted-alias.fe".into(), source);
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
    let file = output.new_stand_alone("quoted-alias.fe".into(), source);
    output
        .generation_inputs()
        .set_requests(&mut output)
        .to(vec![request]);
    let (module, _) = output.top_mod(file);
    let errors = fe_hir::lower::generation_lowering_errors(&output, module);
    assert!(
        errors.iter().any(|error| error
            .message
            .contains("a quoted type is an associated type member")),
        "{errors:?}"
    );
    assert!(
        !module
            .children_non_nested(&output)
            .any(|item| matches!(item, ItemKind::TypeAlias(_))),
        "a quoted type must not be published as a module alias"
    );
}

#[test]
fn inherent_header_carries_provider_and_lowers_plan_functions_and_constants() {
    use parser::ast::{self, prelude::*};

    let source = r#"use core::meta::{Code, Item}
struct Inspect {}
struct Pair { first: u256, second: u256 }
const fn make() -> Code<Item> {
    quote {
        pub const COUNT: u256 = 2
        pub const fn sum(self) -> u256 {
            self.first + self.second + Self::COUNT
        }
    }
}
impl Pair with (Inspect)
const CHECK: u256 = Pair { first: 10, second: 30 }.sum()
"#;
    let mut input = HirAnalysisTestDb::default();
    let file = input.new_stand_alone("inherent-header-lower.fe".into(), source);
    let (module, _) = input.top_mod(file);
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
    let root = fe_hir::lower::parse_file_impl(&input, module).root.clone();
    let root = ast::Root::cast(parser::SyntaxNode::new_root(root)).unwrap();
    let header_range = root
        .items()
        .unwrap()
        .into_iter()
        .find_map(|item| match item.kind() {
            Some(ast::ItemKind::Impl(impl_)) if impl_.derive_provider().is_some() => {
                Some(impl_.syntax().text_range())
            }
            _ => None,
        })
        .expect("inherent header in source");
    let mut request = code.freeze_request(&input, file).unwrap();
    request.request_impl = Some(header_range);

    let mut output = HirAnalysisTestDb::default();
    let file = output.new_stand_alone("inherent-header-lower.fe".into(), source);
    output
        .generation_inputs()
        .set_requests(&mut output)
        .to(vec![request]);
    let (module, _) = output.top_mod(file);
    let impls = module.all_impls(&output);
    assert_eq!(impls.len(), 1);
    let impl_ = impls[0];
    let provider_path = impl_
        .derive_provider(&output)
        .expect("inherent header must carry derive_provider PathId");
    assert_eq!(provider_path.pretty_print(&output), "Inspect");
    assert_eq!(impl_.hir_consts(&output).len(), 1);
    assert_eq!(impl_.funcs(&output).count(), 1);
    let check_const = module
        .children_non_nested(&output)
        .find_map(|item| match item {
            ItemKind::Const(c)
                if c.name(&output)
                    .to_opt()
                    .is_some_and(|n| n.data(&output) == "CHECK") =>
            {
                Some(c)
            }
            _ => None,
        })
        .expect("CHECK const");
    let EvalOutcome::Ready(check_val) = eval_body_owner_const(
        &output,
        BodyOwner::Const(check_const),
        GenericSubst::for_body_owner(&output, BodyOwner::Const(check_const), vec![]),
    ) else {
        panic!("CHECK evaluation failed");
    };
    assert!(
        matches!(
            check_val.value(&output),
            SemConstValue::Scalar { value: SemConstScalar::Int { value }, .. } if value.to_string() == "42"
        ),
        "expected 42, got {check_val:?}"
    );

    let bad_source = r#"use core::meta::{Code, Item}
struct Inspect {}
struct Pair { first: u256, second: u256 }
const fn make() -> Code<Item> {
    quote {
        impl Pair {
            pub const fn zero() -> u256 { 0 }
        }
    }
}
impl Pair with (Inspect)
"#;
    let mut bad_in = HirAnalysisTestDb::default();
    let bad_file = bad_in.new_stand_alone("inherent-header-bad.fe".into(), bad_source);
    let (bad_mod, _) = bad_in.top_mod(bad_file);
    let bad_owner = BodyOwner::Func(find_func(&bad_in, bad_mod, "make"), None);
    let EvalOutcome::Ready(bad_val) = eval_body_owner_const(
        &bad_in,
        bad_owner,
        GenericSubst::for_body_owner(&bad_in, bad_owner, vec![]),
    ) else {
        panic!("bad quote evaluation failed");
    };
    let SemConstValue::ItemCode { code: bad_code, .. } = bad_val.value(&bad_in) else {
        panic!("provider did not return item code");
    };
    let bad_root = fe_hir::lower::parse_file_impl(&bad_in, bad_mod)
        .root
        .clone();
    let bad_root = ast::Root::cast(parser::SyntaxNode::new_root(bad_root)).unwrap();
    let bad_header_range = bad_root
        .items()
        .unwrap()
        .into_iter()
        .find_map(|item| match item.kind() {
            Some(ast::ItemKind::Impl(impl_)) if impl_.derive_provider().is_some() => {
                Some(impl_.syntax().text_range())
            }
            _ => None,
        })
        .expect("inherent header in bad_source");
    let mut bad_req = bad_code.freeze_request(&bad_in, bad_file).unwrap();
    bad_req.request_impl = Some(bad_header_range);
    let mut bad_out = HirAnalysisTestDb::default();
    let bad_file = bad_out.new_stand_alone("inherent-header-bad.fe".into(), bad_source);
    bad_out
        .generation_inputs()
        .set_requests(&mut bad_out)
        .to(vec![bad_req]);
    let (bad_mod, _) = bad_out.top_mod(bad_file);
    let errors = fe_hir::lower::generation_lowering_errors(&bad_out, bad_mod);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert_eq!(errors[0].primary_range, bad_header_range);
    assert!(
        errors[0].message.contains(
            "A member of an inherent header must be a function or a constant; the quotation at inherent-header-bad.fe:5 writes an `impl`"
        ),
        "{}",
        errors[0].message
    );
}
