//! Regression cases for public review #1842. Run against the richer stage.
use common::{InputDb, generation::FrozenGenerationRequest};
use fe_hir::{
    analysis::{
        semantic::{
            EvalOutcome, GenericSubst, SemConstId, SemConstValue, eval_body_owner_const_with_args,
        },
        ty::{corelib::resolve_lib_type_path, ty_check::BodyOwner},
    },
    core::meta::{MetaValue, RecordInput},
    test_db::{HirAnalysisTestDb, find_func, format_diagnostics},
};
use salsa::Setter;

fn request(source: &str, target: &str) -> Result<FrozenGenerationRequest, String> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("richer-review.fe".into(), source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let func = find_func(&db, module, "make");
    let carrier = resolve_lib_type_path(&db, module.scope(), "core::meta::Record").unwrap();
    let ty = resolve_lib_type_path(&db, module.scope(), target).unwrap();
    let value = SemConstId::new(
        &db,
        SemConstValue::Meta {
            ty: carrier,
            value: MetaValue::Record(RecordInput::for_authored_type(&db, ty).unwrap()),
        },
    );
    let owner = BodyOwner::Func(func, None);
    match eval_body_owner_const_with_args(
        &db,
        owner,
        GenericSubst::for_body_owner(&db, owner, vec![]),
        vec![value],
    ) {
        EvalOutcome::Ready(value) => {
            let SemConstValue::ItemCode { code, .. } = value.value(&db) else {
                panic!("{value:?}")
            };
            code.freeze_request(&db, file).map_err(str::to_string)
        }
        outcome => Err(format!("{outcome:?}")),
    }
}

fn output(source: &str, target: &str) -> String {
    let plan = request(source, target).unwrap();
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("richer-review.fe".into(), source);
    db.generation_inputs().set_requests(&mut db).to(vec![plan]);
    let (module, _) = db.top_mod(file);
    let mut diagnostics = format_diagnostics(&db, &db.run_on_top_mod(module));
    for error in fe_hir::lower::generation_lowering_errors(&db, module) {
        diagnostics.push_str(&error.message);
        diagnostics.push('\n');
    }
    diagnostics
}

const PRELUDE: &str = "use core::meta::{Code, Item, Record, Expr, Pat, field, field_type, type_is, field_count, variant_count, variant}\nstruct Pair { value: u256 }\n";

#[test]
fn item_constants_and_nested_impl_members_are_not_dropped() {
    let source = format!("{PRELUDE}
        trait Chosen {{ fn selected() -> u256\n const VALUE: u256 }}\n fn via<T: Chosen>() -> u256 {{ T::selected() + T::VALUE }}
        const fn make(_ record: Record) -> Code<Item> {{
            let members: Code<Item> = quote {{ fn selected() -> u256 {{ 42 }}\n const VALUE: u256 = 7 }}
            quote {{ const GENERATED: u256 = 9\n
                impl Pair {{ ${{members}} }}\n
                impl Chosen for Pair {{ ${{members}} }}\n
                fn generated() -> u256 {{ Pair::selected() + via<Pair>() + GENERATED }} }}
        }}");
    assert!(output(&source, "Pair").is_empty());
}

#[test]
fn binder_patterns_are_charged_for_the_patterns_they_expand_into() {
    let wide = |arms: usize| {
        let fields = (0..200)
            .map(|index| format!("f{index}: u256"))
            .collect::<Vec<_>>()
            .join(", ");
        let arm_list = "${pattern} => true\n".repeat(arms);
        format!(
            "use core::meta::{{Code, Item, Record, binders}}\nstruct Wide {{ {fields} }}\n
            const fn make(_ record: Record) -> Code<Item> {{ let pattern = binders(record, 0)\n
                quote {{ fn generated(_ wide: Wide) -> bool {{ match wide {{ {arm_list} }} }} }}
            }}"
        )
    };
    // Each use of the binder pattern is copied into a record pattern with a
    // binding for each of the 200 fields: 10 uses are about 2000 nodes.
    request(&wide(10), "Wide").expect("a few uses stay within the expanded node limit");
    // 30 uses are about 6000 nodes although the quotation itself is small.
    let refusal =
        request(&wide(30), "Wide").expect_err("expanded binder patterns exceed the limit");
    assert!(refusal.contains("expanded AST node limit"), "{refusal}");
}

#[test]
fn opened_names_require_a_lexically_enclosing_binder_at_each_use() {
    for quoted in [
        "fn sibling(_ x: u256) -> u256 { x }\n fn escaped() -> u256 { x }",
        "fn escaped() -> u256 { { let x = 1 }\n x }",
    ] {
        let source = format!(
            "{PRELUDE} const x: u256 = 99\n const fn make(_ record: Record) -> Code<Item> {{ quote(x) {{ {quoted} }} }}"
        );
        let diags = output(&source, "Pair");
        assert!(diags.contains("no enclosing binder at this use"), "{diags}");
    }
    let source = format!(
        "{PRELUDE} const fn make(_ record: Record) -> Code<Item> {{ quote(x) {{ fn good(_ x: u256) -> u256 {{ x }} }} }}"
    );
    assert!(output(&source, "Pair").is_empty());
}

#[test]
fn reflection_rejects_wrong_record_kinds_and_unsupported_type_is_candidates() {
    for (target, expression, expected) in [
        (
            "Pair",
            "variant_count(record)",
            "variant_count requires a whole",
        ),
        (
            "Choice",
            "field_count(record)",
            "field_count requires a struct",
        ),
        (
            "Choice",
            "variant_count(variant(record, 0))",
            "variant_count requires a whole",
        ),
        (
            "Pair",
            "if type_is<Pair>(field_type(field(record, 0))) { 1 } else { 0 }",
            "type_is supports only bool, u64, and u256",
        ),
    ] {
        let source = format!(
            "{PRELUDE} enum Choice {{ One(u256) }}\n const fn make(_ record: Record) -> Code<Item> {{ let n = {expression}\n quote {{ fn result() -> usize {{ ${{n}} }} }} }}"
        );
        let error = request(&source, target).unwrap_err();
        assert!(error.contains(expected), "{error}");
    }
    let source = format!(
        "{PRELUDE} type Count = u256\n const fn make(_ record: Record) -> Code<Item> {{ let yes = type_is<Count>(field_type(field(record, 0)))\n quote {{ fn result() -> bool {{ ${{yes}} }} }} }}"
    );
    assert!(output(&source, "Pair").is_empty());
}

#[test]
fn metadata_field_names_lower_in_declarations_and_expression_holes() {
    let source = format!(
        "{PRELUDE}
        const fn make(_ record: Record) -> Code<Item> {{ let f = field(record, 0)
            quote {{ fn ${{f}}() -> u256 {{ 7 }}\n
                fn copy(_ value: u256) -> Pair {{ Pair {{ ${{f}} }} }} }}
        }}"
    );
    assert!(output(&source, "Pair").is_empty());
}

#[test]
fn const_generic_record_splice_preserves_const_argument_kind() {
    let source = format!("{PRELUDE} struct Array<const N: usize> {{ value: [u256; N] }}\n type Concrete = Array<2>\n
        const fn make(_ record: Record) -> Code<Item> {{ quote {{ impl ${{record}} {{ fn marker() -> u256 {{ 1 }} }} }} }}");
    // The provider sees one concrete record; generated target still copies the declaration's N binder.
    assert!(output(&source, "Concrete").is_empty());
}

#[test]
fn pattern_only_fragments_copy_into_generated_bindings() {
    for pattern in ["_", "mut x", "ref x"] {
        let source = format!(
            "{PRELUDE} const fn make(_ record: Record) -> Code<Item> {{
            let p: Code<Pat> = quote {{ {pattern} }}
            quote {{ fn generated() -> u256 {{ let ${{p}} = 7\n 42 }} }}
        }}"
        );
        assert!(output(&source, "Pair").is_empty(), "{pattern}");
    }
}

#[test]
fn or_patterns_in_pattern_fragments_are_preserved() {
    let source = format!(
        "{PRELUDE} const fn make(_ record: Record) -> Code<Item> {{
            let p: Code<Pat> = quote {{ true | false }}
            quote {{ fn generated() -> u256 {{ match true {{ ${{p}} => 1 }} }} }}
        }}"
    );
    let diags = output(&source, "Pair");
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn closure_parameters_follow_the_hygienic_names_of_their_uses() {
    let source = format!(
        "{PRELUDE} const fn make(_ record: Record) -> Code<Item> {{
            let body: Code<Expr> = quote {{ let f = |x: u256| x + 1\n f(41) }}
            quote {{ fn generated() -> u256 {{ ${{body}} }} }}
        }}"
    );
    let diags = output(&source, "Pair");
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn spliced_members_meet_the_attribute_rules_of_written_members() {
    const FUNC: &str =
        "fn first() -> u256 { 1 }\n #[generate(make)]\n fn selected() -> u256 { 42 }";
    const CONST: &str = "const FIRST: u256 = 1\n #[generate(make)]\n const VALUE: u256 = 7";
    for (members, destination) in [
        (FUNC, "impl Pair { ${members} }"),
        (CONST, "impl Pair { ${members} }"),
        (FUNC, "impl Chosen for Pair { ${members} }"),
        (
            FUNC,
            "fn wrapper() -> u256 { trait Local { ${members} } 0 }",
        ),
    ] {
        let source = format!(
            "{PRELUDE} trait Chosen {{ fn selected() -> u256\n const VALUE: u256 }}
            const fn make(_ record: Record) -> Code<Item> {{
                let members: Code<Item> = quote {{ {members} }}
                quote {{ {destination} }}
            }}"
        );
        let diags = output(&source, "Pair");
        assert!(diags.contains("#[generate]"), "{destination}: {diags}");
    }
}

#[test]
fn quoted_top_level_constants_meet_the_attribute_rules_of_written_constants() {
    let source = format!(
        "{PRELUDE} const fn make(_ record: Record) -> Code<Item> {{
            quote {{ #[generate(make)]\n const VALUE: u256 = 1 }}
        }}"
    );
    let diags = output(&source, "Pair");
    assert!(diags.contains("#[generate]"), "{diags}");
}

#[test]
fn member_splices_keep_their_producer_and_method_generic_binders() {
    let source = format!("{PRELUDE}
        mod author {{ use core::meta::{{Code, Item}}\n pub trait Number {{ const fn number() -> u256 }}\n impl Number for u256 {{ const fn number() -> u256 {{ 42 }} }}\n
            const fn private_value() -> u256 {{ 42 }}\n
            pub const fn methods() -> Code<Item> {{ quote {{ const fn selected<T>(_ x: T) -> u256 {{ private_value() }}\n const fn projected<T: Number>() -> u256 {{ T::number() }} }} }}
        }}
        const fn private_value() -> u256 {{ 999 }}
        const fn make(_ record: Record) -> Code<Item> {{ let methods = author::methods()\n
            quote {{ impl Pair {{ ${{methods}} }}\n
                const fn generated() -> u256 {{ Pair::selected(7) + Pair::projected<u256>() - 42 }} }}
        }}");
    let plan = request(&source, "Pair").unwrap();
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("richer-review.fe".into(), &source);
    db.generation_inputs().set_requests(&mut db).to(vec![plan]);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    assert!(fe_hir::lower::generation_lowering_errors(&db, module).is_empty());
    let owner = BodyOwner::Func(find_func(&db, module, "generated"), None);
    let outcome = fe_hir::analysis::semantic::eval_body_owner_const(
        &db,
        owner,
        GenericSubst::for_body_owner(&db, owner, vec![]),
    );
    let EvalOutcome::Ready(value) = outcome else {
        panic!("{outcome:?}")
    };
    assert!(
        matches!(value.value(&db), SemConstValue::Scalar { value: fe_hir::analysis::semantic::SemConstScalar::Int {value}, .. } if value.to_string() == "42")
    );
}

#[test]
fn field_name_holes_cover_constant_declarations_and_definitions() {
    let source = format!(
        "{PRELUDE}
        trait Declared {{ const value: u256 }}
        const fn make(_ record: Record) -> Code<Item> {{ let f = field(record, 0)
            quote {{
                const ${{f}}: u256 = 3
                impl Pair {{ const ${{f}}: u256 = 9 }}
                impl Declared for Pair {{ const ${{f}}: u256 = 7 }}
                fn generated() -> u256 {{
                    trait Local {{ const ${{f}}: u256 }}
                    impl Local for Pair {{ const ${{f}}: u256 = 5 }}
                    value + Pair::value + <Pair as Declared>::value + <Pair as Local>::value
                }}
            }}
        }}"
    );
    assert!(output(&source, "Pair").is_empty());
}

#[test]
fn nested_trait_member_splices_are_checked_at_the_destination() {
    let source = format!(
        "{PRELUDE}
        const fn make(_ record: Record) -> Code<Item> {{
            let declaration: Code<Item> = quote {{ const VALUE: u256 }}
            let definition: Code<Item> = quote {{ const VALUE: u256 = 7 }}
            quote {{ fn generated() -> u256 {{
                trait Local {{ ${{declaration}} }}
                impl Local for Pair {{ ${{definition}} }}
                <Pair as Local>::VALUE
            }} }}
        }}"
    );
    assert!(output(&source, "Pair").is_empty());
}
