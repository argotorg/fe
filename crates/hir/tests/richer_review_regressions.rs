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
use parser::ast::prelude::*;
use salsa::Setter;

fn request(source: &str, target: &str) -> Result<FrozenGenerationRequest, String> {
    request_with_input(source, target, false)
}

fn request_with_input(
    source: &str,
    target: &str,
    declaration_identity: bool,
) -> Result<FrozenGenerationRequest, String> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("richer-review.fe".into(), source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let func = find_func(&db, module, "make");
    let carrier = resolve_lib_type_path(&db, module.scope(), "core::meta::Record").unwrap();
    let record = if declaration_identity {
        let (green, errors) = parser::parse_source_file(source, parser::RecoveryMode::NoRecover);
        assert!(errors.is_empty(), "{errors:?}");
        let root = parser::SyntaxNode::new_root(green);
        let syntax = root
            .descendants()
            .filter_map(parser::ast::Struct::cast)
            .find(|item| item.name().is_some_and(|name| name.text() == target))
            .unwrap();
        RecordInput::for_authored_syntax(&db, module, &syntax).unwrap()
    } else {
        let ty = resolve_lib_type_path(&db, module.scope(), target).unwrap();
        RecordInput::for_authored_type(&db, ty).unwrap()
    };
    let value = SemConstId::new(
        &db,
        SemConstValue::Meta {
            ty: carrier,
            value: MetaValue::Record(record),
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
    output_request(source, request(source, target).unwrap())
}

fn output_request(source: &str, plan: FrozenGenerationRequest) -> String {
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
        (
            "Pair",
            "if type_is<u8>(field_type(field(record, 0))) { 1 } else { 0 }",
            "type_is supports only bool, u64, and u256",
        ),
    ] {
        let source = format!(
            "{PRELUDE} enum Choice {{ One(u256) }}\n const fn make(_ record: Record) -> Code<Item> {{ let n: usize = {expression}\n quote {{ fn result() -> usize {{ ${{n}} }} }} }}"
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
    let source = format!("{PRELUDE} struct Array<const N: usize> {{ value: [u256; N] }}\n
        const fn make(_ record: Record) -> Code<Item> {{ quote {{ impl ${{record}} {{ fn marker() -> u256 {{ 1 }} }} }} }}");
    // This test copies the declaration's identity Array<N>, not Array<2>.
    // The explicit derive stage separately rejects losing concrete arguments.
    let plan = request_with_input(&source, "Array", true).unwrap();
    let diags = output_request(&source, plan);
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn pattern_only_fragments_copy_into_generated_bindings() {
    for pattern in ["_", "mut x", "ref x"] {
        let source = format!(
            "{PRELUDE} const fn make(_ record: Record) -> Code<Item> {{
            let p: Code<Pat> = quote {{ {pattern} }}
            quote {{ fn generated() -> u256 {{ var source: u256 = 7\n let ${{p}}: u256 = source\n source + 35 }} }}
        }}"
        );
        let diags = output(&source, "Pair");
        assert!(diags.is_empty(), "{pattern}: {diags}");
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
    const FUNC: &str = "fn first() -> u256 { 1 }\n #[payable]\n fn selected() -> u256 { 42 }";
    const CONST: &str = "const FIRST: u256 = 1\n #[payable]\n const VALUE: u256 = 7";
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
        // Item quotations are validated when the provider returns them, so the
        // attribute may be refused before any output is lowered.
        let diags = match request(&source, "Pair") {
            Ok(plan) => output_request(&source, plan),
            Err(refusal) => refusal,
        };
        assert!(diags.contains("#[payable]"), "{destination}: {diags}");
    }
}

#[test]
fn quoted_top_level_constants_meet_the_attribute_rules_of_written_constants() {
    let source = format!(
        "{PRELUDE} const fn make(_ record: Record) -> Code<Item> {{
            quote {{ #[payable]\n const VALUE: u256 = 1 }}
        }}"
    );
    let diags = output(&source, "Pair");
    assert!(diags.contains("#[payable]"), "{diags}");
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
                const fn generated() -> u256 {{ Pair::selected<u256>(7) + Pair::projected<u256>() - 42 }} }}
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
            let declaration: Code<Item> = quote {{ const VALUE: u256 = 3 }}
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

#[test]
fn sub_quote_open_names_skip_unopened_intermediate_quote_frames() {
    let unbound_through_wrapper = format!(
        "{PRELUDE}
        const fn make(_ record: Record) -> Code<Item> {{
            let leaf: Code<Expr> = quote(acc) {{ acc + 1 }}
            let wrapped: Code<Expr> = quote {{
                let step: u256 = 0
                ${{leaf}} + step
            }}
            quote(acc) {{
                const fn compute(_ acc: u256) -> u256 {{
                    ${{wrapped}}
                }}
            }}
        }}"
    );
    let diags = output(&unbound_through_wrapper, "Pair");
    assert!(diags.is_empty(), "{diags}");

    let hygiene_through_wrapper = format!(
        "{PRELUDE}
        pub struct Check<const N: u256> {{}}
        const fn make(_ record: Record) -> Code<Item> {{
            let leaf: Code<Expr> = quote(acc) {{ acc + 1 }}
            let wrapped: Code<Expr> = quote {{
                let acc: u256 = 999
                ${{leaf}}
            }}
            quote(acc) {{
                const fn compute(_ acc: u256) -> u256 {{
                    ${{wrapped}}
                }}
                fn verify() -> Check<11> {{
                    let x: Check<{{ compute(10) }}> = Check {{}}
                    x
                }}
            }}
        }}"
    );
    let diags = output(&hygiene_through_wrapper, "Pair");
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn items_declared_in_expr_quote_blocks_bind_own_params_and_block_names() {
    let source = format!(
        "{PRELUDE}
        const fn make(_ record: Record) -> Code<Item> {{
            let body: Code<Expr> = quote {{
                let base: u256 = 40
                fn step(_ v: u256) -> u256 {{
                    v + 2
                }}
                step(base)
            }}
            quote {{
                fn run() -> u256 {{
                    ${{body}}
                }}
            }}
        }}"
    );
    let diags = output(&source, "Pair");
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn pat_quote_binders_and_open_names_respect_hygiene_and_variant_paths() {
    let closed_mut_binder = format!(
        "{PRELUDE}
        const fn make(_ record: Record) -> Code<Item> {{
            let p: Code<Pat> = quote {{ mut x }}
            quote {{
                fn generated() -> u256 {{
                    var source: u256 = 7
                    let ${{p}}: u256 = source
                    x + 35
                }}
            }}
        }}"
    );
    let diags = output(&closed_mut_binder, "Pair");
    assert!(diags.contains("undefined variable `x`"), "{diags}");

    let self_satisfied_open_binder = format!(
        "{PRELUDE}
        const fn make(_ record: Record) -> Code<Item> {{
            let p: Code<Pat> = quote(x) {{ mut x }}
            quote {{
                fn generated() -> u256 {{
                    var source: u256 = 7
                    let ${{p}}: u256 = source
                    x + 35
                }}
            }}
        }}"
    );
    let diags = output(&self_satisfied_open_binder, "Pair");
    assert!(
        diags.contains("quoted open name `x` has no enclosing binder"),
        "{diags}"
    );

    let variant_as_binder = format!(
        "{PRELUDE}
        const fn make(_ record: Record) -> Code<Item> {{
            let e: Code<Expr> = quote(None) {{ 42 }}
            quote {{
                fn test_match(_ x: Option<u256>) -> u256 {{
                    match x {{
                        None => ${{e}}
                        Some(v) => v
                    }}
                }}
            }}
        }}"
    );
    let diags = output(&variant_as_binder, "Pair");
    assert!(
        diags.contains("quoted open name `None` has no enclosing binder"),
        "{diags}"
    );

    let shared_open_mut_binder = format!(
        "{PRELUDE}
        const fn make(_ record: Record) -> Code<Item> {{
            let p: Code<Pat> = quote(x) {{ mut x }}
            quote(x) {{
                fn generated() -> u256 {{
                    var source: u256 = 7
                    let ${{p}}: u256 = source
                    x + 35
                }}
            }}
        }}"
    );
    let diags = output(&shared_open_mut_binder, "Pair");
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn bare_pattern_names_bind_open_names_as_pattern_checking_resolves_them() {
    // A bare name in a pattern binds unless it names a constant or unit
    // variant, whatever its letter case.
    let upper_binder = format!(
        "{PRELUDE}
        const fn make(_ record: Record) -> Code<Item> {{
            let e: Code<Expr> = quote(V) {{ V + 1 }}
            quote {{
                fn bump(_ x: Option<u256>) -> u256 {{
                    match x {{
                        Some(V) => ${{e}}
                        None => 0
                    }}
                }}
            }}
        }}"
    );
    let diags = output(&upper_binder, "Pair");
    assert!(diags.is_empty(), "{diags}");

    for constant in ["ONE", "one"] {
        let constant_pattern = format!(
            "{PRELUDE}
        const {constant}: u256 = 1
        const fn make(_ record: Record) -> Code<Item> {{
            let e: Code<Expr> = quote({constant}) {{ 42 }}
            quote {{
                fn pick(_ x: u256) -> u256 {{
                    match x {{
                        {constant} => ${{e}}
                        _ => 0
                    }}
                }}
            }}
        }}"
        );
        let diags = output(&constant_pattern, "Pair");
        assert!(
            diags.contains(&format!(
                "quoted open name `{constant}` has no enclosing binder"
            )),
            "{constant}: {diags}"
        );
    }
}

#[test]
fn or_pattern_arms_allow_splicing_same_binder_group_per_arm() {
    let per_arm = r#"use core::meta::{Code, Item, Record, binders, variant}
enum Flag {
    A,
    B,
}
const fn make(_ r: Record) -> Code<Item> {
    let a = variant(r, 0)
    let pat_b = binders(a, 0)
    quote {
        fn has_a(_ pair: (Flag, Flag)) -> bool {
            match pair {
                (${pat_b}, _) | (_, ${pat_b}) => true
                _ => false
            }
        }
    }
}
"#;
    let diags = output(per_arm, "Flag");
    assert!(diags.is_empty(), "{diags}");

    let duplicate_in_same_arm = r#"use core::meta::{Code, Item, Record, binders, variant}
enum Flag {
    A,
    B,
}
const fn make(_ r: Record) -> Code<Item> {
    let a = variant(r, 0)
    let pat_b = binders(a, 0)
    quote {
        fn both_a(_ pair: (Flag, Flag)) -> bool {
            match pair {
                (${pat_b}, ${pat_b}) | _ => true
            }
        }
    }
}
"#;
    let diags = output(duplicate_in_same_arm, "Flag");
    assert!(
        diags.contains("binder group already bound in an enclosing pattern"),
        "{diags}"
    );
}

#[test]
fn unsafe_block_parses_and_lowers_as_expr_quotation() {
    let source = format!(
        "{PRELUDE}
        const fn make(_ r: Record) -> Code<Item> {{
            let body: Code<Expr> = quote {{
                unsafe {{ 42 }}
            }}
            quote {{
                fn f() -> u256 {{
                    ${{body}}
                }}
            }}
        }}"
    );
    let diags = output(&source, "Pair");
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn type_holes_reject_variants_and_unused_pat_expr_quotes_validate_holes() {
    let variant_in_type_hole = r#"use core::meta::{Code, Item, Record, variant}
enum Choice {
    First(u256),
    Second(bool),
}
const fn make(_ r: Record) -> Code<Item> {
    let v = variant(r, 0)
    quote {
        fn accept_variant(_ x: own ${v}) -> ${v} {
            x
        }
    }
}
"#;
    let err = request(variant_in_type_hole, "Choice").unwrap_err();
    assert!(
        err.contains(
            "type quotation hole requires a record, field type, or checked type reference"
        ),
        "{err}"
    );

    let unused_pat_hole = format!(
        "{PRELUDE}
        const fn make(_ r: Record) -> Code<Item> {{
            let _unused_pat: Code<Pat> = quote {{ ${{r}} }}
            quote {{
                fn ok() -> u256 {{
                    1
                }}
            }}
        }}"
    );
    let err = request(&unused_pat_hole, "Pair").unwrap_err();
    assert!(
        err.contains("pattern quotation hole requires Code<Pat>"),
        "{err}"
    );

    let unused_expr_hole = format!(
        "{PRELUDE}
        const fn make(_ r: Record) -> Code<Item> {{
            let _unused_expr: Code<Expr> = quote {{ ${{r}} }}
            quote {{
                fn ok() -> u256 {{
                    1
                }}
            }}
        }}"
    );
    let err = request(&unused_expr_hole, "Pair").unwrap_err();
    assert!(
        err.contains("expression quotation hole requires Code<Expr>"),
        "{err}"
    );
}

#[test]
fn field_type_splices_preserve_compound_const_generic_expressions() {
    let source = format!(
        "{PRELUDE}
        pub struct Packet<const N: usize> {{
            pub data: [u8; N + 1],
        }}

        const fn make(_ r: Record) -> Code<Item> {{
            let f = field(r, 0)
            let f_ty = field_type(f)
            quote {{
                impl ${{r}} {{
                    pub fn get_data(own self) -> ${{f_ty}} {{
                        self.data
                    }}
                }}
                pub fn use_packet(_ p: own Packet<4>) -> [u8; 5] {{
                    p.get_data()
                }}
            }}
        }}"
    );
    let plan = request_with_input(&source, "Packet", true).unwrap();
    let diags = output_request(&source, plan);
    assert!(
        diags.is_empty(),
        "expected no diagnostics for spliced field type with compound const generic expression, got: {diags}"
    );
}

#[test]
fn mixed_item_and_expr_hole_quote_blocks_infer_and_lower_as_expr() {
    let source = format!(
        "{PRELUDE}
        const fn make(_ r: Record) -> Code<Item> {{
            let a: Code<Item> = quote {{
                fn helper() -> u256 {{ 41 }}
                fn bump() -> u256 {{ helper() + 1 }}
            }}
            let b: Code<Expr> = quote {{ 1 }}
            let combined = quote {{
                ${{a}}
                ${{b}}
            }}
            quote {{
                pub fn calc() -> u256 {{
                    ${{combined}}
                }}
            }}
        }}"
    );
    let diags = output(&source, "Pair");
    assert!(
        diags.is_empty(),
        "expected mixed item+expr quote block to infer and lower as Code<Expr>, got: {diags}"
    );

    // The expression fragment's `helper` is a free name of its producer,
    // which declares no `helper`. The item that a sibling fragment splices
    // into the same block does not capture it.
    let sibling_capture = format!(
        "{PRELUDE}
        const fn make(_ r: Record) -> Code<Item> {{
            let a: Code<Item> = quote {{ fn helper() -> u256 {{ 41 }} }}
            let b: Code<Expr> = quote {{ helper() + 1 }}
            let combined = quote {{
                ${{a}}
                ${{b}}
            }}
            quote {{
                pub fn calc() -> u256 {{
                    ${{combined}}
                }}
            }}
        }}"
    );
    let diags = output(&sibling_capture, "Pair");
    assert!(
        diags.contains("undefined variable `helper`"),
        "expected the sibling fragment's item not to capture a free name, got: {diags}"
    );

    let bad_item_block = format!(
        "{PRELUDE}
        const fn make(_ r: Record) -> Code<Item> {{
            let a: Code<Item> = quote {{ fn f() {{}} }}
            let b: Code<Expr> = quote {{ 1 }}
            let combined: Code<Item> = quote {{
                ${{a}}
                ${{b}}
            }}
            combined
        }}"
    );
    let err = request(&bad_item_block, "Pair").unwrap_err();
    assert!(
        err.contains("item quotation hole requires Code<Item> splice"),
        "{err}"
    );
}

#[test]
fn spliced_fragment_free_names_keep_producer_origin_over_destination_items() {
    // `e` names the producer's `helper` (1). The destination block declares
    // its own `helper` (1000), which must not capture the spliced name.
    let destination_item = format!(
        "{PRELUDE}
        struct Check<const N: u256> {{}}
        const fn helper() -> u256 {{ 1 }}
        const fn make(_ r: Record) -> Code<Item> {{
            let e: Code<Expr> = quote {{ helper() }}
            quote {{
                const fn run() -> u256 {{
                    const fn helper() -> u256 {{ 1000 }}
                    ${{e}}
                }}
                fn verify() -> Check<1> {{
                    let x: Check<{{ run() }}> = Check {{}}
                    x
                }}
            }}
        }}"
    );
    let diags = output(&destination_item, "Pair");
    assert!(
        diags.is_empty(),
        "expected the spliced `helper` to keep its producer origin, got: {diags}"
    );

    // A fragment's own items bind its own uses where it is spliced into a
    // block: `bump` calls the fragment's `helper` (41), not the producer's.
    let own_items = format!(
        "{PRELUDE}
        struct Check<const N: u256> {{}}
        const fn helper() -> u256 {{ 1 }}
        const fn make(_ r: Record) -> Code<Item> {{
            let a: Code<Item> = quote {{
                const fn helper() -> u256 {{ 41 }}
                const fn bump() -> u256 {{ helper() + 1 }}
            }}
            quote {{
                const fn run() -> u256 {{
                    ${{a}}
                    bump()
                }}
                fn verify() -> Check<42> {{
                    let x: Check<{{ run() }}> = Check {{}}
                    x
                }}
            }}
        }}"
    );
    let diags = output(&own_items, "Pair");
    assert!(
        diags.is_empty(),
        "expected a fragment's own items to bind its own uses, got: {diags}"
    );
}

#[test]
fn compound_patterns_with_pattern_only_tokens_parse_in_quote_blocks() {
    let source = format!(
        "{PRELUDE}
        const fn make(_ r: Record) -> Code<Item> {{
            let p_tuple: Code<Pat> = quote(y) {{ (_, mut y) }}
            let p_rec: Code<Pat> = quote(a) {{ Pair {{ value: mut a, .. }} }}
            let p_var: Code<Pat> = quote(v) {{ core::option::Option::Some((_, v)) }}
            quote(y, a, v) {{
                fn read_all(_ opt: core::option::Option<(u256, u256)>) -> u256 {{
                    var pair: (u256, u256) = (1, 2)
                    var p: Pair = Pair {{ value: 3 }}
                    let ${{p_tuple}}: (u256, u256) = pair
                    let ${{p_rec}}: Pair = p
                    let tail = match opt {{
                        ${{p_var}} => v
                        core::option::Option::None => 0
                    }}
                    y + a + tail
                }}
            }}
        }}"
    );
    let diags = output(&source, "Pair");
    assert!(
        diags.is_empty(),
        "expected compound pattern quotations to parse and lower cleanly, got: {diags}"
    );
}

#[test]
fn record_pattern_field_name_splices_parse_and_lower() {
    let source = format!(
        "{PRELUDE}
        const fn make(_ r: Record) -> Code<Item> {{
            let f = field(r, 0)
            let pat_q: Code<Pat> = quote(w) {{ Pair {{ ${{f}}: w, .. }} }}
            quote(w) {{
                fn first(_ p: Pair) -> u256 {{
                    let a = match p {{
                        Pair {{ ${{f}}: v, .. }} => v
                    }}
                    let b = match p {{
                        ${{pat_q}} => w
                    }}
                    a + b
                }}
            }}
        }}"
    );
    let diags = output(&source, "Pair");
    assert!(
        diags.is_empty(),
        "expected record pattern field name splices to parse and lower cleanly, got: {diags}"
    );
}
