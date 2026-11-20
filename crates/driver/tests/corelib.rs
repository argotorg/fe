//! Analysis of the builtin core and std ingots through the driver.

use common::InputDb;
use common::compilation::{Platform, Target};
use common::indexmap::IndexMap;
use common::stdlib::{HasBuiltinCore, HasBuiltinStd};
use fe_driver::{DriverDataBase, db::DiagnosticsCollection};
use hir::Ingot;
use hir::analysis::name_resolution::Tier;
use hir::analysis::ty::{
    corelib::{resolve_core_trait, resolve_lib_type_path},
    trait_resolution::{GoalSatisfiability, TraitSolveCx, is_goal_satisfiable},
    ty_check::check_func_body,
};
use hir::hir_def::{
    Expr, HirIngot, ItemKind, LitKind, Partial, Pat,
    attr::{Attr, AttrArgValue},
};
use salsa::Setter;
use url::Url;

#[cfg(target_arch = "wasm32")]
use test_utils::url_utils::UrlExt;

// Core is checked for each target, since the width of `usize` depends on it.
#[test]
fn analyze_corelib() {
    for target in Target::ALL {
        let mut db = DriverDataBase::default();
        db.compilation_settings().set_target(&mut db).to(target);
        let core = db.builtin_core();
        assert!(
            core.files(&db).iter().next().is_some(),
            "builtin core ingot should not be empty"
        );

        let core_diags = db.run_on_ingot(core);
        assert_builtin_clean(&db, core, core_diags, &format!("core ({target:?})"));
    }
}

// Std is checked for each target and supported platform, since `#[cfg]`
// selects its items by them.
#[test]
fn analyze_stdlib() {
    let configs = Target::ALL
        .into_iter()
        .map(|target| (target, None))
        .chain(Platform::SUPPORTED.map(|platform| (Target::Native, Some(platform))));
    for (target, platform) in configs {
        let mut db = DriverDataBase::default();
        db.compilation_settings().set_target(&mut db).to(target);
        db.compilation_settings().set_platform(&mut db).to(platform);
        let std_ingot = db.builtin_std();
        assert!(
            std_ingot.files(&db).iter().next().is_some(),
            "builtin std ingot should not be empty"
        );

        let std_diags = db.run_on_ingot(std_ingot);
        assert_builtin_clean(
            &db,
            std_ingot,
            std_diags,
            &format!("std ({target:?}, {platform:?})"),
        );
    }
}

fn assert_builtin_clean(
    db: &DriverDataBase,
    ingot: Ingot<'_>,
    diags: DiagnosticsCollection<'_>,
    name: &str,
) {
    if !diags.is_empty() {
        diags.emit(db);
        panic!(
            "expected no HIR diagnostics for builtin {name}, but got:\n{}",
            diags.format_diags(db)
        );
    }

    let mir_diags = db.mir_diagnostics_for_ingot(ingot);
    if !mir_diags.is_empty() {
        db.emit_complete_diagnostics(&mir_diags);
        let internal: Vec<_> = mir_diags
            .iter()
            .filter(|diagnostic| {
                diagnostic
                    .message
                    .starts_with("internal borrow checking error")
            })
            .collect();
        panic!(
            "expected no MIR diagnostics for builtin {name}, but got:\n{}\nInternal diagnostic details: {internal:#?}",
            db.format_complete_diagnostics(&mir_diags)
        );
    }
}

fn release_profile_db() -> DriverDataBase {
    let mut db = DriverDataBase::default();
    db.compilation_settings()
        .set_profile(&mut db)
        .to("release".into());
    db
}

#[test]
fn builtin_core_calls_have_semantic_lowerings() {
    let db = DriverDataBase::default();
    let core = db.builtin_core();
    let mut missing = Vec::new();

    // Query in reverse declaration order so semantic call metadata cannot
    // accidentally depend on callers having been checked before callees.
    for &func in core.all_funcs(&db).iter().rev() {
        let Some(body) = func.body(&db) else {
            continue;
        };
        let typed = &check_func_body(&db, func).1;
        for (expr, expr_data) in body.exprs(&db).iter() {
            if matches!(
                expr_data,
                Partial::Present(Expr::Call(..) | Expr::MethodCall(..))
            ) && typed.semantic_expr_lowering(expr).is_none()
            {
                let name = func
                    .name(&db)
                    .to_opt()
                    .map_or_else(|| "<fn>".to_string(), |name| name.data(&db).to_string());
                missing.push(format!("{name}: {expr:?}"));
            }
        }
    }

    assert!(
        missing.is_empty(),
        "typed core calls without semantic lowerings:\n{}",
        missing.join("\n")
    );
}

#[test]
fn mem_slice_get_requires_copy_elements() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///mem_slice_get_requires_copy_elements.fe").unwrap();
    let src = r#"
fn get_noncopy(
    slice: core::ptr::MemSlice<core::ptr::MemBuffer>,
) -> core::option::Option<core::ptr::MemBuffer> {
    slice.get(0)
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    let rendered = diags.format_diags(&db);

    assert!(
        rendered.contains("`MemBuffer` doesn't implement `Copy`"),
        "{rendered}"
    );
}

#[test]
fn solver_proves_encode_for_fixed_bool_arrays() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///encode_fixed_bool_array_goal.fe").unwrap();
    let src = r#"
use core::abi::{AbiFrame, Encode}
use std::abi::Sol

pub fn root(values: [bool; 5], out: AbiFrame) {
    values.encode(out)
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    assert!(
        diags.is_empty(),
        "unexpected diagnostics: {}",
        diags.format_diags(&db)
    );

    let func = top_mod.all_funcs(&db)[0];
    let body = func.body(&db).expect("root should have a body");
    let typed_body = &check_func_body(&db, func).1;
    let callable = body
        .exprs(&db)
        .keys()
        .find_map(|expr| {
            let callable = typed_body.callable_expr(expr)?;
            let name = callable.callable_def.name(&db)?;
            (name.data(&db) == "encode").then_some(callable)
        })
        .expect("encode callable should resolve during type checking");
    let inst = callable
        .trait_inst()
        .expect("encode should be a trait method");
    let solve_cx = TraitSolveCx::new(&db, func.scope(), Tier::All);

    match is_goal_satisfiable(&db, solve_cx, inst) {
        GoalSatisfiability::Satisfied(_) => {}
        other => panic!(
            "expected `{}` to be satisfiable, got {other:?}",
            inst.pretty_print(&db, true)
        ),
    }
}

#[test]
fn solver_proves_core_abi_traits_for_u256_and_sol() {
    let mut db = release_profile_db();
    let url = Url::parse("file:///solver_proves_core_abi_traits_for_u256_and_sol.fe").unwrap();
    let src = "pub fn root() {}\n";

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    assert!(
        diags.is_empty(),
        "unexpected diagnostics: {}",
        diags.format_diags(&db)
    );

    let scope = top_mod.scope();
    let solve_cx = TraitSolveCx::new(&db, scope, Tier::All);
    let encode_trait = resolve_core_trait(&db, scope, &["abi", "Encode"]).unwrap();
    let decode_trait = resolve_core_trait(&db, scope, &["abi", "Decode"]).unwrap();
    let abi_trait = resolve_core_trait(&db, scope, &["abi", "Abi"]).unwrap();
    let sol_ty = resolve_lib_type_path(&db, scope, "std::abi::Sol").unwrap();
    let u256_ty = hir::analysis::ty::ty_def::TyId::u256(&db);

    for (label, inst) in [
        (
            "Sol: Abi",
            hir::analysis::ty::trait_def::TraitInstId::new(
                &db,
                abi_trait,
                vec![sol_ty],
                IndexMap::default(),
                hir::hir_def::ModeMarker::None,
            ),
        ),
        (
            "u256: Encode<Sol>",
            hir::analysis::ty::trait_def::TraitInstId::new(
                &db,
                encode_trait,
                vec![u256_ty, sol_ty],
                IndexMap::default(),
                hir::hir_def::ModeMarker::None,
            ),
        ),
        (
            "u256: Decode<Sol>",
            hir::analysis::ty::trait_def::TraitInstId::new(
                &db,
                decode_trait,
                vec![u256_ty, sol_ty],
                IndexMap::default(),
                hir::hir_def::ModeMarker::None,
            ),
        ),
    ] {
        match is_goal_satisfiable(&db, solve_cx, inst) {
            GoalSatisfiability::Satisfied(_) => {}
            other => panic!("expected `{label}` to be satisfiable, got {other:?}"),
        }
    }
}

#[test]
fn generic_call_specializes_abi_trait_bounds_to_concrete_args() {
    let mut db = release_profile_db();
    let url = Url::parse("file:///generic_call_specializes_abi_trait_bounds_to_concrete_args.fe")
        .unwrap();
    let src = r#"
use core::abi::{Decode, Encode}
use std::abi::Sol

pub fn require_encode<T>() where T: Encode<Sol> {}
pub fn require_decode<T>() where T: Decode<Sol> {}

pub fn root() {
    require_encode<u256>()
    require_decode<u256>()
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    assert!(
        diags.is_empty(),
        "unexpected diagnostics: {}",
        diags.format_diags(&db)
    );

    let root = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "root")
        })
        .expect("missing root function");
    let body = root.body(&db).expect("root should have a body");
    let typed_body = &check_func_body(&db, root).1;

    let mut seen = 0;
    for expr in body.exprs(&db).keys() {
        let Some(callable) = typed_body.callable_expr(expr) else {
            continue;
        };
        let Some(name) = callable.callable_def.name(&db) else {
            continue;
        };
        if !matches!(name.data(&db).as_str(), "require_encode" | "require_decode") {
            continue;
        }
        seen += 1;
        assert_eq!(
            callable
                .generic_args()
                .iter()
                .map(|ty| ty.pretty_print(&db).to_string())
                .collect::<Vec<_>>(),
            vec!["u256".to_string()]
        );
    }
    assert_eq!(seen, 2, "expected both generic calls to be present");
}

#[test]
fn ingot_analysis_accepts_concrete_encode_decode_call_bounds() {
    let mut db = release_profile_db();
    let url =
        Url::parse("file:///ingot_analysis_accepts_concrete_encode_decode_call_bounds.fe").unwrap();
    let src = r#"
use core::abi::{Decode, Encode}
use std::abi::Sol

pub fn require_encode<T>() where T: Encode<Sol> {}
pub fn require_decode<T>() where T: Decode<Sol> {}

pub fn root() {
    require_encode<u256>()
    require_decode<u256>()
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let diags = db.run_on_ingot(top_mod.ingot(&db));
    assert!(
        diags.is_empty(),
        "unexpected diagnostics: {}",
        diags.format_diags(&db)
    );
}

#[test]
fn mir_analysis_accepts_concrete_encode_decode_call_bounds() {
    let mut db = release_profile_db();
    let url =
        Url::parse("file:///mir_analysis_accepts_concrete_encode_decode_call_bounds.fe").unwrap();
    let src = r#"
use core::abi::{Decode, Encode}
use std::abi::Sol

pub fn require_encode<T>() where T: Encode<Sol> {}
pub fn require_decode<T>() where T: Decode<Sol> {}

pub fn root() {
    require_encode<u256>()
    require_decode<u256>()
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let hir_diags = db.run_on_top_mod(top_mod);
    assert!(
        hir_diags.is_empty(),
        "unexpected HIR diagnostics: {}",
        hir_diags.format_diags(&db)
    );

    let mir_diags = db.mir_diagnostics_for_top_mod(top_mod);
    assert!(
        mir_diags.is_empty(),
        "unexpected MIR diagnostics: {}",
        db.format_complete_diagnostics(&mir_diags)
    );
}

#[test]
fn string_literal_types_use_decoded_utf8_bytes() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///string_escapes.fe").unwrap();
    let src = r#"
pub fn root() {
    let controls: [u8; 5] = "\"\\\n\r\t"
    let unicode: String<3> = "é\n"
    let backslash: [u8; 2] = "\\n"
}
"#;
    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    assert!(diags.is_empty(), "{}", diags.format_diags(&db));
    let func = top_mod.all_funcs(&db)[0];
    let body = func.body(&db).unwrap();
    let typed_body = &check_func_body(&db, func).1;
    let literals: Vec<_> = body
        .exprs(&db)
        .keys()
        .filter_map(|expr| {
            if let Partial::Present(Expr::Lit(LitKind::String(value))) = expr.data(&db, body) {
                Some((
                    value.data(&db).as_str(),
                    typed_body.expr_ty(&db, expr).pretty_print(&db).as_str(),
                ))
            } else {
                None
            }
        })
        .collect();
    assert_eq!(
        literals,
        [
            ("\"\\\n\r\t", "[u8; 5]"),
            ("é\n", "String<3>"),
            ("\\n", "[u8; 2]"),
        ]
    );
}

#[test]
fn invalid_string_escapes_do_not_become_semantic_strings() {
    for src in [
        r#"pub fn root() { let text = "\q" }"#,
        r#"pub fn root(text: String<1>) { match text { "\q" => (), _ => () } }"#,
        r#"#[example(value = "\q")]
pub fn root() {}"#,
        r#"#[example = "\q"]
pub fn root() {}"#,
        r#"msg M {
#[selector = "\q"]
Bad,
#[selector = 1]
Good
}"#,
    ] {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///invalid_string_escape.fe").unwrap();
        let src = format!("{src}\npub fn following() {{ let number: u256 = 1 }}");
        let file = db.workspace().touch(&mut db, url, Some(src.clone()));
        let top_mod = db.top_mod(file);
        for func in top_mod.all_funcs(&db) {
            if let Some(body) = func.body(&db) {
                assert!(
                    !body.exprs(&db).keys().any(|expr| matches!(
                        expr.data(&db, body),
                        Partial::Present(Expr::Lit(LitKind::String(_)))
                    )),
                    "{src}"
                );
                assert!(
                    !body.pats(&db).keys().any(|pat| matches!(
                        pat.data(&db, body),
                        Partial::Present(Pat::Lit(Partial::Present(LitKind::String(_))))
                    )),
                    "{src}"
                );
            }
            for attr in ItemKind::Func(*func).attrs(&db).unwrap().data(&db) {
                // An expression value keeps its source; only a literal is
                // decoded into a string.
                let is_string = |value: &Option<AttrArgValue>| {
                    matches!(value, Some(AttrArgValue::Lit(LitKind::String(_))))
                };
                if let Attr::Normal(attr) = attr {
                    assert!(!is_string(&attr.value), "{src}");
                    assert!(!attr.args.iter().any(|arg| is_string(&arg.value)), "{src}");
                }
            }
        }
        let diags = db.run_on_top_mod(top_mod);
        assert!(
            diags.format_diags(&db).contains("invalid string escape"),
            "{src}"
        );
        assert!(top_mod.all_funcs(&db).iter().any(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "following")
        }));
    }
}

#[test]
fn string_literals_can_pick_up_dynstring_api_from_later_use() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///string_dynstring_api.fe").unwrap();
    let src = r#"
pub fn root() -> u8 {
    let text = "hello-dynamic-api"
    text.span().byte_at(0)
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    assert!(
        diags.is_empty(),
        "unexpected diagnostics: {}",
        diags.format_diags(&db)
    );

    let func = top_mod.all_funcs(&db)[0];
    let body = func.body(&db).expect("root should have a body");
    let typed_body = &check_func_body(&db, func).1;
    let literal_expr = body
        .exprs(&db)
        .keys()
        .find(|expr| {
            matches!(
                expr.data(&db, body),
                Partial::Present(Expr::Lit(LitKind::String(_)))
            )
        })
        .expect("string literal should be present");

    assert_eq!(
        typed_body.expr_ty(&db, literal_expr).pretty_print(&db),
        "DynString"
    );
}

#[test]
fn long_string_literals_can_pick_up_dynstring_api_from_later_use() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///long_string_dynstring_api.fe").unwrap();
    let src = r#"
pub fn root() -> u8 {
    let text = "hello-dynamic-api-abcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789"
    text.span().byte_at(0)
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    assert!(
        diags.is_empty(),
        "unexpected diagnostics: {}",
        diags.format_diags(&db)
    );

    let func = top_mod.all_funcs(&db)[0];
    let body = func.body(&db).expect("root should have a body");
    let typed_body = &check_func_body(&db, func).1;
    let literal_expr = body
        .exprs(&db)
        .keys()
        .find(|expr| {
            matches!(
                expr.data(&db, body),
                Partial::Present(Expr::Lit(LitKind::String(_)))
            )
        })
        .expect("string literal should be present");

    assert_eq!(
        typed_body.expr_ty(&db, literal_expr).pretty_print(&db),
        "DynString"
    );
}

#[test]
fn runtime_string_literals_can_infer_dynstring_from_return_type() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///string_runtime_return_dynstring.fe").unwrap();
    let src = r#"
use std::abi::Text

pub fn root() -> Text {
    let text = "hello-runtime-return"
    text
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    assert!(
        diags.is_empty(),
        "unexpected diagnostics: {}",
        diags.format_diags(&db)
    );

    let func = top_mod.all_funcs(&db)[0];
    let body = func.body(&db).expect("root should have a body");
    let typed_body = &check_func_body(&db, func).1;
    let literal_expr = body
        .exprs(&db)
        .keys()
        .find(|expr| {
            matches!(
                expr.data(&db, body),
                Partial::Present(Expr::Lit(LitKind::String(_)))
            )
        })
        .expect("string literal should be present");

    assert_eq!(
        typed_body.expr_ty(&db, literal_expr).pretty_print(&db),
        "DynString"
    );
}

#[test]
fn long_runtime_string_literals_can_infer_dynstring_from_return_type() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///long_string_runtime_return_dynstring.fe").unwrap();
    let src = r#"
use std::abi::Text

pub fn root() -> Text {
    let text = "hello-runtime-return-abcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789"
    text
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    assert!(
        diags.is_empty(),
        "unexpected diagnostics: {}",
        diags.format_diags(&db)
    );

    let func = top_mod.all_funcs(&db)[0];
    let body = func.body(&db).expect("root should have a body");
    let typed_body = &check_func_body(&db, func).1;
    let literal_expr = body
        .exprs(&db)
        .keys()
        .find(|expr| {
            matches!(
                expr.data(&db, body),
                Partial::Present(Expr::Lit(LitKind::String(_)))
            )
        })
        .expect("string literal should be present");

    assert_eq!(
        typed_body.expr_ty(&db, literal_expr).pretty_print(&db),
        "DynString"
    );
}

#[test]
fn runtime_sol_selector_still_accepts_string_literals() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///runtime_sol_selector.fe").unwrap();
    let src = r#"
use std::abi::sol

pub fn root() -> u32 {
    let signature = "ping()"
    let selector = sol(signature)
    selector
}
"#;

    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    assert!(
        diags.is_empty(),
        "unexpected diagnostics: {}",
        diags.format_diags(&db)
    );
}

#[test]
fn sol_codec_fallback_proof_waits_for_its_element_type() {
    use hir::analysis::ty::{
        evidence::{Evidence, prove},
        trait_def::{ImplementorOrigin, TraitInstId},
        trait_resolution::PredicateListId,
        ty_def::TyId,
    };
    use hir::hir_def::CallableDef;

    let mut db = release_profile_db();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///sol_codec_fallback_proof.fe").unwrap(),
        Some("pub fn root<T>() {}\n".to_string()),
    );
    let top_mod = db.top_mod(file);
    let func = top_mod.all_funcs(&db)[0];
    let scope = func.scope();
    let codec = resolve_lib_type_path(&db, scope, "std::evm::SolLanes").unwrap();
    let trait_ = resolve_core_trait(&db, scope, &["ops", "LaneCodec"]).unwrap();
    let goal = |element| TraitInstId::new_simple(&db, trait_, vec![codec, element]);
    let generic = goal(CallableDef::Func(func).params(&db)[0]);
    let solve_cx = TraitSolveCx::new(&db, scope, Tier::All);

    // The solver's provisional whole-slot fallback cannot become a lasting
    // implementation proof: substituting a packed element changes its answer.
    assert!(matches!(
        is_goal_satisfiable(&db, solve_cx, generic),
        GoalSatisfiability::Satisfied(_)
    ));
    let deferred = prove(&db, solve_cx, generic).unwrap();
    assert!(matches!(
        deferred.data(&db),
        Evidence::DeferredSolCodec { .. }
    ));
    assert!(!deferred.is_closed(&db));

    let supplied = solve_cx.with_assumptions(PredicateListId::new(&db, vec![generic]));
    assert!(matches!(
        prove(&db, supplied, generic).unwrap().data(&db),
        Evidence::Assumption(_)
    ));
    for (element, structural) in [(TyId::u8(&db), false), (TyId::unit(&db), true)] {
        let proof = prove(&db, solve_cx, goal(element)).unwrap();
        let Evidence::Impl { selected, .. } = proof.data(&db) else {
            panic!("concrete codec must have an implementation proof");
        };
        assert_eq!(
            matches!(selected.origin(&db), ImplementorOrigin::Structural),
            structural
        );
    }
}

/// The traits whose presence the compiler consults to choose behaviour are
/// declared once, in the libraries, with `#[decides = "..."]`. The refusal of
/// generated implementations and the compiler code that asks the facts both
/// read this declaration.
#[test]
fn the_traits_whose_presence_decides_behaviour_are_declared_in_the_libraries() {
    use hir::analysis::ty::corelib::{presence_decision, presence_traits};
    let db = DriverDataBase::default();
    let scope = db.builtin_std().root_mod(&db).scope();
    let mut declared: Vec<(String, String)> = presence_traits(&db, scope)
        .into_iter()
        .map(|trait_| {
            (
                trait_.name(&db).to_opt().unwrap().data(&db).to_string(),
                presence_decision(&db, trait_).unwrap(),
            )
        })
        .collect();
    declared.sort();
    let expected = [
        ("Copy", "whether a value is copied or moved"),
        ("Driver", "which protocol a `for` loop runs"),
        ("Drop", "what runs where a value goes out of use"),
        (
            "Encode",
            "whether a panic off the EVM reverts with its value or traps",
        ),
        ("ErrorVariant", "what data a revert with a value carries"),
        ("Fn", "which implementation a call of a value runs"),
        ("FnMut", "which implementation a call of a value runs"),
        (
            "LaneCodec",
            "how a collection packs its elements into lanes",
        ),
        ("Pinned", "where a value may be kept"),
        ("Producer", "which protocol a `for` loop runs"),
        ("Snapshot", "whether a view takes a value by copy"),
        ("SolPacked", "how a stored value is packed"),
        (
            "StateIndex",
            "whether indexing a collection is a call or an entry",
        ),
        (
            "WrappingAdd",
            "which trait an operator calls in unchecked code",
        ),
        (
            "WrappingMul",
            "which trait an operator calls in unchecked code",
        ),
        (
            "WrappingNeg",
            "which trait an operator calls in unchecked code",
        ),
        (
            "WrappingPow",
            "which trait an operator calls in unchecked code",
        ),
        (
            "WrappingSub",
            "which trait an operator calls in unchecked code",
        ),
    ]
    .map(|(name, what)| (name.to_string(), what.to_string()));
    assert_eq!(declared, expected);
}

/// Only core and std declare it: the same attribute on a trait of a package
/// declares nothing.
#[test]
fn a_package_cannot_declare_a_trait_whose_presence_decides_behaviour() {
    use hir::analysis::ty::corelib::presence_decision;
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///presence_declaration.fe").unwrap();
    let src = "#[decides = \"anything\"]\ntrait Mine {}\n";
    let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
    let top_mod = db.top_mod(file);
    let traits = top_mod.all_traits(&db);
    assert_eq!(traits.len(), 1);
    assert_eq!(presence_decision(&db, traits[0]), None);
}

/// The string literals of Rust source outside comments, with the byte offset
/// of each opening quote. Raw strings are read as ordinary ones, which the
/// sources scanned below do not contain in a way that matters.
fn string_literals(text: &str) -> Vec<(usize, &str)> {
    let bytes = text.as_bytes();
    let mut out = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'/' if bytes.get(i + 1) == Some(&b'/') => {
                while i < bytes.len() && bytes[i] != b'\n' {
                    i += 1;
                }
            }
            b'/' if bytes.get(i + 1) == Some(&b'*') => {
                i += 2;
                while i + 1 < bytes.len() && !(bytes[i] == b'*' && bytes[i + 1] == b'/') {
                    i += 1;
                }
                i += 2;
            }
            // A character literal (`'"'`, `'\"'`) must not open a string; a
            // lifetime has no closing quote and is skipped by its tick.
            b'\'' if bytes.get(i + 1) == Some(&b'\\') => {
                i += 2 + text[i + 2..].find('\'').map_or(0, |end| end + 1);
            }
            b'\'' if bytes.get(i + 2) == Some(&b'\'') => i += 3,
            b'"' => {
                let start = i;
                i += 1;
                while i < bytes.len() && bytes[i] != b'"' {
                    i += if bytes[i] == b'\\' { 2 } else { 1 };
                }
                out.push((start, &text[start + 1..i.min(text.len())]));
                i += 1;
            }
            _ => i += 1,
        }
    }
    out
}

/// The arguments of the call whose opening parenthesis ends `text[..open]`,
/// split at commas outside brackets and strings.
fn call_arguments(text: &str, open: usize) -> Vec<&str> {
    let bytes = text.as_bytes();
    let (mut depth, mut start, mut i) = (1, open, open);
    let mut args = Vec::new();
    while i < bytes.len() {
        match bytes[i] {
            b'"' => {
                i += 1;
                while i < bytes.len() && bytes[i] != b'"' {
                    i += if bytes[i] == b'\\' { 2 } else { 1 };
                }
            }
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' => {
                depth -= 1;
                if depth == 0 {
                    args.push(text[start..i].trim());
                    return args;
                }
            }
            b',' if depth == 1 => {
                args.push(text[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
        i += 1;
    }
    args
}

/// Whether `argument` is a path written out as string literals alone
/// (`"core::ops::Drop"`, `&["ops", "Drop"]`), and not a variable, a constant
/// or a computed value.
fn is_written_out(argument: &str) -> bool {
    let rest = argument.trim_start_matches(['&', '[', ' ']);
    let mut rest = rest;
    let mut seen = false;
    while let Some(after) = rest.strip_prefix('"') {
        let Some(end) = after.find('"') else {
            return false;
        };
        seen = true;
        rest = after[end + 1..].trim_start_matches([',', ' ', ']']);
    }
    seen && rest.is_empty()
}

/// Whether the literal at `offset` is inside the argument list of a call of
/// `callee`, the sanctioned way to ask a deciding trait.
fn inside_call(text: &str, offset: usize, callee: &str) -> bool {
    let Some(call) = text[..offset].rfind(callee) else {
        return false;
    };
    let between = &text[call + callee.len()..offset];
    // The call must be open: more opening than closing parentheses between.
    between.matches('(').count() >= between.matches(')').count()
        && !text[..call].ends_with("fn ")
        && !between.contains(';')
}

fn comment_free_line_start(text: &str, offset: usize) -> bool {
    let line_start = text[..offset].rfind('\n').map_or(0, |i| i + 1);
    !text[line_start..offset].trim_start().starts_with("//")
}

/// What the scan below reads, tested on a sample: literals outside comments
/// and char literals, and the arguments of a call.
#[test]
fn the_scan_for_deciding_trait_names_reads_strings_and_arguments() {
    let text = "fn f() {
    // \"Drop\" in a comment
    let q = '\"'; let r = '\\\"';
    let a = g(db, \"core::ops::Drop\", &[\"ops\", \"Copy\"], other(1, 2));
    /* \"Fn\" */ let b = \"FnMut\";
}";
    let literals: Vec<&str> = string_literals(text).into_iter().map(|(_, s)| s).collect();
    assert_eq!(literals, ["core::ops::Drop", "ops", "Copy", "FnMut"]);
    let open = text.find("g(").unwrap() + 2;
    let args = call_arguments(text, open);
    assert_eq!(args.len(), 4, "{args:?}");
    assert!(is_written_out(args[1]) && is_written_out(args[2]));
    assert!(!is_written_out(args[0]) && !is_written_out(args[3]));
    let offset = text.find("\"core::ops::Drop\"").unwrap();
    assert!(inside_call(text, offset, "g("));
    assert!(!inside_call(text, text.find("\"FnMut\"").unwrap(), "g("));
}

/// Compiler code asks whether a type implements a deciding trait through
/// `presence_trait`, which asserts the declaration. A consumer that reaches
/// such a trait another way would bypass the declaration, and a generated
/// implementation could then change what it decides without being refused.
///
/// This is a scan of the Rust sources of the compiler, not a proof. It
/// covers:
/// - every string literal that is a deciding trait's name or a path to it
///   (`"Drop"`, `"core::ops::Drop"`, an element of `&["ops", "Drop"]`),
///   wherever it is used (a resolver, a core method lookup, a closure or a
///   constant), except inside the arguments of `presence_trait`;
/// - every call of a resolver (`resolve_core_trait`, `resolve_lib_trait_path`,
///   `resolve_lib_trait`, `core_method_callee_key`, `resolve_core_method`)
///   whose trait path is not written out as string literals, so a variable, a
///   constant or a computed path is a place to read.
///
/// It does not cover: a deciding trait reached without its name appearing as
/// one string in these sources (a name assembled at run time, a trait found
/// from an implementation, a method or an item of another trait); the name in
/// a raw string or a macro; code outside `crates/*/src`; and Fe sources, where
/// the library itself uses the traits. A new place fails the test: route the
/// question through `presence_trait`, or add the place below with the reason
/// it only compares with or recognises a written implementation.
#[test]
fn compiler_code_resolves_a_deciding_trait_only_through_the_helper() {
    use hir::analysis::ty::corelib::presence_traits;
    use std::collections::BTreeMap;
    let db = DriverDataBase::default();
    let scope = db.builtin_std().root_mod(&db).scope();
    let names: Vec<String> = presence_traits(&db, scope)
        .into_iter()
        .map(|trait_| trait_.name(&db).to_opt().unwrap().data(&db).to_string())
        .collect();
    assert_eq!(names.len(), 18);
    let crates = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    // The position of the trait path among a resolver's arguments.
    let resolvers: [(&str, usize); 5] = [
        ("resolve_core_trait(", 2),
        ("resolve_lib_trait_path(", 2),
        ("resolve_lib_trait(", 2),
        ("core_method_callee_key(", 3),
        ("resolve_core_method(", 0),
    ];
    let mut named: BTreeMap<(String, String), usize> = BTreeMap::new();
    let mut computed: BTreeMap<(String, String), usize> = BTreeMap::new();
    let mut pending = vec![];
    for entry in std::fs::read_dir(&crates).unwrap() {
        let src = entry.unwrap().path().join("src");
        if src.is_dir() {
            pending.push(src);
        }
    }
    while let Some(dir) = pending.pop() {
        for entry in std::fs::read_dir(&dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                pending.push(path);
                continue;
            }
            if !path.extension().is_some_and(|ext| ext == "rs") {
                continue;
            }
            let text = std::fs::read_to_string(&path).unwrap();
            let file = path
                .strip_prefix(&crates)
                .unwrap()
                .components()
                .map(|component| component.as_os_str().to_string_lossy())
                .collect::<Vec<_>>()
                .join("/");
            for (offset, literal) in string_literals(&text) {
                let last = literal.rsplit("::").next().unwrap();
                let a_path = literal == last
                    || literal.starts_with("core::")
                    || literal.starts_with("std::");
                if a_path
                    && names.iter().any(|name| name == last)
                    && !inside_call(&text, offset, "presence_trait(")
                {
                    *named.entry((file.clone(), last.to_string())).or_default() += 1;
                }
            }
            for (callee, index) in resolvers {
                for (start, _) in text.match_indices(callee) {
                    if text[..start].ends_with("fn ") || !comment_free_line_start(&text, start) {
                        continue;
                    }
                    let args = call_arguments(&text, start + callee.len());
                    if !args.get(index).is_some_and(|arg| is_written_out(arg)) {
                        *computed
                            .entry((file.clone(), callee.trim_end_matches('(').to_string()))
                            .or_default() += 1;
                    }
                }
            }
        }
    }
    let expected = |list: &[(&str, &str, usize)]| {
        list.iter()
            .map(|&(file, what, count)| ((file.to_string(), what.to_string()), count))
            .collect::<BTreeMap<_, _>>()
    };
    let expected_named = expected(&[
        // Which function of a `Drop` implementation is `drop`, a direct call of
        // it, and the call that drop glue makes after the presence of `Drop`
        // was decided: identities of written code.
        ("hir/src/analysis/ty/drop_glue.rs", "Drop", 1),
        ("hir/src/analysis/semantic/access/implicit.rs", "Drop", 1),
        ("hir/src/analysis/ty/ty_check/expr.rs", "Drop", 1),
        ("mir/src/runtime/lower/body/drops.rs", "Drop", 2),
        // Whether a written `Pinned` implementation names a state space.
        ("hir/src/analysis/ty/provider.rs", "Pinned", 1),
        // Checks of written `Copy`, `Drop` and `Pinned` implementations.
        ("hir/src/diagnosable.rs", "Copy", 5),
        ("hir/src/diagnosable.rs", "Drop", 2),
        ("hir/src/diagnosable.rs", "Pinned", 1),
        // Tests of `ty_is_copy`.
        ("hir/src/analysis/ty/mod.rs", "Copy", 2),
        // The names `Driver` and `Producer` that `loop_family_trait` hands to
        // `presence_trait`.
        ("hir/src/analysis/ty/ty_check/stmt.rs", "Driver", 3),
        ("hir/src/analysis/ty/ty_check/stmt.rs", "Producer", 3),
        // Whether a trait is `Fn` or `FnMut` by its name, with the paths
        // handed to `presence_trait`, and the name shown in a diagnostic.
        ("hir/src/analysis/ty/closure.rs", "Fn", 2),
        ("hir/src/analysis/ty/closure.rs", "FnMut", 2),
        ("hir/src/analysis/ty/trait_def.rs", "Fn", 1),
        ("hir/src/analysis/ty/trait_def.rs", "FnMut", 1),
        // Checks of written named implementations of `Copy` and `Drop`.
        ("hir/src/analysis/ty/trait_def.rs", "Copy", 1),
        ("hir/src/analysis/ty/trait_def.rs", "Drop", 1),
        ("hir/src/analysis/ty/trait_def.rs", "Snapshot", 1),
        ("hir/src/analysis/ty/trait_def.rs", "Pinned", 1),
        ("hir/src/analysis/ty/trait_def.rs", "ErrorVariant", 1),
        // The `impl ErrorVariant<Sol>` that `#[error]` writes.
        ("hir/src/core/lower/error.rs", "ErrorVariant", 1),
        // The syntax of `Fn(A) -> R` and `FnMut(A) -> R`.
        ("parser/src/parser/param.rs", "Fn", 1),
        ("parser/src/parser/param.rs", "FnMut", 1),
        // ABI checks of written fields and messages, where an absent `Encode`
        // is an error.
        ("hir/src/analysis/analysis_pass.rs", "Encode", 1),
        ("hir/src/analysis/ty/mod.rs", "Encode", 1),
        ("hir/src/analysis/ty/msg_selector.rs", "Encode", 1),
        // The `impl Encode<Sol>` that `#[error]`, `msg` and `#[abi]` write.
        ("hir/src/core/lower/error.rs", "Encode", 1),
        ("hir/src/core/lower/msg.rs", "Encode", 1),
        ("hir/src/core/lower/abi_struct.rs", "Encode", 1),
        // A parser test of a trait bound.
        ("parser/src/ast/item.rs", "Encode", 1),
        // The names of the wrapping operator traits that `mode_op` hands to
        // `presence_trait`, and that the operator calls after it.
        ("hir/src/analysis/ty/ty_check/expr.rs", "WrappingAdd", 1),
        ("hir/src/analysis/ty/ty_check/expr.rs", "WrappingSub", 1),
        ("hir/src/analysis/ty/ty_check/expr.rs", "WrappingMul", 1),
        ("hir/src/analysis/ty/ty_check/expr.rs", "WrappingPow", 1),
        ("hir/src/analysis/ty/ty_check/expr.rs", "WrappingNeg", 1),
    ]);
    let expected_computed = expected(&[
        // The two ends of a core method lookup: the callers pass the path
        // written out, and the places that do are counted above.
        (
            "hir/src/analysis/semantic/instance/const_ref.rs",
            "resolve_core_trait",
            1,
        ),
        ("mir/src/runtime/lower/body.rs", "core_method_callee_key", 1),
        // Helpers of the library accessors: a path or a trait handed on by
        // the caller, and the path of a library trait that is not a deciding
        // one (the seal of the EVM capabilities).
        ("hir/src/analysis/ty/corelib.rs", "resolve_lib_trait", 1),
        (
            "hir/src/analysis/ty/corelib.rs",
            "resolve_lib_trait_path",
            5,
        ),
        (
            "hir/src/analysis/ty/ty_check/expr.rs",
            "resolve_core_trait",
            2,
        ),
        // `Collection`, which does not decide anything, by `["iter", name]`.
        (
            "hir/src/analysis/ty/ty_check/stmt.rs",
            "resolve_core_trait",
            1,
        ),
    ]);
    assert_eq!(
        named, expected_named,
        "a deciding trait is named outside `presence_trait`"
    );
    assert_eq!(
        computed, expected_computed,
        "a trait path is computed for a resolver"
    );
}

/// Why compiler code resolves a trait of core or std, by the one phrase that
/// says why a generated implementation of the trait cannot change a program
/// silently there, or why it is refused.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Reason {
    /// Asks whether a type implements a trait that declares `#[decides]`,
    /// through `presence_trait`.
    Deciding,
    /// The path comes from the caller or from a list of constants; the
    /// callers are rows of their own.
    HandedOn,
    /// An unsatisfied bound is an error, and a satisfied one does nothing else.
    ErrorOnly,
    /// One implementation is selected, else the use is an error or ambiguous.
    Unique,
    /// Written candidates answer first and generated ones only when none does.
    AuthoredFirst,
    /// Recognises a trait or one of its methods by identity, after a deciding
    /// question was answered elsewhere, or in a check of written code.
    Identity,
    /// The trait is sealed by a private supertrait of std, so no package can
    /// implement it.
    Sealed,
    /// The compiler supplies the implementation, which a package cannot
    /// replace.
    Structural,
    /// Test code.
    Test,
}

/// Every place where compiler code resolves a trait of core or std by a path
/// or a name: the file under `crates/`, the resolver, the trait argument as
/// written, the number of calls, and the reason. `Deciding` is the only reason
/// that a generated implementation is refused for; the others are why it need
/// not be.
#[rustfmt::skip]
const LIBRARY_TRAIT_RESOLUTIONS: &[(&str, &str, &str, usize, Reason, &str)] = {
    use Reason::*;
    &[
        ("fe/src/abi.rs", "resolve_lib_trait_path", "\"std::evm::effects::Log\"", 1, Identity, "classifies a callee that is already resolved"),
        ("fe/src/abi.rs", "resolve_lib_trait_path", "\"std::evm::event::Event\"", 1, Identity, "classifies a callee that is already resolved"),
        ("hir/src/analysis/analysis_pass.rs", "resolve_core_trait", "&[\"abi\", \"AbiSize\"]", 1, ErrorOnly, "ABI checks of `#[abi]` fields"),
        ("hir/src/analysis/analysis_pass.rs", "resolve_core_trait", "&[\"abi\", \"Encode\"]", 1, ErrorOnly, "ABI checks of `#[abi]` fields"),
        ("hir/src/analysis/analysis_pass.rs", "resolve_lib_trait_path", "\"std::abi::SolCompat\"", 1, ErrorOnly, "ABI checks of `#[abi]` fields"),
        ("hir/src/analysis/semantic/access/implicit.rs", "resolve_core_trait", "&[\"abi\", \"Decode\"]", 1, Unique, "the trust walk follows the instance that the contract lowering selects, and none where that is an error"),
        ("hir/src/analysis/semantic/access/implicit.rs", "resolve_core_trait", "&[\"contracts\", \"ContractHost\"]", 1, Unique, "the trust walk follows the instance that the contract lowering selects, and none where that is an error"),
        ("hir/src/analysis/semantic/access/implicit.rs", "core_method_callee_key", "&[\"ops\", \"Drop\"]", 1, Identity, "the `drop` call of drop glue that the trust walk follows, after its presence was decided"),
        ("hir/src/analysis/semantic/ctfe/machine.rs", "resolve_core_trait", "&[\"meta\", \"FieldQuery\"]", 1, Unique, "the query of a field is selected or an error"),
        ("hir/src/analysis/semantic/instance/const_ref.rs", "resolve_core_trait", "trait_path", 1, HandedOn, "the core method lookup; its callers pass the path written out"),
        ("hir/src/analysis/semantic/instance/semantic.rs", "presence_trait", "\"core::abi::Encode\"", 1, Deciding, "whether a panic off the EVM reverts with its value or traps"),
        ("hir/src/analysis/semantic/instance/semantic.rs", "resolve_core_trait", "&[\"abi\", \"AbiSize\"]", 1, Identity, "the supertrait of `Encode`, asked with it"),
        ("hir/src/analysis/ty/abi_ty.rs", "resolve_lib_trait_path", "\"std::abi::SolCompat\"", 1, Unique, "an enum's ABI type name: absent is an error, two are ambiguous"),
        ("hir/src/analysis/ty/abi_ty.rs", "resolve_lib_trait_path", "\"core::AsBytes\"", 1, Unique, "an enum's ABI type name: absent is an error, two are ambiguous"),
        ("hir/src/analysis/ty/adt_def.rs", "presence_trait", "\"core::marker::Pinned\"", 1, Deciding, "whether a type is pinned to a state space"),
        ("hir/src/analysis/ty/clearable.rs", "resolve_core_trait", "&[\"marker\", \"Clearable\"]", 1, Structural, "the compiler writes `Clearable` and a package cannot"),
        ("hir/src/analysis/ty/closure.rs", "presence_trait", "path", 1, HandedOn, "`Fn` and `FnMut`, named in the caller"),
        ("hir/src/analysis/ty/corelib.rs", "resolve_lib_trait", "path_id", 1, HandedOn, "the cached end of the library accessors"),
        ("hir/src/analysis/ty/corelib.rs", "resolve_lib_trait_path", "\"core::meta::FieldQuery\"", 1, Identity, "recognises core's own trait"),
        ("hir/src/analysis/ty/corelib.rs", "resolve_lib_trait_path", "\"std::evm::Contract\"", 1, Identity, "recognises the metadata methods of the trait"),
        ("hir/src/analysis/ty/corelib.rs", "resolve_lib_trait_path", "path", 3, HandedOn, "the helper of `presence_trait` and the sealed EVM capability lists"),
        ("hir/src/analysis/ty/corelib.rs", "resolve_lib_trait_path", "EVM_CAPABILITY_SEAL", 2, Sealed, "the private seal of std's EVM capabilities"),
        ("hir/src/analysis/ty/drop_glue.rs", "resolve_core_trait", "&[\"ops\", \"Drop\"]", 1, Identity, "which function of `Drop` is `drop`, after its presence was decided"),
        ("hir/src/analysis/ty/drop_glue.rs", "presence_trait", "\"core::ops::Drop\"", 1, Deciding, "whether a type has drop glue"),
        ("hir/src/analysis/ty/evidence.rs", "resolve_core_trait", "&[\"marker\", \"Clearable\"]", 1, Structural, "the deferred proof regression checks the compiler's structural `Clearable` proof"),
        ("hir/src/analysis/ty/mod.rs", "resolve_core_trait", "&[\"abi\", \"AbiSize\"]", 1, ErrorOnly, "ABI checks of `msg` and `#[abi]` fields"),
        ("hir/src/analysis/ty/mod.rs", "resolve_core_trait", "&[\"abi\", \"AbiSpan\"]", 1, ErrorOnly, "ABI checks of `msg` and `#[abi]` fields"),
        ("hir/src/analysis/ty/mod.rs", "resolve_core_trait", "&[\"abi\", \"Encode\"]", 1, ErrorOnly, "ABI checks of `msg` and `#[abi]` fields"),
        ("hir/src/analysis/ty/mod.rs", "resolve_core_trait", "&[\"abi\", \"Decode\"]", 1, ErrorOnly, "ABI checks of `msg` and `#[abi]` fields"),
        ("hir/src/analysis/ty/mod.rs", "resolve_core_trait", "&[\"contracts\", \"Target\"]", 1, Structural, "the default root effect of std's own types"),
        ("hir/src/analysis/ty/mod.rs", "resolve_core_trait", "&[\"marker\", \"Copy\"]", 2, Test, "tests of `ty_is_copy`"),
        ("hir/src/analysis/ty/mod.rs", "resolve_lib_trait_path", "\"std::abi::SolCompat\"", 2, ErrorOnly, "ABI checks of `msg` and `#[abi]` fields"),
        ("hir/src/analysis/ty/mod.rs", "presence_trait", "\"core::marker::Snapshot\"", 1, Deciding, "whether a type is a snapshot"),
        ("hir/src/analysis/ty/mod.rs", "presence_trait", "\"core::ops::StateIndex\"", 1, Deciding, "whether a type indexes state"),
        ("hir/src/analysis/ty/mod.rs", "presence_trait", "\"core::marker::Copy\"", 1, Deciding, "whether a value is copied or moved"),
        ("hir/src/analysis/ty/msg_selector.rs", "resolve_core_trait", "&[\"abi\", \"AbiSize\"]", 1, ErrorOnly, "ABI checks of `msg` fields"),
        ("hir/src/analysis/ty/msg_selector.rs", "resolve_core_trait", "&[\"abi\", \"Encode\"]", 1, ErrorOnly, "ABI checks of `msg` fields"),
        ("hir/src/analysis/ty/msg_selector.rs", "resolve_core_trait", "&[\"abi\", \"Decode\"]", 1, ErrorOnly, "ABI checks of `msg` fields"),
        ("hir/src/analysis/ty/provider.rs", "resolve_core_trait", "&[\"marker\", \"Pinned\"]", 1, Identity, "whether a written `Pinned` names a state space"),
        ("hir/src/analysis/ty/provider.rs", "presence_trait", "\"core::marker::Pinned\"", 1, Deciding, "whether a type is pinned to a state space"),
        ("hir/src/analysis/ty/provider.rs", "presence_trait", "\"core::ops::StateIndex\"", 1, Deciding, "whether a type indexes state"),
        ("hir/src/analysis/ty/provider.rs", "presence_trait", "\"core::ops::LaneCodec\"", 1, Deciding, "whether the entries of a state index are packed"),
        ("hir/src/analysis/ty/sol_codec.rs", "presence_trait", "\"std::evm::SolPacked\"", 1, Deciding, "whether a type is stored in one slot with others"),
        ("hir/src/analysis/ty/sol_codec.rs", "presence_trait", "\"core::ops::LaneCodec\"", 1, Deciding, "whether the entries of a state index are packed"),
        ("hir/src/analysis/ty/trait_def.rs", "resolve_core_trait", "&[\"error\", \"ErrorVariant\"]", 1, Identity, "checks of written named implementations"),
        ("hir/src/analysis/ty/trait_def.rs", "resolve_core_trait", "&[\"marker\", \"Pinned\"]", 1, Identity, "checks of written named implementations"),
        ("hir/src/analysis/ty/trait_def.rs", "resolve_core_trait", "&[\"marker\", \"Snapshot\"]", 1, Identity, "checks of written named implementations"),
        ("hir/src/analysis/ty/trait_def.rs", "resolve_core_trait", "&[\"marker\", \"Copy\"]", 1, Identity, "checks of written named implementations"),
        ("hir/src/analysis/ty/trait_def.rs", "resolve_core_trait", "&[\"ops\", \"Drop\"]", 1, Identity, "checks of written named implementations"),
        ("hir/src/analysis/ty/trait_resolution/generation.rs", "resolve_core_trait", "&[\"derive\", \"Derive\"]", 1, Identity, "the request machinery"),
        ("hir/src/analysis/ty/ty_check/contract.rs", "resolve_core_trait", "&[\"abi\", \"Decode\"]", 1, ErrorOnly, "a recv message must decode"),
        ("hir/src/analysis/ty/ty_check/expr.rs", "resolve_core_trait", "&[\"ops\", \"Drop\"]", 1, Identity, "a direct call of `drop`, after its presence was decided"),
        ("hir/src/analysis/ty/ty_check/expr.rs", "resolve_core_trait", "&op.trait_path_segments()", 1, AuthoredFirst, "operator traits"),
        ("hir/src/analysis/ty/ty_check/expr.rs", "resolve_core_trait", "&UnOp::Minus.trait_path_segments()", 1, AuthoredFirst, "the trait named in the diagnostic for unary negation"),
        ("hir/src/analysis/ty/ty_check/expr.rs", "presence_trait", "&format!(\"core::{}\", mode_op.trait_path_segments().join(\"::\"))", 1, HandedOn, "the wrapping operator traits, named in `ModeOp::triple`"),
        ("hir/src/analysis/ty/ty_check/expr.rs", "presence_trait", "\"core::ops::StateIndex\"", 1, Deciding, "whether a type indexes state"),
        ("hir/src/analysis/ty/ty_check/expr.rs", "presence_trait", "\"core::ops::LaneCodec\"", 1, Deciding, "whether the entries of a state index are packed"),
        ("hir/src/analysis/ty/ty_check/stmt.rs", "resolve_core_trait", "&[\"iter\", name]", 1, ErrorOnly, "`Collection`: absent is an error"),
        ("hir/src/analysis/ty/ty_check/stmt.rs", "presence_trait", "&format!(\"core::iter::{name}\")", 1, HandedOn, "`Driver` and `Producer`, named in the caller"),
        ("hir/src/core/meta.rs", "resolve_core_trait", "&[\"derive\", \"Derive\"]", 1, Identity, "the written trait derive protocol"),
        ("hir/src/core/meta.rs", "resolve_core_trait", "&[\"derive\", \"DeriveInherent\"]", 1, Identity, "the written inherent derive protocol"),
        ("hir/src/core/semantic/mod.rs", "resolve_lib_trait_path", "\"core::message::MsgVariant\"", 2, Unique, "the implementation for the ABI of a recv block is selected, or an arm that names none is an error"),
        ("hir/src/core/semantic/storage_layout.rs", "presence_trait", "\"std::evm::SolPacked\"", 1, Deciding, "how many bits a stored value packs into"),
        ("hir/src/core/semantic/storage_layout.rs", "resolve_lib_trait_path", "\"std::evm::sol_slot::SolValueType\"", 1, Sealed, "a `pub(ingot)` marker of std that no other package can name"),
        ("hir/src/diagnosable.rs", "resolve_core_trait", "&[\"marker\", \"Copy\"]", 5, Identity, "checks of written implementations"),
        ("hir/src/diagnosable.rs", "resolve_core_trait", "&[\"ops\", \"Drop\"]", 2, Identity, "checks of written implementations"),
        ("hir/src/diagnosable.rs", "resolve_core_trait", "&[\"marker\", \"Pinned\"]", 1, Identity, "checks of written implementations"),
        ("hir/src/diagnosable.rs", "resolve_core_trait", "&[\"marker\", \"Clearable\"]", 1, ErrorOnly, "an implementation of `Clearable` is an error"),
        ("mir/src/runtime/lower/body.rs", "presence_trait", "\"core::error::ErrorVariant\"", 1, Deciding, "whether an error type prefixes its revert data"),
        ("mir/src/runtime/lower/body.rs", "core_method_callee_key", "trait_path", 1, HandedOn, "the core method lookup; its callers pass the path written out"),
        ("mir/src/runtime/lower/body/drops.rs", "core_method_callee_key", "&[\"ops\", \"Drop\"]", 1, Identity, "the `drop` call of drop glue, after its presence was decided"),
        ("mir/src/runtime/lower/body/drops.rs", "resolve_core_method", "&[\"ops\", \"Drop\"]", 1, Identity, "the `drop` call of drop glue, after its presence was decided"),
        ("mir/src/runtime/package.rs", "resolve_core_trait", "&[\"contracts\", \"ContractHost\"]", 1, Unique, "resolved as a unique runtime instance, else an error"),
        ("mir/src/runtime/package.rs", "resolve_core_trait", "&[\"abi\", \"Decode\"]", 1, Unique, "resolved as a unique runtime instance, else an error"),
        ("mir/src/runtime/synthetic.rs", "resolve_core_trait", "&[\"abi\", \"Abi\"]", 1, Unique, "resolved as a unique runtime instance, else an error"),
    ]
};

/// Every call that resolves a trait of core or std by a path or a name in the
/// Rust sources of the compiler, as `(file, resolver, trait argument)` with
/// its number of calls. The file is relative to `crates/`; the argument is the
/// source text with white space collapsed.
fn library_trait_resolutions() -> std::collections::BTreeMap<(String, String, String), usize> {
    // The position of the trait path among a resolver's arguments.
    let resolvers: [(&str, usize); 6] = [
        ("resolve_core_trait(", 2),
        ("resolve_lib_trait_path(", 2),
        ("resolve_lib_trait(", 2),
        ("presence_trait(", 2),
        ("core_method_callee_key(", 3),
        ("resolve_core_method(", 0),
    ];
    let crates = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let mut found = std::collections::BTreeMap::new();
    let mut pending = vec![];
    for entry in std::fs::read_dir(&crates).unwrap() {
        let src = entry.unwrap().path().join("src");
        if src.is_dir() {
            pending.push(src);
        }
    }
    while let Some(dir) = pending.pop() {
        for entry in std::fs::read_dir(&dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                pending.push(path);
                continue;
            }
            if !path.extension().is_some_and(|ext| ext == "rs") {
                continue;
            }
            let text = std::fs::read_to_string(&path).unwrap();
            let file = path
                .strip_prefix(&crates)
                .unwrap()
                .components()
                .map(|component| component.as_os_str().to_string_lossy())
                .collect::<Vec<_>>()
                .join("/");
            for (callee, index) in resolvers {
                for (start, _) in text.match_indices(callee) {
                    if text[..start].ends_with("fn ") || !comment_free_line_start(&text, start) {
                        continue;
                    }
                    let args = call_arguments(&text, start + callee.len());
                    let argument = args
                        .get(index)
                        .map(|arg| arg.split_whitespace().collect::<Vec<_>>().join(" "))
                        .unwrap_or_default();
                    *found
                        .entry((
                            file.clone(),
                            callee.trim_end_matches('(').to_string(),
                            argument,
                        ))
                        .or_default() += 1;
                }
            }
        }
    }
    found
}

/// Compiler code decides by a trait of core or std only if it resolves the
/// trait by path or by name first. This test lists every such place with the
/// reason that a generated implementation of the trait cannot change a program
/// silently there, and fails when a place appears or disappears. The two
/// consumers of a trait that no bound names (`MsgVariant`, `LaneCodec`) were
/// found by reading; a new one would be found here.
///
/// A trait found another way is not listed: from an implementation, from a
/// bound or an associated type of another trait, or from a method. Fe sources
/// and code outside `crates/*/src` are not scanned either.
///
/// A new place fails the test with its file, resolver and argument. Add a row
/// to `LIBRARY_TRAIT_RESOLUTIONS` with the reason, and if the answer chooses
/// between two behaviours, declare the trait `#[decides]` and ask through
/// `presence_trait`.
#[test]
fn every_resolution_of_a_library_trait_in_compiler_code_has_a_reason() {
    use hir::analysis::ty::corelib::presence_traits;
    use std::collections::BTreeMap;

    let found = library_trait_resolutions();
    let mut listed: BTreeMap<(String, String, String), usize> = BTreeMap::new();
    for &(file, resolver, argument, count, _, _) in LIBRARY_TRAIT_RESOLUTIONS {
        let key = (file.to_string(), resolver.to_string(), argument.to_string());
        assert!(
            listed.insert(key, count).is_none(),
            "{file} {resolver}({argument}) is listed twice"
        );
    }
    let appeared: Vec<_> = found
        .iter()
        .filter(|(key, count)| listed.get(*key) != Some(*count))
        .collect();
    let disappeared: Vec<_> = listed
        .iter()
        .filter(|(key, count)| found.get(*key) != Some(*count))
        .collect();
    assert!(
        appeared.is_empty() && disappeared.is_empty(),
        "the resolutions of library traits changed.\nin the sources, not in the list: {appeared:#?}\nin the list, not in the sources: {disappeared:#?}"
    );

    // The reasons agree with how the trait is reached.
    let db = DriverDataBase::default();
    let scope = db.builtin_std().root_mod(&db).scope();
    let deciding: Vec<String> = presence_traits(&db, scope)
        .into_iter()
        .map(|trait_| trait_.name(&db).to_opt().unwrap().data(&db).to_string())
        .collect();
    for &(file, resolver, argument, _, reason, why) in LIBRARY_TRAIT_RESOLUTIONS {
        let place = format!("{file} {resolver}({argument})");
        assert!(!why.is_empty() && !why.ends_with('.'), "{place}: a phrase");
        let written_out = is_written_out(argument);
        match reason {
            Reason::Deciding => {
                assert_eq!(resolver, "presence_trait", "{place}: a deciding trait");
                let path = argument.trim_matches('"');
                let name = path.rsplit("::").next().unwrap();
                assert!(
                    written_out && deciding.iter().any(|declared| declared == name),
                    "{place}: `{name}` does not declare `#[decides]`"
                );
            }
            Reason::HandedOn => assert!(!written_out, "{place}: the path is written out"),
            _ => assert_ne!(
                resolver, "presence_trait",
                "{place}: `presence_trait` is for deciding traits"
            ),
        }
        if reason != Reason::HandedOn && resolver == "presence_trait" {
            assert_eq!(reason, Reason::Deciding, "{place}");
        }
    }
}
