use camino::Utf8PathBuf;
use fe_hir::analysis::semantic::{
    GenericSubst, get_or_build_semantic_instance, instantiate_typed_body,
    root_semantic_instance_key, typed_body_template,
};
use fe_hir::analysis::ty::trait_def::resolve_trait_method_instance;
use fe_hir::analysis::ty::trait_resolution::TraitSolveCx;
use fe_hir::analysis::ty::ty_check::{BodyOwner, check_func_body};
use fe_hir::hir_def::{Expr, ItemKind, Partial};
use fe_hir::test_db::HirAnalysisTestDb;

#[test]
fn resolve_trait_method_instance_uses_inherited_default_body() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("resolve_trait_method_instance_uses_inherited_default_body.fe"),
        r#"
trait CreateLike {
    fn create2(mut self, value: u256, args: u256, salt: u256) -> u256 {
        value + args + salt
    }
}

struct Evm {}

impl CreateLike for Evm {}

fn run(evm: mut Evm) -> u256 {
    evm.create2(value: 1, args: 2, salt: 3)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let trait_ = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Trait(trait_)
                if trait_
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "CreateLike") =>
            {
                Some(trait_)
            }
            _ => None,
        })
        .expect("missing trait");
    let impl_trait = top_mod.all_impl_traits(&db)[0];
    let inst = impl_trait
        .trait_inst(&db)
        .expect("missing impl-trait instance");
    let (method_name, trait_method) = trait_
        .method_defs(&db)
        .iter()
        .find(|(name, _)| name.data(&db) == "create2")
        .map(|(name, method)| (*name, *method))
        .expect("missing trait method");

    let resolved = resolve_trait_method_instance(
        &db,
        TraitSolveCx::new(&db, impl_trait.scope()),
        inst,
        method_name,
    )
    .expect("missing resolved method");
    let resolved_method = resolved.func;

    assert_eq!(resolved_method, trait_method);
    assert!(resolved_method.body(&db).is_some());

    let instantiated = instantiate_typed_body(
        &db,
        typed_body_template(&db, BodyOwner::Func(resolved_method)),
        GenericSubst::new(&db, resolved.impl_args),
    );
    let self_binding = instantiated.param_binding(0).expect("missing self binding");
    let self_ty = instantiated.binding_ty(&db, self_binding);
    assert_eq!(self_ty.pretty_print(&db).to_string(), "mut Evm");
}

#[test]
fn semantic_callee_key_uses_std_create2_default_body() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("semantic_callee_key_uses_std_create2_default_body.fe"),
        r#"
pub contract C {}

fn run() uses (evm: mut Evm) {
    let _ = evm.create2<C>(value: 0, args: (), salt: 0)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let run = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "run") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing run function");
    let typed_body = check_func_body(&db, run).1.clone();
    let body = run.body(&db).expect("missing run body");
    let method_call = body
        .exprs(&db)
        .keys()
        .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::MethodCall(..))))
        .expect("missing create2 call");
    typed_body
        .callable_expr(method_call)
        .expect("missing callable for create2");
    let semantic = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(run)).expect("missing root semantic key"),
    );
    let create2 = semantic
        .callees(&db)
        .iter()
        .find_map(|callee| match callee.key.owner(&db) {
            BodyOwner::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "create2") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing create2 semantic callee");
    assert!(
        create2.body(&db).is_some(),
        "create2 callee should use the default body"
    );
}

#[test]
fn test_roots_keep_std_create2_as_a_body_backed_trait_call() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("test_roots_keep_std_create2_as_a_body_backed_trait_call.fe"),
        r#"
pub contract C {}

#[test]
fn test_large_by_value_array_args() uses (evm: mut Evm) {
    let c = evm.create2<C>(value: 0, args: (), salt: 0)
    let out: Address = c
    assert!(out.inner == c.inner)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let test_func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "test_large_by_value_array_args") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing test function");
    let typed_body = check_func_body(&db, test_func).1.clone();
    let body = test_func.body(&db).expect("missing test body");
    let method_call = body
        .exprs(&db)
        .keys()
        .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::MethodCall(..))))
        .expect("missing create2 call");
    let callable = typed_body
        .callable_expr(method_call)
        .expect("missing callable for create2");
    let fe_hir::hir_def::CallableDef::Func(target) = callable.callable_def() else {
        panic!("create2 call should target a function");
    };
    assert_eq!(
        target
            .name(&db)
            .to_opt()
            .map(|name| name.data(&db).as_str()),
        Some("create2")
    );
    assert!(
        target.containing_trait(&db).is_some(),
        "typed create2 callable should still be a trait method"
    );
    let semantic = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(test_func))
            .expect("missing root semantic key"),
    );
    let create2 = semantic
        .callees(&db)
        .iter()
        .filter_map(|callee| match callee.key.owner(&db) {
            BodyOwner::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "create2") =>
            {
                Some(func)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        create2.len(),
        1,
        "expected exactly one semantic create2 callee"
    );
    assert!(
        create2[0].body(&db).is_some(),
        "semantic create2 callee should use the default body"
    );
}

#[test]
fn std_create2_raw_calls_the_low_level_extern_create2() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("std_create2_raw_calls_the_low_level_extern_create2.fe"),
        r#"
pub contract C {}

fn run() uses (evm: mut Evm) {
    let _ = evm.create2<C>(value: 0, args: (), salt: 0)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let run = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "run") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing run function");
    let root = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(run)).expect("missing root semantic key"),
    );
    let create2 = root
        .callees(&db)
        .iter()
        .find_map(|callee| match callee.key.owner(&db) {
            BodyOwner::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "create2") =>
            {
                Some(get_or_build_semantic_instance(&db, callee.key))
            }
            _ => None,
        })
        .expect("missing create2 callee");
    let create2_raw = create2
        .callees(&db)
        .iter()
        .find_map(|callee| match callee.key.owner(&db) {
            BodyOwner::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "create2_raw") =>
            {
                Some(get_or_build_semantic_instance(&db, callee.key))
            }
            _ => None,
        })
        .expect("missing create2_raw callee");
    let low_level_create2 = create2_raw
        .callees(&db)
        .iter()
        .find_map(|callee| match callee.key.owner(&db) {
            BodyOwner::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "create2") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing low-level create2 callee");
    assert!(
        low_level_create2.is_extern(&db),
        "create2_raw should call the low-level extern create2"
    );
    assert_eq!(low_level_create2.params(&db).count(), 4);
    let low_level_calls = create2_raw
        .body(&db)
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .filter_map(|stmt| match &stmt.kind {
            fe_hir::analysis::semantic::SStmtKind::Assign {
                expr: fe_hir::analysis::semantic::SExpr::Call { callee, args, .. },
                ..
            } if callee.key.owner(&db) == BodyOwner::Func(low_level_create2) => Some(args.len()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(low_level_calls, vec![4]);
}

#[test]
fn large_by_value_array_args_test_root_keeps_body_backed_create2() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("large_by_value_array_args_test_root_keeps_body_backed_create2.fe"),
        r#"
msg BigArgsMsg {
    #[selector = 0x01020304]
    Run -> u256,
}

fn prove_like(
    p_a: [u256; 2],
    p_b: [[u256; 2]; 2],
    p_c: [u256; 2],
    pub_signals: [u256; 38],
) -> u256 {
    let _ = p_b[0][0]
    let _ = p_c[0]
    let _ = pub_signals[0]
    p_a[0]
}

pub contract C {
    recv BigArgsMsg {
        Run -> u256 {
            let p_a: [u256; 2] = [1, 2]
            let p_b: [[u256; 2]; 2] = [[3, 4], [5, 6]]
            let p_c: [u256; 2] = [7, 8]
            let pub_signals: [u256; 38] = [
                9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
                28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,
            ]
            prove_like(p_a, p_b, p_c, pub_signals)
        }
    }
}

#[test]
fn test_large_by_value_array_args() uses (evm: mut Evm) {
    let c = evm.create2<C>(value: 0, args: (), salt: 0)
    let out: u256 = evm.call(
        addr: c,
        gas: 100000,
        value: 0,
        message: BigArgsMsg::Run {},
    )
    assert!(out == 1)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let test_func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "test_large_by_value_array_args") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing test function");
    let semantic = get_or_build_semantic_instance(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(test_func))
            .expect("missing root semantic key"),
    );
    let create2 = semantic
        .callees(&db)
        .iter()
        .filter_map(|callee| match callee.key.owner(&db) {
            BodyOwner::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "create2") =>
            {
                Some(func)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        create2.len(),
        1,
        "expected exactly one semantic create2 callee"
    );
    assert!(
        create2[0].body(&db).is_some(),
        "semantic create2 callee should use the default body in the large-array test root"
    );
}
