use camino::Utf8PathBuf;
use ena::unify::InPlace;
use fe_hir::{
    analysis::ty::{
        ty_check::{BodyOwner, TypedCallableBody, check_func_body},
        ty_def::{
            ClosureCaptures, ClosureParamMode, ClosureSignature, ClosureTy, MAX_CLOSURE_FIELDS,
            TyId, closure_field_count_is_supported,
        },
        unify::{InferenceKey, UnificationError, UnificationTableBase},
    },
    hir_def::{Func, ItemKind, TopLevelMod},
    test_db::{HirAnalysisTestDb, format_diagnostics},
};

fn find_func<'db>(db: &'db HirAnalysisTestDb, top_mod: TopLevelMod<'db>, name: &str) -> Func<'db> {
    top_mod
        .all_items(db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|func_name| func_name.data(db) == name) =>
            {
                Some(*func)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"))
}

#[test]
fn closure_unification_rejects_logical_parameter_mode_mismatch() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_mode_unification.fe"),
        r#"
fn make() {
    let identity = |value: own u256| value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let make = find_func(&db, top_mod, "make");
    let (_, typed_body) = check_func_body(&db, make);
    let (_, info) = typed_body
        .closure_infos()
        .next()
        .expect("missing closure metadata");
    let mismatched = ClosureTy::new(
        &db,
        info.ty.def(&db),
        info.ty.parent_args(&db).clone(),
        ClosureCaptures::new(
            info.ty.captures(&db).to_vec(),
            info.ty.capture_accesses(&db).to_vec(),
        ),
        ClosureSignature::new(
            info.ty.params(&db).to_vec(),
            vec![ClosureParamMode::View],
            info.ty.ret_ty(&db),
        ),
    );

    let mut table = UnificationTableBase::<InPlace<InferenceKey<'_>>>::new(&db);
    assert_eq!(
        table.unify(TyId::closure(&db, info.ty), TyId::closure(&db, mismatched)),
        Err(UnificationError::TypeMismatch),
    );
}

#[test]
fn closure_types_print_distinctly_from_function_types() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_type_pretty_print.fe"),
        r#"
fn make() {
    let describe = |
        viewed: u256,
        owned: own u256,
        shared: ref u256,
        exclusive: mut u256,
    | -> u256 {
        viewed
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let make = find_func(&db, top_mod, "make");
    let (_, typed_body) = check_func_body(&db, make);
    let (_, info) = typed_body
        .closure_infos()
        .next()
        .expect("missing closure metadata");

    assert_eq!(
        info.ty.pretty_print(&db),
        "|view u256, own u256, ref u256, mut u256| -> u256",
    );
}

#[test]
fn typed_closure_descriptor_rejects_positional_type_drift() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_descriptor_specialization_invariant.fe"),
        r#"
struct Left {
    value: u256,
}

struct Right {
    value: u256,
}

fn make(_ first: own Left, _ second: own Right) {
    let ordered = |alpha: own Left, beta: own Right| -> u256 {
        second.value + first.value + alpha.value + beta.value
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let make = find_func(&db, top_mod, "make");
    let (_, typed_body) = check_func_body(&db, make);
    let (_, info) = typed_body
        .closure_infos()
        .next()
        .expect("missing closure metadata");
    let callable = TypedCallableBody::new(BodyOwner::closure(&db, info.ty), typed_body);
    assert!(callable.closure_body(&db, info.ty).is_some());

    let mut reversed_captures = info.ty.captures(&db).to_vec();
    assert_eq!(reversed_captures.len(), 2);
    assert_ne!(reversed_captures[0], reversed_captures[1]);
    reversed_captures.reverse();
    let capture_drift = ClosureTy::new(
        &db,
        info.ty.def(&db),
        info.ty.parent_args(&db).clone(),
        ClosureCaptures::new(reversed_captures, info.ty.capture_accesses(&db).to_vec()),
        ClosureSignature::new(
            info.ty.params(&db).to_vec(),
            info.ty.param_modes(&db).to_vec(),
            info.ty.ret_ty(&db),
        ),
    );
    assert!(
        callable.closure_body(&db, capture_drift).is_none(),
        "capture types must remain attached to their named body bindings",
    );

    let mut reversed_params = info.ty.params(&db).to_vec();
    assert_eq!(reversed_params.len(), 2);
    assert_ne!(reversed_params[0], reversed_params[1]);
    reversed_params.reverse();
    let param_drift = ClosureTy::new(
        &db,
        info.ty.def(&db),
        info.ty.parent_args(&db).clone(),
        ClosureCaptures::new(
            info.ty.captures(&db).to_vec(),
            info.ty.capture_accesses(&db).to_vec(),
        ),
        ClosureSignature::new(
            reversed_params,
            info.ty.param_modes(&db).to_vec(),
            info.ty.ret_ty(&db),
        ),
    );
    assert!(
        callable.closure_body(&db, param_drift).is_none(),
        "parameter types must remain attached to their indexed body bindings",
    );
}

#[test]
fn typed_closure_descriptor_accepts_parent_type_specialization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_descriptor_type_specialization.fe"),
        r#"
fn make<T>(_ captured: own T) {
    let consume = |value: own T| -> T {
        let _observed = captured
        value
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let make = find_func(&db, top_mod, "make");
    let (_, typed_body) = check_func_body(&db, make);
    let (_, info) = typed_body
        .closure_infos()
        .next()
        .expect("missing closure metadata");
    let specialized_ty = TyId::u256(&db);
    let specialized = ClosureTy::new(
        &db,
        info.ty.def(&db),
        vec![specialized_ty],
        ClosureCaptures::new(vec![specialized_ty], info.ty.capture_accesses(&db).to_vec()),
        ClosureSignature::new(
            vec![specialized_ty],
            info.ty.param_modes(&db).to_vec(),
            specialized_ty,
        ),
    );

    let callable = TypedCallableBody::new(BodyOwner::closure(&db, specialized), typed_body);
    let body = callable
        .closure_body(&db, specialized)
        .expect("parent type substitution must preserve descriptor identity");
    assert_eq!(
        body.params(&db).map(|param| param.ty).collect::<Vec<_>>(),
        vec![specialized_ty]
    );
    assert_eq!(
        body.captures(&db)
            .map(|capture| capture.ty)
            .collect::<Vec<_>>(),
        vec![specialized_ty]
    );
}

#[test]
fn typed_closure_descriptor_accepts_parent_const_specialization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("closure_descriptor_const_specialization.fe"),
        r#"
struct Rooted<const ROOT: u256> {
    value: u256,
}

fn make<const ROOT: u256>(_ captured: own Rooted<ROOT>) {
    let consume = |value: own Rooted<ROOT>| -> Rooted<ROOT> {
        let _observed = captured
        value
    }
}

fn concrete(_ value: own Rooted<7>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let concrete = find_func(&db, top_mod, "concrete");
    let (_, concrete_body) = check_func_body(&db, concrete);
    let concrete_binding = concrete_body
        .param_binding(0)
        .expect("missing concrete parameter");
    let specialized_ty = concrete_body.binding_ty(&db, concrete_binding);
    let specialized_const = specialized_ty.generic_args(&db)[0];

    let make = find_func(&db, top_mod, "make");
    let (_, typed_body) = check_func_body(&db, make);
    let (_, info) = typed_body
        .closure_infos()
        .next()
        .expect("missing closure metadata");
    let specialized = ClosureTy::new(
        &db,
        info.ty.def(&db),
        vec![specialized_const],
        ClosureCaptures::new(vec![specialized_ty], info.ty.capture_accesses(&db).to_vec()),
        ClosureSignature::new(
            vec![specialized_ty],
            info.ty.param_modes(&db).to_vec(),
            specialized_ty,
        ),
    );

    let callable = TypedCallableBody::new(BodyOwner::closure(&db, specialized), typed_body);
    let body = callable
        .closure_body(&db, specialized)
        .expect("parent const substitution must preserve descriptor identity");
    assert_eq!(
        body.params(&db).map(|param| param.ty).collect::<Vec<_>>(),
        vec![specialized_ty]
    );
    assert_eq!(
        body.captures(&db)
            .map(|capture| capture.ty)
            .collect::<Vec<_>>(),
        vec![specialized_ty]
    );
}

#[test]
fn closure_field_limit_matches_the_semantic_field_index_width() {
    assert!(closure_field_count_is_supported(MAX_CLOSURE_FIELDS));
    assert!(!closure_field_count_is_supported(MAX_CLOSURE_FIELDS + 1));
}

#[test]
fn oversized_closure_argument_pack_is_diagnosed() {
    let mut db = HirAnalysisTestDb::default();
    let params = (0..=MAX_CLOSURE_FIELDS)
        .map(|idx| format!("p{idx}"))
        .collect::<Vec<_>>()
        .join(",\n");
    let source = format!(
        r#"
fn probe() {{
    let oversized = |{params}| 0
}}
"#
    );
    let file = db.new_stand_alone(
        Utf8PathBuf::from("oversized_closure_argument_pack.fe"),
        &source,
    );
    let (top_mod, _) = db.top_mod(file);
    let rendered = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(
        rendered.contains("closure has too many parameters"),
        "{rendered}"
    );
    assert!(
        rendered.contains(&format!(
            "closure has {} parameters, but at most {MAX_CLOSURE_FIELDS} are supported",
            MAX_CLOSURE_FIELDS + 1
        )),
        "{rendered}"
    );
}
