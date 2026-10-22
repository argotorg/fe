use camino::Utf8PathBuf;
use fe_hir::analysis::semantic::{
    SemanticInstance, identity_semantic_instance_key, root_semantic_instance_key,
    typed_body_template,
};
use fe_hir::analysis::ty::evidence::{Evidence, EvidenceId};
use fe_hir::analysis::ty::trait_resolution::{PredicateListId, TraitSolveCx};
use fe_hir::analysis::ty::ty_check::{BodyOwner, Callable, ConstRef, TypedBody, check_func_body};
use fe_hir::hir_def::{Expr, Func, ItemKind, Partial, TopLevelMod};
use fe_hir::test_db::HirAnalysisTestDb;

const SOURCE: &str = r#"
struct Wrap<T> {
    t: T,
}

impl<T: Copy> Copy for Wrap<T> {}

trait Grow {
    fn grow(self) -> u256
}

impl<T: Copy> Grow for T {
    fn grow(self) -> u256 {
        1
    }
}

fn wrapped<T: Copy>(_ x: T) -> u256 {
    (Wrap { t: x }).grow()
}

fn bound<T: Grow>(_ x: T) -> u256 {
    x.grow()
}

fn outer<T: Copy>(_ x: T) -> u256 {
    bound(Wrap { t: x })
}

fn root() -> u256 {
    outer(1 as u256)
}

trait HasD {
    const D: u256
}

impl<T: Copy> HasD for Wrap<T> {
    const D: u256 = 3
}

trait HasE {
    const E: u256 = 2
}

impl HasE for u256 {}

fn root_d() -> u256 {
    Wrap<u256>::D
}

fn generic_e<T: HasE>(_ x: T) -> u256 {
    T::E
}

fn root_e() -> u256 {
    generic_e(1 as u256)
}

trait HasK {
    const K: u256
}

impl<T: Copy> HasK for T {
    const K: u256 = 1
}

fn wrapped_k<T: Copy>(_ x: T) -> u256 {
    Wrap<T>::K
}

fn bound_k<T: HasK>(_ x: T) -> u256 {
    T::K
}

fn pair<T: Grow, U: Grow>(_ x: T, _ y: U) -> u256 {
    x.grow() + y.grow()
}

fn half<U: Grow>(_ y: U) -> u256 {
    pair(1 as u256, y)
}

fn relay<U: Grow + Copy>(_ x: U) -> u256 {
    bound(x)
}
"#;

fn func<'db>(db: &'db HirAnalysisTestDb, top_mod: TopLevelMod<'db>, name: &str) -> Func<'db> {
    top_mod
        .children_non_nested(db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|func_name| func_name.data(db) == name) =>
            {
                Some(func)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"))
}

/// `func`'s checked body and its only method call.
fn method_call<'db>(
    db: &'db HirAnalysisTestDb,
    func: Func<'db>,
) -> (TypedBody<'db>, Callable<'db>) {
    let typed_body = check_func_body(db, func).1.clone();
    let body = func.body(db).expect("missing body");
    let call = body
        .exprs(db)
        .keys()
        .find(|expr| matches!(expr.data(db, body), Partial::Present(Expr::MethodCall(..))))
        .expect("missing method call");
    let callable = typed_body
        .callable_expr(call)
        .expect("missing callable")
        .clone();
    (typed_body, callable)
}

/// The checked body's assumption a proof leaf names.
fn assumption<'db>(
    db: &'db HirAnalysisTestDb,
    typed_body: &TypedBody<'db>,
    evidence: EvidenceId<'db>,
) -> String {
    let Evidence::Assumption(index) = evidence.data(db) else {
        panic!("expected an assumption: {:?}", evidence.data(db));
    };
    typed_body.assumptions().list(db)[*index as usize].pretty_print(db, true)
}

/// The proofs of an implementation's conditions.
fn conds<'db>(db: &'db HirAnalysisTestDb, evidence: EvidenceId<'db>) -> &'db [EvidenceId<'db>] {
    let Evidence::Impl { conds, .. } = evidence.data(db) else {
        panic!("expected an implementation: {:?}", evidence.data(db));
    };
    conds
}

#[test]
fn a_proof_names_the_body_assumptions_it_uses() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("a_proof_names_the_body_assumptions_it_uses.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `Wrap<T>: Grow` holds by the blanket implementation, whose condition
    // `Wrap<T>: Copy` holds by `Copy for Wrap<T>`, whose condition is the
    // body's own bound.
    let (typed_body, callable) = method_call(&db, func(&db, top_mod, "wrapped"));
    let evidence = callable.evidence().expect("the call's goal is proved");
    let [wrap_copy] = conds(&db, evidence) else {
        panic!("the blanket implementation has one condition");
    };
    let [t_copy] = conds(&db, *wrap_copy) else {
        panic!("`Copy for Wrap<T>` has one condition");
    };
    assert_eq!(assumption(&db, &typed_body, *t_copy), "T: Copy");

    let (typed_body, callable) = method_call(&db, func(&db, top_mod, "bound"));
    let evidence = callable.evidence().expect("the call's goal is proved");
    assert_eq!(assumption(&db, &typed_body, evidence), "T: Grow");
}

/// The body `func`'s only method call runs, selected in a context with no
/// assumptions, and whether selecting with the body's assumptions reaches a
/// body.
fn grow_call_bodies<'db>(db: &'db HirAnalysisTestDb, func: Func<'db>) -> (Option<Func<'db>>, bool) {
    let (typed_body, callable) = method_call(db, func);
    let without_assumptions = callable
        .selected_method(
            db,
            TraitSolveCx::new(db, func.scope()).with_assumptions(PredicateListId::empty_list(db)),
        )
        .and_then(|method| method.body());
    let with_assumptions = callable
        .selected_method(
            db,
            TraitSolveCx::new(db, func.scope()).with_assumptions(typed_body.assumptions()),
        )
        .is_some_and(|method| method.body().is_some());
    (without_assumptions, with_assumptions)
}

/// Whether `func`'s only trait-constant use reaches a defining body where
/// nothing is assumed, and where the body's assumptions are.
fn k_use_bodies(db: &HirAnalysisTestDb, func: Func<'_>) -> (bool, bool) {
    let typed_body = check_func_body(db, func).1.clone();
    let Some(ConstRef::TraitConst {
        use_,
        evidence,
        bound_evidence,
    }) = typed_body.const_refs().next()
    else {
        panic!("missing trait constant use");
    };
    let without_assumptions = ConstRef::TraitConst {
        use_: use_.with_env(func.scope(), PredicateListId::empty_list(db)),
        evidence,
        bound_evidence,
    };
    let with_assumptions = ConstRef::TraitConst {
        use_: use_.with_env(func.scope(), typed_body.assumptions()),
        evidence,
        bound_evidence,
    };
    (
        without_assumptions.trait_const_body(db).is_some(),
        with_assumptions.trait_const_body(db).is_some(),
    )
}

#[test]
fn checker_selection_answers_a_call_without_the_body_context() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("checker_selection_answers_a_call_without_the_body_context.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `Wrap<T>: Grow` holds only because `T: Copy` is assumed. The checker
    // selected the blanket implementation, so the call reaches its body even
    // where nothing is assumed.
    let (body, _) = grow_call_bodies(&db, func(&db, top_mod, "wrapped"));
    let body = body.expect("the recorded blanket implementation answers the call");
    assert!(matches!(
        body.scope().parent_item(&db),
        Some(ItemKind::ImplTrait(_))
    ));
}

#[test]
fn bound_dispatch_is_selected_where_the_body_is_instantiated() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("bound_dispatch_is_selected_where_the_body_is_instantiated.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `T: Grow` holds by the body's own bound: no implementation is chosen
    // until `T` is known.
    let (without_assumptions, with_assumptions) =
        grow_call_bodies(&db, func(&db, top_mod, "bound"));
    assert_eq!(without_assumptions, None);
    assert!(!with_assumptions);
}

#[test]
fn checker_selection_answers_a_constant_without_the_body_context() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("checker_selection_answers_a_constant_without_the_body_context.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // As for a call: the blanket implementation the checker selected
    // defines `Wrap<T>::K` even where `T: Copy` is not assumed.
    assert_eq!(
        k_use_bodies(&db, func(&db, top_mod, "wrapped_k")),
        (true, true)
    );
    // `T::K` holds by the body's bound: it has no defining body until `T` is
    // known.
    assert_eq!(
        k_use_bodies(&db, func(&db, top_mod, "bound_k")),
        (false, false)
    );
}

#[test]
fn an_instance_receives_its_callers_proofs_of_its_bounds() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("an_instance_receives_its_callers_proofs_of_its_bounds.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `outer` proves `bound`'s `T: Grow` for `Wrap<T>` from its own
    // `T: Copy`: the call records that proof.
    let outer_fn = func(&db, top_mod, "outer");
    let outer_body = check_func_body(&db, outer_fn).1.clone();
    let outer_call = outer_fn
        .body(&db)
        .and_then(|body| {
            body.exprs(&db)
                .keys()
                .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::Call(..))))
        })
        .expect("missing call");
    let recorded = outer_body
        .callable_expr(outer_call)
        .and_then(Callable::bound_evidence)
        .and_then(|bound| bound.entries(&db).iter().flatten().next().copied())
        .expect("the call records its proof of `T: Grow`");
    let [wrap_copy] = conds(&db, recorded) else {
        panic!("the blanket implementation has one condition");
    };
    let [t_copy] = conds(&db, *wrap_copy) else {
        panic!("`Copy for Wrap<T>` has one condition");
    };
    assert_eq!(assumption(&db, &outer_body, *t_copy), "T: Copy");

    // In `outer<u256>`, which `root` makes, that proof names `u256: Copy`;
    // `bound<Wrap<u256>>` receives it, so its call `x.grow()`, checked
    // against the bound, carries the blanket implementation with its
    // conditions proved.
    let root = SemanticInstance::new(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(func(&db, top_mod, "root"), None))
            .expect("missing root key"),
    );
    let outer = root
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(outer_fn, None))
        .map(|callee| SemanticInstance::new(&db, callee.key))
        .expect("`root` calls `outer`");
    let bound_fn = func(&db, top_mod, "bound");
    let bound = outer
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(bound_fn, None))
        .expect("`outer` calls `bound`");
    let body = bound_fn.body(&db).expect("missing body");
    let call = body
        .exprs(&db)
        .keys()
        .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::MethodCall(..))))
        .expect("missing method call");
    let evidence = bound
        .key
        .typed_body(&db)
        .callable_expr(call)
        .and_then(Callable::evidence)
        .expect("the call's goal is proved");
    let [wrap_copy] = conds(&db, evidence) else {
        panic!("the blanket implementation has one condition");
    };
    let [u256_copy] = conds(&db, *wrap_copy) else {
        panic!("`Copy for Wrap<T>` has one condition");
    };
    assert!(conds(&db, *u256_copy).is_empty());
}

/// Whether the body defining the only trait-constant use in `body` receives
/// a closed proof of each assumption it is checked under.
fn const_body_evidence(db: &HirAnalysisTestDb, body: &TypedBody<'_>) -> Vec<bool> {
    let const_ref = body
        .const_refs()
        .next()
        .expect("missing trait constant use");
    let (_, _, _, evidence) = const_ref
        .trait_const_body(db)
        .expect("the use has a defining body");
    evidence
        .entries(db)
        .iter()
        .map(|entry| entry.is_some_and(|evidence| evidence.is_closed(db)))
        .collect()
}

#[test]
fn a_constant_body_receives_proofs_of_its_assumptions() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("a_constant_body_receives_proofs_of_its_assumptions.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `Wrap<u256>::D` is the implementation's value, checked under its
    // condition `T: Copy`: the selection's proof of the condition supplies
    // it.
    let root_d = typed_body_template(&db, BodyOwner::Func(func(&db, top_mod, "root_d"), None));
    assert_eq!(const_body_evidence(&db, &root_d.body), vec![true]);

    // `T::E` in `generic_e` is the trait's default once `T` is known: in
    // `generic_e<u256>`, which `root_e` makes, the use's proof of
    // `Self: HasE`, the instance's own `u256: HasE`, supplies it.
    let root_e = SemanticInstance::new(
        &db,
        root_semantic_instance_key(&db, BodyOwner::Func(func(&db, top_mod, "root_e"), None))
            .expect("missing root key"),
    );
    let generic_e_fn = func(&db, top_mod, "generic_e");
    let generic_e = root_e
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(generic_e_fn, None))
        .expect("`root_e` calls `generic_e`");
    assert!(const_body_evidence(&db, generic_e.key.typed_body(&db)).contains(&true));
}

#[test]
fn an_instance_proves_the_bounds_its_arguments_close() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("an_instance_proves_the_bounds_its_arguments_close.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `half` leaves `pair`'s closed bound `u256: Grow` to the instance and
    // proves `U: Grow` by its own bound. In `pair<u256, U>`, which `half`'s
    // body makes, `x.grow()` so carries the blanket implementation, and
    // `y.grow()` still holds by `half`'s bound.
    let half = SemanticInstance::new(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func(&db, top_mod, "half"), None)),
    );
    let pair_fn = func(&db, top_mod, "pair");
    let pair = half
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(pair_fn, None))
        .expect("`half` calls `pair`");
    let body = pair_fn.body(&db).expect("missing body");
    let mut proofs: Vec<_> = body
        .exprs(&db)
        .keys()
        .filter(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::MethodCall(..))))
        .map(|call| {
            let evidence = pair
                .key
                .typed_body(&db)
                .callable_expr(call)
                .and_then(Callable::evidence)
                .expect("the call's goal is proved");
            match evidence.data(&db) {
                Evidence::Impl { .. } if evidence.is_closed(&db) => "closed implementation",
                Evidence::Impl { .. } => "implementation",
                Evidence::Assumption(_) => "assumption",
                Evidence::DeferredSolCodec { .. } => "deferred codec",
            }
        })
        .collect();
    proofs.sort();
    assert_eq!(proofs, ["assumption", "closed implementation"]);
}

#[test]
fn a_generic_instance_dispatches_through_the_bound_its_caller_proved() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("a_generic_instance_dispatches_through_the_bound_its_caller_proved.fe"),
        SOURCE,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    // `relay` proves `bound`'s `T: Grow` for `U` by its own bound. In
    // `bound<U>`, which `relay`'s body makes, `x.grow()` is dispatched
    // through that bound, although the blanket implementation also applies
    // where `U: Copy` is assumed: no body is chosen until `U` is known.
    let relay_fn = func(&db, top_mod, "relay");
    let relay = SemanticInstance::new(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(relay_fn, None)),
    );
    let bound_fn = func(&db, top_mod, "bound");
    let bound = relay
        .callees(&db)
        .iter()
        .find(|callee| callee.key.owner(&db) == BodyOwner::Func(bound_fn, None))
        .expect("`relay` calls `bound`");
    let body = bound_fn.body(&db).expect("missing body");
    let call = body
        .exprs(&db)
        .keys()
        .find(|expr| matches!(expr.data(&db, body), Partial::Present(Expr::MethodCall(..))))
        .expect("missing method call");
    let relay_assumptions = check_func_body(&db, relay_fn).1.assumptions();
    let method = bound
        .key
        .typed_body(&db)
        .callable_expr(call)
        .expect("missing callable")
        .selected_method(
            &db,
            TraitSolveCx::new(&db, relay_fn.scope()).with_assumptions(relay_assumptions),
        )
        .expect("the bound answers the call");
    assert_eq!(method.body(), None);
}
