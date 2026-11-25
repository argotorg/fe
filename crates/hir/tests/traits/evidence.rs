use camino::Utf8PathBuf;
use fe_hir::analysis::semantic::{
    GenericSubst, SemConstScalar, SemConstValue, SemanticInstance, eval_body_owner_const,
    identity_semantic_instance_key, root_semantic_instance_key, typed_body_template,
};
use fe_hir::analysis::ty::evidence::{Evidence, EvidenceId};
use fe_hir::analysis::ty::trait_resolution::{PredicateListId, TraitSolveCx};
use fe_hir::analysis::ty::ty_check::{BodyOwner, Callable, ConstRef, TypedBody, check_func_body};
use fe_hir::hir_def::{Expr, Func, ItemKind, Partial, TopLevelMod};
use fe_hir::test_db::HirAnalysisTestDb;
use num_traits::ToPrimitive;

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
    typed_body.assumptions().list(db)[index.index as usize].pretty_print(db, true)
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
    let Some(ConstRef::TraitConst { use_, evidence }) = typed_body.const_refs().next() else {
        panic!("missing trait constant use");
    };
    let without_assumptions = ConstRef::TraitConst {
        use_: use_.with_env(func.scope(), PredicateListId::empty_list(db)),
        evidence,
    };
    let with_assumptions = ConstRef::TraitConst {
        use_: use_.with_env(func.scope(), typed_body.assumptions()),
        evidence,
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

#[test]
fn bound_slots_are_owned_and_foreign_slot_zero_cannot_bind() {
    use fe_hir::analysis::ty::evidence::{BoundEvidence, BoundOwner, BoundSchemaId, EvidenceList};
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(Utf8PathBuf::from("bound_slot_ownership.fe"), SOURCE);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = func(&db, module, "bound");
    let (body, call) = method_call(&db, owner);
    let proof = call.evidence().unwrap();
    let Evidence::Assumption(slot) = proof.data(&db) else {
        panic!("bound proof")
    };
    assert_eq!(
        slot.schema.owner(&db),
        BoundOwner::Body(BodyOwner::func(&db, owner))
    );
    assert_eq!(slot.schema, body.bound_schema());
    assert_eq!(slot.index, 0);

    let other_owner = BodyOwner::func(&db, func(&db, module, "relay"));
    let adapter = BoundSchemaId::new(
        &db,
        BoundOwner::MethodBasis {
            body: owner,
            nominal: func(&db, module, "relay"),
            mode: None,
        },
        slot.schema.slots(&db),
    );
    let adapter_leaf = EvidenceId::new(&db, Evidence::Assumption(adapter.slot(&db, 0).unwrap()));
    assert_eq!(
        proof.bind(
            &db,
            BoundEvidence {
                schema: adapter,
                entries: EvidenceList::of(&db, vec![Some(adapter_leaf)]),
            }
        ),
        proof,
        "an adapter basis is not the body's signature"
    );
    // Deliberately identical goals and index, with a different declaration owner.
    let foreign = BoundSchemaId::new(&db, BoundOwner::Body(other_owner), slot.schema.slots(&db));
    let foreign_leaf = EvidenceId::new(&db, Evidence::Assumption(foreign.slot(&db, 0).unwrap()));
    let entries = EvidenceList::of(&db, vec![Some(foreign_leaf)]);
    assert_eq!(
        proof.bind(
            &db,
            BoundEvidence {
                schema: foreign,
                entries
            }
        ),
        proof
    );
    assert_eq!(
        proof.bind(
            &db,
            BoundEvidence {
                schema: slot.schema,
                entries
            }
        ),
        foreign_leaf
    );
}

#[test]
fn a_schema_view_may_extend_a_prefix_but_cannot_reorder_it() {
    use fe_hir::analysis::ty::evidence::{BoundEvidence, BoundOwner, BoundSchemaId, EvidenceList};
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(Utf8PathBuf::from("bound_slot_prefix.fe"), SOURCE);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::func(&db, func(&db, module, "pair"));
    let (body, _) = method_call(&db, func(&db, module, "bound"));
    let pair = &check_func_body(&db, func(&db, module, "pair")).1;
    let goals = pair.bound_schema().slots(&db).list(&db);
    assert_eq!(goals.len(), 2);
    let prefix = BoundSchemaId::new(
        &db,
        BoundOwner::Body(owner),
        PredicateListId::new(&db, vec![goals[0]]),
    );
    let leaf = EvidenceId::new(&db, Evidence::Assumption(prefix.slot(&db, 0).unwrap()));
    let replacement = EvidenceId::new(
        &db,
        Evidence::Assumption(body.bound_schema().slot(&db, 0).unwrap()),
    );
    let entries = EvidenceList::of(&db, vec![Some(replacement)]);
    assert_eq!(
        leaf.bind(
            &db,
            BoundEvidence {
                schema: pair.bound_schema(),
                entries
            }
        ),
        replacement
    );
    let reordered = BoundSchemaId::new(
        &db,
        BoundOwner::Body(owner),
        PredicateListId::new(&db, vec![goals[1], goals[0]]),
    );
    assert_eq!(
        leaf.bind(
            &db,
            BoundEvidence {
                schema: reordered,
                entries
            }
        ),
        leaf
    );
}

fn diagnostics(source: &str) -> Vec<String> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("evidence_regressions.fe".into(), source);
    let (module, _) = db.top_mod(file);
    fe_hir::analysis::initialize_analysis_pass()
        .run_on_module(&db, module)
        .into_iter()
        .map(|diagnostic| diagnostic.to_complete(&db).error_code.to_string())
        .collect()
}

#[test]
fn mut_trait_call_under_choice_retains_progressed_obligation() {
    let source = r#"
struct Inner { pub val: u256 }
struct Item { pub inner: Inner }
trait Dummy {}

trait Sub {
    fn sub(mut? self) -> mut? Self
}

impl Sub for bool {
    fn sub(self) -> ref Self { ref self }
}

impl<C> Sub for (u256, C) {
    fn sub(mut? self) -> mut? Self { mut? self }
}

impl Sub for (u256, u256) as ChosenRefOnly {
    fn sub(self) -> ref Self { ref self }
}

trait Access<A, B> {
    fn get(mut? self, _ a: A, _ b: B) -> mut? Inner
}

impl<A: Dummy, B: mut? Sub> Access<A, B> for Item {
    fn get(mut? self, _ a: A, _ b: B) -> mut? Inner { mut? self.inner }
}

fn make<T>() -> T { todo() }

pub fn mutate_non_mut(var item: own Item) uses (d: Dummy) {
    with (ChosenRefOnly) {
        let b = make()
        item.get(d, b).val = 1
        let _: (u256, u256) = b
    }
}
"#;
    let codes = diagnostics(source);
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
}

#[test]
fn written_type_and_alias_with_internal_choice_are_refused() {
    for snippet in [
        "type Sneaky = Array<{ with (Discount) { Item::value() } }>",
        "pub fn bypass_local() { let x: Array<{ with (Discount) { Item::value() } }> = Array {}\n let y: Array<70> = x }",
        "pub fn bypass_expr() { let x = Array<{ with (Discount) { Item::value() } }> {}\n let y: Array<70> = x }",
    ] {
        let source = format!(
            r#"
struct Item {{}}
trait Pick {{
    const fn value() -> u256
}}
impl Pick for Item {{
    const fn value() -> u256 {{ 100 }}
}}
impl Pick for Item as Discount {{
    const fn value() -> u256 {{ 70 }}
}}
struct Array<const N: u256> {{}}
{snippet}
"#
        );
        let codes = diagnostics(&source);
        assert!(
            codes.contains(&"8-0138".to_string()),
            "{snippet}: {codes:?}"
        );
    }

    let ordinary_control = r#"
struct Item {}
struct Other {}
struct Box<T> { value: T }
trait Pick {
    const fn value() -> u256
}
impl<T> Pick for Box<T> {
    const fn value() -> u256 { 100 }
}
impl Pick for Box<Item> as DiscountBox {
    const fn value() -> u256 { 70 }
}
const fn bound<T: Pick>() -> u256 { T::value() }
struct Array<const N: u256> {}
type BoxedOther = Array<{ with (DiscountBox) { bound<Box<Other>>() } }>
pub fn use_other() {
    let x: BoxedOther = Array {}
    let y: Array<{ with (DiscountBox) { bound<Box<Other>>() } }> = x
}
"#;
    assert!(diagnostics(ordinary_control).is_empty());
    let chosen_box = ordinary_control.replace("Box<Other>", "Box<Item>");
    let codes = diagnostics(&chosen_box);
    assert!(codes.contains(&"8-0138".to_string()), "{codes:?}");
}

#[test]
fn qualified_ordinary_with_operand_uses_surrounding_assumptions_and_choices() {
    let source = r#"
struct Item {}
struct Box<T> { pub value: T }

trait Pick {
    const fn value() -> u256
}

impl Pick for Item {
    const fn value() -> u256 { 100 }
}

impl Pick for Item as Discount {
    const fn value() -> u256 { 70 }
}

trait Total {
    const fn total() -> u256
}

impl<T: Pick> Total for Box<T> {
    const fn total() -> u256 { T::value() }
}

impl<T: Pick> Total for Box<T> as Markup {
    const fn total() -> u256 {
        with (<Box<T> as Total>) { Box<T>::total() + 1 }
    }
}

pub const fn chosen_markup() -> u256 {
    with (Markup) { Box<Item>::total() }
}

pub const fn chosen_discount_markup() -> u256 {
    with (Discount, Markup) { Box<Item>::total() }
}

pub const fn generic_escape<T: Pick>() -> u256 {
    with (Markup) {
        with (<Box<T> as Total>) { Box<T>::total() }
    }
}

pub const fn run_generic_escape() -> u256 {
    generic_escape<Item>()
}
"#;
    assert!(diagnostics(source).is_empty(), "{:?}", diagnostics(source));
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("qualified_ordinary_with.fe".into(), source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for (name, expected) in [
        ("chosen_markup", 101usize),
        ("chosen_discount_markup", 71usize),
        ("run_generic_escape", 100usize),
    ] {
        let owner = BodyOwner::func(&db, func(&db, module, name));
        let evaluated =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
        let value = evaluated.into_ready().expect("const evaluation succeeded");
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } = value.value(&db)
        else {
            panic!("expected scalar int");
        };
        assert_eq!(value.to_usize(), Some(expected), "{name}");
    }

    let missing_ordinary = r#"
struct Item {}
trait Pick {
    const fn value() -> u256
}
impl Pick for Item as Only {
    const fn value() -> u256 { 23 }
}
pub const fn bad_named_only() -> u256 {
    with (<Item as Pick>) { Item::value() }
}
"#;
    let codes = diagnostics(missing_ordinary);
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
    assert!(!codes.contains(&"8-0030".to_string()), "{codes:?}");

    let assumption_only = r#"
trait Pick {
    const fn value() -> u256
}
pub const fn bad_assumption<T: Pick>() -> u256 {
    with (<T as Pick>) { T::value() }
}
"#;
    let codes = diagnostics(assumption_only);
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
    assert!(!codes.contains(&"8-0030".to_string()), "{codes:?}");
}

#[test]
fn qualified_ordinary_with_operand_refuses_conditions_that_cannot_hold() {
    // The header `Box<T>: Total` matches, but its condition `T: Pick` holds
    // neither for `Other` nor for an unbounded `T`. The opening is refused
    // even though the block does not use it.
    let prelude = r#"
struct Item {}
struct Other {}
struct Box<T> { pub value: T }

trait Pick {
    const fn value() -> u256
}
impl Pick for Item {
    const fn value() -> u256 { 100 }
}

trait Total {
    const fn total() -> u256
}
impl<T: Pick> Total for Box<T> {
    const fn total() -> u256 { T::value() }
}
"#;
    for opening in [
        "pub const fn opened_unused() -> u256 { with (<Box<Other> as Total>) { 1 } }",
        "pub const fn opened<T>() -> u256 { with (<Box<T> as Total>) { 1 } }",
    ] {
        let codes = diagnostics(&format!("{prelude}{opening}\n"));
        assert!(
            codes.contains(&"6-0003".to_string()),
            "{opening}: {codes:?}"
        );
        assert!(
            !codes.contains(&"8-0030".to_string()),
            "{opening}: {codes:?}"
        );
    }
    // A condition that the surrounding bounds prove still opens.
    let bounded = format!(
        "{prelude}pub const fn opened<T: Pick>() -> u256 {{ with (<Box<T> as Total>) {{ 1 }} }}\n"
    );
    assert!(
        diagnostics(&bounded).is_empty(),
        "{:?}",
        diagnostics(&bounded)
    );
}

#[test]
fn assoc_const_path_uses_chosen_impls_and_non_param_assumptions() {
    let source = r#"
trait Pick {
    const fn value() -> u256
}

trait Count {
    const N: u256
}

struct Item {}
struct Box<T> {
    pub value: T
}

impl Pick for Item as Only {
    const fn value() -> u256 { 7 }
}

impl<T: Pick> Count for Box<T> {
    const N: u256 = 23
}

pub const fn qualified() -> u256 {
    with (Only) { <Box<Item> as Count>::N }
}

pub const fn unqualified() -> u256 {
    with (Only) { Box<Item>::N }
}

trait Step {
    const STEP: u256
    const NEXT: u256
}

impl<T: Pick> Step for Box<T> {
    const STEP: u256 = Box<T>::N
    const NEXT: u256 = Self::STEP + 1
}

pub const fn read_step() -> u256 {
    with (Only) { Box<Item>::NEXT }
}
"#;
    assert!(diagnostics(source).is_empty(), "{:?}", diagnostics(source));
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("assoc_const_chosen_impls.fe".into(), source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for (name, expected) in [
        ("qualified", 23usize),
        ("unqualified", 23usize),
        ("read_step", 24usize),
    ] {
        let owner = BodyOwner::func(&db, func(&db, module, name));
        let evaluated =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
        let value = evaluated.into_ready().expect("const evaluation succeeded");
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } = value.value(&db)
        else {
            panic!("expected scalar int");
        };
        assert_eq!(value.to_usize(), Some(expected), "{name}");
    }
}

#[test]
fn trait_const_pattern_uses_chosen_evidence_and_refuses_conflict() {
    let source = r#"
trait Price {
    const fn value() -> u256
}

trait Priced {
    const N: u256
}

struct Item {}
struct Box<T> {
    pub value: T
}

impl Price for Item as Discount {
    const fn value() -> u256 { 7 }
}

impl<T: Price> Priced for Box<T> {
    const N: u256 = 7
}

pub const fn matched_qualified() -> u256 {
    let x: u256 = 7
    with (Discount) {
        match x {
            <Box<Item> as Priced>::N => 1
            _ => 0
        }
    }
}

pub const fn matched_unqualified() -> u256 {
    let x: u256 = 7
    with (Discount) {
        match x {
            Box<Item>::N => 1
            _ => 0
        }
    }
}
"#;
    assert!(diagnostics(source).is_empty(), "{:?}", diagnostics(source));
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("trait_const_pattern_choice.fe".into(), source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for (name, expected) in [
        ("matched_qualified", 1usize),
        ("matched_unqualified", 1usize),
    ] {
        let owner = BodyOwner::func(&db, func(&db, module, name));
        let evaluated =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
        let value = evaluated.into_ready().expect("const evaluation succeeded");
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } = value.value(&db)
        else {
            panic!("expected scalar int");
        };
        assert_eq!(value.to_usize(), Some(expected), "{name}");
    }

    let conflict_source = r#"
trait Base {
    const N: u256
}
trait Left: Base {}
trait Right: Base {}
trait Join: Left + Right {
    const M: u256 = Self::N
}
struct Item {}
impl Base for Item {
    const N: u256 = 1
}
impl Base for Item as LeftBase {
    const N: u256 = 1
}
impl Base for Item as RightBase {
    const N: u256 = 1
}
impl Left for Item as LeftImpl {}
impl Right for Item as RightImpl {}
impl Join for Item {}
pub fn check(x: u256) -> u256 {
    with (LeftBase, LeftImpl, RightBase, RightImpl) {
        match x {
            Item::M => 1
            _ => 0
        }
    }
}
"#;
    let codes = diagnostics(conflict_source);
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
    assert!(codes.contains(&"8-0160".to_string()), "{codes:?}");

    // One list may not hold two implementations for the same goal, so the
    // same conflict is written with one list for each side.
    let nested_source = conflict_source.replace(
        "    with (LeftBase, LeftImpl, RightBase, RightImpl) {\n        match x {\n            Item::M => 1\n            _ => 0\n        }\n    }",
        "    with (LeftBase, LeftImpl) {\n        with (RightBase, RightImpl) {\n            match x {\n                Item::M => 1\n                _ => 0\n            }\n        }\n    }",
    );
    assert_ne!(
        nested_source, conflict_source,
        "the nested form must replace the list"
    );
    let codes = diagnostics(&nested_source);
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
    assert!(!codes.contains(&"8-0160".to_string()), "{codes:?}");
}

#[test]
fn named_impl_deferred_assoc_const_referencing_trait_const_is_accepted() {
    let source = r#"
struct Box<T> { pub value: T }
trait HasLen {
    const LEN: u256
}
trait Size {
    const CAP: u256
    const fn size() -> u256
}
impl<T: HasLen> Size for Box<T> {
    const CAP: u256 = {
        let _: u256 = 0
        T::LEN
    }
    const fn size() -> u256 { 100 }
}
impl<T: HasLen> Size for Box<T> as Named {
    const CAP: u256 = {
        let _: u256 = 0
        T::LEN
    }
    const fn size() -> u256 { 70 }
}
"#;
    assert!(diagnostics(source).is_empty(), "{:?}", diagnostics(source));

    let mismatch_source = r#"
struct Box<T> { pub value: T }
trait HasLen {
    const LEN: u256
}
trait Size {
    const CAP: u256
    const fn size() -> u256
}
impl<T: HasLen> Size for Box<T> {
    const CAP: u256 = {
        let _: u256 = 0
        T::LEN
    }
    const fn size() -> u256 { 100 }
}
impl<T: HasLen> Size for Box<T> as Named {
    const CAP: u256 = {
        let _: u256 = 0
        T::LEN + 1
    }
    const fn size() -> u256 { 70 }
}
"#;
    let codes = diagnostics(mismatch_source);
    assert!(codes.contains(&"5-0005".to_string()), "{codes:?}");
}
