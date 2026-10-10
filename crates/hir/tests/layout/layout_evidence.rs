use std::collections::HashSet;

use crate::layout_test_support::{parse_module, parse_ok};
use camino::Utf8PathBuf;
use cranelift_entity::EntityRef;
use fe_hir::{
    analysis::{
        initialize_analysis_pass,
        semantic::{
            EffectProviderSubst, GenericSubst, ImplEnv, LayoutEvidenceBase, LayoutEvidenceBody,
            LayoutEvidenceComponentValue, LayoutEvidenceError, LayoutEvidenceExpr,
            LayoutEvidenceOperand, LayoutEvidenceVerifyError, NExpr, NStatementKind,
            NormalizedArtifacts, SExpr, SStmtKind, SemanticInstanceKey,
            collect_layout_evidence_diagnostic_vouchers, get_or_build_semantic_instance,
            identity_semantic_instance_key, layout_evidence_body, normalize_runtime_semantic_body,
            normalized::{NLayoutLocals, NStatementId},
            verify_layout_evidence_body as verify_normalized_layout_evidence_body,
            verify_layout_evidence_runtime_compatibility as verify_normalized_layout_evidence_runtime_compatibility,
        },
        ty::{
            CallableLayoutParamPort, CallableLayoutPort, LayoutBundleComponentId,
            LayoutBundleComponentTransport, LayoutBundleSchemaError, LayoutBundleUnrepresentable,
            LayoutEvidencePathStep, LayoutViewAlias, const_ty::CallableInputLayoutHoleOrigin,
            ty_check::BodyOwner,
        },
    },
    core::semantic::ContractLayoutError,
    hir_def::{CallableDef, IdentId, ItemKind, TopLevelMod},
    test_db::{HirAnalysisTestDb, find_contract, find_func},
};

fn verify_layout_evidence_body<'db>(
    db: &'db HirAnalysisTestDb,
    normalized: &NormalizedArtifacts<'db>,
    evidence: &LayoutEvidenceBody<'db>,
) -> Result<(), LayoutEvidenceVerifyError> {
    verify_normalized_layout_evidence_body(
        db,
        &normalized.body,
        &normalized.layout_plan,
        normalized.body.owner.body(db),
        evidence,
    )
}

fn verify_layout_evidence_runtime_compatibility<'db>(
    db: &'db HirAnalysisTestDb,
    normalized: &NormalizedArtifacts<'db>,
    evidence: &LayoutEvidenceBody<'db>,
) -> Result<(), LayoutEvidenceVerifyError> {
    verify_normalized_layout_evidence_runtime_compatibility(
        db,
        &normalized.body,
        &normalized.layout_plan,
        normalized.body.owner.body(db),
        evidence,
    )
}

fn assert_layoutizes(name: &str, src: &str) {
    assert_layoutizes_in(name, src, false);
}

fn assert_trusted_layoutizes(name: &str, src: &str) {
    assert_layoutizes_in(name, src, true);
}

/// Reads a fixture from `test_files/layout_evidence`, returning its real path
/// and its text.
fn layout_evidence_fixture(name: &str) -> (Utf8PathBuf, String) {
    let path = Utf8PathBuf::from(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/test_files/layout_evidence"
    ))
    .join(name);
    let text = std::fs::read_to_string(&path).expect("fixture should be readable");
    (path, text)
}

#[test]
fn finite_nested_options_through_named_fields_have_finite_layouts() {
    let (path, text) = layout_evidence_fixture("finite_nested_options.fe");
    assert_layoutizes(path.as_str(), &text);
}

#[test]
fn expanding_structural_type_arguments_do_not_expand_layouts_forever() {
    let (path, text) = layout_evidence_fixture("expanding_structural_arguments.fe");
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(path, &text);
    let (top_mod, _) = db.top_mod(file);
    assert!(reports_non_regular_cycle(&db, top_mod, "inspect"));
}

/// Whether the layout schema of the first input of function `name` reports a
/// non-regular view cycle.
fn reports_non_regular_cycle<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
    name: &str,
) -> bool {
    let inspect = get_or_build_semantic_instance(
        db,
        identity_semantic_instance_key(db, BodyOwner::Func(find_func(db, top_mod, name))),
    );
    let signature = inspect.key(db).layout_bundle_signature(db);
    matches!(
        signature.inputs[0].interface.schema.unrepresentable,
        Some(LayoutBundleUnrepresentable::NonRegularViewCycle { .. })
    )
}

/// Growth through an inserted wrapper, directly or through a second type, is
/// still caught by the growth guard. The check runs on a worker thread so a
/// layout walk that never terminates fails the test instead of hanging it.
#[test]
fn expanding_layouts_through_wrappers_are_rejected_in_bounded_time() {
    let (path, text) = layout_evidence_fixture("expanding_through_wrappers.fe");
    let (sender, receiver) = std::sync::mpsc::channel();
    std::thread::spawn(move || {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(path, &text);
        let (top_mod, _) = db.top_mod(file);
        for name in ["inspect_wrapped", "inspect_ping"] {
            let rejected = reports_non_regular_cycle(&db, top_mod, name);
            sender
                .send((name, rejected))
                .expect("test thread is waiting");
        }
    });
    for _ in 0..2 {
        let (name, rejected) = match receiver.recv_timeout(std::time::Duration::from_secs(60)) {
            Ok(result) => result,
            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
                panic!("layout growth check did not finish within 60 seconds")
            }
            Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
                panic!("layout growth check panicked")
            }
        };
        assert!(
            rejected,
            "`{name}` should be reported as a non-regular cycle"
        );
    }
}

fn assert_layoutizes_in(name: &str, src: &str, std_module: bool) {
    let mut db = HirAnalysisTestDb::default();
    let path = Utf8PathBuf::from(name);
    let file = if std_module {
        db.new_trusted_effect_handle_module(path, src)
    } else {
        db.new_stand_alone(path, src)
    };
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    for item in top_mod.all_items(&db) {
        match item {
            ItemKind::Func(func) if func.body(&db).is_some() => {
                let instance = get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                );
                layout_evidence_body(&db, instance).unwrap_or_else(|error| {
                    let name = func
                        .name(&db)
                        .to_opt()
                        .map_or("<unnamed>", |name| name.data(&db));
                    panic!("failed to layoutize {name}: {error:?}")
                });
            }
            ItemKind::Contract(contract) => {
                let init = get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(
                        &db,
                        BodyOwner::ContractInit {
                            contract: *contract,
                        },
                    ),
                );
                layout_evidence_body(&db, init)
                    .unwrap_or_else(|error| panic!("failed to layoutize init: {error:?}"));
                for (recv_idx, recv) in contract.recvs(&db).data(&db).iter().enumerate() {
                    for arm_idx in 0..recv.arms.data(&db).len() {
                        let arm = get_or_build_semantic_instance(
                            &db,
                            identity_semantic_instance_key(
                                &db,
                                BodyOwner::ContractRecvArm {
                                    contract: *contract,
                                    recv_idx: recv_idx as u32,
                                    arm_idx: arm_idx as u32,
                                },
                            ),
                        );
                        layout_evidence_body(&db, arm).unwrap_or_else(|error| {
                            panic!(
                                "failed to layoutize {name} recv {recv_idx}/{arm_idx}: {error:?}"
                            )
                        });
                    }
                }
            }
            ItemKind::Const(_)
            | ItemKind::Body(_)
            | ItemKind::Func(_)
            | ItemKind::Mod(_)
            | ItemKind::Struct(_)
            | ItemKind::Enum(_)
            | ItemKind::Trait(_)
            | ItemKind::Impl(_)
            | ItemKind::ImplTrait(_)
            | ItemKind::TypeAlias(_)
            | ItemKind::StaticAssert(_)
            | ItemKind::Use(_)
            | ItemKind::TopMod(_) => {}
        }
    }
}

#[test]
fn runtime_const_uses_bind_one_explicit_layout_input_port() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

fn root<const ROOT: u256>(map: StorageMap<u256, u256, ROOT>) -> u256 {
    ROOT
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "root"))),
    );
    let normalized = normalize_runtime_semantic_body(&db, instance).expect("normalization failed");
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    assert!(std::ptr::eq(
        evidence,
        layout_evidence_body(&db, instance).expect("cached layoutization failed")
    ));
    let bindings = evidence
        .constant_bindings
        .iter()
        .flatten()
        .collect::<Vec<_>>();
    let [binding] = bindings.as_slice() else {
        panic!("root use must have one explicit layout binding")
    };
    assert!(matches!(
        binding.source,
        CallableLayoutParamPort::Input(CallableLayoutPort {
            origin: CallableInputLayoutHoleOrigin::ValueParam(0),
            ..
        })
    ));
    assert!(matches!(binding.value, LayoutEvidenceOperand::Local(_)));
    verify_layout_evidence_body(&db, &normalized, evidence).expect("evidence must verify");
    verify_layout_evidence_runtime_compatibility(&db, &normalized, evidence)
        .expect("evidence must match the runtime body");
}

#[test]
fn derived_layout_values_do_not_reify_their_const_dependencies() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

fn original<const ROOT: u256>(value: Rooted<{ ROOT + 1 }>) -> u256 {
    ROOT
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "original"))),
    );
    assert!(matches!(
        layout_evidence_body(&db, instance),
        Err(LayoutEvidenceError::MissingConstBinding { .. })
    ));
}

#[test]
fn equal_specialized_args_preserve_formal_const_binding_identity() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

struct Independent<const FIRST: u256, const SECOND: u256> {
    first: StorageMap<u256, u256, FIRST>,
    second: StorageMap<u256, u256, SECOND>,
}

fn first<const FIRST: u256, const SECOND: u256>(
    value: Independent<FIRST, SECOND>,
) -> u256 {
    FIRST
}
"#,
    );
    let func = find_func(&db, top_mod, "first");
    let params = CallableDef::Func(func).params(&db);
    let key = SemanticInstanceKey::new(
        &db,
        BodyOwner::Func(func),
        GenericSubst::for_owner(&db, func.into(), vec![params[1], params[1]]),
        EffectProviderSubst::empty(&db),
        ImplEnv::empty(&db, func.scope()),
    );
    let instance = get_or_build_semantic_instance(&db, key);
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    let bindings = evidence
        .constant_bindings
        .iter()
        .flatten()
        .collect::<Vec<_>>();
    let [binding] = bindings.as_slice() else {
        panic!("FIRST must bind exactly one formal layout component")
    };
    assert_eq!(
        binding.param, params[0],
        "the binding must retain the declaration-level FIRST parameter"
    );
}

#[test]
fn equal_specialized_args_preserve_output_witness_identity() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

fn convert<const FIRST: u256, const SECOND: u256>(
    value: Rooted<SECOND>,
) -> Rooted<FIRST> {
    Rooted {}
}

fn forward<const ROOT: u256>(value: Rooted<ROOT>) -> Rooted<ROOT> {
    convert(value: value)
}
"#,
    );

    let convert = find_func(&db, top_mod, "convert");
    let params = CallableDef::Func(convert).params(&db);
    let convert = get_or_build_semantic_instance(
        &db,
        SemanticInstanceKey::new(
            &db,
            BodyOwner::Func(convert),
            GenericSubst::for_owner(&db, convert.into(), vec![params[1], params[1]]),
            EffectProviderSubst::empty(&db),
            ImplEnv::empty(&db, convert.scope()),
        ),
    );
    let signature = convert.key(&db).layout_bundle_signature(&db);
    assert_eq!(
        signature.output_witnesses.schema.components.len(),
        1,
        "equal instantiated values must not make independent formal ports interchangeable"
    );
    let evidence = layout_evidence_body(&db, convert).expect("layoutization failed");
    assert_eq!(evidence.params.len(), 2);

    let forward = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "forward"))),
    );
    let evidence = layout_evidence_body(&db, forward).expect("forward layoutization failed");
    let call = evidence
        .statements
        .iter()
        .find_map(|statement| statement.call.as_ref())
        .expect("missing call evidence");
    assert_eq!(call.args.len(), 2);
    assert!(matches!(
        call.args[1].target,
        CallableLayoutParamPort::OutputWitness(_)
    ));
}

#[test]
fn equal_specialized_args_preserve_call_binding_identity_without_output_context() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

fn convert<const FIRST: u256, const SECOND: u256>(
    value: Rooted<SECOND>,
) -> Rooted<FIRST> {
    Rooted {}
}

fn discard<const FIRST: u256, const SECOND: u256>(
    first: Rooted<FIRST>,
    second: Rooted<SECOND>,
) {
    let value: Rooted<FIRST> = convert(value: second)
}
"#,
    );

    let discard = find_func(&db, top_mod, "discard");
    let params = CallableDef::Func(discard).params(&db);
    let instance = get_or_build_semantic_instance(
        &db,
        SemanticInstanceKey::new(
            &db,
            BodyOwner::Func(discard),
            GenericSubst::for_owner(&db, discard.into(), vec![params[1], params[1]]),
            EffectProviderSubst::empty(&db),
            ImplEnv::empty(&db, discard.scope()),
        ),
    );
    layout_evidence_body(&db, instance).expect("layoutization must retain formal call identity");
}

#[test]
fn inherited_impl_const_params_preserve_formal_layout_identity() {
    assert_layoutizes(
        "inherited_impl_const_params_preserve_formal_layout_identity.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Rooted<ROOT> {
    fn fresh() -> Self { Self {} }
}

struct Phantom<const ROOT: u256 = _> {}

impl<const ROOT: u256> Phantom<ROOT> {
    fn discard(self) {
        let value: Rooted<ROOT> = Rooted::fresh()
    }
}
"#,
    );
}

#[test]
fn contextual_method_call_outputs_preserve_caller_layout_identity() {
    assert_layoutizes(
        "contextual_method_call_outputs_preserve_caller_layout_identity.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

struct Factory {}
impl Factory {
    fn fresh<const ROOT: u256>(self) -> Rooted<ROOT> { Rooted {} }
}

fn make<const ROOT: u256>(factory: Factory, anchor: Rooted<ROOT>) {
    let value: Rooted<ROOT> = factory.fresh()
}
"#,
    );
}

#[test]
fn abstract_layout_expressions_without_concrete_evidence_are_rejected() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

fn fresh_offset<const ROOT: u256>() -> Rooted<{ ROOT + 1 }> {
    Rooted {}
}

fn make<const ROOT: u256>(value: Rooted<ROOT>) {
    let offset: Rooted<{ ROOT + 1 }> = fresh_offset()
}
"#,
    );
    let diagnostics = collect_layout_evidence_diagnostic_vouchers(&db, top_mod);
    let rendered = diagnostics
        .iter()
        .map(|diagnostic| format!("{:?}", diagnostic.to_complete(&db)))
        .collect::<Vec<_>>()
        .join("\n");
    assert_eq!(diagnostics.len(), 1, "{rendered}");
    assert!(rendered.contains("cannot determine inferred layout in `make`"));
    assert!(rendered.contains("no runtime layout root is available"));
}

#[test]
fn arrays_reaching_roots_through_recursive_provider_targets_are_rejected() {
    for target in ["([Loop; 2], Rooted)", "(Rooted, [Loop; 2])"] {
        let source = format!(
            r#"
use core::effect_ref::{{AddressSpace, EffectHandle}}

struct Rooted<const ROOT: u256 = _> {{}}
struct Loop {{ raw: u256 }}
impl EffectHandle for Loop {{
    type Target = {target}
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 {{ self.raw }}
}}

struct Plain {{ raw: u256 }}
impl EffectHandle for Plain {{
    type Target = ([Plain; 2], u256)
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 {{ self.raw }}
}}

fn take(l: Loop) {{}}

fn take_plain(p: Plain) {{}}
"#
        );
        parse_ok!(trusted db, top_mod, &source);
        let mut messages = collect_layout_evidence_diagnostic_vouchers(&db, top_mod)
            .iter()
            .map(|diagnostic| diagnostic.to_complete(&db).message)
            .collect::<Vec<_>>();
        messages.sort();
        // `Loop::raw` receives a `Loop`; root-free `Plain` stays valid.
        assert_eq!(
            messages,
            [
                "array of layout-root values in `raw`",
                "array of layout-root values in `take`",
            ],
            "{target}"
        );
    }
}

#[test]
fn arrays_reaching_roots_through_chained_provider_back_edges_are_rejected() {
    // Expanding `Outer`, the array closes a back-edge to `Inner`, which
    // reaches `Outer`'s roots only through its own back-edge to `Outer`,
    // directly or by way of `Mid`.
    for target in [
        "(Inner, Rooted)",
        "(Rooted, Inner)",
        "(Mid, Rooted)",
        "(Rooted, Mid)",
    ] {
        let source = format!(
            r#"
use core::effect_ref::{{AddressSpace, EffectHandle}}

struct Rooted<const ROOT: u256 = _> {{}}
struct Outer {{ raw: u256 }}
impl EffectHandle for Outer {{
    type Target = {target}
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 {{ self.raw }}
}}
struct Inner {{ raw: u256 }}
impl EffectHandle for Inner {{
    type Target = ([Inner; 2], Outer)
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 {{ self.raw }}
}}
struct Mid {{ raw: u256 }}
impl EffectHandle for Mid {{
    type Target = (Inner, u256)
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 {{ self.raw }}
}}

struct PlainOuter {{ raw: u256 }}
impl EffectHandle for PlainOuter {{
    type Target = (PlainInner, u256)
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 {{ self.raw }}
}}
struct PlainInner {{ raw: u256 }}
impl EffectHandle for PlainInner {{
    type Target = ([PlainInner; 2], PlainOuter)
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 {{ self.raw }}
}}

fn take_outer(o: Outer) {{}}

fn take_inner(i: Inner) {{}}

fn take_mid(m: Mid) {{}}

fn take_plain(p: PlainOuter) {{}}
"#
        );
        parse_ok!(trusted db, top_mod, &source);
        let mut messages = collect_layout_evidence_diagnostic_vouchers(&db, top_mod)
            .iter()
            .map(|diagnostic| diagnostic.to_complete(&db).message)
            .collect::<Vec<_>>();
        messages.sort();
        // Every `raw` method receives a root-reaching handle; the root-free
        // chain stays valid.
        assert_eq!(
            messages,
            [
                "array of layout-root values in `raw`",
                "array of layout-root values in `raw`",
                "array of layout-root values in `raw`",
                "array of layout-root values in `take_inner`",
                "array of layout-root values in `take_mid`",
                "array of layout-root values in `take_outer`",
            ],
            "{target}"
        );
    }
}

#[test]
fn arrays_of_layout_root_values_are_rejected() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

fn inferred(values: [Rooted; 2]) {}

fn explicit(values: [Rooted<7>; 2]) {}

fn generic<const ROOT: u256>(values: [Rooted<ROOT>; 2]) {}

fn pair<T: Copy>(_ value: T) -> [T; 2] {
    [value, value]
}

fn hidden<T: Copy>(_ value: T) {
    let values = [value, value]
}

fn caller<const ROOT: u256>(value: Rooted<ROOT>) {
    let values = pair(value)
    hidden(value)
}

fn empty(values: [Rooted; 0]) {}

fn plain(values: [u256; 2]) {}

trait Api {
    fn declared(values: [Rooted; 2])
    fn plain_declared(values: [u256; 2])
}

extern {
    fn external() -> [Rooted<7>; 2]
}
"#,
    );
    let mut messages = collect_layout_evidence_diagnostic_vouchers(&db, top_mod)
        .iter()
        .map(|diagnostic| diagnostic.to_complete(&db).message)
        .collect::<Vec<_>>();
    messages.sort();
    assert_eq!(
        messages,
        [
            "array of layout-root values in `caller`",
            "array of layout-root values in `declared`",
            "array of layout-root values in `empty`",
            "array of layout-root values in `explicit`",
            "array of layout-root values in `external`",
            "array of layout-root values in `generic`",
            "array of layout-root values in `inferred`",
        ]
    );

    // A generic body's own arrays are only root-bearing after specialization.
    let caller = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "caller"))),
    );
    let normalized = normalize_runtime_semantic_body(&db, caller).expect("normalization failed");
    let hidden = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .find_map(|statement| match &statement.kind {
            NStatementKind::Define {
                expr: NExpr::Call { callee, .. },
                ..
            } if matches!(
                callee.key.owner(&db),
                BodyOwner::Func(func) if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "hidden")
            ) =>
            {
                Some(get_or_build_semantic_instance(&db, callee.key))
            }
            _ => None,
        })
        .expect("missing specialized `hidden` call");
    let error = layout_evidence_body(&db, hidden)
        .expect_err("the specialized body contains an array of layout-root values");
    assert!(
        matches!(
            error.unrepresentable(),
            Some(LayoutBundleUnrepresentable::RootArray { .. })
        ),
        "{error:?}"
    );
}

#[test]
fn ambiguous_runtime_const_layout_sources_are_rejected() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

fn root<const ROOT: u256>(
    left: StorageMap<u256, u256, ROOT>,
    right: StorageMap<u256, u256, ROOT>,
) -> u256 {
    ROOT
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "root"))),
    );
    assert!(matches!(
        layout_evidence_body(&db, instance),
        Err(LayoutEvidenceError::AmbiguousConstBinding { sources, .. })
            if sources.len() == 2
    ));
    let diagnostics = collect_layout_evidence_diagnostic_vouchers(&db, top_mod);
    let rendered = diagnostics
        .iter()
        .map(|diagnostic| format!("{:?}", diagnostic.to_complete(&db)))
        .collect::<Vec<_>>()
        .join("\n");
    assert_eq!(diagnostics.len(), 1, "{rendered}");
    assert!(rendered.contains("cannot determine inferred layout in `root`"));
    assert!(rendered.contains("this inferred slot has multiple runtime values"));
    assert!(rendered.contains("value parameter 1"));
    assert!(rendered.contains("value parameter 2"));
}

#[test]
fn effect_layout_evidence_is_defined_at_entry() {
    assert_layoutizes(
        "effect_layout_evidence_is_defined_at_entry.fe",
        r#"
use std::evm::StorageMap

fn is_set(key: u256) -> bool
    uses (map: StorageMap<u256, u256>)
{
    map.get(key: key) != 0
}
"#,
    );
}

#[test]
fn provider_backed_field_places_use_the_semantic_root_type() {
    assert_layoutizes(
        "provider_backed_field_places_use_the_semantic_root_type.fe",
        r#"
struct Cell { value: u256 }

fn write(value: u256) uses (cell: mut Cell) {
    cell.value = value
}
"#,
    );
}

#[test]
fn effect_value_arguments_select_the_callee_layout_view() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle, EffectRef}

struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

struct Ptr<T> { raw: *T }

impl<T> Copy for Ptr<T> {}

impl<T> EffectHandle for Ptr<T> {
    type Target = T
    type Raw = *T
    const SPACE: AddressSpace = AddressSpace::Memory

    fn raw(self) -> *T { self.raw }
}

impl<T> EffectRef<T> for Ptr<T> {}

impl<T> Ptr<T> {
    fn load(self) -> T
        where T: Copy
    {
        let mut ptr = self
        with (ptr) {
            core::effect_ref::read(ptr)
        }
    }
}

fn inspect<const ROOT: u256>() uses (ptr: Ptr<Rooted<ROOT>>) {}

fn forward<const ROOT: u256>(ptr: Ptr<Rooted<ROOT>>) -> Rooted<ROOT> {
    with (Ptr<Rooted<ROOT>> = ptr) {
        inspect()
    }
    ptr.load()
}
"#,
    );
    let mut pending = vec![get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "forward"))),
    )];
    let mut seen = HashSet::new();
    let mut found_load = false;
    while let Some(instance) = pending.pop() {
        if !seen.insert(instance.key(&db)) || instance.key(&db).owner(&db).body(&db).is_none() {
            continue;
        }
        let owner = instance.key(&db).owner(&db);
        let name = match owner {
            BodyOwner::Func(func) => func.name(&db).to_opt().map(|name| name.data(&db).clone()),
            _ => None,
        };
        layout_evidence_body(&db, instance).unwrap_or_else(|error| {
            panic!(
                "failed to layoutize reachable instance {name:?} {:?}: {error:?}",
                instance.key(&db),
            )
        });
        if let BodyOwner::Func(func) = owner
            && func
                .name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "load")
        {
            found_load = true;
        }
        pending.extend(
            instance
                .callees(&db)
                .iter()
                .map(|callee| get_or_build_semantic_instance(&db, callee.key)),
        );
    }
    assert!(found_load, "forward must reach the specialized Ptr::load");
}

#[test]
fn schema_view_rebasing_rejects_same_shaped_unrelated_roots() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Left<const ROOT: u256 = _> {}
struct Right<const ROOT: u256 = _> {}

fn take_left(value: Left) {}
fn take_right(value: Right) {}
"#,
    );
    let signature = |name| {
        get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, name))),
        )
        .key(&db)
        .layout_bundle_signature(&db)
    };
    let left = signature("take_left");
    let right = signature("take_right");
    let [left] = left.inputs.as_slice() else {
        panic!("take_left must have one layout input")
    };
    let [right] = right.inputs.as_slice() else {
        panic!("take_right must have one layout input")
    };
    assert_eq!(
        left.interface.schema.components[0].port,
        right.interface.schema.components[0].port
    );
    assert_eq!(
        left.interface.schema.components[0].ty,
        right.interface.schema.components[0].ty
    );
    assert!(
        right
            .interface
            .runtime_view_mapping(&left.interface.schema, &[])
            .expect("valid schema")
            .is_none()
    );
}

#[test]
fn effect_handle_providers_carry_physical_and_target_views() {
    assert_trusted_layoutizes(
        "effect_handle_providers_carry_physical_and_target_views.fe",
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Rooted<ROOT> {
    fn root(self) -> u256 { ROOT }
}

struct Payload {
    left: Rooted,
    right: Rooted,
}

impl Payload {
    fn root(self, lane: usize) -> u256 {
        if lane == 0 { self.left.root() } else { self.right.root() }
    }
}

struct Wrapper<const META: u256 = _> {
    marker: Rooted<META>,
    raw: u256,
}

impl<const META: u256> EffectHandle for Wrapper<META> {
    type Target = Payload
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 { self.raw }
}

impl<const META: u256> Wrapper<META> {
    fn marker_root(self) -> u256 { self.marker.root() }
}

msg Msg {
    #[selector = 1]
    Root { lane: usize } -> u256,
}

contract C {
    mut wrapper: Wrapper,

    recv Msg {
        Root { lane } -> u256 uses (wrapper) {
            wrapper.root(lane: lane)
        }
    }
}
"#,
    );
}

#[test]
fn self_recursive_effect_handle_view_uses_one_component_set() {
    let src = r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Rooted<ROOT> {
    fn root(self) -> u256 { ROOT }
}

struct Loop<const ROOT: u256 = _> {
    marker: Rooted<ROOT>,
    raw: u256,
}

impl<const ROOT: u256> EffectHandle for Loop<ROOT> {
    type Target = Loop<ROOT>
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 { self.raw }
}

impl<const ROOT: u256> Loop<ROOT> {
    fn root(self) -> u256 { self.marker.root() }
}

fn inspect<const ROOT: u256>(value: Loop<ROOT>) {}

msg Msg {
    #[selector = 1]
    Root {} -> u256,
}

contract C {
    mut value: Loop,

    recv Msg {
        Root {} -> u256 uses (value) {
            value.root()
        }
    }
}
"#;
    assert_trusted_layoutizes("self_recursive_effect_handle_view.fe", src);
    parse_ok!(trusted db, top_mod, src,);
    let inspect = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "inspect"))),
    );
    let signature = inspect.key(&db).layout_bundle_signature(&db);
    let [input] = signature.inputs.as_slice() else {
        panic!("self-recursive handle must have one layout-bearing input")
    };
    assert_eq!(input.interface.schema.components.len(), 1);
    assert_eq!(input.interface.schema.view_aliases.len(), 1);
    assert_eq!(
        input.interface.schema.view_aliases[0].alias,
        [LayoutEvidencePathStep::EffectTarget]
    );
    assert!(input.interface.schema.view_aliases[0].canonical.is_empty());
    assert!(input.interface.schema.validate().is_ok());

    let mut invalid = input.interface.schema.clone();
    invalid.view_aliases[0].canonical = vec![LayoutEvidencePathStep::Field(0)];
    assert!(matches!(
        invalid.validate(),
        Err(LayoutBundleSchemaError::InvalidViewAlias { alias: 0 })
    ));

    let mut invalid = input.interface.schema.clone();
    invalid.components[0].port.value_path = vec![LayoutEvidencePathStep::EffectTarget];
    assert!(matches!(
        invalid.validate(),
        Err(LayoutBundleSchemaError::NonCanonicalComponent { .. })
    ));

    let mut invalid = input.interface.schema.clone();
    invalid.view_aliases.push(LayoutViewAlias {
        alias: vec![
            LayoutEvidencePathStep::EffectTarget,
            LayoutEvidencePathStep::EffectTarget,
        ],
        canonical: Vec::new(),
    });
    assert!(matches!(
        invalid.validate(),
        Err(LayoutBundleSchemaError::OverlappingViewAlias {
            first: 0,
            second: 1,
        })
    ));
}

#[test]
fn mutually_recursive_effect_handle_views_have_stable_evidence() {
    assert_trusted_layoutizes(
        "mutually_recursive_effect_handle_views_have_stable_evidence.fe",
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Rooted<ROOT> {
    fn root(self) -> u256 { ROOT }
}

struct A<const ROOT: u256 = _> {
    marker: Rooted<ROOT>,
    raw: u256,
}

struct B<const ROOT: u256> {
    marker: Rooted<ROOT>,
    raw: u256,
}

impl<const ROOT: u256> EffectHandle for A<ROOT> {
    type Target = B<ROOT>
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 { self.raw }
}

impl<const ROOT: u256> EffectHandle for B<ROOT> {
    type Target = A<ROOT>
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 { self.raw }
}

impl<const ROOT: u256> B<ROOT> {
    fn target_root(self) -> u256 { self.marker.root() }
}

struct Holder<const ROOT: u256> {
    value: A<ROOT>,
}

fn forward_nested<const ROOT: u256>(holder: Holder<ROOT>) -> Holder<ROOT> {
    holder
}

fn forward<const ROOT: u256>(value: A<ROOT>) -> A<ROOT> {
    value
}

fn forwarded<const ROOT: u256>(value: A<ROOT>) -> A<ROOT> {
    forward(value)
}

msg Msg {
    #[selector = 1]
    Root {} -> u256,
}

contract C {
    mut value: A,

    recv Msg {
        Root {} -> u256 uses (value) {
            value.target_root()
        }
    }
}
"#,
    );
}

#[test]
fn finite_effect_target_chain_can_rejoin_an_older_permutation_family() {
    assert_trusted_layoutizes(
        "finite_effect_target_chain_can_rejoin_an_older_permutation_family.fe",
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Rooted<const ROOT: u256 = _> {}

struct A<T, U, const ROOT: u256 = _> {
    marker: Rooted<ROOT>,
    raw: u256,
}

impl<const ROOT: u256> EffectHandle for A<A<u8, u16, ROOT>, u8, ROOT> {
    type Target = A<u8, u16, ROOT>
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 { self.raw }
}

impl<const ROOT: u256> EffectHandle for A<u8, u16, ROOT> {
    type Target = A<u8, A<u8, u16, ROOT>, ROOT>
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 { self.raw }
}

struct Entry<const ROOT: u256 = _> {
    value: A<A<u8, u16, ROOT>, u8, ROOT>,
}

fn inspect<const ROOT: u256>(value: A<A<u8, u16, ROOT>, u8, ROOT>) {}

contract C {
    mut value: Entry,
}
"#,
    );
}

#[test]
fn permuted_recursive_effect_handle_views_have_stable_evidence() {
    let src = r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Rooted<ROOT> {
    fn root(self) -> u256 { ROOT }
}

struct A<const LEFT: u256 = _, const RIGHT: u256 = _> {
    left: Rooted<LEFT>,
    right: Rooted<RIGHT>,
    raw: u256,
}

struct B<const LEFT: u256, const RIGHT: u256> {
    left: Rooted<LEFT>,
    right: Rooted<RIGHT>,
    raw: u256,
}

impl<const LEFT: u256, const RIGHT: u256> EffectHandle for A<LEFT, RIGHT> {
    type Target = B<RIGHT, LEFT>
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 { self.raw }
}

impl<const LEFT: u256, const RIGHT: u256> EffectHandle for B<LEFT, RIGHT> {
    type Target = A<LEFT, RIGHT>
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 { self.raw }
}

impl<const LEFT: u256, const RIGHT: u256> B<LEFT, RIGHT> {
    fn left_root(self) -> u256 { self.left.root() }
}

fn inspect<const LEFT: u256, const RIGHT: u256>(value: A<LEFT, RIGHT>) {}

msg Msg {
    #[selector = 1]
    Root {} -> u256,
}

contract C {
    mut value: A,

    recv Msg {
        Root {} -> u256 uses (value) {
            value.left_root()
        }
    }
}
"#;
    assert_trusted_layoutizes(
        "permuted_recursive_effect_handle_views_have_stable_evidence.fe",
        src,
    );
    parse_ok!(trusted db, top_mod, src,);
    let inspect = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "inspect"))),
    );
    let signature = inspect.key(&db).layout_bundle_signature(&db);
    let [input] = signature.inputs.as_slice() else {
        panic!("permuted recursive handle must have one layout-bearing input")
    };
    assert_eq!(input.interface.schema.components.len(), 8);
    assert_eq!(input.interface.schema.view_aliases.len(), 1);
    assert_eq!(
        input.interface.schema.view_aliases[0].alias,
        vec![LayoutEvidencePathStep::EffectTarget; 4]
    );
    assert!(input.interface.schema.view_aliases[0].canonical.is_empty());
}

#[test]
fn non_regular_recursive_effect_handle_views_are_rejected() {
    parse_module!(
        trusted db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Rooted<const ROOT: u256 = _> {}

struct A<const ROOT: u256 = _> {
    marker: Rooted<ROOT>,
    raw: u256,
}

impl<const ROOT: u256> EffectHandle for A<ROOT> {
    type Target = A<{ ROOT + 1 }>
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Storage

    fn raw(self) -> u256 { self.raw }
}

fn inspect<const ROOT: u256>(value: A<ROOT>) {}

contract C {
    mut value: A,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db);
    let errors = layout.field_errors(&IdentId::new(&db, "value".to_string()));
    let inspect = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "inspect"))),
    );
    let signature = inspect.key(&db).layout_bundle_signature(&db);
    assert!(matches!(
        errors,
        Some([ContractLayoutError::NonRegularProviderCycle, ..])
    ));
    assert!(matches!(
        signature.inputs[0].interface.schema.unrepresentable,
        Some(LayoutBundleUnrepresentable::NonRegularViewCycle { .. })
    ));
    let field_diagnostics = initialize_analysis_pass()
        .run_on_module(&db, top_mod)
        .iter()
        .map(|diagnostic| diagnostic.to_complete(&db).message)
        .collect::<Vec<_>>();
    assert!(
        field_diagnostics
            .iter()
            .any(|message| message == "provider target layout is not finitely recursive"),
        "{field_diagnostics:#?}"
    );
    let rendered = collect_layout_evidence_diagnostic_vouchers(&db, top_mod)
        .iter()
        .map(|diagnostic| format!("{:?}", diagnostic.to_complete(&db)))
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        rendered.contains("cannot be represented by a finite layout-evidence interface"),
        "{rendered}"
    );
}

#[test]
fn sibling_runtime_const_layout_sources_are_rejected() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

struct Pair<const ROOT: u256 = _> {
    left: Rooted<ROOT>,
    right: Rooted<ROOT>,
}

fn root<const ROOT: u256>(pair: Pair<ROOT>) -> u256 {
    ROOT
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "root"))),
    );
    assert!(matches!(
        layout_evidence_body(&db, instance),
        Err(LayoutEvidenceError::AmbiguousConstBinding { sources, .. })
            if matches!(sources.as_ref(), [
                CallableLayoutParamPort::Input(left),
                CallableLayoutParamPort::Input(right),
            ] if left.component != right.component)
    ));
}

#[test]
fn layout_evidence_uses_one_descriptor_local_per_component() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

fn select<const ROOT: u256>(
    map: StorageMap<u256, u256, ROOT>,
) -> StorageMap<u256, u256, ROOT> {
    map
}
"#,
    );
    let func = find_func(&db, top_mod, "select");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let normalized = normalize_runtime_semantic_body(&db, instance).expect("normalization failed");
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    let source = normalized.body.owner.body(&db);
    let input = source
        .locals
        .iter()
        .enumerate()
        .find_map(|(idx, local)| {
            (local
                .source
                .and_then(|source| source.callable_input_origin(&db))
                == Some(CallableInputLayoutHoleOrigin::ValueParam(0)))
            .then_some(idx)
        })
        .expect("missing map input");
    let value = &evidence.semantic_values[input];
    let [component] = value.components.as_ref() else {
        panic!("a map must have one evidence component")
    };
    let LayoutEvidenceComponentValue::Dynamic(descriptor) = component else {
        panic!("generic map roots must be dynamic evidence")
    };

    assert_eq!(evidence.params, [*descriptor]);
    let representations =
        NLayoutLocals::new(&db, &normalized.body, &normalized.layout_plan, source);
    assert_eq!(evidence.semantic_values.len(), representations.locals.len());
    assert_eq!(evidence.output.schema.components.len(), 1);
    assert_eq!(evidence.output.runtime_descriptor_count(), 1);
    assert_eq!(evidence.terminators.len(), normalized.body.blocks.len());
    assert_eq!(
        evidence.statements.len(),
        normalized
            .body
            .blocks
            .iter()
            .map(|block| block.statements.len())
            .sum::<usize>()
    );
    let returns = evidence
        .terminators
        .iter()
        .find_map(|terminator| (!terminator.returns.is_empty()).then_some(&terminator.returns))
        .expect("missing layout evidence return");
    assert_eq!(returns.len(), 1);
    assert!(matches!(&returns[0].value, LayoutEvidenceOperand::Local(_)));
}

#[test]
fn mixed_compile_time_and_runtime_components_use_canonical_abi_order() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

struct Mixed<const ROOT: u256> {
    fixed: StorageMap<u256, u256, 7>,
    dynamic: StorageMap<u256, u256, ROOT>,
}

fn pass<const ROOT: u256>(value: Mixed<ROOT>) -> Mixed<ROOT> {
    value
}

fn forward<const ROOT: u256>(value: Mixed<ROOT>) -> Mixed<ROOT> {
    pass(value: value)
}
"#,
    );

    let pass = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "pass"))),
    );
    let signature = pass.key(&db).layout_bundle_signature(&db);
    let input = &signature.inputs[0].interface;
    assert_eq!(input.schema.components.len(), 2);
    assert_eq!(
        input.transport.component(LayoutBundleComponentId(0)),
        Some(LayoutBundleComponentTransport::CompileTime)
    );
    assert_eq!(
        input.transport.component(LayoutBundleComponentId(1)),
        Some(LayoutBundleComponentTransport::Runtime)
    );
    let mapping = input
        .runtime_view_mapping(&input.schema, &[])
        .expect("valid schema")
        .expect("identity view must map the runtime component");
    assert_eq!(mapping.source(LayoutBundleComponentId(0)), None);
    assert_eq!(
        mapping.source(LayoutBundleComponentId(1)),
        Some(LayoutBundleComponentId(1))
    );
    let params = signature.runtime_params().collect::<Vec<_>>();
    assert_eq!(params.len(), 1);
    assert_eq!(params[0].component_id, LayoutBundleComponentId(1));
    layout_evidence_body(&db, pass).expect("pass layoutization failed");

    let forward = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "forward"))),
    );
    let normalized = normalize_runtime_semantic_body(&db, forward).expect("normalization failed");
    let evidence = layout_evidence_body(&db, forward).expect("layoutization failed");
    let call = evidence
        .statements
        .iter()
        .find_map(|statement| statement.call.as_ref())
        .expect("missing call evidence");
    assert_eq!(call.args.len(), 1);
    assert!(matches!(
        &call.args[0].target,
        CallableLayoutParamPort::Input(port)
            if port.component == input.schema.components[1].port
    ));
    verify_layout_evidence_body(&db, &normalized, evidence).expect("evidence must verify");
}

#[test]
fn compile_time_call_outputs_materialize_without_runtime_call_evidence() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

const fn fixed() -> Rooted<7> {
    Rooted {}
}

fn pass() -> Rooted<7> {
    fixed()
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "pass"))),
    );
    let normalized = normalize_runtime_semantic_body(&db, instance).expect("normalization failed");
    let source = instance.body(&db);
    let source_call_ids = source
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .filter_map(|statement| {
            matches!(
                statement.kind,
                SStmtKind::Assign {
                    expr: SExpr::Call { .. },
                    ..
                }
            )
            .then_some(statement.id)
        })
        .collect::<Vec<_>>();
    let runtime_ids = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| {
            block
                .statements
                .iter()
                .filter_map(|statement| statement.source)
        })
        .collect::<Vec<_>>();
    assert_eq!(
        runtime_ids.len(),
        source
            .blocks
            .iter()
            .map(|block| block.stmts.len())
            .sum::<usize>()
    );
    assert!(
        runtime_ids
            .iter()
            .enumerate()
            .all(|(idx, id)| id.index() == idx)
    );
    assert!(
        normalized
            .body
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .any(|statement| {
                statement
                    .source
                    .is_some_and(|id| source_call_ids.contains(&id))
                    && matches!(
                        statement.kind,
                        NStatementKind::Define {
                            expr: NExpr::Const(_),
                            ..
                        }
                    )
            })
    );
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    let assignment = evidence
        .statements
        .iter()
        .inspect(|statement| assert!(statement.call.is_none()))
        .flat_map(|statement| &statement.assignments)
        .find(|assignment| {
            matches!(
                assignment.expr,
                LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(_))
            )
        })
        .expect("fixed call should initialize one local evidence component");
    assert!(matches!(
        assignment.expr,
        LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(ref value))
            if matches!(value.base, LayoutEvidenceBase::Root(_))
    ));
    assert_eq!(evidence.output.runtime_descriptor_count(), 0);
    assert!(
        evidence
            .terminators
            .iter()
            .all(|terminator| terminator.returns.is_empty())
    );
    verify_layout_evidence_body(&db, &normalized, evidence).expect("evidence must verify");
    verify_layout_evidence_runtime_compatibility(&db, &normalized, evidence)
        .expect("compile-time-only calls may disappear from the runtime body");
}

#[test]
fn runtime_layout_calls_survive_semantic_const_folding() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

const fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

const fn alternate<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn pass<const ROOT: u256>(anchor: Rooted<ROOT>) -> Rooted<ROOT> {
    let _preserved = anchor
    fresh()
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "pass"))),
    );
    let normalized = normalize_runtime_semantic_body(&db, instance).expect("normalization failed");
    assert!(normalized.body.blocks.iter().any(|block| {
        block.statements.iter().any(|statement| {
            matches!(
                statement.kind,
                NStatementKind::Define {
                    expr: NExpr::Call { .. },
                    ..
                }
            )
        })
    }));
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    assert!(
        evidence
            .statements
            .iter()
            .any(|statement| statement.call.is_some())
    );
    verify_layout_evidence_body(&db, &normalized, evidence).expect("evidence must verify");
    verify_layout_evidence_runtime_compatibility(&db, &normalized, evidence)
        .expect("evidence must match the runtime body");

    let mut reordered = normalized.clone();
    let positions = reordered
        .body
        .blocks
        .iter()
        .enumerate()
        .flat_map(|(block, data)| {
            data.statements
                .iter()
                .enumerate()
                .filter(|(_, statement)| statement.source.is_some())
                .map(move |(statement, _)| (block, statement))
        })
        .take(2)
        .collect::<Vec<_>>();
    let [
        (first_block, first_statement),
        (second_block, second_statement),
    ] = positions.as_slice()
    else {
        panic!("fixture must contain two statements")
    };
    let first = reordered.body.blocks[*first_block].statements[*first_statement].clone();
    let second = reordered.body.blocks[*second_block].statements[*second_statement].clone();
    reordered.body.blocks[*first_block].statements[*first_statement] = second;
    reordered.body.blocks[*second_block].statements[*second_statement] = first;
    verify_layout_evidence_runtime_compatibility(&db, &reordered, evidence)
        .expect("statement identity must make evidence independent of statement position");

    let mut duplicate = normalized.clone();
    let duplicate_id = duplicate
        .body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .map(|statement| statement.id)
        .next()
        .expect("fixture must contain a statement");
    duplicate
        .body
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.statements)
        .filter(|statement| statement.source.is_some())
        .nth(1)
        .expect("fixture must contain another statement")
        .id = duplicate_id;
    assert_eq!(
        verify_layout_evidence_runtime_compatibility(&db, &duplicate, evidence),
        Err(LayoutEvidenceVerifyError::DuplicateStatementId(
            duplicate_id
        ))
    );

    let mut invalid = normalized.clone();
    let invalid_id = NStatementId::from_u32(evidence.statements.len() as u32);
    invalid
        .body
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.statements)
        .find(|statement| statement.source.is_some())
        .expect("fixture must contain a statement")
        .id = invalid_id;
    assert!(matches!(
        verify_layout_evidence_runtime_compatibility(&db, &invalid, evidence),
        Err(LayoutEvidenceVerifyError::InvalidStatementId { id, .. }) if id == invalid_id
    ));

    let mut malformed = (*evidence).clone();
    malformed
        .statements
        .iter_mut()
        .find(|statement| statement.call.is_some())
        .expect("missing runtime evidence call")
        .call = None;
    assert!(matches!(
        verify_layout_evidence_runtime_compatibility(&db, &normalized, &malformed),
        Err(LayoutEvidenceVerifyError::CallPresence { .. })
    ));

    let alternate =
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "alternate")));
    let mut mismatched = normalized.clone();
    let callee = mismatched
        .body
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.statements)
        .find_map(|statement| match &mut statement.kind {
            NStatementKind::Define {
                expr: NExpr::Call { callee, .. },
                ..
            } => Some(callee),
            NStatementKind::Define { .. } | NStatementKind::Store { .. } => None,
        })
        .expect("missing runtime evidence call");
    callee.key = alternate;
    assert!(matches!(
        verify_layout_evidence_runtime_compatibility(&db, &mismatched, evidence),
        Err(LayoutEvidenceVerifyError::CallCalleeMismatch { .. })
    ));
}

#[test]
fn recursive_runtime_layout_calls_do_not_form_a_signature_cycle() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

fn recurse<const ROOT: u256>(value: Rooted<ROOT>, depth: u256) -> Rooted<ROOT> {
    if depth == 0 {
        return value
    }
    recurse(value, depth: depth - 1)
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "recurse"))),
    );
    let normalized = normalize_runtime_semantic_body(&db, instance).expect("normalization failed");
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    verify_layout_evidence_runtime_compatibility(&db, &normalized, evidence)
        .expect("recursive call evidence must match the runtime body");
}

#[test]
fn output_only_generic_layout_params_are_supplied_by_output_witness() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn rebuild<const ROOT: u256>(seed: Rooted<ROOT>) -> Rooted<ROOT> {
    fresh()
}
"#,
    );
    for name in ["fresh", "rebuild"] {
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, name))),
        );
        let normalized =
            normalize_runtime_semantic_body(&db, instance).expect("normalization failed");
        let evidence = layout_evidence_body(&db, instance)
            .unwrap_or_else(|error| panic!("{name} layoutization failed: {error:?}"));
        if name == "rebuild" {
            assert!(
                instance
                    .key(&db)
                    .layout_bundle_signature(&db)
                    .output_witnesses
                    .schema
                    .components
                    .is_empty()
            );
            assert_eq!(evidence.params.len(), 1);
        }
        verify_layout_evidence_body(&db, &normalized, evidence).expect("evidence must verify");
    }
}

#[test]
fn ambiguous_input_components_do_not_invent_an_output_witness() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

fn fresh_from<const ROOT: u256>(
    left: Rooted<ROOT>,
    right: Rooted<ROOT>,
) -> Rooted<ROOT> {
    Rooted {}
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "fresh_from"))),
    );
    let signature = instance.key(&db).layout_bundle_signature(&db);
    assert!(signature.output_witnesses.schema.components.is_empty());
    assert!(matches!(
        layout_evidence_body(&db, instance),
        Err(LayoutEvidenceError::AmbiguousComponentBinding { sources, .. })
            if sources.len() == 2
    ));
}

#[test]
fn output_witness_worklist_composes_nested_struct_and_enum_paths() {
    assert_layoutizes(
        "output_witness_worklist_composes_nested_struct_and_enum_paths.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

struct Pair<const ROOT: u256 = _> {
    left: Rooted<ROOT>,
    right: Rooted<ROOT>,
}

struct Boxed<const ROOT: u256 = _> {
    pair: Pair<ROOT>,
}

enum Choice<const ROOT: u256 = _> {
    One(Rooted<ROOT>),
    Two(Pair<ROOT>),
}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn nested<const ROOT: u256>() -> Boxed<ROOT> {
    Boxed {
        pair: Pair { left: fresh(), right: fresh() },
    }
}

fn choice<const ROOT: u256>(one: bool) -> Choice<ROOT> {
    if one {
        Choice::One(fresh())
    } else {
        Choice::Two(Pair { left: fresh(), right: fresh() })
    }
}
"#,
    );
}

#[test]
fn output_witness_worklist_propagates_through_whole_and_field_stores() {
    assert_layoutizes(
        "output_witness_worklist_propagates_through_whole_and_field_stores.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

struct Pair<const ROOT: u256 = _> {
    left: Rooted<ROOT>,
    right: Rooted<ROOT>,
}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn replace_value<const ROOT: u256>() -> Rooted<ROOT> {
    let mut value = fresh()
    value = fresh()
    value
}

fn replace_field<const ROOT: u256>() -> Pair<ROOT> {
    let mut pair = Pair { left: fresh(), right: fresh() }
    pair.left = fresh()
    pair
}
"#,
    );
}

#[test]
fn output_witness_worklist_propagates_through_enum_extraction() {
    assert_layoutizes(
        "output_witness_worklist_propagates_through_enum_extraction.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

enum Choice<const ROOT: u256 = _> {
    Left(Rooted<ROOT>),
    Right(Rooted<ROOT>),
}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn choose<const ROOT: u256>(left: bool) -> Rooted<ROOT> {
    let choice = if left {
        Choice::Left(fresh())
    } else {
        Choice::Right(fresh())
    }
    match choice {
        Choice::Left(value) => value
        Choice::Right(value) => value
    }
}
"#,
    );
}

#[test]
fn zero_length_arrays_of_layout_root_values_are_rejected() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

fn empty<const ROOT: u256>() -> [Rooted<ROOT>; 0] {
    []
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "empty"))),
    );
    let error = layout_evidence_body(&db, instance)
        .expect_err("a zero-length array of layout-root values is still rejected");
    assert!(
        matches!(
            error.unrepresentable(),
            Some(LayoutBundleUnrepresentable::RootArray { .. })
        ),
        "{error:?}"
    );
}

#[test]
fn control_flow_selects_layout_evidence_with_the_value() {
    assert_layoutizes(
        "control_flow_selects_layout_evidence_with_the_value.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

impl<const ROOT: u256> Rooted<ROOT> {
    fn root(self) -> u256 {
        ROOT
    }
}

fn choose<const ROOT: u256>(
    first: Rooted<ROOT>,
    second: Rooted<ROOT>,
    left: bool,
) -> Rooted<ROOT> {
    if left { first } else { second }
}

fn consume<const ROOT: u256>(value: Rooted<ROOT>, left: bool) -> u256 {
    choose(first: value, second: value, left).root()
}
"#,
    );
}

#[test]
fn verifier_requires_branch_definitions_on_every_path() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

fn consume<const ROOT: u256>(_ value: Rooted<ROOT>) {}

fn identity<const ROOT: u256>(_ value: Rooted<ROOT>) -> Rooted<ROOT> {
    value
}

fn branch<const ROOT: u256>(first: Rooted<ROOT>, second: Rooted<ROOT>, left: bool) {
    let selected = if left { identity(first) } else { identity(second) }
    consume(selected)
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "branch"))),
    );
    let normalized = normalize_runtime_semantic_body(&db, instance).expect("normalization failed");
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    let calls_to = |name: &str| {
        normalized
            .body
            .blocks
            .iter()
            .enumerate()
            .flat_map(|(block, data)| {
                data.statements
                    .iter()
                    .enumerate()
                    .map(move |(statement, data)| (block, statement, data))
            })
            .filter_map(|(block, statement, data)| match &data.kind {
                NStatementKind::Define {
                    expr: NExpr::Call { callee, .. },
                    ..
                } if matches!(
                    callee.key.owner(&db),
                    BodyOwner::Func(func) if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|callee_name| callee_name.data(&db) == name)
                ) =>
                {
                    Some((block, statement, data.id))
                }
                _ => None,
            })
            .collect::<Vec<_>>()
    };
    let (_, _, branch_call) = calls_to("identity")[0];
    let branch_local = evidence
        .statement(branch_call)
        .expect("missing branch call evidence")
        .assignments
        .iter()
        .find_map(|assignment| {
            matches!(assignment.expr, LayoutEvidenceExpr::CallResult { .. })
                .then_some(assignment.dst)
        })
        .expect("missing branch-local call result evidence");
    let [(call_block, call_statement, call_id)] = calls_to("consume")[..] else {
        panic!("expected one post-merge layout call")
    };

    let mut malformed = (*evidence).clone();
    malformed.statements[call_id.index()]
        .call
        .as_mut()
        .expect("missing post-merge layout call")
        .args[0]
        .value = LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Local(branch_local));
    assert_eq!(
        verify_layout_evidence_body(&db, &normalized, &malformed),
        Err(LayoutEvidenceVerifyError::UndefinedLocal {
            block: call_block,
            statement: Some(call_statement),
            local: branch_local,
        })
    );
}

#[test]
fn verifier_rejects_component_identity_corruption() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

struct Pair<const LEFT: u256 = _, const RIGHT: u256 = _> {
    left: Rooted<LEFT>,
    right: Rooted<RIGHT>,
}

fn pass<const LEFT: u256, const RIGHT: u256>(
    value: Pair<LEFT, RIGHT>,
) -> Pair<LEFT, RIGHT> {
    value
}

fn caller<const LEFT: u256, const RIGHT: u256>(
    value: Pair<LEFT, RIGHT>,
) -> Pair<LEFT, RIGHT> {
    pass(value: value)
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "caller"))),
    );
    let normalized = normalize_runtime_semantic_body(&db, instance).expect("normalization failed");
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");

    let mut malformed = (*evidence).clone();
    let (local, value) = malformed
        .semantic_values
        .iter_mut()
        .enumerate()
        .find(|(_, value)| value.components.len() == 2)
        .expect("missing two-component evidence value");
    value.components = value.components[..1].into();
    assert!(matches!(
        verify_layout_evidence_body(&db, &normalized, &malformed),
        Err(LayoutEvidenceVerifyError::ComponentValueCount {
            local: actual,
            expected: 2,
            actual: 1,
        }) if actual.index() == local
    ));

    let mut malformed = (*evidence).clone();
    let assignments = malformed
        .statements
        .iter_mut()
        .find_map(|statement| {
            statement
                .call
                .is_some()
                .then_some(&mut statement.assignments)
        })
        .expect("missing call evidence assignments");
    let [first, second] = assignments.as_mut() else {
        panic!("call must return two evidence components")
    };
    let LayoutEvidenceExpr::CallResult {
        component: second_component,
    } = &second.expr
    else {
        panic!("second assignment must read a call result")
    };
    first.expr = LayoutEvidenceExpr::CallResult {
        component: *second_component,
    };
    assert!(matches!(
        verify_layout_evidence_body(&db, &normalized, &malformed),
        Err(LayoutEvidenceVerifyError::InvalidCallResult { .. })
    ));

    let mut malformed = (*evidence).clone();
    let returns = malformed
        .terminators
        .iter_mut()
        .find_map(|terminator| (terminator.returns.len() == 2).then_some(&mut terminator.returns))
        .expect("missing two-component evidence return");
    returns.swap(0, 1);
    assert!(matches!(
        verify_layout_evidence_body(&db, &normalized, &malformed),
        Err(LayoutEvidenceVerifyError::ReturnComponentMismatch { .. })
    ));
}

#[test]
fn constructors_preserve_roots() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

fn rebuild<const ROOT: u256>(value: Rooted<ROOT>) -> Rooted<ROOT> {
    Rooted {}
}
"#,
    );
    let rebuild = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "rebuild"))),
    );
    let rebuild = layout_evidence_body(&db, rebuild).expect("rebuild layoutization failed");
    let rebuild_returns = rebuild
        .terminators
        .iter()
        .find_map(|terminator| (!terminator.returns.is_empty()).then_some(&terminator.returns))
        .expect("missing rebuild evidence return");
    assert_eq!(rebuild.params.len(), 1);
    assert_eq!(rebuild_returns.len(), 1);
}

#[test]
fn effect_handle_values_use_declared_target_evidence() {
    parse_ok!(
        trusted db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Rooted<const ROOT: u256 = _> {}

struct Pair<const LEFT: u256, const RIGHT: u256> {
    left: Rooted<LEFT>,
    right: Rooted<RIGHT>,
}

struct Handle<T> {
    raw: u256,
}

struct DualHandle<T, const META: u256 = _> {
    marker: Rooted<META>,
    raw: u256,
}

struct MirrorTarget<const LOGICAL: u256> {
    value: Rooted<LOGICAL>,
}

struct MirrorHandle<T, const PHYSICAL: u256> {
    value: Rooted<PHYSICAL>,
    raw: u256,
}

impl<T> EffectHandle for Handle<T> {
    type Target = T
    type Raw = u256

    const SPACE: AddressSpace = AddressSpace::Memory

    fn raw(self) -> u256 {
        self.raw
    }
}

impl<T> Handle<T> {
    fn replace_raw(mut self, raw: u256) {
        self.raw = raw
    }
}

impl<T, const META: u256> EffectHandle for DualHandle<T, META> {
    type Target = T
    type Raw = u256

    const SPACE: AddressSpace = AddressSpace::Memory

    fn raw(self) -> u256 {
        self.raw
    }
}

impl<T, const META: u256> DualHandle<T, META> {
    fn marker(self) -> Rooted<META> {
        self.marker
    }
}

impl<T, const PHYSICAL: u256> EffectHandle for MirrorHandle<T, PHYSICAL> {
    type Target = T
    type Raw = u256

    const SPACE: AddressSpace = AddressSpace::Memory

    fn raw(self) -> u256 {
        self.raw
    }
}

fn rebuild<const ROOT: u256>(
    source: Rooted<ROOT>,
    raw: u256,
) -> Handle<Rooted<ROOT>> {
    Handle { raw }
}

fn replace_raw_with_nested_target<const LEFT: u256, const RIGHT: u256>(
    handle: mut Handle<Pair<LEFT, RIGHT>>,
    raw: u256,
) {
    handle.replace_raw(raw)
}

fn marker_at<const META: u256>(handle: DualHandle<Pair<1, 2>, META>) -> Rooted<META> {
    handle.marker()
}

fn inspect_views<const PHYSICAL: u256, const LOGICAL: u256>(
    handle: MirrorHandle<MirrorTarget<LOGICAL>, PHYSICAL>,
) {}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "rebuild"))),
    );
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    let [input] = evidence.params.as_slice() else {
        panic!("generic handle target must have one evidence input")
    };
    assert!(
        evidence
            .statements
            .iter()
            .flat_map(|statement| &statement.assignments)
            .any(|assignment| matches!(
                assignment.expr,
                LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Local(source)) if source == *input
            ))
    );

    let replace_raw_caller = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "replace_raw_with_nested_target")),
        ),
    );
    let normalized =
        normalize_runtime_semantic_body(&db, replace_raw_caller).expect("normalization failed");
    let callee = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .find_map(|statement| match statement.kind {
            NStatementKind::Define {
                expr: NExpr::Call { callee, .. },
                ..
            } => Some(callee),
            NStatementKind::Define { .. } | NStatementKind::Store { .. } => None,
        })
        .expect("missing Handle::replace_raw call");
    let replace_raw = get_or_build_semantic_instance(&db, callee.key);
    let normalized =
        normalize_runtime_semantic_body(&db, replace_raw).expect("normalization failed");
    let evidence = layout_evidence_body(&db, replace_raw)
        .expect("physical EffectHandle field writes must preserve target evidence opaquely");
    let mut stores = 0;
    for block in &normalized.body.blocks {
        for statement in &block.statements {
            let evidence_statement = evidence
                .statement(statement.id)
                .expect("missing statement evidence");
            if matches!(statement.kind, NStatementKind::Store { .. }) {
                stores += 1;
                assert!(
                    evidence_statement.assignments.is_empty(),
                    "physical handle writes must not update logical target evidence"
                );
            }
        }
    }
    assert_eq!(stores, 1, "expected one physical handle-field write");

    let marker_at = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "marker_at"))),
    );
    layout_evidence_body(&db, marker_at)
        .expect("handles must carry both physical and target layout evidence");

    let inspect_views = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "inspect_views")),
        ),
    );
    let signature = inspect_views.key(&db).layout_bundle_signature(&db);
    let [input] = signature.inputs.as_slice() else {
        panic!("dual-view handle must have one layout-bearing input")
    };
    let [physical, target] = input.interface.schema.components.as_slice() else {
        panic!("dual-view handle must retain one physical and one target component")
    };
    assert!(
        !physical
            .port
            .value_path
            .contains(&LayoutEvidencePathStep::EffectTarget)
    );
    assert!(
        target
            .port
            .value_path
            .contains(&LayoutEvidencePathStep::EffectTarget)
    );
    assert_eq!(physical.supplied_const_params.len(), 1);
    assert_eq!(target.supplied_const_params.len(), 1);
    assert_ne!(
        physical.supplied_const_params, target.supplied_const_params,
        "physical and target components must bind metadata from their own view"
    );
}

#[test]
fn provider_stores_supply_layout_context_to_fresh_values() {
    assert_layoutizes(
        "provider_stores_supply_layout_context_to_fresh_values.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

contract C {
    rooted: Rooted,

    init() uses (mut rooted) {
        rooted = fresh()
    }
}
"#,
    );
}

#[test]
fn layout_evidence_covers_existing_forwarding_matrix() {
    for (name, src) in [
        (
            "layout_root_constructed_aggregate_forwarding.fe",
            include_str!(
                "../../../fe/tests/fixtures/fe_test/layout_root_constructed_aggregate_forwarding.fe"
            ),
        ),
        (
            "layout_root_fresh_constructor_forwarding.fe",
            include_str!(
                "../../../fe/tests/fixtures/fe_test/layout_root_fresh_constructor_forwarding.fe"
            ),
        ),
        (
            "layout_root_enum_helper_forwarding.fe",
            include_str!(
                "../../../fe/tests/fixtures/fe_test/layout_root_enum_helper_forwarding.fe"
            ),
        ),
        (
            "layout_root_return_effect_forwarding.fe",
            include_str!(
                "../../../fe/tests/fixtures/fe_test/layout_root_return_effect_forwarding.fe"
            ),
        ),
        (
            "effect_handle_field_deref.fe",
            include_str!("../../../codegen/tests/fixtures/effect_handle_field_deref.fe"),
        ),
        (
            "layout_root_aggregate_effect_forwarding.fe",
            include_str!(
                "../../../fe/tests/fixtures/fe_test/layout_root_aggregate_effect_forwarding.fe"
            ),
        ),
        (
            "layout_root_recursive_forwarding.fe",
            include_str!("../../../fe/tests/fixtures/fe_test/layout_root_recursive_forwarding.fe"),
        ),
        (
            "mutable_array_args_and_effects.fe",
            include_str!("../../../fe/tests/fixtures/fe_test/mutable_array_args_and_effects.fe"),
        ),
        (
            "nested_provider_layout_roots.fe",
            include_str!("../../../fe/tests/fixtures/fe_test/nested_provider_layout_roots.fe"),
        ),
        (
            "with_block_custom_effect.fe",
            include_str!("../../../fe/tests/fixtures/fe_test/with_block_custom_effect.fe"),
        ),
    ] {
        assert_layoutizes(name, src);
    }
}
