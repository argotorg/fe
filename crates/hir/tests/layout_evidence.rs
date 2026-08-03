#[path = "support/layout.rs"]
mod layout_test_support;

use common::{
    InputDb,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use cranelift_entity::EntityRef;
use fe_hir::{
    analysis::{
        initialize_analysis_pass,
        semantic::{
            EffectProviderSubst, GenericSubst, ImplEnv, LayoutEvidenceBase,
            LayoutEvidenceComponentValue, LayoutEvidenceError, LayoutEvidenceExpr,
            LayoutEvidenceIndex, LayoutEvidenceOperand, LayoutEvidenceVerifyError, NExpr,
            NSStmtKind, NSTerminatorKind, SBlockId, SStmtId, SemanticInstanceKey,
            collect_layout_evidence_diagnostic_vouchers,
            collect_semantic_borrow_diagnostic_vouchers, get_or_build_semantic_instance,
            identity_semantic_instance_key, layout_evidence_body, non_regular_recursive_call_sites,
            normalize_semantic_body, normalize_semantic_body_for_layout_evidence,
            normalized_cfg_reachable_blocks, verify_layout_evidence_body,
            verify_layout_evidence_runtime_compatibility,
        },
        ty::{
            CallableLayoutParamPort, CallableLayoutPort, LayoutBundleComponentId,
            LayoutBundleComponentTransport, LayoutBundleSchemaError, LayoutEvidencePathStep,
            LayoutViewAlias, const_ty::CallableInputLayoutHoleOrigin, ty_check::BodyOwner,
        },
    },
    core::semantic::ContractLayoutError,
    hir_def::{CallableDef, IdentId, ItemKind},
    test_db::{HirAnalysisTestDb, find_contract, find_func, format_diagnostics},
};
use layout_test_support::{parse_module, parse_ok};
use url::Url;

fn assert_layoutizes(name: &str, src: &str) {
    parse_ok!(db, top_mod, src);
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
fn function_item_values_layoutize_as_zero_sized_values() {
    assert_layoutizes(
        "function_item_values_layoutize_as_zero_sized_values.fe",
        r#"
fn answer(value: u256) -> u256 {
    value
}

fn identity<T>(value: own T) -> T {
    value
}

struct Number {
    value: u256,
}

impl Number {
    fn read(number: Number) -> u256 {
        number.value
    }
}

trait Read {
    fn read(number: Self) -> u256
}

impl Read for Number {
    fn read(number: Number) -> u256 {
        number.value
    }
}

enum Maybe {
    Some(u256),
    None,
}

fn seed() -> u256 {
    let answer_value = answer
    let identity_value = identity
    let inherent_value = Number::read
    let trait_value = Read::read
    let ufcs_value = <Number as Read>::read
    let constructor_value = Maybe::Some
    let maybe = constructor_value(6)
    let constructed = match maybe {
        Maybe::Some(value) => value
        Maybe::None => 0
    }
    answer_value(value: 1)
        + identity_value(value: 2)
        + inherent_value(number: Number { value: 3 })
        + trait_value(number: Number { value: 4 })
        + ufcs_value(number: Number { value: 5 })
        + constructed
}
"#,
    );
}

#[test]
fn layout_evidence_preserves_sparse_statement_identities_after_terminal_pruning() {
    parse_ok!(
        db,
        top_mod,
        r#"
fn probe(_ stop: bool) {
    if stop {
        panic()
        let dead: u256 = 1
        let _ = dead
    }
    let live: u256 = 2
    let _ = live
}
"#,
    );
    let probe = find_func(&db, top_mod, "probe");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(probe)),
    );
    let normalized = normalize_semantic_body(&db, instance).expect("normalization failed");
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    verify_layout_evidence_body(&db, &normalized, evidence).expect("evidence must verify");
    verify_layout_evidence_runtime_compatibility(&db, &normalized, evidence)
        .expect("evidence must match the runtime body");

    // Lowering now prunes statements after a terminal call before assigning
    // identities. Synthesize a gap so this test exercises the sparse identity
    // contract without depending on that obsolete lowering side effect.
    let mut sparse_normalized = normalized.clone();
    let mut statement_ids = Vec::new();
    for statement in sparse_normalized
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.stmts)
    {
        statement.id = SStmtId::from_u32(
            u32::try_from(statement.id.index() + 1).expect("statement identity overflow"),
        );
        statement_ids.push(statement.id);
    }
    let mut sparse_evidence = (*evidence).clone();
    sparse_evidence.statements.insert(0, None);

    assert_eq!(sparse_evidence.statements.first(), Some(&None));
    assert_eq!(
        sparse_evidence.statements.iter().flatten().count(),
        statement_ids.len()
    );
    assert!(
        statement_ids
            .iter()
            .all(|statement| sparse_evidence.statement(*statement).is_some())
    );
    verify_layout_evidence_body(&db, &sparse_normalized, &sparse_evidence)
        .expect("sparse evidence must verify");
    verify_layout_evidence_runtime_compatibility(&db, &sparse_normalized, &sparse_evidence)
        .expect("sparse evidence must match the runtime body");
}

#[test]
fn closure_capture_layout_outputs_cross_the_call_boundary() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {
    value: u256,
}

fn captured<const ROOT: u256>(_ value: own Rooted<ROOT>) -> Rooted<ROOT> {
    let take = || value
    take.call_once()
}
"#,
    );
    let parent = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "captured"))),
    );
    layout_evidence_body(&db, parent).expect("parent layoutization failed");
    let closure = parent
        .callees(&db)
        .iter()
        .copied()
        .find(|callee| matches!(callee.key.owner(&db), BodyOwner::Closure { .. }))
        .expect("missing closure instance");
    let closure = get_or_build_semantic_instance(&db, closure.key);
    layout_evidence_body(&db, closure).expect("closure layoutization failed");
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
    let normalized = normalize_semantic_body(&db, instance).expect("normalization failed");
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    assert!(std::ptr::eq(
        evidence,
        layout_evidence_body(&db, instance).expect("cached layoutization failed")
    ));
    let bindings = evidence
        .statements
        .iter()
        .flatten()
        .flat_map(|statement| &statement.const_bindings)
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
        GenericSubst::new(&db, vec![params[1], params[1]]),
        EffectProviderSubst::empty(&db),
        ImplEnv::empty(&db, func.scope()),
    );
    let instance = get_or_build_semantic_instance(&db, key);
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    let bindings = evidence
        .statements
        .iter()
        .flatten()
        .flat_map(|statement| &statement.const_bindings)
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
            GenericSubst::new(&db, vec![params[1], params[1]]),
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
        .flatten()
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
            GenericSubst::new(&db, vec![params[1], params[1]]),
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
fn fresh_call_arguments_do_not_borrow_layout_evidence_from_siblings() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn take_same<T>(first: T, second: T) {}

fn forward<const ROOT: u256>(values: [Rooted<ROOT>; 2], lane: usize) {
    let selected = values[lane]
    take_same(first: selected, second: fresh())
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
    assert!(rendered.contains("cannot determine inferred layout in `forward`"));
    assert!(rendered.contains("no runtime layout root is available"));
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

struct Ptr<T> { raw: u256 }

impl<T> Copy for Ptr<T> {}

impl<T> EffectHandle for Ptr<T> {
    type Target = T
    const SPACE: AddressSpace = AddressSpace::Memory
    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
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
    let mut seen = std::collections::HashSet::new();
    let mut found_load = false;
    while let Some(instance) = pending.pop() {
        if !seen.insert(instance.key(&db)) || instance.key(&db).owner(&db).body(&db).is_none() {
            continue;
        }
        layout_evidence_body(&db, instance).unwrap_or_else(|error| {
            panic!(
                "failed to layoutize reachable instance {:?}: {error:?}",
                instance.key(&db),
            )
        });
        if let BodyOwner::Func(func) = instance.key(&db).owner(&db)
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
        left.interface.schema.components[0].map_ty(),
        right.interface.schema.components[0].map_ty()
    );
    assert!(
        right
            .interface
            .runtime_view_mapping(&left.interface.schema, &[])
            .is_none()
    );
}

#[test]
fn effect_handle_providers_carry_physical_and_target_views() {
    assert_layoutizes(
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
    const SPACE: AddressSpace = AddressSpace::Storage

    fn from_raw(_ raw: u256) -> Self {
        Self { marker: Rooted {}, raw }
    }

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
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { marker: Rooted {}, raw } }
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
    assert_layoutizes("self_recursive_effect_handle_view.fe", src);
    parse_ok!(db, top_mod, src,);
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
    assert_layoutizes(
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
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { marker: Rooted {}, raw } }
    fn raw(self) -> u256 { self.raw }
}

impl<const ROOT: u256> EffectHandle for B<ROOT> {
    type Target = A<ROOT>
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { marker: Rooted {}, raw } }
    fn raw(self) -> u256 { self.raw }
}

impl<const ROOT: u256> B<ROOT> {
    fn target_root(self) -> u256 { self.marker.root() }
}

struct Holder<const ROOT: u256> {
    value: A<ROOT>,
}

fn forward_array<const ROOT: u256>(values: own [A<ROOT>; 2]) -> [A<ROOT>; 2] {
    values
}

fn forward_nested<const ROOT: u256>(holder: own Holder<ROOT>) -> Holder<ROOT> {
    holder
}

fn forward<const ROOT: u256>(value: own A<ROOT>) -> A<ROOT> {
    value
}

fn forwarded<const ROOT: u256>(value: own A<ROOT>) -> A<ROOT> {
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
    assert_layoutizes(
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
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { marker: Rooted {}, raw } }
    fn raw(self) -> u256 { self.raw }
}

impl<const ROOT: u256> EffectHandle for A<u8, u16, ROOT> {
    type Target = A<u8, A<u8, u16, ROOT>, ROOT>
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { marker: Rooted {}, raw } }
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
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self {
        Self { left: Rooted {}, right: Rooted {}, raw }
    }
    fn raw(self) -> u256 { self.raw }
}

impl<const LEFT: u256, const RIGHT: u256> EffectHandle for B<LEFT, RIGHT> {
    type Target = A<LEFT, RIGHT>
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self {
        Self { left: Rooted {}, right: Rooted {}, raw }
    }
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
    assert_layoutizes(
        "permuted_recursive_effect_handle_views_have_stable_evidence.fe",
        src,
    );
    parse_ok!(db, top_mod, src,);
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
        db,
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
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { marker: Rooted {}, raw } }
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
    assert!(
        signature.inputs[0]
            .interface
            .schema
            .non_regular_view_cycle
            .is_some()
    );
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
fn provider_whole_array_stores_supply_ranked_context() {
    assert_layoutizes(
        "provider_whole_array_stores_supply_ranked_context.fe",
        r#"
struct Rooted<const ROOT: u256 = _> { value: u256 }

contract C {
    values: [Rooted; 3],

    init() uses (mut values) {
        values = [
            Rooted { value: 1 },
            Rooted { value: 2 },
            Rooted { value: 3 },
        ]
    }
}
"#,
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
    maps: [[StorageMap<u256, u256, ROOT>; 3]; 2],
    row: usize,
    col: usize,
) -> StorageMap<u256, u256, ROOT> {
    maps[row][col]
}
"#,
    );
    let func = find_func(&db, top_mod, "select");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let normalized = normalize_semantic_body(&db, instance).expect("normalization failed");
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    let input = normalized
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
        .expect("missing maps input");
    let value = &evidence.semantic_values[input];
    let [component] = value.components.as_ref() else {
        panic!("nested map array must have one evidence component")
    };
    let LayoutEvidenceComponentValue::Dynamic(descriptor) = component else {
        panic!("generic map roots must be dynamic evidence")
    };

    assert_eq!(value.schema.components[0].rank(), 2);
    assert_eq!(evidence.params, [*descriptor]);
    assert_eq!(evidence.locals[descriptor.index()].map_ty.rank(), 2);
    assert_eq!(evidence.semantic_values.len(), normalized.locals.len());
    assert_eq!(evidence.output.schema.components.len(), 1);
    assert_eq!(evidence.output.schema.components[0].rank(), 0);
    assert_eq!(evidence.output.runtime_descriptor_count(), 1);
    assert_eq!(evidence.terminators.len(), normalized.blocks.len());
    assert_eq!(
        evidence.statements.iter().flatten().count(),
        normalized
            .blocks
            .iter()
            .map(|block| block.stmts.len())
            .sum()
    );
    let (source, indices) = evidence
        .statements
        .iter()
        .flatten()
        .flat_map(|stmt| stmt.assignments.iter())
        .find_map(|assignment| match &assignment.expr {
            LayoutEvidenceExpr::Project { source, indices } => Some((source, indices)),
            LayoutEvidenceExpr::Use(_)
            | LayoutEvidenceExpr::Array { .. }
            | LayoutEvidenceExpr::Repeat { .. }
            | LayoutEvidenceExpr::Update { .. }
            | LayoutEvidenceExpr::CallResult { .. } => None,
        })
        .expect("nested indexing must emit a descriptor projection");
    assert_eq!(source, &LayoutEvidenceOperand::Local(evidence.params[0]));
    assert_eq!(indices.len(), 2);
    assert_eq!(
        evidence.locals[descriptor.index()]
            .map_ty
            .projected(indices.len())
            .expect("valid projection")
            .rank(),
        0
    );
    assert!(matches!(indices[0], LayoutEvidenceIndex::Dynamic(_)));
    assert!(matches!(indices[1], LayoutEvidenceIndex::Dynamic(_)));
    let returns = evidence
        .terminators
        .iter()
        .find_map(|terminator| (!terminator.returns.is_empty()).then_some(&terminator.returns))
        .expect("missing layout evidence return");
    assert_eq!(returns.len(), 1);
    assert!(matches!(&returns[0].value, LayoutEvidenceOperand::Local(_)));
}

#[test]
fn contract_fields_materialize_allocator_strides() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

msg Msg {
    #[selector = 1]
    Get { lane: usize, key: u256 } -> u256,
}

pub contract C {
    mut maps: [StorageMap<u256, u256>; 2],

    recv Msg {
        Get { lane, key } -> u256 uses (maps) {
            maps[lane].get(key: key)
        }
    }
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx: 0,
                arm_idx: 0,
            },
        ),
    );
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    let (source, indices) = evidence
        .statements
        .iter()
        .flatten()
        .flat_map(|stmt| stmt.assignments.iter())
        .find_map(|assignment| match &assignment.expr {
            LayoutEvidenceExpr::Project {
                source, indices, ..
            } => Some((source, indices)),
            LayoutEvidenceExpr::Use(_)
            | LayoutEvidenceExpr::Array { .. }
            | LayoutEvidenceExpr::Repeat { .. }
            | LayoutEvidenceExpr::Update { .. }
            | LayoutEvidenceExpr::CallResult { .. } => None,
        })
        .expect("contract array projection must materialize assigned evidence");

    let LayoutEvidenceOperand::Constant(source) = source else {
        panic!("contract layout must be a known descriptor")
    };
    assert_eq!(source.base, LayoutEvidenceBase::Slot(0));
    assert_eq!(source.strides.as_ref(), [1]);
    assert_eq!(indices.len(), 1);
    assert_eq!(source.map_ty.dimensions, [2]);
    assert!(matches!(indices[0], LayoutEvidenceIndex::Dynamic(_)));
}

#[test]
fn calls_pass_and_return_complete_affine_evidence() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

fn lane<const ROOT: u256>(
    maps: [[StorageMap<u256, u256, ROOT>; 3]; 2],
    row: usize,
    col: usize,
) -> StorageMap<u256, u256, ROOT> {
    maps[row][col]
}

fn read<const ROOT: u256>(
    maps: [[StorageMap<u256, u256, ROOT>; 3]; 2],
    row: usize,
    col: usize,
    key: u256,
) -> u256 {
    lane(maps: maps, row: row, col: col).get(key: key)
}
"#,
    );
    let read = find_func(&db, top_mod, "read");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(read)),
    );
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    let calls = evidence
        .statements
        .iter()
        .flatten()
        .filter_map(|stmt| stmt.call.as_ref())
        .collect::<Vec<_>>();
    let family_call = calls
        .iter()
        .find(|call| call.args.len() == 1)
        .expect("missing whole-family call evidence");

    let assignments = evidence
        .statements
        .iter()
        .flatten()
        .flat_map(|stmt| stmt.assignments.iter())
        .collect::<Vec<_>>();
    let forwarded = family_call
        .args
        .iter()
        .map(|arg| match &arg.value {
            LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Local(local)) => assignments
                .iter()
                .find_map(|assignment| (assignment.dst == *local).then_some(&assignment.expr))
                .cloned()
                .unwrap_or_else(|| arg.value.clone()),
            LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Constant(_))
            | LayoutEvidenceExpr::Project { .. }
            | LayoutEvidenceExpr::Array { .. }
            | LayoutEvidenceExpr::Repeat { .. }
            | LayoutEvidenceExpr::Update { .. }
            | LayoutEvidenceExpr::CallResult { .. } => arg.value.clone(),
        })
        .collect::<Vec<_>>();
    assert_eq!(
        forwarded,
        evidence
            .params
            .iter()
            .copied()
            .map(LayoutEvidenceOperand::Local)
            .map(LayoutEvidenceExpr::Use)
            .collect::<Vec<_>>()
    );
    assert!(
        evidence
            .statements
            .iter()
            .flatten()
            .flat_map(|stmt| stmt.assignments.iter())
            .any(|assignment| matches!(assignment.expr, LayoutEvidenceExpr::CallResult { .. }))
    );

    let normalized = normalize_semantic_body(&db, instance).expect("normalization failed");
    verify_layout_evidence_body(&db, &normalized, evidence).expect("evidence must verify");
    let mut malformed = (*evidence).clone();
    let call = malformed
        .statements
        .iter_mut()
        .flatten()
        .find_map(|statement| statement.call.as_mut().filter(|call| call.args.len() == 1))
        .expect("missing mutable family call");
    call.args = Box::new([]);
    assert!(matches!(
        verify_layout_evidence_body(&db, &normalized, &malformed),
        Err(LayoutEvidenceVerifyError::CallArgCount {
            expected: 1,
            actual: 0,
            ..
        })
    ));
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

fn pass<const ROOT: u256>(value: own Mixed<ROOT>) -> Mixed<ROOT> {
    value
}

fn forward<const ROOT: u256>(value: own Mixed<ROOT>) -> Mixed<ROOT> {
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
    let normalized = normalize_semantic_body(&db, forward).expect("normalization failed");
    let evidence = layout_evidence_body(&db, forward).expect("layoutization failed");
    let call = evidence
        .statements
        .iter()
        .flatten()
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
    let normalized = normalize_semantic_body(&db, instance).expect("normalization failed");
    let layout_normalized = normalize_semantic_body_for_layout_evidence(&db, instance)
        .expect("layout normalization failed");
    let runtime_ids = normalized
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter().map(|statement| statement.id))
        .collect::<Vec<_>>();
    let layout_ids = layout_normalized
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter().map(|statement| statement.id))
        .collect::<Vec<_>>();
    assert_eq!(runtime_ids, layout_ids);
    assert!(
        runtime_ids
            .iter()
            .enumerate()
            .all(|(idx, id)| id.index() == idx)
    );
    assert!(
        normalized
            .blocks
            .iter()
            .zip(&layout_normalized.blocks)
            .any(
                |(runtime, layout)| runtime.stmts.iter().zip(&layout.stmts).any(
                    |(runtime, layout)| {
                        runtime.id == layout.id
                            && matches!(
                                (&runtime.kind, &layout.kind),
                                (
                                    NSStmtKind::Assign {
                                        expr: NExpr::Const(_),
                                        ..
                                    },
                                    NSStmtKind::Assign {
                                        expr: NExpr::Call { .. },
                                        ..
                                    }
                                )
                            )
                    }
                )
            )
    );
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    let assignment = evidence
        .statements
        .iter()
        .flatten()
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
    let normalized = normalize_semantic_body(&db, instance).expect("normalization failed");
    assert!(normalized.blocks.iter().any(|block| {
        block.stmts.iter().any(|statement| {
            matches!(
                statement.kind,
                NSStmtKind::Assign {
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
            .flatten()
            .any(|statement| statement.call.is_some())
    );
    verify_layout_evidence_body(&db, &normalized, evidence).expect("evidence must verify");
    verify_layout_evidence_runtime_compatibility(&db, &normalized, evidence)
        .expect("evidence must match the runtime body");

    let mut reordered = normalized.clone();
    let positions = reordered
        .blocks
        .iter()
        .enumerate()
        .flat_map(|(block, data)| {
            data.stmts
                .iter()
                .enumerate()
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
    let first = reordered.blocks[*first_block].stmts[*first_statement].clone();
    let second = reordered.blocks[*second_block].stmts[*second_statement].clone();
    reordered.blocks[*first_block].stmts[*first_statement] = second;
    reordered.blocks[*second_block].stmts[*second_statement] = first;
    verify_layout_evidence_runtime_compatibility(&db, &reordered, evidence)
        .expect("statement identity must make evidence independent of statement position");

    let mut duplicate = normalized.clone();
    let duplicate_id = duplicate
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .next()
        .expect("fixture must contain a statement")
        .id;
    duplicate
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.stmts)
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
    let invalid_id = SStmtId::from_u32(evidence.statements.len() as u32);
    invalid
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.stmts)
        .next()
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
        .flatten()
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
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.stmts)
        .find_map(|statement| match &mut statement.kind {
            NSStmtKind::Assign {
                expr: NExpr::Call { callee, .. },
                ..
            } => Some(callee),
            NSStmtKind::Assign { .. } | NSStmtKind::Store { .. } => None,
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

fn recurse<const ROOT: u256>(value: own Rooted<ROOT>, depth: u256) -> Rooted<ROOT> {
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
    let normalized = normalize_semantic_body(&db, instance).expect("normalization failed");
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    verify_layout_evidence_runtime_compatibility(&db, &normalized, evidence)
        .expect("recursive call evidence must match the runtime body");
}

#[test]
fn recursive_owned_layout_carriers_do_not_cycle_never_return_analysis() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

struct Wrapped {
    maps: [StorageMap<u256, u256>; 2],
}

fn recurse(value: own Wrapped, depth: u256) -> Wrapped {
    if depth == 0 {
        return value
    }
    recurse(value, depth: depth - 1)
}

msg M {
    #[selector = 1]
    Get { depth: u256 } -> u256,
}

contract C {
    wrapped: Wrapped,

    recv M {
        Get { depth } -> u256 uses (wrapped) {
            recurse(wrapped, depth).maps[0].get(key: 0)
        }
    }
}
"#,
    );
    let recurse = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "recurse"))),
    );
    assert!(!recurse.known_never_returns(&db));
    let recv = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::ContractRecvArm {
                contract: find_contract(&db, top_mod, "C"),
                recv_idx: 0,
                arm_idx: 0,
            },
        ),
    );
    normalize_semantic_body(&db, recv).expect("normalization must not form a query cycle");
}

#[test]
fn never_return_inference_uses_refined_control_flow_and_recursive_fixed_points() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::functional::Fn

fn sccp_abort() -> u256 {
    let done = true
    if done {
        core::panic()
    }
    0
}

fn maybe_abort(flag: bool) -> u256 {
    if flag {
        core::panic()
    }
    0
}

fn leaf_abort() -> u256 {
    core::panic()
}

fn middle_abort() -> u256 {
    leaf_abort()
}

fn outer_abort() -> u256 {
    middle_abort()
}

fn direct_recursive() -> u256 {
    direct_recursive()
}

fn mutual_left() -> u256 {
    mutual_right()
}

fn mutual_right() -> u256 {
    mutual_left()
}

fn recursive_with_base(flag: bool) -> u256 {
    if flag {
        return 0
    }
    recursive_with_base(flag)
}

fn polymorphic_recursive<T>(value: T) -> u256 {
    polymorphic_recursive(value: [value])
}

fn finite_self_specialization<T>(_ value: T) -> u256 {
    finite_self_specialization(0 as u256)
}

fn finite_self_specialization_wrapper() -> u256 {
    finite_self_specialization(true)
}

fn mutually_expanding_left<T>(value: T) -> u256 {
    mutually_expanding_right(value: [value])
}

fn mutually_expanding_right<T>(value: T) -> u256 {
    mutually_expanding_left(value: [value])
}

fn generic_abort<T>(_ value: T) -> u256 {
    core::panic()
}

fn finite_generic_siblings(flag: bool) -> u256 {
    if flag {
        generic_abort(0 as u256)
    } else {
        generic_abort(true)
    }
}

fn nested_distinct_closures() -> u256 {
    let inner = || -> u256 { core::panic() }
    let outer = || -> u256 { inner.call() }
    outer.call()
}

extern {
    fn ordinary_external() -> u256
}
"#,
    );

    for name in [
        "sccp_abort",
        "leaf_abort",
        "middle_abort",
        "outer_abort",
        "direct_recursive",
        "mutual_left",
        "mutual_right",
        "finite_self_specialization_wrapper",
        "finite_generic_siblings",
        "nested_distinct_closures",
    ] {
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, name))),
        );
        assert!(
            instance.known_never_returns(&db),
            "`{name}` has no executable normal-return path"
        );
    }

    for name in ["maybe_abort", "recursive_with_base", "ordinary_external"] {
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, name))),
        );
        assert!(
            !instance.known_never_returns(&db),
            "`{name}` has an executable normal-return path"
        );
    }
    for name in ["polymorphic_recursive", "mutually_expanding_left"] {
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, name))),
        );
        assert!(
            !instance.known_never_returns(&db),
            "`{name}` must terminate analysis conservatively instead of expanding unbounded instances"
        );
    }
}

#[test]
fn non_regular_polymorphic_recursion_is_rejected_before_instance_expansion() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::functional::Fn

struct ConstRoot<const ROOT: u256> {}
struct ConstPair<const LEFT: u256, const RIGHT: u256> {}

trait Project {
    type Assoc: Project
}

extern {
    fn fabricate<T>() -> T
}

fn direct_non_regular<T>(value: T) -> u256 {
    direct_non_regular(value: [value])
}

fn mutual_non_regular_left<T>(value: T) -> u256 {
    mutual_non_regular_right(value: [value])
}

fn mutual_non_regular_right<T>(value: T) -> u256 {
    mutual_non_regular_left(value: [value])
}

fn const_non_regular<const ROOT: u256>(_ value: ConstRoot<ROOT>) -> u256 {
    let next: ConstRoot<{ ROOT + 1 }> = ConstRoot {}
    const_non_regular(value: next)
}

fn projection_non_regular<T: Project>(_ value: T) -> u256 {
    let next: T::Assoc = fabricate()
    projection_non_regular(value: next)
}

fn closure_non_regular<T>(value: T) -> u256 {
    let recurse = || -> u256 { closure_non_regular(value: [value]) }
    recurse.call()
}

fn contained_closure_non_regular<T>(value: T) -> u256 {
    let _recurse = || -> u256 { contained_closure_non_regular(value: [value]) }
    0
}

fn nested_contained_closure_non_regular<T>(_ value: T) -> u256 {
    let _outer = || -> u256 {
        let _inner = || -> u256 {
            let next: [T; 1] = fabricate()
            nested_contained_closure_non_regular(value: next)
        }
        0
    }
    0
}

fn upstream_of_non_regular_closure() -> u256 {
    closure_non_regular(value: true)
}

fn closure_regular<T>(value: T) -> u256 {
    let recurse = || -> u256 { closure_regular(value: value) }
    recurse.call()
}

fn contained_closure_regular<T>(value: T) -> u256 {
    let _recurse = || -> u256 { contained_closure_regular(value: value) }
    0
}

fn closure_concrete_specialization<T>(_ value: T) -> u256 {
    let recurse = || -> u256 { closure_concrete_specialization(value: true) }
    recurse.call()
}

fn direct_regular_permutation<T, U>(left: T, right: U) -> u256 {
    direct_regular_permutation(left: right, right: left)
}

fn mutual_regular_permutation_left<T, U>(left: T, right: U) -> u256 {
    mutual_regular_permutation_right(left: right, right: left)
}

fn mutual_regular_permutation_right<T, U>(left: T, right: U) -> u256 {
    mutual_regular_permutation_left(left: right, right: left)
}

fn const_regular_permutation<const LEFT: u256, const RIGHT: u256>(
    _ value: ConstPair<LEFT, RIGHT>,
) -> u256 {
    let next: ConstPair<RIGHT, LEFT> = ConstPair {}
    const_regular_permutation(value: next)
}

fn direct_concrete_specialization<T>(_ value: T) -> u256 {
    direct_concrete_specialization(value: true)
}

fn mutual_concrete_specialization_left<T>(_ value: T) -> u256 {
    mutual_concrete_specialization_right(value: true)
}

fn mutual_concrete_specialization_right<T>(_ value: T) -> u256 {
    mutual_concrete_specialization_left(value: 0 as u256)
}

fn const_concrete_specialization<const ROOT: u256>(_ value: ConstRoot<ROOT>) -> u256 {
    let next: ConstRoot<1> = ConstRoot {}
    const_concrete_specialization(value: next)
}

fn finite_reset_growth<T>(value: T) -> u256 {
    finite_reset_bridge(value: [value])
}

fn finite_reset_bridge<T>(_ value: T) -> u256 {
    finite_reset_root()
}

fn finite_reset_root() -> u256 {
    finite_reset_growth(value: true)
}

fn finite_partial_reset<T, U>(_ left: T, right: U) -> u256 {
    finite_partial_reset(left: [right], right: true)
}
"#,
    );

    for name in [
        "direct_non_regular",
        "mutual_non_regular_left",
        "mutual_non_regular_right",
        "const_non_regular",
        "projection_non_regular",
    ] {
        let owner = BodyOwner::Func(find_func(&db, top_mod, name));
        assert_eq!(
            non_regular_recursive_call_sites(&db, owner).len(),
            1,
            "`{name}` must have one blocked recursive source edge"
        );
    }
    for name in [
        "direct_regular_permutation",
        "mutual_regular_permutation_left",
        "mutual_regular_permutation_right",
        "const_regular_permutation",
        "closure_regular",
        "contained_closure_regular",
        "closure_concrete_specialization",
        "direct_concrete_specialization",
        "mutual_concrete_specialization_left",
        "mutual_concrete_specialization_right",
        "const_concrete_specialization",
        "finite_reset_growth",
        "finite_partial_reset",
    ] {
        let owner = BodyOwner::Func(find_func(&db, top_mod, name));
        assert!(
            non_regular_recursive_call_sites(&db, owner).is_empty(),
            "`{name}` must retain a finite recursive specialization graph"
        );
    }
    for name in ["finite_reset_root", "finite_partial_reset"] {
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, name))),
        );
        assert!(
            instance.known_never_returns(&db),
            "`{name}` must materialize its finite reset specialization family"
        );
    }
    let upstream = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "upstream_of_non_regular_closure")),
        ),
    );
    assert!(
        !upstream.known_never_returns(&db),
        "an upstream never-return query must stop conservatively at the blocked recursive component"
    );

    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert_eq!(
        rendered
            .matches("non-regular polymorphic recursion")
            .count(),
        8,
        "{rendered}"
    );
    for name in [
        "direct_non_regular",
        "mutual_non_regular_left",
        "mutual_non_regular_right",
        "const_non_regular",
        "projection_non_regular",
    ] {
        assert!(
            rendered.contains(&format!("non-regular polymorphic recursion in `fn {name}`")),
            "{rendered}"
        );
    }
    assert_eq!(
        rendered
            .matches("non-regular polymorphic recursion in `fn <closure>`")
            .count(),
        3,
        "{rendered}"
    );
}

#[test]
fn trait_dispatch_recursion_rejects_growth_but_preserves_stable_and_finite_families() {
    parse_ok!(
        db,
        top_mod,
        r#"
trait GrowingStep {
    fn step(self) -> u256
}

struct GrowingSeed {}
struct GrowingWrap<T> {
    inner: T,
}

impl GrowingStep for GrowingSeed {
    fn step(self) -> u256 {
        0
    }
}

impl<T> GrowingStep for GrowingWrap<T> {
    fn step(self) -> u256 {
        growing_expand(value: GrowingWrap { inner: self })
    }
}

fn growing_expand<T: GrowingStep>(value: T) -> u256 {
    value.step()
}

trait StableStep {
    fn step(self) -> u256
}

struct StableSeed {}
struct StableWrap<T> {
    inner: T,
}

impl StableStep for StableSeed {
    fn step(self) -> u256 {
        0
    }
}

impl<T> StableStep for StableWrap<T> {
    fn step(self) -> u256 {
        stable_expand(value: self)
    }
}

fn stable_expand<T: StableStep>(value: T) -> u256 {
    value.step()
}

fn stable_dispatch_root() -> u256 {
    stable_expand(value: StableWrap { inner: StableSeed {} })
}

trait FiniteStep {
    fn step(self) -> u256
}

struct FiniteA {}
struct FiniteB {}
struct FiniteC {}

impl FiniteStep for FiniteA {
    fn step(self) -> u256 {
        finite_dispatch(value: FiniteB {})
    }
}

impl FiniteStep for FiniteB {
    fn step(self) -> u256 {
        finite_dispatch(value: FiniteC {})
    }
}

impl FiniteStep for FiniteC {
    fn step(self) -> u256 {
        finite_dispatch(value: FiniteC {})
    }
}

fn finite_dispatch<T: FiniteStep>(value: T) -> u256 {
    value.step()
}

fn finite_dispatch_root() -> u256 {
    finite_dispatch(value: FiniteA {})
}

trait RoutedStep<Route> {
    fn step(self) -> u256
}

struct ActiveRoute {}
struct InactiveRoute {}
struct ActiveWrap<T> {
    inner: T,
}
struct InactiveWrap<T> {
    inner: T,
}

impl<T> RoutedStep<ActiveRoute> for ActiveWrap<T> {
    fn step(self) -> u256 {
        0
    }
}

impl<T> RoutedStep<InactiveRoute> for InactiveWrap<T> {
    fn step(self) -> u256 {
        active_dispatch(value: ActiveWrap { inner: self })
    }
}

fn active_dispatch<T: RoutedStep<ActiveRoute>>(value: T) -> u256 {
    value.step()
}

fn routed_dispatch_root() -> u256 {
    active_dispatch(value: ActiveWrap { inner: FiniteA {} })
}

trait DescendingStep {
    fn step(self) -> u256
}

struct DescendingSeed {}
struct DescendingWrap<T> {
    inner: T,
}

impl DescendingStep for DescendingSeed {
    fn step(self) -> u256 {
        0
    }
}

impl<T: DescendingStep> DescendingStep for DescendingWrap<T> {
    fn step(self) -> u256 {
        descending_dispatch(value: self.inner)
    }
}

fn descending_dispatch<T: DescendingStep>(value: T) -> u256 {
    value.step()
}

fn descending_dispatch_root() -> u256 {
    descending_dispatch(
        value: DescendingWrap {
            inner: DescendingWrap { inner: DescendingSeed {} },
        },
    )
}

trait AlternatingFirst {
    fn first(self) -> u256
}

trait AlternatingSecond {
    fn second(self) -> u256
}

struct AlternatingLeft<T> {
    inner: T,
}

struct AlternatingRight<T> {
    inner: T,
}

impl<T> AlternatingFirst for AlternatingLeft<T> {
    fn first(self) -> u256 {
        alternating_second(value: AlternatingRight { inner: self.inner })
    }
}

impl<T> AlternatingSecond for AlternatingRight<T> {
    fn second(self) -> u256 {
        alternating_first(value: AlternatingLeft { inner: self.inner })
    }
}

fn alternating_first<T: AlternatingFirst>(value: T) -> u256 {
    value.first()
}

fn alternating_second<T: AlternatingSecond>(value: T) -> u256 {
    value.second()
}

fn alternating_dispatch_root() -> u256 {
    alternating_first(value: AlternatingLeft { inner: FiniteA {} })
}

struct DefaultGrowthWrap<T> {
    inner: T,
}

trait DefaultGrowthStep {
    fn step(self) -> u256 {
        default_growth_expand(value: DefaultGrowthWrap { inner: self })
    }
}

impl<T> DefaultGrowthStep for DefaultGrowthWrap<T> {}

struct DefaultGrowthSeed {}

impl DefaultGrowthStep for DefaultGrowthSeed {}

fn default_growth_expand<T: DefaultGrowthStep>(value: T) -> u256 {
    value.step()
}
"#,
    );

    let growing = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "growing_expand")),
        ),
    );
    assert!(
        !growing.known_never_returns(&db),
        "an invalid dispatch family must stop never-return analysis conservatively"
    );

    for name in [
        "stable_dispatch_root",
        "finite_dispatch_root",
        "alternating_dispatch_root",
    ] {
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, name))),
        );
        assert!(
            instance.known_never_returns(&db),
            "`{name}` must retain its finite recursive specialization graph"
        );
    }
    for name in [
        "stable_expand",
        "finite_dispatch",
        "alternating_first",
        "alternating_second",
    ] {
        let owner = BodyOwner::Func(find_func(&db, top_mod, name));
        assert!(
            non_regular_recursive_call_sites(&db, owner).is_empty(),
            "`{name}` must not be rejected as a growing recursive family"
        );
    }
    let routed = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "routed_dispatch_root")),
        ),
    );
    assert!(
        !routed.known_never_returns(&db),
        "an implementation for a different trait argument must not create a dispatch edge"
    );
    assert!(
        non_regular_recursive_call_sites(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "active_dispatch")),
        )
        .is_empty(),
        "trait-argument-incompatible implementations must not form false recursive components"
    );
    let descending = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "descending_dispatch_root")),
        ),
    );
    assert!(
        !descending.known_never_returns(&db),
        "a finite decreasing blanket-impl family must reach its concrete base implementation"
    );
    assert!(
        non_regular_recursive_call_sites(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "descending_dispatch")),
        )
        .is_empty(),
        "deconstructing an impl `Self` to an exact generic coordinate is finite"
    );

    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert_eq!(
        rendered
            .matches("non-regular polymorphic recursion")
            .count(),
        2,
        "{rendered}"
    );
}

#[test]
fn trait_dispatch_preserves_method_generic_argument_flow() {
    parse_ok!(
        db,
        top_mod,
        r#"
trait StableRelay {
    fn relay<U>(self, value: U) -> u256
}

struct StableRelayImpl {}

impl StableRelay for StableRelayImpl {
    fn relay<U>(self, value: U) -> u256 {
        stable_method_dispatch(receiver: self, value: value)
    }
}

fn stable_method_dispatch<R: StableRelay, U>(receiver: R, value: U) -> u256 {
    receiver.relay(value: value)
}

trait GrowingRelay {
    fn relay<U>(self, value: U) -> u256
}

struct GrowingRelayImpl {}
struct MethodWrap<T> {
    inner: T,
}

impl GrowingRelay for GrowingRelayImpl {
    fn relay<U>(self, value: U) -> u256 {
        growing_method_dispatch(
            receiver: self,
            value: MethodWrap { inner: value },
        )
    }
}

fn growing_method_dispatch<R: GrowingRelay, U>(receiver: R, value: U) -> u256 {
    receiver.relay(value: value)
}
"#,
    );

    let stable_owner = BodyOwner::Func(find_func(&db, top_mod, "stable_method_dispatch"));
    assert!(
        non_regular_recursive_call_sites(&db, stable_owner).is_empty(),
        "forwarding a trait method's own generic argument must remain finite"
    );
    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert_eq!(
        rendered
            .matches("non-regular polymorphic recursion")
            .count(),
        1,
        "{rendered}"
    );
}

#[test]
fn concrete_dispatch_arguments_do_not_reopen_unrelated_trait_implementations() {
    parse_ok!(
        db,
        top_mod,
        r#"
trait DecodeValue {
    fn decode<D>(self, decoder: D) -> u256
}

struct Scalar {}
struct Message {}

impl DecodeValue for Scalar {
    fn decode<D>(self, decoder: D) -> u256 {
        0
    }
}

impl DecodeValue for Message {
    fn decode<D>(self, decoder: D) -> u256 {
        decode_field(value: Scalar {}, decoder: decoder)
    }
}

fn decode_field<T: DecodeValue, D>(value: T, decoder: D) -> u256 {
    value.decode(decoder: decoder)
}
"#,
    );

    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert!(
        !rendered.contains("non-regular polymorphic recursion"),
        "a concrete helper specialization must not dispatch through unrelated implementations: \
         {rendered}"
    );
}

#[test]
fn computed_ground_arguments_widen_before_source_graph_expansion() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Counter<const N: u256> {}

extern {
    fn choose() -> bool
}

fn concrete_const_seed<D>(decoder: D) -> u256 {
    let seed: Counter<0> = Counter {}
    evolving_const(value: seed, decoder: decoder)
}

fn evolving_const<const N: u256, D>(_ value: Counter<N>, decoder: D) -> u256 {
    if choose() {
        concrete_const_seed(decoder: decoder)
    } else {
        let next: Counter<{ N + 1 }> = Counter {}
        evolving_const(value: next, decoder: decoder)
    }
}
"#,
    );

    let seed_owner = BodyOwner::Func(find_func(&db, top_mod, "concrete_const_seed"));
    assert!(
        non_regular_recursive_call_sites(&db, seed_owner).is_empty(),
        "the concrete seed must not be diagnosed as the growing call site"
    );
    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert_eq!(
        rendered
            .matches("non-regular polymorphic recursion")
            .count(),
        1,
        "{rendered}"
    );
    assert!(
        rendered.contains("non-regular polymorphic recursion in `fn evolving_const`"),
        "{rendered}"
    );
}

#[test]
fn projection_bounds_do_not_hide_structural_dispatch_descent() {
    parse_ok!(
        db,
        top_mod,
        r#"
trait Backend<A> {}

trait Family {
    type Encoder: Backend<Self>
}

trait Encode<A: Family> {
    fn encode<E: Backend<A>>(self, encoder: E) -> u256
}

struct Scalar {}

impl<A: Family> Encode<A> for Scalar {
    fn encode<E: Backend<A>>(self, encoder: E) -> u256 {
        0
    }
}

impl<A: Family, T0, T1> Encode<A> for (T0, T1)
    where T0: Encode<A>, T1: Encode<A>
{
    fn encode<E: Backend<A>>(self, encoder: E) -> u256 {
        encode_field<A, T0, E>(value: self.0, encoder: encoder)
    }
}

fn encode_field<A: Family, T: Encode<A>, E: Backend<A>>(value: T, encoder: E) -> u256 {
    value.encode(encoder: encoder)
}
"#,
    );

    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert!(
        !rendered.contains("non-regular polymorphic recursion"),
        "dispatching from an aggregate to one of its generic fields is structural descent even \
         when inherited bounds contain associated projections: {rendered}"
    );
}

#[test]
fn ground_associated_projection_state_preserves_stable_abi_dispatch_components() {
    for (name, source) in [
        (
            "msg_array_arg.fe",
            include_str!("../../fe/tests/fixtures/fe_test/msg_array_arg.fe"),
        ),
        (
            "msg_nested_array_arg.fe",
            include_str!("../../fe/tests/fixtures/fe_test/msg_nested_array_arg.fe"),
        ),
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(name.into(), source);
        let (top_mod, _) = db.top_mod(file);
        let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
        let rendered = format_diagnostics(&db, &diagnostics);
        assert!(
            !rendered.contains("non-regular polymorphic recursion"),
            "ground associated projections such as the ABI encoder/decoder types are stable \
             auxiliary state in `{name}`: {rendered}"
        );
    }
}

#[test]
fn trait_dispatch_recursion_includes_local_impls_of_dependency_dispatchers() {
    fn touch(db: &mut HirAnalysisTestDb, url: &str, content: &str) -> common::file::File {
        db.workspace().touch(
            db,
            Url::parse(url).expect("valid test URL"),
            Some(content.to_string()),
        )
    }

    let mut db = HirAnalysisTestDb::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();
    touch(
        &mut db,
        "file:///nonregular-dispatch-cross-ingot/dispatch/fe.toml",
        r#"
[ingot]
name = "dispatch"
version = "0.1.0"
"#,
    );
    let dispatch_file = touch(
        &mut db,
        "file:///nonregular-dispatch-cross-ingot/dispatch/src/lib.fe",
        r#"
pub trait Step {
    fn step(self) -> u256
}

pub fn expand<T: Step>(value: T) -> u256 {
    value.step()
}
"#,
    );
    touch(
        &mut db,
        "file:///nonregular-dispatch-cross-ingot/app/fe.toml",
        r#"
[ingot]
name = "app"
version = "0.1.0"

[dependencies]
dispatch = { path = "../dispatch" }
"#,
    );
    let app_file = touch(
        &mut db,
        "file:///nonregular-dispatch-cross-ingot/app/src/lib.fe",
        r#"
use dispatch::Step
use dispatch::expand

struct Seed {}
struct Wrap<T> {
    inner: T,
}

impl Step for Seed {
    fn step(self) -> u256 {
        0
    }
}

impl<T> Step for Wrap<T> {
    fn step(self) -> u256 {
        expand(value: Wrap { inner: self })
    }
}

fn main() -> u256 {
    expand(value: Wrap { inner: Seed {} })
}
"#,
    );

    let (dispatch, _) = db.top_mod(dispatch_file);
    let (app, _) = db.top_mod(app_file);
    db.assert_no_diags(dispatch);

    let main = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, app, "main"))),
    );
    assert!(
        !main.known_never_returns(&db),
        "the application graph must stop before specializing the dependency dispatcher without bound"
    );
    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, app);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert_eq!(
        rendered
            .matches("non-regular polymorphic recursion")
            .count(),
        1,
        "{rendered}"
    );
}

#[test]
fn inherited_dependency_defaults_use_the_application_dispatch_context() {
    fn touch(db: &mut HirAnalysisTestDb, url: &str, content: &str) -> common::file::File {
        db.workspace().touch(
            db,
            Url::parse(url).expect("valid test URL"),
            Some(content.to_string()),
        )
    }

    let mut db = HirAnalysisTestDb::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();
    touch(
        &mut db,
        "file:///nonregular-default-cross-ingot/dispatch/fe.toml",
        r#"
[ingot]
name = "dispatch"
version = "0.1.0"
"#,
    );
    let dispatch_file = touch(
        &mut db,
        "file:///nonregular-default-cross-ingot/dispatch/src/lib.fe",
        r#"
pub trait DefaultStep {
    type Next: DefaultStep

    fn next(own self) -> Self::Next

    fn step(own self) -> u256 {
        default_expand(value: self.next())
    }
}

pub fn default_expand<T: DefaultStep>(value: own T) -> u256 {
    value.step()
}
"#,
    );
    touch(
        &mut db,
        "file:///nonregular-default-cross-ingot/app/fe.toml",
        r#"
[ingot]
name = "app"
version = "0.1.0"

[dependencies]
dispatch = { path = "../dispatch" }
"#,
    );
    let app_file = touch(
        &mut db,
        "file:///nonregular-default-cross-ingot/app/src/lib.fe",
        r#"
use dispatch::DefaultStep
use dispatch::default_expand

struct Seed {}

struct LocalWrap<T> {
    inner: T,
}

impl<T> DefaultStep for LocalWrap<T> {
    type Next = LocalWrap<LocalWrap<T>>

    fn next(own self) -> Self::Next {
        LocalWrap { inner: self }
    }
}

fn main() -> u256 {
    default_expand(value: LocalWrap { inner: Seed {} })
}
"#,
    );

    let (dispatch, _) = db.top_mod(dispatch_file);
    let (app, _) = db.top_mod(app_file);
    db.assert_no_diags(dispatch);

    let main = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, app, "main"))),
    );
    assert!(
        !main.known_never_returns(&db),
        "an inherited dependency default must see application-local dispatch candidates"
    );
    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, app);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert_eq!(
        rendered
            .matches("non-regular polymorphic recursion")
            .count(),
        1,
        "{rendered}"
    );
}

#[test]
fn operator_trait_dispatch_rejects_growing_specialization_families() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::ops::Add

struct OperatorSeed {}

struct OperatorWrap<T> {
    inner: T,
}

impl<T> Add for OperatorWrap<T> {
    fn add(own self, _ other: own OperatorWrap<T>) -> OperatorWrap<T> {
        let _ = operator_expand(
            left: OperatorWrap { inner: self },
            right: OperatorWrap { inner: other },
        )
        self
    }
}

fn operator_expand<T: Add<T>>(left: own T, right: own T) -> u256 {
    let _ = left + right
    0
}

fn operator_dispatch_root() -> u256 {
    operator_expand(
        left: OperatorWrap { inner: OperatorSeed {} },
        right: OperatorWrap { inner: OperatorSeed {} },
    )
}
"#,
    );

    let root = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "operator_dispatch_root")),
        ),
    );
    assert!(
        !root.known_never_returns(&db),
        "a growing operator-dispatch family must stop semantic instance expansion"
    );

    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert_eq!(
        rendered
            .matches("non-regular polymorphic recursion")
            .count(),
        1,
        "{rendered}"
    );
}

#[test]
fn operator_trait_dispatch_preserves_stable_specialization_families() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::ops::Add

struct StableOperatorSeed {}

struct StableOperatorWrap<T> {
    inner: T,
}

impl<T> Add for StableOperatorWrap<T> {
    fn add(own self, _ other: own StableOperatorWrap<T>) -> StableOperatorWrap<T> {
        let _ = stable_operator_expand(left: self, right: other)
        self
    }
}

fn stable_operator_expand<T: Add<T>>(left: own T, right: own T) -> u256 {
    let _ = left + right
    0
}

fn stable_operator_dispatch_root() -> u256 {
    stable_operator_expand(
        left: StableOperatorWrap { inner: StableOperatorSeed {} },
        right: StableOperatorWrap { inner: StableOperatorSeed {} },
    )
}
"#,
    );

    assert!(
        non_regular_recursive_call_sites(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "stable_operator_expand")),
        )
        .is_empty(),
        "a size-preserving operator-dispatch cycle must remain valid"
    );
    let root = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "stable_operator_dispatch_root")),
        ),
    );
    assert!(
        root.known_never_returns(&db),
        "the finite stable specialization graph should retain never-return inference"
    );
    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert!(
        !rendered.contains("non-regular polymorphic recursion"),
        "{rendered}"
    );
}

#[test]
fn raw_cycle_prefilter_preserves_unknown_and_module_qualified_edges() {
    parse_ok!(
        db,
        top_mod,
        r#"
// Deliberately collides with the generic parameter below. A global namespace
// spelling index must not mistake `T::grow` for this concrete `T`.
struct T {}

trait StaticGrow {
    fn grow(value: own Self) -> u256
}

struct StaticWrap<U> {
    inner: U,
}

impl<U> StaticGrow for StaticWrap<U> {
    fn grow(value: own Self) -> u256 {
        static_expand(value: StaticWrap { inner: value })
    }
}

fn static_expand<T: StaticGrow>(value: own T) -> u256 {
    T::grow(value: value)
}

struct SelfWrap<T> {
    inner: T,
}

fn self_qualified_growth<T>(value: T) -> u256 {
    self::self_qualified_growth(value: SelfWrap { inner: value })
}
"#,
    );

    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert_eq!(
        rendered
            .matches("non-regular polymorphic recursion")
            .count(),
        2,
        "{rendered}"
    );
}

#[test]
fn raw_cycle_prefilter_preserves_ufcs_alias_and_for_loop_edges() {
    parse_ok!(
        db,
        top_mod,
        r#"
use AliasGrow as GrowAlias
use core::Seq

trait AliasGrow {
    fn grow(value: own Self) -> u256
}

struct AliasSeed {}
struct AliasWrap<T> {
    inner: T,
}

impl<T> AliasGrow for AliasWrap<T> {
    fn grow(value: own Self) -> u256 {
        alias_expand(value: AliasWrap { inner: value })
    }
}

fn alias_expand<T: AliasGrow>(value: own T) -> u256 {
    <T as GrowAlias>::grow(value: value)
}

struct SeqSeed {}
struct SeqWrap<T> {
    inner: T,
}

impl<T> Seq for SeqWrap<T> {
    type Item = u256

    fn len(self) -> usize {
        let _ = seq_expand(value: SeqWrap { inner: self })
        0
    }

    fn get(self, _ index: usize) -> u256 {
        let _ = seq_expand(value: SeqWrap { inner: self })
        0
    }
}

fn seq_expand<T>(value: SeqWrap<T>) -> u256 {
    for _item in value {}
    0
}
"#,
    );

    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert_eq!(
        rendered
            .matches("non-regular polymorphic recursion")
            .count(),
        3,
        "{rendered}"
    );
    for name in ["grow", "len", "get"] {
        assert!(
            rendered.contains(&format!("non-regular polymorphic recursion in `fn {name}`")),
            "{rendered}"
        );
    }
}

#[test]
fn raw_cycle_prefilter_preserves_tuple_packed_callable_method_edges() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::functional::{Fn, FnOnce}

struct CallableSeed {}
struct CallableWrap<T> {
    inner: T,
}

impl<T> FnOnce<(u256, u256), u256> for CallableWrap<T> {
    fn call_once(self: own Self, _ args: own (u256, u256)) -> u256 {
        self.call(args.0, args.1)
    }
}

impl<T> Fn<(u256, u256), u256> for CallableWrap<T> {
    fn call(self, _ args: own (u256, u256)) -> u256 {
        invoke_callable(
            func: CallableWrap { inner: self },
            left: args.0,
            right: args.1,
        )
    }
}

fn invoke_callable<T: Fn<(u256, u256), u256>>(
    func: T,
    left: u256,
    right: u256,
) -> u256 {
    func.call(left, right)
}

fn callable_root() -> u256 {
    invoke_callable(
        func: CallableWrap { inner: CallableSeed {} },
        left: 1,
        right: 2,
    )
}
"#,
    );

    let diagnostics = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
    let rendered = format_diagnostics(&db, &diagnostics);
    assert_eq!(
        rendered
            .matches("non-regular polymorphic recursion")
            .count(),
        1,
        "{rendered}"
    );
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
        let normalized = normalize_semantic_body(&db, instance).expect("normalization failed");
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
fn output_witnesses_flow_into_constructed_array_elements() {
    assert_layoutizes(
        "output_witnesses_flow_into_constructed_array_elements.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn fresh_pair<const ROOT: u256>() -> [Rooted<ROOT>; 2] {
    [fresh(), fresh()]
}
"#,
    );
}

#[test]
fn output_witness_worklist_composes_nested_array_struct_and_enum_paths() {
    assert_layoutizes(
        "output_witness_worklist_composes_nested_array_struct_and_enum_paths.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

struct Boxed<const ROOT: u256 = _> {
    rows: [[Rooted<ROOT>; 2]; 2],
}

enum Choice<const ROOT: u256 = _> {
    One(Rooted<ROOT>),
    Many([Rooted<ROOT>; 2]),
}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn nested<const ROOT: u256>() -> Boxed<ROOT> {
    Boxed {
        rows: [[fresh(), fresh()], [fresh(), fresh()]],
    }
}

fn choice<const ROOT: u256>(one: bool) -> Choice<ROOT> {
    if one {
        Choice::One(fresh())
    } else {
        Choice::Many([fresh(), fresh()])
    }
}
"#,
    );
}

#[test]
fn output_witness_worklist_propagates_through_indexed_stores() {
    assert_layoutizes(
        "output_witness_worklist_propagates_through_indexed_stores.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn replace<const ROOT: u256>(lane: usize) -> [Rooted<ROOT>; 2] {
    let mut values = [fresh(), fresh()]
    values[lane] = fresh()
    values
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
fn one_value_cannot_consume_two_distinct_output_witness_projections() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn duplicate<const ROOT: u256>() -> [Rooted<ROOT>; 2] {
    let value = fresh()
    [value, value]
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
    assert!(rendered.contains("cannot determine inferred layout in `duplicate`"));
    assert!(rendered.contains("required to carry two different runtime layout roots"));
}

#[test]
fn refined_dead_returns_and_transfers_do_not_add_layout_context() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn choose<const ROOT: u256>() -> [Rooted<ROOT>; 2] {
    let done = false
    if done {
        let value = fresh()
        return [value, value]
    }
    [fresh(), fresh()]
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "choose"))),
    );
    let normalized =
        normalize_semantic_body_for_layout_evidence(&db, instance).expect("normalization failed");
    let reachable = normalized_cfg_reachable_blocks(&db, &normalized);
    assert!(
        normalized
            .blocks
            .iter()
            .enumerate()
            .any(|(block_idx, block)| {
                !reachable[SBlockId::new(block_idx).index()]
                    && matches!(&block.terminator.kind, NSTerminatorKind::Return(Some(_)))
            }),
        "the fixture must retain a refined-unreachable return"
    );
    layout_evidence_body(&db, instance).expect("dead layout context must not contaminate lowering");
}

#[test]
fn one_evaluation_satisfies_a_single_element_output_layout_family() {
    assert_layoutizes(
        "one_evaluation_satisfies_a_single_element_output_layout_family.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn repeat_fresh<const ROOT: u256>() -> [Rooted<ROOT>; 1] {
    [fresh(); 1]
}
"#,
    );
}

#[test]
fn one_evaluation_cannot_satisfy_an_arbitrary_output_layout_family() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn repeat_fresh<const ROOT: u256>() -> [Rooted<ROOT>; 2] {
    [fresh(); 2]
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
    assert!(rendered.contains("cannot determine inferred layout in `repeat_fresh`"));
    assert!(rendered.contains("no runtime layout root is available"));
}

#[test]
fn zero_length_arrays_do_not_require_runtime_layout_evidence() {
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
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    assert!(evidence.output.schema.components.is_empty());
    assert!(evidence.params.is_empty());
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
    values: [Rooted<ROOT>; 2],
    lane: usize,
) -> Rooted<ROOT> {
    if lane == 0 { values[0] } else { values[1] }
}

fn consume<const ROOT: u256>(values: [Rooted<ROOT>; 2], lane: usize) -> u256 {
    choose(values: values, lane: lane).root()
}
"#,
    );
}

#[test]
fn statically_unreachable_predecessors_do_not_erase_layout_evidence_definitions() {
    assert_layoutizes(
        "statically_unreachable_layout_predecessor.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

fn consume<const ROOT: u256>(_ value: Rooted<ROOT>) {}

fn known_branch<const ROOT: u256>(value: Rooted<ROOT>) {
    let known = true
    let mut selected: Rooted<ROOT>
    if known {
        selected = value
    }
    consume(value: selected)
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

fn consume<const ROOT: u256>(_ value: Rooted<ROOT>) {}

fn branch<const ROOT: u256>(
    values: [Rooted<ROOT>; 2],
    left: bool,
) {
    let selected = if left { values[0] } else { values[1] }
    consume(value: selected)
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "branch"))),
    );
    let normalized = normalize_semantic_body(&db, instance).expect("normalization failed");
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    let (branch_local, branch_index) = evidence
        .statements
        .iter()
        .flatten()
        .flat_map(|statement| &statement.assignments)
        .find_map(|assignment| match &assignment.expr {
            LayoutEvidenceExpr::Project { indices, .. } => indices.iter().find_map(|index| {
                if let LayoutEvidenceIndex::Dynamic(index) = index {
                    Some((assignment.dst, *index))
                } else {
                    None
                }
            }),
            LayoutEvidenceExpr::Use(_)
            | LayoutEvidenceExpr::Array { .. }
            | LayoutEvidenceExpr::Repeat { .. }
            | LayoutEvidenceExpr::Update { .. }
            | LayoutEvidenceExpr::CallResult { .. } => None,
        })
        .expect("missing branch-local projection evidence");
    let (call_block, call_statement, call_id) = normalized
        .blocks
        .iter()
        .enumerate()
        .find_map(|(block, data)| {
            data.stmts
                .iter()
                .enumerate()
                .find(|(_, statement)| {
                    evidence
                        .statement(statement.id)
                        .is_some_and(|statement| statement.call.is_some())
                })
                .map(|(statement, data)| (block, statement, data.id))
        })
        .expect("missing post-merge layout call");

    let mut malformed = (*evidence).clone();
    malformed.statements[call_id.index()]
        .as_mut()
        .expect("statement")
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

    let mut malformed = (*evidence).clone();
    malformed.statements[call_id.index()]
        .as_mut()
        .expect("statement")
        .call
        .as_mut()
        .expect("missing post-merge layout call")
        .args[0]
        .value = LayoutEvidenceExpr::Project {
        source: LayoutEvidenceOperand::Local(evidence.params[0]),
        indices: Box::new([LayoutEvidenceIndex::Dynamic(branch_index)]),
    };
    assert_eq!(
        verify_layout_evidence_body(&db, &normalized, &malformed),
        Err(LayoutEvidenceVerifyError::UndefinedIndexLocal {
            block: call_block,
            statement: call_statement,
            local: branch_index,
        })
    );
}

#[test]
fn indexed_assignment_prepares_destination_before_output_witness_call() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: usize = _> {}

fn index(value: usize) -> usize {
    value
}

fn fresh<const ROOT: usize>() -> Rooted<ROOT> {
    Rooted {}
}

fn replace<const ROOT: usize>(
    values: own [Rooted<ROOT>; 2],
    lane: usize,
) -> [Rooted<ROOT>; 2] {
    let mut result = values
    result[index(value: lane)] = fresh()
    let _later = index(value: lane)
    result
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "replace"))),
    );
    let normalized = normalize_semantic_body(&db, instance).expect("normalization failed");
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    let (block_idx, statement_idx, index_local) = normalized
        .blocks
        .iter()
        .enumerate()
        .find_map(|(block_idx, block)| {
            block
                .stmts
                .iter()
                .enumerate()
                .find_map(|(statement_idx, normalized_statement)| {
                    evidence
                        .statement(normalized_statement.id)
                        .and_then(|statement| statement.call.as_ref())
                        .and_then(|call| {
                            call.args.iter().find_map(|arg| match &arg.value {
                                LayoutEvidenceExpr::Project { indices, .. } => {
                                    indices.iter().find_map(|index| match index {
                                        LayoutEvidenceIndex::Dynamic(index) => {
                                            Some((block_idx, statement_idx, *index))
                                        }
                                        LayoutEvidenceIndex::Constant(_) => None,
                                    })
                                }
                                LayoutEvidenceExpr::Use(_)
                                | LayoutEvidenceExpr::Array { .. }
                                | LayoutEvidenceExpr::Repeat { .. }
                                | LayoutEvidenceExpr::Update { .. }
                                | LayoutEvidenceExpr::CallResult { .. } => None,
                            })
                        })
                })
        })
        .expect("fresh call must receive a dynamically projected output witness");
    let index_definition = normalized.blocks[block_idx]
        .stmts
        .iter()
        .position(|statement| {
            matches!(statement.kind, NSStmtKind::Assign { dst, .. } if dst == index_local)
        })
        .expect("destination index must have a semantic definition");
    assert!(
        index_definition < statement_idx,
        "destination index must be evaluated before the witnessed RHS call"
    );

    let future_index = normalized.blocks[block_idx]
        .stmts
        .iter()
        .enumerate()
        .skip(statement_idx + 1)
        .find_map(|(_, statement)| match &statement.kind {
            NSStmtKind::Assign {
                dst,
                expr: NExpr::Call { .. },
            } if normalized.locals[dst.index()].ty == normalized.locals[index_local.index()].ty => {
                Some(*dst)
            }
            NSStmtKind::Assign { .. } | NSStmtKind::Store { .. } => None,
        })
        .expect("fixture must define another usize call result after fresh");
    let mut malformed = (*evidence).clone();
    let statement_id = normalized.blocks[block_idx].stmts[statement_idx].id;
    let index = malformed.statements[statement_id.index()]
        .as_mut()
        .expect("statement")
        .call
        .as_mut()
        .and_then(|call| {
            call.args.iter_mut().find_map(|arg| match &mut arg.value {
                LayoutEvidenceExpr::Project { indices, .. } => {
                    indices.iter_mut().find(|index| {
                        matches!(index, LayoutEvidenceIndex::Dynamic(local) if *local == index_local)
                    })
                }
                LayoutEvidenceExpr::Use(_)
                | LayoutEvidenceExpr::Array { .. }
                | LayoutEvidenceExpr::Repeat { .. }
                | LayoutEvidenceExpr::Update { .. }
                | LayoutEvidenceExpr::CallResult { .. } => None,
            })
        })
        .expect("missing dynamic output-witness projection");
    *index = LayoutEvidenceIndex::Dynamic(future_index);
    assert_eq!(
        verify_layout_evidence_body(&db, &normalized, &malformed),
        Err(LayoutEvidenceVerifyError::UndefinedIndexLocal {
            block: block_idx,
            statement: statement_idx,
            local: future_index,
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
    value: own Pair<LEFT, RIGHT>,
) -> Pair<LEFT, RIGHT> {
    value
}

fn caller<const LEFT: u256, const RIGHT: u256>(
    value: own Pair<LEFT, RIGHT>,
) -> Pair<LEFT, RIGHT> {
    pass(value: value)
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "caller"))),
    );
    let normalized = normalize_semantic_body(&db, instance).expect("normalization failed");
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
        .flatten()
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
fn verifier_rejects_wrong_map_shapes_ports_and_indices() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

fn take_two<const ROOT: u256>(_ values: [Rooted<ROOT>; 2]) {}

fn call<const ROOT: u256>(
    two: [Rooted<ROOT>; 2],
    three: [Rooted<ROOT>; 3],
) {
    take_two(values: two)
}

fn first<const ROOT: u256>(values: [Rooted<ROOT>; 2]) -> Rooted<ROOT> {
    values[0]
}
"#,
    );

    let call_instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "call"))),
    );
    let call_normalized =
        normalize_semantic_body(&db, call_instance).expect("normalization failed");
    let call_evidence = layout_evidence_body(&db, call_instance).expect("layoutization failed");
    let three = call_evidence
        .params
        .iter()
        .copied()
        .find(|local| call_evidence.locals[local.index()].map_ty.dimensions == [3])
        .expect("missing length-three evidence parameter");

    let mut malformed = (*call_evidence).clone();
    let arg = malformed
        .statements
        .iter_mut()
        .flatten()
        .find_map(|statement| statement.call.as_mut())
        .and_then(|call| call.args.first_mut())
        .expect("missing layout call argument");
    arg.value = LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Local(three));
    assert!(matches!(
        verify_layout_evidence_body(&db, &call_normalized, &malformed),
        Err(LayoutEvidenceVerifyError::MapTypeMismatch)
    ));

    let mut malformed = (*call_evidence).clone();
    let arg = malformed
        .statements
        .iter_mut()
        .flatten()
        .find_map(|statement| statement.call.as_mut())
        .and_then(|call| call.args.first_mut())
        .expect("missing layout call argument");
    arg.target = CallableLayoutParamPort::Input(CallableLayoutPort {
        origin: CallableInputLayoutHoleOrigin::ValueParam(1),
        component: match &arg.target {
            CallableLayoutParamPort::Input(port) => port.component.clone(),
            CallableLayoutParamPort::OutputWitness(_) => panic!("expected input call target"),
        },
    });
    assert!(matches!(
        verify_layout_evidence_body(&db, &call_normalized, &malformed),
        Err(LayoutEvidenceVerifyError::MapTypeMismatch)
    ));

    let first_instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "first"))),
    );
    let first_normalized =
        normalize_semantic_body(&db, first_instance).expect("normalization failed");
    let first_evidence = layout_evidence_body(&db, first_instance).expect("layoutization failed");
    let mut malformed = (*first_evidence).clone();
    let index = malformed
        .statements
        .iter_mut()
        .flatten()
        .flat_map(|statement| &mut statement.assignments)
        .find_map(|assignment| match &mut assignment.expr {
            LayoutEvidenceExpr::Project { indices, .. } => indices.first_mut(),
            LayoutEvidenceExpr::Use(_)
            | LayoutEvidenceExpr::Array { .. }
            | LayoutEvidenceExpr::Repeat { .. }
            | LayoutEvidenceExpr::Update { .. }
            | LayoutEvidenceExpr::CallResult { .. } => None,
        })
        .expect("missing constant layout projection");
    *index = LayoutEvidenceIndex::Constant(2);
    assert!(matches!(
        verify_layout_evidence_body(&db, &first_normalized, &malformed),
        Err(LayoutEvidenceVerifyError::InvalidProjection)
    ));
}

#[test]
fn constructors_preserve_roots_and_array_repeat_uses_zero_stride() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

fn rebuild<const ROOT: u256>(value: Rooted<ROOT>) -> Rooted<ROOT> {
    Rooted {}
}

fn repeat<const ROOT: u256>(value: Rooted<ROOT>) -> [Rooted<ROOT>; 2] {
    [value; 2]
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

    let repeat = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "repeat"))),
    );
    let repeat = layout_evidence_body(&db, repeat).expect("repeat layoutization failed");
    let repeat_returns = repeat
        .terminators
        .iter()
        .find_map(|terminator| (!terminator.returns.is_empty()).then_some(&terminator.returns))
        .expect("missing repeat evidence return");
    assert_eq!(repeat.output.schema.components[0].rank(), 1);
    assert_eq!(repeat_returns.len(), 1);
    assert!(
        repeat
            .statements
            .iter()
            .flatten()
            .flat_map(|stmt| stmt.assignments.iter())
            .any(|assignment| matches!(
                &assignment.expr,
                LayoutEvidenceExpr::Repeat { len: 2, .. }
            ))
    );
}

#[test]
fn effect_handle_constructors_use_declared_target_evidence() {
    parse_ok!(
        db,
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

    const SPACE: AddressSpace = AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { raw }
    }

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

    const SPACE: AddressSpace = AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { marker: Rooted {}, raw }
    }

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

    const SPACE: AddressSpace = AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { value: Rooted {}, raw }
    }

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

fn from_raw_with_nested_target(raw: u256) -> Handle<Pair<1, 2>> {
    Handle::from_raw(raw)
}

fn replace_raw_with_nested_target<const LEFT: u256, const RIGHT: u256>(
    handle: mut Handle<Pair<LEFT, RIGHT>>,
    raw: u256,
) {
    handle.replace_raw(raw)
}

fn marker_at<const META: u256>(
    handles: [DualHandle<Pair<1, 2>, META>; 2],
    lane: usize,
) -> Rooted<META> {
    handles[lane].marker()
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
            .flatten()
            .flat_map(|statement| &statement.assignments)
            .any(|assignment| matches!(
                assignment.expr,
                LayoutEvidenceExpr::Use(LayoutEvidenceOperand::Local(source)) if source == *input
            ))
    );

    let from_raw_caller = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "from_raw_with_nested_target")),
        ),
    );
    let normalized = normalize_semantic_body(&db, from_raw_caller).expect("normalization failed");
    let callee = normalized
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find_map(|statement| match statement.kind {
            NSStmtKind::Assign {
                expr: NExpr::Call { callee, .. },
                ..
            } => Some(callee),
            NSStmtKind::Assign { .. } | NSStmtKind::Store { .. } => None,
        })
        .expect("missing EffectHandle::from_raw call");
    let from_raw = get_or_build_semantic_instance(&db, callee.key);
    layout_evidence_body(&db, from_raw)
        .expect("specialized EffectHandle::from_raw must preserve its target evidence opaquely");

    let replace_raw_caller = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "replace_raw_with_nested_target")),
        ),
    );
    let normalized =
        normalize_semantic_body(&db, replace_raw_caller).expect("normalization failed");
    let callee = normalized
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find_map(|statement| match statement.kind {
            NSStmtKind::Assign {
                expr: NExpr::Call { callee, .. },
                ..
            } => Some(callee),
            NSStmtKind::Assign { .. } | NSStmtKind::Store { .. } => None,
        })
        .expect("missing Handle::replace_raw call");
    let replace_raw = get_or_build_semantic_instance(&db, callee.key);
    let normalized = normalize_semantic_body(&db, replace_raw).expect("normalization failed");
    let evidence = layout_evidence_body(&db, replace_raw)
        .expect("physical EffectHandle field writes must preserve target evidence opaquely");
    let mut stores = 0;
    for block in &normalized.blocks {
        for statement in &block.stmts {
            let evidence_statement = evidence
                .statement(statement.id)
                .expect("missing statement evidence");
            if matches!(statement.kind, NSStmtKind::Store { .. }) {
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
        .expect("indexed handles must carry both physical and target layout evidence");

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
fn projections_use_the_selected_occurrence_rank() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

struct Mixed<const ROOT: u256> {
    scalar: StorageMap<u256, u256, ROOT>,
    family: [StorageMap<u256, u256, ROOT>; 2],
}

fn scalar<const ROOT: u256>(mixed: Mixed<ROOT>) -> StorageMap<u256, u256, ROOT> {
    mixed.scalar
}

fn family<const ROOT: u256>(mixed: Mixed<ROOT>) -> [StorageMap<u256, u256, ROOT>; 2] {
    mixed.family
}
"#,
    );

    for name in ["scalar", "family"] {
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, name))),
        );
        layout_evidence_body(&db, instance)
            .unwrap_or_else(|error| panic!("failed to layoutize {name}: {error:?}"));
    }
}

#[test]
fn distinct_occurrence_families_never_coalesce_after_substitution() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

struct Split<const ROOT: u256> {
    left: [StorageMap<u256, u256, ROOT>; 2],
    right: [StorageMap<u256, u256, ROOT>; 2],
}

struct Mixed<const ROOT: u256> {
    scalar: StorageMap<u256, u256, ROOT>,
    family: [StorageMap<u256, u256, ROOT>; 2],
}

fn split<const ROOT: u256>(
    left: [StorageMap<u256, u256, ROOT>; 2],
    right: [StorageMap<u256, u256, ROOT>; 2],
) -> Split<ROOT> {
    Split { left, right }
}

fn mixed<const ROOT: u256>(
    scalar: StorageMap<u256, u256, ROOT>,
    family: [StorageMap<u256, u256, ROOT>; 2],
) -> Mixed<ROOT> {
    Mixed { scalar, family }
}

fn right_first<const ROOT: u256>(value: Split<ROOT>) -> StorageMap<u256, u256, ROOT> {
    value.right[0]
}
"#,
    );

    for (name, ranks) in [
        ("split", vec![1, 1]),
        ("mixed", vec![0, 1]),
        ("right_first", vec![0]),
    ] {
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, name))),
        );
        let evidence = layout_evidence_body(&db, instance)
            .unwrap_or_else(|error| panic!("failed to layoutize {name}: {error:?}"));
        assert_eq!(
            evidence
                .output
                .schema
                .components
                .iter()
                .map(|component| component.rank())
                .collect::<Vec<_>>(),
            ranks,
        );
        assert_eq!(
            evidence
                .output
                .schema
                .components
                .iter()
                .map(|component| &component.port)
                .collect::<std::collections::HashSet<_>>()
                .len(),
            evidence.output.schema.components.len(),
        );
    }
}

#[test]
fn array_construction_supports_affine_and_dense_layout_maps() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

fn rebuild<const ROOT: u256>(
    maps: [StorageMap<u256, u256, ROOT>; 3],
) -> [StorageMap<u256, u256, ROOT>; 3] {
    [maps[0], maps[1], maps[2]]
}

fn reorder<const ROOT: u256>(
    maps: [StorageMap<u256, u256, ROOT>; 3],
) -> [StorageMap<u256, u256, ROOT>; 3] {
    [maps[0], maps[2], maps[1]]
}
"#,
    );
    for name in ["rebuild", "reorder"] {
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, name))),
        );
        let evidence = layout_evidence_body(&db, instance)
            .unwrap_or_else(|error| panic!("failed to layoutize {name}: {error:?}"));
        assert!(
            evidence
                .statements
                .iter()
                .flatten()
                .flat_map(|statement| &statement.assignments)
                .any(|assignment| matches!(
                    &assignment.expr,
                    LayoutEvidenceExpr::Array { elements } if elements.len() == 3
                ))
        );
    }
}

#[test]
fn indexed_stores_produce_layout_map_updates() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

fn replace<const ROOT: u256>(
    maps: own [StorageMap<u256, u256, ROOT>; 3],
    lane: usize,
    map: StorageMap<u256, u256, ROOT>,
) -> [StorageMap<u256, u256, ROOT>; 3] {
    let mut result = maps
    result[lane] = map
    result
}
"#,
    );
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "replace"))),
    );
    let evidence = layout_evidence_body(&db, instance).expect("layoutization failed");
    assert!(
        evidence
            .statements
            .iter()
            .flatten()
            .flat_map(|statement| &statement.assignments)
            .any(|assignment| matches!(
                &assignment.expr,
                LayoutEvidenceExpr::Update { indices, .. }
                    if indices.len() == 1
                        && matches!(indices[0], LayoutEvidenceIndex::Dynamic(_))
            ))
    );
}

#[test]
fn indexed_stores_supply_layout_context_to_fresh_values() {
    assert_layoutizes(
        "indexed_stores_supply_layout_context_to_fresh_values.fe",
        r#"
struct Rooted<const ROOT: u256 = _> {}

fn fresh<const ROOT: u256>() -> Rooted<ROOT> {
    Rooted {}
}

fn replace<const ROOT: u256>(
    values: own [Rooted<ROOT>; 3],
    lane: usize,
) -> [Rooted<ROOT>; 3] {
    let mut result = values
    result[lane] = fresh()
    result
}
"#,
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
fn mutable_inferred_root_payloads_can_form_repeated_layout_maps() {
    assert_layoutizes(
        "mutable_inferred_root_payloads_can_form_repeated_layout_maps.fe",
        r#"
use std::evm::StorageMap

enum MapChoice {
    Scalar(StorageMap<u256, u256>),
    Family([StorageMap<u256, u256>; 3]),
}

impl MapChoice {
    fn identity(own self) -> Self {
        self
    }

    fn expand(mut own self) {
        match self {
            MapChoice::Scalar(map) => self = MapChoice::Family([map, map, map]),
            MapChoice::Family(_) => {}
        }
        self = self.identity()
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
                "../../fe/tests/fixtures/fe_test/layout_root_constructed_aggregate_forwarding.fe"
            ),
        ),
        (
            "layout_root_fresh_constructor_forwarding.fe",
            include_str!(
                "../../fe/tests/fixtures/fe_test/layout_root_fresh_constructor_forwarding.fe"
            ),
        ),
        (
            "layout_root_return_index_forwarding.fe",
            include_str!("../../fe/tests/fixtures/fe_test/layout_root_return_index_forwarding.fe"),
        ),
        (
            "layout_root_enum_helper_forwarding.fe",
            include_str!("../../fe/tests/fixtures/fe_test/layout_root_enum_helper_forwarding.fe"),
        ),
        (
            "layout_root_return_effect_forwarding.fe",
            include_str!("../../fe/tests/fixtures/fe_test/layout_root_return_effect_forwarding.fe"),
        ),
        (
            "layout_root_array_enum_overlay.fe",
            include_str!("../../fe/tests/fixtures/fe_test/layout_root_array_enum_overlay.fe"),
        ),
        (
            "effect_handle_field_deref.fe",
            include_str!("../../codegen/tests/fixtures/effect_handle_field_deref.fe"),
        ),
        (
            "layout_root_aggregate_effect_forwarding.fe",
            include_str!(
                "../../fe/tests/fixtures/fe_test/layout_root_aggregate_effect_forwarding.fe"
            ),
        ),
        (
            "layout_root_nested_provider_matrix.fe",
            include_str!("../../fe/tests/fixtures/fe_test/layout_root_nested_provider_matrix.fe"),
        ),
        (
            "layout_root_recursive_forwarding.fe",
            include_str!("../../fe/tests/fixtures/fe_test/layout_root_recursive_forwarding.fe"),
        ),
        (
            "mutable_array_args_and_effects.fe",
            include_str!("../../fe/tests/fixtures/fe_test/mutable_array_args_and_effects.fe"),
        ),
        (
            "nested_provider_layout_roots.fe",
            include_str!("../../fe/tests/fixtures/fe_test/nested_provider_layout_roots.fe"),
        ),
        (
            "with_block_custom_effect.fe",
            include_str!("../../fe/tests/fixtures/fe_test/with_block_custom_effect.fe"),
        ),
    ] {
        assert_layoutizes(name, src);
    }
}

#[test]
fn stable_runtime_recursive_layout_forwarding_is_not_polymorphic_growth() {
    parse_ok!(
        db,
        top_mod,
        include_str!("../../fe/tests/fixtures/fe_test/layout_root_recursive_forwarding.fe"),
    );
    for name in ["direct_dynamic", "mutual_a", "recursive_wrapped"] {
        let sites =
            non_regular_recursive_call_sites(&db, BodyOwner::Func(find_func(&db, top_mod, name)));
        assert!(sites.is_empty(), "{name}: {sites:#?}");
    }
}

#[test]
fn specialized_array_enum_leaf_methods_bind_runtime_layout_consts() {
    parse_ok!(
        db,
        top_mod,
        include_str!("../../fe/tests/fixtures/fe_test/layout_root_array_enum_overlay.fe"),
    );
    let contract = find_contract(&db, top_mod, "C");
    let mut pending = Vec::new();
    for (recv_idx, recv) in contract.recvs(&db).data(&db).iter().enumerate() {
        for (arm_idx, _) in recv.arms.data(&db).iter().enumerate() {
            pending.push(get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(
                    &db,
                    BodyOwner::ContractRecvArm {
                        contract,
                        recv_idx: recv_idx as u32,
                        arm_idx: arm_idx as u32,
                    },
                ),
            ));
        }
    }
    let mut seen = std::collections::HashSet::new();
    let mut found = false;
    while let Some(instance) = pending.pop() {
        if !seen.insert(instance.key(&db)) || instance.key(&db).owner(&db).body(&db).is_none() {
            continue;
        }
        let evidence = layout_evidence_body(&db, instance).unwrap_or_else(|error| {
            panic!(
                "failed to layoutize reachable instance {:?}: {error:?}",
                instance.key(&db),
            )
        });
        if let BodyOwner::Func(func) = instance.key(&db).owner(&db)
            && func
                .name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "root")
            && func
                .expected_self_ty(&db)
                .is_some_and(|ty| ty.pretty_print(&db).to_string().starts_with("Slot<"))
        {
            found = true;
            assert!(
                evidence
                    .statements
                    .iter()
                    .flatten()
                    .any(|statement| !statement.const_bindings.is_empty()),
                "specialized Slot::root must bind ROOT from receiver evidence: {evidence:#?}",
            );
        }
        pending.extend(
            instance
                .callees(&db)
                .iter()
                .map(|callee| get_or_build_semantic_instance(&db, callee.key)),
        );
    }
    assert!(
        found,
        "fixture must reach a specialized Slot::root instance"
    );
}
