use common::InputDb;
use driver::DriverDataBase;
use fe_codegen::{OptLevel, emit_module_sonatina_ir, emit_runtime_package_sonatina_ir_optimized};
use hir::{analysis::ty::AddressSpace, hir_def::TopLevelMod};
use mir::runtime::RefKind;
use mir::{
    IntrinsicArithBinOp, Layout, LayoutId, PlaceElem, PlaceRoot, RExpr, RLocalId, RStmt,
    RTerminator, RuntimeBody, RuntimeBuiltin, RuntimeCarrier, RuntimeClass, RuntimeInstance,
    RuntimeLocalRoot, RuntimePackage, RuntimePlace, build_runtime_package,
    build_test_runtime_package,
};
use url::Url;

fn sonatina_function_body<'a>(ir: &'a str, name: &str) -> &'a str {
    let marker = format!("func private %{name}");
    let start = ir
        .find(&marker)
        .unwrap_or_else(|| panic!("missing function `{name}` in Sonatina IR:\n{ir}"));
    let tail = &ir[start..];
    let end = tail
        .find("\n\nfunc ")
        .or_else(|| tail.find("\n\nobject "))
        .unwrap_or(tail.len());
    &tail[..end]
}

fn sonatina_ops(body: &str) -> Vec<&str> {
    body.lines()
        .filter_map(|line| {
            let line = line.trim();
            let rhs = line.split_once(" = ")?.1;
            rhs.split_whitespace().next()
        })
        .collect()
}

fn contains_op_subsequence(body: &str, expected: &[&str]) -> bool {
    let mut ops = sonatina_ops(body).into_iter();
    expected
        .iter()
        .all(|expected| ops.any(|op| op == *expected))
}

fn with_top_mod_for_source<T>(
    name: &str,
    source: impl Into<String>,
    f: impl for<'db> FnOnce(&'db DriverDataBase, TopLevelMod<'db>) -> T,
) -> T {
    let mut db = DriverDataBase::default();
    let file_url = Url::parse(&format!("file:///{name}")).unwrap();
    db.workspace()
        .touch(&mut db, file_url.clone(), Some(source.into()));
    let file = db
        .workspace()
        .get(&db, &file_url)
        .expect("file should be loaded");
    f(&db, db.top_mod(file))
}

fn sonatina_ir_for_source(name: &str, source: impl Into<String>) -> String {
    with_top_mod_for_source(name, source, |db, top_mod| {
        emit_module_sonatina_ir(db, top_mod).expect("Sonatina IR")
    })
}

macro_rules! with_runtime_package {
    ($name:expr, $source:expr, |$db:ident, $package:ident| $body:block) => {{
        let mut $db = DriverDataBase::default();
        let file_url = Url::parse(&format!("file:///{}", $name)).unwrap();
        $db.workspace()
            .touch(&mut $db, file_url.clone(), Some($source.into()));
        let file = $db
            .workspace()
            .get(&$db, &file_url)
            .expect("file should be loaded");
        let top_mod = $db.top_mod(file);
        let $package = build_runtime_package(&$db, top_mod).expect("runtime package");
        $body
    }};
}

macro_rules! with_test_runtime_package {
    ($name:expr, $source:expr, |$db:ident, $package:ident| $body:block) => {{
        let mut $db = DriverDataBase::default();
        let file_url = Url::parse(&format!("file:///{}", $name)).unwrap();
        $db.workspace()
            .touch(&mut $db, file_url.clone(), Some($source.into()));
        let file = $db
            .workspace()
            .get(&$db, &file_url)
            .expect("file should be loaded");
        let top_mod = $db.top_mod(file);
        let $package =
            build_test_runtime_package(&$db, top_mod, None).expect("runtime test package");
        $body
    }};
}

fn runtime_body_for_symbol<'db>(
    db: &'db DriverDataBase,
    package: RuntimePackage<'db>,
    symbol: &str,
) -> RuntimeBody<'db> {
    let function = package
        .functions(db)
        .iter()
        .copied()
        .find(|function| function.symbol(db).contains(symbol))
        .unwrap_or_else(|| panic!("missing runtime function `{symbol}`"));
    function.instance(db).body(db).clone()
}

fn runtime_body_stmts<'a, 'db>(body: &'a RuntimeBody<'db>) -> impl Iterator<Item = &'a RStmt<'db>> {
    body.blocks.iter().flat_map(|block| block.stmts.iter())
}

fn body_has_object_materialization(body: &RuntimeBody<'_>) -> bool {
    runtime_body_stmts(body).any(|stmt| {
        matches!(
            stmt,
            RStmt::Assign {
                expr: RExpr::AllocObject { .. }
                    | RExpr::MaterializeToObject { .. }
                    | RExpr::MaterializePlaceToObject { .. },
                ..
            }
        )
    })
}

fn body_extracts_param_fields(body: &RuntimeBody<'_>, param: RLocalId, fields: &[u32]) -> bool {
    fields.iter().all(|field| {
        runtime_body_stmts(body).any(|stmt| {
            matches!(
                stmt,
                RStmt::Assign {
                    expr: RExpr::AggregateExtract { value, index },
                    ..
                } if *value == param && index == field
            )
        })
    })
}

fn transported_local_from_param(body: &RuntimeBody<'_>, param: RLocalId) -> RLocalId {
    runtime_body_stmts(body)
        .find_map(|stmt| match stmt {
            RStmt::Assign {
                dst,
                expr: RExpr::AddrOf { place },
            } if matches!(
                place.root,
                PlaceRoot::Ptr { addr, .. } if addr == param && place.path.is_empty()
            ) =>
            {
                Some(*dst)
            }
            RStmt::Assign {
                dst,
                expr: RExpr::RetagRef { value },
            } if *value == param => Some(*dst),
            RStmt::Assign {
                dst,
                expr: RExpr::Use(value),
            } if *value == param => Some(*dst),
            _ => None,
        })
        .unwrap_or(param)
}

fn storage_pair_ref_layout<'db>(class: &RuntimeClass<'db>) -> Option<LayoutId<'db>> {
    match class {
        RuntimeClass::Ref {
            pointee,
            kind:
                RefKind::Provider {
                    space: AddressSpace::Storage,
                    ..
                },
            ..
        } => pointee.aggregate_layout(),
        RuntimeClass::Scalar(_)
        | RuntimeClass::AggregateValue { .. }
        | RuntimeClass::RawAddr { .. }
        | RuntimeClass::Ref { .. } => None,
    }
}

#[test]
fn local_storage_handles_preserve_storage_effect_transport() {
    with_runtime_package!(
        "local_storage_handles_preserve_storage_effect_transport.fe",
        include_str!("fixtures/borrow_storage_field_handle.fe").to_string(),
        |db, package| {
            let body = runtime_body_for_symbol(&db, package, "borrow_storage_field_handle");
            let [store] = body.signature.params.as_slice() else {
                panic!("storage effect helper should have one runtime parameter:\n{body:#?}");
            };
            assert!(
                matches!(
                    store.class,
                    RuntimeClass::Ref {
                        kind: RefKind::Provider {
                            space: AddressSpace::Storage,
                            ..
                        },
                        ..
                    }
                ),
                "local storage handles must cross effect boundaries as storage providers:\n{body:#?}"
            );
        }
    );
}
#[test]
fn provider_root_trait_receivers_preserve_concrete_runtime_layouts() {
    with_runtime_package!(
        "provider_root_trait_receivers_preserve_concrete_runtime_layouts.fe",
        include_str!("fixtures/by_ref_trait_provider_storage_bug.fe").to_string(),
        |db, package| {
            let use_ctx = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db).contains("use_ctx"))
                .expect("generated use_ctx runtime function");
            let body = use_ctx.instance(&db).body(&db);
            let ctx_param = body.signature.params[0].local;

            let layout = storage_pair_ref_layout(&body.signature.params[0].class).unwrap_or_else(|| {
        panic!("use_ctx should receive its provider-bound receiver as a storage ref:\n{body:#?}")
    });
            let Layout::Struct(layout_data) = layout.data(&db) else {
                panic!("storage receiver should use the concrete Pair layout:\n{body:#?}");
            };
            assert_eq!(layout_data.fields.len(), 2);
            assert!(
                layout_data
                    .fields
                    .iter()
                    .all(|field| matches!(field, RuntimeClass::Scalar(_))),
                "Pair layout should carry two scalar fields:\n{layout_data:#?}",
            );

            let (callee, arg) = body
                .blocks
                .iter()
                .flat_map(|block| block.stmts.iter())
                .find_map(|stmt| match stmt {
                    RStmt::Assign {
                        expr: RExpr::Call { callee, args },
                        ..
                    } => args.first().map(|arg| (*callee, *arg)),
                    RStmt::Assign { .. }
                    | RStmt::AssertIndexInBounds { .. }
                    | RStmt::EnumAssertVariant { .. }
                    | RStmt::Store { .. }
                    | RStmt::CopyInto { .. }
                    | RStmt::EnumSetTag { .. }
                    | RStmt::EnumWriteVariant { .. }
                    | RStmt::End { .. } => None,
                })
                .unwrap_or_else(|| {
                    panic!("use_ctx should call the concrete trait receiver:\n{body:#?}")
                });

            let arg_class = body.value_class(arg).unwrap_or_else(|| {
                panic!("use_ctx receiver argument should be runtime-visible:\n{body:#?}")
            });
            assert_eq!(
                storage_pair_ref_layout(arg_class),
                Some(layout),
                "use_ctx should pass the storage receiver handle directly:\n{body:#?}"
            );
            let callee_signature = callee.interface_signature(&db);
            assert_eq!(
                callee_signature
                    .params
                    .first()
                    .and_then(|param| storage_pair_ref_layout(&param.class)),
                Some(layout),
                "callee should receive the same concrete storage receiver layout:\ncallee={callee_signature:#?}\ncaller={body:#?}"
            );
            assert!(
                !body
                    .blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .any(|stmt| {
                        matches!(
                            stmt,
                            RStmt::Assign {
                                expr: RExpr::Load { place },
                                ..
                            } if place.path.is_empty()
                                && (place.root == PlaceRoot::Ref(ctx_param)
                                    || matches!(place.root, PlaceRoot::Provider(_)))
                        )
                    }),
                "view receiver should not load the whole provider-bound aggregate before dispatch:\n{body:#?}"
            );
        }
    );
}

#[test]
fn view_receiver_with_storage_aggregate_keeps_receiver_runtime_visible() {
    with_runtime_package!(
        "view_receiver_with_storage_aggregate_keeps_receiver_runtime_visible.fe",
        include_str!("fixtures/by_ref_trait_provider_storage_bug.fe").to_string(),
        |db, package| {
            let sum = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db).contains("sum"))
                .expect("generated Pair::sum runtime function");
            let body = sum.instance(&db).body(&db);
            let self_param = body.signature.params.first().unwrap_or_else(|| {
                panic!("sum should keep its self view param visible:\n{body:#?}")
            });
            let Some(layout) = storage_pair_ref_layout(&self_param.class) else {
                panic!("view receiver should use ref-like transport:\n{body:#?}");
            };
            let Layout::Struct(layout_data) = layout.data(&db) else {
                panic!("view receiver should point at the stored Pair layout:\n{body:#?}");
            };
            assert_eq!(layout_data.fields.len(), 2);
            assert!(
                layout_data
                    .fields
                    .iter()
                    .all(|field| matches!(field, RuntimeClass::Scalar(_))),
                "Pair layout should carry two scalar fields:\n{layout_data:#?}",
            );
            assert!(
                body.blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .filter(|stmt| {
                        matches!(
                                stmt,
                                RStmt::Assign {
                                    expr: RExpr::Load { place },
                                    ..
                            } if place.root == PlaceRoot::Ref(self_param.local)
                                && matches!(place.path.as_ref(), [PlaceElem::Field(_)])
                        )
                    })
                    .count()
                    >= 2,
                "view receiver should project and load Pair fields through the incoming storage ref:\n{body:#?}"
            );
            assert!(
        !body
            .blocks
            .iter()
            .flat_map(|block| block.stmts.iter())
            .any(|stmt| {
                matches!(
                        stmt,
                        RStmt::Assign {
                            expr: RExpr::Load { place },
                            ..
                    } if place.root == PlaceRoot::Ref(self_param.local) && place.path.is_empty()
                )
        }),
        "view receiver should not load the whole aggregate before field projection:\n{body:#?}"
    );
        }
    );
}

#[test]
fn pointer_helpers_use_native_raw_addr_transport_in_rmir() {
    with_runtime_package!(
        "pointer_helpers_use_native_raw_addr_transport_in_rmir.fe",
        r#"
pub fn pointer_roundtrip() -> u256 {
    let ptr = core::ptr::alloc<u256>()
    unsafe { *ptr = 7 }
    unsafe { *ptr }
}
"#
        .to_string(),
        |db, package| {
            let test_fn = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db).contains("pointer_roundtrip"))
                .unwrap_or_else(|| {
                    panic!(
                        "generated pointer_roundtrip runtime function; symbols={:?}",
                        package
                            .functions(&db)
                            .iter()
                            .map(|function| function.symbol(&db))
                            .collect::<Vec<_>>()
                    )
                });
            let body = test_fn.instance(&db).body(&db);
            assert!(
                body.blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .any(|stmt| {
                        matches!(
                            stmt,
                            RStmt::Store {
                                dst: RuntimePlace {
                                    root: PlaceRoot::Ptr {
                                        space: AddressSpace::Memory,
                                        ..
                                    },
                                    ..
                                },
                                ..
                            }
                        )
                    }),
                "native pointer writes should lower to memory pointer stores:\n{body:#?}"
            );
            assert!(
                body.blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .any(|stmt| {
                        matches!(
                            stmt,
                            RStmt::Assign {
                                expr: RExpr::Load {
                                    place: RuntimePlace {
                                        root: PlaceRoot::Ptr {
                                            space: AddressSpace::Memory,
                                            ..
                                        },
                                        ..
                                    },
                                },
                                ..
                            }
                        )
                    }),
                "native pointer reads should lower to memory pointer loads:\n{body:#?}"
            );
        }
    );
}

#[test]
fn pointer_params_are_address_words() {
    with_runtime_package!(
        "pointer_params_are_address_words.fe",
        r#"
struct Data {
    value: u256,
}

fn read(data: *Data) -> u256 {
    data.value
}

fn write(data: *Data, value: u256) {
    data.value = value
}

fn identity(data: *Data) -> *Data {
    data
}

fn replace(var _ data: own *Data, other: *Data) -> u256 {
    data = other
    data.value
}

pub fn entry() -> u256 {
    let data = core::ptr::alloc<Data>()
    let other = core::ptr::alloc<Data>()
    data.value = 7
    other.value = 9
    write(data, value: 8)
    read(data: identity(data)) + replace(data, other)
}
"#
        .to_string(),
        |db, package| {
            let word_param = |body: &RuntimeBody<'_>, root: fn(&RuntimeLocalRoot<'_>) -> bool| {
                let local = body.local(body.signature.params[0].local).unwrap();
                matches!(
                    local.carrier,
                    RuntimeCarrier::Value(RuntimeClass::Scalar(_))
                ) && root(&local.root)
            };
            // A pointee place is rooted at the pointer word's address.
            let pointee_root = |body: &RuntimeBody<'_>, place: &RuntimePlace<'_>| {
                let PlaceRoot::Ptr {
                    addr,
                    space: AddressSpace::Memory,
                    ..
                } = place.root
                else {
                    return false;
                };
                let param = body.signature.params[0].local;
                matches!(place.path.as_ref(), [PlaceElem::Field(_)])
                    && runtime_body_stmts(body).any(|stmt| {
                        matches!(
                            stmt,
                            RStmt::Assign {
                                dst,
                                expr: RExpr::WordToRawAddr { value, .. },
                            } if *dst == addr && *value == param
                        )
                    })
            };

            let read = runtime_body_for_symbol(&db, package, "read");
            assert!(
                word_param(&read, |root| matches!(root, RuntimeLocalRoot::None))
                    && runtime_body_stmts(&read).any(|stmt| matches!(
                        stmt,
                        RStmt::Assign {
                            expr: RExpr::Load { place },
                            ..
                        } if pointee_root(&read, place)
                    )),
                "pointee reads should address the pointer word directly:\n{read:#?}"
            );

            let write = runtime_body_for_symbol(&db, package, "write");
            assert!(
                word_param(&write, |root| matches!(root, RuntimeLocalRoot::None))
                    && runtime_body_stmts(&write).any(|stmt| matches!(
                        stmt,
                        RStmt::Store { dst, .. } if pointee_root(&write, dst)
                    )),
                "pointee writes should not materialize a slot for the pointer value:\n{write:#?}"
            );

            let identity = runtime_body_for_symbol(&db, package, "identity");
            assert!(
                word_param(&identity, |root| matches!(root, RuntimeLocalRoot::None)),
                "pointer value reads should not load the pointee:\n{identity:#?}"
            );

            let replace = runtime_body_for_symbol(&db, package, "replace");
            assert!(
                word_param(&replace, |root| matches!(root, RuntimeLocalRoot::Slot(_))),
                "reassigned pointer values still require addressable local storage:\n{replace:#?}"
            );
        }
    );
}
#[test]
fn projected_enum_field_snapshots_preserve_full_enum_payloads() {
    let output = sonatina_ir_for_source(
        "projected_enum_field_snapshots_preserve_full_enum_payloads.fe",
        r#"enum Flag {
    A(u256),
    B(u256),
}

impl Copy for Flag {}

struct Inner {
    flag: Flag,
}

struct Wrapper {
    inner: Inner,
}

fn repack(wrapper: Wrapper) -> Inner {
    let flag = wrapper.inner.flag
    let copy = flag
    Inner { flag: copy }
}

fn entry() -> Inner {
    repack(Wrapper {
        inner: Inner { flag: Flag::A(42) },
    })
}
"#,
    );
    let repack = sonatina_function_body(&output, "repack");

    let repacks_full_payload =
        contains_op_subsequence(repack, &["extract_value", "extract_value", "insert_value"])
            || (repack.contains("obj.load") && repack.contains("obj.store"));
    assert!(
        repacks_full_payload,
        "repack should preserve the full enum payload while rebuilding the wrapper:\n{repack}"
    );
    assert!(
        !sonatina_ops(repack)
            .into_iter()
            .any(|op| op == "enum_tag" || op == "enum_tag_of"),
        "projected enum field payload should stay a full enum value, not an enum tag:\n{repack}"
    );
}

#[test]
fn read_only_scalar_params_used_as_indices_stay_unrooted() {
    let source = r#"
const VALS: [u256; 3] = [10, 20, 30]

fn sum_two(i: usize, j: usize) -> u256 {
    VALS[i] + VALS[j]
}

fn entry() -> u256 {
    sum_two(i: 0, j: 1)
}
"#;

    with_runtime_package!(
        "read_only_scalar_params_used_as_indices_stay_unrooted.fe",
        source,
        |db, package| {
            let body = runtime_body_for_symbol(&db, package, "sum_two");

            assert!(
                matches!(body.locals[0].root, RuntimeLocalRoot::None),
                "read-only scalar index param `i` should not get a runtime root:\n{body:#?}"
            );
            assert!(
                matches!(body.locals[1].root, RuntimeLocalRoot::None),
                "read-only scalar index param `j` should not get a runtime root:\n{body:#?}"
            );
        }
    );

    let ir = sonatina_ir_for_source(
        "read_only_scalar_params_used_as_indices_stay_unrooted.fe",
        source,
    );
    let sum_two = sonatina_function_body(&ir, "sum_two");

    assert!(
        !sum_two.contains("alloca i256"),
        "read-only scalar index params should not allocate stack slots:\n{sum_two}"
    );
    assert!(
        !sum_two.contains("mload"),
        "read-only scalar index params should be used directly, not reloaded:\n{sum_two}"
    );
}

#[test]
fn loop_value_arguments_do_not_force_scalar_storage() {
    with_runtime_package!(
        "loop_value_arguments_do_not_force_scalar_storage.fe",
        include_str!("fixtures/for_array.fe"),
        |db, package| {
            let body = runtime_body_for_symbol(&db, package, "for_array_sum");
            assert!(
                body.locals.iter().all(|local| !matches!(
                    local.root,
                    RuntimeLocalRoot::Slot(RuntimeClass::Scalar(_))
                )),
                "value reads and whole-local assignments must keep the loop index and sum in SSA:\n{body:#?}"
            );
        }
    );
}

#[test]
fn arrays_viewed_for_address_taking_params_are_passed_by_address() {
    with_runtime_package!(
        "arrays_viewed_for_address_taking_params_are_passed_by_address.fe",
        r#"fn sum_view(_ xs: [u256; 8]) -> u256 {
    var s = 0
    for x in xs {
        s += x
    }
    s
}

fn sum_own(_ xs: own [u256; 8]) -> u256 {
    var s = 0
    for x in xs {
        s += x
    }
    s
}

fn element(_ xs: [u256; 8], _ i: usize) -> u256 {
    xs[i]
}

fn sum_calls(_ xs: [u256; 8]) -> u256 {
    var s = 0
    var i: usize = 0
    while i < 8 {
        s += element(xs, i)
        i += 1
    }
    s
}

fn sum_local(_ a: u256) -> u256 {
    let xs: [u256; 8] = [a; 8]
    var s = 0
    for x in xs {
        s += x
    }
    s
}

fn run(_ a: u256) -> u256 {
    let xs: [u256; 8] = [a; 8]
    sum_view(xs) + sum_own(xs) + sum_calls(xs) + sum_local(a)
}

fn entry() -> u256 {
    run(1)
}
"#,
        |db, package| {
            // Copying the array into a slot of its own, as a projection's or
            // callee's receiver, would run once per element.
            for symbol in ["sum_view", "sum_own", "sum_calls", "sum_local"] {
                let body = runtime_body_for_symbol(&db, package, symbol);
                let is_array = |local: RLocalId| {
                    body.locals[local.as_u32() as usize]
                        .semantic_ty
                        .pretty_print(&db)
                        == "[u256; 8]"
                };
                assert!(
                    !runtime_body_stmts(&body).any(|stmt| matches!(
                        stmt,
                        RStmt::Assign { dst, expr: RExpr::Use(src) }
                            if is_array(*dst)
                                && is_array(*src)
                                && matches!(body.locals[dst.as_u32() as usize].root, RuntimeLocalRoot::Slot(_))
                    )),
                    "`{symbol}` should pass its array by address, not copy it per access:\n{body:#?}"
                );
            }
            let element = runtime_body_for_symbol(&db, package, "element");
            assert!(
                matches!(element.signature.params[0].class, RuntimeClass::Ref { .. }),
                "a dynamically indexed array view should arrive by address:\n{element:#?}"
            );
        }
    );
}

#[test]
fn mutated_scalar_locals_stay_rooted() {
    with_runtime_package!(
        "mutated_scalar_locals_stay_rooted.fe",
        r#"
fn bump(size: u256) -> u256 {
    var value: u256 = size
    let counter = mut value
    counter += 1
    value
}

fn entry() -> u256 {
    bump(size: 1)
}
"#,
        |db, package| {
            let body = runtime_body_for_symbol(&db, package, "bump");

            assert!(
                matches!(body.locals[0].root, RuntimeLocalRoot::None),
                "read-only scalar param should not get a runtime root:\n{body:#?}"
            );
            let rooted_scalar = body
                .locals
                .iter()
                .enumerate()
                .find_map(|(idx, local)| {
                    matches!(local.root, RuntimeLocalRoot::Slot(RuntimeClass::Scalar(_)))
                        .then(|| RLocalId::from_u32(idx as u32))
                })
                .unwrap_or_else(|| {
                    panic!("mutated scalar local should keep runtime storage:\n{body:#?}")
                });
            let rooted_ptrs = runtime_body_stmts(&body)
                .filter_map(|stmt| match stmt {
                    RStmt::Assign {
                        dst,
                        expr: RExpr::AddrOf { place },
                    } if place.root == PlaceRoot::Slot(rooted_scalar) => Some(*dst),
                    _ => None,
                })
                // The address and the copies the access binding makes of it.
                .flat_map(|rooted_ptr| {
                    std::iter::successors(Some(rooted_ptr), |&ptr| {
                        let next = transported_local_from_param(&body, ptr);
                        (next != ptr).then_some(next)
                    })
                })
                .collect::<Vec<_>>();
            assert!(
                !rooted_ptrs.is_empty(),
                "mutated scalar local should take the address of its rooted slot:\n{body:#?}"
            );
            assert!(
                runtime_body_stmts(&body).any(|stmt| {
                    matches!(
                        stmt,
                        RStmt::Store { dst, .. }
                            if matches!(dst.root, PlaceRoot::Ref(addr) if rooted_ptrs.contains(&addr))
                    )
                }),
                "mutated scalar local should store through its object reference:\n{body:#?}"
            );
        }
    );
}

#[test]
fn immutable_own_aggregate_param_field_reads_stay_unrooted() {
    with_runtime_package!(
        "immutable_own_aggregate_param_field_reads_stay_unrooted.fe",
        r#"struct Pair {
    left: u256,
    right: u256,
}

fn sum_pair(_ pair: own Pair) -> u256 {
    pair.left + pair.right
}

fn entry() -> u256 {
    sum_pair(Pair { left: 1, right: 2 })
}
"#,
        |db, package| {
            let body = runtime_body_for_symbol(&db, package, "sum_pair");
            let pair = RLocalId::from_u32(0);

            assert!(
                matches!(
                    body.signature.params[0].class,
                    RuntimeClass::AggregateValue { .. }
                ),
                "owned aggregate param should be passed as an aggregate value:\n{body:#?}"
            );
            assert!(
                matches!(body.locals[0].root, RuntimeLocalRoot::None),
                "field-read-only owned aggregate param should not get a runtime root:\n{body:#?}"
            );
            assert!(
                body_extracts_param_fields(&body, pair, &[0, 1]),
                "field reads should extract directly from the aggregate param:\n{body:#?}"
            );
            assert!(
                !body_has_object_materialization(&body),
                "field-read-only owned aggregate param should not materialize to an object:\n{body:#?}"
            );
        }
    );
}

#[test]
fn immutable_own_pointer_aggregate_param_field_reads_stay_unrooted() {
    with_runtime_package!(
        "immutable_own_pointer_aggregate_param_field_reads_stay_unrooted.fe",
        r#"use core::abi::Bytes

struct Note {
    id: u256,
    body: Bytes,
}

fn note_size(_ note: own Note) -> u256 {
    note.id + note.body.len() as u256
}

fn entry() -> u256 {
    note_size(Note { id: 1, body: Bytes::empty() })
}
"#,
        |db, package| {
            let body = runtime_body_for_symbol(&db, package, "note_size");

            assert!(
                matches!(body.locals[0].root, RuntimeLocalRoot::None),
                "a pointer field should not root a field-read-only owned aggregate param:\n{body:#?}"
            );
            assert!(
                !body_has_object_materialization(&body),
                "a pointer-holding owned aggregate param should not materialize to an object:\n{body:#?}"
            );
        }
    );
}

#[test]
fn pointer_aggregate_temporaries_stay_values() {
    with_runtime_package!(
        "pointer_aggregate_temporaries_stay_values.fe",
        r#"use core::abi::store_word
use core::ptr::MemBuffer

fn buffer_len() -> usize {
    let buffer = MemBuffer::alloc(32)
    unsafe { store_word(ptr: buffer.ptr(), value: 42) }
    buffer.span().len()
}
"#,
        |db, package| {
            let body = runtime_body_for_symbol(&db, package, "buffer_len");
            let mut buffers = body
                .locals
                .iter()
                .filter(|local| local.semantic_ty.pretty_print(&db) == "MemBuffer")
                .peekable();

            assert!(
                buffers.peek().is_some(),
                "missing MemBuffer local:\n{body:#?}"
            );
            assert!(
                buffers.all(|buffer| {
                    matches!(
                        buffer.carrier,
                        RuntimeCarrier::Value(RuntimeClass::AggregateValue { .. })
                    ) && matches!(buffer.root, RuntimeLocalRoot::None)
                }),
                "a read-only pointer-holding temporary should stay an unrooted aggregate value:\n{body:#?}"
            );
        }
    );
}

#[test]
fn read_only_mut_own_aggregate_receiver_field_reads_stay_unrooted() {
    with_runtime_package!(
        "read_only_mut_own_aggregate_receiver_field_reads_stay_unrooted.fe",
        r#"struct Pair {
    left: u256,
    right: u256,
}

impl Pair {
    fn sum(var own self) -> u256 {
        self.left + self.right
    }
}

fn entry() -> u256 {
    Pair { left: 1, right: 2 }.sum()
}
"#,
        |db, package| {
            let body = runtime_body_for_symbol(&db, package, "sum");
            let receiver = RLocalId::from_u32(0);

            assert!(
                matches!(
                    body.signature.params[0].class,
                    RuntimeClass::AggregateValue { .. }
                ),
                "mut own receiver should still be passed as an aggregate value:\n{body:#?}"
            );
            assert!(
                matches!(body.locals[0].root, RuntimeLocalRoot::None),
                "read-only mut own aggregate receiver should not get a runtime root:\n{body:#?}"
            );
            assert!(
                body_extracts_param_fields(&body, receiver, &[0, 1]),
                "receiver field reads should extract directly from the aggregate param:\n{body:#?}"
            );
            assert!(
                !body_has_object_materialization(&body),
                "read-only mut own aggregate receiver should not materialize to an object:\n{body:#?}"
            );
        }
    );
}

#[test]
fn immutable_own_tuple_destructuring_field_reads_stay_unrooted() {
    with_runtime_package!(
        "immutable_own_tuple_destructuring_field_reads_stay_unrooted.fe",
        r#"struct Pair {
    left: u256,
    right: u256,
}

fn sum_tuple(_ items: own (Pair, Pair)) -> u256 {
    let (first, second) = items
    first.left + second.right
}

fn entry() -> u256 {
    sum_tuple((Pair { left: 1, right: 2 }, Pair { left: 3, right: 4 }))
}
"#,
        |db, package| {
            let body = runtime_body_for_symbol(&db, package, "sum_tuple");
            let items = RLocalId::from_u32(0);

            assert!(
                matches!(
                    body.signature.params[0].class,
                    RuntimeClass::AggregateValue { .. }
                ),
                "owned tuple param should be passed as an aggregate value:\n{body:#?}"
            );
            assert!(
                matches!(body.locals[0].root, RuntimeLocalRoot::None),
                "field-read-only owned tuple param should not get a runtime root:\n{body:#?}"
            );
            assert!(
                body_extracts_param_fields(&body, items, &[0, 1]),
                "tuple destructuring should extract directly from the aggregate param:\n{body:#?}"
            );
            assert!(
                !body_has_object_materialization(&body),
                "tuple destructuring field reads should not materialize to an object:\n{body:#?}"
            );
        }
    );
}

#[test]
fn immutable_own_tuple_destructuring_call_args_stay_unrooted() {
    with_runtime_package!(
        "immutable_own_tuple_destructuring_call_args_stay_unrooted.fe",
        r#"struct Pair {
    left: u256,
    right: u256,
}

fn consume(_ pair: own Pair) -> u256 {
    pair.left + pair.right
}

fn sum_tuple(_ items: own (Pair, Pair)) -> u256 {
    let (first, second) = items
    consume(first) + consume(second)
}

fn entry() -> u256 {
    sum_tuple((Pair { left: 1, right: 2 }, Pair { left: 3, right: 4 }))
}
"#,
        |db, package| {
            let body = runtime_body_for_symbol(&db, package, "sum_tuple");
            let items = RLocalId::from_u32(0);

            assert!(
                matches!(
                    body.signature.params[0].class,
                    RuntimeClass::AggregateValue { .. }
                ),
                "owned tuple param should be passed as an aggregate value:\n{body:#?}"
            );
            assert!(
                matches!(body.locals[0].root, RuntimeLocalRoot::None),
                "owned tuple param should not get a runtime root for call-only destructured fields:\n{body:#?}"
            );
            assert!(
                body_extracts_param_fields(&body, items, &[0, 1]),
                "tuple destructuring call args should extract directly from the aggregate param:\n{body:#?}"
            );
            assert!(
                !body_has_object_materialization(&body),
                "tuple destructuring call args should not materialize to an object:\n{body:#?}"
            );
        }
    );
}

#[test]
fn mutating_mut_own_aggregate_receiver_stays_rooted() {
    with_runtime_package!(
        "mutating_mut_own_aggregate_receiver_stays_rooted.fe",
        r#"struct Pair {
    left: u256,
    right: u256,
}

impl Pair {
    fn bump(var own self) -> Pair {
        let left = mut self.left
        left += 1
        self
    }
}

fn entry() -> Pair {
    Pair { left: 1, right: 2 }.bump()
}
"#,
        |db, package| {
            let body = runtime_body_for_symbol(&db, package, "bump");
            let receiver = RLocalId::from_u32(0);

            assert!(
                matches!(body.locals[0].root, RuntimeLocalRoot::Slot(_)),
                "mutated mut own aggregate receiver should keep object-backed runtime storage:\n{body:#?}"
            );
            assert!(
                runtime_body_stmts(&body).any(|stmt| {
                    matches!(
                        stmt,
                        RStmt::Assign {
                            expr: RExpr::AddrOf { place },
                            ..
                        } if place.root == PlaceRoot::Slot(receiver)
                                && matches!(place.path.as_ref(), [PlaceElem::Field(_)])
                    )
                }),
                "field mutation should take the address of the rooted receiver field:\n{body:#?}"
            );
            assert!(
                runtime_body_stmts(&body).any(|stmt| {
                    matches!(
                        stmt,
                        RStmt::Store { dst, .. } if matches!(dst.root, PlaceRoot::Ref(_))
                    )
                }),
                "field mutation should store through the rooted receiver field ref:\n{body:#?}"
            );
        }
    );
}

#[test]
fn owned_aggregate_values_with_place_style_reads_get_object_backed_runtime_storage() {
    with_runtime_package!(
        "owned_aggregate_values_with_place_style_reads_get_object_backed_runtime_storage.fe",
        r#"struct Table {
    used: [u8; 4],
}

impl Table {
    fn get(self, _ slot: usize) -> u8 {
        let used = self.used
        used[slot]
    }
}

fn entry() -> u8 {
    Table { used: [1, 2, 3, 4] }.get(2)
}
"#,
        |db, package| {
            let get = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db).contains("get"))
                .expect("generated get runtime function");
            let body = get.instance(&db).body(&db);
            let used_local = body
        .locals
        .iter()
        .enumerate()
        .find_map(|(idx, local)| {
            (idx >= body.signature.params.len()
                && local.semantic_ty.pretty_print(&db) == "[u8; 4]"
                && matches!(
                    local.carrier,
                    mir::RuntimeCarrier::Value(RuntimeClass::Ref {
                        kind: RefKind::Object,
                        ..
                    })
                ))
            .then_some(RLocalId::from_u32(idx as u32))
        })
        .unwrap_or_else(|| {
            panic!(
                "owned aggregate local with later place-style reads should be object-backed:\n{body:#?}"
            )
        });

            assert!(
                body.blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .any(|stmt| {
                        matches!(
                            stmt,
                            RStmt::Assign {
                                expr: RExpr::MaterializePlaceToObject { .. }
                                    | RExpr::MaterializeToObject { .. },
                                ..
                            }
                        )
                    }),
                "owned aggregate local should materialize through object-backed lowering:\n{body:#?}"
            );
            assert!(
                body.blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .any(|stmt| {
                        matches!(
                            stmt,
                            RStmt::Assign {
                                expr: RExpr::Load { place },
                                ..
                            } if place.root == PlaceRoot::Ref(used_local)
                                && matches!(place.path.as_ref(), [PlaceElem::Index(_)])
                        )
                    }),
                "later projections should read from the owned aggregate local itself:\n{body:#?}"
            );
        }
    );
}

#[test]
fn view_receiver_field_operands_are_read_in_place() {
    with_runtime_package!(
        "view_receiver_field_operands_are_read_in_place.fe",
        r#"pub struct Pair {
    pub a: u256,
    pub b: u256,
}

impl Copy for Pair {}

impl Pair {
    pub fn check(self, _ x: u256) -> bool {
        x == self.a
    }
}

pub fn entry() -> bool {
    Pair { a: 1, b: 2 }.check(1)
}
"#,
        |db, package| {
            let check = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db).contains("check"))
                .expect("generated check runtime function");
            let body = check.instance(&db).body(&db);
            let receiver = RLocalId::from_u32(0);

            assert!(
                !body
                    .blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .any(|stmt| matches!(
                        stmt,
                        RStmt::Store { dst, .. } | RStmt::CopyInto { dst, .. }
                            if dst.root == PlaceRoot::Slot(receiver)
                    )),
                "field operands should not write back to the receiver:\n{body:#?}"
            );
            assert!(
                body.locals
                    .iter()
                    .skip(body.signature.params.len())
                    .all(|local| matches!(local.root, RuntimeLocalRoot::None)),
                "field operands should be read without rooting a copy:\n{body:#?}"
            );
        }
    );
}

#[test]
fn linear_probe_big_struct_keeps_array_projection_reads_place_based() {
    let mut db = DriverDataBase::default();
    let file_url =
        Url::parse("file:///linear_probe_big_struct_keeps_array_projection_reads_place_based.fe")
            .unwrap();
    db.workspace().touch(
        &mut db,
        file_url.clone(),
        Some(
            format!(
                "{}\nfn entry() -> u256 {{\n    var table = Table::empty()\n    table.set(17, 99)\n    table.get(17)\n}}\n",
                include_str!("../../fe/tests/fixtures/fe_test/linear_probe_big_struct.fe")
            ),
        ),
    );
    let file = db
        .workspace()
        .get(&db, &file_url)
        .expect("file should be loaded");
    let top_mod = db.top_mod(file);
    let ir = emit_module_sonatina_ir(&db, top_mod)
        .expect("linear_probe_big_struct should lower through Sonatina");
    let get_header = sonatina_function_body(&ir, "get")
        .lines()
        .next()
        .expect("get function should have a signature line");
    assert!(
        !get_header.contains("v0.@layout"),
        "view aggregate receiver should not lower to a Sonatina value aggregate param:\n{get_header}"
    );
    let package = build_runtime_package(&db, top_mod).expect("runtime package");
    let mut get_instance = None;

    for (name, expected_fields) in [("set", [true, true, false]), ("get", [true, true, true])] {
        let function = package
            .functions(&db)
            .iter()
            .copied()
            .find(|function| function.symbol(&db).contains(name))
            .unwrap_or_else(|| panic!("missing `{name}` runtime function"));
        let body = function.instance(&db).body(&db);
        if name == "get" {
            get_instance = Some(function.instance(&db));
            assert!(
                matches!(
                    body.signature.params[0].class,
                    RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. }
                ),
                "view aggregate receiver should use ref-like runtime transport:\n{body:#?}"
            );
        }
        let self_local = body.signature.params[0].local;
        let mut saw_indexed_reads = [false; 3];
        for stmt in body.blocks.iter().flat_map(|block| block.stmts.iter()) {
            let RStmt::Assign {
                expr: RExpr::Load { place },
                ..
            } = stmt
            else {
                continue;
            };
            if place.root != PlaceRoot::Ref(self_local) && place.root != PlaceRoot::Slot(self_local)
            {
                continue;
            }
            match place.path.as_ref() {
                [PlaceElem::Field(field)] if field.0 < 3 => {
                    panic!(
                        "{name} should not load whole array field {} before indexing:\n{body:#?}",
                        field.0
                    );
                }
                [PlaceElem::Field(field), PlaceElem::Index(_)] if field.0 < 3 => {
                    saw_indexed_reads[field.0 as usize] = true;
                }
                _ => {}
            }
        }
        for (idx, expected) in expected_fields.into_iter().enumerate() {
            assert_eq!(
                saw_indexed_reads[idx],
                expected,
                "{name} should {}read field {} through a composite place:\n{body:#?}",
                if expected { "" } else { "not " },
                idx
            );
        }
    }
    let get_instance = get_instance.expect("get runtime instance");
    let entry = package
        .functions(&db)
        .iter()
        .copied()
        .find(|function| function.symbol(&db).contains("entry"))
        .expect("entry runtime function");
    let entry_body = entry.instance(&db).body(&db);
    let mut saw_get_call = false;
    for stmt in entry_body
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
    {
        let RStmt::Assign {
            expr: RExpr::Call { callee, args },
            ..
        } = stmt
        else {
            continue;
        };
        if *callee != get_instance {
            continue;
        }
        saw_get_call = true;
        assert!(
            !matches!(
                entry_body.value_class(args[0]),
                Some(RuntimeClass::AggregateValue { .. })
            ),
            "caller should not materialize a whole aggregate before calling get:\n{entry_body:#?}"
        );
    }
    assert!(
        saw_get_call,
        "entry should contain a direct call to get:\n{entry_body:#?}"
    );
}

#[test]
fn materialized_scalar_uses_do_not_keep_rawaddr_runtime_carriers() {
    with_runtime_package!(
        "materialized_scalar_uses_do_not_keep_rawaddr_runtime_carriers.fe",
        r#"fn bump(_ x: mut u8) -> u8 {
    x += 1
    x
}

fn entry() -> u8 {
    var x: u8 = 2
    bump(mut x)
}
"#,
        |db, package| {
            let bump = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db).contains("bump"))
                .expect("generated bump runtime function");
            let body = bump.instance(&db).body(&db);
            let param_count = body.signature.params.len();
            let rawaddr_temps = body
                .locals
                .iter()
                .enumerate()
                .skip(param_count)
                .filter_map(|(idx, local)| {
                    matches!(
                        local.carrier,
                        mir::RuntimeCarrier::Value(RuntimeClass::RawAddr { pointee: None, .. })
                    )
                    .then_some(RLocalId::from_u32(idx as u32))
                })
                .collect::<Vec<_>>();

            assert!(
                body.blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .any(|stmt| {
                        matches!(
                            stmt,
                            RStmt::Assign {
                                expr: RExpr::Load { .. },
                                ..
                            }
                        )
                    }),
                "materializing a mutable scalar use should load from its place root:\n{body:#?}"
            );
            assert!(
                rawaddr_temps.iter().all(|temp| {
                    body.blocks
                        .iter()
                        .flat_map(|block| block.stmts.iter())
                        .any(|stmt| {
                            matches!(
                                stmt,
                                RStmt::Assign {
                                    expr:
                                        RExpr::Load {
                                            place:
                                                mir::RuntimePlace {
                                                    root:
                                                        PlaceRoot::Ptr {
                                                            addr,
                                                            class: RuntimeClass::Scalar(_),
                                                            ..
                                                        },
                                                    ..
                                                },
                                        },
                                    ..
                                } if addr == temp
                            )
                        })
                }),
                "raw-address scalar temps must be materialized through scalar loads before use:\n{body:#?}"
            );
        }
    );
}

#[test]
fn checked_extern_intrinsics_do_not_become_runtime_functions() {
    with_runtime_package!(
        "checked_extern_intrinsics_do_not_become_runtime_functions.fe",
        r#"
fn add(a: u256, b: u256) -> u256 {
    a + b
}

fn neg(x: i256) -> i256 {
    -x
}

fn entry_add() -> u256 {
    add(1, 2)
}

fn entry_neg() -> i256 {
    neg(-3)
}
"#,
        |db, package| {
            assert!(
                package
                    .functions(&db)
                    .iter()
                    .all(|function| !function.symbol(&db).contains("__checked_")),
                "checked extern intrinsics should lower directly through rMIR expressions, not as runtime functions:\n{:#?}",
                package.functions(&db)
            );
        }
    );
}

#[test]
fn empty_revert_intrinsic_has_no_synthetic_pointer_operand() {
    with_runtime_package!(
        "empty_revert_intrinsic_has_no_synthetic_pointer_operand.fe",
        r#"
use std::abi::bytes_from_words_prefix

fn trigger() {
    let _ = bytes_from_words_prefix<33, 1>([0])
}
"#,
        |db, package| {
            let body = runtime_body_for_symbol(&db, package, "bytes_from_words_prefix");
            assert!(
                body.blocks
                    .iter()
                    .any(|block| matches!(block.terminator, RTerminator::RevertEmpty)),
                "empty stdlib reverts should remain operand-free through rMIR:\n{body:#?}"
            );

            let functions = package.functions(&db);
            let function = functions
                .iter()
                .find(|function| function.symbol(&db).contains("bytes_from_words_prefix"))
                .expect("instantiated bytes_from_words_prefix function");
            let symbol = function.symbol(&db);
            let output = emit_runtime_package_sonatina_ir_optimized(&db, &package, OptLevel::O0)
                .expect("Sonatina IR");
            let function_ir = sonatina_function_body(&output, symbol);
            assert!(
                function_ir.contains("evm_revert 0.i256 0.i256")
                    && !function_ir.contains("evm_malloc 0.i256"),
                "empty revert lowering must not manufacture a pointer:\n{function_ir}"
            );
        }
    );
}

#[test]
fn whole_handle_loads_materialize_values_before_rebinding_object_locals() {
    with_runtime_package!(
        "whole_handle_loads_materialize_values_before_rebinding_object_locals.fe",
        include_str!("../../fe/tests/fixtures/fe_test/poseidon_mock.fe").to_string(),
        |db, package| {
            for function in package.functions(&db) {
                let body = function.instance(&db).body(&db);
                assert!(
                    !body
                        .blocks
                        .iter()
                        .flat_map(|block| block.stmts.iter())
                        .any(|stmt| {
                            matches!(
                                stmt,
                                RStmt::Assign {
                                    dst,
                                    expr:
                                        RExpr::Load {
                                            place:
                                                mir::RuntimePlace {
                                                    root: PlaceRoot::Ref(_),
                                                    path,
                                                },
                                        },
                                } if path.is_empty()
                                    && matches!(
                                        body.locals[dst.as_u32() as usize].carrier,
                                        mir::RuntimeCarrier::Value(mir::RuntimeClass::Ref { .. })
                                    )
                            )
                        }),
                    "whole-handle loads must materialize aggregate values before rebinding handle locals:\n{body:#?}"
                );
            }
        }
    );
}

#[test]
fn by_value_enum_constants_do_not_become_const_regions() {
    with_runtime_package!(
        "by_value_enum_constants_do_not_become_const_regions.fe",
        r#"enum E { A, B }

fn select(flag: bool) -> E {
    if flag {
        E::A
    } else {
        E::B
    }
}

fn entry() -> u8 {
    match select(true) {
        E::A => 1,
        E::B => 0,
    }
}"#,
        |db, package| {
            let const_handle_bodies = package
                .functions(&db)
                .iter()
                .filter_map(|function| {
                    let body = function.instance(&db).body(&db);
                    body.blocks
                        .iter()
                        .flat_map(|block| block.stmts.iter())
                        .any(|stmt| {
                            matches!(
                                stmt,
                                RStmt::Assign {
                                    expr: RExpr::ConstRef { .. },
                                    ..
                                }
                            )
                        })
                        .then(|| format!("{}:\n{body:#?}", function.symbol(&db)))
                })
                .collect::<Vec<_>>();

            assert!(
                package.const_regions(&db).is_empty(),
                "by-value enum constants should not create package const regions:\nregions={:#?}\n{}",
                package.const_regions(&db),
                const_handle_bodies.join("\n\n")
            );
            assert!(
                const_handle_bodies.is_empty(),
                "by-value enum constants should lower inline, not through ConstRef:\n{}",
                const_handle_bodies.join("\n\n")
            );
            assert!(
                package.functions(&db).iter().any(|function| {
                    let body = function.instance(&db).body(&db);
                    body.blocks
                        .iter()
                        .flat_map(|block| block.stmts.iter())
                        .any(|stmt| {
                            matches!(
                                stmt,
                                RStmt::Assign {
                                    expr: RExpr::EnumMake { .. },
                                    ..
                                }
                            )
                        })
                }),
                "enum constants should lower through EnumMake in value position",
            );
        }
    );
}

#[test]
fn immutable_code_view_parameter_forwarding_does_not_load_aggregate() {
    let ir = sonatina_ir_for_source(
        "immutable_code_view_parameter_forwarding.fe",
        r#"
struct Env {
    first: u256,
    second: u256,
    third: u256,
}
impl Copy for Env {}

fn pick(_ env: Env, _ which: u256) -> u256 {
    if which == env.second { env.third } else { env.first }
}

fn forward(_ env: Env, _ which: u256) -> u256 {
    pick(env, which)
}

msg Query {
    #[selector = 1]
    Read { which: u256 } -> u256,
}

pub contract CodeView {
    env: Env,
    init() uses (mut env) {
        env = Env { first: 11, second: 22, third: 33 }
    }
    recv Query {
        Read { which } -> u256 uses (env) { forward(env, which) }
    }
}
"#,
    );
    let forward = sonatina_function_body(&ir, "forward");
    assert!(
        !forward.contains("evm_code_copy")
            && !forward.contains("evm_code_load")
            && !forward.contains("insert_value"),
        "forwarding an immutable code view should retain its address:\n{ir}"
    );
    let pick = sonatina_function_body(&ir, "pick");
    assert!(
        pick.contains("evm_code_load"),
        "fields should be read from code in the consuming helper:\n{ir}"
    );
}

#[test]
fn immutable_array_snapshots_retain_their_backing() {
    let cases = [
        (
            r#"
const VALUES: [u256; 3] = [11, 22, 33]
pub fn entry() -> u256 { forward(VALUES) }
"#,
            "const.load",
        ),
        (
            r#"
fn read_input() -> u256 uses (values: [u256; 3]) { forward(values) }
pub fn entry() -> u256 uses (evm: std::evm::RawOps) {
    let data = unsafe { evm.calldata_ptr<[u256; 3]>(0) }
    unsafe { with (*data) { read_input() } }
}
"#,
            "evm_calldata_load",
        ),
    ];
    for (entry, load) in cases {
        let ir = sonatina_ir_for_source(
            "immutable_array_snapshots.fe",
            format!(
                r#"
fn pick(_ values: [u256; 3]) -> u256 {{ values[1] }}
fn forward(_ values: [u256; 3]) -> u256 {{ pick(values) }}
{entry}
"#
            ),
        );
        let forward = sonatina_function_body(&ir, "forward");
        assert!(
            !forward.contains(load) && !forward.contains("insert_value"),
            "immutable array forwarding should retain its backing:\n{ir}"
        );
        let pick = sonatina_function_body(&ir, "pick");
        assert!(
            pick.contains(load),
            "the consuming helper should load the field:\n{ir}"
        );
    }
}

#[test]
fn immutable_enum_snapshots_preserve_tag_validation() {
    let ir = sonatina_ir_for_source(
        "immutable_enum_snapshots.fe",
        r#"
enum Choice { A, B }
impl Copy for Choice {}
struct Input { first: u256, choices: [Choice; 2] }
impl Copy for Input {}
fn pick(_ input: Input) -> u256 { input.first }
fn forward(_ input: Input) -> u256 { pick(input) }
fn read_input() -> u256 uses (input: Input) { forward(input) }
pub fn entry() -> u256 uses (evm: std::evm::RawOps) {
    let data = unsafe { evm.calldata_ptr<Input>(0) }
    unsafe { with (*data) { read_input() } }
}
"#,
    );
    // A `Copy` view argument is a copy: copying untrusted calldata validates
    // its enum tags before the call.
    let read_input = sonatina_function_body(&ir, "read_input");
    assert!(
        read_input.contains("unreachable"),
        "snapshotting nested enums must still validate their tags before the call:\n{ir}"
    );
}

#[test]
fn const_backed_array_args_lower_as_const_refs_in_sonatina() {
    let cases = [
        (
            "const_backed_local_borrows_lower_as_const_refs_in_sonatina.fe",
            r#"
const C: [u256; 4] = [11, 22, 33, 44]

fn pick(values: ref [u256; 4], idx: usize) -> u256 {
    values[idx]
}

pub fn entry() -> u256 {
    let values: [u256; 4] = C
    let idx: usize = 2
    pick(values: ref values, idx)
}
"#,
            "borrowed const-backed array",
        ),
        (
            "const_backed_view_params_lower_as_const_refs_in_sonatina.fe",
            r#"
const C: [u256; 4] = [11, 22, 33, 44]

fn pick(values: [u256; 4], idx: usize) -> u256 {
    values[idx]
}

pub fn entry() -> u256 {
    let values: [u256; 4] = C
    let idx: usize = 2
    pick(values, idx)
}
"#,
            "view array parameter",
        ),
    ];

    for (name, source, label) in cases {
        let ir = sonatina_ir_for_source(name, source);
        let pick = sonatina_function_body(&ir, "pick");

        assert!(
            pick.contains("constref<[i256; 4]>"),
            "{label} should be passed as a const ref:\n{ir}"
        );
        assert!(
            pick.contains("const.index") && pick.contains("const.load"),
            "{label} should load through const data projections:\n{pick}"
        );
        assert!(
            !ir.contains("[i256; 4] = const.load"),
            "{label} should not load the full array:\n{ir}"
        );
    }
}

#[test]
fn mutable_with_provider_reuses_materialized_root_for_later_readonly_effects() {
    with_test_runtime_package!(
        "mutable_with_provider_reuses_materialized_root_for_later_readonly_effects.fe",
        include_str!("../../fe/tests/fixtures/fe_test/with_block_custom_effect.fe").to_string(),
        |db, package| {
            let incs = package
                .functions(&db)
                .iter()
                .copied()
                .filter(|function| function.symbol(&db).contains("inc"))
                .map(|function| function.instance(&db))
                .collect::<Vec<_>>();
            let reads = package
                .functions(&db)
                .iter()
                .copied()
                .filter(|function| function.symbol(&db).contains("read"))
                .map(|function| function.instance(&db))
                .collect::<Vec<_>>();
            assert!(!incs.is_empty(), "missing inc runtime function");
            assert!(!reads.is_empty(), "missing read runtime function");
            let test = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db) == "test_with_block_custom_effect")
                .expect("test_with_block_custom_effect runtime function");
            let body = test.instance(&db).body(&db);
            let call_arg = |callees: &[RuntimeInstance<'_>]| {
                body.blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .find_map(|stmt| match stmt {
                        RStmt::Assign {
                            expr: RExpr::Call { callee, args },
                            ..
                        } if callees.contains(callee) => Some(args[0]),
                        _ => None,
                    })
                    .unwrap_or_else(|| panic!("missing call in test body:\n{body:#?}"))
            };
            let provider_root = |arg| {
                if let Some(root) = body
                    .blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .find_map(|stmt| match stmt {
                        RStmt::Assign {
                            dst,
                            expr: RExpr::AddrOf { place },
                        } if *dst == arg && place.path.is_empty() => match place.root {
                            PlaceRoot::Ref(root) | PlaceRoot::Slot(root) => Some(root),
                            PlaceRoot::Ptr { .. } | PlaceRoot::Provider(_) => None,
                        },
                        _ => None,
                    })
                {
                    return root;
                }
                if matches!(
                    body.value_class(arg),
                    Some(RuntimeClass::Ref {
                        kind: RefKind::Object,
                        ..
                    })
                ) {
                    return arg;
                }
                panic!("call arg should carry or address the provider root:\n{body:#?}")
            };

            let inc_root = provider_root(call_arg(&incs));
            let read_arg = call_arg(&reads);
            let read_root = provider_root(read_arg);

            assert_eq!(
                inc_root, read_root,
                "mutable and readonly uses of the same with-provider should share one root:\n{body:#?}"
            );
            assert!(
                matches!(
                    body.locals[inc_root.as_u32() as usize].root,
                    RuntimeLocalRoot::Slot(RuntimeClass::AggregateValue { .. })
                ),
                "shared mutable with-provider root should be object-backed:\n{body:#?}"
            );
            assert!(
                matches!(
                    body.value_class(read_arg),
                    Some(RuntimeClass::Ref {
                        kind: RefKind::Object,
                        ..
                    })
                ),
                "later readonly effect should read from the object-backed provider root, not the original const ref:\n{body:#?}"
            );
        }
    );
}

#[test]
fn readonly_with_provider_can_remain_const_backed() {
    with_test_runtime_package!(
        "readonly_with_provider_can_remain_const_backed.fe",
        // Two words: a one-word constant struct is passed as the plain word.
        r#"pub struct Counter {
    pub value: u256,
    pub step: u256,
}

fn read() -> u256 uses (counter: Counter) {
    counter.value
}

#[test]
fn test_readonly_with_provider() {
    let out: u256 = with (Counter = Counter { value: 7, step: 1 }) {
        read() + read()
    }
    let other: u256 = with (Counter = { Counter { value: 9, step: 1 } }) {
        read() + read()
    }

    assert!(out == 14)
    assert!(other == 18)
}"#,
        |db, package| {
            let reads = package
                .functions(&db)
                .iter()
                .copied()
                .filter(|function| {
                    let symbol = function.symbol(&db);
                    symbol == "read" || symbol.starts_with("read_")
                })
                .map(|function| function.instance(&db))
                .collect::<Vec<_>>();
            assert!(!reads.is_empty(), "read runtime functions");
            let test = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db) == "test_readonly_with_provider")
                .expect("test runtime function");
            let body = test.instance(&db).body(&db);
            let read_args = body
                .blocks
                .iter()
                .flat_map(|block| block.stmts.iter())
                .filter_map(|stmt| match stmt {
                    RStmt::Assign {
                        expr: RExpr::Call { callee, args },
                        ..
                    } if reads.contains(callee) => Some(args[0]),
                    _ => None,
                })
                .collect::<Vec<_>>();

            assert_eq!(
                read_args.len(),
                4,
                "readonly with-provider test should read each provider twice:\n{body:#?}"
            );
            assert!(
                read_args.iter().all(|arg| matches!(
                    body.value_class(*arg),
                    Some(RuntimeClass::Ref {
                        kind: RefKind::Const,
                        ..
                    })
                )),
                "readonly-only with-provider should stay const-backed:\n{body:#?}"
            );
            assert!(
                !body
                    .blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .any(|stmt| matches!(
                        stmt,
                        RStmt::Assign {
                            expr: RExpr::MaterializeToObject { .. } | RExpr::AllocObject { .. },
                            ..
                        }
                    )),
                "readonly-only with-provider should not materialize the aggregate provider root:\n{body:#?}"
            );
        }
    );
}

#[test]
fn borrows_of_aggregate_constant_locals_lower_as_const_refs() {
    with_runtime_package!(
        "borrows_of_aggregate_constant_locals_lower_as_const_refs.fe",
        r#"fn first(xs: ref [u256; 2]) -> u256 {
    xs[0]
}

fn entry() -> u256 {
    let xs: [u256; 2] = [10, 20]
    first(xs: ref xs)
}"#,
        |db, package| {
            let first = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db) == "first")
                .expect("first runtime function")
                .instance(&db);
            let entry = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db) == "entry")
                .expect("entry runtime function");
            let body = entry.instance(&db).body(&db);
            let first_args = body
                .blocks
                .iter()
                .flat_map(|block| block.stmts.iter())
                .filter_map(|stmt| match stmt {
                    RStmt::Assign {
                        expr: RExpr::Call { callee, args },
                        ..
                    } if *callee == first => Some(args[0]),
                    _ => None,
                })
                .collect::<Vec<_>>();

            assert_eq!(
                first_args.len(),
                1,
                "entry should call first exactly once:\n{body:#?}"
            );
            assert!(
                matches!(
                    body.value_class(first_args[0]),
                    Some(RuntimeClass::Ref {
                        kind: RefKind::Const,
                        ..
                    })
                ),
                "borrow-typed aggregate literal should be passed as a const ref:\n{body:#?}"
            );
            assert!(
                !body
                    .blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .any(|stmt| matches!(
                        stmt,
                        RStmt::Assign {
                            expr: RExpr::MaterializeToObject { .. } | RExpr::AllocObject { .. },
                            ..
                        }
                    )),
                "borrow-typed aggregate literal should not materialize before the call:\n{body:#?}"
            );
        }
    );
}

#[test]
fn sonatina_enum_tag_matches_preserve_typed_tag_values() {
    let _ = sonatina_ir_for_source(
        "sonatina_enum_tag_matches_preserve_typed_tag_values.fe",
        r#"enum Maybe {
    None,
    Some(u256),
}

pub fn code(x: Maybe) -> u256 {
    match x {
        Maybe::None => 0,
        Maybe::Some(v) => v,
    }
}

pub fn main() -> u256 {
    code(Maybe::Some(1))
}"#,
    );
}

#[test]
fn object_backed_scalar_field_borrows_lower_as_typed_refs() {
    with_runtime_package!(
        "object_backed_scalar_field_borrows_lower_as_typed_refs.fe",
        include_str!("fixtures/effect_handle_field_deref.fe").to_string(),
        |db, package| {
            let mut saw_scalar_ref_borrow = false;
            for function in package.functions(&db) {
                let body = function.instance(&db).body(&db);
                for stmt in body.blocks.iter().flat_map(|block| block.stmts.iter()) {
                    let RStmt::Assign {
                        dst,
                        expr: RExpr::AddrOf { place },
                    } = stmt
                    else {
                        continue;
                    };
                    if !matches!(place.root, PlaceRoot::Ref(_)) {
                        continue;
                    }
                    let Some(RuntimeClass::Ref { pointee, kind, .. }) = body.value_class(*dst)
                    else {
                        continue;
                    };
                    if !matches!(**pointee, RuntimeClass::Scalar(_)) {
                        continue;
                    }
                    assert!(
                        matches!(
                            kind,
                            RefKind::Object
                                | RefKind::Provider { .. }
                                | RefKind::Const
                                | RefKind::Native
                        ),
                        "scalar field borrow should lower as a typed ref, not a raw address:\n{body:#?}"
                    );
                    saw_scalar_ref_borrow = true;
                }
            }

            assert!(
                saw_scalar_ref_borrow,
                "expected at least one object-backed scalar field borrow to lower as RuntimeClass::Ref"
            );
        }
    );
}

#[test]
fn checked_overflow_exprs_survive_into_rmir() {
    with_runtime_package!(
        "debug_checked_add_unused_local_rmir.fe",
        r#"
fn test_add_overflow_u8() {
    let x: u8 = 255
    let y: u8 = x + 1
}
"#,
        |db, package| {
            let func = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db).contains("test_add_overflow_u8"))
                .expect("generated runtime function");
            let body = func.instance(&db).body(&db);
            assert!(
                body.blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .any(|stmt| {
                        matches!(
                            stmt,
                            RStmt::Assign {
                                expr: RExpr::Call { .. }
                                    | RExpr::Builtin(RuntimeBuiltin::IntrinsicArith {
                                        op: IntrinsicArithBinOp::Add,
                                        checked: true,
                                        ..
                                    }),
                                ..
                            }
                        )
                    }),
                "checked overflow expression should survive semantic const canonicalization and lower to executable rMIR arithmetic:\n{body:#?}"
            );
        }
    );
}

#[test]
fn unit_branch_mutations_do_not_use_erased_call_results() {
    with_runtime_package!(
        "unit_branch_mutations_do_not_use_erased_call_results.fe",
        r#"
#[test]
fn unit_branch_add_assign() {
    var x: usize = 0
    if true {
        x += 1
    } else {
        x += 1
    }
}
"#,
        |db, package| {
            for function in package.functions(&db) {
                let body = function.instance(&db).body(&db);
                assert!(
                    !body
                        .blocks
                        .iter()
                        .flat_map(|block| block.stmts.iter())
                        .any(|stmt| {
                            matches!(
                                stmt,
                                RStmt::Assign {
                                    expr: RExpr::Use(value),
                                    ..
                                } if body.value_class(*value).is_none()
                            )
                        }),
                    "unit-valued mutation branches should not try to use erased call results:\n{}:\n{body:#?}",
                    function.symbol(&db),
                );
            }
        }
    );
}

#[test]
fn by_value_array_returns_keep_visible_aggregate_signatures() {
    with_runtime_package!(
        "by_value_array_returns_keep_visible_aggregate_signatures.fe",
        r#"
fn return_array_after_projection(xs: [u8; 4]) -> [u8; 4] {
    let ys = xs
    let _ = ys[2]
    ys
}

fn use_returned_array() -> u8 {
    var xs: [u8; 4] = [1, 2, 3, 4]
    xs = return_array_after_projection(xs)
    xs[0]
}
"#,
        |db, package| {
            let callee = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| {
                    function
                        .symbol(&db)
                        .contains("return_array_after_projection")
                })
                .expect("return_array_after_projection runtime function");
            let body = callee.instance(&db).body(&db);

            assert!(
                matches!(
                    body.signature.ret,
                    Some(RuntimeClass::AggregateValue { .. })
                ),
                "by-value aggregate helper should keep a visible aggregate return signature:\n{body:#?}"
            );
            assert!(
        body.locals
            .iter()
            .skip(body.signature.params.len())
            .any(|local| matches!(
                &local.carrier,
                mir::RuntimeCarrier::Value(RuntimeClass::Ref {
                    kind: RefKind::Object,
                    pointee,
                    ..
                }) if matches!(pointee.as_ref(), RuntimeClass::AggregateValue { layout } if matches!(layout.data(&db), Layout::Array(_)))
        )),
        "helper should still be free to use internal object-backed storage for projectable owned aggregates:\n{body:#?}"
    );
        }
    );
}

#[test]
fn callers_of_by_value_array_returns_do_not_receive_object_ref_results() {
    with_runtime_package!(
        "callers_of_by_value_array_returns_do_not_receive_object_ref_results.fe",
        r#"
fn return_array_after_projection(xs: [u8; 4]) -> [u8; 4] {
    let ys = xs
    let _ = ys[2]
    ys
}

fn use_returned_array() -> u8 {
    var xs: [u8; 4] = [1, 2, 3, 4]
    xs = return_array_after_projection(xs)
    xs[0]
}
"#,
        |db, package| {
            let callee = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| {
                    function
                        .symbol(&db)
                        .contains("return_array_after_projection")
                })
                .expect("return_array_after_projection runtime function")
                .instance(&db);
            let caller = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db).contains("use_returned_array"))
                .expect("use_returned_array runtime function");
            let body = caller.instance(&db).body(&db);

            let call_results = body
                .blocks
                .iter()
                .flat_map(|block| block.stmts.iter())
                .filter_map(|stmt| match stmt {
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Call { callee: target, .. },
                    } if *target == callee => Some(*dst),
                    _ => None,
                })
                .collect::<Vec<_>>();

            assert!(
                !call_results.is_empty(),
                "caller should contain a direct call to the array-return helper:\n{body:#?}"
            );
            assert!(
                call_results.iter().all(|result| matches!(
                    body.value_class(*result),
                    Some(RuntimeClass::AggregateValue { .. })
                )),
                "by-value aggregate call results should stay visible aggregate values in callers, not object refs:\n{body:#?}"
            );
        }
    );
}

#[test]
fn constant_aggregate_initializers_store_their_const_data_whole() {
    with_test_runtime_package!(
        "constant_aggregate_initializers_store_their_const_data_whole.fe",
        r#"
const TABLE: [u256; 4] = [1, 2, 3, 4]

fn zeroed_bytes(i: usize) -> u8 {
    var bytes: [u8; 64] = [0; 64]
    bytes[i] = 7
    bytes[1]
}

fn copied_table(i: usize) -> u256 {
    var xs: [u256; 4] = TABLE
    xs[i] = 7
    xs[1]
}

#[test]
fn initializers() {
    assert(zeroed_bytes(i: 3) == 0 && copied_table(i: 1) == 7)
}
"#,
        |db, package| {
            let output = emit_runtime_package_sonatina_ir_optimized(&db, &package, OptLevel::O0)
                .expect("Sonatina IR");
            for (name, ty) in [("zeroed_bytes", "[i8; 64]"), ("copied_table", "[i256; 4]")] {
                let body = sonatina_function_body(&output, name);
                assert!(
                    body.contains(&format!("{ty} = const.load")) && !body.contains("insert_value"),
                    "a constant initializer should load its const data whole:\n{body}"
                );
                assert_eq!(
                    body.matches("obj.alloc").count(),
                    1,
                    "a constant initializer should store into its binding without a temporary:\n{body}"
                );
            }
        }
    );
}

#[test]
fn by_value_array_returns_store_whole_into_object_storage() {
    with_runtime_package!(
        "by_value_array_returns_store_whole_into_object_storage.fe",
        r#"
fn return_array_after_projection(xs: [u256; 3]) -> [u256; 3] {
    let ys = xs
    let _ = ys[2]
    ys
}

fn use_returned_array() -> u256 {
    var xs: [u256; 3] = [1, 2, 3]
    xs = return_array_after_projection(xs)
    xs[0]
}
"#,
        |db, package| {
            let output = emit_runtime_package_sonatina_ir_optimized(&db, &package, OptLevel::O0)
                .expect("Sonatina IR");
            let body = sonatina_function_body(&output, "use_returned_array");
            let result = body
                .lines()
                .find_map(|line| {
                    line.contains("call %return_array_after_projection")
                        .then(|| line.trim().split('.').next())
                        .flatten()
                })
                .unwrap_or_else(|| panic!("missing array call:\n{body}"));

            assert!(
                body.lines().any(|line| line.contains("obj.store")
                    && line.trim().ends_with(&format!(" {result};"))),
                "a by-value array result should store into its object whole:\n{body}"
            );
            assert!(
                !body.contains("extract_value"),
                "a by-value array result should not be copied element by element:\n{body}"
            );
        }
    );
}

#[test]
fn enum_bearing_struct_results_store_whole_into_object_storage() {
    with_test_runtime_package!(
        "enum_bearing_struct_results_store_whole_into_object_storage.fe",
        r#"
enum E {
    A,
    B,
}

struct Pair {
    xs: [u256; 3],
    e: E,
}

fn build(flag: bool) -> Pair {
    let xs: [u256; 3] = [1, 2, 3]
    let e = if flag { E::A } else { E::B }
    Pair { xs, e }
}

#[test]
fn exercise() {
    var pair = build(true)
    pair.xs[1] = 5
    assert(pair.xs[0] == 1 && pair.xs[1] == 5)
}
"#,
        |db, package| {
            let output = emit_runtime_package_sonatina_ir_optimized(&db, &package, OptLevel::O0)
                .expect("Sonatina IR");
            assert!(
                !output.contains("enum.set_tag") && !output.contains("br_table"),
                "a struct result should keep its enum field inside one whole store:\n{output}"
            );
        }
    );
}

#[test]
fn capability_gated_provider_ref_conversion_preserves_transport_in_rmir() {
    with_runtime_package!(
        "capability_gated_provider_ref_conversion_preserves_transport_in_rmir.fe",
        include_str!("fixtures/effect_handle_field_deref.fe").to_string(),
        |db, package| {
            assert!(
                package
                    .functions(&db)
                    .iter()
                    .all(|function| !function.symbol(&db).ends_with("from_raw")),
                "raw effect-handle constructors must not be emitted as callable helpers"
            );

            let provider_ref_conversions = package
                .functions(&db)
                .iter()
                .copied()
                .filter(|function| {
                    function
                        .instance(&db)
                        .body(&db)
                        .blocks
                        .iter()
                        .flat_map(|block| block.stmts.iter())
                        .any(|stmt| {
                            matches!(
                                stmt,
                                RStmt::Assign {
                                    expr: RExpr::ProviderRefFromRaw { .. },
                                    ..
                                }
                            )
                        })
                })
                .collect::<Vec<_>>();
            assert!(
                !provider_ref_conversions.is_empty(),
                "expected capability-backed raw-to-provider conversion in runtime package"
            );

            for function in provider_ref_conversions {
                let body = function.instance(&db).body(&db);
                assert!(
                    !body
                        .blocks
                        .iter()
                        .flat_map(|block| block.stmts.iter())
                        .any(|stmt| {
                            matches!(
                                stmt,
                                RStmt::Assign {
                                    expr: RExpr::AddrOf {
                                        place: RuntimePlace {
                                            root: PlaceRoot::Slot(_),
                                            ..
                                        },
                                    } | RExpr::MaterializeToObject { .. },
                                    ..
                                } | RStmt::CopyInto { .. }
                            )
                        }),
                    "raw-to-provider conversion should not materialize or take the address of the raw slot:\n{body:#?}"
                );
            }
        }
    );
}

/// A sum shape is not a value: the caller of a projection yielding
/// `Option<mut T>` binds the grant where the yield takes its variant, so no
/// enum value carries it and it needs no `Native` reference, whether it is
/// a storage place or a memory one.
#[test]
fn sum_shape_grants_are_bound_without_enum_values() {
    let source = r#"
struct Counter { value: u256 }

impl Counter {
    fn maybe(mut self, _ present: bool) -> Option<mut u256> {
        if present { Option::Some(mut self.value) } else { Option::None }
    }
}

msg Msg {
    #[selector = 1]
    Inc -> u256,
}

pub contract C {
    mut counter: Counter,

    recv Msg {
        Inc -> u256 uses (mut counter) {
            match counter.maybe(true) {
                Option::Some(mut value) => { value += 1 }
                Option::None => {}
            }
            counter.value
        }
    }
}

#[test]
fn in_memory() {
    var counter = Counter { value: 1 }
    if let Option::Some(mut value) = counter.maybe(true) {
        value += 1
    }
    assert(counter.value == 2)
}
"#;
    with_top_mod_for_source("sum_shapes.fe", source, |db, top_mod| {
        for (package, symbol) in [
            (build_runtime_package(db, top_mod), "__C_recv_0_0"),
            (build_test_runtime_package(db, top_mod, None), "in_memory"),
        ] {
            let package = package.expect("the package lowers");
            let body = runtime_body_for_symbol(db, package, symbol);
            assert!(
                !runtime_body_stmts(&body).any(|stmt| matches!(
                    stmt,
                    RStmt::Assign {
                        expr: RExpr::NativeRef { .. },
                        ..
                    }
                ) || matches!(
                    stmt,
                    RStmt::Assign {
                        expr: RExpr::EnumMake { fields, .. },
                        ..
                    } if !fields.is_empty()
                )),
                "`{symbol}` builds a reference into a value: {body:#?}"
            );
        }
    });
}

fn body_has_native_ref_or_enum_payload(body: &RuntimeBody<'_>) -> bool {
    runtime_body_stmts(body).any(|stmt| {
        matches!(
            stmt,
            RStmt::Assign {
                expr: RExpr::NativeRef { .. },
                ..
            }
        ) || matches!(
            stmt,
            RStmt::Assign {
                expr: RExpr::EnumMake { fields, .. },
                ..
            } if !fields.is_empty()
        )
    })
}

/// Acceptance 46: a storage struct's method is one instance whichever
/// field or caller reaches it: the slot is the receiver's transport.
#[test]
fn storage_struct_method_is_one_instance_for_every_caller() {
    let source = r#"
struct Counter { value: u256 }

impl Counter {
    fn bump(mut self) {
        self.value += 1
    }
}

fn twice(_ c: mut Counter) {
    c.bump()
    c.bump()
}

msg Msg {
    #[selector = 1]
    BumpA,
    #[selector = 2]
    BumpB,
    #[selector = 3]
    Twice,
}

pub contract C {
    mut a: Counter,
    mut b: Counter,

    recv Msg {
        BumpA uses (mut a) { a.bump() }
        BumpB uses (mut b) { b.bump() }
        Twice uses (mut a) { twice(mut a) }
    }
}
"#;
    with_runtime_package!("one_instance.fe", source, |db, package| {
        let bumps = package
            .functions(&db)
            .iter()
            .filter(|function| function.symbol(&db).contains("bump"))
            .count();
        assert_eq!(bumps, 1, "`Counter::bump` is instantiated per caller");
    });
}

/// Acceptance 47 and 48: grants of packed `u8` lanes, from two yield sites
/// and through `Option<mut u8>`, are write-back copies: no native
/// reference, and no enum value carries one.
#[test]
fn packed_lane_grants_are_write_back_copies() {
    let source = r#"
struct S { a: u8, b: u8 }

impl S {
    fn pick(mut self, _ c: bool) -> mut u8 {
        if c { mut self.b } else { mut self.a }
    }

    fn lane(mut self, _ present: bool) -> Option<mut u8> {
        if present { Option::Some(mut self.b) } else { Option::None }
    }
}

msg Msg {
    #[selector = 1]
    Pick { c: bool },
    #[selector = 2]
    Lane,
}

pub contract C {
    mut s: S,

    recv Msg {
        Pick { c } uses (mut s) {
            let x = s.pick(c)
            x = 42
        }
        Lane uses (mut s) {
            if let Option::Some(mut x) = s.lane(true) {
                x += 1
            }
        }
    }
}
"#;
    with_runtime_package!("packed_lane_grants.fe", source, |db, package| {
        for symbol in ["__C_recv_0_0", "__C_recv_0_1"] {
            let body = runtime_body_for_symbol(&db, package, symbol);
            assert!(
                !body_has_native_ref_or_enum_payload(&body),
                "`{symbol}` builds a reference into a value: {body:#?}"
            );
        }
    });
}

/// Acceptance 49: `days[k].steps += 1` on a map of three-slot structs loads
/// and stores the one slot it writes.
#[test]
fn map_entry_field_update_touches_one_slot() {
    let source = r#"
use std::evm::StorageMap

struct DaySummary { steps: u256, distance: u256, calories: u256 }

msg Msg {
    #[selector = 1]
    Walk { day: u256 },
}

pub contract Tracker {
    mut days: StorageMap<u256, DaySummary>,

    recv Msg {
        Walk { day } uses (mut days) {
            days[day].steps += 1
        }
    }
}
"#;
    with_runtime_package!("one_slot.fe", source, |db, package| {
        let ir = emit_runtime_package_sonatina_ir_optimized(&db, &package, OptLevel::O1)
            .expect("Sonatina IR");
        let count = |op: &str| {
            sonatina_ops(&ir).into_iter().filter(|o| *o == op).count()
                + ir.lines()
                    .filter(|line| line.trim_start().starts_with(op))
                    .count()
        };
        assert_eq!(count("evm_sload"), 1, "{ir}");
        assert_eq!(count("evm_sstore"), 1, "{ir}");
    });
}

/// L-09: a mixed-space `Mutex<u256>` passed `mut` crosses as its one root
/// number, from which the callee derives the transient lock and the storage
/// value.
#[test]
fn mixed_space_mutex_passes_one_root_number() {
    let source = r#"
use std::evm::Mutex

fn bump(_ m: mut Mutex<u256>) {
    assert!(m.try_lock())
    m.value() += 1
    m.unlock()
}

msg Msg {
    #[selector = 1]
    Bump,
}

pub contract C {
    mut vault: Mutex<u256>,

    recv Msg {
        Bump uses (mut vault) { bump(mut vault) }
    }
}
"#;
    with_runtime_package!("mixed_space_mutex.fe", source, |db, package| {
        let body = runtime_body_for_symbol(&db, package, "bump");
        assert_eq!(
            body.signature.params.len(),
            1,
            "`bump` should take the mutex's root number alone"
        );
    });
}
