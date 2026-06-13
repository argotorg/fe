use common::InputDb;
use driver::DriverDataBase;
use fe_codegen::{OptLevel, emit_module_sonatina_ir, emit_runtime_package_sonatina_ir_optimized};
use hir::hir_def::TopLevelMod;
use mir::runtime::{AddressSpaceKind, RefKind};
use mir::{
    IntrinsicArithBinOp, Layout, LayoutId, PlaceElem, PlaceRoot, RExpr, RLocalId, RStmt,
    RuntimeBody, RuntimeBuiltin, RuntimeCarrier, RuntimeClass, RuntimeInstance, RuntimeLocalRoot,
    RuntimePackage, build_runtime_package, build_test_runtime_package,
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
    function.instance(db).body(db)
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

fn body_preserves_handle_field(body: &RuntimeBody<'_>, param: RLocalId, field: u16) -> bool {
    let transported = transported_local_from_param(body, param);
    runtime_body_stmts(body).any(|stmt| match stmt {
        RStmt::Assign {
            expr: RExpr::AggregateMake { fields, .. },
            ..
        } => fields
            .get(field as usize)
            .is_some_and(|src| *src == transported),
        RStmt::Store { dst, src } => {
            *src == transported
                && matches!(dst.root, PlaceRoot::Ref(_))
                && matches!(dst.path.as_ref(), [PlaceElem::Field(stored)] if stored.0 == field)
        }
        _ => false,
    })
}

fn storage_pair_ref_layout<'db>(class: &RuntimeClass<'db>) -> Option<LayoutId<'db>> {
    match class {
        RuntimeClass::Ref {
            pointee,
            kind:
                RefKind::Provider {
                    space: AddressSpaceKind::Storage,
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
fn transparent_wrapper_returns_preserve_handle_fields_in_rmir() {
    let src = format!(
        "{}\nfn emit_helpers() -> u256 {{\n    let arr: [u256; 8] = [1, 2, 3, 4, 5, 6, 7, 8]\n    sum_last4(arr) + sum_first4(arr)\n}}\n",
        include_str!("../../fe/tests/fixtures/fe_test/view_param_local_ref_take_reverse.fe"),
    );
    with_test_runtime_package!(
        "transparent_wrapper_returns_preserve_handle_fields_in_rmir.fe",
        src,
        |db, package| {
            let take_debug = package
                .functions(&db)
                .iter()
                .copied()
                .filter(|function| function.symbol(&db).contains("take"))
                .map(|function| {
                    let body = function.instance(&db).body(&db);
                    format!("{}:\n{body:#?}", function.symbol(&db))
                })
                .collect::<Vec<_>>();
            let take_u256 = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| {
                    if !function.symbol(&db).contains("take") {
                        return false;
                    }
                    let body = function.instance(&db).body(&db);
                    let [_, param] = body.signature.params.as_slice() else {
                        return false;
                    };
                    matches!(
                        param.class,
                        RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. }
                    ) && body_preserves_handle_field(&body, param.local, 1)
                })
                .unwrap_or_else(|| {
                    panic!(
                        "generated take helper that preserves the incoming handle field\n\n{}",
                        take_debug.join("\n\n")
                    )
                });
            let body = take_u256.instance(&db).body(&db);
            let seq_param = body.signature.params[1].local;

            assert!(
                !body
                    .blocks
                    .iter()
                    .flat_map(|block| block.stmts.iter())
                    .any(|stmt| matches!(
                        stmt,
                        RStmt::Assign {
                            expr: RExpr::MaterializeToObject { .. },
                            ..
                        }
                    )),
                "transparent wrapper returns should not materialize handle fields:\n{body:#?}"
            );
            assert!(
                body_preserves_handle_field(&body, seq_param, 1),
                "transparent wrapper returns should carry the incoming borrow transport directly into the wrapper field:\n{body:#?}"
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
            assert_eq!(layout_data.source_ty.pretty_print(&db).to_string(), "Pair");

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
                    | RStmt::EnumAssertVariant { .. }
                    | RStmt::Store { .. }
                    | RStmt::CopyInto { .. }
                    | RStmt::EnumSetTag { .. }
                    | RStmt::EnumWriteVariant { .. } => None,
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
            assert_eq!(layout_data.source_ty.pretty_print(&db).to_string(), "Pair");
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
fn effect_handle_from_raw_helpers_preserve_transport_in_rmir() {
    with_runtime_package!(
        "effect_handle_from_raw_helpers_preserve_transport_in_rmir.fe",
        include_str!("fixtures/effect_handle_field_deref.fe").to_string(),
        |db, package| {
            let from_raw_helpers = package
                .functions(&db)
                .iter()
                .copied()
                .filter(|function| function.symbol(&db).contains("from_raw"))
                .collect::<Vec<_>>();
            assert!(
                !from_raw_helpers.is_empty(),
                "expected generated from_raw helpers in runtime package"
            );

            for function in from_raw_helpers {
                let body = function.instance(&db).body(&db);
                assert!(
                    body.blocks
                        .iter()
                        .flat_map(|block| block.stmts.iter())
                        .any(|stmt| {
                            matches!(
                                stmt,
                                RStmt::Assign {
                                    expr: RExpr::ProviderFromRaw { .. },
                                    ..
                                }
                            )
                        }),
                    "from_raw helper should rebuild the handle from the raw word:\n{body:#?}"
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
                                    expr: RExpr::AddrOf { .. } | RExpr::MaterializeToObject { .. },
                                    ..
                                } | RStmt::CopyInto { .. }
                            )
                        }),
                    "from_raw helper should not materialize or take the address of the raw slot:\n{body:#?}"
                );
            }
        }
    );
}

#[test]
fn mem_ptr_from_raw_helpers_use_raw_addr_transport_in_rmir() {
    with_runtime_package!(
        "mem_ptr_from_raw_helpers_use_raw_addr_transport_in_rmir.fe",
        include_str!("fixtures/raw_log_emit.fe").to_string(),
        |db, package| {
            let from_raw_helpers = package
                .functions(&db)
                .iter()
                .copied()
                .filter(|function| function.symbol(&db).contains("from_raw"))
                .collect::<Vec<_>>();
            assert!(
                !from_raw_helpers.is_empty(),
                "expected generated MemPtr::from_raw helper in runtime package"
            );

            for function in from_raw_helpers {
                let body = function.instance(&db).body(&db);
                assert!(
                    body.blocks
                        .iter()
                        .flat_map(|block| block.stmts.iter())
                        .any(|stmt| {
                            matches!(
                                stmt,
                                RStmt::Assign {
                                    expr: RExpr::WordToRawAddr {
                                        space: AddressSpaceKind::Memory,
                                        ..
                                    },
                                    ..
                                }
                            )
                        }),
                    "MemPtr::from_raw should keep memory handles as raw-address transport:\n{body:#?}"
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
                                    expr: RExpr::ProviderFromRaw {
                                        space: AddressSpaceKind::Memory,
                                        ..
                                    },
                                    ..
                                }
                            )
                        }),
                    "MemPtr::from_raw must not reconstruct memory provider refs:\n{body:#?}"
                );
            }
        }
    );
}

#[test]
fn object_backed_nested_const_handle_fields_load_carriers_before_const_projection() {
    let output = sonatina_ir_for_source(
        "object_backed_nested_const_handle_fields_load_carriers_before_const_projection.fe",
        r#"struct Data {
    x: u256,
}

struct View {
    d: ref Data,
}

fn read(v: own View) -> u256 {
    v.d.x
}

pub fn entry() -> u256 {
    let data = Data { x: 1 }
    let view = View { d: ref data }
    read(view)
}
"#,
    );
    let read = sonatina_function_body(&output, "read");

    assert!(
        contains_op_subsequence(read, &["obj.proj", "obj.load", "const.proj", "const.load"]),
        "object-backed aggregates may store nested const refs, but field access must load the handle carrier before projecting through the const ref:\n{read}"
    );
}

#[test]
fn storage_backed_nested_handle_fields_follow_carriers_before_projecting_children() {
    let output = sonatina_ir_for_source(
        "storage_backed_nested_handle_fields_follow_carriers_before_projecting_children.fe",
        include_str!("fixtures/effect_handle_field_deref.fe").to_string(),
    );
    let bump = sonatina_function_body(&output, "bump");

    assert!(
        contains_op_subsequence(bump, &["obj.proj", "obj.load", "evm_sload"]),
        "nested storage-backed handle field access should load/follow the carrier before loading children:\n{bump}"
    );
}

#[test]
fn storage_backed_nested_handle_field_borrows_use_storage_transport() {
    with_runtime_package!(
        "storage_backed_nested_handle_field_borrows_use_storage_transport.fe",
        include_str!("fixtures/effect_handle_field_deref.fe").to_string(),
        |db, package| {
            let bump = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db).contains("bump"))
                .expect("generated bump runtime function");
            let body = bump.instance(&db).body(&db);

            let nested_field_borrow = body
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match stmt {
            RStmt::Assign {
                dst,
                expr: RExpr::AddrOf { place },
            } if matches!(place.root, PlaceRoot::Ref(_))
                && matches!(place.path.as_ref(), [PlaceElem::Field(field0), PlaceElem::Deref, PlaceElem::Field(field1)] if field0.0 == 0 && field1.0 == 0) =>
            {
                Some(*dst)
            }
            RStmt::Assign { .. }
            | RStmt::EnumAssertVariant { .. }
            | RStmt::Store { .. }
            | RStmt::CopyInto { .. }
            | RStmt::EnumSetTag { .. }
            | RStmt::EnumWriteVariant { .. } => None,
        })
        .unwrap_or_else(|| {
            panic!("expected nested field borrow in bump runtime body:\n{body:#?}")
        });

            let Some(RuntimeClass::Ref { pointee, kind, .. }) =
                body.value_class(nested_field_borrow)
            else {
                panic!(
                    "nested storage-backed field borrow should lower as a typed ref:\n{body:#?}"
                );
            };
            assert!(
                matches!(**pointee, RuntimeClass::Scalar(_)),
                "nested storage-backed field borrow should point at the scalar field:\n{body:#?}"
            );
            assert!(
                matches!(
                    kind,
                    RefKind::Provider {
                        space: mir::AddressSpaceKind::Storage,
                        ..
                    }
                ),
                "nested storage-backed field borrow should use storage transport, not object/memory transport:\n{body:#?}"
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
fn mutated_scalar_locals_stay_rooted() {
    with_runtime_package!(
        "mutated_scalar_locals_stay_rooted.fe",
        r#"
fn bump(size: u256) -> u256 {
    let mut value: u256 = size
    value += 1
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
            let rooted_ptr = runtime_body_stmts(&body)
                .find_map(|stmt| match stmt {
                    RStmt::Assign {
                        dst,
                        expr: RExpr::AddrOf { place },
                    } if place.root == PlaceRoot::Slot(rooted_scalar) => Some(*dst),
                    _ => None,
                })
                .unwrap_or_else(|| {
                    panic!("mutated scalar local should take the address of its rooted slot:\n{body:#?}")
                });
            let stored_ptr = transported_local_from_param(&body, rooted_ptr);
            assert!(
                runtime_body_stmts(&body).any(|stmt| {
                    matches!(
                        stmt,
                        RStmt::Store { dst, .. }
                            if matches!(dst.root, PlaceRoot::Ptr { addr, .. } if addr == stored_ptr)
                    )
                }),
                "mutated scalar local should store through the rooted slot pointer:\n{body:#?}"
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
fn read_only_mut_own_aggregate_receiver_field_reads_stay_unrooted() {
    with_runtime_package!(
        "read_only_mut_own_aggregate_receiver_field_reads_stay_unrooted.fe",
        r#"struct Pair {
    left: u256,
    right: u256,
}

impl Pair {
    fn sum(mut own self) -> u256 {
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
    fn bump(mut own self) -> Pair {
        self.left += 1
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
fn derived_place_bound_field_aliases_do_not_write_back_to_receiver() {
    with_runtime_package!(
        "derived_place_bound_field_aliases_do_not_write_back_to_receiver.fe",
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
                "field reads through a derived place-bound alias should not write back to the receiver:\n{body:#?}"
            );
            assert!(
                body.locals
                    .iter()
                    .skip(body.signature.params.len())
                    .any(|local| {
                        local.semantic_ty.pretty_print(&db) == "u256"
                            && matches!(local.carrier, RuntimeCarrier::Erased)
                            && matches!(local.root, RuntimeLocalRoot::None)
                    }),
                "derived place-bound scalar alias should stay carrierless/rootless:\n{body:#?}"
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
                "{}\nfn entry() -> u256 {{\n    let mut table = Table::empty()\n    table.set(17, 99)\n    table.get(17)\n}}\n",
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
    let mut x: u8 = 2
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
                        mir::RuntimeCarrier::Value(RuntimeClass::RawAddr { target: None, .. })
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
            !ir.contains("obj.init.const"),
            "{label} should not materialize the full array:\n{ir}"
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
                            PlaceRoot::Ref(root) => Some(root),
                            PlaceRoot::Slot(_) | PlaceRoot::Ptr { .. } | PlaceRoot::Provider(_) => {
                                None
                            }
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
                    body.value_class(inc_root),
                    Some(RuntimeClass::Ref {
                        kind: RefKind::Object,
                        ..
                    })
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
        r#"pub struct Counter {
    pub value: u256,
}

fn read() -> u256 uses (counter: Counter) {
    counter.value
}

#[test]
fn test_readonly_with_provider() {
    let out: u256 = with (Counter = Counter { value: 7 }) {
        read() + read()
    }

    assert!(out == 14)
}"#,
        |db, package| {
            let read = package
                .functions(&db)
                .iter()
                .copied()
                .find(|function| function.symbol(&db) == "read")
                .expect("read runtime function")
                .instance(&db);
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
                    } if *callee == read => Some(args[0]),
                    _ => None,
                })
                .collect::<Vec<_>>();

            assert_eq!(
                read_args.len(),
                2,
                "readonly with-provider test should call read twice:\n{body:#?}"
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
fn borrow_typed_aggregate_literals_lower_without_const_shape_mismatch() {
    with_runtime_package!(
        "borrow_typed_aggregate_literals_lower_without_const_shape_mismatch.fe",
        r#"fn first(xs: ref [u256; 2]) -> u256 {
    xs[0]
}

fn entry() -> u256 {
    first([10, 20])
}"#,
        |db, package| {
            let debug = package
                .functions(&db)
                .iter()
                .map(|function| {
                    let body = function.instance(&db).body(&db);
                    format!("{}:\n{body:#?}", function.symbol(&db))
                })
                .collect::<Vec<_>>();

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
                                    expr: RExpr::MaterializeToObject { .. }
                                        | RExpr::AllocObject { .. },
                                    ..
                                }
                            )
                        })
                }),
                "borrow-typed aggregate literals should materialize through normal object/value lowering:\n{}",
                debug.join("\n\n")
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
                            RefKind::Object | RefKind::Provider { .. } | RefKind::Const
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
    let mut x: usize = 0
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
    let mut xs: [u8; 4] = [1, 2, 3, 4]
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
    let mut xs: [u8; 4] = [1, 2, 3, 4]
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
fn by_value_array_return_materialization_structurally_copies_into_object_storage() {
    with_runtime_package!(
        "by_value_array_return_materialization_structurally_copies_into_object_storage.fe",
        r#"
fn return_array_after_projection(xs: [u256; 3]) -> [u256; 3] {
    let ys = xs
    let _ = ys[2]
    ys
}

fn use_returned_array() -> u256 {
    let mut xs: [u256; 3] = [1, 2, 3]
    xs = return_array_after_projection(xs)
    xs[0]
}
"#,
        |db, package| {
            let output = emit_runtime_package_sonatina_ir_optimized(
                &db,
                &package,
                fe_codegen::EVM_LAYOUT,
                OptLevel::O0,
            )
            .expect("Sonatina IR");
            let body = sonatina_function_body(&output, "use_returned_array");

            assert!(
                body.contains("extract_value"),
                "by-value aggregate materialization should structurally extract fields instead of whole-object obj.store:\n{body}"
            );
            assert!(
                body.contains("obj.load"),
                "object-backed aggregate copies should load leaf values from the source object before storing them:\n{body}"
            );
        }
    );
}

#[test]
fn fieldless_enum_fields_copy_into_object_storage_via_enum_ops() {
    with_test_runtime_package!(
        "fieldless_enum_fields_copy_into_object_storage_via_enum_ops.fe",
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
    let pair = build(true)
    assert(pair.xs[0] == 1)
}
"#,
        |db, package| {
            let output = emit_runtime_package_sonatina_ir_optimized(
                &db,
                &package,
                fe_codegen::EVM_LAYOUT,
                OptLevel::O0,
            )
            .expect("Sonatina IR");
            assert!(
                output.contains("enum.set_tag"),
                "fieldless enum object copies should set the destination enum tag explicitly:\n{output}"
            );
        }
    );
}
