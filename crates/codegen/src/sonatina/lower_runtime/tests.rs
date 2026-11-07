use common::InputDb;
#[cfg(any(
    feature = "wasm",
    all(
        feature = "cranelift",
        any(target_arch = "x86_64", target_arch = "aarch64")
    )
))]
use common::compilation::Target;
use contract_harness::{ExecutionOptions, RuntimeInstance as EvmRuntime};
use driver::DriverDataBase;
use hir::analysis::semantic::FieldIndex;
#[cfg(all(
    feature = "cranelift",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]
use hir::analysis::ty::AddressSpace;
use mir::{
    ConstScalar, MirDb, PlaceElem, PlaceRoot, RBlock, RExpr, RLocal, RLocalId, RStmt, RTerminator,
    RefKind, RefView, RuntimeBody, RuntimeCarrier, RuntimeClass, RuntimeInstance, RuntimeLocalRoot,
    RuntimePackage, RuntimePlace, build_runtime_package, verify_runtime_body,
};
#[cfg(any(
    feature = "wasm",
    all(
        feature = "cranelift",
        any(target_arch = "x86_64", target_arch = "aarch64")
    )
))]
use mir::{Layout, RuntimeMemoryLayout, VariantId};
#[cfg(all(
    feature = "cranelift",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]
use mir::{NativeOutputKind, ScalarClass, ScalarRole, build_native_package};
#[cfg(any(
    feature = "wasm",
    all(
        feature = "cranelift",
        any(target_arch = "x86_64", target_arch = "aarch64")
    )
))]
use salsa::Setter;
#[cfg(all(
    feature = "cranelift",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]
use sonatina_ir::ir_writer::ModuleWriter;
use sonatina_ir::{builder::ModuleBuilder, isa::Isa, module::ModuleCtx};
use url::Url;

use super::{FunctionLowerer, ModuleLowerer, NativePanics};
use crate::{
    OptLevel,
    sonatina::{create_evm_isa, emit_runtime_module_sonatina_bytecode_with_options},
};

#[cfg(all(
    feature = "cranelift",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]
#[test]
fn backend_addresses_must_be_as_wide_as_the_target_usize() {
    // An EVM compilation, whose layouts take 256-bit addresses.
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///evm_target_native_backend.fe").unwrap(),
        Some("pub fn main() -> i32 {\n    0\n}\n".into()),
    );
    let package = build_runtime_package(&db, db.top_mod(file)).unwrap();
    let isa = crate::sonatina::create_native_isa().unwrap();
    let Err(error) = ModuleLowerer::new(
        &db,
        ModuleBuilder::new(ModuleCtx::new(&isa)),
        isa.inst_set(),
        &package,
        &[],
        &[],
        NativePanics::Abort,
    ) else {
        panic!("a native backend must not lower an EVM compilation");
    };
    assert!(
        error
            .to_string()
            .contains("the Evm compilation target has 256-bit addresses"),
        "unexpected error: {error}"
    );
}

#[cfg(all(
    feature = "cranelift",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]
#[test]
fn native_raw_address_slots_take_the_address_width() {
    let mut db = DriverDataBase::default();
    db.compilation_settings()
        .set_target(&mut db)
        .to(Target::Native);
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///native_raw_address_slot.fe").unwrap(),
        Some(
            r#"
#[inline(never)]
fn address() -> usize { 0 }
pub fn main() -> i32 {
    let _ = address()
    0
}
"#
            .into(),
        ),
    );
    let package =
        build_native_package(&db, db.top_mod(file), NativeOutputKind::Executable).unwrap();
    let target = package
        .functions(&db)
        .iter()
        .find(|function| function.symbol(&db) == "address")
        .expect("address helper")
        .instance(&db);
    let mut body = target.body(&db).clone();
    let address = body.signature.ret.clone().expect("address return");
    let RuntimeClass::Scalar(address_scalar) = address.clone() else {
        panic!("a `usize` is a scalar: {address:?}");
    };
    // Only a memory pointer converts to a raw memory address.
    let pointer = RuntimeClass::Scalar(ScalarClass {
        role: ScalarRole::Pointer,
        ..address_scalar.clone()
    });
    let raw = RuntimeClass::RawAddr {
        space: AddressSpace::Memory,
        pointee: None,
    };
    let semantic_ty = body.locals[0].semantic_ty;
    let origin = body.terminator_origins[0];
    // A raw address in a stack slot, stored and loaded back.
    body.locals = [
        (pointer, false),
        (raw.clone(), true),
        (raw, false),
        (address, false),
    ]
    .into_iter()
    .map(|(class, stored)| RLocal {
        semantic_ty,
        carrier: RuntimeCarrier::Value(class.clone()),
        root: if stored {
            RuntimeLocalRoot::Slot(class)
        } else {
            RuntimeLocalRoot::None
        },
    })
    .collect();
    let local = RLocalId::from_u32;
    let stmts = vec![
        RStmt::Assign {
            dst: local(0),
            expr: RExpr::ConstScalar(ConstScalar::Int {
                words: 0x1234u64.to_be_bytes().to_vec(),
                bits: 64,
                signed: false,
            }),
        },
        RStmt::Assign {
            dst: local(1),
            expr: RExpr::WordToRawAddr {
                value: local(0),
                space: AddressSpace::Memory,
            },
        },
        RStmt::Assign {
            dst: local(2),
            expr: RExpr::Use(local(1)),
        },
        RStmt::Assign {
            dst: local(3),
            expr: RExpr::Cast {
                value: local(2),
                to: address_scalar,
            },
        },
    ];
    body.stmt_origins = vec![vec![origin; stmts.len()]];
    body.terminator_origins = vec![origin];
    body.blocks = vec![RBlock {
        stmts,
        terminator: RTerminator::Return(Some(local(3))),
    }];
    let program: &dyn MirDb = &db;
    verify_runtime_body(&db, &program, &body).expect("valid raw address slot");

    let isa = crate::sonatina::create_native_isa().unwrap();
    let mut lowerer = ModuleLowerer::new(
        &db,
        ModuleBuilder::new(ModuleCtx::new(&isa)),
        isa.inst_set(),
        &package,
        &[],
        &[],
        NativePanics::Abort,
    )
    .unwrap();
    lowerer.declare_functions().unwrap();
    let func_ref = lowerer.func_ref(target).unwrap();
    FunctionLowerer::new(&mut lowerer, body, func_ref)
        .unwrap()
        .lower()
        .unwrap();
    let ir = ModuleWriter::new(&lowerer.finish().0).dump_string();
    let function = ir
        .split("func ")
        .find(|function| function.contains("%address("))
        .expect("lowered address helper");
    // The slot, its store and its load all take a 64-bit pointer.
    assert!(function.contains("alloca *i8"), "{function}");
    assert!(!function.contains("i256"), "{function}");
}

#[cfg(any(
    feature = "wasm",
    all(
        feature = "cranelift",
        any(target_arch = "x86_64", target_arch = "aarch64")
    )
))]
const LAYOUT_ZOO: &str = r#"
use core::option::Option
use core::ptr::{self, StorPtr}
use core::result::Result

struct Flags { a: bool, b: bool, c: u8 }
enum Three { A, B(bool, u16), C(u128) }
enum Nested { X(Three), Y(Flags, u32) }
struct Empty {}
struct Spaced { e: Empty, x: u8, f: Empty, y: u16 }
struct Overaligned { a: u8, none: [u256; 0] }
enum EmptyPayload { A(u8), B([u256; 0]) }

struct Zoo {
    flags: Flags,
    three: Three,
    nested: Nested,
    spaced: Spaced,
    overaligned: Overaligned,
    empty_payload: EmptyPayload,
    tuple: (u8, u256, i16, bool),
    options: [Option<u16>; 3],
    bools: [bool; 3],
    word: Option<u256>,
    result: Result<u8, u128>,
    text: String<5>,
    node: *Zoo,
    slot: StorPtr<u8>,
    size: usize,
}

#[inline(never)]
fn keep(_ zoo: Zoo) -> u8 { zoo.flags.c }

pub fn main() -> i32 {
    let zoo = ptr::alloc<Zoo>()
    let _ = keep(unsafe { *zoo })
    0
}
"#;

/// Fe lays out raw memory itself, and Sonatina lays out the objects it views
/// that memory as: every size and offset of the types `package` lowers must
/// agree under `isa`.
#[cfg(any(
    feature = "wasm",
    all(
        feature = "cranelift",
        any(target_arch = "x86_64", target_arch = "aarch64")
    )
))]
fn assert_raw_memory_layouts_match_objects<I>(
    db: &DriverDataBase,
    package: &RuntimePackage<'_>,
    isa: &I,
) where
    I: Isa,
    I::InstSet: super::LoweringInstSet,
{
    let mut lowerer = ModuleLowerer::new(
        db,
        ModuleBuilder::new(ModuleCtx::new(isa)),
        isa.inst_set(),
        package,
        &[],
        &[],
        NativePanics::Trap,
    )
    .unwrap();
    lowerer.declare_functions().unwrap();
    lowerer.lower_bodies().unwrap();
    // Lowering `Zoo` lowers the type of every field in it.
    assert!(
        lowerer.type_cache.keys().any(|layout| matches!(
            layout.data(db),
            Layout::Struct(data) if data.fields.len() == 15
        )),
        "`Zoo` was not lowered"
    );
    let raw = RuntimeMemoryLayout::raw(db);
    let ctx = &lowerer.builder.ctx;
    let offset = |offset: Option<(usize, _)>| offset.map(|(offset, _)| offset as u64);
    for (&layout, &ty) in &lowerer.type_cache {
        let data = layout.data(db);
        assert_eq!(
            raw.layout_size(layout).ok(),
            ctx.size_of(ty).ok().map(|size| size as u64),
            "{data:?}"
        );
        match &data {
            Layout::Struct(record) => {
                for (field, _) in record.fields.iter().enumerate() {
                    assert_eq!(
                        raw.struct_field_offset(record, field).ok(),
                        offset(ctx.aggregate_elem_offset(ty, field)),
                        "{data:?} field {field}"
                    );
                }
            }
            Layout::Array(array) => {
                if array.len > 1 {
                    assert_eq!(
                        raw.array_element_offset(array, 1).ok(),
                        offset(ctx.aggregate_elem_offset(ty, 1)),
                        "{data:?}"
                    );
                }
            }
            Layout::Enum(choice) => {
                for (index, variant) in choice.variants.iter().enumerate() {
                    for (field, _) in variant.fields.iter().enumerate() {
                        let id = VariantId {
                            enum_layout: layout,
                            index: index as u16,
                        };
                        assert_eq!(
                            raw.variant_field_offset(id, FieldIndex(field as u16)).ok(),
                            offset(ctx.enum_variant_field_offset(ty, index, field)),
                            "{data:?} variant {index} field {field}"
                        );
                    }
                }
            }
        }
    }
}

#[cfg(all(
    feature = "cranelift",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]
#[test]
fn native_raw_memory_layouts_match_sonatina_objects() {
    let mut db = DriverDataBase::default();
    db.compilation_settings()
        .set_target(&mut db)
        .to(Target::Native);
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///native_layouts.fe").unwrap(),
        Some(LAYOUT_ZOO.into()),
    );
    let package =
        build_native_package(&db, db.top_mod(file), NativeOutputKind::Executable).unwrap();
    let isa = crate::sonatina::create_native_isa().unwrap();
    assert_raw_memory_layouts_match_objects(&db, &package, &isa);
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_raw_memory_layouts_match_sonatina_objects() {
    // Pointers, references and `usize` take 4 bytes here.
    let mut db = DriverDataBase::default();
    db.compilation_settings()
        .set_target(&mut db)
        .to(Target::Wasm);
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///wasm_layouts.fe").unwrap(),
        Some(LAYOUT_ZOO.into()),
    );
    let package = mir::build_wasm_export_package(&db, db.top_mod(file))
        .unwrap()
        .package;
    let isa = crate::sonatina::wasm::create_wasm32_isa();
    assert_raw_memory_layouts_match_objects(&db, &package, &isa);
}

/// Deploys `contract` from `package` with `target`'s body replaced by `body`.
fn deploy_with_body<'db>(
    db: &'db DriverDataBase,
    package: RuntimePackage<'db>,
    target: RuntimeInstance<'db>,
    body: RuntimeBody<'db>,
    opt: OptLevel,
    contract: &str,
) -> EvmRuntime {
    let program: &dyn MirDb = db;
    verify_runtime_body(db, &program, &body).expect("valid runtime body");

    let isa = create_evm_isa();
    let mut lowerer = ModuleLowerer::new(
        db,
        ModuleBuilder::new(ModuleCtx::new(&isa)),
        isa.inst_set(),
        &package,
        &[],
        &[],
        NativePanics::Trap,
    )
    .unwrap();
    lowerer.declare_functions().unwrap();
    lowerer.lower_const_regions().unwrap();
    for &function in package.functions(db) {
        let instance = function.instance(db);
        let selected = if instance == target {
            body.clone()
        } else {
            instance.body(db).clone()
        };
        let func_ref = lowerer.func_ref(instance).unwrap();
        FunctionLowerer::new(&mut lowerer, selected, func_ref)
            .unwrap()
            .lower()
            .unwrap();
    }
    lowerer.declare_objects().unwrap();
    let (mut artifacts, _) = emit_runtime_module_sonatina_bytecode_with_options(
        db,
        &package,
        lowerer.finish().0,
        opt,
        false,
        None,
    )
    .unwrap_or_else(|error| panic!("compile {contract} at {opt:?}: {error}"));
    let artifact = artifacts.remove(contract).unwrap();
    EvmRuntime::deploy(&hex::encode(artifact.deploy)).unwrap()
}

#[test]
fn stack_native_reference_borrows_preserve_slot_contents_and_identity() {
    for (opt, replace) in [
        (OptLevel::O0, false),
        (OptLevel::O0, true),
        (OptLevel::O2, false),
        (OptLevel::O2, true),
    ] {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///stack_native_reference.fe").unwrap(),
            Some(
                r#"
#[inline(never)]
fn borrow_slot() -> u256 { 0 }
msg SlotMsg {
    #[selector = 1]
    Read -> u256,
}
pub contract SlotContract {
    recv SlotMsg { Read -> u256 { borrow_slot() } }
}
"#
                .into(),
            ),
        );
        let top_mod = db.top_mod(file);
        let diagnostics = db.run_on_top_mod(top_mod);
        assert!(diagnostics.is_empty(), "{}", diagnostics.format_diags(&db));
        let package = build_runtime_package(&db, top_mod).expect("runtime package");
        let target = package
            .functions(&db)
            .iter()
            .find(|function| function.symbol(&db) == "borrow_slot")
            .expect("slot helper")
            .instance(&db);
        let mut body = target.body(&db).clone();
        let scalar = body.signature.ret.clone().expect("word return");
        let semantic_ty = body.locals[0].semantic_ty;
        let origin = body.terminator_origins[0];
        let object_ref = RuntimeClass::Ref {
            pointee: Box::new(scalar.clone()),
            kind: RefKind::Object,
            view: RefView::Whole,
        };
        let native_ref = RuntimeClass::Ref {
            pointee: Box::new(scalar.clone()),
            kind: RefKind::Native,
            view: RefView::Whole,
        };
        let slot_ref = RuntimeClass::Ref {
            pointee: Box::new(native_ref.clone()),
            kind: RefKind::Native,
            view: RefView::Whole,
        };
        body.locals = [
            (scalar.clone(), true),
            (object_ref.clone(), false),
            (native_ref.clone(), false),
            (native_ref.clone(), true),
            (slot_ref, false),
            (native_ref.clone(), false),
            (scalar.clone(), false),
            (scalar, true),
            (object_ref, false),
            (native_ref, false),
        ]
        .into_iter()
        .map(|(class, stored)| RLocal {
            semantic_ty,
            carrier: RuntimeCarrier::Value(class.clone()),
            root: if stored {
                RuntimeLocalRoot::Slot(class)
            } else {
                RuntimeLocalRoot::None
            },
        })
        .collect();
        let local = RLocalId::from_u32;
        let place = |root| RuntimePlace {
            root,
            path: Box::default(),
        };
        let assign = |dst, expr| RStmt::Assign {
            dst: local(dst),
            expr,
        };
        let constant = |value: u64| {
            RExpr::ConstScalar(ConstScalar::Int {
                words: value.to_be_bytes().to_vec(),
                bits: 256,
                signed: false,
            })
        };
        // Exercise the verified runtime-IR contract directly: slot 3 contains a
        // native carrier, and taking its address must create a second descriptor.
        let mut stmts = vec![
            assign(0, constant(37)),
            assign(
                1,
                RExpr::AddrOf {
                    place: place(PlaceRoot::Slot(local(0))),
                },
            ),
            assign(2, RExpr::NativeRef { value: local(1) }),
            assign(3, RExpr::Use(local(2))),
            assign(
                4,
                RExpr::AddrOf {
                    place: place(PlaceRoot::Slot(local(3))),
                },
            ),
        ];
        if replace {
            stmts.extend([
                assign(7, constant(73)),
                assign(
                    8,
                    RExpr::AddrOf {
                        place: place(PlaceRoot::Slot(local(7))),
                    },
                ),
                assign(9, RExpr::NativeRef { value: local(8) }),
                RStmt::Store {
                    dst: place(PlaceRoot::Ref(local(4))),
                    src: local(9),
                },
            ]);
        }
        // Read through the borrow, or read the original slot after updating it
        // through the borrow. Both must preserve the stored carrier's identity.
        let read = if replace {
            PlaceRoot::Slot(local(3))
        } else {
            PlaceRoot::Ref(local(4))
        };
        stmts.extend([
            assign(5, RExpr::Load { place: place(read) }),
            assign(
                6,
                RExpr::Load {
                    place: place(PlaceRoot::Ref(local(5))),
                },
            ),
        ]);
        body.stmt_origins = vec![vec![origin; stmts.len()]];
        body.terminator_origins = vec![origin];
        body.blocks = vec![RBlock {
            stmts,
            terminator: RTerminator::Return(Some(local(6))),
        }];
        let mut runtime = deploy_with_body(&db, package, target, body, opt, "SlotContract");
        let result = runtime
            .call_raw(&[0, 0, 0, 1], ExecutionOptions::default())
            .unwrap_or_else(|error| panic!("{opt:?}, replace={replace}: {error}"));
        let mut expected = vec![0; 32];
        expected[31] = if replace { 73 } else { 37 };
        assert_eq!(result.return_data, expected, "{opt:?}, replace={replace}");
    }
}

#[test]
fn a_trapping_revert_drops_only_stores_into_its_own_allocations() {
    let local = RLocalId::from_u32;
    let constant = |dst| RStmt::Assign {
        dst: local(dst),
        expr: RExpr::ConstScalar(ConstScalar::Int {
            words: vec![0],
            bits: 64,
            signed: false,
        }),
    };
    let malloc = |dst, size| RStmt::Assign {
        dst: local(dst),
        expr: RExpr::Builtin(mir::RuntimeBuiltin::Malloc { size: local(size) }),
    };
    let mstore = |dst, addr, value| RStmt::Assign {
        dst: local(dst),
        expr: RExpr::Builtin(mir::RuntimeBuiltin::Mstore {
            addr: local(addr),
            value: local(value),
        }),
    };
    let start = |stmts, terminator| super::revert_payload_start(&RBlock { stmts, terminator });

    // A payload written into a fresh allocation is not lowered.
    let payload = vec![constant(1), malloc(2, 1), mstore(3, 2, 1)];
    assert_eq!(start(payload.clone(), RTerminator::RevertEmpty), 0);
    assert_eq!(start(payload, RTerminator::Return(None)), 3);
    // A store through the parameter `0` is observable after the trap, and so
    // is everything before it.
    let through_parameter = vec![constant(1), malloc(2, 1), mstore(3, 0, 1), mstore(4, 2, 1)];
    assert_eq!(start(through_parameter, RTerminator::RevertEmpty), 4);
    let before_payload = vec![constant(1), mstore(2, 0, 1), malloc(3, 1), mstore(4, 3, 1)];
    assert_eq!(start(before_payload, RTerminator::RevertEmpty), 2);
}

#[test]
fn stored_references_to_storage_keep_packed_newtype_lanes() {
    // A stored reference to a storage `S`, projected to the value of a packed
    // newtype that shares its slot (`tag`, slot 0 from byte 1) and of one
    // narrowed in a slot of its own (`lone`, slot 2). No source program
    // stores a reference to storage, so the reference is built by hand.
    const SOURCE: &str = r#"
use std::abi::sol::Bytes4
use std::evm::RawStorage

struct S {
    flag: bool,
    tag: Bytes4,
    big: u256,
    lone: Bytes4,
    after: u256,
}

#[inline(never)]
fn probe(_ s: mut S, _ value: u256) -> u256 {
    value
}

msg M {
    #[selector = 1]
    Probe { value: u256 } -> u256,
    #[selector = 2]
    Slot { slot: u256 } -> u256,
    #[selector = 3]
    Dirty { slot: u256, value: u256 },
}

pub contract C {
    mut s: S,

    recv M {
        Probe { value } -> u256 uses (mut s) {
            probe(mut s, value)
        }
        Slot { slot } -> u256 uses (raw: RawStorage) {
            unsafe { raw.sload(slot) }
        }
        Dirty { slot, value } uses (raw: mut RawStorage) {
            unsafe { raw.sstore(slot, value) }
        }
    }
}
"#;
    // A word with `low` in its low bytes, and bit 200 set when `high`.
    let word = |low: u64, high: bool| {
        let mut word = [0; 32];
        word[24..].copy_from_slice(&low.to_be_bytes());
        word[6] = u8::from(high);
        word
    };
    let call = |runtime: &mut EvmRuntime, selector: u8, args: &[[u8; 32]]| {
        let mut calldata = vec![0, 0, 0, selector];
        calldata.extend(args.iter().flatten());
        runtime.call_raw(&calldata, ExecutionOptions::default())
    };
    for opt in [OptLevel::O0, OptLevel::O2] {
        for (field, slot, offset) in [(1, 0, 1), (3, 2, 0)] {
            for write in [false, true] {
                let mut db = DriverDataBase::default();
                let file = db.workspace().touch(
                    &mut db,
                    Url::parse("file:///stored_lane_reference.fe").unwrap(),
                    Some(SOURCE.into()),
                );
                let top_mod = db.top_mod(file);
                let diagnostics = db.run_on_top_mod(top_mod);
                assert!(diagnostics.is_empty(), "{}", diagnostics.format_diags(&db));
                let package = build_runtime_package(&db, top_mod).expect("runtime package");
                let target = package
                    .functions(&db)
                    .iter()
                    .find(|function| function.symbol(&db) == "probe")
                    .expect("probe")
                    .instance(&db);
                let mut body = target.body(&db).clone();
                let [s, value] = [0, 1].map(|idx| body.signature.params[idx].local);
                let RuntimeClass::Ref { pointee, .. } = body.signature.params[0].class.clone()
                else {
                    panic!("`s` is a reference");
                };
                let reference = RLocalId::from_u32(body.locals.len() as u32);
                let read = RLocalId::from_u32(reference.as_u32() + 1);
                let semantic_ty = body.locals[0].semantic_ty;
                body.locals.extend(
                    [
                        RuntimeClass::Ref {
                            pointee,
                            kind: RefKind::Native,
                            view: RefView::Whole,
                        },
                        body.signature.ret.clone().expect("word return"),
                    ]
                    .map(|class| RLocal {
                        semantic_ty,
                        carrier: RuntimeCarrier::Value(class),
                        root: RuntimeLocalRoot::None,
                    }),
                );
                // `s.<field>.val`, through the stored reference.
                let place = RuntimePlace {
                    root: PlaceRoot::Ref(reference),
                    path: Box::new([
                        PlaceElem::Field(FieldIndex(field)),
                        PlaceElem::Field(FieldIndex(0)),
                    ]),
                };
                let stmts = vec![
                    RStmt::Assign {
                        dst: reference,
                        expr: RExpr::NativeRef { value: s },
                    },
                    if write {
                        RStmt::Store {
                            dst: place,
                            src: value,
                        }
                    } else {
                        RStmt::Assign {
                            dst: read,
                            expr: RExpr::Load { place },
                        }
                    },
                ];
                let origin = body.terminator_origins[0];
                body.stmt_origins = vec![vec![origin; stmts.len()]];
                body.terminator_origins = vec![origin];
                body.blocks = vec![RBlock {
                    stmts,
                    terminator: RTerminator::Return(Some(if write { value } else { read })),
                }];
                let mut runtime = deploy_with_body(&db, package, target, body, opt, "C");

                // The shared lane's slot also holds `flag`.
                let rest = u64::from(offset == 1);
                let case = format!("{opt:?}, field {field}, write={write}");
                if write {
                    call(&mut runtime, 3, &[word(slot, false), word(rest, false)]).unwrap();
                    assert!(
                        call(&mut runtime, 1, &[word(1 << 32, false)]).is_err(),
                        "{case}: a value wider than the lane must revert"
                    );
                    call(&mut runtime, 1, &[word(0x11223344, false)]).unwrap();
                    let stored = call(&mut runtime, 2, &[word(slot, false)]).unwrap();
                    assert_eq!(
                        stored.return_data,
                        word(rest | (0x11223344 << (8 * offset)), false),
                        "{case}"
                    );
                } else {
                    // Bits above the lane are not part of its value.
                    let dirty = rest | (0x11223344 << (8 * offset));
                    call(&mut runtime, 3, &[word(slot, false), word(dirty, true)]).unwrap();
                    let read = call(&mut runtime, 1, &[word(0, false)]).unwrap();
                    assert_eq!(read.return_data, word(0x11223344, false), "{case}");
                }
            }
        }
    }
}
