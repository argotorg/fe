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

#[unsafe(export)]
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

#[test]
fn uninhabited_types_emit_no_empty_enums_and_satisfy_sonatina_v4() {
    use rustc_hash::FxHashSet;
    use sonatina_ir::{Type, types::CompoundType};
    use sonatina_verifier::{VerificationLevel, VerifierConfig, verify_module};

    const SOURCE: &str = r#"
use core::intrinsic::clear
use core::option::Option
use core::result::Result

enum Empty {}
enum AllEmpty {
    A(!),
    B(Empty),
}
struct Mixed {
    a: u256,
    b: !,
}
enum Flipped {
    Err(Empty),
    Ok(u256),
}

#[inline(never)]
fn eliminate(_ e: Empty) -> u8 {
    match e {}
}

#[inline(never)]
fn coerce_never(_ x: !) -> u256 {
    x
}

#[inline(never)]
fn dead_proj(_ m: Mixed) -> u256 {
    m.a
}

#[inline(never)]
fn ref_empty(_ r: ref Empty) -> u256 {
    1
}

#[inline(never)]
fn dead_variant_proj(_ r: own Result<Mixed, u256>) -> u256 {
    match r {
        Result::Ok(v) => v
        Result::Err(m) => m.a
    }
}

#[inline(never)]
fn unwrap_flipped(_ f: own Flipped) -> u256 {
    match f {
        Flipped::Ok(v) => v
    }
}

msg UninhabitedMsg {
    #[selector = 0x01]
    ReadResult -> u256,
    #[selector = 0x02]
    SetResult { v: u256 } -> u256,
    #[selector = 0x03]
    ClearResult -> u256,
    #[selector = 0x04]
    ReadOption -> u256,
    #[selector = 0x05]
    CheckSizes -> u256,
    #[selector = 0x06]
    CheckFlipped { v: u256 } -> u256,
    #[selector = 0x07]
    CheckDeadVariant { v: u256 } -> u256,
}

pub contract UninhabitedContract {
    mut res: Result<Empty, u256>,
    mut opt: Option<Empty>,
    mut zero_arr: [Empty; 0],

    recv UninhabitedMsg {
        ReadResult -> u256 uses (res) {
            match res {
                Result::Ok(v) => v
            }
        }
        SetResult { v } -> u256 uses (mut res) {
            res = Result::Ok(v)
            match res {
                Result::Ok(cur) => cur
                Result::Err(ref e) => match e {}
            }
        }
        ClearResult -> u256 uses (mut res, mut zero_arr) {
            unsafe {
                clear(mut res)
                clear(mut zero_arr)
            }
            match res {
                Result::Ok(v) => v
            }
        }
        ReadOption -> u256 uses (opt) {
            match opt {
                Option::None => 11
                Option::Some(ref e) => match e {}
            }
        }
        CheckSizes -> u256 {
            let s0 = core::size_of<!>() as u256
            let s1 = core::size_of<Empty>() as u256
            let s2 = core::size_of<AllEmpty>() as u256
            let s3 = core::size_of<Mixed>() as u256
            let s4 = core::size_of<[Empty; 3]>() as u256
            let s5 = core::size_of<[Empty; 0]>() as u256
            let s6 = core::size_of<*Empty>() as u256
            s0 + s1 + s2 + s3 + s4 + s5 + s6
        }
        CheckFlipped { v } -> u256 {
            unwrap_flipped(Flipped::Ok(v))
        }
        CheckDeadVariant { v } -> u256 {
            dead_variant_proj(Result::Ok(v))
        }
    }
}
"#;

    fn sonatina_ty_inhabited(
        ctx: &ModuleCtx,
        ty: Type,
        visiting: &mut FxHashSet<sonatina_ir::types::CompoundTypeRef>,
    ) -> bool {
        match ty {
            Type::Unit
            | Type::I1
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::I128
            | Type::I256 => true,
            Type::EnumTag(enum_ref) => ctx.with_ty_store(|store| {
                store
                    .enum_data(enum_ref)
                    .is_some_and(|d| !d.variants.is_empty())
            }),
            Type::Compound(cmpd) => {
                if !visiting.insert(cmpd) {
                    return true;
                }
                let data = ctx.with_ty_store(|store| store.resolve_compound(cmpd).clone());
                let inhabited = match data {
                    CompoundType::Ptr(_)
                    | CompoundType::ObjRef(_)
                    | CompoundType::ConstRef(_)
                    | CompoundType::Func { .. } => true,
                    CompoundType::Array { elem, len } => {
                        len == 0 || sonatina_ty_inhabited(ctx, elem, visiting)
                    }
                    CompoundType::Struct(s) => s
                        .fields
                        .iter()
                        .all(|&f| sonatina_ty_inhabited(ctx, f, visiting)),
                    CompoundType::Enum(e) => e.variants.iter().any(|v| {
                        v.fields
                            .iter()
                            .all(|&f| sonatina_ty_inhabited(ctx, f, visiting))
                    }),
                };
                visiting.remove(&cmpd);
                inhabited
            }
        }
    }

    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///uninhabited_v4.fe").unwrap(),
        Some(SOURCE.into()),
    );
    let top_mod = db.top_mod(file);
    let diagnostics = db.run_on_top_mod(top_mod);
    assert!(diagnostics.is_empty(), "{}", diagnostics.format_diags(&db));
    let package = build_runtime_package(&db, top_mod).expect("runtime package");
    mir::verify_runtime_package(&db, package).expect("MIR verification");

    let isa = create_evm_isa();
    let mut lowerer = ModuleLowerer::new(
        &db,
        ModuleBuilder::new(ModuleCtx::new(&isa)),
        isa.inst_set(),
        &package,
        &[],
        &[],
        NativePanics::Trap,
    )
    .unwrap();
    lowerer.declare_functions().unwrap();
    lowerer.lower_bodies().unwrap();
    lowerer.declare_objects().unwrap();
    let (module, _) = lowerer.finish();

    let report = verify_module(&module, &VerifierConfig::for_level(VerificationLevel::Full));
    assert!(report.is_ok(), "Sonatina verification failed:\n{report:?}");

    // Check V4 across every declared type, function signature, and SSA value.
    for func_ref in module.funcs() {
        let (name, args, rets) = module.ctx.func_sig(func_ref, |sig| {
            (
                sig.name().to_owned(),
                sig.args().to_vec(),
                sig.ret_tys().to_vec(),
            )
        });
        for arg in args {
            assert!(
                sonatina_ty_inhabited(&module.ctx, arg, &mut FxHashSet::default()),
                "function `{name}` has uninhabited parameter type {arg:?}",
            );
        }
        for ret in rets {
            assert!(
                sonatina_ty_inhabited(&module.ctx, ret, &mut FxHashSet::default()),
                "function `{name}` has uninhabited return type {ret:?}",
            );
        }
        module.func_store.view(func_ref, |func| {
            for value_id in func.dfg.values.keys() {
                if !func.dfg.has_value(value_id) {
                    continue;
                }
                let ty = func.dfg.value_ty(value_id);
                assert!(
                    sonatina_ty_inhabited(&module.ctx, ty, &mut FxHashSet::default()),
                    "function `{name}` has uninhabited SSA value {value_id:?} of type {ty:?}",
                );
            }
        });
    }

    // Compile and execute on EVM at both O0 and O2.
    let word = |n: u64| {
        let mut out = [0u8; 32];
        out[24..].copy_from_slice(&n.to_be_bytes());
        out
    };
    let call = |rt: &mut EvmRuntime, selector: u8, args: &[[u8; 32]]| {
        let mut calldata = vec![0, 0, 0, selector];
        calldata.extend(args.iter().flatten());
        rt.call_raw(&calldata, ExecutionOptions::default())
            .expect("EVM call should succeed")
            .return_data
    };

    for opt in [OptLevel::O0, OptLevel::O2] {
        let mut lowerer = ModuleLowerer::new(
            &db,
            ModuleBuilder::new(ModuleCtx::new(&isa)),
            isa.inst_set(),
            &package,
            &[],
            &[],
            NativePanics::Trap,
        )
        .unwrap();
        lowerer.declare_functions().unwrap();
        lowerer.lower_bodies().unwrap();
        lowerer.declare_objects().unwrap();
        let (mut artifacts, _) = emit_runtime_module_sonatina_bytecode_with_options(
            &db,
            &package,
            lowerer.finish().0,
            opt,
            false,
            None,
        )
        .unwrap_or_else(|e| panic!("compile at {opt:?}: {e}"));
        let artifact = artifacts.remove("UninhabitedContract").unwrap();
        let mut rt = EvmRuntime::deploy(&hex::encode(artifact.deploy)).unwrap();

        assert_eq!(
            call(&mut rt, 1, &[]),
            word(0),
            "{opt:?} default Result::Ok(0)"
        );
        assert_eq!(
            call(&mut rt, 2, &[word(42)]),
            word(42),
            "{opt:?} set Result::Ok(42)"
        );
        assert_eq!(
            call(&mut rt, 1, &[]),
            word(42),
            "{opt:?} read Result::Ok(42)"
        );
        assert_eq!(
            call(&mut rt, 3, &[]),
            word(0),
            "{opt:?} clear Result::Ok(0)"
        );
        assert_eq!(
            call(&mut rt, 4, &[]),
            word(11),
            "{opt:?} default Option::None"
        );
        assert_eq!(
            call(&mut rt, 5, &[]),
            word(32),
            "{opt:?} size_of sum (*Empty == 32)"
        );
        assert_eq!(
            call(&mut rt, 6, &[word(99)]),
            word(99),
            "{opt:?} Flipped::Ok(99)"
        );
        assert_eq!(
            call(&mut rt, 7, &[word(77)]),
            word(77),
            "{opt:?} Result<Mixed, u256>::Ok(77)"
        );
    }
}

#[test]
fn evm_handover_ir_uses_return_code_and_mload_bytes() {
    use contract_harness::{ExecutionOptions, RuntimeInstance as EvmRuntime};
    use sonatina_ir::inst::evm::{EvmCodeCopy, EvmMloadBytes, EvmReturn, EvmReturnCode};
    use sonatina_verifier::{VerificationLevel, VerifierConfig, verify_module};

    const SOURCE: &str = r#"
use core::ptr::MemBuffer

msg Q {
    #[selector = 1]
    ReadNarrow -> u256,
}

pub contract Contract {
    recv Q {
        ReadNarrow -> u256 {
            var buf = MemBuffer::new(16)
            buf.write_be<u8>(offset: 0, value: 0x12)
            buf.write_be<u16>(offset: 1, value: 0x3456)
            let a: u256 = buf.read_be<u8>(0) as u256
            let b: u256 = buf.read_be<u16>(1) as u256
            (a << 16) | b
        }
    }
}
"#;

    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///evm_part_a_ops.fe").unwrap(),
        Some(SOURCE.into()),
    );
    let top_mod = db.top_mod(file);
    let diagnostics = db.run_on_top_mod(top_mod);
    assert!(diagnostics.is_empty(), "{}", diagnostics.format_diags(&db));
    let package = build_runtime_package(&db, top_mod).expect("runtime package");
    mir::verify_runtime_package(&db, package).expect("MIR verification");

    let isa = create_evm_isa();
    let mut lowerer = ModuleLowerer::new(
        &db,
        ModuleBuilder::new(ModuleCtx::new(&isa)),
        isa.inst_set(),
        &package,
        &[],
        &[],
        NativePanics::Trap,
    )
    .unwrap();
    lowerer.declare_functions().unwrap();
    lowerer.lower_bodies().unwrap();
    lowerer.declare_objects().unwrap();
    let (module, _) = lowerer.finish();

    let report = verify_module(&module, &VerifierConfig::for_level(VerificationLevel::Full));
    assert!(report.is_ok(), "Sonatina verification failed:\n{report:?}");

    let mut saw_return_code = false;
    let mut saw_mload_bytes = false;
    for func_ref in module.funcs() {
        let name = module.ctx.func_sig(func_ref, |sig| sig.name().to_owned());
        module.func_store.view(func_ref, |func| {
            let is_init_root = name.contains("init_root") || name.ends_with("__init");
            for block in func.layout.iter_block() {
                for inst in func.layout.iter_inst(block) {
                    if func.dfg.downcast::<&EvmReturnCode>(inst).is_some() {
                        saw_return_code = true;
                    }
                    if func.dfg.downcast::<&EvmMloadBytes>(inst).is_some() {
                        saw_mload_bytes = true;
                    }
                    if is_init_root {
                        assert!(
                            func.dfg.downcast::<&EvmCodeCopy>(inst).is_none()
                                && func.dfg.downcast::<&EvmReturn>(inst).is_none(),
                            "immut_slots == 0 init root `{name}` should use evm_return_code"
                        );
                    }
                }
            }
        });
    }
    assert!(
        saw_return_code,
        "expected evm_return_code in contract init root"
    );
    assert!(
        saw_mload_bytes,
        "expected evm_mload_bytes for narrow memory loads"
    );

    for opt in [OptLevel::O0, OptLevel::O2] {
        let mut lowerer = ModuleLowerer::new(
            &db,
            ModuleBuilder::new(ModuleCtx::new(&isa)),
            isa.inst_set(),
            &package,
            &[],
            &[],
            NativePanics::Trap,
        )
        .unwrap();
        lowerer.declare_functions().unwrap();
        lowerer.lower_bodies().unwrap();
        lowerer.declare_objects().unwrap();
        let (mut artifacts, _) = emit_runtime_module_sonatina_bytecode_with_options(
            &db,
            &package,
            lowerer.finish().0,
            opt,
            false,
            None,
        )
        .unwrap_or_else(|e| panic!("compile at {opt:?}: {e}"));
        let artifact = artifacts.remove("Contract").unwrap();
        let mut rt = EvmRuntime::deploy(&hex::encode(artifact.deploy)).unwrap();
        let out = rt
            .call_raw(&[0, 0, 0, 1], ExecutionOptions::default())
            .expect("call should succeed")
            .return_data;
        let mut expected = [0u8; 32];
        expected[29..].copy_from_slice(&[0x12, 0x34, 0x56]);
        assert_eq!(out, expected, "{opt:?} narrow read");
    }
}
