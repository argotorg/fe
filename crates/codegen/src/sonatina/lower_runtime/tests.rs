use common::InputDb;
use contract_harness::{ExecutionOptions, RuntimeInstance as EvmRuntime};
use driver::DriverDataBase;
use mir::{
    ConstScalar, MirDb, PlaceRoot, RBlock, RExpr, RLocal, RLocalId, RStmt, RTerminator, RefKind,
    RefView, RuntimeCarrier, RuntimeClass, RuntimeLocalRoot, RuntimePlace, build_runtime_package,
    verify_runtime_body,
};
use sonatina_ir::{builder::ModuleBuilder, isa::Isa, module::ModuleCtx};
use url::Url;

use super::{FunctionLowerer, ModuleLowerer};
use crate::{
    OptLevel,
    sonatina::{create_evm_isa, emit_runtime_module_sonatina_bytecode_with_options},
};

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
        let mut body = target.body(&db);
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
        let program: &dyn MirDb = &db;
        verify_runtime_body(&db, &program, &body).expect("valid native slot borrow");

        let isa = create_evm_isa();
        let mut lowerer = ModuleLowerer::new(
            &db,
            ModuleBuilder::new(ModuleCtx::new(&isa)),
            isa.inst_set(),
            &package,
            None,
        );
        lowerer.declare_functions().unwrap();
        lowerer.lower_const_regions().unwrap();
        for function in package.functions(&db) {
            let instance = function.instance(&db);
            let selected = if instance == target {
                body.clone()
            } else {
                instance.body(&db)
            };
            let func_ref = lowerer.func_ref(instance).unwrap();
            FunctionLowerer::new(&mut lowerer, selected, func_ref)
                .unwrap()
                .lower()
                .unwrap();
        }
        lowerer.declare_objects().unwrap();
        let (mut artifacts, _) = emit_runtime_module_sonatina_bytecode_with_options(
            &db,
            &package,
            lowerer.finish().0,
            opt,
            false,
            None,
        )
        .expect("compile native slot borrow");
        let artifact = artifacts.remove("SlotContract").unwrap();
        let mut runtime = EvmRuntime::deploy(&hex::encode(artifact.deploy)).unwrap();
        let result = runtime
            .call_raw(&[0, 0, 0, 1], ExecutionOptions::default())
            .unwrap_or_else(|error| panic!("{opt:?}, replace={replace}: {error}"));
        let mut expected = vec![0; 32];
        expected[31] = if replace { 73 } else { 37 };
        assert_eq!(result.return_data, expected, "{opt:?}, replace={replace}");
    }
}
