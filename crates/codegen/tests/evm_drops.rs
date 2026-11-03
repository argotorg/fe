use common::InputDb;
use driver::DriverDataBase;
use fe_codegen::emit_module_sonatina_ir;
use mir::{RExpr, RStmt, build_runtime_package};
use url::Url;

const PROGRAM: &str = r#"
use core::ops::Drop
use core::Option

struct Token { counter: *u256 }

DROP

fn consume(_ token: own Token) {}

fn run(_ c: bool) -> u256 {
    let counter = core::ptr::alloc<u256>()
    let a = Token { counter }
    let b = Option::Some(Token { counter })
    if c {
        consume(a)
    }
    var d = Token { counter }
    d = Token { counter }
    unsafe { *counter }
}

msg Msg {
    #[selector = 0]
    Run { c: bool } -> u256,
}

contract C {
    recv Msg {
        Run { c } -> u256 {
            run(c)
        }
    }
}
"#;

fn evm_ir(drop: &str) -> String {
    emit_evm_ir(PROGRAM.replace("DROP", drop))
}

fn emit_evm_ir(source: String) -> String {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///evm_drops.fe").unwrap();
    db.workspace().touch(&mut db, url.clone(), Some(source));
    let file = db.workspace().get(&db, &url).unwrap();
    let top_mod = db.top_mod(file);
    emit_module_sonatina_ir(&db, top_mod).expect("Sonatina IR should emit")
}

#[test]
fn effect_free_drops_leave_evm_code_unchanged() {
    let none = evm_ir("");
    assert_eq!(evm_ir("impl Drop for Token { fn drop(own self) {} }"), none);
    // A call is an effect only if its callee has one.
    assert_eq!(
        evm_ir(
            "fn same(_ p: *u256) -> *u256 { p }\nimpl Drop for Token { fn drop(own self) { let _ = same(self.counter) } }"
        ),
        none
    );
    let counted =
        evm_ir("impl Drop for Token { fn drop(own self) { unsafe { *self.counter += 1 } } }");
    assert_ne!(counted, none);
    assert!(counted.contains("drop"), "{counted}");
}

const MANY: &str = r#"
use core::mem::needs_drop
use core::ops::Drop

struct Token { counter: *u256 }

TOKEN_DROP

// Many values in raw memory.
struct Many<T> { ptr: *T, len: usize }

MANY_DROP

fn run(_ n: usize) -> u256 {
    let counter = core::ptr::alloc<u256>()
    let _plain = Many { ptr: core::ptr::alloc<u256>(), len: n }
    let _tokens = Many { ptr: core::ptr::alloc<Token>(), len: n }
    unsafe { *counter }
}

msg Msg {
    #[selector = 0]
    Run { n: usize } -> u256,
}

contract C {
    recv Msg {
        Run { n } -> u256 {
            run(n)
        }
    }
}
"#;

#[test]
fn needs_drop_skips_destruction_that_does_nothing_on_evm() {
    let ir = |token_drop: &str, many_drop: bool| {
        let many_drop = if many_drop {
            "impl<T> Drop for Many<T> { fn drop(own self) { if needs_drop<T>() { for i in 0..self.len { unsafe { core::mem::drop(*core::ptr::offset(self.ptr, i)) } } } } }"
        } else {
            ""
        };
        emit_evm_ir(
            MANY.replace("TOKEN_DROP", token_drop)
                .replace("MANY_DROP", many_drop),
        )
    };
    assert_eq!(ir("", true), ir("", false));
    let counted = "impl Drop for Token { fn drop(own self) { unsafe { *self.counter += 1 } } }";
    assert_ne!(ir(counted, true), ir(counted, false));
}

#[test]
fn memory_owners_of_plain_values_are_not_destroyed_on_evm() {
    let ir = emit_evm_ir(
        r#"
use core::{Box, MemArray, MemBuffer, MemVec}

fn run(_ n: usize) -> u256 {
    var values: MemVec<u256> = MemVec::new()
    for i in 0..n {
        values.push(i as u256)
    }
    values.truncate(1)
    let boxed = Box::new(values[0])
    let array = MemArray<u256>::new_zeroed(n)
    var buffer = MemBuffer::with_capacity(n)
    buffer.resize(n)
    boxed.get() + array.len() as u256 + buffer.len() as u256
}

msg Msg {
    #[selector = 0]
    Run { n: usize } -> u256,
}

contract C {
    recv Msg {
        Run { n } -> u256 {
            run(n)
        }
    }
}
"#
        .to_string(),
    );
    // The library's own calls remain at O0, but no `Drop::drop` is emitted.
    assert!(!ir.contains("__drop"), "{ir}");
}

const RECURSIVE: &str = r#"
use core::{Box, MemVec, Option}
use core::ops::Drop

struct Token { counter: *u256 }

TOKEN_DROP

struct Node { token: Token, next: Option<Box<Node>> }

struct Tree { token: Token, children: MemVec<Tree> }

fn run(_ n: u256) -> u256 {
    let counter = core::ptr::alloc<u256>()
    var head: Option<Box<Node>> = Option::None
    var tree = Tree { token: Token { counter }, children: MemVec::new() }
    var i: u256 = 0
    while i < n {
        head = Option::Some(Box::new(Node { token: Token { counter }, next: head }))
        tree.children.push(Tree { token: Token { counter }, children: MemVec::new() })
        i += 1
    }
    unsafe { *counter }
}

msg Msg {
    #[selector = 0]
    Run { n: u256 } -> u256,
}

contract C {
    recv Msg {
        Run { n } -> u256 {
            run(n)
        }
    }
}
"#;

#[test]
fn recursion_alone_does_not_make_owners_destroyed_on_evm() {
    let ir = |token_drop: &str| emit_evm_ir(RECURSIVE.replace("TOKEN_DROP", token_drop));
    // Holding itself adds no effect, so a list or tree of plain values has
    // nothing to destroy.
    let plain = ir("");
    assert!(!plain.contains("__drop"), "{plain}");
    // A part whose drop has an effect is still reached through the recursion.
    let counted = ir("impl Drop for Token { fn drop(own self) { unsafe { *self.counter += 1 } } }");
    assert!(counted.contains("__drop"), "{counted}");
}

#[test]
fn indexed_stores_check_bounds_before_destroying() {
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///evm_drops.fe").unwrap();
    db.workspace().touch(
        &mut db,
        url.clone(),
        Some(
            r#"
use core::ops::Drop

struct Token { counter: *u256 }

impl Drop for Token { fn drop(own self) { unsafe { *self.counter += 1 } } }

// Its only calls are drops.
fn store(_ i: usize, _ counter: *u256) -> u256 {
    var tokens = [Token { counter }, Token { counter }]
    tokens[i] = Token { counter }
    unsafe { *counter }
}

msg Msg {
    #[selector = 0]
    Store { i: usize } -> u256,
}

contract C {
    recv Msg {
        Store { i } -> u256 {
            store(i, core::ptr::alloc<u256>())
        }
    }
}
"#
            .to_string(),
        ),
    );
    let file = db.workspace().get(&db, &url).unwrap();
    let package = build_runtime_package(&db, db.top_mod(file)).expect("runtime package");
    let store = package
        .functions(&db)
        .iter()
        .find(|function| function.symbol(&db).contains("store"))
        .expect("runtime function `store`");
    let body = store.instance(&db).body(&db);
    // The drop of the old `tokens[i]` reads it, so `i` is checked first.
    let first = body
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find(|stmt| {
            matches!(
                stmt,
                RStmt::AssertIndexInBounds { .. }
                    | RStmt::Assign {
                        expr: RExpr::Call { .. },
                        ..
                    }
            )
        });
    assert!(
        matches!(first, Some(RStmt::AssertIndexInBounds { .. })),
        "{body:#?}"
    );
}
