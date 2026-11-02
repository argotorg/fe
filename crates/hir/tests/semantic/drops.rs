use fe_hir::analysis::semantic::{
    DropStep, SemanticInstance, drop_elaboration, identity_semantic_instance_key,
};
use fe_hir::analysis::ty::ty_check::BodyOwner;
use fe_hir::test_db::HirAnalysisTestDb;

/// Each drop of function `name`, in statement order: its parts as paths
/// (`.n` a field, `:v.n` a variant's field, `[i]` an element, `*` the whole
/// target), each followed by `?` and its flags when only some paths reach
/// it initialized, or `-` for a drop with nothing to destroy.
fn drops(source: &str, name: &str) -> Vec<String> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("drops.fe".into(), source);
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let func = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|ident| ident.data(&db) == name)
        })
        .unwrap_or_else(|| panic!("no function `{name}`"));
    let key = identity_semantic_instance_key(&db, BodyOwner::Func(func, None));
    let instance = SemanticInstance::new(&db, key);
    let elaboration = drop_elaboration(&db, instance).expect("a checked body");
    elaboration
        .drops
        .iter()
        .map(|(_, parts)| {
            if parts.is_empty() {
                return "-".to_string();
            }
            parts
                .iter()
                .map(|part| {
                    let mut text: String = part
                        .path
                        .iter()
                        .map(|step| match step {
                            DropStep::Field(field) => format!(".{field}"),
                            DropStep::Variant { variant, field } => format!(":{variant}.{field}"),
                            DropStep::Index(index) => format!("[{index}]"),
                        })
                        .collect();
                    if text.is_empty() {
                        text.push('*');
                    }
                    if !part.flags.is_empty() {
                        text.push('?');
                        text.push_str(
                            &part
                                .flags
                                .iter()
                                .map(u32::to_string)
                                .collect::<Vec<_>>()
                                .join(","),
                        );
                    }
                    text
                })
                .collect::<Vec<_>>()
                .join(" ")
        })
        .collect()
}

const PRELUDE: &str = r#"
use core::ops::Drop
use core::Option

struct Buf { id: u64 }
impl Drop for Buf { fn drop(own self) {} }

struct Pair { a: Buf, b: Buf }

fn make(_ id: u64) -> Buf { Buf { id } }
fn consume(_ b: own Buf) -> u64 { b.id }
fn view(_ b: Buf) -> u64 { b.id }
"#;

fn check(body: &str, name: &str, expected: &[&str]) {
    assert_eq!(drops(&format!("{PRELUDE}{body}"), name), expected);
}

#[test]
fn bindings_drop_at_scope_end_unless_moved() {
    check(
        "fn f() -> u64 { let a = make(1)\n let b = make(2)\n consume(a) + view(b) }",
        "f",
        // The temporaries, the argument, `b`, and the moved `a`.
        &["-", "-", "-", "*", "-"],
    );
}

#[test]
fn a_conditional_move_is_flagged() {
    check(
        "fn f(_ c: bool) { let a = make(1)\n if c { consume(a) } }",
        "f",
        &["-", "-", "*?0"],
    );
}

#[test]
fn a_partial_move_drops_the_rest() {
    check(
        "fn f(_ p: own Pair) -> u64 { let a = p.a\n consume(a) }",
        "f",
        &["-", "-", "-", ".1"],
    );
}

#[test]
fn reassignment_drops_the_old_value() {
    check(
        "fn f() { var x = make(1)\n x = make(2)\n consume(x) }",
        "f",
        // The first value's temporary, `x` before the store, the second
        // value's temporary, the argument and `x` at the end.
        &["-", "*", "-", "-", "-"],
    );
}

#[test]
fn a_returned_binding_is_not_dropped() {
    check(
        "fn f(_ c: bool) -> Buf { let a = make(1)\n let b = make(2)\n if c { return a }\n b }",
        "f",
        // The temporaries, `b` where `a` returns, and `a` where `b` does.
        &["-", "-", "*", "*"],
    );
}

#[test]
fn a_matched_payload_moved_in_one_arm_is_flagged() {
    check(
        "fn f(_ o: own Option<Buf>) -> u64 { match o { Option::Some(b) => consume(b)\n Option::None => 0 } }",
        "f",
        &["-", "-", ":1.0?0"],
    );
}

#[test]
fn types_without_drop_glue_are_not_dropped() {
    check(
        "struct Plain { x: u64 }\nfn plain() -> Plain { Plain { x: 1 } }\nfn f() -> u64 { let p = plain()\n p.x }",
        "f",
        &[],
    );
}

#[test]
fn a_let_else_drops_its_bindings_only_where_they_are_bound() {
    check(
        "fn f(_ o: own Option<Buf>) -> u64 { let Option::Some(b) = o else { return 0 }\n consume(b) }",
        "f",
        // The argument, the moved `b` and `o` without its payload, then `o`
        // whole where the `else` returns.
        &["-", "-", "-", "*"],
    );
    check(
        "fn f(_ o: own Option<Buf>) -> u64 { let Option::Some(b) = o else { return 0 }\n view(b) }",
        "f",
        &["*", "-", "*"],
    );
}

#[test]
fn a_destroyed_temporary_does_not_hold_the_accesses_it_carries() {
    // The tuple lives as long as `n`, but its access ends at `n`'s last use.
    check(
        "struct Cell { n: u64 }\nimpl Cell { fn peek(mut? self) -> (Buf, mut? u64) { (make(1), mut? self.n) } }\nfn f() -> u64 { var cell = Cell { n: 1 }\n let (_, ref n) = cell.peek()\n let m = n + 1\n cell.n = 5\n m }",
        "f",
        // The tuple at the end, and the moved `make(1)`.
        &["*", "-"],
    );
}

#[test]
fn a_question_mark_exit_destroys_what_its_path_holds() {
    check(
        "fn half(_ x: u64) -> Option<u64> { if x == 0 { Option::None } else { Option::Some(x / 2) } }\nfn f(_ n: u64) -> Option<Buf> { let b = make(1)\n let h = half(n)?\n Option::Some(b) }",
        "f",
        // Only where `?` returns is `b` destroyed; elsewhere it moved.
        &["-", "-", "-", "*"],
    );
    check(
        "use core::Result\nfn f(_ r: own Result<Buf, u64>) -> Result<Buf, Buf> { let b = make(1)\n let v = r?\n Result::Ok(make(v + b.id)) }",
        "f",
        // `b` on both paths, and `r` whole only on the path that goes on:
        // where `?` returns, its error moved out.
        &["-", "-", "*", "*", "-", "*", "-"],
    );
}

#[test]
fn a_branch_that_leaves_does_not_destroy_the_value_of_its_siblings() {
    check(
        "fn f(_ o: own Option<Buf>) -> u64 { let b = match o { Option::Some(b) => b\n Option::None => return 0 }\n consume(b) }",
        "f",
        // Only `o` is initialized where the `None` arm returns.
        &["-", "-", "-", "-", "-", "-", "*"],
    );
    check(
        "fn f(_ c: bool) -> u64 { let b = if c { make(1) } else { return 0 }\n view(b) }",
        "f",
        // The `else` returns holding nothing; `b` lives to the end.
        &["-", "-", "*"],
    );
}
