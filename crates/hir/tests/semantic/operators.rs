//! The arithmetic mode of the body an operator is written in selects its
//! operation: a built-in operation with explicit overflow behavior on
//! primitive operands, and the checked or the wrapping operator trait
//! otherwise.

use fe_hir::test_db::{HirAnalysisTestDb, find_func};
use fe_hir::{
    analysis::{
        semantic::{
            BinaryOp, CtfeError, EvalFailure, EvalOutcome, GenericSubst, NExpr, NStatementKind,
            Overflow, SemanticInstance, UnaryOp, eval_body_owner_const,
            identity_semantic_instance_key, normalize_semantic_body,
        },
        ty::ty_check::BodyOwner,
    },
    hir_def::{CompBinOp, TopLevelMod},
};

/// The operations and called functions of `name`'s normalized body.
fn operations<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
    name: &str,
) -> (Vec<BinaryOp>, Vec<UnaryOp>, Vec<String>) {
    let func = find_func(db, top_mod, name);
    let instance = SemanticInstance::new(
        db,
        identity_semantic_instance_key(db, BodyOwner::Func(func, None)),
    );
    let normalized = normalize_semantic_body(db, instance).expect("normalized body");
    let (mut binary, mut unary, mut calls) = (Vec::new(), Vec::new(), Vec::new());
    for statement in normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
    {
        let NStatementKind::Define { expr, .. } = &statement.kind else {
            continue;
        };
        match expr {
            NExpr::Binary { op, .. } => binary.push(*op),
            NExpr::Unary { op, .. } => unary.push(*op),
            NExpr::Call { callee, .. } => {
                let BodyOwner::Func(func, _) = callee.key.owner(db) else {
                    continue;
                };
                calls.push(func.name(db).to_opt().unwrap().data(db).clone());
            }
            _ => {}
        }
    }
    (binary, unary, calls)
}

#[test]
fn primitive_operators_are_builtin_operations_with_the_mode_overflow() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "primitive_operators.fe".into(),
        r#"
fn checked(x: i8, y: i8) -> bool {
    var z = x + y
    z *= -x
    z / y == z % y
}

#[arithmetic(unchecked)]
fn unchecked(x: i8, y: i8) -> bool {
    var z = x + y
    z *= -x
    z / y == z % y
}

#[arithmetic(unchecked)]
fn literal_typed_late() -> u8 {
    var z = 255
    z += 1
    z ** 2
}

fn bit_not_typed_late() -> u8 {
    let a = 5
    !a | ~a
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    for (name, overflow) in [
        ("checked", Overflow::Checked),
        ("unchecked", Overflow::Wrapping),
    ] {
        let (binary, unary, calls) = operations(&db, top_mod, name);
        assert_eq!(
            binary,
            [
                BinaryOp::Add(overflow),
                BinaryOp::Mul(overflow),
                BinaryOp::Div(overflow),
                BinaryOp::Rem,
                BinaryOp::Comp(CompBinOp::Eq),
            ],
            "{name}"
        );
        assert_eq!(unary, [UnaryOp::Neg(overflow)], "{name}");
        assert!(calls.is_empty(), "{name}: {calls:?}");
    }

    let (binary, _, calls) = operations(&db, top_mod, "literal_typed_late");
    assert_eq!(
        binary,
        [
            BinaryOp::Add(Overflow::Wrapping),
            BinaryOp::Pow(Overflow::Wrapping)
        ]
    );
    assert!(calls.is_empty(), "{calls:?}");

    let (_, unary, _) = operations(&db, top_mod, "bit_not_typed_late");
    assert_eq!(unary, [UnaryOp::BitNot, UnaryOp::BitNot]);
}

#[test]
fn unchecked_operators_call_the_wrapping_traits_a_type_implements() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "wrapping_traits.fe".into(),
        r#"
use core::ops::{Add, AddAssign, Div, Neg, SubAssign, WrappingAdd, WrappingNeg}

struct Money { cents: i256 }
impl Copy for Money {}

impl Add for Money {
    fn add(own self, _ other: own Money) -> Money { Money { cents: self.cents + other.cents } }
}
impl AddAssign for Money {
    fn add_assign(mut self, _ other: own Money) { self.cents += other.cents }
}
impl Div<i256> for Money {
    fn div(own self, _ other: own i256) -> Money { Money { cents: self.cents / other } }
}
impl Neg for Money {
    fn neg(own self) -> Money { Money { cents: -self.cents } }
}
impl WrappingAdd for Money {
    #[arithmetic(unchecked)]
    fn wrapping_add(self, _ other: Money) -> Money { Money { cents: self.cents + other.cents } }
}
impl WrappingNeg for Money {
    #[arithmetic(unchecked)]
    fn wrapping_neg(self) -> Money { Money { cents: -self.cents } }
}

fn checked(a: Money, b: Money) -> Money {
    var c = a + b
    c += -a
    c / 2
}

#[arithmetic(unchecked)]
fn unchecked(a: Money, b: Money) -> Money {
    var c = a + b
    c += -a
    c / 2
}

// A type without the wrapping traits calls the checked ones in either mode.
struct Points { value: u8 }
impl Copy for Points {}

impl Add for Points {
    fn add(own self, _ other: own Points) -> Points { Points { value: self.value + other.value } }
}
impl SubAssign<u8> for Points {
    fn sub_assign(mut self, _ other: own u8) { self.value -= other }
}

#[arithmetic(unchecked)]
fn checked_only(a: Points, b: Points) -> Points {
    var c = a + b
    c -= 1
    c
}

// A type parameter implements what its bounds say.
#[arithmetic(unchecked)]
fn generic<T: Add<Output = T>>(_ a: own T, _ b: own T) -> T {
    a + b
}

#[arithmetic(unchecked)]
fn generic_wrapping<T: Add<Output = T> + WrappingAdd<Output = T>>(_ a: own T, _ b: own T) -> T {
    a + b
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    for (name, expected) in [
        ("checked", &["add", "neg", "add_assign", "div"][..]),
        (
            "unchecked",
            &["wrapping_add", "wrapping_neg", "wrapping_add", "div"],
        ),
        ("checked_only", &["add", "sub_assign"]),
        ("generic", &["add"]),
        ("generic_wrapping", &["wrapping_add"]),
    ] {
        let (binary, unary, calls) = operations(&db, top_mod, name);
        assert!(binary.is_empty() && unary.is_empty(), "{name}");
        assert_eq!(calls, expected, "{name}");
    }
}

#[test]
fn ctfe_evaluates_the_explicit_overflow() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "ctfe_overflow.fe".into(),
        r#"
const fn checked_add() -> u8 {
    let x: u8 = 255
    x + 1
}

#[arithmetic(unchecked)]
const fn wrapping_add() -> u8 {
    let x: u8 = 255
    x + 1
}

#[arithmetic(unchecked)]
const fn wrapping_signed_div() -> i8 {
    let x: i8 = -128
    x / -1
}

// A wrapping `**` reduces modulo the type's width, with a negative
// exponent read as an unsigned word, as at runtime.
#[arithmetic(unchecked)]
const fn wrapping_pow_huge_exponent() -> u256 {
    let x: u256 = 2
    x ** 4294967296
}

#[arithmetic(unchecked)]
const fn wrapping_pow_long() -> u256 {
    let x: u256 = 3
    x ** 20000000
}

#[arithmetic(unchecked)]
const fn wrapping_pow_negative_exponent() -> i8 {
    let x: i8 = 3
    x ** -1
}

#[arithmetic(unchecked)]
const fn unchecked_div_by_zero() -> u8 {
    let x: u8 = 1
    x / 0
}

#[arithmetic(unchecked)]
const fn unchecked_rem_by_zero() -> u8 {
    let x: u8 = 1
    x % 0
}

// A called function's arithmetic is its own: `add` is checked.
const fn add(_ a: u8, _ b: u8) -> u8 {
    a + b
}

#[arithmetic(unchecked)]
const fn checked_callee() -> u8 {
    add(255, 1)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let eval = |name| {
        let owner = BodyOwner::Func(find_func(&db, top_mod, name), None);
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]))
    };
    let root_error = |name| {
        let EvalOutcome::Failed(EvalFailure::Ctfe(mut error)) = eval(name) else {
            panic!("{name} should fail: {:?}", eval(name));
        };
        while let CtfeError::CalleeError { source, .. } = error {
            error = *source;
        }
        error
    };

    for (name, expected) in [
        ("wrapping_add", "0"),
        ("wrapping_signed_div", "-128"),
        ("wrapping_pow_huge_exponent", "0"),
        (
            "wrapping_pow_long",
            "51833636103163324557804808377416182321812292029380532382974275346112971727873",
        ),
        ("wrapping_pow_negative_exponent", "-85"),
    ] {
        let EvalOutcome::Ready(value) = eval(name) else {
            panic!("{name}: {:?}", eval(name));
        };
        assert_eq!(value.pretty_print(&db), expected, "{name}");
    }
    for name in ["checked_add", "checked_callee"] {
        assert!(
            matches!(root_error(name), CtfeError::ArithmeticOverflow { .. }),
            "{name}"
        );
    }
    for name in ["unchecked_div_by_zero", "unchecked_rem_by_zero"] {
        assert!(
            matches!(root_error(name), CtfeError::DivisionByZero { .. }),
            "{name}"
        );
    }
}

#[test]
fn constant_expressions_take_the_mode_of_their_function() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "lexical_constants.fe".into(),
        r#"
const fn pick<const N: u8>() -> u8 { N }

#[arithmetic(unchecked)]
const fn generic_arg() -> u8 {
    pick<{ 255 + 1 }>()
}

// Evaluated by full CTFE rather than the early integer evaluator.
#[arithmetic(unchecked)]
const fn generic_arg_block() -> u8 {
    pick<{
        let x: u8 = 255
        x + 2
    }>()
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    for (name, expected) in [("generic_arg", "0"), ("generic_arg_block", "1")] {
        let owner = BodyOwner::Func(find_func(&db, top_mod, name), None);
        let value =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
        let EvalOutcome::Ready(value) = value else {
            panic!("{name}: {value:?}");
        };
        assert_eq!(value.pretty_print(&db), expected, "{name}");
    }
}
