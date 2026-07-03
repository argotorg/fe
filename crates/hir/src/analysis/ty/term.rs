//! Normalized const-predicate terms — the M5 term language.
//!
//! This module gives where-clause const predicates (`T::SIZE >= 50`,
//! `1 + 1 == 3`, `check<T>()`) a *semantic identity*: a salsa-interned,
//! normalized term ([`TermId`]). Stage 2 `Eq` goals, evidence keys, and the
//! predicate prover compare predicates by this identity instead of by raw HIR
//! syntax, so two occurrences of the same predicate intern to the same term
//! and law proofs stop failing with "expected X, found X".
//!
//! The module is deliberately standalone: it consumes existing HIR and
//! name-resolution queries, and is itself consumed by `ty_check/mod.rs`
//! (declaration-side const-predicate checking), `method_cmp.rs` (method
//! conformance), `diagnostics.rs` (rendering), `provider_executor.rs`
//! (steering comparisons via `compare_nats`), and `test_db.rs` (test
//! wiring). Its three contracts are:
//!
//! 1. **Lowering** ([`lower_hir_to_term`]) is *partial*: only the predicate
//!    expression fragment is accepted, and every rejected construct gets a
//!    precise, per-variant error ([`TermLowerError`]) — never a catch-all.
//! 2. **Normalization** ([`normalize_term`]) is bottom-up, value-preserving
//!    constant folding plus determinism-only operand ordering. It is *not* a
//!    semantic canonicalizer: distinct comparison relations are never merged
//!    (`x >= 50` and `50 <= x` stay different terms, by design — see
//!    [`normalize_term`]).
//! 3. **Versioning** ([`TERM_LANG_VERSION`]) tracks the shape of the term
//!    language so persisted/derived artifacts keyed on terms can detect
//!    staleness. The in-module ledger test fails whenever the shape changes
//!    without a version bump.
//!
//! Provenance: the accepted expression fragment mirrors what effort2's
//! const-predicate matcher actually walked
//! (`ty_check/const_predicate_prover.rs:165-256`: literals, binary/unary
//! ops, paths, calls) plus what the M5 port map's gate semantics require
//! (CTFE-opaque const-fn applications such as `check<T>()`). The
//! application shape mirrors this branch's `ConstExpr::UserConstFnCall`
//! (`analysis/ty/const_expr.rs`).

use num_bigint::BigUint;
use num_traits::Zero;
use salsa::Update;
use salsa::plumbing::AsId;

use crate::analysis::HirAnalysisDb;
use crate::analysis::name_resolution::{PathRes, resolve_path};
use crate::hir_def::{
    ArithBinOp, BinOp, Body, CallableDef, CompBinOp, Const, Expr, ExprId, Func, IdentId, IntegerId,
    LitKind, LogicalBinOp, Partial, PathId, UnOp,
};

use super::binder::Binder;
use super::const_ty::{ConstTyData, EvaluatedConstTy, HoleAnchor, HoleMinter, LayoutHoleArgSite};
use super::trait_def::TraitInstId;
use super::trait_resolution::PredicateListId;
use super::ty_def::{TyBase, TyData, TyId};
use super::ty_lower::lower_generic_arg_list;

/// Version of the term language.
///
/// # Staleness contract
///
/// `TERM_LANG_VERSION` MUST be bumped (and a new entry appended to the
/// in-module shape ledger — see `TERM_LANG_LEDGER` in the test suite)
/// whenever any of the following changes:
///
/// * the set, names, or payloads of [`TermNode`] variants;
/// * the set of operators in [`TermArithOp`] / [`TermCmpOp`];
/// * any normalization rule (additions, removals, or semantic changes —
///   the rules are listed in [`NORMALIZATION_RULES`]).
///
/// Consumers that persist or cache anything keyed on a [`TermId`]'s
/// *meaning* (evidence keys, proof caches, serialized obligations) must
/// store this version next to the key and treat a mismatch as a cache miss.
/// The `term_lang_version_ledger_matches_shape` test enforces the bump: the
/// ledger records a fingerprint of the shape descriptors, so a shape or
/// rule change without a matching version bump fails the test.
pub const TERM_LANG_VERSION: u32 = 1;

/// One descriptor string per [`TermNode`] variant.
///
/// Hand-maintained, but compile-coupled to the enum: the test-suite's
/// `shape_of` helper exhaustively destructures every variant (full field
/// lists, no `..`), so adding/removing a variant or field fails to compile
/// until both `shape_of` and this list are updated, which changes
/// [`term_lang_fingerprint`] and trips the ledger test.
pub const TERM_LANG_SHAPE: &[&str] = &[
    "Int(IntegerId)",
    "Bool(bool)",
    "ConstParam(TyId:const-ty-param)",
    "ConstRef(Const)",
    "AssocConst{inst:TraitInstId,name:IdentId}",
    "Arith{op:TermArithOp,lhs:TermId,rhs:TermId}",
    "Cmp{op:TermCmpOp,lhs:TermId,rhs:TermId}",
    "And(TermId,TermId)",
    "Or(TermId,TermId)",
    "Not(TermId)",
    "App{callee:Func,generic_args:Vec<TyId>,args:Vec<TermId>}",
];

/// Descriptor strings for [`TermArithOp`], in declaration order.
pub const TERM_ARITH_OP_SHAPE: &[&str] = &["Add", "Sub", "Mul", "Div", "Rem"];

/// Descriptor strings for [`TermCmpOp`], in declaration order.
pub const TERM_CMP_OP_SHAPE: &[&str] = &["Eq", "Ne", "Lt", "Le", "Gt", "Ge"];

/// The normalization rules implemented by [`normalize_term`], as stable
/// rule identifiers.
///
/// This list is part of the versioned term-language shape: editing it (to
/// reflect a rule change) alters [`term_lang_fingerprint`] and therefore
/// requires a [`TERM_LANG_VERSION`] bump. Keep the identifiers in sync with
/// the implementation.
pub const NORMALIZATION_RULES: &[&str] = &[
    // Exact natural-number folding. Sub only when lhs >= rhs (no negative
    // naturals); Div/Rem only when the divisor is a nonzero literal, so
    // CTFE-failing terms like `1 / 0` are left intact.
    "fold-arith-int-literals",
    // All six relations fold on two integer literals.
    "fold-cmp-int-literals",
    // Only Eq/Ne fold on two bool literals (bools are unordered here).
    "fold-cmp-bool-eq-ne",
    // And(true,x)=>x; And(false,x)=>false; Or(true,x)=>true; Or(false,x)=>x;
    // And(x,true)=>x; Or(x,false)=>x. And(x,false)/Or(x,true) are NOT folded:
    // `&&`/`||` short-circuit left-to-right, so dropping a possibly
    // CTFE-failing lhs would not be value-preserving.
    "fold-bool-connective-literal",
    // Not(Bool(b)) => Bool(!b).
    "fold-not-literal",
    // Not(Not(x)) => x.
    "elim-double-negation",
    // Operands of Add/Mul/Eq/Ne are put in interned-id order. Determinism
    // only — NOT semantic canonicalization (see `normalize_term`).
    "order-commutative-operands-by-intern-id",
];

/// A salsa-interned, structurally shared term of the const-predicate term
/// language.
///
/// Interning makes term identity O(1) (`==` on ids) and makes structurally
/// equal predicates — wherever they were written — the same term. Terms form
/// DAGs: shared subterms intern once.
#[salsa::interned]
#[derive(Debug)]
pub struct TermId<'db> {
    /// The term's root node.
    #[return_ref]
    pub data: TermNode<'db>,
}

impl<'db> TermId<'db> {
    /// Interns an integer-literal term.
    pub fn int(db: &'db dyn HirAnalysisDb, value: BigUint) -> Self {
        Self::new(db, TermNode::Int(IntegerId::new(db, value)))
    }

    /// Interns a boolean-literal term.
    pub fn bool(db: &'db dyn HirAnalysisDb, value: bool) -> Self {
        Self::new(db, TermNode::Bool(value))
    }
}

/// A node of the term language.
///
/// Variant selection follows what effort2's predicates actually exercised
/// (integer/bool literals, const params, arithmetic, comparisons, calls —
/// `const_predicate_prover.rs:165-256`, `hir/tests/constraints.rs`) plus the
/// port map's gate-semantics requirements (associated consts `T::SIZE`,
/// CTFE-opaque const-fn applications). String literals are deliberately
/// excluded: no shipped predicate needed them, and excluding them keeps
/// folding total over naturals and bools.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum TermNode<'db> {
    /// An unsigned integer literal (Fe integer literals are non-negative;
    /// negation is not part of the predicate fragment).
    ///
    /// Invariant: the term language models *mathematical naturals*. Typed
    /// width, overflow, and trapping are CTFE's concern, not identity's.
    Int(IntegerId<'db>),

    /// A boolean literal (`where false` is a real predicate — effort2
    /// exercised it).
    Bool(bool),

    /// A reference to a const generic parameter (`N`).
    ///
    /// Invariant: the payload `TyId`'s data is `TyData::ConstTy(c)` with
    /// `c.data` a `ConstTyData::TyParam`. Lowering only constructs the
    /// variant from such types. Identity is the interned parameter type, so
    /// the same parameter referenced twice is the same term, while equally
    /// named parameters of different owners stay distinct (instantiation /
    /// binder-folding is the future prover's job, not this module's).
    ConstParam(TyId<'db>),

    /// A reference to a top-level `const` item.
    ///
    /// Invariant: purely symbolic. The const's value is never inlined and
    /// never evaluated here — CTFE must not run inside identity formation
    /// (port-map rule 4: the solver/identity layer never evaluates).
    ConstRef(Const<'db>),

    /// A trait-associated const used through a (possibly generic) receiver,
    /// e.g. `T::SIZE` resolved to `<T as HasSize>::SIZE`.
    ///
    /// Invariant: `inst.self_ty(db)` is the receiver the const was selected
    /// for. Unlike `AssocConstUse`, no origin scope or assumption set is
    /// stored: identity must not depend on the use site.
    AssocConst {
        /// The trait instance the const was selected from.
        inst: TraitInstId<'db>,
        /// The associated const's name within the trait.
        name: IdentId<'db>,
    },

    /// A binary arithmetic node over the supported operator subset.
    Arith {
        /// The arithmetic operator.
        op: TermArithOp,
        /// Left operand.
        lhs: TermId<'db>,
        /// Right operand.
        rhs: TermId<'db>,
    },

    /// A binary comparison node.
    ///
    /// Invariant: the relation is kept exactly as written (modulo literal
    /// folding and Eq/Ne operand ordering). Cross-relation rewrites are
    /// forbidden — see [`normalize_term`].
    Cmp {
        /// The comparison relation.
        op: TermCmpOp,
        /// Left operand.
        lhs: TermId<'db>,
        /// Right operand.
        rhs: TermId<'db>,
    },

    /// Logical conjunction (`&&`).
    ///
    /// Invariant: operand order is preserved (no commutative reordering):
    /// `&&` short-circuits, so order is observable through CTFE errors.
    And(TermId<'db>, TermId<'db>),

    /// Logical disjunction (`||`). Same ordering invariant as [`Self::And`].
    Or(TermId<'db>, TermId<'db>),

    /// Logical negation (`!`).
    Not(TermId<'db>),

    /// A CTFE-opaque application of a const function symbol, e.g.
    /// `check<T>(5)`.
    ///
    /// Invariants:
    /// * `callee.is_const(db)` holds (enforced by lowering); evaluation is
    ///   still CTFE's job — the term only carries the symbol.
    /// * `generic_args` are the explicit generic arguments written on the
    ///   callee path, lowered to types (empty when none are written;
    ///   inference-completed arguments are a type-checker concern and never
    ///   appear here).
    /// * Call-site argument labels are dropped: once the callee symbol is
    ///   fixed, argument positions determine semantics, and label validity
    ///   is the type checker's concern.
    App {
        /// The const function being applied.
        callee: Func<'db>,
        /// Explicitly written generic arguments of the callee path.
        generic_args: Vec<TyId<'db>>,
        /// The value arguments, in call order.
        args: Vec<TermId<'db>>,
    },
}

/// Arithmetic operators of the term language: exactly `+ - * / %`.
///
/// Deliberately narrower than HIR's [`ArithBinOp`]: `**`, shifts, bitwise
/// ops, and ranges are not part of the predicate fragment and are rejected
/// during lowering with [`TermLowerError::UnsupportedArithOp`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum TermArithOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Rem,
}

impl TermArithOp {
    /// All arithmetic operators, in declaration order.
    pub const ALL: [Self; 5] = [Self::Add, Self::Sub, Self::Mul, Self::Div, Self::Rem];
}

/// Comparison relations of the term language.
///
/// Each relation is its own identity: `Ge` is *not* the same term head as a
/// swapped `Le`, even though they are logically interchangeable. See
/// [`normalize_term`] for why.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum TermCmpOp {
    /// `==` (symmetric: operands are ordered for determinism)
    Eq,
    /// `!=` (symmetric: operands are ordered for determinism)
    Ne,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `>`
    Gt,
    /// `>=`
    Ge,
}

impl TermCmpOp {
    /// All comparison relations, in declaration order.
    pub const ALL: [Self; 6] = [Self::Eq, Self::Ne, Self::Lt, Self::Le, Self::Gt, Self::Ge];
}

/// A named, unsupported HIR expression construct encountered during
/// lowering.
///
/// One variant per rejected [`Expr`] form, so diagnostics (and tests) can
/// name exactly what the predicate fragment does not admit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnsupportedExprKind {
    /// A block expression (`{ .. }`).
    Block,
    /// A cast expression (`x as T`).
    Cast,
    /// The `assert!` builtin.
    Assert,
    /// A method call (`x.f(..)`).
    MethodCall,
    /// A record construction (`S { .. }`).
    RecordInit,
    /// A field access (`x.f`, `x.0`).
    Field,
    /// A tuple literal (`(a, b)`).
    Tuple,
    /// An array literal (`[a, b]`).
    Array,
    /// An array-repeat literal (`[x; N]`).
    ArrayRep,
    /// An `if` expression.
    If,
    /// A `match` expression.
    Match,
    /// An assignment (`x = y`).
    Assign,
    /// A compound assignment (`x += y`).
    AugAssign,
    /// A `with (..) { .. }` effect-binding expression.
    With,
    /// A `quote { .. }` template.
    Quote,
    /// A `${..}` splice hole.
    QuoteHole,
    /// A `base.${..}` member splice hole.
    QuoteFieldHole,
}

/// A precise lowering failure: the construct that the predicate fragment
/// does not admit, or the resolution step that failed.
///
/// There is intentionally no catch-all variant; every rejection names what
/// was rejected.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TermLowerError<'db> {
    /// The expression (or a required sub-expression / path) is a syntax
    /// hole (`Partial::Absent`); the parser has already reported it.
    AbsentExpr(ExprId),
    /// A syntactic construct outside the predicate fragment, named by
    /// [`UnsupportedExprKind`].
    UnsupportedExpr(ExprId, UnsupportedExprKind),
    /// An arithmetic operator outside `+ - * / %` (e.g. `**`, `<<`, `&`).
    UnsupportedArithOp(ExprId, ArithBinOp),
    /// The indexing operator `[..]`.
    IndexOperator(ExprId),
    /// A unary operator other than `!` (e.g. `-x`, `~x`).
    UnsupportedUnOp(ExprId, UnOp),
    /// A string literal; strings are not part of the term language.
    StringLiteral(ExprId),
    /// Path resolution failed (unresolved name, ambiguity, or invalid
    /// segment); name resolution reports the details.
    UnresolvedPath(ExprId, PathId<'db>),
    /// The path resolved, but not to a const value. The payload names the
    /// resolved domain (`"type"`, `"trait"`, `"module"`, ...).
    NonTermPath(ExprId, &'static str),
    /// The path resolved to a const-typed entity whose const type carries
    /// no symbolic identity usable here (inference variable, layout hole,
    /// abstract/unevaluated const, or a non-scalar evaluated value).
    /// Defensive: unreachable from source predicates today.
    OpaqueConstTy(ExprId),
    /// A call whose callee is not a path expression (e.g. `(f)(x)` chains
    /// through a non-path callee).
    CalleeNotPath(ExprId),
    /// A call whose callee path resolved to something other than a plain
    /// function. The payload names the resolved domain.
    CalleeNotFunc(ExprId, &'static str),
    /// A call to a function that is not `const fn`; only CTFE-evaluable
    /// symbols may appear in predicate terms.
    CalleeNotConst(ExprId, Func<'db>),
}

/// Lowers the HIR expression `expr` of `body` (a where-clause const
/// predicate fragment) to an interned, *unnormalized* term.
///
/// * `assumptions` is the predicate set in scope at the predicate's owner
///   (used by path resolution to select trait-associated consts such as
///   `T::SIZE`); pass `PredicateListId::empty_list(db)` when none apply.
/// * Paths are resolved from `body.scope()`: predicate fragments contain no
///   local bindings, so block scoping is irrelevant.
/// * The result is **not** normalized; pair with [`normalize_term`] when an
///   identity is needed.
///
/// Returns a [`TermLowerError`] naming the first construct outside the
/// fragment (evaluation order: operators are checked before operands;
/// operands are lowered left to right).
pub fn lower_hir_to_term<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expr: ExprId,
    assumptions: PredicateListId<'db>,
) -> Result<TermId<'db>, TermLowerError<'db>> {
    let Partial::Present(expr_data) = expr.data(db, body) else {
        return Err(TermLowerError::AbsentExpr(expr));
    };

    match expr_data {
        Expr::Lit(LitKind::Int(value)) => Ok(TermId::new(db, TermNode::Int(*value))),
        Expr::Lit(LitKind::Bool(value)) => Ok(TermId::new(db, TermNode::Bool(*value))),
        Expr::Lit(LitKind::String(_)) => Err(TermLowerError::StringLiteral(expr)),

        Expr::Bin(lhs, rhs, op) => lower_bin(db, body, expr, *lhs, *rhs, *op, assumptions),

        Expr::Un(inner, op) => match op {
            UnOp::Not => {
                let inner = lower_hir_to_term(db, body, *inner, assumptions)?;
                Ok(TermId::new(db, TermNode::Not(inner)))
            }
            UnOp::Plus | UnOp::Minus | UnOp::BitNot | UnOp::Mut | UnOp::Ref => {
                Err(TermLowerError::UnsupportedUnOp(expr, *op))
            }
        },

        Expr::Path(path) => {
            let Partial::Present(path) = path else {
                return Err(TermLowerError::AbsentExpr(expr));
            };
            lower_value_path(db, body, expr, *path, assumptions)
        }

        Expr::Call(callee, args) => {
            let (callee_func, generic_args) = lower_callee(db, body, expr, *callee, assumptions)?;
            let mut term_args = Vec::with_capacity(args.len());
            for arg in args {
                // Argument labels are dropped by design: see `TermNode::App`.
                term_args.push(lower_hir_to_term(db, body, arg.expr, assumptions)?);
            }
            Ok(TermId::new(
                db,
                TermNode::App {
                    callee: callee_func,
                    generic_args,
                    args: term_args,
                },
            ))
        }

        Expr::Block(_) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::Block,
        )),
        Expr::Cast(..) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::Cast,
        )),
        Expr::Assert(_) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::Assert,
        )),
        Expr::MethodCall(..) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::MethodCall,
        )),
        Expr::RecordInit(..) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::RecordInit,
        )),
        Expr::Field(..) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::Field,
        )),
        Expr::Tuple(_) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::Tuple,
        )),
        Expr::Array(_) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::Array,
        )),
        Expr::ArrayRep(..) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::ArrayRep,
        )),
        Expr::If(..) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::If,
        )),
        Expr::Match(..) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::Match,
        )),
        Expr::Assign(..) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::Assign,
        )),
        Expr::AugAssign(..) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::AugAssign,
        )),
        Expr::With(..) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::With,
        )),
        Expr::Quote { .. } => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::Quote,
        )),
        Expr::QuoteHole(_) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::QuoteHole,
        )),
        Expr::QuoteFieldHole(..) => Err(TermLowerError::UnsupportedExpr(
            expr,
            UnsupportedExprKind::QuoteFieldHole,
        )),
    }
}

/// Lowers a binary expression, mapping the HIR operator onto the term
/// language and rejecting operators outside the fragment.
fn lower_bin<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expr: ExprId,
    lhs: ExprId,
    rhs: ExprId,
    op: BinOp,
    assumptions: PredicateListId<'db>,
) -> Result<TermId<'db>, TermLowerError<'db>> {
    let term_op = match op {
        BinOp::Arith(arith) => match arith {
            ArithBinOp::Add => LoweredBinOp::Arith(TermArithOp::Add),
            ArithBinOp::Sub => LoweredBinOp::Arith(TermArithOp::Sub),
            ArithBinOp::Mul => LoweredBinOp::Arith(TermArithOp::Mul),
            ArithBinOp::Div => LoweredBinOp::Arith(TermArithOp::Div),
            ArithBinOp::Rem => LoweredBinOp::Arith(TermArithOp::Rem),
            ArithBinOp::Pow
            | ArithBinOp::LShift
            | ArithBinOp::RShift
            | ArithBinOp::BitAnd
            | ArithBinOp::BitOr
            | ArithBinOp::BitXor
            | ArithBinOp::Range => {
                return Err(TermLowerError::UnsupportedArithOp(expr, arith));
            }
        },
        BinOp::Comp(comp) => match comp {
            CompBinOp::Eq => LoweredBinOp::Cmp(TermCmpOp::Eq),
            CompBinOp::NotEq => LoweredBinOp::Cmp(TermCmpOp::Ne),
            CompBinOp::Lt => LoweredBinOp::Cmp(TermCmpOp::Lt),
            CompBinOp::LtEq => LoweredBinOp::Cmp(TermCmpOp::Le),
            CompBinOp::Gt => LoweredBinOp::Cmp(TermCmpOp::Gt),
            CompBinOp::GtEq => LoweredBinOp::Cmp(TermCmpOp::Ge),
        },
        BinOp::Logical(logical) => match logical {
            LogicalBinOp::And => LoweredBinOp::And,
            LogicalBinOp::Or => LoweredBinOp::Or,
        },
        BinOp::Index => return Err(TermLowerError::IndexOperator(expr)),
    };

    let lhs = lower_hir_to_term(db, body, lhs, assumptions)?;
    let rhs = lower_hir_to_term(db, body, rhs, assumptions)?;
    let node = match term_op {
        LoweredBinOp::Arith(op) => TermNode::Arith { op, lhs, rhs },
        LoweredBinOp::Cmp(op) => TermNode::Cmp { op, lhs, rhs },
        LoweredBinOp::And => TermNode::And(lhs, rhs),
        LoweredBinOp::Or => TermNode::Or(lhs, rhs),
    };
    Ok(TermId::new(db, node))
}

/// Internal classification of a supported binary operator.
enum LoweredBinOp {
    Arith(TermArithOp),
    Cmp(TermCmpOp),
    And,
    Or,
}

/// Lowers a value-position path to a const-shaped term: a const generic
/// parameter, a `const` item reference, or a trait-associated const.
fn lower_value_path<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expr: ExprId,
    path: PathId<'db>,
    assumptions: PredicateListId<'db>,
) -> Result<TermId<'db>, TermLowerError<'db>> {
    let Ok(res) = resolve_path(db, path, body.scope(), assumptions, true) else {
        return Err(TermLowerError::UnresolvedPath(expr, path));
    };

    match res {
        PathRes::Const(const_, _ty) => Ok(TermId::new(db, TermNode::ConstRef(const_))),
        PathRes::TraitConst(_self_ty, inst, name) => {
            Ok(TermId::new(db, TermNode::AssocConst { inst, name }))
        }
        PathRes::Ty(ty) => term_of_resolved_const_ty(db, expr, ty),
        ref other @ (PathRes::TyAlias(..)
        | PathRes::Func(_)
        | PathRes::FuncParam(..)
        | PathRes::Trait(_)
        | PathRes::TraitMethod(..)
        | PathRes::EnumVariant(_)
        | PathRes::InherentConst(..)
        | PathRes::Mod(_)
        | PathRes::Method(..)) => Err(TermLowerError::NonTermPath(expr, other.kind_name())),
    }
}

/// Classifies a path that resolved to a type: const generic parameters and
/// already-evaluated scalar const types become terms; everything else is a
/// precise error.
fn term_of_resolved_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    expr: ExprId,
    ty: TyId<'db>,
) -> Result<TermId<'db>, TermLowerError<'db>> {
    match ty.data(db) {
        TyData::ConstTy(const_ty) => match const_ty.data(db) {
            ConstTyData::TyParam(..) => Ok(TermId::new(db, TermNode::ConstParam(ty))),
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(value), _) => {
                Ok(TermId::new(db, TermNode::Int(*value)))
            }
            ConstTyData::Evaluated(EvaluatedConstTy::LitBool(value), _) => {
                Ok(TermId::new(db, TermNode::Bool(*value)))
            }
            ConstTyData::Evaluated(
                EvaluatedConstTy::Unit
                | EvaluatedConstTy::Tuple(_)
                | EvaluatedConstTy::Array(_)
                | EvaluatedConstTy::Bytes(_)
                | EvaluatedConstTy::Record(_)
                | EvaluatedConstTy::EnumVariant(_)
                | EvaluatedConstTy::Invalid,
                _,
            ) => Err(TermLowerError::OpaqueConstTy(expr)),
            ConstTyData::TyVar(..)
            | ConstTyData::Hole(..)
            | ConstTyData::Abstract(..)
            | ConstTyData::UnEvaluated { .. } => Err(TermLowerError::OpaqueConstTy(expr)),
        },
        TyData::TyVar(_)
        | TyData::TyParam(_)
        | TyData::AssocTy(_)
        | TyData::QualifiedTy(_)
        | TyData::ConstraintTerm(_)
        | TyData::TraitCtor(_)
        | TyData::TyApp(..)
        | TyData::TyBase(_)
        | TyData::Never
        | TyData::Invalid(_) => Err(TermLowerError::NonTermPath(expr, "type")),
    }
}

/// Resolves a call's callee to a const function symbol plus its explicitly
/// written generic arguments.
fn lower_callee<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    call_expr: ExprId,
    callee: ExprId,
    assumptions: PredicateListId<'db>,
) -> Result<(Func<'db>, Vec<TyId<'db>>), TermLowerError<'db>> {
    let Partial::Present(callee_data) = callee.data(db, body) else {
        return Err(TermLowerError::AbsentExpr(callee));
    };
    let Expr::Path(path) = callee_data else {
        return Err(TermLowerError::CalleeNotPath(callee));
    };
    let Partial::Present(path) = path else {
        return Err(TermLowerError::AbsentExpr(callee));
    };

    let Ok(res) = resolve_path(db, *path, body.scope(), assumptions, true) else {
        return Err(TermLowerError::UnresolvedPath(callee, *path));
    };

    let func_ty = match res {
        PathRes::Func(ty) => ty,
        ref other @ (PathRes::Ty(_)
        | PathRes::TyAlias(..)
        | PathRes::FuncParam(..)
        | PathRes::Trait(_)
        | PathRes::TraitMethod(..)
        | PathRes::EnumVariant(_)
        | PathRes::Const(..)
        | PathRes::TraitConst(..)
        | PathRes::InherentConst(..)
        | PathRes::Mod(_)
        | PathRes::Method(..)) => {
            return Err(TermLowerError::CalleeNotFunc(call_expr, other.kind_name()));
        }
    };

    let func = callee_from_func_ty(db, call_expr, func_ty)?;
    if !func.is_const(db) {
        return Err(TermLowerError::CalleeNotConst(call_expr, func));
    }

    // `PathRes::Func` deliberately does not apply path generic args (the
    // type checker aligns them with implicit params); capture the
    // explicitly written ones here so `check<u8>()` and `check<u16>()`
    // have distinct identities.
    let minter = HoleMinter::new(HoleAnchor::TemplatePath {
        path: *path,
        scope: body.scope(),
    });
    let generic_args = lower_generic_arg_list(
        db,
        path.generic_args(db),
        body.scope(),
        assumptions,
        LayoutHoleArgSite::Path(*path),
        &minter,
    );
    Ok((func, generic_args))
}

/// Extracts the HIR function item from a `PathRes::Func` type.
fn callee_from_func_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    call_expr: ExprId,
    func_ty: TyId<'db>,
) -> Result<Func<'db>, TermLowerError<'db>> {
    let (base, _args) = func_ty.decompose_ty_app(db);
    let callable = match base.data(db) {
        TyData::TyBase(TyBase::Func(callable)) => *callable,
        TyData::TyBase(TyBase::Prim(_) | TyBase::Adt(_) | TyBase::Contract(_))
        | TyData::TyVar(_)
        | TyData::TyParam(_)
        | TyData::AssocTy(_)
        | TyData::QualifiedTy(_)
        | TyData::ConstraintTerm(_)
        | TyData::TraitCtor(_)
        | TyData::TyApp(..)
        | TyData::ConstTy(_)
        | TyData::Never
        | TyData::Invalid(_) => {
            // Defensive: `PathRes::Func` always carries a function base
            // type today.
            return Err(TermLowerError::CalleeNotFunc(
                call_expr,
                "non-function type",
            ));
        }
    };
    match callable {
        CallableDef::Func(func) => Ok(func),
        CallableDef::VariantCtor(_) => Err(TermLowerError::CalleeNotFunc(
            call_expr,
            "enum variant constructor",
        )),
    }
}

/// Rewrites a term's parameters by `args`, mapping each generic parameter
/// referenced in the term (by index) to the corresponding argument — the
/// binder-folding the identity layer otherwise leaves to "the future prover".
///
/// This lets a callee's predicate term (over the callee's parameters) be
/// expressed over the *call's* arguments, so it can be compared by identity
/// against the caller's in-scope assumption terms. Embedded `TyId` and
/// `TraitInstId` are substituted through the existing instantiation folder; the
/// term structure is rebuilt and re-interned. Pure: no CTFE, no normalization
/// (callers normalize afterwards).
pub fn substitute_term<'db>(
    db: &'db dyn HirAnalysisDb,
    term: TermId<'db>,
    args: &[TyId<'db>],
) -> TermId<'db> {
    let node = match term.data(db) {
        TermNode::Int(_) | TermNode::Bool(_) | TermNode::ConstRef(_) => return term,
        TermNode::ConstParam(ty) => TermNode::ConstParam(Binder::bind(*ty).instantiate(db, args)),
        TermNode::AssocConst { inst, name } => TermNode::AssocConst {
            inst: Binder::bind(*inst).instantiate(db, args),
            name: *name,
        },
        TermNode::Arith { op, lhs, rhs } => TermNode::Arith {
            op: *op,
            lhs: substitute_term(db, *lhs, args),
            rhs: substitute_term(db, *rhs, args),
        },
        TermNode::Cmp { op, lhs, rhs } => TermNode::Cmp {
            op: *op,
            lhs: substitute_term(db, *lhs, args),
            rhs: substitute_term(db, *rhs, args),
        },
        TermNode::And(lhs, rhs) => TermNode::And(
            substitute_term(db, *lhs, args),
            substitute_term(db, *rhs, args),
        ),
        TermNode::Or(lhs, rhs) => TermNode::Or(
            substitute_term(db, *lhs, args),
            substitute_term(db, *rhs, args),
        ),
        TermNode::Not(inner) => TermNode::Not(substitute_term(db, *inner, args)),
        TermNode::App {
            callee,
            generic_args,
            args: call_args,
        } => TermNode::App {
            callee: *callee,
            generic_args: generic_args
                .iter()
                .map(|ty| Binder::bind(*ty).instantiate(db, args))
                .collect(),
            args: call_args
                .iter()
                .map(|arg| substitute_term(db, *arg, args))
                .collect(),
        },
    };
    TermId::new(db, node)
}

/// Rewrites a term's embedded parameter types and trait instances through
/// `subst`, an arbitrary substitution over generic-parameter types.
///
/// Where [`substitute_term`] rebases by *positional* generic arguments (a
/// call's `generic_args`), this rebases by a caller-supplied mapping over
/// parameter `TyId`s. Its purpose is method const-predicate conformance: a
/// trait method's predicate term lives over the *trait* method's parameter
/// frame, so to compare it by identity against the *impl* method's predicate
/// terms it must first be re-expressed in the impl method's frame. The caller
/// supplies the trait→impl parameter substitution (the same one used for the
/// trait-bound comparison); `subst` returns its argument unchanged for any
/// parameter it does not remap.
///
/// Embedded `TyId` (a [`TermNode::ConstParam`]) and `TraitInstId` (a
/// [`TermNode::AssocConst`] receiver, and `App` generic args) are rewritten
/// through the existing instantiation folder; the term structure is rebuilt and
/// re-interned. Pure: no CTFE, no normalization (callers normalize afterwards).
pub fn rebase_term_with<'db, F>(
    db: &'db dyn HirAnalysisDb,
    term: TermId<'db>,
    subst: &F,
) -> TermId<'db>
where
    F: Fn(TyId<'db>) -> TyId<'db>,
{
    let node = match term.data(db) {
        TermNode::Int(_) | TermNode::Bool(_) | TermNode::ConstRef(_) => return term,
        TermNode::ConstParam(ty) => {
            TermNode::ConstParam(Binder::bind(*ty).instantiate_with(db, subst))
        }
        TermNode::AssocConst { inst, name } => TermNode::AssocConst {
            inst: Binder::bind(*inst).instantiate_with(db, subst),
            name: *name,
        },
        TermNode::Arith { op, lhs, rhs } => TermNode::Arith {
            op: *op,
            lhs: rebase_term_with(db, *lhs, subst),
            rhs: rebase_term_with(db, *rhs, subst),
        },
        TermNode::Cmp { op, lhs, rhs } => TermNode::Cmp {
            op: *op,
            lhs: rebase_term_with(db, *lhs, subst),
            rhs: rebase_term_with(db, *rhs, subst),
        },
        TermNode::And(lhs, rhs) => TermNode::And(
            rebase_term_with(db, *lhs, subst),
            rebase_term_with(db, *rhs, subst),
        ),
        TermNode::Or(lhs, rhs) => TermNode::Or(
            rebase_term_with(db, *lhs, subst),
            rebase_term_with(db, *rhs, subst),
        ),
        TermNode::Not(inner) => TermNode::Not(rebase_term_with(db, *inner, subst)),
        TermNode::App {
            callee,
            generic_args,
            args,
        } => TermNode::App {
            callee: *callee,
            generic_args: generic_args
                .iter()
                .map(|ty| Binder::bind(*ty).instantiate_with(db, subst))
                .collect(),
            args: args
                .iter()
                .map(|arg| rebase_term_with(db, *arg, subst))
                .collect(),
        },
    };
    TermId::new(db, node)
}

/// Normalizes a term bottom-up. Idempotent: `normalize_term(normalize_term(t))
/// == normalize_term(t)`.
///
/// The rules are exactly [`NORMALIZATION_RULES`]: exact natural-number and
/// boolean constant folding (total — terms whose evaluation CTFE would
/// reject, such as `1 / 0` or `3 - 5`, are left intact), short-circuit-safe
/// boolean simplification, double-negation elimination, and interned-id
/// ordering of `Add`/`Mul`/`Eq`/`Ne` operands.
///
/// # Determinism, not canonicalization
///
/// Commutative operand ordering uses the salsa intern id as the key. This
/// makes normalization deterministic *within a database* (the same pair of
/// operands always lands in the same order, so `N + 1` and `1 + N` intern
/// to one term), but the order itself is arbitrary — it reflects interning
/// history, not any semantic property, and is not stable across databases.
/// Nothing may attach meaning to which operand comes first.
///
/// # What is deliberately NOT normalized
///
/// * **Distinct comparison relations are never merged.** `x >= 50` and
///   `50 <= x` are logically equivalent but remain distinct terms; so do
///   `!(a == b)` and `a != b`. The disclosed predicate-matching contract is
///   verbatim-after-normalization, and collapsing relations would silently
///   widen it. Equivalences across relations are a future *prover* upgrade
///   (M5 port map §6), not an identity property.
/// * **`&&`/`||` operands keep their order** (short-circuiting makes order
///   observable through CTFE errors).
/// * **No CTFE.** Const refs, associated consts, and applications stay
///   symbolic; only literals already present in the term are folded.
#[salsa::tracked]
pub fn normalize_term<'db>(db: &'db dyn HirAnalysisDb, term: TermId<'db>) -> TermId<'db> {
    match term.data(db).clone() {
        TermNode::Int(_)
        | TermNode::Bool(_)
        | TermNode::ConstParam(_)
        | TermNode::ConstRef(_)
        | TermNode::AssocConst { .. } => term,
        TermNode::Arith { op, lhs, rhs } => {
            let lhs = normalize_term(db, lhs);
            let rhs = normalize_term(db, rhs);
            normalize_arith(db, op, lhs, rhs)
        }
        TermNode::Cmp { op, lhs, rhs } => {
            let lhs = normalize_term(db, lhs);
            let rhs = normalize_term(db, rhs);
            normalize_cmp(db, op, lhs, rhs)
        }
        TermNode::And(lhs, rhs) => {
            let lhs = normalize_term(db, lhs);
            let rhs = normalize_term(db, rhs);
            normalize_and(db, lhs, rhs)
        }
        TermNode::Or(lhs, rhs) => {
            let lhs = normalize_term(db, lhs);
            let rhs = normalize_term(db, rhs);
            normalize_or(db, lhs, rhs)
        }
        TermNode::Not(inner) => {
            let inner = normalize_term(db, inner);
            normalize_not(db, inner)
        }
        TermNode::App {
            callee,
            generic_args,
            args,
        } => {
            let args = args
                .into_iter()
                .map(|arg| normalize_term(db, arg))
                .collect();
            TermId::new(
                db,
                TermNode::App {
                    callee,
                    generic_args,
                    args,
                },
            )
        }
    }
}

/// Rebuilds an arithmetic node over normalized operands: folds two integer
/// literals where the result stays a natural number and the operation is
/// CTFE-total, otherwise orders commutative operands.
fn normalize_arith<'db>(
    db: &'db dyn HirAnalysisDb,
    op: TermArithOp,
    lhs: TermId<'db>,
    rhs: TermId<'db>,
) -> TermId<'db> {
    if let (TermNode::Int(lhs_lit), TermNode::Int(rhs_lit)) = (lhs.data(db), rhs.data(db)) {
        let a = lhs_lit.data(db);
        let b = rhs_lit.data(db);
        let folded = match op {
            TermArithOp::Add => Some(a + b),
            // No negative naturals: underflowing subtraction is left
            // intact for CTFE to reject.
            TermArithOp::Sub => (a >= b).then(|| a - b),
            TermArithOp::Mul => Some(a * b),
            // Division by zero is a CTFE error, not `false`/`0`
            // (effort2's `1 / 0 == 0` fixture relies on this).
            TermArithOp::Div => (!b.is_zero()).then(|| a / b),
            TermArithOp::Rem => (!b.is_zero()).then(|| a % b),
        };
        if let Some(value) = folded {
            return TermId::int(db, value);
        }
    }

    let (lhs, rhs) = match op {
        TermArithOp::Add | TermArithOp::Mul => order_commutative(lhs, rhs),
        TermArithOp::Sub | TermArithOp::Div | TermArithOp::Rem => (lhs, rhs),
    };
    TermId::new(db, TermNode::Arith { op, lhs, rhs })
}

/// The shared natural-number comparison primitive: the SINGLE place that
/// decides a [`TermCmpOp`] over two concrete naturals.
///
/// Const-predicate folding ([`normalize_cmp`]) and derive-provider steering
/// (the provider executor's integer-comparison path) both route their integer
/// comparisons through this one function. That is the SGK-B "no second
/// evaluator" invariant for comparison: provider steering grows no local
/// integer comparator; a pure numeric decision in a provider body is decided by
/// the same code that decides a `where N > 0` const predicate.
pub(crate) fn compare_nats(op: TermCmpOp, a: &BigUint, b: &BigUint) -> bool {
    match op {
        TermCmpOp::Eq => a == b,
        TermCmpOp::Ne => a != b,
        TermCmpOp::Lt => a < b,
        TermCmpOp::Le => a <= b,
        TermCmpOp::Gt => a > b,
        TermCmpOp::Ge => a >= b,
    }
}

/// Rebuilds a comparison node over normalized operands: folds literal
/// comparisons, orders the operands of the symmetric relations (`Eq`/`Ne`),
/// and preserves every relation as written.
fn normalize_cmp<'db>(
    db: &'db dyn HirAnalysisDb,
    op: TermCmpOp,
    lhs: TermId<'db>,
    rhs: TermId<'db>,
) -> TermId<'db> {
    if let (TermNode::Int(lhs_lit), TermNode::Int(rhs_lit)) = (lhs.data(db), rhs.data(db)) {
        let result = compare_nats(op, lhs_lit.data(db), rhs_lit.data(db));
        return TermId::bool(db, result);
    }

    if let (TermNode::Bool(a), TermNode::Bool(b)) = (lhs.data(db), rhs.data(db)) {
        match op {
            TermCmpOp::Eq => return TermId::bool(db, a == b),
            TermCmpOp::Ne => return TermId::bool(db, a != b),
            // Booleans are unordered in the term language; leave ordered
            // comparisons over bool literals intact (CTFE rejects them).
            TermCmpOp::Lt | TermCmpOp::Le | TermCmpOp::Gt | TermCmpOp::Ge => {}
        }
    }

    let (lhs, rhs) = match op {
        TermCmpOp::Eq | TermCmpOp::Ne => order_commutative(lhs, rhs),
        TermCmpOp::Lt | TermCmpOp::Le | TermCmpOp::Gt | TermCmpOp::Ge => (lhs, rhs),
    };
    TermId::new(db, TermNode::Cmp { op, lhs, rhs })
}

/// Rebuilds a conjunction over normalized operands with short-circuit-safe
/// literal folding only (`And(x, false)` is NOT folded: `x` may CTFE-fail).
fn normalize_and<'db>(
    db: &'db dyn HirAnalysisDb,
    lhs: TermId<'db>,
    rhs: TermId<'db>,
) -> TermId<'db> {
    if let TermNode::Bool(lhs_lit) = *lhs.data(db) {
        return if lhs_lit {
            rhs
        } else {
            TermId::bool(db, false)
        };
    }
    if let TermNode::Bool(true) = rhs.data(db) {
        return lhs;
    }
    TermId::new(db, TermNode::And(lhs, rhs))
}

/// Rebuilds a disjunction over normalized operands with short-circuit-safe
/// literal folding only (`Or(x, true)` is NOT folded: `x` may CTFE-fail).
fn normalize_or<'db>(
    db: &'db dyn HirAnalysisDb,
    lhs: TermId<'db>,
    rhs: TermId<'db>,
) -> TermId<'db> {
    if let TermNode::Bool(lhs_lit) = *lhs.data(db) {
        return if lhs_lit { TermId::bool(db, true) } else { rhs };
    }
    if let TermNode::Bool(false) = rhs.data(db) {
        return lhs;
    }
    TermId::new(db, TermNode::Or(lhs, rhs))
}

/// Rebuilds a negation over a normalized operand: literal folding and
/// double-negation elimination. `Not` is never pushed through comparisons
/// (that would be a cross-relation rewrite).
fn normalize_not<'db>(db: &'db dyn HirAnalysisDb, inner: TermId<'db>) -> TermId<'db> {
    match inner.data(db) {
        TermNode::Bool(value) => TermId::bool(db, !value),
        TermNode::Not(target) => *target,
        TermNode::Int(_)
        | TermNode::ConstParam(_)
        | TermNode::ConstRef(_)
        | TermNode::AssocConst { .. }
        | TermNode::Arith { .. }
        | TermNode::Cmp { .. }
        | TermNode::And(..)
        | TermNode::Or(..)
        | TermNode::App { .. } => TermId::new(db, TermNode::Not(inner)),
    }
}

/// Orders a commutative operand pair by interned id.
///
/// This is a determinism device only: the salsa intern id reflects
/// interning history within the current database and carries no semantic
/// meaning. See the "Determinism, not canonicalization" note on
/// [`normalize_term`].
fn order_commutative<'db>(lhs: TermId<'db>, rhs: TermId<'db>) -> (TermId<'db>, TermId<'db>) {
    if term_intern_key(lhs) <= term_intern_key(rhs) {
        (lhs, rhs)
    } else {
        (rhs, lhs)
    }
}

/// The raw salsa intern key of a term (database-local, history-dependent).
fn term_intern_key(term: TermId<'_>) -> u32 {
    term.as_id().as_u32()
}

/// Renders a term as compact, source-like text for receipts, hover, and
/// diagnostics (e.g. `(B::WORD_BITS == 256)`).
///
/// Not a parser-faithful printer: every compound node is fully parenthesized so
/// the structure is unambiguous without re-deriving precedence, and operand
/// order reflects the *normalized* term (interning order), not the source. It
/// is a display of identity, which is exactly what an evidence consumer wants.
pub fn pretty_print_term<'db>(db: &'db dyn HirAnalysisDb, term: TermId<'db>) -> String {
    match term.data(db) {
        TermNode::Int(value) => value.data(db).to_string(),
        TermNode::Bool(value) => value.to_string(),
        TermNode::ConstParam(ty) => ty.pretty_print(db).to_string(),
        TermNode::ConstRef(const_) => const_
            .name(db)
            .to_opt()
            .map(|name| name.data(db).to_string())
            .unwrap_or_else(|| "{const}".to_string()),
        TermNode::AssocConst { inst, name } => {
            format!("{}::{}", inst.self_ty(db).pretty_print(db), name.data(db))
        }
        TermNode::Arith { op, lhs, rhs } => format!(
            "({} {} {})",
            pretty_print_term(db, *lhs),
            arith_op_symbol(*op),
            pretty_print_term(db, *rhs),
        ),
        TermNode::Cmp { op, lhs, rhs } => format!(
            "({} {} {})",
            pretty_print_term(db, *lhs),
            cmp_op_symbol(*op),
            pretty_print_term(db, *rhs),
        ),
        TermNode::And(lhs, rhs) => format!(
            "({} && {})",
            pretty_print_term(db, *lhs),
            pretty_print_term(db, *rhs),
        ),
        TermNode::Or(lhs, rhs) => format!(
            "({} || {})",
            pretty_print_term(db, *lhs),
            pretty_print_term(db, *rhs),
        ),
        TermNode::Not(inner) => format!("!{}", pretty_print_term(db, *inner)),
        TermNode::App {
            callee,
            generic_args,
            args,
        } => {
            let generics = if generic_args.is_empty() {
                String::new()
            } else {
                format!(
                    "::<{}>",
                    generic_args
                        .iter()
                        .map(|ty| ty.pretty_print(db).to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            };
            let call_args = args
                .iter()
                .map(|arg| pretty_print_term(db, *arg))
                .collect::<Vec<_>>()
                .join(", ");
            let callee = callee
                .name(db)
                .to_opt()
                .map(|name| name.data(db).to_string())
                .unwrap_or_else(|| "{fn}".to_string());
            format!("{callee}{generics}({call_args})")
        }
    }
}

/// The source spelling of a term arithmetic operator.
fn arith_op_symbol(op: TermArithOp) -> &'static str {
    match op {
        TermArithOp::Add => "+",
        TermArithOp::Sub => "-",
        TermArithOp::Mul => "*",
        TermArithOp::Div => "/",
        TermArithOp::Rem => "%",
    }
}

/// The source spelling of a term comparison operator.
fn cmp_op_symbol(op: TermCmpOp) -> &'static str {
    match op {
        TermCmpOp::Eq => "==",
        TermCmpOp::Ne => "!=",
        TermCmpOp::Lt => "<",
        TermCmpOp::Le => "<=",
        TermCmpOp::Gt => ">",
        TermCmpOp::Ge => ">=",
    }
}

/// FNV-1a fingerprint of the term-language shape: [`TERM_LANG_SHAPE`],
/// [`TERM_ARITH_OP_SHAPE`], [`TERM_CMP_OP_SHAPE`], and
/// [`NORMALIZATION_RULES`].
///
/// Recorded in the version ledger (see [`TERM_LANG_VERSION`]); exposed so a
/// CI gate can cross-check the ledger without running the test suite.
pub fn term_lang_fingerprint() -> u64 {
    const FNV_OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
    const FNV_PRIME: u64 = 0x0000_0100_0000_01b3;

    let mut hash = FNV_OFFSET;
    let sections: [&[&str]; 4] = [
        TERM_LANG_SHAPE,
        TERM_ARITH_OP_SHAPE,
        TERM_CMP_OP_SHAPE,
        NORMALIZATION_RULES,
    ];
    for section in sections {
        for entry in section {
            for byte in entry.bytes() {
                hash ^= u64::from(byte);
                hash = hash.wrapping_mul(FNV_PRIME);
            }
            // Separator so entry boundaries are part of the fingerprint.
            hash ^= u64::from(b'\n');
            hash = hash.wrapping_mul(FNV_PRIME);
        }
        hash ^= u64::from(b';');
        hash = hash.wrapping_mul(FNV_PRIME);
    }
    hash
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;
    use num_bigint::BigUint;

    use super::{
        NORMALIZATION_RULES, TERM_ARITH_OP_SHAPE, TERM_CMP_OP_SHAPE, TERM_LANG_SHAPE,
        TERM_LANG_VERSION, TermArithOp, TermCmpOp, TermId, TermLowerError, TermNode,
        UnsupportedExprKind, callee_from_func_ty, compare_nats, lower_hir_to_term, normalize_cmp,
        normalize_term, term_lang_fingerprint, term_of_resolved_const_ty,
    };
    use crate::analysis::ty::const_ty::{ConstTyData, ConstTyId};
    use crate::analysis::ty::trait_resolution::{PredicateListId, constraint::collect_constraints};
    use crate::analysis::ty::ty_def::TyId;
    use crate::hir_def::{
        ArithBinOp, BinOp, Body, CallableDef, CompBinOp, Const, EnumVariant, Expr, ExprId, Func,
        GenericParamOwner, ItemKind, Partial, TopLevelMod, UnOp,
    };
    use crate::test_db::HirAnalysisTestDb;

    /// The version ledger: append-only history of
    /// `(TERM_LANG_VERSION, term_lang_fingerprint())` pairs.
    ///
    /// To change the term language: append a NEW entry with the bumped
    /// version and the new fingerprint (the failing test prints it), and
    /// bump `TERM_LANG_VERSION` to match. Never edit an existing entry in
    /// place — review rejects ledger rewrites.
    const TERM_LANG_LEDGER: &[(u32, u64)] = &[(1, 0x92de_9054_97e3_98d6)];

    fn setup(name: &str, src: &str) -> (HirAnalysisTestDb, common::file::File) {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from(format!("{name}.fe")), src);
        (db, file)
    }

    fn named_const<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: TopLevelMod<'db>,
        name: &str,
    ) -> Const<'db> {
        top_mod
            .children_non_nested(db)
            .find_map(|item| {
                if let ItemKind::Const(const_) = item
                    && const_
                        .name(db)
                        .to_opt()
                        .is_some_and(|ident| ident.data(db) == name)
                {
                    Some(const_)
                } else {
                    None
                }
            })
            .unwrap_or_else(|| panic!("missing `{name}` const"))
    }

    fn named_func<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: TopLevelMod<'db>,
        name: &str,
    ) -> Func<'db> {
        top_mod
            .children_non_nested(db)
            .find_map(|item| {
                if let ItemKind::Func(func) = item
                    && func
                        .name(db)
                        .to_opt()
                        .is_some_and(|ident| ident.data(db) == name)
                {
                    Some(func)
                } else {
                    None
                }
            })
            .unwrap_or_else(|| panic!("missing `{name}` function"))
    }

    fn const_body<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: TopLevelMod<'db>,
        name: &str,
    ) -> Body<'db> {
        named_const(db, top_mod, name)
            .body(db)
            .to_opt()
            .unwrap_or_else(|| panic!("`{name}` const has no body"))
    }

    fn func_body<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: TopLevelMod<'db>,
        name: &str,
    ) -> Body<'db> {
        named_func(db, top_mod, name)
            .body(db)
            .unwrap_or_else(|| panic!("`{name}` function has no body"))
    }

    /// Finds the first (lexically lowered) expression matching `pred`.
    fn find_expr<'db>(
        db: &'db HirAnalysisTestDb,
        body: Body<'db>,
        pred: impl Fn(&Expr<'db>) -> bool,
    ) -> ExprId {
        body.exprs(db)
            .iter()
            .find_map(|(id, expr)| {
                if let Partial::Present(expr) = expr
                    && pred(expr)
                {
                    Some(id)
                } else {
                    None
                }
            })
            .expect("no expression matched the predicate")
    }

    fn no_assumptions(db: &HirAnalysisTestDb) -> PredicateListId<'_> {
        PredicateListId::empty_list(db)
    }

    fn func_assumptions<'db>(db: &'db HirAnalysisTestDb, func: Func<'db>) -> PredicateListId<'db> {
        collect_constraints(db, GenericParamOwner::Func(func)).instantiate_identity()
    }

    fn lower_root<'db>(
        db: &'db HirAnalysisTestDb,
        body: Body<'db>,
    ) -> Result<TermId<'db>, TermLowerError<'db>> {
        lower_hir_to_term(db, body, body.expr(db), no_assumptions(db))
    }

    // =====================================================================
    // Suite 1: per-variant lowering-error tests (every `TermLowerError`
    // variant and every `UnsupportedExprKind` exercised), plus the
    // success-path shape tests the other suites build on.
    // =====================================================================

    #[test]
    fn absent_operand_reports_absent_expr() {
        let (db, file) = setup("absent_operand", "const P: bool = 1 +");
        let (top_mod, _) = db.top_mod(file);
        let body = const_body(&db, top_mod, "P");
        assert!(matches!(
            lower_root(&db, body),
            Err(TermLowerError::AbsentExpr(_))
        ));
    }

    #[test]
    fn block_expr_unsupported() {
        let (db, file) = setup("block_expr", "fn host() -> bool {\n    true\n}");
        let (top_mod, _) = db.top_mod(file);
        let body = func_body(&db, top_mod, "host");
        // A function body's root expression is always a block.
        assert!(matches!(
            lower_root(&db, body),
            Err(TermLowerError::UnsupportedExpr(
                _,
                UnsupportedExprKind::Block
            ))
        ));
    }

    /// Asserts that the first expression of `kind_pred` in `host`'s body
    /// lowers to `UnsupportedExpr` with the given kind.
    fn assert_unsupported_in_host(
        src: &str,
        kind_pred: impl Fn(&Expr<'_>) -> bool,
        expected: UnsupportedExprKind,
    ) {
        let (db, file) = setup(&format!("unsupported_{expected:?}"), src);
        let (top_mod, _) = db.top_mod(file);
        let body = func_body(&db, top_mod, "host");
        let expr = find_expr(&db, body, kind_pred);
        let result = lower_hir_to_term(&db, body, expr, no_assumptions(&db));
        match result {
            Err(TermLowerError::UnsupportedExpr(_, kind)) => assert_eq!(kind, expected),
            Err(other) => panic!("expected UnsupportedExpr({expected:?}), got {other:?}"),
            Ok(term) => panic!("expected UnsupportedExpr({expected:?}), got term {term:?}"),
        }
    }

    #[test]
    fn if_expr_unsupported() {
        assert_unsupported_in_host(
            "fn host() {\n    let x = if true { 1 } else { 2 }\n}",
            |e| matches!(e, Expr::If(..)),
            UnsupportedExprKind::If,
        );
    }

    #[test]
    fn match_expr_unsupported() {
        assert_unsupported_in_host(
            "fn host() {\n    let y = match 1 {\n        1 => 2\n        other => 3\n    }\n}",
            |e| matches!(e, Expr::Match(..)),
            UnsupportedExprKind::Match,
        );
    }

    #[test]
    fn tuple_expr_unsupported() {
        assert_unsupported_in_host(
            "fn host() {\n    let x = (1, 2)\n}",
            |e| matches!(e, Expr::Tuple(_)),
            UnsupportedExprKind::Tuple,
        );
    }

    #[test]
    fn array_expr_unsupported() {
        assert_unsupported_in_host(
            "fn host() {\n    let x = [1, 2]\n}",
            |e| matches!(e, Expr::Array(_)),
            UnsupportedExprKind::Array,
        );
    }

    #[test]
    fn array_rep_expr_unsupported() {
        assert_unsupported_in_host(
            "fn host() {\n    let x = [0; 2]\n}",
            |e| matches!(e, Expr::ArrayRep(..)),
            UnsupportedExprKind::ArrayRep,
        );
    }

    #[test]
    fn cast_expr_unsupported() {
        assert_unsupported_in_host(
            "fn host() {\n    let x = 1 as u8\n}",
            |e| matches!(e, Expr::Cast(..)),
            UnsupportedExprKind::Cast,
        );
    }

    #[test]
    fn field_expr_unsupported() {
        assert_unsupported_in_host(
            "struct S {\n    pub f: u256,\n}\n\nfn host(s: S) {\n    let x = s.f\n}",
            |e| matches!(e, Expr::Field(..)),
            UnsupportedExprKind::Field,
        );
    }

    #[test]
    fn record_init_expr_unsupported() {
        assert_unsupported_in_host(
            "struct S {}\n\nfn host() {\n    let s = S {}\n}",
            |e| matches!(e, Expr::RecordInit(..)),
            UnsupportedExprKind::RecordInit,
        );
    }

    #[test]
    fn method_call_expr_unsupported() {
        assert_unsupported_in_host(
            "fn host(x: u256) {\n    let y = x.foo()\n}",
            |e| matches!(e, Expr::MethodCall(..)),
            UnsupportedExprKind::MethodCall,
        );
    }

    #[test]
    fn assign_expr_unsupported() {
        assert_unsupported_in_host(
            "fn host() {\n    let mut x = 1\n    x = 2\n}",
            |e| matches!(e, Expr::Assign(..)),
            UnsupportedExprKind::Assign,
        );
    }

    #[test]
    fn aug_assign_expr_unsupported() {
        assert_unsupported_in_host(
            "fn host() {\n    let mut x = 1\n    x += 2\n}",
            |e| matches!(e, Expr::AugAssign(..)),
            UnsupportedExprKind::AugAssign,
        );
    }

    #[test]
    fn with_expr_unsupported() {
        assert_unsupported_in_host(
            "fn host() {\n    with (L = 1) {\n        true\n    }\n}",
            |e| matches!(e, Expr::With(..)),
            UnsupportedExprKind::With,
        );
    }

    #[test]
    fn assert_expr_unsupported() {
        assert_unsupported_in_host(
            "fn host() {\n    assert!(true)\n}",
            |e| matches!(e, Expr::Assert(_)),
            UnsupportedExprKind::Assert,
        );
    }

    #[test]
    fn quote_exprs_unsupported() {
        let src = "fn host() {\n    let q = quote { a.${b} && ${c} }\n}";
        assert_unsupported_in_host(
            src,
            |e| matches!(e, Expr::Quote { .. }),
            UnsupportedExprKind::Quote,
        );
        assert_unsupported_in_host(
            src,
            |e| matches!(e, Expr::QuoteHole(_)),
            UnsupportedExprKind::QuoteHole,
        );
        assert_unsupported_in_host(
            src,
            |e| matches!(e, Expr::QuoteFieldHole(..)),
            UnsupportedExprKind::QuoteFieldHole,
        );
    }

    #[test]
    fn unsupported_arith_ops_are_named() {
        let cases = [
            ("const P: u256 = 1 ** 2", ArithBinOp::Pow),
            ("const P: u256 = 1 << 2", ArithBinOp::LShift),
            ("const P: u256 = 1 >> 2", ArithBinOp::RShift),
            ("const P: u256 = 1 & 2", ArithBinOp::BitAnd),
            ("const P: u256 = 1 | 2", ArithBinOp::BitOr),
            ("const P: u256 = 1 ^ 2", ArithBinOp::BitXor),
        ];
        for (src, expected_op) in cases {
            let (db, file) = setup(&format!("unsupported_arith_{expected_op:?}"), src);
            let (top_mod, _) = db.top_mod(file);
            let body = const_body(&db, top_mod, "P");
            match lower_root(&db, body) {
                Err(TermLowerError::UnsupportedArithOp(_, op)) => assert_eq!(op, expected_op),
                other => panic!("expected UnsupportedArithOp({expected_op:?}), got {other:?}"),
            }
        }
    }

    #[test]
    fn index_operator_unsupported() {
        let (db, file) = setup(
            "index_operator",
            "fn host() {\n    let xs = [1, 2]\n    let y = xs[0]\n}",
        );
        let (top_mod, _) = db.top_mod(file);
        let body = func_body(&db, top_mod, "host");
        let expr = find_expr(&db, body, |e| matches!(e, Expr::Bin(_, _, BinOp::Index)));
        assert!(matches!(
            lower_hir_to_term(&db, body, expr, no_assumptions(&db)),
            Err(TermLowerError::IndexOperator(_))
        ));
    }

    #[test]
    fn unsupported_unary_ops_are_named() {
        let (db, file) = setup("unary_minus", "const P: u256 = -1");
        let (top_mod, _) = db.top_mod(file);
        let body = const_body(&db, top_mod, "P");
        match lower_root(&db, body) {
            Err(TermLowerError::UnsupportedUnOp(_, op)) => assert_eq!(op, UnOp::Minus),
            other => panic!("expected UnsupportedUnOp(Minus), got {other:?}"),
        }

        let (db, file) = setup("unary_bitnot", "const P: u256 = ~1");
        let (top_mod, _) = db.top_mod(file);
        let body = const_body(&db, top_mod, "P");
        match lower_root(&db, body) {
            Err(TermLowerError::UnsupportedUnOp(_, op)) => assert_eq!(op, UnOp::BitNot),
            other => panic!("expected UnsupportedUnOp(BitNot), got {other:?}"),
        }
    }

    #[test]
    fn string_literal_unsupported() {
        let (db, file) = setup("string_literal", r#"const P: bool = "a" == "a""#);
        let (top_mod, _) = db.top_mod(file);
        let body = const_body(&db, top_mod, "P");
        assert!(matches!(
            lower_root(&db, body),
            Err(TermLowerError::StringLiteral(_))
        ));
    }

    #[test]
    fn unresolved_path_reported() {
        let (db, file) = setup("unresolved_path", "const P: u256 = MISSING + 1");
        let (top_mod, _) = db.top_mod(file);
        let body = const_body(&db, top_mod, "P");
        assert!(matches!(
            lower_root(&db, body),
            Err(TermLowerError::UnresolvedPath(..))
        ));
    }

    #[test]
    fn non_term_paths_name_resolved_domain() {
        let (db, file) = setup("non_term_path_type", "struct S {}\n\nconst P: u256 = S + 1");
        let (top_mod, _) = db.top_mod(file);
        let body = const_body(&db, top_mod, "P");
        match lower_root(&db, body) {
            Err(TermLowerError::NonTermPath(_, kind)) => assert_eq!(kind, "type"),
            other => panic!("expected NonTermPath(type), got {other:?}"),
        }

        let (db, file) = setup(
            "non_term_path_trait",
            "trait Tr {}\n\nconst P: u256 = Tr + 1",
        );
        let (top_mod, _) = db.top_mod(file);
        let body = const_body(&db, top_mod, "P");
        match lower_root(&db, body) {
            Err(TermLowerError::NonTermPath(_, kind)) => assert_eq!(kind, "trait"),
            other => panic!("expected NonTermPath(trait), got {other:?}"),
        }
    }

    #[test]
    fn opaque_const_ty_classified() {
        // Unreachable from source predicates; exercise the classifier
        // directly with a synthetic unevaluated const type.
        let (db, file) = setup("opaque_const_ty", "const P: bool = true");
        let (top_mod, _) = db.top_mod(file);
        let body = const_body(&db, top_mod, "P");
        let expr = body.expr(&db);
        let opaque = TyId::const_ty(
            &db,
            ConstTyId::new(
                &db,
                ConstTyData::UnEvaluated {
                    body,
                    ty: None,
                    const_def: None,
                    generic_args: vec![],
                    preserve_unevaluated: false,
                },
            ),
        );
        assert!(matches!(
            term_of_resolved_const_ty(&db, expr, opaque),
            Err(TermLowerError::OpaqueConstTy(_))
        ));
    }

    #[test]
    fn callee_not_path_reported() {
        let (db, file) = setup("callee_not_path", "const P: u256 = (1 + 2)(3)");
        let (top_mod, _) = db.top_mod(file);
        let body = const_body(&db, top_mod, "P");
        assert!(matches!(
            lower_root(&db, body),
            Err(TermLowerError::CalleeNotPath(_))
        ));
    }

    #[test]
    fn callee_not_func_reported() {
        let (db, file) = setup(
            "callee_not_func",
            "struct S {}\n\nfn host() {\n    let x = S()\n}",
        );
        let (top_mod, _) = db.top_mod(file);
        let body = func_body(&db, top_mod, "host");
        let expr = find_expr(&db, body, |e| matches!(e, Expr::Call(..)));
        match lower_hir_to_term(&db, body, expr, no_assumptions(&db)) {
            Err(TermLowerError::CalleeNotFunc(_, kind)) => assert_eq!(kind, "type"),
            other => panic!("expected CalleeNotFunc(type), got {other:?}"),
        }
    }

    #[test]
    fn variant_ctor_callee_reported() {
        // `PathRes::Func` never carries a variant ctor today; exercise the
        // classifier directly.
        let (db, file) = setup(
            "variant_ctor_callee",
            "enum E {\n    V(u256),\n}\n\nconst P: bool = true",
        );
        let (top_mod, _) = db.top_mod(file);
        let enum_ = top_mod
            .children_non_nested(&db)
            .find_map(|item| {
                if let ItemKind::Enum(enum_) = item {
                    Some(enum_)
                } else {
                    None
                }
            })
            .expect("missing enum");
        let body = const_body(&db, top_mod, "P");
        let expr = body.expr(&db);
        let ctor_ty = TyId::func(&db, CallableDef::VariantCtor(EnumVariant::new(enum_, 0)));
        match callee_from_func_ty(&db, expr, ctor_ty) {
            Err(TermLowerError::CalleeNotFunc(_, kind)) => {
                assert_eq!(kind, "enum variant constructor")
            }
            other => panic!("expected CalleeNotFunc(enum variant constructor), got {other:?}"),
        }
    }

    #[test]
    fn callee_not_const_reported() {
        let (db, file) = setup(
            "callee_not_const",
            "fn plain() -> u256 {\n    5\n}\n\nfn host() {\n    let x = plain()\n}",
        );
        let (top_mod, _) = db.top_mod(file);
        let body = func_body(&db, top_mod, "host");
        let expr = find_expr(&db, body, |e| matches!(e, Expr::Call(..)));
        assert!(matches!(
            lower_hir_to_term(&db, body, expr, no_assumptions(&db)),
            Err(TermLowerError::CalleeNotConst(..))
        ));
    }

    // --- success-path shapes ---

    #[test]
    fn lowers_int_and_bool_literals() {
        let (db, file) = setup("lit_shapes", "const P: bool = false\nconst Q: u256 = 42");
        let (top_mod, _) = db.top_mod(file);

        let p = lower_root(&db, const_body(&db, top_mod, "P")).unwrap();
        assert_eq!(*p.data(&db), TermNode::Bool(false));

        let q = lower_root(&db, const_body(&db, top_mod, "Q")).unwrap();
        assert_eq!(q, TermId::int(&db, BigUint::from(42u32)));
    }

    #[test]
    fn lowers_operator_shapes() {
        let (db, file) = setup(
            "operator_shapes",
            "const P: bool = !(1 + 2 == 3) && 4 % 2 < 5 || 6 != 7",
        );
        let (top_mod, _) = db.top_mod(file);
        let term = lower_root(&db, const_body(&db, top_mod, "P")).unwrap();
        // Shape: Or(And(Not(Cmp(Eq, Arith(Add,..),..)), Cmp(Lt, Arith(Rem,..),..)), Cmp(Ne,..))
        let TermNode::Or(and_term, ne_term) = *term.data(&db) else {
            panic!("expected Or at root, got {:?}", term.data(&db));
        };
        let TermNode::And(not_term, lt_term) = *and_term.data(&db) else {
            panic!("expected And, got {:?}", and_term.data(&db));
        };
        let TermNode::Not(eq_term) = *not_term.data(&db) else {
            panic!("expected Not, got {:?}", not_term.data(&db));
        };
        assert!(matches!(
            eq_term.data(&db),
            TermNode::Cmp {
                op: TermCmpOp::Eq,
                ..
            }
        ));
        assert!(matches!(
            lt_term.data(&db),
            TermNode::Cmp {
                op: TermCmpOp::Lt,
                ..
            }
        ));
        assert!(matches!(
            ne_term.data(&db),
            TermNode::Cmp {
                op: TermCmpOp::Ne,
                ..
            }
        ));
    }

    #[test]
    fn lowers_const_param() {
        let (db, file) = setup(
            "const_param",
            "fn host<const N: u256>() -> bool {\n    N >= 50\n}",
        );
        let (top_mod, _) = db.top_mod(file);
        let body = func_body(&db, top_mod, "host");
        let expr = find_expr(&db, body, |e| {
            matches!(e, Expr::Bin(_, _, BinOp::Comp(CompBinOp::GtEq)))
        });
        let term = lower_hir_to_term(&db, body, expr, no_assumptions(&db)).unwrap();
        let TermNode::Cmp {
            op: TermCmpOp::Ge,
            lhs,
            rhs,
        } = *term.data(&db)
        else {
            panic!("expected Ge comparison, got {:?}", term.data(&db));
        };
        assert!(matches!(lhs.data(&db), TermNode::ConstParam(_)));
        assert_eq!(rhs, TermId::int(&db, BigUint::from(50u32)));
    }

    #[test]
    fn lowers_const_ref_symbolically() {
        let (db, file) = setup(
            "const_ref",
            "const A: u256 = 5\n\nconst P: bool = A + 1 == 6",
        );
        let (top_mod, _) = db.top_mod(file);
        let term = lower_root(&db, const_body(&db, top_mod, "P")).unwrap();
        let a = named_const(&db, top_mod, "A");
        let TermNode::Cmp {
            op: TermCmpOp::Eq,
            lhs,
            ..
        } = *term.data(&db)
        else {
            panic!("expected Eq, got {:?}", term.data(&db));
        };
        let TermNode::Arith {
            op: TermArithOp::Add,
            lhs: a_ref,
            ..
        } = *lhs.data(&db)
        else {
            panic!("expected Add, got {:?}", lhs.data(&db));
        };
        // The const is referenced symbolically; its value (5) is NOT inlined.
        assert_eq!(*a_ref.data(&db), TermNode::ConstRef(a));
    }

    #[test]
    fn lowers_assoc_const_through_assumption() {
        let (db, file) = setup(
            "assoc_const",
            "trait HasSize {\n    const SIZE: u256\n}\n\nfn host<T>() -> bool\nwhere\n    T: HasSize\n{\n    T::SIZE >= 50\n}",
        );
        let (top_mod, _) = db.top_mod(file);
        let func = named_func(&db, top_mod, "host");
        let body = func_body(&db, top_mod, "host");
        let assumptions = func_assumptions(&db, func);
        let expr = find_expr(&db, body, |e| {
            matches!(e, Expr::Bin(_, _, BinOp::Comp(CompBinOp::GtEq)))
        });
        let term = lower_hir_to_term(&db, body, expr, assumptions).unwrap();
        let TermNode::Cmp {
            op: TermCmpOp::Ge,
            lhs,
            ..
        } = *term.data(&db)
        else {
            panic!("expected Ge, got {:?}", term.data(&db));
        };
        let TermNode::AssocConst { inst: _, name } = *lhs.data(&db) else {
            panic!("expected AssocConst, got {:?}", lhs.data(&db));
        };
        assert_eq!(name.data(&db), "SIZE");
    }

    #[test]
    fn lowers_const_fn_application() {
        let (db, file) = setup(
            "const_fn_app",
            "const fn check(_ x: u256) -> bool {\n    true\n}\n\nfn host() -> bool {\n    check(5)\n}",
        );
        let (top_mod, _) = db.top_mod(file);
        let body = func_body(&db, top_mod, "host");
        let expr = find_expr(&db, body, |e| matches!(e, Expr::Call(..)));
        let term = lower_hir_to_term(&db, body, expr, no_assumptions(&db)).unwrap();
        let check = named_func(&db, top_mod, "check");
        let TermNode::App {
            callee,
            ref generic_args,
            ref args,
        } = *term.data(&db)
        else {
            panic!("expected App, got {:?}", term.data(&db));
        };
        assert_eq!(callee, check);
        assert!(generic_args.is_empty());
        assert_eq!(args.as_slice(), &[TermId::int(&db, BigUint::from(5u32))]);
    }

    #[test]
    fn application_generic_args_are_part_of_identity() {
        let (db, file) = setup(
            "generic_app_identity",
            "const fn check<T>() -> bool {\n    true\n}\n\nfn host() -> bool {\n    check<u8>() && check<u16>() && check<u8>()\n}",
        );
        let (top_mod, _) = db.top_mod(file);
        let body = func_body(&db, top_mod, "host");
        let assumptions = no_assumptions(&db);
        let calls: Vec<_> = body
            .exprs(&db)
            .iter()
            .filter_map(|(id, expr)| {
                if let Partial::Present(Expr::Call(..)) = expr {
                    Some(id)
                } else {
                    None
                }
            })
            .collect();
        assert_eq!(calls.len(), 3);
        let terms: Vec<_> = calls
            .iter()
            .map(|&call| lower_hir_to_term(&db, body, call, assumptions).unwrap())
            .collect();
        // check<u8>() != check<u16>(), but the two check<u8>() coincide.
        assert_ne!(terms[0], terms[1]);
        assert_eq!(terms[0], terms[2]);
    }

    #[test]
    fn argument_labels_are_dropped_from_identity() {
        let (db, file) = setup(
            "label_identity",
            "const fn check(x: u256) -> bool {\n    true\n}\n\nfn host() -> bool {\n    check(x: 5) && check(5)\n}",
        );
        let (top_mod, _) = db.top_mod(file);
        let body = func_body(&db, top_mod, "host");
        let calls: Vec<_> = body
            .exprs(&db)
            .iter()
            .filter_map(|(id, expr)| {
                if let Partial::Present(Expr::Call(..)) = expr {
                    Some(id)
                } else {
                    None
                }
            })
            .collect();
        assert_eq!(calls.len(), 2);
        let labeled = lower_hir_to_term(&db, body, calls[0], no_assumptions(&db)).unwrap();
        let positional = lower_hir_to_term(&db, body, calls[1], no_assumptions(&db)).unwrap();
        assert_eq!(labeled, positional);
    }

    // =====================================================================
    // Suite 2: idempotence + determinism property tests.
    // =====================================================================

    /// Fixture providing non-literal leaves (a const param and a const
    /// ref) for synthetic term enumeration.
    const ENUM_FIXTURE: &str = "const A: u256 = 7\n\nconst fn check(_ x: u256) -> bool {\n    true\n}\n\nfn host<const N: u256>() -> bool {\n    N >= 50\n}";

    fn enumeration_leaves<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<TermId<'db>> {
        let body = func_body(db, top_mod, "host");
        let cmp = find_expr(db, body, |e| {
            matches!(e, Expr::Bin(_, _, BinOp::Comp(CompBinOp::GtEq)))
        });
        let cmp_term = lower_hir_to_term(db, body, cmp, no_assumptions(db)).unwrap();
        let TermNode::Cmp { lhs: param, .. } = *cmp_term.data(db) else {
            panic!("expected comparison fixture");
        };
        let const_ref = TermId::new(db, TermNode::ConstRef(named_const(db, top_mod, "A")));
        vec![
            TermId::int(db, BigUint::from(0u32)),
            TermId::int(db, BigUint::from(1u32)),
            TermId::int(db, BigUint::from(50u32)),
            TermId::bool(db, true),
            TermId::bool(db, false),
            param,
            const_ref,
        ]
    }

    /// All binary nodes over the given operand pool, plus negations.
    fn build_layer<'db>(db: &'db HirAnalysisTestDb, pool: &[TermId<'db>]) -> Vec<TermId<'db>> {
        let mut layer = Vec::new();
        for &lhs in pool {
            for &rhs in pool {
                for op in TermArithOp::ALL {
                    layer.push(TermId::new(db, TermNode::Arith { op, lhs, rhs }));
                }
                for op in TermCmpOp::ALL {
                    layer.push(TermId::new(db, TermNode::Cmp { op, lhs, rhs }));
                }
                layer.push(TermId::new(db, TermNode::And(lhs, rhs)));
                layer.push(TermId::new(db, TermNode::Or(lhs, rhs)));
            }
            layer.push(TermId::new(db, TermNode::Not(lhs)));
        }
        layer
    }

    #[test]
    fn normalize_is_idempotent_over_enumerated_terms() {
        let (db, file) = setup("idempotence", ENUM_FIXTURE);
        let (top_mod, _) = db.top_mod(file);
        let leaves = enumeration_leaves(&db, top_mod);
        let check = named_func(&db, top_mod, "check");

        let layer1 = build_layer(&db, &leaves);
        // A bounded second layer: pair each layer-1 term with a rotating
        // partner, one node kind per pairing.
        let mut layer2 = Vec::new();
        for (i, &lhs) in layer1.iter().enumerate() {
            let rhs = layer1[(i * 7 + 3) % layer1.len()];
            let op = TermArithOp::ALL[i % TermArithOp::ALL.len()];
            layer2.push(TermId::new(&db, TermNode::Arith { op, lhs, rhs }));
            let op = TermCmpOp::ALL[i % TermCmpOp::ALL.len()];
            layer2.push(TermId::new(&db, TermNode::Cmp { op, lhs, rhs }));
            layer2.push(TermId::new(&db, TermNode::And(lhs, rhs)));
            layer2.push(TermId::new(&db, TermNode::Or(lhs, rhs)));
            layer2.push(TermId::new(&db, TermNode::Not(lhs)));
            layer2.push(TermId::new(
                &db,
                TermNode::App {
                    callee: check,
                    generic_args: vec![],
                    args: vec![lhs, rhs],
                },
            ));
        }

        let mut checked = 0usize;
        for term in leaves.iter().chain(layer1.iter()).chain(layer2.iter()) {
            let once = normalize_term(&db, *term);
            let twice = normalize_term(&db, once);
            assert_eq!(
                once,
                twice,
                "normalize not idempotent for {:?}: {:?} vs {:?}",
                term.data(&db),
                once.data(&db),
                twice.data(&db)
            );
            checked += 1;
        }
        assert!(checked > 4000, "enumeration unexpectedly small: {checked}");
    }

    #[test]
    fn commutative_operand_order_is_irrelevant_after_normalization() {
        let (db, file) = setup("commutativity", ENUM_FIXTURE);
        let (top_mod, _) = db.top_mod(file);
        let leaves = enumeration_leaves(&db, top_mod);
        for &lhs in &leaves {
            for &rhs in &leaves {
                for op in [TermArithOp::Add, TermArithOp::Mul] {
                    let ab = TermId::new(&db, TermNode::Arith { op, lhs, rhs });
                    let ba = TermId::new(
                        &db,
                        TermNode::Arith {
                            op,
                            lhs: rhs,
                            rhs: lhs,
                        },
                    );
                    assert_eq!(normalize_term(&db, ab), normalize_term(&db, ba));
                }
                for op in [TermCmpOp::Eq, TermCmpOp::Ne] {
                    let ab = TermId::new(&db, TermNode::Cmp { op, lhs, rhs });
                    let ba = TermId::new(
                        &db,
                        TermNode::Cmp {
                            op,
                            lhs: rhs,
                            rhs: lhs,
                        },
                    );
                    assert_eq!(normalize_term(&db, ab), normalize_term(&db, ba));
                }
            }
        }
    }

    #[test]
    fn same_source_order_independence_through_lowering() {
        let (db, file) = setup(
            "source_commutativity",
            "fn host<const N: u256>() -> bool {\n    let a = N + 1 == 1 + N\n    let b = N * 2 == 2 * N\n    a && b\n}",
        );
        let (top_mod, _) = db.top_mod(file);
        let body = func_body(&db, top_mod, "host");
        let eqs: Vec<_> = body
            .exprs(&db)
            .iter()
            .filter_map(|(id, expr)| {
                if let Partial::Present(Expr::Bin(_, _, BinOp::Comp(CompBinOp::Eq))) = expr {
                    Some(id)
                } else {
                    None
                }
            })
            .collect();
        assert_eq!(eqs.len(), 2);
        for eq in eqs {
            let term = lower_hir_to_term(&db, body, eq, no_assumptions(&db)).unwrap();
            let TermNode::Cmp { lhs, rhs, .. } = *term.data(&db) else {
                panic!("expected comparison");
            };
            // `N + 1` and `1 + N` lower differently...
            assert_ne!(lhs, rhs);
            // ...but normalize to the same term.
            assert_eq!(normalize_term(&db, lhs), normalize_term(&db, rhs));
        }
    }

    #[test]
    fn same_source_text_interns_to_same_term() {
        let (db, file) = setup(
            "same_source_identity",
            "const P1: bool = 1 + 2 == 3 && true\nconst P2: bool = 1 + 2 == 3 && true",
        );
        let (top_mod, _) = db.top_mod(file);
        let p1 = lower_root(&db, const_body(&db, top_mod, "P1")).unwrap();
        let p2 = lower_root(&db, const_body(&db, top_mod, "P2")).unwrap();
        // Structural interning: identical predicate text in two distinct
        // bodies is one term, before any normalization.
        assert_eq!(p1, p2);
        assert_eq!(normalize_term(&db, p1), normalize_term(&db, p2));
    }

    #[test]
    fn non_commutative_operand_order_is_preserved() {
        let (db, file) = setup("non_commutative", ENUM_FIXTURE);
        let (top_mod, _) = db.top_mod(file);
        let leaves = enumeration_leaves(&db, top_mod);
        let param = leaves[5];
        let const_ref = leaves[6];
        for op in [TermArithOp::Sub, TermArithOp::Div, TermArithOp::Rem] {
            let ab = TermId::new(
                &db,
                TermNode::Arith {
                    op,
                    lhs: param,
                    rhs: const_ref,
                },
            );
            let ba = TermId::new(
                &db,
                TermNode::Arith {
                    op,
                    lhs: const_ref,
                    rhs: param,
                },
            );
            assert_ne!(normalize_term(&db, ab), normalize_term(&db, ba));
        }
        // `&&`/`||` short-circuit: operand order is semantics, not noise.
        let x = TermId::new(
            &db,
            TermNode::Cmp {
                op: TermCmpOp::Lt,
                lhs: param,
                rhs: const_ref,
            },
        );
        let y = TermId::new(
            &db,
            TermNode::Cmp {
                op: TermCmpOp::Gt,
                lhs: param,
                rhs: const_ref,
            },
        );
        assert_ne!(
            normalize_term(&db, TermId::new(&db, TermNode::And(x, y))),
            normalize_term(&db, TermId::new(&db, TermNode::And(y, x)))
        );
        assert_ne!(
            normalize_term(&db, TermId::new(&db, TermNode::Or(x, y))),
            normalize_term(&db, TermId::new(&db, TermNode::Or(y, x)))
        );
    }

    #[test]
    fn constant_folding_is_exact_and_total() {
        let (db, file) = setup(
            "folding",
            "const P: bool = 1 + 1 == 3\nconst Q: bool = 1 / 0 == 0\nconst R: u256 = 7 / 2 + 7 % 2 + (5 - 3) * 2",
        );
        let (top_mod, _) = db.top_mod(file);

        // effort2's arithmetic-false fixture folds all the way to `false`.
        let p = lower_root(&db, const_body(&db, top_mod, "P")).unwrap();
        assert_eq!(normalize_term(&db, p), TermId::bool(&db, false));

        // Division by zero must NOT fold (CTFE reports it; the term keeps
        // its shape so the comparison stays symbolic too).
        let q = lower_root(&db, const_body(&db, top_mod, "Q")).unwrap();
        let q_norm = normalize_term(&db, q);
        let TermNode::Cmp {
            op: TermCmpOp::Eq, ..
        } = *q_norm.data(&db)
        else {
            panic!(
                "1 / 0 == 0 must stay a comparison, got {:?}",
                q_norm.data(&db)
            );
        };

        // 7/2 + 7%2 + (5-3)*2 = 3 + 1 + 4 = 8.
        let r = lower_root(&db, const_body(&db, top_mod, "R")).unwrap();
        assert_eq!(
            normalize_term(&db, r),
            TermId::int(&db, BigUint::from(8u32))
        );

        // Underflowing subtraction stays symbolic.
        let underflow = TermId::new(
            &db,
            TermNode::Arith {
                op: TermArithOp::Sub,
                lhs: TermId::int(&db, BigUint::from(3u32)),
                rhs: TermId::int(&db, BigUint::from(5u32)),
            },
        );
        assert_eq!(normalize_term(&db, underflow), underflow);
    }

    /// SGK-B island differential: a derive provider's integer comparison
    /// (decided by the shared `compare_nats`) agrees with ordinary
    /// const-predicate folding (`normalize_cmp`) for every relation over every
    /// sample. Both route through `compare_nats`, so a future second comparator
    /// in either path would make them diverge and trip this test.
    #[test]
    fn steering_int_comparison_agrees_with_ctfe_folding() {
        let (db, _file) = setup("island_diff", "");
        let samples = [
            (0u32, 0u32),
            (0, 1),
            (1, 0),
            (1, 1),
            (2, 1),
            (1, 2),
            (7, 7),
            (50, 49),
        ];
        for (a, b) in samples {
            let (na, nb) = (BigUint::from(a), BigUint::from(b));
            for op in TermCmpOp::ALL {
                // CTFE path: fold a comparison of two integer-literal terms.
                let ctfe = normalize_cmp(
                    &db,
                    op,
                    TermId::int(&db, na.clone()),
                    TermId::int(&db, nb.clone()),
                );
                // Steering path: the shared `compare_nats` decider the provider
                // executor's integer comparison calls.
                let steering = TermId::bool(&db, compare_nats(op, &na, &nb));
                assert_eq!(
                    ctfe, steering,
                    "const-predicate folding and provider-steering compare_nats disagree \
                     for {op:?} on ({a}, {b})"
                );
            }
        }
    }

    #[test]
    fn boolean_short_circuit_folds() {
        let (db, file) = setup("bool_folds", ENUM_FIXTURE);
        let (top_mod, _) = db.top_mod(file);
        let leaves = enumeration_leaves(&db, top_mod);
        let param = leaves[5];
        let const_ref = leaves[6];
        let x = TermId::new(
            &db,
            TermNode::Cmp {
                op: TermCmpOp::Lt,
                lhs: param,
                rhs: const_ref,
            },
        );
        let t = TermId::bool(&db, true);
        let f = TermId::bool(&db, false);

        let and = |a, b| TermId::new(&db, TermNode::And(a, b));
        let or = |a, b| TermId::new(&db, TermNode::Or(a, b));

        assert_eq!(normalize_term(&db, and(t, x)), x);
        assert_eq!(normalize_term(&db, and(f, x)), f);
        assert_eq!(normalize_term(&db, and(x, t)), x);
        // And(x, false) is NOT folded: x may CTFE-fail before the rhs.
        assert_eq!(normalize_term(&db, and(x, f)), and(x, f));

        assert_eq!(normalize_term(&db, or(t, x)), t);
        assert_eq!(normalize_term(&db, or(f, x)), x);
        assert_eq!(normalize_term(&db, or(x, f)), x);
        // Or(x, true) is NOT folded for the same reason.
        assert_eq!(normalize_term(&db, or(x, t)), or(x, t));
    }

    #[test]
    fn double_negation_eliminated() {
        let (db, file) = setup("double_neg", ENUM_FIXTURE);
        let (top_mod, _) = db.top_mod(file);
        let leaves = enumeration_leaves(&db, top_mod);
        let param = leaves[5];
        let x = TermId::new(
            &db,
            TermNode::Cmp {
                op: TermCmpOp::Lt,
                lhs: param,
                rhs: TermId::int(&db, BigUint::from(4u32)),
            },
        );
        let not = |t| TermId::new(&db, TermNode::Not(t));

        assert_eq!(normalize_term(&db, not(not(x))), normalize_term(&db, x));
        assert_eq!(
            normalize_term(&db, not(not(not(x)))),
            normalize_term(&db, not(x))
        );
        assert_eq!(
            normalize_term(&db, not(TermId::bool(&db, true))),
            TermId::bool(&db, false)
        );
        // A single negation of a non-literal stays.
        assert_eq!(normalize_term(&db, not(x)), not(normalize_term(&db, x)));
    }

    // =====================================================================
    // Suite 3: non-canonicalization assertions + the TERM_LANG_VERSION
    // staleness mechanism.
    // =====================================================================

    #[test]
    fn ge_is_not_canonicalized_to_swapped_le() {
        // Both from source (the disclosed verbatim-contract example:
        // `N >= 50` does not match `50 <= N`) and structurally.
        let (db, file) = setup(
            "ge_vs_le",
            "fn host<const N: u256>() -> bool {\n    let ge = N >= 50\n    let le = 50 <= N\n    ge && le\n}",
        );
        let (top_mod, _) = db.top_mod(file);
        let body = func_body(&db, top_mod, "host");
        let ge = find_expr(&db, body, |e| {
            matches!(e, Expr::Bin(_, _, BinOp::Comp(CompBinOp::GtEq)))
        });
        let le = find_expr(&db, body, |e| {
            matches!(e, Expr::Bin(_, _, BinOp::Comp(CompBinOp::LtEq)))
        });
        let ge_term = lower_hir_to_term(&db, body, ge, no_assumptions(&db)).unwrap();
        let le_term = lower_hir_to_term(&db, body, le, no_assumptions(&db)).unwrap();

        assert_ne!(
            normalize_term(&db, ge_term),
            normalize_term(&db, le_term),
            "Ge(x, 50) must NOT be canonicalized into Le(50, x)"
        );

        // Structural double-check on the same operands.
        let TermNode::Cmp {
            lhs: x, rhs: fifty, ..
        } = *ge_term.data(&db)
        else {
            panic!("expected comparison");
        };
        let structural_ge = TermId::new(
            &db,
            TermNode::Cmp {
                op: TermCmpOp::Ge,
                lhs: x,
                rhs: fifty,
            },
        );
        let structural_le = TermId::new(
            &db,
            TermNode::Cmp {
                op: TermCmpOp::Le,
                lhs: fifty,
                rhs: x,
            },
        );
        assert_ne!(
            normalize_term(&db, structural_ge),
            normalize_term(&db, structural_le)
        );
    }

    #[test]
    fn gt_is_not_canonicalized_to_swapped_lt() {
        let (db, file) = setup("gt_vs_lt", ENUM_FIXTURE);
        let (top_mod, _) = db.top_mod(file);
        let leaves = enumeration_leaves(&db, top_mod);
        let param = leaves[5];
        let fifty = TermId::int(&db, BigUint::from(50u32));
        let gt = TermId::new(
            &db,
            TermNode::Cmp {
                op: TermCmpOp::Gt,
                lhs: param,
                rhs: fifty,
            },
        );
        let lt_swapped = TermId::new(
            &db,
            TermNode::Cmp {
                op: TermCmpOp::Lt,
                lhs: fifty,
                rhs: param,
            },
        );
        assert_ne!(normalize_term(&db, gt), normalize_term(&db, lt_swapped));
    }

    #[test]
    fn negated_eq_is_not_canonicalized_to_ne() {
        let (db, file) = setup("not_eq_vs_ne", ENUM_FIXTURE);
        let (top_mod, _) = db.top_mod(file);
        let leaves = enumeration_leaves(&db, top_mod);
        let param = leaves[5];
        let fifty = TermId::int(&db, BigUint::from(50u32));
        let not_eq = TermId::new(
            &db,
            TermNode::Not(TermId::new(
                &db,
                TermNode::Cmp {
                    op: TermCmpOp::Eq,
                    lhs: param,
                    rhs: fifty,
                },
            )),
        );
        let ne = TermId::new(
            &db,
            TermNode::Cmp {
                op: TermCmpOp::Ne,
                lhs: param,
                rhs: fifty,
            },
        );
        assert_ne!(normalize_term(&db, not_eq), normalize_term(&db, ne));
    }

    #[test]
    fn distinct_relations_stay_distinct_on_equal_operands() {
        let (db, file) = setup("distinct_relations", ENUM_FIXTURE);
        let (top_mod, _) = db.top_mod(file);
        let leaves = enumeration_leaves(&db, top_mod);
        let param = leaves[5];
        let fifty = TermId::int(&db, BigUint::from(50u32));
        let normalized: Vec<_> = TermCmpOp::ALL
            .iter()
            .map(|&op| {
                normalize_term(
                    &db,
                    TermId::new(
                        &db,
                        TermNode::Cmp {
                            op,
                            lhs: param,
                            rhs: fifty,
                        },
                    ),
                )
            })
            .collect();
        for (i, a) in normalized.iter().enumerate() {
            for b in normalized.iter().skip(i + 1) {
                assert_ne!(a, b, "two distinct relations normalized to one term");
            }
        }
    }

    // --- TERM_LANG_VERSION staleness mechanism ---

    /// Exhaustively destructures a node (full field lists, no `..` on
    /// payload-bearing variants) and returns its shape descriptor. Adding,
    /// removing, or re-shaping a variant fails to compile here, forcing the
    /// descriptor — and therefore the fingerprint and version — to be
    /// revisited.
    fn shape_of(node: &TermNode<'_>) -> &'static str {
        match node {
            TermNode::Int(_int) => "Int(IntegerId)",
            TermNode::Bool(_value) => "Bool(bool)",
            TermNode::ConstParam(_ty) => "ConstParam(TyId:const-ty-param)",
            TermNode::ConstRef(_const) => "ConstRef(Const)",
            TermNode::AssocConst { inst: _, name: _ } => {
                "AssocConst{inst:TraitInstId,name:IdentId}"
            }
            TermNode::Arith {
                op: _,
                lhs: _,
                rhs: _,
            } => "Arith{op:TermArithOp,lhs:TermId,rhs:TermId}",
            TermNode::Cmp {
                op: _,
                lhs: _,
                rhs: _,
            } => "Cmp{op:TermCmpOp,lhs:TermId,rhs:TermId}",
            TermNode::And(_lhs, _rhs) => "And(TermId,TermId)",
            TermNode::Or(_lhs, _rhs) => "Or(TermId,TermId)",
            TermNode::Not(_inner) => "Not(TermId)",
            TermNode::App {
                callee: _,
                generic_args: _,
                args: _,
            } => "App{callee:Func,generic_args:Vec<TyId>,args:Vec<TermId>}",
        }
    }

    fn arith_op_shape(op: TermArithOp) -> &'static str {
        match op {
            TermArithOp::Add => "Add",
            TermArithOp::Sub => "Sub",
            TermArithOp::Mul => "Mul",
            TermArithOp::Div => "Div",
            TermArithOp::Rem => "Rem",
        }
    }

    fn cmp_op_shape(op: TermCmpOp) -> &'static str {
        match op {
            TermCmpOp::Eq => "Eq",
            TermCmpOp::Ne => "Ne",
            TermCmpOp::Lt => "Lt",
            TermCmpOp::Le => "Le",
            TermCmpOp::Gt => "Gt",
            TermCmpOp::Ge => "Ge",
        }
    }

    #[test]
    fn term_lang_shape_covers_all_variants() {
        // Build one term of every variant; the set of their shape
        // descriptors must equal TERM_LANG_SHAPE exactly. Combined with the
        // exhaustive destructuring in `shape_of`, this pins the descriptor
        // list to the real enum.
        let (db, file) = setup("shape_coverage", ENUM_FIXTURE);
        let (top_mod, _) = db.top_mod(file);
        let leaves = enumeration_leaves(&db, top_mod);
        let param = leaves[5];
        let const_ref = leaves[6];
        let one = TermId::int(&db, BigUint::from(1u32));
        let t = TermId::bool(&db, true);
        let check = named_func(&db, top_mod, "check");

        // An AssocConst sample needs the trait fixture.
        let (db2, file2) = setup(
            "shape_coverage_assoc",
            "trait HasSize {\n    const SIZE: u256\n}\n\nfn host<T>() -> bool\nwhere\n    T: HasSize\n{\n    T::SIZE >= 50\n}",
        );
        let (top_mod2, _) = db2.top_mod(file2);
        let func2 = named_func(&db2, top_mod2, "host");
        let body2 = func_body(&db2, top_mod2, "host");
        let cmp2 = find_expr(&db2, body2, |e| {
            matches!(e, Expr::Bin(_, _, BinOp::Comp(CompBinOp::GtEq)))
        });
        let assoc_cmp =
            lower_hir_to_term(&db2, body2, cmp2, func_assumptions(&db2, func2)).unwrap();
        let TermNode::Cmp { lhs: assoc, .. } = *assoc_cmp.data(&db2) else {
            panic!("expected comparison fixture");
        };

        let samples = vec![
            shape_of(one.data(&db)),
            shape_of(t.data(&db)),
            shape_of(param.data(&db)),
            shape_of(const_ref.data(&db)),
            shape_of(assoc.data(&db2)),
            shape_of(
                TermId::new(
                    &db,
                    TermNode::Arith {
                        op: TermArithOp::Add,
                        lhs: param,
                        rhs: one,
                    },
                )
                .data(&db),
            ),
            shape_of(
                TermId::new(
                    &db,
                    TermNode::Cmp {
                        op: TermCmpOp::Ge,
                        lhs: param,
                        rhs: one,
                    },
                )
                .data(&db),
            ),
            shape_of(TermId::new(&db, TermNode::And(t, t)).data(&db)),
            shape_of(TermId::new(&db, TermNode::Or(t, t)).data(&db)),
            shape_of(TermId::new(&db, TermNode::Not(t)).data(&db)),
            shape_of(
                TermId::new(
                    &db,
                    TermNode::App {
                        callee: check,
                        generic_args: vec![],
                        args: vec![one],
                    },
                )
                .data(&db),
            ),
        ];

        assert_eq!(
            samples,
            TERM_LANG_SHAPE.to_vec(),
            "TERM_LANG_SHAPE is out of sync with TermNode"
        );

        let arith: Vec<_> = TermArithOp::ALL
            .iter()
            .map(|&op| arith_op_shape(op))
            .collect();
        assert_eq!(arith, TERM_ARITH_OP_SHAPE.to_vec());
        let cmp: Vec<_> = TermCmpOp::ALL.iter().map(|&op| cmp_op_shape(op)).collect();
        assert_eq!(cmp, TERM_CMP_OP_SHAPE.to_vec());
    }

    #[test]
    fn term_lang_version_ledger_matches_shape() {
        // The versioned-key fixture: any change to the term-language shape
        // (TermNode variants, operator sets, or normalization rules)
        // changes the fingerprint and fails this test until a NEW ledger
        // entry with a bumped TERM_LANG_VERSION is appended.
        let fingerprint = term_lang_fingerprint();
        let (head_version, head_fingerprint) =
            *TERM_LANG_LEDGER.last().expect("ledger must not be empty");

        assert_eq!(
            head_version, TERM_LANG_VERSION,
            "ledger head version must equal TERM_LANG_VERSION; append a new \
             (version, fingerprint) entry instead of editing in place"
        );
        assert_eq!(
            head_fingerprint,
            fingerprint,
            "term-language shape changed: bump TERM_LANG_VERSION to {} and append \
             ({}, {fingerprint:#018x}) to TERM_LANG_LEDGER",
            TERM_LANG_VERSION + 1,
            TERM_LANG_VERSION + 1,
        );

        // The ledger is append-only history with strictly increasing
        // versions.
        for pair in TERM_LANG_LEDGER.windows(2) {
            assert!(
                pair[0].0 < pair[1].0,
                "ledger versions must strictly increase"
            );
        }
        assert!(!NORMALIZATION_RULES.is_empty());
    }
}
