//! The derive-provider command-language executor.
//!
//! Provider bodies are written in a restricted subset of Fe: boolean locals,
//! `if`/`else`, `for` loops over reflection lists, and method calls on the
//! capability values bound by the `uses` clause. This module interprets such
//! a body at compile time (inside the expansion stage) and records the
//! builder commands it issues. The commands are *transient command data*:
//! they are replayed into real HIR by [`super::provider_synthesis`] and
//! never persisted as a semantic artifact.
//!
//! Compared to the metaprogramming prototype this interpreter:
//! * dispatches method calls on the *resolved value* of the receiver, so
//!   `let builder = ..` shadowing behaves like ordinary Fe scoping instead
//!   of matching receiver identifier text;
//! * makes `return` actually stop execution;
//! * enforces an explicit step budget and a command-count cap, so a buggy
//!   provider degrades into a diagnostic instead of a hang.

use parser::TextRange;
use parser::ast::prelude::*;

use super::{
    base_scope_graph_impl,
    provider::{TargetReflection, ValidatedProvider, canonical_trait_path, resolve_trait_def},
    top_mod_ast,
};
use crate::{
    HirDb,
    analysis::ty::ty_def::MAX_INLINE_STRING_BYTES,
    hir_def::{
        BinOp, Body, CompBinOp, Cond, CondId, Expr, ExprId, Func, GenericArg, GenericArgListId,
        IdentId, ItemKind, LitKind, LogicalBinOp, MatchArm, Partial, Pat, PatId, PathId, PathKind,
        QuoteBody, Stmt, StmtId, StringId, Trait, TraitRefId, TypeId, TypeKind,
        scope_graph::ScopeId,
    },
    span::HirOrigin,
};

/// Maximum number of statement/expression evaluations for one provider run.
const STEP_BUDGET: usize = 100_000;
/// Maximum number of builder commands (and generated expression nodes) for
/// one provider run.
const COMMAND_BUDGET: usize = 10_000;

// === FROZEN command surface (TD5.0) ====================================
//
// FREEZE (TD5.0): the provider-body executor recognizes a fixed set of
// operations. Adding a provider-body op requires a TD5 category decision
// (see docs/dev/TD5_PROVIDER_COMMAND_SURFACE.md). The canonical lists below
// are the single source of truth for the *names* the dispatch recognizes;
// the `recognized_*_ops_are_frozen` tests pin them so the surface cannot
// grow silently while later TD5 rungs shrink it. Update these lists ONLY as
// part of a TD5 rung (and update the inventory doc in the same change).
//
// These are pure inventory: the dispatch `match`es in `eval_builder_method`
// and `eval_method_call` remain the runtime authority; the catch-all arms
// `debug_assert!` that any name routed to them is NOT in the canonical list,
// so the list and the dispatch cannot drift apart.

/// Every `builder.*` operation recognized by [`ProviderExecutor::eval_builder_method`].
/// One entry per `(method, args)` arm spelling. Includes obligation
/// (`require`), finish (`finish`), and the generated-HIR builder cluster.
const RECOGNIZED_BUILDER_OPS: &[&str] = &[
    // B-obl
    "require",
    // FIN
    "finish",
    // B-build: emit
    "emit_method",
    "emit_const",
    "emit_assoc_ty",
    // B-build: generated expressions
    "bool",
    "and",
    "or",
    "add",
    "eq",
    "lt",
    "gt",
    "self_ref",
    "arg_ref",
    "field_get",
    "call",
    "trait_call",
    "trait_const",
    "static_call",
    "keccak",
    "struct_init",
    "variant_init",
    "with_field",
    "match_expr",
    "with_arm",
    "variant_binder",
    "tuple_expr",
    "with_elem",
    "str",
    // B-build: generated patterns
    "wildcard_pat",
    "variant_pat",
    // B-build: generated types
    "ty",
    "target_ty",
    "self_ty",
    "str_ty",
    "tuple_ty",
    "with_elem_ty",
    "trait_assoc_ty",
    // B-build: generated method signatures.
    // DEVX-A: the `method`/`with_self`/`with_arg`/`returns` signature dance was
    // dropped — the emitted method's signature is inferred from the goal trait's
    // declaration at `emit_method(name, body)` (see `infer_method_sig`), so
    // these four ops have no authoring use and are off the recognized surface.
    // B-build: compile-time string fold spelled as a `builder.*` method.
    "concat",
    // TD5c: `same_ty`/`same_field` are NO LONGER here. They are spelled
    // `builder.*` but are pure read-only identity comparisons (mis-shelved by
    // spelling, per the inventory); they moved onto the typed read-only
    // `ReflectionCompare` table and are resolved in the `eval_builder_method`
    // catch-all by name — they are no longer bespoke `("name",` arms, so the
    // freeze scan no longer counts them.
];

/// Every reflection-read operation still recognized as a *bespoke* method-call
/// arm on a `reflect`/`field`/`variant` handle in
/// [`ProviderExecutor::eval_method_call`].
///
/// TD5c: this list is now EMPTY. Every reflection read — scalar AND iterating —
/// has migrated off the bespoke executor:
/// * the non-iterating reads (`reflect.is_struct`/`is_enum`/`target_name`,
///   `field.ty`/`name`, `variant.is_default`/`precedes`) migrated onto the
///   typed read-only handle the receiver `Value` carries
///   ([`ReflectHandle`]/[`FieldHandle`]/[`VariantHandle`]), which owns its own
///   read vocabulary; the executor consults the handle by name and no longer
///   pins any of those names as dispatch arms.
/// * the iterables (`reflect.fields`/`reflect.variants`/`variant.fields`) are
///   now ORDINARY method calls returning a `Value::Seq` of those same handles
///   (built eagerly in [`ProviderExecutor::reflection_sequence`] /
///   [`ProviderExecutor::variant_field_sequence`]); the `for`-loop iterates the
///   resulting sequence value like any other, so there is no longer an
///   `eval_iterable` interception of the iterable *expression*.
///
/// The only executor-owned named surface that remains is the `builder.*` op
/// cluster ([`RECOGNIZED_BUILDER_OPS`]).
const RECOGNIZED_REFLECT_OPS: &[&str] = &[];

/// FREEZE (TD5.0): a compile-time pin on the size of the recognized command
/// surface. Changing any list above without updating these counts (and
/// `docs/dev/TD5_PROVIDER_COMMAND_SURFACE.md`) fails to compile. The
/// `recognized_*_ops_*` tests additionally pin exact membership against the
/// dispatch arms. (This `const` block also keeps the lists referenced in
/// non-test builds, so they are not dead code.)
const _: () = {
    // TD5c: was 45; `same_ty`/`same_field` (mis-shelved `builder.*`-spelled
    // identity reads) moved onto the typed read-only `ReflectionCompare` table
    // (45 → 43). DEVX-A: the four signature-dance ops (`method`/`with_self`/
    // `with_arg`/`returns`) were dropped — the emitted method's signature is now
    // inferred from the goal trait's declaration at `emit_method(name, body)`
    // (43 → 39).
    assert!(RECOGNIZED_BUILDER_OPS.len() == 39);
    // TD5c: was 7, then 4, now 0; ALL reflection reads — non-iterating (onto the
    // typed read-only handles `ReflectHandle`/`FieldHandle`/`VariantHandle`) AND
    // the `fields`/`variants` iterables (now ordinary method calls returning a
    // `Value::Seq` of those handles) — are off the bespoke executor. The
    // executor's only named surface is now `builder.*`.
    assert!(RECOGNIZED_REFLECT_OPS.is_empty());
};

// === end FROZEN command surface ========================================

/// Why a provider execution failed. Rendered into a derive diagnostic at the
/// request site (with the failing provider expression as the primary span
/// when it lies in the same file, see `expansion`).
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub enum ProviderFailureKind {
    /// The body uses a construct outside the provider command language.
    UnsupportedBody,
    /// The provider returned without calling `builder.finish()`.
    MissingFinish,
    /// `builder.finish()` was called more than once.
    DuplicateFinish,
    /// A builder command was issued after `builder.finish()`.
    CommandAfterFinish,
    /// A `require` command had a malformed trait argument or operand.
    InvalidRequirement { detail: String },
    /// A method-emission command was malformed (bad signature value, body
    /// value, or duplicate method name).
    InvalidMethod { detail: String },
    /// An associated-item emission command (`emit_const`, `emit_assoc_ty`)
    /// was malformed (bad name, type, or value, or a duplicate name).
    InvalidAssoc { detail: String },
    /// A compile-time string operation was malformed (non-string operand,
    /// or a piece exceeding the inline string capacity).
    InvalidString { detail: String },
    /// A quote template was malformed: an unsupported construct in the
    /// body, an open name the destination method does not bind, or a hole
    /// filled with a wrong-kind value.
    InvalidQuote { detail: String },
    /// The interpreter step budget or command cap was exceeded.
    BudgetExceeded,
}

impl ProviderFailureKind {
    pub fn message(&self) -> String {
        match self {
            Self::UnsupportedBody => {
                "this construct is not supported in derive provider bodies".into()
            }
            Self::MissingFinish => "the provider returned without calling `finish`".into(),
            Self::DuplicateFinish => "the provider called `finish` more than once".into(),
            Self::CommandAfterFinish => {
                "the provider issued a builder command after `finish`".into()
            }
            Self::InvalidRequirement { detail } => format!("invalid `require` command: {detail}"),
            Self::InvalidMethod { detail } => format!("invalid method emission: {detail}"),
            Self::InvalidAssoc { detail } => {
                format!("invalid associated item emission: {detail}")
            }
            Self::InvalidString { detail } => {
                format!("invalid compile-time string operation: {detail}")
            }
            Self::InvalidQuote { detail } => format!("invalid quote: {detail}"),
            Self::BudgetExceeded => {
                "the provider exceeded its compile-time execution budget".into()
            }
        }
    }
}

/// An execution failure, with the source range of the offending construct
/// *inside the provider's file*.
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub(super) struct ExecError {
    pub(super) kind: ProviderFailureKind,
    pub(super) range: TextRange,
}

/// A generated expression node, built by builder expression commands and
/// replayed into real HIR expressions by the synthesis module.
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub(super) enum GenExpr<'db> {
    Bool(bool),
    /// `lhs && rhs`
    And(GenExprId, GenExprId),
    /// `lhs || rhs`
    Or(GenExprId, GenExprId),
    /// `lhs + rhs` (integer addition; used by layout-fact providers folding
    /// per-field size consts, e.g. ABI `HEAD_WORDS`).
    Add(GenExprId, GenExprId),
    /// The generated method's `self` value.
    SelfRef,
    /// A reference to a generated method parameter by name.
    ArgRef(IdentId<'db>),
    /// `base.field`
    FieldGet(GenExprId, FieldKey),
    /// `lhs == rhs`
    EqCmp(GenExprId, GenExprId),
    /// `lhs < rhs`
    LtCmp(GenExprId, GenExprId),
    /// `lhs > rhs`
    GtCmp(GenExprId, GenExprId),
    /// `<ty as GoalishTrait>::method(args..)` — a qualified call of the
    /// request's goal trait method on `ty` (`Self::method(args..)` when `ty`
    /// is the `Self` type).
    TraitCall {
        ty: TypeId<'db>,
        method: IdentId<'db>,
        args: Vec<GenExprId>,
    },
    /// `<ty as GoalishTrait>::NAME` — a qualified reference to an associated
    /// const of the request's goal trait on `ty` (`Self::NAME` when `ty` is
    /// the `Self` type).
    TraitConst {
        ty: TypeId<'db>,
        name: IdentId<'db>,
    },
    /// `<ty as Trait>::NAME` — a qualified reference to an associated const of
    /// an *arbitrary* (non-goal) trait on `ty`, e.g. `<FieldTy as AbiSize>::HEAD_SIZE`
    /// emitted by an `Encode` provider. `trait_path` is the canonical
    /// (import-resolved) trait path, so synthesis spells a qualifier that
    /// resolves in the generated impl's scope regardless of the user module's
    /// imports. (The goal-trait case uses the shorter `TraitConst`, which can
    /// also spell `Self::NAME`.)
    QualifiedConst {
        ty: TypeId<'db>,
        trait_path: PathId<'db>,
        name: IdentId<'db>,
    },
    /// `receiver.method(args..)` — a method call on a generated expression.
    MethodCall {
        receiver: GenExprId,
        method: IdentId<'db>,
        args: Vec<GenExprId>,
    },
    /// `path(args..)` — a call through a path built from a type as written
    /// with an associated-function name appended (e.g. `Hash712::new()`).
    StaticCall {
        path: PathId<'db>,
        args: Vec<GenExprId>,
    },
    /// A string literal with the exact inline width of its text.
    StrLit(StringId<'db>),
    /// `(elem0, elem1, ..)`
    Tuple(Vec<GenExprId>),
    /// `core::keccak(arg)`
    Keccak(GenExprId),
    /// `Self { field: value, .. }`
    StructInit {
        fields: Vec<(FieldKey, GenExprId)>,
    },
    /// `Enum::Variant` / `Enum::Variant(..)` / `Enum::Variant { .. }`
    VariantInit {
        variant: usize,
        fields: Vec<(FieldKey, GenExprId)>,
    },
    /// `match scrutinee { arms }`
    Match {
        scrutinee: GenExprId,
        arms: Vec<(GenPatId, GenExprId)>,
    },
    /// A reference to the binder introduced for `field` by a
    /// [`GenPat::Variant`] pattern with the same `prefix`.
    VariantBinder {
        variant: usize,
        field: usize,
        prefix: IdentId<'db>,
    },
}

/// A generated type, built by builder type commands and materialized into a
/// real [`TypeId`] by the synthesis module (some forms, like exact-width
/// string types, need a lowering context to build their const-argument
/// bodies, so materialization cannot happen during execution).
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub(super) enum GenTy<'db> {
    /// `String<LEN>` — an exact-width inline string type.
    StringN(usize),
    /// A tuple of generated types.
    Tuple(Vec<GenTyId>),
    /// `<ty as GoalishTrait>::name` — a projection of the goal trait's
    /// associated type on `ty`.
    Projection { ty: TypeId<'db>, name: IdentId<'db> },
    /// A type as written (e.g. from `ty<T>()` or `field.ty()`).
    Concrete(TypeId<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub(super) enum GenPat<'db> {
    Wildcard,
    /// A pattern matching `variant`, binding every payload field to
    /// `{prefix}_{field-name-or-index}`.
    Variant {
        variant: usize,
        prefix: IdentId<'db>,
    },
}

/// A generated method signature. DEVX-A: inferred from the goal trait's
/// declaration of the emitted method at `emit_method(name, body)` (see
/// [`ProviderExecutor::infer_method_sig`]); previously built op-by-op with the
/// dropped `method`/`with_self`/`with_arg`/`returns` dance.
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub(super) struct GenMethodSig<'db> {
    pub(super) name: IdentId<'db>,
    pub(super) takes_self: bool,
    pub(super) args: Vec<(IdentId<'db>, TypeId<'db>)>,
    pub(super) ret: Option<TypeId<'db>>,
}

/// A field reference: `(variant index, field index)` into the target
/// reflection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct FieldKey {
    pub(super) variant: Option<usize>,
    pub(super) index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct GenExprId(pub(super) usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct GenPatId(pub(super) usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct GenTyId(pub(super) usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct SigId(pub(super) usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct QuoteId(usize);

/// A quote template value: the inert body of a `quote(open, ..) { .. }`
/// expression plus the hole values captured when the quote expression was
/// evaluated. Templates only become generated expressions when a builder
/// emission command elaborates them (see [`ProviderExecutor::elaborate_quote`]).
#[derive(Debug, Clone)]
struct QuoteTemplate<'db> {
    /// The `Expr::Quote` expression, for error spans.
    origin: ExprId,
    /// Declared open names (`quote(other) { .. }`); `self` is implicitly
    /// open.
    open: Vec<IdentId<'db>>,
    /// The template body: a block expression in the provider's body, or a
    /// match-arm sequence.
    body: QuoteBody,
    /// Captured hole values, keyed by the hole expression
    /// (`Expr::QuoteHole` / `Expr::QuoteFieldHole`) for expression holes,
    /// and by the hole's inner expression for pattern holes
    /// (`Pat::QuoteHole`).
    holes: Vec<(ExprId, Value<'db>)>,
}

/// A binder group introduced by a `${variant}(group)` arm pattern enclosing
/// the elaboration point: the group name and the matched variant's index.
type BinderGroup<'db> = (IdentId<'db>, usize);

/// A typed *provider effect*: the observable, side-effecting things a provider
/// body does that re-enter ordinary compilation, recorded as an internal,
/// dumpable IR seam (TD5.1).
///
/// This is the strangler-fig contract for the executor burn-down: each effect
/// family migrated OUT of the bespoke `BuilderCommand` replay and into this
/// typed trace, which synthesis replays. It is deliberately **observability
/// only** — it is NOT a parallel command language. It lands paired with the
/// migration of the effect it records (the ratchet rule); a trace with no
/// migration is not progress.
///
/// TD5.2 migrated the first family: [`ProviderEffect::Require`] — a
/// provider-origin trait obligation read by
/// [`super::provider_synthesis::requirement_where_clause`], which replays a
/// generic-param requirement as an ordinary `ty: Trait` where-predicate on the
/// generated impl. A concrete requirement is a concrete obligation discharged at
/// the generated body's use site. Both flow through normal obligation checking.
///
/// TD5.x then migrated the three EMIT families
/// ([`ProviderEffect::EmitMethod`]/[`ProviderEffect::EmitConst`]/
/// [`ProviderEffect::EmitAssocTy`]) off the last bespoke replay vehicle and
/// DELETED `BuilderCommand`. The emit effects carry the same generated-item
/// payloads the old commands did; synthesis
/// ([`super::provider_synthesis::synthesize_provider_impl`]) walks this trace in
/// push order to build the generated impl's members. This trace is now the SOLE
/// replay authority for everything a provider body does — requirements (filter
/// `Require`) and emitted members (filter `Emit*`).
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub(super) enum ProviderEffect<'db> {
    /// `builder.require<Trait>(ty)`: the generated impl asserts `ty: Trait`.
    ///
    /// Carries the provenance of the requirement when it is cheaply
    /// recoverable: `field_origin` is the reflected field whose `field.ty()`
    /// produced `ty` (when the require argument was literally `field.ty()`),
    /// for "provider required `Trait<ty>` because field `f` was reflected"
    /// attribution. `None` when the require argument was some other type
    /// expression (e.g. `builder.ty<u256>()`).
    Require {
        ty: TypeId<'db>,
        trait_path: PathId<'db>,
        field_origin: Option<FieldKey>,
    },
    /// `builder.emit_method(name, body)`: a method member of the generated
    /// impl. `sig` is the signature inferred from the goal trait's declaration
    /// of `name` (DEVX-A); `body` is the generated body expression.
    EmitMethod { sig: SigId, body: GenExprId },
    /// `builder.emit_assoc_ty(name, ty)`: `type name = ty` in the generated
    /// impl.
    EmitAssocTy { name: IdentId<'db>, ty: GenTyId },
    /// `builder.emit_const(name, ty, value)`: `const name: ty = value` in the
    /// generated impl.
    EmitConst {
        name: IdentId<'db>,
        ty: GenTyId,
        value: GenExprId,
    },
}

/// The provider impl's *skeleton*: the typed effect trace plus the arenas the
/// generated *signature* and *type* ids index into. Everything a downstream
/// query needs to shape the `impl` (where-clause + member declarations) WITHOUT
/// materializing the method bodies (TD5.x-2 cleave). The skeleton arenas are
/// disjoint from [`ProviderBodies`]: no `GenExpr` references a `GenTyId`, and
/// `GenMethodSig` holds resolved `TypeId`s, never `GenTy` arena indices.
#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub(super) struct ProviderSkeleton<'db> {
    /// The typed effect trace (TD5.1). Recorded in body-execution order; the
    /// SOLE replay authority for everything a provider body does. Internal/
    /// dumpable; consumed by [`super::provider_synthesis`], which filters
    /// `Require` for where-predicates and `Emit*` for generated members.
    pub(super) effects: Vec<ProviderEffect<'db>>,
    pub(super) sigs: Vec<GenMethodSig<'db>>,
    pub(super) tys: Vec<GenTy<'db>>,
}

/// The provider impl's *bodies*: the arenas the generated expression/pattern
/// ids index into. Split off from [`ProviderSkeleton`] (TD5.x-2) so a later
/// step can move body production into a separate downstream query.
#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub(super) struct ProviderBodies<'db> {
    pub(super) exprs: Vec<GenExpr<'db>>,
    pub(super) pats: Vec<GenPat<'db>>,
}

/// The successful result of running a provider body: the [`ProviderSkeleton`]
/// (effect trace + signature/type arenas) plus the [`ProviderBodies`]
/// (expression/pattern arenas the generated ids index into).
#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub(super) struct ProviderOutput<'db> {
    pub(super) skeleton: ProviderSkeleton<'db>,
    pub(super) bodies: ProviderBodies<'db>,
}

impl<'db> ProviderOutput<'db> {
    /// Renders the effect trace as a stable, dumpable string (TD5.1
    /// observability). One line per recorded effect.
    #[cfg_attr(not(test), allow(dead_code))]
    pub(super) fn dump_effects(&self, db: &'db dyn HirDb) -> String {
        use std::fmt::Write;
        let mut out = String::new();
        for effect in &self.skeleton.effects {
            match effect {
                ProviderEffect::Require {
                    ty,
                    trait_path,
                    field_origin,
                } => {
                    let _ = write!(
                        out,
                        "require {}: {}",
                        ty.pretty_print(db),
                        trait_path.pretty_print(db),
                    );
                    if let Some(field) = field_origin {
                        let _ = write!(out, " (field {:?}.{})", field.variant, field.index);
                    }
                    out.push('\n');
                }
                ProviderEffect::EmitMethod { sig, .. } => {
                    let _ = writeln!(out, "emit_method {}", self.skeleton.sigs[sig.0].name.data(db));
                }
                ProviderEffect::EmitAssocTy { name, .. } => {
                    let _ = writeln!(out, "emit_assoc_ty {}", name.data(db));
                }
                ProviderEffect::EmitConst { name, .. } => {
                    let _ = writeln!(out, "emit_const {}", name.data(db));
                }
            }
        }
        out
    }
}

/// A compile-time value in the provider command language.
///
/// Not `Copy`: TD5c's [`Value::Seq`] carries an owned `Vec` of element values
/// (the reflection iterables' handles). Every other variant is small and
/// `Copy`; the few sites that previously relied on copying a looked-up/captured
/// value now `clone()` (cheap for the non-`Seq` variants, an owned vector copy
/// for a bound sequence).
#[derive(Debug, Clone)]
enum Value<'db> {
    Bool(bool),
    /// A compile-time string (string literals, reflected names, and
    /// `concat` results).
    Str(StringId<'db>),
    /// A reflected field handle, as a typed read-only handle (TD5c). Scalar
    /// reads (`ty`/`name`) resolve against the handle's own property table; its
    /// `FieldKey` identity (which flows into generated HIR / provenance /
    /// identity comparisons) is exposed via [`FieldHandle::key`].
    Field(FieldHandle<'db>),
    /// A reflected variant handle, as a typed read-only handle (TD5c). The
    /// scalar read `is_default` and the binary read `precedes` resolve against
    /// the handle's own vocabulary; its declaration-order index identity is
    /// exposed via [`VariantHandle::index`].
    Variant(VariantHandle),
    /// A type witness (e.g. the result of `field.ty()`).
    Ty(TypeId<'db>),
    /// A generated type (e.g. the result of `str_ty` / `tuple_ty`).
    GenTy(GenTyId),
    /// A generated expression.
    Expr(GenExprId),
    /// A generated pattern.
    Pat(GenPatId),
    /// A quote template (`quote { .. }`).
    Quote(QuoteId),
    /// The `builder: mut ImplBuilder<..>` capability.
    Builder,
    /// The `reflect: Reflect<..>` capability, as a typed read-only handle
    /// (TD5c). Scalar reads (`is_struct`/`is_enum`/`target_name`) resolve
    /// against the handle's own property table, not bespoke executor arms.
    Reflect(ReflectHandle<'db>),
    /// An opaque evidence value (the provider's ordinary parameters).
    Evidence,
    /// The result of a command call; carries no data.
    Unit,
    /// An ordinary compile-time sequence of values (TD5c). The reflection
    /// iterables (`reflect.fields()`/`reflect.variants()`/`variant.fields()`)
    /// are plain method calls returning one of these — a `Vec` of typed
    /// read-only handles (`Value::Field`/`Value::Variant`) built eagerly from
    /// the reflection at the call site. The `for`-loop iterates this like any
    /// other sequence value; the executor no longer special-cases the iterable
    /// *expression*.
    Seq(Vec<Value<'db>>),
}

/// A typed read-only compile-time handle over the derive target's reflection
/// (TD5c). It is the value `Value::Reflect` carries.
///
/// The point of the handle is to take the `reflect.*` *scalar* reads OFF the
/// bespoke executor: instead of `eval_method_call` string-matching
/// `("is_struct", [])` / `("is_enum", [])` / `("target_name", [])` against a
/// `Value::Reflect` receiver and reaching into ambient executor state, the
/// handle owns its own read-only property vocabulary ([`Self::scalar_read`]).
/// The executor no longer knows those names — it asks the handle to resolve a
/// named, argument-free read and turns the result into a `Value`. This is a
/// read-only CTFE view: it carries copies of the facts (no `&` into the
/// reflection arena) and emits no commands/obligations.
#[derive(Debug, Clone, Copy)]
struct ReflectHandle<'db> {
    is_struct: bool,
    is_enum: bool,
    target_name: IdentId<'db>,
}

/// The typed result of a read-only *argument-free* reflection read (TD5c). Maps
/// onto a `Value` without the read site knowing which property produced it. Each
/// reflection handle (`ReflectHandle`/`FieldHandle`/`VariantHandle`) owns a
/// `scalar_read(name)` table over this type.
#[derive(Debug, Clone, Copy)]
enum ScalarRead<'db> {
    Bool(bool),
    /// A reflected identifier, rendered into a compile-time string.
    Name(IdentId<'db>),
    /// A pre-rendered compile-time string (e.g. a positional field's index).
    Str(StringId<'db>),
    /// A reflected type witness.
    Ty(TypeId<'db>),
}

impl<'db> ScalarRead<'db> {
    /// Lifts a resolved scalar read into a provider-body `Value`. This is the
    /// only place the read result becomes a `Value`; the read sites do not know
    /// which property produced it.
    fn into_value(self, db: &'db dyn HirDb) -> Value<'db> {
        match self {
            ScalarRead::Bool(value) => Value::Bool(value),
            ScalarRead::Name(name) => Value::Str(StringId::new(db, name.data(db).clone())),
            ScalarRead::Str(value) => Value::Str(value),
            ScalarRead::Ty(ty) => Value::Ty(ty),
        }
    }
}

/// A typed read-only handle over a reflected field (TD5c). It is the value
/// `Value::Field` carries.
///
/// Like [`ReflectHandle`], it takes the `field.*` scalar reads (`ty`/`name`)
/// OFF the bespoke executor: it copies the field's `ty` and rendered `name` at
/// construction and owns its own scalar-read property table ([`Self::scalar_read`]).
/// The executor consults the table by name and no longer knows `ty`/`name`. The
/// `FieldKey` identity ([`Self::key`]) is preserved unchanged — it still flows
/// into generated HIR (`field_get`/`with_field`/`variant_binder`), provenance
/// (`require_field_origin`), and identity comparisons (`same_field`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FieldHandle<'db> {
    key: FieldKey,
    ty: TypeId<'db>,
    name: StringId<'db>,
}

impl<'db> FieldHandle<'db> {
    /// Builds a read-only handle for the field at `key`, copying its scalar
    /// facts (`ty`, rendered `name`) out of the reflection arena. Returns
    /// `None` if `key` names no field (a malformed handle), exactly as the old
    /// `self.reflection.field(..)` lookup did.
    fn new(db: &'db dyn HirDb, reflection: &TargetReflection<'db>, key: FieldKey) -> Option<Self> {
        let reflected = reflection.field(key.variant, key.index)?;
        let name = match reflected.name {
            super::provider::FieldName::Named(name) => name.data(db).clone(),
            super::provider::FieldName::Positional(idx) => idx.to_string(),
        };
        Some(FieldHandle {
            key,
            ty: reflected.ty,
            name: StringId::new(db, name),
        })
    }

    /// This field's `FieldKey` identity — the part of the handle that flows into
    /// generated HIR, provenance, and identity comparisons (NOT a scalar read).
    fn key(&self) -> FieldKey {
        self.key
    }

    /// This handle's read-only scalar property vocabulary (`ty`/`name`), as a
    /// data table. The vocabulary lives HERE, not as arms in the executor's
    /// dispatch `match` (TD5c).
    fn scalar_reads(&self) -> [(&'static str, ScalarRead<'db>); 2] {
        [
            ("ty", ScalarRead::Ty(self.ty)),
            ("name", ScalarRead::Str(self.name)),
        ]
    }

    fn scalar_read(&self, name: &str) -> Option<ScalarRead<'db>> {
        self.scalar_reads()
            .into_iter()
            .find(|(prop, _)| *prop == name)
            .map(|(_, value)| value)
    }
}

/// A typed read-only handle over a reflected variant (TD5c). It is the value
/// `Value::Variant` carries.
///
/// It takes the `variant.*` reads (`is_default` scalar; `precedes` binary) OFF
/// the bespoke executor: it copies `is_default` at construction and owns its own
/// read vocabulary (`scalar_read` for `is_default`, `precedes` for the
/// declaration-order compare). The executor consults the handle by name and no
/// longer knows those names. The declaration-order index identity
/// ([`Self::index`]) is preserved unchanged — it still flows into generated HIR
/// (`variant_init`/`variant_pat`/`variant_binder`) and is the basis of the
/// `precedes` compare.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct VariantHandle {
    index: usize,
    is_default: bool,
}

impl VariantHandle {
    /// Builds a read-only handle for the variant at `index`, copying its
    /// `is_default` fact. Returns `None` if `index` names no variant, exactly as
    /// the old `self.reflection.variant(..)` lookup did.
    fn new(reflection: &TargetReflection<'_>, index: usize) -> Option<Self> {
        let reflected = reflection.variant(index)?;
        Some(VariantHandle {
            index,
            is_default: reflected.is_default,
        })
    }

    /// This variant's declaration-order index identity — the part of the handle
    /// that flows into generated HIR (NOT a scalar read).
    fn index(&self) -> usize {
        self.index
    }

    /// This handle's read-only scalar vocabulary (`is_default`), as a data
    /// table (TD5c).
    fn scalar_reads<'db>(&self) -> [(&'static str, ScalarRead<'db>); 1] {
        [("is_default", ScalarRead::Bool(self.is_default))]
    }

    fn scalar_read<'db>(&self, name: &str) -> Option<ScalarRead<'db>> {
        self.scalar_reads()
            .into_iter()
            .find(|(prop, _)| *prop == name)
            .map(|(_, value)| value)
    }

    /// This handle's read-only *binary* vocabulary: argument-taking reads that
    /// compare this handle against a second operand. `precedes(other)` is a
    /// declaration-order compare — a variant handle is its index into the
    /// target's variant list (declaration order), so `self < other` means
    /// "declared earlier". The vocabulary (the name and the expected operand
    /// kind) lives HERE; the executor evaluates the operand and applies the
    /// resolved comparator without knowing the name (TD5c).
    fn binary_read<'db>(&self, name: &str) -> Option<BinaryRead<'db>> {
        match name {
            "precedes" => Some(BinaryRead {
                operand: CompareOperand::Variant,
                apply: BinaryCompare::Precedes(self.index),
            }),
            _ => None,
        }
    }
}

/// The expected operand kind of a binary read/compare (TD5c). The executor uses
/// it to coerce the already-evaluated second operand to the right `Value` shape
/// before applying the comparator — without knowing the comparator's name.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CompareOperand {
    Ty,
    Field,
    Variant,
}

/// A resolved binary identity/order comparison (TD5c). Carries the first
/// operand's already-extracted identity plus the operation; the executor coerces
/// the second operand to a matching identity and applies this.
#[derive(Debug, Clone, Copy)]
enum BinaryCompare<'db> {
    /// `self_index < other_index` (variant declaration-order precedence).
    Precedes(usize),
    /// `self_ty == other_ty` (syntactic type identity).
    SameTy(TypeId<'db>),
    /// `self_field == other_field` (`FieldKey` identity).
    SameField(FieldKey),
}

/// A binary read resolved by a handle's/vocabulary's name table (TD5c): the
/// operand kind to coerce the second argument to, plus the comparator to apply
/// once both operands' identities are in hand.
#[derive(Debug, Clone, Copy)]
struct BinaryRead<'db> {
    operand: CompareOperand,
    apply: BinaryCompare<'db>,
}

/// The read-only *identity-comparison* vocabulary spelled as `builder.*` methods
/// (`same_ty`/`same_field`) but, per the TD5.0 inventory, pure reflection-style
/// reads mis-shelved by spelling. TD5c moves their vocabulary OFF the bespoke
/// `eval_builder_method` arms onto this typed read-only table: `eval_builder_method`
/// consults it by name (after the genuine `builder.*` build ops) and applies the
/// resolved comparator without knowing `same_ty`/`same_field`. The comparisons
/// are pure (no reflection state) — they compare two already-evaluated operands
/// — so the vocabulary is a free-standing table, not tied to a receiver handle.
struct ReflectionCompare;

impl ReflectionCompare {
    /// Resolves a `builder.*`-spelled identity compare by name. The first
    /// operand is supplied as an already-evaluated `Value` (the executor cannot
    /// pre-extract it the way a receiver-bound handle does); `None` for any name
    /// or first-operand kind this vocabulary does not handle, which the executor
    /// maps to "unsupported" exactly as an unrecognized `builder.*` op would.
    fn binary_read<'db>(name: &str, lhs: &Value<'db>) -> Option<BinaryRead<'db>> {
        match (name, lhs) {
            ("same_ty", Value::Ty(ty)) => Some(BinaryRead {
                operand: CompareOperand::Ty,
                apply: BinaryCompare::SameTy(*ty),
            }),
            ("same_field", Value::Field(field)) => Some(BinaryRead {
                operand: CompareOperand::Field,
                apply: BinaryCompare::SameField(field.key()),
            }),
            _ => None,
        }
    }

    /// Whether `name` is one of this vocabulary's compare ops (regardless of
    /// operand kinds). Lets the executor attribute a wrong-first-operand error
    /// to the operand's span (as the old bespoke arms did) rather than the call
    /// expression, while a genuinely unknown name falls through to the normal
    /// unrecognized-`builder.*` path.
    fn is_compare_name(name: &str) -> bool {
        matches!(name, "same_ty" | "same_field")
    }
}

impl<'db> BinaryRead<'db> {
    /// Applies this binary read against an already-evaluated second operand,
    /// coercing it to the expected kind. `Ok(Some(bool))` on success;
    /// `Ok(None)` if the operand is the wrong kind (the executor maps that to
    /// "unsupported", exactly as a mismatched bespoke arm would).
    fn apply(self, rhs: &Value<'db>) -> Option<bool> {
        match (self.operand, self.apply, rhs) {
            (CompareOperand::Variant, BinaryCompare::Precedes(lhs_index), Value::Variant(other)) => {
                Some(lhs_index < other.index())
            }
            (CompareOperand::Ty, BinaryCompare::SameTy(lhs_ty), Value::Ty(rhs_ty)) => {
                Some(lhs_ty == *rhs_ty)
            }
            (CompareOperand::Field, BinaryCompare::SameField(lhs_field), Value::Field(rhs)) => {
                Some(lhs_field == rhs.key())
            }
            _ => None,
        }
    }
}

impl<'db> ReflectHandle<'db> {
    fn new(reflection: &TargetReflection<'db>, target_name: IdentId<'db>) -> Self {
        ReflectHandle {
            is_struct: reflection.is_struct(),
            is_enum: reflection.is_enum(),
            target_name,
        }
    }

    /// This handle's read-only scalar property vocabulary: the named,
    /// argument-free reads it answers, as a *data table* of `(name, value)`.
    /// The vocabulary lives HERE, on the handle — not as arms in the
    /// executor's dispatch `match`. The executor consults the table by name
    /// ([`Self::scalar_read`]); it does not know `is_struct`/`is_enum`/
    /// `target_name` (TD5c).
    fn scalar_reads(&self) -> [(&'static str, ScalarRead<'db>); 3] {
        [
            ("is_struct", ScalarRead::Bool(self.is_struct)),
            ("is_enum", ScalarRead::Bool(self.is_enum)),
            ("target_name", ScalarRead::Name(self.target_name)),
        ]
    }

    /// Resolves an argument-free scalar read by name against this handle's
    /// property table. `None` for any name the handle does not expose — the
    /// executor maps that to "unsupported", exactly as an unknown method would
    /// have.
    fn scalar_read(&self, name: &str) -> Option<ScalarRead<'db>> {
        self.scalar_reads()
            .into_iter()
            .find(|(prop, _)| *prop == name)
            .map(|(_, value)| value)
    }
}

enum Flow {
    Continue,
    Return,
}

/// Where a goal-trait-declared type sits in the inferred signature (DEVX-A).
/// Self/target types are spelled differently per position to match the dance
/// byte-for-byte: `target_ty()` for arguments, `self_ty()` (`Self`) for the
/// return type.
#[derive(Debug, Clone, Copy)]
enum SigPosition {
    Arg,
    Return,
}

pub(super) struct ProviderExecutor<'a, 'db> {
    db: &'db dyn HirDb,
    body: Body<'db>,
    reflection: &'a TargetReflection<'db>,
    /// The impl self type with generic args applied (`Pair<A, B>`), exposed
    /// as `builder.target_ty()`.
    target_ty: TypeId<'db>,
    // TD5c: the target's bare name (exposed as `reflect.target_name()`) is no
    // longer ambient executor state — it lives on the typed `ReflectHandle`
    // carried by `Value::Reflect`, which owns that read.
    /// The provider's module, for canonicalizing `require<Trait>` paths.
    provider_top_mod: crate::hir_def::TopLevelMod<'db>,
    /// The canonical path of the provider's goal trait (`core::ops::Eq`). The
    /// 2-arg `emit_method(name, body)` form infers a method's signature from
    /// this trait's declaration of `name` (DEVX-A).
    goal_trait_path: PathId<'db>,
    /// Lexically scoped value bindings; the innermost binding of a name
    /// shadows outer ones (including the capability params).
    scopes: Vec<Vec<(IdentId<'db>, Value<'db>)>>,

    exprs: Vec<GenExpr<'db>>,
    pats: Vec<GenPat<'db>>,
    tys: Vec<GenTy<'db>>,
    sigs: Vec<GenMethodSig<'db>>,
    quotes: Vec<QuoteTemplate<'db>>,
    /// The typed effect trace (TD5.1): everything a provider body does that
    /// re-enters ordinary compilation (requirements + emitted members),
    /// recorded in execution order. The sole replay authority for synthesis.
    effects: Vec<ProviderEffect<'db>>,
    emitted_methods: Vec<IdentId<'db>>,
    emitted_assocs: Vec<IdentId<'db>>,
    finished: bool,

    steps: usize,
    /// Lazily resolved syntax root of the provider's file, for error spans.
    root: Option<parser::SyntaxNode>,
    fallback_range: TextRange,
}

impl<'a, 'db> ProviderExecutor<'a, 'db> {
    pub(super) fn run(
        db: &'db dyn HirDb,
        provider: &ValidatedProvider<'db>,
        reflection: &'a TargetReflection<'db>,
        target_ty: TypeId<'db>,
        target_name: IdentId<'db>,
    ) -> Result<ProviderOutput<'db>, ExecError> {
        let mut initial_scope = Vec::new();
        for &name in &provider.param_names {
            initial_scope.push((name, Value::Evidence));
        }
        for &capability in &provider.capabilities {
            let value = match capability {
                super::provider::Capability::Reflect(_) => {
                    Value::Reflect(ReflectHandle::new(reflection, target_name))
                }
                super::provider::Capability::ImplBuilder(_) => Value::Builder,
            };
            initial_scope.push((capability.binding(), value));
        }

        let fallback_range = super::provider::impl_provider_name_range(db, provider.provider);
        let mut executor = ProviderExecutor {
            db,
            body: provider.body,
            reflection,
            target_ty,
            provider_top_mod: provider.provider.top_mod(db),
            goal_trait_path: provider.trait_path,
            scopes: vec![initial_scope],
            exprs: Vec::new(),
            pats: Vec::new(),
            tys: Vec::new(),
            sigs: Vec::new(),
            quotes: Vec::new(),
            effects: Vec::new(),
            emitted_methods: Vec::new(),
            emitted_assocs: Vec::new(),
            finished: false,
            steps: 0,
            root: None,
            fallback_range,
        };

        let root_expr = executor.body.expr(db);
        executor.execute_expr(root_expr)?;
        if !executor.finished {
            return Err(ExecError {
                kind: ProviderFailureKind::MissingFinish,
                range: executor.expr_range(root_expr),
            });
        }
        Ok(ProviderOutput {
            skeleton: ProviderSkeleton {
                effects: executor.effects,
                sigs: executor.sigs,
                tys: executor.tys,
            },
            bodies: ProviderBodies {
                exprs: executor.exprs,
                pats: executor.pats,
            },
        })
    }

    // --- spans ----------------------------------------------------------

    fn syntax_root(&mut self) -> parser::SyntaxNode {
        if self.root.is_none() {
            self.root = Some(
                top_mod_ast(self.db, self.body.top_mod(self.db))
                    .syntax()
                    .clone(),
            );
        }
        self.root.clone().unwrap()
    }

    fn expr_range(&mut self, expr: ExprId) -> TextRange {
        let origin = self
            .body
            .source_map(self.db)
            .expr_map
            .node_to_source(expr)
            .clone();
        self.origin_range(origin)
    }

    fn stmt_range(&mut self, stmt: StmtId) -> TextRange {
        let origin = self
            .body
            .source_map(self.db)
            .stmt_map
            .node_to_source(stmt)
            .clone();
        self.origin_range(origin)
    }

    fn origin_range<T>(&mut self, origin: HirOrigin<T>) -> TextRange
    where
        T: parser::ast::prelude::AstNode<Language = parser::FeLang> + Clone + std::hash::Hash + Eq,
    {
        match origin {
            HirOrigin::Raw(ptr) => {
                let root = self.syntax_root();
                ptr.syntax_node_ptr()
                    .try_to_node(&root)
                    .map(|node| node.text_range())
                    .unwrap_or(self.fallback_range)
            }
            _ => self.fallback_range,
        }
    }

    fn unsupported_expr(&mut self, expr: ExprId) -> ExecError {
        ExecError {
            kind: ProviderFailureKind::UnsupportedBody,
            range: self.expr_range(expr),
        }
    }

    fn unsupported_stmt(&mut self, stmt: StmtId) -> ExecError {
        ExecError {
            kind: ProviderFailureKind::UnsupportedBody,
            range: self.stmt_range(stmt),
        }
    }

    // --- budget ---------------------------------------------------------

    fn tick(&mut self, range: TextRange) -> Result<(), ExecError> {
        self.steps += 1;
        // `effects` is the sole replay trace: TD5.2 moved `require` and TD5.x
        // moved the emit families out of bespoke `commands` (deleted) into the
        // effect trace, so the command-count cap is measured over `effects`.
        if self.steps > STEP_BUDGET
            || self.effects.len() + self.exprs.len() > COMMAND_BUDGET
        {
            return Err(ExecError {
                kind: ProviderFailureKind::BudgetExceeded,
                range,
            });
        }
        Ok(())
    }

    // --- environment ----------------------------------------------------

    fn bind(&mut self, name: IdentId<'db>, value: Value<'db>) {
        self.scopes
            .last_mut()
            .expect("executor scope stack is never empty")
            .push((name, value));
    }

    fn lookup(&self, name: IdentId<'db>) -> Option<Value<'db>> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev())
            .find_map(|(bound, value)| (*bound == name).then(|| value.clone()))
    }

    fn assign(&mut self, name: IdentId<'db>, value: Value<'db>) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            if let Some((_, slot)) = scope.iter_mut().rev().find(|(bound, _)| *bound == name) {
                *slot = value;
                return true;
            }
        }
        false
    }

    // --- execution ------------------------------------------------------

    fn execute_stmt(&mut self, stmt: StmtId) -> Result<Flow, ExecError> {
        let range = self.stmt_range(stmt);
        self.tick(range)?;
        let Partial::Present(stmt_data) = stmt.data(self.db, self.body) else {
            return Ok(Flow::Continue);
        };
        match stmt_data {
            Stmt::Let(pat, _ty, init) => {
                let Some(init) = init else {
                    return Err(self.unsupported_stmt(stmt));
                };
                let value = self.eval_expr(*init)?;
                let Some(name) = self.simple_pat_binding(*pat) else {
                    return Err(self.unsupported_stmt(stmt));
                };
                self.bind(name, value);
                Ok(Flow::Continue)
            }
            Stmt::For(pat, iterable, loop_body, _unroll) => {
                let Some(binding) = self.simple_pat_binding(*pat) else {
                    return Err(self.unsupported_stmt(stmt));
                };
                // TD5c: the iterable is an ORDINARY expression now. It evaluates
                // to a `Value::Seq` of typed read-only handles (built eagerly at
                // the `reflect.fields()`/`reflect.variants()`/`variant.fields()`
                // method-call site); the executor no longer special-cases the
                // iterable expression. Any non-sequence iterable is unsupported.
                let Value::Seq(items) = self.eval_expr(*iterable)? else {
                    return Err(self.unsupported_expr(*iterable));
                };
                for item in items {
                    self.scopes.push(vec![(binding, item)]);
                    let flow = self.execute_expr(*loop_body);
                    self.scopes.pop();
                    if matches!(flow?, Flow::Return) {
                        return Ok(Flow::Return);
                    }
                }
                Ok(Flow::Continue)
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.eval_expr(*expr)?;
                }
                Ok(Flow::Return)
            }
            Stmt::Expr(expr) => self.execute_expr(*expr),
            Stmt::While(..) | Stmt::Continue | Stmt::Break => Err(self.unsupported_stmt(stmt)),
        }
    }

    /// Executes `expr` for effect, threading control flow. Used for block
    /// and `if` bodies; values are discarded.
    fn execute_expr(&mut self, expr: ExprId) -> Result<Flow, ExecError> {
        let range = self.expr_range(expr);
        self.tick(range)?;
        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            return Ok(Flow::Continue);
        };
        match expr_data {
            Expr::Block(stmts) => {
                self.scopes.push(Vec::new());
                let mut flow = Flow::Continue;
                let mut error = None;
                for &stmt in stmts {
                    match self.execute_stmt(stmt) {
                        Ok(Flow::Continue) => {}
                        Ok(Flow::Return) => {
                            flow = Flow::Return;
                            break;
                        }
                        Err(err) => {
                            error = Some(err);
                            break;
                        }
                    }
                }
                self.scopes.pop();
                match error {
                    Some(err) => Err(err),
                    None => Ok(flow),
                }
            }
            Expr::If(cond, then_expr, else_expr) => {
                if self.eval_cond(*cond)? {
                    self.execute_expr(*then_expr)
                } else if let Some(else_expr) = else_expr {
                    self.execute_expr(*else_expr)
                } else {
                    Ok(Flow::Continue)
                }
            }
            Expr::Assign(lhs, rhs) => {
                let value = self.eval_expr(*rhs)?;
                let Some(name) = self.simple_expr_path_ident(*lhs) else {
                    return Err(self.unsupported_expr(*lhs));
                };
                if !self.assign(name, value) {
                    return Err(self.unsupported_expr(*lhs));
                }
                Ok(Flow::Continue)
            }
            _ => {
                self.eval_expr(expr)?;
                Ok(Flow::Continue)
            }
        }
    }

    fn eval_cond(&mut self, cond: CondId) -> Result<bool, ExecError> {
        let Partial::Present(cond_data) = cond.data(self.db, self.body) else {
            return Err(ExecError {
                kind: ProviderFailureKind::UnsupportedBody,
                range: self.fallback_range,
            });
        };
        match cond_data {
            Cond::Expr(expr) => match self.eval_expr(*expr)? {
                Value::Bool(value) => Ok(value),
                _ => Err(self.unsupported_expr(*expr)),
            },
            Cond::Bin(lhs, rhs, LogicalBinOp::And) => {
                Ok(self.eval_cond(*lhs)? && self.eval_cond(*rhs)?)
            }
            Cond::Bin(lhs, rhs, LogicalBinOp::Or) => {
                Ok(self.eval_cond(*lhs)? || self.eval_cond(*rhs)?)
            }
            Cond::Let(..) => Err(ExecError {
                kind: ProviderFailureKind::UnsupportedBody,
                range: self.fallback_range,
            }),
        }
    }

    fn eval_expr(&mut self, expr: ExprId) -> Result<Value<'db>, ExecError> {
        let range = self.expr_range(expr);
        self.tick(range)?;
        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            return Err(self.unsupported_expr(expr));
        };
        match expr_data {
            Expr::Lit(LitKind::Bool(value)) => Ok(Value::Bool(*value)),
            Expr::Lit(LitKind::String(value)) => Ok(Value::Str(*value)),
            Expr::Path(_) => {
                let Some(name) = self.simple_expr_path_ident(expr) else {
                    return Err(self.unsupported_expr(expr));
                };
                self.lookup(name).ok_or_else(|| self.unsupported_expr(expr))
            }
            Expr::MethodCall(receiver, method, generic_args, args) => {
                self.eval_method_call(expr, *receiver, *method, *generic_args, args.clone())
            }
            Expr::Un(inner, crate::hir_def::UnOp::Not) => match self.eval_expr(*inner)? {
                Value::Bool(value) => Ok(Value::Bool(!value)),
                _ => Err(self.unsupported_expr(expr)),
            },
            Expr::Quote { open, body } => {
                let open = open.clone();
                let body = body.clone();
                let mut holes = Vec::new();
                match &body {
                    QuoteBody::Expr(root) => self.capture_quote_holes(*root, &mut holes)?,
                    QuoteBody::Arms(arms) => {
                        let arms = arms.clone();
                        self.capture_arm_holes(&arms, &mut holes)?;
                    }
                }
                self.quotes.push(QuoteTemplate {
                    origin: expr,
                    open,
                    body,
                    holes,
                });
                Ok(Value::Quote(QuoteId(self.quotes.len() - 1)))
            }
            Expr::QuoteHole(..) | Expr::QuoteFieldHole(..) => Err(self.invalid_quote(
                expr,
                "`${...}` splice holes are only meaningful inside a `quote` body",
            )),
            _ => Err(self.unsupported_expr(expr)),
        }
    }

    /// Walks a quote template at quote-construction time, evaluating every
    /// hole's inner expression in the current environment and recording the
    /// captured values. Only the v1 template vocabulary is walked; constructs
    /// outside it cannot carry live holes because elaboration rejects them
    /// before any hole beneath them is reached.
    fn capture_quote_holes(
        &mut self,
        expr: ExprId,
        holes: &mut Vec<(ExprId, Value<'db>)>,
    ) -> Result<(), ExecError> {
        let range = self.expr_range(expr);
        self.tick(range)?;
        let Partial::Present(data) = expr.data(self.db, self.body) else {
            return Ok(());
        };
        match data {
            Expr::QuoteHole(inner) => {
                let value = self.eval_expr(*inner)?;
                holes.push((expr, value));
                Ok(())
            }
            Expr::QuoteFieldHole(base, inner) => {
                let (base, inner) = (*base, *inner);
                self.capture_quote_holes(base, holes)?;
                let value = self.eval_expr(inner)?;
                holes.push((expr, value));
                Ok(())
            }
            Expr::Quote { .. } => Err(self.invalid_quote(
                expr,
                "`quote` inside a quote body is not supported; build the inner \
                 fragment in a separate `let` and splice it with `${...}`",
            )),
            Expr::Block(stmts) => {
                let stmts = stmts.clone();
                for stmt in stmts {
                    if let Partial::Present(Stmt::Expr(stmt_expr)) = stmt.data(self.db, self.body) {
                        self.capture_quote_holes(*stmt_expr, holes)?;
                    }
                }
                Ok(())
            }
            Expr::Bin(lhs, rhs, _) => {
                let (lhs, rhs) = (*lhs, *rhs);
                self.capture_quote_holes(lhs, holes)?;
                self.capture_quote_holes(rhs, holes)
            }
            Expr::MethodCall(receiver, _, _, args) => {
                let receiver = *receiver;
                let args: Vec<ExprId> = args.iter().map(|arg| arg.expr).collect();
                self.capture_quote_holes(receiver, holes)?;
                for arg in args {
                    self.capture_quote_holes(arg, holes)?;
                }
                Ok(())
            }
            Expr::Match(scrutinee, arms) => {
                let scrutinee = *scrutinee;
                let arms = match arms {
                    Partial::Present(arms) => arms.clone(),
                    Partial::Absent => Vec::new(),
                };
                self.capture_quote_holes(scrutinee, holes)?;
                self.capture_arm_holes(&arms, holes)
            }
            // Leaves carry no holes; the remaining constructs are outside
            // the template vocabulary and are rejected at elaboration.
            Expr::Lit(..)
            | Expr::Path(..)
            | Expr::Un(..)
            | Expr::Cast(..)
            | Expr::Call(..)
            | Expr::Assert(..)
            | Expr::RecordInit(..)
            | Expr::Field(..)
            | Expr::Tuple(..)
            | Expr::Array(..)
            | Expr::ArrayRep(..)
            | Expr::If(..)
            | Expr::Assign(..)
            | Expr::AugAssign(..)
            | Expr::With(..) => Ok(()),
        }
    }

    /// Captures hole values from a match-arm sequence: pattern holes in arm
    /// patterns (keyed by the hole's inner expression) and expression holes
    /// in arm bodies. Arm splices (`${arms}` standing alone) have an absent
    /// pattern and their hole as the arm body, so the body walk covers them.
    fn capture_arm_holes(
        &mut self,
        arms: &[MatchArm],
        holes: &mut Vec<(ExprId, Value<'db>)>,
    ) -> Result<(), ExecError> {
        for arm in arms {
            self.capture_pat_holes(arm.pat, holes)?;
            self.capture_quote_holes(arm.body, holes)?;
        }
        Ok(())
    }

    fn capture_pat_holes(
        &mut self,
        pat: PatId,
        holes: &mut Vec<(ExprId, Value<'db>)>,
    ) -> Result<(), ExecError> {
        let Partial::Present(data) = pat.data(self.db, self.body) else {
            return Ok(());
        };
        match data {
            Pat::QuoteHole(inner, _binders) => {
                let inner = *inner;
                let value = self.eval_expr(inner)?;
                holes.push((inner, value));
                Ok(())
            }
            // Other patterns carry no live holes; the ones outside the
            // template vocabulary are rejected at elaboration.
            Pat::WildCard
            | Pat::Rest
            | Pat::Lit(..)
            | Pat::Tuple(..)
            | Pat::Path(..)
            | Pat::PathTuple(..)
            | Pat::Record(..)
            | Pat::Or(..) => Ok(()),
        }
    }

    /// Validates a template's declared open names against the destination:
    /// the emitted method's parameter names, plus the binder groups of the
    /// match arms enclosing the splice point.
    fn validate_open_names(
        &mut self,
        template: &QuoteTemplate<'db>,
        sig: SigId,
        binders: &[BinderGroup<'db>],
    ) -> Result<(), ExecError> {
        let sig_data = &self.sigs[sig.0];
        let method_name = sig_data.name;
        let takes_self = sig_data.takes_self;
        let params: Vec<IdentId<'db>> = sig_data.args.iter().map(|(name, _)| *name).collect();
        for open in &template.open {
            if params.contains(open) || binders.iter().any(|(group, _)| group == open) {
                continue;
            }
            let mut binds = Vec::new();
            if takes_self {
                binds.push("`self`".to_string());
            }
            binds.extend(
                params
                    .iter()
                    .map(|param| format!("`{}`", param.data(self.db))),
            );
            let binds = if binds.is_empty() {
                "no names".to_string()
            } else {
                binds.join(", ")
            };
            let detail = if binders.is_empty() {
                format!(
                    "the quote declares open name `{}`, but the emitted method `{}` binds {}; \
                     open names bind against the destination method's parameter names",
                    open.data(self.db),
                    method_name.data(self.db),
                    binds,
                )
            } else {
                let groups = binders
                    .iter()
                    .map(|(group, _)| format!("`{}`", group.data(self.db)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "the quote declares open name `{}`, but the emitted method `{}` binds {} \
                     and the enclosing match arms bind {}; open names bind against the \
                     destination method's parameters or enclosing arm binder groups",
                    open.data(self.db),
                    method_name.data(self.db),
                    binds,
                    groups,
                )
            };
            return Err(self.invalid_quote(template.origin, &detail));
        }
        Ok(())
    }

    /// Elaborates a quote template into a generated expression for emission
    /// under `sig`. Open names (the quote's own and those of every spliced
    /// quote) must match the destination signature's parameter names or the
    /// binder groups of enclosing match arms.
    fn elaborate_quote(
        &mut self,
        quote: QuoteId,
        sig: SigId,
        binders: &[BinderGroup<'db>],
    ) -> Result<GenExprId, ExecError> {
        let template = self.quotes[quote.0].clone();
        self.validate_open_names(&template, sig, binders)?;

        // Expression quotes: the body block must hold exactly one
        // expression.
        let block = match &template.body {
            QuoteBody::Expr(block) => *block,
            QuoteBody::Arms(_) => {
                return Err(self.invalid_quote(
                    template.origin,
                    "the quote holds match arms; expression positions need an expression \
                     template (wrap the arms in a `match`)",
                ));
            }
        };
        let Partial::Present(Expr::Block(stmts)) = block.data(self.db, self.body) else {
            return Err(self.invalid_quote(template.origin, "malformed quote body"));
        };
        let root = match stmts.as_slice() {
            [stmt] => match stmt.data(self.db, self.body) {
                Partial::Present(Stmt::Expr(root)) => *root,
                _ => {
                    return Err(self.invalid_quote(
                        template.origin,
                        "quote bodies must be a single expression",
                    ));
                }
            },
            [] => {
                return Err(self.invalid_quote(
                    template.origin,
                    "the quote is empty; an empty quote only splices in match-arm position",
                ));
            }
            _ => {
                return Err(
                    self.invalid_quote(template.origin, "quote bodies must be a single expression")
                );
            }
        };
        self.elab_template_expr(root, &template, sig, binders)
    }

    /// Elaborates a quote spliced in match-arm position, appending its arms
    /// to `out`. The spliced quote must be a match-arm template
    /// (`quote { pat => expr, .. }`) or the empty quote (`quote { }`).
    fn elaborate_quote_arms(
        &mut self,
        quote: QuoteId,
        sig: SigId,
        binders: &[BinderGroup<'db>],
        out: &mut Vec<(GenPatId, GenExprId)>,
    ) -> Result<(), ExecError> {
        let template = self.quotes[quote.0].clone();
        self.validate_open_names(&template, sig, binders)?;
        match &template.body {
            QuoteBody::Arms(arms) => {
                let arms = arms.clone();
                self.elab_arm_items(&arms, &template, sig, binders, out)
            }
            QuoteBody::Expr(block) => {
                // The empty quote is the empty arm sequence — the natural
                // seed for arm folds.
                if let Partial::Present(Expr::Block(stmts)) = block.data(self.db, self.body)
                    && stmts.is_empty()
                {
                    return Ok(());
                }
                Err(self.invalid_quote(
                    template.origin,
                    "the quote holds an expression template; arm splices need match arms \
                     (`pat => expr` items) or an empty `quote { }`",
                ))
            }
        }
    }

    /// Elaborates a match-arm sequence (the arms of a template `match` or
    /// the items of a match-arm template) into generated (pattern, body)
    /// pairs.
    fn elab_arm_items(
        &mut self,
        arms: &[MatchArm],
        template: &QuoteTemplate<'db>,
        sig: SigId,
        binders: &[BinderGroup<'db>],
        out: &mut Vec<(GenPatId, GenExprId)>,
    ) -> Result<(), ExecError> {
        for arm in arms {
            let range = self.expr_range(arm.body);
            self.tick(range)?;
            match arm.pat.data(self.db, self.body) {
                // An arm splice: `${arms}` standing alone has no pattern
                // and carries its hole as the arm body.
                Partial::Absent => {
                    let Partial::Present(Expr::QuoteHole(_)) = arm.body.data(self.db, self.body)
                    else {
                        return Err(
                            self.invalid_quote(arm.body, "malformed match arm in quote body")
                        );
                    };
                    let value = self.quote_hole_value(arm.body, template)?;
                    let Value::Quote(inner) = value else {
                        let detail = format!(
                            "a {} cannot fill an arm splice; arm splices accept quote values \
                             holding match arms",
                            value_kind_name(&value),
                        );
                        return Err(self.invalid_quote(arm.body, &detail));
                    };
                    self.elaborate_quote_arms(inner, sig, binders, out)?;
                }
                Partial::Present(Pat::WildCard) => {
                    let body = self.elab_template_expr(arm.body, template, sig, binders)?;
                    let pat = self.push_gen_pat(GenPat::Wildcard);
                    out.push((pat, body));
                }
                Partial::Present(Pat::QuoteHole(inner, groups)) => {
                    let (inner, groups) = (*inner, groups.clone());
                    let value = self.quote_hole_value(inner, template)?;
                    let Value::Variant(variant) = value else {
                        let detail = format!(
                            "a {} cannot fill a pattern hole; pattern holes accept `Variant` \
                             handles",
                            value_kind_name(&value),
                        );
                        return Err(self.invalid_quote(inner, &detail));
                    };
                    let variant = variant.index();
                    let group = self.binder_group_name(inner, &groups)?;
                    let pat = self.push_gen_pat(GenPat::Variant {
                        variant,
                        prefix: group,
                    });
                    let mut arm_binders = binders.to_vec();
                    arm_binders.push((group, variant));
                    let body = self.elab_template_expr(arm.body, template, sig, &arm_binders)?;
                    out.push((pat, body));
                }
                Partial::Present(_) => {
                    return Err(self.invalid_quote(
                        arm.body,
                        "this pattern is not supported in quote match arms (arms take \
                         `${variant}(group)` pattern holes and `_`)",
                    ));
                }
            }
        }
        Ok(())
    }

    /// The binder-group name of a `${variant}(group)` pattern hole.
    fn binder_group_name(
        &mut self,
        hole_expr: ExprId,
        groups: &[PatId],
    ) -> Result<IdentId<'db>, ExecError> {
        let group = match groups {
            [group] => self.simple_pat_binding(*group),
            _ => None,
        };
        let Some(name) = group else {
            return Err(self.invalid_quote(
                hole_expr,
                "variant pattern holes bind their payload under a single group name: \
                 `${variant}(group)`",
            ));
        };
        if name.is_self(self.db) {
            return Err(self.invalid_quote(hole_expr, "a binder group cannot be named `self`"));
        }
        Ok(name)
    }

    fn elab_template_expr(
        &mut self,
        expr: ExprId,
        template: &QuoteTemplate<'db>,
        sig: SigId,
        binders: &[BinderGroup<'db>],
    ) -> Result<GenExprId, ExecError> {
        let range = self.expr_range(expr);
        self.tick(range)?;
        let Partial::Present(data) = expr.data(self.db, self.body) else {
            return Err(self.invalid_quote(expr, "malformed quote body"));
        };
        match data {
            Expr::Lit(LitKind::Bool(value)) => Ok(self.push_gen(GenExpr::Bool(*value))),
            Expr::Lit(LitKind::String(value)) => {
                let value = *value;
                self.check_inline_capacity(expr, value)?;
                Ok(self.push_gen(GenExpr::StrLit(value)))
            }
            Expr::Lit(LitKind::Int(_)) => {
                Err(self.invalid_quote(expr, "integer literals are not supported in quote bodies"))
            }
            Expr::Bin(lhs, rhs, BinOp::Logical(LogicalBinOp::And)) => {
                let (lhs, rhs) = (*lhs, *rhs);
                let lhs = self.elab_template_expr(lhs, template, sig, binders)?;
                let rhs = self.elab_template_expr(rhs, template, sig, binders)?;
                Ok(self.push_gen(GenExpr::And(lhs, rhs)))
            }
            Expr::Bin(lhs, rhs, BinOp::Logical(LogicalBinOp::Or)) => {
                let (lhs, rhs) = (*lhs, *rhs);
                let lhs = self.elab_template_expr(lhs, template, sig, binders)?;
                let rhs = self.elab_template_expr(rhs, template, sig, binders)?;
                Ok(self.push_gen(GenExpr::Or(lhs, rhs)))
            }
            Expr::Bin(lhs, rhs, BinOp::Comp(CompBinOp::Eq)) => {
                let (lhs, rhs) = (*lhs, *rhs);
                let lhs = self.elab_template_expr(lhs, template, sig, binders)?;
                let rhs = self.elab_template_expr(rhs, template, sig, binders)?;
                Ok(self.push_gen(GenExpr::EqCmp(lhs, rhs)))
            }
            Expr::Bin(lhs, rhs, BinOp::Comp(CompBinOp::Lt)) => {
                let (lhs, rhs) = (*lhs, *rhs);
                let lhs = self.elab_template_expr(lhs, template, sig, binders)?;
                let rhs = self.elab_template_expr(rhs, template, sig, binders)?;
                Ok(self.push_gen(GenExpr::LtCmp(lhs, rhs)))
            }
            Expr::Bin(lhs, rhs, BinOp::Comp(CompBinOp::Gt)) => {
                let (lhs, rhs) = (*lhs, *rhs);
                let lhs = self.elab_template_expr(lhs, template, sig, binders)?;
                let rhs = self.elab_template_expr(rhs, template, sig, binders)?;
                Ok(self.push_gen(GenExpr::GtCmp(lhs, rhs)))
            }
            Expr::Bin(..) => Err(self.invalid_quote(
                expr,
                "this operator is not supported in quote bodies (quotes support `&&`, `||`, \
                 `==`, `<`, `>`, and method calls)",
            )),
            Expr::Path(_) => {
                if let Some(name) = self.simple_expr_path_ident(expr) {
                    return self.elab_template_name(expr, name, template, sig, binders);
                }
                if let Some((ty, trait_, name)) = self.extract_qualified_path(expr) {
                    return self.elab_qualified_const(expr, ty, trait_, name);
                }
                Err(self.invalid_quote(
                    expr,
                    "paths in quote bodies must be a single name or a qualified associated-const \
                     access `<Ty as Trait>::item`",
                ))
            }
            Expr::QuoteHole(_) => {
                let value = self.quote_hole_value(expr, template)?;
                match value {
                    Value::Quote(inner) => self.elaborate_quote(inner, sig, binders),
                    Value::Bool(value) => Ok(self.push_gen(GenExpr::Bool(value))),
                    Value::Str(value) => {
                        self.check_inline_capacity(expr, value)?;
                        Ok(self.push_gen(GenExpr::StrLit(value)))
                    }
                    Value::Field(_) => Err(self.invalid_quote(
                        expr,
                        "a `Field` handle only fills member-access holes (`self.${field}`); \
                         expression holes accept quote values and compile-time bool/string \
                         values",
                    )),
                    other => {
                        let detail = format!(
                            "a {} cannot fill an expression hole; expression holes accept \
                             quote values and compile-time bool/string values",
                            value_kind_name(&other),
                        );
                        Err(self.invalid_quote(expr, &detail))
                    }
                }
            }
            Expr::QuoteFieldHole(base, _) => {
                let base = *base;
                let value = self.quote_hole_value(expr, template)?;
                // `group.${field}` — a payload-binder reference when the
                // base is an open name bound by an enclosing arm pattern.
                if let Some(name) = self.simple_expr_path_ident(base)
                    && !name.is_self(self.db)
                    && let Some(variant) = binders
                        .iter()
                        .rev()
                        .find_map(|(group, variant)| (*group == name).then_some(*variant))
                {
                    if !template.open.contains(&name) {
                        let detail = format!(
                            "`{}` matches a binder group of an enclosing match arm but is \
                             not declared open; declare it with `quote({}) {{ .. }}`",
                            name.data(self.db),
                            name.data(self.db),
                        );
                        return Err(self.invalid_quote(expr, &detail));
                    }
                    let Value::Field(field) = value else {
                        let detail = format!(
                            "member-access holes (`base.${{...}}`) accept `Field` handles, \
                             found a {}",
                            value_kind_name(&value),
                        );
                        return Err(self.invalid_quote(expr, &detail));
                    };
                    let field = field.key();
                    if field.variant != Some(variant) {
                        let detail = format!(
                            "the field does not belong to the variant matched by `{}`",
                            name.data(self.db),
                        );
                        return Err(self.invalid_quote(expr, &detail));
                    }
                    return Ok(self.push_gen(GenExpr::VariantBinder {
                        variant,
                        field: field.index,
                        prefix: name,
                    }));
                }
                let Value::Field(field) = value else {
                    let detail = format!(
                        "member-access holes (`base.${{...}}`) accept `Field` handles, \
                         found a {}",
                        value_kind_name(&value),
                    );
                    return Err(self.invalid_quote(expr, &detail));
                };
                let base = self.elab_template_expr(base, template, sig, binders)?;
                Ok(self.push_gen(GenExpr::FieldGet(base, field.key())))
            }
            Expr::MethodCall(receiver, method, generic_args, args) => {
                let Some(method) = method.to_opt() else {
                    return Err(self.invalid_quote(expr, "malformed method call in quote body"));
                };
                if !generic_args.data(self.db).is_empty() {
                    return Err(self.invalid_quote(
                        expr,
                        "generic method calls are not supported in quote bodies",
                    ));
                }
                let receiver = *receiver;
                let arg_exprs: Vec<ExprId> = args.iter().map(|arg| arg.expr).collect();
                let receiver = self.elab_template_expr(receiver, template, sig, binders)?;
                let mut call_args = Vec::with_capacity(arg_exprs.len());
                for arg in arg_exprs {
                    call_args.push(self.elab_template_expr(arg, template, sig, binders)?);
                }
                Ok(self.push_gen(GenExpr::MethodCall {
                    receiver,
                    method,
                    args: call_args,
                }))
            }
            Expr::Match(scrutinee, arms) => {
                let scrutinee = *scrutinee;
                let arms = match arms {
                    Partial::Present(arms) => arms.clone(),
                    Partial::Absent => {
                        return Err(self.invalid_quote(expr, "malformed quote body"));
                    }
                };
                self.elab_template_match(expr, scrutinee, &arms, template, sig, binders)
            }
            Expr::Field(..) => Err(self.invalid_quote(
                expr,
                "field access in quote bodies goes through a member-access hole \
                 (`self.${field}`)",
            )),
            Expr::Quote { .. } => Err(self.invalid_quote(
                expr,
                "`quote` inside a quote body is not supported; build the inner \
                 fragment in a separate `let` and splice it with `${...}`",
            )),
            Expr::Block(..)
            | Expr::Un(..)
            | Expr::Cast(..)
            | Expr::Call(..)
            | Expr::Assert(..)
            | Expr::RecordInit(..)
            | Expr::Tuple(..)
            | Expr::Array(..)
            | Expr::ArrayRep(..)
            | Expr::If(..)
            | Expr::Assign(..)
            | Expr::AugAssign(..)
            | Expr::With(..) => Err(self.invalid_quote(
                expr,
                "this construct is not supported in quote bodies (quotes support literals, \
                 `&&`, `==`, `self`, declared open names, method calls, `match`, and \
                 `${...}` holes)",
            )),
        }
    }

    /// Elaborates a `match` inside a quote body. Variant arm patterns name
    /// the derive target's variants, so the scrutinee must be the target
    /// value (`self` or a target-typed parameter), and the arms must cover
    /// every reflected variant unless a `_` arm is present.
    fn elab_template_match(
        &mut self,
        expr: ExprId,
        scrutinee: ExprId,
        arms: &[MatchArm],
        template: &QuoteTemplate<'db>,
        sig: SigId,
        binders: &[BinderGroup<'db>],
    ) -> Result<GenExprId, ExecError> {
        let scrutinee_gen = self.elab_template_expr(scrutinee, template, sig, binders)?;
        let is_target_value = match &self.exprs[scrutinee_gen.0] {
            GenExpr::SelfRef => true,
            GenExpr::ArgRef(name) => {
                let name = *name;
                self.sigs[sig.0].args.iter().any(|(arg, ty)| {
                    *arg == name
                        && (*ty == self.target_ty || *ty == TypeId::fallback_self_ty(self.db))
                })
            }
            _ => false,
        };
        if !is_target_value {
            return Err(self.invalid_quote(
                scrutinee,
                "match scrutinees in quote bodies must be `self` or a parameter of the \
                 derive target's type (variant patterns name the target's variants)",
            ));
        }

        let mut gen_arms = Vec::new();
        self.elab_arm_items(arms, template, sig, binders, &mut gen_arms)?;

        // Exhaustiveness over the target's variants, checked at the
        // template so the failure names the provider's match rather than
        // surfacing later from the generated code.
        let has_wildcard = gen_arms
            .iter()
            .any(|(pat, _)| matches!(self.pats[pat.0], GenPat::Wildcard));
        if !has_wildcard {
            let mut uncovered = None;
            for variant in self.reflection.variants() {
                let covered = gen_arms.iter().any(|(pat, _)| {
                    matches!(
                        &self.pats[pat.0],
                        GenPat::Variant { variant: v, .. } if *v == variant.index
                    )
                });
                if !covered {
                    uncovered = Some(variant.name);
                    break;
                }
            }
            if let Some(name) = uncovered {
                let detail = format!(
                    "the template match does not cover variant `{}`; add a \
                     `${{variant}}(group)` arm for it or a `_` arm",
                    name.data(self.db),
                );
                return Err(self.invalid_quote(expr, &detail));
            }
        }

        Ok(self.push_gen(GenExpr::Match {
            scrutinee: scrutinee_gen,
            arms: gen_arms,
        }))
    }

    /// Resolves a bare name in a quote template: `self`, or a declared open
    /// name bound at the destination — by the innermost enclosing arm's
    /// binder group of that name, or by the emitted method's parameters.
    fn elab_template_name(
        &mut self,
        expr: ExprId,
        name: IdentId<'db>,
        template: &QuoteTemplate<'db>,
        sig: SigId,
        binders: &[BinderGroup<'db>],
    ) -> Result<GenExprId, ExecError> {
        let sig_data = &self.sigs[sig.0];
        let method_name = sig_data.name;
        let takes_self = sig_data.takes_self;
        let is_param = sig_data.args.iter().any(|(arg, _)| *arg == name);
        let is_group = binders.iter().any(|(group, _)| *group == name);
        if name.is_self(self.db) {
            if !takes_self {
                let detail = format!(
                    "the quote uses `self`, but the emitted method `{}` does not bind `self`",
                    method_name.data(self.db),
                );
                return Err(self.invalid_quote(expr, &detail));
            }
            return Ok(self.push_gen(GenExpr::SelfRef));
        }
        if template.open.contains(&name) {
            // Binder groups shadow method parameters; a bare group never
            // names a value — its payload is reached field by field.
            if is_group {
                let detail = format!(
                    "`{}` is bound by an enclosing `${{variant}}({})` arm pattern as a \
                     binder group; payload fields are reached with `{}.${{field}}`",
                    name.data(self.db),
                    name.data(self.db),
                    name.data(self.db),
                );
                return Err(self.invalid_quote(expr, &detail));
            }
            // Validated against the destination signature when elaboration
            // started.
            return Ok(self.push_gen(GenExpr::ArgRef(name)));
        }
        let detail = if is_param {
            format!(
                "`{}` matches a parameter of the emitted method but is not declared open; \
                 declare it with `quote({}) {{ .. }}`",
                name.data(self.db),
                name.data(self.db),
            )
        } else if is_group {
            format!(
                "`{}` matches a binder group of an enclosing match arm but is not declared \
                 open; declare it with `quote({}) {{ .. }}`",
                name.data(self.db),
                name.data(self.db),
            )
        } else {
            format!(
                "cannot resolve `{}` in a quote body; quotes support `self`, declared \
                 open names, and `${{...}}` holes",
                name.data(self.db),
            )
        };
        Err(self.invalid_quote(expr, &detail))
    }

    /// Elaborates a quote-body `<Ty as Trait>::CONST` qualified associated-const
    /// access (DEVX-B). The goal trait uses [`GenExpr::TraitConst`] (synthesis
    /// can spell the qualifier as `<ty as GoalTrait>` or `Self::CONST`); any
    /// other trait uses [`GenExpr::QualifiedConst`], which carries the canonical
    /// (import-resolved) trait path so synthesis emits a qualifier that resolves
    /// in the generated impl's scope.
    fn elab_qualified_const(
        &mut self,
        expr: ExprId,
        ty: TypeId<'db>,
        trait_: TraitRefId<'db>,
        name: IdentId<'db>,
    ) -> Result<GenExprId, ExecError> {
        let Some(written_path) = trait_.path(self.db).to_opt() else {
            return Err(self.invalid_quote(
                expr,
                "the trait in a `<Ty as Trait>::item` quote access is missing",
            ));
        };
        let written_path = canonical_trait_path(self.db, self.provider_top_mod, written_path);
        let written_def = resolve_trait_def(self.db, self.provider_top_mod, written_path);
        let goal_def = resolve_trait_def(self.db, self.provider_top_mod, self.goal_trait_path);
        let matches = match (written_def, goal_def) {
            (Some(written), Some(goal)) => written == goal,
            // An end did not resolve to a `Trait` def (e.g. a goal trait local
            // to the provider's own module): fall back to comparing the written
            // and goal traits' last-segment names, mirroring the named-selection
            // compat path in `goal_matches_provider`.
            _ => {
                let written_last = written_path.ident(self.db).to_opt();
                let goal_last = self.goal_trait_path.ident(self.db).to_opt();
                written_last.is_some() && written_last == goal_last
            }
        };
        if matches {
            // The goal trait: the shorter `TraitConst` (synthesis can spell
            // `Self::NAME` for the `Self` type).
            Ok(self.push_gen(GenExpr::TraitConst { ty, name }))
        } else {
            // An arbitrary (non-goal) trait, e.g. `<FieldTy as AbiSize>::HEAD_SIZE`
            // from an `Encode` provider. Carry the canonical trait path so
            // synthesis emits `<ty as CanonicalTrait>::NAME`, which resolves in
            // the generated impl's scope regardless of the user module's imports.
            Ok(self.push_gen(GenExpr::QualifiedConst {
                ty,
                trait_path: written_path,
                name,
            }))
        }
    }

    /// The value captured for a hole expression when its quote was built.
    fn quote_hole_value(
        &mut self,
        expr: ExprId,
        template: &QuoteTemplate<'db>,
    ) -> Result<Value<'db>, ExecError> {
        match template
            .holes
            .iter()
            .find_map(|(hole, value)| (*hole == expr).then(|| value.clone()))
        {
            Some(value) => Ok(value),
            None => Err(self.invalid_quote(
                expr,
                "splice hole was not captured when the quote was built",
            )),
        }
    }

    fn eval_method_call(
        &mut self,
        expr: ExprId,
        receiver: ExprId,
        method: Partial<IdentId<'db>>,
        generic_args: GenericArgListId<'db>,
        args: Vec<crate::hir_def::expr::CallArg<'db>>,
    ) -> Result<Value<'db>, ExecError> {
        let Some(method) = method.to_opt() else {
            return Err(self.unsupported_expr(expr));
        };
        let receiver_value = self.eval_expr(receiver)?;
        let method_name = method.data(self.db).clone();
        // FREEZE (TD5.0): the reflection-read method names are pinned by
        // [`RECOGNIZED_REFLECT_OPS`]; `builder.*` ops are pinned in
        // `eval_builder_method`. See docs/dev/TD5_PROVIDER_COMMAND_SURFACE.md.
        //
        // TD5c: there are NO bespoke reflection-read arms here any more.
        // Every `reflect.*`/`field.*`/`variant.*` non-iterating read is served
        // by the typed read-only handle the receiver `Value` carries
        // ([`ReflectHandle`]/[`FieldHandle`]/[`VariantHandle`]): the executor
        // asks the handle to resolve a named read (argument-free via
        // `scalar_read`, or the unary `precedes` via `binary_read`) and lifts
        // the result into a `Value` — it does not know those names. So
        // [`RECOGNIZED_REFLECT_OPS`] is now EMPTY: the reflection-read surface
        // is off the recognized string-keyed executor surface entirely.
        //
        // The reflection iterables (`reflect.fields()`/`reflect.variants()`/
        // `variant.fields()`) are likewise ORDINARY method calls now: they
        // return a `Value::Seq` of typed read-only handles, built eagerly from
        // the reflection here (`reflection_sequence`). The `for`-loop iterates
        // that sequence like any other value; the executor no longer
        // special-cases the iterable *expression* (the `eval_iterable`
        // interception is gone).
        match receiver_value {
            Value::Builder => self.eval_builder_method(expr, &method_name, generic_args, &args),
            Value::Reflect(handle) => {
                if !args.is_empty() {
                    return Err(self.unsupported_expr(expr));
                }
                // Argument-free scalar read (`is_struct`/`is_enum`/
                // `target_name`) → handle table; otherwise the `fields`/
                // `variants` iterables → a `Value::Seq` of handles.
                if let Some(read) = handle.scalar_read(method_name.as_str()) {
                    return Ok(read.into_value(self.db));
                }
                match self.reflection_sequence(method_name.as_str()) {
                    Some(seq) => Ok(Value::Seq(seq)),
                    None => Err(self.unsupported_expr(expr)),
                }
            }
            Value::Field(field) => {
                if !args.is_empty() {
                    return Err(self.unsupported_expr(expr));
                }
                match field.scalar_read(method_name.as_str()) {
                    Some(read) => Ok(read.into_value(self.db)),
                    None => Err(self.unsupported_expr(expr)),
                }
            }
            Value::Variant(variant) => {
                // Argument-free reads on a variant: the scalar `is_default`
                // (handle table) or the `fields` iterable (a `Value::Seq` of
                // field handles for this variant's payload).
                if args.is_empty() {
                    if let Some(read) = variant.scalar_read(method_name.as_str()) {
                        return Ok(read.into_value(self.db));
                    }
                    return match self.variant_field_sequence(variant.index(), method_name.as_str()) {
                        Some(seq) => Ok(Value::Seq(seq)),
                        None => Err(self.unsupported_expr(expr)),
                    };
                }
                // Binary read (`precedes(other)`) → handle's binary vocabulary.
                if let [other] = args.as_slice()
                    && let Some(read) = variant.binary_read(method_name.as_str())
                {
                    let other_value = self.eval_expr(other.expr)?;
                    return match read.apply(&other_value) {
                        Some(result) => Ok(Value::Bool(result)),
                        None => Err(self.unsupported_expr(other.expr)),
                    };
                }
                Err(self.unsupported_expr(expr))
            }
            _ => Err(self.unsupported_expr(expr)),
        }
    }

    /// Builds the ordinary sequence value produced by a `reflect.*` iterable
    /// method (`fields`/`variants`); `None` for any other name (the executor
    /// maps that to "unsupported", exactly as an unrecognized read would). The
    /// elements are the SAME typed read-only handles the scalar path builds —
    /// `FieldHandle`/`VariantHandle` constructed from the reflection arena —
    /// so identity (`FieldKey` / variant decl-order index) and order
    /// (declaration order) are preserved exactly. A handle ctor only fails for
    /// a key/index the arena does not hold; since the items come from that same
    /// arena the lookup always succeeds, so a malformed handle (a defect) is
    /// skipped via `filter_map` rather than aborting the sequence.
    fn reflection_sequence(&self, method: &str) -> Option<Vec<Value<'db>>> {
        match method {
            "fields" => Some(
                self.reflection
                    .struct_fields()
                    .iter()
                    .filter_map(|field| {
                        FieldHandle::new(
                            self.db,
                            self.reflection,
                            FieldKey {
                                variant: field.variant,
                                index: field.index,
                            },
                        )
                        .map(Value::Field)
                    })
                    .collect(),
            ),
            "variants" => Some(
                self.reflection
                    .variants()
                    .iter()
                    .filter_map(|variant| {
                        VariantHandle::new(self.reflection, variant.index).map(Value::Variant)
                    })
                    .collect(),
            ),
            _ => None,
        }
    }

    /// Builds the ordinary sequence value produced by `variant.fields()` — the
    /// payload fields of the variant at `variant_index`, as `FieldHandle`
    /// values in declaration order. `None` for any other method name. Same
    /// identity/order preservation and `filter_map` rationale as
    /// [`Self::reflection_sequence`].
    fn variant_field_sequence(&self, variant_index: usize, method: &str) -> Option<Vec<Value<'db>>> {
        if method != "fields" {
            return None;
        }
        let fields = self
            .reflection
            .variant(variant_index)
            .map(|variant| {
                variant
                    .fields
                    .iter()
                    .filter_map(|field| {
                        FieldHandle::new(
                            self.db,
                            self.reflection,
                            FieldKey {
                                variant: field.variant,
                                index: field.index,
                            },
                        )
                        .map(Value::Field)
                    })
                    .collect()
            })
            .unwrap_or_default();
        Some(fields)
    }

    /// The reflected field a `require<Trait>(ty)` argument came from, when the
    /// argument is literally `<field-expr>.ty()` and the receiver evaluates to a
    /// reflected field handle. Best-effort provenance for the effect trace
    /// (TD5.1); returns `None` for any other type expression. Re-evaluating the
    /// receiver is side-effect free — `field.ty()` is a pure reflection read.
    fn require_field_origin(&mut self, arg: ExprId) -> Option<FieldKey> {
        let Partial::Present(Expr::MethodCall(receiver, method, _, method_args)) =
            arg.data(self.db, self.body)
        else {
            return None;
        };
        if method.to_opt()?.data(self.db).as_str() != "ty" || !method_args.is_empty() {
            return None;
        }
        match self.eval_expr(*receiver).ok()? {
            Value::Field(field) => Some(field.key()),
            _ => None,
        }
    }

    /// The main dispatch site for `builder.*` operations.
    ///
    /// FREEZE (TD5.0): every `(method, args)` arm below is a recognized
    /// provider-body op, mirrored by [`RECOGNIZED_BUILDER_OPS`]. Adding an op
    /// requires a TD5 category decision (see
    /// `docs/dev/TD5_PROVIDER_COMMAND_SURFACE.md`); update the canonical list
    /// and the inventory doc in the same change, or the freeze test fails.
    fn eval_builder_method(
        &mut self,
        expr: ExprId,
        method: &str,
        generic_args: GenericArgListId<'db>,
        args: &[crate::hir_def::expr::CallArg<'db>],
    ) -> Result<Value<'db>, ExecError> {
        // Commands check the finish flag; pure expression builders do not.
        match (method, args) {
            // --- commands ---------------------------------------------
            ("require", [arg]) => {
                self.check_not_finished(expr)?;
                let Some(trait_path) = self.single_type_generic_arg_path(generic_args) else {
                    return Err(ExecError {
                        kind: ProviderFailureKind::InvalidRequirement {
                            detail: "the type argument must be a single trait path, e.g. \
                                     `builder.require<Eq>(field.ty())`"
                                .into(),
                        },
                        range: self.expr_range(expr),
                    });
                };
                let Value::Ty(ty) = self.eval_expr(arg.expr)? else {
                    return Err(ExecError {
                        kind: ProviderFailureKind::InvalidRequirement {
                            detail: format!(
                                "`require<{}>(..)` expects a type argument, e.g. `field.ty()`",
                                trait_path.pretty_print(self.db)
                            ),
                        },
                        range: self.expr_range(arg.expr),
                    });
                };
                let trait_path = canonical_trait_path(self.db, self.provider_top_mod, trait_path);
                // TD5.2 migration: `require` re-enters ordinary compilation as a
                // typed provider effect, NOT a bespoke executor command (the
                // `BuilderCommand::Require` variant is deleted). Synthesis
                // ([`super::provider_synthesis::requirement_where_clause`]) replays
                // a generic-param requirement as an ordinary `ty: Trait`
                // where-predicate on the generated impl; a concrete requirement is
                // a concrete obligation discharged at the generated body's use site.
                // Either way the executor no longer OWNS the requirement.
                let field_origin = self.require_field_origin(arg.expr);
                self.effects.push(ProviderEffect::Require {
                    ty,
                    trait_path,
                    field_origin,
                });
                Ok(Value::Unit)
            }
            ("emit_method", [name_arg, body_arg]) => {
                self.check_not_finished(expr)?;
                // DEVX-A: the signature is INFERRED from the goal trait's
                // declaration of `name` — for a derive provider the emitted
                // method always *is* the goal trait's method, so re-spelling
                // its signature (the old `method`/`with_self`/`with_arg`/
                // `returns` dance) was pure ceremony. We synthesize the same
                // `GenMethodSig` the dance produced: self-ness, arg names, and
                // arg/return types come from the declaration, with the trait's
                // `Self`/self-type-param substituted by `target_ty()` (argument
                // position) / `self_ty()` (return position) exactly as the dance
                // did. The generated impl is byte-identical.
                let name = self.string_value_ident(name_arg.expr)?;
                let sig = self.infer_method_sig(name_arg.expr, name)?;
                let body = match self.eval_expr(body_arg.expr)? {
                    Value::Expr(body) => body,
                    // Quotes land here: the template elaborates into the
                    // same generated-expression layer the explicit builder
                    // calls produce.
                    Value::Quote(quote) => self.elaborate_quote(quote, sig, &[])?,
                    _ => {
                        return Err(self.invalid_method(
                            body_arg.expr,
                            "expected a generated expression or a quote",
                        ));
                    }
                };
                if self.emitted_methods.contains(&name) {
                    return Err(self.invalid_method(
                        name_arg.expr,
                        &format!("duplicate generated method `{}`", name.data(self.db)),
                    ));
                }
                self.emitted_methods.push(name);
                self.effects.push(ProviderEffect::EmitMethod { sig, body });
                Ok(Value::Unit)
            }
            ("emit_assoc_ty", [name_arg, ty_arg]) => {
                self.check_not_finished(expr)?;
                let name = self.string_value_ident(name_arg.expr)?;
                let ty = self.gen_ty_arg(ty_arg.expr)?;
                self.check_fresh_assoc(name_arg.expr, name)?;
                self.effects.push(ProviderEffect::EmitAssocTy { name, ty });
                Ok(Value::Unit)
            }
            ("emit_const", [name_arg, ty_arg, value_arg]) => {
                self.check_not_finished(expr)?;
                let name = self.string_value_ident(name_arg.expr)?;
                let ty = self.gen_ty_arg(ty_arg.expr)?;
                let Value::Expr(value) = self.eval_expr(value_arg.expr)? else {
                    return Err(
                        self.invalid_assoc(value_arg.expr, "expected a generated expression")
                    );
                };
                self.check_fresh_assoc(name_arg.expr, name)?;
                self.effects
                    .push(ProviderEffect::EmitConst { name, ty, value });
                Ok(Value::Unit)
            }
            ("finish", []) => {
                if self.finished {
                    return Err(ExecError {
                        kind: ProviderFailureKind::DuplicateFinish,
                        range: self.expr_range(expr),
                    });
                }
                self.finished = true;
                Ok(Value::Unit)
            }

            // --- expression builders -----------------------------------
            ("bool", [arg]) => {
                let Value::Bool(value) = self.eval_expr(arg.expr)? else {
                    return Err(self.unsupported_expr(arg.expr));
                };
                Ok(self.push_expr(GenExpr::Bool(value)))
            }
            ("and", [lhs, rhs]) => {
                let lhs = self.gen_expr_arg(lhs.expr)?;
                let rhs = self.gen_expr_arg(rhs.expr)?;
                Ok(self.push_expr(GenExpr::And(lhs, rhs)))
            }
            ("add", [lhs, rhs]) => {
                let lhs = self.gen_expr_arg(lhs.expr)?;
                let rhs = self.gen_expr_arg(rhs.expr)?;
                Ok(self.push_expr(GenExpr::Add(lhs, rhs)))
            }
            ("or", [lhs, rhs]) => {
                let lhs = self.gen_expr_arg(lhs.expr)?;
                let rhs = self.gen_expr_arg(rhs.expr)?;
                Ok(self.push_expr(GenExpr::Or(lhs, rhs)))
            }
            ("self_ref", []) => Ok(self.push_expr(GenExpr::SelfRef)),
            ("arg_ref", [arg]) => {
                let name = self.string_value_ident(arg.expr)?;
                Ok(self.push_expr(GenExpr::ArgRef(name)))
            }
            ("field_get", [base, field]) => {
                let base = self.gen_expr_arg(base.expr)?;
                let Value::Field(field) = self.eval_expr(field.expr)? else {
                    return Err(self.unsupported_expr(expr));
                };
                Ok(self.push_expr(GenExpr::FieldGet(base, field.key())))
            }
            ("eq", [lhs, rhs]) => {
                let lhs = self.gen_expr_arg(lhs.expr)?;
                let rhs = self.gen_expr_arg(rhs.expr)?;
                Ok(self.push_expr(GenExpr::EqCmp(lhs, rhs)))
            }
            ("lt", [lhs, rhs]) => {
                let lhs = self.gen_expr_arg(lhs.expr)?;
                let rhs = self.gen_expr_arg(rhs.expr)?;
                Ok(self.push_expr(GenExpr::LtCmp(lhs, rhs)))
            }
            ("gt", [lhs, rhs]) => {
                let lhs = self.gen_expr_arg(lhs.expr)?;
                let rhs = self.gen_expr_arg(rhs.expr)?;
                Ok(self.push_expr(GenExpr::GtCmp(lhs, rhs)))
            }
            ("trait_call", [ty_arg, method_arg, extra @ ..]) => {
                let Value::Ty(ty) = self.eval_expr(ty_arg.expr)? else {
                    return Err(self.unsupported_expr(ty_arg.expr));
                };
                let method = self.string_value_ident(method_arg.expr)?;
                let mut call_args = Vec::with_capacity(extra.len());
                for arg in extra {
                    call_args.push(self.gen_expr_arg(arg.expr)?);
                }
                Ok(self.push_expr(GenExpr::TraitCall {
                    ty,
                    method,
                    args: call_args,
                }))
            }
            ("trait_const", [ty_arg, name_arg]) => {
                let Value::Ty(ty) = self.eval_expr(ty_arg.expr)? else {
                    return Err(self.unsupported_expr(ty_arg.expr));
                };
                let name = self.string_value_ident(name_arg.expr)?;
                Ok(self.push_expr(GenExpr::TraitConst { ty, name }))
            }
            ("call", [receiver_arg, method_arg, extra @ ..]) => {
                let receiver = self.gen_expr_arg(receiver_arg.expr)?;
                let method = self.string_value_ident(method_arg.expr)?;
                let mut call_args = Vec::with_capacity(extra.len());
                for arg in extra {
                    call_args.push(self.gen_expr_arg(arg.expr)?);
                }
                Ok(self.push_expr(GenExpr::MethodCall {
                    receiver,
                    method,
                    args: call_args,
                }))
            }
            ("static_call", [ty_arg, method_arg, extra @ ..]) => {
                let Value::Ty(ty) = self.eval_expr(ty_arg.expr)? else {
                    return Err(self.unsupported_expr(ty_arg.expr));
                };
                // The callee path is the type as written with the function
                // name appended, so only path types can be call targets.
                let TypeKind::Path(Partial::Present(ty_path)) = ty.data(self.db) else {
                    return Err(self.unsupported_expr(ty_arg.expr));
                };
                let method = self.string_value_ident(method_arg.expr)?;
                let path = ty_path.push_ident(self.db, method);
                let mut call_args = Vec::with_capacity(extra.len());
                for arg in extra {
                    call_args.push(self.gen_expr_arg(arg.expr)?);
                }
                Ok(self.push_expr(GenExpr::StaticCall {
                    path,
                    args: call_args,
                }))
            }
            // --- compile-time strings ----------------------------------
            ("concat", [lhs, rhs]) => {
                let lhs = self.str_value(lhs.expr)?;
                let rhs = self.str_value(rhs.expr)?;
                let joined = format!("{}{}", lhs.data(self.db), rhs.data(self.db));
                Ok(Value::Str(StringId::new(self.db, joined)))
            }
            ("str", [arg]) => {
                let value = self.checked_inline_str(arg.expr)?;
                Ok(self.push_expr(GenExpr::StrLit(value)))
            }
            ("str_ty", [arg]) => {
                let value = self.checked_inline_str(arg.expr)?;
                Ok(self.push_ty(GenTy::StringN(value.data(self.db).len())))
            }
            // --- tuples ------------------------------------------------
            ("tuple_expr", []) => Ok(self.push_expr(GenExpr::Tuple(Vec::new()))),
            ("with_elem", [tuple_arg, elem_arg]) => {
                let tuple = self.gen_expr_arg(tuple_arg.expr)?;
                let elem = self.gen_expr_arg(elem_arg.expr)?;
                let GenExpr::Tuple(elems) = &self.exprs[tuple.0] else {
                    return Err(self.invalid_method(tuple_arg.expr, "`with_elem` expects a tuple"));
                };
                let mut elems = elems.clone();
                elems.push(elem);
                Ok(self.push_expr(GenExpr::Tuple(elems)))
            }
            ("tuple_ty", []) => Ok(self.push_ty(GenTy::Tuple(Vec::new()))),
            ("with_elem_ty", [tuple_arg, elem_arg]) => {
                let tuple = self.gen_ty_arg(tuple_arg.expr)?;
                let elem = self.gen_ty_arg(elem_arg.expr)?;
                let GenTy::Tuple(elems) = &self.tys[tuple.0] else {
                    return Err(
                        self.invalid_method(tuple_arg.expr, "`with_elem_ty` expects a tuple type")
                    );
                };
                let mut elems = elems.clone();
                elems.push(elem);
                Ok(self.push_ty(GenTy::Tuple(elems)))
            }
            ("trait_assoc_ty", [ty_arg, name_arg]) => {
                let Value::Ty(ty) = self.eval_expr(ty_arg.expr)? else {
                    return Err(self.unsupported_expr(ty_arg.expr));
                };
                let name = self.string_value_ident(name_arg.expr)?;
                Ok(self.push_ty(GenTy::Projection { ty, name }))
            }
            // --- misc --------------------------------------------------
            ("keccak", [arg]) => {
                let arg = self.gen_expr_arg(arg.expr)?;
                Ok(self.push_expr(GenExpr::Keccak(arg)))
            }
            // TD5c: `same_ty`/`same_field` are NO LONGER bespoke arms here. They
            // are spelled `builder.*` but, per the TD5.0 inventory, are pure
            // read-only identity comparisons mis-shelved by spelling; their
            // vocabulary now lives on the typed read-only [`ReflectionCompare`]
            // table, consulted in the catch-all below.
            ("struct_init", []) => {
                if !self.reflection.is_struct() {
                    return Err(self.invalid_method(expr, "`struct_init` on a non-struct target"));
                }
                Ok(self.push_expr(GenExpr::StructInit { fields: Vec::new() }))
            }
            ("variant_init", [variant_arg]) => {
                let Value::Variant(variant) = self.eval_expr(variant_arg.expr)? else {
                    return Err(self.unsupported_expr(variant_arg.expr));
                };
                Ok(self.push_expr(GenExpr::VariantInit {
                    variant: variant.index(),
                    fields: Vec::new(),
                }))
            }
            ("with_field", [init_arg, field_arg, value_arg]) => {
                let init = self.gen_expr_arg(init_arg.expr)?;
                let Value::Field(field) = self.eval_expr(field_arg.expr)? else {
                    return Err(self.unsupported_expr(field_arg.expr));
                };
                let field = field.key();
                let value = self.gen_expr_arg(value_arg.expr)?;
                let extended = match &self.exprs[init.0] {
                    GenExpr::StructInit { fields } => {
                        if field.variant.is_some() {
                            return Err(self.invalid_method(
                                field_arg.expr,
                                "variant field used in a struct initializer",
                            ));
                        }
                        let mut fields = fields.clone();
                        fields.push((field, value));
                        GenExpr::StructInit { fields }
                    }
                    GenExpr::VariantInit { variant, fields } => {
                        if field.variant != Some(*variant) {
                            return Err(self.invalid_method(
                                field_arg.expr,
                                "field does not belong to the initialized variant",
                            ));
                        }
                        let mut fields = fields.clone();
                        fields.push((field, value));
                        GenExpr::VariantInit {
                            variant: *variant,
                            fields,
                        }
                    }
                    _ => {
                        return Err(self.invalid_method(
                            init_arg.expr,
                            "`with_field` expects a struct or variant initializer",
                        ));
                    }
                };
                Ok(self.push_expr(extended))
            }
            ("match_expr", [scrutinee_arg]) => {
                let scrutinee = self.gen_expr_arg(scrutinee_arg.expr)?;
                Ok(self.push_expr(GenExpr::Match {
                    scrutinee,
                    arms: Vec::new(),
                }))
            }
            ("with_arm", [match_arg, pat_arg, body_arg]) => {
                let match_ = self.gen_expr_arg(match_arg.expr)?;
                let Value::Pat(pat) = self.eval_expr(pat_arg.expr)? else {
                    return Err(self.unsupported_expr(pat_arg.expr));
                };
                let body = self.gen_expr_arg(body_arg.expr)?;
                let GenExpr::Match { scrutinee, arms } = &self.exprs[match_.0] else {
                    return Err(self
                        .invalid_method(match_arg.expr, "`with_arm` expects a match expression"));
                };
                let scrutinee = *scrutinee;
                let mut arms = arms.clone();
                arms.push((pat, body));
                Ok(self.push_expr(GenExpr::Match { scrutinee, arms }))
            }
            ("wildcard_pat", []) => Ok(self.push_pat(GenPat::Wildcard)),
            ("variant_pat", [variant_arg, prefix_arg]) => {
                let Value::Variant(variant) = self.eval_expr(variant_arg.expr)? else {
                    return Err(self.unsupported_expr(variant_arg.expr));
                };
                let prefix = self.string_value_ident(prefix_arg.expr)?;
                Ok(self.push_pat(GenPat::Variant {
                    variant: variant.index(),
                    prefix,
                }))
            }
            ("variant_binder", [variant_arg, field_arg, prefix_arg]) => {
                let Value::Variant(variant) = self.eval_expr(variant_arg.expr)? else {
                    return Err(self.unsupported_expr(variant_arg.expr));
                };
                let Value::Field(field) = self.eval_expr(field_arg.expr)? else {
                    return Err(self.unsupported_expr(field_arg.expr));
                };
                let variant = variant.index();
                let field = field.key();
                if field.variant != Some(variant) {
                    return Err(self.invalid_method(
                        field_arg.expr,
                        "field does not belong to the named variant",
                    ));
                }
                let prefix = self.string_value_ident(prefix_arg.expr)?;
                Ok(self.push_expr(GenExpr::VariantBinder {
                    variant,
                    field: field.index,
                    prefix,
                }))
            }
            // DEVX-A: the `method`/`with_self`/`with_arg`/`returns` signature
            // dance was dropped. A derive provider's emitted method signature
            // always re-spelled the goal trait's own method declaration, so it
            // is now INFERRED at `emit_method(name, body)` (see
            // `infer_method_sig`) instead of authored op-by-op.
            ("target_ty", []) => Ok(Value::Ty(self.target_ty)),
            ("self_ty", []) => Ok(Value::Ty(TypeId::fallback_self_ty(self.db))),
            ("ty", []) => {
                let Some(path) = self.single_type_generic_arg(generic_args) else {
                    return Err(self.unsupported_expr(expr));
                };
                Ok(Value::Ty(path))
            }
            // TD5c: `same_ty`/`same_field` are spelled `builder.*` but are pure
            // read-only identity comparisons (mis-shelved by spelling, per the
            // TD5.0 inventory). Their vocabulary lives on the typed read-only
            // [`ReflectionCompare`] table, NOT as bespoke arms above — the
            // executor consults it by name and applies the resolved comparator
            // without knowing those names. Because this is a fall-through (not a
            // `("name",` arm), the freeze scan no longer counts `same_ty`/
            // `same_field` in RECOGNIZED_BUILDER_OPS (45 → 43).
            (name, [lhs, rhs]) if ReflectionCompare::is_compare_name(name) => {
                let lhs_value = self.eval_expr(lhs.expr)?;
                // Known compare op: a wrong first operand is attributed to the
                // operand's span, exactly as the old bespoke arms did.
                let Some(read) = ReflectionCompare::binary_read(name, &lhs_value) else {
                    return Err(self.unsupported_expr(lhs.expr));
                };
                let rhs_value = self.eval_expr(rhs.expr)?;
                match read.apply(&rhs_value) {
                    Some(result) => Ok(Value::Bool(result)),
                    None => Err(self.unsupported_expr(rhs.expr)),
                }
            }
            // FREEZE (TD5.0): an unrecognized `builder.*` op (or a recognized
            // one with the wrong arity) falls here. The set of recognized
            // names is pinned to RECOGNIZED_BUILDER_OPS by the
            // `recognized_builder_ops_match_dispatch` test, which scans this
            // function's arms — see docs/dev/TD5_PROVIDER_COMMAND_SURFACE.md.
            _ => Err(self.unsupported_expr(expr)),
        }
    }

    fn check_not_finished(&mut self, expr: ExprId) -> Result<(), ExecError> {
        if self.finished {
            return Err(ExecError {
                kind: ProviderFailureKind::CommandAfterFinish,
                range: self.expr_range(expr),
            });
        }
        Ok(())
    }

    fn invalid_method(&mut self, expr: ExprId, detail: &str) -> ExecError {
        ExecError {
            kind: ProviderFailureKind::InvalidMethod {
                detail: detail.to_string(),
            },
            range: self.expr_range(expr),
        }
    }

    fn invalid_assoc(&mut self, expr: ExprId, detail: &str) -> ExecError {
        ExecError {
            kind: ProviderFailureKind::InvalidAssoc {
                detail: detail.to_string(),
            },
            range: self.expr_range(expr),
        }
    }

    fn invalid_string(&mut self, expr: ExprId, detail: &str) -> ExecError {
        ExecError {
            kind: ProviderFailureKind::InvalidString {
                detail: detail.to_string(),
            },
            range: self.expr_range(expr),
        }
    }

    fn invalid_quote(&mut self, expr: ExprId, detail: &str) -> ExecError {
        ExecError {
            kind: ProviderFailureKind::InvalidQuote {
                detail: detail.to_string(),
            },
            range: self.expr_range(expr),
        }
    }

    /// Rejects a second `emit_const` / `emit_assoc_ty` / method-name reuse
    /// for `name` (the generated impl namespaces consts, types, and methods
    /// together for simplicity; EIP-712-style providers never collide).
    fn check_fresh_assoc(&mut self, expr: ExprId, name: IdentId<'db>) -> Result<(), ExecError> {
        if self.emitted_assocs.contains(&name) {
            return Err(self.invalid_assoc(
                expr,
                &format!(
                    "duplicate generated associated item `{}`",
                    name.data(self.db)
                ),
            ));
        }
        self.emitted_assocs.push(name);
        Ok(())
    }

    fn push_expr(&mut self, expr: GenExpr<'db>) -> Value<'db> {
        Value::Expr(self.push_gen(expr))
    }

    fn push_gen(&mut self, expr: GenExpr<'db>) -> GenExprId {
        self.exprs.push(expr);
        GenExprId(self.exprs.len() - 1)
    }

    fn push_pat(&mut self, pat: GenPat<'db>) -> Value<'db> {
        Value::Pat(self.push_gen_pat(pat))
    }

    fn push_gen_pat(&mut self, pat: GenPat<'db>) -> GenPatId {
        self.pats.push(pat);
        GenPatId(self.pats.len() - 1)
    }

    fn push_ty(&mut self, ty: GenTy<'db>) -> Value<'db> {
        self.tys.push(ty);
        Value::GenTy(GenTyId(self.tys.len() - 1))
    }

    fn gen_expr_arg(&mut self, expr: ExprId) -> Result<GenExprId, ExecError> {
        match self.eval_expr(expr)? {
            Value::Expr(id) => Ok(id),
            _ => Err(self.unsupported_expr(expr)),
        }
    }

    /// DEVX-A: infer the signature of the emitted method `name` from the goal
    /// trait's declaration, returning a fresh [`SigId`].
    ///
    /// For a derive provider the emitted method always *is* the goal trait's
    /// method, so its signature is exactly the trait's declaration of `name`.
    /// The old author-facing dance (`builder.method("name")` then
    /// `with_self`/`with_arg`/`returns`) merely re-spelled that declaration; we
    /// reconstruct the identical [`GenMethodSig`] here so the generated impl is
    /// byte-identical:
    ///
    /// * self-ness ← the declaration's first parameter being a `self` receiver;
    /// * argument names ← the declaration's remaining parameter names, in order;
    /// * argument/return *types* ← the declared types, with the trait's `Self`
    ///   type and its own generic parameters (a saturated derive goal binds
    ///   every trait type-param to the target) substituted by the SAME witness
    ///   the dance used — `target_ty()` in argument position, `self_ty()`
    ///   (`Self`) in return position. Any other declared type is used as
    ///   written.
    ///
    /// `at` is the `name` argument expression of `emit_method`, used for error
    /// spans.
    fn infer_method_sig(&mut self, at: ExprId, name: IdentId<'db>) -> Result<SigId, ExecError> {
        let goal = self.goal_trait_path;
        let Some(trait_def) = resolve_trait_def(self.db, self.provider_top_mod, goal) else {
            return Err(self.invalid_method(
                at,
                &format!(
                    "cannot infer the signature of `{}`: the goal trait `{}` does not \
                     resolve to a trait declaration",
                    name.data(self.db),
                    goal.pretty_print(self.db),
                ),
            ));
        };

        let Some(method) = self.trait_method(trait_def, name) else {
            return Err(self.invalid_method(
                at,
                &format!(
                    "the goal trait `{}` declares no method `{}` to infer a signature from",
                    goal.pretty_print(self.db),
                    name.data(self.db),
                ),
            ));
        };

        // The trait's own generic parameter names: a saturated derive goal
        // (`Eq<Target>`) binds each of these to the target type, so a reference
        // to one in the method signature is the target type — exactly what the
        // dance spelled `target_ty()` for (e.g. `Eq<T = Self>`'s `_ other: T`).
        let trait_params: Vec<IdentId<'db>> = trait_def
            .generic_params(self.db)
            .data(self.db)
            .iter()
            .filter_map(|param| param.name().to_opt())
            .collect();

        let mut takes_self = false;
        let mut args = Vec::new();
        if let Some(params) = method.params_list(self.db).to_opt() {
            for (idx, param) in params.data(self.db).iter().enumerate() {
                if idx == 0 && param.is_self_param(self.db) {
                    takes_self = true;
                    continue;
                }
                let Some(arg_name) = param.name() else {
                    return Err(self.invalid_method(
                        at,
                        &format!(
                            "cannot infer the signature of `{}`: parameter {} of the goal \
                             trait's declaration has no name",
                            name.data(self.db),
                            idx,
                        ),
                    ));
                };
                let Some(decl_ty) = param.ty.to_opt() else {
                    return Err(self.invalid_method(
                        at,
                        &format!(
                            "cannot infer the signature of `{}`: parameter `{}` of the goal \
                             trait's declaration has no type",
                            name.data(self.db),
                            arg_name.data(self.db),
                        ),
                    ));
                };
                let ty = self.substitute_target(decl_ty, &trait_params, SigPosition::Arg);
                args.push((arg_name, ty));
            }
        }

        let ret = method
            .ret_type_ref(self.db)
            .map(|decl_ty| self.substitute_target(decl_ty, &trait_params, SigPosition::Return));

        self.sigs.push(GenMethodSig {
            name,
            takes_self,
            args,
            ret,
        });
        Ok(SigId(self.sigs.len() - 1))
    }

    /// Finds the goal trait's method `name`, reading the trait's BASE scope
    /// graph (never the requesting ingot's merged graph — the executor runs in
    /// the expansion stage), mirroring [`resolve_trait_def`]'s stratification.
    fn trait_method(&self, trait_def: Trait<'db>, name: IdentId<'db>) -> Option<Func<'db>> {
        let scope = ScopeId::from_item(trait_def.into());
        let base = base_scope_graph_impl(self.db, trait_def.top_mod(self.db));
        base.child_items(scope).find_map(|item| match item {
            ItemKind::Func(func) if func.name(self.db).to_opt() == Some(name) => Some(func),
            _ => None,
        })
    }

    /// Maps a goal-trait-declared signature type to the witness the dance used:
    /// the trait's `Self` type, or any of the trait's own generic parameters
    /// (bound to the target in a saturated derive goal), becomes `target_ty()`
    /// in argument position and `self_ty()` (`Self`) in return position; any
    /// other type is used as written.
    fn substitute_target(
        &self,
        decl_ty: TypeId<'db>,
        trait_params: &[IdentId<'db>],
        position: SigPosition,
    ) -> TypeId<'db> {
        let is_target = decl_ty.is_self_ty(self.db)
            || matches!(
                decl_ty.data(self.db),
                TypeKind::Path(Partial::Present(path))
                    if path.as_ident(self.db).is_some_and(|id| trait_params.contains(&id))
            );
        if is_target {
            match position {
                SigPosition::Arg => self.target_ty,
                SigPosition::Return => TypeId::fallback_self_ty(self.db),
            }
        } else {
            decl_ty
        }
    }

    /// A generated-type argument. Concrete `Ty` witnesses (from `ty<T>()` /
    /// `field.ty()` / `target_ty()`) are accepted and wrapped, so type
    /// commands take either currency.
    fn gen_ty_arg(&mut self, expr: ExprId) -> Result<GenTyId, ExecError> {
        match self.eval_expr(expr)? {
            Value::GenTy(id) => Ok(id),
            Value::Ty(ty) => {
                let Value::GenTy(id) = self.push_ty(GenTy::Concrete(ty)) else {
                    unreachable!("push_ty returns a GenTy value");
                };
                Ok(id)
            }
            _ => Err(self.unsupported_expr(expr)),
        }
    }

    /// A compile-time string operand: a string literal, a reflected name,
    /// or a `concat` result.
    fn str_value(&mut self, expr: ExprId) -> Result<StringId<'db>, ExecError> {
        match self.eval_expr(expr)? {
            Value::Str(value) => Ok(value),
            _ => Err(self.invalid_string(expr, "expected a compile-time string")),
        }
    }

    /// A compile-time string destined for a generated string literal or
    /// exact-width string type; enforces the inline string capacity.
    fn checked_inline_str(&mut self, expr: ExprId) -> Result<StringId<'db>, ExecError> {
        let value = self.str_value(expr)?;
        self.check_inline_capacity(expr, value)?;
        Ok(value)
    }

    fn check_inline_capacity(
        &mut self,
        expr: ExprId,
        value: StringId<'db>,
    ) -> Result<(), ExecError> {
        let len = value.data(self.db).len();
        if len > MAX_INLINE_STRING_BYTES {
            return Err(self.invalid_string(
                expr,
                &format!(
                    "string piece is {len} bytes; inline strings hold at most \
                     {MAX_INLINE_STRING_BYTES}"
                ),
            ));
        }
        Ok(())
    }

    fn string_value_ident(&mut self, expr: ExprId) -> Result<IdentId<'db>, ExecError> {
        let value = self.str_value(expr)?;
        Ok(IdentId::new(self.db, value.data(self.db).to_string()))
    }

    /// The single type argument of `require<Trait>(..)` / `ty<T>()`, as a
    /// path. Returns `None` for malformed argument lists.
    fn single_type_generic_arg_path(
        &self,
        generic_args: GenericArgListId<'db>,
    ) -> Option<crate::hir_def::PathId<'db>> {
        let [GenericArg::Type(type_arg)] = generic_args.data(self.db).as_slice() else {
            return None;
        };
        let ty = type_arg.ty.to_opt()?;
        match ty.data(self.db) {
            crate::hir_def::TypeKind::Path(path) => path.to_opt(),
            _ => None,
        }
    }

    /// The single type argument of `ty<T>()`, as written.
    fn single_type_generic_arg(&self, generic_args: GenericArgListId<'db>) -> Option<TypeId<'db>> {
        let [GenericArg::Type(type_arg)] = generic_args.data(self.db).as_slice() else {
            return None;
        };
        type_arg.ty.to_opt()
    }

    fn simple_pat_binding(&self, pat: PatId) -> Option<IdentId<'db>> {
        let Partial::Present(Pat::Path(Partial::Present(path), _)) = pat.data(self.db, self.body)
        else {
            return None;
        };
        path.as_ident(self.db)
    }

    fn simple_expr_path_ident(&self, expr: ExprId) -> Option<IdentId<'db>> {
        let Partial::Present(Expr::Path(Partial::Present(path))) = expr.data(self.db, self.body)
        else {
            return None;
        };
        path.as_ident(self.db)
    }

    /// Recognizes a quote-body qualified-path expression `<Ty as Trait>::item`:
    /// a final bare-`Ident` segment whose parent is a single `QualifiedType`
    /// segment. Returns the qualifying `(type_, trait_, item-name)` triple, or
    /// `None` for any other path shape (a bare ident, a generic-arg-bearing
    /// item segment, or a longer/multi-segment qualified path that this grammar
    /// does not yet accept).
    fn extract_qualified_path(
        &self,
        expr: ExprId,
    ) -> Option<(TypeId<'db>, TraitRefId<'db>, IdentId<'db>)> {
        let Partial::Present(Expr::Path(Partial::Present(path))) = expr.data(self.db, self.body)
        else {
            return None;
        };
        qualified_path_parts(self.db, *path)
    }
}

/// The pure path-shape recognizer behind
/// [`ProviderExecutor::extract_qualified_path`]: a `<Ty as Trait>::item`
/// associated-item access is a final bare-`Ident` segment (no generic args)
/// whose single parent is a `QualifiedType` segment. Any other shape — a bare
/// ident, an item segment carrying generic args, or a longer path — returns
/// `None`.
fn qualified_path_parts<'db>(
    db: &'db dyn HirDb,
    path: PathId<'db>,
) -> Option<(TypeId<'db>, TraitRefId<'db>, IdentId<'db>)> {
    // Final segment: a bare associated-item name with no generic args.
    let PathKind::Ident {
        ident,
        generic_args,
    } = path.kind(db)
    else {
        return None;
    };
    if !generic_args.is_empty(db) {
        return None;
    }
    let name = ident.to_opt()?;
    // Its sole parent: the `<Ty as Trait>` qualifier segment.
    let parent = path.parent(db)?;
    if parent.parent(db).is_some() {
        return None;
    }
    let PathKind::QualifiedType { type_, trait_ } = parent.kind(db) else {
        return None;
    };
    Some((type_, trait_, name))
}

/// A human-readable kind name for hole-value diagnostics.
fn value_kind_name(value: &Value) -> &'static str {
    match value {
        Value::Bool(_) => "compile-time bool",
        Value::Str(_) => "compile-time string",
        Value::Field(_) => "`Field` handle",
        Value::Variant(_) => "`Variant` handle",
        Value::Ty(_) | Value::GenTy(_) => "type value",
        Value::Expr(_) => "generated expression",
        Value::Pat(_) => "generated pattern",
        Value::Quote(_) => "quote value",
        Value::Builder => "builder capability",
        Value::Reflect(_) => "reflect capability",
        Value::Evidence => "evidence value",
        Value::Unit => "unit value",
        Value::Seq(_) => "compile-time sequence",
    }
}

#[cfg(test)]
mod freeze_guard {
    //! FREEZE (TD5.0): adding a provider-body op requires a TD5 category
    //! decision (see TD5_PROVIDER_COMMAND_SURFACE.md). Update this list only as
    //! part of a TD5 rung.
    //!
    //! These tests pin the executor's recognized command surface. They scan
    //! this very source file (`include_str!`) and cross-check the dispatch
    //! arms against the canonical `RECOGNIZED_*` lists, so the surface cannot
    //! grow (or shrink) silently while later TD5 rungs migrate ops off the
    //! executor. A new `("name", ..)` arm with no entry in the canonical list
    //! — or a list entry with no arm — fails the corresponding test.

    use super::{RECOGNIZED_BUILDER_OPS, RECOGNIZED_REFLECT_OPS};

    /// This module's source, embedded at compile time so the scan is
    /// path-independent and always matches the dispatch it pins.
    const SOURCE: &str = include_str!("provider_executor.rs");

    /// Returns the source slice from the first occurrence of `start` to the
    /// first occurrence of `end` after it. Panics if either marker is missing,
    /// so a refactor that renames the dispatch fns trips the test loudly.
    fn slice_between<'s>(start: &str, end: &str) -> &'s str {
        let from = SOURCE
            .find(start)
            .unwrap_or_else(|| panic!("freeze scan: marker `{start}` not found"));
        let rest = &SOURCE[from..];
        let to = rest
            .find(end)
            .unwrap_or_else(|| panic!("freeze scan: marker `{end}` not found after `{start}`"));
        &rest[..to]
    }

    /// Extracts every leading `("name",` arm key from a match body slice: a
    /// line whose first non-whitespace characters are `("`, taking the
    /// identifier up to the closing `"`. This is exactly the spelling of every
    /// recognized-op arm in the dispatch functions.
    fn arm_keys(slice: &str) -> Vec<String> {
        let mut keys = Vec::new();
        for line in slice.lines() {
            let trimmed = line.trim_start();
            if let Some(after) = trimmed.strip_prefix("(\"")
                && let Some(end) = after.find('"')
            {
                keys.push(after[..end].to_string());
            }
        }
        keys
    }

    fn sorted_unique(mut v: Vec<String>) -> Vec<String> {
        v.sort();
        v.dedup();
        v
    }

    #[test]
    fn recognized_builder_ops_match_dispatch() {
        // The body of `eval_builder_method`, from its signature to the
        // catch-all freeze marker, so neither the `RECOGNIZED_*` consts nor
        // unrelated string literals are scanned.
        let body = slice_between(
            "fn eval_builder_method(",
            "// FREEZE (TD5.0): an unrecognized `builder.*` op",
        );
        let dispatch = sorted_unique(arm_keys(body));
        let canonical = sorted_unique(
            RECOGNIZED_BUILDER_OPS
                .iter()
                .map(|s| s.to_string())
                .collect(),
        );
        assert_eq!(
            dispatch, canonical,
            "FREEZE (TD5.0): eval_builder_method dispatch arms drifted from \
             RECOGNIZED_BUILDER_OPS. Adding/removing a `builder.*` op requires a TD5 \
             category decision — see docs/dev/TD5_PROVIDER_COMMAND_SURFACE.md."
        );
        // No accidental duplicate spellings in the canonical list.
        assert_eq!(
            RECOGNIZED_BUILDER_OPS.len(),
            canonical.len(),
            "RECOGNIZED_BUILDER_OPS has duplicate entries"
        );
        // Pin the count too, so a same-size swap is still flagged for review.
        assert_eq!(
            RECOGNIZED_BUILDER_OPS.len(),
            39,
            "FREEZE (TD5.0): the builder command surface changed size; update the count \
             and docs/dev/TD5_PROVIDER_COMMAND_SURFACE.md as part of a TD5 rung. \
             (TD5c moved `same_ty`/`same_field` — mis-shelved `builder.*`-spelled identity \
             reads — off the bespoke executor onto the typed `ReflectionCompare` table, 45 → 43. \
             DEVX-A dropped the four signature-dance ops `method`/`with_self`/`with_arg`/`returns` \
             — the emitted method's signature is inferred from the goal trait's declaration at \
             `emit_method(name, body)`, 43 → 39.)"
        );
    }

    #[test]
    fn recognized_reflect_ops_match_dispatch() {
        // The reflect/field/variant method arms live between the
        // `eval_method_call` freeze comment and the start of
        // `eval_builder_method`.
        let body = slice_between(
            "// FREEZE (TD5.0): the reflection-read method names below",
            "fn eval_builder_method(",
        );
        let mut dispatch = arm_keys(body);
        // `eval_method_call`'s arms repeat no names except across receiver
        // blocks (none here), so a plain sort/dedup yields the recognized set.
        dispatch = sorted_unique(dispatch);
        let canonical = sorted_unique(
            RECOGNIZED_REFLECT_OPS
                .iter()
                .map(|s| s.to_string())
                .collect(),
        );
        assert_eq!(
            dispatch, canonical,
            "FREEZE (TD5.0): eval_method_call reflection-read arms drifted from \
             RECOGNIZED_REFLECT_OPS — see docs/dev/TD5_PROVIDER_COMMAND_SURFACE.md."
        );
        assert!(
            RECOGNIZED_REFLECT_OPS.is_empty(),
            "FREEZE (TD5.0): the reflection-read surface changed size; update the count \
             and docs/dev/TD5_PROVIDER_COMMAND_SURFACE.md as part of a TD5 rung. \
             (TD5c moved ALL non-iterating reflection reads off the bespoke executor onto \
             the typed read-only handles `ReflectHandle`/`FieldHandle`/`VariantHandle`; \
             only the `for`-iterables remain executor-owned.)"
        );
    }

    #[test]
    fn iterable_reads_are_off_the_executor() {
        // TD5c: the reflection iterables are ordinary method calls now — there
        // is no longer an iterable-expression interception or an iterable-ops
        // const. Pin both deletions structurally so neither can creep back
        // without tripping this test.
        //
        // The needles are assembled at runtime so they do not appear verbatim
        // in this file's own embedded `SOURCE` (which would defeat the scan, as
        // it did for the deleted markers).
        let interception_fn = format!("fn eval_{}", "iterable");
        assert!(
            !SOURCE.contains(&interception_fn),
            "FREEZE (TD5.0): the iterable-expression interception `{interception_fn}` is back; \
             the reflection iterables must stay ordinary method calls returning a `Value::Seq` \
             — see docs/dev/TD5_PROVIDER_COMMAND_SURFACE.md."
        );
        let iterable_ops_const = format!("RECOGNIZED_{}_OPS", "ITERABLE");
        assert!(
            !SOURCE.contains(&iterable_ops_const),
            "FREEZE (TD5.0): the const `{iterable_ops_const}` is back; the iterable surface is \
             gone — see docs/dev/TD5_PROVIDER_COMMAND_SURFACE.md."
        );
    }

    #[test]
    fn total_recognized_method_surface_is_pinned() {
        // The full named method surface the freeze pins: 39 builder ops + 0
        // reflection reads = 39 distinct literals. TD5c first moved the three
        // `reflect.*` scalar reads onto `ReflectHandle` (54 → 51), then moved
        // the `field.*`/`variant.*` reads onto `FieldHandle`/`VariantHandle`
        // (RECOGNIZED_REFLECT_OPS → 0) and `same_ty`/`same_field` onto
        // `ReflectionCompare` (builder 45 → 43) (51 → 45), then this slice made
        // the `fields`/`variants` iterables ordinary method calls returning a
        // `Value::Seq` of handles (the iterable-ops const deleted; 45 → 43).
        // DEVX-A then dropped the four signature-dance ops (`method`/`with_self`/
        // `with_arg`/`returns`) — the emitted method's signature is inferred from
        // the goal trait's declaration at `emit_method(name, body)` (43 → 39).
        // `builder.*` is the executor's only named surface.
        let total = RECOGNIZED_BUILDER_OPS.len() + RECOGNIZED_REFLECT_OPS.len();
        assert_eq!(
            total, 39,
            "FREEZE (TD5.0): the recognized command surface changed size. A new op requires \
             a TD5 category decision — see docs/dev/TD5_PROVIDER_COMMAND_SURFACE.md."
        );
    }

    /// TD5.x deletion guard: `BuilderCommand` is GONE. Every provider effect —
    /// requirements AND emitted members — now rides the typed
    /// [`super::ProviderEffect`] trace, which is the sole replay authority. This
    /// exhaustive match fails to compile if the bespoke command replay is
    /// reintroduced under a new name, or if an effect family is dropped.
    #[test]
    fn all_effects_ride_the_typed_trace() {
        fn _assert_effect_families(effect: &super::ProviderEffect<'_>) {
            match effect {
                super::ProviderEffect::Require { .. }
                | super::ProviderEffect::EmitMethod { .. }
                | super::ProviderEffect::EmitAssocTy { .. }
                | super::ProviderEffect::EmitConst { .. } => {}
            }
        }
        // `BuilderCommand` no longer exists; there is no parallel command
        // language for synthesis to read. The source-level freeze scan below
        // (in the `command_surface_freeze` module) keeps the named op surface
        // pinned independently of this type-level guard.
    }
}

#[cfg(test)]
mod effect_trace {
    //! TD5.1 observability: the typed [`super::ProviderEffect`] trace is the
    //! internal, dumpable seam recording the provider effects that re-enter
    //! ordinary compilation. It lands PAIRED with the TD5.2 require migration
    //! (the ratchet rule), not as a standalone shim.

    use super::{FieldKey, ProviderBodies, ProviderEffect, ProviderOutput, ProviderSkeleton};
    use crate::{HirDb, hir_def::PathId, test_db::TestDb};

    #[test]
    fn require_effect_dumps_with_field_provenance() {
        let db = TestDb::default();
        let db: &dyn HirDb = &db;

        let ty = crate::hir_def::TypeId::new(
            db,
            crate::hir_def::TypeKind::Path(crate::hir_def::Partial::Present(PathId::from_ident(
                db,
                crate::hir_def::IdentId::new(db, "u256".to_string()),
            ))),
        );
        let trait_path = PathId::from_ident(db, crate::hir_def::IdentId::new(db, "Eq".to_string()));

        let output = ProviderOutput {
            skeleton: ProviderSkeleton {
                effects: vec![
                    ProviderEffect::Require {
                        ty,
                        trait_path,
                        field_origin: Some(FieldKey {
                            variant: None,
                            index: 0,
                        }),
                    },
                    ProviderEffect::Require {
                        ty,
                        trait_path,
                        field_origin: None,
                    },
                ],
                sigs: vec![],
                tys: vec![],
            },
            bodies: ProviderBodies {
                exprs: vec![],
                pats: vec![],
            },
        };

        let dump = output.dump_effects(db);
        // The effect IR records BOTH the require and (cheaply) its reflected
        // field origin — the observability paired with the migration.
        assert_eq!(
            dump,
            "require u256: Eq (field None.0)\nrequire u256: Eq\n",
            "TD5.1 effect trace must render each require (with field provenance \
             when available)"
        );
    }
}

#[cfg(test)]
mod qualified_path {
    //! DEVX-B R1: the quote-body `<Ty as Trait>::item` shape recognizer.

    use super::qualified_path_parts;
    use crate::{
        HirDb,
        hir_def::{
            GenericArg, GenericArgListId, IdentId, Partial, PathId, PathKind, TraitRefId, TypeId,
            TypeKind,
        },
        test_db::TestDb,
    };

    fn path_ty<'db>(db: &'db dyn HirDb, name: &str) -> TypeId<'db> {
        TypeId::new(
            db,
            TypeKind::Path(Partial::Present(PathId::from_str(db, name))),
        )
    }

    #[test]
    fn recognizes_qualified_const_access() {
        let db = TestDb::default();
        let db: &dyn HirDb = &db;

        let ty = path_ty(db, "Point");
        let trait_ = TraitRefId::new(db, Partial::Present(PathId::from_str(db, "HasK")));
        // `<Point as HasK>::K`
        let path = PathId::new(
            db,
            PathKind::QualifiedType { type_: ty, trait_ },
            None,
        )
        .push_str(db, "K");

        let (got_ty, got_trait, got_name) =
            qualified_path_parts(db, path).expect("a `<Ty as Trait>::item` path is recognized");
        assert_eq!(got_ty, ty);
        assert_eq!(got_trait, trait_);
        assert_eq!(got_name, IdentId::new(db, "K".to_string()));
    }

    #[test]
    fn bare_ident_is_not_qualified() {
        let db = TestDb::default();
        let db: &dyn HirDb = &db;
        let path = PathId::from_str(db, "K");
        assert!(qualified_path_parts(db, path).is_none());
    }

    #[test]
    fn item_segment_with_generic_args_is_rejected() {
        let db = TestDb::default();
        let db: &dyn HirDb = &db;
        let ty = path_ty(db, "Point");
        let trait_ = TraitRefId::new(db, Partial::Present(PathId::from_str(db, "HasK")));
        let args = GenericArgListId::given(
            db,
            vec![GenericArg::Type(crate::hir_def::TypeGenericArg {
                ty: Partial::Present(path_ty(db, "u256")),
            })],
        );
        // `<Point as HasK>::K<u256>` — a generic-arg-bearing final segment is
        // not the bare associated-item form this grammar accepts.
        let path = PathId::new(
            db,
            PathKind::QualifiedType { type_: ty, trait_ },
            None,
        )
        .push_str_args(db, "K", args);
        assert!(qualified_path_parts(db, path).is_none());
    }
}
