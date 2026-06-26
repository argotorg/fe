//! Replay of the derive-provider effect trace into real HIR.
//!
//! The executor ([`super::provider_executor`]) records what a provider
//! *wants* (requirements, emitted methods/consts/assoc-types) as a typed
//! [`ProviderEffect`] trace. This module replays those effects through the
//! same [`HirBuilder`]/[`BodyBuilder`] synthesis vocabulary used by the
//! `#[event]`/`#[error]` desugarings, producing an ordinary
//! `impl Trait for Target` item in the expansion stage's scope graph. The
//! trace is the sole replay authority: `Require` effects become where-clause
//! predicates, the `Emit*` effects become impl members (in push order).
//!
//! No derive shape knowledge lives here: which fields are compared, how
//! variant matches nest, what the method bodies look like — all of that is
//! decided by the provider's Fe code. This module only knows how to map
//! each generated-expression node onto the corresponding HIR expression.

use num_bigint::BigUint;

use super::{
    hir_builder::{BodyBuilder, HirBuilder},
    provider::{FieldName, ReflectedVariantKind, TargetReflection},
    provider_executor::{
        FieldKey, GenExpr, GenExprId, GenPat, GenPatId, GenTy, GenTyId, ProviderEffect,
        ProviderOutput,
    },
};
use crate::{
    HirDb,
    hir_def::{
        ArithBinOp, AssocConstDef, AssocTyDef, BinOp, CompBinOp, Expr, ExprId, Field, FieldIndex,
        FuncModifiers, GenericArg, IdentId, IntegerId, LitKind, LogicalBinOp, MatchArm, Partial,
        PathId, PathKind, RecordPatField, TraitRefId, TupleTypeId, TypeBound, TypeId, TypeKind,
        Visibility, WhereClauseId, WherePredicate,
    },
    span::DeriveDesugared,
};

/// Replays `output` into an `impl Trait for Target` item. `trait_ref` is the
/// canonical trait reference from the selected provider; `self_ty` is the
/// target's name applied to its own generic params; `generics` carries the
/// target's generic parameters and inherited predicates.
pub(super) fn synthesize_provider_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, DeriveDesugared>,
    target_name: IdentId<'db>,
    self_ty: TypeId<'db>,
    generics: &super::derive::DeriveGenerics<'db>,
    reflection: &TargetReflection<'db>,
    trait_ref: TraitRefId<'db>,
    output: &ProviderOutput<'db>,
) {
    let db = builder.db();
    let where_clause = requirement_where_clause(db, generics, output);
    let replay = ReplayCtxt {
        target_name,
        trait_ref,
        reflection,
        output,
    };

    builder.impl_trait_generic_assocs_build(
        trait_ref,
        self_ty,
        generics.impl_params,
        where_clause,
        |builder| {
            let mut types = Vec::new();
            let mut consts = Vec::new();
            // Replay the emit effects in push order (TD5.x): the executor
            // recorded these in body-execution order, so iterating `effects`
            // and filtering the emit variants reproduces the exact member order
            // the old `output.commands` walk produced. `Require` effects belong
            // to the where-clause pass and are skipped here.
            for effect in &output.skeleton.effects {
                match effect {
                    ProviderEffect::EmitAssocTy { name, ty } => {
                        let ty = replay.materialize_ty(builder, *ty);
                        types.push(AssocTyDef {
                            attributes: builder.empty_attrs(),
                            name: Partial::Present(*name),
                            type_ref: Partial::Present(ty),
                        });
                    }
                    ProviderEffect::EmitConst { name, ty, value } => {
                        let ty = replay.materialize_ty(builder, *ty);
                        let value_expr = *value;
                        let value_body = builder
                            .anonymous_expr_body(|body| replay.replay_expr(body, value_expr));
                        consts.push(AssocConstDef {
                            attributes: builder.empty_attrs(),
                            name: Partial::Present(*name),
                            ty: Partial::Present(ty),
                            value: Partial::Present(value_body),
                            vis: crate::hir_def::Visibility::Public,
                        });
                    }
                    ProviderEffect::EmitMethod { .. } | ProviderEffect::Require { .. } => {}
                }
            }
            (types, consts)
        },
        |builder| {
            // Same push-order replay as the assoc/const pass: filter the
            // `EmitMethod` effects out of the trace, preserving the order the
            // executor recorded them in.
            for effect in &output.skeleton.effects {
                let ProviderEffect::EmitMethod { sig, body } = effect else {
                    continue;
                };
                let sig = &output.skeleton.sigs[sig.0];

                let mut params = Vec::new();
                if sig.takes_self {
                    params.push(builder.param_view_self());
                }
                for (name, ty) in &sig.args {
                    params.push(builder.param_underscore_named(*name, *ty));
                }
                let params = builder.params(params);

                let body_expr = *body;
                builder.func_with_body_inline_always(
                    sig.name,
                    builder.empty_generic_params(),
                    params,
                    sig.ret,
                    FuncModifiers::new(Visibility::Private, false, false, false),
                    move |body| {
                        let result = replay.replay_expr(body, body_expr);
                        body.emit_return(Some(result));
                    },
                );
            }
        },
    );
}

/// The where clause of a provider-generated impl: the target's own predicates,
/// plus one `ty: Trait` predicate for every **generic-param** `require<Trait>(ty)`
/// effect the provider issued.
///
/// TD5.2 (the require migration). Two things changed from the old executor-owned
/// path, and one thing did NOT (and must not — see the W4/ambiguity note below):
///
/// 1. SOURCE: the requirement no longer comes from the executor's bespoke
///    `BuilderCommand::Require` (deleted) walked by this function. It now comes
///    from the typed [`ProviderEffect::Require`] trace — the obligation re-enters
///    ordinary compilation as a provider effect, not an executor command. The
///    executor no longer OWNS generated trait requirements.
/// 2. SHAPE: the requirement is emitted on the **whole** require type (no
///    per-param decomposition of composites); a requirement on `Pair<T, bool>`
///    becomes `where Pair<T, bool>: Trait`, not the old `where T: Trait`.
///
/// W4 (the solve-line) — what stays. The predicate is the bounded form (subject
/// `ty`, bound `Trait`). It is collected by the SAME constraint machinery as a
/// hand-written `where ty: Trait` (`collect_decl_constraints` → `Deferred::Bound`
/// → `lower_trait_ref`), which lowers `ty` to a concrete `TyId` and builds a
/// concrete `TraitInstId`. `ty` is whatever `field.ty()` produced — a
/// `TypeKind::Path`/tuple/etc., never a `* -> Constraint` head — so **no live
/// `P` ever reaches the solver**. A generic-param `ty` (e.g. `T`) becomes
/// `where T: Trait`, concrete at each instantiation.
///
/// CONCRETE requirements are NOT emitted as predicates (the
/// `requirement_mentions_param` gate). This is not the old "silent drop": the
/// concrete obligation IS enforced — it is discharged at the generated body's
/// USE site (`<NoAbi as AbiSize>::HEAD_SIZE`, `self.field.eq(..)`, …), where a
/// missing impl fails through the normal `6-0003` trait-bound diagnostic (the W6
/// const-ref fix, #42, ensures this for assoc-const reads instead of an ICE).
/// Emitting a where-predicate on a concrete `ty` whose impl is in scope is
/// actively WRONG: it adds a duplicate method-resolution candidate (the bare
/// where-assumption alongside the real impl), which the solver reports as
/// `8-0026` "multiple trait candidates" for `self.field.<method>()` in the
/// generated body (observed on array/tuple field types).
///
/// PS-MR framing-2 (ratified by the architect 2026-06-24) IS this gate: a
/// fully concrete require must NOT become a param-env assumption. Its
/// observable contract is realized here: no added method-resolution candidate
/// (no `8-0026`), and a missing concrete impl fails through the normal
/// `6-0003` trait-bound diagnostic at the body use site (pinned by
/// `derived_abi_size_missing_field_bound` / `abi_size_concrete_missing_field`).
/// PROACTIVELY verifying a concrete require the body never uses at a call site
/// would need analysis to read the provider's `Require` effects, but
/// `ProviderOutput` is expansion-stage (`validate_impl_provider` is `HirDb`),
/// so that crossing is the deferred staged-generation (x-3d) work, tracked
/// under SGK, not this slice. The general assumption-vs-impl dedup (framing-1)
/// is separately deferred, witness-keyed on `ImplementorId`.
fn requirement_where_clause<'db>(
    db: &'db dyn HirDb,
    generics: &super::derive::DeriveGenerics<'db>,
    output: &ProviderOutput<'db>,
) -> WhereClauseId<'db> {
    let param_names: Vec<IdentId<'db>> = generics
        .param_tys
        .iter()
        .filter_map(|param_ty| ty_as_bare_ident(db, *param_ty))
        .collect();

    let mut preds = generics.inherited_preds.clone();
    let mut seen: Vec<(TypeId<'db>, PathId<'db>)> = Vec::new();
    for effect in &output.skeleton.effects {
        // The trace interleaves requirements and emitted members; this pass
        // owns only `Require` (the emit variants are replayed in
        // `synthesize_provider_impl`'s member closures). Filtering here keeps
        // the two readers of the single trace from double-processing.
        let ProviderEffect::Require { ty, trait_path, .. } = effect else {
            continue;
        };
        // Concrete requirements are discharged at the generated body's use site,
        // not as a predicate (see the doc comment): only generic-param
        // requirements become where-predicates.
        if !ty_mentions_param(db, *ty, &param_names) {
            continue;
        }
        if seen.contains(&(*ty, *trait_path)) {
            continue;
        }
        seen.push((*ty, *trait_path));
        preds.push(WherePredicate {
            ty: Partial::Present(*ty),
            bounds: vec![TypeBound::Trait(TraitRefId::new(
                db,
                Partial::Present(*trait_path),
            ))],
        });
    }

    WhereClauseId::new(db, preds, vec![])
}

/// The identifier of `ty` when it is a bare single-segment path type.
fn ty_as_bare_ident<'db>(db: &'db dyn HirDb, ty: TypeId<'db>) -> Option<IdentId<'db>> {
    match ty.data(db) {
        TypeKind::Path(path) => path.to_opt()?.as_ident(db),
        _ => None,
    }
}

/// Whether `ty` syntactically mentions any of the target's generic parameter
/// names — i.e. whether the requirement is generic (vs fully concrete).
fn ty_mentions_param<'db>(db: &'db dyn HirDb, ty: TypeId<'db>, params: &[IdentId<'db>]) -> bool {
    if params.is_empty() {
        return false;
    }
    match ty.data(db) {
        TypeKind::Path(path) => path
            .to_opt()
            .is_some_and(|path| path_mentions_param(db, path, params)),
        TypeKind::Ptr(inner) | TypeKind::Mode(_, inner) => inner
            .to_opt()
            .is_some_and(|inner| ty_mentions_param(db, inner, params)),
        TypeKind::Array(elem, _len) => elem
            .to_opt()
            .is_some_and(|elem| ty_mentions_param(db, elem, params)),
        TypeKind::Tuple(tuple) => tuple.data(db).iter().any(|elem| {
            elem.to_opt()
                .is_some_and(|elem| ty_mentions_param(db, elem, params))
        }),
        TypeKind::Never => false,
    }
}

fn path_mentions_param<'db>(
    db: &'db dyn HirDb,
    path: PathId<'db>,
    params: &[IdentId<'db>],
) -> bool {
    // A bare single-segment occurrence of a parameter name.
    if path.parent(db).is_none()
        && let PathKind::Ident {
            ident,
            generic_args,
        } = path.kind(db)
        && generic_args.is_empty(db)
        && ident.to_opt().is_some_and(|ident| params.contains(&ident))
    {
        return true;
    }
    // A parameter mentioned in the generic arguments of any segment.
    let mut cursor = Some(path);
    while let Some(p) = cursor {
        if let PathKind::Ident { generic_args, .. } = p.kind(db) {
            for arg in generic_args.data(db) {
                if let GenericArg::Type(type_arg) = arg
                    && type_arg
                        .ty
                        .to_opt()
                        .is_some_and(|ty| ty_mentions_param(db, ty, params))
                {
                    return true;
                }
            }
        }
        cursor = p.parent(db);
    }
    false
}

/// Shared replay context for one generated method body.
#[derive(Clone, Copy)]
struct ReplayCtxt<'a, 'db> {
    target_name: IdentId<'db>,
    trait_ref: TraitRefId<'db>,
    reflection: &'a TargetReflection<'db>,
    output: &'a ProviderOutput<'db>,
}

impl<'a, 'db> ReplayCtxt<'a, 'db> {
    fn replay_expr(
        &self,
        body: &mut BodyBuilder<'_, 'db, DeriveDesugared>,
        expr: GenExprId,
    ) -> ExprId {
        let db = body.db();
        match &self.output.bodies.exprs[expr.0] {
            GenExpr::Bool(value) => body.bool_lit_expr(*value),
            GenExpr::And(lhs, rhs) => {
                let lhs = self.replay_expr(body, *lhs);
                let rhs = self.replay_expr(body, *rhs);
                body.push_expr(Expr::Bin(lhs, rhs, BinOp::Logical(LogicalBinOp::And)))
            }
            GenExpr::Or(lhs, rhs) => {
                let lhs = self.replay_expr(body, *lhs);
                let rhs = self.replay_expr(body, *rhs);
                body.push_expr(Expr::Bin(lhs, rhs, BinOp::Logical(LogicalBinOp::Or)))
            }
            GenExpr::Add(lhs, rhs) => {
                let lhs = self.replay_expr(body, *lhs);
                let rhs = self.replay_expr(body, *rhs);
                body.push_expr(Expr::Bin(lhs, rhs, BinOp::Arith(ArithBinOp::Add)))
            }
            GenExpr::SelfRef => body.path_expr(PathId::from_ident(db, IdentId::make_self(db))),
            GenExpr::ArgRef(name) => body.ident_expr(*name),
            GenExpr::FieldGet(base, field) => {
                let base = self.replay_expr(body, *base);
                let index = self.field_index(db, *field);
                body.push_expr(Expr::Field(base, Partial::Present(index)))
            }
            GenExpr::EqCmp(lhs, rhs) => {
                let lhs = self.replay_expr(body, *lhs);
                let rhs = self.replay_expr(body, *rhs);
                body.push_expr(Expr::Bin(lhs, rhs, BinOp::Comp(CompBinOp::Eq)))
            }
            GenExpr::LtCmp(lhs, rhs) => {
                let lhs = self.replay_expr(body, *lhs);
                let rhs = self.replay_expr(body, *rhs);
                body.push_expr(Expr::Bin(lhs, rhs, BinOp::Comp(CompBinOp::Lt)))
            }
            GenExpr::GtCmp(lhs, rhs) => {
                let lhs = self.replay_expr(body, *lhs);
                let rhs = self.replay_expr(body, *rhs);
                body.push_expr(Expr::Bin(lhs, rhs, BinOp::Comp(CompBinOp::Gt)))
            }
            GenExpr::TraitCall { ty, method, args } => {
                let callee_path = self.goal_item_path(db, *ty, *method);
                let callee = body.path_expr(callee_path);
                let args = args
                    .iter()
                    .map(|arg| self.replay_expr(body, *arg))
                    .collect();
                body.call_expr(callee, args)
            }
            GenExpr::TraitConst { ty, name } => {
                let path = self.goal_item_path(db, *ty, *name);
                body.path_expr(path)
            }
            GenExpr::QualifiedConst {
                ty,
                trait_path,
                name,
            } => {
                // `<ty as CanonicalTrait>::name` — an arbitrary (non-goal) trait
                // const. The canonical trait path resolves in the generated
                // impl's scope; unlike `TraitConst` there is no `Self` shorthand.
                let trait_ref = TraitRefId::new(db, Partial::Present(*trait_path));
                let path = PathId::new(
                    db,
                    PathKind::QualifiedType {
                        type_: *ty,
                        trait_: trait_ref,
                    },
                    None,
                )
                .push_ident(db, *name);
                body.path_expr(path)
            }
            GenExpr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let receiver = self.replay_expr(body, *receiver);
                let args = args
                    .iter()
                    .map(|arg| self.replay_expr(body, *arg))
                    .collect();
                body.method_call_expr(receiver, *method, args)
            }
            GenExpr::StaticCall { path, args } => {
                let callee = body.path_expr(*path);
                let args = args
                    .iter()
                    .map(|arg| self.replay_expr(body, *arg))
                    .collect();
                body.call_expr(callee, args)
            }
            GenExpr::StrLit(value) => body.push_expr(Expr::Lit(LitKind::String(*value))),
            GenExpr::Tuple(elems) => {
                let elems = elems
                    .iter()
                    .map(|elem| self.replay_expr(body, *elem))
                    .collect();
                body.push_expr(Expr::Tuple(elems))
            }
            GenExpr::Keccak(arg) => {
                let arg = self.replay_expr(body, *arg);
                body.core_keccak_call(arg)
            }
            GenExpr::StructInit { fields } => {
                let field_inits = fields
                    .iter()
                    .map(|(field, value)| {
                        let expr = self.replay_expr(body, *value);
                        Field {
                            label: self.field_label(*field),
                            expr,
                        }
                    })
                    .collect();
                let self_path = Partial::Present(PathId::from_ident(db, IdentId::make_self_ty(db)));
                body.push_expr(Expr::RecordInit(self_path, field_inits))
            }
            GenExpr::VariantInit { variant, fields } => {
                let variant_path = self.variant_path(db, *variant);
                let kind = self
                    .reflection
                    .variant(*variant)
                    .map(|v| v.kind)
                    .unwrap_or(ReflectedVariantKind::Unit);
                match kind {
                    ReflectedVariantKind::Unit => body.path_expr(variant_path),
                    ReflectedVariantKind::Tuple => {
                        let mut ordered = fields.clone();
                        ordered.sort_by_key(|(field, _)| field.index);
                        let args = ordered
                            .iter()
                            .map(|(_, value)| self.replay_expr(body, *value))
                            .collect();
                        let callee = body.path_expr(variant_path);
                        body.call_expr(callee, args)
                    }
                    ReflectedVariantKind::Record => {
                        let field_inits = fields
                            .iter()
                            .map(|(field, value)| {
                                let expr = self.replay_expr(body, *value);
                                Field {
                                    label: self.field_label(*field),
                                    expr,
                                }
                            })
                            .collect();
                        body.push_expr(Expr::RecordInit(
                            Partial::Present(variant_path),
                            field_inits,
                        ))
                    }
                }
            }
            GenExpr::Match { scrutinee, arms } => {
                let scrutinee = self.replay_expr(body, *scrutinee);
                let arms = arms
                    .iter()
                    .map(|(pat, arm_body)| {
                        let pat = self.replay_pat(body, *pat);
                        let arm_body = self.replay_expr(body, *arm_body);
                        MatchArm {
                            pat,
                            body: arm_body,
                        }
                    })
                    .collect();
                body.match_expr(scrutinee, arms)
            }
            GenExpr::VariantBinder {
                variant,
                field,
                prefix,
            } => {
                let binder = self.binder_ident(
                    db,
                    *prefix,
                    FieldKey {
                        variant: Some(*variant),
                        index: *field,
                    },
                );
                body.ident_expr(binder)
            }
        }
    }

    fn replay_pat(
        &self,
        body: &mut BodyBuilder<'_, 'db, DeriveDesugared>,
        pat: GenPatId,
    ) -> crate::hir_def::PatId {
        let db = body.db();
        match &self.output.bodies.pats[pat.0] {
            GenPat::Wildcard => body.wildcard_pat(),
            GenPat::Variant { variant, prefix } => {
                let variant_path = self.variant_path(db, *variant);
                let Some(reflected) = self.reflection.variant(*variant) else {
                    return body.wildcard_pat();
                };
                match reflected.kind {
                    ReflectedVariantKind::Unit => body.path_pat(variant_path),
                    ReflectedVariantKind::Tuple => {
                        let elems = reflected
                            .fields
                            .iter()
                            .map(|field| {
                                let binder = self.binder_ident(
                                    db,
                                    *prefix,
                                    FieldKey {
                                        variant: Some(*variant),
                                        index: field.index,
                                    },
                                );
                                body.bind_pat(binder)
                            })
                            .collect();
                        body.path_tuple_pat(variant_path, elems)
                    }
                    ReflectedVariantKind::Record => {
                        let fields = reflected
                            .fields
                            .iter()
                            .map(|field| {
                                let binder = self.binder_ident(
                                    db,
                                    *prefix,
                                    FieldKey {
                                        variant: Some(*variant),
                                        index: field.index,
                                    },
                                );
                                let pat = body.bind_pat(binder);
                                RecordPatField {
                                    label: match field.name {
                                        FieldName::Named(name) => Partial::Present(name),
                                        FieldName::Positional(_) => Partial::Absent,
                                    },
                                    pat,
                                }
                            })
                            .collect();
                        body.record_pat(variant_path, fields)
                    }
                }
            }
        }
    }

    /// The path of an associated item of the goal trait on `ty`:
    /// `<ty as Trait>::name`, or `Self::name` when `ty` is the `Self` type
    /// (resolving through the surrounding impl, which implements the goal
    /// trait by construction).
    fn goal_item_path(
        &self,
        db: &'db dyn HirDb,
        ty: TypeId<'db>,
        name: IdentId<'db>,
    ) -> PathId<'db> {
        if ty == TypeId::fallback_self_ty(db) {
            return PathId::from_ident(db, IdentId::make_self_ty(db)).push_ident(db, name);
        }
        PathId::new(
            db,
            PathKind::QualifiedType {
                type_: ty,
                trait_: self.trait_ref,
            },
            None,
        )
        .push_ident(db, name)
    }

    /// Materializes a generated type into a real [`TypeId`]. Exact-width
    /// string types need an anonymous const-argument body, hence the
    /// builder.
    fn materialize_ty(
        &self,
        builder: &mut HirBuilder<'_, 'db, DeriveDesugared>,
        ty: GenTyId,
    ) -> TypeId<'db> {
        let db = builder.db();
        match &self.output.skeleton.tys[ty.0] {
            GenTy::StringN(len) => builder.string_n_ty(*len),
            GenTy::Tuple(elems) => {
                let elems: Vec<Partial<TypeId<'db>>> = elems
                    .iter()
                    .map(|elem| Partial::Present(self.materialize_ty(builder, *elem)))
                    .collect();
                TupleTypeId::new(db, elems).to_ty(db)
            }
            GenTy::Projection { ty, name } => {
                let path = self.goal_item_path(db, *ty, *name);
                TypeId::new(db, TypeKind::Path(Partial::Present(path)))
            }
            GenTy::Concrete(ty) => *ty,
        }
    }

    fn variant_path(&self, db: &'db dyn HirDb, variant: usize) -> PathId<'db> {
        let base = PathId::from_ident(db, self.target_name);
        match self.reflection.variant(variant) {
            Some(reflected) => base.push_ident(db, reflected.name),
            None => base,
        }
    }

    fn field_label(&self, field: FieldKey) -> Option<IdentId<'db>> {
        match self
            .reflection
            .field(field.variant, field.index)
            .map(|f| f.name)
        {
            Some(FieldName::Named(name)) => Some(name),
            _ => None,
        }
    }

    fn field_index(&self, db: &'db dyn HirDb, field: FieldKey) -> FieldIndex<'db> {
        match self
            .reflection
            .field(field.variant, field.index)
            .map(|f| f.name)
        {
            Some(FieldName::Named(name)) => FieldIndex::Ident(name),
            Some(FieldName::Positional(idx)) => {
                FieldIndex::Index(IntegerId::new(db, BigUint::from(idx)))
            }
            None => FieldIndex::Index(IntegerId::new(db, BigUint::from(field.index))),
        }
    }

    /// The binder name a [`GenPat::Variant`] pattern introduces for `field`:
    /// `{prefix}_{name}` for record fields, `{prefix}_{index}` for tuple
    /// fields.
    fn binder_ident(
        &self,
        db: &'db dyn HirDb,
        prefix: IdentId<'db>,
        field: FieldKey,
    ) -> IdentId<'db> {
        let suffix = match self
            .reflection
            .field(field.variant, field.index)
            .map(|f| f.name)
        {
            Some(FieldName::Named(name)) => name.data(db).to_string(),
            Some(FieldName::Positional(idx)) => idx.to_string(),
            None => field.index.to_string(),
        };
        IdentId::new(db, format!("{}_{}", prefix.data(db), suffix))
    }
}
