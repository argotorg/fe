use either::Either;
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::ToPrimitive;
use rustc_hash::FxHashMap;
use smallvec1::SmallVec;

use crate::core::{
    hir_def::{
        ArithBinOp, BinOp, BindingMarker, CallArg as HirCallArg, CallableDef, CompBinOp, Cond,
        CondId, Expr, ExprId, FieldIndex, Func, FuncParam, FuncParamListId, FuncParamMode, IdentId,
        IntegerId, LitKind, LogicalBinOp, ModeMarker, Partial, Pat, PatId, PathId, Stmt, StmtId,
        TypeId as HirTyId, TypeKind, TypeMode, UnOp, VariantKind, WithBinding,
    },
    semantic::{EffectRequirement, EffectRequirementKey as SemanticEffectRequirementKey},
};
use crate::span::{DynLazySpan, types::LazyTySpan};

use super::{
    BodyOwner, BuiltinOp, CastLayers, CodeRegionIntrinsicKind, ConstIntrinsicKind, ConstRef,
    EffectArg, EffectArgTarget, RecordLike, SemanticExprLowering, SumShapeOp, Typeable,
    ValuePathRef,
    effect_env::{FrameLookupResult, MatchedKeyedEntry},
    env::{
        BindingAccess, ClosureExpectation, EffectOrigin, EffectParamSite, ExprProp, LocalBinding,
        ParamSite, PendingEffects, PendingEffectsTarget, PendingNegation, Prober, ProvidedEffect,
        TraitObligation, TraitObligationOrigin, TyCheckEnv, hir_param,
    },
    merged_borrow_provider,
    path::ResolvedPathInBody,
    single_field_newtype_ty,
    stmt::ForLoopCall,
    ty_may_be_code_region_token,
    with_operand::{
        BareName, OperandClass, OperandList, OperandMeaning, OperandSite, OperandValues, SiteKind,
        classify_with_operand,
    },
};
use crate::analysis::place::{Place, PlaceBase};
use crate::analysis::semantic::{RuntimeSizeError, runtime_size_bytes};
use crate::analysis::ty::adt_def::AdtRef;
use crate::analysis::ty::{
    AddressSpace,
    assoc_const::{AssocConstUse, InherentConstUse},
    canonical::{Canonicalized, Solution},
    const_ty::{LoweringContext, instantiate_inherent_const_decl_ty},
    corelib::{
        presence_trait, resolve_core_range_types, resolve_core_trait, resolve_lib_func_path,
        resolve_lib_type_path,
    },
    diagnostics::{
        BodyDiag, FuncBodyDiag, MarkerPosition, MustUseSubject, MutabilityFix, ReadOnlyProvider,
        TraitConstraintDiag, TyDiagCollection, TyLowerDiag,
    },
    effects::{
        EffectKeyKind, EffectPatternKey, EffectQuery, EffectRequirementDecl, EffectWitness,
        ForwardedEffectKey, PatternSlotKind, StoredEffectKey, StoredTraitKey, StoredTypeKey,
        TraitKeySchema, TraitPatternKey, TypePatternKey,
        elaborate::{
            build_barrier_pattern_for_with_key,
            build_conservative_same_family_barrier_pattern_in_scope, build_effect_query_for_call,
            contains_projection_or_invalid_query_state, effect_requirement_decls_for_callable,
            finalize_stored_effect_key, query_contains_unresolved_inference,
            seed_forwarder_from_requirement,
        },
        instantiate_trait_effect_key,
        match_::{
            KeyMatchCommit, apply_key_match_commit, instantiate_trait_pattern_in,
            instantiate_trait_pattern_in_with_bindings,
        },
        normalize_effect_identity_trait, place_effect_provider_param_index_map,
        rows::{RowExpansion, RowKey, RowPath, expand_rows},
        stored_value_contains_implicit_layout_params, stored_value_contains_out_of_scope_params,
    },
    fold::{TyFoldable as _, TyFolder},
    instantiate_trait_self,
    normalize::normalize_with_trait_evidence,
    provider::provider_semantics_for_specialized_call,
    shape::{
        Shape, SumKind, is_param_product, lower_opaque_param, lower_param_product, may_name_shape,
        param_product_diags, sum_kind,
    },
    trait_def::TraitInstId,
    trait_lower::{complete_impl_trait, lower_impl_trait},
    trait_resolution::{
        GoalSatisfiability, PredicateListId, TraitGoalSolution, TraitSolveCx, is_goal_satisfiable,
    },
    ty_check::callable::{
        Callable, EffectArgProvider, EffectProviderProvenance, EffectProviderSpecialization,
    },
    ty_def::{
        BorrowKind, ClosureDef, ClosureTy, PrimTy, TyBase, TyData, TyVarSort,
        int_literal_fits_prim, prim_int_bits,
    },
    ty_error::{diag_from_invalid_cause, first_invalid_ty_cause, generic_arg_list_diags},
    unify::UnificationTable,
};
use crate::analysis::{
    HirAnalysisDb, Spanned,
    name_resolution::{
        Decision, EarlyNameQueryId, ExpectedPathKind, NameDomain, NameResBucket,
        NameResolutionError, PathRes, QueryDirective, Tier, Viewer, authored_first,
        choose_in_tiers,
        diagnostics::PathResDiag,
        method_selection::{
            AmbiguousTraitMethodCand, AmbiguousTraitMethods, MethodCandidate, MethodSelectionError,
            TraitMethodCand, select_callable_value_method, select_method_candidate_in,
            select_method_candidate_with_choices, select_trait_method_candidates,
        },
        resolve_name_res_with_minter, resolve_query_in,
    },
    place::{field_index, projectable_place_ty, resolve_place_field},
    semantic::{
        BinaryOp, CtfeConfig, EvalOutcome, Overflow, SemConstScalar, SemConstValue, SemOrigin,
        UnaryOp, eval_const_ref, force_const_term_value, instance::resolve_semantic_const_ref,
        int_const,
    },
    ty::{
        const_ty::{ConstTyId, const_ty_from_sem_const, try_eval_const_int_expr},
        normalize::normalize_ty,
        trait_resolution::ChosenImpls,
        ty_check::{RecordInitLowering, TyChecker, path::RecordInitChecker},
        ty_def::{InvalidCause, TyId},
    },
};
use crate::core::semantic::storage_slot_span;
use crate::hir_def::{BlockKind, FieldParent, ItemKind, Trait, scope_graph::ScopeId};
use common::{compilation::Target, indexmap::IndexMap};

#[derive(Debug, Clone, Copy)]
pub(super) struct KeyedWitnessBuildScope<'db> {
    pub(super) scope: ScopeId<'db>,
    pub(super) assumptions: PredicateListId<'db>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct KeyedWitnessBuildOptions<'db> {
    pub(super) scope: KeyedWitnessBuildScope<'db>,
    pub(super) emit_diag: bool,
    pub(super) mode: WitnessBuildMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum WitnessBuildMode {
    ExplicitKeyedWith,
    SeededRequirement,
}

/// A provider that satisfies an effect query.
#[derive(Debug, Clone)]
struct EffectEvidence<'db> {
    provider: ProvidedEffect<'db>,
    /// The type a type key is provided as; a trait key has none.
    target_ty: Option<TyId<'db>>,
    commit: EffectCommitPlan<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AssignLhsStatus {
    Assignable,
    Immutable,
    NonAssignable,
}

/// What `check_projected_field` made of `h.f`.
enum Projected<'db> {
    /// The type of `h` does not implement `core::ops::Project`.
    NotProject,
    /// It does, and the target type has no field `f`.
    NoSuchField(TyId<'db>),
    /// The field expression is checked, as a projection call or as an error.
    Done(ExprProp<'db>),
}

#[derive(Debug, Clone, Default)]
pub(super) struct EffectCommitPlan<'db> {
    key_match: Option<KeyMatchCommit<'db>>,
    trait_solutions: SmallVec<[(TraitInstId<'db>, Solution<TraitGoalSolution<'db>>); 2]>,
}

/// A candidate implementation of an operator trait for a binary use, with the
/// receiver type it was found for.
type OpsCandidate<'db> = (
    TyId<'db>,
    Canonicalized<'db, TyId<'db>>,
    AmbiguousTraitMethodCand<'db>,
);

/// The candidates of an operator trait for a binary use, selected in one tier.
struct OpsSelection<'db> {
    /// The candidates of every receiver type the left-hand side can be.
    candidates: Vec<OpsCandidate<'db>>,
    /// The first error of a receiver type, if one had an error.
    error: Option<MethodSelectionError<'db>>,
    /// The right-hand side, typed once the candidates are known.
    rhs: Option<ExprProp<'db>>,
    /// The candidates the use goes on with.
    viable: Vec<OpsCandidate<'db>>,
    /// Whether the right-hand side can be an argument of some candidate.
    unifies: bool,
}

impl OpsSelection<'_> {
    /// Whether this tier answers the use: it has a candidate that the
    /// right-hand side fits, or it fails in a way other than having no
    /// implementation at all.
    fn answers(&self) -> bool {
        if self.candidates.is_empty() {
            !matches!(
                self.error,
                None | Some(
                    MethodSelectionError::NotFound | MethodSelectionError::ReceiverTypeMustBeKnown
                )
            )
        } else {
            self.unifies
        }
    }
}

#[derive(Debug, Clone)]
enum EffectResolution<'db> {
    Chosen(Box<EffectEvidence<'db>>),
    BlockedByBarrier,
    Missing,
    Ambiguous,
}

/// The one viable provider of a frame; two are ambiguous, whatever their
/// names: `with (Key = value)` chooses one.
fn choose_effect_evidence<'db>(
    viable: SmallVec<[EffectEvidence<'db>; 2]>,
) -> EffectResolution<'db> {
    match viable.as_slice() {
        [only] => EffectResolution::Chosen(Box::new(only.clone())),
        _ => EffectResolution::Ambiguous,
    }
}

fn keyed_effect_evidence<'db>(matched: MatchedKeyedEntry<'db>) -> EffectEvidence<'db> {
    let (provider, target_ty, key_commit) = match matched {
        MatchedKeyedEntry::Witness(matched) => (
            matched.witness.provider,
            match matched.witness.key {
                StoredEffectKey::Type(stored) => Some(stored.carrier),
                StoredEffectKey::Trait(_) => None,
            },
            matched.key_commit,
        ),
        MatchedKeyedEntry::Forwarder(matched) => (
            matched.forwarder.provider,
            // The body's own binding provides its key directly: its type is
            // the key with the layout its caller supplied, which the key's
            // pattern leaves open.
            matches!(matched.forwarder.key, ForwardedEffectKey::Type(_))
                .then_some(matched.forwarder.provider.ty),
            matched.key_commit,
        ),
    };
    EffectEvidence {
        provider,
        target_ty,
        commit: EffectCommitPlan {
            key_match: Some(key_commit),
            trait_solutions: SmallVec::new(),
        },
    }
}

impl<'db> TyChecker<'db> {
    fn code_region_intrinsic_kind(
        &self,
        callable_def: CallableDef<'db>,
    ) -> Option<CodeRegionIntrinsicKind> {
        let CallableDef::Func(func) = callable_def else {
            return None;
        };
        let offset = resolve_lib_func_path(
            self.db,
            self.env.scope(),
            "std::evm::intrinsic::code_region_offset",
        );
        let len = resolve_lib_func_path(
            self.db,
            self.env.scope(),
            "std::evm::intrinsic::code_region_len",
        );
        Some(if Some(func) == offset {
            CodeRegionIntrinsicKind::Offset
        } else if Some(func) == len {
            CodeRegionIntrinsicKind::Len
        } else {
            return None;
        })
    }

    pub(super) fn code_region_method_kind(
        &self,
        receiver_ty: TyId<'db>,
        method_name: IdentId<'db>,
    ) -> Option<CodeRegionIntrinsicKind> {
        let evm_ty = resolve_lib_type_path(self.db, self.env.scope(), "std::evm::Evm")?;
        if receiver_ty != evm_ty {
            return None;
        }
        let name = method_name.data(self.db);
        Some(if name == "code_region_offset" {
            CodeRegionIntrinsicKind::Offset
        } else if name == "code_region_len" {
            CodeRegionIntrinsicKind::Len
        } else {
            return None;
        })
    }

    fn const_intrinsic_kind(&self, callable_def: CallableDef<'db>) -> Option<ConstIntrinsicKind> {
        let CallableDef::Func(func) = callable_def else {
            return None;
        };
        let path = |path| resolve_lib_func_path(self.db, self.env.scope(), path) == Some(func);
        if path("core::size_of") {
            Some(ConstIntrinsicKind::SizeOf)
        } else if path("core::intrinsic::storage_slots") {
            Some(ConstIntrinsicKind::StorageSlots)
        } else {
            None
        }
    }

    /// The `Option` or `Result` method `callable_def` is, if it works on a sum
    /// shape.
    fn sum_shape_op(&self, callable_def: CallableDef<'db>) -> Option<SumShapeOp> {
        let CallableDef::Func(func) = callable_def else {
            return None;
        };
        [
            (
                "core::option::Option::is_some",
                SumShapeOp::Test { payload: true },
            ),
            (
                "core::option::Option::is_none",
                SumShapeOp::Test { payload: false },
            ),
            ("core::option::Option::unwrap", SumShapeOp::Unwrap),
            (
                "core::result::Result::is_ok",
                SumShapeOp::Test { payload: true },
            ),
            (
                "core::result::Result::is_err",
                SumShapeOp::Test { payload: false },
            ),
            ("core::result::Result::unwrap", SumShapeOp::Unwrap),
        ]
        .into_iter()
        .find_map(|(path, op)| {
            (resolve_lib_func_path(self.db, self.env.scope(), path) == Some(func)).then_some(op)
        })
    }

    /// Checks `op` on `receiver`, a sum shape of `sum_ty` whose payload is
    /// `payload`. The call consumes the shape: a test ends its access, and
    /// `unwrap` grants the payload's, as `?` does, calling the core function
    /// the method's empty variant panics with.
    fn check_sum_shape_op(
        &mut self,
        expr: ExprId,
        receiver: ExprId,
        sum_ty: TyId<'db>,
        payload: Shape<'db>,
        method: Callable<'db>,
        op: SumShapeOp,
    ) -> ExprProp<'db> {
        self.consume_access(receiver);
        let (prop, panic) = match op {
            SumShapeOp::Test { .. } => (ExprProp::new(TyId::bool(self.db), true), None),
            SumShapeOp::Unwrap => {
                let panic = self.sum_shape_panic(sum_ty, expr.span(self.body()).into());
                let shape = (!matches!(payload, Shape::Owned(_))).then(|| payload.clone());
                // The payload is mutable as a call's result is.
                let prop = ExprProp {
                    is_mut: matches!(shape, None | Some(Shape::Access(BorrowKind::Mut, _))),
                    shape,
                    ..ExprProp::new(payload.erased_ty(self.db), true)
                };
                (prop, Some(panic))
            }
        };
        self.env.register_semantic_expr_lowering(
            expr,
            SemanticExprLowering::SumShape { op, method, panic },
        );
        prop
    }

    /// The core function `unwrap` of the `Option` or `Result` `sum_ty` panics
    /// with on its empty variant: `panic`, or `panic_with_value` of the error.
    fn sum_shape_panic(&mut self, sum_ty: TyId<'db>, span: DynLazySpan<'db>) -> Callable<'db> {
        let kind = sum_kind(self.db, self.env.scope(), sum_ty)
            .expect("a sum shape is an `Option` or a `Result`");
        let path = match kind {
            SumKind::Option => "core::panic",
            SumKind::Result => "core::panic_with_value",
        };
        let func = resolve_lib_func_path(self.db, self.env.scope(), path)
            .expect("core declares `panic` and `panic_with_value`");
        let func_ty = self.instantiate_to_term(TyId::func(self.db, CallableDef::Func(func)));
        let callable = Callable::new(self.db, func_ty, span, None)
            .unwrap_or_else(|_| panic!("`{path}` is callable"));
        if kind == SumKind::Result {
            self.table
                .unify(callable.generic_args()[0], sum_ty.generic_args(self.db)[0])
                .expect("`panic_with_value` takes any error");
        }
        callable
    }

    fn check_and_register_const_intrinsic(
        &mut self,
        expr: ExprId,
        callable: Callable<'db>,
        kind: ConstIntrinsicKind,
    ) -> bool {
        if let Some(ty) = callable.generic_args().first().copied() {
            let ty = self.normalize_ty(ty);
            let primary = expr.span(self.body()).into();
            match kind {
                ConstIntrinsicKind::SizeOf
                    if matches!(
                        runtime_size_bytes(self.db, ty),
                        Err(RuntimeSizeError::Overflow)
                    ) =>
                {
                    self.push_diag(BodyDiag::TypeSizeOverflow { primary, ty });
                    return false;
                }
                ConstIntrinsicKind::StorageSlots => {
                    if let Err(error) = storage_slot_span(self.db, ty) {
                        self.push_diag(BodyDiag::NoStorageLayout {
                            primary,
                            ty,
                            reason: error.summary(),
                        });
                        return false;
                    }
                }
                _ => {}
            }
        }
        self.env.register_const_intrinsic(expr, callable, kind);
        true
    }

    /// An expression's type can reach the normalization limit only once
    /// generic arguments are filled in, as in a call to
    /// `fn get<T: Tr>() -> T::Out`. The limit leaves an invalid type, which
    /// matches any type, so report it at the first expression that has it;
    /// enclosing expressions that inherit the type are not reported again,
    /// nor is a type that came from the expected type, which is reported
    /// where that type is written.
    fn report_normalization_limit(&mut self, expr: ExprId, ty: TyId<'db>, expected: TyId<'db>) {
        let is_limit =
            |ty| first_invalid_ty_cause(self.db, ty) == Some(InvalidCause::TypeNormalizationLimit);
        if !is_limit(ty) || is_limit(expected) {
            return;
        }
        let reported = self.diags.iter().any(|diag| {
            matches!(
                diag,
                FuncBodyDiag::Ty(TyDiagCollection::Ty(TyLowerDiag::TypeNormalizationLimit(_)))
            )
        });
        if !reported {
            self.push_diag(TyDiagCollection::from(TyLowerDiag::TypeNormalizationLimit(
                expr.span(self.body()).into(),
            )));
        }
    }

    pub(super) fn check_expr(&mut self, expr: ExprId, expected: TyId<'db>) -> ExprProp<'db> {
        self.check_expr_with_result_context(expr, expected, false)
    }

    pub(super) fn check_expr_unknown(&mut self, expr: ExprId) -> ExprProp<'db> {
        let t = self.fresh_ty();
        self.check_expr(expr, t)
    }

    pub(super) fn check_expr_with_discarded_result(
        &mut self,
        expr: ExprId,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let prop = self.check_expr_with_result_context(expr, expected, true);
        if !self.expr_propagates_discarded_result(expr) {
            self.check_unused_must_use(expr, prop.clone());
        }
        prop
    }

    fn check_expr_with_result_context(
        &mut self,
        expr: ExprId,
        expected: TyId<'db>,
        result_discarded: bool,
    ) -> ExprProp<'db> {
        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            let typed = ExprProp::invalid(self.db);
            self.env.type_expr(expr, typed.clone());
            return typed;
        };

        let expected = normalize_ty(self.db, expected, self.env.scope(), self.env.assumptions());

        self.env.set_never_coercion(expr, false);
        self.env.enter_expr(expr);
        let mut actual = match expr_data {
            Expr::ItemQuote(holes) => 'quote: {
                use crate::core::meta::{
                    Admit, hole_slot, quote_holes, quote_is_all_hole_stmts, quote_syntax_kinds,
                };
                use common::generation::FrozenCodeKind;
                let hole_tys = holes
                    .iter()
                    .map(|&hole| self.check_expr_unknown(hole).ty)
                    .collect::<Vec<_>>();
                let expected_resolved = expected.fold_with(self.db, &mut self.table);
                let expected_kind =
                    crate::core::meta::code_kind_from_ty(self.db, expected_resolved);
                let quote = self.quote_syntax_node(expr);
                // A quotation has type `Code<K>` only if its syntax admits
                // the kind `K`.
                if let (Some(expected_kind), Some(quote)) = (expected_kind, &quote) {
                    let kinds = quote_syntax_kinds(quote);
                    if let Some(&found) = kinds.first()
                        && !kinds.contains(&expected_kind)
                    {
                        self.push_diag(BodyDiag::QuoteKindMismatch {
                            primary: expr.span(self.body()).into(),
                            found: code_kind_name(found),
                            expected: code_kind_name(expected_kind),
                        });
                        break 'quote ExprProp::invalid(self.db);
                    }
                }
                let kind = expected_kind.unwrap_or_else(|| {
                    let Some(quote) = &quote else {
                        return FrozenCodeKind::Expr;
                    };
                    // A quotation made only of holes has the kind that all of
                    // its holes agree on.
                    if quote_is_all_hole_stmts(quote) {
                        let stmts = quote.body().map_or(0, |block| block.stmts().count());
                        if let Some(&first) = hole_tys.first()
                            && let Some(hole_kind) = crate::core::meta::code_kind_from_ty(
                                self.db,
                                first.fold_with(self.db, &mut self.table),
                            )
                            && (stmts == 1 || hole_kind != FrozenCodeKind::Pat)
                            && hole_tys[1..].iter().all(|ty| {
                                crate::core::meta::code_kind_from_ty(
                                    self.db,
                                    ty.fold_with(self.db, &mut self.table),
                                ) == Some(hole_kind)
                            })
                        {
                            return hole_kind;
                        }
                        return FrozenCodeKind::Expr;
                    }
                    quote_syntax_kinds(quote)
                        .first()
                        .copied()
                        .unwrap_or(FrozenCodeKind::Expr)
                });
                // Each splice must fit its position: the same table that
                // checks the value of the quotation when it is evaluated.
                if let Some(quote) = &quote
                    && !quote_is_all_hole_stmts(quote)
                    && let Ok(syntax_holes) = quote_holes(quote)
                    && syntax_holes.len() == hole_tys.len()
                {
                    for ((syntax_hole, &hole), ty) in syntax_holes.iter().zip(holes).zip(&hole_tys)
                    {
                        let Ok(slot) = hole_slot(quote, syntax_hole, kind) else {
                            continue;
                        };
                        let ty = ty.fold_with(self.db, &mut self.table);
                        if let Admit::No(reason) = slot.admits_ty(self.db, ty) {
                            self.push_diag(BodyDiag::QuoteSpliceMismatch {
                                primary: hole.span(self.body()).into(),
                                reason,
                            });
                            break 'quote ExprProp::invalid(self.db);
                        }
                    }
                }
                let marker_path = match kind {
                    common::generation::FrozenCodeKind::Item => "core::meta::Item",
                    common::generation::FrozenCodeKind::Expr => "core::meta::Expr",
                    common::generation::FrozenCodeKind::Pat => "core::meta::Pat",
                    common::generation::FrozenCodeKind::Arm => "core::meta::Arm",
                    common::generation::FrozenCodeKind::Where => "core::meta::Where",
                };
                let code = crate::analysis::ty::corelib::resolve_lib_type_path(
                    self.db,
                    self.env.scope(),
                    "core::meta::Code",
                );
                let marker = crate::analysis::ty::corelib::resolve_lib_type_path(
                    self.db,
                    self.env.scope(),
                    marker_path,
                );
                match (code, marker) {
                    (Some(code), Some(marker)) => {
                        ExprProp::new(TyId::app(self.db, code, marker), true)
                    }
                    _ => ExprProp::invalid(self.db),
                }
            }

            Expr::Lit(LitKind::String(string_id)) => {
                ExprProp::new(self.string_literal_ty(*string_id, expected), true)
            }
            Expr::Lit(LitKind::Int(integer)) if integer.data(self.db).bits() > 256 => {
                // No primitive integer type can represent more than one EVM word.
                // Reject before lowering can silently discard the high bits, even
                // when the literal's contextual type has not been inferred yet.
                self.push_diag(BodyDiag::IntLiteralOutOfRange {
                    primary: expr.span(self.body()).into(),
                    literal: integer.data(self.db).to_string(),
                    ty: TyId::u256(self.db),
                });
                ExprProp::invalid(self.db)
            }
            Expr::Lit(lit) => ExprProp::new(self.lit_ty_for_expected(lit, expected), true),
            Expr::Block(..) => self.check_block(expr, expr_data, expected, result_discarded),
            Expr::Closure {
                params,
                ret_ty,
                body,
            } => self.check_closure(expr, *params, *ret_ty, *body),
            Expr::Un(..) => self.check_unary(expr, expr_data),
            Expr::Cast(inner, ty) => self.check_cast(expr, *inner, *ty),
            Expr::Try(inner) => self.check_try(expr, *inner),
            Expr::Bin(lhs, rhs, op) => self.check_binary(expr, *lhs, *rhs, *op),
            Expr::Call(..) => self.check_call(expr, expr_data),
            Expr::Assert(args) => self.check_assert(expr, args),
            Expr::UnsupportedMacroCall => {
                self.push_diag(BodyDiag::UnsupportedMacroCall(
                    expr.span(self.body()).into(),
                ));
                ExprProp::invalid(self.db)
            }
            Expr::MethodCall(..) => self.check_method_call(expr, expr_data),
            Expr::Path(..) => self.check_path(expr, expr_data),
            Expr::RecordInit(..) => self.check_record_init(expr, expr_data, expected),
            Expr::Field(..) => self.check_field(expr, expr_data),
            Expr::Tuple(..) => self.check_tuple(expr, expr_data, expected),
            Expr::Array(..) => self.check_array(expr, expr_data, expected),
            Expr::ArrayRep(..) => self.check_array_rep(expr, expr_data, expected),
            Expr::If(..) => self.check_if(expr, expr_data, expected, result_discarded),
            Expr::Match(..) => self.check_match(expr, expr_data, expected, result_discarded),
            Expr::Assign(..) => self.check_assign(expr, expr_data),
            Expr::AugAssign(..) => self.check_aug_assign(expr, expr_data),
            Expr::With(bindings, body) => {
                self.check_with(bindings, *body, expected, result_discarded)
            }
        };
        self.env.leave_expr();
        // Only `ref p`, `mut p`, projection calls and a product parameter grant
        // accesses; a block (`unsafe { mut *p }`) forwards its tail's. An `if`
        // or `match` is not a place, so it grants none.
        if !matches!(
            expr_data,
            Expr::Un(_, UnOp::Ref | UnOp::Mut | UnOp::MaybeMut)
                | Expr::Path(_)
                | Expr::Call(..)
                | Expr::MethodCall(..)
                | Expr::Bin(_, _, BinOp::Index)
                | Expr::Field(..)
                | Expr::Block(..)
                | Expr::Try(..)
        ) {
            actual.shape = None;
        }

        actual.ty = normalize_ty(self.db, actual.ty, self.env.scope(), self.env.assumptions());
        self.report_normalization_limit(expr, actual.ty, expected);
        if !self.is_compile_time_body()
            && let Some(meta) = crate::core::meta::ty_contains_meta_carrier(self.db, actual.ty)
        {
            self.push_diag(BodyDiag::CompileTimePhase {
                primary: expr.span(self.body()).into(),
                message: format!(
                    "`{}` is compile-time-only in a runtime expression",
                    meta.pretty_print(self.db)
                ),
                label: BodyDiag::RUNTIME_CONTEXT_LABEL,
            });
            actual = ExprProp::invalid(self.db);
        }
        let typeable = Typeable::Expr(expr, actual.clone());
        actual.ty = self.unify_ty(typeable, actual.ty, expected);
        actual
    }

    fn expr_propagates_discarded_result(&self, expr: ExprId) -> bool {
        matches!(
            self.env.expr_data(expr),
            Partial::Present(Expr::Block(..) | Expr::If(..) | Expr::Match(..) | Expr::With(..))
        )
    }

    fn lit_ty_for_expected(&mut self, lit: &LitKind<'db>, expected: TyId<'db>) -> TyId<'db> {
        match lit {
            LitKind::String(_) if expected.is_core_dyn_string(self.db) => expected,
            _ => self.lit_ty(lit),
        }
    }

    fn check_block(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
        result_discarded: bool,
    ) -> ExprProp<'db> {
        let Expr::Block(stmts, kind) = expr_data else {
            unreachable!()
        };

        if stmts.is_empty() {
            ExprProp::new(TyId::unit(self.db), true)
        } else {
            self.env.enter_scope(expr);
            for &stmt in stmts[..stmts.len() - 1].iter() {
                self.check_discarded_stmt(stmt);
            }

            let last_stmt = stmts[stmts.len() - 1];
            let res = if result_discarded {
                let expected = if expected == TyId::unit(self.db) {
                    self.fresh_ty()
                } else {
                    expected
                };
                ExprProp::new(
                    self.check_discarded_stmt_with_expected(last_stmt, expected),
                    true,
                )
            } else if expected == TyId::unit(self.db) {
                self.check_discarded_stmt(last_stmt);
                ExprProp::new(TyId::unit(self.db), true)
            } else {
                match self.env.stmt_data(last_stmt) {
                    Partial::Present(Stmt::Expr(expr)) => self.check_expr(*expr, expected),
                    Partial::Present(_) => {
                        ExprProp::new(self.check_stmt(last_stmt, expected), true)
                    }
                    Partial::Absent => ExprProp::invalid(self.db),
                }
            };
            self.env.leave_scope();
            if *kind == BlockKind::Unsafe
                && let Partial::Present(Stmt::Expr(tail)) = self.env.stmt_data(last_stmt)
                && let Some(shape) = self.check_unsafe_split(*tail)
            {
                return ExprProp {
                    is_mut: !shape.has_access_of(self.db, BorrowKind::Ref),
                    shape: Some(shape),
                    ..res
                };
            }
            res
        }
    }

    /// The shape of an unsafe split: an `unsafe` block's tail `tail` that is a
    /// tuple of accesses grants each of them, in one session whose
    /// separation the block promises.
    fn check_unsafe_split(&mut self, tail: ExprId) -> Option<Shape<'db>> {
        let Partial::Present(Expr::Tuple(elems)) = self.env.expr_data(tail) else {
            return None;
        };
        let elems = elems.clone();
        let shapes: Vec<_> = elems
            .iter()
            .map(|elem| {
                let prop = self.env.typed_expr(*elem)?;
                Some(prop.shape.unwrap_or(Shape::Owned(prop.ty)))
            })
            .collect::<Option<_>>()?;
        if !shapes.iter().any(|shape| !matches!(shape, Shape::Owned(_))) {
            return None;
        }
        // Access components are places or calls of `#[raw_place]`
        // projections, which keep nothing for after their yield.
        for (elem, shape) in elems.iter().zip(&shapes) {
            if matches!(shape, Shape::Owned(_)) {
                continue;
            }
            let mut call = *elem;
            while let Partial::Present(Expr::Un(inner, UnOp::Mut | UnOp::MaybeMut | UnOp::Ref)) =
                self.env.expr_data(call)
            {
                call = *inner;
            }
            if let Some(CallableDef::Func(func)) = self
                .env
                .callable_expr(call)
                .map(|callable| callable.callable_def)
                && !func.is_raw_place(self.db)
                && let Some(callee) = func.name(self.db).to_opt()
            {
                self.push_diag(BodyDiag::UnsafeSplitOperand {
                    primary: elem.span(self.body()).into(),
                    callee,
                });
            }
        }
        for elem in elems {
            self.consume_access(elem);
        }
        // The split is as mutable as its grants: a `ref` one makes it read.
        let shape = Shape::Tuple(shapes);
        let prop = self.env.typed_expr(tail)?;
        self.env.type_expr(
            tail,
            ExprProp {
                is_mut: !shape.has_access_of(self.db, BorrowKind::Ref),
                shape: Some(shape.clone()),
                ..prop
            },
        );
        self.env.record_unsafe_split(tail);
        Some(shape)
    }

    fn check_discarded_stmt(&mut self, stmt: StmtId) {
        let ty = self.fresh_ty();
        self.check_discarded_stmt_with_expected(stmt, ty);
    }

    fn check_discarded_stmt_with_expected(
        &mut self,
        stmt: StmtId,
        expected: TyId<'db>,
    ) -> TyId<'db> {
        match self.env.stmt_data(stmt) {
            Partial::Present(Stmt::Expr(expr)) => {
                self.check_expr_with_discarded_result(*expr, expected).ty
            }
            Partial::Present(_) => self.check_stmt(stmt, expected),
            Partial::Absent => TyId::invalid(self.db, InvalidCause::ParseError),
        }
    }

    fn check_unused_must_use(&mut self, expr: ExprId, prop: ExprProp<'db>) {
        if prop.ty.has_invalid(self.db) {
            return;
        }

        if let Some(adt_ref) = prop.ty.adt_ref(self.db)
            && adt_ref.is_must_use(self.db)
        {
            self.push_diag(BodyDiag::UnusedMustUse {
                primary: expr.span(self.body()).into(),
                subject: MustUseSubject::Type(prop.ty),
            });
            return;
        }

        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            return;
        };
        if matches!(expr_data, Expr::Call(..) | Expr::MethodCall(..))
            && let Some(callable) = self.env.callable_expr(expr)
            && callable.callable_def().is_must_use(self.db)
        {
            self.push_diag(BodyDiag::UnusedMustUse {
                primary: expr.span(self.body()).into(),
                subject: MustUseSubject::Function(callable.callable_def()),
            });
        }
    }

    /// Checks a closure expression and its body, inline: the body sees the
    /// enclosing bindings, and each one it uses is a capture, copied or
    /// moved into the closure's environment.
    fn check_closure(
        &mut self,
        expr: ExprId,
        params: FuncParamListId<'db>,
        ret_ty: Option<HirTyId<'db>>,
        body: ExprId,
    ) -> ExprProp<'db> {
        let def = ClosureDef {
            body: self.body(),
            expr,
            mode: self.env.owner().mode(),
        };
        let params = params.data(self.db);
        let span: DynLazySpan<'db> = expr.span(self.body()).into();
        if matches!(
            self.env.owner(),
            BodyOwner::Const(_)
                | BodyOwner::AnonConstBody { .. }
                | BodyOwner::InheritedConst { .. }
        ) {
            self.push_diag(BodyDiag::ClosureInConstContext {
                primary: span.clone(),
            });
        }

        // The shape the callee the closure is passed to expects types its
        // unannotated parameters and its result in context. Against a shape
        // of another arity they are invalid, so the body does not cascade.
        let expectation = self.env.take_closure_expectation(expr).map(|expectation| {
            if expectation.params.len() == params.len() {
                return expectation;
            }
            // One pair, a product's or a tuple's, taken as two parameters.
            let untupled = matches!(&expectation.params[..], [param] if match param {
                Shape::Tuple(parts) => parts.len() == params.len(),
                _ if param.param_mode().is_some() => {
                    let ty = param.erased_ty(self.db);
                    ty.is_tuple(self.db) && ty.generic_args(self.db).len() == params.len()
                }
                _ => false,
            });
            self.push_diag(BodyDiag::ClosureArityMismatch {
                primary: span.clone(),
                expected: expectation.params.len(),
                given: params.len(),
                untupled,
            });
            let invalid = TyId::invalid(self.db, InvalidCause::Other);
            ClosureExpectation {
                params: vec![Shape::Access(BorrowKind::Ref, invalid); params.len()],
                ret: expectation.ret,
            }
        });

        self.env.enter_closure(def);
        let closure_span = expr.span(self.body()).into_closure_expr();
        let site = ParamSite::Closure(def);
        let mut param_shapes = Vec::new();
        for (idx, param) in params.iter().enumerate() {
            let expected = expectation
                .as_ref()
                .map(|expectation| &expectation.params[idx]);
            let param_span = closure_span.clone().params().param(idx);
            // A spread belongs only in a trait method's declaration.
            let shape = if param.is_spread() {
                self.push_diag(TyDiagCollection::from(TyLowerDiag::MisplacedSpread(
                    param_span.clone().into(),
                )));
                Shape::Owned(TyId::invalid(self.db, InvalidCause::Other))
            } else {
                self.closure_param(param, expected, param_span.clone().ty())
            };
            // A product parameter's binding owns the carriers of its parts,
            // which its pattern takes apart.
            // A parameter of an opaque shape takes a value of it whole.
            let mode = shape.param_mode().unwrap_or_else(|| {
                if !param.is_destructured() && !matches!(shape, Shape::Opaque(_)) {
                    self.push_diag(TyDiagCollection::from(
                        TyLowerDiag::ProductParamNotDestructured {
                            span: param_span.into(),
                            closure: true,
                        },
                    ));
                }
                self.env.register_param_product(site, idx, shape.clone());
                FuncParamMode::Own
            });
            let binding = LocalBinding::Param {
                site,
                idx,
                mode,
                ty: shape.erased_ty(self.db),
                is_mut: param.is_mut,
            };
            self.env
                .register_closure_param(param.binding_name(), binding);
            param_shapes.push(shape);
        }
        let ret = match ret_ty {
            Some(ret_ty) => self.lower_ty(ret_ty, closure_span.ret_ty(), true),
            None => expectation
                .and_then(|expectation| expectation.ret)
                .unwrap_or_else(|| self.fresh_ty()),
        };

        // The body is its own for control flow: `return` leaves the closure,
        // which is no projection, and the enclosing body's loops are not its.
        let expected = std::mem::replace(&mut self.expected, ret);
        let projection_shape = self.projection_shape.take();
        let explicit_yields = std::mem::replace(&mut self.explicit_yields, false);
        let return_borrow_provider = self.first_return_borrow_provider.take();
        let loops = self.env.swap_loop_stack(Vec::new());
        self.check_expr(body, ret);
        self.env.swap_loop_stack(loops);
        self.first_return_borrow_provider = return_borrow_provider;
        self.explicit_yields = explicit_yields;
        self.projection_shape = projection_shape;
        self.expected = expected;
        // A call deferred in the body resolves while the row can still take
        // its effects.
        self.resolve_pending(Some(def));
        let (info, effects) = self.env.leave_closure(expr);

        let parent_args = match self.env.owner() {
            BodyOwner::Func(func, _) => CallableDef::Func(func).params(self.db).to_vec(),
            _ => Vec::new(),
        };
        let closure = ClosureTy::new(
            self.db,
            def,
            super::super::ty_def::ClosureContext {
                args: parent_args,
                evidence: None,
            },
            info.captures
                .iter()
                .map(|capture| capture.ty)
                .collect::<Vec<_>>(),
            param_shapes,
            ret,
            effects,
        );
        self.env.register_closure_info(expr, info);
        ExprProp::new(TyId::closure(self.db, closure), true)
    }

    /// An enclosing body's effect binding used in a closure body: the
    /// closure's row's component with its key, which each call's caller
    /// provides.
    fn capture_effect(&mut self, binding: LocalBinding<'db>) -> ExprProp<'db> {
        let LocalBinding::EffectParam {
            site,
            idx,
            binding_name,
            is_mut,
            ..
        } = binding
        else {
            unreachable!("an effect binding is captured")
        };
        // A binding whose requirement does not resolve has its error already.
        let Some((component, provided)) =
            self.env
                .semantic_effect_requirement(site, idx)
                .and_then(|requirement| {
                    self.env.closure_effect(
                        requirement.key,
                        binding_name,
                        requirement.binding_ty,
                        is_mut,
                    )
                })
        else {
            return ExprProp::invalid(self.db);
        };
        if let Some(provided) = provided {
            self.seed_closure_effect(provided);
        }
        let ty = self.env.lookup_binding_ty(&component);
        ExprProp {
            binding: Some(component),
            borrow_provider: self.concrete_borrow_provider_for_binding(component),
            ..ExprProp::new(ty, is_mut)
        }
    }

    /// Makes `req`, which no provider in a closure body meets, a component
    /// of the closure's row, returning whether it did. Rows cannot be.
    fn provide_closure_effect(
        &mut self,
        req: &EffectRequirementDecl<'db>,
        query: &EffectQuery<'db>,
    ) -> bool {
        if !self.env.in_closure() {
            return false;
        }
        let key = match &query.key {
            EffectPatternKey::Type(key) => {
                SemanticEffectRequirementKey::Type(self.table.fold_ty(self.db, key.carrier))
            }
            EffectPatternKey::Trait(key) => SemanticEffectRequirementKey::Trait(
                TraitKeySchema {
                    def: key.def,
                    args_no_self: key.args_no_self.clone(),
                    assoc_bindings: key.assoc_bindings.clone(),
                }
                .into_trait_inst(self.db)
                .fold_with(self.db, &mut self.table),
            ),
        };
        let has_var = match &key {
            SemanticEffectRequirementKey::Trait(inst) => {
                inst.args(self.db).iter().any(|ty| ty.has_var(self.db))
                    || inst
                        .assoc_ty_bindings(self.db)
                        .iter()
                        .any(|(_, ty)| ty.has_var(self.db))
            }
            key => key.binding_ty(self.db).is_none_or(|ty| ty.has_var(self.db)),
        };
        if has_var {
            return false;
        }
        let name = req
            .name
            .or_else(|| req.key_ty.as_path(self.db)?.ident(self.db).to_opt())
            .unwrap_or_else(|| IdentId::new(self.db, "effect".to_string()));
        let Some((_, provided)) = self
            .env
            .closure_effect(key, name, req.key_ty, req.required_mut)
        else {
            return false;
        };
        if let Some(provided) = provided {
            self.seed_closure_effect(provided);
        }
        true
    }

    /// Provides a new component of a closure's row in its body, below any
    /// `with` the body opens.
    fn seed_closure_effect(&mut self, provided: ProvidedEffect<'db>) {
        let Some(LocalBinding::EffectParam { site, idx, .. }) = provided.binding() else {
            return;
        };
        let (scope, assumptions) = (self.env.owner().scope(), self.env.assumptions());
        if let Some(requirement) = self.env.semantic_effect_requirement(site, idx)
            && let Some(req) = EffectRequirementDecl::from_effect_requirement(self.db, &requirement)
            && let Some(forwarder) =
                seed_forwarder_from_requirement(self, &req, provided, scope, assumptions)
        {
            self.env
                .effect_env_mut()
                .insert_base_forwarder(self.db, forwarder, scope, assumptions);
        }
    }

    /// `op` with `mut?` taken at the body's mode. A body without a mode has
    /// no `mut?`, which `expr` reports, taken as `ref`.
    pub(super) fn access_op(&mut self, expr: ExprId, op: UnOp) -> UnOp {
        if op != UnOp::MaybeMut {
            return op;
        }
        if self.env.owner().mode().is_none() {
            self.push_diag(BodyDiag::MaybeMutWithoutMode {
                primary: expr.span(self.body()).into(),
            });
        }
        match self.maybe_mut_kind() {
            BorrowKind::Mut => UnOp::Mut,
            BorrowKind::Ref => UnOp::Ref,
        }
    }

    /// The shape of a closure parameter: as annotated, where a product
    /// carries its parts' modes, else as `expected`, the shape the closure's
    /// callable bound gives the parameter, has it, else a view of an inferred
    /// type. A mode written without a type (`x: own`) keeps the mode and
    /// infers the type.
    fn closure_param(
        &mut self,
        param: &FuncParam<'db>,
        expected: Option<&Shape<'db>>,
        span: LazyTySpan<'db>,
    ) -> Shape<'db> {
        let db = self.db;
        let annotation = param.ty.to_opt();
        let (mode, payload) = match annotation.map(|ty| ty.data(db)) {
            Some(TypeKind::Mode(mode, payload)) => (
                Some(match mode {
                    TypeMode::Own => FuncParamMode::Own,
                    TypeMode::Mut => FuncParamMode::Mut,
                    TypeMode::Ref => FuncParamMode::View,
                    // `mut?` is the mode of the `mut?` function that defines
                    // the closure.
                    TypeMode::MaybeMut => {
                        if self.env.owner().mode().is_none() {
                            self.push_diag(BodyDiag::MaybeMutWithoutMode {
                                primary: span.clone().into(),
                            });
                        }
                        match self.maybe_mut_kind() {
                            BorrowKind::Mut => FuncParamMode::Mut,
                            BorrowKind::Ref => FuncParamMode::View,
                        }
                    }
                }),
                payload.to_opt(),
            ),
            _ => (None, annotation),
        };
        let param_mode = mode
            .or_else(|| expected.and_then(Shape::param_mode))
            .unwrap_or(FuncParamMode::View);
        match payload {
            // A product's parts carry its modes.
            Some(ty) if is_param_product(db, ty) && mode.is_some() => {
                self.push_diag(TyDiagCollection::from(TyLowerDiag::ModeOnProduct {
                    span: span.into(),
                    shape: false,
                }));
                Shape::Owned(TyId::invalid(db, InvalidCause::Other))
            }
            Some(ty) if is_param_product(db, ty) => {
                let (scope, assumptions) = (self.env.scope(), self.env.assumptions());
                let diags = param_product_diags(db, ty, scope, span, assumptions);
                if !diags.is_empty() {
                    diags.into_iter().for_each(|diag| self.push_diag(diag));
                    return Shape::Owned(TyId::invalid(db, InvalidCause::Other));
                }
                lower_param_product(db, ty, scope, assumptions, self.env.owner().mode())
                    .expect("a product lowers to its shape")
                    .map_tys(self.db, &mut |ty| self.normalize_ty(ty))
            }
            // An associated shape's value, taken whole.
            Some(ty)
                if mode.is_none()
                    && may_name_shape(db, ty, self.env.owner().mode().is_some())
                    && let Some(opaque) = lower_opaque_param(
                        db,
                        ty,
                        self.env.scope(),
                        self.env.assumptions(),
                        self.env.owner().mode(),
                    ) =>
            {
                opaque.map_tys(self.db, &mut |ty| self.normalize_ty(ty))
            }
            Some(ty) => {
                let lowered = self.lower_ty(ty, span.clone(), true);
                match expected {
                    // A mode-free annotation constrains a product's type and
                    // keeps its parts' modes.
                    Some(expected @ Shape::Tuple(_)) if mode.is_none() => {
                        self.equate_ty(lowered, expected.erased_ty(db), span.into());
                        expected.clone()
                    }
                    _ => Shape::param(param_mode, lowered),
                }
            }
            None => match expected {
                Some(expected) if mode.is_none() => expected.clone(),
                _ => {
                    let ty = expected.map_or_else(|| self.fresh_ty(), |shape| shape.erased_ty(db));
                    Shape::param(param_mode, ty)
                }
            },
        }
    }

    fn check_unary(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Un(lhs, op) = expr_data else {
            unreachable!()
        };
        let op = &self.access_op(expr, *op);
        let mut prop = self.check_expr_unknown(*lhs);
        if prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        if *op == UnOp::Deref {
            // Calldata is immutable input.
            if let Some((pointee, space)) = prop.ty.as_ptr(self.db) {
                return ExprProp::new(pointee, space != AddressSpace::Calldata);
            }

            let expected = TyId::ptr_to(self.db, self.fresh_ty());
            self.push_diag(BodyDiag::TypeMismatch {
                span: lhs.span(self.body()).into(),
                expected,
                given: prop.ty,
            });
            return ExprProp::invalid(self.db);
        }

        if prop.ty.is_integral_var(self.db)
            && matches!(op, UnOp::Plus | UnOp::Minus | UnOp::Not | UnOp::BitNot)
        {
            if *op == UnOp::Plus {
                return prop;
            }
            // Every integer type has `!` and `~`. `-` needs a signed type,
            // which is checked once the operand's type is known.
            let builtin = if *op == UnOp::Minus {
                self.env
                    .register_pending_negation(PendingNegation { expr, inner: *lhs });
                UnaryOp::Neg(self.overflow)
            } else {
                UnaryOp::BitNot
            };
            self.env
                .register_builtin_op(expr, BuiltinOp::Unary(builtin));
            return ExprProp::new(prop.ty, true);
        }

        if matches!(op, UnOp::Mut | UnOp::Ref) {
            if *op == UnOp::Mut && self.select_mut_place(*lhs) {
                prop = self.env.typed_expr(*lhs).expect("selected place is typed");
            }
            // A value of an opaque shape is passed on whole, at its mode:
            // `mut d.at()` calls the projection at `mut`.
            if let Some(shape @ Shape::Opaque(_)) = &prop.shape {
                self.consume_access(*lhs);
                if *op == UnOp::Mut && !shape.has_access_of(self.db, BorrowKind::Mut) {
                    self.report_cannot_borrow_mut(*lhs, expr.span(self.body()).into());
                    return ExprProp::invalid(self.db);
                }
                return prop;
            }
            if !self.env.is_place_expr(*lhs) {
                self.push_diag(BodyDiag::BorrowFromNonPlace {
                    primary: expr.span(self.body()).into(),
                });
                return ExprProp::invalid(self.db);
            }
            // A projection's result is a place: `mut v.at(i)` opens an
            // access inside the projection's grant.
            self.consume_access(*lhs);

            let place_ty = prop.ty;
            let borrow_provider = self.access_provider(*lhs);

            let kind = if *op == UnOp::Mut {
                if !prop.is_mut {
                    self.report_cannot_borrow_mut(*lhs, expr.span(self.body()).into());
                    return ExprProp::invalid(self.db);
                }
                BorrowKind::Mut
            } else {
                BorrowKind::Ref
            };
            return ExprProp {
                is_mut: kind == BorrowKind::Mut,
                borrow_provider,
                shape: Some(Shape::Access(kind, place_ty)),
                ..ExprProp::new(place_ty, false)
            };
        }

        let base_ty = prop.ty.base_ty(self.db);
        if base_ty.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        if *op == UnOp::Plus {
            if prop.ty.is_integral(self.db) {
                return prop;
            }
            let diag = BodyDiag::UnsupportedUnaryPlus(expr.span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        if let Some(builtin) = self.builtin_unary_op(prop.ty, *op) {
            self.env
                .register_builtin_op(expr, BuiltinOp::Unary(builtin));
            return ExprProp::new(prop.ty, true);
        }
        let op = self.mode_op(prop.ty, *op);
        self.check_ops_trait(expr, prop.ty, &op, None)
    }

    /// The operator `op` on an operand of type `ty` as written in this body.
    /// In unchecked code, an operator that can overflow calls its wrapping
    /// trait. On a concrete type that doesn't implement it for any right
    /// operand, it calls the trait it calls in checked code. So does a type
    /// parameter or associated type whose bounds don't say it implements it,
    /// if the mode comes from configuration, which code doesn't choose. An
    /// `#[arithmetic(unchecked)]` over the code requires the bound.
    fn mode_op<T: Copy>(&mut self, ty: TyId<'db>, op: T) -> ModeOp<T>
    where
        ModeOp<T>: TraitOps,
    {
        let mode_op = ModeOp {
            op,
            overflow: self.overflow,
        };
        if !mode_op.wraps() {
            return mode_op;
        }
        // Whether the operand's type implements the wrapping trait decides
        // which trait the operator calls, so the trait declares `#[decides]`.
        let Some(trait_def) = presence_trait(
            self.db,
            self.env.scope(),
            &format!("core::{}", mode_op.trait_path_segments().join("::")),
        ) else {
            return mode_op;
        };
        let ty = ty.fold_with(self.db, &mut self.table);
        if matches!(ty.data(self.db), TyData::TyParam(_) | TyData::AssocTy(_))
            && self
                .env
                .owner()
                .scope()
                .source_arithmetic_mode(self.db)
                .is_some()
        {
            return mode_op;
        }
        let snapshot = self.snapshot_state();
        let mut args = vec![ty];
        args.extend(self.fresh_tys_n(trait_def.params(self.db).len() - 1));
        let goal = TraitInstId::new_simple(self.db, trait_def, args);
        let solve_cx = self.env.solve_cx();
        let implemented = !matches!(
            is_goal_satisfiable(self.db, solve_cx, goal),
            GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid
        );
        self.rollback_state(snapshot);
        if implemented {
            mode_op
        } else {
            ModeOp {
                op,
                overflow: Overflow::Checked,
            }
        }
    }

    /// The built-in operation of the unary operator `op` on an operand of
    /// type `ty`. Operators on integers and `bool`s are built in, not calls of
    /// operator traits.
    fn builtin_unary_op(&mut self, ty: TyId<'db>, op: UnOp) -> Option<UnaryOp> {
        let ty = ty.fold_with(self.db, &mut self.table);
        Some(match op {
            UnOp::Minus if ty.as_prim(self.db).is_some_and(PrimTy::is_signed_int) => {
                UnaryOp::Neg(self.overflow)
            }
            UnOp::Not if ty.is_bool(self.db) => UnaryOp::Not,
            UnOp::Not | UnOp::BitNot if ty.is_integral(self.db) => UnaryOp::BitNot,
            _ => return None,
        })
    }

    /// The built-in operation of the binary operator `op` on a left operand
    /// of type `ty`, whose right operand has the same type. Operators on
    /// integers and `bool`s are built in, not calls of operator traits.
    fn builtin_binary_op(&mut self, ty: TyId<'db>, op: BinOp) -> Option<BinaryOp> {
        let ty = ty.fold_with(self.db, &mut self.table);
        let builtin = BinaryOp::new(op, self.overflow)?;
        (ty.is_integral(self.db)
            || ty.is_bool(self.db)
                && matches!(
                    builtin,
                    BinaryOp::BitAnd
                        | BinaryOp::BitOr
                        | BinaryOp::BitXor
                        | BinaryOp::Comp(CompBinOp::Eq | CompBinOp::NotEq)
                ))
        .then_some(builtin)
    }

    /// `e?` on an `Option` or `Result`, a value or a sum shape: the payload,
    /// or else an early exit with the operand's empty variant, which the
    /// function's return must be able to carry (the same kind of sum, with
    /// the same error type).
    fn check_try(&mut self, expr: ExprId, inner: ExprId) -> ExprProp<'db> {
        let prop = self.check_expr_unknown(inner);
        let ty = prop.ty.fold_with(self.db, &mut self.table);
        if ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }
        let scope = self.env.scope();
        let Some(kind) = sum_kind(self.db, scope, ty) else {
            self.push_diag(BodyDiag::TryOnNonSum {
                primary: inner.span(self.body()).into(),
                ty,
            });
            return ExprProp::invalid(self.db);
        };
        let ret = self.expected.fold_with(self.db, &mut self.table);
        let carries_exit = sum_kind(self.db, scope, ret) == Some(kind)
            && (kind == SumKind::Option
                || self
                    .table
                    .unify(ty.generic_args(self.db)[0], ret.generic_args(self.db)[0])
                    .is_ok());
        if !carries_exit {
            if !ret.has_invalid(self.db) {
                self.push_diag(BodyDiag::TryReturnMismatch {
                    primary: expr.span(self.body()).into(),
                    operand: ty,
                    ret,
                });
            }
            return ExprProp::invalid(self.db);
        }
        if let Some(Shape::Sum { payload, .. }) = prop.shape {
            self.consume_access(inner);
            let shape = (!matches!(*payload, Shape::Owned(_))).then(|| (*payload).clone());
            // The payload is mutable as a call's result is.
            return ExprProp {
                is_mut: matches!(shape, None | Some(Shape::Access(BorrowKind::Mut, _))),
                shape,
                ..ExprProp::new(payload.erased_ty(self.db), true)
            };
        }
        let args = ty.generic_args(self.db);
        ExprProp::new(args[kind.payload_arg()], true)
    }

    fn check_cast(
        &mut self,
        expr: ExprId,
        inner_expr: ExprId,
        target_ty: Partial<crate::hir_def::TypeId<'db>>,
    ) -> ExprProp<'db> {
        let Some(hir_target_ty) = target_ty.to_opt() else {
            return ExprProp::invalid(self.db);
        };

        let span = expr.span(self.body()).into_cast_expr().ty();
        let target_ty = self.lower_ty(hir_target_ty, span, true);
        if target_ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        // A fixed String cast supplies the width of a string literal, just
        // as an annotation does. Other casts still infer the source type
        // independently before checking whether the conversion is allowed.
        let inner_prop = if target_ty.is_string(self.db)
            && matches!(
                inner_expr.data(self.db, self.body()),
                Partial::Present(Expr::Lit(LitKind::String(_)))
            ) {
            self.check_expr(inner_expr, target_ty)
        } else {
            self.check_expr_unknown(inner_expr)
        };
        if inner_prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let from = normalize_ty(
            self.db,
            inner_prop.ty,
            self.env.scope(),
            self.env.assumptions(),
        );
        let to = normalize_ty(self.db, target_ty, self.env.scope(), self.env.assumptions());

        if from == to {
            return self.accept_cast(expr, from, to);
        }

        let literal = match inner_expr.data(self.db, self.body()) {
            Partial::Present(Expr::Lit(LitKind::Int(integer))) => Some((*integer, false)),
            Partial::Present(Expr::Un(inner, UnOp::Minus)) => {
                match inner.data(self.db, self.body()) {
                    Partial::Present(Expr::Lit(LitKind::Int(integer))) => Some((*integer, true)),
                    _ => None,
                }
            }
            _ => None,
        };
        if let Some((int_id, negative)) = literal {
            let value = int_id.data(self.db);
            let leaf = self.peel_transparent_newtypes(to);
            if !negative
                && to.is_string(self.db)
                && self.int_literal_fits_in_ty(value, TyId::u256(self.db))
            {
                let _ = self.table.unify(from, TyId::u256(self.db));
                return self.accept_cast(expr, from, to);
            }

            if if negative {
                self.negated_int_literal_fits_in_ty(value, to)
            } else {
                self.int_literal_fits_in_ty(value, to)
            } {
                // Unify the literal's type variable with the target leaf type
                // so it doesn't remain unresolved.
                let _ = self.table.unify(from, leaf);
                return self.accept_cast(expr, from, to);
            }

            // Unify to prevent a spurious "type annotation needed" error.
            let _ = self.table.unify(from, leaf);
            let diag = BodyDiag::InvalidCast {
                primary: expr.span(self.body()).into(),
                from,
                to,
                hint: Some(format!(
                    "integer literal `{}{}` is not representable in `{}`",
                    if negative { "-" } else { "" },
                    value,
                    leaf.pretty_print(self.db),
                )),
            };
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        // Fail if the source type is unknown.
        if from.base_ty(self.db).is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(inner_expr.span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        if self.is_lossless_cast(from, to)
            || self.is_provably_lossless_cast_expr(inner_expr, from, to)
        {
            return self.accept_cast(expr, from, to);
        }

        // Check if the cast failed due to invisible struct fields.
        let hint = if self.newtype_field_hidden(from) || self.newtype_field_hidden(to) {
            Some("cast is not allowed because the struct field is not `pub`".to_string())
        } else if from.as_ptr(self.db).is_some() && to.as_ptr(self.db).is_some() {
            Some("a pointer cannot change its address space".to_string())
        } else if from.as_ptr(self.db).is_some() || to.as_ptr(self.db).is_some() {
            Some(
                "a memory pointer converts only to and from its `usize` address, and a state \
                 pointer to and from its `u256` slot or offset"
                    .to_string(),
            )
        } else {
            None
        };

        let diag = BodyDiag::InvalidCast {
            primary: expr.span(self.body()).into(),
            from,
            to,
            hint,
        };
        self.push_diag(diag);
        ExprProp::invalid(self.db)
    }

    // Allow eg `(some_u256 >> 224) as u32`
    fn is_provably_lossless_cast_expr(&self, expr: ExprId, from: TyId<'db>, to: TyId<'db>) -> bool {
        let body = self.body();
        let (from, to) = (
            self.peel_transparent_newtypes(from),
            self.peel_transparent_newtypes(to),
        );
        if let Partial::Present(Expr::Bin(_, rhs, BinOp::Arith(ArithBinOp::RShift))) =
            expr.data(self.db, body)
            && let Partial::Present(Expr::Lit(LitKind::Int(shift_int))) = rhs.data(self.db, body)
            && let Some(shift) = shift_int.data(self.db).to_usize()
        {
            Target::ALL.into_iter().all(|target| {
                matches!(
                    (
                        self.prim_int_signed_bits(from, target),
                        self.prim_int_signed_bits(to, target),
                    ),
                    (Some((false, from_bits)), Some((false, to_bits)))
                        if to_bits >= from_bits.saturating_sub(shift)
                )
            })
        } else {
            false
        }
    }

    fn is_lossless_cast(&self, from: TyId<'db>, to: TyId<'db>) -> bool {
        if from == to {
            return true;
        }

        let from_leaf = self.peel_transparent_newtypes(from);
        let to_leaf = self.peel_transparent_newtypes(to);

        if from_leaf == to_leaf {
            return true;
        }

        if from_leaf.is_bool(self.db) {
            return self.prim_int_signed_bits(to_leaf, Target::Evm).is_some();
        }

        if to_leaf.is_bool(self.db) {
            return false;
        }

        if self.is_string_word_cast(from_leaf, to_leaf) {
            return true;
        }

        if self.is_pointer_cast(from_leaf, to_leaf)
            || self.is_pointer_address_cast(from_leaf, to_leaf)
        {
            return true;
        }

        self.is_lossless_int_cast(from_leaf, to_leaf)
    }

    fn is_string_word_cast(&self, from: TyId<'db>, to: TyId<'db>) -> bool {
        (from.is_string(self.db) && self.is_plain_u256(to))
            || (self.is_plain_u256(from) && to.is_string(self.db))
    }

    /// A cast between pointers into one space.
    fn is_pointer_cast(&self, from: TyId<'db>, to: TyId<'db>) -> bool {
        matches!(
            (from.as_ptr(self.db), to.as_ptr(self.db)),
            (Some((_, from)), Some((_, to))) if from == to
        )
    }

    /// A memory pointer is its `usize` address; a state pointer is the `u256`
    /// slot or offset it names.
    fn is_pointer_address_cast(&self, from: TyId<'db>, to: TyId<'db>) -> bool {
        let is_address = |ptr: TyId<'db>, address: TyId<'db>| {
            ptr.as_ptr(self.db).is_some_and(|(_, space)| {
                if space == AddressSpace::Memory {
                    address.as_prim(self.db) == Some(PrimTy::Usize)
                } else {
                    self.is_plain_u256(address)
                }
            })
        };
        is_address(from, to) || is_address(to, from)
    }

    fn is_plain_u256(&self, ty: TyId<'db>) -> bool {
        matches!(
            ty.base_ty(self.db).data(self.db),
            TyData::TyBase(TyBase::Prim(PrimTy::U256))
        )
    }

    /// Constructing or destructuring a tuple struct requires all of its fields
    /// to be visible. Reports the first invisible field at `span` and returns
    /// `false` if one exists.
    pub(super) fn check_tuple_struct_fields_visible(
        &mut self,
        ty: TyId<'db>,
        span: DynLazySpan<'db>,
        site: MemberSite,
    ) -> bool {
        let Some(struct_) = ty.as_tuple_struct(self.db) else {
            return true;
        };
        let parent = FieldParent::Struct(struct_);
        for (idx, field) in parent.fields(self.db).enumerate() {
            let scope = ScopeId::Field(parent, idx as u16);
            if !self.member_visible(scope, site) {
                let Some(name) = field.name(self.db) else {
                    continue;
                };
                self.push_diag(PathResDiag::Invisible(span, name, scope.name_span(self.db)));
                return false;
            }
        }
        true
    }

    fn peel_transparent_newtypes(&self, ty: TyId<'db>) -> TyId<'db> {
        self.peel_newtypes(ty).0
    }

    /// `ty` peeled of the newtype layers that the cast being checked sees
    /// through, and how many there are. A layer is seen through only if its
    /// field is visible to the writer of the cast (`newtype_field_visible`).
    /// This is the one place that decides it: the layers of an accepted cast
    /// are recorded (`accept_cast`), and lowering and the unsafe check read
    /// them.
    fn peel_newtypes(&self, mut ty: TyId<'db>) -> (TyId<'db>, usize) {
        let mut layers = 0;
        while let Some(inner) = single_field_newtype_ty(self.db, ty)
            && self.newtype_field_visible(ty)
        {
            ty = inner;
            layers += 1;
        }
        (ty, layers)
    }

    /// Records the layers of the cast `expr` of a value of type `from` to
    /// `to`, which the type checker accepts, and types it as `to`.
    fn accept_cast(&mut self, expr: ExprId, from: TyId<'db>, to: TyId<'db>) -> ExprProp<'db> {
        let from = from.fold_with(self.db, &mut self.table);
        let layers = CastLayers {
            from: self.peel_newtypes(from).1,
            to: self.peel_newtypes(to).1,
        };
        self.env.register_cast_layers(expr, layers);
        ExprProp::new(to, true)
    }

    /// Whether `ty` is a one-field struct whose field the writer of the code
    /// being checked cannot see.
    fn newtype_field_hidden(&self, ty: TyId<'db>) -> bool {
        single_field_newtype_ty(self.db, ty).is_some() && !self.newtype_field_visible(ty)
    }

    /// Whether the field of the newtype `ty` is visible to the code being
    /// checked, by the rule of `member_visible`. A cast sees through a
    /// newtype only then.
    fn newtype_field_visible(&self, ty: TyId<'db>) -> bool {
        match ty.adt_ref(self.db) {
            Some(AdtRef::Struct(struct_)) if ty.field_types(self.db).len() == 1 => self
                .member_visible(
                    ScopeId::Field(FieldParent::Struct(struct_), 0),
                    MemberSite::Current,
                ),
            _ => true,
        }
    }

    fn prim_int_signed_bits(&self, ty: TyId<'db>, target: Target) -> Option<(bool, usize)> {
        let prim = ty.as_prim(self.db)?;
        Some((prim.is_signed_int(), prim_int_bits(prim, target)?))
    }

    /// `usize` and `isize` take the target's width, so a cast must be
    /// lossless on every target.
    fn is_lossless_int_cast(&self, from: TyId<'db>, to: TyId<'db>) -> bool {
        Target::ALL.into_iter().all(|target| {
            match (
                self.prim_int_signed_bits(from, target),
                self.prim_int_signed_bits(to, target),
            ) {
                (Some((from_signed, from_bits)), Some((to_signed, to_bits))) => {
                    match (from_signed, to_signed) {
                        (false, false) | (true, true) => to_bits >= from_bits,
                        (false, true) => to_bits > from_bits,
                        (true, false) => false,
                    }
                }
                _ => false,
            }
        })
    }

    fn int_literal_fits_in_ty(&self, value: &BigUint, target_ty: TyId<'db>) -> bool {
        self.peel_transparent_newtypes(target_ty)
            .as_prim(self.db)
            .is_some_and(|prim| {
                int_literal_fits_prim(
                    self.db.compilation_settings().target(self.db),
                    prim,
                    value,
                    false,
                )
            })
    }

    fn negated_int_literal_fits_in_ty(&self, value: &BigUint, target_ty: TyId<'db>) -> bool {
        self.peel_transparent_newtypes(target_ty)
            .as_prim(self.db)
            .is_some_and(|prim| {
                int_literal_fits_prim(
                    self.db.compilation_settings().target(self.db),
                    prim,
                    value,
                    true,
                )
            })
    }

    fn check_binary(
        &mut self,
        expr: ExprId,
        lhs_expr: ExprId,
        rhs_expr: ExprId,
        op: BinOp,
    ) -> ExprProp<'db> {
        // Logical operands must be bools
        if let BinOp::Logical(_) = op {
            let bool = TyId::bool(self.db);
            let lhs = self.check_expr(lhs_expr, bool);
            let rhs = self.check_expr(rhs_expr, bool);
            return if lhs.ty.is_bool(self.db) && rhs.ty.is_bool(self.db) {
                ExprProp::new(bool, true)
            } else {
                ExprProp::invalid(self.db)
            };
        }

        // Range expressions construct Range types directly
        if matches!(op, BinOp::Arith(ArithBinOp::Range)) {
            return self.check_range_expr(expr, lhs_expr, rhs_expr);
        }

        let lhs = self.check_expr_unknown(lhs_expr);
        if lhs.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let lhs_place_ty = lhs.ty;
        if matches!(op, BinOp::Index) {
            // The indexed base is a place, possibly a projection's result.
            self.consume_access(lhs_expr);
        }
        if matches!(op, BinOp::Index) && lhs_place_ty.is_array(self.db) {
            // Built-in array indexing (TODO: move to trait impl)
            let args = lhs_place_ty.generic_args(self.db);
            let elem_ty = args[0];
            let index_ty = args[1].const_ty_ty(self.db).unwrap();
            self.check_expr(rhs_expr, index_ty);
            // Check the index for static array
            if let Some(index) = self.try_eval_static_int(rhs_expr, index_ty)
                && let Some(len) = lhs_place_ty.array_len(self.db)
                && index.data(self.db) >= &BigUint::from(len)
            {
                self.push_diag(BodyDiag::ArrayIndexOutOfBounds {
                    primary: rhs_expr.span(self.body()).into(),
                    index,
                    len,
                })
            }
            return ExprProp::new(elem_ty, lhs.is_mut);
        }
        if matches!(op, BinOp::Index) {
            let base_ty = self.normalize_ty(lhs_place_ty);
            if let Some((key_ty, output_ty)) = crate::analysis::ty::state_index_tys(
                self.db,
                self.env.scope(),
                base_ty,
                self.env.assumptions(),
            ) {
                // An entry of a collection whose elements are places is a
                // place, mutable through a mutable base, like an array element.
                self.check_expr(rhs_expr, key_ty);
                let trait_def = presence_trait(self.db, self.env.scope(), "core::ops::StateIndex")
                    .expect("StateIndex was resolved for this entry");
                let func = trait_def.method_defs(self.db)[&IdentId::new(self.db, "locate")];
                let goal = TraitInstId::new_simple(self.db, trait_def, vec![base_ty]);
                let callable = Callable::from_item(
                    self.db,
                    CallableDef::Func(func),
                    vec![base_ty],
                    Some(goal),
                );
                let lanes = goal
                    .trait_ref(self.db)
                    .project_assoc_ty(self.db, IdentId::new(self.db, "Lanes"))
                    .expect("StateIndex declares Lanes");
                let codec_ty = self.normalize_ty(lanes);
                let codec_trait = presence_trait(self.db, self.env.scope(), "core::ops::LaneCodec")
                    .expect("core declares LaneCodec");
                let codec_goal =
                    TraitInstId::new_simple(self.db, codec_trait, vec![codec_ty, output_ty]);
                let codec = ["get", "set"].map(|name| {
                    let method = codec_trait.method_defs(self.db)[&IdentId::new(self.db, name)];
                    Callable::from_item(
                        self.db,
                        CallableDef::Func(method),
                        vec![codec_ty, output_ty],
                        Some(codec_goal),
                    )
                });
                self.env.record_place_entry(
                    expr,
                    super::PlaceEntryCalls {
                        locate: callable,
                        codec,
                        codec_choice_failed: false,
                        codec_choices: None,
                    },
                );
                return ExprProp::new(output_ty, lhs.is_mut);
            }
        }

        // The operators on integers, including ones whose type is not yet
        // known, and on bools are built in.
        if let Some(builtin) = self.builtin_binary_op(lhs.ty, op) {
            self.check_expr(rhs_expr, lhs.ty);
            self.env
                .register_builtin_op(expr, BuiltinOp::Binary(builtin));
            let ty = if matches!(op, BinOp::Comp(_)) {
                TyId::bool(self.db)
            } else {
                lhs.ty
            };
            return ExprProp::new(ty, true);
        }

        // Fail if lhs ty is unknown. An open string literal's operator is
        // looked up on the types the literal may take.
        if let TyData::TyVar(var) = lhs.ty.base_ty(self.db).data(self.db)
            && !matches!(var.sort, TyVarSort::String { .. })
        {
            self.check_expr_unknown(rhs_expr);
            let diag = BodyDiag::TypeMustBeKnown(lhs_expr.span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        let op = self.mode_op(lhs.ty, op);
        self.check_ops_trait(expr, lhs.ty, &op, Some(rhs_expr))
    }

    /// Checks `-x` on an integer whose type was not known where it is
    /// written, once it is: a negated literal must fit the type, and the type
    /// must be signed. Returns whether the type is known.
    pub(super) fn check_pending_negation(&mut self, pending: PendingNegation) -> bool {
        let PendingNegation { expr, inner } = pending;
        let Some(inner_prop) = self.env.typed_expr(inner) else {
            return true;
        };
        let operand_ty = {
            let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
            inner_prop.ty.fold_with(self.db, &mut prober)
        };
        let operand_ty = self.normalize_ty(operand_ty);
        if operand_ty.has_invalid(self.db) {
            return true;
        }
        if operand_ty.is_integral_var(self.db) || operand_ty.base_ty(self.db).is_ty_var(self.db) {
            return false;
        }
        if let Some(int_id) = self.try_get_literal_int(inner) {
            let literal = int_id.data(self.db);
            if self.negated_int_literal_fits_in_ty(literal, operand_ty) {
                return true;
            }
            if self
                .peel_transparent_newtypes(operand_ty)
                .base_ty(self.db)
                .is_prim(self.db)
            {
                self.push_diag(BodyDiag::IntLiteralOutOfRange {
                    primary: expr.span(self.body()).into(),
                    literal: format!("-{literal}"),
                    ty: operand_ty,
                });
                return true;
            }
        }
        if self.builtin_unary_op(operand_ty, UnOp::Minus).is_none() {
            let diag = BodyDiag::ops_trait_not_implemented(
                self.db,
                expr.span(self.body()).into(),
                operand_ty,
                &UnOp::Minus,
                resolve_core_trait(
                    self.db,
                    self.env.scope(),
                    &UnOp::Minus.trait_path_segments(),
                )
                .is_some_and(|trait_def| self.provider_offers(trait_def)),
            );
            self.push_diag(diag);
        }
        true
    }

    fn check_let_condition(&mut self, pat: PatId, scrutinee: ExprId) -> ExprProp<'db> {
        let scrutinee_ty = self.fresh_ty();
        let scrutinee_prop = self.check_expr(scrutinee, scrutinee_ty);
        self.take_initializer_marker(scrutinee, &[pat], MarkerPosition::Scrutinee);
        let scrutinee_prop = self.open_matched_place(scrutinee, scrutinee_prop, [pat]);
        self.check_pat(pat, scrutinee_prop.ty);
        self.bind_pattern_source(pat, scrutinee, &scrutinee_prop, false);

        ExprProp::new(TyId::bool(self.db), true)
    }

    pub(super) fn check_cond(&mut self, cond: CondId) -> ExprProp<'db> {
        let Partial::Present(cond_data) = cond.data(self.db, self.body()) else {
            return ExprProp::invalid(self.db);
        };

        match cond_data {
            Cond::Expr(expr) => self.check_expr(*expr, TyId::bool(self.db)),
            Cond::Let(pat, scrutinee) => self.check_let_condition(*pat, *scrutinee),
            Cond::Bin(lhs, rhs, op) => {
                let lhs = self.check_cond(*lhs);
                match op {
                    LogicalBinOp::And => self.env.flush_pending_bindings(),
                    LogicalBinOp::Or => self.env.clear_pending_bindings(),
                }
                let rhs = self.check_cond(*rhs);
                if lhs.ty.is_bool(self.db) && rhs.ty.is_bool(self.db) {
                    ExprProp::new(TyId::bool(self.db), true)
                } else {
                    ExprProp::invalid(self.db)
                }
            }
        }
    }

    /// Check a range expression `start..end` and return the Range type.
    ///
    /// Both operands must be `usize`. The result type depends on whether bounds
    /// are compile-time constants:
    /// - `Range<Known<S>, Known<E>>` when both are literals (0 words)
    /// - `Range<Known<S>, Unknown>` when only start is literal (1 word)
    /// - `Range<Unknown, Known<E>>` when only end is literal (1 word)
    /// - `Range<Unknown, Unknown>` when neither is literal (2 words)
    fn check_range_expr(
        &mut self,
        _expr: ExprId,
        start_expr: ExprId,
        end_expr: ExprId,
    ) -> ExprProp<'db> {
        let usize_ty = TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));

        // Check that both operands are usize
        self.check_expr(start_expr, usize_ty);
        self.check_expr(end_expr, usize_ty);

        // Try to detect if bounds are literal integers
        let start_lit = self.try_get_literal_int(start_expr);
        let end_lit = self.try_get_literal_int(end_expr);

        // Resolve Range types from core library
        match resolve_core_range_types(self.db, self.env.scope()) {
            Some(types) => {
                // Construct appropriate bound types based on constness
                let start_bound =
                    self.make_range_bound(start_lit, types.known, types.unknown, usize_ty);
                let end_bound =
                    self.make_range_bound(end_lit, types.known, types.unknown, usize_ty);

                // Construct Range<StartBound, EndBound>
                let range_s = TyId::app(self.db, types.range, start_bound);
                let range_full = TyId::app(self.db, range_s, end_bound);
                ExprProp::new(range_full, true)
            }
            _ => {
                // Fallback: if Range/Known/Unknown isn't found, return invalid
                // This shouldn't happen in normal usage
                ExprProp::invalid(self.db)
            }
        }
    }

    /// Try to extract a literal integer value from an expression.
    /// Returns `Some(IntegerId)` if the expression is a literal integer, `None` otherwise.
    fn try_get_literal_int(&self, expr: ExprId) -> Option<IntegerId<'db>> {
        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            return None;
        };

        match expr_data {
            Expr::Lit(LitKind::Int(int_id)) => Some(*int_id),
            _ => None,
        }
    }

    fn try_eval_static_int(&self, expr: ExprId, expected: TyId<'db>) -> Option<IntegerId<'db>> {
        if let Some(value) = self.try_get_literal_int(expr) {
            // A literal out of its type's range is reported as such, not as
            // a value.
            return self
                .int_literal_fits_in_ty(value.data(self.db), expected)
                .then_some(value);
        }
        if let Some(const_ref) = self.env.expr_const_ref(expr)
            && let Some(const_ref) =
                resolve_semantic_const_ref(self.db, const_ref, expected, SemOrigin::Expr(expr))
            && let EvalOutcome::Ready(value) = eval_const_ref(self.db, const_ref)
            && let SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } = value.value(self.db)
        {
            let (sign, bytes) = value.to_bytes_be();
            if sign != Sign::Minus {
                return Some(IntegerId::new(self.db, BigUint::from_bytes_be(&bytes)));
            }
        }
        let value = try_eval_const_int_expr(self.db, self.body(), expr, expected)?;
        let (sign, bytes) = value.to_bytes_be();
        (sign != Sign::Minus).then(|| IntegerId::new(self.db, BigUint::from_bytes_be(&bytes)))
    }

    /// Create a range bound type: either `Known<N>` for a literal or `Unknown`.
    fn make_range_bound(
        &self,
        lit: Option<IntegerId<'db>>,
        known_base: TyId<'db>,
        unknown_ty: TyId<'db>,
        usize_ty: TyId<'db>,
    ) -> TyId<'db> {
        match lit {
            Some(int_id) => {
                // Create Known<N> where N is the literal value
                let const_ty = const_ty_from_sem_const(
                    self.db,
                    int_const(
                        self.db,
                        usize_ty,
                        BigInt::from(int_id.data(self.db).clone()),
                    ),
                );
                let const_ty_id = TyId::const_ty(self.db, const_ty);
                TyId::app(self.db, known_base, const_ty_id)
            }
            None => unknown_ty,
        }
    }

    fn try_open_derive_provider(
        &mut self,
        value: ExprId,
        provider_ty: TyId<'db>,
        alias_scope: Option<crate::hir_def::scope_graph::ScopeId<'db>>,
    ) -> bool {
        use super::super::trait_resolution::{
            ChosenImpls,
            generation::{ProviderOpening, provider_hints},
        };
        use parser::ast::prelude::*;
        let provider_ty = normalize_ty(
            self.db,
            provider_ty,
            self.env.scope(),
            self.env.assumptions(),
        );
        let hints = match provider_hints(
            self.db,
            self.env.scope(),
            self.env.assumptions(),
            provider_ty,
        ) {
            Ok(Some(hints)) => Ok(hints),
            // No `Derive` implementation can serve the type: the operand is
            // an ordinary effect value.
            Ok(None) => return false,
            Err(problem) => Err(problem),
        };
        // Everything a quotation lowered is generated code, whichever item
        // holds it, and generated code opens no provider. The same question
        // decides the trust and the visibility rules.
        let generated_owner = self.env.scope().is_in_published_item(self.db);
        let body = self.body();
        let origin = body.source_map(self.db).expr_map.node_to_source(value);
        let key = if !generated_owner
            && let crate::span::HirOrigin::Raw(pointer) = origin
            && let Some(file) = body.top_mod(self.db).file(self.db).url(self.db)
        {
            let root = crate::lower::top_mod_ast(self.db, body.top_mod(self.db));
            Some(common::generation::ScopedDeriveOpeningKey {
                file,
                binding_range: pointer.to_node(root.syntax()).syntax().text_range(),
            })
        } else {
            None
        };
        let Some(key) = key else {
            self.push_diag(BodyDiag::CompileTimePhase {
                primary: value.span(body).into(),
                message: "scoped derive openings require a uniquely identified authored body; generated openings need a portable generated-owner identity".into(),
                label: BodyDiag::REFUSED_OPENING_LABEL,
            });
            self.env.named_impl_operands.insert(value);
            self.env.type_expr(value, ExprProp::invalid(self.db));
            return true;
        };
        let viewer = self.viewer(MemberSite::Expr(value));
        for scope in [alias_scope, provider_ty.as_scope(self.db)]
            .into_iter()
            .flatten()
        {
            if !viewer.sees(self.db, scope) {
                self.env
                    .generation_demands
                    .refuse(key, "scoped derive provider path is not visible");
                if !matches!(
                    value.data(self.db, body),
                    Partial::Present(Expr::RecordInit(..))
                ) && let Some(name) = scope.name(self.db)
                {
                    self.push_diag(PathResDiag::Invisible(
                        value.span(body).into(),
                        name,
                        scope.name_span(self.db),
                    ));
                }
                self.env.named_impl_operands.insert(value);
                self.env.type_expr(value, ExprProp::invalid(self.db));
                return true;
            }
        }
        // The types written in the operand's path are checked where they are
        // written, as in any other path. A record literal's path is checked
        // with the literal.
        if let Partial::Present(Expr::Path(Partial::Present(path))) = value.data(self.db, body) {
            let diags = super::super::ty_error::path_generic_arg_diags(
                self.db,
                *path,
                value.span(body).into_path_expr().path(),
                self.viewer(MemberSite::Expr(value)),
                self.env.assumptions(),
                false,
            );
            if !diags.is_empty() {
                self.env.generation_demands.refuse(
                    key,
                    "scoped derive provider path has an invalid generic argument",
                );
                for diag in diags {
                    self.push_diag(diag);
                }
                self.env.named_impl_operands.insert(value);
                self.env.type_expr(value, ExprProp::invalid(self.db));
                return true;
            }
        }
        let hints = match hints {
            Ok(hints) => hints,
            Err(problem) => {
                // A `Derive` implementation could serve the type, but what it
                // offers cannot be determined: the operand is no ordinary
                // value and no usable provider.
                self.env.generation_demands.refuse(key, problem);
                self.push_diag(BodyDiag::CompileTimePhase {
                    primary: value.span(body).into(),
                    message: problem.into(),
                    label: BodyDiag::REFUSED_OPENING_LABEL,
                });
                self.env.named_impl_operands.insert(value);
                self.env.type_expr(value, ExprProp::invalid(self.db));
                return true;
            }
        };
        if let Partial::Present(Expr::RecordInit(_, fields)) = value.data(self.db, body)
            && !fields.is_empty()
        {
            let message = "scoped derive provider literals must have no fields; provider values are not evaluated";
            self.env.generation_demands.refuse(key, message);
            self.push_diag(BodyDiag::CompileTimePhase {
                primary: value.span(body).into(),
                message: message.into(),
                label: BodyDiag::REFUSED_OPENING_LABEL,
            });
            self.env.named_impl_operands.insert(value);
            self.env.type_expr(value, ExprProp::invalid(self.db));
            return true;
        }
        let opening = ProviderOpening::new(self.db, key, provider_ty, hints, self.env.scope());
        self.env.provider_openings.push(opening);
        self.env.choices = Some(ChosenImpls::push_provider(
            self.db,
            self.env.choices,
            opening,
        ));
        self.env.named_impl_operands.insert(value);
        self.env
            .type_expr(value, ExprProp::new(TyId::unit(self.db), false));
        true
    }

    /// Admits the implementation that the operand `operand` of a `with` list
    /// pushed onto `choice`, unless an earlier operand answers the same goal.
    /// The conditions of an opened implementation are proved once the whole
    /// list is read.
    fn admit_with_operand(
        &mut self,
        list: &mut OperandList<'db>,
        operand: ExprId,
        choice: ChosenImpls<'db>,
        conditions: Vec<TraitInstId<'db>>,
    ) {
        let entry = choice
            .items(self.db)
            .last()
            .and_then(|entry| entry.implementation())
            .expect("a pushed choice is the last item");
        let span: DynLazySpan<'db> = operand.span(self.body()).into();
        match list.admit(
            self.db,
            self.env.solve_cx(),
            entry,
            choice.items(self.db).len() - 1,
            conditions,
            span.clone(),
        ) {
            Ok(()) => self.env.choices = Some(choice),
            Err((first, goal, possible)) => {
                self.push_diag(BodyDiag::DuplicateWithChoice {
                    primary: span,
                    first,
                    goal,
                    possible,
                });
            }
        }
        self.env.named_impl_operands.insert(operand);
        // A named impl is a compile-time operand, with no runtime value.
        self.env
            .type_expr(operand, ExprProp::new(TyId::unit(self.db), false));
    }

    fn check_with(
        &mut self,
        bindings: &[WithBinding<'db>],
        body_expr: ExprId,
        expected: TyId<'db>,
        result_discarded: bool,
    ) -> ExprProp<'db> {
        self.env.effect_env_mut().push_frame();
        // The keys this `with` provides, each with its binding's span.
        let mut keyed: Vec<(StoredEffectKey<'db>, DynLazySpan<'db>)> = Vec::new();
        let outer_choices = self.env.choices;
        let mut ambiguous_opening = false;
        // The implementations this `with` lists, and the conditions of its
        // openings.
        let mut list = OperandList::new();
        let mut providers = Vec::new();

        for binding in bindings {
            let meaning = if binding.key_path.is_none()
                && let Partial::Present(Expr::Path(path)) = binding.value.data(self.db, self.body())
                && let Some(path) = path.to_opt()
            {
                let site = OperandSite {
                    scope: self.env.scope(),
                    assumptions: self.env.assumptions(),
                    written_assumptions: self.env.written_assumptions(),
                    viewer: self.viewer(MemberSite::Expr(binding.value)),
                    outer: self.env.choices,
                    span: binding.value.span(self.body()).into(),
                    name_span: self.with_operand_name_span(path, binding.value),
                };
                Some(classify_with_operand(
                    self.db,
                    &site,
                    path,
                    &BlockOperands {
                        db: self.db,
                        env: &self.env,
                    },
                ))
            } else {
                None
            };
            match meaning {
                Some(OperandMeaning::Refused {
                    diags,
                    changed_choice,
                    leaves_open,
                }) => {
                    ambiguous_opening |= leaves_open;
                    for diag in diags {
                        match diag {
                            FuncBodyDiag::Body(diag) if changed_choice => {
                                self.push_changed_choice(diag);
                            }
                            diag => self.push_diag(diag),
                        }
                    }
                    self.env.named_impl_operands.insert(binding.value);
                    self.env
                        .type_expr(binding.value, ExprProp::new(TyId::unit(self.db), false));
                    continue;
                }
                Some(OperandMeaning::Named { choice }) => {
                    self.admit_with_operand(&mut list, binding.value, choice, Vec::new());
                    continue;
                }
                Some(OperandMeaning::Opening { choice, conditions }) => {
                    self.admit_with_operand(&mut list, binding.value, choice, conditions);
                    continue;
                }
                Some(OperandMeaning::NotAnImplementation(class)) => {
                    debug_assert!(SiteKind::Block.accepts(&class));
                    // A type is a derive provider candidate; any other name
                    // is checked as an expression.
                    if let OperandClass::ProviderType(provider_ty, alias_scope) = class
                        && self.try_open_derive_provider(binding.value, provider_ty, alias_scope)
                    {
                        continue;
                    }
                }
                None => {}
            }
            let value_prop = self.check_expr_unknown(binding.value);
            // A record literal is also a spelling of a compile-time provider.
            // Its type is owned by ordinary expression checking, not re-resolved
            // from the constructor's source spelling.
            let provider_ty = self.table.fold_ty(self.db, value_prop.ty);
            if binding.key_path.is_none()
                && let Partial::Present(Expr::RecordInit(path, _)) =
                    binding.value.data(self.db, self.body())
            {
                let (resolved_ty, alias_scope) = path
                    .to_opt()
                    .and_then(|path| {
                        crate::analysis::name_resolution::resolve_path(
                            self.db,
                            path,
                            self.env.scope(),
                            self.env.assumptions(),
                            false,
                        )
                        .ok()
                    })
                    .map_or((None, None), |resolved| match resolved {
                        crate::analysis::name_resolution::PathRes::Ty(ty) => (Some(ty), None),
                        crate::analysis::name_resolution::PathRes::TyAlias(alias, ty) => {
                            (Some(ty), Some(alias.alias.scope()))
                        }
                        _ => (None, None),
                    });
                let provider_ty = if provider_ty.has_invalid(self.db) {
                    resolved_ty.unwrap_or(provider_ty)
                } else {
                    provider_ty
                };
                if self.try_open_derive_provider(binding.value, provider_ty, alias_scope) {
                    continue;
                }
            }
            // A provider confers authority over its place for the body; it
            // opens no access.
            self.consume_access(binding.value);
            let is_mut = value_prop.is_mut;
            if self.provider_covered(binding.value) {
                self.env.cover_provider(binding.value);
            }

            let provided = ProvidedEffect {
                origin: EffectOrigin::With {
                    value_expr: binding.value,
                    binding: value_prop.binding,
                },
                ty: self.table.fold_ty(self.db, value_prop.ty),
                is_mut,
            };

            self.install_with_provider(binding, provided, &mut keyed, false);
            providers.push((binding, provided));
        }

        // An opened implementation's conditions are proved in the frame this
        // `with` builds, without the opening itself: the enclosing choices and
        // every other operand of the list, whatever their order. An opening
        // whose conditions can never hold is refused whether or not the block
        // uses it.
        if let Some(list_choices) = self.env.choices {
            let start = outer_choices.map_or(0, |outer| outer.items(self.db).len());
            let list_choices = list_choices.group_after(self.db, start);
            self.env.choices = Some(list_choices);
            for (goal, context, span) in list.obligations(self.db, list_choices) {
                self.env.register_trait_obligation(TraitObligation {
                    choices: context,
                    goal,
                    origin: TraitObligationOrigin::GenericConfirmation,
                    span,
                });
            }
        }

        if let Some(choices) = self.env.choices {
            use super::super::trait_resolution::ChoiceEntry;
            let start = outer_choices.map_or(0, |outer| outer.items(self.db).len());
            let entries = &choices.items(self.db)[start..];
            if entries.len() > 1
                && entries
                    .iter()
                    .any(|entry| matches!(entry, ChoiceEntry::Provider(_)))
            {
                use super::super::trait_resolution::generation::closed_offers;

                let mut claimed = rustc_hash::FxHashMap::default();
                let mut conflict = None;
                let mut exhausted = None;
                for &entry in entries {
                    let (families, provider) = match entry {
                        ChoiceEntry::Impl(choice) => (
                            crate::core::meta::derive_families(
                                self.db,
                                choice.implementation().trait_inst(self.db),
                                self.env.scope(),
                                self.env.assumptions(),
                            )
                            .into_iter()
                            .collect(),
                            false,
                        ),
                        // A block's own entries never hold a declared operand.
                        ChoiceEntry::Declared(_) => continue,
                        ChoiceEntry::Provider(opening) => match closed_offers(self.db, opening) {
                            Ok(families) => (families, true),
                            Err(problem) => {
                                exhausted = Some((opening, problem));
                                break;
                            }
                        },
                    };
                    for family in families {
                        // Two spellings of one family claim it once.
                        let family = crate::core::meta::family_identity(self.db, family);
                        if let Some(previous_provider) = claimed.insert(family, provider)
                            && (previous_provider || provider)
                        {
                            conflict = Some(if previous_provider && provider {
                                "one `with` header gives two derive providers for the same trait application"
                            } else {
                                "one `with` header gives a derive provider and a choice for the same trait application"
                            });
                        }
                    }
                }
                let refused = exhausted.or_else(|| {
                    conflict.map(|message| {
                        let opening = entries
                            .iter()
                            .find_map(|entry| match entry {
                                ChoiceEntry::Provider(opening) => Some(*opening),
                                ChoiceEntry::Impl(_) | ChoiceEntry::Declared(_) => None,
                            })
                            .expect("provider-containing group");
                        (opening, message)
                    })
                });
                if let Some((opening, message)) = refused {
                    self.env
                        .generation_demands
                        .refuse(opening.key(self.db).clone(), message);
                    self.push_diag(BodyDiag::CompileTimePhase {
                        primary: body_expr.span(self.body()).into(),
                        message: message.into(),
                        label: BodyDiag::REFUSED_OPENING_LABEL,
                    });
                    // An invalid opening supplies no candidates or requests.
                    // In particular, do not check its body under a winning
                    // provider or silently retry it under ordinary selection.
                    self.env.effect_env_mut().pop_frame();
                    self.env.choices = outer_choices;
                    return ExprProp::invalid(self.db);
                }
            }
        }
        if ambiguous_opening {
            // An operand with two meanings, or with arguments that name no
            // implementation, leaves the block's meaning open: its body is
            // not checked, so that it reports nothing the operand would have
            // provided. Preparation consumes demands before final
            // diagnostics, so an invalid header must not let an unrelated
            // provider execute first.
            if let Some(opening) = self.env.choices.and_then(|choices| {
                choices.items(self.db).iter().find_map(|entry| match entry {
                    super::super::trait_resolution::ChoiceEntry::Provider(opening) => {
                        Some(*opening)
                    }
                    super::super::trait_resolution::ChoiceEntry::Impl(_)
                    | super::super::trait_resolution::ChoiceEntry::Declared(_) => None,
                })
            }) {
                self.env.generation_demands.refuse(
                    opening.key(self.db).clone(),
                    "ambiguous implementation operand in `with` header",
                );
            }
            self.env.effect_env_mut().pop_frame();
            self.env.choices = outer_choices;
            return ExprProp::invalid(self.db);
        }

        // Operand expressions see the effects and choices already in scope
        // when they are checked. The body gets each checked provider under
        // the complete set of choices. Rebuild the witnesses without checking
        // or evaluating the operand expressions again.
        if providers
            .iter()
            .any(|(binding, _)| binding.key_path.is_some())
        {
            self.env.effect_env_mut().pop_frame();
            self.env.effect_env_mut().push_frame();
            keyed.clear();
            for (binding, provided) in providers {
                self.install_with_provider(binding, provided, &mut keyed, true);
            }
        }

        let result = if result_discarded {
            self.check_expr_with_discarded_result(body_expr, expected)
        } else {
            self.check_expr(body_expr, expected)
        };
        self.env.effect_env_mut().pop_frame();
        self.env.choices = outer_choices;
        result
    }

    fn install_with_provider(
        &mut self,
        binding: &WithBinding<'db>,
        provider: ProvidedEffect<'db>,
        keyed: &mut Vec<(StoredEffectKey<'db>, DynLazySpan<'db>)>,
        emit_diag: bool,
    ) {
        let Some(key_path) = binding.key_path else {
            self.env.effect_env_mut().insert_unkeyed(provider);
            return;
        };
        let Some(key_path) = key_path.to_opt() else {
            return;
        };
        let provider = ProvidedEffect {
            ty: self.table.fold_ty(self.db, provider.ty),
            ..provider
        };
        let span: DynLazySpan<'db> = binding.value.span(self.body()).into();
        let (witness, commit) =
            match self.validate_keyed_with(key_path, provider, span.clone(), emit_diag) {
                Ok(witness) => witness,
                Err(pattern) => {
                    self.insert_effect_barrier(key_path, *pattern);
                    return;
                }
            };
        let committed = self.apply_effect_commit_plan(commit);
        debug_assert!(committed, "validated keyed `with` binding commit failed");
        if !committed {
            let pattern = build_barrier_pattern_for_with_key(self, key_path)
                .expect("validated keyed binding should have a barrier pattern");
            self.insert_effect_barrier(key_path, pattern);
            return;
        }
        if let Some((_, first)) = keyed.iter().find(|(key, _)| *key == witness.key) {
            if emit_diag {
                self.push_diag(BodyDiag::DuplicateWithKey {
                    primary: span,
                    first: first.clone(),
                    key: key_path,
                });
            }
            return;
        }
        keyed.push((witness.key.clone(), span));
        let (scope, assumptions) = (self.env.scope(), self.env.assumptions());
        self.env
            .effect_env_mut()
            .insert_witness(self.db, witness, scope, assumptions);
    }

    /// The span of the last segment of the `with` operand `operand`, written
    /// as the path `path`: the name a diagnostic about the operand points at.
    fn with_operand_name_span(&self, path: PathId<'db>, operand: ExprId) -> DynLazySpan<'db> {
        operand
            .span(self.body())
            .into_path_expr()
            .path()
            .segment(path.segment_index(self.db))
            .ident()
            .into()
    }

    fn check_call(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Call(callee, args) = expr_data else {
            unreachable!()
        };
        let callee_prop = self.check_expr_unknown(*callee);
        if callee_prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        // Only a path names the function it calls; any other callee, even one
        // of a function item's type, is a value the call evaluates first.
        let is_path = matches!(
            callee.data(self.db, self.body()),
            Partial::Present(Expr::Path(..))
        );
        let mut callable = if is_path && let Some(existing) = self.env.callable_expr(*callee) {
            existing.clone()
        } else if !callee_prop.ty.base_ty(self.db).is_ty_var(self.db)
            && (!is_path || !callee_prop.ty.is_func(self.db))
        {
            return self.check_value_call(expr, *callee, callee_prop, args);
        } else {
            match Callable::new(
                self.db,
                callee_prop.ty,
                callee.span(self.body()).into(),
                None,
            ) {
                Ok(callable) => callable,
                Err(diag) => {
                    self.push_diag(diag);
                    return ExprProp::invalid(self.db);
                }
            }
        };

        let call_span = expr.span(self.body()).into_call_expr();

        if let Some(kind) = self.code_region_intrinsic_kind(callable.callable_def())
            && args.len() == 1
            && let Some(result) = self.check_code_region_intrinsic(
                expr,
                &mut callable,
                args,
                kind,
                None,
                Some(*callee),
            )
        {
            return result;
        }

        callable.check_args(self, args, call_span.clone().args(), None, false);

        // The callee's bounds infer what its rows need, such as the
        // arguments of a function item passed for an `F: Fn(..)`.
        callable.process_constraints(self, expr, call_span.callee().into());
        self.check_callable_effects(expr, &mut callable);
        self.finish_call(expr, callable)
    }

    /// Checks a call `f(..)` of a callable value as the call `f.call(..)` of
    /// the callable trait it implements, or `f.call_mut(..)` of an
    /// `FnMut` shape: the callee is the method's receiver.
    fn check_value_call(
        &mut self,
        expr: ExprId,
        callee: ExprId,
        callee_prop: ExprProp<'db>,
        args: &[HirCallArg<'db>],
    ) -> ExprProp<'db> {
        let call_span = expr.span(self.body()).into_call_expr();
        let callee_span: DynLazySpan<'db> = callee.span(self.body()).into();
        let callee_ty = callee_prop.ty;
        let canonical = Canonicalized::new(self.db, callee_ty);
        let candidate = match select_callable_value_method(
            self.db,
            &canonical,
            self.env.scope(),
            self.env.assumptions(),
            self.env.choices,
        ) {
            Ok(candidate) => candidate,
            Err(MethodSelectionError::AmbiguousTraitMethod(ambiguous)) => {
                self.env.record_value_call(expr);
                return self.defer_method_call(
                    expr,
                    callee_ty,
                    &canonical,
                    ambiguous,
                    args,
                    callee_span,
                );
            }
            Err(MethodSelectionError::ReceiverTypeMustBeKnown) => {
                self.push_diag(BodyDiag::TypeMustBeKnown(callee_span));
                return ExprProp::invalid(self.db);
            }
            Err(_) => {
                self.push_diag(BodyDiag::NotCallable(callee_span, callee_ty));
                return ExprProp::invalid(self.db);
            }
        };
        let (func_ty, trait_inst) = self.selected_method_ty(
            candidate,
            &canonical,
            callee_ty,
            expr.span(self.body()).into(),
        );
        let mut callable = match Callable::new(self.db, func_ty, callee_span.clone(), trait_inst) {
            Ok(callable) => callable,
            Err(diag) => {
                self.push_diag(diag);
                return ExprProp::invalid(self.db);
            }
        };
        let Some(params) = self.call_params(&callable, call_span.clone().args().into()) else {
            return ExprProp::invalid(self.db);
        };
        let expected = params.len() - 1;
        if args.len() != expected {
            let def_span = callee_ty
                .as_closure(self.db)
                .map_or(callee_span, |closure| {
                    let def = closure.def(self.db);
                    def.expr.span(def.body).into()
                });
            self.push_diag(BodyDiag::CallArgNumMismatch {
                primary: call_span.args().into(),
                def_span,
                given: args.len(),
                expected,
            });
            return ExprProp::invalid(self.db);
        }
        self.env.record_value_call(expr);
        callable.check_args(
            self,
            args,
            call_span.clone().args(),
            Some((callee, callee_prop)),
            false,
        );
        callable.process_constraints(self, expr, call_span.callee().into());
        self.check_callable_effects(expr, &mut callable);
        self.finish_call(expr, callable)
    }

    /// Whether a value of type `ty` has a field `name` that holds a callable
    /// value: a function, or a value a call `f(..)` calls.
    fn is_callable_field(&self, ty: TyId<'db>, name: IdentId<'db>) -> bool {
        let Some(field_ty) = RecordLike::from_ty(ty).record_field_ty(self.db, name) else {
            return false;
        };
        if field_ty.has_invalid(self.db) {
            return false;
        }
        field_ty.is_func(self.db)
            || !matches!(
                select_callable_value_method(
                    self.db,
                    &Canonicalized::new(self.db, field_ty),
                    self.env.scope(),
                    self.env.assumptions(),
                    self.env.choices,
                ),
                Err(MethodSelectionError::NotFound | MethodSelectionError::ReceiverTypeMustBeKnown)
            )
    }

    /// The function type of the method `candidate` selects for a receiver of
    /// type `receiver_ty`, and the instance of its trait, if any. A
    /// candidate that needs confirmation obliges the call at `span` to prove
    /// its trait bound.
    fn selected_method_ty(
        &mut self,
        candidate: MethodCandidate<'db>,
        canonical: &Canonicalized<'db, TyId<'db>>,
        receiver_ty: TyId<'db>,
        span: DynLazySpan<'db>,
    ) -> (TyId<'db>, Option<TraitInstId<'db>>) {
        let method_name = candidate.name(self.db);
        match candidate {
            MethodCandidate::InherentMethod(cand) => (
                self.extract_inherent_method_to_term(cand, receiver_ty),
                None,
            ),
            MethodCandidate::TraitMethod(cand) => {
                let inst = canonical.extract_solution(&mut self.table, cand.inst);
                let inst = self.specialize_same_trait_method_inst(method_name, inst);
                let func_ty = self.instantiate_trait_method_to_term(cand.method, receiver_ty, inst);
                (func_ty, Some(inst))
            }
            MethodCandidate::NeedsConfirmation(cand) => {
                let inst = canonical.extract_solution(&mut self.table, cand.inst);
                let inst = self.specialize_same_trait_method_inst(method_name, inst);
                self.env.register_trait_obligation(TraitObligation {
                    choices: self.env.choices,
                    goal: inst,
                    origin: TraitObligationOrigin::GenericConfirmation,
                    span,
                });
                let func_ty = self.instantiate_trait_method_to_term(cand.method, receiver_ty, inst);
                (func_ty, Some(inst))
            }
        }
    }

    /// Defers a call whose method is one of several trait methods, which
    /// argument and result types choose once the body is checked, unless the
    /// arguments already leave one. `span` is the method's name, or a value
    /// call's callee.
    fn defer_method_call(
        &mut self,
        expr: ExprId,
        receiver_ty: TyId<'db>,
        canonical: &Canonicalized<'db, TyId<'db>>,
        ambiguous: AmbiguousTraitMethods<'db>,
        args: &[HirCallArg<'db>],
        span: DynLazySpan<'db>,
    ) -> ExprProp<'db> {
        let method_name = ambiguous.candidates[0]
            .cand
            .method
            .name(self.db)
            .to_opt()
            .expect("trait methods have names");
        let typed = ExprProp::new(self.fresh_ty(), true);
        self.env.type_expr(expr, typed.clone());
        // The arguments are typed now so that they constrain the choice.
        for arg in args {
            self.check_expr_unknown(arg.expr);
        }
        let candidates = ambiguous
            .candidates
            .into_iter()
            .map(|candidate| {
                let inst = canonical.extract_solution(&mut self.table, candidate.cand.inst);
                let inst = self.specialize_same_trait_method_inst(method_name, inst);
                super::env::PendingMethodCandidate {
                    inst,
                    method: candidate.cand.method,
                    needs_confirmation: candidate.needs_confirmation,
                }
            })
            .collect();
        let pending = super::env::PendingMethod {
            expr,
            recv_ty: receiver_ty,
            method_name,
            candidates,
            span,
            site: self.env.call_site(),
        };
        // Inference only narrows the candidates, unless one is rejected on a
        // projection that waits on it, and the call's type is not yet
        // constrained, so a single fitting candidate is the one the deferred
        // call would resolve to. Resolving it now types the call for an
        // operator applied to it: `!s.eq(t)`.
        if let Some(parts) = super::pending_call_parts(self.db, self.body(), &self.env, expr) {
            let recv_ty = receiver_ty.fold_with(self.db, &mut self.table);
            let (viable, undecided) =
                self.viable_pending_candidates(&pending, recv_ty, typed.ty, parts, false);
            if !undecided && let [candidate] = viable[..] {
                self.resolve_pending_method(&pending, candidate, recv_ty, typed.ty, parts);
                return typed;
            }
            // For the same reason, a type all fitting candidates return is
            // the call's: `!s.eq("serve")` is a `bool` whichever impl the
            // literal picks.
            if !undecided
                && let Some(ret_tys) = viable
                    .iter()
                    .map(|&candidate| {
                        self.method_viability(&pending, recv_ty, typed.ty, parts, candidate)
                            .ret_ty
                    })
                    .collect::<Option<Vec<_>>>()
                && let [ret_ty, rest @ ..] = &ret_tys[..]
                && rest.iter().all(|ty| ty == ret_ty)
            {
                self.equate_ty(typed.ty, *ret_ty, pending.span.clone());
            }
        }
        self.env.register_pending_method(pending);
        typed
    }

    /// A callee's return type with the call's generic arguments filled in can
    /// hold a const that fails to evaluate only now, such as `S<{N - 5}>` at
    /// `N = 2`. The failure leaves an invalid type, which matches any type and
    /// so would let the call pass checking. Report it instead.
    fn report_instantiated_const_fault(
        &mut self,
        callable: &Callable<'db>,
        ret_ty: TyId<'db>,
        expr: ExprId,
    ) {
        // A failure already in the declared return type is reported with the
        // callee's signature.
        let declared = callable
            .callable_def()
            .ret_ty(self.db)
            .instantiate_identity();
        if declared.has_invalid(self.db) {
            return;
        }
        let call: DynLazySpan<'db> = expr.span(self.body()).into();
        if let Some(cause) = first_invalid_ty_cause(self.db, ret_ty)
            && cause.const_eval_fault().is_some()
            && let Some(fault) = diag_from_invalid_cause(call.clone(), &cause)
        {
            self.push_diag(BodyDiag::CallReturnTypeConstFault {
                call,
                fault: Box::new(fault),
            });
        }
    }

    fn check_assert(&mut self, expr: ExprId, args: &[HirCallArg<'db>]) -> ExprProp<'db> {
        if !(1..=2).contains(&args.len()) {
            for arg in args {
                self.check_expr_unknown(arg.expr);
            }
            self.push_diag(BodyDiag::AssertArgNumMismatch {
                primary: expr.span(self.body()).into_macro_call_expr().args().into(),
                given: args.len(),
            });
            return ExprProp::invalid(self.db);
        }

        self.check_expr(args[0].expr, TyId::bool(self.db));
        if let Some(message_arg) = args.get(1) {
            let message = message_arg.expr;
            let Partial::Present(Expr::Lit(LitKind::String(string_id))) =
                message.data(self.db, self.body())
            else {
                self.check_expr_unknown(message);
                self.push_diag(BodyDiag::AssertMessageMustBeStringLiteral {
                    primary: message.span(self.body()).into(),
                });
                return ExprProp::invalid(self.db);
            };

            let expected = self.string_literal_byte_array_ty(string_id.len_bytes(self.db));
            self.check_expr(message, expected);
        }
        ExprProp::new(TyId::unit(self.db), true)
    }

    fn check_code_region_intrinsic(
        &mut self,
        expr: ExprId,
        callable: &mut Callable<'db>,
        args: &[crate::hir_def::CallArg<'db>],
        kind: CodeRegionIntrinsicKind,
        receiver: Option<(ExprId, ExprProp<'db>)>,
        direct_callee: Option<ExprId>,
    ) -> Option<ExprProp<'db>> {
        let arg_expr = args[0].expr;
        callable.check_args(
            self,
            args,
            expr.span(self.body()).into_call_expr().args(),
            receiver,
            false,
        );
        let arg_ty = self.env.typed_expr(arg_expr).map_or_else(
            || TyId::invalid(self.db, InvalidCause::Other),
            |prop| self.normalize_ty(prop.ty),
        );
        if !ty_may_be_code_region_token(self.db, arg_ty) {
            return None;
        }
        if let Some(callee) = direct_callee {
            self.env
                .type_expr(callee, ExprProp::new(callable.ty(self.db), true));
        }
        self.env
            .register_code_region_intrinsic(expr, callable.clone(), arg_expr, kind);
        Some(ExprProp::new(TyId::u256(self.db), true))
    }

    pub(super) fn check_callable_effects(&mut self, expr: ExprId, callable: &mut Callable<'db>) {
        if self.rows_awaiting_inference(callable).is_empty() {
            self.resolve_call_effects(expr, callable);
        } else {
            let site = self.env.call_site();
            self.env.register_pending_effects(PendingEffects {
                target: PendingEffectsTarget::Call(expr),
                site,
            });
        }
    }

    fn resolve_call_effects(&mut self, expr: ExprId, callable: &mut Callable<'db>) {
        let call_span: DynLazySpan<'db> = expr.span(self.body()).into();
        let args = self.resolve_callable_effects(call_span, callable, Some(expr));
        for arg in args {
            self.env.push_call_effect_arg(expr, arg);
        }
    }

    /// The rows of `callable`'s callee that stay abstract only because
    /// inference hasn't finished with their instances, so whether their
    /// implementations give them effects isn't known yet.
    pub(super) fn rows_awaiting_inference(&mut self, callable: &Callable<'db>) -> Vec<RowKey<'db>> {
        let CallableDef::Func(func) = callable.callable_def else {
            return Vec::new();
        };
        if !func.has_effects(self.db) {
            return Vec::new();
        }
        let rows = self.callee_rows(callable, func);
        rows.abstract_rows
            .iter()
            .map(|&(_, row)| row)
            .chain(
                rows.components
                    .iter()
                    .filter_map(|component| component.requirement.key.key_row()),
            )
            .filter(|row| row.inst.args(self.db).iter().any(|ty| ty.has_var(self.db)))
            .collect()
    }

    /// Resolves the effects `pending` waits for, at its call site. If its rows
    /// still wait, registers it again and returns false, unless `force`, which
    /// binds each open literal in them to its fallback type: their effects
    /// depend on that type, so it can't change afterwards.
    pub(super) fn resolve_pending_effects(
        &mut self,
        pending: PendingEffects<'db>,
        force: bool,
    ) -> bool {
        let callables: Vec<_> = match pending.target {
            PendingEffectsTarget::Call(expr) => {
                self.env.callable_expr(expr).into_iter().cloned().collect()
            }
            PendingEffectsTarget::Loop(stmt) => self
                .env
                .for_loop_plan(stmt)
                .into_iter()
                .flat_map(|plan| &plan.calls)
                .map(|call| call.callable.clone())
                .collect(),
        };
        let waiting: Vec<_> = callables
            .iter()
            .flat_map(|callable| self.rows_awaiting_inference(callable))
            .collect();
        if !force && !waiting.is_empty() {
            self.env.register_pending_effects(pending);
            return false;
        }
        let scope = self.env.scope();
        for &arg in waiting.iter().flat_map(|row| row.inst.args(self.db)) {
            let settled = arg.fold_with(self.db, &mut Prober::new(&mut self.table, scope));
            self.table.unify(arg, settled).ok();
        }
        let enclosing_choices = self.env.enter_call_site(&pending.site);
        match pending.target {
            PendingEffectsTarget::Call(expr) => {
                for mut callable in callables {
                    self.resolve_call_effects(expr, &mut callable);
                    self.env
                        .set_call_effect_providers(expr, callable.effect_providers());
                }
            }
            PendingEffectsTarget::Loop(stmt) => {
                if let Some(plan) = self.env.for_loop_plan(stmt) {
                    let span: DynLazySpan<'db> = plan.base.span(self.body()).into();
                    for (step, mut callable) in callables.into_iter().enumerate() {
                        let effect_args =
                            self.resolve_callable_effects(span.clone(), &mut callable, None);
                        self.env.set_for_loop_call(
                            stmt,
                            step,
                            ForLoopCall {
                                callable,
                                effect_args,
                            },
                        );
                    }
                }
            }
        }
        self.env.leave_call_site(enclosing_choices);
        true
    }

    /// Resolves the effects `callable` requires where it is called. A missing
    /// effect of a `mut?` call at `ref`, `call`, is reported once the call's
    /// mode is final.
    pub(super) fn resolve_callable_effects(
        &mut self,
        call_span: DynLazySpan<'db>,
        callable: &mut Callable<'db>,
        call: Option<ExprId>,
    ) -> Vec<super::ResolvedEffectArg<'db>> {
        let CallableDef::Func(func) = callable.callable_def else {
            return Vec::new();
        };

        if !func.has_effects(self.db) {
            return Vec::new();
        }

        let mut resolved_args: Vec<super::ResolvedEffectArg<'db>> = Vec::new();
        let mut specialized_providers: FxHashMap<u32, EffectProviderSpecialization<'db>> =
            FxHashMap::default();

        let callee_provider_arg_idx_by_effect =
            place_effect_provider_param_index_map(self.db, func);
        let callee_site = EffectParamSite::Func(func, callable.mode());
        let callee_effect_env = crate::core::semantic::EffectEnvView::new(callee_site);

        // Each component of a callee row the call expands is resolved like the
        // callee's own effects, and named by its path.
        let rows = self.callee_rows(callable, func);
        let reqs: Vec<(Option<&RowPath>, _)> =
            effect_requirement_decls_for_callable(self.db, callable.callable_def, callable.mode())
                .into_iter()
                .map(|req| (None, req))
                .chain(rows.components.iter().filter_map(|component| {
                    let req = EffectRequirementDecl::from_effect_requirement(
                        self.db,
                        &component.requirement,
                    )?;
                    Some((Some(&component.path), req))
                }))
                .collect();
        let in_drop = self.env.in_drop();
        for &(path, ref req) in &reqs {
            let key_ty = req.key_ty;
            let mode = if req.required_mut {
                ModeMarker::Mut
            } else {
                ModeMarker::None
            };
            let missing = || BodyDiag::MissingEffect {
                primary: call_span.clone(),
                func,
                key: key_ty,
                mode,
                in_drop,
            };
            // A `mut?` call at `ref` may be raised, so its missing effect is
            // reported once its mode is final.
            let defer = call
                .filter(|_| callable.mode() == Some(BorrowKind::Ref) && path.is_none())
                .map(|call| (call, req.binding_idx));
            let Some(query) = build_effect_query_for_call(self, callable, req) else {
                continue;
            };
            let provider_arg_idx_for_param = path
                .is_none()
                .then(|| callee_provider_arg_idx_by_effect.get(req.binding_idx as usize))
                .flatten()
                .copied()
                .flatten();

            let mut resolution = self.resolve_effect_query(query.clone(), call_span.clone());
            if matches!(resolution, EffectResolution::Missing)
                && self.provide_closure_effect(req, &query)
            {
                resolution = self.resolve_effect_query(query.clone(), call_span.clone());
            }
            match resolution {
                EffectResolution::Chosen(evidence) => {
                    let EffectEvidence {
                        provider,
                        target_ty: instantiated_key_ty,
                        commit,
                    } = *evidence;
                    let committed = self.apply_effect_commit_plan(commit);
                    debug_assert!(committed, "chosen effect evidence commit failed");
                    if !committed {
                        self.push_missing_effect(defer, missing());
                        continue;
                    }

                    if req.required_mut
                        && !self.require_mut_provider(call_span.clone(), func, key_ty, provider)
                    {
                        continue;
                    }
                    // A raise to `mut` requires the provider as this does.
                    if let Some(call) = call
                        && callable.mode() == Some(BorrowKind::Ref)
                        && path.is_none()
                    {
                        self.effect_providers
                            .insert((call, req.binding_idx), provider);
                    }
                    let arg = match provider.origin {
                        EffectOrigin::With { value_expr, .. } => EffectArg::With(value_expr),
                        EffectOrigin::Param(binding) => EffectArg::Binding(binding),
                    };
                    // A type key's provider is the place it names, or a
                    // temporary; a trait key's is the provider as it is.
                    let place = instantiated_key_ty.and_then(|_| self.provider_place(provider));
                    let provider_target_ty =
                        instantiated_key_ty.map(|ty| self.table.fold_ty(self.db, ty));
                    // A trait key's provider is of the callee's provider
                    // parameter for the effect.
                    if instantiated_key_ty.is_none()
                        && let Some(provider_arg_idx) = provider_arg_idx_for_param
                        && let Some(provider_var) =
                            callable.generic_args().get(provider_arg_idx).copied()
                    {
                        let given = self.table.fold_ty(self.db, provider.ty);
                        let existing_provider = self.table.fold_ty(self.db, provider_var);
                        let snapshot = self.table.snapshot();
                        if self.table.unify(provider_var, given).is_err() {
                            self.table.rollback_to(snapshot);
                            self.push_diag(BodyDiag::EffectProviderMismatch {
                                primary: call_span.clone(),
                                func,
                                key: key_ty,
                                expected: existing_provider,
                                given,
                                provided_span: Some(provider.origin.span(self.db, self.body())),
                            });
                        }
                    }
                    let provider_space = match (&place, arg) {
                        (Some(place), _) => self.concrete_borrow_provider_for_place(place),
                        (None, EffectArg::With(value)) => {
                            self.env.typed_expr(value).and_then(|prop| {
                                prop.borrow_provider.or_else(|| {
                                    self.concrete_borrow_provider_for_capability_ty(prop.ty)
                                })
                            })
                        }
                        (None, EffectArg::Binding(binding)) => {
                            self.concrete_borrow_provider_for_binding(binding)
                        }
                    }
                    .or_else(|| self.concrete_borrow_provider_for_capability_ty(provider.ty))
                    // A temporary lies in memory.
                    .or_else(|| {
                        (place.is_none() && instantiated_key_ty.is_some())
                            .then_some(AddressSpace::Memory)
                    });
                    // A declared effect's provider has a slot in every view
                    // of the callee. A component's is given one by each
                    // instance of the call.
                    let target = if let Some(path) = path {
                        EffectArgTarget::Component {
                            path: path.clone(),
                            provider: self.effect_arg_provider(
                                provider,
                                place.as_ref(),
                                provider_target_ty,
                                provider_space,
                            ),
                        }
                    } else {
                        if let Some(binding) =
                            callee_effect_env.resolved_binding(self.db, req.binding_idx as usize)
                        {
                            let provider_idx = binding.provider.provider_idx;
                            let specialization = self
                                .effect_arg_provider(
                                    provider,
                                    place.as_ref(),
                                    provider_target_ty,
                                    provider_space,
                                )
                                .in_slot(provider_idx, binding.provider.source);
                            if let Some(previous) =
                                specialized_providers.insert(provider_idx, specialization.clone())
                            {
                                assert_eq!(
                                    previous, specialization,
                                    "conflicting call-site provider specialization for function effect provider slot {provider_idx} at {key_ty:?}",
                                );
                            }
                        }
                        EffectArgTarget::Declared(req.binding_idx)
                    };
                    resolved_args.push(super::ResolvedEffectArg {
                        target,
                        key: key_ty,
                        arg,
                        required_mut: req.required_mut,
                        key_kind: if instantiated_key_ty.is_some() {
                            EffectKeyKind::Type
                        } else {
                            EffectKeyKind::Trait
                        },
                        instantiated_key_ty,
                        provider_target_ty,
                        provider: provider_space,
                    });
                }
                EffectResolution::BlockedByBarrier => {}
                EffectResolution::Missing => {
                    let diag = match self.env.late_closure() {
                        Some(closure) => BodyDiag::LateClosureEffect {
                            primary: call_span.clone(),
                            func,
                            key: key_ty,
                            is_mut: req.required_mut,
                            closure: closure.expr.span(self.body()).into(),
                        },
                        None => missing(),
                    };
                    self.push_missing_effect(defer, diag);
                }
                EffectResolution::Ambiguous => {
                    self.push_diag(BodyDiag::AmbiguousEffect {
                        primary: call_span.clone(),
                        func,
                        key: key_ty,
                    });
                }
            }
        }
        // A row that stays abstract is the caller's own row, forwarded.
        let effects = func.effect_requirements(self.db, callable.mode());
        let forwarded = rows
            .abstract_rows
            .iter()
            .map(|&(entry, row)| (&effects[entry as usize], RowPath::entry(entry), row))
            .chain(rows.components.iter().filter_map(|component| {
                let row = component.requirement.key.key_row()?;
                Some((&component.requirement, component.path.clone(), row))
            }));
        for (requirement, path, row) in forwarded {
            match self.own_row_binding(requirement, row) {
                Some((binding, own)) => resolved_args.push(super::ResolvedEffectArg {
                    target: EffectArgTarget::Forwarded { callee: path, own },
                    key: requirement.binding_ty,
                    arg: EffectArg::Binding(binding),
                    required_mut: requirement.is_mut,
                    key_kind: EffectKeyKind::Row,
                    instantiated_key_ty: None,
                    provider_target_ty: None,
                    provider: None,
                }),
                // A row of an instance no implementation satisfies is the
                // unsatisfied bound's error, reported once.
                None if matches!(
                    is_goal_satisfiable(self.db, self.env.solve_cx(), row.inst),
                    GoalSatisfiability::UnSat(_)
                ) => {}
                None => self.push_diag(match self.env.late_closure() {
                    Some(closure) => BodyDiag::LateClosureEffect {
                        primary: call_span.clone(),
                        func,
                        key: requirement.binding_ty,
                        is_mut: requirement.is_mut,
                        closure: closure.expr.span(self.body()).into(),
                    },
                    None => BodyDiag::MissingRow {
                        primary: call_span.clone(),
                        func,
                        row,
                        in_drop: self.env.in_drop(),
                    },
                }),
            }
        }
        let mut providers = specialized_providers.into_values().collect::<Vec<_>>();
        providers.sort_by_key(|provider| provider.provider.provider_idx);
        *callable.effect_providers_mut() = providers;

        resolved_args
    }

    /// The callee's rows as this call instantiates them.
    fn callee_rows(&mut self, callable: &Callable<'db>, func: Func<'db>) -> RowExpansion<'db> {
        let requirements: Vec<_> = func
            .effect_requirements(self.db, callable.mode())
            .iter()
            .map(|requirement| match requirement.key {
                SemanticEffectRequirementKey::Row(row) => {
                    let inst = instantiate_trait_effect_key(self.db, row.inst, callable)
                        .fold_with(self.db, &mut self.table);
                    let inst = normalize_effect_identity_trait(
                        self.db,
                        inst,
                        self.env.scope(),
                        self.env.assumptions(),
                        callable.trait_inst(),
                    );
                    EffectRequirement {
                        key: SemanticEffectRequirementKey::Row(RowKey { inst, row: row.row }),
                        ..requirement.clone()
                    }
                }
                _ => requirement.clone(),
            })
            .collect();
        expand_rows(
            self.db,
            &requirements,
            self.env.scope(),
            self.env.assumptions(),
        )
    }

    /// The caller's own effect binding that forwards the abstract row `row`,
    /// with where `row` sits in it: an entry naming it, or an entry whose row
    /// expands to it here.
    fn own_row_binding(
        &mut self,
        requirement: &EffectRequirement<'db>,
        row: RowKey<'db>,
    ) -> Option<(LocalBinding<'db>, RowPath)> {
        // In a closure's body, a row is a component of the closure's row.
        if self.env.in_closure() || self.env.late_closure().is_some() {
            let (binding, _) = self.env.closure_effect(
                SemanticEffectRequirementKey::Row(row),
                requirement.binding_name,
                requirement.binding_ty,
                requirement.is_mut,
            )?;
            let LocalBinding::EffectParam { idx, .. } = binding else {
                return None;
            };
            return Some((binding, RowPath::entry(idx as u32)));
        }
        let super::BodyOwner::Func(caller, mode) = self.env.owner() else {
            return None;
        };
        let (scope, assumptions) = (self.env.scope(), self.env.assumptions());
        let names_row = |requirement: &EffectRequirement<'db>| {
            requirement.key.key_row().is_some_and(|own| {
                own.row == row.row
                    && normalize_effect_identity_trait(self.db, own.inst, scope, assumptions, None)
                        == row.inst
            })
        };
        let requirements = caller.effect_requirements(self.db, mode);
        let path = match requirements
            .iter()
            .find(|requirement| names_row(requirement))
        {
            Some(requirement) => RowPath::entry(requirement.binding_idx),
            None => {
                expand_rows(self.db, requirements, scope, assumptions)
                    .components
                    .into_iter()
                    .find(|component| names_row(&component.requirement))?
                    .path
            }
        };
        let entry = requirements
            .iter()
            .find(|requirement| requirement.binding_idx == path.entry)?;
        let binding = LocalBinding::EffectParam {
            site: entry.binding_site,
            idx: entry.binding_idx as usize,
            binding_name: entry.binding_name,
            provider_idx: entry.binding_idx,
            is_mut: entry.is_mut,
        };
        Some((binding, path))
    }

    /// The place a provider names: its `with` value's or effect binding's.
    fn provider_place(&self, provider: ProvidedEffect<'db>) -> Option<Place<'db>> {
        match provider.origin {
            EffectOrigin::With { value_expr, .. } => self.env.expr_place(value_expr),
            EffectOrigin::Param(binding) => Some(Place::new(PlaceBase::Binding(binding))),
        }
    }

    /// Whether the function's own authority covers the resources the `with`
    /// value `value` names: its place lies in an effect provider.
    fn provider_covered(&self, value: ExprId) -> bool {
        self.env.expr_place(value).is_some_and(|place| {
            let PlaceBase::Binding(binding) = place.base;
            self.env.binding_has_authority(&binding)
        })
    }

    /// Resolves the requirement `query` at the call `span`. Which provider
    /// answers it is a preference between implementations: the innermost
    /// `with` frame with a provider that implements the required trait. It is
    /// decided among the implementations written in the package and its
    /// dependencies, and in the full tier only where those answer nothing. A
    /// generated implementation that would make another provider answer is
    /// refused where it is requested.
    fn resolve_effect_query(
        &mut self,
        query: EffectQuery<'db>,
        span: DynLazySpan<'db>,
    ) -> EffectResolution<'db> {
        let ingot = self.env.scope().ingot(self.db);
        let choice = choose_in_tiers(
            self.db,
            ingot,
            |tier, comparing| {
                // An answer that is only compared must leave no trace.
                let snapshot = comparing.then(|| self.snapshot_state());
                let resolution = self.resolve_effect_query_in(query.clone(), tier);
                if let Some(snapshot) = snapshot {
                    self.rollback_state(snapshot);
                }
                resolution
            },
            |resolution| !matches!(resolution, EffectResolution::Missing),
            |a, b| match (a, b) {
                (EffectResolution::Chosen(a), EffectResolution::Chosen(b)) => {
                    a.provider == b.provider
                }
                (EffectResolution::BlockedByBarrier, EffectResolution::BlockedByBarrier)
                | (EffectResolution::Missing, EffectResolution::Missing)
                | (EffectResolution::Ambiguous, EffectResolution::Ambiguous) => true,
                _ => false,
            },
        );
        if let Some(changed) = &choice.changed_to {
            let (trait_, ty) = match (&query.key, changed) {
                (EffectPatternKey::Trait(pattern), EffectResolution::Chosen(evidence)) => {
                    (Some(pattern.def), evidence.provider.ty)
                }
                (EffectPatternKey::Trait(pattern), _) => {
                    let provider = match &choice.chosen {
                        EffectResolution::Chosen(evidence) => evidence.provider.ty,
                        _ => TyId::invalid(self.db, InvalidCause::Other),
                    };
                    (Some(pattern.def), provider)
                }
                (EffectPatternKey::Type(_), _) => {
                    (None, TyId::invalid(self.db, InvalidCause::Other))
                }
            };
            self.push_changed_choice(BodyDiag::GenerationChangesChoice {
                primary: span,
                decision: Decision::EffectProvider,
                trait_,
                subject: ty.pretty_print(self.db).to_string(),
                ty: Some(ty),
                generated: None,
            });
        }
        choice.chosen
    }

    fn resolve_effect_query_in(
        &mut self,
        query: EffectQuery<'db>,
        tier: Tier,
    ) -> EffectResolution<'db> {
        let mut viable: SmallVec<[EffectEvidence<'db>; 2]> = SmallVec::new();
        let effect_env = self.env.effect_env().clone();
        for frame in effect_env.lookup_effect_frames(&query, self) {
            match frame {
                FrameLookupResult::KeyedMatched {
                    entries,
                    blocked_by_barrier,
                } => {
                    viable.extend(entries.iter().cloned().map(keyed_effect_evidence));
                    if viable.is_empty() {
                        return if blocked_by_barrier {
                            EffectResolution::BlockedByBarrier
                        } else {
                            EffectResolution::Missing
                        };
                    }
                    return choose_effect_evidence(viable);
                }
                FrameLookupResult::KeyedFamily { entries, providers } => {
                    let mut family_viable: SmallVec<[EffectEvidence<'db>; 2]> =
                        entries.iter().cloned().map(keyed_effect_evidence).collect();
                    for provider in providers.iter().copied() {
                        let evidence = match query.key.clone() {
                            EffectPatternKey::Type(type_query) => {
                                self.evaluate_unkeyed_type_provider(type_query, provider)
                            }
                            EffectPatternKey::Trait(trait_query) => {
                                self.evaluate_unkeyed_trait_provider(trait_query, provider, tier)
                            }
                        };
                        if let Some(evidence) = evidence {
                            family_viable.push(evidence);
                        }
                    }
                    if !family_viable.is_empty() {
                        return choose_effect_evidence(family_viable);
                    }
                }
                FrameLookupResult::Unkeyed { providers } => {
                    for provider in providers {
                        let evidence = match query.key.clone() {
                            EffectPatternKey::Type(type_query) => {
                                self.evaluate_unkeyed_type_provider(type_query, provider)
                            }
                            EffectPatternKey::Trait(trait_query) => {
                                self.evaluate_unkeyed_trait_provider(trait_query, provider, tier)
                            }
                        };
                        if let Some(evidence) = evidence {
                            viable.push(evidence);
                        }
                    }
                    if !viable.is_empty() {
                        return choose_effect_evidence(viable);
                    }
                }
            }
        }
        EffectResolution::Missing
    }

    fn evaluate_unkeyed_type_provider(
        &mut self,
        query: TypePatternKey<'db>,
        provider: ProvidedEffect<'db>,
    ) -> Option<EffectEvidence<'db>> {
        let key_match = KeyMatchCommit::QueryToType {
            query,
            actual: provider.ty,
        };
        let snapshot = self.snapshot_state();
        let ok = apply_key_match_commit(self, key_match.clone());
        self.rollback_state(snapshot);
        ok.then(|| EffectEvidence {
            provider,
            target_ty: Some(provider.ty),
            commit: EffectCommitPlan {
                key_match: Some(key_match),
                trait_solutions: SmallVec::new(),
            },
        })
    }

    fn evaluate_unkeyed_trait_provider(
        &mut self,
        query: TraitPatternKey<'db>,
        provider: ProvidedEffect<'db>,
        tier: Tier,
    ) -> Option<EffectEvidence<'db>> {
        let provider_ty = self.table.fold_ty(self.db, provider.ty);
        if provider_ty.has_var(self.db) {
            return None;
        }
        let instantiated = instantiate_trait_pattern_in(self.db, &mut self.table, query);
        let trait_goal = instantiate_trait_self(self.db, instantiated, provider_ty);
        let GoalSatisfiability::Satisfied(solution) =
            self.trait_effect_goal_satisfiability(tier, trait_goal)
        else {
            return None;
        };
        Some(EffectEvidence {
            provider,
            target_ty: None,
            commit: EffectCommitPlan {
                key_match: None,
                trait_solutions: SmallVec::from_iter([(trait_goal, solution)]),
            },
        })
    }

    /// Requires `provider`, which passes the effect keyed `key` to a call of
    /// `func`, at `span`, that takes it `mut`, to be mutable: a closure's
    /// effect it provides becomes `mut`, unless the closure's row is already
    /// fixed. Returns whether it is mutable.
    pub(super) fn require_mut_provider(
        &mut self,
        span: DynLazySpan<'db>,
        func: Func<'db>,
        key: HirTyId<'db>,
        provider: ProvidedEffect<'db>,
    ) -> bool {
        if let Some(LocalBinding::EffectParam {
            site: EffectParamSite::Closure(closure),
            idx,
            ..
        }) = provider.binding()
            && !self.env.require_mut_closure_effect(closure, idx)
        {
            self.push_diag(BodyDiag::LateClosureEffect {
                primary: span,
                func,
                key,
                is_mut: true,
                closure: closure.expr.span(self.body()).into(),
            });
            return false;
        }
        if !provider.is_mut {
            self.push_diag(BodyDiag::EffectMutabilityMismatch {
                primary: span,
                func,
                key,
                provided_span: Some(provider.origin.span(self.db, self.body())),
                read_only: provider
                    .binding()
                    .filter(|&binding| self.binding_is_maybe_mut(binding))
                    .map(|binding| ReadOnlyProvider::Binding(binding.binding_name(&self.env)))
                    .or_else(|| match provider.origin {
                        EffectOrigin::With { value_expr, .. } => self
                            .ref_maybe_mut_callee(value_expr)
                            .map(ReadOnlyProvider::Call),
                        EffectOrigin::Param(_) => None,
                    }),
                split: self
                    .split_demand
                    .map(|split| split.span(self.body()).into()),
            });
        }
        provider.is_mut
    }

    /// The `mut?` function `expr`, or the tail of the block it is, calls at
    /// `ref`: `mut` before the call would call it at `mut`.
    fn ref_maybe_mut_callee(&self, expr: ExprId) -> Option<Func<'db>> {
        match self.env.expr_data(expr) {
            Partial::Present(Expr::Block(stmts, _)) => {
                let Partial::Present(Stmt::Expr(tail)) = stmts.last()?.data(self.db, self.body())
                else {
                    return None;
                };
                self.ref_maybe_mut_callee(*tail)
            }
            Partial::Present(Expr::Call(..) | Expr::MethodCall(..)) => {
                let callable = self.env.callable_expr(expr)?;
                match callable.callable_def() {
                    CallableDef::Func(func) if callable.mode() == Some(BorrowKind::Ref) => {
                        Some(func)
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Reports a missing effect, or keeps it until the mode of the call
    /// `defer` names is final, as its effect's slot there.
    fn push_missing_effect(&mut self, defer: Option<(ExprId, u32)>, diag: BodyDiag<'db>) {
        match defer {
            Some((call, binding_idx)) => self.missing_effects.push((call, binding_idx, diag)),
            None => self.push_diag(diag),
        }
    }

    /// The provider a call gives a callee effect for `provided`, passed as
    /// `place` if it names one: the caller's own, passed on, or one the call
    /// makes.
    fn effect_arg_provider(
        &mut self,
        provided: ProvidedEffect<'db>,
        place: Option<&Place<'db>>,
        provider_target_ty: Option<TyId<'db>>,
        provider_space: Option<AddressSpace>,
    ) -> EffectArgProvider<'db> {
        // A part of a binding is a provider of its own.
        let binding = match place {
            Some(place) => {
                let PlaceBase::Binding(binding) = place.base;
                place.projections.is_empty().then_some(binding)
            }
            None => provided.binding(),
        };
        let owner = self.env.owner();
        let provenance = match (binding, provided.origin) {
            (Some(binding), _) | (None, EffectOrigin::Param(binding)) => {
                EffectProviderProvenance::Binding { owner, binding }
            }
            (None, EffectOrigin::With { value_expr, .. }) => EffectProviderProvenance::Expr {
                owner,
                expr: value_expr,
            },
        };
        let target_ty = provider_target_ty.map(|ty| self.table.fold_ty(self.db, ty));
        let existing = match binding {
            Some(LocalBinding::EffectParam {
                site, provider_idx, ..
            }) => self.env.provider_binding(site, provider_idx),
            Some(LocalBinding::Param {
                site: ParamSite::EffectField(effect_site),
                idx,
                ..
            }) => self.env.resolved_provider_binding(effect_site, idx),
            _ => None,
        };
        if let Some(provider) = existing.filter(|provider| {
            target_ty.is_none_or(|target_ty| provider.effective_target_ty() == target_ty)
        }) {
            return EffectArgProvider {
                provider_ty: provider.provider_ty,
                is_mut: provider.is_mut,
                semantics: provider.semantics,
                source: Some(provider.source),
                provenance,
            };
        }
        // A type key's provider is a place of the key's type.
        let provider_ty = target_ty.unwrap_or_else(|| self.table.fold_ty(self.db, provided.ty));
        let semantics = provider_semantics_for_specialized_call(
            self.db,
            provider_ty,
            target_ty,
            provider_space,
        );
        EffectArgProvider {
            provider_ty,
            is_mut: provided.is_mut,
            semantics,
            source: None,
            provenance,
        }
    }

    /// Records a decision that generated implementations would change. Only a
    /// decision that written code makes is reported: a body that generation
    /// produced may use generated implementations, and nothing written changes.
    pub(super) fn push_changed_choice(&mut self, diag: BodyDiag<'db>) {
        let ingot = self.env.scope().ingot(self.db);
        if !self.env.owner().scope().is_within_generated(self.db, ingot) {
            self.push_diag(diag);
        }
    }

    pub(super) fn trait_effect_goal_satisfiability(
        &self,
        tier: Tier,
        trait_goal: TraitInstId<'db>,
    ) -> GoalSatisfiability<'db> {
        self.trait_effect_goal_satisfiability_in_scope(
            tier,
            self.env.scope(),
            self.env.assumptions(),
            trait_goal,
        )
    }

    pub(super) fn trait_effect_goal_satisfiability_in_scope(
        &self,
        tier: Tier,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        trait_goal: TraitInstId<'db>,
    ) -> GoalSatisfiability<'db> {
        let solve_cx = TraitSolveCx::new(self.db, scope, tier)
            .with_assumptions(assumptions)
            .with_choices(self.env.choices);
        let query = solve_cx.query(self.db, trait_goal);
        crate::analysis::ty::trait_resolution::is_goal_query_satisfiable(self.db, solve_cx, &query)
    }

    pub(super) fn commit_trait_goal_solution(
        &mut self,
        trait_goal: TraitInstId<'db>,
        solution: Solution<TraitGoalSolution<'db>>,
    ) -> TraitGoalSolution<'db> {
        let canonical_goal = Canonicalized::new(self.db, trait_goal);
        let solved = canonical_goal.extract_solution(&mut self.table, solution);
        self.table.unify(trait_goal, solved.inst).unwrap();
        solved
    }

    /// The provided type, when it matches the type key `pattern`.
    pub(super) fn apply_effect_commit_plan(&mut self, commit: EffectCommitPlan<'db>) -> bool {
        let snapshot = self.snapshot_state();
        for (goal, solution) in commit.trait_solutions {
            self.commit_trait_goal_solution(goal, solution);
        }
        let ok = commit
            .key_match
            .is_none_or(|key_match| apply_key_match_commit(self, key_match));
        if ok {
            self.commit_state(snapshot);
        } else {
            self.rollback_state(snapshot);
        }
        ok
    }

    fn validate_keyed_with(
        &mut self,
        key_path: PathId<'db>,
        provider: ProvidedEffect<'db>,
        span: DynLazySpan<'db>,
        emit_diag: bool,
    ) -> Result<
        (
            EffectWitness<'db, ProvidedEffect<'db>>,
            EffectCommitPlan<'db>,
        ),
        Box<EffectPatternKey<'db>>,
    > {
        let Some(pattern) = build_barrier_pattern_for_with_key(self, key_path) else {
            let fallback = build_conservative_same_family_barrier_pattern_in_scope(
                self.db,
                self.env.scope(),
                self.env.assumptions(),
                key_path,
            );
            if let Some(EffectPatternKey::Trait(pattern)) = fallback.clone() {
                if emit_diag {
                    self.push_diag(BodyDiag::WithEffectTraitUnsatisfied {
                        primary: span,
                        key: key_path,
                        trait_req: instantiate_trait_self(
                            self.db,
                            TraitKeySchema {
                                def: pattern.def,
                                args_no_self: pattern.args_no_self.clone(),
                                assoc_bindings: pattern.assoc_bindings.clone(),
                            }
                            .into_trait_inst(self.db),
                            provider.ty,
                        ),
                        given: provider.ty,
                    });
                }
                return Err(Box::new(EffectPatternKey::Trait(pattern)));
            }
            let expected = TyId::invalid(self.db, InvalidCause::Other);
            if emit_diag {
                self.push_diag(BodyDiag::WithEffectTypeUnsatisfied {
                    primary: span,
                    key: key_path,
                    expected,
                    given: provider.ty,
                });
            }
            return Err(Box::new(fallback.unwrap_or(EffectPatternKey::Type(
                TypePatternKey {
                    carrier: expected,
                    family: crate::analysis::ty::effects::effect_family_for_type(self.db, expected),
                    slots: crate::analysis::ty::effects::PatternSlots::empty(),
                },
            ))));
        };

        self.build_keyed_witness_from_pattern_in_scope(
            pattern,
            key_path,
            provider,
            span,
            KeyedWitnessBuildOptions {
                scope: KeyedWitnessBuildScope {
                    scope: self.env.scope(),
                    assumptions: self.env.assumptions(),
                },
                emit_diag,
                mode: WitnessBuildMode::ExplicitKeyedWith,
            },
        )
    }

    pub(super) fn build_keyed_witness_from_pattern_in_scope(
        &mut self,
        pattern: EffectPatternKey<'db>,
        key_path: PathId<'db>,
        provider: ProvidedEffect<'db>,
        span: DynLazySpan<'db>,
        options: KeyedWitnessBuildOptions<'db>,
    ) -> Result<
        (
            EffectWitness<'db, ProvidedEffect<'db>>,
            EffectCommitPlan<'db>,
        ),
        Box<EffectPatternKey<'db>>,
    > {
        let KeyedWitnessBuildOptions {
            scope: KeyedWitnessBuildScope { scope, assumptions },
            emit_diag,
            mode,
        } = options;
        match pattern {
            EffectPatternKey::Type(pattern) => {
                let explicit = matches!(mode, WitnessBuildMode::ExplicitKeyedWith);
                let key_match = KeyMatchCommit::QueryToType {
                    query: pattern.clone(),
                    actual: provider.ty,
                };
                let omits_arg = explicit
                    && pattern
                        .slots
                        .entries
                        .iter()
                        .any(|slot| slot.kind == PatternSlotKind::OmittedExplicitArg);
                // The key as the provider's type specializes it.
                let key = if omits_arg {
                    None
                } else {
                    let snapshot = self.snapshot_state();
                    let key = apply_key_match_commit(self, key_match.clone())
                        .then(|| self.table.fold_ty(self.db, provider.ty))
                        .and_then(|carrier| {
                            finalize_stored_effect_key(
                                self.db,
                                StoredEffectKey::Type(StoredTypeKey {
                                    carrier,
                                    family: pattern.family,
                                }),
                                scope,
                                assumptions,
                            )
                        });
                    self.rollback_state(snapshot);
                    key.filter(|key| {
                        !matches!(
                            key,
                            StoredEffectKey::Type(stored)
                                if explicit
                                    && (stored_value_contains_implicit_layout_params(
                                        self.db,
                                        stored.carrier,
                                    ) || stored_value_contains_out_of_scope_params(
                                        self.db,
                                        scope,
                                        stored.carrier,
                                    ))
                        )
                    })
                };
                let Some(key) = key else {
                    if emit_diag {
                        self.push_diag(BodyDiag::WithEffectTypeUnsatisfied {
                            primary: span,
                            key: key_path,
                            expected: pattern.carrier,
                            given: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectPatternKey::Type(pattern)));
                };
                Ok((
                    EffectWitness { key, provider },
                    EffectCommitPlan {
                        key_match: Some(key_match),
                        trait_solutions: SmallVec::new(),
                    },
                ))
            }
            EffectPatternKey::Trait(pattern) => {
                if matches!(mode, WitnessBuildMode::ExplicitKeyedWith)
                    && pattern
                        .slots
                        .entries
                        .iter()
                        .any(|slot| slot.kind == PatternSlotKind::OmittedExplicitArg)
                {
                    let trait_req = instantiate_trait_self(
                        self.db,
                        TraitKeySchema {
                            def: pattern.def,
                            args_no_self: pattern.args_no_self.clone(),
                            assoc_bindings: pattern.assoc_bindings.clone(),
                        }
                        .into_trait_inst(self.db),
                        provider.ty,
                    );
                    if emit_diag {
                        self.push_diag(BodyDiag::WithEffectTraitUnsatisfied {
                            primary: span.clone(),
                            key: key_path,
                            trait_req,
                            given: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectPatternKey::Trait(pattern)));
                }
                if provider.ty.has_var(self.db) {
                    if emit_diag {
                        self.push_diag(BodyDiag::TypeAnnotationNeeded {
                            span: span.clone(),
                            ty: provider.ty,
                            variadic: false,
                        });
                    }
                    return Err(Box::new(EffectPatternKey::Trait(pattern)));
                }
                let mut table = UnificationTable::new(self.db);
                let (instantiated, slot_bindings) = instantiate_trait_pattern_in_with_bindings(
                    self.db,
                    &mut table,
                    pattern.clone(),
                );
                let trait_goal = instantiate_trait_self(self.db, instantiated, provider.ty);
                let solution = match self.trait_effect_goal_satisfiability_in_scope(
                    Tier::All,
                    scope,
                    assumptions,
                    trait_goal,
                ) {
                    GoalSatisfiability::Satisfied(solution) => solution,
                    GoalSatisfiability::NeedsGeneration(request) if emit_diag => {
                        self.env.generation_demands.request(self.db, request);
                        self.push_diag(BodyDiag::unprepared_provider(span));
                        return Err(Box::new(EffectPatternKey::Trait(pattern)));
                    }
                    _ => {
                        if emit_diag {
                            self.push_diag(BodyDiag::WithEffectTraitUnsatisfied {
                                primary: span,
                                key: key_path,
                                trait_req: trait_goal,
                                given: provider.ty,
                            });
                        }
                        return Err(Box::new(EffectPatternKey::Trait(pattern)));
                    }
                };
                let canonical_goal = Canonicalized::new(self.db, trait_goal);
                let solved = canonical_goal.extract_solution(&mut table, solution);
                let solved_inst = complete_impl_trait(self.db, solved.implementor)
                    .trait_inst(self.db)
                    .fold_with(self.db, &mut table);
                let instantiated = reify_unresolved_pattern_slots(
                    self.db,
                    instantiated.fold_with(self.db, &mut table),
                    &slot_bindings,
                );
                let specialized = if matches!(mode, WitnessBuildMode::SeededRequirement)
                    && (stored_value_contains_implicit_layout_params(self.db, solved_inst)
                        || stored_value_contains_out_of_scope_params(self.db, scope, solved_inst))
                {
                    instantiated
                } else {
                    solved_inst
                };
                let Some(key) = finalize_stored_effect_key(
                    self.db,
                    StoredEffectKey::Trait(StoredTraitKey {
                        def: specialized.def(self.db),
                        args_no_self: specialized.args(self.db)[1..].iter().copied().collect(),
                        assoc_bindings: specialized
                            .assoc_ty_bindings(self.db)
                            .into_iter()
                            .collect(),
                        family: pattern.family,
                    }),
                    scope,
                    assumptions,
                ) else {
                    return Err(Box::new(EffectPatternKey::Trait(pattern)));
                };
                Ok((
                    EffectWitness { key, provider },
                    EffectCommitPlan {
                        key_match: None,
                        trait_solutions: SmallVec::new(),
                    },
                ))
            }
        }
    }

    /// Hides the key `pattern` of the invalid `with` binding keyed
    /// `key_path` from the frames outside it. A pattern that is not fully
    /// resolved hides its whole family.
    fn insert_effect_barrier(&mut self, key_path: PathId<'db>, pattern: EffectPatternKey<'db>) {
        let precise = !query_contains_unresolved_inference(self.db, &pattern)
            && !contains_projection_or_invalid_query_state(self.db, &pattern);
        let pattern = if precise {
            Some(pattern)
        } else {
            build_conservative_same_family_barrier_pattern_in_scope(
                self.db,
                self.env.scope(),
                self.env.assumptions(),
                key_path,
            )
        };
        if let Some(pattern) = pattern {
            self.env.effect_env_mut().insert_barrier(pattern);
        }
    }

    fn specialize_same_trait_method_inst(
        &mut self,
        method_name: IdentId<'db>,
        inst: TraitInstId<'db>,
    ) -> TraitInstId<'db> {
        let Some(CallableDef::Func(current_func)) = self.env.func() else {
            return inst;
        };
        if current_func.name(self.db).to_opt() != Some(method_name) {
            return inst;
        }

        let Some(enclosing_inst) = (match current_func.scope().parent_item(self.db) {
            Some(ItemKind::Trait(trait_)) => Some(TraitInstId::new_simple(
                self.db,
                trait_,
                trait_.params(self.db).to_vec(),
            )),
            Some(ItemKind::ImplTrait(impl_trait)) => impl_trait.trait_inst_result(self.db).ok(),
            _ => None,
        }) else {
            return inst;
        };

        if enclosing_inst.def(self.db) != inst.def(self.db) {
            return inst;
        }

        let enclosing_args = enclosing_inst.args(self.db);
        let inst_args = inst.args(self.db);
        if inst_args.len() != enclosing_args.len() || inst_args.is_empty() {
            return inst;
        }

        // The enclosing impl's arguments fill in what method selection left
        // open, as when forwarding to a field. They must not replace
        // arguments it already found, as for `self.inner.eq(other.inner)`
        // inside `impl<T> Eq for Wrap<T>`.
        let snapshot = self.snapshot_state();
        let compatible = inst_args[1..]
            .iter()
            .zip(&enclosing_args[1..])
            .all(|(&arg, &enclosing)| self.table.unify(arg, enclosing).is_ok());
        self.rollback_state(snapshot);
        if !compatible {
            return inst;
        }

        let mut args = inst_args.to_vec();
        args[1..].clone_from_slice(&enclosing_args[1..]);
        TraitInstId::new(
            self.db,
            inst.def(self.db),
            args,
            enclosing_inst.assoc_type_bindings(self.db).clone(),
            inst.mode(self.db),
        )
    }

    fn check_method_call(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::MethodCall(receiver, method_name, generic_args, args) = expr_data else {
            unreachable!()
        };
        let call_span = expr.span(self.body()).into_method_call_expr();
        let Some(method_name) = method_name.to_opt() else {
            return ExprProp::invalid(self.db);
        };

        let receiver_prop = self.check_expr_unknown(*receiver);
        let arg_diags = generic_arg_list_diags(
            self.db,
            *generic_args,
            call_span.clone().generic_args(),
            self.viewer(MemberSite::Expr(expr)),
            self.env.assumptions(),
        );
        let args_invalid = !arg_diags.is_empty();
        self.diags.extend(arg_diags.into_iter().map(Into::into));
        if args_invalid || receiver_prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let receiver_tys = self.receiver_candidates(receiver_prop.ty);
        let method_assumptions = self.env.assumptions();
        let method_choices = self.env.choices;

        let mut selected_receiver_ty = receiver_tys[0];
        let mut canonical_r_ty = Canonicalized::new(self.db, selected_receiver_ty);
        let mut candidate = self.select_method_in_scope(
            &canonical_r_ty,
            method_name,
            method_assumptions,
            method_choices,
            None,
        );
        if matches!(
            candidate,
            Err(MethodSelectionError::NotFound | MethodSelectionError::ReceiverTypeMustBeKnown)
        ) {
            for &receiver_ty in receiver_tys.iter().skip(1) {
                let fallback_canonical = Canonicalized::new(self.db, receiver_ty);
                let fallback = self.select_method_in_scope(
                    &fallback_canonical,
                    method_name,
                    method_assumptions,
                    method_choices,
                    None,
                );

                if fallback.is_ok() || !matches!(fallback, Err(MethodSelectionError::NotFound)) {
                    selected_receiver_ty = receiver_ty;
                    canonical_r_ty = fallback_canonical;
                    candidate = fallback;
                    break;
                }
            }
        }
        let candidate = match candidate {
            Ok(candidate) => candidate,
            Err(MethodSelectionError::AmbiguousTraitMethod(ambiguous)) => {
                return self.defer_method_call(
                    expr,
                    selected_receiver_ty,
                    &canonical_r_ty,
                    ambiguous,
                    args,
                    call_span.method_name().into(),
                );
            }
            Err(MethodSelectionError::NotFound)
                if self.is_callable_field(selected_receiver_ty, method_name) =>
            {
                let receiver = match receiver.data(self.db, self.body()) {
                    Partial::Present(Expr::Path(Partial::Present(path)))
                        if path.is_bare_ident(self.db) =>
                    {
                        path.ident(self.db).to_opt()
                    }
                    _ => None,
                };
                self.push_diag(BodyDiag::CallableFieldCalledAsMethod {
                    primary: call_span.method_name().into(),
                    field: method_name,
                    receiver_ty: selected_receiver_ty,
                    receiver,
                });
                return ExprProp::invalid(self.db);
            }
            Err(err) => {
                let diag = body_diag_from_method_selection_err(
                    self.db,
                    err,
                    Spanned::new(selected_receiver_ty, receiver.span(self.body()).into()),
                    Spanned::new(method_name, call_span.method_name().into()),
                );
                self.push_diag(diag);
                return ExprProp::invalid(self.db);
            }
        };

        // The selection sees the members of the package the code runs in. Code
        // that another package wrote may use only what `member_visible` allows.
        if let MethodCandidate::InherentMethod(method) = &candidate {
            let scope = method.def.scope();
            if !self.member_visible(scope, MemberSite::Expr(expr)) {
                self.push_diag(PathResDiag::Invisible(
                    call_span.method_name().into(),
                    method_name,
                    scope.name_span(self.db),
                ));
                return ExprProp::invalid(self.db);
            }
        }

        let (candidate, args_typed) = self.trait_method_for_arguments(
            candidate,
            &canonical_r_ty,
            selected_receiver_ty,
            method_name,
            args,
        );

        let (func_ty, trait_inst) = self.selected_method_ty(
            candidate,
            &canonical_r_ty,
            selected_receiver_ty,
            call_span.clone().into(),
        );

        let mut callable = match Callable::new(
            self.db,
            func_ty,
            receiver.span(self.body()).into(),
            trait_inst,
        ) {
            Ok(callable) => callable,
            Err(diag) => {
                self.push_diag(diag);
                return ExprProp::invalid(self.db);
            }
        };

        if !callable.unify_generic_args(self, *generic_args, call_span.clone().generic_args()) {
            return ExprProp::invalid(self.db);
        }

        if !callable.callable_def.is_method(self.db) {
            let diag = BodyDiag::NotAMethod {
                span: call_span,
                receiver_ty: receiver_prop.ty,
                func_name: method_name,
                func_ty,
            };
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        // The methods take no arguments; a call with any is checked as usual,
        // which reports them.
        if let Some(Shape::Sum { ty, payload, .. }) = &receiver_prop.shape
            && args.is_empty()
            && let Some(op) = self.sum_shape_op(callable.callable_def())
        {
            let (sum_ty, payload) = (*ty, (**payload).clone());
            return self.check_sum_shape_op(expr, *receiver, sum_ty, payload, callable, op);
        }

        if let Some(kind) = self
            .code_region_intrinsic_kind(callable.callable_def())
            .or_else(|| self.code_region_method_kind(selected_receiver_ty, method_name))
            && args.len() == 1
            && !args_typed
            && let Some(result) = self.check_code_region_intrinsic(
                expr,
                &mut callable,
                args,
                kind,
                Some((*receiver, receiver_prop.clone())),
                None,
            )
        {
            return result;
        }

        callable.check_args(
            self,
            args,
            call_span.clone().args(),
            Some((*receiver, receiver_prop)),
            args_typed,
        );

        callable.process_constraints(self, expr, call_span.method_name().into());
        self.check_callable_effects(expr, &mut callable);
        self.finish_call(expr, callable)
    }

    /// The trait method that a call `x.m(args)` runs when the package generates
    /// implementations: the method that the implementations written in the
    /// package and its dependencies select, unless its parameters do not
    /// accept the arguments and exactly one other candidate, supplied by a
    /// generated implementation, does. A generated implementation fills a gap
    /// here as it does for the operator `x == y`, which takes the right-hand
    /// side into account the same way. The arguments are typed before the
    /// choice then, and the result says so.
    fn trait_method_for_arguments(
        &mut self,
        candidate: MethodCandidate<'db>,
        canonical: &Canonicalized<'db, TyId<'db>>,
        receiver_ty: TyId<'db>,
        method_name: IdentId<'db>,
        args: &[HirCallArg<'db>],
    ) -> (MethodCandidate<'db>, bool) {
        let ingot = self.env.scope().ingot(self.db);
        let written = match candidate {
            MethodCandidate::TraitMethod(cand) | MethodCandidate::NeedsConfirmation(cand) => cand,
            MethodCandidate::InherentMethod(_) => return (candidate, false),
        };
        // A closure argument takes its types from the parameter it is passed
        // to, and so does a string literal (a byte array where the parameter
        // is one), so neither can be typed before the choice.
        if args.is_empty()
            || Tier::Authored.in_ingot(self.db, ingot) == Tier::All
            || args.iter().any(|arg| {
                matches!(
                    arg.expr.data(self.db, self.body()),
                    Partial::Present(
                        Expr::Closure { .. } | Expr::Lit(LitKind::String(_)) | Expr::Tuple(_)
                    )
                )
            })
        {
            return (candidate, false);
        }
        let others: Vec<MethodCandidate<'db>> = match select_method_candidate_in(
            self.db,
            canonical,
            method_name,
            self.env.scope(),
            self.env.assumptions(),
            self.env.choices,
            Tier::All,
        )
        .result
        {
            Ok(only) => vec![only],
            Err(MethodSelectionError::AmbiguousTraitMethod(ambiguous)) => ambiguous
                .candidates
                .iter()
                .map(|cand| {
                    if cand.needs_confirmation {
                        MethodCandidate::NeedsConfirmation(cand.cand)
                    } else {
                        MethodCandidate::TraitMethod(cand.cand)
                    }
                })
                .collect(),
            Err(_) => return (candidate, false),
        };
        // The other trait candidates, each as selected: one that needs its
        // trait bound confirmed keeps that, so the call still obliges it.
        let others: Vec<(MethodCandidate<'db>, TraitMethodCand<'db>)> = others
            .into_iter()
            .filter_map(|other| match other {
                MethodCandidate::TraitMethod(cand) | MethodCandidate::NeedsConfirmation(cand) => {
                    (cand != written).then_some((other, cand))
                }
                MethodCandidate::InherentMethod(_) => None,
            })
            .collect();
        if others.is_empty() {
            return (candidate, false);
        }
        for arg in args {
            if self.env.typed_expr(arg.expr).is_none() {
                self.check_expr_unknown(arg.expr);
            }
        }
        if self.trait_method_accepts(written, canonical, receiver_ty, args) {
            return (candidate, true);
        }
        let accepting: Vec<_> = others
            .into_iter()
            .filter(|&(_, other)| self.trait_method_accepts(other, canonical, receiver_ty, args))
            .collect();
        match accepting.as_slice() {
            [(only, _)] => (*only, true),
            _ => (candidate, true),
        }
    }

    /// Whether the arguments, already typed, unify with the parameters of the
    /// trait method `cand` called on `receiver_ty`. Leaves no trace.
    fn trait_method_accepts(
        &mut self,
        cand: TraitMethodCand<'db>,
        canonical: &Canonicalized<'db, TyId<'db>>,
        receiver_ty: TyId<'db>,
        args: &[HirCallArg<'db>],
    ) -> bool {
        let snapshot = self.snapshot_state();
        let accepted = (|| {
            let inst = canonical.extract_solution(&mut self.table, cand.inst);
            let func_ty = super::try_instantiate_trait_method(
                self.db,
                cand.method,
                &mut self.table,
                receiver_ty,
                inst,
            )
            .ok()?;
            let func_ty = self.table.instantiate_to_term(func_ty);
            let (base, gen_args) = func_ty.decompose_ty_app(self.db);
            let TyData::TyBase(TyBase::Func(func_def)) = base.data(self.db) else {
                return None;
            };
            let params = func_def.arg_tys(self.db);
            if params.len() != args.len() + 1 {
                return None;
            }
            for (arg, param) in args.iter().zip(params.iter().skip(1)) {
                let expected = self.normalize_ty(normalize_with_trait_evidence(
                    self.db,
                    param.instantiate(self.db, gen_args),
                    self.env.scope(),
                    inst,
                ));
                let given = self.env.typed_expr(arg.expr)?.ty;
                self.table.unify(given, expected).ok()?;
            }
            Some(())
        })()
        .is_some();
        self.rollback_state(snapshot);
        accepted
    }

    /// Registers a checked call and types its result. A projection's result
    /// is a place granted by the call's session; its shape says which parts
    /// are accesses.
    fn finish_call(&mut self, expr: ExprId, callable: Callable<'db>) -> ExprProp<'db> {
        self.check_drop_call(expr, callable.callable_def());
        let ret_ty = callable.ret_ty(self.db);
        let ret_ty = self.normalize_ty(ret_ty);
        // black_box returns its argument type. Diagnose known non-scalar
        // arguments here, while the source call span is still available.
        if let CallableDef::Func(func) = callable.callable_def()
            && Some(func)
                == resolve_lib_func_path(self.db, self.env.scope(), "core::hint::black_box")
            && (ret_ty.is_array(self.db)
                || ret_ty.is_tuple(self.db)
                || ret_ty.adt_ref(self.db).is_some()
                || ret_ty.as_ptr(self.db).is_some())
        {
            self.push_diag(BodyDiag::BlackBoxNonScalar {
                primary: expr.span(self.body()).into(),
                ty: ret_ty,
            });
            return ExprProp::invalid(self.db);
        }
        self.report_instantiated_const_fault(&callable, ret_ty, expr);
        let shape = callable
            .ret_shape(self.db)
            .map(|shape| shape.map_tys(self.db, &mut |ty| self.normalize_ty(ty)));
        if let Some(kind) = self.const_intrinsic_kind(callable.callable_def()) {
            if !self.check_and_register_const_intrinsic(expr, callable, kind) {
                return ExprProp::invalid(self.db);
            }
        } else {
            self.env.register_semantic_call(expr, callable);
        }
        ExprProp {
            is_mut: matches!(shape, None | Some(Shape::Access(BorrowKind::Mut, _))),
            shape,
            ..ExprProp::new(ret_ty, true)
        }
    }

    /// `Drop::drop` runs only where the compiler destroys a value.
    fn check_drop_call(&mut self, expr: ExprId, callee: CallableDef<'db>) {
        let CallableDef::Func(func) = callee else {
            return;
        };
        let trait_ = func.containing_trait(self.db).or_else(|| {
            lower_impl_trait(self.db, func.containing_impl_trait(self.db)?)
                .map(|implementor| implementor.trait_def(self.db))
        });
        if trait_.is_some()
            && trait_ == resolve_core_trait(self.db, self.env.scope(), &["ops", "Drop"])
        {
            self.push_diag(BodyDiag::DropCalledDirectly {
                primary: expr.span(self.body()).into(),
            });
        }
    }

    /// Selects a method under the lexical choices at this point of the body.
    /// Requests that candidate probes stopped on become demands of the body.
    fn select_method_in_scope(
        &mut self,
        receiver: &Canonicalized<'db, TyId<'db>>,
        method_name: IdentId<'db>,
        assumptions: PredicateListId<'db>,
        choices: Option<ChosenImpls<'db>>,
        trait_: Option<Trait<'db>>,
    ) -> Result<MethodCandidate<'db>, MethodSelectionError<'db>> {
        let outcome = select_method_candidate_with_choices(
            self.db,
            receiver,
            method_name,
            self.env.scope(),
            assumptions,
            choices,
            trait_,
        );
        self.env
            .generation_demands
            .requests(self.db, outcome.pending);
        outcome.result
    }

    fn check_path(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Path(path) = expr_data else {
            unreachable!()
        };

        let Partial::Present(path) = path else {
            return ExprProp::invalid(self.db);
        };
        let path = *path;

        let path_expr_span = expr.span(self.body()).into_path_expr();
        let path_span = path_expr_span.clone().path();

        let is_call_callee = self.env.parent_expr().is_some_and(|parent| {
            matches!(
                self.body().exprs(self.db)[parent],
                Partial::Present(Expr::Call(callee, _)) if callee == expr
            )
        });

        let idx = path.segment_index(self.db);
        let generic_args = path.generic_args(self.db);
        let generic_args_span = path_span.clone().segment(idx).generic_args();
        let lowering_cx = LoweringContext::new().with_method_choices(self.env.choices);
        let unify_generic_args = |tc: &mut Self, callable: &mut Callable<'db>| {
            callable.unify_generic_args(tc, generic_args, generic_args_span.clone())
        };

        let res = if path.is_bare_ident(self.db) {
            let ident_span: DynLazySpan<'db> = path_expr_span.clone().into();
            match resolve_ident_expr(self.db, &self.env, path, ident_span, &lowering_cx) {
                ResolvedPathInBody::Reso(reso) => {
                    ResolvedPathInBody::Reso(self.env.instantiate_scope_res(reso))
                }
                resolved => resolved,
            }
        } else {
            match self.resolve_path(
                path,
                true,
                path_span.clone(),
                path_expr_span.clone().into(),
                &lowering_cx,
                MemberSite::Expr(expr),
            ) {
                Ok(r) => ResolvedPathInBody::Reso(r),
                Err(err) => {
                    let expected_kind = if is_call_callee {
                        ExpectedPathKind::Function
                    } else {
                        ExpectedPathKind::Value
                    };

                    if let Some(diag) =
                        err.into_diag(self.db, path, path_span.clone(), expected_kind)
                    {
                        self.push_diag(diag)
                    }
                    ResolvedPathInBody::Invalid
                }
            }
        };
        self.env
            .generation_demands
            .requests(self.db, lowering_cx.take_generation_requests());
        match res {
            ResolvedPathInBody::Binding(binding) => {
                let ty = self
                    .env
                    .lookup_binding_ty(&binding)
                    .fold_with(self.db, &mut self.table);
                let ty = self.normalize_ty(ty);
                let access = self.env.binding_access(&binding);
                // A closure has its own read-only copy of each binding it
                // captures.
                let captured = self.env.binding_is_capture(binding);
                if captured {
                    if matches!(binding, LocalBinding::EffectParam { .. }) {
                        return self.capture_effect(binding);
                    }
                    // A value of an opaque shape is only passed on, never
                    // copied into a closure.
                    if let Some(Shape::Opaque(shape)) = self.env.param_product(&binding) {
                        self.push_diag(BodyDiag::OpaqueShapeBound {
                            primary: path_expr_span.clone().into(),
                            shape: *shape,
                        });
                    }
                    // A known product's accesses are captured as any access
                    // is. An access's type still being inferred is checked
                    // once it is known.
                    let is_access = access.is_some()
                        || self.env.param_product(&binding).is_some_and(|shape| {
                            !matches!(shape, Shape::Opaque(_)) && shape.has_access()
                        });
                    if is_access && ty.has_var(self.db) {
                        self.env.record_access_capture(expr, binding, ty);
                    } else if is_access && !self.ty_is_copy(ty) {
                        self.push_diag(BodyDiag::AccessCapture {
                            primary: path_expr_span.into(),
                            name: binding.binding_name(&self.env),
                        });
                    }
                    self.env.record_capture(binding, ty);
                }
                // A product parameter of one access part is that access.
                let access = access.or(match self.env.param_product(&binding) {
                    Some(Shape::Access(BorrowKind::Mut, _)) => Some(BindingAccess::Mut),
                    Some(Shape::Access(BorrowKind::Ref, _)) => Some(BindingAccess::Ref),
                    _ => None,
                });
                let is_mut = !captured && access.map_or(binding.is_mut(), BindingAccess::is_mut);
                ExprProp {
                    binding: Some(binding),
                    borrow_provider: self.concrete_borrow_provider_for_binding(binding),
                    // A product parameter is the accesses its parts grant;
                    // a closure's copy of a tuple of them is a value.
                    shape: self
                        .env
                        .param_product(&binding)
                        .filter(|shape| !captured || !matches!(shape, Shape::Tuple(_)))
                        .cloned(),
                    ..ExprProp::new(ty, is_mut)
                }
            }
            ResolvedPathInBody::NewBinding(ident) => {
                let diag = match self.env.owner() {
                    // A field whose layout failed has reported its own error.
                    BodyOwner::ContractRecvArm { contract, .. }
                    | BodyOwner::ContractInit { contract }
                        if contract
                            .storage_layout(self.db)
                            .field_errors(&ident)
                            .is_some() =>
                    {
                        None
                    }
                    BodyOwner::ContractRecvArm { contract, .. }
                        if contract.fields(self.db).contains_key(&ident) =>
                    {
                        Some(BodyDiag::ContractFieldNotInUses {
                            primary: path_expr_span.into(),
                            field: ident,
                        })
                    }
                    _ => {
                        let assoc_fn = enclosing_assoc_fn(self.db, self.body(), ident);
                        Some(BodyDiag::UndefinedVariable(
                            path_expr_span.into(),
                            ident,
                            assoc_fn,
                        ))
                    }
                };
                if let Some(diag) = diag {
                    self.push_diag(diag);
                }

                ExprProp::invalid(self.db)
            }
            ResolvedPathInBody::Diag(diag) => {
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }
            ResolvedPathInBody::Invalid => ExprProp::invalid(self.db),

            ResolvedPathInBody::Reso(reso) => match reso {
                PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                    if let Some(const_ty_ty) = ty.const_ty_ty(self.db) {
                        self.env
                            .register_value_path_ref(expr, ValuePathRef::TypeConst(ty));
                        ExprProp::new(self.table.instantiate_to_term(const_ty_ty), true)
                    } else if matches!(reso, PathRes::Ty(_))
                        && let Some(ctor_ty) = ty.tuple_struct_ctor_ty(self.db)
                    {
                        if !self.check_tuple_struct_fields_visible(
                            ty,
                            path_expr_span.clone().into(),
                            MemberSite::Expr(expr),
                        ) {
                            return ExprProp::invalid(self.db);
                        }
                        self.env
                            .register_value_path_ref(expr, ValuePathRef::FunctionItem);
                        ExprProp::new(self.instantiate_to_term(ctor_ty), true)
                    } else {
                        let diag = if ty.is_struct(self.db) {
                            let record_like = RecordLike::from_ty(ty);
                            BodyDiag::unit_variant_expected(
                                self.db,
                                path_expr_span.clone().into(),
                                Some(record_like),
                            )
                        } else {
                            BodyDiag::NotValue {
                                primary: path_expr_span.clone().into(),
                                given: Either::Right(ty),
                            }
                        };
                        self.push_diag(diag);

                        ExprProp::invalid(self.db)
                    }
                }
                PathRes::Func(ty) => {
                    let ty = self.instantiate_to_term(ty);
                    let mut callable =
                        Callable::new(self.db, ty, expr.span(self.body()).into(), None)
                            .expect("function item path should resolve to callable");
                    if !unify_generic_args(self, &mut callable) {
                        return ExprProp::invalid(self.db);
                    }
                    // A call reports a direct `Drop::drop` as it finishes.
                    if !is_call_callee {
                        self.check_drop_call(expr, callable.callable_def);
                    }

                    self.env
                        .register_value_path_ref(expr, ValuePathRef::FunctionItem);
                    ExprProp::new(callable.ty(self.db), true)
                }
                PathRes::Trait(trait_) => {
                    let diag = BodyDiag::NotValue {
                        primary: path_expr_span.clone().into(),
                        given: Either::Left(trait_.def(self.db).into()),
                    };
                    self.push_diag(diag);
                    ExprProp::invalid(self.db)
                }
                PathRes::EnumVariant(variant) => {
                    let ty = match variant.kind(self.db) {
                        VariantKind::Unit => {
                            self.env
                                .register_value_path_ref(expr, ValuePathRef::UnitVariant(variant));
                            variant.ty
                        }
                        VariantKind::Tuple(_) => {
                            self.env
                                .register_value_path_ref(expr, ValuePathRef::FunctionItem);
                            let ty = variant.constructor_func_ty(self.db).unwrap();
                            self.instantiate_to_term(ty)
                        }
                        VariantKind::Record(_) => {
                            let record_like = RecordLike::from_variant(variant);
                            let diag = BodyDiag::unit_variant_expected(
                                self.db,
                                expr.span(self.body()).into(),
                                Some(record_like),
                            );
                            self.push_diag(diag);

                            TyId::invalid(self.db, InvalidCause::Other)
                        }
                    };

                    ExprProp::new(self.instantiate_to_term(ty), true)
                }
                PathRes::Const(const_def, ty) => {
                    self.env
                        .register_const_ref(expr, ConstRef::Const(const_def));
                    ExprProp::new(ty, true)
                }
                PathRes::Method(receiver_ty, candidate) => {
                    let canonical_r_ty = Canonicalized::new(self.db, receiver_ty);
                    let (method_ty, trait_inst) = match candidate {
                        MethodCandidate::InherentMethod(cand) => (
                            self.extract_inherent_method_to_term(cand, receiver_ty),
                            None,
                        ),
                        MethodCandidate::TraitMethod(cand)
                        | MethodCandidate::NeedsConfirmation(cand) => {
                            let inst = canonical_r_ty.extract_solution(&mut self.table, cand.inst);
                            let inst = if inst
                                .self_ty(self.db)
                                .kind(self.db)
                                .does_match(receiver_ty.kind(self.db))
                            {
                                let mut args = inst.args(self.db).to_vec();
                                if let Some(self_arg) = args.first_mut() {
                                    *self_arg = receiver_ty;
                                }
                                TraitInstId::new(
                                    self.db,
                                    inst.def(self.db),
                                    args,
                                    inst.assoc_type_bindings(self.db).clone(),
                                    inst.mode(self.db),
                                )
                            } else {
                                inst
                            };
                            if matches!(candidate, MethodCandidate::NeedsConfirmation(_)) {
                                self.env.register_trait_obligation(TraitObligation {
                                    choices: self.env.choices,
                                    goal: inst,
                                    origin: TraitObligationOrigin::GenericConfirmation,
                                    span: path_expr_span.clone().into(),
                                });
                            }
                            let method_ty = if cand.method.is_method(self.db) {
                                self.instantiate_trait_method_to_term(
                                    cand.method,
                                    receiver_ty,
                                    inst,
                                )
                            } else {
                                self.instantiate_trait_assoc_fn_to_term(
                                    cand.method.as_callable(self.db).unwrap(),
                                    inst,
                                )
                            };
                            (method_ty, Some(inst))
                        }
                    };

                    let mut callable = Callable::new(
                        self.db,
                        method_ty,
                        expr.span(self.body()).into(),
                        trait_inst,
                    )
                    .expect("method path should resolve to callable");

                    if !unify_generic_args(self, &mut callable) {
                        return ExprProp::invalid(self.db);
                    }

                    let method_ty = callable.ty(self.db);
                    if !is_call_callee {
                        self.check_drop_call(expr, callable.callable_def());
                    }
                    self.env.register_callable(expr, callable);
                    self.env
                        .register_value_path_ref(expr, ValuePathRef::FunctionItem);
                    ExprProp::new(method_ty, true)
                }
                PathRes::TraitMethod(trait_inst, method) => {
                    if let Some(existing) = self.env.callable_expr(expr) {
                        return ExprProp::new(existing.ty(self.db), true);
                    }

                    let inst = if matches!(
                        trait_inst.self_ty(self.db).data(self.db),
                        TyData::TyParam(param) if param.is_trait_self()
                    ) {
                        let old_self = trait_inst.self_ty(self.db);
                        let new_self = self.table.new_var_from_param(old_self);

                        struct ReplaceSelf<'db> {
                            from: TyId<'db>,
                            to: TyId<'db>,
                        }

                        impl<'db> TyFolder<'db> for ReplaceSelf<'db> {
                            fn fold_ty(
                                &mut self,
                                db: &'db dyn HirAnalysisDb,
                                ty: TyId<'db>,
                            ) -> TyId<'db> {
                                if ty == self.from {
                                    return self.to;
                                }
                                ty.super_fold_with(db, self)
                            }
                        }

                        let mut folder = ReplaceSelf {
                            from: old_self,
                            to: new_self,
                        };

                        let args = trait_inst
                            .args(self.db)
                            .iter()
                            .map(|&ty| ty.fold_with(self.db, &mut folder))
                            .collect::<Vec<_>>();

                        let assoc_type_bindings: IndexMap<_, _> = trait_inst
                            .assoc_type_bindings(self.db)
                            .iter()
                            .map(|(name, ty)| (*name, ty.fold_with(self.db, &mut folder)))
                            .collect();

                        TraitInstId::new(
                            self.db,
                            trait_inst.def(self.db),
                            args,
                            assoc_type_bindings,
                            trait_inst.mode(self.db),
                        )
                    } else {
                        trait_inst
                    };

                    self.env.register_trait_obligation(TraitObligation {
                        choices: self.env.choices,
                        goal: inst,
                        origin: TraitObligationOrigin::GenericConfirmation,
                        span: path_expr_span.clone().into(),
                    });

                    let func_ty = self.instantiate_trait_assoc_fn_to_term(
                        method.as_callable(self.db).unwrap(),
                        inst,
                    );

                    let mut callable =
                        Callable::new(self.db, func_ty, expr.span(self.body()).into(), Some(inst))
                            .expect("trait method path should resolve to callable");

                    if !unify_generic_args(self, &mut callable) {
                        return ExprProp::invalid(self.db);
                    }

                    let func_ty = callable.ty(self.db);
                    if !is_call_callee {
                        self.check_drop_call(expr, callable.callable_def());
                    }
                    self.env.register_callable(expr, callable);
                    self.env
                        .register_value_path_ref(expr, ValuePathRef::FunctionItem);
                    ExprProp::new(func_ty, true)
                }
                PathRes::TraitConst(recv_ty, inst, name) => {
                    let mut args = inst.args(self.db).clone();
                    if let Some(self_arg) = args.first_mut() {
                        *self_arg = recv_ty;
                    }
                    let inst = TraitInstId::new(
                        self.db,
                        inst.def(self.db),
                        args,
                        inst.assoc_type_bindings(self.db).clone(),
                        inst.mode(self.db),
                    );

                    if !super::trait_const_goal_has_foreign_params(self.db, inst, self.env.scope())
                        && let GoalSatisfiability::UnSat(_) =
                            is_goal_satisfiable(self.db, self.env.solve_cx(), inst)
                    {
                        self.push_diag(TyDiagCollection::from(
                            TraitConstraintDiag::TraitBoundNotSat {
                                span: path_expr_span.clone().into(),
                                primary_goal: inst,
                                unsat_subgoal: None,
                                required_by: None,
                            },
                        ));
                        return ExprProp::invalid(self.db);
                    }

                    self.env.register_const_ref(
                        expr,
                        ConstRef::TraitConst {
                            use_: AssocConstUse::new(
                                self.env.scope(),
                                self.env.assumptions(),
                                inst,
                                name,
                            ),
                            evidence: Default::default(),
                        },
                    );
                    // Look up the associated const's declared type in the trait and
                    // instantiate it with the trait instance's args (including Self).
                    let trait_ = inst.def(self.db);
                    if let Some(const_view) = trait_.const_(self.db, name)
                        && let Some(ty_binder) = const_view.ty_binder(self.db)
                    {
                        // Instantiate with the concrete args of the trait instance
                        let instantiated = ty_binder.instantiate(self.db, inst.args(self.db));
                        let ty = self.table.instantiate_to_term(instantiated);

                        ExprProp::new(ty, true)
                    } else {
                        // Fallback to invalid type if the declaration isn't found
                        ExprProp::invalid(self.db)
                    }
                }
                PathRes::InherentConst(recv_ty, impl_, name) => {
                    if let Some(ty) = instantiate_inherent_const_decl_ty(
                        self.db,
                        &mut self.table,
                        impl_,
                        recv_ty,
                        name,
                    ) {
                        self.env.register_const_ref(
                            expr,
                            ConstRef::inherent(InherentConstUse::new(
                                self.env.scope(),
                                self.env.assumptions(),
                                impl_,
                                recv_ty,
                                name,
                            )),
                        );
                        ExprProp::new(ty, true)
                    } else {
                        ExprProp::invalid(self.db)
                    }
                }
                PathRes::Mod(scope) => {
                    let diag = BodyDiag::NotValue {
                        primary: path_expr_span.clone().into(),
                        given: Either::Left(scope.item()),
                    };
                    self.push_diag(diag);
                    ExprProp::invalid(self.db)
                }
                PathRes::FuncParam(..) => {
                    // An anonymous const body can see the declaration's scope,
                    // but does not have its runtime parameter bindings.
                    self.push_diag(BodyDiag::RuntimeParameterInConst {
                        primary: path_expr_span.clone().into(),
                        path,
                    });
                    ExprProp::invalid(self.db)
                }
            },
        }
    }

    fn check_record_init(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::RecordInit(path, ..) = expr_data else {
            unreachable!()
        };
        let span = expr.span(self.body()).into_record_init_expr();

        let Partial::Present(path) = path else {
            return ExprProp::invalid(self.db);
        };

        let path_span = span.clone().path();
        let lowering_cx = LoweringContext::new();
        let reso = match self.resolve_path(
            *path,
            true,
            path_span.clone(),
            span.clone().into(),
            &lowering_cx,
            MemberSite::Expr(expr),
        ) {
            Ok(reso) => reso,
            Err(err) => {
                if let Some(diag) =
                    err.into_diag(self.db, *path, path_span, ExpectedPathKind::Record)
                {
                    self.push_diag(diag);
                }
                return ExprProp::invalid(self.db);
            }
        };

        match reso {
            PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                // Use the expected type to constrain the record's generic args
                // before checking fields.
                let snapshot = self.snapshot_state();
                let ty = if self.table.unify(ty, expected).is_ok() {
                    self.commit_state(snapshot);
                    self.canonical_nominal_ty_from_expected(ty, expected)
                } else {
                    self.rollback_state(snapshot);
                    ty.fold_with(self.db, &mut self.table)
                };

                let record_like = RecordLike::from_ty(ty);
                if record_like.is_record(self.db) {
                    self.env
                        .register_record_init_lowering(expr, RecordInitLowering::Struct);
                    self.check_record_init_fields(&record_like, expr);
                    ExprProp::new(ty, true)
                } else {
                    let diag =
                        BodyDiag::record_expected(self.db, span.path().into(), Some(record_like));
                    self.push_diag(diag);
                    ExprProp::invalid(self.db)
                }
            }

            PathRes::Func(ty)
            | PathRes::Const(_, ty)
            | PathRes::TraitConst(ty, ..)
            | PathRes::InherentConst(ty, ..) => {
                let record_like = RecordLike::from_ty(ty);
                let diag =
                    BodyDiag::record_expected(self.db, span.path().into(), Some(record_like));
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }
            PathRes::TraitMethod(..) | PathRes::Method(..) | PathRes::FuncParam(..) => {
                let diag = BodyDiag::record_expected(self.db, span.path().into(), None);
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }

            PathRes::EnumVariant(variant) => {
                // Constrain the variant type with the expected type before
                // checking fields (same rationale as record inits).
                let resolved_ty = variant.ty;
                let snapshot = self.snapshot_state();
                let ty = if self.table.unify(resolved_ty, expected).is_ok() {
                    self.commit_state(snapshot);
                    self.canonical_nominal_ty_from_expected(resolved_ty, expected)
                } else {
                    self.rollback_state(snapshot);
                    resolved_ty.fold_with(self.db, &mut self.table)
                };
                let variant = crate::analysis::name_resolution::ResolvedVariant { ty, ..variant };

                let record_like = RecordLike::from_variant(variant);
                if record_like.is_record(self.db) {
                    self.env.register_record_init_lowering(
                        expr,
                        RecordInitLowering::EnumVariant(variant),
                    );
                    self.check_record_init_fields(&record_like, expr);
                    ExprProp::new(ty, true)
                } else {
                    let diag = BodyDiag::record_expected(self.db, span.path().into(), None);
                    self.push_diag(diag);

                    ExprProp::invalid(self.db)
                }
            }
            PathRes::Mod(scope) => {
                let diag = BodyDiag::NotValue {
                    primary: span.into(),
                    given: Either::Left(scope.item()),
                };
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }
            PathRes::Trait(trait_) => {
                let diag = BodyDiag::NotValue {
                    primary: span.into(),
                    given: Either::Left(trait_.def(self.db).into()),
                };
                self.push_diag(diag);
                ExprProp::invalid(self.db)
            }
        }
    }

    fn check_record_init_fields(&mut self, record_like: &RecordLike<'db>, expr: ExprId) {
        let hir_db = self.db;

        let Partial::Present(Expr::RecordInit(_, fields)) = expr.data(hir_db, self.body()) else {
            unreachable!()
        };
        let span = expr.span(self.body()).into_record_init_expr().fields();

        let mut rec_checker = RecordInitChecker::new(self, record_like, MemberSite::Expr(expr));

        for (i, field) in fields.iter().enumerate() {
            let label = field.label_eagerly(rec_checker.tc.db, rec_checker.tc.body());
            let field_span = span.clone().field(i).into();

            let expected = match rec_checker.feed_label(label, field_span) {
                Ok(ty) => ty,
                Err(diag) => {
                    rec_checker.tc.push_diag(diag);
                    TyId::invalid(rec_checker.tc.db, InvalidCause::Other)
                }
            };

            rec_checker.tc.check_expr(field.expr, expected);
        }

        if let Err(diag) = rec_checker.finalize(span.into(), false) {
            self.push_diag(diag);
        }
    }

    fn check_field(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Field(lhs, index) = expr_data else {
            unreachable!()
        };
        let Partial::Present(field) = index else {
            return ExprProp::invalid(self.db);
        };

        let lhs_ty = self.fresh_ty();
        let typed_lhs = self.check_expr(*lhs, lhs_ty);
        // The base is a place, possibly a projection's result.
        self.consume_access(*lhs);
        let lhs_place_ty = typed_lhs.ty;

        if lhs_place_ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        if lhs_place_ty.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        // An own or pointee field that is not visible here does not hide the
        // field of a `Project` target of the same name.
        let mut hidden = None;
        if let Some(resolved_field) = resolve_place_field(self.db, lhs_place_ty, *field) {
            let (ty_base, ty_args) = resolved_field.base_ty.decompose_ty_app(self.db);
            // An access part of a product parameter is as writable as its
            // access.
            let part = match self.product_part_shape(*lhs) {
                Some(Shape::Tuple(parts)) => parts.into_iter().nth(resolved_field.index.into()),
                _ => None,
            };
            let is_mut = match part {
                Some(Shape::Access(kind, _)) => kind == BorrowKind::Mut,
                _ => typed_lhs.is_mut || resolved_field.implicit_deref_ty.is_some(),
            };

            // Tuple struct fields are named by their position.
            let label = match field {
                FieldIndex::Ident(label) => Some(*label),
                FieldIndex::Index(index)
                    if resolved_field.base_ty.as_tuple_struct(self.db).is_some() =>
                {
                    Some(IdentId::new(self.db, index.data(self.db).to_string()))
                }
                FieldIndex::Index(_) => None,
            };
            match label {
                Some(label) => {
                    let record_like = RecordLike::from_ty(resolved_field.base_ty);
                    if let Some(field_ty) = record_like.record_field_ty(self.db, label) {
                        if let Some(scope) = record_like.record_field_scope(self.db, label)
                            && !self.member_visible(scope, MemberSite::Expr(expr))
                        {
                            // Check the visibility of the field.
                            hidden = Some(PathResDiag::Invisible(
                                expr.span(self.body()).into_field_expr().accessor().into(),
                                label,
                                scope.name_span(self.db),
                            ));
                        } else {
                            self.env
                                .register_resolved_field_index(expr, resolved_field.index);
                            return ExprProp::new(field_ty, is_mut);
                        }
                    }
                }

                None => {
                    if ty_base.is_tuple(self.db) {
                        self.env
                            .register_resolved_field_index(expr, resolved_field.index);
                        let ty = ty_args[usize::from(resolved_field.index)];
                        return ExprProp::new(ty, is_mut);
                    }
                }
            }
        }

        let given_ty = match self.check_projected_field(expr, *lhs, lhs_place_ty, *field) {
            Projected::Done(prop) => return prop,
            Projected::NoSuchField(target) => target,
            Projected::NotProject => lhs_place_ty,
        };
        if let Some(diag) = hidden {
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        let diag = BodyDiag::AccessedFieldNotFound {
            primary: expr.span(self.body()).into(),
            given_ty,
            index: *field,
        };
        self.push_diag(diag);

        ExprProp::invalid(self.db)
    }

    /// `h.f` for an `h` whose type implements `core::ops::Project` and has
    /// no visible field `f`: the call of `ProjectField::project` with the
    /// descriptor `Field<Target, F, Before>` of the field `f` of `Target`,
    /// where `Before` holds the types of the fields declared before `f` as a
    /// tree (`fields_before_ty`). The checking of the call
    /// is the tail of `check_ops_trait`.
    fn check_projected_field(
        &mut self,
        expr: ExprId,
        lhs: ExprId,
        lhs_ty: TyId<'db>,
        field: FieldIndex<'db>,
    ) -> Projected<'db> {
        let base_ty = self.normalize_ty(projectable_place_ty(self.db, lhs_ty));
        let Some(target) = crate::analysis::ty::project_target_ty(
            self.db,
            self.env.scope(),
            base_ty,
            self.env.assumptions(),
        ) else {
            return Projected::NotProject;
        };
        if target.is_ty_var(self.db) {
            self.push_diag(BodyDiag::TypeMustBeKnown(lhs.span(self.body()).into()));
            return Projected::Done(ExprProp::invalid(self.db));
        }
        let Some(position) = field_index(self.db, target, field) else {
            return Projected::NoSuchField(target);
        };

        // Tuple struct fields are named by their position.
        let label = match field {
            FieldIndex::Ident(label) => Some(label),
            FieldIndex::Index(index) if target.as_tuple_struct(self.db).is_some() => {
                Some(IdentId::new(self.db, index.data(self.db).to_string()))
            }
            FieldIndex::Index(_) => None,
        };
        if let Some(label) = label
            && let Some(scope) = RecordLike::from_ty(target).record_field_scope(self.db, label)
            && !self.member_visible(scope, MemberSite::Expr(expr))
        {
            let diag = PathResDiag::Invisible(
                expr.span(self.body()).into_field_expr().accessor().into(),
                label,
                scope.name_span(self.db),
            );
            self.push_diag(diag);
            return Projected::Done(ExprProp::invalid(self.db));
        }

        let field_tys = target.field_types(self.db);
        let position = usize::from(position);
        let before = crate::analysis::ty::fields_before_ty(self.db, &field_tys[..position]);
        let Some(field_ctor) = resolve_lib_type_path(self.db, self.env.scope(), "core::ops::Field")
        else {
            return Projected::NotProject;
        };
        let descriptor = TyId::foldl(self.db, field_ctor, &[target, field_tys[position], before]);
        let Some(trait_def) =
            resolve_core_trait(self.db, self.env.scope(), &["ops", "ProjectField"])
        else {
            return Projected::NotProject;
        };
        let inst = TraitInstId::new_simple(self.db, trait_def, vec![base_ty, descriptor]);
        self.env.register_trait_obligation(TraitObligation {
            choices: self.env.choices,
            goal: inst,
            origin: TraitObligationOrigin::GenericConfirmation,
            span: expr.span(self.body()).into(),
        });
        if matches!(
            is_goal_satisfiable(self.db, self.env.solve_cx(), inst),
            GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid
        ) {
            return Projected::Done(ExprProp::invalid(self.db));
        }

        let method = trait_def.method_defs(self.db)[&IdentId::new(self.db, "project".to_string())];
        let func_ty = self.instantiate_trait_method_to_term(method, base_ty, inst);
        let mut callable =
            Callable::new(self.db, func_ty, expr.span(self.body()).into(), Some(inst))
                .expect("ProjectField::project is callable");
        callable.set_checked_input_tys(vec![self.normalize_ty(lhs_ty)]);
        self.check_callable_effects(expr, &mut callable);

        let ret_ty = self.normalize_ty(callable.ret_ty(self.db));
        let shape = callable
            .ret_shape(self.db)
            .map(|shape| shape.map_tys(self.db, &mut |ty| self.normalize_ty(ty)));
        self.env.register_semantic_call(expr, callable);
        Projected::Done(ExprProp {
            is_mut: matches!(shape, None | Some(Shape::Access(BorrowKind::Mut, _))),
            shape,
            ..ExprProp::new(ret_ty, true)
        })
    }

    fn check_tuple(
        &mut self,
        _expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::Tuple(elems) = expr_data else {
            unreachable!()
        };

        let elem_tys = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_tuple(self.db) && args.len() == elems.len() => args.to_vec(),
            _ => self.fresh_tys_n(elems.len()),
        };

        for (elem, elem_ty) in elems.iter().zip(elem_tys.iter()) {
            self.check_expr(*elem, *elem_ty);
        }

        let ty = TyId::tuple_with_elems(self.db, &elem_tys);
        ExprProp::new(ty, true)
    }

    fn check_array(
        &mut self,
        _expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::Array(elems) = expr_data else {
            unreachable!()
        };

        let mut expected_elem_ty = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_array(self.db) => args[0],
            _ => self.fresh_ty(),
        };

        for elem in elems {
            expected_elem_ty = self.check_expr(*elem, expected_elem_ty).ty;
        }

        let ty = TyId::array_with_len(self.db, expected_elem_ty, elems.len());
        ExprProp::new(ty, true)
    }

    fn check_array_rep(
        &mut self,
        _expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
    ) -> ExprProp<'db> {
        let Expr::ArrayRep(elem, len) = expr_data else {
            unreachable!()
        };

        let mut expected_elem_ty = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_array(self.db) => args[0],
            _ => self.fresh_ty(),
        };

        let prop = self.check_expr(*elem, expected_elem_ty);
        expected_elem_ty = prop.ty;
        if !expected_elem_ty.has_invalid(self.db) && !self.ty_is_copy(expected_elem_ty) {
            self.push_diag(BodyDiag::ArrayRepeatRequiresCopy {
                primary: elem.span(self.body()).into(),
                ty: expected_elem_ty,
            });
        }

        let array = TyId::array(self.db, expected_elem_ty);
        let ty = if let Some(len_body) = len.to_opt() {
            let expected_len_ty = array
                .applicable_ty(self.db)
                .and_then(|applicable| applicable.const_ty);

            let len_ty = ConstTyId::from_body(self.db, len_body, expected_len_ty, None);
            let len_ty = TyId::const_ty(self.db, len_ty);
            let array_ty = TyId::app(self.db, array, len_ty);

            if let Some(diag) = array_ty.emit_diag(self.db, len_body.span().into()) {
                self.push_diag(diag);
            }

            let mut requires_known_const = false;
            if !array_ty.has_invalid(self.db)
                && let (_, args) = array_ty.decompose_ty_app(self.db)
                && let Some(len_ty) = args.get(1)
                && let TyData::ConstTy(const_ty) = len_ty.data(self.db)
                && !self.array_len_const_is_acceptable(*const_ty)
            {
                requires_known_const = true;
                self.push_diag(BodyDiag::ConstValueMustBeKnown(len_body.span().into()));
            }

            if requires_known_const && expected.base_ty(self.db).is_array(self.db) {
                expected
            } else {
                array_ty
            }
        } else {
            let len_ty = ConstTyId::invalid(self.db, InvalidCause::ParseError);
            let len_ty = TyId::const_ty(self.db, len_ty);
            TyId::app(self.db, array, len_ty)
        };

        ExprProp::new(ty, true)
    }

    /// Whether `const_ty` is acceptable as an array-repeat length. Each
    /// specialization materializes the repeat by forcing its length through
    /// the common CTFE service, so the length must be forceable now or be
    /// blocked only on facts that specialization supplies. Admission depends
    /// on that outcome alone, not on how the length is described: the service
    /// reports a reached fault or an unsupported operation, such as an opaque
    /// extern call with known inputs, as a failure, and any fault behind an
    /// unresolved parameter, selection or type when a concrete use forces it.
    /// Holes and inference variables are never supplied, so a length that
    /// mentions one cannot wait.
    fn array_len_const_is_acceptable(&self, const_ty: ConstTyId<'db>) -> bool {
        let len = TyId::const_ty(self.db, const_ty);
        !len.has_var(self.db)
            && !matches!(
                force_const_term_value(
                    self.db,
                    const_ty,
                    CtfeConfig::default(),
                    SemOrigin::Synthetic
                ),
                EvalOutcome::Failed(_)
            )
    }

    fn check_if(
        &mut self,
        _expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
        result_discarded: bool,
    ) -> ExprProp<'db> {
        let Expr::If(cond, then, else_) = expr_data else {
            unreachable!()
        };

        // Keep let-chain bindings scoped to this conditional so they can flow
        // into the then branch (and chained `&&` segments) without leaking to
        // the enclosing scope.
        self.env.enter_lexical_scope();
        self.check_cond(*cond);

        if let Some(else_) = else_ {
            self.env.enter_scope(*then);
            self.env.flush_pending_bindings();
            let then_prop = if result_discarded {
                self.check_expr_with_discarded_result(*then, expected)
            } else {
                self.check_expr(*then, expected)
            };
            self.env.leave_scope();
            self.env.clear_pending_bindings();
            self.env.leave_scope();
            let else_prop = self.check_expr_in_new_scope(*else_, expected, result_discarded);
            let borrow_provider =
                merged_borrow_provider(then_prop.borrow_provider, else_prop.borrow_provider);
            // The conditional only diverges if both branches do.
            let else_ty = else_prop.ty.fold_with(self.db, &mut self.table);
            let then_ty = then_prop.ty.fold_with(self.db, &mut self.table);
            let ty = if else_ty.is_never(self.db) && !then_ty.is_never(self.db) {
                then_prop.ty
            } else {
                else_prop.ty
            };
            ExprProp {
                borrow_provider,
                ..ExprProp::new(ty, true)
            }
        } else {
            let if_ty = self.fresh_ty();
            // If there is no else branch, the if expression itself typed as `()`
            self.env.enter_scope(*then);
            self.env.flush_pending_bindings();
            self.check_expr_with_discarded_result(*then, if_ty);
            self.env.leave_scope();
            self.env.clear_pending_bindings();
            self.env.leave_scope();
            ExprProp::new(TyId::unit(self.db), true)
        }
    }

    fn check_match(
        &mut self,
        expr: ExprId,
        expr_data: &Expr<'db>,
        expected: TyId<'db>,
        result_discarded: bool,
    ) -> ExprProp<'db> {
        let Expr::Match(scrutinee, arms) = expr_data else {
            unreachable!()
        };

        let scrutinee_ty = self.fresh_ty();
        let scrutinee_prop = self.check_expr(*scrutinee, scrutinee_ty);
        let scrutinee_pat_ty = scrutinee_prop.ty;

        let Partial::Present(arms) = arms else {
            return ExprProp::invalid(self.db);
        };
        let arm_pats: Vec<_> = arms.iter().map(|arm| arm.pat).collect();
        self.take_initializer_marker(*scrutinee, &arm_pats, MarkerPosition::Scrutinee);
        let scrutinee_prop = self.open_matched_place(*scrutinee, scrutinee_prop, arm_pats);

        let mut match_ty = expected;
        let mut diverging_arms = 0;
        let mut first_provider: Option<AddressSpace> = None;
        let mut provider_unknown = false;
        let mut provider_conflict = false;
        let mut arm_statuses = Vec::with_capacity(arms.len());

        for arm in arms.iter() {
            let pat_result = self.check_pat(arm.pat, scrutinee_pat_ty);
            self.bind_pattern_source(arm.pat, *scrutinee, &scrutinee_prop, false);
            arm_statuses.push(pat_result.analysis);

            self.env.enter_scope(arm.body);
            self.env.flush_pending_bindings();
            let arm_prop = if result_discarded {
                self.check_expr_with_discarded_result(arm.body, match_ty)
            } else {
                self.check_expr(arm.body, match_ty)
            };
            // A diverging arm doesn't determine the type of the match, which
            // only diverges if every arm does.
            if arm_prop
                .ty
                .fold_with(self.db, &mut self.table)
                .is_never(self.db)
            {
                diverging_arms += 1;
            } else {
                match_ty = arm_prop.ty;
            }
            self.env.leave_scope();

            if arm_prop.shape.is_some() {
                if let Some(provider) = arm_prop.borrow_provider {
                    if let Some(previous) = first_provider {
                        provider_conflict |=
                            merged_borrow_provider(Some(previous), Some(provider)).is_none();
                    } else {
                        first_provider = Some(provider);
                    }
                } else {
                    provider_unknown = true;
                }
            }
        }

        let mut exhaustive = false;
        if !scrutinee_pat_ty.has_invalid(self.db)
            && arm_statuses.iter().all(|status| status.is_ready())
        {
            let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
            let pattern_store = self
                .env
                .pattern_store()
                .clone()
                .fold_with(self.db, &mut prober);
            let scrutinee_pat_ty = scrutinee_pat_ty.fold_with(self.db, &mut prober);
            let roots: Vec<_> = arm_statuses
                .iter()
                .filter_map(|status| status.ready_root())
                .collect();
            let analysis = crate::analysis::ty::pattern_analysis::analyze_match(
                self.db,
                &pattern_store,
                &roots,
                scrutinee_pat_ty,
            );

            for i in analysis.unreachable_arms {
                let diag = BodyDiag::UnreachablePattern {
                    primary: arms[i].pat.span(self.body()).into(),
                };
                self.push_diag(diag);
            }

            match analysis.exhaustiveness {
                crate::analysis::ty::pattern_analysis::MatchExhaustiveness::Exhaustive => {
                    exhaustive = true;
                }
                crate::analysis::ty::pattern_analysis::MatchExhaustiveness::NonExhaustive(
                    missing_patterns,
                ) => {
                    let diag = BodyDiag::NonExhaustiveMatch {
                        primary: expr.span(self.body()).into(),
                        scrutinee_ty: scrutinee_pat_ty,
                        missing_patterns,
                    };
                    self.push_diag(diag);
                }
                crate::analysis::ty::pattern_analysis::MatchExhaustiveness::Inconclusive(
                    reason,
                ) => {
                    let diag = BodyDiag::PatternAnalysisInconclusive {
                        primary: expr.span(self.body()).into(),
                        reason,
                    };
                    self.push_diag(diag);
                }
            }
        }

        if (!arms.is_empty() || exhaustive) && diverging_arms == arms.len() {
            match_ty = TyId::never(self.db);
        }

        ExprProp {
            borrow_provider: if provider_unknown || provider_conflict {
                None
            } else {
                first_provider
            },
            ..ExprProp::new(match_ty, true)
        }
    }

    fn check_assign(&mut self, _expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Assign(lhs, rhs) = expr_data else {
            unreachable!()
        };

        let mut typed_lhs = self.check_expr_unknown(*lhs);
        if self.select_mut_place(*lhs) {
            typed_lhs = self.env.typed_expr(*lhs).expect("selected place is typed");
        }
        // The target is a place, possibly a projection's result.
        self.consume_access(*lhs);
        // Assignment is an expected-type boundary: the value is checked
        // against the target's type.
        let mut rhs_prop = self.check_expr(*rhs, typed_lhs.ty);
        rhs_prop.ty = self.unify_ty(
            Typeable::Expr(*rhs, rhs_prop.clone()),
            rhs_prop.ty,
            typed_lhs.ty,
        );

        self.check_assign_lhs(*lhs, &typed_lhs);

        ExprProp::new(TyId::unit(self.db), true)
    }

    fn check_aug_assign(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::AugAssign(lhs, rhs, op) = expr_data else {
            unreachable!()
        };

        let unit = ExprProp::new(TyId::unit(self.db), true);

        let mut typed_lhs = self.check_expr_unknown(*lhs);
        if self.select_mut_place(*lhs) {
            typed_lhs = self.env.typed_expr(*lhs).expect("selected place is typed");
        }
        self.consume_access(*lhs);
        let lhs_place_ty = typed_lhs.ty;
        if typed_lhs.ty.has_invalid(self.db) {
            return unit;
        }
        if self.check_assign_lhs(*lhs, &typed_lhs) == AssignLhsStatus::NonAssignable {
            return unit;
        }

        // `x op= y` on an integer or `bool` writes the built-in operation's
        // result back.
        let bin_op = BinOp::Arith(*op);
        if let Some(builtin) = self.builtin_binary_op(lhs_place_ty, bin_op) {
            self.check_expr(*rhs, lhs_place_ty);
            self.env
                .register_builtin_op(expr, BuiltinOp::Binary(builtin));
            return unit;
        }

        let lhs_base_ty = lhs_place_ty.base_ty(self.db);
        if lhs_base_ty.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.span(self.body()).into());
            self.push_diag(diag);
            return unit;
        }

        // In unchecked code, an operator that can overflow calls its wrapping
        // trait if the type implements it, which has no `*Assign`
        // counterpart: `x += y` is `x = x.wrapping_add(y)`, with the place
        // read and written once. Otherwise `x += y` calls
        // `AddAssign::add_assign`. The assignment is unit even if resolution
        // fails.
        let mode_op = self.mode_op(lhs_place_ty, bin_op);
        if mode_op.wraps() {
            let result = self.check_ops_trait(expr, lhs_place_ty, &mode_op, Some(*rhs));
            if !result.ty.has_invalid(self.db) {
                self.equate_ty(result.ty, lhs_place_ty, expr.span(self.body()).into());
                self.env.register_operator_assign(expr);
            }
        } else {
            self.check_ops_trait(expr, lhs_place_ty, &AugAssignOp(*op), Some(*rhs));
        }
        unit
    }

    /// Selects the candidates of the operator trait `trait_def` for a binary
    /// use of `lhs_ty` with the right-hand side `rhs_expr`, in `tier`: the
    /// implementations for every receiver type in `lhs_candidates`, and, among
    /// them, those whose right-hand side the use's can be, preferring those
    /// under which an open literal takes its default type.
    fn select_ops_candidates(
        &mut self,
        lhs_ty: TyId<'db>,
        lhs_candidates: &[TyId<'db>],
        op: &dyn TraitOps,
        trait_def: Trait<'db>,
        rhs_expr: ExprId,
        tier: Tier,
    ) -> OpsSelection<'db> {
        let method_assumptions = self.env.assumptions();
        let method_choices = self.env.choices;
        let mut candidates = Vec::new();
        let mut error = None;
        for &candidate_ty in lhs_candidates {
            let c_candidate_ty = Canonicalized::new(self.db, candidate_ty);
            match select_trait_method_candidates(
                self.db,
                &c_candidate_ty,
                op.trait_method(self.db),
                self.env.scope(),
                method_assumptions,
                method_choices,
                trait_def,
                tier,
            ) {
                Ok(methods) => candidates.extend(
                    methods
                        .candidates
                        .into_iter()
                        .map(|method| (candidate_ty, c_candidate_ty.clone(), method)),
                ),
                Err(err) => {
                    error.get_or_insert(err);
                }
            }
        }
        if candidates.is_empty() {
            return OpsSelection {
                candidates,
                error,
                rhs: None,
                viable: Vec::new(),
                unifies: false,
            };
        }
        let rhs = self
            .env
            .typed_expr(rhs_expr)
            .unwrap_or_else(|| self.check_expr_unknown(rhs_expr));
        // The candidates the right-hand side can be an argument of, each with
        // whether an open literal keeps its default type under it.
        let mut unifying = Vec::new();
        if !rhs.ty.has_invalid(self.db) {
            for candidate in &candidates {
                let (candidate_ty, c_candidate_ty, method) = candidate;
                let snapshot = self.snapshot_state();
                let keeps_defaults = (|| {
                    self.table.unify(lhs_ty, *candidate_ty).ok()?;
                    let inst = c_candidate_ty.extract_solution(&mut self.table, method.cand.inst);
                    let func_ty = super::try_instantiate_trait_method(
                        self.db,
                        method.cand.method,
                        &mut self.table,
                        *candidate_ty,
                        inst,
                    )
                    .ok()?;
                    let func_ty = self.table.instantiate_to_term(func_ty);
                    let expected_rhs = self.instantiated_ops_rhs_ty(func_ty, inst)?;
                    self.table.unify(rhs.ty, expected_rhs).ok()?;
                    Some(self.keeps_literal_defaults([lhs_ty, rhs.ty]))
                })();
                self.rollback_state(snapshot);
                if let Some(keeps_defaults) = keeps_defaults {
                    unifying.push((candidate.clone(), keeps_defaults));
                }
            }
        }
        let unifies = !unifying.is_empty();
        // A single candidate of a single receiver type is used as it is, so
        // that a right-hand side of another type is reported as a mismatch of
        // that candidate. Otherwise an open string literal takes its default
        // type when that settles the impl: `span == "x"` compares with a
        // `String<1>`.
        let viable = if lhs_candidates.len() == 1 && candidates.len() == 1 {
            candidates.clone()
        } else {
            super::prefer_literal_defaults(unifying)
        };
        OpsSelection {
            candidates,
            error,
            rhs: Some(rhs),
            viable,
            unifies,
        }
    }

    /// Resolve a core::ops trait method for an operator on a given LHS type and
    /// optionally check the RHS against the inferred method parameter type.
    /// Returns the fully-instantiated function type and concrete trait instance.
    /// Whether a `with` in scope asks a derive provider for `trait_def`.
    fn provider_offers(&self, trait_def: Trait<'db>) -> bool {
        self.env.choices.is_some_and(|choices| {
            choices.items(self.db).iter().any(|entry| {
                matches!(
                    entry,
                    super::super::trait_resolution::ChoiceEntry::Provider(opening)
                        if opening.may_offer(self.db, trait_def)
                )
            })
        })
    }

    pub(super) fn check_ops_trait(
        &mut self,
        expr: ExprId,
        lhs_ty: TyId<'db>,
        op: &dyn TraitOps,
        rhs_expr: Option<ExprId>,
    ) -> ExprProp<'db> {
        let Some(trait_def) =
            resolve_core_trait(self.db, self.env.scope(), &op.trait_path_segments())
        else {
            return ExprProp::invalid(self.db);
        };

        let method_assumptions = self.env.assumptions();
        let method_choices = self.env.choices;
        let mut checked_rhs_ty = None;

        let (method, inst) = if let Some(rhs_expr) = rhs_expr {
            // An open string literal on the left may become an inline
            // `String` or a `DynString`, so the impls of both are candidates.
            let lhs_candidates = match lhs_ty.data(self.db) {
                TyData::TyVar(var) if matches!(var.sort, TyVarSort::String { .. }) => {
                    let string = TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::String)));
                    let mut candidates = vec![self.table.instantiate_to_term(string)];
                    candidates.extend(resolve_lib_type_path(
                        self.db,
                        self.env.scope(),
                        "core::abi::DynString",
                    ));
                    candidates
                }
                _ => vec![lhs_ty],
            };
            // Which implementation of an operator trait a use selects is a
            // selection among candidates, as for a method call: the candidates
            // that the package writes (and its dependencies) answer it, and
            // generated implementations answer only where those leave the use
            // without a viable candidate.
            let OpsSelection {
                candidates,
                error: selection_err,
                rhs,
                viable,
                ..
            } = authored_first(
                self.db,
                self.env.scope().ingot(self.db),
                |tier| {
                    self.select_ops_candidates(
                        lhs_ty,
                        &lhs_candidates,
                        op,
                        trait_def,
                        rhs_expr,
                        tier,
                    )
                },
                |selection| selection.answers(),
            );

            match selection_err {
                Some(MethodSelectionError::NotFound) | None if candidates.is_empty() => {
                    let diag = BodyDiag::ops_trait_not_implemented(
                        self.db,
                        expr.span(self.body()).into(),
                        lhs_ty,
                        op,
                        self.provider_offers(trait_def),
                    );
                    self.push_diag(diag);
                    return ExprProp::invalid(self.db);
                }
                Some(err) if candidates.is_empty() => {
                    let span = expr.span(self.body());
                    let diag = body_diag_from_method_selection_err(
                        self.db,
                        err,
                        Spanned::new(lhs_ty, span.clone().into()),
                        Spanned::new(op.trait_method(self.db), span.into()),
                    );
                    self.push_diag(diag);
                    return ExprProp::invalid(self.db);
                }
                _ => {}
            }
            let Some(rhs) = rhs else {
                return ExprProp::invalid(self.db);
            };
            if rhs.ty.has_invalid(self.db) {
                return ExprProp::invalid(self.db);
            }
            let mut viable = viable;

            match viable.len() {
                0 => {
                    let diag = BodyDiag::ops_trait_not_implemented(
                        self.db,
                        expr.span(self.body()).into(),
                        lhs_ty,
                        op,
                        self.provider_offers(trait_def),
                    );
                    self.push_diag(diag);
                    return ExprProp::invalid(self.db);
                }
                1 => {
                    let (selected_lhs_ty, c_lhs_ty, candidate) = viable.pop().unwrap();
                    self.equate_ty(lhs_ty, selected_lhs_ty, expr.span(self.body()).into());
                    let inst = c_lhs_ty.extract_solution(&mut self.table, candidate.cand.inst);
                    if candidate.needs_confirmation {
                        self.env.register_trait_obligation(TraitObligation {
                            choices: self.env.choices,
                            goal: inst,
                            origin: TraitObligationOrigin::GenericConfirmation,
                            span: expr.span(self.body()).into(),
                        });
                    }
                    let func_ty = self.instantiate_trait_method_to_term(
                        candidate.cand.method,
                        selected_lhs_ty,
                        inst,
                    );
                    if let Some(expected_rhs) = self.instantiated_ops_rhs_ty(func_ty, inst) {
                        checked_rhs_ty = Some(self.equate_ty(
                            rhs.ty,
                            expected_rhs,
                            rhs_expr.span(self.body()).into(),
                        ));
                    }
                    (func_ty, inst)
                }
                _ => {
                    let cands = viable
                        .into_iter()
                        .map(|(_, c_lhs_ty, candidate)| {
                            c_lhs_ty.extract_solution(&mut self.table, candidate.cand.inst)
                        })
                        .collect();
                    self.push_diag(BodyDiag::AmbiguousTraitInst {
                        primary: expr.span(self.body()).into(),
                        cands,
                        required_by: None,
                    });
                    return ExprProp::invalid(self.db);
                }
            }
        } else {
            let lhs_candidates = self.receiver_candidates(lhs_ty);
            let mut selected_lhs_ty = lhs_candidates[0];
            let mut c_lhs_ty = Canonicalized::new(self.db, selected_lhs_ty);
            let mut method_candidate = self.select_method_in_scope(
                &c_lhs_ty,
                op.trait_method(self.db),
                method_assumptions,
                method_choices,
                Some(trait_def),
            );
            if matches!(
                method_candidate,
                Err(MethodSelectionError::NotFound | MethodSelectionError::ReceiverTypeMustBeKnown)
            ) {
                for &candidate_ty in lhs_candidates.iter().skip(1) {
                    let c_candidate_ty = Canonicalized::new(self.db, candidate_ty);
                    let fallback = self.select_method_in_scope(
                        &c_candidate_ty,
                        op.trait_method(self.db),
                        method_assumptions,
                        method_choices,
                        Some(trait_def),
                    );
                    if fallback.is_ok() || !matches!(fallback, Err(MethodSelectionError::NotFound))
                    {
                        selected_lhs_ty = candidate_ty;
                        c_lhs_ty = c_candidate_ty;
                        method_candidate = fallback;
                        break;
                    }
                }
            }

            match method_candidate {
                Ok(MethodCandidate::InherentMethod(_)) => unreachable!(),
                Ok(
                    res @ (MethodCandidate::TraitMethod(cand)
                    | MethodCandidate::NeedsConfirmation(cand)),
                ) => {
                    let inst = c_lhs_ty.extract_solution(&mut self.table, cand.inst);
                    if matches!(res, MethodCandidate::NeedsConfirmation(_)) {
                        self.env.register_trait_obligation(TraitObligation {
                            choices: self.env.choices,
                            goal: inst,
                            origin: TraitObligationOrigin::GenericConfirmation,
                            span: expr.span(self.body()).into(),
                        });
                    }

                    let func_ty =
                        self.instantiate_trait_method_to_term(cand.method, selected_lhs_ty, inst);
                    (func_ty, inst)
                }
                Err(MethodSelectionError::AmbiguousTraitMethod(_)) => {
                    unreachable!("unary core::ops ambiguity")
                }
                Err(MethodSelectionError::NotFound) => {
                    let diag = BodyDiag::ops_trait_not_implemented(
                        self.db,
                        expr.span(self.body()).into(),
                        lhs_ty,
                        op,
                        self.provider_offers(trait_def),
                    );
                    self.push_diag(diag);
                    return ExprProp::invalid(self.db);
                }
                Err(err) => {
                    let span = expr.span(self.body());
                    let diag = body_diag_from_method_selection_err(
                        self.db,
                        err,
                        Spanned::new(lhs_ty, span.clone().into()),
                        Spanned::new(op.trait_method(self.db), span.into()),
                    );
                    self.push_diag(diag);
                    return ExprProp::invalid(self.db);
                }
            }
        };

        let mut callable =
            Callable::new(self.db, method, expr.span(self.body()).into(), Some(inst))
                .expect("failed to create Callable for core::ops trait method");

        let mut checked_inputs = vec![self.normalize_ty(lhs_ty)];
        if let Some(rhs_expr) = rhs_expr {
            let rhs_ty = checked_rhs_ty.unwrap_or_else(|| {
                self.env
                    .typed_expr(rhs_expr)
                    .expect("checked operator RHS has a type")
                    .ty
            });
            checked_inputs.push(self.normalize_ty(rhs_ty));
        }
        callable.set_checked_input_tys(checked_inputs);
        self.check_callable_effects(expr, &mut callable);

        let ret_ty = self.normalize_ty(callable.ret_ty(self.db));
        let shape = callable
            .ret_shape(self.db)
            .map(|shape| shape.map_tys(self.db, &mut |ty| self.normalize_ty(ty)));
        self.env.register_semantic_call(expr, callable);
        ExprProp {
            is_mut: matches!(shape, None | Some(Shape::Access(BorrowKind::Mut, _))),
            shape,
            ..ExprProp::new(ret_ty, true)
        }
    }

    fn instantiated_ops_rhs_ty(
        &mut self,
        func_ty: TyId<'db>,
        inst: TraitInstId<'db>,
    ) -> Option<TyId<'db>> {
        let (base, gen_args) = func_ty.decompose_ty_app(self.db);
        let TyData::TyBase(TyBase::Func(func_def)) = base.data(self.db) else {
            return None;
        };
        let expected_rhs = func_def
            .arg_tys(self.db)
            .get(1)?
            .instantiate(self.db, gen_args);
        let expected_rhs = self.normalize_ty(normalize_with_trait_evidence(
            self.db,
            expected_rhs,
            self.env.scope(),
            inst,
        ));
        Some(expected_rhs)
    }

    fn check_assign_lhs(&mut self, lhs: ExprId, typed_lhs: &ExprProp<'db>) -> AssignLhsStatus {
        if !self.is_assignable_expr(lhs) || !self.env.is_place_expr(lhs) {
            if !typed_lhs.ty.has_invalid(self.db) {
                let diag = BodyDiag::NonAssignableExpr(lhs.span(self.body()).into());
                self.push_diag(diag);
            }

            return AssignLhsStatus::NonAssignable;
        }

        if let Some(pointer) = self.calldata_pointer_of_place(lhs) {
            let binding = self.find_base_binding(pointer).map(|binding| {
                (
                    binding.binding_name(&self.env),
                    binding.def_span(&self.env),
                    MutabilityFix::Calldata,
                )
            });
            self.push_diag(BodyDiag::ImmutableAssignment {
                primary: lhs.span(self.body()).into(),
                binding,
            });
            return AssignLhsStatus::Immutable;
        }

        if !typed_lhs.is_mut {
            let binding = self.find_base_binding(lhs);
            if self.report_write_to_capture(binding, lhs.span(self.body()).into()) {
                return AssignLhsStatus::Immutable;
            }
            // A `mut` binding of a read access is reported where it binds.
            if let Some(binding @ LocalBinding::Local { pat, .. }) = binding
                && self.env.binding_access(&binding).is_some()
                && let Partial::Present(Pat::Path(_, marker)) = pat.data(self.db, self.body())
                && self.binding_marker(pat, *marker) == BindingMarker::Mut
            {
                return AssignLhsStatus::Immutable;
            }
            let binding = binding.map(|binding| {
                let ident = binding.binding_name(&self.env);
                let fix = match binding {
                    _ if self.binding_is_maybe_mut(binding) => MutabilityFix::MaybeMut,
                    LocalBinding::Local { pat, .. } => {
                        if self.ref_part_bindings.contains(&pat) {
                            MutabilityFix::RefPart
                        } else if self.env.binding_access(&binding).is_some() {
                            MutabilityFix::ReadAccess
                        } else if self.mutable_sources.contains(&pat) {
                            MutabilityFix::MutComponent
                        } else {
                            MutabilityFix::Var
                        }
                    }
                    LocalBinding::Param { .. } if self.env.param_product(&binding).is_some() => {
                        MutabilityFix::ProductParam
                    }
                    LocalBinding::Param { mode, .. } => {
                        match (mode == FuncParamMode::Own, ident.is_self(self.db)) {
                            (true, true) => MutabilityFix::OwnSelf,
                            (true, false) => MutabilityFix::OwnParam,
                            (false, true) => MutabilityFix::ViewSelf,
                            (false, false) => MutabilityFix::ViewParam,
                        }
                    }
                    LocalBinding::ParamPart { .. } => MutabilityFix::ReadAccess,
                    LocalBinding::EffectParam { .. } => MutabilityFix::Effect,
                };
                (ident, binding.def_span(&self.env), fix)
            });
            let diag = BodyDiag::ImmutableAssignment {
                primary: lhs.span(self.body()).into(),
                binding,
            };

            self.push_diag(diag);
            return AssignLhsStatus::Immutable;
        }

        AssignLhsStatus::Assignable
    }

    fn check_expr_in_new_scope(
        &mut self,
        expr: ExprId,
        expected: TyId<'db>,
        result_discarded: bool,
    ) -> ExprProp<'db> {
        self.env.enter_scope(expr);
        let ty = if result_discarded {
            self.check_expr_with_discarded_result(expr, expected)
        } else {
            self.check_expr(expr, expected)
        };
        self.env.leave_scope();

        ty
    }

    pub(super) fn report_cannot_borrow_mut(&mut self, expr: ExprId, primary: DynLazySpan<'db>) {
        let binding = self.find_base_binding(expr);
        if self.report_write_to_capture(binding, primary.clone()) {
            return;
        }
        self.push_diag(BodyDiag::CannotBorrowMut {
            primary,
            binding: binding
                .map(|binding| (binding.binding_name(&self.env), binding.def_span(&self.env))),
            maybe_mut: binding.is_some_and(|binding| self.binding_is_maybe_mut(binding)),
            split: self
                .split_demand
                .map(|split| split.span(self.body()).into()),
        });
    }

    /// Whether `binding` is `mut?`: a `mut?` name, parameter or effect,
    /// which a `mut?` function's `ref` body only reads.
    pub(super) fn binding_is_maybe_mut(&self, binding: LocalBinding<'db>) -> bool {
        match binding {
            LocalBinding::Local { pat, .. } => matches!(
                pat.data(self.db, self.body()),
                Partial::Present(Pat::Path(_, BindingMarker::MaybeMut))
            ),
            LocalBinding::Param { site, idx, .. } => {
                hir_param(self.db, site, idx).is_some_and(|param| param.is_maybe_mut(self.db))
            }
            LocalBinding::EffectParam {
                site: EffectParamSite::Func(func, _),
                idx,
                ..
            } => func
                .effects(self.db)
                .data(self.db)
                .get(idx)
                .is_some_and(|effect| effect.mode == ModeMarker::MaybeMut),
            LocalBinding::EffectParam { .. } | LocalBinding::ParamPart { .. } => false,
        }
    }

    /// Reports a write through `binding` if it is a capture of the closure
    /// being checked, returning whether it is.
    fn report_write_to_capture(
        &mut self,
        binding: Option<LocalBinding<'db>>,
        primary: DynLazySpan<'db>,
    ) -> bool {
        let Some(binding) = binding.filter(|&binding| self.env.binding_is_capture(binding)) else {
            return false;
        };
        self.push_diag(BodyDiag::WriteToCapture {
            primary,
            name: binding.binding_name(&self.env),
        });
        true
    }

    /// Returns the base binding for a given expression if it exists.
    ///
    /// This function traverses the expression tree to find the base binding,
    /// which is the original variable or binding that the expression refers to.
    ///
    /// # Parameters
    ///
    /// - `expr`: The expression ID for which to find the base binding.
    ///
    /// # Returns
    ///
    /// An `Option` containing the `LocalBinding` if a base binding is found,
    /// or `None` if there is no base binding.
    fn find_base_binding(&self, expr: ExprId) -> Option<LocalBinding<'db>> {
        let Partial::Present(expr_data) = self.env.expr_data(expr) else {
            return None;
        };

        match expr_data {
            Expr::Field(lhs, ..) => self.find_base_binding(*lhs),
            Expr::Bin(lhs, _rhs, op) if *op == BinOp::Index => self.find_base_binding(*lhs),
            Expr::Un(inner, UnOp::Deref) => self.find_base_binding(*inner),
            Expr::Path(..) => self.env.typed_expr(expr)?.binding,
            _ => None,
        }
    }

    /// The part of a product parameter `expr` names through tuple fields. A
    /// closure's copy of the parameter is a value, with no parts.
    fn product_part_shape(&self, expr: ExprId) -> Option<Shape<'db>> {
        match self.env.expr_data(expr) {
            Partial::Present(Expr::Path(..)) => {
                let binding = self.env.typed_expr(expr)?.binding?;
                if self.env.binding_is_capture(binding) {
                    return None;
                }
                self.env.param_product(&binding).cloned()
            }
            Partial::Present(Expr::Field(base, ..)) => match self.product_part_shape(*base)? {
                Shape::Tuple(parts) => parts
                    .into_iter()
                    .nth(self.env.resolved_field_index(expr)?.into()),
                _ => None,
            },
            _ => None,
        }
    }

    /// Returns `true`` if the expression can be used as an left hand side of an
    /// assignment.
    /// This method doesn't take mutability into account.
    pub(super) fn is_assignable_expr(&self, expr: ExprId) -> bool {
        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return false;
        };

        match expr_data {
            Expr::Path(..) | Expr::Field(..) => true,
            Expr::Bin(_, _, op) if *op == BinOp::Index => true,
            Expr::Un(_, UnOp::Deref) => true,
            _ => self.env.is_grant_place(expr),
        }
    }

    /// The calldata pointer whose pointee holds the place `expr` names.
    fn calldata_pointer_of_place(&mut self, expr: ExprId) -> Option<ExprId> {
        let (base, deref) = match self.env.expr_data(expr) {
            Partial::Present(Expr::Un(inner, UnOp::Deref)) => (*inner, true),
            Partial::Present(Expr::Field(base, ..)) => (*base, false),
            Partial::Present(Expr::Bin(base, _, BinOp::Index)) => {
                return self.calldata_pointer_of_place(*base);
            }
            _ => return None,
        };
        let base_ty = self.env.typed_expr(base)?.ty;
        if matches!(
            self.normalize_ty(base_ty).as_ptr(self.db),
            Some((_, AddressSpace::Calldata))
        ) {
            Some(base)
        } else if deref {
            None
        } else {
            self.calldata_pointer_of_place(base)
        }
    }
}

/// The names a block adds to an operand's scope: its local bindings and the
/// instantiation of the default it checks.
struct BlockOperands<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    env: &'a TyCheckEnv<'db>,
}

impl<'db> OperandValues<'db> for BlockOperands<'_, 'db> {
    fn bare_name(&self, path: PathId<'db>, span: DynLazySpan<'db>) -> BareName<'db> {
        match resolve_ident_expr(self.db, self.env, path, span, &LoweringContext::new()) {
            ResolvedPathInBody::Binding(value) => BareName::Binding(value.def_span(self.env)),
            ResolvedPathInBody::Reso(value) => BareName::Item(value),
            ResolvedPathInBody::Diag(diagnostic) => BareName::Diag(diagnostic),
            ResolvedPathInBody::NewBinding(_) => BareName::Unbound,
            ResolvedPathInBody::Invalid => BareName::Invalid,
        }
    }

    fn shadows_type(&self, path: PathId<'db>) -> bool {
        path.is_bare_ident(self.db)
            && self
                .env
                .visible_binding(
                    path.local_ident(self.db)
                        .unwrap_or_else(|| path.ident(self.db).unwrap()),
                )
                .is_some()
    }

    fn instantiate_res(&self, res: PathRes<'db>) -> PathRes<'db> {
        self.env.instantiate_scope_res(res)
    }

    fn instantiate_ty(&self, ty: TyId<'db>) -> TyId<'db> {
        self.env.instantiate_scope_ty(ty)
    }
}

fn reify_unresolved_pattern_slots<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    slot_bindings: &FxHashMap<TyId<'db>, TyId<'db>>,
) -> T
where
    T: crate::analysis::ty::fold::TyFoldable<'db>,
{
    struct SlotReifier<'a, 'db> {
        slot_bindings: &'a FxHashMap<TyId<'db>, TyId<'db>>,
    }

    impl<'db> crate::analysis::ty::fold::TyFolder<'db> for SlotReifier<'_, 'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            self.slot_bindings
                .get(&ty)
                .copied()
                .unwrap_or_else(|| ty.super_fold_with(db, self))
        }
    }

    value.fold_with(db, &mut SlotReifier { slot_bindings })
}

fn body_diag_from_method_selection_err<'db>(
    db: &'db dyn HirAnalysisDb,
    err: MethodSelectionError<'db>,
    receiver: Spanned<'db, TyId<'db>>,
    method: Spanned<'db, IdentId<'db>>,
) -> FuncBodyDiag<'db> {
    match err {
        MethodSelectionError::ReceiverTypeMustBeKnown => {
            BodyDiag::TypeMustBeKnown(receiver.span).into()
        }
        MethodSelectionError::AmbiguousInherentMethod(candidates) => {
            BodyDiag::AmbiguousInherentMethodCall {
                primary: method.span,
                method_name: method.data,
                candidates,
            }
            .into()
        }

        MethodSelectionError::AmbiguousTraitMethod(ambiguous) => BodyDiag::AmbiguousTrait {
            primary: method.span,
            method_name: method.data,
            traits: ambiguous.diagnostic_traits,
        }
        .into(),

        MethodSelectionError::NotFound => {
            let base_ty = receiver.data.base_ty(db);
            PathResDiag::MethodNotFound {
                primary: method.span,
                method_name: method.data,
                receiver: Either::Left(base_ty),
            }
            .into()
        }

        MethodSelectionError::InvisibleInherentMethod(func) => {
            PathResDiag::Invisible(method.span, method.data, func.name_span().into()).into()
        }

        MethodSelectionError::InvisibleTraitMethod(traits) => BodyDiag::InvisibleAmbiguousTrait {
            primary: method.span,
            traits,
        }
        .into(),
    }
}

fn resolve_ident_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    env: &TyCheckEnv<'db>,
    path: PathId<'db>,
    ident_span: DynLazySpan<'db>,
    lowering_cx: &LoweringContext<'db>,
) -> ResolvedPathInBody<'db> {
    let ident = path.ident(db).unwrap();
    let local_ident = path.local_ident(db).unwrap_or(ident);
    let hygiene_scope = path.hygiene_scope(db);

    let resolve_bucket = |bucket: &NameResBucket<'db>, scope| {
        // Surface ambiguity or conflict in an expression's name domains.
        for (domain, err) in bucket.errors() {
            if !domain.intersects(NameDomain::VALUE | NameDomain::TYPE) {
                continue;
            }
            match err {
                NameResolutionError::Ambiguous(cands) => {
                    let mut cand_spans = Vec::new();
                    for name in cands.iter() {
                        if let Some(span) = name.kind.name_span(db) {
                            let from_implicit = name
                                .derivation
                                .use_stmt()
                                .is_some_and(|use_| use_.is_synthetic_use(db));
                            cand_spans.push((span, from_implicit));
                        }
                    }

                    let diag = PathResDiag::Ambiguous(ident_span.clone(), ident, cand_spans);
                    return ResolvedPathInBody::Diag(diag.into());
                }
                NameResolutionError::Conflict(conf_ident, spans) => {
                    let diag = PathResDiag::Conflict(*conf_ident, spans.clone());
                    return ResolvedPathInBody::Diag(diag.into());
                }
                _ => {}
            }
        }

        let Ok(res) = bucket.pick_any(&[NameDomain::VALUE, NameDomain::TYPE]) else {
            return ResolvedPathInBody::Invalid;
        };
        let Ok(reso) = resolve_name_res_with_minter(
            db,
            res,
            path,
            scope,
            env.written_assumptions(),
            lowering_cx,
        ) else {
            return ResolvedPathInBody::Invalid;
        };
        ResolvedPathInBody::Reso(reso)
    };

    // The walk over the blocks and then the module is one lookup procedure:
    // it runs in the authored tier from the innermost block to the module, and
    // only if it finds nothing there does it run again with the generated
    // items. A generated item thus never wins a step of the walk that an
    // authored item further out would have won.
    let body_scope = hygiene_scope.unwrap_or_else(|| env.body().scope());
    let search = |tier: Tier| {
        let mut current_idx = env.current_block_idx();
        loop {
            let block = env.get_block(current_idx);
            if let Some(binding) = block.lookup_var(local_ident) {
                return Some(ResolvedPathInBody::Binding(binding));
            }

            // A spliced quote keeps its producer's free names. Only its hygienic
            // local binders may resolve in the enclosing generated body.
            if hygiene_scope.is_none() {
                let scope = block.scope;
                let directive = QueryDirective::for_scope(db, scope).disallow_lex();
                let query = EarlyNameQueryId::new(db, ident, scope, directive);
                let bucket = resolve_query_in(db, query, tier);
                let resolved = resolve_bucket(bucket, scope);
                if !matches!(resolved, ResolvedPathInBody::Invalid) {
                    return Some(resolved);
                }
            }
            if current_idx == 0 {
                break;
            }
            current_idx -= 1;
        }

        let directive = QueryDirective::for_scope(db, body_scope);
        let directive = if hygiene_scope.is_some() {
            directive.disallow_enclosing_item_bindings()
        } else {
            directive
        };
        let query = EarlyNameQueryId::new(db, ident, body_scope, directive);
        let bucket = resolve_query_in(db, query, tier);
        match resolve_bucket(bucket, hygiene_scope.unwrap_or_else(|| env.scope())) {
            ResolvedPathInBody::Invalid => None,
            r => Some(r),
        }
    };
    authored_first(db, body_scope.ingot(db), search, Option::is_some)
        .unwrap_or(ResolvedPathInBody::NewBinding(ident))
}

/// If `ident` names a function of the `impl`, `impl trait` or `trait` that
/// encloses `body`, returns the container kind (`"impl"` or `"trait"`) and
/// whether that function takes `self`. Such functions are not in scope as bare
/// names.
fn enclosing_assoc_fn<'db>(
    db: &'db dyn HirAnalysisDb,
    body: crate::hir_def::Body<'db>,
    ident: IdentId<'db>,
) -> Option<(&'static str, bool)> {
    let mut scope = body.scope();
    let (container, kind) = loop {
        let item = scope.parent_item(db)?;
        match item {
            ItemKind::Impl(_) | ItemKind::ImplTrait(_) => break (item, "impl"),
            ItemKind::Trait(_) => break (item, "trait"),
            ItemKind::Mod(_) | ItemKind::TopMod(_) => return None,
            _ => scope = ScopeId::from_item(item),
        }
    };
    ScopeId::from_item(container)
        .child_items(db)
        .find_map(|child| match child {
            ItemKind::Func(func) if func.name(db).to_opt() == Some(ident) => {
                Some((kind, func.is_method(db)))
            }
            _ => None,
        })
}

/// This traits are intended to be implemented by the operators that can work as
/// a syntax sugar for a trait method. For example, binary `+` operator
/// implements this trait to be able to work as a syntax sugar for
/// `core::ops::Add` trait method.
///
/// TODO: We need to refine this trait definition to connect core library traits
/// smoothly.
pub(crate) trait TraitOps {
    fn trait_path_segments(&self) -> [&str; 2] {
        ["ops", self.triple()[0]]
    }

    fn core_trait_path<'db>(&self, db: &'db dyn HirAnalysisDb) -> PathId<'db> {
        let mut path = PathId::from_ident(db, IdentId::new(db, "core".to_string()));
        for s in self.trait_path_segments() {
            path = path.push_ident(db, IdentId::new(db, s.to_string()));
        }
        path
    }

    fn trait_method<'db>(&self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        IdentId::new(db, self.triple()[1].to_string())
    }

    fn op_symbol<'db>(&self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        IdentId::new(db, self.triple()[2].to_string())
    }

    /// Whether the operator calls a wrapping trait, as it does in unchecked
    /// code.
    fn wraps(&self) -> bool {
        false
    }

    fn triple(&self) -> [&str; 3];
}

/// An operator in the arithmetic mode of the body it is written in. In
/// unchecked code, `+`, `-`, `*`, `**` and unary `-` call the wrapping traits
/// (`WrappingAdd::wrapping_add`, ...) on a type that implements them; every
/// other operator calls the same trait in either mode.
#[derive(Clone, Copy, Debug)]
struct ModeOp<T> {
    op: T,
    overflow: Overflow,
}

impl TraitOps for ModeOp<BinOp> {
    fn wraps(&self) -> bool {
        self.overflow == Overflow::Wrapping
            && matches!(
                self.op,
                BinOp::Arith(ArithBinOp::Add | ArithBinOp::Sub | ArithBinOp::Mul | ArithBinOp::Pow)
            )
    }

    fn triple(&self) -> [&str; 3] {
        if !self.wraps() {
            return self.op.triple();
        }
        match self.op {
            BinOp::Arith(ArithBinOp::Add) => ["WrappingAdd", "wrapping_add", "+"],
            BinOp::Arith(ArithBinOp::Sub) => ["WrappingSub", "wrapping_sub", "-"],
            BinOp::Arith(ArithBinOp::Mul) => ["WrappingMul", "wrapping_mul", "*"],
            BinOp::Arith(ArithBinOp::Pow) => ["WrappingPow", "wrapping_pow", "**"],
            _ => unreachable!("only `+`, `-`, `*` and `**` wrap"),
        }
    }
}

impl TraitOps for ModeOp<UnOp> {
    fn wraps(&self) -> bool {
        self.overflow == Overflow::Wrapping && self.op == UnOp::Minus
    }

    fn triple(&self) -> [&str; 3] {
        if self.wraps() {
            ["WrappingNeg", "wrapping_neg", "-"]
        } else {
            self.op.triple()
        }
    }
}

impl TraitOps for UnOp {
    fn triple(&self) -> [&str; 3] {
        match self {
            UnOp::Plus => ["UnaryPlus", "add", "+"],
            UnOp::Minus => ["Neg", "neg", "-"],
            UnOp::Not => ["Not", "not", "!"],
            UnOp::BitNot => ["BitNot", "bit_not", "~"],
            UnOp::Mut => ["MutBorrow", "mut_borrow", "mut"],
            UnOp::Ref => ["RefBorrow", "ref_borrow", "ref"],
            UnOp::MaybeMut => unreachable!("`mut?` is the body's mode, never trait-lowered"),
            UnOp::Deref => unreachable!("pointer dereference is not trait-lowered"),
        }
    }
}

impl TraitOps for BinOp {
    fn triple(&self) -> [&str; 3] {
        match self {
            BinOp::Arith(arith_op) => {
                use ArithBinOp::*;

                match arith_op {
                    Add => ["Add", "add", "+"],
                    Sub => ["Sub", "sub", "-"],
                    Mul => ["Mul", "mul", "*"],
                    Div => ["Div", "div", "/"],
                    Rem => ["Rem", "rem", "%"],
                    Pow => ["Pow", "pow", "**"],
                    LShift => ["Shl", "shl", "<<"],
                    RShift => ["Shr", "shr", ">>"],
                    BitAnd => ["BitAnd", "bitand", "&"],
                    BitOr => ["BitOr", "bitor", "|"],
                    BitXor => ["BitXor", "bitxor", "^"],
                    // Range is handled specially - it constructs a Range type
                    // rather than calling a trait method
                    Range => ["Range", "range", ".."],
                }
            }

            BinOp::Comp(comp_op) => {
                use crate::core::hir_def::CompBinOp::*;

                // Comp
                match comp_op {
                    Eq => ["Eq", "eq", "=="],
                    NotEq => ["Eq", "ne", "!="],
                    Lt => ["Ord", "lt", "<"],
                    LtEq => ["Ord", "le", "<="],
                    Gt => ["Ord", "gt", ">"],
                    GtEq => ["Ord", "ge", ">="],
                }
            }

            BinOp::Logical(_) => {
                unreachable!()
            }

            BinOp::Index => ["Index", "index", "[]"],
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct AugAssignOp(ArithBinOp);

impl TraitOps for AugAssignOp {
    fn triple(&self) -> [&str; 3] {
        use ArithBinOp::*;
        match self.0 {
            Add => ["AddAssign", "add_assign", "+="],
            Sub => ["SubAssign", "sub_assign", "-="],
            Mul => ["MulAssign", "mul_assign", "*="],
            Div => ["DivAssign", "div_assign", "/="],
            Rem => ["RemAssign", "rem_assign", "%="],
            Pow => ["PowAssign", "pow_assign", "**="],
            LShift => ["ShlAssign", "shl_assign", "<<="],
            RShift => ["ShrAssign", "shr_assign", ">>="],
            BitAnd => ["BitAndAssign", "bitand_assign", "&="],
            BitOr => ["BitOrAssign", "bitor_assign", "|="],
            BitXor => ["BitXorAssign", "bitxor_assign", "^="],
            // Range doesn't have an augmented assignment form
            Range => unreachable!("Range operator cannot be used in augmented assignment"),
        }
    }
}

/// Where a member of a type is used, for `TyChecker::member_visible`: an
/// expression, a pattern, or the expression being checked.
#[derive(Clone, Copy, Debug)]
pub(super) enum MemberSite {
    Expr(ExprId),
    Pat(PatId),
    Current,
}

impl<'db> TyChecker<'db> {
    /// Whether the code at `site` may use `target`, a field, a variant or an
    /// inherent method, as `viewer` decides.
    pub(super) fn member_visible(&self, target: ScopeId<'db>, site: MemberSite) -> bool {
        self.viewer(site).sees(self.db, target)
    }

    /// Who looks at a name at `site`: the place the code runs, the package that
    /// wrote it and the record that the header around it requests. The writer
    /// is the package of the innermost quotation around the expression or
    /// pattern (the package that holds hand-written code). Name resolution and
    /// the member checks both decide visibility through this value.
    pub(super) fn viewer(&self, site: MemberSite) -> Viewer<'db> {
        let body = self.body();
        let chain = match site {
            MemberSite::Expr(expr) => body.expr_quote_chain(self.db, expr),
            MemberSite::Pat(pat) => body.pat_quote_chain(self.db, pat),
            MemberSite::Current => match self.env.current_expr() {
                Some(expr) => body.expr_quote_chain(self.db, expr),
                None => body.quote_chain(self.db),
            },
        };
        Viewer::written_by(self.db, self.env.scope(), chain[0].ingot(self.db))
    }
}

fn code_kind_name(kind: common::generation::FrozenCodeKind) -> String {
    use common::generation::FrozenCodeKind;
    match kind {
        FrozenCodeKind::Item => "Item",
        FrozenCodeKind::Expr => "Expr",
        FrozenCodeKind::Pat => "Pat",
        FrozenCodeKind::Arm => "Arm",
        FrozenCodeKind::Where => "Where",
    }
    .to_string()
}

impl<'db> TyChecker<'db> {
    /// The syntax of the quotation `expr`, in the module that holds the text
    /// of the body: a generated body's text is its producer's.
    fn quote_syntax_node(&self, expr: ExprId) -> Option<parser::ast::QuoteExpr> {
        use parser::ast::{self, prelude::*};
        let body = self.env.body();
        let crate::span::HirOrigin::Raw(ptr) =
            body.source_map(self.db).expr_map.node_to_source(expr)
        else {
            return None;
        };
        let root = crate::lower::top_mod_ast(self.db, body.syntax_top_mod(self.db));
        match ptr.to_node(root.syntax()).kind() {
            ast::ExprKind::Quote(quote) => Some(quote),
            _ => None,
        }
    }
}

impl<'db> TyChecker<'db> {
    /// Whether the body runs at compile time, where compile-time-only values
    /// are ordinary.
    pub(super) fn is_compile_time_body(&self) -> bool {
        match self.env.owner() {
            BodyOwner::Func(function, _) => function.is_const(self.db),
            BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::InheritedConst { .. } => true,
            _ => false,
        }
    }
}
