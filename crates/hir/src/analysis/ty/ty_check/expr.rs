use either::Either;
use num_bigint::BigUint;
use num_traits::ToPrimitive;
use rustc_hash::FxHashMap;
use smallvec1::SmallVec;

use crate::core::hir_def::{
    ArithBinOp, BinOp, CallArg as HirCallArg, CallableDef, Cond, CondId, Expr, ExprId, FieldIndex,
    IdentId, IntegerId, LitKind, LogicalBinOp, Partial, PatId, PathId, Stmt, StmtId, UnOp,
    VariantKind, WithBinding,
};
use crate::span::DynLazySpan;

use super::{
    CodeRegionIntrinsicKind, ConstIntrinsicKind, ConstRef, RecordLike, Typeable, ValuePathRef,
    effect_env::{
        FamilyKeyedEntry, FrameLookupResult, MatchedForwarder, MatchedKeyedEntry, MatchedWitness,
    },
    env::{
        EffectOrigin, EffectParamSite, ExprProp, LocalBinding, ParamSite, PendingPrimitiveOp,
        ProvidedEffect, TraitObligation, TraitObligationOrigin, TyCheckEnv,
    },
    path::ResolvedPathInBody,
    ty_may_be_code_region_token,
};
use crate::analysis::place::{Place, PlaceBase};
use crate::analysis::ty::{
    adt_def::AdtRef,
    assoc_const::AssocConstUse,
    canonical::{Canonicalized, Solution},
    corelib::{
        resolve_core_range_types, resolve_core_trait, resolve_lib_func_path, resolve_lib_type_path,
    },
    diagnostics::{BodyDiag, FuncBodyDiag, MustUseSubject, TraitConstraintDiag, TyDiagCollection},
    effect_handle_metadata,
    effects::{
        BarrierReason, EffectBarrier, EffectKeyKind, EffectPatternKey, EffectQuery,
        EffectRequirementDecl, EffectRequirementKey, EffectWitness, ForwardedEffectKey,
        PatternSlotKind, StoredEffectKey, StoredTraitKey, StoredTypeKey, TraitPatternKey,
        TypePatternKey, WitnessTransport,
        elaborate::{
            build_barrier_pattern_for_with_key,
            build_conservative_same_family_barrier_pattern_in_scope, build_effect_query_for_call,
            contains_projection_or_invalid_query_state, effect_requirement_decls_for_callable,
            erase_unresolved_trailing_layout_hole_default_args, finalize_stored_effect_key,
            query_contains_unresolved_inference,
        },
        match_::{
            KeyMatchCommit, apply_key_match_commit, instantiate_trait_pattern_in,
            instantiate_trait_pattern_in_with_bindings, query_matches_forwarder,
            query_matches_witness,
        },
        place_effect_provider_param_index_map, stored_value_contains_implicit_layout_params,
        stored_value_contains_out_of_scope_params,
    },
    fold::{AssocTySubst, TyFoldable as _, TyFolder},
    provider::{ProviderTransport, provider_semantics_for_specialized_call},
    trait_def::TraitInstId,
    trait_resolution::{
        GoalSatisfiability, PredicateListId, TraitGoalSolution, TraitSolveCx, is_goal_satisfiable,
    },
    ty_check::callable::{Callable, EffectProviderProvenance, EffectProviderSpecialization},
    ty_def::{CapabilityKind, PrimTy, TyBase, TyData, prim_int_bits},
    unify::UnificationTable,
};
use crate::analysis::{
    HirAnalysisDb, Spanned,
    name_resolution::{
        EarlyNameQueryId, ExpectedPathKind, NameDomain, NameResBucket, NameResolutionError,
        PathRes, QueryDirective,
        diagnostics::PathResDiag,
        is_scope_visible_from,
        method_selection::{MethodCandidate, MethodSelectionError, select_method_candidate},
        resolve_name_res, resolve_query,
    },
    place::resolve_place_field_index,
    ty::{
        const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy},
        normalize::normalize_ty,
        ty_check::{RecordInitLowering, TyChecker, path::RecordInitChecker},
        ty_def::{InvalidCause, TyId},
        ty_lower::instantiate_callable_effect_layout_args,
    },
};
use crate::hir_def::{FieldParent, ItemKind, scope_graph::ScopeId};
use crate::semantic::ProviderBinding;
use common::indexmap::IndexMap;

#[derive(Debug, Clone, Copy)]
pub(super) enum TypeEffectBindingMatch<'db> {
    Direct {
        given: TyId<'db>,
    },
    Provider {
        resolution: ProviderTargetResolution<'db>,
    },
}

#[derive(Debug, Clone, Copy)]
pub(super) struct ProviderTargetResolution<'db> {
    target_ty: TyId<'db>,
    target_seed_ty: TyId<'db>,
    handle_proof: Option<(TraitInstId<'db>, Solution<TraitGoalSolution<'db>>)>,
    effect_ref_proof: Option<(TraitInstId<'db>, Solution<TraitGoalSolution<'db>>)>,
    effect_ref_mut_proof: Option<(TraitInstId<'db>, Solution<TraitGoalSolution<'db>>)>,
}

impl<'db> ProviderTargetResolution<'db> {
    fn direct(target_ty: TyId<'db>) -> Self {
        Self {
            target_ty,
            target_seed_ty: target_ty,
            handle_proof: None,
            effect_ref_proof: None,
            effect_ref_mut_proof: None,
        }
    }
}

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

#[derive(Debug, Clone)]
enum EffectEvidence<'db> {
    Keyed {
        provider: ProvidedEffect<'db>,
        key_kind: EffectKeyKind,
        target_ty: Option<TyId<'db>>,
        commit: EffectCommitPlan<'db>,
        arg_style: EffectArgStyle,
    },
    UnkeyedType {
        provider: ProvidedEffect<'db>,
        commit: EffectCommitPlan<'db>,
        arg_style: EffectArgStyle,
    },
    UnkeyedTrait {
        provider: ProvidedEffect<'db>,
        commit: EffectCommitPlan<'db>,
        arg_style: EffectArgStyle,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EffectArgStyle {
    Place,
    TempPlace,
    Value,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AssignLhsStatus {
    Assignable,
    Immutable,
    NonAssignable,
}

#[derive(Debug, Clone, Default)]
pub(super) struct EffectCommitPlan<'db> {
    key_match: Option<KeyMatchCommit<'db>>,
    trait_solutions: SmallVec<[(TraitInstId<'db>, Solution<TraitGoalSolution<'db>>); 2]>,
    provider_resolution: Option<ProviderTargetResolution<'db>>,
    extra_unifications: SmallVec<[(TyId<'db>, TyId<'db>); 4]>,
}

#[derive(Debug, Clone)]
enum EffectResolution<'db> {
    Chosen(Box<EffectEvidence<'db>>),
    BlockedByBarrier,
    Missing,
    Ambiguous,
}

fn evidence_provider<'db>(evidence: &EffectEvidence<'db>) -> ProvidedEffect<'db> {
    match evidence {
        EffectEvidence::Keyed { provider, .. }
        | EffectEvidence::UnkeyedType { provider, .. }
        | EffectEvidence::UnkeyedTrait { provider, .. } => *provider,
    }
}

pub(super) enum PendingPrimitiveOpResolution {
    Pending,
    Resolved,
    Done,
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
        (resolve_lib_func_path(self.db, self.env.scope(), "core::size_of") == Some(func))
            .then_some(ConstIntrinsicKind::SizeOf)
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

        self.env.enter_expr(expr);
        let mut actual = match expr_data {
            Expr::Lit(LitKind::String(string_id)) => {
                ExprProp::new(self.string_literal_ty(*string_id, expected), true)
            }
            Expr::Lit(lit) => ExprProp::new(self.lit_ty_for_expected(lit, expected), true),
            Expr::Block(..) => self.check_block(expr, expr_data, expected, result_discarded),
            Expr::Un(..) => self.check_unary(expr, expr_data),
            Expr::Cast(inner, ty) => self.check_cast(expr, *inner, *ty),
            Expr::Bin(lhs, rhs, op) => self.check_binary(expr, *lhs, *rhs, *op),
            Expr::Call(..) => self.check_call(expr, expr_data),
            Expr::Assert(args) => self.check_assert(expr, args),
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

        actual.ty = normalize_ty(self.db, actual.ty, self.env.scope(), self.env.assumptions());
        if let Some(coerced) =
            self.try_coerce_capability_for_expr_to_expected(expr, actual.ty, expected)
        {
            actual.ty = coerced;
        }
        let typeable = Typeable::Expr(expr, actual.clone());
        actual.ty = self.unify_ty(typeable, actual.ty, expected);
        match expr_data {
            Expr::Call(..) => {
                if let Some(callable) = self.env.callable_expr(expr).cloned() {
                    let span = expr.span(self.body()).into_call_expr().callee().into();
                    callable.enqueue_constraints(self, expr, span);
                }
            }
            Expr::MethodCall(..) => {
                if let Some(callable) = self.env.callable_expr(expr).cloned() {
                    let span = expr
                        .span(self.body())
                        .into_method_call_expr()
                        .method_name()
                        .into();
                    callable.enqueue_constraints(self, expr, span);
                }
            }
            _ => {}
        }
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
        let Expr::Block(stmts) = expr_data else {
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
            res
        }
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

    fn check_unary(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Un(lhs, op) = expr_data else {
            unreachable!()
        };
        let prop = self.check_expr_unknown(*lhs);
        if prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        if prop.ty.is_integral_var(self.db) && matches!(op, UnOp::Plus | UnOp::Minus | UnOp::BitNot)
        {
            if matches!(op, UnOp::Minus) {
                self.env
                    .register_pending_primitive_op(PendingPrimitiveOp::Unary {
                        expr,
                        inner: *lhs,
                        op: *op,
                    });
            }
            return prop;
        }

        if matches!(op, UnOp::Mut | UnOp::Ref) {
            if self.env.expr_place(*lhs).is_none() {
                self.push_diag(BodyDiag::BorrowFromNonPlace {
                    primary: expr.span(self.body()).into(),
                });
                return ExprProp::invalid(self.db);
            }

            let place_ty = prop
                .ty
                .as_capability(self.db)
                .map(|(_, inner)| inner)
                .unwrap_or(prop.ty);
            let borrow_provider = self
                .env
                .expr_place(*lhs)
                .and_then(|place| self.concrete_borrow_provider_for_place(&place));

            return match op {
                UnOp::Ref => ExprProp {
                    ty: TyId::borrow_ref_of(self.db, place_ty),
                    is_mut: false,
                    binding: None,
                    borrow_provider,
                    path_read_semantics: None,
                },
                UnOp::Mut => {
                    if !prop.is_mut {
                        let binding = self.find_base_binding(*lhs).map(|binding| {
                            (binding.binding_name(&self.env), binding.def_span(&self.env))
                        });
                        self.push_diag(BodyDiag::CannotBorrowMut {
                            primary: expr.span(self.body()).into(),
                            binding,
                        });
                        return ExprProp::invalid(self.db);
                    }
                    ExprProp {
                        ty: TyId::borrow_mut_of(self.db, place_ty),
                        is_mut: true,
                        binding: None,
                        borrow_provider,
                        path_read_semantics: None,
                    }
                }
                _ => unreachable!(),
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

        let lhs_ty = self.copy_inner_from_borrow(prop.ty).unwrap_or(prop.ty);
        if lhs_ty != prop.ty {
            self.unify_ty(Typeable::Expr(*lhs, prop.clone()), lhs_ty, lhs_ty);
        }

        self.check_ops_trait(expr, lhs_ty, op, None)
    }

    fn check_cast(
        &mut self,
        expr: ExprId,
        inner_expr: ExprId,
        target_ty: Partial<crate::hir_def::TypeId<'db>>,
    ) -> ExprProp<'db> {
        let inner_prop = self.check_expr_unknown(inner_expr);
        if inner_prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let Some(hir_target_ty) = target_ty.to_opt() else {
            return ExprProp::invalid(self.db);
        };

        let span = expr.span(self.body()).into_cast_expr().ty();
        let target_ty = self.lower_ty(hir_target_ty, span, true);
        if target_ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let mut from = normalize_ty(
            self.db,
            inner_prop.ty,
            self.env.scope(),
            self.env.assumptions(),
        );
        let to = normalize_ty(self.db, target_ty, self.env.scope(), self.env.assumptions());

        // Casts operate on values, so for Copy capabilities treat the source as
        // the inner value type. This allows widening/narrowing checks such as
        // `(selector as u256)` when `selector` comes from a view parameter.
        if let Some((_, inner)) = from.as_capability(self.db)
            && self.ty_is_copy(inner)
        {
            from = inner;
        }

        if from == to {
            return ExprProp::new(to, true);
        }

        if let Partial::Present(Expr::Lit(LitKind::Int(int_id))) =
            inner_expr.data(self.db, self.body())
        {
            let value = int_id.data(self.db);
            if to.is_string(self.db) && self.int_literal_fits_in_ty(value, TyId::u256(self.db)) {
                let _ = self.table.unify(from, TyId::u256(self.db));
                return ExprProp::new(to, true);
            }

            if self.int_literal_fits_in_ty(value, to) {
                // Unify the literal's type variable with the target leaf type
                // so it doesn't remain unresolved.
                let leaf = self.peel_transparent_newtypes(to);
                let _ = self.table.unify(from, leaf);
                return ExprProp::new(to, true);
            }

            let leaf = self.peel_transparent_newtypes(to);
            // Unify to prevent a spurious "type annotation needed" error.
            let _ = self.table.unify(from, leaf);
            let diag = BodyDiag::InvalidCast {
                primary: expr.span(self.body()).into(),
                from,
                to,
                hint: Some(format!(
                    "integer literal `{}` is not representable in `{}`",
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
            return ExprProp::new(to, true);
        }

        // Check if the cast failed due to invisible struct fields.
        let hint = if self.is_single_field_struct_with_invisible_field(from)
            || self.is_single_field_struct_with_invisible_field(to)
        {
            Some("cast is not allowed because the struct field is not `pub`".to_string())
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
        if let Some((false, from_bits)) =
            self.prim_int_signed_bits(self.peel_transparent_newtypes(from))
            && let Some((false, to_bits)) =
                self.prim_int_signed_bits(self.peel_transparent_newtypes(to))
            && let Partial::Present(Expr::Bin(_, rhs, BinOp::Arith(ArithBinOp::RShift))) =
                expr.data(self.db, body)
            && let Partial::Present(Expr::Lit(LitKind::Int(shift_int))) = rhs.data(self.db, body)
            && let Some(shift) = shift_int.data(self.db).to_usize()
        {
            to_bits >= from_bits.saturating_sub(shift)
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
            return self.prim_int_signed_bits(to_leaf).is_some();
        }

        if to_leaf.is_bool(self.db) {
            return false;
        }

        if self.is_string_word_cast(from_leaf, to_leaf) {
            return true;
        }

        self.is_lossless_int_cast(from_leaf, to_leaf)
    }

    fn is_string_word_cast(&self, from: TyId<'db>, to: TyId<'db>) -> bool {
        (from.is_string(self.db) && self.is_plain_u256(to))
            || (self.is_plain_u256(from) && to.is_string(self.db))
    }

    fn is_plain_u256(&self, ty: TyId<'db>) -> bool {
        matches!(
            ty.base_ty(self.db).data(self.db),
            TyData::TyBase(TyBase::Prim(PrimTy::U256))
        )
    }

    fn transparent_newtype_field_ty(&self, ty: TyId<'db>) -> Option<TyId<'db>> {
        if ty.is_tuple(self.db) {
            let field_tys = ty.field_types(self.db);
            return (field_tys.len() == 1).then(|| field_tys[0]);
        }

        if ty.is_struct(self.db) {
            let field_tys = ty.field_types(self.db);
            if field_tys.len() != 1 {
                return None;
            }

            // Reject cast if the struct field is not visible from the current scope.
            if self.is_single_field_struct_with_invisible_field(ty) {
                return None;
            }

            return Some(field_tys[0]);
        }

        None
    }

    /// Returns `true` if `ty` is a single-field struct whose field is not visible
    /// from the current scope.
    fn is_single_field_struct_with_invisible_field(&self, ty: TyId<'db>) -> bool {
        if !ty.is_struct(self.db) {
            return false;
        }
        let field_tys = ty.field_types(self.db);
        if field_tys.len() != 1 {
            return false;
        }
        let Some(adt_def) = ty.adt_def(self.db) else {
            return false;
        };
        let AdtRef::Struct(s) = adt_def.adt_ref(self.db) else {
            return false;
        };
        let field_scope = ScopeId::Field(FieldParent::Struct(s), 0);
        !is_scope_visible_from(self.db, field_scope, self.env.scope())
    }

    fn peel_transparent_newtypes(&self, mut ty: TyId<'db>) -> TyId<'db> {
        while let Some(inner) = self.transparent_newtype_field_ty(ty) {
            ty = inner;
        }
        ty
    }

    fn prim_int_signed_bits(&self, ty: TyId<'db>) -> Option<(bool, usize)> {
        let base = ty.base_ty(self.db);
        let TyData::TyBase(TyBase::Prim(prim)) = base.data(self.db) else {
            return None;
        };
        let bits = prim_int_bits(*prim)?;
        let signed = matches!(
            prim,
            PrimTy::I8
                | PrimTy::I16
                | PrimTy::I32
                | PrimTy::I64
                | PrimTy::I128
                | PrimTy::I256
                | PrimTy::Isize
        );
        Some((signed, bits))
    }

    fn is_lossless_int_cast(&self, from: TyId<'db>, to: TyId<'db>) -> bool {
        let Some((from_signed, from_bits)) = self.prim_int_signed_bits(from) else {
            return false;
        };
        let Some((to_signed, to_bits)) = self.prim_int_signed_bits(to) else {
            return false;
        };

        match (from_signed, to_signed) {
            (false, false) => to_bits >= from_bits,
            (true, true) => to_bits >= from_bits,
            (false, true) => to_bits > from_bits,
            (true, false) => false,
        }
    }

    fn int_literal_fits_in_ty(&self, value: &BigUint, target_ty: TyId<'db>) -> bool {
        let leaf = self.peel_transparent_newtypes(target_ty);
        let Some((signed, bits)) = self.prim_int_signed_bits(leaf) else {
            return false;
        };

        if signed {
            let max = (BigUint::from(1u8) << (bits - 1)) - BigUint::from(1u8);
            value <= &max
        } else {
            let max = (BigUint::from(1u8) << bits) - BigUint::from(1u8);
            value <= &max
        }
    }

    fn negated_int_literal_fits_in_ty(&self, value: &BigUint, target_ty: TyId<'db>) -> bool {
        let leaf = self.peel_transparent_newtypes(target_ty);
        let Some((signed, bits)) = self.prim_int_signed_bits(leaf) else {
            return false;
        };
        if !signed {
            return false;
        }

        let max = BigUint::from(1u8) << (bits - 1);
        value <= &max
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

        let lhs_place_ty = lhs
            .ty
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or(lhs.ty);

        if matches!(op, BinOp::Index) && lhs_place_ty.is_array(self.db) {
            // Built-in array indexing (TODO: move to trait impl)
            let args = lhs_place_ty.generic_args(self.db);
            let elem_ty = args[0];
            let index_ty = args[1].const_ty_ty(self.db).unwrap();
            self.check_expr(rhs_expr, index_ty);
            return ExprProp::new(elem_ty, lhs.is_mut);
        } else if lhs.ty.is_integral_var(self.db) {
            // Avoid 'type must be known' diagnostics when lhs is an unknown integer ty.
            // For unknown integer types, the result type depends on the operator:
            // - arithmetic: same integer type
            // - comparison: bool
            self.check_expr(rhs_expr, lhs.ty);
            if matches!(
                op,
                BinOp::Arith(
                    ArithBinOp::Add
                        | ArithBinOp::Sub
                        | ArithBinOp::Mul
                        | ArithBinOp::Div
                        | ArithBinOp::Rem
                        | ArithBinOp::Pow
                ) | BinOp::Comp(..)
            ) {
                self.env
                    .register_pending_primitive_op(PendingPrimitiveOp::Binary {
                        expr,
                        lhs: lhs_expr,
                        rhs: rhs_expr,
                        op,
                    });
            }

            if matches!(op, BinOp::Comp(_)) {
                return ExprProp::new(TyId::bool(self.db), true);
            }

            return lhs;
        }

        // Fail if lhs ty is unknown
        if lhs.ty.base_ty(self.db).is_ty_var(self.db) {
            self.check_expr_unknown(rhs_expr);
            let diag = BodyDiag::TypeMustBeKnown(lhs_expr.span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        let lhs_ty = self.copy_inner_from_borrow(lhs.ty).unwrap_or(lhs.ty);
        if lhs_ty != lhs.ty {
            self.unify_ty(Typeable::Expr(lhs_expr, lhs.clone()), lhs_ty, lhs_ty);
        }

        self.check_ops_trait(expr, lhs_ty, &op, Some(rhs_expr))
    }

    pub(super) fn resolve_pending_primitive_op(
        &mut self,
        pending: &PendingPrimitiveOp,
    ) -> PendingPrimitiveOpResolution {
        if self.env.callable_expr(pending.expr()).is_some() {
            return PendingPrimitiveOpResolution::Done;
        }

        let Some(expr_prop) = self.env.typed_expr(pending.expr()) else {
            return PendingPrimitiveOpResolution::Done;
        };
        let expr_ty = {
            let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
            expr_prop.ty.fold_with(self.db, &mut prober)
        };
        if expr_ty.has_invalid(self.db) {
            return PendingPrimitiveOpResolution::Done;
        }

        let resolved = match pending {
            PendingPrimitiveOp::Unary { expr, inner, op } => {
                let Some(inner_prop) = self.env.typed_expr(*inner) else {
                    return PendingPrimitiveOpResolution::Done;
                };
                let operand_ty = {
                    let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
                    inner_prop.ty.fold_with(self.db, &mut prober)
                };
                let operand_ty = operand_ty
                    .as_capability(self.db)
                    .map(|(_, inner)| inner)
                    .unwrap_or(operand_ty);
                let operand_ty = self.normalize_ty(operand_ty);
                if operand_ty.has_invalid(self.db) {
                    return PendingPrimitiveOpResolution::Done;
                }
                if operand_ty.is_integral_var(self.db)
                    || operand_ty.base_ty(self.db).is_ty_var(self.db)
                {
                    return PendingPrimitiveOpResolution::Pending;
                }
                if matches!(op, UnOp::Minus)
                    && let Some(int_id) = self.try_get_literal_int(*inner)
                {
                    let literal = int_id.data(self.db);
                    if self.negated_int_literal_fits_in_ty(literal, operand_ty) {
                        return PendingPrimitiveOpResolution::Done;
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
                        return PendingPrimitiveOpResolution::Done;
                    }
                }
                self.check_ops_trait(*expr, operand_ty, op, None)
            }
            PendingPrimitiveOp::Binary { expr, lhs, rhs, op } => {
                let Some(lhs_prop) = self.env.typed_expr(*lhs) else {
                    return PendingPrimitiveOpResolution::Done;
                };
                let Some(rhs_prop) = self.env.typed_expr(*rhs) else {
                    return PendingPrimitiveOpResolution::Done;
                };
                let lhs_ty = {
                    let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
                    lhs_prop.ty.fold_with(self.db, &mut prober)
                };
                let rhs_ty = {
                    let mut prober = super::env::Prober::new(&mut self.table, self.env.scope());
                    rhs_prop.ty.fold_with(self.db, &mut prober)
                };
                let lhs_ty = lhs_ty
                    .as_capability(self.db)
                    .map(|(_, inner)| inner)
                    .unwrap_or(lhs_ty);
                let lhs_ty = self.normalize_ty(lhs_ty);
                let rhs_ty = rhs_ty
                    .as_capability(self.db)
                    .map(|(_, inner)| inner)
                    .unwrap_or(rhs_ty);
                let rhs_ty = self.normalize_ty(rhs_ty);
                if lhs_ty.has_invalid(self.db) || rhs_ty.has_invalid(self.db) {
                    return PendingPrimitiveOpResolution::Done;
                }
                if lhs_ty.is_integral_var(self.db)
                    || lhs_ty.base_ty(self.db).is_ty_var(self.db)
                    || rhs_ty.is_integral_var(self.db)
                    || rhs_ty.base_ty(self.db).is_ty_var(self.db)
                {
                    return PendingPrimitiveOpResolution::Pending;
                }
                self.check_ops_trait(*expr, lhs_ty, op, Some(*rhs))
            }
        };

        if resolved.ty.has_invalid(self.db) {
            return PendingPrimitiveOpResolution::Done;
        }
        self.table.unify(expr_ty, resolved.ty).ok();
        PendingPrimitiveOpResolution::Resolved
    }

    fn check_let_condition(&mut self, pat: PatId, scrutinee: ExprId) -> ExprProp<'db> {
        let scrutinee_ty = self.fresh_ty();
        let scrutinee_prop = self.check_expr(scrutinee, scrutinee_ty);
        let (pat_expected, mode) = self.destructure_source_mode(scrutinee_prop.ty);
        self.check_pat(pat, pat_expected);
        if let super::PatternDestructureMode::Borrow(kind) = mode {
            self.retype_pattern_bindings_for_borrow(pat, kind);
        }

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
                let const_value = EvaluatedConstTy::LitInt(int_id);
                let const_data = ConstTyData::Evaluated(const_value, usize_ty);
                let const_ty = ConstTyId::new(self.db, const_data);
                let const_ty_id = TyId::const_ty(self.db, const_ty);
                TyId::app(self.db, known_base, const_ty_id)
            }
            None => unknown_ty,
        }
    }

    fn check_with(
        &mut self,
        bindings: &[WithBinding<'db>],
        body_expr: ExprId,
        expected: TyId<'db>,
        result_discarded: bool,
    ) -> ExprProp<'db> {
        self.env.effect_env_mut().push_frame();

        for binding in bindings {
            let value_prop = self.check_expr_unknown(binding.value);

            let is_mut = value_prop
                .binding
                .map(|b| b.is_mut())
                .unwrap_or(value_prop.is_mut);

            let provided = ProvidedEffect {
                origin: EffectOrigin::With {
                    value_expr: binding.value,
                },
                ty: self.table.fold_ty(self.db, value_prop.ty),
                is_mut,
                binding: value_prop.binding,
            };

            match binding.key_path {
                Some(key_path) => {
                    if let Some(key_path) = key_path.to_opt() {
                        let folded_provider_ty = self.table.fold_ty(self.db, provided.ty);
                        match self.validate_keyed_with(
                            key_path,
                            ProvidedEffect {
                                ty: folded_provider_ty,
                                ..provided
                            },
                            binding.value.span(self.body()).into(),
                        ) {
                            Ok((witness, commit)) => {
                                let committed = self.apply_effect_commit_plan(commit);
                                debug_assert!(
                                    committed,
                                    "validated keyed `with` binding commit failed"
                                );
                                if !committed {
                                    let barrier = EffectBarrier {
                                        pattern: build_barrier_pattern_for_with_key(self, key_path)
                                            .expect("validated keyed binding should have a barrier pattern"),
                                        reason: self.barrier_reason_for_pattern(
                                            key_path,
                                            witness.key.clone(),
                                            binding.value.span(self.body()).into(),
                                        ),
                                    };
                                    self.insert_effect_barrier(barrier);
                                    continue;
                                }
                                self.env.effect_env_mut().insert_witness(self.db, witness);
                            }
                            Err(barrier) => {
                                self.insert_effect_barrier(*barrier);
                            }
                        }
                    }
                }
                None => {
                    self.env.effect_env_mut().insert_unkeyed(provided);
                }
            }
        }

        let result = if result_discarded {
            self.check_expr_with_discarded_result(body_expr, expected)
        } else {
            self.check_expr(body_expr, expected)
        };
        self.env.effect_env_mut().pop_frame();
        result
    }

    fn check_call(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Call(callee, args) = expr_data else {
            unreachable!()
        };
        let callee_prop = self.check_expr_unknown(*callee);
        if callee_prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let mut callable = if matches!(
            callee.data(self.db, self.body()),
            Partial::Present(Expr::Path(..))
        ) && let Some(existing) = self.env.callable_expr(*callee)
        {
            existing.clone()
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

        self.check_callable_effects(expr, &mut callable);

        let ret_ty = callable.ret_ty(self.db);
        let normalized_ret_ty = self.normalize_ty(ret_ty);
        if let Some(kind) = self.const_intrinsic_kind(callable.callable_def()) {
            self.env.register_const_intrinsic(expr, callable, kind);
        } else {
            self.env.register_semantic_call(expr, callable);
        }
        ExprProp::new(normalized_ret_ty, true)
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
        let arg_ty = self
            .env
            .typed_expr(arg_expr)
            .map(|prop| self.normalize_ty(prop.ty))
            .unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other));
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
        let body = self.body();
        let call_span: DynLazySpan<'db> = expr.span(body).into();
        let args = self.resolve_callable_effects(call_span.clone(), callable);
        for arg in args {
            self.env.push_call_effect_arg(expr, arg);
        }
    }

    pub(super) fn resolve_callable_effects(
        &mut self,
        call_span: DynLazySpan<'db>,
        callable: &mut Callable<'db>,
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

        let body = self.body();
        let callee_provider_arg_idx_by_effect =
            place_effect_provider_param_index_map(self.db, func);
        let callee_effect_env =
            crate::core::semantic::EffectEnvView::new(EffectParamSite::Func(func));

        let provided_span = |provided: ProvidedEffect<'db>| match provided.origin {
            EffectOrigin::With { value_expr } => Some(value_expr.span(body).into()),
            EffectOrigin::Param { .. } => None,
        };
        let reqs = effect_requirement_decls_for_callable(self.db, callable.callable_def);
        for (param_idx, req) in reqs.iter().enumerate() {
            let Some(key_path) = req.key_path else {
                continue;
            };
            let Some(query) = build_effect_query_for_call(self, callable, req) else {
                continue;
            };
            let provider_arg_idx_for_param = callee_provider_arg_idx_by_effect
                .get(req.binding_idx as usize)
                .copied()
                .flatten();

            match self.resolve_effect_query(func, req.clone(), query.clone(), call_span.clone()) {
                EffectResolution::Chosen(evidence) => {
                    let (provider, arg_style, key_kind, instantiated_key_ty) = match *evidence {
                        EffectEvidence::Keyed {
                            provider,
                            key_kind,
                            target_ty,
                            commit,
                            arg_style,
                        } => {
                            let committed = self.apply_effect_commit_plan(commit);
                            debug_assert!(committed, "chosen keyed effect evidence commit failed");
                            if !committed {
                                let diag = BodyDiag::MissingEffect {
                                    primary: call_span.clone(),
                                    func,
                                    key: key_path,
                                };
                                self.push_diag(diag);
                                continue;
                            }
                            (provider, arg_style, key_kind, target_ty)
                        }
                        EffectEvidence::UnkeyedType {
                            provider,
                            commit,
                            arg_style,
                        } => {
                            let target_ty = match commit.key_match.clone() {
                                Some(KeyMatchCommit::QueryToType { actual, .. }) => {
                                    Some(self.table.fold_ty(self.db, actual))
                                }
                                _ => match req.key.clone() {
                                    EffectRequirementKey::Type(_) => self
                                        .query_type_key(&query.key)
                                        .map(|ty| self.table.fold_ty(self.db, ty)),
                                    EffectRequirementKey::Trait(_) => None,
                                },
                            };
                            let committed = self.apply_effect_commit_plan(commit);
                            debug_assert!(
                                committed,
                                "chosen unkeyed type effect evidence commit failed"
                            );
                            if !committed {
                                let diag = BodyDiag::MissingEffect {
                                    primary: call_span.clone(),
                                    func,
                                    key: key_path,
                                };
                                self.push_diag(diag);
                                continue;
                            }
                            (provider, arg_style, EffectKeyKind::Type, target_ty)
                        }
                        EffectEvidence::UnkeyedTrait {
                            provider,
                            commit,
                            arg_style,
                        } => {
                            let committed = self.apply_effect_commit_plan(commit);
                            debug_assert!(
                                committed,
                                "chosen unkeyed trait effect evidence commit failed"
                            );
                            if !committed {
                                let diag = BodyDiag::MissingEffect {
                                    primary: call_span.clone(),
                                    func,
                                    key: key_path,
                                };
                                self.push_diag(diag);
                                continue;
                            }
                            (provider, arg_style, EffectKeyKind::Trait, None)
                        }
                    };

                    let (arg, pass_mode) =
                        self.effect_arg_for_provider(provider, arg_style, req.required_mut);
                    let provider_target_ty = self.provider_target_ty_for_effect_arg(
                        provider,
                        instantiated_key_ty,
                        req.required_mut,
                    );
                    if req.required_mut && matches!(pass_mode, super::EffectPassMode::Unknown) {
                        let diag = BodyDiag::EffectMutabilityMismatch {
                            primary: call_span.clone(),
                            func,
                            key: key_path,
                            provided_span: provided_span(provider),
                        };
                        self.push_diag(diag);
                        continue;
                    }
                    if !self.effect_arg_is_valid(&arg, pass_mode) {
                        let diag = BodyDiag::MissingEffect {
                            primary: call_span.clone(),
                            func,
                            key: key_path,
                        };
                        self.push_diag(diag);
                        continue;
                    }
                    if let Some(provider_arg_idx) = provider_arg_idx_for_param
                        && let Some(provider_var) =
                            callable.generic_args().get(provider_arg_idx).copied()
                        && let Some(given) = self.inferred_provider_ty_for_effect_arg(
                            provider,
                            &arg,
                            pass_mode,
                            provider_target_ty,
                        )
                    {
                        let existing_provider = self.table.fold_ty(self.db, provider_var);
                        let snapshot = self.table.snapshot();
                        if self.table.unify(provider_var, given).is_err() {
                            self.table.rollback_to(snapshot);
                            self.push_diag(BodyDiag::EffectProviderMismatch {
                                primary: call_span.clone(),
                                func,
                                key: key_path,
                                expected: existing_provider,
                                given,
                                provided_span: provided_span(provider),
                            });
                        }
                    }
                    if let Some(target_ty) = instantiated_key_ty {
                        self.instantiate_callable_effect_layout_args(
                            callable,
                            func,
                            req.binding_idx as usize,
                            target_ty,
                        );
                    }
                    let provider_space = self
                        .effect_arg_provider_space(&arg, pass_mode)
                        .or_else(|| self.concrete_borrow_provider_for_effect_handle_ty(provider.ty))
                        .or_else(|| {
                            matches!(pass_mode, super::EffectPassMode::ByTempPlace)
                                .then_some(super::ProviderAddressSpace::Memory)
                        });
                    if matches!(
                        pass_mode,
                        super::EffectPassMode::ByPlace | super::EffectPassMode::ByTempPlace
                    ) && provider_space.is_none()
                        && !self.effect_arg_provider_space_can_remain_unresolved(&arg)
                    {
                        panic!(
                            "effect arg provider space must be explicit for {pass_mode:?} at {key_path:?}"
                        );
                    }
                    if let Some(resolved_binding) =
                        callee_effect_env.resolved_binding(self.db, req.binding_idx as usize)
                    {
                        let provider_idx = resolved_binding.provider.provider_idx;
                        let specialization = self.specialize_effect_provider_binding(
                            resolved_binding.provider,
                            provider,
                            &arg,
                            pass_mode,
                            provider_target_ty,
                            provider_space,
                        );
                        if let Some(previous) =
                            specialized_providers.insert(provider_idx, specialization.clone())
                        {
                            assert_eq!(
                                previous, specialization,
                                "conflicting call-site provider specialization for function effect provider slot {} at {:?}",
                                provider_idx, key_path,
                            );
                        }
                    }
                    resolved_args.push(super::ResolvedEffectArg {
                        param_idx,
                        binding_idx: req.binding_idx,
                        key: key_path,
                        arg,
                        pass_mode,
                        required_mut: req.required_mut,
                        key_kind,
                        instantiated_key_ty,
                        provider_target_ty,
                        provider: provider_space,
                    });
                }
                EffectResolution::BlockedByBarrier => {}
                EffectResolution::Missing => {
                    self.push_diag(BodyDiag::MissingEffect {
                        primary: call_span.clone(),
                        func,
                        key: key_path,
                    });
                }
                EffectResolution::Ambiguous => {
                    self.push_diag(BodyDiag::AmbiguousEffect {
                        primary: call_span.clone(),
                        func,
                        key: key_path,
                    });
                }
            }
        }
        let mut providers = specialized_providers.into_values().collect::<Vec<_>>();
        providers.sort_by_key(|provider| provider.provider.provider_idx);
        *callable.effect_providers_mut() = providers;

        resolved_args
    }

    fn instantiate_callable_effect_layout_args(
        &mut self,
        callable: &mut Callable<'db>,
        callee: crate::hir_def::Func<'db>,
        effect_idx: usize,
        actual_key_ty: TyId<'db>,
    ) {
        instantiate_callable_effect_layout_args(
            self.db,
            callee,
            effect_idx,
            self.table.fold_ty(self.db, actual_key_ty),
            callable.generic_args_mut(),
        );
    }

    fn resolve_effect_query(
        &mut self,
        func: crate::hir_def::Func<'db>,
        req: EffectRequirementDecl<'db>,
        query: EffectQuery<'db>,
        call_span: DynLazySpan<'db>,
    ) -> EffectResolution<'db> {
        let mut viable: SmallVec<[EffectEvidence<'db>; 2]> = SmallVec::new();
        let effect_env = self.env.effect_env().clone();
        for frame in effect_env.lookup_effect_frames(&query, self) {
            match frame {
                FrameLookupResult::KeyedMatched {
                    entries,
                    blocked_by_barrier,
                    barrier_reason,
                } => {
                    for matched in entries.iter().cloned() {
                        if let Some(evidence) =
                            self.evaluate_keyed_entry(query.required_mut, matched)
                        {
                            viable.push(evidence);
                        }
                    }
                    if viable.is_empty() {
                        return if blocked_by_barrier {
                            let _ = barrier_reason;
                            EffectResolution::BlockedByBarrier
                        } else {
                            EffectResolution::Missing
                        };
                    }
                    return self.choose_effect_evidence(req.name, viable);
                }
                FrameLookupResult::KeyedFamily { entries, providers } => {
                    let mut family_viable = SmallVec::new();
                    for entry in entries.iter().cloned() {
                        let Some(matched) = self.match_family_keyed_entry(&query.key, entry) else {
                            continue;
                        };
                        if let Some(evidence) =
                            self.evaluate_keyed_entry(query.required_mut, matched)
                        {
                            family_viable.push(evidence);
                        }
                    }
                    for provider in providers.iter().copied() {
                        let evidence = match query.key.clone() {
                            EffectPatternKey::Type(type_query) => self
                                .evaluate_unkeyed_type_provider(
                                    type_query,
                                    provider,
                                    query.required_mut,
                                ),
                            EffectPatternKey::Trait(trait_query) => {
                                self.evaluate_unkeyed_trait_provider(trait_query, provider)
                            }
                        };
                        if let Some(evidence) = evidence {
                            family_viable.push(evidence);
                        }
                    }
                    if !family_viable.is_empty() {
                        return self.choose_effect_evidence(req.name, family_viable);
                    }
                }
                FrameLookupResult::Unkeyed { providers } => {
                    for provider in providers {
                        let evidence = match query.key.clone() {
                            EffectPatternKey::Type(type_query) => self
                                .evaluate_unkeyed_type_provider(
                                    type_query,
                                    provider,
                                    query.required_mut,
                                ),
                            EffectPatternKey::Trait(trait_query) => {
                                self.evaluate_unkeyed_trait_provider(trait_query, provider)
                            }
                        };
                        if let Some(evidence) = evidence {
                            viable.push(evidence);
                        }
                    }
                    if !viable.is_empty() {
                        return self.choose_effect_evidence(req.name, viable);
                    }
                }
            }
        }
        let _ = (func, call_span);
        EffectResolution::Missing
    }

    fn match_family_keyed_entry(
        &mut self,
        query: &EffectPatternKey<'db>,
        entry: FamilyKeyedEntry<'db>,
    ) -> Option<MatchedKeyedEntry<'db>> {
        match entry {
            FamilyKeyedEntry::Witness(witness) => {
                let key_commit = query_matches_witness(self, query, &witness.key)?;
                Some(MatchedKeyedEntry::Witness(MatchedWitness {
                    witness,
                    key_commit,
                }))
            }
            FamilyKeyedEntry::Forwarder(forwarder) => {
                let key_commit = query_matches_forwarder(self, query, &forwarder.key)?;
                Some(MatchedKeyedEntry::Forwarder(MatchedForwarder {
                    forwarder,
                    key_commit,
                }))
            }
        }
    }

    fn choose_effect_evidence(
        &self,
        required_name: Option<IdentId<'db>>,
        viable: SmallVec<[EffectEvidence<'db>; 2]>,
    ) -> EffectResolution<'db> {
        match viable.as_slice() {
            [only] => EffectResolution::Chosen(Box::new(only.clone())),
            _ => {
                let Some(required_name) = required_name else {
                    return EffectResolution::Ambiguous;
                };
                let mut name_matches = viable.into_iter().filter(|evidence| {
                    let provider = evidence_provider(evidence);
                    match (provider.origin, provider.binding) {
                        (
                            EffectOrigin::Param {
                                name: Some(name), ..
                            },
                            _,
                        ) => name == required_name,
                        (EffectOrigin::With { .. }, Some(binding)) => {
                            binding.binding_name(&self.env) == required_name
                        }
                        _ => false,
                    }
                });
                if let Some(best) = name_matches.next()
                    && name_matches.next().is_none()
                {
                    EffectResolution::Chosen(Box::new(best))
                } else {
                    EffectResolution::Ambiguous
                }
            }
        }
    }

    fn evaluate_keyed_entry(
        &mut self,
        required_mut: bool,
        matched: MatchedKeyedEntry<'db>,
    ) -> Option<EffectEvidence<'db>> {
        let (provider, transport, key_kind, target_ty, key_commit) = match matched {
            MatchedKeyedEntry::Witness(matched) => {
                let (key_kind, target_ty) = match matched.witness.key {
                    StoredEffectKey::Type(stored) => (EffectKeyKind::Type, Some(stored.carrier)),
                    StoredEffectKey::Trait(_) => (EffectKeyKind::Trait, None),
                };
                (
                    matched.witness.provider,
                    matched.witness.transport,
                    key_kind,
                    target_ty,
                    matched.key_commit,
                )
            }
            MatchedKeyedEntry::Forwarder(matched) => {
                let (key_kind, target_ty) = match matched.forwarder.key {
                    ForwardedEffectKey::Type(forwarded) => {
                        (EffectKeyKind::Type, Some(forwarded.carrier))
                    }
                    ForwardedEffectKey::Trait(_) => (EffectKeyKind::Trait, None),
                };
                (
                    matched.forwarder.provider,
                    matched.forwarder.transport,
                    key_kind,
                    target_ty,
                    matched.key_commit,
                )
            }
        };
        let arg_style = match (transport, target_ty) {
            (WitnessTransport::Direct, Some(target_ty)) => self
                .direct_arg_style_for_provider(provider, target_ty, required_mut)
                .unwrap_or(EffectArgStyle::Value),
            _ => EffectArgStyle::Value,
        };
        Some(EffectEvidence::Keyed {
            provider,
            key_kind,
            target_ty,
            commit: EffectCommitPlan {
                key_match: Some(key_commit),
                trait_solutions: SmallVec::new(),
                provider_resolution: None,
                extra_unifications: SmallVec::new(),
            },
            arg_style,
        })
    }

    fn evaluate_unkeyed_type_provider(
        &mut self,
        query: TypePatternKey<'db>,
        provider: ProvidedEffect<'db>,
        required_mut: bool,
    ) -> Option<EffectEvidence<'db>> {
        let direct_style =
            self.direct_arg_style_for_provider(provider, query.carrier, required_mut);
        if let Some(arg_style) = direct_style {
            let snapshot = self.snapshot_state();
            let ok = apply_key_match_commit(
                self,
                KeyMatchCommit::QueryToType {
                    query: query.clone(),
                    actual: provider
                        .ty
                        .as_capability(self.db)
                        .map(|(_, inner)| inner)
                        .unwrap_or(provider.ty),
                },
            );
            self.rollback_state(snapshot);
            if ok {
                return Some(EffectEvidence::UnkeyedType {
                    provider,
                    commit: EffectCommitPlan {
                        key_match: Some(KeyMatchCommit::QueryToType {
                            query: query.clone(),
                            actual: provider
                                .ty
                                .as_capability(self.db)
                                .map(|(_, inner)| inner)
                                .unwrap_or(provider.ty),
                        }),
                        trait_solutions: SmallVec::new(),
                        provider_resolution: None,
                        extra_unifications: SmallVec::new(),
                    },
                    arg_style,
                });
            }
        }

        let resolution = self.effect_provider_target_resolution(provider.ty, required_mut)?;
        let snapshot = self.snapshot_state();
        let ok = apply_key_match_commit(
            self,
            KeyMatchCommit::QueryToType {
                query: query.clone(),
                actual: resolution.target_ty,
            },
        );
        self.rollback_state(snapshot);
        ok.then_some(EffectEvidence::UnkeyedType {
            provider,
            commit: EffectCommitPlan {
                key_match: Some(KeyMatchCommit::QueryToType {
                    query,
                    actual: resolution.target_ty,
                }),
                trait_solutions: SmallVec::new(),
                provider_resolution: Some(resolution),
                extra_unifications: SmallVec::new(),
            },
            arg_style: EffectArgStyle::Value,
        })
    }

    fn evaluate_unkeyed_trait_provider(
        &mut self,
        query: TraitPatternKey<'db>,
        provider: ProvidedEffect<'db>,
    ) -> Option<EffectEvidence<'db>> {
        let provider_ty = self.table.fold_ty(self.db, provider.ty);
        if provider_ty.has_var(self.db) {
            return None;
        }
        let instantiated = instantiate_trait_pattern_in(self.db, &mut self.table, query);
        let args = std::iter::once(provider_ty)
            .chain(instantiated.args(self.db).iter().skip(1).copied())
            .collect::<Vec<_>>();
        let trait_goal = TraitInstId::new(
            self.db,
            instantiated.def(self.db),
            args,
            instantiated.assoc_type_bindings(self.db).clone(),
        );
        let GoalSatisfiability::Satisfied(solution) =
            self.trait_effect_goal_satisfiability(trait_goal)
        else {
            return None;
        };
        Some(EffectEvidence::UnkeyedTrait {
            provider,
            commit: EffectCommitPlan {
                key_match: None,
                trait_solutions: SmallVec::from_iter([(trait_goal, solution)]),
                provider_resolution: None,
                extra_unifications: SmallVec::new(),
            },
            arg_style: EffectArgStyle::Value,
        })
    }

    fn direct_arg_style_for_provider(
        &self,
        provider: ProvidedEffect<'db>,
        target_ty: TyId<'db>,
        _: bool,
    ) -> Option<EffectArgStyle> {
        let target_ty = normalize_ty(self.db, target_ty, self.env.scope(), self.env.assumptions());
        if effect_handle_metadata(self.db, self.env.scope(), self.env.assumptions(), target_ty)
            .is_some()
        {
            return Some(EffectArgStyle::Value);
        }
        let place = match provider.origin {
            EffectOrigin::With { value_expr } => self.env.expr_place(value_expr),
            EffectOrigin::Param { .. } => provider
                .binding
                .map(|binding| Place::new(PlaceBase::Binding(binding))),
        };
        Some(match place {
            Some(_) => EffectArgStyle::Place,
            None if matches!(provider.origin, EffectOrigin::With { .. }) => {
                EffectArgStyle::TempPlace
            }
            None => return None,
        })
    }

    fn effect_arg_for_provider(
        &mut self,
        provider: ProvidedEffect<'db>,
        arg_style: EffectArgStyle,
        required_mut: bool,
    ) -> (super::EffectArg<'db>, super::EffectPassMode) {
        if required_mut && !self.provider_supports_mut(provider) {
            return (super::EffectArg::Unknown, super::EffectPassMode::Unknown);
        }

        match arg_style {
            EffectArgStyle::Place => {
                let place = match provider.origin {
                    EffectOrigin::With { value_expr } => self.env.expr_place(value_expr),
                    EffectOrigin::Param { .. } => provider
                        .binding
                        .map(|binding| Place::new(PlaceBase::Binding(binding))),
                };
                (
                    place
                        .map(super::EffectArg::Place)
                        .unwrap_or(super::EffectArg::Unknown),
                    super::EffectPassMode::ByPlace,
                )
            }
            EffectArgStyle::TempPlace => match provider.origin {
                EffectOrigin::With { value_expr } => (
                    super::EffectArg::Value(value_expr),
                    super::EffectPassMode::ByTempPlace,
                ),
                EffectOrigin::Param { .. } => {
                    (super::EffectArg::Unknown, super::EffectPassMode::Unknown)
                }
            },
            EffectArgStyle::Value => (
                match provider.origin {
                    EffectOrigin::With { value_expr } => super::EffectArg::Value(value_expr),
                    EffectOrigin::Param { .. } => provider
                        .binding
                        .map(super::EffectArg::Binding)
                        .unwrap_or(super::EffectArg::Unknown),
                },
                super::EffectPassMode::ByValue,
            ),
        }
    }

    fn provider_supports_mut(&mut self, provider: ProvidedEffect<'db>) -> bool {
        if let Some((kind, _)) = provider.ty.as_capability(self.db) {
            return matches!(kind, CapabilityKind::Mut)
                || self
                    .effect_provider_target_resolution(provider.ty, true)
                    .is_some();
        }

        provider.is_mut
            || self
                .effect_provider_target_resolution(provider.ty, true)
                .is_some()
    }

    fn inferred_provider_ty_for_effect_arg(
        &mut self,
        provider: ProvidedEffect<'db>,
        arg: &super::EffectArg<'db>,
        pass_mode: super::EffectPassMode,
        provider_target_ty: Option<TyId<'db>>,
    ) -> Option<TyId<'db>> {
        match pass_mode {
            super::EffectPassMode::ByValue => Some(self.table.fold_ty(self.db, provider.ty)),
            super::EffectPassMode::ByTempPlace => {
                let target_ty = self.table.fold_ty(self.db, provider_target_ty?);
                let mem_ptr_ctor =
                    resolve_lib_type_path(self.db, self.env.scope(), "core::effect_ref::MemPtr")?;
                Some(TyId::app(self.db, mem_ptr_ctor, target_ty))
            }
            super::EffectPassMode::ByPlace => {
                let binding = match arg {
                    super::EffectArg::Place(place) => {
                        let PlaceBase::Binding(binding) = place.base;
                        Some(binding)
                    }
                    super::EffectArg::Binding(binding) => Some(*binding),
                    super::EffectArg::Value(_) | super::EffectArg::Unknown => provider.binding,
                }?;

                let inferred = match binding {
                    LocalBinding::EffectParam { site, idx, .. } => self
                        .env
                        .resolved_provider_binding(site, idx)
                        .map(|binding| binding.provider_ty)
                        .unwrap_or(provider.ty),
                    LocalBinding::Param {
                        site: ParamSite::EffectField(effect_site),
                        ..
                    } => {
                        let contract = match effect_site {
                            EffectParamSite::Contract(contract)
                            | EffectParamSite::ContractInit { contract }
                            | EffectParamSite::ContractRecvArm { contract, .. } => contract,
                            EffectParamSite::Func(_) => {
                                unreachable!(
                                    "effect field bindings cannot originate from function sites"
                                )
                            }
                        };
                        let ident = binding.binding_name(&self.env);
                        contract
                            .fields(self.db)
                            .get(&ident)
                            .map(|field| field.declared_ty)?
                    }
                    LocalBinding::Local { .. } | LocalBinding::Param { .. } => provider.ty,
                };

                Some(self.table.fold_ty(self.db, inferred))
            }
            super::EffectPassMode::Unknown => None,
        }
    }

    fn provider_target_ty_for_effect_arg(
        &mut self,
        provider: ProvidedEffect<'db>,
        instantiated_key_ty: Option<TyId<'db>>,
        required_mut: bool,
    ) -> Option<TyId<'db>> {
        let instantiated_key_ty = instantiated_key_ty.map(|ty| self.table.fold_ty(self.db, ty));
        if let Some(target_ty) = instantiated_key_ty
            && let Some(metadata) =
                effect_handle_metadata(self.db, self.env.scope(), self.env.assumptions(), target_ty)
        {
            return Some(self.table.fold_ty(self.db, metadata.target_ty));
        }
        let provider_ty = self.table.fold_ty(self.db, provider.ty);
        self.effect_provider_target_resolution(provider_ty, required_mut)
            .map(|resolution| self.table.fold_ty(self.db, resolution.target_ty))
            .or(instantiated_key_ty)
    }

    fn effect_arg_is_valid(
        &self,
        arg: &super::EffectArg<'db>,
        pass_mode: super::EffectPassMode,
    ) -> bool {
        match pass_mode {
            super::EffectPassMode::ByPlace => matches!(arg, &super::EffectArg::Place(_)),
            super::EffectPassMode::ByTempPlace => matches!(arg, &super::EffectArg::Value(_)),
            super::EffectPassMode::ByValue => !matches!(
                arg,
                &super::EffectArg::Unknown | &super::EffectArg::Place(_)
            ),
            super::EffectPassMode::Unknown => false,
        }
    }

    fn effect_arg_provider_space(
        &self,
        arg: &super::EffectArg<'db>,
        pass_mode: super::EffectPassMode,
    ) -> Option<super::ProviderAddressSpace> {
        match pass_mode {
            super::EffectPassMode::ByTempPlace => match arg {
                super::EffectArg::Value(expr) => self.env.typed_expr(*expr).and_then(|prop| {
                    prop.borrow_provider
                        .or_else(|| self.concrete_borrow_provider_for_effect_handle_ty(prop.ty))
                }),
                super::EffectArg::Place(place) => self.concrete_borrow_provider_for_place(place),
                super::EffectArg::Binding(binding) => {
                    self.concrete_borrow_provider_for_binding(*binding)
                }
                super::EffectArg::Unknown => None,
            },
            super::EffectPassMode::ByPlace => match arg {
                super::EffectArg::Place(place) => self.concrete_borrow_provider_for_place(place),
                super::EffectArg::Binding(binding) => {
                    self.concrete_borrow_provider_for_binding(*binding)
                }
                super::EffectArg::Value(_) | super::EffectArg::Unknown => None,
            },
            super::EffectPassMode::ByValue => match arg {
                super::EffectArg::Place(place) => self.concrete_borrow_provider_for_place(place),
                super::EffectArg::Value(expr) => self.env.typed_expr(*expr).and_then(|prop| {
                    prop.borrow_provider
                        .or_else(|| self.concrete_borrow_provider_for_effect_handle_ty(prop.ty))
                }),
                super::EffectArg::Binding(binding) => {
                    self.concrete_borrow_provider_for_binding(*binding)
                }
                super::EffectArg::Unknown => None,
            },
            super::EffectPassMode::Unknown => None,
        }
    }

    fn effect_arg_provider_space_can_remain_unresolved(&self, arg: &super::EffectArg<'db>) -> bool {
        let binding = match arg {
            super::EffectArg::Place(place) => {
                let PlaceBase::Binding(binding) = place.base;
                Some(binding)
            }
            super::EffectArg::Binding(binding) => Some(*binding),
            super::EffectArg::Value(_) | super::EffectArg::Unknown => None,
        };

        matches!(binding, Some(LocalBinding::EffectParam { .. }))
    }

    fn specialize_effect_provider_binding(
        &mut self,
        slot: ProviderBinding<'db>,
        provided: ProvidedEffect<'db>,
        arg: &super::EffectArg<'db>,
        pass_mode: super::EffectPassMode,
        provider_target_ty: Option<TyId<'db>>,
        provider_space: Option<super::ProviderAddressSpace>,
    ) -> EffectProviderSpecialization<'db> {
        let provenance = self
            .effect_provider_provenance(provided, arg)
            .unwrap_or_else(|| {
                panic!(
                    "missing call-site provider provenance for {:?} in {:?}",
                    slot.provider_idx,
                    self.env.owner(),
                )
            });
        let target_ty = provider_target_ty.map(|ty| self.table.fold_ty(self.db, ty));
        let provider = self
            .existing_provider_binding_for_effect_arg(provided, arg)
            .filter(|provider| {
                target_ty.is_none_or(|target_ty| provider.effective_target_ty() == target_ty)
            })
            .map(|provider| ProviderBinding {
                provider_idx: slot.provider_idx,
                ..provider
            })
            .unwrap_or_else(|| {
                let provider_ty = self
                    .inferred_provider_ty_for_effect_arg(
                        provided,
                        arg,
                        pass_mode,
                        provider_target_ty,
                    )
                    .unwrap_or_else(|| self.table.fold_ty(self.db, provided.ty));
                let semantics = provider_semantics_for_specialized_call(
                    self.db,
                    self.env.scope(),
                    self.env.assumptions(),
                    provider_ty,
                    target_ty,
                    provider_space,
                    match pass_mode {
                        super::EffectPassMode::ByPlace => ProviderTransport::ByPlace,
                        super::EffectPassMode::ByTempPlace => ProviderTransport::ByTempPlace,
                        super::EffectPassMode::ByValue | super::EffectPassMode::Unknown => {
                            ProviderTransport::ByValue
                        }
                    },
                );
                ProviderBinding {
                    provider_idx: slot.provider_idx,
                    provider_ty,
                    is_mut: provided.is_mut,
                    source: slot.source,
                    semantics,
                }
            });
        EffectProviderSpecialization {
            provider,
            provenance,
        }
    }

    fn existing_provider_binding_for_effect_arg(
        &self,
        provided: ProvidedEffect<'db>,
        arg: &super::EffectArg<'db>,
    ) -> Option<ProviderBinding<'db>> {
        let binding = match arg {
            super::EffectArg::Place(place) => {
                if !place.projections.is_empty() {
                    return None;
                }
                let PlaceBase::Binding(binding) = place.base;
                Some(binding)
            }
            super::EffectArg::Binding(binding) => Some(*binding),
            super::EffectArg::Value(_) | super::EffectArg::Unknown => provided.binding,
        }?;
        match binding {
            LocalBinding::EffectParam {
                site, provider_idx, ..
            } => self.env.provider_binding(site, provider_idx),
            LocalBinding::Param {
                site: ParamSite::EffectField(effect_site),
                idx,
                ..
            } => self.env.resolved_provider_binding(effect_site, idx),
            LocalBinding::Local { .. } | LocalBinding::Param { .. } => None,
        }
    }

    fn effect_provider_provenance(
        &self,
        provided: ProvidedEffect<'db>,
        arg: &super::EffectArg<'db>,
    ) -> Option<EffectProviderProvenance<'db>> {
        let owner = self.env.owner();
        let binding = match arg {
            super::EffectArg::Place(place) => {
                if !place.projections.is_empty()
                    && let EffectOrigin::With { value_expr } = provided.origin
                {
                    return Some(EffectProviderProvenance::Expr {
                        owner,
                        expr: value_expr,
                    });
                }
                let PlaceBase::Binding(binding) = place.base;
                Some(binding)
            }
            super::EffectArg::Binding(binding) => Some(*binding),
            super::EffectArg::Value(_) | super::EffectArg::Unknown => provided.binding,
        };
        binding
            .map(|binding| EffectProviderProvenance::Binding { owner, binding })
            .or(match provided.origin {
                EffectOrigin::With { value_expr } => Some(EffectProviderProvenance::Expr {
                    owner,
                    expr: value_expr,
                }),
                EffectOrigin::Param { .. } => None,
            })
    }

    fn query_type_key(&self, key: &EffectPatternKey<'db>) -> Option<TyId<'db>> {
        match key {
            EffectPatternKey::Type(key) => Some(key.carrier),
            EffectPatternKey::Trait(_) => None,
        }
    }

    fn trait_effect_goal_satisfiability(
        &self,
        trait_goal: TraitInstId<'db>,
    ) -> GoalSatisfiability<'db> {
        self.trait_effect_goal_satisfiability_in_scope(
            self.env.scope(),
            self.env.assumptions(),
            trait_goal,
        )
    }

    pub(super) fn trait_effect_goal_satisfiability_in_scope(
        &self,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        trait_goal: TraitInstId<'db>,
    ) -> GoalSatisfiability<'db> {
        let solve_cx = TraitSolveCx::new(self.db, scope).with_assumptions(assumptions);
        let query = crate::analysis::ty::trait_resolution::CanonicalGoalQuery::new(
            self.db,
            trait_goal,
            assumptions,
        );
        crate::analysis::ty::trait_resolution::is_goal_query_satisfiable(self.db, solve_cx, &query)
            .clone()
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

    fn commit_provider_target_resolution(
        &mut self,
        resolution: ProviderTargetResolution<'db>,
    ) -> TyId<'db> {
        self.commit_provider_target_resolution_in_scope(
            resolution,
            self.env.scope(),
            self.env.assumptions(),
        )
    }

    pub(super) fn commit_provider_target_resolution_in_scope(
        &mut self,
        resolution: ProviderTargetResolution<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> TyId<'db> {
        let mut target_ty = self.renormalize_effect_provider_target_ty_in_scope(
            resolution.target_seed_ty,
            scope,
            assumptions,
        );
        if let Some((handle_goal, handle_solution)) = resolution.handle_proof {
            self.commit_trait_goal_solution(handle_goal, handle_solution);
            target_ty =
                self.renormalize_effect_provider_target_ty_in_scope(target_ty, scope, assumptions);
        }
        if let Some((effect_ref_goal, effect_ref_solution)) = resolution.effect_ref_proof {
            self.commit_trait_goal_solution(effect_ref_goal, effect_ref_solution);
            target_ty =
                self.renormalize_effect_provider_target_ty_in_scope(target_ty, scope, assumptions);
        }
        if let Some((effect_ref_mut_goal, effect_ref_mut_solution)) =
            resolution.effect_ref_mut_proof
        {
            self.commit_trait_goal_solution(effect_ref_mut_goal, effect_ref_mut_solution);
            target_ty =
                self.renormalize_effect_provider_target_ty_in_scope(target_ty, scope, assumptions);
        }
        target_ty
    }

    fn renormalize_effect_provider_target_ty_in_scope(
        &mut self,
        target_ty: TyId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> TyId<'db> {
        normalize_ty(
            self.db,
            self.table.fold_ty(self.db, target_ty),
            scope,
            assumptions,
        )
        .fold_with(self.db, &mut self.table)
    }

    fn select_type_effect_binding_match(
        &mut self,
        pattern: TypePatternKey<'db>,
        provided: ProvidedEffect<'db>,
    ) -> Option<TypeEffectBindingMatch<'db>> {
        self.select_type_effect_binding_match_in_scope(
            pattern,
            provided,
            self.env.scope(),
            self.env.assumptions(),
        )
    }

    pub(super) fn select_type_effect_binding_match_in_scope(
        &mut self,
        pattern: TypePatternKey<'db>,
        provided: ProvidedEffect<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Option<TypeEffectBindingMatch<'db>> {
        let can_commit_key_relation = |this: &mut Self, given: TyId<'db>| {
            let snapshot = this.snapshot_state();
            let ok = apply_key_match_commit(
                this,
                KeyMatchCommit::QueryToType {
                    query: pattern.clone(),
                    actual: given,
                },
            );
            this.rollback_state(snapshot);
            ok
        };

        let matches_key = |this: &mut Self, actual_ty: TyId<'db>| {
            if can_commit_key_relation(this, actual_ty) {
                return true;
            }

            let erased_actual =
                erase_unresolved_trailing_layout_hole_default_args(this.db, actual_ty);
            erased_actual != actual_ty && can_commit_key_relation(this, erased_actual)
        };
        let direct_ty = if let Some((_, inner)) = provided.ty.as_capability(self.db) {
            inner
        } else {
            provided.ty
        };

        if matches_key(self, direct_ty) {
            return Some(TypeEffectBindingMatch::Direct { given: direct_ty });
        }

        self.effect_provider_target_resolution_in_scope(provided.ty, false, scope, assumptions)
            .and_then(|resolution| {
                matches_key(self, resolution.target_ty)
                    .then_some(TypeEffectBindingMatch::Provider { resolution })
            })
    }

    fn effect_provider_target_resolution(
        &mut self,
        provided_ty: TyId<'db>,
        required_mut: bool,
    ) -> Option<ProviderTargetResolution<'db>> {
        self.effect_provider_target_resolution_in_scope(
            provided_ty,
            required_mut,
            self.env.scope(),
            self.env.assumptions(),
        )
    }

    fn effect_provider_target_resolution_in_scope(
        &mut self,
        provided_ty: TyId<'db>,
        required_mut: bool,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Option<ProviderTargetResolution<'db>> {
        if let Some((kind, inner_ty)) = provided_ty.as_capability(self.db) {
            if required_mut && !matches!(kind, CapabilityKind::Mut) {
                return None;
            }
            return Some(ProviderTargetResolution::direct(inner_ty));
        }

        let effect_ref_trait = resolve_core_trait(self.db, scope, &["EffectRef"])
            .expect("missing required core trait `core::EffectRef`");
        let effect_ref_mut_trait = resolve_core_trait(self.db, scope, &["EffectRefMut"])
            .expect("missing required core trait `core::EffectRefMut`");
        let effect_handle_trait = resolve_core_trait(self.db, scope, &["EffectHandle"])
            .expect("missing required core trait `core::EffectHandle`");
        let target_ident = IdentId::new(self.db, "Target".to_string());
        let effect_handle_inst = TraitInstId::new(
            self.db,
            effect_handle_trait,
            vec![provided_ty],
            IndexMap::new(),
        );
        let GoalSatisfiability::Satisfied(handle_solution) =
            self.trait_effect_goal_satisfiability_in_scope(scope, assumptions, effect_handle_inst)
        else {
            return None;
        };

        let snapshot = self.snapshot_state();
        let resolution = (|| {
            self.commit_trait_goal_solution(effect_handle_inst, handle_solution);

            let target_assoc = effect_handle_inst.assoc_ty(self.db, target_ident)?;
            let mut target_ty = normalize_ty(self.db, target_assoc, scope, assumptions)
                .fold_with(self.db, &mut self.table);
            let mut provided_ty = self.table.fold_ty(self.db, provided_ty);

            let effect_ref_inst = TraitInstId::new(
                self.db,
                effect_ref_trait,
                vec![provided_ty, target_ty],
                IndexMap::new(),
            );
            let GoalSatisfiability::Satisfied(effect_ref_solution) =
                self.trait_effect_goal_satisfiability_in_scope(scope, assumptions, effect_ref_inst)
            else {
                return None;
            };
            self.commit_trait_goal_solution(effect_ref_inst, effect_ref_solution);
            provided_ty = self.table.fold_ty(self.db, provided_ty);
            target_ty =
                self.renormalize_effect_provider_target_ty_in_scope(target_ty, scope, assumptions);

            let effect_ref_mut_proof = if required_mut {
                let effect_ref_mut_inst = TraitInstId::new(
                    self.db,
                    effect_ref_mut_trait,
                    vec![provided_ty, target_ty],
                    IndexMap::new(),
                );
                let GoalSatisfiability::Satisfied(effect_ref_mut_solution) = self
                    .trait_effect_goal_satisfiability_in_scope(
                        scope,
                        assumptions,
                        effect_ref_mut_inst,
                    )
                else {
                    return None;
                };
                self.commit_trait_goal_solution(effect_ref_mut_inst, effect_ref_mut_solution);
                target_ty = self.renormalize_effect_provider_target_ty_in_scope(
                    target_ty,
                    scope,
                    assumptions,
                );
                Some((effect_ref_mut_inst, effect_ref_mut_solution))
            } else {
                None
            };

            Some(ProviderTargetResolution {
                target_ty,
                target_seed_ty: target_assoc,
                handle_proof: Some((effect_handle_inst, handle_solution)),
                effect_ref_proof: Some((effect_ref_inst, effect_ref_solution)),
                effect_ref_mut_proof,
            })
        })();
        self.rollback_state(snapshot);
        resolution
    }

    pub(super) fn apply_effect_commit_plan(&mut self, commit: EffectCommitPlan<'db>) -> bool {
        let snapshot = self.snapshot_state();
        let ok = self.apply_effect_commit_plan_inner(commit);
        if ok {
            self.commit_state(snapshot);
        } else {
            self.rollback_state(snapshot);
        }
        ok
    }

    fn apply_effect_commit_plan_inner(&mut self, commit: EffectCommitPlan<'db>) -> bool {
        for (goal, solution) in commit.trait_solutions {
            self.commit_trait_goal_solution(goal, solution);
        }
        if let Some(resolution) = commit.provider_resolution {
            self.commit_provider_target_resolution(resolution);
        }
        if let Some(key_match) = commit.key_match
            && !apply_key_match_commit(self, key_match)
        {
            return false;
        }
        commit
            .extra_unifications
            .into_iter()
            .all(|(expected, given)| self.table.unify(expected, given).is_ok())
    }

    fn validate_keyed_with(
        &mut self,
        key_path: PathId<'db>,
        provider: ProvidedEffect<'db>,
        span: DynLazySpan<'db>,
    ) -> Result<
        (
            EffectWitness<'db, ProvidedEffect<'db>>,
            EffectCommitPlan<'db>,
        ),
        Box<EffectBarrier<'db>>,
    > {
        let Some(pattern) = build_barrier_pattern_for_with_key(self, key_path) else {
            let fallback = build_conservative_same_family_barrier_pattern_in_scope(
                self.db,
                self.env.scope(),
                self.env.assumptions(),
                key_path,
            );
            if let Some(EffectPatternKey::Trait(pattern)) = fallback.clone() {
                self.push_diag(BodyDiag::WithEffectTraitUnsatisfied {
                    primary: span.clone(),
                    key: key_path,
                    trait_req: TraitInstId::new(
                        self.db,
                        pattern.def,
                        std::iter::once(provider.ty)
                            .chain(pattern.args_no_self.iter().copied())
                            .collect::<Vec<_>>(),
                        pattern
                            .assoc_bindings
                            .iter()
                            .copied()
                            .collect::<IndexMap<_, _>>(),
                    ),
                    given: provider.ty,
                });
                return Err(Box::new(EffectBarrier {
                    pattern: EffectPatternKey::Trait(pattern),
                    reason: BarrierReason::InvalidExplicitTraitKey { span, key_path },
                }));
            }
            let expected = TyId::invalid(self.db, InvalidCause::Other);
            self.push_diag(BodyDiag::WithEffectTypeUnsatisfied {
                primary: span.clone(),
                key: key_path,
                expected,
                given: provider.ty,
            });
            return Err(Box::new(EffectBarrier {
                pattern: fallback.unwrap_or(EffectPatternKey::Type(TypePatternKey {
                    carrier: expected,
                    family: crate::analysis::ty::effects::effect_family_for_type(self.db, expected),
                    slots: crate::analysis::ty::effects::PatternSlots::empty(),
                })),
                reason: BarrierReason::InvalidExplicitTypeKey { span, key_path },
            }));
        };

        self.build_keyed_witness_from_pattern_in_scope(
            pattern,
            key_path,
            provider,
            span,
            KeyedWitnessBuildOptions {
                scope: KeyedWitnessBuildScope {
                    scope: self.env.scope(),
                    assumptions: self.env.base_assumptions(),
                },
                emit_diag: true,
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
        Box<EffectBarrier<'db>>,
    > {
        let KeyedWitnessBuildOptions {
            scope: KeyedWitnessBuildScope { scope, assumptions },
            emit_diag,
            mode,
        } = options;
        match pattern {
            EffectPatternKey::Type(pattern) => {
                if matches!(mode, WitnessBuildMode::ExplicitKeyedWith)
                    && pattern
                        .slots
                        .entries
                        .iter()
                        .any(|slot| slot.kind == PatternSlotKind::OmittedExplicitArg)
                {
                    if emit_diag {
                        self.push_diag(BodyDiag::WithEffectTypeUnsatisfied {
                            primary: span.clone(),
                            key: key_path,
                            expected: pattern.carrier,
                            given: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Type(pattern),
                        reason: BarrierReason::InvalidExplicitTypeKey {
                            span: span.clone(),
                            key_path,
                        },
                    }));
                }
                let Some(binding_match) =
                    self.select_type_effect_binding_match(pattern.clone(), provider)
                else {
                    if emit_diag {
                        self.push_diag(BodyDiag::WithEffectTypeUnsatisfied {
                            primary: span.clone(),
                            key: key_path,
                            expected: pattern.carrier,
                            given: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Type(pattern),
                        reason: BarrierReason::InvalidExplicitTypeKey {
                            span: span.clone(),
                            key_path,
                        },
                    }));
                };
                let (actual, provider_resolution, transport) = match binding_match {
                    TypeEffectBindingMatch::Direct { given } => {
                        (given, None, WitnessTransport::Direct)
                    }
                    TypeEffectBindingMatch::Provider { resolution } => (
                        resolution.target_ty,
                        Some(resolution),
                        WitnessTransport::ByValue,
                    ),
                };
                let snapshot = self.snapshot_state();
                let specialized = self
                    .specialize_type_pattern_key(pattern.clone(), actual)
                    .map(|carrier| StoredTypeKey {
                        carrier,
                        family: pattern.family,
                    })
                    .and_then(|stored| {
                        finalize_stored_effect_key(
                            self.db,
                            StoredEffectKey::Type(stored),
                            scope,
                            assumptions,
                        )
                    });
                self.rollback_state(snapshot);
                let Some(key) = specialized.filter(|key| {
                    !matches!(
                        (mode, key),
                        (WitnessBuildMode::ExplicitKeyedWith, StoredEffectKey::Type(stored))
                            if stored_value_contains_implicit_layout_params(
                                self.db,
                                stored.carrier,
                            ) || stored_value_contains_out_of_scope_params(
                                self.db,
                                scope,
                                stored.carrier,
                            )
                    )
                }) else {
                    if emit_diag {
                        self.push_diag(BodyDiag::WithEffectTypeUnsatisfied {
                            primary: span.clone(),
                            key: key_path,
                            expected: pattern.carrier,
                            given: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Type(pattern),
                        reason: BarrierReason::InvalidExplicitTypeKey {
                            span: span.clone(),
                            key_path,
                        },
                    }));
                };
                Ok((
                    EffectWitness {
                        key,
                        provider,
                        transport,
                    },
                    EffectCommitPlan {
                        key_match: Some(KeyMatchCommit::QueryToType {
                            query: pattern,
                            actual,
                        }),
                        trait_solutions: SmallVec::new(),
                        provider_resolution,
                        extra_unifications: SmallVec::new(),
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
                    let trait_req = TraitInstId::new(
                        self.db,
                        pattern.def,
                        std::iter::once(provider.ty)
                            .chain(pattern.args_no_self.iter().copied())
                            .collect::<Vec<_>>(),
                        pattern
                            .assoc_bindings
                            .iter()
                            .copied()
                            .collect::<IndexMap<_, _>>(),
                    );
                    if emit_diag {
                        self.push_diag(BodyDiag::WithEffectTraitUnsatisfied {
                            primary: span.clone(),
                            key: key_path,
                            trait_req,
                            given: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Trait(pattern),
                        reason: BarrierReason::InvalidExplicitTraitKey {
                            span: span.clone(),
                            key_path,
                        },
                    }));
                }
                if provider.ty.has_var(self.db) {
                    if emit_diag {
                        self.push_diag(BodyDiag::TypeAnnotationNeeded {
                            span: span.clone(),
                            ty: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Trait(pattern),
                        reason: BarrierReason::UnstableExplicitKeyedProvider {
                            span: span.clone(),
                            key_path,
                        },
                    }));
                }
                let mut table = UnificationTable::new(self.db);
                let (instantiated, slot_bindings) = instantiate_trait_pattern_in_with_bindings(
                    self.db,
                    &mut table,
                    pattern.clone(),
                );
                let args = std::iter::once(provider.ty)
                    .chain(instantiated.args(self.db).iter().skip(1).copied())
                    .collect::<Vec<_>>();
                let trait_goal = TraitInstId::new(
                    self.db,
                    instantiated.def(self.db),
                    args,
                    instantiated.assoc_type_bindings(self.db).clone(),
                );
                let GoalSatisfiability::Satisfied(solution) =
                    self.trait_effect_goal_satisfiability_in_scope(scope, assumptions, trait_goal)
                else {
                    if emit_diag {
                        self.push_diag(BodyDiag::WithEffectTraitUnsatisfied {
                            primary: span.clone(),
                            key: key_path,
                            trait_req: trait_goal,
                            given: provider.ty,
                        });
                    }
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Trait(pattern),
                        reason: BarrierReason::InvalidExplicitTraitKey {
                            span: span.clone(),
                            key_path,
                        },
                    }));
                };
                let canonical_goal = Canonicalized::new(self.db, trait_goal);
                let solved = canonical_goal.extract_solution(&mut table, solution);
                let solved_inst = solved
                    .implementor
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
                    return Err(Box::new(EffectBarrier {
                        pattern: EffectPatternKey::Trait(pattern),
                        reason: BarrierReason::InvalidExplicitTraitKey { span, key_path },
                    }));
                };
                Ok((
                    EffectWitness {
                        key,
                        provider,
                        transport: WitnessTransport::ByValue,
                    },
                    EffectCommitPlan {
                        key_match: None,
                        trait_solutions: SmallVec::new(),
                        provider_resolution: None,
                        extra_unifications: SmallVec::new(),
                    },
                ))
            }
        }
    }

    fn specialize_type_pattern_key(
        &mut self,
        pattern: TypePatternKey<'db>,
        actual: TyId<'db>,
    ) -> Option<TyId<'db>> {
        let key_match = KeyMatchCommit::QueryToType {
            query: pattern,
            actual,
        };
        apply_key_match_commit(self, key_match).then(|| self.table.fold_ty(self.db, actual))
    }

    fn barrier_reason_for_pattern(
        &self,
        key_path: PathId<'db>,
        key: StoredEffectKey<'db>,
        span: DynLazySpan<'db>,
    ) -> BarrierReason<'db> {
        match key {
            StoredEffectKey::Type(_) => BarrierReason::InvalidExplicitTypeKey { span, key_path },
            StoredEffectKey::Trait(_) => BarrierReason::InvalidExplicitTraitKey { span, key_path },
        }
    }

    fn insert_effect_barrier(&mut self, barrier: EffectBarrier<'db>) {
        let Some(barrier) = self.refine_effect_barrier(barrier) else {
            return;
        };
        self.env
            .effect_env_mut()
            .insert_barrier(barrier.pattern.clone().family(), barrier);
    }

    fn refine_effect_barrier(&self, barrier: EffectBarrier<'db>) -> Option<EffectBarrier<'db>> {
        if self.barrier_pattern_is_precise(&barrier.pattern) {
            return Some(barrier);
        }

        // Exact invalid keyed barriers participate only when precise; otherwise we downgrade to
        // a same-family conservative barrier so invalid explicit keyed bindings still shadow outer
        // providers conservatively.
        let key_path = match &barrier.reason {
            BarrierReason::InvalidExplicitTypeKey { key_path, .. }
            | BarrierReason::InvalidExplicitTraitKey { key_path, .. }
            | BarrierReason::UnstableExplicitKeyedProvider { key_path, .. } => *key_path,
        };
        let pattern = build_conservative_same_family_barrier_pattern_in_scope(
            self.db,
            self.env.scope(),
            self.env.assumptions(),
            key_path,
        )?;
        Some(EffectBarrier {
            pattern,
            reason: barrier.reason,
        })
    }

    fn barrier_pattern_is_precise(&self, pattern: &EffectPatternKey<'db>) -> bool {
        !query_contains_unresolved_inference(self.db, pattern)
            && !contains_projection_or_invalid_query_state(self.db, pattern)
    }

    fn specialize_same_trait_method_inst(
        &self,
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
            Some(ItemKind::Trait(trait_)) => Some(TraitInstId::new(
                self.db,
                trait_,
                trait_.params(self.db).to_vec(),
                IndexMap::new(),
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

        let mut args = inst_args.to_vec();
        args[1..].clone_from_slice(&enclosing_args[1..]);
        TraitInstId::new(
            self.db,
            inst.def(self.db),
            args,
            enclosing_inst.assoc_type_bindings(self.db).clone(),
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
        if receiver_prop.ty.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }

        let receiver_tys = self.method_receiver_tys(*receiver, &receiver_prop);
        let method_assumptions = self.env.assumptions();

        let mut selected_receiver_ty = receiver_tys[0];
        let mut canonical_r_ty = Canonicalized::new(self.db, selected_receiver_ty);
        let mut candidate = select_method_candidate(
            self.db,
            &canonical_r_ty,
            method_name,
            self.env.scope(),
            method_assumptions,
            None,
        );
        if matches!(
            candidate,
            Err(MethodSelectionError::NotFound | MethodSelectionError::ReceiverTypeMustBeKnown)
        ) {
            for &receiver_ty in receiver_tys.iter().skip(1) {
                let fallback_canonical = Canonicalized::new(self.db, receiver_ty);
                let fallback = select_method_candidate(
                    self.db,
                    &fallback_canonical,
                    method_name,
                    self.env.scope(),
                    method_assumptions,
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
            Err(err) => {
                match err {
                    MethodSelectionError::AmbiguousTraitMethod(ambiguous) => {
                        // Defer resolution using return-type constraints
                        let ret_ty = self.fresh_ty();
                        let typed = ExprProp::new(ret_ty, true);
                        self.env.type_expr(expr, typed.clone());
                        // Still type-check argument expressions so they have types and can
                        // participate in later constraint solving.
                        for arg in args.iter() {
                            self.check_expr_unknown(arg.expr);
                        }
                        let candidates = ambiguous
                            .candidates
                            .into_iter()
                            .map(|candidate| {
                                let inst = canonical_r_ty
                                    .extract_solution(&mut self.table, candidate.cand.inst);
                                let inst =
                                    self.specialize_same_trait_method_inst(method_name, inst);
                                super::env::PendingMethodCandidate {
                                    inst,
                                    method: candidate.cand.method,
                                    needs_confirmation: candidate.needs_confirmation,
                                }
                            })
                            .collect();

                        self.env.register_pending_method(super::env::PendingMethod {
                            expr,
                            recv_ty: selected_receiver_ty,
                            method_name,
                            candidates,
                            span: call_span.method_name().into(),
                        });
                        return typed;
                    }
                    _ => {
                        let diag = body_diag_from_method_selection_err(
                            self.db,
                            err,
                            Spanned::new(selected_receiver_ty, receiver.span(self.body()).into()),
                            Spanned::new(method_name, call_span.method_name().into()),
                        );
                        self.push_diag(diag);
                        return ExprProp::invalid(self.db);
                    }
                }
            }
        };

        let (func_ty, trait_inst) = match candidate {
            MethodCandidate::InherentMethod(cand) => (
                self.extract_inherent_method_to_term(&canonical_r_ty, cand, selected_receiver_ty),
                None,
            ),

            MethodCandidate::TraitMethod(cand) => {
                let inst = canonical_r_ty.extract_solution(&mut self.table, cand.inst);
                let inst = self.specialize_same_trait_method_inst(method_name, inst);
                let func_ty =
                    self.instantiate_trait_method_to_term(cand.method, selected_receiver_ty, inst);
                (func_ty, Some(inst))
            }

            MethodCandidate::NeedsConfirmation(cand) => {
                let inst = canonical_r_ty.extract_solution(&mut self.table, cand.inst);
                let inst = self.specialize_same_trait_method_inst(method_name, inst);
                self.env.register_trait_obligation(TraitObligation {
                    goal: inst,
                    origin: TraitObligationOrigin::GenericConfirmation,
                    span: call_span.clone().into(),
                });
                let func_ty =
                    self.instantiate_trait_method_to_term(cand.method, selected_receiver_ty, inst);
                (func_ty, Some(inst))
            }
        };

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

        if let Some(kind) = self
            .code_region_intrinsic_kind(callable.callable_def())
            .or_else(|| self.code_region_method_kind(selected_receiver_ty, method_name))
            && args.len() == 1
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
            false,
        );

        // Check required effects for the method call
        self.check_callable_effects(expr, &mut callable);

        let ret_ty = callable.ret_ty(self.db);
        let normalized_ret_ty = self.normalize_ty(ret_ty);
        if let Some(kind) = self.const_intrinsic_kind(callable.callable_def()) {
            self.env.register_const_intrinsic(expr, callable, kind);
        } else {
            self.env.register_semantic_call(expr, callable);
        }
        ExprProp::new(normalized_ret_ty, true)
    }

    fn method_receiver_tys(
        &self,
        _receiver: ExprId,
        receiver_prop: &ExprProp<'db>,
    ) -> Vec<TyId<'db>> {
        self.capability_fallback_candidates(receiver_prop.ty)
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
        let unify_generic_args = |tc: &mut Self, callable: &mut Callable<'db>| {
            callable.unify_generic_args(tc, generic_args, generic_args_span.clone())
        };

        let res = if path.is_bare_ident(self.db) {
            let ident_span: DynLazySpan<'db> = path_expr_span.clone().into();
            resolve_ident_expr(self.db, &self.env, path, ident_span)
        } else {
            match self.resolve_path(path, true, path_span.clone()) {
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

        match res {
            ResolvedPathInBody::Binding(binding) => {
                let ty = self
                    .env
                    .lookup_binding_ty(&binding)
                    .fold_with(self.db, &mut self.table);
                let ty = self.normalize_ty(ty);
                let mut is_mut = binding.is_mut();
                if let Some((cap, _)) = ty.as_capability(self.db) {
                    is_mut = match cap {
                        CapabilityKind::Mut => true,
                        CapabilityKind::Ref => false,
                        CapabilityKind::View => binding.is_mut(),
                    };
                }
                ExprProp {
                    ty,
                    is_mut,
                    binding: Some(binding),
                    borrow_provider: self.concrete_borrow_provider_for_binding(binding),
                    path_read_semantics: None,
                }
            }
            ResolvedPathInBody::NewBinding(ident) => {
                let diag = BodyDiag::UndefinedVariable(path_expr_span.into(), ident);
                self.push_diag(diag);

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
                    } else {
                        let diag = if ty.is_struct(self.db) {
                            let record_like = RecordLike::from_ty(ty);
                            BodyDiag::unit_variant_expected(
                                self.db,
                                path_expr_span.clone().into(),
                                record_like,
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
                    let mut callable =
                        Callable::new(self.db, ty, expr.span(self.body()).into(), None)
                            .expect("function item path should resolve to callable");
                    if !unify_generic_args(self, &mut callable) {
                        return ExprProp::invalid(self.db);
                    }

                    ExprProp::new(self.instantiate_to_term(callable.ty(self.db)), true)
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
                            let ty = variant.constructor_func_ty(self.db).unwrap();
                            self.instantiate_to_term(ty)
                        }
                        VariantKind::Record(_) => {
                            let record_like = RecordLike::from_variant(variant);
                            let diag = BodyDiag::unit_variant_expected(
                                self.db,
                                expr.span(self.body()).into(),
                                record_like,
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
                            self.extract_inherent_method_to_term(
                                &canonical_r_ty,
                                cand,
                                receiver_ty,
                            ),
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
                                )
                            } else {
                                inst
                            };
                            if matches!(candidate, MethodCandidate::NeedsConfirmation(_)) {
                                self.env.register_trait_obligation(TraitObligation {
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
                    self.env.register_callable(expr, callable);
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

                        let assoc_type_bindings: IndexMap<IdentId<'db>, TyId<'db>> = trait_inst
                            .assoc_type_bindings(self.db)
                            .iter()
                            .map(|(name, ty)| (*name, ty.fold_with(self.db, &mut folder)))
                            .collect();

                        TraitInstId::new(
                            self.db,
                            trait_inst.def(self.db),
                            args,
                            assoc_type_bindings,
                        )
                    } else {
                        trait_inst
                    };

                    self.env.register_trait_obligation(TraitObligation {
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
                    self.env.register_callable(expr, callable);
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
                    );

                    if !super::trait_const_goal_has_foreign_params(self.db, inst, self.env.scope())
                        && let GoalSatisfiability::UnSat(_) = is_goal_satisfiable(
                            self.db,
                            TraitSolveCx::new(self.db, self.env.scope())
                                .with_assumptions(self.env.assumptions()),
                            inst,
                        )
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
                        ConstRef::TraitConst(AssocConstUse::new(
                            self.env.scope(),
                            self.env.assumptions(),
                            inst,
                            name,
                        )),
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
                PathRes::Mod(scope) => {
                    let diag = BodyDiag::NotValue {
                        primary: path_expr_span.clone().into(),
                        given: Either::Left(scope.item()),
                    };
                    self.push_diag(diag);
                    ExprProp::invalid(self.db)
                }
                PathRes::FuncParam(..) => {
                    unreachable!("func params should be resolved as bindings")
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
        let reso = match self.resolve_path(*path, true, path_span.clone()) {
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
                // before checking fields. This is important when record fields
                // depend on generic parameters (e.g. via associated types).
                let snapshot = self.snapshot_state();
                if self.table.unify(ty, expected).is_ok() {
                    self.commit_state(snapshot);
                } else {
                    self.rollback_state(snapshot);
                }
                let ty = ty.fold_with(self.db, &mut self.table);

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

            PathRes::Func(ty) | PathRes::Const(_, ty) | PathRes::TraitConst(ty, ..) => {
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
                let ty = variant.ty;
                let snapshot = self.snapshot_state();
                if self.table.unify(ty, expected).is_ok() {
                    self.commit_state(snapshot);
                } else {
                    self.rollback_state(snapshot);
                }
                let ty = ty.fold_with(self.db, &mut self.table);

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

        let mut rec_checker = RecordInitChecker::new(self, record_like);

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

            let prop = rec_checker.tc.check_expr(field.expr, expected);
            rec_checker
                .tc
                .record_implicit_move_for_owned_expr(field.expr, prop.ty);
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
        let lhs_ty = typed_lhs.ty;
        let lhs_place_ty = lhs_ty
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or(lhs_ty);
        // let lhs_ty = normalize_ty(self.db, lhs_ty, self.env.scope(), self.env.assumptions());

        let (ty_base, ty_args) = lhs_place_ty.decompose_ty_app(self.db);

        if ty_base.has_invalid(self.db) {
            return ExprProp::invalid(self.db);
        }
        let ty_base = lhs_place_ty;

        if ty_base.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.span(self.body()).into());
            self.push_diag(diag);
            return ExprProp::invalid(self.db);
        }

        match field {
            FieldIndex::Ident(label) => {
                let record_like = RecordLike::from_ty(lhs_place_ty);
                if let Some(field_ty) = record_like.record_field_ty(self.db, *label) {
                    if let Some(scope) = record_like.record_field_scope(self.db, *label)
                        && !is_scope_visible_from(self.db, scope, self.env.scope())
                    {
                        // Check the visibility of the field.
                        let diag = PathResDiag::Invisible(
                            expr.span(self.body()).into_field_expr().accessor().into(),
                            *label,
                            scope.name_span(self.db),
                        );

                        self.push_diag(diag);
                        return ExprProp::invalid(self.db);
                    }
                    if let Some(field_index) =
                        resolve_place_field_index(self.db, lhs_place_ty, *field)
                    {
                        self.env.register_resolved_field_index(expr, field_index);
                    }
                    return ExprProp::new(field_ty, typed_lhs.is_mut);
                }
            }

            FieldIndex::Index(i) => {
                let arg_len = ty_args.len().into();
                if ty_base.is_tuple(self.db) && i.data(self.db) < &arg_len {
                    let i: usize = i.data(self.db).try_into().unwrap();
                    if let Ok(field_index) = u16::try_from(i) {
                        self.env.register_resolved_field_index(expr, field_index);
                    }
                    let ty = ty_args[i];
                    return ExprProp::new(ty, typed_lhs.is_mut);
                }
            }
        };

        let diag = BodyDiag::AccessedFieldNotFound {
            primary: expr.span(self.body()).into(),
            given_ty: lhs_place_ty,
            index: *field,
        };
        self.push_diag(diag);

        ExprProp::invalid(self.db)
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
            let prop = self.check_expr(*elem, *elem_ty);
            self.record_implicit_move_for_owned_expr(*elem, prop.ty);
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
            let prop = self.check_expr(*elem, expected_elem_ty);
            expected_elem_ty = prop.ty;
            self.record_implicit_move_for_owned_expr(*elem, expected_elem_ty);
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
                && !matches!(
                    const_ty.data(self.db),
                    ConstTyData::Evaluated(EvaluatedConstTy::LitInt(_), _)
                        | ConstTyData::TyParam(..)
                )
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

        match else_ {
            Some(else_) => {
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
                let borrow_provider = self.merge_concrete_borrow_providers(
                    then.span(self.body()).into(),
                    then_prop.borrow_provider,
                    else_.span(self.body()).into(),
                    else_prop.borrow_provider,
                );
                ExprProp {
                    ty: else_prop.ty,
                    is_mut: true,
                    binding: None,
                    borrow_provider,
                    path_read_semantics: None,
                }
            }

            None => {
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
        let scrutinee_ty = self.check_expr(*scrutinee, scrutinee_ty).ty;
        let (scrutinee_pat_ty, mode) = self.destructure_source_mode(scrutinee_ty);

        let Partial::Present(arms) = arms else {
            return ExprProp::invalid(self.db);
        };

        let mut match_ty = expected;
        let mut first_provider: Option<(DynLazySpan<'db>, super::ProviderAddressSpace)> = None;
        let mut provider_unknown = false;
        let mut provider_conflict = false;
        let mut arm_statuses = Vec::with_capacity(arms.len());

        for arm in arms.iter() {
            let pat_result = self.check_pat(arm.pat, scrutinee_pat_ty);
            if let super::PatternDestructureMode::Borrow(kind) = mode {
                self.retype_pattern_bindings_for_borrow(arm.pat, kind);
            }
            arm_statuses.push(pat_result.analysis);

            self.env.enter_scope(arm.body);
            self.env.flush_pending_bindings();
            let arm_prop = if result_discarded {
                self.check_expr_with_discarded_result(arm.body, match_ty)
            } else {
                self.check_expr(arm.body, match_ty)
            };
            match_ty = arm_prop.ty;
            self.env.leave_scope();

            if arm_prop.ty.as_capability(self.db).is_some() {
                if let Some(provider) = arm_prop.borrow_provider {
                    if let Some((ref span, previous)) = first_provider {
                        provider_conflict |= self
                            .merge_concrete_borrow_providers(
                                span.clone(),
                                Some(previous),
                                arm.body.span(self.body()).into(),
                                Some(provider),
                            )
                            .is_none();
                    } else {
                        first_provider = Some((arm.body.span(self.body()).into(), provider));
                    }
                } else {
                    provider_unknown = true;
                }
            }
        }

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
            let reachability = crate::analysis::ty::pattern_analysis::check_reachability(
                self.db,
                &pattern_store,
                &roots,
            );

            for (i, is_reachable) in reachability.iter().enumerate() {
                if *is_reachable {
                    continue;
                }
                let diag = BodyDiag::UnreachablePattern {
                    primary: arms[i].pat.span(self.body()).into(),
                };
                self.push_diag(diag);
            }

            if let Err(missing_patterns) =
                crate::analysis::ty::pattern_analysis::check_exhaustiveness(
                    self.db,
                    &pattern_store,
                    &roots,
                    scrutinee_pat_ty,
                )
            {
                let diag = BodyDiag::NonExhaustiveMatch {
                    primary: expr.span(self.body()).into(),
                    scrutinee_ty: scrutinee_pat_ty,
                    missing_patterns,
                };
                self.push_diag(diag);
            }
        }

        ExprProp {
            ty: match_ty,
            is_mut: true,
            binding: None,
            borrow_provider: if provider_unknown || provider_conflict {
                None
            } else {
                first_provider.map(|(_, provider)| provider)
            },
            path_read_semantics: None,
        }
    }

    fn check_assign(&mut self, _expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::Assign(lhs, rhs) = expr_data else {
            unreachable!()
        };

        let typed_lhs = self.check_expr_unknown(*lhs);
        let lhs_ty = typed_lhs
            .ty
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or(typed_lhs.ty);
        let mut rhs_prop = self.check_expr_unknown(*rhs);
        if let Some(coerced) =
            self.try_coerce_capability_for_expr_to_expected(*rhs, rhs_prop.ty, lhs_ty)
        {
            rhs_prop.ty = coerced;
        }
        rhs_prop.ty = self.unify_ty(Typeable::Expr(*rhs, rhs_prop.clone()), rhs_prop.ty, lhs_ty);

        let lhs_status = self.check_assign_lhs(*lhs, &typed_lhs);
        self.record_implicit_move_for_owned_expr(*rhs, rhs_prop.ty);

        if lhs_status == AssignLhsStatus::Assignable
            && typed_lhs.ty.as_capability(self.db).is_some()
            && let Some(place) = self.env.expr_place(*lhs)
            && place.projections.is_empty()
        {
            let PlaceBase::Binding(binding) = place.base;
            self.merge_concrete_borrow_providers(
                binding.def_span(&self.env),
                self.concrete_borrow_provider_for_binding(binding),
                rhs.span(self.body()).into(),
                rhs_prop.borrow_provider,
            );
        }

        ExprProp::new(TyId::unit(self.db), true)
    }

    fn check_aug_assign(&mut self, expr: ExprId, expr_data: &Expr<'db>) -> ExprProp<'db> {
        let Expr::AugAssign(lhs, rhs, op) = expr_data else {
            unreachable!()
        };

        let unit = ExprProp::new(TyId::unit(self.db), true);

        let typed_lhs = self.check_expr_unknown(*lhs);
        let lhs_ty = typed_lhs.ty;
        let lhs_place_ty = lhs_ty
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or(lhs_ty);
        if lhs_ty.has_invalid(self.db) {
            return unit;
        }
        if self.check_assign_lhs(*lhs, &typed_lhs) == AssignLhsStatus::NonAssignable {
            return unit;
        }

        // Avoid 'type must be known' diagnostics for unknown integer ty
        if lhs_place_ty.is_integral_var(self.db) {
            self.check_expr(*rhs, lhs_place_ty);
            return unit;
        }

        let lhs_base_ty = lhs_place_ty.base_ty(self.db);
        if lhs_base_ty.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(lhs.span(self.body()).into());
            self.push_diag(diag);
            return unit;
        }

        // `x += y` is semantically defined by the `*Assign` traits. Primitive
        // integer fast paths are introduced later during MIR lowering without
        // changing which trait method the source program resolves to here.
        self.check_ops_trait(expr, lhs_place_ty, &AugAssignOp(*op), Some(*rhs));

        // Return unit ty even if trait resolution fails
        unit
    }

    /// Resolve a core::ops trait method for an operator on a given LHS type and
    /// optionally check the RHS against the inferred method parameter type.
    /// Returns the fully-instantiated function type and concrete trait instance.
    fn check_ops_trait(
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

        let lhs_candidates = self.capability_fallback_candidates(lhs_ty);
        let method_assumptions = self.env.assumptions();

        let mut selected_lhs_ty = lhs_candidates[0];
        let mut c_lhs_ty = Canonicalized::new(self.db, selected_lhs_ty);
        let mut method_candidate = select_method_candidate(
            self.db,
            &c_lhs_ty,
            op.trait_method(self.db),
            self.env.scope(),
            method_assumptions,
            Some(trait_def),
        );
        if matches!(
            method_candidate,
            Err(MethodSelectionError::NotFound | MethodSelectionError::ReceiverTypeMustBeKnown)
        ) {
            for &candidate_ty in lhs_candidates.iter().skip(1) {
                let c_candidate_ty = Canonicalized::new(self.db, candidate_ty);
                let fallback = select_method_candidate(
                    self.db,
                    &c_candidate_ty,
                    op.trait_method(self.db),
                    self.env.scope(),
                    method_assumptions,
                    Some(trait_def),
                );
                if fallback.is_ok() || !matches!(fallback, Err(MethodSelectionError::NotFound)) {
                    selected_lhs_ty = candidate_ty;
                    c_lhs_ty = c_candidate_ty;
                    method_candidate = fallback;
                    break;
                }
            }
        }

        let (method, inst) = match method_candidate {
            Ok(MethodCandidate::InherentMethod(_)) => unreachable!(),
            Ok(
                res @ (MethodCandidate::TraitMethod(cand)
                | MethodCandidate::NeedsConfirmation(cand)),
            ) => {
                let inst = c_lhs_ty.extract_solution(&mut self.table, cand.inst);
                if matches!(res, MethodCandidate::NeedsConfirmation(_)) {
                    self.env.register_trait_obligation(TraitObligation {
                        goal: inst,
                        origin: TraitObligationOrigin::GenericConfirmation,
                        span: expr.span(self.body()).into(),
                    });
                }

                let func_ty =
                    self.instantiate_trait_method_to_term(cand.method, selected_lhs_ty, inst);

                if let Some(rhs_expr) = rhs_expr
                    && let Some(expected_rhs) = self.instantiated_ops_rhs_ty(func_ty, inst)
                {
                    self.check_or_constrain_expr_to_expected(rhs_expr, expected_rhs);
                }

                (func_ty, inst)
            }
            Err(MethodSelectionError::AmbiguousTraitMethod(ambiguous)) => {
                let Some(rhs_expr) = rhs_expr else {
                    unreachable!("unary core::ops ambiguity");
                };

                let rhs = self.check_expr_unknown(rhs_expr);
                if rhs.ty.has_invalid(self.db) {
                    return ExprProp::invalid(self.db);
                }

                let mut viable = Vec::new();
                for candidate in ambiguous.candidates.iter().copied() {
                    let snapshot = self.snapshot_state();
                    let unifies = (|| {
                        let inst = c_lhs_ty.extract_solution(&mut self.table, candidate.cand.inst);
                        let func_ty = super::try_instantiate_trait_method(
                            self.db,
                            candidate.cand.method,
                            &mut self.table,
                            selected_lhs_ty,
                            inst,
                        )
                        .ok()?;
                        let func_ty = self.table.instantiate_to_term(func_ty);
                        let expected_rhs = self.instantiated_ops_rhs_ty(func_ty, inst)?;
                        let rhs_ty = self
                            .try_coerce_capability_for_expr_to_expected(
                                rhs_expr,
                                rhs.ty,
                                expected_rhs,
                            )
                            .unwrap_or(rhs.ty);
                        self.table.unify(rhs_ty, expected_rhs).ok()
                    })()
                    .is_some();
                    self.rollback_state(snapshot);
                    if unifies {
                        viable.push(candidate);
                    }
                }

                match viable.len() {
                    0 => {
                        let diag = BodyDiag::ops_trait_not_implemented(
                            self.db,
                            expr.span(self.body()).into(),
                            lhs_ty,
                            op,
                        );
                        self.push_diag(diag);
                        return ExprProp::invalid(self.db);
                    }
                    1 => {
                        let candidate = viable.pop().unwrap();
                        let inst = c_lhs_ty.extract_solution(&mut self.table, candidate.cand.inst);
                        if candidate.needs_confirmation {
                            self.env.register_trait_obligation(TraitObligation {
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
                            let rhs_ty = self
                                .try_coerce_capability_for_expr_to_expected(
                                    rhs_expr,
                                    rhs.ty,
                                    expected_rhs,
                                )
                                .unwrap_or(rhs.ty);
                            self.unify_ty(
                                Typeable::Expr(rhs_expr, rhs.clone()),
                                rhs_ty,
                                expected_rhs,
                            );
                        }
                        (func_ty, inst)
                    }
                    _ => {
                        let cands = viable
                            .into_iter()
                            .map(|candidate| {
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
            }
            Err(MethodSelectionError::NotFound) => {
                let diag = BodyDiag::ops_trait_not_implemented(
                    self.db,
                    expr.span(self.body()).into(),
                    lhs_ty,
                    op,
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
        };

        let callable = Callable::new(self.db, method, expr.span(self.body()).into(), Some(inst))
            .expect("failed to create Callable for core::ops trait method");

        let ret_ty = self.normalize_ty(callable.ret_ty(self.db));
        self.env.register_semantic_call(expr, callable);
        ExprProp::new(ret_ty, true)
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
        let mut expected_rhs = func_def
            .arg_tys(self.db)
            .get(1)?
            .instantiate(self.db, gen_args);
        let mut subst = AssocTySubst::new(inst);
        expected_rhs = self.normalize_ty(expected_rhs.fold_with(self.db, &mut subst));
        Some(expected_rhs)
    }

    fn check_or_constrain_expr_to_expected(&mut self, expr: ExprId, expected: TyId<'db>) {
        let Some(prop) = self.env.typed_expr(expr) else {
            self.check_expr(expr, expected);
            return;
        };
        let actual = self
            .try_coerce_capability_for_expr_to_expected(expr, prop.ty, expected)
            .unwrap_or(prop.ty);
        self.unify_ty(Typeable::Expr(expr, prop), actual, expected);
    }

    fn check_assign_lhs(&mut self, lhs: ExprId, typed_lhs: &ExprProp<'db>) -> AssignLhsStatus {
        if !self.is_assignable_expr(lhs) || self.env.expr_place(lhs).is_none() {
            if !typed_lhs.ty.has_invalid(self.db) {
                let diag = BodyDiag::NonAssignableExpr(lhs.span(self.body()).into());
                self.push_diag(diag);
            }

            return AssignLhsStatus::NonAssignable;
        }

        if !typed_lhs.is_mut {
            let binding = self.find_base_binding(lhs);
            let diag = match binding {
                Some(binding) => {
                    let (ident, def_span) =
                        (binding.binding_name(&self.env), binding.def_span(&self.env));

                    BodyDiag::ImmutableAssignment {
                        primary: lhs.span(self.body()).into(),
                        binding: Some((ident, def_span)),
                    }
                }

                None => BodyDiag::ImmutableAssignment {
                    primary: lhs.span(self.body()).into(),
                    binding: None,
                },
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
            Expr::Path(..) => self.env.typed_expr(expr)?.binding,
            _ => None,
        }
    }

    /// Returns `true`` if the expression can be used as an left hand side of an
    /// assignment.
    /// This method doesn't take mutability into account.
    fn is_assignable_expr(&self, expr: ExprId) -> bool {
        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return false;
        };

        match expr_data {
            Expr::Path(..) | Expr::Field(..) => true,
            Expr::Bin(_, _, op) if *op == BinOp::Index => true,
            _ => false,
        }
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
) -> ResolvedPathInBody<'db> {
    let ident = path.ident(db).unwrap();

    let resolve_bucket = |bucket: &NameResBucket<'db>, scope| {
        // First, surface any ambiguity/conflict in the bucket as a dedicated
        // name-resolution diagnostic instead of silently degrading to
        // "undefined variable".
        for (_, err) in bucket.errors() {
            match err {
                NameResolutionError::Ambiguous(cands) => {
                    let mut cand_spans = Vec::new();
                    for name in cands.iter() {
                        if let Some(span) = name.kind.name_span(db) {
                            let from_implicit = name
                                .derivation
                                .use_stmt()
                                .map(|use_| use_.is_synthetic_use(db))
                                .unwrap_or(false);
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
        let Ok(reso) = resolve_name_res(db, res, None, path, scope, env.assumptions()) else {
            return ResolvedPathInBody::Invalid;
        };
        ResolvedPathInBody::Reso(reso)
    };

    let mut current_idx = env.current_block_idx();

    loop {
        let block = env.get_block(current_idx);
        if let Some(binding) = block.lookup_var(ident) {
            return ResolvedPathInBody::Binding(binding);
        }

        let scope = block.scope;
        let directive = QueryDirective::for_scope(db, scope).disallow_lex();
        let query = EarlyNameQueryId::new(db, ident, scope, directive);
        let bucket = resolve_query(db, query);

        let resolved = resolve_bucket(bucket, scope);
        if matches!(resolved, ResolvedPathInBody::Invalid) {
            if current_idx == 0 {
                break;
            } else {
                current_idx -= 1;
            }
        } else {
            return resolved;
        }
    }

    let body_scope = env.body().scope();
    let directive = QueryDirective::for_scope(db, body_scope);
    let query = EarlyNameQueryId::new(db, ident, body_scope, directive);
    let bucket = resolve_query(db, query);
    match resolve_bucket(bucket, env.scope()) {
        ResolvedPathInBody::Invalid => ResolvedPathInBody::NewBinding(ident),
        r => r,
    }
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

    fn triple(&self) -> [&str; 3];
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
