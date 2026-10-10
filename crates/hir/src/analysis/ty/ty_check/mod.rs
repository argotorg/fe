mod callable;
mod const_requirements;
pub(crate) use const_requirements::check_declared_type_requirements;
pub use const_requirements::{EvaluationStop, RequirementFailure};
mod contract;
mod effect_env;
pub(crate) mod env;
mod expr;
mod owner;
mod pat;
mod path;
mod stmt;

pub(crate) use self::contract::eval_msg_variant_selector;
pub use self::contract::{
    ResolvedRecvVariant, VariantResError, check_contract_field_address_spaces,
    check_contract_immutable_fields_initialized, check_contract_init_body,
    check_contract_recv_arm_body, check_contract_recv_block, check_contract_recv_blocks,
    resolve_variant_bare, resolve_variant_in_msg,
};
pub use self::path::RecordLike;
use crate::analysis::name_resolution::ResolvedVariant;
pub use crate::analysis::ty::ProviderAddressSpace;
use crate::analysis::ty::const_check::const_body_language_failure;
use crate::analysis::ty::corelib::resolve_lib_type_path;
use crate::analysis::ty::fold::{TyFoldable, TyFolder};
use crate::analysis::ty::method_table::ProbedMethod;
use crate::analysis::ty::provider::{ProviderKind, provider_semantics};
use crate::analysis::ty::trait_lower::lower_impl_trait;
use crate::analysis::ty::trait_resolution::constraint::{
    PredicateSource, collect_func_decl_constraint_pairs, collect_func_decl_constraints,
};
use crate::analysis::ty::visitor::{TyVisitable, TyVisitor, walk_const_ty, walk_ty};
use crate::hir_def::{CallableDef, ConstGenericArgValue, ImplTrait, Trait};
use crate::{
    hir_def::{
        BinOp, Body, CondId, Const, Contract, ContractRecvArm, Expr, ExprId, Func, GenericArg,
        GenericParam, GenericParamOwner, LitKind, ManualContractRootAttr, Partial, Pat, PatId,
        PathId, StaticAssert, StaticAssertComparison, Stmt, StmtId, StringId, TypeId as HirTyId,
        TypeKind, WhereClauseOwner, scope_graph::ScopeId,
    },
    span::{
        DynLazySpan, expr::LazyExprSpan, pat::LazyPatSpan, path::LazyPathSpan, types::LazyTySpan,
    },
    visitor::{Visitor, VisitorCtxt, walk_expr, walk_pat},
};
use callable::{CallGenericArgPhase, CallGenericArgUnifyError, unify_explicit_call_generic_args};
pub use callable::{Callable, EffectProviderProvenance, EffectProviderSpecialization};
use common::indexmap::IndexMap;
use cranelift_entity::{PrimaryMap, SecondaryMap, entity_impl, packed_option::PackedOption};
use ena::unify::InPlace;
use env::TyCheckEnv;
pub use env::{
    EffectParamSite, ExprProp, LocalBinding, ParamSite, PatBindingMode, PathReadSemantics,
};
pub(super) use expr::TraitOps;
use num_traits::ToPrimitive;
pub use owner::BodyOwner;
pub use owner::EffectParamOwner;
use std::sync::Arc;
pub use stmt::ForLoopSeq;

use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;

use crate::analysis::place::{Place, PlaceBase, PlaceProjection, is_pointer_place_expr};

use super::{
    LayoutBundlePath, LayoutBundlePathStep,
    adt_def::ConcreteTypeView,
    assoc_const::{AssocConstUse, InherentConstUse},
    binder::Binder,
    canonical::Canonical,
    diagnostics::{
        BodyDiag, CallConstraintDiagInfo, FuncBodyDiag, RevealedBy, StaticAssertComparisonValues,
        TraitConstraintDiag, TyDiagCollection, TyLowerDiag,
    },
    effects::{EffectKeyKind, ResolvedEffectKey, resolve_effect_key},
    generic_defaults::{GenericDefault, default_assumptions, generic_default},
    layout_holes::merge_equated_layout_holes,
    trait_def::{TraitInstId, resolve_trait_method_instance},
    trait_resolution::{
        CanonicalGoalQuery, GoalSatisfiability, PredicateListId, Selection, TraitSolveCx,
        goal_query_has_no_distinct_solution, is_goal_query_satisfiable, is_goal_satisfiable,
    },
    ty_contains_const_hole,
    ty_def::{
        AssocTy, BorrowKind, CapabilityKind, InvalidCause, Kind, MAX_INLINE_STRING_BYTES,
        StringFallback, TyId, TyParam, TyVar, TyVarSort,
    },
    ty_lower::{
        CallableInputLayoutBackingSource, ParamDomainId, ParamKey, ParamSchemaId, PartialSubst,
        callable_input_layout_backing_index_lengths, callable_input_layout_backing_sources,
        collect_generic_params, layout_param_projection_paths_in_ty, lower_hir_ty,
        lower_hir_ty_deferred, resolve_callable_input_effect_key,
    },
    unify::{InferenceKey, Snapshot, UnificationError, UnificationTable},
};
use crate::analysis::semantic::SemanticCodeRegionRef;
use crate::analysis::semantic::{
    BlockedInfo, ConstDependency, ConstUsePolicy, CtfeConfig, CtfeError, EffectProviderSubst,
    EvalFailure, EvalOutcome, GenericSubst, ImplEnv, RuntimeSizeError, SemConstId, SemConstScalar,
    SemConstValue, SemOrigin, SemanticInstanceKey, const_computation_for_instance,
    describe_const_computation, eval_body_owner_const, get_or_build_semantic_instance,
    reify_runtime_const_for_ty, runtime_size_bytes_with_source,
};
use crate::analysis::ty::ty_def::{TyBase, TyData};
use crate::analysis::ty::{
    const_ty::{
        BodyHoleSite, CallableInputLayoutHoleOrigin, ConstTyData, ConstTyId, HoleAnchor,
        LoweringContext, invalid_cause_from_eval_failure, origin_expr_for_const_eval_diag,
        root_ctfe_error,
    },
    normalize::{normalize_ty, normalize_with_trait_evidence},
    pattern_ir::{
        ConstructorKind, PatternAnalysisStatus, PatternStore, ValidatedPatId, ValidatedPatKind,
    },
    pattern_types::{
        PatternDestructureMode, apply_pattern_borrow_mode, destructure_pattern_source,
    },
    ty_error::{
        collect_hir_ty_diags, collect_ty_lower_errors, diag_from_invalid_cause,
        normalization_limit_diag, path_generic_args, qualified_path_wf_diags,
    },
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        PathRes, PathResError, PathResErrorKind, diagnostics::PathResDiag, resolve_path,
        resolve_path_with_observer_and_minter,
    },
    ty::{
        ty_def::{TyFlags, inference_keys},
        visitor::collect_flags,
    },
};

#[salsa::tracked(return_ref)]
pub fn check_func_body<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    check_body(db, BodyOwner::Func(func))
}

#[salsa::tracked(return_ref)]
pub fn check_const_body<'db>(
    db: &'db dyn HirAnalysisDb,
    const_: Const<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let (mut diags, body) = check_body(db, BodyOwner::Const(const_));
    // An invalid declared type suppresses body/value checking. Diagnose the
    // declaration itself rather than passing an invalid constant to CTFE.
    if let Some(hir_ty) = const_.hir_ty(db) {
        diags.extend(
            super::ty_error::collect_hir_ty_diags(
                db,
                const_.scope(),
                hir_ty,
                const_.span().ty(),
                PredicateListId::empty_list(db),
            )
            .into_iter()
            .map(Into::into),
        );
    }
    (diags, body)
}

#[salsa::tracked(return_ref)]
pub fn check_anon_const_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: TyId<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    check_body(db, BodyOwner::AnonConstBody { body, expected })
}

/// Type-checks the value bodies of an `impl Trait` block's associated consts.
/// Bodies are checked against the trait's declared type for the const
/// (instantiated with the impl's trait args) so a wrong impl-side header
/// doesn't cascade; consts not declared in the trait fall back to their own
/// header type. Header-vs-trait conformance itself is reported by the
/// `ImplTrait` item diagnostics.
#[salsa::tracked(return_ref)]
pub fn check_impl_trait_const_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let generated_origin = match impl_trait.origin(db) {
        crate::span::HirOrigin::Raw(_) => None,
        crate::span::HirOrigin::Desugared(crate::span::DesugaredOrigin::Event(_)) => {
            Some("generated `#[event]` implementation")
        }
        crate::span::HirOrigin::Desugared(crate::span::DesugaredOrigin::Error(_)) => {
            Some("generated `#[error]` implementation")
        }
        crate::span::HirOrigin::Desugared(crate::span::DesugaredOrigin::AbiStruct(_)) => {
            Some("generated `#[abi]` implementation")
        }
        _ => return Vec::new(),
    };
    let Some(implementor) = lower_impl_trait(db, impl_trait) else {
        return Vec::new();
    };
    let trait_hir = implementor.trait_def(db);
    let trait_args = implementor.trait_(db).args(db);
    let policy = if generated_origin.is_some() {
        ConstUsePolicy::RequireValue
    } else {
        // Associated values may retain a contextual selection until use.
        // Reached body failures still report under AllowDependent.
        ConstUsePolicy::AllowDependent
    };

    let mut diags = Vec::new();
    for impl_const in impl_trait.assoc_consts(db) {
        let Some(body) = impl_const.value_body(db) else {
            continue;
        };
        let expected_ty = impl_const
            .name(db)
            .and_then(|name| trait_hir.const_(db, name))
            .and_then(|c| c.ty_binder(db))
            .map(|binder| binder.instantiate(db, trait_args))
            .or_else(|| impl_const.ty(db));
        let Some(expected_ty) = expected_ty else {
            continue;
        };
        if expected_ty.has_invalid(db) {
            continue;
        }
        let body_diags = &check_anon_const_body(db, body, expected_ty).0;
        if generated_origin.is_none() {
            diags.extend(body_diags.iter().cloned());
        }
        if body_diags.is_empty() {
            let context = generated_origin.map(|origin| ConstDiagContext {
                const_name: impl_const.name(db).map_or_else(
                    || "<associated const>".to_string(),
                    |name| name.data(db).clone(),
                ),
                origin: origin.to_string(),
            });
            diags.extend(
                const_body_ctfe_diags_with_context(db, body, expected_ty, policy, context)
                    .into_iter()
                    .filter(|diag| {
                        // ImplTrait::diags_assoc_const_evaluability owns the
                        // dedicated cycle diagnostic for these declarations.
                        !matches!(
                            diag,
                            FuncBodyDiag::Ty(TyDiagCollection::Ty(
                                TyLowerDiag::ConstEvalRecursiveConst(_)
                            ))
                        )
                    }),
            );
        }
    }
    diags
}

/// Type-checks the default value bodies of a trait's associated consts
/// against their declared types. Nothing else checks these: impls that don't
/// override a default only force its evaluation transitively, and a trait
/// with no concrete impl never checks it at all.
#[salsa::tracked(return_ref)]
pub fn check_trait_const_default_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_: Trait<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    if !matches!(trait_.origin(db), crate::span::HirOrigin::Raw(_)) {
        return Vec::new();
    }

    let mut diags = Vec::new();
    for trait_const in trait_.assoc_consts(db) {
        let Some(body) = trait_const.default_body(db) else {
            continue;
        };
        let Some(expected_ty) = trait_const.ty(db) else {
            continue;
        };
        let flags = expected_ty.flags(db);
        if flags.contains(TyFlags::HAS_INVALID) {
            continue;
        }
        let body_diags = &check_anon_const_body(db, body, expected_ty).0;
        if flags.contains(TyFlags::HAS_PARAM) {
            // A declared type mentioning `Self` or generic params (`const C:
            // T = 1`) types the default per instantiation: impl overrides are
            // checked with the impl's bindings, and the value is retyped at
            // use sites. Mismatches against the rigid param are expected
            // here; instantiation-independent errors (unresolved names, bad
            // calls, ...) still must surface.
            diags.extend(
                body_diags
                    .iter()
                    .filter(|diag| !diag_depends_on_param_instantiation(db, diag))
                    .cloned(),
            );
        } else {
            diags.extend(body_diags.iter().cloned());
        }
        if body_diags.is_empty() {
            diags.extend(const_body_ctfe_diags(
                db,
                body,
                expected_ty,
                ConstUsePolicy::AllowDependent,
            ));
        }
    }
    diags
}

/// Checks const bodies in generic defaults at their declarations: a const
/// parameter's default, and every anonymous constant written in a type
/// parameter's default, including one that the lowered default does not hold
/// (`written_type_const_args`). A use may omit or override a default, so
/// applications cannot own these diagnostics.
#[salsa::tracked(return_ref)]
pub fn check_generic_default_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let mut diags = Vec::new();
    for view in owner.params(db) {
        for checked in check_generic_default_body_types(db, owner, view.idx) {
            diags.extend(checked.diagnostics.iter().cloned());
            if checked.ctfe_ready {
                diags.extend(const_body_ctfe_diags(
                    db,
                    checked.body,
                    checked.expected,
                    ConstUsePolicy::AllowDependent,
                ));
            }
        }
    }
    diags
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub(crate) struct DefaultConstBodyCheck<'db> {
    pub(crate) body: Body<'db>,
    pub(crate) expected: TyId<'db>,
    pub(crate) diagnostics: Vec<FuncBodyDiag<'db>>,
    pub(crate) ctfe_ready: bool,
}

/// Name/type checking stays separate from const execution so structural
/// default discovery can remain cycle-free and checked symbolic defaults can
/// retain their concrete-evaluation obligations.
#[salsa::tracked(return_ref)]
pub(crate) fn check_generic_default_body_types<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    param_idx: usize,
) -> Vec<DefaultConstBodyCheck<'db>> {
    let Ok(Some(default)) = generic_default(db, owner, param_idx) else {
        return Vec::new();
    };
    let bodies = match default {
        GenericDefault::Const {
            value: ConstGenericArgValue::Expr(Partial::Present(body)),
            expected,
        } => vec![(*body, expected.instantiate_identity())],
        // The constants written in the default, not only those its lowering
        // holds: an alias can drop one, and a qualifier can use one only to
        // select an impl.
        GenericDefault::Type(_) => {
            let view = owner.param_view(db, param_idx);
            let GenericParam::Type(param) = view.param else {
                return Vec::new();
            };
            let Some(hir_ty) = param.default_ty else {
                return Vec::new();
            };
            const_requirements::written_type_const_args(
                db,
                hir_ty,
                view.span().into_type_param().default_ty(),
                owner.scope(),
                default_assumptions(db, owner),
            )
        }
        GenericDefault::Const { .. } => Vec::new(),
    };
    bodies
        .into_iter()
        .filter(|(_, expected)| !expected.has_invalid(db))
        .map(|(body, expected)| {
            let body_diags = &check_anon_const_body(db, body, expected).0;
            let diagnostics = if expected.has_param(db) {
                body_diags
                    .iter()
                    .filter(|diag| !diag_depends_on_param_instantiation(db, diag))
                    .cloned()
                    .collect()
            } else {
                body_diags.clone()
            };
            DefaultConstBodyCheck {
                body,
                expected,
                diagnostics,
                ctfe_ready: body_diags.is_empty(),
            }
        })
        .collect()
}

/// The anonymous constant bodies that `tys`, lowered with deferred const
/// bodies, still hold, each with the type its position checks it against.
pub(super) fn unevaluated_const_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    tys: &[TyId<'db>],
) -> Vec<(Body<'db>, TyId<'db>)> {
    struct NestedConstBodies<'db> {
        db: &'db dyn HirAnalysisDb,
        seen: FxHashSet<(Body<'db>, TyId<'db>)>,
        bodies: Vec<(Body<'db>, TyId<'db>)>,
    }

    impl<'db> TyVisitor<'db> for NestedConstBodies<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_const_ty(&mut self, const_ty: &ConstTyId<'db>) {
            if let ConstTyData::UnEvaluated {
                body,
                template_ty,
                ty,
                ..
            } = const_ty.data(self.db)
                && let Some(expected) = template_ty.or(*ty)
                && self.seen.insert((*body, expected))
            {
                self.bodies.push((*body, expected));
            }
            walk_const_ty(self, const_ty);
        }
    }

    let mut nested = NestedConstBodies {
        db,
        seen: FxHashSet::default(),
        bodies: Vec::new(),
    };
    for ty in tys {
        ty.visit_with(&mut nested);
    }
    nested.bodies
}

/// Whether a default-body diagnostic could be an artifact of checking
/// against a rigid generic param instead of a per-instantiation type.
fn diag_depends_on_param_instantiation<'db>(
    db: &'db dyn HirAnalysisDb,
    diag: &FuncBodyDiag<'db>,
) -> bool {
    match diag {
        FuncBodyDiag::Body(BodyDiag::TypeMismatch {
            expected, given, ..
        }) => expected.has_param(db) || given.has_param(db),
        FuncBodyDiag::Body(BodyDiag::TypeAnnotationNeeded { .. }) => true,
        _ => false,
    }
}

/// The `constraint_idx`-th bound of `callable_def` as declared, in terms of
/// the callable's own generic parameters.
fn declared_call_bound<'db>(
    db: &'db dyn HirAnalysisDb,
    callable_def: CallableDef<'db>,
    constraint_idx: usize,
) -> Option<TraitInstId<'db>> {
    collect_func_decl_constraints(db, callable_def, true)
        .instantiate_identity()
        .list(db)
        .get(constraint_idx)
        .copied()
}

/// If the failing goal's self type is a capability (a borrowed value) and
/// owning that value would satisfy `primary`, returns the capability type.
/// The failing goal is `unsat`, the unsatisfied sub-goal, or `primary` itself
/// when there is none.
///
/// This is the only claim the explanation makes; it names no impl, since the
/// impl proving the owned goal may differ from the one whose bound failed for
/// the borrowed value (`impl<A: Mark> Foo for W<A>` next to `impl Foo for
/// W<S>`). It must never be wrong:
/// - Either the capability type occurs once in `primary`'s self type, and
///   elsewhere only in positions that follow it, or repeated positions come
///   from one declared type binding containing the capability exactly once.
///   The repeated case must reconstruct the complete bound, mention no other
///   parameter, and leave no occurrence of that exact capability when owned.
///   Other bindings can depend on the borrowed type through separate bounds,
///   so holding them fixed would not prove the owned form of this bound.
/// - `primary` holds with the value owned. The solver re-derives every
///   sub-goal from its declared bounds (including `Self`-dependent defaults
///   and explicit arguments such as `A: Rel<ref S>`).
///
/// A trait argument or associated type binding follows the value when
/// `declared` (the call bound `primary` was instantiated from) writes it in
/// terms of the bound's self type alone: exactly the self type (`X: Eq`, which
/// is `X: Eq<X>`; `Out<Item = X>`), or, when the self type is a generic
/// parameter `X`, a type mentioning no other parameter (`Rel<(X,)>`,
/// `Out<Item = W<X>>`). Such a position is recomputed from its declared type
/// with `X` bound to the owned value, after checking that binding `X` to the
/// borrowed value reproduces the instantiated position.
///
/// One type binding can describe multiple input values. This explanation is
/// about the bound's types, not a claim that owning one argument fixes a call.
fn capability_only_unsat_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    primary: TraitInstId<'db>,
    call_bound: Option<(CallableDef<'db>, usize)>,
    unsat: Option<TraitInstId<'db>>,
) -> Option<TyId<'db>> {
    fn occurrences<'db>(
        db: &'db dyn HirAnalysisDb,
        target: TyId<'db>,
        value: &impl TyVisitable<'db>,
    ) -> usize {
        struct CountTy<'db> {
            db: &'db dyn HirAnalysisDb,
            target: TyId<'db>,
            count: usize,
        }

        impl<'db> TyVisitor<'db> for CountTy<'db> {
            fn db(&self) -> &'db dyn HirAnalysisDb {
                self.db
            }

            fn visit_ty(&mut self, ty: TyId<'db>) {
                if ty == self.target {
                    self.count += 1;
                } else {
                    walk_ty(self, ty);
                }
            }
        }

        let mut counter = CountTy {
            db,
            target,
            count: 0,
        };
        value.visit_with(&mut counter);
        counter.count
    }

    /// Whether `ty` mentions `param` and no other parameter, inference
    /// variable or projection, so it is determined by `param` alone.
    fn determined_by<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, param: TyId<'db>) -> bool {
        struct Determined<'db> {
            db: &'db dyn HirAnalysisDb,
            param: TyId<'db>,
            mentions: bool,
            other: bool,
        }

        impl<'db> TyVisitor<'db> for Determined<'db> {
            fn db(&self) -> &'db dyn HirAnalysisDb {
                self.db
            }

            fn visit_ty(&mut self, ty: TyId<'db>) {
                if ty == self.param {
                    self.mentions = true;
                } else if matches!(ty.data(self.db), TyData::QualifiedTy(_)) {
                    self.other = true;
                } else {
                    walk_ty(self, ty);
                }
            }

            fn visit_var(&mut self, _: &TyVar<'db>) {
                self.other = true;
            }

            fn visit_param(&mut self, _: &TyParam<'db>) {
                self.other = true;
            }

            fn visit_const_param(&mut self, _: &TyParam<'db>, _: TyId<'db>) {
                self.other = true;
            }

            fn visit_assoc_ty(&mut self, _: &AssocTy<'db>) {
                self.other = true;
            }
        }

        let mut visitor = Determined {
            db,
            param,
            mentions: false,
            other: false,
        };
        ty.visit_with(&mut visitor);
        visitor.mentions && !visitor.other
    }

    struct ReplaceTy<'db> {
        from: TyId<'db>,
        to: TyId<'db>,
    }

    impl<'db> TyFolder<'db> for ReplaceTy<'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            if ty == self.from {
                self.to
            } else {
                ty.super_fold_with(db, self)
            }
        }
    }

    fn match_ty<'db>(
        db: &'db dyn HirAnalysisDb,
        declared: TyId<'db>,
        actual: TyId<'db>,
        subst: &mut PartialSubst<'db>,
        parameters: &mut IndexMap<ParamKey<'db>, bool>,
    ) -> Option<()> {
        if declared.as_generic_param(db).is_some() {
            let key = subst.domain().schema(db).original_key(db, declared)?;
            subst.bind(db, key, actual).ok()?;
            parameters.insert(key, matches!(declared.data(db), TyData::TyParam(_)));
            Some(())
        } else if let (TyData::TyApp(left, right), TyData::TyApp(actual_left, actual_right)) =
            (declared.data(db), actual.data(db))
        {
            match_ty(db, *left, *actual_left, subst, parameters)?;
            match_ty(db, *right, *actual_right, subst, parameters)
        } else {
            (declared == actual).then_some(())
        }
    }

    let replace = |ty: TyId<'db>, from: TyId<'db>, to: TyId<'db>| {
        ty.fold_with(db, &mut ReplaceTy { from, to })
    };
    let holds = |goal| {
        matches!(
            is_goal_satisfiable(db, solve_cx, goal),
            GoalSatisfiability::Satisfied(_)
        )
    };
    let self_ty = unsat.unwrap_or(primary).self_ty(db);
    let (_, inner) = self_ty.as_capability(db)?;

    let primary_self = primary.self_ty(db);
    let count = occurrences(db, self_ty, &primary_self);
    if count == 0 {
        return None;
    }
    let declared = call_bound
        .and_then(|(callable_def, idx)| declared_call_bound(db, callable_def, idx))
        .filter(|declared| {
            declared.def(db) == primary.def(db) && declared.args(db).len() == primary.args(db).len()
        });
    if count > 1 {
        let (callable_def, _) = call_bound?;
        let declared = declared?;
        let unsupported =
            TyFlags::HAS_PROJECTION | TyFlags::HAS_VAR | TyFlags::HAS_INVALID | TyFlags::HAS_HOLE;
        if collect_flags(db, declared).intersects(unsupported)
            || collect_flags(db, primary).intersects(unsupported)
            || declared.assoc_type_bindings(db).len() != primary.assoc_type_bindings(db).len()
        {
            return None;
        }

        let domain = ParamDomainId::full(db, ParamSchemaId::callable(db, callable_def));
        let mut subst = PartialSubst::new(db, domain);
        let mut parameters = IndexMap::new();
        for (&declared_ty, &actual) in declared.args(db).iter().zip(primary.args(db)) {
            match_ty(db, declared_ty, actual, &mut subst, &mut parameters)?;
        }
        for (name, &declared_ty) in declared.assoc_type_bindings(db) {
            let actual = *primary.assoc_type_bindings(db).get(name)?;
            match_ty(db, declared_ty, actual, &mut subst, &mut parameters)?;
        }
        let bound = Binder::bind(callable_def.generic_owner(), declared);
        if bound.instantiate_subst(db, &subst.residualize(db)).ok()? != primary
            || parameters.len() != 1
        {
            return None;
        }
        let (&key, &is_type) = parameters.iter().next()?;
        let actual = subst.get(db, key)?;
        if !is_type
            || !matches!(key, ParamKey::Source { .. } | ParamKey::TraitSelf(_))
            || occurrences(db, self_ty, &actual) != 1
            || collect_flags(db, actual).intersects(unsupported)
        {
            return None;
        }
        let mut owned_subst = PartialSubst::new(db, domain);
        owned_subst
            .bind(db, key, replace(actual, self_ty, inner))
            .ok()?;
        let owned_primary = bound
            .instantiate_subst(db, &owned_subst.residualize(db))
            .ok()?;
        return (occurrences(db, self_ty, &owned_primary) == 0
            && !collect_flags(db, owned_primary).intersects(unsupported)
            && holds(owned_primary))
        .then_some(self_ty);
    }

    let owned_self = replace(primary_self, self_ty, inner);
    let declared_self = declared.map(|declared| declared.self_ty(db));
    // The owned form of a position declared as `declared_ty` and instantiated
    // as `actual`, if it follows the value.
    let follow = |declared_ty: TyId<'db>, actual: TyId<'db>| -> Option<TyId<'db>> {
        let declared_self = declared_self?;
        if declared_ty == declared_self {
            return (actual == primary_self).then_some(owned_self);
        }
        if !matches!(declared_self.data(db), TyData::TyParam(_))
            || !determined_by(db, declared_ty, declared_self)
            || replace(declared_ty, declared_self, primary_self) != actual
        {
            return None;
        }
        Some(replace(declared_ty, declared_self, owned_self))
    };

    let mut elsewhere = 0;
    let mut owned_args = primary.args(db).clone();
    owned_args[0] = owned_self;
    for (idx, arg) in owned_args.iter_mut().enumerate().skip(1) {
        match declared.and_then(|declared| follow(declared.args(db)[idx], *arg)) {
            Some(owned) => *arg = owned,
            None => elsewhere += occurrences(db, self_ty, arg),
        }
    }
    let mut owned_bindings = primary.assoc_type_bindings(db).clone();
    for (name, ty) in owned_bindings.iter_mut() {
        let declared_ty =
            declared.and_then(|declared| declared.assoc_type_bindings(db).get(name).copied());
        match declared_ty.and_then(|declared_ty| follow(declared_ty, *ty)) {
            Some(owned) => *ty = owned,
            None => elsewhere += occurrences(db, self_ty, ty),
        }
    }
    if elsewhere != 0 {
        return None;
    }
    let owned_primary = TraitInstId::new(db, primary.def(db), owned_args, owned_bindings);
    holds(owned_primary).then_some(self_ty)
}

/// Ground predicates are declaration obligations. Ordinary generic functions
/// and ADTs retain parameter-dependent predicates for substitution at uses.
/// A failed or unsupported evaluation must never count as a satisfied condition.
pub(crate) fn check_where_const_predicates<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: WhereClauseOwner<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let predicates = owner.where_clause(db).const_predicates(db);
    if predicates.is_empty() {
        return Vec::new();
    }
    // `where T` parses as a const predicate. When the lone path names a type,
    // the author most likely left out the trait bound, so say that instead of
    // reporting a type used as a value.
    let (missing_bounds, predicates): (Vec<Body<'db>>, Vec<Body<'db>>) = predicates
        .iter()
        .partition(|body| predicate_names_a_type(db, **body));
    let mut diags: Vec<FuncBodyDiag<'db>> = missing_bounds
        .iter()
        .map(|body| BodyDiag::WhereTypeBoundMissing(body.span().into()).into())
        .collect();
    if predicates.is_empty() {
        return diags;
    }
    let generic_declaration = match owner {
        WhereClauseOwner::Func(func) if func.is_free_or_inherent(db) => {
            Some(GenericParamOwner::Func(func))
        }
        WhereClauseOwner::Struct(record) => Some(GenericParamOwner::Struct(record)),
        WhereClauseOwner::Enum(enum_) => Some(GenericParamOwner::Enum(enum_)),
        _ => None,
    };
    // Inherent method parameter lists include their impl's generic parameters.
    // Requirement substitution reconciles their owners. Other generic contexts remain
    // unsupported, including trait methods and nested generic declarations.
    let inherited_impl = match owner {
        WhereClauseOwner::Func(func) => func
            .scope()
            .parent_item(db)
            .filter(|item| matches!(item, crate::hir_def::ItemKind::Impl(_))),
        _ => None,
    };
    let mut item = Some(crate::hir_def::ItemKind::from(owner));
    while let Some(current) = item {
        if let Some(params) = GenericParamOwner::from_item_opt(current)
            && !collect_generic_params(db, params).params(db).is_empty()
            && generic_declaration
                .is_none_or(|declaration| current != crate::hir_def::ItemKind::from(declaration))
            && Some(current) != inherited_impl
        {
            diags.extend(
                predicates.iter().map(|body| {
                    BodyDiag::GenericConstPredicateUnsupported(body.span().into()).into()
                }),
            );
            return diags;
        }
        item = current.scope().parent_item(db);
    }

    for &body in &predicates {
        let formation = const_requirements::check_predicate_formation(db, body);
        if formation.well_formed().is_none() {
            diags.extend(formation.diags.iter().cloned());
            continue;
        }
        if let Some(declaration) = generic_declaration
            && !collect_generic_params(db, declaration)
                .params(db)
                .is_empty()
            && const_requirements::predicate_may_depend_on_params(db, body)
        {
            continue;
        }
        let owner = BodyOwner::const_predicate(db, body);
        let outcome = condition_outcome(db, owner, GenericSubst::none(db));
        diags.extend(const_predicate_outcome_diag(db, body, outcome));
    }
    diags
}

/// What a boolean compile-time condition evaluated to. Only `True` holds: a
/// blocked or failed evaluation never counts as a satisfied condition.
pub(super) enum ConditionOutcome<'db> {
    True,
    False,
    /// Evaluation finished with a value that is not a `bool`.
    NotBool,
    Blocked(BlockedInfo<'db>),
    Failed(InvalidCause<'db>),
}

pub(super) fn condition_outcome<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    subst: GenericSubst<'db>,
) -> ConditionOutcome<'db> {
    match eval_body_owner_const(db, owner, subst) {
        EvalOutcome::Ready(value) => match static_assert_bool_value(db, value) {
            Some(true) => ConditionOutcome::True,
            Some(false) => ConditionOutcome::False,
            None => ConditionOutcome::NotBool,
        },
        EvalOutcome::Blocked(info) => ConditionOutcome::Blocked(info),
        EvalOutcome::Failed(failure) => {
            ConditionOutcome::Failed(invalid_cause_from_eval_failure(db, owner, failure))
        }
    }
}

/// The error of a condition whose evaluation was blocked on an unknown
/// dependency.
fn blocked_condition_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    condition: Body<'db>,
    info: &BlockedInfo<'db>,
) -> FuncBodyDiag<'db> {
    let (primary, dependency) = blocked_const_detail(db, condition, info);
    BodyDiag::ConstDependencyMustBeKnown {
        primary,
        dependency,
    }
    .into()
}

/// The error of a condition whose evaluation failed, when the failure has a
/// rendered form.
fn failed_condition_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    condition: Body<'db>,
    cause: InvalidCause<'db>,
) -> Option<FuncBodyDiag<'db>> {
    TyId::invalid(db, cause)
        .emit_diag(db, condition.span().into())
        .map(FuncBodyDiag::from)
}

/// Reports a const predicate's outcome at its declaration. The predicate
/// holds only when it evaluates to `true`, and any other outcome is an error.
fn const_predicate_outcome_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    predicate: Body<'db>,
    outcome: ConditionOutcome<'db>,
) -> Option<FuncBodyDiag<'db>> {
    let unknown = || BodyDiag::ConstValueMustBeKnown(predicate.span().into()).into();
    Some(match outcome {
        ConditionOutcome::True => return None,
        ConditionOutcome::False => {
            BodyDiag::WhereConstPredicateFailed(predicate.span().into()).into()
        }
        ConditionOutcome::NotBool => unknown(),
        ConditionOutcome::Blocked(info) => blocked_condition_diag(db, predicate, &info),
        ConditionOutcome::Failed(cause) => {
            failed_condition_diag(db, predicate, cause).unwrap_or_else(unknown)
        }
    })
}

/// Whether a const predicate is a lone path that names a type, as in `where T`.
/// A const generic parameter also resolves to a type, but it is a value here,
/// matching how the type checker reads a path expression.
fn predicate_names_a_type<'db>(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> bool {
    let Partial::Present(Expr::Path(Partial::Present(path))) = body.expr(db).data(db, body) else {
        return false;
    };
    let resolved = crate::analysis::name_resolution::resolve_path(
        db,
        *path,
        body.scope(),
        PredicateListId::empty_list(db),
        true,
    );
    matches!(
        resolved,
        Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) if ty.const_ty_ty(db).is_none()
    )
}

/// What an effect key that is neither a valid type nor trait key reports. A
/// key whose path names a type or trait reports the error written in it, at
/// that error; only a key naming neither is an unresolved effect.
pub(crate) fn invalid_effect_key_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: EffectParamOwner<'db>,
    idx: usize,
    key: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let span = owner.effect_param_ty_span(db, idx);
    let written = match key.data(db) {
        TypeKind::Path(Partial::Present(path)) => {
            let path_span = span.clone().into_path_type().path();
            let arg_diags = || {
                path_generic_args(db, *path, path_span.clone())
                    .filter_map(|(arg, span)| match arg {
                        GenericArg::Type(arg) => {
                            Some((arg.ty.to_opt()?, span.into_type_arg().ty()))
                        }
                        GenericArg::AssocType(binding) => {
                            Some((binding.ty.to_opt()?, span.into_assoc_type_arg().ty()))
                        }
                        GenericArg::Const(_) => None,
                    })
                    .flat_map(|(ty, span)| collect_hir_ty_diags(db, scope, ty, span, assumptions))
                    .collect::<Vec<_>>()
            };
            match resolve_path(db, *path, scope, assumptions, false) {
                Ok(PathRes::Ty(_) | PathRes::TyAlias(..)) => {
                    collect_hir_ty_diags(db, scope, key, span, assumptions)
                }
                Ok(PathRes::Trait(_)) => arg_diags(),
                // A fault in the reference's own arguments, such as a const
                // argument failing evaluation, is reported where it is written.
                Err(err)
                    if !matches!(err.kind, PathResErrorKind::NotFound { .. })
                        && err.is_on_path(db, *path) =>
                {
                    err.into_trait_ref_diag(db, *path, path_span)
                        .into_iter()
                        .collect()
                }
                Err(_) => arg_diags(),
                Ok(_) => Vec::new(),
            }
        }
        _ => collect_hir_ty_diags(db, scope, key, span, assumptions),
    };
    if written.is_empty() {
        vec![BodyDiag::InvalidEffectKey { owner, key, idx }.into()]
    } else {
        written.into_iter().map(FuncBodyDiag::Ty).collect()
    }
}

#[salsa::tracked(return_ref)]
pub fn check_static_assert<'db>(
    db: &'db dyn HirAnalysisDb,
    assert_: StaticAssert<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let condition = assert_.condition(db);
    let expected = TyId::bool(db);
    let owner = BodyOwner::AnonConstBody {
        body: condition,
        expected,
    };
    let (body_diags, typed_body) = check_anon_const_body(db, condition, expected);
    if !diags_allow_evaluation(db, body_diags) {
        return body_diags.clone();
    }

    match condition_outcome(db, owner, GenericSubst::none(db)) {
        ConditionOutcome::True => Vec::new(),
        ConditionOutcome::False => {
            let mut diags = body_diags.clone();
            let comparison = if body_diags.is_empty() {
                assert_.comparison(db).and_then(|comparison| {
                    static_assert_comparison_values(db, condition, typed_body, comparison)
                })
            } else {
                None
            };
            diags.push(
                BodyDiag::StaticAssertFailed {
                    primary: condition.span().into(),
                    comparison,
                }
                .into(),
            );
            diags
        }
        ConditionOutcome::NotBool => {
            let cause = InvalidCause::ConstEvalInvariant {
                body: condition,
                expr: condition.expr(db),
                message: "static assertion CTFE returned a non-boolean value".into(),
            };
            failed_condition_diag(db, condition, cause)
                .into_iter()
                .collect()
        }
        ConditionOutcome::Blocked(info) => vec![blocked_condition_diag(db, condition, &info)],
        ConditionOutcome::Failed(cause) => failed_condition_diag(db, condition, cause)
            .into_iter()
            .collect(),
    }
}

fn static_assert_bool_value<'db>(
    db: &'db dyn HirAnalysisDb,
    value: SemConstId<'db>,
) -> Option<bool> {
    let SemConstValue::Scalar {
        value: SemConstScalar::Bool(value),
        ..
    } = value.value(db)
    else {
        return None;
    };
    Some(*value)
}

/// Whether a const body with these diagnostics can still be evaluated: it
/// has none, or only integer literals whose type inference left open, which
/// evaluation defaults.
fn diags_allow_evaluation<'db>(db: &'db dyn HirAnalysisDb, diags: &[FuncBodyDiag<'db>]) -> bool {
    diags.iter().all(|diag| {
        matches!(
            diag,
            FuncBodyDiag::Body(BodyDiag::TypeAnnotationNeeded { ty, .. })
                if ty.is_integral_var(db)
        )
    })
}

fn static_assert_comparison_values<'db>(
    db: &'db dyn HirAnalysisDb,
    condition: Body<'db>,
    typed_body: &TypedBody<'db>,
    comparison: StaticAssertComparison<'db>,
) -> Option<StaticAssertComparisonValues> {
    let Partial::Present(Expr::Bin(lhs_expr, rhs_expr, BinOp::Comp(op))) =
        condition.expr(db).data(db, condition)
    else {
        return None;
    };
    if *op != comparison.op {
        return None;
    }

    let lhs = eval_static_assert_comparison_operand(
        db,
        comparison.lhs,
        typed_body.expr_ty(db, *lhs_expr),
    )?;
    let rhs = eval_static_assert_comparison_operand(
        db,
        comparison.rhs,
        typed_body.expr_ty(db, *rhs_expr),
    )?;

    Some(StaticAssertComparisonValues {
        op: comparison.op,
        lhs: lhs.pretty_print(db),
        rhs: rhs.pretty_print(db),
    })
}

fn eval_static_assert_comparison_operand<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: TyId<'db>,
) -> Option<SemConstId<'db>> {
    if expected.has_invalid(db) {
        return None;
    }
    let owner = BodyOwner::AnonConstBody { body, expected };
    let body_diags = &check_anon_const_body(db, body, expected).0;
    if !diags_allow_evaluation(db, body_diags) {
        return None;
    }
    eval_body_owner_const(db, owner, GenericSubst::none(db)).into_ready()
}

pub(super) fn check_body<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let CheckedBody { diags, typed, .. } = checked_body(db, owner);
    (diags, typed)
}

/// A body checked by the pipeline every kind of body takes: inference with
/// its const-language checks (`infer_body`), then, when inference leaves the
/// body evaluable, its requirement checks.
pub(super) struct CheckedBody<'db> {
    pub(super) diags: Vec<FuncBodyDiag<'db>>,
    pub(super) typed: TypedBody<'db>,
    /// Whether a requirement the body uses depends on itself.
    pub(super) recursive: bool,
}

pub(super) fn checked_body<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> CheckedBody<'db> {
    let (mut diags, typed) = infer_body(db, owner).clone();
    let mut recursive = false;
    if diags_allow_evaluation(db, &diags) {
        let requirements = const_requirements::check_body_requirements(db, owner, &typed);
        recursive = requirements.recursive;
        diags.extend(requirements.diags);
    }
    CheckedBody {
        diags,
        typed,
        recursive,
    }
}

/// Inference and const-language checking, without requirement discharge.
/// CTFE consumes this completed template. It must not re-enter the enclosing
/// discharge query by requesting a checked body while evaluating a predicate.
/// Compiler diagnostics still use the checked body entry points above.
pub(crate) fn infer_body<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> &'db (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    infer_body_query(db, BodyInferenceKey::new(db, owner))
}

#[salsa::interned]
struct BodyInferenceKey<'db> {
    owner: BodyOwner<'db>,
}

#[salsa::tracked(return_ref, cycle_initial=infer_body_cycle_initial, cycle_fn=infer_body_cycle_recover)]
fn infer_body_query<'db>(
    db: &'db dyn HirAnalysisDb,
    key: BodyInferenceKey<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let owner = key.owner(db);
    let Ok(mut checker) = TyChecker::new(db, owner) else {
        return (
            Vec::new(),
            match owner {
                BodyOwner::Func(func) => typed_body_for_bodyless_func(db, func),
                BodyOwner::Const(_)
                | BodyOwner::AnonConstBody { .. }
                | BodyOwner::ContractInit { .. }
                | BodyOwner::ContractRecvArm { .. } => TypedBody::empty(db),
            },
        );
    };

    checker.run();
    let (mut diags, typed_body) = checker.finish();
    if let BodyOwner::Func(func) = owner
        && func.is_const(db)
        && !func.is_extern(db)
    {
        diags.extend(crate::analysis::ty::const_check::check_const_fn_body(
            db,
            func,
            &typed_body,
        ));
    }
    // A `where` condition is held to the const language as a const
    // function's body is, once it type checks. This adds no edge that can
    // close a query cycle: recognizing a condition reads only its item's
    // lowered `where` clause, and the check reads each callee's declaration,
    // effect requirements and trait method resolution, as it does for a
    // const function above. None of those infers a condition: only
    // requirement checking, declaration checks, borrow checking and layout
    // evidence do, and type lowering and compile-time evaluation, which
    // those reads can reach, never call them.
    if let BodyOwner::AnonConstBody { body, .. } = owner
        && owner.is_const_predicate(db)
        && diags_allow_evaluation(db, &diags)
    {
        diags.extend(
            crate::analysis::ty::const_check::check_const_body_expressions(db, body, &typed_body),
        );
    }

    (diags, typed_body)
}

// A constant evaluated from a type expression can call the body currently
// being inferred. Start that cycle with a failed template, never a
// provisional successful value; existing query handlers may still iterate.
fn infer_body_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    key: BodyInferenceKey<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let mut typed = TypedBody::empty(db);
    typed.tables_mut().result_ty = TyId::invalid(db, InvalidCause::TypeLoweringCycle);
    let span = key
        .owner(db)
        .body(db)
        .map(|body| body.span().into())
        .unwrap_or_else(DynLazySpan::invalid);
    (
        vec![FuncBodyDiag::Ty(
            TyLowerDiag::TypeLoweringCycle(span).into(),
        )],
        typed,
    )
}

fn infer_body_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &(Vec<FuncBodyDiag<'db>>, TypedBody<'db>),
    _count: u32,
    _key: BodyInferenceKey<'db>,
) -> salsa::CycleRecoveryAction<(Vec<FuncBodyDiag<'db>>, TypedBody<'db>)> {
    salsa::CycleRecoveryAction::Iterate
}

/// Whether inference met a type lowering cycle, in the body or in the
/// result type it checks the body against. Its typed body is then a
/// provisional fixpoint value, which compile-time evaluation must not run:
/// a value computed from it could make the cycle converge on a success.
pub(crate) fn inference_met_lowering_cycle<'db>(
    db: &'db dyn HirAnalysisDb,
    (diags, typed_body): &(Vec<FuncBodyDiag<'db>>, TypedBody<'db>),
) -> bool {
    diags.iter().any(|diag| {
        matches!(
            diag,
            FuncBodyDiag::Ty(TyDiagCollection::Ty(TyLowerDiag::TypeLoweringCycle(_)))
        )
    }) || matches!(
        typed_body.result_ty().invalid_cause(db),
        Some(InvalidCause::TypeLoweringCycle)
    )
}

/// Forces evaluation of a const item's value and reports failures
/// (unknowable values, recursion, arithmetic errors). This is deliberately a
/// separate query from `check_const_body`: forcing evaluates *other* consts
/// the body references, and their `ensure_const_evaluable` checks call
/// `check_const_body` back — if typing and forcing were one query, a
/// recursive const definition would be an unrecoverable salsa cycle through
/// body checking instead of converging to a `RecursiveConst` error in the
/// eval queries.
#[salsa::tracked(return_ref)]
pub fn check_const_value<'db>(
    db: &'db dyn HirAnalysisDb,
    const_: Const<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    if !check_const_body(db, const_).0.is_empty() {
        return Vec::new();
    }
    let Some(body) = const_.body(db).to_opt() else {
        return Vec::new();
    };
    if const_.ty(db).has_invalid(db) {
        return Vec::new();
    }

    const_body_ctfe_diags(db, body, const_.ty(db), ConstUsePolicy::RequireValue)
}

/// CTFE-validates an anonymous const body (a top-level `const` initializer or
/// an inherent associated-const value). Mirrors the validation top-level
/// consts get, so a body that type-checks but is not const-evaluable (e.g.
/// `const C: u256 = ordinary_fn()`) is rejected here rather than only when
/// referenced.
///
/// `AllowDependent` applies to consts on generic impls: their value
/// (e.g. `256 / BITS`) is legitimately parametric and is fully evaluated per
/// instantiation, so a dependent description is expected. A
/// genuinely non-const body still yields `NotConstEvaluable` and is flagged
/// regardless.
pub(crate) fn const_body_ctfe_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: TyId<'db>,
    policy: ConstUsePolicy,
) -> Vec<FuncBodyDiag<'db>> {
    const_body_ctfe_diags_with_context(db, body, expected, policy, None)
}

struct ConstDiagContext {
    const_name: String,
    origin: String,
}

fn const_body_ctfe_diags_with_context<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: TyId<'db>,
    policy: ConstUsePolicy,
    context: Option<ConstDiagContext>,
) -> Vec<FuncBodyDiag<'db>> {
    if expected.has_invalid(db) {
        return Vec::new();
    }
    let owner = BodyOwner::AnonConstBody { body, expected };
    let mut diags = Vec::new();
    let require_value = match policy {
        ConstUsePolicy::AllowDependent => false,
        ConstUsePolicy::RequireValue => true,
        ConstUsePolicy::OptionalFold => {
            unreachable!("optional folding does not produce declaration diagnostics")
        }
    };
    // A dependent body waits on its parameters before reaching a call it may
    // not make, so the const language is checked before any evaluation.
    let language_failure = const_body_language_failure(db, body, &infer_body(db, owner).1);
    let outcome = if let Some(cause) = language_failure {
        Err(cause)
    } else if require_value {
        Ok(eval_body_owner_const(db, owner, GenericSubst::none(db)))
    } else {
        // A dependent declaration may describe opaque computations, such as
        // extern const calls, that only a value demand has to execute.
        let key = SemanticInstanceKey::new(
            db,
            owner,
            GenericSubst::none(db),
            EffectProviderSubst::empty(db),
            ImplEnv::empty(db, owner.scope()),
        );
        let request = const_computation_for_instance(db, key, Vec::new());
        match describe_const_computation(db, request, CtfeConfig::default()) {
            EvalOutcome::Failed(failure) => Ok(EvalOutcome::Failed(failure)),
            EvalOutcome::Ready(_) | EvalOutcome::Blocked(_) => return diags,
        }
    };
    // A function is evaluated only through its callers, so a fault in one is
    // revealed by what this body evaluates. A constant's declaration reports
    // a fault in its own body.
    let mut revealed_at = None;
    let cause = match outcome {
        Err(cause) => cause,
        Ok(EvalOutcome::Failed(failure)) => {
            if let EvalFailure::Ctfe(error @ CtfeError::CalleeError { origin, .. }) = &failure
                && matches!(root_ctfe_error(db, owner, error).0, BodyOwner::Func(_))
            {
                revealed_at = Some(origin_expr_for_const_eval_diag(db, body, *origin));
            }
            invalid_cause_from_eval_failure(db, owner, failure)
        }
        Ok(EvalOutcome::Ready(value)) => {
            if matches!(value.value(db), SemConstValue::Description(..)) {
                let cause = InvalidCause::ConstEvalInvariant {
                    body,
                    expr: body.expr(db),
                    message: "CTFE published an unresolved value as complete".into(),
                };
                if let Some(context) = context {
                    diags.push(
                        BodyDiag::ConstEvaluationFailed {
                            primary: body.span().into(),
                            const_name: context.const_name,
                            origin: context.origin,
                            reason: "hit a compiler invariant: CTFE published an unresolved value as complete".into(),
                        }
                        .into(),
                    );
                } else if let Some(diag) =
                    TyId::invalid(db, cause).emit_diag(db, body.span().into())
                {
                    diags.push(diag.into());
                }
            } else if require_value {
                let key = SemanticInstanceKey::new(
                    db,
                    owner,
                    GenericSubst::none(db),
                    EffectProviderSubst::empty(db),
                    ImplEnv::empty(db, owner.scope()),
                );
                let semantic = get_or_build_semantic_instance(db, key);
                if reify_runtime_const_for_ty(db, semantic, expected, value).is_none() {
                    push_const_eval_diag(
                        &mut diags,
                        body,
                        context,
                        "could not be reified for runtime lowering",
                    );
                }
            }
            return diags;
        }
        Ok(EvalOutcome::Blocked(info)) => {
            if require_value {
                let (primary, dependency) = blocked_const_detail(db, body, &info);
                if let Some(context) = context {
                    diags.push(
                        BodyDiag::ConstEvaluationFailed {
                            primary,
                            const_name: context.const_name,
                            origin: context.origin,
                            reason: format!("requires unresolved {dependency}"),
                        }
                        .into(),
                    );
                } else {
                    diags.push(
                        BodyDiag::ConstDependencyMustBeKnown {
                            primary,
                            dependency,
                        }
                        .into(),
                    );
                }
            }
            return diags;
        }
    };
    if let Some(context) = context {
        let reason = match &cause {
            InvalidCause::ConstEvalAssertionFailed { message, .. } => message.as_ref().map_or_else(
                || "failed an assertion".to_string(),
                |message| format!("failed an assertion: {message}"),
            ),
            InvalidCause::ConstEvalDivisionByZero { .. } => "divided by zero".into(),
            InvalidCause::ConstEvalOutOfBounds { .. } => "indexed out of bounds".into(),
            InvalidCause::ConstEvalInvalidOperation { message, .. } => message.clone(),
            InvalidCause::ConstEvalInvalidBorrow { .. } => "used an invalid borrow".into(),
            InvalidCause::ConstEvalInvalidProviderUse { .. } => {
                "used an invalid effect provider".into()
            }
            InvalidCause::ConstEvalVariantMismatch { .. } => {
                "selected the wrong enum variant".into()
            }
            InvalidCause::ConstEvalUninitializedLocal { .. } => {
                "read an uninitialized value".into()
            }
            InvalidCause::ConstEvalArithmeticOverflow { .. } => "overflowed".into(),
            InvalidCause::ConstEvalNegativeExponent { .. } => "used a negative exponent".into(),
            InvalidCause::ConstEvalStepLimitExceeded { .. } => {
                "exceeded the CTFE step limit".into()
            }
            InvalidCause::ConstEvalRecursionLimitExceeded { .. } => {
                "exceeded the CTFE recursion limit".into()
            }
            InvalidCause::ConstEvalRecursiveConst { .. } => "depends recursively on itself".into(),
            InvalidCause::ConstEvalNonConstCall { .. } => "called a non-const function".into(),
            InvalidCause::ConstEvalInvariant { message, .. } => {
                format!("hit a compiler invariant: {message}")
            }
            _ => "failed during compile-time evaluation".into(),
        };
        diags.push(
            BodyDiag::ConstEvaluationFailed {
                primary: body.span().into(),
                const_name: context.const_name,
                origin: context.origin,
                reason,
            }
            .into(),
        );
    } else if let Some(fault) = TyId::invalid(db, cause).emit_diag(db, body.span().into()) {
        diags.push(FuncBodyDiag::Ty(match revealed_at {
            Some(expr) => TyLowerDiag::RevealedConstFault {
                site: expr.span(body).into(),
                revealed_by: RevealedBy::Evaluation,
                fault: Box::new(fault),
            }
            .into(),
            None => fault,
        }));
    }
    diags
}

fn blocked_const_detail<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    info: &BlockedInfo<'db>,
) -> (DynLazySpan<'db>, String) {
    let origin_body = info
        .trace
        .first()
        .and_then(|key| key.owner(db).body(db))
        .or_else(|| match info.origin {
            SemOrigin::Body(owner) => owner.body(db),
            SemOrigin::Expr(_) | SemOrigin::Stmt(_) | SemOrigin::Synthetic => None,
        })
        .unwrap_or(body);
    let primary = origin_expr_for_const_eval_diag(db, origin_body, info.origin)
        .span(origin_body)
        .into();
    let dependency = match info.first_dependency {
        ConstDependency::Value(ty) => format!("value `{}`", ty.pretty_print(db)),
        ConstDependency::Type(ty) => format!("type `{}`", ty.pretty_print(db)),
        ConstDependency::Selection(expr) => format!("selection `{}`", expr.pretty_print(db)),
        ConstDependency::Computation(computation) => format!(
            "constant computation producing `{}`",
            computation.result_ty(db).pretty_print(db)
        ),
        ConstDependency::AssociatedSelection(use_) => {
            format!("associated constant `{}`", use_.name().data(db))
        }
        ConstDependency::InherentSelection(use_) => {
            format!("inherent constant `{}`", use_.name().data(db))
        }
    };
    (primary, dependency)
}

fn push_const_eval_diag<'db>(
    diags: &mut Vec<FuncBodyDiag<'db>>,
    body: Body<'db>,
    context: Option<ConstDiagContext>,
    reason: &str,
) {
    if let Some(context) = context {
        diags.push(
            BodyDiag::ConstEvaluationFailed {
                primary: body.span().into(),
                const_name: context.const_name,
                origin: context.origin,
                reason: reason.to_string(),
            }
            .into(),
        );
    } else {
        diags.push(BodyDiag::ConstValueMustBeKnown(body.span().into()).into());
    }
}

fn typed_body_for_bodyless_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> TypedBody<'db> {
    let assumptions = crate::semantic::func_body_assumptions(db, func).extend_all_bounds(db);
    let mut result_ty = func.return_ty(db);
    if !result_ty.is_star_kind(db) || ty_contains_const_hole(db, result_ty) {
        result_ty = TyId::invalid(db, InvalidCause::Other);
    }
    let param_bindings = func
        .params(db)
        .enumerate()
        .map(|(idx, view)| {
            let mut ty = *func
                .arg_tys(db)
                .get(idx)
                .map_or(&TyId::invalid(db, InvalidCause::ParseError), |binder| {
                    binder.skip_binder()
                });
            if !ty.is_star_kind(db) || (!view.is_self_param(db) && ty_contains_const_hole(db, ty)) {
                ty = TyId::invalid(db, InvalidCause::Other);
            }
            LocalBinding::Param {
                site: ParamSite::Func(func),
                idx,
                mode: view.mode(db),
                ty,
                is_mut: view.is_mut(db),
            }
        })
        .collect();
    TypedBodyTables {
        body: None,
        result_ty,
        assumptions,
        pat_ty: SecondaryMap::new(),
        expr_ty: SecondaryMap::new(),
        implicit_moves: FxHashSet::default(),
        const_refs: SecondaryMap::new(),
        value_path_refs: SecondaryMap::new(),
        semantic_expr_lowering: SecondaryMap::new(),
        record_init_lowering: SecondaryMap::new(),
        resolved_field_index: SecondaryMap::new(),
        call_effect_args: SecondaryMap::new(),
        return_borrow_provider: None,
        param_bindings,
        pat_bindings: SecondaryMap::new(),
        pat_binding_modes: SecondaryMap::new(),
        pattern_store: PatternStore::default(),
        pattern_status: SecondaryMap::with_default(PatternAnalysisStatus::Invalid),
        for_loop_seq: SecondaryMap::new(),
        expr_place: SecondaryMap::new(),
        expr_places: PrimaryMap::new(),
        path_applications: Vec::new(),
    }
    .into()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BindingInterfaceShape<'db> {
    OrdinaryValue,
    ProviderValue {
        kind: ProviderKind,
        value_ty: TyId<'db>,
    },
    PlaceCarrier {
        value_ty: TyId<'db>,
    },
    DirectCarrier {
        target_ty: TyId<'db>,
    },
}

#[derive(Clone)]
pub(super) enum PatternLayoutContext<'db> {
    ContractField {
        field: &'db crate::semantic::FieldStorageLayout<'db>,
        view: crate::semantic::LayoutViewKind,
        base_projections: Vec<crate::semantic::LayoutProjection>,
    },
    CallableInput {
        func: Func<'db>,
        origin: crate::analysis::ty::const_ty::CallableInputLayoutHoleOrigin,
        base_path: LayoutBundlePath,
    },
}

pub struct TyChecker<'db> {
    pub(crate) db: &'db dyn HirAnalysisDb,
    pub(crate) env: TyCheckEnv<'db>,
    pub(crate) table: UnificationTable<'db>,
    expected: TyId<'db>,
    effect_provider_keys: FxHashSet<InferenceKey<'db>>,
    first_return_borrow_provider: Option<(DynLazySpan<'db>, ProviderAddressSpace)>,
    diags: Vec<FuncBodyDiag<'db>>,
    /// Set once deferred work is resolved for the last time, after which type
    /// equalities are decided rather than retained.
    final_pass: bool,
}

pub(crate) struct TyCheckerSnapshot<'db> {
    table: Snapshot<InPlace<InferenceKey<'db>>>,
    deferred_len: usize,
}

enum TraitObligationOutcome<'db> {
    Discharged,
    Progressed,
    Requeue(env::TraitObligation<'db>),
}

/// Whether a projection or const computation in `ty` still waits on inference.
fn depends_on_type_inference<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    struct Computation<'db> {
        db: &'db dyn HirAnalysisDb,
        found: bool,
    }
    impl<'db> TyVisitor<'db> for Computation<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.found || !ty.has_var(self.db) {
                return;
            }
            self.found = match ty.data(self.db) {
                TyData::ConstTy(const_ty) => matches!(
                    const_ty.data(self.db),
                    ConstTyData::Abstract(..)
                        | ConstTyData::Computation { .. }
                        | ConstTyData::UnEvaluated { .. }
                ),
                TyData::AssocTy(_) | TyData::QualifiedTy(_) => true,
                _ => false,
            };
            if !self.found {
                walk_ty(self, ty);
            }
        }
    }
    let mut visitor = Computation { db, found: false };
    ty.visit_with(&mut visitor);
    visitor.found
}

impl<'db> TyChecker<'db> {
    fn string_literal_fallback(&self) -> StringFallback {
        StringFallback::Fixed
    }

    fn new(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Result<Self, ()> {
        let env = TyCheckEnv::new(db, owner)?;
        let expected = env.compute_expected_return();
        let mut checker = Self::new_internal(db, env, expected);
        checker.seed_effect_witnesses();
        Ok(checker)
    }

    fn run(&mut self) {
        self.check_effect_param_keys_resolve();
        self.check_own_param_types();

        if let BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } = self.env.owner()
        {
            let recv_span = self.env.owner().recv_span().unwrap();
            let arm_span = self.env.owner().recv_arm_span().unwrap();
            let arm = contract
                .recv_arm(self.db, recv_idx as usize, arm_idx as usize)
                .expect("recv arm exists");
            let msg_path = contract
                .recvs(self.db)
                .data(self.db)
                .get(recv_idx as usize)
                .and_then(|r| r.msg_path);
            let (pat_ty, ret_ty) =
                self.resolve_recv_arm_types(contract, msg_path, arm, recv_span.path(), arm_span);
            self.expected = ret_ty;
            self.check_pat(arm.pat, pat_ty);
            self.seed_pat_bindings(arm.pat);
            self.env.flush_pending_bindings();
        }

        let root_expr = self.env.body().expr(self.db);
        self.check_expr(root_expr, self.expected);
        self.record_implicit_move_for_owned_expr(root_expr, self.expected);
    }

    fn check_own_param_types(&mut self) {
        match self.env.owner() {
            BodyOwner::Func(_) => {}
            BodyOwner::ContractInit { contract } => {
                let Some(init) = contract.init(self.db) else {
                    return;
                };
                let scope = self.env.scope();
                let assumptions = self.env.assumptions();
                for (idx, param) in init.params(self.db).data(self.db).iter().enumerate() {
                    let Some(hir_ty) = param.ty.to_opt() else {
                        continue;
                    };
                    let ty = lower_hir_ty(self.db, hir_ty, scope, assumptions);
                    let span = contract.span().init_block().params().param(idx).ty().into();

                    if ty_contains_const_hole(self.db, ty) {
                        self.push_diag(TyDiagCollection::from(
                            TyLowerDiag::ConstHoleInValuePosition { span, ty },
                        ));
                        continue;
                    }

                    let source = lower_hir_ty_deferred(self.db, hir_ty, scope, assumptions);
                    match runtime_size_bytes_with_source(self.db, ConcreteTypeView::new(ty, source))
                    {
                        Err(RuntimeSizeError::InvalidType(cause)) => {
                            if let Some(diag) = diag_from_invalid_cause(span.clone(), &cause) {
                                self.push_diag(diag);
                            } else {
                                self.push_diag(BodyDiag::TypeMustBeKnown(span));
                            }
                            continue;
                        }
                        Err(RuntimeSizeError::Overflow) => {
                            self.push_diag(BodyDiag::TypeSizeOverflow { primary: span, ty });
                            continue;
                        }
                        Err(RuntimeSizeError::UnavailableConcrete) | Ok(None) => {
                            self.push_diag(BodyDiag::TypeMustBeKnown(span));
                            continue;
                        }
                        Ok(Some(_)) => {}
                    }

                    if param.mode != crate::hir_def::params::FuncParamMode::Own {
                        continue;
                    }

                    if ty.as_borrow(self.db).is_some() {
                        self.push_diag(BodyDiag::OwnParamCannotBeBorrow {
                            primary: contract.span().init_block().params().param(idx).ty().into(),
                            ty,
                        });
                    }
                }
            }
            BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::ContractRecvArm { .. } => {}
        }
    }

    fn check_effect_param_keys_resolve(&mut self) {
        match self.env.owner() {
            BodyOwner::Func(func) => self.check_free_func_effect_list(func, func.effects(self.db)),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => {}
            owner @ BodyOwner::ContractInit { contract } => {
                self.check_contract_scoped_effect_list(owner, contract, owner.effects(self.db));
            }
            owner @ BodyOwner::ContractRecvArm { contract, .. } => {
                self.check_contract_scoped_effect_list(owner, contract, owner.effects(self.db));
            }
        }
    }

    fn check_free_func_effect_list(
        &mut self,
        func: Func<'db>,
        effects: crate::hir_def::EffectParamListId<'db>,
    ) {
        for (idx, effect) in effects.data(self.db).iter().enumerate() {
            let Some(key_ty) = effect.key_ty.to_opt() else {
                continue;
            };

            if !matches!(
                resolve_callable_input_effect_key(
                    self.db,
                    func,
                    idx,
                    key_ty,
                    self.env.assumptions(),
                ),
                ResolvedEffectKey::Type(_) | ResolvedEffectKey::Trait(_)
            ) {
                for diag in invalid_effect_key_diags(
                    self.db,
                    EffectParamOwner::Func(func),
                    idx,
                    key_ty,
                    func.scope(),
                    self.env.assumptions(),
                ) {
                    self.push_diag(diag);
                }
            }
        }
    }

    fn check_contract_scoped_effect_list(
        &mut self,
        owner: BodyOwner<'db>,
        contract: Contract<'db>,
        effects: crate::hir_def::EffectParamListId<'db>,
    ) {
        let (owner, site) = match owner {
            BodyOwner::Func(func) => (EffectParamOwner::Func(func), EffectParamSite::Func(func)),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => unreachable!(),
            BodyOwner::ContractInit { contract } => (
                EffectParamOwner::ContractInit { contract },
                EffectParamSite::ContractInit { contract },
            ),
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => (
                EffectParamOwner::ContractRecvArm {
                    contract,
                    recv_idx,
                    arm_idx,
                },
                EffectParamSite::ContractRecvArm {
                    contract,
                    recv_idx,
                    arm_idx,
                },
            ),
        };
        let view = crate::core::semantic::EffectEnvView::new(site);

        let assumptions = PredicateListId::empty_list(self.db);
        let root_effect_ty = crate::analysis::ty::registered_root_providers(
            self.db,
            EffectParamSite::Contract(contract),
        )
        .first()
        .map(|registration| registration.provider_ty);

        for (idx, effect) in effects.data(self.db).iter().enumerate() {
            let Some(key_ty) = effect.key_ty.to_opt() else {
                continue;
            };

            // Labeled effects are always type/trait keyed: `name: Type`.
            if effect.name.is_some() {
                match resolve_effect_key(self.db, key_ty, contract.scope(), assumptions) {
                    ResolvedEffectKey::Trait(schema) => {
                        let Some(root_effect_ty) = root_effect_ty else {
                            continue;
                        };
                        let trait_req = super::instantiate_trait_self(
                            self.db,
                            schema.into_trait_inst(self.db),
                            root_effect_ty,
                        );
                        if matches!(
                            is_goal_satisfiable(
                                self.db,
                                TraitSolveCx::new(self.db, contract.scope()),
                                trait_req
                            ),
                            GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid
                        ) {
                            self.push_diag(BodyDiag::ContractRootEffectTraitNotImplemented {
                                owner,
                                idx,
                                root_ty: root_effect_ty,
                                trait_req,
                            });
                        }
                    }
                    ResolvedEffectKey::Type(schema) => {
                        let given =
                            normalize_ty(self.db, schema.carrier, contract.scope(), assumptions);
                        if !given.is_zero_sized(self.db) {
                            self.push_diag(BodyDiag::ContractRootEffectTypeNotZeroSized {
                                owner,
                                key: key_ty,
                                idx,
                                given,
                            });
                        }
                    }
                    ResolvedEffectKey::Invalid | ResolvedEffectKey::Other => {
                        for diag in invalid_effect_key_diags(
                            self.db,
                            owner,
                            idx,
                            key_ty,
                            contract.scope(),
                            assumptions,
                        ) {
                            self.push_diag(diag);
                        }
                    }
                }
                continue;
            }

            match view.resolved_binding(self.db, idx) {
                Some(binding)
                    if matches!(
                        binding.provider.source,
                        crate::core::semantic::ProviderSource::ContractField { .. }
                            | crate::core::semantic::ProviderSource::RootProvider { .. }
                    ) =>
                {
                    // Contract fields are immutable unless declared `mut`. The
                    // one exception — assigning immutable code-backed fields in
                    // `init` — is already reflected in the provider's
                    // site-specific mutability.
                    if binding.requirement.is_mut
                        && !binding.provider.is_mut
                        && let crate::core::semantic::ProviderSource::ContractField { field } =
                            binding.provider.source
                    {
                        self.push_diag(BodyDiag::ImmutableContractFieldMutBinding {
                            primary: owner.effect_param_ty_span(self.db, idx).into(),
                            field: binding.requirement.binding_name,
                            field_span: crate::hir_def::FieldParent::Contract(field.contract)
                                .field_name_span(field.index as usize),
                        });
                    }
                }
                _ => {
                    self.push_diag(BodyDiag::InvalidEffectKey {
                        owner,
                        key: key_ty,
                        idx,
                    });
                }
            }
        }
    }
    fn finish(self) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
        TyCheckerFinalizer::new(self).finish()
    }

    pub(crate) fn snapshot_state(&mut self) -> TyCheckerSnapshot<'db> {
        TyCheckerSnapshot {
            table: self.table.snapshot(),
            deferred_len: self.env.deferred_len(),
        }
    }

    pub(crate) fn rollback_state(&mut self, snapshot: TyCheckerSnapshot<'db>) {
        self.table.rollback_to(snapshot.table);
        self.env.truncate_deferred_tasks(snapshot.deferred_len);
    }

    fn commit_state(&mut self, snapshot: TyCheckerSnapshot<'db>) {
        self.table.commit(snapshot.table);
    }

    fn concrete_borrow_provider_for_effect_handle_ty(
        &self,
        ty: TyId<'db>,
    ) -> Option<ProviderAddressSpace> {
        let semantics = provider_semantics(self.db, self.env.scope(), self.env.assumptions(), ty);
        // Root-object defaults are binding-site semantics, not type-implied address spaces.
        (!matches!(semantics.kind, ProviderKind::RootObject)).then_some(semantics.address_space)?
    }

    fn concrete_borrow_provider_for_effect_field(
        &self,
        site: EffectParamSite<'db>,
        idx: usize,
    ) -> Option<ProviderAddressSpace> {
        self.env
            .resolved_provider_binding(site, idx)?
            .semantics
            .address_space
    }

    fn concrete_borrow_provider_for_binding(
        &self,
        binding: LocalBinding<'db>,
    ) -> Option<ProviderAddressSpace> {
        let binding_ty = self.env.lookup_binding_ty(&binding);
        match binding {
            LocalBinding::Local { pat, .. } => self.env.local_borrow_provider(pat),
            LocalBinding::EffectParam {
                site, provider_idx, ..
            } => self
                .env
                .provider_binding(site, provider_idx)
                .and_then(|provider| provider.semantics.address_space),
            LocalBinding::Param {
                site: ParamSite::EffectField(site),
                idx,
                ..
            } => self.concrete_borrow_provider_for_effect_field(site, idx),
            _ => None,
        }
        .or_else(|| {
            matches!(
                binding,
                LocalBinding::Local { .. } | LocalBinding::Param { .. }
            )
            .then(|| {
                binding_ty
                    .as_capability(self.db)
                    .map(|_| ProviderAddressSpace::Memory)
            })?
        })
    }

    fn concrete_borrow_provider_for_place(
        &self,
        place: &Place<'db>,
    ) -> Option<ProviderAddressSpace> {
        let PlaceBase::Binding(binding) = place.base;
        let binding_ty = normalize_ty(
            self.db,
            self.env.lookup_binding_ty(&binding),
            self.env.scope(),
            self.env.assumptions(),
        );
        let place_result_ty = normalize_ty(
            self.db,
            place
                .projections
                .last()
                .map_or(binding_ty, |projection| projection.result_ty()),
            self.env.scope(),
            self.env.assumptions(),
        );
        if binding_ty.as_capability(self.db).is_some() {
            return self.concrete_borrow_provider_for_binding(binding);
        }
        let binding_provider = match binding {
            LocalBinding::EffectParam { .. } => self.concrete_borrow_provider_for_binding(binding),
            LocalBinding::Param {
                site: ParamSite::EffectField(..),
                ..
            } => self.concrete_borrow_provider_for_binding(binding),
            LocalBinding::Local { .. } | LocalBinding::Param { .. } => None,
        };
        binding_provider
            .or_else(|| self.concrete_borrow_provider_for_effect_handle_ty(place_result_ty))
            .or_else(|| {
                matches!(
                    binding,
                    LocalBinding::Local { .. } | LocalBinding::Param { .. }
                )
                .then_some(ProviderAddressSpace::Memory)
            })
    }

    fn merge_concrete_borrow_providers(
        &mut self,
        previous_span: DynLazySpan<'db>,
        previous: Option<ProviderAddressSpace>,
        current_span: DynLazySpan<'db>,
        current: Option<ProviderAddressSpace>,
    ) -> Option<ProviderAddressSpace> {
        if let (Some(previous), Some(current)) = (previous, current)
            && previous != current
        {
            self.push_diag(BodyDiag::IncompatibleBorrowProviders {
                primary: current_span,
                previous: previous_span,
                previous_provider: previous,
                current_provider: current,
            });
        }

        previous
            .zip(current)
            .and_then(|(previous, current)| (previous == current).then_some(previous))
    }

    fn has_dead_inference_keys<T>(&self, value: &T) -> bool
    where
        T: TyVisitable<'db>,
    {
        inference_keys(self.db, value)
            .into_iter()
            .any(|key| key.0 as usize >= self.table.len())
    }

    pub(super) fn normalize_trait_goal(&mut self, goal: TraitInstId<'db>) -> TraitInstId<'db> {
        let db = self.db;
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();
        let goal = goal.fold_with(db, &mut self.table);
        let args: Vec<_> = goal
            .args(db)
            .iter()
            .copied()
            .map(|ty| normalize_ty(db, ty, scope, assumptions))
            .collect();
        let assoc_type_bindings: IndexMap<_, _> = goal
            .assoc_type_bindings(db)
            .iter()
            .map(|(&name, &ty)| (name, normalize_ty(db, ty, scope, assumptions)))
            .collect();
        TraitInstId::new(db, goal.def(db), args, assoc_type_bindings)
    }

    fn trait_goal_is_concrete_for_diagnostics(&self, goal: TraitInstId<'db>) -> bool {
        !collect_flags(self.db, goal).intersects(TyFlags::HAS_VAR | TyFlags::HAS_INVALID)
    }

    fn dedup_equivalent_trait_insts(&self, insts: Vec<TraitInstId<'db>>) -> Vec<TraitInstId<'db>> {
        let db = self.db;
        let mut seen = FxHashSet::default();
        let mut unique = Vec::new();
        for inst in insts {
            if seen.insert(Canonical::new(db, inst)) {
                unique.push(inst);
            }
        }
        unique
    }

    fn dedup_equivalent_pending_method_candidates(
        &self,
        candidates: Vec<env::PendingMethodCandidate<'db>>,
    ) -> Vec<env::PendingMethodCandidate<'db>> {
        let db = self.db;
        let mut seen = FxHashSet::default();
        let mut unique = Vec::new();
        for candidate in candidates {
            if seen.insert(Canonical::new(db, candidate.inst)) {
                unique.push(candidate);
            }
        }
        unique
    }

    fn call_constraint_diag_info(
        &self,
        callable_def: CallableDef<'db>,
        constraint_idx: usize,
    ) -> Option<CallConstraintDiagInfo<'db>> {
        Some(CallConstraintDiagInfo {
            callable_def,
            bound_span: self.call_constraint_bound_span(callable_def, constraint_idx)?,
        })
    }

    fn call_constraint_bound_span(
        &self,
        callable_def: CallableDef<'db>,
        constraint_idx: usize,
    ) -> Option<DynLazySpan<'db>> {
        let pairs = collect_func_decl_constraint_pairs(self.db, callable_def);
        let (_, source) = pairs.get(constraint_idx)?;
        Some(self.call_constraint_owner_bound_span(*source))
    }

    fn call_constraint_owner_bound_span(&self, bound: PredicateSource<'db>) -> DynLazySpan<'db> {
        match bound {
            PredicateSource::GenericParamBound {
                owner,
                param_idx,
                bound_idx,
            } => match owner {
                GenericParamOwner::Func(func) => func
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::Struct(struct_) => struct_
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::Enum(enum_) => enum_
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::TypeAlias(type_alias) => type_alias
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::Impl(impl_) => impl_
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::Trait(trait_) => trait_
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                GenericParamOwner::ImplTrait(impl_trait) => impl_trait
                    .span()
                    .generic_params()
                    .param(param_idx)
                    .into_type_param()
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
            },
            PredicateSource::WherePredicateBound {
                owner,
                pred_idx,
                bound_idx,
            } => match owner {
                WhereClauseOwner::Func(func) => func
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::Struct(struct_) => struct_
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::Enum(enum_) => enum_
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::Impl(impl_) => impl_
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::Trait(trait_) => trait_
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
                WhereClauseOwner::ImplTrait(impl_trait) => impl_trait
                    .span()
                    .where_clause()
                    .predicate(pred_idx)
                    .bounds()
                    .bound(bound_idx)
                    .trait_bound()
                    .into(),
            },
        }
    }

    fn process_trait_obligation(
        &mut self,
        mut obligation: env::TraitObligation<'db>,
        final_pass: bool,
    ) -> TraitObligationOutcome<'db> {
        let db = self.db;
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();

        if self.has_dead_inference_keys(&obligation.goal) {
            return TraitObligationOutcome::Discharged;
        }

        if final_pass {
            // Apply literal fallbacks (e.g. string type variables defaulting to
            // `String<N>`) before the last solving attempt. The typed body will
            // contain the fallback types anyway, and without this an
            // unsatisfied goal that mentions a literal variable would be
            // discharged as "not concrete" below, silently suppressing the
            // diagnostic and letting the error surface as an ICE in later
            // lowering stages.
            let mut prober = env::Prober::new(&mut self.table, scope);
            obligation.goal = obligation.goal.fold_with(db, &mut prober);
        }

        obligation.goal = self.normalize_trait_goal(obligation.goal);
        let goal = obligation.goal;
        let flags = collect_flags(db, goal);
        if flags.contains(TyFlags::HAS_INVALID) || self.has_dead_inference_keys(&goal) {
            return TraitObligationOutcome::Discharged;
        }

        let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
        let query = CanonicalGoalQuery::new(db, goal, assumptions);
        match is_goal_query_satisfiable(db, solve_cx, &query) {
            GoalSatisfiability::Satisfied(solution) => {
                if goal.self_ty(db).has_var(db) {
                    let outcome = if final_pass {
                        // Don't invent the self type at the end either; leave it alone
                        TraitObligationOutcome::Discharged
                    } else {
                        TraitObligationOutcome::Requeue(obligation)
                    };
                    return outcome;
                }
                let solved = query.extract_solution(&mut self.table, solution).inst;
                if self.has_dead_inference_keys(&solved) {
                    return TraitObligationOutcome::Discharged;
                }
                self.table.unify(goal, solved).unwrap();
                if self.normalize_trait_goal(goal) != goal {
                    TraitObligationOutcome::Progressed
                } else {
                    TraitObligationOutcome::Discharged
                }
            }
            GoalSatisfiability::NeedsConfirmation {
                solutions: ambiguous,
                completion,
            } => {
                // Answer identity includes the selected implementor, whereas
                // inference below deduplicates by the resulting trait instance.
                // If that proof-identity cutoff is reached with only one
                // distinct inferred instance, ask a fair fallback search
                // whether any different instance exists. `NotFound` is only
                // returned after saturation; resource/depth stops remain
                // incomplete and cannot justify committing inference.
                let first_projected = ambiguous
                    .first()
                    .map(|solution| Canonical::new(db, solution.value.inst));
                let fallback_proved_complete = completion.hit_root_answer_limit()
                    && first_projected.is_some_and(|first_projected| {
                        ambiguous.iter().all(|solution| {
                            Canonical::new(db, solution.value.inst) == first_projected
                        }) && goal_query_has_no_distinct_solution(
                            db,
                            solve_cx,
                            &query,
                            first_projected,
                        )
                    });
                let answers_complete_for_inference =
                    completion.is_saturated() || fallback_proved_complete;

                let mut candidates: Vec<_> = ambiguous
                    .iter()
                    .map(|solution| query.extract_solution(&mut self.table, *solution).inst)
                    .collect();
                candidates.retain(|candidate| !self.has_dead_inference_keys(candidate));
                let candidates = self.dedup_equivalent_trait_insts(candidates);

                if let [solution] = candidates.as_slice()
                    && answers_complete_for_inference
                    && !goal.self_ty(db).has_var(db)
                {
                    if self.table.unify(goal, *solution).is_ok()
                        && self.normalize_trait_goal(goal) != goal
                    {
                        TraitObligationOutcome::Progressed
                    } else {
                        TraitObligationOutcome::Discharged
                    }
                } else {
                    if final_pass && self.trait_goal_is_concrete_for_diagnostics(goal) {
                        let required_by = match obligation.origin {
                            env::TraitObligationOrigin::CallConstraint {
                                callable_def,
                                constraint_idx,
                                ..
                            } => self.call_constraint_diag_info(callable_def, constraint_idx),
                            env::TraitObligationOrigin::GenericConfirmation => None,
                        };
                        self.push_diag(BodyDiag::AmbiguousTraitInst {
                            primary: obligation.span.clone(),
                            cands: candidates.into_iter().collect(),
                            required_by,
                        });
                        return TraitObligationOutcome::Discharged;
                    }

                    if final_pass {
                        TraitObligationOutcome::Discharged
                    } else {
                        TraitObligationOutcome::Requeue(obligation)
                    }
                }
            }
            GoalSatisfiability::UnSat(subgoal) => {
                if final_pass && self.trait_goal_is_concrete_for_diagnostics(goal) {
                    let required_by = match obligation.origin {
                        env::TraitObligationOrigin::CallConstraint {
                            callable_def,
                            constraint_idx,
                            ..
                        } => self.call_constraint_diag_info(callable_def, constraint_idx),
                        env::TraitObligationOrigin::GenericConfirmation => None,
                    };
                    let unsat = subgoal.map(|goal| query.extract_subgoal(&mut self.table, goal));
                    let call_bound = match obligation.origin {
                        env::TraitObligationOrigin::CallConstraint {
                            callable_def,
                            constraint_idx,
                            ..
                        } => Some((callable_def, constraint_idx)),
                        env::TraitObligationOrigin::GenericConfirmation => None,
                    };
                    let capability_hint =
                        capability_only_unsat_ty(db, solve_cx, goal, call_bound, unsat);
                    self.push_diag(TyDiagCollection::from(
                        TraitConstraintDiag::TraitBoundNotSat {
                            span: obligation.span.clone(),
                            primary_goal: goal,
                            unsat_subgoal: unsat,
                            required_by,
                            capability_hint,
                        },
                    ));
                    TraitObligationOutcome::Discharged
                } else if final_pass {
                    TraitObligationOutcome::Discharged
                } else {
                    TraitObligationOutcome::Requeue(obligation)
                }
            }
            GoalSatisfiability::ContainsInvalid => TraitObligationOutcome::Discharged,
        }
    }

    fn process_type_equality(&mut self, mut equality: env::TypeEquality<'db>) -> bool {
        if self.final_pass {
            let mut prober = env::Prober::new(&mut self.table, self.env.scope());
            equality.actual = equality.actual.fold_with(self.db, &mut prober);
            equality.expected = equality.expected.fold_with(self.db, &mut prober);
        }
        equality.actual = self.normalize_ty(equality.actual);
        equality.expected = self.normalize_ty(equality.expected);
        if let Some(expr) = equality.expr
            && let Some(coerced) = self.try_coerce_capability_for_expr_to_expected(
                expr,
                equality.actual,
                equality.expected,
            )
        {
            equality.actual = coerced;
        }
        match self.table.unify(equality.actual, equality.expected) {
            Ok(()) => true,
            Err(UnificationError::TypeMismatch) => !self.retain_or_report_mismatch(equality),
            Err(UnificationError::OccursCheckFailed) => {
                self.push_diag(BodyDiag::InfiniteOccurrence(equality.span));
                true
            }
        }
    }

    /// Retains a mismatched equality while a projection or const computation
    /// in it still waits on inference, to decide it once other constraints
    /// and literal fallback settle, and otherwise reports the mismatch.
    /// Returns whether the equality was retained.
    fn retain_or_report_mismatch(&mut self, equality: env::TypeEquality<'db>) -> bool {
        if !self.final_pass
            && (depends_on_type_inference(self.db, equality.actual)
                || depends_on_type_inference(self.db, equality.expected))
        {
            self.env.register_type_equality(equality);
            return true;
        }
        if !equality.actual.has_invalid(self.db) && !equality.expected.has_invalid(self.db) {
            self.push_diag(BodyDiag::TypeMismatch {
                span: equality.span,
                expected: equality.expected,
                given: equality.actual,
            });
        }
        false
    }

    fn resolve_deferred(&mut self) {
        let db = self.db;
        let body = self.env.body();
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();

        enum Viability {
            Viable,
            Incompatible,
            ReturnTypeMismatch,
        }

        let check_viability =
            |this: &mut Self,
             pending: &env::PendingMethod<'db>,
             expr_ty: TyId<'db>,
             receiver: ExprId,
             generic_args: crate::hir_def::GenericArgListId<'db>,
             call_args: &[crate::hir_def::CallArg<'db>],
             candidate: env::PendingMethodCandidate<'db>| {
                let snap = this.snapshot_state();

                let viability = (|| {
                    let inst = candidate.inst;
                    let recv_ty = {
                        let mut prober = env::Prober::new(&mut this.table, scope);
                        pending.recv_ty.fold_with(db, &mut prober)
                    };

                    let inst_self = this.table.instantiate_to_term(inst.self_ty(db));
                    let recv_ty = if this.table.unify(inst_self, recv_ty).is_ok() {
                        recv_ty
                    } else {
                        let Some((_, recv_inner)) = recv_ty.as_capability(db) else {
                            return Viability::Incompatible;
                        };
                        if this.table.unify(inst_self, recv_inner).is_err() {
                            return Viability::Incompatible;
                        }
                        recv_inner
                    };

                    let Ok(func_ty) = try_instantiate_trait_method(
                        db,
                        candidate.method,
                        &mut this.table,
                        recv_ty,
                        inst,
                    ) else {
                        return Viability::Incompatible;
                    };
                    let func_ty = this.table.instantiate_to_term(func_ty);
                    let Ok(mut callable) =
                        Callable::new(db, func_ty, receiver.span(body).into(), Some(inst))
                    else {
                        return Viability::Incompatible;
                    };

                    let expected_arity = callable.callable_def.arg_tys(db).len();
                    let given_arity = call_args.len() + 1;
                    if expected_arity != given_arity {
                        return Viability::Incompatible;
                    }

                    match unify_explicit_call_generic_args(
                        &mut callable,
                        this,
                        generic_args,
                        HoleAnchor::BodySyntax {
                            body,
                            site: BodyHoleSite::Expr(pending.expr),
                        },
                        CallGenericArgPhase::Probe,
                        |this, _, given, current| this.table.unify(given, *current).is_ok(),
                    ) {
                        Ok(()) => {}
                        Err(CallGenericArgUnifyError::ArityMismatch { .. })
                        | Err(CallGenericArgUnifyError::UnificationFailed)
                        | Err(CallGenericArgUnifyError::InvalidArgument(_)) => {
                            return Viability::Incompatible;
                        }
                    }

                    let Some(receiver_prop) = this.env.typed_expr(receiver) else {
                        return Viability::Incompatible;
                    };
                    let mut all_arg_tys = Vec::with_capacity(call_args.len() + 1);
                    all_arg_tys.push(receiver_prop.ty);
                    for arg in call_args.iter() {
                        let Some(prop) = this.env.typed_expr(arg.expr) else {
                            return Viability::Incompatible;
                        };
                        all_arg_tys.push(prop.ty);
                    }

                    for (&given, expected) in all_arg_tys
                        .iter()
                        .zip(callable.callable_def.arg_tys(db).iter())
                    {
                        let mut expected = expected.instantiate(db, callable.generic_args());
                        if let Some(inst) = callable.trait_inst() {
                            expected = normalize_with_trait_evidence(db, expected, scope, inst);
                        }
                        let expected = normalize_ty(
                            db,
                            expected.fold_with(db, &mut this.table),
                            scope,
                            assumptions,
                        );
                        let given = normalize_ty(
                            db,
                            given.fold_with(db, &mut this.table),
                            scope,
                            assumptions,
                        );
                        let given = this
                            .try_coerce_capability_to_expected(given, expected)
                            .unwrap_or(given);
                        if this.table.unify(given, expected).is_err() {
                            return Viability::Incompatible;
                        }
                    }

                    let method_name_span = pending
                        .expr
                        .span(body)
                        .into_method_call_expr()
                        .method_name()
                        .into();
                    callable.process_constraints(this, pending.expr, method_name_span);

                    let ret_ty = normalize_ty(
                        db,
                        callable.ret_ty(db).fold_with(db, &mut this.table),
                        scope,
                        assumptions,
                    );

                    if this.table.unify(expr_ty, ret_ty).is_ok() {
                        Viability::Viable
                    } else {
                        Viability::ReturnTypeMismatch
                    }
                })();
                this.rollback_state(snap);
                viability
            };

        // Fixed-point pass over deferred tasks.
        let mut progressed = true;
        while progressed {
            progressed = false;
            let tasks = self.env.take_deferred_tasks();
            for task in tasks {
                match task {
                    env::DeferredTask::TypeEquality(equality) => {
                        progressed |= self.process_type_equality(equality);
                    }
                    env::DeferredTask::Obligation(obligation) => {
                        match self.process_trait_obligation(obligation, false) {
                            TraitObligationOutcome::Discharged => {}
                            TraitObligationOutcome::Progressed => progressed = true,
                            TraitObligationOutcome::Requeue(obligation) => {
                                self.env.register_trait_obligation(obligation);
                            }
                        }
                    }
                    env::DeferredTask::Method(pending) => {
                        let (receiver, generic_args, call_args) = match pending.expr.data(db, body)
                        {
                            Partial::Present(Expr::MethodCall(receiver, _, generic_args, args)) => {
                                (*receiver, *generic_args, args.as_slice())
                            }
                            _ => continue,
                        };

                        let Some(expr_prop) = self.env.typed_expr(pending.expr) else {
                            continue;
                        };
                        let expr_ty = {
                            let mut prober = env::Prober::new(&mut self.table, scope);
                            expr_prop.ty.fold_with(db, &mut prober)
                        };
                        if expr_ty.has_invalid(db) {
                            self.env.register_pending_method(pending);
                            continue;
                        }

                        let (strict, soft): (Vec<_>, Vec<_>) = pending
                            .candidates
                            .iter()
                            .copied()
                            .filter_map(|candidate| {
                                let viability = check_viability(
                                    self,
                                    &pending,
                                    expr_ty,
                                    receiver,
                                    generic_args,
                                    call_args,
                                    candidate,
                                );
                                if matches!(viability, Viability::Incompatible) {
                                    None
                                } else {
                                    Some((candidate, viability))
                                }
                            })
                            .partition(|(_, viability)| matches!(viability, Viability::Viable));
                        let viable = if strict.is_empty() {
                            soft.into_iter().map(|(candidate, _)| candidate).collect()
                        } else {
                            strict.into_iter().map(|(candidate, _)| candidate).collect()
                        };
                        let viable = self.dedup_equivalent_pending_method_candidates(viable);

                        if let [candidate] = viable.as_slice() {
                            if self.env.callable_expr(pending.expr).is_none() {
                                let call_span = pending.expr.span(body).into_method_call_expr();
                                let inst = candidate.inst;

                                let receiver_prop = self
                                    .env
                                    .typed_expr(receiver)
                                    .unwrap_or_else(|| ExprProp::invalid(db));
                                let recv_ty = {
                                    let mut prober = env::Prober::new(&mut self.table, scope);
                                    pending.recv_ty.fold_with(db, &mut prober)
                                };

                                let func_ty = self.instantiate_trait_method_to_term(
                                    candidate.method,
                                    recv_ty,
                                    inst,
                                );

                                let mut callable = match Callable::new(
                                    db,
                                    func_ty,
                                    receiver.span(body).into(),
                                    Some(inst),
                                ) {
                                    Ok(callable) => callable,
                                    Err(diag) => {
                                        self.push_diag(diag);
                                        progressed = true;
                                        continue;
                                    }
                                };

                                if candidate.needs_confirmation {
                                    self.env.register_trait_obligation(env::TraitObligation {
                                        goal: inst,
                                        origin: env::TraitObligationOrigin::GenericConfirmation,
                                        span: call_span.clone().into(),
                                    });
                                }

                                if !callable.unify_generic_args(
                                    self,
                                    generic_args,
                                    HoleAnchor::BodySyntax {
                                        body,
                                        site: BodyHoleSite::Expr(pending.expr),
                                    },
                                    call_span.clone().generic_args(),
                                ) {
                                    progressed = true;
                                    continue;
                                }

                                callable.check_args(
                                    self,
                                    call_args,
                                    call_span.clone().args(),
                                    Some((receiver, receiver_prop)),
                                    true,
                                );

                                callable.process_constraints(
                                    self,
                                    pending.expr,
                                    call_span.method_name().into(),
                                );

                                let ret_ty = self.normalize_ty(callable.ret_ty(db));
                                match self.table.unify(expr_prop.ty, ret_ty) {
                                    Ok(()) => {}
                                    Err(UnificationError::TypeMismatch) => {
                                        self.push_diag(BodyDiag::TypeMismatch {
                                            span: pending.expr.span(body).into(),
                                            expected: expr_prop.ty,
                                            given: ret_ty,
                                        });
                                        continue;
                                    }
                                    Err(UnificationError::OccursCheckFailed) => {
                                        let span = pending.expr.span(body).into();
                                        self.push_diag(BodyDiag::InfiniteOccurrence(span));
                                        continue;
                                    }
                                }

                                self.specialize_callable_layout_args(
                                    &mut callable,
                                    Some(receiver),
                                    call_args,
                                );

                                self.check_callable_effects(pending.expr, &mut callable);

                                if let Some(kind) =
                                    self.code_region_method_kind(recv_ty, pending.method_name)
                                    && call_args.len() == 1
                                    && self.env.typed_expr(call_args[0].expr).is_some_and(|prop| {
                                        ty_may_be_code_region_token(
                                            db,
                                            normalize_ty(db, prop.ty, scope, assumptions),
                                        )
                                    })
                                {
                                    self.env.register_code_region_intrinsic(
                                        pending.expr,
                                        callable,
                                        call_args[0].expr,
                                        kind,
                                    );
                                } else {
                                    self.env.register_semantic_call(pending.expr, callable);
                                }
                            }

                            progressed = true;
                        } else {
                            self.env.register_pending_method(pending);
                        }
                    }
                    env::DeferredTask::PrimitiveOp(pending) => {
                        match self.resolve_pending_primitive_op(&pending) {
                            expr::PendingPrimitiveOpResolution::Pending => {
                                self.env.register_pending_primitive_op(pending);
                            }
                            expr::PendingPrimitiveOpResolution::Resolved => {
                                progressed = true;
                            }
                            expr::PendingPrimitiveOpResolution::Done => {}
                        }
                    }
                }
            }
        }

        // Final trait solving can still bind types after literal fallback.
        // Check retained equalities after those constraints have settled.
        self.final_pass = true;
        let mut equalities = Vec::new();
        for task in self.env.take_deferred_tasks() {
            match task {
                env::DeferredTask::TypeEquality(equality) => {
                    equalities.push(equality);
                }
                env::DeferredTask::Obligation(obligation) => {
                    let _ = self.process_trait_obligation(obligation, true);
                }
                env::DeferredTask::Method(pending) => {
                    let Some(expr_prop) = self.env.typed_expr(pending.expr) else {
                        continue;
                    };
                    let expr_ty = {
                        let mut prober = env::Prober::new(&mut self.table, scope);
                        expr_prop.ty.fold_with(db, &mut prober)
                    };
                    if expr_ty.has_invalid(db) {
                        continue;
                    }

                    let (receiver, generic_args, call_args) = match pending.expr.data(db, body) {
                        Partial::Present(Expr::MethodCall(receiver, _, generic_args, args)) => {
                            (*receiver, *generic_args, args.as_slice())
                        }
                        _ => continue,
                    };

                    let viable: Vec<_> = pending
                        .candidates
                        .iter()
                        .copied()
                        .filter(|&candidate| {
                            let viability = check_viability(
                                self,
                                &pending,
                                expr_ty,
                                receiver,
                                generic_args,
                                call_args,
                                candidate,
                            );
                            !matches!(viability, Viability::Incompatible)
                        })
                        .collect();
                    let viable = self.dedup_equivalent_pending_method_candidates(viable);
                    if viable.len() > 1 {
                        self.push_diag(BodyDiag::AmbiguousTrait {
                            primary: pending.span.clone(),
                            method_name: pending.method_name,
                            traits: viable.into_iter().map(|candidate| candidate.inst).collect(),
                        });
                    }
                }
                env::DeferredTask::PrimitiveOp(pending) => {
                    let _ = self.resolve_pending_primitive_op(&pending);
                }
            }
        }
        for equality in equalities {
            self.process_type_equality(equality);
        }
    }

    fn new_internal(db: &'db dyn HirAnalysisDb, env: TyCheckEnv<'db>, expected: TyId<'db>) -> Self {
        let table = UnificationTable::new(db);
        Self {
            db,
            env,
            table,
            expected,
            effect_provider_keys: FxHashSet::default(),
            first_return_borrow_provider: None,
            diags: Vec::new(),
            final_pass: false,
        }
    }

    /// Resolves the pattern type and return type for a recv arm.
    /// Returns (pattern_type, return_type).
    fn resolve_recv_arm_types(
        &mut self,
        contract: Contract<'db>,
        msg_path: Option<PathId<'db>>,
        arm: ContractRecvArm<'db>,
        path_span: LazyPathSpan<'db>,
        arm_span: crate::span::item::LazyRecvArmSpan<'db>,
    ) -> (TyId<'db>, TyId<'db>) {
        let invalid_ty = TyId::invalid(self.db, InvalidCause::Other);

        if arm.is_fallback(self.db) {
            return (TyId::unit(self.db), TyId::unit(self.db));
        }

        // Get variant path from arm pattern
        let Some(variant_path) = arm.variant_path(self.db) else {
            return (invalid_ty, invalid_ty);
        };

        let assumptions = self.env.assumptions();

        // Resolve based on whether this is a named or bare recv block
        let resolved = if let Some(msg_mod) = contract::resolve_recv_msg_mod(
            self.db,
            contract,
            msg_path,
            path_span,
            &mut self.diags,
            false,
        ) {
            // Named recv block - resolve within the msg module
            match contract::resolve_variant_in_msg(self.db, msg_mod, variant_path, assumptions) {
                Ok(resolved) => resolved,
                _ => {
                    // Return invalid types to suppress spurious type mismatch errors
                    // when the pattern doesn't resolve to a valid msg variant
                    return (invalid_ty, invalid_ty);
                }
            }
        } else if msg_path.is_none() {
            // Bare recv block - resolve from contract scope
            match contract::resolve_variant_bare(self.db, contract, variant_path, assumptions) {
                Ok(resolved) => resolved,
                _ => {
                    // Return invalid types to suppress spurious type mismatch errors
                    return (invalid_ty, invalid_ty);
                }
            }
        } else {
            // msg_path was Some but didn't resolve - diagnostics already emitted
            return (invalid_ty, invalid_ty);
        };

        let pat_ty = resolved.ty;

        // Get annotated return type from the arm
        let arm_ret_span = arm_span.clone().ret_ty();
        let annotated = arm
            .ret_ty
            .map(|hir_ty| self.lower_ty(hir_ty, arm_ret_span.clone(), true));
        let variant_ret = contract::get_msg_variant_return_type(self.db, pat_ty, self.env.scope());

        let ret_ty = match (variant_ret, annotated) {
            (Some(var_ty), Some(annot_ty)) => {
                self.equate_ty(annot_ty, var_ty, arm_ret_span.into());
                var_ty
            }
            (Some(var_ty), None) => {
                // Only require annotation if return type is not unit
                if var_ty != TyId::unit(self.db) {
                    self.push_diag(BodyDiag::RecvArmRetTypeMissing {
                        primary: arm_span.pat().into(),
                        expected: var_ty,
                    });
                }
                var_ty
            }
            (None, Some(annot_ty)) => annot_ty,
            (None, None) => TyId::unit(self.db),
        };

        (pat_ty, ret_ty)
    }

    fn push_diag(&mut self, diag: impl Into<FuncBodyDiag<'db>>) {
        self.diags.push(diag.into())
    }

    fn body(&self) -> Body<'db> {
        self.env.body()
    }

    fn ty_is_copy(&self, ty: TyId<'db>) -> bool {
        crate::analysis::ty::ty_is_copy(self.db, self.env.scope(), ty, self.env.assumptions())
    }

    fn copy_inner_from_borrow(&self, ty: TyId<'db>) -> Option<TyId<'db>> {
        let (_, inner) = ty.as_capability(self.db)?;
        self.ty_is_copy(inner).then_some(inner)
    }

    fn ty_unifies(&mut self, lhs: TyId<'db>, rhs: TyId<'db>) -> bool {
        let snapshot = self.snapshot_state();
        let unifies = self.table.unify(lhs, rhs).is_ok();
        self.rollback_state(snapshot);
        unifies
    }

    fn binding_interface_shape(
        &mut self,
        binding: LocalBinding<'db>,
    ) -> BindingInterfaceShape<'db> {
        let ty = self.normalize_ty(self.env.lookup_binding_ty(&binding));
        if let Some((_, value_ty)) = ty.as_capability(self.db) {
            return BindingInterfaceShape::PlaceCarrier {
                value_ty: self.normalize_ty(value_ty),
            };
        }
        let provider = match binding {
            LocalBinding::EffectParam {
                site, provider_idx, ..
            } => self.env.provider_binding(site, provider_idx),
            LocalBinding::Param {
                site: ParamSite::EffectField(site),
                idx,
                ..
            } => self.env.resolved_provider_binding(site, idx),
            LocalBinding::Local { .. } | LocalBinding::Param { .. } => None,
        };
        let semantics = provider_semantics(self.db, self.env.scope(), self.env.assumptions(), ty);
        if let Some(target_ty) = semantics.binding_target_ty(self.db, provider.is_some()) {
            return BindingInterfaceShape::DirectCarrier {
                target_ty: self.normalize_ty(target_ty),
            };
        }
        provider.map_or(BindingInterfaceShape::OrdinaryValue, |provider| {
            BindingInterfaceShape::ProviderValue {
                kind: provider.semantics.kind,
                value_ty: ty,
            }
        })
    }

    fn binding_path_read_semantics(
        &mut self,
        binding: LocalBinding<'db>,
        expr_ty: TyId<'db>,
    ) -> PathReadSemantics {
        let expr_ty = self.normalize_ty(expr_ty);
        let binding_ty = self.normalize_ty(self.env.lookup_binding_ty(&binding));
        if binding_ty == expr_ty {
            return PathReadSemantics::ReuseLocal;
        }

        match self.binding_interface_shape(binding) {
            BindingInterfaceShape::OrdinaryValue => PathReadSemantics::MaterializeValue,
            BindingInterfaceShape::ProviderValue { .. }
            | BindingInterfaceShape::DirectCarrier { .. } => PathReadSemantics::ForwardInterface,
            BindingInterfaceShape::PlaceCarrier { .. } => expr_ty
                .as_capability(self.db)
                .map_or(PathReadSemantics::MaterializeValue, |_| {
                    PathReadSemantics::ForwardInterface
                }),
        }
    }

    /// Contextual capability coercion:
    /// - `mut T -> ref T`
    /// - `mut/ref/view T -> view T`
    /// - `mut/ref/view T -> T` when `T: Copy`
    /// - `T -> view T`
    fn try_coerce_capability_to_expected(
        &mut self,
        actual: TyId<'db>,
        expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        self.try_coerce_capability_expr_to_expected(None, actual, expected)
    }

    fn try_coerce_capability_for_expr_to_expected(
        &mut self,
        expr: ExprId,
        actual: TyId<'db>,
        expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        self.try_coerce_capability_expr_to_expected(Some(expr), actual, expected)
    }

    fn try_coerce_capability_expr_to_expected(
        &mut self,
        expr: Option<ExprId>,
        actual: TyId<'db>,
        expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        if expected.is_ty_var(self.db) {
            let actual = self.normalize_ty(actual);
            if let Some((CapabilityKind::View, inner)) = actual.as_capability(self.db)
                && self.ty_is_copy(inner)
            {
                return Some(inner);
            }
            return None;
        }

        let actual = self.normalize_ty(actual);
        let expected = self.normalize_ty(expected);
        if actual.has_invalid(self.db) || expected.has_invalid(self.db) {
            return None;
        }

        let actual_cap = actual.as_capability(self.db);
        let expected_cap = expected.as_capability(self.db);

        match (actual_cap, expected_cap) {
            (Some((given_kind, given_inner)), Some((required_kind, required_inner))) => {
                if given_kind.rank() < required_kind.rank() {
                    return None;
                }
                if !self.ty_unifies(given_inner, required_inner) {
                    return None;
                }
                let coerced = match required_kind {
                    CapabilityKind::Mut => TyId::borrow_mut_of(self.db, given_inner),
                    CapabilityKind::Ref => TyId::borrow_ref_of(self.db, given_inner),
                    CapabilityKind::View => TyId::view_of(self.db, given_inner),
                };
                Some(coerced)
            }
            (Some((given_kind, given_inner)), None) => {
                if !self.ty_unifies(given_inner, expected) {
                    return None;
                }

                if self.ty_is_copy(given_inner)
                    || (matches!(given_kind, CapabilityKind::View)
                        && expr.is_some_and(|expr| self.expr_can_move_from_place(expr)))
                {
                    return Some(given_inner);
                }

                None
            }
            (None, Some((CapabilityKind::View, required_inner))) => {
                if !self.ty_unifies(actual, required_inner) {
                    return None;
                }
                Some(TyId::view_of(self.db, actual))
            }
            (None, Some((CapabilityKind::Ref | CapabilityKind::Mut, _))) | (None, None) => None,
        }
    }

    fn expr_can_move_from_place(&self, expr: ExprId) -> bool {
        let Partial::Present(expr_data) = expr.data(self.db, self.body()) else {
            return false;
        };

        match expr_data {
            Expr::Path(_) => true,
            Expr::Field(lhs, _) => self.expr_can_move_from_place(*lhs),
            Expr::Bin(lhs, _, crate::hir_def::expr::BinOp::Index) => {
                self.expr_can_move_from_place(*lhs)
            }
            _ => false,
        }
    }

    /// In "owned" contexts, non-`Copy` values are implicitly moved from places.
    ///
    /// `Copy` values may be duplicated implicitly.
    fn record_implicit_move_for_owned_expr(&mut self, expr: ExprId, ty: TyId<'db>) {
        self.record_implicit_move_for_owned_expr_inner(expr, Some(ty));
    }

    fn record_implicit_move_for_owned_expr_inner(
        &mut self,
        expr: ExprId,
        expected_ty: Option<TyId<'db>>,
    ) {
        let db = self.db;
        let body = self.body();
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return;
        };

        match expr_data {
            Expr::Block(stmts) => {
                let Some(last) = stmts.last() else {
                    return;
                };
                let Partial::Present(stmt) = last.data(db, body) else {
                    return;
                };
                let crate::hir_def::Stmt::Expr(tail) = stmt else {
                    return;
                };
                self.record_implicit_move_for_owned_expr_inner(*tail, expected_ty);
            }
            Expr::With(_, body_expr) | Expr::Cast(body_expr, _) | Expr::If(_, body_expr, None) => {
                self.record_implicit_move_for_owned_expr_inner(*body_expr, expected_ty);
            }
            Expr::If(_, then_expr, Some(else_expr)) => {
                self.record_implicit_move_for_owned_expr_inner(*then_expr, expected_ty);
                self.record_implicit_move_for_owned_expr_inner(*else_expr, expected_ty);
            }
            Expr::Match(_, arms) => {
                let Partial::Present(arms) = arms else {
                    return;
                };
                for arm in arms {
                    self.record_implicit_move_for_owned_expr_inner(arm.body, expected_ty);
                }
            }
            _ => {
                if !self.is_assignable_expr(expr) {
                    return;
                }

                let Some(prop) = self.env.typed_expr(expr) else {
                    return;
                };
                let expr_ty = prop.ty.fold_with(self.db, &mut self.table);
                let expr_ty = self.normalize_ty(expr_ty);
                if expr_ty.has_invalid(self.db)
                    || expr_ty == TyId::unit(self.db)
                    || expr_ty.is_never(self.db)
                {
                    return;
                }

                let expected_ty = expected_ty
                    .map(|ty| self.normalize_ty(ty))
                    .filter(|ty| {
                        !ty.has_invalid(self.db)
                            && *ty != TyId::unit(self.db)
                            && !ty.is_never(self.db)
                    })
                    .unwrap_or(expr_ty);

                if self.ty_is_copy(expected_ty) {
                    return;
                }

                self.env.record_implicit_move(expr, expected_ty);
            }
        }
    }

    fn lit_ty(&mut self, lit: &LitKind<'db>) -> TyId<'db> {
        match lit {
            LitKind::Bool(_) => TyId::bool(self.db),
            LitKind::Int(_) => self.table.new_var(TyVarSort::Integral, &Kind::Star),
            LitKind::String(s) => {
                let len_bytes = s.len_bytes(self.db);
                if len_bytes > MAX_INLINE_STRING_BYTES {
                    return TyId::invalid(
                        self.db,
                        InvalidCause::StringTooLarge {
                            max: MAX_INLINE_STRING_BYTES,
                            given: len_bytes,
                        },
                    );
                }
                self.table.new_var(
                    TyVarSort::String {
                        min_len: len_bytes,
                        fallback: self.string_literal_fallback(),
                    },
                    &Kind::Star,
                )
            }
        }
    }

    pub(crate) fn string_literal_ty(
        &mut self,
        string_id: StringId<'db>,
        expected: TyId<'db>,
    ) -> TyId<'db> {
        if self.string_literal_should_use_byte_array(expected) {
            return self.string_literal_byte_array_ty(string_id.len_bytes(self.db));
        }
        if expected.is_core_dyn_string(self.db) {
            return expected;
        }

        let len_bytes = string_id.len_bytes(self.db);
        self.table.new_var(
            TyVarSort::String {
                min_len: len_bytes,
                fallback: self.string_literal_fallback(),
            },
            &Kind::Star,
        )
    }

    pub(crate) fn string_literal_byte_array_ty(&self, len_bytes: usize) -> TyId<'db> {
        let u8_ty = TyId::new(
            self.db,
            TyData::TyBase(TyBase::Prim(crate::analysis::ty::ty_def::PrimTy::U8)),
        );
        TyId::array_with_len(self.db, u8_ty, len_bytes)
    }

    pub(crate) fn string_literal_should_use_byte_array(&self, expected: TyId<'db>) -> bool {
        let expected = normalize_ty(self.db, expected, self.env.scope(), self.env.assumptions());
        let (base, args) = expected.decompose_ty_app(self.db);
        matches!(
            base.data(self.db),
            TyData::TyBase(TyBase::Prim(crate::analysis::ty::ty_def::PrimTy::Array))
        ) && args.len() == 2
            && matches!(
                args[0].data(self.db),
                TyData::TyBase(TyBase::Prim(crate::analysis::ty::ty_def::PrimTy::U8))
            )
    }

    fn lower_ty(
        &mut self,
        hir_ty: HirTyId<'db>,
        span: LazyTySpan<'db>,
        star_kind_required: bool,
    ) -> TyId<'db> {
        let ty = lower_hir_ty(self.db, hir_ty, self.env.scope(), self.env.assumptions());

        // If lowering failed, try to produce precise diagnostics (e.g., path resolution errors)
        if ty.has_invalid(self.db) {
            let diags = collect_ty_lower_errors(
                self.db,
                self.env.scope(),
                hir_ty,
                span.clone(),
                self.env.assumptions(),
            );
            if !diags.is_empty() {
                for d in diags {
                    self.push_diag(d);
                }
                // Avoid cascading kind errors for already-invalid types
                return TyId::invalid(self.db, InvalidCause::Other);
            }
        }

        // A qualified path resolves to a type that no longer mentions the
        // types written inside it, so check those where they are written.
        for diag in qualified_path_wf_diags(
            self.db,
            self.env.scope(),
            hir_ty,
            span.clone(),
            self.env.assumptions(),
            TraitSolveCx::new(self.db, self.env.scope()).with_assumptions(self.env.assumptions()),
        ) {
            self.push_diag(diag);
        }

        if let Some(diag) = ty.emit_diag(self.db, span.clone().into()).or_else(|| {
            normalization_limit_diag(
                self.db,
                ty,
                self.env.scope(),
                self.env.assumptions(),
                span.clone().into(),
            )
        }) {
            self.push_diag(diag)
        }

        if ty_contains_const_hole(self.db, ty) {
            self.push_diag(TyDiagCollection::from(
                TyLowerDiag::ConstHoleInValuePosition {
                    span: span.clone().into(),
                    ty,
                },
            ));
            return TyId::invalid(self.db, InvalidCause::Other);
        }

        if star_kind_required && ty.is_star_kind(self.db) {
            ty
        } else {
            let diag: TyDiagCollection = TyLowerDiag::ExpectedStarKind(span.into()).into();
            self.push_diag(diag);
            TyId::invalid(self.db, InvalidCause::Other)
        }
    }

    /// Returns the fresh type variable for pattern and expr type checking. The
    /// kind of the type variable is `*`, and the sort is `General`.
    fn fresh_ty(&mut self) -> TyId<'db> {
        self.table.new_var(TyVarSort::General, &Kind::Star)
    }

    fn fresh_tys_n(&mut self, n: usize) -> Vec<TyId<'db>> {
        (0..n).map(|_| self.fresh_ty()).collect()
    }

    fn capability_fallback_candidates(&self, ty: TyId<'db>) -> Vec<TyId<'db>> {
        let mut candidates = vec![ty];
        if let TyData::TyVar(var) = ty.base_ty(self.db).data(self.db)
            && matches!(var.sort, TyVarSort::String { .. })
            && let Some(text_ty) =
                resolve_lib_type_path(self.db, self.env.scope(), "core::abi::DynString")
        {
            candidates.push(text_ty);
            candidates.push(TyId::view_of(self.db, text_ty));
        }
        if let Some((cap, inner)) = ty.as_capability(self.db) {
            if matches!(cap, CapabilityKind::Mut) {
                candidates.push(TyId::borrow_ref_of(self.db, inner));
            }
            if !matches!(cap, CapabilityKind::View) {
                candidates.push(TyId::view_of(self.db, inner));
            }
            candidates.push(inner);
        } else {
            candidates.push(TyId::view_of(self.db, ty));
        }
        candidates.dedup();
        candidates
    }

    fn pattern_binds_any(&self, pat: PatId) -> bool {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return false;
        };
        match pat_data {
            Pat::WildCard | Pat::Rest | Pat::Lit(_) => false,
            Pat::Path(..) => self
                .env
                .pat_binding(pat)
                .is_some_and(|binding| matches!(binding, LocalBinding::Local { .. })),
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                pats.iter().any(|pat| self.pattern_binds_any(*pat))
            }
            Pat::Record(_, fields) => fields.iter().any(|field| self.pattern_binds_any(field.pat)),
            Pat::Or(lhs, rhs) => self.pattern_binds_any(*lhs) || self.pattern_binds_any(*rhs),
        }
    }

    fn destructure_source_mode(&self, ty: TyId<'db>) -> (TyId<'db>, PatternDestructureMode) {
        destructure_pattern_source(self.db, ty)
    }

    fn retype_pattern_bindings_for_borrow(&mut self, pat: PatId, kind: BorrowKind) {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return;
        };
        match pat_data {
            Pat::Path(..) => {
                let Some(binding) = self.env.pat_binding(pat) else {
                    return;
                };
                if !matches!(binding, LocalBinding::Local { .. }) {
                    return;
                }
                self.env.set_pat_binding_mode(pat, PatBindingMode::ByBorrow);
                let inner = self.env.lookup_binding_ty(&binding);
                if inner.has_invalid(self.db) || inner.as_capability(self.db).is_some() {
                    return;
                }
                let borrow_ty =
                    apply_pattern_borrow_mode(self.db, PatternDestructureMode::Borrow(kind), inner);
                self.env.type_pat(pat, borrow_ty);
            }
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                for pat in pats {
                    self.retype_pattern_bindings_for_borrow(*pat, kind);
                }
            }
            Pat::Record(_, fields) => {
                for field in fields {
                    self.retype_pattern_bindings_for_borrow(field.pat, kind);
                }
            }
            Pat::Or(lhs, rhs) => {
                self.retype_pattern_bindings_for_borrow(*lhs, kind);
                self.retype_pattern_bindings_for_borrow(*rhs, kind);
            }
            Pat::WildCard | Pat::Rest | Pat::Lit(..) => {}
        }
    }

    /// Ensure all binding patterns are registered in the current scope.
    fn seed_pat_bindings(&mut self, pat: PatId) {
        let Partial::Present(pat_data) = pat.data(self.db, self.env.body()) else {
            return;
        };

        match pat_data {
            Pat::Path(path, is_mut) => {
                let Partial::Present(path) = path else {
                    return;
                };
                let Some(LocalBinding::Local { .. }) = self.env.pat_binding(pat) else {
                    return;
                };
                if let Some(ident) = path.as_ident(self.db) {
                    let current = self.env.current_block_idx();
                    if self.env.get_block(current).lookup_var(ident).is_none() {
                        let binding = LocalBinding::local(pat, *is_mut);
                        self.env.register_pending_binding(ident, binding);
                    }
                }
            }
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                for &p in pats {
                    self.seed_pat_bindings(p);
                }
            }
            Pat::Record(_, fields) => {
                for field in fields {
                    self.seed_pat_bindings(field.pat);
                }
            }
            Pat::Or(lhs, rhs) => {
                self.seed_pat_bindings(*lhs);
                self.seed_pat_bindings(*rhs);
            }
            Pat::WildCard | Pat::Rest | Pat::Lit(..) => {}
        }
    }

    fn unify_ty<T>(&mut self, t: T, actual: TyId<'db>, expected: TyId<'db>) -> TyId<'db>
    where
        T: Into<Typeable<'db>>,
    {
        let t = t.into();
        let span = t.clone().span(self.env.body());
        let expr = match &t {
            Typeable::Expr(expr, _) => Some(*expr),
            Typeable::Pat(_) => None,
        };
        let actual = self.equate_ty_with_expr(actual, expected, span, expr);

        self.retype_expr_or_pat(t, actual);
        actual
    }

    fn retype_expr_or_pat(&mut self, typeable: Typeable<'db>, ty: TyId<'db>) {
        match typeable {
            Typeable::Expr(expr, mut typed_expr) => {
                typed_expr.ty = ty;
                if typed_expr.borrow_provider.is_none() {
                    typed_expr.borrow_provider =
                        self.concrete_borrow_provider_for_effect_handle_ty(ty);
                }
                typed_expr.path_read_semantics = typed_expr
                    .binding
                    .map(|binding| self.binding_path_read_semantics(binding, ty));
                self.env.type_expr(expr, typed_expr);
            }
            Typeable::Pat(pat) => self.env.type_pat(pat, ty),
        }
    }

    fn equate_ty(
        &mut self,
        actual: TyId<'db>,
        expected: TyId<'db>,
        span: DynLazySpan<'db>,
    ) -> TyId<'db> {
        self.equate_ty_with_expr(actual, expected, span, None)
    }

    fn equate_ty_with_expr(
        &mut self,
        actual: TyId<'db>,
        expected: TyId<'db>,
        span: DynLazySpan<'db>,
        expr: Option<ExprId>,
    ) -> TyId<'db> {
        // FIXME: This is a temporary workaround, this should be removed when we
        // implement subtyping.
        if expected.is_never(self.db) && !actual.is_never(self.db) {
            if !actual.has_invalid(self.db) && !expected.has_invalid(self.db) {
                let diag = BodyDiag::TypeMismatch {
                    span,
                    expected,
                    given: actual,
                };
                self.push_diag(diag);
            }
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        // Resolve associated types before unification
        let actual = actual.fold_with(self.db, &mut self.table);
        let expected = expected.fold_with(self.db, &mut self.table);
        let actual = self.normalize_ty(actual);
        let expected = self.normalize_ty(expected);
        match self.table.unify(actual, expected) {
            Ok(()) => {
                // FIXME: This is a temporary workaround, this should be removed when we
                // implement subtyping.
                let actual = actual.fold_with(self.db, &mut self.table);
                if actual.is_never(self.db) {
                    expected
                } else {
                    let expected = expected.fold_with(self.db, &mut self.table);
                    merge_equated_layout_holes(self.db, actual, expected)
                }
            }

            Err(UnificationError::TypeMismatch) => {
                let actual = actual.fold_with(self.db, &mut self.table);
                let expected = expected.fold_with(self.db, &mut self.table);
                if self.retain_or_report_mismatch(env::TypeEquality {
                    actual,
                    expected,
                    span,
                    expr,
                }) {
                    merge_equated_layout_holes(self.db, expected, actual)
                } else {
                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }

            Err(UnificationError::OccursCheckFailed) => {
                self.push_diag(BodyDiag::InfiniteOccurrence(span));

                TyId::invalid(self.db, InvalidCause::Other)
            }
        }
    }

    fn resolve_path(
        &mut self,
        path: PathId<'db>,
        resolve_tail_as_value: bool,
        span: LazyPathSpan<'db>,
        site: DynLazySpan<'db>,
        minter: &LoweringContext<'db>,
    ) -> Result<PathRes<'db>, PathResError<'db>> {
        let scope = self.env.scope();
        let mut invisible = None;
        let mut applications = Vec::new();
        let mut observe_segment = |path: PathId<'db>, reso: &PathRes<'db>| {
            applications.extend(const_requirements::constrained_applications(self.db, reso));
            if invisible.is_some() {
                return;
            }
            if !reso.is_visible_from(self.db, scope) {
                invisible = Some((path, reso.name_span(self.db)));
            }
        };

        let res = match resolve_path_with_observer_and_minter(
            self.db,
            path,
            scope,
            self.env.assumptions(),
            resolve_tail_as_value,
            &mut observe_segment,
            minter,
        ) {
            Ok(r) => {
                self.env.register_path_applications(site, applications);
                Ok(r.map_over_ty(|ty| self.instantiate_to_term(ty)))
            }
            Err(err) => Err(err),
        };

        if let Some((path, deriv_span)) = invisible {
            let span = span.clone().segment(path.segment_index(self.db)).ident();
            let ident = path.ident(self.db);
            let diag = PathResDiag::Invisible(span.into(), ident.unwrap(), deriv_span);
            self.diags.push(diag.into());
        }

        res
    }

    fn instantiate_to_term(&mut self, ty: TyId<'db>) -> TyId<'db> {
        let base = ty.base_ty(self.db);
        let TyData::TyBase(TyBase::Func(def)) = base.data(self.db) else {
            return self.table.instantiate_to_term(ty);
        };
        self.instantiate_callable_to_term(ty, *def)
    }

    fn instantiate_trait_method_to_term(
        &mut self,
        method: Func<'db>,
        receiver_ty: TyId<'db>,
        inst: TraitInstId<'db>,
    ) -> TyId<'db> {
        let ty = instantiate_trait_method(self.db, method, &mut self.table, receiver_ty, inst);
        self.instantiate_callable_to_term(ty, method.as_callable(self.db).unwrap())
    }

    fn instantiate_trait_assoc_fn_to_term(
        &mut self,
        method: CallableDef<'db>,
        inst: TraitInstId<'db>,
    ) -> TyId<'db> {
        let ty = instantiate_trait_assoc_fn(self.db, method, inst);
        self.instantiate_callable_to_term(ty, method)
    }

    /// Brings a probed inherent-method candidate into this checker's
    /// inference context: extracts the bound candidate from the probe's
    /// solution, registers effect-provider inference keys for its args, and
    /// unifies the bound probe key against the receiver so the solution's
    /// bindings propagate into receiver inference variables (the inherent
    /// analog of the trait path unifying the instance's self type). The
    /// matching rule itself — key choice, normalization, binder application —
    /// lives in the method-table probe; nothing is re-derived here.
    fn extract_inherent_method_to_term(
        &mut self,
        cand: ProbedMethod<'db>,
        receiver_ty: TyId<'db>,
    ) -> TyId<'db> {
        let bound = cand.extract(&mut self.table);
        self.register_effect_provider_args(cand.def, bound.func_ty);
        let snapshot = self.table.snapshot();
        let matched = self.table.unify(bound.key_ty, receiver_ty).is_ok()
            && cand
                .query
                .assumptions
                .list(self.db)
                .iter()
                .zip(bound.assumptions.list(self.db))
                .all(|(&original, &solved)| self.table.unify(original, solved).is_ok());
        if matched {
            self.table.commit(snapshot);
        } else {
            self.table.rollback_to(snapshot);
        }
        bound.func_ty
    }

    fn instantiate_callable_to_term(
        &mut self,
        mut ty: TyId<'db>,
        callable: CallableDef<'db>,
    ) -> TyId<'db> {
        if ty.has_invalid(self.db) {
            return ty;
        }

        while let Some(prop) = ty.applicable_ty(self.db) {
            let (_, args) = ty.decompose_ty_app(self.db);
            let param_index = args.len();
            let param_ty = callable.params(self.db).get(param_index).copied();
            let arg = self.table.new_var_for(prop);
            if let Some(param_ty) = param_ty
                && self.is_effect_provider_param(param_ty)
            {
                self.effect_provider_keys
                    .extend(inference_keys(self.db, &arg));
            }
            ty = TyId::app(self.db, ty, arg);
        }

        ty
    }

    fn is_effect_provider_param(&self, param_ty: TyId<'db>) -> bool {
        matches!(param_ty.data(self.db), TyData::TyParam(p) if p.is_effect_provider())
            || matches!(
                param_ty.data(self.db),
                TyData::ConstTy(const_ty)
                    if matches!(
                        const_ty.data(self.db),
                        ConstTyData::TyParam(param, _) if param.is_implicit()
                    )
            )
    }

    /// Registers effect-provider inference keys for the generic args of an
    /// already-applied callable type (the applied counterpart of the
    /// bookkeeping `instantiate_callable_to_term` does while applying).
    fn register_effect_provider_args(&mut self, callable: CallableDef<'db>, ty: TyId<'db>) {
        let params = callable.params(self.db);
        for (param_index, arg) in ty.generic_args(self.db).iter().enumerate() {
            let Some(&param_ty) = params.get(param_index) else {
                break;
            };
            if self.is_effect_provider_param(param_ty) {
                self.effect_provider_keys
                    .extend(inference_keys(self.db, arg));
            }
        }
    }

    /// Resolve associated type to concrete type if possible
    fn normalize_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
        normalize_ty(
            self.db,
            ty.fold_with(self.db, &mut self.table),
            self.env.scope(),
            self.env.assumptions(),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum EffectArg<'db> {
    Place(Place<'db>),
    Value(ExprId),
    Binding(LocalBinding<'db>),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum EffectPassMode {
    /// The provided effect is already a place; pass it directly.
    ByPlace,
    /// The provided effect is an rvalue; materialize it into a block-scoped temp place.
    ByTempPlace,
    ByValue,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum EffectArgLayoutView {
    Direct,
    ProviderTarget,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ResolvedEffectArg<'db> {
    pub param_idx: usize,
    pub binding_idx: u32,
    pub key: HirTyId<'db>,
    pub arg: EffectArg<'db>,
    /// The with-binding whose source was evaluated on entry to the block.
    pub with_source: Option<ExprId>,
    pub pass_mode: EffectPassMode,
    pub layout_view: EffectArgLayoutView,
    pub required_mut: bool,
    pub key_kind: EffectKeyKind,
    pub instantiated_key_ty: Option<TyId<'db>>,
    pub provider_target_ty: Option<TyId<'db>>,
    pub provider: Option<ProviderAddressSpace>,
}

/// Resolved reference for a `const`-valued path expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ConstRef<'db> {
    Const(Const<'db>),
    TraitConst(AssocConstUse<'db>),
    InherentConst(InherentConstUse<'db>),
}

impl<'db> TyVisitable<'db> for ConstRef<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        match self {
            ConstRef::Const(_) => {}
            ConstRef::TraitConst(assoc) => assoc.visit_with(visitor),
            ConstRef::InherentConst(use_) => use_.visit_with(visitor),
        }
    }
}

impl<'db> TyFoldable<'db> for ConstRef<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        match self {
            ConstRef::Const(const_def) => ConstRef::Const(const_def),
            ConstRef::TraitConst(assoc) => ConstRef::TraitConst(assoc.fold_with(db, folder)),
            ConstRef::InherentConst(use_) => ConstRef::InherentConst(use_.fold_with(db, folder)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
struct ExprPlaceId(u32);
entity_impl!(ExprPlaceId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum SmirLoweringIssue {
    InvalidExpr(ExprId),
    IncompleteExpr(ExprId),
    InvalidStmt(StmtId),
    IncompleteStmt(StmtId),
    InvalidCond(CondId),
}

impl SmirLoweringIssue {
    pub(crate) fn is_incomplete(self) -> bool {
        matches!(self, Self::IncompleteExpr(_) | Self::IncompleteStmt(_))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
#[cfg(test)]
pub(crate) enum SmirLoweringReadiness {
    Ready,
    BlockedByInvalidProgram,
    IncompletePlan,
}

/// The result of type checking one body.
///
/// Cloning is cheap: the inferred tables are shared. The checked-body queries
/// (`check_func_body` and friends) return a clone of the `infer_body`
/// template, so each body's tables are stored once rather than twice.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedBody<'db> {
    tables: Arc<TypedBodyTables<'db>>,
}

// A private module keeps the shared tables type out of the crate's public
// API.
mod typed_body_tables {
    use super::*;

    /// Shared storage behind [`TypedBody`]; its fields are private to type checking.
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct TypedBodyTables<'db> {
        pub(super) body: Option<Body<'db>>,
        pub(super) result_ty: TyId<'db>,
        pub(super) assumptions: PredicateListId<'db>,
        pub(super) pat_ty: SecondaryMap<PatId, Option<TyId<'db>>>,
        pub(super) expr_ty: SecondaryMap<ExprId, Option<ExprProp<'db>>>,
        pub(super) implicit_moves: FxHashSet<ExprId>,
        pub(super) const_refs: SecondaryMap<ExprId, Option<ConstRef<'db>>>,
        pub(super) value_path_refs: SecondaryMap<ExprId, Option<ValuePathRef<'db>>>,
        pub(super) semantic_expr_lowering: SecondaryMap<ExprId, Option<SemanticExprLowering<'db>>>,
        pub(super) record_init_lowering: SecondaryMap<ExprId, Option<RecordInitLowering<'db>>>,
        pub(super) resolved_field_index: SecondaryMap<ExprId, Option<u16>>,
        pub(super) call_effect_args: SecondaryMap<ExprId, Option<Vec<ResolvedEffectArg<'db>>>>,
        pub(super) return_borrow_provider: Option<ProviderAddressSpace>,
        /// Bindings for function parameters (indexed by param position)
        pub(super) param_bindings: Vec<LocalBinding<'db>>,
        /// Bindings for local variables (keyed by the pattern that introduces them)
        pub(super) pat_bindings: SecondaryMap<PatId, Option<LocalBinding<'db>>>,
        /// Binding capture mode for local variables (keyed by the pattern that introduces them)
        pub(super) pat_binding_modes: SecondaryMap<PatId, Option<PatBindingMode>>,
        pub(super) pattern_store: PatternStore<'db>,
        pub(super) pattern_status: SecondaryMap<PatId, PatternAnalysisStatus>,
        /// Resolved Seq trait methods for for-loops
        pub(super) for_loop_seq: SecondaryMap<StmtId, Option<ForLoopSeq<'db>>>,
        pub(super) expr_place: SecondaryMap<ExprId, PackedOption<ExprPlaceId>>,
        pub(super) expr_places: PrimaryMap<ExprPlaceId, Place<'db>>,
        /// The constrained type applications that resolving the body's paths
        /// passed through, at every segment, each with the span of the
        /// expression or pattern whose path it is. Const requirements check
        /// them there (`check_body_requirements`).
        pub(super) path_applications: Vec<(DynLazySpan<'db>, TyId<'db>)>,
    }
}
use typed_body_tables::TypedBodyTables;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingSource {
    pub init_expr: ExprId,
    pub projection: Vec<ReturnProjectionStep>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReturnProjectionStep {
    Field(u16),
    VariantField { variant: u16, field: u16 },
    ConstantIndex(usize),
    ParamIndex(usize),
    AnyIndex,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnSource {
    pub result_projection: Vec<ReturnProjectionStep>,
    pub origin: CallableInputLayoutHoleOrigin,
    pub projection: Vec<ReturnProjectionStep>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReturnProvenance {
    Fresh,
    Forwarded(Vec<ReturnSource>),
    Unknown,
}

#[salsa::tracked(
    return_ref,
    cycle_fn=func_return_provenance_cycle_recover,
    cycle_initial=func_return_provenance_cycle_initial
)]
fn func_return_provenance<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> ReturnProvenance {
    let (diags, typed_body) = infer_body(db, BodyOwner::Func(func));
    if !diags.is_empty() {
        return ReturnProvenance::Unknown;
    }
    typed_body.return_provenance_for_func(db, func, &mut FxHashSet::default())
}

fn func_return_provenance_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _func: Func<'db>,
) -> ReturnProvenance {
    ReturnProvenance::Forwarded(Vec::new())
}

fn func_return_provenance_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &ReturnProvenance,
    _count: u32,
    _func: Func<'db>,
) -> salsa::CycleRecoveryAction<ReturnProvenance> {
    salsa::CycleRecoveryAction::Iterate
}

fn call_like_expr_args(expr: &Expr<'_>) -> Option<Vec<ExprId>> {
    match expr {
        Expr::Call(_, args) => Some(args.iter().map(|arg| arg.expr).collect()),
        Expr::MethodCall(receiver, _, _, args) => {
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(*receiver);
            call_args.extend(args.iter().map(|arg| arg.expr));
            Some(call_args)
        }
        Expr::Un(receiver, _) => Some(vec![*receiver]),
        Expr::Bin(receiver, arg, _) | Expr::AugAssign(receiver, arg, _) => {
            Some(vec![*receiver, *arg])
        }
        _ => None,
    }
}

type LayoutBackingEquivalenceKey = (
    usize,
    Option<(CallableInputLayoutHoleOrigin, Vec<LayoutBundlePathStep>)>,
);

fn layout_backing_equivalence_key(
    param_idx: usize,
    source: &CallableInputLayoutBackingSource,
) -> LayoutBackingEquivalenceKey {
    let family = source
        .projection
        .iter()
        .rposition(|step| matches!(step, LayoutBundlePathStep::Index))
        .map(|last_index| (source.origin, source.projection[..=last_index].to_vec()));
    (param_idx, family)
}

fn degenerate_return_projection(
    path: &[LayoutBundlePathStep],
    index_lengths: &[usize],
) -> Option<Vec<ReturnProjectionStep>> {
    let mut projection = Vec::new();
    let mut path_idx = 0;
    let mut length_idx = 0;
    while path_idx < path.len() {
        match path[path_idx] {
            LayoutBundlePathStep::Field(field) => {
                projection.push(ReturnProjectionStep::Field(field));
                path_idx += 1;
            }
            LayoutBundlePathStep::Variant(variant) => {
                let Some(LayoutBundlePathStep::Field(field)) = path.get(path_idx + 1) else {
                    return None;
                };
                projection.push(ReturnProjectionStep::VariantField {
                    variant,
                    field: *field,
                });
                path_idx += 2;
            }
            LayoutBundlePathStep::Index => {
                if index_lengths.get(length_idx) != Some(&1) {
                    return None;
                }
                projection.push(ReturnProjectionStep::ConstantIndex(0));
                path_idx += 1;
                length_idx += 1;
            }
            LayoutBundlePathStep::ConstParam(_) => {
                path_idx += 1;
            }
        }
    }
    (length_idx == index_lengths.len()).then_some(projection)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Update)]
pub enum ValuePathRef<'db> {
    UnitVariant(ResolvedVariant<'db>),
    TypeConst(TyId<'db>),
    FunctionItem,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum CodeRegionIntrinsicKind {
    Offset,
    Len,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ConstIntrinsicKind {
    SizeOf,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub enum SemanticExprLowering<'db> {
    Call {
        callable: Callable<'db>,
    },
    CodeRegionIntrinsic {
        callable: Callable<'db>,
        region_arg: ExprId,
        kind: CodeRegionIntrinsicKind,
    },
    ConstIntrinsic {
        callable: Callable<'db>,
        kind: ConstIntrinsicKind,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum RecordInitLowering<'db> {
    Struct,
    EnumVariant(ResolvedVariant<'db>),
}

impl<'db> TyVisitable<'db> for ValuePathRef<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        match self {
            Self::UnitVariant(variant) => variant.ty.visit_with(visitor),
            Self::TypeConst(ty) => ty.visit_with(visitor),
            Self::FunctionItem => {}
        }
    }
}

impl<'db> TyFoldable<'db> for ValuePathRef<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        match self {
            Self::UnitVariant(variant) => Self::UnitVariant(ResolvedVariant {
                ty: variant.ty.fold_with(db, folder),
                ..variant
            }),
            // A type-level value path is a symbolic reference into the
            // template owner's generic parameter list. Keep that reference
            // formal: semantic const evaluation applies the instance's
            // substitution explicitly, while runtime lowering needs the
            // original parameter index to select an exact runtime ABI value.
            // Folding it here would retain only the actual value and erase
            // which formal const parameter the expression referenced.
            Self::TypeConst(ty) => Self::TypeConst(ty),
            Self::FunctionItem => Self::FunctionItem,
        }
    }
}

impl<'db> TyVisitable<'db> for SemanticExprLowering<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        match self {
            Self::Call { callable }
            | Self::CodeRegionIntrinsic { callable, .. }
            | Self::ConstIntrinsic { callable, .. } => callable.visit_with(visitor),
        }
    }
}

impl<'db> TyFoldable<'db> for SemanticExprLowering<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        match self {
            Self::Call { callable } => Self::Call {
                callable: callable.fold_with(db, folder),
            },
            Self::CodeRegionIntrinsic {
                callable,
                region_arg,
                kind,
            } => Self::CodeRegionIntrinsic {
                callable: callable.fold_with(db, folder),
                region_arg,
                kind,
            },
            Self::ConstIntrinsic { callable, kind } => Self::ConstIntrinsic {
                callable: callable.fold_with(db, folder),
                kind,
            },
        }
    }
}

impl<'db> TyVisitable<'db> for RecordInitLowering<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        if let Self::EnumVariant(variant) = self {
            variant.ty.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for RecordInitLowering<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        match self {
            Self::Struct => Self::Struct,
            Self::EnumVariant(variant) => Self::EnumVariant(ResolvedVariant {
                ty: variant.ty.fold_with(db, folder),
                ..variant
            }),
        }
    }
}

impl<'db> TyVisitable<'db> for TypedBody<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        self.tables.assumptions.visit_with(visitor);
        self.visit_body_types(visitor);
    }
}

impl<'db> TypedBody<'db> {
    /// Visits the types the body's expressions, patterns, bindings and
    /// resolutions carry, but not the ambient assumptions the body was
    /// checked under, which are not dependencies of the body itself.
    pub(super) fn visit_body_types<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        self.tables.result_ty.visit_with(visitor);
        for ty in self.tables.pat_ty.values().flatten() {
            ty.visit_with(visitor);
        }
        for prop in self.tables.expr_ty.values().flatten() {
            prop.visit_with(visitor);
        }
        for cref in self.tables.const_refs.values().flatten() {
            cref.visit_with(visitor);
        }
        for value_path in self.tables.value_path_refs.values().flatten() {
            value_path.visit_with(visitor);
        }
        for lowering in self.tables.semantic_expr_lowering.values().flatten() {
            lowering.visit_with(visitor);
        }
        for lowering in self.tables.record_init_lowering.values().flatten() {
            lowering.visit_with(visitor);
        }
        for args in self.tables.call_effect_args.values().flatten() {
            args.visit_with(visitor);
        }
        self.tables.param_bindings.visit_with(visitor);
        for binding in self.tables.pat_bindings.values().flatten() {
            binding.visit_with(visitor);
        }
        for place in self.tables.expr_places.values() {
            place.visit_with(visitor);
        }
        self.tables.pattern_store.visit_with(visitor);
        for seq in self.tables.for_loop_seq.values().flatten() {
            seq.visit_with(visitor);
        }
        for (_, ty) in &self.tables.path_applications {
            ty.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for TypedBody<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: crate::analysis::ty::fold::TyFolder<'db>,
    {
        // Folding writes every table, so take the tables, copying them if
        // another `TypedBody` still shares them.
        let mut this = Arc::unwrap_or_clone(self.tables);
        this.result_ty = this.result_ty.fold_with(db, folder);
        this.assumptions = this.assumptions.fold_with(db, folder);
        this.pat_ty
            .values_mut()
            .flatten()
            .for_each(|ty| *ty = ty.fold_with(db, folder));
        this.expr_ty
            .values_mut()
            .flatten()
            .for_each(|prop| *prop = prop.clone().fold_with(db, folder));
        this.const_refs
            .values_mut()
            .flatten()
            .for_each(|cref| *cref = (*cref).fold_with(db, folder));
        this.value_path_refs
            .values_mut()
            .flatten()
            .for_each(|path| *path = (*path).fold_with(db, folder));
        this.semantic_expr_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| *lowering = lowering.clone().fold_with(db, folder));
        this.record_init_lowering
            .values_mut()
            .flatten()
            .for_each(|lowering| *lowering = (*lowering).fold_with(db, folder));
        for args in this.call_effect_args.values_mut().flatten() {
            for arg in args {
                *arg = arg.clone().fold_with(db, folder);
            }
        }
        this.param_bindings
            .iter_mut()
            .for_each(|binding| *binding = binding.fold_with(db, folder));
        this.pat_bindings
            .values_mut()
            .flatten()
            .for_each(|binding| *binding = binding.fold_with(db, folder));
        this.pattern_store = this.pattern_store.fold_with(db, folder);
        this.for_loop_seq
            .values_mut()
            .flatten()
            .for_each(|seq| *seq = seq.clone().fold_with(db, folder));
        this.expr_places
            .values_mut()
            .for_each(|place| *place = place.clone().fold_with(db, folder));
        this.path_applications
            .iter_mut()
            .for_each(|(_, ty)| *ty = ty.fold_with(db, folder));
        this.into()
    }
}

impl<'db> From<TypedBodyTables<'db>> for TypedBody<'db> {
    fn from(tables: TypedBodyTables<'db>) -> Self {
        Self {
            tables: Arc::new(tables),
        }
    }
}

unsafe impl<'db> Update for TypedBody<'db> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_value = unsafe { &mut *old_pointer };
        if *old_value == new_value {
            false
        } else {
            *old_value = new_value;
            true
        }
    }
}

impl<'db> TypedBody<'db> {
    /// The tables for writing, copied first if another `TypedBody` shares
    /// them.
    fn tables_mut(&mut self) -> &mut TypedBodyTables<'db> {
        Arc::make_mut(&mut self.tables)
    }

    /// The constrained type applications that resolving the body's paths
    /// passed through, each with the span of the expression or pattern whose
    /// path it is.
    pub(super) fn path_applications(&self) -> &[(DynLazySpan<'db>, TyId<'db>)] {
        &self.tables.path_applications
    }

    pub fn body(&self) -> Option<Body<'db>> {
        self.tables.body
    }

    pub fn result_ty(&self) -> TyId<'db> {
        self.tables.result_ty
    }

    pub(crate) fn smir_lowering_issues(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<SmirLoweringIssue> {
        let Some(body) = self.tables.body else {
            return Vec::new();
        };

        let mut issues = body
            .exprs(db)
            .iter()
            .filter_map(|(expr, expr_data)| match expr_data {
                Partial::Present(expr_data) => self.expr_smir_lowering_issue(db, expr, expr_data),
                Partial::Absent => Some(SmirLoweringIssue::InvalidExpr(expr)),
            })
            .collect::<Vec<_>>();
        issues.extend(
            body.stmts(db)
                .iter()
                .filter_map(|(stmt, stmt_data)| match stmt_data {
                    Partial::Present(stmt_data) => {
                        self.stmt_smir_lowering_issue(db, stmt, stmt_data)
                    }
                    Partial::Absent => Some(SmirLoweringIssue::InvalidStmt(stmt)),
                }),
        );
        issues.extend(body.conds(db).iter().filter_map(|(cond, cond_data)| {
            matches!(cond_data, Partial::Absent).then_some(SmirLoweringIssue::InvalidCond(cond))
        }));
        issues
    }

    #[cfg(test)]
    pub(crate) fn smir_lowering_readiness(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> SmirLoweringReadiness {
        let issues = self.smir_lowering_issues(db);
        if issues.iter().copied().any(SmirLoweringIssue::is_incomplete) {
            SmirLoweringReadiness::IncompletePlan
        } else if issues.is_empty() {
            SmirLoweringReadiness::Ready
        } else {
            SmirLoweringReadiness::BlockedByInvalidProgram
        }
    }

    fn expr_smir_lowering_issue(
        &self,
        db: &'db dyn HirAnalysisDb,
        expr: ExprId,
        expr_data: &Expr<'db>,
    ) -> Option<SmirLoweringIssue> {
        let expr_ty = self.expr_ty(db, expr);
        let missing_plan = || {
            Some(if expr_ty.has_invalid(db) || expr_ty.has_var(db) {
                SmirLoweringIssue::InvalidExpr(expr)
            } else {
                SmirLoweringIssue::IncompleteExpr(expr)
            })
        };

        match expr_data {
            Expr::Path(_)
                if expr_ty.has_invalid(db)
                    && self.expr_binding(expr).is_none()
                    && self.expr_const_ref(expr).is_none()
                    && self.expr_code_region_ref(db, expr).is_none()
                    && self.value_path_ref(expr).is_none() =>
            {
                Some(SmirLoweringIssue::InvalidExpr(expr))
            }
            Expr::Call(..) | Expr::MethodCall(..)
                if self.semantic_expr_lowering(expr).is_none() =>
            {
                missing_plan()
            }
            Expr::UnsupportedMacroCall => Some(SmirLoweringIssue::InvalidExpr(expr)),
            Expr::Assert(_) | Expr::Cast(..) if expr_ty.has_invalid(db) => {
                Some(SmirLoweringIssue::InvalidExpr(expr))
            }
            // A resolved constructor plan can survive a failed contextual
            // unification. Lowering must not use that invalid expression type
            // to resolve record fields or select the aggregate representation.
            Expr::RecordInit(..) if expr_ty.has_invalid(db) || expr_ty.has_var(db) => {
                Some(SmirLoweringIssue::InvalidExpr(expr))
            }
            Expr::RecordInit(..) if self.record_init_lowering(expr).is_none() => missing_plan(),
            Expr::Field(..)
                if self.expr_place(expr).is_none() && self.resolved_field_index(expr).is_none() =>
            {
                missing_plan()
            }
            Expr::Un(inner, crate::hir_def::expr::UnOp::Mut | crate::hir_def::expr::UnOp::Ref)
                if !self.has_lowerable_place(db, *inner) =>
            {
                missing_plan()
            }
            Expr::Assign(dst, _)
                if self.semantic_expr_lowering(*dst).is_none()
                    && !self.has_lowerable_place(db, *dst) =>
            {
                Some(self.missing_place_issue(db, expr, *dst))
            }
            Expr::AugAssign(dst, _, _)
                if self.semantic_expr_lowering(expr).is_none()
                    && !self.has_lowerable_place(db, *dst) =>
            {
                Some(self.missing_place_issue(db, expr, *dst))
            }
            Expr::Match(_, Partial::Absent) => Some(SmirLoweringIssue::InvalidExpr(expr)),
            Expr::Match(_, Partial::Present(arms)) => {
                if arms.iter().any(|arm| {
                    matches!(
                        self.pattern_status(arm.pat),
                        PatternAnalysisStatus::Unsupported
                    )
                }) {
                    Some(SmirLoweringIssue::IncompleteExpr(expr))
                } else if arms.iter().any(|arm| {
                    matches!(self.pattern_status(arm.pat), PatternAnalysisStatus::Invalid)
                }) {
                    Some(SmirLoweringIssue::InvalidExpr(expr))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn stmt_smir_lowering_issue(
        &self,
        db: &'db dyn HirAnalysisDb,
        stmt: StmtId,
        stmt_data: &Stmt<'db>,
    ) -> Option<SmirLoweringIssue> {
        match stmt_data {
            Stmt::For(pat, expr, ..) if self.for_loop_seq(stmt).is_none() => {
                let iterable_ty = self.expr_ty(db, *expr);
                Some(
                    if iterable_ty.has_invalid(db)
                        || iterable_ty.has_var(db)
                        || self.pat_ty(db, *pat).has_invalid(db)
                    {
                        SmirLoweringIssue::InvalidStmt(stmt)
                    } else {
                        SmirLoweringIssue::IncompleteStmt(stmt)
                    },
                )
            }
            _ => None,
        }
    }

    /// Pointer rvalues are evaluated once before lowering their target place.
    fn has_lowerable_place(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> bool {
        self.expr_place(expr).is_some()
            || self.tables.body.is_some_and(|body| {
                is_pointer_place_expr(db, body, expr, &mut |expr| self.expr_ty(db, expr))
            })
    }

    fn missing_place_issue(
        &self,
        db: &'db dyn HirAnalysisDb,
        expr: ExprId,
        place: ExprId,
    ) -> SmirLoweringIssue {
        if self.missing_place_is_explained_by_invalid_program(db, place) {
            SmirLoweringIssue::InvalidExpr(expr)
        } else {
            SmirLoweringIssue::IncompleteExpr(expr)
        }
    }

    fn missing_place_is_explained_by_invalid_program(
        &self,
        db: &'db dyn HirAnalysisDb,
        expr: ExprId,
    ) -> bool {
        let Some(body) = self.tables.body else {
            return false;
        };
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return true;
        };
        if self.expr_ty(db, expr).has_invalid(db) {
            return true;
        }
        match expr_data {
            Expr::Path(..) => self.expr_binding(expr).is_none(),
            Expr::Field(base, field) => {
                field.to_opt().is_none()
                    || self.missing_place_is_explained_by_invalid_program(db, *base)
            }
            Expr::Bin(base, _, crate::hir_def::BinOp::Index) => {
                self.semantic_expr_lowering(expr).is_some()
                    || self.missing_place_is_explained_by_invalid_program(db, *base)
            }
            _ => true,
        }
    }

    pub fn assumptions(&self) -> PredicateListId<'db> {
        self.tables.assumptions
    }

    pub fn expr_ty(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> TyId<'db> {
        self.expr_prop(db, expr).ty
    }

    pub fn expr_prop(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> ExprProp<'db> {
        self.tables
            .expr_ty
            .get(expr)
            .cloned()
            .flatten()
            .unwrap_or_else(|| ExprProp::invalid(db))
    }

    pub fn is_implicit_move(&self, expr: ExprId) -> bool {
        self.tables.implicit_moves.contains(&expr)
    }

    /// All const references registered in this body, in arbitrary order.
    pub fn const_refs(&self) -> impl Iterator<Item = ConstRef<'db>> + '_ {
        self.tables.const_refs.values().flatten().copied()
    }

    pub fn expr_const_ref(&self, expr: ExprId) -> Option<ConstRef<'db>> {
        self.tables.const_refs[expr]
    }

    pub fn value_path_ref(&self, expr: ExprId) -> Option<ValuePathRef<'db>> {
        self.tables.value_path_refs[expr]
    }

    pub fn expr_code_region_ref(
        &self,
        db: &'db dyn HirAnalysisDb,
        expr: ExprId,
    ) -> Option<SemanticCodeRegionRef<'db>> {
        self.expr_binding(expr)
            .and_then(|binding| manual_contract_root_ref_from_ty(db, self.binding_ty(db, binding)))
            .or_else(|| manual_contract_root_ref_from_ty(db, self.expr_ty(db, expr)))
    }

    pub fn semantic_expr_lowering(&self, expr: ExprId) -> Option<&SemanticExprLowering<'db>> {
        self.tables.semantic_expr_lowering[expr].as_ref()
    }

    pub fn record_init_lowering(&self, expr: ExprId) -> Option<RecordInitLowering<'db>> {
        self.tables.record_init_lowering[expr]
    }

    pub fn resolved_field_index(&self, expr: ExprId) -> Option<u16> {
        self.tables.resolved_field_index[expr]
    }

    // Final typed pattern/binding view. This can intentionally differ from
    // validated-pattern match types when destructuring borrowed carriers.
    pub fn pat_ty(&self, db: &'db dyn HirAnalysisDb, pat: PatId) -> TyId<'db> {
        self.tables
            .pat_ty
            .get(pat)
            .copied()
            .flatten()
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }

    pub fn callable_expr(&self, expr: ExprId) -> Option<&Callable<'db>> {
        match self.semantic_expr_lowering(expr)? {
            SemanticExprLowering::Call { callable }
            | SemanticExprLowering::CodeRegionIntrinsic { callable, .. }
            | SemanticExprLowering::ConstIntrinsic { callable, .. } => Some(callable),
        }
    }

    pub fn code_region_ref(
        &self,
        db: &'db dyn HirAnalysisDb,
        expr: ExprId,
    ) -> Option<SemanticCodeRegionRef<'db>> {
        match self.semantic_expr_lowering(expr)? {
            SemanticExprLowering::CodeRegionIntrinsic { region_arg, .. } => {
                self.expr_code_region_ref(db, *region_arg)
            }
            SemanticExprLowering::Call { .. } | SemanticExprLowering::ConstIntrinsic { .. } => None,
        }
    }

    pub fn call_effect_args(&self, call_expr: ExprId) -> Option<&[ResolvedEffectArg<'db>]> {
        self.tables.call_effect_args[call_expr].as_deref()
    }

    pub fn return_borrow_provider(&self) -> Option<ProviderAddressSpace> {
        self.tables.return_borrow_provider
    }

    /// Get the binding for a function parameter by index.
    pub fn param_binding(&self, idx: usize) -> Option<LocalBinding<'db>> {
        self.tables.param_bindings.get(idx).copied()
    }

    /// Get the binding for a local variable by its pattern.
    pub fn pat_binding(&self, pat: PatId) -> Option<LocalBinding<'db>> {
        self.tables.pat_bindings[pat]
    }

    /// Get how this local binding is captured by its source pattern destructuring.
    pub fn pat_binding_mode(&self, pat: PatId) -> Option<PatBindingMode> {
        self.tables.pat_binding_modes[pat]
    }

    pub fn binding_ty(&self, db: &'db dyn HirAnalysisDb, binding: LocalBinding<'db>) -> TyId<'db> {
        match binding {
            LocalBinding::Local { pat, .. } => self.pat_ty(db, pat),
            LocalBinding::Param { ty, .. } => ty,
            LocalBinding::EffectParam { site, idx, .. } => {
                crate::core::semantic::EffectEnvView::new(site)
                    .visible_effect_binding_ty(db, idx)
                    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
            }
        }
    }

    pub fn path_expr_read_semantics(&self, expr: ExprId) -> Option<PathReadSemantics> {
        self.tables.expr_ty[expr]
            .as_ref()
            .and_then(|prop| prop.path_read_semantics)
    }

    pub fn path_expr_reuses_local(&self, expr: ExprId) -> bool {
        matches!(
            self.path_expr_read_semantics(expr),
            Some(PathReadSemantics::ReuseLocal)
        )
    }

    pub fn pattern_store(&self) -> &PatternStore<'db> {
        &self.tables.pattern_store
    }

    pub fn pattern_status(&self, pat: PatId) -> PatternAnalysisStatus {
        self.tables.pattern_status[pat]
    }

    pub fn pattern_root(&self, pat: PatId) -> Option<ValidatedPatId> {
        self.pattern_status(pat).ready_root()
    }

    /// Get the resolved Seq methods for a for-loop statement.
    pub fn for_loop_seq(&self, stmt: StmtId) -> Option<&ForLoopSeq<'db>> {
        self.tables.for_loop_seq[stmt].as_ref()
    }

    pub fn binding_source(
        &self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> Option<BindingSource> {
        let LocalBinding::Local { pat, .. } = binding else {
            return None;
        };
        let body = self.body()?;

        for (_, stmt) in body.stmts(db).iter() {
            if let Partial::Present(crate::hir_def::Stmt::Let(stmt_pat, _, Some(init_expr))) = stmt
                && let Some(projection) = self.binding_projection_in_pat(*stmt_pat, pat)
            {
                return Some(BindingSource {
                    init_expr: *init_expr,
                    projection,
                });
            }
        }

        None
    }

    fn binding_projection_in_pat(
        &self,
        pat: PatId,
        binding_pat: PatId,
    ) -> Option<Vec<ReturnProjectionStep>> {
        self.binding_projection_in_validated_pat(self.pattern_root(pat)?, binding_pat)
    }

    fn binding_projection_in_validated_pat(
        &self,
        pat: ValidatedPatId,
        binding_pat: PatId,
    ) -> Option<Vec<ReturnProjectionStep>> {
        match self.tables.pattern_store.node(pat).kind() {
            ValidatedPatKind::Wildcard { binding } => binding
                .filter(|binding| binding.representative_pat == binding_pat)
                .map(|_| Vec::new()),
            ValidatedPatKind::Constructor { ctor, fields } => {
                fields.iter().enumerate().find_map(|(field_idx, child)| {
                    let suffix = self.binding_projection_in_validated_pat(*child, binding_pat)?;
                    let field = u16::try_from(field_idx).ok()?;
                    let step = match ctor {
                        ConstructorKind::Variant(variant, _) => {
                            ReturnProjectionStep::VariantField {
                                variant: variant.idx,
                                field,
                            }
                        }
                        ConstructorKind::Type(_) => ReturnProjectionStep::Field(field),
                        ConstructorKind::Literal(_, _) => return None,
                    };
                    let mut path = Vec::with_capacity(suffix.len() + 1);
                    path.push(step);
                    path.extend(suffix);
                    Some(path)
                })
            }
            ValidatedPatKind::Or(pats) => {
                let mut paths = pats
                    .iter()
                    .filter_map(|pat| self.binding_projection_in_validated_pat(*pat, binding_pat))
                    .collect::<Vec<_>>();
                let first = paths.pop()?;
                paths.into_iter().all(|path| path == first).then_some(first)
            }
        }
    }

    pub fn return_provenance(&self, db: &'db dyn HirAnalysisDb) -> ReturnProvenance {
        let Some(body) = self.body() else {
            return ReturnProvenance::Unknown;
        };
        let Some(func) = body.containing_func(db) else {
            return ReturnProvenance::Unknown;
        };
        match func_return_provenance(db, func) {
            ReturnProvenance::Forwarded(sources) if sources.is_empty() => ReturnProvenance::Fresh,
            provenance => provenance.clone(),
        }
    }

    pub fn forwarded_return_sources(&self, db: &'db dyn HirAnalysisDb) -> Vec<ReturnSource> {
        let Some(body) = self.body() else {
            return Vec::new();
        };
        let Some(func) = body.containing_func(db) else {
            return Vec::new();
        };
        self.collect_return_sources_for_func(db, func, &mut FxHashSet::default())
            .map(|(sources, _)| sources)
            .unwrap_or_default()
    }

    fn return_provenance_for_func(
        &self,
        db: &'db dyn HirAnalysisDb,
        func: Func<'db>,
        seen: &mut FxHashSet<Func<'db>>,
    ) -> ReturnProvenance {
        let Some((sources, saw_non_param)) = self.collect_return_sources_for_func(db, func, seen)
        else {
            return ReturnProvenance::Unknown;
        };
        if !saw_non_param {
            ReturnProvenance::Forwarded(sources)
        } else {
            ReturnProvenance::Fresh
        }
    }

    fn collect_return_sources_for_func(
        &self,
        db: &'db dyn HirAnalysisDb,
        func: Func<'db>,
        seen: &mut FxHashSet<Func<'db>>,
    ) -> Option<(Vec<ReturnSource>, bool)> {
        if !seen.insert(func) {
            return None;
        }

        let (diags, typed_body) = infer_body(db, BodyOwner::Func(func));
        if !diags.is_empty() {
            seen.remove(&func);
            return None;
        }
        let Some(body) = typed_body.body() else {
            seen.remove(&func);
            return None;
        };
        let Some(func_body) = func.body(db) else {
            seen.remove(&func);
            return None;
        };
        let root_expr = func_body.expr(db);
        let mut out = FxHashSet::default();
        let mut saw_non_param = false;
        typed_body.collect_explicit_return_param_sources_in_expr(
            db,
            body,
            root_expr,
            &mut out,
            &mut saw_non_param,
            seen,
        );
        typed_body.collect_implicit_return_param_sources_from_expr(
            db,
            body,
            root_expr,
            &mut out,
            &mut saw_non_param,
            seen,
        );
        let mut sources = out.into_iter().collect::<Vec<_>>();
        sources.sort_unstable();
        seen.remove(&func);
        Some((sources, saw_non_param))
    }

    fn forwarded_return_param_sources_from_expr(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
        seen: &mut FxHashSet<Func<'db>>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Option<Vec<ReturnSource>> {
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return None;
        };

        if let Some(SemanticExprLowering::Call { callable }) = self.semantic_expr_lowering(expr) {
            let args = call_like_expr_args(expr_data)?;
            if let CallableDef::VariantCtor(variant) = callable.callable_def() {
                let mut sources = Vec::new();
                for (field, arg) in args.into_iter().enumerate() {
                    let Some(mut field_sources) = self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        arg,
                        seen,
                        visited_locals,
                    ) else {
                        continue;
                    };
                    prefix_return_sources(
                        &mut field_sources,
                        &[ReturnProjectionStep::VariantField {
                            variant: variant.idx,
                            field: u16::try_from(field).ok()?,
                        }],
                    );
                    sources.extend(field_sources);
                }
                return (!sources.is_empty())
                    .then_some(sources)
                    .or_else(|| self.inferred_fresh_layout_return_sources(db, body, expr));
            }
            return self.forwarded_return_param_sources_from_call(
                db,
                body,
                expr,
                &args,
                seen,
                visited_locals,
            );
        }

        if (!matches!(expr_data, Expr::Path(_))
            || matches!(
                self.path_expr_read_semantics(expr),
                Some(PathReadSemantics::ReuseLocal | PathReadSemantics::ForwardInterface)
            )
            || {
                let ty = self.expr_ty(db, expr);
                ty.has_param(db) || ty.has_var(db) || ty_contains_const_hole(db, ty)
            })
            && let Some(place) = self.expr_place(expr).cloned()
            && let Some(sources) =
                self.forwarded_return_sources_from_place(db, body, &place, seen, visited_locals)
        {
            return Some(sources);
        }

        let forwarded = match expr_data {
            Expr::Block(stmts) => {
                let tail = stmts.last()?;
                match tail.data(db, body) {
                    Partial::Present(crate::hir_def::Stmt::Expr(tail_expr)) => self
                        .forwarded_return_param_sources_from_expr(
                            db,
                            body,
                            *tail_expr,
                            seen,
                            visited_locals,
                        ),
                    Partial::Present(crate::hir_def::Stmt::Return(Some(return_expr))) => self
                        .forwarded_return_param_sources_from_expr(
                            db,
                            body,
                            *return_expr,
                            seen,
                            visited_locals,
                        ),
                    _ => None,
                }
            }
            Expr::If(_, then_expr, else_expr) => merge_forwarded_param_sets(
                self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    *then_expr,
                    seen,
                    visited_locals,
                ),
                else_expr.and_then(|else_expr| {
                    self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        else_expr,
                        seen,
                        visited_locals,
                    )
                }),
            ),
            Expr::Match(_, arms) => {
                let Partial::Present(arms) = arms else {
                    return None;
                };
                let mut merged = FxHashSet::default();
                for arm in arms {
                    for idx in self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        arm.body,
                        seen,
                        visited_locals,
                    )? {
                        merged.insert(idx);
                    }
                }
                let mut out = merged.into_iter().collect::<Vec<_>>();
                out.sort_unstable();
                Some(out)
            }
            Expr::With(_, with_body) => self.forwarded_return_param_sources_from_expr(
                db,
                body,
                *with_body,
                seen,
                visited_locals,
            ),
            Expr::Un(inner, crate::hir_def::UnOp::Mut | crate::hir_def::UnOp::Ref) => self
                .forwarded_return_param_sources_from_expr(db, body, *inner, seen, visited_locals),
            Expr::RecordInit(_, fields) => {
                let (record_like, variant) = match self.record_init_lowering(expr)? {
                    RecordInitLowering::Struct => (RecordLike::Type(self.expr_ty(db, expr)), None),
                    RecordInitLowering::EnumVariant(variant) => {
                        (RecordLike::from_variant(variant), Some(variant.variant.idx))
                    }
                };
                let mut sources = Vec::new();
                for field in fields {
                    let label = field.label_eagerly(db, body)?;
                    let field_idx = record_like.record_field_idx(db, label)?;
                    let field_idx = u16::try_from(field_idx).ok()?;
                    let Some(mut field_sources) = self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        field.expr,
                        seen,
                        visited_locals,
                    ) else {
                        continue;
                    };
                    let projection =
                        variant.map_or(ReturnProjectionStep::Field(field_idx), |variant| {
                            ReturnProjectionStep::VariantField {
                                variant,
                                field: field_idx,
                            }
                        });
                    prefix_return_sources(&mut field_sources, &[projection]);
                    sources.extend(field_sources);
                }
                (!sources.is_empty()).then_some(sources)
            }
            Expr::Tuple(items) | Expr::Array(items) => {
                let is_array = matches!(expr_data, Expr::Array(_));
                let mut sources = Vec::new();
                for (idx, item) in items.iter().copied().enumerate() {
                    let Some(mut item_sources) = self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        item,
                        seen,
                        visited_locals,
                    ) else {
                        continue;
                    };
                    let projection = if is_array {
                        ReturnProjectionStep::ConstantIndex(idx)
                    } else {
                        ReturnProjectionStep::Field(u16::try_from(idx).ok()?)
                    };
                    prefix_return_sources(&mut item_sources, &[projection]);
                    sources.extend(item_sources);
                }
                (!sources.is_empty()).then_some(sources)
            }
            Expr::ArrayRep(value, _) => {
                let mut sources = self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    *value,
                    seen,
                    visited_locals,
                )?;
                prefix_return_sources(&mut sources, &[ReturnProjectionStep::AnyIndex]);
                Some(sources)
            }
            Expr::Path(_) => None,
            Expr::Call(..) | Expr::MethodCall(..) => None,
            _ => None,
        };
        forwarded.or_else(|| self.inferred_fresh_layout_return_sources(db, body, expr))
    }

    fn inferred_fresh_layout_return_sources(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
    ) -> Option<Vec<ReturnSource>> {
        let func = body.containing_func(db)?;
        let result_ty = self.expr_ty(db, expr);
        let mut out = FxHashSet::default();
        let mut found_result_root = false;
        for param_idx in collect_generic_params(db, func.into())
            .params(db)
            .iter()
            .filter_map(|param| match param.data(db) {
                TyData::ConstTy(const_ty) => match const_ty.data(db) {
                    ConstTyData::TyParam(param, _) => Some(param.idx),
                    _ => None,
                },
                _ => None,
            })
        {
            let output_paths = layout_param_projection_paths_in_ty(db, result_ty, param_idx);
            if output_paths.is_empty() {
                continue;
            }
            found_result_root = true;
            let output_projections = output_paths
                .iter()
                .map(|(path, lengths)| degenerate_return_projection(path, lengths))
                .collect::<Option<Vec<_>>>()?;
            let mut input_groups = FxHashMap::default();
            for source in callable_input_layout_backing_sources(db, func, param_idx) {
                let lengths = callable_input_layout_backing_index_lengths(db, func, &source)?;
                let Some(projection) = degenerate_return_projection(&source.projection, &lengths)
                else {
                    continue;
                };
                input_groups
                    .entry(layout_backing_equivalence_key(param_idx, &source))
                    .or_insert((source.origin, projection));
            }
            if input_groups.len() != 1 {
                return None;
            }
            let (origin, projection) = input_groups
                .into_values()
                .next()
                .expect("one layout-source equivalence group must contain one source");
            for result_projection in output_projections {
                out.insert(ReturnSource {
                    result_projection,
                    origin,
                    projection: projection.clone(),
                });
            }
        }
        if !found_result_root || out.is_empty() {
            return None;
        }
        let mut out = out.into_iter().collect::<Vec<_>>();
        out.sort_unstable();
        Some(out)
    }

    fn forwarded_return_param_sources_from_call(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
        call_args: &[ExprId],
        seen: &mut FxHashSet<Func<'db>>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Option<Vec<ReturnSource>> {
        let callable = self.callable_expr(expr)?;
        let CallableDef::Func(mut func) = callable.callable_def() else {
            return None;
        };
        if let Some(inst) = callable.trait_inst()
            && let Some(name) = func.name(db).to_opt()
            && let Selection::Unique(method) = resolve_trait_method_instance(
                db,
                TraitSolveCx::new(db, body.scope()).with_assumptions(self.assumptions()),
                inst,
                name,
            )
            && let Some(body_func) = method.body()
        {
            func = body_func;
        }
        let callee_sources = self.forwarded_return_param_sources_from_callable(db, func)?;
        let mut merged = FxHashSet::default();
        for callee_source in callee_sources {
            for source in self.forwarded_return_sources_from_call_source(
                db,
                expr,
                &callee_source,
                call_args,
                seen,
                visited_locals,
            )? {
                merged.insert(source);
            }
        }
        let mut out = merged.into_iter().collect::<Vec<_>>();
        out.sort_unstable();
        Some(out)
    }

    fn forwarded_return_sources_from_place(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        place: &Place<'db>,
        seen: &mut FxHashSet<Func<'db>>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Option<Vec<ReturnSource>> {
        let PlaceBase::Binding(binding) = place.base;
        let sources = match binding {
            LocalBinding::Param {
                site: ParamSite::Func(func),
                idx,
                ..
            } => vec![ReturnSource {
                result_projection: Vec::new(),
                origin: if func.is_method(db) && idx == 0 {
                    CallableInputLayoutHoleOrigin::Receiver
                } else {
                    CallableInputLayoutHoleOrigin::ValueParam(idx)
                },
                projection: Vec::new(),
            }],
            LocalBinding::Param {
                site: ParamSite::EffectField(_),
                idx,
                ..
            }
            | LocalBinding::EffectParam { idx, .. } => vec![ReturnSource {
                result_projection: Vec::new(),
                origin: CallableInputLayoutHoleOrigin::Effect(idx),
                projection: Vec::new(),
            }],
            binding @ LocalBinding::Local { pat, .. } => {
                if !visited_locals.insert(pat) {
                    return None;
                }
                let sources = self.binding_source(db, binding).and_then(|binding_source| {
                    let sources = self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        binding_source.init_expr,
                        seen,
                        visited_locals,
                    )?;
                    project_return_sources(sources, &binding_source.projection)
                });
                visited_locals.remove(&pat);
                sources?
            }
            LocalBinding::Param {
                site: ParamSite::ContractInit(_),
                ..
            } => return None,
        };
        let projection = place
            .projections
            .iter()
            .filter_map(|projection| match projection {
                PlaceProjection::Deref { .. } => None,
                PlaceProjection::Field { index, .. } => {
                    Some(Some(ReturnProjectionStep::Field(*index)))
                }
                PlaceProjection::Index { index_expr, .. } => {
                    Some(self.return_index_projection(db, body, *index_expr))
                }
            })
            .collect::<Option<Vec<_>>>()?;
        project_return_sources(sources, &projection)
    }

    fn forwarded_return_sources_from_call_source(
        &self,
        db: &'db dyn HirAnalysisDb,
        call_expr: ExprId,
        callee_source: &ReturnSource,
        call_args: &[ExprId],
        seen: &mut FxHashSet<Func<'db>>,
        visited_locals: &mut FxHashSet<PatId>,
    ) -> Option<Vec<ReturnSource>> {
        let body = self.body()?;
        let sources = match callee_source.origin {
            CallableInputLayoutHoleOrigin::Receiver => self
                .forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    *call_args.first()?,
                    seen,
                    visited_locals,
                ),
            CallableInputLayoutHoleOrigin::ValueParam(param_idx) => self
                .forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    *call_args.get(param_idx)?,
                    seen,
                    visited_locals,
                ),
            CallableInputLayoutHoleOrigin::Effect(effect_idx) => {
                let effect_arg = self
                    .call_effect_args(call_expr)?
                    .iter()
                    .find(|arg| arg.binding_idx as usize == effect_idx)?;
                match &effect_arg.arg {
                    EffectArg::Place(place) => self.forwarded_return_sources_from_place(
                        db,
                        body,
                        place,
                        seen,
                        visited_locals,
                    ),
                    EffectArg::Value(expr) => self.forwarded_return_param_sources_from_expr(
                        db,
                        body,
                        *expr,
                        seen,
                        visited_locals,
                    ),
                    EffectArg::Binding(binding) => self.forwarded_return_sources_from_place(
                        db,
                        body,
                        &Place::new(PlaceBase::Binding(*binding)),
                        seen,
                        visited_locals,
                    ),
                    EffectArg::Unknown => None,
                }
            }
        }?;
        let projection =
            self.instantiate_return_projection(db, body, &callee_source.projection, call_args)?;
        let mut sources = project_return_sources(sources, &projection)?;
        prefix_return_sources(&mut sources, &callee_source.result_projection);
        Some(sources)
    }

    fn return_index_projection(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
    ) -> Option<ReturnProjectionStep> {
        if let Partial::Present(Expr::Lit(LitKind::Int(value))) = expr.data(db, body) {
            return value
                .data(db)
                .to_usize()
                .map(ReturnProjectionStep::ConstantIndex);
        }
        match self.expr_binding(expr)? {
            LocalBinding::Param {
                site: ParamSite::Func(_),
                idx,
                ..
            } => Some(ReturnProjectionStep::ParamIndex(idx)),
            LocalBinding::Local { .. }
            | LocalBinding::Param { .. }
            | LocalBinding::EffectParam { .. } => None,
        }
    }

    fn instantiate_return_projection(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        projection: &[ReturnProjectionStep],
        call_args: &[ExprId],
    ) -> Option<Vec<ReturnProjectionStep>> {
        projection
            .iter()
            .map(|step| match *step {
                ReturnProjectionStep::ParamIndex(param_idx) => {
                    self.return_index_projection(db, body, *call_args.get(param_idx)?)
                }
                ReturnProjectionStep::AnyIndex => Some(ReturnProjectionStep::AnyIndex),
                step => Some(step),
            })
            .collect()
    }

    fn forwarded_return_param_sources_from_callable(
        &self,
        db: &'db dyn HirAnalysisDb,
        func: Func<'db>,
    ) -> Option<Vec<ReturnSource>> {
        match func_return_provenance(db, func) {
            ReturnProvenance::Forwarded(sources) => Some(sources.clone()),
            ReturnProvenance::Fresh | ReturnProvenance::Unknown => None,
        }
    }

    fn collect_explicit_return_param_sources_in_stmt(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        stmt: StmtId,
        out: &mut FxHashSet<ReturnSource>,
        saw_non_param: &mut bool,
        seen: &mut FxHashSet<Func<'db>>,
    ) {
        let Partial::Present(stmt_data) = stmt.data(db, body) else {
            return;
        };

        match stmt_data {
            crate::hir_def::Stmt::Let(_, _, Some(init)) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *init,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            crate::hir_def::Stmt::For(_, iter, loop_body, _) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *iter,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *loop_body,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            crate::hir_def::Stmt::While(cond, loop_body) => {
                self.collect_explicit_return_param_sources_in_cond(
                    db,
                    body,
                    *cond,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *loop_body,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            crate::hir_def::Stmt::Return(Some(expr)) => {
                let mut visited_locals = FxHashSet::default();
                if let Some(indices) = self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    *expr,
                    seen,
                    &mut visited_locals,
                ) {
                    out.extend(indices);
                } else {
                    *saw_non_param = true;
                }
            }
            crate::hir_def::Stmt::Expr(expr) => self.collect_explicit_return_param_sources_in_expr(
                db,
                body,
                *expr,
                out,
                saw_non_param,
                seen,
            ),
            crate::hir_def::Stmt::Let(_, _, None)
            | crate::hir_def::Stmt::Return(None)
            | crate::hir_def::Stmt::Continue
            | crate::hir_def::Stmt::Break => {}
        }
    }

    fn collect_explicit_return_param_sources_in_expr(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
        out: &mut FxHashSet<ReturnSource>,
        saw_non_param: &mut bool,
        seen: &mut FxHashSet<Func<'db>>,
    ) {
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return;
        };

        match expr_data {
            Expr::Block(stmts) => {
                for stmt in stmts {
                    self.collect_explicit_return_param_sources_in_stmt(
                        db,
                        body,
                        *stmt,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Bin(lhs, rhs, _) | Expr::Assign(lhs, rhs) | Expr::AugAssign(lhs, rhs, _) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *lhs,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *rhs,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            Expr::Un(inner, _) | Expr::Cast(inner, _) | Expr::Field(inner, _) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *inner,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            Expr::Call(callee, args) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *callee,
                    out,
                    saw_non_param,
                    seen,
                );
                for arg in args {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        arg.expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Assert(args) => {
                for arg in args {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        arg.expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::MethodCall(receiver, _, _, args) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *receiver,
                    out,
                    saw_non_param,
                    seen,
                );
                for arg in args {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        arg.expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::RecordInit(_, fields) => {
                for field in fields {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        field.expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Tuple(items) | Expr::Array(items) => {
                for item in items {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        *item,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::ArrayRep(value, _) => self.collect_explicit_return_param_sources_in_expr(
                db,
                body,
                *value,
                out,
                saw_non_param,
                seen,
            ),
            Expr::If(cond, then_expr, else_expr) => {
                self.collect_explicit_return_param_sources_in_cond(
                    db,
                    body,
                    *cond,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *then_expr,
                    out,
                    saw_non_param,
                    seen,
                );
                if let Some(else_expr) = else_expr {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        *else_expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Match(scrutinee, arms) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *scrutinee,
                    out,
                    saw_non_param,
                    seen,
                );
                if let Partial::Present(arms) = arms {
                    for arm in arms {
                        self.collect_explicit_return_param_sources_in_expr(
                            db,
                            body,
                            arm.body,
                            out,
                            saw_non_param,
                            seen,
                        );
                    }
                }
            }
            Expr::With(bindings, with_body) => {
                for binding in bindings {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        binding.value,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *with_body,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            Expr::Lit(_) | Expr::Path(_) | Expr::UnsupportedMacroCall => {}
        }
    }

    fn collect_implicit_return_param_sources_from_expr(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
        out: &mut FxHashSet<ReturnSource>,
        saw_non_param: &mut bool,
        seen: &mut FxHashSet<Func<'db>>,
    ) {
        let Partial::Present(expr_data) = expr.data(db, body) else {
            return;
        };

        match expr_data {
            Expr::Block(stmts) => {
                if let Some(last_stmt) = stmts.last()
                    && let Partial::Present(crate::hir_def::Stmt::Expr(tail_expr)) =
                        last_stmt.data(db, body)
                {
                    self.collect_implicit_return_param_sources_from_expr(
                        db,
                        body,
                        *tail_expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::If(_, then_expr, else_expr) => {
                self.collect_implicit_return_param_sources_from_expr(
                    db,
                    body,
                    *then_expr,
                    out,
                    saw_non_param,
                    seen,
                );
                if let Some(else_expr) = else_expr {
                    self.collect_implicit_return_param_sources_from_expr(
                        db,
                        body,
                        *else_expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
            }
            Expr::Match(_, arms) => {
                if let Partial::Present(arms) = arms {
                    for arm in arms {
                        self.collect_implicit_return_param_sources_from_expr(
                            db,
                            body,
                            arm.body,
                            out,
                            saw_non_param,
                            seen,
                        );
                    }
                }
            }
            Expr::With(_, with_body) => self.collect_implicit_return_param_sources_from_expr(
                db,
                body,
                *with_body,
                out,
                saw_non_param,
                seen,
            ),
            _ => {
                let mut visited_locals = FxHashSet::default();
                let Some(indices) = self.forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    expr,
                    seen,
                    &mut visited_locals,
                ) else {
                    *saw_non_param = true;
                    return;
                };
                out.extend(indices);
            }
        }
    }

    fn collect_explicit_return_param_sources_in_cond(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        cond: crate::hir_def::CondId,
        out: &mut FxHashSet<ReturnSource>,
        saw_non_param: &mut bool,
        seen: &mut FxHashSet<Func<'db>>,
    ) {
        let Partial::Present(cond_data) = cond.data(db, body) else {
            return;
        };

        match cond_data {
            crate::hir_def::Cond::Expr(expr) => self.collect_explicit_return_param_sources_in_expr(
                db,
                body,
                *expr,
                out,
                saw_non_param,
                seen,
            ),
            crate::hir_def::Cond::Let(_, value) => {
                self.collect_explicit_return_param_sources_in_expr(
                    db,
                    body,
                    *value,
                    out,
                    saw_non_param,
                    seen,
                );
            }
            crate::hir_def::Cond::Bin(lhs, rhs, _) => {
                self.collect_explicit_return_param_sources_in_cond(
                    db,
                    body,
                    *lhs,
                    out,
                    saw_non_param,
                    seen,
                );
                self.collect_explicit_return_param_sources_in_cond(
                    db,
                    body,
                    *rhs,
                    out,
                    saw_non_param,
                    seen,
                );
            }
        }
    }

    /// Get the definition span for an expression that references a local binding.
    ///
    /// Returns `Some(span)` if the expression references a local variable, parameter,
    /// or effect parameter. Returns `None` if the expression doesn't have a binding
    /// or if no body is available.
    ///
    /// This is used by the language server for goto-definition on local variables.
    pub fn expr_binding_def_span(&self, func: Func<'db>, expr: ExprId) -> Option<DynLazySpan<'db>> {
        let body = self.tables.body?;
        let binding = self.expr_binding(expr)?;
        Some(binding.def_span_with(body, func))
    }

    /// Like `expr_binding_def_span` but takes a `Body` directly.
    ///
    /// Use this when the body may not belong to a function (e.g., contract bodies).
    pub fn expr_binding_def_span_in_body(
        &self,
        body: Body<'db>,
        expr: ExprId,
    ) -> Option<DynLazySpan<'db>> {
        let binding = self.expr_binding(expr)?;
        Some(binding.def_span_in_body(body))
    }

    /// Get the binding kind for an expression that references a local binding.
    ///
    /// Returns the identity of the binding (param index, pattern id, or effect param ident).
    pub fn expr_binding(&self, expr: ExprId) -> Option<LocalBinding<'db>> {
        self.tables.expr_ty[expr]
            .as_ref()
            .and_then(|prop| prop.binding)
    }

    /// Returns a place representation for `expr` if it denotes an assignable location.
    pub fn expr_place(&self, expr: ExprId) -> Option<&Place<'db>> {
        self.tables.expr_place[expr]
            .expand()
            .and_then(|place_id| self.tables.expr_places.get(place_id))
    }

    /// Find all expressions that reference the same local binding as the given expression.
    ///
    /// Returns a list of ExprIds that share the same local binding (variable, parameter,
    /// or effect parameter). Returns an empty list if the expression doesn't have a binding.
    ///
    /// This is used by the language server for find-all-references and rename on local variables.
    pub fn local_references(&self, expr: ExprId) -> Vec<ExprId> {
        let Some(binding) = self.tables.expr_ty[expr]
            .as_ref()
            .and_then(|prop| prop.binding)
        else {
            return vec![];
        };

        self.tables
            .expr_ty
            .iter()
            .filter_map(|(id, prop)| {
                if prop.as_ref().and_then(|prop| prop.binding) == Some(binding) {
                    Some(id)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Find all expressions that reference a binding by its kind.
    ///
    /// This is the general method for finding all references to any kind of binding
    /// (param, local, or effect param).
    pub fn references_by_binding(&self, binding: LocalBinding<'db>) -> Vec<ExprId> {
        self.tables
            .expr_ty
            .iter()
            .filter_map(|(id, prop)| {
                if prop.as_ref().and_then(|prop| prop.binding) == Some(binding) {
                    Some(id)
                } else {
                    None
                }
            })
            .collect()
    }

    fn empty(db: &'db dyn HirAnalysisDb) -> Self {
        TypedBodyTables {
            body: None,
            result_ty: TyId::unit(db),
            assumptions: PredicateListId::empty_list(db),
            pat_ty: SecondaryMap::new(),
            expr_ty: SecondaryMap::new(),
            implicit_moves: FxHashSet::default(),
            const_refs: SecondaryMap::new(),
            value_path_refs: SecondaryMap::new(),
            semantic_expr_lowering: SecondaryMap::new(),
            record_init_lowering: SecondaryMap::new(),
            resolved_field_index: SecondaryMap::new(),
            call_effect_args: SecondaryMap::new(),
            return_borrow_provider: None,
            param_bindings: Vec::new(),
            pat_bindings: SecondaryMap::new(),
            pat_binding_modes: SecondaryMap::new(),
            pattern_store: PatternStore::default(),
            pattern_status: SecondaryMap::with_default(PatternAnalysisStatus::Invalid),
            for_loop_seq: SecondaryMap::new(),
            expr_place: SecondaryMap::new(),
            expr_places: PrimaryMap::new(),
            path_applications: Vec::new(),
        }
        .into()
    }
}

pub(super) fn manual_contract_root_ref_from_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<SemanticCodeRegionRef<'db>> {
    let ty = strip_code_region_token_wrapper(db, ty);
    let TyData::TyBase(TyBase::Func(CallableDef::Func(func))) = ty.base_ty(db).data(db) else {
        return None;
    };
    match func.manual_contract_root_attr(db)? {
        ManualContractRootAttr::Init { .. } | ManualContractRootAttr::Runtime { .. } => {
            Some(SemanticCodeRegionRef::ManualContractRoot { func: *func })
        }
        ManualContractRootAttr::Error(_) => None,
    }
}

/// Returns true if the trait-const goal's args contain generic params that
/// are not in scope at `scope`. Such params come from the selected impl's or
/// trait's own binder and are existentials awaiting inference (e.g.
/// `GenericForwardStruct<7>::B` selecting through
/// `impl<const N> GenericForward<N> for GenericForwardStruct<N>`); checking
/// the goal with them treated as rigid universals produces false
/// "unsatisfied bound" errors.
pub(super) fn trait_const_goal_has_foreign_params<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
) -> bool {
    use crate::analysis::ty::visitor::{TyVisitable, TyVisitor, walk_ty};

    fn is_enclosing_item<'db>(
        db: &'db dyn HirAnalysisDb,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        owner: crate::hir_def::scope_graph::ScopeId<'db>,
    ) -> bool {
        let owner_item = owner.item();
        let mut item = Some(scope.item());
        while let Some(current) = item {
            if current == owner_item {
                return true;
            }
            item = current.scope().parent_item(db);
        }
        false
    }

    struct ForeignParamFinder<'db> {
        db: &'db dyn HirAnalysisDb,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        found: bool,
    }

    impl<'db> TyVisitor<'db> for ForeignParamFinder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.found {
                return;
            }
            let param = match ty.data(self.db) {
                TyData::TyParam(param) => Some(param),
                TyData::ConstTy(const_ty) => match const_ty.data(self.db) {
                    crate::analysis::ty::const_ty::ConstTyData::TyParam(param, _) => Some(param),
                    _ => None,
                },
                _ => None,
            };
            if let Some(param) = param
                && !is_enclosing_item(self.db, self.scope, param.owner)
            {
                self.found = true;
                return;
            }
            walk_ty(self, ty);
        }
    }

    let mut finder = ForeignParamFinder {
        db,
        scope,
        found: false,
    };
    for &arg in inst.args(db) {
        arg.visit_with(&mut finder);
        if finder.found {
            return true;
        }
    }
    false
}

pub(super) fn ty_may_be_code_region_token<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    let ty = strip_code_region_token_wrapper(db, ty);
    manual_contract_root_ref_from_ty(db, ty).is_some()
        || matches!(
            ty.base_ty(db).data(db),
            TyData::TyParam(_) | TyData::TyVar(_)
        )
}

fn strip_code_region_token_wrapper<'db>(
    db: &'db dyn HirAnalysisDb,
    mut ty: TyId<'db>,
) -> TyId<'db> {
    while let Some((_, inner)) = ty.as_capability(db) {
        ty = inner;
    }
    ty
}

fn merge_forwarded_param_sets(
    lhs: Option<Vec<ReturnSource>>,
    rhs: Option<Vec<ReturnSource>>,
) -> Option<Vec<ReturnSource>> {
    let mut merged = FxHashSet::default();
    for source in lhs?.into_iter().chain(rhs?) {
        merged.insert(source);
    }
    let mut out = merged.into_iter().collect::<Vec<_>>();
    out.sort_unstable();
    Some(out)
}

fn return_projection_step_matches(
    pattern: ReturnProjectionStep,
    candidate: ReturnProjectionStep,
) -> bool {
    pattern == candidate
        || matches!(
            (pattern, candidate),
            (
                ReturnProjectionStep::AnyIndex,
                ReturnProjectionStep::AnyIndex
                    | ReturnProjectionStep::ConstantIndex(_)
                    | ReturnProjectionStep::ParamIndex(_)
            )
        )
}

fn return_projection_is_prefix(
    prefix: &[ReturnProjectionStep],
    path: &[ReturnProjectionStep],
) -> bool {
    prefix.len() <= path.len()
        && prefix
            .iter()
            .copied()
            .zip(path.iter().copied())
            .all(|(pattern, candidate)| return_projection_step_matches(pattern, candidate))
}

fn project_return_sources(
    sources: Vec<ReturnSource>,
    projection: &[ReturnProjectionStep],
) -> Option<Vec<ReturnSource>> {
    let mut projected = FxHashSet::default();
    for mut source in sources {
        if return_projection_is_prefix(&source.result_projection, projection) {
            source
                .projection
                .extend_from_slice(&projection[source.result_projection.len()..]);
            source.result_projection.clear();
            projected.insert(source);
        } else if return_projection_is_prefix(projection, &source.result_projection) {
            source.result_projection.drain(..projection.len());
            projected.insert(source);
        }
    }
    let mut projected = projected.into_iter().collect::<Vec<_>>();
    projected.sort_unstable();
    (!projected.is_empty()).then_some(projected)
}

fn prefix_return_sources(sources: &mut [ReturnSource], prefix: &[ReturnProjectionStep]) {
    for source in sources {
        let mut result_projection = prefix.to_vec();
        result_projection.append(&mut source.result_projection);
        source.result_projection = result_projection;
    }
}

#[derive(Clone, PartialEq, Eq, derive_more::From)]
enum Typeable<'db> {
    Expr(ExprId, ExprProp<'db>),
    Pat(PatId),
}

impl Typeable<'_> {
    fn span(self, body: Body) -> DynLazySpan {
        match self {
            Self::Expr(expr, ..) => expr.span(body).into(),
            Self::Pat(pat) => pat.span(body).into(),
        }
    }
}

fn try_instantiate_trait_method<'db>(
    db: &'db dyn HirAnalysisDb,
    method: Func<'db>,
    table: &mut UnificationTable<'db>,
    receiver_ty: TyId<'db>,
    inst: TraitInstId<'db>,
) -> Result<TyId<'db>, UnificationError> {
    // Positional application is sound here because the args and the binder
    // come from the same definition: a trait method's leading binder params
    // are its trait's params in declaration order, and `inst.args` are
    // exactly their instantiations. Positionally bridging *across*
    // definitions (e.g. a receiver's type args into an impl's binder) is
    // not — an impl's self type is an arbitrary pattern over its params;
    // that binding comes from the method-table probe's unification.
    let ty = TyId::foldl(
        db,
        TyId::func(db, method.as_callable(db).unwrap()),
        inst.args(db),
    );

    let inst_self = table.instantiate_to_term(inst.self_ty(db));
    table.unify(inst_self, receiver_ty)?;

    Ok(normalize_with_trait_evidence(db, ty, method.scope(), inst))
}

fn instantiate_trait_method<'db>(
    db: &'db dyn HirAnalysisDb,
    method: Func<'db>,
    table: &mut UnificationTable<'db>,
    receiver_ty: TyId<'db>,
    inst: TraitInstId<'db>,
) -> TyId<'db> {
    try_instantiate_trait_method(db, method, table, receiver_ty, inst)
        .expect("selected trait method candidate receiver should unify")
}

/// Instantiate a trait-associated function type (no receiver), e.g. `T::make`.
fn instantiate_trait_assoc_fn<'db>(
    db: &'db dyn HirAnalysisDb,
    method: CallableDef<'db>,
    inst: TraitInstId<'db>,
) -> TyId<'db> {
    // Same-definition positional application: see
    // `try_instantiate_trait_method`.
    let ty = TyId::foldl(db, TyId::func(db, method), inst.args(db));

    normalize_with_trait_evidence(db, ty, method.scope(), inst)
}

struct TyCheckerFinalizer<'db> {
    db: &'db dyn HirAnalysisDb,
    body: TypedBody<'db>,
    assumptions: PredicateListId<'db>,
    ty_vars: FxHashSet<InferenceKey<'db>>,
    effect_provider_keys: FxHashSet<InferenceKey<'db>>,
    direct_call_callees: FxHashSet<ExprId>,
    diags: Vec<FuncBodyDiag<'db>>,
}

impl<'db> Visitor<'db> for TyCheckerFinalizer<'db> {
    fn visit_pat(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyPatSpan<'db>>,
        pat: PatId,
        _: &Pat<'db>,
    ) {
        let ty = self.body.pat_ty(self.db, pat);
        let span = ctxt.span().unwrap();
        self.check_unknown(ty, span.clone().into());

        walk_pat(self, ctxt, pat)
    }

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
        expr: ExprId,
        expr_data: &Expr<'db>,
    ) {
        // Skip the check if the expr is block.
        if !matches!(expr_data, Expr::Block(..)) {
            let prop = self.body.expr_prop(self.db, expr);
            let span = ctxt.span().unwrap();
            self.check_unknown(prop.ty, span.clone().into());
            // A string literal only gets an oversized `String<N>` type through
            // inference fallback; report it here instead of leaving an invalid
            // literal type for constant evaluation to trip over.
            if let Expr::Lit(LitKind::String(_)) = expr_data
                && let TyData::Invalid(InvalidCause::StringTooLarge { max, given }) =
                    prop.ty.data(self.db)
            {
                let diag = TyLowerDiag::StringTooLarge {
                    span: span.clone().into(),
                    max: *max,
                    given: *given,
                };
                self.diags.push(TyDiagCollection::from(diag).into());
            }
            let is_direct_call_callee =
                matches!(expr_data, Expr::Path(..)) && self.direct_call_callees.contains(&expr);
            if prop.binding.is_none() && !is_direct_call_callee {
                self.check_wf(prop.ty, span.into());
            }
        }

        // We need this additional check for method call because the callable type is
        // not tied to the expression type. Well-formedness is not re-checked
        // here: the callable's declared constraints were already registered as
        // call-constraint obligations (with a `required by this bound` note),
        // so a WF check would report every unsatisfied bound a second time —
        // the same reason direct-call callees are excluded above.
        if let Expr::MethodCall(..) = expr_data
            && let Some(callable) = self.body.callable_expr(expr)
        {
            let callable_ty = callable.ty(self.db);
            let span = ctxt.span().unwrap().into_method_call_expr().method_name();
            self.check_unknown(callable_ty, span.into());
        }

        walk_expr(self, ctxt, expr);
    }

    fn visit_item(
        &mut self,
        _: &mut VisitorCtxt<'db, crate::core::visitor::prelude::LazyItemSpan<'db>>,
        _: crate::core::hir_def::ItemKind<'db>,
    ) {
    }
}

impl<'db> TyCheckerFinalizer<'db> {
    fn new(mut checker: TyChecker<'db>) -> Self {
        let assumptions = checker.env.assumptions();
        checker.resolve_deferred();
        let mut body = checker.env.finish(&mut checker.table);
        body.tables_mut().return_borrow_provider = checker
            .first_return_borrow_provider
            .map(|(_, provider)| provider);
        let direct_call_callees = body.body().map_or_else(FxHashSet::default, |body_id| {
            body_id
                .exprs(checker.db)
                .iter()
                .filter_map(|(_expr, expr_data)| match expr_data {
                    Partial::Present(Expr::Call(callee, ..)) => Some(*callee),
                    _ => None,
                })
                .collect()
        });

        Self {
            db: checker.db,
            body,
            assumptions,
            ty_vars: FxHashSet::default(),
            effect_provider_keys: checker.effect_provider_keys,
            direct_call_callees,
            diags: checker.diags,
        }
    }

    fn finish(mut self) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
        self.check_unknown_types();
        (self.diags, self.body)
    }

    fn check_unknown_types(&mut self) {
        if let Some(body) = self.body.body() {
            let mut ctxt = VisitorCtxt::with_body(self.db, body);
            self.visit_body(&mut ctxt, body);
        }
    }

    fn check_unknown(&mut self, ty: TyId<'db>, span: DynLazySpan<'db>) {
        let flags = ty.flags(self.db);
        if flags.contains(TyFlags::HAS_INVALID) || !flags.contains(TyFlags::HAS_VAR) {
            return;
        }

        let keys = inference_keys(self.db, &ty);
        if !keys.is_empty()
            && keys
                .iter()
                .all(|key| self.effect_provider_keys.contains(key))
        {
            return;
        }

        let mut skip_diag = false;
        for key in keys {
            // If at least one of the inference keys are already seen, we will skip emitting
            // diagnostics.
            skip_diag |= !self.ty_vars.insert(key);
        }

        if !skip_diag {
            let diag = BodyDiag::TypeAnnotationNeeded { span, ty };
            self.diags.push(diag.into())
        }
    }

    fn check_wf(&mut self, ty: TyId<'db>, span: DynLazySpan<'db>) {
        let flags = ty.flags(self.db);
        if flags.contains(TyFlags::HAS_INVALID) || flags.contains(TyFlags::HAS_VAR) {
            return;
        }

        let solve_cx = TraitSolveCx::new(self.db, self.body.body().unwrap().scope());
        if let Some(diag) = ty.emit_wf_diag(self.db, solve_cx, self.assumptions, span) {
            self.diags.push(diag.into());
        }
    }
}

#[cfg(test)]
mod capability_hint_tests {
    use crate::{
        analysis::ty::{
            diagnostics::{FuncBodyDiag, TraitConstraintDiag, TyDiagCollection},
            trait_resolution::{
                GoalSatisfiability, TraitSolveCompletion, TraitSolveCx, is_goal_satisfiable,
            },
            ty_def::CapabilityKind,
        },
        test_db::{HirAnalysisTestDb, find_func},
    };

    use super::{check_func_body, declared_call_bound};

    #[test]
    fn ambiguous_owned_bound_keeps_the_ordinary_capability_diagnostic() {
        let mut db = HirAnalysisTestDb::default();
        // Query the body and solver directly: coherence checking rejects the
        // overlapping owned impls before an ordinary source fixture can isolate
        // this diagnostic boundary.
        let file = db.new_stand_alone(
            "ambiguous_owned_capability_hint.fe".into(),
            r#"
trait Mark {}
trait Foo {}
struct S { x: u256 }
impl Mark for S {}
struct W<A> { a: A }
struct P<A, B> { a: A, b: B }
impl<A: Mark, B: Mark> Foo for P<A, B> {}
impl Foo for P<S, S> {}

fn need<X>(_ w: W<X>) where P<X, X>: Foo {}
fn borrowed(_ s: ref S) { need(W { a: s }) }
fn owned_bound() where P<S, S>: Foo {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let owned_bound = find_func(&db, top_mod, "owned_bound");
        let owned_goal = declared_call_bound(&db, owned_bound.into(), 0).expect("owned bound");
        assert!(matches!(
            is_goal_satisfiable(&db, TraitSolveCx::new(&db, top_mod.scope()), owned_goal),
            GoalSatisfiability::NeedsConfirmation {
                solutions,
                completion: TraitSolveCompletion::Saturated,
            } if solutions.len() == 2
        ));

        let borrowed = find_func(&db, top_mod, "borrowed");
        let (diags, _) = check_func_body(&db, borrowed);
        let [
            FuncBodyDiag::Ty(TyDiagCollection::Satisfiability(
                TraitConstraintDiag::TraitBoundNotSat {
                    primary_goal,
                    unsat_subgoal: Some(unsat),
                    capability_hint,
                    ..
                },
            )),
        ] = diags.as_slice()
        else {
            panic!("expected one direct capability-bound failure, got {diags:?}");
        };
        assert_eq!(primary_goal.pretty_print(&db, true), "P<ref S, ref S>: Foo");
        assert_eq!(unsat.pretty_print(&db, true), "ref S: Mark");
        assert_eq!(
            unsat.self_ty(&db).as_capability(&db).map(|(kind, _)| kind),
            Some(CapabilityKind::Ref)
        );
        assert_eq!(*capability_hint, None);
    }
}

#[cfg(test)]
mod smir_lowering_readiness_tests {
    use camino::Utf8PathBuf;

    use crate::{
        analysis::ty::pattern_ir::PatternAnalysisStatus,
        hir_def::{Expr, ItemKind, Partial},
        test_db::HirAnalysisTestDb,
    };

    use super::{SmirLoweringReadiness, check_func_body};

    #[test]
    fn unrelated_diagnostic_does_not_hide_an_incomplete_lowering_plan() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("smir_lowering_readiness.fe"),
            r#"
fn callee() -> u256 {
    1
}

fn target() -> u256 {
    let wrong: u256 = true
    callee()
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let target = top_mod
            .all_items(&db)
            .iter()
            .find_map(|item| match item {
                ItemKind::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "target") =>
                {
                    Some(*func)
                }
                _ => None,
            })
            .expect("target function");
        let (diags, checked) = check_func_body(&db, target);
        assert!(
            !diags.is_empty(),
            "fixture must have an unrelated type error"
        );
        assert_eq!(
            checked.smir_lowering_readiness(&db),
            SmirLoweringReadiness::Ready
        );

        let mut incomplete = checked.clone();
        let body = incomplete.body().expect("target body");
        let call = body
            .exprs(&db)
            .iter()
            .find_map(|(expr, data)| {
                matches!(data, Partial::Present(Expr::Call(..))).then_some(expr)
            })
            .expect("target call");
        assert!(incomplete.semantic_expr_lowering(call).is_some());
        incomplete.tables_mut().semantic_expr_lowering[call] = None;

        assert_eq!(
            incomplete.smir_lowering_readiness(&db),
            SmirLoweringReadiness::IncompletePlan
        );
    }

    #[test]
    fn incomplete_match_pattern_dominates_invalid_pattern_in_any_arm_order() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("smir_match_readiness.fe"),
            r#"
enum Choice {
    First,
    Second,
}

fn target(choice: Choice) -> u256 {
    match choice {
        Choice::First => 0,
        Choice::Second => 1,
    }
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let target = top_mod
            .all_items(&db)
            .iter()
            .find_map(|item| match item {
                ItemKind::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "target") =>
                {
                    Some(*func)
                }
                _ => None,
            })
            .expect("target function");
        let (_, checked) = check_func_body(&db, target);
        let body = checked.body().expect("target body");
        let arms = body
            .exprs(&db)
            .iter()
            .find_map(|(_, data)| match data {
                Partial::Present(Expr::Match(_, Partial::Present(arms))) => Some(arms),
                _ => None,
            })
            .expect("target match");
        assert_eq!(arms.len(), 2);

        for (invalid, unsupported) in [(0, 1), (1, 0)] {
            let mut mixed = checked.clone();
            mixed.tables_mut().pattern_status[arms[invalid].pat] = PatternAnalysisStatus::Invalid;
            mixed.tables_mut().pattern_status[arms[unsupported].pat] =
                PatternAnalysisStatus::Unsupported;
            assert_eq!(
                mixed.smir_lowering_readiness(&db),
                SmirLoweringReadiness::IncompletePlan
            );
        }
    }
}
