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
mod projection;
pub(crate) use projection::constructed_variant;
mod stmt;

pub use self::contract::{
    RecvArmLookup, RecvArmResolution, RecvArmVariant, RecvMsg, RecvMsgResolution,
    check_contract_field_address_spaces, check_contract_field_slots,
    check_contract_immutable_fields_initialized, check_contract_init_body,
    check_contract_recv_arm_body, check_contract_recv_block, check_contract_recv_blocks,
    recv_arm_resolution, recv_msg,
};
pub(crate) use self::contract::{contract_field_slot, eval_msg_variant_selector};
pub(crate) use self::env::{func_declared_bounds, func_signature};
pub use self::path::RecordLike;
use crate::analysis::name_resolution::{ResolvedVariant, is_scope_visible_from};
pub use crate::analysis::ty::AddressSpace;
use crate::analysis::ty::adt_def::AdtRef;
use crate::analysis::ty::const_check::const_body_language_failure;
use crate::analysis::ty::corelib::resolve_lib_type_path;
use crate::analysis::ty::fold::TyFoldable;
use crate::analysis::ty::method_table::ProbedMethod;
use crate::analysis::ty::provider::{ProviderKind, provider_semantics};
use crate::analysis::ty::shape::{Shape, maybe_mut_kind, view_param_misuse};
use crate::analysis::ty::trait_lower::lower_impl_trait;
use crate::analysis::ty::trait_resolution::constraint::{
    PredicateSource, collect_func_decl_constraint_pairs,
};
use crate::analysis::ty::visitor::{TyVisitable, TyVisitor, walk_const_ty, walk_ty};
use crate::hir_def::{
    CallArg, CallableDef, FieldParent, GenericArgListId, ImplTrait, ModeMarker, Trait,
    params::FuncParamMode, scope_graph::ScopeId,
};
use crate::{
    hir_def::{
        ArithBinOp, BinOp, BindingMarker, Body, CondId, Const, Contract, ContractRecvArm, Expr,
        ExprId, Func, GenericParam, GenericParamOwner, LitKind, ManualContractRootAttr, Partial,
        Pat, PatId, PathId, StaticAssert, StaticAssertComparison, Stmt, StmtId, StringId,
        TypeId as HirTyId, TypeKind, UnOp, WhereClauseOwner,
    },
    span::{
        DynLazySpan, expr::LazyExprSpan, pat::LazyPatSpan, path::LazyPathSpan, types::LazyTySpan,
    },
    visitor::{Visitor, VisitorCtxt, walk_expr, walk_pat},
};
use callable::{CallGenericArgPhase, CallGenericArgUnifyError, unify_explicit_call_generic_args};
pub use callable::{CallParam, Callable, EffectProviderProvenance, EffectProviderSpecialization};
use common::indexmap::IndexMap;
use cranelift_entity::{PrimaryMap, SecondaryMap, entity_impl, packed_option::PackedOption};
use ena::unify::InPlace;
pub use env::{
    BindingAccess, ClosureCapture, ClosureInfo, EffectParamSite, ExprProp, LocalBinding, ParamSite,
    PatBindingMode, PathReadSemantics,
};
use env::{ProvidedEffect, TyCheckEnv, func_param_bindings};
pub(super) use expr::TraitOps;
use num_traits::ToPrimitive;
pub use owner::BodyOwner;
pub use owner::EffectParamOwner;
use std::{iter, sync::Arc};
pub use stmt::{ForLoopCall, ForLoopPlan, ForLoopStep};

use rustc_hash::{FxHashMap, FxHashSet};
use salsa::SalsaValue;

use crate::analysis::place::{
    Place, PlaceBase, PlaceProjection, is_grant_place_expr, is_pointer_place_expr,
};

use super::{
    adt_def::ConcreteTypeView,
    assoc_const::{AssocConstUse, InherentConstUse},
    canonical::Canonical,
    diagnostics::{
        BodyDiag, CallConstraintDiagInfo, FuncBodyDiag, MarkerPosition, RevealedBy,
        StaticAssertComparisonValues, TraitConstraintDiag, TyDiagCollection, TyLowerDiag,
    },
    effects::{EffectKeyKind, ResolvedEffectKey, resolve_effect_key, rows::RowPath},
    evidence::{
        Evidence, EvidenceId, EvidenceList, declared_bound_evidence, impl_const_bound_evidence,
        instantiated_selection,
    },
    generic_defaults::{GenericDefault, default_assumptions, generic_default},
    instantiate_trait_self,
    trait_def::{TraitInstId, selected_assoc_const_body_template},
    trait_resolution::{
        CanonicalGoalQuery, GoalSatisfiability, PredicateListId, TraitSolveCx, WellFormedness,
        check_trait_inst_wf_without_self, check_ty_wf, goal_query_has_no_distinct_solution,
        is_goal_query_satisfiable, is_goal_satisfiable,
    },
    ty_def::{
        BorrowKind, InvalidCause, Kind, MAX_INLINE_STRING_BYTES, StringFallback, TyId, TyVarSort,
    },
    ty_lower::{CompleteSubst, collect_generic_params, lower_hir_ty, lower_hir_ty_deferred},
    unify::{InferenceKey, Snapshot, UnificationError, UnificationTable},
    unsafe_check::check_unsafe_ops,
};
use crate::analysis::semantic::SemanticCodeRegionRef;
use crate::analysis::semantic::{
    BinaryOp, BlockedInfo, ConstDependency, ConstUsePolicy, CtfeConfig, CtfeError,
    EffectProviderSubst, EvalFailure, EvalOutcome, GenericSubst, ImplEnv, Overflow,
    RuntimeSizeError, SemConstId, SemConstScalar, SemConstValue, SemOrigin, SemanticInstance,
    SemanticInstanceKey, UnaryOp, const_computation_for_instance, describe_const_computation,
    eval_body_owner_const, eval_const_instance, reify_runtime_const_for_ty,
    runtime_size_bytes_with_source,
};
use crate::analysis::ty::ty_def::{ClosureDef, ClosureTy, TyBase, TyData};
use crate::analysis::ty::{
    closure::{CallableTrait, CallableValue},
    const_ty::{
        CallableInputOrigin, ConstTyData, ConstTyId, LoweringContext, const_body_assumptions,
        invalid_cause_from_eval_failure, origin_expr_for_const_eval_diag, root_ctfe_error,
    },
    normalize::{normalize_ty, normalize_with_trait_evidence},
    pattern_ir::{
        ConstructorKind, PatternAnalysisStatus, PatternStore, ValidatedPatId, ValidatedPatKind,
    },
    ty_error::{
        collect_hir_ty_diags, collect_ty_lower_errors, diag_from_invalid_cause,
        normalization_limit_diag, path_arg_diags, qualified_path_wf_diags,
    },
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        ExpectedPathKind, PathRes, PathResError, PathResErrorKind, diagnostics::PathResDiag,
        resolve_path, resolve_path_with_observer_and_minter,
    },
    ty::{
        ty_def::{TyFlags, inference_keys},
        visitor::collect_flags,
    },
};

/// A function's checked body. A `mut?` function's bodies, one per mode,
/// report together, a diagnostic both report once; the typed body is the
/// `ref` one's.
#[salsa::tracked]
pub fn check_func_body<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let mut bodies = BodyOwner::func_bodies(db, func).map(|owner| check_body(db, owner));
    let (mut diags, typed) = bodies.next().expect("a function has a body");
    for diag in bodies.flat_map(|(diags, _)| diags) {
        // An effect the `ref` body's call misses at `ref` and the `mut`
        // body's at `mut` is missing at `mut?`.
        if let FuncBodyDiag::Body(BodyDiag::MissingEffect {
            primary,
            func: callee,
            key,
            mode: ModeMarker::Mut,
            in_drop,
        }) = &diag
            && let Some(FuncBodyDiag::Body(BodyDiag::MissingEffect { mode, .. })) =
                diags.iter_mut().find(|other| {
                    **other
                        == FuncBodyDiag::Body(BodyDiag::MissingEffect {
                            primary: primary.clone(),
                            func: *callee,
                            key: *key,
                            mode: ModeMarker::None,
                            in_drop: *in_drop,
                        })
                })
        {
            *mode = ModeMarker::MaybeMut;
            continue;
        }
        if !diags.contains(&diag) {
            diags.push(diag);
        }
    }
    (diags, typed)
}

#[salsa::tracked]
pub fn check_const_body<'db>(
    db: &'db dyn HirAnalysisDb,
    const_: Const<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let (mut diags, body) = check_body(db, BodyOwner::Const(const_));
    // An invalid declared type suppresses body/value checking. Diagnose the
    // declaration itself rather than passing an invalid constant to CTFE.
    if let Some(hir_ty) = const_.hir_ty(db) {
        let ty_diags = super::ty_error::collect_hir_ty_diags(
            db,
            const_.scope(),
            hir_ty,
            const_.span().ty(),
            PredicateListId::empty_list(db),
        );
        if ty_diags.is_empty() {
            diags.extend(
                const_type_diag(db, const_.span().ty().into(), const_.ty(db)).map(Into::into),
            );
        }
        diags.extend(ty_diags.into_iter().map(Into::into));
    }
    (diags, body)
}

/// A const is an owned value, copied at each use, so its type is neither a
/// `#[view]` type, whose values are made only in its own module, nor a
/// state-only one.
pub(crate) fn const_type_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    span: DynLazySpan<'db>,
    ty: TyId<'db>,
) -> Option<TyDiagCollection<'db>> {
    if let Some(view) = ty.is_view(db).then_some(ty).or_else(|| ty.view_part(db)) {
        return Some(
            TyLowerDiag::ViewTypeMode {
                span,
                ty: view,
                mode: "the type of a `const`",
            }
            .into(),
        );
    }
    let collection = ty.pinned_part(db)?;
    Some(
        TyLowerDiag::StateOnlyValue {
            span,
            ty,
            collection,
            position: "the type of a `const`",
        }
        .into(),
    )
}

#[salsa::tracked]
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
#[salsa::tracked]
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
#[salsa::tracked]
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
#[salsa::tracked]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub(crate) struct DefaultConstBodyCheck<'db> {
    pub(crate) body: Body<'db>,
    pub(crate) expected: TyId<'db>,
    pub(crate) diagnostics: Vec<FuncBodyDiag<'db>>,
    pub(crate) ctfe_ready: bool,
}

/// Name/type checking stays separate from const execution so structural
/// default discovery can remain cycle-free and checked symbolic defaults can
/// retain their concrete-evaluation obligations.
#[salsa::tracked]
pub(crate) fn check_generic_default_body_types<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    param_idx: usize,
) -> Vec<DefaultConstBodyCheck<'db>> {
    let Ok(Some(default)) = generic_default(db, owner, param_idx) else {
        return Vec::new();
    };
    let bodies = match default {
        GenericDefault::Const { value, expected } => {
            vec![(*value, expected.instantiate_identity())]
        }
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
    condition_outcome_from(db, owner, eval_body_owner_const(db, owner, subst))
}

/// `condition_outcome` with `impl_env` deciding the impls `subst` selects:
/// a substitution that mentions a caller's parameters selects impls under
/// the caller's assumptions.
pub(super) fn condition_outcome_in<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    subst: GenericSubst<'db>,
    impl_env: ImplEnv<'db>,
) -> ConditionOutcome<'db> {
    let key = SemanticInstanceKey::of(
        db,
        owner,
        subst,
        EffectProviderSubst::empty(db),
        impl_env,
        EvidenceList::none(db),
    );
    let outcome = eval_const_instance(db, SemanticInstance::new(db, key));
    condition_outcome_from(db, owner, outcome)
}

fn condition_outcome_from<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    outcome: EvalOutcome<'db, SemConstId<'db>>,
) -> ConditionOutcome<'db> {
    match outcome {
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
            let arg_diags = || path_arg_diags(db, *path, path_span.clone(), scope, assumptions);
            match resolve_path(db, *path, scope, assumptions, false) {
                Ok(PathRes::Ty(_) | PathRes::TyAlias(..)) => {
                    collect_hir_ty_diags(db, scope, key, span, assumptions)
                }
                Ok(PathRes::Trait(_)) => arg_diags(),
                // A fault in the reference's own arguments, such as a const
                // argument failing evaluation, is reported where it is written.
                Err(err)
                    if !matches!(
                        err.kind,
                        PathResErrorKind::NotFound { .. }
                            | PathResErrorKind::TraitGenericArgType { .. }
                    ) =>
                {
                    err.into_diag(db, *path, path_span, ExpectedPathKind::Trait)
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

/// The diagnostic for an effect key that names a type or trait whose
/// arguments do not satisfy their bounds. A trait key's `Self` is its
/// provider, which answers for the trait's requirements on it; a default the
/// key applies names it as the trait's own `Self`.
pub(crate) fn effect_key_wf_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: EffectParamOwner<'db>,
    idx: usize,
    key: &ResolvedEffectKey<'db>,
    solve_cx: TraitSolveCx<'db>,
) -> Option<FuncBodyDiag<'db>> {
    let wf = match key {
        ResolvedEffectKey::Type(schema) => check_ty_wf(db, solve_cx, schema.carrier),
        ResolvedEffectKey::Trait(schema) => check_trait_inst_wf_without_self(
            db,
            solve_cx,
            instantiate_trait_self(
                db,
                schema.clone().into_trait_inst(db),
                schema.def.self_param(db),
            ),
        ),
        ResolvedEffectKey::Row(_) | ResolvedEffectKey::Invalid | ResolvedEffectKey::Other => {
            return None;
        }
    };
    let WellFormedness::IllFormed { goal, subgoal } = wf else {
        return None;
    };
    Some(FuncBodyDiag::Ty(
        TraitConstraintDiag::TraitBoundNotSat {
            span: owner.effect_param_ty_span(db, idx).into(),
            primary_goal: goal,
            unsat_subgoal: subgoal,
            required_by: None,
        }
        .into(),
    ))
}

#[salsa::tracked]
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
        ) || matches!(diag, FuncBodyDiag::Body(BodyDiag::MutAccessCopied { .. }))
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

/// The checked body of `owner`, with its own diagnostics: one mode's of a
/// `mut?` function.
pub fn check_body<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let CheckedBody { diags, typed, .. } = checked_body(db, owner);
    (diags, typed)
}

/// How the bindings of a pattern take what its source grants.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum AccessBinding {
    /// A place matched by component: a `ref` or `mut` binding opens that
    /// access on its component's path, and the others take its value.
    Place,
    /// A single name bound to an access its initializer names (`ref p`,
    /// `mut p`): the binding is that access.
    Named,
    /// A projection's result, or the components of a named access: each
    /// binding takes a value or an access by `TyChecker::binding_access_kind`.
    Result,
    /// A projection's result bound whole to the single name of a `let`: a
    /// `mut` result of a type with no values stays the access, as `let s`
    /// of a call returning `mut MemSpan`.
    LetResult,
    /// A destructured parameter's parts, bound as a result's are; a name
    /// binds an opaque part whole, as an opaque parameter is.
    Param,
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
    #[returns(copy)]
    owner: BodyOwner<'db>,
}

#[salsa::tracked(cycle_initial=infer_body_cycle_initial)]
fn infer_body_query<'db>(
    db: &'db dyn HirAnalysisDb,
    key: BodyInferenceKey<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let owner = key.owner(db);
    // A closure's body is checked inline in its parent's.
    if let BodyOwner::Closure { ty, def, receiver } = owner {
        let body = BodyOwner::closure_parent(db, def).map_or_else(
            || TypedBody::empty(db),
            |parent| infer_body(db, parent).1.for_closure(db, ty, receiver),
        );
        return (Vec::new(), body);
    }
    let Ok(mut checker) = TyChecker::new(db, owner) else {
        return (
            Vec::new(),
            match owner {
                BodyOwner::Func(func, mode) => typed_body_for_bodyless_func(db, func, mode),
                BodyOwner::Const(_)
                | BodyOwner::AnonConstBody { .. }
                | BodyOwner::ContractInit { .. }
                | BodyOwner::ContractRecvArm { .. }
                | BodyOwner::Closure { .. } => TypedBody::empty(db),
            },
        );
    };

    checker.run();
    let (mut diags, typed_body) = checker.finish();
    diags.extend(check_unsafe_ops(db, owner, &typed_body));
    if let BodyOwner::Func(func, mode) = owner
        && func.is_const(db)
        && !func.is_extern(db)
    {
        diags.extend(crate::analysis::ty::const_check::check_const_fn_body(
            db,
            func,
            mode,
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
    _: salsa::Id,
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
#[salsa::tracked]
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
        let key = SemanticInstanceKey::of(
            db,
            owner,
            GenericSubst::none(db),
            EffectProviderSubst::empty(db),
            ImplEnv::empty(db, owner.scope()),
            EvidenceList::none(db),
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
                && matches!(root_ctfe_error(db, owner, error).0, BodyOwner::Func(..))
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
                let key = SemanticInstanceKey::of(
                    db,
                    owner,
                    GenericSubst::none(db),
                    EffectProviderSubst::empty(db),
                    ImplEnv::empty(db, owner.scope()),
                    EvidenceList::none(db),
                );
                let semantic = SemanticInstance::new(db, key);
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
            InvalidCause::ConstEvalPanicked { code, .. } => format!("panicked with code {code:#x}"),
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
            SemOrigin::Expr(_) | SemOrigin::Stmt(_) | SemOrigin::Pat(_) | SemOrigin::Synthetic => {
                None
            }
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
    mode: Option<BorrowKind>,
) -> TypedBody<'db> {
    let assumptions = crate::semantic::func_body_assumptions(db, func)
        .at_mode(db, mode)
        .extend_all_bounds(db);
    let mut result_ty = func.return_ty(db);
    if !result_ty.is_star_kind(db) {
        result_ty = TyId::invalid(db, InvalidCause::Other);
    }
    let bindings = func_param_bindings(db, func, mode);
    TypedBodyTables {
        body: None,
        result_ty,
        signature: assumptions,
        assumptions,
        pat_ty: SecondaryMap::new(),
        expr_ty: SecondaryMap::new(),
        yield_shapes: SecondaryMap::new(),
        place_entries: FxHashSet::default(),
        unsafe_splits: FxHashSet::default(),
        product_args: FxHashSet::default(),
        value_calls: FxHashSet::default(),
        builtin_ops: FxHashMap::default(),
        operator_assigns: FxHashSet::default(),
        matched_places: FxHashSet::default(),
        inspected_places: FxHashSet::default(),
        covered_providers: FxHashSet::default(),
        const_refs: SecondaryMap::new(),
        value_path_refs: SecondaryMap::new(),
        semantic_expr_lowering: SecondaryMap::new(),
        record_init_lowering: SecondaryMap::new(),
        closure_infos: SecondaryMap::new(),
        resolved_field_index: SecondaryMap::new(),
        call_effect_args: SecondaryMap::new(),
        return_borrow_provider: None,
        param_bindings: bindings.params,
        input_bindings: bindings.inputs,
        param_products: bindings.products,
        transport_parts: Vec::new(),
        pat_bindings: SecondaryMap::new(),
        pat_binding_modes: SecondaryMap::new(),
        pattern_store: PatternStore::default(),
        pattern_status: SecondaryMap::with_default(PatternAnalysisStatus::Invalid),
        for_loop_plans: SecondaryMap::new(),
        expr_place: SecondaryMap::new(),
        expr_places: PrimaryMap::new(),
        path_applications: Vec::new(),
    }
    .into()
}

pub struct TyChecker<'db> {
    pub(crate) db: &'db dyn HirAnalysisDb,
    pub(crate) env: TyCheckEnv<'db>,
    pub(crate) table: UnificationTable<'db>,
    expected: TyId<'db>,
    /// The return shape of the projection being checked.
    projection_shape: Option<Shape<'db>>,
    /// Whether the body yields with `yield` statements. Every yield is then
    /// explicit, and the body's tail ends the slide.
    explicit_yields: bool,
    effect_provider_keys: FxHashSet<InferenceKey<'db>>,
    first_return_borrow_provider: Option<AddressSpace>,
    /// Plain names a reported marker on their initializer marks, so the body
    /// checks as the fix reads.
    implied_markers: FxHashMap<PatId, BindingMarker>,
    /// Markers on initializers, reported once their pattern's names are
    /// known: the marked expression, whether it is `mut`, where it stands
    /// and the plain names it implies a marker for.
    initializer_markers: Vec<(ExprId, bool, MarkerPosition, Vec<PatId>)>,
    /// Plain names that take a value from a mutable place or grant, which
    /// `mut` would write through.
    mutable_sources: FxHashSet<PatId>,
    /// The names parameter patterns bind, with the parameters' site.
    param_pattern_sites: FxHashMap<PatId, ParamSite<'db>>,
    /// The names a product parameter's pattern binds to its `ref` parts,
    /// which the parts' modes in the signature keep read-only.
    ref_part_bindings: FxHashSet<PatId>,
    /// Names bound to a shape whole, which is reported where they bind.
    shape_bindings: FxHashSet<PatId>,
    /// The missing effects of `mut?` calls at `ref`, reported once the calls'
    /// modes are final: the call, the effect's binding index in its callee,
    /// and the report.
    missing_effects: Vec<(ExprId, u32, BodyDiag<'db>)>,
    /// The provider each effect of a `mut?` call at `ref` resolved to, by the
    /// call and the effect's binding index, which a raise to `mut` requires
    /// mutable as resolution does.
    effect_providers: FxHashMap<(ExprId, u32), ProvidedEffect<'db>>,
    /// The unsafe split whose `mut` binding is asking its accesses for
    /// `mut`, which the errors that asks for name.
    split_demand: Option<ExprId>,
    /// The read accesses of the unsafe split a pattern is bound to that
    /// kept it from granting `mut`, which a `mut` binding of it names.
    split_reads: Vec<DynLazySpan<'db>>,
    diags: Vec<FuncBodyDiag<'db>>,
    /// Set once deferred work is resolved for the last time, after which type
    /// equalities are decided rather than retained.
    final_pass: bool,
    /// What the arithmetic operators written in the body do on overflow, by
    /// its arithmetic mode. They wrap in unchecked code.
    overflow: Overflow,
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

enum Viability {
    Viable,
    Incompatible,
    ReturnTypeMismatch,
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
        self.check_param_effect_names();

        if let BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } = self.env.owner()
        {
            let arm_span = self.env.owner().recv_arm_span().unwrap();
            let arm = contract
                .recv_arm(self.db, recv_idx as usize, arm_idx as usize)
                .expect("recv arm exists");
            let (pat_ty, ret_ty) =
                self.resolve_recv_arm_types(contract, recv_idx, arm_idx, arm, arm_span);
            self.expected = ret_ty;
            self.check_recv_arm_pat(arm.pat, pat_ty);
            self.seed_pat_bindings(arm.pat);
            self.check_arm_effect_names(arm.pat);
            self.env.flush_pending_bindings();
        }

        let root_expr = self.env.body().expr(self.db);
        if self.explicit_yields && self.projection_shape.is_some() {
            self.check_expr(root_expr, TyId::unit(self.db));
        } else {
            self.check_expr(root_expr, self.expected);
            if let Some(shape) = self.projection_shape.clone() {
                self.check_yield_site(root_expr, &shape);
            } else if self.expected.has_invalid(self.db) {
                // A result reported where it is declared takes the tail,
                // whatever it is.
                self.consume_access(root_expr);
            }
        }
        self.check_access_uses();
        self.check_marked_bindings();
        self.report_initializer_markers();
    }

    /// A closure may capture an access only of a `Copy` value, which it
    /// copies: checked here for the accesses whose types were still being
    /// inferred where they were captured.
    fn check_access_captures(&mut self) {
        for (expr, binding, ty) in self.env.take_access_captures() {
            let ty = ty.fold_with(self.db, &mut self.table);
            let ty = self.normalize_ty(ty);
            if !ty.has_var(self.db) && !ty.has_invalid(self.db) && !self.ty_is_copy(ty) {
                self.push_diag(BodyDiag::AccessCapture {
                    primary: expr.span(self.body()).into(),
                    name: binding.binding_name(&self.env),
                });
            }
        }
    }

    fn check_own_param_types(&mut self) {
        match self.env.owner() {
            BodyOwner::Func(_, _) => {}
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
                    let span: DynLazySpan<'db> =
                        contract.span().init_block().params().param(idx).ty().into();
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
                }
            }
            BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::ContractRecvArm { .. }
            | BodyOwner::Closure { .. } => {}
        }
    }

    fn check_effect_param_keys_resolve(&mut self) {
        match self.env.owner() {
            BodyOwner::Func(func, _) => {
                self.check_free_func_effect_list(func, func.effects(self.db))
            }
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } | BodyOwner::Closure { .. } => {}
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

            let key = resolve_effect_key(self.db, key_ty, func.scope(), self.env.assumptions());
            if matches!(key, ResolvedEffectKey::Invalid | ResolvedEffectKey::Other) {
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
            } else if let Some(diag) = effect_key_wf_diag(
                self.db,
                EffectParamOwner::Func(func),
                idx,
                &key,
                TraitSolveCx::new(self.db, func.scope()).with_assumptions(self.env.assumptions()),
            ) {
                self.push_diag(diag);
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
            BodyOwner::Func(func, mode) => (
                EffectParamOwner::Func(func),
                EffectParamSite::Func(func, mode),
            ),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } | BodyOwner::Closure { .. } => {
                unreachable!()
            }
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
            // A contract's bodies have no mode.
            if effect.mode == ModeMarker::MaybeMut {
                self.push_diag(BodyDiag::MaybeMutWithoutMode {
                    primary: owner.effect_param_ty_span(self.db, idx).into(),
                });
            }
            let Some(key_ty) = effect.key_ty.to_opt() else {
                continue;
            };

            // Labeled effects are always type/trait keyed: `name: Type`.
            if effect.name.is_some() {
                let key = resolve_effect_key(self.db, key_ty, contract.scope(), assumptions);
                if let Some(diag) = effect_key_wf_diag(
                    self.db,
                    owner,
                    idx,
                    &key,
                    TraitSolveCx::new(self.db, contract.scope()).with_assumptions(assumptions),
                ) {
                    self.push_diag(diag);
                    continue;
                }
                match key {
                    ResolvedEffectKey::Trait(schema) => {
                        let Some(root_effect_ty) = root_effect_ty else {
                            continue;
                        };
                        let trait_req = instantiate_trait_self(
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
                    ResolvedEffectKey::Row(_) => {
                        self.push_diag(BodyDiag::InvalidEffectKey {
                            owner,
                            key: key_ty,
                            idx,
                        });
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

    fn concrete_borrow_provider_for_capability_ty(&self, ty: TyId<'db>) -> Option<AddressSpace> {
        let semantics = provider_semantics(self.db, ty);
        // Root-object defaults are binding-site semantics, not type-implied address spaces.
        (!matches!(semantics.kind, ProviderKind::RootObject)).then_some(semantics.address_space)?
    }

    fn concrete_borrow_provider_for_effect_field(
        &self,
        site: EffectParamSite<'db>,
        idx: usize,
    ) -> Option<AddressSpace> {
        self.env
            .resolved_provider_binding(site, idx)?
            .semantics
            .address_space
    }

    fn concrete_borrow_provider_for_binding(
        &self,
        binding: LocalBinding<'db>,
    ) -> Option<AddressSpace> {
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
                LocalBinding::Local { .. }
                    | LocalBinding::Param { .. }
                    | LocalBinding::ParamPart { .. }
            )
            .then(|| {
                self.env
                    .binding_access(&binding)
                    .map(|_| AddressSpace::Memory)
            })?
        })
    }

    fn concrete_borrow_provider_for_place(&self, place: &Place<'db>) -> Option<AddressSpace> {
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
        // A place under a dereference lies in its pointer's space.
        let mut ty = binding_ty;
        let mut deref_space = None;
        for projection in &place.projections {
            if matches!(projection, PlaceProjection::Deref { .. }) {
                deref_space = ty
                    .as_capability(self.db)
                    .map_or(ty, |(_, inner)| inner)
                    .as_ptr(self.db)
                    .map(|(_, space)| space);
            }
            ty = normalize_ty(
                self.db,
                projection.result_ty(),
                self.env.scope(),
                self.env.assumptions(),
            );
        }
        if deref_space.is_some() {
            return deref_space;
        }
        if self.env.binding_access(&binding).is_some() {
            return self.concrete_borrow_provider_for_binding(binding);
        }
        let binding_provider = match binding {
            LocalBinding::EffectParam { .. } => self.concrete_borrow_provider_for_binding(binding),
            LocalBinding::Param {
                site: ParamSite::EffectField(..),
                ..
            } => self.concrete_borrow_provider_for_binding(binding),
            LocalBinding::Local { .. }
            | LocalBinding::Param { .. }
            | LocalBinding::ParamPart { .. } => None,
        };
        binding_provider
            .or_else(|| self.concrete_borrow_provider_for_capability_ty(place_result_ty))
            .or_else(|| {
                matches!(
                    binding,
                    LocalBinding::Local { .. }
                        | LocalBinding::Param { .. }
                        | LocalBinding::ParamPart { .. }
                )
                .then_some(AddressSpace::Memory)
            })
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
        TraitInstId::new(db, goal.def(db), args, assoc_type_bindings, goal.mode(db))
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
                // A function item's shape determines its generic arguments:
                // the function is known, so solving does not invent it.
                let self_ty = goal.self_ty(db);
                if self_ty.has_var(db)
                    && !(CallableTrait::of(db, goal.def(db)).is_some()
                        && CallableValue::of(db, self_ty).is_some())
                {
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
                // A function item's callable shape is its own, whatever its
                // effect providers infer to.
                let function_item = CallableTrait::of(db, goal.def(db)).is_some()
                    && matches!(
                        goal.self_ty(db).decompose_ty_app(db).0.data(db),
                        TyData::TyBase(TyBase::Func(_))
                    );
                if final_pass
                    && (self.trait_goal_is_concrete_for_diagnostics(goal) || function_item)
                {
                    let required_by = match obligation.origin {
                        env::TraitObligationOrigin::CallConstraint {
                            callable_def,
                            constraint_idx,
                            ..
                        } => self.call_constraint_diag_info(callable_def, constraint_idx),
                        env::TraitObligationOrigin::GenericConfirmation => None,
                    };
                    let unsat = subgoal.map(|goal| query.extract_subgoal(&mut self.table, goal));
                    self.push_diag(TyDiagCollection::from(
                        TraitConstraintDiag::TraitBoundNotSat {
                            span: obligation.span.clone(),
                            primary_goal: goal,
                            unsat_subgoal: unsat,
                            required_by,
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
        self.resolve_pending(None);
        let db = self.db;
        let body = self.env.body();
        let scope = self.env.scope();

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
                    let [expr_ty, recv_ty] = [expr_prop.ty, pending.recv_ty]
                        .map(|ty| ty.fold_with(db, &mut env::Prober::new(&mut self.table, scope)));
                    if expr_ty.has_invalid(db) {
                        continue;
                    }

                    let Some((receiver, generic_args, call_args)) =
                        pending_call_parts(db, body, &self.env, pending.expr)
                    else {
                        continue;
                    };

                    let viable: Vec<_> = pending
                        .candidates
                        .iter()
                        .copied()
                        .filter(|&candidate| {
                            let viability = self.method_viability(
                                &pending,
                                recv_ty,
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
                env::DeferredTask::Negation(pending) => {
                    self.check_pending_negation(pending);
                }
            }
        }
        for equality in equalities {
            self.process_type_equality(equality);
        }
    }

    /// Resolves deferred tasks until none progresses. A method call resolves
    /// at its call site: see `CallSite`. With `closure`, whose body has just
    /// been checked, only the calls in that body do, while its row can still
    /// take what they need, and the pass ends once none of them waits.
    ///
    /// A method call sees a literal's fallback type (`"a"` as `String<1>`)
    /// only once nothing resolves without it, and not with `closure`: the
    /// enclosing body can still fix the type of a literal the closure uses.
    fn resolve_pending(&mut self, closure: Option<ClosureDef<'db>>) {
        let db = self.db;
        let body = self.env.body();
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();

        // Fixed-point pass over deferred tasks.
        let mut fallback = false;
        let mut progressed = true;
        while closure.is_none_or(|def| self.env.has_pending_method_in(def)) {
            if !progressed {
                if fallback || closure.is_some() {
                    break;
                }
                fallback = true;
            }
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
                    env::DeferredTask::Method(pending)
                        if closure.is_some() && pending.site.closure != closure =>
                    {
                        self.env.register_pending_method(pending);
                    }
                    env::DeferredTask::Method(pending) => {
                        let Some((receiver, generic_args, call_args)) =
                            pending_call_parts(db, body, &self.env, pending.expr)
                        else {
                            continue;
                        };

                        let Some(expr_prop) = self.env.typed_expr(pending.expr) else {
                            continue;
                        };
                        let [expr_ty, recv_ty] = [expr_prop.ty, pending.recv_ty].map(|ty| {
                            if fallback {
                                ty.fold_with(db, &mut env::Prober::new(&mut self.table, scope))
                            } else {
                                ty.fold_with(db, &mut self.table)
                            }
                        });
                        if expr_ty.has_invalid(db) {
                            self.env.register_pending_method(pending);
                            continue;
                        }

                        let (strict, soft): (Vec<_>, Vec<_>) = pending
                            .candidates
                            .iter()
                            .copied()
                            .filter_map(|candidate| {
                                let viability = self.method_viability(
                                    &pending,
                                    recv_ty,
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
                                self.env.enter_call_site(&pending.site);
                                'resolve: {
                                    let call_span = pending.expr.span(body);
                                    let inst = candidate.inst;

                                    let receiver_prop = self
                                        .env
                                        .typed_expr(receiver)
                                        .unwrap_or_else(|| ExprProp::invalid(db));

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
                                            break 'resolve;
                                        }
                                    };

                                    if candidate.needs_confirmation {
                                        self.env.register_trait_obligation(env::TraitObligation {
                                            goal: inst,
                                            origin: env::TraitObligationOrigin::GenericConfirmation,
                                            span: call_span.clone().into(),
                                        });
                                    }

                                    // A value call `f(..)` has no explicit generic
                                    // arguments of its own.
                                    let args_span = if self.env.is_value_call(pending.expr) {
                                        call_span.into_call_expr().args()
                                    } else {
                                        let call_span = call_span.into_method_call_expr();
                                        if !callable.unify_generic_args(
                                            self,
                                            generic_args,
                                            call_span.clone().generic_args(),
                                        ) {
                                            break 'resolve;
                                        }
                                        call_span.args()
                                    };

                                    callable.check_args(
                                        self,
                                        call_args,
                                        args_span,
                                        Some((receiver, receiver_prop)),
                                        true,
                                    );

                                    callable.process_constraints(
                                        self,
                                        pending.expr,
                                        pending.span.clone(),
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
                                            break 'resolve;
                                        }
                                        Err(UnificationError::OccursCheckFailed) => {
                                            let span = pending.expr.span(body).into();
                                            self.push_diag(BodyDiag::InfiniteOccurrence(span));
                                            break 'resolve;
                                        }
                                    }

                                    self.check_callable_effects(pending.expr, &mut callable);

                                    if let Some(kind) =
                                        self.code_region_method_kind(recv_ty, pending.method_name)
                                        && call_args.len() == 1
                                        && self.env.typed_expr(call_args[0].expr).is_some_and(
                                            |prop| {
                                                ty_may_be_code_region_token(
                                                    db,
                                                    normalize_ty(db, prop.ty, scope, assumptions),
                                                )
                                            },
                                        )
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
                                self.env.leave_call_site();
                            }

                            progressed = true;
                        } else {
                            self.env.register_pending_method(pending);
                        }
                    }
                    env::DeferredTask::Negation(pending) => {
                        if !self.check_pending_negation(pending) {
                            self.env.register_pending_negation(pending);
                        }
                    }
                }
            }
        }
    }

    /// How `candidate` fits the deferred method call `pending` on a receiver
    /// of type `recv_ty`, typed `expr_ty`, probed without committing to it.
    #[allow(clippy::too_many_arguments)]
    fn method_viability(
        &mut self,
        pending: &env::PendingMethod<'db>,
        recv_ty: TyId<'db>,
        expr_ty: TyId<'db>,
        receiver: ExprId,
        generic_args: GenericArgListId<'db>,
        call_args: &[CallArg<'db>],
        candidate: env::PendingMethodCandidate<'db>,
    ) -> Viability {
        let db = self.db;
        let body = self.env.body();
        let scope = self.env.scope();
        let assumptions = self.env.assumptions();
        let snap = self.snapshot_state();

        let viability = (|| {
            let inst = candidate.inst;
            let inst_self = self.table.instantiate_to_term(inst.self_ty(db));
            if self.table.unify(inst_self, recv_ty).is_err() {
                return Viability::Incompatible;
            }

            let Ok(func_ty) =
                try_instantiate_trait_method(db, candidate.method, &mut self.table, recv_ty, inst)
            else {
                return Viability::Incompatible;
            };
            let func_ty = self.table.instantiate_to_term(func_ty);
            let Ok(mut callable) =
                Callable::new(db, func_ty, receiver.span(body).into(), Some(inst))
            else {
                return Viability::Incompatible;
            };

            match unify_explicit_call_generic_args(
                &mut callable,
                self,
                generic_args,
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

            let Ok(params) = callable.params_resolving(db, |shape| {
                normalize_ty(db, shape.fold_with(db, &mut self.table), scope, assumptions)
            }) else {
                return Viability::Incompatible;
            };
            if params.len() != call_args.len() + 1 {
                return Viability::Incompatible;
            }

            let Some(receiver_prop) = self.env.typed_expr(receiver) else {
                return Viability::Incompatible;
            };
            let mut all_arg_tys = Vec::with_capacity(call_args.len() + 1);
            all_arg_tys.push(receiver_prop.ty);
            for arg in call_args.iter() {
                let Some(prop) = self.env.typed_expr(arg.expr) else {
                    return Viability::Incompatible;
                };
                all_arg_tys.push(prop.ty);
            }

            for (&given, param) in all_arg_tys.iter().zip(&params) {
                let expected = normalize_ty(
                    db,
                    param.shape.erased_ty(db).fold_with(db, &mut self.table),
                    scope,
                    assumptions,
                );
                let given =
                    normalize_ty(db, given.fold_with(db, &mut self.table), scope, assumptions);
                if self.table.unify(given, expected).is_err() {
                    return Viability::Incompatible;
                }
            }

            callable.process_constraints(self, pending.expr, pending.span.clone());

            let ret_ty = normalize_ty(
                db,
                callable.ret_ty(db).fold_with(db, &mut self.table),
                scope,
                assumptions,
            );

            if self.table.unify(expr_ty, ret_ty).is_ok() {
                Viability::Viable
            } else {
                Viability::ReturnTypeMismatch
            }
        })();
        self.rollback_state(snap);
        viability
    }

    fn new_internal(db: &'db dyn HirAnalysisDb, env: TyCheckEnv<'db>, expected: TyId<'db>) -> Self {
        let table = UnificationTable::new(db);
        // A shape whose impl is known is the parts it lists. A result
        // reported where it is declared has no yields to check.
        let projection_shape = match env.owner() {
            BodyOwner::Func(func, mode) if !expected.has_invalid(db) => func
                .return_shape(db, mode)
                .map(|shape| normalize_ty(db, shape.clone(), env.scope(), env.assumptions())),
            _ => None,
        };
        let explicit_yields = env
            .body()
            .stmts(db)
            .values()
            .any(|stmt| matches!(stmt, Partial::Present(crate::hir_def::Stmt::Yield(_))));
        Self {
            db,
            table,
            expected,
            projection_shape,
            explicit_yields,
            effect_provider_keys: FxHashSet::default(),
            first_return_borrow_provider: None,
            implied_markers: FxHashMap::default(),
            initializer_markers: Vec::new(),
            mutable_sources: FxHashSet::default(),
            param_pattern_sites: FxHashMap::default(),
            ref_part_bindings: FxHashSet::default(),
            shape_bindings: FxHashSet::default(),
            missing_effects: Vec::new(),
            effect_providers: FxHashMap::default(),
            split_demand: None,
            split_reads: Vec::new(),
            diags: Vec::new(),
            final_pass: false,
            overflow: env.owner().scope().arithmetic_mode(db).into(),
            env,
        }
    }

    /// Resolves the pattern type and return type for a recv arm.
    /// Returns (pattern_type, return_type).
    fn resolve_recv_arm_types(
        &mut self,
        contract: Contract<'db>,
        recv_idx: u32,
        arm_idx: u32,
        arm: ContractRecvArm<'db>,
        arm_span: crate::span::item::LazyRecvArmSpan<'db>,
    ) -> (TyId<'db>, TyId<'db>) {
        if arm.is_fallback(self.db) {
            return (TyId::unit(self.db), TyId::unit(self.db));
        }

        let resolution = contract::recv_arm_resolution(self.db, contract, recv_idx, arm_idx);
        // The arm's pattern doesn't resolve its path again, so the
        // applications the path passes through are checked from here.
        self.env.register_path_applications(
            arm.pat.span(self.body()).into(),
            resolution.applications.iter().copied(),
        );
        let Some(variant) = resolution.variant else {
            // Invalid types suppress spurious type mismatch errors when the
            // pattern doesn't name a msg variant, which the recv block reports.
            let invalid_ty = TyId::invalid(self.db, InvalidCause::Other);
            return (invalid_ty, invalid_ty);
        };
        let pat_ty = variant.ty;

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

    /// Whether a provider supplies `binding`, whose reads then forward the
    /// provider's interface.
    fn binding_has_provider(&self, binding: LocalBinding<'db>) -> bool {
        if self.env.binding_access(&binding).is_some() {
            return false;
        }
        match binding {
            LocalBinding::EffectParam {
                site, provider_idx, ..
            } => self.env.provider_binding(site, provider_idx).is_some(),
            LocalBinding::Param {
                site: ParamSite::EffectField(site),
                idx,
                ..
            } => self.env.resolved_provider_binding(site, idx).is_some(),
            LocalBinding::Local { .. }
            | LocalBinding::Param { .. }
            | LocalBinding::ParamPart { .. } => false,
        }
    }

    fn binding_path_read_semantics(
        &mut self,
        binding: LocalBinding<'db>,
        expr_ty: TyId<'db>,
    ) -> PathReadSemantics {
        let expr_ty = self.normalize_ty(expr_ty);
        let binding_ty = self.normalize_ty(self.env.lookup_binding_ty(&binding));
        if binding_ty == expr_ty {
            PathReadSemantics::ReuseLocal
        } else if self.binding_has_provider(binding) {
            PathReadSemantics::ForwardInterface
        } else {
            PathReadSemantics::MaterializeValue
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
                        fallback: StringFallback::Fixed,
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

        // A literal whose context leaves its type open is an inline
        // `String<len>` while it fits in one word, else a `DynString`.
        let len_bytes = string_id.len_bytes(self.db);
        let fallback = if len_bytes > MAX_INLINE_STRING_BYTES {
            StringFallback::Dynamic
        } else {
            StringFallback::Fixed
        };
        self.table.new_var(
            TyVarSort::String {
                min_len: len_bytes,
                fallback,
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

    /// Types to look a receiver's methods up on: the receiver's type, and the
    /// runtime text type for a string literal.
    fn receiver_candidates(&self, ty: TyId<'db>) -> Vec<TyId<'db>> {
        let mut candidates = vec![ty];
        if let TyData::TyVar(var) = ty.base_ty(self.db).data(self.db)
            && matches!(var.sort, TyVarSort::String { .. })
            && let Some(text_ty) =
                resolve_lib_type_path(self.db, self.env.scope(), "core::abi::DynString")
        {
            candidates.push(text_ty);
        }
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

    /// Binds `pat`, the pattern of a `let` when `is_let`, to what `source`
    /// grants, when it grants accesses: see `AccessBinding` for how each kind
    /// of source binds.
    fn bind_pattern_source(
        &mut self,
        pat: PatId,
        source: ExprId,
        prop: &ExprProp<'db>,
        is_let: bool,
    ) {
        let Some(shape) = &prop.shape else {
            return;
        };
        // A `mut` binding of an access component asks the `mut?` call the
        // source grants through for `mut`, as an earlier arm may have.
        let split_reads = if self.pattern_asks_mut(pat, shape) && !self.select_mut_place(source) {
            self.split_read_accesses(source)
        } else {
            Vec::new()
        };
        let selected = self.env.typed_expr(source).and_then(|source| source.shape);
        let shape = selected.as_ref().unwrap_or(shape);
        self.consume_access(source);
        let authority = self.expr_has_authority(source);
        let single_name = matches!(
            pat.data(self.db, self.body()),
            Partial::Present(Pat::Path(..))
        );
        // The `let` a destructured product parameter's body begins with.
        let product = self.destructured_param(prop.binding).is_some()
            && prop
                .binding
                .is_some_and(|binding| self.env.param_product(&binding).is_some());
        let binding = if self.env.is_matched_place(source) {
            AccessBinding::Place
        } else if product {
            AccessBinding::Param
        } else if single_name && self.is_named_access(source) {
            AccessBinding::Named
        } else if single_name && is_let {
            AccessBinding::LetResult
        } else {
            AccessBinding::Result
        };
        self.split_reads = split_reads;
        self.bind_pattern_accesses(pat, shape, authority, binding);
        self.split_reads.clear();
    }

    /// Whether `expr` names the accesses it grants: `ref p`, `mut p`, an
    /// unsafe split of them, or a block ending in one.
    fn is_named_access(&self, expr: ExprId) -> bool {
        match self.env.expr_data(expr) {
            Partial::Present(Expr::Un(_, UnOp::Ref | UnOp::Mut | UnOp::MaybeMut)) => true,
            Partial::Present(Expr::Tuple(_)) => self.env.is_unsafe_split(expr),
            Partial::Present(Expr::Block(stmts, _)) => {
                matches!(
                    stmts.last().map(|stmt| stmt.data(self.db, self.body())),
                    Some(Partial::Present(Stmt::Expr(tail))) if self.is_named_access(*tail)
                )
            }
            _ => false,
        }
    }

    /// Whether the place `expr` names, or a projection result derives from,
    /// carries effect authority (`binding_has_authority`).
    pub(super) fn expr_has_authority(&self, expr: ExprId) -> bool {
        if let Some(place) = self.env.expr_place(expr) {
            let PlaceBase::Binding(binding) = place.base;
            return self.env.binding_has_authority(&binding);
        }
        match expr.data(self.db, self.body()) {
            Partial::Present(Expr::Un(inner, UnOp::Mut | UnOp::MaybeMut | UnOp::Ref)) => {
                self.expr_has_authority(*inner)
            }
            Partial::Present(Expr::Bin(base, _, BinOp::Index)) => self.expr_has_authority(*base),
            Partial::Present(Expr::MethodCall(receiver, _, _, args)) => {
                self.expr_has_authority(*receiver)
                    || args.iter().any(|arg| self.expr_has_authority(arg.expr))
            }
            Partial::Present(Expr::Call(callee, args)) => {
                (self.env.is_value_call(expr) && self.expr_has_authority(*callee))
                    || args.iter().any(|arg| self.expr_has_authority(arg.expr))
            }
            _ => false,
        }
    }

    /// The address space of the place an access of `expr` names, if known.
    pub(super) fn access_provider(&self, expr: ExprId) -> Option<AddressSpace> {
        self.env
            .expr_place(expr)
            .and_then(|place| self.concrete_borrow_provider_for_place(&place))
            .or_else(|| {
                self.env
                    .is_pointer_place_expr(expr)
                    .then_some(AddressSpace::Memory)
            })
    }

    /// A pattern matched against a place binds its components there: a `mut`
    /// binding opens a `mut` access on its component's path and a `ref`
    /// binding a read access, and the other bindings take their component's
    /// value, or a read access to a component with no value reading. A
    /// non-`Copy` place is matched there even without markers, so a pattern
    /// moves only what its names bind; its tests read the place's value
    /// (`is_inspected_place`). Index syntax `t[i]` calls `index` at `mut` for
    /// a `mut` binding and is destructured as that projection's result.
    fn open_matched_place(
        &mut self,
        scrutinee: ExprId,
        prop: ExprProp<'db>,
        pats: impl IntoIterator<Item = PatId>,
    ) -> ExprProp<'db> {
        let pats: Vec<_> = pats.into_iter().collect();
        if prop.is_mut && self.env.is_place_expr(scrutinee) {
            self.record_mutable_sources(&pats);
        }
        let marked = |this: &Self, marker| {
            pats.iter()
                .find_map(|pat| this.first_marked_binding(*pat, marker))
        };
        let mut_pat = marked(self, BindingMarker::Mut);
        let ty = prop.ty.fold_with(self.db, &mut self.table);
        let inspected = mut_pat.is_none()
            && marked(self, BindingMarker::Ref).is_none()
            && ty.has_value_reading(self.db);
        if inspected && (ty.has_invalid(self.db) || self.ty_is_copy(ty))
            || !self.env.is_place_expr(scrutinee)
        {
            return prop;
        }
        let prop = if mut_pat.is_some() && !prop.is_mut && self.select_mut_place(scrutinee) {
            self.env
                .typed_expr(scrutinee)
                .expect("selected place is typed")
        } else {
            prop
        };
        if prop.shape.is_some() {
            return prop;
        }
        if let Some(mut_pat) = mut_pat
            && !prop.is_mut
        {
            self.report_cannot_borrow_mut(scrutinee, mut_pat.span(self.body()).into());
        }
        let kind = if mut_pat.is_some() {
            BorrowKind::Mut
        } else {
            BorrowKind::Ref
        };
        let prop = ExprProp {
            borrow_provider: self.access_provider(scrutinee),
            shape: Some(Shape::Access(kind, prop.ty)),
            ..prop
        };
        self.env
            .open_matched_place(scrutinee, prop.clone(), inspected);
        prop
    }

    /// The first binding in `pat` marked `marker`.
    pub(super) fn first_marked_binding(&self, pat: PatId, marker: BindingMarker) -> Option<PatId> {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return None;
        };
        match pat_data {
            Pat::Path(_, binding) if self.binding_marker(pat, *binding) == marker => Some(pat),
            Pat::WildCard | Pat::Rest | Pat::Lit(_) | Pat::Path(..) => None,
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => pats
                .iter()
                .find_map(|pat| self.first_marked_binding(*pat, marker)),
            Pat::Record(_, fields) => fields
                .iter()
                .find_map(|field| self.first_marked_binding(field.pat, marker)),
            Pat::Or(lhs, rhs) => self
                .first_marked_binding(*lhs, marker)
                .or_else(|| self.first_marked_binding(*rhs, marker)),
        }
    }

    /// The marker the binding `pat`, written with `marker`, takes: its own,
    /// for a plain name the one a reported marker on its initializer
    /// implies, and for `mut? x` the body's mode.
    pub(super) fn binding_marker(&self, pat: PatId, marker: BindingMarker) -> BindingMarker {
        match marker {
            BindingMarker::Plain => self.implied_markers.get(&pat).copied().unwrap_or(marker),
            BindingMarker::MaybeMut => self.maybe_mut_marker(),
            _ => marker,
        }
    }

    /// The marker `mut?` is in this body: `mut` at `mut`, `ref` at `ref`.
    fn maybe_mut_marker(&self) -> BindingMarker {
        match self.maybe_mut_kind() {
            BorrowKind::Mut => BindingMarker::Mut,
            BorrowKind::Ref => BindingMarker::Ref,
        }
    }

    /// The access `mut?` is in this body: its mode's.
    pub(super) fn maybe_mut_kind(&self) -> BorrowKind {
        maybe_mut_kind(self.env.owner().mode())
    }

    /// `ref p` or `mut p` standing as `source`, the scrutinee or initializer
    /// of `pats` or a loop's base, where markers belong on the names the
    /// patterns bind. Each plain name takes the marker, so the body checks
    /// as the fix reads; the report waits until the names are known.
    /// Returns the marker.
    pub(super) fn take_initializer_marker(
        &mut self,
        source: ExprId,
        pats: &[PatId],
        position: MarkerPosition,
    ) -> Option<BindingMarker> {
        let (marked, marker) = self.initializer_marker(source)?;
        let mut names = Vec::new();
        for &pat in pats {
            self.collect_plain_names(pat, &mut names);
        }
        for &name in &names {
            self.implied_markers.insert(name, marker);
        }
        self.initializer_markers
            .push((marked, marker == BindingMarker::Mut, position, names));
        Some(marker)
    }

    /// The `ref p`, `mut p` or `mut? p` that `expr` is or ends in, with its
    /// marker.
    fn initializer_marker(&self, expr: ExprId) -> Option<(ExprId, BindingMarker)> {
        match self.env.expr_data(expr) {
            Partial::Present(Expr::Un(_, UnOp::Ref)) => Some((expr, BindingMarker::Ref)),
            Partial::Present(Expr::Un(_, UnOp::Mut)) => Some((expr, BindingMarker::Mut)),
            Partial::Present(Expr::Un(_, UnOp::MaybeMut)) => Some((expr, self.maybe_mut_marker())),
            Partial::Present(Expr::Block(stmts, _)) => {
                match stmts.last()?.data(self.db, self.body()) {
                    Partial::Present(Stmt::Expr(tail)) => self.initializer_marker(*tail),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// The unmarked path patterns in `pat`, which are its plain names once
    /// resolved.
    fn collect_plain_names(&self, pat: PatId, names: &mut Vec<PatId>) {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return;
        };
        match pat_data {
            Pat::Path(_, BindingMarker::Plain) => names.push(pat),
            Pat::WildCard | Pat::Rest | Pat::Lit(_) | Pat::Path(..) => {}
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                for &pat in pats {
                    self.collect_plain_names(pat, names);
                }
            }
            Pat::Record(_, fields) => {
                for field in fields {
                    self.collect_plain_names(field.pat, names);
                }
            }
            Pat::Or(lhs, rhs) => {
                self.collect_plain_names(*lhs, names);
                self.collect_plain_names(*rhs, names);
            }
        }
    }

    /// Notes the plain names in `pats` as taking values from a mutable place
    /// or grant.
    pub(super) fn record_mutable_sources(&mut self, pats: &[PatId]) {
        let mut names = Vec::new();
        for &pat in pats {
            self.collect_plain_names(pat, &mut names);
        }
        self.mutable_sources.extend(names);
    }

    /// Reports the markers on initializers, labelling the plain names the
    /// marker belongs on.
    fn report_initializer_markers(&mut self) {
        let body = self.body();
        for (marked, marker, position, names) in std::mem::take(&mut self.initializer_markers) {
            let names = names
                .into_iter()
                .filter(|&pat| {
                    matches!(self.env.pat_binding(pat), Some(LocalBinding::Local { .. }))
                })
                .filter_map(|pat| {
                    let Partial::Present(Pat::Path(Partial::Present(path), _)) =
                        pat.data(self.db, body)
                    else {
                        return None;
                    };
                    Some((path.as_ident(self.db)?, pat.span(body).into()))
                })
                .collect();
            self.push_diag(BodyDiag::InitializerMarker {
                primary: marked.span(body).into_un_expr().op().into(),
                marker,
                position,
                names,
            });
        }
    }

    /// Whether `pat` binds a component of `shape` that is a `ref` access with
    /// a `mut` binding, which needs the access to be `mut`.
    fn pattern_asks_mut(&self, pat: PatId, shape: &Shape<'db>) -> bool {
        self.access_pats(pat, shape).into_iter().any(|(pat, kind)| {
            kind == BorrowKind::Ref && self.first_marked_binding(pat, BindingMarker::Mut).is_some()
        })
    }

    /// The parts of `pat` that bind the access components of `shape`, with
    /// each component's kind.
    pub(super) fn access_pats(&self, pat: PatId, shape: &Shape<'db>) -> Vec<(PatId, BorrowKind)> {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return Vec::new();
        };
        match (shape, pat_data) {
            (Shape::Access(kind, _), _) => vec![(pat, *kind)],
            (_, Pat::Or(lhs, rhs)) => {
                let mut pats = self.access_pats(*lhs, shape);
                pats.extend(self.access_pats(*rhs, shape));
                pats
            }
            (Shape::Tuple(elems), Pat::Tuple(pats)) => {
                let rest = pats
                    .iter()
                    .position(|pat| pat.is_rest(self.db, self.body()));
                pats.iter()
                    .enumerate()
                    .flat_map(|(idx, &pat)| {
                        let elem = match rest {
                            Some(rest) if idx > rest => (elems.len() + idx).checked_sub(pats.len()),
                            _ => Some(idx),
                        };
                        elem.and_then(|elem| elems.get(elem))
                            .map_or_else(Vec::new, |elem| self.access_pats(pat, elem))
                    })
                    .collect()
            }
            (Shape::Sum { payload, .. }, Pat::PathTuple(_, pats)) => match pats.as_slice() {
                [payload_pat] => self.access_pats(*payload_pat, payload),
                _ => Vec::new(),
            },
            _ => Vec::new(),
        }
    }

    /// Gives the bindings of `pat` the accesses its source grants. An access
    /// destructures uniformly: every binding below it is an access of its
    /// kind. Tuple and sum shapes assign their components, and owned
    /// components bind by value. A shape is not a value, so a single binding
    /// cannot hold a whole tuple or sum shape.
    pub(super) fn bind_pattern_accesses(
        &mut self,
        pat: PatId,
        shape: &Shape<'db>,
        authority: bool,
        binding: AccessBinding,
    ) {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return;
        };
        match (shape, pat_data) {
            (Shape::Owned(_), _) | (_, Pat::WildCard | Pat::Rest | Pat::Lit(_)) => {}
            (Shape::Access(kind, _), _) => self.set_pattern_access(pat, *kind, authority, binding),
            (_, Pat::Or(lhs, rhs)) => {
                self.bind_pattern_accesses(*lhs, shape, authority, binding);
                self.bind_pattern_accesses(*rhs, shape, authority, binding);
            }
            (Shape::Tuple(elems), Pat::Tuple(pats)) => {
                let rest = pats
                    .iter()
                    .position(|pat| pat.is_rest(self.db, self.body()));
                for (idx, &pat) in pats.iter().enumerate() {
                    let elem = match rest {
                        Some(rest) if idx > rest => (elems.len() + idx).checked_sub(pats.len()),
                        _ => Some(idx),
                    };
                    if let Some(elem) = elem.and_then(|elem| elems.get(elem)) {
                        self.bind_pattern_accesses(pat, elem, authority, binding);
                    }
                }
            }
            (
                Shape::Sum {
                    variant, payload, ..
                },
                Pat::PathTuple(_, pats),
            ) => {
                if self.pattern_variant(pat) == Some(*variant)
                    && let [payload_pat] = pats.as_slice()
                {
                    self.bind_pattern_accesses(*payload_pat, payload, authority, binding);
                }
            }
            // A value of an opaque shape is only passed on, never bound but
            // as a parameter's part.
            (Shape::Opaque(term), Pat::Path(..)) if self.env.pat_binding(pat).is_some() => {
                match self.env.pat_binding(pat) {
                    Some(LocalBinding::Local { pat: name, .. })
                        if binding == AccessBinding::Param =>
                    {
                        self.env.register_transport_part(name, shape.clone());
                    }
                    _ => {
                        self.shape_bindings.insert(pat);
                        self.push_diag(BodyDiag::OpaqueShapeBound {
                            primary: pat.span(self.body()).into(),
                            shape: *term,
                        });
                    }
                }
            }
            (_, Pat::Path(..)) if self.env.pat_binding(pat).is_some() => {
                self.shape_bindings.insert(pat);
                self.push_diag(BodyDiag::ShapeNotDestructured {
                    primary: pat.span(self.body()).into(),
                    part: binding == AccessBinding::Param,
                });
            }
            _ => {}
        }
    }

    /// The enum variant a checked constructor pattern matches.
    fn pattern_variant(&self, pat: PatId) -> Option<u16> {
        let root = self.env.pattern_store().root(pat)?;
        match self.env.pattern_store().node(root).kind() {
            ValidatedPatKind::Constructor {
                ctor: ConstructorKind::Variant(variant, _),
                ..
            } => Some(variant.idx),
            _ => None,
        }
    }

    /// Gives each binding of `pat`, below an access of `kind`, what `binding`
    /// says it takes: the access, a weaker read access, or a copy.
    fn set_pattern_access(
        &mut self,
        pat: PatId,
        kind: BorrowKind,
        authority: bool,
        binding: AccessBinding,
    ) {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return;
        };
        match pat_data {
            Pat::Path(_, marker) => {
                let Some(LocalBinding::Local { .. }) = self.env.pat_binding(pat) else {
                    return;
                };
                let Some(ty) = self.env.pat_ty(pat) else {
                    return;
                };
                let ty = ty.fold_with(self.db, &mut self.table);
                let marker = self.binding_marker(pat, *marker);
                let Some(access) = self.binding_access_kind(pat, ty, marker, kind, binding) else {
                    if kind == BorrowKind::Mut && marker == BindingMarker::Plain {
                        self.mutable_sources.insert(pat);
                    }
                    return;
                };
                if binding == AccessBinding::Param && kind == BorrowKind::Ref {
                    self.ref_part_bindings.insert(pat);
                }
                self.env.set_pat_binding_mode(
                    pat,
                    PatBindingMode::Access {
                        kind: access,
                        authority,
                    },
                );
            }
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                for pat in pats {
                    self.set_pattern_access(*pat, kind, authority, binding);
                }
            }
            Pat::Record(_, fields) => {
                for field in fields {
                    self.set_pattern_access(field.pat, kind, authority, binding);
                }
            }
            Pat::Or(lhs, rhs) => {
                self.set_pattern_access(*lhs, kind, authority, binding);
                self.set_pattern_access(*rhs, kind, authority, binding);
            }
            Pat::WildCard | Pat::Rest | Pat::Lit(..) => {}
        }
    }

    /// The access a binding marked `marker`, of type `ty`, takes below an
    /// access of `kind`, or `None` when it takes a value.
    ///
    /// `ref x` is a read access and `mut x` a `mut` access, never a copy;
    /// `var x` is a mutable value. A plain name takes a value as `let x = e`
    /// does, except that a type with no value reading (`#[view]` or
    /// state-only) binds a read access, whatever the grant's mode, and a
    /// `let` binding such a `mut` result whole keeps it. A `let` binding a
    /// `mut` result of any other type whole needs `mut`. A value of a read
    /// result is a copy, so a non-`Copy` one needs `ref`; one of a `mut`
    /// result may move, as through any `mut` access.
    fn binding_access_kind(
        &mut self,
        pat: PatId,
        ty: TyId<'db>,
        marker: BindingMarker,
        kind: BorrowKind,
        binding: AccessBinding,
    ) -> Option<BorrowKind> {
        let primary = || pat.span(self.body()).into();
        match (binding, marker) {
            (_, BindingMarker::MaybeMut) => unreachable!("`binding_marker` resolves `mut?`"),
            (_, BindingMarker::Ref) => Some(BorrowKind::Ref),
            (_, BindingMarker::Var) if !ty.has_value_reading(self.db) => {
                self.push_diag(BodyDiag::VarOfNoValue {
                    primary: primary(),
                    ty,
                });
                Some(BorrowKind::Ref)
            }
            (AccessBinding::Place, BindingMarker::Mut) => Some(BorrowKind::Mut),
            (AccessBinding::Place, BindingMarker::Plain) if !ty.has_value_reading(self.db) => {
                Some(BorrowKind::Ref)
            }
            (AccessBinding::Place, _) => None,
            (AccessBinding::Named, BindingMarker::Var) => {
                self.push_diag(BodyDiag::VarOfAccess { primary: primary() });
                None
            }
            (AccessBinding::Named, _) => Some(kind),
            _ if ty.has_invalid(self.db) => Some(kind),
            (_, BindingMarker::Mut) if kind == BorrowKind::Mut => Some(kind),
            (_, BindingMarker::Mut) => {
                self.push_diag(BodyDiag::MutBindingOfRead {
                    primary: primary(),
                    split_reads: self.split_reads.clone(),
                });
                Some(BorrowKind::Ref)
            }
            (AccessBinding::LetResult, BindingMarker::Plain) if kind == BorrowKind::Mut => {
                if ty.has_value_reading(self.db) {
                    self.push_diag(BodyDiag::MutBindingRequired {
                        primary: primary(),
                        ty,
                    });
                }
                Some(kind)
            }
            (_, BindingMarker::Plain) if !ty.has_value_reading(self.db) => Some(BorrowKind::Ref),
            (_, BindingMarker::Var) if self.ty_is_copy(ty) => {
                if kind == BorrowKind::Mut {
                    self.push_diag(BodyDiag::MutAccessCopied {
                        primary: primary(),
                        ty,
                    });
                }
                None
            }
            (_, BindingMarker::Plain) if self.ty_is_copy(ty) => None,
            (_, BindingMarker::Plain | BindingMarker::Var) if kind == BorrowKind::Mut => None,
            (_, BindingMarker::Plain | BindingMarker::Var) => {
                self.push_diag(BodyDiag::RefBindingRequired {
                    primary: primary(),
                    ty,
                });
                Some(BorrowKind::Ref)
            }
        }
    }

    /// Reports `ref` and `mut` bindings that bind a value: of an owned value
    /// or component, or a copied element, which has no access to hold. A
    /// name bound to a shape whole is reported already.
    fn check_marked_bindings(&mut self) {
        let body = self.body();
        for (pat, data) in body.pats(self.db).iter() {
            if !self.shape_bindings.contains(&pat)
                && let Partial::Present(Pat::Path(_, marker)) = data
                && let marker @ (BindingMarker::Ref | BindingMarker::Mut) =
                    self.binding_marker(pat, *marker)
                && let Some(binding @ LocalBinding::Local { .. }) = self.env.pat_binding(pat)
                && self.env.binding_access(&binding).is_none()
                && self
                    .env
                    .pat_ty(pat)
                    .is_some_and(|ty| !ty.fold_with(self.db, &mut self.table).has_invalid(self.db))
            {
                self.push_diag(BodyDiag::MarkedBindingWithoutAccess {
                    primary: pat.span(body).into(),
                    marker: marker == BindingMarker::Mut,
                });
            }
        }
    }

    /// Ensure all binding patterns are registered in the current scope.
    fn seed_pat_bindings(&mut self, pat: PatId) {
        let Partial::Present(pat_data) = pat.data(self.db, self.env.body()) else {
            return;
        };

        match pat_data {
            Pat::Path(path, marker) => {
                let Partial::Present(path) = path else {
                    return;
                };
                let Some(LocalBinding::Local { .. }) = self.env.pat_binding(pat) else {
                    return;
                };
                if let Some(ident) = path.as_ident(self.db) {
                    let current = self.env.current_block_idx();
                    if self.env.get_block(current).lookup_var(ident).is_none() {
                        let binding = LocalBinding::local(
                            pat,
                            matches!(
                                self.binding_marker(pat, *marker),
                                BindingMarker::Var | BindingMarker::Mut
                            ),
                        );
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
        let actual = self.equate_ty(actual, expected, span);

        self.retype_expr_or_pat(t, actual);
        actual
    }

    fn retype_expr_or_pat(&mut self, typeable: Typeable<'db>, ty: TyId<'db>) {
        match typeable {
            Typeable::Expr(expr, mut typed_expr) => {
                typed_expr.ty = ty;
                if typed_expr.borrow_provider.is_none() {
                    typed_expr.borrow_provider =
                        self.concrete_borrow_provider_for_capability_ty(ty);
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
                    // A diverging expression takes the expected type, except
                    // while that is still unknown: answering with the open
                    // variable would hide that the expression diverges.
                    let expected = expected.fold_with(self.db, &mut self.table);
                    if matches!(expected.data(self.db), TyData::TyVar(_)) {
                        actual
                    } else {
                        expected
                    }
                } else {
                    actual
                }
            }

            Err(UnificationError::TypeMismatch) => {
                let actual = actual.fold_with(self.db, &mut self.table);
                let expected = expected.fold_with(self.db, &mut self.table);
                // A retained equality is decided, and reported, where it is
                // written; enclosing expressions see the type they expect.
                if self.retain_or_report_mismatch(env::TypeEquality {
                    actual,
                    expected,
                    span,
                }) {
                    expected
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
        lowering_cx: &LoweringContext<'db>,
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
            lowering_cx,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub enum EffectArg<'db> {
    Place(Place<'db>),
    Value(ExprId),
    Binding(LocalBinding<'db>),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum EffectPassMode {
    /// The provided effect is already a place; pass it directly.
    ByPlace,
    /// The provided effect is an rvalue; materialize it into a block-scoped temp place.
    ByTempPlace,
    ByValue,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub struct ResolvedEffectArg<'db> {
    pub param_idx: usize,
    pub binding_idx: u32,
    pub key: HirTyId<'db>,
    pub arg: EffectArg<'db>,
    /// The with-binding whose source was evaluated on entry to the block.
    pub with_source: Option<ExprId>,
    pub pass_mode: EffectPassMode,
    pub required_mut: bool,
    pub key_kind: EffectKeyKind,
    pub instantiated_key_ty: Option<TyId<'db>>,
    pub provider_target_ty: Option<TyId<'db>>,
    pub provider: Option<AddressSpace>,
    /// For a component of one of the callee's rows, or a row it forwards,
    /// where it sits: `binding_idx` numbers it only in the call's view.
    pub row_arg: Option<RowArg>,
}

/// Where an effect argument sits among the callee's rows.
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub enum RowArg {
    /// A component of one of the callee's rows.
    Component(RowPath),
    /// One of the callee's rows, which the call forwards from the caller's
    /// own row at `own`: one of its entries, or a component of one.
    Forwarded { callee: RowPath, own: RowPath },
}

/// Resolved reference for a `const`-valued path expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum ConstRef<'db> {
    Const(Const<'db>),
    TraitConst {
        use_: AssocConstUse<'db>,
        /// The checker's proof of the use's trait goal, as for a call's
        /// [`Callable`].
        evidence: Option<EvidenceId<'db>>,
        /// Proofs, in the checked body, of the bounds the trait's default
        /// value body declares, by their places among the assumptions it is
        /// checked under: the evidence that body's instance receives.
        bound_evidence: Option<EvidenceList<'db>>,
    },
    InherentConst(InherentConstUse<'db>),
}

impl<'db> ConstRef<'db> {
    /// A trait constant's defining body, its declared type, the substitution
    /// over the body's owner, and the evidence the body's instance receives:
    /// from the implementation the checker selected, or, for a use the
    /// checker answered by an assumption, the one the use's context selects.
    pub fn trait_const_body(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(Body<'db>, TyId<'db>, CompleteSubst<'db>, EvidenceList<'db>)> {
        let ConstRef::TraitConst {
            use_,
            evidence,
            bound_evidence,
        } = self
        else {
            return None;
        };
        let resolved = instantiated_selection(db, evidence, use_.solve_cx(db), use_.inst())?;
        let (body, ty, subst) = selected_assoc_const_body_template(db, resolved, use_.name())?;
        let default = use_
            .inst()
            .def(db)
            .const_(db, use_.name())
            .and_then(|constant| constant.default_body(db));
        // The trait's default receives the use's proofs; an implementation's
        // value receives them from the selection's proofs of its conditions.
        let received = match (bound_evidence, evidence) {
            (Some(bound), _) if default == Some(body) => bound,
            (_, Some(selection))
                if default != Some(body) && matches!(selection.data(db), Evidence::Impl { .. }) =>
            {
                impl_const_bound_evidence(
                    db,
                    body,
                    selection,
                    GenericSubst::complete(db, subst.clone()),
                )
            }
            _ => EvidenceList::none(db),
        };
        Some((body, ty, subst, received))
    }

    /// Proves, in the use's context, the bounds the trait's default value
    /// body declares, instantiated for the use, as `declared_bound_evidence`
    /// records them.
    pub(super) fn record_bound_evidence(&mut self, db: &'db dyn HirAnalysisDb) {
        let ConstRef::TraitConst {
            use_,
            bound_evidence,
            ..
        } = self
        else {
            return;
        };
        let trait_ = use_.inst().def(db);
        let (Some(body), Ok(subst)) = (
            trait_
                .const_(db, use_.name())
                .and_then(|constant| constant.default_body(db)),
            CompleteSubst::for_owner(db, trait_.into(), use_.inst().args(db).to_vec()),
        ) else {
            return;
        };
        let declared = const_body_assumptions(db, body.scope());
        *bound_evidence = Some(declared_bound_evidence(
            db,
            declared,
            declared.extend_all_bounds(db),
            &subst,
            use_.solve_cx(db),
        ));
    }
}

impl<'db> TyVisitable<'db> for ConstRef<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        match self {
            ConstRef::Const(_) => {}
            ConstRef::TraitConst {
                use_,
                evidence,
                bound_evidence,
            } => {
                use_.visit_with(visitor);
                if let Some(evidence) = evidence {
                    evidence.visit_with(visitor);
                }
                if let Some(bound_evidence) = bound_evidence {
                    bound_evidence.visit_with(visitor);
                }
            }
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
            ConstRef::TraitConst {
                use_,
                evidence,
                bound_evidence,
            } => ConstRef::TraitConst {
                use_: use_.fold_with(db, folder),
                evidence: evidence.map(|evidence| evidence.fold_with(db, folder)),
                bound_evidence: bound_evidence
                    .map(|bound_evidence| bound_evidence.fold_with(db, folder)),
            },
            ConstRef::InherentConst(use_) => ConstRef::InherentConst(use_.fold_with(db, folder)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
struct ExprPlaceId(u32);
entity_impl!(ExprPlaceId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
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
#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub struct TypedBody<'db> {
    // SAFETY: The tables own their data, which is salsa ids and plain values;
    // the entity maps holding them have no `SalsaValue` impls.
    #[salsa_value(unsafe(prove_safe_to_retain_manually))]
    tables: Arc<TypedBodyTables<'db>>,
}

/// The field type of a transparent newtype, which a cast sees through: a
/// one-element tuple, or a struct with one field visible from `scope`.
pub(crate) fn transparent_newtype_field_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
) -> Option<TyId<'db>> {
    if !ty.is_tuple(db) && !ty.is_struct(db) {
        return None;
    }
    let [field_ty] = ty.field_types(db)[..] else {
        return None;
    };
    (!has_invisible_newtype_field(db, scope, ty)).then_some(field_ty)
}

/// Whether `ty` is a single-field struct whose field `scope` cannot see.
pub(crate) fn has_invisible_newtype_field<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
) -> bool {
    let Some(AdtRef::Struct(s)) = ty.adt_def(db).map(|adt| adt.adt_ref(db)) else {
        return false;
    };
    ty.field_types(db).len() == 1
        && !is_scope_visible_from(db, ScopeId::Field(FieldParent::Struct(s), 0), scope)
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
        /// The prefix of `assumptions` the body's owner declares; see
        /// `TypedBody::signature`.
        pub(super) signature: PredicateListId<'db>,
        pub(super) assumptions: PredicateListId<'db>,
        pub(super) pat_ty: SecondaryMap<PatId, Option<TyId<'db>>>,
        pub(super) expr_ty: SecondaryMap<ExprId, Option<ExprProp<'db>>>,
        pub(super) yield_shapes: SecondaryMap<ExprId, Option<Shape<'db>>>,
        /// Index expressions naming an entry of a collection whose elements
        /// are places.
        pub(super) place_entries: FxHashSet<ExprId>,
        /// The tuples that are unsafe splits: an `unsafe` block's tail tuple
        /// of accesses, granted by one session.
        pub(super) unsafe_splits: FxHashSet<ExprId>,
        /// The tuples passed to product parameters, a part each.
        pub(super) product_args: FxHashSet<ExprId>,
        /// The calls `f(..)` of callable values: the callee is the receiver
        /// of a callable trait's method.
        pub(super) value_calls: FxHashSet<ExprId>,
        /// The operators on primitive operands, by their built-in operation.
        pub(super) builtin_ops: FxHashMap<ExprId, BuiltinOp>,
        /// The compound assignments that assign their operator method's
        /// result (`x = x.wrapping_add(y)`) rather than call an `*Assign`
        /// method.
        pub(super) operator_assigns: FxHashSet<ExprId>,
        /// The places a pattern is matched against, which bind each
        /// component where it lies.
        pub(super) matched_places: FxHashSet<ExprId>,
        /// The matched places whose patterns have no `ref` or `mut` binding.
        pub(super) inspected_places: FxHashSet<ExprId>,
        /// `with` values whose resources the function's own authority covers.
        pub(super) covered_providers: FxHashSet<ExprId>,
        pub(super) const_refs: SecondaryMap<ExprId, Option<ConstRef<'db>>>,
        pub(super) value_path_refs: SecondaryMap<ExprId, Option<ValuePathRef<'db>>>,
        pub(super) semantic_expr_lowering: SecondaryMap<ExprId, Option<SemanticExprLowering<'db>>>,
        pub(super) record_init_lowering: SecondaryMap<ExprId, Option<RecordInitLowering<'db>>>,
        pub(super) closure_infos: SecondaryMap<ExprId, Option<ClosureInfo<'db>>>,
        pub(super) resolved_field_index: SecondaryMap<ExprId, Option<u16>>,
        pub(super) call_effect_args: SecondaryMap<ExprId, Option<Vec<ResolvedEffectArg<'db>>>>,
        pub(super) return_borrow_provider: Option<AddressSpace>,
        /// Bindings for function parameters (indexed by param position)
        pub(super) param_bindings: Vec<LocalBinding<'db>>,
        /// The inputs a call passes the body: each parameter, or each part of
        /// a product parameter.
        pub(super) input_bindings: Vec<LocalBinding<'db>>,
        /// The shapes of the product parameters, the body's own and its
        /// closures', by site and index.
        pub(super) param_products: Vec<(ParamSite<'db>, usize, Shape<'db>)>,
        /// The opaque parts of products that destructured parameters bind
        /// whole, by the name's pattern.
        pub(super) transport_parts: Vec<(PatId, Shape<'db>)>,
        /// Bindings for local variables (keyed by the pattern that introduces them)
        pub(super) pat_bindings: SecondaryMap<PatId, Option<LocalBinding<'db>>>,
        /// Binding capture mode for local variables (keyed by the pattern that introduces them)
        pub(super) pat_binding_modes: SecondaryMap<PatId, Option<PatBindingMode>>,
        pub(super) pattern_store: PatternStore<'db>,
        pub(super) pattern_status: SecondaryMap<PatId, PatternAnalysisStatus>,
        /// The Collection protocol plan of each for loop
        pub(super) for_loop_plans: SecondaryMap<StmtId, Option<ForLoopPlan<'db>>>,
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
    VariantField {
        variant: u16,
        field: u16,
    },
    ConstantIndex(usize),
    /// An index the call passes: parameter `param`, or its part `part` if it
    /// is a product.
    ParamIndex {
        param: usize,
        part: Option<u16>,
    },
    AnyIndex,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnSource {
    pub result_projection: Vec<ReturnProjectionStep>,
    pub origin: CallableInputOrigin,
    pub projection: Vec<ReturnProjectionStep>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReturnProvenance {
    Fresh,
    Forwarded(Vec<ReturnSource>),
    Unknown,
}

#[salsa::tracked(cycle_initial=func_return_provenance_cycle_initial)]
fn func_return_provenance<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> ReturnProvenance {
    let (diags, typed_body) = infer_body(db, BodyOwner::func(db, func));
    if !diags.is_empty() {
        return ReturnProvenance::Unknown;
    }
    typed_body.return_provenance_for_func(db, func, &mut FxHashSet::default())
}

fn func_return_provenance_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _func: Func<'db>,
) -> ReturnProvenance {
    ReturnProvenance::Forwarded(Vec::new())
}

/// The receiver, explicit generic arguments and arguments of the deferred
/// method call `expr`: a method call, or a call `f(..)` of a callable value,
/// whose callee is the receiver.
fn pending_call_parts<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    env: &TyCheckEnv<'db>,
    expr: ExprId,
) -> Option<(ExprId, GenericArgListId<'db>, &'db [CallArg<'db>])> {
    match expr.data(db, body) {
        Partial::Present(Expr::MethodCall(receiver, _, generic_args, args)) => {
            Some((*receiver, *generic_args, args.as_slice()))
        }
        Partial::Present(Expr::Call(callee, args)) if env.is_value_call(expr) => {
            Some((*callee, GenericArgListId::none(db), args.as_slice()))
        }
        _ => None,
    }
}

/// The operands of a call-like expression in parameter order: a receiver,
/// including a value call's callee (`value_call`), first.
fn call_like_expr_args(expr: &Expr<'_>, value_call: bool) -> Option<Vec<ExprId>> {
    match expr {
        Expr::Call(callee, args) => Some(
            value_call
                .then_some(*callee)
                .into_iter()
                .chain(args.iter().map(|arg| arg.expr))
                .collect(),
        ),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, SalsaValue)]
pub enum ValuePathRef<'db> {
    UnitVariant(ResolvedVariant<'db>),
    TypeConst(TyId<'db>),
    FunctionItem,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum CodeRegionIntrinsicKind {
    Offset,
    Len,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum ConstIntrinsicKind {
    SizeOf,
    StorageSlots,
}

/// The operation an operator expression on primitive operands performs,
/// which the language defines: no operator trait is called. Its overflow
/// behavior is the arithmetic mode of the body the operator is written in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum BuiltinOp {
    Unary(UnaryOp),
    Binary(BinaryOp),
}

#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
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

impl<'db> SemanticExprLowering<'db> {
    pub(super) fn callable_mut(&mut self) -> &mut Callable<'db> {
        match self {
            Self::Call { callable }
            | Self::CodeRegionIntrinsic { callable, .. }
            | Self::ConstIntrinsic { callable, .. } => callable,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
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
        for shape in self.tables.yield_shapes.values().flatten() {
            shape.visit_with(visitor);
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
        for info in self.tables.closure_infos.values().flatten() {
            info.visit_with(visitor);
        }
        for args in self.tables.call_effect_args.values().flatten() {
            args.visit_with(visitor);
        }
        self.tables.param_bindings.visit_with(visitor);
        self.tables.input_bindings.visit_with(visitor);
        for (_, _, product) in &self.tables.param_products {
            product.visit_with(visitor);
        }
        for (_, part) in &self.tables.transport_parts {
            part.visit_with(visitor);
        }
        for binding in self.tables.pat_bindings.values().flatten() {
            binding.visit_with(visitor);
        }
        for place in self.tables.expr_places.values() {
            place.visit_with(visitor);
        }
        self.tables.pattern_store.visit_with(visitor);
        for plan in self.tables.for_loop_plans.values().flatten() {
            plan.visit_with(visitor);
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
        this.signature = this.signature.fold_with(db, folder);
        this.assumptions = this.assumptions.fold_with(db, folder);
        this.pat_ty
            .values_mut()
            .flatten()
            .for_each(|ty| *ty = ty.fold_with(db, folder));
        this.expr_ty
            .values_mut()
            .flatten()
            .for_each(|prop| *prop = prop.clone().fold_with(db, folder));
        this.yield_shapes
            .values_mut()
            .flatten()
            .for_each(|shape| *shape = shape.clone().fold_with(db, folder));
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
        this.closure_infos
            .values_mut()
            .flatten()
            .for_each(|info| *info = info.clone().fold_with(db, folder));
        for args in this.call_effect_args.values_mut().flatten() {
            for arg in args {
                *arg = arg.clone().fold_with(db, folder);
            }
        }
        this.param_bindings
            .iter_mut()
            .for_each(|binding| *binding = binding.fold_with(db, folder));
        this.input_bindings
            .iter_mut()
            .for_each(|binding| *binding = binding.fold_with(db, folder));
        this.param_products
            .iter_mut()
            .for_each(|(_, _, product)| *product = product.clone().fold_with(db, folder));
        this.transport_parts
            .iter_mut()
            .for_each(|(_, part)| *part = part.clone().fold_with(db, folder));
        this.pat_bindings
            .values_mut()
            .flatten()
            .for_each(|binding| *binding = binding.fold_with(db, folder));
        this.pattern_store = this.pattern_store.fold_with(db, folder);
        this.for_loop_plans
            .values_mut()
            .flatten()
            .for_each(|plan| *plan = plan.clone().fold_with(db, folder));
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

impl<'db> TypedBody<'db> {
    /// The tables for writing, copied first if another `TypedBody` shares
    /// them.
    fn tables_mut(&mut self) -> &mut TypedBodyTables<'db> {
        Arc::make_mut(&mut self.tables)
    }

    /// Replaces the assumptions the body's evidence names with `bound`'s
    /// proofs of them: an instance's own.
    pub(crate) fn bind_evidence(&mut self, db: &'db dyn HirAnalysisDb, bound: EvidenceList<'db>) {
        if bound.entries(db).is_empty() {
            return;
        }
        let tables = self.tables_mut();
        for lowering in tables.semantic_expr_lowering.values_mut().flatten() {
            lowering.callable_mut().bind_evidence(db, bound);
        }
        for plan in tables.for_loop_plans.values_mut().flatten() {
            for call in &mut plan.calls {
                call.callable.bind_evidence(db, bound);
            }
        }
        for const_ref in tables.const_refs.values_mut().flatten() {
            if let ConstRef::TraitConst {
                evidence,
                bound_evidence,
                ..
            } = const_ref
            {
                *evidence = evidence.map(|evidence| evidence.bind(db, bound));
                *bound_evidence =
                    bound_evidence.map(|bound_evidence| bound_evidence.bind(db, bound));
            }
        }
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
            // An operator calls an operator method or performs a built-in
            // operation.
            Expr::Bin(_, _, BinOp::Arith(_) | BinOp::Comp(_))
            | Expr::Un(_, UnOp::Minus | UnOp::Not | UnOp::BitNot)
            | Expr::AugAssign(..)
                if !matches!(expr_data, Expr::Bin(_, _, BinOp::Arith(ArithBinOp::Range)))
                    && self.semantic_expr_lowering(expr).is_none()
                    && self.builtin_op(expr).is_none() =>
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
            Expr::Un(
                inner,
                crate::hir_def::expr::UnOp::Mut
                | crate::hir_def::expr::UnOp::MaybeMut
                | crate::hir_def::expr::UnOp::Ref,
            ) if !self.has_lowerable_place(db, *inner)
                && !matches!(self.expr_prop(db, *inner).shape, Some(Shape::Opaque(_))) =>
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
                if (self.semantic_expr_lowering(expr).is_none()
                    || self.assigns_operator_result(expr))
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
            Stmt::For(pat, expr, ..) if self.for_loop_plan(stmt).is_none() => {
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
                    || is_grant_place_expr(db, body, expr, &|expr| {
                        self.expr_prop(db, expr).access().is_some()
                    })
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

    /// The assumptions the body's owner declares, in the order the body's
    /// evidence numbers them: a prefix of `assumptions`, which a closure in
    /// the body extends with its own. An instance receives proofs of these.
    pub fn signature(&self) -> PredicateListId<'db> {
        self.tables.signature
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

    /// The part of the projection's return shape the yield site `expr`
    /// grants.
    pub fn yield_shape(&self, expr: ExprId) -> Option<&Shape<'db>> {
        self.tables.yield_shapes[expr].as_ref()
    }

    /// Whether the index expression `expr` names an entry of a collection
    /// whose elements are places (`core::ops::StateIndex`), a path step.
    pub fn is_place_entry(&self, expr: ExprId) -> bool {
        self.tables.place_entries.contains(&expr)
    }

    pub fn builtin_op(&self, expr: ExprId) -> Option<BuiltinOp> {
        self.tables.builtin_ops.get(&expr).copied()
    }

    /// Whether the compound assignment `expr`, `x op= y`, assigns the result
    /// of its operator method, `x = x.wrapping_add(y)`, reading and writing
    /// the place once, rather than calling an `*Assign` method.
    pub fn assigns_operator_result(&self, expr: ExprId) -> bool {
        self.tables.operator_assigns.contains(&expr)
    }

    /// Whether `expr` is a place a pattern is matched against: its bindings
    /// take their components where they lie.
    pub fn is_matched_place(&self, expr: ExprId) -> bool {
        self.tables.matched_places.contains(&expr)
    }

    /// Whether the matched place `expr` has no `ref` or `mut` binding, so
    /// its tests read its value rather than open an access to it.
    pub fn is_inspected_place(&self, expr: ExprId) -> bool {
        self.tables.inspected_places.contains(&expr)
    }

    /// Whether the tuple `expr` is an unsafe split: one session granting
    /// each of its components, disjoint by the `unsafe` block's promise.
    pub fn is_unsafe_split(&self, expr: ExprId) -> bool {
        self.tables.unsafe_splits.contains(&expr)
    }

    /// Whether the tuple `expr` is passed to a product parameter, each of its
    /// elements as a part.
    pub fn is_product_arg(&self, expr: ExprId) -> bool {
        self.tables.product_args.contains(&expr)
    }

    /// The receiver of the call `expr`: a method call's receiver, or the
    /// callee of a call `f(..)` of a callable value, which runs a callable
    /// callable trait's method on it.
    pub fn call_receiver(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> Option<ExprId> {
        match expr.data(db, self.body()?) {
            Partial::Present(Expr::MethodCall(receiver, ..)) => Some(*receiver),
            Partial::Present(Expr::Call(callee, _)) if self.tables.value_calls.contains(&expr) => {
                Some(*callee)
            }
            _ => None,
        }
    }

    /// Whether the function's own authority covers the resources the `with`
    /// value `expr` names: its place lies in an effect provider, or a
    /// provider in scope holds each static slot handle it holds.
    pub fn provider_covered(&self, expr: ExprId) -> bool {
        self.tables.covered_providers.contains(&expr)
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

    pub fn closure_info(&self, expr: ExprId) -> Option<&ClosureInfo<'db>> {
        self.tables.closure_infos[expr].as_ref()
    }

    /// This (parent) typed body viewed as the body of `closure` called with
    /// a `receiver` environment: its parameters are the environment and the
    /// closure's parameters, and its result is the closure's. A closure's
    /// body is checked inline in its parent's, so every other table is
    /// shared.
    pub(crate) fn for_closure(
        &self,
        db: &'db dyn HirAnalysisDb,
        closure: ClosureTy<'db>,
        receiver: FuncParamMode,
    ) -> Self {
        let params = self
            .closure_info(closure.def(db).expr)
            .map(|info| info.params.clone())
            .unwrap_or_default();
        let mut body = self.clone();
        let tables = body.tables_mut();
        tables.result_ty = closure.ret_ty(db);
        tables.return_borrow_provider = None;
        tables.param_bindings = iter::once(LocalBinding::closure_env(db, closure, receiver))
            .chain(params)
            .collect();
        // A product parameter's inputs are its parts.
        let (products, parts) = (&tables.param_products, &tables.transport_parts);
        tables.input_bindings = tables
            .param_bindings
            .iter()
            .flat_map(
                |binding| match (binding, env::param_product(products, parts, binding)) {
                    (&LocalBinding::Param { site, idx, .. }, Some(product)) => {
                        env::product_inputs(db, site, idx, product).collect()
                    }
                    _ => vec![*binding],
                },
            )
            .collect();
        body
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

    pub fn return_borrow_provider(&self) -> Option<AddressSpace> {
        self.tables.return_borrow_provider
    }

    /// Get the binding for a function parameter by index.
    pub fn param_binding(&self, idx: usize) -> Option<LocalBinding<'db>> {
        self.tables.param_bindings.get(idx).copied()
    }

    /// The inputs a call passes the body, in order: each parameter, or each
    /// part of a product parameter.
    pub fn input_bindings(&self) -> &[LocalBinding<'db>] {
        &self.tables.input_bindings
    }

    /// The body of an instance whose opaque parameters' shapes are known, as
    /// `normalize` makes them: each takes the inputs its shape lists, as does
    /// a product with an opaque part.
    pub(crate) fn with_known_params(
        mut self,
        db: &'db dyn HirAnalysisDb,
        mut normalize: impl FnMut(Shape<'db>) -> Shape<'db>,
    ) -> Self {
        let tables = self.tables_mut();
        for (site, idx, product) in &mut tables.param_products {
            // An opaque part is one input, a value of the shape at its mode.
            let has_opaque = product
                .leaves(db)
                .iter()
                .any(|(access, ty)| access.is_none() && ty.as_shape_value(db).is_some());
            if !has_opaque {
                continue;
            }
            let known = normalize(product.clone());
            if known == *product {
                continue;
            }
            let part_of = |input: &LocalBinding<'db>| {
                matches!(*input, LocalBinding::ParamPart { site: part_site, idx: part_idx, .. }
                    if part_site == *site && part_idx == *idx)
            };
            if let Some(start) = tables.input_bindings.iter().position(part_of) {
                let end = start
                    + tables.input_bindings[start..]
                        .iter()
                        .take_while(|input| part_of(input))
                        .count();
                let inputs: Vec<_> = env::product_inputs(db, *site, *idx, &known).collect();
                tables.input_bindings.splice(start..end, inputs);
            }
            *product = known;
        }
        self
    }

    /// The body of an instance of `func` whose spread, its parameter
    /// `spread`, lists `listed`: a call passes those parameters last, each
    /// as an input of its own, or a product's parts.
    pub(crate) fn with_spread_inputs(
        mut self,
        db: &'db dyn HirAnalysisDb,
        func: Func<'db>,
        spread: usize,
        listed: Vec<Shape<'db>>,
    ) -> Self {
        let site = ParamSite::Func(func);
        for (idx, shape) in (spread..).zip(listed) {
            let inputs = &mut self.tables_mut().input_bindings;
            match shape.param_mode() {
                Some(mode) => inputs.push(LocalBinding::Param {
                    site,
                    idx,
                    mode,
                    ty: shape.erased_ty(db),
                    is_mut: false,
                }),
                None => inputs.extend(env::product_inputs(db, site, idx, &shape)),
            }
        }
        self
    }

    /// The position of `binding` among the body's inputs.
    pub fn input_index(&self, binding: LocalBinding<'db>) -> Option<usize> {
        self.tables
            .input_bindings
            .iter()
            .position(|input| *input == binding)
    }

    /// The input a return forwarding parameter `param`'s place at
    /// `projection` reads, and the steps it takes from that input's place: a
    /// product parameter's leading field steps select its part.
    pub fn return_source_input<'a>(
        &self,
        param: usize,
        projection: &'a [ReturnProjectionStep],
    ) -> Option<(usize, &'a [ReturnProjectionStep])> {
        let binding = self.param_binding(param)?;
        let Some(mut shape) = self.param_product(binding) else {
            return Some((self.input_index(binding)?, projection));
        };
        let mut part = 0;
        let mut steps = projection;
        while let Shape::Tuple(elems) = shape {
            let (ReturnProjectionStep::Field(field), rest) = steps.split_first()? else {
                return None;
            };
            let field = usize::from(*field);
            part += elems[..field.min(elems.len())]
                .iter()
                .map(Shape::leaf_count)
                .sum::<usize>();
            shape = elems.get(field)?;
            steps = rest;
        }
        Some((
            self.param_input_index(param, Some(u16::try_from(part).ok()?))?,
            steps,
        ))
    }

    /// The position among the body's inputs of parameter `param`, or of its
    /// part `part` if it is a product.
    pub fn param_input_index(&self, param: usize, part: Option<u16>) -> Option<usize> {
        let Some(part) = part else {
            return self.input_index(self.param_binding(param)?);
        };
        self.tables.input_bindings.iter().position(|input| {
            matches!(input, LocalBinding::ParamPart { idx, part: leaf, .. }
                if *idx == param && *leaf == part)
        })
    }

    /// The shape of `binding` if it is a product parameter's, the product
    /// the body destructures, or an opaque part of one bound whole.
    pub fn param_product(&self, binding: LocalBinding<'db>) -> Option<&Shape<'db>> {
        env::param_product(
            &self.tables.param_products,
            &self.tables.transport_parts,
            &binding,
        )
    }

    /// Get the binding for a local variable by its pattern.
    pub fn pat_binding(&self, pat: PatId) -> Option<LocalBinding<'db>> {
        self.tables.pat_bindings[pat]
    }

    /// Get how this local binding is captured by its source pattern destructuring.
    pub fn pat_binding_mode(&self, pat: PatId) -> Option<PatBindingMode> {
        self.tables.pat_binding_modes[pat]
    }

    /// How `binding` refers to its value.
    pub fn binding_access(&self, binding: LocalBinding<'db>) -> Option<BindingAccess> {
        env::binding_access(&binding, |pat| self.pat_binding_mode(pat))
    }

    /// Whether a place rooted in `binding` carries effect authority.
    pub fn binding_has_authority(&self, binding: LocalBinding<'db>) -> bool {
        env::binding_has_authority(&binding, |pat| self.pat_binding_mode(pat))
    }

    /// The semantic-IR type of `binding`: an access binding or a view or
    /// `mut` parameter is a carrier of its place.
    pub fn binding_carrier_ty(
        &self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> TyId<'db> {
        if let Some(product) = self.param_product(binding) {
            return product.carrier_ty(db);
        }
        let ty = self.binding_ty(db, binding);
        // A value of an opaque shape is carried by the shape's carrier.
        if let Some(shape) = ty.as_shape_value(db) {
            return Shape::Opaque(shape).carrier_ty(db);
        }
        match self.binding_access(binding) {
            None => ty,
            Some(BindingAccess::View) => TyId::view_of(db, ty),
            Some(BindingAccess::Ref) => TyId::borrow_ref_of(db, ty),
            Some(BindingAccess::Mut) => TyId::borrow_mut_of(db, ty),
        }
    }

    pub fn binding_ty(&self, db: &'db dyn HirAnalysisDb, binding: LocalBinding<'db>) -> TyId<'db> {
        match binding {
            LocalBinding::Local { pat, .. } => self.pat_ty(db, pat),
            LocalBinding::Param { ty, .. } | LocalBinding::ParamPart { ty, .. } => ty,
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

    /// The Collection protocol plan of a for-loop statement.
    pub fn for_loop_plan(&self, stmt: StmtId) -> Option<&ForLoopPlan<'db>> {
        self.tables.for_loop_plans[stmt].as_ref()
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
            if let Partial::Present(crate::hir_def::Stmt::Let(stmt_pat, _, Some(init_expr), _)) =
                stmt
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

        let (diags, typed_body) = infer_body(db, BodyOwner::func(db, func));
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
            let args = call_like_expr_args(expr_data, self.tables.value_calls.contains(&expr))?;
            // Constructors build an aggregate: `Some(variant)` for tuple
            // variants, `None` for tuple structs.
            let ctor_variant = match callable.callable_def() {
                CallableDef::VariantCtor(variant) => Some(Some(variant.idx)),
                CallableDef::StructCtor(_) => Some(None),
                _ => None,
            };
            if let Some(variant) = ctor_variant {
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
                    let field = u16::try_from(field).ok()?;
                    let projection = variant
                        .map_or(ReturnProjectionStep::Field(field), |variant| {
                            ReturnProjectionStep::VariantField { variant, field }
                        });
                    prefix_return_sources(&mut field_sources, &[projection]);
                    sources.extend(field_sources);
                }
                return (!sources.is_empty()).then_some(sources);
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
                ty.has_param(db) || ty.has_var(db)
            })
            && let Some(place) = self.expr_place(expr).cloned()
            && let Some(sources) =
                self.forwarded_return_sources_from_place(db, body, &place, seen, visited_locals)
        {
            return Some(sources);
        }

        match expr_data {
            Expr::Block(stmts, _) => {
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
            Expr::Un(
                inner,
                crate::hir_def::UnOp::Mut
                | crate::hir_def::UnOp::MaybeMut
                | crate::hir_def::UnOp::Ref,
            ) => self.forwarded_return_param_sources_from_expr(
                db,
                body,
                *inner,
                seen,
                visited_locals,
            ),
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
        }
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
        if let Some(method) = callable.selected_method(
            db,
            TraitSolveCx::new(db, body.scope()).with_assumptions(self.assumptions()),
        ) && let Some(body_func) = method.body()
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
                    CallableInputOrigin::Receiver
                } else {
                    CallableInputOrigin::ValueParam(idx)
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
                origin: CallableInputOrigin::Effect(idx),
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
            // A part is never named, so no place is rooted in one.
            LocalBinding::ParamPart { .. }
            | LocalBinding::Param {
                site: ParamSite::ContractInit(_) | ParamSite::Closure(_) | ParamSite::ClosureEnv(_),
                ..
            } => return None,
        };
        if place.has_entry() {
            return None;
        }
        let projection = place
            .projections
            .iter()
            .filter_map(|projection| match projection {
                PlaceProjection::Deref { .. } | PlaceProjection::Entry { .. } => None,
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
            CallableInputOrigin::Receiver => self.forwarded_return_param_sources_from_expr(
                db,
                body,
                *call_args.first()?,
                seen,
                visited_locals,
            ),
            CallableInputOrigin::ValueParam(param_idx) => self
                .forwarded_return_param_sources_from_expr(
                    db,
                    body,
                    *call_args.get(param_idx)?,
                    seen,
                    visited_locals,
                ),
            CallableInputOrigin::Effect(effect_idx) => {
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
        let projection = self.instantiate_return_projection(
            db,
            body,
            call_expr,
            &callee_source.projection,
            call_args,
        )?;
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
            } => Some(ReturnProjectionStep::ParamIndex {
                param: idx,
                part: None,
            }),
            // A product parameter's part, which the body's leading `let`
            // binds as the call passes it.
            binding @ LocalBinding::Local { pat, is_mut: false }
                if !matches!(
                    self.pat_binding_mode(pat),
                    Some(PatBindingMode::Access {
                        kind: BorrowKind::Mut,
                        ..
                    })
                ) =>
            {
                let source = self.binding_source(db, binding)?;
                let LocalBinding::Param {
                    site: ParamSite::Func(_),
                    idx,
                    ..
                } = self.expr_binding(source.init_expr)?
                else {
                    return None;
                };
                let (input, []) = self.return_source_input(idx, &source.projection)? else {
                    return None;
                };
                let LocalBinding::ParamPart { part, .. } = self.tables.input_bindings[input] else {
                    return None;
                };
                Some(ReturnProjectionStep::ParamIndex {
                    param: idx,
                    part: Some(part),
                })
            }
            LocalBinding::Local { .. }
            | LocalBinding::Param { .. }
            | LocalBinding::ParamPart { .. }
            | LocalBinding::EffectParam { .. } => None,
        }
    }

    /// The expression `arg`, a call's argument to a product parameter of
    /// `product`, passes as the product's `part`th part, if it writes the
    /// parts out.
    fn product_arg_part(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        arg: ExprId,
        product: &Shape<'db>,
        mut part: usize,
    ) -> Option<ExprId> {
        let (Shape::Tuple(parts), Partial::Present(Expr::Tuple(elems))) =
            (product, arg.data(db, body))
        else {
            return None;
        };
        if !self.is_product_arg(arg) {
            return None;
        }
        for (&elem, shape) in elems.iter().zip(parts) {
            let leaves = shape.leaves(db).len();
            if part < leaves {
                return match shape {
                    Shape::Tuple(_) => self.product_arg_part(db, body, elem, shape, part),
                    _ => Some(elem),
                };
            }
            part -= leaves;
        }
        None
    }

    fn instantiate_return_projection(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        call_expr: ExprId,
        projection: &[ReturnProjectionStep],
        call_args: &[ExprId],
    ) -> Option<Vec<ReturnProjectionStep>> {
        projection
            .iter()
            .map(|step| match *step {
                ReturnProjectionStep::ParamIndex { param, part } => {
                    let mut arg = *call_args.get(param)?;
                    if let Some(part) = part {
                        let callable = self.callable_expr(call_expr)?;
                        let product =
                            callable
                                .callable_def()
                                .param_product(db, param, callable.mode())?;
                        arg = self.product_arg_part(
                            db,
                            body,
                            arg,
                            product.skip_binder(),
                            usize::from(part),
                        )?;
                    }
                    self.return_index_projection(db, body, arg)
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
            crate::hir_def::Stmt::Let(_, _, Some(init), else_) => {
                for expr in std::iter::once(init).chain(else_) {
                    self.collect_explicit_return_param_sources_in_expr(
                        db,
                        body,
                        *expr,
                        out,
                        saw_non_param,
                        seen,
                    );
                }
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
            crate::hir_def::Stmt::Return(Some(expr)) | crate::hir_def::Stmt::Yield(expr) => {
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
            crate::hir_def::Stmt::Let(_, _, None, _)
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
            Expr::Block(stmts, _) => {
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
            Expr::Un(inner, _)
            | Expr::Cast(inner, _)
            | Expr::Field(inner, _)
            | Expr::Try(inner) => {
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
            Expr::Closure { .. } => {
                *saw_non_param = true;
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
            Expr::Block(stmts, _) => {
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
    pub fn expr_binding_def_span(
        &self,
        db: &'db dyn HirAnalysisDb,
        func: Func<'db>,
        expr: ExprId,
    ) -> Option<DynLazySpan<'db>> {
        let body = self.tables.body?;
        let binding = self.expr_binding(expr)?;
        Some(binding.def_span_with(db, body, func))
    }

    /// Like `expr_binding_def_span` but takes a `Body` directly.
    ///
    /// Use this when the body may not belong to a function (e.g., contract bodies).
    pub fn expr_binding_def_span_in_body(
        &self,
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: ExprId,
    ) -> Option<DynLazySpan<'db>> {
        let binding = self.expr_binding(expr)?;
        Some(binding.def_span_in_body(db, body))
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
            signature: PredicateListId::empty_list(db),
            assumptions: PredicateListId::empty_list(db),
            pat_ty: SecondaryMap::new(),
            expr_ty: SecondaryMap::new(),
            yield_shapes: SecondaryMap::new(),
            place_entries: FxHashSet::default(),
            unsafe_splits: FxHashSet::default(),
            product_args: FxHashSet::default(),
            value_calls: FxHashSet::default(),
            builtin_ops: FxHashMap::default(),
            operator_assigns: FxHashSet::default(),
            matched_places: FxHashSet::default(),
            inspected_places: FxHashSet::default(),
            covered_providers: FxHashSet::default(),
            const_refs: SecondaryMap::new(),
            value_path_refs: SecondaryMap::new(),
            semantic_expr_lowering: SecondaryMap::new(),
            record_init_lowering: SecondaryMap::new(),
            closure_infos: SecondaryMap::new(),
            resolved_field_index: SecondaryMap::new(),
            call_effect_args: SecondaryMap::new(),
            return_borrow_provider: None,
            param_bindings: Vec::new(),
            input_bindings: Vec::new(),
            param_products: Vec::new(),
            transport_parts: Vec::new(),
            pat_bindings: SecondaryMap::new(),
            pat_binding_modes: SecondaryMap::new(),
            pattern_store: PatternStore::default(),
            pattern_status: SecondaryMap::with_default(PatternAnalysisStatus::Invalid),
            for_loop_plans: SecondaryMap::new(),
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
                    | ReturnProjectionStep::ParamIndex { .. }
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
        if self.body.pat_binding(pat).is_some()
            && !matches!(
                self.body.pat_binding_mode(pat),
                Some(PatBindingMode::Access { .. })
            )
        {
            self.check_state_only_value(ty, span.clone().into(), "bound by value");
        }

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

        match expr_data {
            Expr::RecordInit(..) | Expr::Path(..)
                if matches!(expr_data, Expr::RecordInit(..))
                    || matches!(
                        self.body.value_path_ref(expr),
                        Some(ValuePathRef::UnitVariant(_))
                    ) =>
            {
                let ty = self.body.expr_prop(self.db, expr).ty;
                self.check_view_construction(ty, ctxt.span().unwrap().into());
                // A unit variant at a projection's yield site builds the sum
                // shape it yields, not a value.
                if !matches!(self.body.yield_shape(expr), Some(Shape::Sum { .. })) {
                    self.check_state_only_value(
                        ty,
                        ctxt.span().unwrap().into(),
                        "constructed as a value",
                    );
                }
            }
            // A tuple of accesses is a projection's yield, an unsafe split or
            // reported as an access used as a value. A product argument is no
            // value: its elements are arguments of their own.
            Expr::Tuple(elems) | Expr::Array(elems)
                if !self.body.yield_shape(expr).is_some_and(Shape::has_access)
                    && !self.body.is_product_arg(expr)
                    && !elems
                        .iter()
                        .any(|&elem| self.body.expr_prop(self.db, elem).shape.is_some()) =>
            {
                let ty = self.body.expr_prop(self.db, expr).ty;
                self.check_state_only_value(
                    ty,
                    ctxt.span().unwrap().into(),
                    "constructed as a value",
                );
            }
            // A closure's signature takes `#[view]` and state-only types
            // only where a function's may.
            Expr::Closure { ret_ty, .. } => {
                let db = self.db;
                if let Some(closure) = self.body.expr_prop(db, expr).ty.as_closure(db)
                    && let Some(body) = self.body.body()
                {
                    let normalize = |ty| normalize_ty(db, ty, body.scope(), self.assumptions);
                    let span = ctxt.span().unwrap().into_closure_expr();
                    let mut diags = Vec::new();
                    // Each part of a product is checked as a parameter of its
                    // mode.
                    let parts = closure
                        .params(db)
                        .iter()
                        .enumerate()
                        .flat_map(|(idx, param)| {
                            param
                                .part_modes(db)
                                .into_iter()
                                .map(move |part| (idx, part))
                        });
                    for (idx, (mode, ty)) in parts {
                        let ty = normalize(ty);
                        let span: DynLazySpan<'db> = span.clone().params().param(idx).into();
                        if let Some((ty, mode)) = view_param_misuse(db, mode, ty) {
                            diags.push(TyLowerDiag::ViewTypeMode {
                                span: span.clone(),
                                ty,
                                mode,
                            });
                        }
                        if mode == FuncParamMode::Own
                            && let Some(collection) = ty.pinned_part(db)
                        {
                            diags.push(TyLowerDiag::StateOnlyValue {
                                span,
                                ty,
                                collection,
                                position: "an `own` parameter",
                            });
                        }
                    }
                    let ret_span: DynLazySpan<'db> = if ret_ty.is_some() {
                        span.ret_ty().into()
                    } else {
                        span.body().into()
                    };
                    let ret = Shape::Owned(normalize(closure.ret_ty(db)));
                    if let Some((ty, mode)) = ret.view_misuse(db) {
                        diags.push(TyLowerDiag::ViewTypeMode {
                            span: ret_span.clone(),
                            ty,
                            mode,
                        });
                    }
                    if let Some((ty, collection)) = ret.state_only_value(db) {
                        diags.push(TyLowerDiag::StateOnlyValue {
                            span: ret_span,
                            ty,
                            collection,
                            position: "returned by value",
                        });
                    }
                    self.diags.extend(
                        diags
                            .into_iter()
                            .map(|diag| TyDiagCollection::from(diag).into()),
                    );
                }
            }
            Expr::Call(..) | Expr::MethodCall(..) => {
                if let Some(callable) = self.body.callable_expr(expr).cloned() {
                    self.check_view_call(&callable, expr, ctxt.span().unwrap().into());
                    self.check_state_only_call(&callable, expr, ctxt.span().unwrap().into());
                }
            }
            _ => {}
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
        checker.check_access_captures();
        for (_, _, diag) in std::mem::take(&mut checker.missing_effects) {
            checker.push_diag(diag);
        }
        let mut body = checker.env.finish(&mut checker.table);
        body.tables_mut().return_borrow_provider = checker.first_return_borrow_provider;
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

    /// A `#[view]` value is built only in its type's module, by the
    /// projections that yield it.
    fn check_view_construction(&mut self, ty: TyId<'db>, span: DynLazySpan<'db>) {
        let db = self.db;
        if ty.is_view(db)
            && let Some(adt) = ty.base_ty(db).adt_ref(db)
            && let Some(module) = adt.scope().parent_module(db)
            && let Some(body) = self.body.body()
            && !body.scope().is_transitive_child_of(db, module)
        {
            let diag = TyLowerDiag::ViewTypeMode {
                span,
                ty,
                mode: "constructed outside its defining module",
            };
            self.diags.push(TyDiagCollection::from(diag).into());
        }
    }

    /// A call constructing a `#[view]` value, or instantiating its callee at
    /// a `#[view]` type in a position a view type cannot take.
    fn check_view_call(&mut self, callable: &Callable<'db>, expr: ExprId, span: DynLazySpan<'db>) {
        let db = self.db;
        if let CallableDef::VariantCtor(_) | CallableDef::StructCtor(_) = callable.callable_def {
            let ty = self.body.expr_prop(db, expr).ty;
            return self.check_view_construction(ty, span);
        }
        // A callee's declared signature is checked where it is declared.
        let def = callable.callable_def;
        if callable.generic_args().is_empty()
            || matches!(def, CallableDef::Func(func) if !func.diags_view_types(db).is_empty())
        {
            return;
        }
        let Some(body) = self.body.body() else {
            return;
        };
        let (scope, assumptions) = (body.scope(), self.assumptions);
        let misuse = callable
            .input_modes(db)
            .into_iter()
            .find_map(|(mode, ty)| {
                view_param_misuse(db, mode, normalize_ty(db, ty, scope, assumptions))
            })
            .or_else(|| {
                let ret = callable
                    .ret_shape(db)
                    .unwrap_or_else(|| Shape::Owned(callable.ret_ty(db)));
                normalize_ty(db, ret, scope, assumptions).view_misuse(db)
            });
        if let Some((ty, position)) = misuse {
            self.diags.push(
                BodyDiag::ViewInstantiation {
                    primary: span,
                    ty,
                    position,
                }
                .into(),
            );
        }
    }

    /// A state-only type is never a value: it lives at its storage place.
    fn check_state_only_value(
        &mut self,
        ty: TyId<'db>,
        span: DynLazySpan<'db>,
        position: &'static str,
    ) {
        if let Some(collection) = ty.pinned_part(self.db) {
            let diag = TyLowerDiag::StateOnlyValue {
                span,
                ty,
                collection,
                position,
            };
            self.diags.push(TyDiagCollection::from(diag).into());
        }
    }

    /// A call constructing a state-only value, or instantiating its callee
    /// so that a state-only value would pass through an `own` parameter or
    /// an owned return. A variant constructor at a projection's yield site
    /// builds the sum shape it yields, not a value.
    fn check_state_only_call(
        &mut self,
        callable: &Callable<'db>,
        expr: ExprId,
        span: DynLazySpan<'db>,
    ) {
        let db = self.db;
        if let CallableDef::VariantCtor(_) = callable.callable_def {
            if matches!(self.body.yield_shape(expr), Some(Shape::Sum { .. })) {
                return;
            }
            let ty = self.body.expr_prop(db, expr).ty;
            return self.check_state_only_value(ty, span, "constructed as a value");
        }
        if let CallableDef::StructCtor(_) = callable.callable_def {
            let ty = self.body.expr_prop(db, expr).ty;
            return self.check_state_only_value(ty, span, "constructed as a value");
        }
        // A callee's declared signature is checked where it is declared.
        let def = callable.callable_def;
        if callable.generic_args().is_empty()
            || matches!(def, CallableDef::Func(func) if !func.diags_state_only_types(db).is_empty())
        {
            return;
        }
        let Some(body) = self.body.body() else {
            return;
        };
        let (scope, assumptions) = (body.scope(), self.assumptions);
        let value = callable
            .input_modes(db)
            .into_iter()
            .find_map(|(mode, ty)| {
                let ty = normalize_ty(db, ty, scope, assumptions);
                (mode == FuncParamMode::Own)
                    .then(|| ty.pinned_part(db).map(|collection| (ty, collection)))?
            })
            .or_else(|| {
                let ret = callable
                    .ret_shape(db)
                    .unwrap_or_else(|| Shape::Owned(callable.ret_ty(db)));
                normalize_ty(db, ret, scope, assumptions).state_only_value(db)
            });
        if let Some((ty, collection)) = value {
            let diag = TyLowerDiag::StateOnlyValue {
                span,
                ty,
                collection,
                position: "passed or returned by value",
            };
            self.diags.push(TyDiagCollection::from(diag).into());
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

/// The provider two joined accesses share, if any. Accesses backed by
/// different providers join only at yield sites, where the access checker
/// requires one address space per instantiation.
pub(super) fn merged_borrow_provider(
    previous: Option<AddressSpace>,
    current: Option<AddressSpace>,
) -> Option<AddressSpace> {
    previous
        .zip(current)
        .and_then(|(previous, current)| (previous == current).then_some(previous))
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
