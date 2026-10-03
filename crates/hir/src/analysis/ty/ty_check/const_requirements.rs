//! Const declaration requirements are discharged after inference, for every body
//! owner. Concrete discharge uses ordinary CTFE. Symbolic forwarding compares
//! resolved, typed expressions after substitution, without evaluating
//! unknown parameters or assuming the obligation being checked.
use super::*;
use crate::analysis::name_resolution::{
    method_selection::{MethodCandidate, select_method_candidate},
    resolve_path_with_minter,
};
use crate::analysis::ty::canonical::Canonicalized;
use crate::analysis::ty::{
    const_ty::ConstBodyLowering,
    subst::substitute_complete,
    ty_lower::{CompleteSubst, SubstError, lower_hir_ty_with_resolutions, lower_type_alias},
};
use crate::hir_def::{GenericArg, GenericArgListId, ItemKind, UnOp, scope_graph::ScopeId};

#[derive(Debug, Clone, PartialEq, Eq)]
struct PredicateKey<'db> {
    ty: TyId<'db>,
    arithmetic: crate::hir_def::attr::ArithmeticMode,
    operation: Option<Callable<'db>>,
    term: PredicateTerm<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum PredicateTerm<'db> {
    Literal(LitKind<'db>),
    TypeConst(TyId<'db>),
    Const(Const<'db>),
    TraitConst(TraitInstId<'db>, crate::hir_def::IdentId<'db>),
    InherentConst(
        crate::hir_def::Impl<'db>,
        TyId<'db>,
        crate::hir_def::IdentId<'db>,
    ),
    Binary(BinOp, Box<PredicateKey<'db>>, Box<PredicateKey<'db>>),
    Unary(UnOp, Box<PredicateKey<'db>>),
    Cast(Box<PredicateKey<'db>>),
    Call(Vec<PredicateKey<'db>>),
}

// A declaration's full parameter schema also resolves the parameters that
// lexical lookup keeps owned by an enclosing impl, so one simultaneous
// substitution instantiates a method's requirement with its callable arguments.
fn requirement_subst<'db>(
    db: &'db dyn HirAnalysisDb,
    declaration: WhereClauseOwner<'db>,
    args: &[TyId<'db>],
) -> Result<CompleteSubst<'db>, SubstError<'db>> {
    CompleteSubst::for_owner(db, declaration.into(), args.to_vec())
}

// Arguments inferred in a method body can mention its impl's parameters as the
// impl owns them. Rebase them onto the method's own formals, which is how the
// caller's premises are instantiated, so both sides compare in one basis. A
// rebase that fails is an error, as an instantiation that fails is
// (`requirement_subst`): comparing in two bases could match the wrong premise
// or miss the right one.
fn caller_args<'db>(
    db: &'db dyn HirAnalysisDb,
    caller: GenericParamOwner<'db>,
    args: Vec<TyId<'db>>,
) -> Result<Vec<TyId<'db>>, SubstError<'db>> {
    let identity = collect_generic_params(db, caller).params(db).to_vec();
    let subst = CompleteSubst::for_owner(db, caller, identity)?;
    substitute_complete(db, args, &subst)
}

/// The forwardable form of a predicate's expression, instantiated with
/// `subst`, or `None` for an expression that cannot be forwarded. A
/// substitution that fails is an error, not an expression that cannot be
/// forwarded: it would report the wrong failure, or keep a premise from
/// matching.
fn predicate_key<'db>(
    db: &'db dyn HirAnalysisDb,
    predicate: Body<'db>,
    typed: &TypedBody<'db>,
    subst: &CompleteSubst<'db>,
) -> Result<Option<PredicateKey<'db>>, SubstError<'db>> {
    PredicateKeyBuilder {
        db,
        body: predicate,
        typed,
        subst,
        arithmetic: BodyOwner::const_predicate(db, predicate).arithmetic_mode(db),
    }
    .key(predicate.expr(db))
}

/// What building a predicate's key needs at every node of its expression.
struct PredicateKeyBuilder<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    typed: &'a TypedBody<'db>,
    subst: &'a CompleteSubst<'db>,
    /// The predicate's arithmetic mode, which each node's key records.
    arithmetic: crate::hir_def::attr::ArithmeticMode,
}

impl<'db> PredicateKeyBuilder<'_, 'db> {
    fn instantiate<T>(&self, value: T) -> Result<T, SubstError<'db>>
    where
        T: TyFoldable<'db>,
    {
        substitute_complete(self.db, value, self.subst)
    }

    fn key(&self, expr: ExprId) -> Result<Option<PredicateKey<'db>>, SubstError<'db>> {
        let (db, typed) = (self.db, self.typed);
        let Some(data) = expr.data(db, self.body).borrowed().to_opt() else {
            return Ok(None);
        };
        let callable = typed.callable_expr(expr);
        let term = match data {
            Expr::Lit(lit) => PredicateTerm::Literal(*lit),
            Expr::Path(_) => {
                if let Some(ValuePathRef::TypeConst(ty)) = typed.value_path_ref(expr) {
                    PredicateTerm::TypeConst(self.instantiate(ty)?)
                } else {
                    // A reference's lookup scope is provenance, not its identity.
                    // Keep the resolved declaration and substituted receiver/goal.
                    match typed.expr_const_ref(expr) {
                        None => return Ok(None),
                        Some(ConstRef::Const(constant)) => PredicateTerm::Const(constant),
                        Some(ConstRef::TraitConst(reference)) => PredicateTerm::TraitConst(
                            self.instantiate(reference.inst())?,
                            reference.name(),
                        ),
                        Some(ConstRef::InherentConst(reference)) => PredicateTerm::InherentConst(
                            reference.impl_(),
                            self.instantiate(reference.receiver_ty())?,
                            reference.name(),
                        ),
                    }
                }
            }
            Expr::Bin(lhs, rhs, op) => {
                let (Some(lhs), Some(rhs)) = (self.key(*lhs)?, self.key(*rhs)?) else {
                    return Ok(None);
                };
                PredicateTerm::Binary(*op, Box::new(lhs), Box::new(rhs))
            }
            Expr::Un(value, op) => {
                let Some(value) = self.key(*value)? else {
                    return Ok(None);
                };
                PredicateTerm::Unary(*op, Box::new(value))
            }
            Expr::Cast(value, _) => {
                let Some(value) = self.key(*value)? else {
                    return Ok(None);
                };
                PredicateTerm::Cast(Box::new(value))
            }
            Expr::Call(_, call_args) => {
                if callable.is_none() {
                    return Ok(None);
                }
                let args = call_args
                    .iter()
                    .map(|arg| self.key(arg.expr))
                    .collect::<Result<Option<_>, _>>()?;
                let Some(args) = args else {
                    return Ok(None);
                };
                PredicateTerm::Call(args)
            }
            _ => return Ok(None),
        };
        Ok(Some(PredicateKey {
            ty: self.instantiate(typed.expr_ty(db, expr))?,
            arithmetic: self.arithmetic,
            operation: callable
                .map(|callable| self.instantiate(callable.clone()))
                .transpose()?,
            term,
        }))
    }
}

/// The generic parameters that a predicate's expression mentions, each
/// once, as types. The ambient trait assumptions it is checked under are not
/// dependencies of the expression, so their parameters are left out.
#[salsa::tracked(return_ref)]
fn predicate_params<'db>(db: &'db dyn HirAnalysisDb, predicate: Body<'db>) -> Vec<TyId<'db>> {
    use crate::analysis::ty::visitor::walk_ty;
    struct Params<'db> {
        db: &'db dyn HirAnalysisDb,
        found: Vec<TyId<'db>>,
    }
    impl<'db> TyVisitor<'db> for Params<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            let is_param = match ty.data(self.db) {
                TyData::TyParam(_) => true,
                TyData::ConstTy(const_ty) => {
                    matches!(const_ty.data(self.db), ConstTyData::TyParam(..))
                }
                _ => false,
            };
            if is_param && !self.found.contains(&ty) {
                self.found.push(ty);
            }
            walk_ty(self, ty);
        }
    }
    let mut params = Params {
        db,
        found: Vec::new(),
    };
    infer_body(db, BodyOwner::const_predicate(db, predicate))
        .1
        .visit_body_types(&mut params);
    params.found
}

/// Why a const requirement does not hold at a use. Discharge decides it, and
/// the use's diagnostic renders it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum RequirementFailure {
    /// The condition evaluated to `false`.
    False,
    /// The requirement depends on itself.
    Recursive,
    /// Compile-time evaluation of the condition stopped.
    Evaluation(EvaluationStop),
    /// The condition is ill-formed, or its evaluation gave no `bool`.
    NotEstablished,
    /// The condition could not be instantiated with the use's arguments.
    NotInstantiable,
    /// A generic use states no identical condition.
    NoMatchingPremise,
    /// A generic use cannot forward the condition's expression.
    NotForwardable,
    /// A record with conditions that is not fully applied.
    PartiallyAppliedRecord,
    /// An enum with conditions that is not fully applied.
    PartiallyAppliedEnum,
}

/// Why compile-time evaluation of a condition stopped.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum EvaluationStop {
    DivisionByZero,
    Overflow,
    StepLimit,
    RecursionLimit,
    RecursiveConst,
}

impl RequirementFailure {
    fn from_evaluation(cause: &InvalidCause<'_>) -> Self {
        Self::Evaluation(match cause {
            InvalidCause::ConstEvalDivisionByZero { .. } => EvaluationStop::DivisionByZero,
            InvalidCause::ConstEvalArithmeticOverflow { .. } => EvaluationStop::Overflow,
            InvalidCause::ConstEvalStepLimitExceeded { .. } => EvaluationStop::StepLimit,
            InvalidCause::ConstEvalRecursionLimitExceeded { .. } => EvaluationStop::RecursionLimit,
            InvalidCause::ConstEvalRecursiveConst { .. } => EvaluationStop::RecursiveConst,
            _ => return Self::NotEstablished,
        })
    }
}

/// Whether a use's const requirement holds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub(super) enum Discharge {
    Holds,
    Fails(RequirementFailure),
}

/// Whether a predicate can be discharged.
#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub(super) enum FormationStatus<'db> {
    /// It type checks as a `bool` const body and does not depend on itself.
    /// Discharge reads its typed body.
    WellFormed(TypedBody<'db>),
    IllFormed,
    Recursive,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub(super) struct PredicateFormation<'db> {
    /// The predicate's own diagnostics, reported at its declaration.
    pub(super) diags: Vec<FuncBodyDiag<'db>>,
    pub(super) status: FormationStatus<'db>,
}

impl<'db> PredicateFormation<'db> {
    /// The typed body of a well-formed predicate.
    pub(super) fn well_formed(&self) -> Option<&TypedBody<'db>> {
        match &self.status {
            FormationStatus::WellFormed(typed) => Some(typed),
            FormationStatus::IllFormed | FormationStatus::Recursive => None,
        }
    }
}

/// A body's requirement check: its diagnostics, and whether a requirement it
/// uses depends on itself, which makes a predicate's formation recursive.
#[derive(Default)]
pub(super) struct RequirementCheck<'db> {
    pub(super) diags: Vec<FuncBodyDiag<'db>>,
    pub(super) recursive: bool,
}

impl<'db> RequirementCheck<'db> {
    fn unmet(
        &mut self,
        primary: DynLazySpan<'db>,
        predicate: Body<'db>,
        failure: RequirementFailure,
    ) {
        self.recursive |= failure == RequirementFailure::Recursive;
        self.diags
            .push(unmet_requirement_diag(primary, predicate, failure, None));
    }

    /// Reports the unmet requirement of a type that only inference gives
    /// at `primary`, naming the type.
    fn unmet_inferred(&mut self, primary: DynLazySpan<'db>, unmet: &TypeRequirementFailure<'db>) {
        self.recursive |= unmet.failure == RequirementFailure::Recursive;
        self.diags.push(unmet_requirement_diag(
            primary,
            unmet.predicate,
            unmet.failure,
            Some(unmet.ty),
        ));
    }

    /// Reports a type application's unmet requirement, and keeps the
    /// application in `entered`.
    fn unmet_type(
        &mut self,
        primary: DynLazySpan<'db>,
        unmet: TypeRequirementFailure<'db>,
        entered: &mut FxHashSet<TyId<'db>>,
    ) {
        entered.insert(unmet.ty);
        self.unmet(primary, unmet.predicate, unmet.failure);
    }

    fn extend(&mut self, other: Self) {
        self.recursive |= other.recursive;
        self.diags.extend(other.diags);
    }
}

fn unmet_requirement_diag<'db>(
    primary: DynLazySpan<'db>,
    predicate: Body<'db>,
    failure: RequirementFailure,
    inferred: Option<TyId<'db>>,
) -> FuncBodyDiag<'db> {
    BodyDiag::ConstRequirementNotSatisfied {
        primary,
        predicate: predicate.span().into(),
        reason: failure,
        inferred,
    }
    .into()
}

#[salsa::tracked(return_ref, cycle_initial=formation_cycle_initial, cycle_fn=formation_cycle_recover)]
pub(super) fn check_predicate_formation<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> PredicateFormation<'db> {
    let owner = BodyOwner::const_predicate(db, body);
    // A condition the parser could not read has no expression to check or
    // evaluate. The parser reported it, at its position.
    if matches!(body.expr(db).data(db, body), Partial::Absent) {
        return PredicateFormation {
            diags: infer_body(db, owner).0.clone(),
            status: FormationStatus::IllFormed,
        };
    }
    // The condition is checked as every body is (`checked_body`).
    let CheckedBody {
        mut diags,
        typed,
        recursive,
    } = checked_body(db, owner);
    // A recursive failure is absorbing, and is reported once, at this predicate.
    let status = if recursive {
        diags = vec![BodyDiag::RecursiveConstRequirement(body.span().into()).into()];
        FormationStatus::Recursive
    } else if diags_allow_evaluation(db, &diags) {
        FormationStatus::WellFormed(typed)
    } else {
        FormationStatus::IllFormed
    };
    PredicateFormation { diags, status }
}

/// Whether uses of a declaration check `predicate`. A ground predicate, one
/// that mentions no generic parameter, is checked once where it is declared
/// (`check_where_const_predicates`), whether or not anything uses the item,
/// so a use does not report it again.
fn checked_at_uses<'db>(db: &'db dyn HirAnalysisDb, predicate: Body<'db>) -> bool {
    predicate_may_depend_on_params(db, predicate)
}

pub(super) fn predicate_may_depend_on_params<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> bool {
    !predicate_params(db, body).is_empty()
}

// Requirements scope over function signatures/bodies and ADT fields, but
// their formation must be checked without those assumptions. In particular,
// nested anonymous constants inside a predicate are part of its formation.
fn requirement_premise_owner<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Option<GenericParamOwner<'db>> {
    if !matches!(owner, BodyOwner::Func(_) | BodyOwner::AnonConstBody { .. }) {
        return None;
    }
    premise_owner_in_scope(db, owner.scope())
}

/// The declaration whose generic parameters the premises at `origin` are
/// stated over (see `caller_premises`).
fn premise_owner_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    origin: ScopeId<'db>,
) -> Option<GenericParamOwner<'db>> {
    let mut current = Some(origin);
    while let Some(scope) = current {
        match scope.item() {
            // A trait method can neither state conditions nor assume a header's.
            ItemKind::Func(func)
                if matches!(func.scope().parent_item(db), Some(ItemKind::Trait(_))) =>
            {
                return None;
            }
            item @ (ItemKind::Func(_) | ItemKind::Struct(_) | ItemKind::Enum(_)) => {
                let candidate = WhereClauseOwner::from_item_opt(item)?;
                if candidate
                    .where_clause(db)
                    .const_predicates(db)
                    .iter()
                    .any(|predicate| origin.is_transitive_child_of(db, predicate.scope()))
                {
                    return None;
                }
                return GenericParamOwner::from_item_opt(item);
            }
            item @ (ItemKind::Impl(_) | ItemKind::ImplTrait(_) | ItemKind::TypeAlias(_)) => {
                return GenericParamOwner::from_item_opt(item);
            }
            ItemKind::Body(_) => current = scope.parent(db),
            // A nested declaration does not inherit function premises.
            _ => return None,
        }
    }
    None
}

/// The conditions that hold where premises are stated over `caller`'s
/// generic parameters, each with the substitution that states it there:
/// - the caller's own conditions, when its uses check them;
/// - inside an impl, the conditions of the records and enums in its self
///   type, since every use of the impl instantiates the self type with a
///   checked type (`check_entered_header`, and the receiver and argument
///   types);
/// - in a type alias, the conditions of the aliased type, since every
///   application of the alias is checked after expansion.
///
/// An impl's own `where` conditions are not premises: they are rejected at
/// the impl, and no use checks them. Nor are the conditions of a trait impl's
/// trait arguments: trait solving can select the impl with arguments that no
/// written or checked type holds, as `call(Holder<0> {})` selects
/// `impl<const N: usize> Tr<Bounded<N>> for Holder<N>` for a bound `T: Tr<U>`.
/// So `Bounded<N>` in that header needs a condition the impl cannot state, and
/// the impl is rejected where it writes it.
fn caller_premises<'db>(
    db: &'db dyn HirAnalysisDb,
    caller: GenericParamOwner<'db>,
) -> Result<Vec<(Body<'db>, CompleteSubst<'db>)>, SubstError<'db>> {
    let item = ItemKind::from(caller);
    let mut premises = Vec::new();
    let own = match caller {
        GenericParamOwner::Func(func) => func.is_free_or_inherent(db),
        GenericParamOwner::Struct(_) | GenericParamOwner::Enum(_) => true,
        _ => false,
    };
    if own && let Some(owner) = WhereClauseOwner::from_item_opt(item) {
        let subst = requirement_subst(db, owner, collect_generic_params(db, caller).params(db))?;
        premises.extend(
            owner
                .where_clause(db)
                .const_predicates(db)
                .iter()
                .map(|&predicate| (predicate, subst.clone())),
        );
    }
    let implied = match caller {
        GenericParamOwner::Func(func) => func
            .scope()
            .parent_item(db)
            .and_then(|parent| impl_self_ty(db, parent)),
        GenericParamOwner::Impl(_) | GenericParamOwner::ImplTrait(_) => impl_self_ty(db, item),
        GenericParamOwner::TypeAlias(alias) => {
            Some(lower_type_alias(db, alias).alias_to.instantiate_identity())
        }
        _ => None,
    };
    for ty in caller_args(db, caller, implied.into_iter().collect())? {
        type_conditions(db, ty, &mut premises)?;
    }
    Ok(premises)
}

/// The record or enum declaration of `ty`'s base, which states the
/// conditions of its applications.
fn adt_declaration<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Option<WhereClauseOwner<'db>> {
    use crate::analysis::ty::adt_def::AdtRef;
    let TyData::TyBase(TyBase::Adt(adt)) = ty.base_ty(db).data(db) else {
        return None;
    };
    Some(match adt.adt_ref(db) {
        AdtRef::Struct(record) => WhereClauseOwner::Struct(record),
        AdtRef::Enum(enum_) => WhereClauseOwner::Enum(enum_),
    })
}

/// The conditions of each fully applied record or enum in `ty`, instantiated
/// with its arguments.
fn type_conditions<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    conditions: &mut Vec<(Body<'db>, CompleteSubst<'db>)>,
) -> Result<(), SubstError<'db>> {
    let (_, args) = ty.decompose_ty_app(db);
    for &arg in args {
        type_conditions(db, arg, conditions)?;
    }
    let Some(declaration) = adt_declaration(db, ty) else {
        return Ok(());
    };
    let predicates = declaration.where_clause(db).const_predicates(db);
    if predicates.is_empty() {
        return Ok(());
    }
    let subst = requirement_subst(db, declaration, args)?;
    conditions.extend(
        predicates
            .iter()
            .map(|&predicate| (predicate, subst.clone())),
    );
    Ok(())
}

/// A condition of a record or enum in `ty`, if any.
fn first_condition<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<Body<'db>> {
    ty.decompose_ty_app(db)
        .1
        .iter()
        .find_map(|&arg| first_condition(db, arg))
        .or_else(|| {
            adt_declaration(db, ty)?
                .where_clause(db)
                .const_predicates(db)
                .first()
                .copied()
        })
}

/// Checks the requirements of the types and callees that enter a typed body.
///
/// Requirements are checked where a type enters the body: a written type
/// (`written_type_check`), a path that passes through an application, or an
/// instantiating expression. A path does not report an application that a
/// written type carries, and an expression does not report one that either
/// carries, since those report it. A pattern, a binding use, or a block or
/// branch only carries a type that entered elsewhere, so checking it again
/// repeats that report. A function-typed expression is not checked for its
/// type arguments here: they are written types, checked where they are
/// written, or inferred, and the check of inferred types at the end covers
/// them. An item reached through a path checks the header types it
/// instantiates that no checked value carries (`check_entered_header`).
pub(super) fn check_body_requirements<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    typed: &TypedBody<'db>,
) -> RequirementCheck<'db> {
    let Some(body) = typed.body() else {
        return RequirementCheck::default();
    };
    let mut checker = BodyRequirements::new(db, owner, typed, body);
    checker.check_path_applications();
    let mut entered = Vec::new();
    for (expr, data) in body.exprs(db).iter() {
        if !checker.carries(expr) {
            entered.push(checker.check_expr(expr, data));
        }
    }
    let mut check = checker.check_inferred_types(&entered);
    for (nested, expected) in expression_const_bodies(db, body, typed) {
        check.extend(anon_const_position_check(db, nested, expected));
    }
    check
}

fn is_function_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    matches!(ty.base_ty(db).data(db), TyData::TyBase(TyBase::Func(_)))
}

/// An expression that `check_body_requirements` checked, with its type.
struct CheckedExpr<'db> {
    expr: ExprId,
    ty: TyId<'db>,
    /// Whether a requirement of `ty` failed.
    failed: bool,
}

/// The state of `check_body_requirements` for one body.
struct BodyRequirements<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    typed: &'a TypedBody<'db>,
    body: Body<'db>,
    /// The generic owner whose conditions discharge a use's requirements.
    premise_owner: Option<GenericParamOwner<'db>>,
    /// The callee expressions of the body's calls.
    direct_callees: FxHashSet<ExprId>,
    /// The applications that a written type or a path carries, which an
    /// expression does not report again.
    written: FxHashSet<TyId<'db>>,
    /// The failing applications reported so far.
    entered: FxHashSet<TyId<'db>>,
    check: RequirementCheck<'db>,
}

impl<'a, 'db> BodyRequirements<'a, 'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
        typed: &'a TypedBody<'db>,
        body: Body<'db>,
    ) -> Self {
        let direct_callees = body
            .exprs(db)
            .values()
            .filter_map(|expr| match expr.borrowed().to_opt()? {
                Expr::Call(callee, _) => Some(*callee),
                _ => None,
            })
            .collect();
        Self {
            db,
            scope: owner.scope(),
            typed,
            body,
            premise_owner: requirement_premise_owner(db, owner),
            direct_callees,
            written: written_applications(db, owner),
            entered: FxHashSet::default(),
            check: RequirementCheck::default(),
        }
    }

    /// Whether `expr` only carries a type that entered elsewhere.
    fn carries(&self, expr: ExprId) -> bool {
        self.typed.expr_binding(expr).is_some()
            || matches!(
                expr.data(self.db, self.body).borrowed().to_opt(),
                Some(Expr::Block(..) | Expr::If(..) | Expr::Match(..) | Expr::With(..))
            )
    }

    /// Whether `expr` is a path that names a function, as opposed to a
    /// binding use or another expression that holds a function value.
    fn names_function_value(&self, expr: ExprId) -> bool {
        self.typed.expr_binding(expr).is_none()
            && matches!(
                expr.data(self.db, self.body).borrowed().to_opt(),
                Some(Expr::Path(..))
            )
    }

    fn unmet_type_at(&mut self, expr: ExprId, unmet: TypeRequirementFailure<'db>) {
        self.check
            .unmet_type(expr.span(self.body).into(), unmet, &mut self.entered);
    }

    /// Checks the applications that the body's paths pass through, once per
    /// site, and adds them to the written applications.
    fn check_path_applications(&mut self) {
        let mut reported_paths = FxHashSet::default();
        for (site, application) in self.typed.path_applications() {
            if !reported_paths.insert((site, *application)) {
                continue;
            }
            if let Some(unmet) =
                check_path_application(self.db, *application, self.scope, &self.written)
            {
                self.check
                    .unmet_type(site.clone(), unmet, &mut self.entered);
            }
        }
        self.written
            .extend(self.typed.path_applications().iter().map(|&(_, ty)| ty));
    }

    /// Checks the type of an expression that does not only carry one, the
    /// header types a constant reference enters, and a callee's
    /// requirements.
    fn check_expr(&mut self, expr: ExprId, data: &Partial<Expr<'db>>) -> CheckedExpr<'db> {
        let ty = self.typed.expr_ty(self.db, expr);
        let mut failed = false;
        if !is_function_ty(self.db, ty)
            && let Some(unmet) = check_type_requirements(self.db, ty, self.scope, &self.written)
        {
            failed = true;
            self.unmet_type_at(expr, unmet);
        }
        if let Some(headers) = const_ref_headers(self.db, self.typed, expr)
            && let Some(unmet) = check_entered_header(
                self.db,
                self.typed,
                expr,
                headers,
                self.scope,
                &self.written,
            )
        {
            self.unmet_type_at(expr, unmet);
        }
        self.check_callee(expr, data, ty);
        CheckedExpr { expr, ty, failed }
    }

    /// Checks the requirements of the function or constructor that `expr`
    /// calls or names as a value.
    fn check_callee(&mut self, expr: ExprId, data: &Partial<Expr<'db>>, ty: TyId<'db>) {
        let callable = self.typed.callable_expr(expr);
        if callable.is_none() && self.direct_callees.contains(&expr) {
            return;
        }
        // A function value enters the body where it is written, at a path
        // that names the function. A call through anything else, such as a
        // binding, block, branch or field, calls such a value, whose
        // requirements were checked there.
        if let Some(Expr::Call(callee, _)) = data.borrowed().to_opt()
            && !self.names_function_value(*callee)
        {
            return;
        }
        let (definition, args) = match callable {
            Some(callable) => (callable.callable_def(), callable.generic_args()),
            // Any other expression with a function type, such as a field
            // read, carries a value written elsewhere.
            None if !self.names_function_value(expr) => return,
            None => {
                let (base, args) = ty.decompose_ty_app(self.db);
                let TyData::TyBase(TyBase::Func(definition)) = base.data(self.db) else {
                    return;
                };
                (*definition, args)
            }
        };
        match definition {
            CallableDef::Func(func) => {
                self.check_callee_headers(expr, func, args);
                self.discharge_callee_conditions(expr, func, args);
            }
            CallableDef::VariantCtor(_) | CallableDef::StructCtor(_) => {
                self.check_constructor_value(expr, data, definition, args);
            }
        }
    }

    /// A constructor used as a value enters its enum type here, since no call
    /// expression instantiates it. A called constructor's enum type enters at
    /// the call, whose type is checked in `check_expr`.
    fn check_constructor_value(
        &mut self,
        expr: ExprId,
        data: &Partial<Expr<'db>>,
        definition: CallableDef<'db>,
        args: &[TyId<'db>],
    ) {
        if matches!(data.borrowed().to_opt(), Some(Expr::Path(..)))
            && !self.direct_callees.contains(&expr)
            && let Some(unmet) = check_type_requirements(
                self.db,
                definition.ret_ty(self.db).instantiate(self.db, args),
                self.scope,
                &self.written,
            )
        {
            self.unmet_type_at(expr, unmet);
        }
    }

    /// Checks the header types that a use of `func` instantiates.
    fn check_callee_headers(&mut self, expr: ExprId, func: Func<'db>, args: &[TyId<'db>]) {
        match callee_headers(self.db, func, args) {
            Ok(headers) => {
                if let Some(unmet) = check_entered_header(
                    self.db,
                    self.typed,
                    expr,
                    headers,
                    self.scope,
                    &self.written,
                ) {
                    self.unmet_type_at(expr, unmet);
                }
            }
            // The instantiated headers are unknown, so a condition of a record
            // or enum that they could carry, from the declared header or from
            // the use's arguments, fails as a condition that discharge cannot
            // instantiate does.
            Err(_) => {
                if let Some(predicate) = declared_callee_headers(self.db, func)
                    .into_iter()
                    .chain(args.iter().copied())
                    .find_map(|ty| first_condition(self.db, ty))
                {
                    self.check.unmet(
                        expr.span(self.body).into(),
                        predicate,
                        RequirementFailure::NotInstantiable,
                    );
                }
            }
        }
    }

    /// Discharges the conditions of `func` for a use with `args`.
    ///
    /// Conditions are supported on free functions and inherent methods.
    /// Other associated or generic owner contexts are rejected at their
    /// declarations. Inherent calls keep ordinary method resolution:
    /// requirements constrain the resolved call and do not take part in
    /// candidate selection.
    fn discharge_callee_conditions(&mut self, expr: ExprId, func: Func<'db>, args: &[TyId<'db>]) {
        let db = self.db;
        let predicates = WhereClauseOwner::Func(func)
            .where_clause(db)
            .const_predicates(db);
        if predicates.is_empty() || !func.is_free_or_inherent(db) {
            return;
        }
        // Only a use whose arguments mention the owner's parameters can be
        // discharged by the owner's own conditions.
        let premise_owner = self
            .premise_owner
            .filter(|_| args.iter().any(|ty| ty.has_param(db)));
        for &predicate in predicates
            .iter()
            .filter(|&&predicate| checked_at_uses(db, predicate))
        {
            if let Discharge::Fails(failure) = discharge_requirement(
                db,
                WhereClauseOwner::Func(func),
                predicate,
                args.to_vec(),
                premise_owner,
            ) {
                self.check
                    .unmet(expr.span(self.body).into(), predicate, failure);
            }
        }
    }

    /// Checks every type that inference gave an expression, whatever the
    /// position: the expression's own type, including a function value's
    /// generic arguments, and the generic arguments inferred for each call and
    /// method call.
    ///
    /// The checks in `check_expr` find the positions where a type enters;
    /// this one does not depend on listing them. It catches, for example,
    /// `U = Bounded<0>` inferred by trait solving for `call(Holder<0> {})`
    /// with `fn call<U, T: Tr<U>>(_ x: T)`, which no written type, path or
    /// expression type holds. An application reported before is not reported
    /// again, and any other is reported once, at the first expression that
    /// carries it. Patterns, binding uses, blocks and branches are left out:
    /// their types come from an expression here or from the signature, and a
    /// signature type reaches the body with its layout holes instantiated, so
    /// it would not match the written type that reported it.
    ///
    /// An expression type that `check_expr` found no failure in has none here
    /// either, since `reported` only adds to `written`, so only a function
    /// value's type and the types that failed there are looked at again. A
    /// failure in a function value's type or a call's generic arguments is in
    /// a type that inference supplied, so its error names the type. A further
    /// failure in an expression type that already failed is not: the
    /// expression's type gives it, as it gave the first.
    fn check_inferred_types(self, exprs: &[CheckedExpr<'db>]) -> RequirementCheck<'db> {
        let Self {
            db,
            scope,
            typed,
            body,
            written,
            entered,
            mut check,
            ..
        } = self;
        let mut reported = written;
        reported.extend(entered);
        for &CheckedExpr { expr, ty, failed } in exprs {
            let own = if is_function_ty(db, ty) {
                Some((ty, true))
            } else {
                failed.then_some((ty, false))
            };
            let callable_args = typed
                .callable_expr(expr)
                .map(|callable| callable.generic_args())
                .unwrap_or_default();
            let types = own
                .into_iter()
                .chain(callable_args.iter().map(|&ty| (ty, true)));
            for (ty, inferred) in types {
                while let Some(unmet) = check_type_requirements(db, ty, scope, &reported) {
                    reported.insert(unmet.ty);
                    if inferred {
                        check.unmet_inferred(expr.span(body).into(), &unmet);
                    } else {
                        check.unmet(expr.span(body).into(), unmet.predicate, unmet.failure);
                    }
                }
            }
        }
        check
    }
}

/// What an anonymous constant checked against `expected` reports at its
/// position: its unmet requirements, or the inference failures that its
/// type's error cannot show. Type lowering evaluates such a constant but does
/// not check its requirements, since checking them evaluates code, which can
/// lower the type the constant is part of. So the constant's position owns
/// them, and its owner calls this: `check_body_requirements` for expression
/// positions and `check_declared_type_requirements` for types.
fn anon_const_position_check<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expected: TyId<'db>,
) -> RequirementCheck<'db> {
    use crate::analysis::ty::const_ty::{ConstBodyFailure, const_body_failure};
    let owner = BodyOwner::AnonConstBody { body, expected };
    let (diags, typed) = infer_body(db, owner);
    if diags_allow_evaluation(db, diags) {
        return check_body_requirements(db, owner, typed);
    }
    // A lone path to a constant is read as that constant, not checked as a
    // body, so its failures are reported where the constant is.
    let position = body.scope().parent(db).unwrap_or(body.scope());
    let mut check = RequirementCheck::default();
    if !crate::analysis::ty::ty_lower::const_body_names_a_constant(
        db,
        body,
        position,
        typed.assumptions(),
    ) && let ConstBodyFailure::AtPosition(_) = const_body_failure(db, body, diags, typed)
    {
        check.diags = diags.clone();
    }
    check
}

/// The anonymous constants written directly in `path`'s segments, such as
/// `{ n + 1 }` in `Bounded<{ n + 1 }>::helper`.
fn path_const_bodies<'db>(db: &'db dyn HirAnalysisDb, path: PathId<'db>) -> Vec<Body<'db>> {
    let mut bodies = Vec::new();
    let mut segment = Some(path);
    while let Some(current) = segment {
        bodies.extend(generic_arg_const_bodies(db, current.generic_args(db)));
        segment = current.parent(db);
    }
    bodies
}

fn generic_arg_const_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    args: GenericArgListId<'db>,
) -> impl Iterator<Item = Body<'db>> + 'db {
    args.data(db).iter().filter_map(|arg| match arg {
        GenericArg::Const(arg) => match arg.value {
            ConstGenericArgValue::Expr(Partial::Present(body)) => Some(body),
            _ => None,
        },
        _ => None,
    })
}

/// Pairs each of `bodies` with the type lowering checks it against, read
/// from `lowered`, a lowering that deferred its anonymous constants.
fn positioned_const_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    lowered: &[TyId<'db>],
    bodies: &[Body<'db>],
) -> Vec<(Body<'db>, TyId<'db>)> {
    unevaluated_const_bodies(db, lowered)
        .into_iter()
        .filter(|(body, _)| bodies.contains(body))
        .collect()
}

/// The anonymous constants passed to `path`'s segments, each with the type of
/// the parameter it is passed to, read from that segment's own resolution in
/// `resolutions` (a lowering that deferred its anonymous constants). An
/// argument is checked there even when its value never reaches what the path
/// names: an alias can drop it, and a qualifier such as `Holder<{ n }>` in
/// `Holder<{ n }>::Out` may only select an impl. A segment that did not
/// resolve is reported by resolution. A type, trait or callable segment's
/// resolution holds its arguments, and an argument it has no parameter for
/// is reported by lowering as an arity error. An enum variant or module
/// segment has no generic parameters and its resolution holds no arguments,
/// so arguments written on it, such as `{ n }` in `Choice::Empty<{ n }>`,
/// are not found here. Resolution does not reject them either, and nothing
/// evaluates them. The explicit arguments of a callable are applied with the
/// call, so its uses check them (`expression_const_bodies`).
fn segment_const_args<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    resolutions: &[(PathId<'db>, PathRes<'db>)],
) -> Vec<(Body<'db>, TyId<'db>)> {
    let mut found = Vec::new();
    let mut segment = Some(path);
    while let Some(current) = segment {
        segment = current.parent(db);
        let args = current.generic_args(db);
        let bodies: Vec<_> = generic_arg_const_bodies(db, args).collect();
        if bodies.is_empty() {
            continue;
        }
        let Some(res) = resolutions
            .iter()
            .rev()
            .find_map(|(resolved, res)| (*resolved == current).then_some(res))
        else {
            continue;
        };
        match res {
            // An alias's arguments are its own parameters', whether or not the
            // aliased type uses them.
            PathRes::TyAlias(alias, _) => {
                for (arg, param) in args.data(db).iter().zip(alias.params(db)) {
                    if let GenericArg::Const(arg) = arg
                        && let ConstGenericArgValue::Expr(Partial::Present(body)) = arg.value
                        && let Some(expected) = param.const_ty_ty(db)
                    {
                        found.push((body, expected));
                    }
                }
            }
            res => found.extend(positioned_const_bodies(
                db,
                &path_res_tys(db, res.clone()),
                &bodies,
            )),
        }
    }
    found
}

/// The type of an array's length, which a length is checked against.
fn array_length_ty<'db>(db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
    TyId::array(db, TyId::unit(db)).applicable_ty(db)?.const_ty
}

/// The types a path resolution carries, including trait arguments.
fn path_res_tys<'db>(db: &'db dyn HirAnalysisDb, res: PathRes<'db>) -> Vec<TyId<'db>> {
    let mut tys = Vec::new();
    match res {
        PathRes::Trait(inst) | PathRes::TraitMethod(inst, _) => {
            tys.extend(inst.args(db).iter().copied());
        }
        PathRes::TraitConst(receiver, inst, _) => {
            tys.push(receiver);
            tys.extend(inst.args(db).iter().copied());
        }
        res => {
            res.map_over_ty(|ty| {
                tys.push(ty);
                ty
            });
        }
    }
    tys
}

/// The applications of records and enums with conditions that a path
/// resolution carries, at any depth: `Bounded<0>` for the segment `Bounded<0>`
/// of `Bounded<0>::Out`, or for the receiver of `Bounded<0>::L`. Path
/// resolution reports every segment it resolves, so the lowering of a written
/// type and the inference of a body record these for every prefix of every
/// path they resolve, and the requirement checks read them from there. This
/// reads only declarations, so inference can call it while it resolves.
pub(super) fn constrained_applications<'db>(
    db: &'db dyn HirAnalysisDb,
    res: &PathRes<'db>,
) -> Vec<TyId<'db>> {
    fn collect<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, found: &mut Vec<TyId<'db>>) {
        for &arg in ty.decompose_ty_app(db).1 {
            collect(db, arg, found);
        }
        if adt_declaration(db, ty).is_some_and(|declaration| {
            !declaration.where_clause(db).const_predicates(db).is_empty()
        }) && !found.contains(&ty)
        {
            found.push(ty);
        }
    }
    let mut found = Vec::new();
    for ty in path_res_tys(db, res.clone()) {
        collect(db, ty, &mut found);
    }
    found
}

/// The first unmet requirement of an application that a path passed through.
/// A partial application, such as `Bounded` in `Bounded::helper()`, is not an
/// instantiation: inference supplies its arguments, and the header it
/// instantiates is checked where the item is used (`check_entered_header`).
/// A partial application written as a type is checked as that type.
fn check_path_application<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    reported: &FxHashSet<TyId<'db>>,
) -> Option<TypeRequirementFailure<'db>> {
    let declaration = adt_declaration(db, ty)?;
    let arity = collect_generic_params(db, declaration.into())
        .params(db)
        .len();
    if ty.decompose_ty_app(db).1.len() != arity {
        return None;
    }
    check_type_requirements(db, ty, scope, reported)
}

/// The anonymous constants a body writes in expression and pattern
/// positions, with the type each position checks it against: array repeat
/// lengths, explicit const arguments of calls, method calls and function
/// values, and const arguments of the other path segments. Types written in
/// the body are covered by `check_declared_type_requirements`.
fn expression_const_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    typed: &TypedBody<'db>,
) -> Vec<(Body<'db>, TyId<'db>)> {
    let checked_const_ty = |ty: TyId<'db>| match ty.data(db) {
        TyData::ConstTy(const_ty) if !ty.has_invalid(db) => Some(const_ty.ty(db)),
        _ => None,
    };
    // The callable an expression calls or names, with the generic arguments
    // inference gave it. A call whose explicit arguments failed has no
    // checked callable, so its definition is found again from its path or
    // receiver, without arguments.
    let callee = |expr: ExprId| -> Option<(CallableDef<'db>, Vec<TyId<'db>>)> {
        if let Some(callable) = typed.callable_expr(expr) {
            return Some((callable.callable_def(), callable.generic_args().to_vec()));
        }
        let (base, generic_args) = typed.expr_ty(db, expr).decompose_ty_app(db);
        if let TyData::TyBase(TyBase::Func(definition)) = base.data(db) {
            return Some((*definition, generic_args.to_vec()));
        }
        let candidate_def = |candidate: MethodCandidate<'db>| match candidate {
            MethodCandidate::InherentMethod(method) => method.def,
            MethodCandidate::TraitMethod(method) | MethodCandidate::NeedsConfirmation(method) => {
                CallableDef::Func(method.method)
            }
        };
        let definition = match expr.data(db, body).borrowed().to_opt()? {
            Expr::Path(Partial::Present(path)) => {
                let minter = LoweringContext::deferred(HoleAnchor::TemplatePath {
                    path: *path,
                    scope: body.scope(),
                    assumptions: typed.assumptions(),
                });
                match resolve_path_with_minter(
                    db,
                    *path,
                    body.scope(),
                    typed.assumptions(),
                    true,
                    &minter,
                )
                .ok()?
                {
                    PathRes::Func(ty) => match ty.base_ty(db).data(db) {
                        TyData::TyBase(TyBase::Func(definition)) => *definition,
                        _ => return None,
                    },
                    PathRes::Method(_, candidate) => candidate_def(candidate),
                    PathRes::TraitMethod(_, method) => CallableDef::Func(method),
                    _ => return None,
                }
            }
            Expr::MethodCall(receiver, Partial::Present(name), _, _) => {
                let receiver_ty = typed.expr_ty(db, *receiver);
                if receiver_ty.has_invalid(db) {
                    return None;
                }
                candidate_def(
                    select_method_candidate(
                        db,
                        &Canonicalized::new(db, receiver_ty),
                        *name,
                        body.scope(),
                        typed.assumptions(),
                        None,
                    )
                    .ok()?,
                )
            }
            _ => return None,
        };
        Some((definition, Vec::new()))
    };
    // Explicit arguments of a callable's own parameters, which the type
    // checker applies after the path resolves. Each is checked against its
    // parameter's type: the one its checked argument carries, or, for an
    // argument that failed, the parameter's declared type.
    let explicit_args = |expr: ExprId, args: GenericArgListId<'db>| {
        let Some((definition, generic_args)) = callee(expr) else {
            return Vec::new();
        };
        let offset = definition.offset_to_explicit_params_position(db);
        let expected = |index: usize| {
            generic_args
                .get(index)
                .and_then(|&arg| checked_const_ty(arg))
                .or_else(|| {
                    let CallableDef::Func(func) = definition else {
                        return None;
                    };
                    collect_generic_params(db, func.into())
                        .params(db)
                        .get(index)?
                        .const_ty_ty(db)
                })
        };
        args.data(db)
            .iter()
            .enumerate()
            .filter_map(|(idx, arg)| {
                let GenericArg::Const(arg) = arg else {
                    return None;
                };
                let ConstGenericArgValue::Expr(Partial::Present(body)) = arg.value else {
                    return None;
                };
                Some((body, expected(offset + idx)?))
            })
            .collect::<Vec<_>>()
    };
    // Arguments of the path's segments, which path resolution applies, each
    // checked against its segment's own resolution (`segment_const_args`).
    let segment_args = |path: PathId<'db>, value: bool| {
        if path_const_bodies(db, path).is_empty() {
            return Vec::new();
        }
        let minter = LoweringContext::deferred(HoleAnchor::TemplatePath {
            path,
            scope: body.scope(),
            assumptions: typed.assumptions(),
        })
        .recording_resolutions();
        let resolve = |value| {
            resolve_path_with_minter(db, path, body.scope(), typed.assumptions(), value, &minter)
        };
        // A tail that is not a value can still be a type, as a record's is.
        if resolve(value).is_err() {
            let _ = resolve(!value);
        }
        segment_const_args(db, path, &minter.into_resolutions())
    };

    let mut found: Vec<(Body<'db>, TyId<'db>)> = Vec::new();
    for (expr, data) in body.exprs(db).iter() {
        let entries = match data.borrowed().to_opt() {
            Some(Expr::ArrayRep(_, len)) => {
                len.to_opt().zip(array_length_ty(db)).into_iter().collect()
            }
            Some(Expr::Path(Partial::Present(path))) => {
                let mut entries = segment_args(*path, true);
                entries.extend(explicit_args(expr, path.generic_args(db)));
                entries
            }
            Some(Expr::RecordInit(Partial::Present(path), _)) => segment_args(*path, false),
            Some(Expr::MethodCall(_, _, args, _)) => explicit_args(expr, *args),
            _ => Vec::new(),
        };
        found.extend(entries);
    }
    for pat in body.pats(db).values() {
        if let Some(
            Pat::Path(Partial::Present(path), _)
            | Pat::PathTuple(Partial::Present(path), _)
            | Pat::Record(Partial::Present(path), _),
        ) = pat.borrowed().to_opt()
        {
            found.extend(segment_args(*path, true));
        }
    }
    // A body appears in one position, but two lookups can both find it.
    let mut seen = FxHashSet::default();
    found.retain(|(body, _)| seen.insert(*body));
    found
}

/// The types an impl's header instantiates: its self type and, for a trait
/// impl, the trait's arguments, in the impl's declaration coordinates.
fn header_types<'db>(db: &'db dyn HirAnalysisDb, item: ItemKind<'db>) -> Vec<TyId<'db>> {
    let mut tys: Vec<_> = impl_self_ty(db, item).into_iter().collect();
    if let ItemKind::ImplTrait(impl_trait) = item
        && let Some(inst) = impl_trait.trait_inst(db)
    {
        tys.extend(inst.args(db).iter().copied());
    }
    tys
}

/// The self type of an impl or trait impl, in its declaration coordinates.
fn impl_self_ty<'db>(db: &'db dyn HirAnalysisDb, item: ItemKind<'db>) -> Option<TyId<'db>> {
    match item {
        ItemKind::Impl(impl_) => Some(impl_.ty(db)),
        ItemKind::ImplTrait(impl_trait) => Some(impl_trait.ty(db)),
        _ => None,
    }
}

/// The header types of `func`'s parent in its declaration coordinates: an
/// impl's header, or a trait's `Self` and parameters.
fn declared_callee_headers<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> Vec<TyId<'db>> {
    match func.scope().parent_item(db) {
        Some(ItemKind::Trait(trait_)) => trait_.params(db).to_vec(),
        Some(item) => header_types(db, item),
        None => Vec::new(),
    }
}

/// The header types a use of `func` with `args` instantiates: its impl's
/// header, or a trait method's `Self` and trait arguments. A header that
/// cannot be instantiated with `args` is an error, since dropping it would
/// skip the conditions it carries.
fn callee_headers<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    args: &[TyId<'db>],
) -> Result<Vec<TyId<'db>>, SubstError<'db>> {
    let headers = declared_callee_headers(db, func);
    if headers.is_empty() {
        return Ok(headers);
    }
    let subst = CompleteSubst::for_owner(db, func.into(), args.to_vec())?;
    substitute_complete(db, headers, &subst)
}

/// The header types an associated constant path instantiates.
fn const_ref_headers<'db>(
    db: &'db dyn HirAnalysisDb,
    typed: &TypedBody<'db>,
    expr: ExprId,
) -> Option<Vec<TyId<'db>>> {
    match typed.expr_const_ref(expr)? {
        ConstRef::Const(_) => None,
        ConstRef::TraitConst(reference) => Some(reference.inst().args(db).to_vec()),
        ConstRef::InherentConst(reference) => Some(vec![reference.receiver_ty()]),
    }
}

/// A use of an associated item enters the header types it instantiates, such
/// as `Bounded<0>` in `Bounded<0>::helper()`, which no written type or checked
/// expression may carry. A header type that a checked receiver, argument,
/// result, or called function value already carries was checked where that
/// value entered; any other is checked here.
fn check_entered_header<'db>(
    db: &'db dyn HirAnalysisDb,
    typed: &TypedBody<'db>,
    expr: ExprId,
    headers: Vec<TyId<'db>>,
    scope: ScopeId<'db>,
    written: &FxHashSet<TyId<'db>>,
) -> Option<TypeRequirementFailure<'db>> {
    let body = typed.body()?;
    let mut carried = vec![expr];
    match expr.data(db, body).borrowed().to_opt() {
        Some(Expr::Call(callee, call_args)) => {
            if typed.expr_binding(*callee).is_some() {
                carried.push(*callee);
            }
            carried.extend(call_args.iter().map(|arg| arg.expr));
        }
        Some(Expr::MethodCall(receiver, _, _, call_args)) => {
            carried.push(*receiver);
            carried.extend(call_args.iter().map(|arg| arg.expr));
        }
        _ => {}
    }
    // A function value's own type is not checked (see `check_body_requirements`),
    // so it carries its header types only once bound and used again.
    let carried: Vec<_> = carried
        .into_iter()
        .map(|carrier| (carrier, typed.expr_ty(db, carrier)))
        .filter(|&(carrier, ty)| carrier != expr || !is_function_ty(db, ty))
        .map(|(_, ty)| ty)
        .collect();
    headers
        .into_iter()
        .filter(|&header| !carried.iter().any(|&ty| ty_mentions(db, ty, header)))
        .find_map(|header| check_type_requirements(db, header, scope, written))
}

/// Whether `needle` occurs in `ty`.
fn ty_mentions<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, needle: TyId<'db>) -> bool {
    ty == needle
        || ty
            .decompose_ty_app(db)
            .1
            .iter()
            .any(|&arg| ty_mentions(db, arg, needle))
}

/// An unmet requirement of a type application.
pub(super) struct TypeRequirementFailure<'db> {
    /// The failing application.
    ty: TyId<'db>,
    predicate: Body<'db>,
    failure: RequirementFailure,
}

/// Returns the first unmet requirement in `ty`. Failures of the
/// applications in `reported` were already reported where those types
/// entered, so the search continues past them.
fn check_type_requirements<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    reported: &FxHashSet<TyId<'db>>,
) -> Option<TypeRequirementFailure<'db>> {
    let (base, args) = ty.decompose_ty_app(db);
    for &arg in args {
        if let Some(failure) = check_type_requirements(db, arg, scope, reported) {
            return Some(failure);
        }
    }
    if reported.contains(&ty) {
        return None;
    }
    let TyData::TyBase(TyBase::Adt(adt)) = base.data(db) else {
        return None;
    };
    use crate::analysis::ty::adt_def::AdtRef;
    let (declaration, generic_owner, partially_applied) = match adt.adt_ref(db) {
        AdtRef::Struct(record) => (
            WhereClauseOwner::Struct(record),
            GenericParamOwner::Struct(record),
            RequirementFailure::PartiallyAppliedRecord,
        ),
        AdtRef::Enum(enum_) => (
            WhereClauseOwner::Enum(enum_),
            GenericParamOwner::Enum(enum_),
            RequirementFailure::PartiallyAppliedEnum,
        ),
    };
    let predicates = declaration.where_clause(db).const_predicates(db);
    if predicates.is_empty() {
        return None;
    }
    if args.len() != collect_generic_params(db, generic_owner).params(db).len() {
        return Some(TypeRequirementFailure {
            ty,
            predicate: predicates[0],
            failure: partially_applied,
        });
    }
    if args.iter().any(|arg| arg.has_var(db)) {
        return None;
    }
    let caller = premise_owner_in_scope(db, scope);
    predicates
        .iter()
        .filter(|&&predicate| checked_at_uses(db, predicate))
        .find_map(|&predicate| {
            match discharge_requirement(db, declaration, predicate, args.to_vec(), caller) {
                Discharge::Holds => None,
                Discharge::Fails(failure) => Some(TypeRequirementFailure {
                    ty,
                    predicate,
                    failure,
                }),
            }
        })
}

#[salsa::tracked(cycle_initial=requirement_cycle_initial, cycle_fn=requirement_cycle_recover)]
fn discharge_requirement<'db>(
    db: &'db dyn HirAnalysisDb,
    declaration: WhereClauseOwner<'db>,
    predicate: Body<'db>,
    args: Vec<TyId<'db>>,
    caller: Option<GenericParamOwner<'db>>,
) -> Discharge {
    let typed = match &check_predicate_formation(db, predicate).status {
        FormationStatus::WellFormed(typed) => typed,
        FormationStatus::IllFormed => {
            return Discharge::Fails(RequirementFailure::NotEstablished);
        }
        FormationStatus::Recursive => return Discharge::Fails(RequirementFailure::Recursive),
    };
    let not_instantiable = Discharge::Fails(RequirementFailure::NotInstantiable);
    let args = match caller {
        Some(caller) => match caller_args(db, caller, args) {
            Ok(args) => args,
            Err(_) => return not_instantiable,
        },
        None => args,
    };
    let Ok(subst) = requirement_subst(db, declaration, &args) else {
        return not_instantiable;
    };
    // The use leaves the predicate generic when an argument for one of the
    // parameters it mentions still mentions a parameter.
    let mut symbolic = false;
    for &param in predicate_params(db, predicate) {
        let Ok(arg) = substitute_complete(db, param, &subst) else {
            return not_instantiable;
        };
        symbolic |= arg.has_param(db);
    }
    if symbolic {
        let Ok(key) = predicate_key(db, predicate, typed, &subst) else {
            return not_instantiable;
        };
        if let (Some(key), Some(caller)) = (&key, caller) {
            let Ok(premises) = caller_premises(db, caller) else {
                return not_instantiable;
            };
            for (premise, premise_subst) in premises {
                let Some(premise_typed) = check_predicate_formation(db, premise).well_formed()
                else {
                    continue;
                };
                let Ok(premise_key) = predicate_key(db, premise, premise_typed, &premise_subst)
                else {
                    return not_instantiable;
                };
                if premise_key.as_ref() == Some(key) {
                    return Discharge::Holds;
                }
            }
        }
        return Discharge::Fails(if key.is_some() {
            RequirementFailure::NoMatchingPremise
        } else {
            RequirementFailure::NotForwardable
        });
    }
    let owner = BodyOwner::const_predicate(db, predicate);
    match condition_outcome(db, owner, GenericSubst::for_body_owner(db, owner, args)) {
        ConditionOutcome::True => Discharge::Holds,
        ConditionOutcome::False => Discharge::Fails(RequirementFailure::False),
        ConditionOutcome::NotBool | ConditionOutcome::Blocked(_) => {
            Discharge::Fails(RequirementFailure::NotEstablished)
        }
        ConditionOutcome::Failed(cause) => {
            Discharge::Fails(RequirementFailure::from_evaluation(&cause))
        }
    }
}

// A requirement reached again while it is being discharged depends on itself.
fn requirement_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _declaration: WhereClauseOwner<'db>,
    _predicate: Body<'db>,
    _args: Vec<TyId<'db>>,
    _caller: Option<GenericParamOwner<'db>>,
) -> Discharge {
    Discharge::Fails(RequirementFailure::Recursive)
}

fn requirement_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Discharge,
    _count: u32,
    _declaration: WhereClauseOwner<'db>,
    _predicate: Body<'db>,
    _args: Vec<TyId<'db>>,
    _caller: Option<GenericParamOwner<'db>>,
) -> salsa::CycleRecoveryAction<Discharge> {
    salsa::CycleRecoveryAction::Iterate
}

// A predicate reached again while its formation is checked depends on
// itself.
fn formation_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> PredicateFormation<'db> {
    PredicateFormation {
        diags: vec![BodyDiag::RecursiveConstRequirement(body.span().into()).into()],
        status: FormationStatus::Recursive,
    }
}
fn formation_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &PredicateFormation<'db>,
    _count: u32,
    _body: Body<'db>,
) -> salsa::CycleRecoveryAction<PredicateFormation<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

/// The assumptions a type written at `scope` is lowered with.
fn assumptions_at<'db>(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> PredicateListId<'db> {
    let mut enclosing = scope;
    while matches!(enclosing.item(), ItemKind::Body(_)) {
        let Some(parent) = enclosing.parent(db) else {
            break;
        };
        enclosing = parent;
    }
    crate::semantic::constraints_for(db, enclosing.item())
}

/// The applications that the types written in `owner`'s signature and body
/// carry, as `written_types` lists them for `written_type_check`. That check
/// reports a written type's unmet requirement where the type is written, so a
/// path or expression that carries the same application does not report it
/// again.
fn written_applications<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> FxHashSet<TyId<'db>> {
    fn examined<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, found: &mut FxHashSet<TyId<'db>>) {
        if found.insert(ty) {
            for &arg in ty.decompose_ty_app(db).1 {
                examined(db, arg, found);
            }
        }
    }
    let signature = match owner {
        BodyOwner::Func(func) => Some(ItemKind::Func(func)),
        BodyOwner::Const(const_) => Some(ItemKind::Const(const_)),
        _ => None,
    };
    let mut found = FxHashSet::default();
    for item in signature
        .into_iter()
        .chain(owner.body(db).map(ItemKind::Body))
    {
        for entry in written_types(db, item) {
            if let WrittenEntry::Type(written) = entry {
                for &ty in written.lowered.iter().chain(&written.applications) {
                    examined(db, ty, &mut found);
                }
            }
        }
    }
    found
}

/// Check every authored type position, including unused defaults and aliases,
/// and the anonymous constants written directly in types and trait
/// references. Each item and body of the module is checked on its own
/// (`written_type_check`). Inferred expression types are checked separately
/// after body inference.
pub(crate) fn check_declared_type_requirements<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: crate::hir_def::TopLevelMod<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    top_mod
        .all_items(db)
        .iter()
        .flat_map(|&item| written_type_check(db, item).iter().cloned())
        .collect()
}

/// A position in an item or body that `written_type_check` checks, in the
/// order it checks them.
#[derive(Debug, Clone, PartialEq, Eq, Update)]
enum WrittenEntry<'db> {
    /// Anonymous constants written in a type or trait reference, each with
    /// the type its position checks it against.
    ConstBodies(Vec<(Body<'db>, TyId<'db>)>),
    /// A written type, listed after the types nested in it.
    Type(WrittenType<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
struct WrittenType<'db> {
    span: DynLazySpan<'db>,
    scope: ScopeId<'db>,
    /// Its lowering, unless that is invalid.
    lowered: Option<TyId<'db>>,
    /// The constrained applications its paths passed through.
    applications: Vec<TyId<'db>>,
    /// The index of the first entry nested in this type.
    nested_from: usize,
}

#[salsa::interned]
struct WrittenTypeRegion<'db> {
    item: ItemKind<'db>,
}

/// The types written in `item` itself, and the anonymous constants written
/// in them. An item or body nested in it is a region of its own, since
/// `TopLevelMod::all_items` lists every item and body of a module, including
/// the default of a const parameter. Listing them only lowers types, so a
/// body's requirement check can read the list while a requirement is being
/// discharged; `written_type_check` discharges.
fn written_types<'db>(db: &'db dyn HirAnalysisDb, item: ItemKind<'db>) -> &'db [WrittenEntry<'db>] {
    written_types_query(db, WrittenTypeRegion::new(db, item))
}

#[salsa::tracked(return_ref)]
fn written_types_query<'db>(
    db: &'db dyn HirAnalysisDb,
    region: WrittenTypeRegion<'db>,
) -> Vec<WrittenEntry<'db>> {
    use crate::hir_def::{TraitRefId, TypeKind};
    use crate::span::types::LazyTySpan;
    use crate::visitor::{
        Visitor, VisitorCtxt,
        prelude::{LazyBodySpan, LazyExprSpan, LazyItemSpan, LazyTraitRefSpan},
        walk_body, walk_expr, walk_item, walk_path, walk_trait_ref, walk_type,
    };
    struct Collector<'db> {
        db: &'db dyn HirAnalysisDb,
        root: ItemKind<'db>,
        entries: Vec<WrittenEntry<'db>>,
        /// Type parameter defaults, and how deep the walk is inside one.
        /// `check_generic_default_bodies` owns the anonymous constants there,
        /// and finds them with the same rule (`written_type_const_args`).
        defaults: FxHashSet<crate::hir_def::TypeId<'db>>,
        default_depth: usize,
    }
    impl<'db> Visitor<'db> for Collector<'db> {
        // Nested items and bodies are their own regions.
        fn visit_item(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyItemSpan<'db>>,
            item: ItemKind<'db>,
        ) {
            if item == self.root {
                walk_item(self, ctxt, item);
            }
        }

        fn visit_body(&mut self, ctxt: &mut VisitorCtxt<'db, LazyBodySpan<'db>>, body: Body<'db>) {
            if ItemKind::Body(body) == self.root {
                walk_body(self, ctxt, body);
            }
        }

        fn visit_generic_param(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, crate::visitor::prelude::LazyGenericParamSpan<'db>>,
            param: &crate::hir_def::GenericParam<'db>,
        ) {
            if let crate::hir_def::GenericParam::Type(param) = param
                && let Some(default) = param.default_ty
            {
                self.defaults.insert(default);
            }
            crate::visitor::walk_generic_param(self, ctxt, param);
        }

        fn visit_trait_ref(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyTraitRefSpan<'db>>,
            trait_ref: TraitRefId<'db>,
        ) {
            if let Some(path) = trait_ref.path(self.db).to_opt()
                && self.default_depth == 0
            {
                let scope = ctxt.scope();
                let bodies =
                    trait_ref_const_args(self.db, path, scope, assumptions_at(self.db, scope));
                if !bodies.is_empty() {
                    self.entries.push(WrittenEntry::ConstBodies(bodies));
                }
            }
            walk_trait_ref(self, ctxt, trait_ref);
        }

        fn visit_ty(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyTySpan<'db>>,
            hir_ty: crate::hir_def::TypeId<'db>,
        ) {
            let scope = ctxt.scope();
            let span = ctxt.span().map(Into::into);
            self.written_type(hir_ty, scope, span, |this| walk_type(this, ctxt, hir_ty));
        }

        // A `with` key is a type written in the body, whose paths are
        // resolved like any other written type's.
        fn visit_expr(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
            expr: ExprId,
            expr_data: &Expr<'db>,
        ) {
            if let Expr::With(bindings, _) = expr_data {
                for (idx, binding) in bindings.iter().enumerate() {
                    let Some(Partial::Present(key)) = binding.key_path else {
                        continue;
                    };
                    let hir_ty =
                        crate::hir_def::TypeId::new(self.db, TypeKind::Path(Partial::Present(key)));
                    let span = LazyExprSpan::new(ctxt.body(), expr)
                        .into_with_expr()
                        .params()
                        .param(idx)
                        .path();
                    let mut key_ctxt = VisitorCtxt::new(self.db, ctxt.scope(), span.clone());
                    self.written_type(hir_ty, ctxt.scope(), Some(span.into()), |this| {
                        walk_path(this, &mut key_ctxt, key)
                    });
                }
            }
            walk_expr(self, ctxt, expr);
        }
    }
    impl<'db> Collector<'db> {
        /// Lists a written type at `span`: the anonymous constants written in
        /// it, then the types nested in it, which `walk_nested` visits, then
        /// the type itself.
        fn written_type(
            &mut self,
            hir_ty: crate::hir_def::TypeId<'db>,
            scope: ScopeId<'db>,
            span: Option<DynLazySpan<'db>>,
            walk_nested: impl FnOnce(&mut Self),
        ) {
            let db = self.db;
            let assumptions = assumptions_at(db, scope);
            let (ty, applications) = match hir_ty.data(db) {
                // Only a path of two or more segments passes through types
                // that its lowering does not hold. A one-segment path lowers
                // to what it resolves to, its arguments are written types or
                // anonymous constants of their own, and an alias's arguments
                // are checked after expansion or as constants
                // (`written_const_args`).
                TypeKind::Path(Partial::Present(path)) if path.len(db) > 1 => {
                    let (ty, resolutions) = lower_hir_ty_with_resolutions(
                        db,
                        hir_ty,
                        scope,
                        assumptions,
                        ConstBodyLowering::Eager,
                    );
                    let applications = resolutions
                        .iter()
                        .flat_map(|(_, res)| constrained_applications(db, res))
                        .collect();
                    (ty, applications)
                }
                _ => (lower_hir_ty(db, hir_ty, scope, assumptions), Vec::new()),
            };
            let in_default = self.defaults.contains(&hir_ty);
            self.default_depth += usize::from(in_default);
            if self.default_depth == 0 {
                let bodies = written_const_args(db, hir_ty, scope, assumptions);
                if !bodies.is_empty() {
                    self.entries.push(WrittenEntry::ConstBodies(bodies));
                }
            }
            let nested_from = self.entries.len();
            walk_nested(self);
            if let Some(span) = span {
                self.entries.push(WrittenEntry::Type(WrittenType {
                    span,
                    scope,
                    lowered: (!ty.has_invalid(db)).then_some(ty),
                    applications,
                    nested_from,
                }));
            }
            self.default_depth -= usize::from(in_default);
        }
    }
    let root = region.item(db);
    let mut collector = Collector {
        db,
        root,
        entries: Vec::new(),
        defaults: FxHashSet::default(),
        default_depth: 0,
    };
    collector.visit_item(&mut VisitorCtxt::with_item(db, root), root);
    collector.entries
}

/// The unmet requirements of the types and anonymous constants written in
/// `item` itself (`written_types`), each reported where it is written. A
/// type's nested types are checked first. A failure one of them reported
/// entered there, so the enclosing type does not repeat it; a failure
/// reachable only through an alias expansion, or through a type its paths
/// pass through, is reported at the enclosing type.
fn written_type_check<'db>(
    db: &'db dyn HirAnalysisDb,
    item: ItemKind<'db>,
) -> &'db Vec<FuncBodyDiag<'db>> {
    written_type_check_query(db, WrittenTypeRegion::new(db, item))
}

#[salsa::tracked(return_ref)]
fn written_type_check_query<'db>(
    db: &'db dyn HirAnalysisDb,
    region: WrittenTypeRegion<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let entries = written_types(db, region.item(db));
    let mut diags = Vec::new();
    // The failing application each entry reported, if any.
    let mut reported: Vec<Option<TyId<'db>>> = Vec::with_capacity(entries.len());
    for entry in entries {
        let report = match entry {
            WrittenEntry::ConstBodies(bodies) => {
                for &(body, expected) in bodies {
                    diags.extend(anon_const_position_check(db, body, expected).diags);
                }
                None
            }
            WrittenEntry::Type(written) => {
                let nested: FxHashSet<_> = reported[written.nested_from..]
                    .iter()
                    .flatten()
                    .copied()
                    .collect();
                let unmet = written
                    .lowered
                    .and_then(|ty| check_type_requirements(db, ty, written.scope, &nested))
                    .or_else(|| {
                        written.applications.iter().find_map(|&application| {
                            check_path_application(db, application, written.scope, &nested)
                        })
                    });
                unmet.map(|unmet| {
                    diags.push(unmet_requirement_diag(
                        written.span.clone(),
                        unmet.predicate,
                        unmet.failure,
                        None,
                    ));
                    unmet.ty
                })
            }
        };
        reported.push(report);
    }
    diags
}

/// The anonymous constants written in `hir_ty` and in every type and trait
/// reference nested in it, each with the type its position checks it
/// against. This is the rule `written_types` applies to each type and trait
/// reference it lists (`written_const_args`, `trait_ref_const_args`), for a
/// type parameter's default, whose declaration check owns its constants
/// (`check_generic_default_bodies`). A default's lowering does not hold every
/// constant written in it: an alias can drop one, and a qualifier such as
/// `Holder<{ n }>` in `Holder<{ n }>::Out` may only select an impl.
pub(super) fn written_type_const_args<'db>(
    db: &'db dyn HirAnalysisDb,
    hir_ty: crate::hir_def::TypeId<'db>,
    span: crate::span::types::LazyTySpan<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<(Body<'db>, TyId<'db>)> {
    use crate::hir_def::TraitRefId;
    use crate::span::types::LazyTySpan;
    use crate::visitor::{
        Visitor, VisitorCtxt,
        prelude::{LazyBodySpan, LazyTraitRefSpan},
        walk_trait_ref, walk_type,
    };
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        assumptions: PredicateListId<'db>,
        found: Vec<(Body<'db>, TyId<'db>)>,
    }
    impl<'db> Visitor<'db> for Finder<'db> {
        fn visit_ty(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyTySpan<'db>>,
            hir_ty: crate::hir_def::TypeId<'db>,
        ) {
            self.found.extend(written_const_args(
                self.db,
                hir_ty,
                ctxt.scope(),
                self.assumptions,
            ));
            walk_type(self, ctxt, hir_ty);
        }

        fn visit_trait_ref(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyTraitRefSpan<'db>>,
            trait_ref: TraitRefId<'db>,
        ) {
            if let Some(path) = trait_ref.path(self.db).to_opt() {
                self.found.extend(trait_ref_const_args(
                    self.db,
                    path,
                    ctxt.scope(),
                    self.assumptions,
                ));
            }
            walk_trait_ref(self, ctxt, trait_ref);
        }

        // An anonymous constant's own body is checked as that constant.
        fn visit_body(&mut self, _: &mut VisitorCtxt<'db, LazyBodySpan<'db>>, _: Body<'db>) {}
    }
    let mut finder = Finder {
        db,
        assumptions,
        found: Vec::new(),
    };
    finder.visit_ty(&mut VisitorCtxt::new(db, scope, span), hir_ty);
    finder.found
}

/// The anonymous constants passed to the segments of a trait reference's
/// path, such as `{ n }` in `Tr<{ n }>`, each with the type of the parameter
/// it is passed to (`segment_const_args`).
fn trait_ref_const_args<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<(Body<'db>, TyId<'db>)> {
    if path_const_bodies(db, path).is_empty() {
        return Vec::new();
    }
    let minter = LoweringContext::deferred(HoleAnchor::TemplatePath {
        path,
        scope,
        assumptions,
    })
    .recording_resolutions();
    let _ = resolve_path_with_minter(db, path, scope, assumptions, false, &minter);
    segment_const_args(db, path, &minter.into_resolutions())
}

/// The anonymous constants written directly in `hir_ty`, each with the type
/// its position checks it against: an array's length, or the arguments of its
/// path's segments (`segment_const_args`).
fn written_const_args<'db>(
    db: &'db dyn HirAnalysisDb,
    hir_ty: crate::hir_def::TypeId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<(Body<'db>, TyId<'db>)> {
    use crate::hir_def::TypeKind;
    match hir_ty.data(db) {
        TypeKind::Array(_, Partial::Present(len)) => array_length_ty(db)
            .map(|expected| (*len, expected))
            .into_iter()
            .collect(),
        TypeKind::Path(Partial::Present(path)) if !path_const_bodies(db, *path).is_empty() => {
            let (_, resolutions) = lower_hir_ty_with_resolutions(
                db,
                hir_ty,
                scope,
                assumptions,
                ConstBodyLowering::Deferred,
            );
            segment_const_args(db, *path, &resolutions)
        }
        _ => Vec::new(),
    }
}
