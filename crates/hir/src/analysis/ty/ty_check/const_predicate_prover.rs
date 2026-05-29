use crate::analysis::HirAnalysisDb;
use crate::analysis::semantic::{SemConstScalar, SemConstValue, eval_body_owner_const};
use crate::analysis::ty::{
    constraint::{
        ConstPredicateInstId, ConstProofId, ConstraintId, ConstraintKind, EvidenceId, EvidenceKind,
        ParamEnv,
    },
    ty_def::{TyData, TyFlags, TyId},
    ty_lower::collect_generic_params,
    visitor::collect_flags,
};
use crate::hir_def::{
    Body, Expr, ExprId, GenericParamOwner, IdentId, LitKind, Partial, PathId, WhereClauseOwner,
    path::PathKind,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ConstProveResult<'db> {
    Proven(EvidenceId<'db>),
    Disproved,
    Ambiguous,
    Error,
}

pub(crate) fn prove_const_predicate<'db>(
    db: &'db dyn HirAnalysisDb,
    pred: ConstPredicateInstId<'db>,
    env: ParamEnv<'db>,
) -> ConstProveResult<'db> {
    let flags = collect_flags(db, pred);
    if flags.contains(TyFlags::HAS_INVALID) {
        return ConstProveResult::Error;
    }

    if flags.contains(TyFlags::HAS_PARAM) {
        if let Some(evidence) = assumption_evidence(db, pred, env) {
            return ConstProveResult::Proven(evidence);
        }
        return ConstProveResult::Ambiguous;
    }

    let owner = super::BodyOwner::AnonConstBody {
        body: pred.body(db),
        expected: TyId::bool(db),
    };
    match eval_body_owner_const(db, owner, pred.args(db).to_vec()) {
        Ok(value) => match value.value(db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Bool(true),
                ..
            } => {
                let proof = ConstProofId { predicate: pred };
                ConstProveResult::Proven(EvidenceId::new(db, EvidenceKind::ConstProof(proof)))
            }
            SemConstValue::Scalar {
                value: SemConstScalar::Bool(false),
                ..
            } => ConstProveResult::Disproved,
            _ => ConstProveResult::Error,
        },
        Err(_) => ConstProveResult::Error,
    }
}

fn assumption_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    pred: ConstPredicateInstId<'db>,
    env: ParamEnv<'db>,
) -> Option<EvidenceId<'db>> {
    for assumption in env.const_assumptions(db) {
        if assumption == pred || assumption_matches_requirement(db, assumption, pred) {
            let constraint = ConstraintId::new(db, ConstraintKind::ConstPredicate(assumption));
            return Some(EvidenceId::new(db, EvidenceKind::Assumption(constraint)));
        }
    }

    None
}

fn assumption_matches_requirement<'db>(
    db: &'db dyn HirAnalysisDb,
    assumption: ConstPredicateInstId<'db>,
    requirement: ConstPredicateInstId<'db>,
) -> bool {
    if assumption.predicate(db) == requirement.predicate(db) {
        return assumption.args(db) == requirement.args(db);
    }

    let requirement_param_names = predicate_param_names(db, requirement);
    let param_map = ParamMap::build(db, requirement.args(db), &requirement_param_names);
    syntactic_identity_proves(db, assumption.body(db), requirement.body(db), &param_map)
}

fn predicate_param_names<'db>(
    db: &'db dyn HirAnalysisDb,
    pred: ConstPredicateInstId<'db>,
) -> Vec<Option<IdentId<'db>>> {
    let owner = match pred.predicate(db).owner {
        WhereClauseOwner::Func(func) => GenericParamOwner::Func(func),
        WhereClauseOwner::Struct(struct_) => GenericParamOwner::Struct(struct_),
        WhereClauseOwner::Enum(enum_) => GenericParamOwner::Enum(enum_),
        WhereClauseOwner::Impl(impl_) => GenericParamOwner::Impl(impl_),
        WhereClauseOwner::Trait(trait_) => GenericParamOwner::Trait(trait_),
        WhereClauseOwner::ImplTrait(impl_trait) => GenericParamOwner::ImplTrait(impl_trait),
    };

    collect_generic_params(db, owner)
        .params(db)
        .iter()
        .map(|ty| match ty.base_ty(db).data(db) {
            TyData::TyParam(param) => Some(param.name),
            _ => None,
        })
        .collect()
}

struct ParamMap<'db> {
    mappings: Vec<(IdentId<'db>, IdentId<'db>)>,
}

impl<'db> ParamMap<'db> {
    fn build(
        db: &'db dyn HirAnalysisDb,
        generic_args: &[TyId<'db>],
        callee_param_names: &[Option<IdentId<'db>>],
    ) -> Self {
        let mut mappings = Vec::new();
        for (i, ty) in generic_args.iter().enumerate() {
            if let TyData::TyParam(param) = ty.base_ty(db).data(db)
                && let Some(callee_name) = callee_param_names.get(i).copied().flatten()
                && callee_name != param.name
            {
                mappings.push((callee_name, param.name));
            }
        }
        Self { mappings }
    }

    fn translate_ident(&self, callee_ident: IdentId<'db>) -> IdentId<'db> {
        for &(from, to) in &self.mappings {
            if from == callee_ident {
                return to;
            }
        }
        callee_ident
    }
}

fn syntactic_identity_proves<'db>(
    db: &'db dyn HirAnalysisDb,
    assumption: Body<'db>,
    requirement: Body<'db>,
    param_map: &ParamMap<'db>,
) -> bool {
    let assumption_root = assumption.expr(db);
    let requirement_root = requirement.expr(db);
    exprs_equivalent(
        db,
        assumption,
        assumption_root,
        requirement,
        requirement_root,
        param_map,
    )
}

fn exprs_equivalent<'db>(
    db: &'db dyn HirAnalysisDb,
    a_body: Body<'db>,
    a_expr: ExprId,
    b_body: Body<'db>,
    b_expr: ExprId,
    param_map: &ParamMap<'db>,
) -> bool {
    let a = a_expr.data(db, a_body);
    let b = b_expr.data(db, b_body);

    match (a, b) {
        (Partial::Present(a), Partial::Present(b)) => match (a, b) {
            (Expr::Lit(lit_a), Expr::Lit(lit_b)) => lits_equal(db, lit_a, lit_b),

            (Expr::Bin(al, ar, a_op), Expr::Bin(bl, br, b_op)) => {
                a_op == b_op
                    && exprs_equivalent(db, a_body, *al, b_body, *bl, param_map)
                    && exprs_equivalent(db, a_body, *ar, b_body, *br, param_map)
            }

            (Expr::Un(ae, a_op), Expr::Un(be, b_op)) => {
                a_op == b_op && exprs_equivalent(db, a_body, *ae, b_body, *be, param_map)
            }

            (Expr::Path(Partial::Present(a_path)), Expr::Path(Partial::Present(b_path))) => {
                paths_equivalent(db, *a_path, *b_path, param_map)
            }

            (Expr::Call(a_callee, a_args), Expr::Call(b_callee, b_args)) => {
                a_args.len() == b_args.len()
                    && exprs_equivalent(db, a_body, *a_callee, b_body, *b_callee, param_map)
                    && a_args.iter().zip(b_args.iter()).all(|(a_arg, b_arg)| {
                        a_arg.label == b_arg.label
                            && exprs_equivalent(
                                db, a_body, a_arg.expr, b_body, b_arg.expr, param_map,
                            )
                    })
            }

            _ => false,
        },
        _ => false,
    }
}

fn paths_equivalent<'db>(
    db: &'db dyn HirAnalysisDb,
    a: PathId<'db>,
    b: PathId<'db>,
    param_map: &ParamMap<'db>,
) -> bool {
    if a == b && param_map.mappings.is_empty() {
        return true;
    }

    match (a.kind(db).clone(), b.kind(db).clone()) {
        (
            PathKind::Ident {
                ident: Partial::Present(a_ident),
                generic_args: a_gargs,
            },
            PathKind::Ident {
                ident: Partial::Present(b_ident),
                generic_args: b_gargs,
            },
        ) => {
            let b_translated = param_map.translate_ident(b_ident);
            if a_ident != b_translated {
                return false;
            }
            if a_gargs != b_gargs {
                return false;
            }
            match (a.parent(db), b.parent(db)) {
                (None, None) => true,
                (Some(ap), Some(bp)) => paths_equivalent(db, ap, bp, param_map),
                _ => false,
            }
        }
        _ => false,
    }
}

fn lits_equal<'db>(db: &'db dyn HirAnalysisDb, a: &LitKind<'db>, b: &LitKind<'db>) -> bool {
    match (a, b) {
        (LitKind::Int(a_id), LitKind::Int(b_id)) => a_id.data(db) == b_id.data(db),
        (LitKind::Bool(a_val), LitKind::Bool(b_val)) => a_val == b_val,
        (LitKind::String(a_id), LitKind::String(b_id)) => a_id.data(db) == b_id.data(db),
        _ => false,
    }
}
