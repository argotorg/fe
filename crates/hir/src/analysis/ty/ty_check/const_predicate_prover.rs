use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::ty_def::{TyData, TyId};
use crate::hir_def::{Body, Expr, ExprId, IdentId, LitKind, Partial, PathId, path::PathKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum EarlyProofResult {
    Proven,
    Unknown,
}

pub(super) fn try_prove_const_predicate<'db>(
    db: &'db dyn HirAnalysisDb,
    requirement: Body<'db>,
    assumptions: &[Body<'db>],
    generic_args: &[TyId<'db>],
    callee_param_names: &[Option<IdentId<'db>>],
) -> EarlyProofResult {
    let param_map = ParamMap::build(db, generic_args, callee_param_names);

    for assumption in assumptions {
        if syntactic_identity_proves(db, *assumption, requirement, &param_map) {
            return EarlyProofResult::Proven;
        }
    }

    EarlyProofResult::Unknown
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
