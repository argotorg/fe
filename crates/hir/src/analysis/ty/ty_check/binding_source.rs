use super::{BindingSource, LocalBinding, ReturnProjectionStep};
use crate::{
    analysis::{
        HirAnalysisDb,
        ty::pattern_ir::{ConstructorKind, PatternStore, ValidatedPatId, ValidatedPatKind},
    },
    hir_def::{Body, Expr, Partial, PatId, Stmt},
};

/// Read the existing checked pattern projections through either an in-progress
/// checker view or a completed typed body. No body query is required here.
pub(super) fn binding_source<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    store: &PatternStore<'db>,
    root: &impl Fn(PatId) -> Option<ValidatedPatId>,
    binding: LocalBinding<'db>,
) -> Option<BindingSource> {
    let LocalBinding::Local { pat, .. } = binding else {
        return None;
    };
    for (_, stmt) in body.stmts(db).iter() {
        if let Partial::Present(Stmt::Let(stmt_pat, _, Some(init_expr))) = stmt
            && let Some(root) = root(*stmt_pat)
            && let Some(projection) = binding_projection(store, root, pat)
        {
            return Some(BindingSource {
                init_expr: *init_expr,
                projection,
            });
        }
    }
    for (_, expr) in body.exprs(db).iter() {
        if let Partial::Present(Expr::Match(scrutinee, Partial::Present(arms))) = expr {
            for arm in arms {
                if let Some(root) = root(arm.pat)
                    && let Some(projection) = binding_projection(store, root, pat)
                {
                    return Some(BindingSource {
                        init_expr: *scrutinee,
                        projection,
                    });
                }
            }
        }
    }
    None
}

fn binding_projection<'db>(
    store: &PatternStore<'db>,
    pat: ValidatedPatId,
    binding_pat: PatId,
) -> Option<Vec<ReturnProjectionStep>> {
    match store.node(pat).kind() {
        ValidatedPatKind::Wildcard { binding } => binding
            .filter(|binding| binding.representative_pat == binding_pat)
            .map(|_| Vec::new()),
        ValidatedPatKind::Constructor { ctor, fields } => {
            fields.iter().enumerate().find_map(|(field_idx, child)| {
                let suffix = binding_projection(store, *child, binding_pat)?;
                let field = u16::try_from(field_idx).ok()?;
                let step = match ctor {
                    ConstructorKind::Variant(variant, _) => ReturnProjectionStep::VariantField {
                        variant: variant.idx,
                        field,
                    },
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
                .filter_map(|pat| binding_projection(store, *pat, binding_pat))
                .collect::<Vec<_>>();
            let first = paths.pop()?;
            paths.into_iter().all(|path| path == first).then_some(first)
        }
    }
}
