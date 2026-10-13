//! Shared interpretation of already checked compiler-known operand sources.
//! The views supply checked facts and the existing CTFE service supplies calls.
//! Neither view performs trait solving or constructs an unchecked proof.
use super::{SemConstId, SemConstValue};
use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            pattern_ir::{PatternStore, ValidatedPatId},
            ty_check::{
                BindingSource, BodyOwner, LocalBinding, RecordInitLowering, RecordLike,
                ReturnProjectionStep,
            },
            ty_def::TyId,
        },
    },
    hir_def::{Body, Expr, ExprId, Partial, PatId},
};

pub(crate) enum KnownOperandRecovery<'db> {
    Value(crate::analysis::semantic::SemConstId<'db>),
    NotKnown,
    Blocked {
        owner: BodyOwner<'db>,
        info: crate::analysis::semantic::BlockedInfo<'db>,
        result_ty: TyId<'db>,
    },
    Failed {
        owner: BodyOwner<'db>,
        failure: crate::analysis::semantic::EvalFailure<'db>,
        result_ty: TyId<'db>,
    },
}

pub(crate) trait OperandRecoveryCx<'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb;
    fn body(&self) -> Option<Body<'db>>;
    fn owner(&self) -> BodyOwner<'db>;
    fn opening(&self, operand: ExprId) -> Option<crate::analysis::ty::ty_check::TraitOpening<'db>>;
    fn expr_ty(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> TyId<'db>;
    fn binding(&self, expr: ExprId) -> Option<LocalBinding<'db>>;
    fn binding_source(
        &self,
        db: &'db dyn HirAnalysisDb,
        binding: LocalBinding<'db>,
    ) -> Option<BindingSource>;
    fn record_init_lowering(&self, expr: ExprId) -> Option<RecordInitLowering<'db>>;
    fn field_index(&self, expr: ExprId) -> Option<u16>;
    fn projected_evidence(
        &self,
        expr: ExprId,
    ) -> Option<crate::analysis::ty::trait_def::ImplApplicationRecord<'db>>;
    fn pattern_root(&self, pat: PatId) -> Option<ValidatedPatId>;
    fn pattern_store(&self) -> &PatternStore<'db>;
    fn callable(&self, expr: ExprId) -> Option<&crate::analysis::ty::ty_check::Callable<'db>>;
    fn leaf(
        &self,
        operand: ExprId,
        stop: &mut Option<KnownOperandRecovery<'db>>,
        active: &mut Vec<ExprId>,
    ) -> Option<SemConstId<'db>>;
}

pub(crate) fn recover_known_operand<'db>(
    cx: &impl OperandRecoveryCx<'db>,
    operand: ExprId,
) -> KnownOperandRecovery<'db> {
    let mut stop = None;
    let value = recover_value(cx, operand, &mut stop, &mut Vec::new());
    stop.unwrap_or_else(|| {
        value.map_or(KnownOperandRecovery::NotKnown, KnownOperandRecovery::Value)
    })
}

pub(crate) fn recover_value<'db>(
    cx: &impl OperandRecoveryCx<'db>,
    operand: ExprId,
    stop: &mut Option<KnownOperandRecovery<'db>>,
    active: &mut Vec<ExprId>,
) -> Option<SemConstId<'db>> {
    if active.contains(&operand) {
        return None;
    }
    active.push(operand);
    let value = recover_value_inner(cx, operand, stop, active);
    active.pop();
    value
}

fn recover_value_inner<'db>(
    cx: &impl OperandRecoveryCx<'db>,
    operand: ExprId,
    stop: &mut Option<KnownOperandRecovery<'db>>,
    active: &mut Vec<ExprId>,
) -> Option<SemConstId<'db>> {
    let db = cx.db();
    let ty = cx.expr_ty(db, operand);
    let body = cx.body()?;
    match operand.data(db, body) {
        Partial::Present(Expr::Field(base, _)) => {
            if let Some(record) = cx.projected_evidence(operand) {
                return super::issue_checked_evidence(
                    db,
                    record,
                    ty,
                    super::SemOrigin::Expr(operand),
                )
                .ok()
                .map(|value| value.value());
            }
            let value = recover_value(cx, *base, stop, active)?;
            return project_value(
                db,
                value,
                &[ReturnProjectionStep::Field(cx.field_index(operand)?)],
            );
        }
        Partial::Present(Expr::Bin(base, index, crate::hir_def::BinOp::Index)) => {
            if let Some(record) = cx.projected_evidence(operand) {
                return super::issue_checked_evidence(
                    db,
                    record,
                    ty,
                    super::SemOrigin::Expr(operand),
                )
                .ok()
                .map(|value| value.value());
            }
            let value = recover_value(cx, *base, stop, active)?;
            let index_value = recover_value(cx, *index, stop, active)?;
            let SemConstValue::Scalar {
                value: super::SemConstScalar::Int { value: index },
                ..
            } = index_value.value(db)
            else {
                return None;
            };
            let SemConstValue::Array { elems, .. } = value.value(db) else {
                return None;
            };
            use num_traits::ToPrimitive;
            if let Some(value) = index.to_usize().and_then(|index| elems.get(index)) {
                return Some(*value);
            }
            *stop = Some(KnownOperandRecovery::Failed {
                owner: cx.owner(),
                result_ty: ty,
                failure: super::EvalFailure::Ctfe(super::CtfeError::OutOfBounds {
                    origin: super::SemOrigin::Expr(operand),
                }),
            });
            return None;
        }
        Partial::Present(Expr::If(condition, then_expr, Some(else_expr))) => {
            let Partial::Present(crate::hir_def::Cond::Expr(condition)) = condition.data(db, body)
            else {
                return None;
            };
            let value = recover_value(cx, *condition, stop, active)?;
            let SemConstValue::Scalar {
                value: super::SemConstScalar::Bool(value),
                ..
            } = value.value(db)
            else {
                return None;
            };
            return recover_value(
                cx,
                if *value { *then_expr } else { *else_expr },
                stop,
                active,
            );
        }
        Partial::Present(Expr::Block(stmts)) => {
            let (tail, prefix) = stmts.split_last()?;
            // Every prefix operation must itself be known. Never discard a fault
            // or a runtime effect while recovering only a block's result.
            for stmt in prefix {
                let value = match stmt.data(db, body) {
                    Partial::Present(
                        crate::hir_def::Stmt::Let(_, _, Some(value))
                        | crate::hir_def::Stmt::Expr(value),
                    ) => *value,
                    _ => return None,
                };
                recover_value(cx, value, stop, active)?;
            }
            let Partial::Present(crate::hir_def::Stmt::Expr(value)) = tail.data(db, body) else {
                return None;
            };
            return recover_value(cx, *value, stop, active);
        }
        Partial::Present(Expr::With(bindings, value)) => {
            for binding in bindings {
                match cx.opening(binding.value) {
                    Some(crate::analysis::ty::ty_check::TraitOpening::Named(_)) => {}
                    Some(crate::analysis::ty::ty_check::TraitOpening::Evidence {
                        operand,
                        goal,
                        admission: Some(context),
                    }) => {
                        let value = recover_value(cx, operand, stop, active)?;
                        revalidate_opened_value(cx, operand, value, context, goal, stop)?;
                    }
                    Some(crate::analysis::ty::ty_check::TraitOpening::Evidence {
                        admission: None,
                        ..
                    }) => return None,
                    None => {
                        recover_value(cx, binding.value, stop, active)?;
                    }
                }
            }
            return recover_value(cx, *value, stop, active);
        }
        Partial::Present(Expr::Match(scrutinee, Partial::Present(arms))) => {
            let value = recover_value(cx, *scrutinee, stop, active)?;
            let roots = arms
                .iter()
                .map(|arm| cx.pattern_root(arm.pat))
                .collect::<Option<Vec<_>>>()?;
            let tree = crate::analysis::ty::decision_tree::build_decision_tree(
                db,
                cx.pattern_store(),
                &roots,
            );
            let arm = known_match_arm(db, &tree, value)?;
            return recover_value(cx, arms.get(arm)?.body, stop, active);
        }
        Partial::Present(Expr::RecordInit(_, fields)) => {
            let lowering = cx.record_init_lowering(operand)?;
            let record = match lowering {
                RecordInitLowering::Struct => RecordLike::Type(ty),
                RecordInitLowering::EnumVariant(variant) => RecordLike::EnumVariant(variant),
            };
            let mut values = vec![None; fields.len()];
            for field in fields {
                let label = field.label_eagerly(db, body)?;
                let index = record.record_field_idx(db, label)?;
                *values.get_mut(index)? = Some(recover_value(cx, field.expr, stop, active)?);
            }
            let values = values
                .into_iter()
                .collect::<Option<Vec<_>>>()?
                .into_boxed_slice();
            return Some(match lowering {
                RecordInitLowering::Struct => {
                    crate::analysis::semantic::struct_const(db, ty, values)
                }
                RecordInitLowering::EnumVariant(variant) => crate::analysis::semantic::enum_const(
                    db,
                    ty,
                    crate::analysis::semantic::VariantIndex(variant.variant.idx),
                    values,
                ),
            });
        }
        Partial::Present(Expr::Tuple(elems) | Expr::Array(elems)) => {
            let values = elems
                .iter()
                .map(|&value| recover_value(cx, value, stop, active))
                .collect::<Option<Vec<_>>>()?;
            return Some(
                if matches!(operand.data(db, body), Partial::Present(Expr::Tuple(_))) {
                    crate::analysis::semantic::tuple_const(db, ty, values.into_boxed_slice())
                } else {
                    crate::analysis::semantic::array_const(db, ty, values.into_boxed_slice())
                },
            );
        }
        Partial::Present(Expr::Lit(crate::hir_def::LitKind::Int(value))) => {
            return Some(crate::analysis::semantic::int_const(
                db,
                ty,
                num_bigint::BigInt::from(value.data(db).clone()),
            ));
        }
        Partial::Present(Expr::Lit(crate::hir_def::LitKind::Bool(value))) => {
            return Some(crate::analysis::semantic::bool_const(db, *value));
        }
        _ => {}
    }
    if let Some(binding @ LocalBinding::Local { is_mut: false, .. }) = cx.binding(operand) {
        let source = cx.binding_source(db, binding)?;
        let mut value = recover_value(cx, source.init_expr, stop, active)?;
        value = project_value(db, value, &source.projection)?;
        return Some(value);
    }
    if let Some(callable) = cx.callable(operand) {
        if let Some(record) = callable.evidence_introduction() {
            return super::issue_checked_evidence(db, record, ty, super::SemOrigin::Expr(operand))
                .ok()
                .map(|value| value.value());
        }
        if let Some((opened, context)) = callable.evidence_opening() {
            let value = recover_value(cx, opened, stop, active)?;
            let goal = crate::analysis::ty::corelib::evidence_goal(db, context.scope(), ty)?;
            let record = revalidate_opened_value(cx, opened, value, context, goal, stop)?;
            return super::issue_checked_evidence(db, record, ty, super::SemOrigin::Expr(operand))
                .ok()
                .map(|value| value.value());
        }
        if let crate::hir_def::CallableDef::VariantCtor(variant) = callable.callable_def() {
            let values = callable
                .argument_operands()
                .iter()
                .map(|&operand| recover_value(cx, operand, stop, active))
                .collect::<Option<Vec<_>>>()?;
            return Some(super::enum_const(
                db,
                ty,
                super::VariantIndex(variant.idx),
                values.into_boxed_slice(),
            ));
        }
    }
    cx.leaf(operand, stop, active)
}

fn revalidate_opened_value<'db>(
    cx: &impl OperandRecoveryCx<'db>,
    operand: ExprId,
    value: SemConstId<'db>,
    context: crate::analysis::ty::trait_resolution::TraitSolveCx<'db>,
    goal: crate::analysis::ty::trait_def::TraitInstId<'db>,
    stop: &mut Option<KnownOperandRecovery<'db>>,
) -> Option<crate::analysis::ty::trait_def::ImplApplicationRecord<'db>> {
    let db = cx.db();
    let result = match value.value(db) {
        SemConstValue::Evidence { application, .. } => application
            .revalidate(db, context, goal)
            .map(|checked| checked.record())
            .map_err(|error| {
                format!("opened evidence application was not admitted at this use: {error:?}")
            }),
        _ => Err("opened evidence requires a compiler-issued application".to_string()),
    };
    match result {
        Ok(record) => Some(record),
        Err(message) => {
            *stop = Some(KnownOperandRecovery::Failed {
                owner: cx.owner(),
                result_ty: cx.expr_ty(db, operand),
                failure: super::EvalFailure::Ctfe(super::CtfeError::InvalidOperation {
                    origin: super::SemOrigin::Expr(operand),
                    message,
                }),
            });
            None
        }
    }
}

pub(crate) fn project_value<'db>(
    db: &'db dyn HirAnalysisDb,
    mut value: SemConstId<'db>,
    projection: &[ReturnProjectionStep],
) -> Option<SemConstId<'db>> {
    for step in projection {
        value = match (step, value.value(db)) {
            (ReturnProjectionStep::Field(index), SemConstValue::Tuple { elems, .. }) => {
                *elems.get(usize::from(*index))?
            }
            (ReturnProjectionStep::Field(index), SemConstValue::Struct { fields, .. }) => {
                *fields.get(usize::from(*index))?
            }
            (ReturnProjectionStep::ConstantIndex(index), SemConstValue::Array { elems, .. }) => {
                *elems.get(*index)?
            }
            (
                ReturnProjectionStep::VariantField { variant, field },
                SemConstValue::Enum {
                    variant: actual,
                    fields,
                    ..
                },
            ) if *variant == actual.0 => *fields.get(usize::from(*field))?,
            _ => return None,
        };
    }
    Some(value)
}

/// Read the existing checked decision tree for a known scalar/variant source.
/// Switch occurrences use the same checked structural projections as bindings.
fn known_match_arm<'db>(
    db: &'db dyn HirAnalysisDb,
    tree: &crate::analysis::ty::decision_tree::DecisionTree<'db>,
    value: crate::analysis::semantic::SemConstId<'db>,
) -> Option<usize> {
    use crate::analysis::semantic::SemConstScalar;
    use crate::analysis::ty::{
        decision_tree::{Case, DecisionTree},
        pattern_ir::ConstructorKind,
    };
    match tree {
        DecisionTree::Unreachable => None,
        DecisionTree::Leaf(leaf) => Some(leaf.arm_index),
        DecisionTree::Switch(switch) => {
            let path = switch
                .occurrence
                .iter()
                .map(|step| match step {
                    crate::projection::Projection::Field(index) => {
                        Some(ReturnProjectionStep::Field(u16::try_from(*index).ok()?))
                    }
                    crate::projection::Projection::VariantField {
                        variant, field_idx, ..
                    } => Some(ReturnProjectionStep::VariantField {
                        variant: variant.idx,
                        field: u16::try_from(*field_idx).ok()?,
                    }),
                    crate::projection::Projection::Index(
                        crate::projection::IndexSource::Constant(index),
                    ) => Some(ReturnProjectionStep::ConstantIndex(*index)),
                    _ => None,
                })
                .collect::<Option<Vec<_>>>()?;
            let projected = project_value(db, value, &path)?;
            let branch = switch.arms.iter().find_map(|(case, branch)| {
                let matches = match (case, projected.value(db)) {
                    (Case::Default | Case::Constructor(ConstructorKind::Type(_)), _) => true,
                    (
                        Case::Constructor(ConstructorKind::Literal(
                            crate::hir_def::LitKind::Bool(expected),
                            _,
                        )),
                        SemConstValue::Scalar {
                            value: SemConstScalar::Bool(actual),
                            ..
                        },
                    ) => expected == actual,
                    (
                        Case::Constructor(ConstructorKind::Literal(
                            crate::hir_def::LitKind::Int(expected),
                            _,
                        )),
                        SemConstValue::Scalar {
                            value: SemConstScalar::Int { value: actual },
                            ..
                        },
                    ) => num_bigint::BigInt::from(expected.data(db).clone()) == *actual,
                    (
                        Case::Constructor(ConstructorKind::Variant(expected, _)),
                        SemConstValue::Enum { variant, .. },
                    ) => expected.idx == variant.0,
                    _ => false,
                };
                matches.then_some(branch)
            })?;
            known_match_arm(db, branch, value)
        }
    }
}
