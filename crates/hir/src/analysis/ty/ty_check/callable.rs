use crate::{
    hir_def::{
        BinOp, CallArg as HirCallArg, Expr, ExprId, FieldIndex, GenericArgListId, IdentId, LitKind,
        Partial, UnOp,
    },
    span::{
        DynLazySpan,
        expr::{LazyCallArgListSpan, LazyCallArgSpan},
        params::LazyGenericArgListSpan,
    },
};
use salsa::Update;

use super::{ExprProp, TyChecker};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        diagnostics::{BodyDiag, FuncBodyDiag},
        fold::{AssocTySubst, TyFoldable, TyFolder},
        trait_def::TraitInstId,
        trait_resolution::constraint::collect_func_def_constraints,
        ty_def::{BorrowKind, CapabilityKind},
        ty_def::{InvalidCause, TyBase, TyData, TyId},
        ty_lower::lower_generic_arg_list,
        visitor::{TyVisitable, TyVisitor},
    },
};
use crate::hir_def::Body;
use crate::hir_def::CallableDef;
use crate::hir_def::params::FuncParamMode;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct Callable<'db> {
    pub callable_def: CallableDef<'db>,
    base_ty: TyId<'db>,
    generic_args: Vec<TyId<'db>>,
    /// The originating trait instance if this callable comes from a trait method
    /// (e.g., operator overloading, method call, indexing). None for inherent functions.
    pub trait_inst: Option<TraitInstId<'db>>,
}

impl<'db> TyVisitable<'db> for Callable<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.generic_args.visit_with(visitor);
        if let Some(inst) = self.trait_inst {
            inst.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for Callable<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        Self {
            callable_def: self.callable_def,
            base_ty: self.base_ty,
            generic_args: self.generic_args.fold_with(db, folder),
            trait_inst: self.trait_inst.map(|i| i.fold_with(db, folder)),
        }
    }
}

impl<'db> Callable<'db> {
    pub fn new(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        span: DynLazySpan<'db>,
        trait_inst: Option<TraitInstId<'db>>,
    ) -> Result<Self, FuncBodyDiag<'db>> {
        let (base, args) = ty.decompose_ty_app(db);

        if base.is_ty_var(db) {
            return Err(BodyDiag::TypeMustBeKnown(span).into());
        }

        let TyData::TyBase(TyBase::Func(callable_def)) = base.data(db) else {
            return Err(BodyDiag::NotCallable(span, ty).into());
        };

        let params = ty.generic_args(db);
        assert_eq!(params.len(), args.len());

        let callable_def = *callable_def;

        Ok(Self {
            callable_def,
            base_ty: base,
            generic_args: args.to_vec(),
            trait_inst,
        })
    }

    pub fn generic_args(&self) -> &[TyId<'db>] {
        &self.generic_args
    }

    pub fn generic_args_mut(&mut self) -> &mut Vec<TyId<'db>> {
        &mut self.generic_args
    }

    pub fn trait_inst(&self) -> Option<TraitInstId<'db>> {
        self.trait_inst
    }

    pub fn ret_ty(&self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let ret = self
            .callable_def
            .ret_ty(db)
            .instantiate(db, &self.generic_args);
        if let Some(inst) = self.trait_inst {
            let mut subst = AssocTySubst::new(inst);
            ret.fold_with(db, &mut subst)
        } else {
            ret
        }
    }

    pub fn ty(&self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let ty = TyId::foldl(db, self.base_ty, &self.generic_args);
        if let Some(inst) = self.trait_inst {
            let mut subst = AssocTySubst::new(inst);
            ty.fold_with(db, &mut subst)
        } else {
            ty
        }
    }

    pub(super) fn unify_generic_args(
        &mut self,
        tc: &mut TyChecker<'db>,
        args: GenericArgListId<'db>,
        span: LazyGenericArgListSpan<'db>,
    ) -> bool {
        let db = tc.db;
        if !args.is_given(db) {
            return true;
        }

        let given_args = lower_generic_arg_list(db, args, tc.env.scope(), tc.env.assumptions());
        let offset = self.callable_def.offset_to_explicit_params_position(db);
        let current_args = &mut self.generic_args[offset..];

        if current_args.len() != given_args.len() {
            let diag = BodyDiag::CallGenericArgNumMismatch {
                primary: span.into(),
                def_span: self.callable_def.name_span(),
                given: given_args.len(),
                expected: current_args.len(),
            };
            tc.push_diag(diag);

            return false;
        }

        for (i, (&given, arg)) in given_args.iter().zip(current_args.iter_mut()).enumerate() {
            *arg = tc.equate_ty(given, *arg, span.clone().arg(i).into());
        }

        true
    }

    pub(super) fn check_args(
        &self,
        tc: &mut TyChecker<'db>,
        call_args: &[HirCallArg<'db>],
        span: LazyCallArgListSpan<'db>,
        receiver: Option<(ExprId, ExprProp<'db>)>,
        already_typed: bool,
    ) {
        let db = tc.db;

        let expected_arity = self.callable_def.arg_tys(db).len();
        let given_arity = if receiver.is_some() {
            call_args.len() + 1
        } else {
            call_args.len()
        };
        let has_receiver = receiver.is_some();
        if given_arity != expected_arity {
            let diag = BodyDiag::CallArgNumMismatch {
                primary: span.into(),
                def_span: self.callable_def.name_span(),
                given: given_arity,
                expected: expected_arity,
            };
            tc.push_diag(diag);
            return;
        }

        let mut args = if let Some((receiver_expr, receiver_prop)) = receiver {
            let mut args = Vec::with_capacity(call_args.len() + 1);
            let arg = CallArg::new(
                IdentId::make_self(db).into(),
                receiver_expr,
                receiver_prop,
                None,
                receiver_expr.span(tc.body()).into(),
            );
            args.push(arg);
            args
        } else {
            Vec::with_capacity(call_args.len())
        };

        for (i, hir_arg) in call_args.iter().enumerate() {
            args.push(CallArg::from_hir_arg(
                tc,
                hir_arg,
                span.clone().arg(i),
                already_typed,
            ));
        }

        let expected_arg_tys = self.callable_def.arg_tys(db);
        let func_params: Option<Vec<_>> = match self.callable_def {
            CallableDef::Func(func) => {
                let params: Vec<_> = func.params(db).collect();
                if params.len() != expected_arg_tys.len() {
                    panic!(
                        "callable param length mismatch: expected {} param tys but have {} params",
                        expected_arg_tys.len(),
                        params.len()
                    );
                }
                Some(params)
            }
            CallableDef::VariantCtor(_) => None,
        };

        let body = tc.body();
        let is_unary = |expr: ExprId, op: UnOp| {
            matches!(
                expr.data(db, body),
                Partial::Present(Expr::Un(_, found)) if *found == op
            )
        };

        for (i, (given, expected)) in args.into_iter().zip(expected_arg_tys.iter()).enumerate() {
            // Only check labels when the caller explicitly provides one.
            // If no label is provided (given.label is None), it's a positional argument.
            if let Some(given_label) = given.label
                && let Some(expected_label) = self.callable_def.param_label(db, i)
                && !expected_label.is_self(db)
                && given_label != expected_label
            {
                let diag = BodyDiag::CallArgLabelMismatch {
                    primary: given.label_span.unwrap_or(given.expr_span.clone()),
                    def_span: self.callable_def.name_span(),
                    given: given.label,
                    expected: expected_label,
                };
                tc.push_diag(diag);
            }

            let mut expected = expected.instantiate(db, &self.generic_args);
            if let Some(inst) = self.trait_inst {
                let mut subst = AssocTySubst::new(inst);
                expected = expected.fold_with(db, &mut subst);
            }
            let mut expected = tc.normalize_ty(expected);
            let mode = func_params
                .as_ref()
                .and_then(|params| params.get(i).copied())
                .map(|param| param.mode(db));
            let given_ty = tc.normalize_ty(given.expr_prop.ty);
            let own_capability_inner = if mode == Some(FuncParamMode::Own)
                && !expected.is_ty_var(db)
                && let Some((kind, inner)) = given_ty.as_capability(db)
                && tc.ty_unifies(inner, expected)
                && !tc.ty_is_copy(inner)
            {
                Some((kind, inner))
            } else {
                None
            };
            let own_tyvar = mode == Some(FuncParamMode::Own) && expected.is_ty_var(db);
            let mut actual = if let Some((kind, inner)) = own_capability_inner {
                tc.push_diag(BodyDiag::OwnArgMustBeOwnedMove {
                    primary: given.expr_span.clone(),
                    kind,
                    given: inner,
                });
                TyId::invalid(db, InvalidCause::Other)
            } else if own_tyvar && let Some((kind, inner)) = given_ty.as_capability(db) {
                if tc.ty_is_copy(inner) {
                    inner
                } else {
                    tc.push_diag(BodyDiag::OwnArgMustBeOwnedMove {
                        primary: given.expr_span.clone(),
                        kind,
                        given: inner,
                    });
                    TyId::invalid(db, InvalidCause::Other)
                }
            } else {
                tc.try_coerce_capability_for_expr_to_expected(
                    given.expr,
                    given.expr_prop.ty,
                    expected,
                )
                .unwrap_or(given.expr_prop.ty)
            };
            if has_receiver
                && i == 0
                && let Some((required_kind, required_inner)) = expected.as_capability(db)
                && matches!(required_kind, CapabilityKind::Mut | CapabilityKind::Ref)
                && actual == given.expr_prop.ty
                && tc.ty_unifies(given_ty, required_inner)
            {
                actual = match required_kind {
                    CapabilityKind::Mut => TyId::borrow_mut_of(db, given_ty),
                    CapabilityKind::Ref => TyId::borrow_ref_of(db, given_ty),
                    CapabilityKind::View => unreachable!(),
                };
            }
            let mut has_targeted_borrow_diag = false;

            // Enforce explicit call-site borrow syntax for places.
            //
            // Borrow handles are copyable values, and `own` parameters consume their argument.
            // Requiring explicit `ref`/`mut` on *place* arguments makes aliasing visible at the
            // call site, and ensures MIR borrow checking sees the right loan operations.
            if let Some(params) = func_params.as_ref() {
                let arg_is_place = tc.env.expr_place(given.expr).is_some();

                let given_capability = tc
                    .normalize_ty(given.expr_prop.ty)
                    .as_capability(db)
                    .map(|(kind, _)| kind);
                if let Some((kind, _)) = expected.as_capability(db)
                    && matches!(kind, CapabilityKind::Mut | CapabilityKind::Ref)
                    && !(has_receiver && i == 0)
                    && given_capability.is_none()
                    && !given.expr_prop.ty.has_invalid(db)
                {
                    let borrow_kind = match kind {
                        CapabilityKind::Mut => BorrowKind::Mut,
                        CapabilityKind::Ref => BorrowKind::Ref,
                        CapabilityKind::View => unreachable!(),
                    };
                    let unary_borrow = match kind {
                        CapabilityKind::Mut => UnOp::Mut,
                        CapabilityKind::Ref => UnOp::Ref,
                        CapabilityKind::View => unreachable!(),
                    };

                    if arg_is_place {
                        if !is_unary(given.expr, unary_borrow) {
                            tc.push_diag(BodyDiag::ExplicitBorrowRequired {
                                primary: given.expr_span.clone(),
                                kind: borrow_kind,
                                suggestion: place_borrow_suggestion(
                                    db,
                                    tc.body(),
                                    given.expr,
                                    borrow_kind,
                                ),
                            });
                            has_targeted_borrow_diag = true;
                        }
                    } else {
                        tc.push_diag(BodyDiag::BorrowArgMustBePlace {
                            primary: given.expr_span.clone(),
                            kind: borrow_kind,
                        });
                        has_targeted_borrow_diag = true;
                    }
                }

                if !has_targeted_borrow_diag {
                    tc.equate_ty(actual, expected, given.expr_span.clone());
                    expected = tc.normalize_ty(expected);
                }

                let mode = mode.unwrap_or_else(|| {
                    params
                        .get(i)
                        .copied()
                        .unwrap_or_else(|| panic!("missing func param at index {i}"))
                        .mode(db)
                });
                if mode == FuncParamMode::Own {
                    if expected.as_borrow(db).is_some() {
                        tc.push_diag(BodyDiag::OwnParamCannotBeBorrow {
                            primary: given.expr_span.clone(),
                            ty: expected,
                        });
                    } else {
                        tc.record_implicit_move_for_owned_expr(given.expr, expected);
                    }
                }
            } else {
                tc.equate_ty(actual, expected, given.expr_span.clone());
                expected = tc.normalize_ty(expected);
                // Variant constructors materialize their fields immediately (owned context).
                tc.record_implicit_move_for_owned_expr(given.expr, expected);
            }
        }
    }
}

fn place_borrow_suggestion<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expr: ExprId,
    kind: BorrowKind,
) -> Option<String> {
    let kw = match kind {
        BorrowKind::Mut => "mut",
        BorrowKind::Ref => "ref",
    };
    place_expr_hint(db, body, expr).map(|place| format!("{kw} {place}"))
}

fn place_expr_hint<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expr: ExprId,
) -> Option<String> {
    match expr.data(db, body) {
        Partial::Present(Expr::Path(Partial::Present(path))) => Some(path.pretty_print(db)),
        Partial::Present(Expr::Field(base, Partial::Present(field_idx))) => {
            let base = place_expr_hint(db, body, *base)?;
            match field_idx {
                FieldIndex::Ident(ident) => Some(format!("{base}.{}", ident.data(db))),
                FieldIndex::Index(index) => Some(format!("{base}.{}", index.data(db))),
            }
        }
        Partial::Present(Expr::Bin(base, index, BinOp::Index)) => {
            let base = place_expr_hint(db, body, *base)?;
            let index = expr_hint(db, body, *index)?;
            Some(format!("{base}[{index}]"))
        }
        _ => None,
    }
}

fn expr_hint<'db>(db: &'db dyn HirAnalysisDb, body: Body<'db>, expr: ExprId) -> Option<String> {
    match expr.data(db, body) {
        Partial::Present(Expr::Path(Partial::Present(path))) => Some(path.pretty_print(db)),
        Partial::Present(Expr::Lit(lit)) => match lit {
            LitKind::Int(int_id) => Some(int_id.data(db).to_string()),
            LitKind::Bool(value) => Some(value.to_string()),
            LitKind::String(value) => Some(format!("{:?}", value.data(db))),
        },
        _ => None,
    }
}

/// The lowered representation of [`HirCallArg`]
struct CallArg<'db> {
    label: Option<IdentId<'db>>,
    expr: ExprId,
    expr_prop: ExprProp<'db>,
    label_span: Option<DynLazySpan<'db>>,
    expr_span: DynLazySpan<'db>,
}

impl<'db> CallArg<'db> {
    fn from_hir_arg(
        tc: &mut TyChecker<'db>,
        arg: &HirCallArg<'db>,
        span: LazyCallArgSpan<'db>,
        already_typed: bool,
    ) -> Self {
        let expr_prop = if already_typed {
            let db = tc.db;
            tc.env
                .typed_expr(arg.expr)
                .unwrap_or_else(|| ExprProp::invalid(db))
        } else {
            let ty = tc.fresh_ty();
            tc.check_expr(arg.expr, ty)
        };
        // Only use explicit labels for function calls, not inferred labels.
        // The `label_eagerly` behavior (inferring labels from variable names) is
        // useful for struct field initialization syntax, but for function calls
        // we should only require a match when the user explicitly provides a label.
        let label = arg.label;
        let label_span = arg.label.is_some().then(|| span.clone().label().into());
        let expr_span = span.expr().into();

        Self::new(label, arg.expr, expr_prop, label_span, expr_span)
    }

    fn new(
        label: Option<IdentId<'db>>,
        expr: ExprId,
        expr_prop: ExprProp<'db>,
        label_span: Option<DynLazySpan<'db>>,
        expr_span: DynLazySpan<'db>,
    ) -> Self {
        Self {
            label,
            expr,
            expr_prop,
            label_span,
            expr_span,
        }
    }
}

impl<'db> Callable<'db> {
    pub(super) fn check_constraints(&self, tc: &mut TyChecker<'db>, span: DynLazySpan<'db>) {
        let db = tc.db;

        // Get the function's constraints
        let constraints = collect_func_def_constraints(db, self.callable_def, true);

        // Instantiate constraints with the actual type arguments
        let instantiated = constraints.instantiate(db, &self.generic_args);

        // Normalize each constraint to resolve associated types
        for &constraint in instantiated.list(db) {
            // Normalize the constraint's arguments
            let normalized_args: Vec<_> = constraint
                .args(db)
                .iter()
                .map(|&arg| tc.normalize_ty(arg))
                .collect();

            let normalized_constraint = TraitInstId::new(
                db,
                constraint.def(db),
                normalized_args,
                constraint.assoc_type_bindings(db).clone(),
            );

            // Register the normalized constraint for confirmation
            tc.env
                .register_confirmation(normalized_constraint, span.clone());
        }
    }
}
