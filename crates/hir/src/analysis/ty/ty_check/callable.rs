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

use super::{BodyOwner, ExprProp, LocalBinding, TyChecker};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        const_ty::LayoutHoleArgSite,
        corelib::resolve_lib_func_path,
        diagnostics::{BodyDiag, FuncBodyDiag},
        fold::{AssocTySubst, TyFoldable, TyFolder},
        normalize::normalize_ty,
        trait_def::TraitInstId,
        trait_resolution::constraint::collect_func_decl_constraints,
        ty_def::{BorrowKind, CapabilityKind},
        ty_def::{InvalidCause, TyBase, TyData, TyFlags, TyId},
        ty_lower::lower_generic_arg_list,
        visitor::{TyVisitable, TyVisitor, collect_flags},
    },
};
use crate::core::semantic::{ProviderBinding, ProviderSource};
use crate::hir_def::Body;
use crate::hir_def::CallableDef;
use crate::hir_def::params::FuncParamMode;

pub(super) enum CallGenericArgUnifyError {
    ArityMismatch { given: usize, expected: usize },
    UnificationFailed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum EffectProviderProvenance<'db> {
    Binding {
        owner: BodyOwner<'db>,
        binding: LocalBinding<'db>,
    },
    Expr {
        owner: BodyOwner<'db>,
        expr: ExprId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct EffectProviderSpecialization<'db> {
    pub provider: ProviderBinding<'db>,
    pub provenance: EffectProviderProvenance<'db>,
}

impl<'db> TyVisitable<'db> for EffectProviderSpecialization<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.provider.provider_ty.visit_with(visitor);
        self.provider.semantics.provider_ty.visit_with(visitor);
        if let Some(target_ty) = self.provider.semantics.target_ty {
            target_ty.visit_with(visitor);
        }
        match &self.provider.source {
            ProviderSource::UsesParam { .. } | ProviderSource::ContractField { .. } => {}
            ProviderSource::RootProvider { registration, .. } => {
                registration.provider_ty.visit_with(visitor);
            }
        }
        if let EffectProviderProvenance::Binding { binding, .. } = self.provenance {
            binding.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for EffectProviderSpecialization<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let provider_ty = self.provider.provider_ty.fold_with(db, folder);
        let semantics = crate::analysis::ty::provider::ProviderSemantics {
            provider_ty: self.provider.semantics.provider_ty.fold_with(db, folder),
            target_ty: self
                .provider
                .semantics
                .target_ty
                .map(|ty| ty.fold_with(db, folder)),
            ..self.provider.semantics
        };
        let source = match self.provider.source {
            ProviderSource::UsesParam {
                site,
                requirement_idx,
            } => ProviderSource::UsesParam {
                site,
                requirement_idx,
            },
            ProviderSource::ContractField {
                contract,
                field_idx,
            } => ProviderSource::ContractField {
                contract,
                field_idx,
            },
            ProviderSource::RootProvider { site, registration } => ProviderSource::RootProvider {
                site,
                registration: crate::analysis::ty::provider::RootProviderRegistration {
                    provider_ty: registration.provider_ty.fold_with(db, folder),
                    ..registration
                },
            },
        };
        let provenance = match self.provenance {
            EffectProviderProvenance::Binding { owner, binding } => {
                EffectProviderProvenance::Binding {
                    owner,
                    binding: binding.fold_with(db, folder),
                }
            }
            EffectProviderProvenance::Expr { owner, expr } => {
                EffectProviderProvenance::Expr { owner, expr }
            }
        };
        Self {
            provider: ProviderBinding {
                provider_ty,
                semantics,
                source,
                ..self.provider
            },
            provenance,
        }
    }
}

pub(super) fn unify_explicit_call_generic_args<'db>(
    callable: &mut Callable<'db>,
    tc: &mut TyChecker<'db>,
    args: GenericArgListId<'db>,
    mut unify_arg: impl FnMut(&mut TyChecker<'db>, usize, TyId<'db>, &mut TyId<'db>) -> bool,
) -> Result<(), CallGenericArgUnifyError> {
    let db = tc.db;
    if !args.is_given(db) {
        return Ok(());
    }

    let given_args = lower_generic_arg_list(
        db,
        args,
        tc.env.scope(),
        tc.env.assumptions(),
        LayoutHoleArgSite::GenericArgList(args),
    );
    let offset = callable.callable_def.offset_to_explicit_params_position(db);
    let current_args = &mut callable.generic_args[offset..];
    if current_args.len() != given_args.len() {
        return Err(CallGenericArgUnifyError::ArityMismatch {
            given: given_args.len(),
            expected: current_args.len(),
        });
    }

    for (idx, (given, current)) in given_args
        .into_iter()
        .zip(current_args.iter_mut())
        .enumerate()
    {
        if !unify_arg(tc, idx, given, current) {
            return Err(CallGenericArgUnifyError::UnificationFailed);
        }
    }

    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct Callable<'db> {
    pub callable_def: CallableDef<'db>,
    base_ty: TyId<'db>,
    generic_args: Vec<TyId<'db>>,
    effect_providers: Vec<EffectProviderSpecialization<'db>>,
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
        self.effect_providers.visit_with(visitor);
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
            effect_providers: self.effect_providers.fold_with(db, folder),
            trait_inst: self.trait_inst.map(|i| i.fold_with(db, folder)),
        }
    }
}

impl<'db> Callable<'db> {
    pub fn callable_def(&self) -> CallableDef<'db> {
        self.callable_def
    }

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
            effect_providers: Vec::new(),
            trait_inst,
        })
    }

    pub fn generic_args(&self) -> &[TyId<'db>] {
        &self.generic_args
    }

    pub fn generic_args_mut(&mut self) -> &mut Vec<TyId<'db>> {
        &mut self.generic_args
    }

    pub fn effect_providers(&self) -> &[EffectProviderSpecialization<'db>] {
        &self.effect_providers
    }

    pub fn effect_providers_mut(&mut self) -> &mut Vec<EffectProviderSpecialization<'db>> {
        &mut self.effect_providers
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

    pub fn arg_ty(&self, db: &'db dyn HirAnalysisDb, idx: usize) -> Option<TyId<'db>> {
        let mut arg = self
            .callable_def
            .arg_tys(db)
            .get(idx)?
            .instantiate(db, &self.generic_args);
        if let Some(inst) = self.trait_inst {
            let mut subst = AssocTySubst::new(inst);
            arg = arg.fold_with(db, &mut subst);
        }
        Some(arg)
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
        match unify_explicit_call_generic_args(self, tc, args, |tc, idx, given, current| {
            *current = tc.equate_ty(given, *current, span.clone().arg(idx).into());
            true
        }) {
            Ok(()) => true,
            Err(CallGenericArgUnifyError::ArityMismatch { given, expected }) => {
                tc.push_diag(BodyDiag::CallGenericArgNumMismatch {
                    primary: span.into(),
                    def_span: self.callable_def.name_span(),
                    given,
                    expected,
                });
                false
            }
            Err(CallGenericArgUnifyError::UnificationFailed) => false,
        }
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
            let arg_idx = if has_receiver { i + 1 } else { i };
            let expected_hint =
                self.compile_time_string_literal_arg_expected(tc, hir_arg.expr, arg_idx);
            args.push(CallArg::from_hir_arg(
                tc,
                hir_arg,
                span.clone().arg(i),
                already_typed,
                expected_hint,
            ));
        }

        let body = tc.body();
        let is_unary = |expr: ExprId, op: UnOp| {
            matches!(
                expr.data(db, body),
                Partial::Present(Expr::Un(_, found)) if *found == op
            )
        };

        for (i, (given, expected)) in args.into_iter().zip(expected_arg_tys.iter()).enumerate() {
            // Call labels are either explicit (`f(x: value)`) or inferred from a bare
            // identifier argument (`f(x)`), but not from arbitrary expressions (`f(10)`).
            // If the callee parameter has an unsuppressed label, the call must provide
            // one of those labels. Do not disable this check or guard it on explicit
            // syntax only; that would silently allow missing labels.
            if let Some(expected_label) = self.callable_def.param_label(db, i)
                && !expected_label.is_self(db)
                && Some(expected_label) != given.label
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
            let given_string_source_ty = given
                .expr_prop
                .binding
                .map(|binding| tc.normalize_ty(tc.env.lookup_binding_ty(&binding)))
                .unwrap_or(given_ty);
            let const_string_arg_ty = if matches!(self.callable_def, CallableDef::Func(func) if func.is_const(db))
                && expected.is_ty_var(db)
                && let TyData::TyVar(var) = given_string_source_ty.base_ty(db).data(db)
                && let crate::analysis::ty::ty_def::TyVarSort::String { min_len, .. } = var.sort
            {
                Some(TyId::string_with_len(db, min_len))
            } else {
                None
            };
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
            } else if let Some(fixed_string_ty) = const_string_arg_ty {
                if let Some(binding) = given.expr_prop.binding {
                    tc.equate_ty(
                        tc.env.lookup_binding_ty(&binding),
                        fixed_string_ty,
                        given.expr_span.clone(),
                    );
                }
                tc.equate_ty(given.expr_prop.ty, fixed_string_ty, given.expr_span.clone());
                fixed_string_ty
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

                let mode = match mode {
                    Some(m) => m,
                    None => match params.get(i).copied() {
                        Some(p) => p.mode(db),
                        None => {
                            unreachable!(
                                "missing func param at index {i} — length check above should have caught this"
                            );
                        }
                    },
                };
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

    fn compile_time_string_literal_arg_expected(
        &self,
        tc: &TyChecker<'db>,
        expr: ExprId,
        arg_idx: usize,
    ) -> Option<TyId<'db>> {
        let Partial::Present(Expr::Lit(LitKind::String(string_id))) = expr.data(tc.db, tc.body())
        else {
            return None;
        };

        let mut expected = self
            .callable_def
            .arg_tys(tc.db)
            .get(arg_idx)?
            .instantiate(tc.db, &self.generic_args);
        if let Some(inst) = self.trait_inst {
            let mut subst = AssocTySubst::new(inst);
            expected = expected.fold_with(tc.db, &mut subst);
        }
        let expected = normalize_ty(tc.db, expected, tc.env.scope(), tc.env.assumptions());
        if tc.string_literal_should_use_byte_array(expected)
            || self
                .callable_def
                .accepts_compile_time_string_literal_bytes(tc.db, tc.env.scope())
        {
            return Some(tc.string_literal_byte_array_ty(string_id.len_bytes(tc.db)));
        }

        None
    }
}

impl<'db> CallableDef<'db> {
    fn accepts_compile_time_string_literal_bytes(
        self,
        db: &'db dyn HirAnalysisDb,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
    ) -> bool {
        let Self::Func(func) = self else {
            return false;
        };

        resolve_lib_func_path(db, scope, "core::keccak")
            .is_some_and(|core_keccak| func == core_keccak)
            || resolve_lib_func_path(db, scope, "std::abi::sol")
                .is_some_and(|std_sol| func == std_sol)
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
        expected_hint: Option<TyId<'db>>,
    ) -> Self {
        let expr_prop = if already_typed && expected_hint.is_none() {
            let db = tc.db;
            tc.env
                .typed_expr(arg.expr)
                .unwrap_or_else(|| ExprProp::invalid(db))
        } else {
            let ty = expected_hint.unwrap_or_else(|| tc.fresh_ty());
            tc.check_expr(arg.expr, ty)
        };
        let label = arg.label_eagerly(tc.db, tc.body());
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
    pub(super) fn enqueue_constraints(
        &self,
        tc: &mut TyChecker<'db>,
        call_expr: ExprId,
        span: DynLazySpan<'db>,
    ) {
        let db = tc.db;
        let constraints = collect_func_decl_constraints(db, self.callable_def, true);
        let instantiated = constraints.instantiate(db, &self.generic_args);

        for (constraint_idx, &constraint) in instantiated.list(db).iter().enumerate() {
            let constraint = if let Some(inst) = self.trait_inst {
                let mut subst = AssocTySubst::new(inst);
                constraint.fold_with(db, &mut subst)
            } else {
                constraint
            };
            let constraint = tc.normalize_trait_goal(constraint);
            if collect_flags(db, constraint).contains(TyFlags::HAS_INVALID) {
                continue;
            }

            tc.env
                .register_trait_obligation(super::env::TraitObligation {
                    goal: constraint,
                    origin: super::env::TraitObligationOrigin::CallConstraint {
                        call_expr,
                        callable_def: self.callable_def,
                        constraint_idx,
                    },
                    span: span.clone(),
                });
        }

        // Check where-clause const predicates.
        self.check_const_predicates(tc, span);
    }

    /// Evaluate where-clause const predicates with the call's concrete generic
    /// args. If any predicate evaluates to `false`, push a diagnostic.
    fn check_const_predicates(&self, tc: &mut TyChecker<'db>, span: DynLazySpan<'db>) {
        use crate::analysis::semantic::{SemConstScalar, SemConstValue, eval_body_owner_const};
        use crate::hir_def::{CallableDef, WhereClauseOwner};

        let db = tc.db;
        let func = match self.callable_def {
            CallableDef::Func(f) => f,
            CallableDef::VariantCtor(_) => return,
        };

        let where_clause = WhereClauseOwner::Func(func).where_clause(db);
        let const_preds = where_clause.const_predicates(db);
        if const_preds.is_empty() {
            return;
        }

        // Skip evaluation if any generic arg still contains inference variables.
        if self
            .generic_args
            .iter()
            .any(|ty| collect_flags(db, *ty).contains(TyFlags::HAS_VAR))
        {
            return;
        }

        let bool_ty = TyId::bool(db);
        for body in const_preds {
            let owner = BodyOwner::AnonConstBody {
                body: *body,
                expected: bool_ty,
            };
            match eval_body_owner_const(db, owner, self.generic_args.clone()) {
                Ok(value) => {
                    let is_false = matches!(
                        value.value(db),
                        SemConstValue::Scalar {
                            value: SemConstScalar::Bool(false),
                            ..
                        }
                    );
                    if is_false {
                        tc.push_diag(BodyDiag::WhereConstPredicateFailed {
                            primary: span.clone(),
                        });
                    }
                }
                Err(_) => {
                    // CTFE evaluation failed -- silently skip for the prototype.
                    // In a production implementation, we'd emit a more specific
                    // diagnostic here.
                }
            }
        }
    }
}
