use crate::{
    hir_def::{
        BinOp, CallArg as HirCallArg, Expr, ExprId, FieldIndex, GenericArgListId, IdentId,
        ItemKind, LitKind, Partial, UnOp,
    },
    span::{
        DynLazySpan,
        expr::{LazyCallArgListSpan, LazyCallArgSpan},
        params::LazyGenericArgListSpan,
    },
};
use common::indexmap::IndexMap;
use salsa::Update;

use super::{
    BodyOwner, ClosureCaptureAccess, ClosureExpectation, ExprProp, LocalBinding,
    TraitObligationOutcome, TyChecker, ValueAccess,
};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        closure::ClosureCallTrait,
        const_ty::{CallableInputLayoutHoleOrigin, HoleAnchor, HoleMinter, LayoutHoleArgSite},
        corelib::resolve_lib_func_path,
        diagnostics::{BodyDiag, CallArgDefinition, FuncBodyDiag},
        fold::{AssocTySubst, TyFoldable, TyFolder},
        normalize::normalize_ty,
        trait_def::TraitInstId,
        trait_resolution::{
            TraitSolveCx, check_trait_inst_wf, constraint::collect_func_decl_constraints,
        },
        ty_def::{BorrowKind, CapabilityKind},
        ty_def::{InvalidCause, TyBase, TyData, TyFlags, TyId},
        ty_is_noesc,
        ty_lower::{lower_generic_arg_list, specialized_callable_layout_bundle_signature},
        visitor::{TyVisitable, TyVisitor, collect_flags, walk_ty},
    },
};
use crate::core::semantic::{ProviderBinding, ProviderSource, param_env};
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
        let evidence = match self.provider.semantics.evidence {
            crate::analysis::ty::provider::ProviderLayoutEvidence::ResolvedHandle(instance) => {
                crate::analysis::ty::provider::ProviderLayoutEvidence::ResolvedHandle(
                    instance.fold_with(db, folder),
                )
            }
            evidence => evidence,
        };
        let semantics = crate::analysis::ty::provider::ProviderSemantics {
            provider_ty: self.provider.semantics.provider_ty.fold_with(db, folder),
            target_ty: self
                .provider
                .semantics
                .target_ty
                .map(|ty| ty.fold_with(db, folder)),
            evidence,
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
            ProviderSource::ContractField { field } => ProviderSource::ContractField { field },
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
    anchor: HoleAnchor<'db>,
    mut unify_arg: impl FnMut(&mut TyChecker<'db>, usize, TyId<'db>, &mut TyId<'db>) -> bool,
) -> Result<(), CallGenericArgUnifyError> {
    let db = tc.db;
    if !args.is_given(db) {
        return Ok(());
    }

    let minter = HoleMinter::new(anchor);
    let given_args = lower_generic_arg_list(
        db,
        args,
        tc.env.scope(),
        tc.env.assumptions(),
        LayoutHoleArgSite::GenericArgList(args),
        &minter,
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
    fn closure_expectations_for_arg(
        &self,
        tc: &mut TyChecker<'db>,
        arg_idx: usize,
    ) -> Vec<(TyId<'db>, ClosureExpectation<'db>)> {
        struct ContainsTy<'db> {
            db: &'db dyn HirAnalysisDb,
            needle: TyId<'db>,
            found: bool,
        }

        impl<'db> TyVisitor<'db> for ContainsTy<'db> {
            fn db(&self) -> &'db dyn HirAnalysisDb {
                self.db
            }

            fn visit_ty(&mut self, ty: TyId<'db>) {
                if ty == self.needle {
                    self.found = true;
                } else if !self.found {
                    walk_ty(self, ty);
                }
            }
        }

        let db = tc.db;
        let constraints = collect_func_decl_constraints(db, self.callable_def, true);
        let declared = constraints.instantiate_identity();
        let normalized_generic_args = self
            .generic_args
            .iter()
            .map(|&ty| tc.normalize_ty(ty))
            .collect::<Vec<_>>();
        let instantiated = constraints.instantiate(db, &normalized_generic_args);
        let formal_arg_ty = self
            .callable_def
            .arg_tys(db)
            .get(arg_idx)
            .expect("call argument index must have a formal type")
            .instantiate_identity();
        let formal_subject_ty = formal_arg_ty
            .as_capability(db)
            .map(|(_, inner)| inner)
            .unwrap_or(formal_arg_ty);
        let mut found = Vec::new();

        for (&declared, &constraint) in declared.list(db).iter().zip(instantiated.list(db)) {
            let Some(&declared_subject) = declared.args(db).first() else {
                continue;
            };
            let mut contains = ContainsTy {
                db,
                needle: declared_subject,
                found: false,
            };
            contains.visit_ty(formal_subject_ty);
            if !contains.found
                || ClosureCallTrait::for_trait(db, tc.env.scope(), declared.def(db)).is_none()
            {
                continue;
            }
            let constraint = if let Some(inst) = self.trait_inst {
                let mut subst = AssocTySubst::new(inst);
                constraint.fold_with(db, &mut subst)
            } else {
                constraint
            };
            let constraint = tc.normalize_trait_goal(constraint);
            let args = constraint.args(db);
            if args.len() != 3 {
                continue;
            }
            let pack = tc.normalize_ty(args[1]);
            if !pack.is_tuple(db) {
                continue;
            }
            let subject = tc.normalize_ty(args[0]);
            let expected = ClosureExpectation {
                params: pack.field_types(db),
                ret_ty: tc.normalize_ty(args[2]),
            };
            if !found.contains(&(subject, expected.clone())) {
                found.push((subject, expected));
            }
        }
        found
    }

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
        let trait_inst = trait_inst.or_else(|| {
            let CallableDef::Func(func) = callable_def else {
                return None;
            };
            let Some(ItemKind::Trait(trait_)) = func.scope().parent_item(db) else {
                return None;
            };
            let trait_arg_count = trait_.params(db).len();
            (args.len() >= trait_arg_count).then(|| {
                TraitInstId::new(
                    db,
                    trait_,
                    args[..trait_arg_count].to_vec(),
                    IndexMap::new(),
                )
            })
        });

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

    pub(super) fn specialize_arg_from_actual(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        arg_idx: usize,
        actual: TyId<'db>,
    ) {
        let Some(expected) = self
            .callable_def
            .arg_tys(db)
            .get(arg_idx)
            .map(|ty| ty.instantiate_identity())
        else {
            return;
        };
        self.specialize_params_from_actual(db, expected, actual);
    }

    pub(super) fn specialize_params_from_actual(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        expected: TyId<'db>,
        actual: TyId<'db>,
    ) {
        bind_callable_params_from_actual(db, expected, actual, &mut self.generic_args);
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

    pub fn call_trait_args_pack_ty(
        &self,
        db: &'db dyn HirAnalysisDb,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
    ) -> Option<TyId<'db>> {
        let inst = self.trait_inst?;
        ClosureCallTrait::for_trait(db, scope, inst.def(db))?;
        inst.args(db).get(1).copied()
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
        anchor: HoleAnchor<'db>,
        span: LazyGenericArgListSpan<'db>,
    ) -> bool {
        match unify_explicit_call_generic_args(self, tc, args, anchor, |tc, idx, given, current| {
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
        &mut self,
        tc: &mut TyChecker<'db>,
        call_args: &[HirCallArg<'db>],
        span: LazyCallArgListSpan<'db>,
        receiver: Option<(ExprId, ExprProp<'db>)>,
        already_typed: bool,
    ) {
        let db = tc.db;

        let closure_call_ty = self.trait_inst().and_then(|inst| {
            ClosureCallTrait::for_trait(db, tc.env.scope(), inst.def(db))?;
            tc.normalize_ty(inst.self_ty(db)).base_ty(db).as_closure(db)
        });
        let call_args_pack = self
            .call_trait_args_pack_ty(db, tc.env.scope())
            .map(|ty| tc.normalize_ty(ty));
        if let Some(args_ty) = call_args_pack.filter(|ty| !ty.is_tuple(db)) {
            tc.push_diag(BodyDiag::CallArgsMustBeTuple {
                primary: span.into(),
                args_ty,
            });
            return;
        }
        let call_arg_tys = call_args_pack.map(|ty| ty.field_types(db));
        let has_receiver = receiver.is_some();
        let expected_arity = call_arg_tys.as_ref().map_or_else(
            || {
                self.callable_def
                    .arg_tys(db)
                    .len()
                    .checked_sub(usize::from(has_receiver))
                    .expect("a method callable must have a receiver parameter")
            },
            |args| args.len() + usize::from(!has_receiver),
        );
        let given_arity = call_args.len();
        if given_arity != expected_arity {
            let definition = closure_call_ty.map_or_else(
                || CallArgDefinition::Function(self.callable_def.name_span()),
                CallArgDefinition::Closure,
            );
            let diag = BodyDiag::CallArgNumMismatch {
                primary: span.into(),
                definition,
                given: given_arity,
                expected: expected_arity,
            };
            tc.push_diag(diag);
            return;
        }

        let physical_params: Option<Vec<_>> = match self.callable_def {
            CallableDef::Func(func) => {
                let params: Vec<_> = func.params(db).collect();
                if params.len() != self.callable_def.arg_tys(db).len() {
                    panic!(
                        "callable param length mismatch: expected {} param tys but have {} params",
                        self.callable_def.arg_tys(db).len(),
                        params.len()
                    );
                }
                Some(params)
            }
            CallableDef::VariantCtor(_) => None,
        };
        let expected_arg_tys = if let Some(call_arg_tys) = call_arg_tys {
            let mut expected = Vec::with_capacity(call_arg_tys.len() + 1);
            expected.push(
                self.arg_ty(db, 0)
                    .expect("call trait method must have a receiver"),
            );
            expected.extend(call_arg_tys);
            expected
        } else {
            (0..self.callable_def.arg_tys(db).len())
                .map(|idx| {
                    self.arg_ty(db, idx)
                        .expect("callable argument index must be valid")
                })
                .collect()
        };
        let arg_modes = physical_params.as_ref().map(|params| {
            if call_args_pack.is_some() {
                let mut modes = Vec::with_capacity(expected_arg_tys.len());
                modes.push(params[0].mode(db));
                modes.extend(expected_arg_tys[1..].iter().map(|ty| {
                    if ty.as_capability(db).is_some() {
                        FuncParamMode::View
                    } else {
                        FuncParamMode::Own
                    }
                }));
                modes
            } else {
                params.iter().map(|param| param.mode(db)).collect()
            }
        });
        let expected_labels = (0..expected_arg_tys.len())
            .map(|idx| {
                if call_args_pack.is_some() && idx > 0 {
                    None
                } else {
                    self.callable_def.param_label(db, idx)
                }
            })
            .collect::<Vec<_>>();
        let has_closure_arg = call_args.iter().any(|arg| {
            matches!(
                arg.expr.data(db, tc.body()),
                Partial::Present(Expr::Closure { .. })
            )
        });
        if has_closure_arg && call_args_pack.is_none() {
            for (idx, arg) in call_args.iter().enumerate() {
                if matches!(
                    arg.expr.data(db, tc.body()),
                    Partial::Present(Expr::Closure { .. })
                ) || tc.env.typed_expr(arg.expr).is_some()
                {
                    continue;
                }
                let arg_idx = idx + usize::from(has_receiver);
                let actual = tc.check_expr_unknown(arg.expr).ty;
                let expected = tc.normalize_ty(expected_arg_tys[arg_idx]);
                let actual_payload = actual
                    .as_capability(db)
                    .map(|(_, inner)| inner)
                    .unwrap_or(actual);
                let expected_payload = expected
                    .as_capability(db)
                    .map(|(_, inner)| inner)
                    .unwrap_or(expected);
                tc.table.unify(actual_payload, expected_payload).ok();
            }
        }
        let layout_input_origins = match self.callable_def {
            CallableDef::Func(func) => {
                specialized_callable_layout_bundle_signature(db, func, &self.generic_args)
                    .inputs
                    .into_iter()
                    .map(|input| input.origin)
                    .collect::<Vec<_>>()
            }
            CallableDef::VariantCtor(_) => Vec::new(),
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
            let closure_expectations = if call_args_pack.is_none() {
                self.closure_expectations_for_arg(tc, arg_idx)
            } else {
                Default::default()
            };
            let layout_origin = match (self.callable_def, call_args_pack) {
                (_, Some(_)) => None,
                (CallableDef::Func(_), None) => {
                    Some(CallableInputLayoutHoleOrigin::ValueParam(arg_idx))
                }
                (CallableDef::VariantCtor(_), None) => None,
            };
            let accepts_contextual_hint = matches!(
                hir_arg.expr.data(db, tc.body()),
                Partial::Present(
                    Expr::RecordInit(..)
                        | Expr::Tuple(..)
                        | Expr::Array(..)
                        | Expr::ArrayRep(..)
                        | Expr::Call(..)
                        | Expr::MethodCall(..)
                        | Expr::Block(..)
                        | Expr::If(..)
                        | Expr::Match(..)
                        | Expr::With(..)
                )
            );
            let expected_arg_ty = expected_arg_tys.get(arg_idx).copied().map(|ty| {
                let ty = tc.normalize_ty(ty);
                ty.as_view(db).unwrap_or(ty)
            });
            let payload_contains_noesc = expected_arg_ty.is_some_and(|ty| {
                let payload = ty.as_capability(db).map_or(ty, |(_, payload)| payload);
                ty_is_noesc(db, payload)
            });
            let carries_contextual_closure = !closure_expectations.is_empty()
                && (tc.expr_contains_closure_syntax(hir_arg.expr)
                    || tc.expr_can_replay_contextual_closure(hir_arg.expr));
            // A `view F` forwarded into another default-view `F` parameter must first be
            // checked through that interface carrier. Feeding the bare closure payload hint to
            // every `Fn`-bounded argument would instead infer the callee's `F` as `view F`.
            // Closure syntax and known closure aliases can use the payload immediately; an alias
            // discovered only while checking is replayed with `deferred_closure_hint` below.
            let deferred_closure_hint = (!closure_expectations.is_empty())
                .then_some(expected_arg_ty)
                .flatten();
            // Aggregate constructors need the logical parameter type when it carries layout
            // evidence or nested capabilities. Layout roots must be anchored to the value being
            // passed, while nested capabilities must reach tuple/array elements before the
            // aggregate is formed; whole-value coercion is too late to recover either.
            //
            // Keep contextual typing selective because applying every expected call type changes
            // ordinary inference and compile-time evaluation. A top-level `view` remains an
            // interface capability rather than the constructed value's type, so strip only that
            // outer layer and preserve capabilities nested in the payload.
            let expected_hint = call_args_pack
                .is_none()
                .then(|| self.compile_time_string_literal_arg_expected(tc, hir_arg.expr, arg_idx))
                .flatten()
                .or_else(|| {
                    (accepts_contextual_hint
                        && (matches!(self.callable_def, CallableDef::VariantCtor(_))
                            || layout_origin
                                .is_some_and(|origin| layout_input_origins.contains(&origin))
                            || payload_contains_noesc))
                        .then_some(expected_arg_ty)
                        .flatten()
                })
                .or_else(|| {
                    carries_contextual_closure
                        .then_some(expected_arg_ty)
                        .flatten()
                });
            args.push(CallArg::from_hir_arg(
                tc,
                hir_arg,
                span.clone().arg(i),
                already_typed || tc.env.typed_expr(hir_arg.expr).is_some(),
                expected_hint,
                deferred_closure_hint,
                closure_expectations,
            ));
        }

        let body = tc.body();
        let is_unary = |expr: ExprId, op: UnOp| {
            matches!(
                expr.data(db, body),
                Partial::Present(Expr::Un(_, found)) if *found == op
            )
        };

        for (i, (given, expected)) in args.into_iter().zip(expected_arg_tys).enumerate() {
            // Call labels are either explicit (`f(x: value)`) or inferred from a bare
            // identifier argument (`f(x)`), but not from arbitrary expressions (`f(10)`).
            // If the callee parameter has an unsuppressed label, the call must provide
            // one of those labels. Do not disable this check or guard it on explicit
            // syntax only; that would silently allow missing labels.
            if let Some(expected_label) = expected_labels[i]
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

            let expected = tc.normalize_ty(expected);
            let mut expected = tc.env.rewrite_closure_types(expected);
            let mode = arg_modes.as_ref().and_then(|modes| modes.get(i).copied());
            let given_ty = tc.normalize_ty(given.expr_prop.ty);
            let given_ty = tc.env.rewrite_closure_types(given_ty);
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
                tc.try_coerce_capability_for_expr_to_expected(given.expr, given_ty, expected)
                    .unwrap_or(given_ty)
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
            if let Some(modes) = arg_modes.as_ref() {
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
                    *modes.get(i).unwrap_or_else(|| {
                        unreachable!(
                            "missing func param at index {i} — length check above should have caught this"
                        )
                    })
                });
                match mode {
                    FuncParamMode::Own => {
                        if expected.as_borrow(db).is_some() {
                            tc.push_diag(BodyDiag::OwnParamCannotBeBorrow {
                                primary: given.expr_span.clone(),
                                ty: expected,
                            });
                        } else {
                            tc.record_owned_value_use(given.expr, expected);
                        }
                    }
                    FuncParamMode::View => tc.record_expr_value_use(
                        given.expr,
                        ValueAccess::Read,
                        ClosureCaptureAccess::Read,
                    ),
                }
            } else {
                tc.equate_ty(actual, expected, given.expr_span.clone());
                expected = tc.normalize_ty(expected);
                // Variant constructors materialize their fields immediately (owned context).
                tc.record_owned_value_use(given.expr, expected);
            }
        }

        if let Some(closure) = closure_call_ty {
            for (param_idx, arg) in call_args.iter().enumerate() {
                tc.record_contextual_closure_call_param_origins(closure, param_idx, arg.expr);
            }
        }
        *self = self.clone().fold_with(db, &mut tc.table);
        *self = tc.env.rewrite_closure_types(self.clone());
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

fn bind_callable_params_from_actual<'db>(
    db: &'db dyn HirAnalysisDb,
    expected: TyId<'db>,
    actual: TyId<'db>,
    args: &mut [TyId<'db>],
) {
    match expected.data(db) {
        TyData::TyParam(param) => {
            if let Some(arg) = args.get_mut(param.idx) {
                *arg = actual;
            }
            return;
        }
        TyData::ConstTy(const_ty) => {
            if let crate::analysis::ty::const_ty::ConstTyData::TyParam(param, _) = const_ty.data(db)
            {
                if let Some(arg) = args.get_mut(param.idx) {
                    *arg = actual;
                }
                return;
            }
        }
        _ => {}
    }
    if let Some((expected_kind, expected_inner)) = expected.as_capability(db)
        && let Some((actual_kind, actual_inner)) = actual.as_capability(db)
        && expected_kind == actual_kind
    {
        bind_callable_params_from_actual(db, expected_inner, actual_inner, args);
        return;
    }
    let (expected_base, expected_args) = expected.decompose_ty_app(db);
    let (actual_base, actual_args) = actual.decompose_ty_app(db);
    if expected_base == actual_base && expected_args.len() == actual_args.len() {
        for (&expected, &actual) in expected_args.iter().zip(actual_args) {
            bind_callable_params_from_actual(db, expected, actual, args);
        }
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
        deferred_closure_hint: Option<TyId<'db>>,
        closure_expectations: Vec<(TyId<'db>, ClosureExpectation<'db>)>,
    ) -> Self {
        let expr_prop = if !closure_expectations.is_empty() {
            let ty = expected_hint.unwrap_or_else(|| tc.fresh_ty());
            let retry_expectations = (expected_hint.is_none() && deferred_closure_hint.is_some())
                .then(|| closure_expectations.clone());
            let mut prop =
                tc.check_expr_with_closure_type_expectations(arg.expr, ty, closure_expectations);
            if let (Some(expected), Some(expectations)) =
                (deferred_closure_hint, retry_expectations)
                && tc.expr_can_replay_contextual_closure(arg.expr)
            {
                prop =
                    tc.check_expr_with_closure_type_expectations(arg.expr, expected, expectations);
            }
            prop
        } else if already_typed && expected_hint.is_none() {
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
    pub(super) fn process_constraints(
        &self,
        tc: &mut TyChecker<'db>,
        call_expr: ExprId,
        span: DynLazySpan<'db>,
    ) -> bool {
        self.handle_constraints(tc, call_expr, span, true)
    }

    pub(super) fn enqueue_constraints(
        &self,
        tc: &mut TyChecker<'db>,
        call_expr: ExprId,
        span: DynLazySpan<'db>,
    ) {
        self.handle_constraints(tc, call_expr, span, false);
    }

    fn handle_constraints(
        &self,
        tc: &mut TyChecker<'db>,
        call_expr: ExprId,
        span: DynLazySpan<'db>,
        process_immediately: bool,
    ) -> bool {
        let mut progressed = false;
        let db = tc.db;
        let constraints = collect_func_decl_constraints(db, self.callable_def, true);
        let declared = constraints.instantiate_identity();
        let instantiated = constraints.instantiate(db, &self.generic_args);
        let definition_assumptions = match self.callable_def {
            CallableDef::Func(func) => param_env(db, func.into()),
            CallableDef::VariantCtor(_) => declared,
        };
        let definition_solve_cx = TraitSolveCx::new(db, self.callable_def.scope())
            .with_assumptions(definition_assumptions);

        let mut pending_obligations = Vec::new();
        for (constraint_idx, (&constraint, &declared_constraint)) in instantiated
            .list(db)
            .iter()
            .zip(declared.list(db))
            .enumerate()
        {
            // The declaration pass reports ill-formed bounds at their source.
            // Do not turn the same malformed template into a downstream call
            // error merely because candidate signatures are intentionally
            // deferred during lookup.
            if !check_trait_inst_wf(db, definition_solve_cx, declared_constraint).is_wf() {
                continue;
            }
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

            let obligation = super::env::TraitObligation {
                goal: constraint,
                origin: super::env::TraitObligationOrigin::CallConstraint {
                    call_expr,
                    callable_def: self.callable_def,
                    constraint_idx,
                },
                span: span.clone(),
            };

            if !process_immediately {
                pending_obligations.push(obligation);
                continue;
            }

            match tc.process_trait_obligation(obligation, false) {
                TraitObligationOutcome::Discharged => {}
                TraitObligationOutcome::Progressed => progressed = true,
                TraitObligationOutcome::Requeue(obligation) => {
                    pending_obligations.push(obligation);
                }
            };
        }

        let mut round_progressed = progressed;
        while round_progressed && !pending_obligations.is_empty() {
            round_progressed = false;
            for obligation in std::mem::take(&mut pending_obligations) {
                match tc.process_trait_obligation(obligation, false) {
                    TraitObligationOutcome::Discharged => {}
                    TraitObligationOutcome::Progressed => {
                        progressed = true;
                        round_progressed = true;
                    }
                    TraitObligationOutcome::Requeue(obligation) => {
                        pending_obligations.push(obligation);
                    }
                };
            }
        }

        for obligation in pending_obligations {
            tc.env.register_trait_obligation(obligation);
        }
        progressed
    }
}
