use crate::{
    hir_def::{
        BinOp, CallArg as HirCallArg, Expr, ExprId, FieldIndex, GenericArgListId, IdentId,
        ItemKind, LitKind, ModeMarker, Partial,
    },
    span::{
        DynLazySpan,
        expr::{LazyCallArgListSpan, LazyCallArgSpan},
        params::LazyGenericArgListSpan,
    },
};
use salsa::SalsaValue;

use super::{
    BodyOwner, EffectParamSite, ExprProp, LocalBinding, TraitObligationOutcome, TyChecker,
    ValuePathRef,
    env::{ClosureExpectation, func_declared_bounds, func_signature},
};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        closure::{CallableTrait, out_ident},
        const_ty::LoweringContext,
        corelib::resolve_lib_func_path,
        diagnostics::{BodyDiag, FuncBodyDiag},
        evidence::{
            EvidenceId, EvidenceList, declared_bound_evidence, instantiated_selection, prove,
        },
        fold::{TyFoldable, TyFolder},
        generic_defaults::{DefaultApplication, GenericArgError},
        normalize::{normalize_ty, normalize_with_trait_evidence},
        shape::Shape,
        trait_def::{AssocBinding, ResolvedMethodInstance, TraitInstId},
        trait_resolution::{
            TraitSolveCx, check_trait_inst_wf, constraint::collect_func_decl_constraints,
        },
        ty_def::{
            BorrowKind, InvalidCause, Kind, ShapeCon, StringFallback, TyBase, TyData, TyFlags,
            TyId, TyVarSort,
        },
        ty_error::emit_invalid_ty_error,
        ty_lower::{CompleteSubst, collect_generic_params, lower_generic_arg_list},
        visitor::{TyVisitable, TyVisitor, collect_flags},
    },
};
use crate::core::semantic::{ProviderBinding, ProviderSource, param_env};
use crate::hir_def::Body;
use crate::hir_def::CallableDef;
use crate::hir_def::params::FuncParamMode;

pub(super) enum CallGenericArgUnifyError<'db> {
    ArityMismatch { given: usize, expected: usize },
    UnificationFailed,
    InvalidArgument(GenericArgError<'db>),
}

#[derive(Clone, Copy)]
pub(super) enum CallGenericArgPhase {
    Probe,
    Check,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub struct EffectProviderSpecialization<'db> {
    pub provider: ProviderBinding<'db>,
    pub provenance: EffectProviderProvenance<'db>,
}

impl<'db> TyVisitable<'db> for ProviderBinding<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.provider_ty.visit_with(visitor);
        self.semantics.provider_ty.visit_with(visitor);
        if let Some(target_ty) = self.semantics.target_ty {
            target_ty.visit_with(visitor);
        }
        match &self.source {
            ProviderSource::UsesParam { .. } | ProviderSource::ContractField { .. } => {}
            ProviderSource::RootProvider { registration, .. } => {
                registration.provider_ty.visit_with(visitor);
            }
        }
    }
}

impl<'db> TyVisitable<'db> for EffectProviderSpecialization<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.provider.visit_with(visitor);
        if let EffectProviderProvenance::Binding { binding, .. } = self.provenance {
            binding.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for ProviderBinding<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        let provider_ty = self.provider_ty.fold_with(db, folder);
        let semantics = crate::analysis::ty::provider::ProviderSemantics {
            provider_ty: self.semantics.provider_ty.fold_with(db, folder),
            target_ty: self.semantics.target_ty.map(|ty| ty.fold_with(db, folder)),
            ..self.semantics
        };
        let source = match self.source {
            ProviderSource::UsesParam {
                site,
                requirement_idx,
            } => ProviderSource::UsesParam {
                site,
                requirement_idx,
            },
            ProviderSource::ContractField { field } => ProviderSource::ContractField { field },
            ProviderSource::RootProvider {
                scope,
                registration,
            } => ProviderSource::RootProvider {
                scope,
                registration: crate::analysis::ty::provider::RootProviderRegistration {
                    provider_ty: registration.provider_ty.fold_with(db, folder),
                    ..registration
                },
            },
        };
        Self {
            provider_ty,
            semantics,
            source,
            ..self
        }
    }
}

impl<'db> TyFoldable<'db> for EffectProviderSpecialization<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
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
            provider: self.provider.fold_with(db, folder),
            provenance,
        }
    }
}

pub(super) fn unify_explicit_call_generic_args<'db>(
    callable: &mut Callable<'db>,
    tc: &mut TyChecker<'db>,
    args: GenericArgListId<'db>,
    phase: CallGenericArgPhase,
    mut unify_arg: impl FnMut(&mut TyChecker<'db>, usize, TyId<'db>, &mut TyId<'db>) -> bool,
) -> Result<(), CallGenericArgUnifyError<'db>> {
    let db = tc.db;
    let lowering_cx = LoweringContext::new();
    let given_args = args.is_given(db).then(|| {
        lower_generic_arg_list(db, args, tc.env.scope(), tc.env.assumptions(), &lowering_cx)
    });
    let given_count = given_args.as_ref().map_or(0, Vec::len);
    let offset = callable.callable_def.offset_to_explicit_params_position(db);
    let explicit_arg_count = callable.generic_args.len() - offset;

    let completed_args = match callable.callable_def {
        CallableDef::Func(func) => {
            let param_set = collect_generic_params(db, func.into());
            let required = param_set.required_explicit_param_count(db);
            if given_args.is_some() && !(required..=explicit_arg_count).contains(&given_count) {
                return Err(CallGenericArgUnifyError::ArityMismatch {
                    given: given_count,
                    expected: if given_count < required {
                        required
                    } else {
                        explicit_arg_count
                    },
                });
            }

            // Without explicit arguments, required parameters stay inferred.
            let provided = given_args
                .as_deref()
                .unwrap_or_else(|| &callable.generic_args[offset..offset + required]);
            param_set
                .complete_args(
                    db,
                    &callable.generic_args[..offset],
                    provided,
                    match phase {
                        CallGenericArgPhase::Probe => {
                            DefaultApplication::StructuralMetadata(&lowering_cx)
                        }
                        CallGenericArgPhase::Check => {
                            DefaultApplication::CheckedMetadata(&lowering_cx)
                        }
                    },
                )
                .map_err(CallGenericArgUnifyError::InvalidArgument)?
        }
        CallableDef::VariantCtor(_) | CallableDef::StructCtor(_) => given_args
            .clone()
            .unwrap_or_else(|| callable.generic_args[offset..].to_vec()),
    };

    if completed_args.len() != explicit_arg_count {
        return Err(CallGenericArgUnifyError::ArityMismatch {
            given: given_count,
            expected: explicit_arg_count,
        });
    }

    for (idx, (completed, current)) in completed_args
        .into_iter()
        .zip(&mut callable.generic_args[offset..])
        .enumerate()
    {
        if idx >= given_count {
            *current = completed;
        } else if !unify_arg(tc, idx, completed, current) {
            return Err(CallGenericArgUnifyError::UnificationFailed);
        }
    }

    Ok(())
}

/// A parameter of a call: its label, and the shape of the argument it takes,
/// an owned value, a view or `mut` access, or a product's parts.
#[derive(Debug, Clone)]
pub struct CallParam<'db> {
    pub label: Option<IdentId<'db>>,
    pub shape: Shape<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub struct Callable<'db> {
    pub callable_def: CallableDef<'db>,
    base_ty: TyId<'db>,
    generic_args: Vec<TyId<'db>>,
    effect_providers: Vec<EffectProviderSpecialization<'db>>,
    checked_input_tys: Option<Vec<TyId<'db>>>,
    /// The originating trait instance if this callable comes from a trait method
    /// (e.g., operator overloading, method call, indexing). None for inherent functions.
    pub trait_inst: Option<TraitInstId<'db>>,
    /// The checker's proof of `trait_inst` in the checked body. A goal
    /// proved by an assumption, a bound of that body, is selected again where
    /// the body is instantiated.
    evidence: Option<EvidenceId<'db>>,
    /// Proofs, in the checked body, of the assumptions the callee's body is
    /// checked under (`func_signature`): the evidence the callee's instance
    /// receives.
    bound_evidence: Option<EvidenceList<'db>>,
    /// The mode a `mut?` callee is called at: `ref` unless a use of the
    /// call's result asks for `mut`.
    mode: Option<BorrowKind>,
}

impl<'db> TyVisitable<'db> for Callable<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.generic_args.visit_with(visitor);
        self.effect_providers.visit_with(visitor);
        if let Some(inputs) = &self.checked_input_tys {
            inputs.visit_with(visitor);
        }
        if let Some(inst) = self.trait_inst {
            inst.visit_with(visitor);
        }
        if let Some(evidence) = &self.evidence {
            evidence.visit_with(visitor);
        }
        if let Some(bound_evidence) = &self.bound_evidence {
            bound_evidence.visit_with(visitor);
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
            checked_input_tys: self
                .checked_input_tys
                .map(|inputs| inputs.fold_with(db, folder)),
            trait_inst: self.trait_inst.map(|i| i.fold_with(db, folder)),
            evidence: self.evidence.map(|evidence| evidence.fold_with(db, folder)),
            bound_evidence: self
                .bound_evidence
                .map(|bound_evidence| bound_evidence.fold_with(db, folder)),
            mode: self.mode,
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
        // Function items retain their trait arguments in the type even when
        // stored in a local; reconstruct the witness needed for later dispatch.
        let trait_inst = trait_inst.or_else(|| {
            let CallableDef::Func(func) = callable_def else {
                return None;
            };
            let ItemKind::Trait(trait_) = func.scope().parent_item(db)? else {
                return None;
            };
            let trait_arg_count = trait_.params(db).len();
            (args.len() >= trait_arg_count)
                .then(|| TraitInstId::new_simple(db, trait_, args[..trait_arg_count].to_vec()))
        });

        Ok(Self {
            callable_def,
            base_ty: base,
            generic_args: args.to_vec(),
            effect_providers: Vec::new(),
            checked_input_tys: None,
            trait_inst,
            evidence: None,
            bound_evidence: None,
            mode: callable_def.modes(db)[0],
        })
    }

    /// A completed function item: `generic_args` covers the item's full schema.
    pub(crate) fn from_item(
        db: &'db dyn HirAnalysisDb,
        callable_def: CallableDef<'db>,
        generic_args: Vec<TyId<'db>>,
        trait_inst: Option<TraitInstId<'db>>,
    ) -> Self {
        Self {
            callable_def,
            base_ty: TyId::func(db, callable_def),
            generic_args,
            effect_providers: Vec::new(),
            checked_input_tys: None,
            trait_inst,
            evidence: None,
            bound_evidence: None,
            mode: callable_def.modes(db)[0],
        }
    }

    pub fn generic_args(&self) -> &[TyId<'db>] {
        &self.generic_args
    }

    pub fn checked_input_tys(&self) -> Option<&[TyId<'db>]> {
        self.checked_input_tys.as_deref()
    }

    pub(super) fn set_checked_input_tys(&mut self, inputs: Vec<TyId<'db>>) {
        self.checked_input_tys = Some(inputs);
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

    /// Proves the call's trait goal in the checked body's context.
    pub(crate) fn record_evidence(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        solve_cx: TraitSolveCx<'db>,
    ) {
        self.evidence = self.trait_inst.and_then(|inst| prove(db, solve_cx, inst));
    }

    /// The checker's proof of the call's trait goal.
    pub fn evidence(&self) -> Option<EvidenceId<'db>> {
        self.evidence
    }

    /// Proves, in the checked body's context `solve_cx`, the bounds the
    /// callee's body declares, instantiated for this call, as
    /// `declared_bound_evidence` records them.
    pub(crate) fn record_bound_evidence(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        solve_cx: TraitSolveCx<'db>,
    ) {
        let CallableDef::Func(func) = self.callable_def else {
            return;
        };
        let mode = BodyOwner::func_at(db, func, self.mode).mode();
        let Ok(subst) = CompleteSubst::for_owner(db, func.into(), self.generic_args.clone()) else {
            return;
        };
        self.bound_evidence = Some(declared_bound_evidence(
            db,
            func_declared_bounds(db, func, mode),
            func_signature(db, func, mode),
            &subst,
            solve_cx,
        ));
    }

    /// Missing dictionaries cannot be reconstructed ordinarily after a lexical
    /// choice has committed the call, including conflicting supertrait projections.
    pub(crate) fn missing_bound_evidence(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<TraitInstId<'db>> {
        let CallableDef::Func(func) = self.callable_def else {
            return Vec::new();
        };
        let Some(evidence) = self.bound_evidence else {
            return Vec::new();
        };
        let Ok(subst) = CompleteSubst::for_owner(db, func.into(), self.generic_args.clone()) else {
            return Vec::new();
        };
        let mode = BodyOwner::func_at(db, func, self.mode).mode();
        func_signature(db, func, mode)
            .list(db)
            .iter()
            .enumerate()
            .filter_map(|(index, &bound)| {
                if evidence.get(db, index).is_some() {
                    return None;
                }
                let goal = super::super::subst::substitute_complete(db, bound, &subst).ok()?;
                // A provider that is still schematic is resolved when the
                // caller is instantiated. Concrete provider goals must obey
                // the same selected-dictionary requirements as other bounds.
                if super::super::visitor::mentions_effect_provider(db, goal) {
                    return None;
                }
                (!super::super::visitor::collect_flags(db, goal).intersects(
                    super::super::ty_def::TyFlags::HAS_VAR
                        | super::super::ty_def::TyFlags::HAS_INVALID,
                ))
                .then_some(goal)
            })
            .collect()
    }

    /// Proofs of the assumptions the callee's body is checked under, as the
    /// caller's checked body gives them.
    pub fn bound_evidence(&self) -> Option<EvidenceList<'db>> {
        self.bound_evidence
    }

    /// Replaces the assumptions this call's evidence names, its caller's,
    /// with `bound`'s proofs of them.
    pub(super) fn bind_evidence(&mut self, db: &'db dyn HirAnalysisDb, bound: EvidenceList<'db>) {
        self.evidence = self.evidence.map(|evidence| evidence.bind(db, bound));
        self.bound_evidence = self
            .bound_evidence
            .map(|bound_evidence| bound_evidence.bind(db, bound));
    }

    /// The trait method the call runs, with its body: from the
    /// implementation the checker selected, or, for a call the checker
    /// dispatched through an assumption, the one `solve_cx` selects for the
    /// instantiated goal. Absent for a call of no trait method.
    pub fn selected_method(
        &self,
        db: &'db dyn HirAnalysisDb,
        solve_cx: TraitSolveCx<'db>,
    ) -> Option<ResolvedMethodInstance<'db>> {
        let inst = self.trait_inst?;
        let CallableDef::Func(func) = self.callable_def else {
            return None;
        };
        let name = func.name(db).to_opt()?;
        instantiated_selection(db, self.evidence, solve_cx, inst)?.method(db, name, self.mode)
    }

    /// The mode a `mut?` callee is called at; `None` for any other.
    pub fn mode(&self) -> Option<BorrowKind> {
        self.mode
    }

    /// Calls the callee at `mode`. A provider its effects resolved to the
    /// callee's own slot names that slot at `mode`; a provider a recursive
    /// call passes on from the caller's own effect stays the caller's.
    pub(super) fn set_mode(&mut self, mode: Option<BorrowKind>) {
        if let CallableDef::Func(func) = self.callable_def {
            let slot_site = EffectParamSite::Func(func, self.mode);
            for specialization in &mut self.effect_providers {
                let forwarded = |site, requirement_idx| {
                    matches!(
                        specialization.provenance,
                        EffectProviderProvenance::Binding {
                            binding: LocalBinding::EffectParam { site: own, idx, .. },
                            ..
                        } if own == site && idx == requirement_idx as usize
                    )
                };
                if let ProviderSource::UsesParam {
                    site,
                    requirement_idx,
                } = specialization.provider.source
                    && site == slot_site
                    && !forwarded(site, requirement_idx)
                {
                    specialization.provider.source = ProviderSource::UsesParam {
                        site: EffectParamSite::Func(func, mode),
                        requirement_idx,
                    };
                }
            }
        }
        self.mode = mode;
    }

    fn normalize_with_trait_evidence<T>(&self, db: &'db dyn HirAnalysisDb, value: T) -> T
    where
        T: TyFoldable<'db>,
    {
        if let Some(inst) = self.trait_inst {
            normalize_with_trait_evidence(db, value, self.callable_def.scope(), inst)
        } else {
            value
        }
    }

    pub fn ret_ty(&self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let ret = self
            .callable_def
            .ret_ty(db)
            .instantiate(db, &self.generic_args);
        self.normalize_with_trait_evidence(db, ret)
    }

    /// The instantiated return shape when the callee is a projection.
    pub fn ret_shape(&self, db: &'db dyn HirAnalysisDb) -> Option<Shape<'db>> {
        let shape = self
            .callable_def
            .ret_shape(db, self.mode)?
            .instantiate(db, &self.generic_args);
        Some(self.normalize_with_trait_evidence(db, shape))
    }

    /// The mode and type of each input the call passes: each parameter, or
    /// each part of a product parameter.
    pub fn input_modes(&self, db: &'db dyn HirAnalysisDb) -> Vec<(FuncParamMode, TyId<'db>)> {
        self.params(db)
            .unwrap_or_default()
            .iter()
            .flat_map(|param| param.shape.part_modes(db))
            .collect()
    }

    /// The parameters the call passes arguments to, at its mode.
    pub fn params(&self, db: &'db dyn HirAnalysisDb) -> Option<Vec<CallParam<'db>>> {
        self.params_resolving(db, |shape| shape).ok()
    }

    /// The parameters the call passes arguments to, at its mode: those the
    /// callee declares, with a spread expanded into those its instantiated
    /// shape lists once `resolve` resolves it. The shape is the error if it
    /// is no parameter list.
    pub(super) fn params_resolving(
        &self,
        db: &'db dyn HirAnalysisDb,
        resolve: impl FnOnce(TyId<'db>) -> TyId<'db>,
    ) -> Result<Vec<CallParam<'db>>, TyId<'db>> {
        let def = self.callable_def;
        let (declared, spread) = def.param_shapes(db, self.mode);
        let mut params: Vec<_> = declared
            .into_iter()
            .enumerate()
            .map(|(idx, shape)| CallParam {
                label: def.param_label(db, idx),
                shape: self
                    .normalize_with_trait_evidence(db, shape.instantiate(db, &self.generic_args)),
            })
            .collect();
        if let Some(spread) = spread {
            let shape = spread.instantiate(db, &self.generic_args);
            let shape = resolve(self.normalize_with_trait_evidence(db, shape));
            let listed = shape.as_params_shape(db).ok_or(shape)?;
            params.extend(
                listed
                    .into_iter()
                    .map(|shape| CallParam { label: None, shape }),
            );
        }
        Ok(params)
    }

    pub fn ty(&self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        let ty = TyId::foldl(db, self.base_ty, &self.generic_args);
        self.normalize_with_trait_evidence(db, ty)
    }

    pub(super) fn unify_generic_args(
        &mut self,
        tc: &mut TyChecker<'db>,
        args: GenericArgListId<'db>,
        span: LazyGenericArgListSpan<'db>,
    ) -> bool {
        match unify_explicit_call_generic_args(
            self,
            tc,
            args,
            CallGenericArgPhase::Check,
            |tc, idx, given, current| {
                *current = tc.equate_ty(given, *current, span.clone().arg(idx).into());
                true
            },
        ) {
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
            Err(CallGenericArgUnifyError::InvalidArgument(error)) => {
                // Invalid declarations are diagnosed once, at their default.
                if error.diagnostic_owner.is_none() {
                    let error_span = if error.from_default {
                        span.into()
                    } else {
                        span.arg(error.index).into()
                    };
                    if let Some(diag) =
                        emit_invalid_ty_error(tc.db, TyId::invalid(tc.db, error.cause), error_span)
                    {
                        tc.push_diag(diag);
                    }
                }
                false
            }
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

        let Some(params) = tc.call_params(self, span.clone().into()) else {
            return;
        };
        let expected_arity = params.len();
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
            let arg_idx = if has_receiver { i + 1 } else { i };
            // A variant or tuple struct constructor's arguments see its field
            // types, like a struct literal's fields. Keep this contextual typing
            // selective: applying it to every call argument changes ordinary
            // inference and compile-time evaluation.
            let param_ty = params[arg_idx].shape.erased_ty(db);
            let expected_hint = self
                .compile_time_string_literal_arg_expected(tc, hir_arg.expr, param_ty)
                .or_else(|| {
                    matches!(
                        self.callable_def,
                        CallableDef::VariantCtor(_) | CallableDef::StructCtor(_)
                    )
                    .then(|| tc.normalize_ty(param_ty))
                });
            if !already_typed
                && matches!(
                    hir_arg.expr.data(db, tc.body()),
                    Partial::Present(Expr::Closure { .. })
                )
            {
                // A closure's unannotated parameters take their types from the
                // arguments before it (`fold(0, |acc, x| acc + x)`); a mismatch
                // is reported below.
                for (arg, param) in args.iter().zip(&params).skip(usize::from(has_receiver)) {
                    let (given, expected) = (
                        tc.normalize_ty(arg.expr_prop.ty),
                        tc.normalize_ty(param.shape.erased_ty(db)),
                    );
                    let _ = tc.table.unify(given, expected);
                }
                if let Some(expectation) = self.closure_arg_expectation(tc, param_ty) {
                    tc.env.expect_closure(hir_arg.expr, expectation);
                }
            }
            args.push(CallArg::from_hir_arg(
                tc,
                hir_arg,
                span.clone().arg(i),
                already_typed,
                expected_hint,
            ));
        }

        let mut checked_inputs = Vec::with_capacity(expected_arity);
        for (i, (mut given, param)) in args.into_iter().zip(params).enumerate() {
            // Call labels are either explicit (`f(x: value)`) or inferred from a bare
            // identifier argument (`f(x)`), but not from arbitrary expressions (`f(10)`).
            // If the callee parameter has an unsuppressed label, the call must provide
            // one of those labels. Do not disable this check or guard it on explicit
            // syntax only; that would silently allow missing labels.
            if let Some(expected_label) = param.label
                && !expected_label.is_self(db)
                && Some(expected_label) != given.label
            {
                let diag = BodyDiag::CallArgLabelMismatch {
                    primary: given
                        .label_span
                        .clone()
                        .unwrap_or_else(|| given.expr_span.clone()),
                    def_span: self.callable_def.name_span(),
                    given: given.label,
                    expected: expected_label,
                };
                tc.push_diag(diag);
            }

            // A concrete caller knows a shape the callee names opaquely:
            // `take<Pair>(p.at())` passes `Pair`'s step.
            let shape = param.shape.map_tys(db, &mut |ty| tc.normalize_ty(ty));
            let expected = tc.normalize_ty(shape.erased_ty(db));
            let given_ty = tc.normalize_ty(given.expr_prop.ty);
            let given_string_source_ty = given.expr_prop.binding.map_or(given_ty, |binding| {
                tc.normalize_ty(tc.env.lookup_binding_ty(&binding))
            });
            let mut actual = if matches!(self.callable_def, CallableDef::Func(func) if func.is_const(db))
                && expected.is_ty_var(db)
                && let TyData::TyVar(var) = given_string_source_ty.base_ty(db).data(db)
                && let TyVarSort::String {
                    min_len,
                    fallback: StringFallback::Fixed,
                } = var.sort
            {
                let fixed_string_ty = TyId::string_with_len(db, min_len);
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
                given.expr_prop.ty
            };

            let is_receiver = has_receiver && i == 0;
            let diagnosed = check_param_arg(tc, &mut given, &shape, is_receiver);

            if !diagnosed {
                actual = tc.equate_ty(actual, expected, given.expr_span.clone());
            }
            checked_inputs.push(tc.normalize_ty(actual));
        }
        self.checked_input_tys = Some(checked_inputs);
    }

    /// The type of a string literal `expr` passed to a parameter of type
    /// `expected`, when it is a byte array.
    fn compile_time_string_literal_arg_expected(
        &self,
        tc: &mut TyChecker<'db>,
        expr: ExprId,
        expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        let string_id = match expr.data(tc.db, tc.body()) {
            Partial::Present(Expr::Lit(LitKind::String(string_id))) => *string_id,
            Partial::Present(Expr::Tuple(_))
                if self
                    .callable_def
                    .accepts_compile_time_string_literal_bytes(tc.db, tc.env.scope()) =>
            {
                return string_literal_tuple_bytes_hint(tc, expr);
            }
            _ => return None,
        };

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

/// Types the string literals of a (possibly nested) tuple argument as byte
/// arrays, like a string literal passed directly to a compile-time byte
/// consumer such as `core::keccak`. This keeps literals longer than the inline
/// `String` capacity usable as tuple parts. Other elements get fresh variables.
fn string_literal_tuple_bytes_hint<'db>(
    tc: &mut TyChecker<'db>,
    expr: ExprId,
) -> Option<TyId<'db>> {
    let elems = match expr.data(tc.db, tc.body()) {
        Partial::Present(Expr::Lit(LitKind::String(string_id))) => {
            return Some(tc.string_literal_byte_array_ty(string_id.len_bytes(tc.db)));
        }
        Partial::Present(Expr::Tuple(elems)) => elems.clone(),
        _ => return None,
    };
    let hints: Vec<_> = elems
        .iter()
        .map(|elem| string_literal_tuple_bytes_hint(tc, *elem))
        .collect();
    if hints.iter().all(Option::is_none) {
        return None;
    }
    let elem_tys: Vec<_> = hints
        .into_iter()
        .map(|hint| hint.unwrap_or_else(|| tc.fresh_ty()))
        .collect();
    Some(TyId::tuple_with_elems(tc.db, &elem_tys))
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

pub(super) fn place_borrow_suggestion<'db>(
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
        Partial::Present(Expr::Try(inner)) => {
            Some(format!("{}?", place_expr_hint(db, body, *inner)?))
        }
        Partial::Present(Expr::MethodCall(
            receiver,
            Partial::Present(name),
            generic_args,
            args,
        )) if generic_args.is_empty(db) => {
            let receiver = place_expr_hint(db, body, *receiver)?;
            let args = args
                .iter()
                .map(|arg| {
                    let value = expr_hint(db, body, arg.expr)?;
                    Some(match arg.label {
                        Some(label) => format!("{}: {value}", label.data(db)),
                        None => value,
                    })
                })
                .collect::<Option<Vec<_>>>()?;
            Some(format!("{receiver}.{}({})", name.data(db), args.join(", ")))
        }
        _ => None,
    }
}

fn expr_hint<'db>(db: &'db dyn HirAnalysisDb, body: Body<'db>, expr: ExprId) -> Option<String> {
    match expr.data(db, body) {
        Partial::Present(Expr::Path(Partial::Present(path))) => Some(path.pretty_print(db)),
        Partial::Present(Expr::Lit(lit)) => Some(lit.pretty_print(db)),
        _ => None,
    }
}

/// Checks `arg` against a parameter of `mode`, or the receiver: a view or
/// `mut` parameter accesses the caller's place. Returns whether it reported
/// an error.
fn check_arg_mode<'db>(
    tc: &mut TyChecker<'db>,
    arg: &mut CallArg<'db>,
    mode: FuncParamMode,
    is_receiver: bool,
) -> bool {
    let db = tc.db;
    // A view or `mut` parameter accesses the caller's place for the
    // call. An argument that is already an access (`ref p`, `mut p`, a
    // projection's result) is used as one; a receiver is accessed
    // implicitly; any other `mut` argument must be written `mut p`.
    // A `mut self` receiver needs a mutable place.
    if is_receiver
        && mode == FuncParamMode::Mut
        && !arg.expr_prop.is_mut
        && tc.select_mut_place(arg.expr)
    {
        arg.expr_prop = tc
            .env
            .typed_expr(arg.expr)
            .expect("selected place is typed");
    }
    // A `mut?` call passed to a `mut` parameter is called at `mut`.
    if !is_receiver
        && mode == FuncParamMode::Mut
        && arg.expr_prop.access() == Some(BorrowKind::Ref)
        && tc.select_mut_place(arg.expr)
    {
        arg.expr_prop = tc.env.typed_expr(arg.expr).expect("selected call is typed");
    }
    let access = arg.expr_prop.access();
    let mut diagnosed = false;
    match mode {
        FuncParamMode::Own => {}
        FuncParamMode::View => {
            if access.is_some() {
                tc.consume_access(arg.expr);
            }
        }
        // A projection's `mut` result is marked `mut` like a place,
        // unless its type has no values, so the type says it is an
        // access (`fill(buf.span())`).
        FuncParamMode::Mut
            if access == Some(BorrowKind::Mut)
                && (is_receiver
                    || tc.is_named_access(arg.expr)
                    || !arg.expr_prop.ty.has_value_reading(db)) =>
        {
            tc.consume_access(arg.expr);
        }
        FuncParamMode::Mut if access == Some(BorrowKind::Mut) => {
            tc.consume_access(arg.expr);
            tc.push_diag(BodyDiag::ExplicitBorrowRequired {
                primary: arg.expr_span.clone(),
                kind: BorrowKind::Mut,
                suggestion: place_borrow_suggestion(db, tc.body(), arg.expr, BorrowKind::Mut),
                split: None,
            });
            diagnosed = true;
        }
        FuncParamMode::Mut if is_receiver && access.is_none() => {
            if !arg.expr_prop.is_mut && tc.env.is_place_expr(arg.expr) {
                tc.report_cannot_borrow_mut(arg.expr, arg.expr_span.clone());
                diagnosed = true;
            }
        }
        // A closure literal is a temporary its callee may mutate:
        // its captures are its own, read-only. A function item named
        // by its path is a temporary that holds nothing.
        FuncParamMode::Mut
            if matches!(
                arg.expr.data(db, tc.body()),
                Partial::Present(Expr::Closure { .. })
            ) || tc.env.value_path_ref(arg.expr) == Some(ValuePathRef::FunctionItem) => {}
        FuncParamMode::Mut => {
            diagnosed = true;
            if access.is_some() {
                tc.consume_access(arg.expr);
                tc.report_cannot_borrow_mut(arg.expr, arg.expr_span.clone());
            } else if arg.expr_prop.ty.has_invalid(db) {
                // An invalid argument, such as `mut x` of an immutable `x`,
                // is reported already.
            } else if tc.env.is_place_expr(arg.expr) {
                tc.push_diag(BodyDiag::ExplicitBorrowRequired {
                    primary: arg.expr_span.clone(),
                    kind: BorrowKind::Mut,
                    suggestion: place_borrow_suggestion(db, tc.body(), arg.expr, BorrowKind::Mut),
                    split: None,
                });
            } else {
                tc.push_diag(BodyDiag::BorrowArgMustBePlace {
                    primary: arg.expr_span.clone(),
                    kind: BorrowKind::Mut,
                    split: None,
                });
            }
        }
    }
    diagnosed
}

/// Checks `arg` against a parameter of `shape`, or the receiver. Returns
/// whether it reported an error.
fn check_param_arg<'db>(
    tc: &mut TyChecker<'db>,
    arg: &mut CallArg<'db>,
    shape: &Shape<'db>,
    is_receiver: bool,
) -> bool {
    if let Some(mode) = shape.param_mode() {
        return check_arg_mode(tc, arg, mode, is_receiver);
    }
    let Shape::Opaque(required) = shape else {
        return check_product_arg(tc, arg, shape);
    };
    // An opaque shape takes a value of it at its mode, which a value
    // granting it is: a `mut?` call is asked for `mut` if the shape is.
    if tc.grants_product(arg.expr, shape) {
        tc.consume_access(arg.expr);
        return false;
    }
    // A value of the shape at another mode.
    let Some(Shape::Opaque(given)) = tc.env.typed_expr(arg.expr).and_then(|prop| prop.shape) else {
        return false;
    };
    tc.consume_access(arg.expr);
    let db = tc.db;
    tc.push_diag(BodyDiag::TypeMismatch {
        span: arg.expr_span.clone(),
        expected: TyId::shape_part(db, ShapeCon::Value, *required),
        given: TyId::shape_part(db, ShapeCon::Value, given),
    });
    true
}

/// Checks `arg`, passed to a product parameter of `product`: a tuple whose
/// elements are passed to the parts as arguments of the parts' modes, or a
/// projection's result granting the parts. Returns whether it reported an
/// error.
fn check_product_arg<'db>(
    tc: &mut TyChecker<'db>,
    arg: &CallArg<'db>,
    product: &Shape<'db>,
) -> bool {
    let db = tc.db;
    let Shape::Tuple(parts) = product else {
        unreachable!("a product is a tuple of parts")
    };
    if let Partial::Present(Expr::Tuple(elems)) = arg.expr.data(db, tc.body())
        && elems.len() == parts.len()
    {
        tc.env.record_product_arg(arg.expr);
        let mut diagnosed = false;
        for (&elem, part) in elems.iter().zip(parts) {
            let prop = tc
                .env
                .typed_expr(elem)
                .unwrap_or_else(|| ExprProp::invalid(db));
            let mut elem = CallArg::new(None, elem, prop, None, elem.span(tc.body()).into());
            diagnosed |= check_param_arg(tc, &mut elem, part, false);
        }
        return diagnosed;
    }
    if tc.grants_product(arg.expr, product) {
        tc.consume_access(arg.expr);
        return false;
    }
    tc.report_product_arg(arg.expr, product, false);
    true
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

impl<'db> TyChecker<'db> {
    /// The parameters `callable` passes arguments to, or `None` after
    /// reporting at `span` a spread whose shape lists no known parameters.
    pub(super) fn call_params(
        &mut self,
        callable: &Callable<'db>,
        span: DynLazySpan<'db>,
    ) -> Option<Vec<CallParam<'db>>> {
        let db = self.db;
        callable
            .params_resolving(db, |shape| self.normalize_ty(shape))
            .map_err(|shape| {
                if *shape.kind(db) == Kind::Shape && !shape.has_invalid(db) {
                    self.push_diag(BodyDiag::SpreadNotKnown {
                        primary: span,
                        def_span: callable.callable_def.name_span(),
                        shape,
                    });
                    return;
                }
                // A spread of no shape is reported where it is declared. The
                // call's arguments go unchecked, so what they would infer is
                // unknown too.
                let invalid = TyId::invalid(db, InvalidCause::Other);
                for &arg in callable.generic_args() {
                    if self.table.fold_ty(db, arg).is_ty_var(db) {
                        let _ = self.table.unify(arg, invalid);
                    }
                }
            })
            .ok()
    }
}

impl<'db> Callable<'db> {
    /// The callable shape expected of a closure literal passed to a
    /// parameter of type `param_ty`: a callable trait bound on that type
    /// whose shape lists its parameters.
    fn closure_arg_expectation(
        &self,
        tc: &mut TyChecker<'db>,
        param_ty: TyId<'db>,
    ) -> Option<ClosureExpectation<'db>> {
        let db = tc.db;
        let param_ty = tc.normalize_ty(param_ty);
        collect_func_decl_constraints(db, self.callable_def, true)
            .instantiate(db, &self.generic_args)
            .list(db)
            .iter()
            .find_map(|&constraint| {
                let constraint = self.normalize_with_trait_evidence(db, constraint);
                CallableTrait::of(db, constraint.def(db))?;
                if tc.normalize_ty(constraint.self_ty(db)) != param_ty {
                    return None;
                }
                let shape = tc.normalize_ty(*constraint.args(db).get(1)?);
                Some(ClosureExpectation {
                    params: shape.as_params_shape(db)?,
                    ret: constraint.bound_assoc_ty(db, AssocBinding::of(out_ident(db))),
                })
            })
    }

    pub(super) fn process_constraints(
        &self,
        tc: &mut TyChecker<'db>,
        call_expr: ExprId,
        span: DynLazySpan<'db>,
    ) -> bool {
        self.process_constraints_where(tc, call_expr, span, |_| true)
    }

    /// Checks the callee's `mut?` bounds again, after the call is called at
    /// `mut`.
    pub(super) fn process_maybe_mut_constraints(
        &self,
        tc: &mut TyChecker<'db>,
        call_expr: ExprId,
        span: DynLazySpan<'db>,
    ) -> bool {
        self.process_constraints_where(tc, call_expr, span, |declared| {
            declared == ModeMarker::MaybeMut
        })
    }

    /// Checks the callee's bounds, at the call's mode, whose declared mode
    /// `keep` accepts.
    fn process_constraints_where(
        &self,
        tc: &mut TyChecker<'db>,
        call_expr: ExprId,
        span: DynLazySpan<'db>,
        keep: impl Fn(ModeMarker) -> bool,
    ) -> bool {
        let mut progressed = false;
        let db = tc.db;
        let constraints = collect_func_decl_constraints(db, self.callable_def, true);
        let declared = constraints.instantiate_identity();
        let instantiated = constraints.instantiate(db, &self.generic_args);
        let definition_assumptions = match self.callable_def {
            CallableDef::Func(func) => param_env(db, func.into()),
            CallableDef::VariantCtor(_) | CallableDef::StructCtor(_) => declared,
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
            if !keep(declared_constraint.mode(db))
                || !check_trait_inst_wf(db, definition_solve_cx, declared_constraint).is_wf()
            {
                continue;
            }
            let constraint = constraint.at_mode(db, self.mode);
            let constraint = self.normalize_with_trait_evidence(db, constraint);
            let constraint = tc.normalize_trait_goal(constraint);
            if collect_flags(db, constraint).contains(TyFlags::HAS_INVALID) {
                continue;
            }

            let obligation = super::env::TraitObligation {
                choices: tc.env.choices,
                goal: constraint,
                origin: super::env::TraitObligationOrigin::CallConstraint {
                    call_expr,
                    callable_def: self.callable_def,
                    constraint_idx,
                },
                span: span.clone(),
            };

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
