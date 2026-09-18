use crate::{
    analysis::{
        HirAnalysisDb,
        place::PlaceBase,
        semantic::{
            Mutability, SEffectArg, SEffectArgValue, SExpr, SOperand, SPlace, SStmtKind, SValueId,
            SemOrigin, provisional_provider_binding_for_instance_effect,
            provisional_provider_idx_for_requirement,
            resolved_provider_binding_for_instance_effect,
        },
        ty::{
            ProviderAddressSpace, ProviderKind,
            effects::EffectKeyKind,
            provider::provider_semantics,
            ty_check::{
                BodyOwner, EffectArg, EffectParamSite, EffectPassMode, LocalBinding,
                ResolvedEffectArg,
            },
        },
    },
    hir_def::ExprId,
    semantic::{
        EffectEnvSite, EffectEnvView, ProviderBinding, resolved_effect_binding_infos_for_site,
    },
};

use super::body::SmirLowerCtxt;

/// A root object is borrowed from this place on each call. Handle values retain
/// their explicit transport and are evaluated once when entering the block.
#[derive(Clone)]
pub(super) enum WithBindingSource<'db> {
    Place(SPlace<'db>),
    Value(SOperand),
}

impl<'a, 'db> SmirLowerCtxt<'a, 'db> {
    pub(super) fn lower_with_expr(
        &mut self,
        bindings: &[crate::hir_def::expr::WithBinding<'db>],
        body: ExprId,
    ) -> SValueId {
        let mut saved = Vec::with_capacity(bindings.len());
        for binding in bindings {
            let value_expr = binding.value;
            let source = if self.is_root_provider_expr(value_expr) {
                let place = if let Some(place) = self.typed_body.expr_place(value_expr) {
                    self.capture_place(place)
                } else {
                    let value = self.lower_expr(value_expr);
                    let local =
                        self.alloc_local(self.expr_ty(value_expr), Mutability::Mutable, None);
                    self.push_stmt(
                        SemOrigin::Expr(value_expr),
                        SStmtKind::Assign {
                            dst: local,
                            expr: SExpr::UseValue(SOperand::expr(value, value_expr)),
                        },
                    );
                    SPlace::new(local)
                };
                WithBindingSource::Place(place)
            } else {
                let value = self.lower_expr(value_expr);
                WithBindingSource::Value(SOperand::expr(value, value_expr))
            };
            saved.push((
                value_expr,
                self.with_binding_sources.insert(value_expr, source),
            ));
        }

        let body_value = self.lower_expr(body);
        for (expr, previous) in saved.into_iter().rev() {
            if let Some(previous) = previous {
                self.with_binding_sources.insert(expr, previous);
            } else {
                self.with_binding_sources.remove(&expr);
            }
        }
        body_value
    }

    fn effect_arg_provider_space(
        &self,
        arg: &ResolvedEffectArg<'db>,
    ) -> Option<ProviderAddressSpace> {
        arg.provider.or_else(|| match &arg.arg {
            EffectArg::Place(place) => {
                let PlaceBase::Binding(binding) = place.base;
                self.binding_provider(binding)
                    .and_then(|provider| provider.semantics.address_space)
            }
            EffectArg::Binding(binding) => self
                .binding_provider(*binding)
                .and_then(|provider| provider.semantics.address_space),
            EffectArg::Value(_) | EffectArg::Unknown => None,
        })
    }

    fn binding_provider(&self, binding: LocalBinding<'db>) -> Option<ProviderBinding<'db>> {
        match self.binding_role_mode {
            super::body::BindingRoleMode::Final => {
                resolved_provider_binding_for_instance_effect(self.db, self.instance, binding)
            }
            super::body::BindingRoleMode::Provisional => {
                provisional_provider_binding_for_instance_effect(self.db, self.instance, binding)
            }
        }
    }

    pub(super) fn lower_effect_arg_slice(
        &mut self,
        args: &[ResolvedEffectArg<'db>],
    ) -> Box<[SEffectArg<'db>]> {
        args.iter().map(|arg| self.lower_effect_arg(arg)).collect()
    }

    fn is_root_provider_expr(&self, expr: ExprId) -> bool {
        if let Some(place) = self.typed_body.expr_place(expr)
            && place.projections.is_empty()
        {
            let PlaceBase::Binding(binding) = place.base;
            if let Some(provider) = self.binding_provider(binding) {
                return provider.semantics.kind == ProviderKind::RootObject;
            }
        }
        provider_semantics(
            self.db,
            self.body.scope(),
            self.assumptions,
            self.expr_ty(expr),
        )
        .kind
            == ProviderKind::RootObject
    }

    fn lower_effect_arg(&mut self, arg: &ResolvedEffectArg<'db>) -> SEffectArg<'db> {
        let source = arg.with_source.map(|expr| {
            self.with_binding_sources
                .get(&expr)
                .expect("effect provider should be captured by its with binding")
                .clone()
        });
        let value = match source {
            Some(WithBindingSource::Place(place)) => SEffectArgValue::Place(place),
            Some(WithBindingSource::Value(value)) => {
                if matches!(
                    arg.pass_mode,
                    EffectPassMode::ByPlace | EffectPassMode::ByTempPlace
                ) {
                    SEffectArgValue::Place(SPlace::new(value.value))
                } else {
                    SEffectArgValue::Value(value)
                }
            }
            None => match &arg.arg {
                EffectArg::Place(place) => SEffectArgValue::Place(self.lower_place_data(place)),
                EffectArg::Binding(binding) => {
                    let local = self.alloc_binding_local(*binding);
                    if matches!(arg.pass_mode, EffectPassMode::ByPlace)
                        || self.binding_provider(*binding).is_some_and(|provider| {
                            provider.semantics.kind == ProviderKind::RootObject
                        })
                    {
                        SEffectArgValue::Place(SPlace::new(local))
                    } else {
                        SEffectArgValue::Value(SOperand::inherited(local))
                    }
                }
                EffectArg::Value(_) => unreachable!("with provider is missing its source"),
                EffectArg::Unknown => {
                    SEffectArgValue::Value(SOperand::synthetic(self.unit_value()))
                }
            },
        };
        SEffectArg {
            binding_idx: arg.binding_idx,
            pass_mode: if matches!(value, SEffectArgValue::Place(_)) {
                EffectPassMode::ByPlace
            } else {
                arg.pass_mode
            },
            arg: value,
            layout_view: arg.layout_view,
            required_mut: arg.required_mut,
            provider_target_ty: arg.provider_target_ty,
            provider: self.effect_arg_provider_space(arg),
        }
    }
}

pub fn owner_effect_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Vec<LocalBinding<'db>> {
    effect_param_site(owner)
        .into_iter()
        .flat_map(|site| {
            resolved_effect_binding_infos_for_site(db, EffectEnvSite::new(db, site))
                .iter()
                .filter_map(|binding| binding.as_ref())
                .filter(|binding| {
                    matches!(
                        binding.requirement.key.kind(),
                        EffectKeyKind::Type | EffectKeyKind::Trait
                    )
                })
                .map(LocalBinding::effect_param)
                .collect::<Vec<_>>()
        })
        .collect()
}

pub(super) fn provisional_owner_effect_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Vec<LocalBinding<'db>> {
    effect_param_site(owner)
        .into_iter()
        .flat_map(|site| {
            EffectEnvView::new(site)
                .requirements(db)
                .into_iter()
                .filter_map(move |requirement| {
                    if !matches!(
                        requirement.key.kind(),
                        EffectKeyKind::Type | EffectKeyKind::Trait
                    ) {
                        return None;
                    }
                    let provider_idx = provisional_provider_idx_for_requirement(
                        db,
                        site,
                        requirement.binding_idx,
                    )?;
                    Some(LocalBinding::EffectParam {
                        site: requirement.binding_site,
                        idx: requirement.binding_idx as usize,
                        binding_name: requirement.binding_name,
                        provider_idx,
                        is_mut: requirement.is_mut,
                    })
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub fn effect_param_site<'db>(owner: BodyOwner<'db>) -> Option<EffectParamSite<'db>> {
    match owner {
        BodyOwner::Func(func) => Some(EffectParamSite::Func(func)),
        BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => None,
        BodyOwner::ContractInit { contract } => Some(EffectParamSite::ContractInit { contract }),
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => Some(EffectParamSite::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        }),
    }
}

pub fn same_owner_effect_binding<'db>(lhs: LocalBinding<'db>, rhs: LocalBinding<'db>) -> bool {
    match (lhs, rhs) {
        (
            LocalBinding::EffectParam {
                site: lhs_site,
                idx: lhs_idx,
                ..
            },
            LocalBinding::EffectParam {
                site: rhs_site,
                idx: rhs_idx,
                ..
            },
        ) => lhs_site == rhs_site && lhs_idx == rhs_idx,
        (
            LocalBinding::Param {
                site: crate::analysis::ty::ty_check::ParamSite::EffectField(_),
                idx: lhs_idx,
                ty: lhs_ty,
                ..
            },
            LocalBinding::Param {
                site: crate::analysis::ty::ty_check::ParamSite::EffectField(_),
                idx: rhs_idx,
                ty: rhs_ty,
                ..
            },
        ) => lhs_idx == rhs_idx && lhs_ty == rhs_ty,
        _ => lhs == rhs,
    }
}
