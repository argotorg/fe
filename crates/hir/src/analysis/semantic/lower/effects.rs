use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            SEffectArg, SEffectArgValue, SOperand, SPlace, SValueId,
            provisional_provider_binding_for_instance_effect,
            provisional_provider_idx_for_requirement,
            resolved_provider_binding_for_instance_effect,
        },
        ty::{
            ProviderAddressSpace,
            effects::EffectKeyKind,
            ty_check::{
                BodyOwner, EffectArg, EffectParamSite, EffectPassMode, LocalBinding,
                ResolvedEffectArg,
            },
        },
    },
    hir_def::ExprId,
    semantic::{EffectEnvSite, EffectEnvView, resolved_effect_binding_infos_for_site},
};

use super::body::SmirLowerCtxt;

impl<'a, 'db> SmirLowerCtxt<'a, 'db> {
    pub(super) fn lower_with_expr(
        &mut self,
        bindings: &[crate::hir_def::expr::WithBinding<'db>],
        body: ExprId,
    ) -> SValueId {
        let mut saved = Vec::with_capacity(bindings.len());
        for binding in bindings {
            let value_expr = binding.value;
            let value = self.lower_expr(value_expr);
            saved.push((
                value_expr,
                self.with_binding_values.insert(value_expr, value),
            ));
        }

        let body_value = self.lower_expr(body);
        for (expr, previous) in saved.into_iter().rev() {
            if let Some(previous) = previous {
                self.with_binding_values.insert(expr, previous);
            } else {
                self.with_binding_values.remove(&expr);
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
                let crate::analysis::place::PlaceBase::Binding(binding) = place.base;
                self.binding_provider_space(binding)
            }
            EffectArg::Binding(binding) => self.binding_provider_space(*binding),
            EffectArg::Value(_) | EffectArg::Unknown => None,
        })
    }

    fn binding_provider_space(&self, binding: LocalBinding<'db>) -> Option<ProviderAddressSpace> {
        match self.binding_role_mode {
            super::body::BindingRoleMode::Final => {
                resolved_provider_binding_for_instance_effect(self.db, self.instance, binding)
            }
            super::body::BindingRoleMode::Provisional => {
                provisional_provider_binding_for_instance_effect(self.db, self.instance, binding)
            }
        }
        .and_then(|provider| provider.semantics.address_space)
    }

    pub(super) fn lower_effect_arg_slice(
        &mut self,
        args: &[ResolvedEffectArg<'db>],
    ) -> Box<[SEffectArg<'db>]> {
        args.iter().map(|arg| self.lower_effect_arg(arg)).collect()
    }

    fn lower_effect_arg(&mut self, arg: &ResolvedEffectArg<'db>) -> SEffectArg<'db> {
        SEffectArg {
            binding_idx: arg.binding_idx,
            arg: match &arg.arg {
                EffectArg::Place(place) => SEffectArgValue::Place(self.lower_place_data(place)),
                EffectArg::Value(expr) => SEffectArgValue::Value(SOperand::expr(
                    self.with_binding_values
                        .get(expr)
                        .copied()
                        .unwrap_or_else(|| self.lower_expr(*expr)),
                    *expr,
                )),
                EffectArg::Binding(binding) => {
                    let local = self.alloc_binding_local(*binding);
                    if matches!(arg.pass_mode, EffectPassMode::ByPlace) {
                        SEffectArgValue::Place(SPlace::new(local))
                    } else {
                        SEffectArgValue::Value(SOperand::inherited(local))
                    }
                }
                EffectArg::Unknown => {
                    SEffectArgValue::Value(SOperand::synthetic(self.unit_value()))
                }
            },
            pass_mode: arg.pass_mode,
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
