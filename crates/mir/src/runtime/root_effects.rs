use hir::{
    analysis::{
        semantic::{
            SemanticInstance, owner_effect_bindings, resolved_provider_binding_for_instance_effect,
        },
        ty::{ty_check::LocalBinding, ty_def::TyId},
    },
    hir_def::{Contract, Func},
    semantic::{ContractFieldLayoutInfo, ProviderBinding, ProviderSource},
};
use rustc_hash::FxHashMap;

use crate::{
    db::MirDb,
    runtime::{
        AddressSpaceKind, ContractFieldBinding, EntryEffectArgPlan, RefKind, RefView, RuntimeClass,
        TargetRootProviderBinding, TargetRootProviderMaterialization,
        lower::{
            classify::{provider_erases_runtime_root, runtime_effect_binding_plan},
            type_info::RuntimeTypeEnv,
        },
        package::LowerError,
    },
};

#[derive(Clone, Copy)]
pub(crate) enum EntryEffectContext<'db> {
    StandaloneFunc { func: Func<'db> },
    TestFunc { func: Func<'db> },
    ManualContractRoot { func: Func<'db> },
    HighLevelContract { contract: Contract<'db> },
}

pub(crate) fn entry_effect_arg_plans<'db>(
    db: &'db dyn MirDb,
    context: EntryEffectContext<'db>,
    semantic: SemanticInstance<'db>,
) -> Result<Vec<EntryEffectArgPlan<'db>>, LowerError> {
    let owner = semantic.key(db).owner(db);
    let contract_fields = context.contract().map(|contract| {
        contract
            .field_layout(db)
            .values()
            .cloned()
            .map(|field| (field.index, field))
            .collect::<FxHashMap<_, _>>()
    });
    owner_effect_bindings(db, owner)
        .into_iter()
        .filter_map(|binding| {
            let provider = resolved_provider_binding_for_instance_effect(db, semantic, binding)?;
            Some(entry_effect_arg_plan_for_binding(
                db,
                context,
                semantic,
                binding,
                provider,
                contract_fields.as_ref(),
            ))
        })
        .filter_map(|result| result.transpose())
        .collect()
}

impl<'db> EntryEffectContext<'db> {
    fn contract(self) -> Option<Contract<'db>> {
        match self {
            Self::HighLevelContract { contract } => Some(contract),
            Self::StandaloneFunc { .. }
            | Self::TestFunc { .. }
            | Self::ManualContractRoot { .. } => None,
        }
    }

    fn label(self, db: &'db dyn MirDb) -> String {
        match self {
            Self::StandaloneFunc { func } => {
                format!("standalone root `{}`", func_display_name(db, func))
            }
            Self::TestFunc { func } => format!("test root `{}`", func_display_name(db, func)),
            Self::ManualContractRoot { func } => {
                format!("manual contract root `{}`", func_display_name(db, func))
            }
            Self::HighLevelContract { contract } => {
                format!("contract `{}`", contract_display_name(db, contract))
            }
        }
    }
}

fn entry_effect_arg_plan_for_binding<'db>(
    db: &'db dyn MirDb,
    context: EntryEffectContext<'db>,
    semantic: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
    provider: ProviderBinding<'db>,
    contract_fields: Option<&FxHashMap<u32, ContractFieldLayoutInfo<'db>>>,
) -> Result<Option<EntryEffectArgPlan<'db>>, LowerError> {
    match provider.source.clone() {
        ProviderSource::ContractField { field_idx, .. } => {
            let Some(fields) = contract_fields else {
                return Err(unsupported_entry_effect(
                    db,
                    context,
                    binding,
                    "contract field",
                ));
            };
            let field = fields.get(&field_idx).ok_or_else(|| {
                LowerError::Unsupported(format!(
                    "missing contract field layout for field {field_idx} in {}",
                    context.label(db)
                ))
            })?;
            Ok(Some(EntryEffectArgPlan::ContractField(
                contract_field_binding(db, context, field, semantic, binding)?,
            )))
        }
        ProviderSource::RootProvider { .. } => {
            let Some(plan) = runtime_effect_binding_plan(db, semantic, binding) else {
                return Ok(None);
            };
            let materialization =
                target_root_provider_materialization(&plan.class).ok_or_else(|| {
                    LowerError::Unsupported(format!(
                        "{} cannot synthesize effect binding `{}` because root provider class `{:?}` has no supported entry materialization",
                        context.label(db),
                        binding_display_name(db, binding),
                        plan.class,
                    ))
                })?;
            Ok(Some(EntryEffectArgPlan::TargetRootProvider(
                TargetRootProviderBinding {
                    declared_ty: semantic.binding_ty(db, binding),
                    class: plan.class,
                    materialization,
                },
            )))
        }
        ProviderSource::UsesParam { .. } => {
            let env = RuntimeTypeEnv::for_semantic(db, semantic);
            if runtime_effect_binding_plan(db, semantic, binding).is_none()
                && provider_erases_runtime_root(db, &provider, env.scope, env.assumptions)
            {
                Ok(None)
            } else {
                Err(unsupported_entry_effect(
                    db,
                    context,
                    binding,
                    "ordinary uses parameter",
                ))
            }
        }
    }
}

fn contract_field_binding<'db>(
    db: &'db dyn MirDb,
    context: EntryEffectContext<'db>,
    field: &ContractFieldLayoutInfo<'db>,
    semantic: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
) -> Result<ContractFieldBinding<'db>, LowerError> {
    let binding_ty = semantic.binding_ty(db, binding);
    let class = runtime_effect_binding_plan(db, semantic, binding)
        .map(|plan| plan.class)
        .ok_or_else(|| {
            LowerError::Unsupported(format!(
                "contract field `{}` in {} has no runtime effect binding plan",
                field.name.data(db),
                context.label(db),
            ))
        })?;
    let kind = match &class {
        RuntimeClass::Ref { kind, .. } => kind.clone(),
        RuntimeClass::RawAddr { space, .. } => RefKind::Provider {
            provider_ty: TyId::borrow_ref_of(db, binding_ty),
            space: *space,
        },
        RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. } => {
            return Err(LowerError::Unsupported(format!(
                "contract field `{}` in {} does not lower to a provider-style runtime class",
                field.name.data(db),
                context.label(db),
            )));
        }
    };
    Ok(ContractFieldBinding {
        slot: field.slot_offset as u128,
        declared_ty: binding_ty,
        class,
        kind,
    })
}

pub(crate) fn target_root_provider_materialization<'db>(
    class: &RuntimeClass<'db>,
) -> Option<TargetRootProviderMaterialization<'db>> {
    match class {
        RuntimeClass::RawAddr {
            space: AddressSpaceKind::Memory,
            target: Some(layout),
        } => Some(TargetRootProviderMaterialization::MemoryRawAddr { layout: *layout }),
        RuntimeClass::Ref {
            pointee,
            kind:
                RefKind::Object
                | RefKind::Provider {
                    space: AddressSpaceKind::Memory,
                    ..
                },
            view: RefView::Whole,
        } => pointee
            .aggregate_layout()
            .map(|layout| TargetRootProviderMaterialization::MemoryObject { layout }),
        RuntimeClass::Scalar(_)
        | RuntimeClass::AggregateValue { .. }
        | RuntimeClass::RawAddr { .. }
        | RuntimeClass::Ref { .. } => None,
    }
}

fn unsupported_entry_effect<'db>(
    db: &'db dyn MirDb,
    context: EntryEffectContext<'db>,
    binding: LocalBinding<'db>,
    source: &str,
) -> LowerError {
    LowerError::Unsupported(format!(
        "{} cannot synthesize effect binding `{}` from {source}; entry roots have no caller to supply ordinary effect parameters, so move the effectful logic into a helper and call it with a concrete provider using `with (...)`, or use a contract field/provider context",
        context.label(db),
        binding_display_name(db, binding),
    ))
}

fn binding_display_name<'db>(db: &'db dyn MirDb, binding: LocalBinding<'db>) -> String {
    match binding {
        LocalBinding::EffectParam { binding_name, .. } => binding_name.data(db).to_string(),
        LocalBinding::Local { .. } | LocalBinding::Param { .. } => "<unknown>".to_string(),
    }
}

fn func_display_name<'db>(db: &'db dyn MirDb, func: Func<'db>) -> String {
    func.name(db)
        .to_opt()
        .map(|name| name.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".to_string())
}

fn contract_display_name<'db>(db: &'db dyn MirDb, contract: Contract<'db>) -> String {
    contract
        .name(db)
        .to_opt()
        .map(|name| name.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".to_string())
}
