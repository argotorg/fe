use hir::{
    analysis::{
        semantic::{
            SemanticInstance, assigned_provider_layout_evidence, owner_effect_bindings,
            resolved_provider_binding_for_instance_effect,
        },
        ty::{
            CallableLayoutParamPort, CallableLayoutPort, ProviderAddressSpace,
            const_ty::CallableInputLayoutHoleOrigin,
            ty_check::{BodyOwner, LocalBinding},
            ty_def::TyId,
        },
    },
    hir_def::{Contract, Func},
    semantic::{ContractFieldId, FieldStorageLayout, ProviderBinding, ProviderSource},
};
use rustc_hash::FxHashMap;

use crate::{
    db::MirDb,
    runtime::{
        AddressSpaceKind, ContractFieldBinding, ContractFieldSlot, EntryEffectArgPlan,
        EntryLayoutEvidenceArgPlan, EntrySemanticArgsPlan, RefKind, RefView, RuntimeClass,
        TargetRootProviderBinding, TargetRootProviderMaterialization,
        lower::{
            classify::{
                provider_erases_runtime_root, provider_source_erases_zero_sized_effect_value,
                runtime_effect_binding_plan,
            },
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

pub(crate) fn entry_semantic_args_plan<'db>(
    db: &'db dyn MirDb,
    context: EntryEffectContext<'db>,
    semantic: SemanticInstance<'db>,
) -> Result<EntrySemanticArgsPlan<'db>, LowerError> {
    let owner = semantic.key(db).owner(db);
    let (contract_fields, total_code_slots) = if let Some(contract) = context.contract() {
        (
            Some(
                contract
                    .storage_layout(db)
                    .values()
                    .cloned()
                    .map(|field| (field.field, field))
                    .collect::<FxHashMap<_, _>>(),
            ),
            contract.code_address_space_slot_count(db),
        )
    } else {
        (None, 0)
    };
    let resolved = owner_effect_bindings(db, owner)
        .into_iter()
        .filter_map(|binding| {
            let provider = resolved_provider_binding_for_instance_effect(db, semantic, binding)?;
            Some((binding, provider))
        })
        .collect::<Vec<_>>();
    let effects = resolved
        .iter()
        .filter_map(|(binding, provider)| {
            entry_effect_arg_plan_for_binding(
                db,
                context,
                semantic,
                *binding,
                provider.clone(),
                contract_fields.as_ref(),
                total_code_slots,
            )
            .transpose()
        })
        .collect::<Result<Vec<_>, _>>()?;
    let signature = semantic.key(db).layout_bundle_signature(db);
    let mut layout_evidence = Vec::new();
    for input in &signature.inputs {
        if input.interface.runtime_descriptor_count() == 0 {
            continue;
        }
        let CallableInputLayoutHoleOrigin::Effect(_) = input.origin else {
            return Err(LowerError::Unsupported(format!(
                "{} cannot synthesize runtime layout evidence for non-effect input {:?}",
                context.label(db),
                input.origin,
            )));
        };
        let providers = resolved
            .iter()
            .filter(|(binding, _)| binding.callable_input_origin(db) == Some(input.origin))
            .map(|(_, provider)| provider)
            .collect::<Vec<_>>();
        let [provider] = providers.as_slice() else {
            return Err(LowerError::Unsupported(format!(
                "{} has no unique provider for runtime layout-evidence input {:?}",
                context.label(db),
                input.origin,
            )));
        };
        let values = assigned_provider_layout_evidence(db, provider, &input.interface.schema)
            .map_err(|error| {
                LowerError::Unsupported(format!(
                    "{} cannot resolve assigned layout evidence for input {:?}: {error:?}",
                    context.label(db),
                    input.origin,
                ))
            })?;
        for (_, component) in input.interface.runtime_components() {
            let Some(value) = values.component(&component.port) else {
                return Err(LowerError::Unsupported(format!(
                    "{} has no assigned value for layout-evidence component {:?}",
                    context.label(db),
                    component.port,
                )));
            };
            layout_evidence.push(EntryLayoutEvidenceArgPlan {
                target: CallableLayoutParamPort::Input(CallableLayoutPort {
                    origin: input.origin,
                    component: component.port.clone(),
                }),
                value: value.clone(),
            });
        }
    }
    if signature.output_witnesses.runtime_descriptor_count() != 0 {
        return Err(LowerError::Unsupported(format!(
            "{} requires caller-supplied output layout witnesses",
            context.label(db),
        )));
    }
    Ok(EntrySemanticArgsPlan {
        effects: effects.into_boxed_slice(),
        layout_evidence: layout_evidence.into_boxed_slice(),
    })
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
    contract_fields: Option<&FxHashMap<ContractFieldId<'db>, FieldStorageLayout<'db>>>,
    total_code_slots: usize,
) -> Result<Option<EntryEffectArgPlan<'db>>, LowerError> {
    match provider.source.clone() {
        ProviderSource::ContractField { field: field_id } => {
            let env = RuntimeTypeEnv::for_semantic(db, semantic);
            if runtime_effect_binding_plan(db, semantic, binding).is_none()
                && provider_erases_runtime_root(db, &provider, env.scope, env.assumptions)
            {
                return Ok(None);
            }
            let Some(fields) = contract_fields else {
                return Err(unsupported_entry_effect(
                    db,
                    context,
                    binding,
                    "contract field",
                ));
            };
            let field = fields.get(&field_id).ok_or_else(|| {
                LowerError::Unsupported(format!(
                    "missing contract field layout for field {} in {}",
                    field_id.index,
                    context.label(db)
                ))
            })?;
            Ok(Some(EntryEffectArgPlan::ContractField(
                contract_field_binding(db, context, field, semantic, binding, total_code_slots)?,
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
            let value_ty = provider.semantics.target_ty.unwrap_or(provider.provider_ty);
            if runtime_effect_binding_plan(db, semantic, binding).is_none()
                && provider_source_erases_zero_sized_effect_value(
                    db,
                    &provider,
                    value_ty,
                    env.scope,
                    env.assumptions,
                )
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
    field: &FieldStorageLayout<'db>,
    semantic: SemanticInstance<'db>,
    binding: LocalBinding<'db>,
    total_code_slots: usize,
) -> Result<ContractFieldBinding<'db>, LowerError> {
    let binding_ty = semantic.binding_ty(db, binding);
    let field_space = field.address_space;
    let init_immutable = matches!(semantic.key(db).owner(db), BodyOwner::ContractInit { .. })
        && field_space == ProviderAddressSpace::Code;
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
    let slot = match &kind {
        RefKind::Provider {
            space: AddressSpaceKind::Code,
            ..
        } => {
            let total_bytes = i128::try_from(total_code_slots)
                .ok()
                .and_then(|slots| slots.checked_mul(32))
                .ok_or_else(|| {
                    LowerError::Unsupported(format!(
                        "code-backed layout extent overflow for field `{}` in {}",
                        field.name.data(db),
                        context.label(db),
                    ))
                })?;
            let field_bytes = i128::try_from(field.slot_offset)
                .ok()
                .and_then(|slots| slots.checked_mul(32))
                .ok_or_else(|| {
                    LowerError::Unsupported(format!(
                        "code-backed field offset overflow for `{}` in {}",
                        field.name.data(db),
                        context.label(db),
                    ))
                })?;
            ContractFieldSlot::CodeTailBytes(field_bytes.checked_sub(total_bytes).ok_or_else(
                || {
                    LowerError::Unsupported(format!(
                        "internal layout error: code-backed field `{}` begins past the validated code-space high-water mark in {}",
                        field.name.data(db),
                        context.label(db),
                    ))
                },
            )?)
        }
        _ => ContractFieldSlot::Words(u128::try_from(field.slot_offset).map_err(|_| {
            LowerError::Unsupported(format!(
                "contract field offset overflow for `{}` in {}",
                field.name.data(db),
                context.label(db),
            ))
        })?),
    };
    Ok(ContractFieldBinding {
        field: field.field,
        slot,
        declared_ty: binding_ty,
        class,
        kind,
        init_immutable,
    })
}

pub(crate) fn target_root_provider_materialization<'db>(
    class: &RuntimeClass<'db>,
) -> Option<TargetRootProviderMaterialization<'db>> {
    match class {
        RuntimeClass::RawAddr {
            space: AddressSpaceKind::Memory,
            pointee: Some(pointee),
        } => match pointee.as_ref() {
            RuntimeClass::AggregateValue { layout } => {
                Some(TargetRootProviderMaterialization::MemoryRawAddr { layout: *layout })
            }
            RuntimeClass::Scalar(_) | RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => {
                None
            }
        },
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
