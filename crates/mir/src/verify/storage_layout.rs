//! Independent checks at the authoritative HIR layout to runtime-entry seam.
//!
//! HIR owns allocation, overlap, and address-space high-water validation. MIR
//! entry planning copies each allocated field location while independently
//! deriving its runtime transport class. This module verifies only that the
//! copied plan still describes the source field selected by the semantic
//! callee; it deliberately does not recreate HIR's allocation model.

use hir::{
    analysis::semantic::{owner_effect_bindings, resolved_provider_binding_for_instance_effect},
    analysis::ty::{ProviderAddressSpace, ty_def::TyId},
    hir_def::Contract,
    semantic::{ContractFieldId, FieldStorageLayout, ProviderSource},
};

use crate::{
    db::MirDb,
    instance::RuntimeInstance,
    runtime::{
        AddressSpaceKind, ContractFieldBinding, ContractFieldSlot, EntryEffectArgPlan, RefKind,
        RefView, RuntimeClass, RuntimeFunctionOwner, RuntimeSyntheticSpec,
        lower::{
            classify::runtime_effect_binding_plan, type_info::provider_address_space_to_runtime,
        },
    },
    verify::VerifyError,
};

pub(super) fn verify_contract_storage_seam<'db>(
    db: &'db dyn MirDb,
    owner: &RuntimeFunctionOwner<'db>,
) -> Result<(), VerifyError<'db>> {
    match owner {
        RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::ContractInitAbi { plan }) => {
            verify_contract_entry_args(
                db,
                plan.contract,
                plan.user_init,
                &plan.entry_args.effects,
                true,
            )
        }
        RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::ContractRecvAbi { plan }) => {
            verify_contract_entry_args(
                db,
                plan.contract,
                Some(plan.user_recv),
                &plan.entry_args.effects,
                false,
            )
        }
        RuntimeFunctionOwner::Semantic(_)
        | RuntimeFunctionOwner::Synthetic(
            RuntimeSyntheticSpec::MainRoot { .. }
            | RuntimeSyntheticSpec::TestRoot { .. }
            | RuntimeSyntheticSpec::ManualContractRoot { .. }
            | RuntimeSyntheticSpec::ContractInitRoot { .. }
            | RuntimeSyntheticSpec::ContractRuntimeRoot { .. },
        ) => Ok(()),
    }
}

fn verify_contract_entry_args<'db>(
    db: &'db dyn MirDb,
    contract: Contract<'db>,
    callee: Option<RuntimeInstance<'db>>,
    args: &[EntryEffectArgPlan<'db>],
    is_init: bool,
) -> Result<(), VerifyError<'db>> {
    let expected_fields = callee
        .map(|callee| expected_contract_field_args(db, callee))
        .transpose()?
        .unwrap_or_default();
    if expected_fields.len() != args.len() {
        return Err(VerifyError::ContractFieldArgumentCountMismatch {
            contract,
            expected: expected_fields.len(),
            actual: args.len(),
        });
    }
    for (expected, arg) in expected_fields.into_iter().zip(args) {
        let actual = match arg {
            EntryEffectArgPlan::ContractField(binding) => Some(binding.field),
            EntryEffectArgPlan::TargetRootProvider(_) => None,
        };
        if expected != actual {
            return Err(VerifyError::ContractFieldIdentityMismatch { expected, actual });
        }
        if let EntryEffectArgPlan::ContractField(binding) = arg {
            verify_contract_field_binding(db, contract, binding, is_init)?;
        }
    }
    Ok(())
}

fn expected_contract_field_args<'db>(
    db: &'db dyn MirDb,
    callee: RuntimeInstance<'db>,
) -> Result<Vec<Option<ContractFieldId<'db>>>, VerifyError<'db>> {
    let Some(semantic) = callee.key(db).semantic(db) else {
        return Err(VerifyError::InvalidPackageFunction(callee));
    };
    Ok(owner_effect_bindings(db, semantic.key(db).owner(db))
        .into_iter()
        .filter_map(|binding| {
            runtime_effect_binding_plan(db, semantic, binding)?;
            let provider = resolved_provider_binding_for_instance_effect(db, semantic, binding)?;
            Some(match provider.source {
                ProviderSource::ContractField { field } => Some(field),
                ProviderSource::UsesParam { .. } | ProviderSource::RootProvider { .. } => None,
            })
        })
        .collect())
}

fn verify_contract_field_binding<'db>(
    db: &'db dyn MirDb,
    contract: Contract<'db>,
    binding: &ContractFieldBinding<'db>,
    is_init: bool,
) -> Result<(), VerifyError<'db>> {
    if binding.field.contract != contract {
        return Err(VerifyError::ContractFieldOwnerMismatch {
            contract,
            field: binding.field,
        });
    }
    let field = contract
        .storage_layout(db)
        .values()
        .find(|field| field.field == binding.field)
        .ok_or(VerifyError::UnknownContractField(binding.field))?;
    let expected_ty = field
        .target_effect_binding_ty(db)
        .map_err(|_| VerifyError::InvalidContractFieldLayout(binding.field))?;
    if binding.declared_ty != expected_ty {
        return Err(VerifyError::ContractFieldTypeMismatch {
            field: binding.field,
            expected: expected_ty,
            actual: binding.declared_ty,
        });
    }

    let expected_init_immutable = is_init && field.address_space == ProviderAddressSpace::Code;
    if binding.init_immutable != expected_init_immutable {
        return Err(VerifyError::ContractFieldInitModeMismatch {
            field: binding.field,
            expected: expected_init_immutable,
            actual: binding.init_immutable,
        });
    }
    let expected_space = if expected_init_immutable {
        AddressSpaceKind::Memory
    } else {
        provider_address_space_to_runtime(field.address_space)
    };
    let actual_space = binding.class.address_space();
    if actual_space != Some(expected_space) {
        return Err(VerifyError::ContractFieldAddressSpaceMismatch {
            field: binding.field,
            expected: expected_space,
            actual: actual_space,
        });
    }

    let expected_slot = expected_contract_field_slot(db, contract, field, is_init)
        .ok_or(VerifyError::InvalidContractFieldLayout(binding.field))?;
    if binding.slot != expected_slot {
        return Err(VerifyError::ContractFieldSlotMismatch {
            field: binding.field,
            expected: expected_slot,
            actual: binding.slot,
        });
    }

    let expected_kind = contract_field_ref_kind(db, binding).ok_or_else(|| {
        VerifyError::InvalidContractFieldClass {
            field: binding.field,
            class: binding.class.clone(),
        }
    })?;
    if binding.kind != expected_kind {
        return Err(VerifyError::ContractFieldKindMismatch(binding.field));
    }
    let pointee =
        binding
            .class
            .deref_target()
            .ok_or_else(|| VerifyError::InvalidContractFieldClass {
                field: binding.field,
                class: binding.class.clone(),
            })?;
    let mir_span = pointee.span_words(db);
    if u64::try_from(field.inline_span).ok() != Some(mir_span) {
        return Err(VerifyError::ContractFieldSpanMismatch {
            field: binding.field,
            hir_span: field.inline_span,
            mir_span,
        });
    }
    Ok(())
}

fn expected_contract_field_slot<'db>(
    db: &'db dyn MirDb,
    contract: Contract<'db>,
    field: &FieldStorageLayout<'db>,
    is_init: bool,
) -> Option<ContractFieldSlot> {
    if field.address_space == ProviderAddressSpace::Code && !is_init {
        let field_slot = i128::try_from(field.slot_offset).ok()?;
        let high_water = i128::try_from(contract.code_address_space_slot_count(db)).ok()?;
        field_slot
            .checked_sub(high_water)?
            .checked_mul(32)
            .map(ContractFieldSlot::CodeTailBytes)
    } else {
        u128::try_from(field.slot_offset)
            .ok()
            .map(ContractFieldSlot::Words)
    }
}

fn contract_field_ref_kind<'db>(
    db: &'db dyn MirDb,
    binding: &ContractFieldBinding<'db>,
) -> Option<RefKind<'db>> {
    match &binding.class {
        RuntimeClass::Ref {
            kind,
            view: RefView::Whole,
            ..
        } => Some(kind.clone()),
        RuntimeClass::RawAddr { space, .. } => Some(RefKind::Provider {
            provider_ty: TyId::borrow_ref_of(db, binding.declared_ty),
            space: *space,
        }),
        RuntimeClass::Scalar(_)
        | RuntimeClass::AggregateValue { .. }
        | RuntimeClass::Ref {
            view: RefView::EnumVariant(_),
            ..
        } => None,
    }
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::{analysis::ty::ProviderAddressSpace, semantic::ContractFieldId};
    use url::Url;

    use super::*;
    use crate::{
        build_runtime_package,
        instance::{
            RuntimeInstanceKey, RuntimeInstanceSource, RuntimeSyntheticInstance,
            get_or_build_runtime_instance,
        },
        runtime::{
            EntrySemanticArgsPlan, RuntimeFunction, RuntimeInlineHint, RuntimeLinkage,
            RuntimePackage, RuntimePackagePlan,
        },
        verify_runtime_package,
    };

    const CONTRACTS: &str = r#"
use std::evm::StorageMap
use std::evm::effects::TStorPtr

struct Rooted<const ROOT: u256 = _> {
    value: u256,
}

msg LayoutMsg {
    #[selector = 1]
    Read -> u256,
}

pub contract Layouts {
    mut words: [[u256; 2]; 2],
    mut maps: [StorageMap<u256, u256>; 2],
    mut temp: TStorPtr<u256>,
    fixed: [Rooted; 2],

    init() uses (mut fixed) {
        fixed = [Rooted { value: 11 }, Rooted { value: 22 }]
    }

    recv LayoutMsg {
        Read -> u256 uses (words, maps, temp, fixed) {
            words[0][0] + maps[0].get(key: 0) + temp + fixed[0].value
        }
    }
}

pub contract Other {
    other: u256,

    init() uses (mut other) {
        other = 1
    }
}
"#;

    #[derive(Clone, Copy)]
    enum EntryKind {
        Init,
        Recv,
    }

    fn with_package<T>(
        f: impl for<'db> FnOnce(&'db DriverDataBase, RuntimePackage<'db>) -> T,
    ) -> T {
        let mut db = DriverDataBase::default();
        let file_url = Url::parse("file:///contract_field_layout_seam.fe").unwrap();
        let file = db
            .workspace()
            .touch(&mut db, file_url, Some(CONTRACTS.to_string()));
        let top_mod = db.top_mod(file);
        let diagnostics = db.run_on_top_mod(top_mod);
        assert!(
            diagnostics.is_empty(),
            "contract source should be valid:\n{}",
            diagnostics.format_diags(&db)
        );
        let package = build_runtime_package(&db, top_mod).expect("contract package should build");
        f(&db, package)
    }

    fn contract_named<'db>(
        db: &'db DriverDataBase,
        package: RuntimePackage<'db>,
        name: &str,
    ) -> Contract<'db> {
        package
            .top_mod(db)
            .all_contracts(db)
            .iter()
            .copied()
            .find(|contract| {
                contract
                    .name(db)
                    .to_opt()
                    .is_some_and(|candidate| candidate.data(db) == name)
            })
            .unwrap_or_else(|| panic!("missing contract `{name}`"))
    }

    fn field_named<'db>(
        db: &'db DriverDataBase,
        contract: Contract<'db>,
        name: &str,
    ) -> ContractFieldId<'db> {
        contract
            .storage_layout(db)
            .values()
            .find(|field| field.name.data(db) == name)
            .map(|field| field.field)
            .unwrap_or_else(|| panic!("missing field `{name}`"))
    }

    fn wrapper_spec<'db>(
        db: &'db DriverDataBase,
        package: RuntimePackage<'db>,
        contract: Contract<'db>,
        kind: EntryKind,
    ) -> (RuntimeFunction<'db>, RuntimeSyntheticSpec<'db>) {
        package
            .functions(db)
            .iter()
            .copied()
            .find_map(|function| {
                let RuntimeFunctionOwner::Synthetic(spec) = function.owner(db) else {
                    return None;
                };
                let matches = match (&spec, kind) {
                    (RuntimeSyntheticSpec::ContractInitAbi { plan }, EntryKind::Init) => {
                        plan.contract == contract
                    }
                    (RuntimeSyntheticSpec::ContractRecvAbi { plan }, EntryKind::Recv) => {
                        plan.contract == contract
                    }
                    _ => false,
                };
                matches.then_some((function, spec))
            })
            .unwrap_or_else(|| panic!("missing contract wrapper"))
    }

    fn entry_args_mut<'a, 'db>(
        spec: &'a mut RuntimeSyntheticSpec<'db>,
        kind: EntryKind,
    ) -> &'a mut EntrySemanticArgsPlan<'db> {
        match (spec, kind) {
            (RuntimeSyntheticSpec::ContractInitAbi { plan }, EntryKind::Init) => {
                &mut plan.entry_args
            }
            (RuntimeSyntheticSpec::ContractRecvAbi { plan }, EntryKind::Recv) => {
                &mut plan.entry_args
            }
            _ => panic!("wrapper kind changed while mutating it"),
        }
    }

    fn field_binding<'a, 'db>(
        args: &'a EntrySemanticArgsPlan<'db>,
        field: ContractFieldId<'db>,
    ) -> &'a ContractFieldBinding<'db> {
        args.effects
            .iter()
            .find_map(|arg| match arg {
                EntryEffectArgPlan::ContractField(binding) if binding.field == field => {
                    Some(binding)
                }
                EntryEffectArgPlan::ContractField(_)
                | EntryEffectArgPlan::TargetRootProvider(_) => None,
            })
            .unwrap_or_else(|| panic!("missing binding for field {}", field.index))
    }

    fn field_binding_mut<'a, 'db>(
        args: &'a mut EntrySemanticArgsPlan<'db>,
        field: ContractFieldId<'db>,
    ) -> &'a mut ContractFieldBinding<'db> {
        args.effects
            .iter_mut()
            .find_map(|arg| match arg {
                EntryEffectArgPlan::ContractField(binding) if binding.field == field => {
                    Some(binding)
                }
                EntryEffectArgPlan::ContractField(_)
                | EntryEffectArgPlan::TargetRootProvider(_) => None,
            })
            .unwrap_or_else(|| panic!("missing binding for field {}", field.index))
    }

    fn mutated_wrapper_error<'db>(
        db: &'db DriverDataBase,
        package: RuntimePackage<'db>,
        contract: Contract<'db>,
        kind: EntryKind,
        mutate: impl FnOnce(&mut EntrySemanticArgsPlan<'db>),
    ) -> VerifyError<'db> {
        mutated_wrapper_spec_error(db, package, contract, kind, |spec| {
            mutate(entry_args_mut(spec, kind));
        })
    }

    fn mutated_wrapper_spec_error<'db>(
        db: &'db DriverDataBase,
        package: RuntimePackage<'db>,
        contract: Contract<'db>,
        kind: EntryKind,
        mutate: impl FnOnce(&mut RuntimeSyntheticSpec<'db>),
    ) -> VerifyError<'db> {
        let (function, mut spec) = wrapper_spec(db, package, contract, kind);
        mutate(&mut spec);
        let synthetic = RuntimeSyntheticInstance::new(db, spec.clone());
        let key = RuntimeInstanceKey::new(db, RuntimeInstanceSource::Synthetic(synthetic), vec![]);
        let instance = get_or_build_runtime_instance(db, key);
        let function = RuntimeFunction::new(
            db,
            instance,
            function.symbol(db),
            RuntimeLinkage::Private,
            RuntimeInlineHint::Auto,
            RuntimeFunctionOwner::Synthetic(spec),
            vec![],
        );
        let package = RuntimePackage::new(
            db,
            package.top_mod(db),
            vec![function],
            RuntimePackagePlan::new(db, vec![], vec![], vec![], vec![], None),
        );
        verify_runtime_package(db, package).expect_err("mutated package should be invalid")
    }

    fn set_binding_space(binding: &mut ContractFieldBinding<'_>, space: AddressSpaceKind) {
        match &mut binding.class {
            RuntimeClass::Ref {
                kind: RefKind::Provider { space: actual, .. },
                ..
            }
            | RuntimeClass::RawAddr { space: actual, .. } => *actual = space,
            class => panic!("contract binding has non-provider class {class:?}"),
        }
        let RefKind::Provider { space: actual, .. } = &mut binding.kind else {
            panic!("contract binding has non-provider kind")
        };
        *actual = space;
    }

    fn set_binding_pointee<'db>(
        binding: &mut ContractFieldBinding<'db>,
        pointee: RuntimeClass<'db>,
    ) {
        let RuntimeClass::Ref {
            pointee: actual, ..
        } = &mut binding.class
        else {
            panic!("test field should lower to a reference")
        };
        **actual = pointee;
    }

    #[test]
    fn valid_contract_field_bindings_cover_all_spaces_and_shapes() {
        with_package(|db, package| {
            verify_runtime_package(db, package).expect("valid package should verify twice");
            let contract = contract_named(db, package, "Layouts");
            let words = field_named(db, contract, "words");
            let maps = field_named(db, contract, "maps");
            let temp = field_named(db, contract, "temp");
            let fixed = field_named(db, contract, "fixed");
            let (_, mut recv) = wrapper_spec(db, package, contract, EntryKind::Recv);
            let recv_args = entry_args_mut(&mut recv, EntryKind::Recv).clone();

            for (field, space, span) in [
                (words, ProviderAddressSpace::Storage, 4),
                (maps, ProviderAddressSpace::Storage, 0),
                (temp, ProviderAddressSpace::Transient, 1),
                (fixed, ProviderAddressSpace::Code, 2),
            ] {
                let hir = contract
                    .storage_layout(db)
                    .values()
                    .find(|layout| layout.field == field)
                    .expect("field layout");
                let binding = field_binding(&recv_args, field);
                assert_eq!(hir.address_space, space);
                assert_eq!(hir.inline_span, span);
                assert_eq!(
                    binding.class.deref_target().unwrap().span_words(db),
                    span as u64
                );
            }

            let (_, mut init) = wrapper_spec(db, package, contract, EntryKind::Init);
            let init_binding = field_binding(entry_args_mut(&mut init, EntryKind::Init), fixed);
            assert!(init_binding.init_immutable);
            assert!(matches!(init_binding.slot, ContractFieldSlot::Words(_)));
            let recv_binding = field_binding(&recv_args, fixed);
            assert!(!recv_binding.init_immutable);
            assert!(matches!(
                recv_binding.slot,
                ContractFieldSlot::CodeTailBytes(_)
            ));
        });
    }

    #[test]
    fn package_verifier_rejects_contract_field_identity_mutations() {
        with_package(|db, package| {
            let contract = contract_named(db, package, "Layouts");
            let other_contract = contract_named(db, package, "Other");
            let words = field_named(db, contract, "words");
            let maps = field_named(db, contract, "maps");
            let other = field_named(db, other_contract, "other");

            assert!(matches!(
                mutated_wrapper_error(db, package, contract, EntryKind::Recv, |args| {
                    field_binding_mut(args, words).field = maps
                }),
                VerifyError::ContractFieldIdentityMismatch {
                    expected: Some(expected),
                    actual: Some(actual),
                } if expected == words && actual == maps
            ));

            assert!(matches!(
                mutated_wrapper_error(db, package, contract, EntryKind::Recv, |args| {
                    field_binding_mut(args, words).field = other
                }),
                VerifyError::ContractFieldIdentityMismatch {
                    expected: Some(expected),
                    actual: Some(actual),
                } if expected == words && actual == other
            ));
        });
    }

    #[test]
    fn package_verifier_rejects_space_slot_and_immutable_mode_mutations() {
        with_package(|db, package| {
            let contract = contract_named(db, package, "Layouts");
            let words = field_named(db, contract, "words");
            let temp = field_named(db, contract, "temp");
            let fixed = field_named(db, contract, "fixed");

            assert!(matches!(
                mutated_wrapper_error(db, package, contract, EntryKind::Recv, |args| {
                    let binding = field_binding_mut(args, words);
                    let ContractFieldSlot::Words(slot) = binding.slot else {
                        panic!("storage field should use a word slot")
                    };
                    binding.slot = ContractFieldSlot::Words(slot + 1);
                }),
                VerifyError::ContractFieldSlotMismatch { field, .. } if field == words
            ));

            assert!(matches!(
                mutated_wrapper_error(db, package, contract, EntryKind::Recv, |args| {
                    set_binding_space(field_binding_mut(args, words), AddressSpaceKind::Transient)
                }),
                VerifyError::ContractFieldAddressSpaceMismatch { field, .. }
                    if field == words
            ));

            assert!(matches!(
                mutated_wrapper_error(db, package, contract, EntryKind::Recv, |args| {
                    set_binding_space(field_binding_mut(args, temp), AddressSpaceKind::Storage)
                }),
                VerifyError::ContractFieldAddressSpaceMismatch { field, .. }
                    if field == temp
            ));

            assert!(matches!(
                mutated_wrapper_error(db, package, contract, EntryKind::Recv, |args| {
                    set_binding_space(field_binding_mut(args, fixed), AddressSpaceKind::Storage)
                }),
                VerifyError::ContractFieldAddressSpaceMismatch { field, .. }
                    if field == fixed
            ));

            assert!(matches!(
                mutated_wrapper_error(db, package, contract, EntryKind::Recv, |args| {
                    let binding = field_binding_mut(args, fixed);
                    let ContractFieldSlot::CodeTailBytes(offset) = binding.slot else {
                        panic!("immutable receive field should use a code-tail offset")
                    };
                    binding.slot = ContractFieldSlot::CodeTailBytes(offset + 32);
                }),
                VerifyError::ContractFieldSlotMismatch { field, .. } if field == fixed
            ));

            assert!(matches!(
                mutated_wrapper_error(db, package, contract, EntryKind::Init, |args| {
                    field_binding_mut(args, fixed).init_immutable = false
                }),
                VerifyError::ContractFieldInitModeMismatch { field, .. } if field == fixed
            ));
        });
    }

    #[test]
    fn package_verifier_rejects_owner_count_type_and_kind_mutations() {
        with_package(|db, package| {
            let contract = contract_named(db, package, "Layouts");
            let other_contract = contract_named(db, package, "Other");
            let words = field_named(db, contract, "words");

            assert!(matches!(
                mutated_wrapper_spec_error(db, package, contract, EntryKind::Recv, |spec| {
                    let RuntimeSyntheticSpec::ContractRecvAbi { plan } = spec else {
                        panic!("expected receive wrapper")
                    };
                    plan.contract = other_contract;
                }),
                VerifyError::ContractFieldOwnerMismatch {
                    contract: owner,
                    field,
                } if owner == other_contract && field == words
            ));

            assert!(matches!(
                mutated_wrapper_error(db, package, contract, EntryKind::Recv, |args| {
                    args.effects = args.effects[1..].into()
                }),
                VerifyError::ContractFieldArgumentCountMismatch {
                    contract: owner,
                    expected,
                    actual,
                } if owner == contract && expected == 4 && actual == 3
            ));

            assert!(matches!(
                mutated_wrapper_error(db, package, contract, EntryKind::Recv, |args| {
                    field_binding_mut(args, words).declared_ty = TyId::bool(db)
                }),
                VerifyError::ContractFieldTypeMismatch { field, .. } if field == words
            ));

            assert!(matches!(
                mutated_wrapper_error(db, package, contract, EntryKind::Recv, |args| {
                    field_binding_mut(args, words).kind = RefKind::Object
                }),
                VerifyError::ContractFieldKindMismatch(field) if field == words
            ));
        });
    }

    #[test]
    fn package_verifier_rejects_array_and_hole_field_span_mutations() {
        with_package(|db, package| {
            let contract = contract_named(db, package, "Layouts");
            let words = field_named(db, contract, "words");
            let maps = field_named(db, contract, "maps");
            let (_, mut recv) = wrapper_spec(db, package, contract, EntryKind::Recv);
            let args = entry_args_mut(&mut recv, EntryKind::Recv);
            let words_pointee = field_binding(args, words).class.deref_target().unwrap();
            let maps_pointee = field_binding(args, maps).class.deref_target().unwrap();

            assert!(matches!(
                mutated_wrapper_error(db, package, contract, EntryKind::Recv, |args| {
                    set_binding_pointee(field_binding_mut(args, words), maps_pointee.clone())
                }),
                VerifyError::ContractFieldSpanMismatch {
                    field,
                    hir_span: 4,
                    mir_span: 0,
                } if field == words
            ));

            assert!(matches!(
                mutated_wrapper_error(db, package, contract, EntryKind::Recv, |args| {
                    set_binding_pointee(field_binding_mut(args, maps), words_pointee.clone())
                }),
                VerifyError::ContractFieldSpanMismatch {
                    field,
                    hir_span: 0,
                    mir_span: 4,
                } if field == maps
            ));
        });
    }
}
