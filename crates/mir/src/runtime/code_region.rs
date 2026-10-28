use hir::{
    analysis::semantic::{ManualContractSection, SemanticCodeRegionRef},
    hir_def::{Func, ManualContractRootAttr},
};

use crate::{
    db::MirDb,
    instance::RuntimeInstance,
    runtime::{RuntimeCodeRegion, RuntimeCodeRegionKey, RuntimeSectionName},
};

use super::package::manual_contract_root_instance;

pub(crate) fn runtime_code_region_for_semantic_ref<'db>(
    db: &'db dyn MirDb,
    region: &SemanticCodeRegionRef<'db>,
) -> RuntimeCodeRegion<'db> {
    match region {
        SemanticCodeRegionRef::ManualContractRoot { func } => {
            runtime_code_region_for_manual_root(db, *func)
                .expect("semantic manual contract root should resolve to a runtime code region")
        }
    }
}

pub(crate) fn runtime_code_region_for_manual_root<'db>(
    db: &'db dyn MirDb,
    func: Func<'db>,
) -> Option<RuntimeCodeRegion<'db>> {
    match func.manual_contract_root_attr(db)? {
        ManualContractRootAttr::Init { .. } | ManualContractRootAttr::Runtime { .. } => {}
        ManualContractRootAttr::Error(_) => return None,
    }
    Some(RuntimeCodeRegion::new(
        db,
        RuntimeCodeRegionKey::ManualContractRoot { func },
    ))
}

/// The symbol under which the code of `region` is embedded. Derived from the
/// name of the object that holds the region, which is unique in a package, so
/// that same-named contracts do not share a symbol.
pub(crate) fn code_region_symbol<'db>(
    db: &'db dyn MirDb,
    region: RuntimeCodeRegion<'db>,
    object_name: &str,
) -> String {
    match region.key(db) {
        RuntimeCodeRegionKey::ContractInit { .. } => format!("{object_name}_init"),
        RuntimeCodeRegionKey::ContractRuntime { .. } => format!("{object_name}_runtime"),
        RuntimeCodeRegionKey::ManualContractRoot { func } => {
            let metadata = manual_contract_root_metadata(db, func)
                .expect("manual contract root region should resolve manual contract metadata");
            manual_contract_section_symbol(object_name, metadata.section)
        }
    }
}

pub(crate) fn code_region_section_name<'db>(
    db: &'db dyn MirDb,
    region: RuntimeCodeRegion<'db>,
) -> Option<RuntimeSectionName> {
    match region.key(db) {
        RuntimeCodeRegionKey::ContractInit { .. } => Some(RuntimeSectionName::Init),
        RuntimeCodeRegionKey::ContractRuntime { .. } => Some(RuntimeSectionName::Runtime),
        RuntimeCodeRegionKey::ManualContractRoot { func } => {
            let metadata = manual_contract_root_metadata(db, func)?;
            Some(match metadata.section {
                ManualContractSection::Init => RuntimeSectionName::Init,
                ManualContractSection::Runtime => RuntimeSectionName::Runtime,
            })
        }
    }
}

pub(crate) fn code_region_runtime_entry<'db>(
    db: &'db dyn MirDb,
    region: RuntimeCodeRegion<'db>,
) -> Option<RuntimeInstance<'db>> {
    match region.key(db) {
        RuntimeCodeRegionKey::ManualContractRoot { func } => {
            Some(manual_contract_root_metadata(db, func)?.entry)
        }
        RuntimeCodeRegionKey::ContractInit { .. }
        | RuntimeCodeRegionKey::ContractRuntime { .. } => None,
    }
}

#[derive(Clone)]
struct ManualContractRootMetadata<'db> {
    section: ManualContractSection,
    entry: RuntimeInstance<'db>,
}

fn manual_contract_root_metadata<'db>(
    db: &'db dyn MirDb,
    func: Func<'db>,
) -> Option<ManualContractRootMetadata<'db>> {
    let section = match func.manual_contract_root_attr(db)? {
        ManualContractRootAttr::Init { .. } => ManualContractSection::Init,
        ManualContractRootAttr::Runtime { .. } => ManualContractSection::Runtime,
        ManualContractRootAttr::Error(_) => return None,
    };
    let entry = manual_contract_root_instance(db, func).unwrap_or_else(|err| {
        panic!(
            "manual contract root `{}` must synthesize a zero-arg runtime entry: {err}",
            func.name(db)
                .to_opt()
                .map(|name| name.data(db).to_string())
                .unwrap_or_else(|| "<anonymous>".to_string())
        )
    });
    Some(ManualContractRootMetadata { section, entry })
}

fn manual_contract_section_symbol(contract_name: &str, section: ManualContractSection) -> String {
    match section {
        ManualContractSection::Init => format!("__{}_init", sanitize_symbol(contract_name)),
        ManualContractSection::Runtime => {
            format!("__{}_runtime", sanitize_symbol(contract_name))
        }
    }
}

fn sanitize_symbol(value: &str) -> String {
    value
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}
