use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            corelib::resolve_lib_type_path,
            trait_resolution::PredicateListId,
            ty_check::EffectParamSite,
            ty_def::{CapabilityKind, TyId},
        },
    },
    hir_def::{Contract, Func, scope_graph::ScopeId},
};

use super::{effect_handle_metadata, resolve_default_root_effect_ty};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ProviderAddressSpace {
    Memory,
    Storage,
    Transient,
    Calldata,
    Code,
}

impl ProviderAddressSpace {
    pub fn pretty(self) -> &'static str {
        match self {
            Self::Memory => "memory",
            Self::Storage => "storage",
            Self::Transient => "transient storage",
            Self::Calldata => "calldata",
            Self::Code => "code",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ProviderKind {
    RootObject,
    Handle,
    RawAddress,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ProviderTransport {
    ByValue,
    ByPlace,
    ByTempPlace,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ProviderSemantics<'db> {
    pub provider_ty: TyId<'db>,
    pub kind: ProviderKind,
    pub address_space: Option<ProviderAddressSpace>,
    pub target_ty: Option<TyId<'db>>,
    pub transport: ProviderTransport,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum RootProviderSiteKind {
    Func,
    Contract,
    ContractInit,
    ContractRecvArm,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct RootProviderRegistration<'db> {
    pub idx: u32,
    pub site_kind: RootProviderSiteKind,
    pub provider_ty: TyId<'db>,
}

pub fn registered_root_providers<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
) -> &'db [RootProviderRegistration<'db>] {
    match site {
        EffectParamSite::Func(func) => registered_root_providers_for_func(db, func).as_slice(),
        EffectParamSite::Contract(contract) => {
            registered_root_providers_for_contract(db, contract, RootProviderSiteKind::Contract)
                .as_slice()
        }
        EffectParamSite::ContractInit { contract } => {
            registered_root_providers_for_contract(db, contract, RootProviderSiteKind::ContractInit)
                .as_slice()
        }
        EffectParamSite::ContractRecvArm { contract, .. } => {
            registered_root_providers_for_contract(
                db,
                contract,
                RootProviderSiteKind::ContractRecvArm,
            )
            .as_slice()
        }
    }
}

#[salsa::tracked(return_ref)]
fn registered_root_providers_for_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<RootProviderRegistration<'db>> {
    registered_root_providers_for_scope(db, RootProviderSiteKind::Func, func.scope())
}

#[salsa::tracked(return_ref)]
fn registered_root_providers_for_contract<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    site_kind: RootProviderSiteKind,
) -> Vec<RootProviderRegistration<'db>> {
    registered_root_providers_for_scope(db, site_kind, contract.scope())
}

fn registered_root_providers_for_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    site_kind: RootProviderSiteKind,
    scope: ScopeId<'db>,
) -> Vec<RootProviderRegistration<'db>> {
    let assumptions = PredicateListId::empty_list(db);
    let Some(provider_ty) = resolve_default_root_effect_ty(db, scope, assumptions) else {
        return Vec::new();
    };
    vec![RootProviderRegistration {
        idx: 0,
        site_kind,
        provider_ty,
    }]
}

pub fn provider_semantics<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    provider_ty: TyId<'db>,
) -> ProviderSemantics<'db> {
    if let Some((kind, inner)) = provider_ty.as_capability(db) {
        let address_space = match kind {
            CapabilityKind::View | CapabilityKind::Ref | CapabilityKind::Mut => {
                Some(ProviderAddressSpace::Memory)
            }
        };
        return ProviderSemantics {
            provider_ty,
            kind: provider_kind_for_target_ty(db, inner),
            address_space,
            target_ty: Some(inner),
            transport: ProviderTransport::ByValue,
        };
    }

    if let Some(metadata) = effect_handle_metadata(db, scope, assumptions, provider_ty) {
        return ProviderSemantics {
            provider_ty,
            kind: provider_kind_for_target_ty(db, metadata.target_ty),
            address_space: address_space_from_ty(db, scope, metadata.address_space),
            target_ty: Some(metadata.target_ty),
            transport: ProviderTransport::ByValue,
        };
    }

    ProviderSemantics {
        provider_ty,
        kind: ProviderKind::RootObject,
        address_space: Some(ProviderAddressSpace::Memory),
        target_ty: None,
        transport: ProviderTransport::ByValue,
    }
}

pub fn provider_semantics_for_specialized_call<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    provider_ty: TyId<'db>,
    target_ty: Option<TyId<'db>>,
    address_space: Option<ProviderAddressSpace>,
    transport: ProviderTransport,
) -> ProviderSemantics<'db> {
    let mut semantics = provider_semantics(db, scope, assumptions, provider_ty);
    if let Some(target_ty) = target_ty {
        semantics.kind = provider_kind_for_target_ty(db, target_ty);
        semantics.target_ty = Some(target_ty);
    }
    if let Some(address_space) = address_space {
        semantics.address_space = Some(address_space);
    }
    semantics.transport = transport;
    semantics
}

pub fn address_space_from_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
) -> Option<ProviderAddressSpace> {
    for (path, space) in [
        ("core::effect_ref::Memory", ProviderAddressSpace::Memory),
        ("core::effect_ref::Storage", ProviderAddressSpace::Storage),
        (
            "core::effect_ref::TransientStorage",
            ProviderAddressSpace::Transient,
        ),
        ("core::effect_ref::Calldata", ProviderAddressSpace::Calldata),
        ("core::effect_ref::Code", ProviderAddressSpace::Code),
    ] {
        if ty == resolve_lib_type_path(db, scope, path)? {
            return Some(space);
        }
    }
    None
}

fn provider_kind_for_target_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    target_ty: TyId<'db>,
) -> ProviderKind {
    let target_ty = if let Some((_, inner)) = target_ty.as_borrow(db) {
        inner
    } else {
        target_ty
    };
    if target_ty.is_struct(db)
        || target_ty.is_array(db)
        || target_ty.is_tuple(db)
        || target_ty.as_enum(db).is_some()
    {
        ProviderKind::Handle
    } else {
        ProviderKind::RawAddress
    }
}
