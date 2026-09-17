use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            provider::{ProviderKind, ProviderTransport, provider_semantics},
            trait_resolution::PredicateListId,
            ty_def::{BorrowKind, CapabilityKind, TyId},
        },
    },
    hir_def::scope_graph::ScopeId,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CapabilityClass {
    Borrow(BorrowKind),
    View,
    Handle,
}

/// Storage policy is separate from the capability's access authority.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum StorageClass {
    Borrowed,
    ProviderValue,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct CapabilitySemantics<'db> {
    pub class: CapabilityClass,
    pub target_ty: TyId<'db>,
    pub representation_ty: TyId<'db>,
    pub transport: ProviderTransport,
    pub storage: StorageClass,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UnresolvedCapability<'db>(pub TyId<'db>);

pub fn capability_semantics<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    ty: TyId<'db>,
) -> Result<Option<CapabilitySemantics<'db>>, UnresolvedCapability<'db>> {
    if let Some((kind, target_ty)) = ty.as_capability(db) {
        let class = match kind {
            CapabilityKind::Mut => CapabilityClass::Borrow(BorrowKind::Mut),
            CapabilityKind::Ref => CapabilityClass::Borrow(BorrowKind::Ref),
            CapabilityKind::View => CapabilityClass::View,
        };
        return Ok(Some(CapabilitySemantics {
            class,
            target_ty,
            representation_ty: ty,
            transport: ProviderTransport::ByValue,
            storage: StorageClass::Borrowed,
        }));
    }
    let provider = provider_semantics(db, scope, assumptions, ty);
    if provider.kind == ProviderKind::InvalidHandle {
        return Err(UnresolvedCapability(ty));
    }
    Ok(provider.target_ty.map(|target_ty| CapabilitySemantics {
        class: CapabilityClass::Handle,
        target_ty,
        representation_ty: ty,
        transport: provider.transport,
        storage: StorageClass::ProviderValue,
    }))
}
