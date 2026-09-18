use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            provider::{EffectHandleResolution, resolve_effect_handle},
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
    /// A copyable raw memory address. Passing it does not reserve its referent.
    Pointer,
}

/// Ordinary argument transport is independent of access authority and storage.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TransportClass {
    /// Native mutable handles require memory unless a receiver/effect contract
    /// explicitly preserves the provider's address space.
    MemoryBorrow,
    /// Shared borrows and views permit read-only provider transport.
    ReadOnly,
    /// A nominal handle transports its representation without accessing its target.
    ProviderValue,
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
    pub transport: TransportClass,
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
            transport: match kind {
                CapabilityKind::Mut => TransportClass::MemoryBorrow,
                CapabilityKind::Ref | CapabilityKind::View => TransportClass::ReadOnly,
            },
            storage: StorageClass::Borrowed,
        }));
    }
    if let Some(target_ty) = ty.as_ptr(db) {
        return Ok(Some(CapabilitySemantics {
            class: CapabilityClass::Pointer,
            target_ty,
            representation_ty: ty,
            transport: TransportClass::ProviderValue,
            storage: StorageClass::Borrowed,
        }));
    }
    // Semantic shapes retain generic targets; allocation layout requires a concrete target.
    let target_ty = match resolve_effect_handle(db, scope, assumptions, ty) {
        EffectHandleResolution::NotHandle => None,
        EffectHandleResolution::Resolved { target_ty, .. } => Some(target_ty),
        EffectHandleResolution::Invalid(_) => return Err(UnresolvedCapability(ty)),
    };
    Ok(target_ty.map(|target_ty| CapabilitySemantics {
        class: CapabilityClass::Handle,
        target_ty,
        representation_ty: ty,
        transport: TransportClass::ProviderValue,
        storage: StorageClass::ProviderValue,
    }))
}
