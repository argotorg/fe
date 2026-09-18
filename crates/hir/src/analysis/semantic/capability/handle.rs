//! Explicit origins for nominal handles manufactured without an input referent.
use super::{
    index::{IndexExpr, IndexSubst},
    semantics::UnresolvedCapability,
};
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{SemanticInstance, normalized::NValueId},
        ty::{
            assoc_const::AssocConstUse,
            const_ty::{ConstTyId, const_ty_or_abstract_from_assoc_const_use},
            fold::TyFoldable,
            provider::{
                EffectHandleTargetResolution, ProviderAddressSpace, effect_space_from_const_ty,
                resolve_effect_handle_target,
            },
            trait_resolution::PredicateListId,
            ty_def::{TyData, TyId},
        },
    },
    hir_def::{IdentId, scope_graph::ScopeId},
};

/// A semantic address-space contract can remain symbolic in a generic body.
/// Unknown spaces may alias every concrete space; they never default to memory.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HandleAddressSpace<'db> {
    /// Native capabilities inherit the caller's referent space at instantiation.
    Unspecified,
    Known(ProviderAddressSpace),
    Declared {
        value: TyId<'db>,
        scope: ScopeId<'db>,
    },
}

impl<'db> HandleAddressSpace<'db> {
    pub fn known(self) -> Option<ProviderAddressSpace> {
        match self {
            Self::Known(space) => Some(space),
            Self::Declared { .. } | Self::Unspecified => None,
        }
    }

    pub fn may_alias(self, other: Self) -> bool {
        match (self.known(), other.known()) {
            (Some(left), Some(right)) => left == right,
            _ => true,
        }
    }

    fn from_const(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>, value: ConstTyId<'db>) -> Self {
        effect_space_from_const_ty(db, scope, value).map_or_else(
            || Self::Declared {
                value: TyId::const_ty(db, value),
                scope,
            },
            Self::Known,
        )
    }

    pub(super) fn substitute(self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        match self {
            Self::Known(_) | Self::Unspecified => self,
            Self::Declared { value, scope } => {
                let value = value.fold_with(db, &mut subst.clone());
                let TyData::ConstTy(value) = value.data(db) else {
                    unreachable!("address-space substitution preserves the declared constant")
                };
                Self::from_const(db, scope, *value)
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OpaqueHandleContract<'db> {
    pub handle_ty: TyId<'db>,
    pub target_ty: TyId<'db>,
    pub address_space: HandleAddressSpace<'db>,
}

impl<'db> OpaqueHandleContract<'db> {
    pub fn for_ty(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        ty: TyId<'db>,
    ) -> Result<Option<Self>, UnresolvedCapability<'db>> {
        match resolve_effect_handle_target(db, scope, assumptions, ty) {
            EffectHandleTargetResolution::NotHandle => Ok(None),
            EffectHandleTargetResolution::Resolved {
                impl_instance,
                target_ty,
                ..
            } => {
                let inst = impl_instance.trait_inst();
                let name = IdentId::new(db, "SPACE");
                let expected = inst
                    .def(db)
                    .const_(db, name)
                    .and_then(|constant| constant.ty_binder(db))
                    .map(|binder| binder.instantiate(db, inst.args(db)))
                    .ok_or(UnresolvedCapability(ty))?;
                let value = const_ty_or_abstract_from_assoc_const_use(
                    db,
                    AssocConstUse::new(scope, assumptions, inst, name),
                    expected,
                )
                .ok_or(UnresolvedCapability(ty))?;
                let address_space = HandleAddressSpace::from_const(db, scope, value);
                Ok(Some(Self {
                    handle_ty: ty,
                    target_ty,
                    address_space,
                }))
            }
            EffectHandleTargetResolution::Ambiguous
            | EffectHandleTargetResolution::UnresolvedTarget => Err(UnresolvedCapability(ty)),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OpaqueHandleOccurrence<'db> {
    Value {
        instance: SemanticInstance<'db>,
        value: NValueId,
        choice: u32,
    },
    Summary(u32),
}

/// Equal occurrences preserve identity through copies. Different occurrences may
/// alias: constructing a handle does not imply allocating a fresh referent.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OpaqueHandleRef<'db> {
    pub contract: OpaqueHandleContract<'db>,
    pub occurrence: OpaqueHandleOccurrence<'db>,
    pub arguments: Box<[IndexExpr<'db>]>,
}

impl<'db> OpaqueHandleRef<'db> {
    pub fn substitute(&self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        Self {
            contract: OpaqueHandleContract {
                handle_ty: self.contract.handle_ty.fold_with(db, &mut subst.clone()),
                target_ty: self.contract.target_ty.fold_with(db, &mut subst.clone()),
                address_space: self.contract.address_space.substitute(db, subst),
            },
            occurrence: self.occurrence,
            arguments: self
                .arguments
                .iter()
                .map(|index| subst.apply(*index))
                .collect(),
        }
    }
}
