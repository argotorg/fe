//! Typed external storage identities, including followed and widened referents.
use std::collections::BTreeMap;

use crate::analysis::{
    HirAnalysisDb,
    semantic::normalized::NRootId,
    ty::{
        ProviderAddressSpace,
        fold::TyFoldable,
        provider::ProviderKind,
        ty_def::{TyData, TyId},
    },
};

use super::{
    guard::Guard,
    handle::{HandleAddressSpace, OpaqueHandleRef},
    index::{BinderScope, IndexExpr, IndexSubst},
    path::RegionPath,
    region::{ProviderRegionId, RegionRoot, path_alias_guard},
    source::InputSource,
};

/// Physical referent typing is independent of a capability's conversion views.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferentContract<'db> {
    pub ty: TyId<'db>,
    pub address_space: HandleAddressSpace<'db>,
    pub addressable: bool,
}

impl<'db> ReferentContract<'db> {
    pub fn new(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        address_space: HandleAddressSpace<'db>,
    ) -> Self {
        Self {
            ty,
            address_space,
            addressable: !ty.is_zero_sized(db),
        }
    }

    pub fn substitute(self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        Self::new(
            db,
            self.ty.fold_with(db, &mut subst.clone()),
            self.address_space.substitute(db, subst),
        )
    }

    pub fn is_abstract(self, db: &'db dyn HirAnalysisDb) -> bool {
        self.ty.has_param(db)
            || matches!(
                self.ty.base_ty(db).data(db),
                TyData::TyParam(_) | TyData::AssocTy(_) | TyData::QualifiedTy(_)
            )
    }

    pub fn may_alias(self, other: Self) -> bool {
        self.addressable && other.addressable && self.address_space.may_alias(other.address_space)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExternalOrigin<'db> {
    /// A typed, conservatively reachable part of an abstract local value.
    Local(NRootId),
    Input(InputSource<'db>),
    Provider {
        provider: ProviderRegionId<'db>,
        target_ty: TyId<'db>,
    },
    OpaqueHandle(OpaqueHandleRef<'db>),
}

/// A source names storage, not the representation of the handle used to reach it.
/// Its final type remains explicit after recursive widening.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExternalSource<'db> {
    pub origin: ExternalOrigin<'db>,
    pub contract: ReferentContract<'db>,
    dereferences: Box<[RegionPath<IndexExpr<'db>>]>,
    reachable: bool,
    uncertain: bool,
}

impl<'db> ExternalSource<'db> {
    pub fn abstract_target(root: &RegionRoot<'db>, contract: ReferentContract<'db>) -> Self {
        let mut source = match root {
            RegionRoot::External(source) => source.clone().widen(),
            RegionRoot::Root { root, .. } => Self {
                origin: ExternalOrigin::Local(*root),
                contract,
                dereferences: Box::new([]),
                reachable: true,
                uncertain: true,
            },
            RegionRoot::Value(_) => unreachable!("an abstract referent has storage"),
        };
        source.contract = contract;
        source
    }

    pub fn input(
        source: InputSource<'db>,
        contract: ReferentContract<'db>,
        uncertain: bool,
    ) -> Self {
        let reachable = source.is_reachable();
        Self {
            origin: ExternalOrigin::Input(source),
            contract,
            dereferences: Box::new([]),
            reachable,
            uncertain,
        }
    }

    pub fn provider(
        db: &'db dyn HirAnalysisDb,
        provider: ProviderRegionId<'db>,
        target_ty: TyId<'db>,
    ) -> Self {
        let binding = provider.binding(db);
        let space = binding
            .semantics
            .address_space
            .map(HandleAddressSpace::Known)
            .unwrap_or_else(|| {
                if binding.semantics.kind == ProviderKind::RootObject {
                    HandleAddressSpace::Known(ProviderAddressSpace::Memory)
                } else {
                    HandleAddressSpace::Unspecified
                }
            });
        Self {
            origin: ExternalOrigin::Provider {
                provider,
                target_ty,
            },
            contract: ReferentContract::new(db, target_ty, space),
            dereferences: Box::new([]),
            reachable: false,
            uncertain: false,
        }
    }

    pub fn opaque(db: &'db dyn HirAnalysisDb, handle: OpaqueHandleRef<'db>) -> Self {
        Self {
            contract: ReferentContract::new(
                db,
                handle.contract.target_ty,
                handle.contract.address_space,
            ),
            origin: ExternalOrigin::OpaqueHandle(handle),
            dereferences: Box::new([]),
            reachable: false,
            uncertain: true,
        }
    }

    pub fn follow(
        &self,
        path: RegionPath<IndexExpr<'db>>,
        contract: ReferentContract<'db>,
        uncertain: bool,
    ) -> Self {
        let mut source = self.clone();
        source.contract = contract;
        source.uncertain |= uncertain;
        if source.reachable || source.dereferences.len() >= InputSource::MAX_DEREFERENCES {
            return source.widen();
        }
        let mut dereferences = source.dereferences.to_vec();
        dereferences.push(path);
        source.dereferences = dereferences.into();
        source
    }

    pub fn widen(mut self) -> Self {
        if let ExternalOrigin::Input(input) = &self.origin {
            self.origin = ExternalOrigin::Input(InputSource::reachable(input.param()));
        }
        self.dereferences = Box::new([]);
        self.reachable = true;
        self.uncertain = true;
        self
    }

    pub fn is_reachable(&self) -> bool {
        self.reachable
    }
    pub fn uncertain(&self) -> bool {
        self.uncertain || self.reachable
    }
    pub fn dereferences(&self) -> &[RegionPath<IndexExpr<'db>>] {
        &self.dereferences
    }
    pub fn param(&self) -> Option<u32> {
        match &self.origin {
            ExternalOrigin::Input(input) => Some(input.param()),
            _ => None,
        }
    }

    pub fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>> + '_ {
        let (input, handle) = match &self.origin {
            ExternalOrigin::Input(input) => (Some(input), None),
            ExternalOrigin::OpaqueHandle(handle) => (None, Some(handle)),
            ExternalOrigin::Provider { .. } | ExternalOrigin::Local(_) => (None, None),
        };
        input
            .into_iter()
            .flat_map(InputSource::indices)
            .chain(
                handle
                    .into_iter()
                    .flat_map(|handle| handle.arguments.iter().copied()),
            )
            .chain(self.dereferences.iter().flat_map(RegionPath::indices))
    }

    pub fn substitute(&self, db: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
        let mut result = self.rename_indices(subst);
        result.contract = self.contract.substitute(db, subst);
        match &self.origin {
            ExternalOrigin::OpaqueHandle(handle) => {
                result.origin = ExternalOrigin::OpaqueHandle(handle.substitute(db, subst))
            }
            ExternalOrigin::Provider {
                provider,
                target_ty,
            } => {
                result.origin = ExternalOrigin::Provider {
                    provider: *provider,
                    target_ty: target_ty.fold_with(db, &mut subst.clone()),
                }
            }
            ExternalOrigin::Input(_) | ExternalOrigin::Local(_) => {}
        }
        result
    }

    pub(super) fn rename_indices(&self, subst: &IndexSubst<'db>) -> Self {
        let origin = match &self.origin {
            ExternalOrigin::Local(root) => ExternalOrigin::Local(*root),
            ExternalOrigin::Input(input) => ExternalOrigin::Input(input.substitute(subst)),
            ExternalOrigin::Provider {
                provider,
                target_ty,
            } => ExternalOrigin::Provider {
                provider: *provider,
                target_ty: *target_ty,
            },
            ExternalOrigin::OpaqueHandle(handle) => ExternalOrigin::OpaqueHandle(OpaqueHandleRef {
                arguments: handle
                    .arguments
                    .iter()
                    .map(|index| subst.apply(*index))
                    .collect(),
                ..handle.clone()
            }),
        };
        Self {
            origin,
            contract: self.contract,
            dereferences: self
                .dereferences
                .iter()
                .map(|path| path.substitute(subst))
                .collect(),
            reachable: self.reachable,
            uncertain: self.uncertain,
        }
    }

    /// Storage-family matching is distinct from a semantic coverage proof.
    /// A widened typed cell can be read, but never supports a strong update.
    pub fn match_instance(
        &self,
        db: &'db dyn HirAnalysisDb,
        scope: &BinderScope,
        instance: &Self,
        instance_scope: &BinderScope,
    ) -> Option<(IndexSubst<'db>, Guard<'db>)> {
        let mut bindings = BTreeMap::new();
        for (formal, actual) in self.indices().zip(instance.indices()) {
            if matches!(formal, IndexExpr::Bound(_)) {
                bindings.entry(formal).or_insert(actual);
            }
        }
        let subst = IndexSubst::new(scope, instance_scope, bindings).ok()?;
        let guard = self
            .substitute(db, &subst)
            .identity_guard(instance, Guard::always(instance_scope))?;
        Some((subst, guard))
    }

    fn identity_guard(&self, other: &Self, mut guard: Guard<'db>) -> Option<Guard<'db>> {
        if self.contract != other.contract
            || self.reachable != other.reachable
            || self.dereferences.len() != other.dereferences.len()
        {
            return None;
        }
        guard = match (&self.origin, &other.origin) {
            (ExternalOrigin::Local(left), ExternalOrigin::Local(right)) if left == right => guard,
            (ExternalOrigin::Input(left), ExternalOrigin::Input(right)) if self.reachable => {
                (left.param() == right.param()).then_some(guard)?
            }
            (ExternalOrigin::Input(left), ExternalOrigin::Input(right)) => {
                left.alias_guard(right, guard, false)?
            }
            (
                ExternalOrigin::Provider {
                    provider: left,
                    target_ty: left_ty,
                },
                ExternalOrigin::Provider {
                    provider: right,
                    target_ty: right_ty,
                },
            ) if left == right && left_ty == right_ty => guard,
            (ExternalOrigin::OpaqueHandle(left), ExternalOrigin::OpaqueHandle(right))
                if left.occurrence == right.occurrence
                    && left.contract == right.contract
                    && left.arguments.len() == right.arguments.len() =>
            {
                for (left, right) in left.arguments.iter().zip(&right.arguments) {
                    guard = guard.with_equality(*left, *right)?;
                }
                guard
            }
            _ => return None,
        };
        for (left, right) in self.dereferences.iter().zip(&other.dereferences) {
            if left.as_slice().len() != right.as_slice().len() {
                return None;
            }
            guard = path_alias_guard(left.as_slice(), right.as_slice(), guard, false)?;
        }
        Some(guard)
    }

    pub(super) fn alias_guard(
        &self,
        other: &Self,
        guard: Guard<'db>,
        allow_unknown: bool,
    ) -> Option<Guard<'db>> {
        if !self.reachable
            && !other.reachable
            && let Some(exact) = self.identity_guard(other, guard.clone())
        {
            return Some(exact);
        }
        (allow_unknown
            && (self.uncertain() || other.uncertain())
            && self.contract.may_alias(other.contract))
        .then_some(guard)
    }
}
