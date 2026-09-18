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
    source::{InputSource, SourceExpr},
    value::IndexPayload,
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
        // A symbolic length or layout constant does not hide capability fields.
        // Only unresolved type structure requires conservative reachability.
        matches!(
            self.ty.base_ty(db).data(db),
            TyData::TyParam(_) | TyData::AssocTy(_) | TyData::QualifiedTy(_)
        ) || self
            .ty
            .generic_args(db)
            .iter()
            .any(|ty| Self { ty: *ty, ..self }.is_abstract(db))
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
    /// An allocator-created object. Its identity is distinct from every older input.
    Allocation(OpaqueHandleRef<'db>),
    /// A typed interpretation of raw memory at an element-scaled offset.
    /// This is a memory location, never a structural Index on a scalar type.
    Memory {
        base: Box<SourceExpr<'db>>,
        element: Option<(TyId<'db>, IndexExpr<'db>)>,
        target_ty: TyId<'db>,
    },
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

    pub fn allocation(db: &'db dyn HirAnalysisDb, allocation: OpaqueHandleRef<'db>) -> Self {
        let mut source = Self::opaque(db, allocation.clone());
        source.origin = ExternalOrigin::Allocation(allocation);
        source.uncertain = false;
        source
    }

    pub fn memory(
        db: &'db dyn HirAnalysisDb,
        mut base: SourceExpr<'db>,
        target_ty: TyId<'db>,
        mut element: Option<(TyId<'db>, IndexExpr<'db>)>,
    ) -> Self {
        if element.is_some_and(|(_, index)| index == IndexExpr::Const(0)) {
            element = None;
        }
        // Repeated casts at the same address retain one physical base.
        if base.path.is_empty()
            && base.views.iter().next().is_none()
            && base.source.dereferences.is_empty()
            && !base.source.reachable
            && let ExternalOrigin::Memory {
                base: original,
                element: old,
                ..
            } = &base.source.origin
            && (element.is_none() || old.is_none())
        {
            element = element.or(*old);
            base = *original.clone();
        }
        if element.is_none()
            && base.path.is_empty()
            && base.views.iter().next().is_none()
            && base.source.contract.ty == target_ty
        {
            return base.source;
        }
        Self {
            contract: ReferentContract::new(db, target_ty, base.source.contract.address_space),
            uncertain: base.source.uncertain(),
            origin: ExternalOrigin::Memory {
                base: Box::new(base),
                element,
                target_ty,
            },
            dereferences: Box::new([]),
            reachable: false,
        }
    }

    /// Rewrites construction occurrences through every nested memory base.
    pub fn map_occurrences(&mut self, f: &mut impl FnMut(&mut OpaqueHandleRef<'db>)) {
        match &mut self.origin {
            ExternalOrigin::OpaqueHandle(source) | ExternalOrigin::Allocation(source) => f(source),
            ExternalOrigin::Memory { base, .. } => base.source.map_occurrences(f),
            _ => {}
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

    /// Direct bytes in an allocation created by this invocation cannot alias
    /// any caller loan. Following a pointer stored there loses that guarantee.
    pub fn is_fresh_allocation(&self) -> bool {
        !self.is_reachable()
            && self.dereferences.is_empty()
            && match &self.origin {
                ExternalOrigin::Allocation(_) => true,
                ExternalOrigin::Memory { base, .. } => base.source.is_fresh_allocation(),
                _ => false,
            }
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
    /// Entry pointers cannot refer to storage first created in the callee's
    /// frame. Once overwritten, their explicit replacement regions take over.
    pub fn is_incoming(&self) -> bool {
        match &self.origin {
            ExternalOrigin::Input(_) => true,
            ExternalOrigin::Memory { base, .. } => base.source.is_incoming(),
            _ => false,
        }
    }

    pub fn param(&self) -> Option<u32> {
        match &self.origin {
            ExternalOrigin::Input(input) => Some(input.param()),
            ExternalOrigin::Memory { base, .. } => base.source.param(),
            _ => None,
        }
    }

    pub fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>> + '_ {
        let indices: Vec<_> = match &self.origin {
            ExternalOrigin::Input(input) => input.indices().collect(),
            ExternalOrigin::OpaqueHandle(handle) | ExternalOrigin::Allocation(handle) => {
                handle.arguments.to_vec()
            }
            ExternalOrigin::Memory { base, element, .. } => base
                .indices()
                .chain(element.iter().map(|(_, index)| *index))
                .collect(),
            ExternalOrigin::Provider { .. } | ExternalOrigin::Local(_) => Vec::new(),
        };
        indices
            .into_iter()
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
            ExternalOrigin::Allocation(handle) => {
                result.origin = ExternalOrigin::Allocation(handle.substitute(db, subst));
            }
            ExternalOrigin::Memory {
                base,
                element,
                target_ty,
            } => {
                result.origin = ExternalOrigin::Memory {
                    target_ty: target_ty.fold_with(db, &mut subst.clone()),
                    base: Box::new(base.substitute(db, subst)),
                    element: element.map(|(ty, index)| {
                        (ty.fold_with(db, &mut subst.clone()), subst.apply(index))
                    }),
                };
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
            ExternalOrigin::Memory {
                base,
                element,
                target_ty,
            } => ExternalOrigin::Memory {
                target_ty: *target_ty,
                base: Box::new(SourceExpr {
                    source: base.source.rename_indices(subst),
                    path: base.path.substitute(subst),
                    views: base.views.clone(),
                }),
                element: element.map(|(ty, index)| (ty, subst.apply(index))),
            },
            ExternalOrigin::Allocation(handle) => ExternalOrigin::Allocation(OpaqueHandleRef {
                arguments: handle
                    .arguments
                    .iter()
                    .map(|index| subst.apply(*index))
                    .collect(),
                ..handle.clone()
            }),
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
            (
                ExternalOrigin::Memory {
                    base: left,
                    element: left_element,
                    target_ty: left_ty,
                },
                ExternalOrigin::Memory {
                    base: right,
                    element: right_element,
                    target_ty: right_ty,
                },
            ) if left_ty == right_ty
                && left.views == right.views
                && left.path.as_slice().len() == right.path.as_slice().len() =>
            {
                guard = left.source.identity_guard(&right.source, guard)?;
                guard =
                    path_alias_guard(left.path.as_slice(), right.path.as_slice(), guard, false)?;
                match (left_element, right_element) {
                    (None, None) => guard,
                    (Some((left_ty, left)), Some((right_ty, right))) if left_ty == right_ty => {
                        guard.with_equality(*left, *right)?
                    }
                    _ => return None,
                }
            }
            (ExternalOrigin::OpaqueHandle(left), ExternalOrigin::OpaqueHandle(right))
            | (ExternalOrigin::Allocation(left), ExternalOrigin::Allocation(right))
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
        if !allow_unknown {
            return None;
        }
        if self.dereferences.is_empty()
            && !self.reachable
            && let ExternalOrigin::Memory {
                base: left,
                element: left_element,
                ..
            } = &self.origin
        {
            let other_base = match &other.origin {
                ExternalOrigin::Memory {
                    base,
                    element: right_element,
                    ..
                } if other.dereferences.is_empty() && !other.reachable => {
                    if left.source == base.source
                        && left.path == base.path
                        && left.views == base.views
                        && let (Some((left_ty, left_index)), Some((right_ty, right_index))) =
                            (left_element, right_element)
                        && left_ty == right_ty
                    {
                        return guard.with_equality(*left_index, *right_index);
                    }
                    Some(&**base)
                }
                _ => None,
            };
            let right = other_base.map_or(other, |base| &base.source);
            return left.source.alias_guard(right, guard, true);
        }
        if other.dereferences.is_empty()
            && !other.reachable
            && let ExternalOrigin::Memory { base: right, .. } = &other.origin
        {
            return self.alias_guard(&right.source, guard, true);
        }
        // Distinct fresh allocations and incoming pointers cannot identify the
        // same object. Unknown manufactured addresses remain conservative.
        if matches!(
            (&self.origin, &other.origin),
            (
                ExternalOrigin::Allocation(_),
                ExternalOrigin::Input(_) | ExternalOrigin::Allocation(_)
            ) | (ExternalOrigin::Input(_), ExternalOrigin::Allocation(_))
        ) && self.dereferences.is_empty()
            && other.dereferences.is_empty()
        {
            return None;
        }
        (allow_unknown
            && (self.uncertain() || other.uncertain())
            && self.contract.may_alias(other.contract))
        .then_some(guard)
    }
}
