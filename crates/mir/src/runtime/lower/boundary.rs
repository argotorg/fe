use std::borrow::Cow;

use common::indexmap::IndexSet;
use hir::analysis::{
    semantic::SLocalId,
    ty::{
        trait_resolution::PredicateListId,
        ty_def::{BorrowKind, CapabilityKind, TyId},
        ty_is_known_copy,
    },
};
use hir::hir_def::scope_graph::ScopeId;
use rustc_hash::FxHashMap;

use crate::{
    db::MirDb,
    runtime::{
        AddressSpaceKind, BorrowAccess, BorrowTransportSet, LayoutId, RawPointeeId, RefKind,
        RefView, RuntimeBoundarySpec, RuntimeCarrier, RuntimeClass, RuntimePlace,
        relation::{raw_pointee_matches_class, runtime_classes_equivalent},
    },
};

use super::{
    classify::{BodyEnv, InferClassCache, carrier_value_class_ref},
    type_info::{
        RuntimeTypeEnv, provider_class_for_target_in_env, runtime_interface_ty_in_env,
        runtime_repr_ty_in_env, runtime_transport_sensitive_aggregate, runtime_zero_sized_ty,
        stored_class_for_ty_in_env, top_level_class_for_ty_in_env,
    },
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct BoundarySiteId(u32);

#[derive(Clone, Debug)]
pub(super) struct StagedBoundary<'db> {
    site: BoundarySiteId,
    pub(super) boundary: RuntimeBoundarySpec<'db>,
    matcher: BoundaryShapeMatcher<'db>,
}

#[derive(Clone, Copy)]
pub(super) struct BoundaryRef<'a, 'db> {
    site: Option<BoundarySiteId>,
    boundary: &'a RuntimeBoundarySpec<'db>,
    matcher: Option<&'a BoundaryShapeMatcher<'db>>,
}

impl<'a, 'db> BoundaryRef<'a, 'db> {
    pub(super) fn unstaged(boundary: &'a RuntimeBoundarySpec<'db>) -> Self {
        Self {
            site: None,
            boundary,
            matcher: None,
        }
    }

    pub(super) fn staged(boundary: &'a StagedBoundary<'db>) -> Self {
        Self {
            site: Some(boundary.site),
            boundary: &boundary.boundary,
            matcher: Some(&boundary.matcher),
        }
    }
}

#[derive(Default)]
pub(super) struct BoundarySiteAllocator {
    next: u32,
}

impl BoundarySiteAllocator {
    pub(super) fn stage<'db>(&mut self, boundary: RuntimeBoundarySpec<'db>) -> StagedBoundary<'db> {
        let site = BoundarySiteId(self.next);
        self.next += 1;
        let matcher = BoundaryShapeMatcher::for_boundary(&boundary);
        StagedBoundary {
            site,
            boundary,
            matcher,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct BoundarySpecializationCacheKey<'db> {
    local: SLocalId,
    site: BoundarySiteId,
    aggregate_layout: Option<LayoutId<'db>>,
}

#[derive(Clone, Debug)]
pub(super) enum BoundarySpecializationCacheValue<'db> {
    Unchanged,
    Specialized {
        boundary: RuntimeBoundarySpec<'db>,
        matcher: BoundaryShapeMatcher<'db>,
    },
}

pub(super) type BoundarySpecializationCache<'db> =
    FxHashMap<BoundarySpecializationCacheKey<'db>, BoundarySpecializationCacheValue<'db>>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum RuntimeClassShape<'db> {
    Scalar(crate::runtime::ScalarClass<'db>),
    AggregateValue {
        layout: LayoutId<'db>,
    },
    Ref {
        pointee: Box<RuntimeClassShape<'db>>,
        kind: RefShapeKind,
        view: RefView<'db>,
    },
    /// Raw targets stay unresolved leaves; matchers resolve them on demand.
    RawAddr {
        space: AddressSpaceKind,
        pointee: Option<RawPointeeId<'db>>,
    },
}

impl<'db> RuntimeClassShape<'db> {
    pub(super) fn from_class(class: &RuntimeClass<'db>) -> Self {
        match class {
            RuntimeClass::Scalar(class) => Self::Scalar(class.clone()),
            RuntimeClass::AggregateValue { layout } => Self::AggregateValue { layout: *layout },
            RuntimeClass::Ref {
                pointee,
                kind,
                view,
            } => Self::Ref {
                pointee: Box::new(Self::from_class(pointee)),
                kind: RefShapeKind::from_kind(kind),
                view: view.clone(),
            },
            RuntimeClass::RawAddr { space, pointee } => Self::RawAddr {
                space: *space,
                pointee: *pointee,
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) enum RefShapeKind {
    Native,
    Const,
    Object,
    Provider(AddressSpaceKind),
}

impl RefShapeKind {
    fn from_kind(kind: &RefKind<'_>) -> Self {
        match kind {
            RefKind::Native => Self::Native,
            RefKind::Const => Self::Const,
            RefKind::Object => Self::Object,
            RefKind::Provider { space, .. } => Self::Provider(*space),
        }
    }
}

#[derive(Clone, Debug)]
pub(super) enum BoundaryShapeMatcher<'db> {
    Exact(ExactBoundaryShapeMatcher<'db>),
    BorrowLike {
        pointee: RuntimeClassShape<'db>,
        allow_object: bool,
        allow_const: bool,
        provider_spaces: Box<[AddressSpaceKind]>,
        allow_raw_addr: bool,
    },
}

impl<'db> BoundaryShapeMatcher<'db> {
    fn for_boundary(boundary: &RuntimeBoundarySpec<'db>) -> Self {
        match boundary {
            RuntimeBoundarySpec::ExactTransport(class) | RuntimeBoundarySpec::ExactShape(class) => {
                Self::Exact(ExactBoundaryShapeMatcher::for_class(class))
            }
            RuntimeBoundarySpec::BorrowLike { pointee, allow, .. } => Self::BorrowLike {
                pointee: RuntimeClassShape::from_class(pointee),
                allow_object: allow.allow_object,
                allow_const: allow.allow_const,
                provider_spaces: allow.provider_spaces.clone(),
                allow_raw_addr: allow.allow_raw_addr,
            },
        }
    }

    pub(super) fn matches_shape(
        &self,
        db: &'db dyn MirDb,
        actual: &RuntimeClassShape<'db>,
    ) -> bool {
        match self {
            Self::Exact(matcher) => matcher.matches_shape(db, actual),
            Self::BorrowLike {
                pointee,
                allow_object,
                allow_const,
                provider_spaces,
                allow_raw_addr,
            } => match actual {
                RuntimeClassShape::Ref {
                    pointee: actual_pointee,
                    kind: RefShapeKind::Object | RefShapeKind::Native,
                    view: RefView::Whole,
                } => *allow_object && **actual_pointee == *pointee,
                RuntimeClassShape::Ref {
                    pointee: actual_pointee,
                    kind: RefShapeKind::Const,
                    view: RefView::Whole,
                } => *allow_const && **actual_pointee == *pointee,
                RuntimeClassShape::Ref {
                    pointee: actual_pointee,
                    kind: RefShapeKind::Provider(space),
                    view: RefView::Whole,
                } => provider_spaces.contains(space) && **actual_pointee == *pointee,
                RuntimeClassShape::RawAddr {
                    pointee: Some(actual_pointee),
                    ..
                } => *allow_raw_addr && raw_target_shape(db, *actual_pointee) == *pointee,
                RuntimeClassShape::RawAddr { pointee: None, .. } => false,
                RuntimeClassShape::Scalar(_)
                | RuntimeClassShape::AggregateValue { .. }
                | RuntimeClassShape::Ref {
                    view: RefView::EnumVariant(_),
                    ..
                } => false,
            },
        }
    }
}

#[derive(Clone, Debug)]
pub(super) enum ExactBoundaryShapeMatcher<'db> {
    Scalar(crate::runtime::ScalarClass<'db>),
    AggregateValue(LayoutId<'db>),
    Ref {
        pointee: RuntimeClassShape<'db>,
        view: RefView<'db>,
    },
    RawAddr {
        pointee: Option<RawPointeeId<'db>>,
    },
}

impl<'db> ExactBoundaryShapeMatcher<'db> {
    fn for_class(class: &RuntimeClass<'db>) -> Self {
        match class {
            RuntimeClass::Scalar(class) => Self::Scalar(class.clone()),
            RuntimeClass::AggregateValue { layout } => Self::AggregateValue(*layout),
            RuntimeClass::Ref { pointee, view, .. } => Self::Ref {
                pointee: RuntimeClassShape::from_class(pointee),
                view: view.clone(),
            },
            RuntimeClass::RawAddr { pointee, .. } => Self::RawAddr { pointee: *pointee },
        }
    }

    fn matches_shape(&self, db: &'db dyn MirDb, actual: &RuntimeClassShape<'db>) -> bool {
        match (self, actual) {
            (Self::Scalar(expected), RuntimeClassShape::Scalar(actual)) => actual == expected,
            (Self::AggregateValue(expected), RuntimeClassShape::AggregateValue { layout }) => {
                layout == expected
            }
            (
                Self::Ref { pointee, view, .. },
                RuntimeClassShape::Ref {
                    pointee: actual_pointee,
                    view: actual_view,
                    ..
                },
            ) => **actual_pointee == *pointee && actual_view == view,
            (
                Self::Ref { pointee, .. },
                RuntimeClassShape::RawAddr {
                    pointee: actual, ..
                },
            ) => actual.is_some_and(|actual| raw_target_shape(db, actual) == *pointee),
            // Raw-to-raw shape preservation requires the same target recipe;
            // an equivalent spelling is adapted to the declared target instead.
            (Self::RawAddr { pointee: expected }, RuntimeClassShape::RawAddr { pointee, .. }) => {
                pointee == expected
            }
            _ => false,
        }
    }
}

fn raw_target_shape<'db>(db: &'db dyn MirDb, pointee: RawPointeeId<'db>) -> RuntimeClassShape<'db> {
    RuntimeClassShape::from_class(&pointee.target(db))
}

pub(super) struct SpecializedBoundary<'a, 'db> {
    pub(super) boundary: Cow<'a, RuntimeBoundarySpec<'db>>,
    matcher: BoundaryShapeMatcher<'db>,
}

pub(super) fn specialize_boundary_for_runtime_source_in_context<'a, 'db>(
    env: BodyEnv<'_, 'db>,
    local: SLocalId,
    boundary: BoundaryRef<'a, 'db>,
    carriers: &[RuntimeCarrier<'db>],
    mut class_cache: Option<&mut InferClassCache<'db>>,
) -> SpecializedBoundary<'a, 'db> {
    let aggregate_layout = class_cache
        .as_deref_mut()
        .and_then(|cache| cache.local_dynamic_facts(env, local, carriers))
        .and_then(|facts| facts.aggregate_layout)
        .or_else(|| {
            env.boundary_source_transport_sensitive(local)
                .then(|| env.actual_aggregate_class_for_source(carriers, local))
                .flatten()
                .and_then(|class| class.aggregate_layout())
        });
    if let (Some(site), Some(cache)) = (
        boundary.site,
        class_cache
            .as_deref_mut()
            .map(|cache| &mut cache.boundary_specializations),
    ) {
        let key = BoundarySpecializationCacheKey {
            local,
            site,
            aggregate_layout,
        };
        if let Some(cached) = cache.get(&key) {
            let specialized = match cached {
                BoundarySpecializationCacheValue::Unchanged => SpecializedBoundary {
                    boundary: Cow::Borrowed(boundary.boundary),
                    matcher: boundary
                        .matcher
                        .cloned()
                        .unwrap_or_else(|| BoundaryShapeMatcher::for_boundary(boundary.boundary)),
                },
                BoundarySpecializationCacheValue::Specialized { boundary, matcher } => {
                    SpecializedBoundary {
                        boundary: Cow::Owned(boundary.clone()),
                        matcher: matcher.clone(),
                    }
                }
            };
            return preserve_actual_shape_boundary_for_runtime_source(
                env,
                local,
                specialized,
                carriers,
                class_cache,
            );
        }
        let specialized_boundary =
            specialize_boundary_for_aggregate_layout(env.db(), boundary.boundary, aggregate_layout);
        let specialized_matcher = match &specialized_boundary {
            Cow::Borrowed(_) => boundary
                .matcher
                .cloned()
                .unwrap_or_else(|| BoundaryShapeMatcher::for_boundary(boundary.boundary)),
            Cow::Owned(boundary) => BoundaryShapeMatcher::for_boundary(boundary),
        };
        cache.insert(
            key,
            match &specialized_boundary {
                Cow::Borrowed(_) => BoundarySpecializationCacheValue::Unchanged,
                Cow::Owned(boundary) => BoundarySpecializationCacheValue::Specialized {
                    boundary: boundary.clone(),
                    matcher: specialized_matcher.clone(),
                },
            },
        );
        return preserve_actual_shape_boundary_for_runtime_source(
            env,
            local,
            SpecializedBoundary {
                boundary: specialized_boundary,
                matcher: specialized_matcher,
            },
            carriers,
            class_cache,
        );
    }
    preserve_actual_shape_boundary_for_runtime_source(
        env,
        local,
        SpecializedBoundary {
            matcher: boundary
                .matcher
                .cloned()
                .unwrap_or_else(|| BoundaryShapeMatcher::for_boundary(boundary.boundary)),
            boundary: specialize_boundary_for_aggregate_layout(
                env.db(),
                boundary.boundary,
                aggregate_layout,
            ),
        },
        carriers,
        class_cache,
    )
}

pub(super) fn specialize_boundary_for_aggregate_layout<'a, 'db>(
    db: &'db dyn MirDb,
    boundary: &'a RuntimeBoundarySpec<'db>,
    aggregate_layout: Option<LayoutId<'db>>,
) -> Cow<'a, RuntimeBoundarySpec<'db>> {
    match boundary {
        RuntimeBoundarySpec::ExactTransport(desired) => {
            match specialize_exact_boundary_for_aggregate_layout(db, desired, aggregate_layout) {
                Cow::Borrowed(_) => Cow::Borrowed(boundary),
                Cow::Owned(class) => Cow::Owned(RuntimeBoundarySpec::ExactTransport(class)),
            }
        }
        RuntimeBoundarySpec::ExactShape(desired) => {
            match specialize_exact_boundary_for_aggregate_layout(db, desired, aggregate_layout) {
                Cow::Borrowed(_) => Cow::Borrowed(boundary),
                Cow::Owned(class) => Cow::Owned(RuntimeBoundarySpec::ExactShape(class)),
            }
        }
        RuntimeBoundarySpec::BorrowLike {
            pointee:
                RuntimeClass::AggregateValue {
                    layout: desired_layout,
                },
            access,
            allow,
        } => match aggregate_layout {
            Some(layout) if layout != *desired_layout => {
                Cow::Owned(RuntimeBoundarySpec::BorrowLike {
                    pointee: RuntimeClass::AggregateValue { layout },
                    access: *access,
                    allow: allow.clone(),
                })
            }
            Some(_) | None => Cow::Borrowed(boundary),
        },
        RuntimeBoundarySpec::BorrowLike { .. } => Cow::Borrowed(boundary),
    }
}

fn specialize_exact_boundary_for_aggregate_layout<'a, 'db>(
    db: &'db dyn MirDb,
    desired: &'a RuntimeClass<'db>,
    aggregate_layout: Option<LayoutId<'db>>,
) -> Cow<'a, RuntimeClass<'db>> {
    match (desired, aggregate_layout) {
        (_, None) => Cow::Borrowed(desired),
        (
            RuntimeClass::AggregateValue {
                layout: desired_layout,
            },
            Some(layout),
        ) if layout == *desired_layout => Cow::Borrowed(desired),
        (RuntimeClass::AggregateValue { .. }, Some(layout)) => {
            Cow::Owned(RuntimeClass::AggregateValue { layout })
        }
        (
            RuntimeClass::Ref {
                pointee,
                kind,
                view,
            },
            Some(layout),
        ) if pointee.aggregate_layout().is_some() && pointee.aggregate_layout() != Some(layout) => {
            Cow::Owned(RuntimeClass::Ref {
                pointee: Box::new(RuntimeClass::AggregateValue { layout }),
                kind: kind.clone(),
                view: view.clone(),
            })
        }
        (RuntimeClass::Ref { .. }, Some(_)) => Cow::Borrowed(desired),
        (
            RuntimeClass::RawAddr {
                space,
                pointee: Some(pointee),
            },
            Some(layout),
        ) if pointee
            .target(db)
            .aggregate_layout()
            .is_some_and(|target| target != layout) =>
        {
            Cow::Owned(RuntimeClass::raw_addr(
                db,
                *space,
                RuntimeClass::AggregateValue { layout },
            ))
        }
        (RuntimeClass::Scalar(_) | RuntimeClass::RawAddr { .. }, Some(_)) => Cow::Borrowed(desired),
    }
}

fn preserve_actual_shape_boundary_for_runtime_source<'a, 'db>(
    env: BodyEnv<'_, 'db>,
    local: SLocalId,
    boundary: SpecializedBoundary<'a, 'db>,
    carriers: &[RuntimeCarrier<'db>],
    class_cache: Option<&mut InferClassCache<'db>>,
) -> SpecializedBoundary<'a, 'db> {
    if !matches!(
        boundary.boundary.as_ref(),
        RuntimeBoundarySpec::ExactShape(_)
    ) {
        return boundary;
    }
    let actual_matches = if let Some(class_cache) = class_cache {
        class_cache
            .local_dynamic_facts(env, local, carriers)
            .and_then(|facts| facts.exact_source_shape)
            .is_some_and(|shape| boundary.matcher.matches_shape(env.db(), &shape))
    } else if let Some(actual) = carrier_value_class_ref(local, carriers) {
        boundary
            .matcher
            .matches_shape(env.db(), &RuntimeClassShape::from_class(actual))
    } else {
        env.semantic_value_class(carriers, local)
            .as_ref()
            .map(RuntimeClassShape::from_class)
            .is_some_and(|shape| boundary.matcher.matches_shape(env.db(), &shape))
    };
    if !actual_matches {
        return boundary;
    }
    let actual = if let Some(actual) = carrier_value_class_ref(local, carriers) {
        actual.clone()
    } else {
        let Some(actual) = env.semantic_value_class(carriers, local) else {
            return boundary;
        };
        actual
    };
    let actual = BoundaryMatcher::retarget_accepted_class(env.db(), &actual, &boundary.boundary);
    SpecializedBoundary {
        matcher: BoundaryShapeMatcher::for_boundary(&RuntimeBoundarySpec::ExactShape(
            actual.clone(),
        )),
        boundary: Cow::Owned(RuntimeBoundarySpec::ExactShape(actual)),
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum RuntimeValueMaterialization<'db> {
    ObjectRef(LayoutId<'db>),
    /// A fresh memory slot holding `pointee`, passed by its raw address.
    /// `target` is `pointee` interned as the address's exact target.
    RawAddrSlot {
        pointee: RuntimeClass<'db>,
        target: RawPointeeId<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum RuntimeValueUsePlan<'db> {
    UseValue,
    AddrOfRuntimePlace {
        place: RuntimePlace<'db>,
        class: RuntimeClass<'db>,
    },
    CoerceValue(RuntimeClass<'db>),
    MaterializeValue(RuntimeValueMaterialization<'db>),
}

impl<'db> RuntimeValueUsePlan<'db> {
    pub(crate) fn class(&self, source: &RuntimeClass<'db>) -> RuntimeClass<'db> {
        match self {
            Self::UseValue => source.clone(),
            Self::AddrOfRuntimePlace { class, .. } => class.clone(),
            Self::CoerceValue(target) => target.clone(),
            Self::MaterializeValue(materialization) => materialization.class(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct RuntimeValueAddress<'db> {
    pub(crate) place: RuntimePlace<'db>,
    pub(crate) class: RuntimeClass<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct RuntimeValueSource<'db> {
    pub(crate) value: RuntimeClass<'db>,
    pub(crate) address: Option<RuntimeValueAddress<'db>>,
}

pub(crate) struct RuntimeValueUsePlanner;

impl RuntimeValueUsePlanner {
    pub(crate) fn select<'db>(
        db: &'db dyn MirDb,
        source: RuntimeValueSource<'db>,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> Option<RuntimeValueUsePlan<'db>> {
        match boundary {
            RuntimeBoundarySpec::ExactTransport(target) => {
                Some(RuntimeValueUsePlan::CoerceValue(target.clone()))
            }
            RuntimeBoundarySpec::ExactShape(target) => {
                if let Some(plan) = source.value_use_plan(db, boundary) {
                    return Some(plan);
                }
                if let Some(address) = source.compatible_address(db, boundary) {
                    return Some(RuntimeValueUsePlan::AddrOfRuntimePlace {
                        place: address.place,
                        class: address.class,
                    });
                }
                Some(RuntimeValueUsePlan::CoerceValue(target.clone()))
            }
            RuntimeBoundarySpec::BorrowLike { .. } => {
                if let Some(plan) = source.value_use_plan(db, boundary) {
                    return Some(plan);
                }
                if let Some(address) = source.compatible_address(db, boundary) {
                    return Some(RuntimeValueUsePlan::AddrOfRuntimePlace {
                        place: address.place,
                        class: address.class,
                    });
                }
                RuntimeValueMaterialization::for_boundary(db, boundary)
                    .map(RuntimeValueUsePlan::MaterializeValue)
            }
        }
    }
}

pub(crate) struct BoundaryMatcher;

impl BoundaryMatcher {
    pub(crate) fn class_satisfies_boundary<'db>(
        db: &'db dyn MirDb,
        class: &RuntimeClass<'db>,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> bool {
        match boundary {
            RuntimeBoundarySpec::ExactTransport(expected) => class == expected,
            RuntimeBoundarySpec::ExactShape(expected) => {
                Self::class_matches_shape_boundary(db, class, expected)
            }
            RuntimeBoundarySpec::BorrowLike { pointee, allow, .. } => match class {
                RuntimeClass::Ref {
                    pointee: actual_pointee,
                    kind: RefKind::Object | RefKind::Native,
                    view: RefView::Whole,
                } => allow.allow_object && runtime_classes_equivalent(db, actual_pointee, pointee),
                RuntimeClass::Ref {
                    pointee: actual_pointee,
                    kind: RefKind::Const,
                    view: RefView::Whole,
                } => allow.allow_const && runtime_classes_equivalent(db, actual_pointee, pointee),
                RuntimeClass::Ref {
                    pointee: actual_pointee,
                    kind: RefKind::Provider { space, .. },
                    view: RefView::Whole,
                } => {
                    allow.provider_spaces.contains(space)
                        && runtime_classes_equivalent(db, actual_pointee, pointee)
                }
                RuntimeClass::Ref {
                    view: RefView::EnumVariant(_),
                    ..
                } => false,
                RuntimeClass::RawAddr {
                    pointee: Some(actual_pointee),
                    ..
                } => {
                    allow.allow_raw_addr && raw_pointee_matches_class(db, *actual_pointee, pointee)
                }
                RuntimeClass::RawAddr { pointee: None, .. } => false,
                RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. } => false,
            },
        }
    }

    /// The exact class `class` is passed as when it satisfies `boundary`.
    pub(crate) fn selected_class<'db>(
        db: &'db dyn MirDb,
        class: &RuntimeClass<'db>,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> Option<RuntimeClass<'db>> {
        Self::class_satisfies_boundary(db, class, boundary)
            .then(|| Self::retarget_accepted_class(db, class, boundary))
    }

    /// A raw address accepted by a boundary that describes its pointee by
    /// class is re-targeted to exactly that class. Source and exact spellings
    /// of the same target then select the same parameter class, so they
    /// share one runtime instance. The accepted address space is kept.
    pub(crate) fn retarget_accepted_class<'db>(
        db: &'db dyn MirDb,
        class: &RuntimeClass<'db>,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> RuntimeClass<'db> {
        let RuntimeClass::RawAddr {
            space,
            pointee: Some(_),
        } = class
        else {
            return class.clone();
        };
        let pointee = match boundary {
            RuntimeBoundarySpec::BorrowLike { pointee, .. } => pointee,
            RuntimeBoundarySpec::ExactShape(RuntimeClass::Ref { pointee, .. }) => pointee.as_ref(),
            RuntimeBoundarySpec::ExactTransport(_) | RuntimeBoundarySpec::ExactShape(_) => {
                return class.clone();
            }
        };
        RuntimeClass::raw_addr(db, *space, pointee.clone())
    }

    pub(crate) fn placeholder_class<'db>(
        db: &'db dyn MirDb,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> Option<RuntimeClass<'db>> {
        match boundary {
            RuntimeBoundarySpec::ExactTransport(class) | RuntimeBoundarySpec::ExactShape(class) => {
                Some(class.clone())
            }
            RuntimeBoundarySpec::BorrowLike { pointee, allow, .. }
                if pointee.aggregate_layout().is_some() && allow.allow_object =>
            {
                Some(RuntimeClass::Ref {
                    pointee: Box::new(pointee.clone()),
                    kind: RefKind::Object,
                    view: RefView::Whole,
                })
            }
            RuntimeBoundarySpec::BorrowLike { pointee, allow, .. }
                if pointee.aggregate_layout().is_some() && allow.allow_const =>
            {
                Some(RuntimeClass::Ref {
                    pointee: Box::new(pointee.clone()),
                    kind: RefKind::Const,
                    view: RefView::Whole,
                })
            }
            RuntimeBoundarySpec::BorrowLike { pointee, allow, .. } if allow.allow_raw_addr => Some(
                RuntimeClass::raw_addr(db, AddressSpaceKind::Memory, pointee.clone()),
            ),
            RuntimeBoundarySpec::BorrowLike { .. } => None,
        }
    }

    fn class_matches_shape_boundary<'db>(
        db: &'db dyn MirDb,
        actual: &RuntimeClass<'db>,
        expected: &RuntimeClass<'db>,
    ) -> bool {
        match (actual, expected) {
            (
                RuntimeClass::Ref {
                    pointee: actual_pointee,
                    view: actual_view,
                    ..
                },
                RuntimeClass::Ref {
                    pointee: expected_pointee,
                    view: expected_view,
                    ..
                },
            ) => {
                actual_view == expected_view
                    && runtime_classes_equivalent(db, actual_pointee, expected_pointee)
            }
            (
                RuntimeClass::RawAddr {
                    pointee: actual_pointee,
                    ..
                },
                RuntimeClass::Ref { pointee, .. },
            ) => actual_pointee.is_some_and(|actual_pointee| {
                raw_pointee_matches_class(db, actual_pointee, pointee)
            }),
            (
                RuntimeClass::RawAddr {
                    pointee: actual_pointee,
                    ..
                },
                RuntimeClass::RawAddr {
                    pointee: expected_pointee,
                    ..
                },
                // Same recipe only: an equivalent spelling is coerced to the
                // declared target rather than accepted as-is.
            ) => actual_pointee == expected_pointee,
            _ => actual == expected,
        }
    }
}

impl<'db> RuntimeValueMaterialization<'db> {
    pub(crate) fn for_boundary(
        db: &'db dyn MirDb,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> Option<Self> {
        match boundary {
            RuntimeBoundarySpec::BorrowLike { pointee, allow, .. }
                if pointee.aggregate_layout().is_some() && allow.allow_object =>
            {
                Some(Self::ObjectRef(
                    pointee.aggregate_layout().expect("aggregate layout"),
                ))
            }
            RuntimeBoundarySpec::BorrowLike { pointee, allow, .. }
                if pointee.aggregate_layout().is_none() && allow.allow_raw_addr =>
            {
                Some(Self::RawAddrSlot {
                    pointee: pointee.clone(),
                    target: RawPointeeId::exact(db, pointee.clone()),
                })
            }
            RuntimeBoundarySpec::ExactTransport(_)
            | RuntimeBoundarySpec::ExactShape(_)
            | RuntimeBoundarySpec::BorrowLike { .. } => None,
        }
    }

    pub(crate) fn class(&self) -> RuntimeClass<'db> {
        match self {
            Self::ObjectRef(layout) => RuntimeClass::object_ref(*layout),
            Self::RawAddrSlot { target, .. } => RuntimeClass::RawAddr {
                space: AddressSpaceKind::Memory,
                pointee: Some(*target),
            },
        }
    }
}

impl<'db> RuntimeValueSource<'db> {
    /// Uses the value as-is, or re-targets an accepted raw address.
    fn value_use_plan(
        &self,
        db: &'db dyn MirDb,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> Option<RuntimeValueUsePlan<'db>> {
        let selected = BoundaryMatcher::selected_class(db, &self.value, boundary)?;
        Some(if selected == self.value {
            RuntimeValueUsePlan::UseValue
        } else {
            RuntimeValueUsePlan::CoerceValue(selected)
        })
    }

    fn compatible_address(
        &self,
        db: &'db dyn MirDb,
        boundary: &RuntimeBoundarySpec<'db>,
    ) -> Option<RuntimeValueAddress<'db>> {
        let address = self.address.as_ref()?;
        Some(RuntimeValueAddress {
            place: address.place.clone(),
            class: BoundaryMatcher::selected_class(db, &address.class, boundary)?,
        })
    }
}

pub(crate) fn boundary_spec_for_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
    default_space: AddressSpaceKind,
) -> Option<RuntimeBoundarySpec<'db>> {
    runtime_boundary_spec(db, ty, default_space, env.scope, env.assumptions)
}

pub(crate) fn default_borrow_transport_set(
    access: BorrowAccess,
    default_space: AddressSpaceKind,
) -> BorrowTransportSet {
    let mut provider_spaces = IndexSet::new();
    provider_spaces.insert(default_space);
    provider_spaces.insert(AddressSpaceKind::Memory);
    provider_spaces.insert(AddressSpaceKind::Storage);
    provider_spaces.insert(AddressSpaceKind::Transient);
    if matches!(access, BorrowAccess::ReadOnly) {
        provider_spaces.insert(AddressSpaceKind::Calldata);
        provider_spaces.insert(AddressSpaceKind::Code);
    }
    BorrowTransportSet {
        allow_object: true,
        allow_const: matches!(access, BorrowAccess::ReadOnly),
        provider_spaces: provider_spaces.into_iter().collect(),
        allow_raw_addr: true,
    }
}

pub(crate) fn aggregate_transport_depends_on_runtime_source<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    runtime_transport_sensitive_aggregate(db, ty, scope, assumptions)
}

pub(crate) fn boundary_source_uses_transport_sensitive_aggregate<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    runtime_boundary_source_uses_transport_sensitive_aggregate(db, ty, scope, assumptions)
}

#[salsa::tracked]
fn runtime_boundary_spec<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    default_space: AddressSpaceKind,
    scope: Option<ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeBoundarySpec<'db>> {
    let env = RuntimeTypeEnv::new(scope, assumptions);
    let interface_ty = runtime_interface_ty_in_env(db, env, ty);
    if let Some((CapabilityKind::View, inner)) = interface_ty.as_capability(db) {
        let inner_boundary = runtime_boundary_spec(db, inner, default_space, scope, assumptions);
        if runtime_zero_sized_ty(db, inner, scope, assumptions) {
            return inner_boundary;
        }
        let pointee = stored_class_for_ty_in_env(db, env, inner);
        let inner_is_copy =
            scope.is_some_and(|scope| ty_is_known_copy(db, scope, inner, assumptions));
        if inner_is_copy && pointee.aggregate_layout().is_none() {
            return inner_boundary;
        }
        if pointee.aggregate_layout().is_none()
            && !matches!(
                &inner_boundary,
                Some(
                    RuntimeBoundarySpec::ExactTransport(RuntimeClass::AggregateValue { .. })
                        | RuntimeBoundarySpec::ExactShape(RuntimeClass::AggregateValue { .. })
                )
            )
        {
            return inner_boundary;
        }
        return Some(RuntimeBoundarySpec::BorrowLike {
            pointee,
            access: BorrowAccess::ReadOnly,
            allow: default_borrow_transport_set(BorrowAccess::ReadOnly, default_space),
        });
    }
    if let Some((kind, inner)) = interface_ty.as_borrow(db) {
        if runtime_zero_sized_ty(db, inner, scope, assumptions) {
            return Some(RuntimeBoundarySpec::ExactShape(
                provider_class_for_target_in_env(db, env, Some(inner), default_space),
            ));
        }
        let access = match kind {
            BorrowKind::Ref => BorrowAccess::ReadOnly,
            BorrowKind::Mut => BorrowAccess::ReadWrite,
        };
        return Some(RuntimeBoundarySpec::BorrowLike {
            pointee: stored_class_for_ty_in_env(db, env, inner),
            access,
            allow: default_borrow_transport_set(access, default_space),
        });
    }
    if let Some((_, inner)) = interface_ty.as_capability(db) {
        return Some(RuntimeBoundarySpec::ExactShape(
            provider_class_for_target_in_env(db, env, Some(inner), default_space),
        ));
    }
    let repr_ty = runtime_repr_ty_in_env(db, env, interface_ty);
    if runtime_zero_sized_ty(db, repr_ty, scope, assumptions) {
        return None;
    }
    top_level_class_for_ty_in_env(db, env, repr_ty, default_space).map(|class| {
        if class.is_transport()
            || runtime_transport_sensitive_aggregate(db, repr_ty, scope, assumptions)
        {
            RuntimeBoundarySpec::ExactShape(class)
        } else {
            RuntimeBoundarySpec::ExactTransport(class)
        }
    })
}

#[salsa::tracked]
fn runtime_boundary_source_uses_transport_sensitive_aggregate<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    let env = RuntimeTypeEnv::new(scope, assumptions);
    let interface_ty = runtime_interface_ty_in_env(db, env, ty);
    if let Some((_, inner)) = interface_ty.as_borrow(db) {
        return runtime_transport_sensitive_aggregate(db, inner, scope, assumptions);
    }
    if let Some((_, inner)) = interface_ty.as_capability(db) {
        return runtime_transport_sensitive_aggregate(db, inner, scope, assumptions);
    }
    let repr_ty = runtime_repr_ty_in_env(db, env, interface_ty);
    runtime_transport_sensitive_aggregate(db, repr_ty, scope, assumptions)
}

pub(crate) fn default_by_place_boundary<'db>(
    db: &'db dyn MirDb,
    type_env: RuntimeTypeEnv<'db>,
    target_ty: Option<TyId<'db>>,
    space: AddressSpaceKind,
) -> RuntimeBoundarySpec<'db> {
    let Some(target_ty) = target_ty else {
        return RuntimeBoundarySpec::ExactShape(provider_class_for_target_in_env(
            db, type_env, None, space,
        ));
    };
    RuntimeBoundarySpec::BorrowLike {
        pointee: stored_class_for_ty_in_env(db, type_env, target_ty),
        access: BorrowAccess::ReadWrite,
        allow: default_borrow_transport_set(BorrowAccess::ReadWrite, space),
    }
}

#[cfg(test)]
mod tests {
    use cranelift_entity::EntityRef;
    use driver::DriverDataBase;
    use hir::analysis::ty::ty_def::TyId;

    use crate::runtime::{
        EnumLayoutKey, EnumVariantLayout, LayoutKey, PlaceRoot, RLocalId, ScalarClass, ScalarRepr,
        ScalarRole, StructLayout,
    };

    use super::*;

    fn word_class<'db>() -> RuntimeClass<'db> {
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int {
                bits: 256,
                signed: false,
            },
            role: ScalarRole::Plain,
        })
    }

    fn bool_class<'db>() -> RuntimeClass<'db> {
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Bool,
            role: ScalarRole::Plain,
        })
    }

    fn raw_addr_class<'db>(db: &'db dyn MirDb, space: AddressSpaceKind) -> RuntimeClass<'db> {
        RuntimeClass::raw_addr(db, space, word_class())
    }

    fn ref_class<'db>(
        pointee: RuntimeClass<'db>,
        kind: RefKind<'db>,
        view: RefView<'db>,
    ) -> RuntimeClass<'db> {
        RuntimeClass::Ref {
            pointee: Box::new(pointee),
            kind,
            view,
        }
    }

    fn provider_ref<'db>(
        db: &'db dyn MirDb,
        pointee: RuntimeClass<'db>,
        space: AddressSpaceKind,
    ) -> RuntimeClass<'db> {
        ref_class(
            pointee,
            RefKind::Provider {
                provider_ty: TyId::unit(db),
                space,
            },
            RefView::Whole,
        )
    }

    fn source_with_value<'db>(value: RuntimeClass<'db>) -> RuntimeValueSource<'db> {
        RuntimeValueSource {
            value,
            address: None,
        }
    }

    fn source_with_address<'db>(
        value: RuntimeClass<'db>,
        address: RuntimeClass<'db>,
    ) -> RuntimeValueSource<'db> {
        RuntimeValueSource {
            value,
            address: Some(RuntimeValueAddress {
                place: RuntimePlace {
                    root: PlaceRoot::Slot(RLocalId::new(0)),
                    path: Box::default(),
                },
                class: address,
            }),
        }
    }

    fn scalar_borrow_boundary<'db>(
        access: BorrowAccess,
        default_space: AddressSpaceKind,
    ) -> RuntimeBoundarySpec<'db> {
        RuntimeBoundarySpec::BorrowLike {
            pointee: word_class(),
            access,
            allow: default_borrow_transport_set(access, default_space),
        }
    }

    fn test_struct_layout<'db>(db: &'db dyn MirDb) -> LayoutId<'db> {
        LayoutId::new(
            db,
            LayoutKey::Struct(StructLayout {
                fields: vec![word_class()].into(),
            }),
        )
    }

    fn test_enum_variant<'db>(db: &'db dyn MirDb) -> crate::runtime::VariantId<'db> {
        let enum_layout = LayoutId::new(
            db,
            LayoutKey::Enum(EnumLayoutKey {
                variants: vec![EnumVariantLayout {
                    fields: vec![word_class()].into(),
                }]
                .into(),
            }),
        );
        crate::runtime::VariantId {
            enum_layout,
            index: 0,
        }
    }

    #[test]
    fn exact_transport_requires_transport_match_but_exact_shape_preserves_source_transport() {
        let db = DriverDataBase::default();
        let source = raw_addr_class(&db, AddressSpaceKind::Storage);
        let target = raw_addr_class(&db, AddressSpaceKind::Memory);
        let exact_transport = RuntimeBoundarySpec::ExactTransport(target.clone());
        let exact_shape = RuntimeBoundarySpec::ExactShape(target.clone());

        assert!(!BoundaryMatcher::class_satisfies_boundary(
            &db,
            &source,
            &exact_transport
        ));
        assert!(BoundaryMatcher::class_satisfies_boundary(
            &db,
            &source,
            &exact_shape
        ));
        assert_eq!(
            RuntimeValueUsePlanner::select(
                &db,
                source_with_value(source.clone()),
                &exact_transport
            ),
            Some(RuntimeValueUsePlan::CoerceValue(target))
        );
        let shape_plan =
            RuntimeValueUsePlanner::select(&db, source_with_value(source.clone()), &exact_shape)
                .expect("exact-shape-compatible source should select a plan");
        assert_eq!(shape_plan, RuntimeValueUsePlan::UseValue);
        assert_eq!(shape_plan.class(&source), source);
    }

    #[test]
    fn exact_shape_ref_matching_ignores_provider_space_but_not_view_or_pointee() {
        let db = DriverDataBase::default();
        let boundary = RuntimeBoundarySpec::ExactShape(provider_ref(
            &db,
            word_class(),
            AddressSpaceKind::Memory,
        ));

        assert!(BoundaryMatcher::class_satisfies_boundary(
            &db,
            &provider_ref(&db, word_class(), AddressSpaceKind::Storage),
            &boundary
        ));
        assert!(!BoundaryMatcher::class_satisfies_boundary(
            &db,
            &provider_ref(&db, bool_class(), AddressSpaceKind::Storage),
            &boundary
        ));
        assert!(!BoundaryMatcher::class_satisfies_boundary(
            &db,
            &ref_class(
                word_class(),
                RefKind::Provider {
                    provider_ty: TyId::unit(&db),
                    space: AddressSpaceKind::Storage,
                },
                RefView::EnumVariant(test_enum_variant(&db))
            ),
            &boundary
        ));
        assert!(!BoundaryMatcher::class_satisfies_boundary(
            &db,
            &provider_ref(&db, word_class(), AddressSpaceKind::Storage),
            &RuntimeBoundarySpec::ExactTransport(provider_ref(
                &db,
                word_class(),
                AddressSpaceKind::Memory
            ))
        ));
    }

    #[test]
    fn borrow_like_boundary_respects_transport_allowlist() {
        let db = DriverDataBase::default();
        let boundary = scalar_borrow_boundary(BorrowAccess::ReadWrite, AddressSpaceKind::Storage);
        let cases = [
            (
                "object ref",
                ref_class(word_class(), RefKind::Object, RefView::Whole),
                true,
            ),
            (
                "storage provider",
                provider_ref(&db, word_class(), AddressSpaceKind::Storage),
                true,
            ),
            (
                "memory provider",
                provider_ref(&db, word_class(), AddressSpaceKind::Memory),
                true,
            ),
            (
                "calldata provider",
                provider_ref(&db, word_class(), AddressSpaceKind::Calldata),
                false,
            ),
            (
                "const ref",
                ref_class(word_class(), RefKind::Const, RefView::Whole),
                false,
            ),
            (
                "raw addr",
                raw_addr_class(&db, AddressSpaceKind::Memory),
                true,
            ),
            (
                "wrong raw pointee",
                RuntimeClass::raw_addr(
                    &db,
                    AddressSpaceKind::Memory,
                    RuntimeClass::Scalar(ScalarClass {
                        repr: ScalarRepr::Int {
                            bits: 8,
                            signed: false,
                        },
                        role: ScalarRole::Plain,
                    }),
                ),
                false,
            ),
            ("plain scalar", word_class(), false),
            (
                "variant view",
                ref_class(
                    word_class(),
                    RefKind::Object,
                    RefView::EnumVariant(test_enum_variant(&db)),
                ),
                false,
            ),
        ];

        for (name, class, expected) in cases {
            assert_eq!(
                BoundaryMatcher::class_satisfies_boundary(&db, &class, &boundary),
                expected,
                "{name}"
            );
        }
    }

    #[test]
    fn borrow_like_planner_prefers_compatible_address_then_scalar_slot_materialization() {
        let db = DriverDataBase::default();
        let boundary = scalar_borrow_boundary(BorrowAccess::ReadWrite, AddressSpaceKind::Storage);
        let address = provider_ref(&db, word_class(), AddressSpaceKind::Storage);

        assert_eq!(
            RuntimeValueUsePlanner::select(
                &db,
                source_with_address(word_class(), address.clone()),
                &boundary
            ),
            Some(RuntimeValueUsePlan::AddrOfRuntimePlace {
                place: RuntimePlace {
                    root: PlaceRoot::Slot(RLocalId::new(0)),
                    path: Box::default(),
                },
                class: address,
            })
        );
        assert_eq!(
            RuntimeValueUsePlanner::select(&db, source_with_value(word_class()), &boundary),
            Some(RuntimeValueUsePlan::MaterializeValue(
                RuntimeValueMaterialization::RawAddrSlot {
                    pointee: word_class(),
                    target: RawPointeeId::exact(&db, word_class()),
                },
            ))
        );
    }

    #[test]
    fn aggregate_borrow_like_materializes_object_ref() {
        let db = DriverDataBase::default();
        let layout = test_struct_layout(&db);
        let boundary = RuntimeBoundarySpec::BorrowLike {
            pointee: RuntimeClass::AggregateValue { layout },
            access: BorrowAccess::ReadWrite,
            allow: default_borrow_transport_set(BorrowAccess::ReadWrite, AddressSpaceKind::Memory),
        };

        assert_eq!(
            RuntimeValueUsePlanner::select(
                &db,
                source_with_value(RuntimeClass::AggregateValue { layout }),
                &boundary,
            ),
            Some(RuntimeValueUsePlan::MaterializeValue(
                RuntimeValueMaterialization::ObjectRef(layout),
            ))
        );
    }
}
