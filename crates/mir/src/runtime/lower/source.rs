use cranelift_entity::EntityRef;
use hir::analysis::{
    semantic::{
        NBorrowRoot, NBorrowRootId, SBlockId, SLocalId, SemanticLocalKind, VariantIndex,
        borrowck::{
            NExpr, NSLocal, NSPlace, NSPlaceRoot, NSStmtKind, NormalizedBindingLowering,
            NormalizedSemanticBody,
        },
    },
    ty::ty_def::TyId,
};
use hir::projection::Projection;

use crate::runtime::{RuntimeCarrier, RuntimeClass, RuntimeLocalRoot};

use super::classify::{
    BodyEnv, carrier_value_class, nonself_backing_value_place, provider_erases_runtime_root,
    runtime_class_for_direct_value_provider_in_env,
    runtime_class_for_effect_binding_provider_in_env, snapshot_source_place,
};
use super::infer::{plan_runtime_local_root, runtime_local_is_signature_pinned};

#[derive(Clone, Copy, Debug)]
pub(super) enum RuntimeSourceMode<'roots, 'db> {
    Abstract,
    Concrete(&'roots [RuntimeLocalRoot<'db>]),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum SemanticPlaceValueSource<'db> {
    PlaceValue {
        place: NSPlace<'db>,
        semantic_ty: TyId<'db>,
    },
    ValueExtract {
        place: NSPlace<'db>,
        semantic_ty: TyId<'db>,
    },
}

pub(super) struct RuntimeSourceQuery<'a, 'carriers, 'roots, 'db> {
    env: BodyEnv<'a, 'db>,
    carriers: &'carriers [RuntimeCarrier<'db>],
    mode: RuntimeSourceMode<'roots, 'db>,
}

impl<'a, 'carriers, 'roots, 'db> RuntimeSourceQuery<'a, 'carriers, 'roots, 'db> {
    pub(super) fn new(
        env: BodyEnv<'a, 'db>,
        carriers: &'carriers [RuntimeCarrier<'db>],
        mode: RuntimeSourceMode<'roots, 'db>,
    ) -> Self {
        Self {
            env,
            carriers,
            mode,
        }
    }

    pub(super) fn semantic_operand_value_is_available(&self, local: SLocalId) -> bool {
        matches!(self.mode, RuntimeSourceMode::Abstract)
            || self.semantic_operand_value_is_lowerable(local)
    }

    pub(super) fn semantic_place_value_source(
        &self,
        local: SLocalId,
    ) -> Option<SemanticPlaceValueSource<'db>> {
        let local_data = self.env.body().locals.get(local.index())?;
        if matches!(local_data.facts.interface, SemanticLocalKind::PlaceCarrier)
            && self.local_has_transport_carrier(local)
            && let Some(source) = self.place_value_source(
                NSPlace {
                    root: NSPlaceRoot::CarrierDerefLocal(local),
                    path: Default::default(),
                },
                local_data.ty,
            )
        {
            return Some(source);
        }
        [
            snapshot_source_place(self.env.body(), local).cloned(),
            nonself_backing_value_place(self.env.body(), local).cloned(),
            alias_source_place_for_local(self.env.body(), local),
        ]
        .into_iter()
        .flatten()
        .find_map(|place| self.place_value_source(place, local_data.ty))
    }

    pub(super) fn place_is_lowerable(&self, place: &NSPlace<'db>) -> bool {
        let mut visiting = vec![false; self.env.body().locals.len()];
        self.place_is_lowerable_with_seen(place, &mut visiting)
    }

    pub(super) fn place_has_existing_runtime_root(&self, place: &NSPlace<'db>) -> bool {
        match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => self.local_has_existing_runtime_root(local),
            NSPlaceRoot::Root(root) => match self.env.body().root(root) {
                Some(NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local }) => {
                    self.local_has_existing_runtime_root(*local)
                }
                Some(NBorrowRoot::Provider { binding, .. }) => {
                    self.provider_place_root_is_lowerable(binding)
                }
                None => false,
            },
        }
    }

    pub(super) fn local_has_existing_runtime_root(&self, local: SLocalId) -> bool {
        let Some(local_data) = self.env.body().locals.get(local.index()) else {
            return false;
        };
        self.concrete_roots()
            .and_then(|roots| roots.get(local.index()))
            .is_some_and(|root| !matches!(root, RuntimeLocalRoot::None))
            || self.abstract_runtime_root(local)
            || (matches!(local_data.facts.interface, SemanticLocalKind::PlaceCarrier)
                && self.local_has_transport_carrier(local))
            || self.local_root_provider_is_lowerable(local_data)
    }

    fn abstract_runtime_root(&self, local: SLocalId) -> bool {
        if !matches!(self.mode, RuntimeSourceMode::Abstract) {
            return false;
        }
        let Some(carrier) = self.carriers.get(local.index()).cloned() else {
            return false;
        };
        let (_, root) = plan_runtime_local_root(
            self.env.with_carriers(self.carriers),
            local,
            carrier,
            runtime_local_is_signature_pinned(self.env, local),
        );
        !matches!(root, RuntimeLocalRoot::None)
    }

    fn provider_place_root_is_lowerable(
        &self,
        provider: &hir::semantic::ProviderBinding<'db>,
    ) -> bool {
        if provider_erases_runtime_root(
            self.env.db(),
            provider,
            self.env.scope(),
            self.env.assumptions(),
        ) {
            return false;
        }
        self.env
            .actual_runtime_visible_root_provider_class(self.carriers, provider)
            .is_some()
            || runtime_class_for_effect_binding_provider_in_env(
                self.env.db(),
                self.env.type_env(),
                provider,
            )
            .is_some()
            || runtime_class_for_direct_value_provider_in_env(
                self.env.db(),
                self.env.type_env(),
                provider,
            )
            .is_some()
    }

    pub(super) fn handle_like_semantic_value_is_available(&self, local: SLocalId) -> bool {
        let Some(local_data) = self.env.body().locals.get(local.index()) else {
            return false;
        };
        if self.local_has_transport_carrier(local) {
            return !matches!(
                local_data.facts.interface,
                SemanticLocalKind::PlaceBoundValue
            ) || local_data.facts.origin.root_provider().is_some();
        }
        self.local_root_provider_is_lowerable(local_data)
    }

    pub(super) fn semantic_operand_place_address_is_lowerable(&self, local: SLocalId) -> bool {
        let Some(local_data) = self.env.body().locals.get(local.index()) else {
            return false;
        };
        (matches!(local_data.facts.interface, SemanticLocalKind::PlaceCarrier)
            && self.local_has_transport_carrier(local))
            || self.local_root_provider_is_lowerable(local_data)
    }

    fn concrete_roots(&self) -> Option<&'roots [RuntimeLocalRoot<'db>]> {
        match self.mode {
            RuntimeSourceMode::Abstract => None,
            RuntimeSourceMode::Concrete(roots) => Some(roots),
        }
    }

    fn semantic_operand_value_is_lowerable(&self, local: SLocalId) -> bool {
        let Some(local_data) = self.env.body().locals.get(local.index()) else {
            return false;
        };
        match local_data.facts.interface {
            SemanticLocalKind::Erased => false,
            SemanticLocalKind::DirectValue
                if carrier_value_class(local, self.carriers).is_some() =>
            {
                true
            }
            SemanticLocalKind::DirectCarrier => self.handle_like_semantic_value_is_available(local),
            SemanticLocalKind::DirectValue
            | SemanticLocalKind::PlaceCarrier
            | SemanticLocalKind::PlaceBoundValue => {
                self.semantic_place_value_source(local).is_some()
                    || self.local_has_existing_runtime_root(local)
            }
        }
    }

    fn place_value_source(
        &self,
        place: NSPlace<'db>,
        semantic_ty: TyId<'db>,
    ) -> Option<SemanticPlaceValueSource<'db>> {
        if self.place_is_lowerable(&place) {
            return Some(SemanticPlaceValueSource::PlaceValue { place, semantic_ty });
        }
        self.value_extract_place_is_lowerable(&place)
            .then(|| SemanticPlaceValueSource::ValueExtract { place, semantic_ty })
    }

    fn value_extract_place_is_lowerable(&self, place: &NSPlace<'db>) -> bool {
        let Some(base) = place_root_local(self.env.body(), place) else {
            return false;
        };
        if self.local_has_existing_runtime_root(base) {
            return false;
        }
        let Some(class) = carrier_value_class(base, self.carriers) else {
            return false;
        };
        if place.path.is_empty() {
            return !class.is_transport();
        }
        matches!(class, RuntimeClass::AggregateValue { .. })
            && place.path.iter().all(value_extractable_projection)
    }

    fn place_is_lowerable_with_seen(&self, place: &NSPlace<'db>, visiting: &mut [bool]) -> bool {
        match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => {
                carrier_value_class(local, self.carriers).is_some_and(|class| class.is_transport())
                    || self.semantic_place_root_is_lowerable(local, visiting)
            }
            NSPlaceRoot::Root(root) => match self.env.body().root(root) {
                Some(NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local }) => {
                    self.semantic_place_root_is_lowerable(*local, visiting)
                }
                Some(NBorrowRoot::Provider { binding, .. }) => {
                    self.provider_place_root_is_lowerable(binding)
                }
                None => false,
            },
        }
    }

    fn semantic_place_root_is_lowerable(&self, local: SLocalId, visiting: &mut [bool]) -> bool {
        let Some(local_data) = self.env.body().locals.get(local.index()) else {
            return false;
        };
        if std::mem::replace(&mut visiting[local.index()], true) {
            return false;
        }
        let lowerable = self.local_has_existing_runtime_root(local)
            || snapshot_source_place(self.env.body(), local)
                .is_some_and(|place| self.place_is_lowerable_with_seen(place, visiting))
            || local_data
                .backing_place()
                .is_some_and(|place| self.place_is_lowerable_with_seen(place, visiting))
            || (matches!(self.mode, RuntimeSourceMode::Abstract)
                && local_data.facts.root_demand.needs_runtime_root());
        visiting[local.index()] = false;
        lowerable
    }

    fn local_has_transport_carrier(&self, local: SLocalId) -> bool {
        carrier_value_class(local, self.carriers).is_some_and(|class| class.is_transport())
    }

    fn local_root_provider_is_lowerable(&self, local: &NSLocal<'db>) -> bool {
        local
            .facts
            .origin
            .root_provider()
            .is_some_and(|provider| self.provider_place_root_is_lowerable(provider))
    }
}

pub(super) fn alias_source_place_for_local<'db>(
    body: &NormalizedSemanticBody<'db>,
    local: SLocalId,
) -> Option<NSPlace<'db>> {
    let local_data = body.local(local)?;
    if let Some(place) = local_data.backing_place() {
        return Some(place.clone());
    }
    match &local_data.lowering {
        NormalizedBindingLowering::CarrierLocal {
            provider,
            target_ty,
            ..
        } => {
            let root = if let Some(provider) = provider {
                NSPlaceRoot::Root(provider_borrow_root(body, provider, *target_ty)?)
            } else {
                NSPlaceRoot::CarrierDerefLocal(local)
            };
            Some(NSPlace {
                root,
                path: Default::default(),
            })
        }
        NormalizedBindingLowering::Erased
        | NormalizedBindingLowering::ValueLocal { .. }
        | NormalizedBindingLowering::PlaceBoundValue { .. } => None,
    }
}

fn provider_borrow_root<'db>(
    body: &NormalizedSemanticBody<'db>,
    provider: &hir::semantic::ProviderBinding<'db>,
    value_ty: TyId<'db>,
) -> Option<NBorrowRootId> {
    body.borrow_roots
        .iter()
        .position(|root| {
            matches!(
                root,
                NBorrowRoot::Provider {
                    binding,
                    value_ty: root_value_ty,
                } if binding == provider && *root_value_ty == value_ty
            )
        })
        .map(|idx| NBorrowRootId::from_u32(idx as u32))
}

pub(super) fn local_read_places_extractable_from_value<'db>(
    env: BodyEnv<'_, 'db>,
    carriers: &[RuntimeCarrier<'db>],
    body: &NormalizedSemanticBody<'db>,
    local: SLocalId,
) -> bool {
    let snapshot_sources_are_extractable = body
        .locals
        .iter()
        .enumerate()
        .filter(|(dst, _)| env.local_is_reachable(SLocalId::new(*dst)))
        .all(|(dst, local_data)| {
            local_data.snapshot_source_place().is_none_or(|place| {
                place_root_local(body, place) != Some(local)
                    || place_is_value_extractable_into(
                        env,
                        carriers,
                        SLocalId::from_u32(dst as u32),
                        place,
                    )
            })
        });
    snapshot_sources_are_extractable
        && body
            .blocks
            .iter()
            .enumerate()
            .filter(|(block_idx, _)| env.block_is_reachable(SBlockId::new(*block_idx)))
            .all(|(_, block)| {
                block.stmts.iter().all(|stmt| match &stmt.kind {
                    NSStmtKind::Assign { dst, expr } => expr_read_places_extractable_from_value(
                        env, carriers, body, local, *dst, expr,
                    ),
                    NSStmtKind::Store { .. } => true,
                })
            })
}

pub(super) fn place_root_local<'db>(
    body: &NormalizedSemanticBody<'db>,
    place: &NSPlace<'db>,
) -> Option<SLocalId> {
    match place.root {
        NSPlaceRoot::CarrierDerefLocal(local) => Some(local),
        NSPlaceRoot::Root(root) => match body.root(root) {
            Some(NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local }) => {
                Some(*local)
            }
            Some(NBorrowRoot::Provider { .. }) | None => None,
        },
    }
}

fn value_extractable_projection<'db>(
    projection: &Projection<TyId<'db>, VariantIndex, SLocalId>,
) -> bool {
    matches!(
        projection,
        Projection::Field(_) | Projection::VariantField { .. } | Projection::Index(_)
    )
}

fn expr_read_places_extractable_from_value<'db>(
    env: BodyEnv<'_, 'db>,
    carriers: &[RuntimeCarrier<'db>],
    body: &NormalizedSemanticBody<'db>,
    local: SLocalId,
    dst: SLocalId,
    expr: &NExpr<'db>,
) -> bool {
    match expr {
        NExpr::ReadPlace { place, .. } if place_root_local(body, place) == Some(local) => {
            place_is_value_extractable_into(env, carriers, dst, place)
        }
        _ => true,
    }
}

fn place_is_value_extractable_into<'db>(
    env: BodyEnv<'_, 'db>,
    carriers: &[RuntimeCarrier<'db>],
    dst: SLocalId,
    place: &NSPlace<'db>,
) -> bool {
    let projected = env.normalized_place_class(carriers, place);
    let target = carriers
        .get(dst.index())
        .and_then(RuntimeCarrier::value_class);
    let target_requires_transport = env.body().locals.get(dst.index()).is_some_and(|local| {
        matches!(
            local.facts.interface,
            SemanticLocalKind::PlaceCarrier
                | SemanticLocalKind::PlaceBoundValue
                | SemanticLocalKind::DirectCarrier
        )
    }) && target.is_some_and(RuntimeClass::is_transport);
    place.path.iter().all(value_extractable_projection)
        && (!target_requires_transport || projected.is_some_and(|class| class.is_transport()))
}
