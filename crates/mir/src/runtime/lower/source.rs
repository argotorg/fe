use cranelift_entity::EntityRef;
use hir::analysis::{
    semantic::{
        SLocal, SLocalId, SemanticLocalKind,
        normalized::{
            NDataProjection, NExpr, NIndex, NPlace, NPlaceBase, NRootKind, NStatementKind,
            NValueDefinition, NValueId,
        },
    },
    ty::ty_def::TyId,
};

use crate::{
    db::MirDb,
    runtime::{RuntimeCarrier, RuntimeClass, RuntimeLocalRoot},
};

use super::{
    classify::{
        BodyEnv, carrier_value_class, provider_erases_runtime_root,
        runtime_class_for_direct_value_provider_in_env,
        runtime_class_for_effect_binding_provider_in_env,
    },
    semantic_body::{RuntimeOperand, RuntimeSemanticBody},
};

#[derive(Clone, Copy, Debug)]
pub(super) enum RuntimeSourceMode<'roots, 'db> {
    Abstract,
    Concrete(&'roots [RuntimeLocalRoot<'db>]),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum SemanticPlaceValueSource<'db> {
    PlaceValue {
        place: NPlace<'db>,
        semantic_ty: TyId<'db>,
    },
    ValueExtract {
        place: NPlace<'db>,
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
        operand: RuntimeOperand,
    ) -> Option<SemanticPlaceValueSource<'db>> {
        let local = self.env.body().local(operand.local)?;
        let place = self.semantic_operand_place(operand)?;
        self.place_value_source(place, local.ty)
    }

    pub(super) fn semantic_place_address_source(
        &self,
        operand: RuntimeOperand,
    ) -> Option<(NPlace<'db>, TyId<'db>)> {
        let local = self.env.body().local(operand.local)?;
        let place = self.semantic_operand_place(operand)?;
        self.place_is_lowerable(&place).then_some((place, local.ty))
    }

    fn semantic_operand_place(&self, operand: RuntimeOperand) -> Option<NPlace<'db>> {
        operand
            .value
            .and_then(|value| normalized_value_place(self.env.db(), self.env.body(), value))
            .or_else(|| alias_source_place_for_local(self.env.db(), self.env.body(), operand.local))
    }

    pub(super) fn place_is_lowerable(&self, place: &NPlace<'db>) -> bool {
        match place.base {
            NPlaceBase::CapabilityTarget { carrier } => self
                .env
                .value_local(carrier)
                .is_some_and(|local| self.local_has_transport_carrier(local)),
            NPlaceBase::Root(root_id) => {
                let Some(root) = self.env.body().normalized.root(root_id) else {
                    return false;
                };
                match &root.kind {
                    NRootKind::LocalSlot { .. }
                    | NRootKind::Temporary { .. }
                    | NRootKind::ParamPlace { .. } => self
                        .env
                        .body()
                        .root_local(root_id)
                        .is_some_and(|local| self.local_has_existing_runtime_root(local)),
                    NRootKind::Provider { binding } => {
                        self.provider_place_root_is_lowerable(binding)
                    }
                    NRootKind::CapabilityRepresentation { carrier } => self
                        .env
                        .value_local(*carrier)
                        .is_some_and(|local| self.local_has_transport_carrier(local)),
                }
            }
        }
    }

    pub(super) fn local_has_existing_runtime_root(&self, local: SLocalId) -> bool {
        let Some(local_data) = self.env.body().local(local) else {
            return false;
        };
        self.concrete_roots()
            .and_then(|roots| roots.get(local.index()))
            .is_some_and(|root| !matches!(root, RuntimeLocalRoot::None))
            || (matches!(local_data.role.kind(), SemanticLocalKind::PlaceCarrier)
                && self.local_has_transport_carrier(local))
            || self.local_root_provider_is_lowerable(local_data)
    }

    pub(super) fn local_has_concrete_runtime_root(&self, local: SLocalId) -> bool {
        self.concrete_roots()
            .and_then(|roots| roots.get(local.index()))
            .is_some_and(|root| !matches!(root, RuntimeLocalRoot::None))
    }

    fn provider_place_root_is_lowerable(
        &self,
        provider: &hir::semantic::ProviderBinding<'db>,
    ) -> bool {
        if self
            .env
            .actual_runtime_visible_root_provider_class(self.carriers, provider)
            .is_some()
        {
            return true;
        }
        if provider_erases_runtime_root(
            self.env.db(),
            provider,
            self.env.scope(),
            self.env.assumptions(),
        ) {
            return false;
        }
        runtime_class_for_effect_binding_provider_in_env(
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
        let Some(local_data) = self.env.body().local(local) else {
            return false;
        };
        if self.local_has_transport_carrier(local) {
            return !matches!(local_data.role.kind(), SemanticLocalKind::PlaceBoundValue)
                || local_data
                    .role
                    .root_provider(&self.env.body().locals)
                    .is_some();
        }
        self.local_root_provider_is_lowerable(local_data)
    }

    pub(super) fn semantic_operand_place_address_is_lowerable(&self, local: SLocalId) -> bool {
        let Some(local_data) = self.env.body().local(local) else {
            return false;
        };
        (matches!(local_data.role.kind(), SemanticLocalKind::PlaceCarrier)
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
        let Some(local_data) = self.env.body().local(local) else {
            return false;
        };
        match local_data.role.kind() {
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
                alias_source_place_for_local(self.env.db(), self.env.body(), local).is_some()
                    || self.local_has_existing_runtime_root(local)
            }
        }
    }

    fn place_value_source(
        &self,
        place: NPlace<'db>,
        semantic_ty: TyId<'db>,
    ) -> Option<SemanticPlaceValueSource<'db>> {
        if self.place_is_lowerable(&place) {
            return Some(SemanticPlaceValueSource::PlaceValue { place, semantic_ty });
        }
        self.value_extract_place_is_lowerable(&place)
            .then_some(SemanticPlaceValueSource::ValueExtract { place, semantic_ty })
    }

    fn value_extract_place_is_lowerable(&self, place: &NPlace<'db>) -> bool {
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

    fn local_has_transport_carrier(&self, local: SLocalId) -> bool {
        carrier_value_class(local, self.carriers).is_some_and(|class| class.is_transport())
    }

    fn local_root_provider_is_lowerable(&self, local: &SLocal<'db>) -> bool {
        local
            .role
            .root_provider(&self.env.body().locals)
            .is_some_and(|provider| self.provider_place_root_is_lowerable(&provider))
    }
}

pub(super) fn alias_source_place_for_local<'db>(
    db: &'db dyn MirDb,
    body: &RuntimeSemanticBody<'db>,
    local: SLocalId,
) -> Option<NPlace<'db>> {
    body.layout_plan
        .value_representations
        .iter()
        .rev()
        .filter(|representation| body.value_local(representation.value) == Some(local))
        .find_map(|representation| normalized_value_place(db, body, representation.value))
}

pub(super) fn declared_root_place_for_local<'db>(
    body: &RuntimeSemanticBody<'db>,
    local: SLocalId,
) -> Option<NPlace<'db>> {
    let local_data = body.local(local)?;
    let provider = local_data.role.root_provider(&body.locals);
    let root = body
        .normalized
        .roots
        .iter()
        .enumerate()
        .find_map(|(index, root)| {
            let root_id = hir::analysis::semantic::normalized::NRootId::new(index);
            let matches_local = body.root_local(root_id) == Some(local);
            let matches_provider = provider.as_ref().is_some_and(|provider| {
                matches!(
                    &root.kind,
                    NRootKind::Provider { binding }
                        if binding == provider && root.ty == local_data.role.layout_ty(local_data.ty)
                )
            });
            (matches_local || matches_provider).then_some((root_id, root))
        })?;
    Some(NPlace {
        base: NPlaceBase::Root(root.0),
        path: Default::default(),
        ty: root.1.ty,
        origin: root.1.origin,
    })
}

pub(super) fn nonself_alias_source_place_for_local<'db>(
    db: &'db dyn MirDb,
    body: &RuntimeSemanticBody<'db>,
    local: SLocalId,
) -> Option<NPlace<'db>> {
    alias_source_place_for_local(db, body, local)
        .filter(|place| !is_self_rooted_value_place(body, local, place))
}

fn is_self_rooted_value_place(
    body: &RuntimeSemanticBody<'_>,
    local: SLocalId,
    place: &NPlace<'_>,
) -> bool {
    if !place.path.is_empty() {
        return false;
    }
    match place.base {
        NPlaceBase::CapabilityTarget { carrier } => body.value_local(carrier) == Some(local),
        NPlaceBase::Root(root) => {
            matches!(
                body.normalized.root(root).map(|root| &root.kind),
                Some(
                    NRootKind::LocalSlot { .. }
                        | NRootKind::Temporary { .. }
                        | NRootKind::ParamPlace { .. }
                )
            ) && body.root_local(root) == Some(local)
        }
    }
}

fn normalized_value_place<'db>(
    db: &'db dyn MirDb,
    body: &RuntimeSemanticBody<'db>,
    value: NValueId,
) -> Option<NPlace<'db>> {
    fn resolve<'db>(
        db: &'db dyn MirDb,
        body: &RuntimeSemanticBody<'db>,
        value: NValueId,
        visiting: &mut [bool],
    ) -> Option<NPlace<'db>> {
        if std::mem::replace(visiting.get_mut(value.index())?, true) {
            return None;
        }
        let data = body.normalized.value(value)?;
        let place = match data.definition {
            NValueDefinition::Statement { block, statement } => match &body
                .normalized
                .block(block)?
                .statements
                .get(statement as usize)?
                .kind
            {
                NStatementKind::Define {
                    expr:
                        NExpr::Load { place, .. }
                        | NExpr::Borrow { place, .. }
                        | NExpr::MakeView { place, .. },
                    ..
                } => Some(place.clone()),
                NStatementKind::Define {
                    expr: NExpr::Forward { src },
                    ..
                } => resolve(db, body, src.value, visiting),
                NStatementKind::Define {
                    expr: NExpr::ProjectValue { value, path },
                    ..
                } => resolve(db, body, value.value, visiting).map(|mut place| {
                    place.path = place.path.concat(&path.0);
                    place.ty = data.ty;
                    place
                }),
                NStatementKind::Define { .. } | NStatementKind::Store { .. } => None,
            },
            NValueDefinition::EntryParam { .. } | NValueDefinition::BlockParam { .. } => None,
        }
        .or_else(|| {
            data.ty.as_capability(db).map(|(_, ty)| NPlace {
                base: NPlaceBase::CapabilityTarget { carrier: value },
                path: Default::default(),
                ty,
                origin: data.origin,
            })
        });
        visiting[value.index()] = false;
        place
    }

    resolve(
        db,
        body,
        value,
        &mut vec![false; body.normalized.values.len()],
    )
}

pub(super) fn local_read_places_extractable_from_value(
    body: &RuntimeSemanticBody<'_>,
    local: SLocalId,
) -> bool {
    body.normalized.blocks.iter().all(|block| {
        block
            .statements
            .iter()
            .all(|statement| match &statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Load { place, .. } | NExpr::MakeView { place, .. },
                    ..
                } => {
                    place_root_local(body, place) != Some(local)
                        || place.path.iter().all(value_extractable_projection)
                }
                NStatementKind::Define { .. } | NStatementKind::Store { .. } => true,
            })
    })
}

fn place_root_local(body: &RuntimeSemanticBody<'_>, place: &NPlace<'_>) -> Option<SLocalId> {
    match place.base {
        NPlaceBase::CapabilityTarget { carrier } => body.value_local(carrier),
        NPlaceBase::Root(root) => body.root_local(root),
    }
}

fn value_extractable_projection(projection: &NDataProjection) -> bool {
    matches!(
        projection,
        NDataProjection::Field(_)
            | NDataProjection::VariantField { .. }
            | NDataProjection::Index(NIndex::Const(_))
    )
}
