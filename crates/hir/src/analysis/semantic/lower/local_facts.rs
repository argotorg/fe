use cranelift_entity::EntityRef;

use crate::analysis::{
    semantic::{
        PlaceProvenance, SExpr, SLocalId, SPlace, SStmtKind, SemanticLocalRole, ValueProvenance,
    },
    ty::{effect_handle_metadata, normalize::normalize_ty, ty_def::TyId},
};
use crate::semantic::ProviderBinding;

use super::body::SmirLowerCtxt;

impl<'a, 'db> SmirLowerCtxt<'a, 'db> {
    pub(super) fn update_stmt_local_facts(&mut self, kind: &SStmtKind<'db>) {
        let SStmtKind::Assign { dst, expr } = kind else {
            return;
        };
        self.update_assigned_local_facts(*dst, expr);
    }

    fn update_assigned_local_facts(&mut self, dst: SLocalId, expr: &SExpr<'db>) {
        let dst_idx = dst.index();
        let has_source = self.locals[dst_idx].source.is_some();
        let fallback = (!has_source).then(|| self.fallback_local_role(self.locals[dst_idx].ty));
        let next_role = fallback
            .as_ref()
            .map(|_| self.classify_expr_local_role(self.locals[dst_idx].ty, expr));
        let next_snapshot = self.classify_expr_snapshot_source(expr);

        if let Some((next_role, fallback)) = next_role.zip(fallback) {
            self.locals[dst_idx].role =
                merge_local_roles(self.locals[dst_idx].role.clone(), next_role, fallback);
        }
        if self.assigned_snapshots[dst_idx] {
            self.locals[dst_idx].snapshot_source =
                merge_snapshot_sources(self.locals[dst_idx].snapshot_source.clone(), next_snapshot);
        } else {
            self.locals[dst_idx].snapshot_source = next_snapshot;
            self.assigned_snapshots[dst_idx] = true;
        }
    }

    fn fallback_local_role(&self, ty: TyId<'db>) -> SemanticLocalRole<'db> {
        let ty = normalize_ty(self.db, ty, self.body.scope(), self.assumptions);
        if let Some((_, value_ty)) = ty.as_capability(self.db) {
            return SemanticLocalRole::PlaceCarrier {
                provider: None,
                value_ty: normalize_ty(self.db, value_ty, self.body.scope(), self.assumptions),
            };
        }
        effect_handle_metadata(self.db, self.body.scope(), self.assumptions, ty).map_or(
            ordinary_direct_value_role(),
            |metadata| SemanticLocalRole::DirectCarrier {
                provider: None,
                target_ty: metadata.target_ty,
            },
        )
    }

    fn classify_expr_local_role(
        &self,
        dst_ty: TyId<'db>,
        expr: &SExpr<'db>,
    ) -> SemanticLocalRole<'db> {
        match expr {
            SExpr::Forward(value) => self.classify_forward_role(dst_ty, value.value),
            SExpr::UseValue(_) | SExpr::Borrow { .. } | SExpr::Call { .. } => {
                self.fallback_local_role(dst_ty)
            }
            SExpr::ReadPlace { place } => {
                self.classify_projection_local_role(dst_ty, place.clone())
            }
            SExpr::Field { base, field } => {
                self.classify_projection_local_role(dst_ty, SPlace::field(base.value, *field))
            }
            SExpr::Index { base, index } => self.classify_projection_local_role(
                dst_ty,
                SPlace::dynamic_index(base.value, index.value),
            ),
            SExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => self.classify_projection_local_role(
                dst_ty,
                SPlace::variant_field(
                    value.value,
                    *variant,
                    self.locals[value.value.index()].ty,
                    *field,
                ),
            ),
            SExpr::ArrayRepeat { ty, .. } | SExpr::AggregateMake { ty, .. } => {
                let fallback = self.fallback_local_role(*ty);
                match fallback {
                    SemanticLocalRole::PlaceCarrier { .. }
                    | SemanticLocalRole::PlaceBoundValue { .. }
                    | SemanticLocalRole::DirectCarrier { .. } => fallback,
                    SemanticLocalRole::Erased | SemanticLocalRole::DirectValue { .. } => {
                        ordinary_direct_value_role()
                    }
                }
            }
            SExpr::CodeRegionRef { .. }
            | SExpr::Const(_)
            | SExpr::Unary { .. }
            | SExpr::Binary { .. }
            | SExpr::Cast { .. }
            | SExpr::EnumMake { .. }
            | SExpr::GetEnumTag { .. }
            | SExpr::IsEnumVariant { .. }
            | SExpr::CodeRegionOffset { .. }
            | SExpr::CodeRegionLen { .. } => ordinary_direct_value_role(),
        }
    }

    fn classify_expr_snapshot_source(&self, expr: &SExpr<'db>) -> Option<PlaceProvenance<'db>> {
        match expr {
            SExpr::Forward(value) | SExpr::UseValue(value) => {
                self.locals[value.value.index()].snapshot_source.clone()
            }
            SExpr::ReadPlace { place } => self.classify_projection_snapshot_source(place.clone()),
            SExpr::Field { base, field } => {
                self.classify_projection_snapshot_source(SPlace::field(base.value, *field))
            }
            SExpr::Index { base, index } => self.classify_projection_snapshot_source(
                SPlace::dynamic_index(base.value, index.value),
            ),
            SExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => self.classify_projection_snapshot_source(SPlace::variant_field(
                value.value,
                *variant,
                self.locals[value.value.index()].ty,
                *field,
            )),
            SExpr::Borrow { .. }
            | SExpr::Call { .. }
            | SExpr::ArrayRepeat { .. }
            | SExpr::AggregateMake { .. }
            | SExpr::CodeRegionRef { .. }
            | SExpr::Const(_)
            | SExpr::Unary { .. }
            | SExpr::Binary { .. }
            | SExpr::Cast { .. }
            | SExpr::EnumMake { .. }
            | SExpr::GetEnumTag { .. }
            | SExpr::IsEnumVariant { .. }
            | SExpr::CodeRegionOffset { .. }
            | SExpr::CodeRegionLen { .. } => None,
        }
    }

    fn classify_projection_snapshot_source(
        &self,
        place: SPlace<'db>,
    ) -> Option<PlaceProvenance<'db>> {
        (!matches!(
            self.locals[place.local.index()].role,
            SemanticLocalRole::Erased
        ))
        .then_some(PlaceProvenance::Derived(place))
    }

    fn classify_forward_role(&self, dst_ty: TyId<'db>, value: SLocalId) -> SemanticLocalRole<'db> {
        let fallback = self.fallback_local_role(dst_ty);
        match (self.locals[value.index()].role.clone(), &fallback) {
            (SemanticLocalRole::Erased, _) => SemanticLocalRole::Erased,
            (
                SemanticLocalRole::DirectValue { provenance },
                SemanticLocalRole::DirectValue { .. },
            ) => SemanticLocalRole::DirectValue { provenance },
            (
                SemanticLocalRole::PlaceCarrier {
                    provider,
                    value_ty: src_value_ty,
                },
                SemanticLocalRole::PlaceCarrier {
                    value_ty: dst_value_ty,
                    ..
                },
            ) if src_value_ty == *dst_value_ty => SemanticLocalRole::PlaceCarrier {
                provider,
                value_ty: src_value_ty,
            },
            (
                SemanticLocalRole::PlaceBoundValue {
                    provenance,
                    value_ty: src_value_ty,
                },
                SemanticLocalRole::PlaceBoundValue {
                    value_ty: dst_value_ty,
                    ..
                },
            ) if src_value_ty == *dst_value_ty => SemanticLocalRole::PlaceBoundValue {
                provenance,
                value_ty: src_value_ty,
            },
            (
                SemanticLocalRole::DirectCarrier {
                    provider,
                    target_ty: src_target_ty,
                },
                SemanticLocalRole::DirectCarrier {
                    target_ty: dst_target_ty,
                    ..
                },
            ) if src_target_ty == *dst_target_ty => SemanticLocalRole::DirectCarrier {
                provider,
                target_ty: src_target_ty,
            },
            _ => fallback,
        }
    }

    fn classify_projection_local_role(
        &self,
        dst_ty: TyId<'db>,
        place: SPlace<'db>,
    ) -> SemanticLocalRole<'db> {
        let fallback = self.fallback_local_role(dst_ty);
        let base_role = self.locals[place.local.index()].role.clone();
        match fallback {
            SemanticLocalRole::Erased => SemanticLocalRole::Erased,
            SemanticLocalRole::DirectValue { .. } => fallback,
            SemanticLocalRole::PlaceCarrier { value_ty, .. }
            | SemanticLocalRole::PlaceBoundValue { value_ty, .. }
                if local_role_supports_place_provenance(&base_role) =>
            {
                SemanticLocalRole::PlaceBoundValue {
                    provenance: PlaceProvenance::Derived(place),
                    value_ty,
                }
            }
            SemanticLocalRole::PlaceCarrier { .. } | SemanticLocalRole::PlaceBoundValue { .. } => {
                fallback
            }
            SemanticLocalRole::DirectCarrier { target_ty, .. } => base_role
                .root_provider(&self.locals)
                // A handle projected out of a provider-backed place is only
                // that provider's carrier when it provides the same target;
                // an unrelated embedded handle (e.g. a `TStorPtr<bool>` field
                // inside a storage struct) is an ordinary value whose own
                // transport class would contradict the provider's.
                .filter(|provider| provider.effective_target_ty() == target_ty)
                .map_or(fallback, |provider| SemanticLocalRole::DirectCarrier {
                    provider: Some(provider),
                    target_ty,
                }),
        }
    }
}

pub(super) fn ordinary_direct_value_role<'db>() -> SemanticLocalRole<'db> {
    SemanticLocalRole::DirectValue {
        provenance: ValueProvenance::Ordinary,
    }
}

pub(super) fn initial_snapshot_source<'db>(
    role: &SemanticLocalRole<'db>,
) -> Option<PlaceProvenance<'db>> {
    match role {
        SemanticLocalRole::DirectValue {
            provenance: ValueProvenance::RootProvider(provider),
        } => Some(PlaceProvenance::RootProvider(provider.clone())),
        SemanticLocalRole::Erased
        | SemanticLocalRole::DirectValue {
            provenance: ValueProvenance::Ordinary,
        }
        | SemanticLocalRole::PlaceCarrier { .. }
        | SemanticLocalRole::PlaceBoundValue { .. }
        | SemanticLocalRole::DirectCarrier { .. } => None,
    }
}

fn local_role_supports_place_provenance(role: &SemanticLocalRole<'_>) -> bool {
    match role {
        SemanticLocalRole::Erased
        | SemanticLocalRole::DirectValue {
            provenance: ValueProvenance::Ordinary,
        }
        | SemanticLocalRole::DirectCarrier { provider: None, .. } => false,
        SemanticLocalRole::DirectValue { .. }
        | SemanticLocalRole::PlaceCarrier { .. }
        | SemanticLocalRole::PlaceBoundValue { .. }
        | SemanticLocalRole::DirectCarrier {
            provider: Some(_), ..
        } => true,
    }
}

fn merge_local_roles<'db>(
    current: SemanticLocalRole<'db>,
    next: SemanticLocalRole<'db>,
    fallback: SemanticLocalRole<'db>,
) -> SemanticLocalRole<'db> {
    if current == next {
        return current;
    }
    match (current, next) {
        (
            SemanticLocalRole::DirectValue {
                provenance: left_provenance,
            },
            SemanticLocalRole::DirectValue {
                provenance: right_provenance,
            },
        ) => merge_direct_value_role(left_provenance, right_provenance).unwrap_or(fallback),
        (
            SemanticLocalRole::DirectCarrier {
                provider: left_provider,
                target_ty: left_target_ty,
            },
            SemanticLocalRole::DirectCarrier {
                provider: right_provider,
                target_ty: right_target_ty,
            },
        ) if left_target_ty == right_target_ty => SemanticLocalRole::DirectCarrier {
            provider: (left_provider == right_provider)
                .then_some(left_provider)
                .flatten(),
            target_ty: left_target_ty,
        },
        (
            SemanticLocalRole::PlaceCarrier {
                provider: left_provider,
                value_ty: left_value_ty,
            },
            SemanticLocalRole::PlaceCarrier {
                provider: right_provider,
                value_ty: right_value_ty,
            },
        ) if left_value_ty == right_value_ty => SemanticLocalRole::PlaceCarrier {
            provider: merge_role_provider(left_provider, right_provider),
            value_ty: left_value_ty,
        },
        (
            SemanticLocalRole::DirectValue {
                provenance: ValueProvenance::Ordinary,
            },
            next,
        ) => next,
        (
            current,
            SemanticLocalRole::DirectValue {
                provenance: ValueProvenance::Ordinary,
            },
        ) => current,
        _ => fallback,
    }
}

fn merge_role_provider<'db>(
    left: Option<ProviderBinding<'db>>,
    right: Option<ProviderBinding<'db>>,
) -> Option<ProviderBinding<'db>> {
    (left == right).then_some(left).flatten()
}

fn merge_direct_value_role<'db>(
    left: ValueProvenance<'db>,
    right: ValueProvenance<'db>,
) -> Option<SemanticLocalRole<'db>> {
    let provenance = match (left, right) {
        (ValueProvenance::Ordinary, _) | (_, ValueProvenance::Ordinary) => {
            ValueProvenance::Ordinary
        }
        (ValueProvenance::RootProvider(left), ValueProvenance::RootProvider(right))
            if left == right =>
        {
            ValueProvenance::RootProvider(left)
        }
        (ValueProvenance::RootProvider(_), ValueProvenance::RootProvider(_)) => {
            return None;
        }
    };
    Some(SemanticLocalRole::DirectValue { provenance })
}

fn merge_snapshot_sources<'db>(
    current: Option<PlaceProvenance<'db>>,
    next: Option<PlaceProvenance<'db>>,
) -> Option<PlaceProvenance<'db>> {
    (current == next).then_some(current).flatten()
}
