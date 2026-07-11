use cranelift_entity::EntityRef;

use crate::semantic::ProviderBinding;
use crate::{
    analysis::{
        semantic::{
            FieldIndex, LayoutBackingPlace, LayoutBackingProjection, LayoutBackingSource,
            PlaceProvenance, SEffectArgValue, SExpr, SLocalId, SPlace, SStmtKind,
            SemanticLocalRole, SemanticProjectionPath, ValueProvenance, VariantIndex,
        },
        ty::{
            adt_def::instantiate_adt_field_shape,
            const_ty::CallableInputLayoutHoleOrigin,
            normalize::normalize_ty,
            provider::{ProviderLayoutEvidence, provider_semantics},
            ty_check::{LocalBinding, ParamSite, ReturnProjectionStep, ReturnProvenance},
            ty_def::TyId,
        },
    },
    projection::{IndexSource, Projection},
};

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
        let next_layout_backing_sources = self.classify_expr_layout_backing_sources(expr);

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
        if self.assigned_layout_backing_sources[dst_idx] {
            self.locals[dst_idx].layout_backing_sources = merge_layout_backing_sources(
                self.locals[dst_idx].layout_backing_sources.clone(),
                next_layout_backing_sources,
            );
        } else {
            self.locals[dst_idx].layout_backing_sources = next_layout_backing_sources;
            self.assigned_layout_backing_sources[dst_idx] = true;
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
        let semantics = provider_semantics(self.db, self.body.scope(), self.assumptions, ty);
        match (semantics.evidence, semantics.target_ty) {
            (ProviderLayoutEvidence::ResolvedHandle(_), Some(target_ty)) => {
                SemanticLocalRole::DirectCarrier {
                    provider: None,
                    target_ty,
                }
            }
            _ => ordinary_direct_value_role(),
        }
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

    fn classify_expr_layout_backing_sources(
        &self,
        expr: &SExpr<'db>,
    ) -> Vec<LayoutBackingSource<'db>> {
        match expr {
            SExpr::Forward(value) | SExpr::UseValue(value) => {
                self.local_layout_backing_sources(value.value)
            }
            SExpr::ReadPlace { place } | SExpr::Borrow { place, .. } => {
                self.layout_backing_sources_for_place(place)
            }
            SExpr::Field { base, field } => {
                self.layout_backing_sources_for_place(&SPlace::field(base.value, *field))
            }
            SExpr::Index { base, index } => self
                .layout_backing_sources_for_place(&SPlace::dynamic_index(base.value, index.value)),
            SExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => self.layout_backing_sources_for_place(&SPlace::variant_field(
                value.value,
                *variant,
                self.locals[value.value.index()].ty,
                *field,
            )),
            SExpr::Call {
                callee,
                args,
                effect_args,
                ..
            } => self.classify_call_layout_backing_sources(*callee, args, effect_args),
            SExpr::ArrayRepeat { value, .. } => {
                let mut sources = self.local_layout_backing_sources(value.value);
                prefix_layout_backing_sources(&mut sources, LayoutBackingProjection::Index(None));
                sources
            }
            SExpr::AggregateMake { ty, fields } => {
                let mut sources = Vec::new();
                for (idx, field) in fields.iter().enumerate() {
                    let mut field_sources = self.local_layout_backing_sources(field.value);
                    let projection = if ty.is_array(self.db) {
                        LayoutBackingProjection::Index(Some(idx))
                    } else {
                        let Ok(idx) = u16::try_from(idx) else {
                            continue;
                        };
                        LayoutBackingProjection::Field(FieldIndex(idx))
                    };
                    prefix_layout_backing_sources(&mut field_sources, projection);
                    sources.extend(field_sources);
                }
                dedup_layout_backing_sources(sources)
            }
            SExpr::EnumMake {
                enum_ty: _,
                variant,
                fields,
            } => {
                let mut sources = Vec::new();
                for (idx, field) in fields.iter().enumerate() {
                    let Ok(idx) = u16::try_from(idx) else {
                        continue;
                    };
                    let mut field_sources = self.local_layout_backing_sources(field.value);
                    prefix_layout_backing_sources(
                        &mut field_sources,
                        LayoutBackingProjection::VariantField {
                            variant: *variant,
                            field: FieldIndex(idx),
                        },
                    );
                    sources.extend(field_sources);
                }
                dedup_layout_backing_sources(sources)
            }
            SExpr::CodeRegionRef { .. }
            | SExpr::Const(_)
            | SExpr::Unary { .. }
            | SExpr::Binary { .. }
            | SExpr::Cast { .. }
            | SExpr::GetEnumTag { .. }
            | SExpr::IsEnumVariant { .. }
            | SExpr::CodeRegionOffset { .. }
            | SExpr::CodeRegionLen { .. } => Vec::new(),
        }
    }

    fn classify_call_layout_backing_sources(
        &self,
        callee: crate::analysis::semantic::SemanticCalleeRef<'db>,
        args: &[crate::analysis::semantic::SOperand],
        effect_args: &[crate::analysis::semantic::SEffectArg<'db>],
    ) -> Vec<LayoutBackingSource<'db>> {
        let ReturnProvenance::Forwarded(return_sources) =
            callee.key.typed_body(self.db).return_provenance(self.db)
        else {
            return Vec::new();
        };
        let mut out = Vec::new();
        for return_source in return_sources {
            let (base_ty, base_sources) = match return_source.origin {
                CallableInputLayoutHoleOrigin::Receiver => {
                    let Some(base) = args.first().map(|arg| arg.value) else {
                        continue;
                    };
                    (
                        self.locals[base.index()].ty,
                        self.local_layout_backing_sources(base),
                    )
                }
                CallableInputLayoutHoleOrigin::ValueParam(param_idx) => {
                    let Some(base) = args.get(param_idx).map(|arg| arg.value) else {
                        continue;
                    };
                    (
                        self.locals[base.index()].ty,
                        self.local_layout_backing_sources(base),
                    )
                }
                CallableInputLayoutHoleOrigin::Effect(effect_idx) => {
                    let Some(effect_arg) = effect_args
                        .iter()
                        .find(|arg| arg.binding_idx as usize == effect_idx)
                    else {
                        continue;
                    };
                    match &effect_arg.arg {
                        SEffectArgValue::Place(place) => (
                            effect_arg
                                .target_ty
                                .unwrap_or(self.locals[place.local.index()].ty),
                            self.layout_backing_sources_for_place(place),
                        ),
                        SEffectArgValue::Value(value) => {
                            let base = value.value;
                            (
                                effect_arg.target_ty.unwrap_or(self.locals[base.index()].ty),
                                self.local_layout_backing_sources(base),
                            )
                        }
                    }
                }
            };
            let Some((target, path)) =
                self.return_projection_path(base_ty, &return_source.projection, args)
            else {
                continue;
            };
            let mut projected = project_layout_backing_sources(base_sources, &target, &path);
            let Some(result_target) = self.return_result_target(&return_source.result_projection)
            else {
                continue;
            };
            prefix_layout_backing_source_path(&mut projected, &result_target);
            out.extend(projected);
        }
        dedup_layout_backing_sources(out)
    }

    fn return_projection_path(
        &self,
        base_ty: TyId<'db>,
        projection: &[ReturnProjectionStep],
        args: &[crate::analysis::semantic::SOperand],
    ) -> Option<(Vec<LayoutBackingProjection>, SemanticProjectionPath<'db>)> {
        let mut target = Vec::new();
        let mut path = SemanticProjectionPath::new();
        let mut ty = base_ty
            .as_capability(self.db)
            .map_or(base_ty, |(_, inner)| inner);
        for step in projection {
            match *step {
                ReturnProjectionStep::Field(field) => {
                    target.push(LayoutBackingProjection::Field(FieldIndex(field)));
                    path.push(Projection::Field(field as usize));
                    ty = *ty.field_types(self.db).get(field as usize)?;
                }
                ReturnProjectionStep::VariantField { variant, field } => {
                    target.push(LayoutBackingProjection::VariantField {
                        variant: VariantIndex(variant),
                        field: FieldIndex(field),
                    });
                    let enum_ty = ty;
                    let adt = ty.adt_def(self.db)?;
                    path.push(Projection::VariantField {
                        variant: VariantIndex(variant),
                        enum_ty,
                        field_idx: field as usize,
                    });
                    ty = instantiate_adt_field_shape(
                        self.db,
                        adt,
                        variant as usize,
                        field as usize,
                        ty.generic_args(self.db),
                    );
                }
                ReturnProjectionStep::ConstantIndex(index) => {
                    target.push(LayoutBackingProjection::Index(Some(index)));
                    path.push(Projection::Index(IndexSource::Constant(index)));
                    ty = *ty.generic_args(self.db).first()?;
                }
                ReturnProjectionStep::ParamIndex(param_idx) => {
                    target.push(LayoutBackingProjection::Index(None));
                    path.push(Projection::Index(IndexSource::Dynamic(
                        args.get(param_idx)?.value,
                    )));
                    ty = *ty.generic_args(self.db).first()?;
                }
                ReturnProjectionStep::AnyIndex => {
                    target.push(LayoutBackingProjection::Index(None));
                    path.push(Projection::Index(IndexSource::Constant(0)));
                    ty = *ty.generic_args(self.db).first()?;
                }
            }
        }
        Some((target, path))
    }

    fn return_result_target(
        &self,
        projection: &[ReturnProjectionStep],
    ) -> Option<Vec<LayoutBackingProjection>> {
        projection
            .iter()
            .map(|step| match *step {
                ReturnProjectionStep::Field(field) => {
                    Some(LayoutBackingProjection::Field(FieldIndex(field)))
                }
                ReturnProjectionStep::VariantField { variant, field } => {
                    Some(LayoutBackingProjection::VariantField {
                        variant: VariantIndex(variant),
                        field: FieldIndex(field),
                    })
                }
                ReturnProjectionStep::ConstantIndex(index) => {
                    Some(LayoutBackingProjection::Index(Some(index)))
                }
                ReturnProjectionStep::AnyIndex => Some(LayoutBackingProjection::Index(None)),
                ReturnProjectionStep::ParamIndex(_) => None,
            })
            .collect()
    }

    fn layout_backing_sources_for_place(
        &self,
        place: &SPlace<'db>,
    ) -> Vec<LayoutBackingSource<'db>> {
        let base_sources = self.local_layout_backing_sources(place.local);
        let target = layout_backing_source_target_for_path(&place.path);
        let projected = target.map_or_else(Vec::new, |target| {
            project_layout_backing_sources(base_sources, &target, &place.path)
        });
        if projected.is_empty() {
            whole_layout_backing_source(LayoutBackingPlace::Local(place.clone()))
        } else {
            projected
        }
    }

    fn local_layout_backing_sources(&self, local: SLocalId) -> Vec<LayoutBackingSource<'db>> {
        if !self.locals[local.index()].layout_backing_sources.is_empty() {
            return self.locals[local.index()].layout_backing_sources.clone();
        }
        if matches!(
            self.locals[local.index()].source,
            Some(
                LocalBinding::Param {
                    site: ParamSite::Func(_) | ParamSite::EffectField(_),
                    ..
                } | LocalBinding::EffectParam { .. }
            )
        ) {
            whole_layout_backing_source(LayoutBackingPlace::Local(SPlace::new(local)))
        } else {
            Vec::new()
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
        let base_value_ty = match &base_role {
            SemanticLocalRole::PlaceCarrier { value_ty, .. }
            | SemanticLocalRole::PlaceBoundValue { value_ty, .. } => Some(*value_ty),
            SemanticLocalRole::DirectCarrier { target_ty, .. } => Some(*target_ty),
            SemanticLocalRole::Erased | SemanticLocalRole::DirectValue { .. } => None,
        };
        match fallback {
            SemanticLocalRole::Erased => SemanticLocalRole::Erased,
            SemanticLocalRole::DirectValue { .. } => fallback,
            SemanticLocalRole::PlaceCarrier { value_ty, .. }
                if value_ty.is_zero_sized(self.db)
                    && base_value_ty.is_some_and(|ty| ty.is_zero_sized(self.db)) =>
            {
                // A projection out of a layout-only aggregate has no runtime
                // backing place to bind. Keep it as its own zero-sized
                // carrier while layout-backing provenance retains the exact
                // physical origin that borrow normalization must keep rooted.
                fallback
            }
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

fn whole_layout_backing_source<'db>(
    source: LayoutBackingPlace<'db>,
) -> Vec<LayoutBackingSource<'db>> {
    vec![LayoutBackingSource {
        target: Vec::new(),
        source,
    }]
}

fn dedup_layout_backing_sources<'db>(
    sources: Vec<LayoutBackingSource<'db>>,
) -> Vec<LayoutBackingSource<'db>> {
    let mut deduped = Vec::new();
    for source in sources {
        if !deduped.contains(&source) {
            deduped.push(source);
        }
    }
    deduped
}

fn merge_layout_backing_sources<'db>(
    current: Vec<LayoutBackingSource<'db>>,
    incoming: Vec<LayoutBackingSource<'db>>,
) -> Vec<LayoutBackingSource<'db>> {
    current
        .into_iter()
        .filter(|source| incoming.contains(source))
        .collect()
}

fn prefix_layout_backing_sources<'db>(
    sources: &mut [LayoutBackingSource<'db>],
    projection: LayoutBackingProjection,
) {
    for source in sources {
        source.target.insert(0, projection);
    }
}

fn prefix_layout_backing_source_path<'db>(
    sources: &mut [LayoutBackingSource<'db>],
    prefix: &[LayoutBackingProjection],
) {
    for source in sources {
        let mut target = prefix.to_vec();
        target.append(&mut source.target);
        source.target = target;
    }
}

fn layout_backing_source_projection_matches(
    pattern: LayoutBackingProjection,
    candidate: LayoutBackingProjection,
) -> bool {
    pattern == candidate
        || matches!(
            (pattern, candidate),
            (
                LayoutBackingProjection::Index(None),
                LayoutBackingProjection::Index(_)
            )
        )
}

fn layout_backing_source_path_is_prefix(
    prefix: &[LayoutBackingProjection],
    path: &[LayoutBackingProjection],
) -> bool {
    prefix.len() <= path.len()
        && prefix
            .iter()
            .copied()
            .zip(path.iter().copied())
            .all(|(pattern, candidate)| {
                layout_backing_source_projection_matches(pattern, candidate)
            })
}

fn append_layout_backing_source_path<'db>(
    source: &mut LayoutBackingPlace<'db>,
    suffix: &SemanticProjectionPath<'db>,
) {
    match source {
        LayoutBackingPlace::Local(place) => place.path = place.path.concat(suffix),
        LayoutBackingPlace::RootProvider { path, .. } => *path = path.concat(suffix),
    }
}

fn project_layout_backing_sources<'db>(
    sources: Vec<LayoutBackingSource<'db>>,
    target: &[LayoutBackingProjection],
    path: &SemanticProjectionPath<'db>,
) -> Vec<LayoutBackingSource<'db>> {
    debug_assert_eq!(target.len(), path.len());
    let mut projected = Vec::new();
    for mut source in sources {
        if layout_backing_source_path_is_prefix(&source.target, target) {
            let mut suffix = SemanticProjectionPath::new();
            for projection in path.iter().skip(source.target.len()) {
                suffix.push(projection.clone());
            }
            append_layout_backing_source_path(&mut source.source, &suffix);
            source.target.clear();
            projected.push(source);
        } else if layout_backing_source_path_is_prefix(target, &source.target) {
            source.target.drain(..target.len());
            projected.push(source);
        }
    }
    dedup_layout_backing_sources(projected)
}

fn layout_backing_source_target_for_path<'db>(
    path: &SemanticProjectionPath<'db>,
) -> Option<Vec<LayoutBackingProjection>> {
    path.iter()
        .filter_map(|projection| match projection {
            Projection::Field(field) => u16::try_from(*field)
                .ok()
                .map(FieldIndex)
                .map(LayoutBackingProjection::Field)
                .map(Some),
            Projection::VariantField {
                variant, field_idx, ..
            } => u16::try_from(*field_idx)
                .ok()
                .map(FieldIndex)
                .map(|field| LayoutBackingProjection::VariantField {
                    variant: *variant,
                    field,
                })
                .map(Some),
            Projection::Index(IndexSource::Constant(index)) => {
                Some(Some(LayoutBackingProjection::Index(Some(*index))))
            }
            Projection::Index(IndexSource::Dynamic(_)) => {
                Some(Some(LayoutBackingProjection::Index(None)))
            }
            Projection::Deref => None,
            Projection::Discriminant => Some(None),
        })
        .collect()
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
