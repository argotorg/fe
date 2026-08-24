use cranelift_entity::SecondaryMap;

use crate::{
    analysis::{
        HirAnalysisDb,
        place::projectable_place_ty,
        semantic::{FieldIndex, LayoutBackingProjection, SLocalId, SemOrigin, SemanticInstance},
        ty::{
            adt_def::{AdtRef, instantiate_adt_field_shape},
            provider::{ProviderAddressSpace, ProviderKind},
            ty_check::LocalBinding,
            ty_def::{BorrowKind, TyId},
            ty_is_noesc,
        },
    },
    projection::{IndexSource, Projection},
};

use super::{
    diagnostics::normalized_body_internal_diag,
    guard::{ExistentialId, Guard, IndexExpr},
    ir::{
        NBorrowRoot, NBorrowRootId, NSPlace, NSPlaceRoot, NSProjectionPath,
        NormalizedBindingLowering, NormalizedSemanticBody, SemanticBorrowDiagnostic,
        layout_path_for_semantic_projection, resolved_layout_backing_places,
        semantic_projection_for_layout_path, semantic_projection_ty,
    },
    loan::{AuthoritySet, LoanDef, LoanRef, ParentSet},
    region::{RegionProjection, RegionRoot, RegionSet, SymbolicPlace},
    shape::capability_shape,
    summary::{BorrowSource, BorrowSourceClause, SummaryPath, SummaryProjection},
    transfer::{BorrowState, BorrowStateValueId, slot_path_for_layout},
};

pub(super) fn address_space_for_region_root<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedSemanticBody<'db>,
    root: &RegionRoot<'db>,
    origin: SemOrigin<'db>,
) -> Result<ProviderAddressSpace, SemanticBorrowDiagnostic<'db>> {
    match root {
        RegionRoot::ParamPlace(_) | RegionRoot::ParamCapability { .. } | RegionRoot::Local(_) => {
            Ok(ProviderAddressSpace::Memory)
        }
        RegionRoot::Provider(binding) => match binding.semantics.address_space {
            Some(space) => Ok(space),
            None if matches!(binding.semantics.kind, ProviderKind::RootObject) => {
                Ok(ProviderAddressSpace::Memory)
            }
            None => Err(normalized_body_internal_diag(
                db,
                instance,
                body,
                origin,
                format!(
                    "provider `{}` has no address space",
                    binding.provider_ty.pretty_print(db)
                ),
            )),
        },
    }
}

fn region_projection_from_semantic<'db>(
    path: &NSProjectionPath<'db>,
) -> Option<Vec<RegionProjection>> {
    let mut out = Vec::new();
    for projection in path.iter() {
        out.push(match projection {
            Projection::Field(field) => {
                RegionProjection::Field(FieldIndex(u16::try_from(*field).ok()?))
            }
            Projection::VariantField {
                variant, field_idx, ..
            } => RegionProjection::VariantField {
                variant: *variant,
                field: FieldIndex(u16::try_from(*field_idx).ok()?),
            },
            Projection::Index(IndexSource::Constant(index)) => {
                RegionProjection::Index(IndexExpr::Const(*index))
            }
            Projection::Index(IndexSource::Dynamic(index)) => {
                RegionProjection::Index(IndexExpr::Runtime(*index))
            }
            Projection::Discriminant => continue,
            Projection::Deref => return None,
        });
    }
    Some(out)
}

fn region_projection_for_layout_path<'db>(
    db: &'db dyn HirAnalysisDb,
    mut ty: TyId<'db>,
    path: &[LayoutBackingProjection],
) -> Option<Vec<RegionProjection>> {
    let mut out = Vec::new();
    let mut next_existential = 0;
    for step in path {
        ty = projectable_place_ty(db, ty);
        match *step {
            LayoutBackingProjection::Field(field) => {
                ty = *ty.field_types(db).get(field.0 as usize)?;
                out.push(RegionProjection::Field(field));
            }
            LayoutBackingProjection::VariantField { variant, field } => {
                let adt = ty.adt_def(db)?;
                if !matches!(adt.adt_ref(db), AdtRef::Enum(_)) {
                    return None;
                }
                let field_ty = instantiate_adt_field_shape(
                    db,
                    adt,
                    variant.0 as usize,
                    field.0 as usize,
                    ty.generic_args(db),
                );
                out.push(RegionProjection::VariantField { variant, field });
                ty = field_ty;
            }
            LayoutBackingProjection::Index(index) => {
                if !ty.is_array(db)
                    || index.is_some_and(|index| ty.array_len(db).is_some_and(|len| index >= len))
                {
                    return None;
                }
                ty = *ty.generic_args(db).first()?;
                out.push(RegionProjection::Index(index.map_or_else(
                    || {
                        let id = ExistentialId(next_existential);
                        next_existential += 1;
                        IndexExpr::Existential(id)
                    },
                    IndexExpr::Const,
                )));
            }
            LayoutBackingProjection::IndexFamily(_) => {
                if !ty.is_array(db) {
                    return None;
                }
                ty = *ty.generic_args(db).first()?;
                let id = ExistentialId(next_existential);
                next_existential += 1;
                out.push(RegionProjection::Index(IndexExpr::Existential(id)));
            }
        }
    }
    Some(out)
}

pub(super) struct BorrowCanonCx<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &'a NormalizedSemanticBody<'db>,
    loans: &'a [LoanDef<'db>],
    constant_indices: &'a SecondaryMap<SLocalId, Option<usize>>,
}

impl<'a, 'db> BorrowCanonCx<'a, 'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: &'a NormalizedSemanticBody<'db>,
        loans: &'a [LoanDef<'db>],
        constant_indices: &'a SecondaryMap<SLocalId, Option<usize>>,
    ) -> Self {
        Self {
            db,
            instance,
            body,
            loans,
            constant_indices,
        }
    }

    fn materialize_constant_indices(&self, path: &NSProjectionPath<'db>) -> NSProjectionPath<'db> {
        let mut out = NSProjectionPath::new();
        for projection in path.iter() {
            out.push(match projection {
                Projection::Index(IndexSource::Dynamic(index))
                    if let Some(index) = self.constant_indices[*index] =>
                {
                    Projection::Index(IndexSource::Constant(index))
                }
                projection => projection.clone(),
            });
        }
        out
    }

    fn layout_path(&self, path: &NSProjectionPath<'db>) -> Option<Vec<LayoutBackingProjection>> {
        layout_path_for_semantic_projection(&self.materialize_constant_indices(path))
    }

    pub(super) fn active_region_for_held(&self, held: &LoanRef, guard: &Guard) -> RegionSet<'db> {
        self.loans[held.id.0 as usize]
            .instantiate(held)
            .with_guard(guard)
    }

    fn deepest_held_projection_region(
        &self,
        state: &BorrowState<'db>,
        local: SLocalId,
        path: &NSProjectionPath<'db>,
    ) -> Option<RegionSet<'db>> {
        let path = self.materialize_constant_indices(path);
        let projection = self.layout_path(&path)?;
        let shape = self.local_shape(local)?;
        let (depth, value) = (0..=projection.len()).rev().find_map(|depth| {
            let path = slot_path_for_layout(self.db, shape, &projection[..depth])?;
            let value = state.project(local, &path, super::guard::ValueScope::Local(local))?;
            (!state
                .leaves(value, super::guard::ValueScope::Local(local))
                .is_empty())
            .then_some((depth, value))
        })?;
        let mut consumed = 0;
        let mut suffix = NSProjectionPath::default();
        for projection in path.iter() {
            if consumed < depth {
                if !matches!(projection, Projection::Deref) {
                    consumed += 1;
                }
                continue;
            }
            if suffix.is_empty() && matches!(projection, Projection::Deref) {
                continue;
            }
            suffix.push(projection.clone());
        }
        let suffix = region_projection_from_semantic(&suffix)?;
        let mut region = RegionSet::empty();
        for leaf in state.leaves(value, super::guard::ValueScope::Local(local)) {
            region = region.union(
                &self
                    .active_region_for_held(&leaf.payload, &leaf.guard)
                    .project(&suffix),
            );
        }
        Some(region)
    }

    fn local_shape(&self, local: SLocalId) -> Option<super::shape::ShapeId<'db>> {
        self.body
            .local(local)
            .map(|local| capability_shape(self.db, local.ty))
    }

    fn place_base_local(&self, place: &NSPlace<'db>) -> Option<SLocalId> {
        match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => Some(local),
            NSPlaceRoot::Root(root) => match self.body.root(root)? {
                NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local } => Some(*local),
                NBorrowRoot::Provider { .. } => None,
            },
        }
    }

    pub(super) fn value_region(&self, state: &BorrowState<'db>, local: SLocalId) -> RegionSet<'db> {
        if self
            .body
            .local(local)
            .is_some_and(|local| local.ty.as_borrow(self.db).is_some())
        {
            return self.borrow_local_region(state, local);
        }

        let Some(local_data) = self.body.local(local) else {
            return RegionSet::empty();
        };
        if let Some(place) = local_data.lowering.place() {
            return self.place_region(state, place);
        }
        let root = match &local_data.lowering {
            NormalizedBindingLowering::CarrierLocal { root, provider, .. } => provider
                .clone()
                .map(RegionRoot::Provider)
                .or_else(|| root.and_then(|root| self.root_to_region_root(root))),
            NormalizedBindingLowering::Erased => None,
            NormalizedBindingLowering::ValueLocal { .. }
            | NormalizedBindingLowering::PlaceBoundValue { .. } => unreachable!(),
        };
        root.map_or_else(RegionSet::empty, |root| {
            RegionSet::singleton(SymbolicPlace::new(root, []))
        })
    }

    pub(super) fn value_projection_region(
        &self,
        state: &BorrowState<'db>,
        local: SLocalId,
        projection: &NSProjectionPath<'db>,
    ) -> RegionSet<'db> {
        let Some(local_data) = self.body.local(local) else {
            return RegionSet::empty();
        };
        let projection = self.materialize_constant_indices(projection);
        let traverses_capability = semantic_projection_ty(self.db, local_data.ty, &projection)
            .is_none_or(|(_, traverses_capability)| traverses_capability);
        if traverses_capability
            && let Some(region) = self.deepest_held_projection_region(state, local, &projection)
        {
            return region;
        }
        if local_data.ty.as_borrow(self.db).is_none() {
            let resolved =
                resolved_layout_backing_places(local_data.layout_backing_sources(), &projection);
            if !resolved.is_empty() {
                return resolved
                    .iter()
                    .map(|place| self.place_region(state, place))
                    .fold(RegionSet::empty(), |region, source| region.union(&source));
            }
        }
        if local_data.ty.as_borrow(self.db).is_none()
            && ty_is_noesc(self.db, local_data.ty)
            && !matches!(
                local_data.source,
                Some(LocalBinding::Param { .. } | LocalBinding::EffectParam { .. })
            )
            && traverses_capability
        {
            return RegionSet::empty();
        }

        region_projection_from_semantic(&projection).map_or_else(RegionSet::empty, |projection| {
            self.value_region(state, local).project(&projection)
        })
    }

    pub(super) fn value_layout_region(
        &self,
        state: &BorrowState<'db>,
        local: SLocalId,
        projection: &[LayoutBackingProjection],
    ) -> RegionSet<'db> {
        let Some(local_ty) = self.body.local(local).map(|local| local.ty) else {
            return RegionSet::empty();
        };
        if let Some(projection) = semantic_projection_for_layout_path(self.db, local_ty, projection)
        {
            return self.value_projection_region(state, local, &projection);
        }
        let Some(local_data) = self.body.local(local) else {
            return RegionSet::empty();
        };
        if let Some(shape) = self.local_shape(local)
            && let Some(path) = slot_path_for_layout(self.db, shape, projection)
            && let Some(value) = state.project(local, &path, super::guard::ValueScope::Local(local))
        {
            let region = state
                .leaves(value, super::guard::ValueScope::Local(local))
                .into_iter()
                .fold(RegionSet::empty(), |region, leaf| {
                    region.union(&self.active_region_for_held(&leaf.payload, &leaf.guard))
                });
            if !region.is_empty() {
                return region;
            }
        }
        let Some(suffix) = region_projection_for_layout_path(self.db, local_ty, projection) else {
            return self.borrow_local_region(state, local);
        };
        if local_data.ty.as_borrow(self.db).is_none()
            && ty_is_noesc(self.db, local_data.ty)
            && !matches!(
                local_data.source,
                Some(LocalBinding::Param { .. } | LocalBinding::EffectParam { .. })
            )
        {
            return RegionSet::empty();
        }
        self.value_region(state, local).project(&suffix)
    }

    pub(super) fn instantiate_call_source(
        &self,
        state: &BorrowState<'db>,
        args: &[super::ir::NOperand],
        clause: &BorrowSourceClause,
    ) -> (RegionSet<'db>, ParentSet) {
        let Some(arg) = args.get(clause.source.param() as usize) else {
            return (RegionSet::empty(), ParentSet::default());
        };
        let (region, parents) = match &clause.source {
            BorrowSource::ParamCapability { param, slot } => self
                .local_shape(arg.local)
                .and_then(|shape| super::transfer::slot_path_for_summary(self.db, shape, slot))
                .and_then(|path| {
                    state.project(arg.local, &path, super::guard::ValueScope::Argument(*param))
                })
                .map(|value| self.regions_and_parents_for_value(state, value, *param))
                .unwrap_or_default(),
            BorrowSource::ParamPlace { path, .. } => {
                let region = region_projection_for_summary_path(path)
                    .map(|path| self.value_region(state, arg.local).project(&path))
                    .unwrap_or_default();
                (region, ParentSet::default())
            }
            BorrowSource::AnyAccessible { param, class } => {
                let mut resolved = self
                    .local_shape(arg.local)
                    .zip(state.value(arg.local))
                    .map(|(_, value)| self.regions_and_parents_for_value(state, value, *param))
                    .unwrap_or_default();
                let direct = self.value_region(state, arg.local);
                resolved.0 = resolved.0.union(&direct);
                if matches!(class, super::summary::AccessClass::Shared) {
                    resolved.1 = ParentSet::default();
                }
                resolved
            }
        };
        (region.with_guard(&clause.guard), parents)
    }

    fn regions_and_parents_for_value(
        &self,
        state: &BorrowState<'db>,
        value: BorrowStateValueId<'db>,
        param: u32,
    ) -> (RegionSet<'db>, ParentSet) {
        let mut region = RegionSet::empty();
        let mut parents = Vec::new();
        for leaf in state.leaves(value, super::guard::ValueScope::Argument(param)) {
            region = region.union(&self.active_region_for_held(&leaf.payload, &leaf.guard));
            if self.loans[leaf.payload.id.0 as usize].kind() == BorrowKind::Mut {
                parents.push((leaf.guard, leaf.payload));
            }
        }
        (region, ParentSet::from_guarded_references(parents))
    }

    pub(super) fn place_layout_region(
        &self,
        state: &BorrowState<'db>,
        place: &NSPlace<'db>,
        target_ty: TyId<'db>,
        projection: &[LayoutBackingProjection],
    ) -> RegionSet<'db> {
        if let Some(suffix) = semantic_projection_for_layout_path(self.db, target_ty, projection) {
            if let Some(local) = self.place_base_local(place) {
                let path = self
                    .materialize_constant_indices(&place.path)
                    .concat(&suffix);
                return self.value_projection_region(state, local, &path);
            }
            let mut projected = place.clone();
            projected.path = projected.path.concat(&suffix);
            return self.place_region(state, &projected);
        }

        if let Some(local) = self.place_base_local(place) {
            let mut path = self
                .layout_path(&self.materialize_constant_indices(&place.path))
                .unwrap_or_default();
            path.extend_from_slice(projection);
            let region = self
                .local_shape(local)
                .and_then(|shape| slot_path_for_layout(self.db, shape, &path))
                .and_then(|path| {
                    state.project(local, &path, super::guard::ValueScope::Local(local))
                })
                .into_iter()
                .flat_map(|value| state.leaves(value, super::guard::ValueScope::Local(local)))
                .fold(RegionSet::empty(), |region, leaf| {
                    region.union(&self.active_region_for_held(&leaf.payload, &leaf.guard))
                });
            if !region.is_empty() {
                return region;
            }
            if self.body.local(local).is_some_and(|local| {
                ty_is_noesc(self.db, local.ty)
                    && !matches!(
                        local.source,
                        Some(LocalBinding::Param { .. } | LocalBinding::EffectParam { .. })
                    )
            }) {
                return RegionSet::empty();
            }
        }

        self.place_region(state, place)
    }

    pub(super) fn borrow_local_region(
        &self,
        state: &BorrowState<'db>,
        local: SLocalId,
    ) -> RegionSet<'db> {
        let Some(local_data) = self.body.local(local) else {
            return RegionSet::empty();
        };
        let held_loans = state.leaves_in(local, super::guard::ValueScope::Local(local));
        let has_tracked_loan = !held_loans.is_empty();
        let mut region = RegionSet::empty();
        for leaf in held_loans {
            region = region.union(&self.active_region_for_held(&leaf.payload, &leaf.guard));
        }
        if !region.is_empty() || has_tracked_loan {
            return region;
        }

        if let Some(place) = local_data.lowering.place() {
            return self.place_region(state, place);
        }
        match &local_data.lowering {
            NormalizedBindingLowering::CarrierLocal { root, provider, .. } => provider
                .clone()
                .map(RegionRoot::Provider)
                .or_else(|| root.and_then(|root| self.root_to_region_root(root)))
                .map_or_else(RegionSet::empty, |root| {
                    RegionSet::singleton(SymbolicPlace::new(root, []))
                }),
            NormalizedBindingLowering::Erased => RegionSet::empty(),
            NormalizedBindingLowering::ValueLocal { .. }
            | NormalizedBindingLowering::PlaceBoundValue { .. } => RegionSet::empty(),
        }
    }

    pub(super) fn resolve_place(
        &self,
        state: &BorrowState<'db>,
        place: &NSPlace<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<RegionSet<'db>, SemanticBorrowDiagnostic<'db>> {
        let region = self.place_region(state, place);
        if region.is_empty() {
            return Err(self.internal_diag(
                origin,
                "cannot canonicalize carrier-rooted place".to_string(),
            ));
        }
        Ok(region)
    }

    fn place_region(&self, state: &BorrowState<'db>, place: &NSPlace<'db>) -> RegionSet<'db> {
        let Some(path) =
            region_projection_from_semantic(&self.materialize_constant_indices(&place.path))
        else {
            return RegionSet::empty();
        };
        match place.root {
            NSPlaceRoot::Root(root) => {
                let root = self
                    .root_to_region_root(root)
                    .expect("normalized borrow root");
                RegionSet::singleton(SymbolicPlace::new(root, path))
            }
            NSPlaceRoot::CarrierDerefLocal(local) => {
                let suffix = path;
                let mut region = RegionSet::empty();
                let mut resolved = false;
                for leaf in state.leaves_in(local, super::guard::ValueScope::Local(local)) {
                    resolved = true;
                    region = region.union(
                        &self
                            .active_region_for_held(&leaf.payload, &leaf.guard)
                            .project(&suffix),
                    );
                }
                if !resolved
                    && let Some(NormalizedBindingLowering::CarrierLocal { root, provider, .. }) =
                        self.body.local(local).map(|local| &local.lowering)
                {
                    if let Some(provider) = provider {
                        region = region.union(&RegionSet::singleton(SymbolicPlace::new(
                            RegionRoot::Provider(provider.clone()),
                            suffix.clone(),
                        )));
                    } else if let Some(root) = root.and_then(|root| self.root_to_region_root(root))
                    {
                        region =
                            region.union(&RegionSet::singleton(SymbolicPlace::new(root, suffix)));
                    }
                }
                region
            }
        }
    }

    pub(super) fn root_to_region_root(&self, root: NBorrowRootId) -> Option<RegionRoot<'db>> {
        match self.body.root(root)? {
            NBorrowRoot::Param { param_idx, .. } => Some(RegionRoot::ParamPlace(*param_idx)),
            NBorrowRoot::LocalSlot { local } => Some(RegionRoot::Local(*local)),
            NBorrowRoot::Provider { binding, .. } => Some(RegionRoot::Provider(binding.clone())),
        }
    }

    pub(super) fn mut_authority_for_place(
        &self,
        state: &BorrowState<'db>,
        place: &NSPlace<'db>,
    ) -> AuthoritySet {
        match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => {
                self.authority_for_value(state, local, Some(BorrowKind::Mut), None)
            }
            NSPlaceRoot::Root(_) => AuthoritySet::default(),
        }
    }

    pub(super) fn mut_parent_refs_for_place(
        &self,
        state: &BorrowState<'db>,
        place: &NSPlace<'db>,
        region: &RegionSet<'db>,
    ) -> ParentSet {
        self.place_base_local(place)
            .map_or_else(ParentSet::default, |local| {
                self.mut_parent_refs_for_value(state, local, region)
            })
    }

    pub(super) fn mut_authority_for_place_targets(
        &self,
        state: &BorrowState<'db>,
        place: &NSPlace<'db>,
        region: &RegionSet<'db>,
    ) -> AuthoritySet {
        self.place_base_local(place).map_or_else(
            || self.mut_authority_for_place(state, place),
            |local| self.authority_for_value(state, local, Some(BorrowKind::Mut), Some(region)),
        )
    }

    pub(super) fn authority_for_place_targets(
        &self,
        state: &BorrowState<'db>,
        place: &NSPlace<'db>,
        region: &RegionSet<'db>,
    ) -> AuthoritySet {
        self.place_base_local(place).map_or_else(
            || self.authority_for_place(state, place),
            |local| self.authority_for_value(state, local, None, Some(region)),
        )
    }

    pub(super) fn authority_for_place(
        &self,
        state: &BorrowState<'db>,
        place: &NSPlace<'db>,
    ) -> AuthoritySet {
        match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => {
                self.authority_for_value(state, local, None, None)
            }
            NSPlaceRoot::Root(_) => AuthoritySet::default(),
        }
    }

    pub(super) fn mut_parent_refs_for_value(
        &self,
        state: &BorrowState<'db>,
        local: SLocalId,
        region: &RegionSet<'db>,
    ) -> ParentSet {
        self.value_authorities(state, local, Some(BorrowKind::Mut), Some(region))
    }

    pub(super) fn mut_authority_for_value_targets(
        &self,
        state: &BorrowState<'db>,
        local: SLocalId,
        region: &RegionSet<'db>,
    ) -> AuthoritySet {
        self.authority_for_value(state, local, Some(BorrowKind::Mut), Some(region))
    }

    pub(super) fn authority_for_value_targets(
        &self,
        state: &BorrowState<'db>,
        local: SLocalId,
        region: &RegionSet<'db>,
    ) -> AuthoritySet {
        self.authority_for_value(state, local, None, Some(region))
    }

    fn authority_for_value(
        &self,
        state: &BorrowState<'db>,
        local: SLocalId,
        kind: Option<BorrowKind>,
        region: Option<&RegionSet<'db>>,
    ) -> AuthoritySet {
        AuthoritySet::from_parents(self.value_authorities(state, local, kind, region))
    }

    fn value_authorities(
        &self,
        state: &BorrowState<'db>,
        local: SLocalId,
        kind: Option<BorrowKind>,
        region: Option<&RegionSet<'db>>,
    ) -> ParentSet {
        let mut leaves = state.leaves_in(local, super::guard::ValueScope::Local(local));
        if leaves.is_empty()
            && let Some(source) = self
                .body
                .local(local)
                .and_then(|local| local.snapshot_source_place())
                .and_then(|source| self.place_base_local(source))
        {
            leaves = state.leaves_in(source, super::guard::ValueScope::Local(source));
        }
        ParentSet::from_guarded_references(leaves.into_iter().filter_map(|leaf| {
            (kind.is_none_or(|kind| self.loans[leaf.payload.id.0 as usize].kind() == kind)
                && region.is_none_or(|region| {
                    self.active_region_for_held(&leaf.payload, &leaf.guard)
                        .may_overlap(region)
                        .is_some()
                }))
            .then_some((leaf.guard, leaf.payload))
        }))
    }

    fn internal_diag(
        &self,
        origin: SemOrigin<'db>,
        message: String,
    ) -> SemanticBorrowDiagnostic<'db> {
        normalized_body_internal_diag(self.db, self.instance, self.body, origin, message)
    }
}

fn region_projection_for_summary_path(path: &SummaryPath) -> Option<Vec<RegionProjection>> {
    path.as_slice()
        .iter()
        .map(|projection| match projection {
            SummaryProjection::Field(field) => Some(RegionProjection::Field(*field)),
            SummaryProjection::VariantField { variant, field } => {
                Some(RegionProjection::VariantField {
                    variant: *variant,
                    field: *field,
                })
            }
            SummaryProjection::Index(index) => Some(RegionProjection::Index(*index)),
        })
        .collect()
}
