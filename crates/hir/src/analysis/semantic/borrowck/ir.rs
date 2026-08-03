use cranelift_entity::{EntityRef, entity_impl};
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        place::projectable_place_ty,
        semantic::{
            BorrowActivation, BorrowSlotFamilyId, FieldIndex, LayoutBackingProjection, Mutability,
            SCallReturnSource, SConst, SLocalId, SStmtId, SemOrigin, SemanticBody,
            SemanticCalleeRef, SemanticCodeRegionRef, SemanticCodeRegionTarget, SemanticLocalKind,
            SemanticProjectionPath, VariantIndex,
        },
        ty::{
            adt_def::{AdtRef, instantiate_adt_field_shape},
            provider::ProviderAddressSpace,
            ty_check::{
                BodyOwner, EffectPassMode, LocalBinding, ReturnProjectionStep, ReturnSource,
            },
            ty_def::{BorrowKind, TyId},
        },
    },
    hir_def::StringId,
    projection::{IndexSource, Projection},
    semantic::ProviderBinding,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NBorrowRootId(u32);
entity_impl!(NBorrowRootId);

pub type NSProjectionPath<'db> = SemanticProjectionPath<'db>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NormalizedSemanticBody<'db> {
    pub owner: crate::analysis::semantic::SemanticInstance<'db>,
    pub template_owner: BodyOwner<'db>,
    pub entry_locals: Vec<SLocalId>,
    pub locals: Vec<NSLocal<'db>>,
    pub blocks: Vec<NSBlock<'db>>,
    pub borrow_roots: Vec<NBorrowRoot<'db>>,
}

impl<'db> NormalizedSemanticBody<'db> {
    pub fn local(&self, id: SLocalId) -> Option<&NSLocal<'db>> {
        self.locals.get(id.index())
    }

    pub fn block(&self, id: crate::analysis::semantic::SBlockId) -> Option<&NSBlock<'db>> {
        self.blocks.get(id.index())
    }

    pub fn root(&self, id: NBorrowRootId) -> Option<&NBorrowRoot<'db>> {
        self.borrow_roots.get(id.index())
    }

    /// Returns the semantic value type at the root of a normalized place.
    ///
    /// Provider identity describes where a value lives, but it does not
    /// necessarily describe the value projected through the place. Keeping this
    /// query on normalized IR prevents layout and runtime lowering from
    /// independently reconstructing that type from provider metadata.
    pub fn place_root_ty(&self, root: &NSPlaceRoot) -> Option<TyId<'db>> {
        match root {
            NSPlaceRoot::CarrierDerefLocal(local) => self.local(*local).map(NSLocal::layout_ty),
            NSPlaceRoot::Root(root) => match self.root(*root)? {
                NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local } => {
                    self.local(*local).map(NSLocal::layout_ty)
                }
                NBorrowRoot::Provider { value_ty, .. } => Some(*value_ty),
            },
        }
    }

    pub fn place_ty(&self, db: &'db dyn HirAnalysisDb, place: &NSPlace<'db>) -> Option<TyId<'db>> {
        let root_ty = self.place_root_ty(&place.root)?;
        semantic_projection_ty(db, root_ty, &place.path).map(|(ty, _)| ty)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NSLocal<'db> {
    pub ty: TyId<'db>,
    pub mutability: Mutability,
    pub source: Option<LocalBinding<'db>>,
    pub lowering: NormalizedBindingLowering<'db>,
    pub facts: NLocalFacts<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NormalizedBindingLowering<'db> {
    Erased,
    ValueLocal {
        place: NSPlace<'db>,
    },
    PlaceBoundValue {
        place: NSPlace<'db>,
        value_ty: TyId<'db>,
    },
    CarrierLocal {
        root: Option<NBorrowRootId>,
        provider: Option<ProviderBinding<'db>>,
        target_ty: TyId<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NLocalOrigin<'db> {
    SelfRooted,
    AliasedPlace,
    RootProvider(ProviderBinding<'db>),
}

impl<'db> NLocalOrigin<'db> {
    pub fn root_provider(&self) -> Option<&ProviderBinding<'db>> {
        match self {
            Self::RootProvider(provider) => Some(provider),
            Self::SelfRooted | Self::AliasedPlace => None,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct NLocalRootDemand {
    pub read_by_place: bool,
    pub written_by_place: bool,
    pub borrowed_or_addr_taken: bool,
    pub mut_borrowed_or_addr_taken: bool,
    pub passed_by_place: bool,
    pub nonself_backing_place: bool,
    pub always_rooted: bool,
}

impl NLocalRootDemand {
    pub fn needs_runtime_root(self) -> bool {
        self.read_by_place
            || self.written_by_place
            || self.borrowed_or_addr_taken
            || self.mut_borrowed_or_addr_taken
            || self.passed_by_place
            || self.nonself_backing_place
            || self.always_rooted
    }

    pub fn needs_projectable_owned_storage(self) -> bool {
        self.read_by_place
            || self.written_by_place
            || self.borrowed_or_addr_taken
            || self.mut_borrowed_or_addr_taken
            || self.passed_by_place
    }

    pub fn permits_unrooted_value_projection_reads(self) -> bool {
        !self.written_by_place
            && !self.borrowed_or_addr_taken
            && !self.mut_borrowed_or_addr_taken
            && !self.passed_by_place
    }

    pub fn disallows_const_ref_storage(self) -> bool {
        self.written_by_place || self.mut_borrowed_or_addr_taken
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NLocalFacts<'db> {
    pub interface: SemanticLocalKind,
    pub origin: NLocalOrigin<'db>,
    pub snapshot_source_place: Option<NSPlace<'db>>,
    /// Analysis-only possible ownership origins retained across explicitly
    /// non-consuming reads and control-flow joins.
    pub ownership_sources: Vec<NValueOwnershipSource<'db>>,
    pub layout_backing_sources: Vec<NLayoutBackingSource<'db>>,
    pub root_demand: NLocalRootDemand,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NValueOwnershipSource<'db> {
    Local,
    Place(NSPlace<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NLayoutBackingSource<'db> {
    pub target: Vec<LayoutBackingProjection>,
    pub source: NSPlace<'db>,
}

fn layout_backing_projection_matches(
    pattern: LayoutBackingProjection,
    candidate: LayoutBackingProjection,
) -> bool {
    pattern == candidate
        || matches!(
            (pattern, candidate),
            (
                LayoutBackingProjection::Index(None) | LayoutBackingProjection::IndexFamily(_),
                LayoutBackingProjection::Index(_) | LayoutBackingProjection::IndexFamily(_)
            )
        )
}

fn layout_backing_path_is_prefix(
    prefix: &[LayoutBackingProjection],
    path: &[LayoutBackingProjection],
) -> bool {
    prefix.len() <= path.len()
        && prefix
            .iter()
            .copied()
            .zip(path.iter().copied())
            .all(|(pattern, candidate)| layout_backing_projection_matches(pattern, candidate))
}

fn semantic_layout_query<'db>(
    path: &SemanticProjectionPath<'db>,
) -> Option<(Vec<LayoutBackingProjection>, SemanticProjectionPath<'db>)> {
    let mut target = Vec::new();
    let mut filtered = SemanticProjectionPath::new();
    for projection in path.iter() {
        let step = match projection {
            Projection::Field(field) => {
                LayoutBackingProjection::Field(FieldIndex(u16::try_from(*field).ok()?))
            }
            Projection::VariantField {
                variant, field_idx, ..
            } => LayoutBackingProjection::VariantField {
                variant: *variant,
                field: FieldIndex(u16::try_from(*field_idx).ok()?),
            },
            Projection::Index(IndexSource::Constant(index)) => {
                LayoutBackingProjection::Index(Some(*index))
            }
            Projection::Index(IndexSource::Dynamic(_)) => LayoutBackingProjection::Index(None),
            Projection::Deref => continue,
            Projection::Discriminant => return None,
        };
        target.push(step);
        filtered.push(projection.clone());
    }
    Some((target, filtered))
}

pub(super) fn layout_path_for_semantic_projection<'db>(
    path: &SemanticProjectionPath<'db>,
) -> Option<Vec<LayoutBackingProjection>> {
    semantic_layout_query(path).map(|(target, _)| target)
}

pub(super) fn semantic_projection_for_layout_path<'db>(
    db: &'db dyn HirAnalysisDb,
    mut ty: TyId<'db>,
    path: &[LayoutBackingProjection],
) -> Option<SemanticProjectionPath<'db>> {
    let mut out = SemanticProjectionPath::new();
    for step in path {
        ty = projectable_place_ty(db, ty);
        match *step {
            LayoutBackingProjection::Field(field) => {
                ty = *ty.field_types(db).get(field.0 as usize)?;
                out.push(Projection::Field(field.0 as usize));
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
                out.push(Projection::VariantField {
                    variant,
                    enum_ty: ty,
                    field_idx: field.0 as usize,
                });
                ty = field_ty;
            }
            LayoutBackingProjection::Index(Some(index)) => {
                if !ty.is_array(db) || ty.array_len(db).is_some_and(|len| index >= len) {
                    return None;
                }
                ty = *ty.generic_args(db).first()?;
                out.push(Projection::Index(IndexSource::Constant(index)));
            }
            LayoutBackingProjection::Index(None) | LayoutBackingProjection::IndexFamily(_) => {
                return None;
            }
        }
    }
    Some(out)
}

pub(crate) fn semantic_projection_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    mut ty: TyId<'db>,
    path: &SemanticProjectionPath<'db>,
) -> Option<(TyId<'db>, bool)> {
    let mut traverses_capability = false;
    for projection in path.iter() {
        if !matches!(projection, Projection::Deref) {
            while let Some((_, inner)) = ty.as_capability(db) {
                traverses_capability = true;
                ty = inner;
            }
        }
        ty = match projection {
            Projection::Field(field) => *ty.field_types(db).get(*field)?,
            Projection::VariantField {
                variant, field_idx, ..
            } => {
                let adt = ty.adt_def(db)?;
                instantiate_adt_field_shape(
                    db,
                    adt,
                    variant.0 as usize,
                    *field_idx,
                    ty.generic_args(db),
                )
            }
            Projection::Index(_) => {
                if !ty.is_array(db) {
                    return None;
                }
                *ty.generic_args(db).first()?
            }
            Projection::Deref => {
                let (_, inner) = ty.as_capability(db)?;
                traverses_capability = true;
                inner
            }
            Projection::Discriminant => return None,
        };
    }
    traverses_capability |= ty.as_capability(db).is_some();
    Some((ty, traverses_capability))
}

/// Returns whether a store replaces a capability held in an aggregate slot,
/// rather than writing through that capability.
///
/// The destination must end at a capability and the source must itself be a
/// capability. This remains a rebind when the slot is reached through an
/// earlier capability; runtime lowering still stores the source's transport
/// in that nested slot.
pub fn store_rebinds_capability<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedSemanticBody<'db>,
    dst: &NSPlace<'db>,
    src: NOperand,
) -> bool {
    if dst.path.is_empty()
        || body
            .place_ty(db, dst)
            .and_then(|ty| ty.as_capability(db))
            .is_none()
        || body
            .local(src.local)
            .and_then(|local| local.ty.as_capability(db))
            .is_none()
    {
        return false;
    }

    true
}

/// Returns whether a return-provenance path reaches storage supplied through
/// an incoming capability carrier.
///
/// A borrow into an owned aggregate parameter is rooted in fresh callee
/// storage, even when the result itself is a capability. Only a capability
/// encountered on the input side makes that borrow replayable at the call
/// site.
pub(crate) fn return_projection_reaches_capability<'db>(
    db: &'db dyn HirAnalysisDb,
    mut ty: TyId<'db>,
    projection: &[ReturnProjectionStep],
) -> bool {
    let mut traverses_capability = false;
    for step in projection {
        while let Some((_, inner)) = ty.as_capability(db) {
            traverses_capability = true;
            ty = inner;
        }
        ty = match step {
            ReturnProjectionStep::Field(field) => {
                let Some(field_ty) = ty.field_types(db).get(usize::from(*field)).copied() else {
                    return false;
                };
                field_ty
            }
            ReturnProjectionStep::VariantField { variant, field } => {
                let Some(adt) = ty.adt_def(db) else {
                    return false;
                };
                instantiate_adt_field_shape(
                    db,
                    adt,
                    usize::from(*variant),
                    usize::from(*field),
                    ty.generic_args(db),
                )
            }
            ReturnProjectionStep::ConstantIndex(_)
            | ReturnProjectionStep::DynamicIndex(_)
            | ReturnProjectionStep::AnyIndex => {
                if !ty.is_array(db) {
                    return false;
                }
                let Some(element_ty) = ty.generic_args(db).first().copied() else {
                    return false;
                };
                element_ty
            }
        };
    }
    traverses_capability || ty.as_capability(db).is_some()
}

/// Tests a single returned borrow slot against its corresponding input leaf.
///
/// `source.result_projection` may describe an aggregate containing the borrow
/// rather than the borrow slot itself. The unmatched result suffix therefore
/// has to be replayed on the input before deciding whether the source reaches
/// an incoming capability.
pub(crate) fn return_source_borrow_input_reaches_capability<'db>(
    db: &'db dyn HirAnalysisDb,
    input_ty: TyId<'db>,
    source: &ReturnSource,
    result: &[LayoutBackingProjection],
) -> Option<bool> {
    if source.result_projection.len() > result.len() {
        return None;
    }
    let mut input_projection = source.projection.clone();
    for (source_step, result_step) in source.result_projection.iter().zip(result) {
        let matches = match (source_step, result_step) {
            (ReturnProjectionStep::Field(source), LayoutBackingProjection::Field(result)) => {
                *source == result.0
            }
            (
                ReturnProjectionStep::VariantField {
                    variant: source_variant,
                    field: source_field,
                },
                LayoutBackingProjection::VariantField {
                    variant: result_variant,
                    field: result_field,
                },
            ) => *source_variant == result_variant.0 && *source_field == result_field.0,
            (
                ReturnProjectionStep::ConstantIndex(source),
                LayoutBackingProjection::Index(Some(result)),
            ) => source == result,
            (
                ReturnProjectionStep::ConstantIndex(_)
                | ReturnProjectionStep::DynamicIndex(_)
                | ReturnProjectionStep::AnyIndex,
                LayoutBackingProjection::Index(None) | LayoutBackingProjection::IndexFamily(_),
            )
            | (
                ReturnProjectionStep::DynamicIndex(_) | ReturnProjectionStep::AnyIndex,
                LayoutBackingProjection::Index(Some(_)),
            ) => true,
            _ => false,
        };
        if !matches {
            return None;
        }
    }
    input_projection.extend(result[source.result_projection.len()..].iter().map(
        |step| match step {
            LayoutBackingProjection::Field(field) => ReturnProjectionStep::Field(field.0),
            LayoutBackingProjection::VariantField { variant, field } => {
                ReturnProjectionStep::VariantField {
                    variant: variant.0,
                    field: field.0,
                }
            }
            LayoutBackingProjection::Index(Some(index)) => {
                ReturnProjectionStep::ConstantIndex(*index)
            }
            LayoutBackingProjection::Index(None) | LayoutBackingProjection::IndexFamily(_) => {
                ReturnProjectionStep::AnyIndex
            }
        },
    ));
    Some(return_projection_reaches_capability(
        db,
        input_ty,
        &input_projection,
    ))
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BorrowResult {
    pub kind: BorrowKind,
    pub projection: Vec<LayoutBackingProjection>,
}

#[derive(Default)]
pub(super) struct BorrowSlotFamilyIds {
    next: BorrowSlotFamilyId,
}

impl BorrowSlotFamilyIds {
    fn allocate(&mut self) -> BorrowSlotFamilyId {
        let family = self.next;
        self.next = self
            .next
            .checked_add(1)
            .expect("borrow-slot family id space exhausted");
        family
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::semantic::LayoutBackingProjection;

    use super::BorrowSlotFamilyIds;

    #[test]
    fn borrow_slot_family_ids_remain_distinct_beyond_u16_space() {
        let mut family_ids = BorrowSlotFamilyIds::default();
        let first_id_outside_u16 = usize::from(u16::MAX) + 1;
        let mut previous = None;

        for expected in 0..=first_id_outside_u16 {
            let projection = LayoutBackingProjection::IndexFamily(family_ids.allocate());
            assert_eq!(projection, LayoutBackingProjection::IndexFamily(expected));
            assert_ne!(Some(projection), previous);
            previous = Some(projection);
        }
    }
}

pub(crate) fn borrow_results_in_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Vec<BorrowResult> {
    borrow_results_in_ty_impl(db, ty, false, &mut BorrowSlotFamilyIds::default())
}

pub(crate) fn return_borrow_results_in_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Vec<BorrowResult> {
    borrow_results_in_ty_impl(db, ty, true, &mut BorrowSlotFamilyIds::default())
}

pub(super) fn borrow_results_in_ty_with_family_ids<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    family_ids: &mut BorrowSlotFamilyIds,
) -> Vec<BorrowResult> {
    borrow_results_in_ty_impl(db, ty, false, family_ids)
}

pub(super) fn return_borrow_results_in_ty_with_family_ids<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    family_ids: &mut BorrowSlotFamilyIds,
) -> Vec<BorrowResult> {
    borrow_results_in_ty_impl(db, ty, true, family_ids)
}

fn borrow_results_in_ty_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    track_views: bool,
    family_ids: &mut BorrowSlotFamilyIds,
) -> Vec<BorrowResult> {
    fn collect<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        track_views: bool,
        path: &mut Vec<LayoutBackingProjection>,
        visiting: &mut Vec<TyId<'db>>,
        family_ids: &mut BorrowSlotFamilyIds,
        out: &mut Vec<BorrowResult>,
    ) {
        if let Some((kind, _)) = ty.as_borrow(db) {
            out.push(BorrowResult {
                kind,
                projection: path.clone(),
            });
            return;
        }
        if let Some(inner) = ty.as_view(db) {
            if track_views && inner.as_capability(db).is_none() {
                out.push(BorrowResult {
                    kind: BorrowKind::Ref,
                    projection: path.clone(),
                });
            }
            collect(db, inner, track_views, path, visiting, family_ids, out);
            return;
        }
        if visiting.contains(&ty) {
            return;
        }
        visiting.push(ty);

        if ty.is_array(db) {
            if ty.array_len(db) == Some(0) {
                visiting.pop();
                return;
            }
            if let Some(elem) = ty.generic_args(db).first().copied() {
                let family_checkpoint = family_ids.next;
                let result_checkpoint = out.len();
                let family = family_ids.allocate();
                path.push(LayoutBackingProjection::IndexFamily(family));
                collect(db, elem, track_views, path, visiting, family_ids, out);
                path.pop();
                if out.len() == result_checkpoint {
                    family_ids.next = family_checkpoint;
                }
            }
        } else if let Some(closure) = ty.as_closure(db) {
            for (idx, field_ty) in closure.captures(db).iter().copied().enumerate() {
                let Ok(idx) = u16::try_from(idx) else {
                    continue;
                };
                path.push(LayoutBackingProjection::Field(FieldIndex(idx)));
                collect(db, field_ty, track_views, path, visiting, family_ids, out);
                path.pop();
            }
        } else if ty.is_tuple(db)
            || ty
                .adt_def(db)
                .is_some_and(|adt| matches!(adt.adt_ref(db), AdtRef::Struct(_)))
        {
            for (idx, field_ty) in ty.field_types(db).into_iter().enumerate() {
                let Ok(idx) = u16::try_from(idx) else {
                    continue;
                };
                path.push(LayoutBackingProjection::Field(FieldIndex(idx)));
                collect(db, field_ty, track_views, path, visiting, family_ids, out);
                path.pop();
            }
        } else if let Some(adt) = ty.adt_def(db)
            && matches!(adt.adt_ref(db), AdtRef::Enum(_))
        {
            for (variant_idx, variant) in adt.fields(db).iter().enumerate() {
                let Ok(variant_idx) = u16::try_from(variant_idx) else {
                    continue;
                };
                for field_idx in 0..variant.num_types() {
                    let Ok(field) = u16::try_from(field_idx) else {
                        continue;
                    };
                    let field_ty = instantiate_adt_field_shape(
                        db,
                        adt,
                        variant_idx as usize,
                        field_idx,
                        ty.generic_args(db),
                    );
                    path.push(LayoutBackingProjection::VariantField {
                        variant: VariantIndex(variant_idx),
                        field: FieldIndex(field),
                    });
                    collect(db, field_ty, track_views, path, visiting, family_ids, out);
                    path.pop();
                }
            }
        }

        visiting.pop();
    }

    let mut out = Vec::new();
    collect(
        db,
        ty,
        track_views,
        &mut Vec::new(),
        &mut Vec::new(),
        family_ids,
        &mut out,
    );
    out.sort_unstable();
    out.dedup();
    out
}

pub(super) fn resolved_layout_backing_places<'db>(
    sources: &[NLayoutBackingSource<'db>],
    requested: &SemanticProjectionPath<'db>,
) -> Vec<NSPlace<'db>> {
    let Some((target, path)) = semantic_layout_query(requested) else {
        return Vec::new();
    };
    let mut resolved = Vec::new();
    for source in sources {
        if !layout_backing_path_is_prefix(&source.target, &target) {
            continue;
        }
        let mut suffix = SemanticProjectionPath::new();
        for projection in path.iter().skip(source.target.len()) {
            suffix.push(projection.clone());
        }
        let mut place = source.source.clone();
        place.path = place.path.concat(&suffix);
        if !resolved.contains(&place) {
            resolved.push(place);
        }
    }
    resolved
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NBorrowRoot<'db> {
    Param {
        local: SLocalId,
        param_idx: u32,
    },
    LocalSlot {
        local: SLocalId,
    },
    Provider {
        binding: ProviderBinding<'db>,
        value_ty: TyId<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NSBlock<'db> {
    pub stmts: Vec<NSStmt<'db>>,
    pub terminator: NSTerminator<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NSPlace<'db> {
    pub root: NSPlaceRoot,
    pub path: NSProjectionPath<'db>,
}

impl<'db> NSPlace<'db> {
    pub fn dynamic_index_locals(&self) -> impl Iterator<Item = SLocalId> + '_ {
        self.path.iter().filter_map(|projection| match projection {
            Projection::Index(IndexSource::Dynamic(index)) => Some(*index),
            _ => None,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NSPlaceRoot {
    Root(NBorrowRootId),
    CarrierDerefLocal(SLocalId),
}

impl NSPlaceRoot {
    pub fn borrow_root(&self) -> Option<NBorrowRootId> {
        match self {
            Self::Root(root) => Some(*root),
            Self::CarrierDerefLocal(_) => None,
        }
    }
}

impl<'db> NormalizedBindingLowering<'db> {
    pub fn root(&self) -> Option<NBorrowRootId> {
        match self {
            Self::Erased => None,
            Self::ValueLocal { place } => place.root.borrow_root(),
            Self::PlaceBoundValue { place, .. } => place.root.borrow_root(),
            Self::CarrierLocal { root, .. } => *root,
        }
    }

    pub fn place(&self) -> Option<&NSPlace<'db>> {
        match self {
            Self::ValueLocal { place } | Self::PlaceBoundValue { place, .. } => Some(place),
            Self::Erased | Self::CarrierLocal { .. } => None,
        }
    }
}

impl<'db> NSLocal<'db> {
    pub fn is_derived_place_bound_alias(&self) -> bool {
        matches!(
            (&self.facts.interface, &self.facts.origin),
            (
                SemanticLocalKind::PlaceBoundValue,
                NLocalOrigin::AliasedPlace
            )
        )
    }

    pub fn layout_ty(&self) -> TyId<'db> {
        match (&self.facts.interface, &self.lowering) {
            (
                SemanticLocalKind::PlaceCarrier | SemanticLocalKind::DirectCarrier,
                NormalizedBindingLowering::CarrierLocal { target_ty, .. },
            ) => *target_ty,
            (
                SemanticLocalKind::PlaceBoundValue,
                NormalizedBindingLowering::PlaceBoundValue { value_ty, .. },
            ) => *value_ty,
            (
                SemanticLocalKind::Erased | SemanticLocalKind::DirectValue,
                NormalizedBindingLowering::Erased
                | NormalizedBindingLowering::ValueLocal { .. }
                | NormalizedBindingLowering::PlaceBoundValue { .. }
                | NormalizedBindingLowering::CarrierLocal { .. },
            ) => self.ty,
            (
                SemanticLocalKind::PlaceCarrier
                | SemanticLocalKind::PlaceBoundValue
                | SemanticLocalKind::DirectCarrier,
                _,
            ) => panic!("normalized semantic local has inconsistent interface and lowering"),
        }
    }

    pub fn backing_place(&self) -> Option<&NSPlace<'db>> {
        self.lowering.place()
    }

    pub fn snapshot_source_place(&self) -> Option<&NSPlace<'db>> {
        self.facts.snapshot_source_place.as_ref()
    }

    pub fn ownership_sources(&self) -> &[NValueOwnershipSource<'db>] {
        &self.facts.ownership_sources
    }

    pub fn layout_backing_sources(&self) -> &[NLayoutBackingSource<'db>] {
        &self.facts.layout_backing_sources
    }
}

pub(crate) fn local_has_runtime_move_semantics<'db>(
    db: &'db dyn HirAnalysisDb,
    local: &NSLocal<'db>,
    borrow_roots: &[NBorrowRoot<'db>],
) -> bool {
    !matches!(
        local.lowering,
        NormalizedBindingLowering::Erased | NormalizedBindingLowering::CarrierLocal { .. }
    ) && local.ty.as_capability(db).is_none()
        && match &local.lowering {
            NormalizedBindingLowering::ValueLocal { place } => !matches!(
                place
                    .root
                    .borrow_root()
                    .and_then(|root| borrow_roots.get(root.index())),
                Some(NBorrowRoot::Provider { .. })
            ),
            NormalizedBindingLowering::PlaceBoundValue { .. }
            | NormalizedBindingLowering::CarrierLocal { .. }
            | NormalizedBindingLowering::Erased => false,
        }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ReadMode {
    Copy,
    Read,
    Move,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NOperand {
    pub local: SLocalId,
    pub origin: Option<crate::hir_def::ExprId>,
    pub mode: ReadMode,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct NCallReturnSources<'a> {
    pub(crate) sources: &'a [SCallReturnSource],
    pub(crate) complete: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NEffectArg<'db> {
    pub binding_idx: u32,
    pub arg: NEffectArgValue<'db>,
    pub pass_mode: EffectPassMode,
    pub required_mut: bool,
    pub target_ty: Option<TyId<'db>>,
    pub provider: Option<ProviderAddressSpace>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NEffectArgValue<'db> {
    Place(NSPlace<'db>),
    Value(NOperand),
}

impl<'db> NEffectArgValue<'db> {
    pub fn place_operand(&self) -> Option<&NSPlace<'db>> {
        match self {
            Self::Place(place) => Some(place),
            Self::Value(_) => None,
        }
    }

    pub fn value_operand(&self) -> Option<NOperand> {
        match self {
            Self::Value(value) => Some(*value),
            Self::Place(_) => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NExpr<'db> {
    Use(NOperand),
    CodeRegionRef {
        region: SemanticCodeRegionRef<'db>,
    },
    ReadPlace {
        place: NSPlace<'db>,
        mode: ReadMode,
    },
    Borrow {
        place: NSPlace<'db>,
        kind: BorrowKind,
        provider: Option<ProviderAddressSpace>,
        activation: BorrowActivation,
    },
    Const(SConst<'db>),
    Unary {
        op: crate::hir_def::UnOp,
        value: NOperand,
    },
    Binary {
        op: crate::hir_def::BinOp,
        lhs: NOperand,
        rhs: NOperand,
    },
    Cast {
        value: NOperand,
        to: TyId<'db>,
    },
    ArrayRepeat {
        ty: TyId<'db>,
        value: NOperand,
    },
    AggregateMake {
        ty: TyId<'db>,
        fields: Box<[NOperand]>,
    },
    EnumMake {
        enum_ty: TyId<'db>,
        variant: VariantIndex,
        fields: Box<[NOperand]>,
    },
    GetEnumTag {
        value: NOperand,
    },
    IsEnumVariant {
        value: NOperand,
        variant: VariantIndex,
    },
    ExtractEnumField {
        value: NOperand,
        variant: VariantIndex,
        field: FieldIndex,
    },
    CodeRegionOffset {
        target: SemanticCodeRegionTarget<'db>,
    },
    CodeRegionLen {
        target: SemanticCodeRegionTarget<'db>,
    },
    Call {
        call_site: crate::analysis::semantic::CallSiteId,
        callee: SemanticCalleeRef<'db>,
        args: Box<[NOperand]>,
        effect_args: Box<[NEffectArg<'db>]>,
        return_sources: Box<[SCallReturnSource]>,
        return_sources_complete: bool,
    },
}

impl<'db> NExpr<'db> {
    pub fn for_each_value_operand(&self, mut f: impl FnMut(NOperand)) {
        match self {
            Self::Use(value)
            | Self::Unary { value, .. }
            | Self::Cast { value, .. }
            | Self::ArrayRepeat { value, .. }
            | Self::GetEnumTag { value }
            | Self::IsEnumVariant { value, .. }
            | Self::ExtractEnumField { value, .. } => f(*value),
            Self::Binary { lhs, rhs, .. } => {
                f(*lhs);
                f(*rhs);
            }
            Self::AggregateMake { fields, .. } | Self::EnumMake { fields, .. } => {
                for field in fields {
                    f(*field);
                }
            }
            Self::Call {
                args,
                effect_args,
                return_sources,
                ..
            } => {
                for arg in args {
                    f(*arg);
                }
                for value in effect_args
                    .iter()
                    .filter_map(|effect_arg| effect_arg.arg.value_operand())
                {
                    f(value);
                }
                for index in return_sources
                    .iter()
                    .flat_map(|source| source.result_projection.iter().chain(&source.projection))
                    .filter_map(|step| match step {
                        crate::analysis::semantic::SCallReturnProjectionStep::DynamicIndex(
                            index,
                        ) => Some(*index),
                        crate::analysis::semantic::SCallReturnProjectionStep::Field(_)
                        | crate::analysis::semantic::SCallReturnProjectionStep::VariantField {
                            ..
                        }
                        | crate::analysis::semantic::SCallReturnProjectionStep::ConstantIndex(_)
                        | crate::analysis::semantic::SCallReturnProjectionStep::AnyIndex => None,
                    })
                {
                    f(NOperand {
                        local: index,
                        origin: None,
                        mode: ReadMode::Copy,
                    });
                }
            }
            Self::ReadPlace { .. }
            | Self::Borrow { .. }
            | Self::Const(_)
            | Self::CodeRegionRef { .. }
            | Self::CodeRegionOffset { .. }
            | Self::CodeRegionLen { .. } => {}
        }
    }

    pub fn try_for_each_value_operand<E>(
        &self,
        mut f: impl FnMut(NOperand) -> Result<(), E>,
    ) -> Result<(), E> {
        let mut result = Ok(());
        self.for_each_value_operand(|operand| {
            if result.is_ok() {
                result = f(operand);
            }
        });
        result
    }

    pub fn for_each_place_operand(&self, mut f: impl FnMut(&NSPlace<'db>)) {
        match self {
            Self::ReadPlace { place, .. } | Self::Borrow { place, .. } => f(place),
            Self::Call { effect_args, .. } => {
                for place in effect_args
                    .iter()
                    .filter_map(|effect_arg| effect_arg.arg.place_operand())
                {
                    f(place);
                }
            }
            Self::Use(_)
            | Self::CodeRegionRef { .. }
            | Self::Const(_)
            | Self::Unary { .. }
            | Self::Binary { .. }
            | Self::Cast { .. }
            | Self::ArrayRepeat { .. }
            | Self::AggregateMake { .. }
            | Self::EnumMake { .. }
            | Self::GetEnumTag { .. }
            | Self::IsEnumVariant { .. }
            | Self::ExtractEnumField { .. }
            | Self::CodeRegionOffset { .. }
            | Self::CodeRegionLen { .. } => {}
        }
    }

    pub fn try_for_each_place_operand<E>(
        &self,
        mut f: impl FnMut(&NSPlace<'db>) -> Result<(), E>,
    ) -> Result<(), E> {
        let mut result = Ok(());
        self.for_each_place_operand(|place| {
            if result.is_ok() {
                result = f(place);
            }
        });
        result
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NSStmt<'db> {
    pub id: SStmtId,
    pub origin: SemOrigin<'db>,
    pub kind: NSStmtKind<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NSStmtKind<'db> {
    Assign { dst: SLocalId, expr: NExpr<'db> },
    Store { dst: NSPlace<'db>, src: NOperand },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NSTerminator<'db> {
    pub origin: SemOrigin<'db>,
    pub kind: NSTerminatorKind<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NSTerminatorKind<'db> {
    Goto(crate::analysis::semantic::SBlockId),
    Branch {
        cond: NOperand,
        then_bb: crate::analysis::semantic::SBlockId,
        else_bb: crate::analysis::semantic::SBlockId,
    },
    MatchEnum {
        value: NOperand,
        enum_ty: TyId<'db>,
        cases: Box<[(VariantIndex, crate::analysis::semantic::SBlockId)]>,
        default: Option<crate::analysis::semantic::SBlockId>,
    },
    Assert {
        message: Option<StringId<'db>>,
    },
    Return(Option<NOperand>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SemanticNormalizeError<'db> {
    MissingBorrowRoot {
        local: SLocalId,
    },
    LocalProvenanceCycle {
        owner: crate::analysis::semantic::SemanticInstance<'db>,
        local: SLocalId,
    },
    NonPlaceDerivedValue {
        owner: crate::analysis::semantic::SemanticInstance<'db>,
        local: SLocalId,
        base: SLocalId,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BorrowInput {
    Place {
        param: u32,
        projection: Vec<LayoutBackingProjection>,
    },
    AnyInParam(u32),
}

impl BorrowInput {
    pub fn param(&self) -> u32 {
        match self {
            Self::Place { param, .. } | Self::AnyInParam(param) => *param,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BorrowTransform {
    pub result: BorrowResult,
    pub input: BorrowInput,
}

pub type BorrowSummary = Vec<BorrowTransform>;

#[salsa::interned]
#[derive(Debug)]
pub struct BorrowSummaryId<'db> {
    #[return_ref]
    pub items: Vec<BorrowTransform>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct SemanticBorrowDiagnostic<'db> {
    pub kind: SemanticBorrowDiagKind,
    pub instance: crate::analysis::semantic::SemanticInstance<'db>,
    pub primary: SemanticBorrowDiagnosticLabel<'db>,
    pub secondaries: Vec<SemanticBorrowDiagnosticLabel<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct SemanticBorrowDiagnosticLabel<'db> {
    pub message: String,
    pub span: SemanticBorrowDiagnosticSpan<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticBorrowDiagnosticSpan<'db> {
    Origin {
        owner: BodyOwner<'db>,
        origin: SemOrigin<'db>,
    },
    OriginWithTemplateFallback {
        owner: BodyOwner<'db>,
        template_owner: BodyOwner<'db>,
        origin: SemOrigin<'db>,
    },
    LocalSourceOrBody {
        instance: crate::analysis::semantic::SemanticInstance<'db>,
        local: SLocalId,
    },
}

#[salsa::interned]
#[derive(Debug)]
pub struct BorrowDiagnosticId<'db> {
    pub diag: SemanticBorrowDiagnostic<'db>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct NormalizedSemanticBodyId<'db> {
    #[return_ref]
    pub body: NormalizedSemanticBody<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticBorrowSummaryResult<'db> {
    Ok(Option<BorrowSummaryId<'db>>),
    Err(BorrowDiagnosticId<'db>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticBorrowCheckResult<'db> {
    Ok,
    Err(BorrowDiagnosticId<'db>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticNormalizeResult<'db> {
    Ok(NormalizedSemanticBodyId<'db>),
    Err(BorrowDiagnosticId<'db>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticBorrowDiagKind {
    BorrowConflict,
    MoveConflict,
    InvalidReturnBorrow,
    Internal,
    NoEscViolation,
    ProviderProvenanceConflict,
    UninitializedLocal,
    NonRegularPolymorphicRecursion,
}

pub fn empty_normalized_body<'db>(
    body: &SemanticBody<'db>,
    locals: Vec<NSLocal<'db>>,
    borrow_roots: Vec<NBorrowRoot<'db>>,
) -> NormalizedSemanticBody<'db> {
    NormalizedSemanticBody {
        owner: body.owner,
        template_owner: body.template_owner,
        entry_locals: body.entry_locals.clone(),
        locals,
        blocks: Vec::new(),
        borrow_roots,
    }
}
