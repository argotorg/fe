use common::indexmap::IndexMap;
use num_bigint::BigUint;
use num_traits::ToPrimitive;
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            ProviderAddressSpace,
            adt_def::{AdtDef, AdtRef, instantiate_adt_field_layout, instantiate_adt_field_shape},
            binder::Binder,
            const_ty::{
                ConstCanonEnv, ConstCanonMode, ConstTyData, ConstTyId, EvaluatedConstTy,
                HoleAnchor, HoleMinter, LayoutBoundaryIdentity, LayoutInstantiationContext,
                LayoutInstantiationId, LayoutOccurrenceStep, LayoutRootId, StructuralHoleOrigin,
                canonicalize_ty_for_mode,
            },
            layout_holes::{
                LayoutIndexDimension, LayoutInstantiation, LayoutViewRecurrence,
                classify_layout_view_recurrence, instantiate_layout_template,
                layout_hole_fallback_ty, layout_root_descends_from, layout_root_id,
                layout_root_lineage, layout_shape_key, rewrite_structural_holes,
                structural_hole_id,
            },
            normalize::{normalize_layout_root_uses, normalize_ty},
            provider::{
                ProviderLayoutResolution, StaticSlotLayoutResolution, resolve_effect_handle_layout,
                resolve_static_slot_layout,
            },
            trait_def::{ImplementorId, ResolvedImplInstance},
            trait_resolution::PredicateListId,
            ty_def::{PrimTy, TyBase, TyData, TyId},
            ty_lower::{lower_layout_root_uses_in_hir_ty, lower_opt_hir_ty},
        },
    },
    hir_def::{Contract, EnumVariant, FieldParent, IdentId, IntegerId, VariantKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct ContractFieldId<'db> {
    pub contract: Contract<'db>,
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct StoragePlace<'db> {
    pub field: ContractFieldId<'db>,
    pub steps: Vec<PlaceStep>,
}

impl<'db> StoragePlace<'db> {
    pub fn root(field: ContractFieldId<'db>) -> Self {
        Self {
            field,
            steps: Vec::new(),
        }
    }

    pub fn with_step(&self, step: PlaceStep) -> Self {
        let mut steps = self.steps.clone();
        steps.push(step);
        Self {
            field: self.field,
            steps,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum PlaceStep {
    StructField(u32),
    TupleElem(u32),
    EnumVariant(u32),
    EnumPayloadField(u32),
    ConstParam(u32),
    ArrayElem(usize),
    ProviderTarget,
    DeclaredWrapper,
    TransparentInner,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct RootOccurrenceId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct ConcreteRootOccurrenceId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct RootCellId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct LayoutRootFamilyId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum AllocationUnitId {
    Scalar(RootCellId),
    Indexed(LayoutRootFamilyId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum RootRole {
    Counted,
    MaterializeOnly,
}

impl RootRole {
    fn join(self, other: Self) -> Self {
        if matches!(self, Self::Counted) || matches!(other, Self::Counted) {
            Self::Counted
        } else {
            Self::MaterializeOnly
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct RootAllocation {
    pub space: ProviderAddressSpace,
    pub slot: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct RootOccurrence<'db> {
    pub id: RootOccurrenceId,
    pub root: LayoutRootId<'db>,
    pub placeholder: TyId<'db>,
    pub place: StoragePlace<'db>,
    pub selector: Vec<PlaceStep>,
    pub index_dimensions: Vec<LayoutIndexDimension<'db>>,
    pub role: RootRole,
    pub space: ProviderAddressSpace,
    pub order: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ConcreteRootOccurrence<'db> {
    pub id: ConcreteRootOccurrenceId,
    pub value: IntegerId<'db>,
    pub ty: TyId<'db>,
    pub owner: TyId<'db>,
    pub place: StoragePlace<'db>,
    pub selector: Vec<PlaceStep>,
    pub index_dimensions: Vec<LayoutIndexDimension<'db>>,
    pub role: RootRole,
    pub space: ProviderAddressSpace,
    pub order: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ExplicitRootReservation<'db> {
    pub value: IntegerId<'db>,
    pub space: ProviderAddressSpace,
    pub occurrences: Vec<(ContractFieldId<'db>, ConcreteRootOccurrenceId)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ContractLayoutReport<'db> {
    pub entries: Vec<ContractLayoutEntry<'db>>,
}

impl<'db> ContractLayoutReport<'db> {
    pub fn entries_for_field(
        &self,
        field: ContractFieldId<'db>,
    ) -> impl Iterator<Item = &ContractLayoutEntry<'db>> {
        self.entries
            .iter()
            .filter(move |entry| entry.field == field)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ContractLayoutEntry<'db> {
    pub field: ContractFieldId<'db>,
    pub path: ContractLayoutPath<'db>,
    pub ty: TyId<'db>,
    pub address_space: ProviderAddressSpace,
    pub value: ContractLayoutValue<'db>,
    pub kind: ContractLayoutEntryKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ContractLayoutEntryKind {
    InlineField,
    EnumTag,
    Parameter(ContractLayoutParameterOrigin),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ContractLayoutParameterOrigin {
    Explicit,
    Inferred,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum ContractLayoutValue<'db> {
    Scalar(IntegerId<'db>),
    Indexed {
        base: IntegerId<'db>,
        dimensions: Vec<usize>,
        strides: Vec<usize>,
        extent: usize,
    },
}

impl<'db> ContractLayoutValue<'db> {
    pub fn base(&self) -> IntegerId<'db> {
        match self {
            Self::Scalar(value) => *value,
            Self::Indexed { base, .. } => *base,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ContractLayoutPath<'db> {
    pub field: IdentId<'db>,
    pub segments: Vec<ContractLayoutPathSegment<'db>>,
}

impl<'db> ContractLayoutPath<'db> {
    pub fn display(&self, db: &'db dyn HirAnalysisDb) -> String {
        let mut path = self.field.data(db).to_string();
        for segment in &self.segments {
            match segment {
                ContractLayoutPathSegment::Member { name, .. } => {
                    path.push('.');
                    path.push_str(name.data(db));
                }
                ContractLayoutPathSegment::TupleElement(index) => {
                    path.push('.');
                    path.push_str(&index.to_string());
                }
                ContractLayoutPathSegment::Variant { name, .. } => {
                    path.push_str("::");
                    path.push_str(name.data(db));
                }
                ContractLayoutPathSegment::ArrayElement { dimension, .. } => {
                    path.push_str(&format!("[i{dimension}]"));
                }
                ContractLayoutPathSegment::ConstParameter { name, .. } => {
                    path.push('.');
                    path.push_str(name.data(db));
                }
                ContractLayoutPathSegment::EnumTag => path.push_str(".<tag>"),
            }
        }
        path
    }

    pub fn index_dimensions(&self) -> impl Iterator<Item = (u32, usize)> + '_ {
        self.segments.iter().filter_map(|segment| match segment {
            ContractLayoutPathSegment::ArrayElement { dimension, len } => Some((*dimension, *len)),
            _ => None,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ContractLayoutPathSegment<'db> {
    Member { name: IdentId<'db>, index: u32 },
    TupleElement(u32),
    Variant { name: IdentId<'db>, index: u32 },
    ArrayElement { dimension: u32, len: usize },
    ConstParameter { name: IdentId<'db>, index: u32 },
    EnumTag,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct RootCell<'db> {
    pub id: RootCellId,
    pub root: LayoutRootId<'db>,
    pub occurrences: Vec<RootOccurrenceId>,
    pub role: RootRole,
    pub space: ProviderAddressSpace,
    pub allocation: Option<RootAllocation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct LayoutRootFamily<'db> {
    pub id: LayoutRootFamilyId,
    pub lane: LayoutRootId<'db>,
    pub dimensions: Vec<LayoutIndexDimension<'db>>,
    pub strides: Vec<usize>,
    pub extent: usize,
    pub occurrences: Vec<RootOccurrenceId>,
    pub role: RootRole,
    pub space: ProviderAddressSpace,
    pub allocation: Option<RootAllocation>,
}

impl LayoutRootFamily<'_> {
    pub fn slot_for_indices(&self, indices: &[usize]) -> Option<usize> {
        let allocation = self.allocation?;
        if indices.len() != self.dimensions.len() {
            return None;
        }
        let mut offset = 0usize;
        for ((index, dimension), stride) in indices.iter().zip(&self.dimensions).zip(&self.strides)
        {
            if *index >= dimension.len {
                return None;
            }
            offset = offset.checked_add(index.checked_mul(*stride)?)?;
        }
        allocation.slot.checked_add(offset)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct EnumOverlayGroup<'db> {
    pub enum_place: StoragePlace<'db>,
    pub lane: u32,
    pub members: Vec<AllocationUnitId>,
    pub space: ProviderAddressSpace,
    pub reserved_extent: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutBindingTarget {
    Scalar(RootCellId),
    Indexed(LayoutRootFamilyId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct LayoutBindingLeaf {
    pub selector: Vec<PlaceStep>,
    pub target: LayoutBindingTarget,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum LayoutBinding {
    Bound(Vec<LayoutBindingLeaf>),
    NonPhysical,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutViewKind {
    Declared,
    Target,
    SlotBasis,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutProjection {
    Field(u16),
    VariantField { variant: u16, field: u16 },
    Index(Option<usize>),
    ConstParam(u16),
    EffectTarget,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update, Default)]
pub struct LayoutSelection {
    pub selector: Vec<PlaceStep>,
    pub indices: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum LayoutViewError<'db> {
    RootNotClassified { root: LayoutRootId<'db> },
    NonPhysicalRoot { root: LayoutRootId<'db> },
    RootNeedsLanding { root: LayoutRootId<'db> },
    RootNeedsIndex { root: LayoutRootId<'db> },
    InvalidIndex { root: LayoutRootId<'db> },
    MissingAllocation { root: LayoutRootId<'db> },
    InvalidProjection,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct AssignedLayoutTy<'db> {
    pub template: TyId<'db>,
    pub bindings: IndexMap<LayoutRootId<'db>, LayoutBinding>,
}

impl<'db> AssignedLayoutTy<'db> {
    pub fn all_roots_classified(&self, db: &'db dyn HirAnalysisDb) -> bool {
        crate::analysis::ty::layout_holes::collect_unique_structural_holes_in_order(
            db,
            self.template,
        )
        .into_iter()
        .all(|hole| self.bindings.contains_key(&hole.root(db)))
    }

    pub fn binding(&self, root: LayoutRootId<'db>) -> Option<&LayoutBinding> {
        self.bindings.get(&root)
    }

    /// Returns the source-level type shape without consuming any assigned root
    /// value. Callers that need an operational root must use `root_value` or a
    /// concrete/projected view instead.
    pub fn shape_ty(&self) -> TyId<'db> {
        self.template
    }

    pub fn shape_key(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> crate::analysis::ty::LayoutShapeKey<'db> {
        layout_shape_key(db, self.template)
    }
}

fn matching_binding_leaves<'a>(
    leaves: &'a [LayoutBindingLeaf],
    selector: &[PlaceStep],
) -> Vec<&'a LayoutBindingLeaf> {
    if selector.is_empty() {
        return leaves.iter().collect();
    }
    leaves
        .iter()
        .filter(|leaf| leaf.selector.starts_with(selector))
        .collect()
}

fn layout_projection_path(
    selector: &[PlaceStep],
    include_effect_targets: bool,
) -> Option<Vec<LayoutProjection>> {
    let mut projections = Vec::new();
    let mut steps = selector.iter();
    while let Some(step) = steps.next() {
        match *step {
            PlaceStep::StructField(field) | PlaceStep::TupleElem(field) => {
                projections.push(LayoutProjection::Field(field.try_into().ok()?));
            }
            PlaceStep::EnumVariant(variant) => {
                let PlaceStep::EnumPayloadField(field) = *steps.next()? else {
                    return None;
                };
                projections.push(LayoutProjection::VariantField {
                    variant: variant.try_into().ok()?,
                    field: field.try_into().ok()?,
                });
            }
            PlaceStep::ArrayElem(_) => projections.push(LayoutProjection::Index(None)),
            PlaceStep::ConstParam(param) => {
                projections.push(LayoutProjection::ConstParam(param.try_into().ok()?));
            }
            PlaceStep::ProviderTarget if include_effect_targets => {
                projections.push(LayoutProjection::EffectTarget);
            }
            PlaceStep::ProviderTarget => {}
            PlaceStep::DeclaredWrapper | PlaceStep::TransparentInner => {}
            PlaceStep::EnumPayloadField(_) => return None,
        }
    }
    Some(projections)
}

fn visible_layout_projection_path(selector: &[PlaceStep]) -> Option<Vec<LayoutProjection>> {
    layout_projection_path(selector, false)
}

fn visible_layout_projection_matches(
    requested: LayoutProjection,
    candidate: LayoutProjection,
) -> bool {
    requested == candidate
        || matches!(
            (requested, candidate),
            (LayoutProjection::Index(_), LayoutProjection::Index(_))
        )
}

fn visible_layout_projection_is_prefix(
    requested: &[LayoutProjection],
    candidate: &[LayoutProjection],
) -> bool {
    requested.len() <= candidate.len()
        && requested
            .iter()
            .copied()
            .zip(candidate.iter().copied())
            .all(|(requested, candidate)| visible_layout_projection_matches(requested, candidate))
}

fn project_layout_template<'db>(
    db: &'db dyn HirAnalysisDb,
    mut ty: TyId<'db>,
    selector: &[PlaceStep],
) -> Result<TyId<'db>, LayoutViewError<'db>> {
    let mut enum_variant = None;
    for step in selector {
        match *step {
            PlaceStep::ProviderTarget | PlaceStep::DeclaredWrapper => {}
            PlaceStep::TransparentInner => {
                ty = ty
                    .as_capability(db)
                    .map(|(_, inner)| inner)
                    .ok_or(LayoutViewError::InvalidProjection)?;
            }
            PlaceStep::StructField(field_idx) => {
                let adt = ty.adt_def(db).ok_or(LayoutViewError::InvalidProjection)?;
                if !matches!(adt.adt_ref(db), AdtRef::Struct(_)) {
                    return Err(LayoutViewError::InvalidProjection);
                }
                ty = instantiate_adt_field_shape(
                    db,
                    adt,
                    0,
                    field_idx as usize,
                    ty.generic_args(db),
                );
            }
            PlaceStep::TupleElem(elem_idx) => {
                if !ty.is_tuple(db) {
                    return Err(LayoutViewError::InvalidProjection);
                }
                ty = *ty
                    .generic_args(db)
                    .get(elem_idx as usize)
                    .ok_or(LayoutViewError::InvalidProjection)?;
            }
            PlaceStep::EnumVariant(variant_idx) => {
                let adt = ty.adt_def(db).ok_or(LayoutViewError::InvalidProjection)?;
                if !matches!(adt.adt_ref(db), AdtRef::Enum(_)) {
                    return Err(LayoutViewError::InvalidProjection);
                }
                enum_variant = Some((adt, variant_idx as usize, ty.generic_args(db).to_vec()));
            }
            PlaceStep::EnumPayloadField(field_idx) => {
                let (adt, variant_idx, args) = enum_variant
                    .take()
                    .ok_or(LayoutViewError::InvalidProjection)?;
                ty = instantiate_adt_field_shape(db, adt, variant_idx, field_idx as usize, &args);
            }
            PlaceStep::ArrayElem(_) => {
                if !ty.is_array(db) {
                    return Err(LayoutViewError::InvalidProjection);
                }
                ty = *ty
                    .generic_args(db)
                    .first()
                    .ok_or(LayoutViewError::InvalidProjection)?;
            }
            PlaceStep::ConstParam(param_idx) => {
                ty = *ty
                    .generic_args(db)
                    .get(param_idx as usize)
                    .ok_or(LayoutViewError::InvalidProjection)?;
            }
        }
    }
    if enum_variant.is_some() {
        return Err(LayoutViewError::InvalidProjection);
    }
    Ok(ty)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum AssignedRootValue<'db> {
    Literal {
        space: ProviderAddressSpace,
        slot: usize,
        ty: TyId<'db>,
    },
    Indexed {
        space: ProviderAddressSpace,
        base: usize,
        dimensions: Vec<LayoutIndexDimension<'db>>,
        strides: Vec<usize>,
        ty: TyId<'db>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum ContractLayoutError<'db> {
    InvalidFieldType,
    ExplicitContractLayoutHole { placeholder: TyId<'db> },
    NonSlotContractLayoutHole { placeholder: TyId<'db> },
    UnresolvedConcreteLayoutRoot { value: TyId<'db> },
    AmbiguousProviderLayout,
    UnresolvedProviderTarget,
    UnresolvedProviderSpace,
    NonRegularProviderCycle,
    UnresolvedStaticSlotSpace { owner: TyId<'db> },
    AmbiguousStaticSlot { owner: TyId<'db> },
    ConflictingLayoutRootSpaces { root: LayoutRootId<'db> },
    UnknownArrayLengthWithLayoutRoots { array: TyId<'db> },
    LayoutExtentOverflow,
    IncompleteAdtLayoutProjection { ty: TyId<'db> },
    AmbiguousLayoutBindingSelector { root: LayoutRootId<'db> },
    InconsistentLayoutRootType { root: LayoutRootId<'db> },
    LayoutRootNeedsLanding { root: LayoutRootId<'db> },
    LayoutRootNeedsIndex { root: LayoutRootId<'db> },
    InternalLayoutGraph,
}

impl ContractLayoutError<'_> {
    pub fn summary(&self) -> &'static str {
        match self {
            Self::InvalidFieldType => "field type is invalid",
            Self::ExplicitContractLayoutHole { .. } => {
                "explicit `_` const arguments are not layout roots"
            }
            Self::NonSlotContractLayoutHole { .. } => {
                "an unresolved const is not a `u256` or `usize` layout root"
            }
            Self::UnresolvedConcreteLayoutRoot { .. } => {
                "an explicit layout root did not evaluate to a concrete integer"
            }
            Self::AmbiguousProviderLayout => "provider layout selection is ambiguous",
            Self::UnresolvedProviderTarget => "provider target type is unresolved",
            Self::UnresolvedProviderSpace => "provider address space is unresolved",
            Self::NonRegularProviderCycle => {
                "provider target recursion changes its layout arguments"
            }
            Self::UnresolvedStaticSlotSpace { .. } => "static-slot address space is unresolved",
            Self::AmbiguousStaticSlot { .. } => "static-slot implementation is ambiguous",
            Self::ConflictingLayoutRootSpaces { .. } => {
                "one layout root has conflicting address spaces"
            }
            Self::UnknownArrayLengthWithLayoutRoots { .. } => {
                "root-bearing array length is not known"
            }
            Self::LayoutExtentOverflow => "layout extent overflowed",
            Self::IncompleteAdtLayoutProjection { .. } => "layout projection is incomplete",
            Self::AmbiguousLayoutBindingSelector { .. } => "layout binding selector is ambiguous",
            Self::InconsistentLayoutRootType { .. } => {
                "one layout root has inconsistent const types"
            }
            Self::LayoutRootNeedsLanding { .. } => "layout root needs a concrete landing",
            Self::LayoutRootNeedsIndex { .. } => "layout root needs a concrete index",
            Self::InternalLayoutGraph => "layout graph failed internal validation",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LayoutInvariantError<'db> {
    InvalidScalarCell {
        field: ContractFieldId<'db>,
        cell: RootCellId,
    },
    MissingScalarAllocation {
        field: ContractFieldId<'db>,
        cell: RootCellId,
    },
    MissingFamilyAllocation {
        field: ContractFieldId<'db>,
        family: LayoutRootFamilyId,
    },
    InvalidFamilyRegion {
        field: ContractFieldId<'db>,
        family: LayoutRootFamilyId,
    },
    InvalidOverlayGroup {
        field: ContractFieldId<'db>,
        lane: u32,
    },
    AllocationOverlap {
        space: ProviderAddressSpace,
        first_field: ContractFieldId<'db>,
        second_field: ContractFieldId<'db>,
    },
    InvalidFieldExtent {
        field: ContractFieldId<'db>,
    },
    InvalidAddressSpaceHighWater {
        space: ProviderAddressSpace,
    },
    UnclassifiedViewRoot {
        field: ContractFieldId<'db>,
        view: LayoutViewKind,
    },
    InvalidOccurrenceGraph {
        field: ContractFieldId<'db>,
        occurrence: RootOccurrenceId,
    },
    InvalidConcreteOccurrence {
        field: ContractFieldId<'db>,
        occurrence: ConcreteRootOccurrenceId,
    },
    InvalidExplicitReservation {
        space: ProviderAddressSpace,
    },
    ExplicitReservationOverlap {
        space: ProviderAddressSpace,
        field: ContractFieldId<'db>,
        value: IntegerId<'db>,
    },
    InvalidRootBinding {
        field: ContractFieldId<'db>,
        root: LayoutRootId<'db>,
    },
    InvalidPlaceBinding {
        field: ContractFieldId<'db>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
struct AllocationLane<'db> {
    members: Vec<AllocationUnitId>,
    overlays: Vec<AllocationOverlay<'db>>,
    space: ProviderAddressSpace,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
struct AllocationOverlay<'db> {
    place: StoragePlace<'db>,
    lane: u32,
    members: Vec<AllocationUnitId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct FieldStorageLayout<'db> {
    pub field: ContractFieldId<'db>,
    pub name: IdentId<'db>,
    pub is_mut: bool,
    pub is_provider: bool,
    pub address_space: ProviderAddressSpace,
    pub slot_offset: usize,
    pub slot_count: usize,
    pub inline_span: usize,
    inline_leaves: Vec<InlineLayoutLeaf<'db>>,
    pub declared: AssignedLayoutTy<'db>,
    pub target: AssignedLayoutTy<'db>,
    pub slot_basis: AssignedLayoutTy<'db>,
    pub occurrences: Vec<RootOccurrence<'db>>,
    pub concrete_occurrences: Vec<ConcreteRootOccurrence<'db>>,
    pub cells: Vec<RootCell<'db>>,
    pub families: Vec<LayoutRootFamily<'db>>,
    pub overlay_groups: Vec<EnumOverlayGroup<'db>>,
    pub place_roots: IndexMap<StoragePlace<'db>, Vec<LayoutBindingTarget>>,
    pub root_bindings: IndexMap<LayoutRootId<'db>, LayoutBinding>,
}

impl<'db> FieldStorageLayout<'db> {
    pub fn view(&self, kind: LayoutViewKind) -> &AssignedLayoutTy<'db> {
        match kind {
            LayoutViewKind::Declared => &self.declared,
            LayoutViewKind::Target => &self.target,
            LayoutViewKind::SlotBasis => &self.slot_basis,
        }
    }

    pub fn selection_for_projections(
        &self,
        db: &'db dyn HirAnalysisDb,
        kind: LayoutViewKind,
        projections: &[LayoutProjection],
    ) -> Result<LayoutSelection, LayoutViewError<'db>> {
        let mut ty = self.view(kind).template;
        let mut selector = self.view_selector(kind);
        let mut indices = Vec::new();
        let mut all_indices_static = true;
        for projection in projections {
            while let Some((_, inner)) = ty.as_capability(db) {
                selector.push(PlaceStep::TransparentInner);
                ty = inner;
            }
            let steps = match *projection {
                LayoutProjection::Field(index) if ty.is_tuple(db) => {
                    vec![PlaceStep::TupleElem(index as u32)]
                }
                LayoutProjection::Field(index)
                    if ty
                        .adt_def(db)
                        .is_some_and(|adt| matches!(adt.adt_ref(db), AdtRef::Struct(_))) =>
                {
                    vec![PlaceStep::StructField(index as u32)]
                }
                LayoutProjection::VariantField { variant, field }
                    if ty
                        .adt_def(db)
                        .is_some_and(|adt| matches!(adt.adt_ref(db), AdtRef::Enum(_))) =>
                {
                    vec![
                        PlaceStep::EnumVariant(variant as u32),
                        PlaceStep::EnumPayloadField(field as u32),
                    ]
                }
                LayoutProjection::Field(_) | LayoutProjection::VariantField { .. } => {
                    return Err(LayoutViewError::InvalidProjection);
                }
                LayoutProjection::Index(index) if ty.is_array(db) => {
                    if let Some(index) = index {
                        if all_indices_static {
                            indices.push(index);
                        }
                    } else {
                        all_indices_static = false;
                        indices.clear();
                    }
                    vec![PlaceStep::ArrayElem(0)]
                }
                LayoutProjection::Index(_) => return Err(LayoutViewError::InvalidProjection),
                LayoutProjection::ConstParam(param) => {
                    vec![PlaceStep::ConstParam(param as u32)]
                }
                LayoutProjection::EffectTarget => vec![PlaceStep::ProviderTarget],
            };
            ty = project_layout_template(db, ty, &steps)?;
            selector.extend(steps);
        }
        Ok(LayoutSelection { selector, indices })
    }

    fn view_selector(&self, kind: LayoutViewKind) -> Vec<PlaceStep> {
        match (self.is_provider, kind) {
            (true, LayoutViewKind::Declared) => vec![PlaceStep::DeclaredWrapper],
            (true, LayoutViewKind::Target | LayoutViewKind::SlotBasis) => {
                vec![PlaceStep::ProviderTarget]
            }
            (false, _) => Vec::new(),
        }
    }

    pub fn project(
        &self,
        db: &'db dyn HirAnalysisDb,
        kind: LayoutViewKind,
        selection: &LayoutSelection,
    ) -> Result<AssignedLayoutTy<'db>, LayoutViewError<'db>> {
        let template = project_layout_template(db, self.view(kind).template, &selection.selector)?;
        let view = assigned_view(db, template, &self.root_bindings);
        let mut bindings = IndexMap::new();
        for (root, binding) in view.bindings {
            let binding = match binding {
                LayoutBinding::NonPhysical => LayoutBinding::NonPhysical,
                LayoutBinding::Bound(leaves) => {
                    let leaves = matching_binding_leaves(&leaves, &selection.selector);
                    if leaves.is_empty() {
                        return Err(LayoutViewError::RootNeedsLanding { root });
                    }
                    LayoutBinding::Bound(leaves.into_iter().cloned().collect())
                }
            };
            bindings.insert(root, binding);
        }
        Ok(AssignedLayoutTy { template, bindings })
    }

    pub fn projected_concrete_ty(
        &self,
        db: &'db dyn HirAnalysisDb,
        kind: LayoutViewKind,
        selection: &LayoutSelection,
    ) -> Result<TyId<'db>, LayoutViewError<'db>> {
        let view = self.project(db, kind, selection)?;
        self.try_concrete_ty(db, &view, selection)
    }

    pub fn root_target_for_place(&self, place: &StoragePlace<'db>) -> Option<LayoutBindingTarget> {
        let [target] = self.place_roots.get(place)?.as_slice() else {
            return None;
        };
        Some(*target)
    }

    pub fn root_targets_for_place(&self, place: &StoragePlace<'db>) -> &[LayoutBindingTarget] {
        self.place_roots.get(place).map_or(&[], Vec::as_slice)
    }

    pub fn root_allocation_for_place(&self, place: &StoragePlace<'db>) -> Option<RootAllocation> {
        let LayoutBindingTarget::Scalar(cell) = self.root_target_for_place(place)? else {
            return None;
        };
        self.cells.get(cell.0 as usize)?.allocation
    }

    pub fn assigned_slot_for_place(&self, place: &StoragePlace<'db>) -> Option<usize> {
        self.root_allocation_for_place(place)
            .map(|allocation| allocation.slot)
    }

    pub fn assigned_root_value(
        &self,
        target: LayoutBindingTarget,
        ty: TyId<'db>,
        indices: &[usize],
    ) -> Option<AssignedRootValue<'db>> {
        match target {
            LayoutBindingTarget::Scalar(cell) => {
                let allocation = self.cells.get(cell.0 as usize)?.allocation?;
                Some(AssignedRootValue::Literal {
                    space: allocation.space,
                    slot: allocation.slot,
                    ty,
                })
            }
            LayoutBindingTarget::Indexed(family) => {
                let family = self.families.get(family.0 as usize)?;
                let allocation = family.allocation?;
                if indices.is_empty() {
                    return Some(AssignedRootValue::Indexed {
                        space: allocation.space,
                        base: allocation.slot,
                        dimensions: family.dimensions.clone(),
                        strides: family.strides.clone(),
                        ty,
                    });
                }
                Some(AssignedRootValue::Literal {
                    space: allocation.space,
                    slot: family.slot_for_indices(indices)?,
                    ty,
                })
            }
        }
    }

    pub fn root_value(
        &self,
        view: &AssignedLayoutTy<'db>,
        root: LayoutRootId<'db>,
        ty: TyId<'db>,
        selection: &LayoutSelection,
    ) -> Result<AssignedRootValue<'db>, LayoutViewError<'db>> {
        let binding = view
            .binding(root)
            .ok_or(LayoutViewError::RootNotClassified { root })?;
        let LayoutBinding::Bound(leaves) = binding else {
            return Err(LayoutViewError::NonPhysicalRoot { root });
        };
        let leaves = matching_binding_leaves(leaves, &selection.selector);
        self.root_value_from_leaves(root, ty, &selection.indices, &leaves)
    }

    /// Resolves a root from a source-level projection path. Layout-only graph edges such as
    /// provider-target and transparent-wrapper transitions are intentionally invisible to the
    /// caller but remain authoritative in the binding leaves matched here.
    pub fn root_value_for_visible_projections(
        &self,
        kind: LayoutViewKind,
        root: LayoutRootId<'db>,
        ty: TyId<'db>,
        projections: &[LayoutProjection],
    ) -> Result<AssignedRootValue<'db>, LayoutViewError<'db>> {
        let binding = self
            .root_bindings
            .get(&root)
            .ok_or(LayoutViewError::RootNotClassified { root })?;
        let LayoutBinding::Bound(leaves) = binding else {
            return Err(LayoutViewError::NonPhysicalRoot { root });
        };
        let view_selector = self.view_selector(kind);
        let leaves = leaves
            .iter()
            .filter(|leaf| {
                leaf.selector
                    .strip_prefix(view_selector.as_slice())
                    .and_then(visible_layout_projection_path)
                    .is_some_and(|candidate| {
                        visible_layout_projection_is_prefix(projections, &candidate)
                    })
            })
            .collect::<Vec<_>>();
        let mut indices = Vec::new();
        let mut all_indices_static = true;
        for projection in projections {
            match projection {
                LayoutProjection::Index(Some(index)) if all_indices_static => {
                    indices.push(*index);
                }
                LayoutProjection::Index(None) => {
                    all_indices_static = false;
                    indices.clear();
                }
                LayoutProjection::Field(_)
                | LayoutProjection::VariantField { .. }
                | LayoutProjection::Index(Some(_))
                | LayoutProjection::ConstParam(_)
                | LayoutProjection::EffectTarget => {}
            }
        }
        self.root_value_from_leaves(root, ty, &indices, &leaves)
    }

    /// Resolves a root using the complete semantic layout-view path.
    ///
    /// Unlike source-visible projections, nested effect-target transitions
    /// remain explicit so physical wrapper roots cannot satisfy target ports
    /// (or vice versa) merely because the source syntax hides a dereference.
    pub fn root_value_for_evidence_projections(
        &self,
        kind: LayoutViewKind,
        root: LayoutRootId<'db>,
        ty: TyId<'db>,
        projections: &[LayoutProjection],
    ) -> Result<AssignedRootValue<'db>, LayoutViewError<'db>> {
        let binding = self
            .root_bindings
            .get(&root)
            .ok_or(LayoutViewError::RootNotClassified { root })?;
        let LayoutBinding::Bound(leaves) = binding else {
            return Err(LayoutViewError::NonPhysicalRoot { root });
        };
        let view_selector = self.view_selector(kind);
        let leaves = leaves
            .iter()
            .filter(|leaf| {
                leaf.selector
                    .strip_prefix(view_selector.as_slice())
                    .and_then(|selector| layout_projection_path(selector, true))
                    .is_some_and(|candidate| {
                        visible_layout_projection_is_prefix(projections, &candidate)
                    })
            })
            .collect::<Vec<_>>();
        let mut indices = Vec::new();
        let mut all_indices_static = true;
        for projection in projections {
            match projection {
                LayoutProjection::Index(Some(index)) if all_indices_static => {
                    indices.push(*index);
                }
                LayoutProjection::Index(None) => {
                    all_indices_static = false;
                    indices.clear();
                }
                LayoutProjection::Field(_)
                | LayoutProjection::VariantField { .. }
                | LayoutProjection::Index(Some(_))
                | LayoutProjection::ConstParam(_)
                | LayoutProjection::EffectTarget => {}
            }
        }
        self.root_value_from_leaves(root, ty, &indices, &leaves)
    }

    /// Resolves a source projection whose semantic root remains runtime-selected.
    ///
    /// This is intentionally strict: the visible projection must identify one
    /// physical value across the complete field layout. Multiple semantic roots
    /// are accepted only when they resolve to the same assigned transport.
    pub fn unique_root_value_for_visible_projections(
        &self,
        kind: LayoutViewKind,
        ty: TyId<'db>,
        projections: &[LayoutProjection],
    ) -> Option<AssignedRootValue<'db>> {
        let values = self
            .root_bindings
            .keys()
            .filter_map(|root| {
                self.root_value_for_visible_projections(kind, *root, ty, projections)
                    .ok()
            })
            .collect::<FxHashSet<_>>();
        let values = values.into_iter().collect::<Vec<_>>();
        let [value] = values.as_slice() else {
            return None;
        };
        Some(value.clone())
    }

    pub fn unique_root_value_for_evidence_projections(
        &self,
        kind: LayoutViewKind,
        ty: TyId<'db>,
        projections: &[LayoutProjection],
    ) -> Option<AssignedRootValue<'db>> {
        let values = self
            .root_bindings
            .keys()
            .filter_map(|root| {
                self.root_value_for_evidence_projections(kind, *root, ty, projections)
                    .ok()
            })
            .collect::<FxHashSet<_>>();
        let values = values.into_iter().collect::<Vec<_>>();
        let [value] = values.as_slice() else {
            return None;
        };
        Some(value.clone())
    }

    fn root_value_from_leaves(
        &self,
        root: LayoutRootId<'db>,
        ty: TyId<'db>,
        indices: &[usize],
        leaves: &[&LayoutBindingLeaf],
    ) -> Result<AssignedRootValue<'db>, LayoutViewError<'db>> {
        let Some(target) = leaves.first().map(|leaf| leaf.target) else {
            return Err(LayoutViewError::RootNeedsLanding { root });
        };
        if leaves.iter().any(|leaf| leaf.target != target) {
            return Err(LayoutViewError::RootNeedsLanding { root });
        }
        if let LayoutBindingTarget::Indexed(family) = target {
            let family = self
                .families
                .get(family.0 as usize)
                .ok_or(LayoutViewError::MissingAllocation { root })?;
            if !indices.is_empty() && indices.len() != family.dimensions.len() {
                return Err(LayoutViewError::RootNeedsIndex { root });
            }
        }
        let value = self
            .assigned_root_value(target, ty, indices)
            .ok_or(match target {
                LayoutBindingTarget::Indexed(_) => LayoutViewError::InvalidIndex { root },
                LayoutBindingTarget::Scalar(_) => LayoutViewError::MissingAllocation { root },
            })?;
        Ok(value)
    }

    pub fn try_concrete_ty(
        &self,
        db: &'db dyn HirAnalysisDb,
        view: &AssignedLayoutTy<'db>,
        selection: &LayoutSelection,
    ) -> Result<TyId<'db>, LayoutViewError<'db>> {
        let mut error = None;
        let ty = rewrite_structural_holes(db, view.template, |hole, hole_ty| {
            if error.is_some() {
                return None;
            }
            match self.root_value(
                view,
                hole.root(db),
                layout_hole_fallback_ty(db, hole_ty),
                selection,
            ) {
                Ok(AssignedRootValue::Literal { slot, ty, .. }) => {
                    Some(slot_const_ty(db, slot, ty))
                }
                Ok(AssignedRootValue::Indexed { .. }) => {
                    error = Some(LayoutViewError::RootNeedsIndex {
                        root: hole.root(db),
                    });
                    None
                }
                Err(view_error) => {
                    error = Some(view_error);
                    None
                }
            }
        });
        error.map_or(Ok(ty), Err)
    }

    pub fn declared_concrete_ty(
        &self,
        db: &'db dyn HirAnalysisDb,
        selection: &LayoutSelection,
    ) -> Result<TyId<'db>, LayoutViewError<'db>> {
        self.try_concrete_ty(db, &self.declared, selection)
    }

    pub fn target_concrete_ty(
        &self,
        db: &'db dyn HirAnalysisDb,
        selection: &LayoutSelection,
    ) -> Result<TyId<'db>, LayoutViewError<'db>> {
        self.try_concrete_ty(db, &self.target, selection)
    }

    /// Type used to identify a whole contract-field effect binding. Scalar
    /// layouts retain their assigned literals. A whole view that intentionally
    /// requires a structural landing, runtime index, or has only non-physical
    /// roots uses its source shape together with the binding's `layout_env`.
    /// Graph/allocation failures remain errors and are never erased.
    pub fn target_effect_binding_ty(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Result<TyId<'db>, LayoutViewError<'db>> {
        match self.target_concrete_ty(db, &LayoutSelection::default()) {
            Ok(ty) => Ok(ty),
            Err(
                LayoutViewError::NonPhysicalRoot { .. }
                | LayoutViewError::RootNeedsLanding { .. }
                | LayoutViewError::RootNeedsIndex { .. },
            ) => Ok(self.target.shape_ty()),
            Err(error) => Err(error),
        }
    }

    pub fn slot_basis_concrete_ty(
        &self,
        db: &'db dyn HirAnalysisDb,
        selection: &LayoutSelection,
    ) -> Result<TyId<'db>, LayoutViewError<'db>> {
        self.try_concrete_ty(db, &self.slot_basis, selection)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ValidatedFieldLayoutPlan<'db> {
    field: ContractFieldId<'db>,
    name: IdentId<'db>,
    is_mut: bool,
    is_provider: bool,
    address_space: ProviderAddressSpace,
    inline_span: usize,
    inline_leaves: Vec<InlineLayoutLeaf<'db>>,
    declared_template: TyId<'db>,
    target_template: TyId<'db>,
    slot_basis_template: TyId<'db>,
    occurrences: Vec<RootOccurrence<'db>>,
    concrete_occurrences: Vec<ConcreteRootOccurrence<'db>>,
    cells: Vec<RootCell<'db>>,
    families: Vec<LayoutRootFamily<'db>>,
    overlay_groups: Vec<EnumOverlayGroup<'db>>,
    place_roots: IndexMap<StoragePlace<'db>, Vec<LayoutBindingTarget>>,
    bindings: IndexMap<LayoutRootId<'db>, LayoutBinding>,
    counted_lanes: Vec<AllocationLane<'db>>,
    materialize_only_lanes: Vec<AllocationLane<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct AllocatedContractStorageLayout<'db> {
    pub fields: IndexMap<IdentId<'db>, FieldStorageLayout<'db>>,
    pub explicit_reservations: Vec<ExplicitRootReservation<'db>>,
    pub high_water_by_address_space: FxHashMap<ProviderAddressSpace, usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ContractFieldLayoutResult<'db> {
    pub field: ContractFieldId<'db>,
    pub name: IdentId<'db>,
    pub result: Result<ValidatedFieldLayoutPlan<'db>, Vec<ContractLayoutError<'db>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ContractStorageLayoutResult<'db> {
    pub field_results: Vec<ContractFieldLayoutResult<'db>>,
    pub allocated: Option<AllocatedContractStorageLayout<'db>>,
}

impl<'db> ContractStorageLayoutResult<'db> {
    pub fn field(&self, name: &IdentId<'db>) -> Option<&FieldStorageLayout<'db>> {
        self.allocated.as_ref()?.fields.get(name)
    }

    pub fn field_errors(&self, name: &IdentId<'db>) -> Option<&[ContractLayoutError<'db>]> {
        self.field_results
            .iter()
            .find(|field| field.name == *name)?
            .result
            .as_ref()
            .err()
            .map(Vec::as_slice)
    }

    pub fn field_errors_for_id(
        &self,
        field: ContractFieldId<'db>,
    ) -> Option<&[ContractLayoutError<'db>]> {
        self.field_results
            .iter()
            .find(|result| result.field == field)?
            .result
            .as_ref()
            .err()
            .map(Vec::as_slice)
    }

    pub fn get(&self, name: &IdentId<'db>) -> Option<&FieldStorageLayout<'db>> {
        self.field(name)
    }

    pub fn values(&self) -> impl Iterator<Item = &FieldStorageLayout<'db>> {
        self.allocated
            .iter()
            .flat_map(|layout| layout.fields.values())
    }

    pub(crate) fn semantic_fields(
        &self,
        db: &'db dyn HirAnalysisDb,
    ) -> Vec<(
        IdentId<'db>,
        ContractFieldId<'db>,
        bool,
        bool,
        AssignedLayoutTy<'db>,
        AssignedLayoutTy<'db>,
    )> {
        if let Some(layout) = &self.allocated {
            return layout
                .fields
                .iter()
                .map(|(name, field)| {
                    (
                        *name,
                        field.field,
                        field.is_mut,
                        field.is_provider,
                        field.declared.clone(),
                        field.target.clone(),
                    )
                })
                .collect();
        }
        self.field_results
            .iter()
            .filter_map(|field| {
                let plan = field.result.as_ref().ok()?;
                Some((
                    field.name,
                    plan.field,
                    plan.is_mut,
                    plan.is_provider,
                    assigned_view(db, plan.declared_template, &plan.bindings),
                    assigned_view(db, plan.target_template, &plan.bindings),
                ))
            })
            .collect()
    }
}

fn slot_const_ty<'db>(db: &'db dyn HirAnalysisDb, value: usize, ty: TyId<'db>) -> TyId<'db> {
    let int = IntegerId::new(db, BigUint::from(value));
    TyId::const_ty(
        db,
        ConstTyId::new(
            db,
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int), ty),
        ),
    )
}

fn const_ty_to_usize<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<usize> {
    let TyData::ConstTy(const_ty) = ty.data(db) else {
        return None;
    };
    match const_ty.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int), _) => int.data(db).to_usize(),
        _ => None,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum WalkMode {
    Counted,
    MaterializeOnly,
}

impl From<WalkMode> for RootRole {
    fn from(mode: WalkMode) -> Self {
        match mode {
            WalkMode::Counted => Self::Counted,
            WalkMode::MaterializeOnly => Self::MaterializeOnly,
        }
    }
}

#[derive(Clone, Debug)]
enum WalkEvent<'db> {
    Root(RootOccurrenceId),
    Enum {
        place: StoragePlace<'db>,
        variants: Vec<Vec<WalkEvent<'db>>>,
    },
}

#[derive(Clone, Debug)]
struct WalkOutput<'db> {
    inline_span: usize,
    inline_leaves: Vec<InlineLayoutLeaf<'db>>,
    events: Vec<WalkEvent<'db>>,
}

impl<'db> WalkOutput<'db> {
    fn empty() -> Self {
        Self {
            inline_span: 0,
            inline_leaves: Vec::new(),
            events: Vec::new(),
        }
    }

    fn scalar(
        ty: TyId<'db>,
        place: StoragePlace<'db>,
        dimensions: &[LayoutIndexDimension<'db>],
    ) -> Self {
        Self {
            inline_span: 1,
            inline_leaves: vec![InlineLayoutLeaf {
                place,
                ty,
                offset: 0,
                dimensions: dimensions.to_vec(),
                strides: vec![0; dimensions.len()],
                kind: InlineLayoutLeafKind::Field,
            }],
            events: Vec::new(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
enum InlineLayoutLeafKind {
    Field,
    EnumTag,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
struct InlineLayoutLeaf<'db> {
    place: StoragePlace<'db>,
    ty: TyId<'db>,
    offset: usize,
    dimensions: Vec<LayoutIndexDimension<'db>>,
    strides: Vec<usize>,
    kind: InlineLayoutLeafKind,
}

#[derive(Clone, Debug)]
struct ConcreteApplication<'db> {
    ty: TyId<'db>,
    direct_roots: FxHashSet<LayoutRootId<'db>>,
}

#[derive(Clone, Debug)]
struct ConcreteRootSite<'db> {
    owner: TyId<'db>,
    place: StoragePlace<'db>,
    dimensions: Vec<LayoutIndexDimension<'db>>,
    mode: WalkMode,
    default_space: ProviderAddressSpace,
}

#[derive(Clone, Copy)]
struct ProviderTargetEdge<'db> {
    impl_instance: ResolvedImplInstance<'db>,
    target_template: TyId<'db>,
    space: ProviderAddressSpace,
}

#[derive(Clone, Copy)]
struct ExpandingProvider<'db> {
    ty: TyId<'db>,
    implementation: ImplementorId<'db>,
}

fn instantiate_provider_target_layout<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    parent_instance: LayoutInstantiationId<'db>,
    impl_instance: ResolvedImplInstance<'db>,
    target_template: TyId<'db>,
) -> Result<LayoutInstantiation<'db>, ContractLayoutError<'db>> {
    let boundary = LayoutBoundaryIdentity::ProviderTarget(impl_instance.selected());
    let target = instantiate_layout_template(
        db,
        target_template,
        impl_instance.impl_params(db),
        impl_instance.impl_args(db),
        LayoutInstantiationContext::Nested(parent_instance),
        boundary,
        vec![LayoutOccurrenceStep::Instantiation(0)],
    );
    let normalized_root_uses = normalize_layout_root_uses(db, target.ty, scope, assumptions);
    let normalized = normalize_ty(db, target.ty, scope, assumptions);
    // Normalization can expose structural roots owned by another associated
    // type definition. Re-land the complete equality partition under this
    // provider occurrence so every root carries the exact provider boundary.
    let mut target = instantiate_layout_template(
        db,
        normalized,
        &[],
        &[],
        LayoutInstantiationContext::Nested(target.instance),
        boundary,
        vec![LayoutOccurrenceStep::Normalization],
    );
    let target_ident = IdentId::new(db, "Target".to_string());
    for root_use in impl_instance.assoc_ty_layout_root_uses(db, target_ident) {
        let value = Binder::bind(root_use.value).instantiate(db, impl_instance.impl_args(db));
        if layout_root_id(db, value).is_some() {
            continue;
        }
        let root_use = crate::analysis::ty::layout_holes::LayoutRootUse {
            value,
            owner: root_use.owner.map(|owner| {
                normalize_ty(
                    db,
                    Binder::bind(owner).instantiate(db, impl_instance.impl_args(db)),
                    scope,
                    assumptions,
                )
            }),
            selector: root_use.selector,
            index_dimensions: root_use.index_dimensions,
        };
        if !target.root_uses.contains(&root_use) {
            target.root_uses.push(root_use);
        }
    }
    for mut root_use in normalized_root_uses {
        if layout_root_id(db, root_use.value).is_some() {
            continue;
        }
        root_use.owner = root_use.owner.or(Some(target.ty));
        if !target.root_uses.contains(&root_use) {
            target.root_uses.push(root_use);
        }
    }
    if target.ty.has_invalid(db)
        || target.ty.has_param(db)
        || target.ty.has_var(db)
        || ty_has_incomplete_adt_application(db, target.ty)
    {
        Err(ContractLayoutError::UnresolvedProviderTarget)
    } else {
        Ok(target)
    }
}

struct FieldCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    active_space: ProviderAddressSpace,
    occurrences: Vec<RootOccurrence<'db>>,
    concrete_occurrences: Vec<ConcreteRootOccurrence<'db>>,
    errors: Vec<ContractLayoutError<'db>>,
    applications: Vec<ConcreteApplication<'db>>,
    reached_concrete_sites: Vec<ConcreteRootSite<'db>>,
    visiting: FxHashSet<(TyId<'db>, StoragePlace<'db>)>,
    expanding_providers: Vec<ExpandingProvider<'db>>,
    nonterminal_occurrences: FxHashSet<RootOccurrenceId>,
}

impl<'db> FieldCollector<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        scope: crate::hir_def::scope_graph::ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        active_space: ProviderAddressSpace,
    ) -> Self {
        Self {
            db,
            scope,
            assumptions,
            active_space,
            occurrences: Vec::new(),
            concrete_occurrences: Vec::new(),
            errors: Vec::new(),
            applications: Vec::new(),
            reached_concrete_sites: Vec::new(),
            visiting: FxHashSet::default(),
            expanding_providers: Vec::new(),
            nonterminal_occurrences: FxHashSet::default(),
        }
    }

    fn push_error(&mut self, error: ContractLayoutError<'db>) {
        if self.errors.contains(&error) {
            return;
        }
        if matches!(error, ContractLayoutError::NonRegularProviderCycle) {
            self.errors.insert(0, error);
        } else {
            self.errors.push(error);
        }
    }

    fn concrete_owner(&self, root: LayoutRootId<'db>) -> Option<TyId<'db>> {
        self.applications.iter().rev().find_map(|application| {
            application
                .direct_roots
                .iter()
                .any(|candidate| layout_root_descends_from(self.db, root, *candidate))
                .then_some(application.ty)
        })
    }

    fn root_space(&mut self, root: LayoutRootId<'db>) -> Option<ProviderAddressSpace> {
        let Some(owner) = self.concrete_owner(root) else {
            return Some(self.active_space);
        };
        self.root_space_for_owner(owner, self.active_space)
    }

    fn root_space_for_owner(
        &mut self,
        owner: TyId<'db>,
        default_space: ProviderAddressSpace,
    ) -> Option<ProviderAddressSpace> {
        match resolve_static_slot_layout(self.db, self.scope, self.assumptions, owner) {
            StaticSlotLayoutResolution::NotStaticSlot => Some(default_space),
            StaticSlotLayoutResolution::Resolved(space) => Some(space),
            StaticSlotLayoutResolution::Ambiguous => {
                self.push_error(ContractLayoutError::AmbiguousStaticSlot { owner });
                None
            }
            StaticSlotLayoutResolution::UnresolvedSpace => {
                self.push_error(ContractLayoutError::UnresolvedStaticSlotSpace { owner });
                None
            }
        }
    }

    fn emit_root(
        &mut self,
        placeholder: TyId<'db>,
        place: StoragePlace<'db>,
        selector: Vec<PlaceStep>,
        dimensions: &[LayoutIndexDimension<'db>],
        mode: WalkMode,
    ) -> Option<WalkEvent<'db>> {
        if dimensions.iter().any(|dimension| dimension.len == 0) {
            return None;
        }
        let hole = structural_hole_id(self.db, placeholder)?;
        if matches!(
            hole.origin(self.db),
            StructuralHoleOrigin::ExplicitWildcard { .. }
        ) {
            self.push_error(ContractLayoutError::ExplicitContractLayoutHole { placeholder });
            return None;
        }
        let expected = layout_hole_fallback_ty(self.db, hole.expected_ty(self.db));
        if !matches!(
            expected.data(self.db),
            TyData::TyBase(TyBase::Prim(PrimTy::U256 | PrimTy::Usize))
        ) {
            self.push_error(ContractLayoutError::NonSlotContractLayoutHole { placeholder });
            return None;
        }
        let root = hole.root(self.db);
        let space = self.root_space(root)?;
        let id = RootOccurrenceId(self.occurrences.len() as u32);
        self.occurrences.push(RootOccurrence {
            id,
            root,
            placeholder,
            place,
            selector,
            index_dimensions: dimensions.to_vec(),
            role: mode.into(),
            space,
            order: id.0,
        });
        Some(WalkEvent::Root(id))
    }

    fn emit_concrete_root(
        &mut self,
        value: TyId<'db>,
        site: ConcreteRootSite<'db>,
        selector: Vec<PlaceStep>,
    ) {
        let ConcreteRootSite {
            owner,
            place,
            dimensions,
            mode,
            default_space,
        } = site;
        let canon_env = ConstCanonEnv::new(self.scope, self.assumptions, None);
        let value = canonicalize_ty_for_mode(self.db, value, canon_env, ConstCanonMode::Identity);
        let TyData::ConstTy(const_ty) = value.data(self.db) else {
            self.push_error(ContractLayoutError::InternalLayoutGraph);
            return;
        };
        let expected_ty = const_ty.ty(self.db);
        if expected_ty.has_invalid(self.db) {
            self.push_error(ContractLayoutError::InvalidFieldType);
            return;
        }
        if !matches!(
            expected_ty.data(self.db),
            TyData::TyBase(TyBase::Prim(PrimTy::U256 | PrimTy::Usize))
        ) {
            return;
        }
        let Ok(value) = value.evaluate_const_ty(self.db, Some(expected_ty)) else {
            self.push_error(ContractLayoutError::InvalidFieldType);
            return;
        };
        let value = canonicalize_ty_for_mode(self.db, value, canon_env, ConstCanonMode::Identity);
        if value.has_invalid(self.db) {
            self.push_error(ContractLayoutError::InvalidFieldType);
            return;
        }
        let TyData::ConstTy(const_ty) = value.data(self.db) else {
            self.push_error(ContractLayoutError::InternalLayoutGraph);
            return;
        };
        let ConstTyData::Evaluated(EvaluatedConstTy::LitInt(value), ty) = const_ty.data(self.db)
        else {
            self.push_error(ContractLayoutError::UnresolvedConcreteLayoutRoot { value });
            return;
        };
        if *ty != expected_ty {
            self.push_error(ContractLayoutError::InternalLayoutGraph);
            return;
        }
        let Some(space) = self.root_space_for_owner(owner, default_space) else {
            return;
        };
        if self.concrete_occurrences.iter().any(|occurrence| {
            occurrence.value == *value
                && occurrence.ty == *ty
                && occurrence.owner == owner
                && occurrence.place == place
                && occurrence.selector == selector
                && occurrence.index_dimensions == dimensions
                && occurrence.role == mode.into()
                && occurrence.space == space
        }) {
            return;
        }
        let id = ConcreteRootOccurrenceId(self.concrete_occurrences.len() as u32);
        self.concrete_occurrences.push(ConcreteRootOccurrence {
            id,
            value: *value,
            ty: *ty,
            owner,
            place,
            selector,
            index_dimensions: dimensions,
            role: mode.into(),
            space,
            order: id.0,
        });
    }

    fn emit_reached_concrete_uses(
        &mut self,
        instantiation: &LayoutInstantiation<'db>,
        reached_start: usize,
        mode: WalkMode,
    ) {
        for root_use in &instantiation.root_uses {
            if root_use.root(self.db).is_some() {
                continue;
            }
            let owner = root_use.owner.unwrap_or(instantiation.ty);
            let reached = self.reached_concrete_sites[reached_start..]
                .iter()
                .filter(|reached| reached.owner == owner && reached.mode == mode)
                .cloned()
                .collect::<Vec<_>>();
            for reached in reached {
                let mut selector = reached.place.steps.clone();
                let parameter = root_use
                    .selector
                    .iter()
                    .rev()
                    .find_map(|step| match step {
                        LayoutOccurrenceStep::GenericArg(index) => Some(*index),
                        _ => None,
                    })
                    .or_else(|| {
                        root_use.selector.iter().rev().find_map(|step| match step {
                            LayoutOccurrenceStep::ConstParam(index) => Some(*index),
                            _ => None,
                        })
                    });
                if let Some(index) = parameter
                    && !matches!(selector.last(), Some(PlaceStep::ConstParam(last)) if *last == index)
                {
                    selector.push(PlaceStep::ConstParam(index));
                }
                self.emit_concrete_root(root_use.value, reached, selector);
            }
        }
    }

    fn walk_instantiation(
        &mut self,
        instantiation: &LayoutInstantiation<'db>,
        place: StoragePlace<'db>,
        dimensions: &[LayoutIndexDimension<'db>],
        mode: WalkMode,
    ) -> WalkOutput<'db> {
        let reached_start = self.reached_concrete_sites.len();
        let output = self.walk_ty(
            instantiation.ty,
            instantiation.instance,
            place,
            dimensions,
            mode,
        );
        self.emit_reached_concrete_uses(instantiation, reached_start, mode);
        output
    }

    fn walk_provider_wrapper_instantiation(
        &mut self,
        instantiation: &LayoutInstantiation<'db>,
        place: StoragePlace<'db>,
        dimensions: &[LayoutIndexDimension<'db>],
        mode: WalkMode,
    ) -> WalkOutput<'db> {
        let reached_start = self.reached_concrete_sites.len();
        self.reached_concrete_sites.push(ConcreteRootSite {
            owner: instantiation.ty,
            place: place.clone(),
            dimensions: dimensions.to_vec(),
            mode,
            default_space: self.active_space,
        });
        let output = if self.visiting.insert((instantiation.ty, place.clone())) {
            let output = self.walk_ty_representation(
                instantiation.ty,
                instantiation.instance,
                place.clone(),
                dimensions,
                mode,
            );
            self.visiting.remove(&(instantiation.ty, place));
            output
        } else {
            WalkOutput::scalar(instantiation.ty, place, dimensions)
        };
        self.emit_reached_concrete_uses(instantiation, reached_start, mode);
        output
    }

    fn direct_root_args(
        &self,
        adt: AdtDef<'db>,
        args: &[TyId<'db>],
    ) -> FxHashSet<LayoutRootId<'db>> {
        adt.params(self.db)
            .iter()
            .enumerate()
            .filter_map(|(idx, param)| {
                matches!(param.data(self.db), TyData::ConstTy(_))
                    .then(|| args.get(idx).and_then(|arg| layout_root_id(self.db, *arg)))
                    .flatten()
            })
            .collect()
    }

    fn walk_ty(
        &mut self,
        ty: TyId<'db>,
        parent_instance: LayoutInstantiationId<'db>,
        place: StoragePlace<'db>,
        dimensions: &[LayoutIndexDimension<'db>],
        mode: WalkMode,
    ) -> WalkOutput<'db> {
        if let Some(event) =
            self.emit_root(ty, place.clone(), place.steps.clone(), dimensions, mode)
        {
            return WalkOutput {
                inline_span: 0,
                inline_leaves: Vec::new(),
                events: vec![event],
            };
        }
        self.reached_concrete_sites.push(ConcreteRootSite {
            owner: ty,
            place: place.clone(),
            dimensions: dimensions.to_vec(),
            mode,
            default_space: self.active_space,
        });
        if !self.visiting.insert((ty, place.clone())) {
            return WalkOutput::scalar(ty, place, dimensions);
        }

        let provider = ty
            .adt_def(self.db)
            .map(|_| resolve_effect_handle_layout(self.db, self.scope, self.assumptions, ty));
        let output = match provider {
            Some(ProviderLayoutResolution::Resolved {
                impl_instance,
                target_template,
                space,
            }) => self.walk_embedded_provider(
                ty,
                parent_instance,
                place.clone(),
                dimensions,
                mode,
                ProviderTargetEdge {
                    impl_instance,
                    target_template,
                    space,
                },
            ),
            Some(ProviderLayoutResolution::Ambiguous) => {
                self.push_error(ContractLayoutError::AmbiguousProviderLayout);
                WalkOutput::empty()
            }
            Some(ProviderLayoutResolution::UnresolvedTarget) => {
                self.push_error(ContractLayoutError::UnresolvedProviderTarget);
                WalkOutput::empty()
            }
            Some(ProviderLayoutResolution::UnresolvedSpace) => {
                self.push_error(ContractLayoutError::UnresolvedProviderSpace);
                WalkOutput::empty()
            }
            Some(ProviderLayoutResolution::NotHandle) | None => {
                self.walk_ty_representation(ty, parent_instance, place.clone(), dimensions, mode)
            }
        };
        self.visiting.remove(&(ty, place));
        output
    }

    fn walk_ty_representation(
        &mut self,
        ty: TyId<'db>,
        parent_instance: LayoutInstantiationId<'db>,
        place: StoragePlace<'db>,
        dimensions: &[LayoutIndexDimension<'db>],
        mode: WalkMode,
    ) -> WalkOutput<'db> {
        if let Some((_, inner)) = ty.as_capability(self.db) {
            self.walk_ty(
                inner,
                parent_instance,
                place.with_step(PlaceStep::TransparentInner),
                dimensions,
                mode,
            )
        } else if let TyData::ConstTy(const_ty) = ty.data(self.db) {
            self.walk_ty(
                const_ty.ty(self.db),
                parent_instance,
                place.clone(),
                dimensions,
                mode,
            )
        } else if ty.is_tuple(self.db) {
            self.walk_sequence(
                ty.field_types(self.db)
                    .into_iter()
                    .enumerate()
                    .map(|(idx, elem)| {
                        (
                            LayoutInstantiation {
                                ty: elem,
                                root_uses: Vec::new(),
                                instance: parent_instance,
                            },
                            place.with_step(PlaceStep::TupleElem(idx as u32)),
                        )
                    }),
                dimensions,
                mode,
            )
        } else if ty.is_array(self.db) {
            self.walk_array(ty, parent_instance, place.clone(), dimensions, mode)
        } else if let Some(adt) = ty.adt_def(self.db) {
            self.walk_adt(ty, adt, parent_instance, place.clone(), dimensions, mode)
        } else {
            let inline_span = if ty.is_never(self.db)
                || ty.is_zero_sized(self.db)
                || matches!(
                    ty.base_ty(self.db).data(self.db),
                    TyData::TyBase(TyBase::Func(_) | TyBase::Contract(_))
                ) {
                0
            } else {
                1
            };
            if inline_span == 0 {
                WalkOutput::empty()
            } else {
                WalkOutput::scalar(ty, place, dimensions)
            }
        }
    }

    fn walk_embedded_provider(
        &mut self,
        ty: TyId<'db>,
        parent_instance: LayoutInstantiationId<'db>,
        place: StoragePlace<'db>,
        dimensions: &[LayoutIndexDimension<'db>],
        mode: WalkMode,
        target_edge: ProviderTargetEdge<'db>,
    ) -> WalkOutput<'db> {
        match classify_layout_view_recurrence(
            self.db,
            ty,
            target_edge.impl_instance.selected(),
            self.expanding_providers
                .iter()
                .enumerate()
                .rev()
                .map(|(idx, frame)| (idx, frame.ty, frame.implementation)),
        ) {
            LayoutViewRecurrence::BackEdge { .. } => {
                return self.walk_ty_representation(ty, parent_instance, place, dimensions, mode);
            }
            LayoutViewRecurrence::NonRegular { .. } => {
                self.push_error(ContractLayoutError::NonRegularProviderCycle);
                return self.walk_ty_representation(ty, parent_instance, place, dimensions, mode);
            }
            LayoutViewRecurrence::Expand => {}
        }
        self.expanding_providers.push(ExpandingProvider {
            ty,
            implementation: target_edge.impl_instance.selected(),
        });

        let declared_start = self.occurrences.len();
        let mut output =
            self.walk_ty_representation(ty, parent_instance, place.clone(), dimensions, mode);
        let target = match instantiate_provider_target_layout(
            self.db,
            self.scope,
            self.assumptions,
            parent_instance,
            target_edge.impl_instance,
            target_edge.target_template,
        ) {
            Ok(target) => target,
            Err(error) => {
                self.push_error(error);
                self.expanding_providers.pop();
                return output;
            }
        };
        let target_start = self.occurrences.len();
        let enclosing_space = self.active_space;
        self.active_space = target_edge.space;
        let target_output = self.walk_instantiation(
            &target,
            place.with_step(PlaceStep::ProviderTarget),
            dimensions,
            mode,
        );
        self.active_space = enclosing_space;

        let direct_roots = ty
            .adt_def(self.db)
            .map(|adt| self.direct_root_args(adt, ty.generic_args(self.db)))
            .unwrap_or_default();
        let target_roots = self.occurrences[target_start..]
            .iter()
            .map(|occurrence| occurrence.root)
            .collect::<Vec<_>>();
        let nonterminal = self.occurrences[declared_start..target_start]
            .iter()
            .filter(|occurrence| {
                direct_roots.contains(&occurrence.root)
                    && target_roots
                        .iter()
                        .any(|root| layout_root_descends_from(self.db, *root, occurrence.root))
            })
            .map(|occurrence| occurrence.id)
            .collect::<Vec<_>>();
        self.nonterminal_occurrences.extend(nonterminal);
        output.events.extend(target_output.events);
        self.expanding_providers.pop();
        output
    }

    fn walk_sequence(
        &mut self,
        items: impl IntoIterator<Item = (LayoutInstantiation<'db>, StoragePlace<'db>)>,
        dimensions: &[LayoutIndexDimension<'db>],
        mode: WalkMode,
    ) -> WalkOutput<'db> {
        let mut inline_span = 0usize;
        let mut inline_leaves = Vec::new();
        let mut events = Vec::new();
        for (instantiation, place) in items {
            let mut output = self.walk_instantiation(&instantiation, place, dimensions, mode);
            let Some(next) = inline_span.checked_add(output.inline_span) else {
                self.push_error(ContractLayoutError::LayoutExtentOverflow);
                continue;
            };
            for leaf in &mut output.inline_leaves {
                let Some(offset) = leaf.offset.checked_add(inline_span) else {
                    self.push_error(ContractLayoutError::LayoutExtentOverflow);
                    continue;
                };
                leaf.offset = offset;
            }
            inline_span = next;
            inline_leaves.extend(output.inline_leaves);
            events.extend(output.events);
        }
        WalkOutput {
            inline_span,
            inline_leaves,
            events,
        }
    }

    fn walk_array(
        &mut self,
        ty: TyId<'db>,
        parent_instance: LayoutInstantiationId<'db>,
        place: StoragePlace<'db>,
        dimensions: &[LayoutIndexDimension<'db>],
        mode: WalkMode,
    ) -> WalkOutput<'db> {
        let (_, args) = ty.decompose_ty_app(self.db);
        let Some(&element) = args.first() else {
            self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
            return WalkOutput::empty();
        };
        let Some(len) = args
            .get(1)
            .copied()
            .and_then(|len| const_ty_to_usize(self.db, len))
        else {
            if contains_layout_roots(self.db, element) {
                self.push_error(ContractLayoutError::UnknownArrayLengthWithLayoutRoots {
                    array: ty,
                });
            } else {
                self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
            }
            return WalkOutput::empty();
        };
        if len == 0 {
            return WalkOutput::empty();
        }
        let element = instantiate_layout_template(
            self.db,
            element,
            &[],
            &[],
            LayoutInstantiationContext::Nested(parent_instance),
            LayoutBoundaryIdentity::ArrayElement,
            vec![LayoutOccurrenceStep::ArrayDimension(dimensions.len() as u32)],
        );
        let mut element_dimensions = dimensions.to_vec();
        element_dimensions.push(LayoutIndexDimension {
            instance: element.instance,
            len,
        });
        let mut output = self.walk_ty(
            element.ty,
            element.instance,
            place.with_step(PlaceStep::ArrayElem(0)),
            &element_dimensions,
            mode,
        );
        let Some(inline_span) = output.inline_span.checked_mul(len) else {
            self.push_error(ContractLayoutError::LayoutExtentOverflow);
            return WalkOutput {
                inline_span: 0,
                inline_leaves: output.inline_leaves,
                events: output.events,
            };
        };
        for leaf in &mut output.inline_leaves {
            leaf.strides[dimensions.len()] = output.inline_span;
        }
        WalkOutput {
            inline_span,
            inline_leaves: output.inline_leaves,
            events: output.events,
        }
    }

    fn walk_adt(
        &mut self,
        ty: TyId<'db>,
        adt: AdtDef<'db>,
        parent_instance: LayoutInstantiationId<'db>,
        place: StoragePlace<'db>,
        dimensions: &[LayoutIndexDimension<'db>],
        mode: WalkMode,
    ) -> WalkOutput<'db> {
        if adt.recursive_cycle(self.db).is_some() {
            self.push_error(ContractLayoutError::InvalidFieldType);
            return WalkOutput::empty();
        }
        let args = ty.generic_args(self.db);
        if args.len() != adt.params(self.db).len() {
            self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
            return WalkOutput::empty();
        }
        let direct_roots = self.direct_root_args(adt, args);
        self.applications
            .push(ConcreteApplication { ty, direct_roots });
        let mut direct_events = Vec::new();
        for (idx, param) in adt.params(self.db).iter().enumerate() {
            if !matches!(param.data(self.db), TyData::ConstTy(_)) {
                continue;
            }
            if let Some(&arg) = args.get(idx)
                && let Some(event) = self.emit_root(
                    arg,
                    place.clone(),
                    place.with_step(PlaceStep::ConstParam(idx as u32)).steps,
                    dimensions,
                    mode,
                )
            {
                direct_events.push(event);
            } else if adt
                .param_set(self.db)
                .const_param_default_is_layout_hole(self.db, idx)
                && let Some(&arg) = args.get(idx)
                && structural_hole_id(self.db, arg).is_none()
            {
                self.emit_concrete_root(
                    arg,
                    ConcreteRootSite {
                        owner: ty,
                        place: place.clone(),
                        dimensions: dimensions.to_vec(),
                        mode,
                        default_space: self.active_space,
                    },
                    place.with_step(PlaceStep::ConstParam(idx as u32)).steps,
                );
            }
        }
        let direct_ids = direct_events
            .iter()
            .filter_map(|event| match event {
                WalkEvent::Root(id) => Some(*id),
                WalkEvent::Enum { .. } => None,
            })
            .collect::<Vec<_>>();
        let child_occurrence_start = self.occurrences.len();

        let output = match adt.adt_ref(self.db) {
            AdtRef::Struct(_) => {
                let fields = &adt.fields(self.db)[0];
                let mut items = Vec::with_capacity(fields.num_types());
                for field_idx in 0..fields.num_types() {
                    let inst = instantiate_adt_field_layout(
                        self.db,
                        adt,
                        0,
                        field_idx,
                        args,
                        parent_instance,
                        vec![LayoutOccurrenceStep::StructField(field_idx as u32)],
                    );
                    let field_place = place.with_step(PlaceStep::StructField(field_idx as u32));
                    items.push((inst, field_place));
                }
                let mut output = self.walk_sequence(items, dimensions, mode);
                direct_events.append(&mut output.events);
                output.events = direct_events;
                output
            }
            AdtRef::Enum(_) => {
                let mut variants = Vec::new();
                let mut inline_leaves = vec![InlineLayoutLeaf {
                    place: place.clone(),
                    ty,
                    offset: 0,
                    dimensions: dimensions.to_vec(),
                    strides: vec![0; dimensions.len()],
                    kind: InlineLayoutLeafKind::EnumTag,
                }];
                let mut max_payload = 0usize;
                for (variant_idx, variant) in adt.fields(self.db).iter().enumerate() {
                    let variant_place = place.with_step(PlaceStep::EnumVariant(variant_idx as u32));
                    let mut items = Vec::with_capacity(variant.num_types());
                    for field_idx in 0..variant.num_types() {
                        let inst = instantiate_adt_field_layout(
                            self.db,
                            adt,
                            variant_idx,
                            field_idx,
                            args,
                            parent_instance,
                            vec![
                                LayoutOccurrenceStep::EnumVariant(variant_idx as u32),
                                LayoutOccurrenceStep::EnumPayloadField(field_idx as u32),
                            ],
                        );
                        let field_place =
                            variant_place.with_step(PlaceStep::EnumPayloadField(field_idx as u32));
                        items.push((inst, field_place));
                    }
                    let mut output = self.walk_sequence(items, dimensions, mode);
                    max_payload = max_payload.max(output.inline_span);
                    for leaf in &mut output.inline_leaves {
                        let Some(offset) = leaf.offset.checked_add(1) else {
                            self.push_error(ContractLayoutError::LayoutExtentOverflow);
                            continue;
                        };
                        leaf.offset = offset;
                    }
                    inline_leaves.extend(output.inline_leaves);
                    variants.push(output.events);
                }
                let inline_span = max_payload.checked_add(1).unwrap_or_else(|| {
                    self.push_error(ContractLayoutError::LayoutExtentOverflow);
                    0
                });
                direct_events.push(WalkEvent::Enum { place, variants });
                WalkOutput {
                    inline_span,
                    inline_leaves,
                    events: direct_events,
                }
            }
        };
        for direct_id in direct_ids {
            let direct_root = self.occurrences[direct_id.0 as usize].root;
            if self
                .occurrences
                .iter()
                .skip(child_occurrence_start)
                .any(|occurrence| layout_root_descends_from(self.db, occurrence.root, direct_root))
            {
                self.nonterminal_occurrences.insert(direct_id);
            }
        }
        self.applications.pop();
        output
    }
}

fn contains_layout_roots<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    !crate::analysis::ty::layout_holes::collect_unique_structural_holes_in_order(db, ty).is_empty()
}

fn ty_has_incomplete_adt_application<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    fn inner<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        visiting: &mut FxHashSet<TyId<'db>>,
    ) -> bool {
        if !visiting.insert(ty) {
            return false;
        }
        let (base, args) = ty.decompose_ty_app(db);
        let incomplete = if let TyData::TyBase(TyBase::Adt(adt)) = base.data(db) {
            args.len() != adt.params(db).len()
        } else {
            false
        } || args.iter().any(|arg| inner(db, *arg, visiting));
        visiting.remove(&ty);
        incomplete
    }
    inner(db, ty, &mut FxHashSet::default())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FamilyKey<'db> {
    root: LayoutRootId<'db>,
    dimensions: Vec<LayoutInstantiationId<'db>>,
}

fn row_major_family_geometry(
    dimensions: &[LayoutIndexDimension<'_>],
) -> Option<(usize, Vec<usize>)> {
    let mut extent = 1usize;
    for dimension in dimensions {
        extent = extent.checked_mul(dimension.len)?;
    }
    let mut stride = 1usize;
    let mut reversed = Vec::with_capacity(dimensions.len());
    for dimension in dimensions.iter().rev() {
        reversed.push(stride);
        stride = stride.checked_mul(dimension.len)?;
    }
    reversed.reverse();
    Some((extent, reversed))
}

fn affine_family_extent(
    dimensions: &[LayoutIndexDimension<'_>],
    strides: &[usize],
) -> Option<usize> {
    if dimensions.len() != strides.len() {
        return None;
    }
    dimensions
        .iter()
        .zip(strides)
        .try_fold(0usize, |offset, (dimension, stride)| {
            offset.checked_add(dimension.len.checked_sub(1)?.checked_mul(*stride)?)
        })?
        .checked_add(1)
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FamilyGeometry {
    strides: Vec<usize>,
    extent: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FamilyGeometryError {
    Overflow,
    InvalidFamily(LayoutRootFamilyId),
    InvalidOverlay(usize),
}

fn unit_occurrence_at_overlay<'a, 'db>(
    occurrences: &'a [RootOccurrence<'db>],
    cells: &[RootCell<'db>],
    families: &[LayoutRootFamily<'db>],
    unit: AllocationUnitId,
    enum_place: &StoragePlace<'db>,
) -> Option<&'a RootOccurrence<'db>> {
    allocation_unit_occurrences(cells, families, unit)?
        .iter()
        .filter_map(|occurrence| occurrences.get(occurrence.0 as usize))
        .find(|occurrence| {
            occurrence.place.steps.starts_with(&enum_place.steps)
                && matches!(
                    occurrence.place.steps.get(enum_place.steps.len()),
                    Some(PlaceStep::EnumVariant(_))
                )
        })
}

fn derive_family_geometry<'db>(
    occurrences: &[RootOccurrence<'db>],
    cells: &[RootCell<'db>],
    families: &[LayoutRootFamily<'db>],
    overlay_groups: &[EnumOverlayGroup<'db>],
) -> Result<(Vec<FamilyGeometry>, Vec<usize>), FamilyGeometryError> {
    let mut geometry = families
        .iter()
        .map(|family| {
            let (extent, strides) = row_major_family_geometry(&family.dimensions)
                .ok_or(FamilyGeometryError::Overflow)?;
            Ok(FamilyGeometry { strides, extent })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut group_indices = (0..overlay_groups.len()).collect::<Vec<_>>();
    group_indices.sort_by_key(|idx| std::cmp::Reverse(overlay_groups[*idx].enum_place.steps.len()));

    for group_idx in group_indices {
        let group = &overlay_groups[group_idx];
        let prefix_len = group
            .enum_place
            .steps
            .iter()
            .filter(|step| matches!(step, PlaceStep::ArrayElem(_)))
            .count();
        let mut prefix = None;
        let mut block_extent = 0usize;
        for member in &group.members {
            let occurrence = unit_occurrence_at_overlay(
                occurrences,
                cells,
                families,
                *member,
                &group.enum_place,
            )
            .ok_or(FamilyGeometryError::InvalidOverlay(group_idx))?;
            if occurrence.index_dimensions.len() < prefix_len
                || prefix.as_ref().is_some_and(|prefix: &Vec<_>| {
                    prefix.as_slice() != &occurrence.index_dimensions[..prefix_len]
                })
            {
                return Err(FamilyGeometryError::InvalidOverlay(group_idx));
            }
            prefix.get_or_insert_with(|| occurrence.index_dimensions[..prefix_len].to_vec());
            let member_extent = match member {
                AllocationUnitId::Scalar(_) => {
                    if occurrence
                        .index_dimensions
                        .iter()
                        .any(|dimension| dimension.len != 1)
                    {
                        return Err(FamilyGeometryError::InvalidOverlay(group_idx));
                    }
                    1
                }
                AllocationUnitId::Indexed(family) => {
                    let family_data = families
                        .get(family.0 as usize)
                        .ok_or(FamilyGeometryError::InvalidFamily(*family))?;
                    let family_geometry = geometry
                        .get(family.0 as usize)
                        .ok_or(FamilyGeometryError::InvalidFamily(*family))?;
                    if family_data.dimensions.get(..prefix_len)
                        != occurrence.index_dimensions.get(..prefix_len)
                    {
                        return Err(FamilyGeometryError::InvalidOverlay(group_idx));
                    }
                    affine_family_extent(
                        &family_data.dimensions[prefix_len..],
                        &family_geometry.strides[prefix_len..],
                    )
                    .ok_or(FamilyGeometryError::Overflow)?
                }
            };
            block_extent = block_extent.max(member_extent);
        }
        if block_extent == 0 {
            return Err(FamilyGeometryError::InvalidOverlay(group_idx));
        }
        for member in &group.members {
            let AllocationUnitId::Indexed(family) = member else {
                continue;
            };
            let family_data = families
                .get(family.0 as usize)
                .ok_or(FamilyGeometryError::InvalidFamily(*family))?;
            let family_geometry = geometry
                .get_mut(family.0 as usize)
                .ok_or(FamilyGeometryError::InvalidFamily(*family))?;
            let mut stride = block_extent;
            for dimension_idx in (0..prefix_len).rev() {
                family_geometry.strides[dimension_idx] =
                    family_geometry.strides[dimension_idx].max(stride);
                stride = family_geometry.strides[dimension_idx]
                    .checked_mul(family_data.dimensions[dimension_idx].len)
                    .ok_or(FamilyGeometryError::Overflow)?;
            }
            family_geometry.extent =
                affine_family_extent(&family_data.dimensions, &family_geometry.strides)
                    .ok_or(FamilyGeometryError::Overflow)?;
        }
    }

    let group_extents = overlay_groups
        .iter()
        .enumerate()
        .map(|(group_idx, group)| {
            group
                .members
                .iter()
                .try_fold(0usize, |extent, member| {
                    let member_extent = match member {
                        AllocationUnitId::Scalar(_) => 1,
                        AllocationUnitId::Indexed(family) => {
                            geometry
                                .get(family.0 as usize)
                                .ok_or(FamilyGeometryError::InvalidFamily(*family))?
                                .extent
                        }
                    };
                    Ok(extent.max(member_extent))
                })
                .and_then(|extent| {
                    (extent != 0)
                        .then_some(extent)
                        .ok_or(FamilyGeometryError::InvalidOverlay(group_idx))
                })
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok((geometry, group_extents))
}

fn collect_event_units<'db>(
    events: &[WalkEvent<'db>],
    occurrence_units: &[Option<AllocationUnitId>],
    seen: &mut FxHashSet<AllocationUnitId>,
    units: &mut Vec<AllocationUnitId>,
) {
    for event in events {
        match event {
            WalkEvent::Root(occurrence) => {
                if let Some(unit) = occurrence_units
                    .get(occurrence.0 as usize)
                    .copied()
                    .flatten()
                    && seen.insert(unit)
                {
                    units.push(unit);
                }
            }
            WalkEvent::Enum { variants, .. } => {
                for variant in variants {
                    collect_event_units(variant, occurrence_units, seen, units);
                }
            }
        }
    }
}

fn mutually_exclusive_enum_place<'db>(
    first: &StoragePlace<'db>,
    second: &StoragePlace<'db>,
) -> Option<StoragePlace<'db>> {
    if first.field != second.field {
        return None;
    }
    let mut common = Vec::new();
    for (first_step, second_step) in first.steps.iter().zip(&second.steps) {
        if first_step == second_step {
            common.push(*first_step);
            continue;
        }
        return matches!(
            (first_step, second_step),
            (PlaceStep::EnumVariant(first), PlaceStep::EnumVariant(second)) if first != second
        )
        .then_some(StoragePlace {
            field: first.field,
            steps: common,
        });
    }
    None
}

fn allocation_unit_occurrences<'a, 'db>(
    cells: &'a [RootCell<'db>],
    families: &'a [LayoutRootFamily<'db>],
    unit: AllocationUnitId,
) -> Option<&'a [RootOccurrenceId]> {
    match unit {
        AllocationUnitId::Scalar(cell) => cells
            .get(cell.0 as usize)
            .map(|cell| cell.occurrences.as_slice()),
        AllocationUnitId::Indexed(family) => families
            .get(family.0 as usize)
            .map(|family| family.occurrences.as_slice()),
    }
}

/// Returns every enum place that proves at least one occurrence pair mutually
/// exclusive, but only when *all* occurrence pairs are mutually exclusive.
/// Identity-equal roots can occur in several variants, so checking one pair or
/// one positional enum lane is not sufficient to prove an allocation overlay.
fn allocation_unit_overlay_witnesses<'db>(
    occurrences: &[RootOccurrence<'db>],
    cells: &[RootCell<'db>],
    families: &[LayoutRootFamily<'db>],
    first: AllocationUnitId,
    second: AllocationUnitId,
) -> Option<Vec<StoragePlace<'db>>> {
    let first_occurrences = allocation_unit_occurrences(cells, families, first)?;
    let second_occurrences = allocation_unit_occurrences(cells, families, second)?;
    let mut witnesses = Vec::new();
    for first in first_occurrences {
        let first = occurrences.get(first.0 as usize)?;
        for second in second_occurrences {
            let second = occurrences.get(second.0 as usize)?;
            let witness = mutually_exclusive_enum_place(&first.place, &second.place)?;
            if !witnesses.contains(&witness) {
                witnesses.push(witness);
            }
        }
    }
    (!witnesses.is_empty()).then_some(witnesses)
}

fn allocation_unit_space<'db>(
    cells: &[RootCell<'db>],
    families: &[LayoutRootFamily<'db>],
    unit: AllocationUnitId,
) -> Option<ProviderAddressSpace> {
    match unit {
        AllocationUnitId::Scalar(cell) => cells.get(cell.0 as usize).map(|cell| cell.space),
        AllocationUnitId::Indexed(family) => {
            families.get(family.0 as usize).map(|family| family.space)
        }
    }
}

fn allocation_unit_role<'db>(
    cells: &[RootCell<'db>],
    families: &[LayoutRootFamily<'db>],
    unit: AllocationUnitId,
) -> Option<RootRole> {
    match unit {
        AllocationUnitId::Scalar(cell) => cells.get(cell.0 as usize).map(|cell| cell.role),
        AllocationUnitId::Indexed(family) => {
            families.get(family.0 as usize).map(|family| family.role)
        }
    }
}

fn finalize_lanes<'db>(
    events: &[WalkEvent<'db>],
    occurrence_units: &[Option<AllocationUnitId>],
    occurrences: &[RootOccurrence<'db>],
    cells: &[RootCell<'db>],
    families: &[LayoutRootFamily<'db>],
    role: RootRole,
) -> Vec<AllocationLane<'db>> {
    let mut units = Vec::new();
    collect_event_units(
        events,
        occurrence_units,
        &mut FxHashSet::default(),
        &mut units,
    );
    let mut lanes = Vec::<AllocationLane<'db>>::new();
    for unit in units {
        if allocation_unit_role(cells, families, unit) != Some(role) {
            continue;
        }
        let Some(space) = allocation_unit_space(cells, families, unit) else {
            continue;
        };
        if let Some(lane) = lanes.iter_mut().find(|lane| {
            lane.space == space
                && lane.members.iter().all(|member| {
                    allocation_unit_overlay_witnesses(occurrences, cells, families, *member, unit)
                        .is_some()
                })
        }) {
            lane.members.push(unit);
        } else {
            lanes.push(AllocationLane {
                members: vec![unit],
                overlays: Vec::new(),
                space,
            });
        }
    }
    for (lane_idx, lane) in lanes.iter_mut().enumerate() {
        let mut overlay_members: IndexMap<StoragePlace<'db>, Vec<AllocationUnitId>> =
            IndexMap::new();
        for (idx, first) in lane.members.iter().enumerate() {
            for second in lane.members.iter().skip(idx + 1) {
                let witnesses = allocation_unit_overlay_witnesses(
                    occurrences,
                    cells,
                    families,
                    *first,
                    *second,
                )
                .expect("one allocation lane must contain only mutually exclusive units");
                for witness in witnesses {
                    let members = overlay_members.entry(witness).or_default();
                    for member in [*first, *second] {
                        if !members.contains(&member) {
                            members.push(member);
                        }
                    }
                }
            }
        }
        lane.overlays = overlay_members
            .into_iter()
            .map(|(place, members)| AllocationOverlay {
                place,
                lane: lane_idx as u32,
                members,
            })
            .collect();
    }
    lanes
}

fn assigned_view<'db>(
    db: &'db dyn HirAnalysisDb,
    template: TyId<'db>,
    bindings: &IndexMap<LayoutRootId<'db>, LayoutBinding>,
) -> AssignedLayoutTy<'db> {
    let mut view_bindings = IndexMap::new();
    for hole in
        crate::analysis::ty::layout_holes::collect_unique_structural_holes_in_order(db, template)
    {
        let root = hole.root(db);
        if let Some(binding) = bindings.get(&root) {
            view_bindings.insert(root, binding.clone());
        }
    }
    AssignedLayoutTy {
        template,
        bindings: view_bindings,
    }
}

fn remap_walk_events<'db>(
    events: Vec<WalkEvent<'db>>,
    occurrence_map: &[Option<RootOccurrenceId>],
) -> Vec<WalkEvent<'db>> {
    events
        .into_iter()
        .filter_map(|event| match event {
            WalkEvent::Root(id) => occurrence_map
                .get(id.0 as usize)
                .copied()
                .flatten()
                .map(WalkEvent::Root),
            WalkEvent::Enum { place, variants } => Some(WalkEvent::Enum {
                place,
                variants: variants
                    .into_iter()
                    .map(|variant| remap_walk_events(variant, occurrence_map))
                    .collect(),
            }),
        })
        .collect()
}

struct CollectedFieldLayoutPlan<'db> {
    field: ContractFieldId<'db>,
    name: IdentId<'db>,
    is_mut: bool,
    is_provider: bool,
    address_space: ProviderAddressSpace,
    inline_span: usize,
    inline_leaves: Vec<InlineLayoutLeaf<'db>>,
    declared_template: TyId<'db>,
    target_template: TyId<'db>,
    slot_basis_template: TyId<'db>,
    view_roots: Vec<LayoutRootId<'db>>,
    occurrences: Vec<RootOccurrence<'db>>,
    concrete_occurrences: Vec<ConcreteRootOccurrence<'db>>,
    nonterminal_occurrences: FxHashSet<RootOccurrenceId>,
    counted_events: Vec<WalkEvent<'db>>,
    materialize_events: Vec<WalkEvent<'db>>,
}

fn finish_field_plan<'db>(
    db: &'db dyn HirAnalysisDb,
    collected: CollectedFieldLayoutPlan<'db>,
) -> Result<ValidatedFieldLayoutPlan<'db>, Vec<ContractLayoutError<'db>>> {
    let CollectedFieldLayoutPlan {
        field,
        name,
        is_mut,
        is_provider,
        address_space,
        inline_span,
        inline_leaves,
        declared_template,
        target_template,
        slot_basis_template,
        view_roots,
        occurrences,
        concrete_occurrences,
        nonterminal_occurrences,
        counted_events,
        materialize_events,
    } = collected;
    let counted_roots = occurrences
        .iter()
        .filter(|occurrence| {
            occurrence.role == RootRole::Counted
                && !nonterminal_occurrences.contains(&occurrence.id)
        })
        .map(|occurrence| occurrence.root)
        .collect::<Vec<_>>();
    let active = occurrences
        .iter()
        .map(|occurrence| {
            !nonterminal_occurrences.contains(&occurrence.id)
                && (occurrence.role == RootRole::Counted
                    || !counted_roots
                        .iter()
                        .any(|root| layout_root_descends_from(db, *root, occurrence.root)))
        })
        .collect::<Vec<_>>();
    let mut occurrence_map = vec![None; occurrences.len()];
    let mut next_occurrence = 0u32;
    let occurrences = occurrences
        .into_iter()
        .filter(|occurrence| active[occurrence.id.0 as usize])
        .map(|mut occurrence| {
            let old = occurrence.id;
            let id = RootOccurrenceId(next_occurrence);
            next_occurrence += 1;
            occurrence.id = id;
            occurrence.order = id.0;
            occurrence_map[old.0 as usize] = Some(id);
            occurrence
        })
        .collect::<Vec<_>>();
    let counted_events = remap_walk_events(counted_events, &occurrence_map);
    let materialize_events = remap_walk_events(materialize_events, &occurrence_map);

    let mut cells = Vec::<RootCell<'db>>::new();
    let mut families = Vec::<LayoutRootFamily<'db>>::new();
    let mut cell_by_root = FxHashMap::<LayoutRootId<'db>, RootCellId>::default();
    let mut family_by_key = FxHashMap::<FamilyKey<'db>, LayoutRootFamilyId>::default();
    let mut occurrence_units = vec![None; occurrences.len()];
    let mut place_roots: IndexMap<StoragePlace<'db>, Vec<LayoutBindingTarget>> = IndexMap::new();
    let mut expected_by_root = FxHashMap::default();
    let mut errors = Vec::new();

    for occurrence in &occurrences {
        let expected = structural_hole_id(db, occurrence.placeholder)
            .map(|hole| layout_hole_fallback_ty(db, hole.expected_ty(db)))
            .expect("root occurrences must retain their structural placeholder");
        if let Some(previous) = expected_by_root.insert(occurrence.root, expected)
            && previous != expected
        {
            errors.push(ContractLayoutError::InconsistentLayoutRootType {
                root: occurrence.root,
            });
        }
        let is_family = occurrence
            .index_dimensions
            .iter()
            .any(|dimension| dimension.len > 1);
        let unit = if is_family {
            let key = FamilyKey {
                root: occurrence.root,
                dimensions: occurrence
                    .index_dimensions
                    .iter()
                    .map(|dimension| dimension.instance)
                    .collect(),
            };
            if let Some(id) = family_by_key.get(&key).copied() {
                let family = &mut families[id.0 as usize];
                if family.space != occurrence.space {
                    errors.push(ContractLayoutError::ConflictingLayoutRootSpaces {
                        root: occurrence.root,
                    });
                }
                family.role = family.role.join(occurrence.role);
                family.occurrences.push(occurrence.id);
                AllocationUnitId::Indexed(id)
            } else {
                let id = LayoutRootFamilyId(families.len() as u32);
                families.push(LayoutRootFamily {
                    id,
                    lane: occurrence.root,
                    dimensions: occurrence.index_dimensions.clone(),
                    strides: Vec::new(),
                    extent: 0,
                    occurrences: vec![occurrence.id],
                    role: occurrence.role,
                    space: occurrence.space,
                    allocation: None,
                });
                family_by_key.insert(key, id);
                AllocationUnitId::Indexed(id)
            }
        } else if let Some(id) = cell_by_root.get(&occurrence.root).copied() {
            let cell = &mut cells[id.0 as usize];
            if cell.space != occurrence.space {
                errors.push(ContractLayoutError::ConflictingLayoutRootSpaces {
                    root: occurrence.root,
                });
            }
            cell.role = cell.role.join(occurrence.role);
            cell.occurrences.push(occurrence.id);
            AllocationUnitId::Scalar(id)
        } else {
            let id = RootCellId(cells.len() as u32);
            cells.push(RootCell {
                id,
                root: occurrence.root,
                occurrences: vec![occurrence.id],
                role: occurrence.role,
                space: occurrence.space,
                allocation: None,
            });
            cell_by_root.insert(occurrence.root, id);
            AllocationUnitId::Scalar(id)
        };
        occurrence_units[occurrence.id.0 as usize] = Some(unit);
        let target = match unit {
            AllocationUnitId::Scalar(id) => LayoutBindingTarget::Scalar(id),
            AllocationUnitId::Indexed(id) => LayoutBindingTarget::Indexed(id),
        };
        let place_targets = place_roots.entry(occurrence.place.clone()).or_default();
        if !place_targets.contains(&target) {
            place_targets.push(target);
        }
    }

    let counted_lanes = finalize_lanes(
        &counted_events,
        &occurrence_units,
        &occurrences,
        &cells,
        &families,
        RootRole::Counted,
    );
    let materialize_only_lanes = finalize_lanes(
        &materialize_events,
        &occurrence_units,
        &occurrences,
        &cells,
        &families,
        RootRole::MaterializeOnly,
    );

    let mut bound_leaves: IndexMap<LayoutRootId<'db>, Vec<LayoutBindingLeaf>> = IndexMap::new();
    for occurrence in &occurrences {
        let Some(unit) = occurrence_units[occurrence.id.0 as usize] else {
            continue;
        };
        let target = match unit {
            AllocationUnitId::Scalar(id) => LayoutBindingTarget::Scalar(id),
            AllocationUnitId::Indexed(id) => LayoutBindingTarget::Indexed(id),
        };
        let leaf = LayoutBindingLeaf {
            selector: occurrence.selector.clone(),
            target,
        };
        for root in layout_root_lineage(db, occurrence.root) {
            let leaves = bound_leaves.entry(root).or_default();
            if let Some(previous) = leaves
                .iter()
                .find(|candidate| candidate.selector == leaf.selector)
                && previous.target != leaf.target
            {
                errors.push(ContractLayoutError::AmbiguousLayoutBindingSelector { root });
            } else if !leaves.contains(&leaf) {
                leaves.push(leaf.clone());
            }
        }
    }

    let mut bindings = bound_leaves
        .into_iter()
        .map(|(root, leaves)| (root, LayoutBinding::Bound(leaves)))
        .collect::<IndexMap<_, _>>();
    for root in view_roots {
        bindings.entry(root).or_insert(LayoutBinding::NonPhysical);
    }

    if !errors.is_empty() {
        return Err(errors);
    }
    let mut overlay_groups = Vec::new();
    for lane in counted_lanes.iter().chain(&materialize_only_lanes) {
        for overlay in &lane.overlays {
            let group = EnumOverlayGroup {
                enum_place: overlay.place.clone(),
                lane: overlay.lane,
                members: overlay.members.clone(),
                space: lane.space,
                reserved_extent: 0,
            };
            if !overlay_groups.contains(&group) {
                overlay_groups.push(group);
            }
        }
    }
    let (geometry, group_extents) =
        match derive_family_geometry(&occurrences, &cells, &families, &overlay_groups) {
            Ok(geometry) => geometry,
            Err(FamilyGeometryError::Overflow) => {
                return Err(vec![ContractLayoutError::LayoutExtentOverflow]);
            }
            Err(FamilyGeometryError::InvalidFamily(_) | FamilyGeometryError::InvalidOverlay(_)) => {
                return Err(vec![ContractLayoutError::InternalLayoutGraph]);
            }
        };
    for (family, geometry) in families.iter_mut().zip(geometry) {
        family.strides = geometry.strides;
        family.extent = geometry.extent;
    }
    for (group, extent) in overlay_groups.iter_mut().zip(group_extents) {
        group.reserved_extent = extent;
    }

    Ok(ValidatedFieldLayoutPlan {
        field,
        name,
        is_mut,
        is_provider,
        address_space,
        inline_span,
        inline_leaves,
        declared_template,
        target_template,
        slot_basis_template,
        occurrences,
        concrete_occurrences,
        cells,
        families,
        overlay_groups,
        place_roots,
        bindings,
        counted_lanes,
        materialize_only_lanes,
    })
}

fn validate_template_roots<'db>(
    db: &'db dyn HirAnalysisDb,
    template: TyId<'db>,
    errors: &mut Vec<ContractLayoutError<'db>>,
) {
    for hole in
        crate::analysis::ty::layout_holes::collect_unique_structural_holes_in_order(db, template)
    {
        let placeholder = TyId::const_ty(
            db,
            ConstTyId::hole_with_id(
                db,
                hole.expected_ty(db),
                crate::analysis::ty::const_ty::HoleId::Structural(hole),
            ),
        );
        let error = if matches!(
            hole.origin(db),
            StructuralHoleOrigin::ExplicitWildcard { .. }
        ) {
            Some(ContractLayoutError::ExplicitContractLayoutHole { placeholder })
        } else if !matches!(
            layout_hole_fallback_ty(db, hole.expected_ty(db)).data(db),
            TyData::TyBase(TyBase::Prim(PrimTy::U256 | PrimTy::Usize))
        ) {
            Some(ContractLayoutError::NonSlotContractLayoutHole { placeholder })
        } else {
            None
        };
        if let Some(error) = error
            && !errors.contains(&error)
        {
            errors.push(error);
        }
    }
}

fn collect_field_plan<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    field_index: u32,
) -> Option<(
    IdentId<'db>,
    Result<ValidatedFieldLayoutPlan<'db>, Vec<ContractLayoutError<'db>>>,
)> {
    let scope = contract.scope();
    let assumptions = PredicateListId::empty_list(db);
    let field = contract
        .hir_fields(db)
        .data(db)
        .iter()
        .filter(|field| field.name.is_present())
        .nth(field_index as usize)?;
    let name = field.name.unwrap();
    let field_id = ContractFieldId {
        contract,
        index: field_index,
    };
    let default_space = if field.is_mut {
        ProviderAddressSpace::Storage
    } else {
        ProviderAddressSpace::Code
    };
    let lowered = lower_opt_hir_ty(db, field.type_ref(), scope, assumptions);
    if lowered.has_invalid(db) || ty_has_incomplete_adt_application(db, lowered) {
        return Some((name, Err(vec![ContractLayoutError::InvalidFieldType])));
    }
    let hir_ty = field.type_ref().to_opt()?;
    let mut declared = instantiate_layout_template(
        db,
        lowered,
        &[],
        &[],
        LayoutInstantiationContext::Lowering(HoleAnchor::TemplateTy {
            ty: hir_ty,
            scope,
            assumptions,
        }),
        LayoutBoundaryIdentity::ContractField {
            contract,
            field_index,
        },
        vec![LayoutOccurrenceStep::Instantiation(0)],
    );
    let minter = HoleMinter::new(HoleAnchor::TemplateTy {
        ty: hir_ty,
        scope,
        assumptions,
    });
    declared.root_uses.extend(lower_layout_root_uses_in_hir_ty(
        db,
        hir_ty,
        scope,
        assumptions,
        &minter,
    ));

    let (is_provider, address_space, target) =
        match resolve_effect_handle_layout(db, scope, assumptions, declared.ty) {
            ProviderLayoutResolution::NotHandle => (false, default_space, declared.clone()),
            ProviderLayoutResolution::Resolved {
                impl_instance,
                target_template,
                space,
            } => {
                let target = match instantiate_provider_target_layout(
                    db,
                    scope,
                    assumptions,
                    declared.instance,
                    impl_instance,
                    target_template,
                ) {
                    Ok(target) => target,
                    Err(error) => return Some((name, Err(vec![error]))),
                };
                (true, space, target)
            }
            ProviderLayoutResolution::Ambiguous => {
                return Some((
                    name,
                    Err(vec![ContractLayoutError::AmbiguousProviderLayout]),
                ));
            }
            ProviderLayoutResolution::UnresolvedTarget => {
                return Some((
                    name,
                    Err(vec![ContractLayoutError::UnresolvedProviderTarget]),
                ));
            }
            ProviderLayoutResolution::UnresolvedSpace => {
                return Some((
                    name,
                    Err(vec![ContractLayoutError::UnresolvedProviderSpace]),
                ));
            }
        };

    let mut collector = FieldCollector::new(db, scope, assumptions, address_space);
    validate_template_roots(db, declared.ty, &mut collector.errors);
    validate_template_roots(db, target.ty, &mut collector.errors);
    let root_place = StoragePlace::root(field_id);
    let basis_place = if is_provider {
        root_place.with_step(PlaceStep::ProviderTarget)
    } else {
        root_place.clone()
    };
    let counted =
        collector.walk_instantiation(&target, basis_place.clone(), &[], WalkMode::Counted);
    let materialize = if is_provider {
        collector.walk_provider_wrapper_instantiation(
            &declared,
            root_place.with_step(PlaceStep::DeclaredWrapper),
            &[],
            WalkMode::MaterializeOnly,
        )
    } else {
        WalkOutput::empty()
    };
    if !collector.errors.is_empty() {
        return Some((name, Err(collector.errors)));
    }
    let mut view_roots = Vec::new();
    for root in declared
        .root_uses
        .iter()
        .chain(&target.root_uses)
        .filter_map(|root_use| root_use.root(db))
    {
        if !view_roots.contains(&root) {
            view_roots.push(root);
        }
    }
    Some((
        name,
        finish_field_plan(
            db,
            CollectedFieldLayoutPlan {
                field: field_id,
                name,
                is_mut: field.is_mut,
                is_provider,
                address_space,
                inline_span: counted.inline_span,
                inline_leaves: counted.inline_leaves,
                declared_template: declared.ty,
                target_template: target.ty,
                slot_basis_template: target.ty,
                view_roots,
                occurrences: collector.occurrences,
                concrete_occurrences: collector.concrete_occurrences,
                nonterminal_occurrences: collector.nonterminal_occurrences,
                counted_events: counted.events,
                materialize_events: materialize.events,
            },
        ),
    ))
}

fn allocation_unit_extent<'db>(
    plan: &ValidatedFieldLayoutPlan<'db>,
    unit: AllocationUnitId,
) -> Option<usize> {
    match unit {
        AllocationUnitId::Scalar(_) => Some(1),
        AllocationUnitId::Indexed(family) => Some(plan.families.get(family.0 as usize)?.extent),
    }
}

fn assign_allocation_unit<'db>(
    plan: &mut ValidatedFieldLayoutPlan<'db>,
    unit: AllocationUnitId,
    space: ProviderAddressSpace,
    base: usize,
) -> Option<()> {
    match unit {
        AllocationUnitId::Scalar(cell) => {
            plan.cells.get_mut(cell.0 as usize)?.allocation =
                Some(RootAllocation { space, slot: base });
        }
        AllocationUnitId::Indexed(family) => {
            let family = plan.families.get_mut(family.0 as usize)?;
            family.allocation = Some(RootAllocation { space, slot: base });
        }
    }
    Some(())
}

fn allocation_lane_extent<'db>(
    plan: &ValidatedFieldLayoutPlan<'db>,
    lane: &AllocationLane<'db>,
) -> Result<usize, ContractLayoutError<'db>> {
    lane.members.iter().try_fold(0usize, |extent, member| {
        allocation_unit_extent(plan, *member)
            .map(|member_extent| extent.max(member_extent))
            .ok_or(ContractLayoutError::LayoutExtentOverflow)
    })
}

fn field_block_extents<'db>(
    plan: &ValidatedFieldLayoutPlan<'db>,
) -> Result<IndexMap<ProviderAddressSpace, usize>, ContractLayoutError<'db>> {
    let mut extents = IndexMap::new();
    extents.insert(plan.address_space, plan.inline_span);
    for lane in plan
        .counted_lanes
        .iter()
        .chain(&plan.materialize_only_lanes)
    {
        let extent = allocation_lane_extent(plan, lane)?;
        let total = extents.entry(lane.space).or_insert(0usize);
        *total = total
            .checked_add(extent)
            .ok_or(ContractLayoutError::LayoutExtentOverflow)?;
    }
    Ok(extents)
}

fn find_unreserved_block(
    cursor: usize,
    extent: usize,
    reservations: &[usize],
) -> Option<(usize, usize)> {
    if extent == 0 {
        return Some((cursor, cursor));
    }
    let mut base = cursor;
    loop {
        let end = base.checked_add(extent)?;
        let Some(reserved) = reservations
            .iter()
            .copied()
            .find(|reserved| *reserved >= base && *reserved < end)
        else {
            return Some((base, end));
        };
        base = reserved.checked_add(1)?;
    }
}

fn explicit_reservations<'db>(
    field_results: &[ContractFieldLayoutResult<'db>],
) -> Vec<ExplicitRootReservation<'db>> {
    let mut reservations: IndexMap<
        (ProviderAddressSpace, IntegerId<'db>),
        Vec<(ContractFieldId<'db>, ConcreteRootOccurrenceId)>,
    > = IndexMap::new();
    for field in field_results {
        let plan = field
            .result
            .as_ref()
            .expect("reservation collection requires every field to validate");
        for occurrence in &plan.concrete_occurrences {
            reservations
                .entry((occurrence.space, occurrence.value))
                .or_default()
                .push((plan.field, occurrence.id));
        }
    }
    reservations
        .into_iter()
        .map(|((space, value), occurrences)| ExplicitRootReservation {
            value,
            space,
            occurrences,
        })
        .collect()
}

fn allocate_lanes<'db>(
    plan: &mut ValidatedFieldLayoutPlan<'db>,
    lanes: &[AllocationLane<'db>],
    cursors: &mut FxHashMap<ProviderAddressSpace, usize>,
) -> Result<(), ContractLayoutError<'db>> {
    for lane in lanes {
        let extent = allocation_lane_extent(plan, lane)?;
        let base = *cursors.entry(lane.space).or_insert(0);
        let next = base
            .checked_add(extent)
            .ok_or(ContractLayoutError::LayoutExtentOverflow)?;
        for member in &lane.members {
            assign_allocation_unit(plan, *member, lane.space, base)
                .ok_or(ContractLayoutError::LayoutExtentOverflow)?;
        }
        cursors.insert(lane.space, next);
    }
    Ok(())
}

fn allocate_contract<'db>(
    db: &'db dyn HirAnalysisDb,
    field_results: &[ContractFieldLayoutResult<'db>],
) -> Result<AllocatedContractStorageLayout<'db>, (ContractFieldId<'db>, ContractLayoutError<'db>)> {
    let explicit_reservations = explicit_reservations(field_results);
    let mut reserved_slots: FxHashMap<ProviderAddressSpace, Vec<usize>> = FxHashMap::default();
    for reservation in &explicit_reservations {
        if let Some(slot) = reservation.value.data(db).to_usize() {
            reserved_slots
                .entry(reservation.space)
                .or_default()
                .push(slot);
        }
    }
    for slots in reserved_slots.values_mut() {
        slots.sort_unstable();
        slots.dedup();
    }
    let mut counters: FxHashMap<ProviderAddressSpace, usize> = FxHashMap::default();
    let mut fields = IndexMap::new();
    for field in field_results {
        let mut plan = field
            .result
            .as_ref()
            .expect("allocation requires every field to validate")
            .clone();
        let extents = field_block_extents(&plan).map_err(|error| (field.field, error))?;
        let mut bases = FxHashMap::default();
        let mut cursors = FxHashMap::default();
        for (space, extent) in extents {
            let cursor = *counters.entry(space).or_insert(0);
            let (base, end) = find_unreserved_block(
                cursor,
                extent,
                reserved_slots.get(&space).map_or(&[], Vec::as_slice),
            )
            .ok_or((field.field, ContractLayoutError::LayoutExtentOverflow))?;
            bases.insert(space, base);
            cursors.insert(space, base);
            if extent != 0 {
                if space == ProviderAddressSpace::Code && end.checked_mul(32).is_none() {
                    return Err((field.field, ContractLayoutError::LayoutExtentOverflow));
                }
                counters.insert(space, end);
            }
        }
        let slot_offset = bases[&plan.address_space];
        let slot_count = counters
            .get(&plan.address_space)
            .copied()
            .unwrap_or(slot_offset)
            .checked_sub(slot_offset)
            .ok_or((field.field, ContractLayoutError::LayoutExtentOverflow))?;
        let field_cursor = cursors
            .get_mut(&plan.address_space)
            .expect("the primary field block must have a cursor");
        *field_cursor = field_cursor
            .checked_add(plan.inline_span)
            .ok_or((field.field, ContractLayoutError::LayoutExtentOverflow))?;
        let counted_lanes = plan.counted_lanes.clone();
        allocate_lanes(&mut plan, &counted_lanes, &mut cursors)
            .map_err(|error| (field.field, error))?;
        let materialize_only_lanes = plan.materialize_only_lanes.clone();
        allocate_lanes(&mut plan, &materialize_only_lanes, &mut cursors)
            .map_err(|error| (field.field, error))?;
        debug_assert!(
            cursors.iter().all(|(space, cursor)| {
                counters.get(space).copied().unwrap_or(*cursor) == *cursor
            })
        );
        let declared = assigned_view(db, plan.declared_template, &plan.bindings);
        let target = assigned_view(db, plan.target_template, &plan.bindings);
        let slot_basis = assigned_view(db, plan.slot_basis_template, &plan.bindings);
        debug_assert!(declared.all_roots_classified(db));
        debug_assert!(target.all_roots_classified(db));
        debug_assert!(slot_basis.all_roots_classified(db));
        fields.insert(
            field.name,
            FieldStorageLayout {
                field: plan.field,
                name: plan.name,
                is_mut: plan.is_mut,
                is_provider: plan.is_provider,
                address_space: plan.address_space,
                slot_offset,
                slot_count,
                inline_span: plan.inline_span,
                inline_leaves: plan.inline_leaves,
                declared,
                target,
                slot_basis,
                occurrences: plan.occurrences,
                concrete_occurrences: plan.concrete_occurrences,
                cells: plan.cells,
                families: plan.families,
                overlay_groups: plan.overlay_groups,
                place_roots: plan.place_roots,
                root_bindings: plan.bindings,
            },
        );
    }
    Ok(AllocatedContractStorageLayout {
        fields,
        explicit_reservations,
        high_water_by_address_space: counters,
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum IntervalOwner<'db> {
    Inline(ContractFieldId<'db>),
    Unit(ContractFieldId<'db>, AllocationUnitId),
}

#[derive(Debug, Clone, Copy)]
struct AllocationInterval<'db> {
    field: ContractFieldId<'db>,
    owner: IntervalOwner<'db>,
    start: usize,
    end: usize,
}

fn occurrence_placeholder_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    occurrence: &RootOccurrence<'db>,
) -> Option<TyId<'db>> {
    let hole = structural_hole_id(db, occurrence.placeholder)?;
    let expected = layout_hole_fallback_ty(db, hole.expected_ty(db));
    (hole.root(db) == occurrence.root
        && !matches!(
            hole.origin(db),
            StructuralHoleOrigin::ExplicitWildcard { .. }
        )
        && matches!(
            expected.data(db),
            TyData::TyBase(TyBase::Prim(PrimTy::U256 | PrimTy::Usize))
        ))
    .then_some(expected)
}

struct ResolvedStoragePlace<'db> {
    ty: TyId<'db>,
    space: ProviderAddressSpace,
    segments: Vec<ContractLayoutPathSegment<'db>>,
}

fn resolve_storage_place<'db>(
    db: &'db dyn HirAnalysisDb,
    field: &FieldStorageLayout<'db>,
    place: &StoragePlace<'db>,
) -> Option<ResolvedStoragePlace<'db>> {
    if place.field != field.field {
        return None;
    }
    let mut steps = place.steps.as_slice();
    let mut ty = field.target.template;
    if field.is_provider
        && let Some((first, rest)) = steps.split_first()
    {
        match first {
            PlaceStep::ProviderTarget => steps = rest,
            PlaceStep::DeclaredWrapper => {
                ty = field.declared.template;
                steps = rest;
            }
            _ => {}
        }
    }
    let mut space = field.address_space;
    let mut enum_variant = None;
    let mut segments = Vec::new();
    let mut dimension = 0u32;
    for step in steps {
        match *step {
            PlaceStep::ProviderTarget => {
                let ProviderLayoutResolution::Resolved {
                    impl_instance,
                    space: target_space,
                    ..
                } = resolve_effect_handle_layout(
                    db,
                    field.field.contract.scope(),
                    PredicateListId::empty_list(db),
                    ty,
                )
                else {
                    return None;
                };
                let target_ident = IdentId::new(db, "Target".to_string());
                ty = normalize_ty(
                    db,
                    impl_instance.instantiated_assoc_ty(db, target_ident)?,
                    field.field.contract.scope(),
                    PredicateListId::empty_list(db),
                );
                space = target_space;
            }
            PlaceStep::DeclaredWrapper => {}
            PlaceStep::TransparentInner => {
                ty = ty.as_capability(db)?.1;
            }
            PlaceStep::StructField(field_idx) => {
                let adt = ty.adt_def(db)?;
                let AdtRef::Struct(struct_) = adt.adt_ref(db) else {
                    return None;
                };
                let name = FieldParent::Struct(struct_)
                    .fields(db)
                    .nth(field_idx as usize)
                    .and_then(|field| field.name(db))?;
                segments.push(ContractLayoutPathSegment::Member {
                    name,
                    index: field_idx,
                });
                ty = instantiate_adt_field_shape(
                    db,
                    adt,
                    0,
                    field_idx as usize,
                    ty.generic_args(db),
                );
            }
            PlaceStep::TupleElem(elem_idx) => {
                if !ty.is_tuple(db) {
                    return None;
                }
                segments.push(ContractLayoutPathSegment::TupleElement(elem_idx));
                ty = *ty.generic_args(db).get(elem_idx as usize)?;
            }
            PlaceStep::EnumVariant(variant_idx) => {
                let adt = ty.adt_def(db)?;
                let AdtRef::Enum(enum_) = adt.adt_ref(db) else {
                    return None;
                };
                let variant = EnumVariant::new(enum_, variant_idx as usize);
                segments.push(ContractLayoutPathSegment::Variant {
                    name: variant.ident(db)?,
                    index: variant_idx,
                });
                enum_variant = Some((adt, variant, ty.generic_args(db).to_vec()));
            }
            PlaceStep::EnumPayloadField(field_idx) => {
                let (adt, variant, args) = enum_variant.take()?;
                match variant.kind(db) {
                    VariantKind::Record(_) => {
                        let name = FieldParent::Variant(variant)
                            .fields(db)
                            .nth(field_idx as usize)
                            .and_then(|field| field.name(db))?;
                        segments.push(ContractLayoutPathSegment::Member {
                            name,
                            index: field_idx,
                        });
                    }
                    VariantKind::Tuple(_) => {
                        segments.push(ContractLayoutPathSegment::TupleElement(field_idx))
                    }
                    VariantKind::Unit => return None,
                }
                ty = instantiate_adt_field_shape(
                    db,
                    adt,
                    variant.idx as usize,
                    field_idx as usize,
                    &args,
                );
            }
            PlaceStep::ArrayElem(_) => {
                if !ty.is_array(db) {
                    return None;
                }
                let args = ty.generic_args(db);
                segments.push(ContractLayoutPathSegment::ArrayElement {
                    dimension,
                    len: args
                        .get(1)
                        .copied()
                        .and_then(|len| const_ty_to_usize(db, len))?,
                });
                dimension += 1;
                ty = *args.first()?;
            }
            PlaceStep::ConstParam(param_idx) => {
                let adt = ty.adt_def(db)?;
                let param = adt.params(db).get(param_idx as usize)?;
                let TyData::ConstTy(param) = param.data(db) else {
                    return None;
                };
                let ConstTyData::TyParam(param, _) = param.data(db) else {
                    return None;
                };
                segments.push(ContractLayoutPathSegment::ConstParameter {
                    name: param.name,
                    index: param_idx,
                });
                ty = *ty.generic_args(db).get(param_idx as usize)?;
            }
        }
    }
    enum_variant.is_none().then_some(ResolvedStoragePlace {
        ty,
        space,
        segments,
    })
}

fn storage_place_ty_and_space<'db>(
    db: &'db dyn HirAnalysisDb,
    field: &FieldStorageLayout<'db>,
    place: &StoragePlace<'db>,
) -> Option<(TyId<'db>, ProviderAddressSpace)> {
    let resolved = resolve_storage_place(db, field, place)?;
    Some((resolved.ty, resolved.space))
}

fn resolve_storage_place_with_dimensions<'db>(
    db: &'db dyn HirAnalysisDb,
    field: &FieldStorageLayout<'db>,
    place: &StoragePlace<'db>,
    dimensions: &[LayoutIndexDimension<'db>],
) -> Option<ResolvedStoragePlace<'db>> {
    let resolved = resolve_storage_place(db, field, place)?;
    resolved
        .segments
        .iter()
        .filter_map(|segment| match segment {
            ContractLayoutPathSegment::ArrayElement { len, .. } => Some(*len),
            _ => None,
        })
        .eq(dimensions.iter().map(|dimension| dimension.len))
        .then_some(resolved)
}

fn resolved_layout_parameter_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    field: &FieldStorageLayout<'db>,
    place: &StoragePlace<'db>,
    dimensions: &[LayoutIndexDimension<'db>],
) -> Option<TyId<'db>> {
    let resolved = resolve_storage_place_with_dimensions(db, field, place, dimensions)?;
    let TyData::ConstTy(const_ty) = resolved.ty.data(db) else {
        return None;
    };
    Some(const_ty.ty(db))
}

fn contract_layout_path<'db>(
    db: &'db dyn HirAnalysisDb,
    field: &FieldStorageLayout<'db>,
    place: &StoragePlace<'db>,
) -> ContractLayoutPath<'db> {
    let segments = resolve_storage_place(db, field, place)
        .expect("allocated layout paths are independently validated")
        .segments;
    ContractLayoutPath {
        field: field.name,
        segments,
    }
}

fn layout_integer<'db>(db: &'db dyn HirAnalysisDb, value: usize) -> IntegerId<'db> {
    IntegerId::new(db, BigUint::from(value))
}

fn indexed_layout_value<'db>(
    db: &'db dyn HirAnalysisDb,
    base: usize,
    dimensions: &[LayoutIndexDimension<'db>],
    strides: &[usize],
    extent: usize,
) -> ContractLayoutValue<'db> {
    ContractLayoutValue::Indexed {
        base: layout_integer(db, base),
        dimensions: dimensions.iter().map(|dimension| dimension.len).collect(),
        strides: strides.to_vec(),
        extent,
    }
}

fn inferred_parameter_entry<'db>(
    db: &'db dyn HirAnalysisDb,
    field: &FieldStorageLayout<'db>,
    occurrence: &RootOccurrence<'db>,
    address_space: ProviderAddressSpace,
    value: ContractLayoutValue<'db>,
) -> ContractLayoutEntry<'db> {
    let place = StoragePlace {
        field: occurrence.place.field,
        steps: occurrence.selector.clone(),
    };
    ContractLayoutEntry {
        field: field.field,
        path: contract_layout_path(db, field, &place),
        ty: occurrence_placeholder_ty(db, occurrence).unwrap_or(occurrence.placeholder),
        address_space,
        value,
        kind: ContractLayoutEntryKind::Parameter(ContractLayoutParameterOrigin::Inferred),
    }
}

fn allocated_contract_layout_report<'db>(
    db: &'db dyn HirAnalysisDb,
    fields: &IndexMap<IdentId<'db>, FieldStorageLayout<'db>>,
) -> ContractLayoutReport<'db> {
    let mut entries = Vec::new();
    for field in fields.values() {
        for leaf in &field.inline_leaves {
            let base = field
                .slot_offset
                .checked_add(leaf.offset)
                .expect("validated inline layout offset must fit in usize");
            let value = if leaf.dimensions.is_empty() {
                ContractLayoutValue::Scalar(layout_integer(db, base))
            } else {
                indexed_layout_value(
                    db,
                    base,
                    &leaf.dimensions,
                    &leaf.strides,
                    affine_family_extent(&leaf.dimensions, &leaf.strides)
                        .expect("validated inline layout family must have finite extent"),
                )
            };
            let mut path = contract_layout_path(db, field, &leaf.place);
            let kind = match leaf.kind {
                InlineLayoutLeafKind::Field => ContractLayoutEntryKind::InlineField,
                InlineLayoutLeafKind::EnumTag => {
                    path.segments.push(ContractLayoutPathSegment::EnumTag);
                    ContractLayoutEntryKind::EnumTag
                }
            };
            entries.push(ContractLayoutEntry {
                field: field.field,
                path,
                ty: leaf.ty,
                address_space: field.address_space,
                value,
                kind,
            });
        }
        for occurrence in &field.concrete_occurrences {
            let place = StoragePlace {
                field: occurrence.place.field,
                steps: occurrence.selector.clone(),
            };
            entries.push(ContractLayoutEntry {
                field: field.field,
                path: contract_layout_path(db, field, &place),
                ty: occurrence.ty,
                address_space: occurrence.space,
                value: ContractLayoutValue::Scalar(occurrence.value),
                kind: ContractLayoutEntryKind::Parameter(ContractLayoutParameterOrigin::Explicit),
            });
        }
        for cell in &field.cells {
            let Some(allocation) = cell.allocation else {
                continue;
            };
            for occurrence in &cell.occurrences {
                let occurrence = &field.occurrences[occurrence.0 as usize];
                entries.push(inferred_parameter_entry(
                    db,
                    field,
                    occurrence,
                    allocation.space,
                    ContractLayoutValue::Scalar(layout_integer(db, allocation.slot)),
                ));
            }
        }
        for family in &field.families {
            let Some(allocation) = family.allocation else {
                continue;
            };
            for occurrence in &family.occurrences {
                let occurrence = &field.occurrences[occurrence.0 as usize];
                entries.push(inferred_parameter_entry(
                    db,
                    field,
                    occurrence,
                    allocation.space,
                    indexed_layout_value(
                        db,
                        allocation.slot,
                        &family.dimensions,
                        &family.strides,
                        family.extent,
                    ),
                ));
            }
        }
    }
    let mut seen = FxHashSet::default();
    entries.retain(|entry| seen.insert(entry.clone()));
    entries.sort_by(|first, second| {
        let space_order = |space| match space {
            ProviderAddressSpace::Storage => 0,
            ProviderAddressSpace::Transient => 1,
            ProviderAddressSpace::Code => 2,
            ProviderAddressSpace::Memory => 3,
            ProviderAddressSpace::Calldata => 4,
        };
        space_order(first.address_space)
            .cmp(&space_order(second.address_space))
            .then_with(|| {
                first
                    .value
                    .base()
                    .data(db)
                    .cmp(second.value.base().data(db))
            })
    });
    ContractLayoutReport { entries }
}

#[salsa::tracked(return_ref)]
pub(crate) fn build_contract_layout_report<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> Option<ContractLayoutReport<'db>> {
    let fields = &contract.storage_layout(db).allocated.as_ref()?.fields;
    Some(allocated_contract_layout_report(db, fields))
}

fn storage_owner_space<'db>(
    db: &'db dyn HirAnalysisDb,
    field: &FieldStorageLayout<'db>,
    place: &StoragePlace<'db>,
    owner: TyId<'db>,
) -> Option<ProviderAddressSpace> {
    let (_, default_space) = storage_place_ty_and_space(db, field, place)?;
    owner_space_in_field(db, field, owner, default_space)
}

fn storage_place_root_space<'db>(
    db: &'db dyn HirAnalysisDb,
    field: &FieldStorageLayout<'db>,
    place: &StoragePlace<'db>,
) -> Option<ProviderAddressSpace> {
    let (owner, default_space) = storage_place_ty_and_space(db, field, place)?;
    owner_space_in_field(db, field, owner, default_space)
}

fn owner_space_in_field<'db>(
    db: &'db dyn HirAnalysisDb,
    field: &FieldStorageLayout<'db>,
    owner: TyId<'db>,
    default_space: ProviderAddressSpace,
) -> Option<ProviderAddressSpace> {
    match resolve_static_slot_layout(
        db,
        field.field.contract.scope(),
        PredicateListId::empty_list(db),
        owner,
    ) {
        StaticSlotLayoutResolution::NotStaticSlot => Some(default_space),
        StaticSlotLayoutResolution::Resolved(space) => Some(space),
        StaticSlotLayoutResolution::Ambiguous | StaticSlotLayoutResolution::UnresolvedSpace => None,
    }
}

/// Validates the completed graph independently of allocation construction.
/// Identity-equal occurrences are represented by one unit, and the only
/// permitted overlap between distinct units is membership in one explicit
/// enum overlay group.
pub fn validate_allocated_contract_layout<'db>(
    db: &'db dyn HirAnalysisDb,
    layout: &AllocatedContractStorageLayout<'db>,
) -> Result<(), LayoutInvariantError<'db>> {
    let mut intervals: FxHashMap<ProviderAddressSpace, Vec<AllocationInterval<'db>>> =
        FxHashMap::default();
    let mut permitted_overlaps = FxHashSet::default();
    let mut expected_reservations: IndexMap<
        (ProviderAddressSpace, IntegerId<'db>),
        Vec<(ContractFieldId<'db>, ConcreteRootOccurrenceId)>,
    > = IndexMap::new();
    let mut seen_fields = FxHashSet::default();

    for (field_idx, (name, field)) in layout.fields.iter().enumerate() {
        if *name != field.name
            || field.field.index as usize != field_idx
            || !seen_fields.insert(field.field)
            || (field.inline_span == 0) != field.inline_leaves.is_empty()
        {
            return Err(LayoutInvariantError::InvalidFieldExtent { field: field.field });
        }
        for leaf in &field.inline_leaves {
            let Some(extent) = affine_family_extent(&leaf.dimensions, &leaf.strides) else {
                return Err(LayoutInvariantError::InvalidFieldExtent { field: field.field });
            };
            if leaf.place.field != field.field
                || leaf.dimensions.iter().any(|dimension| dimension.len == 0)
                || resolve_storage_place_with_dimensions(db, field, &leaf.place, &leaf.dimensions)
                    .is_none()
                || leaf
                    .offset
                    .checked_add(extent)
                    .is_none_or(|end| end > field.inline_span)
            {
                return Err(LayoutInvariantError::InvalidFieldExtent { field: field.field });
            }
        }
        for (view, assigned) in [
            (LayoutViewKind::Declared, &field.declared),
            (LayoutViewKind::Target, &field.target),
            (LayoutViewKind::SlotBasis, &field.slot_basis),
        ] {
            if !assigned.all_roots_classified(db) {
                return Err(LayoutInvariantError::UnclassifiedViewRoot {
                    field: field.field,
                    view,
                });
            }
        }
        let (expected_family_geometry, expected_group_extents) = match derive_family_geometry(
            &field.occurrences,
            &field.cells,
            &field.families,
            &field.overlay_groups,
        ) {
            Ok(geometry) => geometry,
            Err(FamilyGeometryError::Overflow) => {
                return Err(LayoutInvariantError::InvalidFieldExtent { field: field.field });
            }
            Err(FamilyGeometryError::InvalidFamily(family)) => {
                return Err(LayoutInvariantError::InvalidFamilyRegion {
                    field: field.field,
                    family,
                });
            }
            Err(FamilyGeometryError::InvalidOverlay(group_idx)) => {
                return Err(LayoutInvariantError::InvalidOverlayGroup {
                    field: field.field,
                    lane: field
                        .overlay_groups
                        .get(group_idx)
                        .map_or(u32::MAX, |group| group.lane),
                });
            }
        };

        let mut occurrence_units = FxHashMap::default();
        let mut expected_by_root = FxHashMap::default();
        let mut scalar_roots = FxHashSet::default();
        let mut family_keys = FxHashSet::default();

        for (idx, occurrence) in field.concrete_occurrences.iter().enumerate() {
            let valid_ty = matches!(
                occurrence.ty.data(db),
                TyData::TyBase(TyBase::Prim(PrimTy::U256 | PrimTy::Usize))
            );
            let owner_space = storage_owner_space(db, field, &occurrence.place, occurrence.owner);
            let selector = StoragePlace {
                field: occurrence.place.field,
                steps: occurrence.selector.clone(),
            };
            if occurrence.id.0 as usize != idx
                || occurrence.order != occurrence.id.0
                || occurrence.place.field != field.field
                || !occurrence.selector.starts_with(&occurrence.place.steps)
                || occurrence
                    .index_dimensions
                    .iter()
                    .any(|dimension| dimension.len == 0)
                || !valid_ty
                || resolved_layout_parameter_ty(db, field, &selector, &occurrence.index_dimensions)
                    != Some(occurrence.ty)
                || owner_space != Some(occurrence.space)
            {
                return Err(LayoutInvariantError::InvalidConcreteOccurrence {
                    field: field.field,
                    occurrence: occurrence.id,
                });
            }
            expected_reservations
                .entry((occurrence.space, occurrence.value))
                .or_default()
                .push((field.field, occurrence.id));
        }

        if field.inline_span != 0 {
            let end = field
                .slot_offset
                .checked_add(field.inline_span)
                .ok_or(LayoutInvariantError::InvalidFieldExtent { field: field.field })?;
            intervals
                .entry(field.address_space)
                .or_default()
                .push(AllocationInterval {
                    field: field.field,
                    owner: IntervalOwner::Inline(field.field),
                    start: field.slot_offset,
                    end,
                });
        }

        for (cell_idx, cell) in field.cells.iter().enumerate() {
            if cell.id.0 as usize != cell_idx
                || cell.occurrences.is_empty()
                || !scalar_roots.insert(cell.root)
            {
                return Err(LayoutInvariantError::InvalidScalarCell {
                    field: field.field,
                    cell: cell.id,
                });
            }
            let mut role = RootRole::MaterializeOnly;
            for occurrence in &cell.occurrences {
                let Some(data) = field.occurrences.get(occurrence.0 as usize) else {
                    return Err(LayoutInvariantError::InvalidOccurrenceGraph {
                        field: field.field,
                        occurrence: *occurrence,
                    });
                };
                let Some(expected) = occurrence_placeholder_ty(db, data) else {
                    return Err(LayoutInvariantError::InvalidOccurrenceGraph {
                        field: field.field,
                        occurrence: *occurrence,
                    });
                };
                let selector = StoragePlace {
                    field: data.place.field,
                    steps: data.selector.clone(),
                };
                if data.id != *occurrence
                    || data.root != cell.root
                    || data.space != cell.space
                    || storage_place_root_space(db, field, &data.place) != Some(data.space)
                    || data.place.field != field.field
                    || !data.selector.starts_with(&data.place.steps)
                    || data
                        .index_dimensions
                        .iter()
                        .any(|dimension| dimension.len != 1)
                    || resolved_layout_parameter_ty(db, field, &selector, &data.index_dimensions)
                        != Some(expected)
                    || occurrence_units
                        .insert(*occurrence, AllocationUnitId::Scalar(cell.id))
                        .is_some()
                {
                    return Err(LayoutInvariantError::InvalidOccurrenceGraph {
                        field: field.field,
                        occurrence: *occurrence,
                    });
                }
                if let Some(previous) = expected_by_root.insert(data.root, expected)
                    && previous != expected
                {
                    return Err(LayoutInvariantError::InvalidOccurrenceGraph {
                        field: field.field,
                        occurrence: *occurrence,
                    });
                }
                role = role.join(data.role);
            }
            if role != cell.role {
                return Err(LayoutInvariantError::InvalidScalarCell {
                    field: field.field,
                    cell: cell.id,
                });
            }
            let allocation =
                cell.allocation
                    .ok_or(LayoutInvariantError::MissingScalarAllocation {
                        field: field.field,
                        cell: cell.id,
                    })?;
            if allocation.space != cell.space {
                return Err(LayoutInvariantError::InvalidScalarCell {
                    field: field.field,
                    cell: cell.id,
                });
            }
            let end = allocation
                .slot
                .checked_add(1)
                .ok_or(LayoutInvariantError::InvalidFieldExtent { field: field.field })?;
            intervals
                .entry(allocation.space)
                .or_default()
                .push(AllocationInterval {
                    field: field.field,
                    owner: IntervalOwner::Unit(field.field, AllocationUnitId::Scalar(cell.id)),
                    start: allocation.slot,
                    end,
                });
        }

        for (family_idx, family) in field.families.iter().enumerate() {
            let family_key = FamilyKey {
                root: family.lane,
                dimensions: family
                    .dimensions
                    .iter()
                    .map(|dimension| dimension.instance)
                    .collect(),
            };
            if family.id.0 as usize != family_idx
                || family.occurrences.is_empty()
                || family.dimensions.is_empty()
                || family.dimensions.iter().any(|dimension| dimension.len == 0)
                || !family.dimensions.iter().any(|dimension| dimension.len > 1)
                || family.extent == 0
                || family.strides.len() != family.dimensions.len()
                || !family_keys.insert(family_key)
            {
                return Err(LayoutInvariantError::InvalidFamilyRegion {
                    field: field.field,
                    family: family.id,
                });
            }
            let mut role = RootRole::MaterializeOnly;
            for occurrence in &family.occurrences {
                let Some(data) = field.occurrences.get(occurrence.0 as usize) else {
                    return Err(LayoutInvariantError::InvalidOccurrenceGraph {
                        field: field.field,
                        occurrence: *occurrence,
                    });
                };
                let Some(expected) = occurrence_placeholder_ty(db, data) else {
                    return Err(LayoutInvariantError::InvalidOccurrenceGraph {
                        field: field.field,
                        occurrence: *occurrence,
                    });
                };
                let selector = StoragePlace {
                    field: data.place.field,
                    steps: data.selector.clone(),
                };
                if data.id != *occurrence
                    || data.root != family.lane
                    || data.index_dimensions != family.dimensions
                    || data.space != family.space
                    || storage_place_root_space(db, field, &data.place) != Some(data.space)
                    || data.place.field != field.field
                    || !data.selector.starts_with(&data.place.steps)
                    || resolved_layout_parameter_ty(db, field, &selector, &data.index_dimensions)
                        != Some(expected)
                    || occurrence_units
                        .insert(*occurrence, AllocationUnitId::Indexed(family.id))
                        .is_some()
                {
                    return Err(LayoutInvariantError::InvalidOccurrenceGraph {
                        field: field.field,
                        occurrence: *occurrence,
                    });
                }
                if let Some(previous) = expected_by_root.insert(data.root, expected)
                    && previous != expected
                {
                    return Err(LayoutInvariantError::InvalidOccurrenceGraph {
                        field: field.field,
                        occurrence: *occurrence,
                    });
                }
                role = role.join(data.role);
            }
            if role != family.role {
                return Err(LayoutInvariantError::InvalidFamilyRegion {
                    field: field.field,
                    family: family.id,
                });
            }
            let allocation =
                family
                    .allocation
                    .ok_or(LayoutInvariantError::MissingFamilyAllocation {
                        field: field.field,
                        family: family.id,
                    })?;
            let expected_geometry = &expected_family_geometry[family_idx];
            if expected_geometry.extent != family.extent
                || expected_geometry.strides != family.strides
                || allocation.space != family.space
                || family.slot_for_indices(
                    &family
                        .dimensions
                        .iter()
                        .map(|dimension| dimension.len - 1)
                        .collect::<Vec<_>>(),
                ) != allocation.slot.checked_add(family.extent - 1)
            {
                return Err(LayoutInvariantError::InvalidFamilyRegion {
                    field: field.field,
                    family: family.id,
                });
            }
            let end = allocation.slot.checked_add(family.extent).ok_or(
                LayoutInvariantError::InvalidFamilyRegion {
                    field: field.field,
                    family: family.id,
                },
            )?;
            intervals
                .entry(allocation.space)
                .or_default()
                .push(AllocationInterval {
                    field: field.field,
                    owner: IntervalOwner::Unit(field.field, AllocationUnitId::Indexed(family.id)),
                    start: allocation.slot,
                    end,
                });
        }

        if occurrence_units.len() != field.occurrences.len()
            || field
                .occurrences
                .iter()
                .enumerate()
                .any(|(idx, occurrence)| {
                    occurrence.id.0 as usize != idx
                        || occurrence.order != occurrence.id.0
                        || !occurrence_units.contains_key(&occurrence.id)
                })
        {
            let occurrence = field
                .occurrences
                .iter()
                .find(|occurrence| !occurrence_units.contains_key(&occurrence.id))
                .map_or(RootOccurrenceId(u32::MAX), |occurrence| occurrence.id);
            return Err(LayoutInvariantError::InvalidOccurrenceGraph {
                field: field.field,
                occurrence,
            });
        }

        let mut expected_bindings = IndexMap::new();
        let mut expected_places: IndexMap<StoragePlace<'db>, Vec<LayoutBindingTarget>> =
            IndexMap::new();
        for occurrence in &field.occurrences {
            let unit = occurrence_units[&occurrence.id];
            let target = match unit {
                AllocationUnitId::Scalar(cell) => LayoutBindingTarget::Scalar(cell),
                AllocationUnitId::Indexed(family) => LayoutBindingTarget::Indexed(family),
            };
            let targets = expected_places.entry(occurrence.place.clone()).or_default();
            if !targets.contains(&target) {
                targets.push(target);
            }
            let leaf = LayoutBindingLeaf {
                selector: occurrence.selector.clone(),
                target,
            };
            for root in layout_root_lineage(db, occurrence.root) {
                let binding = expected_bindings
                    .entry(root)
                    .or_insert_with(|| LayoutBinding::Bound(Vec::new()));
                let LayoutBinding::Bound(leaves) = binding else {
                    return Err(LayoutInvariantError::InvalidRootBinding {
                        field: field.field,
                        root,
                    });
                };
                if !leaves.contains(&leaf) {
                    leaves.push(leaf.clone());
                }
            }
        }
        for assigned in [&field.declared, &field.target, &field.slot_basis] {
            for hole in crate::analysis::ty::layout_holes::collect_unique_structural_holes_in_order(
                db,
                assigned.template,
            ) {
                expected_bindings
                    .entry(hole.root(db))
                    .or_insert(LayoutBinding::NonPhysical);
            }
        }
        let binding_mismatch = field.root_bindings.len() != expected_bindings.len()
            || expected_bindings
                .iter()
                .any(|(root, binding)| field.root_bindings.get(root) != Some(binding));
        if binding_mismatch {
            let Some(root) = expected_bindings
                .keys()
                .chain(field.root_bindings.keys())
                .find(|root| expected_bindings.get(*root) != field.root_bindings.get(*root))
                .copied()
            else {
                return Err(LayoutInvariantError::InvalidPlaceBinding { field: field.field });
            };
            return Err(LayoutInvariantError::InvalidRootBinding {
                field: field.field,
                root,
            });
        }
        for (view, assigned) in [
            (LayoutViewKind::Declared, &field.declared),
            (LayoutViewKind::Target, &field.target),
            (LayoutViewKind::SlotBasis, &field.slot_basis),
        ] {
            if assigned != &assigned_view(db, assigned.template, &expected_bindings) {
                return Err(LayoutInvariantError::UnclassifiedViewRoot {
                    field: field.field,
                    view,
                });
            }
        }
        if field.place_roots.len() != expected_places.len()
            || expected_places
                .iter()
                .any(|(place, targets)| field.place_roots.get(place) != Some(targets))
        {
            return Err(LayoutInvariantError::InvalidPlaceBinding { field: field.field });
        }

        for (group_idx, group) in field.overlay_groups.iter().enumerate() {
            if group.enum_place.field != field.field
                || group.members.len() < 2
                || group.reserved_extent == 0
            {
                return Err(LayoutInvariantError::InvalidOverlayGroup {
                    field: field.field,
                    lane: group.lane,
                });
            }
            let mut base = None;
            let mut extent = 0usize;
            let mut seen_members = FxHashSet::default();
            for member in &group.members {
                if !seen_members.insert(*member) {
                    return Err(LayoutInvariantError::InvalidOverlayGroup {
                        field: field.field,
                        lane: group.lane,
                    });
                }
                let (member_space, member_base, member_extent) = match member {
                    AllocationUnitId::Scalar(cell) => {
                        let cell = field.cells.get(cell.0 as usize).ok_or(
                            LayoutInvariantError::InvalidOverlayGroup {
                                field: field.field,
                                lane: group.lane,
                            },
                        )?;
                        let allocation =
                            cell.allocation
                                .ok_or(LayoutInvariantError::InvalidOverlayGroup {
                                    field: field.field,
                                    lane: group.lane,
                                })?;
                        (cell.space, allocation.slot, 1)
                    }
                    AllocationUnitId::Indexed(family) => {
                        let family = field.families.get(family.0 as usize).ok_or(
                            LayoutInvariantError::InvalidOverlayGroup {
                                field: field.field,
                                lane: group.lane,
                            },
                        )?;
                        let allocation =
                            family
                                .allocation
                                .ok_or(LayoutInvariantError::InvalidOverlayGroup {
                                    field: field.field,
                                    lane: group.lane,
                                })?;
                        (family.space, allocation.slot, family.extent)
                    }
                };
                if member_space != group.space || base.is_some_and(|base| base != member_base) {
                    return Err(LayoutInvariantError::InvalidOverlayGroup {
                        field: field.field,
                        lane: group.lane,
                    });
                }
                base = Some(member_base);
                extent = extent.max(member_extent);
            }
            let mut witnessed_members = FxHashSet::default();
            for (idx, member) in group.members.iter().enumerate() {
                for other in group.members.iter().skip(idx + 1) {
                    let Some(witnesses) = allocation_unit_overlay_witnesses(
                        &field.occurrences,
                        &field.cells,
                        &field.families,
                        *member,
                        *other,
                    ) else {
                        return Err(LayoutInvariantError::InvalidOverlayGroup {
                            field: field.field,
                            lane: group.lane,
                        });
                    };
                    if witnesses.contains(&group.enum_place) {
                        witnessed_members.insert(*member);
                        witnessed_members.insert(*other);
                    }
                    permitted_overlaps.insert((
                        IntervalOwner::Unit(field.field, *member),
                        IntervalOwner::Unit(field.field, *other),
                    ));
                    permitted_overlaps.insert((
                        IntervalOwner::Unit(field.field, *other),
                        IntervalOwner::Unit(field.field, *member),
                    ));
                }
            }
            if witnessed_members.len() != group.members.len()
                || extent != group.reserved_extent
                || expected_group_extents[group_idx] != group.reserved_extent
            {
                return Err(LayoutInvariantError::InvalidOverlayGroup {
                    field: field.field,
                    lane: group.lane,
                });
            }
        }

        let expected_end = field
            .slot_offset
            .checked_add(field.slot_count)
            .ok_or(LayoutInvariantError::InvalidFieldExtent { field: field.field })?;
        let mut primary_range = None;
        for (space, space_intervals) in &intervals {
            let mut field_intervals = space_intervals
                .iter()
                .filter(|interval| interval.field == field.field)
                .collect::<Vec<_>>();
            field_intervals.sort_by_key(|interval| (interval.start, interval.end));
            let Some(first) = field_intervals.first() else {
                continue;
            };
            let mut end = first.end;
            for interval in field_intervals.iter().skip(1) {
                if interval.start > end {
                    return Err(LayoutInvariantError::InvalidFieldExtent { field: field.field });
                }
                end = end.max(interval.end);
            }
            if *space == field.address_space {
                primary_range = Some((first.start, end));
            }
        }
        if primary_range.map_or((field.slot_offset, field.slot_offset), |range| range)
            != (field.slot_offset, expected_end)
        {
            return Err(LayoutInvariantError::InvalidFieldExtent { field: field.field });
        }
    }

    if layout.explicit_reservations.len() != expected_reservations.len() {
        let space = layout
            .explicit_reservations
            .first()
            .map_or(ProviderAddressSpace::Storage, |reservation| {
                reservation.space
            });
        return Err(LayoutInvariantError::InvalidExplicitReservation { space });
    }
    let mut seen_reservations = FxHashSet::default();
    for reservation in &layout.explicit_reservations {
        if !seen_reservations.insert((reservation.space, reservation.value))
            || expected_reservations.get(&(reservation.space, reservation.value))
                != Some(&reservation.occurrences)
        {
            return Err(LayoutInvariantError::InvalidExplicitReservation {
                space: reservation.space,
            });
        }
        if let Some(value) = reservation.value.data(db).to_usize()
            && let Some(interval) = intervals
                .get(&reservation.space)
                .into_iter()
                .flatten()
                .find(|interval| value >= interval.start && value < interval.end)
        {
            return Err(LayoutInvariantError::ExplicitReservationOverlap {
                space: reservation.space,
                field: interval.field,
                value: reservation.value,
            });
        }
    }

    for (space, space_intervals) in &mut intervals {
        space_intervals.sort_by_key(|interval| (interval.start, interval.end));
        for (idx, first) in space_intervals.iter().enumerate() {
            for second in space_intervals.iter().skip(idx + 1) {
                if second.start >= first.end {
                    break;
                }
                if !permitted_overlaps.contains(&(first.owner, second.owner)) {
                    return Err(LayoutInvariantError::AllocationOverlap {
                        space: *space,
                        first_field: first.field,
                        second_field: second.field,
                    });
                }
            }
        }
        let actual = space_intervals
            .iter()
            .map(|interval| interval.end)
            .max()
            .unwrap_or(0);
        if layout.high_water_by_address_space.get(space).copied() != Some(actual) {
            return Err(LayoutInvariantError::InvalidAddressSpaceHighWater { space: *space });
        }
    }
    for (space, high_water) in &layout.high_water_by_address_space {
        if (*space == ProviderAddressSpace::Code && high_water.checked_mul(32).is_none())
            || (*high_water != 0 && !intervals.contains_key(space))
        {
            return Err(LayoutInvariantError::InvalidAddressSpaceHighWater { space: *space });
        }
    }
    Ok(())
}

#[salsa::tracked(return_ref)]
pub(crate) fn build_contract_storage_layout<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> ContractStorageLayoutResult<'db> {
    let field_count = contract
        .hir_fields(db)
        .data(db)
        .iter()
        .filter(|field| field.name.is_present())
        .count();
    let mut field_results = Vec::with_capacity(field_count);
    let mut first_by_name = FxHashMap::default();
    for field_index in 0..field_count {
        if let Some((name, result)) = collect_field_plan(db, contract, field_index as u32) {
            let field = ContractFieldId {
                contract,
                index: field_index as u32,
            };
            let result_idx = field_results.len();
            field_results.push(ContractFieldLayoutResult {
                field,
                name,
                result,
            });
            if let Some(first_idx) = first_by_name.insert(name, result_idx) {
                field_results[first_idx].result = Err(vec![ContractLayoutError::InvalidFieldType]);
                field_results[result_idx].result = Err(vec![ContractLayoutError::InvalidFieldType]);
            }
        }
    }
    let allocated = if field_results.iter().all(|field| field.result.is_ok()) {
        match allocate_contract(db, &field_results) {
            Ok(layout) => match validate_allocated_contract_layout(db, &layout) {
                Ok(()) => Some(layout),
                Err(error) => {
                    debug_assert!(false, "invalid completed contract layout graph: {error:?}");
                    if let Some(field) = field_results.first_mut() {
                        field.result = Err(vec![ContractLayoutError::InternalLayoutGraph]);
                    }
                    None
                }
            },
            Err((field, error)) => {
                if let Some(result) = field_results
                    .iter_mut()
                    .find(|result| result.field == field)
                {
                    result.result = Err(vec![error]);
                }
                None
            }
        }
    } else {
        None
    };
    ContractStorageLayoutResult {
        field_results,
        allocated,
    }
}
