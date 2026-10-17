use common::{
    indexmap::IndexMap,
    layout::{StorageFieldShape, StorageLane, storage_fields_layout},
};
use num_bigint::BigUint;
use num_traits::ToPrimitive;
use ruint::aliases::U256;
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;

use crate::{
    HirDb,
    analysis::{
        HirAnalysisDb,
        semantic::{RuntimeSizeError, runtime_size_bytes_with_source},
        ty::{
            AddressSpace,
            adt_def::{
                AdtDef, AdtRef, ConcreteTypeView, instantiate_adt_field_for_concrete_demand,
            },
            const_ty::{ConcreteArrayLengthError, demand_concrete_array_length},
            normalize::normalize_ty,
            provider::pinned_space,
            state_index_tys,
            trait_resolution::PredicateListId,
            ty_check::contract_field_slot,
            ty_def::{PrimTy, TyBase, TyData, TyId},
            ty_lower::lower_opt_hir_ty,
        },
    },
    hir_def::{
        Contract, EnumVariant, FieldParent, IdentId, IntegerId, VariantKind, scope_graph::ScopeId,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct ContractFieldId<'db> {
    pub contract: Contract<'db>,
    pub index: u32,
}

impl<'db> ContractFieldId<'db> {
    /// The field's scope. The id's index counts only the named fields.
    pub fn scope(self, db: &'db dyn HirDb) -> Option<ScopeId<'db>> {
        let (idx, _) = self
            .contract
            .hir_fields(db)
            .data(db)
            .iter()
            .enumerate()
            .filter(|(_, field)| field.name.is_present())
            .nth(self.index as usize)?;
        Some(ScopeId::Field(
            FieldParent::Contract(self.contract),
            idx as u16,
        ))
    }
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
    pub address_space: AddressSpace,
    pub value: ContractLayoutValue<'db>,
    /// For a scalar packed with others into one slot, its bytes in the slot.
    pub lane: Option<ContractLayoutLane>,
    pub kind: ContractLayoutEntryKind,
}

/// The bytes of a storage slot that hold a packed scalar, counted from the
/// low-order end like Solidity's `offset`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct ContractLayoutLane {
    pub byte_offset: u8,
    pub byte_width: u8,
}

impl From<StorageLane> for ContractLayoutLane {
    fn from(lane: StorageLane) -> Self {
        Self {
            byte_offset: lane.byte_offset as u8,
            byte_width: lane.byte_width as u8,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ContractLayoutEntryKind {
    InlineField,
    /// A storage collection, whose place is its identity.
    Collection,
    EnumTag,
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
    EnumTag,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum ContractLayoutError<'db> {
    InvalidFieldType,
    InvalidConcreteArrayLength {
        invalid: TyId<'db>,
    },
    InconsistentConcreteArrayLength {
        array: TyId<'db>,
    },
    /// `#[slot(e)]` whose `e` is not a constant `u256`.
    InvalidExplicitSlot,
    /// `#[slot(e)]` on a field that lives in code, which has no slots.
    ExplicitSlotInCode,
    /// `#[slot(e)]` whose slots overlap those of the explicitly placed `other`.
    ExplicitSlotOverlap {
        other: IdentId<'db>,
    },
    LayoutExtentOverflow,
    IncompleteAdtLayoutProjection {
        ty: TyId<'db>,
    },
    /// A field holding a pointer into memory or calldata, which last at
    /// most one call.
    CallPointerInState {
        pointer: TyId<'db>,
    },
    /// A persistent collection inside a transient collection's contents.
    PersistentUnderTransient {
        collection: TyId<'db>,
    },
    /// A type whose all-zero representation is not a value, which a state
    /// space holds until it is written.
    ZeroInvalidInState {
        invalid: TyId<'db>,
    },
    /// A collection entry type that cannot be laid out, for `reason`.
    EntryLayout {
        entry: TyId<'db>,
        reason: &'static str,
    },
}

impl ContractLayoutError<'_> {
    pub fn summary(&self) -> &'static str {
        match self {
            Self::InvalidFieldType => "field type is invalid",
            Self::InvalidConcreteArrayLength { .. } => "array length const evaluation failed",
            Self::InconsistentConcreteArrayLength { .. } => {
                "canonical and source array lengths disagree"
            }
            Self::InvalidExplicitSlot => "the explicit slot is not a constant `u256`",
            Self::ExplicitSlotInCode => "a code field has no slot",
            Self::ExplicitSlotOverlap { .. } => "the explicit slots overlap another field's",
            Self::LayoutExtentOverflow => "layout extent overflowed",
            Self::IncompleteAdtLayoutProjection { .. } => "layout projection is incomplete",
            Self::CallPointerInState { .. } => {
                "contract state holds a pointer into memory or calldata"
            }
            Self::PersistentUnderTransient { .. } => {
                "a persistent collection lies in transient storage"
            }
            Self::ZeroInvalidInState { .. } => "contract state holds a type zero is no value of",
            Self::EntryLayout { .. } => "a collection entry cannot be laid out",
        }
    }
}

/// A contract field's layout before the contract places it: its address
/// space, the slots it spans and the parts that take them.
#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ValidatedFieldLayoutPlan<'db> {
    field: ContractFieldId<'db>,
    name: IdentId<'db>,
    is_mut: bool,
    address_space: AddressSpace,
    declared: TyId<'db>,
    /// The slot `#[slot(e)]` places the field at.
    explicit_slot: Option<U256>,
    slot_count: usize,
    /// The spaces the field's slot numbers are taken in: its own, when a
    /// part of it lies outside a pinned type, and those its pinned types are
    /// pinned to, which keep their contents at their own slot numbers there
    /// (a `TSlot`'s value in transient storage).
    spaces: Vec<AddressSpace>,
    inline_leaves: Vec<InlineLayoutLeaf<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct FieldStorageLayout<'db> {
    pub field: ContractFieldId<'db>,
    pub name: IdentId<'db>,
    pub is_mut: bool,
    pub address_space: AddressSpace,
    /// The field's declared type.
    pub declared: TyId<'db>,
    /// The field's first slot, or for a code field its first word.
    pub slot_offset: U256,
    pub slot_count: usize,
    inline_leaves: Vec<InlineLayoutLeaf<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct AllocatedContractStorageLayout<'db> {
    pub fields: IndexMap<IdentId<'db>, FieldStorageLayout<'db>>,
    /// The words the code fields take.
    pub code_slot_count: usize,
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

    /// The fields whose layout is valid, by name: index, `is_mut` and
    /// declared type.
    pub(crate) fn semantic_fields(
        &self,
    ) -> Vec<(IdentId<'db>, ContractFieldId<'db>, bool, TyId<'db>)> {
        self.field_results
            .iter()
            .filter_map(|field| {
                let plan = field.result.as_ref().ok()?;
                Some((field.name, plan.field, plan.is_mut, plan.declared))
            })
            .collect()
    }
}

/// The bytes a scalar struct or tuple field takes in a packed storage slot:
/// booleans and integers narrower than a word, like Solidity's value types.
/// Everything else takes whole slots. Runtime lowering classifies scalars
/// the same way (`mir::runtime::storage_scalar_bytes`).
fn storage_packable_bytes(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<u32> {
    let TyData::TyBase(TyBase::Prim(prim)) = ty.base_ty(db).data(db) else {
        return None;
    };
    match prim {
        PrimTy::Bool | PrimTy::U8 | PrimTy::I8 => Some(1),
        PrimTy::U16 | PrimTy::I16 => Some(2),
        PrimTy::U32 | PrimTy::I32 => Some(4),
        PrimTy::U64 | PrimTy::I64 => Some(8),
        PrimTy::U128 | PrimTy::I128 => Some(16),
        _ => None,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
enum InlineLayoutLeafKind {
    Field,
    Collection,
    EnumTag,
}

/// A part of a field that takes slots: a scalar, a storage collection or an
/// enum tag, at `offset` slots from the field's first, repeated along each
/// enclosing array's dimension at its stride.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
struct InlineLayoutLeaf<'db> {
    path: Vec<ContractLayoutPathSegment<'db>>,
    ty: TyId<'db>,
    offset: usize,
    /// The bytes of slot `offset` holding a packed scalar.
    lane: Option<ContractLayoutLane>,
    dimensions: Vec<usize>,
    strides: Vec<usize>,
    kind: InlineLayoutLeafKind,
}

#[derive(Clone, Debug)]
struct WalkOutput<'db> {
    span: usize,
    leaves: Vec<InlineLayoutLeaf<'db>>,
    /// The bytes of a scalar that packs with its neighbours in a struct or
    /// tuple; `None` for values that take whole slots.
    packable_bytes: Option<u32>,
}

impl<'db> WalkOutput<'db> {
    fn empty() -> Self {
        Self {
            span: 0,
            leaves: Vec::new(),
            packable_bytes: None,
        }
    }

    fn leaf(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        path: &[ContractLayoutPathSegment<'db>],
        dimensions: &[usize],
        kind: InlineLayoutLeafKind,
    ) -> Self {
        Self {
            span: 1,
            leaves: vec![InlineLayoutLeaf {
                path: path.to_vec(),
                ty,
                offset: 0,
                lane: None,
                dimensions: dimensions.to_vec(),
                strides: vec![0; dimensions.len()],
                kind,
            }],
            packable_bytes: match kind {
                InlineLayoutLeafKind::Field => storage_packable_bytes(db, ty),
                InlineLayoutLeafKind::Collection | InlineLayoutLeafKind::EnumTag => None,
            },
        }
    }
}

/// Lays out one contract field's value: the slots each part takes, from
/// the field's first.
struct FieldWalker<'db> {
    db: &'db dyn HirAnalysisDb,
    errors: Vec<ContractLayoutError<'db>>,
}

impl<'db> FieldWalker<'db> {
    fn push_error(&mut self, error: ContractLayoutError<'db>) {
        if self.errors.contains(&error) {
            return;
        }
        // A field's array length error is the one its diagnostic reports.
        let position = if matches!(
            error,
            ContractLayoutError::InvalidConcreteArrayLength { .. }
        ) {
            self.errors
                .iter()
                .position(|prior| {
                    !matches!(
                        prior,
                        ContractLayoutError::InvalidConcreteArrayLength { .. }
                    )
                })
                .unwrap_or(self.errors.len())
        } else {
            self.errors.len()
        };
        self.errors.insert(position, error);
    }

    fn walk_ty(
        &mut self,
        views: ConcreteTypeView<'db>,
        path: &[ContractLayoutPathSegment<'db>],
        dimensions: &[usize],
    ) -> WalkOutput<'db> {
        let ConcreteTypeView {
            canonical: ty,
            source,
        } = views;
        if let Some((kind, inner)) = ty.as_capability(self.db) {
            let Some((_, source_inner)) = source
                .as_capability(self.db)
                .filter(|(source_kind, _)| *source_kind == kind)
            else {
                self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
                return WalkOutput::empty();
            };
            self.walk_ty(ConcreteTypeView::new(inner, source_inner), path, dimensions)
        } else if let TyData::ConstTy(const_ty) = ty.data(self.db) {
            let TyData::ConstTy(source_const_ty) = source.data(self.db) else {
                self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
                return WalkOutput::empty();
            };
            self.walk_ty(
                ConcreteTypeView::new(const_ty.ty(self.db), source_const_ty.ty(self.db)),
                path,
                dimensions,
            )
        } else if ty.is_tuple(self.db) {
            let source_fields = source.field_types(self.db);
            let fields = ty.field_types(self.db);
            if !source.is_tuple(self.db) || fields.len() != source_fields.len() {
                self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
                return WalkOutput::empty();
            }
            let items = fields
                .into_iter()
                .zip(source_fields)
                .enumerate()
                .map(|(idx, (elem, source_elem))| {
                    (
                        ConcreteTypeView::new(elem, source_elem),
                        ContractLayoutPathSegment::TupleElement(idx as u32),
                    )
                })
                .collect();
            self.walk_sequence(items, path, dimensions, true)
        } else if ty.as_closure(self.db).is_some() {
            // A closure lies in storage as a product of its captures, as MIR
            // stores it.
            let source_captures = source.field_types(self.db);
            let captures = ty.field_types(self.db);
            if source.as_closure(self.db).is_none() || captures.len() != source_captures.len() {
                self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
                return WalkOutput::empty();
            }
            let items = captures
                .into_iter()
                .zip(source_captures)
                .enumerate()
                .map(|(idx, (capture, source_capture))| {
                    (
                        ConcreteTypeView::new(capture, source_capture),
                        ContractLayoutPathSegment::TupleElement(idx as u32),
                    )
                })
                .collect();
            self.walk_sequence(items, path, dimensions, true)
        } else if ty.is_array(self.db) {
            self.walk_array(views, path, dimensions)
        } else if let Some(adt) = ty.adt_def(self.db) {
            let output = self.walk_adt(views, adt, path, dimensions);
            // A pinned type is reported at its place, which is its identity,
            // rather than by the slots its private fields take. It takes whole
            // slots, at least one even without fields: its root number derives
            // its region, so siblings need distinct ones.
            if ty.pin(self.db).is_some() {
                WalkOutput {
                    span: output.span.max(1),
                    ..WalkOutput::leaf(
                        self.db,
                        ty,
                        path,
                        dimensions,
                        InlineLayoutLeafKind::Collection,
                    )
                }
            } else {
                output
            }
        // A contract value is its address, a word, as MIR lays it out.
        } else if ty.is_never(self.db)
            || ty.is_zero_sized(self.db)
            || matches!(
                ty.base_ty(self.db).data(self.db),
                TyData::TyBase(TyBase::Func(_))
            )
        {
            WalkOutput::empty()
        } else {
            WalkOutput::leaf(self.db, ty, path, dimensions, InlineLayoutLeafKind::Field)
        }
    }

    /// Lays out a struct's or tuple's fields (`packed`) or an enum payload.
    /// Packed sequences place narrow scalars like Solidity places struct
    /// members, see `common::layout::storage_fields_layout`.
    fn walk_sequence(
        &mut self,
        items: Vec<(ConcreteTypeView<'db>, ContractLayoutPathSegment<'db>)>,
        path: &[ContractLayoutPathSegment<'db>],
        dimensions: &[usize],
        packed: bool,
    ) -> WalkOutput<'db> {
        let outputs: Vec<_> = items
            .into_iter()
            .map(|(views, segment)| {
                let mut item_path = path.to_vec();
                item_path.push(segment);
                self.walk_ty(views, &item_path, dimensions)
            })
            .collect();
        let placements = if packed {
            let shapes = outputs.iter().map(|output| match output.packable_bytes {
                Some(bytes) => StorageFieldShape::Scalar { bytes },
                None => StorageFieldShape::Aggregate {
                    slots: output.span as u64,
                },
            });
            match storage_fields_layout(shapes) {
                Ok(layout) => Some(layout),
                Err(_) => {
                    self.push_error(ContractLayoutError::LayoutExtentOverflow);
                    return WalkOutput::empty();
                }
            }
        } else {
            None
        };
        let mut span = 0usize;
        let mut leaves = Vec::new();
        for (idx, mut output) in outputs.into_iter().enumerate() {
            let (start, lane) = match &placements {
                Some(layout) => {
                    let placement = layout.placements[idx];
                    let Ok(slot) = usize::try_from(placement.slot) else {
                        self.push_error(ContractLayoutError::LayoutExtentOverflow);
                        continue;
                    };
                    (slot, placement.lane.map(ContractLayoutLane::from))
                }
                None => (span, None),
            };
            let Some(end) = start.checked_add(output.span) else {
                self.push_error(ContractLayoutError::LayoutExtentOverflow);
                continue;
            };
            for leaf in &mut output.leaves {
                let Some(offset) = leaf.offset.checked_add(start) else {
                    self.push_error(ContractLayoutError::LayoutExtentOverflow);
                    continue;
                };
                leaf.offset = offset;
                if lane.is_some() {
                    leaf.lane = lane;
                }
            }
            span = span.max(end);
            leaves.extend(output.leaves);
        }
        if let Some(layout) = &placements {
            let Ok(slots) = usize::try_from(layout.slots) else {
                self.push_error(ContractLayoutError::LayoutExtentOverflow);
                return WalkOutput::empty();
            };
            span = slots;
        }
        WalkOutput {
            span,
            leaves,
            packable_bytes: None,
        }
    }

    fn walk_array(
        &mut self,
        views: ConcreteTypeView<'db>,
        path: &[ContractLayoutPathSegment<'db>],
        dimensions: &[usize],
    ) -> WalkOutput<'db> {
        let ConcreteTypeView {
            canonical: ty,
            source,
        } = views;
        let (_, args) = ty.decompose_ty_app(self.db);
        let (_, source_args) = source.decompose_ty_app(self.db);
        if !source.is_array(self.db) || args.len() != 2 || source_args.len() != 2 {
            self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
            return WalkOutput::empty();
        }
        let element = ConcreteTypeView::new(args[0], source_args[0]);
        let len = match demand_concrete_array_length(self.db, args[1], source_args[1]) {
            Ok(None) => None,
            Ok(Some(len)) => {
                // A length the layout cannot count.
                let Some(len) = len.to_usize() else {
                    self.push_error(ContractLayoutError::LayoutExtentOverflow);
                    return WalkOutput::empty();
                };
                Some(len)
            }
            Err(ConcreteArrayLengthError::Invalid(cause)) => {
                self.push_error(ContractLayoutError::InvalidConcreteArrayLength {
                    invalid: TyId::invalid(self.db, cause),
                });
                return WalkOutput::empty();
            }
            Err(ConcreteArrayLengthError::Mismatch) => {
                self.push_error(ContractLayoutError::InconsistentConcreteArrayLength { array: ty });
                return WalkOutput::empty();
            }
        };
        if (len.is_none() || len == Some(0))
            && let Err(RuntimeSizeError::InvalidType(cause)) =
                runtime_size_bytes_with_source(self.db, element)
        {
            self.push_error(ContractLayoutError::InvalidConcreteArrayLength {
                invalid: TyId::invalid(self.db, cause),
            });
            return WalkOutput::empty();
        }
        let Some(len) = len else {
            self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
            return WalkOutput::empty();
        };
        if len == 0 {
            return WalkOutput::empty();
        }
        let mut element_path = path.to_vec();
        element_path.push(ContractLayoutPathSegment::ArrayElement {
            dimension: dimensions.len() as u32,
            len,
        });
        let mut element_dimensions = dimensions.to_vec();
        element_dimensions.push(len);
        let mut output = self.walk_ty(element, &element_path, &element_dimensions);
        let Some(span) = output.span.checked_mul(len) else {
            self.push_error(ContractLayoutError::LayoutExtentOverflow);
            return WalkOutput::empty();
        };
        for leaf in &mut output.leaves {
            leaf.strides[dimensions.len()] = output.span;
        }
        WalkOutput {
            span,
            leaves: output.leaves,
            packable_bytes: None,
        }
    }

    fn walk_adt(
        &mut self,
        views: ConcreteTypeView<'db>,
        adt: AdtDef<'db>,
        path: &[ContractLayoutPathSegment<'db>],
        dimensions: &[usize],
    ) -> WalkOutput<'db> {
        let ConcreteTypeView {
            canonical: ty,
            source,
        } = views;
        if adt.recursive_cycle(self.db).is_some() {
            self.push_error(ContractLayoutError::InvalidFieldType);
            return WalkOutput::empty();
        }
        let args = ty.generic_args(self.db);
        let source_args = source.generic_args(self.db);
        if source.adt_def(self.db) != Some(adt)
            || args.len() != adt.params(self.db).len()
            || source_args.len() != args.len()
        {
            self.push_error(ContractLayoutError::IncompleteAdtLayoutProjection { ty });
            return WalkOutput::empty();
        }
        let db = self.db;
        // A field's type normalized, as MIR lays out an associated type's
        // field, `<K as Tr>::Out`, as the type it names.
        let normalize = |ty| normalize_ty(db, ty, adt.scope(db), PredicateListId::empty_list(db));
        let field = |variant: usize, field: usize| {
            let view = instantiate_adt_field_for_concrete_demand(
                db,
                adt,
                variant,
                field,
                args,
                source_args,
            );
            ConcreteTypeView::new(normalize(view.canonical), normalize(view.source))
        };
        match adt.adt_ref(self.db) {
            AdtRef::Struct(struct_) => {
                let mut items = Vec::new();
                for (idx, view) in FieldParent::Struct(struct_).fields(self.db).enumerate() {
                    let Some(name) = view.name(self.db) else {
                        self.push_error(ContractLayoutError::InvalidFieldType);
                        return WalkOutput::empty();
                    };
                    let index = idx as u32;
                    items.push((
                        field(0, idx),
                        ContractLayoutPathSegment::Member { name, index },
                    ));
                }
                self.walk_sequence(items, path, dimensions, true)
            }
            AdtRef::Enum(enum_) => {
                let mut tag_path = path.to_vec();
                tag_path.push(ContractLayoutPathSegment::EnumTag);
                let mut leaves = WalkOutput::leaf(
                    self.db,
                    ty,
                    &tag_path,
                    dimensions,
                    InlineLayoutLeafKind::EnumTag,
                )
                .leaves;
                let mut max_payload = 0usize;
                for (variant_idx, variant) in adt.fields(self.db).iter().enumerate() {
                    let variant_def = EnumVariant::new(enum_, variant_idx);
                    let Some(name) = variant_def.ident(self.db) else {
                        self.push_error(ContractLayoutError::InvalidFieldType);
                        return WalkOutput::empty();
                    };
                    let mut variant_path = path.to_vec();
                    variant_path.push(ContractLayoutPathSegment::Variant {
                        name,
                        index: variant_idx as u32,
                    });
                    let mut items = Vec::with_capacity(variant.num_types());
                    for field_idx in 0..variant.num_types() {
                        let index = field_idx as u32;
                        let segment = match variant_def.kind(self.db) {
                            VariantKind::Record(_) => {
                                let Some(name) = FieldParent::Variant(variant_def)
                                    .fields(self.db)
                                    .nth(field_idx)
                                    .and_then(|field| field.name(self.db))
                                else {
                                    self.push_error(ContractLayoutError::InvalidFieldType);
                                    return WalkOutput::empty();
                                };
                                ContractLayoutPathSegment::Member { name, index }
                            }
                            VariantKind::Tuple(_) | VariantKind::Unit => {
                                ContractLayoutPathSegment::TupleElement(index)
                            }
                        };
                        items.push((field(variant_idx, field_idx), segment));
                    }
                    // Enum payloads are not packed: runtime lowering offsets
                    // payload fields by whole words.
                    let mut output = self.walk_sequence(items, &variant_path, dimensions, false);
                    max_payload = max_payload.max(output.span);
                    for leaf in &mut output.leaves {
                        let Some(offset) = leaf.offset.checked_add(1) else {
                            self.push_error(ContractLayoutError::LayoutExtentOverflow);
                            continue;
                        };
                        leaf.offset = offset;
                    }
                    leaves.extend(output.leaves);
                }
                let span = max_payload.checked_add(1).unwrap_or_else(|| {
                    self.push_error(ContractLayoutError::LayoutExtentOverflow);
                    0
                });
                WalkOutput {
                    span,
                    leaves,
                    packable_bytes: None,
                }
            }
        }
    }
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

fn affine_extent(dimensions: &[usize], strides: &[usize]) -> Option<usize> {
    dimensions
        .iter()
        .zip(strides)
        .try_fold(0usize, |offset, (dimension, stride)| {
            offset.checked_add(dimension.checked_sub(1)?.checked_mul(*stride)?)
        })?
        .checked_add(1)
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
    let declared = lower_opt_hir_ty(db, field.type_ref(), scope, assumptions);
    if declared.has_invalid(db) || ty_has_incomplete_adt_application(db, declared) {
        return Some((name, Err(vec![ContractLayoutError::InvalidFieldType])));
    }
    let address_space = if field.is_mut {
        AddressSpace::Storage
    } else {
        AddressSpace::Code
    };
    let explicit_slot = match field.slot {
        Some(_) if address_space == AddressSpace::Code => {
            return Some((name, Err(vec![ContractLayoutError::ExplicitSlotInCode])));
        }
        Some(body) => match contract_field_slot(db, body) {
            Ok(slot) => Some(slot),
            Err(_) => return Some((name, Err(vec![ContractLayoutError::InvalidExplicitSlot]))),
        },
        None => None,
    };
    let mut walker = FieldWalker {
        db,
        errors: Vec::new(),
    };
    let output = walker.walk_ty(ConcreteTypeView::identity(declared), &[], &[]);
    // Every contract field outlives the call: code-backed ones too.
    if walker.errors.is_empty()
        && let Some(pointer) = call_pointer_in_state(db, scope, declared)
    {
        walker.push_error(ContractLayoutError::CallPointerInState { pointer });
    }
    if walker.errors.is_empty()
        && let Some(invalid) = zero_invalid_in_state(db, scope, declared)
    {
        walker.push_error(ContractLayoutError::ZeroInvalidInState { invalid });
    }
    if walker.errors.is_empty()
        && let Some((entry, error)) = entry_layout_error(db, scope, declared)
    {
        walker.push_error(ContractLayoutError::EntryLayout {
            entry,
            reason: error.summary(),
        });
    }
    if walker.errors.is_empty()
        && address_space == AddressSpace::Storage
        && let Some(collection) = persistent_under_transient(db, scope, declared)
    {
        walker.push_error(ContractLayoutError::PersistentUnderTransient { collection });
    }
    if !walker.errors.is_empty() {
        return Some((name, Err(walker.errors)));
    }
    // The field takes its numbers in its own space only when some part of
    // it lies outside a pinned type: a `TSlot` field leaves its storage
    // number free. It takes them in every space its parts and its
    // collections' entries are pinned to, since each roots there at its
    // number.
    let mut spaces = Vec::new();
    if output
        .leaves
        .iter()
        .any(|leaf| leaf.kind != InlineLayoutLeafKind::Collection)
    {
        spaces.push(address_space);
    }
    for space in state_spaces(db, scope, declared) {
        if !spaces.contains(&space) {
            spaces.push(space);
        }
    }
    Some((
        name,
        Ok(ValidatedFieldLayoutPlan {
            field: ContractFieldId {
                contract,
                index: field_index,
            },
            name,
            is_mut: field.is_mut,
            address_space,
            declared,
            explicit_slot,
            slot_count: output.span,
            spaces,
            inline_leaves: output.leaves,
        }),
    ))
}

/// The slots a value of `ty` takes in storage, as the layout places a
/// contract field of its type, or why `ty` has no storage layout.
#[salsa::tracked]
pub(crate) fn storage_slot_span<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Result<usize, ContractLayoutError<'db>> {
    let mut walker = FieldWalker {
        db,
        errors: Vec::new(),
    };
    let output = walker.walk_ty(ConcreteTypeView::identity(ty), &[], &[]);
    match walker.errors.into_iter().next() {
        Some(error) => Err(error),
        None => Ok(output.span),
    }
}

/// Which fields of struct or tuple `ty` are lanes where a value of `ty` lies
/// in storage or transient storage: packed scalars that share their word or
/// lie above its low byte, as MIR's `storage_field_lane` decides. A retained
/// grant to a lane is a memory copy.
#[salsa::tracked(return_ref)]
pub(crate) fn storage_field_lanes<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Vec<bool> {
    if !ty.is_tuple(db) && !ty.adt_def(db).is_some_and(|adt| adt.is_struct(db)) {
        return Vec::new();
    }
    let mut walker = FieldWalker {
        db,
        errors: Vec::new(),
    };
    let shapes: Vec<_> = ty
        .field_types(db)
        .into_iter()
        .map(|field| {
            let output = walker.walk_ty(ConcreteTypeView::identity(field), &[], &[]);
            match output.packable_bytes {
                Some(bytes) => StorageFieldShape::Scalar { bytes },
                None => StorageFieldShape::Aggregate {
                    slots: output.span as u64,
                },
            }
        })
        .collect();
    let Ok(layout) = storage_fields_layout(shapes) else {
        return Vec::new();
    };
    layout
        .placements
        .iter()
        .map(|placement| {
            placement
                .lane
                .is_some_and(|lane| placement.shared || lane.byte_offset != 0)
        })
        .collect()
}

/// How deep the search for a persistent collection under a transient one
/// follows nested types: a collection's element type may grow without bound,
/// as in `struct G<T> { m: StorageMap<u256, G<[T; 1]>> }`.
const MAX_STATE_NESTING: usize = 64;

/// The types a value of `ty` holds in state: its fields, its tuple or array
/// elements, and a collection's entries (`StateIndex::Entry`), which lie
/// outside its own slots.
fn state_parts<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
) -> Vec<TyId<'db>> {
    let assumptions = PredicateListId::empty_list(db);
    let mut parts: Vec<_> = state_index_tys(db, scope, ty, assumptions)
        .map(|(_, entry)| entry)
        .into_iter()
        .collect();
    if let Some(adt) = ty.adt_def(db) {
        let args = ty.generic_args(db);
        parts.extend(
            adt.fields(db)
                .iter()
                .flat_map(|variant| variant.iter_types(db))
                .map(|field| field.instantiate(db, args)),
        );
    } else if ty.is_tuple(db) || ty.is_array(db) {
        parts.extend(
            ty.generic_args(db)
                .iter()
                .copied()
                .filter(|arg| !matches!(arg.data(db), TyData::ConstTy(_))),
        );
    } else if ty.as_closure(db).is_some() {
        parts.extend(ty.field_types(db));
    }
    parts
        .into_iter()
        .map(|part| normalize_ty(db, part, scope, assumptions))
        .collect()
}

/// The persistent collection `ty` keeps inside a transient collection's
/// contents, as the map of a `TSlot<StorageMap<K, V>>`, whose entries would
/// persist after the value holding them is gone.
fn persistent_under_transient<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
) -> Option<TyId<'db>> {
    fn walk<'db>(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        ty: TyId<'db>,
        mut transient: bool,
        depth: usize,
        seen: &mut FxHashSet<(TyId<'db>, bool)>,
    ) -> Option<TyId<'db>> {
        if depth > MAX_STATE_NESTING || !seen.insert((ty, transient)) {
            return None;
        }
        match pinned_space(db, scope, PredicateListId::empty_list(db), ty) {
            Some(AddressSpace::Storage) if transient => return Some(ty),
            Some(AddressSpace::Transient) => transient = true,
            _ => {}
        }
        state_parts(db, scope, ty)
            .into_iter()
            .find_map(|part| walk(db, scope, part, transient, depth + 1, seen))
    }
    walk(db, scope, ty, false, 0, &mut FxHashSet::default())
}

/// The state spaces a value of `ty` has parts pinned to, in its own slots or
/// in a collection's entries.
fn state_spaces<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
) -> Vec<AddressSpace> {
    fn walk<'db>(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        ty: TyId<'db>,
        depth: usize,
        seen: &mut FxHashSet<TyId<'db>>,
        spaces: &mut Vec<AddressSpace>,
    ) {
        if depth > MAX_STATE_NESTING || !seen.insert(ty) {
            return;
        }
        if let Some(space) = pinned_space(db, scope, PredicateListId::empty_list(db), ty)
            && !spaces.contains(&space)
        {
            spaces.push(space);
        }
        for part in state_parts(db, scope, ty) {
            walk(db, scope, part, depth + 1, seen, spaces);
        }
    }
    let mut spaces = Vec::new();
    walk(db, scope, ty, 0, &mut FxHashSet::default(), &mut spaces);
    spaces
}

/// The first memory or calldata pointer a value of `ty` keeps in state, in
/// its own slots or in a collection's entries: it would outlive the
/// allocation or the call it points into.
fn call_pointer_in_state<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
) -> Option<TyId<'db>> {
    fn walk<'db>(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        ty: TyId<'db>,
        depth: usize,
        seen: &mut FxHashSet<TyId<'db>>,
    ) -> Option<TyId<'db>> {
        if depth > MAX_STATE_NESTING || !seen.insert(ty) {
            return None;
        }
        if matches!(
            ty.as_ptr(db),
            Some((_, AddressSpace::Memory | AddressSpace::Calldata))
        ) {
            return Some(ty);
        }
        state_parts(db, scope, ty)
            .into_iter()
            .find_map(|part| walk(db, scope, part, depth + 1, seen))
    }
    walk(db, scope, ty, 0, &mut FxHashSet::default())
}

/// The first type a value of `ty` holds in state, in its own slots or in a
/// collection's entries, whose all-zero representation is not a value: one
/// with `unsafe` fields. A state space starts zeroed, so a slot no one wrote
/// would read as such a value.
fn zero_invalid_in_state<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
) -> Option<TyId<'db>> {
    fn walk<'db>(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        ty: TyId<'db>,
        depth: usize,
        seen: &mut FxHashSet<TyId<'db>>,
    ) -> Option<TyId<'db>> {
        if depth > MAX_STATE_NESTING || !seen.insert(ty) {
            return None;
        }
        if ty.adt_def(db).is_some_and(|adt| adt.has_unsafe_field(db)) {
            return Some(ty);
        }
        state_parts(db, scope, ty)
            .into_iter()
            .find_map(|part| walk(db, scope, part, depth + 1, seen))
    }
    walk(db, scope, ty, 0, &mut FxHashSet::default())
}

/// The first collection entry type a value of `ty` holds in state that has no
/// storage layout, with why: an entry is laid out like a field, at the slots
/// its collection locates.
fn entry_layout_error<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
) -> Option<(TyId<'db>, ContractLayoutError<'db>)> {
    fn walk<'db>(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        ty: TyId<'db>,
        depth: usize,
        seen: &mut FxHashSet<TyId<'db>>,
    ) -> Option<(TyId<'db>, ContractLayoutError<'db>)> {
        if depth > MAX_STATE_NESTING || !seen.insert(ty) {
            return None;
        }
        let assumptions = PredicateListId::empty_list(db);
        if let Some((_, entry)) = state_index_tys(db, scope, ty, assumptions) {
            let entry = normalize_ty(db, entry, scope, assumptions);
            if let Err(error) = storage_slot_span(db, entry) {
                return Some((entry, error));
            }
        }
        state_parts(db, scope, ty)
            .into_iter()
            .find_map(|part| walk(db, scope, part, depth + 1, seen))
    }
    walk(db, scope, ty, 0, &mut FxHashSet::default())
}

/// Places each field: a field with `#[slot(e)]` at `e`, any other at the next
/// number of its space's counter, skipping the numbers fields placed before
/// it take in any space it takes numbers in. Each space counts its own
/// fields, but every field that lies at least partly in storage or code is
/// placed before the fields that lie only in transient storage, which fit
/// around them: a transient field never moves a persistent one. A field
/// that mixes spaces takes its whole range in each, since one number roots
/// all of it.
fn allocate_contract<'db>(
    field_results: &[ContractFieldLayoutResult<'db>],
) -> Result<AllocatedContractStorageLayout<'db>, (ContractFieldId<'db>, ContractLayoutError<'db>)> {
    let plans: Vec<_> = field_results
        .iter()
        .map(|field| {
            field
                .result
                .as_ref()
                .expect("allocation requires every field to validate")
        })
        .collect();
    // The numbers placed fields take, by space: explicit fields first.
    let mut taken: FxHashMap<AddressSpace, Vec<(U256, U256, IdentId<'db>)>> = FxHashMap::default();
    for plan in &plans {
        let Some(slot) = plan.explicit_slot else {
            continue;
        };
        let end = slot
            .checked_add(U256::from(plan.slot_count))
            .ok_or((plan.field, ContractLayoutError::LayoutExtentOverflow))?;
        for space in &plan.spaces {
            let ranges = taken.entry(*space).or_default();
            if let Some(&(_, _, other)) = ranges
                .iter()
                .find(|(start, stop, _)| slot < *stop && *start < end)
            {
                return Err((
                    plan.field,
                    ContractLayoutError::ExplicitSlotOverlap { other },
                ));
            }
            if slot < end {
                ranges.push((slot, end, plan.name));
            }
        }
    }
    let transient_only =
        |plan: &ValidatedFieldLayoutPlan<'db>| plan.spaces.as_slice() == [AddressSpace::Transient];
    let mut counters: FxHashMap<AddressSpace, U256> = FxHashMap::default();
    let mut code_slot_count = 0;
    let mut offsets = vec![U256::ZERO; plans.len()];
    for transient_pass in [false, true] {
        for (idx, plan) in plans.iter().enumerate() {
            if transient_only(plan) != transient_pass {
                continue;
            }
            if let Some(slot) = plan.explicit_slot {
                offsets[idx] = slot;
                continue;
            }
            let count = U256::from(plan.slot_count);
            let space = if transient_pass {
                AddressSpace::Transient
            } else {
                plan.address_space
            };
            let counter = counters.entry(space).or_insert(U256::ZERO);
            let mut start = *counter;
            while let Some(&(_, stop, _)) = plan
                .spaces
                .iter()
                .filter_map(|space| taken.get(space))
                .flatten()
                .find(|(first, stop, _)| {
                    !count.is_zero() && start < *stop && *first < start.saturating_add(count)
                })
            {
                start = stop;
            }
            let end = start
                .checked_add(count)
                .ok_or((plan.field, ContractLayoutError::LayoutExtentOverflow))?;
            if !count.is_zero() {
                *counter = end;
                for space in &plan.spaces {
                    taken
                        .entry(*space)
                        .or_default()
                        .push((start, end, plan.name));
                }
            }
            if space == AddressSpace::Code {
                code_slot_count = usize::try_from(end)
                    .ok()
                    .filter(|end| end.checked_mul(32).is_some())
                    .ok_or((plan.field, ContractLayoutError::LayoutExtentOverflow))?;
            }
            offsets[idx] = start;
        }
    }
    let fields = plans
        .iter()
        .zip(offsets)
        .map(|(plan, slot_offset)| {
            (
                plan.name,
                FieldStorageLayout {
                    field: plan.field,
                    name: plan.name,
                    is_mut: plan.is_mut,
                    address_space: plan.address_space,
                    declared: plan.declared,
                    slot_offset,
                    slot_count: plan.slot_count,
                    inline_leaves: plan.inline_leaves.clone(),
                },
            )
        })
        .collect();
    Ok(AllocatedContractStorageLayout {
        fields,
        code_slot_count,
    })
}

fn layout_integer<'db>(db: &'db dyn HirAnalysisDb, value: U256) -> IntegerId<'db> {
    IntegerId::new(db, BigUint::from_bytes_be(&value.to_be_bytes::<32>()))
}

fn allocated_contract_layout_report<'db>(
    db: &'db dyn HirAnalysisDb,
    fields: &IndexMap<IdentId<'db>, FieldStorageLayout<'db>>,
) -> ContractLayoutReport<'db> {
    let mut entries = Vec::new();
    for field in fields.values() {
        for leaf in &field.inline_leaves {
            // Slots wrap modulo `2**256`, like Solidity's.
            let base = field.slot_offset.wrapping_add(U256::from(leaf.offset));
            let value = if leaf.dimensions.is_empty() {
                ContractLayoutValue::Scalar(layout_integer(db, base))
            } else {
                ContractLayoutValue::Indexed {
                    base: layout_integer(db, base),
                    dimensions: leaf.dimensions.clone(),
                    strides: leaf.strides.clone(),
                    extent: affine_extent(&leaf.dimensions, &leaf.strides)
                        .expect("an allocated field's leaves have a finite extent"),
                }
            };
            entries.push(ContractLayoutEntry {
                field: field.field,
                path: ContractLayoutPath {
                    field: field.name,
                    segments: leaf.path.clone(),
                },
                ty: leaf.ty,
                // A collection lies in the space it is pinned to.
                address_space: match leaf.kind {
                    InlineLayoutLeafKind::Collection => leaf.ty.pin(db),
                    InlineLayoutLeafKind::Field | InlineLayoutLeafKind::EnumTag => None,
                }
                .unwrap_or(field.address_space),
                value,
                lane: leaf.lane,
                kind: match leaf.kind {
                    InlineLayoutLeafKind::Field => ContractLayoutEntryKind::InlineField,
                    InlineLayoutLeafKind::Collection => ContractLayoutEntryKind::Collection,
                    InlineLayoutLeafKind::EnumTag => ContractLayoutEntryKind::EnumTag,
                },
            });
        }
    }
    entries.sort_by(|first, second| {
        let space_order = |space| match space {
            AddressSpace::Storage => 0,
            AddressSpace::Transient => 1,
            AddressSpace::Code => 2,
            AddressSpace::Memory => 3,
            AddressSpace::Calldata => 4,
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
        match allocate_contract(&field_results) {
            Ok(layout) => Some(layout),
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
