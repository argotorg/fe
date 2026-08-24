use cranelift_entity::{EntityRef, entity_impl};
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        place::projectable_place_ty,
        semantic::{
            BorrowActivation, FieldIndex, LayoutBackingProjection, Mutability, SConst, SLocalId,
            SStmtId, SemOrigin, SemanticBody, SemanticCalleeRef, SemanticCodeRegionRef,
            SemanticCodeRegionTarget, SemanticLocalKind, SemanticProjectionPath, VariantIndex,
        },
        ty::{
            adt_def::{AdtRef, instantiate_adt_field_shape},
            provider::ProviderAddressSpace,
            ty_check::{BodyOwner, EffectPassMode, LocalBinding},
            ty_def::{BorrowKind, TyId},
        },
    },
    hir_def::StringId,
    projection::{IndexSource, Projection},
    semantic::ProviderBinding,
};

use super::summary::BorrowSummary;

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
    pub layout_backing_sources: Vec<NLayoutBackingSource<'db>>,
    pub root_demand: NLocalRootDemand,
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
                args, effect_args, ..
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

#[salsa::interned]
#[derive(Debug)]
pub struct BorrowSummaryId<'db> {
    #[return_ref]
    pub summary: BorrowSummary,
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
