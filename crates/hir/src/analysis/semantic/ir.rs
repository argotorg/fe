use cranelift_entity::{EntityRef, entity_impl};
use salsa::SalsaValue;

use crate::{
    analysis::{
        semantic::instance::{SemanticInstance, SemanticInstanceKey},
        ty::{
            provider::AddressSpace,
            ty_check::{BodyOwner, EffectPassMode, ForLoopStep, LocalBinding},
            ty_def::{BorrowKind, TyId},
        },
    },
    hir_def::{BinOp, ExprId, Func, PatId, StmtId, StringId, UnOp},
    projection::{IndexSource, Projection, ProjectionPath},
    semantic::ProviderBinding,
};

use super::{consts::SemConstId, ctfe::VerifiedConstValueId};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, SalsaValue)]
pub struct FieldIndex(pub u16);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, SalsaValue)]
pub struct VariantIndex(pub u16);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, SalsaValue)]
pub struct SLocalId(u32);
entity_impl!(SLocalId);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, SalsaValue)]
pub struct SBlockId(u32);
entity_impl!(SBlockId);

/// Stable identity of one statement within a semantic body.
///
/// Constant canonicalization may replace the statement's expression, but it
/// must preserve this identity so side tables can follow the semantic
/// operation without relying on block/statement coordinates.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, SalsaValue)]
pub struct SStmtId(u32);
entity_impl!(SStmtId);

pub type SValueId = SLocalId;
pub type SemanticProjectionPath<'db> = ProjectionPath<TyId<'db>, VariantIndex, SLocalId>;

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct SemanticBody<'db> {
    pub owner: SemanticInstance<'db>,
    pub template_owner: BodyOwner<'db>,
    /// Locals initialized by the callable boundary before the entry block.
    pub entry_locals: Vec<SLocalId>,
    pub locals: Vec<SLocal<'db>>,
    pub blocks: Vec<SBlock<'db>>,
    /// The components of each unsafe split, which one session grants.
    pub unsafe_splits: Vec<Vec<SValueId>>,
}

impl<'db> SemanticBody<'db> {
    pub fn local(&self, id: SLocalId) -> Option<&SLocal<'db>> {
        self.locals.get(id.index())
    }

    pub fn block(&self, id: SBlockId) -> Option<&SBlock<'db>> {
        self.blocks.get(id.index())
    }

    pub fn callees(&self) -> Vec<SemanticCalleeRef<'db>> {
        let mut callees = Vec::new();
        for block in &self.blocks {
            for stmt in &block.stmts {
                if let SStmtKind::Assign {
                    expr: SExpr::Call { callee, .. },
                    ..
                } = &stmt.kind
                {
                    callees.push(*callee);
                }
            }
        }
        callees
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct SLocal<'db> {
    pub ty: TyId<'db>,
    pub mutability: Mutability,
    pub source: Option<LocalBinding<'db>>,
    pub role: SemanticLocalRole<'db>,
    pub snapshot_source: Option<PlaceProvenance<'db>>,
    pub layout_backing_sources: Vec<LayoutBackingSource<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum ValueProvenance<'db> {
    Ordinary,
    RootProvider(ProviderBinding<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum PlaceProvenance<'db> {
    RootProvider(ProviderBinding<'db>),
    Derived(SPlace<'db>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum LayoutBackingProjection {
    Field(FieldIndex),
    VariantField {
        variant: VariantIndex,
        field: FieldIndex,
    },
    Index(Option<usize>),
}

pub(crate) fn layout_backing_query<'db>(
    path: &SemanticProjectionPath<'db>,
) -> Option<(Vec<LayoutBackingProjection>, SemanticProjectionPath<'db>)> {
    let mut target = Vec::new();
    let mut structural_path = SemanticProjectionPath::new();
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
            Projection::Index(IndexSource::Dynamic(_) | IndexSource::Any) => {
                LayoutBackingProjection::Index(None)
            }
            Projection::Deref => continue,
            // An entry lies at a slot computed at runtime.
            Projection::Discriminant | Projection::Entry(_) => return None,
        };
        target.push(step);
        structural_path.push(projection.clone());
    }
    Some((target, structural_path))
}

/// Physical place provenance for value projections.
///
/// This keeps backing storage alive and lets borrow normalization recover a
/// projectable carrier place.
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum LayoutBackingPlace<'db> {
    Local(SPlace<'db>),
    RootProvider {
        provider: ProviderBinding<'db>,
        /// Semantic type of the provider-backed root before `path` is applied.
        ///
        /// This is intentionally distinct from `provider.provider_ty`: root-object
        /// effect keys and physical handle types need not be the type of the value
        /// projected through this place.
        value_ty: TyId<'db>,
        path: SemanticProjectionPath<'db>,
    },
}

/// Maps a projection in the current value to the place that physically backs
/// it: carrier provenance.
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct LayoutBackingSource<'db> {
    pub target: Vec<LayoutBackingProjection>,
    pub source: LayoutBackingPlace<'db>,
}

impl<'db> PlaceProvenance<'db> {
    pub fn into_layout_backing_place(self, value_ty: TyId<'db>) -> LayoutBackingPlace<'db> {
        match self {
            PlaceProvenance::RootProvider(provider) => LayoutBackingPlace::RootProvider {
                provider,
                value_ty,
                path: SemanticProjectionPath::new(),
            },
            PlaceProvenance::Derived(place) => LayoutBackingPlace::Local(place),
        }
    }

    pub fn root_provider(&self, locals: &[SLocal<'db>]) -> Option<ProviderBinding<'db>> {
        match self {
            Self::RootProvider(provider) => Some(provider.clone()),
            Self::Derived(place) => locals
                .get(place.local.index())
                .and_then(|local| local.role.root_provider(locals)),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum SemanticLocalKind {
    Erased,
    DirectValue,
    PlaceCarrier,
    PlaceBoundValue,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum SemanticLocalRole<'db> {
    Erased,
    DirectValue {
        provenance: ValueProvenance<'db>,
    },
    PlaceCarrier {
        provider: Option<ProviderBinding<'db>>,
        value_ty: TyId<'db>,
    },
    PlaceBoundValue {
        provenance: PlaceProvenance<'db>,
        value_ty: TyId<'db>,
    },
}

impl<'db> SemanticLocalRole<'db> {
    pub fn kind(&self) -> SemanticLocalKind {
        match self {
            Self::Erased => SemanticLocalKind::Erased,
            Self::DirectValue { .. } => SemanticLocalKind::DirectValue,
            Self::PlaceCarrier { .. } => SemanticLocalKind::PlaceCarrier,
            Self::PlaceBoundValue { .. } => SemanticLocalKind::PlaceBoundValue,
        }
    }

    pub fn layout_ty(&self, local_ty: TyId<'db>) -> TyId<'db> {
        match self {
            Self::PlaceCarrier { value_ty, .. } | Self::PlaceBoundValue { value_ty, .. } => {
                *value_ty
            }
            Self::Erased | Self::DirectValue { .. } => local_ty,
        }
    }

    pub fn root_provider(&self, locals: &[SLocal<'db>]) -> Option<ProviderBinding<'db>> {
        match self {
            Self::DirectValue {
                provenance: ValueProvenance::RootProvider(provider),
            } => Some(provider.clone()),
            Self::PlaceBoundValue { provenance, .. } => provenance.root_provider(locals),
            Self::PlaceCarrier {
                provider: Some(provider),
                ..
            } => Some(provider.clone()),
            Self::Erased
            | Self::DirectValue {
                provenance: ValueProvenance::Ordinary,
            }
            | Self::PlaceCarrier { provider: None, .. } => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct SBlock<'db> {
    pub stmts: Vec<SStmt<'db>>,
    pub terminator: STerminator<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct SemanticCalleeRef<'db> {
    pub key: SemanticInstanceKey<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum SemOrigin<'db> {
    Expr(ExprId),
    Stmt(StmtId),
    /// A pattern binding.
    Pat(PatId),
    Body(BodyOwner<'db>),
    Synthetic,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum CallSiteId {
    Expr(ExprId),
    ForLoop(StmtId, ForLoopStep),
}

#[salsa::interned]
#[derive(Debug)]
pub struct SemanticConstRef<'db> {
    #[returns(copy)]
    pub instance: SemanticInstanceKey<'db>,
    #[returns(copy)]
    pub ty: TyId<'db>,
    #[returns(copy)]
    pub origin: SemOrigin<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum ManualContractSection {
    Init,
    Runtime,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum SemanticCodeRegionRef<'db> {
    ManualContractRoot { func: Func<'db> },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum SemanticCodeRegionTarget<'db> {
    Resolved(SemanticCodeRegionRef<'db>),
    Deferred { arg: ExprId, ty: TyId<'db> },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct SPlace<'db> {
    pub local: SLocalId,
    pub path: SemanticProjectionPath<'db>,
}

impl<'db> SPlace<'db> {
    pub fn new(local: SLocalId) -> Self {
        Self {
            local,
            path: SemanticProjectionPath::default(),
        }
    }

    pub fn with_projection(
        local: SLocalId,
        projection: Projection<TyId<'db>, VariantIndex, SLocalId>,
    ) -> Self {
        let mut place = Self::new(local);
        place.path.push(projection);
        place
    }

    pub fn field(local: SLocalId, field: FieldIndex) -> Self {
        Self::with_projection(local, Projection::Field(field.0 as usize))
    }

    pub fn dynamic_index(local: SLocalId, index: SValueId) -> Self {
        Self::with_projection(local, Projection::Index(IndexSource::Dynamic(index)))
    }

    pub fn deref(local: SLocalId) -> Self {
        Self::with_projection(local, Projection::Deref)
    }

    pub fn variant_field(
        local: SLocalId,
        variant: VariantIndex,
        enum_ty: TyId<'db>,
        field: FieldIndex,
    ) -> Self {
        Self::with_projection(
            local,
            Projection::VariantField {
                variant,
                enum_ty,
                field_idx: field.0 as usize,
            },
        )
    }

    pub fn push_field(&mut self, field: FieldIndex) {
        self.path.push(Projection::Field(field.0 as usize));
    }

    pub fn push_dynamic_index(&mut self, index: SValueId) {
        self.path
            .push(Projection::Index(IndexSource::Dynamic(index)));
    }

    pub fn push_entry(&mut self, key: SValueId) {
        self.path.push(Projection::Entry(key));
    }

    pub fn push_deref(&mut self) {
        self.path.push(Projection::Deref);
    }
}

impl SPlace<'_> {
    /// The locals computing the place's address: its base and dynamic indices.
    pub fn used_locals(&self) -> Vec<SLocalId> {
        std::iter::once(self.local)
            .chain(self.path.iter().filter_map(|projection| match projection {
                Projection::Index(IndexSource::Dynamic(index)) | Projection::Entry(index) => {
                    Some(*index)
                }
                Projection::Field(_)
                | Projection::VariantField { .. }
                | Projection::Discriminant
                | Projection::Index(IndexSource::Constant(_))
                | Projection::Deref
                | Projection::Index(IndexSource::Any) => None,
            }))
            .collect()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct SEffectArg<'db> {
    pub binding_idx: u32,
    pub arg: SEffectArgValue<'db>,
    pub pass_mode: EffectPassMode,
    pub required_mut: bool,
    pub provider_target_ty: Option<TyId<'db>>,
    pub provider: Option<AddressSpace>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum SEffectArgValue<'db> {
    Place(SPlace<'db>),
    Value(SOperand),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum SOperandOrigin {
    Inherited,
    Expr(ExprId),
    Synthetic,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct SOperand {
    pub value: SValueId,
    pub origin: SOperandOrigin,
}

impl SOperand {
    pub fn inherited(value: SValueId) -> Self {
        Self {
            value,
            origin: SOperandOrigin::Inherited,
        }
    }

    pub fn expr(value: SValueId, expr: ExprId) -> Self {
        Self {
            value,
            origin: SOperandOrigin::Expr(expr),
        }
    }

    pub fn synthetic(value: SValueId) -> Self {
        Self {
            value,
            origin: SOperandOrigin::Synthetic,
        }
    }

    pub fn sem_origin<'db>(self, fallback: SemOrigin<'db>) -> SemOrigin<'db> {
        match self.origin {
            SOperandOrigin::Inherited => fallback,
            SOperandOrigin::Expr(expr) => SemOrigin::Expr(expr),
            SOperandOrigin::Synthetic => SemOrigin::Synthetic,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum SExpr<'db> {
    /// Passes a value on as it is, copying it: never the read of a
    /// non-`Copy` binding by value, which `UseValue` is, so it forwards values
    /// a read already took, such as a join's branch values.
    Forward(SOperand),
    UseValue(SOperand),
    ReadPlace {
        place: SPlace<'db>,
    },
    /// A read of a place's value that does not take it: the tests of a
    /// pattern whose bindings take their components where they lie.
    InspectPlace {
        place: SPlace<'db>,
    },
    CodeRegionRef {
        region: SemanticCodeRegionRef<'db>,
    },
    Const(SConst<'db>),
    Unary {
        op: UnOp,
        value: SOperand,
    },
    Binary {
        op: BinOp,
        lhs: SOperand,
        rhs: SOperand,
    },
    Cast {
        value: SOperand,
        to: TyId<'db>,
    },
    ArrayRepeat {
        ty: TyId<'db>,
        value: SOperand,
    },
    AggregateMake {
        ty: TyId<'db>,
        fields: Box<[SOperand]>,
    },
    EnumMake {
        enum_ty: TyId<'db>,
        variant: VariantIndex,
        fields: Box<[SOperand]>,
    },
    Field {
        base: SOperand,
        field: FieldIndex,
    },
    Index {
        base: SOperand,
        index: SOperand,
    },
    Borrow {
        place: SPlace<'db>,
        kind: BorrowKind,
        provider: Option<AddressSpace>,
    },
    GetEnumTag {
        value: SOperand,
    },
    IsEnumVariant {
        value: SOperand,
        variant: VariantIndex,
    },
    ExtractEnumField {
        value: SOperand,
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
        call_site: CallSiteId,
        callee: SemanticCalleeRef<'db>,
        args: Box<[SOperand]>,
        effect_args: Box<[SEffectArg<'db>]>,
    },
}

impl SExpr<'_> {
    /// The locals the expression reads, including place addresses.
    pub fn used_locals(&self) -> Vec<SLocalId> {
        let mut locals = Vec::new();
        match self {
            Self::Forward(value)
            | Self::UseValue(value)
            | Self::Unary { value, .. }
            | Self::Cast { value, .. }
            | Self::ArrayRepeat { value, .. }
            | Self::GetEnumTag { value }
            | Self::IsEnumVariant { value, .. }
            | Self::ExtractEnumField { value, .. }
            | Self::Field { base: value, .. } => locals.push(value.value),
            Self::ReadPlace { place }
            | Self::InspectPlace { place }
            | Self::Borrow { place, .. } => {
                locals.extend(place.used_locals());
            }
            Self::Binary { lhs, rhs, .. }
            | Self::Index {
                base: lhs,
                index: rhs,
            } => {
                locals.push(lhs.value);
                locals.push(rhs.value);
            }
            Self::AggregateMake { fields, .. } | Self::EnumMake { fields, .. } => {
                locals.extend(fields.iter().map(|field| field.value));
            }
            Self::Call {
                args, effect_args, ..
            } => {
                locals.extend(args.iter().map(|arg| arg.value));
                for arg in effect_args {
                    match &arg.arg {
                        SEffectArgValue::Place(place) => locals.extend(place.used_locals()),
                        SEffectArgValue::Value(value) => locals.push(value.value),
                    }
                }
            }
            Self::CodeRegionRef { .. }
            | Self::Const(_)
            | Self::CodeRegionOffset { .. }
            | Self::CodeRegionLen { .. } => {}
        }
        locals
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum SConst<'db> {
    Value(VerifiedConstValueId<'db>),
    Description(SemConstId<'db>),
    Evidence(SemConstId<'db>),
    Invalid(SemConstId<'db>),
    Ref(SemanticConstRef<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct SStmt<'db> {
    pub id: SStmtId,
    pub origin: SemOrigin<'db>,
    pub kind: SStmtKind<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum SStmtKind<'db> {
    Assign {
        dst: SLocalId,
        expr: SExpr<'db>,
    },
    Store {
        dst: SPlace<'db>,
        src: SOperand,
    },
    /// Closes the access opened by statement `access` (a borrow, a view, or
    /// a projection call's session). Liveness elaboration places it after
    /// the access's last use on each path.
    End {
        access: SStmtId,
    },
    /// Destroys what `place` still owns: its owner's scope ends, or a store
    /// is about to overwrite it. Drop elaboration decides which of its parts
    /// are initialized.
    Drop {
        place: SPlace<'db>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct STerminator<'db> {
    pub origin: SemOrigin<'db>,
    pub kind: STerminatorKind<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum STerminatorKind<'db> {
    Goto(SBlockId),
    Branch {
        cond: SOperand,
        then_bb: SBlockId,
        else_bb: SBlockId,
    },
    MatchEnum {
        value: SOperand,
        enum_ty: TyId<'db>,
        cases: Box<[(VariantIndex, SBlockId)]>,
        default: Option<SBlockId>,
    },
    Assert {
        message: Option<StringId<'db>>,
    },
    Return(Option<SOperand>),
    /// A projection grants `value` to its caller and suspends. When the
    /// caller ends the session, the slide resumes at `resume`.
    Yield {
        value: SOperand,
        resume: SBlockId,
    },
}

impl STerminatorKind<'_> {
    pub fn successors(&self) -> Vec<SBlockId> {
        match self {
            Self::Goto(block) | Self::Yield { resume: block, .. } => vec![*block],
            Self::Branch {
                then_bb, else_bb, ..
            } => vec![*then_bb, *else_bb],
            Self::MatchEnum { cases, default, .. } => cases
                .iter()
                .map(|(_, block)| *block)
                .chain(*default)
                .collect(),
            Self::Assert { .. } | Self::Return(_) => Vec::new(),
        }
    }

    /// Successors in the same order as [`Self::successors`].
    pub fn successors_mut(&mut self) -> Vec<&mut SBlockId> {
        match self {
            Self::Goto(block) | Self::Yield { resume: block, .. } => vec![block],
            Self::Branch {
                then_bb, else_bb, ..
            } => vec![then_bb, else_bb],
            Self::MatchEnum { cases, default, .. } => cases
                .iter_mut()
                .map(|(_, block)| block)
                .chain(default.as_mut())
                .collect(),
            Self::Assert { .. } | Self::Return(_) => Vec::new(),
        }
    }

    pub fn used_locals(&self) -> Vec<SLocalId> {
        match self {
            Self::Goto(_) | Self::Assert { .. } | Self::Return(None) => Vec::new(),
            Self::Branch { cond: value, .. }
            | Self::MatchEnum { value, .. }
            | Self::Return(Some(value))
            | Self::Yield { value, .. } => vec![value.value],
        }
    }
}
