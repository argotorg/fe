use cranelift_entity::{EntityRef, entity_impl};
use salsa::Update;

use crate::{
    analysis::{
        semantic::instance::{SemanticInstance, SemanticInstanceKey},
        ty::{
            provider::ProviderAddressSpace,
            ty_check::{BodyOwner, EffectPassMode, LocalBinding},
            ty_def::{BorrowKind, TyId},
        },
    },
    hir_def::{BinOp, ExprId, Func, StmtId, StringId, UnOp},
    projection::{IndexSource, Projection, ProjectionPath},
    semantic::ProviderBinding,
};

use super::consts::SemConstId;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct FieldIndex(pub u16);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct VariantIndex(pub u16);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct SLocalId(u32);
entity_impl!(SLocalId);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct SBlockId(u32);
entity_impl!(SBlockId);

pub type SValueId = SLocalId;
pub type SemanticProjectionPath<'db> = ProjectionPath<TyId<'db>, VariantIndex, SLocalId>;

unsafe impl<'db> Update for SemanticProjectionPath<'db> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_value = unsafe { &mut *old_pointer };
        if *old_value == new_value {
            false
        } else {
            *old_value = new_value;
            true
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct SemanticBody<'db> {
    pub owner: SemanticInstance<'db>,
    pub template_owner: BodyOwner<'db>,
    pub locals: Vec<SLocal<'db>>,
    pub blocks: Vec<SBlock<'db>>,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct SLocal<'db> {
    pub ty: TyId<'db>,
    pub mutability: Mutability,
    pub source: Option<LocalBinding<'db>>,
    pub role: SemanticLocalRole<'db>,
    pub snapshot_source: Option<PlaceProvenance<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum ValueProvenance<'db> {
    Ordinary,
    RootProvider(ProviderBinding<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum PlaceProvenance<'db> {
    RootProvider(ProviderBinding<'db>),
    Derived(SPlace<'db>),
}

impl<'db> PlaceProvenance<'db> {
    pub fn root_provider(&self, locals: &[SLocal<'db>]) -> Option<ProviderBinding<'db>> {
        match self {
            Self::RootProvider(provider) => Some(provider.clone()),
            Self::Derived(place) => locals
                .get(place.local.index())
                .and_then(|local| local.role.root_provider(locals)),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticLocalKind {
    Erased,
    DirectValue,
    PlaceCarrier,
    PlaceBoundValue,
    DirectCarrier,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
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
    DirectCarrier {
        provider: Option<ProviderBinding<'db>>,
        target_ty: TyId<'db>,
    },
}

impl<'db> SemanticLocalRole<'db> {
    pub fn kind(&self) -> SemanticLocalKind {
        match self {
            Self::Erased => SemanticLocalKind::Erased,
            Self::DirectValue { .. } => SemanticLocalKind::DirectValue,
            Self::PlaceCarrier { .. } => SemanticLocalKind::PlaceCarrier,
            Self::PlaceBoundValue { .. } => SemanticLocalKind::PlaceBoundValue,
            Self::DirectCarrier { .. } => SemanticLocalKind::DirectCarrier,
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
            Self::DirectCarrier {
                provider: Some(provider),
                ..
            } => Some(provider.clone()),
            Self::Erased
            | Self::DirectValue {
                provenance: ValueProvenance::Ordinary,
            }
            | Self::PlaceCarrier { provider: None, .. }
            | Self::DirectCarrier { provider: None, .. } => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct SBlock<'db> {
    pub stmts: Vec<SStmt<'db>>,
    pub terminator: STerminator<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub struct SemanticCalleeRef<'db> {
    pub key: SemanticInstanceKey<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemOrigin<'db> {
    Expr(ExprId),
    Stmt(StmtId),
    Body(BodyOwner<'db>),
    Synthetic,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum CallSiteId {
    Expr(ExprId),
    ForLoopLen(StmtId),
    ForLoopGet(StmtId),
}

#[salsa::interned]
#[derive(Debug)]
pub struct SemanticConstRef<'db> {
    pub instance: SemanticInstanceKey<'db>,
    pub ty: TyId<'db>,
    pub origin: SemOrigin<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum ManualContractSection {
    Init,
    Runtime,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticCodeRegionRef<'db> {
    ManualContractRoot { func: Func<'db> },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SemanticCodeRegionTarget<'db> {
    Resolved(SemanticCodeRegionRef<'db>),
    Deferred { arg: ExprId, ty: TyId<'db> },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
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

    pub fn push_deref(&mut self) {
        self.path.push(Projection::Deref);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct SEffectArg<'db> {
    pub binding_idx: u32,
    pub arg: SEffectArgValue<'db>,
    pub pass_mode: EffectPassMode,
    pub required_mut: bool,
    pub target_ty: Option<TyId<'db>>,
    pub provider: Option<ProviderAddressSpace>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SEffectArgValue<'db> {
    Place(SPlace<'db>),
    Value(SOperand),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum SOperandOrigin {
    Inherited,
    Expr(ExprId),
    Synthetic,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SExpr<'db> {
    Forward(SOperand),
    UseValue(SOperand),
    ReadPlace {
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
        provider: Option<ProviderAddressSpace>,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SConst<'db> {
    Value(SemConstId<'db>),
    Ref(SemanticConstRef<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct SStmt<'db> {
    pub origin: SemOrigin<'db>,
    pub kind: SStmtKind<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum SStmtKind<'db> {
    Assign { dst: SLocalId, expr: SExpr<'db> },
    Store { dst: SPlace<'db>, src: SOperand },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct STerminator<'db> {
    pub origin: SemOrigin<'db>,
    pub kind: STerminatorKind<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
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
}
