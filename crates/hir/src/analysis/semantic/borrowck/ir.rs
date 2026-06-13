use cranelift_entity::{EntityRef, entity_impl};
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            FieldIndex, Mutability, SConst, SLocalId, SemOrigin, SemanticBody, SemanticCalleeRef,
            SemanticCodeRegionRef, SemanticCodeRegionTarget, SemanticLocalKind,
            SemanticProjectionPath, VariantIndex,
        },
        ty::{
            provider::ProviderAddressSpace,
            ty_check::{BodyOwner, EffectPassMode, LocalBinding},
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
    pub root_demand: NLocalRootDemand,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NBorrowRoot<'db> {
    Param { local: SLocalId, param_idx: u32 },
    LocalSlot { local: SLocalId },
    Provider { binding: ProviderBinding<'db> },
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
    pub fn backing_place(&self) -> Option<&NSPlace<'db>> {
        self.lowering.place()
    }

    pub fn snapshot_source_place(&self) -> Option<&NSPlace<'db>> {
        self.facts.snapshot_source_place.as_ref()
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
    IllegalCarrierPlace {
        local: SLocalId,
        origin: SemOrigin<'db>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BorrowInputRef {
    Param(u32),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BorrowTransform<'db> {
    pub input: BorrowInputRef,
    pub proj: NSProjectionPath<'db>,
}

pub type BorrowSummary<'db> = Vec<BorrowTransform<'db>>;

#[salsa::interned]
#[derive(Debug)]
pub struct BorrowSummaryId<'db> {
    #[return_ref]
    pub items: Vec<BorrowTransform<'db>>,
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
        locals,
        blocks: Vec::new(),
        borrow_roots,
    }
}
