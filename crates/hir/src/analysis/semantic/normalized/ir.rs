use cranelift_entity::{EntityRef, entity_impl};
use salsa::Update;

use crate::{
    analysis::{
        semantic::{
            BorrowActivation, CallSiteId, FieldIndex, Mutability, SConst, SStmtId, SemOrigin,
            SemanticCalleeRef, SemanticCodeRegionRef, SemanticCodeRegionTarget, SemanticInstance,
            VariantIndex, capability::handle::OpaqueHandleContract,
        },
        ty::{
            provider::ProviderAddressSpace,
            ty_check::{BodyOwner, EffectPassMode, LocalBinding},
            ty_def::{BorrowKind, TyId},
        },
    },
    hir_def::{BinOp, ExprId, StringId, UnOp},
    semantic::ProviderBinding,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NValueId(u32);
entity_impl!(NValueId);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NRootId(u32);
entity_impl!(NRootId);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NBlockId(u32);
entity_impl!(NBlockId);

/// Stable identity for every normalized operation, including synthetic operations.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct NStatementId(u32);
entity_impl!(NStatementId);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NormalizedBody<'db> {
    pub owner: SemanticInstance<'db>,
    pub template_owner: BodyOwner<'db>,
    pub values: Vec<NValue<'db>>,
    pub roots: Vec<NRoot<'db>>,
    pub blocks: Vec<NBlock<'db>>,
    pub entry: NBlockId,
}

impl<'db> NormalizedBody<'db> {
    pub fn value(&self, id: NValueId) -> Option<&NValue<'db>> {
        self.values.get(id.index())
    }

    pub fn root(&self, id: NRootId) -> Option<&NRoot<'db>> {
        self.roots.get(id.index())
    }

    pub fn block(&self, id: NBlockId) -> Option<&NBlock<'db>> {
        self.blocks.get(id.index())
    }

    pub fn place_base_ty(
        &self,
        db: &'db dyn crate::analysis::HirAnalysisDb,
        base: NPlaceBase,
    ) -> Option<TyId<'db>> {
        match base {
            NPlaceBase::Root(root) => Some(self.root(root)?.ty),
            NPlaceBase::CapabilityTarget { carrier } => self
                .value(carrier)?
                .ty
                .as_capability(db)
                .map(|(_, target)| target),
        }
    }

    /// Values required to access a place, including the value that initializes
    /// temporary storage or supplies a handle's representation root.
    pub fn place_values<'a>(
        &'a self,
        place: &'a NPlace<'db>,
    ) -> impl Iterator<Item = NValueId> + 'a {
        let base = match place.base {
            NPlaceBase::CapabilityTarget { carrier } => Some(carrier),
            NPlaceBase::Root(root) => self.root(root).and_then(|root| match root.kind {
                NRootKind::CapabilityRepresentation { carrier }
                | NRootKind::Temporary { value: carrier } => Some(carrier),
                NRootKind::LocalSlot { .. }
                | NRootKind::ParamPlace { .. }
                | NRootKind::Provider { .. } => None,
            }),
        };
        base.into_iter()
            .chain(place.path.iter().filter_map(|projection| match projection {
                NDataProjection::Index(NIndex::Value(value)) => Some(*value),
                NDataProjection::Field(_)
                | NDataProjection::VariantField { .. }
                | NDataProjection::Index(NIndex::Const(_)) => None,
            }))
    }

    pub(crate) fn value_is_used(&self, target: NValueId) -> bool {
        self.value_is_used_with(target, |expr| {
            let mut used = false;
            expr.for_each_value_operand(|operand| used |= operand.value == target);
            used
        })
    }

    /// Test uses while allowing a consumer to refine which expression value
    /// operands it materializes. Place operands and control-flow uses always count.
    pub fn value_is_used_with(
        &self,
        target: NValueId,
        mut expression_uses_value: impl FnMut(&NExpr<'db>) -> bool,
    ) -> bool {
        let place_uses_target =
            |place: &NPlace<'db>| self.place_values(place).any(|value| value == target);
        for block in &self.blocks {
            for statement in &block.statements {
                let used = match &statement.kind {
                    NStatementKind::Define { expr, .. } => {
                        let mut used = expression_uses_value(expr);
                        if let NExpr::ProjectValue { path, .. } = expr {
                            used |= path.0.iter().any(|projection| {
                                matches!(projection, NDataProjection::Index(NIndex::Value(value)) if *value == target)
                            });
                        }
                        expr.for_each_place_operand(|place| used |= place_uses_target(place));
                        used
                    }
                    NStatementKind::Store { destination, value } => {
                        value.value == target || place_uses_target(destination)
                    }
                };
                if used {
                    return true;
                }
            }
            let direct_use = match &block.terminator.kind {
                NTerminatorKind::Branch { cond, .. } => cond.value == target,
                NTerminatorKind::MatchEnum { value, .. } => value.value == target,
                NTerminatorKind::Return(Some(value)) => value.value == target,
                NTerminatorKind::Goto(_)
                | NTerminatorKind::Assert { .. }
                | NTerminatorKind::Return(None) => false,
            };
            if direct_use
                || block
                    .terminator
                    .kind
                    .successors()
                    .into_iter()
                    .flat_map(|successor| &successor.args)
                    .any(|argument| argument.value == target)
            {
                return true;
            }
        }
        false
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NValue<'db> {
    pub ty: TyId<'db>,
    pub mutability: Mutability,
    pub origin: SemOrigin<'db>,
    pub definition: NValueDefinition,
    /// Source binding identity is diagnostic metadata, never value identity.
    pub source: Option<LocalBinding<'db>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NValueDefinition {
    EntryParam { param: u32 },
    BlockParam { block: NBlockId, index: u32 },
    Statement { block: NBlockId, statement: u32 },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NRoot<'db> {
    pub kind: NRootKind<'db>,
    pub ty: TyId<'db>,
    pub address_space: ProviderAddressSpace,
    pub mutability: Mutability,
    pub origin: SemOrigin<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NRootKind<'db> {
    LocalSlot {
        binding: Option<LocalBinding<'db>>,
    },
    ParamPlace {
        param: u32,
    },
    Provider {
        binding: ProviderBinding<'db>,
    },
    CapabilityRepresentation {
        carrier: NValueId,
    },
    /// Addressable storage initialized from a normalized temporary value.
    Temporary {
        value: NValueId,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NPlace<'db> {
    pub base: NPlaceBase,
    pub path: NDataPath,
    pub ty: TyId<'db>,
    pub origin: SemOrigin<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NPlaceBase {
    Root(NRootId),
    CapabilityTarget { carrier: NValueId },
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct NDataPath(Box<[NDataProjection]>);

impl NDataPath {
    pub fn new(projections: impl Into<Box<[NDataProjection]>>) -> Self {
        Self(projections.into())
    }

    pub fn empty() -> Self {
        Self::default()
    }

    pub fn iter(&self) -> impl Iterator<Item = &NDataProjection> {
        self.0.iter()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn appended(&self, projection: NDataProjection) -> Self {
        let mut projections = self.0.to_vec();
        projections.push(projection);
        Self(projections.into_boxed_slice())
    }

    pub fn concat(&self, suffix: &Self) -> Self {
        let mut projections = self.0.to_vec();
        projections.extend_from_slice(&suffix.0);
        Self(projections.into_boxed_slice())
    }

    pub fn push(&mut self, projection: NDataProjection) {
        *self = self.appended(projection);
    }

    pub fn is_prefix_of(&self, other: &Self) -> bool {
        self.len() <= other.len() && self.iter().zip(other.iter()).all(|(lhs, rhs)| lhs == rhs)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NDataProjection {
    Field(FieldIndex),
    VariantField {
        variant: VariantIndex,
        field: FieldIndex,
    },
    Index(NIndex),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NIndex {
    Const(usize),
    Value(NValueId),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NStructuralPath(pub NDataPath);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ReadMode {
    Copy,
    Read,
    Move,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NOperand {
    pub value: NValueId,
    pub origin: Option<ExprId>,
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
    Place(NPlace<'db>),
    Value(NOperand),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ViewAccess {
    Read,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum HandleOrigin<'db> {
    Provider(ProviderBinding<'db>),
    Opaque(OpaqueHandleContract<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NExpr<'db> {
    Forward {
        src: NOperand,
    },
    ProjectValue {
        value: NOperand,
        path: NStructuralPath,
    },
    Load {
        place: NPlace<'db>,
        mode: ReadMode,
    },
    Borrow {
        place: NPlace<'db>,
        kind: BorrowKind,
        activation: BorrowActivation<'db>,
        provider: Option<ProviderAddressSpace>,
    },
    MakeView {
        place: NPlace<'db>,
        access: ViewAccess,
    },
    MakeHandle {
        ty: TyId<'db>,
        variant: Option<VariantIndex>,
        fields: Box<[NOperand]>,
        origin: HandleOrigin<'db>,
    },
    StructuralRepack {
        value: NOperand,
        mapping: StructuralRepack,
    },
    CodeRegionRef {
        region: SemanticCodeRegionRef<'db>,
    },
    Const(SConst<'db>),
    Unary {
        op: UnOp,
        value: NOperand,
    },
    Binary {
        op: BinOp,
        lhs: NOperand,
        rhs: NOperand,
    },
    ScalarCast {
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
    CodeRegionOffset {
        target: SemanticCodeRegionTarget<'db>,
    },
    CodeRegionLen {
        target: SemanticCodeRegionTarget<'db>,
    },
    Call {
        call_site: CallSiteId,
        callee: SemanticCalleeRef<'db>,
        args: Box<[NOperand]>,
        effect_args: Box<[NEffectArg<'db>]>,
    },
}

impl<'db> NExpr<'db> {
    pub fn for_each_value_operand(&self, mut f: impl FnMut(NOperand)) {
        match self {
            Self::Forward { src }
            | Self::ProjectValue { value: src, .. }
            | Self::StructuralRepack { value: src, .. }
            | Self::Unary { value: src, .. }
            | Self::ScalarCast { value: src, .. }
            | Self::ArrayRepeat { value: src, .. }
            | Self::GetEnumTag { value: src }
            | Self::IsEnumVariant { value: src, .. } => f(*src),
            Self::Binary { lhs, rhs, .. } => {
                f(*lhs);
                f(*rhs);
            }
            Self::AggregateMake { fields, .. }
            | Self::EnumMake { fields, .. }
            | Self::MakeHandle { fields, .. } => {
                fields.iter().copied().for_each(f);
            }
            Self::Call {
                args, effect_args, ..
            } => {
                args.iter().copied().for_each(&mut f);
                effect_args
                    .iter()
                    .filter_map(|arg| match arg.arg {
                        NEffectArgValue::Value(value) => Some(value),
                        NEffectArgValue::Place(_) => None,
                    })
                    .for_each(f);
            }
            Self::Load { .. }
            | Self::Borrow { .. }
            | Self::MakeView { .. }
            | Self::CodeRegionRef { .. }
            | Self::Const(_)
            | Self::CodeRegionOffset { .. }
            | Self::CodeRegionLen { .. } => {}
        }
    }

    pub fn for_each_place_operand(&self, mut f: impl FnMut(&NPlace<'db>)) {
        match self {
            Self::Load { place, .. }
            | Self::Borrow { place, .. }
            | Self::MakeView { place, .. } => f(place),
            Self::Call { effect_args, .. } => effect_args
                .iter()
                .filter_map(|arg| match &arg.arg {
                    NEffectArgValue::Place(place) => Some(place),
                    NEffectArgValue::Value(_) => None,
                })
                .for_each(f),
            Self::Forward { .. }
            | Self::ProjectValue { .. }
            | Self::StructuralRepack { .. }
            | Self::CodeRegionRef { .. }
            | Self::Const(_)
            | Self::Unary { .. }
            | Self::Binary { .. }
            | Self::ScalarCast { .. }
            | Self::ArrayRepeat { .. }
            | Self::AggregateMake { .. }
            | Self::MakeHandle { .. }
            | Self::EnumMake { .. }
            | Self::GetEnumTag { .. }
            | Self::IsEnumVariant { .. }
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

    pub fn try_for_each_place_operand<E>(
        &self,
        mut f: impl FnMut(&NPlace<'db>) -> Result<(), E>,
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
pub struct StructuralRepack {
    pub fields: Box<[(NDataPath, NDataPath)]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NBlock<'db> {
    pub params: Box<[NValueId]>,
    pub statements: Vec<NStatement<'db>>,
    pub terminator: NTerminator<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NStatement<'db> {
    pub id: NStatementId,
    /// Stable raw-SMIR identity when this operation directly corresponds to a
    /// source semantic statement. Normalization-introduced loads have no raw
    /// statement identity.
    pub source: Option<SStmtId>,
    pub origin: SemOrigin<'db>,
    pub kind: NStatementKind<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NStatementKind<'db> {
    Define {
        result: NValueId,
        expr: NExpr<'db>,
    },
    Store {
        destination: NPlace<'db>,
        value: NOperand,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NSuccessor {
    pub block: NBlockId,
    pub args: Box<[NOperand]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NTerminator<'db> {
    pub origin: SemOrigin<'db>,
    pub kind: NTerminatorKind<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NTerminatorKind<'db> {
    Goto(NSuccessor),
    Branch {
        cond: NOperand,
        then_target: NSuccessor,
        else_target: NSuccessor,
    },
    MatchEnum {
        value: NOperand,
        enum_ty: TyId<'db>,
        cases: Box<[(VariantIndex, NSuccessor)]>,
        default: Option<NSuccessor>,
    },
    Assert {
        message: Option<StringId<'db>>,
    },
    Return(Option<NOperand>),
}

impl NTerminatorKind<'_> {
    pub fn successors(&self) -> Vec<&NSuccessor> {
        match self {
            Self::Goto(target) => vec![target],
            Self::Branch {
                then_target,
                else_target,
                ..
            } => vec![then_target, else_target],
            Self::MatchEnum { cases, default, .. } => cases
                .iter()
                .map(|(_, target)| target)
                .chain(default.iter())
                .collect(),
            Self::Assert { .. } | Self::Return(_) => Vec::new(),
        }
    }
}
