use cranelift_entity::{EntityRef, entity_impl};
use salsa::SalsaValue;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            BinaryOp, CallSiteId, FieldIndex, Mutability, SConst, SStmtId, SemConstScalar,
            SemConstValue, SemOrigin, SemanticCalleeRef, SemanticCodeRegionRef,
            SemanticCodeRegionTarget, SemanticInstance, UnaryOp, VariantIndex,
        },
        ty::{
            provider::AddressSpace,
            ty_check::{BodyOwner, EffectPassMode, LocalBinding},
            ty_def::{BorrowKind, TyId},
        },
    },
    hir_def::{ExprId, StringId},
    semantic::ProviderBinding,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NValueId(u32);
entity_impl!(NValueId);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NRootId(u32);
entity_impl!(NRootId);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, SalsaValue)]
pub struct NBlockId(u32);
entity_impl!(NBlockId);

/// Stable identity for every normalized operation, including synthetic operations.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, SalsaValue)]
pub struct NStatementId(u32);
entity_impl!(NStatementId);

#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct NormalizedBody<'db> {
    pub owner: SemanticInstance<'db>,
    pub template_owner: BodyOwner<'db>,
    pub values: Vec<NValue<'db>>,
    pub roots: Vec<NRoot<'db>>,
    pub blocks: Vec<NBlock<'db>>,
    pub entry: NBlockId,
    /// The carriers of each unsafe split's components, which one session
    /// grants: disjoint by the `unsafe` block's promise.
    pub unsafe_splits: Vec<Vec<NValueId>>,
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

    /// The block and expression that define a statement-defined value.
    pub fn defining_expr(&self, value: NValueId) -> Option<(NBlockId, &NExpr<'db>)> {
        let NValueDefinition::Statement { block, statement } = self.value(value)?.definition else {
            return None;
        };
        match &self.block(block)?.statements.get(statement as usize)?.kind {
            NStatementKind::Define { expr, .. } => Some((block, expr)),
            NStatementKind::Store { .. }
            | NStatementKind::End { .. }
            | NStatementKind::Drop { .. } => None,
        }
    }

    pub fn place_base_ty(&self, db: &'db dyn HirAnalysisDb, base: NPlaceBase) -> Option<TyId<'db>> {
        match base {
            NPlaceBase::Root(root) => Some(self.root(root)?.ty),
            NPlaceBase::CapabilityTarget { carrier } => self
                .value(carrier)?
                .ty
                .as_capability(db)
                .map(|(_, target)| target),
            NPlaceBase::Deref { pointer } => Some(self.value(pointer)?.ty.as_ptr(db)?.0),
        }
    }

    /// The type of `place` after the first `len` steps of its path.
    pub fn place_prefix_ty(
        &self,
        db: &'db dyn HirAnalysisDb,
        place: &NPlace<'db>,
        len: usize,
    ) -> Option<TyId<'db>> {
        let base_ty = self.place_base_ty(db, place.base)?;
        let prefix = NDataPath::new(place.path.iter().take(len).copied().collect::<Vec<_>>());
        super::verify::project_path_ty(db, self.owner, &self.values, base_ty, &prefix).ok()
    }

    /// The type of `value` after the first `len` steps of `path`.
    pub fn value_prefix_ty(
        &self,
        db: &'db dyn HirAnalysisDb,
        value: NValueId,
        path: &NDataPath,
        len: usize,
    ) -> Option<TyId<'db>> {
        let prefix = NDataPath::new(path.iter().take(len).copied().collect::<Vec<_>>());
        super::verify::project_path_ty(db, self.owner, &self.values, self.value(value)?.ty, &prefix)
            .ok()
    }

    /// Values required to access a place, including the value that initializes
    /// temporary storage or supplies a capability's representation root.
    pub fn place_values<'a>(
        &'a self,
        place: &'a NPlace<'db>,
    ) -> impl Iterator<Item = NValueId> + 'a {
        let base = match place.base {
            NPlaceBase::CapabilityTarget { carrier } => Some(carrier),
            NPlaceBase::Deref { pointer } => Some(pointer),
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
                NDataProjection::Index(NIndex::Value(value)) | NDataProjection::Entry(value) => {
                    Some(*value)
                }
                NDataProjection::Field(_)
                | NDataProjection::VariantField { .. }
                | NDataProjection::Index(NIndex::Const(_)) => None,
            }))
    }

    /// Test uses while allowing a consumer to refine which expression value
    /// operands it materializes. Place operands and control-flow uses always count.
    pub fn value_is_used_with(
        &self,
        target: NValueId,
        expression_uses_value: impl FnMut(&NExpr<'db>) -> bool,
    ) -> bool {
        self.value_uses(target, expression_uses_value, true)
    }

    /// Whether anything but the `End`s of the access `target` defines uses it.
    pub fn access_is_used(&self, target: NValueId) -> bool {
        self.value_uses(
            target,
            |expr| {
                let mut used = false;
                expr.for_each_value_operand(|operand| used |= operand.value == target);
                used
            },
            false,
        )
    }

    fn value_uses(
        &self,
        target: NValueId,
        mut expression_uses_value: impl FnMut(&NExpr<'db>) -> bool,
        end_uses: bool,
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
                                matches!(projection, NDataProjection::Index(NIndex::Value(value)) | NDataProjection::Entry(value) if *value == target)
                            });
                        }
                        expr.for_each_place_operand(|place| used |= place_uses_target(place));
                        used
                    }
                    NStatementKind::Store { destination, value } => {
                        value.value == target || place_uses_target(destination)
                    }
                    NStatementKind::End { access } => end_uses && *access == target,
                    NStatementKind::Drop {
                        target: NDropTarget::Value(value),
                    } => *value == target,
                    NStatementKind::Drop {
                        target: NDropTarget::Place(place),
                    } => place_uses_target(place),
                };
                if used {
                    return true;
                }
            }
            let direct_use = match &block.terminator.kind {
                NTerminatorKind::Branch { cond, .. } => cond.value == target,
                NTerminatorKind::MatchEnum { value, .. } => value.value == target,
                NTerminatorKind::Return(Some(value)) | NTerminatorKind::Yield { value, .. } => {
                    value.value == target
                }
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct NRoot<'db> {
    pub kind: NRootKind<'db>,
    pub ty: TyId<'db>,
    pub address_space: AddressSpace,
    pub mutability: Mutability,
    pub origin: SemOrigin<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct NPlace<'db> {
    pub base: NPlaceBase,
    pub path: NDataPath,
    pub ty: TyId<'db>,
    pub origin: SemOrigin<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NPlaceBase {
    Root(NRootId),
    /// The target of a capability value: a place the carrier reaches.
    CapabilityTarget {
        carrier: NValueId,
    },
    /// The pointee of a raw pointer value.
    Deref {
        pointer: NValueId,
    },
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

    pub fn as_slice(&self) -> &[NDataProjection] {
        &self.0
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
    /// The element at key `value` of a collection whose elements are places
    /// (`core::ops::StateIndex`), at a slot the collection computes.
    Entry(NValueId),
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct NEffectArg<'db> {
    pub binding_idx: u32,
    pub arg: NEffectArgValue<'db>,
    pub pass_mode: EffectPassMode,
    pub required_mut: bool,
    pub provider_target_ty: Option<TyId<'db>>,
    pub provider: Option<AddressSpace>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum NEffectArgValue<'db> {
    Place(NPlace<'db>),
    Value(NOperand),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ViewAccess {
    Read,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
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
        provider: Option<AddressSpace>,
    },
    MakeView {
        place: NPlace<'db>,
        access: ViewAccess,
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
        op: UnaryOp,
        value: NOperand,
    },
    Binary {
        op: BinaryOp,
        lhs: NOperand,
        rhs: NOperand,
    },
    /// Raw address conversion is explicit; it is not a scalar-only operation.
    PointerCast {
        value: NOperand,
        to: TyId<'db>,
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
            | Self::PointerCast { value: src, .. }
            | Self::ScalarCast { value: src, .. }
            | Self::ArrayRepeat { value: src, .. }
            | Self::GetEnumTag { value: src }
            | Self::IsEnumVariant { value: src, .. } => f(*src),
            Self::Binary { lhs, rhs, .. } => {
                f(*lhs);
                f(*rhs);
            }
            Self::AggregateMake { fields, .. } | Self::EnumMake { fields, .. } => {
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
            | Self::PointerCast { .. }
            | Self::ScalarCast { .. }
            | Self::ArrayRepeat { .. }
            | Self::AggregateMake { .. }
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct NBlock<'db> {
    pub params: Box<[NValueId]>,
    pub statements: Vec<NStatement<'db>>,
    pub terminator: NTerminator<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct NStatement<'db> {
    pub id: NStatementId,
    /// Stable raw-SMIR identity when this operation directly corresponds to a
    /// source semantic statement. Normalization-introduced loads have no raw
    /// statement identity.
    pub source: Option<SStmtId>,
    pub origin: SemOrigin<'db>,
    pub kind: NStatementKind<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum NStatementKind<'db> {
    Define {
        result: NValueId,
        expr: NExpr<'db>,
    },
    Store {
        destination: NPlace<'db>,
        value: NOperand,
    },
    /// Closes the access whose carrier `access` defines: a borrow, a view,
    /// or a projection call's session.
    End {
        access: NValueId,
    },
    /// Destroys what `target` still owns (`SStmtKind::Drop`).
    Drop {
        target: NDropTarget<'db>,
    },
}

/// What a `Drop` destroys: an owned value no place holds, or a place.
#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum NDropTarget<'db> {
    Value(NValueId),
    Place(NPlace<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NSuccessor {
    pub block: NBlockId,
    pub args: Box<[NOperand]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct NTerminator<'db> {
    pub origin: SemOrigin<'db>,
    pub kind: NTerminatorKind<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
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
    /// A projection grants `value` to its caller and suspends; its slide
    /// resumes at `resume` when the caller ends the session.
    Yield {
        value: NOperand,
        resume: NSuccessor,
    },
}

impl NTerminatorKind<'_> {
    /// The value the body hands its caller here: a returned value, or a
    /// projection's yield.
    pub fn returned(&self) -> Option<NOperand> {
        match self {
            Self::Return(Some(value)) | Self::Yield { value, .. } => Some(*value),
            Self::Goto(_)
            | Self::Branch { .. }
            | Self::MatchEnum { .. }
            | Self::Assert { .. }
            | Self::Return(None) => None,
        }
    }

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
            Self::Yield { resume, .. } => vec![resume],
            Self::Assert { .. } | Self::Return(_) => Vec::new(),
        }
    }

    /// Successors in the same order as [`Self::successors`].
    pub fn successors_mut(&mut self) -> Vec<&mut NSuccessor> {
        match self {
            Self::Goto(target) => vec![target],
            Self::Branch {
                then_target,
                else_target,
                ..
            } => vec![then_target, else_target],
            Self::MatchEnum { cases, default, .. } => cases
                .iter_mut()
                .map(|(_, target)| target)
                .chain(default.iter_mut())
                .collect(),
            Self::Yield { resume, .. } => vec![resume],
            Self::Assert { .. } | Self::Return(_) => Vec::new(),
        }
    }
}

/// Primitive scalars and raw pointers cross ordinary argument boundaries by
/// value. Their implicit frontend views do not borrow the caller's storage.
pub fn copied_scalar_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
    ty.as_view(db)
        .filter(|target| {
            target.as_ptr(db).is_some() || target.is_integral(db) || target.is_bool(db)
        })
        .unwrap_or(ty)
}

/// Whether a constant of `ty` is a dynamic string literal, whose runtime
/// representation owns a freshly allocated ABI payload.
pub fn is_literal_allocation<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    constant: &SConst<'db>,
) -> bool {
    let SConst::Value(value) = constant else {
        return false;
    };
    ty.is_core_dyn_string(db)
        && matches!(
            value.value().value(db),
            SemConstValue::Scalar {
                value: SemConstScalar::Bytes(_),
                ..
            }
        )
}
