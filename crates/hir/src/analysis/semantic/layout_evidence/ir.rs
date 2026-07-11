use cranelift_entity::entity_impl;
use salsa::Update;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        NOperand, SLocalId, SemConstId, SemConstValue, SemanticBorrowDiagnostic, SemanticCalleeRef,
        SemanticInstance,
    },
    ty::{
        CallableLayoutParamPort, LayoutBundleComponentId, LayoutBundleSchema,
        LayoutBundleSchemaError, LayoutMapTy, LayoutPortKey,
        const_ty::{CallableInputLayoutHoleOrigin, ConstTyData},
        ty_check::BodyOwner,
        ty_def::{TyData, TyId},
    },
};

pub(super) fn layout_const_param_uses<'db>(
    db: &'db dyn HirAnalysisDb,
    value: SemConstId<'db>,
) -> Vec<TyId<'db>> {
    fn collect<'db>(db: &'db dyn HirAnalysisDb, value: SemConstId<'db>, uses: &mut Vec<TyId<'db>>) {
        match value.value(db) {
            SemConstValue::TypeLevel { const_ty, .. } => {
                let TyData::ConstTy(const_ty_id) = const_ty.data(db) else {
                    return;
                };
                if matches!(const_ty_id.data(db), ConstTyData::TyParam(_, _))
                    && !uses.contains(&const_ty)
                {
                    uses.push(const_ty);
                }
            }
            SemConstValue::Tuple { elems, .. } | SemConstValue::Array { elems, .. } => {
                for element in elems {
                    collect(db, element, uses);
                }
            }
            SemConstValue::Struct { fields, .. } | SemConstValue::Enum { fields, .. } => {
                for field in fields {
                    collect(db, field, uses);
                }
            }
            SemConstValue::Unit | SemConstValue::Scalar { .. } => {}
        }
    }

    let mut uses = Vec::new();
    collect(db, value, &mut uses);
    uses
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct LayoutEvidenceLocalId(u32);
entity_impl!(LayoutEvidenceLocalId);

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct LayoutEvidenceLocal<'db> {
    pub semantic_local: Option<SLocalId>,
    pub component: LayoutBundleComponentId,
    pub map_ty: LayoutMapTy<'db>,
    pub param: Option<CallableLayoutParamPort>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum LayoutEvidenceComponentValue<'db> {
    Known(LayoutEvidenceConstant<'db>),
    Dynamic(LayoutEvidenceLocalId),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct LayoutEvidenceValue<'db> {
    pub schema: LayoutBundleSchema<'db>,
    pub components: Box<[LayoutEvidenceComponentValue<'db>]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum LayoutEvidenceBase<'db> {
    Root(TyId<'db>),
    Slot(usize),
}

/// A compile-time affine layout map. Dense maps are runtime expressions and
/// are never expanded into this constant representation.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct LayoutEvidenceConstant<'db> {
    pub map_ty: LayoutMapTy<'db>,
    pub base: LayoutEvidenceBase<'db>,
    pub strides: Box<[usize]>,
}

impl LayoutEvidenceConstant<'_> {
    pub fn rank(&self) -> usize {
        self.map_ty.rank()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum LayoutEvidenceOperand<'db> {
    Local(LayoutEvidenceLocalId),
    Constant(LayoutEvidenceConstant<'db>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum LayoutEvidenceIndex {
    Constant(usize),
    Dynamic(NOperand),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub struct LayoutEvidenceProjectionTerm {
    pub index: LayoutEvidenceIndex,
    pub len: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum LayoutEvidenceExpr<'db> {
    Use(LayoutEvidenceOperand<'db>),
    /// Select one or more outer axes. The descriptor performs affine offset
    /// arithmetic or dense lookup according to its runtime representation.
    Project {
        source: LayoutEvidenceOperand<'db>,
        terms: Box<[LayoutEvidenceProjectionTerm]>,
    },
    /// Build one array axis from independently supplied elements.
    Array {
        elements: Box<[LayoutEvidenceExpr<'db>]>,
    },
    /// Build one array axis by repeating the same complete child map.
    Repeat {
        len: usize,
        element: Box<LayoutEvidenceExpr<'db>>,
    },
    /// Functionally replace one indexed sub-map.
    Update {
        source: LayoutEvidenceOperand<'db>,
        terms: Box<[LayoutEvidenceProjectionTerm]>,
        value: Box<LayoutEvidenceExpr<'db>>,
    },
    CallResult {
        component: LayoutBundleComponentId,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct LayoutEvidenceAssignment<'db> {
    pub dst: LayoutEvidenceLocalId,
    pub expr: LayoutEvidenceExpr<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct LayoutEvidenceCallArg<'db> {
    pub target: CallableLayoutParamPort,
    pub value: LayoutEvidenceExpr<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct LayoutEvidenceCall<'db> {
    /// The semantic call this evidence belongs to. Statement coordinates alone
    /// are not sufficient phase identity: normalization must not silently pair
    /// evidence arguments with a different callee at the same coordinate.
    pub callee: SemanticCalleeRef<'db>,
    pub args: Box<[LayoutEvidenceCallArg<'db>]>,
}

/// An explicit binding from a callable's inferred const parameter to the
/// layout-map input port that supplies its runtime value.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct LayoutEvidenceConstBinding<'db> {
    /// Declaration-level const parameter supplied by this binding.
    pub param: TyId<'db>,
    pub source: CallableLayoutParamPort,
    /// Scalar layout-map value that supplies the runtime const.
    pub value: LayoutEvidenceOperand<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update, Default)]
pub struct LayoutEvidenceStatement<'db> {
    pub assignments: Box<[LayoutEvidenceAssignment<'db>]>,
    pub call: Option<LayoutEvidenceCall<'db>>,
    pub const_bindings: Box<[LayoutEvidenceConstBinding<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update, Default)]
pub struct LayoutEvidenceTerminator<'db> {
    pub returns: Box<[LayoutEvidenceReturn<'db>]>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct LayoutEvidenceReturn<'db> {
    pub component: LayoutBundleComponentId,
    pub value: LayoutEvidenceOperand<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update, Default)]
pub struct LayoutEvidenceBlock<'db> {
    pub statements: Vec<LayoutEvidenceStatement<'db>>,
    pub terminator: LayoutEvidenceTerminator<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct LayoutEvidenceBody<'db> {
    pub owner: SemanticInstance<'db>,
    pub template_owner: BodyOwner<'db>,
    pub locals: Vec<LayoutEvidenceLocal<'db>>,
    pub semantic_values: Vec<LayoutEvidenceValue<'db>>,
    pub params: Vec<LayoutEvidenceLocalId>,
    pub output: LayoutBundleSchema<'db>,
    pub blocks: Vec<LayoutEvidenceBlock<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum LayoutEvidenceError<'db> {
    Normalize(SemanticBorrowDiagnostic<'db>),
    MissingBody(BodyOwner<'db>),
    TemplateLocalCountMismatch {
        expected: usize,
        actual: usize,
    },
    InvalidSchema {
        local: Option<SLocalId>,
        error: LayoutBundleSchemaError,
    },
    DuplicateInput(CallableInputLayoutHoleOrigin),
    InvalidPlace,
    ProviderPlace,
    ShapeMismatch {
        dst: SLocalId,
        expected: usize,
        actual: usize,
    },
    MissingComponent {
        local: SLocalId,
        component: LayoutBundleComponentId,
    },
    MissingPort {
        local: SLocalId,
        port: LayoutPortKey,
    },
    ConflictingContextualSource {
        local: SLocalId,
        port: LayoutPortKey,
    },
    AmbiguousComponentBinding {
        local: SLocalId,
        component: LayoutBundleComponentId,
        sources: Box<[CallableLayoutParamPort]>,
    },
    IncompatibleComponent {
        dst: SLocalId,
        component: LayoutBundleComponentId,
    },
    MapTypeMismatch {
        dst: SLocalId,
        component: LayoutBundleComponentId,
    },
    AmbiguousConstBinding {
        param: TyId<'db>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        sources: Box<[CallableLayoutParamPort]>,
    },
    MissingConstBinding {
        param: TyId<'db>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    },
    UnprojectedConstBinding {
        param: TyId<'db>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        source: CallableLayoutParamPort,
    },
    Verify(LayoutEvidenceVerifyError),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum LayoutEvidenceVerifyError {
    OwnerMismatch,
    TemplateOwnerMismatch,
    SemanticValueCount {
        expected: usize,
        actual: usize,
    },
    BlockCount {
        expected: usize,
        actual: usize,
    },
    StatementCount {
        block: usize,
        expected: usize,
        actual: usize,
    },
    InvalidSchema {
        local: Option<SLocalId>,
        error: LayoutBundleSchemaError,
    },
    InvalidEvidenceLocal(LayoutEvidenceLocalId),
    DuplicateEvidenceLocal(LayoutEvidenceLocalId),
    OrphanEvidenceLocal(LayoutEvidenceLocalId),
    InvalidComponentValue {
        local: SLocalId,
        component: LayoutBundleComponentId,
    },
    ComponentValueCount {
        local: SLocalId,
        expected: usize,
        actual: usize,
    },
    MissingInput(CallableInputLayoutHoleOrigin),
    InvalidParams,
    OutputMismatch,
    CallPresence {
        block: usize,
        statement: usize,
    },
    CallCalleeMismatch {
        block: usize,
        statement: usize,
    },
    CallArgCount {
        block: usize,
        statement: usize,
        expected: usize,
        actual: usize,
    },
    CallResultCount {
        block: usize,
        statement: usize,
        expected: usize,
        actual: usize,
    },
    InvalidCallResult {
        block: usize,
        statement: usize,
        component: LayoutBundleComponentId,
    },
    ReturnComponentMismatch {
        block: usize,
        component: LayoutBundleComponentId,
    },
    ReturnCount {
        block: usize,
        expected: usize,
        actual: usize,
    },
    InvalidAssignmentTarget {
        block: usize,
        statement: usize,
        local: LayoutEvidenceLocalId,
    },
    AssignmentCount {
        block: usize,
        statement: usize,
        expected: usize,
        actual: usize,
    },
    InvalidOperand(LayoutEvidenceLocalId),
    InvalidIndexLocal(SLocalId),
    InvalidProjection,
    EmptyArray,
    MapTypeMismatch,
    InvalidConstBinding {
        block: usize,
        statement: usize,
    },
    UndefinedLocal {
        block: usize,
        statement: Option<usize>,
        local: LayoutEvidenceLocalId,
    },
    UndefinedIndexLocal {
        block: usize,
        statement: usize,
        local: SLocalId,
    },
}
