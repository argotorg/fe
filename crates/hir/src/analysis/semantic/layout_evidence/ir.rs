use cranelift_entity::{EntityRef, entity_impl};
use salsa::Update;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        BlockedSemanticBody, SBlockId, SLocalId, SemConstId, SemConstValue, SemanticCalleeRef,
        SemanticDiagnostic, SemanticInstance, normalized::NStatementId,
    },
    ty::{
        CallableLayoutParamPort, LayoutBundleComponentId, LayoutBundleInterface,
        LayoutBundleInterfaceError, LayoutBundleSchema, LayoutBundleSchemaError,
        LayoutBundleUnrepresentable, LayoutPortKey,
        const_ty::{CallableInputLayoutHoleOrigin, ConstTyData},
        ty_check::BodyOwner,
        ty_def::TyId,
    },
};

pub(super) fn layout_const_param_uses<'db>(
    db: &'db dyn HirAnalysisDb,
    value: SemConstId<'db>,
) -> Vec<TyId<'db>> {
    fn collect<'db>(db: &'db dyn HirAnalysisDb, value: SemConstId<'db>, uses: &mut Vec<TyId<'db>>) {
        match value.value(db) {
            SemConstValue::Description(term) => {
                let const_ty = TyId::const_ty(db, *term);
                if matches!(term.data(db), ConstTyData::TyParam(_, _)) && !uses.contains(&const_ty)
                {
                    uses.push(const_ty);
                }
            }
            SemConstValue::Tuple { elems, .. } | SemConstValue::Array { elems, .. } => {
                for element in elems {
                    collect(db, *element, uses);
                }
            }
            SemConstValue::Struct { fields, .. } | SemConstValue::Enum { fields, .. } => {
                for field in fields {
                    collect(db, *field, uses);
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
    /// The root's scalar type.
    pub ty: TyId<'db>,
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

/// A layout root known at compile time.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct LayoutEvidenceConstant<'db> {
    /// The root's scalar type.
    pub ty: TyId<'db>,
    pub base: LayoutEvidenceBase<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum LayoutEvidenceOperand<'db> {
    Local(LayoutEvidenceLocalId),
    Constant(LayoutEvidenceConstant<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum LayoutEvidenceExpr<'db> {
    Use(LayoutEvidenceOperand<'db>),
    CallResult { component: LayoutBundleComponentId },
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
    /// The semantic call this evidence belongs to. Verification must not
    /// silently pair evidence arguments with a different callee carrying the
    /// same stable statement identity.
    pub callee: SemanticCalleeRef<'db>,
    pub args: Box<[LayoutEvidenceCallArg<'db>]>,
}

/// An explicit binding from a callable's inferred const parameter to the
/// layout input port that supplies its runtime value.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct LayoutEvidenceConstBinding<'db> {
    /// Declaration-level const parameter supplied by this binding.
    pub param: TyId<'db>,
    pub source: CallableLayoutParamPort,
    /// Layout root value that supplies the runtime const.
    pub value: LayoutEvidenceOperand<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update, Default)]
pub struct LayoutEvidenceStatement<'db> {
    pub assignments: Box<[LayoutEvidenceAssignment<'db>]>,
    pub call: Option<LayoutEvidenceCall<'db>>,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct LayoutEvidenceBody<'db> {
    pub owner: SemanticInstance<'db>,
    pub template_owner: BodyOwner<'db>,
    pub locals: Vec<LayoutEvidenceLocal<'db>>,
    pub semantic_values: Vec<LayoutEvidenceValue<'db>>,
    pub params: Vec<LayoutEvidenceLocalId>,
    pub output: LayoutBundleInterface<'db>,
    /// Evidence operations indexed by stable normalized statement identity, including synthetic operations.
    pub statements: Vec<LayoutEvidenceStatement<'db>>,
    /// Constant layout inputs indexed by normalized SSA value, including synthetic constants.
    pub constant_bindings: Vec<Box<[LayoutEvidenceConstBinding<'db>]>>,
    /// Return evidence indexed by semantic block identity.
    pub terminators: Vec<LayoutEvidenceTerminator<'db>>,
}

impl<'db> LayoutEvidenceBody<'db> {
    pub fn statement(&self, id: NStatementId) -> Option<&LayoutEvidenceStatement<'db>> {
        self.statements.get(id.index())
    }

    pub fn terminator(&self, id: SBlockId) -> Option<&LayoutEvidenceTerminator<'db>> {
        self.terminators.get(id.index())
    }
}

/// A position in a callable's layout signature.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum LayoutSignaturePort {
    Input(CallableInputLayoutHoleOrigin),
    Output,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum LayoutEvidenceError<'db> {
    Blocked(BlockedSemanticBody<'db>),
    Normalize(SemanticDiagnostic<'db>),
    MissingBody(BodyOwner<'db>),
    TemplateLocalCountMismatch {
        expected: usize,
        actual: usize,
    },
    InvalidStatementIdentity(NStatementId),
    InvalidSchema {
        local: Option<SLocalId>,
        error: LayoutBundleSchemaError,
    },
    InvalidInterface {
        local: SLocalId,
        error: LayoutBundleInterfaceError,
    },
    InvalidSignature {
        port: LayoutSignaturePort,
        error: LayoutBundleInterfaceError,
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
    RootTypeMismatch {
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
    Verify(LayoutEvidenceVerifyError),
}

impl<'db> LayoutEvidenceError<'db> {
    /// The value shape that a finite layout-evidence interface cannot
    /// represent, when that is the cause of this error.
    pub fn unrepresentable(&self) -> Option<&LayoutBundleUnrepresentable> {
        match self {
            Self::InvalidSchema {
                error: LayoutBundleSchemaError::Unrepresentable(unrepresentable),
                ..
            }
            | Self::InvalidInterface {
                error:
                    LayoutBundleInterfaceError::Schema(LayoutBundleSchemaError::Unrepresentable(
                        unrepresentable,
                    )),
                ..
            }
            | Self::InvalidSignature {
                error:
                    LayoutBundleInterfaceError::Schema(LayoutBundleSchemaError::Unrepresentable(
                        unrepresentable,
                    )),
                ..
            } => Some(unrepresentable),
            _ => None,
        }
    }
}

impl From<LayoutBundleSchemaError> for LayoutEvidenceError<'_> {
    fn from(error: LayoutBundleSchemaError) -> Self {
        Self::InvalidSchema { local: None, error }
    }
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
        expected: usize,
        actual: usize,
    },
    InvalidStatementId {
        block: usize,
        statement: usize,
        id: NStatementId,
    },
    DuplicateStatementId(NStatementId),
    InvalidSchema {
        local: Option<SLocalId>,
        error: LayoutBundleSchemaError,
    },
    InvalidInterface {
        local: Option<SLocalId>,
        error: LayoutBundleInterfaceError,
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
    ConstantBindingCount {
        expected: usize,
        actual: usize,
    },
    UnmappedValue {
        block: usize,
        statement: usize,
    },
    RootTypeMismatch,
    InvalidConstBinding {
        block: usize,
        statement: usize,
    },
    UndefinedLocal {
        block: usize,
        statement: Option<usize>,
        local: LayoutEvidenceLocalId,
    },
}
