use cranelift_entity::EntityRef;
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{One, ToPrimitive, Zero};
use ruint::aliases::U256;
use rustc_hash::FxHashMap;
use salsa::Update;
use std::rc::Rc;
use tiny_keccak::{Hasher, Keccak};

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::instance::{
            GenericSubst, ImplEnv, SemanticInstance, SemanticInstanceKey,
            get_or_build_semantic_instance,
        },
        semantic::{
            BlockedInfo, ConstDemandKind, ConstDependency, EvalFailure, EvalOutcome, FieldIndex,
            PrimitiveFault, SConst, SEffectArgValue, SExpr, SLocalId, SOperand, SPlace, SStmt,
            SStmtKind, STerminatorKind, SemConstId, SemConstScalar, SemConstValue, SemOrigin,
            SemanticBody, SemanticConstRef, VariantIndex, array_const, bool_const, bytes_const,
            consts::instantiate_const_template, enum_const, execute_scalar_cast,
            execute_source_int_binary, execute_source_int_unary, instantiate_with_generic_args,
            int_const, int_in_range, int_ty_shape, normalize_int_to_shape, runtime_size_bytes,
            sem_const_eq, sem_const_from_ty, sem_const_ty, struct_const, tuple_const, unit_const,
        },
        ty::{
            const_check::const_effects_supported,
            const_ty::{ConstTyData, ConstTyId},
            corelib::{
                CtfeExternIntrinsic, NumericExternIntrinsic, PrimitiveWrapperCallKind,
                SaturatingArithmetic, core_primitive_wrapper_call_kind, ctfe_extern_intrinsic_kind,
            },
            normalize::normalize_ty,
            provider::ProviderAddressSpace,
            ty_check::{
                BodyOwner, EffectArgLayoutView, EffectParamSite, EffectPassMode, LocalBinding,
                ParamSite,
            },
            ty_def::{PrimTy, TyBase, TyData, TyId},
        },
    },
    core::hir_def::expr::LogicalBinOp,
    hir_def::{ArithBinOp, BinOp, CompBinOp, UnOp, attr::ArithmeticMode},
    projection::{IndexSource, Projection},
};

use super::{
    outcome::{EvalResult, EvalStop, FoldAttempt, FoldMissReason},
    request::VerifiedConstValueId,
    service::force_const_term_value_with_steps,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct CtfeConfig {
    pub step_limit: usize,
    pub recursion_limit: usize,
}

impl Default for CtfeConfig {
    fn default() -> Self {
        Self {
            step_limit: 1_000_000,
            recursion_limit: 64,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum CtfeError<'db> {
    NotConstEvaluable {
        origin: SemOrigin<'db>,
    },
    AssertionFailed {
        origin: SemOrigin<'db>,
        message: Option<String>,
    },
    InvalidOperation {
        origin: SemOrigin<'db>,
        message: String,
    },
    InvalidBorrow {
        origin: SemOrigin<'db>,
    },
    InvalidProviderUse {
        origin: SemOrigin<'db>,
    },
    NonConstCall {
        origin: SemOrigin<'db>,
    },
    InvalidBody {
        origin: SemOrigin<'db>,
    },
    DivisionByZero {
        origin: SemOrigin<'db>,
    },
    ArithmeticOverflow {
        origin: SemOrigin<'db>,
    },
    NegativeExponent {
        origin: SemOrigin<'db>,
    },
    OutOfBounds {
        origin: SemOrigin<'db>,
    },
    VariantMismatch {
        origin: SemOrigin<'db>,
    },
    UninitializedLocal {
        origin: SemOrigin<'db>,
    },
    StepLimitExceeded {
        origin: SemOrigin<'db>,
    },
    RecursionLimitExceeded {
        origin: SemOrigin<'db>,
    },
    /// Evaluating the const required its own value (directly or through a
    /// cycle of const items). Produced as the fixpoint-initial value of the
    /// eval queries below, so a recursive definition converges to this error
    /// instead of panicking on the salsa dependency cycle.
    RecursiveConst {
        origin: SemOrigin<'db>,
    },
    CalleeError {
        origin: SemOrigin<'db>,
        callee: SemanticInstance<'db>,
        source: Box<CtfeError<'db>>,
    },
}

impl<'db> CtfeError<'db> {
    /// Whether the root error (through any `CalleeError` chain) is a
    /// recursive-const error.
    pub fn root_is_recursive_const(&self) -> bool {
        match self {
            CtfeError::RecursiveConst { .. } => true,
            CtfeError::CalleeError { source, .. } => source.root_is_recursive_const(),
            _ => false,
        }
    }
}

#[derive(Clone, Copy)]
enum EvmModularArithmetic {
    Add,
    Mul,
}

// Const-item references are evaluated through these salsa queries (the
// machine calls `eval_const_ref` when it hits an `SConst::Ref`), so a
// recursive const definition is a salsa dependency cycle, not a deep machine
// stack. Each query recovers with `Err(RecursiveConst)` as the fixpoint
// initial value; the machine's ref site keeps that error shape stable across
// iterations (see `SExpr::Const(SConst::Ref(..))` in `eval_expr`), so the
// cycle converges to a recursion error instead of panicking.
#[salsa::tracked(cycle_initial=eval_const_instance_cycle_initial, cycle_fn=eval_const_instance_cycle_recover)]
pub fn eval_const_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> EvalOutcome<'db, SemConstId<'db>> {
    let mut machine = CtfeMachine::new(db, CtfeConfig::default());
    machine
        .eval_root(
            instance,
            Vec::new(),
            SemOrigin::Body(instance.key(db).owner(db)),
        )
        .into()
}

fn eval_const_instance_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> EvalOutcome<'db, SemConstId<'db>> {
    EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::RecursiveConst {
        origin: SemOrigin::Body(instance.key(db).owner(db)),
    }))
}

fn eval_const_instance_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &EvalOutcome<'db, SemConstId<'db>>,
    _count: u32,
    _instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<EvalOutcome<'db, SemConstId<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

#[salsa::tracked(cycle_initial=eval_const_ref_cycle_initial, cycle_fn=eval_const_ref_cycle_recover)]
pub fn eval_const_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    cref: SemanticConstRef<'db>,
) -> EvalOutcome<'db, SemConstId<'db>> {
    let mut machine = CtfeMachine::new(db, CtfeConfig::default());
    machine
        .eval_root(
            SemanticInstance::new(db, cref.instance(db)),
            Vec::new(),
            cref.origin(db),
        )
        .into()
}

fn eval_const_ref_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    cref: SemanticConstRef<'db>,
) -> EvalOutcome<'db, SemConstId<'db>> {
    EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::RecursiveConst {
        origin: cref.origin(db),
    }))
}

fn eval_const_ref_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &EvalOutcome<'db, SemConstId<'db>>,
    _count: u32,
    _cref: SemanticConstRef<'db>,
) -> salsa::CycleRecoveryAction<EvalOutcome<'db, SemConstId<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

#[salsa::tracked(cycle_initial=eval_body_owner_const_cycle_initial, cycle_fn=eval_body_owner_const_cycle_recover)]
pub fn eval_body_owner_const<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    generic_args: Vec<crate::analysis::ty::ty_def::TyId<'db>>,
) -> EvalOutcome<'db, SemConstId<'db>> {
    let key = SemanticInstanceKey::new(
        db,
        owner,
        GenericSubst::new(db, generic_args),
        crate::analysis::semantic::EffectProviderSubst::empty(db),
        ImplEnv::empty(db, owner.scope()),
    );
    eval_const_instance(db, get_or_build_semantic_instance(db, key))
}

fn eval_body_owner_const_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    _generic_args: Vec<crate::analysis::ty::ty_def::TyId<'db>>,
) -> EvalOutcome<'db, SemConstId<'db>> {
    EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::RecursiveConst {
        origin: SemOrigin::Body(owner),
    }))
}

fn eval_body_owner_const_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &EvalOutcome<'db, SemConstId<'db>>,
    _count: u32,
    _owner: BodyOwner<'db>,
    _generic_args: Vec<crate::analysis::ty::ty_def::TyId<'db>>,
) -> salsa::CycleRecoveryAction<EvalOutcome<'db, SemConstId<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

#[salsa::tracked(cycle_initial=eval_body_owner_const_with_args_cycle_initial, cycle_fn=eval_body_owner_const_with_args_cycle_recover)]
pub fn eval_body_owner_const_with_args<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    generic_args: Vec<crate::analysis::ty::ty_def::TyId<'db>>,
    args: Vec<SemConstId<'db>>,
) -> EvalOutcome<'db, SemConstId<'db>> {
    let key = SemanticInstanceKey::new(
        db,
        owner,
        GenericSubst::new(db, generic_args),
        crate::analysis::semantic::EffectProviderSubst::empty(db),
        ImplEnv::empty(db, owner.scope()),
    );
    execute_resolved_const_computation(db, key, args, CtfeConfig::default(), SemOrigin::Body(owner))
}

pub(super) fn execute_resolved_const_computation<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    args: Vec<SemConstId<'db>>,
    config: CtfeConfig,
    origin: SemOrigin<'db>,
) -> EvalOutcome<'db, SemConstId<'db>> {
    execute_resolved_const_computation_with_steps(db, key, args, config, origin, &mut 0)
}

pub(super) fn execute_resolved_const_computation_with_steps<'db>(
    db: &'db dyn HirAnalysisDb,
    key: SemanticInstanceKey<'db>,
    args: Vec<SemConstId<'db>>,
    config: CtfeConfig,
    origin: SemOrigin<'db>,
    steps: &mut usize,
) -> EvalOutcome<'db, SemConstId<'db>> {
    let mut machine = CtfeMachine::new(db, config);
    machine.steps = *steps;
    let args = args
        .into_iter()
        .map(|arg| machine.load_sem_const(arg, origin))
        .collect::<EvalResult<'db, Vec<_>>>();
    let outcome = match args {
        Ok(args) => machine
            .eval_root(get_or_build_semantic_instance(db, key), args, origin)
            .into(),
        Err(stop) => EvalOutcome::from(Err(stop)),
    };
    *steps = machine.steps;
    outcome
}

fn eval_body_owner_const_with_args_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    _generic_args: Vec<crate::analysis::ty::ty_def::TyId<'db>>,
    _args: Vec<SemConstId<'db>>,
) -> EvalOutcome<'db, SemConstId<'db>> {
    EvalOutcome::Failed(EvalFailure::Ctfe(CtfeError::RecursiveConst {
        origin: SemOrigin::Body(owner),
    }))
}

fn eval_body_owner_const_with_args_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &EvalOutcome<'db, SemConstId<'db>>,
    _count: u32,
    _owner: BodyOwner<'db>,
    _generic_args: Vec<crate::analysis::ty::ty_def::TyId<'db>>,
    _args: Vec<SemConstId<'db>>,
) -> salsa::CycleRecoveryAction<EvalOutcome<'db, SemConstId<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

pub(super) fn attempt_optional_const_fold<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &SemanticBody<'db>,
    result_ty: TyId<'db>,
    expr: &SExpr<'db>,
    locals: &[Option<SemConstId<'db>>],
    origin: SemOrigin<'db>,
) -> FoldAttempt<'db> {
    let mut machine = CtfeMachine::new(db, CtfeConfig::default());
    match machine.eval_expr_with_locals(body, result_ty, expr.clone(), locals, origin) {
        Ok(value) if sem_const_dependency(db, value).is_none() => {
            match VerifiedConstValueId::from_complete_execution(db, value) {
                Ok(value) => FoldAttempt::Folded(value),
                Err(message) => FoldAttempt::InvariantFailure(EvalFailure::Invariant {
                    origin,
                    message: message.into(),
                }),
            }
        }
        Ok(_) => FoldAttempt::InvariantFailure(EvalFailure::Invariant {
            origin,
            message: "optional CTFE fold completed with a dependent value".into(),
        }),
        Err(EvalStop::Blocked(info)) => FoldAttempt::NotFoldable(FoldMissReason::Dependent(info)),
        Err(EvalStop::Failed(EvalFailure::Ctfe(CtfeError::UninitializedLocal { .. }))) => {
            FoldAttempt::NotFoldable(FoldMissReason::UnknownRuntimeInput)
        }
        Err(EvalStop::Failed(EvalFailure::Ctfe(err))) => {
            FoldAttempt::NotFoldable(FoldMissReason::ReachedFailure(err))
        }
        Err(EvalStop::Failed(failure)) => FoldAttempt::InvariantFailure(failure),
    }
}

struct CtfeMachine<'db, 'body> {
    db: &'db dyn HirAnalysisDb,
    config: CtfeConfig,
    steps: usize,
    instance_cache: FxHashMap<SemanticInstanceKey<'db>, SemanticInstance<'db>>,
    frames: Vec<CtfeFrame<'db, 'body>>,
    /// Memoized results of const-item references evaluated by this machine.
    const_results: FxHashMap<SemanticInstanceKey<'db>, EvalResult<'db, SemConstId<'db>>>,
    /// Const items currently being evaluated, outermost first. A reference to
    /// a const already on this stack is a recursive definition; the machine
    /// owns this check so const recursion never becomes a salsa query cycle.
    const_stack: Vec<SemanticInstanceKey<'db>>,
}

struct CtfeFrame<'db, 'body> {
    body: &'body SemanticBody<'db>,
    locals: Vec<CtfeSlot<'db>>,
    current: usize,
}

#[derive(Clone)]
enum CtfeSlot<'db> {
    Uninit,
    Init(CtfeValue<'db>),
}

#[derive(Clone)]
enum CtfeValue<'db> {
    Value(CtfeConstValue<'db>),
    Ref(CtfeRef),
}

#[derive(Clone)]
struct CtfeConstValue<'db> {
    kind: CtfeConstKind<'db>,
}

#[derive(Clone)]
enum CtfeConstKind<'db> {
    Interned(VerifiedConstValueId<'db>),
    Unit,
    Bool(bool),
    Int {
        ty: TyId<'db>,
        value: CtfeInt,
    },
    Bytes {
        ty: TyId<'db>,
        bytes: Rc<[u8]>,
    },
    Tuple {
        ty: TyId<'db>,
        elems: Rc<[CtfeConstValue<'db>]>,
    },
    Struct {
        ty: TyId<'db>,
        fields: Rc<[CtfeConstValue<'db>]>,
    },
    Array {
        ty: TyId<'db>,
        elems: Rc<[CtfeConstValue<'db>]>,
    },
    Enum {
        ty: TyId<'db>,
        variant: VariantIndex,
        fields: Rc<[CtfeConstValue<'db>]>,
    },
}

#[derive(Clone)]
enum CtfeInt {
    Word { bits: u16, signed: bool, word: U256 },
    Big(BigInt),
}

impl<'db> CtfeConstValue<'db> {
    fn unit() -> Self {
        Self {
            kind: CtfeConstKind::Unit,
        }
    }

    fn bool(value: bool) -> Self {
        Self {
            kind: CtfeConstKind::Bool(value),
        }
    }

    fn int(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, value: BigInt) -> Self {
        Self {
            kind: CtfeConstKind::Int {
                ty,
                value: CtfeInt::from_bigint(db, ty, value),
            },
        }
    }

    fn int_word(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, word: U256) -> Self {
        let value = match int_ty_shape(db, ty) {
            Some((bits, signed)) => CtfeInt::from_word(bits, signed, word),
            None => CtfeInt::Big(bigint_from_u256(word)),
        };
        Self {
            kind: CtfeConstKind::Int { ty, value },
        }
    }

    fn bytes(ty: TyId<'db>, bytes: Vec<u8>) -> Self {
        Self {
            kind: CtfeConstKind::Bytes {
                ty,
                bytes: bytes.into(),
            },
        }
    }

    fn tuple(ty: TyId<'db>, elems: Vec<CtfeConstValue<'db>>) -> Self {
        Self {
            kind: CtfeConstKind::Tuple {
                ty,
                elems: elems.into(),
            },
        }
    }

    fn struct_(ty: TyId<'db>, fields: Vec<CtfeConstValue<'db>>) -> Self {
        Self {
            kind: CtfeConstKind::Struct {
                ty,
                fields: fields.into(),
            },
        }
    }

    fn array(ty: TyId<'db>, elems: Vec<CtfeConstValue<'db>>) -> Self {
        Self {
            kind: CtfeConstKind::Array {
                ty,
                elems: elems.into(),
            },
        }
    }

    fn enum_(ty: TyId<'db>, variant: VariantIndex, fields: Vec<CtfeConstValue<'db>>) -> Self {
        Self {
            kind: CtfeConstKind::Enum {
                ty,
                variant,
                fields: fields.into(),
            },
        }
    }

    fn concrete(db: &'db dyn HirAnalysisDb, value: VerifiedConstValueId<'db>) -> Self {
        let kind = match value.value().value(db) {
            SemConstValue::Unit => CtfeConstKind::Unit,
            SemConstValue::Scalar {
                value: SemConstScalar::Bool(value),
                ..
            } => CtfeConstKind::Bool(value),
            SemConstValue::Scalar {
                ty,
                value: SemConstScalar::Int { value },
            } => CtfeConstKind::Int {
                ty,
                value: CtfeInt::from_bigint(db, ty, value.clone()),
            },
            SemConstValue::Scalar {
                ty,
                value: SemConstScalar::Bytes(bytes),
            } => CtfeConstKind::Bytes {
                ty,
                bytes: Rc::from(bytes.as_slice()),
            },
            SemConstValue::Tuple { .. }
            | SemConstValue::Struct { .. }
            | SemConstValue::Array { .. }
            | SemConstValue::Enum { .. } => CtfeConstKind::Interned(value),
            SemConstValue::Description(..) => unreachable!("verified CTFE value is dependent"),
        };
        Self { kind }
    }

    fn expand_sem_const_shallow(
        db: &'db dyn HirAnalysisDb,
        value: VerifiedConstValueId<'db>,
    ) -> Self {
        let kind = match value.value().value(db) {
            SemConstValue::Unit => CtfeConstKind::Unit,
            SemConstValue::Scalar {
                value: SemConstScalar::Bool(value),
                ..
            } => CtfeConstKind::Bool(value),
            SemConstValue::Scalar {
                ty,
                value: SemConstScalar::Int { value },
            } => CtfeConstKind::Int {
                ty,
                value: CtfeInt::from_bigint(db, ty, value.clone()),
            },
            SemConstValue::Scalar {
                ty,
                value: SemConstScalar::Bytes(bytes),
            } => CtfeConstKind::Bytes {
                ty,
                bytes: Rc::from(bytes.as_slice()),
            },
            SemConstValue::Description(..) => unreachable!("verified CTFE value is dependent"),
            SemConstValue::Tuple { ty, .. } => CtfeConstKind::Tuple {
                ty,
                elems: value
                    .aggregate_children(db)
                    .into_iter()
                    .map(|child| Self::concrete(db, child))
                    .collect::<Vec<_>>()
                    .into(),
            },
            SemConstValue::Struct { ty, .. } => CtfeConstKind::Struct {
                ty,
                fields: value
                    .aggregate_children(db)
                    .into_iter()
                    .map(|child| Self::concrete(db, child))
                    .collect::<Vec<_>>()
                    .into(),
            },
            SemConstValue::Array { ty, .. } => CtfeConstKind::Array {
                ty,
                elems: value
                    .aggregate_children(db)
                    .into_iter()
                    .map(|child| Self::concrete(db, child))
                    .collect::<Vec<_>>()
                    .into(),
            },
            SemConstValue::Enum { ty, variant, .. } => CtfeConstKind::Enum {
                ty,
                variant,
                fields: value
                    .aggregate_children(db)
                    .into_iter()
                    .map(|child| Self::concrete(db, child))
                    .collect::<Vec<_>>()
                    .into(),
            },
        };
        Self { kind }
    }

    fn materialize(&self, db: &'db dyn HirAnalysisDb) -> SemConstId<'db> {
        match &self.kind {
            CtfeConstKind::Interned(value) => value.value(),
            CtfeConstKind::Unit => unit_const(db),
            CtfeConstKind::Bool(value) => bool_const(db, *value),
            CtfeConstKind::Int { ty, value } => int_const(db, *ty, value.to_bigint()),
            CtfeConstKind::Bytes { ty, bytes } => bytes_const(db, *ty, bytes.to_vec()),
            CtfeConstKind::Tuple { ty, elems } => tuple_const(
                db,
                *ty,
                elems
                    .iter()
                    .map(|elem| elem.materialize(db))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            CtfeConstKind::Struct { ty, fields } => struct_const(
                db,
                *ty,
                fields
                    .iter()
                    .map(|field| field.materialize(db))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            CtfeConstKind::Array { ty, elems } => array_const(
                db,
                *ty,
                elems
                    .iter()
                    .map(|elem| elem.materialize(db))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            CtfeConstKind::Enum {
                ty,
                variant,
                fields,
            } => enum_const(
                db,
                *ty,
                *variant,
                fields
                    .iter()
                    .map(|field| field.materialize(db))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
        }
    }

    fn ty(&self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        match &self.kind {
            CtfeConstKind::Interned(value) => sem_const_ty(db, value.value()),
            CtfeConstKind::Unit => TyId::unit(db),
            CtfeConstKind::Bool(_) => TyId::bool(db),
            CtfeConstKind::Int { ty, .. }
            | CtfeConstKind::Bytes { ty, .. }
            | CtfeConstKind::Tuple { ty, .. }
            | CtfeConstKind::Struct { ty, .. }
            | CtfeConstKind::Array { ty, .. }
            | CtfeConstKind::Enum { ty, .. } => *ty,
        }
    }

    fn is_scalar(&self, db: &'db dyn HirAnalysisDb) -> bool {
        match &self.kind {
            CtfeConstKind::Bool(_) | CtfeConstKind::Int { .. } | CtfeConstKind::Bytes { .. } => {
                true
            }
            CtfeConstKind::Interned(value) => {
                matches!(value.value().value(db), SemConstValue::Scalar { .. })
            }
            CtfeConstKind::Unit
            | CtfeConstKind::Tuple { .. }
            | CtfeConstKind::Struct { .. }
            | CtfeConstKind::Array { .. }
            | CtfeConstKind::Enum { .. } => false,
        }
    }
}

impl CtfeInt {
    fn from_bigint<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, value: BigInt) -> Self {
        let Some((bits, signed)) = int_ty_shape(db, ty) else {
            return Self::Big(value);
        };
        let value = normalize_int_to_shape(value, bits, false);
        Self::Word {
            bits,
            signed,
            word: u256_from_bigint(&value),
        }
    }

    fn from_word(bits: u16, signed: bool, word: U256) -> Self {
        let word = if bits == 0 {
            U256::ZERO
        } else if bits < 256 {
            word & ((U256::from(1u8) << usize::from(bits)) - U256::from(1u8))
        } else {
            word
        };
        Self::Word { bits, signed, word }
    }

    fn to_bigint(&self) -> BigInt {
        match self {
            CtfeInt::Word { bits, signed, word } => {
                let unsigned = bigint_from_u256(*word);
                if *signed && *bits > 0 {
                    let sign_bit = BigInt::one() << usize::from(bits - 1);
                    if unsigned >= sign_bit {
                        return unsigned - (BigInt::one() << usize::from(*bits));
                    }
                }
                unsigned
            }
            CtfeInt::Big(value) => value.clone(),
        }
    }

    fn to_u256(&self) -> U256 {
        match self {
            CtfeInt::Word { word, .. } => *word,
            CtfeInt::Big(value) => {
                u256_from_bigint(&normalize_int_to_shape(value.clone(), 256, false))
            }
        }
    }
}

fn u256_from_bigint(value: &BigInt) -> U256 {
    let (_, bytes) = value.to_bytes_be();
    let mut out = [0u8; 32];
    let bytes = if bytes.len() > out.len() {
        &bytes[bytes.len() - out.len()..]
    } else {
        &bytes
    };
    let offset = out.len() - bytes.len();
    out[offset..].copy_from_slice(bytes);
    U256::from_be_bytes(out)
}

fn biguint_from_u256(value: U256) -> BigUint {
    BigUint::from_bytes_be(&value.to_be_bytes::<32>())
}

fn bigint_from_u256(value: U256) -> BigInt {
    BigInt::from_bytes_be(Sign::Plus, &value.to_be_bytes::<32>())
}

fn signed_word_is_negative(bits: u16, word: U256) -> bool {
    bits > 0 && word.bit(usize::from(bits - 1))
}

fn sign_extend_word(bits: u16, word: U256) -> U256 {
    if bits == 0 || bits == 256 || !signed_word_is_negative(bits, word) {
        word
    } else {
        word | (U256::MAX << usize::from(bits))
    }
}

fn wrapping_shift_word(bits: u16, signed: bool, lhs: U256, rhs: U256, left: bool) -> U256 {
    if rhs >= U256::from(256u16) {
        if left || !signed || !signed_word_is_negative(bits, lhs) {
            U256::ZERO
        } else {
            U256::MAX
        }
    } else if left {
        lhs.wrapping_shl(rhs.wrapping_to::<usize>())
    } else if signed {
        sign_extend_word(bits, lhs).arithmetic_shr(rhs.wrapping_to::<usize>())
    } else {
        lhs.wrapping_shr(rhs.wrapping_to::<usize>())
    }
}

fn expect_binary_args<'a, 'db>(
    args: &'a [CtfeConstValue<'db>],
    origin: SemOrigin<'db>,
) -> Result<(&'a CtfeConstValue<'db>, &'a CtfeConstValue<'db>), CtfeError<'db>> {
    let [lhs, rhs] = args else {
        return Err(CtfeError::NotConstEvaluable { origin });
    };
    Ok((lhs, rhs))
}

fn checked_result<'db>(
    value: BigInt,
    machine: &CtfeMachine<'db, '_>,
    result_ty: TyId<'db>,
    origin: SemOrigin<'db>,
) -> Result<BigInt, CtfeError<'db>> {
    if !machine.int_in_range(result_ty, &value) {
        return Err(CtfeError::ArithmeticOverflow { origin });
    }
    Ok(value)
}

pub(super) fn primitive_error<'db>(
    origin: SemOrigin<'db>,
    fault: PrimitiveFault,
) -> CtfeError<'db> {
    match fault {
        PrimitiveFault::ArithmeticOverflow => CtfeError::ArithmeticOverflow { origin },
        PrimitiveFault::DivisionByZero => CtfeError::DivisionByZero { origin },
        PrimitiveFault::NegativeExponent => CtfeError::NegativeExponent { origin },
        PrimitiveFault::InvalidPowerExponent => CtfeError::InvalidOperation {
            origin,
            message: "invalid power exponent".into(),
        },
        PrimitiveFault::UnsupportedCast => CtfeError::NotConstEvaluable { origin },
        PrimitiveFault::InvalidCast => CtfeError::InvalidOperation {
            origin,
            message: "unsupported cast in CTFE".into(),
        },
        PrimitiveFault::OutsideSupportedSubset => CtfeError::InvalidOperation {
            origin,
            message: "unsupported primitive operation in CTFE".into(),
        },
    }
}

fn int_bounds(bits: u16, signed: bool) -> (BigInt, BigInt) {
    if signed {
        let half = BigInt::one() << (usize::from(bits) - 1);
        (-half.clone(), half - BigInt::one())
    } else {
        (
            BigInt::zero(),
            (BigInt::one() << usize::from(bits)) - BigInt::one(),
        )
    }
}

pub(super) fn sem_const_dependency<'db>(
    db: &'db dyn HirAnalysisDb,
    value: SemConstId<'db>,
) -> Option<ConstDependency<'db>> {
    match value.value(db) {
        SemConstValue::Description(term) => Some(ConstDependency::Value(TyId::const_ty(db, term))),
        SemConstValue::Tuple { elems, .. } | SemConstValue::Array { elems, .. } => elems
            .iter()
            .copied()
            .find_map(|elem| sem_const_dependency(db, elem)),
        SemConstValue::Struct { fields, .. } | SemConstValue::Enum { fields, .. } => fields
            .iter()
            .copied()
            .find_map(|field| sem_const_dependency(db, field)),
        SemConstValue::Unit | SemConstValue::Scalar { .. } => None,
    }
}

impl<'db> CtfeValue<'db> {
    fn concrete(db: &'db dyn HirAnalysisDb, value: VerifiedConstValueId<'db>) -> Self {
        Self::Value(CtfeConstValue::concrete(db, value))
    }
}

#[derive(Clone)]
struct CtfeRef {
    frame: usize,
    root: SLocalId,
    path: Box<[CtfePathElem]>,
}

#[derive(Clone)]
enum CtfePathElem {
    Field(FieldIndex),
    VariantField {
        variant: VariantIndex,
        field: FieldIndex,
    },
    Index(usize),
}

impl<'db, 'body> CtfeMachine<'db, 'body> {
    fn new(db: &'db dyn HirAnalysisDb, config: CtfeConfig) -> Self {
        Self {
            db,
            config,
            steps: 0,
            instance_cache: FxHashMap::default(),
            frames: Vec::new(),
            const_results: FxHashMap::default(),
            const_stack: Vec::new(),
        }
    }

    fn instance_for_key(&mut self, key: SemanticInstanceKey<'db>) -> SemanticInstance<'db> {
        if let Some(instance) = self.instance_cache.get(&key).copied() {
            return instance;
        }
        let instance = SemanticInstance::new(self.db, key);
        self.instance_cache.insert(key, instance);
        instance
    }

    fn eval_root(
        &mut self,
        instance: SemanticInstance<'db>,
        args: Vec<CtfeValue<'db>>,
        origin: SemOrigin<'db>,
    ) -> EvalResult<'db, SemConstId<'db>> {
        for arg in &args {
            match arg {
                CtfeValue::Value(value) => {
                    if let Some(dependency) =
                        sem_const_dependency(self.db, value.materialize(self.db))
                    {
                        return Err(EvalStop::Blocked(BlockedInfo::new(
                            ConstDemandKind::Value,
                            dependency,
                            origin,
                        )));
                    }
                }
                CtfeValue::Ref(_) => {
                    return Err(EvalStop::Failed(EvalFailure::Invariant {
                        origin,
                        message: "CTFE request input contains a machine reference".into(),
                    }));
                }
            }
        }
        let value = if let BodyOwner::Func(func) = instance.key(self.db).owner(self.db)
            && func.is_extern(self.db)
        {
            if !func.is_const(self.db) {
                return Err(CtfeError::NonConstCall { origin }.into());
            }
            let body = instance
                .admitted_body(self.db)
                .map_err(|_| CtfeError::InvalidBody { origin })?;
            self.frames.push(CtfeFrame {
                body,
                locals: Vec::new(),
                current: 0,
            });
            let result = (|| {
                let args = self.value_args(args, origin)?;
                self.eval_extern_const_fn(
                    instance,
                    func,
                    instance.key(self.db).typed_body(self.db).result_ty(),
                    &args,
                    origin,
                )
            })();
            self.frames.pop();
            CtfeValue::Value(result?)
        } else {
            self.eval_instance(instance, args, Vec::new(), origin)?
        };
        let CtfeValue::Value(value) = value else {
            return Err(CtfeError::InvalidBorrow { origin }.into());
        };
        let value = value.materialize(self.db);
        if sem_const_dependency(self.db, value).is_some() {
            return Err(EvalStop::Failed(EvalFailure::Invariant {
                origin,
                message: "CTFE completed with a symbolic machine value".into(),
            }));
        }
        Ok(value)
    }

    fn load_sem_const(
        &mut self,
        value: SemConstId<'db>,
        origin: SemOrigin<'db>,
    ) -> EvalResult<'db, CtfeValue<'db>> {
        if sem_const_dependency(self.db, value).is_some() {
            let term = ConstTyId::new(self.db, ConstTyData::Description(value));
            let value = self.force_term(term, ConstDemandKind::Value, origin)?;
            return Ok(CtfeValue::concrete(self.db, value));
        }
        let value =
            VerifiedConstValueId::from_complete_execution(self.db, value).map_err(|message| {
                EvalStop::Failed(EvalFailure::Invariant {
                    origin,
                    message: message.into(),
                })
            })?;
        Ok(CtfeValue::concrete(self.db, value))
    }

    fn force_term(
        &mut self,
        term: ConstTyId<'db>,
        demand: ConstDemandKind,
        origin: SemOrigin<'db>,
    ) -> EvalResult<'db, VerifiedConstValueId<'db>> {
        let config = CtfeConfig {
            recursion_limit: self
                .config
                .recursion_limit
                .saturating_sub(self.frames.len().saturating_sub(self.const_stack.len())),
            ..self.config.clone()
        };
        force_const_term_value_with_steps(self.db, term, config, origin, &mut self.steps)
            .into_result()
            .map_err(|stop| match stop {
                EvalStop::Blocked(mut info) => {
                    info.demand = demand;
                    EvalStop::Blocked(info)
                }
                stop => stop,
            })
    }

    fn eval_expr_with_locals(
        &mut self,
        body: &'body SemanticBody<'db>,
        result_ty: TyId<'db>,
        expr: SExpr<'db>,
        locals: &[Option<SemConstId<'db>>],
        origin: SemOrigin<'db>,
    ) -> EvalResult<'db, SemConstId<'db>> {
        let mut frame_locals = vec![CtfeSlot::Uninit; body.locals.len()];
        for (idx, value) in locals.iter().copied().enumerate() {
            if let Some(value) = value
                && let Some(slot) = frame_locals.get_mut(idx)
            {
                *slot = CtfeSlot::Init(self.load_sem_const(value, origin)?);
            }
        }
        let frame_idx = self.frames.len();
        self.frames.push(CtfeFrame {
            body,
            locals: frame_locals,
            current: 0,
        });
        let result = match self.eval_expr(frame_idx, result_ty, expr, origin) {
            Ok(CtfeValue::Value(value)) => Ok(value.materialize(self.db)),
            Ok(CtfeValue::Ref(_)) => Err(CtfeError::InvalidBorrow { origin }.into()),
            Err(stop) => Err(stop),
        };
        self.frames.pop();
        result
    }

    /// Evaluates a const-item reference in this machine, pushing a frame for
    /// the referenced instance instead of re-entering the salsa eval
    /// queries. The const stack makes recursive definitions a detected
    /// error (with the reference site as the origin) rather than a salsa
    /// dependency cycle, and results are memoized per machine so shared
    /// sub-consts are evaluated once.
    fn eval_const_ref_value(
        &mut self,
        cref: SemanticConstRef<'db>,
    ) -> EvalResult<'db, SemConstId<'db>> {
        let key = cref.instance(self.db);
        if let Some(result) = self.const_results.get(&key) {
            return result.clone();
        }
        let origin = cref.origin(self.db);
        if self.const_stack.contains(&key) {
            return Err(CtfeError::RecursiveConst { origin }.into());
        }

        self.const_stack.push(key);
        let result = self
            .eval_instance(
                SemanticInstance::new(self.db, key),
                Vec::new(),
                Vec::new(),
                origin,
            )
            .and_then(|value| match value {
                CtfeValue::Value(value) => {
                    let value = value.materialize(self.db);
                    if sem_const_dependency(self.db, value).is_some() {
                        Err(EvalStop::Failed(EvalFailure::Invariant {
                            origin,
                            message: "nested CTFE completed with a symbolic value".into(),
                        }))
                    } else {
                        Ok(value)
                    }
                }
                CtfeValue::Ref(_) => Err(CtfeError::InvalidBorrow { origin }.into()),
            });
        self.const_stack.pop();
        self.const_results.insert(key, result.clone());
        result
    }

    fn eval_instance(
        &mut self,
        instance: SemanticInstance<'db>,
        args: Vec<CtfeValue<'db>>,
        effects: Vec<(u32, CtfeValue<'db>)>,
        origin: SemOrigin<'db>,
    ) -> EvalResult<'db, CtfeValue<'db>> {
        let body = self.const_evaluable_body(instance, origin)?;
        // Const-item frames are exempt from the limit: the const stack's
        // cycle check already bounds them (each const is evaluated at most
        // once per path), and a long but finite chain of const definitions
        // is not call recursion.
        if self.frames.len().saturating_sub(self.const_stack.len()) >= self.config.recursion_limit {
            return Err(CtfeError::RecursionLimitExceeded { origin }.into());
        }

        let mut locals = vec![CtfeSlot::Uninit; body.locals.len()];
        let mut arg_locals = match instance.key(self.db).owner(self.db) {
            BodyOwner::Func(func) => body
                .entry_locals
                .iter()
                .filter_map(|local| match body.local(*local)?.source? {
                    LocalBinding::Param {
                        site: ParamSite::Func(candidate),
                        idx,
                        ..
                    } if candidate == func => Some((idx, *local)),
                    LocalBinding::Local { .. }
                    | LocalBinding::EffectParam { .. }
                    | LocalBinding::Param { .. } => None,
                })
                .collect::<Vec<_>>(),
            BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::ContractInit { .. }
            | BodyOwner::ContractRecvArm { .. } => Vec::new(),
        };
        arg_locals.sort_unstable_by_key(|(idx, _)| *idx);
        if args.len() != arg_locals.len()
            || arg_locals
                .iter()
                .enumerate()
                .any(|(expected, (actual, _))| expected != *actual)
        {
            return Err(CtfeError::InvalidOperation {
                origin,
                message: "CTFE call arity mismatch".into(),
            }
            .into());
        }
        for ((_, local), arg) in arg_locals.into_iter().zip(args) {
            let Some(slot) = locals.get_mut(local.index()) else {
                return Err(CtfeError::InvalidOperation {
                    origin,
                    message: "CTFE call arity mismatch".into(),
                }
                .into());
            };
            *slot = CtfeSlot::Init(arg);
        }
        // Effect positions use their declaration binding index, independently
        // of ordinary argument positions. Never silently drop or invent a slot.
        // A request that supplies no providers (a root evaluation of an
        // effectful function) is unsupported; any other mismatch is malformed.
        let mut effect_locals = body
            .entry_locals
            .iter()
            .filter_map(|local| match body.local(*local)?.source? {
                LocalBinding::EffectParam {
                    site: EffectParamSite::Func(func),
                    idx,
                    ..
                } if instance.key(self.db).owner(self.db) == BodyOwner::Func(func) => {
                    Some((idx as u32, *local))
                }
                _ => None,
            })
            .collect::<FxHashMap<_, _>>();
        if effects.is_empty() && !effect_locals.is_empty() {
            return Err(CtfeError::NotConstEvaluable { origin }.into());
        }
        let effect_arity_mismatch = || -> EvalStop<'db> {
            CtfeError::InvalidOperation {
                origin,
                message: "CTFE effect arity mismatch".into(),
            }
            .into()
        };
        if effect_locals.len() != effects.len() {
            return Err(effect_arity_mismatch());
        }
        for (idx, value) in effects {
            let Some(local) = effect_locals.remove(&idx) else {
                return Err(effect_arity_mismatch());
            };
            if !matches!(value, CtfeValue::Value(_)) {
                return Err(CtfeError::InvalidBorrow { origin }.into());
            }
            locals[local.index()] = CtfeSlot::Init(value);
        }
        let frame_idx = self.frames.len();
        self.frames.push(CtfeFrame {
            body,
            locals,
            current: 0,
        });
        let result = self.run_frame(frame_idx).and_then(|value| {
            if let CtfeValue::Ref(r#ref) = &value {
                let key = instance.key(self.db);
                let result_ty = match key.owner(self.db) {
                    BodyOwner::Func(func) => func.return_ty(self.db),
                    BodyOwner::Const(const_) => const_.ty(self.db),
                    BodyOwner::AnonConstBody { expected, .. } => expected,
                    BodyOwner::ContractInit { .. } | BodyOwner::ContractRecvArm { .. } => {
                        return Err(CtfeError::NotConstEvaluable { origin }.into());
                    }
                };
                let ty = instantiate_with_generic_args(
                    self.db,
                    result_ty,
                    key.subst(self.db).generic_args(self.db),
                );
                let returns_borrow = normalize_ty(
                    self.db,
                    ty,
                    key.impl_env(self.db).normalization_scope(self.db),
                    key.impl_env(self.db).assumptions(self.db),
                )
                .as_capability(self.db)
                .is_some();
                // Returning an ordinary value reads the referent before its
                // frame disappears. Returning a capability preserves the ref.
                if !returns_borrow {
                    return self
                        .load_ref_value(r#ref, origin)
                        .map(CtfeValue::Value)
                        .map_err(Into::into);
                }
            }
            // A callee may return a reference into a caller's frame, but never
            // into the frame that is about to be removed.
            if matches!(&value, CtfeValue::Ref(r#ref) if r#ref.frame >= frame_idx) {
                Err(CtfeError::InvalidBorrow { origin }.into())
            } else {
                Ok(value)
            }
        });
        self.frames.pop();
        result
    }

    fn const_evaluable_body(
        &self,
        instance: SemanticInstance<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<&'body SemanticBody<'db>, CtfeError<'db>> {
        match instance.key(self.db).owner(self.db) {
            BodyOwner::Func(func) if !func.is_const(self.db) => {
                Err(CtfeError::NonConstCall { origin })
            }
            BodyOwner::Func(func) if !const_effects_supported(self.db, func) => {
                Err(CtfeError::NotConstEvaluable { origin })
            }
            BodyOwner::ContractInit { .. } | BodyOwner::ContractRecvArm { .. } => {
                Err(CtfeError::NotConstEvaluable { origin })
            }
            BodyOwner::Func(_) | BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => instance
                .admitted_body(self.db)
                .map_err(|_| CtfeError::InvalidBody { origin }),
        }
    }

    fn run_frame(&mut self, frame_idx: usize) -> EvalResult<'db, CtfeValue<'db>> {
        loop {
            let block = self.frames[frame_idx].body.blocks[self.frames[frame_idx].current].clone();
            for stmt in block.stmts {
                self.bump(stmt.origin)?;
                self.exec_stmt(frame_idx, stmt)?;
            }
            let term_origin = block.terminator.origin;
            self.bump(term_origin)?;
            match block.terminator.kind {
                STerminatorKind::Goto(bb) => self.frames[frame_idx].current = bb.index(),
                STerminatorKind::Branch {
                    cond,
                    then_bb,
                    else_bb,
                } => {
                    let cond = self.load_value(frame_idx, cond, term_origin)?;
                    let cond = self.expect_bool(cond, term_origin)?;
                    self.frames[frame_idx].current = if cond {
                        then_bb.index()
                    } else {
                        else_bb.index()
                    };
                }
                STerminatorKind::MatchEnum {
                    value,
                    cases,
                    default,
                    ..
                } => {
                    let value = self.load_value(frame_idx, value, term_origin)?;
                    let tag = self.load_enum_variant(value, term_origin)?;
                    self.frames[frame_idx].current = cases
                        .iter()
                        .find(|(variant, _)| *variant == tag)
                        .map_or_else(|| default.map_or(0, |bb| bb.index()), |(_, bb)| bb.index());
                }
                STerminatorKind::Assert { message } => {
                    return Err(CtfeError::AssertionFailed {
                        origin: term_origin,
                        message: message.map(|message| message.data(self.db).to_string()),
                    }
                    .into());
                }
                STerminatorKind::Return(Some(value)) => {
                    return Ok(self.read_operand(frame_idx, value, term_origin)?);
                }
                STerminatorKind::Return(None) => {
                    return Ok(CtfeValue::Value(CtfeConstValue::unit()));
                }
            }
        }
    }

    fn exec_stmt(&mut self, frame_idx: usize, stmt: SStmt<'db>) -> EvalResult<'db, ()> {
        let origin = stmt.origin;
        match stmt.kind {
            SStmtKind::Assign { dst, expr } => {
                let ty = self.frames[frame_idx].body.locals[dst.index()].ty;
                let value = self.eval_expr(frame_idx, ty, expr, origin)?;
                self.frames[frame_idx].locals[dst.index()] = CtfeSlot::Init(value);
            }
            SStmtKind::Store { dst, src } => {
                let place = self.resolve_place(frame_idx, &dst, origin)?;
                let CtfeValue::Value(value) = self.read_operand(frame_idx, src, origin)? else {
                    return Err(CtfeError::InvalidBorrow { origin }.into());
                };
                self.store_place(place, value, origin)?;
            }
        }
        Ok(())
    }

    fn eval_expr(
        &mut self,
        frame_idx: usize,
        result_ty: TyId<'db>,
        expr: SExpr<'db>,
        origin: SemOrigin<'db>,
    ) -> EvalResult<'db, CtfeValue<'db>> {
        self.bump(origin)?;
        // Keep the large value-operation frame off recursive call paths.
        match expr {
            SExpr::Call {
                callee,
                args,
                effect_args,
                ..
            } => {
                let mut effects = Vec::with_capacity(effect_args.len());
                for arg in &effect_args {
                    if arg.required_mut
                        || !matches!(
                            arg.pass_mode,
                            EffectPassMode::ByValue | EffectPassMode::ByPlace
                        )
                        || arg.layout_view != EffectArgLayoutView::Direct
                        || arg
                            .provider
                            .is_some_and(|space| space != ProviderAddressSpace::Memory)
                        || arg.provider_target_ty.is_some()
                    {
                        return Err(CtfeError::NotConstEvaluable { origin }.into());
                    }
                    let value = match &arg.arg {
                        SEffectArgValue::Value(operand) => {
                            let value = self.read_operand(frame_idx, *operand, origin)?;
                            if !matches!(value, CtfeValue::Value(_)) {
                                return Err(CtfeError::NotConstEvaluable { origin }.into());
                            }
                            value
                        }
                        // A `with` provider is captured once as a place and
                        // passed by place. An immutable provider reads the
                        // same as a copy of its value.
                        SEffectArgValue::Place(place) => {
                            let place = self.resolve_place(frame_idx, place, origin)?;
                            let r#ref = CtfeRef {
                                frame: place.frame,
                                root: place.root,
                                path: place.path.into_boxed_slice(),
                            };
                            CtfeValue::Value(self.load_ref_value(&r#ref, origin)?)
                        }
                    };
                    effects.push((arg.binding_idx, value));
                }
                let args = self
                    .eval_args(frame_idx, &args, origin)?
                    .into_iter()
                    .collect::<Vec<_>>();
                let instance = self.instance_for_key(callee.key);
                if !effect_args.is_empty() {
                    let BodyOwner::Func(func) = instance.key(self.db).owner(self.db) else {
                        return Err(CtfeError::NotConstEvaluable { origin }.into());
                    };
                    if !const_effects_supported(self.db, func) {
                        return Err(CtfeError::NotConstEvaluable { origin }.into());
                    }
                }
                if let Some(value) = self.try_eval_core_primitive_wrapper_call(
                    frame_idx, instance, result_ty, &args, origin,
                )? {
                    return Ok(CtfeValue::Value(value));
                }
                if let BodyOwner::Func(func) = instance.key(self.db).owner(self.db)
                    && func.is_extern(self.db)
                {
                    if !func.is_const(self.db) {
                        return Err(CtfeError::NonConstCall { origin }.into());
                    }
                    let value_args = self.value_args(args, origin)?;
                    return self
                        .eval_extern_const_fn(instance, func, result_ty, &value_args, origin)
                        .map(CtfeValue::Value)
                        .map_err(Into::into);
                }
                if let BodyOwner::Func(func) = instance.key(self.db).owner(self.db)
                    && !func.is_const(self.db)
                {
                    return Err(CtfeError::NonConstCall { origin }.into());
                }
                match self.eval_instance(instance, args, effects, origin) {
                    Ok(value) => Ok(value),
                    Err(EvalStop::Blocked(mut info)) => {
                        info.trace.push(instance.key(self.db));
                        Err(EvalStop::Blocked(info))
                    }
                    Err(EvalStop::Failed(EvalFailure::Ctfe(err))) => Err(CtfeError::CalleeError {
                        origin,
                        callee: instance,
                        source: Box::new(err),
                    }
                    .into()),
                    Err(stop) => Err(stop),
                }
            }
            SExpr::Const(SConst::Ref(cref)) => {
                let value = self.eval_const_ref_value(cref).map_err(|stop| match stop {
                    // Re-originating recursion errors at the reference site
                    // keeps the origin an expression of the body being
                    // evaluated, so the eventual diagnostic anchors in the
                    // right body. (It also keeps the error shape stable if
                    // an outer salsa fixpoint iteration replays this site.)
                    EvalStop::Failed(EvalFailure::Ctfe(err)) if err.root_is_recursive_const() => {
                        CtfeError::RecursiveConst {
                            origin: cref.origin(self.db),
                        }
                        .into()
                    }
                    EvalStop::Failed(EvalFailure::Ctfe(err)) => CtfeError::CalleeError {
                        origin: cref.origin(self.db),
                        callee: SemanticInstance::new(self.db, cref.instance(self.db)),
                        source: Box::new(err),
                    }
                    .into(),
                    EvalStop::Blocked(mut info) => {
                        info.trace.push(cref.instance(self.db));
                        EvalStop::Blocked(info)
                    }
                    failure => failure,
                })?;
                self.load_sem_const(value, origin)
            }
            expr => self.eval_value_expr(frame_idx, result_ty, expr, origin),
        }
    }

    fn eval_value_expr(
        &mut self,
        frame_idx: usize,
        result_ty: TyId<'db>,
        expr: SExpr<'db>,
        origin: SemOrigin<'db>,
    ) -> EvalResult<'db, CtfeValue<'db>> {
        self.bump(origin)?;
        match expr {
            SExpr::Call { .. } | SExpr::Const(SConst::Ref(..)) => {
                unreachable!("calls are dispatched separately")
            }
            SExpr::Forward(value) => Ok(self.read_operand(frame_idx, value, origin)?),
            SExpr::UseValue(value) => {
                let read = self.read_operand(frame_idx, value, origin)?;
                let body = self.frames[frame_idx].body;
                let source_ty = body
                    .owner
                    .normalized_ty(self.db, body.locals[value.value.index()].ty);
                let result_ty = body.owner.normalized_ty(self.db, result_ty);
                if source_ty != result_ty
                    && result_ty.as_capability(self.db).is_none()
                    && let CtfeValue::Ref(r#ref) = &read
                {
                    Ok(CtfeValue::Value(self.load_ref_value(r#ref, origin)?))
                } else {
                    Ok(read)
                }
            }
            SExpr::CodeRegionRef { .. } => Err(CtfeError::NotConstEvaluable { origin }.into()),
            SExpr::Const(SConst::Value(value)) => Ok(CtfeValue::concrete(self.db, value)),
            SExpr::Const(SConst::Description(value)) => self.load_sem_const(value, origin),
            SExpr::Const(SConst::Evidence(value)) => {
                let SemConstValue::Description(template) = value.value(self.db) else {
                    return Err(EvalStop::Failed(EvalFailure::Invariant {
                        origin,
                        message: "formal evidence must name a constant template".into(),
                    }));
                };
                let term = instantiate_const_template(
                    self.db,
                    self.frames[frame_idx].body.owner,
                    template,
                );
                let value = sem_const_from_ty(self.db, TyId::const_ty(self.db, term))
                    .ok_or(CtfeError::InvalidBody { origin })?;
                self.load_sem_const(value, origin)
            }
            SExpr::Const(SConst::Invalid(value)) => self.load_sem_const(value, origin),
            SExpr::Unary { op, value } => {
                let value = self.load_value(frame_idx, value, origin)?;
                Ok(self.eval_unary(frame_idx, result_ty, op, value, origin)?)
            }
            SExpr::Binary { op, lhs, rhs } => {
                let lhs = self.load_value(frame_idx, lhs, origin)?;
                let rhs = self.load_value(frame_idx, rhs, origin)?;
                Ok(self.eval_binary(frame_idx, result_ty, op, lhs, rhs, origin)?)
            }
            SExpr::Cast { value, .. } => {
                let value = self.load_value(frame_idx, value, origin)?;
                Ok(self.eval_cast(result_ty, value, origin)?)
            }
            SExpr::AggregateMake { fields, .. } => {
                let fields = self.eval_value_args(frame_idx, &fields, origin)?;
                Ok(self.make_aggregate_value(result_ty, fields))
            }
            SExpr::ArrayRepeat { ty, value } => {
                let CtfeValue::Value(value) = self.read_operand(frame_idx, value, origin)? else {
                    return Err(CtfeError::InvalidBorrow { origin }.into());
                };
                if !ty.is_array(self.db) || ty.has_invalid(self.db) {
                    return Err(CtfeError::InvalidBody { origin }.into());
                }
                let Some(length) = ty.generic_args(self.db).get(1) else {
                    return Err(CtfeError::InvalidBody { origin }.into());
                };
                let TyData::ConstTy(length) = length.data(self.db) else {
                    return Err(CtfeError::InvalidBody { origin }.into());
                };
                let length = self.force_term(*length, ConstDemandKind::ArrayLength, origin)?;
                let SemConstValue::Scalar {
                    value: SemConstScalar::Int { value: length },
                    ..
                } = length.value().value(self.db)
                else {
                    return Err(CtfeError::InvalidOperation {
                        origin,
                        message: "array extent is not an integer".into(),
                    }
                    .into());
                };
                let len = length
                    .to_usize()
                    .ok_or(CtfeError::StepLimitExceeded { origin })?;
                self.steps = self
                    .steps
                    .checked_add(len)
                    .filter(|steps| *steps <= self.config.step_limit)
                    .ok_or(CtfeError::StepLimitExceeded { origin })?;
                Ok(self.make_aggregate_value(result_ty, vec![value; len]))
            }
            SExpr::EnumMake {
                variant, fields, ..
            } => {
                let fields = self.eval_value_args(frame_idx, &fields, origin)?;
                Ok(CtfeValue::Value(CtfeConstValue::enum_(
                    result_ty, variant, fields,
                )))
            }
            SExpr::ReadPlace { place } => {
                let place = self.resolve_place(frame_idx, &place, origin)?;
                let r#ref = CtfeRef {
                    frame: place.frame,
                    root: place.root,
                    path: place.path.into_boxed_slice(),
                };
                Ok(self.load_ref_value(&r#ref, origin).map(CtfeValue::Value)?)
            }
            SExpr::Field { base, field } => {
                let value = self.load_value(frame_idx, base, origin)?;
                Ok(self
                    .project_field(value, field, origin)
                    .map(CtfeValue::Value)?)
            }
            SExpr::Index { base, index } => {
                let value = self.load_value(frame_idx, base, origin)?;
                let index_value = self.load_value(frame_idx, index, origin)?;
                let index = self.index_from_value(index_value, origin)?;
                Ok(self
                    .project_index(value, index, origin)
                    .map(CtfeValue::Value)?)
            }
            SExpr::Borrow {
                place, provider, ..
            } => {
                if let Some(provider) = provider {
                    let locals = &self.frames[frame_idx].body.locals;
                    // Ordinary explicit local borrows also carry Memory
                    // metadata. Only frame-backed places are meaningful here:
                    // CTFE has no external address-space/provider state.
                    if provider != ProviderAddressSpace::Memory
                        || place
                            .path
                            .iter()
                            .any(|elem| matches!(elem, Projection::Deref))
                        || locals[place.local.index()]
                            .role
                            .root_provider(locals)
                            .is_some()
                    {
                        return Err(CtfeError::InvalidProviderUse { origin }.into());
                    }
                }
                let place = self.resolve_place(frame_idx, &place, origin)?;
                Ok(CtfeValue::Ref(CtfeRef {
                    frame: place.frame,
                    root: place.root,
                    path: place.path.into_boxed_slice(),
                }))
            }
            SExpr::GetEnumTag { value } => {
                let value = self.load_value(frame_idx, value, origin)?;
                let variant = self.load_enum_variant(value, origin)?;
                Ok(CtfeValue::Value(CtfeConstValue::int(
                    self.db,
                    result_ty,
                    BigInt::from(variant.0),
                )))
            }
            SExpr::IsEnumVariant { value, variant } => {
                let value = self.load_value(frame_idx, value, origin)?;
                let actual = self.load_enum_variant(value, origin)?;
                Ok(CtfeValue::Value(CtfeConstValue::bool(actual == variant)))
            }
            SExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => {
                let value = self.load_value(frame_idx, value, origin)?;
                Ok(self
                    .enum_extract(value, variant, field, origin)
                    .map(CtfeValue::Value)?)
            }
            SExpr::CodeRegionOffset { .. } | SExpr::CodeRegionLen { .. } => {
                Err(CtfeError::NotConstEvaluable { origin }.into())
            }
        }
    }

    fn try_eval_core_primitive_wrapper_call(
        &mut self,
        frame_idx: usize,
        instance: SemanticInstance<'db>,
        result_ty: TyId<'db>,
        args: &[CtfeValue<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<Option<CtfeConstValue<'db>>, CtfeError<'db>> {
        let BodyOwner::Func(func) = instance.key(self.db).owner(self.db) else {
            return Ok(None);
        };
        let Some(call) = core_primitive_wrapper_call_kind(self.db, func, result_ty) else {
            return Ok(None);
        };
        if let PrimitiveWrapperCallKind::Assign(op) = call {
            return self.try_eval_core_primitive_assign(frame_idx, op, args, origin);
        }

        let value_args = self.value_args(args.to_vec(), origin)?;
        if !value_args.iter().all(|arg| arg.is_scalar(self.db)) {
            return Ok(None);
        }

        let value = match call {
            PrimitiveWrapperCallKind::Unary(op) => {
                let [value] = value_args.as_slice() else {
                    return Ok(None);
                };
                let CtfeValue::Value(value) =
                    self.eval_unary(frame_idx, result_ty, op, value.clone(), origin)?
                else {
                    return Err(CtfeError::InvalidBorrow { origin });
                };
                value
            }
            PrimitiveWrapperCallKind::Binary(op) => {
                let [lhs, rhs] = value_args.as_slice() else {
                    return Ok(None);
                };
                let CtfeValue::Value(value) =
                    self.eval_binary(frame_idx, result_ty, op, lhs.clone(), rhs.clone(), origin)?
                else {
                    return Err(CtfeError::InvalidBorrow { origin });
                };
                value
            }
            PrimitiveWrapperCallKind::Assign(_) => unreachable!(),
        };
        Ok(Some(value))
    }

    fn try_eval_core_primitive_assign(
        &mut self,
        frame_idx: usize,
        op: BinOp,
        args: &[CtfeValue<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<Option<CtfeConstValue<'db>>, CtfeError<'db>> {
        let [dst, rhs] = args else {
            return Ok(None);
        };
        let CtfeValue::Ref(dst_ref) = dst else {
            return Ok(None);
        };
        let lhs = self.load_ref_value(dst_ref, origin)?;
        let rhs = match rhs {
            CtfeValue::Value(value) => value.clone(),
            CtfeValue::Ref(r#ref) => self.load_ref_value(r#ref, origin)?,
        };
        if !lhs.is_scalar(self.db) || !rhs.is_scalar(self.db) {
            return Ok(None);
        }
        let ty = lhs.ty(self.db);
        let CtfeValue::Value(value) = self.eval_binary(frame_idx, ty, op, lhs, rhs, origin)? else {
            return Err(CtfeError::InvalidBorrow { origin });
        };
        self.store_place(
            ResolvedPlace {
                frame: dst_ref.frame,
                root: dst_ref.root,
                path: dst_ref.path.clone().into_vec(),
            },
            value,
            origin,
        )?;
        Ok(Some(CtfeConstValue::unit()))
    }

    fn value_args(
        &self,
        args: Vec<CtfeValue<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<Vec<CtfeConstValue<'db>>, CtfeError<'db>> {
        args.into_iter()
            .map(|arg| match arg {
                CtfeValue::Value(value) => Ok(value),
                CtfeValue::Ref(r#ref) => self.load_ref_value(&r#ref, origin),
            })
            .collect()
    }

    fn eval_extern_const_fn(
        &self,
        instance: SemanticInstance<'db>,
        func: crate::hir_def::Func<'db>,
        result_ty: TyId<'db>,
        args: &[CtfeConstValue<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        match ctfe_extern_intrinsic_kind(self.db, func) {
            Some(CtfeExternIntrinsic::AddMod) => {
                self.eval_evm_modular_arithmetic(result_ty, args, EvmModularArithmetic::Add, origin)
            }
            Some(CtfeExternIntrinsic::MulMod) => {
                self.eval_evm_modular_arithmetic(result_ty, args, EvmModularArithmetic::Mul, origin)
            }
            Some(CtfeExternIntrinsic::LeadingZeros) => {
                self.eval_leading_zeros(result_ty, args, origin)
            }
            Some(CtfeExternIntrinsic::SizeOf) => {
                self.eval_intrinsic_size_of(instance, result_ty, args, origin)
            }
            Some(CtfeExternIntrinsic::AsBytes) => {
                self.eval_intrinsic_as_bytes(result_ty, args, origin)
            }
            Some(CtfeExternIntrinsic::Keccak256) => {
                self.eval_intrinsic_keccak(result_ty, args, origin)
            }
            Some(CtfeExternIntrinsic::Bitcast) => {
                self.eval_intrinsic_bitcast(result_ty, args, origin)
            }
            Some(CtfeExternIntrinsic::Numeric(kind)) => {
                self.eval_numeric_extern_intrinsic(kind, result_ty, args, origin)
            }
            None => Err(CtfeError::NotConstEvaluable { origin }),
        }
    }

    fn eval_numeric_extern_intrinsic(
        &self,
        kind: NumericExternIntrinsic,
        result_ty: TyId<'db>,
        args: &[CtfeConstValue<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        match kind {
            NumericExternIntrinsic::CheckedBinary(op) => {
                let (lhs, rhs) = expect_binary_args(args, origin)?;
                self.eval_checked_numeric_binary(result_ty, op, lhs.clone(), rhs.clone(), origin)
            }
            NumericExternIntrinsic::WrappingBinary(op) => {
                let (lhs, rhs) = expect_binary_args(args, origin)?;
                self.eval_wrapping_numeric_binary(result_ty, op, lhs.clone(), rhs.clone(), origin)
            }
            NumericExternIntrinsic::SaturatingBinary(op) => {
                let (lhs, rhs) = expect_binary_args(args, origin)?;
                self.eval_saturating_numeric_binary(result_ty, op, lhs.clone(), rhs.clone(), origin)
            }
            NumericExternIntrinsic::Comparison(op) => {
                let (lhs, rhs) = expect_binary_args(args, origin)?;
                let CtfeValue::Value(value) =
                    self.eval_compare(op, lhs.clone(), rhs.clone(), origin)?
                else {
                    return Err(CtfeError::InvalidBorrow { origin });
                };
                Ok(value)
            }
            NumericExternIntrinsic::BoolBinary(op) => {
                let (lhs, rhs) = expect_binary_args(args, origin)?;
                let lhs = self.expect_bool(lhs.clone(), origin)?;
                let rhs = self.expect_bool(rhs.clone(), origin)?;
                let value = match op {
                    ArithBinOp::BitAnd => lhs & rhs,
                    ArithBinOp::BitOr => lhs | rhs,
                    ArithBinOp::BitXor => lhs ^ rhs,
                    _ => return Err(CtfeError::NotConstEvaluable { origin }),
                };
                Ok(CtfeConstValue::bool(value))
            }
            NumericExternIntrinsic::CheckedNeg => {
                let [value] = args else {
                    return Err(CtfeError::NotConstEvaluable { origin });
                };
                let value = -self.expect_int(value.clone(), origin)?;
                if !self.int_in_range(result_ty, &value) {
                    return Err(CtfeError::ArithmeticOverflow { origin });
                }
                Ok(CtfeConstValue::int(self.db, result_ty, value))
            }
            NumericExternIntrinsic::WrappingNeg => {
                let [value] = args else {
                    return Err(CtfeError::NotConstEvaluable { origin });
                };
                if let Some(word) = self.expect_matching_int_word(value, result_ty, origin)? {
                    return Ok(CtfeConstValue::int_word(
                        self.db,
                        result_ty,
                        word.wrapping_neg(),
                    ));
                }
                Ok(CtfeConstValue::int(
                    self.db,
                    result_ty,
                    -self.expect_int(value.clone(), origin)?,
                ))
            }
            NumericExternIntrinsic::BitNot => {
                let [value] = args else {
                    return Err(CtfeError::NotConstEvaluable { origin });
                };
                if let Some(word) = self.expect_matching_int_word(value, result_ty, origin)? {
                    return Ok(CtfeConstValue::int_word(self.db, result_ty, word.not()));
                }
                let value = self.expect_int(value.clone(), origin)?;
                Ok(CtfeConstValue::int(
                    self.db,
                    result_ty,
                    -value - BigInt::one(),
                ))
            }
            NumericExternIntrinsic::BoolNot => {
                let [value] = args else {
                    return Err(CtfeError::NotConstEvaluable { origin });
                };
                Ok(CtfeConstValue::bool(
                    !self.expect_bool(value.clone(), origin)?,
                ))
            }
        }
    }

    fn eval_checked_numeric_binary(
        &self,
        result_ty: TyId<'db>,
        op: ArithBinOp,
        lhs: CtfeConstValue<'db>,
        rhs: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        let lhs = self.expect_int(lhs, origin)?;
        let rhs = self.expect_int(rhs, origin)?;
        let value = match op {
            ArithBinOp::Add => checked_result(lhs + rhs, self, result_ty, origin)?,
            ArithBinOp::Sub => checked_result(lhs - rhs, self, result_ty, origin)?,
            ArithBinOp::Mul => checked_result(lhs * rhs, self, result_ty, origin)?,
            ArithBinOp::Div => {
                if rhs.is_zero() {
                    return Err(CtfeError::DivisionByZero { origin });
                }
                if self.signed_div_overflows(result_ty, &lhs, &rhs) {
                    return Err(CtfeError::ArithmeticOverflow { origin });
                }
                lhs / rhs
            }
            ArithBinOp::Rem => {
                if rhs.is_zero() {
                    return Err(CtfeError::DivisionByZero { origin });
                }
                lhs % rhs
            }
            ArithBinOp::Pow => self.checked_pow(result_ty, lhs, rhs, origin)?,
            ArithBinOp::Range
            | ArithBinOp::LShift
            | ArithBinOp::RShift
            | ArithBinOp::BitAnd
            | ArithBinOp::BitOr
            | ArithBinOp::BitXor => {
                return Err(CtfeError::NotConstEvaluable { origin });
            }
        };
        Ok(CtfeConstValue::int(self.db, result_ty, value))
    }

    fn eval_wrapping_numeric_binary(
        &self,
        result_ty: TyId<'db>,
        op: ArithBinOp,
        lhs: CtfeConstValue<'db>,
        rhs: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        if let Some(value) =
            self.eval_wrapping_numeric_binary_word(result_ty, op, &lhs, &rhs, origin)?
        {
            return Ok(value);
        }
        let lhs = self.expect_int(lhs, origin)?;
        let rhs = self.expect_int(rhs, origin)?;
        let value = match op {
            ArithBinOp::Add => lhs + rhs,
            ArithBinOp::Sub => lhs - rhs,
            ArithBinOp::Mul => lhs * rhs,
            ArithBinOp::Div => {
                if rhs.is_zero() {
                    BigInt::zero()
                } else {
                    lhs / rhs
                }
            }
            ArithBinOp::Rem => {
                if rhs.is_zero() {
                    BigInt::zero()
                } else {
                    lhs % rhs
                }
            }
            ArithBinOp::Pow => self.wrapping_pow(result_ty, &lhs, &rhs, origin)?,
            ArithBinOp::LShift => self.wrapping_shift(result_ty, lhs, rhs, true, origin)?,
            ArithBinOp::RShift => self.wrapping_shift(result_ty, lhs, rhs, false, origin)?,
            ArithBinOp::BitAnd => self.bitwise(result_ty, lhs, rhs, |lhs, rhs| lhs & rhs)?,
            ArithBinOp::BitOr => self.bitwise(result_ty, lhs, rhs, |lhs, rhs| lhs | rhs)?,
            ArithBinOp::BitXor => self.bitwise(result_ty, lhs, rhs, |lhs, rhs| lhs ^ rhs)?,
            ArithBinOp::Range => return Err(CtfeError::NotConstEvaluable { origin }),
        };
        Ok(CtfeConstValue::int(self.db, result_ty, value))
    }

    fn eval_wrapping_numeric_binary_word(
        &self,
        result_ty: TyId<'db>,
        op: ArithBinOp,
        lhs: &CtfeConstValue<'db>,
        rhs: &CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<Option<CtfeConstValue<'db>>, CtfeError<'db>> {
        let Some((bits, signed)) = int_ty_shape(self.db, result_ty) else {
            return Ok(None);
        };
        let Some(lhs) = self.expect_matching_int_word(lhs, result_ty, origin)? else {
            return Ok(None);
        };
        let Some(rhs) = self.expect_matching_int_word(rhs, result_ty, origin)? else {
            return Ok(None);
        };
        let value = match op {
            ArithBinOp::Add => lhs.wrapping_add(rhs),
            ArithBinOp::Sub => lhs.wrapping_sub(rhs),
            ArithBinOp::Mul => lhs.wrapping_mul(rhs),
            ArithBinOp::Div if !signed => {
                if rhs.is_zero() {
                    U256::ZERO
                } else {
                    lhs.wrapping_div(rhs)
                }
            }
            ArithBinOp::Rem if !signed => {
                if rhs.is_zero() {
                    U256::ZERO
                } else {
                    lhs.wrapping_rem(rhs)
                }
            }
            ArithBinOp::Pow => lhs.wrapping_pow(rhs),
            ArithBinOp::LShift => wrapping_shift_word(bits, signed, lhs, rhs, true),
            ArithBinOp::RShift => wrapping_shift_word(bits, signed, lhs, rhs, false),
            ArithBinOp::BitAnd => lhs & rhs,
            ArithBinOp::BitOr => lhs | rhs,
            ArithBinOp::BitXor => lhs ^ rhs,
            ArithBinOp::Div | ArithBinOp::Rem => return Ok(None),
            ArithBinOp::Range => return Err(CtfeError::NotConstEvaluable { origin }),
        };
        Ok(Some(CtfeConstValue::int_word(self.db, result_ty, value)))
    }

    fn eval_saturating_numeric_binary(
        &self,
        result_ty: TyId<'db>,
        op: SaturatingArithmetic,
        lhs: CtfeConstValue<'db>,
        rhs: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        let lhs = self.expect_int(lhs, origin)?;
        let rhs = self.expect_int(rhs, origin)?;
        let value = match op {
            SaturatingArithmetic::Add => lhs + rhs,
            SaturatingArithmetic::Sub => lhs - rhs,
            SaturatingArithmetic::Mul => lhs * rhs,
        };
        let Some((bits, signed)) = int_ty_shape(self.db, result_ty) else {
            return Err(CtfeError::NotConstEvaluable { origin });
        };
        let (min, max) = int_bounds(bits, signed);
        Ok(CtfeConstValue::int(
            self.db,
            result_ty,
            value.clamp(min, max),
        ))
    }

    fn eval_evm_modular_arithmetic(
        &self,
        result_ty: TyId<'db>,
        args: &[CtfeConstValue<'db>],
        op: EvmModularArithmetic,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        if result_ty != TyId::u256(self.db) {
            return Err(CtfeError::NotConstEvaluable { origin });
        }
        let [lhs, rhs, modulus] = args else {
            return Err(CtfeError::NotConstEvaluable { origin });
        };
        let lhs = self.expect_u256_const(lhs, origin)?;
        let rhs = self.expect_u256_const(rhs, origin)?;
        let modulus = self.expect_u256_const(modulus, origin)?;
        if modulus.is_zero() {
            return Ok(CtfeConstValue::int_word(self.db, result_ty, U256::ZERO));
        }
        let value = match op {
            EvmModularArithmetic::Add => lhs.add_mod(rhs, modulus),
            EvmModularArithmetic::Mul => lhs.mul_mod(rhs, modulus),
        };
        Ok(CtfeConstValue::int_word(self.db, result_ty, value))
    }

    fn eval_leading_zeros(
        &self,
        result_ty: TyId<'db>,
        args: &[CtfeConstValue<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        if result_ty != TyId::u256(self.db) {
            return Err(CtfeError::NotConstEvaluable { origin });
        }
        let [value] = args else {
            return Err(CtfeError::NotConstEvaluable { origin });
        };
        let value = self.expect_u256_const(value, origin)?;
        let zeros = U256::from(value.leading_zeros());
        Ok(CtfeConstValue::int_word(self.db, result_ty, zeros))
    }

    fn expect_u256_const(
        &self,
        value: &CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<U256, CtfeError<'db>> {
        if value.ty(self.db) != TyId::u256(self.db) {
            return Err(CtfeError::NotConstEvaluable { origin });
        }
        match &value.kind {
            CtfeConstKind::Int { value, .. } => Ok(value.to_u256()),
            CtfeConstKind::Interned(value) => {
                let SemConstValue::Scalar {
                    value: SemConstScalar::Int { value },
                    ..
                } = value.value().value(self.db)
                else {
                    return Err(CtfeError::NotConstEvaluable { origin });
                };
                Ok(u256_from_bigint(&normalize_int_to_shape(
                    value.clone(),
                    256,
                    false,
                )))
            }
            _ => Err(CtfeError::NotConstEvaluable { origin }),
        }
    }

    fn eval_intrinsic_bitcast(
        &self,
        result_ty: TyId<'db>,
        args: &[CtfeConstValue<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        let [value] = args else {
            return Err(CtfeError::NotConstEvaluable { origin });
        };
        if int_ty_shape(self.db, result_ty).is_none() {
            return Err(CtfeError::NotConstEvaluable { origin });
        }
        let value = match &value.kind {
            CtfeConstKind::Int { value, .. } => value.to_bigint(),
            CtfeConstKind::Interned(value) => {
                let SemConstValue::Scalar {
                    value: SemConstScalar::Int { value },
                    ..
                } = value.value().value(self.db)
                else {
                    return Err(CtfeError::NotConstEvaluable { origin });
                };
                value.clone()
            }
            _ => return Err(CtfeError::NotConstEvaluable { origin }),
        };
        Ok(CtfeConstValue::int(self.db, result_ty, value))
    }

    fn eval_intrinsic_size_of(
        &self,
        instance: SemanticInstance<'db>,
        result_ty: TyId<'db>,
        args: &[CtfeConstValue<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        if !args.is_empty() {
            return Err(CtfeError::NotConstEvaluable { origin });
        }
        let ty = *instance
            .key(self.db)
            .subst(self.db)
            .generic_args(self.db)
            .first()
            .ok_or(CtfeError::NotConstEvaluable { origin })?;
        let ty = normalize_ty(
            self.db,
            ty,
            instance.key(self.db).owner(self.db).scope(),
            instance
                .key(self.db)
                .instantiate_typed_body(self.db)
                .assumptions(),
        );
        let size = runtime_size_bytes(self.db, ty)
            .map_err(|_| CtfeError::ArithmeticOverflow { origin })?
            .ok_or(CtfeError::NotConstEvaluable { origin })?;
        Ok(CtfeConstValue::int(self.db, result_ty, BigInt::from(size)))
    }

    fn eval_intrinsic_as_bytes(
        &self,
        result_ty: TyId<'db>,
        args: &[CtfeConstValue<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        if !is_u8_array_ty(self.db, result_ty) {
            return Err(CtfeError::NotConstEvaluable { origin });
        }
        let [value] = args else {
            return Err(CtfeError::NotConstEvaluable { origin });
        };
        let mut bytes = self.const_as_bytes(value, origin)?;
        if let Some(len) = array_len(self.db, result_ty)
            && bytes.len() != len
        {
            if let Some(string_bytes) = self.fixed_string_bytes_for_len(value, len) {
                bytes = string_bytes;
            } else {
                return Err(CtfeError::NotConstEvaluable { origin });
            }
        }
        Ok(CtfeConstValue::bytes(result_ty, bytes))
    }

    fn fixed_string_bytes_for_len(
        &self,
        value: &CtfeConstValue<'db>,
        len: usize,
    ) -> Option<Vec<u8>> {
        let value = self.expand_interned(value.clone());
        let CtfeConstKind::Bytes { ty, bytes } = &value.kind else {
            return None;
        };
        if !ty.is_string(self.db) {
            return None;
        }

        let mut out = vec![0u8; len];
        let suffix = if bytes.len() > len {
            &bytes[bytes.len() - len..]
        } else {
            bytes.as_ref()
        };
        let offset = len - suffix.len();
        out[offset..].copy_from_slice(suffix);
        Some(out)
    }

    fn eval_intrinsic_keccak(
        &self,
        result_ty: TyId<'db>,
        args: &[CtfeConstValue<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        let [value] = args else {
            return Err(CtfeError::NotConstEvaluable { origin });
        };
        let bytes = self.const_as_bytes(value, origin)?;
        let mut hasher = Keccak::v256();
        hasher.update(&bytes);
        let mut out = [0u8; 32];
        hasher.finalize(&mut out);
        Ok(CtfeConstValue::int_word(
            self.db,
            result_ty,
            U256::from_be_bytes(out),
        ))
    }

    fn eval_args(
        &mut self,
        frame_idx: usize,
        args: &[SOperand],
        origin: SemOrigin<'db>,
    ) -> Result<Vec<CtfeValue<'db>>, CtfeError<'db>> {
        args.iter()
            .map(|arg| self.read_operand(frame_idx, *arg, origin))
            .collect()
    }

    fn eval_value_args(
        &mut self,
        frame_idx: usize,
        args: &[SOperand],
        origin: SemOrigin<'db>,
    ) -> Result<Vec<CtfeConstValue<'db>>, CtfeError<'db>> {
        args.iter()
            .map(|arg| {
                let CtfeValue::Value(value) = self.read_operand(frame_idx, *arg, origin)? else {
                    return Err(CtfeError::InvalidBorrow { origin });
                };
                Ok(value)
            })
            .collect()
    }

    fn read_slot(
        &self,
        frame_idx: usize,
        local: SLocalId,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeValue<'db>, CtfeError<'db>> {
        match self.frames[frame_idx].locals.get(local.index()) {
            Some(CtfeSlot::Init(value)) => Ok(value.clone()),
            Some(CtfeSlot::Uninit) | None => Err(CtfeError::UninitializedLocal { origin }),
        }
    }

    fn read_operand(
        &self,
        frame_idx: usize,
        operand: SOperand,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeValue<'db>, CtfeError<'db>> {
        self.read_slot(frame_idx, operand.value, operand.sem_origin(origin))
    }

    fn load_value(
        &mut self,
        frame_idx: usize,
        operand: SOperand,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        let origin = operand.sem_origin(origin);
        match self.read_slot(frame_idx, operand.value, origin)? {
            CtfeValue::Value(value) => Ok(value),
            CtfeValue::Ref(r#ref) => self.load_ref_value(&r#ref, origin),
        }
    }

    fn resolve_place(
        &mut self,
        frame_idx: usize,
        place: &SPlace<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<ResolvedPlace, CtfeError<'db>> {
        let mut resolved = match self.read_slot(frame_idx, place.local, origin)? {
            CtfeValue::Ref(r#ref) => ResolvedPlace {
                frame: r#ref.frame,
                root: r#ref.root,
                path: r#ref.path.into_vec(),
            },
            CtfeValue::Value(_) => ResolvedPlace {
                frame: frame_idx,
                root: place.local,
                path: Vec::new(),
            },
        };
        for elem in place.path.iter() {
            resolved.path.push(match elem {
                Projection::Field(field) => {
                    CtfePathElem::Field(FieldIndex((*field).try_into().map_err(|_| {
                        CtfeError::InvalidOperation {
                            origin,
                            message: "field index does not fit in semantic field index".into(),
                        }
                    })?))
                }
                Projection::VariantField {
                    variant, field_idx, ..
                } => CtfePathElem::VariantField {
                    variant: *variant,
                    field: FieldIndex((*field_idx).try_into().map_err(|_| {
                        CtfeError::InvalidOperation {
                            origin,
                            message: "variant field index does not fit in semantic field index"
                                .into(),
                        }
                    })?),
                },
                Projection::Index(IndexSource::Dynamic(index)) => {
                    let index = self.load_value(frame_idx, SOperand::synthetic(*index), origin)?;
                    CtfePathElem::Index(self.index_from_value(index, origin)?)
                }
                Projection::Index(IndexSource::Constant(index)) => CtfePathElem::Index(*index),
                Projection::Index(IndexSource::Any) => {
                    return Err(CtfeError::InvalidOperation {
                        origin,
                        message: "analysis wildcard index is not valid in CTFE".into(),
                    });
                }
                Projection::Deref | Projection::Discriminant => {
                    return Err(CtfeError::InvalidOperation {
                        origin,
                        message: "invalid CTFE place projection".into(),
                    });
                }
            });
        }
        Ok(resolved)
    }

    fn load_ref_value(
        &self,
        r#ref: &CtfeRef,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        let root = match self.frames[r#ref.frame].locals.get(r#ref.root.index()) {
            Some(CtfeSlot::Init(CtfeValue::Value(value))) => value.clone(),
            Some(CtfeSlot::Init(CtfeValue::Ref(_))) | Some(CtfeSlot::Uninit) | None => {
                return Err(CtfeError::InvalidBorrow { origin });
            }
        };
        self.project_value(root, &r#ref.path, origin)
    }

    fn store_place(
        &mut self,
        place: ResolvedPlace,
        value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), CtfeError<'db>> {
        let mut root = {
            let slot = self
                .frames
                .get_mut(place.frame)
                .and_then(|frame| frame.locals.get_mut(place.root.index()))
                .ok_or(CtfeError::InvalidBorrow { origin })?;
            match std::mem::replace(slot, CtfeSlot::Uninit) {
                CtfeSlot::Init(CtfeValue::Value(value)) => value,
                other => {
                    *slot = other;
                    return Err(CtfeError::InvalidBorrow { origin });
                }
            }
        };
        let result = self.store_const_value_in_place(&mut root, &place.path, value, origin);
        self.frames[place.frame].locals[place.root.index()] =
            CtfeSlot::Init(CtfeValue::Value(root));
        result
    }

    fn project_field(
        &self,
        value: CtfeConstValue<'db>,
        field: FieldIndex,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        self.project_value(value, &[CtfePathElem::Field(field)], origin)
    }

    fn project_index(
        &self,
        value: CtfeConstValue<'db>,
        index: usize,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        let value = self.expand_interned(value);
        let projected = match &value.kind {
            CtfeConstKind::Bytes { bytes, .. } => {
                let byte = *bytes.get(index).ok_or(CtfeError::OutOfBounds { origin })?;
                CtfeConstValue::int(
                    self.db,
                    TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::U8))),
                    byte.into(),
                )
            }
            CtfeConstKind::Array { elems, .. } => elems
                .get(index)
                .cloned()
                .ok_or(CtfeError::OutOfBounds { origin })?,
            _ => {
                return Err(CtfeError::InvalidOperation {
                    origin,
                    message: "invalid const projection".into(),
                });
            }
        };
        Ok(projected)
    }

    fn enum_extract(
        &self,
        value: CtfeConstValue<'db>,
        variant: VariantIndex,
        field: FieldIndex,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        let value = self.expand_interned(value);
        let CtfeConstKind::Enum {
            variant: actual,
            fields,
            ..
        } = &value.kind
        else {
            return Err(CtfeError::VariantMismatch { origin });
        };
        if *actual != variant {
            return Err(CtfeError::VariantMismatch { origin });
        }
        fields
            .get(field.0 as usize)
            .cloned()
            .ok_or(CtfeError::OutOfBounds { origin })
    }

    fn load_enum_variant(
        &self,
        value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<VariantIndex, CtfeError<'db>> {
        let value = self.expand_interned(value);
        let CtfeConstKind::Enum { variant, .. } = value.kind else {
            return Err(CtfeError::VariantMismatch { origin });
        };
        Ok(variant)
    }

    fn expect_bool(
        &self,
        value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<bool, CtfeError<'db>> {
        match &value.kind {
            CtfeConstKind::Bool(value) => Ok(*value),
            CtfeConstKind::Interned(interned) => match interned.value().value(self.db) {
                SemConstValue::Scalar {
                    value: SemConstScalar::Bool(value),
                    ..
                } => Ok(value),
                _ => Err(CtfeError::InvalidOperation {
                    origin,
                    message: "expected bool".into(),
                }),
            },
            _ => Err(CtfeError::InvalidOperation {
                origin,
                message: "expected bool".into(),
            }),
        }
    }

    fn is_bool_like(&self, value: &CtfeConstValue<'db>) -> bool {
        match &value.kind {
            CtfeConstKind::Bool(_) => true,
            CtfeConstKind::Interned(value) => matches!(
                value.value().value(self.db),
                SemConstValue::Scalar {
                    value: SemConstScalar::Bool(_),
                    ..
                }
            ),
            _ => false,
        }
    }

    fn is_int_like(&self, value: &CtfeConstValue<'db>) -> bool {
        match &value.kind {
            CtfeConstKind::Int { .. } => true,
            CtfeConstKind::Interned(value) => matches!(
                value.value().value(self.db),
                SemConstValue::Scalar {
                    value: SemConstScalar::Int { .. },
                    ..
                }
            ),
            _ => false,
        }
    }

    fn expect_int(
        &self,
        value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<BigInt, CtfeError<'db>> {
        match &value.kind {
            CtfeConstKind::Int { value, .. } => Ok(value.to_bigint()),
            CtfeConstKind::Interned(interned) => match interned.value().value(self.db) {
                SemConstValue::Scalar {
                    value: SemConstScalar::Int { value },
                    ..
                } => Ok(value.clone()),
                _ => Err(CtfeError::InvalidOperation {
                    origin,
                    message: "expected int".into(),
                }),
            },
            _ => Err(CtfeError::InvalidOperation {
                origin,
                message: "expected int".into(),
            }),
        }
    }

    fn expect_matching_int_word(
        &self,
        value: &CtfeConstValue<'db>,
        result_ty: TyId<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<Option<U256>, CtfeError<'db>> {
        let Some((bits, signed)) = int_ty_shape(self.db, result_ty) else {
            return Ok(None);
        };
        match &value.kind {
            CtfeConstKind::Int {
                value:
                    CtfeInt::Word {
                        bits: value_bits,
                        signed: value_signed,
                        word,
                    },
                ..
            } if (*value_bits, *value_signed) == (bits, signed) => Ok(Some(*word)),
            CtfeConstKind::Int { .. } => Ok(None),
            CtfeConstKind::Interned(interned) => match interned.value().value(self.db) {
                SemConstValue::Scalar {
                    ty,
                    value: SemConstScalar::Int { value },
                } if int_ty_shape(self.db, ty) == Some((bits, signed)) => Ok(Some(
                    u256_from_bigint(&normalize_int_to_shape(value.clone(), bits, false)),
                )),
                SemConstValue::Scalar {
                    value: SemConstScalar::Int { .. },
                    ..
                } => Ok(None),
                _ => Err(CtfeError::InvalidOperation {
                    origin,
                    message: "expected int".into(),
                }),
            },
            _ => Err(CtfeError::InvalidOperation {
                origin,
                message: "expected int".into(),
            }),
        }
    }

    fn index_from_value(
        &self,
        value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<usize, CtfeError<'db>> {
        let index = self.expect_int(value, origin)?;
        index.to_usize().ok_or(CtfeError::OutOfBounds { origin })
    }

    fn eval_unary(
        &self,
        frame_idx: usize,
        result_ty: TyId<'db>,
        op: UnOp,
        value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeValue<'db>, CtfeError<'db>> {
        match op {
            UnOp::Plus => Ok(CtfeValue::Value(value)),
            UnOp::Minus => {
                let arithmetic_mode = self.frames[frame_idx]
                    .body
                    .template_owner
                    .arithmetic_mode(self.db);
                if arithmetic_mode == ArithmeticMode::Unchecked
                    && let Some(word) = self.expect_matching_int_word(&value, result_ty, origin)?
                {
                    return Ok(CtfeValue::Value(CtfeConstValue::int_word(
                        self.db,
                        result_ty,
                        word.wrapping_neg(),
                    )));
                }
                let value = self.expect_int(value, origin)?;
                let value =
                    execute_source_int_unary(self.db, result_ty, arithmetic_mode, op, value)
                        .map_err(|fault| primitive_error(origin, fault))?;
                Ok(CtfeValue::Value(CtfeConstValue::int(
                    self.db, result_ty, value,
                )))
            }
            UnOp::Not => Ok(CtfeValue::Value(CtfeConstValue::bool(
                !self.expect_bool(value, origin)?,
            ))),
            UnOp::BitNot => {
                if let Some(word) = self.expect_matching_int_word(&value, result_ty, origin)? {
                    return Ok(CtfeValue::Value(CtfeConstValue::int_word(
                        self.db,
                        result_ty,
                        word.not(),
                    )));
                }
                let int = self.expect_int(value, origin)?;
                Ok(CtfeValue::Value(CtfeConstValue::int(
                    self.db,
                    result_ty,
                    -int - BigInt::one(),
                )))
            }
            UnOp::Mut | UnOp::Ref | UnOp::Deref => Err(CtfeError::InvalidOperation {
                origin,
                message: "unexpected place operator in CTFE unary evaluation".into(),
            }),
        }
    }

    fn eval_binary(
        &self,
        frame_idx: usize,
        result_ty: TyId<'db>,
        op: BinOp,
        lhs: CtfeConstValue<'db>,
        rhs: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeValue<'db>, CtfeError<'db>> {
        match op {
            BinOp::Comp(comp) => self.eval_compare(comp, lhs, rhs, origin),
            BinOp::Logical(logical) => {
                let lhs = self.expect_bool(lhs, origin)?;
                let rhs = self.expect_bool(rhs, origin)?;
                let value = match logical {
                    LogicalBinOp::And => lhs && rhs,
                    LogicalBinOp::Or => lhs || rhs,
                };
                Ok(CtfeValue::Value(CtfeConstValue::bool(value)))
            }
            BinOp::Index => Err(CtfeError::InvalidOperation {
                origin,
                message: "invalid binary op in CTFE expression".into(),
            }),
            BinOp::Arith(ArithBinOp::Range) => Err(CtfeError::NotConstEvaluable { origin }),
            BinOp::Arith(arith) => {
                let arithmetic_mode = self.frames[frame_idx]
                    .body
                    .template_owner
                    .arithmetic_mode(self.db);
                if self.is_bool_like(&lhs) && self.is_bool_like(&rhs) {
                    return match arith {
                        ArithBinOp::BitAnd => Ok(CtfeValue::Value(CtfeConstValue::bool(
                            self.expect_bool(lhs, origin)? & self.expect_bool(rhs, origin)?,
                        ))),
                        ArithBinOp::BitOr => Ok(CtfeValue::Value(CtfeConstValue::bool(
                            self.expect_bool(lhs, origin)? | self.expect_bool(rhs, origin)?,
                        ))),
                        ArithBinOp::BitXor => Ok(CtfeValue::Value(CtfeConstValue::bool(
                            self.expect_bool(lhs, origin)? ^ self.expect_bool(rhs, origin)?,
                        ))),
                        _ => Err(CtfeError::InvalidOperation {
                            origin,
                            message: "expected int".into(),
                        }),
                    };
                }
                let lhs = self.expect_int(lhs, origin)?;
                let rhs = self.expect_int(rhs, origin)?;
                let value = match arith {
                    ArithBinOp::Add
                    | ArithBinOp::Sub
                    | ArithBinOp::Mul
                    | ArithBinOp::Div
                    | ArithBinOp::Rem
                    | ArithBinOp::Pow => execute_source_int_binary(
                        self.db,
                        result_ty,
                        arithmetic_mode,
                        arith,
                        lhs,
                        rhs,
                    )
                    .map_err(|fault| primitive_error(origin, fault))?,
                    ArithBinOp::LShift => self.wrapping_shift(result_ty, lhs, rhs, true, origin)?,
                    ArithBinOp::RShift => {
                        self.wrapping_shift(result_ty, lhs, rhs, false, origin)?
                    }
                    ArithBinOp::BitAnd => {
                        self.bitwise(result_ty, lhs, rhs, |lhs, rhs| lhs & rhs)?
                    }
                    ArithBinOp::BitOr => self.bitwise(result_ty, lhs, rhs, |lhs, rhs| lhs | rhs)?,
                    ArithBinOp::BitXor => {
                        self.bitwise(result_ty, lhs, rhs, |lhs, rhs| lhs ^ rhs)?
                    }
                    ArithBinOp::Range => unreachable!(),
                };
                Ok(CtfeValue::Value(CtfeConstValue::int(
                    self.db, result_ty, value,
                )))
            }
        }
    }

    fn eval_compare(
        &self,
        op: CompBinOp,
        lhs: CtfeConstValue<'db>,
        rhs: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeValue<'db>, CtfeError<'db>> {
        let result = if self.is_bool_like(&lhs) && self.is_bool_like(&rhs) {
            let lhs = self.expect_bool(lhs, origin)?;
            let rhs = self.expect_bool(rhs, origin)?;
            match op {
                CompBinOp::Eq => lhs == rhs,
                CompBinOp::NotEq => lhs != rhs,
                CompBinOp::Lt => !lhs && rhs,
                CompBinOp::LtEq => !lhs || rhs,
                CompBinOp::Gt => lhs && !rhs,
                CompBinOp::GtEq => lhs || !rhs,
            }
        } else if self.is_int_like(&lhs) && self.is_int_like(&rhs) {
            let lhs = self.expect_int(lhs, origin)?;
            let rhs = self.expect_int(rhs, origin)?;
            match op {
                CompBinOp::Eq => lhs == rhs,
                CompBinOp::NotEq => lhs != rhs,
                CompBinOp::Lt => lhs < rhs,
                CompBinOp::LtEq => lhs <= rhs,
                CompBinOp::Gt => lhs > rhs,
                CompBinOp::GtEq => lhs >= rhs,
            }
        } else {
            match op {
                CompBinOp::Eq => {
                    sem_const_eq(self.db, lhs.materialize(self.db), rhs.materialize(self.db))
                }
                CompBinOp::NotEq => {
                    !sem_const_eq(self.db, lhs.materialize(self.db), rhs.materialize(self.db))
                }
                CompBinOp::Lt => self.expect_int(lhs, origin)? < self.expect_int(rhs, origin)?,
                CompBinOp::LtEq => self.expect_int(lhs, origin)? <= self.expect_int(rhs, origin)?,
                CompBinOp::Gt => self.expect_int(lhs, origin)? > self.expect_int(rhs, origin)?,
                CompBinOp::GtEq => self.expect_int(lhs, origin)? >= self.expect_int(rhs, origin)?,
            }
        };
        Ok(CtfeValue::Value(CtfeConstValue::bool(result)))
    }

    fn signed_div_overflows(&self, result_ty: TyId<'db>, lhs: &BigInt, rhs: &BigInt) -> bool {
        if let Some((bits, true)) = int_ty_shape(self.db, result_ty) {
            lhs == &-(BigInt::one() << (usize::from(bits) - 1)) && rhs == &-BigInt::one()
        } else {
            false
        }
    }

    fn checked_pow(
        &self,
        result_ty: TyId<'db>,
        lhs: BigInt,
        rhs: BigInt,
        origin: SemOrigin<'db>,
    ) -> Result<BigInt, CtfeError<'db>> {
        if rhs.sign() == num_bigint::Sign::Minus {
            return Err(CtfeError::NegativeExponent { origin });
        }
        let Some(mut exp) = rhs.to_biguint() else {
            return Err(CtfeError::NegativeExponent { origin });
        };
        let mut acc = BigInt::one();
        let mut base = lhs;
        while !exp.is_zero() {
            if (&exp & BigUint::one()) == BigUint::one() {
                acc *= base.clone();
                if !self.int_in_range(result_ty, &acc) {
                    return Err(CtfeError::ArithmeticOverflow { origin });
                }
            }
            exp >>= 1usize;
            if exp.is_zero() {
                break;
            }
            base = base.clone() * base;
            if !self.int_in_range(result_ty, &base) {
                return Err(CtfeError::ArithmeticOverflow { origin });
            }
        }
        Ok(acc)
    }

    fn wrapping_pow(
        &self,
        result_ty: TyId<'db>,
        lhs: &BigInt,
        rhs: &BigInt,
        origin: SemOrigin<'db>,
    ) -> Result<BigInt, CtfeError<'db>> {
        let Some((bits, _)) = int_ty_shape(self.db, result_ty) else {
            return Err(CtfeError::NotConstEvaluable { origin });
        };
        if bits == 0 {
            return Ok(BigInt::zero());
        }
        let modulus = BigUint::one() << usize::from(bits);
        let base = biguint_from_u256(u256_from_bigint(&normalize_int_to_shape(
            lhs.clone(),
            bits,
            false,
        )));
        let exp = biguint_from_u256(u256_from_bigint(&normalize_int_to_shape(
            rhs.clone(),
            bits,
            false,
        )));
        Ok(BigInt::from_biguint(
            Sign::Plus,
            base.modpow(&exp, &modulus),
        ))
    }

    fn wrapping_shift(
        &self,
        result_ty: TyId<'db>,
        lhs: BigInt,
        rhs: BigInt,
        left: bool,
        origin: SemOrigin<'db>,
    ) -> Result<BigInt, CtfeError<'db>> {
        let Some((bits, signed)) = int_ty_shape(self.db, result_ty) else {
            let Some(shift) = rhs.to_usize() else {
                return Err(CtfeError::InvalidOperation {
                    origin,
                    message: "invalid shift amount".into(),
                });
            };
            return Ok(if left { lhs << shift } else { lhs >> shift });
        };
        let shift_word = u256_from_bigint(&normalize_int_to_shape(rhs, bits, false));
        if shift_word >= U256::from(256u16) {
            return Ok(if left || !signed || lhs.sign() != Sign::Minus {
                BigInt::zero()
            } else {
                -BigInt::one()
            });
        }
        let shift = bigint_from_u256(shift_word)
            .to_usize()
            .expect("shift amount below 256 fits usize");
        Ok(if left { lhs << shift } else { lhs >> shift })
    }

    fn bitwise(
        &self,
        result_ty: TyId<'db>,
        lhs: BigInt,
        rhs: BigInt,
        op: impl Fn(BigInt, BigInt) -> BigInt,
    ) -> Result<BigInt, CtfeError<'db>> {
        let Some((bits, signed)) = int_ty_shape(self.db, result_ty) else {
            return Ok(op(lhs, rhs));
        };
        let lhs = normalize_int_to_shape(lhs, bits, false);
        let rhs = normalize_int_to_shape(rhs, bits, false);
        Ok(normalize_int_to_shape(op(lhs, rhs), bits, signed))
    }

    fn int_in_range(&self, result_ty: TyId<'db>, value: &BigInt) -> bool {
        int_in_range(self.db, result_ty, value)
    }

    fn eval_cast(
        &mut self,
        result_ty: TyId<'db>,
        value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> EvalResult<'db, CtfeValue<'db>> {
        let value = execute_scalar_cast(self.db, result_ty, value.materialize(self.db))
            .map_err(|fault| primitive_error(origin, fault))?;
        self.load_sem_const(value, origin)
    }

    fn make_aggregate_value(
        &self,
        result_ty: TyId<'db>,
        fields: Vec<CtfeConstValue<'db>>,
    ) -> CtfeValue<'db> {
        let value = if result_ty.is_tuple(self.db) {
            CtfeConstValue::tuple(result_ty, fields)
        } else if result_ty.is_array(self.db) {
            CtfeConstValue::array(result_ty, fields)
        } else {
            CtfeConstValue::struct_(result_ty, fields)
        };
        CtfeValue::Value(value)
    }

    fn project_value(
        &self,
        value: CtfeConstValue<'db>,
        path: &[CtfePathElem],
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        let mut value = value;
        for elem in path {
            value = self.expand_interned(value);
            let projected = match (&value.kind, elem) {
                (CtfeConstKind::Tuple { elems, .. }, CtfePathElem::Field(field))
                | (CtfeConstKind::Struct { fields: elems, .. }, CtfePathElem::Field(field)) => {
                    elems
                        .get(field.0 as usize)
                        .cloned()
                        .ok_or(CtfeError::OutOfBounds { origin })?
                }
                (
                    CtfeConstKind::Enum {
                        variant: actual,
                        fields,
                        ..
                    },
                    CtfePathElem::VariantField { variant, field },
                ) if actual == variant => fields
                    .get(field.0 as usize)
                    .cloned()
                    .ok_or(CtfeError::OutOfBounds { origin })?,
                (_, CtfePathElem::Index(index)) => {
                    self.project_index(value.clone(), *index, origin)?
                }
                _ => {
                    return Err(CtfeError::InvalidOperation {
                        origin,
                        message: "invalid const projection".into(),
                    });
                }
            };
            value = projected;
        }
        Ok(value)
    }

    fn expand_interned(&self, value: CtfeConstValue<'db>) -> CtfeConstValue<'db> {
        match value.kind {
            CtfeConstKind::Interned(interned) => {
                CtfeConstValue::expand_sem_const_shallow(self.db, interned)
            }
            kind => CtfeConstValue { kind },
        }
    }

    fn expand_interned_in_place(&self, value: &mut CtfeConstValue<'db>) {
        let interned = match &value.kind {
            CtfeConstKind::Interned(interned) => *interned,
            _ => return,
        };
        *value = CtfeConstValue::expand_sem_const_shallow(self.db, interned);
    }

    fn store_const_value_in_place(
        &self,
        root: &mut CtfeConstValue<'db>,
        path: &[CtfePathElem],
        new_value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), CtfeError<'db>> {
        self.expand_interned_in_place(root);
        let Some((head, tail)) = path.split_first() else {
            *root = new_value;
            return Ok(());
        };
        let root_origin = origin;
        match &mut root.kind {
            CtfeConstKind::Tuple { elems, .. } => {
                let CtfePathElem::Field(field) = head else {
                    return Err(CtfeError::InvalidOperation {
                        origin: root_origin,
                        message: "tuple store requires field projection".into(),
                    });
                };
                let elems = Rc::make_mut(elems);
                let slot = elems
                    .get_mut(field.0 as usize)
                    .ok_or(CtfeError::OutOfBounds { origin })?;
                self.store_const_value_in_place(slot, tail, new_value, origin)?;
            }
            CtfeConstKind::Struct { fields, .. } => {
                let CtfePathElem::Field(field) = head else {
                    return Err(CtfeError::InvalidOperation {
                        origin: root_origin,
                        message: "struct store requires field projection".into(),
                    });
                };
                let fields = Rc::make_mut(fields);
                let slot = fields
                    .get_mut(field.0 as usize)
                    .ok_or(CtfeError::OutOfBounds { origin })?;
                self.store_const_value_in_place(slot, tail, new_value, origin)?;
            }
            CtfeConstKind::Array { elems, .. } => {
                let CtfePathElem::Index(index) = head else {
                    return Err(CtfeError::InvalidOperation {
                        origin: root_origin,
                        message: "array store requires index projection".into(),
                    });
                };
                let elems = Rc::make_mut(elems);
                let slot = elems
                    .get_mut(*index)
                    .ok_or(CtfeError::OutOfBounds { origin })?;
                self.store_const_value_in_place(slot, tail, new_value, origin)?;
            }
            CtfeConstKind::Enum {
                variant, fields, ..
            } => {
                let CtfePathElem::VariantField {
                    variant: expected,
                    field,
                } = head
                else {
                    return Err(CtfeError::InvalidOperation {
                        origin: root_origin,
                        message: "enum store requires variant field projection".into(),
                    });
                };
                if *variant != *expected {
                    return Err(CtfeError::VariantMismatch {
                        origin: root_origin,
                    });
                }
                let fields = Rc::make_mut(fields);
                let slot = fields
                    .get_mut(field.0 as usize)
                    .ok_or(CtfeError::OutOfBounds { origin })?;
                self.store_const_value_in_place(slot, tail, new_value, origin)?;
            }
            _ => {
                return Err(CtfeError::InvalidOperation {
                    origin: root_origin,
                    message: "invalid CTFE store target".into(),
                });
            }
        }
        Ok(())
    }

    fn bump(&mut self, origin: SemOrigin<'db>) -> Result<(), CtfeError<'db>> {
        self.steps += 1;
        if self.steps > self.config.step_limit {
            return Err(CtfeError::StepLimitExceeded { origin });
        }
        Ok(())
    }

    fn const_as_bytes(
        &self,
        value: &CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<Vec<u8>, CtfeError<'db>> {
        let value = self.expand_interned(value.clone());
        match &value.kind {
            CtfeConstKind::Bool(flag) => Ok(vec![u8::from(*flag)]),
            CtfeConstKind::Int { ty, value } => {
                let Some((bits, _)) = int_ty_shape(self.db, *ty) else {
                    return Err(CtfeError::NotConstEvaluable { origin });
                };
                let width = usize::from(bits / 8);
                match value {
                    CtfeInt::Word { word, .. } => {
                        Ok(word.to_be_bytes::<32>()[32 - width..].to_vec())
                    }
                    CtfeInt::Big(value) => {
                        let (_, bytes) =
                            normalize_int_to_shape(value.clone(), bits, false).to_bytes_be();
                        if bytes.len() > width {
                            return Err(CtfeError::NotConstEvaluable { origin });
                        }
                        let mut out = vec![0u8; width];
                        let offset = width - bytes.len();
                        out[offset..].copy_from_slice(&bytes);
                        Ok(out)
                    }
                }
            }
            CtfeConstKind::Bytes { bytes, .. } => Ok(bytes.to_vec()),
            CtfeConstKind::Tuple { elems, .. } | CtfeConstKind::Array { elems, .. } => {
                let mut out = Vec::new();
                for elem in elems.iter() {
                    out.extend(self.const_as_bytes(elem, origin)?);
                }
                Ok(out)
            }
            CtfeConstKind::Struct { fields, .. } => {
                let mut out = Vec::new();
                for field in fields.iter() {
                    out.extend(self.const_as_bytes(field, origin)?);
                }
                Ok(out)
            }
            CtfeConstKind::Enum { ty, variant, .. } if ty.is_unit_variant_only_enum(self.db) => {
                let width = 32;
                let (_, bytes) = BigInt::from(variant.0).to_bytes_be();
                let mut out = vec![0u8; width];
                let offset = width - bytes.len();
                out[offset..].copy_from_slice(&bytes);
                Ok(out)
            }
            CtfeConstKind::Unit | CtfeConstKind::Interned(_) | CtfeConstKind::Enum { .. } => {
                Err(CtfeError::NotConstEvaluable { origin })
            }
        }
    }
}

#[derive(Clone)]
struct ResolvedPlace {
    frame: usize,
    root: SLocalId,
    path: Vec<CtfePathElem>,
}

fn is_u8_array_ty<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    if !ty.is_array(db) {
        return false;
    }
    let (_, args) = ty.decompose_ty_app(db);
    matches!(
        args.first().copied().map(|ty| ty.base_ty(db).data(db)),
        Some(TyData::TyBase(TyBase::Prim(PrimTy::U8)))
    )
}

fn array_len<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<usize> {
    let (_, args) = ty.decompose_ty_app(db);
    let TyData::ConstTy(const_ty) = args.get(1)?.data(db) else {
        return None;
    };
    const_ty.integer_value(db)?.to_usize()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{analysis::semantic::identity_semantic_instance_key, test_db::HirAnalysisTestDb};

    #[test]
    fn unverified_semantic_value_cannot_enter_machine_storage() {
        let db = HirAnalysisTestDb::default();
        let malformed = SemConstId::new(
            &db,
            SemConstValue::Scalar {
                ty: TyId::bool(&db),
                value: SemConstScalar::Int {
                    value: BigInt::from(1),
                },
            },
        );
        let mut machine = CtfeMachine::new(&db, CtfeConfig::default());
        assert!(matches!(
            machine.load_sem_const(malformed, SemOrigin::Synthetic),
            Err(EvalStop::Failed(EvalFailure::Invariant { .. }))
        ));
    }

    #[test]
    fn replay_root_rejects_live_reference_input_before_creating_a_frame() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "internal_ctfe_reference_input.fe".into(),
            "const fn anchor(_ value: u256) -> u256 { value }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(module.all_funcs(&db)[0])),
        );
        let mut machine = CtfeMachine::new(&db, CtfeConfig::default());
        let live_reference = CtfeValue::Ref(CtfeRef {
            frame: 0,
            root: SLocalId::new(0),
            path: Box::new([]),
        });
        assert!(matches!(
            machine.eval_root(instance, vec![live_reference], SemOrigin::Synthetic),
            Err(EvalStop::Failed(EvalFailure::Invariant { .. }))
        ));
        assert!(machine.frames.is_empty());
    }

    #[test]
    fn typed_use_value_reads_referent_and_forward_keeps_reference() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "internal_ctfe_read.fe".into(),
            "const fn read(_ value: ref u256) -> u256 { value }\nconst fn anchor() -> u256 { 7 }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let funcs = module.all_funcs(&db);
        let read = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(funcs[0])),
        );
        let anchor = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(funcs[1])),
        );
        let read_body = read.body(&db);
        let anchor_body = anchor.body(&db);
        let (dst, src, origin) = read_body
            .blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .find_map(|stmt| match &stmt.kind {
                SStmtKind::Assign {
                    dst,
                    expr: SExpr::UseValue(src),
                } => Some((*dst, *src, stmt.origin)),
                _ => None,
            })
            .expect("reference read must lower as UseValue");
        let u256_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        let root = SLocalId::new(
            anchor_body
                .locals
                .iter()
                .position(|local| local.ty == u256_ty)
                .expect("anchor has a u256 local"),
        );
        let mut anchor_locals = vec![CtfeSlot::Uninit; anchor_body.locals.len()];
        anchor_locals[root.index()] = CtfeSlot::Init(CtfeValue::Value(CtfeConstValue::int(
            &db,
            u256_ty,
            BigInt::from(7),
        )));
        let mut read_locals = vec![CtfeSlot::Uninit; read_body.locals.len()];
        read_locals[src.value.index()] = CtfeSlot::Init(CtfeValue::Ref(CtfeRef {
            frame: 0,
            root,
            path: Box::new([]),
        }));
        let mut machine = CtfeMachine::new(&db, CtfeConfig::default());
        machine.frames.push(CtfeFrame {
            body: anchor_body,
            locals: anchor_locals,
            current: 0,
        });
        machine.frames.push(CtfeFrame {
            body: read_body,
            locals: read_locals,
            current: 0,
        });
        assert!(matches!(
            machine.eval_expr(1, u256_ty, SExpr::Forward(src), origin),
            Ok(CtfeValue::Ref(_))
        ));
        let CtfeValue::Value(value) = machine
            .eval_expr(
                1,
                read_body.locals[dst.index()].ty,
                SExpr::UseValue(src),
                origin,
            )
            .expect("admitted machine reference should be readable")
        else {
            panic!("typed value read must load the referent");
        };
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } = value.materialize(&db).value(&db)
        else {
            panic!("expected integer referent");
        };
        assert_eq!(value, BigInt::from(7));
    }

    #[test]
    fn blocked_attempt_discards_nested_reference_writes() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "internal_ctfe_reference_retry.fe".into(),
            "const fn read(_ value: ref u256) -> u256 { value }\nconst fn anchor() -> u256 { 7 }\nconst fn dependent<const N: u256>() -> u256 { N }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let funcs = module.all_funcs(&db);
        let read = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(funcs[0])),
        );
        let anchor = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(funcs[1])),
        );
        let dependent = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(funcs[2])),
        );
        let read_body = read.body(&db);
        let anchor_body = anchor.body(&db);
        let dependent_body = dependent.body(&db);
        let (src, origin) = read_body
            .blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .find_map(|stmt| match &stmt.kind {
                SStmtKind::Assign {
                    expr: SExpr::UseValue(src),
                    ..
                } => Some((*src, stmt.origin)),
                _ => None,
            })
            .expect("reference read must lower as UseValue");
        let symbolic = dependent_body
            .blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .find_map(|stmt| match &stmt.kind {
                SStmtKind::Assign {
                    expr: SExpr::Const(SConst::Evidence(value)),
                    ..
                } if sem_const_dependency(&db, *value).is_some() => Some(*value),
                _ => None,
            })
            .expect("generic parameter must remain a dependency");
        let u256_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        let root = SLocalId::new(
            anchor_body
                .locals
                .iter()
                .position(|local| local.ty == u256_ty)
                .expect("anchor has a u256 local"),
        );
        let mut anchor_locals = vec![CtfeSlot::Uninit; anchor_body.locals.len()];
        anchor_locals[root.index()] = CtfeSlot::Init(CtfeValue::Value(CtfeConstValue::int(
            &db,
            u256_ty,
            BigInt::from(7),
        )));
        let mut read_locals = vec![CtfeSlot::Uninit; read_body.locals.len()];
        read_locals[src.value.index()] = CtfeSlot::Init(CtfeValue::Ref(CtfeRef {
            frame: 0,
            root,
            path: Box::new([]),
        }));

        let mut attempt = CtfeMachine::new(&db, CtfeConfig::default());
        attempt.frames.push(CtfeFrame {
            body: anchor_body,
            locals: anchor_locals.clone(),
            current: 0,
        });
        attempt.frames.push(CtfeFrame {
            body: read_body,
            locals: read_locals.clone(),
            current: 0,
        });
        let place = attempt
            .resolve_place(1, &SPlace::new(src.value), origin)
            .expect("nested reference resolves into the caller frame");
        assert_eq!(place.frame, 0);
        attempt
            .store_place(
                place,
                CtfeConstValue::int(&db, u256_ty, BigInt::from(9)),
                origin,
            )
            .expect("nested reference store succeeds inside the attempt");
        assert!(matches!(
            attempt.load_sem_const(symbolic, origin),
            Err(EvalStop::Blocked(_))
        ));
        let CtfeValue::Value(changed) = attempt.read_slot(0, root, origin).unwrap() else {
            panic!("anchor root must remain a value");
        };
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value: changed },
            ..
        } = changed.materialize(&db).value(&db)
        else {
            panic!("anchor root must remain an integer");
        };
        assert_eq!(changed, BigInt::from(9));
        drop(attempt);

        let mut retry = CtfeMachine::new(&db, CtfeConfig::default());
        retry.frames.push(CtfeFrame {
            body: anchor_body,
            locals: anchor_locals,
            current: 0,
        });
        retry.frames.push(CtfeFrame {
            body: read_body,
            locals: read_locals,
            current: 0,
        });
        let CtfeValue::Value(original) = retry.read_slot(0, root, origin).unwrap() else {
            panic!("retry root must remain a value");
        };
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value: original },
            ..
        } = original.materialize(&db).value(&db)
        else {
            panic!("retry root must remain an integer");
        };
        assert_eq!(original, BigInt::from(7));
    }

    #[test]
    fn borrow_through_pointer_is_rejected() {
        // A pointer argument cannot come from a verified request, so reach the
        // borrow directly: memory outside CTFE frames is not addressable.
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "internal_ctfe_pointer_borrow.fe".into(),
            "const fn external(p: *u8) -> mut u8 { mut *p }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(module.all_funcs(&db)[0])),
        );
        let body = instance.body(&db);
        let (dst, expr, origin) = body
            .blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .find_map(|stmt| match &stmt.kind {
                SStmtKind::Assign {
                    dst,
                    expr:
                        expr @ SExpr::Borrow {
                            provider: Some(ProviderAddressSpace::Memory),
                            place,
                            ..
                        },
                } if place
                    .path
                    .iter()
                    .any(|elem| matches!(elem, Projection::Deref)) =>
                {
                    Some((*dst, expr.clone(), stmt.origin))
                }
                _ => None,
            })
            .expect("`mut *p` must lower as a memory borrow through a dereference");
        let mut machine = CtfeMachine::new(&db, CtfeConfig::default());
        machine.frames.push(CtfeFrame {
            body,
            locals: vec![CtfeSlot::Uninit; body.locals.len()],
            current: 0,
        });
        assert!(matches!(
            machine.eval_expr(0, body.locals[dst.index()].ty, expr, origin),
            Err(EvalStop::Failed(EvalFailure::Ctfe(
                CtfeError::InvalidProviderUse { .. }
            )))
        ));
    }

    #[test]
    fn raw_core_numeric_edges_keep_evm_word_semantics() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "raw_numeric_edges.fe".into(),
            "const fn anchor() -> u8 { 0 }",
        );
        let (module, _) = db.top_mod(file);
        let origin = SemOrigin::Body(BodyOwner::Func(module.all_funcs(&db)[0]));
        let u8_ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::U8)));
        let machine = CtfeMachine::new(&db, CtfeConfig::default());

        for (op, lhs, rhs, expected) in [
            (ArithBinOp::Div, 9, 0, 0),
            (ArithBinOp::Rem, 9, 0, 0),
            (ArithBinOp::Pow, 2, 8, 0),
        ] {
            let args = [lhs, rhs].map(|value| CtfeConstValue::int(&db, u8_ty, value.into()));
            let actual = machine
                .eval_numeric_extern_intrinsic(
                    NumericExternIntrinsic::WrappingBinary(op),
                    u8_ty,
                    &args,
                    origin,
                )
                .unwrap();
            let SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } = actual.materialize(&db).value(&db)
            else {
                panic!("expected numeric intrinsic result");
            };
            assert_eq!(value, BigInt::from(expected), "{op:?}");
        }
    }
}
