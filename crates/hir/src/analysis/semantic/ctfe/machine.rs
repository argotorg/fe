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
            FieldIndex, SConst, SExpr, SLocalId, SOperand, SPlace, SStmt, SStmtKind,
            STerminatorKind, SemConstId, SemConstScalar, SemConstValue, SemOrigin, SemanticBody,
            SemanticConstRef, VariantIndex, array_const, bool_const, bytes_const,
            consts::demand_concrete_const_ty, enum_const, int_const, int_ty_shape,
            normalize_int_to_shape, runtime_size_bytes, sem_const_eq, sem_const_from_ty,
            sem_const_ty, struct_const, tuple_const, unit_const,
        },
        ty::{
            const_expr::{ConstExpr, ConstExprId},
            const_ty::{ConstTyData, ConstTyId, EvaluatedConstTy, const_ty_from_sem_const},
            corelib::{
                PrimitiveWrapperCallKind, RuntimeBuiltinFuncKind, core_primitive_wrapper_call_kind,
                runtime_builtin_func_kind,
            },
            normalize::normalize_ty,
            ty_check::{BodyOwner, check_anon_const_body, check_const_body, check_func_body},
            ty_def::{PrimTy, TyBase, TyData, TyId},
        },
    },
    core::hir_def::expr::LogicalBinOp,
    hir_def::{ArithBinOp, BinOp, CompBinOp, UnOp, attr::ArithmeticMode},
    projection::{IndexSource, Projection},
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

#[derive(Clone, Copy)]
enum NumericExternIntrinsic {
    CheckedBinary(ArithBinOp),
    WrappingBinary(ArithBinOp),
    SaturatingBinary(SaturatingArithmetic),
    Comparison(CompBinOp),
    BoolBinary(ArithBinOp),
    CheckedNeg,
    WrappingNeg,
    BitNot,
    BoolNot,
}

#[derive(Clone, Copy)]
enum SaturatingArithmetic {
    Add,
    Sub,
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
) -> Result<SemConstId<'db>, CtfeError<'db>> {
    let mut machine = CtfeMachine::new(db, CtfeConfig::default());
    machine.eval_root(
        instance,
        Vec::new(),
        SemOrigin::Body(instance.key(db).owner(db)),
    )
}

fn eval_const_instance_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<SemConstId<'db>, CtfeError<'db>> {
    Err(CtfeError::RecursiveConst {
        origin: SemOrigin::Body(instance.key(db).owner(db)),
    })
}

fn eval_const_instance_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Result<SemConstId<'db>, CtfeError<'db>>,
    _count: u32,
    _instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<Result<SemConstId<'db>, CtfeError<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

#[salsa::tracked(cycle_initial=eval_const_ref_cycle_initial, cycle_fn=eval_const_ref_cycle_recover)]
pub fn eval_const_ref<'db>(
    db: &'db dyn HirAnalysisDb,
    cref: SemanticConstRef<'db>,
) -> Result<SemConstId<'db>, CtfeError<'db>> {
    let mut machine = CtfeMachine::new(db, CtfeConfig::default());
    machine.eval_root(
        SemanticInstance::new(db, cref.instance(db)),
        Vec::new(),
        cref.origin(db),
    )
}

fn eval_const_ref_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    cref: SemanticConstRef<'db>,
) -> Result<SemConstId<'db>, CtfeError<'db>> {
    Err(CtfeError::RecursiveConst {
        origin: cref.origin(db),
    })
}

fn eval_const_ref_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Result<SemConstId<'db>, CtfeError<'db>>,
    _count: u32,
    _cref: SemanticConstRef<'db>,
) -> salsa::CycleRecoveryAction<Result<SemConstId<'db>, CtfeError<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

#[salsa::tracked(cycle_initial=eval_body_owner_const_cycle_initial, cycle_fn=eval_body_owner_const_cycle_recover)]
pub fn eval_body_owner_const<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    generic_args: Vec<crate::analysis::ty::ty_def::TyId<'db>>,
) -> Result<SemConstId<'db>, CtfeError<'db>> {
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
) -> Result<SemConstId<'db>, CtfeError<'db>> {
    Err(CtfeError::RecursiveConst {
        origin: SemOrigin::Body(owner),
    })
}

fn eval_body_owner_const_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Result<SemConstId<'db>, CtfeError<'db>>,
    _count: u32,
    _owner: BodyOwner<'db>,
    _generic_args: Vec<crate::analysis::ty::ty_def::TyId<'db>>,
) -> salsa::CycleRecoveryAction<Result<SemConstId<'db>, CtfeError<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

#[salsa::tracked(cycle_initial=eval_body_owner_const_with_args_cycle_initial, cycle_fn=eval_body_owner_const_with_args_cycle_recover)]
pub fn eval_body_owner_const_with_args<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    generic_args: Vec<crate::analysis::ty::ty_def::TyId<'db>>,
    args: Vec<SemConstId<'db>>,
) -> Result<SemConstId<'db>, CtfeError<'db>> {
    let key = SemanticInstanceKey::new(
        db,
        owner,
        GenericSubst::new(db, generic_args),
        crate::analysis::semantic::EffectProviderSubst::empty(db),
        ImplEnv::empty(db, owner.scope()),
    );
    let instance = get_or_build_semantic_instance(db, key);
    let mut machine = CtfeMachine::new(db, CtfeConfig::default());
    machine.eval_root(
        instance,
        args.into_iter()
            .map(|arg| CtfeValue::concrete(db, arg))
            .collect(),
        SemOrigin::Body(owner),
    )
}

fn eval_body_owner_const_with_args_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    _generic_args: Vec<crate::analysis::ty::ty_def::TyId<'db>>,
    _args: Vec<SemConstId<'db>>,
) -> Result<SemConstId<'db>, CtfeError<'db>> {
    Err(CtfeError::RecursiveConst {
        origin: SemOrigin::Body(owner),
    })
}

fn eval_body_owner_const_with_args_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Result<SemConstId<'db>, CtfeError<'db>>,
    _count: u32,
    _owner: BodyOwner<'db>,
    _generic_args: Vec<crate::analysis::ty::ty_def::TyId<'db>>,
    _args: Vec<SemConstId<'db>>,
) -> salsa::CycleRecoveryAction<Result<SemConstId<'db>, CtfeError<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

pub(super) fn try_eval_expr_to_const<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    result_ty: TyId<'db>,
    expr: &SExpr<'db>,
    locals: &[Option<SemConstId<'db>>],
    origin: SemOrigin<'db>,
) -> Option<SemConstId<'db>> {
    let mut machine = CtfeMachine::new(db, CtfeConfig::default());
    machine
        .eval_expr_with_locals(instance, result_ty, expr.clone(), locals, origin)
        .ok()
}

struct CtfeMachine<'db> {
    db: &'db dyn HirAnalysisDb,
    config: CtfeConfig,
    steps: usize,
    instance_cache: FxHashMap<SemanticInstanceKey<'db>, SemanticInstance<'db>>,
    body_cache: FxHashMap<SemanticInstanceKey<'db>, Rc<SemanticBody<'db>>>,
    frames: Vec<CtfeFrame<'db>>,
    /// Memoized results of const-item references evaluated by this machine.
    const_results: FxHashMap<SemanticInstanceKey<'db>, Result<SemConstId<'db>, CtfeError<'db>>>,
    /// Const items currently being evaluated, outermost first. A reference to
    /// a const already on this stack is a recursive definition; the machine
    /// owns this check so const recursion never becomes a salsa query cycle.
    const_stack: Vec<SemanticInstanceKey<'db>>,
}

struct CtfeFrame<'db> {
    body: Rc<SemanticBody<'db>>,
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
    deferred_origin: Option<SemOrigin<'db>>,
}

#[derive(Clone)]
enum CtfeConstKind<'db> {
    Interned(SemConstId<'db>),
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
            deferred_origin: None,
        }
    }

    fn bool(value: bool) -> Self {
        Self {
            kind: CtfeConstKind::Bool(value),
            deferred_origin: None,
        }
    }

    fn int(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, value: BigInt) -> Self {
        Self {
            kind: CtfeConstKind::Int {
                ty,
                value: CtfeInt::from_bigint(db, ty, value),
            },
            deferred_origin: None,
        }
    }

    fn int_word(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, word: U256) -> Self {
        let value = match int_ty_shape(db, ty) {
            Some((bits, signed)) => CtfeInt::from_word(bits, signed, word),
            None => CtfeInt::Big(bigint_from_u256(word)),
        };
        Self {
            kind: CtfeConstKind::Int { ty, value },
            deferred_origin: None,
        }
    }

    fn bytes(ty: TyId<'db>, bytes: Vec<u8>) -> Self {
        Self {
            kind: CtfeConstKind::Bytes {
                ty,
                bytes: bytes.into(),
            },
            deferred_origin: None,
        }
    }

    fn tuple(ty: TyId<'db>, elems: Vec<CtfeConstValue<'db>>) -> Self {
        Self {
            kind: CtfeConstKind::Tuple {
                ty,
                elems: elems.into(),
            },
            deferred_origin: None,
        }
    }

    fn struct_(ty: TyId<'db>, fields: Vec<CtfeConstValue<'db>>) -> Self {
        Self {
            kind: CtfeConstKind::Struct {
                ty,
                fields: fields.into(),
            },
            deferred_origin: None,
        }
    }

    fn array(ty: TyId<'db>, elems: Vec<CtfeConstValue<'db>>) -> Self {
        Self {
            kind: CtfeConstKind::Array {
                ty,
                elems: elems.into(),
            },
            deferred_origin: None,
        }
    }

    fn enum_(ty: TyId<'db>, variant: VariantIndex, fields: Vec<CtfeConstValue<'db>>) -> Self {
        Self {
            kind: CtfeConstKind::Enum {
                ty,
                variant,
                fields: fields.into(),
            },
            deferred_origin: None,
        }
    }

    fn concrete(db: &'db dyn HirAnalysisDb, value: SemConstId<'db>) -> Self {
        let kind = match value.value(db) {
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
            SemConstValue::TypeLevel { .. }
            | SemConstValue::Tuple { .. }
            | SemConstValue::Struct { .. }
            | SemConstValue::Array { .. }
            | SemConstValue::Enum { .. } => CtfeConstKind::Interned(value),
        };
        Self {
            kind,
            deferred_origin: None,
        }
    }

    fn expand_sem_const_shallow(db: &'db dyn HirAnalysisDb, value: SemConstId<'db>) -> Self {
        let kind = match value.value(db) {
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
            SemConstValue::TypeLevel { .. } => CtfeConstKind::Interned(value),
            SemConstValue::Tuple { ty, elems } => CtfeConstKind::Tuple {
                ty,
                elems: elems
                    .iter()
                    .copied()
                    .map(|elem| Self::concrete(db, elem))
                    .collect::<Vec<_>>()
                    .into(),
            },
            SemConstValue::Struct { ty, fields } => CtfeConstKind::Struct {
                ty,
                fields: fields
                    .iter()
                    .copied()
                    .map(|field| Self::concrete(db, field))
                    .collect::<Vec<_>>()
                    .into(),
            },
            SemConstValue::Array { ty, elems } => CtfeConstKind::Array {
                ty,
                elems: elems
                    .iter()
                    .copied()
                    .map(|elem| Self::concrete(db, elem))
                    .collect::<Vec<_>>()
                    .into(),
            },
            SemConstValue::Enum {
                ty,
                variant,
                fields,
            } => CtfeConstKind::Enum {
                ty,
                variant,
                fields: fields
                    .iter()
                    .copied()
                    .map(|field| Self::concrete(db, field))
                    .collect::<Vec<_>>()
                    .into(),
            },
        };
        Self {
            kind,
            deferred_origin: None,
        }
    }

    fn with_deferred_origin(
        mut self,
        db: &'db dyn HirAnalysisDb,
        deferred_origin: Option<SemOrigin<'db>>,
    ) -> Self {
        self.set_deferred_origin(db, deferred_origin);
        self
    }

    fn set_deferred_origin(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        deferred_origin: Option<SemOrigin<'db>>,
    ) {
        self.deferred_origin = if deferred_origin.is_some() && self.contains_type_level(db) {
            deferred_origin
        } else {
            None
        };
    }

    fn error_origin(&self, origin: SemOrigin<'db>) -> SemOrigin<'db> {
        self.deferred_origin.unwrap_or(origin)
    }

    fn materialize(&self, db: &'db dyn HirAnalysisDb) -> SemConstId<'db> {
        match &self.kind {
            CtfeConstKind::Interned(value) => *value,
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
            CtfeConstKind::Interned(value) => sem_const_ty(db, *value),
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
                matches!(value.value(db), SemConstValue::Scalar { .. })
            }
            CtfeConstKind::Unit
            | CtfeConstKind::Tuple { .. }
            | CtfeConstKind::Struct { .. }
            | CtfeConstKind::Array { .. }
            | CtfeConstKind::Enum { .. } => false,
        }
    }

    fn contains_type_level(&self, db: &'db dyn HirAnalysisDb) -> bool {
        match &self.kind {
            CtfeConstKind::Interned(value) => sem_const_contains_type_level(db, *value),
            CtfeConstKind::Tuple { elems, .. } | CtfeConstKind::Array { elems, .. } => {
                elems.iter().any(|elem| elem.contains_type_level(db))
            }
            CtfeConstKind::Struct { fields, .. } | CtfeConstKind::Enum { fields, .. } => {
                fields.iter().any(|field| field.contains_type_level(db))
            }
            CtfeConstKind::Unit
            | CtfeConstKind::Bool(_)
            | CtfeConstKind::Int { .. }
            | CtfeConstKind::Bytes { .. } => false,
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
    machine: &CtfeMachine<'db>,
    result_ty: TyId<'db>,
    origin: SemOrigin<'db>,
) -> Result<BigInt, CtfeError<'db>> {
    if !machine.int_in_range(result_ty, &value) {
        return Err(CtfeError::ArithmeticOverflow { origin });
    }
    Ok(value)
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

fn numeric_extern_intrinsic(name: &str) -> Option<NumericExternIntrinsic> {
    Some(match name {
        "__checked_add" => NumericExternIntrinsic::CheckedBinary(ArithBinOp::Add),
        "__checked_sub" => NumericExternIntrinsic::CheckedBinary(ArithBinOp::Sub),
        "__checked_mul" => NumericExternIntrinsic::CheckedBinary(ArithBinOp::Mul),
        "__checked_div" => NumericExternIntrinsic::CheckedBinary(ArithBinOp::Div),
        "__checked_rem" => NumericExternIntrinsic::CheckedBinary(ArithBinOp::Rem),
        "__checked_pow" => NumericExternIntrinsic::CheckedBinary(ArithBinOp::Pow),
        "__checked_neg" => NumericExternIntrinsic::CheckedNeg,
        "__saturating_add" => NumericExternIntrinsic::SaturatingBinary(SaturatingArithmetic::Add),
        "__saturating_sub" => NumericExternIntrinsic::SaturatingBinary(SaturatingArithmetic::Sub),
        "__saturating_mul" => NumericExternIntrinsic::SaturatingBinary(SaturatingArithmetic::Mul),
        "__not_bool" => NumericExternIntrinsic::BoolNot,
        "__bitand_bool" => NumericExternIntrinsic::BoolBinary(ArithBinOp::BitAnd),
        "__bitor_bool" => NumericExternIntrinsic::BoolBinary(ArithBinOp::BitOr),
        "__bitxor_bool" => NumericExternIntrinsic::BoolBinary(ArithBinOp::BitXor),
        "__eq_bool" => NumericExternIntrinsic::Comparison(CompBinOp::Eq),
        "__ne_bool" => NumericExternIntrinsic::Comparison(CompBinOp::NotEq),
        _ => {
            let suffix = |prefix| {
                name.strip_prefix(prefix)
                    .filter(|suffix| has_integer_numeric_suffix(suffix))
            };
            if suffix("__add_").is_some() {
                NumericExternIntrinsic::WrappingBinary(ArithBinOp::Add)
            } else if suffix("__sub_").is_some() {
                NumericExternIntrinsic::WrappingBinary(ArithBinOp::Sub)
            } else if suffix("__mul_").is_some() {
                NumericExternIntrinsic::WrappingBinary(ArithBinOp::Mul)
            } else if suffix("__div_").is_some() {
                NumericExternIntrinsic::WrappingBinary(ArithBinOp::Div)
            } else if suffix("__rem_").is_some() {
                NumericExternIntrinsic::WrappingBinary(ArithBinOp::Rem)
            } else if suffix("__pow_").is_some() {
                NumericExternIntrinsic::WrappingBinary(ArithBinOp::Pow)
            } else if suffix("__shl_").is_some() {
                NumericExternIntrinsic::WrappingBinary(ArithBinOp::LShift)
            } else if suffix("__shr_").is_some() {
                NumericExternIntrinsic::WrappingBinary(ArithBinOp::RShift)
            } else if suffix("__bitand_").is_some() {
                NumericExternIntrinsic::WrappingBinary(ArithBinOp::BitAnd)
            } else if suffix("__bitor_").is_some() {
                NumericExternIntrinsic::WrappingBinary(ArithBinOp::BitOr)
            } else if suffix("__bitxor_").is_some() {
                NumericExternIntrinsic::WrappingBinary(ArithBinOp::BitXor)
            } else if suffix("__eq_").is_some() {
                NumericExternIntrinsic::Comparison(CompBinOp::Eq)
            } else if suffix("__ne_").is_some() {
                NumericExternIntrinsic::Comparison(CompBinOp::NotEq)
            } else if suffix("__lt_").is_some() {
                NumericExternIntrinsic::Comparison(CompBinOp::Lt)
            } else if suffix("__le_").is_some() {
                NumericExternIntrinsic::Comparison(CompBinOp::LtEq)
            } else if suffix("__gt_").is_some() {
                NumericExternIntrinsic::Comparison(CompBinOp::Gt)
            } else if suffix("__ge_").is_some() {
                NumericExternIntrinsic::Comparison(CompBinOp::GtEq)
            } else if suffix("__neg_").is_some() {
                NumericExternIntrinsic::WrappingNeg
            } else if suffix("__bitnot_").is_some() {
                NumericExternIntrinsic::BitNot
            } else {
                return None;
            }
        }
    })
}

fn has_integer_numeric_suffix(suffix: &str) -> bool {
    matches!(
        suffix,
        "u8" | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "u256"
            | "usize"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "i256"
            | "isize"
    )
}

fn sem_const_contains_type_level<'db>(db: &'db dyn HirAnalysisDb, value: SemConstId<'db>) -> bool {
    match value.value(db) {
        SemConstValue::TypeLevel { .. } => true,
        SemConstValue::Tuple { elems, .. } | SemConstValue::Array { elems, .. } => elems
            .iter()
            .copied()
            .any(|elem| sem_const_contains_type_level(db, elem)),
        SemConstValue::Struct { fields, .. } | SemConstValue::Enum { fields, .. } => fields
            .iter()
            .copied()
            .any(|field| sem_const_contains_type_level(db, field)),
        SemConstValue::Unit | SemConstValue::Scalar { .. } => false,
    }
}

impl<'db> CtfeValue<'db> {
    fn concrete(db: &'db dyn HirAnalysisDb, value: SemConstId<'db>) -> Self {
        Self::Value(CtfeConstValue::concrete(db, value))
    }

    fn deferred(
        db: &'db dyn HirAnalysisDb,
        value: SemConstId<'db>,
        origin: SemOrigin<'db>,
    ) -> Self {
        Self::Value(CtfeConstValue::concrete(db, value).with_deferred_origin(db, Some(origin)))
    }
}

#[derive(Clone)]
struct CtfeRef {
    frame: usize,
    root: SLocalId,
    path: Box<[CtfePathElem]>,
}

#[derive(Clone)]
pub(super) enum CtfePathElem {
    Field(FieldIndex),
    VariantField {
        variant: VariantIndex,
        field: FieldIndex,
    },
    Index(usize),
}

impl<'db> CtfeMachine<'db> {
    fn new(db: &'db dyn HirAnalysisDb, config: CtfeConfig) -> Self {
        Self {
            db,
            config,
            steps: 0,
            instance_cache: FxHashMap::default(),
            body_cache: FxHashMap::default(),
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

    fn body_for_instance(&mut self, instance: SemanticInstance<'db>) -> Rc<SemanticBody<'db>> {
        let key = instance.key(self.db);
        if let Some(body) = self.body_cache.get(&key) {
            return body.clone();
        }
        let body = Rc::new(instance.body(self.db));
        self.body_cache.insert(key, body.clone());
        body
    }

    fn eval_root(
        &mut self,
        instance: SemanticInstance<'db>,
        args: Vec<CtfeValue<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<SemConstId<'db>, CtfeError<'db>> {
        let value = self.eval_instance(instance, args, origin)?;
        let CtfeValue::Value(value) = value else {
            return Err(CtfeError::InvalidBorrow { origin });
        };
        Ok(value.materialize(self.db))
    }

    fn eval_expr_with_locals(
        &mut self,
        instance: SemanticInstance<'db>,
        result_ty: TyId<'db>,
        expr: SExpr<'db>,
        locals: &[Option<SemConstId<'db>>],
        origin: SemOrigin<'db>,
    ) -> Result<SemConstId<'db>, CtfeError<'db>> {
        let body = self.body_for_instance(instance);
        let mut frame_locals = vec![CtfeSlot::Uninit; body.locals.len()];
        for (idx, value) in locals.iter().copied().enumerate() {
            if let Some(value) = value
                && let Some(slot) = frame_locals.get_mut(idx)
            {
                *slot = CtfeSlot::Init(CtfeValue::concrete(self.db, value));
            }
        }
        let frame_idx = self.frames.len();
        self.frames.push(CtfeFrame {
            body,
            locals: frame_locals,
            current: 0,
        });
        let result = match self.eval_expr(frame_idx, result_ty, expr, origin)? {
            CtfeValue::Value(value) => Ok(value.materialize(self.db)),
            CtfeValue::Ref(_) => Err(CtfeError::InvalidBorrow { origin }),
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
    ) -> Result<SemConstId<'db>, CtfeError<'db>> {
        let key = cref.instance(self.db);
        if let Some(result) = self.const_results.get(&key) {
            return result.clone();
        }
        let origin = cref.origin(self.db);
        if self.const_stack.contains(&key) {
            return Err(CtfeError::RecursiveConst { origin });
        }

        self.const_stack.push(key);
        let result = self
            .eval_instance(SemanticInstance::new(self.db, key), Vec::new(), origin)
            .and_then(|value| match value {
                CtfeValue::Value(value) => Ok(value.materialize(self.db)),
                CtfeValue::Ref(_) => Err(CtfeError::InvalidBorrow { origin }),
            });
        self.const_stack.pop();
        self.const_results.insert(key, result.clone());
        result
    }

    fn eval_instance(
        &mut self,
        instance: SemanticInstance<'db>,
        args: Vec<CtfeValue<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeValue<'db>, CtfeError<'db>> {
        self.ensure_const_evaluable(instance, origin)?;
        // Const-item frames are exempt from the limit: the const stack's
        // cycle check already bounds them (each const is evaluated at most
        // once per path), and a long but finite chain of const definitions
        // is not call recursion.
        if self.frames.len().saturating_sub(self.const_stack.len()) >= self.config.recursion_limit {
            return Err(CtfeError::RecursionLimitExceeded { origin });
        }

        let body = self.body_for_instance(instance);
        let mut locals = vec![CtfeSlot::Uninit; body.locals.len()];
        for (idx, arg) in args.into_iter().enumerate() {
            let Some(slot) = locals.get_mut(idx) else {
                return Err(CtfeError::InvalidOperation {
                    origin,
                    message: "CTFE call arity mismatch".into(),
                });
            };
            *slot = CtfeSlot::Init(arg);
        }
        let frame_idx = self.frames.len();
        self.frames.push(CtfeFrame {
            body,
            locals,
            current: 0,
        });
        let result = self.run_frame(frame_idx);
        self.frames.pop();
        result
    }

    fn ensure_const_evaluable(
        &self,
        instance: SemanticInstance<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), CtfeError<'db>> {
        match instance.key(self.db).owner(self.db) {
            BodyOwner::Func(func) if !func.is_const(self.db) => {
                Err(CtfeError::NonConstCall { origin })
            }
            BodyOwner::Func(func) => {
                let (diags, typed_body) = check_func_body(self.db, func);
                if !diags.is_empty() && typed_body.has_smir_lowering_blocker(self.db) {
                    Err(CtfeError::InvalidBody { origin })
                } else {
                    Ok(())
                }
            }
            BodyOwner::Const(const_) => {
                let (diags, typed_body) = check_const_body(self.db, const_);
                if !diags.is_empty() && typed_body.has_smir_lowering_blocker(self.db) {
                    Err(CtfeError::InvalidBody { origin })
                } else {
                    Ok(())
                }
            }
            BodyOwner::AnonConstBody { body, expected } => {
                let (diags, typed_body) = check_anon_const_body(self.db, body, expected);
                if !diags.is_empty() && typed_body.has_smir_lowering_blocker(self.db) {
                    Err(CtfeError::InvalidBody { origin })
                } else {
                    Ok(())
                }
            }
            BodyOwner::ContractInit { .. } | BodyOwner::ContractRecvArm { .. } => {
                Err(CtfeError::NotConstEvaluable { origin })
            }
        }
    }

    fn run_frame(&mut self, frame_idx: usize) -> Result<CtfeValue<'db>, CtfeError<'db>> {
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
                    let cond = self.expect_bool(frame_idx, cond, term_origin)?;
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
                    });
                }
                STerminatorKind::Return(Some(value)) => {
                    return self.read_operand(frame_idx, value, term_origin);
                }
                STerminatorKind::Return(None) => {
                    return Ok(CtfeValue::Value(CtfeConstValue::unit()));
                }
            }
        }
    }

    fn exec_stmt(&mut self, frame_idx: usize, stmt: SStmt<'db>) -> Result<(), CtfeError<'db>> {
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
                    return Err(CtfeError::InvalidBorrow { origin });
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
    ) -> Result<CtfeValue<'db>, CtfeError<'db>> {
        self.bump(origin)?;
        match expr {
            SExpr::Forward(value) | SExpr::UseValue(value) => {
                self.read_operand(frame_idx, value, origin)
            }
            SExpr::CodeRegionRef { .. } => Err(CtfeError::NotConstEvaluable { origin }),
            SExpr::Const(SConst::Value(value)) => Ok(CtfeValue::concrete(self.db, value)),
            SExpr::Const(SConst::Ref(cref)) => self
                .eval_const_ref_value(cref)
                .map(|value| CtfeValue::concrete(self.db, value))
                .map_err(|err| {
                    // Re-originating recursion errors at the reference site
                    // keeps the origin an expression of the body being
                    // evaluated, so the eventual diagnostic anchors in the
                    // right body. (It also keeps the error shape stable if
                    // an outer salsa fixpoint iteration replays this site.)
                    if err.root_is_recursive_const() {
                        CtfeError::RecursiveConst {
                            origin: cref.origin(self.db),
                        }
                    } else {
                        CtfeError::CalleeError {
                            origin: cref.origin(self.db),
                            callee: SemanticInstance::new(self.db, cref.instance(self.db)),
                            source: Box::new(err),
                        }
                    }
                }),
            SExpr::Unary { op, value } => {
                let value = self.load_value(frame_idx, value, origin)?;
                self.eval_unary(frame_idx, result_ty, op, value, origin)
            }
            SExpr::Binary { op, lhs, rhs } => {
                let lhs = self.load_value(frame_idx, lhs, origin)?;
                let rhs = self.load_value(frame_idx, rhs, origin)?;
                self.eval_binary(frame_idx, result_ty, op, lhs, rhs, origin)
            }
            SExpr::Cast { value, .. } => {
                let value = self.load_value(frame_idx, value, origin)?;
                self.eval_cast(result_ty, value, origin)
            }
            SExpr::AggregateMake { fields, .. } => {
                let fields = self.eval_value_args(frame_idx, &fields, origin)?;
                Ok(self.make_aggregate_value(result_ty, fields))
            }
            SExpr::ArrayRepeat { ty, value } => {
                let Some(len) = ty.array_len(self.db) else {
                    return Err(CtfeError::NotConstEvaluable { origin });
                };
                let CtfeValue::Value(value) = self.read_operand(frame_idx, value, origin)? else {
                    return Err(CtfeError::InvalidBorrow { origin });
                };
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
                self.load_ref_value(&r#ref, origin).map(CtfeValue::Value)
            }
            SExpr::Field { base, field } => {
                let value = self.load_value(frame_idx, base, origin)?;
                self.project_field(value, field, origin)
                    .map(CtfeValue::Value)
            }
            SExpr::Index { base, index } => {
                let value = self.load_value(frame_idx, base, origin)?;
                let index_value = self.load_value(frame_idx, index, origin)?;
                let index = self.index_from_value(frame_idx, index_value, origin)?;
                self.project_index(value, index, origin)
                    .map(CtfeValue::Value)
            }
            SExpr::Borrow {
                place: _,
                provider: Some(_),
                ..
            } => Err(CtfeError::InvalidProviderUse { origin }),
            SExpr::Borrow {
                place,
                provider: None,
                ..
            } => {
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
                self.enum_extract(value, variant, field, origin)
                    .map(CtfeValue::Value)
            }
            SExpr::Call {
                callee,
                args,
                effect_args,
                ..
            } => {
                if !effect_args.is_empty() {
                    return Err(CtfeError::NotConstEvaluable { origin });
                }
                let args = self
                    .eval_args(frame_idx, &args, origin)?
                    .into_iter()
                    .collect::<Vec<_>>();
                let instance = self.instance_for_key(callee.key);
                if let Some(value) = self.try_eval_core_primitive_wrapper_call(
                    frame_idx, instance, result_ty, &args, origin,
                )? {
                    return Ok(CtfeValue::Value(value));
                }
                if let BodyOwner::Func(func) = instance.key(self.db).owner(self.db)
                    && func.is_extern(self.db)
                {
                    if !func.is_const(self.db) {
                        return Err(CtfeError::NonConstCall { origin });
                    }
                    let deferred_origin = self.first_deferred_origin(&args).unwrap_or(origin);
                    let value_args = self.value_args(args, origin)?;
                    return match self.eval_extern_const_fn(
                        frame_idx,
                        instance,
                        func,
                        result_ty,
                        &value_args,
                        origin,
                    ) {
                        Ok(value) => Ok(CtfeValue::Value(value)),
                        Err(CtfeError::NotConstEvaluable { .. }) => {
                            let materialized_args = value_args
                                .iter()
                                .map(|arg| arg.materialize(self.db))
                                .collect::<Vec<_>>();
                            Ok(CtfeValue::deferred(
                                self.db,
                                self.abstract_const_call(
                                    ConstExpr::ExternConstFnCall {
                                        func,
                                        generic_args: instance
                                            .key(self.db)
                                            .subst(self.db)
                                            .generic_args(self.db)
                                            .clone(),
                                        args: materialized_args
                                            .iter()
                                            .copied()
                                            .map(|arg| {
                                                TyId::const_ty(
                                                    self.db,
                                                    const_ty_from_sem_const(self.db, arg),
                                                )
                                            })
                                            .collect(),
                                    },
                                    result_ty,
                                ),
                                deferred_origin,
                            ))
                        }
                        Err(err) => Err(err),
                    };
                }
                if let BodyOwner::Func(func) = instance.key(self.db).owner(self.db)
                    && !func.is_const(self.db)
                {
                    return Err(CtfeError::NonConstCall { origin });
                }
                match self.eval_instance(instance, args.clone(), origin) {
                    Ok(value) => Ok(value),
                    Err(CtfeError::NotConstEvaluable { .. }) if matches!(instance.key(self.db).owner(self.db), BodyOwner::Func(func) if func.is_const(self.db)) =>
                    {
                        let BodyOwner::Func(func) = instance.key(self.db).owner(self.db) else {
                            unreachable!();
                        };
                        let deferred_origin = self.first_deferred_origin(&args).unwrap_or(origin);
                        let value_args = self.materialize_args(args, origin)?;
                        Ok(CtfeValue::deferred(
                            self.db,
                            self.abstract_const_call(
                                ConstExpr::UserConstFnCall {
                                    func,
                                    generic_args: instance
                                        .key(self.db)
                                        .subst(self.db)
                                        .generic_args(self.db)
                                        .clone(),
                                    args: value_args
                                        .iter()
                                        .copied()
                                        .map(|arg| {
                                            TyId::const_ty(
                                                self.db,
                                                const_ty_from_sem_const(self.db, arg),
                                            )
                                        })
                                        .collect(),
                                },
                                result_ty,
                            ),
                            deferred_origin,
                        ))
                    }
                    Err(err) => Err(match err {
                        CtfeError::NotConstEvaluable { .. } => {
                            CtfeError::NotConstEvaluable { origin }
                        }
                        _ => CtfeError::CalleeError {
                            origin,
                            callee: instance,
                            source: Box::new(err),
                        },
                    }),
                }
            }
            SExpr::CodeRegionOffset { .. } | SExpr::CodeRegionLen { .. } => {
                Err(CtfeError::NotConstEvaluable { origin })
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

    fn materialize_args(
        &self,
        args: Vec<CtfeValue<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<Vec<SemConstId<'db>>, CtfeError<'db>> {
        args.into_iter()
            .map(|arg| match arg {
                CtfeValue::Value(value) => Ok(value.materialize(self.db)),
                CtfeValue::Ref(r#ref) => self.load_ref(&r#ref, origin),
            })
            .collect()
    }

    fn first_deferred_origin(&self, args: &[CtfeValue<'db>]) -> Option<SemOrigin<'db>> {
        args.iter().find_map(|arg| match arg {
            CtfeValue::Value(value) => value.deferred_origin,
            CtfeValue::Ref(_) => None,
        })
    }

    fn eval_extern_const_fn(
        &self,
        frame_idx: usize,
        instance: SemanticInstance<'db>,
        func: crate::hir_def::Func<'db>,
        result_ty: TyId<'db>,
        args: &[CtfeConstValue<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        match runtime_builtin_func_kind(self.db, func) {
            Some(RuntimeBuiltinFuncKind::AddMod) => {
                return self.eval_evm_modular_arithmetic(
                    result_ty,
                    args,
                    EvmModularArithmetic::Add,
                    origin,
                );
            }
            Some(RuntimeBuiltinFuncKind::MulMod) => {
                return self.eval_evm_modular_arithmetic(
                    result_ty,
                    args,
                    EvmModularArithmetic::Mul,
                    origin,
                );
            }
            _ => {}
        }

        let Some(name) = func.name(self.db).to_opt() else {
            return Err(CtfeError::NotConstEvaluable { origin });
        };

        match name.data(self.db).as_str() {
            "size_of" => self.eval_intrinsic_size_of(instance, result_ty, args, origin),
            "__as_bytes" => self.eval_intrinsic_as_bytes(result_ty, args, origin),
            "__keccak256" => self.eval_intrinsic_keccak(result_ty, args, origin),
            "__bitcast" => self.eval_intrinsic_bitcast(result_ty, args, origin),
            name => self.eval_numeric_extern_intrinsic(frame_idx, name, result_ty, args, origin),
        }
    }

    fn eval_numeric_extern_intrinsic(
        &self,
        frame_idx: usize,
        name: &str,
        result_ty: TyId<'db>,
        args: &[CtfeConstValue<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        let Some(kind) = numeric_extern_intrinsic(name) else {
            return Err(CtfeError::NotConstEvaluable { origin });
        };

        match kind {
            NumericExternIntrinsic::CheckedBinary(op) => {
                let (lhs, rhs) = expect_binary_args(args, origin)?;
                if self.is_type_level(lhs) || self.is_type_level(rhs) {
                    let CtfeValue::Value(value) = self.eval_binary(
                        frame_idx,
                        result_ty,
                        BinOp::Arith(op),
                        lhs.clone(),
                        rhs.clone(),
                        origin,
                    )?
                    else {
                        return Err(CtfeError::InvalidBorrow { origin });
                    };
                    return Ok(value);
                }
                self.eval_checked_numeric_binary(
                    frame_idx,
                    result_ty,
                    op,
                    lhs.clone(),
                    rhs.clone(),
                    origin,
                )
            }
            NumericExternIntrinsic::WrappingBinary(op) => {
                let (lhs, rhs) = expect_binary_args(args, origin)?;
                if self.is_type_level(lhs) || self.is_type_level(rhs) {
                    let CtfeValue::Value(value) = self.eval_binary(
                        frame_idx,
                        result_ty,
                        BinOp::Arith(op),
                        lhs.clone(),
                        rhs.clone(),
                        origin,
                    )?
                    else {
                        return Err(CtfeError::InvalidBorrow { origin });
                    };
                    return Ok(value);
                }
                self.eval_wrapping_numeric_binary(
                    frame_idx,
                    result_ty,
                    op,
                    lhs.clone(),
                    rhs.clone(),
                    origin,
                )
            }
            NumericExternIntrinsic::SaturatingBinary(op) => {
                let (lhs, rhs) = expect_binary_args(args, origin)?;
                if self.is_type_level(lhs) || self.is_type_level(rhs) {
                    return Err(CtfeError::NotConstEvaluable { origin });
                }
                self.eval_saturating_numeric_binary(
                    frame_idx,
                    result_ty,
                    op,
                    lhs.clone(),
                    rhs.clone(),
                    origin,
                )
            }
            NumericExternIntrinsic::Comparison(op) => {
                let (lhs, rhs) = expect_binary_args(args, origin)?;
                if self.is_type_level(lhs) || self.is_type_level(rhs) {
                    return Err(CtfeError::NotConstEvaluable { origin });
                }
                let CtfeValue::Value(value) =
                    self.eval_compare(frame_idx, op, lhs.clone(), rhs.clone(), origin)?
                else {
                    return Err(CtfeError::InvalidBorrow { origin });
                };
                Ok(value)
            }
            NumericExternIntrinsic::BoolBinary(op) => {
                let (lhs, rhs) = expect_binary_args(args, origin)?;
                if self.is_type_level(lhs) || self.is_type_level(rhs) {
                    return Err(CtfeError::NotConstEvaluable { origin });
                }
                let lhs = self.expect_bool(frame_idx, lhs.clone(), origin)?;
                let rhs = self.expect_bool(frame_idx, rhs.clone(), origin)?;
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
                if self.is_type_level(value) {
                    let CtfeValue::Value(value) =
                        self.eval_unary(frame_idx, result_ty, UnOp::Minus, value.clone(), origin)?
                    else {
                        return Err(CtfeError::InvalidBorrow { origin });
                    };
                    return Ok(value);
                }
                let value = -self.expect_int(frame_idx, value.clone(), origin)?;
                if !self.int_in_range(result_ty, &value) {
                    return Err(CtfeError::ArithmeticOverflow { origin });
                }
                Ok(CtfeConstValue::int(self.db, result_ty, value))
            }
            NumericExternIntrinsic::WrappingNeg => {
                let [value] = args else {
                    return Err(CtfeError::NotConstEvaluable { origin });
                };
                if self.is_type_level(value) {
                    let CtfeValue::Value(value) =
                        self.eval_unary(frame_idx, result_ty, UnOp::Minus, value.clone(), origin)?
                    else {
                        return Err(CtfeError::InvalidBorrow { origin });
                    };
                    return Ok(value);
                }
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
                    -self.expect_int(frame_idx, value.clone(), origin)?,
                ))
            }
            NumericExternIntrinsic::BitNot => {
                let [value] = args else {
                    return Err(CtfeError::NotConstEvaluable { origin });
                };
                if self.is_type_level(value) {
                    return Err(CtfeError::NotConstEvaluable { origin });
                }
                if let Some(word) = self.expect_matching_int_word(value, result_ty, origin)? {
                    return Ok(CtfeConstValue::int_word(self.db, result_ty, word.not()));
                }
                let value = self.expect_int(frame_idx, value.clone(), origin)?;
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
                if self.is_type_level(value) {
                    return Err(CtfeError::NotConstEvaluable { origin });
                }
                Ok(CtfeConstValue::bool(!self.expect_bool(
                    frame_idx,
                    value.clone(),
                    origin,
                )?))
            }
        }
    }

    fn eval_checked_numeric_binary(
        &self,
        frame_idx: usize,
        result_ty: TyId<'db>,
        op: ArithBinOp,
        lhs: CtfeConstValue<'db>,
        rhs: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        let lhs = self.expect_int(frame_idx, lhs, origin)?;
        let rhs = self.expect_int(frame_idx, rhs, origin)?;
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
        frame_idx: usize,
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
        let lhs = self.expect_int(frame_idx, lhs, origin)?;
        let rhs = self.expect_int(frame_idx, rhs, origin)?;
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
        frame_idx: usize,
        result_ty: TyId<'db>,
        op: SaturatingArithmetic,
        lhs: CtfeConstValue<'db>,
        rhs: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeConstValue<'db>, CtfeError<'db>> {
        let lhs = self.expect_int(frame_idx, lhs, origin)?;
        let rhs = self.expect_int(frame_idx, rhs, origin)?;
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
                } = value.value(self.db)
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
                } = value.value(self.db)
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
        let size =
            runtime_size_bytes(self.db, ty).ok_or(CtfeError::NotConstEvaluable { origin })?;
        Ok(CtfeConstValue::int(self.db, result_ty, BigInt::from(size)))
    }

    fn abstract_const_call(&self, expr: ConstExpr<'db>, result_ty: TyId<'db>) -> SemConstId<'db> {
        let const_ty = ConstTyId::new(
            self.db,
            ConstTyData::Abstract(ConstExprId::new(self.db, expr), result_ty),
        );
        SemConstId::new(
            self.db,
            SemConstValue::TypeLevel {
                ty: result_ty,
                const_ty: TyId::const_ty(self.db, const_ty),
            },
        )
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
                    CtfePathElem::Index(self.index_from_value(frame_idx, index, origin)?)
                }
                Projection::Index(IndexSource::Constant(index)) => CtfePathElem::Index(*index),
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

    fn load_ref(
        &self,
        r#ref: &CtfeRef,
        origin: SemOrigin<'db>,
    ) -> Result<SemConstId<'db>, CtfeError<'db>> {
        Ok(self.load_ref_value(r#ref, origin)?.materialize(self.db))
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
        let deferred_origin = value.deferred_origin.or(root.deferred_origin);
        let result = self.store_const_value_in_place(&mut root, &place.path, value, origin);
        if result.is_ok() {
            root.set_deferred_origin(self.db, root.deferred_origin.or(deferred_origin));
        }
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
        self.project_value(value, &[CtfePathElem::Index(index)], origin)
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
            return Err(CtfeError::VariantMismatch {
                origin: value.error_origin(origin),
            });
        };
        if *actual != variant {
            return Err(CtfeError::VariantMismatch {
                origin: value.error_origin(origin),
            });
        }
        fields
            .get(field.0 as usize)
            .cloned()
            .map(|field| self.value_with_origin(field, value.deferred_origin))
            .ok_or(CtfeError::OutOfBounds {
                origin: value.error_origin(origin),
            })
    }

    fn load_enum_variant(
        &self,
        value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<VariantIndex, CtfeError<'db>> {
        let value = self.expand_interned(value);
        let CtfeConstKind::Enum { variant, .. } = value.kind else {
            return Err(CtfeError::VariantMismatch {
                origin: value.error_origin(origin),
            });
        };
        Ok(variant)
    }

    fn expect_bool(
        &self,
        frame_idx: usize,
        value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<bool, CtfeError<'db>> {
        match &value.kind {
            CtfeConstKind::Bool(value) => Ok(*value),
            CtfeConstKind::Interned(interned) => match interned.value(self.db) {
                SemConstValue::Scalar {
                    value: SemConstScalar::Bool(value),
                    ..
                } => Ok(value),
                SemConstValue::TypeLevel { ty, const_ty } if ty == TyId::bool(self.db) => {
                    let subst = self.frames[frame_idx]
                        .body
                        .owner
                        .key(self.db)
                        .subst(self.db);
                    let Some(const_ty) = demand_concrete_const_ty(
                        self.db,
                        const_ty,
                        ty,
                        subst.generic_args(self.db),
                    ) else {
                        return Err(CtfeError::InvalidOperation {
                            origin: value.error_origin(origin),
                            message: format!("expected bool, got {:?}", interned.value(self.db)),
                        });
                    };
                    let ConstTyData::Evaluated(EvaluatedConstTy::LitBool(value), _) =
                        const_ty.data(self.db)
                    else {
                        return Err(CtfeError::InvalidOperation {
                            origin: value.error_origin(origin),
                            message: "expected bool".into(),
                        });
                    };
                    Ok(*value)
                }
                _ => Err(CtfeError::InvalidOperation {
                    origin: value.error_origin(origin),
                    message: "expected bool".into(),
                }),
            },
            _ => Err(CtfeError::InvalidOperation {
                origin: value.error_origin(origin),
                message: "expected bool".into(),
            }),
        }
    }

    fn is_bool_like(&self, value: &CtfeConstValue<'db>) -> bool {
        match &value.kind {
            CtfeConstKind::Bool(_) => true,
            CtfeConstKind::Interned(value) => match value.value(self.db) {
                SemConstValue::Scalar {
                    value: SemConstScalar::Bool(_),
                    ..
                } => true,
                SemConstValue::TypeLevel { ty, .. } => ty == TyId::bool(self.db),
                _ => false,
            },
            _ => false,
        }
    }

    fn is_int_like(&self, value: &CtfeConstValue<'db>) -> bool {
        match &value.kind {
            CtfeConstKind::Int { .. } => true,
            CtfeConstKind::Interned(value) => match value.value(self.db) {
                SemConstValue::Scalar {
                    value: SemConstScalar::Int { .. },
                    ..
                } => true,
                SemConstValue::TypeLevel { ty, .. } => int_ty_shape(self.db, ty).is_some(),
                _ => false,
            },
            _ => false,
        }
    }

    fn expect_int(
        &self,
        frame_idx: usize,
        value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<BigInt, CtfeError<'db>> {
        match &value.kind {
            CtfeConstKind::Int { value, .. } => Ok(value.to_bigint()),
            CtfeConstKind::Interned(interned) => match interned.value(self.db) {
                SemConstValue::Scalar {
                    value: SemConstScalar::Int { value },
                    ..
                } => Ok(value.clone()),
                SemConstValue::TypeLevel { ty, const_ty } => {
                    let subst = self.frames[frame_idx]
                        .body
                        .owner
                        .key(self.db)
                        .subst(self.db);
                    let Some(const_ty) = demand_concrete_const_ty(
                        self.db,
                        const_ty,
                        ty,
                        subst.generic_args(self.db),
                    ) else {
                        return Err(CtfeError::InvalidOperation {
                            origin: value.error_origin(origin),
                            message: format!("expected int, got {:?}", interned.value(self.db)),
                        });
                    };
                    let ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) =
                        const_ty.data(self.db)
                    else {
                        return Err(CtfeError::InvalidOperation {
                            origin: value.error_origin(origin),
                            message: "expected int".into(),
                        });
                    };
                    Ok(BigInt::from(int_id.data(self.db).clone()))
                }
                _ => Err(CtfeError::InvalidOperation {
                    origin: value.error_origin(origin),
                    message: "expected int".into(),
                }),
            },
            _ => Err(CtfeError::InvalidOperation {
                origin: value.error_origin(origin),
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
            CtfeConstKind::Interned(interned) => match interned.value(self.db) {
                SemConstValue::Scalar {
                    ty,
                    value: SemConstScalar::Int { value },
                } if int_ty_shape(self.db, ty) == Some((bits, signed)) => Ok(Some(
                    u256_from_bigint(&normalize_int_to_shape(value.clone(), bits, false)),
                )),
                SemConstValue::Scalar {
                    value: SemConstScalar::Int { .. },
                    ..
                }
                | SemConstValue::TypeLevel { .. } => Ok(None),
                _ => Err(CtfeError::InvalidOperation {
                    origin: value.error_origin(origin),
                    message: "expected int".into(),
                }),
            },
            _ => Err(CtfeError::InvalidOperation {
                origin: value.error_origin(origin),
                message: "expected int".into(),
            }),
        }
    }

    fn index_from_value(
        &self,
        frame_idx: usize,
        value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<usize, CtfeError<'db>> {
        let error_origin = value.error_origin(origin);
        let index = self.expect_int(frame_idx, value, origin)?;
        index.to_usize().ok_or(CtfeError::OutOfBounds {
            origin: error_origin,
        })
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
                if self.is_type_level(&value)
                    && let Some(value) =
                        self.eval_type_level_unary(result_ty, op, value.clone(), origin)
                {
                    return Ok(CtfeValue::Value(value));
                }
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
                let value = self.expect_int(frame_idx, value, origin)?;
                let value = match arithmetic_mode {
                    ArithmeticMode::Checked => {
                        let value = -value;
                        if !self.int_in_range(result_ty, &value) {
                            return Err(CtfeError::ArithmeticOverflow { origin });
                        }
                        value
                    }
                    ArithmeticMode::Unchecked => -value,
                };
                Ok(CtfeValue::Value(CtfeConstValue::int(
                    self.db, result_ty, value,
                )))
            }
            UnOp::Not => Ok(CtfeValue::Value(CtfeConstValue::bool(
                !self.expect_bool(frame_idx, value, origin)?,
            ))),
            UnOp::BitNot => {
                if let Some(word) = self.expect_matching_int_word(&value, result_ty, origin)? {
                    return Ok(CtfeValue::Value(CtfeConstValue::int_word(
                        self.db,
                        result_ty,
                        word.not(),
                    )));
                }
                let int = self.expect_int(frame_idx, value, origin)?;
                Ok(CtfeValue::Value(CtfeConstValue::int(
                    self.db,
                    result_ty,
                    -int - BigInt::one(),
                )))
            }
            UnOp::Mut | UnOp::Ref => Err(CtfeError::InvalidOperation {
                origin,
                message: "unexpected borrow operator in CTFE unary evaluation".into(),
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
            BinOp::Comp(comp) => self.eval_compare(frame_idx, comp, lhs, rhs, origin),
            BinOp::Logical(logical) => {
                let lhs = self.expect_bool(frame_idx, lhs, origin)?;
                let rhs = self.expect_bool(frame_idx, rhs, origin)?;
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
                if (self.is_type_level(&lhs) || self.is_type_level(&rhs))
                    && let Some(value) = self.eval_type_level_binary(
                        result_ty,
                        arith,
                        lhs.clone(),
                        rhs.clone(),
                        origin,
                    )
                {
                    return Ok(CtfeValue::Value(value));
                }
                if self.is_bool_like(&lhs) && self.is_bool_like(&rhs) {
                    return match arith {
                        ArithBinOp::BitAnd => Ok(CtfeValue::Value(CtfeConstValue::bool(
                            self.expect_bool(frame_idx, lhs, origin)?
                                & self.expect_bool(frame_idx, rhs, origin)?,
                        ))),
                        ArithBinOp::BitOr => Ok(CtfeValue::Value(CtfeConstValue::bool(
                            self.expect_bool(frame_idx, lhs, origin)?
                                | self.expect_bool(frame_idx, rhs, origin)?,
                        ))),
                        ArithBinOp::BitXor => Ok(CtfeValue::Value(CtfeConstValue::bool(
                            self.expect_bool(frame_idx, lhs, origin)?
                                ^ self.expect_bool(frame_idx, rhs, origin)?,
                        ))),
                        _ => Err(CtfeError::InvalidOperation {
                            origin,
                            message: "expected int".into(),
                        }),
                    };
                }
                let lhs = self.expect_int(frame_idx, lhs, origin)?;
                let rhs = self.expect_int(frame_idx, rhs, origin)?;
                let arithmetic_mode = self.frames[frame_idx]
                    .body
                    .template_owner
                    .arithmetic_mode(self.db);
                let value = match arith {
                    ArithBinOp::Add => {
                        let value = lhs + rhs;
                        if arithmetic_mode == ArithmeticMode::Checked
                            && !self.int_in_range(result_ty, &value)
                        {
                            return Err(CtfeError::ArithmeticOverflow { origin });
                        }
                        value
                    }
                    ArithBinOp::Sub => {
                        let value = lhs - rhs;
                        if arithmetic_mode == ArithmeticMode::Checked
                            && !self.int_in_range(result_ty, &value)
                        {
                            return Err(CtfeError::ArithmeticOverflow { origin });
                        }
                        value
                    }
                    ArithBinOp::Mul => {
                        let value = lhs * rhs;
                        if arithmetic_mode == ArithmeticMode::Checked
                            && !self.int_in_range(result_ty, &value)
                        {
                            return Err(CtfeError::ArithmeticOverflow { origin });
                        }
                        value
                    }
                    ArithBinOp::Div => {
                        if rhs.is_zero() {
                            return Err(CtfeError::DivisionByZero { origin });
                        }
                        if arithmetic_mode == ArithmeticMode::Checked
                            && let Some((bits, true)) = int_ty_shape(self.db, result_ty)
                            && lhs == -(BigInt::one() << (usize::from(bits) - 1))
                            && rhs == -BigInt::one()
                        {
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
                    ArithBinOp::Pow => {
                        if rhs.sign() == num_bigint::Sign::Minus {
                            return Err(CtfeError::NegativeExponent { origin });
                        }
                        let Some(exp) = rhs.to_biguint() else {
                            return Err(CtfeError::NegativeExponent { origin });
                        };
                        if arithmetic_mode == ArithmeticMode::Checked {
                            let mut acc = BigInt::one();
                            let mut base = lhs;
                            let mut exp = exp;
                            while !exp.is_zero() {
                                if (&exp & num_bigint::BigUint::one()) == num_bigint::BigUint::one()
                                {
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
                            acc
                        } else {
                            lhs.pow(exp.to_u32().ok_or(CtfeError::InvalidOperation {
                                origin,
                                message: "invalid power exponent".into(),
                            })?)
                        }
                    }
                    ArithBinOp::LShift => {
                        lhs << rhs.to_usize().ok_or(CtfeError::InvalidOperation {
                            origin,
                            message: "invalid left shift amount".into(),
                        })?
                    }
                    ArithBinOp::RShift => {
                        lhs >> rhs.to_usize().ok_or(CtfeError::InvalidOperation {
                            origin,
                            message: "invalid right shift amount".into(),
                        })?
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
        frame_idx: usize,
        op: CompBinOp,
        lhs: CtfeConstValue<'db>,
        rhs: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeValue<'db>, CtfeError<'db>> {
        let result = if self.is_bool_like(&lhs) && self.is_bool_like(&rhs) {
            let lhs = self.expect_bool(frame_idx, lhs, origin)?;
            let rhs = self.expect_bool(frame_idx, rhs, origin)?;
            match op {
                CompBinOp::Eq => lhs == rhs,
                CompBinOp::NotEq => lhs != rhs,
                CompBinOp::Lt => !lhs && rhs,
                CompBinOp::LtEq => !lhs || rhs,
                CompBinOp::Gt => lhs && !rhs,
                CompBinOp::GtEq => lhs || !rhs,
            }
        } else if self.is_int_like(&lhs) && self.is_int_like(&rhs) {
            let lhs = self.expect_int(frame_idx, lhs, origin)?;
            let rhs = self.expect_int(frame_idx, rhs, origin)?;
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
                CompBinOp::Lt => {
                    self.expect_int(frame_idx, lhs, origin)?
                        < self.expect_int(frame_idx, rhs, origin)?
                }
                CompBinOp::LtEq => {
                    self.expect_int(frame_idx, lhs, origin)?
                        <= self.expect_int(frame_idx, rhs, origin)?
                }
                CompBinOp::Gt => {
                    self.expect_int(frame_idx, lhs, origin)?
                        > self.expect_int(frame_idx, rhs, origin)?
                }
                CompBinOp::GtEq => {
                    self.expect_int(frame_idx, lhs, origin)?
                        >= self.expect_int(frame_idx, rhs, origin)?
                }
            }
        };
        Ok(CtfeValue::Value(CtfeConstValue::bool(result)))
    }

    fn eval_type_level_unary(
        &self,
        result_ty: TyId<'db>,
        op: UnOp,
        value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Option<CtfeConstValue<'db>> {
        let const_value = value.materialize(self.db);
        let const_ty = TyId::const_ty(self.db, const_ty_from_sem_const(self.db, const_value));
        let expr = ConstExprId::new(self.db, ConstExpr::UnOp { op, expr: const_ty });
        let const_ty = ConstTyId::new(self.db, ConstTyData::Abstract(expr, result_ty))
            .evaluate(self.db, Some(result_ty));
        sem_const_from_ty(self.db, TyId::const_ty(self.db, const_ty)).map(|const_value| {
            self.value_with_origin(
                CtfeConstValue::concrete(self.db, const_value),
                value.deferred_origin.or(Some(origin)),
            )
        })
    }

    fn eval_type_level_binary(
        &self,
        result_ty: TyId<'db>,
        op: ArithBinOp,
        lhs: CtfeConstValue<'db>,
        rhs: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Option<CtfeConstValue<'db>> {
        let lhs_value = lhs.materialize(self.db);
        let rhs_value = rhs.materialize(self.db);
        let lhs_ty = TyId::const_ty(self.db, const_ty_from_sem_const(self.db, lhs_value));
        let rhs_ty = TyId::const_ty(self.db, const_ty_from_sem_const(self.db, rhs_value));
        let expr = ConstExprId::new(
            self.db,
            ConstExpr::ArithBinOp {
                op,
                lhs: lhs_ty,
                rhs: rhs_ty,
            },
        );
        let const_ty = ConstTyId::new(self.db, ConstTyData::Abstract(expr, result_ty))
            .evaluate(self.db, Some(result_ty));
        sem_const_from_ty(self.db, TyId::const_ty(self.db, const_ty)).map(|value| {
            self.value_with_origin(
                CtfeConstValue::concrete(self.db, value),
                lhs.deferred_origin.or(rhs.deferred_origin).or(Some(origin)),
            )
        })
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
        let Some((bits, signed)) = int_ty_shape(self.db, result_ty) else {
            return true;
        };
        if signed {
            let half = BigInt::one() << (usize::from(bits) - 1);
            let min = -half.clone();
            let max = half - BigInt::one();
            value >= &min && value <= &max
        } else {
            value >= &BigInt::zero() && value < &(BigInt::one() << usize::from(bits))
        }
    }

    fn eval_cast(
        &self,
        result_ty: TyId<'db>,
        value: CtfeConstValue<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<CtfeValue<'db>, CtfeError<'db>> {
        fn fixed_string_capacity_bytes<'db>(
            db: &'db dyn HirAnalysisDb,
            ty: TyId<'db>,
        ) -> Option<usize> {
            if !ty.is_string(db) {
                return None;
            }
            let (_, args) = ty.decompose_ty_app(db);
            let len_ty = args.first().copied()?;
            let TyData::ConstTy(const_ty) = len_ty.data(db) else {
                return None;
            };
            match const_ty.data(db) {
                ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => {
                    int_id.data(db).to_usize()
                }
                _ => None,
            }
        }

        let value = self.expand_interned(value);
        match &value.kind {
            CtfeConstKind::Bool(value) if int_ty_shape(self.db, result_ty).is_some() => {
                Ok(CtfeValue::Value(CtfeConstValue::int(
                    self.db,
                    result_ty,
                    if *value {
                        BigInt::one()
                    } else {
                        BigInt::zero()
                    },
                )))
            }
            CtfeConstKind::Int { value, .. } if result_ty == TyId::bool(self.db) => Ok(
                CtfeValue::Value(CtfeConstValue::bool(!value.to_bigint().is_zero())),
            ),
            CtfeConstKind::Int { value, .. } if int_ty_shape(self.db, result_ty).is_some() => Ok(
                CtfeValue::Value(CtfeConstValue::int(self.db, result_ty, value.to_bigint())),
            ),
            CtfeConstKind::Int { value, .. } if result_ty.is_string(self.db) => {
                fixed_string_capacity_bytes(self.db, result_ty)
                    .ok_or(CtfeError::NotConstEvaluable { origin })?;
                let word = value.to_u256();
                Ok(CtfeValue::Value(CtfeConstValue::bytes(
                    result_ty,
                    word.to_be_bytes::<32>().to_vec(),
                )))
            }
            CtfeConstKind::Bytes { bytes, .. }
                if matches!(int_ty_shape(self.db, result_ty), Some((_, false))) =>
            {
                let Some((bits, false)) = int_ty_shape(self.db, result_ty) else {
                    unreachable!("match guard should ensure unsigned int shape");
                };
                let width = usize::from(bits / 8);
                if bytes.len() > width && bytes[..bytes.len() - width].iter().any(|byte| *byte != 0)
                {
                    return Err(CtfeError::NotConstEvaluable { origin });
                }
                let suffix = if bytes.len() > width {
                    &bytes[bytes.len() - width..]
                } else {
                    bytes.as_ref()
                };
                let value = BigInt::from(BigUint::from_bytes_be(suffix));
                Ok(CtfeValue::Value(CtfeConstValue::int(
                    self.db, result_ty, value,
                )))
            }
            CtfeConstKind::Bytes { bytes, .. } => Ok(CtfeValue::Value(CtfeConstValue::bytes(
                result_ty,
                bytes.to_vec(),
            ))),
            _ => Err(CtfeError::InvalidOperation {
                origin: value.error_origin(origin),
                message: "unsupported cast in CTFE".into(),
            }),
        }
    }

    fn make_aggregate_value(
        &self,
        result_ty: TyId<'db>,
        fields: Vec<CtfeConstValue<'db>>,
    ) -> CtfeValue<'db> {
        let deferred_origin = fields.iter().find_map(|field| field.deferred_origin);
        let value = if result_ty.is_tuple(self.db) {
            CtfeConstValue::tuple(result_ty, fields)
        } else if result_ty.is_array(self.db) {
            CtfeConstValue::array(result_ty, fields)
        } else {
            CtfeConstValue::struct_(result_ty, fields)
        };
        CtfeValue::Value(self.value_with_origin(value, deferred_origin))
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
                (CtfeConstKind::Bytes { bytes, .. }, CtfePathElem::Index(index)) => {
                    let byte = *bytes.get(*index).ok_or(CtfeError::OutOfBounds { origin })?;
                    CtfeConstValue::int(
                        self.db,
                        TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::U8))),
                        byte.into(),
                    )
                }
                (CtfeConstKind::Array { elems, .. }, CtfePathElem::Index(index)) => elems
                    .get(*index)
                    .cloned()
                    .ok_or(CtfeError::OutOfBounds { origin })?,
                (CtfeConstKind::Interned(interned), _)
                    if matches!(interned.value(self.db), SemConstValue::TypeLevel { .. }) =>
                {
                    return Err(CtfeError::InvalidOperation {
                        origin: value.error_origin(origin),
                        message: "invalid const projection".into(),
                    });
                }
                _ => {
                    return Err(CtfeError::InvalidOperation {
                        origin,
                        message: "invalid const projection".into(),
                    });
                }
            };
            value = self.value_with_origin(projected, value.deferred_origin);
        }
        Ok(value)
    }

    fn value_with_origin(
        &self,
        value: CtfeConstValue<'db>,
        deferred_origin: Option<SemOrigin<'db>>,
    ) -> CtfeConstValue<'db> {
        value.with_deferred_origin(self.db, deferred_origin)
    }

    fn is_type_level(&self, value: &CtfeConstValue<'db>) -> bool {
        matches!(
            &value.kind,
            CtfeConstKind::Interned(value)
                if matches!(value.value(self.db), SemConstValue::TypeLevel { .. })
        )
    }

    fn expand_interned(&self, value: CtfeConstValue<'db>) -> CtfeConstValue<'db> {
        let deferred_origin = value.deferred_origin;
        match value.kind {
            CtfeConstKind::Interned(interned)
                if !matches!(interned.value(self.db), SemConstValue::TypeLevel { .. }) =>
            {
                CtfeConstValue::expand_sem_const_shallow(self.db, interned)
                    .with_deferred_origin(self.db, deferred_origin)
            }
            kind => CtfeConstValue {
                kind,
                deferred_origin,
            },
        }
    }

    fn expand_interned_in_place(&self, value: &mut CtfeConstValue<'db>) {
        let interned = match &value.kind {
            CtfeConstKind::Interned(interned)
                if !matches!(interned.value(self.db), SemConstValue::TypeLevel { .. }) =>
            {
                *interned
            }
            _ => return,
        };
        *value = CtfeConstValue::expand_sem_const_shallow(self.db, interned)
            .with_deferred_origin(self.db, value.deferred_origin);
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
        let root_origin = root.error_origin(origin);
        let root_deferred_origin = root.deferred_origin;
        let deferred_origin = match &mut root.kind {
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
                slot.deferred_origin.or(root_deferred_origin)
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
                slot.deferred_origin.or(root_deferred_origin)
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
                slot.deferred_origin.or(root_deferred_origin)
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
                slot.deferred_origin.or(root_deferred_origin)
            }
            _ => {
                return Err(CtfeError::InvalidOperation {
                    origin: root_origin,
                    message: "invalid CTFE store target".into(),
                });
            }
        };
        root.set_deferred_origin(self.db, deferred_origin);
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
    match const_ty.data(db) {
        ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => int_id.data(db).to_usize(),
        _ => None,
    }
}
