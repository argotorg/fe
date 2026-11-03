use common::ingot::IngotKind;
use num_traits::ToPrimitive;
use rustc_hash::FxHashSet;

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{NameDomain, PathRes, resolve_ident_to_bucket, resolve_path},
        ty::{
            const_ty::demand_concrete_specialized_const_ty,
            trait_def::TraitInstId,
            trait_resolution::{
                GoalSatisfiability, PredicateListId, TraitSolveCx, goal_holds, is_goal_satisfiable,
            },
            ty_def::{BorrowKind, PrimTy, TyBase, TyData, TyId},
            visitor::{TyVisitable, TyVisitor, walk_ty},
        },
    },
    hir_def::{
        ArithBinOp, BinOp, CallableDef, CompBinOp, Func, FuncParamMode, IdentId, ItemKind, PathId,
        Trait, UnOp, scope_graph::ScopeId,
    },
    semantic::EffectRequirementKey,
};

/// Resolve a trait in the core library by an explicit trait path, excluding the "core" root segment.
pub fn resolve_core_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    segments: &[&str],
) -> Option<Trait<'db>> {
    let (module_segments, [trait_name]) = segments.split_last_chunk::<1>()?;
    let mut module_path = lib_root_path(db, scope, "core");

    for segment in module_segments {
        module_path = module_path.push_str(db, segment);
    }

    let assumptions = PredicateListId::empty_list(db);
    let Ok(PathRes::Mod(module_scope)) = resolve_path(db, module_path, scope, assumptions, false)
    else {
        return None;
    };

    let trait_name = IdentId::new(db, trait_name.to_string());
    let bucket = resolve_ident_to_bucket(db, PathId::from_ident(db, trait_name), module_scope);
    bucket.pick(NameDomain::TYPE).as_ref().ok()?.trait_()
}

#[salsa::interned]
#[derive(Debug)]
pub struct LibPath<'db> {
    pub string: String,
}

/// Resolve a type by a fully-qualified `core::...` or `std::...` path string.
///
/// This is a cached wrapper around `resolve_path` intended for backend consumers (e.g. MIR)
/// that need stable access to a small set of core/std helper types.
pub fn resolve_lib_type_path<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path: &str,
) -> Option<TyId<'db>> {
    let path_id = LibPath::new(db, path.to_string());
    resolve_lib_path(db, scope, path_id)
}

/// The concrete value of `n`, a specialized `usize` const such as the `N` of
/// `__keccak256<N>`, whose signature fixes the type of its input.
pub fn concrete_usize<'db>(db: &'db dyn HirAnalysisDb, n: TyId<'db>) -> Option<usize> {
    let usize_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)));
    demand_concrete_specialized_const_ty(db, n, usize_ty)?
        .integer_value(db)?
        .to_usize()
}

/// Resolve a function by a fully-qualified `core::...` or `std::...` path string.
///
/// Returns the `Func` HIR item for the resolved function.
pub fn resolve_lib_func_path<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path: &str,
) -> Option<crate::hir_def::Func<'db>> {
    let path_id = LibPath::new(db, path.to_string());
    resolve_lib_func(db, scope, path_id)
}

/// Resolve a trait by a fully-qualified `core::...` or `std::...` path string.
pub fn resolve_lib_trait_path<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path: &str,
) -> Option<Trait<'db>> {
    let path_id = LibPath::new(db, path.to_string());
    resolve_lib_trait(db, scope, path_id)
}

/// Returns `true` if `func` is the library function at the fully-qualified
/// `core::...` or `std::...` path.
///
/// This resolves from the owning ingot root instead of an arbitrary caller or
/// nested module scope, so backend consumers can classify already-resolved
/// library callees without reintroducing lookup drift.
pub fn lib_func_matches<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>, path: &str) -> bool {
    let func = func.trait_method_def(db).unwrap_or(func);
    resolve_lib_func_path(db, func.scope(), path) == Some(func)
}

#[derive(Clone, Copy)]
pub enum ContractMetadataKind {
    InitCodeOffset,
    InitCodeLen,
}

/// These declarations are implemented by the compiler only for concrete contract
/// types. An unresolved trait call still needs its implementation selected.
pub fn contract_metadata_kind<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Option<ContractMetadataKind> {
    let trait_ = func.containing_trait(db)?;
    if resolve_lib_trait_path(db, func.scope(), "std::evm::Contract") != Some(trait_) {
        return None;
    }
    match func.name(db).to_opt()?.data(db).as_str() {
        "init_code_offset" => Some(ContractMetadataKind::InitCodeOffset),
        "init_code_len" => Some(ContractMetadataKind::InitCodeLen),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MemoryAccessKind {
    Read,
    MutAccess,
    Write,
    Move,
}

impl MemoryAccessKind {
    pub fn borrow_kind(self) -> BorrowKind {
        match self {
            Self::Read => BorrowKind::Ref,
            Self::MutAccess | Self::Write | Self::Move => BorrowKind::Mut,
        }
    }
}

macro_rules! define_runtime_intrinsics {
    ($($variant:ident => ($ingot:ident, [$($segment:literal),+])),+ $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
        pub enum RuntimeBuiltinFuncKind {
            $($variant),+
        }

        #[salsa::tracked(returns(copy))]
        pub fn runtime_builtin_func_kind<'db>(
            db: &'db dyn HirAnalysisDb,
            func: Func<'db>,
        ) -> Option<RuntimeBuiltinFuncKind> {
            let ingot = func.top_mod(db).ingot(db).kind(db);
            let path = runtime_builtin_func_path(db, func)?;
            let builtin = match (ingot, path.as_slice()) {
                $((IngotKind::$ingot, [$($segment),+]) => RuntimeBuiltinFuncKind::$variant),+,
                _ => return None,
            };
            let root = match ingot {
                IngotKind::Core => "core",
                IngotKind::Std => "std",
                _ => return None,
            };
            let mut stable_path = root.to_string();
            for segment in path {
                stable_path.push_str("::");
                stable_path.push_str(segment);
            }
            (resolve_lib_func_path(db, func.scope(), &stable_path) == Some(func))
                .then_some(builtin)
        }

    };
}

define_runtime_intrinsics! {
    Malloc => (Core, ["ptr", "alloc_raw"]),
    TryMalloc => (Core, ["ptr", "try_alloc_raw"]),
    Free => (Core, ["ptr", "dealloc_raw"]),
    Forget => (Core, ["mem", "forget"]),
    NeedsDrop => (Core, ["mem", "needs_drop"]),
    PtrOffsetBytes => (Core, ["ptr", "offset_bytes"]),
    PtrEq => (Core, ["ptr", "addr_eq"]),
    Mload => (Std, ["evm", "ops", "mload"]),
    Mstore => (Std, ["evm", "ops", "mstore"]),
    Mstore8 => (Std, ["evm", "ops", "mstore8"]),
    Mcopy => (Core, ["ptr", "copy_mem"]),
    ZeroMem => (Core, ["ptr", "zero_mem"]),
    Msize => (Std, ["evm", "ops", "msize"]),
    Sload => (Std, ["evm", "ops", "sload"]),
    Sstore => (Std, ["evm", "ops", "sstore"]),
    SloadHashed => (Std, ["evm", "ops", "sload_hashed"]),
    SstoreHashed => (Std, ["evm", "ops", "sstore_hashed"]),
    CallDataLoad => (Std, ["evm", "ops", "calldataload"]),
    CallDataCopy => (Std, ["evm", "ops", "calldatacopy"]),
    CallDataSize => (Std, ["evm", "ops", "calldatasize"]),
    ReturnDataCopy => (Std, ["evm", "ops", "returndatacopy"]),
    ReturnDataSize => (Std, ["evm", "ops", "returndatasize"]),
    CodeCopy => (Std, ["evm", "ops", "codecopy"]),
    CodeSize => (Std, ["evm", "ops", "codesize"]),
    ExtCodeCopy => (Std, ["evm", "ops", "extcodecopy"]),
    ExtCodeSize => (Std, ["evm", "ops", "extcodesize"]),
    ExtCodeHash => (Std, ["evm", "ops", "extcodehash"]),
    Keccak256 => (Std, ["evm", "ops", "keccak256"]),
    AddMod => (Core, ["num", "addmod"]),
    MulMod => (Core, ["num", "mulmod"]),
    LeadingZeros => (Core, ["num", "leading_zeros"]),
    ToBigEndian => (Core, ["num", "to_be"]),
    FromBigEndian => (Core, ["num", "from_be"]),
    ToLittleEndian => (Core, ["num", "to_le"]),
    FromLittleEndian => (Core, ["num", "from_le"]),
    SwapBytes => (Core, ["num", "swap_bytes"]),
    BlackBox => (Core, ["hint", "black_box"]),
    Byte => (Std, ["evm", "ops", "byte"]),
    SignExtend => (Std, ["evm", "ops", "signextend"]),
    Address => (Std, ["evm", "ops", "address"]),
    Caller => (Std, ["evm", "ops", "caller"]),
    CallValue => (Std, ["evm", "ops", "callvalue"]),
    Origin => (Std, ["evm", "ops", "origin"]),
    GasPrice => (Std, ["evm", "ops", "gasprice"]),
    CoinBase => (Std, ["evm", "ops", "coinbase"]),
    Balance => (Std, ["evm", "ops", "balance"]),
    Timestamp => (Std, ["evm", "ops", "timestamp"]),
    Number => (Std, ["evm", "ops", "number"]),
    PrevRandao => (Std, ["evm", "ops", "prevrandao"]),
    GasLimit => (Std, ["evm", "ops", "gaslimit"]),
    ChainId => (Std, ["evm", "ops", "chainid"]),
    BaseFee => (Std, ["evm", "ops", "basefee"]),
    SelfBalance => (Std, ["evm", "ops", "selfbalance"]),
    BlockHash => (Std, ["evm", "ops", "blockhash"]),
    BlobHash => (Std, ["evm", "ops", "blobhash"]),
    BlobBaseFee => (Std, ["evm", "ops", "blobbasefee"]),
    Gas => (Std, ["evm", "ops", "gas"]),
    Call => (Std, ["evm", "ops", "call"]),
    StaticCall => (Std, ["evm", "ops", "staticcall"]),
    StaticCallPrecompile => (Std, ["evm", "ops", "staticcall_precompile"]),
    DelegateCall => (Std, ["evm", "ops", "delegatecall"]),
    Create => (Std, ["evm", "ops", "create"]),
    Create2 => (Std, ["evm", "ops", "create2"]),
    Log0 => (Std, ["evm", "ops", "log0"]),
    Log1 => (Std, ["evm", "ops", "log1"]),
    Log2 => (Std, ["evm", "ops", "log2"]),
    Log3 => (Std, ["evm", "ops", "log3"]),
    Log4 => (Std, ["evm", "ops", "log4"]),
    Revert => (Std, ["evm", "ops", "revert"]),
    RevertEmpty => (Std, ["evm", "ops", "revert_empty"]),
    ReturnData => (Std, ["evm", "ops", "return_data"]),
    SelfDestruct => (Std, ["evm", "ops", "selfdestruct"]),
    Stop => (Std, ["evm", "ops", "stop"]),
    Panic => (Core, ["panic"]),
    PanicWithValue => (Core, ["panic_with_value"]),
    PanicCode => (Core, ["panic_code"]),
    Todo => (Core, ["todo"]),
    IntrinsicKeccak256 => (Core, ["intrinsic", "__keccak256"]),
    IntrinsicKeccakWords => (Core, ["intrinsic", "__keccak_words"]),
    Clear => (Core, ["intrinsic", "clear"]),
}

#[derive(Clone, Copy)]
pub(crate) enum NumericExternIntrinsic {
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
pub(crate) enum CtfeExternIntrinsic {
    SizeOf,
    StorageSlots,
    Keccak256,
    KeccakWords,
    Bitcast,
    Numeric(NumericExternIntrinsic),
    AddMod,
    MulMod,
    LeadingZeros,
    PanicCode,
}

pub(crate) fn ctfe_extern_intrinsic_kind<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Option<CtfeExternIntrinsic> {
    if !func.is_extern(db) || func.body(db).is_some() {
        return None;
    }
    match runtime_builtin_func_kind(db, func) {
        Some(RuntimeBuiltinFuncKind::AddMod) => return Some(CtfeExternIntrinsic::AddMod),
        Some(RuntimeBuiltinFuncKind::MulMod) => return Some(CtfeExternIntrinsic::MulMod),
        Some(RuntimeBuiltinFuncKind::LeadingZeros) => {
            return Some(CtfeExternIntrinsic::LeadingZeros);
        }
        Some(RuntimeBuiltinFuncKind::PanicCode) => return Some(CtfeExternIntrinsic::PanicCode),
        _ => {}
    }
    if lib_func_matches(db, func, "core::intrinsic::size_of") {
        Some(CtfeExternIntrinsic::SizeOf)
    } else if lib_func_matches(db, func, "core::intrinsic::storage_slots") {
        Some(CtfeExternIntrinsic::StorageSlots)
    } else if lib_func_matches(db, func, "core::intrinsic::__keccak256") {
        Some(CtfeExternIntrinsic::Keccak256)
    } else if lib_func_matches(db, func, "core::intrinsic::__keccak_words") {
        Some(CtfeExternIntrinsic::KeccakWords)
    } else if lib_func_matches(db, func, "core::num::__bitcast") {
        Some(CtfeExternIntrinsic::Bitcast)
    } else {
        core_numeric_const_intrinsic(db, func).map(CtfeExternIntrinsic::Numeric)
    }
}

#[derive(Clone, Copy)]
pub(crate) enum SaturatingArithmetic {
    Add,
    Sub,
    Mul,
}

pub(crate) fn numeric_extern_intrinsic(name: &str) -> Option<NumericExternIntrinsic> {
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

pub(crate) fn core_numeric_const_intrinsic<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Option<NumericExternIntrinsic> {
    if func.top_mod(db).ingot(db).kind(db) != IngotKind::Core || func.body(db).is_some() {
        return None;
    }
    let path = runtime_builtin_func_path(db, func)?;
    let [module @ ("num" | "num_intrinsics"), name] = path.as_slice() else {
        return None;
    };
    let kind = numeric_extern_intrinsic(name)?;
    lib_func_matches(db, func, &format!("core::{module}::{name}")).then_some(kind)
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

/// Sealed std capabilities whose methods start external executions, which can
/// reenter the contract and access any persistent or transient slot.
const REENTRANT_CAPABILITIES: [&str; 3] = [
    "std::evm::effects::Call",
    "std::evm::effects::Create",
    "std::evm::effects::Super",
];

/// The sealed std capability that addresses persistent slots directly.
const RAW_STORAGE_CAPABILITY: &str = "std::evm::effects::RawStorage";

/// Whether `trait_` is `target` or has it as a transitive super-trait.
fn trait_reaches<'db>(db: &'db dyn HirAnalysisDb, trait_: Trait<'db>, target: Trait<'db>) -> bool {
    trait_ == target
        || trait_
            .super_trait_bounds(db)
            .any(|bound| trait_reaches(db, bound.def(db), target))
}

/// The persistent and transient state an external execution started by `func`
/// can access. A call or creation can reenter the contract and write any slot;
/// a static call only reads; precompiles cannot call back. Methods of reentrant
/// std capabilities start such executions; shared receiver authority only
/// permits reads, while mutable or owned authority can write.
pub fn external_call_state_access<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    mode: Option<BorrowKind>,
) -> Option<MemoryAccessKind> {
    match runtime_builtin_func_kind(db, func) {
        Some(
            RuntimeBuiltinFuncKind::Call
            | RuntimeBuiltinFuncKind::DelegateCall
            | RuntimeBuiltinFuncKind::Create
            | RuntimeBuiltinFuncKind::Create2,
        ) => return Some(MemoryAccessKind::Write),
        Some(RuntimeBuiltinFuncKind::StaticCall) => return Some(MemoryAccessKind::Read),
        Some(_) => return None,
        None => {}
    }
    let func = func.trait_method_def(db).unwrap_or(func);
    let containing_trait = func.containing_trait(db)?;
    if !REENTRANT_CAPABILITIES
        .iter()
        .any(|path| resolve_lib_trait_path(db, func.scope(), path) == Some(containing_trait))
    {
        return None;
    }
    Some(
        if CallableDef::Func(func).param_mode(db, 0, mode) == FuncParamMode::View {
            MemoryAccessKind::Read
        } else {
            MemoryAccessKind::Write
        },
    )
}

/// The persistent and transient state an effect keyed by `key` reaches in
/// every slot: a reentrant capability can start external executions and a raw
/// storage capability addresses slots directly. Immutable authority only
/// reads.
pub fn effect_key_state_access<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    key: EffectRequirementKey<'db>,
    is_mut: bool,
) -> Option<MemoryAccessKind> {
    let reaches = |path: &&str| {
        let Some(target) = resolve_lib_trait_path(db, scope, path) else {
            return false;
        };
        match key {
            EffectRequirementKey::Trait(inst) => trait_reaches(db, inst.def(db), target),
            EffectRequirementKey::Type(ty) => matches!(
                is_goal_satisfiable(
                    db,
                    TraitSolveCx::new(db, scope).with_assumptions(assumptions),
                    TraitInstId::new_simple(db, target, vec![ty]),
                ),
                GoalSatisfiability::Satisfied(_)
            ),
            // A row reaches what its components reach, once it expands.
            EffectRequirementKey::Row(_) | EffectRequirementKey::Other => false,
        }
    };
    REENTRANT_CAPABILITIES
        .iter()
        .chain([&RAW_STORAGE_CAPABILITY])
        .any(reaches)
        .then_some(if is_mut {
            MemoryAccessKind::Write
        } else {
            MemoryAccessKind::Read
        })
}

/// The seal of the std EVM capabilities, which only the zero-sized
/// `std::evm::Evm` implements.
const EVM_CAPABILITY_SEAL: &str = "std::evm::effects::EvmCapabilitySeal";

/// Whether every bound `assumptions` place on the type parameters of `ty`
/// is a sealed std EVM capability trait, which only `std::evm::Evm`
/// implements: a generic body then reaches no more through a value of `ty`
/// than those bounds give it, whatever `ty` is instantiated with. A bound of
/// any other trait could be implemented to reach more.
pub fn bounded_by_sealed_capabilities<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    ty: TyId<'db>,
) -> bool {
    struct Params<'db> {
        db: &'db dyn HirAnalysisDb,
        found: FxHashSet<TyId<'db>>,
    }
    impl<'db> TyVisitor<'db> for Params<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }
        fn visit_ty(&mut self, ty: TyId<'db>) {
            if matches!(ty.data(self.db), TyData::TyParam(_)) {
                self.found.insert(ty);
            }
            walk_ty(self, ty)
        }
    }
    let params = |ty: TyId<'db>| {
        let mut params = Params {
            db,
            found: FxHashSet::default(),
        };
        ty.visit_with(&mut params);
        params.found
    };
    let Some(seal) = resolve_lib_trait_path(db, scope, EVM_CAPABILITY_SEAL) else {
        return false;
    };
    // An effect's provider evidence (`EffectRef`) names its binding, not a
    // trait an implementation could make reach more.
    let effect_ref = resolve_core_trait(db, scope, &["EffectRef"]);
    let own = params(ty);
    !own.is_empty()
        && assumptions.list(db).iter().all(|bound| {
            effect_ref.is_some_and(|effect_ref| trait_reaches(db, bound.def(db), effect_ref))
                || !bound
                    .args(db)
                    .iter()
                    .any(|arg| !params(*arg).is_disjoint(&own))
                || trait_reaches(db, bound.def(db), seal)
        })
}

/// Whether `ty` implements the std EVM capability seal, so a place of it holds
/// nothing even when `ty` is a type parameter bounded by `Ctx` or `Log`.
pub fn is_sealed_evm_capability<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    ty: TyId<'db>,
) -> bool {
    resolve_lib_trait_path(db, scope, EVM_CAPABILITY_SEAL).is_some_and(|seal| {
        goal_holds(
            db,
            TraitSolveCx::new(db, scope).with_assumptions(assumptions),
            TraitInstId::new_simple(db, seal, vec![ty]),
        )
    })
}

fn runtime_builtin_func_path<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Option<Vec<&'db str>> {
    let mut segments = Vec::new();
    let mut scope = Some(func.scope());
    while let Some(current) = scope {
        let name = current.name(db)?;
        segments.push(name.data(db).as_str());
        scope = current.parent(db);
    }
    segments.reverse();
    if segments.first() == Some(&"lib") {
        segments.remove(0);
    }
    Some(segments)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveWrapperCallKind {
    Unary(UnOp),
    Binary(BinOp),
    Assign(BinOp),
}

pub fn core_primitive_wrapper_call_kind<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    result_ty: TyId<'db>,
) -> Option<PrimitiveWrapperCallKind> {
    if func.top_mod(db).ingot(db).kind(db) != IngotKind::Core {
        return None;
    }
    let Some(ItemKind::ImplTrait(impl_trait)) = func.scope().parent_item(db) else {
        return None;
    };
    // Core also defines operators for aggregates (for example String, arrays,
    // and tuples). Their method bodies carry the comparison semantics and must
    // not be bypassed just because the trait and method names match.
    let self_ty = impl_trait.ty(db);
    if !self_ty.is_integral(db) && !self_ty.is_bool(db) && self_ty.as_ptr(db).is_none() {
        return None;
    }
    let method = func.name(db).to_opt()?.data(db);
    let matches_trait = |segments: &[&str]| {
        impl_trait.trait_def(db) == resolve_core_trait(db, func.scope(), segments)
    };
    Some(if method == "add" && matches_trait(&["ops", "Add"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Arith(ArithBinOp::Add))
    } else if method == "sub" && matches_trait(&["ops", "Sub"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Arith(ArithBinOp::Sub))
    } else if method == "mul" && matches_trait(&["ops", "Mul"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Arith(ArithBinOp::Mul))
    } else if method == "div" && matches_trait(&["ops", "Div"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Arith(ArithBinOp::Div))
    } else if method == "rem" && matches_trait(&["ops", "Rem"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Arith(ArithBinOp::Rem))
    } else if method == "pow" && matches_trait(&["ops", "Pow"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Arith(ArithBinOp::Pow))
    } else if method == "shl" && matches_trait(&["ops", "Shl"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Arith(ArithBinOp::LShift))
    } else if method == "shr" && matches_trait(&["ops", "Shr"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Arith(ArithBinOp::RShift))
    } else if method == "bitand" && matches_trait(&["ops", "BitAnd"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Arith(ArithBinOp::BitAnd))
    } else if method == "bitor" && matches_trait(&["ops", "BitOr"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Arith(ArithBinOp::BitOr))
    } else if method == "bitxor" && matches_trait(&["ops", "BitXor"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Arith(ArithBinOp::BitXor))
    } else if method == "eq" && matches_trait(&["ops", "Eq"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Comp(CompBinOp::Eq))
    } else if method == "ne" && matches_trait(&["ops", "Eq"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Comp(CompBinOp::NotEq))
    } else if method == "lt" && matches_trait(&["ops", "Ord"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Comp(CompBinOp::Lt))
    } else if method == "le" && matches_trait(&["ops", "Ord"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Comp(CompBinOp::LtEq))
    } else if method == "gt" && matches_trait(&["ops", "Ord"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Comp(CompBinOp::Gt))
    } else if method == "ge" && matches_trait(&["ops", "Ord"]) {
        PrimitiveWrapperCallKind::Binary(BinOp::Comp(CompBinOp::GtEq))
    } else if method == "neg" && matches_trait(&["ops", "Neg"]) {
        PrimitiveWrapperCallKind::Unary(UnOp::Minus)
    } else if method == "bit_not" && matches_trait(&["ops", "BitNot"]) {
        PrimitiveWrapperCallKind::Unary(UnOp::BitNot)
    } else if method == "not" && matches_trait(&["ops", "Not"]) {
        PrimitiveWrapperCallKind::Unary(if result_ty == TyId::bool(db) {
            UnOp::Not
        } else {
            UnOp::BitNot
        })
    } else if method == "add_assign" && matches_trait(&["ops", "AddAssign"]) {
        PrimitiveWrapperCallKind::Assign(BinOp::Arith(ArithBinOp::Add))
    } else if method == "sub_assign" && matches_trait(&["ops", "SubAssign"]) {
        PrimitiveWrapperCallKind::Assign(BinOp::Arith(ArithBinOp::Sub))
    } else if method == "mul_assign" && matches_trait(&["ops", "MulAssign"]) {
        PrimitiveWrapperCallKind::Assign(BinOp::Arith(ArithBinOp::Mul))
    } else if method == "div_assign" && matches_trait(&["ops", "DivAssign"]) {
        PrimitiveWrapperCallKind::Assign(BinOp::Arith(ArithBinOp::Div))
    } else if method == "rem_assign" && matches_trait(&["ops", "RemAssign"]) {
        PrimitiveWrapperCallKind::Assign(BinOp::Arith(ArithBinOp::Rem))
    } else if method == "pow_assign" && matches_trait(&["ops", "PowAssign"]) {
        PrimitiveWrapperCallKind::Assign(BinOp::Arith(ArithBinOp::Pow))
    } else if method == "shl_assign" && matches_trait(&["ops", "ShlAssign"]) {
        PrimitiveWrapperCallKind::Assign(BinOp::Arith(ArithBinOp::LShift))
    } else if method == "shr_assign" && matches_trait(&["ops", "ShrAssign"]) {
        PrimitiveWrapperCallKind::Assign(BinOp::Arith(ArithBinOp::RShift))
    } else if method == "bitand_assign" && matches_trait(&["ops", "BitAndAssign"]) {
        PrimitiveWrapperCallKind::Assign(BinOp::Arith(ArithBinOp::BitAnd))
    } else if method == "bitor_assign" && matches_trait(&["ops", "BitOrAssign"]) {
        PrimitiveWrapperCallKind::Assign(BinOp::Arith(ArithBinOp::BitOr))
    } else if method == "bitxor_assign" && matches_trait(&["ops", "BitXorAssign"]) {
        PrimitiveWrapperCallKind::Assign(BinOp::Arith(ArithBinOp::BitXor))
    } else {
        return None;
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CoreRangeTypes<'db> {
    pub range: TyId<'db>,
    pub known: TyId<'db>,
    pub unknown: TyId<'db>,
}

pub fn resolve_core_range_types<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
) -> Option<CoreRangeTypes<'db>> {
    let range = resolve_lib_type_path(db, scope, "core::range::Range")?;
    let known = resolve_lib_type_path(db, scope, "core::range::Known")?;
    let unknown = resolve_lib_type_path(db, scope, "core::range::Unknown")?;
    Some(CoreRangeTypes {
        range,
        known,
        unknown,
    })
}

#[salsa::tracked(returns(copy))]
fn resolve_lib_path<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path: LibPath<'db>,
) -> Option<TyId<'db>> {
    let mut segments = path.string(db).split("::");
    let mut path = lib_root_path(db, scope, segments.next()?);

    for segment in segments {
        path = path.push_str(db, segment);
    }

    let assumptions = PredicateListId::empty_list(db);
    match resolve_path(db, path, scope, assumptions, true).ok()? {
        PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => Some(ty),
        _ => None,
    }
}

#[salsa::tracked(returns(copy))]
fn resolve_lib_func<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path: LibPath<'db>,
) -> Option<Func<'db>> {
    let mut segments = path.string(db).split("::");
    let mut path = lib_root_path(db, scope, segments.next()?);

    for segment in segments {
        path = path.push_str(db, segment);
    }

    let assumptions = PredicateListId::empty_list(db);
    match resolve_path(db, path, scope, assumptions, true).ok()? {
        PathRes::Func(ty) => {
            let TyData::TyBase(TyBase::Func(CallableDef::Func(func))) = ty.data(db) else {
                return None;
            };
            Some(*func)
        }
        PathRes::TraitMethod(_, func) => Some(func),
        _ => None,
    }
}

#[salsa::tracked(returns(copy))]
fn resolve_lib_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path: LibPath<'db>,
) -> Option<Trait<'db>> {
    let mut segments = path.string(db).split("::");
    let mut path = lib_root_path(db, scope, segments.next()?);

    for segment in segments {
        path = path.push_str(db, segment);
    }

    let assumptions = PredicateListId::empty_list(db);
    match resolve_path(db, path, scope, assumptions, true).ok()? {
        PathRes::Trait(trait_) => Some(trait_.def(db)),
        _ => None,
    }
}

pub(crate) fn lib_root_path<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    root: &str,
) -> PathId<'db> {
    let ingot_kind = scope.top_mod(db).ingot(db).kind(db);
    if (ingot_kind == IngotKind::Core && root == "core")
        || (ingot_kind == IngotKind::Std && root == "std")
    {
        PathId::from_ident(db, IdentId::make_ingot(db))
    } else {
        PathId::from_str(db, root)
    }
}
