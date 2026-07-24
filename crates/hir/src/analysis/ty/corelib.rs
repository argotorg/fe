use common::ingot::IngotKind;

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{NameDomain, PathRes, resolve_ident_to_bucket, resolve_path},
        ty::{
            trait_resolution::PredicateListId,
            ty_def::{BorrowKind, TyBase, TyData, TyId},
        },
    },
    hir_def::{
        ArithBinOp, BinOp, CallableDef, CompBinOp, Func, IdentId, ItemKind, PathId, Trait, UnOp,
        scope_graph::ScopeId,
    },
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
    #[return_ref]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicPointerReturn {
    FreshMemory,
    InputPointee,
    InputArrayElem,
    InputMemArrayElem,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicMemoryProjection {
    Value,
    Pointee,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntrinsicMemoryAccess {
    pub input: u32,
    pub projection: IntrinsicMemoryProjection,
    pub kind: MemoryAccessKind,
}

impl IntrinsicMemoryAccess {
    const fn value(input: u32, kind: MemoryAccessKind) -> Self {
        Self {
            input,
            projection: IntrinsicMemoryProjection::Value,
            kind,
        }
    }

    const fn pointee(input: u32, kind: MemoryAccessKind) -> Self {
        Self {
            input,
            projection: IntrinsicMemoryProjection::Pointee,
            kind,
        }
    }
}

pub type IntrinsicMemoryContract = &'static [IntrinsicMemoryAccess];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntrinsicContract {
    pub pointer_return: Option<IntrinsicPointerReturn>,
    pub memory: Option<IntrinsicMemoryContract>,
}

const RAW_MEM_READS: &[IntrinsicMemoryAccess] = &[
    IntrinsicMemoryAccess::value(0, MemoryAccessKind::Read),
    IntrinsicMemoryAccess::pointee(1, MemoryAccessKind::Read),
];
const RAW_MEM_WRITES: &[IntrinsicMemoryAccess] = &[
    IntrinsicMemoryAccess::value(0, MemoryAccessKind::MutAccess),
    IntrinsicMemoryAccess::pointee(1, MemoryAccessKind::Write),
];
const READ_POINTEE_0: &[IntrinsicMemoryAccess] =
    &[IntrinsicMemoryAccess::pointee(0, MemoryAccessKind::Read)];
const WRITE_POINTEE_0: &[IntrinsicMemoryAccess] =
    &[IntrinsicMemoryAccess::pointee(0, MemoryAccessKind::Write)];
const COPY_MEMORY: &[IntrinsicMemoryAccess] = &[
    IntrinsicMemoryAccess::pointee(1, MemoryAccessKind::Read),
    IntrinsicMemoryAccess::pointee(0, MemoryAccessKind::Write),
];
const WRITE_POINTEE_1: &[IntrinsicMemoryAccess] =
    &[IntrinsicMemoryAccess::pointee(1, MemoryAccessKind::Write)];
const CALL_MEMORY: &[IntrinsicMemoryAccess] = &[
    IntrinsicMemoryAccess::pointee(3, MemoryAccessKind::Read),
    IntrinsicMemoryAccess::pointee(5, MemoryAccessKind::Write),
];
const STATIC_CALL_MEMORY: &[IntrinsicMemoryAccess] = &[
    IntrinsicMemoryAccess::pointee(2, MemoryAccessKind::Read),
    IntrinsicMemoryAccess::pointee(4, MemoryAccessKind::Write),
];
const READ_POINTEE_1: &[IntrinsicMemoryAccess] =
    &[IntrinsicMemoryAccess::pointee(1, MemoryAccessKind::Read)];
const NO_MEMORY_ACCESSES: IntrinsicMemoryContract = &[];

macro_rules! define_runtime_intrinsics {
    ($($variant:ident => ($ingot:ident, [$($segment:literal),+], $memory:expr, $pointer:expr)),+ $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
        pub enum RuntimeBuiltinFuncKind {
            $($variant),+
        }

        #[salsa::tracked]
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

        fn runtime_intrinsic_contract(kind: RuntimeBuiltinFuncKind) -> IntrinsicContract {
            match kind {
                $(RuntimeBuiltinFuncKind::$variant => IntrinsicContract {
                    pointer_return: $pointer,
                    memory: Some($memory),
                }),+
            }
        }
    };
}

define_runtime_intrinsics! {
    Malloc => (Core, ["ptr", "alloc_raw"], NO_MEMORY_ACCESSES, Some(IntrinsicPointerReturn::FreshMemory)),
    PtrOffsetBytes => (Core, ["ptr", "offset_bytes"], NO_MEMORY_ACCESSES, Some(IntrinsicPointerReturn::InputPointee)),
    PtrEq => (Core, ["ptr", "addr_eq"], NO_MEMORY_ACCESSES, None),
    Mload => (Std, ["evm", "ops", "mload"], READ_POINTEE_0, None),
    Mstore => (Std, ["evm", "ops", "mstore"], WRITE_POINTEE_0, None),
    Mstore8 => (Std, ["evm", "ops", "mstore8"], WRITE_POINTEE_0, None),
    Mcopy => (Core, ["abi", "mcopy"], COPY_MEMORY, None),
    ZeroMem => (Core, ["ptr", "zero_mem"], WRITE_POINTEE_0, None),
    Msize => (Std, ["evm", "ops", "msize"], NO_MEMORY_ACCESSES, None),
    Sload => (Std, ["evm", "ops", "sload"], NO_MEMORY_ACCESSES, None),
    Sstore => (Std, ["evm", "ops", "sstore"], NO_MEMORY_ACCESSES, None),
    CallDataLoad => (Std, ["evm", "ops", "calldataload"], NO_MEMORY_ACCESSES, None),
    CallDataCopy => (Std, ["evm", "ops", "calldatacopy"], WRITE_POINTEE_0, None),
    CallDataSize => (Std, ["evm", "ops", "calldatasize"], NO_MEMORY_ACCESSES, None),
    ReturnDataCopy => (Std, ["evm", "ops", "returndatacopy"], WRITE_POINTEE_0, None),
    ReturnDataSize => (Std, ["evm", "ops", "returndatasize"], NO_MEMORY_ACCESSES, None),
    CodeCopy => (Std, ["evm", "ops", "codecopy"], WRITE_POINTEE_0, None),
    CodeSize => (Std, ["evm", "ops", "codesize"], NO_MEMORY_ACCESSES, None),
    ExtCodeCopy => (Std, ["evm", "ops", "extcodecopy"], WRITE_POINTEE_1, None),
    ExtCodeSize => (Std, ["evm", "ops", "extcodesize"], NO_MEMORY_ACCESSES, None),
    ExtCodeHash => (Std, ["evm", "ops", "extcodehash"], NO_MEMORY_ACCESSES, None),
    Keccak256 => (Std, ["evm", "ops", "keccak256"], READ_POINTEE_0, None),
    AddMod => (Std, ["evm", "ops", "addmod"], NO_MEMORY_ACCESSES, None),
    MulMod => (Std, ["evm", "ops", "mulmod"], NO_MEMORY_ACCESSES, None),
    Byte => (Std, ["evm", "ops", "byte"], NO_MEMORY_ACCESSES, None),
    SignExtend => (Std, ["evm", "ops", "signextend"], NO_MEMORY_ACCESSES, None),
    Address => (Std, ["evm", "ops", "address"], NO_MEMORY_ACCESSES, None),
    Caller => (Std, ["evm", "ops", "caller"], NO_MEMORY_ACCESSES, None),
    CallValue => (Std, ["evm", "ops", "callvalue"], NO_MEMORY_ACCESSES, None),
    Origin => (Std, ["evm", "ops", "origin"], NO_MEMORY_ACCESSES, None),
    GasPrice => (Std, ["evm", "ops", "gasprice"], NO_MEMORY_ACCESSES, None),
    CoinBase => (Std, ["evm", "ops", "coinbase"], NO_MEMORY_ACCESSES, None),
    Balance => (Std, ["evm", "ops", "balance"], NO_MEMORY_ACCESSES, None),
    Timestamp => (Std, ["evm", "ops", "timestamp"], NO_MEMORY_ACCESSES, None),
    Number => (Std, ["evm", "ops", "number"], NO_MEMORY_ACCESSES, None),
    PrevRandao => (Std, ["evm", "ops", "prevrandao"], NO_MEMORY_ACCESSES, None),
    GasLimit => (Std, ["evm", "ops", "gaslimit"], NO_MEMORY_ACCESSES, None),
    ChainId => (Std, ["evm", "ops", "chainid"], NO_MEMORY_ACCESSES, None),
    BaseFee => (Std, ["evm", "ops", "basefee"], NO_MEMORY_ACCESSES, None),
    SelfBalance => (Std, ["evm", "ops", "selfbalance"], NO_MEMORY_ACCESSES, None),
    BlockHash => (Std, ["evm", "ops", "blockhash"], NO_MEMORY_ACCESSES, None),
    BlobHash => (Std, ["evm", "ops", "blobhash"], NO_MEMORY_ACCESSES, None),
    BlobBaseFee => (Std, ["evm", "ops", "blobbasefee"], NO_MEMORY_ACCESSES, None),
    Gas => (Std, ["evm", "ops", "gas"], NO_MEMORY_ACCESSES, None),
    Call => (Std, ["evm", "ops", "call"], CALL_MEMORY, None),
    StaticCall => (Std, ["evm", "ops", "staticcall"], STATIC_CALL_MEMORY, None),
    DelegateCall => (Std, ["evm", "ops", "delegatecall"], STATIC_CALL_MEMORY, None),
    Create => (Std, ["evm", "ops", "create"], READ_POINTEE_1, None),
    Create2 => (Std, ["evm", "ops", "create2"], READ_POINTEE_1, None),
    Log0 => (Std, ["evm", "ops", "log0"], READ_POINTEE_0, None),
    Log1 => (Std, ["evm", "ops", "log1"], READ_POINTEE_0, None),
    Log2 => (Std, ["evm", "ops", "log2"], READ_POINTEE_0, None),
    Log3 => (Std, ["evm", "ops", "log3"], READ_POINTEE_0, None),
    Log4 => (Std, ["evm", "ops", "log4"], READ_POINTEE_0, None),
    Revert => (Std, ["evm", "ops", "revert"], READ_POINTEE_0, None),
    RevertEmpty => (Std, ["evm", "ops", "revert_empty"], NO_MEMORY_ACCESSES, None),
    ReturnData => (Std, ["evm", "ops", "return_data"], READ_POINTEE_0, None),
    SelfDestruct => (Std, ["evm", "ops", "selfdestruct"], NO_MEMORY_ACCESSES, None),
    Stop => (Std, ["evm", "ops", "stop"], NO_MEMORY_ACCESSES, None),
    Panic => (Core, ["panic"], NO_MEMORY_ACCESSES, None),
    PanicWithValue => (Core, ["panic_with_value"], NO_MEMORY_ACCESSES, None),
    Todo => (Core, ["todo"], NO_MEMORY_ACCESSES, None),
    IntrinsicKeccak256 => (Core, ["intrinsic", "__keccak256"], NO_MEMORY_ACCESSES, None),
}

pub fn intrinsic_contract<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Option<IntrinsicContract> {
    let func = func.trait_method_def(db).unwrap_or(func);
    if let Some(runtime) = runtime_builtin_func_kind(db, func) {
        return Some(runtime_intrinsic_contract(runtime));
    }
    let pointer_return = if lib_func_matches(db, func, "core::ptr::array_elem") {
        Some(IntrinsicPointerReturn::InputArrayElem)
    } else if lib_func_matches(db, func, "core::ptr::mem_array_elem") {
        Some(IntrinsicPointerReturn::InputMemArrayElem)
    } else {
        None
    };
    let memory = raw_mem_contract(db, func);
    (pointer_return.is_some() || memory.is_some()).then_some(IntrinsicContract {
        pointer_return,
        memory,
    })
}

fn raw_mem_contract<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Option<IntrinsicMemoryContract> {
    if lib_func_matches(db, func, "std::evm::effects::RawMem::mload") {
        Some(RAW_MEM_READS)
    } else if lib_func_matches(db, func, "std::evm::effects::RawMem::mstore")
        || lib_func_matches(db, func, "std::evm::effects::RawMem::mstore8")
    {
        Some(RAW_MEM_WRITES)
    } else {
        None
    }
}

/// Returns whether `func` declares or implements a sealed EVM capability trait method.
///
/// These receivers are implemented by the zero-sized `std::evm::Evm` token, so
/// memory reachable through another argument cannot alias receiver storage.
pub fn is_std_evm_effect_method<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> bool {
    let func = func.trait_method_def(db).unwrap_or(func);
    let Some(containing_trait) = func.containing_trait(db) else {
        return false;
    };
    [
        "std::evm::effects::Ctx",
        "std::evm::effects::RawMem",
        "std::evm::effects::RawStorage",
        "std::evm::effects::RawOps",
        "std::evm::effects::Log",
        "std::evm::effects::Create",
        "std::evm::effects::Call",
        "std::evm::effects::Super",
    ]
    .into_iter()
    .any(|path| resolve_lib_trait_path(db, func.scope(), path) == Some(containing_trait))
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

#[salsa::tracked]
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

#[salsa::tracked]
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

#[salsa::tracked]
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
