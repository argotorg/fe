use std::fmt;

use fe_analyzer::namespace::items::ContractId;
use id_arena::Id;

use super::{basic_block::BasicBlockId, function::FunctionId, value::ValueId, SourceInfo, TypeId};

pub type InstId = Id<Inst>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Inst {
    pub kind: InstKind,
    pub source: SourceInfo,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstKind {
    /// This is not a real instruction, just used to tag a position where a
    /// local is declared.
    Declare {
        local: ValueId,
    },

    /// Unary instruction.
    Unary {
        op: UnOp,
        value: ValueId,
    },

    /// Binary instruction.
    Binary {
        op: BinOp,
        lhs: ValueId,
        rhs: ValueId,
    },

    Cast {
        kind: CastKind,
        value: ValueId,
        to: TypeId,
    },

    /// Constructs aggregate value, i.e. struct, tuple and array.
    AggregateConstruct {
        ty: TypeId,
        args: Vec<ValueId>,
    },

    Bind {
        src: ValueId,
    },

    MemCopy {
        src: ValueId,
    },

    /// Load a primitive value from a ptr
    Load {
        src: ValueId,
    },

    /// Access to aggregate fields or elements.
    /// # Example
    ///
    /// ```fe
    /// struct Foo:
    ///     x: i32
    ///     y: Array<i32, 8>
    /// ```
    /// `foo.y` is lowered into `AggregateAccess(foo, [1])' for example.
    AggregateAccess {
        value: ValueId,
        indices: Vec<ValueId>,
    },

    MapAccess {
        key: ValueId,
        value: ValueId,
    },

    Call {
        func: FunctionId,
        args: Vec<ValueId>,
        call_type: CallType,
    },

    /// Unconditional jump instruction.
    Jump {
        dest: BasicBlockId,
    },

    /// Conditional branching instruction.
    Branch {
        cond: ValueId,
        then: BasicBlockId,
        else_: BasicBlockId,
    },

    Switch {
        disc: ValueId,
        table: SwitchTable,
        default: Option<BasicBlockId>,
    },

    Revert {
        arg: Option<ValueId>,
    },

    Emit {
        arg: ValueId,
    },

    Return {
        arg: Option<ValueId>,
    },

    Keccak256 {
        arg: ValueId,
    },

    AbiEncode {
        arg: ValueId,
    },

    Nop,

    Create {
        value: ValueId,
        contract: ContractId,
    },

    Create2 {
        value: ValueId,
        salt: ValueId,
        contract: ContractId,
    },

    YulIntrinsic {
        op: YulIntrinsicOp,
        args: Vec<ValueId>,
    },
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct SwitchTable {
    values: Vec<ValueId>,
    blocks: Vec<BasicBlockId>,
}

impl SwitchTable {
    pub fn iter(&self) -> impl Iterator<Item = (ValueId, BasicBlockId)> + '_ {
        self.values.iter().copied().zip(self.blocks.iter().copied())
    }

    pub fn len(&self) -> usize {
        debug_assert!(self.values.len() == self.blocks.len());
        self.values.len()
    }

    pub fn is_empty(&self) -> bool {
        debug_assert!(self.values.len() == self.blocks.len());
        self.values.is_empty()
    }

    pub fn add_arm(&mut self, value: ValueId, block: BasicBlockId) {
        self.values.push(value);
        self.blocks.push(block);
    }
}

impl Inst {
    pub fn new(kind: InstKind, source: SourceInfo) -> Self {
        Self { kind, source }
    }

    pub fn unary(op: UnOp, value: ValueId, source: SourceInfo) -> Self {
        let kind = InstKind::Unary { op, value };
        Self::new(kind, source)
    }

    pub fn binary(op: BinOp, lhs: ValueId, rhs: ValueId, source: SourceInfo) -> Self {
        let kind = InstKind::Binary { op, lhs, rhs };
        Self::new(kind, source)
    }

    pub fn intrinsic(op: YulIntrinsicOp, args: Vec<ValueId>, source: SourceInfo) -> Self {
        let kind = InstKind::YulIntrinsic { op, args };
        Self::new(kind, source)
    }

    pub fn nop() -> Self {
        Self {
            kind: InstKind::Nop,
            source: SourceInfo::dummy(),
        }
    }

    pub fn is_terminator(&self) -> bool {
        match self.kind {
            InstKind::Jump { .. }
            | InstKind::Branch { .. }
            | InstKind::Switch { .. }
            | InstKind::Revert { .. }
            | InstKind::Return { .. } => true,
            InstKind::YulIntrinsic { op, .. } => op.is_terminator(),
            _ => false,
        }
    }

    pub fn branch_info(&self) -> BranchInfo {
        match self.kind {
            InstKind::Jump { dest } => BranchInfo::Jump(dest),
            InstKind::Branch { cond, then, else_ } => BranchInfo::Branch(cond, then, else_),
            InstKind::Switch {
                disc,
                ref table,
                default,
            } => BranchInfo::Switch(disc, table, default),
            _ => BranchInfo::NotBranch,
        }
    }

    pub fn args(&self) -> ValueIter {
        use InstKind::*;
        match &self.kind {
            Declare { local: arg }
            | Bind { src: arg }
            | MemCopy { src: arg }
            | Load { src: arg }
            | Unary { value: arg, .. }
            | Cast { value: arg, .. }
            | Emit { arg }
            | Keccak256 { arg }
            | AbiEncode { arg }
            | Create { value: arg, .. }
            | Branch { cond: arg, .. } => ValueIter::one(*arg),

            Switch { disc, table, .. } => {
                ValueIter::one(*disc).chain(ValueIter::Slice(table.values.iter()))
            }

            Binary { lhs, rhs, .. }
            | MapAccess {
                value: lhs,
                key: rhs,
            }
            | Create2 {
                value: lhs,
                salt: rhs,
                ..
            } => ValueIter::one(*lhs).chain(ValueIter::one(*rhs)),

            Revert { arg } | Return { arg } => ValueIter::One(*arg),

            Nop | Jump { .. } => ValueIter::Zero,

            AggregateAccess { value, indices } => {
                ValueIter::one(*value).chain(ValueIter::Slice(indices.iter()))
            }

            AggregateConstruct { args, .. } | Call { args, .. } | YulIntrinsic { args, .. } => {
                ValueIter::Slice(args.iter())
            }
        }
    }

    pub fn args_mut(&mut self) -> ValueIterMut {
        use InstKind::*;
        match &mut self.kind {
            Declare { local: arg }
            | Bind { src: arg }
            | MemCopy { src: arg }
            | Load { src: arg }
            | Unary { value: arg, .. }
            | Cast { value: arg, .. }
            | Emit { arg }
            | Keccak256 { arg }
            | AbiEncode { arg }
            | Create { value: arg, .. }
            | Branch { cond: arg, .. } => ValueIterMut::one(arg),

            Switch { disc, table, .. } => {
                ValueIterMut::one(disc).chain(ValueIterMut::Slice(table.values.iter_mut()))
            }

            Binary { lhs, rhs, .. }
            | MapAccess {
                value: lhs,
                key: rhs,
            }
            | Create2 {
                value: lhs,
                salt: rhs,
                ..
            } => ValueIterMut::one(lhs).chain(ValueIterMut::one(rhs)),

            Revert { arg } | Return { arg } => ValueIterMut::One(arg.as_mut()),

            Nop | Jump { .. } => ValueIterMut::Zero,

            AggregateAccess { value, indices } => {
                ValueIterMut::one(value).chain(ValueIterMut::Slice(indices.iter_mut()))
            }

            AggregateConstruct { args, .. } | Call { args, .. } | YulIntrinsic { args, .. } => {
                ValueIterMut::Slice(args.iter_mut())
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
    /// `not` operator for logical inversion.
    Not,
    /// `-` operator for negation.
    Neg,
    /// `~` operator for bitwise inversion.
    Inv,
}

impl fmt::Display for UnOp {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Not => write!(w, "not"),
            Self::Neg => write!(w, "-"),
            Self::Inv => write!(w, "~"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Shl,
    Shr,
    BitOr,
    BitXor,
    BitAnd,
    LogicalAnd,
    LogicalOr,
    Eq,
    Ne,
    Ge,
    Gt,
    Le,
    Lt,
}

impl fmt::Display for BinOp {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(w, "+"),
            Self::Sub => write!(w, "-"),
            Self::Mul => write!(w, "*"),
            Self::Div => write!(w, "/"),
            Self::Mod => write!(w, "%"),
            Self::Pow => write!(w, "**"),
            Self::Shl => write!(w, "<<"),
            Self::Shr => write!(w, ">>"),
            Self::BitOr => write!(w, "|"),
            Self::BitXor => write!(w, "^"),
            Self::BitAnd => write!(w, "&"),
            Self::LogicalAnd => write!(w, "and"),
            Self::LogicalOr => write!(w, "or"),
            Self::Eq => write!(w, "=="),
            Self::Ne => write!(w, "!="),
            Self::Ge => write!(w, ">="),
            Self::Gt => write!(w, ">"),
            Self::Le => write!(w, "<="),
            Self::Lt => write!(w, "<"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallType {
    Internal,
    External,
}

impl fmt::Display for CallType {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Internal => write!(w, "internal"),
            Self::External => write!(w, "external"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CastKind {
    /// A cast from a primitive type to a primitive type.
    Primitive,

    /// A cast from an enum type to its underlying type.
    Untag,
}

// TODO: We don't need all yul intrinsics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum YulIntrinsicOp {
    Stop,
    Add,
    Sub,
    Mul,
    Div,
    Sdiv,
    Mod,
    Smod,
    Exp,
    Not,
    Lt,
    Gt,
    Slt,
    Sgt,
    Eq,
    Iszero,
    And,
    Or,
    Xor,
    Byte,
    Shl,
    Shr,
    Sar,
    Addmod,
    Mulmod,
    Signextend,
    Keccak256,
    Pc,
    Pop,
    Mload,
    Mstore,
    Mstore8,
    Sload,
    Sstore,
    Msize,
    Gas,
    Address,
    Balance,
    Selfbalance,
    Caller,
    Callvalue,
    Calldataload,
    Calldatasize,
    Calldatacopy,
    Codesize,
    Codecopy,
    Extcodesize,
    Extcodecopy,
    Returndatasize,
    Returndatacopy,
    Extcodehash,
    Create,
    Create2,
    Call,
    Callcode,
    Delegatecall,
    Staticcall,
    Return,
    Revert,
    Selfdestruct,
    Invalid,
    Log0,
    Log1,
    Log2,
    Log3,
    Log4,
    Chainid,
    Basefee,
    Origin,
    Gasprice,
    Blockhash,
    Coinbase,
    Timestamp,
    Number,
    Prevrandao,
    Gaslimit,
}
impl YulIntrinsicOp {
    pub fn is_terminator(self) -> bool {
        matches!(
            self,
            Self::Return | Self::Revert | Self::Selfdestruct | Self::Invalid
        )
    }
}

impl fmt::Display for YulIntrinsicOp {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        let op = match self {
            Self::Stop => "__stop",
            Self::Add => "__add",
            Self::Sub => "__sub",
            Self::Mul => "__mul",
            Self::Div => "__div",
            Self::Sdiv => "__sdiv",
            Self::Mod => "__mod",
            Self::Smod => "__smod",
            Self::Exp => "__exp",
            Self::Not => "__not",
            Self::Lt => "__lt",
            Self::Gt => "__gt",
            Self::Slt => "__slt",
            Self::Sgt => "__sgt",
            Self::Eq => "__eq",
            Self::Iszero => "__iszero",
            Self::And => "__and",
            Self::Or => "__or",
            Self::Xor => "__xor",
            Self::Byte => "__byte",
            Self::Shl => "__shl",
            Self::Shr => "__shr",
            Self::Sar => "__sar",
            Self::Addmod => "__addmod",
            Self::Mulmod => "__mulmod",
            Self::Signextend => "__signextend",
            Self::Keccak256 => "__keccak256",
            Self::Pc => "__pc",
            Self::Pop => "__pop",
            Self::Mload => "__mload",
            Self::Mstore => "__mstore",
            Self::Mstore8 => "__mstore8",
            Self::Sload => "__sload",
            Self::Sstore => "__sstore",
            Self::Msize => "__msize",
            Self::Gas => "__gas",
            Self::Address => "__address",
            Self::Balance => "__balance",
            Self::Selfbalance => "__selfbalance",
            Self::Caller => "__caller",
            Self::Callvalue => "__callvalue",
            Self::Calldataload => "__calldataload",
            Self::Calldatasize => "__calldatasize",
            Self::Calldatacopy => "__calldatacopy",
            Self::Codesize => "__codesize",
            Self::Codecopy => "__codecopy",
            Self::Extcodesize => "__extcodesize",
            Self::Extcodecopy => "__extcodecopy",
            Self::Returndatasize => "__returndatasize",
            Self::Returndatacopy => "__returndatacopy",
            Self::Extcodehash => "__extcodehash",
            Self::Create => "__create",
            Self::Create2 => "__create2",
            Self::Call => "__call",
            Self::Callcode => "__callcode",
            Self::Delegatecall => "__delegatecall",
            Self::Staticcall => "__staticcall",
            Self::Return => "__return",
            Self::Revert => "__revert",
            Self::Selfdestruct => "__selfdestruct",
            Self::Invalid => "__invalid",
            Self::Log0 => "__log0",
            Self::Log1 => "__log1",
            Self::Log2 => "__log2",
            Self::Log3 => "__log3",
            Self::Log4 => "__log4",
            Self::Chainid => "__chainid",
            Self::Basefee => "__basefee",
            Self::Origin => "__origin",
            Self::Gasprice => "__gasprice",
            Self::Blockhash => "__blockhash",
            Self::Coinbase => "__coinbase",
            Self::Timestamp => "__timestamp",
            Self::Number => "__number",
            Self::Prevrandao => "__prevrandao",
            Self::Gaslimit => "__gaslimit",
        };

        write!(w, "{op}")
    }
}

impl From<fe_analyzer::builtins::Intrinsic> for YulIntrinsicOp {
    fn from(val: fe_analyzer::builtins::Intrinsic) -> Self {
        use fe_analyzer::builtins::Intrinsic;
        match val {
            Intrinsic::__stop => Self::Stop,
            Intrinsic::__add => Self::Add,
            Intrinsic::__sub => Self::Sub,
            Intrinsic::__mul => Self::Mul,
            Intrinsic::__div => Self::Div,
            Intrinsic::__sdiv => Self::Sdiv,
            Intrinsic::__mod => Self::Mod,
            Intrinsic::__smod => Self::Smod,
            Intrinsic::__exp => Self::Exp,
            Intrinsic::__not => Self::Not,
            Intrinsic::__lt => Self::Lt,
            Intrinsic::__gt => Self::Gt,
            Intrinsic::__slt => Self::Slt,
            Intrinsic::__sgt => Self::Sgt,
            Intrinsic::__eq => Self::Eq,
            Intrinsic::__iszero => Self::Iszero,
            Intrinsic::__and => Self::And,
            Intrinsic::__or => Self::Or,
            Intrinsic::__xor => Self::Xor,
            Intrinsic::__byte => Self::Byte,
            Intrinsic::__shl => Self::Shl,
            Intrinsic::__shr => Self::Shr,
            Intrinsic::__sar => Self::Sar,
            Intrinsic::__addmod => Self::Addmod,
            Intrinsic::__mulmod => Self::Mulmod,
            Intrinsic::__signextend => Self::Signextend,
            Intrinsic::__keccak256 => Self::Keccak256,
            Intrinsic::__pc => Self::Pc,
            Intrinsic::__pop => Self::Pop,
            Intrinsic::__mload => Self::Mload,
            Intrinsic::__mstore => Self::Mstore,
            Intrinsic::__mstore8 => Self::Mstore8,
            Intrinsic::__sload => Self::Sload,
            Intrinsic::__sstore => Self::Sstore,
            Intrinsic::__msize => Self::Msize,
            Intrinsic::__gas => Self::Gas,
            Intrinsic::__address => Self::Address,
            Intrinsic::__balance => Self::Balance,
            Intrinsic::__selfbalance => Self::Selfbalance,
            Intrinsic::__caller => Self::Caller,
            Intrinsic::__callvalue => Self::Callvalue,
            Intrinsic::__calldataload => Self::Calldataload,
            Intrinsic::__calldatasize => Self::Calldatasize,
            Intrinsic::__calldatacopy => Self::Calldatacopy,
            Intrinsic::__codesize => Self::Codesize,
            Intrinsic::__codecopy => Self::Codecopy,
            Intrinsic::__extcodesize => Self::Extcodesize,
            Intrinsic::__extcodecopy => Self::Extcodecopy,
            Intrinsic::__returndatasize => Self::Returndatasize,
            Intrinsic::__returndatacopy => Self::Returndatacopy,
            Intrinsic::__extcodehash => Self::Extcodehash,
            Intrinsic::__create => Self::Create,
            Intrinsic::__create2 => Self::Create2,
            Intrinsic::__call => Self::Call,
            Intrinsic::__callcode => Self::Callcode,
            Intrinsic::__delegatecall => Self::Delegatecall,
            Intrinsic::__staticcall => Self::Staticcall,
            Intrinsic::__return => Self::Return,
            Intrinsic::__revert => Self::Revert,
            Intrinsic::__selfdestruct => Self::Selfdestruct,
            Intrinsic::__invalid => Self::Invalid,
            Intrinsic::__log0 => Self::Log0,
            Intrinsic::__log1 => Self::Log1,
            Intrinsic::__log2 => Self::Log2,
            Intrinsic::__log3 => Self::Log3,
            Intrinsic::__log4 => Self::Log4,
            Intrinsic::__chainid => Self::Chainid,
            Intrinsic::__basefee => Self::Basefee,
            Intrinsic::__origin => Self::Origin,
            Intrinsic::__gasprice => Self::Gasprice,
            Intrinsic::__blockhash => Self::Blockhash,
            Intrinsic::__coinbase => Self::Coinbase,
            Intrinsic::__timestamp => Self::Timestamp,
            Intrinsic::__number => Self::Number,
            Intrinsic::__prevrandao => Self::Prevrandao,
            Intrinsic::__gaslimit => Self::Gaslimit,
        }
    }
}

pub enum BranchInfo<'a> {
    NotBranch,
    Jump(BasicBlockId),
    Branch(ValueId, BasicBlockId, BasicBlockId),
    Switch(ValueId, &'a SwitchTable, Option<BasicBlockId>),
}

impl BranchInfo<'_> {
    pub fn is_not_a_branch(&self) -> bool {
        matches!(self, BranchInfo::NotBranch)
    }

    pub fn block_iter(&self) -> BlockIter {
        match self {
            Self::NotBranch => BlockIter::Zero,
            Self::Jump(block) => BlockIter::one(*block),
            Self::Branch(_, then, else_) => BlockIter::one(*then).chain(BlockIter::one(*else_)),
            Self::Switch(_, table, default) => {
                BlockIter::Slice(table.blocks.iter()).chain(BlockIter::One(*default))
            }
        }
    }
}

pub type BlockIter<'a> = IterBase<'a, BasicBlockId>;
pub type ValueIter<'a> = IterBase<'a, ValueId>;
pub type ValueIterMut<'a> = IterMutBase<'a, ValueId>;

pub enum IterBase<'a, T> {
    Zero,
    One(Option<T>),
    Slice(std::slice::Iter<'a, T>),
    Chain(Box<IterBase<'a, T>>, Box<IterBase<'a, T>>),
}

impl<T> IterBase<'_, T> {
    fn one(value: T) -> Self {
        Self::One(Some(value))
    }

    fn chain(self, rhs: Self) -> Self {
        Self::Chain(self.into(), rhs.into())
    }
}

impl<T> Iterator for IterBase<'_, T>
where
    T: Copy,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Zero => None,
            Self::One(value) => value.take(),
            Self::Slice(s) => s.next().copied(),
            Self::Chain(first, second) => {
                if let Some(value) = first.next() {
                    Some(value)
                } else {
                    second.next()
                }
            }
        }
    }
}

pub enum IterMutBase<'a, T> {
    Zero,
    One(Option<&'a mut T>),
    Slice(std::slice::IterMut<'a, T>),
    Chain(Box<IterMutBase<'a, T>>, Box<IterMutBase<'a, T>>),
}

impl<'a, T> IterMutBase<'a, T> {
    fn one(value: &'a mut T) -> Self {
        Self::One(Some(value))
    }

    fn chain(self, rhs: Self) -> Self {
        Self::Chain(self.into(), rhs.into())
    }
}

impl<'a, T> Iterator for IterMutBase<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Zero => None,
            Self::One(value) => value.take(),
            Self::Slice(s) => s.next(),
            Self::Chain(first, second) => {
                if let Some(value) = first.next() {
                    Some(value)
                } else {
                    second.next()
                }
            }
        }
    }
}
