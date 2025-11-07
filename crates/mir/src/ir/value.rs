use id_arena::Id;
use num_bigint::BigInt;
use smol_str::SmolStr;

use crate::db::MirDb;

use super::{
    constant::ConstantId,
    function::BodyDataStore,
    inst::InstId,
    types::{TypeId, TypeKind},
    SourceInfo,
};

pub type ValueId = Id<Value>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    /// A value resulted from an instruction.
    Temporary { inst: InstId, ty: TypeId },

    /// A local variable declared in a function body.
    Local(Local),

    /// An immediate value.
    Immediate { imm: BigInt, ty: TypeId },

    /// A constant value.
    Constant { constant: ConstantId, ty: TypeId },

    /// A singleton value representing `Unit` type.
    Unit { ty: TypeId },
}

impl Value {
    pub fn ty(&self) -> TypeId {
        match self {
            Self::Local(val) => val.ty,
            Self::Immediate { ty, .. }
            | Self::Temporary { ty, .. }
            | Self::Unit { ty }
            | Self::Constant { ty, .. } => *ty,
        }
    }

    pub fn is_imm(&self) -> bool {
        matches!(self, Self::Immediate { .. })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssignableValue {
    Value(ValueId),
    Aggregate {
        lhs: Box<AssignableValue>,
        idx: ValueId,
    },
    Map {
        lhs: Box<AssignableValue>,
        key: ValueId,
    },
}

impl From<ValueId> for AssignableValue {
    fn from(value: ValueId) -> Self {
        Self::Value(value)
    }
}

impl AssignableValue {
    pub fn ty(&self, db: &dyn MirDb, store: &BodyDataStore) -> TypeId {
        match self {
            Self::Value(value) => store.value_ty(*value),
            Self::Aggregate { lhs, idx } => {
                let lhs_ty = lhs.ty(db, store);
                lhs_ty.projection_ty(db, store.value_data(*idx))
            }
            Self::Map { lhs, .. } => {
                let lhs_ty = lhs.ty(db, store).deref(db);
                match lhs_ty.data(db).kind {
                    TypeKind::Map(def) => def.value_ty.make_sptr(db),
                    _ => unreachable!(),
                }
            }
        }
    }

    pub fn value_id(&self) -> Option<ValueId> {
        match self {
            Self::Value(value) => Some(*value),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Local {
    /// An original name of a local variable.
    pub name: SmolStr,

    pub ty: TypeId,

    /// `true` if a local is a function argument.
    pub is_arg: bool,

    /// `true` if a local is introduced in MIR.
    pub is_tmp: bool,

    pub source: SourceInfo,
}

impl Local {
    pub fn user_local(name: SmolStr, ty: TypeId, source: SourceInfo) -> Local {
        Self {
            name,
            ty,
            is_arg: false,
            is_tmp: false,
            source,
        }
    }

    pub fn arg_local(name: SmolStr, ty: TypeId, source: SourceInfo) -> Local {
        Self {
            name,
            ty,
            is_arg: true,
            is_tmp: false,
            source,
        }
    }

    pub fn tmp_local(name: SmolStr, ty: TypeId) -> Local {
        Self {
            name,
            ty,
            is_arg: false,
            is_tmp: true,
            source: SourceInfo::dummy(),
        }
    }
}
