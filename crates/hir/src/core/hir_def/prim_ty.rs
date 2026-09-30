use salsa::Update;

use super::IdentId;
use crate::HirDb;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum PrimTy {
    Bool,
    Int(IntTy),
    Uint(UintTy),
    String,
}

impl PrimTy {
    pub fn name(self, db: &dyn HirDb) -> IdentId<'_> {
        IdentId::new(db, self.as_str())
    }

    /// The builtin type named `name`, matched without interning every
    /// builtin name.
    pub fn from_name(db: &dyn HirDb, name: IdentId<'_>) -> Option<Self> {
        let name = name.data(db);
        Self::all_types()
            .iter()
            .copied()
            .find(|prim| prim.as_str() == name)
    }

    fn as_str(self) -> &'static str {
        match self {
            PrimTy::Bool => "bool",
            PrimTy::Int(IntTy::I8) => "i8",
            PrimTy::Int(IntTy::I16) => "i16",
            PrimTy::Int(IntTy::I32) => "i32",
            PrimTy::Int(IntTy::I64) => "i64",
            PrimTy::Int(IntTy::I128) => "i128",
            PrimTy::Int(IntTy::I256) => "i256",
            PrimTy::Int(IntTy::Isize) => "isize",
            PrimTy::Uint(UintTy::U8) => "u8",
            PrimTy::Uint(UintTy::U16) => "u16",
            PrimTy::Uint(UintTy::U32) => "u32",
            PrimTy::Uint(UintTy::U64) => "u64",
            PrimTy::Uint(UintTy::U128) => "u128",
            PrimTy::Uint(UintTy::U256) => "u256",
            PrimTy::Uint(UintTy::Usize) => "usize",
            PrimTy::String => "String",
        }
    }

    pub fn all_types() -> &'static [PrimTy] {
        &[
            PrimTy::Bool,
            PrimTy::Int(IntTy::I8),
            PrimTy::Int(IntTy::I16),
            PrimTy::Int(IntTy::I32),
            PrimTy::Int(IntTy::I64),
            PrimTy::Int(IntTy::I128),
            PrimTy::Int(IntTy::I256),
            PrimTy::Int(IntTy::Isize),
            PrimTy::Uint(UintTy::U8),
            PrimTy::Uint(UintTy::U16),
            PrimTy::Uint(UintTy::U32),
            PrimTy::Uint(UintTy::U64),
            PrimTy::Uint(UintTy::U128),
            PrimTy::Uint(UintTy::U256),
            PrimTy::Uint(UintTy::Usize),
            PrimTy::String,
        ]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
    Isize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UintTy {
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    Usize,
}
