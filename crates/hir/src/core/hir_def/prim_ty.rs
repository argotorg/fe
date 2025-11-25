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
        match self {
            PrimTy::Bool => IdentId::make_bool(db),
            PrimTy::Int(ty) => ty.name(db),
            PrimTy::Uint(ty) => ty.name(db),
            PrimTy::String => IdentId::new(db, "String".to_string()),
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

impl IntTy {
    pub fn name(self, db: &dyn HirDb) -> IdentId<'_> {
        match self {
            IntTy::I8 => IdentId::make_i8(db),
            IntTy::I16 => IdentId::make_i16(db),
            IntTy::I32 => IdentId::make_i32(db),
            IntTy::I64 => IdentId::make_i64(db),
            IntTy::I128 => IdentId::make_i128(db),
            IntTy::I256 => IdentId::make_i256(db),
            IntTy::Isize => IdentId::make_isize(db),
        }
    }
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

impl UintTy {
    pub fn name(self, db: &dyn HirDb) -> IdentId<'_> {
        match self {
            UintTy::U8 => IdentId::make_u8(db),
            UintTy::U16 => IdentId::make_u16(db),
            UintTy::U32 => IdentId::make_u32(db),
            UintTy::U64 => IdentId::make_u64(db),
            UintTy::U128 => IdentId::make_u128(db),
            UintTy::U256 => IdentId::make_u256(db),
            UintTy::Usize => IdentId::make_usize(db),
        }
    }
}
