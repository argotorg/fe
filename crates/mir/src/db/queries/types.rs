use std::{fmt, rc::Rc, str::FromStr};

use fe_analyzer::namespace::{items::EnumVariantId, types as analyzer_types};

use num_bigint::BigInt;
use num_traits::ToPrimitive;

use crate::{
    db::MirDb,
    ir::{
        types::{ArrayDef, TupleDef, TypeKind},
        Type, TypeId, Value,
    },
    lower::types::lower_type,
};

pub fn mir_lowered_type(db: &dyn MirDb, analyzer_type: analyzer_types::TypeId) -> TypeId {
    lower_type(db, analyzer_type)
}

impl TypeId {
    pub fn data(self, db: &dyn MirDb) -> Rc<Type> {
        db.lookup_mir_intern_type(self)
    }

    pub fn analyzer_ty(self, db: &dyn MirDb) -> Option<analyzer_types::TypeId> {
        self.data(db).analyzer_ty
    }

    pub fn projection_ty(self, db: &dyn MirDb, access: &Value) -> TypeId {
        let ty = self.deref(db);
        let pty = match &ty.data(db).kind {
            TypeKind::Array(ArrayDef { elem_ty, .. }) => *elem_ty,
            TypeKind::Tuple(def) => {
                let index = expect_projection_index(access);
                def.items[index]
            }
            TypeKind::Struct(def) | TypeKind::Contract(def) => {
                let index = expect_projection_index(access);
                def.fields[index].1
            }
            TypeKind::Enum(_) => {
                let index = expect_projection_index(access);
                debug_assert_eq!(index, 0);
                ty.projection_ty_imm(db, 0)
            }
            _ => panic!("{:?} can't project onto the `access`", self.as_string(db)),
        };
        match &self.data(db).kind {
            TypeKind::SPtr(_) | TypeKind::Contract(_) => pty.make_sptr(db),
            TypeKind::MPtr(_) => pty.make_mptr(db),
            _ => pty,
        }
    }

    pub fn deref(self, db: &dyn MirDb) -> TypeId {
        match self.data(db).kind {
            TypeKind::SPtr(inner) => inner,
            TypeKind::MPtr(inner) => inner.deref(db),
            _ => self,
        }
    }

    pub fn make_sptr(self, db: &dyn MirDb) -> TypeId {
        db.mir_intern_type(Type::new(TypeKind::SPtr(self), None).into())
    }

    pub fn make_mptr(self, db: &dyn MirDb) -> TypeId {
        db.mir_intern_type(Type::new(TypeKind::MPtr(self), None).into())
    }

    pub fn projection_ty_imm(self, db: &dyn MirDb, index: usize) -> TypeId {
        match &self.data(db).kind {
            TypeKind::Array(ArrayDef { elem_ty, .. }) => *elem_ty,
            TypeKind::Tuple(def) => def.items[index],
            TypeKind::Struct(def) | TypeKind::Contract(def) => def.fields[index].1,
            TypeKind::Enum(_) => {
                debug_assert_eq!(index, 0);
                self.enum_disc_type(db)
            }
            _ => panic!("{:?} can't project onto the `index`", self.as_string(db)),
        }
    }

    pub fn aggregate_field_num(self, db: &dyn MirDb) -> usize {
        match &self.data(db).kind {
            TypeKind::Array(ArrayDef { len, .. }) => *len,
            TypeKind::Tuple(def) => def.items.len(),
            TypeKind::Struct(def) | TypeKind::Contract(def) => def.fields.len(),
            TypeKind::Enum(_) => 2,
            _ => unreachable!(),
        }
    }

    pub fn enum_disc_type(self, db: &dyn MirDb) -> TypeId {
        let kind = match &self.deref(db).data(db).kind {
            TypeKind::Enum(def) => def.tag_type(),
            _ => unreachable!(),
        };
        let analyzer_type = match kind {
            TypeKind::U8 => Some(analyzer_types::Integer::U8),
            TypeKind::U16 => Some(analyzer_types::Integer::U16),
            TypeKind::U32 => Some(analyzer_types::Integer::U32),
            TypeKind::U64 => Some(analyzer_types::Integer::U64),
            TypeKind::U128 => Some(analyzer_types::Integer::U128),
            TypeKind::U256 => Some(analyzer_types::Integer::U256),
            _ => None,
        }
        .map(|int| analyzer_types::TypeId::int(db.upcast(), int));

        db.mir_intern_type(Type::new(kind, analyzer_type).into())
    }

    pub fn enum_data_offset(self, db: &dyn MirDb, slot_size: usize) -> usize {
        match &self.data(db).kind {
            TypeKind::Enum(def) => {
                let disc_size = self.enum_disc_type(db).size_of(db, slot_size);
                let mut align = 1;
                for variant in def.variants.iter() {
                    let variant_align = variant.ty.align_of(db, slot_size);
                    align = num_integer::lcm(align, variant_align);
                }
                round_up(disc_size, align)
            }
            _ => unreachable!(),
        }
    }

    pub fn enum_variant_type(self, db: &dyn MirDb, variant_id: EnumVariantId) -> TypeId {
        let name = variant_id.name(db.upcast());
        match &self.deref(db).data(db).kind {
            TypeKind::Enum(def) => def
                .variants
                .iter()
                .find(|variant| variant.name == name)
                .map(|variant| variant.ty)
                .unwrap(),
            _ => unreachable!(),
        }
    }

    pub fn index_from_fname(self, db: &dyn MirDb, fname: &str) -> BigInt {
        let ty = self.deref(db);
        match &ty.data(db).kind {
            TypeKind::Tuple(_) => {
                // TODO: Fix this when the syntax for tuple access changes.
                let index_str = &fname[4..];
                BigInt::from_str(index_str).unwrap()
            }

            TypeKind::Struct(def) | TypeKind::Contract(def) => def
                .fields
                .iter()
                .enumerate()
                .find_map(|(i, field)| (field.0 == fname).then(|| i.into()))
                .unwrap(),

            other => unreachable!("{:?} does not have fields", other),
        }
    }

    pub fn is_primitive(self, db: &dyn MirDb) -> bool {
        matches!(
            &self.data(db).kind,
            TypeKind::I8
                | TypeKind::I16
                | TypeKind::I32
                | TypeKind::I64
                | TypeKind::I128
                | TypeKind::I256
                | TypeKind::U8
                | TypeKind::U16
                | TypeKind::U32
                | TypeKind::U64
                | TypeKind::U128
                | TypeKind::U256
                | TypeKind::Bool
                | TypeKind::Address
                | TypeKind::Unit
        )
    }

    pub fn is_integral(self, db: &dyn MirDb) -> bool {
        matches!(
            &self.data(db).kind,
            TypeKind::I8
                | TypeKind::I16
                | TypeKind::I32
                | TypeKind::I64
                | TypeKind::I128
                | TypeKind::I256
                | TypeKind::U8
                | TypeKind::U16
                | TypeKind::U32
                | TypeKind::U64
                | TypeKind::U128
                | TypeKind::U256
        )
    }

    pub fn is_address(self, db: &dyn MirDb) -> bool {
        matches!(&self.data(db).kind, TypeKind::Address)
    }

    pub fn is_unit(self, db: &dyn MirDb) -> bool {
        matches!(&self.data(db).as_ref().kind, TypeKind::Unit)
    }

    pub fn is_enum(self, db: &dyn MirDb) -> bool {
        matches!(&self.data(db).as_ref().kind, TypeKind::Enum(_))
    }

    pub fn is_signed(self, db: &dyn MirDb) -> bool {
        matches!(
            &self.data(db).kind,
            TypeKind::I8
                | TypeKind::I16
                | TypeKind::I32
                | TypeKind::I64
                | TypeKind::I128
                | TypeKind::I256
        )
    }

    /// Returns size of the type in bytes.
    pub fn size_of(self, db: &dyn MirDb, slot_size: usize) -> usize {
        match &self.data(db).kind {
            TypeKind::Bool | TypeKind::I8 | TypeKind::U8 => 1,
            TypeKind::I16 | TypeKind::U16 => 2,
            TypeKind::I32 | TypeKind::U32 => 4,
            TypeKind::I64 | TypeKind::U64 => 8,
            TypeKind::I128 | TypeKind::U128 => 16,
            TypeKind::String(len) => 32 + len,
            TypeKind::MPtr(..)
            | TypeKind::SPtr(..)
            | TypeKind::I256
            | TypeKind::U256
            | TypeKind::Map(_) => 32,
            TypeKind::Address => 20,
            TypeKind::Unit => 0,

            TypeKind::Array(def) => array_elem_size_imp(db, def, slot_size) * def.len,

            TypeKind::Tuple(def) => {
                if def.items.is_empty() {
                    return 0;
                }
                let last_idx = def.items.len() - 1;
                self.aggregate_elem_offset(db, last_idx, slot_size)
                    + def.items[last_idx].size_of(db, slot_size)
            }

            TypeKind::Struct(def) | TypeKind::Contract(def) => {
                if def.fields.is_empty() {
                    return 0;
                }
                let last_idx = def.fields.len() - 1;
                self.aggregate_elem_offset(db, last_idx, slot_size)
                    + def.fields[last_idx].1.size_of(db, slot_size)
            }

            TypeKind::Enum(def) => {
                let data_offset = self.enum_data_offset(db, slot_size);
                let maximum_data_size = def
                    .variants
                    .iter()
                    .map(|variant| variant.ty.size_of(db, slot_size))
                    .max()
                    .unwrap_or(0);
                data_offset + maximum_data_size
            }
        }
    }

    pub fn is_zero_sized(self, db: &dyn MirDb) -> bool {
        // It's ok to use 1 as a slot size because slot size doesn't affect whether a
        // type is zero sized or not.
        self.size_of(db, 1) == 0
    }

    pub fn align_of(self, db: &dyn MirDb, slot_size: usize) -> usize {
        if self.is_primitive(db) {
            1
        } else {
            // TODO: Too naive, we could implement more efficient layout for aggregate
            // types.
            slot_size
        }
    }

    /// Returns an offset of the element of aggregate type.
    pub fn aggregate_elem_offset<T>(self, db: &dyn MirDb, elem_idx: T, slot_size: usize) -> usize
    where
        T: num_traits::ToPrimitive,
    {
        debug_assert!(self.is_aggregate(db));
        debug_assert!(elem_idx.to_usize().unwrap() < self.aggregate_field_num(db));
        let elem_idx = elem_idx.to_usize().unwrap();

        if elem_idx == 0 {
            return 0;
        }

        match &self.data(db).kind {
            TypeKind::Array(def) => array_elem_size_imp(db, def, slot_size) * elem_idx,
            TypeKind::Enum(_) => self.enum_data_offset(db, slot_size),
            _ => {
                let mut offset = self.aggregate_elem_offset(db, elem_idx - 1, slot_size)
                    + self
                        .projection_ty_imm(db, elem_idx - 1)
                        .size_of(db, slot_size);

                let elem_ty = self.projection_ty_imm(db, elem_idx);
                if (offset % slot_size + elem_ty.size_of(db, slot_size)) > slot_size {
                    offset = round_up(offset, slot_size);
                }

                round_up(offset, elem_ty.align_of(db, slot_size))
            }
        }
    }

    pub fn is_aggregate(self, db: &dyn MirDb) -> bool {
        matches!(
            &self.data(db).kind,
            TypeKind::Array(_)
                | TypeKind::Tuple(_)
                | TypeKind::Struct(_)
                | TypeKind::Enum(_)
                | TypeKind::Contract(_)
        )
    }

    pub fn is_struct(self, db: &dyn MirDb) -> bool {
        matches!(&self.data(db).as_ref().kind, TypeKind::Struct(_))
    }

    pub fn is_array(self, db: &dyn MirDb) -> bool {
        matches!(&self.data(db).kind, TypeKind::Array(_))
    }

    pub fn is_string(self, db: &dyn MirDb) -> bool {
        matches! {
            &self.data(db).kind,
            TypeKind::String(_)
        }
    }

    pub fn is_ptr(self, db: &dyn MirDb) -> bool {
        self.is_mptr(db) || self.is_sptr(db)
    }

    pub fn is_mptr(self, db: &dyn MirDb) -> bool {
        matches!(self.data(db).kind, TypeKind::MPtr(_))
    }

    pub fn is_sptr(self, db: &dyn MirDb) -> bool {
        matches!(self.data(db).kind, TypeKind::SPtr(_))
    }

    pub fn is_map(self, db: &dyn MirDb) -> bool {
        matches!(self.data(db).kind, TypeKind::Map(_))
    }

    pub fn is_contract(self, db: &dyn MirDb) -> bool {
        matches!(self.data(db).kind, TypeKind::Contract(_))
    }

    pub fn array_elem_size(self, db: &dyn MirDb, slot_size: usize) -> usize {
        let data = self.data(db);
        if let TypeKind::Array(def) = &data.kind {
            array_elem_size_imp(db, def, slot_size)
        } else {
            panic!("expected `Array` type; but got {:?}", data.as_ref())
        }
    }

    pub fn print<W: fmt::Write>(&self, db: &dyn MirDb, w: &mut W) -> fmt::Result {
        match &self.data(db).kind {
            TypeKind::I8 => write!(w, "i8"),
            TypeKind::I16 => write!(w, "i16"),
            TypeKind::I32 => write!(w, "i32"),
            TypeKind::I64 => write!(w, "i64"),
            TypeKind::I128 => write!(w, "i128"),
            TypeKind::I256 => write!(w, "i256"),
            TypeKind::U8 => write!(w, "u8"),
            TypeKind::U16 => write!(w, "u16"),
            TypeKind::U32 => write!(w, "u32"),
            TypeKind::U64 => write!(w, "u64"),
            TypeKind::U128 => write!(w, "u128"),
            TypeKind::U256 => write!(w, "u256"),
            TypeKind::Bool => write!(w, "bool"),
            TypeKind::Address => write!(w, "address"),
            TypeKind::Unit => write!(w, "()"),
            TypeKind::String(size) => write!(w, "Str<{size}>"),
            TypeKind::Array(ArrayDef { elem_ty, len }) => {
                write!(w, "[")?;
                elem_ty.print(db, w)?;
                write!(w, "; {len}]")
            }
            TypeKind::Tuple(TupleDef { items }) => {
                write!(w, "(")?;
                if items.is_empty() {
                    return write!(w, ")");
                }

                let len = items.len();
                for item in &items[0..len - 1] {
                    item.print(db, w)?;
                    write!(w, ", ")?;
                }
                items.last().unwrap().print(db, w)?;
                write!(w, ")")
            }
            TypeKind::Struct(def) => {
                write!(w, "{}", def.name)
            }
            TypeKind::Enum(def) => {
                write!(w, "{}", def.name)
            }
            TypeKind::Contract(def) => {
                write!(w, "{}", def.name)
            }
            TypeKind::Map(def) => {
                write!(w, "Map<")?;
                def.key_ty.print(db, w)?;
                write!(w, ",")?;
                def.value_ty.print(db, w)?;
                write!(w, ">")
            }
            TypeKind::MPtr(inner) => {
                write!(w, "*@m ")?;
                inner.print(db, w)
            }
            TypeKind::SPtr(inner) => {
                write!(w, "*@s ")?;
                inner.print(db, w)
            }
        }
    }

    pub fn as_string(&self, db: &dyn MirDb) -> String {
        let mut s = String::new();
        self.print(db, &mut s).unwrap();
        s
    }
}

fn array_elem_size_imp(db: &dyn MirDb, arr: &ArrayDef, slot_size: usize) -> usize {
    let elem_ty = arr.elem_ty;
    let elem = elem_ty.size_of(db, slot_size);
    let align = if elem_ty.is_address(db) {
        slot_size
    } else {
        elem_ty.align_of(db, slot_size)
    };
    round_up(elem, align)
}

fn expect_projection_index(value: &Value) -> usize {
    match value {
        Value::Immediate { imm, .. } => imm.to_usize().unwrap(),
        _ => panic!("given `value` is not an immediate"),
    }
}

fn round_up(value: usize, slot_size: usize) -> usize {
    value.div_ceil(slot_size) * slot_size
}

#[cfg(test)]
mod tests {
    use fe_analyzer::namespace::items::ModuleId;
    use fe_common::Span;

    use super::*;
    use crate::{
        db::{MirDb, NewDb},
        ir::types::StructDef,
    };

    #[test]
    fn test_primitive_type_info() {
        let db = NewDb::default();
        let i8 = db.mir_intern_type(Type::new(TypeKind::I8, None).into());
        let bool = db.mir_intern_type(Type::new(TypeKind::Bool, None).into());

        debug_assert_eq!(i8.size_of(&db, 1), 1);
        debug_assert_eq!(i8.size_of(&db, 32), 1);
        debug_assert_eq!(i8.align_of(&db, 1), 1);
        debug_assert_eq!(i8.align_of(&db, 32), 1);
        debug_assert_eq!(bool.size_of(&db, 1), 1);
        debug_assert_eq!(bool.size_of(&db, 32), 1);
        debug_assert_eq!(i8.align_of(&db, 32), 1);
        debug_assert_eq!(i8.align_of(&db, 32), 1);

        let u32 = db.mir_intern_type(Type::new(TypeKind::U32, None).into());
        debug_assert_eq!(u32.size_of(&db, 1), 4);
        debug_assert_eq!(u32.size_of(&db, 32), 4);
        debug_assert_eq!(u32.align_of(&db, 32), 1);

        let address = db.mir_intern_type(Type::new(TypeKind::Address, None).into());
        debug_assert_eq!(address.size_of(&db, 1), 20);
        debug_assert_eq!(address.size_of(&db, 32), 20);
        debug_assert_eq!(address.align_of(&db, 32), 1);
    }

    #[test]
    fn test_primitive_elem_array_type_info() {
        let db = NewDb::default();
        let i32 = db.mir_intern_type(Type::new(TypeKind::I32, None).into());

        let array_len = 10;
        let array_def = ArrayDef {
            elem_ty: i32,
            len: array_len,
        };
        let array = db.mir_intern_type(Type::new(TypeKind::Array(array_def), None).into());

        let elem_size = array.array_elem_size(&db, 1);
        debug_assert_eq!(elem_size, 4);
        debug_assert_eq!(array.array_elem_size(&db, 32), elem_size);

        debug_assert_eq!(array.size_of(&db, 1), elem_size * array_len);
        debug_assert_eq!(array.size_of(&db, 32), elem_size * array_len);
        debug_assert_eq!(array.align_of(&db, 1), 1);
        debug_assert_eq!(array.align_of(&db, 32), 32);

        debug_assert_eq!(array.aggregate_elem_offset(&db, 3, 32), elem_size * 3);
        debug_assert_eq!(array.aggregate_elem_offset(&db, 9, 1), elem_size * 9);
    }

    #[test]
    fn test_aggregate_elem_array_type_info() {
        let db = NewDb::default();
        let i8 = db.mir_intern_type(Type::new(TypeKind::I8, None).into());
        let i64 = db.mir_intern_type(Type::new(TypeKind::I64, None).into());
        let i128 = db.mir_intern_type(Type::new(TypeKind::I128, None).into());

        let fields = vec![
            ("".into(), i64),
            ("".into(), i64),
            ("".into(), i8),
            ("".into(), i128),
            ("".into(), i8),
        ];

        let struct_def = StructDef {
            name: "".into(),
            fields,
            span: Span::dummy(),
            module_id: ModuleId::from_raw_internal(0),
        };
        let aggregate = db.mir_intern_type(Type::new(TypeKind::Struct(struct_def), None).into());

        let array_len = 10;
        let array_def = ArrayDef {
            elem_ty: aggregate,
            len: array_len,
        };
        let array = db.mir_intern_type(Type::new(TypeKind::Array(array_def), None).into());

        debug_assert_eq!(array.array_elem_size(&db, 1), 34);
        debug_assert_eq!(array.array_elem_size(&db, 32), 64);

        debug_assert_eq!(array.size_of(&db, 1), 34 * array_len);
        debug_assert_eq!(array.size_of(&db, 32), 64 * array_len);

        debug_assert_eq!(array.align_of(&db, 1), 1);
        debug_assert_eq!(array.align_of(&db, 32), 32);

        debug_assert_eq!(array.aggregate_elem_offset(&db, 3, 1), 102);
        debug_assert_eq!(array.aggregate_elem_offset(&db, 3, 32), 192);
    }

    #[test]
    fn test_primitive_elem_aggregate_type_info() {
        let db = NewDb::default();
        let i8 = db.mir_intern_type(Type::new(TypeKind::I8, None).into());
        let i64 = db.mir_intern_type(Type::new(TypeKind::I64, None).into());
        let i128 = db.mir_intern_type(Type::new(TypeKind::I128, None).into());

        let fields = vec![
            ("".into(), i64),
            ("".into(), i64),
            ("".into(), i8),
            ("".into(), i128),
            ("".into(), i8),
        ];

        let struct_def = StructDef {
            name: "".into(),
            fields,
            span: Span::dummy(),
            module_id: ModuleId::from_raw_internal(0),
        };
        let aggregate = db.mir_intern_type(Type::new(TypeKind::Struct(struct_def), None).into());

        debug_assert_eq!(aggregate.size_of(&db, 1), 34);
        debug_assert_eq!(aggregate.size_of(&db, 32), 49);

        debug_assert_eq!(aggregate.align_of(&db, 1), 1);
        debug_assert_eq!(aggregate.align_of(&db, 32), 32);

        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 0, 1), 0);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 0, 32), 0);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 3, 1), 17);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 3, 32), 32);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 4, 1), 33);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 4, 32), 48);
    }

    #[test]
    fn test_aggregate_elem_aggregate_type_info() {
        let db = NewDb::default();
        let i8 = db.mir_intern_type(Type::new(TypeKind::I8, None).into());
        let i64 = db.mir_intern_type(Type::new(TypeKind::I64, None).into());
        let i128 = db.mir_intern_type(Type::new(TypeKind::I128, None).into());

        let fields_inner = vec![
            ("".into(), i64),
            ("".into(), i64),
            ("".into(), i8),
            ("".into(), i128),
            ("".into(), i8),
        ];

        let struct_def_inner = StructDef {
            name: "".into(),
            fields: fields_inner,
            span: Span::dummy(),
            module_id: ModuleId::from_raw_internal(0),
        };
        let aggregate_inner =
            db.mir_intern_type(Type::new(TypeKind::Struct(struct_def_inner), None).into());

        let fields = vec![("".into(), i8), ("".into(), aggregate_inner)];
        let struct_def = StructDef {
            name: "".into(),
            fields,
            span: Span::dummy(),
            module_id: ModuleId::from_raw_internal(0),
        };
        let aggregate = db.mir_intern_type(Type::new(TypeKind::Struct(struct_def), None).into());

        debug_assert_eq!(aggregate.size_of(&db, 1), 35);
        debug_assert_eq!(aggregate.size_of(&db, 32), 81);

        debug_assert_eq!(aggregate.align_of(&db, 1), 1);
        debug_assert_eq!(aggregate.align_of(&db, 32), 32);

        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 0, 1), 0);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 0, 32), 0);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 1, 1), 1);
        debug_assert_eq!(aggregate.aggregate_elem_offset(&db, 1, 32), 32);
    }
}
