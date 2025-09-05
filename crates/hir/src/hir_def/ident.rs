use crate::HirDb;

#[salsa::interned]
#[derive(Debug)]
pub struct IdentId<'db> {
    #[return_ref]
    pub data: String,
}

macro_rules! define_keywords {
    ($(($name: ident, $kw_str:literal),)*) => {
        impl<'db> IdentId<'db> {
        $(
            paste::paste! {
                pub fn [<make_ $name>](db: &'db dyn HirDb) -> Self {
                    Self::new(db, $kw_str.to_string())
                }

                pub fn [<is_ $name>](self, db: &dyn HirDb) -> bool {
                    self.data(db) == $kw_str
                }
            }
        )+
        }
    };
}

define_keywords! {
    (ingot, "ingot"),
    (core, "core"),
    (super, "super"),
    (self, "self"),
    (self_ty, "Self"),
    (bool, "bool"),
    (u8, "u8"),
    (u16, "u16"),
    (u32, "u32"),
    (u64, "u64"),
    (u128, "u128"),
    (u256, "u256"),
    (usize, "usize"),
    (i8, "i8"),
    (i16, "i16"),
    (i32, "i32"),
    (i64, "i64"),
    (i128, "i128"),
    (i256, "i256"),
    (isize, "isize"),
}
