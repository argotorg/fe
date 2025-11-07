use fe_analyzer::namespace::{items as analyzer_items, types as analyzer_types};
use fe_common::{impl_intern_key, Span};
use smol_str::SmolStr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TypeKind,
    pub analyzer_ty: Option<analyzer_types::TypeId>,
}

impl Type {
    pub fn new(kind: TypeKind, analyzer_ty: Option<analyzer_types::TypeId>) -> Self {
        Self { kind, analyzer_ty }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    Bool,
    Address,
    Unit,
    Array(ArrayDef),
    // TODO: we should consider whether we really need `String` type.
    String(usize),
    Tuple(TupleDef),
    Struct(StructDef),
    Enum(EnumDef),
    Contract(StructDef),
    Map(MapDef),
    MPtr(TypeId),
    SPtr(TypeId),
}

/// An interned Id for [`ArrayDef`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);
impl_intern_key!(TypeId);

/// A static array type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayDef {
    pub elem_ty: TypeId,
    pub len: usize,
}

/// A tuple type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleDef {
    pub items: Vec<TypeId>,
}

/// A user defined struct type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub name: SmolStr,
    pub fields: Vec<(SmolStr, TypeId)>,
    pub span: Span,
    pub module_id: analyzer_items::ModuleId,
}

/// A user defined struct type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDef {
    pub name: SmolStr,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
    pub module_id: analyzer_items::ModuleId,
}

impl EnumDef {
    pub fn tag_type(&self) -> TypeKind {
        let variant_num = self.variants.len() as u64;
        if variant_num <= u8::MAX as u64 {
            TypeKind::U8
        } else if variant_num <= u16::MAX as u64 {
            TypeKind::U16
        } else if variant_num <= u32::MAX as u64 {
            TypeKind::U32
        } else {
            TypeKind::U64
        }
    }
}

/// A user defined struct type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub name: SmolStr,
    pub span: Span,
    pub ty: TypeId,
}

/// A user defined struct type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EventDef {
    pub name: SmolStr,
    pub fields: Vec<(SmolStr, TypeId, bool)>,
    pub span: Span,
    pub module_id: analyzer_items::ModuleId,
}

/// A map type definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MapDef {
    pub key_ty: TypeId,
    pub value_ty: TypeId,
}
