use crate::{
    db::MirDb,
    ir::{
        types::{ArrayDef, EnumDef, EnumVariant, MapDef, StructDef, TupleDef},
        Type, TypeId, TypeKind,
    },
};

use fe_analyzer::namespace::{
    items as analyzer_items,
    types::{self as analyzer_types, TraitOrType},
};

pub fn lower_type(db: &dyn MirDb, analyzer_ty: analyzer_types::TypeId) -> TypeId {
    let ty_kind = match analyzer_ty.typ(db.upcast()) {
        analyzer_types::Type::SPtr(inner) => TypeKind::SPtr(lower_type(db, inner)),

        // NOTE: this results in unexpected MIR TypeId inequalities
        //  (when different analyzer types map to the same MIR type).
        //  We could (should?) remove .analyzer_ty from Type.
        analyzer_types::Type::Mut(inner) => match inner.typ(db.upcast()) {
            analyzer_types::Type::SPtr(t) => TypeKind::SPtr(lower_type(db, t)),
            analyzer_types::Type::Base(t) => lower_base(t),
            analyzer_types::Type::Contract(_) => TypeKind::Address,
            _ => TypeKind::MPtr(lower_type(db, inner)),
        },
        analyzer_types::Type::SelfType(inner) => match inner {
            TraitOrType::TypeId(id) => return lower_type(db, id),
            TraitOrType::TraitId(_) => panic!("traits aren't lowered"),
        },
        analyzer_types::Type::Base(base) => lower_base(base),
        analyzer_types::Type::Array(arr) => lower_array(db, &arr),
        analyzer_types::Type::Map(map) => lower_map(db, &map),
        analyzer_types::Type::Tuple(tup) => lower_tuple(db, &tup),
        analyzer_types::Type::String(string) => TypeKind::String(string.max_size),
        analyzer_types::Type::Contract(_) => TypeKind::Address,
        analyzer_types::Type::SelfContract(contract) => lower_contract(db, contract),
        analyzer_types::Type::Struct(struct_) => lower_struct(db, struct_),
        analyzer_types::Type::Enum(enum_) => lower_enum(db, enum_),
        analyzer_types::Type::Generic(_) => {
            panic!("should be lowered in `lower_analyzer_type`")
        }
    };

    intern_type(db, ty_kind, Some(analyzer_ty.deref(db.upcast())))
}

fn lower_base(base: analyzer_types::Base) -> TypeKind {
    use analyzer_types::{Base, Integer};

    match base {
        Base::Numeric(int_ty) => match int_ty {
            Integer::I8 => TypeKind::I8,
            Integer::I16 => TypeKind::I16,
            Integer::I32 => TypeKind::I32,
            Integer::I64 => TypeKind::I64,
            Integer::I128 => TypeKind::I128,
            Integer::I256 => TypeKind::I256,
            Integer::U8 => TypeKind::U8,
            Integer::U16 => TypeKind::U16,
            Integer::U32 => TypeKind::U32,
            Integer::U64 => TypeKind::U64,
            Integer::U128 => TypeKind::U128,
            Integer::U256 => TypeKind::U256,
        },

        Base::Bool => TypeKind::Bool,
        Base::Address => TypeKind::Address,
        Base::Unit => TypeKind::Unit,
    }
}

fn lower_array(db: &dyn MirDb, arr: &analyzer_types::Array) -> TypeKind {
    let len = arr.size;
    let elem_ty = db.mir_lowered_type(arr.inner);

    let def = ArrayDef { elem_ty, len };
    TypeKind::Array(def)
}

fn lower_map(db: &dyn MirDb, map: &analyzer_types::Map) -> TypeKind {
    let key_ty = db.mir_lowered_type(map.key);
    let value_ty = db.mir_lowered_type(map.value);

    let def = MapDef { key_ty, value_ty };
    TypeKind::Map(def)
}

fn lower_tuple(db: &dyn MirDb, tup: &analyzer_types::Tuple) -> TypeKind {
    let items = tup
        .items
        .iter()
        .map(|item| db.mir_lowered_type(*item))
        .collect();

    let def = TupleDef { items };
    TypeKind::Tuple(def)
}

fn lower_contract(db: &dyn MirDb, contract: analyzer_items::ContractId) -> TypeKind {
    let name = contract.name(db.upcast());

    // Note: contract field types are wrapped in SPtr in TypeId::projection_ty
    let fields = contract
        .fields(db.upcast())
        .iter()
        .map(|(fname, fid)| {
            let analyzer_type = fid.typ(db.upcast()).unwrap();
            let ty = db.mir_lowered_type(analyzer_type);
            (fname.clone(), ty)
        })
        .collect();

    // Obtain span.
    let span = contract.span(db.upcast());

    let module_id = contract.module(db.upcast());

    let def = StructDef {
        name,
        fields,
        span,
        module_id,
    };
    TypeKind::Contract(def)
}

fn lower_struct(db: &dyn MirDb, id: analyzer_items::StructId) -> TypeKind {
    let name = id.name(db.upcast());

    // Lower struct fields.
    let fields = id
        .fields(db.upcast())
        .iter()
        .map(|(fname, fid)| {
            let analyzer_types = fid.typ(db.upcast()).unwrap();
            let ty = db.mir_lowered_type(analyzer_types);
            (fname.clone(), ty)
        })
        .collect();

    // obtain span.
    let span = id.span(db.upcast());

    let module_id = id.module(db.upcast());

    let def = StructDef {
        name,
        fields,
        span,
        module_id,
    };
    TypeKind::Struct(def)
}

fn lower_enum(db: &dyn MirDb, id: analyzer_items::EnumId) -> TypeKind {
    let analyzer_variants = id.variants(db.upcast());
    let mut variants = Vec::with_capacity(analyzer_variants.len());
    for variant in analyzer_variants.values() {
        let variant_ty = match variant.kind(db.upcast()).unwrap() {
            analyzer_items::EnumVariantKind::Tuple(elts) => {
                let tuple_ty = analyzer_types::TypeId::tuple(db.upcast(), &elts);
                db.mir_lowered_type(tuple_ty)
            }
            analyzer_items::EnumVariantKind::Unit => {
                let unit_ty = analyzer_types::TypeId::unit(db.upcast());
                db.mir_lowered_type(unit_ty)
            }
        };

        variants.push(EnumVariant {
            name: variant.name(db.upcast()),
            span: variant.span(db.upcast()),
            ty: variant_ty,
        });
    }

    let def = EnumDef {
        name: id.name(db.upcast()),
        span: id.span(db.upcast()),
        variants,
        module_id: id.module(db.upcast()),
    };

    TypeKind::Enum(def)
}

fn intern_type(
    db: &dyn MirDb,
    ty_kind: TypeKind,
    analyzer_type: Option<analyzer_types::TypeId>,
) -> TypeId {
    db.mir_intern_type(Type::new(ty_kind, analyzer_type).into())
}
