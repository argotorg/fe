use crate::analysis::{
    HirAnalysisDb,
    ty::{
        adt_def::{AdtDef, AdtField, AdtRef},
        ty_lower::{GenericParamTypeSet, collect_generic_params},
    },
};
use crate::core::hir_def::{
    Contract, Enum, FieldDefListId, Struct, VariantDefListId, VariantKind, scope_graph::ScopeId,
};

/// Lower HIR ADT definition (`struct/enum/contract`) to [`AdtDef`].
#[salsa::tracked]
pub(crate) fn lower_adt<'db>(db: &'db dyn HirAnalysisDb, adt: AdtRef<'db>) -> AdtDef<'db> {
    let scope = adt.scope();

    let (params, variants) = match adt {
        AdtRef::Contract(c) => (
            GenericParamTypeSet::empty(db, scope),
            vec![collect_field_types(db, scope, c.fields(db))],
        ),
        AdtRef::Struct(s) => (
            collect_generic_params(db, s.into()),
            vec![collect_field_types(db, scope, s.fields(db))],
        ),
        AdtRef::Enum(e) => (
            collect_generic_params(db, e.into()),
            collect_enum_variant_types(db, scope, e.variants_list(db)),
        ),
    };

    AdtDef::new(db, adt, params, variants)
}

fn collect_field_types<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    fields: FieldDefListId<'db>,
) -> AdtField<'db> {
    let tys = fields.data(db).iter().map(|field| field.type_ref).collect();
    AdtField::new(tys, scope)
}

fn collect_enum_variant_types<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    variants: VariantDefListId<'db>,
) -> Vec<AdtField<'db>> {
    variants
        .data(db)
        .iter()
        .map(|variant| {
            let tys = match variant.kind {
                VariantKind::Tuple(tuple_id) => tuple_id.data(db).clone(),
                VariantKind::Record(fields) => {
                    fields.data(db).iter().map(|field| field.type_ref).collect()
                }
                VariantKind::Unit => vec![],
            };
            AdtField::new(tys, scope)
        })
        .collect()
}
