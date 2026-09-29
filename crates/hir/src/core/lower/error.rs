use parser::ast::{self, prelude::*};
use salsa::Accumulator as _;

use super::{
    AbiFieldContext, FileLowerCtxt,
    abi_field::{
        AbiRecordDiagnostic, AbiRecordDiagnosticKind, LoweredAbiRecord, check_abi_record_field_ty,
        lower_abi_record_struct,
    },
    attr::{has_named_attr, named_attr_specs},
    event::create_sol_signature_const,
    hir_builder::HirBuilder,
    msg::{lower_abi_size_impl, lower_sol_encode_impl},
};
use crate::{
    hir_def::{
        AbiRecordKind, AttrListId, FieldDef, IdentId, Partial, PathId, Struct, TrackedItemVariant,
        TraitRefId, TypeId, TypeKind,
    },
    span::ErrorDesugared,
};

pub(super) fn is_error_struct(ast: &ast::Struct) -> bool {
    has_named_attr(ast.attr_list(), "error")
}

pub(super) fn report_event_error_attr_conflict<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Struct,
) {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);

    for attr in named_attr_specs(ast.attr_list(), "error") {
        AbiRecordDiagnostic {
            kind: AbiRecordDiagnosticKind::EventErrorAttrConflict,
            file,
            primary_range: attr.range,
        }
        .accumulate(db);
    }
}

pub(super) fn lower_error_struct<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: ast::Struct,
) -> Struct<'db> {
    let db = ctxt.db();

    let error_desugared = ErrorDesugared {
        error_struct: parser::ast::AstPtr::new(&ast),
    };
    let mut builder = HirBuilder::new(ctxt, error_desugared.clone());

    let LoweredAbiRecord {
        struct_,
        fields: field_specs,
        generated,
    } = lower_abi_record_struct(&mut builder, &ast, AbiFieldContext::Error, |ctxt| {
        parse_error_fields(ctxt, &ast)
    });
    let Some((self_ty, struct_name_str)) = generated else {
        return struct_;
    };

    // Generate impl ErrorVariant<Sol>
    let trait_ref = TraitRefId::new(
        db,
        Partial::Present(
            PathId::from_ident(db, builder.roots().core)
                .push_str(db, "error")
                .push_str_args(db, "ErrorVariant", builder.sol_args()),
        ),
    );

    let field_types: Vec<_> = field_specs.iter().map(|(_, ty)| *ty).collect();

    let impl_trait_idx = builder.ctxt().next_impl_trait_idx();
    builder.with_item_scope(
        TrackedItemVariant::ImplTrait(impl_trait_idx),
        move |builder, id| {
            let sol_path = PathId::from_ident(db, builder.roots().std)
                .push_str(db, "abi")
                .push_str(db, "sol")
                .push_str(db, "sol");
            let u32_ty = TypeId::new(
                db,
                TypeKind::Path(Partial::Present(PathId::from_ident(
                    db,
                    IdentId::new(db, "u32".to_string()),
                ))),
            );
            let selector_const = create_sol_signature_const(
                builder.ctxt(),
                error_desugared.clone(),
                "SELECTOR",
                u32_ty,
                sol_path,
                &struct_name_str,
                &field_types,
            );
            builder.new_impl_trait(
                id,
                Partial::Present(trait_ref),
                Partial::Present(self_ty),
                vec![],
                vec![selector_const],
                builder.origin(),
            )
        },
    );

    // Generate impl AbiRecord and impl AbiSize
    lower_abi_size_impl(&mut builder, self_ty, &field_specs, AbiRecordKind::Error);

    // Generate impl Encode<Sol>
    lower_sol_encode_impl(&mut builder, self_ty, &field_specs);

    struct_
}

/// Lowers the fields of an `#[error]` struct, returning the HIR fields,
/// whether they are valid, and each valid field's name and type.
fn parse_error_fields<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Struct,
) -> (Vec<FieldDef<'db>>, bool, Vec<(IdentId<'db>, TypeId<'db>)>) {
    let mut hir_fields = Vec::new();
    let mut field_specs = Vec::new();
    let mut is_valid = true;

    let Some(fields) = ast.fields() else {
        return (hir_fields, is_valid, field_specs);
    };

    for field in fields {
        super::item::report_unsupported_field_mut(ctxt, &field, "error field");
        let attrs = AttrListId::lower_ast_opt(ctxt, field.attr_list());
        let name_ident = IdentId::lower_token_partial(ctxt, field.name());
        let ty_ref = TypeId::lower_ast_partial(ctxt, field.ty());
        let vis = super::lower_field_visibility(&field);

        hir_fields.push(FieldDef::new(attrs, name_ident, ty_ref, vis, false, false));

        let (Some(name_ident), Some(ty)) = (name_ident.to_opt(), ty_ref.to_opt()) else {
            is_valid = false;
            continue;
        };
        if !check_abi_record_field_ty(ctxt, &field, ty, AbiFieldContext::Error) {
            is_valid = false;
            continue;
        }

        field_specs.push((name_ident, ty));
    }

    (hir_fields, is_valid, field_specs)
}
