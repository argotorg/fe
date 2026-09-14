use parser::ast::{self, prelude::*};
use salsa::Accumulator as _;

use super::{
    AbiFieldContext, AbiFieldDiagnostic, FileLowerCtxt,
    attr::{has_named_attr, lower_attrs_without_named, named_attr_specs},
    event::create_sol_signature_const,
    hir_builder::HirBuilder,
    msg::{lower_abi_record_impl, lower_abi_size_impl, lower_sol_encode_impl},
};
use crate::{
    hir_def::{
        AttrListId, FieldDef, FieldDefListId, GenericParamListId, IdentId, Partial, PathId, Struct,
        TrackedItemVariant, TraitRefId, TypeId, TypeKind,
    },
    span::ErrorDesugared,
};

/// Error-related diagnostics accumulated during `#[error]` lowering / validation.
#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErrorDiagnostic {
    pub kind: ErrorDiagnosticKind,
    pub file: common::file::File,
    pub primary_range: parser::TextRange,
    pub struct_name: Option<String>,
    pub field_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ErrorDiagnosticKind {
    GenericErrorStruct,
    EventErrorAttrConflict,
}

pub(super) fn is_error_struct(ast: &ast::Struct) -> bool {
    has_named_attr(ast.attr_list(), "error")
}

pub(super) fn report_event_error_attr_conflict<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Struct,
) {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);
    let struct_name = ast.name().map(|n| n.text().to_string());

    for attr in named_attr_specs(ast.attr_list(), "error") {
        ErrorDiagnostic {
            kind: ErrorDiagnosticKind::EventErrorAttrConflict,
            file,
            primary_range: attr.range,
            struct_name: struct_name.clone(),
            field_name: None,
        }
        .accumulate(db);
    }
}

pub(super) fn lower_error_struct<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: ast::Struct,
) -> Struct<'db> {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);

    let error_desugared = ErrorDesugared {
        error_struct: parser::ast::AstPtr::new(&ast),
    };
    let mut builder = HirBuilder::new(ctxt, error_desugared.clone());

    let struct_name_token = ast.name();
    let struct_name = struct_name_token.as_ref().map(|n| n.text().to_string());

    // Strip #[error] attribute.
    let attributes = lower_attrs_without_named(builder.ctxt(), ast.attr_list(), "error");

    let vis = super::lower_visibility(&ast);
    let generic_params = GenericParamListId::lower_ast_opt(builder.ctxt(), ast.generic_params());
    if !generic_params.data(db).is_empty() {
        let range = ast
            .generic_params()
            .map(|g| g.syntax().text_range())
            .unwrap_or_else(|| ast.syntax().text_range());
        ErrorDiagnostic {
            kind: ErrorDiagnosticKind::GenericErrorStruct,
            file,
            primary_range: range,
            struct_name: struct_name.clone(),
            field_name: None,
        }
        .accumulate(db);
    }

    let where_clause =
        crate::hir_def::WhereClauseId::lower_ast_opt(builder.ctxt(), ast.where_clause());

    let parsed_fields = parse_error_fields(builder.ctxt(), &ast, struct_name.as_deref());

    let fields_hir = FieldDefListId::new(db, parsed_fields.hir_fields);

    let name_ident = IdentId::lower_token_partial(builder.ctxt(), struct_name_token);

    let struct_ = builder.struct_item(
        name_ident,
        attributes,
        vis,
        generic_params,
        where_clause,
        fields_hir,
    );

    // Generate trait impls only when the struct is well-formed
    if !parsed_fields.is_valid {
        return struct_;
    }
    if !generic_params.data(db).is_empty() {
        return struct_;
    }

    let Some(struct_name_str) = struct_name.clone() else {
        return struct_;
    };

    let Some(struct_name_ident) = name_ident.to_opt() else {
        return struct_;
    };
    let self_ty = TypeId::new(
        db,
        TypeKind::Path(Partial::Present(PathId::from_ident(db, struct_name_ident))),
    );

    // Generate impl ErrorVariant<Sol>
    let trait_ref = TraitRefId::new(
        db,
        Partial::Present(
            PathId::from_ident(db, builder.roots().core)
                .push_str(db, "error")
                .push_str_args(db, "ErrorVariant", builder.sol_args()),
        ),
    );

    let field_specs = parsed_fields.field_specs.clone();
    let field_types: Vec<_> = field_specs.iter().map(|(_, ty)| *ty).collect();

    lower_abi_record_impl(&mut builder, self_ty, &field_specs);

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

    // Generate impl AbiSize
    lower_abi_size_impl(&mut builder, self_ty, &field_specs);

    // Generate impl Encode<Sol>
    lower_sol_encode_impl(&mut builder, self_ty, &field_specs);

    struct_
}

struct ParsedErrorFields<'db> {
    hir_fields: Vec<FieldDef<'db>>,
    field_specs: Vec<(IdentId<'db>, TypeId<'db>)>,
    is_valid: bool,
}

fn parse_error_fields<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Struct,
    struct_name: Option<&str>,
) -> ParsedErrorFields<'db> {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);

    let mut hir_fields = Vec::new();
    let mut field_specs = Vec::new();
    let mut is_valid = true;

    let Some(fields) = ast.fields() else {
        return ParsedErrorFields {
            hir_fields,
            field_specs,
            is_valid,
        };
    };

    for field in fields {
        super::item::report_unsupported_field_mut(ctxt, &field, "error field");
        let attrs = AttrListId::lower_ast_opt(ctxt, field.attr_list());
        let name_tok = field.name();
        let name_ident = IdentId::lower_token_partial(ctxt, name_tok.clone());
        let ty_ref = TypeId::lower_ast_partial(ctxt, field.ty());
        let vis = super::lower_field_visibility(&field);

        hir_fields.push(FieldDef::new(attrs, name_ident, ty_ref, vis, false, false));

        let (Some(name_ident), Some(ty)) = (name_ident.to_opt(), ty_ref.to_opt()) else {
            is_valid = false;
            continue;
        };

        let TypeKind::Path(Partial::Present(_)) = ty.data(db) else {
            AbiFieldDiagnostic {
                context: AbiFieldContext::Error,
                ty: ty.pretty_print(db),
                file,
                primary_range: field
                    .ty()
                    .map(|t| t.syntax().text_range())
                    .unwrap_or_else(|| field.syntax().text_range()),
                struct_name: struct_name.map(|s| s.to_string()),
                field_name: name_tok.map(|n| n.text().to_string()),
            }
            .accumulate(db);
            is_valid = false;
            continue;
        };

        field_specs.push((name_ident, ty));
    }

    ParsedErrorFields {
        hir_fields,
        field_specs,
        is_valid,
    }
}
