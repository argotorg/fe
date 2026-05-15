use parser::ast::{self, prelude::*};
use salsa::Accumulator as _;

use super::{
    FileLowerCtxt,
    attr::{has_named_attr, lower_attrs_without_named, named_attr_specs},
    hir_builder::HirBuilder,
    msg::{
        build_head_size_body_expr, create_direct_encode_assoc_const, create_head_size_assoc_const,
        create_is_dynamic_assoc_const, create_payload_size_func,
    },
};
use crate::{
    hir_def::{
        ArithBinOp, AttrListId, BinOp, Expr, FieldDef, FieldDefListId, FieldIndex, FuncModifiers,
        GenericParamListId, IdentId, Partial, Pat, PathId, Stmt, Struct, TrackedItemVariant,
        TypeId, TypeKind, Visibility,
    },
    span::AbiStructDesugared,
};

/// Diagnostics accumulated during `#[abi]` struct lowering / validation.
#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AbiStructError {
    pub kind: AbiStructErrorKind,
    pub file: common::file::File,
    pub primary_range: parser::TextRange,
    pub struct_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AbiStructErrorKind {
    AbiAttrOnNonStruct { item_kind: &'static str },
    InvalidAbiAttrForm,
    GenericAbiStruct,
}

pub(super) fn is_abi_struct(ast: &ast::Struct) -> bool {
    has_named_attr(ast.attr_list(), "abi")
}

pub(super) fn report_abi_attr_on_non_struct_item<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    item_kind: &'static str,
) {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);

    for attr in named_attr_specs(attrs, "abi") {
        AbiStructError {
            kind: AbiStructErrorKind::AbiAttrOnNonStruct { item_kind },
            file,
            primary_range: attr.range,
            struct_name: None,
        }
        .accumulate(db);
    }
}

pub(super) fn lower_abi_struct<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: ast::Struct,
) -> Struct<'db> {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);

    let abi_desugared = AbiStructDesugared {
        abi_struct: parser::ast::AstPtr::new(&ast),
    };
    let mut builder = HirBuilder::new(ctxt, abi_desugared);

    let struct_name_token = ast.name();
    let struct_name = struct_name_token.as_ref().map(|n| n.text().to_string());

    // Strip #[abi] attribute, validate it is bare
    let stripped_abi_attr = lower_attrs_without_named(builder.ctxt(), ast.attr_list(), "abi");
    let attributes = stripped_abi_attr.retained;
    if let Some(attr) = stripped_abi_attr
        .removed
        .iter()
        .find(|attr| !attr.is_bare())
    {
        AbiStructError {
            kind: AbiStructErrorKind::InvalidAbiAttrForm,
            file,
            primary_range: attr.range,
            struct_name: struct_name.clone(),
        }
        .accumulate(db);
    }

    let vis = super::lower_visibility(&ast);
    let generic_params = GenericParamListId::lower_ast_opt(builder.ctxt(), ast.generic_params());
    if !generic_params.data(db).is_empty() {
        let range = ast
            .generic_params()
            .map(|g| g.syntax().text_range())
            .unwrap_or_else(|| ast.syntax().text_range());
        AbiStructError {
            kind: AbiStructErrorKind::GenericAbiStruct,
            file,
            primary_range: range,
            struct_name: struct_name.clone(),
        }
        .accumulate(db);
    }

    let where_clause =
        crate::hir_def::WhereClauseId::lower_ast_opt(builder.ctxt(), ast.where_clause());

    let parsed_fields = parse_abi_struct_fields(builder.ctxt(), &ast);

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

    // Only generate trait impls when the struct is well-formed
    if !parsed_fields.is_valid {
        return struct_;
    }
    if !generic_params.data(db).is_empty() {
        return struct_;
    }

    let Some(struct_name_ident) = name_ident.to_opt() else {
        return struct_;
    };
    let self_ty = TypeId::new(
        db,
        TypeKind::Path(Partial::Present(PathId::from_ident(db, struct_name_ident))),
    );

    let field_specs = parsed_fields.field_specs.clone();

    // Generate impl AbiSize
    lower_abi_struct_abi_size_impl(&mut builder, self_ty, &field_specs);

    // Generate impl Encode<Sol>
    lower_abi_struct_encode_impl(&mut builder, self_ty, &field_specs);

    // Generate impl Decode<Sol>
    lower_abi_struct_decode_impl(&mut builder, self_ty, &field_specs);

    struct_
}

struct ParsedAbiStructFields<'db> {
    hir_fields: Vec<FieldDef<'db>>,
    field_specs: Vec<(IdentId<'db>, TypeId<'db>)>,
    is_valid: bool,
}

fn parse_abi_struct_fields<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Struct,
) -> ParsedAbiStructFields<'db> {
    let mut hir_fields = Vec::new();
    let mut field_specs = Vec::new();
    let mut is_valid = true;

    let Some(fields) = ast.fields() else {
        return ParsedAbiStructFields {
            hir_fields,
            field_specs,
            is_valid,
        };
    };

    for field in fields {
        let attrs = AttrListId::lower_ast_opt(ctxt, field.attr_list());
        let name_ident = IdentId::lower_token_partial(ctxt, field.name());
        let ty_ref = TypeId::lower_ast_partial(ctxt, field.ty());
        let vis = super::lower_field_visibility(&field);

        hir_fields.push(FieldDef::new(attrs, name_ident, ty_ref, vis));

        let (Some(name_ident), Some(ty)) = (name_ident.to_opt(), ty_ref.to_opt()) else {
            is_valid = false;
            continue;
        };

        field_specs.push((name_ident, ty));
    }

    ParsedAbiStructFields {
        hir_fields,
        field_specs,
        is_valid,
    }
}

fn lower_abi_struct_abi_size_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, AbiStructDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &[(IdentId<'db>, TypeId<'db>)],
) {
    let db = builder.db();
    let roots = builder.roots();
    let trait_path = PathId::from_ident(db, roots.core)
        .push_str(db, "abi")
        .push_str(db, "AbiSize");
    let trait_ref = Partial::Present(crate::hir_def::TraitRefId::new(
        db,
        Partial::Present(trait_path),
    ));
    let ty = Partial::Present(self_ty);
    let impl_trait_idx = builder.ctxt().next_impl_trait_idx();
    builder.with_item_scope(
        TrackedItemVariant::ImplTrait(impl_trait_idx),
        |builder, id| {
            let consts = vec![
                create_head_size_assoc_const(builder, field_specs),
                create_is_dynamic_assoc_const(builder, field_specs),
            ];
            let impl_trait =
                builder.new_impl_trait(id, trait_ref, ty, vec![], consts, builder.origin());
            create_payload_size_func(builder, field_specs);
            impl_trait
        },
    );
}

fn lower_abi_struct_encode_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, AbiStructDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &[(IdentId<'db>, TypeId<'db>)],
) {
    let field_specs = field_specs.to_vec();

    let impl_trait_idx = builder.ctxt().next_impl_trait_idx();
    let trait_ref = Partial::Present(builder.core_abi_trait_ref_sol("Encode"));
    let ty = Partial::Present(self_ty);
    builder.with_item_scope(
        TrackedItemVariant::ImplTrait(impl_trait_idx),
        |builder, id| {
            let direct_encode_const = create_direct_encode_assoc_const(builder, &field_specs);
            let impl_trait = builder.new_impl_trait(
                id,
                trait_ref,
                ty,
                vec![],
                vec![direct_encode_const],
                builder.origin(),
            );

            // encode<E>() method
            let abi_encoder_trait_ref = builder.core_abi_trait_ref_sol("AbiEncoder");
            let (e_generic_params, e_ty) =
                builder.type_param_with_trait_bound("E", abi_encoder_trait_ref);

            let encoder_ident = builder.ident("e");
            let params = builder.params([
                builder.param_own_self(),
                builder.param_mut_underscore_named(encoder_ident, e_ty),
            ]);

            builder.func_generic(
                "encode",
                e_generic_params,
                params,
                None,
                FuncModifiers::new(Visibility::Private, false, false, false),
                |body| {
                    body.encode_fields(&field_specs, encoder_ident, e_ty);
                },
            );

            // encode_to_ptr() method
            let ptr_ident = builder.ident("ptr");
            let ptr_ty = builder.ty_ident(builder.ident("u256"));
            let ptr_param = builder.param_underscore_named(ptr_ident, ptr_ty);
            let params = builder.params([builder.param_own_self(), ptr_param]);
            let encode_to_ptr_ident = builder.ident("encode_to_ptr");

            builder.func_with_body(
                encode_to_ptr_ident,
                builder.empty_generic_params(),
                params,
                None,
                FuncModifiers::new(Visibility::Private, false, false, false),
                |body| {
                    let db = body.db();
                    let self_expr = body.path_expr(PathId::from_ident(db, IdentId::make_self(db)));
                    let mut field_ptr_ident = ptr_ident;

                    for (index, (field_name, field_ty)) in field_specs.iter().copied().enumerate() {
                        let receiver = body.push_expr(Expr::Field(
                            self_expr,
                            Partial::Present(FieldIndex::Ident(field_name)),
                        ));
                        let field_ptr = body.ident_expr(field_ptr_ident);
                        let call =
                            body.method_call_expr(receiver, encode_to_ptr_ident, vec![field_ptr]);
                        body.emit_expr_stmt(call);

                        if index + 1 != field_specs.len() {
                            let next_ptr_ident = IdentId::new(db, format!("__field_ptr{index}"));
                            let current_ptr = body.ident_expr(field_ptr_ident);
                            let field_size = build_head_size_body_expr(body, field_ty);
                            let next_ptr = body.push_expr(Expr::Bin(
                                current_ptr,
                                field_size,
                                BinOp::Arith(ArithBinOp::Add),
                            ));
                            let next_ptr_pat = body.push_pat(Pat::Path(
                                Partial::Present(PathId::from_ident(db, next_ptr_ident)),
                                false,
                            ));
                            body.emit_stmt(Stmt::Let(next_ptr_pat, None, Some(next_ptr)));
                            field_ptr_ident = next_ptr_ident;
                        }
                    }
                },
            );
            impl_trait
        },
    );
}

fn lower_abi_struct_decode_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, AbiStructDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &[(IdentId<'db>, TypeId<'db>)],
) {
    let fields = field_specs.to_vec();
    let field_names: Vec<_> = fields.iter().map(|(name, _)| *name).collect();

    let trait_ref = builder.core_abi_trait_ref_sol("Decode");

    builder.impl_trait(trait_ref, self_ty, |builder| {
        let abi_decoder_trait_ref = builder.core_abi_trait_ref_sol("AbiDecoder");
        let (d_generic_params, d_ty) =
            builder.type_param_with_trait_bound("D", abi_decoder_trait_ref);

        let decoder_ident = builder.ident("d");
        let params = builder.params([builder.param_mut_underscore_named(decoder_ident, d_ty)]);

        builder.func_generic(
            "decode_payload",
            d_generic_params,
            params,
            Some(builder.self_ty()),
            FuncModifiers::new(Visibility::Private, false, false, false),
            |body| {
                for (name, ty) in fields.iter().copied() {
                    body.decode_into(name, ty, d_ty);
                }
                body.return_record_self(&field_names);
            },
        );
    });
}
