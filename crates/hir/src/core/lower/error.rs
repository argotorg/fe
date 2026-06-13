use parser::ast::{self, prelude::*};
use salsa::Accumulator as _;

use super::{
    AbiFieldContext, AbiFieldDiagnostic, FileLowerCtxt,
    attr::{has_named_attr, lower_attrs_without_named, named_attr_specs},
    hir_builder::HirBuilder,
    msg::{
        build_head_size_body_expr, create_direct_encode_assoc_const, create_head_size_assoc_const,
        create_is_dynamic_assoc_const, create_payload_size_func,
    },
};
use crate::{
    hir_def::{
        ArithBinOp, AssocConstDef, AttrListId, BinOp, Body, BodyKind, Expr, FieldDef,
        FieldDefListId, FieldIndex, FuncModifiers, GenericParamListId, IdentId, LitKind, Partial,
        Pat, PathId, Stmt, Struct, TrackedItemVariant, TraitRefId, TypeId, TypeKind, Visibility,
    },
    span::{ErrorDesugared, HirOrigin},
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

    let field_type_paths = parsed_fields.field_type_paths.clone();
    let field_specs = parsed_fields.field_specs.clone();

    let impl_trait_idx = builder.ctxt().next_impl_trait_idx();
    builder.with_item_scope(
        TrackedItemVariant::ImplTrait(impl_trait_idx),
        move |builder, id| {
            let selector_const = create_selector_const(
                builder.ctxt(),
                error_desugared.clone(),
                &struct_name_str,
                &field_type_paths,
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
    lower_error_abi_size_impl(&mut builder, self_ty, &field_specs);

    // Generate impl Encode<Sol>
    lower_error_encode_impl(&mut builder, self_ty, &field_specs);

    struct_
}

struct ParsedErrorFields<'db> {
    hir_fields: Vec<FieldDef<'db>>,
    field_type_paths: Vec<PathId<'db>>,
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
    let mut field_type_paths = Vec::new();
    let mut field_specs = Vec::new();
    let mut is_valid = true;

    let Some(fields) = ast.fields() else {
        return ParsedErrorFields {
            hir_fields,
            field_type_paths,
            field_specs,
            is_valid,
        };
    };

    for field in fields {
        let attrs = AttrListId::lower_ast_opt(ctxt, field.attr_list());
        let name_tok = field.name();
        let name_ident = IdentId::lower_token_partial(ctxt, name_tok.clone());
        let ty_ref = TypeId::lower_ast_partial(ctxt, field.ty());
        let vis = super::lower_field_visibility(&field);

        hir_fields.push(FieldDef::new(attrs, name_ident, ty_ref, vis, false));

        let (Some(name_ident), Some(ty)) = (name_ident.to_opt(), ty_ref.to_opt()) else {
            is_valid = false;
            continue;
        };

        let TypeKind::Path(Partial::Present(path)) = ty.data(db) else {
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

        field_type_paths.push(*path);
        field_specs.push((name_ident, ty));
    }

    ParsedErrorFields {
        hir_fields,
        field_type_paths,
        field_specs,
        is_valid,
    }
}

/// Build SELECTOR as:
///
/// ```text
/// sol(("StructName", "(", Field1Type::SOL_TYPE, ",", Field2Type::SOL_TYPE, ..., ")"))
/// ```
///
/// This produces a `u32` value matching the Solidity custom error selector.
fn create_selector_const<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    desugared: ErrorDesugared,
    struct_name: &str,
    field_type_paths: &[PathId<'db>],
) -> AssocConstDef<'db> {
    let db = ctxt.db();
    let roots = super::hir_builder::LibRoots::for_ctxt(ctxt);

    let selector_name = IdentId::new(db, "SELECTOR".to_string());
    let selector_ty = TypeId::new(
        db,
        TypeKind::Path(Partial::Present(PathId::from_ident(
            db,
            IdentId::new(db, "u32".to_string()),
        ))),
    );

    let origin: HirOrigin<ast::Expr> = HirOrigin::desugared(desugared.clone());

    let id = ctxt.joined_id(TrackedItemVariant::NamelessBody);
    let mut body_ctxt = super::body::BodyCtxt::new(ctxt, id);

    // sol() callee — std::abi::sol::sol
    let sol_path = PathId::from_ident(db, roots.std)
        .push_str(db, "abi")
        .push_str(db, "sol")
        .push_str(db, "sol");
    let callee = Expr::Path(Partial::Present(sol_path));
    let callee_id = body_ctxt.push_expr(callee, origin.clone());

    // Build the tuple: ("StructName", "(", Field1::SOL_TYPE, ",", ..., ")")
    let mut tuple_elems = Vec::new();

    // Struct name as string literal
    let name_lit = Expr::Lit(LitKind::String(crate::hir_def::StringId::new(
        db,
        struct_name.to_string(),
    )));
    tuple_elems.push(body_ctxt.push_expr(name_lit, origin.clone()));

    // "("
    let open_paren = Expr::Lit(LitKind::String(crate::hir_def::StringId::new(
        db,
        "(".to_string(),
    )));
    tuple_elems.push(body_ctxt.push_expr(open_paren, origin.clone()));

    // Field types with comma separators
    for (idx, field_path) in field_type_paths.iter().enumerate() {
        if idx > 0 {
            let comma = Expr::Lit(LitKind::String(crate::hir_def::StringId::new(
                db,
                ",".to_string(),
            )));
            tuple_elems.push(body_ctxt.push_expr(comma, origin.clone()));
        }

        // FieldType::SOL_TYPE
        let sol_type_path = field_path.push_str(db, "SOL_TYPE");
        let sol_type_expr = Expr::Path(Partial::Present(sol_type_path));
        tuple_elems.push(body_ctxt.push_expr(sol_type_expr, origin.clone()));
    }

    // ")"
    let close_paren = Expr::Lit(LitKind::String(crate::hir_def::StringId::new(
        db,
        ")".to_string(),
    )));
    tuple_elems.push(body_ctxt.push_expr(close_paren, origin.clone()));

    // Build the tuple expression and wrap in sol() call
    let tuple_expr = Expr::Tuple(tuple_elems);
    let tuple_id = body_ctxt.push_expr(tuple_expr, origin.clone());

    let call = Expr::Call(
        callee_id,
        vec![crate::hir_def::expr::CallArg {
            label: None,
            expr: tuple_id,
        }],
    );
    let call_id = body_ctxt.push_expr(call, origin.clone());

    let body = Body::new(
        db,
        id,
        call_id,
        BodyKind::Anonymous,
        body_ctxt.stmts,
        body_ctxt.exprs,
        body_ctxt.conds,
        body_ctxt.pats,
        body_ctxt.f_ctxt.top_mod(),
        body_ctxt.source_map,
        origin,
    );
    body_ctxt.f_ctxt.leave_item_scope(body);

    AssocConstDef {
        attributes: AttrListId::new(db, vec![]),
        name: Partial::Present(selector_name),
        ty: Partial::Present(selector_ty),
        value: Partial::Present(body),
    }
}

fn lower_error_abi_size_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, ErrorDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &[(IdentId<'db>, TypeId<'db>)],
) {
    let db = builder.db();
    let roots = builder.roots();
    let trait_path = PathId::from_ident(db, roots.core)
        .push_str(db, "abi")
        .push_str(db, "AbiSize");
    let trait_ref = Partial::Present(TraitRefId::new(db, Partial::Present(trait_path)));
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

fn lower_error_encode_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, ErrorDesugared>,
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

            builder.func_generic_inline_always(
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

            builder.func_with_body_inline_always(
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
