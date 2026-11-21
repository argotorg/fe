use parser::ast::{self, prelude::*};

use super::{
    AbiFieldContext, AbiFieldDiagnostic, AbiFieldDiagnosticKind, FileLowerCtxt,
    attr::{
        AttrForm, AttrRule, AttrTarget, has_named_attr, lower_attrs_without_named,
        named_attr_specs, validate_attr_rules,
    },
    hir_builder::HirBuilder,
};
use crate::{
    hir_def::{
        AssocConstDef, AttrListId, Body, BodyKind, Expr, FieldDef, FieldDefListId,
        GenericParamListId, IdentId, LitKind, Partial, PathId, PathKind, Struct,
        TrackedItemVariant, TraitRefId, TypeId, TypeKind,
    },
    span::{EventDesugared, HirOrigin},
};

/// Event-related errors accumulated during `#[event]` lowering / validation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EventError {
    pub kind: EventErrorKind,
    pub file: common::file::File,
    /// Range of the primary span (attribute, type, or item name).
    pub primary_range: parser::TextRange,
    pub struct_name: Option<String>,
    pub field_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EventErrorKind {
    GenericEventStruct,
    TooManyIndexedFields { indexed_count: usize },
    IndexedDynamicField { ty: String },
    IndexedCompositeField { ty: String },
}

pub(super) fn is_event_struct(ast: &ast::Struct) -> bool {
    has_named_attr(ast.attr_list(), "event")
}

pub(super) fn lower_event_struct<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: ast::Struct,
) -> Struct<'db> {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);

    let event_desugared = EventDesugared {
        event_struct: parser::ast::AstPtr::new(&ast),
    };
    let mut builder = HirBuilder::new(ctxt, event_desugared.clone(), ast.syntax());

    let struct_name_token = ast.name();
    let struct_name = struct_name_token.as_ref().map(|n| n.text().to_string());

    let attributes = lower_attrs_without_named(builder.ctxt(), ast.attr_list(), "event");

    let vis = super::lower_visibility(&ast);
    let generic_params = GenericParamListId::lower_ast_opt(builder.ctxt(), ast.generic_params());
    if !generic_params.data(db).is_empty() {
        let range = ast
            .generic_params()
            .map_or_else(|| ast.syntax().text_range(), |g| g.syntax().text_range());
        builder.ctxt().report(EventError {
            kind: EventErrorKind::GenericEventStruct,
            file,
            primary_range: range,
            struct_name: struct_name.clone(),
            field_name: None,
        });
    }

    let where_clause =
        crate::hir_def::WhereClauseId::lower_ast_opt(builder.ctxt(), ast.where_clause());

    let parsed_fields = parse_event_fields(builder.ctxt(), &ast, struct_name.as_deref());

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

    // Generate `impl Event` only when the struct is well-formed enough to do so.
    if !parsed_fields.is_valid {
        return struct_;
    }
    if !generic_params.data(db).is_empty() {
        return struct_;
    }

    let Some(struct_name_str) = struct_name.clone() else {
        // Parser error: missing name token. Avoid panics/cascades.
        return struct_;
    };

    let Some(struct_name_ident) = name_ident.to_opt() else {
        return struct_;
    };
    let self_ty = TypeId::new(
        db,
        TypeKind::Path(Partial::Present(PathId::from_ident(db, struct_name_ident))),
    );
    let trait_ref = TraitRefId::new(
        db,
        Partial::Present(
            PathId::from_ident(db, builder.roots().std)
                .push_str(db, "evm")
                .push_str(db, "Event"),
        ),
    );

    let attrs = named_attr_specs(ast.attr_list(), "event");
    let [attr] = attrs.as_slice() else {
        return struct_;
    };
    if !attr.is_bare() {
        return struct_;
    }
    let ordered_field_types = parsed_fields.ordered_field_types;
    super::short_form::lower_derive_slot(
        &mut builder,
        self_ty,
        trait_ref,
        "StructEvent",
        (attr.range, 0),
        |ctxt| {
            vec![create_topic0_const(
                ctxt,
                event_desugared,
                &struct_name_str,
                &ordered_field_types,
            )]
        },
    );
    struct_
}

struct ParsedEventFields<'db> {
    hir_fields: Vec<FieldDef<'db>>,
    /// Each field's source type, used to generate Solidity signature fragments.
    ordered_field_types: Vec<TypeId<'db>>,
    is_valid: bool,
}

fn parse_event_fields<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Struct,
    struct_name: Option<&str>,
) -> ParsedEventFields<'db> {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);

    let mut hir_fields = Vec::new();
    let mut ordered_field_types = Vec::new();
    let mut indexed_ranges = Vec::new();
    let mut indexed_count = 0usize;

    let mut is_valid = true;

    let Some(fields) = ast.fields() else {
        return ParsedEventFields {
            hir_fields,
            ordered_field_types,
            is_valid,
        };
    };

    for field in fields {
        validate_attr_rules(
            ctxt,
            field.attr_list(),
            AttrTarget::new(
                "event field",
                field.name().map(|name| name.text().to_string()),
            ),
            &[AttrRule::supported(
                "indexed",
                AttrForm::Bare,
                "`#[indexed]`",
            )],
        );
        super::item::report_unsupported_field_mut(ctxt, &field, "event field");
        let indexed_attrs = named_attr_specs(field.attr_list(), "indexed");
        let attrs = lower_attrs_without_named(ctxt, field.attr_list(), "indexed");
        let indexed_range = indexed_attrs.first().map(|attr| attr.range);
        let is_indexed = indexed_range.is_some();
        if is_indexed {
            indexed_count += 1;
            if let Some(r) = indexed_range {
                indexed_ranges.push(r);
            }
        }
        if indexed_attrs.iter().any(|attr| !attr.is_bare()) || indexed_attrs.len() > 1 {
            is_valid = false;
        }

        let name_tok = field.name();
        let name_ident = IdentId::lower_token_partial(ctxt, name_tok.clone());

        let ty_ref = TypeId::lower_ast_partial(ctxt, field.ty());

        let vis = super::lower_field_visibility(&field);

        hir_fields.push(FieldDef::new(
            attrs, name_ident, ty_ref, vis, is_indexed, false,
        ));

        let (Some(_), Some(ty)) = (name_ident.to_opt(), ty_ref.to_opt()) else {
            is_valid = false;
            continue;
        };

        // Extract the type path. We need it to generate `FieldType::SOL_TYPE`
        // in the TOPIC0 computation. Non-path types (tuples, etc.) are not
        // supported as event fields.
        let TypeKind::Path(Partial::Present(_)) = ty.data(db) else {
            ctxt.report(AbiFieldDiagnostic {
                kind: if matches!(ty.data(db), TypeKind::Array(..)) {
                    AbiFieldDiagnosticKind::UnsupportedArray
                } else {
                    AbiFieldDiagnosticKind::Unsupported
                },
                context: AbiFieldContext::Event,
                ty: ty.pretty_print(db),
                file,
                primary_range: field
                    .ty()
                    .map_or_else(|| field.syntax().text_range(), |t| t.syntax().text_range()),
                struct_name: struct_name.map(|s| s.to_string()),
                field_name: name_tok.map(|n| n.text().to_string()),
            });
            is_valid = false;
            continue;
        };

        ordered_field_types.push(ty);
    }

    if indexed_count > 3 {
        let primary_range = indexed_ranges
            .get(3)
            .copied()
            .unwrap_or_else(|| ast.syntax().text_range());
        ctxt.report(EventError {
            kind: EventErrorKind::TooManyIndexedFields { indexed_count },
            file,
            primary_range,
            struct_name: struct_name.map(|s| s.to_string()),
            field_name: None,
        });
        is_valid = false;
    }

    ParsedEventFields {
        hir_fields,
        ordered_field_types,
        is_valid,
    }
}

/// Build TOPIC0 as:
///
/// ```text
/// keccak(("StructName", "(", Field1Type::SOL_TYPE, ..., ")"))
/// ```
///
/// Longer signatures are nested in chunks to stay within `AsBytes`' tuple-arity
/// implementations. Container types compose their canonical names through
/// `SolCompat::SOL_TYPE`.
fn create_topic0_const<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    desugared: EventDesugared,
    struct_name: &str,
    field_types: &[TypeId<'db>],
) -> AssocConstDef<'db> {
    let db = ctxt.db();
    let roots = super::hir_builder::LibRoots::for_ctxt(ctxt);

    let topic0_name = IdentId::new(db, "TOPIC0".to_string());
    let topic0_ty = TypeId::new(
        db,
        TypeKind::Path(Partial::Present(PathId::from_ident(
            db,
            IdentId::new(db, "u256".to_string()),
        ))),
    );

    let origin: HirOrigin<ast::Expr> = HirOrigin::desugared(desugared);

    let id = ctxt.joined_id(TrackedItemVariant::NamelessBody);
    let mut body_ctxt = super::body::BodyCtxt::new(ctxt, id);

    // keccak callee
    let keccak_path = PathId::from_ident(db, roots.core).push_str(db, "keccak");
    let callee = Expr::Path(Partial::Present(keccak_path));
    let callee_id = body_ctxt.push_expr(callee, origin.clone());

    // Build the signature fragments. Long signatures are chunked below.
    let mut tuple_elems = Vec::new();

    // A name can exceed String<32>; hash word-sized fragments in order.
    for chunk in super::signature_name_chunks(struct_name) {
        let name_lit = Expr::Lit(LitKind::String(crate::hir_def::StringId::new(
            db,
            chunk.to_string(),
        )));
        tuple_elems.push(body_ctxt.push_expr(name_lit, origin.clone()));
    }

    // "("
    let open_paren = Expr::Lit(LitKind::String(crate::hir_def::StringId::new(
        db,
        "(".to_string(),
    )));
    tuple_elems.push(body_ctxt.push_expr(open_paren, origin.clone()));

    // Field types with comma separators
    for (idx, field_ty) in field_types.iter().copied().enumerate() {
        if idx > 0 {
            let comma = Expr::Lit(LitKind::String(crate::hir_def::StringId::new(
                db,
                ",".to_string(),
            )));
            tuple_elems.push(body_ctxt.push_expr(comma, origin.clone()));
        }

        push_sol_type_fragments(&mut body_ctxt, field_ty, &origin, &mut tuple_elems);
    }

    // ")"
    let close_paren = Expr::Lit(LitKind::String(crate::hir_def::StringId::new(
        db,
        ")".to_string(),
    )));
    tuple_elems.push(body_ctxt.push_expr(close_paren, origin.clone()));

    // The `AsBytes` tuple impls in `core` stop at arity 16, so nest the
    // elements into chunks when the signature is longer. Byte concatenation is
    // associative, so the nesting doesn't change the hashed bytes.
    const MAX_TUPLE_ARITY: usize = 16;
    while tuple_elems.len() > MAX_TUPLE_ARITY {
        tuple_elems = tuple_elems
            .chunks(MAX_TUPLE_ARITY)
            .map(|chunk| {
                if let [single] = chunk {
                    *single
                } else {
                    body_ctxt.push_expr(Expr::Tuple(chunk.to_vec()), origin.clone())
                }
            })
            .collect();
    }

    // Build the tuple expression and wrap in keccak call
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

    let quote_origins = body_ctxt.take_quote_origins();
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
        body_ctxt.f_ctxt.lazy_func(),
        quote_origins,
        body_ctxt.source_map,
        origin,
    );
    body_ctxt.f_ctxt.leave_item_scope(body);

    AssocConstDef {
        attributes: AttrListId::new(db, vec![]),
        name: Partial::Present(topic0_name),
        ty: Partial::Present(topic0_ty),
        value: Partial::Present(body),
        vis: crate::hir_def::Visibility::Public,
        generated: None,
    }
}

fn push_sol_type_fragments<'db>(
    body: &mut super::body::BodyCtxt<'_, 'db>,
    ty: TypeId<'db>,
    origin: &HirOrigin<ast::Expr>,
    out: &mut Vec<crate::hir_def::ExprId>,
) {
    let db = body.f_ctxt.db();
    let TypeKind::Path(Partial::Present(_)) = ty.data(db) else {
        return;
    };

    let roots = super::hir_builder::LibRoots::for_ctxt(body.f_ctxt);
    let sol_compat = TraitRefId::new(
        db,
        Partial::Present(
            PathId::from_ident(db, roots.std)
                .push_str(db, "abi")
                .push_str(db, "SolCompat"),
        ),
    );
    let qualified = PathId::new(
        db,
        PathKind::QualifiedType {
            type_: ty,
            trait_: sol_compat,
        },
        None,
    );
    let sol_type_path = qualified.push_str(db, "SOL_TYPE");
    out.push(body.push_expr(Expr::Path(Partial::Present(sol_type_path)), origin.clone()));
}
