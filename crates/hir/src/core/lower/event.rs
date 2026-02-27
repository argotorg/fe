use parser::ast::{self, prelude::*};
use salsa::Accumulator as _;

use super::{FileLowerCtxt, hir_builder::HirBuilder};
use crate::{
    hir_def::{
        AssocConstDef, Attr, AttrListId, Body, BodyKind, Expr, FieldDef, FieldDefListId,
        FieldIndex, FuncModifiers, FuncParam, FuncParamMode, FuncParamName, GenericArgListId,
        GenericParamListId, IdentId, IntegerId, LitKind, Partial, Pat, PathId, Stmt, Struct,
        TrackedItemVariant, TraitRefId, TupleTypeId, TypeId, TypeKind, TypeMode, Visibility,
    },
    span::{EventDesugared, HirOrigin},
};

/// Event-related errors accumulated during `#[event]` lowering / validation.
#[salsa::accumulator]
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
    EventAttrOnNonStruct { item_kind: &'static str },
    InvalidEventAttrForm,
    GenericEventStruct,
    IndexedAttrOutsideEventStruct,
    InvalidIndexedAttrForm,
    TooManyIndexedFields { indexed_count: usize },
    UnsupportedFieldType { ty: String },
}

pub(super) fn is_event_struct(ast: &ast::Struct) -> bool {
    ast.attr_list()
        .is_some_and(|attrs| has_named_attr(attrs, "event"))
}

pub(super) fn report_event_attr_on_non_struct_item<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    item_kind: &'static str,
) {
    let Some(attrs) = attrs else { return };
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);

    for attr in attrs {
        let ast::AttrKind::Normal(normal) = attr.kind() else {
            continue;
        };
        let is_event = normal.path().is_some_and(|p| p.text() == "event");
        if !is_event {
            continue;
        }

        EventError {
            kind: EventErrorKind::EventAttrOnNonStruct { item_kind },
            file,
            primary_range: attr.syntax().text_range(),
            struct_name: None,
            field_name: None,
        }
        .accumulate(db);
    }
}

pub(super) fn report_indexed_attrs_outside_event_struct<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Struct,
) {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);
    let struct_name = ast.name().map(|n| n.text().to_string());

    let Some(fields) = ast.fields() else { return };
    for field in fields {
        let Some(attr_list) = field.attr_list() else {
            continue;
        };
        for attr in attr_list {
            let ast::AttrKind::Normal(normal) = attr.kind() else {
                continue;
            };
            let is_indexed = normal.path().is_some_and(|p| p.text() == "indexed");
            if !is_indexed {
                continue;
            }

            EventError {
                kind: EventErrorKind::IndexedAttrOutsideEventStruct,
                file,
                primary_range: attr.syntax().text_range(),
                struct_name: struct_name.clone(),
                field_name: field.name().map(|n| n.text().to_string()),
            }
            .accumulate(db);
        }
    }
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
    let mut builder = HirBuilder::new(ctxt, event_desugared.clone());

    let struct_name_token = ast.name();
    let struct_name = struct_name_token.as_ref().map(|n| n.text().to_string());

    let (attributes, event_attr_range, event_attr_invalid_form) =
        lower_attrs_without_named(builder.ctxt(), ast.attr_list(), "event");
    if event_attr_invalid_form && let Some(range) = event_attr_range {
        EventError {
            kind: EventErrorKind::InvalidEventAttrForm,
            file,
            primary_range: range,
            struct_name: struct_name.clone(),
            field_name: None,
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
        EventError {
            kind: EventErrorKind::GenericEventStruct,
            file,
            primary_range: range,
            struct_name: struct_name.clone(),
            field_name: None,
        }
        .accumulate(db);
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

    let indexed_fields = parsed_fields.indexed_fields.clone();
    let data_fields = parsed_fields.data_fields.clone();
    let ordered_field_type_paths = parsed_fields.ordered_field_type_paths.clone();

    let impl_trait_idx = builder.ctxt().next_impl_trait_idx();
    let trait_ref = Partial::Present(trait_ref);
    let self_ty = Partial::Present(self_ty);
    builder.with_item_scope(
        TrackedItemVariant::ImplTrait(impl_trait_idx),
        move |builder, id| {
            let topic0_const = create_topic0_const(
                builder.ctxt(),
                event_desugared.clone(),
                &struct_name_str,
                &ordered_field_type_paths,
            );
            let impl_trait = builder.new_impl_trait(
                id,
                trait_ref,
                self_ty,
                vec![],
                vec![topic0_const],
                builder.origin(),
            );
            lower_emit_method(builder, &indexed_fields, &data_fields);
            impl_trait
        },
    );

    struct_
}

struct ParsedEventFields<'db> {
    hir_fields: Vec<FieldDef<'db>>,
    /// The path of each field's type, used to generate `FieldType::SOL_TYPE`
    /// expressions in the TOPIC0 keccak tuple.
    ordered_field_type_paths: Vec<PathId<'db>>,
    /// Indexed fields with their TypeId (for topic encoding).
    indexed_fields: Vec<(IdentId<'db>, TypeId<'db>)>,
    data_fields: Vec<(IdentId<'db>, TypeId<'db>)>,
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
    let mut ordered_field_type_paths = Vec::new();
    let mut indexed_fields = Vec::new();
    let mut data_fields = Vec::new();
    let mut indexed_ranges = Vec::new();
    let mut indexed_count = 0usize;

    let mut is_valid = true;

    let Some(fields) = ast.fields() else {
        return ParsedEventFields {
            hir_fields,
            ordered_field_type_paths,
            indexed_fields,
            data_fields,
            is_valid,
        };
    };

    for field in fields {
        let (attrs, indexed_range, indexed_invalid_form) =
            lower_attrs_without_named(ctxt, field.attr_list(), "indexed");
        let is_indexed = indexed_range.is_some();
        if is_indexed {
            indexed_count += 1;
            if let Some(r) = indexed_range {
                indexed_ranges.push(r);
            }
        }
        if indexed_invalid_form && let Some(range) = indexed_range {
            EventError {
                kind: EventErrorKind::InvalidIndexedAttrForm,
                file,
                primary_range: range,
                struct_name: struct_name.map(|s| s.to_string()),
                field_name: field.name().map(|n| n.text().to_string()),
            }
            .accumulate(db);
            is_valid = false;
        }

        let name_tok = field.name();
        let name_ident = IdentId::lower_token_partial(ctxt, name_tok.clone());

        let ty_ref = TypeId::lower_ast_partial(ctxt, field.ty());

        let vis = if field.pub_kw().is_some() {
            Visibility::Public
        } else {
            Visibility::Private
        };

        hir_fields.push(FieldDef::new(attrs, name_ident, ty_ref, vis));

        let (Some(name_ident), Some(ty)) = (name_ident.to_opt(), ty_ref.to_opt()) else {
            is_valid = false;
            continue;
        };

        // Extract the type path. We need it to generate `FieldType::SOL_TYPE`
        // in the TOPIC0 computation. Non-path types (tuples, etc.) are not
        // supported as event fields.
        let TypeKind::Path(Partial::Present(path)) = ty.data(db) else {
            EventError {
                kind: EventErrorKind::UnsupportedFieldType {
                    ty: ty.pretty_print(db),
                },
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

        ordered_field_type_paths.push(*path);

        if is_indexed {
            indexed_fields.push((name_ident, ty));
        } else {
            data_fields.push((name_ident, ty));
        }
    }

    if indexed_count > 3 {
        let primary_range = indexed_ranges
            .get(3)
            .copied()
            .unwrap_or_else(|| ast.syntax().text_range());
        EventError {
            kind: EventErrorKind::TooManyIndexedFields { indexed_count },
            file,
            primary_range,
            struct_name: struct_name.map(|s| s.to_string()),
            field_name: None,
        }
        .accumulate(db);
        is_valid = false;
    }

    ParsedEventFields {
        hir_fields,
        ordered_field_type_paths,
        indexed_fields,
        data_fields,
        is_valid,
    }
}

/// Build TOPIC0 as:
///
/// ```text
/// keccak(("StructName", "(", Field1Type::SOL_TYPE, ",", Field2Type::SOL_TYPE, ..., ")"))
/// ```
fn create_topic0_const<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    desugared: EventDesugared,
    struct_name: &str,
    field_type_paths: &[PathId<'db>],
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

    let origin: HirOrigin<ast::Expr> = HirOrigin::desugared(desugared.clone());

    let id = ctxt.joined_id(TrackedItemVariant::NamelessBody);
    let mut body_ctxt = super::body::BodyCtxt::new(ctxt, id);

    // keccak callee
    let keccak_path = PathId::from_ident(db, roots.core).push_str(db, "keccak");
    let callee = Expr::Path(Partial::Present(keccak_path));
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
        name: Partial::Present(topic0_name),
        ty: Partial::Present(topic0_ty),
        value: Partial::Present(body),
    }
}

fn lower_emit_method<'db>(
    builder: &mut HirBuilder<'_, 'db, EventDesugared>,
    indexed_fields: &[(IdentId<'db>, TypeId<'db>)],
    data_fields: &[(IdentId<'db>, TypeId<'db>)],
) {
    let db = builder.db();
    let roots = builder.roots();

    let emit_ident = builder.ident("emit");
    let log_provider_ident = builder.ident("log");
    let enc_ident = builder.ident("enc");
    let data_len_ident = builder.ident("data_len");
    let data_ptr_ident = builder.ident("data_ptr");
    let reserve_head_ident = builder.ident("reserve_head");
    let encode_ident = builder.ident("encode");
    let finish_ident = builder.ident("finish");
    let as_topic_ident = builder.ident("as_topic");
    let log_method_ident = builder.ident(&format!("log{}", indexed_fields.len() + 1));

    let log_trait_ref = TraitRefId::new(
        db,
        Partial::Present(
            PathId::from_ident(db, roots.std)
                .push_str(db, "evm")
                .push_str(db, "Log"),
        ),
    );
    let (generic_params, log_ty) = builder.type_param_with_trait_bound("L", log_trait_ref);
    let log_param_ty = TypeId::new(db, TypeKind::Mode(TypeMode::Mut, Partial::Present(log_ty)));

    let self_param = FuncParam {
        mode: FuncParamMode::View,
        is_mut: false,
        has_ref_prefix: false,
        has_own_prefix: false,
        is_label_suppressed: false,
        name: Partial::Present(FuncParamName::Ident(IdentId::make_self(db))),
        ty: Partial::Present(builder.self_ty()),
        self_ty_fallback: true,
    };

    let log_param = FuncParam {
        mode: FuncParamMode::View,
        is_mut: false,
        has_ref_prefix: false,
        has_own_prefix: false,
        is_label_suppressed: false,
        name: Partial::Present(FuncParamName::Ident(log_provider_ident)),
        ty: Partial::Present(log_param_ty),
        self_ty_fallback: false,
    };

    let params = builder.params([self_param, log_param]);
    let modifiers = FuncModifiers::new(Visibility::Private, false, false, false);

    builder.func_with_body(
        emit_ident,
        generic_params,
        params,
        None,
        modifiers,
        move |body| {
            let self_expr = body.path_expr(PathId::from_ident(db, IdentId::make_self(db)));

            let (data_ptr, data_len) = if data_fields.is_empty() {
                (int_lit(body, 0), int_lit(body, 0))
            } else {
                let enc_new_call = {
                    let new_path = PathId::from_ident(db, roots.std)
                        .push_str(db, "abi")
                        .push_str(db, "SolEncoder")
                        .push_str(db, "new");
                    let callee = body.path_expr(new_path);
                    body.call_expr(callee, vec![])
                };
                let enc_pat = body.push_pat(Pat::Path(
                    Partial::Present(PathId::from_ident(db, enc_ident)),
                    true,
                ));
                body.emit_stmt(Stmt::Let(enc_pat, None, Some(enc_new_call)));

                let size_ty = if data_fields.len() == 1 {
                    data_fields[0].1
                } else {
                    let tuple_elems: Vec<Partial<TypeId<'db>>> = data_fields
                        .iter()
                        .map(|(_, ty)| Partial::Present(*ty))
                        .collect();
                    TypeId::new(db, TypeKind::Tuple(TupleTypeId::new(db, tuple_elems)))
                };
                let encoded_size_call = {
                    let args = GenericArgListId::given1_type(db, size_ty);
                    let path = PathId::from_ident(db, roots.std)
                        .push_str(db, "abi")
                        .push_str_args(db, "encoded_size", args);
                    let callee = body.path_expr(path);
                    body.call_expr(callee, vec![])
                };

                let data_len_pat = body.push_pat(Pat::Path(
                    Partial::Present(PathId::from_ident(db, data_len_ident)),
                    false,
                ));
                body.emit_stmt(Stmt::Let(data_len_pat, None, Some(encoded_size_call)));

                let enc_expr = body.ident_expr(enc_ident);
                let data_len_expr = body.ident_expr(data_len_ident);
                let reserve_head =
                    body.method_call_expr(enc_expr, reserve_head_ident, vec![data_len_expr]);
                body.emit_expr_stmt(reserve_head);

                let encode_receiver = if data_fields.len() == 1 {
                    self_field_expr(body, self_expr, data_fields[0].0)
                } else {
                    let mut elems = Vec::with_capacity(data_fields.len());
                    for (name, _) in data_fields.iter().copied() {
                        elems.push(self_field_expr(body, self_expr, name));
                    }
                    body.push_expr(Expr::Tuple(elems))
                };
                let enc_arg_base = body.ident_expr(enc_ident);
                let enc_arg = body.mut_expr(enc_arg_base);
                let encode = body.method_call_expr(encode_receiver, encode_ident, vec![enc_arg]);
                body.emit_expr_stmt(encode);

                let enc_expr = body.ident_expr(enc_ident);
                let finish_call = body.method_call_expr(enc_expr, finish_ident, vec![]);
                let data_ptr_pat = body.push_pat(Pat::Path(
                    Partial::Present(PathId::from_ident(db, data_ptr_ident)),
                    false,
                ));
                let data_len_pat = body.push_pat(Pat::Path(
                    Partial::Present(PathId::from_ident(db, data_len_ident)),
                    false,
                ));
                let tuple_pat = body.push_pat(Pat::Tuple(vec![data_ptr_pat, data_len_pat]));
                body.emit_stmt(Stmt::Let(tuple_pat, None, Some(finish_call)));

                (
                    body.ident_expr(data_ptr_ident),
                    body.ident_expr(data_len_ident),
                )
            };

            let topic0 = {
                let path = PathId::from_ident(db, IdentId::make_self_ty(db)).push_str(db, "TOPIC0");
                body.path_expr(path)
            };
            let mut args = Vec::with_capacity(3 + indexed_fields.len());
            args.push(data_ptr);
            args.push(data_len);
            args.push(topic0);

            for (name, _ty) in indexed_fields.iter().copied() {
                let value = self_field_expr(body, self_expr, name);
                let topic = body.method_call_expr(value, as_topic_ident, vec![]);
                args.push(topic);
            }

            let log_expr = body.ident_expr(log_provider_ident);
            let log_call = body.method_call_expr(log_expr, log_method_ident, args);
            body.emit_expr_stmt(log_call);
        },
    );
}

fn self_field_expr<'db>(
    body: &mut super::hir_builder::BodyBuilder<'_, 'db, EventDesugared>,
    receiver: crate::hir_def::ExprId,
    field: IdentId<'db>,
) -> crate::hir_def::ExprId {
    body.push_expr(Expr::Field(
        receiver,
        Partial::Present(FieldIndex::Ident(field)),
    ))
}

fn int_lit<'db>(
    body: &mut super::hir_builder::BodyBuilder<'_, 'db, EventDesugared>,
    v: usize,
) -> crate::hir_def::ExprId {
    let db = body.db();
    body.push_expr(Expr::Lit(LitKind::Int(IntegerId::from_usize(db, v))))
}

fn has_named_attr(attrs: ast::AttrList, name: &str) -> bool {
    attrs.into_iter().any(|attr| match attr.kind() {
        ast::AttrKind::Normal(normal) => normal.path().is_some_and(|p| p.text() == name),
        ast::AttrKind::DocComment(_) => false,
    })
}

fn lower_attrs_without_named<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    name: &str,
) -> (AttrListId<'db>, Option<parser::TextRange>, bool) {
    let db = ctxt.db();

    let mut attr_range: Option<parser::TextRange> = None;
    let mut invalid_form = false;

    let out = attrs
        .into_iter()
        .flatten()
        .filter_map(|attr| match attr.kind() {
            ast::AttrKind::DocComment(_) => Some(Attr::lower_ast(ctxt, attr)),
            ast::AttrKind::Normal(normal) => {
                let is_named = normal.path().is_some_and(|p| p.text() == name);
                if !is_named {
                    return Some(Attr::lower_ast(ctxt, attr));
                }

                attr_range.get_or_insert(attr.syntax().text_range());
                if normal.args().is_some() || normal.value().is_some() {
                    invalid_form = true;
                }
                None
            }
        })
        .collect::<Vec<_>>();

    (AttrListId::new(db, out), attr_range, invalid_form)
}
