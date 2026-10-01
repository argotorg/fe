use parser::ast::{self, prelude::*};
use salsa::Accumulator as _;

use super::{
    FileLowerCtxt,
    attr::{has_named_attr, lower_attrs_without_named, named_attr_specs},
    hir_builder::{DecodeInputBindings, FuncBodySpec, HirBuilder},
    msg::{
        build_decode_head_pos_expr, create_head_size_assoc_const, create_is_dynamic_assoc_const,
        create_payload_size_func,
    },
};
use crate::{
    HirDb,
    hir_def::{
        AssocConstDef, AttrListId, Body, BodyKind, Expr, ExprId, FieldDefListId, FuncModifiers,
        FuncParam, FuncParamMode, FuncParamName, GenericArgListId, GenericParamListId, IdentId,
        ImplTrait, LitKind, Partial, Pat, PathId, PathKind, Stmt, StringId, Struct,
        TrackedItemVariant, TraitRefId, TupleTypeId, TypeBound, TypeId, TypeKind, Visibility,
        WhereClauseId, WhereClausePredicate, WherePredicate, expr::CallArg,
    },
    span::{AbiStructDesugared, HirOrigin},
};

/// Diagnostics for `#[abi]` struct declarations.
#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AbiStructDiagnostic {
    pub kind: AbiStructDiagnosticKind,
    pub file: common::file::File,
    pub primary_range: parser::TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AbiStructDiagnosticKind {
    GenericStruct,
    /// `#[abi]` together with `#[event]` or `#[error]`.
    AttrConflict,
    /// A field whose type lacks traits the generated codec needs.
    UnsupportedFieldType {
        ty: String,
        missing: Vec<&'static str>,
    },
}

/// Returns true for a struct annotated with `#[abi]`.
pub(super) fn is_abi_struct(ast: &ast::Struct) -> bool {
    has_named_attr(ast.attr_list(), "abi")
}

/// Reports `#[abi]` on a struct that `#[event]` or `#[error]` already encodes.
pub(super) fn report_abi_attr_conflict<'db>(ctxt: &mut FileLowerCtxt<'db>, ast: &ast::Struct) {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);
    for attr in named_attr_specs(ast.attr_list(), "abi") {
        AbiStructDiagnostic {
            kind: AbiStructDiagnosticKind::AttrConflict,
            file,
            primary_range: attr.range,
        }
        .accumulate(db);
    }
}

/// Lowers an `#[abi]` struct and generates its Solidity ABI codec.
///
/// The struct encodes like the Solidity tuple of its fields in declaration
/// order, which is how Solidity encodes a `struct` value:
///
/// ```fe
/// #[abi]
/// pub struct PoolKey { pub currency0: Address, pub fee: Uint24 }
/// ```
///
/// gets `impl AbiSize`, `impl AbiSpan<Sol>`, `impl Encode<Sol>` and
/// `impl Decode<Sol>`, so it can be used in `msg` fields, return types,
/// arrays and other `#[abi]` structs.
pub(super) fn lower_abi_struct<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: ast::Struct,
) -> Struct<'db> {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);
    let desugared = AbiStructDesugared {
        abi_struct: parser::ast::AstPtr::new(&ast),
    };
    let mut builder = HirBuilder::new(ctxt, desugared, ast.syntax());

    let attributes = lower_attrs_without_named(builder.ctxt(), ast.attr_list(), "abi");
    let vis = super::lower_visibility(&ast);
    let generic_params = GenericParamListId::lower_ast_opt(builder.ctxt(), ast.generic_params());
    let where_clause = WhereClauseId::lower_ast_opt(builder.ctxt(), ast.where_clause());
    let fields =
        FieldDefListId::lower_ast_opt_with_context(builder.ctxt(), ast.fields(), "struct field");
    let name = IdentId::lower_token_partial(builder.ctxt(), ast.name());

    let struct_ = builder.struct_item(name, attributes, vis, generic_params, where_clause, fields);

    if !generic_params.data(db).is_empty() {
        AbiStructDiagnostic {
            kind: AbiStructDiagnosticKind::GenericStruct,
            file,
            primary_range: ast
                .generic_params()
                .map(|g| g.syntax().text_range())
                .unwrap_or_else(|| ast.syntax().text_range()),
        }
        .accumulate(db);
        return struct_;
    }

    let Some(name) = name.to_opt() else {
        return struct_;
    };
    let mut field_specs = Vec::new();
    for field in fields.data(db) {
        let (Some(field_name), Some(field_ty)) = (field.name.to_opt(), field.type_ref.to_opt())
        else {
            return struct_;
        };
        field_specs.push((field_name, field_ty));
    }

    let self_ty = TypeId::new(
        db,
        TypeKind::Path(Partial::Present(PathId::from_ident(db, name))),
    );
    lower_abi_size_impl(&mut builder, self_ty, &field_specs);
    lower_abi_span_impl(&mut builder, self_ty, &field_specs);
    lower_encode_impl(&mut builder, self_ty, &field_specs);
    lower_decode_impl(&mut builder, self_ty, &field_specs);
    lower_static_abi_impl(&mut builder, self_ty, &field_specs);
    lower_sol_compat_impl(&mut builder, self_ty, &field_specs);

    struct_
}

type FieldSpecs<'db> = [(IdentId<'db>, TypeId<'db>)];

/// `where F0: Trait, F1: Trait, ...` over the field types.
fn field_bounds<'db>(
    db: &'db dyn HirDb,
    field_specs: &FieldSpecs<'db>,
    trait_ref: TraitRefId<'db>,
) -> WhereClauseId<'db> {
    let predicates: Vec<_> = field_specs
        .iter()
        .map(|(_, ty)| {
            WhereClausePredicate::Type(WherePredicate {
                ty: Partial::Present(*ty),
                bounds: vec![TypeBound::Trait(trait_ref)],
            })
        })
        .collect();
    WhereClauseId::new(db, predicates)
}

fn lower_abi_size_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, AbiStructDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &FieldSpecs<'db>,
) {
    let trait_ref = Partial::Present(builder.core_abi_trait_ref("AbiSize"));
    let impl_trait_idx = builder.ctxt().next_impl_trait_idx();
    builder.with_item_scope(
        TrackedItemVariant::ImplTrait(impl_trait_idx),
        |builder, id| {
            let consts = vec![
                create_head_size_assoc_const(builder, field_specs),
                create_is_dynamic_assoc_const(builder, field_specs),
            ];
            let impl_trait = builder.new_impl_trait(
                id,
                trait_ref,
                Partial::Present(self_ty),
                vec![],
                consts,
                builder.origin(),
            );
            create_payload_size_func(builder, field_specs);
            impl_trait
        },
    );
}

/// `payload_end_with_input_len` checks the head frame and folds every
/// field's end into the frame end, like the tuple impls in `core::abi`;
/// `payload_end` bounds it by the whole input.
fn lower_abi_span_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, AbiStructDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &FieldSpecs<'db>,
) {
    let trait_ref = builder.core_abi_trait_ref_sol("AbiSpan");
    let field_specs = field_specs.to_vec();
    builder.impl_trait(trait_ref, self_ty, |builder| {
        let db = builder.db();
        let u256_ty = builder.ty_ident(builder.ident("u256"));
        let labeled = |name| FuncParam {
            mode: FuncParamMode::View,
            is_mut: false,
            has_ref_prefix: false,
            has_own_prefix: false,
            is_label_suppressed: false,
            name: Partial::Present(FuncParamName::Ident(name)),
            ty: Partial::Present(u256_ty),
            self_ty_fallback: false,
        };
        let input_ident = builder.generated_ident("abi_struct_input");
        let [base_ident, pos_ident, input_len_ident] =
            ["base", "pos", "input_len"].map(|name| builder.ident(name));
        let byte_input = builder.core_abi_trait_ref("ByteInput");
        let modifiers = FuncModifiers::new(Visibility::Private, false, false, false);

        // Self::payload_end_with_input_len(input, base:, pos:, input_len: input.len())
        let (generic_params, input_ty) = builder.type_param_with_trait_bound("I", byte_input);
        let params = builder.params([
            builder.param_underscore_named(input_ident, input_ty),
            labeled(base_ident),
            labeled(pos_ident),
        ]);
        builder.func_generic_inline_always(
            "payload_end",
            generic_params,
            params,
            Some(u256_ty),
            modifiers,
            |body| {
                let callee = body.path_expr(
                    PathId::from_ident(db, IdentId::make_self_ty(db))
                        .push_str(db, "payload_end_with_input_len"),
                );
                let input = body.ident_expr(input_ident);
                let input_len =
                    body.method_call_expr(input, IdentId::new(db, "len".to_string()), vec![]);
                let args = vec![
                    CallArg {
                        label: None,
                        expr: body.ident_expr(input_ident),
                    },
                    CallArg {
                        label: Some(base_ident),
                        expr: body.ident_expr(base_ident),
                    },
                    CallArg {
                        label: Some(pos_ident),
                        expr: body.ident_expr(pos_ident),
                    },
                    CallArg {
                        label: Some(input_len_ident),
                        expr: input_len,
                    },
                ];
                let call = body.call_expr_with_args(callee, args);
                body.emit_return(Some(call));
            },
        );

        let (generic_params, input_ty) = builder.type_param_with_trait_bound("I", byte_input);
        let params = builder.params([
            builder.param_underscore_named(input_ident, input_ty),
            labeled(base_ident),
            labeled(pos_ident),
            labeled(input_len_ident),
        ]);
        builder.func_generic_inline_always(
            "payload_end_with_input_len",
            generic_params,
            params,
            Some(u256_ty),
            modifiers,
            |body| {
                let core = body.roots().core;
                // let mut end: u256 = checked_frame_end<Sol>(pos, Self::HEAD_SIZE, input_len)
                let end_ident = body.generated_ident("end");
                let callee = body.path_expr(
                    PathId::from_ident(db, core)
                        .push_str(db, "abi")
                        .push_str_args(
                            db,
                            "checked_frame_end",
                            GenericArgListId::given_types(db, [body.sol_ty()]),
                        ),
                );
                let args = vec![
                    body.ident_expr(pos_ident),
                    body.abi_size_assoc_expr(TypeId::fallback_self_ty(db), "HEAD_SIZE"),
                    body.ident_expr(input_len_ident),
                ];
                let frame_end = body.call_expr(callee, args);
                let end_pat = body.push_pat(Pat::Path(
                    Partial::Present(PathId::from_ident(db, end_ident)),
                    true,
                ));
                body.emit_stmt(Stmt::Let(end_pat, Some(u256_ty), Some(frame_end)));

                // end = abi_record_field_end<Sol, F, I>(input, pos, head_pos, input_len, end)
                for (idx, (_, field_ty)) in field_specs.iter().copied().enumerate() {
                    let callee = body.path_expr(
                        PathId::from_ident(db, core)
                            .push_str(db, "abi")
                            .push_str_args(
                                db,
                                "abi_record_field_end",
                                GenericArgListId::given_types(
                                    db,
                                    [body.sol_ty(), field_ty, input_ty],
                                ),
                            ),
                    );
                    let args = vec![
                        body.ident_expr(input_ident),
                        body.ident_expr(pos_ident),
                        build_decode_head_pos_expr(body, pos_ident, &field_specs[..idx]),
                        body.ident_expr(input_len_ident),
                        body.ident_expr(end_ident),
                    ];
                    let call = body.call_expr(callee, args);
                    let end_place = body.ident_expr(end_ident);
                    let assign = body.push_expr(Expr::Assign(end_place, call));
                    body.emit_expr_stmt(assign);
                }

                let end = body.ident_expr(end_ident);
                body.emit_return(Some(end));
            },
        );
    });
}

/// `impl StaticAbi for S where F0: StaticAbi, ...`: the struct is static
/// exactly when all of its fields are.
fn lower_static_abi_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, AbiStructDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &FieldSpecs<'db>,
) {
    let db = builder.db();
    let static_abi = TraitRefId::new(
        db,
        Partial::Present(builder.path_from_root(builder.roots().std, &["abi", "StaticAbi"])),
    );
    let where_clause = field_bounds(db, field_specs, static_abi);
    let idx = builder.ctxt().next_impl_trait_idx();
    builder.with_item_scope(TrackedItemVariant::ImplTrait(idx), |builder, id| {
        ImplTrait::new(
            db,
            id,
            Partial::Present(static_abi),
            Partial::Present(self_ty),
            builder.empty_attrs(),
            builder.empty_generic_params(),
            where_clause,
            vec![],
            vec![],
            builder.top_mod(),
            builder.origin(),
        )
    });
}

fn lower_encode_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, AbiStructDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &FieldSpecs<'db>,
) {
    let trait_ref = builder.core_abi_trait_ref_sol("Encode");
    let field_specs = field_specs.to_vec();
    builder.impl_trait(trait_ref, self_ty, |builder| {
        let ptr_ident = builder.ident("ptr");
        let u8_ty = builder.ty_ident(builder.ident("u8"));
        let ptr_ty = builder.ty_ptr(u8_ty);
        let params = builder.params([
            builder.param_own_self(),
            builder.param_underscore_named(ptr_ident, ptr_ty),
        ]);
        builder.func_with_body_inline_always(
            builder.ident("encode"),
            builder.empty_generic_params(),
            params,
            None,
            FuncModifiers::new(Visibility::Private, false, false, false),
            |body| body.encode_fields(&field_specs, ptr_ident),
        );
    });
}

fn lower_decode_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, AbiStructDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &FieldSpecs<'db>,
) {
    let trait_ref = builder.core_abi_trait_ref_sol("Decode");
    let field_specs = field_specs.to_vec();
    let field_names = field_specs
        .iter()
        .map(|(name, _)| *name)
        .collect::<Vec<_>>();
    builder.impl_trait(trait_ref, self_ty, |builder| {
        let abi_decoder = builder.core_abi_trait_ref_sol("AbiDecoder");
        let (generic_params, decoder_ty) = builder.type_param_with_trait_bound("D", abi_decoder);
        let decoder_ident = builder.generated_ident("abi_struct_decoder");
        let params =
            builder.params([builder.param_mut_underscore_named(decoder_ident, decoder_ty)]);
        builder.func_generic_inline_always(
            "decode_payload",
            generic_params,
            params,
            Some(builder.self_ty()),
            FuncModifiers::new(Visibility::Private, false, false, false),
            |body| {
                for (name, ty) in field_specs.iter().copied() {
                    body.decode_into(name, ty, decoder_ident, decoder_ty);
                }
                body.return_record_self(&field_names);
            },
        );

        // Decode fields directly at their head positions without a mutable
        // cursor. Check the head once and preserve the bound on dynamic tails.
        // Let the backend budget these generated direct decoders.
        let u256_ty = builder.ty_ident(builder.ident("u256"));
        let input_ident = builder.generated_ident("abi_struct_input");
        let pos_ident = builder.generated_ident("abi_struct_pos");
        let input_len_ident = builder.generated_ident("abi_struct_input_len");
        let byte_input = builder.core_abi_trait_ref("ByteInput");
        let modifiers = FuncModifiers::new(Visibility::Private, false, false, false);

        let (generic_params, input_ty) = builder.type_param_with_trait_bound("I", byte_input);
        let params = builder.params([
            builder.param_underscore_named(input_ident, input_ty),
            builder.param_underscore_named(pos_ident, u256_ty),
            builder.param_underscore_named(input_len_ident, u256_ty),
        ]);
        builder.func_with_body_spec(
            FuncBodySpec {
                name: builder.ident("decode_from_prechecked_head"),
                attrs: builder.empty_attrs(),
                generic_params,
                params,
                ret_ty: Some(builder.self_ty()),
                modifiers,
            },
            |body| {
                let input = DecodeInputBindings {
                    input_ident,
                    input_ty,
                    base_ident: pos_ident,
                    input_len_ident,
                };
                for (idx, (name, ty)) in field_specs.iter().copied().enumerate() {
                    let head_pos = build_decode_head_pos_expr(body, pos_ident, &field_specs[..idx]);
                    body.decode_field_into(
                        "decode_field_from_prechecked_head",
                        name,
                        ty,
                        input,
                        head_pos,
                    );
                }
                body.return_record_self(&field_names);
            },
        );

        for name in ["decode_from", "decode_from_bounded"] {
            let (generic_params, input_ty) = builder.type_param_with_trait_bound("I", byte_input);
            let with_input_len = name == "decode_from_bounded";
            let mut params = vec![
                builder.param_underscore_named(input_ident, input_ty),
                builder.param_underscore_named(pos_ident, u256_ty),
            ];
            if with_input_len {
                params.push(builder.param_underscore_named(input_len_ident, u256_ty));
            }
            let params = builder.params(params);
            builder.func_with_body_spec(
                FuncBodySpec {
                    name: builder.ident(name),
                    attrs: builder.empty_attrs(),
                    generic_params,
                    params,
                    ret_ty: Some(builder.self_ty()),
                    modifiers,
                },
                |body| {
                    if !with_input_len {
                        body.bind_input_len(input_len_ident, input_ident);
                    }
                    // core::abi::decode_frame_from<Sol, Self, I>(input, pos, input_len)
                    let db = body.db();
                    let callee = body.path_expr(
                        PathId::from_ident(db, body.roots().core)
                            .push_str(db, "abi")
                            .push_str_args(
                                db,
                                "decode_frame_from",
                                GenericArgListId::given_types(
                                    db,
                                    [body.sol_ty(), TypeId::fallback_self_ty(db), input_ty],
                                ),
                            ),
                    );
                    let args = [input_ident, pos_ident, input_len_ident]
                        .map(|ident| body.ident_expr(ident))
                        .to_vec();
                    let call = body.call_expr(callee, args);
                    body.emit_return(Some(call));
                },
            );
        }
    });
}

/// `impl SolCompat for S where F0: SolCompat, ...`: the Solidity type name of
/// the struct is the tuple of its field types, e.g. `(uint8,address)`, so
/// `#[abi]` structs can be event and error fields.
///
/// `S` and `SOL_TYPE` are built from the same element list: punctuation
/// literals typed `std::abi::SolPunctuation` and each field's
/// `<F as SolCompat>::S` and `<F as SolCompat>::SOL_TYPE`, nested in chunks
/// like the event signatures to stay within the `AsBytes` tuple arity.
fn lower_sol_compat_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, AbiStructDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &FieldSpecs<'db>,
) {
    let db = builder.db();
    let sol_compat = TraitRefId::new(
        db,
        Partial::Present(builder.path_from_root(builder.roots().std, &["abi", "SolCompat"])),
    );
    let where_clause = field_bounds(db, field_specs, sol_compat);
    let punctuation_ty =
        builder.ty_path(builder.path_from_root(builder.roots().std, &["abi", "SolPunctuation"]));
    let origin: HirOrigin<ast::Expr> = builder.origin();
    let idx = builder.ctxt().next_impl_trait_idx();
    builder.with_item_scope(TrackedItemVariant::ImplTrait(idx), |builder, id| {
        let ctxt = builder.ctxt();
        let body_id = ctxt.joined_id(TrackedItemVariant::NamelessBody);
        let mut body_ctxt = super::body::BodyCtxt::new(ctxt, body_id);

        // (type, value) of every fragment of the tuple type name.
        let mut elems: Vec<(TypeId<'db>, ExprId)> = Vec::new();
        let punctuation = |body_ctxt: &mut super::body::BodyCtxt<'_, 'db>, text: &str| {
            let lit = Expr::Lit(LitKind::String(StringId::new(db, text.to_string())));
            (punctuation_ty, body_ctxt.push_expr(lit, origin.clone()))
        };
        elems.push(punctuation(&mut body_ctxt, "("));
        for (idx, (_, field_ty)) in field_specs.iter().copied().enumerate() {
            if idx > 0 {
                elems.push(punctuation(&mut body_ctxt, ","));
            }
            let qualified = PathId::new(
                db,
                PathKind::QualifiedType {
                    type_: field_ty,
                    trait_: sol_compat,
                },
                None,
            );
            let ty = TypeId::new(
                db,
                TypeKind::Path(Partial::Present(qualified.push_str(db, "S"))),
            );
            let expr = body_ctxt.push_expr(
                Expr::Path(Partial::Present(qualified.push_str(db, "SOL_TYPE"))),
                origin.clone(),
            );
            elems.push((ty, expr));
        }
        elems.push(punctuation(&mut body_ctxt, ")"));

        // The `AsBytes` tuple impls stop at arity 16; nesting doesn't change
        // the bytes.
        const MAX_TUPLE_ARITY: usize = 16;
        while elems.len() > MAX_TUPLE_ARITY {
            elems = elems
                .chunks(MAX_TUPLE_ARITY)
                .map(|chunk| {
                    if let [single] = chunk {
                        *single
                    } else {
                        let ty = TypeId::new(
                            db,
                            TypeKind::Tuple(TupleTypeId::new(
                                db,
                                chunk
                                    .iter()
                                    .map(|(ty, _)| Partial::Present(*ty))
                                    .collect::<Vec<_>>(),
                            )),
                        );
                        let expr = body_ctxt.push_expr(
                            Expr::Tuple(chunk.iter().map(|(_, expr)| *expr).collect()),
                            origin.clone(),
                        );
                        (ty, expr)
                    }
                })
                .collect();
        }
        let tuple_ty = TypeId::new(
            db,
            TypeKind::Tuple(TupleTypeId::new(
                db,
                elems
                    .iter()
                    .map(|(ty, _)| Partial::Present(*ty))
                    .collect::<Vec<_>>(),
            )),
        );
        let root = body_ctxt.push_expr(
            Expr::Tuple(elems.iter().map(|(_, expr)| *expr).collect()),
            origin.clone(),
        );
        let body = Body::new(
            db,
            body_id,
            root,
            BodyKind::Anonymous,
            body_ctxt.stmts,
            body_ctxt.exprs,
            body_ctxt.conds,
            body_ctxt.pats,
            body_ctxt.f_ctxt.top_mod(),
            body_ctxt.source_map,
            origin.clone(),
        );
        body_ctxt.f_ctxt.leave_item_scope(body);

        let self_s = TypeId::new(
            db,
            TypeKind::Path(Partial::Present(
                PathId::from_ident(db, IdentId::make_self_ty(db)).push_str(db, "S"),
            )),
        );
        let sol_type = AssocConstDef {
            attributes: AttrListId::new(db, vec![]),
            name: Partial::Present(IdentId::new(db, "SOL_TYPE".to_string())),
            ty: Partial::Present(self_s),
            value: Partial::Present(body),
            vis: Visibility::Public,
        };
        let types = vec![builder.assoc_ty("S", Partial::Present(tuple_ty))];
        ImplTrait::new(
            db,
            id,
            Partial::Present(sol_compat),
            Partial::Present(self_ty),
            builder.empty_attrs(),
            builder.empty_generic_params(),
            where_clause,
            types,
            vec![sol_type],
            builder.top_mod(),
            builder.origin(),
        )
    });
}
