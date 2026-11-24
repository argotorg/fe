use common::ingot::IngotKind;
use parser::ast::{self, prelude::*};

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
        AssocConstDef, AttrListId, BindingMarker, Body, BodyKind, Expr, ExprId, FieldDefListId,
        FuncModifiers, FuncParam, FuncParamMode, FuncParamName, GenericArgListId, GenericParam,
        GenericParamListId, IdentId, ImplTrait, ImplTraitModifiers, LitKind, ModeMarker, Partial,
        Pat, PathId, PathKind, Stmt, StringId, Struct, TrackedItemVariant, TraitRefId, TupleTypeId,
        TypeBound, TypeGenericParam, TypeId, TypeKind, Visibility, WhereClauseId,
        WhereClausePredicate, WherePredicate, expr::CallArg,
    },
    span::{AbiStructDesugared, DesugaredOrigin, HirOrigin},
};

/// Diagnostics for `#[abi]` struct declarations.
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
        ctxt.report(AbiStructDiagnostic {
            kind: AbiStructDiagnosticKind::AttrConflict,
            file,
            primary_range: attr.range,
        });
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
/// gets `impl AbiSize`, `impl AbiSpan<Sol>`, `impl Encode<Sol>`,
/// `impl Decode<Sol>`, `impl SolCompat`, `impl std::abi::Part` and
/// `impl std::abi::AbiFields`, so it can be used in `msg` fields, return
/// types, arrays and other `#[abi]` structs, and read by field syntax through
/// an `Encoded` handle.
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
        ctxt.report(AbiStructDiagnostic {
            kind: AbiStructDiagnosticKind::GenericStruct,
            file,
            primary_range: ast
                .generic_params()
                .map(|g| g.syntax().text_range())
                .unwrap_or_else(|| ast.syntax().text_range()),
        });
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
    if matches!(
        builder.top_mod().ingot(db).kind(db),
        IngotKind::Core | IngotKind::Std
    ) {
        // Embedded libraries are loaded before package preparation.
        lower_abi_span_impl(&mut builder, self_ty, &field_specs);
        lower_encode_impl(&mut builder, self_ty, &field_specs);
        lower_decode_impl(&mut builder, self_ty, &field_specs);
    } else if let Some(attr) = named_attr_specs(ast.attr_list(), "abi").first() {
        for (slot, trait_ref, provider) in [
            (
                0,
                builder.core_abi_trait_ref_sol("AbiSpan"),
                "StructAbiSpan",
            ),
            (
                1,
                builder.core_abi_trait_ref_sol("Encode"),
                "StructAbiEncode",
            ),
            (
                2,
                builder.core_abi_trait_ref_sol("Decode"),
                "StructAbiDecode",
            ),
        ] {
            super::short_form::lower_derive_slot(
                &mut builder,
                self_ty,
                trait_ref,
                provider,
                (attr.range, slot),
                |_| Vec::new(),
            );
        }
    }
    // Selectors and authored providers can use the layout and type name
    // before any codec bodies have been prepared.
    lower_sol_compat_impl(&mut builder, self_ty, &field_specs);
    lower_part_impl(&mut builder, self_ty);
    lower_abi_fields_impl(&mut builder, self_ty);

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
                bounds: vec![TypeBound::Trait(trait_ref, ModeMarker::None)],
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
            has_mut_prefix: false,
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
        let (generic_params, [input_ty]) =
            builder.type_params_with_trait_bounds([("I", byte_input)]);
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

        let (generic_params, [input_ty]) =
            builder.type_params_with_trait_bounds([("I", byte_input)]);
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
                let callee = body.path_expr(core.push_str(db, "abi").push_str_args(
                    db,
                    "checked_frame_end",
                    GenericArgListId::given_types(db, [body.sol_ty()]),
                ));
                let args = vec![
                    body.ident_expr(pos_ident),
                    body.abi_size_assoc_expr(TypeId::fallback_self_ty(db), "HEAD_SIZE"),
                    body.ident_expr(input_len_ident),
                ];
                let frame_end = body.call_expr(callee, args);
                let end_pat = body.push_pat(Pat::Path(
                    Partial::Present(PathId::from_ident(db, end_ident)),
                    BindingMarker::Var,
                ));
                body.emit_stmt(Stmt::Let(end_pat, Some(u256_ty), Some(frame_end), None));

                // end = abi_record_field_end<Sol, F, I>(input, pos, head_pos, input_len, end)
                for (idx, (_, field_ty)) in field_specs.iter().copied().enumerate() {
                    let callee = body.path_expr(core.push_str(db, "abi").push_str_args(
                        db,
                        "abi_record_field_end",
                        GenericArgListId::given_types(db, [body.sol_ty(), field_ty, input_ty]),
                    ));
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

fn lower_encode_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, AbiStructDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &FieldSpecs<'db>,
) {
    let trait_ref = builder.core_abi_trait_ref_sol("Encode");
    builder.impl_trait(trait_ref, self_ty, |builder| {
        builder.encode_func(field_specs)
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
        let byte_input = builder.core_abi_trait_ref("ByteInput");
        let (generic_params, payload_tys) =
            builder.type_params_with_trait_bounds([("D", abi_decoder), ("I", byte_input)]);
        let decoder_ident = builder.generated_ident("abi_struct_decoder");
        let input_ident = builder.generated_ident("abi_struct_input");
        let params = builder.params([
            builder.param_mut_underscore_named(decoder_ident, payload_tys[0]),
            builder.param_underscore_named(input_ident, payload_tys[1]),
        ]);
        builder.func_generic_inline_always(
            "decode_payload",
            generic_params,
            params,
            Some(builder.self_ty()),
            FuncModifiers::new(Visibility::Private, false, false, false),
            |body| {
                for (idx, (_, ty)) in field_specs.iter().copied().enumerate() {
                    let local = body.field_local(idx);
                    body.decode_into(local, ty, [decoder_ident, input_ident], payload_tys);
                }
                body.return_record_self(&field_names);
            },
        );

        // Decode fields directly at their head positions without a mutable
        // cursor. Check the head once and preserve the bound on dynamic tails.
        // Let the backend budget these generated direct decoders.
        let u256_ty = builder.ty_ident(builder.ident("u256"));
        let pos_ident = builder.generated_ident("abi_struct_pos");
        let input_len_ident = builder.generated_ident("abi_struct_input_len");
        let modifiers = FuncModifiers::new(Visibility::Private, false, false, false);

        let (generic_params, [input_ty]) =
            builder.type_params_with_trait_bounds([("I", byte_input)]);
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
                for (idx, (_, ty)) in field_specs.iter().copied().enumerate() {
                    let head_pos = build_decode_head_pos_expr(body, pos_ident, &field_specs[..idx]);
                    let local = body.field_local(idx);
                    body.decode_field_into(
                        "decode_field_from_prechecked_head",
                        local,
                        ty,
                        input,
                        head_pos,
                    );
                }
                body.return_record_self(&field_names);
            },
        );

        for name in ["decode_from", "decode_from_bounded"] {
            let (generic_params, [input_ty]) =
                builder.type_params_with_trait_bounds([("I", byte_input)]);
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
                    let callee =
                        body.path_expr(body.roots().core.push_str(db, "abi").push_str_args(
                            db,
                            "decode_frame_from",
                            GenericArgListId::given_types(
                                db,
                                [body.sol_ty(), TypeId::fallback_self_ty(db), input_ty],
                            ),
                        ));
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
        let quote_origins = body_ctxt.take_quote_origins();
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
            body_ctxt.f_ctxt.lazy_func(),
            quote_origins,
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
            generated: None,
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
            vec![],
            ImplTraitModifiers::default(),
            builder.top_mod(),
            builder.origin(),
        )
    });
}

/// `impl<I, A> std::abi::Part<I, A> for S where I: ByteInput + Copy, A: Abi`:
/// read as a part of an encoding (a field, an array element or an argument),
/// the record is the handle `Encoded<I, S, A>` that `record_part` makes, so
/// that `h.f` and `h[i]` on an `Encoded` handle reach it.
fn lower_part_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, AbiStructDesugared>,
    self_ty: TypeId<'db>,
) {
    let db = builder.db();
    let roots = builder.roots();
    let [input_ident, abi_ident] = ["I", "A"].map(|name| builder.generated_ident(name));
    let input_ty = builder.ty_ident(input_ident);
    let abi_ty = builder.ty_ident(abi_ident);
    let std_abi = builder.path_from_root(roots.std, &["abi"]);
    let trait_ref = TraitRefId::new(
        db,
        Partial::Present(std_abi.push_str_args(
            db,
            "Part",
            GenericArgListId::given_types(db, [input_ty, abi_ty]),
        )),
    );
    let encoded_ty = builder.ty_path(std_abi.push_str_args(
        db,
        "Encoded",
        GenericArgListId::given_types(db, [input_ty, self_ty, abi_ty]),
    ));
    let generic_params = GenericParamListId::new(
        db,
        [input_ident, abi_ident]
            .map(|name| {
                GenericParam::Type(TypeGenericParam {
                    name: Partial::Present(name),
                    bounds: vec![],
                    default_ty: None,
                })
            })
            .to_vec(),
    );
    let bound = |path: PathId<'db>| {
        TypeBound::Trait(
            TraitRefId::new(db, Partial::Present(path)),
            ModeMarker::None,
        )
    };
    let where_clause = WhereClauseId::new(
        db,
        vec![
            WhereClausePredicate::Type(WherePredicate {
                ty: Partial::Present(input_ty),
                bounds: vec![
                    bound(builder.path_from_root(roots.core, &["abi", "ByteInput"])),
                    bound(builder.path_from_root(roots.core, &["marker", "Copy"])),
                ],
            }),
            WhereClausePredicate::Type(WherePredicate {
                ty: Partial::Present(abi_ty),
                bounds: vec![bound(builder.path_from_root(roots.core, &["abi", "Abi"]))],
            }),
        ],
    );

    let impl_trait_idx = builder.ctxt().next_impl_trait_idx();
    builder.with_item_scope(
        TrackedItemVariant::ImplTrait(impl_trait_idx),
        |builder, id| {
            let types = vec![builder.assoc_ty("Of", Partial::Present(encoded_ty))];
            let impl_trait = builder.new_impl_trait_with_generics(
                id,
                Partial::Present(trait_ref),
                Partial::Present(self_ty),
                generic_params,
                where_clause,
                types,
                vec![],
                builder.origin(),
            );

            let part_ty = builder.ty_path(std_abi.push_str_args(
                db,
                "PartHead",
                GenericArgListId::given_types(db, [input_ty, self_ty]),
            ));
            let part_param = builder.ident("part");
            let params = builder.params([builder.param_underscore_named(part_param, part_ty)]);
            builder.func_with_body_spec(
                FuncBodySpec {
                    name: builder.ident("read"),
                    attrs: builder.empty_attrs(),
                    generic_params: builder.empty_generic_params(),
                    params,
                    ret_ty: Some(encoded_ty),
                    modifiers: FuncModifiers::new(Visibility::Private, false, false, false),
                },
                |body| {
                    let db = body.db();
                    let callee = body.path_expr(
                        body.roots()
                            .std
                            .push_str(db, "abi")
                            .push_str(db, "record_part"),
                    );
                    let part = body.ident_expr(part_param);
                    let call = body.call_expr(callee, vec![part]);
                    body.emit_return(Some(call));
                },
            );
            impl_trait
        },
    );
}

/// `impl std::abi::AbiFields for S {}`: the record is encoded as the tuple of
/// its fields in declaration order, which is what field syntax on an
/// `Encoded` handle relies on (see `std::abi::AbiFields`).
pub(super) fn lower_abi_fields_impl<'db, O: Clone + Into<DesugaredOrigin>>(
    builder: &mut HirBuilder<'_, 'db, O>,
    self_ty: TypeId<'db>,
) {
    let db = builder.db();
    let trait_ref = TraitRefId::new(
        db,
        Partial::Present(builder.path_from_root(builder.roots().std, &["abi", "AbiFields"])),
    );
    builder.impl_trait(trait_ref, self_ty, |_| {});
}
