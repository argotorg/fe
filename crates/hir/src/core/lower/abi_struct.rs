use parser::ast::{self, prelude::*};
use salsa::Accumulator as _;

use super::{
    FileLowerCtxt,
    attr::{has_named_attr, lower_attrs_without_named, named_attr_specs},
    hir_builder::{BodyBuilder, HirBuilder},
    msg::{
        build_decode_head_pos_expr, create_head_size_assoc_const, create_is_dynamic_assoc_const,
        create_payload_size_func,
    },
};
use crate::{
    ErrorDiagnostic, ErrorDiagnosticKind,
    hir_def::{
        Expr, ExprId, FieldDefListId, FuncModifiers, FuncParam, FuncParamMode, FuncParamName,
        GenericArg, GenericArgListId, GenericParamListId, IdentId, ImplTrait, Partial, Pat, PathId,
        Stmt, Struct, TrackedItemVariant, TraitRefId, TypeBound, TypeGenericArg, TypeId, TypeKind,
        Visibility, WhereClauseId, WherePredicate, expr::CallArg,
    },
    span::AbiStructDesugared,
};

/// Returns true for a struct annotated with `#[abi]`.
pub(super) fn is_abi_struct(ast: &ast::Struct) -> bool {
    has_named_attr(ast.attr_list(), "abi")
}

/// Reports `#[abi]` on a struct that `#[event]` or `#[error]` already encodes.
pub(super) fn report_abi_attr_conflict<'db>(ctxt: &mut FileLowerCtxt<'db>, ast: &ast::Struct) {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);
    for attr in named_attr_specs(ast.attr_list(), "abi") {
        ErrorDiagnostic {
            kind: ErrorDiagnosticKind::AbiAttrConflict,
            file,
            primary_range: attr.range,
            struct_name: ast.name().map(|n| n.text().to_string()),
            field_name: None,
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
    let mut builder = HirBuilder::new(ctxt, desugared);

    let attributes = lower_attrs_without_named(builder.ctxt(), ast.attr_list(), "abi");
    let vis = super::lower_visibility(&ast);
    let generic_params = GenericParamListId::lower_ast_opt(builder.ctxt(), ast.generic_params());
    let where_clause = WhereClauseId::lower_ast_opt(builder.ctxt(), ast.where_clause());
    let fields =
        FieldDefListId::lower_ast_opt_with_context(builder.ctxt(), ast.fields(), "struct field");
    let name = IdentId::lower_token_partial(builder.ctxt(), ast.name());

    let struct_ = builder.struct_item(name, attributes, vis, generic_params, where_clause, fields);

    if !generic_params.data(db).is_empty() {
        ErrorDiagnostic {
            kind: ErrorDiagnosticKind::GenericAbiStruct,
            file,
            primary_range: ast
                .generic_params()
                .map(|g| g.syntax().text_range())
                .unwrap_or_else(|| ast.syntax().text_range()),
            struct_name: ast.name().map(|n| n.text().to_string()),
            field_name: None,
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

    struct_
}

type FieldSpecs<'db> = [(IdentId<'db>, TypeId<'db>)];

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

/// `payload_end` checks the head frame and folds every field's end into the
/// frame end, like the tuple impls in `core::abi`.
fn lower_abi_span_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, AbiStructDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &FieldSpecs<'db>,
) {
    let trait_ref = builder.core_abi_trait_ref_sol("AbiSpan");
    let field_specs = field_specs.to_vec();
    builder.impl_trait(trait_ref, self_ty, |builder| {
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
        let base_ident = builder.ident("base");
        let pos_ident = builder.ident("pos");
        let input_len_ident = builder.ident("input_len");

        for with_input_len in [false, true] {
            let byte_input = builder.core_abi_trait_ref("ByteInput");
            let (generic_params, input_ty) = builder.type_param_with_trait_bound("I", byte_input);
            let mut params = vec![
                builder.param_underscore_named(input_ident, input_ty),
                labeled(base_ident),
                labeled(pos_ident),
            ];
            if with_input_len {
                params.push(labeled(input_len_ident));
            }
            let params = builder.params(params);
            let name = if with_input_len {
                "payload_end_with_input_len"
            } else {
                "payload_end"
            };
            let field_specs = field_specs.clone();
            builder.func_generic_inline_always(
                name,
                generic_params,
                params,
                Some(u256_ty),
                FuncModifiers::new(Visibility::Private, false, false, false),
                |body| {
                    if !with_input_len {
                        body.bind_input_len(input_len_ident, input_ident);
                    }
                    emit_payload_end(
                        body,
                        &field_specs,
                        input_ty,
                        [input_ident, pos_ident, input_len_ident],
                    );
                },
            );
        }
    });
}

fn emit_payload_end<'db>(
    body: &mut BodyBuilder<'_, 'db, AbiStructDesugared>,
    field_specs: &FieldSpecs<'db>,
    input_ty: TypeId<'db>,
    [input_ident, pos_ident, input_len_ident]: [IdentId<'db>; 3],
) {
    let db = body.db();
    let core = body.roots().core;
    let generic_args = |tys: Vec<TypeId<'db>>| {
        GenericArgListId::given(
            db,
            tys.into_iter()
                .map(|ty| {
                    GenericArg::Type(TypeGenericArg {
                        ty: Partial::Present(ty),
                    })
                })
                .collect(),
        )
    };
    let positional = |exprs: Vec<ExprId>| {
        exprs
            .into_iter()
            .map(|expr| CallArg { label: None, expr })
            .collect::<Vec<_>>()
    };

    // let mut __end: u256 = checked_frame_end<Sol>(pos, Self::HEAD_SIZE, input_len)
    let end_ident = IdentId::new(db, "__end".to_string());
    let callee = body.path_expr(
        PathId::from_ident(db, core)
            .push_str(db, "abi")
            .push_str_args(db, "checked_frame_end", generic_args(vec![body.sol_ty()])),
    );
    let args = vec![
        body.ident_expr(pos_ident),
        body.abi_size_assoc_expr(TypeId::fallback_self_ty(db), "HEAD_SIZE"),
        body.ident_expr(input_len_ident),
    ];
    let frame_end = body.call_expr_with_args(callee, positional(args));
    let end_pat = body.push_pat(Pat::Path(
        Partial::Present(PathId::from_ident(db, end_ident)),
        true,
    ));
    let u256_ty = TypeId::new(
        db,
        TypeKind::Path(Partial::Present(PathId::from_ident(
            db,
            IdentId::new(db, "u256".to_string()),
        ))),
    );
    body.emit_stmt(Stmt::Let(end_pat, Some(u256_ty), Some(frame_end)));

    // __end = abi_record_field_end<Sol, F, I>(input, pos, head_pos, input_len, __end)
    for (idx, (_, field_ty)) in field_specs.iter().copied().enumerate() {
        let callee = body.path_expr(
            PathId::from_ident(db, core)
                .push_str(db, "abi")
                .push_str_args(
                    db,
                    "abi_record_field_end",
                    generic_args(vec![body.sol_ty(), field_ty, input_ty]),
                ),
        );
        let args = vec![
            body.ident_expr(input_ident),
            body.ident_expr(pos_ident),
            build_decode_head_pos_expr(body, pos_ident, &field_specs[..idx]),
            body.ident_expr(input_len_ident),
            body.ident_expr(end_ident),
        ];
        let call = body.call_expr_with_args(callee, positional(args));
        let end_place = body.ident_expr(end_ident);
        let assign = body.push_expr(Expr::Assign(end_place, call));
        body.emit_expr_stmt(assign);
    }

    let end = body.ident_expr(end_ident);
    body.emit_return(Some(end));
}

/// `impl StaticAbi for S where F0: StaticAbi, ...`: the struct is static
/// exactly when all of its fields are.
fn lower_static_abi_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, AbiStructDesugared>,
    self_ty: TypeId<'db>,
    field_specs: &FieldSpecs<'db>,
) {
    let db = builder.db();
    let std_root = builder.roots().std;
    let static_abi = || {
        TraitRefId::new(
            db,
            Partial::Present(
                PathId::from_ident(db, std_root)
                    .push_str(db, "abi")
                    .push_str(db, "StaticAbi"),
            ),
        )
    };
    let predicates: Vec<_> = field_specs
        .iter()
        .map(|(_, ty)| WherePredicate {
            ty: Partial::Present(*ty),
            bounds: vec![TypeBound::Trait(static_abi())],
        })
        .collect();
    let where_clause = WhereClauseId::new(db, predicates);
    let idx = builder.ctxt().next_impl_trait_idx();
    builder.with_item_scope(TrackedItemVariant::ImplTrait(idx), |builder, id| {
        ImplTrait::new(
            db,
            id,
            Partial::Present(static_abi()),
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
    });
}
