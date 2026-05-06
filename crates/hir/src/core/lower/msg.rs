use num_bigint::BigUint;
use parser::ast::{self, AttrListOwner as _};
use salsa::Accumulator as _;

use super::{attr::named_attr_specs, hir_builder::HirBuilder};

use crate::{
    HirDb, SelectorError, SelectorErrorKind,
    hir_def::{
        ArithBinOp, AssocConstDef, AttrListId, BinOp, Body, BodyKind, CompBinOp, Expr, FieldDef,
        FieldDefListId, FieldIndex, FuncModifiers, FuncParam, FuncParamMode, FuncParamName,
        IdentId, ImplTrait, IntegerId, LitKind, LogicalBinOp, Mod, Partial, Pat, PathId, PathKind,
        Stmt, Struct, TrackedItemVariant, TraitRefId, TupleTypeId, TypeId, TypeKind, Visibility,
    },
    lower::FileLowerCtxt,
    span::{MsgDesugared, MsgDesugaredFocus},
};

use super::body::BodyCtxt;

/// Desugars a `msg` block into a module containing structs and trait impls.
///
/// ```fe
/// msg Erc20 {
///   #[selector = 0x1234]
///   Transfer { to: Address, amount: u256 } -> bool
/// }
/// ```
///
/// becomes:
///
/// ```fe
/// #[msg]
/// mod Erc20 {
///   pub struct Transfer { pub to: Address, pub amount: u256 }
///   impl MsgVariant for Transfer {
///     const SELECTOR: u32 = 0x1234
///     type Return = bool
///   }
/// }
/// ```
pub(super) fn lower_msg_as_mod<'db>(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Msg) -> Mod<'db> {
    let name = IdentId::lower_token_partial(ctxt, ast.name());

    // Lower any existing attributes on the msg block
    let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());

    let vis = super::lower_visibility(&ast);

    // Create the desugared origin pointing to the original msg AST
    let msg_desugared = MsgDesugared {
        msg: parser::ast::AstPtr::new(&ast),
        variant_idx: None,
        focus: Default::default(),
    };

    let mut builder = HirBuilder::new(ctxt, msg_desugared);
    builder.desugared_mod(name, attributes, vis, |builder| {
        if let Some(variants) = ast.variants() {
            for (idx, variant) in variants.into_iter().enumerate() {
                lower_msg_variant(builder, &ast, idx, variant);
            }
        }
    })
}

/// Lowers a single msg variant to a struct and an impl MsgVariant block.
fn lower_msg_variant<'db>(
    builder: &mut HirBuilder<'_, 'db, MsgDesugared>,
    msg_ast: &ast::Msg,
    variant_idx: usize,
    variant: ast::MsgVariant,
) {
    let mut builder = builder.with_desugared(MsgDesugared {
        msg: parser::ast::AstPtr::new(msg_ast),
        variant_idx: Some(variant_idx),
        focus: MsgDesugaredFocus::VariantName,
    });

    // Create the struct for this variant
    let struct_ = lower_msg_variant_struct(&mut builder, &variant);

    // Create the impl MsgVariant for this variant
    lower_msg_variant_impl(&mut builder, &variant, struct_);

    // Create `impl Encode<Sol> for Variant` and `impl Decode<Sol> for Variant`.
    lower_msg_variant_encode_decode_impls(&mut builder, &variant, struct_);
}

fn variant_struct_ty<'db>(db: &'db dyn HirDb, struct_: Struct<'db>) -> TypeId<'db> {
    let struct_name = struct_.name(db);
    let self_type_path = match struct_name.to_opt() {
        Some(name) => PathId::from_ident(db, name),
        None => PathId::from_ident(db, IdentId::new(db, "_".to_string())),
    };
    TypeId::new(db, TypeKind::Path(Partial::Present(self_type_path)))
}

fn lower_msg_variant_field_specs<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    variant: &ast::MsgVariant,
) -> Vec<(IdentId<'db>, TypeId<'db>)> {
    let mut fields = Vec::new();
    if let Some(params_ast) = variant.params() {
        for field in params_ast.into_iter() {
            let Some(name_token) = field.name() else {
                continue;
            };
            let field_name = IdentId::lower_token(ctxt, name_token);
            let Some(field_ty) = TypeId::lower_ast_partial(ctxt, field.ty()).to_opt() else {
                continue;
            };
            fields.push((field_name, field_ty));
        }
    }
    fields
}

fn lower_msg_variant_encode_decode_impls<'db>(
    builder: &mut HirBuilder<'_, 'db, MsgDesugared>,
    variant: &ast::MsgVariant,
    struct_: Struct<'db>,
) {
    lower_msg_variant_abi_size_impl(builder, variant, struct_);
    lower_msg_variant_encode_impl(builder, variant, struct_);
    lower_msg_variant_decode_trait_impl(builder, variant, struct_);
    lower_msg_variant_abi_validate_impl(builder, variant, struct_);
}

fn lower_msg_variant_abi_size_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, MsgDesugared>,
    variant: &ast::MsgVariant,
    struct_: Struct<'db>,
) -> ImplTrait<'db> {
    let db = builder.db();
    let roots = builder.roots();
    let field_specs = lower_msg_variant_field_specs(builder.ctxt(), variant);
    let trait_path = PathId::from_ident(db, roots.core)
        .push_str(db, "abi")
        .push_str(db, "AbiSize");
    let trait_ref = Partial::Present(TraitRefId::new(db, Partial::Present(trait_path)));
    let ty = Partial::Present(variant_struct_ty(db, struct_));
    let impl_trait_idx = builder.ctxt().next_impl_trait_idx();
    builder.with_item_scope(
        TrackedItemVariant::ImplTrait(impl_trait_idx),
        |builder, id| {
            let consts = vec![
                create_head_size_assoc_const(builder, &field_specs),
                create_is_dynamic_assoc_const(builder, &field_specs),
            ];
            let impl_trait =
                builder.new_impl_trait(id, trait_ref, ty, vec![], consts, builder.origin());
            let payload_size_ident = builder.ident("payload_size");
            let params = builder.params([FuncParam {
                mode: FuncParamMode::View,
                is_mut: false,
                has_ref_prefix: false,
                has_own_prefix: false,
                is_label_suppressed: false,
                name: Partial::Present(FuncParamName::Ident(IdentId::make_self(builder.db()))),
                ty: Partial::Present(builder.self_ty()),
                self_ty_fallback: true,
            }]);
            let ret_ty = builder.ty_ident(builder.ident("u256"));
            let roots = builder.roots();
            builder.func_with_body(
                payload_size_ident,
                builder.empty_generic_params(),
                params,
                Some(ret_ty),
                FuncModifiers::new(Visibility::Private, false, false, false),
                |body| {
                    let db = body.db();
                    let self_expr = body.path_expr(PathId::from_ident(db, IdentId::make_self(db)));
                    let dynamic_payload_size_path = PathId::from_ident(db, roots.core)
                        .push_str(db, "abi")
                        .push_str(db, "dynamic_payload_size");
                    let mut expr =
                        body.abi_size_assoc_expr(TypeId::fallback_self_ty(db), "HEAD_SIZE");

                    for (field_name, _) in field_specs.iter().copied() {
                        let field_expr = body.push_expr(Expr::Field(
                            self_expr,
                            Partial::Present(FieldIndex::Ident(field_name)),
                        ));
                        let dynamic_payload_size = body.path_expr(dynamic_payload_size_path);
                        let field_size = body.call_expr(dynamic_payload_size, vec![field_expr]);
                        expr = body.push_expr(Expr::Bin(
                            expr,
                            field_size,
                            BinOp::Arith(ArithBinOp::Add),
                        ));
                    }

                    body.emit_return(Some(expr));
                },
            );
            impl_trait
        },
    )
}

fn lower_msg_variant_encode_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, MsgDesugared>,
    variant: &ast::MsgVariant,
    struct_: Struct<'db>,
) -> ImplTrait<'db> {
    let field_specs = lower_msg_variant_field_specs(builder.ctxt(), variant);

    let impl_trait_idx = builder.ctxt().next_impl_trait_idx();
    let trait_ref = Partial::Present(builder.core_abi_trait_ref_sol("Encode"));
    let ty = Partial::Present(variant_struct_ty(builder.db(), struct_));
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
    )
}

pub(super) fn create_direct_encode_assoc_const<
    'db,
    O: Clone + Into<crate::span::DesugaredOrigin>,
>(
    builder: &mut HirBuilder<'_, 'db, O>,
    fields: &[(IdentId<'db>, TypeId<'db>)],
) -> AssocConstDef<'db> {
    let db = builder.db();
    let name = builder.ident("DIRECT_ENCODE");
    let ty = builder.ty_ident(builder.ident("bool"));
    let encode_trait = builder.core_abi_trait_ref_sol("Encode");
    let id = builder.ctxt().joined_id(TrackedItemVariant::NamelessBody);
    let origin = builder.origin();
    let mut body_ctxt = BodyCtxt::new(builder.ctxt(), id);
    let mut expr = push_bool_expr(&mut body_ctxt, origin.clone(), true);
    for (_, field_ty) in fields.iter().copied() {
        let field_expr =
            build_direct_encode_expr(&mut body_ctxt, origin.clone(), encode_trait, field_ty);
        expr = body_ctxt.push_expr(
            Expr::Bin(expr, field_expr, BinOp::Logical(LogicalBinOp::And)),
            origin.clone(),
        );
    }
    let body = body_ctxt.build(None, expr, BodyKind::Anonymous);

    AssocConstDef {
        attributes: AttrListId::new(db, vec![]),
        name: Partial::Present(name),
        ty: Partial::Present(ty),
        value: Partial::Present(body),
    }
}

fn build_direct_encode_expr<'db>(
    body_ctxt: &mut BodyCtxt<'_, 'db>,
    origin: crate::span::HirOrigin<ast::Expr>,
    encode_trait: TraitRefId<'db>,
    ty: TypeId<'db>,
) -> crate::hir_def::ExprId {
    let db = body_ctxt.f_ctxt.db();

    match ty.data(db) {
        TypeKind::Path(path) => path
            .to_opt()
            .map(|_| {
                let qualified = PathId::new(
                    db,
                    PathKind::QualifiedType {
                        type_: ty,
                        trait_: encode_trait,
                    },
                    None,
                );
                body_ctxt.push_expr(
                    Expr::Path(Partial::Present(qualified.push_str(db, "DIRECT_ENCODE"))),
                    origin.clone(),
                )
            })
            .unwrap_or_else(|| push_bool_expr(body_ctxt, origin.clone(), false)),
        TypeKind::Tuple(tuple) => {
            let mut expr = push_bool_expr(body_ctxt, origin.clone(), true);
            for elem_ty in tuple.data(db).iter().copied() {
                let elem_expr = elem_ty
                    .to_opt()
                    .map(|elem_ty| {
                        build_direct_encode_expr(body_ctxt, origin.clone(), encode_trait, elem_ty)
                    })
                    .unwrap_or_else(|| push_bool_expr(body_ctxt, origin.clone(), false));
                expr = body_ctxt.push_expr(
                    Expr::Bin(expr, elem_expr, BinOp::Logical(LogicalBinOp::And)),
                    origin.clone(),
                );
            }
            expr
        }
        TypeKind::Mode(_, inner) => inner
            .to_opt()
            .map(|inner| build_direct_encode_expr(body_ctxt, origin.clone(), encode_trait, inner))
            .unwrap_or_else(|| push_bool_expr(body_ctxt, origin.clone(), false)),
        TypeKind::Array(elem_ty, _) => elem_ty
            .to_opt()
            .map(|elem_ty| {
                build_direct_encode_expr(body_ctxt, origin.clone(), encode_trait, elem_ty)
            })
            .unwrap_or_else(|| push_bool_expr(body_ctxt, origin.clone(), false)),
        TypeKind::Ptr(_) | TypeKind::Never => push_bool_expr(body_ctxt, origin, false),
    }
}

fn push_bool_expr<'db>(
    body_ctxt: &mut BodyCtxt<'_, 'db>,
    origin: crate::span::HirOrigin<ast::Expr>,
    value: bool,
) -> crate::hir_def::ExprId {
    body_ctxt.push_expr(Expr::Lit(LitKind::Bool(value)), origin)
}

fn push_int_expr<'db>(
    body_ctxt: &mut BodyCtxt<'_, 'db>,
    origin: crate::span::HirOrigin<ast::Expr>,
    value: u64,
) -> crate::hir_def::ExprId {
    let db = body_ctxt.f_ctxt.db();
    let value = BigUint::from(value);
    body_ctxt.push_expr(Expr::Lit(LitKind::Int(IntegerId::new(db, value))), origin)
}

fn abi_size_trait_ref<'db>(ctxt: &FileLowerCtxt<'db>) -> TraitRefId<'db> {
    let db = ctxt.db();
    let roots = super::hir_builder::LibRoots::for_ctxt(ctxt);
    let path = PathId::from_ident(db, roots.core)
        .push_str(db, "abi")
        .push_str(db, "AbiSize");
    TraitRefId::new(db, Partial::Present(path))
}

fn abi_size_assoc_expr<'db>(
    body_ctxt: &mut BodyCtxt<'_, 'db>,
    origin: crate::span::HirOrigin<ast::Expr>,
    ty: TypeId<'db>,
    assoc_name: &str,
) -> crate::hir_def::ExprId {
    let db = body_ctxt.f_ctxt.db();
    let qualified = PathId::new(
        db,
        PathKind::QualifiedType {
            type_: ty,
            trait_: abi_size_trait_ref(body_ctxt.f_ctxt),
        },
        None,
    );
    body_ctxt.push_expr(
        Expr::Path(Partial::Present(qualified.push_str(db, assoc_name))),
        origin,
    )
}

pub(super) fn create_is_dynamic_assoc_const<'db, O: Clone + Into<crate::span::DesugaredOrigin>>(
    builder: &mut HirBuilder<'_, 'db, O>,
    fields: &[(IdentId<'db>, TypeId<'db>)],
) -> AssocConstDef<'db> {
    create_bool_assoc_const(builder, "IS_DYNAMIC", fields)
}

fn create_bool_assoc_const<'db, O: Clone + Into<crate::span::DesugaredOrigin>>(
    builder: &mut HirBuilder<'_, 'db, O>,
    name: &str,
    fields: &[(IdentId<'db>, TypeId<'db>)],
) -> AssocConstDef<'db> {
    let db = builder.db();
    let name = builder.ident(name);
    let ty = builder.ty_ident(builder.ident("bool"));
    let id = builder.ctxt().joined_id(TrackedItemVariant::NamelessBody);
    let origin = builder.origin();
    let mut body_ctxt = BodyCtxt::new(builder.ctxt(), id);
    let mut expr = push_bool_expr(&mut body_ctxt, origin.clone(), false);

    for (_, field_ty) in fields.iter().copied() {
        let field_is_dynamic = build_is_dynamic_expr(&mut body_ctxt, origin.clone(), field_ty);
        expr = body_ctxt.push_expr(
            Expr::Bin(expr, field_is_dynamic, BinOp::Logical(LogicalBinOp::Or)),
            origin.clone(),
        );
    }

    let body = body_ctxt.build(None, expr, BodyKind::Anonymous);
    AssocConstDef {
        attributes: AttrListId::new(db, vec![]),
        name: Partial::Present(name),
        ty: Partial::Present(ty),
        value: Partial::Present(body),
    }
}

fn build_is_dynamic_expr<'db>(
    body_ctxt: &mut BodyCtxt<'_, 'db>,
    origin: crate::span::HirOrigin<ast::Expr>,
    ty: TypeId<'db>,
) -> crate::hir_def::ExprId {
    let db = body_ctxt.f_ctxt.db();

    match ty.data(db) {
        TypeKind::Path(path) => path
            .to_opt()
            .map(|path| {
                body_ctxt.push_expr(
                    Expr::Path(Partial::Present(path.push_str(db, "IS_DYNAMIC"))),
                    origin.clone(),
                )
            })
            .unwrap_or_else(|| push_bool_expr(body_ctxt, origin.clone(), false)),
        TypeKind::Tuple(tuple) => {
            let mut expr = push_bool_expr(body_ctxt, origin.clone(), false);
            for elem_ty in tuple.data(db).iter().copied() {
                let elem_expr = elem_ty
                    .to_opt()
                    .map(|elem_ty| build_is_dynamic_expr(body_ctxt, origin.clone(), elem_ty))
                    .unwrap_or_else(|| push_bool_expr(body_ctxt, origin.clone(), false));
                expr = body_ctxt.push_expr(
                    Expr::Bin(expr, elem_expr, BinOp::Logical(LogicalBinOp::Or)),
                    origin.clone(),
                );
            }
            expr
        }
        TypeKind::Mode(_, inner) => inner
            .to_opt()
            .map(|inner| build_is_dynamic_expr(body_ctxt, origin.clone(), inner))
            .unwrap_or_else(|| push_bool_expr(body_ctxt, origin.clone(), false)),
        TypeKind::Array(elem_ty, _) => elem_ty
            .to_opt()
            .map(|elem_ty| build_is_dynamic_expr(body_ctxt, origin.clone(), elem_ty))
            .unwrap_or_else(|| push_bool_expr(body_ctxt, origin.clone(), false)),
        TypeKind::Ptr(_) | TypeKind::Never => push_bool_expr(body_ctxt, origin, false),
    }
}

pub(super) fn create_head_size_assoc_const<'db, O: Clone + Into<crate::span::DesugaredOrigin>>(
    builder: &mut HirBuilder<'_, 'db, O>,
    fields: &[(IdentId<'db>, TypeId<'db>)],
) -> AssocConstDef<'db> {
    let db = builder.db();
    let name = builder.ident("HEAD_SIZE");
    let ty = builder.ty_ident(builder.ident("u256"));
    let id = builder.ctxt().joined_id(TrackedItemVariant::NamelessBody);
    let origin = builder.origin();
    let mut body_ctxt = BodyCtxt::new(builder.ctxt(), id);
    let mut expr = push_int_expr(&mut body_ctxt, origin.clone(), 0);

    for (_, field_ty) in fields.iter().copied() {
        let field_size = build_head_size_expr(&mut body_ctxt, origin.clone(), field_ty);
        expr = body_ctxt.push_expr(
            Expr::Bin(expr, field_size, BinOp::Arith(ArithBinOp::Add)),
            origin.clone(),
        );
    }

    let body = body_ctxt.build(None, expr, BodyKind::Anonymous);
    AssocConstDef {
        attributes: AttrListId::new(db, vec![]),
        name: Partial::Present(name),
        ty: Partial::Present(ty),
        value: Partial::Present(body),
    }
}

fn build_head_size_expr<'db>(
    body_ctxt: &mut BodyCtxt<'_, 'db>,
    origin: crate::span::HirOrigin<ast::Expr>,
    ty: TypeId<'db>,
) -> crate::hir_def::ExprId {
    let db = body_ctxt.f_ctxt.db();

    match ty.data(db) {
        TypeKind::Path(path) => path
            .to_opt()
            .map(|path| {
                body_ctxt.push_expr(
                    Expr::Path(Partial::Present(path.push_str(db, "HEAD_SIZE"))),
                    origin.clone(),
                )
            })
            .unwrap_or_else(|| push_int_expr(body_ctxt, origin.clone(), 0)),
        TypeKind::Tuple(tuple) => {
            let mut expr = push_int_expr(body_ctxt, origin.clone(), 0);
            for elem_ty in tuple.data(db).iter().copied() {
                let elem_expr = elem_ty
                    .to_opt()
                    .map(|elem_ty| build_head_size_expr(body_ctxt, origin.clone(), elem_ty))
                    .unwrap_or_else(|| push_int_expr(body_ctxt, origin.clone(), 0));
                expr = body_ctxt.push_expr(
                    Expr::Bin(expr, elem_expr, BinOp::Arith(ArithBinOp::Add)),
                    origin.clone(),
                );
            }
            expr
        }
        TypeKind::Mode(_, inner) => inner
            .to_opt()
            .map(|inner| build_head_size_expr(body_ctxt, origin.clone(), inner))
            .unwrap_or_else(|| push_int_expr(body_ctxt, origin.clone(), 0)),
        TypeKind::Array(_, _) => abi_size_assoc_expr(body_ctxt, origin, ty, "HEAD_SIZE"),
        TypeKind::Ptr(_) | TypeKind::Never => push_int_expr(body_ctxt, origin, 0),
    }
}

pub(super) fn build_head_size_body_expr<'db, O: Clone + Into<crate::span::DesugaredOrigin>>(
    body: &mut super::hir_builder::BodyBuilder<'_, 'db, O>,
    ty: TypeId<'db>,
) -> crate::hir_def::ExprId {
    let db = body.db();

    match ty.data(db) {
        TypeKind::Path(path) => path
            .to_opt()
            .map(|path| body.path_expr(path.push_str(db, "HEAD_SIZE")))
            .unwrap_or_else(|| {
                body.push_expr(Expr::Lit(LitKind::Int(IntegerId::from_usize(db, 0))))
            }),
        TypeKind::Tuple(tuple) => {
            let mut expr = body.push_expr(Expr::Lit(LitKind::Int(IntegerId::from_usize(db, 0))));
            for elem_ty in tuple.data(db).iter().copied() {
                let elem_expr = elem_ty
                    .to_opt()
                    .map(|elem_ty| build_head_size_body_expr(body, elem_ty))
                    .unwrap_or_else(|| {
                        body.push_expr(Expr::Lit(LitKind::Int(IntegerId::from_usize(db, 0))))
                    });
                expr = body.push_expr(Expr::Bin(expr, elem_expr, BinOp::Arith(ArithBinOp::Add)));
            }
            expr
        }
        TypeKind::Mode(_, inner) => inner
            .to_opt()
            .map(|inner| build_head_size_body_expr(body, inner))
            .unwrap_or_else(|| {
                body.push_expr(Expr::Lit(LitKind::Int(IntegerId::from_usize(db, 0))))
            }),
        TypeKind::Array(_, _) => body.abi_size_assoc_expr(ty, "HEAD_SIZE"),
        TypeKind::Ptr(_) | TypeKind::Never => {
            body.push_expr(Expr::Lit(LitKind::Int(IntegerId::from_usize(db, 0))))
        }
    }
}

fn lower_msg_variant_decode_trait_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, MsgDesugared>,
    variant: &ast::MsgVariant,
    struct_: Struct<'db>,
) -> ImplTrait<'db> {
    let db = builder.db();
    let fields = lower_msg_variant_field_specs(builder.ctxt(), variant);
    let field_names = fields.iter().map(|(name, _)| *name).collect::<Vec<_>>();

    let trait_ref = builder.core_abi_trait_ref_sol("Decode");
    let ty = variant_struct_ty(db, struct_);

    builder.impl_trait(trait_ref, ty, |builder| {
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
    })
}

fn lower_msg_variant_abi_validate_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, MsgDesugared>,
    variant: &ast::MsgVariant,
    struct_: Struct<'db>,
) -> ImplTrait<'db> {
    let db = builder.db();
    let fields = lower_msg_variant_field_specs(builder.ctxt(), variant);

    let trait_ref = builder.core_abi_trait_ref_sol("AbiValidate");
    let ty = variant_struct_ty(db, struct_);

    builder.impl_trait(trait_ref, ty, |builder| {
        let byte_input_trait_path = PathId::from_ident(db, builder.roots().core)
            .push_str(db, "abi")
            .push_str(db, "ByteInput");
        let byte_input_trait_ref = TraitRefId::new(db, Partial::Present(byte_input_trait_path));
        let (i_generic_params, i_ty) =
            builder.type_param_with_trait_bound("I", byte_input_trait_ref);

        let input_ident = builder.ident("input");
        let base_ident = builder.ident("base");
        let pos_ident = builder.ident("pos");

        let u256_ty = builder.ty_ident(builder.ident("u256"));
        let params = builder.params([
            FuncParam {
                mode: FuncParamMode::View,
                is_mut: false,
                has_ref_prefix: false,
                has_own_prefix: false,
                is_label_suppressed: true,
                name: Partial::Present(FuncParamName::Ident(input_ident)),
                ty: Partial::Present(i_ty),
                self_ty_fallback: false,
            },
            FuncParam {
                mode: FuncParamMode::View,
                is_mut: false,
                has_ref_prefix: false,
                has_own_prefix: false,
                is_label_suppressed: false,
                name: Partial::Present(FuncParamName::Ident(base_ident)),
                ty: Partial::Present(u256_ty),
                self_ty_fallback: false,
            },
            FuncParam {
                mode: FuncParamMode::View,
                is_mut: false,
                has_ref_prefix: false,
                has_own_prefix: false,
                is_label_suppressed: false,
                name: Partial::Present(FuncParamName::Ident(pos_ident)),
                ty: Partial::Present(u256_ty),
                self_ty_fallback: false,
            },
        ]);

        builder.func_generic(
            "validate_end",
            i_generic_params,
            params,
            Some(u256_ty),
            FuncModifiers::new(Visibility::Private, false, false, false),
            |body| {
                let db = body.db();
                let base = body.ident_expr(base_ident);
                body.emit_expr_stmt(base);

                let input = body.ident_expr(input_ident);
                let pos = body.ident_expr(pos_ident);

                if fields.is_empty() {
                    body.emit_return(Some(pos));
                    return;
                }

                let head_pos_ident = IdentId::new(db, "head_pos".to_string());
                let end_ident = IdentId::new(db, "end".to_string());
                let bind = |body: &mut super::hir_builder::BodyBuilder<'_, 'db, MsgDesugared>,
                            ident: IdentId<'db>,
                            expr: crate::hir_def::ExprId| {
                    let pat = body.push_pat(Pat::Path(
                        Partial::Present(PathId::from_ident(db, ident)),
                        false,
                    ));
                    body.emit_stmt(Stmt::Let(pat, None, Some(expr)));
                };

                bind(body, head_pos_ident, pos);
                bind(body, end_ident, pos);

                // For static-only variants, `IS_DYNAMIC` is false at compile-time.
                // Use it to prune max(end, head_end) bookkeeping from passthrough validation.
                let is_dynamic_ident = IdentId::new(db, "is_dynamic".to_string());
                let is_dynamic_expr = body.abi_size_assoc_expr(ty, "IS_DYNAMIC");
                bind(body, is_dynamic_ident, is_dynamic_expr);

                for (idx, (_name, field_ty)) in fields.iter().copied().enumerate() {
                    let head_pos = body.ident_expr(head_pos_ident);
                    let field_end =
                        body.abi_field_validate_end_expr(field_ty, i_ty, input, pos, head_pos);
                    let field_end_ident = IdentId::new(db, format!("field_end_{idx}"));
                    bind(body, field_end_ident, field_end);

                    let end = body.ident_expr(end_ident);
                    let field_end = body.ident_expr(field_end_ident);
                    let cond =
                        body.push_expr(Expr::Bin(field_end, end, BinOp::Comp(CompBinOp::Gt)));
                    let max_end = body.if_expr(cond, field_end, end);
                    let is_dynamic = body.ident_expr(is_dynamic_ident);
                    let next_end = body.if_expr(is_dynamic, max_end, end);
                    bind(body, end_ident, next_end);

                    let head_pos = body.ident_expr(head_pos_ident);
                    let slot = body.abi_field_head_size_expr(field_ty);
                    let next_head_pos =
                        body.push_expr(Expr::Bin(head_pos, slot, BinOp::Arith(ArithBinOp::Add)));
                    bind(body, head_pos_ident, next_head_pos);
                }

                let end = body.ident_expr(end_ident);
                let head_pos = body.ident_expr(head_pos_ident);
                let cond = body.push_expr(Expr::Bin(head_pos, end, BinOp::Comp(CompBinOp::Gt)));
                let ret_dynamic = body.if_expr(cond, head_pos, end);
                let is_dynamic = body.ident_expr(is_dynamic_ident);
                let ret = body.if_expr(is_dynamic, ret_dynamic, head_pos);
                body.emit_return(Some(ret));
            },
        );
    })
}

/// Creates a struct from a msg variant.
fn lower_msg_variant_struct<'db>(
    builder: &mut HirBuilder<'_, 'db, MsgDesugared>,
    variant: &ast::MsgVariant,
) -> Struct<'db> {
    let name = IdentId::lower_token_partial(builder.ctxt(), variant.name());
    super::payable::report_payable_attr_on_msg_variant(builder.ctxt(), variant.attr_list());
    let attributes = filter_msg_variant_attrs(builder.ctxt(), variant.attr_list());
    let fields = lower_msg_variant_fields(builder.ctxt(), variant.params());
    builder.pub_struct(name, attributes, fields)
}

/// Lowers msg variant params to field definitions, making all fields public.
fn lower_msg_variant_fields<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    params: Option<ast::MsgVariantParams>,
) -> FieldDefListId<'db> {
    let db = ctxt.db();
    match params {
        Some(params) => {
            let fields = params
                .into_iter()
                .map(|field| {
                    super::payable::report_payable_attr_on_unsupported_item(
                        ctxt,
                        field.attr_list(),
                        "field",
                    );
                    let attributes = AttrListId::lower_ast_opt(ctxt, field.attr_list());
                    let name = IdentId::lower_token_partial(ctxt, field.name());
                    let type_ref = TypeId::lower_ast_partial(ctxt, field.ty());
                    // All msg variant fields are public
                    FieldDef::new(attributes, name, type_ref, Visibility::Public)
                })
                .collect::<Vec<_>>();
            FieldDefListId::new(db, fields)
        }
        None => FieldDefListId::new(db, vec![]),
    }
}

/// Creates an `impl MsgVariant for VariantStruct` block.
fn lower_msg_variant_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, MsgDesugared>,
    variant: &ast::MsgVariant,
    struct_: Struct<'db>,
) -> ImplTrait<'db> {
    let db = builder.db();
    let roots = builder.roots();
    let abi_args = builder.sol_args();

    let msg_variant_trait_path = PathId::from_ident(db, roots.core)
        .push_str(db, "message")
        .push_str_args(db, "MsgVariant", abi_args);
    let trait_ref = TraitRefId::new(db, Partial::Present(msg_variant_trait_path));

    let ty = variant_struct_ty(db, struct_);

    let variant_name = variant
        .name()
        .map(|t| t.text().to_string())
        .unwrap_or_default();

    builder.impl_trait_assocs_build(trait_ref, ty, |builder| {
        let return_ty = match variant.ret_ty() {
            Some(ret_ty) => TypeId::lower_ast_partial(builder.ctxt(), Some(ret_ty)),
            None => Partial::Present(TypeId::new(
                db,
                TypeKind::Tuple(TupleTypeId::new(db, vec![])),
            )),
        };
        let types = vec![builder.assoc_ty("Return", return_ty)];
        let consts = vec![create_selector_const(
            builder.ctxt(),
            variant,
            &variant_name,
        )];
        (types, consts)
    })
}

/// Result of parsing a selector attribute from an AST.
struct ParsedSelector {
    /// The expression from the selector attribute, if present and in `#[selector = <expr>]` form.
    expr: Option<ast::Expr>,
    /// The text range of the selector attribute for diagnostics.
    range: parser::TextRange,
    /// The error kind, if validation failed.
    error: Option<SelectorErrorKind>,
}

/// Creates the `SELECTOR` associated const from the variant's `#[selector = ...]` attribute.
///
/// The selector value is lowered as a const expression body, which is evaluated later by CTFE.
fn create_selector_const<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    variant: &ast::MsgVariant,
    variant_name: &str,
) -> AssocConstDef<'db> {
    use parser::ast::prelude::*;

    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);
    let selector_name = IdentId::new(db, "SELECTOR".to_string());
    let selector_ty = TypeId::new(
        db,
        TypeKind::Path(Partial::Present(PathId::from_ident(
            db,
            IdentId::new(db, "u32".to_string()),
        ))),
    );

    let parsed = variant
        .attr_list()
        .and_then(|attr_list| parse_selector_attr(ctxt, attr_list));

    let body = match parsed {
        Some(parsed) => {
            if let Some(error_kind) = parsed.error {
                SelectorError {
                    kind: error_kind,
                    file,
                    primary_range: parsed.range,
                    secondary_range: None,
                    variant_name: variant_name.to_string(),
                }
                .accumulate(db);
                Body::lower_ast_with_variant(
                    ctxt,
                    None,
                    TrackedItemVariant::NamelessBody,
                    BodyKind::Anonymous,
                )
            } else if let Some(expr) = parsed.expr {
                Body::lower_ast_nameless(ctxt, expr)
            } else {
                Body::lower_ast_with_variant(
                    ctxt,
                    None,
                    TrackedItemVariant::NamelessBody,
                    BodyKind::Anonymous,
                )
            }
        }
        None => {
            let variant_range = variant
                .name()
                .map(|n| n.text_range())
                .unwrap_or_else(|| variant.syntax().text_range());
            SelectorError {
                kind: SelectorErrorKind::Missing,
                file,
                primary_range: variant_range,
                secondary_range: None,
                variant_name: variant_name.to_string(),
            }
            .accumulate(db);
            Body::lower_ast_with_variant(
                ctxt,
                None,
                TrackedItemVariant::NamelessBody,
                BodyKind::Anonymous,
            )
        }
    };

    AssocConstDef {
        attributes: AttrListId::new(db, vec![]),
        name: Partial::Present(selector_name),
        ty: Partial::Present(selector_ty),
        value: Partial::Present(body),
    }
}

/// Parses a `#[selector = <value>]` attribute.
/// Returns None if no selector attribute found.
/// Returns an error if the attribute uses the wrong form (e.g. `#[selector(value)]`).
fn parse_selector_attr<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attr_list: ast::AttrList,
) -> Option<ParsedSelector> {
    use crate::hir_def::LitKind;
    use num_bigint::BigUint;

    for attr in named_attr_specs(Some(attr_list), "selector") {
        let range = attr.range;
        if let Some(value) = attr.value {
            let expr = match value {
                ast::AttrArgValueKind::Expr(expr) => Some(expr),
                _ => None,
            };

            let error = match &expr {
                Some(expr) => match expr.kind() {
                    ast::ExprKind::Lit(lit_expr) => match lit_expr.lit() {
                        Some(lit) => {
                            let lit_kind = LitKind::lower_ast(ctxt, lit);
                            match &lit_kind {
                                LitKind::Int(int_id) => {
                                    let u32_max = BigUint::from(u32::MAX);
                                    let v = int_id.data(ctxt.db());
                                    (v > &u32_max).then_some(SelectorErrorKind::Overflow)
                                }
                                LitKind::String(_) | LitKind::Bool(_) => {
                                    Some(SelectorErrorKind::InvalidType)
                                }
                            }
                        }
                        None => Some(SelectorErrorKind::InvalidType),
                    },
                    _ => None,
                },
                None => None,
            };

            return Some(ParsedSelector { expr, range, error });
        }

        // Reject `#[selector(value)]` form with helpful error
        if !attr.args.is_empty() {
            return Some(ParsedSelector {
                expr: None,
                range: attr.range,
                error: Some(SelectorErrorKind::InvalidForm),
            });
        }
    }

    None
}

/// Filters out msg-variant attributes that are handled specially during lowering.
/// Currently this removes `#[selector]` and invalid `#[payable]`.
fn filter_msg_variant_attrs<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attr_list: Option<ast::AttrList>,
) -> AttrListId<'db> {
    use crate::hir_def::attr::Attr;

    let db = ctxt.db();
    let Some(attr_list) = attr_list else {
        return AttrListId::new(db, vec![]);
    };

    let filtered: Vec<Attr<'db>> = attr_list
        .into_iter()
        .filter(|attr| {
            if let ast::AttrKind::Normal(normal_attr) = attr.kind()
                && let Some(path) = normal_attr.path()
            {
                let text = path.text();
                return text != "selector" && text != "payable";
            }
            true
        })
        .map(|attr| Attr::lower_ast(ctxt, attr))
        .collect();

    AttrListId::new(db, filtered)
}
