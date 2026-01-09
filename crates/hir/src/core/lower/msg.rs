use parser::ast::{self, AttrListOwner as _, ItemModifierOwner as _};
use salsa::Accumulator as _;

use crate::{
    SelectorError, SelectorErrorKind,
    hir_def::{
        AssocConstDef, AssocTyDef, AttrListId, Body, BodyKind, BodySourceMap, EffectParamListId,
        Expr, ExprId, FieldDef, FieldDefListId, Func, FuncParam, FuncParamListId, FuncParamName,
        GenericArg, GenericArgListId, GenericParamListId, IdentId, Impl, ImplTrait, IntegerId,
        ItemModifier, LitKind, Mod, NodeStore, Partial, Pat, PathId, PathKind, Stmt, StmtId,
        Struct, TrackedItemVariant, TraitRefId, TupleTypeId, TypeGenericArg, TypeId, TypeKind,
        Visibility, WhereClauseId,
    },
    lower::{FileLowerCtxt, body::BodyCtxt},
    span::{HirOrigin, MsgDesugared, MsgDesugaredFocus},
};

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
    use rustc_hash::FxHashMap;

    let db = ctxt.db();
    let name = IdentId::lower_token_partial(ctxt, ast.name());

    // Use Mod variant for the ID since we're creating a module
    let id = ctxt.joined_id(TrackedItemVariant::Mod(name));
    ctxt.enter_item_scope(id, true);

    // Insert synthetic prelude use for the module
    ctxt.insert_synthetic_prelude_use();
    // Insert `use super::*` so msg modules can see parent types
    ctxt.insert_synthetic_super_use();

    // Lower any existing attributes on the msg block
    let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());

    let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();

    // Create the desugared origin pointing to the original msg AST
    let msg_desugared = MsgDesugared {
        msg: parser::ast::AstPtr::new(&ast),
        variant_idx: None,
        focus: Default::default(),
    };

    // Track selectors for duplicate detection
    let mut seen_selectors: FxHashMap<u32, SeenSelector> = FxHashMap::default();

    // Lower each variant as a struct + impl MsgVariant
    if let Some(variants) = ast.variants() {
        for (idx, variant) in variants.into_iter().enumerate() {
            lower_msg_variant(ctxt, &ast, idx, variant, &mut seen_selectors);
        }
    }

    let origin = HirOrigin::desugared(msg_desugared);
    let mod_ = Mod::new(db, id, name, attributes, vis, ctxt.top_mod(), origin);
    ctxt.leave_item_scope(mod_)
}

/// Lowers a single msg variant to a struct and an impl MsgVariant block.
fn lower_msg_variant<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    msg_ast: &ast::Msg,
    variant_idx: usize,
    variant: ast::MsgVariant,
    seen_selectors: &mut rustc_hash::FxHashMap<u32, SeenSelector>,
) {
    // Create the struct for this variant
    let struct_ = lower_msg_variant_struct(ctxt, msg_ast, variant_idx, &variant);

    // Create the impl MsgVariant for this variant
    lower_msg_variant_impl(
        ctxt,
        msg_ast,
        variant_idx,
        &variant,
        struct_,
        seen_selectors,
    );

    // Create `Variant::decode` helper.
    lower_msg_variant_decode_impl(ctxt, msg_ast, variant_idx, &variant, struct_);
}

fn lower_msg_variant_decode_impl<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    msg_ast: &ast::Msg,
    variant_idx: usize,
    variant: &ast::MsgVariant,
    struct_: Struct<'db>,
) -> Impl<'db> {
    use common::ingot::IngotKind;

    let db = ctxt.db();

    let ingot = ctxt.top_mod().ingot(db);
    let core_root = if ingot.kind(db) == IngotKind::Core {
        IdentId::make_ingot(db)
    } else {
        IdentId::make_core(db)
    };
    // impl Variant { ... }
    let struct_name = struct_.name(db);
    let self_type_path = match struct_name.to_opt() {
        Some(name) => PathId::from_ident(db, name),
        None => PathId::from_ident(db, IdentId::new(db, "_".to_string())),
    };
    let ty = TypeId::new(db, TypeKind::Path(Partial::Present(self_type_path)));

    let id = ctxt.joined_id(TrackedItemVariant::Impl(Partial::Present(ty)));
    ctxt.enter_item_scope(id, false);

    let msg_desugared = MsgDesugared {
        msg: parser::ast::AstPtr::new(msg_ast),
        variant_idx: Some(variant_idx),
        focus: Default::default(),
    };
    let body_desugared = msg_desugared.clone();
    let impl_origin: HirOrigin<ast::Impl> = HirOrigin::desugared(msg_desugared.clone());
    let func_origin: HirOrigin<ast::Func> = HirOrigin::desugared(msg_desugared);
    let expr_origin: HirOrigin<ast::Expr> = HirOrigin::desugared(body_desugared.clone());
    let stmt_origin: HirOrigin<ast::Stmt> = HirOrigin::desugared(body_desugared.clone());
    let pat_origin: HirOrigin<ast::Pat> = HirOrigin::desugared(body_desugared);

    let impl_ = Impl::new(
        db,
        id,
        Partial::Present(ty),
        AttrListId::new(db, vec![]),
        GenericParamListId::new(db, vec![]),
        WhereClauseId::new(db, vec![]),
        ctxt.top_mod(),
        impl_origin,
    );

    // Generate the `decode` method inside the impl.
    let decode_fn_name = Partial::Present(IdentId::new(db, "decode".to_string()));
    let decode_fn_id = ctxt.joined_id(TrackedItemVariant::Func(decode_fn_name));
    ctxt.enter_item_scope(decode_fn_id, false);

    let std_root = if ingot.kind(db) == IngotKind::Std {
        IdentId::make_ingot(db)
    } else {
        IdentId::new(db, "std".to_string())
    };
    let call_data_path = PathId::from_segments(db, &["std", "evm", "calldata", "CallData"]);
    let call_data_ty = TypeId::new(db, TypeKind::Path(Partial::Present(call_data_path)));
    let sol_decoder_args = GenericArgListId::new(
        db,
        vec![GenericArg::Type(TypeGenericArg {
            ty: Partial::Present(call_data_ty),
        })],
        true,
    );
    let sol_decoder_path = PathId::from_ident(db, std_root)
        .push_ident(db, IdentId::new(db, "abi".to_string()))
        .push_ident(db, IdentId::new(db, "sol".to_string()))
        .push(
            db,
            PathKind::Ident {
                ident: Partial::Present(IdentId::new(db, "SolDecoder".to_string())),
                generic_args: sol_decoder_args,
            },
        );
    let d_ty = TypeId::new(db, TypeKind::Path(Partial::Present(sol_decoder_path)));
    let params = FuncParamListId::new(
        db,
        vec![FuncParam {
            is_mut: true,
            label: Some(FuncParamName::Underscore),
            name: Partial::Present(FuncParamName::ident(db, "d")),
            ty: Partial::Present(d_ty),
            self_ty_fallback: false,
        }],
    );

    let ret_ty = Some(TypeId::new(
        db,
        TypeKind::Path(Partial::Present(PathId::from_ident(
            db,
            IdentId::make_self_ty(db),
        ))),
    ));

    let mut body_ctxt = BodyCtxt::new(ctxt, ctxt.joined_id(TrackedItemVariant::FuncBody));

    let mut stmts = Vec::new();
    let mut tmp_counter: usize = 0;

    if let Some(params_ast) = variant.params() {
        for field in params_ast.into_iter() {
            let Some(name_token) = field.name() else {
                continue;
            };
            let field_name = IdentId::lower_token(body_ctxt.f_ctxt, name_token);

            let Some(field_ty) = TypeId::lower_ast_partial(body_ctxt.f_ctxt, field.ty()).to_opt()
            else {
                continue;
            };

            lower_msg_decode_into(
                &mut body_ctxt,
                core_root,
                field_name,
                field_ty,
                &mut tmp_counter,
                &mut stmts,
                expr_origin.clone(),
                stmt_origin.clone(),
                pat_origin.clone(),
            );
        }
    }

    let self_path = Partial::Present(PathId::from_ident(db, IdentId::make_self_ty(db)));
    let mut fields = Vec::new();
    if let Some(params_ast) = variant.params() {
        for field in params_ast.into_iter() {
            let Some(name_token) = field.name() else {
                continue;
            };
            let field_name = IdentId::lower_token(body_ctxt.f_ctxt, name_token);
            let expr = body_ctxt.push_expr(
                Expr::Path(Partial::Present(PathId::from_ident(db, field_name))),
                expr_origin.clone(),
            );
            fields.push(crate::hir_def::Field {
                label: Some(field_name),
                expr,
            });
        }
    }
    let record_expr = body_ctxt.push_expr(Expr::RecordInit(self_path, fields), expr_origin.clone());
    stmts.push(body_ctxt.push_stmt(Stmt::Return(Some(record_expr)), stmt_origin.clone()));

    body_ctxt.f_ctxt.enter_block_scope();
    let root_expr = body_ctxt.push_expr(Expr::Block(stmts), expr_origin.clone());
    body_ctxt.f_ctxt.leave_block_scope(root_expr);
    let body = body_ctxt.build(None, root_expr, BodyKind::FuncBody);

    let decode_fn = Func::new(
        db,
        decode_fn_id,
        decode_fn_name,
        AttrListId::new(db, vec![]),
        GenericParamListId::new(db, vec![]),
        WhereClauseId::new(db, vec![]),
        Partial::Present(params),
        EffectParamListId::new(db, vec![]),
        ret_ty,
        ItemModifier::Pub,
        Some(body),
        ctxt.top_mod(),
        func_origin,
    );
    ctxt.leave_item_scope(decode_fn);

    ctxt.leave_item_scope(impl_)
}

pub(super) fn lower_revert_call<'ctxt, 'db>(
    body_ctxt: &mut BodyCtxt<'ctxt, 'db>,
    core_root: IdentId<'db>,
    expr_origin: HirOrigin<ast::Expr>,
) -> ExprId {
    let db = body_ctxt.f_ctxt.db();

    let revert_path =
        PathId::from_ident(db, core_root).push_ident(db, IdentId::new(db, "revert".to_string()));
    let revert_callee = body_ctxt.push_expr(
        Expr::Path(Partial::Present(revert_path)),
        expr_origin.clone(),
    );
    let zero_lit = Expr::Lit(LitKind::Int(IntegerId::from_usize(db, 0)));
    let call_expr = Expr::Call(
        revert_callee,
        vec![
            crate::hir_def::CallArg {
                label: None,
                expr: body_ctxt.push_expr(zero_lit.clone(), expr_origin.clone()),
            },
            crate::hir_def::CallArg {
                label: None,
                expr: body_ctxt.push_expr(zero_lit, expr_origin.clone()),
            },
        ],
    );
    body_ctxt.push_expr(call_expr, expr_origin)
}

#[allow(clippy::too_many_arguments)]
fn lower_msg_decode_into<'ctxt, 'db>(
    body_ctxt: &mut BodyCtxt<'ctxt, 'db>,
    core_root: IdentId<'db>,
    target_ident: IdentId<'db>,
    ty: TypeId<'db>,
    tmp_counter: &mut usize,
    stmts: &mut Vec<StmtId>,
    expr_origin: HirOrigin<ast::Expr>,
    stmt_origin: HirOrigin<ast::Stmt>,
    pat_origin: HirOrigin<ast::Pat>,
) {
    let db = body_ctxt.f_ctxt.db();

    if let TypeKind::Tuple(tup) = ty.data(db) {
        let mut elem_idents = Vec::new();
        for elem_ty in tup.data(db) {
            let Some(elem_ty) = elem_ty.to_opt() else {
                continue;
            };
            let elem_ident = IdentId::new(db, format!("__tmp{}", *tmp_counter));
            *tmp_counter += 1;
            lower_msg_decode_into(
                body_ctxt,
                core_root,
                elem_ident,
                elem_ty,
                tmp_counter,
                stmts,
                expr_origin.clone(),
                stmt_origin.clone(),
                pat_origin.clone(),
            );
            elem_idents.push(elem_ident);
        }

        let elems = elem_idents
            .iter()
            .map(|ident| {
                body_ctxt.push_expr(
                    Expr::Path(Partial::Present(PathId::from_ident(db, *ident))),
                    expr_origin.clone(),
                )
            })
            .collect();
        let tuple_expr = body_ctxt.push_expr(Expr::Tuple(elems), expr_origin.clone());
        let bind_pat = body_ctxt.push_pat(
            Pat::Path(
                Partial::Present(PathId::from_ident(db, target_ident)),
                false,
            ),
            pat_origin.clone(),
        );
        stmts.push(body_ctxt.push_stmt(Stmt::Let(bind_pat, None, Some(tuple_expr)), stmt_origin));
        return;
    }

    let Some(ty_path) = (match ty.data(db) {
        TypeKind::Path(p) => p.to_opt(),
        _ => None,
    }) else {
        let revert = lower_revert_call(body_ctxt, core_root, expr_origin.clone());
        let bind_pat = body_ctxt.push_pat(
            Pat::Path(
                Partial::Present(PathId::from_ident(db, target_ident)),
                false,
            ),
            pat_origin.clone(),
        );
        stmts.push(body_ctxt.push_stmt(Stmt::Let(bind_pat, None, Some(revert)), stmt_origin));
        return;
    };

    let decode_path = ty_path.push_ident(db, IdentId::new(db, "decode".to_string()));
    let decode_callee = body_ctxt.push_expr(
        Expr::Path(Partial::Present(decode_path)),
        expr_origin.clone(),
    );
    let d_expr = body_ctxt.push_expr(
        Expr::Path(Partial::Present(PathId::from_ident(
            db,
            IdentId::new(db, "d".to_string()),
        ))),
        expr_origin.clone(),
    );
    let decode_call = body_ctxt.push_expr(
        Expr::Call(
            decode_callee,
            vec![crate::hir_def::CallArg {
                label: None,
                expr: d_expr,
            }],
        ),
        expr_origin.clone(),
    );

    let bind_pat = body_ctxt.push_pat(
        Pat::Path(
            Partial::Present(PathId::from_ident(db, target_ident)),
            false,
        ),
        pat_origin,
    );
    stmts.push(body_ctxt.push_stmt(Stmt::Let(bind_pat, None, Some(decode_call)), stmt_origin));
}

/// Creates a struct from a msg variant.
fn lower_msg_variant_struct<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    msg_ast: &ast::Msg,
    variant_idx: usize,
    variant: &ast::MsgVariant,
) -> Struct<'db> {
    let db = ctxt.db();
    let name = IdentId::lower_token_partial(ctxt, variant.name());
    let id = ctxt.joined_id(TrackedItemVariant::Struct(name));
    ctxt.enter_item_scope(id, false);

    // Filter out the #[selector] attribute - it's consumed during desugaring
    // and shouldn't be kept on the generated struct
    let attributes = filter_selector_attr(ctxt, variant.attr_list());

    // Msg variant structs are public within the module
    let vis = Visibility::Public;

    // No generic params or where clause for msg variant structs
    let generic_params = GenericParamListId::new(db, vec![]);
    let where_clause = WhereClauseId::new(db, vec![]);

    // Lower the fields, making them all public
    let fields = lower_msg_variant_fields(ctxt, variant.params());

    let msg_desugared = MsgDesugared {
        msg: parser::ast::AstPtr::new(msg_ast),
        variant_idx: Some(variant_idx),
        focus: Default::default(),
    };
    let origin = HirOrigin::desugared(msg_desugared);

    let struct_ = Struct::new(
        db,
        id,
        name,
        attributes,
        vis,
        generic_params,
        where_clause,
        fields,
        ctxt.top_mod(),
        origin,
    );
    ctxt.leave_item_scope(struct_)
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
    ctxt: &mut FileLowerCtxt<'db>,
    msg_ast: &ast::Msg,
    variant_idx: usize,
    variant: &ast::MsgVariant,
    struct_: Struct<'db>,
    seen_selectors: &mut rustc_hash::FxHashMap<u32, SeenSelector>,
) -> ImplTrait<'db> {
    use common::ingot::IngotKind;

    let db = ctxt.db();

    // Build a fully qualified path to core::message::MsgVariant to ensure we
    // always reference the core trait, even if the prelude is shadowed.
    let ingot = ctxt.top_mod().ingot(db);
    let root = if ingot.kind(db) == IngotKind::Core {
        IdentId::make_ingot(db)
    } else {
        IdentId::make_core(db)
    };
    let message = IdentId::new(db, "message".to_string());
    let msg_variant = IdentId::new(db, "MsgVariant".to_string());

    let std_root = if ingot.kind(db) == IngotKind::Std {
        IdentId::make_ingot(db)
    } else {
        IdentId::new(db, "std".to_string())
    };
    let abi = IdentId::new(db, "abi".to_string());
    let sol = IdentId::new(db, "Sol".to_string());
    let sol_path = PathId::from_ident(db, std_root)
        .push_ident(db, abi)
        .push_ident(db, sol);
    let sol_ty = TypeId::new(db, TypeKind::Path(Partial::Present(sol_path)));
    let abi_args = GenericArgListId::new(
        db,
        vec![GenericArg::Type(TypeGenericArg {
            ty: Partial::Present(sol_ty),
        })],
        true,
    );

    let msg_variant_trait_path = PathId::from_ident(db, root).push_ident(db, message).push(
        db,
        PathKind::Ident {
            ident: Partial::Present(msg_variant),
            generic_args: abi_args,
        },
    );
    let trait_ref = TraitRefId::new(db, Partial::Present(msg_variant_trait_path));

    // Create a path to Self (the struct we just created)
    let struct_name = struct_.name(db);
    let self_type_path = match struct_name.to_opt() {
        Some(name) => PathId::from_ident(db, name),
        None => PathId::from_ident(db, IdentId::new(db, "_".to_string())),
    };
    let ty = TypeId::new(db, TypeKind::Path(Partial::Present(self_type_path)));

    let id = ctxt.joined_id(TrackedItemVariant::ImplTrait(
        Partial::Present(trait_ref),
        Partial::Present(ty),
    ));
    ctxt.enter_item_scope(id, false);

    let attributes = AttrListId::new(db, vec![]);
    let generic_params = GenericParamListId::new(db, vec![]);
    let where_clause = WhereClauseId::new(db, vec![]);

    let args_name = IdentId::new(db, "Args".to_string());
    let args_ty = {
        let elems = match variant.params() {
            Some(params) => params
                .into_iter()
                .map(|field| TypeId::lower_ast_partial(ctxt, field.ty()))
                .collect::<Vec<_>>(),
            None => vec![],
        };
        Partial::Present(TypeId::new(
            db,
            TypeKind::Tuple(TupleTypeId::new(db, elems)),
        ))
    };

    // Create associated type Return = <return_type>
    let return_name = IdentId::new(db, "Return".to_string());
    let return_ty = match variant.ret_ty() {
        Some(ret_ty) => TypeId::lower_ast_partial(ctxt, Some(ret_ty)),
        // Default to () if no return type specified
        None => Partial::Present(TypeId::new(
            db,
            TypeKind::Tuple(TupleTypeId::new(db, vec![])),
        )),
    };
    let types = vec![
        AssocTyDef {
            attributes: AttrListId::new(db, vec![]),
            name: Partial::Present(args_name),
            type_ref: args_ty,
        },
        AssocTyDef {
            attributes: AttrListId::new(db, vec![]),
            name: Partial::Present(return_name),
            type_ref: return_ty,
        },
    ];

    // Create associated const SELECTOR: u32 = <value>
    // Extract from #[selector = ...] attribute, or default to 0 if not specified
    let variant_name = variant
        .name()
        .map(|t| t.text().to_string())
        .unwrap_or_default();
    let selector_const = create_selector_const(
        ctxt,
        msg_ast,
        variant,
        variant_idx,
        &variant_name,
        seen_selectors,
    );
    let consts = vec![selector_const];

    let msg_desugared = MsgDesugared {
        msg: parser::ast::AstPtr::new(msg_ast),
        variant_idx: Some(variant_idx),
        focus: Default::default(),
    };
    let origin = HirOrigin::desugared(msg_desugared);

    let impl_trait = ImplTrait::new(
        db,
        id,
        Partial::Present(trait_ref),
        Partial::Present(ty),
        attributes,
        generic_params,
        where_clause,
        types,
        consts,
        ctxt.top_mod(),
        origin,
    );
    ctxt.leave_item_scope(impl_trait)
}

/// Info about a seen selector for duplicate detection.
struct SeenSelector {
    range: parser::TextRange,
    name: String,
}

/// Result of parsing a selector attribute from an AST.
struct ParsedSelector<'db> {
    /// The literal that was found in the selector attribute.
    lit: LitKind<'db>,
    /// The text range of the selector attribute for diagnostics.
    range: parser::TextRange,
    /// The validated selector value, if valid.
    value: Option<u32>,
    /// The error kind, if validation failed.
    error: Option<SelectorErrorKind>,
}

impl<'db> ParsedSelector<'db> {
    /// Validates a literal as a selector value, returning a ParsedSelector.
    fn from_lit(ctxt: &FileLowerCtxt<'db>, lit: LitKind<'db>, range: parser::TextRange) -> Self {
        use crate::hir_def::LitKind;
        use num_bigint::BigUint;
        use num_traits::ToPrimitive;

        let u32_max = BigUint::from(u32::MAX);

        let (value, error) = match &lit {
            LitKind::Int(int_id) => {
                let v = int_id.data(ctxt.db());
                if v > &u32_max {
                    (None, Some(SelectorErrorKind::Overflow))
                } else {
                    (Some(v.to_u32().unwrap_or(0)), None)
                }
            }
            LitKind::String(_) | LitKind::Bool(_) => (None, Some(SelectorErrorKind::InvalidType)),
        };

        Self {
            lit,
            range,
            value,
            error,
        }
    }

    /// Reports any errors to the accumulator and handles duplicate detection.
    /// Returns the literal and focus for creating the const body.
    fn finalize(
        self,
        db: &'db dyn crate::HirDb,
        file: common::file::File,
        variant_name: &str,
        seen_selectors: &mut rustc_hash::FxHashMap<u32, SeenSelector>,
    ) -> (LitKind<'db>, MsgDesugaredFocus) {
        if let Some(error_kind) = self.error {
            SelectorError {
                kind: error_kind,
                file,
                primary_range: self.range,
                secondary_range: None,
                variant_name: variant_name.to_string(),
            }
            .accumulate(db);
            return (self.lit, MsgDesugaredFocus::Selector);
        }

        let selector_value = self.value.unwrap();
        if let Some(first) = seen_selectors.get(&selector_value) {
            SelectorError {
                kind: SelectorErrorKind::Duplicate {
                    first_variant_name: first.name.clone(),
                    selector: selector_value,
                },
                file,
                primary_range: self.range,
                secondary_range: Some(first.range),
                variant_name: variant_name.to_string(),
            }
            .accumulate(db);
        } else {
            seen_selectors.insert(
                selector_value,
                SeenSelector {
                    range: self.range,
                    name: variant_name.to_string(),
                },
            );
        }

        (self.lit, MsgDesugaredFocus::Block)
    }
}

/// Creates the SELECTOR associated const from the variant's #[selector = ...] attribute.
/// Validates the selector value and tracks duplicates.
fn create_selector_const<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    msg_ast: &ast::Msg,
    variant: &ast::MsgVariant,
    variant_idx: usize,
    variant_name: &str,
    seen_selectors: &mut rustc_hash::FxHashMap<u32, SeenSelector>,
) -> AssocConstDef<'db> {
    use crate::hir_def::{IntegerId, LitKind};
    use num_bigint::BigUint;
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

    let (lit_kind, focus) = match parsed {
        Some(selector) => selector.finalize(db, file, variant_name, seen_selectors),
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
            (
                LitKind::Int(IntegerId::new(db, BigUint::from(0u32))),
                MsgDesugaredFocus::Block,
            )
        }
    };

    let msg_desugared = MsgDesugared {
        msg: parser::ast::AstPtr::new(msg_ast),
        variant_idx: Some(variant_idx),
        focus,
    };
    let body = create_lit_body_desugared(ctxt, lit_kind, msg_desugared);

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
) -> Option<ParsedSelector<'db>> {
    use crate::hir_def::LitKind;
    use parser::ast::prelude::*;

    for attr in attr_list {
        let ast::AttrKind::Normal(normal_attr) = attr.kind() else {
            continue;
        };
        let is_selector = normal_attr
            .path()
            .map(|p| p.text() == "selector")
            .unwrap_or(false);
        if !is_selector {
            continue;
        }

        let range = attr.syntax().text_range();

        if let Some(ast::AttrArgValueKind::Lit(lit)) = normal_attr.value() {
            let lit_kind = LitKind::lower_ast(ctxt, lit);
            return Some(ParsedSelector::from_lit(ctxt, lit_kind, range));
        }

        // Reject `#[selector(value)]` form with helpful error
        if normal_attr.args().is_some() {
            use crate::hir_def::IntegerId;
            use num_bigint::BigUint;
            // Use a placeholder literal for the error case
            let lit = LitKind::Int(IntegerId::new(ctxt.db(), BigUint::from(0u32)));
            return Some(ParsedSelector {
                lit,
                range,
                value: None,
                error: Some(SelectorErrorKind::InvalidForm),
            });
        }
    }

    None
}

/// Filters out the #[selector] attribute from an attribute list.
/// Returns an AttrListId containing all attributes except selector.
fn filter_selector_attr<'db>(
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
                return path.text() != "selector";
            }
            true
        })
        .map(|attr| Attr::lower_ast(ctxt, attr))
        .collect();

    AttrListId::new(db, filtered)
}

/// Creates a Body containing a single literal expression with a desugared origin.
/// This allows type errors on synthetic bodies to point back to their source.
fn create_lit_body_desugared<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    lit: LitKind<'db>,
    origin: MsgDesugared,
) -> Body<'db> {
    let db = ctxt.db();
    let id = ctxt.joined_id(TrackedItemVariant::NamelessBody);
    ctxt.enter_body_scope(id);

    let mut exprs: NodeStore<ExprId, Partial<Expr<'db>>> = NodeStore::new();
    let mut source_map = BodySourceMap::default();

    // Create the literal expression with desugared origin
    let expr = Expr::Lit(lit);
    let expr_id = exprs.push(Partial::Present(expr));
    source_map
        .expr_map
        .insert(expr_id, HirOrigin::desugared(origin.clone()));

    let body = Body::new(
        db,
        id,
        expr_id,
        BodyKind::Anonymous,
        NodeStore::new(), // stmts
        exprs,
        NodeStore::new(), // pats
        ctxt.top_mod(),
        source_map,
        HirOrigin::desugared(origin),
    );

    ctxt.leave_item_scope(body);
    body
}
