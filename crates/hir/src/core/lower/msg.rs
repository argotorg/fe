use parser::ast::{self, AttrListOwner as _};
use salsa::Accumulator as _;

use super::hir_builder::HirBuilder;

use crate::{
    HirDb, SelectorError, SelectorErrorKind,
    hir_def::{
        AssocConstDef, AttrListId, Body, BodyKind, FieldDef, FieldDefListId, FuncModifiers,
        IdentId, ImplTrait, Mod, Partial, PathId, Struct, TrackedItemVariant, TraitRefId,
        TupleTypeId, TypeId, TypeKind, Visibility,
    },
    lower::FileLowerCtxt,
    span::MsgDesugared,
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
        focus: Default::default(),
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

fn lower_msg_variant_field_names<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    variant: &ast::MsgVariant,
) -> Vec<IdentId<'db>> {
    let mut names = Vec::new();
    if let Some(params_ast) = variant.params() {
        for field in params_ast.into_iter() {
            let Some(name_token) = field.name() else {
                continue;
            };
            names.push(IdentId::lower_token(ctxt, name_token));
        }
    }
    names
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
    lower_msg_variant_encode_impl(builder, variant, struct_);
    lower_msg_variant_decode_trait_impl(builder, variant, struct_);
}

fn lower_msg_variant_encode_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, MsgDesugared>,
    variant: &ast::MsgVariant,
    struct_: Struct<'db>,
) -> ImplTrait<'db> {
    let field_names = lower_msg_variant_field_names(builder.ctxt(), variant);

    let trait_ref = builder.core_abi_trait_ref_sol("Encode");
    let ty = variant_struct_ty(builder.db(), struct_);

    builder.impl_trait(trait_ref, ty, |builder| {
        let abi_encoder_trait_ref = builder.core_abi_trait_ref_sol("AbiEncoder");
        let (e_generic_params, e_ty) =
            builder.type_param_with_trait_bound("E", abi_encoder_trait_ref);

        let encoder_ident = builder.ident("e");
        let params = builder.params([
            builder.param_self(),
            builder.param_mut_underscore_named(encoder_ident, e_ty),
        ]);

        builder.func_generic(
            "encode",
            e_generic_params,
            params,
            None,
            FuncModifiers::new(Visibility::Private, false, false, false),
            |body| {
                body.let_self_record(&field_names);
                body.encode_fields(&field_names, encoder_ident);
            },
        );
    })
}

fn lower_msg_variant_decode_trait_impl<'db>(
    builder: &mut HirBuilder<'_, 'db, MsgDesugared>,
    variant: &ast::MsgVariant,
    struct_: Struct<'db>,
) -> ImplTrait<'db> {
    let fields = lower_msg_variant_field_specs(builder.ctxt(), variant);
    let field_names = fields.iter().map(|(name, _)| *name).collect::<Vec<_>>();

    let trait_ref = builder.core_abi_trait_ref_sol("Decode");
    let ty = variant_struct_ty(builder.db(), struct_);

    builder.impl_trait(trait_ref, ty, |builder| {
        let abi_decoder_trait_ref = builder.core_abi_trait_ref_sol("AbiDecoder");
        let (d_generic_params, d_ty) =
            builder.type_param_with_trait_bound("D", abi_decoder_trait_ref);

        let decoder_ident = builder.ident("d");
        let params = builder.params([builder.param_mut_underscore_named(decoder_ident, d_ty)]);

        builder.func_generic(
            "decode",
            d_generic_params,
            params,
            Some(builder.self_ty()),
            FuncModifiers::new(Visibility::Private, false, false, false),
            |body| {
                for (name, ty) in fields.iter().copied() {
                    body.decode_into(name, ty);
                }
                body.return_record_self(&field_names);
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
    let attributes = filter_selector_attr(builder.ctxt(), variant.attr_list());
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

        if let Some(value) = normal_attr.value() {
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
        if normal_attr.args().is_some() {
            return Some(ParsedSelector {
                expr: None,
                range,
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
