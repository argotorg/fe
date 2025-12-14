use parser::ast::{self, AstPtr, prelude::*};
use salsa::Accumulator;

use super::{FileLowerCtxt, body::BodyCtxt};
use crate::{
    hir_def::{
        AttrListId, Body, BodyKind, BodySourceMap, EffectParamListId, Expr, ExprId,
        FuncParamListId, GenericParamListId, IdentId, LitKind, NodeStore, Partial, Pat, PathId,
        TraitRefId, TupleTypeId, TypeBound, TypeId, TypeKind, WhereClauseId,
        attr::{Attr, NormalAttr},
        item::*,
    },
    span::{ContractInitDesugared, HirOrigin, MsgDesugared, MsgDesugaredFocus},
};

/// Selector-related errors accumulated during msg block lowering.
#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SelectorError {
    pub kind: SelectorErrorKind,
    pub file: common::file::File,
    /// Range of the primary span (selector attribute or variant name)
    pub primary_range: parser::TextRange,
    /// Range of the secondary span (for duplicates - the first occurrence)
    pub secondary_range: Option<parser::TextRange>,
    pub variant_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SelectorErrorKind {
    /// Selector value overflows u32.
    Overflow,
    /// Selector has invalid type (string or bool).
    InvalidType,
    /// No `#[selector]` attribute found.
    Missing,
    /// Selector attribute has invalid form (e.g. `#[selector(value)]` instead of `#[selector = value]`).
    InvalidForm,
    /// Duplicate selector value.
    Duplicate {
        first_variant_name: String,
        selector: u32,
    },
}

pub(crate) fn lower_module_items(ctxt: &mut FileLowerCtxt<'_>, items: ast::ItemList) {
    for item in items {
        ItemKind::lower_ast(ctxt, item);
    }
}

impl<'db> ItemKind<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Item) {
        let Some(kind) = ast.kind() else {
            return;
        };

        match kind {
            ast::ItemKind::Mod(mod_) => {
                Mod::lower_ast(ctxt, mod_);
            }
            ast::ItemKind::Func(fn_) => {
                Func::lower_ast(ctxt, fn_);
            }
            ast::ItemKind::Struct(struct_) => {
                Struct::lower_ast(ctxt, struct_);
            }
            ast::ItemKind::Contract(contract) => {
                Contract::lower_ast(ctxt, contract);
            }
            ast::ItemKind::Enum(enum_) => {
                Enum::lower_ast(ctxt, enum_);
            }
            ast::ItemKind::Msg(msg) => {
                lower_msg_as_mod(ctxt, msg);
            }
            ast::ItemKind::TypeAlias(alias) => {
                TypeAlias::lower_ast(ctxt, alias);
            }
            ast::ItemKind::Impl(impl_) => {
                Impl::lower_ast(ctxt, impl_);
            }
            ast::ItemKind::Trait(trait_) => {
                Trait::lower_ast(ctxt, trait_);
            }
            ast::ItemKind::ImplTrait(impl_trait) => {
                ImplTrait::lower_ast(ctxt, impl_trait);
            }
            ast::ItemKind::Const(const_) => {
                Const::lower_ast(ctxt, const_);
            }
            ast::ItemKind::Use(use_) => {
                Use::lower_ast(ctxt, use_);
            }
            ast::ItemKind::Extern(extern_) => {
                if let Some(extern_block) = extern_.extern_block() {
                    for fn_ in extern_block {
                        Func::lower_ast(ctxt, fn_);
                    }
                }
            }
        }
    }
}

impl<'db> Mod<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Mod) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Mod(name));
        ctxt.enter_item_scope(id, true);

        ctxt.insert_synthetic_prelude_use();

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        if let Some(items) = ast.items() {
            lower_module_items(ctxt, items);
        }

        let origin = HirOrigin::raw(&ast);
        let mod_ = Self::new(ctxt.db(), id, name, attributes, vis, ctxt.top_mod(), origin);
        ctxt.leave_item_scope(mod_)
    }
}

impl<'db> Func<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Func) -> Self {
        let sig = ast.sig();
        let name = IdentId::lower_token_partial(ctxt, sig.name());
        let id = ctxt.joined_id(TrackedItemVariant::Func(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, sig.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, sig.where_clause());
        let params = sig
            .params()
            .map(|params| FuncParamListId::lower_ast(ctxt, params))
            .into();
        let ret_ty = sig.ret_ty().map(|ty| TypeId::lower_ast(ctxt, ty));
        let effects = lower_uses_clause_opt(ctxt, ast.sig().uses_clause());
        let modifier = ItemModifier::lower_ast(ast.modifier());
        let body = ast
            .body()
            .map(|body| Body::lower_ast(ctxt, ast::Expr::cast(body.syntax().clone()).unwrap()));
        let origin = HirOrigin::raw(&ast);

        let fn_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            generic_params,
            where_clause,
            params,
            effects,
            ret_ty,
            modifier,
            body,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(fn_)
    }
}

impl<'db> Struct<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Struct) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Struct(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let fields = FieldDefListId::lower_ast_opt(ctxt, ast.fields());
        let origin = HirOrigin::raw(&ast);

        let struct_ = Self::new(
            ctxt.db(),
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
}

impl<'db> Contract<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Contract) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Contract(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let fields = FieldDefListId::lower_contract_fields_opt(ctxt, ast.fields());
        // Contract-level uses clause
        let effects = lower_uses_clause_opt(ctxt, ast.uses_clause());

        if let Some(init_ast) = ast.init_block() {
            lower_contract_init_as_func(ctxt, init_ast);
        }

        // Recv blocks (message handlers)
        let recvs = {
            let mut data = Vec::new();
            for (recv_idx, r) in ast.recvs().enumerate() {
                let msg_path = r.path().map(|p| PathId::lower_ast(ctxt, p));
                let arms = r
                    .arms()
                    .map(|arms| ContractRecvArmListId::lower_ast(ctxt, recv_idx, arms))
                    .unwrap_or_else(|| ContractRecvArmListId::new(ctxt.db(), vec![]));
                data.push(ContractRecv { msg_path, arms });
            }
            ContractRecvListId::new(ctxt.db(), data)
        };
        let origin = HirOrigin::raw(&ast);

        let contract = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            fields,
            effects,
            recvs,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(contract)
    }
}

fn lower_contract_init_as_func<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    init_ast: ast::ContractInit,
) -> Func<'db> {
    let db = ctxt.db();

    let init_name = Partial::Present(IdentId::new(db, "init".to_string()));
    let id = ctxt.joined_id(TrackedItemVariant::Func(init_name));
    ctxt.enter_item_scope(id, false);

    let params = init_ast
        .params()
        .map(|p| FuncParamListId::lower_ast(ctxt, p))
        .into();
    let body = init_ast
        .body()
        .map(|b| Body::lower_ast(ctxt, ast::Expr::cast(b.syntax().clone()).unwrap()));

    let init_fn = Func::new(
        db,
        id,
        init_name,
        AttrListId::new(db, vec![]),
        GenericParamListId::new(db, vec![]),
        WhereClauseId::new(db, vec![]),
        params,
        lower_uses_clause_opt(ctxt, init_ast.uses_clause()),
        None,
        ItemModifier::None,
        body,
        ctxt.top_mod(),
        HirOrigin::desugared(ContractInitDesugared {
            init: AstPtr::new(&init_ast),
            focus: Default::default(),
        }),
    );
    ctxt.leave_item_scope(init_fn)
}

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
fn lower_msg_as_mod<'db>(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Msg) -> Mod<'db> {
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

    // Create the #[msg] attribute to mark this as a desugared msg module
    let msg_attr_path = PathId::from_ident(db, IdentId::new(db, "msg".to_string()));
    let msg_attr = Attr::Normal(NormalAttr {
        path: Partial::Present(msg_attr_path),
        args: vec![],
    });

    // Combine with any existing attributes on the msg block
    let mut attrs = vec![msg_attr];
    if let Some(attr_list) = ast.attr_list() {
        for attr in attr_list {
            attrs.push(Attr::lower_ast(ctxt, attr));
        }
    }
    let attributes = AttrListId::new(db, attrs);

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
    // and shouldn't be kept on the generated struct where it would be "unknown"
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

    let msg_variant_trait_path = PathId::from_ident(db, root)
        .push_ident(db, message)
        .push_ident(db, msg_variant);
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
    let types = vec![AssocTyDef {
        name: Partial::Present(return_name),
        type_ref: return_ty,
    }];

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

fn lower_uses_clause_opt<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    uses: Option<ast::UsesClause>,
) -> EffectParamListId<'db> {
    use crate::hir_def::{EffectParam, EffectParamListId};

    let mut data: Vec<EffectParam<'db>> = Vec::new();

    if let Some(uses) = uses {
        if let Some(list) = uses.param_list() {
            for p in list {
                let name = p.name().map(|n| IdentId::lower_token(ctxt, n.syntax()));
                let is_mut = p.mut_token().is_some();
                let key_path = p.path().map(|path| PathId::lower_ast(ctxt, path)).into();
                data.push(EffectParam {
                    name,
                    key_path,
                    is_mut,
                });
            }
        } else if let Some(p) = uses.param() {
            let name = p.name().map(|n| IdentId::lower_token(ctxt, n.syntax()));
            let is_mut = p.mut_token().is_some();
            let key_path = p.path().map(|path| PathId::lower_ast(ctxt, path)).into();
            data.push(EffectParam {
                name,
                key_path,
                is_mut,
            });
        }
    }

    EffectParamListId::new(ctxt.db(), data)
}

impl<'db> Enum<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Enum) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Enum(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let variants = VariantDefListId::lower_ast_opt(ctxt, ast.variants());
        let origin = HirOrigin::raw(&ast);

        let enum_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            generic_params,
            where_clause,
            variants,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(enum_)
    }
}

impl<'db> TypeAlias<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TypeAlias) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.alias());
        let id = ctxt.joined_id(TrackedItemVariant::TypeAlias(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let origin = HirOrigin::raw(&ast);

        let alias = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            generic_params,
            ty,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(alias)
    }
}

impl<'db> Impl<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Impl) -> Self {
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let id = ctxt.joined_id(TrackedItemVariant::Impl(ty));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let origin = HirOrigin::raw(&ast);

        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                Func::lower_ast(ctxt, impl_item);
            }
        }

        let impl_ = Self::new(
            ctxt.db(),
            id,
            ty,
            attributes,
            generic_params,
            where_clause,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(impl_)
    }
}

impl<'db> Trait<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Trait) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Trait(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let super_traits = if let Some(super_traits) = ast.super_trait_list() {
            super_traits
                .into_iter()
                .map(|trait_ref| TraitRefId::lower_ast(ctxt, trait_ref))
                .collect()
        } else {
            vec![]
        };
        let origin = HirOrigin::raw(&ast);

        let mut types = vec![];
        let mut consts = vec![];

        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                match impl_item.kind() {
                    ast::TraitItemKind::Func(func) => {
                        Func::lower_ast(ctxt, func);
                    }
                    ast::TraitItemKind::Type(t) => types.push(AssocTyDecl::lower_ast(ctxt, t)),
                    ast::TraitItemKind::Const(c) => {
                        consts.push(AssocConstDecl::lower_ast(ctxt, c));
                    }
                };
            }
        }

        let trait_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            generic_params,
            super_traits,
            where_clause,
            types,
            consts,
            ctxt.top_mod(),
            origin,
        );

        ctxt.leave_item_scope(trait_)
    }
}

impl<'db> AssocTyDecl<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitTypeItem) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let bounds = ast
            .bounds()
            .map(|bounds| {
                bounds
                    .into_iter()
                    .map(|bound| TypeBound::lower_ast(ctxt, bound))
                    .collect()
            })
            .unwrap_or_default();

        let default = TypeId::lower_ast_partial(ctxt, ast.ty()).to_opt();

        AssocTyDecl {
            name,
            bounds,
            default,
        }
    }
}

impl<'db> ImplTrait<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::ImplTrait) -> Self {
        let trait_ref = TraitRefId::lower_ast_partial(ctxt, ast.trait_ref());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let id = ctxt.joined_id(TrackedItemVariant::ImplTrait(trait_ref, ty));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let origin = HirOrigin::raw(&ast);

        let mut types = vec![];
        let mut consts = vec![];
        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                match impl_item.kind() {
                    ast::TraitItemKind::Func(func) => {
                        Func::lower_ast(ctxt, func);
                    }
                    ast::TraitItemKind::Type(t) => types.push(AssocTyDef::lower_ast(ctxt, t)),
                    ast::TraitItemKind::Const(c) => {
                        consts.push(AssocConstDef::lower_ast(ctxt, c));
                    }
                };
            }
        }

        let impl_trait = Self::new(
            ctxt.db(),
            id,
            trait_ref,
            ty,
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
}

impl<'db> AssocTyDef<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitTypeItem) -> Self {
        AssocTyDef {
            name: IdentId::lower_token_partial(ctxt, ast.name()),
            type_ref: TypeId::lower_ast_partial(ctxt, ast.ty()),
        }
    }
}

impl<'db> AssocConstDecl<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitConstItem) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let default = ast
            .value()
            .map(|expr| crate::hir_def::Partial::Present(Body::lower_ast(ctxt, expr)));
        AssocConstDecl { name, ty, default }
    }
}

impl<'db> AssocConstDef<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitConstItem) -> Self {
        let value = ast.value().map(|expr| Body::lower_ast(ctxt, expr)).into();

        AssocConstDef {
            name: IdentId::lower_token_partial(ctxt, ast.name()),
            ty: TypeId::lower_ast_partial(ctxt, ast.ty()),
            value,
        }
    }
}

impl<'db> Const<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Const) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Const(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let body = ast.value().map(|ast| Body::lower_ast(ctxt, ast)).into();
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let origin = HirOrigin::raw(&ast);

        let const_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            ty,
            body,
            vis,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(const_)
    }
}

impl ItemModifier {
    pub(super) fn lower_ast(ast: Option<ast::ItemModifier>) -> Self {
        let Some(ast) = ast else {
            return Self::None;
        };

        match (ast.pub_kw().is_some(), ast.unsafe_kw().is_some()) {
            (true, true) => Self::PubAndUnsafe,
            (true, false) => Self::Pub,
            (false, true) => Self::Unsafe,
            (false, false) => Self::None,
        }
    }
}

impl<'db> FieldDefListId<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::RecordFieldDefList) -> Self {
        let fields = ast
            .into_iter()
            .map(|field| FieldDef::lower_ast(ctxt, field))
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), fields)
    }

    fn lower_ast_opt(ctxt: &mut FileLowerCtxt<'db>, ast: Option<ast::RecordFieldDefList>) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or(Self::new(ctxt.db(), Vec::new()))
    }

    fn lower_contract_fields_opt(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::ContractFields>,
    ) -> Self {
        match ast {
            Some(cf) => {
                let fields = cf
                    .into_iter()
                    .map(|field| FieldDef::lower_ast(ctxt, field))
                    .collect::<Vec<_>>();
                Self::new(ctxt.db(), fields)
            }
            None => Self::new(ctxt.db(), Vec::new()),
        }
    }
}

impl<'db> FieldDef<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::RecordFieldDef) -> Self {
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let type_ref = TypeId::lower_ast_partial(ctxt, ast.ty());
        let vis = if ast.pub_kw().is_some() {
            Visibility::Public
        } else {
            Visibility::Private
        };

        Self::new(attributes, name, type_ref, vis)
    }
}

impl<'db> VariantDefListId<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::VariantDefList) -> Self {
        let variants = ast
            .into_iter()
            .map(|variant| VariantDef::lower_ast(ctxt, variant))
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), variants)
    }

    fn lower_ast_opt(ctxt: &mut FileLowerCtxt<'db>, ast: Option<ast::VariantDefList>) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or(Self::new(ctxt.db(), Vec::new()))
    }
}

impl<'db> VariantDef<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::VariantDef) -> Self {
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let kind = match ast.kind() {
            ast::VariantKind::Unit => VariantKind::Unit,
            ast::VariantKind::Tuple(t) => VariantKind::Tuple(TupleTypeId::lower_ast(ctxt, t)),
            ast::VariantKind::Record(r) => VariantKind::Record(FieldDefListId::lower_ast(ctxt, r)),
        };

        Self {
            attributes,
            name,
            kind,
        }
    }
}

// MsgVariantListId and MsgVariantDef lowering removed - msg blocks are now
// desugared into modules with structs and trait impls in lower_msg_as_mod()

impl<'db> ContractRecvArmListId<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, recv_idx: usize, ast: ast::RecvArmList) -> Self {
        let arms = ast
            .into_iter()
            .enumerate()
            .map(|(idx, arm)| ContractRecvArm::lower_ast(ctxt, recv_idx, idx, arm))
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), arms)
    }
}

impl<'db> ContractRecvArm<'db> {
    fn lower_ast(
        ctxt: &mut FileLowerCtxt<'db>,
        recv_idx: usize,
        arm_idx: usize,
        ast: ast::RecvArm,
    ) -> Self {
        let variant = TrackedItemVariant::ContractRecvArm {
            recv_idx: recv_idx as u32,
            arm_idx: arm_idx as u32,
        };
        let id = ctxt.joined_id(variant);
        let mut body_ctxt = BodyCtxt::new(ctxt, id);

        let pat = Pat::lower_ast_opt(&mut body_ctxt, ast.pat());
        let body_ast = ast
            .body()
            .map(|b| ast::Expr::cast(b.syntax().clone()).unwrap());
        let body_expr = Expr::push_to_body_opt(&mut body_ctxt, body_ast.clone());
        let body = body_ctxt.build(body_ast.as_ref(), body_expr, BodyKind::FuncBody);
        let ret_ty = ast.ret_ty().map(|ty| TypeId::lower_ast(ctxt, ty));
        let effects = lower_uses_clause_opt(ctxt, ast.uses_clause());

        ContractRecvArm {
            pat,
            ret_ty,
            effects,
            body,
        }
    }
}
