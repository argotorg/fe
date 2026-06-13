use parser::ast::{self, WhereClauseOwner as _, prelude::*};

use super::{
    FileLowerCtxt,
    attr::{AttrForm, AttrRule, AttrTarget, report_unsupported_attr, validate_attr_rules},
};
use crate::{
    hir_def::{
        AttrListId, Body, BodyKind, CompBinOp, EffectParamListId, FuncParamListId,
        GenericParamListId, IdentId, PathId, TraitRefId, TupleTypeId, TypeBound, TypeId,
        WhereClauseId, item::*,
    },
    lower::msg::lower_msg_as_mod,
    span::HirOrigin,
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

const ARITHMETIC_FORM: AttrForm = AttrForm::SingleArg {
    allow_bare: false,
    allowed_args: &["checked", "unchecked"],
};
const INLINE_FORM: AttrForm = AttrForm::SingleArg {
    allow_bare: true,
    allowed_args: &["always", "never"],
};
const UNROLL_FORM: AttrForm = AttrForm::SingleArg {
    allow_bare: true,
    allowed_args: &["never"],
};
const BARE_FORM: AttrForm = AttrForm::Bare;

const ARITHMETIC_EXPECTED: &str = "`#[arithmetic(checked)]` or `#[arithmetic(unchecked)]`";
const INLINE_EXPECTED: &str = "`#[inline]`, `#[inline(always)]`, or `#[inline(never)]`";
const MUST_USE_EXPECTED: &str = "`#[must_use]`";

const ARITHMETIC_TARGETS: &str = "functions and modules";
const EVENT_TARGETS: &str = "structs";
const ERROR_TARGETS: &str = "structs";
const MUST_USE_TARGETS: &str = "functions, structs, and enums";
const PAYABLE_TARGETS: &str = "init blocks and recv arms";
const INDEXED_TARGETS: &str = "event fields";

fn target(kind: &'static str, name: Option<String>) -> AttrTarget {
    AttrTarget::new(kind, name)
}

fn validate_mod_attrs<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    name: Option<String>,
) {
    validate_attr_rules(
        ctxt,
        attrs,
        target("mod", name),
        &[
            AttrRule::supported("arithmetic", ARITHMETIC_FORM, ARITHMETIC_EXPECTED),
            AttrRule::unsupported("event", EVENT_TARGETS),
            AttrRule::unsupported("error", ERROR_TARGETS),
            AttrRule::unsupported("must_use", MUST_USE_TARGETS),
            AttrRule::unsupported("payable", PAYABLE_TARGETS),
        ],
    );
}

pub(super) fn validate_module_inner_attrs<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
) {
    validate_attr_rules(
        ctxt,
        attrs,
        target("module", None),
        &[
            AttrRule::supported("arithmetic", ARITHMETIC_FORM, ARITHMETIC_EXPECTED),
            AttrRule::unsupported("must_use", MUST_USE_TARGETS),
            AttrRule::unsupported("payable", PAYABLE_TARGETS),
        ],
    );
}

fn validate_func_attrs<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    kind: &'static str,
    name: Option<String>,
    arithmetic_supported: bool,
) {
    let arithmetic = if arithmetic_supported {
        AttrRule::supported("arithmetic", ARITHMETIC_FORM, ARITHMETIC_EXPECTED)
    } else {
        AttrRule::unsupported("arithmetic", ARITHMETIC_TARGETS)
    };
    validate_attr_rules(
        ctxt,
        attrs,
        target(kind, name),
        &[
            arithmetic,
            AttrRule::supported("inline", INLINE_FORM, INLINE_EXPECTED),
            AttrRule::supported("must_use", BARE_FORM, MUST_USE_EXPECTED),
            AttrRule::unsupported("event", EVENT_TARGETS),
            AttrRule::unsupported("error", ERROR_TARGETS),
            AttrRule::unsupported("payable", PAYABLE_TARGETS),
        ],
    );
}

fn validate_struct_attrs<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    name: Option<String>,
) {
    validate_attr_rules(
        ctxt,
        attrs,
        target("struct", name),
        &[
            AttrRule::unsupported("arithmetic", ARITHMETIC_TARGETS),
            AttrRule::supported("event", BARE_FORM, "`#[event]`"),
            AttrRule::supported("error", BARE_FORM, "`#[error]`"),
            AttrRule::supported("must_use", BARE_FORM, MUST_USE_EXPECTED),
            AttrRule::unsupported("payable", PAYABLE_TARGETS),
        ],
    );
}

fn validate_enum_attrs<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    name: Option<String>,
) {
    validate_attr_rules(
        ctxt,
        attrs,
        target("enum", name),
        &[
            AttrRule::unsupported("arithmetic", ARITHMETIC_TARGETS),
            AttrRule::unsupported("event", EVENT_TARGETS),
            AttrRule::unsupported("error", ERROR_TARGETS),
            AttrRule::supported("must_use", BARE_FORM, MUST_USE_EXPECTED),
            AttrRule::unsupported("payable", PAYABLE_TARGETS),
        ],
    );
}

fn validate_unsupported_item_attrs<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    kind: &'static str,
    name: Option<String>,
) {
    validate_attr_rules(
        ctxt,
        attrs,
        target(kind, name),
        &[
            AttrRule::unsupported("arithmetic", ARITHMETIC_TARGETS),
            AttrRule::unsupported("event", EVENT_TARGETS),
            AttrRule::unsupported("error", ERROR_TARGETS),
            AttrRule::unsupported("must_use", MUST_USE_TARGETS),
            AttrRule::unsupported("payable", PAYABLE_TARGETS),
        ],
    );
}

pub(super) fn validate_for_loop_attrs<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
) {
    validate_attr_rules(
        ctxt,
        attrs,
        target("for loop", None),
        &[
            AttrRule::supported("unroll", UNROLL_FORM, "`#[unroll]` or `#[unroll(never)]`"),
            AttrRule::unsupported("payable", PAYABLE_TARGETS),
        ],
    );
}

pub(super) fn report_payable_on_unsupported_target<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    kind: &'static str,
    name: Option<String>,
) {
    report_unsupported_attr(ctxt, attrs, "payable", target(kind, name), PAYABLE_TARGETS);
}

fn report_indexed_attrs_outside_event_struct<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Struct,
) {
    let Some(fields) = ast.fields() else { return };
    for field in fields {
        report_unsupported_attr(
            ctxt,
            field.attr_list(),
            "indexed",
            target(
                "struct field",
                field.name().map(|name| name.text().to_string()),
            ),
            INDEXED_TARGETS,
        );
    }
}

impl<'db> ItemKind<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Item) {
        let Some(kind) = ast.kind() else {
            return;
        };

        match kind {
            ast::ItemKind::Mod(mod_) => {
                validate_mod_attrs(
                    ctxt,
                    mod_.attr_list(),
                    mod_.name().map(|n| n.text().to_string()),
                );
                Mod::lower_ast(ctxt, mod_);
            }
            ast::ItemKind::Func(fn_) => {
                validate_func_attrs(
                    ctxt,
                    fn_.attr_list(),
                    "fn",
                    fn_.sig().name().map(|name| name.text().to_string()),
                    true,
                );
                Func::lower_ast(ctxt, fn_);
            }
            ast::ItemKind::Struct(struct_) => {
                validate_struct_attrs(
                    ctxt,
                    struct_.attr_list(),
                    struct_.name().map(|name| name.text().to_string()),
                );
                Struct::lower_ast(ctxt, struct_);
            }
            ast::ItemKind::Contract(contract) => {
                validate_unsupported_item_attrs(
                    ctxt,
                    contract.attr_list(),
                    "contract",
                    contract.name().map(|name| name.text().to_string()),
                );
                Contract::lower_ast(ctxt, contract);
            }
            ast::ItemKind::Enum(enum_) => {
                validate_enum_attrs(
                    ctxt,
                    enum_.attr_list(),
                    enum_.name().map(|name| name.text().to_string()),
                );
                Enum::lower_ast(ctxt, enum_);
            }
            ast::ItemKind::Msg(msg) => {
                validate_unsupported_item_attrs(
                    ctxt,
                    msg.attr_list(),
                    "msg",
                    msg.name().map(|name| name.text().to_string()),
                );
                lower_msg_as_mod(ctxt, msg);
            }
            ast::ItemKind::TypeAlias(alias) => {
                validate_unsupported_item_attrs(
                    ctxt,
                    alias.attr_list(),
                    "type alias",
                    alias.alias().map(|name| name.text().to_string()),
                );
                TypeAlias::lower_ast(ctxt, alias);
            }
            ast::ItemKind::Impl(impl_) => {
                validate_unsupported_item_attrs(ctxt, impl_.attr_list(), "impl", None);
                Impl::lower_ast(ctxt, impl_);
            }
            ast::ItemKind::Trait(trait_) => {
                validate_unsupported_item_attrs(
                    ctxt,
                    trait_.attr_list(),
                    "trait",
                    trait_.name().map(|name| name.text().to_string()),
                );
                Trait::lower_ast(ctxt, trait_);
            }
            ast::ItemKind::ImplTrait(impl_trait) => {
                validate_unsupported_item_attrs(ctxt, impl_trait.attr_list(), "impl trait", None);
                ImplTrait::lower_ast(ctxt, impl_trait);
            }
            ast::ItemKind::Const(const_) => {
                validate_unsupported_item_attrs(
                    ctxt,
                    const_.attr_list(),
                    "const",
                    const_.name().map(|name| name.text().to_string()),
                );
                Const::lower_ast(ctxt, const_);
            }
            ast::ItemKind::StaticAssert(assert_) => {
                validate_unsupported_item_attrs(
                    ctxt,
                    assert_.attr_list(),
                    "static assertion",
                    None,
                );
                StaticAssert::lower_ast(ctxt, assert_);
            }
            ast::ItemKind::Use(use_) => {
                validate_unsupported_item_attrs(ctxt, use_.attr_list(), "use", None);
                Use::lower_ast(ctxt, use_);
            }
            ast::ItemKind::Extern(extern_) => {
                validate_unsupported_item_attrs(ctxt, extern_.attr_list(), "extern", None);
                if let Some(extern_block) = extern_.extern_block() {
                    for fn_ in extern_block {
                        validate_func_attrs(
                            ctxt,
                            fn_.attr_list(),
                            "extern fn",
                            fn_.sig().name().map(|name| name.text().to_string()),
                            false,
                        );
                        Func::lower_ast_extern(ctxt, fn_);
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

        validate_module_inner_attrs(ctxt, ast.items().and_then(|items| items.inner_attr_list()));
        let attributes = AttrListId::lower_ast_merged(
            ctxt,
            ast.attr_list(),
            ast.items().and_then(|items| items.inner_attr_list()),
        );
        let vis = super::lower_visibility(&ast);
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
        Self::lower_ast_impl(ctxt, ast, false)
    }

    pub(super) fn lower_ast_extern(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Func) -> Self {
        Self::lower_ast_impl(ctxt, ast, true)
    }

    fn lower_ast_impl(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Func, is_extern: bool) -> Self {
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
        let vis = super::lower_visibility(&ast);
        let is_unsafe = super::lower_is_unsafe(&ast);
        let is_const = ast.const_kw().is_some();
        let modifiers = FuncModifiers::new(vis, is_unsafe, is_const, is_extern);
        let body = ast
            .body()
            .map(|body| Body::lower_ast(ctxt, ast::Expr::cast(body.syntax().clone()).unwrap()));
        let origin = HirOrigin::raw(&ast);
        let top_mod = ctxt.top_mod();

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
            modifiers,
            body,
            top_mod,
            origin,
        );
        ctxt.leave_item_scope(fn_)
    }
}

impl<'db> Struct<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Struct) -> Self {
        let is_event_struct = super::event::is_event_struct(&ast);
        let is_error_struct = super::error::is_error_struct(&ast);

        if is_event_struct && is_error_struct {
            super::error::report_event_error_attr_conflict(ctxt, &ast);
        }
        if is_event_struct {
            return super::event::lower_event_struct(ctxt, ast);
        }
        if is_error_struct {
            report_indexed_attrs_outside_event_struct(ctxt, &ast);
            return super::error::lower_error_struct(ctxt, ast);
        }

        report_indexed_attrs_outside_event_struct(ctxt, &ast);

        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Struct(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = super::lower_visibility(&ast);
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

pub(super) fn lower_uses_clause_opt<'db>(
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
        let vis = super::lower_visibility(&ast);
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
        let vis = super::lower_visibility(&ast);
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
        let idx = ctxt.next_impl_idx();
        let id = ctxt.joined_id(TrackedItemVariant::Impl(idx));
        ctxt.enter_item_scope(id, false);

        // Lower generic params first so they are in scope for type and where-clause lowering.
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let origin = HirOrigin::raw(&ast);

        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                validate_func_attrs(
                    ctxt,
                    impl_item.attr_list(),
                    "fn",
                    impl_item.sig().name().map(|name| name.text().to_string()),
                    true,
                );
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
        let vis = super::lower_visibility(&ast);
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
                        validate_func_attrs(
                            ctxt,
                            func.attr_list(),
                            "fn",
                            func.sig().name().map(|name| name.text().to_string()),
                            true,
                        );
                        Func::lower_ast(ctxt, func);
                    }
                    ast::TraitItemKind::Type(t) => {
                        validate_unsupported_item_attrs(
                            ctxt,
                            t.attr_list(),
                            "type",
                            t.name().map(|name| name.text().to_string()),
                        );
                        types.push(AssocTyDecl::lower_ast(ctxt, t));
                    }
                    ast::TraitItemKind::Const(c) => {
                        validate_unsupported_item_attrs(
                            ctxt,
                            c.attr_list(),
                            "const",
                            c.name().map(|name| name.text().to_string()),
                        );
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
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
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
            attributes,
            name,
            bounds,
            default,
        }
    }
}

impl<'db> ImplTrait<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::ImplTrait) -> Self {
        let idx = ctxt.next_impl_trait_idx();
        let id = ctxt.joined_id(TrackedItemVariant::ImplTrait(idx));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        // Lower generic params first so they are in scope for trait-ref/type and where-clause lowering.
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let trait_ref = TraitRefId::lower_ast_partial(ctxt, ast.trait_ref());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let origin = HirOrigin::raw(&ast);

        let mut types = vec![];
        let mut consts = vec![];
        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                match impl_item.kind() {
                    ast::TraitItemKind::Func(func) => {
                        validate_func_attrs(
                            ctxt,
                            func.attr_list(),
                            "fn",
                            func.sig().name().map(|name| name.text().to_string()),
                            true,
                        );
                        Func::lower_ast(ctxt, func);
                    }
                    ast::TraitItemKind::Type(t) => {
                        validate_unsupported_item_attrs(
                            ctxt,
                            t.attr_list(),
                            "type",
                            t.name().map(|name| name.text().to_string()),
                        );
                        types.push(AssocTyDef::lower_ast(ctxt, t));
                    }
                    ast::TraitItemKind::Const(c) => {
                        validate_unsupported_item_attrs(
                            ctxt,
                            c.attr_list(),
                            "const",
                            c.name().map(|name| name.text().to_string()),
                        );
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
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        AssocTyDef {
            attributes,
            name: IdentId::lower_token_partial(ctxt, ast.name()),
            type_ref: TypeId::lower_ast_partial(ctxt, ast.ty()),
        }
    }
}

impl<'db> AssocConstDecl<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitConstItem) -> Self {
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let default = ast
            .value()
            .map(|expr| crate::hir_def::Partial::Present(Body::lower_ast(ctxt, expr)));
        AssocConstDecl {
            attributes,
            name,
            ty,
            default,
        }
    }
}

impl<'db> AssocConstDef<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitConstItem) -> Self {
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let value = ast.value().map(|expr| Body::lower_ast(ctxt, expr)).into();

        AssocConstDef {
            attributes,
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
        let vis = super::lower_visibility(&ast);
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

impl<'db> StaticAssert<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::StaticAssert) -> Self {
        let idx = ctxt.next_static_assert_idx();
        let id = ctxt.joined_id(TrackedItemVariant::StaticAssert(idx));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let condition_ast = ast.condition();
        let condition = Body::lower_ast_with_variant(
            ctxt,
            condition_ast.clone(),
            TrackedItemVariant::StaticAssertCondition,
            BodyKind::Anonymous,
        );
        let comparison =
            condition_ast.and_then(|expr| StaticAssertComparison::lower_ast(ctxt, expr));
        let origin = HirOrigin::raw(&ast);

        let assert_ = Self::new(
            ctxt.db(),
            id,
            attributes,
            condition,
            comparison,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(assert_)
    }
}

impl<'db> StaticAssertComparison<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, expr: ast::Expr) -> Option<Self> {
        let expr = peel_parens(expr);
        let ast::ExprKind::Bin(bin) = expr.kind() else {
            return None;
        };
        let ast::BinOp::Comp(op) = bin.op()? else {
            return None;
        };

        Some(Self {
            op: CompBinOp::lower_ast(op),
            lhs: Body::lower_ast_with_variant(
                ctxt,
                bin.lhs(),
                TrackedItemVariant::StaticAssertComparisonLhs,
                BodyKind::Anonymous,
            ),
            rhs: Body::lower_ast_with_variant(
                ctxt,
                bin.rhs(),
                TrackedItemVariant::StaticAssertComparisonRhs,
                BodyKind::Anonymous,
            ),
        })
    }
}

fn peel_parens(mut expr: ast::Expr) -> ast::Expr {
    loop {
        match expr.kind() {
            ast::ExprKind::Paren(paren) => match paren.expr() {
                Some(inner) => expr = inner,
                None => return expr,
            },
            _ => return expr,
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
}

impl<'db> FieldDef<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::RecordFieldDef) -> Self {
        report_payable_on_unsupported_target(ctxt, ast.attr_list(), "field", None);
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let type_ref = TypeId::lower_ast_partial(ctxt, ast.ty());
        let vis = super::lower_field_visibility(&ast);

        Self::new(attributes, name, type_ref, vis, false)
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
        report_payable_on_unsupported_target(ctxt, ast.attr_list(), "variant", None);
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
