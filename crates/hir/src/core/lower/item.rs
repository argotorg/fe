use common::ingot::IngotKind;
use parser::ast::{self, WhereClauseOwner as _, prelude::*};

use super::{
    FileLowerCtxt,
    attr::{AttrForm, AttrRule, AttrTarget, report_unsupported_attr, validate_attr_rules},
};
use crate::{
    hir_def::{
        AttrListId, Body, BodyKind, CompBinOp, EffectParamListId, FuncParamListId, GenericArg,
        GenericArgListId, GenericParamListId, IdentId, ModeMarker, Partial, PathId, TraitRefId,
        TupleTypeId, TypeBound, TypeId, WhereClauseId, item::*, params::KindBound,
    },
    lower::msg::lower_msg_as_mod,
    span::HirOrigin,
};

/// Diagnostics produced while lowering or analyzing a `msg` block.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MsgDiagnostic {
    pub kind: MsgDiagnosticKind,
    pub file: common::file::File,
    /// Range of the primary span.
    pub primary_range: parser::TextRange,
    /// Range of the secondary span, when the diagnostic relates two declarations.
    pub secondary_range: Option<parser::TextRange>,
    pub variant_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MsgDiagnosticKind {
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
    /// A selector signature argument type doesn't match the field's semantic ABI type.
    AbiTypeMismatch {
        selector_ty: String,
        field_name: String,
        field_abi_ty: String,
        /// Fe type with the selector-declared ABI type, if the stdlib has one.
        suggestion: Option<String>,
    },
    /// The selector signature declares a different number of arguments than
    /// the variant has fields.
    ArityMismatch {
        signature_arity: usize,
        field_count: usize,
    },
    /// The `sol("...")` selector signature is not a well-formed Solidity
    /// function signature.
    MalformedSignature { signature: String, reason: String },
    /// A field type has no supported Solidity ABI representation.
    UnsupportedAbiField { ty: String, reason: String },
    /// A field type does not satisfy the traits required by generated ABI helpers.
    MissingAbiTraits {
        ty: String,
        traits: Vec<&'static str>,
    },
}

/// Field modifier errors accumulated during record-field lowering.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldModifierError {
    pub kind: FieldModifierErrorKind,
    pub file: common::file::File,
    pub primary_range: parser::TextRange,
    pub field_kind: &'static str,
    pub field_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FieldModifierErrorKind {
    UnsupportedMut,
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
const POINTER_FORM: AttrForm = AttrForm::SingleArg {
    allow_bare: false,
    allowed_args: &["storage", "transient", "calldata"],
};

const ARITHMETIC_EXPECTED: &str = "`#[arithmetic(checked)]` or `#[arithmetic(unchecked)]`";
const INLINE_EXPECTED: &str = "`#[inline]`, `#[inline(always)]`, or `#[inline(never)]`";
const MUST_USE_EXPECTED: &str = "`#[must_use]`";
const POINTER_EXPECTED: &str =
    "`#[pointer(storage)]`, `#[pointer(transient)]`, or `#[pointer(calldata)]`";

const ARITHMETIC_TARGETS: &str = "functions and modules";
const EVENT_TARGETS: &str = "structs";
const ERROR_TARGETS: &str = "structs";
const ABI_TARGETS: &str = "structs";
const MUST_USE_TARGETS: &str = "functions, structs, and enums";
const VIEW_TARGETS: &str = "structs and enums";
const RAW_PLACE_TARGETS: &str = "functions";
const PAYABLE_TARGETS: &str = "init blocks and recv arms";
const EXPORT_TARGETS: &str = "free functions";
const INDEXED_TARGETS: &str = "event fields";
const POINTER_TARGETS: &str = "type aliases in `core`";

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
            AttrRule::unsupported("abi", ABI_TARGETS),
            AttrRule::unsupported("must_use", MUST_USE_TARGETS),
            AttrRule::unsupported("raw_place", RAW_PLACE_TARGETS),
            AttrRule::unsupported("export", EXPORT_TARGETS),
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
            AttrRule::unsupported("abi", ABI_TARGETS),
        ],
    );
}

/// Where a function is declared, which decides the attributes it takes.
#[derive(Clone, Copy, PartialEq, Eq)]
enum FuncKind {
    Free,
    Associated,
    Extern,
}

fn validate_func_attrs<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    kind: FuncKind,
    name: Option<String>,
) {
    let arithmetic = if kind == FuncKind::Extern {
        AttrRule::unsupported("arithmetic", ARITHMETIC_TARGETS)
    } else {
        AttrRule::supported("arithmetic", ARITHMETIC_FORM, ARITHMETIC_EXPECTED)
    };
    let export = if kind == FuncKind::Free {
        AttrRule::supported("export", BARE_FORM, "`#[export]`")
    } else {
        AttrRule::unsupported("export", EXPORT_TARGETS)
    };
    let target_kind = if kind == FuncKind::Extern {
        "extern fn"
    } else {
        "fn"
    };
    validate_attr_rules(
        ctxt,
        attrs,
        target(target_kind, name),
        &[
            arithmetic,
            export,
            AttrRule::supported("inline", INLINE_FORM, INLINE_EXPECTED),
            AttrRule::supported("must_use", BARE_FORM, MUST_USE_EXPECTED),
            AttrRule::supported("raw_place", BARE_FORM, "`#[raw_place]`"),
            AttrRule::unsupported("view", VIEW_TARGETS),
            AttrRule::unsupported("event", EVENT_TARGETS),
            AttrRule::unsupported("error", ERROR_TARGETS),
            AttrRule::unsupported("abi", ABI_TARGETS),
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
            AttrRule::supported("abi", BARE_FORM, "`#[abi]`"),
            AttrRule::supported("must_use", BARE_FORM, MUST_USE_EXPECTED),
            AttrRule::supported("view", BARE_FORM, "`#[view]`"),
            AttrRule::unsupported("raw_place", RAW_PLACE_TARGETS),
            AttrRule::unsupported("export", EXPORT_TARGETS),
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
            AttrRule::unsupported("abi", ABI_TARGETS),
            AttrRule::supported("must_use", BARE_FORM, MUST_USE_EXPECTED),
            AttrRule::supported("view", BARE_FORM, "`#[view]`"),
            AttrRule::unsupported("raw_place", RAW_PLACE_TARGETS),
            AttrRule::unsupported("export", EXPORT_TARGETS),
            AttrRule::unsupported("payable", PAYABLE_TARGETS),
        ],
    );
}

/// `#[pointer(space)]` names core's built-in pointer into `space`.
fn validate_type_alias_attrs<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    name: Option<String>,
) {
    let db = ctxt.db();
    let pointer = if ctxt.top_mod().ingot(db).kind(db) == IngotKind::Core {
        AttrRule::supported("pointer", POINTER_FORM, POINTER_EXPECTED)
    } else {
        AttrRule::unsupported("pointer", POINTER_TARGETS)
    };
    validate_attr_rules(
        ctxt,
        attrs,
        target("type alias", name),
        &[
            pointer,
            AttrRule::unsupported("arithmetic", ARITHMETIC_TARGETS),
            AttrRule::unsupported("event", EVENT_TARGETS),
            AttrRule::unsupported("error", ERROR_TARGETS),
            AttrRule::unsupported("abi", ABI_TARGETS),
            AttrRule::unsupported("must_use", MUST_USE_TARGETS),
            AttrRule::unsupported("raw_place", RAW_PLACE_TARGETS),
            AttrRule::unsupported("export", EXPORT_TARGETS),
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
            AttrRule::unsupported("abi", ABI_TARGETS),
            AttrRule::unsupported("must_use", MUST_USE_TARGETS),
            AttrRule::unsupported("raw_place", RAW_PLACE_TARGETS),
            AttrRule::unsupported("export", EXPORT_TARGETS),
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

pub(super) fn report_unsupported_field_mut<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    field: &ast::RecordFieldDef,
    field_kind: &'static str,
) {
    let Some(mut_kw) = field.mut_kw() else {
        return;
    };
    let db = ctxt.db();
    ctxt.report(FieldModifierError {
        kind: FieldModifierErrorKind::UnsupportedMut,
        file: ctxt.top_mod().file(db),
        primary_range: mut_kw.text_range(),
        field_kind,
        field_name: field.name().map(|name| name.text().to_string()),
    });
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
                    FuncKind::Free,
                    fn_.sig().name().map(|name| name.text().to_string()),
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
                validate_type_alias_attrs(
                    ctxt,
                    alias.attr_list(),
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
                            FuncKind::Extern,
                            fn_.sig().name().map(|name| name.text().to_string()),
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
        // A body that declares items is lowered with the module, which resolves
        // their imports and collects their impls. Other bodies are lowered when
        // first needed (`Func::body`).
        let eager_body = ast
            .body()
            .filter(ast::BlockExpr::declares_items)
            .map(|body| Body::lower_func_body(ctxt, &ast, body));
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
            eager_body,
            top_mod,
            origin,
        );
        ctxt.leave_item_scope(fn_)
    }
}

impl<'db> Struct<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Struct) -> Self {
        let is_tuple = ast.fields().is_some_and(|fields| fields.is_tuple());
        if is_tuple {
            // Event, error and ABI structs derive their code from named fields.
            let name = ast.name().map(|name| name.text().to_string());
            for attr in ["event", "error", "abi"] {
                report_unsupported_attr(
                    ctxt,
                    ast.attr_list(),
                    attr,
                    target("tuple struct", name.clone()),
                    "record structs",
                );
            }
            report_indexed_attrs_outside_event_struct(ctxt, &ast);
            return Self::lower_plain(ctxt, ast, true);
        }

        let is_event_struct = super::event::is_event_struct(&ast);
        let is_error_struct = super::error::is_error_struct(&ast);
        let is_abi_struct = super::abi_struct::is_abi_struct(&ast);

        if is_event_struct && is_error_struct {
            super::error::report_event_error_attr_conflict(ctxt, &ast);
        }
        if is_abi_struct && (is_event_struct || is_error_struct) {
            super::abi_struct::report_abi_attr_conflict(ctxt, &ast);
        }
        if is_event_struct {
            return super::event::lower_event_struct(ctxt, ast);
        }
        if is_error_struct {
            report_indexed_attrs_outside_event_struct(ctxt, &ast);
            return super::error::lower_error_struct(ctxt, ast);
        }

        report_indexed_attrs_outside_event_struct(ctxt, &ast);
        if is_abi_struct {
            return super::abi_struct::lower_abi_struct(ctxt, ast);
        }

        Self::lower_plain(ctxt, ast, false)
    }

    fn lower_plain(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Struct, is_tuple: bool) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Struct(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = super::lower_visibility(&ast);
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let fields = FieldDefListId::lower_ast_opt_with_context(ctxt, ast.fields(), "struct field");
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
            is_tuple,
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
    let (list, param) = uses.map_or((None, None), |uses| (uses.param_list(), uses.param()));
    lower_uses_params(ctxt, list, param)
}

/// Lowers the effects of a `uses` clause or row: a list or a single entry.
fn lower_uses_params<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    list: Option<ast::UsesParamList>,
    param: Option<ast::UsesParam>,
) -> EffectParamListId<'db> {
    use crate::hir_def::EffectParam;

    let data: Vec<_> = list
        .into_iter()
        .flatten()
        .chain(param)
        .map(|p| EffectParam {
            name: p.name().map(|n| IdentId::lower_token(ctxt, n.syntax())),
            key_ty: TypeId::lower_ast_partial(ctxt, p.ty()),
            mode: if p.mut_token().is_some() {
                ModeMarker::Mut
            } else if p.maybe_mut_token().is_some() {
                ModeMarker::MaybeMut
            } else {
                ModeMarker::None
            },
        })
        .collect();
    EffectParamListId::new(ctxt.db(), data)
}

impl<'db> AssocRow<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitUsesItem) -> Self {
        validate_unsupported_item_attrs(
            ctxt,
            ast.attr_list(),
            "uses",
            ast.name().map(|name| name.text().to_string()),
        );
        AssocRow {
            name: IdentId::lower_token_partial(ctxt, ast.name()),
            effects: ast
                .eq()
                .map(|_| lower_uses_params(ctxt, ast.param_list(), ast.param())),
        }
    }
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

        let mut consts = vec![];
        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                match impl_item.kind() {
                    ast::ImplItemKind::Func(func) => {
                        validate_func_attrs(
                            ctxt,
                            func.attr_list(),
                            FuncKind::Associated,
                            func.sig().name().map(|name| name.text().to_string()),
                        );
                        Func::lower_ast(ctxt, func);
                    }
                    ast::ImplItemKind::Const(c) => {
                        validate_unsupported_item_attrs(
                            ctxt,
                            c.attr_list(),
                            "const",
                            c.name().map(|name| name.text().to_string()),
                        );
                        consts.push(AssocConstDef::lower_ast(ctxt, c));
                    }
                }
            }
        }

        let impl_ = Self::new(
            ctxt.db(),
            id,
            ty,
            attributes,
            generic_params,
            where_clause,
            consts,
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
        let mut rows = vec![];

        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                match impl_item.kind() {
                    ast::TraitItemKind::Func(func) => {
                        validate_func_attrs(
                            ctxt,
                            func.attr_list(),
                            FuncKind::Associated,
                            func.sig().name().map(|name| name.text().to_string()),
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
                    ast::TraitItemKind::Uses(row) => rows.push(AssocRow::lower_ast(ctxt, row)),
                    ast::TraitItemKind::Shape(shape) => {
                        validate_unsupported_item_attrs(
                            ctxt,
                            shape.attr_list(),
                            "shape",
                            shape.name().map(|name| name.text().to_string()),
                        );
                        types.push(AssocTyDecl::lower_shape_ast(ctxt, shape));
                    }
                };
            }
        }

        let trait_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            TraitModifiers {
                vis,
                is_unsafe: ast.unsafe_kw().is_some(),
            },
            generic_params,
            super_traits,
            where_clause,
            types,
            consts,
            rows,
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

    /// An associated shape, `shape Step`: an associated type of the
    /// `shape` kind.
    fn lower_shape_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitShapeItem) -> Self {
        AssocTyDecl {
            attributes: AttrListId::lower_ast_opt(ctxt, ast.attr_list()),
            name: IdentId::lower_token_partial(ctxt, ast.name()),
            bounds: vec![TypeBound::Kind(Partial::Present(KindBound::Shape))],
            default: TypeId::lower_ast_partial(ctxt, ast.ty()).to_opt(),
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
        let mut trait_ref = TraitRefId::lower_ast_partial(ctxt, ast.trait_ref());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let origin = HirOrigin::raw(&ast);

        // The `-> R` of a callable trait's sugar,
        // `impl FnMut(u256) -> u256 for F`, is the implementation's
        // `type Out = R`, not a binding of the trait.
        let mut out_types = vec![];
        if ast
            .trait_ref()
            .is_some_and(|trait_ref| trait_ref.fn_shape().is_some())
            && let Partial::Present(lowered) = trait_ref
            && let Partial::Present(path) = lowered.path(ctxt.db())
            && let Partial::Present(ident) = path.ident(ctxt.db())
        {
            let db = ctxt.db();
            let (outs, args): (Vec<_>, Vec<_>) = path
                .generic_args(db)
                .data(db)
                .iter()
                .cloned()
                .partition(|arg| matches!(arg, GenericArg::AssocType(_)));
            out_types.extend(outs.into_iter().filter_map(|out| match out {
                GenericArg::AssocType(binding) => Some(AssocTyDef {
                    attributes: AttrListId::new(db, Vec::new()),
                    name: binding.name,
                    type_ref: binding.ty,
                    is_shape: false,
                }),
                _ => None,
            }));
            let args = GenericArgListId::new(db, args, true);
            let path = match path.parent(db) {
                Some(parent) => parent.push_ident_args(db, ident, args),
                None => PathId::from_ident(db, ident),
            };
            trait_ref = Partial::Present(TraitRefId::new(db, Partial::Present(path)));
        }

        let mut types = vec![];
        let mut consts = vec![];
        let mut rows = vec![];
        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                match impl_item.kind() {
                    ast::TraitItemKind::Func(func) => {
                        validate_func_attrs(
                            ctxt,
                            func.attr_list(),
                            FuncKind::Associated,
                            func.sig().name().map(|name| name.text().to_string()),
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
                    ast::TraitItemKind::Uses(row) => rows.push(AssocRow::lower_ast(ctxt, row)),
                    ast::TraitItemKind::Shape(shape) => {
                        validate_unsupported_item_attrs(
                            ctxt,
                            shape.attr_list(),
                            "shape",
                            shape.name().map(|name| name.text().to_string()),
                        );
                        types.push(AssocTyDef::lower_shape_ast(ctxt, shape));
                    }
                };
            }
        }

        out_types.retain(|out_type: &AssocTyDef| {
            !types.iter().any(|defined| defined.name == out_type.name)
        });
        types.extend(out_types);
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
            rows,
            ImplTraitModifiers {
                is_unsafe: ast.unsafe_kw().is_some(),
                selection_name: IdentId::lower_token_partial(ctxt, ast.selection_name()).to_opt(),
                vis: super::lower_visibility(&ast),
            },
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
            is_shape: false,
        }
    }

    fn lower_shape_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitShapeItem) -> Self {
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        AssocTyDef {
            attributes,
            name: IdentId::lower_token_partial(ctxt, ast.name()),
            type_ref: TypeId::lower_ast_partial(ctxt, ast.ty()),
            is_shape: true,
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
            vis: super::lower_visibility(&ast),
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
            None,
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
                None,
                TrackedItemVariant::StaticAssertComparisonLhs,
                BodyKind::Anonymous,
            ),
            rhs: Body::lower_ast_with_variant(
                ctxt,
                bin.rhs(),
                None,
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
    fn lower_ast_with_context(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: ast::RecordFieldDefList,
        field_kind: &'static str,
    ) -> Self {
        let is_tuple = ast.is_tuple();
        let fields = ast
            .into_iter()
            .enumerate()
            .map(|(idx, field)| {
                let mut field = FieldDef::lower_ast_with_context(ctxt, field, field_kind);
                if is_tuple {
                    // Tuple fields are named by their position, which is what
                    // `s.0` resolves against.
                    field.name = Partial::Present(IdentId::new(ctxt.db(), idx.to_string()));
                }
                field
            })
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), fields)
    }

    pub(super) fn lower_ast_opt_with_context(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::RecordFieldDefList>,
        field_kind: &'static str,
    ) -> Self {
        ast.map_or(Self::new(ctxt.db(), Vec::new()), |ast| {
            Self::lower_ast_with_context(ctxt, ast, field_kind)
        })
    }
}

impl<'db> FieldDef<'db> {
    fn lower_ast_with_context(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: ast::RecordFieldDef,
        field_kind: &'static str,
    ) -> Self {
        report_payable_on_unsupported_target(ctxt, ast.attr_list(), "field", None);
        report_unsupported_attr(
            ctxt,
            ast.attr_list(),
            "slot",
            target(field_kind, None),
            super::contract::SLOT_TARGETS,
        );
        report_unsupported_field_mut(ctxt, &ast, field_kind);
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let type_ref = TypeId::lower_ast_partial(ctxt, ast.ty());
        let vis = super::lower_field_visibility(&ast);

        Self {
            is_unsafe: ast.unsafe_kw().is_some(),
            ..Self::new(attributes, name, type_ref, vis, false, false)
        }
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
        ast.map_or(Self::new(ctxt.db(), Vec::new()), |ast| {
            Self::lower_ast(ctxt, ast)
        })
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
            ast::VariantKind::Record(r) => VariantKind::Record(
                FieldDefListId::lower_ast_with_context(ctxt, r, "enum variant field"),
            ),
        };

        Self {
            attributes,
            name,
            kind,
        }
    }
}
