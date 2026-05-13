use parser::ast::{self, prelude::AstNode as _};
use salsa::Accumulator as _;

use super::FileLowerCtxt;
use crate::hir_def::{
    AttrListId, KeywordAttrSpec,
    attr::{Attr, parse_marker_attr_spec},
};

/// Payable-related errors accumulated during lowering / validation.
#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PayableError {
    pub kind: PayableErrorKind,
    pub file: common::file::File,
    pub primary_range: parser::TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PayableErrorKind {
    /// `#[payable]` placed on an item that doesn't support it.
    PayableAttrOnUnsupportedItem { item_kind: &'static str },
    /// `#[payable]` placed on a msg variant instead of the corresponding recv arm.
    PayableAttrOnMsgVariant,
    /// `#[payable]` used with arguments or value, e.g. `#[payable(foo)]` or `#[payable = 1]`.
    InvalidPayableAttrForm,
    /// Unknown attribute on an `init` block or `recv` arm (only `#[payable]` is allowed).
    UnknownAttrOnContractEntry {
        attr_name: String,
        entry_kind: &'static str,
    },
}

/// Report an error if `#[payable]` appears on an item that doesn't support it
/// (anything other than a recv arm or init block).
pub(super) fn report_payable_attr_on_unsupported_item<'db>(
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
        let is_payable = normal.path().is_some_and(|p| p.text() == "payable");
        if !is_payable {
            continue;
        }

        PayableError {
            kind: PayableErrorKind::PayableAttrOnUnsupportedItem { item_kind },
            file,
            primary_range: attr.syntax().text_range(),
        }
        .accumulate(db);
    }
}

/// Report a targeted error if `#[payable]` appears on a msg variant.
pub(super) fn report_payable_attr_on_msg_variant<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
) {
    let Some(attrs) = attrs else { return };
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);

    for attr in attrs {
        let ast::AttrKind::Normal(normal) = attr.kind() else {
            continue;
        };
        let is_payable = normal.path().is_some_and(|p| p.text() == "payable");
        if !is_payable {
            continue;
        }

        PayableError {
            kind: PayableErrorKind::PayableAttrOnMsgVariant,
            file,
            primary_range: attr.syntax().text_range(),
        }
        .accumulate(db);
    }
}

/// Report an error for unsupported normal attributes on contract entry points.
pub(super) fn report_unknown_attrs_on_contract_entry<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    entry_kind: &'static str,
) {
    let Some(attrs) = attrs else { return };
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);

    for attr in attrs {
        let ast::AttrKind::Normal(normal) = attr.kind() else {
            // Doc comments are always allowed.
            continue;
        };
        let attr_name = normal
            .path()
            .map(|p| p.text().to_string())
            .unwrap_or_default();
        if attr_name == "payable" || (entry_kind == "recv block" && attr_name == "abi") {
            continue;
        }

        PayableError {
            kind: PayableErrorKind::UnknownAttrOnContractEntry {
                attr_name,
                entry_kind,
            },
            file,
            primary_range: attr.syntax().text_range(),
        }
        .accumulate(db);
    }
}

/// Lower contract-entry attributes and report malformed `#[payable]` forms
/// through the shared lowered-attribute validation pathway.
pub(super) fn lower_contract_entry_attrs_opt<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
) -> AttrListId<'db> {
    let lowered_attrs = AttrListId::lower_ast_opt(ctxt, attrs.clone());
    report_invalid_payable_attr_forms(ctxt, attrs, lowered_attrs);
    lowered_attrs
}

fn report_invalid_payable_attr_forms<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    lowered_attrs: AttrListId<'db>,
) {
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);
    let ranges = attrs
        .into_iter()
        .flatten()
        .filter_map(|attr| {
            let ast::AttrKind::Normal(normal) = attr.kind() else {
                return None;
            };
            normal
                .path()
                .is_some_and(|path| path.text() == "payable")
                .then_some(attr.syntax().text_range())
        })
        .collect::<Vec<_>>();
    let specs = payable_attr_specs(db, lowered_attrs);

    for (range, spec) in ranges.into_iter().zip(specs) {
        if parse_marker_attr_spec(&spec).is_err() {
            PayableError {
                kind: PayableErrorKind::InvalidPayableAttrForm,
                file,
                primary_range: range,
            }
            .accumulate(db);
        }
    }
}

fn payable_attr_specs<'db>(
    db: &'db dyn crate::HirDb,
    attrs: AttrListId<'db>,
) -> Vec<KeywordAttrSpec> {
    attrs
        .data(db)
        .iter()
        .filter_map(|attr| {
            let Attr::Normal(normal_attr) = attr else {
                return None;
            };
            if normal_attr
                .path
                .to_opt()
                .and_then(|path| path.as_ident(db))
                .is_none_or(|ident| ident.data(db) != "payable")
            {
                return None;
            }

            Some(normal_attr.keyword_attr_spec(db))
        })
        .collect()
}
