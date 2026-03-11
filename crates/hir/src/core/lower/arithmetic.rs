use parser::ast::{self, prelude::*};
use salsa::Accumulator as _;

use super::FileLowerCtxt;

#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArithmeticAttrError {
    pub kind: ArithmeticAttrErrorKind,
    pub file: common::file::File,
    pub primary_range: parser::TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArithmeticAttrErrorKind {
    ArithmeticAttrOnNonFunction { item_kind: &'static str },
    InvalidArithmeticAttrForm,
}

pub(super) fn report_arithmetic_attr_on_non_function_item<'db>(
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
        if !is_arithmetic_attr(&normal) {
            continue;
        }

        ArithmeticAttrError {
            kind: ArithmeticAttrErrorKind::ArithmeticAttrOnNonFunction { item_kind },
            file,
            primary_range: attr.syntax().text_range(),
        }
        .accumulate(db);
    }
}

pub(super) fn report_invalid_function_arithmetic_attrs<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    func: &ast::Func,
) {
    let Some(attrs) = func.attr_list() else {
        return;
    };
    let db = ctxt.db();
    let file = ctxt.top_mod().file(db);

    for attr in attrs {
        let ast::AttrKind::Normal(normal) = attr.kind() else {
            continue;
        };
        if is_arithmetic_attr(&normal) && !is_valid_arithmetic_attr(&normal) {
            ArithmeticAttrError {
                kind: ArithmeticAttrErrorKind::InvalidArithmeticAttrForm,
                file,
                primary_range: attr.syntax().text_range(),
            }
            .accumulate(db);
        }
    }
}

fn is_arithmetic_attr(attr: &ast::NormalAttr) -> bool {
    attr.path().is_some_and(|path| path.text() == "arithmetic")
}

fn is_valid_arithmetic_attr(attr: &ast::NormalAttr) -> bool {
    if attr.value().is_some() {
        return false;
    }

    let Some(args) = attr.args() else {
        return false;
    };
    let mut args = args.into_iter();
    let Some(arg) = args.next() else {
        return false;
    };
    if args.next().is_some() || arg.value().is_some() {
        return false;
    }

    arg.key().is_some_and(|key| {
        let key = key.text();
        key == "checked" || key == "unchecked"
    })
}
