use parser::{
    TextRange,
    ast::{self, prelude::*},
};
use salsa::Accumulator as _;

use super::FileLowerCtxt;
use crate::core::hir_def::{IdentId, LitKind, PathId, StringId, attr::*};

#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttrMisuseError {
    pub kind: AttrMisuseErrorKind,
    pub file: common::file::File,
    pub primary_range: TextRange,
    pub attr_name: String,
    pub target: &'static str,
    pub item_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttrMisuseErrorKind {
    UnsupportedTarget { supported_targets: &'static str },
    InvalidForm { expected_form: &'static str },
    Duplicate,
    UnknownInRestrictedContext { expected_attrs: &'static str },
}

#[derive(Debug, Clone, Copy)]
pub(super) enum AttrForm {
    Bare,
    SingleArg {
        allow_bare: bool,
        allowed_args: &'static [&'static str],
    },
}

impl AttrForm {
    fn accepts(self, attr: &AstAttrSpec) -> bool {
        match self {
            Self::Bare => attr.is_bare(),
            Self::SingleArg {
                allow_bare,
                allowed_args,
            } => {
                if attr.is_bare() {
                    return allow_bare;
                }
                if attr.value.is_some() || !attr.has_args || attr.args.len() != 1 {
                    return false;
                }
                let arg = &attr.args[0];
                arg.value.is_none()
                    && arg
                        .key
                        .as_ref()
                        .is_some_and(|key| allowed_args.contains(&key.as_str()))
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) enum AttrSupport {
    Supported,
    Unsupported { supported_targets: &'static str },
}

#[derive(Debug, Clone, Copy)]
pub(super) struct AttrRule {
    pub name: &'static str,
    pub form: AttrForm,
    pub expected_form: &'static str,
    pub singleton: bool,
    pub support: AttrSupport,
}

#[derive(Debug, Clone)]
pub(super) struct AttrTarget {
    pub kind: &'static str,
    pub name: Option<String>,
}

impl AttrRule {
    pub(super) const fn supported(
        name: &'static str,
        form: AttrForm,
        expected_form: &'static str,
    ) -> Self {
        Self {
            name,
            form,
            expected_form,
            singleton: true,
            support: AttrSupport::Supported,
        }
    }

    pub(super) const fn unsupported(name: &'static str, supported_targets: &'static str) -> Self {
        Self {
            name,
            form: AttrForm::Bare,
            expected_form: "",
            singleton: true,
            support: AttrSupport::Unsupported { supported_targets },
        }
    }
}

impl AttrTarget {
    pub(super) fn new(kind: &'static str, name: Option<String>) -> Self {
        Self { kind, name }
    }
}

#[derive(Debug, Clone)]
pub(super) struct AstAttrArgSpec {
    pub key: Option<String>,
    pub value: Option<ast::AttrArgValueKind>,
}

#[derive(Debug, Clone)]
pub(super) struct AstAttrSpec {
    pub range: TextRange,
    pub value: Option<ast::AttrArgValueKind>,
    pub has_args: bool,
    pub args: Vec<AstAttrArgSpec>,
}

impl AstAttrSpec {
    pub(super) fn is_bare(&self) -> bool {
        self.value.is_none() && !self.has_args
    }

    fn lower_ast(ast: ast::NormalAttr) -> Self {
        let args = ast.args();
        Self {
            range: ast.syntax().text_range(),
            value: ast.value(),
            has_args: args.is_some(),
            args: args
                .map(|args| {
                    args.into_iter()
                        .map(|arg| AstAttrArgSpec {
                            key: arg.key().map(|key| key.text().to_string()),
                            value: arg.value(),
                        })
                        .collect()
                })
                .unwrap_or_default(),
        }
    }
}

pub(super) fn has_named_attr(attrs: Option<ast::AttrList>, name: &str) -> bool {
    attrs.is_some_and(|attrs| attrs.normal_attrs_named(name).next().is_some())
}

pub(super) fn named_attr_specs(attrs: Option<ast::AttrList>, name: &str) -> Vec<AstAttrSpec> {
    attrs
        .into_iter()
        .flatten()
        .filter_map(|attr| match attr.kind() {
            ast::AttrKind::Normal(normal_attr) if normal_attr.is_named(name) => {
                Some(AstAttrSpec::lower_ast(normal_attr))
            }
            ast::AttrKind::Normal(_) | ast::AttrKind::DocComment(_) => None,
        })
        .collect()
}

pub(super) fn validate_attr_rules<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    target: AttrTarget,
    rules: &[AttrRule],
) {
    for rule in rules {
        let specs = named_attr_specs(attrs.clone(), rule.name);
        if specs.is_empty() {
            continue;
        }

        match rule.support {
            AttrSupport::Unsupported { supported_targets } => {
                for spec in specs {
                    report_attr_misuse(
                        ctxt,
                        spec.range,
                        rule.name.to_string(),
                        target.clone(),
                        AttrMisuseErrorKind::UnsupportedTarget { supported_targets },
                    );
                }
            }
            AttrSupport::Supported => {
                if let Some(spec) = specs.iter().find(|spec| !rule.form.accepts(spec)) {
                    report_attr_misuse(
                        ctxt,
                        spec.range,
                        rule.name.to_string(),
                        target.clone(),
                        AttrMisuseErrorKind::InvalidForm {
                            expected_form: rule.expected_form,
                        },
                    );
                }

                if rule.singleton
                    && let Some(spec) = specs.get(1)
                {
                    report_attr_misuse(
                        ctxt,
                        spec.range,
                        rule.name.to_string(),
                        target.clone(),
                        AttrMisuseErrorKind::Duplicate,
                    );
                }
            }
        }
    }
}

pub(super) fn validate_unknown_attrs_in_restricted_context<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    target: AttrTarget,
    allowed_names: &[&str],
    expected_attrs: &'static str,
) {
    let Some(attrs) = attrs else { return };

    for attr in attrs {
        let ast::AttrKind::Normal(normal_attr) = attr.kind() else {
            continue;
        };
        let attr_name = normal_attr
            .path()
            .map(|path| path.text().to_string())
            .unwrap_or_default();
        if allowed_names.contains(&attr_name.as_str()) {
            continue;
        }
        report_attr_misuse(
            ctxt,
            attr.syntax().text_range(),
            attr_name,
            target.clone(),
            AttrMisuseErrorKind::UnknownInRestrictedContext { expected_attrs },
        );
    }
}

pub(super) fn report_unsupported_attr<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    attr_name: &'static str,
    target: AttrTarget,
    supported_targets: &'static str,
) {
    validate_attr_rules(
        ctxt,
        attrs,
        target,
        &[AttrRule::unsupported(attr_name, supported_targets)],
    );
}

fn report_attr_misuse<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    primary_range: TextRange,
    attr_name: String,
    target: AttrTarget,
    kind: AttrMisuseErrorKind,
) {
    let db = ctxt.db();
    AttrMisuseError {
        kind,
        file: ctxt.top_mod().file(db),
        primary_range,
        attr_name,
        target: target.kind,
        item_name: target.name,
    }
    .accumulate(db);
}

pub(super) fn lower_attrs_without_named<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    name: &str,
) -> AttrListId<'db> {
    let db = ctxt.db();

    let retained: Vec<Attr<'db>> = attrs
        .into_iter()
        .flatten()
        .filter_map(|attr| match attr.kind() {
            ast::AttrKind::Normal(normal_attr) if normal_attr.is_named(name) => None,
            ast::AttrKind::Normal(_) | ast::AttrKind::DocComment(_) => {
                Some(Attr::lower_ast(ctxt, attr))
            }
        })
        .collect();

    AttrListId::new(db, retained)
}

impl<'db> AttrListId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::AttrList) -> Self {
        let attrs = ast
            .into_iter()
            .map(|attr| Attr::lower_ast(ctxt, attr))
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), attrs)
    }

    pub(super) fn lower_ast_merged(
        ctxt: &mut FileLowerCtxt<'db>,
        first: Option<ast::AttrList>,
        second: Option<ast::AttrList>,
    ) -> Self {
        let mut attrs = Vec::new();
        if let Some(first) = first {
            attrs.extend(first.into_iter().map(|attr| Attr::lower_ast(ctxt, attr)));
        }
        if let Some(second) = second {
            attrs.extend(second.into_iter().map(|attr| Attr::lower_ast(ctxt, attr)));
        }
        Self::new(ctxt.db(), attrs)
    }

    pub(super) fn lower_ast_opt(ctxt: &mut FileLowerCtxt<'db>, ast: Option<ast::AttrList>) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or_else(|| Self::new(ctxt.db(), vec![]))
    }
}

impl<'db> Attr<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Attr) -> Self {
        match ast.kind() {
            ast::AttrKind::Normal(attr) => NormalAttr::lower_ast(ctxt, attr).into(),
            ast::AttrKind::DocComment(attr) => DocCommentAttr::lower_ast(ctxt, attr).into(),
        }
    }
}

impl<'db> NormalAttr<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::NormalAttr) -> Self {
        let path = PathId::lower_ast_partial(ctxt, ast.path());
        let value = ast.value();
        let has_value = value.is_some();
        let value = AttrArgValue::lower_ast_opt(ctxt, value);
        let args = ast.args();
        let has_args = args.is_some();
        let args = args
            .map(|args| {
                args.into_iter()
                    .map(|arg| AttrArg::lower_ast(ctxt, arg))
                    .collect()
            })
            .unwrap_or_default();

        Self {
            path,
            value,
            has_value,
            has_args,
            args,
        }
    }
}

impl<'db> DocCommentAttr<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::DocCommentAttr) -> Self {
        let text = ast
            .doc()
            .map(|doc| doc.text()[3..].to_string())
            .unwrap_or_default();
        Self {
            text: StringId::new(ctxt.db(), text),
        }
    }
}

impl<'db> AttrArg<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::AttrArg) -> Self {
        let key = PathId::lower_ast_partial(ctxt, ast.key());
        let value = ast.value();
        let has_value = value.is_some();
        let value = AttrArgValue::lower_ast_opt(ctxt, value);
        Self {
            key,
            value,
            has_value,
        }
    }
}

impl<'db> AttrArgValue<'db> {
    pub(super) fn lower_ast_opt(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::AttrArgValueKind>,
    ) -> Option<Self> {
        match ast {
            Some(ast::AttrArgValueKind::Ident(token)) => {
                Some(Self::Ident(IdentId::lower_token(ctxt, token)))
            }
            Some(ast::AttrArgValueKind::Lit(lit)) => Some(Self::Lit(LitKind::lower_ast(ctxt, lit))),
            Some(ast::AttrArgValueKind::Expr(_)) => None,
            None => None,
        }
    }
}
