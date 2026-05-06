use parser::{
    TextRange,
    ast::{self, prelude::*},
};

use super::FileLowerCtxt;
use crate::core::hir_def::{IdentId, LitKind, PathId, StringId, attr::*};

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

pub(super) struct LoweredNamedAttrs<'db> {
    pub retained: AttrListId<'db>,
    pub removed: Vec<AstAttrSpec>,
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

pub(super) fn lower_attrs_without_named<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    name: &str,
) -> LoweredNamedAttrs<'db> {
    let db = ctxt.db();
    let mut removed = Vec::new();

    let retained: Vec<Attr<'db>> = attrs
        .into_iter()
        .flatten()
        .filter_map(|attr| match attr.kind() {
            ast::AttrKind::Normal(normal_attr) if normal_attr.is_named(name) => {
                removed.push(AstAttrSpec::lower_ast(normal_attr));
                None
            }
            ast::AttrKind::Normal(_) | ast::AttrKind::DocComment(_) => {
                Some(Attr::lower_ast(ctxt, attr))
            }
        })
        .collect();

    LoweredNamedAttrs {
        retained: AttrListId::new(db, retained),
        removed,
    }
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
            Some(ast::AttrArgValueKind::Expr(expr)) => match expr.kind() {
                ast::ExprKind::Path(path_expr) => PathId::lower_ast_partial(ctxt, path_expr.path())
                    .to_opt()
                    .map(Self::Path),
                _ => None,
            },
            None => None,
        }
    }
}
