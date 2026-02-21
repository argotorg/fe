use parser::ast;

use super::FileLowerCtxt;
use crate::core::hir_def::{IdentId, LitKind, PathId, StringId, attr::*};

impl<'db> AttrListId<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::AttrList) -> Self {
        let attrs = ast
            .into_iter()
            .map(|attr| Attr::lower_ast(ctxt, attr))
            .collect::<Vec<_>>();
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
        let args = ast
            .args()
            .map(|args| {
                args.into_iter()
                    .map(|arg| AttrArg::lower_ast(ctxt, arg))
                    .collect()
            })
            .unwrap_or_default();

        Self { path, args }
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
        let value = AttrArgValue::lower_ast_opt(ctxt, ast.value());
        Self { key, value }
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
