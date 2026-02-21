//! Formatting for attributes and doc comments.

use pretty::DocAllocator;

use crate::RewriteContext;
use parser::ast::{self, AttrArgValueKind, AttrKind, prelude::AstNode};

use super::types::{Doc, ToDoc, block_list_auto, intersperse};

impl ToDoc for ast::AttrList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs: Vec<_> = self.iter().map(|attr| attr.to_doc(ctx)).collect();
        if attrs.is_empty() {
            alloc.nil()
        } else {
            intersperse(alloc, attrs, alloc.hardline()).append(alloc.hardline())
        }
    }
}

impl ToDoc for ast::Attr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.kind() {
            AttrKind::Normal(attr) => attr.to_doc(ctx),
            AttrKind::DocComment(attr) => attr.to_doc(ctx),
        }
    }
}

impl ToDoc for ast::NormalAttr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let path = match self.path() {
            Some(p) => p.to_doc(ctx),
            None => return alloc.text("#[]"),
        };

        // Handle both forms:
        // - #[attr(arg1, arg2)]  -> args()
        // - #[attr = value]      -> value()
        let suffix_doc = if let Some(args) = self.args() {
            args.to_doc(ctx)
        } else if let Some(val) = self.value() {
            let val_doc = match val {
                AttrArgValueKind::Ident(tok) => alloc.text(tok.text().to_string()),
                AttrArgValueKind::Lit(lit) => lit.to_doc(ctx),
                AttrArgValueKind::Expr(expr) => expr.to_doc(ctx),
            };
            alloc.text(" = ").append(val_doc)
        } else {
            alloc.nil()
        };

        alloc
            .text("#[")
            .append(path)
            .append(suffix_doc)
            .append(alloc.text("]"))
    }
}

impl ToDoc for ast::DocCommentAttr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        match self.doc() {
            Some(tok) => alloc.text(tok.text().to_string()),
            None => alloc.nil(),
        }
    }
}

impl ToDoc for ast::AttrArgList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let indent = ctx.config.indent_width as isize;
        block_list_auto(
            ctx,
            self.syntax(),
            "(",
            ")",
            ast::AttrArg::cast,
            indent,
            true,
        )
    }
}

impl ToDoc for ast::AttrArg {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let key = match self.key() {
            Some(p) => p.to_doc(ctx),
            None => return alloc.nil(),
        };

        match self.value() {
            Some(val) => {
                let val_doc = match val {
                    AttrArgValueKind::Ident(tok) => alloc.text(tok.text().to_string()),
                    AttrArgValueKind::Lit(lit) => lit.to_doc(ctx),
                    AttrArgValueKind::Expr(expr) => expr.to_doc(ctx),
                };
                key.append(alloc.text(" = ")).append(val_doc)
            }
            None => key,
        }
    }
}

impl ToDoc for ast::Lit {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        ctx.alloc.text(ctx.snippet_trimmed(self))
    }
}
