//! Formatting for patterns.

use pretty::DocAllocator;

use crate::RewriteContext;
use parser::ast::{self, PatKind, prelude::AstNode};
use parser::syntax_kind::SyntaxKind;

use super::types::{
    Doc, ToDoc, TokenPiece, block_list_auto, block_list_spaced_auto, has_comment_tokens, token_doc,
};

impl ToDoc for ast::Pat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.kind() {
            PatKind::WildCard(wildcard) => wildcard.to_doc(ctx),
            PatKind::Rest(rest) => rest.to_doc(ctx),
            PatKind::Lit(lit) => lit.to_doc(ctx),
            PatKind::Tuple(tuple) => tuple.to_doc(ctx),
            PatKind::Path(path) => path.to_doc(ctx),
            PatKind::PathTuple(path_tuple) => path_tuple.to_doc(ctx),
            PatKind::Record(record) => record.to_doc(ctx),
            PatKind::Or(or) => or.to_doc(ctx),
        }
    }
}

impl ToDoc for ast::WildCardPat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        ctx.alloc.text("_")
    }
}

impl ToDoc for ast::RestPat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        ctx.alloc.text("..")
    }
}

impl ToDoc for ast::LitPat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.lit() {
            Some(l) => ctx.alloc.text(ctx.snippet_trimmed(&l)),
            None => ctx.alloc.nil(),
        }
    }
}

impl ToDoc for ast::TuplePat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        self.elems()
            .map(|elems| elems.to_doc(ctx))
            .unwrap_or_else(|| ctx.alloc.text("()"))
    }
}

impl ToDoc for ast::TuplePatElemList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let indent = ctx.config.indent_width as isize;
        block_list_auto(ctx, self.syntax(), "(", ")", ast::Pat::cast, indent, true)
    }
}

impl ToDoc for ast::PathPat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;
        let mut doc = alloc.nil();

        if self.mut_token().is_some() {
            doc = doc.append(alloc.text("mut "));
        }

        if let Some(path) = self.path() {
            doc = doc.append(path.to_doc(ctx));
        }

        doc
    }
}

impl ToDoc for ast::PathTuplePat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let path = match self.path() {
            Some(p) => p.to_doc(ctx),
            None => return alloc.nil(),
        };

        let elems_doc = self
            .elems()
            .map(|elems| elems.to_doc(ctx))
            .unwrap_or_else(|| alloc.text("()"));

        path.append(elems_doc)
    }
}

impl ToDoc for ast::RecordPat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let path = match self.path() {
            Some(p) => p.to_doc(ctx),
            None => return alloc.nil(),
        };

        let fields_doc = self
            .fields()
            .map(|fields| fields.to_doc(ctx))
            .unwrap_or_else(|| alloc.text("{}"));

        path.append(alloc.text(" ")).append(fields_doc)
    }
}

impl ToDoc for ast::RecordPatFieldList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let indent = ctx.config.indent_width as isize;
        block_list_spaced_auto(
            ctx,
            self.syntax(),
            "{",
            "}",
            ast::RecordPatField::cast,
            indent,
            true,
        )
    }
}

impl ToDoc for ast::RecordPatField {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        match (self.name(), self.pat()) {
            (Some(name), Some(pat)) => alloc
                .text(ctx.token(&name))
                .append(alloc.text(": "))
                .append(pat.to_doc(ctx)),
            (Some(name), None) => alloc.text(ctx.token(&name)),
            (None, Some(pat)) => pat.to_doc(ctx),
            (None, None) => alloc.nil(),
        }
    }
}

impl ToDoc for ast::OrPat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if !has_comment_tokens(self.syntax()) {
            let lhs = match self.lhs() {
                Some(l) => l.to_doc(ctx),
                None => return alloc.nil(),
            };
            let rhs = match self.rhs() {
                Some(r) => r.to_doc(ctx),
                None => return lhs,
            };

            return lhs.append(alloc.text(" | ")).append(rhs);
        }

        let indent = ctx.config.indent_width as isize;
        token_doc(
            ctx,
            self.syntax(),
            indent,
            |node| ast::Pat::cast(node).map(|pat| TokenPiece::new(pat.to_doc(ctx))),
            |token| match token.kind() {
                SyntaxKind::Pipe => Some(TokenPiece::new(alloc.text("|")).spaces()),
                _ => None,
            },
        )
    }
}
