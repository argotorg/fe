//! Formatting for attributes and doc comments.

use pretty::DocAllocator;

use crate::RewriteContext;
use parser::ast::{self, AttrArgValueKind, AttrKind, prelude::AstNode};
use parser::syntax_kind::SyntaxKind;
use parser::syntax_node::NodeOrToken;

use super::types::{Doc, ToDoc, block_list_auto, follows_attr_list};

impl ToDoc for ast::AttrList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let Some(mut doc) = attr_lines(self, ctx) else {
            return alloc.nil();
        };

        // Comments between the attributes and the rest of the item are
        // children of the item, not of the list; keep them with the list.
        for comment in comments_after_attr_list(self) {
            doc = doc
                .append(alloc.hardline())
                .append(alloc.text(ctx.snippet(comment.text_range()).trim_end()));
        }

        doc.append(alloc.hardline())
    }
}

/// Renders the attributes of a list one per line, without a line break after
/// the last one. Plain comments inside the list are kept where they are: on
/// their own line, or after the attribute they follow on the same line.
/// Returns `None` for an empty list.
pub(crate) fn attr_lines<'a>(list: &ast::AttrList, ctx: &'a RewriteContext<'a>) -> Option<Doc<'a>> {
    let alloc = &ctx.alloc;

    let mut doc: Option<Doc<'a>> = None;
    let mut line_break = false;
    for child in list.syntax().children_with_tokens() {
        let (entry, is_comment) = match child {
            NodeOrToken::Node(node) => match ast::Attr::cast(node) {
                Some(attr) => (attr.to_doc(ctx), false),
                None => continue,
            },
            NodeOrToken::Token(token) => match token.kind() {
                SyntaxKind::Newline => {
                    line_break = true;
                    continue;
                }
                SyntaxKind::Comment => {
                    (alloc.text(ctx.snippet(token.text_range()).trim_end()), true)
                }
                _ => continue,
            },
        };

        doc = Some(match doc {
            None => entry,
            Some(doc) if is_comment && !line_break => doc.append(alloc.text(" ")).append(entry),
            Some(doc) => doc.append(alloc.hardline()).append(entry),
        });
        line_break = false;
    }
    doc
}

/// Plain comments that directly follow an item's attribute list, before the
/// rest of the item (`#[attr]` / `// note` / `fn f() {}`).
fn comments_after_attr_list(list: &ast::AttrList) -> impl Iterator<Item = parser::SyntaxToken> {
    std::iter::successors(list.syntax().next_sibling_or_token(), |el| {
        el.next_sibling_or_token()
    })
    .map_while(|el| el.into_token())
    .take_while(|token| {
        matches!(
            token.kind(),
            SyntaxKind::WhiteSpace | SyntaxKind::Newline | SyntaxKind::Comment
        )
    })
    .filter(|token| token.kind() == SyntaxKind::Comment && follows_attr_list(token))
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

        // Inner attributes (`#![...]`) have a `!` after the `#`.
        let open = if is_inner_attr(self) { "#![" } else { "#[" };

        alloc
            .text(open)
            .append(path)
            .append(suffix_doc)
            .append(alloc.text("]"))
    }
}

fn is_inner_attr(attr: &ast::NormalAttr) -> bool {
    let mut tokens = attr
        .syntax()
        .children_with_tokens()
        .filter_map(|child| child.into_token())
        .filter(|token| !token.kind().is_trivia() && token.kind() != SyntaxKind::Newline);
    matches!(
        (
            tokens.next().map(|t| t.kind()),
            tokens.next().map(|t| t.kind())
        ),
        (Some(SyntaxKind::Pound), Some(SyntaxKind::Not))
    )
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
