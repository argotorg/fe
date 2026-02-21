//! Formatting for statements and block expressions.

use pretty::DocAllocator;

use crate::RewriteContext;
use parser::ast::{self, StmtKind, prelude::AstNode};
use parser::syntax_kind::SyntaxKind;

use super::expr::{format_chain_with_prefix, is_chain};
use super::types::{Doc, ToDoc, TokenPiece, has_comment_tokens, token_doc};

impl ToDoc for ast::Stmt {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.kind() {
            StmtKind::Let(let_stmt) => let_stmt.to_doc(ctx),
            StmtKind::For(for_stmt) => for_stmt.to_doc(ctx),
            StmtKind::While(while_stmt) => while_stmt.to_doc(ctx),
            StmtKind::Continue(continue_stmt) => continue_stmt.to_doc(ctx),
            StmtKind::Break(break_stmt) => break_stmt.to_doc(ctx),
            StmtKind::Return(ret) => ret.to_doc(ctx),
            StmtKind::Expr(expr_stmt) => expr_stmt.to_doc(ctx),
        }
    }
}

impl ToDoc for ast::LetStmt {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let pat = match self.pat() {
            Some(p) => p.to_doc(ctx),
            None => return alloc.text("let"),
        };

        let ty_doc = self
            .type_annotation()
            .map(|ty| alloc.text(": ").append(ty.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        match self.initializer() {
            Some(init) if is_chain(&init) => {
                // Use BlockDoc to handle the entire let statement as a unit
                let prefix = alloc
                    .text("let ")
                    .append(pat)
                    .append(ty_doc)
                    .append(alloc.text(" = "));
                format_chain_with_prefix(prefix, &init, ctx)
            }
            Some(init) => alloc
                .text("let ")
                .append(pat)
                .append(ty_doc)
                .append(alloc.text(" = "))
                .append(init.to_doc(ctx)),
            None => alloc.text("let ").append(pat).append(ty_doc),
        }
    }
}

impl ToDoc for ast::ForStmt {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if !has_comment_tokens(self.syntax()) {
            let pat = match self.pat() {
                Some(p) => p.to_doc(ctx),
                None => return alloc.text("for"),
            };
            let iterable = match self.iterable() {
                Some(i) => i.to_doc(ctx),
                None => return alloc.text("for ").append(pat),
            };
            let body = match self.body() {
                Some(b) => b.to_doc(ctx),
                None => {
                    return alloc
                        .text("for ")
                        .append(pat)
                        .append(alloc.text(" in "))
                        .append(iterable);
                }
            };

            return alloc
                .text("for ")
                .append(pat)
                .append(alloc.text(" in "))
                .append(iterable)
                .append(alloc.text(" "))
                .append(body);
        }

        let indent = ctx.config.indent_width as isize;
        let mut seen_pat = false;
        let mut expr_count = 0usize;

        token_doc(
            ctx,
            self.syntax(),
            indent,
            |node| {
                if !seen_pat && let Some(pat) = ast::Pat::cast(node.clone()) {
                    seen_pat = true;
                    return Some(TokenPiece::new(pat.to_doc(ctx)));
                }

                let expr = ast::Expr::cast(node)?;
                expr_count += 1;
                let piece = TokenPiece::new(expr.to_doc(ctx));
                Some(if expr_count == 1 {
                    piece.space_after()
                } else {
                    piece
                })
            },
            |token| match token.kind() {
                SyntaxKind::ForKw => Some(TokenPiece::new(alloc.text("for")).space_after()),
                SyntaxKind::InKw => Some(TokenPiece::new(alloc.text("in")).spaces()),
                _ => None,
            },
        )
    }
}

impl ToDoc for ast::WhileStmt {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if !has_comment_tokens(self.syntax()) {
            let cond = match self.cond() {
                Some(c) => c.to_doc(ctx),
                None => return alloc.text("while"),
            };
            let body = match self.body() {
                Some(b) => b.to_doc(ctx),
                None => return alloc.text("while ").append(cond),
            };

            return alloc
                .text("while ")
                .append(cond)
                .append(alloc.text(" "))
                .append(body);
        }

        let indent = ctx.config.indent_width as isize;
        let mut expr_count = 0usize;

        token_doc(
            ctx,
            self.syntax(),
            indent,
            |node| {
                let expr = ast::Expr::cast(node)?;
                expr_count += 1;
                let piece = TokenPiece::new(expr.to_doc(ctx));
                Some(if expr_count == 1 {
                    piece.space_after()
                } else {
                    piece
                })
            },
            |token| match token.kind() {
                SyntaxKind::WhileKw => Some(TokenPiece::new(alloc.text("while")).space_after()),
                _ => None,
            },
        )
    }
}

impl ToDoc for ast::ContinueStmt {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        ctx.alloc.text("continue")
    }
}

impl ToDoc for ast::BreakStmt {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        ctx.alloc.text("break")
    }
}

impl ToDoc for ast::ReturnStmt {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let expr_doc = self
            .expr()
            .map(|expr| alloc.text(" ").append(expr.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        alloc.text("return").append(expr_doc)
    }
}

impl ToDoc for ast::ExprStmt {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.expr() {
            Some(e) => e.to_doc(ctx),
            None => ctx.alloc.nil(),
        }
    }
}
