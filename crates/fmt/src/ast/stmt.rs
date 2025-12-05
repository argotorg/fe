//! Formatting for statements and block expressions.

use pretty::RcDoc;

use crate::{RewriteContext, Shape};
use parser::ast::{self, StmtKind};

use super::expr::{format_chain_with_prefix, is_chain};
use super::types::ToDoc;

impl ToDoc for ast::Stmt {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.kind() {
            StmtKind::Let(let_stmt) => let_stmt.to_doc(context, shape),
            StmtKind::For(for_stmt) => for_stmt.to_doc(context, shape),
            StmtKind::While(while_stmt) => while_stmt.to_doc(context, shape),
            StmtKind::Continue(continue_stmt) => continue_stmt.to_doc(context, shape),
            StmtKind::Break(break_stmt) => break_stmt.to_doc(context, shape),
            StmtKind::Return(ret) => ret.to_doc(context, shape),
            StmtKind::Expr(expr_stmt) => expr_stmt.to_doc(context, shape),
        }
    }
}

impl ToDoc for ast::LetStmt {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let pat = match self.pat() {
            Some(p) => p.to_doc(context, shape),
            None => return RcDoc::text("let"),
        };

        let ty_doc = self
            .type_annotation()
            .map(|ty| RcDoc::text(": ").append(ty.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        // Build the prefix: "let pat: ty = " or "let pat = "
        let prefix = RcDoc::text("let ")
            .append(pat.clone())
            .append(ty_doc.clone());

        let init_doc = match self.initializer() {
            Some(init) if is_chain(&init) => {
                // Compute prefix width: "let " + pat + ty_doc + " = "
                let prefix_width = {
                    let mut buf = Vec::new();
                    let _ = prefix.clone().render(usize::MAX, &mut buf);
                    String::from_utf8(buf).map(|s| s.len()).unwrap_or(0) + 3 // + " = "
                };
                RcDoc::text(" = ").append(format_chain_with_prefix(
                    &init,
                    prefix_width,
                    context,
                    shape,
                ))
            }
            Some(init) => RcDoc::text(" = ").append(init.to_doc(context, shape)),
            None => RcDoc::nil(),
        };

        prefix.append(init_doc)
    }
}

impl ToDoc for ast::ForStmt {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let pat = match self.pat() {
            Some(p) => p.to_doc(context, shape),
            None => return RcDoc::text("for"),
        };
        let iterable = match self.iterable() {
            Some(i) => i.to_doc(context, shape),
            None => return RcDoc::text("for ").append(pat),
        };
        let body = match self.body() {
            Some(b) => b.to_doc(context, shape),
            None => {
                return RcDoc::text("for ")
                    .append(pat)
                    .append(RcDoc::text(" in "))
                    .append(iterable);
            }
        };

        RcDoc::text("for ")
            .append(pat)
            .append(RcDoc::text(" in "))
            .append(iterable)
            .append(RcDoc::text(" "))
            .append(body)
    }
}

impl ToDoc for ast::WhileStmt {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let cond = match self.cond() {
            Some(c) => c.to_doc(context, shape),
            None => return RcDoc::text("while"),
        };
        let body = match self.body() {
            Some(b) => b.to_doc(context, shape),
            None => return RcDoc::text("while ").append(cond),
        };

        RcDoc::text("while ")
            .append(cond)
            .append(RcDoc::text(" "))
            .append(body)
    }
}

impl ToDoc for ast::ContinueStmt {
    fn to_doc<'a>(&self, _context: &RewriteContext<'_>, _shape: Shape) -> RcDoc<'a, ()> {
        RcDoc::text("continue")
    }
}

impl ToDoc for ast::BreakStmt {
    fn to_doc<'a>(&self, _context: &RewriteContext<'_>, _shape: Shape) -> RcDoc<'a, ()> {
        RcDoc::text("break")
    }
}

impl ToDoc for ast::ReturnStmt {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let expr_doc = self
            .expr()
            .map(|expr| RcDoc::text(" ").append(expr.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        RcDoc::text("return").append(expr_doc)
    }
}

impl ToDoc for ast::ExprStmt {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.expr() {
            Some(e) => e.to_doc(context, shape),
            None => RcDoc::nil(),
        }
    }
}
