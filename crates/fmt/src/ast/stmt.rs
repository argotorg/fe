//! Formatting for statements and block expressions.

use crate::{Indent, Rewrite, RewriteContext, RewriteExt, Shape};
use parser::{
    TextRange,
    ast::{self, StmtKind, prelude::AstNode},
    syntax_kind::SyntaxKind,
    syntax_node::NodeOrToken,
};

use super::push_indent;

impl Rewrite for ast::BlockExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        // Empty blocks keep their compact form to match existing style.
        let has_stmt = self.stmts().next().is_some();
        let has_item = self.items().next().is_some();
        let has_comment = self
            .syntax()
            .children_with_tokens()
            .any(|child| matches!(child, NodeOrToken::Token(t) if t.kind() == SyntaxKind::Comment));

        if !has_stmt && !has_item && !has_comment {
            return Some("{}".to_string());
        }

        let outer_indent = shape.indent.indent_width();
        let indent_width = context.config.indent_width;
        let inner_indent = outer_indent + indent_width;
        let inner_shape = Shape::with_width(shape.width, Indent::from_block(inner_indent));

        let mut out = String::new();
        out.push('{');
        out.push('\n');

        let mut children = self.syntax().children_with_tokens().peekable();
        let mut last_stmt_end: Option<parser::TextSize> = None;

        // Skip the leading `{` and any surrounding trivia.
        while let Some(child) = children.peek() {
            match child {
                NodeOrToken::Token(t) if t.kind() == SyntaxKind::LBrace || t.kind().is_trivia() => {
                    children.next();
                }
                _ => break,
            }
        }

        while let Some(child) = children.peek().cloned() {
            match child {
                NodeOrToken::Node(node) => {
                    let node_range = node.text_range();

                    if let Some(prev_end) = last_stmt_end {
                        let gap = TextRange::new(prev_end, node_range.start());
                        let gap_text = context.snippet(gap);
                        let gap_newlines = gap_text.chars().filter(|c| *c == '\n').count();
                        if gap_newlines >= 2 {
                            out.push('\n');
                        }
                    }

                    if let Some(stmt) = ast::Stmt::cast(node.clone()) {
                        // Consume the node.
                        children.next();

                        push_indent(&mut out, inner_indent);
                        let code = stmt.rewrite_or_original(context, inner_shape);
                        out.push_str(&code);

                        // Attach trailing comment on the same line, if it is directly
                        // adjacent (at most whitespace without a newline).
                        if let Some(NodeOrToken::Token(ws)) = children.peek()
                            && ws.kind() == SyntaxKind::WhiteSpace
                        {
                            let text = context.snippet(ws.text_range());
                            if !text.contains('\n') {
                                children.next();
                            }
                        }
                        if let Some(NodeOrToken::Token(tok)) = children.peek()
                            && tok.kind() == SyntaxKind::Comment
                        {
                            let tok = match children.next().unwrap() {
                                NodeOrToken::Token(t) => t,
                                _ => unreachable!(),
                            };
                            let comment = context.snippet(tok.text_range());
                            out.push(' ');
                            out.push_str(comment.trim_start());
                        }

                        out.push('\n');

                        last_stmt_end = Some(node_range.end());
                    } else if let Some(item) = ast::Item::cast(node.clone()) {
                        children.next();
                        push_indent(&mut out, inner_indent);
                        let code = item.rewrite_or_original(context, inner_shape);
                        out.push_str(&code);
                        out.push('\n');

                        last_stmt_end = Some(node_range.end());
                    } else {
                        // Fallback for unexpected nodes: re-use the original snippet.
                        children.next();
                        push_indent(&mut out, inner_indent);
                        out.push_str(context.snippet(node.text_range()).trim());
                        out.push('\n');
                    }
                }
                NodeOrToken::Token(tok) => match tok.kind() {
                    SyntaxKind::RBrace => {
                        // End of block.
                        children.next();
                        break;
                    }
                    SyntaxKind::Comment => {
                        children.next();

                        // Check for blank lines before the comment, just like we do for statements.
                        let tok_range = tok.text_range();
                        if let Some(prev_end) = last_stmt_end {
                            let gap = TextRange::new(prev_end, tok_range.start());
                            let gap_text = context.snippet(gap);
                            let gap_newlines = gap_text.chars().filter(|c| *c == '\n').count();
                            if gap_newlines >= 2 {
                                out.push('\n');
                            }
                        }

                        push_indent(&mut out, inner_indent);
                        let text = context.snippet(tok_range);
                        out.push_str(text.trim_start());
                        out.push('\n');

                        // Update last_stmt_end so subsequent items don't double-count the gap.
                        last_stmt_end = Some(tok_range.end());
                    }
                    SyntaxKind::WhiteSpace => {
                        // Inter-statement blank lines are handled based on the
                        // original source ranges between statements, so we can
                        // ignore whitespace tokens here.
                        children.next();
                    }
                    _ => {
                        // Other punctuation within the block is usually handled by
                        // statement / expression formatters, so we can skip it here.
                        children.next();
                    }
                },
            }
        }

        // Closing brace.
        push_indent(&mut out, outer_indent);
        out.push('}');

        Some(out)
    }
}

impl Rewrite for ast::Stmt {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        match self.kind() {
            StmtKind::Let(let_stmt) => let_stmt.rewrite(context, shape),
            StmtKind::For(for_stmt) => for_stmt.rewrite(context, shape),
            StmtKind::While(while_stmt) => while_stmt.rewrite(context, shape),
            StmtKind::Continue(continue_stmt) => continue_stmt.rewrite(context, shape),
            StmtKind::Break(break_stmt) => break_stmt.rewrite(context, shape),
            StmtKind::Return(ret) => ret.rewrite(context, shape),
            StmtKind::Expr(expr_stmt) => expr_stmt.rewrite(context, shape),
        }
    }
}

impl Rewrite for ast::LetStmt {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let pat = self.pat()?.rewrite_or_original(context, shape);
        let ty = self.type_annotation().map_or(String::new(), |ty| {
            format!(": {}", ty.rewrite_or_original(context, shape))
        });
        let init = self.initializer().map_or(String::new(), |init| {
            format!(" = {}", init.rewrite_or_original(context, shape))
        });

        Some(format!("let {pat}{ty}{init}"))
    }
}

impl Rewrite for ast::ForStmt {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let pat = self.pat()?.rewrite_or_original(context, shape);
        let iterable = self.iterable()?.rewrite_or_original(context, shape);
        let body = self.body()?.rewrite_or_original(context, shape);

        Some(format!("for {pat} in {iterable} {body}"))
    }
}

impl Rewrite for ast::WhileStmt {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let cond = self.cond()?.rewrite_or_original(context, shape);
        let body = self.body()?.rewrite_or_original(context, shape);

        Some(format!("while {cond} {body}"))
    }
}

impl Rewrite for ast::ContinueStmt {
    fn rewrite(&self, _context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some("continue".into())
    }
}

impl Rewrite for ast::BreakStmt {
    fn rewrite(&self, _context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some("break".into())
    }
}

impl Rewrite for ast::ReturnStmt {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let expr = self.expr().map_or(String::new(), |expr| {
            format!(" {}", expr.rewrite_or_original(context, shape))
        });

        Some(format!("return{expr}"))
    }
}

impl Rewrite for ast::ExprStmt {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        Some(self.expr()?.rewrite_or_original(context, shape))
    }
}
