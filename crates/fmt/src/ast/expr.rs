//! Formatting for expressions.

use pretty::RcDoc;

use crate::{RewriteContext, Shape};
use parser::ast::{self, GenericArgsOwner, prelude::AstNode};

use super::types::{ToDoc, block_list, block_list_spaced};

impl ToDoc for ast::BinExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let lhs = match self.lhs() {
            Some(e) => e.to_doc(context, shape),
            None => return RcDoc::nil(),
        };
        let op = match self.op() {
            Some(o) => context.snippet_node_or_token(&o.syntax()).to_string(),
            None => return lhs,
        };
        let rhs = match self.rhs() {
            Some(e) => e.to_doc(context, shape),
            None => return lhs,
        };

        lhs.append(RcDoc::text(" "))
            .append(RcDoc::text(op))
            .append(RcDoc::text(" "))
            .append(rhs)
    }
}

impl ToDoc for ast::UnExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let op = match self.op() {
            Some(o) => context.snippet(o.syntax().text_range()).trim().to_string(),
            None => return RcDoc::nil(),
        };
        let expr = match self.expr() {
            Some(e) => e.to_doc(context, shape),
            None => return RcDoc::text(op),
        };

        RcDoc::text(op).append(expr)
    }
}

impl ToDoc for ast::CallArg {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let expr = match self.expr() {
            Some(e) => e.to_doc(context, shape),
            None => return RcDoc::nil(),
        };

        if let Some(label) = self.label() {
            let label_text = context.snippet(label.text_range()).trim().to_string();
            RcDoc::text(label_text)
                .append(RcDoc::text(": "))
                .append(expr)
        } else {
            expr
        }
    }
}

impl ToDoc for ast::CallExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let callee = match self.callee() {
            Some(c) => c.to_doc(context, shape),
            None => return RcDoc::nil(),
        };

        let args: Vec<_> = self
            .args()
            .map(|args| args.into_iter().map(|a| a.to_doc(context, shape)).collect())
            .unwrap_or_default();

        let indent = context.config.indent_width as isize;
        callee.append(block_list("(", ")", args, indent, true))
    }
}

impl ToDoc for ast::MethodCallExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let receiver = match self.receiver() {
            Some(r) => r.to_doc(context, shape),
            None => return RcDoc::nil(),
        };
        let name = match self.method_name() {
            Some(n) => context.snippet(n.text_range()).trim().to_string(),
            None => return receiver,
        };

        let generics = self
            .generic_args()
            .map(|args| args.to_doc(context, shape))
            .unwrap_or_else(RcDoc::nil);

        let args: Vec<_> = self
            .args()
            .map(|args| args.into_iter().map(|a| a.to_doc(context, shape)).collect())
            .unwrap_or_default();

        let indent = context.config.indent_width as isize;
        receiver
            .append(RcDoc::text("."))
            .append(RcDoc::text(name))
            .append(generics)
            .append(block_list("(", ")", args, indent, true))
    }
}

impl ToDoc for ast::RecordField {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let label = match self.label() {
            Some(l) => context.snippet(l.text_range()).trim().to_string(),
            None => return RcDoc::nil(),
        };
        let expr = match self.expr() {
            Some(e) => e.to_doc(context, shape),
            None => return RcDoc::text(label),
        };

        RcDoc::text(label).append(RcDoc::text(": ")).append(expr)
    }
}

impl ToDoc for ast::RecordInitExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let path = match self.path() {
            Some(p) => p.to_doc(context, shape),
            None => return RcDoc::nil(),
        };

        let fields: Vec<_> = self
            .fields()
            .map(|f| {
                f.into_iter()
                    .map(|field| field.to_doc(context, shape))
                    .collect()
            })
            .unwrap_or_default();

        let indent = context.config.indent_width as isize;
        path.append(RcDoc::text(" "))
            .append(block_list_spaced("{", "}", fields, indent, true))
    }
}

impl ToDoc for ast::AssignExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let lhs = match self.lhs_expr() {
            Some(e) => e.to_doc(context, shape),
            None => return RcDoc::nil(),
        };
        let rhs = match self.rhs_expr() {
            Some(e) => e.to_doc(context, shape),
            None => return lhs,
        };

        lhs.append(RcDoc::text(" = ")).append(rhs)
    }
}

impl ToDoc for ast::AugAssignExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let lhs = match self.lhs_expr() {
            Some(e) => e.to_doc(context, shape),
            None => return RcDoc::nil(),
        };
        let op = match self.op() {
            Some(o) => context.snippet_node_or_token(&o.syntax()).to_string(),
            None => return lhs,
        };
        let rhs = match self.rhs_expr() {
            Some(e) => e.to_doc(context, shape),
            None => return lhs,
        };

        lhs.append(RcDoc::text(" "))
            .append(RcDoc::text(op))
            .append(RcDoc::text("= "))
            .append(rhs)
    }
}

impl ToDoc for ast::PathExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.path() {
            Some(p) => p.to_doc(context, shape),
            None => RcDoc::nil(),
        }
    }
}

impl ToDoc for ast::FieldExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let base = match self.receiver() {
            Some(r) => r.to_doc(context, shape),
            None => return RcDoc::nil(),
        };
        let field = match self.name_or_index() {
            Some(n) => context.snippet(n.text_range()).trim().to_string(),
            None => return base,
        };

        base.append(RcDoc::text(".")).append(RcDoc::text(field))
    }
}

impl ToDoc for ast::IndexExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let expr = match self.expr() {
            Some(e) => e.to_doc(context, shape),
            None => return RcDoc::nil(),
        };
        let index = match self.index() {
            Some(i) => i.to_doc(context, shape),
            None => return expr,
        };

        expr.append(RcDoc::text("["))
            .append(index)
            .append(RcDoc::text("]"))
    }
}

impl ToDoc for ast::LitExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, _shape: Shape) -> RcDoc<'a, ()> {
        match self.lit() {
            Some(l) => RcDoc::text(context.snippet_trimmed(&l)),
            None => RcDoc::nil(),
        }
    }
}

impl ToDoc for ast::IfExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let cond = match self.cond() {
            Some(c) => c.to_doc(context, shape),
            None => return RcDoc::nil(),
        };
        let then = match self.then() {
            Some(t) => t.to_doc(context, shape),
            None => return RcDoc::text("if ").append(cond),
        };

        let else_doc = self
            .else_()
            .map(|e| RcDoc::text(" else ").append(e.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        RcDoc::text("if ")
            .append(cond)
            .append(RcDoc::text(" "))
            .append(then)
            .append(else_doc)
    }
}

impl ToDoc for ast::UsesClause {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        if let Some(params) = self.param_list() {
            let params_docs: Vec<_> = params
                .into_iter()
                .map(|p| p.to_doc(context, shape))
                .collect();

            let indent = context.config.indent_width as isize;
            RcDoc::text("uses ").append(block_list("(", ")", params_docs, indent, true))
        } else if let Some(param) = self.param() {
            RcDoc::text("uses ").append(param.to_doc(context, shape))
        } else {
            RcDoc::nil()
        }
    }
}

impl ToDoc for ast::UsesParam {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let mut doc = RcDoc::nil();

        if self.mut_token().is_some() {
            doc = doc.append(RcDoc::text("mut "));
        }

        if let Some(name) = self.name() {
            let name_text = context
                .snippet(name.syntax().text_range())
                .trim()
                .to_string();
            doc = doc.append(RcDoc::text(name_text)).append(RcDoc::text(": "));
        }

        if let Some(path) = self.path() {
            doc = doc.append(path.to_doc(context, shape));
        }

        doc
    }
}

impl ToDoc for ast::MatchExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let scrutinee = match self.scrutinee() {
            Some(s) => s.to_doc(context, shape),
            None => return RcDoc::nil(),
        };

        let arms: Vec<_> = self
            .arms()
            .map(|arms| {
                arms.into_iter()
                    .filter_map(|arm| {
                        let pat = arm.pat()?.to_doc(context, shape);
                        let body = arm.body()?.to_doc(context, shape);
                        Some(
                            pat.append(RcDoc::text(" => "))
                                .append(body)
                                .append(RcDoc::text(",")),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default();

        if arms.is_empty() {
            return RcDoc::text("match ")
                .append(scrutinee)
                .append(RcDoc::text(" {}"));
        }

        let arms_doc = RcDoc::concat(arms.into_iter().map(|arm| RcDoc::hardline().append(arm)));

        RcDoc::text("match ")
            .append(scrutinee)
            .append(RcDoc::text(" {"))
            .append(arms_doc.nest(context.config.indent_width as isize))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
    }
}

impl ToDoc for ast::WithParam {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let path = match self.path() {
            Some(p) => p.to_doc(context, shape),
            None => return RcDoc::nil(),
        };
        let value = match self.value_expr() {
            Some(v) => v.to_doc(context, shape),
            None => return path,
        };

        path.append(RcDoc::text(" = ")).append(value)
    }
}

impl ToDoc for ast::WithExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let params: Vec<_> = self
            .params()
            .map(|p| {
                p.into_iter()
                    .map(|param| param.to_doc(context, shape))
                    .collect()
            })
            .unwrap_or_default();

        let indent = context.config.indent_width as isize;
        let params_doc = block_list("(", ")", params, indent, true);

        let body = match self.body() {
            Some(b) => b.to_doc(context, shape),
            None => return RcDoc::text("with ").append(params_doc),
        };

        RcDoc::text("with ")
            .append(params_doc)
            .append(RcDoc::text(" "))
            .append(body)
    }
}

impl ToDoc for ast::TupleExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let elems: Vec<_> = self
            .elems()
            .flatten()
            .map(|e| e.to_doc(context, shape))
            .collect();

        let indent = context.config.indent_width as isize;
        block_list("(", ")", elems, indent, true)
    }
}

impl ToDoc for ast::ArrayExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let elems: Vec<_> = self
            .elems()
            .flatten()
            .map(|e| e.to_doc(context, shape))
            .collect();

        let indent = context.config.indent_width as isize;
        block_list("[", "]", elems, indent, true)
    }
}

impl ToDoc for ast::ArrayRepExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let val = match self.val() {
            Some(v) => v.to_doc(context, shape),
            None => return RcDoc::nil(),
        };
        let len = match self.len() {
            Some(l) => l.to_doc(context, shape),
            None => return RcDoc::text("[").append(val).append(RcDoc::text("]")),
        };

        RcDoc::text("[")
            .append(val)
            .append(RcDoc::text("; "))
            .append(len)
            .append(RcDoc::text("]"))
    }
}

impl ToDoc for ast::ParenExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let expr = match self.expr() {
            Some(e) => e.to_doc(context, shape),
            None => return RcDoc::text("()"),
        };

        RcDoc::text("(").append(expr).append(RcDoc::text(")"))
    }
}

impl ToDoc for ast::BlockExpr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        use parser::TextRange;
        use parser::syntax_kind::SyntaxKind;
        use parser::syntax_node::NodeOrToken;

        let has_stmt = self.stmts().next().is_some();
        let has_item = self.items().next().is_some();
        let has_comment = self
            .syntax()
            .children_with_tokens()
            .any(|child| matches!(child, NodeOrToken::Token(t) if t.kind() == SyntaxKind::Comment));

        if !has_stmt && !has_item && !has_comment {
            return RcDoc::text("{}");
        }

        // Collect all block elements with their source ranges for blank line detection
        struct BlockElement<'a> {
            doc: RcDoc<'a, ()>,
            range: TextRange,
        }

        let mut elements: Vec<BlockElement<'a>> = Vec::new();

        // Process children in source order to preserve blank lines
        let mut children = self.syntax().children_with_tokens().peekable();

        // Skip leading `{` and trivia
        while let Some(child) = children.peek() {
            match child {
                NodeOrToken::Token(t) if t.kind() == SyntaxKind::LBrace || t.kind().is_trivia() => {
                    children.next();
                }
                _ => break,
            }
        }

        for child in children {
            match child {
                NodeOrToken::Node(node) => {
                    let range = node.text_range();
                    if let Some(stmt) = ast::Stmt::cast(node.clone()) {
                        elements.push(BlockElement {
                            doc: stmt.to_doc(context, shape),
                            range,
                        });
                    } else if let Some(item) = ast::Item::cast(node.clone()) {
                        elements.push(BlockElement {
                            doc: item.to_doc(context, shape),
                            range,
                        });
                    }
                }
                NodeOrToken::Token(tok) => {
                    if tok.kind() == SyntaxKind::Comment {
                        let comment_doc =
                            RcDoc::text(context.snippet(tok.text_range()).trim().to_string());

                        // If the comment is on the same line as the previous element, treat it
                        // as a trailing comment on that line instead of forcing a new line.
                        if let Some(last) = elements.last_mut() {
                            let gap = TextRange::new(last.range.end(), tok.text_range().start());
                            let gap_text = context.snippet(gap);
                            let has_newline = gap_text.chars().any(|c| c == '\n');

                            if !has_newline {
                                last.doc = last
                                    .doc
                                    .clone()
                                    .append(RcDoc::text(" "))
                                    .append(comment_doc);
                                last.range =
                                    TextRange::new(last.range.start(), tok.text_range().end());
                                continue;
                            }
                        }

                        elements.push(BlockElement {
                            doc: comment_doc,
                            range: tok.text_range(),
                        });
                    }
                    // Skip other tokens (whitespace, braces, etc.)
                }
            }
        }

        if elements.is_empty() {
            return RcDoc::text("{}");
        }

        // Build the inner document, inserting extra blank lines where the source had them
        let mut inner = RcDoc::nil();
        let mut prev_end: Option<parser::TextSize> = None;
        let indent = context.config.indent_width as isize;

        for elem in elements {
            // Check if there was a blank line before this element
            let needs_blank_line = if let Some(prev) = prev_end {
                let gap = TextRange::new(prev, elem.range.start());
                let gap_text = context.snippet(gap);
                gap_text.chars().filter(|c| *c == '\n').count() >= 2
            } else {
                false
            };

            if needs_blank_line {
                // Extra hardline for blank line (will have trailing whitespace - cleaned up in post-processing)
                inner = inner.append(RcDoc::hardline());
            }
            inner = inner.append(RcDoc::hardline()).append(elem.doc);
            prev_end = Some(elem.range.end());
        }

        RcDoc::text("{")
            .append(inner.nest(indent))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
    }
}
