use crate::{
    Indent, ListFormatting, ListTactic, Rewrite, RewriteContext, RewriteExt, Shape, format_list,
};
use parser::{
    TextRange,
    ast::{
        self, AttrListOwner, ExprKind, GenericArgsOwner, GenericParamsOwner, ItemKind,
        ItemModifierOwner, PatKind, StmtKind, TypeKind, WhereClauseOwner, prelude::AstNode,
    },
    syntax_kind::SyntaxKind,
    syntax_node::NodeOrToken,
};

fn write_attrs<N: AttrListOwner + AstNode>(
    node: &N,
    context: &RewriteContext<'_>,
    out: &mut String,
) {
    if let Some(attrs) = node.attr_list() {
        let text = context.snippet(attrs.syntax().text_range());
        out.push_str(text.trim_end());
        out.push('\n');
    }
}

fn write_item_modifier<N: ItemModifierOwner + AstNode>(node: &N, out: &mut String) {
    if let Some(modifier) = node.modifier() {
        if modifier.pub_kw().is_some() {
            out.push_str("pub ");
        }
        if modifier.unsafe_kw().is_some() {
            out.push_str("unsafe ");
        }
    }
}

fn write_generics<N: GenericParamsOwner + AstNode>(
    node: &N,
    context: &RewriteContext<'_>,
    out: &mut String,
) {
    if let Some(generics) = node.generic_params() {
        out.push_str(&context.snippet_trimmed(&generics));
    }
}

fn write_where_clause<N: WhereClauseOwner + AstNode>(
    node: &N,
    context: &RewriteContext<'_>,
    out: &mut String,
) {
    if let Some(where_clause) = node.where_clause() {
        let text = context.snippet_trimmed(&where_clause);
        out.push(' ');
        out.push_str(&text);
    }
}

impl Rewrite for ast::Root {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();
        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    if let Some(item_list) = ast::ItemList::cast(node.clone()) {
                        out.push_str(&item_list.rewrite_or_original(context, shape));
                    } else {
                        out.push_str(context.snippet(node.text_range()));
                    }
                }
                NodeOrToken::Token(token) => {
                    out.push_str(context.snippet(token.text_range()));
                }
            }
        }
        Some(out)
    }
}

impl Rewrite for ast::ItemList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();
        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    if let Some(item) = ast::Item::cast(node.clone()) {
                        out.push_str(&item.rewrite_or_original(context, shape));
                    } else {
                        out.push_str(context.snippet(node.text_range()));
                    }
                }
                NodeOrToken::Token(token) => {
                    out.push_str(context.snippet(token.text_range()));
                }
            }
        }
        Some(out)
    }
}

impl Rewrite for ast::Item {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        match self.kind()? {
            ItemKind::Mod(_) => Some(context.snippet_trimmed(self)),
            ItemKind::Func(func) => func.rewrite(context, _shape),
            ItemKind::Struct(struct_) => struct_.rewrite(context, _shape),
            ItemKind::Contract(contract) => contract.rewrite(context, _shape),
            ItemKind::Enum(enum_) => enum_.rewrite(context, _shape),
            ItemKind::TypeAlias(_) => Some(context.snippet_trimmed(self)),
            ItemKind::Impl(impl_) => impl_.rewrite(context, _shape),
            ItemKind::Trait(trait_) => trait_.rewrite(context, _shape),
            ItemKind::ImplTrait(impl_trait) => impl_trait.rewrite(context, _shape),
            ItemKind::Const(const_) => const_.rewrite(context, _shape),
            ItemKind::Use(use_) => use_.rewrite(context, _shape),
            ItemKind::Extern(_) => Some(context.snippet_trimmed(self)),
        }
    }
}

impl Rewrite for ast::Func {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let body = self.body()?;

        let func_range = self.syntax().text_range();
        let body_range = body.syntax().text_range();
        let suffix_range = TextRange::new(body_range.end(), func_range.end());
        let suffix = context.snippet(suffix_range);

        let mut out = String::new();

        write_attrs(self, context, &mut out);

        let outer_indent = shape.indent.indent_width();
        let indent_width = context.config.indent_width;
        let where_clause_opt = self.where_clause();
        let has_where = where_clause_opt.is_some();

        // For functions without a `where` clause, keep the existing compact
        // single-line signature style.
        if !has_where {
            write_item_modifier(self, &mut out);

            out.push_str("fn ");

            if let Some(name) = self.name() {
                out.push_str(context.snippet(name.text_range()));
            }

            write_generics(self, context, &mut out);

            let params = if let Some(param_list) = self.params() {
                param_list.rewrite(context, shape)?
            } else {
                "()".to_string()
            };
            out.push_str(&params);

            if let Some(ret_ty) = self.ret_ty() {
                let ty = ret_ty.rewrite_or_original(context, shape);
                out.push_str(" -> ");
                out.push_str(&ty);
            }

            if let Some(uses) = self.uses_clause() {
                let uses_text = uses.rewrite(context, shape)?;
                out.push(' ');
                out.push_str(&uses_text);
            }

            write_where_clause(self, context, &mut out);

            out.push(' ');

            let formatted_body = body.rewrite(context, shape)?;

            out.push_str(&formatted_body);
            out.push_str(suffix);
            return Some(out);
        }

        // Functions with a `where` clause use a width-aware layout. We first
        // try a fully single-line signature, and only spill pieces to new
        // lines when they exceed the configured line width.
        let max_width = shape.width;
        let param_indent = outer_indent + indent_width;

        // Build reusable pieces of the header.
        let mut prefix = String::new();
        write_item_modifier(self, &mut prefix);
        prefix.push_str("fn ");
        if let Some(name) = self.name() {
            prefix.push_str(context.snippet(name.text_range()));
        }
        write_generics(self, context, &mut prefix);

        let params_inline = if let Some(param_list) = self.params() {
            param_list.rewrite(context, shape)?
        } else {
            "()".to_string()
        };

        let ret_inline = if let Some(ret_ty) = self.ret_ty() {
            let ty = ret_ty.rewrite_or_original(context, shape);
            format!(" -> {ty}")
        } else {
            String::new()
        };

        let uses_inline = if let Some(uses) = self.uses_clause() {
            let uses_text = uses.rewrite(context, shape)?;
            format!(" {uses_text}")
        } else {
            String::new()
        };

        let where_clause = where_clause_opt.expect("has_where is true");
        let where_text = context
            .snippet(where_clause.syntax().text_range())
            .trim()
            .to_owned();
        let where_inline = format!(" {where_text}");

        // Fast path: everything fits on a single line.
        let single_line_len = outer_indent
            + prefix.len()
            + params_inline.len()
            + ret_inline.len()
            + uses_inline.len()
            + where_inline.len();

        if single_line_len <= max_width {
            push_indent(&mut out, outer_indent);
            out.push_str(&prefix);
            out.push_str(&params_inline);
            out.push_str(&ret_inline);
            out.push_str(&uses_inline);
            out.push_str(&where_inline);
            out.push(' ');

            let formatted_body = body.rewrite(context, shape)?;
            out.push_str(&formatted_body);
            out.push_str(suffix);
            return Some(out);
        }

        // Multi-line layout: decide whether the parameter list can remain
        // on the first line, then place `uses` / `where` on the same or
        // subsequent lines depending on the remaining width.
        let mut current_line_len = 0usize;

        push_indent(&mut out, outer_indent);
        current_line_len += outer_indent;

        out.push_str(&prefix);
        current_line_len += prefix.len();

        let params_inline_len = params_inline.len();
        let params_and_ret_len = params_inline_len + ret_inline.len();
        let params_inline_fit = current_line_len + params_and_ret_len <= max_width;

        if params_inline_fit {
            // Keep parameters on the same line.
            out.push_str(&params_inline);
            current_line_len += params_inline_len;
        } else if let Some(param_list) = self.params() {
            // Spill parameters onto multiple lines.
            let params: Vec<String> = param_list
                .into_iter()
                .map(|param| param.rewrite_or_original(context, shape))
                .collect();

            if params.is_empty() {
                out.push_str("()");
                current_line_len += 2;
            } else {
                out.push('(');
                out.push('\n');

                for param in params {
                    push_indent(&mut out, param_indent);
                    out.push_str(&param);
                    out.push(',');
                    out.push('\n');
                }

                push_indent(&mut out, outer_indent);
                current_line_len = outer_indent;
                out.push(')');
                current_line_len += 1;
            }
        } else {
            out.push_str("()");
            current_line_len += 2;
        }

        // Return type.
        if !ret_inline.is_empty() {
            let ret_len = ret_inline.len();
            if current_line_len + ret_len <= max_width {
                out.push_str(&ret_inline);
                current_line_len += ret_len;
            } else {
                out.push('\n');
                push_indent(&mut out, outer_indent);
                current_line_len = outer_indent;

                let trimmed = ret_inline.trim_start();
                out.push_str(trimmed);
                current_line_len += trimmed.len();
            }
        }

        // `uses` clause.
        if !uses_inline.is_empty() {
            let uses_len = uses_inline.len();
            let uses_no_space = uses_inline.trim_start();

            if current_line_len + uses_len <= max_width {
                out.push_str(&uses_inline);
            } else {
                out.push('\n');
                push_indent(&mut out, outer_indent + 2);
                out.push_str(uses_no_space);
            }
        }

        // `where` clause – in the multi-line path we never put `where` on the
        // same line as `uses`; they only share a line in the fully inline
        // fast-path above.
        let where_no_space = where_inline.trim_start();
        let where_no_space_len = where_no_space.len();
        let where_indent = outer_indent + 2;

        if where_indent + where_no_space_len <= max_width {
            // Put `where` on its own line.
            out.push('\n');
            push_indent(&mut out, where_indent);
            out.push_str(where_no_space);
        } else {
            // Fully vertical `where` layout.
            out.push('\n');
            push_indent(&mut out, where_indent);
            out.push_str("where");
            out.push('\n');

            for pred in where_clause {
                push_indent(&mut out, param_indent);
                let text = context.snippet(pred.syntax().text_range()).trim();
                out.push_str(text);
                out.push(',');
                out.push('\n');
            }
        }

        if !out.ends_with('\n') {
            out.push('\n');
        }

        let formatted_body = body.rewrite(context, shape)?;

        out.push_str(&formatted_body);
        out.push_str(suffix);
        Some(out)
    }
}

impl Rewrite for ast::FuncParamList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .with_surround("(", ")");
        format_list(self, &formatting, context, shape)
    }
}

impl Rewrite for ast::FuncParam {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        if self.mut_token().is_some() {
            out.push_str("mut ");
        }

        let label = self.label();
        let name = self.name();

        if let (Some(label), Some(name_ref)) = (&label, &name)
            && label.syntax().text_range() != name_ref.syntax().text_range()
        {
            out.push_str(context.snippet(label.syntax().text_range()).trim());
            out.push(' ');
        }

        if let Some(name) = name {
            out.push_str(context.snippet(name.syntax().text_range()).trim());
        }

        if let Some(ty) = self.ty() {
            if !out.is_empty() {
                out.push_str(": ");
            }
            out.push_str(&ty.rewrite_or_original(context, shape));
        }

        Some(out)
    }
}

impl Rewrite for ast::Struct {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        out.push_str("struct ");
        out.push_str(context.snippet(self.name()?.text_range()));

        write_generics(self, context, &mut out);
        write_where_clause(self, context, &mut out);

        if let Some(fields) = self.fields() {
            let text = context.snippet(fields.syntax().text_range());
            out.push(' ');
            out.push_str(text.trim_start());
        } else {
            out.push_str(" {}");
        }

        Some(out)
    }
}

impl Rewrite for ast::Contract {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        out.push_str("contract ");
        out.push_str(context.snippet(self.name()?.text_range()));

        if let Some(fields) = self.fields() {
            let text = context.snippet(fields.syntax().text_range());
            out.push(' ');
            out.push_str(text.trim_start());
        } else {
            out.push_str(" {}");
        }

        Some(out)
    }
}

impl Rewrite for ast::Enum {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        out.push_str("enum ");
        out.push_str(context.snippet(self.name()?.text_range()));

        write_generics(self, context, &mut out);
        write_where_clause(self, context, &mut out);

        if let Some(variants) = self.variants() {
            let text = context.snippet(variants.syntax().text_range());
            out.push(' ');
            out.push_str(text.trim_start());
        } else {
            out.push_str(" {}");
        }

        Some(out)
    }
}

impl Rewrite for ast::Trait {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        out.push_str("trait ");
        out.push_str(context.snippet(self.name()?.text_range()));

        write_generics(self, context, &mut out);

        if let Some(super_traits) = self.super_trait_list() {
            let text = context.snippet(super_traits.syntax().text_range());
            out.push(' ');
            out.push_str(text.trim_start());
        }

        write_where_clause(self, context, &mut out);

        if let Some(items) = self.item_list() {
            let text = context.snippet(items.syntax().text_range());
            out.push(' ');
            out.push_str(text.trim_start());
        } else {
            out.push_str(" {}");
        }

        Some(out)
    }
}

impl Rewrite for ast::Impl {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);

        out.push_str("impl");

        write_generics(self, context, &mut out);

        if let Some(ty) = self.ty() {
            out.push(' ');
            out.push_str(&ty.rewrite_or_original(context, shape));
        }

        write_where_clause(self, context, &mut out);

        if let Some(items) = self.item_list() {
            let text = context.snippet(items.syntax().text_range());
            out.push(' ');
            out.push_str(text.trim_start());
        } else {
            out.push_str(" {}");
        }

        Some(out)
    }
}

impl Rewrite for ast::ImplTrait {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);

        out.push_str("impl");
        write_generics(self, context, &mut out);

        if let Some(trait_ref) = self.trait_ref() {
            out.push(' ');
            out.push_str(&context.snippet_trimmed(&trait_ref));
        }

        if let Some(ty) = self.ty() {
            out.push_str(" for ");
            out.push_str(&ty.rewrite_or_original(context, shape));
        }

        write_where_clause(self, context, &mut out);

        if let Some(items) = self.item_list() {
            let text = context.snippet(items.syntax().text_range());
            out.push(' ');
            out.push_str(text.trim_start());
        } else {
            out.push_str(" {}");
        }

        Some(out)
    }
}

impl Rewrite for ast::Const {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        out.push_str("const ");
        out.push_str(context.snippet(self.name()?.text_range()));

        if let Some(ty) = self.ty() {
            out.push_str(": ");
            out.push_str(&ty.rewrite_or_original(context, shape));
        }

        if let Some(value) = self.value() {
            out.push_str(" = ");
            out.push_str(&value.rewrite_or_original(context, shape));
        }

        Some(out)
    }
}

impl Rewrite for ast::Use {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        out.push_str("use ");
        out.push_str(&context.snippet_trimmed(&self.use_tree()?));

        Some(out)
    }
}

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
                        push_indent(&mut out, inner_indent);
                        let text = context.snippet(tok.text_range());
                        out.push_str(text.trim_start());
                        out.push('\n');
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
        let mut out = String::new();
        out.push_str("let ");

        out.push_str(&self.pat()?.rewrite_or_original(context, shape));

        if let Some(ty) = self.type_annotation() {
            out.push_str(": ");
            out.push_str(&ty.rewrite_or_original(context, shape));
        }

        if let Some(init) = self.initializer() {
            out.push_str(" = ");
            let init_text = init.rewrite_or_original(context, shape);
            out.push_str(&init_text);
        }

        Some(out)
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
        Some("continue".to_string())
    }
}

impl Rewrite for ast::BreakStmt {
    fn rewrite(&self, _context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some("break".to_string())
    }
}

impl Rewrite for ast::ReturnStmt {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();
        out.push_str("return");

        if let Some(expr) = self.expr() {
            out.push(' ');
            let expr_text = expr.rewrite_or_original(context, shape);
            out.push_str(&expr_text);
        }

        Some(out)
    }
}

impl Rewrite for ast::ExprStmt {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        Some(self.expr()?.rewrite_or_original(context, shape))
    }
}

impl Rewrite for ast::Expr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        match self.kind() {
            ExprKind::Lit(lit) => lit.rewrite(context, shape),
            ExprKind::Block(block) => block.rewrite(context, shape),
            ExprKind::Bin(bin) => bin.rewrite(context, shape),
            ExprKind::Un(un) => un.rewrite(context, shape),
            ExprKind::Call(call) => call.rewrite(context, shape),
            ExprKind::MethodCall(method) => method.rewrite(context, shape),
            ExprKind::Path(path) => path.rewrite(context, shape),
            ExprKind::RecordInit(record) => record.rewrite(context, shape),
            ExprKind::Field(field) => field.rewrite(context, shape),
            ExprKind::Index(index) => index.rewrite(context, shape),
            ExprKind::Tuple(tuple) => tuple.rewrite(context, shape),
            ExprKind::Array(array) => array.rewrite(context, shape),
            ExprKind::ArrayRep(array_rep) => array_rep.rewrite(context, shape),
            ExprKind::If(if_expr) => if_expr.rewrite(context, shape),
            ExprKind::Match(match_expr) => match_expr.rewrite(context, shape),
            ExprKind::With(with_expr) => with_expr.rewrite(context, shape),
            ExprKind::Paren(paren) => paren.rewrite(context, shape),
            ExprKind::Assign(assign) => assign.rewrite(context, shape),
            ExprKind::AugAssign(aug_assign) => aug_assign.rewrite(context, shape),
        }
    }
}

impl Rewrite for ast::BinExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let lhs = self.lhs()?.rewrite_or_original(context, shape);
        let op = context.snippet_node_or_token(&self.op()?.syntax());
        let rhs = self.rhs()?.rewrite_or_original(context, shape);

        Some(format!("{lhs} {op} {rhs}"))
    }
}

impl Rewrite for ast::UnExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let op_token = self.op()?.syntax();
        let op = context.snippet(op_token.text_range()).trim();
        let expr = self.expr()?.rewrite_or_original(context, shape);

        Some(format!("{op}{expr}"))
    }
}

impl Rewrite for ast::CallArg {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let expr = self.expr()?.rewrite_or_original(context, shape);
        if let Some(label) = self.label() {
            let label_text = context.snippet(label.text_range()).trim();
            Some(format!("{label_text}: {expr}"))
        } else {
            Some(expr)
        }
    }
}

impl Rewrite for ast::CallExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let callee = self.callee()?.rewrite_or_original(context, shape);
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .with_surround("(", ")");

        let args_str = if let Some(args) = self.args() {
            format_list(args.into_iter(), &formatting, context, shape)?
        } else {
            "()".to_string()
        };

        Some(format!("{callee}{args_str}"))
    }
}

impl Rewrite for ast::MethodCallExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let receiver = self.receiver()?.rewrite_or_original(context, shape);
        let name = context.snippet(self.method_name()?.text_range()).trim();

        let generics = if let Some(args) = self.generic_args() {
            context.snippet_trimmed(&args)
        } else {
            String::new()
        };

        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .with_surround("(", ")");

        let args_str = if let Some(args) = self.args() {
            format_list(args.into_iter(), &formatting, context, shape)?
        } else {
            "()".to_string()
        };

        Some(format!("{receiver}.{name}{generics}{args_str}"))
    }
}

impl Rewrite for ast::RecordInitExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let path = context.snippet_trimmed(&self.path()?);

        let fields_str = if let Some(fields) = self.fields() {
            let fields: Vec<String> = fields
                .into_iter()
                .filter_map(|field| {
                    let label = field.label()?;
                    let label_text = context.snippet(label.text_range()).trim();
                    let expr = field.expr()?.rewrite_or_original(context, shape);
                    Some(format!("{label_text}: {expr}"))
                })
                .collect();

            format!(" {{ {} }}", fields.join(", "))
        } else {
            "{}".to_string()
        };

        Some(format!("{path}{fields_str}"))
    }
}

impl Rewrite for ast::AssignExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let lhs = self.lhs_expr()?.rewrite_or_original(context, shape);
        let rhs = self.rhs_expr()?.rewrite_or_original(context, shape);

        Some(format!("{lhs} = {rhs}"))
    }
}

impl Rewrite for ast::AugAssignExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let lhs = self.lhs_expr()?.rewrite_or_original(context, shape);
        let op = context.snippet_node_or_token(&self.op()?.syntax());
        let rhs = self.rhs_expr()?.rewrite_or_original(context, shape);

        Some(format!("{lhs} {op}= {rhs}"))
    }
}

impl Rewrite for ast::PathExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some(context.snippet_trimmed(self))
    }
}

impl Rewrite for ast::FieldExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let base = self.receiver()?.rewrite_or_original(context, shape);
        let field = context.snippet(self.name_or_index()?.text_range()).trim();

        Some(format!("{base}.{field}"))
    }
}

impl Rewrite for ast::IndexExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let expr = self.expr()?.rewrite_or_original(context, shape);
        let index = self.index()?.rewrite_or_original(context, shape);

        Some(format!("{expr}[{index}]"))
    }
}

impl Rewrite for ast::LitExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some(context.snippet_trimmed(&self.lit()?))
    }
}

impl Rewrite for ast::IfExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let cond = self.cond()?.rewrite_or_original(context, shape);
        let then = self.then()?.rewrite_or_original(context, shape);

        let else_ = if let Some(else_expr) = self.else_() {
            format!(" else {}", else_expr.rewrite_or_original(context, shape))
        } else {
            String::new()
        };

        Some(format!("if {cond} {then}{else_}"))
    }
}

impl Rewrite for ast::UsesClause {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        if let Some(params) = self.param_list() {
            let params: Vec<String> = params
                .into_iter()
                .map(|param| param.rewrite_or_original(context, shape))
                .collect();

            Some(format!("uses ({})", params.join(", ")))
        } else if let Some(param) = self.param() {
            let param_text = param.rewrite_or_original(context, shape);
            Some(format!("uses {param_text}"))
        } else {
            None
        }
    }
}

impl Rewrite for ast::UsesParam {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        let mut out = String::new();

        if self.mut_token().is_some() {
            out.push_str("mut ");
        }

        if let Some(name) = self.name() {
            out.push_str(context.snippet(name.syntax().text_range()).trim());
            out.push_str(": ");
        }

        let path = self.path()?;
        out.push_str(&context.snippet_trimmed(&path));

        Some(out)
    }
}

impl Rewrite for ast::MatchExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let scrutinee = self.scrutinee()?.rewrite_or_original(context, shape);

        let outer_indent = shape.indent.indent_width();
        let indent_width = context.config.indent_width;
        let arms_indent = outer_indent + indent_width;
        let arm_shape = Shape::with_width(shape.width, Indent::from_block(arms_indent));

        let mut out = String::new();
        out.push_str("match ");
        out.push_str(&scrutinee);
        out.push_str(" {");
        out.push('\n');

        if let Some(arms) = self.arms() {
            for arm in arms {
                let pat = arm.pat()?.rewrite_or_original(context, arm_shape);
                let body = arm.body()?.rewrite_or_original(context, arm_shape);

                push_indent(&mut out, arms_indent);
                out.push_str(&pat);
                out.push_str(" => ");
                out.push_str(&body);
                out.push(',');
                out.push('\n');
            }
        }

        push_indent(&mut out, outer_indent);
        out.push('}');

        Some(out)
    }
}

impl Rewrite for ast::WithExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let params = if let Some(params) = self.params() {
            let params: Vec<String> = params
                .into_iter()
                .filter_map(|param| {
                    let path = context.snippet_trimmed(&param.path()?);
                    let value = param.value_expr()?.rewrite_or_original(context, shape);
                    Some(format!("{path} = {value}"))
                })
                .collect();

            params.join(", ")
        } else {
            String::new()
        };

        let body = self.body()?.rewrite_or_original(context, shape);

        Some(format!("with ({params}) {body}"))
    }
}

impl Rewrite for ast::TupleExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .with_surround("(", ")");
        format_list(self.elems().flatten(), &formatting, context, shape)
    }
}

impl Rewrite for ast::ArrayExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .with_surround("[", "]");
        format_list(self.elems().flatten(), &formatting, context, shape)
    }
}

impl Rewrite for ast::ArrayRepExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let val = self.val()?.rewrite_or_original(context, shape);
        let len = self.len()?.rewrite_or_original(context, shape);

        Some(format!("[{val}; {len}]"))
    }
}

impl Rewrite for ast::ParenExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let expr = self.expr()?.rewrite_or_original(context, shape);

        Some(format!("({expr})"))
    }
}

fn push_indent(out: &mut String, width: usize) {
    for _ in 0..width {
        out.push(' ');
    }
}

impl Rewrite for ast::Type {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        match self.kind() {
            TypeKind::Ptr(ptr) => ptr.rewrite(context, shape),
            TypeKind::Path(path) => path.rewrite(context, shape),
            TypeKind::Tuple(tuple) => tuple.rewrite(context, shape),
            TypeKind::Array(array) => array.rewrite(context, shape),
            TypeKind::Never(never) => never.rewrite(context, shape),
        }
    }
}

impl Rewrite for ast::PtrType {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let inner = self.inner()?.rewrite_or_original(context, shape);
        Some(format!("*{inner}"))
    }
}

impl Rewrite for ast::PathType {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some(context.snippet_trimmed(&self.path()?))
    }
}

impl Rewrite for ast::TupleType {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .with_surround("(", ")");
        format_list(self.elem_tys(), &formatting, context, shape)
    }
}

impl Rewrite for ast::ArrayType {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let elem_ty = self.elem_ty()?.rewrite_or_original(context, shape);
        let len = self.len()?.rewrite_or_original(context, shape);

        Some(format!("[{elem_ty}; {len}]"))
    }
}

impl Rewrite for ast::NeverType {
    fn rewrite(&self, _context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some("!".to_string())
    }
}

impl Rewrite for ast::Pat {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        match self.kind() {
            PatKind::WildCard(wildcard) => wildcard.rewrite(context, shape),
            PatKind::Rest(rest) => rest.rewrite(context, shape),
            PatKind::Lit(lit) => lit.rewrite(context, shape),
            PatKind::Tuple(tuple) => tuple.rewrite(context, shape),
            PatKind::Path(path) => path.rewrite(context, shape),
            PatKind::PathTuple(path_tuple) => path_tuple.rewrite(context, shape),
            PatKind::Record(record) => record.rewrite(context, shape),
            PatKind::Or(or) => or.rewrite(context, shape),
        }
    }
}

impl Rewrite for ast::WildCardPat {
    fn rewrite(&self, _context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some("_".to_string())
    }
}

impl Rewrite for ast::RestPat {
    fn rewrite(&self, _context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some("..".to_string())
    }
}

impl Rewrite for ast::LitPat {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some(context.snippet_trimmed(&self.lit()?))
    }
}

impl Rewrite for ast::TuplePat {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .with_surround("(", ")");
        format_list(
            self.elems().into_iter().flatten(),
            &formatting,
            context,
            shape,
        )
    }
}

impl Rewrite for ast::PathPat {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        let mut out = String::new();

        if self.mut_token().is_some() {
            out.push_str("mut ");
        }

        out.push_str(&context.snippet_trimmed(&self.path()?));

        Some(out)
    }
}

impl Rewrite for ast::PathTuplePat {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let path = context.snippet_trimmed(&self.path()?);
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .with_surround("(", ")");
        let elems_str = format_list(
            self.elems().into_iter().flatten(),
            &formatting,
            context,
            shape,
        )?;

        Some(format!("{path}{elems_str}"))
    }
}

impl Rewrite for ast::RecordPat {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let path = context.snippet_trimmed(&self.path()?);

        let fields_str = if let Some(fields) = self.fields() {
            let formatted_fields: Vec<String> = fields
                .into_iter()
                .filter_map(|field| {
                    let name = field.name()?;
                    let name_text = context.snippet(name.text_range()).trim();

                    if let Some(pat) = field.pat() {
                        let pat_text = pat.rewrite_or_original(context, shape);
                        Some(format!("{name_text}: {pat_text}"))
                    } else {
                        Some(name_text.to_string())
                    }
                })
                .collect();

            format!(" {{ {} }}", formatted_fields.join(", "))
        } else {
            "{}".to_string()
        };

        Some(format!("{path}{fields_str}"))
    }
}

impl Rewrite for ast::OrPat {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let lhs = self.lhs()?.rewrite_or_original(context, shape);
        let rhs = self.rhs()?.rewrite_or_original(context, shape);

        Some(format!("{lhs} | {rhs}"))
    }
}
