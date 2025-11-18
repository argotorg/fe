use crate::{Indent, Rewrite, RewriteContext, Shape};
use parser::{
    TextRange,
    ast::{self, ExprKind, GenericArgsOwner, ItemKind, PatKind, StmtKind, TypeKind, prelude::AstNode},
    syntax_kind::SyntaxKind,
    syntax_node::NodeOrToken,
};

trait RewriteExt: Rewrite + AstNode {
    fn rewrite_or_original(&self, context: &RewriteContext<'_>, shape: Shape) -> String {
        self.rewrite(context, shape).unwrap_or_else(|| {
            context
                .snippet(self.syntax().text_range())
                .trim()
                .to_owned()
        })
    }
}

impl<T: Rewrite + AstNode> RewriteExt for T {}

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
            ItemKind::Contract(_) => Some(context.snippet_trimmed(self)),
            ItemKind::Enum(_) => Some(context.snippet_trimmed(self)),
            ItemKind::TypeAlias(_) => Some(context.snippet_trimmed(self)),
            ItemKind::Impl(_) => Some(context.snippet_trimmed(self)),
            ItemKind::Trait(_) => Some(context.snippet_trimmed(self)),
            ItemKind::ImplTrait(_) => Some(context.snippet_trimmed(self)),
            ItemKind::Const(_) => Some(context.snippet_trimmed(self)),
            ItemKind::Use(_) => Some(context.snippet_trimmed(self)),
            ItemKind::Extern(_) => Some(context.snippet_trimmed(self)),
        }
    }
}

impl Rewrite for ast::Func {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let func_range = self.syntax().text_range();
        let body = self.body()?;
        let body_range = body.syntax().text_range();

        // Split the function into:
        //   [prefix: signature and attributes][body block][suffix: trailing trivia]
        let prefix_range = TextRange::new(func_range.start(), body_range.start());
        let suffix_range = TextRange::new(body_range.end(), func_range.end());

        let prefix = context.snippet(prefix_range);
        let suffix = context.snippet(suffix_range);

        let formatted_body = body.rewrite(context, shape)?;

        let mut out = String::new();
        out.push_str(prefix);
        out.push_str(&formatted_body);
        out.push_str(suffix);
        Some(out)
    }
}

impl Rewrite for ast::Struct {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        // For now, just preserve the original formatting for structs
        Some(context.snippet_trimmed(self))
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
                    } else if let Some(item) = ast::Item::cast(node.clone()) {
                        children.next();
                        push_indent(&mut out, inner_indent);
                        let code = item.rewrite_or_original(context, inner_shape);
                        out.push_str(&code);
                        out.push('\n');
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
                        // Preserve at most a single blank line between statements.
                        let text = context.snippet(tok.text_range());
                        let newline_count = text.chars().filter(|c| *c == '\n').count();
                        if newline_count > 1 {
                            out.push('\n');
                        }
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

impl Rewrite for ast::CallExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let callee = self.callee()?.rewrite_or_original(context, shape);

        let args_str = if let Some(args) = self.args() {
            let args: Vec<String> = args
                .into_iter()
                .filter_map(|arg| {
                    let expr = arg.expr()?.rewrite_or_original(context, shape);
                    if let Some(label) = arg.label() {
                        Some(format!("{}: {expr}", context.snippet(label.text_range()).trim()))
                    } else {
                        Some(expr)
                    }
                })
                .collect();

            format!("({})", args.join(", "))
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

        let args_str = if let Some(args) = self.args() {
            let args: Vec<String> = args
                .into_iter()
                .filter_map(|arg| {
                    let expr = arg.expr()?.rewrite_or_original(context, shape);
                    if let Some(label) = arg.label() {
                        Some(format!("{}: {expr}", context.snippet(label.text_range()).trim()))
                    } else {
                        Some(expr)
                    }
                })
                .collect();

            format!("({})", args.join(", "))
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
        let elems: Vec<String> = self
            .elems()
            .flatten()
            .map(|expr| expr.rewrite_or_original(context, shape))
            .collect();

        Some(format!("({})", elems.join(", ")))
    }
}

impl Rewrite for ast::ArrayExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let elems: Vec<String> = self
            .elems()
            .flatten()
            .map(|expr| expr.rewrite_or_original(context, shape))
            .collect();

        Some(format!("[{}]", elems.join(", ")))
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
        let types: Vec<String> = self
            .elem_tys()
            .map(|ty| ty.rewrite_or_original(context, shape))
            .collect();

        Some(format!("({})", types.join(", ")))
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
        let elems = if let Some(elem_list) = self.elems() {
            let formatted_elems: Vec<String> = elem_list
                .into_iter()
                .map(|pat| pat.rewrite_or_original(context, shape))
                .collect();
            formatted_elems.join(", ")
        } else {
            String::new()
        };

        Some(format!("({elems})"))
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

        let elems = if let Some(elem_list) = self.elems() {
            let formatted_elems: Vec<String> = elem_list
                .into_iter()
                .map(|pat| pat.rewrite_or_original(context, shape))
                .collect();
            format!("({})", formatted_elems.join(", "))
        } else {
            "()".to_string()
        };

        Some(format!("{path}{elems}"))
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
