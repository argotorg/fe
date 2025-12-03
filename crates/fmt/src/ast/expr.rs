//! Formatting for expressions.

use std::fmt::Write as _;

use crate::{
    Indent, ListFormatting, ListTactic, Rewrite, RewriteContext, RewriteExt, Shape, format_list,
};
use parser::ast::{self, ExprKind, GenericArgsOwner};

use super::push_indent;

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
            args.rewrite_or_original(context, shape)
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

impl Rewrite for ast::RecordField {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let label = self.label()?;
        let label_text = context.snippet(label.text_range()).trim();
        let expr = self.expr()?.rewrite_or_original(context, shape);
        Some(format!("{label_text}: {expr}"))
    }
}

impl Rewrite for ast::RecordInitExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let path = self.path()?.rewrite_or_original(context, shape);

        let fields_str = if let Some(fields) = self.fields() {
            let formatting = ListFormatting::new(shape)
                .tactic(ListTactic::Mixed)
                .with_surround("{", "}")
                .horizontal_padding(true);
            let list = format_list(fields, &formatting, context, shape)?;
            format!(" {list}")
        } else {
            " {}".to_string()
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
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        self.path()?.rewrite(context, shape)
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
            let formatting = ListFormatting::new(shape)
                .tactic(ListTactic::Mixed)
                .with_surround("(", ")");
            let list = format_list(params, &formatting, context, shape)?;
            Some(format!("uses {list}"))
        } else if let Some(param) = self.param() {
            let param_text = param.rewrite_or_original(context, shape);
            Some(format!("uses {param_text}"))
        } else {
            None
        }
    }
}

impl Rewrite for ast::UsesParam {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        if self.mut_token().is_some() {
            out.push_str("mut ");
        }

        if let Some(name) = self.name() {
            let name_text = context.snippet(name.syntax().text_range()).trim();
            let _ = write!(out, "{name_text}: ");
        }

        out.push_str(&self.path()?.rewrite_or_original(context, shape));

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
        let _ = writeln!(out, "match {scrutinee} {{");

        if let Some(arms) = self.arms() {
            for arm in arms {
                let pat = arm.pat()?.rewrite_or_original(context, arm_shape);
                let body = arm.body()?.rewrite_or_original(context, arm_shape);

                push_indent(&mut out, arms_indent);
                let _ = writeln!(out, "{pat} => {body},");
            }
        }

        push_indent(&mut out, outer_indent);
        out.push('}');

        Some(out)
    }
}

impl Rewrite for ast::WithParam {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let path = self.path()?.rewrite_or_original(context, shape);
        let value = self.value_expr()?.rewrite_or_original(context, shape);
        Some(format!("{path} = {value}"))
    }
}

impl Rewrite for ast::WithExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let params_str = if let Some(params) = self.params() {
            let formatting = ListFormatting::new(shape)
                .tactic(ListTactic::Mixed)
                .with_surround("(", ")");
            format_list(params, &formatting, context, shape)?
        } else {
            "()".to_string()
        };

        let body = self.body()?.rewrite_or_original(context, shape);

        Some(format!("with {params_str} {body}"))
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
