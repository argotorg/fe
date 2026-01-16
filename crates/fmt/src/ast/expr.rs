//! Formatting for expressions.

use pretty::DocAllocator;

use crate::RewriteContext;
use parser::ast::{self, BinOp, ExprKind, GenericArgsOwner, LogicalBinOp, prelude::AstNode};

use super::types::{Doc, ToDoc, block_list};

// ============================================================================
// Binary expression formatting with precedence-aware indentation
// ============================================================================

/// Returns the precedence level of a binary operator.
/// Lower number = lower precedence (binds less tightly).
/// This is used for formatting, not parsing.
fn bin_op_precedence(op: &BinOp) -> u8 {
    use parser::ast::ArithBinOp;
    match op {
        BinOp::Logical(LogicalBinOp::Or(_)) => 1,
        BinOp::Logical(LogicalBinOp::And(_)) => 2,
        BinOp::Comp(_) => 3,
        // Bitwise operators
        BinOp::Arith(ArithBinOp::BitOr(_)) => 4,
        BinOp::Arith(ArithBinOp::BitXor(_)) => 5,
        BinOp::Arith(ArithBinOp::BitAnd(_)) => 6,
        // Shift operators
        BinOp::Arith(ArithBinOp::LShift(_) | ArithBinOp::RShift(_)) => 7,
        // Additive operators
        BinOp::Arith(ArithBinOp::Add(_) | ArithBinOp::Sub(_)) => 8,
        // Multiplicative operators
        BinOp::Arith(ArithBinOp::Mul(_) | ArithBinOp::Div(_) | ArithBinOp::Mod(_)) => 9,
        // Exponentiation (highest arithmetic precedence)
        BinOp::Arith(ArithBinOp::Pow(_)) => 10,
    }
}

/// If expression is a binary expression with the given precedence, return the BinExpr.
fn as_bin_expr_with_precedence(expr: &ast::Expr, precedence: u8) -> Option<ast::BinExpr> {
    if let ExprKind::Bin(bin) = expr.kind()
        && let Some(op) = bin.op()
        && bin_op_precedence(&op) == precedence
    {
        return Some(bin);
    }
    None
}

/// Formats a binary expression with precedence-aware indentation.
///
/// For expressions like `a || b && c || d`:
/// - `||` (lower precedence) breaks at base indentation
/// - `&&` (higher precedence) breaks with extra indentation
///
/// ```text
/// a
/// || b && c
/// || d
/// ```
///
/// And for `a || b && c && d || e`:
/// ```text
/// a
/// || b
///     && c
///     && d
/// || e
/// ```
fn format_bin_expr<'a>(bin: &ast::BinExpr, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
    format_bin_expr_inner(bin, ctx, true)
}

/// Inner implementation with control over outer nesting.
/// `apply_outer_nest`: if true, wraps result in `.nest(indent).group()`
fn format_bin_expr_inner<'a>(
    bin: &ast::BinExpr,
    ctx: &'a RewriteContext<'a>,
    apply_outer_nest: bool,
) -> Doc<'a> {
    let alloc = &ctx.alloc;
    let indent = ctx.config.indent_width as isize;

    let op = match bin.op() {
        Some(o) => o,
        None => {
            return bin
                .lhs()
                .map(|e| e.to_doc(ctx))
                .unwrap_or_else(|| alloc.nil());
        }
    };

    let precedence = bin_op_precedence(&op);

    // Collect all operands and operators at this precedence level
    let mut operands: Vec<ast::Expr> = Vec::new();
    let mut operators: Vec<String> = Vec::new();

    fn collect<'a>(
        expr: &ast::BinExpr,
        precedence: u8,
        operands: &mut Vec<ast::Expr>,
        operators: &mut Vec<String>,
        ctx: &'a RewriteContext<'a>,
    ) {
        if let Some(lhs) = expr.lhs() {
            if let Some(lhs_bin) = as_bin_expr_with_precedence(&lhs, precedence) {
                collect(&lhs_bin, precedence, operands, operators, ctx);
            } else {
                operands.push(lhs);
            }
        }

        if let Some(op) = expr.op() {
            operators.push(ctx.snippet_node_or_token(&op.syntax()));
        }

        if let Some(rhs) = expr.rhs() {
            if let Some(rhs_bin) = as_bin_expr_with_precedence(&rhs, precedence) {
                collect(&rhs_bin, precedence, operands, operators, ctx);
            } else {
                operands.push(rhs);
            }
        }
    }

    collect(bin, precedence, &mut operands, &mut operators, ctx);

    if operands.is_empty() {
        return alloc.nil();
    }

    let first = &operands[0];
    let mut result = first.to_doc(ctx);

    for (i, operand) in operands.iter().skip(1).enumerate() {
        let op_str = &operators[i];

        // Check if operand is a higher-precedence binary expression
        let higher_prec_bin = if let ExprKind::Bin(inner_bin) = operand.kind() {
            inner_bin
                .op()
                .filter(|inner_op| bin_op_precedence(inner_op) > precedence)
                .map(|_| inner_bin)
        } else {
            None
        };

        if let Some(inner_bin) = higher_prec_bin {
            // Format inner chain without its own nesting, we control it here
            let inner_doc = format_bin_expr_inner(&inner_bin, ctx, false);
            result = result
                .append(alloc.line())
                .append(alloc.text(op_str.clone()))
                .append(alloc.text(" "))
                .append(inner_doc.nest(indent));
        } else {
            let operand_doc = operand.to_doc(ctx);
            result = result
                .append(alloc.line())
                .append(alloc.text(op_str.clone()))
                .append(alloc.text(" "))
                .append(operand_doc);
        }
    }

    if apply_outer_nest {
        result.nest(indent).group()
    } else {
        result.group()
    }
}

/// A segment in a method/field chain.
enum ChainSegment {
    /// `.method(args)` or `.method::<T>(args)`
    MethodCall {
        name: String,
        generics: Option<ast::GenericArgList>,
        args: Option<ast::CallArgList>,
    },
    /// `.field`
    Field { name: String },
}

/// Collects chain segments from an expression, returning (root, segments).
/// Segments are in source order (first segment is closest to root).
fn collect_chain(expr: &ast::Expr) -> (ast::Expr, Vec<ChainSegment>) {
    let mut segments = Vec::new();
    let mut current = expr.clone();

    loop {
        match current.kind() {
            ExprKind::MethodCall(method) => {
                let name = method
                    .method_name()
                    .map(|n| n.text().to_string())
                    .unwrap_or_default();
                segments.push(ChainSegment::MethodCall {
                    name,
                    generics: method.generic_args(),
                    args: method.args(),
                });
                match method.receiver() {
                    Some(r) => current = r,
                    None => break,
                }
            }
            ExprKind::Field(field) => {
                let name = field
                    .name_or_index()
                    .map(|n| n.text().to_string())
                    .unwrap_or_default();
                segments.push(ChainSegment::Field { name });
                match field.receiver() {
                    Some(r) => current = r,
                    None => break,
                }
            }
            _ => break,
        }
    }

    // Reverse so segments are in source order (root first)
    segments.reverse();
    (current, segments)
}

/// Builds a document for a single chain segment.
fn segment_to_doc<'a>(seg: &ChainSegment, ctx: &'a RewriteContext<'a>, indent: isize) -> Doc<'a> {
    let alloc = &ctx.alloc;

    match seg {
        ChainSegment::MethodCall {
            name,
            generics,
            args,
        } => {
            let generics_doc = generics
                .as_ref()
                .map(|g| g.to_doc(ctx))
                .unwrap_or_else(|| alloc.nil());
            let args_vec: Vec<_> = args
                .as_ref()
                .map(|a| a.clone().into_iter().map(|arg| arg.to_doc(ctx)).collect())
                .unwrap_or_default();
            alloc
                .text(".")
                .append(alloc.text(name.clone()))
                .append(generics_doc)
                .append(call_args(ctx, args_vec, indent))
        }
        ChainSegment::Field { name } => alloc.text(".").append(alloc.text(name.clone())),
    }
}

/// Returns true if the expression is a method/field chain (has at least one segment).
pub fn is_chain(expr: &ast::Expr) -> bool {
    matches!(expr.kind(), ExprKind::MethodCall(_) | ExprKind::Field(_))
}

/// Formats a method/field chain with a known prefix.
///
/// When the chain needs to break, all segments move to new lines with the dots aligned.
/// The prefix width determines whether the first segment can stay inline.
pub fn format_chain_with_prefix<'a>(
    prefix: Doc<'a>,
    expr: &ast::Expr,
    ctx: &'a RewriteContext<'a>,
) -> Doc<'a> {
    let (root, segments) = collect_chain(expr);

    if segments.is_empty() {
        return prefix.append(root.to_doc(ctx));
    }

    let indent = ctx.config.indent_width as isize;
    format_chain_inner(Some(prefix), &root, &segments, ctx, indent)
}

/// Formats a method/field chain with proper breaking and aligned dots.
fn format_chain<'a>(expr: &ast::Expr, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
    let (root, segments) = collect_chain(expr);

    if segments.is_empty() {
        return root.to_doc(ctx);
    }

    let indent = ctx.config.indent_width as isize;
    format_chain_inner(None, &root, &segments, ctx, indent)
}

/// Estimates the width of a root expression for determining inline behavior.
fn root_width(root: &ast::Expr, ctx: &RewriteContext<'_>) -> usize {
    // For simple identifiers like `self`, `foo`, use the text length
    if let ExprKind::Path(path_expr) = root.kind()
        && let Some(path) = path_expr.path()
    {
        let text = ctx.snippet(path.syntax().text_range());
        return text.trim().len();
    }
    // For other expressions, assume they're "long"
    usize::MAX
}

/// Formats a chain with all dots aligned when broken.
///
/// Behavior depends on whether there's a prefix and the root width:
/// - No prefix + short root (≤4 chars): `root.first_seg` stays inline
/// - No prefix + long root: all segments on new lines
/// - With prefix: all segments on new lines (prefix makes first dot too far right)
///
/// Examples:
/// ```text
/// // Short root, no prefix - first segment inline
/// self.alpha_field
///     .beta_field
///     .gamma_field
///
/// // Long root or with prefix - all segments break
/// some_receiver
///     .alpha_field
///     .beta_field
///
/// let x = foo
///     .alpha_field
///     .beta_field
/// ```
fn format_chain_inner<'a>(
    prefix: Option<Doc<'a>>,
    root: &ast::Expr,
    segments: &[ChainSegment],
    ctx: &'a RewriteContext<'a>,
    indent: isize,
) -> Doc<'a> {
    let alloc = &ctx.alloc;

    // Build the root expression
    let root_doc = root.to_doc(ctx);

    // Determine if the first segment can stay inline with root
    // This happens when: no prefix AND root is short (≤4 chars like `self`, `foo`)
    let root_w = root_width(root, ctx);
    let first_segment_inline = prefix.is_none() && root_w <= 4 && !segments.is_empty();

    if first_segment_inline {
        // Short root: keep root.first_segment on same line, break before remaining segments
        let first_seg_doc = segment_to_doc(&segments[0], ctx, indent);
        let mut chain_doc = root_doc.append(first_seg_doc);

        // Remaining segments each get a line break before them
        for seg in &segments[1..] {
            let seg_doc = segment_to_doc(seg, ctx, indent);
            chain_doc = chain_doc.append(alloc.line_().append(seg_doc).nest(indent));
        }

        chain_doc.group()
    } else {
        // Long root or has prefix: all segments on new lines when broken
        let mut chain_doc = root_doc;
        for seg in segments {
            let seg_doc = segment_to_doc(seg, ctx, indent);
            chain_doc = chain_doc.append(alloc.line_().append(seg_doc).nest(indent));
        }

        let chain_doc = chain_doc.group();

        match prefix {
            Some(p) => p.append(chain_doc),
            None => chain_doc,
        }
    }
}

impl ToDoc for ast::BinExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        format_bin_expr(self, ctx)
    }
}

impl ToDoc for ast::UnExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let op = match self.op() {
            Some(o) => ctx.snippet(o.syntax().text_range()).trim().to_string(),
            None => return alloc.nil(),
        };
        let expr = match self.expr() {
            Some(e) => e.to_doc(ctx),
            None => return alloc.text(op),
        };

        alloc.text(op).append(expr)
    }
}

impl ToDoc for ast::CastExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let expr = match self.expr() {
            Some(e) => e.to_doc(ctx),
            None => return alloc.nil(),
        };
        let ty = match self.ty() {
            Some(t) => t.to_doc(ctx),
            None => return expr,
        };

        expr.append(alloc.text(" as ")).append(ty)
    }
}

impl ToDoc for ast::CallArg {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let expr = match self.expr() {
            Some(e) => e.to_doc(ctx),
            None => return alloc.nil(),
        };

        if let Some(label) = self.label() {
            let label_text = ctx.snippet(label.text_range()).trim().to_string();
            alloc.text(label_text).append(alloc.text(": ")).append(expr)
        } else {
            expr
        }
    }
}

impl ToDoc for ast::CallExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let callee = match self.callee() {
            Some(c) => c.to_doc(ctx),
            None => return alloc.nil(),
        };

        let args: Vec<_> = self
            .args()
            .map(|args| args.into_iter().map(|a| a.to_doc(ctx)).collect())
            .unwrap_or_default();

        let indent = ctx.config.indent_width as isize;
        callee.append(call_args(ctx, args, indent))
    }
}

/// Formats function call arguments with `fn_call_width` support.
/// Uses `max_width_group` to break if args exceed `fn_call_width` when rendered flat.
fn call_args<'a>(ctx: &'a RewriteContext<'a>, args: Vec<Doc<'a>>, indent: isize) -> Doc<'a> {
    use super::types::intersperse;
    let alloc = &ctx.alloc;

    if args.is_empty() {
        return alloc.text("()");
    }

    let sep = alloc.text(",").append(alloc.line());
    let inner = intersperse(alloc, args, sep);
    let trailing = alloc.text(",").flat_alt(alloc.nil());

    alloc
        .text("(")
        .append(alloc.line_().append(inner).append(trailing).nest(indent))
        .append(alloc.line_())
        .append(alloc.text(")"))
        .max_width_group(ctx.config.fn_call_width)
}

impl ToDoc for ast::MethodCallExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        // Delegate to chain formatting which handles the entire chain at once
        format_chain(&ast::Expr::cast(self.syntax().clone()).unwrap(), ctx)
    }
}

impl ToDoc for ast::RecordField {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        match (self.label(), self.expr()) {
            // Named field with explicit value: `label: expr`
            (Some(label), Some(expr)) => {
                let label_str = ctx.snippet(label.text_range()).trim().to_string();
                alloc
                    .text(label_str)
                    .append(alloc.text(": "))
                    .append(expr.to_doc(ctx))
            }
            // Shorthand field: `from` (no colon, expr is the identifier)
            (None, Some(expr)) => expr.to_doc(ctx),
            // Just a label (shouldn't happen in practice)
            (Some(label), None) => alloc.text(ctx.snippet(label.text_range()).trim().to_string()),
            // Empty (shouldn't happen)
            (None, None) => alloc.nil(),
        }
    }
}

impl ToDoc for ast::RecordInitExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        use super::types::intersperse;
        let alloc = &ctx.alloc;

        let path = match self.path() {
            Some(p) => p.to_doc(ctx),
            None => return alloc.nil(),
        };

        let fields: Vec<_> = self
            .fields()
            .map(|f| f.into_iter().map(|field| field.to_doc(ctx)).collect())
            .unwrap_or_default();

        if fields.is_empty() {
            return path.append(alloc.text(" {}"));
        }

        let indent = ctx.config.indent_width as isize;
        let sep = alloc.text(",").append(alloc.line());
        let inner = intersperse(alloc, fields, sep);
        let trailing = alloc.text(",").flat_alt(alloc.nil());

        // Use line() for spaced variant: renders as space when flat, newline when broken
        let break_token = alloc.line();

        let body = alloc
            .text("{")
            .append(
                break_token
                    .clone()
                    .append(inner)
                    .append(trailing)
                    .nest(indent),
            )
            .append(break_token)
            .append(alloc.text("}"))
            .max_width_group(ctx.config.struct_lit_width);

        path.append(alloc.text(" ")).append(body)
    }
}

impl ToDoc for ast::AssignExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let lhs = match self.lhs_expr() {
            Some(e) => e.to_doc(ctx),
            None => return alloc.nil(),
        };

        let rhs_expr = match self.rhs_expr() {
            Some(e) => e,
            None => return lhs,
        };

        // If RHS is a chain, use BlockDoc for intelligent breaking
        if is_chain(&rhs_expr) {
            let prefix = lhs.append(alloc.text(" = "));
            format_chain_with_prefix(prefix, &rhs_expr, ctx)
        } else {
            lhs.append(alloc.text(" = ")).append(rhs_expr.to_doc(ctx))
        }
    }
}

impl ToDoc for ast::AugAssignExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let lhs = match self.lhs_expr() {
            Some(e) => e.to_doc(ctx),
            None => return alloc.nil(),
        };
        let op = match self.op() {
            Some(o) => ctx.snippet_node_or_token(&o.syntax()).to_string(),
            None => return lhs,
        };
        let rhs = match self.rhs_expr() {
            Some(e) => e.to_doc(ctx),
            None => return lhs,
        };

        lhs.append(alloc.text(" "))
            .append(alloc.text(op))
            .append(alloc.text("= "))
            .append(rhs)
    }
}

impl ToDoc for ast::PathExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.path() {
            Some(p) => p.to_doc(ctx),
            None => ctx.alloc.nil(),
        }
    }
}

impl ToDoc for ast::FieldExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        // Delegate to chain formatting which handles the entire chain at once
        format_chain(&ast::Expr::cast(self.syntax().clone()).unwrap(), ctx)
    }
}

impl ToDoc for ast::IndexExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let expr = match self.expr() {
            Some(e) => e.to_doc(ctx),
            None => return alloc.nil(),
        };
        let index = match self.index() {
            Some(i) => i.to_doc(ctx),
            None => return expr,
        };

        expr.append(alloc.text("["))
            .append(index)
            .append(alloc.text("]"))
    }
}

impl ToDoc for ast::LitExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.lit() {
            Some(l) => ctx.alloc.text(ctx.snippet_trimmed(&l)),
            None => ctx.alloc.nil(),
        }
    }
}

impl ToDoc for ast::IfExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let cond = match self.cond() {
            Some(c) => c.to_doc(ctx),
            None => return alloc.nil(),
        };
        let then = match self.then() {
            Some(t) => t.to_doc(ctx),
            None => return alloc.text("if ").append(cond),
        };

        let if_then = alloc
            .text("if ")
            .append(cond)
            .append(alloc.text(" "))
            .append(then);

        match self.else_() {
            Some(e) => if_then
                .append(alloc.text(" else "))
                .append(e.to_doc(ctx))
                .max_width_group(ctx.config.single_line_if_else_max_width),
            None => if_then.max_width_group(ctx.config.single_line_if_max_width),
        }
    }
}

impl ToDoc for ast::UsesClause {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if let Some(params) = self.param_list() {
            let params_docs: Vec<_> = params.into_iter().map(|p| p.to_doc(ctx)).collect();

            let clause_indent = ctx.config.clause_indent as isize;
            alloc
                .text("uses ")
                .append(block_list(ctx, "(", ")", params_docs, clause_indent, true))
        } else if let Some(param) = self.param() {
            alloc.text("uses ").append(param.to_doc(ctx))
        } else {
            alloc.nil()
        }
    }
}

impl ToDoc for ast::UsesParam {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;
        let mut doc = alloc.nil();

        if self.mut_token().is_some() {
            doc = doc.append(alloc.text("mut "));
        }

        if let Some(name) = self.name() {
            let name_text = ctx.snippet(name.syntax().text_range()).trim().to_string();
            doc = doc.append(alloc.text(name_text)).append(alloc.text(": "));
        }

        if let Some(path) = self.path() {
            doc = doc.append(path.to_doc(ctx));
        }

        doc
    }
}

impl ToDoc for ast::MatchExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let scrutinee = match self.scrutinee() {
            Some(s) => s.to_doc(ctx),
            None => return alloc.nil(),
        };

        let arms: Vec<_> = self
            .arms()
            .map(|arms| {
                arms.into_iter()
                    .filter_map(|arm| {
                        let pat = arm.pat()?.to_doc(ctx);
                        let body = arm.body()?.to_doc(ctx);
                        Some(
                            pat.append(alloc.text(" => "))
                                .append(body)
                                .append(alloc.text(",")),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default();

        if arms.is_empty() {
            return alloc
                .text("match ")
                .append(scrutinee)
                .append(alloc.text(" {}"));
        }

        let arms_doc = alloc.concat(arms.into_iter().map(|arm| alloc.hardline().append(arm)));

        alloc
            .text("match ")
            .append(scrutinee)
            .append(alloc.text(" {"))
            .append(arms_doc.nest(ctx.config.indent_width as isize))
            .append(alloc.hardline())
            .append(alloc.text("}"))
    }
}

impl ToDoc for ast::WithParam {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let path = match self.path() {
            Some(p) => p.to_doc(ctx),
            None => return alloc.nil(),
        };
        let value = match self.value_expr() {
            Some(v) => v.to_doc(ctx),
            None => return path,
        };

        path.append(alloc.text(" = ")).append(value)
    }
}

impl ToDoc for ast::WithExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let params: Vec<_> = self
            .params()
            .map(|p| p.into_iter().map(|param| param.to_doc(ctx)).collect())
            .unwrap_or_default();

        let indent = ctx.config.indent_width as isize;
        let params_doc = block_list(ctx, "(", ")", params, indent, true);

        let body = match self.body() {
            Some(b) => b.to_doc(ctx),
            None => return alloc.text("with ").append(params_doc),
        };

        alloc
            .text("with ")
            .append(params_doc)
            .append(alloc.text(" "))
            .append(body)
    }
}

impl ToDoc for ast::TupleExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let elems: Vec<_> = self.elems().flatten().map(|e| e.to_doc(ctx)).collect();

        let indent = ctx.config.indent_width as isize;
        block_list(ctx, "(", ")", elems, indent, true)
    }
}

impl ToDoc for ast::ArrayExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let elems: Vec<_> = self.elems().flatten().map(|e| e.to_doc(ctx)).collect();

        let indent = ctx.config.indent_width as isize;
        block_list(ctx, "[", "]", elems, indent, true)
    }
}

impl ToDoc for ast::ArrayRepExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let val = match self.val() {
            Some(v) => v.to_doc(ctx),
            None => return alloc.nil(),
        };
        let len = match self.len() {
            Some(l) => l.to_doc(ctx),
            None => return alloc.text("[").append(val).append(alloc.text("]")),
        };

        alloc
            .text("[")
            .append(val)
            .append(alloc.text("; "))
            .append(len)
            .append(alloc.text("]"))
    }
}

impl ToDoc for ast::ParenExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let expr = match self.expr() {
            Some(e) => e.to_doc(ctx),
            None => return alloc.text("()"),
        };

        alloc.text("(").append(expr).append(alloc.text(")"))
    }
}

impl ToDoc for ast::BlockExpr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        use parser::TextRange;
        use parser::syntax_kind::SyntaxKind;
        use parser::syntax_node::NodeOrToken;

        let alloc = &ctx.alloc;

        let has_stmt = self.stmts().next().is_some();
        let has_item = self.items().next().is_some();
        let has_comment = self
            .syntax()
            .children_with_tokens()
            .any(|child| matches!(child, NodeOrToken::Token(t) if t.kind() == SyntaxKind::Comment));

        if !has_stmt && !has_item && !has_comment {
            return alloc.text("{}");
        }

        // Collect all block elements with their source ranges for blank line detection
        struct BlockElement<'a> {
            doc: Doc<'a>,
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
                            doc: stmt.to_doc(ctx),
                            range,
                        });
                    } else if let Some(item) = ast::Item::cast(node.clone()) {
                        elements.push(BlockElement {
                            doc: item.to_doc(ctx),
                            range,
                        });
                    }
                }
                NodeOrToken::Token(tok) => {
                    if tok.kind() == SyntaxKind::Comment {
                        let comment_doc =
                            alloc.text(ctx.snippet(tok.text_range()).trim().to_string());

                        // If the comment is on the same line as the previous element, treat it
                        // as a trailing comment on that line instead of forcing a new line.
                        if let Some(last) = elements.last_mut() {
                            let gap = TextRange::new(last.range.end(), tok.text_range().start());
                            let gap_text = ctx.snippet(gap);
                            let has_newline = gap_text.chars().any(|c| c == '\n');

                            if !has_newline {
                                last.doc =
                                    last.doc.clone().append(alloc.text(" ")).append(comment_doc);
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
            return alloc.text("{}");
        }

        let indent = ctx.config.indent_width as isize;

        // Check if this is a simple single-element block that can render on one line
        // (no blank lines between elements, no comments as standalone elements)
        if elements.len() == 1 && !has_comment {
            // Simple block: can render as `{ expr }` on one line or break to multiple lines
            // Don't use .group() here - let the parent (e.g., if-else) control breaking
            let elem_doc = elements.into_iter().next().unwrap().doc;
            return alloc
                .text("{")
                .append(alloc.line().append(elem_doc).nest(indent))
                .append(alloc.line())
                .append(alloc.text("}"));
        }

        // Complex block: always use hard line breaks
        let mut inner = alloc.nil();
        let mut prev_end: Option<parser::TextSize> = None;

        for elem in elements {
            // Check if there was a blank line before this element
            let needs_blank_line = if let Some(prev) = prev_end {
                let gap = TextRange::new(prev, elem.range.start());
                let gap_text = ctx.snippet(gap);
                gap_text.chars().filter(|c| *c == '\n').count() >= 2
            } else {
                false
            };

            if needs_blank_line {
                // Extra hardline for blank line (will have trailing whitespace - cleaned up in post-processing)
                inner = inner.append(alloc.hardline());
            }
            inner = inner.append(alloc.hardline()).append(elem.doc);
            prev_end = Some(elem.range.end());
        }

        alloc
            .text("{")
            .append(inner.nest(indent))
            .append(alloc.hardline())
            .append(alloc.text("}"))
    }
}
