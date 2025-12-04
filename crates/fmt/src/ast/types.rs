//! Formatting for types, paths, generics, and type-related constructs.

use pretty::RcDoc;

use crate::{RewriteContext, Shape};
use parser::ast::{self, GenericArgKind, GenericArgsOwner, GenericParamKind, TypeKind};

/// Extension trait for converting AST nodes to pretty documents.
pub trait ToDoc {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()>;
}

/// Helper to intersperse documents with a separator.
pub fn intersperse<'a>(
    docs: impl IntoIterator<Item = RcDoc<'a, ()>>,
    sep: RcDoc<'a, ()>,
) -> RcDoc<'a, ()> {
    let mut iter = docs.into_iter();
    match iter.next() {
        None => RcDoc::nil(),
        Some(first) => iter.fold(first, |acc, doc| acc.append(sep.clone()).append(doc)),
    }
}

/// Creates a Rust-style block format for delimited lists.
///
/// When flat (non-spaced): `(item1, item2)` (e.g., for parens/brackets)
/// When flat (spaced): `{ item1, item2 }` (e.g., for braces)
/// When broken:
/// ```text
/// open
///     item1,
///     item2,
/// close
/// ```
///
/// The `trailing_comma` parameter controls whether a trailing comma is added
/// when the list is broken across multiple lines.
pub fn block_list<'a>(
    open: &'a str,
    close: &'a str,
    items: Vec<RcDoc<'a, ()>>,
    indent: isize,
    trailing_comma: bool,
) -> RcDoc<'a, ()> {
    block_list_inner(open, close, items, indent, trailing_comma, false)
}

/// Like `block_list`, but adds spaces inside delimiters when rendered flat.
/// Use this for brace-delimited lists like `{ x, y }`.
pub fn block_list_spaced<'a>(
    open: &'a str,
    close: &'a str,
    items: Vec<RcDoc<'a, ()>>,
    indent: isize,
    trailing_comma: bool,
) -> RcDoc<'a, ()> {
    block_list_inner(open, close, items, indent, trailing_comma, true)
}

fn block_list_inner<'a>(
    open: &'a str,
    close: &'a str,
    items: Vec<RcDoc<'a, ()>>,
    indent: isize,
    trailing_comma: bool,
    spaced: bool,
) -> RcDoc<'a, ()> {
    if items.is_empty() {
        return RcDoc::text(format!("{}{}", open, close));
    }

    let sep = RcDoc::text(",").append(RcDoc::line());
    let inner = intersperse(items, sep);

    let trailing = if trailing_comma {
        RcDoc::text(",").flat_alt(RcDoc::nil())
    } else {
        RcDoc::nil()
    };

    // For spaced variant, use line() which renders as space when flat
    // For non-spaced variant, use line_() which renders as empty when flat
    let break_token = if spaced {
        RcDoc::line()
    } else {
        RcDoc::line_()
    };

    RcDoc::text(open)
        .append(
            break_token
                .clone()
                .append(inner)
                .append(trailing)
                .nest(indent),
        )
        .append(break_token)
        .append(RcDoc::text(close))
        .group()
}

impl ToDoc for ast::GenericParamList {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let params: Vec<_> = self.into_iter().map(|p| p.to_doc(context, shape)).collect();

        let indent = context.config.indent_width as isize;
        block_list("<", ">", params, indent, true)
    }
}

impl ToDoc for ast::GenericParam {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.kind() {
            GenericParamKind::Type(ty_param) => ty_param.to_doc(context, shape),
            GenericParamKind::Const(const_param) => const_param.to_doc(context, shape),
        }
    }
}

impl ToDoc for ast::TypeGenericParam {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let name = self
            .name()
            .map(|n| RcDoc::text(context.token(&n).to_string()))
            .unwrap_or_else(RcDoc::nil);

        let bounds = self
            .bounds()
            .map(|b| b.to_doc(context, shape))
            .unwrap_or_else(RcDoc::nil);

        let default = self
            .default_ty()
            .map(|ty| RcDoc::text(" = ").append(ty.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        name.append(bounds).append(default)
    }
}

impl ToDoc for ast::ConstGenericParam {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let name = self
            .name()
            .map(|n| RcDoc::text(context.token(&n).to_string()))
            .unwrap_or_else(RcDoc::nil);

        let ty = self
            .ty()
            .map(|ty| RcDoc::text(": ").append(ty.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        RcDoc::text("const ").append(name).append(ty)
    }
}

impl ToDoc for ast::WhereClause {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let predicates: Vec<_> = self
            .into_iter()
            .map(|pred| pred.to_doc(context, shape))
            .collect();

        if predicates.is_empty() {
            return RcDoc::nil();
        }

        let sep = RcDoc::text(",").append(RcDoc::line());
        let inner = intersperse(predicates, sep);

        inner.group()
    }
}

impl ToDoc for ast::WherePredicate {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let ty = match self.ty() {
            Some(t) => t.to_doc(context, shape),
            None => return RcDoc::nil(),
        };

        if let Some(bounds) = self.bounds() {
            ty.append(bounds.to_doc(context, shape))
        } else {
            ty
        }
    }
}

impl ToDoc for ast::TypeBoundList {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let bounds: Vec<_> = self
            .into_iter()
            .map(|bound| bound.to_doc(context, shape))
            .collect();

        if bounds.is_empty() {
            return RcDoc::nil();
        }

        let sep = RcDoc::text(" + ");
        RcDoc::text(": ").append(intersperse(bounds, sep))
    }
}

impl ToDoc for ast::TypeBound {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        if let Some(trait_bound) = self.trait_bound() {
            trait_bound.to_doc(context, shape)
        } else if let Some(kind_bound) = self.kind_bound() {
            RcDoc::text(context.snippet_trimmed(&kind_bound))
        } else {
            RcDoc::nil()
        }
    }
}

impl ToDoc for ast::TraitRef {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.path() {
            Some(p) => p.to_doc(context, shape),
            None => RcDoc::nil(),
        }
    }
}

impl ToDoc for ast::SuperTraitList {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let traits: Vec<_> = self.into_iter().map(|t| t.to_doc(context, shape)).collect();

        if traits.is_empty() {
            return RcDoc::nil();
        }

        let sep = RcDoc::text(" + ");
        RcDoc::text(": ").append(intersperse(traits, sep))
    }
}

impl ToDoc for ast::Type {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.kind() {
            TypeKind::Ptr(ptr) => ptr.to_doc(context, shape),
            TypeKind::Path(path) => path.to_doc(context, shape),
            TypeKind::Tuple(tuple) => tuple.to_doc(context, shape),
            TypeKind::Array(array) => array.to_doc(context, shape),
            TypeKind::Never(never) => never.to_doc(context, shape),
        }
    }
}

impl ToDoc for ast::PtrType {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.inner() {
            Some(inner) => RcDoc::text("*").append(inner.to_doc(context, shape)),
            None => RcDoc::text("*"),
        }
    }
}

impl ToDoc for ast::PathType {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.path() {
            Some(p) => p.to_doc(context, shape),
            None => RcDoc::nil(),
        }
    }
}

impl ToDoc for ast::Path {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let segments: Vec<_> = self
            .segments()
            .map(|seg| seg.to_doc(context, shape))
            .collect();

        let sep = RcDoc::text("::");
        intersperse(segments, sep)
    }
}

impl ToDoc for ast::PathSegment {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let mut doc = RcDoc::nil();

        if let Some(kind) = self.kind() {
            match kind {
                ast::PathSegmentKind::QualifiedType(q) => {
                    doc = q.to_doc(context, shape);
                }
                _ => {
                    if let Some(ident) = self.ident() {
                        doc = RcDoc::text(context.snippet(ident.text_range()).trim().to_string());
                    }
                }
            }
        }

        if let Some(args) = self.generic_args() {
            doc = doc.append(args.to_doc(context, shape));
        }

        doc
    }
}

impl ToDoc for ast::QualifiedType {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let ty = match self.ty() {
            Some(t) => t.to_doc(context, shape),
            None => return RcDoc::nil(),
        };
        let trait_path = match self.trait_qualifier() {
            Some(p) => p.to_doc(context, shape),
            None => return RcDoc::nil(),
        };

        RcDoc::text("<")
            .append(ty)
            .append(RcDoc::text(" as "))
            .append(trait_path)
            .append(RcDoc::text(">"))
    }
}

impl ToDoc for ast::GenericArgList {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let args: Vec<_> = self.into_iter().map(|a| a.to_doc(context, shape)).collect();

        let indent = context.config.indent_width as isize;
        block_list("<", ">", args, indent, true)
    }
}

impl ToDoc for ast::GenericArg {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.kind() {
            GenericArgKind::Type(ty_arg) => ty_arg.to_doc(context, shape),
            GenericArgKind::Const(const_arg) => const_arg.to_doc(context, shape),
            GenericArgKind::AssocType(assoc_arg) => assoc_arg.to_doc(context, shape),
        }
    }
}

impl ToDoc for ast::TypeGenericArg {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.ty() {
            Some(t) => t.to_doc(context, shape),
            None => RcDoc::nil(),
        }
    }
}

impl ToDoc for ast::ConstGenericArg {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.expr() {
            Some(e) => e.to_doc(context, shape),
            None => RcDoc::nil(),
        }
    }
}

impl ToDoc for ast::AssocTypeGenericArg {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let name = match self.name() {
            Some(n) => context.snippet(n.text_range()).trim().to_string(),
            None => return RcDoc::nil(),
        };
        let ty = match self.ty() {
            Some(t) => t.to_doc(context, shape),
            None => return RcDoc::nil(),
        };

        RcDoc::text(name).append(RcDoc::text(" = ")).append(ty)
    }
}

impl ToDoc for ast::TupleType {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let elems: Vec<_> = self.elem_tys().map(|e| e.to_doc(context, shape)).collect();

        let indent = context.config.indent_width as isize;
        block_list("(", ")", elems, indent, true)
    }
}

impl ToDoc for ast::ArrayType {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let elem_ty = match self.elem_ty() {
            Some(t) => t.to_doc(context, shape),
            None => return RcDoc::nil(),
        };
        let len = match self.len() {
            Some(l) => l.to_doc(context, shape),
            None => return RcDoc::nil(),
        };

        RcDoc::text("[")
            .append(elem_ty)
            .append(RcDoc::text("; "))
            .append(len)
            .append(RcDoc::text("]"))
    }
}

impl ToDoc for ast::NeverType {
    fn to_doc<'a>(&self, _context: &RewriteContext<'_>, _shape: Shape) -> RcDoc<'a, ()> {
        RcDoc::text("!")
    }
}

// Forward declaration for expr::ToDoc - dispatches to specific expression types
impl ToDoc for ast::Expr {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        use parser::ast::ExprKind;
        match self.kind() {
            ExprKind::Lit(lit) => lit.to_doc(context, shape),
            ExprKind::Block(block) => block.to_doc(context, shape),
            ExprKind::Bin(bin) => bin.to_doc(context, shape),
            ExprKind::Un(un) => un.to_doc(context, shape),
            ExprKind::Call(call) => call.to_doc(context, shape),
            ExprKind::MethodCall(method) => method.to_doc(context, shape),
            ExprKind::Path(path) => path.to_doc(context, shape),
            ExprKind::RecordInit(record) => record.to_doc(context, shape),
            ExprKind::Field(field) => field.to_doc(context, shape),
            ExprKind::Index(index) => index.to_doc(context, shape),
            ExprKind::Tuple(tuple) => tuple.to_doc(context, shape),
            ExprKind::Array(array) => array.to_doc(context, shape),
            ExprKind::ArrayRep(array_rep) => array_rep.to_doc(context, shape),
            ExprKind::If(if_expr) => if_expr.to_doc(context, shape),
            ExprKind::Match(match_expr) => match_expr.to_doc(context, shape),
            ExprKind::With(with_expr) => with_expr.to_doc(context, shape),
            ExprKind::Paren(paren) => paren.to_doc(context, shape),
            ExprKind::Assign(assign) => assign.to_doc(context, shape),
            ExprKind::AugAssign(aug_assign) => aug_assign.to_doc(context, shape),
        }
    }
}
