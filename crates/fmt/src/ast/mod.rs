mod expr;
mod items;
mod pat;
mod stmt;
mod types;

use crate::{Indent, RewriteContext, RewriteExt, Shape};
use parser::ast::{
    AttrListOwner, GenericParamsOwner, ItemModifierOwner, WhereClauseOwner, prelude::AstNode,
};

/// Write attributes for a node that has an attribute list.
pub(crate) fn write_attrs<N: AttrListOwner + AstNode>(
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

/// Write the item modifier (pub, unsafe) for a node.
pub(crate) fn write_item_modifier<N: ItemModifierOwner + AstNode>(node: &N, out: &mut String) {
    if let Some(modifier) = node.modifier() {
        if modifier.pub_kw().is_some() {
            out.push_str("pub ");
        }
        if modifier.unsafe_kw().is_some() {
            out.push_str("unsafe ");
        }
    }
}

/// Write generic parameters for a node that has them.
pub(crate) fn write_generics<N: GenericParamsOwner + AstNode>(
    node: &N,
    context: &RewriteContext<'_>,
    shape: Shape,
    out: &mut String,
) {
    if let Some(generics) = node.generic_params() {
        out.push_str(&generics.rewrite_or_original(context, shape));
    }
}

/// Write a where clause for a node that has one.
pub(crate) fn write_where_clause<N: WhereClauseOwner + AstNode>(
    node: &N,
    context: &RewriteContext<'_>,
    shape: Shape,
    out: &mut String,
) {
    if let Some(where_clause) = node.where_clause() {
        out.push(' ');
        out.push_str(&where_clause.rewrite_or_original(context, shape));
    }
}

/// Format a block of items with proper indentation.
pub(crate) fn rewrite_block_items<T>(
    items: impl IntoIterator<Item = T>,
    context: &RewriteContext<'_>,
    shape: Shape,
) -> Option<String>
where
    T: RewriteExt,
{
    let outer_indent = shape.indent.indent_width();
    let indent_width = context.config.indent_width;
    let inner_indent = outer_indent + indent_width;
    let inner_shape = Shape::with_width(shape.width, Indent::from_block(inner_indent));

    let items: Vec<T> = items.into_iter().collect();

    if items.is_empty() {
        return Some("{}".to_string());
    }

    let mut out = String::new();
    out.push_str("{\n");

    for item in items {
        push_indent(&mut out, inner_indent);
        out.push_str(&item.rewrite_or_original(context, inner_shape));
        out.push('\n');
    }

    push_indent(&mut out, outer_indent);
    out.push('}');

    Some(out)
}

/// Push indentation spaces to the output string.
pub(crate) fn push_indent(out: &mut String, width: usize) {
    Indent::push_to(width, out);
}
