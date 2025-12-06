//! Formatting for top-level items (functions, structs, enums, traits, etc.)

use pretty::DocAllocator;

use crate::RewriteContext;
use parser::ast::{self, ItemKind, ItemModifierOwner, TraitItemKind, prelude::AstNode};

use super::types::{Doc, ToDoc, block_list, block_list_spaced, intersperse};

/// Helper to build attributes document for a node.
fn attrs_doc<'a, N: ast::AttrListOwner + AstNode>(
    node: &N,
    ctx: &'a RewriteContext<'a>,
) -> Doc<'a> {
    if let Some(attrs) = node.attr_list() {
        attrs.to_doc(ctx)
    } else {
        ctx.alloc.nil()
    }
}

/// Helper to build item modifier document (pub, unsafe).
fn modifier_doc<'a, N: ItemModifierOwner + AstNode>(
    node: &N,
    ctx: &'a RewriteContext<'a>,
) -> Doc<'a> {
    let alloc = &ctx.alloc;
    let mut doc = alloc.nil();
    if let Some(modifier) = node.modifier() {
        if modifier.pub_kw().is_some() {
            doc = doc.append(alloc.text("pub "));
        }
        if modifier.unsafe_kw().is_some() {
            doc = doc.append(alloc.text("unsafe "));
        }
    }
    doc
}

/// Helper to build generics document.
fn generics_doc<'a, N: ast::GenericParamsOwner + AstNode>(
    node: &N,
    ctx: &'a RewriteContext<'a>,
) -> Doc<'a> {
    if let Some(generics) = node.generic_params() {
        generics.to_doc(ctx)
    } else {
        ctx.alloc.nil()
    }
}

/// Helper to build where clause document.
fn where_doc<'a, N: ast::WhereClauseOwner + AstNode>(
    node: &N,
    ctx: &'a RewriteContext<'a>,
) -> Doc<'a> {
    let alloc = &ctx.alloc;

    if let Some(where_clause) = node.where_clause() {
        let preds: Vec<_> = where_clause
            .into_iter()
            .map(|pred| pred.to_doc(ctx))
            .collect();

        if preds.is_empty() {
            return alloc.nil();
        }

        let sep = alloc.text(",").append(alloc.line());
        let preds_doc = intersperse(alloc, preds, sep).group();

        let where_block = alloc
            .text("where")
            .append(
                alloc
                    .line()
                    .append(preds_doc)
                    .nest(ctx.config.clause_indent as isize),
            )
            .group();

        if ctx.config.where_new_line {
            alloc.hardline().append(where_block)
        } else {
            alloc.line().append(where_block).group()
        }
    } else {
        alloc.nil()
    }
}

/// Format a block of items `{ ... }` preserving blank lines between items.
/// Takes a syntax node and a function to cast child nodes to the item type.
fn block_items_doc<'a, T: ToDoc>(
    syntax: &parser::SyntaxNode,
    cast_fn: impl Fn(parser::SyntaxNode) -> Option<T>,
    ctx: &'a RewriteContext<'a>,
) -> Doc<'a> {
    use parser::syntax_kind::SyntaxKind;
    use parser::syntax_node::NodeOrToken;

    let alloc = &ctx.alloc;
    let mut inner = alloc.nil();
    let mut pending_newlines = 0usize;
    let mut is_first = true;

    for child in syntax.children_with_tokens() {
        match child {
            NodeOrToken::Node(node) => {
                if let Some(item) = cast_fn(node) {
                    // Always add at least one newline before each item.
                    // If source had 2+ newlines (blank line), add exactly 2 (one blank line).
                    // Multiple blank lines are collapsed to one.
                    let newlines_to_add = if pending_newlines >= 2 { 2 } else { 1 };
                    for _ in 0..newlines_to_add {
                        inner = inner.append(alloc.hardline());
                    }
                    pending_newlines = 0;
                    is_first = false;
                    inner = inner.append(item.to_doc(ctx));
                }
            }
            NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::Newline {
                    let text = ctx.snippet(token.text_range());
                    pending_newlines = text.chars().filter(|c| *c == '\n').count();
                }
            }
        }
    }

    if is_first {
        return alloc.text("{}");
    }

    alloc
        .text("{")
        .append(inner.nest(ctx.config.indent_width as isize))
        .append(alloc.hardline())
        .append(alloc.text("}"))
}

impl ToDoc for ast::Root {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        use parser::syntax_kind::SyntaxKind;
        use parser::syntax_node::NodeOrToken;

        let alloc = &ctx.alloc;
        let mut result = alloc.nil();
        let mut pending_newlines = 0usize;
        let mut is_first = true;

        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    if !is_first {
                        // Collapse multiple blank lines to one
                        let newlines_to_add = if pending_newlines >= 2 { 2 } else { 1 };
                        for _ in 0..newlines_to_add {
                            result = result.append(alloc.hardline());
                        }
                    }
                    pending_newlines = 0;
                    is_first = false;

                    if let Some(item_list) = ast::ItemList::cast(node.clone()) {
                        result = result.append(item_list.to_doc(ctx));
                    } else {
                        result =
                            result.append(alloc.text(ctx.snippet(node.text_range()).to_string()));
                    }
                }
                NodeOrToken::Token(token) => {
                    match token.kind() {
                        SyntaxKind::Newline => {
                            let text = ctx.snippet(token.text_range());
                            pending_newlines = text.chars().filter(|c| *c == '\n').count();
                        }
                        SyntaxKind::WhiteSpace => {
                            // Skip stray whitespace between items/comments
                            continue;
                        }
                        _ => {
                            if !is_first {
                                // Collapse multiple blank lines to one
                                let newlines_to_add = if pending_newlines >= 2 { 2 } else { 1 };
                                for _ in 0..newlines_to_add {
                                    result = result.append(alloc.hardline());
                                }
                            }
                            pending_newlines = 0;
                            is_first = false;

                            let mut text = ctx.snippet(token.text_range()).to_string();
                            if token.kind() == SyntaxKind::Comment
                                || token.kind() == SyntaxKind::DocComment
                            {
                                text = text.trim_end().to_string();
                            }
                            result = result.append(alloc.text(text));
                        }
                    }
                }
            }
        }
        result
    }
}

impl ToDoc for ast::ItemList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        use parser::syntax_kind::SyntaxKind;
        use parser::syntax_node::NodeOrToken;

        let alloc = &ctx.alloc;
        let mut result = alloc.nil();
        let mut pending_newlines = 0usize;
        let mut is_first = true;

        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    if let Some(item) = ast::Item::cast(node.clone()) {
                        // Add newlines that were accumulated from whitespace
                        if !is_first {
                            // Collapse multiple blank lines to one
                            let newlines_to_add = if pending_newlines >= 2 { 2 } else { 1 };
                            for _ in 0..newlines_to_add {
                                result = result.append(alloc.hardline());
                            }
                        }
                        pending_newlines = 0;
                        is_first = false;
                        result = result.append(item.to_doc(ctx));
                    } else {
                        result =
                            result.append(alloc.text(ctx.snippet(node.text_range()).to_string()));
                    }
                }
                NodeOrToken::Token(token) => {
                    if token.kind() == SyntaxKind::Newline {
                        let text = ctx.snippet(token.text_range());
                        pending_newlines = text.chars().filter(|c| *c == '\n').count();
                    } else if token.kind() == SyntaxKind::Comment {
                        // Add newlines that were accumulated from whitespace
                        if !is_first {
                            // Collapse multiple blank lines to one
                            let newlines_to_add = if pending_newlines >= 2 { 2 } else { 1 };
                            for _ in 0..newlines_to_add {
                                result = result.append(alloc.hardline());
                            }
                        }
                        pending_newlines = 0;
                        is_first = false;
                        result = result
                            .append(alloc.text(ctx.snippet(token.text_range()).trim().to_string()));
                    } else {
                        // Skip other tokens
                    }
                }
            }
        }
        result
    }
}

impl ToDoc for ast::Item {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.kind() {
            Some(ItemKind::Mod(mod_)) => mod_.to_doc(ctx),
            Some(ItemKind::Func(func)) => func.to_doc(ctx),
            Some(ItemKind::Struct(struct_)) => struct_.to_doc(ctx),
            Some(ItemKind::Contract(contract)) => contract.to_doc(ctx),
            Some(ItemKind::Enum(enum_)) => enum_.to_doc(ctx),
            Some(ItemKind::TypeAlias(type_alias)) => type_alias.to_doc(ctx),
            Some(ItemKind::Impl(impl_)) => impl_.to_doc(ctx),
            Some(ItemKind::Trait(trait_)) => trait_.to_doc(ctx),
            Some(ItemKind::ImplTrait(impl_trait)) => impl_trait.to_doc(ctx),
            Some(ItemKind::Const(const_)) => const_.to_doc(ctx),
            Some(ItemKind::Use(use_)) => use_.to_doc(ctx),
            Some(ItemKind::Extern(extern_)) => extern_.to_doc(ctx),
            None => ctx.alloc.nil(),
        }
    }
}

impl ToDoc for ast::FuncSignature {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let name = match self.name() {
            Some(n) => ctx.token(&n).to_string(),
            None => return alloc.text("fn"),
        };

        let generics = generics_doc(self, ctx);

        let params: Vec<_> = self
            .params()
            .map(|p| p.into_iter().map(|param| param.to_doc(ctx)).collect())
            .unwrap_or_default();

        let indent = ctx.config.indent_width as isize;
        let params_doc = block_list(ctx, "(", ")", params, indent, true);

        let ret_doc = self
            .ret_ty()
            .map(|ty| alloc.text(" -> ").append(ty.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        let uses_doc = self
            .uses_clause()
            .map(|u| alloc.text(" ").append(u.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        let where_clause = where_doc(self, ctx);

        alloc
            .text("fn ")
            .append(alloc.text(name))
            .append(generics)
            .append(params_doc)
            .append(ret_doc)
            .append(uses_doc)
            .append(where_clause)
            .group()
    }
}

impl ToDoc for ast::Func {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);
        let sig = self.sig().to_doc(ctx);

        let doc = if let Some(body) = self.body() {
            // Decide whether the body can stay on the same line as the signature.
            // Render the signature at the current max width to see if it breaks.
            let (sig_len, sig_multiline) = {
                let mut buf = Vec::new();
                let _ = sig
                    .clone()
                    .into_doc()
                    .render(ctx.config.max_width, &mut buf);
                let s = String::from_utf8(buf).unwrap_or_default();
                let mut lines = s.lines();
                let first_len = lines.next().map(|l| l.len()).unwrap_or(0);
                let multiline = lines.next().is_some();
                (first_len, multiline)
            };

            let has_where = self.sig().where_clause().is_some();
            let body_sep = if ctx.config.where_new_line
                || has_where
                    && (sig_multiline || sig_len + 2 /* space + '{' */ > ctx.config.max_width)
            {
                alloc.hardline()
            } else {
                alloc.text(" ")
            };

            sig.append(body_sep).append(body.to_doc(ctx))
        } else {
            sig
        };

        attrs.append(modifier).append(doc)
    }
}

impl ToDoc for ast::FuncParamList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let params: Vec<_> = self.into_iter().map(|p| p.to_doc(ctx)).collect();

        let indent = ctx.config.indent_width as isize;
        block_list(ctx, "(", ")", params, indent, true)
    }
}

impl ToDoc for ast::FuncParam {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;
        let mut doc = alloc.nil();

        if self.mut_token().is_some() {
            doc = doc.append(alloc.text("mut "));
        }

        let label = self.label();
        let name = self.name();

        if let (Some(label), Some(name_ref)) = (&label, &name)
            && label.syntax().text_range() != name_ref.syntax().text_range()
        {
            doc =
                doc.append(alloc.text(ctx.snippet(label.syntax().text_range()).trim().to_string()));
            doc = doc.append(alloc.text(" "));
        }

        if let Some(name) = name {
            doc =
                doc.append(alloc.text(ctx.snippet(name.syntax().text_range()).trim().to_string()));
        }

        if let Some(ty) = self.ty() {
            doc = doc.append(alloc.text(": ")).append(ty.to_doc(ctx));
        }

        doc
    }
}

impl ToDoc for ast::Struct {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let name = self
            .name()
            .map(|n| ctx.token(&n).to_string())
            .unwrap_or_default();
        let generics = generics_doc(self, ctx);
        let where_clause = where_doc(self, ctx);

        let fields_doc = self
            .fields()
            .map(|f| alloc.text(" ").append(f.to_doc(ctx)))
            .unwrap_or_else(|| alloc.text(" {}"));

        attrs
            .append(modifier)
            .append(alloc.text("struct "))
            .append(alloc.text(name))
            .append(generics)
            .append(where_clause)
            .append(fields_doc)
    }
}

impl ToDoc for ast::RecordFieldDefList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let fields: Vec<_> = self.into_iter().map(|f| f.to_doc(ctx)).collect();

        let indent = ctx.config.indent_width as isize;
        block_list_spaced(ctx, "{", "}", fields, indent, true)
    }
}

impl ToDoc for ast::RecordFieldDef {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);

        let mut doc = attrs;

        if self.pub_kw().is_some() {
            doc = doc.append(alloc.text("pub "));
        }

        if let Some(name) = self.name() {
            doc = doc.append(alloc.text(ctx.token(&name).to_string()));
        }

        if let Some(ty) = self.ty() {
            doc = doc.append(alloc.text(": ")).append(ty.to_doc(ctx));
        }

        doc
    }
}

impl ToDoc for ast::Contract {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let name = self
            .name()
            .map(|n| ctx.token(&n).to_string())
            .unwrap_or_default();

        let fields_doc = self
            .fields()
            .map(|f| alloc.text(" ").append(f.to_doc(ctx)))
            .unwrap_or_else(|| alloc.text(" {}"));

        attrs
            .append(modifier)
            .append(alloc.text("contract "))
            .append(alloc.text(name))
            .append(fields_doc)
    }
}

impl ToDoc for ast::Enum {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let name = self
            .name()
            .map(|n| ctx.token(&n).to_string())
            .unwrap_or_default();
        let generics = generics_doc(self, ctx);
        let where_clause = where_doc(self, ctx);

        let variants_doc = self
            .variants()
            .map(|v| alloc.text(" ").append(v.to_doc(ctx)))
            .unwrap_or_else(|| alloc.text(" {}"));

        attrs
            .append(modifier)
            .append(alloc.text("enum "))
            .append(alloc.text(name))
            .append(generics)
            .append(where_clause)
            .append(variants_doc)
    }
}

impl ToDoc for ast::VariantDefList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let variants: Vec<_> = self.into_iter().map(|v| v.to_doc(ctx)).collect();

        if variants.is_empty() {
            return alloc.text("{}");
        }

        let inner = alloc.concat(
            variants
                .into_iter()
                .map(|v| alloc.hardline().append(v).append(alloc.text(","))),
        );

        alloc
            .text("{")
            .append(inner.nest(ctx.config.indent_width as isize))
            .append(alloc.hardline())
            .append(alloc.text("}"))
    }
}

impl ToDoc for ast::VariantDef {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);

        let name = self
            .name()
            .map(|n| ctx.token(&n).to_string())
            .unwrap_or_default();

        let kind_doc = match self.kind() {
            ast::VariantKind::Unit => alloc.nil(),
            ast::VariantKind::Tuple(tuple_type) => tuple_type.to_doc(ctx),
            ast::VariantKind::Record(fields) => {
                // Format struct variant with max_width_group
                let field_docs: Vec<_> = fields.into_iter().map(|f| f.to_doc(ctx)).collect();

                if field_docs.is_empty() {
                    alloc.text(" {}")
                } else {
                    let indent = ctx.config.indent_width as isize;
                    let sep = alloc.text(",").append(alloc.line());
                    let inner = intersperse(alloc, field_docs, sep);
                    let trailing = alloc.text(",").flat_alt(alloc.nil());

                    // Use line() for spaced variant: renders as space when flat
                    let body = alloc
                        .text("{")
                        .append(alloc.line().append(inner).append(trailing).nest(indent))
                        .append(alloc.line())
                        .append(alloc.text("}"))
                        .max_width_group(ctx.config.struct_variant_width);

                    alloc.text(" ").append(body)
                }
            }
        };

        attrs.append(alloc.text(name)).append(kind_doc)
    }
}

impl ToDoc for ast::Trait {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let name = self
            .name()
            .map(|n| ctx.token(&n).to_string())
            .unwrap_or_default();
        let generics = generics_doc(self, ctx);

        let super_traits = self
            .super_trait_list()
            .map(|s| s.to_doc(ctx))
            .unwrap_or_else(|| alloc.nil());

        let where_clause = where_doc(self, ctx);

        let items_doc = self
            .item_list()
            .map(|items| alloc.text(" ").append(items.to_doc(ctx)))
            .unwrap_or_else(|| alloc.text(" {}"));

        attrs
            .append(modifier)
            .append(alloc.text("trait "))
            .append(alloc.text(name))
            .append(generics)
            .append(super_traits)
            .append(where_clause)
            .append(items_doc)
    }
}

impl ToDoc for ast::TraitItemList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        block_items_doc(self.syntax(), ast::TraitItem::cast, ctx)
    }
}

impl ToDoc for ast::TraitItem {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.kind() {
            TraitItemKind::Func(func) => func.to_doc(ctx),
            TraitItemKind::Type(ty) => ty.to_doc(ctx),
            TraitItemKind::Const(c) => c.to_doc(ctx),
        }
    }
}

impl ToDoc for ast::TraitTypeItem {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);

        let name = self
            .name()
            .map(|n| ctx.token(&n).to_string())
            .unwrap_or_default();

        let bounds_doc = self
            .bounds()
            .map(|b| b.to_doc(ctx))
            .unwrap_or_else(|| alloc.nil());

        let ty_doc = self
            .ty()
            .map(|ty| alloc.text(" = ").append(ty.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        attrs
            .append(alloc.text("type "))
            .append(alloc.text(name))
            .append(bounds_doc)
            .append(ty_doc)
    }
}

impl ToDoc for ast::TraitConstItem {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);

        let name = self
            .name()
            .map(|n| ctx.token(&n).to_string())
            .unwrap_or_default();

        let ty_doc = self
            .ty()
            .map(|ty| alloc.text(": ").append(ty.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        let value_doc = self
            .value()
            .map(|v| alloc.text(" = ").append(v.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        attrs
            .append(alloc.text("const "))
            .append(alloc.text(name))
            .append(ty_doc)
            .append(value_doc)
    }
}

impl ToDoc for ast::Impl {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);
        let generics = generics_doc(self, ctx);

        let ty_doc = self
            .ty()
            .map(|ty| alloc.text(" ").append(ty.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        let where_clause = where_doc(self, ctx);

        let items_doc = self
            .item_list()
            .map(|items| alloc.text(" ").append(items.to_doc(ctx)))
            .unwrap_or_else(|| alloc.text(" {}"));

        attrs
            .append(alloc.text("impl"))
            .append(generics)
            .append(ty_doc)
            .append(where_clause)
            .append(items_doc)
    }
}

impl ToDoc for ast::ImplItemList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        block_items_doc(self.syntax(), ast::Func::cast, ctx)
    }
}

impl ToDoc for ast::ImplTrait {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);
        let generics = generics_doc(self, ctx);

        let trait_doc = self
            .trait_ref()
            .map(|t| alloc.text(" ").append(t.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        let ty_doc = self
            .ty()
            .map(|ty| alloc.text(" for ").append(ty.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        let where_clause = where_doc(self, ctx);

        let items_doc = self
            .item_list()
            .map(|items| alloc.text(" ").append(items.to_doc(ctx)))
            .unwrap_or_else(|| alloc.text(" {}"));

        attrs
            .append(alloc.text("impl"))
            .append(generics)
            .append(trait_doc)
            .append(ty_doc)
            .append(where_clause)
            .append(items_doc)
    }
}

impl ToDoc for ast::Const {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let name = self
            .name()
            .map(|n| ctx.token(&n).to_string())
            .unwrap_or_default();

        let ty_doc = self
            .ty()
            .map(|ty| alloc.text(": ").append(ty.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        let value_doc = self
            .value()
            .map(|v| alloc.text(" = ").append(v.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        attrs
            .append(modifier)
            .append(alloc.text("const "))
            .append(alloc.text(name))
            .append(ty_doc)
            .append(value_doc)
    }
}

impl ToDoc for ast::Use {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let tree_doc = self
            .use_tree()
            .map(|t| t.to_doc(ctx))
            .unwrap_or_else(|| alloc.nil());

        attrs
            .append(modifier)
            .append(alloc.text("use "))
            .append(tree_doc)
    }
}

impl ToDoc for ast::UseTree {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;
        let mut doc = alloc.nil();

        if let Some(path) = self.path() {
            doc = doc.append(path.to_doc(ctx));
        }

        if let Some(children) = self.children() {
            if self.path().is_some() {
                doc = doc.append(alloc.text("::"));
            }
            doc = doc.append(children.to_doc(ctx));
        }

        if let Some(alias) = self.alias() {
            doc = doc.append(alias.to_doc(ctx));
        }

        doc
    }
}

impl ToDoc for ast::UseTreeList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;
        let trees: Vec<_> = self.into_iter().map(|t| t.to_doc(ctx)).collect();

        if trees.is_empty() {
            return alloc.text("{}");
        }

        let indent = ctx.config.indent_width as isize;
        let sep = alloc.text(",").append(alloc.line());
        let inner = intersperse(alloc, trees, sep);
        let trailing = alloc.text(",").flat_alt(alloc.nil());

        // Use line_() for no space when flat: {a, b} not { a, b }
        alloc
            .text("{")
            .append(alloc.line_().append(inner).append(trailing).nest(indent))
            .append(alloc.line_())
            .append(alloc.text("}"))
            .max_width_group(ctx.config.use_tree_width)
    }
}

impl ToDoc for ast::UsePath {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let segments: Vec<_> = self.into_iter().map(|seg| seg.to_doc(ctx)).collect();

        let sep = alloc.text("::");
        intersperse(alloc, segments, sep)
    }
}

impl ToDoc for ast::UsePathSegment {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        use parser::ast::UsePathSegmentKind;
        match self.kind() {
            Some(UsePathSegmentKind::Ingot(token)) => alloc.text(ctx.token(&token).to_string()),
            Some(UsePathSegmentKind::Super(token)) => alloc.text(ctx.token(&token).to_string()),
            Some(UsePathSegmentKind::Self_(token)) => alloc.text(ctx.token(&token).to_string()),
            Some(UsePathSegmentKind::Ident(token)) => alloc.text(ctx.token(&token).to_string()),
            Some(UsePathSegmentKind::Glob(token)) => alloc.text(ctx.token(&token).to_string()),
            None => alloc.nil(),
        }
    }
}

impl ToDoc for ast::UseAlias {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if let Some(alias) = self.alias() {
            let alias_text = ctx.snippet(alias.text_range()).trim().to_string();
            alloc.text(" as ").append(alloc.text(alias_text))
        } else {
            alloc.nil()
        }
    }
}

impl ToDoc for ast::TypeAlias {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let alias = self
            .alias()
            .map(|a| ctx.token(&a).to_string())
            .unwrap_or_default();
        let generics = generics_doc(self, ctx);

        let ty_doc = self
            .ty()
            .map(|ty| alloc.text(" = ").append(ty.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        attrs
            .append(modifier)
            .append(alloc.text("type "))
            .append(alloc.text(alias))
            .append(generics)
            .append(ty_doc)
    }
}

impl ToDoc for ast::Mod {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let name = self
            .name()
            .map(|n| ctx.token(&n).to_string())
            .unwrap_or_default();

        let items_doc = self
            .items()
            .map(|items| alloc.text(" ").append(block_items_doc(items.syntax(), ast::Item::cast, ctx)))
            .unwrap_or_else(|| alloc.nil());

        attrs
            .append(modifier)
            .append(alloc.text("mod "))
            .append(alloc.text(name))
            .append(items_doc)
    }
}

impl ToDoc for ast::Extern {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let attrs = attrs_doc(self, ctx);

        let items_doc = self
            .extern_block()
            .map(|items| alloc.text(" ").append(items.to_doc(ctx)))
            .unwrap_or_else(|| alloc.text(" {}"));

        attrs.append(alloc.text("extern")).append(items_doc)
    }
}

impl ToDoc for ast::ExternItemList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        block_items_doc(self.syntax(), ast::Func::cast, ctx)
    }
}
