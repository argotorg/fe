//! Formatting for top-level items (functions, structs, enums, traits, etc.)

use pretty::RcDoc;

use crate::{RewriteContext, Shape};
use parser::ast::{self, ItemKind, ItemModifierOwner, TraitItemKind, prelude::AstNode};

use super::types::{ToDoc, block_list, block_list_spaced, intersperse};

/// Helper to build attributes document for a node.
fn attrs_doc<'a, N: ast::AttrListOwner + AstNode>(
    node: &N,
    context: &RewriteContext<'_>,
) -> RcDoc<'a, ()> {
    if let Some(attrs) = node.attr_list() {
        RcDoc::text(
            context
                .snippet(attrs.syntax().text_range())
                .trim_end()
                .to_string(),
        )
        .append(RcDoc::hardline())
    } else {
        RcDoc::nil()
    }
}

/// Helper to build item modifier document (pub, unsafe).
fn modifier_doc<'a, N: ItemModifierOwner + AstNode>(node: &N) -> RcDoc<'a, ()> {
    let mut doc = RcDoc::nil();
    if let Some(modifier) = node.modifier() {
        if modifier.pub_kw().is_some() {
            doc = doc.append(RcDoc::text("pub "));
        }
        if modifier.unsafe_kw().is_some() {
            doc = doc.append(RcDoc::text("unsafe "));
        }
    }
    doc
}

/// Helper to build generics document.
fn generics_doc<'a, N: ast::GenericParamsOwner + AstNode>(
    node: &N,
    context: &RewriteContext<'_>,
    shape: Shape,
) -> RcDoc<'a, ()> {
    if let Some(generics) = node.generic_params() {
        generics.to_doc(context, shape)
    } else {
        RcDoc::nil()
    }
}

/// Helper to build where clause document.
fn where_doc<'a, N: ast::WhereClauseOwner + AstNode>(
    node: &N,
    context: &RewriteContext<'_>,
    shape: Shape,
) -> RcDoc<'a, ()> {
    if let Some(where_clause) = node.where_clause() {
        let preds: Vec<_> = where_clause
            .into_iter()
            .map(|pred| pred.to_doc(context, shape))
            .collect();

        if preds.is_empty() {
            return RcDoc::nil();
        }

        let sep = RcDoc::text(",").append(RcDoc::line());
        let preds_doc = intersperse(preds, sep).group();

        let where_block = RcDoc::text("where")
            .append(
                RcDoc::line()
                    .append(preds_doc)
                    .nest(context.config.clause_indent as isize),
            )
            .group();

        if context.config.where_new_line {
            RcDoc::hardline().append(where_block)
        } else {
            RcDoc::line().append(where_block).group()
        }
    } else {
        RcDoc::nil()
    }
}

/// Format a block of items with proper indentation.
fn block_items_doc<'a, T: ToDoc>(
    items: impl IntoIterator<Item = T>,
    context: &RewriteContext<'_>,
    shape: Shape,
) -> RcDoc<'a, ()> {
    let items: Vec<_> = items.into_iter().collect();

    if items.is_empty() {
        return RcDoc::text("{}");
    }

    let inner = RcDoc::concat(
        items
            .into_iter()
            .map(|item| RcDoc::hardline().append(item.to_doc(context, shape))),
    );

    RcDoc::text("{")
        .append(inner.nest(context.config.indent_width as isize))
        .append(RcDoc::hardline())
        .append(RcDoc::text("}"))
}

impl ToDoc for ast::Root {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        use parser::syntax_kind::SyntaxKind;
        use parser::syntax_node::NodeOrToken;

        let mut result = RcDoc::nil();
        let mut pending_newlines = 0usize;
        let mut is_first = true;

        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    if !is_first {
                        let newlines_to_add = pending_newlines.max(1);
                        for _ in 0..newlines_to_add {
                            result = result.append(RcDoc::hardline());
                        }
                    }
                    pending_newlines = 0;
                    is_first = false;

                    if let Some(item_list) = ast::ItemList::cast(node.clone()) {
                        result = result.append(item_list.to_doc(context, shape));
                    } else {
                        result = result
                            .append(RcDoc::text(context.snippet(node.text_range()).to_string()));
                    }
                }
                NodeOrToken::Token(token) => {
                    match token.kind() {
                        SyntaxKind::Newline => {
                            let text = context.snippet(token.text_range());
                            pending_newlines = text.chars().filter(|c| *c == '\n').count();
                        }
                        SyntaxKind::WhiteSpace => {
                            // Skip stray whitespace between items/comments
                            continue;
                        }
                        _ => {
                            if !is_first {
                                let newlines_to_add = pending_newlines.max(1);
                                for _ in 0..newlines_to_add {
                                    result = result.append(RcDoc::hardline());
                                }
                            }
                            pending_newlines = 0;
                            is_first = false;

                            let mut text = context.snippet(token.text_range()).to_string();
                            if token.kind() == SyntaxKind::Comment
                                || token.kind() == SyntaxKind::DocComment
                            {
                                text = text.trim_end().to_string();
                            }
                            result = result.append(RcDoc::text(text));
                        }
                    }
                }
            }
        }
        result
    }
}

impl ToDoc for ast::ItemList {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        use parser::syntax_kind::SyntaxKind;
        use parser::syntax_node::NodeOrToken;

        let mut result = RcDoc::nil();
        let mut pending_newlines = 0usize;
        let mut is_first = true;

        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    if let Some(item) = ast::Item::cast(node.clone()) {
                        // Add newlines that were accumulated from whitespace
                        if !is_first {
                            // At least one newline between items
                            let newlines_to_add = pending_newlines.max(1);
                            for _ in 0..newlines_to_add {
                                result = result.append(RcDoc::hardline());
                            }
                        }
                        pending_newlines = 0;
                        is_first = false;
                        result = result.append(item.to_doc(context, shape));
                    } else {
                        result = result
                            .append(RcDoc::text(context.snippet(node.text_range()).to_string()));
                    }
                }
                NodeOrToken::Token(token) => {
                    if token.kind() == SyntaxKind::Newline {
                        let text = context.snippet(token.text_range());
                        pending_newlines = text.chars().filter(|c| *c == '\n').count();
                    } else if token.kind() == SyntaxKind::Comment {
                        // Add newlines that were accumulated from whitespace
                        if !is_first {
                            let newlines_to_add = pending_newlines.max(1);
                            for _ in 0..newlines_to_add {
                                result = result.append(RcDoc::hardline());
                            }
                        }
                        pending_newlines = 0;
                        is_first = false;
                        result = result.append(RcDoc::text(
                            context.snippet(token.text_range()).trim().to_string(),
                        ));
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
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.kind() {
            Some(ItemKind::Mod(mod_)) => mod_.to_doc(context, shape),
            Some(ItemKind::Func(func)) => func.to_doc(context, shape),
            Some(ItemKind::Struct(struct_)) => struct_.to_doc(context, shape),
            Some(ItemKind::Contract(contract)) => contract.to_doc(context, shape),
            Some(ItemKind::Enum(enum_)) => enum_.to_doc(context, shape),
            Some(ItemKind::TypeAlias(type_alias)) => type_alias.to_doc(context, shape),
            Some(ItemKind::Impl(impl_)) => impl_.to_doc(context, shape),
            Some(ItemKind::Trait(trait_)) => trait_.to_doc(context, shape),
            Some(ItemKind::ImplTrait(impl_trait)) => impl_trait.to_doc(context, shape),
            Some(ItemKind::Const(const_)) => const_.to_doc(context, shape),
            Some(ItemKind::Use(use_)) => use_.to_doc(context, shape),
            Some(ItemKind::Extern(extern_)) => extern_.to_doc(context, shape),
            None => RcDoc::nil(),
        }
    }
}

impl ToDoc for ast::FuncSignature {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let name = match self.name() {
            Some(n) => context.token(&n).to_string(),
            None => return RcDoc::text("fn"),
        };

        let generics = generics_doc(self, context, shape);

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

        let ret_doc = self
            .ret_ty()
            .map(|ty| RcDoc::text(" -> ").append(ty.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        let uses_doc = self
            .uses_clause()
            .map(|u| RcDoc::text(" ").append(u.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        let where_clause = where_doc(self, context, shape);

        RcDoc::text("fn ")
            .append(RcDoc::text(name))
            .append(generics)
            .append(params_doc)
            .append(ret_doc)
            .append(uses_doc)
            .append(where_clause)
            .group()
    }
}

impl ToDoc for ast::Func {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);
        let modifier = modifier_doc(self);
        let sig = self.sig().to_doc(context, shape);

        let doc = if let Some(body) = self.body() {
            // Decide whether the body can stay on the same line as the signature.
            // Render the signature at the current max width to see if it breaks.
            let (sig_len, sig_multiline) = {
                let mut buf = Vec::new();
                let _ = sig.clone().render(context.config.max_width, &mut buf);
                let s = String::from_utf8(buf).unwrap_or_default();
                let mut lines = s.lines();
                let first_len = lines.next().map(|l| l.len()).unwrap_or(0);
                let multiline = lines.next().is_some();
                (first_len, multiline)
            };

            let has_where = self.sig().where_clause().is_some();
            let body_sep = if context.config.where_new_line
                || has_where
                    && (sig_multiline || sig_len + 2 /* space + '{' */ > context.config.max_width)
            {
                RcDoc::hardline()
            } else {
                RcDoc::text(" ")
            };

            sig.append(body_sep).append(body.to_doc(context, shape))
        } else {
            sig
        };

        attrs.append(modifier).append(doc)
    }
}

impl ToDoc for ast::FuncParamList {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let params: Vec<_> = self.into_iter().map(|p| p.to_doc(context, shape)).collect();

        let indent = context.config.indent_width as isize;
        block_list("(", ")", params, indent, true)
    }
}

impl ToDoc for ast::FuncParam {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let mut doc = RcDoc::nil();

        if self.mut_token().is_some() {
            doc = doc.append(RcDoc::text("mut "));
        }

        let label = self.label();
        let name = self.name();

        if let (Some(label), Some(name_ref)) = (&label, &name)
            && label.syntax().text_range() != name_ref.syntax().text_range()
        {
            doc = doc.append(RcDoc::text(
                context
                    .snippet(label.syntax().text_range())
                    .trim()
                    .to_string(),
            ));
            doc = doc.append(RcDoc::text(" "));
        }

        if let Some(name) = name {
            doc = doc.append(RcDoc::text(
                context
                    .snippet(name.syntax().text_range())
                    .trim()
                    .to_string(),
            ));
        }

        if let Some(ty) = self.ty() {
            doc = doc
                .append(RcDoc::text(": "))
                .append(ty.to_doc(context, shape));
        }

        doc
    }
}

impl ToDoc for ast::Struct {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);
        let modifier = modifier_doc(self);

        let name = self
            .name()
            .map(|n| context.token(&n).to_string())
            .unwrap_or_default();
        let generics = generics_doc(self, context, shape);
        let where_clause = where_doc(self, context, shape);

        let fields_doc = self
            .fields()
            .map(|f| RcDoc::text(" ").append(f.to_doc(context, shape)))
            .unwrap_or_else(|| RcDoc::text(" {}"));

        attrs
            .append(modifier)
            .append(RcDoc::text("struct "))
            .append(RcDoc::text(name))
            .append(generics)
            .append(where_clause)
            .append(fields_doc)
    }
}

impl ToDoc for ast::RecordFieldDefList {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let fields: Vec<_> = self.into_iter().map(|f| f.to_doc(context, shape)).collect();

        let indent = context.config.indent_width as isize;
        block_list_spaced("{", "}", fields, indent, true)
    }
}

impl ToDoc for ast::RecordFieldDef {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);

        let mut doc = attrs;

        if self.pub_kw().is_some() {
            doc = doc.append(RcDoc::text("pub "));
        }

        if let Some(name) = self.name() {
            doc = doc.append(RcDoc::text(context.token(&name).to_string()));
        }

        if let Some(ty) = self.ty() {
            doc = doc
                .append(RcDoc::text(": "))
                .append(ty.to_doc(context, shape));
        }

        doc
    }
}

impl ToDoc for ast::Contract {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);
        let modifier = modifier_doc(self);

        let name = self
            .name()
            .map(|n| context.token(&n).to_string())
            .unwrap_or_default();

        let fields_doc = self
            .fields()
            .map(|f| RcDoc::text(" ").append(f.to_doc(context, shape)))
            .unwrap_or_else(|| RcDoc::text(" {}"));

        attrs
            .append(modifier)
            .append(RcDoc::text("contract "))
            .append(RcDoc::text(name))
            .append(fields_doc)
    }
}

impl ToDoc for ast::Enum {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);
        let modifier = modifier_doc(self);

        let name = self
            .name()
            .map(|n| context.token(&n).to_string())
            .unwrap_or_default();
        let generics = generics_doc(self, context, shape);
        let where_clause = where_doc(self, context, shape);

        let variants_doc = self
            .variants()
            .map(|v| RcDoc::text(" ").append(v.to_doc(context, shape)))
            .unwrap_or_else(|| RcDoc::text(" {}"));

        attrs
            .append(modifier)
            .append(RcDoc::text("enum "))
            .append(RcDoc::text(name))
            .append(generics)
            .append(where_clause)
            .append(variants_doc)
    }
}

impl ToDoc for ast::VariantDefList {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let variants: Vec<_> = self.into_iter().map(|v| v.to_doc(context, shape)).collect();

        if variants.is_empty() {
            return RcDoc::text("{}");
        }

        let inner = RcDoc::concat(
            variants
                .into_iter()
                .map(|v| RcDoc::hardline().append(v).append(RcDoc::text(","))),
        );

        RcDoc::text("{")
            .append(inner.nest(context.config.indent_width as isize))
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
    }
}

impl ToDoc for ast::VariantDef {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);

        let name = self
            .name()
            .map(|n| context.token(&n).to_string())
            .unwrap_or_default();

        let kind_doc = match self.kind() {
            ast::VariantKind::Unit => RcDoc::nil(),
            ast::VariantKind::Tuple(tuple_type) => tuple_type.to_doc(context, shape),
            ast::VariantKind::Record(fields) => {
                RcDoc::text(" ").append(fields.to_doc(context, shape))
            }
        };

        attrs.append(RcDoc::text(name)).append(kind_doc)
    }
}

impl ToDoc for ast::Trait {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);
        let modifier = modifier_doc(self);

        let name = self
            .name()
            .map(|n| context.token(&n).to_string())
            .unwrap_or_default();
        let generics = generics_doc(self, context, shape);

        let super_traits = self
            .super_trait_list()
            .map(|s| s.to_doc(context, shape))
            .unwrap_or_else(RcDoc::nil);

        let where_clause = where_doc(self, context, shape);

        let items_doc = self
            .item_list()
            .map(|items| RcDoc::text(" ").append(items.to_doc(context, shape)))
            .unwrap_or_else(|| RcDoc::text(" {}"));

        attrs
            .append(modifier)
            .append(RcDoc::text("trait "))
            .append(RcDoc::text(name))
            .append(generics)
            .append(super_traits)
            .append(where_clause)
            .append(items_doc)
    }
}

impl ToDoc for ast::TraitItemList {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        block_items_doc(self, context, shape)
    }
}

impl ToDoc for ast::TraitItem {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.kind() {
            TraitItemKind::Func(func) => func.to_doc(context, shape),
            TraitItemKind::Type(ty) => ty.to_doc(context, shape),
            TraitItemKind::Const(c) => c.to_doc(context, shape),
        }
    }
}

impl ToDoc for ast::TraitTypeItem {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);

        let name = self
            .name()
            .map(|n| context.token(&n).to_string())
            .unwrap_or_default();

        let bounds_doc = self
            .bounds()
            .map(|b| b.to_doc(context, shape))
            .unwrap_or_else(RcDoc::nil);

        let ty_doc = self
            .ty()
            .map(|ty| RcDoc::text(" = ").append(ty.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        attrs
            .append(RcDoc::text("type "))
            .append(RcDoc::text(name))
            .append(bounds_doc)
            .append(ty_doc)
    }
}

impl ToDoc for ast::TraitConstItem {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);

        let name = self
            .name()
            .map(|n| context.token(&n).to_string())
            .unwrap_or_default();

        let ty_doc = self
            .ty()
            .map(|ty| RcDoc::text(": ").append(ty.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        let value_doc = self
            .value()
            .map(|v| RcDoc::text(" = ").append(v.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        attrs
            .append(RcDoc::text("const "))
            .append(RcDoc::text(name))
            .append(ty_doc)
            .append(value_doc)
    }
}

impl ToDoc for ast::Impl {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);
        let generics = generics_doc(self, context, shape);

        let ty_doc = self
            .ty()
            .map(|ty| RcDoc::text(" ").append(ty.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        let where_clause = where_doc(self, context, shape);

        let items_doc = self
            .item_list()
            .map(|items| RcDoc::text(" ").append(items.to_doc(context, shape)))
            .unwrap_or_else(|| RcDoc::text(" {}"));

        attrs
            .append(RcDoc::text("impl"))
            .append(generics)
            .append(ty_doc)
            .append(where_clause)
            .append(items_doc)
    }
}

impl ToDoc for ast::ImplItemList {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        block_items_doc(self, context, shape)
    }
}

impl ToDoc for ast::ImplTrait {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);
        let generics = generics_doc(self, context, shape);

        let trait_doc = self
            .trait_ref()
            .map(|t| RcDoc::text(" ").append(t.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        let ty_doc = self
            .ty()
            .map(|ty| RcDoc::text(" for ").append(ty.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        let where_clause = where_doc(self, context, shape);

        let items_doc = self
            .item_list()
            .map(|items| RcDoc::text(" ").append(items.to_doc(context, shape)))
            .unwrap_or_else(|| RcDoc::text(" {}"));

        attrs
            .append(RcDoc::text("impl"))
            .append(generics)
            .append(trait_doc)
            .append(ty_doc)
            .append(where_clause)
            .append(items_doc)
    }
}

impl ToDoc for ast::Const {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);
        let modifier = modifier_doc(self);

        let name = self
            .name()
            .map(|n| context.token(&n).to_string())
            .unwrap_or_default();

        let ty_doc = self
            .ty()
            .map(|ty| RcDoc::text(": ").append(ty.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        let value_doc = self
            .value()
            .map(|v| RcDoc::text(" = ").append(v.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        attrs
            .append(modifier)
            .append(RcDoc::text("const "))
            .append(RcDoc::text(name))
            .append(ty_doc)
            .append(value_doc)
    }
}

impl ToDoc for ast::Use {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);
        let modifier = modifier_doc(self);

        let tree_doc = self
            .use_tree()
            .map(|t| t.to_doc(context, shape))
            .unwrap_or_else(RcDoc::nil);

        attrs
            .append(modifier)
            .append(RcDoc::text("use "))
            .append(tree_doc)
    }
}

impl ToDoc for ast::UseTree {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let mut doc = RcDoc::nil();

        if let Some(path) = self.path() {
            doc = doc.append(path.to_doc(context, shape));
        }

        if let Some(children) = self.children() {
            if self.path().is_some() {
                doc = doc.append(RcDoc::text("::"));
            }
            doc = doc.append(children.to_doc(context, shape));
        }

        if let Some(alias) = self.alias() {
            doc = doc.append(alias.to_doc(context, shape));
        }

        doc
    }
}

impl ToDoc for ast::UseTreeList {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let trees: Vec<_> = self.into_iter().map(|t| t.to_doc(context, shape)).collect();

        let indent = context.config.indent_width as isize;
        block_list("{", "}", trees, indent, true)
    }
}

impl ToDoc for ast::UsePath {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let segments: Vec<_> = self
            .into_iter()
            .map(|seg| seg.to_doc(context, shape))
            .collect();

        let sep = RcDoc::text("::");
        intersperse(segments, sep)
    }
}

impl ToDoc for ast::UsePathSegment {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, _shape: Shape) -> RcDoc<'a, ()> {
        use parser::ast::UsePathSegmentKind;
        match self.kind() {
            Some(UsePathSegmentKind::Ingot(token)) => {
                RcDoc::text(context.token(&token).to_string())
            }
            Some(UsePathSegmentKind::Super(token)) => {
                RcDoc::text(context.token(&token).to_string())
            }
            Some(UsePathSegmentKind::Self_(token)) => {
                RcDoc::text(context.token(&token).to_string())
            }
            Some(UsePathSegmentKind::Ident(token)) => {
                RcDoc::text(context.token(&token).to_string())
            }
            Some(UsePathSegmentKind::Glob(token)) => RcDoc::text(context.token(&token).to_string()),
            None => RcDoc::nil(),
        }
    }
}

impl ToDoc for ast::UseAlias {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, _shape: Shape) -> RcDoc<'a, ()> {
        if let Some(alias) = self.alias() {
            let alias_text = context.snippet(alias.text_range()).trim().to_string();
            RcDoc::text(" as ").append(RcDoc::text(alias_text))
        } else {
            RcDoc::nil()
        }
    }
}

impl ToDoc for ast::TypeAlias {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);
        let modifier = modifier_doc(self);

        let alias = self
            .alias()
            .map(|a| context.token(&a).to_string())
            .unwrap_or_default();
        let generics = generics_doc(self, context, shape);

        let ty_doc = self
            .ty()
            .map(|ty| RcDoc::text(" = ").append(ty.to_doc(context, shape)))
            .unwrap_or_else(RcDoc::nil);

        attrs
            .append(modifier)
            .append(RcDoc::text("type "))
            .append(RcDoc::text(alias))
            .append(generics)
            .append(ty_doc)
    }
}

impl ToDoc for ast::Mod {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);
        let modifier = modifier_doc(self);

        let name = self
            .name()
            .map(|n| context.token(&n).to_string())
            .unwrap_or_default();

        let items_doc = self
            .items()
            .map(|items| RcDoc::text(" ").append(block_items_doc(items, context, shape)))
            .unwrap_or_else(RcDoc::nil);

        attrs
            .append(modifier)
            .append(RcDoc::text("mod "))
            .append(RcDoc::text(name))
            .append(items_doc)
    }
}

impl ToDoc for ast::Extern {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let attrs = attrs_doc(self, context);

        let items_doc = self
            .extern_block()
            .map(|items| RcDoc::text(" ").append(items.to_doc(context, shape)))
            .unwrap_or_else(|| RcDoc::text(" {}"));

        attrs.append(RcDoc::text("extern")).append(items_doc)
    }
}

impl ToDoc for ast::ExternItemList {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        block_items_doc(self, context, shape)
    }
}
