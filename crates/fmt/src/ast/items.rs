//! Formatting for top-level items (functions, structs, enums, traits, etc.)

use pretty::DocAllocator;

use crate::RewriteContext;
use parser::ast::{self, ItemKind, ItemModifierOwner, TraitItemKind, prelude::AstNode};

use super::types::{
    Doc, ToDoc, TokenPiece, block_list_auto, block_list_spaced_auto, block_list_with_comments,
    hardlines, has_comment_tokens, intersperse, newline_count, token_doc, token_doc_until_token,
};

macro_rules! token_doc_item_like_if_comments {
    ($node:expr, $ctx:expr) => {
        if has_comment_tokens($node.syntax()) {
            return token_doc_item_like($ctx, $node.syntax());
        }
    };
}

fn token_piece_basic<'a>(
    ctx: &'a RewriteContext<'a>,
    token: parser::SyntaxToken,
) -> Option<TokenPiece<'a>> {
    use parser::syntax_kind::SyntaxKind::*;

    let alloc = &ctx.alloc;
    let text = alloc.text(token.text().to_string());
    Some(match token.kind() {
        FnKw => TokenPiece::new(alloc.nil()),
        PubKw | UnsafeKw | MutKw | StructKw | ContractKw | EnumKw | TraitKw | MsgKw | ModKw
        | UseKw | ConstKw | TypeKw | ExternKw => TokenPiece::new(text).space_after(),
        ImplKw => TokenPiece::new(text),
        ForKw => TokenPiece::new(text).space_before(),
        Eq | Arrow => TokenPiece::new(text).spaces(),
        _ => return None,
    })
}

fn token_doc_item_like<'a>(ctx: &'a RewriteContext<'a>, syntax: &parser::SyntaxNode) -> Doc<'a> {
    token_doc(
        ctx,
        syntax,
        0,
        |node| token_doc_item_node_piece(ctx, node),
        |token| token_piece_basic(ctx, token),
    )
}

fn token_doc_item_node_piece<'a>(
    ctx: &'a RewriteContext<'a>,
    node: parser::SyntaxNode,
) -> Option<TokenPiece<'a>> {
    macro_rules! piece {
        ($ty:ty) => {
            <$ty>::cast(node.clone()).map(|n| TokenPiece::new(n.to_doc(ctx)))
        };
        ($ty:ty, $m:ident) => {
            <$ty>::cast(node.clone()).map(|n| TokenPiece::new(n.to_doc(ctx)).$m())
        };
    }
    macro_rules! first_some {
        ($($expr:expr),+ $(,)?) => {
            None$(.or_else(|| $expr))+
        };
    }

    first_some!(
        piece!(ast::AttrList),
        piece!(ast::FuncSignature),
        piece!(ast::FuncParamList),
        piece!(ast::GenericParamList),
        piece!(ast::TypeBoundList),
        piece!(ast::SuperTraitList),
        piece!(ast::WhereClause, space_before),
        piece!(ast::UsesClause, space_before),
        piece!(ast::Path, space_before),
        piece!(ast::Pat),
        piece!(ast::TraitRef, space_before),
        piece!(ast::Type, space_before),
        piece!(ast::Expr),
        piece!(ast::TupleType),
        piece!(ast::RecordFieldDefList, space_before),
        piece!(ast::VariantDefList, space_before),
        piece!(ast::TraitItemList, space_before),
        piece!(ast::ImplItemList, space_before),
        piece!(ast::ExternItemList, space_before),
        piece!(ast::RecvArmList, space_before),
        piece!(ast::MsgVariantList, space_before),
        piece!(ast::MsgVariantParams, space_before),
        piece!(ast::BlockExpr, space_before),
        ast::ItemList::cast(node.clone()).map(|items| {
            TokenPiece::new(block_items_doc(items.syntax(), ast::Item::cast, ctx)).space_before()
        }),
        piece!(ast::UseTree, space_before),
        piece!(ast::UsePath),
        piece!(ast::UsePathSegment),
        piece!(ast::UseTreeList),
        piece!(ast::UseAlias),
    )
}

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
    if node.pub_kw().is_some() {
        doc = doc.append(alloc.text("pub "));
    }
    if node.unsafe_kw().is_some() {
        doc = doc.append(alloc.text("unsafe "));
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
        if where_clause.iter().next().is_none() && !has_comment_tokens(where_clause.syntax()) {
            return alloc.nil();
        }

        let where_block = where_clause.to_doc(ctx);

        if ctx.config.where_new_line {
            alloc.hardline().append(where_block)
        } else {
            alloc.line().append(where_block).group()
        }
    } else {
        alloc.nil()
    }
}

/// Format a block of items `{ ... }`, preserving whether there was a blank line
/// between entries in the source (2+ newlines => one blank line; otherwise none).
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
        let entry_doc = match child {
            NodeOrToken::Node(node) => {
                let Some(item) = cast_fn(node) else {
                    continue;
                };
                Some(item.to_doc(ctx))
            }
            NodeOrToken::Token(token) => match token.kind() {
                SyntaxKind::Newline => {
                    pending_newlines += newline_count(ctx.snippet(token.text_range()));
                    None
                }
                SyntaxKind::WhiteSpace => None,
                SyntaxKind::Comment | SyntaxKind::DocComment => {
                    Some(alloc.text(ctx.snippet(token.text_range()).trim_end()))
                }
                _ => None,
            },
        };

        let Some(entry_doc) = entry_doc else {
            continue;
        };

        if is_first {
            inner = inner.append(alloc.hardline());
            is_first = false;
        } else {
            // Preserve whether there was a blank line (2+ newlines) between entries.
            inner = inner.append(hardlines(alloc, pending_newlines));
        }

        pending_newlines = 0;
        inner = inner.append(entry_doc);
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
                        result = result.append(hardlines(alloc, pending_newlines));
                    }
                    pending_newlines = 0;
                    is_first = false;

                    if let Some(item_list) = ast::ItemList::cast(node.clone()) {
                        result = result.append(item_list.to_doc(ctx));
                    } else {
                        result = result.append(alloc.text(ctx.snippet(node.text_range())));
                    }
                }
                NodeOrToken::Token(token) => {
                    match token.kind() {
                        SyntaxKind::Newline => {
                            pending_newlines = newline_count(ctx.snippet(token.text_range()));
                        }
                        SyntaxKind::WhiteSpace => {
                            // Skip stray whitespace between items/comments
                            continue;
                        }
                        _ => {
                            if !is_first {
                                // Collapse multiple blank lines to one
                                result = result.append(hardlines(alloc, pending_newlines));
                            }
                            pending_newlines = 0;
                            is_first = false;

                            let text = ctx.snippet(token.text_range());
                            let text = if matches!(
                                token.kind(),
                                SyntaxKind::Comment | SyntaxKind::DocComment
                            ) {
                                text.trim_end()
                            } else {
                                text
                            };
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
                            result = result.append(hardlines(alloc, pending_newlines));
                        }
                        pending_newlines = 0;
                        is_first = false;
                        result = result.append(item.to_doc(ctx));
                    } else {
                        result = result.append(alloc.text(ctx.snippet(node.text_range())));
                    }
                }
                NodeOrToken::Token(token) => {
                    if token.kind() == SyntaxKind::Newline {
                        pending_newlines = newline_count(ctx.snippet(token.text_range()));
                    } else if token.kind() == SyntaxKind::Comment {
                        // Add newlines that were accumulated from whitespace
                        if !is_first {
                            // Collapse multiple blank lines to one
                            result = result.append(hardlines(alloc, pending_newlines));
                        }
                        pending_newlines = 0;
                        is_first = false;
                        result = result.append(alloc.text(ctx.token(&token)));
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
            Some(ItemKind::Msg(msg)) => msg.to_doc(ctx),
            None => ctx.alloc.nil(),
        }
    }
}

impl ToDoc for ast::FuncSignature {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        func_sig_to_doc(self, ctx, false)
    }
}

/// Format a function signature.
/// If `include_body_sep` is true, includes a trailing separator that becomes
/// a space when flat and a newline when broken - used when the signature has
/// uses/where clauses so the opening brace moves to a new line with them.
fn func_sig_to_doc<'a>(
    sig: &ast::FuncSignature,
    ctx: &'a RewriteContext<'a>,
    include_body_sep: bool,
) -> Doc<'a> {
    use parser::syntax_kind::SyntaxKind;
    use parser::syntax_node::NodeOrToken;

    let alloc = &ctx.alloc;

    let name = match sig.name() {
        Some(n) => alloc.text(ctx.token(&n)),
        None => return alloc.text("fn"),
    };

    let generics = generics_doc(sig, ctx);

    let params_doc = sig
        .params()
        .map(|params| params.to_doc(ctx))
        .unwrap_or_else(|| alloc.text("()"));

    let ret_doc = sig
        .ret_ty()
        .map(|ty| alloc.text(" -> ").append(ty.to_doc(ctx)))
        .unwrap_or_else(|| alloc.nil());

    let has_uses = sig.uses_clause().is_some();
    let has_where = sig.where_clause().is_some();

    // Build the core signature (before uses/where) to measure its length
    let core_sig = alloc
        .text("fn ")
        .append(name.clone())
        .append(generics.clone())
        .append(params_doc.clone())
        .append(ret_doc.clone());

    // Measure the flat length of the core signature
    let core_flat_len = {
        let mut buf = Vec::new();
        let _ = core_sig.clone().into_doc().render(10000, &mut buf);
        let s = String::from_utf8(buf).unwrap_or_default();
        s.lines().next().map(|l| l.len()).unwrap_or(0)
    };

    // Force clauses to new lines if:
    // - uses_new_line/where_new_line config is set, OR
    // - the core signature is already long (> 60 chars)
    let force_uses_break = has_uses && (ctx.config.uses_new_line || core_flat_len > 60);
    let force_where_break = has_where && (ctx.config.where_new_line || core_flat_len > 60);
    let force_clause_break = force_uses_break || force_where_break;

    let uses_doc = sig
        .uses_clause()
        .map(|u| {
            if force_uses_break {
                alloc.hardline().append(u.to_doc(ctx))
            } else {
                alloc.line().append(u.to_doc(ctx))
            }
        })
        .unwrap_or_else(|| alloc.nil());

    let where_clause = if force_where_break {
        where_doc_forced(sig, ctx)
    } else {
        where_doc(sig, ctx)
    };

    // Body separator: space when flat, newline when broken
    let body_sep = if include_body_sep {
        if force_clause_break {
            alloc.hardline()
        } else {
            alloc.line()
        }
    } else {
        alloc.nil()
    };

    if !has_comment_tokens(sig.syntax()) {
        return alloc
            .text("fn ")
            .append(name)
            .append(generics)
            .append(params_doc)
            .append(ret_doc)
            .append(uses_doc)
            .append(where_clause)
            .append(body_sep)
            .max_width_group(ctx.config.fn_sig_width);
    }

    let indent = ctx.config.clause_indent as isize;
    let mut inner = name.clone();
    let mut pending_newlines = 0usize;
    let mut needs_space = false;

    for child in sig.syntax().children_with_tokens() {
        match child {
            NodeOrToken::Node(node) => {
                if let Some(uses) = ast::UsesClause::cast(node.clone()) {
                    if pending_newlines > 0 {
                        inner = inner
                            .append(hardlines(alloc, pending_newlines))
                            .append(uses.to_doc(ctx));
                        pending_newlines = 0;
                    } else {
                        inner = inner.append(uses_doc.clone());
                    }
                    needs_space = false;
                    continue;
                }
                if let Some(where_clause_node) = ast::WhereClause::cast(node.clone()) {
                    if pending_newlines > 0 {
                        inner = inner
                            .append(hardlines(alloc, pending_newlines))
                            .append(where_clause_node.to_doc(ctx));
                        pending_newlines = 0;
                    } else {
                        inner = inner.append(where_clause.clone());
                    }
                    needs_space = false;
                    continue;
                }

                let elem = if let Some(generics) = ast::GenericParamList::cast(node.clone()) {
                    generics.to_doc(ctx)
                } else if let Some(params) = ast::FuncParamList::cast(node.clone()) {
                    params.to_doc(ctx)
                } else if let Some(ty) = ast::Type::cast(node) {
                    ty.to_doc(ctx)
                } else {
                    continue;
                };

                if pending_newlines > 0 {
                    inner =
                        inner.append(hardlines(alloc, pending_newlines).append(elem).nest(indent));
                    pending_newlines = 0;
                } else {
                    if needs_space {
                        inner = inner.append(alloc.text(" "));
                    }
                    inner = inner.append(elem);
                }
                needs_space = false;
            }
            NodeOrToken::Token(token) => match token.kind() {
                SyntaxKind::Ident => {}
                SyntaxKind::Arrow => {
                    if pending_newlines > 0 {
                        inner = inner.append(
                            hardlines(alloc, pending_newlines)
                                .append(alloc.text("->"))
                                .nest(indent),
                        );
                        pending_newlines = 0;
                    } else {
                        inner = inner.append(alloc.text(" ->"));
                    }
                    needs_space = true;
                }
                SyntaxKind::Newline => {
                    pending_newlines += newline_count(ctx.snippet(token.text_range()));
                }
                SyntaxKind::WhiteSpace => {}
                SyntaxKind::Comment | SyntaxKind::DocComment => {
                    let comment = ctx.snippet(token.text_range()).trim_end();
                    let is_line_comment = comment.starts_with("//");
                    let comment_text = alloc.text(comment);

                    if pending_newlines > 0 {
                        inner = inner.append(
                            hardlines(alloc, pending_newlines)
                                .append(comment_text)
                                .nest(indent),
                        );
                        pending_newlines = 0;
                    } else {
                        inner = inner.append(alloc.text(" ")).append(comment_text);
                    }
                    needs_space = !is_line_comment;
                }
                _ => {}
            },
        }
    }

    alloc
        .text("fn ")
        .append(inner)
        .append(body_sep)
        .max_width_group(ctx.config.fn_sig_width)
}

/// Helper to build where clause document that is forced to a new line.
fn where_doc_forced<'a, N: ast::WhereClauseOwner + AstNode>(
    node: &N,
    ctx: &'a RewriteContext<'a>,
) -> Doc<'a> {
    let alloc = &ctx.alloc;

    if let Some(where_clause) = node.where_clause() {
        if where_clause.iter().next().is_none() && !has_comment_tokens(where_clause.syntax()) {
            return alloc.nil();
        }

        alloc.hardline().append(where_clause.to_doc(ctx))
    } else {
        alloc.nil()
    }
}

impl ToDoc for ast::Func {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);
        let const_kw = if self.const_kw().is_some() {
            alloc.text("const ")
        } else {
            alloc.nil()
        };

        let doc = if let Some(body) = self.body() {
            let has_where = self.sig().where_clause().is_some();
            let has_uses = self.sig().uses_clause().is_some();

            if ctx.config.where_new_line {
                // Always put brace on new line
                let sig = self.sig().to_doc(ctx);
                sig.append(alloc.hardline()).append(body.to_doc(ctx))
            } else if has_where || has_uses {
                // Include body separator in the group so it breaks together with the signature
                let sig_with_body_sep = func_sig_to_doc(&self.sig(), ctx, true);
                sig_with_body_sep.append(body.to_doc(ctx))
            } else {
                // Simple case: space before brace
                let sig = self.sig().to_doc(ctx);
                sig.append(alloc.text(" ")).append(body.to_doc(ctx))
            }
        } else {
            self.sig().to_doc(ctx)
        };

        attrs.append(modifier).append(const_kw).append(doc)
    }
}

impl ToDoc for ast::FuncParamList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let indent = ctx.config.indent_width as isize;
        block_list_auto(
            ctx,
            self.syntax(),
            "(",
            ")",
            ast::FuncParam::cast,
            indent,
            true,
        )
    }
}

impl ToDoc for ast::FuncParam {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;
        let mut doc = alloc.nil();

        if self.mut_token().is_some() {
            doc = doc.append(alloc.text("mut "));
        }
        if self.ref_token().is_some() {
            doc = doc.append(alloc.text("ref "));
        }
        if self.own_token().is_some() {
            doc = doc.append(alloc.text("own "));
        }

        let name = self.name();

        if self.is_label_suppressed() {
            doc = doc.append(alloc.text("_ "));
        }

        if let Some(name) = name {
            doc = doc.append(alloc.text(ctx.snippet(name.syntax().text_range()).trim()));
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

        token_doc_item_like_if_comments!(self, ctx);

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let name = self
            .name()
            .map(|n| alloc.text(ctx.token(&n)))
            .unwrap_or_else(|| alloc.nil());
        let generics = generics_doc(self, ctx);
        let where_clause = where_doc(self, ctx);

        let fields_doc = self
            .fields()
            .map(|f| alloc.text(" ").append(f.to_doc(ctx)))
            .unwrap_or_else(|| alloc.text(" {}"));

        attrs
            .append(modifier)
            .append(alloc.text("struct "))
            .append(name)
            .append(generics)
            .append(where_clause)
            .append(fields_doc)
    }
}

impl ToDoc for ast::RecordFieldDefList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let indent = ctx.config.indent_width as isize;
        block_list_spaced_auto(
            ctx,
            self.syntax(),
            "{",
            "}",
            ast::RecordFieldDef::cast,
            indent,
            true,
        )
    }
}

impl ToDoc for ast::ContractFields {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        use parser::syntax_kind::SyntaxKind;
        use parser::syntax_node::NodeOrToken;

        let alloc = &ctx.alloc;
        let mut result = alloc.nil();
        let mut pending_newlines = 0usize;
        let mut is_first = true;

        for child in self.syntax().children_with_tokens() {
            let entry_doc = match child {
                NodeOrToken::Node(node) => ast::RecordFieldDef::cast(node)
                    .map(|field| field.to_doc(ctx).append(alloc.text(","))),
                NodeOrToken::Token(token) => match token.kind() {
                    SyntaxKind::Newline => {
                        pending_newlines += newline_count(ctx.snippet(token.text_range()));
                        None
                    }
                    SyntaxKind::WhiteSpace | SyntaxKind::Comma => None,
                    SyntaxKind::Comment | SyntaxKind::DocComment => {
                        Some(alloc.text(ctx.snippet(token.text_range()).trim_end()))
                    }
                    _ => None,
                },
            };

            let Some(entry_doc) = entry_doc else {
                continue;
            };

            if is_first {
                is_first = false;
            } else {
                // Preserve whether there was a blank line (2+ newlines) between entries.
                result = result.append(hardlines(alloc, pending_newlines));
            }

            pending_newlines = 0;
            result = result.append(entry_doc);
        }

        if is_first { alloc.nil() } else { result }
    }
}

impl ToDoc for ast::RecordFieldDef {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let attrs = attrs_doc(self, ctx);

        let mut doc = attrs;

        if self.pub_kw().is_some() {
            doc = doc.append(alloc.text("pub "));
        }

        if self.mut_kw().is_some() {
            doc = doc.append(alloc.text("mut "));
        }

        if let Some(name) = self.name() {
            doc = doc.append(alloc.text(ctx.token(&name)));
        }

        if let Some(ty) = self.ty() {
            doc = doc.append(alloc.text(": ")).append(ty.to_doc(ctx));
        }

        doc
    }
}

impl ToDoc for ast::Contract {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        use parser::syntax_kind::SyntaxKind;
        use parser::syntax_node::NodeOrToken;

        let alloc = &ctx.alloc;

        let uses_doc = self.uses_clause().map(|u| u.to_doc(ctx));

        let mut inner = alloc.nil();
        let mut pending_newlines = 0usize;
        let mut is_first = true;
        let mut in_body = false;

        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Token(token) if token.kind() == SyntaxKind::LBrace => {
                    in_body = true;
                    continue;
                }
                NodeOrToken::Token(token) if token.kind() == SyntaxKind::RBrace && in_body => {
                    break;
                }
                _ if !in_body => continue,
                NodeOrToken::Node(node) => {
                    let entry_doc = if let Some(fields) = ast::ContractFields::cast(node.clone()) {
                        (fields.iter().next().is_some() || has_comment_tokens(fields.syntax()))
                            .then(|| fields.to_doc(ctx))
                    } else if let Some(init) = ast::ContractInit::cast(node.clone()) {
                        Some(init.to_doc(ctx))
                    } else {
                        ast::ContractRecv::cast(node).map(|recv| recv.to_doc(ctx))
                    };

                    let Some(entry_doc) = entry_doc else {
                        continue;
                    };

                    if is_first {
                        inner = inner.append(alloc.hardline());
                        is_first = false;
                    } else {
                        inner = inner.append(hardlines(alloc, pending_newlines));
                    }

                    pending_newlines = 0;
                    inner = inner.append(entry_doc);
                }
                NodeOrToken::Token(token) => match token.kind() {
                    SyntaxKind::Newline => {
                        pending_newlines += newline_count(ctx.snippet(token.text_range()));
                    }
                    SyntaxKind::WhiteSpace => continue,
                    SyntaxKind::Comment | SyntaxKind::DocComment => {
                        let comment_doc = alloc.text(ctx.snippet(token.text_range()).trim_end());

                        if is_first {
                            inner = inner.append(alloc.hardline());
                            is_first = false;
                        } else {
                            inner = inner.append(hardlines(alloc, pending_newlines));
                        }

                        pending_newlines = 0;
                        inner = inner.append(comment_doc);
                    }
                    _ => continue,
                },
            }
        }

        let body_doc = if is_first {
            alloc.text("{}")
        } else {
            alloc
                .text("{")
                .append(inner.nest(ctx.config.indent_width as isize))
                .append(alloc.hardline())
                .append(alloc.text("}"))
        };

        if !has_comment_tokens(self.syntax()) {
            let attrs = attrs_doc(self, ctx);
            let modifier = modifier_doc(self, ctx);

            let name = self
                .name()
                .map(|n| alloc.text(ctx.token(&n)))
                .unwrap_or_else(|| alloc.nil());

            let uses_doc = uses_doc
                .map(|u| alloc.text(" ").append(u))
                .unwrap_or_else(|| alloc.nil());

            return attrs
                .append(modifier)
                .append(alloc.text("contract "))
                .append(name)
                .append(uses_doc)
                .append(alloc.text(" "))
                .append(body_doc);
        }

        let (header_doc, ends_with_newline) = token_doc_until_token(
            ctx,
            self.syntax(),
            0,
            SyntaxKind::LBrace,
            |node| token_doc_item_node_piece(ctx, node),
            |token| token_piece_basic(ctx, token),
        );

        let sep = if ends_with_newline {
            alloc.nil()
        } else {
            alloc.text(" ")
        };

        header_doc.append(sep).append(body_doc)
    }
}

impl ToDoc for ast::ContractInit {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let params_doc = self
            .params()
            .map(|params| params.to_doc(ctx))
            .unwrap_or_else(|| alloc.text("()"));

        let uses_doc = self
            .uses_clause()
            .map(|u| alloc.line().append(u.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        let body_doc = self
            .body()
            .map(|b| alloc.line().append(b.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        alloc
            .text("init")
            .append(params_doc)
            .append(uses_doc)
            .append(body_doc)
            .group()
    }
}

impl ToDoc for ast::ContractRecv {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let path_doc = self
            .path()
            .map(|p| alloc.text(" ").append(p.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        let arms_doc = self
            .arms()
            .map(|arms| alloc.text(" ").append(arms.to_doc(ctx)))
            .unwrap_or_else(|| alloc.text(" {}"));

        alloc.text("recv").append(path_doc).append(arms_doc)
    }
}

impl ToDoc for ast::RecvArmList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        block_items_doc(self.syntax(), ast::RecvArm::cast, ctx)
    }
}

impl ToDoc for ast::RecvArm {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let pat_doc = self
            .pat()
            .map(|p| p.to_doc(ctx))
            .unwrap_or_else(|| alloc.nil());

        let ret_ty_doc = self
            .ret_ty()
            .map(|ty| alloc.text(" -> ").append(ty.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        let has_uses = self.uses_clause().is_some();

        // Measure the core signature (pattern + return type) to decide if uses should break
        let core_sig = pat_doc.clone().append(ret_ty_doc.clone());
        let core_flat_len = {
            let mut buf = Vec::new();
            let _ = core_sig.clone().into_doc().render(10000, &mut buf);
            let s = String::from_utf8(buf).unwrap_or_default();
            s.lines().next().map(|l| l.len()).unwrap_or(0)
        };

        // Force uses to a new line if:
        // - uses_new_line config is set, OR
        // - the core signature is long (> 40 chars)
        let force_uses_break = has_uses && (ctx.config.uses_new_line || core_flat_len > 40);

        let uses_doc = self
            .uses_clause()
            .map(|u| {
                if force_uses_break {
                    alloc.hardline().append(u.to_doc(ctx))
                } else {
                    alloc.text(" ").append(u.to_doc(ctx))
                }
            })
            .unwrap_or_else(|| alloc.nil());

        let body_doc = self
            .body()
            .map(|b| {
                if force_uses_break {
                    // If uses broke to new line, put body on new line too
                    alloc.hardline().append(b.to_doc(ctx))
                } else {
                    alloc.text(" ").append(b.to_doc(ctx))
                }
            })
            .unwrap_or_else(|| alloc.nil());

        pat_doc.append(ret_ty_doc).append(uses_doc).append(body_doc)
    }
}

impl ToDoc for ast::Enum {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let name = self
            .name()
            .map(|n| alloc.text(ctx.token(&n)))
            .unwrap_or_else(|| alloc.nil());
        let generics = generics_doc(self, ctx);
        let where_clause = where_doc(self, ctx);

        let variants_doc = self
            .variants()
            .map(|v| alloc.text(" ").append(v.to_doc(ctx)))
            .unwrap_or_else(|| alloc.text(" {}"));

        attrs
            .append(modifier)
            .append(alloc.text("enum "))
            .append(name)
            .append(generics)
            .append(where_clause)
            .append(variants_doc)
    }
}

impl ToDoc for ast::VariantDefList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let indent = ctx.config.indent_width as isize;
        block_list_with_comments(
            ctx,
            self.syntax(),
            "{",
            "}",
            ast::VariantDef::cast,
            indent,
            true,
        )
    }
}

impl ToDoc for ast::VariantDef {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let attrs = attrs_doc(self, ctx);

        let name = self
            .name()
            .map(|n| alloc.text(ctx.token(&n)))
            .unwrap_or_else(|| alloc.nil());

        let kind_doc = match self.kind() {
            ast::VariantKind::Unit => alloc.nil(),
            ast::VariantKind::Tuple(tuple_type) => tuple_type.to_doc(ctx),
            ast::VariantKind::Record(fields) => {
                if has_comment_tokens(fields.syntax()) {
                    return attrs.append(name.clone()).append(alloc.text(" ")).append(
                        fields
                            .to_doc(ctx)
                            .max_width_group(ctx.config.struct_variant_width),
                    );
                }

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

        attrs.append(name).append(kind_doc)
    }
}

impl ToDoc for ast::Trait {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let name = self
            .name()
            .map(|n| alloc.text(ctx.token(&n)))
            .unwrap_or_else(|| alloc.nil());
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
            .append(name)
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

        token_doc_item_like_if_comments!(self, ctx);

        let attrs = attrs_doc(self, ctx);

        let name = self
            .name()
            .map(|n| alloc.text(ctx.token(&n)))
            .unwrap_or_else(|| alloc.nil());

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
            .append(name)
            .append(bounds_doc)
            .append(ty_doc)
    }
}

impl ToDoc for ast::TraitConstItem {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let attrs = attrs_doc(self, ctx);

        let name = self
            .name()
            .map(|n| alloc.text(ctx.token(&n)))
            .unwrap_or_else(|| alloc.nil());

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
            .append(name)
            .append(ty_doc)
            .append(value_doc)
    }
}

impl ToDoc for ast::Impl {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

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

        token_doc_item_like_if_comments!(self, ctx);

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

        token_doc_item_like_if_comments!(self, ctx);

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let name = self
            .name()
            .map(|n| alloc.text(ctx.token(&n)))
            .unwrap_or_else(|| alloc.nil());

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
            .append(name)
            .append(ty_doc)
            .append(value_doc)
    }
}

impl ToDoc for ast::Use {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

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

        token_doc_item_like_if_comments!(self, ctx);

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
        let indent = ctx.config.indent_width as isize;
        block_list_auto(
            ctx,
            self.syntax(),
            "{",
            "}",
            ast::UseTree::cast,
            indent,
            true,
        )
        .max_width_group(ctx.config.use_tree_width)
    }
}

impl ToDoc for ast::UsePath {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let segments: Vec<_> = self.into_iter().map(|seg| seg.to_doc(ctx)).collect();

        let sep = alloc.text("::");
        intersperse(alloc, segments, sep)
    }
}

impl ToDoc for ast::UsePathSegment {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        use parser::ast::UsePathSegmentKind::*;
        match self.kind() {
            Some(Ingot(token) | Super(token) | Self_(token) | Ident(token) | Glob(token)) => {
                alloc.text(ctx.token(&token))
            }
            None => alloc.nil(),
        }
    }
}

impl ToDoc for ast::UseAlias {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if let Some(alias) = self.alias() {
            alloc.text(" as ").append(alloc.text(ctx.token(&alias)))
        } else {
            alloc.nil()
        }
    }
}

impl ToDoc for ast::TypeAlias {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let alias = self
            .alias()
            .map(|a| alloc.text(ctx.token(&a)))
            .unwrap_or_else(|| alloc.nil());
        let generics = generics_doc(self, ctx);

        let ty_doc = self
            .ty()
            .map(|ty| alloc.text(" = ").append(ty.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        attrs
            .append(modifier)
            .append(alloc.text("type "))
            .append(alias)
            .append(generics)
            .append(ty_doc)
    }
}

impl ToDoc for ast::Mod {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let name = self
            .name()
            .map(|n| alloc.text(ctx.token(&n)))
            .unwrap_or_else(|| alloc.nil());

        let items_doc = self
            .items()
            .map(|items| {
                alloc
                    .text(" ")
                    .append(block_items_doc(items.syntax(), ast::Item::cast, ctx))
            })
            .unwrap_or_else(|| alloc.nil());

        attrs
            .append(modifier)
            .append(alloc.text("mod "))
            .append(name)
            .append(items_doc)
    }
}

impl ToDoc for ast::Extern {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

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

impl ToDoc for ast::Msg {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let attrs = attrs_doc(self, ctx);
        let modifier = modifier_doc(self, ctx);

        let name = self
            .name()
            .map(|n| alloc.text(ctx.token(&n)))
            .unwrap_or_else(|| alloc.nil());

        let variants_doc = self
            .variants()
            .map(|v| alloc.text(" ").append(v.to_doc(ctx)))
            .unwrap_or_else(|| alloc.text(" {}"));

        attrs
            .append(modifier)
            .append(alloc.text("msg "))
            .append(name)
            .append(variants_doc)
    }
}

impl ToDoc for ast::MsgVariantList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let indent = ctx.config.indent_width as isize;
        block_list_with_comments(
            ctx,
            self.syntax(),
            "{",
            "}",
            ast::MsgVariant::cast,
            indent,
            true,
        )
    }
}

impl ToDoc for ast::MsgVariant {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        token_doc_item_like_if_comments!(self, ctx);

        let attrs = attrs_doc(self, ctx);

        let name = self
            .name()
            .map(|n| alloc.text(ctx.token(&n)))
            .unwrap_or_else(|| alloc.nil());

        let params_doc = self
            .params()
            .map(|p| alloc.text(" ").append(p.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        let ret_ty_doc = self
            .ret_ty()
            .map(|ty| alloc.text(" -> ").append(ty.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        attrs.append(name).append(params_doc).append(ret_ty_doc)
    }
}

impl ToDoc for ast::MsgVariantParams {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let indent = ctx.config.indent_width as isize;
        block_list_spaced_auto(
            ctx,
            self.syntax(),
            "{",
            "}",
            ast::RecordFieldDef::cast,
            indent,
            true,
        )
    }
}
