//! Formatting for types, paths, generics, and type-related constructs.

use pretty::{DocAllocator, DocBuilder, RcAllocator};

use crate::RewriteContext;
use parser::ast::{
    self, GenericArgKind, GenericArgsOwner, GenericParamKind, TypeKind, prelude::AstNode,
};
use parser::syntax_kind::SyntaxKind;
use parser::syntax_node::NodeOrToken;

/// Type alias for the document builder type used throughout formatting.
pub type Doc<'a> = DocBuilder<'a, RcAllocator, ()>;

/// Extension trait for converting AST nodes to pretty documents.
pub trait ToDoc {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a>;
}

/// Helper to intersperse documents with a separator.
pub fn intersperse<'a>(
    alloc: &'a RcAllocator,
    docs: impl IntoIterator<Item = Doc<'a>>,
    sep: Doc<'a>,
) -> Doc<'a> {
    alloc.intersperse(docs, sep)
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
    ctx: &'a RewriteContext<'a>,
    open: &'a str,
    close: &'a str,
    items: Vec<Doc<'a>>,
    indent: isize,
    trailing_comma: bool,
) -> Doc<'a> {
    block_list_inner(ctx, open, close, items, indent, trailing_comma, false)
}

/// Like `block_list`, but adds spaces inside delimiters when rendered flat.
/// Use this for brace-delimited lists like `{ x, y }`.
pub fn block_list_spaced<'a>(
    ctx: &'a RewriteContext<'a>,
    open: &'a str,
    close: &'a str,
    items: Vec<Doc<'a>>,
    indent: isize,
    trailing_comma: bool,
) -> Doc<'a> {
    block_list_inner(ctx, open, close, items, indent, trailing_comma, true)
}

macro_rules! block_list_auto_impl {
    ($name:ident, $mk:ident) => {
        pub fn $name<'a, T: ToDoc>(
            ctx: &'a RewriteContext<'a>,
            syntax: &parser::SyntaxNode,
            open: &'a str,
            close: &'a str,
            cast_fn: impl Fn(parser::SyntaxNode) -> Option<T>,
            indent: isize,
            trailing_comma: bool,
        ) -> Doc<'a> {
            if has_comment_tokens(syntax) {
                block_list_with_comments(ctx, syntax, open, close, cast_fn, indent, trailing_comma)
            } else {
                let items: Vec<_> = syntax
                    .children()
                    .filter_map(cast_fn)
                    .map(|item| item.to_doc(ctx))
                    .collect();
                $mk(ctx, open, close, items, indent, trailing_comma)
            }
        }
    };
}
block_list_auto_impl!(block_list_auto, block_list);
block_list_auto_impl!(block_list_spaced_auto, block_list_spaced);

pub fn has_comment_tokens(syntax: &parser::SyntaxNode) -> bool {
    syntax.children_with_tokens().any(|child| {
        matches!(
            child,
            NodeOrToken::Token(t) if matches!(t.kind(), SyntaxKind::Comment | SyntaxKind::DocComment)
        )
    })
}

pub(crate) fn hardlines<'a>(alloc: &'a RcAllocator, count: usize) -> Doc<'a> {
    let doc = alloc.hardline();
    if count >= 2 {
        doc.append(alloc.hardline())
    } else {
        doc
    }
}

pub(crate) fn newline_count(text: &str) -> usize {
    text.bytes().filter(|b| *b == b'\n').count()
}

#[derive(Clone)]
pub(crate) struct TokenPiece<'a> {
    pub doc: Doc<'a>,
    pub space_before: bool,
    pub space_after: bool,
    pub nest: bool,
}

impl<'a> TokenPiece<'a> {
    pub fn new(doc: Doc<'a>) -> Self {
        Self {
            doc,
            space_before: false,
            space_after: false,
            nest: true,
        }
    }

    pub fn spaces(mut self) -> Self {
        self.space_before = true;
        self.space_after = true;
        self
    }
    pub fn space_before(mut self) -> Self {
        self.space_before = true;
        self
    }
    pub fn space_after(mut self) -> Self {
        self.space_after = true;
        self
    }
    pub fn no_nest(mut self) -> Self {
        self.nest = false;
        self
    }
}

struct TokenDocBuilder<'a> {
    ctx: &'a RewriteContext<'a>,
    indent: isize,
    doc: Doc<'a>,
    pending_newlines: usize,
    needs_space: bool,
    is_start: bool,
}

impl<'a> TokenDocBuilder<'a> {
    fn new(ctx: &'a RewriteContext<'a>, indent: isize) -> Self {
        let alloc = &ctx.alloc;
        Self {
            ctx,
            indent,
            doc: alloc.nil(),
            pending_newlines: 0,
            needs_space: false,
            is_start: true,
        }
    }

    fn append(&mut self, doc: Doc<'a>) {
        let alloc = &self.ctx.alloc;
        self.doc = std::mem::replace(&mut self.doc, alloc.nil()).append(doc);
    }

    fn bump_newlines(&mut self, token: &parser::SyntaxToken) {
        self.pending_newlines += newline_count(self.ctx.snippet(token.text_range()));
    }

    fn push_piece(&mut self, piece: TokenPiece<'a>) {
        let alloc = &self.ctx.alloc;

        if self.pending_newlines > 0 {
            let doc = hardlines(alloc, self.pending_newlines).append(piece.doc);
            self.append(if piece.nest {
                doc.nest(self.indent)
            } else {
                doc
            });
            self.pending_newlines = 0;
        } else {
            if !self.is_start && (self.needs_space || piece.space_before) {
                self.append(alloc.text(" "));
            }
            self.append(piece.doc);
        }

        self.needs_space = piece.space_after;
        self.is_start = false;
    }

    fn push_comment(&mut self, token: &parser::SyntaxToken) {
        let alloc = &self.ctx.alloc;
        let comment = self.ctx.snippet(token.text_range()).trim_end();
        let is_line_comment = comment.starts_with("//");
        self.push_piece(TokenPiece {
            doc: alloc.text(comment),
            space_before: true,
            space_after: !is_line_comment,
            nest: true,
        });
    }

    fn finish(self) -> Doc<'a> {
        self.doc
    }
}

pub(crate) fn token_doc<'a>(
    ctx: &'a RewriteContext<'a>,
    syntax: &parser::SyntaxNode,
    indent: isize,
    node_piece: impl FnMut(parser::SyntaxNode) -> Option<TokenPiece<'a>>,
    token_piece: impl FnMut(parser::SyntaxToken) -> Option<TokenPiece<'a>>,
) -> Doc<'a> {
    token_doc_inner(ctx, syntax, indent, None, node_piece, token_piece).0
}

pub(crate) fn token_doc_until_token<'a>(
    ctx: &'a RewriteContext<'a>,
    syntax: &parser::SyntaxNode,
    indent: isize,
    stop_kind: SyntaxKind,
    node_piece: impl FnMut(parser::SyntaxNode) -> Option<TokenPiece<'a>>,
    token_piece: impl FnMut(parser::SyntaxToken) -> Option<TokenPiece<'a>>,
) -> (Doc<'a>, bool) {
    token_doc_inner(
        ctx,
        syntax,
        indent,
        Some(stop_kind),
        node_piece,
        token_piece,
    )
}

fn token_doc_inner<'a>(
    ctx: &'a RewriteContext<'a>,
    syntax: &parser::SyntaxNode,
    indent: isize,
    stop_kind: Option<SyntaxKind>,
    mut node_piece: impl FnMut(parser::SyntaxNode) -> Option<TokenPiece<'a>>,
    mut token_piece: impl FnMut(parser::SyntaxToken) -> Option<TokenPiece<'a>>,
) -> (Doc<'a>, bool) {
    let mut builder = TokenDocBuilder::new(ctx, indent);
    let alloc = &ctx.alloc;

    for child in syntax.children_with_tokens() {
        match child {
            NodeOrToken::Node(node) => {
                if let Some(piece) = node_piece(node) {
                    builder.push_piece(piece);
                }
            }
            NodeOrToken::Token(token) => {
                if stop_kind.is_some_and(|kind| token.kind() == kind) {
                    let ends_with_newline = builder.pending_newlines > 0;
                    if ends_with_newline {
                        builder.append(hardlines(alloc, builder.pending_newlines));
                        builder.pending_newlines = 0;
                    }
                    return (builder.finish(), ends_with_newline);
                }

                match token.kind() {
                    SyntaxKind::Newline => builder.bump_newlines(&token),
                    SyntaxKind::WhiteSpace => {}
                    SyntaxKind::Comment | SyntaxKind::DocComment => builder.push_comment(&token),
                    _ => {
                        if let Some(piece) = token_piece(token.clone()) {
                            builder.push_piece(piece);
                        } else {
                            let text = ctx.token(&token);
                            if !text.is_empty() {
                                builder.push_piece(TokenPiece::new(ctx.alloc.text(text)));
                            }
                        }
                    }
                }
            }
        }
    }

    (builder.finish(), false)
}

fn colon_plus_list_with_comments<'a, T: ToDoc>(
    ctx: &'a RewriteContext<'a>,
    syntax: &parser::SyntaxNode,
    cast_fn: impl Fn(parser::SyntaxNode) -> Option<T>,
    indent: isize,
) -> Doc<'a> {
    let alloc = &ctx.alloc;

    let mut doc = alloc.nil();
    let mut pending_newlines = 0usize;
    let mut needs_space = false;

    for child in syntax.children_with_tokens() {
        match child {
            NodeOrToken::Node(node) => {
                let Some(elem) = cast_fn(node) else {
                    continue;
                };

                if pending_newlines > 0 {
                    doc = doc.append(
                        hardlines(alloc, pending_newlines)
                            .append(elem.to_doc(ctx))
                            .nest(indent),
                    );
                    pending_newlines = 0;
                } else {
                    if needs_space {
                        doc = doc.append(alloc.text(" "));
                    }
                    doc = doc.append(elem.to_doc(ctx));
                }
                needs_space = false;
            }
            NodeOrToken::Token(token) => match token.kind() {
                SyntaxKind::Newline => {
                    pending_newlines += newline_count(ctx.snippet(token.text_range()));
                }
                SyntaxKind::WhiteSpace => {}
                SyntaxKind::Colon => {
                    doc = doc.append(alloc.text(":"));
                    needs_space = true;
                }
                SyntaxKind::Plus => {
                    if pending_newlines > 0 {
                        doc = doc.append(
                            hardlines(alloc, pending_newlines)
                                .append(alloc.text("+"))
                                .nest(indent),
                        );
                        pending_newlines = 0;
                    } else {
                        doc = doc.append(alloc.text(" +"));
                    }
                    needs_space = true;
                }
                SyntaxKind::Comment | SyntaxKind::DocComment => {
                    let comment = ctx.snippet(token.text_range()).trim_end();
                    let is_line_comment = comment.starts_with("//");
                    let comment_text = alloc.text(comment);
                    if pending_newlines > 0 {
                        doc = doc.append(
                            hardlines(alloc, pending_newlines)
                                .append(comment_text)
                                .nest(indent),
                        );
                        pending_newlines = 0;
                    } else {
                        doc = doc.append(alloc.text(" ")).append(comment_text);
                    }
                    needs_space = !is_line_comment;
                }
                _ => {}
            },
        }
    }

    doc
}

pub fn block_list_with_comments<'a, T: ToDoc>(
    ctx: &'a RewriteContext<'a>,
    syntax: &parser::SyntaxNode,
    open: &'a str,
    close: &'a str,
    cast_fn: impl Fn(parser::SyntaxNode) -> Option<T>,
    indent: isize,
    trailing_comma: bool,
) -> Doc<'a> {
    let alloc = &ctx.alloc;

    struct Entry<'a> {
        doc: Doc<'a>,
        blank_line_before: bool,
        is_element: bool,
    }

    let mut entries: Vec<Entry<'a>> = Vec::new();
    let mut pending_newlines = 0usize;

    for child in syntax.children_with_tokens() {
        match child {
            NodeOrToken::Node(node) => {
                let Some(elem) = cast_fn(node) else {
                    continue;
                };
                entries.push(Entry {
                    doc: elem.to_doc(ctx),
                    blank_line_before: pending_newlines >= 2,
                    is_element: true,
                });
                pending_newlines = 0;
            }
            NodeOrToken::Token(token) => match token.kind() {
                SyntaxKind::Newline => {
                    pending_newlines += newline_count(ctx.snippet(token.text_range()));
                }
                SyntaxKind::WhiteSpace => {}
                SyntaxKind::Comment | SyntaxKind::DocComment => {
                    entries.push(Entry {
                        doc: alloc.text(ctx.snippet(token.text_range()).trim_end()),
                        blank_line_before: pending_newlines >= 2,
                        is_element: false,
                    });
                    pending_newlines = 0;
                }
                _ => {}
            },
        }
    }

    if entries.is_empty() {
        return alloc.text(format!("{open}{close}"));
    }

    let last_element_idx = entries.iter().rposition(|e| e.is_element);

    let mut inner = alloc.nil();
    for (idx, entry) in entries.into_iter().enumerate() {
        inner = inner.append(hardlines(
            alloc,
            if entry.blank_line_before { 2 } else { 1 },
        ));
        let doc = if entry.is_element && (trailing_comma || last_element_idx != Some(idx)) {
            entry.doc.append(alloc.text(","))
        } else {
            entry.doc
        };
        inner = inner.append(doc);
    }

    alloc
        .text(open)
        .append(inner.nest(indent))
        .append(alloc.hardline())
        .append(alloc.text(close))
}

fn block_list_inner<'a>(
    ctx: &'a RewriteContext<'a>,
    open: &'a str,
    close: &'a str,
    items: Vec<Doc<'a>>,
    indent: isize,
    trailing_comma: bool,
    spaced: bool,
) -> Doc<'a> {
    let alloc = &ctx.alloc;

    if items.is_empty() {
        return alloc.text(format!("{}{}", open, close));
    }

    let sep = alloc.text(",").append(alloc.line());
    let inner = intersperse(alloc, items, sep);

    let trailing = if trailing_comma {
        alloc.text(",").flat_alt(alloc.nil())
    } else {
        alloc.nil()
    };

    // For spaced variant, use line() which renders as space when flat
    // For non-spaced variant, use line_() which renders as empty when flat
    let break_token = if spaced { alloc.line() } else { alloc.line_() };

    alloc
        .text(open)
        .append(
            break_token
                .clone()
                .append(inner)
                .append(trailing)
                .nest(indent),
        )
        .append(break_token)
        .append(alloc.text(close))
        .group()
}

impl ToDoc for ast::GenericParamList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let indent = ctx.config.indent_width as isize;
        block_list_auto(
            ctx,
            self.syntax(),
            "<",
            ">",
            ast::GenericParam::cast,
            indent,
            true,
        )
    }
}

impl ToDoc for ast::GenericParam {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.kind() {
            GenericParamKind::Type(ty_param) => ty_param.to_doc(ctx),
            GenericParamKind::Const(const_param) => const_param.to_doc(ctx),
        }
    }
}

impl ToDoc for ast::TypeGenericParam {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let name = self
            .name()
            .map(|n| alloc.text(ctx.token(&n)))
            .unwrap_or_else(|| alloc.nil());

        let bounds = self
            .bounds()
            .map(|b| b.to_doc(ctx))
            .unwrap_or_else(|| alloc.nil());

        let default = self
            .default_ty()
            .map(|ty| alloc.text(" = ").append(ty.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        name.append(bounds).append(default)
    }
}

impl ToDoc for ast::ConstGenericParam {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let name = self
            .name()
            .map(|n| alloc.text(ctx.token(&n)))
            .unwrap_or_else(|| alloc.nil());

        let ty = self
            .ty()
            .map(|ty| alloc.text(": ").append(ty.to_doc(ctx)))
            .unwrap_or_else(|| alloc.nil());

        alloc.text("const ").append(name).append(ty)
    }
}

impl ToDoc for ast::WhereClause {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let indent = ctx.config.clause_indent as isize;

        if !has_comment_tokens(self.syntax()) {
            let predicates: Vec<_> = self.into_iter().map(|pred| pred.to_doc(ctx)).collect();
            if predicates.is_empty() {
                return alloc.nil();
            }

            let sep = alloc.text(",").append(alloc.line());
            let inner = intersperse(alloc, predicates, sep).group();

            return alloc
                .text("where")
                .append(alloc.line().append(inner).nest(indent))
                .group();
        }

        struct Entry<'a> {
            doc: Doc<'a>,
            blank_line_before: bool,
            is_predicate: bool,
        }

        let mut entries: Vec<Entry<'a>> = Vec::new();
        let mut pending_newlines = 0usize;

        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    let Some(pred) = ast::WherePredicate::cast(node) else {
                        continue;
                    };
                    entries.push(Entry {
                        doc: pred.to_doc(ctx),
                        blank_line_before: pending_newlines >= 2,
                        is_predicate: true,
                    });
                    pending_newlines = 0;
                }
                NodeOrToken::Token(token) => match token.kind() {
                    SyntaxKind::Newline => {
                        pending_newlines += newline_count(ctx.snippet(token.text_range()));
                    }
                    SyntaxKind::WhiteSpace | SyntaxKind::Comma | SyntaxKind::WhereKw => {}
                    SyntaxKind::Comment | SyntaxKind::DocComment => {
                        entries.push(Entry {
                            doc: alloc.text(ctx.snippet(token.text_range()).trim_end()),
                            blank_line_before: pending_newlines >= 2,
                            is_predicate: false,
                        });
                        pending_newlines = 0;
                    }
                    _ => {}
                },
            }
        }

        let last_predicate_idx = entries.iter().rposition(|e| e.is_predicate);
        if last_predicate_idx.is_none() {
            return alloc.nil();
        }

        let mut inner = alloc.nil();
        let mut is_first = true;

        for (idx, entry) in entries.into_iter().enumerate() {
            if is_first {
                is_first = false;
            } else {
                inner = inner.append(alloc.hardline());
                if entry.blank_line_before {
                    inner = inner.append(alloc.hardline());
                }
            }

            let doc = if entry.is_predicate && last_predicate_idx != Some(idx) {
                entry.doc.append(alloc.text(","))
            } else {
                entry.doc
            };
            inner = inner.append(doc);
        }

        alloc
            .text("where")
            .append(alloc.line().append(inner).nest(indent))
            .group()
    }
}

impl ToDoc for ast::WherePredicate {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if !has_comment_tokens(self.syntax()) {
            let ty = match self.ty() {
                Some(t) => t.to_doc(ctx),
                None => return alloc.nil(),
            };

            if let Some(bounds) = self.bounds() {
                return ty.append(bounds.to_doc(ctx));
            }
            return ty;
        }

        let indent = ctx.config.clause_indent as isize;
        let mut doc = alloc.nil();
        let mut pending_newlines = 0usize;

        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    let elem = if let Some(ty) = ast::Type::cast(node.clone()) {
                        ty.to_doc(ctx)
                    } else if let Some(bounds) = ast::TypeBoundList::cast(node) {
                        bounds.to_doc(ctx)
                    } else {
                        continue;
                    };

                    if pending_newlines > 0 {
                        doc = doc
                            .append(hardlines(alloc, pending_newlines).append(elem).nest(indent));
                        pending_newlines = 0;
                    } else {
                        doc = doc.append(elem);
                    }
                }
                NodeOrToken::Token(token) => match token.kind() {
                    SyntaxKind::Newline => {
                        pending_newlines += newline_count(ctx.snippet(token.text_range()));
                    }
                    SyntaxKind::WhiteSpace => {}
                    SyntaxKind::Comment | SyntaxKind::DocComment => {
                        let comment_text = alloc.text(ctx.snippet(token.text_range()).trim_end());

                        if pending_newlines > 0 {
                            doc = doc.append(
                                hardlines(alloc, pending_newlines)
                                    .append(comment_text)
                                    .nest(indent),
                            );
                            pending_newlines = 0;
                        } else {
                            doc = doc.append(alloc.text(" ")).append(comment_text);
                        }
                    }
                    _ => {}
                },
            }
        }

        doc
    }
}

impl ToDoc for ast::TypeBoundList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if self.into_iter().next().is_none() {
            return alloc.nil();
        }

        if has_comment_tokens(self.syntax()) {
            let indent = ctx.config.clause_indent as isize;
            return colon_plus_list_with_comments(ctx, self.syntax(), ast::TypeBound::cast, indent);
        }

        let bounds: Vec<_> = self.into_iter().map(|bound| bound.to_doc(ctx)).collect();
        alloc
            .text(": ")
            .append(intersperse(alloc, bounds, alloc.text(" + ")))
    }
}

impl ToDoc for ast::TypeBound {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if let Some(trait_bound) = self.trait_bound() {
            trait_bound.to_doc(ctx)
        } else if let Some(kind_bound) = self.kind_bound() {
            alloc.text(ctx.snippet_trimmed(&kind_bound))
        } else {
            alloc.nil()
        }
    }
}

impl ToDoc for ast::TraitRef {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.path() {
            Some(p) => p.to_doc(ctx),
            None => ctx.alloc.nil(),
        }
    }
}

impl ToDoc for ast::SuperTraitList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if self.into_iter().next().is_none() {
            return alloc.nil();
        }

        if !has_comment_tokens(self.syntax()) {
            let traits: Vec<_> = self.into_iter().map(|t| t.to_doc(ctx)).collect();
            let sep = alloc.text(" + ");
            return alloc.text(": ").append(intersperse(alloc, traits, sep));
        }

        let indent = ctx.config.clause_indent as isize;
        colon_plus_list_with_comments(ctx, self.syntax(), ast::TraitRef::cast, indent)
    }
}

impl ToDoc for ast::Type {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.kind() {
            TypeKind::Mode(_) => todo!(),
            TypeKind::Ptr(ptr) => ptr.to_doc(ctx),
            TypeKind::Path(path) => path.to_doc(ctx),
            TypeKind::Tuple(tuple) => tuple.to_doc(ctx),
            TypeKind::Array(array) => array.to_doc(ctx),
            TypeKind::Never(never) => never.to_doc(ctx),
        }
    }
}

impl ToDoc for ast::PtrType {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if !has_comment_tokens(self.syntax()) {
            return match self.inner() {
                Some(inner) => alloc.text("*").append(inner.to_doc(ctx)),
                None => alloc.text("*"),
            };
        }

        let indent = ctx.config.indent_width as isize;
        token_doc(
            ctx,
            self.syntax(),
            indent,
            |node| ast::Type::cast(node).map(|ty| TokenPiece::new(ty.to_doc(ctx))),
            |token| match token.kind() {
                SyntaxKind::Star => Some(TokenPiece::new(alloc.text("*"))),
                _ => None,
            },
        )
    }
}

impl ToDoc for ast::PathType {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let mut doc = match self.path() {
            Some(p) => p.to_doc(ctx),
            None => return alloc.nil(),
        };

        if let Some(args) = self.generic_args() {
            doc = doc.append(args.to_doc(ctx));
        }

        doc
    }
}

impl ToDoc for ast::Path {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if !has_comment_tokens(self.syntax()) {
            let segments: Vec<_> = self.segments().map(|seg| seg.to_doc(ctx)).collect();
            return intersperse(alloc, segments, alloc.text("::"));
        }

        let indent = ctx.config.indent_width as isize;
        token_doc(
            ctx,
            self.syntax(),
            indent,
            |node| ast::PathSegment::cast(node).map(|seg| TokenPiece::new(seg.to_doc(ctx))),
            |token| match token.kind() {
                SyntaxKind::Colon2 => Some(TokenPiece::new(alloc.text("::"))),
                _ => None,
            },
        )
    }
}

impl ToDoc for ast::PathSegment {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if !has_comment_tokens(self.syntax()) {
            let mut doc = if let Some(kind) = self.kind() {
                match kind {
                    ast::PathSegmentKind::QualifiedType(q) => q.to_doc(ctx),
                    _ => self
                        .ident()
                        .map(|ident| alloc.text(ctx.token(&ident)))
                        .unwrap_or_else(|| alloc.nil()),
                }
            } else {
                alloc.nil()
            };

            if let Some(args) = self.generic_args() {
                doc = doc.append(args.to_doc(ctx));
            }

            return doc;
        }

        let indent = ctx.config.indent_width as isize;
        token_doc(
            ctx,
            self.syntax(),
            indent,
            |node| {
                if let Some(qualified) = ast::QualifiedType::cast(node.clone()) {
                    return Some(TokenPiece::new(qualified.to_doc(ctx)));
                }
                ast::GenericArgList::cast(node).map(|args| TokenPiece::new(args.to_doc(ctx)))
            },
            |token| match token.kind() {
                SyntaxKind::Ident
                | SyntaxKind::SuperKw
                | SyntaxKind::SelfKw
                | SyntaxKind::SelfTypeKw
                | SyntaxKind::IngotKw => Some(TokenPiece::new(alloc.text(ctx.token(&token)))),
                _ => None,
            },
        )
    }
}

impl ToDoc for ast::QualifiedType {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if !has_comment_tokens(self.syntax()) {
            let ty = match self.ty() {
                Some(t) => t.to_doc(ctx),
                None => return alloc.nil(),
            };
            let trait_path = match self.trait_qualifier() {
                Some(p) => p.to_doc(ctx),
                None => return alloc.nil(),
            };

            return alloc
                .text("<")
                .append(ty)
                .append(alloc.text(" as "))
                .append(trait_path)
                .append(alloc.text(">"));
        }

        let indent = ctx.config.indent_width as isize;
        token_doc(
            ctx,
            self.syntax(),
            indent,
            |node| {
                if let Some(ty) = ast::Type::cast(node.clone()) {
                    return Some(TokenPiece::new(ty.to_doc(ctx)));
                }
                ast::TraitRef::cast(node).map(|tr| TokenPiece::new(tr.to_doc(ctx)))
            },
            |token| match token.kind() {
                SyntaxKind::Lt => Some(TokenPiece::new(alloc.text("<"))),
                SyntaxKind::AsKw => Some(TokenPiece::new(alloc.text("as")).spaces()),
                SyntaxKind::Gt => Some(TokenPiece::new(alloc.text(">")).no_nest()),
                _ => None,
            },
        )
    }
}

impl ToDoc for ast::GenericArgList {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let indent = ctx.config.indent_width as isize;
        block_list_auto(
            ctx,
            self.syntax(),
            "<",
            ">",
            ast::GenericArg::cast,
            indent,
            true,
        )
    }
}

impl ToDoc for ast::GenericArg {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.kind() {
            GenericArgKind::Type(ty_arg) => ty_arg.to_doc(ctx),
            GenericArgKind::Const(const_arg) => const_arg.to_doc(ctx),
            GenericArgKind::AssocType(assoc_arg) => assoc_arg.to_doc(ctx),
        }
    }
}

impl ToDoc for ast::TypeGenericArg {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.ty() {
            Some(t) => t.to_doc(ctx),
            None => ctx.alloc.nil(),
        }
    }
}

impl ToDoc for ast::ConstGenericArg {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.expr() {
            Some(e) => e.to_doc(ctx),
            None => ctx.alloc.nil(),
        }
    }
}

impl ToDoc for ast::AssocTypeGenericArg {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let Some(name) = self.name() else {
            return alloc.nil();
        };
        let Some(ty) = self.ty() else {
            return alloc.nil();
        };

        alloc
            .text(ctx.token(&name))
            .append(alloc.text(" = "))
            .append(ty.to_doc(ctx))
    }
}

impl ToDoc for ast::TupleType {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let indent = ctx.config.indent_width as isize;
        block_list_auto(ctx, self.syntax(), "(", ")", ast::Type::cast, indent, true)
    }
}

impl ToDoc for ast::ArrayType {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        if !has_comment_tokens(self.syntax()) {
            let elem_ty = match self.elem_ty() {
                Some(t) => t.to_doc(ctx),
                None => return alloc.nil(),
            };
            let len = match self.len() {
                Some(l) => l.to_doc(ctx),
                None => return alloc.nil(),
            };

            return alloc
                .text("[")
                .append(elem_ty)
                .append(alloc.text("; "))
                .append(len)
                .append(alloc.text("]"));
        }

        let indent = ctx.config.indent_width as isize;
        token_doc(
            ctx,
            self.syntax(),
            indent,
            |node| {
                if let Some(ty) = ast::Type::cast(node.clone()) {
                    return Some(TokenPiece::new(ty.to_doc(ctx)));
                }
                ast::Expr::cast(node).map(|expr| TokenPiece::new(expr.to_doc(ctx)))
            },
            |token| match token.kind() {
                SyntaxKind::LBracket => Some(TokenPiece::new(alloc.text("["))),
                SyntaxKind::SemiColon => Some(TokenPiece::new(alloc.text(";")).space_after()),
                SyntaxKind::RBracket => Some(TokenPiece::new(alloc.text("]")).no_nest()),
                _ => None,
            },
        )
    }
}

impl ToDoc for ast::NeverType {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        ctx.alloc.text("!")
    }
}

// Forward declaration for expr::ToDoc - dispatches to specific expression types
impl ToDoc for ast::Expr {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        use parser::ast::ExprKind;
        match self.kind() {
            ExprKind::Lit(lit) => lit.to_doc(ctx),
            ExprKind::Block(block) => block.to_doc(ctx),
            ExprKind::Bin(bin) => bin.to_doc(ctx),
            ExprKind::Un(un) => un.to_doc(ctx),
            ExprKind::Cast(cast) => cast.to_doc(ctx),
            ExprKind::Call(call) => call.to_doc(ctx),
            ExprKind::MethodCall(method) => method.to_doc(ctx),
            ExprKind::Path(path) => path.to_doc(ctx),
            ExprKind::RecordInit(record) => record.to_doc(ctx),
            ExprKind::Field(field) => field.to_doc(ctx),
            ExprKind::Index(index) => index.to_doc(ctx),
            ExprKind::Tuple(tuple) => tuple.to_doc(ctx),
            ExprKind::Array(array) => array.to_doc(ctx),
            ExprKind::ArrayRep(array_rep) => array_rep.to_doc(ctx),
            ExprKind::If(if_expr) => if_expr.to_doc(ctx),
            ExprKind::Match(match_expr) => match_expr.to_doc(ctx),
            ExprKind::With(with_expr) => with_expr.to_doc(ctx),
            ExprKind::Paren(paren) => paren.to_doc(ctx),
            ExprKind::Assign(assign) => assign.to_doc(ctx),
            ExprKind::AugAssign(aug_assign) => aug_assign.to_doc(ctx),
        }
    }
}
