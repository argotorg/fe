//! Formatting for patterns.

use pretty::DocAllocator;

use crate::RewriteContext;
use parser::ast::{self, PatKind};

use super::types::{Doc, ToDoc, block_list, block_list_spaced};

impl ToDoc for ast::Pat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.kind() {
            PatKind::WildCard(wildcard) => wildcard.to_doc(ctx),
            PatKind::Rest(rest) => rest.to_doc(ctx),
            PatKind::Lit(lit) => lit.to_doc(ctx),
            PatKind::Tuple(tuple) => tuple.to_doc(ctx),
            PatKind::Path(path) => path.to_doc(ctx),
            PatKind::PathTuple(path_tuple) => path_tuple.to_doc(ctx),
            PatKind::Record(record) => record.to_doc(ctx),
            PatKind::Or(or) => or.to_doc(ctx),
        }
    }
}

impl ToDoc for ast::WildCardPat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        ctx.alloc.text("_")
    }
}

impl ToDoc for ast::RestPat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        ctx.alloc.text("..")
    }
}

impl ToDoc for ast::LitPat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        match self.lit() {
            Some(l) => ctx.alloc.text(ctx.snippet_trimmed(&l)),
            None => ctx.alloc.nil(),
        }
    }
}

impl ToDoc for ast::TuplePat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let elems: Vec<_> = self
            .elems()
            .into_iter()
            .flatten()
            .map(|e| e.to_doc(ctx))
            .collect();

        let indent = ctx.config.indent_width as isize;
        block_list(ctx, "(", ")", elems, indent, true)
    }
}

impl ToDoc for ast::PathPat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;
        let mut doc = alloc.nil();

        if self.mut_token().is_some() {
            doc = doc.append(alloc.text("mut "));
        }

        if let Some(path) = self.path() {
            doc = doc.append(path.to_doc(ctx));
        }

        doc
    }
}

impl ToDoc for ast::PathTuplePat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let path = match self.path() {
            Some(p) => p.to_doc(ctx),
            None => return alloc.nil(),
        };

        let elems: Vec<_> = self
            .elems()
            .into_iter()
            .flatten()
            .map(|e| e.to_doc(ctx))
            .collect();

        let indent = ctx.config.indent_width as isize;
        path.append(block_list(ctx, "(", ")", elems, indent, true))
    }
}

impl ToDoc for ast::RecordPat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let path = match self.path() {
            Some(p) => p.to_doc(ctx),
            None => return alloc.nil(),
        };

        let fields: Vec<_> = self
            .fields()
            .map(|fields| {
                fields
                    .into_iter()
                    .filter_map(|field| {
                        let name = field.name()?;
                        let name_text = ctx.token(&name).to_string();

                        if let Some(pat) = field.pat() {
                            let pat_doc = pat.to_doc(ctx);
                            Some(
                                alloc
                                    .text(name_text)
                                    .append(alloc.text(": "))
                                    .append(pat_doc),
                            )
                        } else {
                            Some(alloc.text(name_text))
                        }
                    })
                    .collect()
            })
            .unwrap_or_default();

        let indent = ctx.config.indent_width as isize;
        path.append(alloc.text(" "))
            .append(block_list_spaced(ctx, "{", "}", fields, indent, true))
    }
}

impl ToDoc for ast::OrPat {
    fn to_doc<'a>(&self, ctx: &'a RewriteContext<'a>) -> Doc<'a> {
        let alloc = &ctx.alloc;

        let lhs = match self.lhs() {
            Some(l) => l.to_doc(ctx),
            None => return alloc.nil(),
        };
        let rhs = match self.rhs() {
            Some(r) => r.to_doc(ctx),
            None => return lhs,
        };

        lhs.append(alloc.text(" | ")).append(rhs)
    }
}
