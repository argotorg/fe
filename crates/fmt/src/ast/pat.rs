//! Formatting for patterns.

use pretty::RcDoc;

use crate::{RewriteContext, Shape};
use parser::ast::{self, PatKind};

use super::types::{ToDoc, block_list, block_list_spaced};

impl ToDoc for ast::Pat {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        match self.kind() {
            PatKind::WildCard(wildcard) => wildcard.to_doc(context, shape),
            PatKind::Rest(rest) => rest.to_doc(context, shape),
            PatKind::Lit(lit) => lit.to_doc(context, shape),
            PatKind::Tuple(tuple) => tuple.to_doc(context, shape),
            PatKind::Path(path) => path.to_doc(context, shape),
            PatKind::PathTuple(path_tuple) => path_tuple.to_doc(context, shape),
            PatKind::Record(record) => record.to_doc(context, shape),
            PatKind::Or(or) => or.to_doc(context, shape),
        }
    }
}

impl ToDoc for ast::WildCardPat {
    fn to_doc<'a>(&self, _context: &RewriteContext<'_>, _shape: Shape) -> RcDoc<'a, ()> {
        RcDoc::text("_")
    }
}

impl ToDoc for ast::RestPat {
    fn to_doc<'a>(&self, _context: &RewriteContext<'_>, _shape: Shape) -> RcDoc<'a, ()> {
        RcDoc::text("..")
    }
}

impl ToDoc for ast::LitPat {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, _shape: Shape) -> RcDoc<'a, ()> {
        match self.lit() {
            Some(l) => RcDoc::text(context.snippet_trimmed(&l)),
            None => RcDoc::nil(),
        }
    }
}

impl ToDoc for ast::TuplePat {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let elems: Vec<_> = self
            .elems()
            .into_iter()
            .flatten()
            .map(|e| e.to_doc(context, shape))
            .collect();

        let indent = context.config.indent_width as isize;
        block_list("(", ")", elems, indent, true)
    }
}

impl ToDoc for ast::PathPat {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let mut doc = RcDoc::nil();

        if self.mut_token().is_some() {
            doc = doc.append(RcDoc::text("mut "));
        }

        if let Some(path) = self.path() {
            doc = doc.append(path.to_doc(context, shape));
        }

        doc
    }
}

impl ToDoc for ast::PathTuplePat {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let path = match self.path() {
            Some(p) => p.to_doc(context, shape),
            None => return RcDoc::nil(),
        };

        let elems: Vec<_> = self
            .elems()
            .into_iter()
            .flatten()
            .map(|e| e.to_doc(context, shape))
            .collect();

        let indent = context.config.indent_width as isize;
        path.append(block_list("(", ")", elems, indent, true))
    }
}

impl ToDoc for ast::RecordPat {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let path = match self.path() {
            Some(p) => p.to_doc(context, shape),
            None => return RcDoc::nil(),
        };

        let fields: Vec<_> = self
            .fields()
            .map(|fields| {
                fields
                    .into_iter()
                    .filter_map(|field| {
                        let name = field.name()?;
                        let name_text = context.token(&name).to_string();

                        if let Some(pat) = field.pat() {
                            let pat_doc = pat.to_doc(context, shape);
                            Some(RcDoc::text(name_text).append(RcDoc::text(": ")).append(pat_doc))
                        } else {
                            Some(RcDoc::text(name_text))
                        }
                    })
                    .collect()
            })
            .unwrap_or_default();

        let indent = context.config.indent_width as isize;
        path.append(RcDoc::text(" ")).append(block_list_spaced("{", "}", fields, indent, true))
    }
}

impl ToDoc for ast::OrPat {
    fn to_doc<'a>(&self, context: &RewriteContext<'_>, shape: Shape) -> RcDoc<'a, ()> {
        let lhs = match self.lhs() {
            Some(l) => l.to_doc(context, shape),
            None => return RcDoc::nil(),
        };
        let rhs = match self.rhs() {
            Some(r) => r.to_doc(context, shape),
            None => return lhs,
        };

        lhs.append(RcDoc::text(" | ")).append(rhs)
    }
}
