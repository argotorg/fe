//! Formatting for patterns.

use crate::{ListFormatting, ListTactic, Rewrite, RewriteContext, RewriteExt, Shape, format_list};
use parser::ast::{self, PatKind};

impl Rewrite for ast::Pat {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        match self.kind() {
            PatKind::WildCard(wildcard) => wildcard.rewrite(context, shape),
            PatKind::Rest(rest) => rest.rewrite(context, shape),
            PatKind::Lit(lit) => lit.rewrite(context, shape),
            PatKind::Tuple(tuple) => tuple.rewrite(context, shape),
            PatKind::Path(path) => path.rewrite(context, shape),
            PatKind::PathTuple(path_tuple) => path_tuple.rewrite(context, shape),
            PatKind::Record(record) => record.rewrite(context, shape),
            PatKind::Or(or) => or.rewrite(context, shape),
        }
    }
}

impl Rewrite for ast::WildCardPat {
    fn rewrite(&self, _context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some("_".to_string())
    }
}

impl Rewrite for ast::RestPat {
    fn rewrite(&self, _context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some("..".to_string())
    }
}

impl Rewrite for ast::LitPat {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some(context.snippet_trimmed(&self.lit()?))
    }
}

impl Rewrite for ast::TuplePat {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .with_surround("(", ")");
        format_list(
            self.elems().into_iter().flatten(),
            &formatting,
            context,
            shape,
        )
    }
}

impl Rewrite for ast::PathPat {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        if self.mut_token().is_some() {
            out.push_str("mut ");
        }

        out.push_str(&self.path()?.rewrite_or_original(context, shape));

        Some(out)
    }
}

impl Rewrite for ast::PathTuplePat {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let path = self.path()?.rewrite_or_original(context, shape);
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .with_surround("(", ")");
        let elems_str = format_list(
            self.elems().into_iter().flatten(),
            &formatting,
            context,
            shape,
        )?;

        Some(format!("{path}{elems_str}"))
    }
}

impl Rewrite for ast::RecordPat {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let path = self.path()?.rewrite_or_original(context, shape);

        let fields_str = if let Some(fields) = self.fields() {
            let formatted_fields: Vec<String> = fields
                .into_iter()
                .filter_map(|field| {
                    let name = field.name()?;
                    let name_text = context.token(&name);

                    if let Some(pat) = field.pat() {
                        let pat_text = pat.rewrite_or_original(context, shape);
                        Some(format!("{name_text}: {pat_text}"))
                    } else {
                        Some(name_text.to_string())
                    }
                })
                .collect();

            format!(" {{ {} }}", formatted_fields.join(", "))
        } else {
            "{}".to_string()
        };

        Some(format!("{path}{fields_str}"))
    }
}

impl Rewrite for ast::OrPat {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let lhs = self.lhs()?.rewrite_or_original(context, shape);
        let rhs = self.rhs()?.rewrite_or_original(context, shape);

        Some(format!("{lhs} | {rhs}"))
    }
}
