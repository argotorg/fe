//! The item a name or a generated member is, described without ids that belong
//! to one database.
//!
//! Preparation reports a generated item that is hidden at its request, and the
//! report names the item and what hides it. A [`UseTarget`] says where an item
//! is written, or for a generated item which ingot it was published into and
//! where the provider's quotation wrote it, so the report can point at the
//! request.

use common::diagnostics::Span;

use crate::{
    SpannedHirDb,
    analysis::{HirAnalysisDb, diagnostics::SpannedHirAnalysisDb},
    hir_def::scope_graph::ScopeId,
    span::LazySpan,
};

/// The function, constant or other item a use or a name resolves to.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UseTarget {
    /// An item written in source, identified by where it is declared.
    Declared {
        file: url::Url,
        range: parser::TextRange,
        name: String,
    },
    /// An item that preparation published. It is identified by the ingot it
    /// was published into and by where its text is written, the quotation of
    /// the provider. A dependency's sealed item and an item the ingot
    /// generates itself differ in the ingot, even when they share a name.
    Generated {
        name: String,
        ingot: url::Url,
        source: Option<(url::Url, parser::TextRange)>,
    },
}

/// Whether `scope` is, or lies inside, an item that preparation published.
fn is_generated(db: &dyn HirAnalysisDb, scope: ScopeId<'_>) -> bool {
    scope.is_in_published_item(db)
}

fn declared(span: Option<Span>, db: &dyn SpannedHirDb, name: String) -> Option<UseTarget> {
    let span = span?;
    Some(UseTarget::Declared {
        file: span.file.url(db)?,
        range: span.range,
        name,
    })
}

/// The target for the item at `scope`, written at `span`: generated when
/// preparation published it, else declared where it is written.
pub(crate) fn target_at<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    scope: ScopeId<'db>,
    span: Option<Span>,
    name: String,
) -> Option<UseTarget> {
    if is_generated(db, scope) {
        Some(UseTarget::Generated {
            name,
            ingot: scope.ingot(db).base(db).clone(),
            source: span.and_then(|span| Some((span.file.url(db)?, span.range))),
        })
    } else {
        declared(span, db, name)
    }
}

/// The target for the item `scope` named `name`, identified by its name.
pub fn item_target<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    scope: ScopeId<'db>,
    name: String,
) -> Option<UseTarget> {
    let span = scope.name_span(db).and_then(|span| span.resolve(db));
    target_at(db, scope, span, name)
}
