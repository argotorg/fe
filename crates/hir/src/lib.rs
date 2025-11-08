use common::InputDb;
pub use core::lower::parse::ParserError;

pub mod analysis;
pub mod core;
// Re-export core modules at crate root for compatibility
pub use core::{hir_def, lower, span, visitor};

pub use common::{file::File, file::Workspace, ingot::Ingot};
#[salsa::db]
pub trait HirDb: salsa::Database + InputDb {}

#[salsa::db]
impl<T> HirDb for T where T: salsa::Database + InputDb {}

/// `LowerHirDb` is a marker trait for lowering AST to HIR items.
/// All code that requires [`LowerHirDb`] is considered have a possibility to
/// invalidate the cache in salsa when a revision is updated. Therefore,
/// implementations relying on `LowerHirDb` are prohibited in all
/// Analysis phases.
#[salsa::db]
pub trait LowerHirDb: salsa::Database + HirDb {}
#[salsa::db]
impl<T> LowerHirDb for T where T: HirDb {}

/// `SpannedHirDb` is a marker trait for extracting span-dependent information
/// from HIR Items.
/// All code that requires [`SpannedHirDb`] is considered have a possibility to
/// invalidate the cache in salsa when a revision is updated. Therefore,
/// implementations relying on `SpannedHirDb` are prohibited in all
/// Analysis phases.
///
/// This marker is mainly used to inject [HirOrigin](crate::core::span::HirOrigin) to
/// generate [CompleteDiagnostic](common::diagnostics::CompleteDiagnostic) from
/// [DiagnosticVoucher](crate::diagnostics::DiagnosticVoucher).
/// See also `[LazySpan]`[`crate::core::span::LazySpan`] for more details.
#[salsa::db]
pub trait SpannedHirDb: salsa::Database + HirDb {}
#[salsa::db]
impl<T> SpannedHirDb for T where T: HirDb {}

#[cfg(test)]
mod test_db {
    use common::{InputDb, define_input_db, file::File};
    use derive_more::TryIntoError;
    use url::Url;

    use crate::{
        hir_def::{ItemKind, TopLevelMod, scope_graph::ScopeGraph},
        lower::{map_file_to_mod, scope_graph},
        span::LazySpan,
    };

    // Use the macro to define our test database with Workspace support
    define_input_db!(TestDb);

    impl TestDb {
        pub fn parse_source(&self, file: File) -> &ScopeGraph<'_> {
            let top_mod = map_file_to_mod(self, file);
            scope_graph(self, top_mod)
        }

        /// Parses the given source text and returns the first inner item in the
        /// file.
        pub fn expect_item<'db, T>(&'db self, file: File) -> T
        where
            ItemKind<'db>: TryInto<T, Error = TryIntoError<ItemKind<'db>>>,
        {
            let tree = self.parse_source(file);
            tree.items_dfs(self)
                .find_map(|it| it.try_into().ok())
                .unwrap()
        }

        pub fn expect_items<'db, T>(&'db self, file: File) -> Vec<T>
        where
            ItemKind<'db>: TryInto<T, Error = TryIntoError<ItemKind<'db>>>,
        {
            let tree = self.parse_source(file);
            tree.items_dfs(self)
                .filter_map(|it| it.try_into().ok())
                .collect()
        }

        pub fn text_at(&self, top_mod: TopLevelMod, span: &impl LazySpan) -> &str {
            let range = span.resolve(self).unwrap().range;
            let file = top_mod.file(self);
            let text = file.text(self);
            &text[range.start().into()..range.end().into()]
        }

        pub fn standalone_file(&mut self, text: &str) -> File {
            self.workspace().touch(
                self,
                Url::parse("file:///hir_test/test_file.fe").unwrap(),
                Some(text.into()),
            )
        }
    }
}
