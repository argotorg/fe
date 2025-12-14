use common::InputDb;
pub use core::lower::{SelectorError, SelectorErrorKind, parse::ParserError};

pub mod analysis;
pub mod core;
pub mod diagnosable;
pub mod projection;
pub use core::{hir_def, lower, print, semantic, span, visitor};

pub mod test_db;

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
