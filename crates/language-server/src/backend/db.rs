use common::InputDb;

use hir::{HirDb, LowerHirDb, SpannedHirDb};
use hir::analysis::{diagnostics::SpannedHirAnalysisDb, HirAnalysisDb};

#[salsa::db]
pub trait LanguageServerDb:
    salsa::Database + SpannedHirAnalysisDb + HirAnalysisDb + HirDb + LowerHirDb + SpannedHirDb + InputDb
{
}

#[salsa::db]
impl<DB> LanguageServerDb for DB where
    DB: Sized
        + salsa::Database
        + SpannedHirAnalysisDb
        + HirAnalysisDb
        + HirDb
        + LowerHirDb
        + SpannedHirDb
        + InputDb
{
}

#[salsa::db]
#[derive(Default, Clone)]
pub struct LanguageServerDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for LanguageServerDatabase {
    fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
}
