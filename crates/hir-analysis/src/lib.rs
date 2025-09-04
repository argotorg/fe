use hir::{HirDb, span::DynLazySpan};
pub mod analysis_pass;
pub mod diagnostics;

#[salsa::db]
pub trait HirAnalysisDb: HirDb {}

#[salsa::db]
impl<T> HirAnalysisDb for T where T: HirDb {}

pub mod name_resolution;
pub mod ty;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<'db, T> {
    pub data: T,
    pub span: DynLazySpan<'db>,
}

impl<'db, T> Spanned<'db, T> {
    pub fn new(data: T, span: DynLazySpan<'db>) -> Self {
        Self { data, span }
    }
}
