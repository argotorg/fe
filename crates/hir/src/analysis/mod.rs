use crate::{HirDb, span::DynLazySpan};
pub mod analysis_pass;
pub mod core_requirements;
pub mod diagnostics;
pub mod place;

use self::analysis_pass::{AnalysisPassManager, EventLowerPass, MsgLowerPass, ParsingPass};
use self::name_resolution::ImportAnalysisPass;
use self::ty::{
    AdtDefAnalysisPass, BodyAnalysisPass, ContractAnalysisPass, DefConflictAnalysisPass,
    FuncAnalysisPass, ImplAnalysisPass, ImplTraitAnalysisPass, MsgSelectorAnalysisPass,
    TraitAnalysisPass, TypeAliasAnalysisPass,
};

#[salsa::db]
pub trait HirAnalysisDb: HirDb {}

#[salsa::db]
impl<T> HirAnalysisDb for T where T: HirDb {}

pub mod name_resolution;
pub mod ty;

pub fn initialize_analysis_pass() -> AnalysisPassManager {
    let mut pass_manager = AnalysisPassManager::new();
    pass_manager.add_module_pass("Parsing", Box::new(ParsingPass {}));
    pass_manager.add_module_pass("MsgLower", Box::new(MsgLowerPass {}));
    pass_manager.add_module_pass("EventLower", Box::new(EventLowerPass {}));
    pass_manager.add_module_pass("MsgSelector", Box::new(MsgSelectorAnalysisPass {}));
    pass_manager.add_module_pass("DefConflict", Box::new(DefConflictAnalysisPass {}));
    pass_manager.add_module_pass("Import", Box::new(ImportAnalysisPass {}));
    pass_manager.add_module_pass("AdtDef", Box::new(AdtDefAnalysisPass {}));
    pass_manager.add_module_pass("TypeAlias", Box::new(TypeAliasAnalysisPass {}));
    pass_manager.add_module_pass("Trait", Box::new(TraitAnalysisPass {}));
    pass_manager.add_module_pass("Impl", Box::new(ImplAnalysisPass {}));
    pass_manager.add_module_pass("ImplTrait", Box::new(ImplTraitAnalysisPass {}));
    pass_manager.add_module_pass("Func", Box::new(FuncAnalysisPass {}));
    pass_manager.add_module_pass("Body", Box::new(BodyAnalysisPass {}));
    pass_manager.add_module_pass("Contract", Box::new(ContractAnalysisPass {}));
    pass_manager
}

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
