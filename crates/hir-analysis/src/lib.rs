use hir::{span::DynLazySpan, HirDb};

#[salsa::jar(db = HirAnalysisDb)]
pub struct Jar(
    /// Functions for import/name resolutions.
    name_resolution::resolve_path_early_impl,
    name_resolution::resolve_imports,
    name_resolution::diagnostics::NameResolutionDiagAccumulator,
    name_resolution::diagnostics::ImportResolutionDiagAccumulator,
    name_resolution::traits_in_scope::available_traits_in_scope_impl,
    name_resolution::traits_in_scope::TraitScope,
    /// Type system.
    ty::ty_def::TyId,
    ty::ty_def::ty_kind,
    ty::ty_def::pretty_print_ty,
    ty::ty_def::AdtDef,
    ty::ty_def::FuncDef,
    ty::ty_def::AdtRefId,
    // ty::ty_def::ty_generic_params,
    ty::ty_def::decompose_ty_app,
    /// Const types.
    ty::const_ty::ConstTyId,
    ty::const_ty::evaluate_const_ty,
    /// Type lowering.
    ty::ty_lower::lower_hir_ty,
    ty::ty_lower::lower_adt,
    ty::ty_lower::lower_func,
    ty::ty_lower::lower_type_alias,
    ty::ty_lower::collect_generic_params,
    ty::ty_lower::GenericParamOwnerId,
    ty::ty_lower::GenericParamTypeSet,
    ty::ty_lower::evaluate_params_precursor,
    /// Trait lowering.
    ty::trait_lower::lower_trait,
    ty::trait_lower::lower_trait_ref,
    ty::trait_lower::collect_trait_impls,
    ty::trait_lower::lower_impl_trait,
    ty::trait_lower::collect_implementor_methods,
    /// Method collection.
    ty::method_table::collect_methods,
    /// Item Definition analysis.
    ty::def_analysis::check_recursive_adt,
    ty::def_analysis::analyze_adt,
    ty::def_analysis::analyze_type_alias,
    ty::def_analysis::analyze_trait,
    ty::def_analysis::analyze_impl,
    ty::def_analysis::analyze_impl_trait,
    ty::def_analysis::analyze_func,
    /// Trait system.
    ty::trait_def::TraitDef,
    ty::trait_def::TraitInstId,
    ty::trait_def::Implementor,
    ty::trait_def::ingot_trait_env,
    ty::trait_def::impls_of_trait,
    ty::trait_def::impls_of_ty,
    ty::constraint::collect_super_traits,
    ty::constraint::collect_trait_constraints,
    ty::constraint::collect_adt_constraints,
    ty::constraint::collect_implementor_constraints,
    ty::constraint::collect_impl_block_constraints,
    ty::constraint::collect_func_def_constraints,
    ty::constraint::collect_func_def_constraints_impl,
    ty::constraint::ty_constraints,
    ty::constraint::PredicateId,
    ty::constraint::PredicateListId,
    ty::constraint_solver::is_goal_satisfiable,
    ty::constraint_solver::check_ty_wf,
    ty::constraint_solver::check_trait_inst_wf,
    /// Type checking.
    ty::ty_check::check_func_body,
    /// Diagnostic accumulators.
    ty::diagnostics::AdtDefDiagAccumulator,
    ty::diagnostics::TypeAliasDefDiagAccumulator,
    ty::diagnostics::TraitDefDiagAccumulator,
    ty::diagnostics::ImplTraitDefDiagAccumulator,
    ty::diagnostics::ImplDefDiagAccumulator,
    ty::diagnostics::FuncDefDiagAccumulator,
    ty::diagnostics::FuncBodyDiagAccumulator,
);

pub trait HirAnalysisDb: salsa::DbWithJar<Jar> + HirDb {
    fn as_hir_analysis_db(&self) -> &dyn HirAnalysisDb {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db::<'_>(self)
    }
}
impl<DB> HirAnalysisDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> + HirDb {}

pub mod name_resolution;
pub mod ty;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub data: T,
    pub span: DynLazySpan,
}
