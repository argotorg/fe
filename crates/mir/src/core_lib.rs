use hir::analysis::HirAnalysisDb;
use hir::analysis::ty::corelib::resolve_lib_type_path;
use hir::analysis::ty::ty_def::TyId;
use hir::hir_def::scope_graph::ScopeId;

/// Target/core helper type resolution cached for MIR lowering.
///
/// This lives in MIR (not HIR) because it is backend-facing plumbing.
pub struct CoreLib<'db> {
    pub scope: ScopeId<'db>,
    pub mem_ptr_ctor: TyId<'db>,
    pub stor_ptr_ctor: TyId<'db>,
    pub addr_space_mem: TyId<'db>,
    pub addr_space_calldata: TyId<'db>,
    pub addr_space_stor: TyId<'db>,
    pub addr_space_transient: TyId<'db>,
}

impl<'db> CoreLib<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Self {
        let mem_ptr_ctor = resolve_lib_type_path(db, scope, "core::effect_ref::MemPtr")
            .unwrap_or_else(|| panic!("missing required core helper `core::effect_ref::MemPtr`"));
        let stor_ptr_ctor = resolve_lib_type_path(db, scope, "core::effect_ref::StorPtr")
            .unwrap_or_else(|| panic!("missing required core helper `core::effect_ref::StorPtr`"));
        let addr_space_mem = resolve_lib_type_path(db, scope, "core::effect_ref::Memory")
            .unwrap_or_else(|| panic!("missing required core helper `core::effect_ref::Memory`"));
        let addr_space_calldata = resolve_lib_type_path(db, scope, "core::effect_ref::Calldata")
            .unwrap_or_else(|| panic!("missing required core helper `core::effect_ref::Calldata`"));
        let addr_space_stor = resolve_lib_type_path(db, scope, "core::effect_ref::Storage")
            .unwrap_or_else(|| panic!("missing required core helper `core::effect_ref::Storage`"));
        let addr_space_transient =
            resolve_lib_type_path(db, scope, "core::effect_ref::TransientStorage").unwrap_or_else(
                || panic!("missing required core helper `core::effect_ref::TransientStorage`"),
            );

        Self {
            scope,
            mem_ptr_ctor,
            stor_ptr_ctor,
            addr_space_mem,
            addr_space_calldata,
            addr_space_stor,
            addr_space_transient,
        }
    }
}
