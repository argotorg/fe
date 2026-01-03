use hir::analysis::HirAnalysisDb;
use hir::analysis::ty::corelib::resolve_lib_type_path;
use hir::analysis::ty::ty_def::TyId;
use hir::hir_def::scope_graph::ScopeId;

/// Target/core helper type resolution cached for MIR lowering.
///
/// This lives in MIR (not HIR) because it is backend-facing plumbing.
pub struct CoreLib<'db> {
    pub scope: ScopeId<'db>,
    effect_mem_ptr_ctor: TyId<'db>,
    effect_stor_ptr_ctor: TyId<'db>,
    effect_address_space_memory: TyId<'db>,
    effect_address_space_calldata: TyId<'db>,
    effect_address_space_storage: TyId<'db>,
    effect_address_space_transient_storage: TyId<'db>,
}

impl<'db> CoreLib<'db> {
    pub fn new(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Self {
        let effect_mem_ptr_ctor = resolve_lib_type_path(db, scope, "core::effect_ref::MemPtr")
            .unwrap_or_else(|| panic!("missing required core helper `core::effect_ref::MemPtr`"));
        let effect_stor_ptr_ctor = resolve_lib_type_path(db, scope, "core::effect_ref::StorPtr")
            .unwrap_or_else(|| panic!("missing required core helper `core::effect_ref::StorPtr`"));
        let effect_address_space_memory =
            resolve_lib_type_path(db, scope, "core::effect_ref::Memory").unwrap_or_else(|| {
                panic!("missing required core helper `core::effect_ref::Memory`")
            });
        let effect_address_space_calldata =
            resolve_lib_type_path(db, scope, "core::effect_ref::Calldata").unwrap_or_else(|| {
                panic!("missing required core helper `core::effect_ref::Calldata`")
            });
        let effect_address_space_storage =
            resolve_lib_type_path(db, scope, "core::effect_ref::Storage").unwrap_or_else(|| {
                panic!("missing required core helper `core::effect_ref::Storage`")
            });
        let effect_address_space_transient_storage =
            resolve_lib_type_path(db, scope, "core::effect_ref::TransientStorage").unwrap_or_else(
                || panic!("missing required core helper `core::effect_ref::TransientStorage`"),
            );

        Self {
            scope,
            effect_mem_ptr_ctor,
            effect_stor_ptr_ctor,
            effect_address_space_memory,
            effect_address_space_calldata,
            effect_address_space_storage,
            effect_address_space_transient_storage,
        }
    }

    pub fn effect_mem_ptr_ctor(&self) -> TyId<'db> {
        self.effect_mem_ptr_ctor
    }

    pub fn effect_stor_ptr_ctor(&self) -> TyId<'db> {
        self.effect_stor_ptr_ctor
    }

    pub fn effect_address_space_memory(&self) -> TyId<'db> {
        self.effect_address_space_memory
    }

    pub fn effect_address_space_calldata(&self) -> TyId<'db> {
        self.effect_address_space_calldata
    }

    pub fn effect_address_space_storage(&self) -> TyId<'db> {
        self.effect_address_space_storage
    }

    pub fn effect_address_space_transient_storage(&self) -> TyId<'db> {
        self.effect_address_space_transient_storage
    }
}
