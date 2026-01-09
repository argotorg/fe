use common::ingot::IngotKind;
use hir::analysis::ty::corelib;
use hir::analysis::{HirAnalysisDb, ty::ty_def::TyId};
use hir::hir_def::Body;
use rustc_hash::FxHashMap;

/// Core helper type resolution for MIR lowering.
///
/// `CoreLib` eagerly resolves the core helper types MIR lowering depends on.
/// Errors surfaced when the core library cannot be resolved.
#[derive(Debug)]
pub enum CoreLibError {
    MissingFunc(String),
    MissingType(String),
}

/// Core helper types used during MIR lowering.
#[allow(clippy::enum_variant_names)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CoreHelperTy {
    EffectMemPtr,
    EffectStorPtr,
    EffectCalldataPtr,
}

impl CoreHelperTy {
    /// Returns all helper types for eager resolution.
    ///
    /// Takes no parameters and returns a slice containing every [`CoreHelperTy`] variant.
    pub const fn all() -> &'static [CoreHelperTy] {
        &[
            CoreHelperTy::EffectMemPtr,
            CoreHelperTy::EffectStorPtr,
            CoreHelperTy::EffectCalldataPtr,
        ]
    }

    /// Fully qualified path string for this helper type.
    ///
    /// * `self` - Helper type whose path should be returned.
    ///
    /// Returns the path string used when resolving the helper type.
    pub const fn path_str(self) -> &'static str {
        match self {
            CoreHelperTy::EffectMemPtr => "std::evm::effects::MemPtr",
            CoreHelperTy::EffectStorPtr => "std::evm::effects::StorPtr",
            CoreHelperTy::EffectCalldataPtr => "std::evm::effects::CalldataPtr",
        }
    }
}

/// Resolves and caches core helper functions and types used by MIR lowering.
pub struct CoreLib<'db> {
    /// Resolved helper types keyed by their enum variant.
    tys: FxHashMap<CoreHelperTy, TyId<'db>>,
}

impl<'db> CoreLib<'db> {
    /// Create a new resolver scoped to a HIR body (used for path resolution).
    ///
    /// * `db` - Analysis database used for name/type queries.
    /// * `body` - The body whose scope anchors path resolution.
    ///
    /// Returns a fully-populated [`CoreLib`] when all helpers resolve, or a
    /// [`CoreLibError`] indicating which helper is missing.
    pub fn new(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> Result<Self, CoreLibError> {
        let allow_missing = matches!(body.top_mod(db).ingot(db).kind(db), IngotKind::Core);
        let mut tys = FxHashMap::default();
        for helper_ty in CoreHelperTy::all() {
            match Self::resolve_core_type(db, body, helper_ty.path_str()) {
                Ok(ty) => {
                    tys.insert(*helper_ty, ty);
                }
                Err(_err) if allow_missing => {}
                Err(err) => return Err(err),
            }
        }

        Ok(Self { tys })
    }

    /// Look up a previously resolved core helper type.
    ///
    /// * `self` - Library containing resolved core helper types.
    /// * `key` - Which helper type to retrieve (e.g. `MemPtr`).
    ///
    /// Returns the resolved [`TyId`] for the requested helper type.
    pub fn helper_ty(&self, key: CoreHelperTy) -> Option<TyId<'db>> {
        self.tys.get(&key).copied()
    }

    /// Resolve a core type given a fully-qualified path string.
    ///
    /// * `db` - Analysis database used for name/type queries.
    /// * `body` - The body whose scope anchors path resolution.
    /// * `path` - Fully-qualified path string (e.g. `"core::ptr::MemPtr"`).
    ///
    /// Returns the `TyId` on success or a [`CoreLibError::MissingType`] if resolution fails.
    fn resolve_core_type(
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        path: &str,
    ) -> Result<TyId<'db>, CoreLibError> {
        corelib::resolve_lib_type_path(db, body, path)
            .ok_or_else(|| CoreLibError::MissingType(path.to_string()))
    }
}
