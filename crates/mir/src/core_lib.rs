use common::ingot::IngotKind;
use hir::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, path_resolver::resolve_path},
    ty::{trait_resolution::PredicateListId, ty_def::TyId},
};
use hir::hir_def::{Body, IdentId, PathId};
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
        let resolve_ty = |segments| Self::resolve_core_type(db, body, segments);

        let allow_missing = matches!(body.top_mod(db).ingot(db).kind(db), IngotKind::Core);
        let mut tys = FxHashMap::default();
        for helper_ty in CoreHelperTy::all() {
            match resolve_ty(helper_ty.path_str()) {
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
        let segments: Vec<&str> = path.split("::").collect();
        match Self::resolve_core_path(db, body, &segments) {
            Some(PathRes::Ty(ty)) => Ok(ty),
            _ => Err(CoreLibError::MissingType(path.to_string())),
        }
    }

    /// Resolve a fully-qualified core path from the current body scope.
    ///
    /// * `db` - Analysis database used for name/type queries.
    /// * `body` - The body whose scope anchors path resolution.
    /// * `segments` - Path segments split on `"::"`.
    ///
    /// Returns the resolved [`PathRes`] if successful.
    fn resolve_core_path(
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        segments: &[&str],
    ) -> Option<PathRes<'db>> {
        let ingot_kind = body.top_mod(db).ingot(db).kind(db);
        let in_core_ingot = matches!(ingot_kind, IngotKind::Core);
        let in_std_ingot = matches!(ingot_kind, IngotKind::Std);

        let mut iter = segments.iter();
        let first = *iter.next()?;
        let mut path =
            PathId::from_ident(db, Self::make_ident(db, first, in_core_ingot, in_std_ingot));
        for segment in iter {
            path = path.push_ident(
                db,
                Self::make_ident(db, segment, in_core_ingot, in_std_ingot),
            );
        }
        resolve_path(
            db,
            path,
            body.scope(),
            PredicateListId::empty_list(db),
            true,
        )
        .ok()
    }

    /// Intern a path segment as an identifier, using `ingot` when lowering core itself.
    ///
    /// * `db` - Analysis database used for identifier interning.
    /// * `segment` - Path component text.
    /// * `in_core_ingot` - Whether the current body belongs to the core ingot.
    ///
    /// Returns an interned [`IdentId`] for the segment.
    fn make_ident(
        db: &'db dyn HirAnalysisDb,
        segment: &str,
        in_core_ingot: bool,
        in_std_ingot: bool,
    ) -> IdentId<'db> {
        if segment == "core" && in_core_ingot {
            IdentId::make_ingot(db)
        } else if segment == "core" {
            IdentId::make_core(db)
        } else if segment == "std" && in_std_ingot {
            IdentId::make_ingot(db)
        } else {
            IdentId::new(db, segment.to_string())
        }
    }
}
