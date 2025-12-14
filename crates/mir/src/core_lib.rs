use common::ingot::IngotKind;
use hir::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, path_resolver::resolve_path},
    ty::{
        trait_resolution::PredicateListId,
        ty_check::Callable,
        ty_def::{TyBase, TyData, TyId},
    },
};
use hir::hir_def::{Body, CallableDef, ExprId, IdentId, PathId};
use rustc_hash::FxHashMap;

/// Core helper resolution and callable construction for MIR lowering.
///
/// `CoreLib` eagerly resolves the handful of core functions/types MIR lowering depends on,
/// then builds `Callable`s for those helpers on demand. The enums below are the single
/// source of truth for what constitutes a “core helper”.
/// Errors surfaced when the core library cannot be resolved.
#[derive(Debug)]
pub enum CoreLibError {
    MissingFunc(String),
    MissingType(String),
}

/// Core helper enum/path definitions. Add new helpers or types here; everything else
/// (resolution, call construction) stays in sync automatically.
macro_rules! define_core_helpers {
    (
        helpers { $($h_variant:ident => $h_path:literal,)+ }
        types { $($t_variant:ident => $t_path:literal,)+ }
    ) => {
        /// Core helper functions used during MIR lowering.
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum CoreHelper { $($h_variant,)+ }

        impl CoreHelper {
            /// Returns all helper variants for eager resolution.
            ///
            /// Takes no parameters and returns a slice containing every [`CoreHelper`] variant.
            pub const fn all() -> &'static [CoreHelper] {
                &[$(CoreHelper::$h_variant,)+]
            }

            /// Fully qualified path string for this helper (e.g. `"core::ptr::get_field"`).
            ///
            /// * `self` - Helper variant whose path should be returned.
            ///
            /// Returns the path string used when resolving the helper function.
            pub const fn path_str(self) -> &'static str {
                match self {
                    $(CoreHelper::$h_variant => $h_path,)+
                }
            }
        }

        /// Core helper types used during MIR lowering.
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum CoreHelperTy { $($t_variant,)+ }

        impl CoreHelperTy {
            /// Returns all helper types for eager resolution.
            ///
            /// Takes no parameters and returns a slice containing every [`CoreHelperTy`] variant.
            pub const fn all() -> &'static [CoreHelperTy] {
                &[$(CoreHelperTy::$t_variant,)+]
            }

            /// Fully qualified path string for this helper type.
            ///
            /// * `self` - Helper type whose path should be returned.
            ///
            /// Returns the path string used when resolving the helper type.
            pub const fn path_str(self) -> &'static str {
                match self {
                    $(CoreHelperTy::$t_variant => $t_path,)+
                }
            }
        }
    };
}

define_core_helpers! {
    helpers {
        GetField => "core::ptr::get_field",
        StoreField => "core::ptr::store_field",
        Alloc => "core::mem::alloc",
        StoreDiscriminant => "core::enum_repr::store_discriminant",
        GetDiscriminant => "core::enum_repr::get_discriminant",
        StoreVariantField => "core::enum_repr::store_variant_field",
        GetVariantField => "core::enum_repr::get_variant_field",
    }
    types {
        MemPtr => "core::ptr::MemPtr",
        StorPtr => "core::ptr::StorPtr",
    }
}

/// Resolves and caches core helper functions and types used by MIR lowering.
pub struct CoreLib<'db> {
    /// Database used for all HIR/ty queries during resolution and callable construction.
    db: &'db dyn HirAnalysisDb,
    /// Body whose scope anchors path resolution.
    body: Body<'db>,
    /// Resolved helper functions keyed by their enum variant.
    funcs: FxHashMap<CoreHelper, CallableDef<'db>>,
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
        let resolve_func = |segments| Self::resolve_core_func(db, body, segments);
        let resolve_ty = |segments| Self::resolve_core_type(db, body, segments);

        let mut funcs = FxHashMap::default();
        for helper in CoreHelper::all() {
            funcs.insert(*helper, resolve_func(helper.path_str())?);
        }

        let mut tys = FxHashMap::default();
        for helper_ty in CoreHelperTy::all() {
            tys.insert(*helper_ty, resolve_ty(helper_ty.path_str())?);
        }

        Ok(Self {
            db,
            body,
            funcs,
            tys,
        })
    }

    /// Build a callable for a core helper with optional generic instantiation.
    ///
    /// * `self` - Library containing resolved core helper definitions.
    /// * `expr` - Expression whose span should be attached to diagnostics.
    /// * `helper` - Which core helper function to target.
    /// * `generics` - Concrete type arguments for generic helpers (can be empty).
    ///
    /// Returns a fully constructed [`Callable`] for use in MIR lowering.
    pub fn make_callable(
        &self,
        expr: ExprId,
        helper: CoreHelper,
        generics: &[TyId<'db>],
    ) -> Callable<'db> {
        let func_def = *self
            .funcs
            .get(&helper)
            .expect("core helper should be resolved at construction");
        let base = TyId::func(self.db, func_def);
        let ty = if generics.is_empty() {
            base
        } else {
            TyId::foldl(self.db, base, generics)
        };
        Callable::new(self.db, ty, expr.span(self.body).into(), None)
            .expect("core helper callable should be valid")
    }

    /// Look up a previously resolved core helper type.
    ///
    /// * `self` - Library containing resolved core helper types.
    /// * `key` - Which helper type to retrieve (e.g. `MemPtr`).
    ///
    /// Returns the resolved [`TyId`] for the requested helper type.
    pub fn helper_ty(&self, key: CoreHelperTy) -> TyId<'db> {
        *self
            .tys
            .get(&key)
            .expect("core helper type should be resolved at construction")
    }

    /// Resolve a core function given a fully-qualified path string.
    ///
    /// * `db` - Analysis database used for name/type queries.
    /// * `body` - The body whose scope anchors path resolution.
    /// * `path` - Fully-qualified path string (e.g. `"core::ptr::get_field"`).
    ///
    /// Returns the `CallableDef` on success or a [`CoreLibError::MissingFunc`]
    /// when the path cannot be resolved to a function.
    fn resolve_core_func(
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        path: &str,
    ) -> Result<CallableDef<'db>, CoreLibError> {
        let segments: Vec<&str> = path.split("::").collect();
        let PathRes::Func(func_ty) = Self::resolve_core_path(db, body, &segments)
            .ok_or_else(|| CoreLibError::MissingFunc(path.to_string()))?
        else {
            return Err(CoreLibError::MissingFunc(path.to_string()));
        };
        let base = func_ty.base_ty(db);
        if let TyData::TyBase(TyBase::Func(func_def)) = base.data(db) {
            Ok(*func_def)
        } else {
            Err(CoreLibError::MissingFunc(path.to_string()))
        }
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
        let in_core_ingot = matches!(body.top_mod(db).ingot(db).kind(db), IngotKind::Core);

        let mut iter = segments.iter();
        let first = *iter.next()?;
        let mut path = PathId::from_ident(db, Self::make_ident(db, first, in_core_ingot));
        for segment in iter {
            path = path.push_ident(db, Self::make_ident(db, segment, in_core_ingot));
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
    fn make_ident(db: &'db dyn HirAnalysisDb, segment: &str, in_core_ingot: bool) -> IdentId<'db> {
        if segment == "core" && in_core_ingot {
            IdentId::make_ingot(db)
        } else if segment == "core" {
            IdentId::make_core(db)
        } else {
            IdentId::new(db, segment.to_string())
        }
    }
}
