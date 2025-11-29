use hir::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, path_resolver::resolve_path},
    ty::{
        trait_resolution::PredicateListId,
        ty_def::{TyBase, TyData, TyId},
    },
};
use hir::hir_def::{Body, CallableDef, IdentId, PathId};

/// Canonical core helper paths used during MIR lowering.
pub const PATH_STORE_DISCRIMINANT: &[&str] = &["core", "enum_repr", "store_discriminant"];
pub const PATH_GET_DISCRIMINANT: &[&str] = &["core", "enum_repr", "get_discriminant"];
pub const PATH_STORE_VARIANT_FIELD: &[&str] = &["core", "enum_repr", "store_variant_field"];
pub const PATH_GET_VARIANT_FIELD: &[&str] = &["core", "enum_repr", "get_variant_field"];
pub const PATH_GET_FIELD: &[&str] = &["core", "ptr", "get_field"];
pub const PATH_STORE_FIELD: &[&str] = &["core", "ptr", "store_field"];
pub const PATH_ALLOC: &[&str] = &["core", "mem", "alloc"];
pub const PATH_ADDRESS_SPACE: &[&str] = &["core", "ptr", "AddressSpace"];

/// Resolves and caches core helper functions and types used by MIR lowering.
pub struct CoreLib<'db> {
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    /// Cache for `core::ptr::get_field`.
    get_field_func: Option<CallableDef<'db>>,
    /// Cache for `core::ptr::store_field`.
    store_field_func: Option<CallableDef<'db>>,
    /// Cache for `core::mem::alloc`.
    alloc_func: Option<CallableDef<'db>>,
    /// Cache for `core::enum_repr::store_discriminant`.
    store_discriminant_func: Option<CallableDef<'db>>,
    /// Cache for `core::enum_repr::get_discriminant`.
    get_discriminant_func: Option<CallableDef<'db>>,
    /// Cache for `core::enum_repr::store_variant_field`.
    store_variant_field_func: Option<CallableDef<'db>>,
    /// Cache for `core::enum_repr::get_variant_field`.
    get_variant_field_func: Option<CallableDef<'db>>,
    /// Cache for `core::ptr::AddressSpace` type.
    address_space_ty: Option<TyId<'db>>,
}

impl<'db> CoreLib<'db> {
    /// Create a new resolver scoped to a HIR body (used for path resolution).
    pub fn new(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> Self {
        Self {
            db,
            body,
            get_field_func: None,
            store_field_func: None,
            alloc_func: None,
            store_discriminant_func: None,
            get_discriminant_func: None,
            store_variant_field_func: None,
            get_variant_field_func: None,
            address_space_ty: None,
        }
    }

    /// Returns the cached `core::enum_repr::store_discriminant` helper if found, resolving once on first use.
    ///
    /// *Returns*: `Some(CallableDef)` when the path resolves to a function, otherwise `None`.
    pub fn store_discriminant_func(&mut self) -> Option<CallableDef<'db>> {
        if self.store_discriminant_func.is_none() {
            self.store_discriminant_func = self.resolve_core_func(PATH_STORE_DISCRIMINANT);
        }
        self.store_discriminant_func
    }

    /// Returns the cached `core::enum_repr::get_discriminant` helper if found, resolving once on first use.
    ///
    /// *Returns*: `Some(CallableDef)` when the path resolves to a function, otherwise `None`.
    pub fn get_discriminant_func(&mut self) -> Option<CallableDef<'db>> {
        if self.get_discriminant_func.is_none() {
            self.get_discriminant_func = self.resolve_core_func(PATH_GET_DISCRIMINANT);
        }
        self.get_discriminant_func
    }

    /// Returns the cached `core::enum_repr::store_variant_field` helper if found, resolving once on first use.
    ///
    /// *Returns*: `Some(CallableDef)` when the path resolves to a function, otherwise `None`.
    pub fn store_variant_field_func(&mut self) -> Option<CallableDef<'db>> {
        if self.store_variant_field_func.is_none() {
            self.store_variant_field_func = self.resolve_core_func(PATH_STORE_VARIANT_FIELD);
        }
        self.store_variant_field_func
    }

    /// Returns the cached `core::enum_repr::get_variant_field` helper if found, resolving once on first use.
    ///
    /// *Returns*: `Some(CallableDef)` when the path resolves to a function, otherwise `None`.
    pub fn get_variant_field_func(&mut self) -> Option<CallableDef<'db>> {
        if self.get_variant_field_func.is_none() {
            self.get_variant_field_func = self.resolve_core_func(PATH_GET_VARIANT_FIELD);
        }
        self.get_variant_field_func
    }

    /// Returns the cached `core::ptr::get_field` helper if found, resolving once on first use.
    ///
    /// *Returns*: `Some(CallableDef)` when the path resolves to a function, otherwise `None`.
    pub fn get_field_func(&mut self) -> Option<CallableDef<'db>> {
        if self.get_field_func.is_none() {
            self.get_field_func = self.resolve_core_func(PATH_GET_FIELD);
        }
        self.get_field_func
    }

    /// Returns the cached `core::ptr::store_field` helper if found, resolving once on first use.
    ///
    /// *Returns*: `Some(CallableDef)` when the path resolves to a function, otherwise `None`.
    pub fn store_field_func(&mut self) -> Option<CallableDef<'db>> {
        if self.store_field_func.is_none() {
            self.store_field_func = self.resolve_core_func(PATH_STORE_FIELD);
        }
        self.store_field_func
    }

    /// Returns the cached `core::mem::alloc` helper if found, resolving once on first use.
    ///
    /// *Returns*: `Some(CallableDef)` when the path resolves to a function, otherwise `None`.
    pub fn alloc_func(&mut self) -> Option<CallableDef<'db>> {
        if self.alloc_func.is_none() {
            self.alloc_func = self.resolve_core_func(PATH_ALLOC);
        }
        self.alloc_func
    }

    /// Returns the cached `core::ptr::AddressSpace` type if found, resolving once on first use.
    ///
    /// *Returns*: `Some(TyId)` when the path resolves to a type, otherwise `None`.
    pub fn address_space_ty(&mut self) -> Option<TyId<'db>> {
        if self.address_space_ty.is_none()
            && let Some(ty) = self.resolve_core_type(PATH_ADDRESS_SPACE)
        {
            self.address_space_ty = Some(ty);
        }
        self.address_space_ty
    }

    /// Resolves a core function at a given path like `["core", "enum_repr", "get_discriminant"]`.
    ///
    /// *Arguments*: `segments` is the fully qualified module path split into string slices.
    /// *Returns*: `Some(CallableDef)` when the path resolves to a function, otherwise `None`.
    fn resolve_core_func(&self, segments: &[&str]) -> Option<CallableDef<'db>> {
        let PathRes::Func(func_ty) = self.resolve_core_path(segments)? else {
            return None;
        };
        let base = func_ty.base_ty(self.db);
        if let TyData::TyBase(TyBase::Func(func_def)) = base.data(self.db) {
            Some(*func_def)
        } else {
            None
        }
    }

    /// Resolves a core type at a given path like `PATH_ADDRESS_SPACE`.
    ///
    /// *Arguments*: `segments` is the fully qualified module path split into string slices.
    /// *Returns*: `Some(TyId)` when the path resolves to a type, otherwise `None`.
    fn resolve_core_type(&self, segments: &[&str]) -> Option<TyId<'db>> {
        let PathRes::Ty(ty) = self.resolve_core_path(segments)? else {
            return None;
        };
        Some(ty)
    }

    /// Resolves a fully-qualified core path like `["core", "ptr", "get_field"]` from the current body scope.
    ///
    /// *Arguments*: `segments` is the fully qualified module path split into string slices.
    /// *Returns*: `Some(PathRes)` when the path resolves successfully, otherwise `None`.
    fn resolve_core_path(&self, segments: &[&str]) -> Option<PathRes<'db>> {
        let mut iter = segments.iter();
        let first = *iter.next()?;
        let mut path = PathId::from_ident(self.db, self.make_ident(first));
        for segment in iter {
            path = path.push_ident(self.db, self.make_ident(segment));
        }
        resolve_path(
            self.db,
            path,
            self.body.scope(),
            PredicateListId::empty_list(self.db),
            true,
        )
        .ok()
    }

    /// Converts a string segment into an interned identifier, handling the special `core` ident.
    ///
    /// *Arguments*: `segment` is the module path component.
    /// *Returns*: Interned `IdentId` for the segment.
    fn make_ident(&self, segment: &str) -> IdentId<'db> {
        if segment == "core" {
            IdentId::make_core(self.db)
        } else {
            IdentId::new(self.db, segment.to_string())
        }
    }
}
