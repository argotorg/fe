// TODO: Remove this when https://github.com/salsa-rs/salsa/pull/513 is fixed.
#![allow(clippy::unused_unit)]
pub mod attr;
pub mod body;
pub mod expr;
pub mod ident;
pub mod item;
pub mod params;
pub mod pat;
pub mod path;
pub mod prim_ty;
pub mod scope_graph;
pub mod stmt;
pub mod types;
pub mod use_tree;

mod scope_graph_viz;

pub(crate) mod module_tree;

pub use attr::*;
pub use body::*;
use common::ingot::{Ingot, IngotKind};
pub use expr::*;
pub use ident::*;
pub use item::*;
pub use module_tree::*;
use num_bigint::BigUint;
pub use params::*;
pub use pat::*;
pub use path::*;
use salsa::Update;
pub use stmt::*;
pub use types::*;
pub use use_tree::*;

use crate::HirDb;

pub trait HirIngot<'db> {
    fn module_tree(self, db: &'db dyn HirDb) -> &'db ModuleTree<'db>;
    fn all_modules(self, db: &'db dyn HirDb) -> &'db Vec<TopLevelMod<'db>>;
    fn root_mod(self, db: &'db dyn HirDb) -> TopLevelMod<'db>;
    fn resolved_external_ingots(self, db: &'db dyn HirDb) -> &'db Vec<(IdentId<'db>, Ingot<'db>)>;
    fn all_enums(self, db: &'db dyn HirDb) -> &'db Vec<Enum<'db>>;
    fn all_impl_traits(self, db: &'db dyn HirDb) -> &'db Vec<ImplTrait<'db>>;
    fn all_impls(self, db: &'db dyn HirDb) -> &'db Vec<Impl<'db>>;
    fn is_core(&self, db: &'db dyn HirDb) -> bool;
}

#[salsa::tracked]
impl<'db> HirIngot<'db> for Ingot<'db> {
    /// Returns a module tree of the given ingot. The resulted tree only includes
    /// top level modules. This function only depends on an ingot structure and
    /// external ingot dependency, and not depends on file contents.
    fn module_tree(self, db: &'db dyn HirDb) -> &'db ModuleTree<'db> {
        module_tree_impl(db, self)
    }

    #[salsa::tracked(return_ref)]
    fn all_modules(self, db: &'db dyn HirDb) -> Vec<TopLevelMod<'db>> {
        self.module_tree(db).all_modules().collect()
    }

    fn root_mod(self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        self.module_tree(db).root_data().top_mod
    }

    /// Returns the root modules and names of external ingots that the given `ingot`
    /// depends on.
    /// From the outside of the crate, this functionality can be accessed via
    /// [`TopLevelMod::external_ingots`](crate::TopLevelMod::external_ingots).
    // The reason why this function is not a public API is that we want to prohibit users of `HirDb` to
    // access `InputIngot` directly.
    #[salsa::tracked(return_ref)]
    fn resolved_external_ingots(self, db: &'db dyn HirDb) -> Vec<(IdentId<'db>, Ingot<'db>)> {
        self.dependencies(db)
            .iter()
            .filter_map(|(name, url)| {
                db.workspace()
                    .containing_ingot(db, url.clone())
                    .map(|ingot_description| (IdentId::new(db, name.as_str()), ingot_description))
            })
            .collect()
    }

    #[salsa::tracked(return_ref)]
    fn all_enums(self, db: &'db dyn HirDb) -> Vec<Enum<'db>> {
        self.all_modules(db)
            .iter()
            .flat_map(|top_mod| {
                let enums = top_mod.all_enums(db);
                enums.to_vec()
            })
            .collect()
    }

    #[salsa::tracked(return_ref)]
    fn all_impl_traits(self, db: &'db dyn HirDb) -> Vec<ImplTrait<'db>> {
        self.all_modules(db)
            .iter()
            .flat_map(|top_mod| {
                let impl_traits = top_mod.all_impl_traits(db);
                impl_traits.to_vec()
            })
            .collect()
    }

    #[salsa::tracked(return_ref)]
    fn all_impls(self, db: &'db dyn HirDb) -> Vec<Impl<'db>> {
        self.all_modules(db)
            .iter()
            .flat_map(|top_mod| {
                let impls = top_mod.all_impls(db);
                impls.to_vec()
            })
            .collect()
    }

    fn is_core(&self, db: &'db dyn HirDb) -> bool {
        matches!(self.kind(db), IngotKind::Core)
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct IntegerId<'db> {
    #[return_ref]
    pub data: BigUint,
}

impl<'db> IntegerId<'db> {
    pub fn from_usize(db: &'db dyn HirDb, value: usize) -> Self {
        let data = BigUint::from(value);
        Self::new(db, data)
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct StringId<'db> {
    /// The text of the string literal, without the quotes.
    #[return_ref]
    pub data: String,
}

impl<'db> StringId<'db> {
    pub fn from_str(db: &'db dyn HirDb, value: &str) -> Self {
        let data = value.to_string();
        Self::new(db, data)
    }

    pub fn len_bytes(&self, db: &dyn HirDb) -> usize {
        self.data(db).len()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From, salsa::Update)]
pub enum LitKind<'db> {
    Int(IntegerId<'db>),
    String(StringId<'db>),
    Bool(bool),
}

/// `Partial<T>` is a type that explicitly indicates the possibility that an HIR
/// node cannot be generated due to syntax errors in the source file.
///
/// If a node is `Partial::Absent`, it means that the corresponding AST either
/// does not exist or is erroneous. When a `Partial::Absent` is generated, the
/// relevant error is always generated by the parser, so in Analysis phases, it
/// can often be ignored.
///
/// This type is clearly distinguished from `Option<T>`. The
/// `Option<T>` type is used to hold syntactically valid optional nodes, while
/// `Partial<T>` means that a syntactically required element may be missing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Partial<T> {
    Present(T),
    Absent,
}
unsafe impl<T> Update for Partial<T>
where
    T: Update,
{
    unsafe fn maybe_update(old_ptr: *mut Self, new_val: Self) -> bool {
        unsafe {
            use Partial::*;

            let old_val = &mut *old_ptr;
            match (old_val, new_val) {
                (Present(old), Present(new)) => T::maybe_update(old, new),
                (Absent, Absent) => false,
                (old_value, new_value) => {
                    *old_value = new_value;
                    true
                }
            }
        }
    }
}

impl<T> Partial<T> {
    pub fn unwrap(&self) -> &T {
        match self {
            Self::Present(value) => value,
            Self::Absent => panic!("unwrap called on absent value"),
        }
    }

    pub fn to_opt(self) -> Option<T> {
        match self {
            Self::Present(value) => Some(value),
            Self::Absent => None,
        }
    }

    pub fn is_present(&self) -> bool {
        matches!(self, Self::Present(_))
    }
}

impl<T> PartialEq<T> for Partial<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &T) -> bool {
        match self {
            Self::Present(a) => a == other,
            _ => false,
        }
    }
}

impl<T> Default for Partial<T> {
    fn default() -> Self {
        Self::Absent
    }
}

impl<T> From<Option<T>> for Partial<T> {
    fn from(value: Option<T>) -> Self {
        if let Some(value) = value {
            Self::Present(value)
        } else {
            Self::Absent
        }
    }
}

impl<T> From<Partial<T>> for Option<T> {
    fn from(value: Partial<T>) -> Option<T> {
        value.to_opt()
    }
}
