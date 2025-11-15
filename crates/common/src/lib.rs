pub mod config;
pub mod core;
pub mod dependencies;
pub mod diagnostics;
pub mod file;
pub mod indexmap;
pub mod ingot;
pub mod urlext;

use dependencies::DependencyGraph;
use file::Workspace;
pub use tracing;

#[salsa::db]
// Each database must implement InputDb explicitly with its own storage mechanism
pub trait InputDb: salsa::Database {
    fn workspace(&self) -> Workspace;
    fn graph(&self) -> DependencyGraph;
}

#[doc(hidden)]
pub use paste::paste;

// Macro for implementing the InputDb trait for a Salsa database struct
// This assumes the database has a field named `index` of type `Option<Workspace>`.
#[macro_export]
macro_rules! impl_input_db {
    ($db_type:ty) => {
        #[salsa::db]
        impl $crate::InputDb for $db_type {
            fn workspace(&self) -> $crate::file::Workspace {
                self.index.clone().expect("Workspace not initialized")
            }
            fn graph(&self) -> $crate::dependencies::DependencyGraph {
                self.graph.clone().expect("Graph not initialized")
            }
        }
    };
}

// Macro for implementing Default for a Salsa database with Workspace

// This assumes the database has a field named `index` of type `Option<Workspace>`,
// and will initialize it properly.
#[macro_export]
macro_rules! impl_db_default {
    ($db_type:ty) => {
        impl Default for $db_type
        where
            $db_type: $crate::core::HasBuiltinCore,
        {
            fn default() -> Self {
                let mut db = Self {
                    storage: salsa::Storage::default(),
                    index: None,
                    graph: None,
                };
                let index = $crate::file::Workspace::default(&db);
                db.index = Some(index);
                let graph = $crate::dependencies::DependencyGraph::default(&db);
                db.graph = Some(graph);
                $crate::core::HasBuiltinCore::initialize_builtin_core(&mut db);
                db
            }
        }
    };
}

// Macro for creating a standard Salsa database with Workspace support
#[macro_export]
macro_rules! define_input_db {
    ($db_name:ident) => {
        #[derive(Clone)]
        #[salsa::db]
        pub struct $db_name {
            storage: salsa::Storage<Self>,
            index: Option<$crate::file::Workspace>,
            graph: Option<$crate::dependencies::DependencyGraph>,
        }

        #[salsa::db]
        impl salsa::Database for $db_name {
            fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
        }

        $crate::impl_input_db!($db_name);
        $crate::impl_db_default!($db_name);
    };
}

#[macro_export]
macro_rules! impl_db_traits {
    ($db_type:ty, $($trait_name:ident),+ $(,)?) => {
        #[salsa::db]
        impl salsa::Database for $db_type {
            fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
        }
    };
}
