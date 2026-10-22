pub mod cache;
pub mod color;
pub mod compilation;
pub mod config;
pub mod dependencies;
pub mod diagnostics;
pub mod file;
pub mod generation;
pub mod graph;
pub mod indexmap;
pub mod ingot;
pub mod layout;
pub mod options;
pub mod origin;
pub mod paths;
pub mod source;
pub mod stdlib;
pub mod urlext;

use compilation::CompilationSettings;
use dependencies::DependencyGraph;
use file::Workspace;
use options::CompilerOptions;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SalsaEventCounters {
    pub will_execute: usize,
    pub did_validate_memoized_value: usize,
    pub will_discard_stale_output: usize,
    pub did_discard: usize,
    pub will_iterate_cycle: usize,
}

impl SalsaEventCounters {
    pub fn record_event(&mut self, event: &salsa::Event) {
        match &event.kind {
            salsa::EventKind::WillExecute { .. } => self.will_execute += 1,
            salsa::EventKind::DidValidateMemoizedValue { .. } => {
                self.did_validate_memoized_value += 1;
            }
            salsa::EventKind::WillDiscardStaleOutput { .. } => {
                self.will_discard_stale_output += 1;
            }
            salsa::EventKind::DidDiscard { .. } => self.did_discard += 1,
            salsa::EventKind::WillIterateCycle { .. } => self.will_iterate_cycle += 1,
            _ => {}
        }
    }
}

#[salsa::db]
// Each database must implement InputDb explicitly with its own storage mechanism
pub trait InputDb: salsa::Database {
    fn generation_inputs(&self) -> generation::GenerationInputs;
    fn workspace(&self) -> Workspace;
    fn dependency_graph(&self) -> DependencyGraph;
    fn compiler_options(&self) -> CompilerOptions;
    fn compilation_settings(&self) -> CompilationSettings;
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
            fn generation_inputs(&self) -> $crate::generation::GenerationInputs {
                self.generation.expect("Generation inputs not initialized")
            }
            fn workspace(&self) -> $crate::file::Workspace {
                self.index.clone().expect("Workspace not initialized")
            }
            fn dependency_graph(&self) -> $crate::dependencies::DependencyGraph {
                self.graph.clone().expect("Graph not initialized")
            }
            fn compiler_options(&self) -> $crate::options::CompilerOptions {
                self.options
                    .clone()
                    .expect("Compiler options not initialized")
            }
            fn compilation_settings(&self) -> $crate::compilation::CompilationSettings {
                self.settings
                    .clone()
                    .expect("Compilation settings not initialized")
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
            $db_type: $crate::stdlib::HasBuiltinCore + $crate::stdlib::HasBuiltinStd,
        {
            fn default() -> Self {
                Self::with_storage(salsa::Storage::default())
            }
        }

        impl $db_type
        where
            $db_type: $crate::stdlib::HasBuiltinCore + $crate::stdlib::HasBuiltinStd,
        {
            /// A database that tallies its salsa events in `counters`.
            #[allow(dead_code)]
            pub fn with_salsa_event_counters(
                counters: std::sync::Arc<std::sync::Mutex<$crate::SalsaEventCounters>>,
            ) -> Self {
                Self::with_storage(salsa::Storage::new(Some(Box::new(move |event| {
                    if let Ok(mut counters) = counters.lock() {
                        counters.record_event(&event);
                    }
                }))))
            }

            fn with_storage(storage: salsa::Storage<Self>) -> Self {
                let mut db = Self {
                    storage,
                    index: None,
                    graph: None,
                    options: None,
                    settings: None,
                    generation: None,
                };
                let index = $crate::file::Workspace::default(&db);
                db.index = Some(index);
                let graph = $crate::dependencies::DependencyGraph::default(&db);
                db.graph = Some(graph);
                let options = $crate::options::CompilerOptions::default(&db);
                db.options = Some(options);
                let settings = $crate::compilation::CompilationSettings::default(&db);
                db.settings = Some(settings);
                db.generation = Some($crate::generation::GenerationInputs::new(
                    &db,
                    vec![],
                    vec![],
                ));
                $crate::stdlib::HasBuiltinCore::initialize_builtin_core(&mut db);
                $crate::stdlib::HasBuiltinStd::initialize_builtin_std(&mut db);
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
            options: Option<$crate::options::CompilerOptions>,
            settings: Option<$crate::compilation::CompilationSettings>,
            generation: Option<$crate::generation::GenerationInputs>,
        }

        #[salsa::db]
        impl salsa::Database for $db_name {}

        $crate::impl_input_db!($db_name);
        $crate::impl_db_default!($db_name);
    };
}
