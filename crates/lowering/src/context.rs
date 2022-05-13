use fe_analyzer::context::{ExpressionAttributes, FunctionBody};
use fe_analyzer::namespace::items::{FunctionId, ModuleId};
use fe_analyzer::namespace::types::{Array, Tuple, Type};
use fe_analyzer::AnalyzerDb;
use fe_parser::node::NodeId;
use indexmap::IndexSet;
use std::rc::Rc;

pub struct ModuleContext<'db> {
    pub db: &'db dyn AnalyzerDb,
    pub module: ModuleId,

    /// List expressions that are used in the module
    pub list_expressions: IndexSet<Array>,

    /// Tuples that are used in the module
    pub tuples: IndexSet<Tuple>,
}

impl<'db> ModuleContext<'db> {
    pub fn new(db: &'db dyn AnalyzerDb, module: ModuleId) -> Self {
        Self {
            db,
            module,
            list_expressions: IndexSet::new(),
            tuples: IndexSet::new(),
        }
    }
}

pub struct FnContext<'a, 'db> {
    pub module: &'a mut ModuleContext<'db>,
    pub body: Rc<FunctionBody>,
    pub id: FunctionId,

    /// Holds fresh id for [`FnContext::make_unique_name`]
    fresh_id: u64,
}

impl<'a, 'db> FnContext<'a, 'db> {
    pub fn new(module: &'a mut ModuleContext<'db>, id: FunctionId, body: Rc<FunctionBody>) -> Self {
        Self {
            module,
            body,
            id,
            fresh_id: 0,
        }
    }

    /// Makes a unique name from the given name, keeping it as readable as possible.
    pub fn make_unique_name(&mut self, name: &str) -> String {
        let id = self.fresh_id;
        self.fresh_id += 1;
        format!("${}_{}", name, id)
    }

    pub fn db(&self) -> &'db dyn AnalyzerDb {
        self.module.db
    }

    pub fn expression_attributes<T: Into<NodeId>>(
        &self,
        node_id: T,
    ) -> Option<&ExpressionAttributes> {
        self.body.expressions.get(&node_id.into())
    }

    pub fn var_decl_type<T: Into<NodeId>>(&self, node_id: T) -> Option<&Type> {
        self.body.var_decl_types.get(&node_id.into())
    }

    pub fn const_decl_type<T: Into<NodeId>>(&self, node_id: T) -> Option<&Type> {
        self.body.var_decl_types.get(&node_id.into())
    }
}

impl<'a, 'db> AsMut<ModuleContext<'db>> for FnContext<'a, 'db> {
    fn as_mut(&mut self) -> &mut ModuleContext<'db> {
        self.module
    }
}
