use crate::errors::SemanticError;
use crate::namespace::scopes::{
    ModuleScope,
    Shared,
};
use crate::namespace::types;
use crate::traversal::{
    contracts,
    structs,
};
use crate::Context;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use std::rc::Rc;

/// Gather context information for a module and check for type errors.
pub fn module(context: Shared<Context>, module: &fe::Module) -> Result<(), SemanticError> {
    let scope = ModuleScope::new();

    let mut contracts = vec![];

    for stmt in module.body.iter() {
        match &stmt.node {
            fe::ModuleStmt::TypeDef { .. } => type_def(Rc::clone(&scope), stmt)?,
            fe::ModuleStmt::StructDef { name, body } => {
                structs::struct_def(Rc::clone(&scope), name.node, body)?
            }
            fe::ModuleStmt::ContractDef { .. } => {
                // Collect contract statements and the scope that we create for them. After we
                // have walked all contracts once, we walk over them again for a
                // more detailed inspection.
                let contract_scope =
                    contracts::contract_def(Rc::clone(&scope), Rc::clone(&context), stmt)?;
                contracts.push((stmt, contract_scope))
            }
            fe::ModuleStmt::FromImport { .. } => unimplemented!(),
            fe::ModuleStmt::SimpleImport { .. } => unimplemented!(),
        }
    }

    for (stmt, scope) in contracts.iter() {
        if let fe::ModuleStmt::ContractDef { .. } = stmt.node {
            contracts::contract_body(Rc::clone(&scope), Rc::clone(&context), stmt)?
        }
    }

    Ok(())
}

fn type_def(
    scope: Shared<ModuleScope>,
    def: &Spanned<fe::ModuleStmt>,
) -> Result<(), SemanticError> {
    if let fe::ModuleStmt::TypeDef { name, typ } = &def.node {
        let typ = types::type_desc(&scope.borrow().type_defs, &typ.node)?;
        scope.borrow_mut().add_type_def(name.node, typ)?;
        return Ok(());
    }

    unreachable!()
}
