use crate::errors::SemanticError;
use crate::namespace::scopes::{
    BlockScope,
    Scope,
    Shared,
};
use crate::namespace::types::Type;
use crate::traversal::{
    expressions,
    types,
};
use crate::Context;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::rc::Rc;

/// Gather context information for var declarations and check for type errors.
pub fn var_decl(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::VarDecl { target, typ, value } = &stmt.kind {
        let name = expressions::expr_name_string(target)?;
        let declared_type = types::type_desc_fixed_size(Scope::Block(Rc::clone(&scope)), typ)?;
        if let Some(value) = value {
            let value_attributes =
                expressions::assignable_expr(Rc::clone(&scope), Rc::clone(&context), value)?;

            if Type::from(declared_type.clone()) != value_attributes.typ {
                return Err(SemanticError::type_error());
            }
        }

        scope.borrow_mut().add_var(&name, declared_type.clone())?;
        context.borrow_mut().add_declaration(stmt, declared_type);

        return Ok(());
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use crate::errors::{
        ErrorKind,
        SemanticError,
    };
    use crate::namespace::scopes::{
        BlockScope,
        ContractScope,
        ModuleScope,
        Shared,
    };
    use crate::namespace::types::{
        FixedSize,
        U256,
    };
    use crate::traversal::declarations::var_decl;
    use crate::Context;
    use fe_parser::{
        grammar::functions::parse_stmt,
        parse_code_chunk,
    };
    use std::rc::Rc;

    fn scope() -> Shared<BlockScope> {
        let module_scope = ModuleScope::new();
        let contract_scope = ContractScope::new("", module_scope);
        BlockScope::from_contract_scope("", contract_scope)
    }

    fn analyze(scope: Shared<BlockScope>, src: &str) -> Result<Context, SemanticError> {
        let context = Context::new_shared();

        let statement = parse_code_chunk(parse_stmt, src).expect("Couldn't build statement AST");

        var_decl(scope, Rc::clone(&context), &statement)?;
        Ok(Rc::try_unwrap(context)
            .map_err(|_| "")
            .unwrap()
            .into_inner())
    }

    #[test]
    fn simple_decl() {
        let statement = "foo: u256 = 26 + 42";
        let scope = scope();
        let context = analyze(Rc::clone(&scope), statement).expect("analysis failed");
        assert_eq!(context.expressions.len(), 3);
        assert_eq!(
            scope.borrow().get_variable_def("foo"),
            Some(FixedSize::Base(U256))
        );
    }

    #[test]
    fn type_error_decl() {
        let statement = "foo: u256[100] = 26";
        let result = analyze(scope(), statement);
        assert_eq!(
            result.expect_err("analysis didn't fail").kind,
            ErrorKind::TypeError
        );
    }

    #[test]
    fn duplicate_var_decl() {
        let statement = "foo: u256 = 0";
        let scope = scope();
        let result = analyze(scope.clone(), statement);
        assert!(result.is_ok());
        let result = analyze(scope, statement);
        assert_eq!(
            result.expect_err("analysis didn't fail").kind,
            ErrorKind::AlreadyDefined,
        );
    }
}
