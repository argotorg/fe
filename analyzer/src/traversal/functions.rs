use crate::context::{Context, ExpressionAttributes, FunctionAttributes, Location};
use crate::errors::SemanticError;
use crate::namespace::scopes::{BlockScope, BlockScopeType, ContractScope, Scope, Shared};
use crate::namespace::types::{Base, FixedSize, Tuple, Type};
use crate::traversal::utils::{expression_attributes_to_types, fixed_sizes_to_types};
use crate::traversal::{assignments, declarations, expressions, types};
use fe_parser::ast as fe;
use fe_parser::node::Node;
use std::rc::Rc;

/// Gather context information for a function definition and check for type
/// errors. Does not inspect the function body.
pub fn func_def(
    contract_scope: Shared<ContractScope>,
    context: Shared<Context>,
    def: &Node<fe::ContractStmt>,
) -> Result<(), SemanticError> {
    if let fe::ContractStmt::FuncDef {
        is_pub,
        name,
        args,
        return_type,
        body: _,
    } = &def.kind
    {
        let name = &name.kind;
        let function_scope = BlockScope::from_contract_scope(name, Rc::clone(&contract_scope));

        let params = args
            .iter()
            .map(|arg| func_def_arg(Rc::clone(&function_scope), Rc::clone(&context), arg))
            .collect::<Result<Vec<_>, _>>()?;

        let return_type = return_type
            .as_ref()
            .map(|typ| {
                types::type_desc_fixed_size(
                    Scope::Block(Rc::clone(&function_scope)),
                    Rc::clone(&context),
                    &typ,
                )
            })
            .transpose()?
            .unwrap_or_else(|| Tuple::empty().into());

        // `__init__` must not return any type other than `()`.
        if name == "__init__" && !return_type.is_empty_tuple() {
            return Err(SemanticError::type_error());
        }

        let attributes: FunctionAttributes = contract_scope
            .borrow_mut()
            .add_function(
                name,
                *is_pub,
                params,
                return_type,
                Rc::clone(&function_scope),
            )?
            .to_owned()
            .into();

        context.borrow_mut().add_function(def, attributes);

        return Ok(());
    }

    unreachable!();
}

/// Gather context information for a function body and check for type errors.
pub fn func_body(
    contract_scope: Shared<ContractScope>,
    context: Shared<Context>,
    def: &Node<fe::ContractStmt>,
) -> Result<(), SemanticError> {
    if let fe::ContractStmt::FuncDef { name, body, .. } = &def.kind {
        let host_func_def = contract_scope
            .borrow()
            .function_def(&name.kind)
            .unwrap_or_else(|| panic!("Failed to lookup function definition for {}", &name.kind));

        // If the return type is an empty tuple we do not have to validate any further
        // at this point because both returning (explicit) or not returning (implicit
        // return) are valid syntax.
        // If the return type is anything else, we do need to ensure that all code paths
        // return or revert.
        if !host_func_def.return_type.is_empty_tuple() {
            validate_all_paths_return_or_revert(&body)?
        }

        traverse_statements(Rc::clone(&host_func_def.scope), Rc::clone(&context), body)?;

        return Ok(());
    }

    unreachable!()
}

fn traverse_statements(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    body: &[Node<fe::FuncStmt>],
) -> Result<(), SemanticError> {
    for stmt in body.iter() {
        func_stmt(Rc::clone(&scope), Rc::clone(&context), stmt)?
    }
    Ok(())
}

fn validate_all_paths_return_or_revert(block: &[Node<fe::FuncStmt>]) -> Result<(), SemanticError> {
    for statement in block.iter().rev() {
        if let fe::FuncStmt::Return { .. } = &statement.kind {
            return Ok(());
        }

        if let fe::FuncStmt::Revert { .. } = &statement.kind {
            return Ok(());
        }

        if let fe::FuncStmt::If {
            test: _,
            body,
            or_else,
        } = &statement.kind
        {
            let body_returns = validate_all_paths_return_or_revert(body).is_ok();
            let or_else_returns =
                or_else.is_empty() || validate_all_paths_return_or_revert(or_else).is_ok();
            if body_returns && or_else_returns {
                return Ok(());
            }
        }
    }

    Err(SemanticError::missing_return())
}

fn func_def_arg(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    arg: &Node<fe::FuncDefArg>,
) -> Result<(String, FixedSize), SemanticError> {
    let name = &arg.kind.name.kind;
    let typ = types::type_desc_fixed_size(Scope::Block(Rc::clone(&scope)), context, &arg.kind.typ)?;

    scope.borrow_mut().add_var(name, typ.clone())?;

    Ok((name.to_string(), typ))
}

fn func_stmt(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    match &stmt.kind {
        fe::FuncStmt::Return { .. } => func_return(scope, context, stmt),
        fe::FuncStmt::VarDecl { .. } => declarations::var_decl(scope, context, stmt),
        fe::FuncStmt::Assign { .. } => assignments::assign(scope, context, stmt),
        fe::FuncStmt::Emit { .. } => emit(scope, context, stmt),
        fe::FuncStmt::AugAssign { .. } => assignments::aug_assign(scope, context, stmt),
        fe::FuncStmt::For { .. } => for_loop(scope, context, stmt),
        fe::FuncStmt::While { .. } => while_loop(scope, context, stmt),
        fe::FuncStmt::If { .. } => if_statement(scope, context, stmt),
        fe::FuncStmt::Assert { .. } => assert(scope, context, stmt),
        fe::FuncStmt::Expr { .. } => expr(scope, context, stmt),
        fe::FuncStmt::Pass => Ok(()),
        fe::FuncStmt::Break => break_statement(scope, context, stmt),
        fe::FuncStmt::Continue => continue_statement(scope, context, stmt),
        fe::FuncStmt::Revert => Ok(()),
    }
    .map_err(|error| error.with_context(stmt.span))
}

fn for_loop(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    match &stmt.kind {
        fe::FuncStmt::For { target, iter, body } => {
            // Create the for loop body scope.
            let body_scope = BlockScope::from_block_scope(BlockScopeType::Loop, Rc::clone(&scope));
            // Make sure iter is in the function scope & it should be an array.
            let target_type = verify_is_array(scope, Rc::clone(&context), iter)?;
            body_scope.borrow_mut().add_var(&target.kind, target_type)?;
            // Traverse the statements within the `for loop` body scope.
            traverse_statements(body_scope, context, body)
        }
        _ => unreachable!(),
    }
}

fn verify_is_array(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    expr: &Node<fe::Expr>,
) -> Result<FixedSize, SemanticError> {
    let attributes = expressions::expr(Rc::clone(&scope), Rc::clone(&context), &expr)?;
    if let Type::Array(array) = attributes.typ {
        // TODO: (SA) Could add a support for tuple
        return Ok(FixedSize::Base(array.inner));
    }
    Err(SemanticError::type_error())
}

fn verify_is_boolean(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    expr: &Node<fe::Expr>,
) -> Result<(), SemanticError> {
    let attributes = expressions::expr(scope, context, expr)?;
    if let Type::Base(Base::Bool) = attributes.typ {
        return Ok(());
    }

    Err(SemanticError::type_error())
}

fn break_statement(
    scope: Shared<BlockScope>,
    _context: Shared<Context>,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Break {} = &stmt.kind {
        return verify_loop_in_scope(scope, SemanticError::break_without_loop());
    }
    unreachable!()
}

fn continue_statement(
    scope: Shared<BlockScope>,
    _context: Shared<Context>,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Continue {} = &stmt.kind {
        return verify_loop_in_scope(scope, SemanticError::continue_without_loop());
    }
    unreachable!()
}

fn verify_loop_in_scope(
    scope: Shared<BlockScope>,
    error: SemanticError,
) -> Result<(), SemanticError> {
    if scope.borrow().inherits_type(BlockScopeType::Loop) {
        Ok(())
    } else {
        Err(error)
    }
}

fn if_statement(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    match &stmt.kind {
        fe::FuncStmt::If {
            test,
            body,
            or_else,
        } => {
            let body_scope =
                BlockScope::from_block_scope(BlockScopeType::IfElse, Rc::clone(&scope));
            traverse_statements(body_scope, Rc::clone(&context), body)?;
            let or_else_scope =
                BlockScope::from_block_scope(BlockScopeType::IfElse, Rc::clone(&scope));
            traverse_statements(or_else_scope, Rc::clone(&context), or_else)?;
            verify_is_boolean(scope, context, test)
        }
        _ => unreachable!(),
    }
}

fn while_loop(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    match &stmt.kind {
        fe::FuncStmt::While { test, body } => {
            let body_scope = BlockScope::from_block_scope(BlockScopeType::Loop, Rc::clone(&scope));
            traverse_statements(body_scope, Rc::clone(&context), body)?;
            verify_is_boolean(scope, context, test)
        }
        _ => unreachable!(),
    }
}

fn expr(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Expr { value } = &stmt.kind {
        let _attributes = expressions::expr(scope, context, value)?;
    }

    Ok(())
}

fn emit(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Emit { name, args } = &stmt.kind {
        return if let Some(event) = scope.borrow().contract_event_def(&name.kind) {
            context.borrow_mut().add_emit(stmt, event.clone());
            let argument_attributes = args
                .kind
                .iter()
                .map(|arg| expressions::call_arg(Rc::clone(&scope), Rc::clone(&context), arg))
                .collect::<Result<Vec<_>, _>>()?;

            if fixed_sizes_to_types(event.all_field_types())
                != expression_attributes_to_types(argument_attributes)
            {
                Err(SemanticError::type_error())
            } else {
                Ok(())
            }
        } else {
            Err(SemanticError::missing_event_definition())
        };
    }

    unreachable!()
}

fn assert(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Assert { test, msg } = &stmt.kind {
        verify_is_boolean(Rc::clone(&scope), Rc::clone(&context), test)?;
        if let Some(msg) = msg {
            let msg_attributes = expressions::expr(scope, context, msg)?;
            if !matches!(msg_attributes.typ, Type::String(_)) {
                return Err(SemanticError::type_error());
            }
        }

        return Ok(());
    }

    unreachable!()
}

fn func_return(
    scope: Shared<BlockScope>,
    context: Shared<Context>,
    stmt: &Node<fe::FuncStmt>,
) -> Result<(), SemanticError> {
    if let fe::FuncStmt::Return { value } = &stmt.kind {
        let attributes = match value {
            Some(val) => expressions::assignable_expr(Rc::clone(&scope), Rc::clone(&context), val)?,
            None => ExpressionAttributes::new(Type::Tuple(Tuple::empty()), Location::Value),
        };

        let host_func_def = scope
            .borrow()
            .current_function_def()
            .expect("Failed to get function definition");
        if attributes.typ != host_func_def.return_type.into() {
            return Err(SemanticError::type_error());
        }

        return Ok(());
    }

    unreachable!()
}
