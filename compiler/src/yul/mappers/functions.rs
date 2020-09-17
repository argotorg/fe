use crate::errors::CompileError;
use crate::yul::mappers::expressions::spanned_expression;
use crate::yul::mappers::{
    assignments,
    declarations,
    expressions,
    types,
};
use crate::yul::namespace::scopes::{
    ContractDef,
    ContractScope,
    FunctionScope,
    Scope,
    Shared,
};
use crate::yul::namespace::types::FixedSize;
use std::rc::Rc;
use fe_parser::ast as vyp;
use fe_parser::span::Spanned;
use yultsur::*;

/// Builds a Yul function definition from a Fe function definition.
pub fn func_def(
    contract_scope: Shared<ContractScope>,
    def: &Spanned<vyp::ContractStmt>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::ContractStmt::FuncDef {
        qual: _,
        name,
        args,
        return_type,
        body,
    } = &def.node
    {
        let function_scope = FunctionScope::new(Rc::clone(&contract_scope));

        let name = name.node.to_string();

        // Map function args to Yul identifiers and `FixedSize` types.
        // Parameters are added to `function_scope` as scope variable definitions
        // in `func_def_arg`.
        let mut param_names = vec![];
        let mut param_types = vec![];
        for arg in args {
            let (name, typ) = func_def_arg(Rc::clone(&function_scope), arg)?;
            param_names.push(name);
            param_types.push(typ);
        }

        // Map the return value to a `FixedSize` type.
        let return_type = if let Some(return_type) = return_type {
            Some(types::type_desc_fixed_size(
                Scope::Function(Rc::clone(&function_scope)),
                &return_type,
            )?)
        } else {
            None
        };

        // Add this function to the contract scope. This is used to generate the ABI and
        // assists in declaring variables in other functions.
        contract_scope
            .borrow_mut()
            .add_function(name.clone(), param_types, return_type.clone());

        // Map all Fe statements to Yul statements.
        let function_name = identifier! {(name)};
        let function_statements = body
            .iter()
            .map(|stmt| func_stmt(Rc::clone(&function_scope), stmt))
            .collect::<Result<Vec<yul::Statement>, _>>()?;

        // Different return types require slightly different functions.
        if let Some(return_type) = return_type {
            match return_type {
                // Base types are returned by value. All memory used by the function is cleared.
                FixedSize::Base(_) => {
                    return Ok(function_definition! {
                        function [function_name]([param_names...]) -> return_val {
                            (let ptr := avail())
                            [function_statements...]
                            (free(ptr))
                        }
                    })
                }
                // Arrays need to keep memory allocated after completing.
                // FIXME: Copy arrays to the lowest available pointer to save memory.
                FixedSize::Array(array) => {
                    let size = literal_expression! {(array.size())};

                    return Ok(function_definition! {
                        function [function_name]([param_names...]) -> return_val {
                            [function_statements...]
                            (free((add(return_val, [size]))))
                        }
                    });
                }
            }
        }

        // Nothing is returned. All memory used by the function is freed.
        return Ok(function_definition! {
            function [function_name]([param_names...]) {
                (let ptr := avail())
                [function_statements...]
                (free(ptr))
            }
        });
    }

    unreachable!()
}

fn func_def_arg(
    scope: Shared<FunctionScope>,
    arg: &Spanned<vyp::FuncDefArg>,
) -> Result<(yul::Identifier, FixedSize), CompileError> {
    let name = arg.node.name.node.to_string();
    let typ = types::type_desc_fixed_size(Scope::Function(Rc::clone(&scope)), &arg.node.typ)?;

    match typ.clone() {
        FixedSize::Base(base) => scope.borrow_mut().add_base(name.clone(), base),
        FixedSize::Array(array) => scope.borrow_mut().add_array(name.clone(), array),
    }

    Ok((identifier! {(name)}, typ))
}

fn func_stmt(
    scope: Shared<FunctionScope>,
    stmt: &Spanned<vyp::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    match &stmt.node {
        vyp::FuncStmt::Return { .. } => func_return(scope, stmt),
        vyp::FuncStmt::VarDecl { .. } => declarations::var_decl(scope, stmt),
        vyp::FuncStmt::Assign { .. } => assignments::assign(scope, stmt),
        vyp::FuncStmt::Emit { .. } => emit(scope, stmt),
        vyp::FuncStmt::AugAssign { .. } => unimplemented!(),
        vyp::FuncStmt::For { .. } => unimplemented!(),
        vyp::FuncStmt::While { .. } => unimplemented!(),
        vyp::FuncStmt::If { .. } => unimplemented!(),
        vyp::FuncStmt::Assert { .. } => unimplemented!(),
        vyp::FuncStmt::Expr { .. } => unimplemented!(),
        vyp::FuncStmt::Pass => unimplemented!(),
        vyp::FuncStmt::Break => unimplemented!(),
        vyp::FuncStmt::Continue => unimplemented!(),
        vyp::FuncStmt::Revert => unimplemented!(),
    }
}

fn emit(
    scope: Shared<FunctionScope>,
    stmt: &Spanned<vyp::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::FuncStmt::Emit { value } = &stmt.node {
        if let vyp::Expr::Call { func, args } = &value.node {
            let event_name = expressions::expr_name_string(func)?;
            let event_values = args
                .node
                .iter()
                .map(|arg| call_arg(Rc::clone(&scope), arg))
                .collect::<Result<_, _>>()?;

            if let Some(ContractDef::Event(event)) = scope.borrow().contract_def(event_name) {
                return event.emit(event_values);
            }

            return Err(CompileError::static_str("missing event definition"));
        }

        return Err(CompileError::static_str(
            "emit statements must contain a call expression",
        ));
    }

    unreachable!()
}

fn call_arg(
    scope: Shared<FunctionScope>,
    arg: &Spanned<vyp::CallArg>,
) -> Result<yul::Expression, CompileError> {
    match &arg.node {
        vyp::CallArg::Arg(value) => {
            let spanned = spanned_expression(&arg.span, value);
            Ok(expressions::expr(scope, &spanned)?.expression)
        }
        vyp::CallArg::Kwarg(vyp::Kwarg { name: _, value }) => {
            Ok(expressions::expr(scope, value)?.expression)
        }
    }
}

fn func_return(
    scope: Shared<FunctionScope>,
    stmt: &Spanned<vyp::FuncStmt>,
) -> Result<yul::Statement, CompileError> {
    if let vyp::FuncStmt::Return { value } = &stmt.node {
        match value {
            Some(value) => {
                let ext = expressions::expr(scope, value)?;

                return Ok(yul::Statement::Block(block! {
                    (return_val := [ext.expression])
                    (leave)
                }));
            }
            None => return Ok(statement! { leave }),
        }
    }

    unreachable!()
}
