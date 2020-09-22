use crate::abi::utils as abi_utils;
use crate::errors::CompileError;
use crate::yul::namespace::scopes::ContractDef;
#[allow(unused_imports)]
use crate::yul::namespace::types::{
    Base,
    FixedSize,
};
use std::collections::HashMap;
use yultsur::*;

/// Builds a switch statement that dispatches calls to the contract.
pub fn dispatcher(
    interface: &[String],
    defs: &HashMap<String, ContractDef>,
) -> Result<yul::Statement, CompileError> {
    let arms = interface
        .iter()
        .map(|name| dispatch_arm(name.to_owned(), defs))
        .collect::<Result<Vec<yul::Case>, CompileError>>()?;

    Ok(switch! {
        switch (cloadn(0, 4))
        [arms...]
    })
}

fn dispatch_arm(
    name: String,
    defs: &HashMap<String, ContractDef>,
) -> Result<yul::Case, CompileError> {
    if let Some(ContractDef::Function { params, returns }) = defs.get(&name) {
        let selector = selector(name.clone(), &params);

        if let Some(returns) = returns {
            let selection = selection(name, &params)?;
            let return_data = returns.encode(selection)?;
            let return_size = literal_expression! {(returns.padded_size())};

            let selection_with_return = statement! { return([return_data], [return_size]) };

            return Ok(case! { case [selector] { [selection_with_return] } });
        }

        let selection = selection_as_statement(name, &params)?;

        return Ok(case! { case [selector] { [selection] } });
    }

    Err(CompileError::static_str("no definition for name"))
}

fn selector(name: String, params: &[FixedSize]) -> yul::Literal {
    let params = params
        .iter()
        .map(|param| param.abi_name())
        .collect::<Vec<String>>();

    literal! {(abi_utils::func_selector(name, params))}
}

fn selection(name: String, params: &[FixedSize]) -> Result<yul::Expression, CompileError> {
    let mut ptr = 4;
    let mut decoded_params = vec![];

    for param in params.iter() {
        decoded_params.push(param.decode(literal_expression! {(ptr)})?);
        ptr += param.padded_size();
    }

    let name = identifier! {(name)};

    Ok(expression! { [name]([decoded_params...]) })
}

fn selection_as_statement(
    name: String,
    params: &[FixedSize],
) -> Result<yul::Statement, CompileError> {
    Ok(yul::Statement::Expression(selection(name, params)?))
}

#[test]
fn test_selector_literal_basic() {
    assert_eq!(
        selector("foo".to_string(), &vec![]).to_string(),
        String::from("0xc2985578"),
    )
}

#[test]
fn test_selector_literal() {
    assert_eq!(
        selector("bar".to_string(), &vec![FixedSize::Base(Base::U256)]).to_string(),
        String::from("0x0423a132"),
    )
}
