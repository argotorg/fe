use fe_common::impl_intern_key;
use num_bigint::BigInt;
use smol_str::SmolStr;

use fe_analyzer::{context, namespace::items as analyzer_items};

use super::{SourceInfo, TypeId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constant {
    /// A name of a constant.
    pub name: SmolStr,

    /// A value of a constant.
    pub value: ConstantValue,

    /// A type of a constant.
    pub ty: TypeId,

    /// A module where a constant is declared.
    pub module_id: analyzer_items::ModuleId,

    /// A span where a constant is declared.
    pub source: SourceInfo,
}

/// An interned Id for [`Constant`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstantId(pub(crate) u32);
impl_intern_key!(ConstantId);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantValue {
    Immediate(BigInt),
    Str(SmolStr),
    Bool(bool),
}

impl From<context::Constant> for ConstantValue {
    fn from(value: context::Constant) -> Self {
        match value {
            context::Constant::Int(num) | context::Constant::Address(num) => Self::Immediate(num),
            context::Constant::Str(s) => Self::Str(s),
            context::Constant::Bool(b) => Self::Bool(b),
        }
    }
}
