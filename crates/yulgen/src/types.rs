use fe_analyzer::namespace::types::{
    Array, Base, Contract, FeString, FixedSize, Integer, Struct, Tuple,
};
use fe_analyzer::AnalyzerDb;

pub trait EvmSized {
    /// The amount of bytes used by the type when being stored.
    fn size(&self) -> usize;
}

impl EvmSized for FixedSize {
    fn size(&self) -> usize {
        match self {
            FixedSize::Base(base) => base.size(),
            FixedSize::Array(array) => array.size(),
            FixedSize::Tuple(tuple) => tuple.size(),
            FixedSize::String(string) => string.size(),
            FixedSize::Contract(contract) => contract.size(),
            FixedSize::Struct(val) => val.size(),
        }
    }
}

impl EvmSized for Integer {
    fn size(&self) -> usize {
        match self {
            Integer::U8 => 1,
            Integer::U16 => 2,
            Integer::U32 => 4,
            Integer::U64 => 8,
            Integer::U128 => 16,
            Integer::U256 => 32,
            Integer::I8 => 1,
            Integer::I16 => 2,
            Integer::I32 => 4,
            Integer::I64 => 8,
            Integer::I128 => 16,
            Integer::I256 => 32,
        }
    }
}

impl EvmSized for Base {
    fn size(&self) -> usize {
        match self {
            Base::Numeric(integer) => integer.size(),
            Base::Bool => 1,
            Base::Address => 32,
            Base::Unit => 0,
        }
    }
}

impl EvmSized for Array {
    fn size(&self) -> usize {
        self.size * self.inner.size()
    }
}

impl EvmSized for Tuple {
    fn size(&self) -> usize {
        self.items.iter().map(|typ| typ.size()).sum()
    }
}

impl EvmSized for Struct {
    fn size(&self) -> usize {
        self.field_count * 32
    }
}

impl EvmSized for FeString {
    fn size(&self) -> usize {
        self.max_size + 32
    }
}

impl EvmSized for Contract {
    fn size(&self) -> usize {
        32
    }
}

/// Solidity ABI type with extra information needed for generation encoding/decoding functions.
#[derive(Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum AbiType {
    StaticArray { inner: Box<AbiType>, size: usize },
    Tuple { components: Vec<AbiType> },
    Uint { size: usize },
    Int { size: usize },
    Bool,
    Address,
    String { max_size: usize },
    Bytes { size: usize },
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum AbiDecodeLocation {
    Calldata,
    Memory,
}

pub fn to_abi_types(db: &dyn AnalyzerDb, types: &[impl AsAbiType]) -> Vec<AbiType> {
    types.iter().map(|typ| typ.as_abi_type(db)).collect()
}

pub fn to_abi_selector_names(types: &[AbiType]) -> Vec<String> {
    types.iter().map(|typ| typ.selector_name()).collect()
}

impl AbiType {
    /// The number of bytes used to encode the type's head.
    pub fn head_size(&self) -> usize {
        match self {
            AbiType::StaticArray { size, .. } => 32 * size,
            AbiType::Tuple { components } => 32 * components.len(),
            AbiType::Uint { .. } => 32,
            AbiType::Int { .. } => 32,
            AbiType::Bool => 32,
            AbiType::Address => 32,
            AbiType::String { .. } => 32,
            AbiType::Bytes { .. } => 32,
        }
    }

    /// The number of bytes used in Fe's data layout. This is used when packing and unpacking
    /// arrays.
    pub fn packed_size(&self) -> usize {
        match *self {
            AbiType::Uint { size } => size,
            AbiType::Int { size } => size,
            AbiType::Bool => 1,
            AbiType::Address => 32,
            _ => todo!("recursive encoding"),
        }
    }

    /// `true` if the encoded value is stored in the data section, `false` if it is not.
    pub fn has_data(&self) -> bool {
        match self {
            AbiType::Uint { .. } => false,
            AbiType::StaticArray { .. } => false,
            AbiType::Tuple { .. } => false,
            AbiType::Int { .. } => false,
            AbiType::Bool => false,
            AbiType::Address => false,
            AbiType::String { .. } => true,
            AbiType::Bytes { .. } => true,
        }
    }

    pub fn selector_name(&self) -> String {
        match self {
            AbiType::StaticArray { inner, size } => format!("{}[{}]", inner.selector_name(), size),
            AbiType::Tuple { components } => format!(
                "({})",
                components
                    .iter()
                    .map(|component| component.selector_name())
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            AbiType::Uint { size } => format!("uint{}", 8 * size),
            AbiType::Int { size } => format!("int{}", 8 * size),
            AbiType::Bool => "bool".to_string(),
            AbiType::Address => "address".to_string(),
            AbiType::String { .. } => "string".to_string(),
            AbiType::Bytes { .. } => "bytes".to_string(),
        }
    }
}

pub trait AsAbiType {
    fn as_abi_type(&self, db: &dyn AnalyzerDb) -> Option<AbiType>;
}

impl AsAbiType for FixedSize {
    fn as_abi_type(&self, db: &dyn AnalyzerDb) -> Option<AbiType> {
        match self {
            FixedSize::Base(base) => base.as_abi_type(db),
            FixedSize::Array(array) => array.as_abi_type(db),
            FixedSize::Tuple(tuple) => tuple.as_abi_type(db),
            FixedSize::String(string) => string.as_abi_type(db),
            FixedSize::Contract(_) => AbiType::Address,
            FixedSize::Struct(val) => val.as_abi_type(db),
        }
    }
}

impl AsAbiType for Base {
    fn as_abi_type(&self, _db: &dyn AnalyzerDb) -> Option<AbiType> {
        match self {
            Base::Numeric(integer) => {
                let size = integer.size();
                if integer.is_signed() {
                    Some(AbiType::Int { size })
                } else {
                    Some(AbiType::Uint { size })
                }
            }
            Base::Address => Some(AbiType::Address),
            Base::Bool => Some(AbiType::Bool),
            Base::Unit => None,
        }
    }
}

impl AsAbiType for Array {
    fn as_abi_type(&self, db: &dyn AnalyzerDb) -> Option<AbiType> {
        if matches!(self.inner, Base::Numeric(Integer::U8)) {
            Some(AbiType::Bytes { size: self.size })
        } else {
            Some(AbiType::StaticArray {
                inner: Box::new(self.inner.as_abi_type(db).unwrap()),
                size: self.size,
            })
        }
    }
}

impl AsAbiType for Struct {
    fn as_abi_type(&self, db: &dyn AnalyzerDb) -> Option<AbiType> {
        let components = self
            .id
            .fields(db)
            .values()
            .map(|field| {
                field
                    .typ(db)
                    .expect("struct field type error")
                    .as_abi_type(db)
            })
            .collect();
        Some(AbiType::Tuple { components })
    }
}

impl AsAbiType for Tuple {
    fn as_abi_type(&self, db: &dyn AnalyzerDb) -> Option<AbiType> {
        Some(AbiType::Tuple {
            components: self.items.iter().map(|typ| typ.as_abi_type(db)).collect(),
        })
    }
}

impl AsAbiType for FeString {
    fn as_abi_type(&self, _db: &dyn AnalyzerDb) -> Option<AbiType> {
        Some(AbiType::String {
            max_size: self.max_size,
        })
    }
}
