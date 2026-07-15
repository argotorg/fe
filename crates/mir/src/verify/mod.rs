mod consts;
mod layout;
mod package;
mod place;
mod runtime;
mod storage_layout;

use hir::{analysis::ty::ty_def::TyId, hir_def::Contract, semantic::ContractFieldId};

use crate::{
    instance::RuntimeInstance,
    runtime::{
        AddressSpaceKind, ConstRegionId, ContractFieldSlot, LayoutId, RBlockId, RLocalId,
        RuntimeClass,
    },
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VerifyError<'db> {
    MissingRuntimeLocal(RLocalId),
    MissingRuntimeBlock(RBlockId),
    ErasedRuntimeValue(crate::runtime::RValueId),
    SlotCarrierMismatch(RLocalId),
    InvalidLayoutRefView(LayoutId<'db>),
    InvalidConstRegion(ConstRegionId<'db>),
    InvalidVariant(LayoutId<'db>, u16),
    InvalidPlace(RuntimeClass<'db>),
    InvalidVariantPlace(RuntimeClass<'db>),
    InvalidEnumTag(LayoutId<'db>),
    MissingEnumVariantProof(RLocalId),
    InvalidReturnClass,
    InvalidExprClass(RLocalId),
    InvalidIndexClass(crate::runtime::RValueId),
    InvalidStoreClass,
    InvalidCopyClass,
    InvalidTerminalCall(RuntimeInstance<'db>),
    CallArgCountMismatch(RuntimeInstance<'db>),
    CallArgClassMismatch(RuntimeInstance<'db>, usize),
    InvalidCodeRegion(crate::runtime::RuntimeCodeRegion<'db>),
    InvalidPackageFunction(crate::instance::RuntimeInstance<'db>),
    InvalidPackageObject(crate::runtime::RuntimeObject<'db>),
    UnknownPackageObject(String),
    InvalidPackageSection(
        crate::runtime::RuntimeObject<'db>,
        crate::runtime::RuntimeSectionName,
    ),
    DuplicateRuntimeSymbol(String),
    ContractFieldArgumentCountMismatch {
        contract: Contract<'db>,
        expected: usize,
        actual: usize,
    },
    ContractFieldIdentityMismatch {
        expected: Option<ContractFieldId<'db>>,
        actual: Option<ContractFieldId<'db>>,
    },
    ContractFieldOwnerMismatch {
        contract: Contract<'db>,
        field: ContractFieldId<'db>,
    },
    UnknownContractField(ContractFieldId<'db>),
    InvalidContractFieldLayout(ContractFieldId<'db>),
    ContractFieldTypeMismatch {
        field: ContractFieldId<'db>,
        expected: TyId<'db>,
        actual: TyId<'db>,
    },
    ContractFieldInitModeMismatch {
        field: ContractFieldId<'db>,
        expected: bool,
        actual: bool,
    },
    ContractFieldAddressSpaceMismatch {
        field: ContractFieldId<'db>,
        expected: AddressSpaceKind,
        actual: Option<AddressSpaceKind>,
    },
    ContractFieldSlotMismatch {
        field: ContractFieldId<'db>,
        expected: ContractFieldSlot,
        actual: ContractFieldSlot,
    },
    InvalidContractFieldClass {
        field: ContractFieldId<'db>,
        class: RuntimeClass<'db>,
    },
    ContractFieldKindMismatch(ContractFieldId<'db>),
    ContractFieldSpanMismatch {
        field: ContractFieldId<'db>,
        hir_span: usize,
        mir_span: u64,
    },
}

impl<'db> VerifyError<'db> {
    pub fn local(&self) -> Option<RLocalId> {
        match self {
            VerifyError::MissingRuntimeLocal(local)
            | VerifyError::SlotCarrierMismatch(local)
            | VerifyError::MissingEnumVariantProof(local)
            | VerifyError::InvalidExprClass(local)
            | VerifyError::InvalidIndexClass(local) => Some(*local),
            VerifyError::MissingRuntimeBlock(_)
            | VerifyError::ErasedRuntimeValue(_)
            | VerifyError::InvalidLayoutRefView(_)
            | VerifyError::InvalidConstRegion(_)
            | VerifyError::InvalidVariant(_, _)
            | VerifyError::InvalidPlace(_)
            | VerifyError::InvalidVariantPlace(_)
            | VerifyError::InvalidEnumTag(_)
            | VerifyError::InvalidStoreClass
            | VerifyError::InvalidCopyClass
            | VerifyError::InvalidTerminalCall(_)
            | VerifyError::InvalidReturnClass
            | VerifyError::CallArgCountMismatch(_)
            | VerifyError::CallArgClassMismatch(_, _)
            | VerifyError::InvalidCodeRegion(_)
            | VerifyError::InvalidPackageFunction(_)
            | VerifyError::InvalidPackageObject(_)
            | VerifyError::UnknownPackageObject(_)
            | VerifyError::InvalidPackageSection(_, _)
            | VerifyError::DuplicateRuntimeSymbol(_)
            | VerifyError::ContractFieldArgumentCountMismatch { .. }
            | VerifyError::ContractFieldIdentityMismatch { .. }
            | VerifyError::ContractFieldOwnerMismatch { .. }
            | VerifyError::UnknownContractField(_)
            | VerifyError::InvalidContractFieldLayout(_)
            | VerifyError::ContractFieldTypeMismatch { .. }
            | VerifyError::ContractFieldInitModeMismatch { .. }
            | VerifyError::ContractFieldAddressSpaceMismatch { .. }
            | VerifyError::ContractFieldSlotMismatch { .. }
            | VerifyError::InvalidContractFieldClass { .. }
            | VerifyError::ContractFieldKindMismatch(_)
            | VerifyError::ContractFieldSpanMismatch { .. } => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeVerifyFailure<'db> {
    pub error: VerifyError<'db>,
    pub site: RuntimeVerifySite,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RuntimeVerifySite {
    SignatureParam(usize),
    LocalRoot(RLocalId),
    LocalCarrier(RLocalId),
    Stmt { block: RBlockId, stmt: usize },
    Terminator { block: RBlockId },
    Body,
}

pub use consts::verify_const_region;
pub use package::verify_runtime_package;
pub use place::{resolve_runtime_place, resolve_runtime_place_address_class};
pub use runtime::{verify_runtime_body, verify_runtime_body_detailed};
