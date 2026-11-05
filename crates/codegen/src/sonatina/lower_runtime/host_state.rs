//! The process state the native entry keeps for the host capabilities. The
//! entry stores its `argc` and `argv` in private globals, and `std::env`
//! reads them through extern functions the compiler defines here. Outside an
//! executable's entry, the globals stay zero.

use sonatina_ir::{
    Linkage,
    func_cursor::InstInserter,
    global_variable::GlobalVariableData,
    inst::{control_flow::Return, data::Mload},
    module::FuncRef,
};

use super::{LowerError, LoweringInstSet, ModuleLowerer};

/// A value of the native entry's that the host capabilities read.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum HostState {
    Argc,
    Argv,
}

impl HostState {
    pub(crate) const ALL: [Self; 2] = [Self::Argc, Self::Argv];

    /// The extern function `std::env` reads the value with.
    pub(crate) fn accessor(self) -> &'static str {
        match self {
            Self::Argc => "__fe_native_argc",
            Self::Argv => "__fe_native_argv",
        }
    }

    /// The global the entry stores the value in.
    pub(crate) fn global(self) -> &'static str {
        match self {
            Self::Argc => "__fe_argc",
            Self::Argv => "__fe_argv",
        }
    }

    pub(super) fn from_accessor(symbol: &str) -> Option<Self> {
        Self::ALL
            .into_iter()
            .find(|state| state.accessor() == symbol)
    }
}

impl<I: LoweringInstSet> ModuleLowerer<'_, '_, I> {
    /// Defines `func`, the accessor of `state`, as a load of its global.
    pub(super) fn define_host_state_accessor(
        &mut self,
        func: FuncRef,
        state: HostState,
    ) -> Result<(), LowerError> {
        let ty = self
            .builder
            .sig(func, |sig| sig.ret_tys().first().copied())
            .ok_or_else(|| {
                LowerError::Internal(format!("`{}` returns no value", state.accessor()))
            })?;
        let global = self.builder.declare_gv(GlobalVariableData::new(
            state.global().to_string(),
            ty,
            Linkage::Private,
            false,
            None,
        ));
        let mut fb = self.builder.func_builder::<InstInserter>(func);
        let entry = fb.append_block();
        fb.switch_to_block(entry);
        let address = fb.make_global_value(global);
        let value = fb.insert_inst(Mload::new(self.inst_set, address, ty), ty);
        fb.insert_inst_no_result(Return::new_single(self.inst_set, value));
        fb.seal_all();
        fb.finish();
        Ok(())
    }
}
