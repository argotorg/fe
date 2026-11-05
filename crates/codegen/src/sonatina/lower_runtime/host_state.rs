//! The process state the native host capabilities read, through extern
//! functions that `std` declares and the compiler defines here. The entry
//! stores its `argc` and `argv` in private globals, which stay zero outside
//! an executable's entry; `environ` is the C library's.

use sonatina_ir::{
    Linkage,
    func_cursor::InstInserter,
    global_variable::GlobalVariableData,
    inst::{control_flow::Return, data::Mload},
    module::FuncRef,
};

use super::{LowerError, LoweringInstSet, ModuleLowerer};

/// A value of the process's that the host capabilities read.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum HostState {
    Argc,
    Argv,
    Environ,
}

impl HostState {
    pub(crate) const ALL: [Self; 3] = [Self::Argc, Self::Argv, Self::Environ];

    /// The values the native entry stores, in the order of its C arguments.
    #[cfg(feature = "cranelift")]
    pub(crate) const ENTRY: [Self; 2] = [Self::Argc, Self::Argv];

    /// The extern function `std` reads the value with.
    pub(crate) fn accessor(self) -> &'static str {
        match self {
            Self::Argc => "__fe_native_argc",
            Self::Argv => "__fe_native_argv",
            Self::Environ => "__fe_native_environ",
        }
    }

    /// The global that holds the value.
    pub(crate) fn global(self) -> &'static str {
        match self {
            Self::Argc => "__fe_argc",
            Self::Argv => "__fe_argv",
            Self::Environ => "environ",
        }
    }

    /// The module defines the entry's globals, and the C library `environ`.
    fn linkage(self) -> Linkage {
        match self {
            Self::Argc | Self::Argv => Linkage::Private,
            Self::Environ => Linkage::External,
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
            state.linkage(),
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
