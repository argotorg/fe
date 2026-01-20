//! Backend abstraction for multi-target code generation.
//!
//! This module defines the [`Backend`] trait that all code generation backends implement.
//! Fe supports multiple backends:
//! - [`YulBackend`]: Emits Yul text for compilation via solc (default)
//! - [`SonatinaBackend`]: Direct EVM bytecode generation via Sonatina IR (WIP)

use driver::DriverDataBase;
use hir::hir_def::TopLevelMod;
use mir::layout::TargetDataLayout;
use std::fmt;

/// Output produced by a backend compilation.
#[derive(Debug, Clone)]
pub enum BackendOutput {
    /// Yul text output (to be compiled by solc).
    Yul(String),
    /// Raw EVM bytecode (init code).
    Bytecode(Vec<u8>),
}

impl BackendOutput {
    /// Returns the Yul text if this is a Yul output.
    pub fn as_yul(&self) -> Option<&str> {
        match self {
            BackendOutput::Yul(s) => Some(s),
            _ => None,
        }
    }

    /// Returns the bytecode if this is a Bytecode output.
    pub fn as_bytecode(&self) -> Option<&[u8]> {
        match self {
            BackendOutput::Bytecode(b) => Some(b),
            _ => None,
        }
    }

    /// Consumes self and returns the Yul text if this is a Yul output.
    pub fn into_yul(self) -> Option<String> {
        match self {
            BackendOutput::Yul(s) => Some(s),
            _ => None,
        }
    }

    /// Consumes self and returns the bytecode if this is a Bytecode output.
    pub fn into_bytecode(self) -> Option<Vec<u8>> {
        match self {
            BackendOutput::Bytecode(b) => Some(b),
            _ => None,
        }
    }
}

/// Error type for backend compilation failures.
#[derive(Debug)]
pub enum BackendError {
    /// MIR lowering failed.
    MirLower(mir::MirLowerError),
    /// Yul emission failed.
    Yul(crate::yul::YulError),
    /// Sonatina compilation failed.
    Sonatina(String),
}

impl fmt::Display for BackendError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BackendError::MirLower(err) => write!(f, "{err}"),
            BackendError::Yul(err) => write!(f, "{err}"),
            BackendError::Sonatina(msg) => write!(f, "sonatina error: {msg}"),
        }
    }
}

impl std::error::Error for BackendError {}

impl From<mir::MirLowerError> for BackendError {
    fn from(err: mir::MirLowerError) -> Self {
        BackendError::MirLower(err)
    }
}

impl From<crate::yul::YulError> for BackendError {
    fn from(err: crate::yul::YulError) -> Self {
        BackendError::Yul(err)
    }
}

impl From<crate::EmitModuleError> for BackendError {
    fn from(err: crate::EmitModuleError) -> Self {
        match err {
            crate::EmitModuleError::MirLower(e) => BackendError::MirLower(e),
            crate::EmitModuleError::Yul(e) => BackendError::Yul(e),
        }
    }
}

/// A code generation backend that transforms HIR modules to target output.
///
/// Backends are responsible for:
/// 1. Lowering the HIR module to MIR
/// 2. Translating MIR to target-specific IR (if any)
/// 3. Producing the final output (Yul text or bytecode)
pub trait Backend {
    /// Returns the human-readable name of this backend.
    fn name(&self) -> &'static str;

    /// Compiles a top-level module to backend output.
    ///
    /// # Arguments
    /// * `db` - Driver database for compiler queries
    /// * `top_mod` - The HIR module to compile
    /// * `layout` - Target data layout for type sizing
    ///
    /// # Returns
    /// The compiled output or an error.
    fn compile(
        &self,
        db: &DriverDataBase,
        top_mod: TopLevelMod<'_>,
        layout: TargetDataLayout,
    ) -> Result<BackendOutput, BackendError>;
}

/// Available backend implementations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BackendKind {
    /// Yul backend (emits Yul text for solc).
    #[default]
    Yul,
    /// Sonatina backend (direct EVM bytecode generation).
    Sonatina,
}

impl BackendKind {
    /// Returns the name of this backend kind.
    pub fn name(&self) -> &'static str {
        match self {
            BackendKind::Yul => "yul",
            BackendKind::Sonatina => "sonatina",
        }
    }

    /// Creates a boxed backend instance for this kind.
    pub fn create(&self) -> Box<dyn Backend> {
        match self {
            BackendKind::Yul => Box::new(YulBackend),
            BackendKind::Sonatina => Box::new(SonatinaBackend),
        }
    }
}

impl std::str::FromStr for BackendKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "yul" => Ok(BackendKind::Yul),
            "sonatina" => Ok(BackendKind::Sonatina),
            _ => Err(format!("unknown backend: {s} (expected 'yul' or 'sonatina')")),
        }
    }
}

/// Yul backend implementation.
///
/// This wraps the existing Yul emitter to implement the [`Backend`] trait.
/// Output is Yul text that can be compiled by solc.
#[derive(Debug, Clone, Copy, Default)]
pub struct YulBackend;

impl Backend for YulBackend {
    fn name(&self) -> &'static str {
        "yul"
    }

    fn compile(
        &self,
        db: &DriverDataBase,
        top_mod: TopLevelMod<'_>,
        layout: TargetDataLayout,
    ) -> Result<BackendOutput, BackendError> {
        let yul = crate::emit_module_yul_with_layout(db, top_mod, layout)?;
        Ok(BackendOutput::Yul(yul))
    }
}

/// Sonatina backend implementation.
///
/// This backend produces EVM bytecode directly via Sonatina IR,
/// bypassing the need for solc.
#[derive(Debug, Clone, Copy, Default)]
pub struct SonatinaBackend;

impl Backend for SonatinaBackend {
    fn name(&self) -> &'static str {
        "sonatina"
    }

    fn compile(
        &self,
        db: &DriverDataBase,
        top_mod: TopLevelMod<'_>,
        layout: TargetDataLayout,
    ) -> Result<BackendOutput, BackendError> {
        use sonatina_codegen::isa::evm::EvmBackend;
        use sonatina_codegen::object::{CompileOptions, EvmObjectBackend, compile_object};
        use sonatina_ir::isa::evm::Evm;
        use sonatina_triple::{Architecture, EvmVersion, OperatingSystem, TargetTriple, Vendor};

        // Lower to Sonatina IR
        let module = crate::sonatina::compile_module(db, top_mod, layout)?;

        // Check if there are any objects to compile
        if module.objects.is_empty() {
            return Err(BackendError::Sonatina(
                "no objects to compile (module has no functions?)".to_string(),
            ));
        }

        // Create the EVM backend for codegen
        let triple = TargetTriple::new(
            Architecture::Evm,
            Vendor::Ethereum,
            OperatingSystem::Evm(EvmVersion::Osaka),
        );
        let isa = Evm::new(triple);
        let evm_backend = EvmBackend::new(isa);
        let object_backend = EvmObjectBackend::new(evm_backend);

        // Compile the "Contract" object
        let opts = CompileOptions::default();
        let artifact = compile_object(&module, &object_backend, "Contract", &opts)
            .map_err(|errors| {
                let msg = errors
                    .iter()
                    .map(|e| format!("{:?}", e))
                    .collect::<Vec<_>>()
                    .join("; ");
                BackendError::Sonatina(msg)
            })?;

        // Extract bytecode from the runtime section
        let runtime_section = artifact
            .sections
            .iter()
            .find(|(name, _)| name.0.as_str() == "runtime")
            .ok_or_else(|| {
                BackendError::Sonatina("compiled object has no runtime section".to_string())
            })?;

        Ok(BackendOutput::Bytecode(runtime_section.1.bytes.clone()))
    }
}
