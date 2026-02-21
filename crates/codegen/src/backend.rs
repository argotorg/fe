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

/// Optimization level for code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OptLevel {
    /// No optimization — maximum debuggability.
    O0,
    /// Balanced optimization (default).
    #[default]
    O1,
    /// Aggressive optimization.
    O2,
}

impl std::str::FromStr for OptLevel {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "0" => Ok(OptLevel::O0),
            "1" => Ok(OptLevel::O1),
            "2" => Ok(OptLevel::O2),
            _ => Err(format!(
                "unknown optimization level: {s} (expected '0', '1', or '2')"
            )),
        }
    }
}

impl OptLevel {
    /// Whether the solc Yul optimizer should be enabled for this level.
    pub fn yul_optimize(&self) -> bool {
        !matches!(self, OptLevel::O0)
    }
}

impl fmt::Display for OptLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OptLevel::O0 => write!(f, "0"),
            OptLevel::O1 => write!(f, "1"),
            OptLevel::O2 => write!(f, "2"),
        }
    }
}

/// Output produced by a backend compilation.
#[derive(Debug, Clone)]
pub enum BackendOutput {
    /// Yul text output (to be compiled by solc).
    Yul { source: String, solc_optimize: bool },
    /// Raw EVM bytecode.
    Bytecode(Vec<u8>),
}

impl BackendOutput {
    /// Returns the Yul text if this is a Yul output.
    pub fn as_yul(&self) -> Option<&str> {
        match self {
            BackendOutput::Yul { source, .. } => Some(source),
            _ => None,
        }
    }

    /// Returns whether the solc optimizer should be enabled for this Yul output.
    pub fn yul_solc_optimize(&self) -> Option<bool> {
        match self {
            BackendOutput::Yul { solc_optimize, .. } => Some(*solc_optimize),
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
            BackendOutput::Yul { source, .. } => Some(source),
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
    /// * `opt_level` - Optimization level for code generation
    ///
    /// # Returns
    /// The compiled output or an error.
    fn compile(
        &self,
        db: &DriverDataBase,
        top_mod: TopLevelMod<'_>,
        layout: TargetDataLayout,
        opt_level: OptLevel,
    ) -> Result<BackendOutput, BackendError>;
}

/// Available backend implementations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BackendKind {
    /// Yul backend (emits Yul text for solc).
    Yul,
    /// Sonatina backend (direct EVM bytecode generation).
    #[default]
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
            _ => Err(format!(
                "unknown backend: {s} (expected 'yul' or 'sonatina')"
            )),
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
        opt_level: OptLevel,
    ) -> Result<BackendOutput, BackendError> {
        let yul = crate::emit_module_yul_with_layout(db, top_mod, layout)?;
        Ok(BackendOutput::Yul {
            source: yul,
            solc_optimize: opt_level.yul_optimize(),
        })
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
        opt_level: OptLevel,
    ) -> Result<BackendOutput, BackendError> {
        use sonatina_codegen::isa::evm::EvmBackend;
        use sonatina_codegen::object::{CompileOptions, compile_object};
        use sonatina_ir::object::{Directive, SectionRef};

        // Lower to Sonatina IR
        let mut module = crate::sonatina::compile_module(db, top_mod, layout)?;
        crate::sonatina::ensure_module_sonatina_ir_valid(&module)?;

        // Run the optimization pipeline based on opt_level.
        match opt_level {
            OptLevel::O0 => { /* no optimization */ }
            OptLevel::O1 => sonatina_codegen::optim::Pipeline::balanced().run(&mut module),
            OptLevel::O2 => sonatina_codegen::optim::Pipeline::aggressive().run(&mut module),
        }
        if opt_level != OptLevel::O0 {
            crate::sonatina::ensure_module_sonatina_ir_valid(&module)?;
        }

        // Check if there are any objects to compile
        if module.objects.is_empty() {
            return Err(BackendError::Sonatina(
                "no objects to compile (module has no functions?)".to_string(),
            ));
        }

        // Create the EVM backend for codegen
        let isa = crate::sonatina::create_evm_isa();
        let evm_backend = EvmBackend::new(isa);

        // Find root objects (not referenced by any other object's Embed directives).
        let mut referenced_objects = std::collections::BTreeSet::new();
        for object in module.objects.values() {
            for section in &object.sections {
                for directive in &section.directives {
                    let Directive::Embed(embed) = directive else {
                        continue;
                    };
                    let SectionRef::External { object, .. } = &embed.source else {
                        continue;
                    };
                    referenced_objects.insert(object.0.as_str().to_string());
                }
            }
        }

        let mut roots: Vec<String> = module
            .objects
            .keys()
            .filter(|name| !referenced_objects.contains(*name))
            .cloned()
            .collect();
        roots.sort();

        if roots.is_empty() {
            return Err(BackendError::Sonatina(
                "failed to select root object (all objects are referenced)".to_string(),
            ));
        }

        // Pick the primary root for output: prefer "Contract", otherwise first root.
        let object_name = if roots.iter().any(|n| n == "Contract") {
            "Contract".to_string()
        } else {
            roots[0].clone()
        };

        // Compile all root objects so codegen errors in any root are caught.
        let opts: CompileOptions<_> = CompileOptions::default();
        let mut primary_artifact = None;
        for root in &roots {
            let artifact =
                compile_object(&module, &evm_backend, root, &opts).map_err(|errors| {
                    let msg = errors
                        .iter()
                        .map(|e| format!("{:?}", e))
                        .collect::<Vec<_>>()
                        .join("; ");
                    BackendError::Sonatina(msg)
                })?;
            if *root == object_name {
                primary_artifact = Some(artifact);
            }
        }
        let artifact = primary_artifact.expect("primary root was in roots list");

        // Extract bytecode from the runtime section
        let section_name = sonatina_ir::object::SectionName::from("runtime");
        let runtime_section = artifact.sections.get(&section_name).ok_or_else(|| {
            BackendError::Sonatina("compiled object has no runtime section".to_string())
        })?;

        Ok(BackendOutput::Bytecode(runtime_section.bytes.clone()))
    }
}
