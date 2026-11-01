use smol_str::SmolStr;

use crate::{InputDb, layout::MemoryLayout};

/// The machine a compilation is for. Integer widths, layouts and destruction
/// depend on it, so a database compiles for one target at a time.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum Target {
    #[default]
    Evm,
    Native,
}

impl Target {
    pub const ALL: [Target; 2] = [Target::Evm, Target::Native];

    /// The name `#[cfg(target = ..)]` matches.
    pub fn name(self) -> &'static str {
        match self {
            Target::Evm => "evm",
            Target::Native => "native",
        }
    }

    /// The width of `usize`, `isize` and memory addresses.
    pub fn usize_bits(self) -> usize {
        match self {
            Target::Evm => 256,
            Target::Native => 64,
        }
    }

    /// The memory layout of a scalar of `bits` bits. EVM memory packs a
    /// scalar into its bytes. Natively a scalar takes the integer type that
    /// holds it, as a Sonatina object field does: 1, 2, 4, 8, 16 or 32 bytes,
    /// aligned to its size up to 16.
    pub fn scalar_memory_layout(self, bits: u16) -> MemoryLayout {
        match self {
            Target::Evm => MemoryLayout {
                size: u64::from(bits.div_ceil(8)),
                align: 1,
            },
            Target::Native => {
                let size = (u64::from(bits).max(8).next_power_of_two() / 8).min(32);
                MemoryLayout {
                    size,
                    align: size.min(16),
                }
            }
        }
    }

    /// The memory layout of an address or reference, a `usize`.
    pub fn pointer_memory_layout(self) -> MemoryLayout {
        self.scalar_memory_layout(self.usize_bits() as u16)
    }
}

/// The operating system native code runs on, which `#[cfg(target_os = ..)]`
/// selects items by. It doesn't change layouts.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Os {
    Linux,
    Macos,
}

impl Os {
    pub const ALL: [Os; 2] = [Os::Linux, Os::Macos];

    /// The operating system native builds target: the compiler's host, if
    /// native code supports it. Native code supports x86-64 Linux and
    /// AArch64 macOS, so code for an OS can assume its architecture.
    pub fn host() -> Option<Os> {
        if cfg!(all(target_arch = "x86_64", target_os = "linux")) {
            Some(Os::Linux)
        } else if cfg!(all(target_arch = "aarch64", target_os = "macos")) {
            Some(Os::Macos)
        } else {
            None
        }
    }

    /// The name `#[cfg(target_os = ..)]` matches.
    pub fn name(self) -> &'static str {
        match self {
            Os::Linux => "linux",
            Os::Macos => "macos",
        }
    }
}

#[salsa::input]
#[derive(Debug)]
pub struct CompilationSettings {
    pub profile: SmolStr,
    #[returns(copy)]
    pub target: Target,
    /// None for EVM code and for native code without an operating system,
    /// such as an SP1 guest.
    #[returns(copy)]
    pub os: Option<Os>,
}

#[salsa::tracked]
impl CompilationSettings {
    pub fn default(db: &dyn InputDb) -> Self {
        Self::new(db, SmolStr::new("dev"), Target::Evm, None)
    }
}
