use std::fmt;

use smol_str::SmolStr;

use crate::{InputDb, layout::MemoryLayout};

/// The machine a compilation is for. Integer widths, layouts and destruction
/// depend on it, so a database compiles for one target at a time.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum Target {
    #[default]
    Evm,
    Native,
    /// WebAssembly: a 32-bit address space with native-style semantics.
    Wasm,
}

impl Target {
    pub const ALL: [Target; 3] = [Target::Evm, Target::Native, Target::Wasm];

    /// The target `name` names, as in `#[cfg(target = ..)]` and `fe.toml`.
    pub fn parse(name: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|target| target.name() == name)
    }

    /// The name `#[cfg(target = ..)]` matches.
    pub fn name(self) -> &'static str {
        match self {
            Target::Evm => "evm",
            Target::Native => "native",
            Target::Wasm => "wasm",
        }
    }

    /// The width of `usize`, `isize` and memory addresses.
    pub fn usize_bits(self) -> usize {
        match self {
            Target::Evm => 256,
            Target::Native => 64,
            Target::Wasm => 32,
        }
    }

    /// The memory layout of a scalar of `bits` bits. EVM memory packs a
    /// scalar into its bytes. On native and WebAssembly targets a scalar takes
    /// the integer type that holds it, as a Sonatina object field does: 1, 2,
    /// 4, 8, 16 or 32 bytes, aligned to its size up to 16.
    pub fn scalar_memory_layout(self, bits: u16) -> MemoryLayout {
        match self {
            Target::Evm => MemoryLayout {
                size: u64::from(bits.div_ceil(8)),
                align: 1,
            },
            Target::Native | Target::Wasm => {
                let size = (u64::from(bits).max(8).next_power_of_two() / 8).min(32);
                MemoryLayout {
                    size,
                    align: size.min(16),
                }
            }
        }
    }

    /// The width of a size in bytes of a value in memory: that of `usize`, at
    /// most 64 bits.
    pub fn memory_size_bits(self) -> usize {
        self.usize_bits().min(64)
    }

    /// The largest size in bytes of a value in memory.
    pub fn max_memory_size(self) -> u64 {
        u64::MAX >> (64 - self.memory_size_bits())
    }

    /// The memory layout of an address or reference, a `usize`.
    pub fn pointer_memory_layout(self) -> MemoryLayout {
        self.scalar_memory_layout(self.usize_bits() as u16)
    }

    /// Whether memory lays values out as Sonatina objects do, so that an
    /// aggregate in memory is an object in place. EVM memory packs fields.
    pub fn memory_has_object_layout(self) -> bool {
        self != Target::Evm
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

    /// The operating system `name` names, as in `#[cfg(target_os = ..)]`.
    pub fn parse(name: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|os| os.name() == name)
    }

    /// The name `#[cfg(target_os = ..)]` matches, as Rust's `std::env::consts::OS`.
    pub fn name(self) -> &'static str {
        match self {
            Os::Linux => "linux",
            Os::Macos => "macos",
        }
    }

    /// Whether `#[cfg(target_family = "unix")]` holds on it.
    pub fn is_unix(self) -> bool {
        match self {
            Os::Linux | Os::Macos => true,
        }
    }
}

impl fmt::Display for Os {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Os::Linux => "Linux",
            Os::Macos => "macOS",
        })
    }
}

/// The processor architecture native code runs on, which
/// `#[cfg(target_arch = ..)]` selects items by. Fe's integer widths and type
/// layouts are the same on every one.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Arch {
    X86_64,
    Aarch64,
}

impl Arch {
    pub const ALL: [Arch; 2] = [Arch::X86_64, Arch::Aarch64];

    /// The architecture `name` names, as in `#[cfg(target_arch = ..)]`.
    pub fn parse(name: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|arch| arch.name() == name)
    }

    /// The name `#[cfg(target_arch = ..)]` matches, as Rust's
    /// `std::env::consts::ARCH`.
    pub fn name(self) -> &'static str {
        match self {
            Arch::X86_64 => "x86_64",
            Arch::Aarch64 => "aarch64",
        }
    }
}

impl fmt::Display for Arch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Arch::X86_64 => "x86-64",
            Arch::Aarch64 => "AArch64",
        })
    }
}

/// The operating system and architecture native code runs on. Native code is
/// built for the host's platform, when it is one of [`Platform::SUPPORTED`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Platform {
    pub os: Os,
    pub arch: Arch,
}

impl Platform {
    /// The platforms native code supports.
    pub const SUPPORTED: [Platform; 3] = [
        Platform {
            os: Os::Linux,
            arch: Arch::X86_64,
        },
        Platform {
            os: Os::Linux,
            arch: Arch::Aarch64,
        },
        Platform {
            os: Os::Macos,
            arch: Arch::Aarch64,
        },
    ];

    /// The compiler's host platform, when native code supports it.
    pub fn host() -> Option<Platform> {
        let platform = Platform {
            os: Os::parse(std::env::consts::OS)?,
            arch: Arch::parse(std::env::consts::ARCH)?,
        };
        Self::SUPPORTED.contains(&platform).then_some(platform)
    }

    /// The supported platforms, for a message that refuses another host.
    pub fn supported_hosts() -> String {
        Self::SUPPORTED
            .map(|platform| platform.to_string())
            .join(", ")
    }
}

impl fmt::Display for Platform {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.arch, self.os)
    }
}

#[salsa::input]
#[derive(Debug)]
pub struct CompilationSettings {
    pub profile: SmolStr,
    #[returns(copy)]
    pub target: Target,
    /// None for EVM code, WebAssembly, and native code without an operating
    /// system, such as an SP1 guest.
    #[returns(copy)]
    pub platform: Option<Platform>,
}

#[salsa::tracked]
impl CompilationSettings {
    pub fn default(db: &dyn InputDb) -> Self {
        Self::new(db, SmolStr::new("dev"), Target::Evm, None)
    }
}
