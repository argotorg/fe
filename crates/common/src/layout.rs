#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TargetDataLayout {
    pub word_size_bytes: usize,
}

impl TargetDataLayout {
    pub const fn evm() -> Self {
        Self {
            word_size_bytes: 32,
        }
    }
}

pub const EVM_LAYOUT: TargetDataLayout = TargetDataLayout::evm();
pub const WORD_SIZE_BYTES: usize = EVM_LAYOUT.word_size_bytes;

pub const fn enum_tag_bits(variant_count: usize) -> u16 {
    if variant_count <= u8::MAX as usize + 1 {
        8
    } else if variant_count <= u16::MAX as usize + 1 {
        16
    } else if variant_count <= u32::MAX as usize + 1 {
        32
    } else {
        64
    }
}
