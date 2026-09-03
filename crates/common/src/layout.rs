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
    let max_discriminant = variant_count.saturating_sub(1);

    if max_discriminant <= u8::MAX as usize {
        8
    } else if max_discriminant <= u16::MAX as usize {
        16
    } else if max_discriminant <= u32::MAX as usize {
        32
    } else {
        64
    }
}

#[cfg(test)]
mod tests {
    use super::enum_tag_bits;

    #[test]
    fn enum_tag_bits_uses_the_smallest_supported_width() {
        assert_eq!(enum_tag_bits(0), 8);
        assert_eq!(enum_tag_bits(1), 8);
        assert_eq!(enum_tag_bits(u8::MAX as usize + 1), 8);
        assert_eq!(enum_tag_bits(u8::MAX as usize + 2), 16);

        #[cfg(target_pointer_width = "16")]
        {
            assert_eq!(enum_tag_bits(usize::MAX), 16);
        }

        #[cfg(target_pointer_width = "32")]
        {
            assert_eq!(enum_tag_bits(u16::MAX as usize + 1), 16);
            assert_eq!(enum_tag_bits(u16::MAX as usize + 2), 32);
            assert_eq!(enum_tag_bits(usize::MAX), 32);
        }

        #[cfg(target_pointer_width = "64")]
        {
            assert_eq!(enum_tag_bits(u16::MAX as usize + 1), 16);
            assert_eq!(enum_tag_bits(u16::MAX as usize + 2), 32);
            assert_eq!(enum_tag_bits(u32::MAX as usize + 1), 32);
            assert_eq!(enum_tag_bits(u32::MAX as usize + 2), 64);
        }
    }
}
