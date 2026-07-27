#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TargetDataLayout {
    pub word_size_bytes: usize,
    pub discriminant_size_bytes: usize,
}

impl TargetDataLayout {
    pub const fn evm() -> Self {
        Self {
            word_size_bytes: 32,
            discriminant_size_bytes: 1,
        }
    }
}

pub const EVM_LAYOUT: TargetDataLayout = TargetDataLayout::evm();
pub const WORD_SIZE_BYTES: usize = EVM_LAYOUT.word_size_bytes;
pub const WORD_SIZE_BITS: u16 = (WORD_SIZE_BYTES * 8) as u16;
pub const DISCRIMINANT_SIZE_BYTES: usize = EVM_LAYOUT.discriminant_size_bytes;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StorageFieldShape {
    pub span_words: u64,
    pub bit_width: Option<u16>,
}

impl StorageFieldShape {
    pub const fn aggregate(span_words: u64) -> Self {
        Self {
            span_words,
            bit_width: None,
        }
    }

    pub const fn scalar(bit_width: u16) -> Self {
        Self {
            span_words: 1,
            bit_width: Some(bit_width),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StorageBitLane {
    pub bit_offset: u16,
    pub bit_width: u16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StorageFieldPlacement {
    pub word_offset: u64,
    pub lane: Option<StorageBitLane>,
    pub requires_read_modify_write: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StorageFieldsLayout {
    pub placements: Vec<StorageFieldPlacement>,
    pub span_words: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StorageLayoutError {
    InvalidBitWidth(u16),
    InvalidScalarSpan(u64),
    ExtentOverflow,
}

fn mark_shared_scalar_group(placements: &mut [StorageFieldPlacement], group_start: Option<usize>) {
    let Some(group_start) = group_start else {
        return;
    };
    if placements.len().saturating_sub(group_start) > 1 {
        for placement in &mut placements[group_start..] {
            placement.requires_read_modify_write = true;
        }
    }
}

pub fn storage_fields_layout(
    fields: impl IntoIterator<Item = StorageFieldShape>,
) -> Result<StorageFieldsLayout, StorageLayoutError> {
    let fields = fields.into_iter();
    let mut placements = Vec::with_capacity(fields.size_hint().0);
    let mut word_offset = 0u64;
    let mut used_bits = 0u16;
    let mut scalar_group_start = None;

    for field in fields {
        match field.bit_width {
            Some(0) => return Err(StorageLayoutError::InvalidBitWidth(0)),
            Some(bit_width) if bit_width > WORD_SIZE_BITS => {
                return Err(StorageLayoutError::InvalidBitWidth(bit_width));
            }
            Some(bit_width) => {
                if field.span_words != 1 {
                    return Err(StorageLayoutError::InvalidScalarSpan(field.span_words));
                }
                if bit_width == WORD_SIZE_BITS {
                    if used_bits > 0 {
                        mark_shared_scalar_group(&mut placements, scalar_group_start);
                        word_offset = word_offset
                            .checked_add(1)
                            .ok_or(StorageLayoutError::ExtentOverflow)?;
                        used_bits = 0;
                        scalar_group_start = None;
                    }
                    placements.push(StorageFieldPlacement {
                        word_offset,
                        lane: None,
                        requires_read_modify_write: false,
                    });
                    word_offset = word_offset
                        .checked_add(1)
                        .ok_or(StorageLayoutError::ExtentOverflow)?;
                    continue;
                }

                if u32::from(used_bits) + u32::from(bit_width) > u32::from(WORD_SIZE_BITS) {
                    mark_shared_scalar_group(&mut placements, scalar_group_start);
                    word_offset = word_offset
                        .checked_add(1)
                        .ok_or(StorageLayoutError::ExtentOverflow)?;
                    used_bits = 0;
                    scalar_group_start = None;
                }
                scalar_group_start.get_or_insert(placements.len());
                let placement_word_offset = word_offset;
                let lane = StorageBitLane {
                    bit_offset: used_bits,
                    bit_width,
                };
                used_bits += bit_width;
                placements.push(StorageFieldPlacement {
                    word_offset: placement_word_offset,
                    lane: Some(lane),
                    requires_read_modify_write: false,
                });
                if used_bits == WORD_SIZE_BITS {
                    mark_shared_scalar_group(&mut placements, scalar_group_start);
                    word_offset = word_offset
                        .checked_add(1)
                        .ok_or(StorageLayoutError::ExtentOverflow)?;
                    used_bits = 0;
                    scalar_group_start = None;
                }
            }
            None => {
                if used_bits > 0 {
                    mark_shared_scalar_group(&mut placements, scalar_group_start);
                    word_offset = word_offset
                        .checked_add(1)
                        .ok_or(StorageLayoutError::ExtentOverflow)?;
                    used_bits = 0;
                    scalar_group_start = None;
                }
                placements.push(StorageFieldPlacement {
                    word_offset,
                    lane: None,
                    requires_read_modify_write: false,
                });
                word_offset = word_offset
                    .checked_add(field.span_words)
                    .ok_or(StorageLayoutError::ExtentOverflow)?;
            }
        }
    }

    if used_bits > 0 {
        mark_shared_scalar_group(&mut placements, scalar_group_start);
        word_offset = word_offset
            .checked_add(1)
            .ok_or(StorageLayoutError::ExtentOverflow)?;
    }

    Ok(StorageFieldsLayout {
        placements,
        span_words: word_offset,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn packs_subword_scalars_and_preserves_aggregate_boundaries() {
        let layout = storage_fields_layout([
            StorageFieldShape::scalar(8),
            StorageFieldShape::scalar(16),
            StorageFieldShape::aggregate(2),
            StorageFieldShape::scalar(1),
        ])
        .unwrap();

        assert_eq!(layout.span_words, 4);
        assert_eq!(
            layout.placements,
            [
                StorageFieldPlacement {
                    word_offset: 0,
                    lane: Some(StorageBitLane {
                        bit_offset: 0,
                        bit_width: 8,
                    }),
                    requires_read_modify_write: true,
                },
                StorageFieldPlacement {
                    word_offset: 0,
                    lane: Some(StorageBitLane {
                        bit_offset: 8,
                        bit_width: 16,
                    }),
                    requires_read_modify_write: true,
                },
                StorageFieldPlacement {
                    word_offset: 1,
                    lane: None,
                    requires_read_modify_write: false,
                },
                StorageFieldPlacement {
                    word_offset: 3,
                    lane: Some(StorageBitLane {
                        bit_offset: 0,
                        bit_width: 1,
                    }),
                    requires_read_modify_write: false,
                },
            ]
        );
    }

    #[test]
    fn starts_a_new_word_when_the_next_lane_does_not_fit() {
        let layout = storage_fields_layout([
            StorageFieldShape::scalar(128),
            StorageFieldShape::scalar(128),
            StorageFieldShape::scalar(8),
        ])
        .unwrap();

        assert_eq!(layout.span_words, 2);
        assert!(layout.placements[0].requires_read_modify_write);
        assert!(layout.placements[1].requires_read_modify_write);
        assert_eq!(
            layout.placements[2],
            StorageFieldPlacement {
                word_offset: 1,
                lane: Some(StorageBitLane {
                    bit_offset: 0,
                    bit_width: 8,
                }),
                requires_read_modify_write: false,
            }
        );
    }

    #[test]
    fn gives_a_lone_subword_scalar_an_explicit_lane() {
        let layout =
            storage_fields_layout([StorageFieldShape::scalar(8)]).expect("valid storage layout");

        assert_eq!(layout.span_words, 1);
        assert_eq!(
            layout.placements[0].lane,
            Some(StorageBitLane {
                bit_offset: 0,
                bit_width: 8,
            })
        );
        assert!(!layout.placements[0].requires_read_modify_write);
    }
}
