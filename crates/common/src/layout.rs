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

/// Bytes in one storage slot.
pub const STORAGE_SLOT_BYTES: u32 = 32;

/// The storage footprint of one field, as the field planner sees it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StorageFieldShape {
    /// A scalar of `bytes` bytes (1..=32). Scalars narrower than a slot share
    /// slots with their neighbours.
    Scalar { bytes: u32 },
    /// An aggregate spanning `slots` whole slots. It starts a new slot, and so
    /// does the field after it. A zero-slot aggregate occupies no storage and
    /// does not interrupt packing.
    Aggregate { slots: u64 },
}

/// Where a scalar lives inside its slot: `byte_offset` counts from the
/// low-order end, like Solidity's `offset`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StorageLane {
    pub byte_offset: u32,
    pub byte_width: u32,
}

impl StorageLane {
    pub fn bit_offset(self) -> u32 {
        self.byte_offset * 8
    }

    pub fn bit_width(self) -> u32 {
        self.byte_width * 8
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StorageFieldPlacement {
    /// Slot offset from the start of the containing aggregate.
    pub slot: u64,
    /// The bytes of `slot` holding the field, for scalars narrower than a
    /// slot. `None` means the field takes whole slots.
    pub lane: Option<StorageLane>,
    /// Whether another field shares the slot, so a write must preserve the
    /// rest of the slot (read-modify-write).
    pub shared: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StorageFieldsLayout {
    pub placements: Vec<StorageFieldPlacement>,
    pub slots: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StorageLayoutError {
    Overflow,
    InvalidScalarWidth(u32),
}

/// Places fields in storage like Solidity places struct members: scalars
/// narrower than a slot are packed from the low-order end in declaration
/// order, a scalar that does not fit the rest of the slot starts the next
/// slot, and aggregates and full-slot scalars take whole slots.
pub fn storage_fields_layout(
    fields: impl IntoIterator<Item = StorageFieldShape>,
) -> Result<StorageFieldsLayout, StorageLayoutError> {
    let mut placements = Vec::new();
    let mut slot = 0u64;
    // Bytes used in `slot`; zero means `slot` is untouched.
    let mut used = 0u32;
    let mut group_start = 0usize;
    // Marks the lanes of one slot as shared when there is more than one.
    let close_group = |placements: &mut Vec<StorageFieldPlacement>, start: usize| {
        let lanes = placements[start..]
            .iter()
            .filter(|placement| placement.lane.is_some())
            .count();
        if lanes > 1 {
            for placement in &mut placements[start..] {
                placement.shared = placement.lane.is_some();
            }
        }
    };
    let next = |slot: u64, by: u64| slot.checked_add(by).ok_or(StorageLayoutError::Overflow);
    for field in fields {
        match field {
            StorageFieldShape::Scalar { bytes } if bytes == 0 || bytes > STORAGE_SLOT_BYTES => {
                return Err(StorageLayoutError::InvalidScalarWidth(bytes));
            }
            StorageFieldShape::Scalar { bytes } if bytes < STORAGE_SLOT_BYTES => {
                if used + bytes > STORAGE_SLOT_BYTES {
                    close_group(&mut placements, group_start);
                    slot = next(slot, 1)?;
                    used = 0;
                }
                if used == 0 {
                    group_start = placements.len();
                }
                placements.push(StorageFieldPlacement {
                    slot,
                    lane: Some(StorageLane {
                        byte_offset: used,
                        byte_width: bytes,
                    }),
                    shared: false,
                });
                used += bytes;
            }
            StorageFieldShape::Aggregate { slots: 0 } => {
                placements.push(StorageFieldPlacement {
                    slot,
                    lane: None,
                    shared: false,
                });
            }
            StorageFieldShape::Scalar { .. } | StorageFieldShape::Aggregate { .. } => {
                let slots = match field {
                    StorageFieldShape::Aggregate { slots } => slots,
                    _ => 1,
                };
                if used > 0 {
                    close_group(&mut placements, group_start);
                    slot = next(slot, 1)?;
                    used = 0;
                }
                placements.push(StorageFieldPlacement {
                    slot,
                    lane: None,
                    shared: false,
                });
                slot = next(slot, slots)?;
            }
        }
    }
    if used > 0 {
        close_group(&mut placements, group_start);
        slot = next(slot, 1)?;
    }
    Ok(StorageFieldsLayout {
        placements,
        slots: slot,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // Expected placements are `solc --storage-layout` output for the same
    // Solidity structs (slot, offset in bytes).
    fn scalar(bytes: u32) -> StorageFieldShape {
        StorageFieldShape::Scalar { bytes }
    }

    fn lane(slot: u64, byte_offset: u32, byte_width: u32, shared: bool) -> StorageFieldPlacement {
        StorageFieldPlacement {
            slot,
            lane: Some(StorageLane {
                byte_offset,
                byte_width,
            }),
            shared,
        }
    }

    fn whole(slot: u64) -> StorageFieldPlacement {
        StorageFieldPlacement {
            slot,
            lane: None,
            shared: false,
        }
    }

    #[test]
    fn packs_narrow_scalars_like_solidity() {
        // struct Small { uint8 a; uint8 b; uint256 c; }
        let small = storage_fields_layout([scalar(1), scalar(1), scalar(32)]).unwrap();
        assert_eq!(
            small.placements,
            [lane(0, 0, 1, true), lane(0, 1, 1, true), whole(1)]
        );
        assert_eq!(small.slots, 2);

        // struct Mixed { bool flag; uint16 b; int8 c; uint128 d; uint128 e; uint64 f; }
        let mixed = storage_fields_layout([
            scalar(1),
            scalar(2),
            scalar(1),
            scalar(16),
            scalar(16),
            scalar(8),
        ])
        .unwrap();
        assert_eq!(
            mixed.placements,
            [
                lane(0, 0, 1, true),
                lane(0, 1, 2, true),
                lane(0, 3, 1, true),
                lane(0, 4, 16, true),
                lane(1, 0, 16, true),
                lane(1, 16, 8, true),
            ]
        );
        assert_eq!(mixed.slots, 2);

        // struct Overflow { uint128 a; uint64 b; uint128 c; }
        let overflow = storage_fields_layout([scalar(16), scalar(8), scalar(16)]).unwrap();
        assert_eq!(
            overflow.placements,
            [
                lane(0, 0, 16, true),
                lane(0, 16, 8, true),
                lane(1, 0, 16, false)
            ]
        );
        assert_eq!(overflow.slots, 2);
    }

    #[test]
    fn aggregates_take_whole_slots_and_end_a_packing_run() {
        // struct Nested { uint8 before; Inner inner; uint8 afterField; }
        // with struct Inner { uint8 x; uint16 y; } taking one slot.
        let nested = storage_fields_layout([
            scalar(1),
            StorageFieldShape::Aggregate { slots: 1 },
            scalar(1),
        ])
        .unwrap();
        assert_eq!(
            nested.placements,
            [lane(0, 0, 1, false), whole(1), lane(2, 0, 1, false)]
        );
        assert_eq!(nested.slots, 3);
    }

    #[test]
    fn zero_slot_aggregates_do_not_interrupt_packing() {
        let layout = storage_fields_layout([
            scalar(1),
            StorageFieldShape::Aggregate { slots: 0 },
            scalar(1),
        ])
        .unwrap();
        assert_eq!(
            layout.placements,
            [lane(0, 0, 1, true), whole(0), lane(0, 1, 1, true)]
        );
        assert_eq!(layout.slots, 1);
        assert_eq!(storage_fields_layout([]).unwrap().slots, 0);
    }

    #[test]
    fn rejects_invalid_scalar_widths() {
        assert_eq!(
            storage_fields_layout([scalar(0)]),
            Err(StorageLayoutError::InvalidScalarWidth(0))
        );
        assert_eq!(
            storage_fields_layout([scalar(33)]),
            Err(StorageLayoutError::InvalidScalarWidth(33))
        );
    }

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
