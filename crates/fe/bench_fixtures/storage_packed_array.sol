// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// Excerpted from Solady's LibMap (Uint8Map portion only).
// Source: https://github.com/Vectorized/solady/blob/main/src/utils/LibMap.sol
// License: MIT, (c) Vectorized.
library LibMap {
    struct Uint8Map {
        mapping(uint256 => uint256) map;
    }

    function get(Uint8Map storage map, uint256 index) internal view returns (uint8 result) {
        /// @solidity memory-safe-assembly
        assembly {
            mstore(0x20, map.slot)
            mstore(0x00, shr(5, index))
            result := byte(and(31, not(index)), sload(keccak256(0x00, 0x40)))
        }
    }

    function set(Uint8Map storage map, uint256 index, uint8 value) internal {
        /// @solidity memory-safe-assembly
        assembly {
            mstore(0x20, map.slot)
            mstore(0x00, shr(5, index))
            let s := keccak256(0x00, 0x40)
            mstore(0x00, sload(s))
            mstore8(and(31, not(index)), value)
            sstore(s, mload(0x00))
        }
    }
}

contract Bench {
    using LibMap for LibMap.Uint8Map;
    LibMap.Uint8Map internal pack;

    function set(uint256 idx, uint256 value) public {
        pack.set(idx, uint8(value));
    }

    function setTwoLanes(uint256 idxA, uint256 valA, uint256 idxB, uint256 valB) public {
        pack.set(idxA, uint8(valA));
        pack.set(idxB, uint8(valB));
    }

    function get(uint256 idx) public view returns (uint256) {
        return uint256(pack.get(idx));
    }

    function setThenGet(uint256 idx, uint256 value) public returns (uint256) {
        pack.set(idx, uint8(value));
        return uint256(pack.get(idx));
    }
}
