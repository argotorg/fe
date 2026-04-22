// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract Bench {
    function counter(uint256 n) public pure returns (uint256) {
        uint256 i = 0;
        while (i < n) {
            i = i + 1;
        }
        return i;
    }

    function sum(uint256 n) public pure returns (uint256) {
        uint256 total = 0;
        uint256 i = 1;
        while (i <= n) {
            total = total + i;
            i = i + 1;
        }
        return total;
    }

    function xorLoop(uint256 n, uint256 seed) public pure returns (uint256) {
        uint256 acc = seed;
        uint256 i = 0;
        while (i < n) {
            acc = acc ^ (seed ^ i);
            i = i + 1;
        }
        return acc;
    }

    function counterUnchecked(uint256 n) public pure returns (uint256) {
        uint256 i = 0;
        while (i < n) {
            unchecked { i = i + 1; }
        }
        return i;
    }

    function sumUnchecked(uint256 n) public pure returns (uint256) {
        uint256 total = 0;
        uint256 i = 1;
        while (i <= n) {
            unchecked {
                total = total + i;
                i = i + 1;
            }
        }
        return total;
    }

    function xorLoopUnchecked(uint256 n, uint256 seed) public pure returns (uint256) {
        uint256 acc = seed;
        uint256 i = 0;
        while (i < n) {
            unchecked {
                acc = acc ^ (seed ^ i);
                i = i + 1;
            }
        }
        return acc;
    }
}
