// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract Bench {
    function and_(uint256 a, uint256 b) public pure returns (uint256) {
        return a & b;
    }

    function or_(uint256 a, uint256 b) public pure returns (uint256) {
        return a | b;
    }

    function xor_(uint256 a, uint256 b) public pure returns (uint256) {
        return a ^ b;
    }

    function shl_(uint256 a, uint256 b) public pure returns (uint256) {
        return a << b;
    }

    function shr_(uint256 a, uint256 b) public pure returns (uint256) {
        return a >> b;
    }
}
