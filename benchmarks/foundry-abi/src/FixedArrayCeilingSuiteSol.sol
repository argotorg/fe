// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

interface IFixedArrayCeilingSuite {
    function echoBoolArray17(bool[17] calldata value) external returns (bool[17] memory);
    function echoUintArray32(uint256[32] calldata value) external returns (uint256[32] memory);
    function echoStringArray17(string[17] calldata value) external returns (string[17] memory);
    function echoBytesArray17(bytes[17] calldata value) external returns (bytes[17] memory);
}

contract FixedArrayCeilingSuiteSol is IFixedArrayCeilingSuite {
    function echoBoolArray17(bool[17] calldata value) external pure returns (bool[17] memory) {
        return value;
    }

    function echoUintArray32(uint256[32] calldata value) external pure returns (uint256[32] memory) {
        return value;
    }

    function echoStringArray17(string[17] calldata value) external pure returns (string[17] memory) {
        return value;
    }

    function echoBytesArray17(bytes[17] calldata value) external pure returns (bytes[17] memory) {
        return value;
    }
}

contract FixedArrayCeilingSolBenchCaller {
    IFixedArrayCeilingSuite public immutable target;

    constructor(address target_) {
        target = IFixedArrayCeilingSuite(target_);
    }

    function benchEchoBoolArray17(bool[17] calldata value) external returns (bool[17] memory) {
        return target.echoBoolArray17(value);
    }

    function benchEchoUintArray32(uint256[32] calldata value) external returns (uint256[32] memory) {
        return target.echoUintArray32(value);
    }

    function benchEchoStringArray17(string[17] calldata value) external returns (string[17] memory) {
        return target.echoStringArray17(value);
    }

    function benchEchoBytesArray17(bytes[17] calldata value) external returns (bytes[17] memory) {
        return target.echoBytesArray17(value);
    }
}

contract FixedArrayCeilingFeBenchCaller {
    IFixedArrayCeilingSuite public immutable target;

    constructor(address target_) {
        target = IFixedArrayCeilingSuite(target_);
    }

    function benchEchoBoolArray17(bool[17] calldata value) external returns (bool[17] memory) {
        return target.echoBoolArray17(value);
    }

    function benchEchoUintArray32(uint256[32] calldata value) external returns (uint256[32] memory) {
        return target.echoUintArray32(value);
    }

    function benchEchoStringArray17(string[17] calldata value) external returns (string[17] memory) {
        return target.echoStringArray17(value);
    }

    function benchEchoBytesArray17(bytes[17] calldata value) external returns (bytes[17] memory) {
        return target.echoBytesArray17(value);
    }
}
