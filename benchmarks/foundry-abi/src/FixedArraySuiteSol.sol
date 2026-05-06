// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

struct FixedBoolAddressPair {
    bool flag;
    address addr;
}

struct FixedStringU64Pair {
    string text;
    uint64 count;
}

struct FixedBytesU64Pair {
    bytes data;
    uint64 count;
}

interface IFixedArraySuite {
    function echoBoolArray5(bool[5] calldata value) external returns (bool[5] memory);
    function echoBoolArray17(bool[17] calldata value) external returns (bool[17] memory);
    function echoUintArray8(uint256[8] calldata value) external returns (uint256[8] memory);
    function echoUintArray16(uint256[16] calldata value) external returns (uint256[16] memory);
    function echoUintArray32(uint256[32] calldata value) external returns (uint256[32] memory);
    function echoStringArray5(string[5] calldata value) external returns (string[5] memory);
    function echoStringArray17(string[17] calldata value) external returns (string[17] memory);
    function echoBytesArray5(bytes[5] calldata value) external returns (bytes[5] memory);
    function echoBytesArray17(bytes[17] calldata value) external returns (bytes[17] memory);
    function echoBoolAddressPairArray8(FixedBoolAddressPair[8] calldata value)
        external
        returns (FixedBoolAddressPair[8] memory);
    function echoStringU64PairArray5(FixedStringU64Pair[5] calldata value)
        external
        returns (FixedStringU64Pair[5] memory);
    function echoBytesU64PairArray5(FixedBytesU64Pair[5] calldata value) external returns (FixedBytesU64Pair[5] memory);
    function echoNestedUintArray2x5(uint256[5][2] calldata value) external returns (uint256[5][2] memory);
}

contract FixedArraySuiteSol is IFixedArraySuite {
    function echoBoolArray5(bool[5] calldata value) external pure returns (bool[5] memory) {
        return value;
    }

    function echoBoolArray17(bool[17] calldata value) external pure returns (bool[17] memory) {
        return value;
    }

    function echoUintArray8(uint256[8] calldata value) external pure returns (uint256[8] memory) {
        return value;
    }

    function echoUintArray16(uint256[16] calldata value) external pure returns (uint256[16] memory) {
        return value;
    }

    function echoUintArray32(uint256[32] calldata value) external pure returns (uint256[32] memory) {
        return value;
    }

    function echoStringArray5(string[5] calldata value) external pure returns (string[5] memory) {
        return value;
    }

    function echoStringArray17(string[17] calldata value) external pure returns (string[17] memory) {
        return value;
    }

    function echoBytesArray5(bytes[5] calldata value) external pure returns (bytes[5] memory) {
        return value;
    }

    function echoBytesArray17(bytes[17] calldata value) external pure returns (bytes[17] memory) {
        return value;
    }

    function echoBoolAddressPairArray8(FixedBoolAddressPair[8] calldata value)
        external
        pure
        returns (FixedBoolAddressPair[8] memory)
    {
        return value;
    }

    function echoStringU64PairArray5(FixedStringU64Pair[5] calldata value)
        external
        pure
        returns (FixedStringU64Pair[5] memory)
    {
        return value;
    }

    function echoBytesU64PairArray5(FixedBytesU64Pair[5] calldata value)
        external
        pure
        returns (FixedBytesU64Pair[5] memory)
    {
        return value;
    }

    function echoNestedUintArray2x5(uint256[5][2] calldata value) external pure returns (uint256[5][2] memory) {
        return value;
    }
}

contract FixedArraySolBenchCaller {
    IFixedArraySuite public immutable target;

    constructor(address target_) {
        target = IFixedArraySuite(target_);
    }

    function benchEchoBoolArray5(bool[5] calldata value) external returns (bool[5] memory) {
        return target.echoBoolArray5(value);
    }

    function benchEchoBoolArray17(bool[17] calldata value) external returns (bool[17] memory) {
        return target.echoBoolArray17(value);
    }

    function benchEchoUintArray8(uint256[8] calldata value) external returns (uint256[8] memory) {
        return target.echoUintArray8(value);
    }

    function benchEchoUintArray16(uint256[16] calldata value) external returns (uint256[16] memory) {
        return target.echoUintArray16(value);
    }

    function benchEchoUintArray32(uint256[32] calldata value) external returns (uint256[32] memory) {
        return target.echoUintArray32(value);
    }

    function benchEchoStringArray5(string[5] calldata value) external returns (string[5] memory) {
        return target.echoStringArray5(value);
    }

    function benchEchoStringArray17(string[17] calldata value) external returns (string[17] memory) {
        return target.echoStringArray17(value);
    }

    function benchEchoBytesArray5(bytes[5] calldata value) external returns (bytes[5] memory) {
        return target.echoBytesArray5(value);
    }

    function benchEchoBytesArray17(bytes[17] calldata value) external returns (bytes[17] memory) {
        return target.echoBytesArray17(value);
    }

    function benchEchoBoolAddressPairArray8(FixedBoolAddressPair[8] calldata value)
        external
        returns (FixedBoolAddressPair[8] memory)
    {
        return target.echoBoolAddressPairArray8(value);
    }

    function benchEchoStringU64PairArray5(FixedStringU64Pair[5] calldata value)
        external
        returns (FixedStringU64Pair[5] memory)
    {
        return target.echoStringU64PairArray5(value);
    }

    function benchEchoBytesU64PairArray5(FixedBytesU64Pair[5] calldata value)
        external
        returns (FixedBytesU64Pair[5] memory)
    {
        return target.echoBytesU64PairArray5(value);
    }

    function benchEchoNestedUintArray2x5(uint256[5][2] calldata value) external returns (uint256[5][2] memory) {
        return target.echoNestedUintArray2x5(value);
    }
}

contract FixedArrayFeBenchCaller {
    IFixedArraySuite public immutable target;

    constructor(address target_) {
        target = IFixedArraySuite(target_);
    }

    function benchEchoBoolArray5(bool[5] calldata value) external returns (bool[5] memory) {
        return target.echoBoolArray5(value);
    }

    function benchEchoBoolArray17(bool[17] calldata value) external returns (bool[17] memory) {
        return target.echoBoolArray17(value);
    }

    function benchEchoUintArray8(uint256[8] calldata value) external returns (uint256[8] memory) {
        return target.echoUintArray8(value);
    }

    function benchEchoUintArray16(uint256[16] calldata value) external returns (uint256[16] memory) {
        return target.echoUintArray16(value);
    }

    function benchEchoUintArray32(uint256[32] calldata value) external returns (uint256[32] memory) {
        return target.echoUintArray32(value);
    }

    function benchEchoStringArray5(string[5] calldata value) external returns (string[5] memory) {
        return target.echoStringArray5(value);
    }

    function benchEchoStringArray17(string[17] calldata value) external returns (string[17] memory) {
        return target.echoStringArray17(value);
    }

    function benchEchoBytesArray5(bytes[5] calldata value) external returns (bytes[5] memory) {
        return target.echoBytesArray5(value);
    }

    function benchEchoBytesArray17(bytes[17] calldata value) external returns (bytes[17] memory) {
        return target.echoBytesArray17(value);
    }

    function benchEchoBoolAddressPairArray8(FixedBoolAddressPair[8] calldata value)
        external
        returns (FixedBoolAddressPair[8] memory)
    {
        return target.echoBoolAddressPairArray8(value);
    }

    function benchEchoStringU64PairArray5(FixedStringU64Pair[5] calldata value)
        external
        returns (FixedStringU64Pair[5] memory)
    {
        return target.echoStringU64PairArray5(value);
    }

    function benchEchoBytesU64PairArray5(FixedBytesU64Pair[5] calldata value)
        external
        returns (FixedBytesU64Pair[5] memory)
    {
        return target.echoBytesU64PairArray5(value);
    }

    function benchEchoNestedUintArray2x5(uint256[5][2] calldata value) external returns (uint256[5][2] memory) {
        return target.echoNestedUintArray2x5(value);
    }
}
