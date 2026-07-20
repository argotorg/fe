// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

struct StringU64PairDyn {
    string text;
    uint64 count;
}

struct BytesU64Pair {
    bytes data;
    uint64 count;
}

interface IDeepDynamicSuite {
    function echoUint24Array(uint24[] calldata value) external returns (uint24[] memory);
    function echoBytesArray(bytes[] calldata value) external returns (bytes[] memory);
    function echoNestedBytesArray(bytes[][] calldata value) external returns (bytes[][] memory);
    function echoNestedUintArray(uint256[][] calldata value) external returns (uint256[][] memory);
    function echoNestedStringArray(string[][] calldata value) external returns (string[][] memory);
    function echoNestedStringU64PairArray(StringU64PairDyn[][] calldata value)
        external
        returns (StringU64PairDyn[][] memory);
    function echoBytesU64Pair(BytesU64Pair calldata value) external returns (BytesU64Pair memory);
    function echoBytesU64PairArray(BytesU64Pair[] calldata value)
        external
        returns (BytesU64Pair[] memory);
    function echoNestedBytesU64PairArray(BytesU64Pair[][] calldata value)
        external
        returns (BytesU64Pair[][] memory);
}

contract DeepDynamicSuiteSol is IDeepDynamicSuite {
    function echoUint24Array(uint24[] calldata value) external pure returns (uint24[] memory) {
        return value;
    }

    function echoBytesArray(bytes[] calldata value) external pure returns (bytes[] memory) {
        return value;
    }

    function echoNestedBytesArray(bytes[][] calldata value)
        external
        pure
        returns (bytes[][] memory)
    {
        return value;
    }

    function echoNestedUintArray(uint256[][] calldata value)
        external
        pure
        returns (uint256[][] memory)
    {
        return value;
    }

    function echoNestedStringArray(string[][] calldata value)
        external
        pure
        returns (string[][] memory)
    {
        return value;
    }

    function echoNestedStringU64PairArray(StringU64PairDyn[][] calldata value)
        external
        pure
        returns (StringU64PairDyn[][] memory)
    {
        return value;
    }

    function echoBytesU64Pair(BytesU64Pair calldata value)
        external
        pure
        returns (BytesU64Pair memory)
    {
        return value;
    }

    function echoBytesU64PairArray(BytesU64Pair[] calldata value)
        external
        pure
        returns (BytesU64Pair[] memory)
    {
        return value;
    }

    function echoNestedBytesU64PairArray(BytesU64Pair[][] calldata value)
        external
        pure
        returns (BytesU64Pair[][] memory)
    {
        return value;
    }
}

contract DeepDynamicSolBenchCaller {
    IDeepDynamicSuite public immutable target;

    constructor(address target_) {
        target = IDeepDynamicSuite(target_);
    }

    function benchEchoUint24Array(uint24[] calldata value) external returns (uint24[] memory) {
        return target.echoUint24Array(value);
    }

    function benchEchoBytesArray(bytes[] calldata value) external returns (bytes[] memory) {
        return target.echoBytesArray(value);
    }

    function benchEchoNestedBytesArray(bytes[][] calldata value)
        external
        returns (bytes[][] memory)
    {
        return target.echoNestedBytesArray(value);
    }

    function benchEchoNestedUintArray(uint256[][] calldata value)
        external
        returns (uint256[][] memory)
    {
        return target.echoNestedUintArray(value);
    }

    function benchEchoNestedStringArray(string[][] calldata value)
        external
        returns (string[][] memory)
    {
        return target.echoNestedStringArray(value);
    }

    function benchEchoNestedStringU64PairArray(StringU64PairDyn[][] calldata value)
        external
        returns (StringU64PairDyn[][] memory)
    {
        return target.echoNestedStringU64PairArray(value);
    }

    function benchEchoBytesU64Pair(BytesU64Pair calldata value)
        external
        returns (BytesU64Pair memory)
    {
        return target.echoBytesU64Pair(value);
    }

    function benchEchoBytesU64PairArray(BytesU64Pair[] calldata value)
        external
        returns (BytesU64Pair[] memory)
    {
        return target.echoBytesU64PairArray(value);
    }

    function benchEchoNestedBytesU64PairArray(BytesU64Pair[][] calldata value)
        external
        returns (BytesU64Pair[][] memory)
    {
        return target.echoNestedBytesU64PairArray(value);
    }
}

contract DeepDynamicFeBenchCaller {
    IDeepDynamicSuite public immutable target;

    constructor(address target_) {
        target = IDeepDynamicSuite(target_);
    }

    function benchEchoUint24Array(uint24[] calldata value) external returns (uint24[] memory) {
        return target.echoUint24Array(value);
    }

    function benchEchoBytesArray(bytes[] calldata value) external returns (bytes[] memory) {
        return target.echoBytesArray(value);
    }

    function benchEchoNestedBytesArray(bytes[][] calldata value)
        external
        returns (bytes[][] memory)
    {
        return target.echoNestedBytesArray(value);
    }

    function benchEchoNestedUintArray(uint256[][] calldata value)
        external
        returns (uint256[][] memory)
    {
        return target.echoNestedUintArray(value);
    }

    function benchEchoNestedStringArray(string[][] calldata value)
        external
        returns (string[][] memory)
    {
        return target.echoNestedStringArray(value);
    }

    function benchEchoNestedStringU64PairArray(StringU64PairDyn[][] calldata value)
        external
        returns (StringU64PairDyn[][] memory)
    {
        return target.echoNestedStringU64PairArray(value);
    }

    function benchEchoBytesU64Pair(BytesU64Pair calldata value)
        external
        returns (BytesU64Pair memory)
    {
        return target.echoBytesU64Pair(value);
    }

    function benchEchoBytesU64PairArray(BytesU64Pair[] calldata value)
        external
        returns (BytesU64Pair[] memory)
    {
        return target.echoBytesU64PairArray(value);
    }

    function benchEchoNestedBytesU64PairArray(BytesU64Pair[][] calldata value)
        external
        returns (BytesU64Pair[][] memory)
    {
        return target.echoNestedBytesU64PairArray(value);
    }
}
