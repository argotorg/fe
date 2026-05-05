// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

// Use suite-prefixed names to avoid collisions with the generated AbiRoundtrip
// harness and other focused suites in the same Foundry compilation unit.
struct NTBoolAddressPair {
    bool flag;
    address addr;
}

struct NTAddressU256Pair {
    address addr;
    uint256 count;
}

// ABI type: ((bool,address),uint256)
struct NTNestedStatic {
    NTBoolAddressPair inner;
    uint256 count;
}

// ABI type: (bool,(address,uint256))
struct NTNestedStaticFlipped {
    bool flag;
    NTAddressU256Pair inner;
}

struct NTStringU64Pair {
    string text;
    uint64 count;
}

// ABI type: ((string,uint64),bool)
struct NTNestedDynamic {
    NTStringU64Pair pair;
    bool flag;
}

struct NTBytesBoolPair {
    bytes data;
    bool flag;
}

// ABI type: ((string,uint64),(bytes,bool))
struct NTNestedDynamicBoth {
    NTStringU64Pair left;
    NTBytesBoolPair right;
}

interface INestedTupleSuite {
    function echoNestedStatic(NTNestedStatic calldata value) external returns (NTNestedStatic memory);
    function echoNestedStaticFlipped(NTNestedStaticFlipped calldata value)
        external
        returns (NTNestedStaticFlipped memory);
    function echoNestedDynamic(NTNestedDynamic calldata value) external returns (NTNestedDynamic memory);
    function echoNestedDynamicBoth(NTNestedDynamicBoth calldata value) external returns (NTNestedDynamicBoth memory);

    function echoNestedStaticArray(NTNestedStatic[4] calldata value)
        external
        returns (NTNestedStatic[4] memory);
    function echoNestedStaticDynArray(NTNestedStatic[] calldata value)
        external
        returns (NTNestedStatic[] memory);
}

contract NestedTupleSuiteSol is INestedTupleSuite {
    function echoNestedStatic(NTNestedStatic calldata value) external pure returns (NTNestedStatic memory) {
        return value;
    }

    function echoNestedStaticFlipped(NTNestedStaticFlipped calldata value)
        external
        pure
        returns (NTNestedStaticFlipped memory)
    {
        return value;
    }

    function echoNestedDynamic(NTNestedDynamic calldata value) external pure returns (NTNestedDynamic memory) {
        return value;
    }

    function echoNestedDynamicBoth(NTNestedDynamicBoth calldata value) external pure returns (NTNestedDynamicBoth memory) {
        return value;
    }

    function echoNestedStaticArray(NTNestedStatic[4] calldata value)
        external
        pure
        returns (NTNestedStatic[4] memory)
    {
        return value;
    }

    function echoNestedStaticDynArray(NTNestedStatic[] calldata value)
        external
        pure
        returns (NTNestedStatic[] memory)
    {
        return value;
    }
}

contract NestedTupleSolBenchCaller {
    INestedTupleSuite public immutable target;

    constructor(address target_) {
        target = INestedTupleSuite(target_);
    }

    function benchEchoNestedStatic(NTNestedStatic calldata value) external returns (NTNestedStatic memory) {
        return target.echoNestedStatic(value);
    }

    function benchEchoNestedStaticFlipped(NTNestedStaticFlipped calldata value)
        external
        returns (NTNestedStaticFlipped memory)
    {
        return target.echoNestedStaticFlipped(value);
    }

    function benchEchoNestedDynamic(NTNestedDynamic calldata value) external returns (NTNestedDynamic memory) {
        return target.echoNestedDynamic(value);
    }

    function benchEchoNestedDynamicBoth(NTNestedDynamicBoth calldata value) external returns (NTNestedDynamicBoth memory) {
        return target.echoNestedDynamicBoth(value);
    }

    function benchEchoNestedStaticArray(NTNestedStatic[4] calldata value)
        external
        returns (NTNestedStatic[4] memory)
    {
        return target.echoNestedStaticArray(value);
    }

    function benchEchoNestedStaticDynArray(NTNestedStatic[] calldata value)
        external
        returns (NTNestedStatic[] memory)
    {
        return target.echoNestedStaticDynArray(value);
    }
}

contract NestedTupleFeBenchCaller {
    INestedTupleSuite public immutable target;

    constructor(address target_) {
        target = INestedTupleSuite(target_);
    }

    function benchEchoNestedStatic(NTNestedStatic calldata value) external returns (NTNestedStatic memory) {
        return target.echoNestedStatic(value);
    }

    function benchEchoNestedStaticFlipped(NTNestedStaticFlipped calldata value)
        external
        returns (NTNestedStaticFlipped memory)
    {
        return target.echoNestedStaticFlipped(value);
    }

    function benchEchoNestedDynamic(NTNestedDynamic calldata value) external returns (NTNestedDynamic memory) {
        return target.echoNestedDynamic(value);
    }

    function benchEchoNestedDynamicBoth(NTNestedDynamicBoth calldata value) external returns (NTNestedDynamicBoth memory) {
        return target.echoNestedDynamicBoth(value);
    }

    function benchEchoNestedStaticArray(NTNestedStatic[4] calldata value)
        external
        returns (NTNestedStatic[4] memory)
    {
        return target.echoNestedStaticArray(value);
    }

    function benchEchoNestedStaticDynArray(NTNestedStatic[] calldata value)
        external
        returns (NTNestedStatic[] memory)
    {
        return target.echoNestedStaticDynArray(value);
    }
}

