// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripSol, BoolAddressPair, FeBenchCaller, IAbiRoundtrip, SolBenchCaller, StringU64Pair} from "../src/AbiRoundtripSol.sol";
import {AbiRoundtripBase} from "./generated/support/AbiRoundtripBase.sol";

contract DynArraySuiteEquivalenceTest is AbiRoundtripBase {
    function setUp() public override {
        solTarget = new AbiRoundtripSol();
        feTarget = deploy(fromHex(vm.readFile("fe-out/DynArraySuite.bin")));
        require(feTarget != address(0), "fe create failed");

        solBench = new SolBenchCaller(address(solTarget));
        feBench = new FeBenchCaller(feTarget);
    }

    function testEchoUintArrayDeterministic() public {
        uint256[] memory value = new uint256[](3);
        value[0] = 1;
        value[1] = 2;
        value[2] = type(uint256).max;

        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUintArray.selector, value);
        assertEquivalent(callData);

        uint256[] memory solValue = solBench.benchEchoUintArray(value);
        uint256[] memory feValue = feBench.benchEchoUintArray(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed uint[] mismatch");
    }

    function testEchoUintArrayFuzz(uint256[] memory value) public {
        value = truncateUintArray(value, 4);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUintArray.selector, value);
        assertEquivalent(callData);
        assertUintArrayTypedEquivalent(value);
    }

    function testEchoBoolAddressPairArrayDeterministic() public {
        BoolAddressPair[] memory value = new BoolAddressPair[](2);
        value[0] = BoolAddressPair({flag: true, addr: address(0x1234)});
        value[1] = BoolAddressPair({flag: false, addr: address(0xBEEF)});

        bytes memory callData =
            abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressPairArray.selector, value);
        assertEquivalent(callData);

        BoolAddressPair[] memory solValue = solBench.benchEchoBoolAddressPairArray(value);
        BoolAddressPair[] memory feValue = feBench.benchEchoBoolAddressPairArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed (bool,address)[] mismatch"
        );
    }

    function testEchoBoolAddressPairArrayFuzz(BoolAddressPair[] memory value) public {
        value = truncateBoolAddressPairArray(value, 4);
        bytes memory callData =
            abi.encodeWithSelector(IAbiRoundtrip.echoBoolAddressPairArray.selector, value);
        assertEquivalent(callData);
        assertBoolAddressPairArrayTypedEquivalent(value);
    }

    function testEchoStringArrayDeterministic() public {
        string[] memory value = new string[](3);
        value[0] = "alpha";
        value[1] = "beta gamma with extra payload bytes beyond thirty-two";
        value[2] = "delta with extra payload bytes beyond thirty-two";

        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringArray.selector, value);
        assertEquivalent(callData);

        string[] memory solValue = solBench.benchEchoStringArray(value);
        string[] memory feValue = feBench.benchEchoStringArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed string[] mismatch"
        );
    }

    function testEchoStringArrayFuzz(string[] memory value) public {
        value = truncateStringArray(value, 4, 96);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoStringArray.selector, value);
        assertEquivalent(callData);
        assertStringArrayTypedEquivalent(value);
    }

    function testEchoStringU64PairArrayDeterministic() public {
        StringU64Pair[] memory value = new StringU64Pair[](2);
        value[0] = StringU64Pair({text: "hello with extra payload bytes beyond thirty-two", count: 7});
        value[1] = StringU64Pair({text: "world with extra payload bytes beyond thirty-two", count: 11});

        bytes memory callData =
            abi.encodeWithSelector(IAbiRoundtrip.echoStringU64PairArray.selector, value);
        assertEquivalent(callData);

        StringU64Pair[] memory solValue = solBench.benchEchoStringU64PairArray(value);
        StringU64Pair[] memory feValue = feBench.benchEchoStringU64PairArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed (string,uint64)[] mismatch"
        );
    }

    function testEchoStringU64PairArrayFuzz(StringU64Pair[] memory value) public {
        value = truncateStringU64PairArray(value, 4, 96);
        bytes memory callData =
            abi.encodeWithSelector(IAbiRoundtrip.echoStringU64PairArray.selector, value);
        assertEquivalent(callData);
        assertStringU64PairArrayTypedEquivalent(value);
    }

    function assertUintArrayTypedEquivalent(uint256[] memory value) internal {
        uint256[] memory solValue = solBench.benchEchoUintArray(value);
        uint256[] memory feValue = feBench.benchEchoUintArray(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed uint[] mismatch");
    }

    function assertBoolAddressPairArrayTypedEquivalent(BoolAddressPair[] memory value) internal {
        BoolAddressPair[] memory solValue = solBench.benchEchoBoolAddressPairArray(value);
        BoolAddressPair[] memory feValue = feBench.benchEchoBoolAddressPairArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed (bool,address)[] mismatch"
        );
    }

    function assertStringArrayTypedEquivalent(string[] memory value) internal {
        string[] memory solValue = solBench.benchEchoStringArray(value);
        string[] memory feValue = feBench.benchEchoStringArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed string[] mismatch"
        );
    }

    function assertStringU64PairArrayTypedEquivalent(StringU64Pair[] memory value) internal {
        StringU64Pair[] memory solValue = solBench.benchEchoStringU64PairArray(value);
        StringU64Pair[] memory feValue = feBench.benchEchoStringU64PairArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed (string,uint64)[] mismatch"
        );
    }

    function truncateUintArray(uint256[] memory value, uint256 maxLen)
        internal
        pure
        returns (uint256[] memory out)
    {
        uint256 len = value.length;
        if (len > maxLen) len = maxLen;

        out = new uint256[](len);
        for (uint256 i = 0; i < len; i++) {
            out[i] = value[i];
        }
    }

    function truncateBoolAddressPairArray(BoolAddressPair[] memory value, uint256 maxLen)
        internal
        pure
        returns (BoolAddressPair[] memory out)
    {
        uint256 len = value.length;
        if (len > maxLen) len = maxLen;

        out = new BoolAddressPair[](len);
        for (uint256 i = 0; i < len; i++) {
            out[i] = value[i];
        }
    }

    function truncateStringArray(string[] memory value, uint256 maxLen, uint256 maxStringBytes)
        internal
        pure
        returns (string[] memory out)
    {
        uint256 len = value.length;
        if (len > maxLen) len = maxLen;

        out = new string[](len);
        for (uint256 i = 0; i < len; i++) {
            out[i] = truncateString(value[i], maxStringBytes);
        }
    }

    function truncateStringU64PairArray(
        StringU64Pair[] memory value,
        uint256 maxLen,
        uint256 maxStringBytes
    ) internal pure returns (StringU64Pair[] memory out) {
        uint256 len = value.length;
        if (len > maxLen) len = maxLen;

        out = new StringU64Pair[](len);
        for (uint256 i = 0; i < len; i++) {
            out[i] = StringU64Pair({
                text: truncateString(value[i].text, maxStringBytes),
                count: value[i].count
            });
        }
    }

    function truncateString(string memory text, uint256 maxBytes)
        internal
        pure
        returns (string memory)
    {
        return string(truncateBytes(bytes(text), maxBytes));
    }

    function truncateBytes(bytes memory value, uint256 maxLen) internal pure returns (bytes memory out) {
        uint256 len = value.length;
        if (len > maxLen) len = maxLen;

        out = new bytes(len);
        for (uint256 i = 0; i < len; i++) {
            out[i] = value[i];
        }
    }
}
