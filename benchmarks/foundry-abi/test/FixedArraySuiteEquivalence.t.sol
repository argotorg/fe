// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {
    FixedArraySuiteSol,
    FixedArraySolBenchCaller,
    FixedArrayFeBenchCaller,
    IFixedArraySuite,
    FixedBoolAddressPair,
    FixedStringU64Pair,
    FixedBytesU64Pair
} from "../src/FixedArraySuiteSol.sol";

interface Vm {
    function readFile(string calldata path) external returns (string memory);
}

contract FixedArraySuiteEquivalenceTest {
    Vm constant vm = Vm(address(uint160(uint256(keccak256("hevm cheat code")))));

    FixedArraySuiteSol internal solTarget;
    address internal feTarget;
    FixedArraySolBenchCaller internal solBench;
    FixedArrayFeBenchCaller internal feBench;

    function setUp() public {
        solTarget = new FixedArraySuiteSol();
        feTarget = deploy(fromHex(vm.readFile("fe-out/FixedArraySuite.bin")));
        require(feTarget != address(0), "fe create failed");

        solBench = new FixedArraySolBenchCaller(address(solTarget));
        feBench = new FixedArrayFeBenchCaller(feTarget);
    }

    function testEchoBoolArray5Deterministic() public {
        bool[5] memory value = [true, false, true, false, true];
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoBoolArray5.selector, value));
        assertBoolArray5TypedEquivalent(value);
    }

    function testEchoBoolArray5Fuzz(bool[5] memory value) public {
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoBoolArray5.selector, value));
        assertBoolArray5TypedEquivalent(value);
    }

    function testEchoBoolArray17Deterministic() public {
        bool[17] memory value;
        for (uint256 i = 0; i < 17; i++) {
            value[i] = i % 2 == 0;
        }

        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoBoolArray17.selector, value));
        assertBoolArray17TypedEquivalent(value);
    }

    function testEchoBoolArray17Fuzz(bool[17] memory value) public {
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoBoolArray17.selector, value));
        assertBoolArray17TypedEquivalent(value);
    }

    function testEchoUintArray8Deterministic() public {
        uint256[8] memory value = [uint256(1), 2, 3, 4, 5, 6, 7, type(uint256).max];
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoUintArray8.selector, value));
        assertUintArray8TypedEquivalent(value);
    }

    function testEchoUintArray8Fuzz(uint256[8] memory value) public {
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoUintArray8.selector, value));
        assertUintArray8TypedEquivalent(value);
    }

    function testEchoUintArray16Deterministic() public {
        uint256[16] memory value = [uint256(1), 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, type(uint256).max];
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoUintArray16.selector, value));
        assertUintArray16TypedEquivalent(value);
    }

    function testEchoUintArray16Fuzz(uint256[16] memory value) public {
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoUintArray16.selector, value));
        assertUintArray16TypedEquivalent(value);
    }

    function testEchoUintArray32Deterministic() public {
        uint256[32] memory value;
        for (uint256 i = 0; i < 32; i++) {
            value[i] = i + 1;
        }
        value[31] = type(uint256).max;

        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoUintArray32.selector, value));
        assertUintArray32TypedEquivalent(value);
    }

    function testEchoUintArray32Fuzz(uint256[32] memory value) public {
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoUintArray32.selector, value));
        assertUintArray32TypedEquivalent(value);
    }

    function testEchoStringArray5Deterministic() public {
        string[5] memory value;
        value[0] = "alpha";
        value[1] = "beta gamma with extra payload bytes beyond thirty-two";
        value[2] = "delta with extra payload bytes beyond thirty-two";
        value[3] = "epsilon with extra payload bytes beyond thirty-two";
        value[4] = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoStringArray5.selector, value));
        assertStringArray5TypedEquivalent(value);
    }

    function testEchoStringArray5Fuzz(string[5] memory value) public {
        value = normalizeStringArray5(value, 96);
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoStringArray5.selector, value));
        assertStringArray5TypedEquivalent(value);
    }

    function testEchoStringArray17Deterministic() public {
        string[17] memory value;
        value[0] = "";
        value[1] = "alpha";
        value[2] = "beta gamma with extra payload bytes beyond thirty-two";
        value[8] = "delta with extra payload bytes beyond thirty-two";
        value[9] = "epsilon with extra payload bytes beyond thirty-two";
        value[16] = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoStringArray17.selector, value));
        assertStringArray17TypedEquivalent(value);
    }

    function testEchoStringArray17Fuzz(string[17] memory value) public {
        value = normalizeStringArray17(value, 96);
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoStringArray17.selector, value));
        assertStringArray17TypedEquivalent(value);
    }

    function testEchoBytesArray5Deterministic() public {
        bytes[5] memory value;
        value[0] = hex"";
        value[1] = hex"01";
        value[2] = hex"deadbeef";
        value[3] = bytes("payload");
        value[4] = hex"ffffffffffffffffffffffffffffffff";

        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoBytesArray5.selector, value));
        assertBytesArray5TypedEquivalent(value);
    }

    function testEchoBytesArray5Fuzz(bytes[5] memory value) public {
        value = normalizeBytesArray5(value, 64);
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoBytesArray5.selector, value));
        assertBytesArray5TypedEquivalent(value);
    }

    function testEchoBytesArray17Deterministic() public {
        bytes[17] memory value;
        value[0] = hex"";
        value[1] = hex"01";
        value[2] = hex"deadbeef";
        value[8] = bytes("payload");
        value[9] = hex"ffffffffffffffffffffffffffffffff";
        value[16] = hex"00112233445566778899aabbccddeeff";

        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoBytesArray17.selector, value));
        assertBytesArray17TypedEquivalent(value);
    }

    function testEchoBytesArray17Fuzz(bytes[17] memory value) public {
        value = normalizeBytesArray17(value, 64);
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoBytesArray17.selector, value));
        assertBytesArray17TypedEquivalent(value);
    }

    function testEchoBoolAddressPairArray8Deterministic() public {
        FixedBoolAddressPair[8] memory value;
        value[0] = FixedBoolAddressPair({flag: true, addr: address(0x1001)});
        value[1] = FixedBoolAddressPair({flag: false, addr: address(0x1002)});
        value[2] = FixedBoolAddressPair({flag: true, addr: address(0x1003)});
        value[3] = FixedBoolAddressPair({flag: false, addr: address(0x1004)});
        value[4] = FixedBoolAddressPair({flag: true, addr: address(0x1005)});
        value[5] = FixedBoolAddressPair({flag: false, addr: address(0x1006)});
        value[6] = FixedBoolAddressPair({flag: true, addr: address(0x1007)});
        value[7] = FixedBoolAddressPair({flag: false, addr: address(0x1008)});

        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoBoolAddressPairArray8.selector, value));
        assertBoolAddressPairArray8TypedEquivalent(value);
    }

    function testEchoBoolAddressPairArray8Fuzz(FixedBoolAddressPair[8] memory value) public {
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoBoolAddressPairArray8.selector, value));
        assertBoolAddressPairArray8TypedEquivalent(value);
    }

    function testEchoStringU64PairArray5Deterministic() public {
        FixedStringU64Pair[5] memory value;
        value[0] = FixedStringU64Pair({text: "hello with extra payload bytes beyond thirty-two", count: 7});
        value[1] = FixedStringU64Pair({text: "world with extra payload bytes beyond thirty-two", count: 11});
        value[2] = FixedStringU64Pair({text: "fixed with extra payload bytes beyond thirty-two", count: 13});
        value[3] = FixedStringU64Pair({text: "arrays with extra payload bytes beyond thirty-two", count: 17});
        value[4] = FixedStringU64Pair({text: "bench with extra payload bytes beyond thirty-two", count: 19});

        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoStringU64PairArray5.selector, value));
        assertStringU64PairArray5TypedEquivalent(value);
    }

    function testEchoStringU64PairArray5Fuzz(FixedStringU64Pair[5] memory value) public {
        value = normalizeStringU64PairArray5(value, 96);
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoStringU64PairArray5.selector, value));
        assertStringU64PairArray5TypedEquivalent(value);
    }

    function testEchoBytesU64PairArray5Deterministic() public {
        FixedBytesU64Pair[5] memory value;
        value[0] = FixedBytesU64Pair({data: hex"", count: 1});
        value[1] = FixedBytesU64Pair({data: hex"01", count: 2});
        value[2] = FixedBytesU64Pair({data: hex"deadbeef", count: 3});
        value[3] = FixedBytesU64Pair({data: bytes("payload"), count: 4});
        value[4] = FixedBytesU64Pair({data: hex"cafebabef00d", count: 5});

        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoBytesU64PairArray5.selector, value));
        assertBytesU64PairArray5TypedEquivalent(value);
    }

    function testEchoBytesU64PairArray5Fuzz(FixedBytesU64Pair[5] memory value) public {
        value = normalizeBytesU64PairArray5(value, 64);
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoBytesU64PairArray5.selector, value));
        assertBytesU64PairArray5TypedEquivalent(value);
    }

    function testEchoNestedUintArray2x5Deterministic() public {
        uint256[5][2] memory value;
        value[0] = [uint256(1), 2, 3, 4, 5];
        value[1] = [uint256(6), 7, 8, 9, 10];

        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoNestedUintArray2x5.selector, value));
        assertNestedUintArray2x5TypedEquivalent(value);
    }

    function testEchoNestedUintArray2x5Fuzz(uint256[5][2] memory value) public {
        assertEquivalent(abi.encodeWithSelector(IFixedArraySuite.echoNestedUintArray2x5.selector, value));
        assertNestedUintArray2x5TypedEquivalent(value);
    }

    function assertEquivalent(bytes memory callData) internal {
        (bool okSol, bytes memory outSol) = address(solTarget).call(callData);
        (bool okFe, bytes memory outFe) = feTarget.call(callData);

        require(okSol == okFe, "success mismatch");
        require(okSol, "call failed");
        require(keccak256(outSol) == keccak256(outFe), "return bytes mismatch");
    }

    function assertBoolArray5TypedEquivalent(bool[5] memory value) internal {
        bool[5] memory solValue = solBench.benchEchoBoolArray5(value);
        bool[5] memory feValue = feBench.benchEchoBoolArray5(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed bool[5] mismatch");
    }

    function assertBoolArray17TypedEquivalent(bool[17] memory value) internal {
        bool[17] memory solValue = solBench.benchEchoBoolArray17(value);
        bool[17] memory feValue = feBench.benchEchoBoolArray17(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed bool[17] mismatch");
    }

    function assertUintArray8TypedEquivalent(uint256[8] memory value) internal {
        uint256[8] memory solValue = solBench.benchEchoUintArray8(value);
        uint256[8] memory feValue = feBench.benchEchoUintArray8(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed uint256[8] mismatch");
    }

    function assertUintArray16TypedEquivalent(uint256[16] memory value) internal {
        uint256[16] memory solValue = solBench.benchEchoUintArray16(value);
        uint256[16] memory feValue = feBench.benchEchoUintArray16(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed uint256[16] mismatch");
    }

    function assertUintArray32TypedEquivalent(uint256[32] memory value) internal {
        uint256[32] memory solValue = solBench.benchEchoUintArray32(value);
        uint256[32] memory feValue = feBench.benchEchoUintArray32(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed uint256[32] mismatch");
    }

    function assertStringArray5TypedEquivalent(string[5] memory value) internal {
        string[5] memory solValue = solBench.benchEchoStringArray5(value);
        string[5] memory feValue = feBench.benchEchoStringArray5(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed string[5] mismatch");
    }

    function assertStringArray17TypedEquivalent(string[17] memory value) internal {
        string[17] memory solValue = solBench.benchEchoStringArray17(value);
        string[17] memory feValue = feBench.benchEchoStringArray17(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed string[17] mismatch");
    }

    function assertBytesArray5TypedEquivalent(bytes[5] memory value) internal {
        bytes[5] memory solValue = solBench.benchEchoBytesArray5(value);
        bytes[5] memory feValue = feBench.benchEchoBytesArray5(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed bytes[5] mismatch");
    }

    function assertBytesArray17TypedEquivalent(bytes[17] memory value) internal {
        bytes[17] memory solValue = solBench.benchEchoBytesArray17(value);
        bytes[17] memory feValue = feBench.benchEchoBytesArray17(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed bytes[17] mismatch");
    }

    function assertBoolAddressPairArray8TypedEquivalent(FixedBoolAddressPair[8] memory value) internal {
        FixedBoolAddressPair[8] memory solValue = solBench.benchEchoBoolAddressPairArray8(value);
        FixedBoolAddressPair[8] memory feValue = feBench.benchEchoBoolAddressPairArray8(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed (bool,address)[8] mismatch");
    }

    function assertStringU64PairArray5TypedEquivalent(FixedStringU64Pair[5] memory value) internal {
        FixedStringU64Pair[5] memory solValue = solBench.benchEchoStringU64PairArray5(value);
        FixedStringU64Pair[5] memory feValue = feBench.benchEchoStringU64PairArray5(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed (string,uint64)[5] mismatch");
    }

    function assertBytesU64PairArray5TypedEquivalent(FixedBytesU64Pair[5] memory value) internal {
        FixedBytesU64Pair[5] memory solValue = solBench.benchEchoBytesU64PairArray5(value);
        FixedBytesU64Pair[5] memory feValue = feBench.benchEchoBytesU64PairArray5(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed (bytes,uint64)[5] mismatch");
    }

    function assertNestedUintArray2x5TypedEquivalent(uint256[5][2] memory value) internal {
        uint256[5][2] memory solValue = solBench.benchEchoNestedUintArray2x5(value);
        uint256[5][2] memory feValue = feBench.benchEchoNestedUintArray2x5(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed uint256[5][2] mismatch");
    }

    function normalizeStringArray5(string[5] memory value, uint256 maxStringBytes)
        internal
        pure
        returns (string[5] memory out)
    {
        for (uint256 i = 0; i < 5; i++) {
            out[i] = truncateString(value[i], maxStringBytes);
        }
    }

    function normalizeStringArray17(string[17] memory value, uint256 maxStringBytes)
        internal
        pure
        returns (string[17] memory out)
    {
        for (uint256 i = 0; i < 17; i++) {
            out[i] = truncateString(value[i], maxStringBytes);
        }
    }

    function normalizeBytesArray5(bytes[5] memory value, uint256 maxBytes) internal pure returns (bytes[5] memory out) {
        for (uint256 i = 0; i < 5; i++) {
            out[i] = truncateBytes(value[i], maxBytes);
        }
    }

    function normalizeBytesArray17(bytes[17] memory value, uint256 maxBytes)
        internal
        pure
        returns (bytes[17] memory out)
    {
        for (uint256 i = 0; i < 17; i++) {
            out[i] = truncateBytes(value[i], maxBytes);
        }
    }

    function normalizeStringU64PairArray5(FixedStringU64Pair[5] memory value, uint256 maxStringBytes)
        internal
        pure
        returns (FixedStringU64Pair[5] memory out)
    {
        for (uint256 i = 0; i < 5; i++) {
            out[i] = FixedStringU64Pair({text: truncateString(value[i].text, maxStringBytes), count: value[i].count});
        }
    }

    function normalizeBytesU64PairArray5(FixedBytesU64Pair[5] memory value, uint256 maxBytes)
        internal
        pure
        returns (FixedBytesU64Pair[5] memory out)
    {
        for (uint256 i = 0; i < 5; i++) {
            out[i] = FixedBytesU64Pair({data: truncateBytes(value[i].data, maxBytes), count: value[i].count});
        }
    }

    function truncateString(string memory text, uint256 maxBytes) internal pure returns (string memory) {
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

    function deploy(bytes memory initCode) internal returns (address deployed) {
        assembly {
            deployed := create(0, add(initCode, 0x20), mload(initCode))
        }
    }

    function fromHex(string memory s) internal pure returns (bytes memory) {
        bytes memory strBytes = bytes(s);
        uint256 start = 0;
        while (start < strBytes.length && isWhitespace(strBytes[start])) {
            start++;
        }

        if (
            start + 1 < strBytes.length && strBytes[start] == bytes1("0")
                && (strBytes[start + 1] == bytes1("x") || strBytes[start + 1] == bytes1("X"))
        ) {
            start += 2;
        }

        uint256 digits = 0;
        for (uint256 i = start; i < strBytes.length; i++) {
            if (isWhitespace(strBytes[i])) continue;
            digits++;
        }
        require(digits % 2 == 0, "odd hex length");

        bytes memory out = new bytes(digits / 2);
        uint256 outIndex = 0;
        uint8 high = 0;
        bool highNibble = true;
        for (uint256 i = start; i < strBytes.length; i++) {
            bytes1 ch = strBytes[i];
            if (isWhitespace(ch)) continue;
            uint8 val = fromHexChar(ch);
            if (highNibble) {
                high = val;
                highNibble = false;
            } else {
                out[outIndex] = bytes1((high << 4) | val);
                outIndex++;
                highNibble = true;
            }
        }
        return out;
    }

    function isWhitespace(bytes1 ch) private pure returns (bool) {
        return ch == 0x20 || ch == 0x0a || ch == 0x0d || ch == 0x09;
    }

    function fromHexChar(bytes1 c) private pure returns (uint8) {
        uint8 b = uint8(c);
        if (b >= 48 && b <= 57) return b - 48;
        if (b >= 65 && b <= 70) return b - 55;
        if (b >= 97 && b <= 102) return b - 87;
        revert("invalid hex");
    }
}
