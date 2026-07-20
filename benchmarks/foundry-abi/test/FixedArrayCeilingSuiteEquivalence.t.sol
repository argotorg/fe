// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {
    FixedArrayCeilingSuiteSol,
    FixedArrayCeilingSolBenchCaller,
    FixedArrayCeilingFeBenchCaller,
    IFixedArrayCeilingSuite
} from "../src/FixedArrayCeilingSuiteSol.sol";

interface Vm {
    function readFile(string calldata path) external returns (string memory);
}

contract FixedArrayCeilingSuiteEquivalenceTest {
    Vm constant vm = Vm(address(uint160(uint256(keccak256("hevm cheat code")))));

    FixedArrayCeilingSuiteSol internal solTarget;
    address internal feTarget;
    FixedArrayCeilingSolBenchCaller internal solBench;
    FixedArrayCeilingFeBenchCaller internal feBench;

    function setUp() public {
        solTarget = new FixedArrayCeilingSuiteSol();
        feTarget = deploy(fromHex(vm.readFile("fe-out/FixedArrayCeilingSuite.bin")));
        require(feTarget != address(0), "fe create failed");

        solBench = new FixedArrayCeilingSolBenchCaller(address(solTarget));
        feBench = new FixedArrayCeilingFeBenchCaller(feTarget);
    }

    function testEchoBoolArray17Deterministic() public {
        bool[17] memory value;
        for (uint256 i = 0; i < 17; i++) {
            value[i] = i % 2 == 0;
        }

        assertEquivalent(abi.encodeWithSelector(IFixedArrayCeilingSuite.echoBoolArray17.selector, value));
        assertBoolArray17TypedEquivalent(value);
    }

    function testEchoBoolArray17Fuzz(bool[17] memory value) public {
        assertEquivalent(abi.encodeWithSelector(IFixedArrayCeilingSuite.echoBoolArray17.selector, value));
        assertBoolArray17TypedEquivalent(value);
    }

    function testEchoUintArray32Deterministic() public {
        uint256[32] memory value;
        for (uint256 i = 0; i < 32; i++) {
            value[i] = i + 1;
        }
        value[31] = type(uint256).max;

        assertEquivalent(abi.encodeWithSelector(IFixedArrayCeilingSuite.echoUintArray32.selector, value));
        assertUintArray32TypedEquivalent(value);
    }

    function testEchoUintArray32Fuzz(uint256[32] memory value) public {
        assertEquivalent(abi.encodeWithSelector(IFixedArrayCeilingSuite.echoUintArray32.selector, value));
        assertUintArray32TypedEquivalent(value);
    }

    function testEchoStringArray17Deterministic() public {
        string[17] memory value;
        value[0] = "";
        value[1] = "alpha";
        value[2] = "beta gamma with extra payload bytes beyond thirty-two";
        value[8] = "delta with extra payload bytes beyond thirty-two";
        value[9] = "epsilon with extra payload bytes beyond thirty-two";
        value[16] = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

        assertEquivalent(abi.encodeWithSelector(IFixedArrayCeilingSuite.echoStringArray17.selector, value));
        assertStringArray17TypedEquivalent(value);
    }

    function testEchoStringArray17Fuzz(string[17] memory value) public {
        value = normalizeStringArray17(value, 96);
        assertEquivalent(abi.encodeWithSelector(IFixedArrayCeilingSuite.echoStringArray17.selector, value));
        assertStringArray17TypedEquivalent(value);
    }

    function testEchoBytesArray17Deterministic() public {
        bytes[17] memory value;
        value[0] = hex"";
        value[1] = hex"01";
        value[2] = hex"deadbeef";
        value[8] = bytes("payload");
        value[9] = hex"ffffffffffffffffffffffffffffffff";
        value[16] = hex"00112233445566778899aabbccddeeff";

        assertEquivalent(abi.encodeWithSelector(IFixedArrayCeilingSuite.echoBytesArray17.selector, value));
        assertBytesArray17TypedEquivalent(value);
    }

    function testEchoBytesArray17Fuzz(bytes[17] memory value) public {
        value = normalizeBytesArray17(value, 64);
        assertEquivalent(abi.encodeWithSelector(IFixedArrayCeilingSuite.echoBytesArray17.selector, value));
        assertBytesArray17TypedEquivalent(value);
    }

    function assertEquivalent(bytes memory callData) internal {
        (bool okSol, bytes memory outSol) = address(solTarget).call(callData);
        (bool okFe, bytes memory outFe) = feTarget.call(callData);

        require(okSol == okFe, "success mismatch");
        require(okSol, "call failed");
        require(keccak256(outSol) == keccak256(outFe), "return bytes mismatch");
    }

    function assertBoolArray17TypedEquivalent(bool[17] memory value) internal {
        bool[17] memory solValue = solBench.benchEchoBoolArray17(value);
        bool[17] memory feValue = feBench.benchEchoBoolArray17(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed bool[17] mismatch");
    }

    function assertUintArray32TypedEquivalent(uint256[32] memory value) internal {
        uint256[32] memory solValue = solBench.benchEchoUintArray32(value);
        uint256[32] memory feValue = feBench.benchEchoUintArray32(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed uint256[32] mismatch");
    }

    function assertStringArray17TypedEquivalent(string[17] memory value) internal {
        string[17] memory solValue = solBench.benchEchoStringArray17(value);
        string[17] memory feValue = feBench.benchEchoStringArray17(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed string[17] mismatch");
    }

    function assertBytesArray17TypedEquivalent(bytes[17] memory value) internal {
        bytes[17] memory solValue = solBench.benchEchoBytesArray17(value);
        bytes[17] memory feValue = feBench.benchEchoBytesArray17(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed bytes[17] mismatch");
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

    function normalizeBytesArray17(bytes[17] memory value, uint256 maxBytes)
        internal
        pure
        returns (bytes[17] memory out)
    {
        for (uint256 i = 0; i < 17; i++) {
            out[i] = truncateBytes(value[i], maxBytes);
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
