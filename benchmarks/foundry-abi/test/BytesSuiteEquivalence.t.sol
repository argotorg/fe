// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {BytesFeBenchCaller, BytesSolBenchCaller, BytesSuiteSol, IBytesSuite} from "../src/BytesSuiteSol.sol";

interface Vm {
    function readFile(string calldata path) external returns (string memory);
    function assume(bool condition) external;
}

contract BytesSuiteEquivalenceTest {
    Vm constant vm = Vm(address(uint160(uint256(keccak256("hevm cheat code")))));

    BytesSuiteSol internal solTarget;
    address internal feTarget;
    BytesSolBenchCaller internal solBench;
    BytesFeBenchCaller internal feBench;

    function setUp() public {
        solTarget = new BytesSuiteSol();
        feTarget = deploy(fromHex(vm.readFile("fe-out/BytesSuite.bin")));
        require(feTarget != address(0), "fe create failed");

        solBench = new BytesSolBenchCaller(address(solTarget));
        feBench = new BytesFeBenchCaller(feTarget);
    }

    function testEchoBytesDeterministicShort() public {
        bytes memory value = hex"00112233445566778899aabbccddeeff";
        assertEquivalent(abi.encodeWithSelector(IBytesSuite.echoBytes.selector, value));

        assertTypedEquivalent(value);
    }

    function testEchoBytesDeterministicLong() public {
        bytes memory value =
            hex"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f3031323334353637";
        assertEquivalent(abi.encodeWithSelector(IBytesSuite.echoBytes.selector, value));

        assertTypedEquivalent(value);
    }

    function testEchoBytesDeterministicWordBoundaries() public {
        bytes memory value31 = hex"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e";
        bytes memory value32 = hex"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";
        bytes memory value33 =
            hex"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20";

        assertEquivalent(abi.encodeWithSelector(IBytesSuite.echoBytes.selector, value31));
        assertTypedEquivalent(value31);

        assertEquivalent(abi.encodeWithSelector(IBytesSuite.echoBytes.selector, value32));
        assertTypedEquivalent(value32);

        assertEquivalent(abi.encodeWithSelector(IBytesSuite.echoBytes.selector, value33));
        assertTypedEquivalent(value33);
    }

    function testEchoBytesFuzz(bytes memory value) public {
        value = truncateBytes(value, 96);
        assertEquivalent(abi.encodeWithSelector(IBytesSuite.echoBytes.selector, value));
        assertTypedEquivalent(value);
    }

    function assertEquivalent(bytes memory callData) internal {
        (bool okSol, bytes memory outSol) = address(solTarget).call(callData);
        (bool okFe, bytes memory outFe) = feTarget.call(callData);

        require(okSol == okFe, "success mismatch");
        require(okSol, "call failed");
        require(keccak256(outSol) == keccak256(outFe), "return bytes mismatch");
    }

    function deploy(bytes memory initCode) internal returns (address deployed) {
        assembly {
            deployed := create(0, add(initCode, 0x20), mload(initCode))
        }
    }

    function assertTypedEquivalent(bytes memory value) internal {
        bytes memory solValue = solBench.benchEchoBytes(value);
        bytes memory feValue = feBench.benchEchoBytes(value);
        require(keccak256(solValue) == keccak256(feValue), "typed bytes mismatch");
    }

    function truncateBytes(bytes memory value, uint256 maxLen) internal pure returns (bytes memory out) {
        uint256 len = value.length;
        if (len > maxLen) len = maxLen;

        out = new bytes(len);
        for (uint256 i = 0; i < len; i++) {
            out[i] = value[i];
        }
    }

    function fromHex(string memory s) internal pure returns (bytes memory) {
        bytes memory strBytes = bytes(s);
        uint256 start = 0;
        while (start < strBytes.length && isWhitespace(strBytes[start])) {
            start++;
        }

        if (
            start + 1 < strBytes.length &&
            strBytes[start] == bytes1("0") &&
            (strBytes[start + 1] == bytes1("x") || strBytes[start + 1] == bytes1("X"))
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
