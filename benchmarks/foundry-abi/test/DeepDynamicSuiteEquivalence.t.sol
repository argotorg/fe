// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {
    BytesU64Pair,
    DeepDynamicFeBenchCaller,
    DeepDynamicSolBenchCaller,
    DeepDynamicSuiteSol,
    IDeepDynamicSuite,
    StringU64PairDyn
} from "../src/DeepDynamicSuiteSol.sol";

interface Vm {
    function readFile(string calldata path) external returns (string memory);
    function assume(bool condition) external;
}

contract DeepDynamicSuiteEquivalenceTest {
    Vm constant vm = Vm(address(uint160(uint256(keccak256("hevm cheat code")))));

    DeepDynamicSuiteSol internal solTarget;
    address internal feTarget;
    DeepDynamicSolBenchCaller internal solBench;
    DeepDynamicFeBenchCaller internal feBench;

    function setUp() public {
        solTarget = new DeepDynamicSuiteSol();
        feTarget = deploy(fromHex(vm.readFile("fe-out/DeepDynamicSuite.bin")));
        require(feTarget != address(0), "fe create failed");

        solBench = new DeepDynamicSolBenchCaller(address(solTarget));
        feBench = new DeepDynamicFeBenchCaller(feTarget);
    }

    function testEchoUint24ArrayDeterministic() public {
        uint24[] memory value = new uint24[](3);
        value[0] = 0;
        value[1] = 1;
        value[2] = type(uint24).max;

        bytes memory callData = abi.encodeWithSelector(IDeepDynamicSuite.echoUint24Array.selector, value);
        assertEquivalent(callData);

        uint24[] memory solValue = solBench.benchEchoUint24Array(value);
        uint24[] memory feValue = feBench.benchEchoUint24Array(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed uint24[] mismatch");
    }

    function testEchoUint24ArrayFuzz(uint24[] memory value) public {
        value = truncateUint24Array(value, 4);
        bytes memory callData = abi.encodeWithSelector(IDeepDynamicSuite.echoUint24Array.selector, value);
        assertEquivalent(callData);
    }

    function testEchoBytesArrayDeterministic() public {
        bytes[] memory value = new bytes[](3);
        value[0] = hex"";
        value[1] = hex"00112233";
        value[2] =
            hex"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";

        bytes memory callData = abi.encodeWithSelector(IDeepDynamicSuite.echoBytesArray.selector, value);
        assertEquivalent(callData);

        bytes[] memory solValue = solBench.benchEchoBytesArray(value);
        bytes[] memory feValue = feBench.benchEchoBytesArray(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed bytes[] mismatch");
    }

    function testEchoBytesArrayFuzz(bytes[] memory value) public {
        value = truncateBytesArray(value, 3, 96);
        bytes memory callData = abi.encodeWithSelector(IDeepDynamicSuite.echoBytesArray.selector, value);
        assertEquivalent(callData);
        assertBytesArrayTypedEquivalent(value);
    }

    function testEchoNestedBytesArrayDeterministic() public {
        bytes[][] memory value = new bytes[][](3);
        value[0] = new bytes[](2);
        value[0][0] = hex"";
        value[0][1] = hex"00112233";
        value[1] = new bytes[](0);
        value[2] = new bytes[](2);
        value[2][0] = hex"101112131415161718191a1b1c1d1e1f";
        value[2][1] =
            hex"202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f";

        bytes memory callData =
            abi.encodeWithSelector(IDeepDynamicSuite.echoNestedBytesArray.selector, value);
        assertEquivalent(callData);
        assertNestedBytesArrayTypedEquivalent(value);
    }

    function testEchoNestedBytesArrayFuzz(bytes[][] memory value) public {
        value = truncateBytesMatrix(value, 3, 3, 96);
        bytes memory callData =
            abi.encodeWithSelector(IDeepDynamicSuite.echoNestedBytesArray.selector, value);
        assertEquivalent(callData);
        assertNestedBytesArrayTypedEquivalent(value);
    }

    function testEchoNestedUintArrayDeterministic() public {
        uint256[][] memory value = new uint256[][](3);
        value[0] = new uint256[](2);
        value[0][0] = 1;
        value[0][1] = 2;
        value[1] = new uint256[](0);
        value[2] = new uint256[](1);
        value[2][0] = type(uint256).max;

        bytes memory callData =
            abi.encodeWithSelector(IDeepDynamicSuite.echoNestedUintArray.selector, value);
        assertEquivalent(callData);

        uint256[][] memory solValue = solBench.benchEchoNestedUintArray(value);
        uint256[][] memory feValue = feBench.benchEchoNestedUintArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed uint256[][] mismatch"
        );
    }

    function testEchoNestedUintArrayFuzz(uint256[][] memory value) public {
        value = truncateUintMatrix(value, 3, 3);
        bytes memory callData =
            abi.encodeWithSelector(IDeepDynamicSuite.echoNestedUintArray.selector, value);
        assertEquivalent(callData);
        assertNestedUintArrayTypedEquivalent(value);
    }

    function testEchoNestedStringArrayDeterministic() public {
        string[][] memory value = new string[][](3);
        value[0] = new string[](2);
        value[0][0] = "alpha";
        value[0][1] = "beta with extra payload bytes beyond thirty-two";
        value[1] = new string[](0);
        value[2] = new string[](1);
        value[2][0] = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

        bytes memory callData =
            abi.encodeWithSelector(IDeepDynamicSuite.echoNestedStringArray.selector, value);
        assertEquivalent(callData);

        string[][] memory solValue = solBench.benchEchoNestedStringArray(value);
        string[][] memory feValue = feBench.benchEchoNestedStringArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed string[][] mismatch"
        );
    }

    function testEchoNestedStringArrayFuzz(string[][] memory value) public {
        value = truncateStringMatrix(value, 3, 3, 96);
        bytes memory callData =
            abi.encodeWithSelector(IDeepDynamicSuite.echoNestedStringArray.selector, value);
        assertEquivalent(callData);
        assertNestedStringArrayTypedEquivalent(value);
    }

    function testEchoNestedStringU64PairArrayDeterministic() public {
        StringU64PairDyn[][] memory value = new StringU64PairDyn[][](2);
        value[0] = new StringU64PairDyn[](2);
        value[0][0] = StringU64PairDyn({text: "pair-one-with-extra-payload-beyond-thirty-two", count: 1});
        value[0][1] = StringU64PairDyn({text: "pair-two-with-extra-payload-beyond-thirty-two", count: 2});
        value[1] = new StringU64PairDyn[](1);
        value[1][0] = StringU64PairDyn({text: "tail-with-extra-payload-beyond-thirty-two", count: type(uint64).max});

        bytes memory callData =
            abi.encodeWithSelector(IDeepDynamicSuite.echoNestedStringU64PairArray.selector, value);
        assertEquivalent(callData);

        StringU64PairDyn[][] memory solValue = solBench.benchEchoNestedStringU64PairArray(value);
        StringU64PairDyn[][] memory feValue = feBench.benchEchoNestedStringU64PairArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed (string,uint64)[][] mismatch"
        );
    }

    function testEchoNestedStringU64PairArrayFuzz(StringU64PairDyn[][] memory value) public {
        value = truncateStringU64PairMatrix(value, 3, 3, 96);
        bytes memory callData =
            abi.encodeWithSelector(IDeepDynamicSuite.echoNestedStringU64PairArray.selector, value);
        assertEquivalent(callData);
        assertNestedStringU64PairArrayTypedEquivalent(value);
    }

    function testEchoBytesU64PairDeterministic() public {
        BytesU64Pair memory value = BytesU64Pair({
            data: hex"00112233445566778899aabbccddeeff1011121314151617",
            count: 77
        });

        bytes memory callData = abi.encodeWithSelector(IDeepDynamicSuite.echoBytesU64Pair.selector, value);
        assertEquivalent(callData);

        BytesU64Pair memory solValue = solBench.benchEchoBytesU64Pair(value);
        BytesU64Pair memory feValue = feBench.benchEchoBytesU64Pair(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed (bytes,uint64) mismatch"
        );
    }

    function testEchoBytesU64PairFuzz(bytes memory data, uint64 count) public {
        BytesU64Pair memory value = BytesU64Pair({data: truncateBytes(data, 96), count: count});
        bytes memory callData = abi.encodeWithSelector(IDeepDynamicSuite.echoBytesU64Pair.selector, value);
        assertEquivalent(callData);
        assertBytesU64PairTypedEquivalent(value);
    }

    function testEchoBytesU64PairArrayDeterministic() public {
        BytesU64Pair[] memory value = new BytesU64Pair[](2);
        value[0] = BytesU64Pair({data: hex"00112233", count: 11});
        value[1] = BytesU64Pair({
            data: hex"101112131415161718191a1b1c1d1e1f2021222324252627",
            count: 22
        });

        bytes memory callData =
            abi.encodeWithSelector(IDeepDynamicSuite.echoBytesU64PairArray.selector, value);
        assertEquivalent(callData);

        BytesU64Pair[] memory solValue = solBench.benchEchoBytesU64PairArray(value);
        BytesU64Pair[] memory feValue = feBench.benchEchoBytesU64PairArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed (bytes,uint64)[] mismatch"
        );
    }

    function testEchoBytesU64PairArrayFuzz(BytesU64Pair[] memory value) public {
        value = truncateBytesU64PairArray(value, 3, 96);
        bytes memory callData =
            abi.encodeWithSelector(IDeepDynamicSuite.echoBytesU64PairArray.selector, value);
        assertEquivalent(callData);
        assertBytesU64PairArrayTypedEquivalent(value);
    }

    function testEchoNestedBytesU64PairArrayDeterministic() public {
        BytesU64Pair[][] memory value = new BytesU64Pair[][](3);
        value[0] = new BytesU64Pair[](2);
        value[0][0] = BytesU64Pair({data: hex"00112233", count: 11});
        value[0][1] = BytesU64Pair({data: hex"", count: 12});
        value[1] = new BytesU64Pair[](0);
        value[2] = new BytesU64Pair[](2);
        value[2][0] = BytesU64Pair({
            data: hex"101112131415161718191a1b1c1d1e1f2021222324252627",
            count: 21
        });
        value[2][1] = BytesU64Pair({
            data: hex"303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f",
            count: type(uint64).max
        });

        bytes memory callData =
            abi.encodeWithSelector(IDeepDynamicSuite.echoNestedBytesU64PairArray.selector, value);
        assertEquivalent(callData);
        assertNestedBytesU64PairArrayTypedEquivalent(value);
    }

    function testEchoNestedBytesU64PairArrayFuzz(BytesU64Pair[][] memory value) public {
        value = truncateBytesU64PairMatrix(value, 3, 3, 96);
        bytes memory callData =
            abi.encodeWithSelector(IDeepDynamicSuite.echoNestedBytesU64PairArray.selector, value);
        assertEquivalent(callData);
        assertNestedBytesU64PairArrayTypedEquivalent(value);
    }

    function assertEquivalent(bytes memory callData) internal {
        (bool okSol, bytes memory outSol) = address(solTarget).call(callData);
        (bool okFe, bytes memory outFe) = feTarget.call(callData);

        require(okSol == okFe, "success mismatch");
        require(okSol, "call failed");
        require(keccak256(outSol) == keccak256(outFe), "return bytes mismatch");
    }

    function assertBytesArrayTypedEquivalent(bytes[] memory value) internal {
        bytes[] memory solValue = solBench.benchEchoBytesArray(value);
        bytes[] memory feValue = feBench.benchEchoBytesArray(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed bytes[] mismatch");
    }

    function assertNestedBytesArrayTypedEquivalent(bytes[][] memory value) internal {
        bytes[][] memory solValue = solBench.benchEchoNestedBytesArray(value);
        bytes[][] memory feValue = feBench.benchEchoNestedBytesArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed bytes[][] mismatch"
        );
    }

    function assertNestedUintArrayTypedEquivalent(uint256[][] memory value) internal {
        uint256[][] memory solValue = solBench.benchEchoNestedUintArray(value);
        uint256[][] memory feValue = feBench.benchEchoNestedUintArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed uint256[][] mismatch"
        );
    }

    function assertNestedStringArrayTypedEquivalent(string[][] memory value) internal {
        string[][] memory solValue = solBench.benchEchoNestedStringArray(value);
        string[][] memory feValue = feBench.benchEchoNestedStringArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed string[][] mismatch"
        );
    }

    function assertNestedStringU64PairArrayTypedEquivalent(StringU64PairDyn[][] memory value) internal {
        StringU64PairDyn[][] memory solValue = solBench.benchEchoNestedStringU64PairArray(value);
        StringU64PairDyn[][] memory feValue = feBench.benchEchoNestedStringU64PairArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed (string,uint64)[][] mismatch"
        );
    }

    function assertBytesU64PairTypedEquivalent(BytesU64Pair memory value) internal {
        BytesU64Pair memory solValue = solBench.benchEchoBytesU64Pair(value);
        BytesU64Pair memory feValue = feBench.benchEchoBytesU64Pair(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed (bytes,uint64) mismatch"
        );
    }

    function assertBytesU64PairArrayTypedEquivalent(BytesU64Pair[] memory value) internal {
        BytesU64Pair[] memory solValue = solBench.benchEchoBytesU64PairArray(value);
        BytesU64Pair[] memory feValue = feBench.benchEchoBytesU64PairArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed (bytes,uint64)[] mismatch"
        );
    }

    function assertNestedBytesU64PairArrayTypedEquivalent(BytesU64Pair[][] memory value) internal {
        BytesU64Pair[][] memory solValue = solBench.benchEchoNestedBytesU64PairArray(value);
        BytesU64Pair[][] memory feValue = feBench.benchEchoNestedBytesU64PairArray(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed (bytes,uint64)[][] mismatch"
        );
    }

    function truncateUint24Array(uint24[] memory value, uint256 maxLen)
        internal
        pure
        returns (uint24[] memory out)
    {
        uint256 len = value.length;
        if (len > maxLen) len = maxLen;

        out = new uint24[](len);
        for (uint256 i = 0; i < len; i++) {
            out[i] = value[i];
        }
    }

    function truncateBytesArray(bytes[] memory value, uint256 maxLen, uint256 maxBytesLen)
        internal
        pure
        returns (bytes[] memory out)
    {
        uint256 len = value.length;
        if (len > maxLen) len = maxLen;

        out = new bytes[](len);
        for (uint256 i = 0; i < len; i++) {
            out[i] = truncateBytes(value[i], maxBytesLen);
        }
    }

    function truncateBytesMatrix(
        bytes[][] memory value,
        uint256 maxOuterLen,
        uint256 maxInnerLen,
        uint256 maxBytesLen
    ) internal pure returns (bytes[][] memory out) {
        uint256 outerLen = value.length;
        if (outerLen > maxOuterLen) outerLen = maxOuterLen;

        out = new bytes[][](outerLen);
        for (uint256 i = 0; i < outerLen; i++) {
            out[i] = truncateBytesArray(value[i], maxInnerLen, maxBytesLen);
        }
    }

    function truncateUintMatrix(uint256[][] memory value, uint256 maxOuterLen, uint256 maxInnerLen)
        internal
        pure
        returns (uint256[][] memory out)
    {
        uint256 outerLen = value.length;
        if (outerLen > maxOuterLen) outerLen = maxOuterLen;

        out = new uint256[][](outerLen);
        for (uint256 i = 0; i < outerLen; i++) {
            uint256 innerLen = value[i].length;
            if (innerLen > maxInnerLen) innerLen = maxInnerLen;

            out[i] = new uint256[](innerLen);
            for (uint256 j = 0; j < innerLen; j++) {
                out[i][j] = value[i][j];
            }
        }
    }

    function truncateStringMatrix(
        string[][] memory value,
        uint256 maxOuterLen,
        uint256 maxInnerLen,
        uint256 maxStringBytes
    ) internal pure returns (string[][] memory out) {
        uint256 outerLen = value.length;
        if (outerLen > maxOuterLen) outerLen = maxOuterLen;

        out = new string[][](outerLen);
        for (uint256 i = 0; i < outerLen; i++) {
            uint256 innerLen = value[i].length;
            if (innerLen > maxInnerLen) innerLen = maxInnerLen;

            out[i] = new string[](innerLen);
            for (uint256 j = 0; j < innerLen; j++) {
                out[i][j] = truncateString(value[i][j], maxStringBytes);
            }
        }
    }

    function truncateStringU64PairMatrix(
        StringU64PairDyn[][] memory value,
        uint256 maxOuterLen,
        uint256 maxInnerLen,
        uint256 maxStringBytes
    ) internal pure returns (StringU64PairDyn[][] memory out) {
        uint256 outerLen = value.length;
        if (outerLen > maxOuterLen) outerLen = maxOuterLen;

        out = new StringU64PairDyn[][](outerLen);
        for (uint256 i = 0; i < outerLen; i++) {
            uint256 innerLen = value[i].length;
            if (innerLen > maxInnerLen) innerLen = maxInnerLen;

            out[i] = new StringU64PairDyn[](innerLen);
            for (uint256 j = 0; j < innerLen; j++) {
                out[i][j] = StringU64PairDyn({
                    text: truncateString(value[i][j].text, maxStringBytes),
                    count: value[i][j].count
                });
            }
        }
    }

    function truncateBytesU64PairArray(
        BytesU64Pair[] memory value,
        uint256 maxLen,
        uint256 maxBytesLen
    ) internal pure returns (BytesU64Pair[] memory out) {
        uint256 len = value.length;
        if (len > maxLen) len = maxLen;

        out = new BytesU64Pair[](len);
        for (uint256 i = 0; i < len; i++) {
            out[i] = BytesU64Pair({
                data: truncateBytes(value[i].data, maxBytesLen),
                count: value[i].count
            });
        }
    }

    function truncateBytesU64PairMatrix(
        BytesU64Pair[][] memory value,
        uint256 maxOuterLen,
        uint256 maxInnerLen,
        uint256 maxBytesLen
    ) internal pure returns (BytesU64Pair[][] memory out) {
        uint256 outerLen = value.length;
        if (outerLen > maxOuterLen) outerLen = maxOuterLen;

        out = new BytesU64Pair[][](outerLen);
        for (uint256 i = 0; i < outerLen; i++) {
            out[i] = truncateBytesU64PairArray(value[i], maxInnerLen, maxBytesLen);
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
