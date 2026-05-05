// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {
    INestedTupleSuite,
    NestedTupleFeBenchCaller,
    NestedTupleSolBenchCaller,
    NestedTupleSuiteSol,
    NTAddressU256Pair,
    NTBoolAddressPair,
    NTBytesBoolPair,
    NTNestedDynamic,
    NTNestedDynamicBoth,
    NTNestedStatic,
    NTNestedStaticFlipped,
    NTStringU64Pair
} from "../src/NestedTupleSuiteSol.sol";

interface Vm {
    function readFile(string calldata path) external returns (string memory);
    function assume(bool condition) external;
}

contract NestedTupleSuiteEquivalenceTest {
    Vm constant vm = Vm(address(uint160(uint256(keccak256("hevm cheat code")))));

    NestedTupleSuiteSol internal solTarget;
    address internal feTarget;
    NestedTupleSolBenchCaller internal solBench;
    NestedTupleFeBenchCaller internal feBench;

    function setUp() public {
        solTarget = new NestedTupleSuiteSol();
        feTarget = deploy(fromHex(vm.readFile("fe-out/NestedTupleSuite.bin")));
        require(feTarget != address(0), "fe create failed");

        solBench = new NestedTupleSolBenchCaller(address(solTarget));
        feBench = new NestedTupleFeBenchCaller(feTarget);
    }

    function testEchoNestedStaticDeterministic() public {
        NTNestedStatic memory value = NTNestedStatic({
            inner: NTBoolAddressPair({flag: true, addr: address(0x1000000000000000000000000000000000000001)}),
            count: 123
        });

        assertEquivalent(abi.encodeWithSelector(INestedTupleSuite.echoNestedStatic.selector, value));
        assertNestedStaticTypedEquivalent(value);
    }

    function testEchoNestedStaticFuzz(bool innerFlag, address innerAddr, uint256 count) public {
        NTNestedStatic memory value =
            NTNestedStatic({inner: NTBoolAddressPair({flag: innerFlag, addr: innerAddr}), count: count});
        assertEquivalent(abi.encodeWithSelector(INestedTupleSuite.echoNestedStatic.selector, value));
    }

    function testEchoNestedStaticFlippedDeterministic() public {
        NTNestedStaticFlipped memory value = NTNestedStaticFlipped({
            flag: false,
            inner: NTAddressU256Pair({addr: address(0x2000000000000000000000000000000000000002), count: type(uint256).max})
        });

        assertEquivalent(abi.encodeWithSelector(INestedTupleSuite.echoNestedStaticFlipped.selector, value));
        assertNestedStaticFlippedTypedEquivalent(value);
    }

    function testEchoNestedStaticFlippedFuzz(bool flag, address innerAddr, uint256 innerCount) public {
        NTNestedStaticFlipped memory value =
            NTNestedStaticFlipped({flag: flag, inner: NTAddressU256Pair({addr: innerAddr, count: innerCount})});
        assertEquivalent(abi.encodeWithSelector(INestedTupleSuite.echoNestedStaticFlipped.selector, value));
    }

    function testEchoNestedDynamicDeterministic() public {
        NTNestedDynamic memory value = NTNestedDynamic({
            pair: NTStringU64Pair({
                text: "alpha with extra payload bytes beyond thirty-two",
                count: type(uint64).max
            }),
            flag: true
        });

        assertEquivalent(abi.encodeWithSelector(INestedTupleSuite.echoNestedDynamic.selector, value));
        assertNestedDynamicTypedEquivalent(value);
    }

    function testEchoNestedDynamicFuzz(string memory text, uint64 count, bool flag) public {
        assumeShortString(text, 96);
        NTNestedDynamic memory value = NTNestedDynamic({pair: NTStringU64Pair({text: text, count: count}), flag: flag});
        assertEquivalent(abi.encodeWithSelector(INestedTupleSuite.echoNestedDynamic.selector, value));
    }

    function testEchoNestedDynamicBothDeterministic() public {
        NTNestedDynamicBoth memory value = NTNestedDynamicBoth({
            left: NTStringU64Pair({
                text: "bravo with extra payload bytes beyond thirty-two",
                count: 42
            }),
            right: NTBytesBoolPair({
                data: hex"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20212223",
                flag: false
            })
        });

        assertEquivalent(abi.encodeWithSelector(INestedTupleSuite.echoNestedDynamicBoth.selector, value));
        assertNestedDynamicBothTypedEquivalent(value);
    }

    function testEchoNestedDynamicBothFuzz(string memory text, uint64 count, bytes memory data, bool flag) public {
        assumeShortString(text, 96);
        data = truncateBytes(data, 96);
        NTNestedDynamicBoth memory value = NTNestedDynamicBoth({
            left: NTStringU64Pair({text: text, count: count}),
            right: NTBytesBoolPair({data: data, flag: flag})
        });
        assertEquivalent(abi.encodeWithSelector(INestedTupleSuite.echoNestedDynamicBoth.selector, value));
    }

    function testEchoNestedStaticArrayDeterministic() public {
        NTNestedStatic[4] memory value;
        value[0] = NTNestedStatic({inner: NTBoolAddressPair({flag: false, addr: address(0)}), count: 0});
        value[1] = NTNestedStatic({
            inner: NTBoolAddressPair({flag: true, addr: address(0x3000000000000000000000000000000000000003)}),
            count: 1
        });
        value[2] = NTNestedStatic({
            inner: NTBoolAddressPair({flag: false, addr: address(0x4000000000000000000000000000000000000004)}),
            count: type(uint256).max
        });
        value[3] = NTNestedStatic({
            inner: NTBoolAddressPair({flag: true, addr: address(0x5000000000000000000000000000000000000005)}),
            count: 999
        });

        assertEquivalent(abi.encodeWithSelector(INestedTupleSuite.echoNestedStaticArray.selector, value));
        assertNestedStaticArrayTypedEquivalent(value);
    }

    function testEchoNestedStaticDynArrayDeterministic() public {
        NTNestedStatic[] memory value = new NTNestedStatic[](3);
        value[0] = NTNestedStatic({inner: NTBoolAddressPair({flag: false, addr: address(0)}), count: 0});
        value[1] = NTNestedStatic({
            inner: NTBoolAddressPair({flag: true, addr: address(0x6000000000000000000000000000000000000006)}),
            count: 123
        });
        value[2] = NTNestedStatic({
            inner: NTBoolAddressPair({flag: false, addr: address(0x7000000000000000000000000000000000000007)}),
            count: type(uint256).max
        });

        assertEquivalent(abi.encodeWithSelector(INestedTupleSuite.echoNestedStaticDynArray.selector, value));
        assertNestedStaticDynArrayTypedEquivalent(value);
    }

    function testEchoNestedStaticDynArrayFuzz(NTNestedStatic[] memory value) public {
        value = truncateNestedStaticDynArray(value, 4);
        assertEquivalent(abi.encodeWithSelector(INestedTupleSuite.echoNestedStaticDynArray.selector, value));
    }

    function assertEquivalent(bytes memory callData) internal {
        (bool okSol, bytes memory outSol) = address(solTarget).call(callData);
        (bool okFe, bytes memory outFe) = feTarget.call(callData);

        require(okSol == okFe, "success mismatch");
        require(okSol, "call failed");
        require(keccak256(outSol) == keccak256(outFe), "return bytes mismatch");
    }

    function assertNestedStaticTypedEquivalent(NTNestedStatic memory value) internal {
        NTNestedStatic memory solValue = solBench.benchEchoNestedStatic(value);
        NTNestedStatic memory feValue = feBench.benchEchoNestedStatic(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed nested static mismatch");
    }

    function assertNestedStaticFlippedTypedEquivalent(NTNestedStaticFlipped memory value) internal {
        NTNestedStaticFlipped memory solValue = solBench.benchEchoNestedStaticFlipped(value);
        NTNestedStaticFlipped memory feValue = feBench.benchEchoNestedStaticFlipped(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed nested static flipped mismatch"
        );
    }

    function assertNestedDynamicTypedEquivalent(NTNestedDynamic memory value) internal {
        NTNestedDynamic memory solValue = solBench.benchEchoNestedDynamic(value);
        NTNestedDynamic memory feValue = feBench.benchEchoNestedDynamic(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed nested dynamic mismatch");
    }

    function assertNestedDynamicBothTypedEquivalent(NTNestedDynamicBoth memory value) internal {
        NTNestedDynamicBoth memory solValue = solBench.benchEchoNestedDynamicBoth(value);
        NTNestedDynamicBoth memory feValue = feBench.benchEchoNestedDynamicBoth(value);
        require(
            keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)),
            "typed nested dynamic both mismatch"
        );
    }

    function assertNestedStaticArrayTypedEquivalent(NTNestedStatic[4] memory value) internal {
        NTNestedStatic[4] memory solValue = solBench.benchEchoNestedStaticArray(value);
        NTNestedStatic[4] memory feValue = feBench.benchEchoNestedStaticArray(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed nested static[4] mismatch");
    }

    function assertNestedStaticDynArrayTypedEquivalent(NTNestedStatic[] memory value) internal {
        NTNestedStatic[] memory solValue = solBench.benchEchoNestedStaticDynArray(value);
        NTNestedStatic[] memory feValue = feBench.benchEchoNestedStaticDynArray(value);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(feValue)), "typed nested static[] mismatch");
    }

    function assumeShortString(string memory text, uint256 maxBytes) internal {
        vm.assume(bytes(text).length <= maxBytes);
    }

    function truncateNestedStaticDynArray(NTNestedStatic[] memory value, uint256 maxLen)
        internal
        pure
        returns (NTNestedStatic[] memory out)
    {
        uint256 len = value.length;
        if (len > maxLen) len = maxLen;

        out = new NTNestedStatic[](len);
        for (uint256 i = 0; i < len; i++) {
            out[i] = value[i];
        }
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

