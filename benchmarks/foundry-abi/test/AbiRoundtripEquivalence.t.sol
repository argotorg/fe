// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {
    AbiRoundtripSol,
    FeBenchCaller,
    IAbiRoundtrip,
    SolBenchCaller,
    StringU64Pair
} from "../src/AbiRoundtripSol.sol";

interface Vm {
    function readFile(string calldata path) external returns (string memory);
}

contract AbiRoundtripEquivalenceTest {
    Vm constant vm = Vm(address(uint160(uint256(keccak256("hevm cheat code")))));

    AbiRoundtripSol internal solTarget;
    address internal feTarget;
    SolBenchCaller internal solBench;
    FeBenchCaller internal feBench;

    function setUp() public {
        solTarget = new AbiRoundtripSol();
        feTarget = deploy(fromHex(vm.readFile("fe-out/AbiRoundtripFe.bin")));
        require(feTarget != address(0), "fe create failed");

        solBench = new SolBenchCaller(address(solTarget));
        feBench = new FeBenchCaller(feTarget);
    }

    function testEchoUintEquivalence() public {
        bytes memory callData =
            abi.encodeWithSelector(IAbiRoundtrip.echoUint.selector, uint256(0x123456789abcdef));
        assertEquivalent(callData);
    }

    function testEchoStringEquivalence() public {
        bytes memory callData =
            abi.encodeWithSelector(IAbiRoundtrip.echoString.selector, string("hello roundtrip"));
        assertEquivalent(callData);
    }

    function testEchoStringBoundaryEquivalence() public {
        string memory text = "0123456789abcdefghijklmnopqrstuv";
        require(bytes(text).length == 32, "expected 32-byte fixture");
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoString.selector, text);
        assertEquivalent(callData);
    }

    function testEchoPairEquivalence() public {
        StringU64Pair memory pair = StringU64Pair({text: "pair payload", count: 42});
        bytes memory callData = abi.encodeWithSelector(
            IAbiRoundtrip.echoPair.selector,
            pair
        );
        assertEquivalent(callData);
    }

    function testBenchEchoUint() public {
        uint256 value = 77;
        require(solBench.benchEchoUint(value) == value, "sol uint");
        require(feBench.benchEchoUint(value) == value, "fe uint");
    }

    function testBenchEchoString() public {
        string memory text = "benchmark string";
        require(
            keccak256(bytes(solBench.benchEchoString(text))) == keccak256(bytes(text)),
            "sol string"
        );
        require(
            keccak256(bytes(feBench.benchEchoString(text))) == keccak256(bytes(text)),
            "fe string"
        );
    }

    function testBenchEchoPair() public {
        StringU64Pair memory pair = StringU64Pair({text: "bench pair", count: 99});

        StringU64Pair memory solValue = solBench.benchEchoPair(pair);
        require(keccak256(abi.encode(solValue)) == keccak256(abi.encode(pair)), "sol value");

        StringU64Pair memory feValue = feBench.benchEchoPair(pair);
        require(keccak256(abi.encode(feValue)) == keccak256(abi.encode(pair)), "fe value");
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
