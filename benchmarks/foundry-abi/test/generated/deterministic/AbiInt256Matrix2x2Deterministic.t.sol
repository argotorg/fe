// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt256Matrix2x2DeterministicTest is AbiRoundtripBase {
    function testEchoInt256Matrix2x2Deterministic0() public {
        int256[2][2] memory value = [[int256(0), int256(-1)], [type(int256).min, type(int256).max]];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt256Matrix2x2.selector, value);
        assertEquivalent(callData);
    }

    function testEchoInt256Matrix2x2Deterministic1() public {
        int256[2][2] memory value = [[int256(-1), type(int256).min], [type(int256).max, int256(0)]];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt256Matrix2x2.selector, value);
        assertEquivalent(callData);
    }
}
