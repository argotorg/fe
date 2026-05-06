// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint24Matrix2x2DeterministicTest is AbiRoundtripBase {
    function testEchoUint24Matrix2x2Deterministic0() public {
        uint24[2][2] memory value = [[uint24(0), uint24(1)], [type(uint24).max, uint24(0)]];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24Matrix2x2.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUint24Matrix2x2Deterministic1() public {
        uint24[2][2] memory value = [[uint24(1), type(uint24).max], [uint24(0), uint24(1)]];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24Matrix2x2.selector, value);
        assertEquivalent(callData);
    }
}
