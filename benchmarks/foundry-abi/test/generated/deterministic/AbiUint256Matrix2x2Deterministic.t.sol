// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint256Matrix2x2DeterministicTest is AbiRoundtripBase {
    function testEchoUintMatrix2x2Deterministic0() public {
        uint256[2][2] memory value = [[uint256(0), uint256(1)], [type(uint256).max, uint256(0)]];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUintMatrix2x2.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUintMatrix2x2Deterministic1() public {
        uint256[2][2] memory value = [[uint256(1), type(uint256).max], [uint256(0), uint256(1)]];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUintMatrix2x2.selector, value);
        assertEquivalent(callData);
    }
}
