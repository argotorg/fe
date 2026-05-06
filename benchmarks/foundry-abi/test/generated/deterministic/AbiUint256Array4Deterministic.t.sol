// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint256Array4DeterministicTest is AbiRoundtripBase {
    function testEchoUintArray4Deterministic0() public {
        uint256[4] memory value = [uint256(0), uint256(1), type(uint256).max, uint256(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUintArray4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUintArray4Deterministic1() public {
        uint256[4] memory value = [uint256(1), type(uint256).max, uint256(0), uint256(1)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUintArray4.selector, value);
        assertEquivalent(callData);
    }
}
