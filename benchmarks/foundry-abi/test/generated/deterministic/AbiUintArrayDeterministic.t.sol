// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUintArrayDeterministicTest is AbiRoundtripBase {
    function testEchoUintArrayDeterministic0() public {
        uint256[] memory value = new uint256[](0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUintArray.selector, value);
        assertEquivalent(callData);
    }

    function testEchoUintArrayDeterministic1() public {
        uint256[] memory value = new uint256[](3);
        value[0] = uint256(0);
        value[1] = uint256(1);
        value[2] = type(uint256).max;
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUintArray.selector, value);
        assertEquivalent(callData);
    }
}
