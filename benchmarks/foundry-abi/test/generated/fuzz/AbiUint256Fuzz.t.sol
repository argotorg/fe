// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint256FuzzTest is AbiRoundtripBase {
    function testEchoUintFuzz(uint256 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint.selector, value);
        assertEquivalent(callData);
    }
}
