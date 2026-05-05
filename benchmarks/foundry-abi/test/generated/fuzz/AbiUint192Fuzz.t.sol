// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint192FuzzTest is AbiRoundtripBase {
    function testEchoUint192Fuzz(uint192 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint192.selector, value);
        assertEquivalent(callData);
    }
}
