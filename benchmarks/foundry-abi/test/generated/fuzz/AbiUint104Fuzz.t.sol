// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint104FuzzTest is AbiRoundtripBase {
    function testEchoUint104Fuzz(uint104 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint104.selector, value);
        assertEquivalent(callData);
    }
}
