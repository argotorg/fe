// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint56FuzzTest is AbiRoundtripBase {
    function testEchoUint56Fuzz(uint56 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint56.selector, value);
        assertEquivalent(callData);
    }
}
