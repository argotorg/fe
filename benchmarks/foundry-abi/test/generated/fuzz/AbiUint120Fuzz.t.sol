// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint120FuzzTest is AbiRoundtripBase {
    function testEchoUint120Fuzz(uint120 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint120.selector, value);
        assertEquivalent(callData);
    }
}
