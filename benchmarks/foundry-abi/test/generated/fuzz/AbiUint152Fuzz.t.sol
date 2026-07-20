// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint152FuzzTest is AbiRoundtripBase {
    function testEchoUint152Fuzz(uint152 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint152.selector, value);
        assertEquivalent(callData);
    }
}
