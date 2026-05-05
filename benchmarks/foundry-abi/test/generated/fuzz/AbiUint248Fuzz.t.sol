// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint248FuzzTest is AbiRoundtripBase {
    function testEchoUint248Fuzz(uint248 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint248.selector, value);
        assertEquivalent(callData);
    }
}
