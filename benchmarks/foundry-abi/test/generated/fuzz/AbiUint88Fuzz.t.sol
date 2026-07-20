// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint88FuzzTest is AbiRoundtripBase {
    function testEchoUint88Fuzz(uint88 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint88.selector, value);
        assertEquivalent(callData);
    }
}
