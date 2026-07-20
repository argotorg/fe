// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint184FuzzTest is AbiRoundtripBase {
    function testEchoUint184Fuzz(uint184 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint184.selector, value);
        assertEquivalent(callData);
    }
}
