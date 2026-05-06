// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint72FuzzTest is AbiRoundtripBase {
    function testEchoUint72Fuzz(uint72 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint72.selector, value);
        assertEquivalent(callData);
    }
}
