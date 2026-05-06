// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint136FuzzTest is AbiRoundtripBase {
    function testEchoUint136Fuzz(uint136 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint136.selector, value);
        assertEquivalent(callData);
    }
}
