// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint144FuzzTest is AbiRoundtripBase {
    function testEchoUint144Fuzz(uint144 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint144.selector, value);
        assertEquivalent(callData);
    }
}
