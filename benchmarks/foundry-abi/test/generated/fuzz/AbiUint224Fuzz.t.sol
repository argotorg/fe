// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint224FuzzTest is AbiRoundtripBase {
    function testEchoUint224Fuzz(uint224 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint224.selector, value);
        assertEquivalent(callData);
    }
}
