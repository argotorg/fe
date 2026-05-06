// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint24FuzzTest is AbiRoundtripBase {
    function testEchoUint24Fuzz(uint24 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24.selector, value);
        assertEquivalent(callData);
    }
}
