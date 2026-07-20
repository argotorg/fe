// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt96FuzzTest is AbiRoundtripBase {
    function testEchoInt96Fuzz(int96 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt96.selector, value);
        assertEquivalent(callData);
    }
}
