// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt144FuzzTest is AbiRoundtripBase {
    function testEchoInt144Fuzz(int144 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt144.selector, value);
        assertEquivalent(callData);
    }
}
