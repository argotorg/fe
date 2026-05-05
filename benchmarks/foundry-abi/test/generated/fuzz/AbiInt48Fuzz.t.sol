// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt48FuzzTest is AbiRoundtripBase {
    function testEchoInt48Fuzz(int48 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt48.selector, value);
        assertEquivalent(callData);
    }
}
