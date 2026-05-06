// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt184FuzzTest is AbiRoundtripBase {
    function testEchoInt184Fuzz(int184 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt184.selector, value);
        assertEquivalent(callData);
    }
}
