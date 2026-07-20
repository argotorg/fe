// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt200FuzzTest is AbiRoundtripBase {
    function testEchoInt200Fuzz(int200 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt200.selector, value);
        assertEquivalent(callData);
    }
}
