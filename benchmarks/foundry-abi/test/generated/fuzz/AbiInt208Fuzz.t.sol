// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt208FuzzTest is AbiRoundtripBase {
    function testEchoInt208Fuzz(int208 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt208.selector, value);
        assertEquivalent(callData);
    }
}
