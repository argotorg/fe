// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt80FuzzTest is AbiRoundtripBase {
    function testEchoInt80Fuzz(int80 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt80.selector, value);
        assertEquivalent(callData);
    }
}
