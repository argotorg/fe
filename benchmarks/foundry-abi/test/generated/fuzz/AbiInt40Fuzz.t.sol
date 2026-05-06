// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt40FuzzTest is AbiRoundtripBase {
    function testEchoInt40Fuzz(int40 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt40.selector, value);
        assertEquivalent(callData);
    }
}
