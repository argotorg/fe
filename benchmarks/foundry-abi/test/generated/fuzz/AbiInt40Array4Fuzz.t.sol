// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt40Array4FuzzTest is AbiRoundtripBase {
    function testEchoInt40Array4Fuzz(int40[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt40Array4.selector, value);
        assertEquivalent(callData);
    }
}
