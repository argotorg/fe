// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt128Array4FuzzTest is AbiRoundtripBase {
    function testEchoInt128Array4Fuzz(int128[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt128Array4.selector, value);
        assertEquivalent(callData);
    }
}
