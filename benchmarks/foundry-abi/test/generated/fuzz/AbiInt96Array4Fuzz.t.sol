// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt96Array4FuzzTest is AbiRoundtripBase {
    function testEchoInt96Array4Fuzz(int96[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt96Array4.selector, value);
        assertEquivalent(callData);
    }
}
