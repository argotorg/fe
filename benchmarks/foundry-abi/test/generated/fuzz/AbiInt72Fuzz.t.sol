// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt72FuzzTest is AbiRoundtripBase {
    function testEchoInt72Fuzz(int72 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt72.selector, value);
        assertEquivalent(callData);
    }
}
