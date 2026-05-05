// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiInt128FuzzTest is AbiRoundtripBase {
    function testEchoInt128Fuzz(int128 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoInt128.selector, value);
        assertEquivalent(callData);
    }
}
