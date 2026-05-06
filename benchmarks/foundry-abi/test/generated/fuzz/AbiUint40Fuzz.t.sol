// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint40FuzzTest is AbiRoundtripBase {
    function testEchoUint40Fuzz(uint40 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint40.selector, value);
        assertEquivalent(callData);
    }
}
