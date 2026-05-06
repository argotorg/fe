// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint168FuzzTest is AbiRoundtripBase {
    function testEchoUint168Fuzz(uint168 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint168.selector, value);
        assertEquivalent(callData);
    }
}
