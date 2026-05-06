// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint160FuzzTest is AbiRoundtripBase {
    function testEchoUint160Fuzz(uint160 value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint160.selector, value);
        assertEquivalent(callData);
    }
}
