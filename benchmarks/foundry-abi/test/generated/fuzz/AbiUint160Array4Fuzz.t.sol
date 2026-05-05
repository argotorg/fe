// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint160Array4FuzzTest is AbiRoundtripBase {
    function testEchoUint160Array4Fuzz(uint160[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint160Array4.selector, value);
        assertEquivalent(callData);
    }
}
