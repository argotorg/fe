// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint24Array4FuzzTest is AbiRoundtripBase {
    function testEchoUint24Array4Fuzz(uint24[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint24Array4.selector, value);
        assertEquivalent(callData);
    }
}
