// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUint96Array4FuzzTest is AbiRoundtripBase {
    function testEchoUint96Array4Fuzz(uint96[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUint96Array4.selector, value);
        assertEquivalent(callData);
    }
}
