// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiAddressArray4FuzzTest is AbiRoundtripBase {
    function testEchoAddressArray4Fuzz(address[4] memory value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoAddressArray4.selector, value);
        assertEquivalent(callData);
    }
}
