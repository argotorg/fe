// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiAddressFuzzTest is AbiRoundtripBase {
    function testEchoAddressFuzz(address value) public {
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoAddress.selector, value);
        assertEquivalent(callData);
    }
}
