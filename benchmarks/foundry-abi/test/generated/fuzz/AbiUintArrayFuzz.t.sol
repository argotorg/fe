// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiUintArrayFuzzTest is AbiRoundtripBase {
    function testEchoUintArrayFuzz(uint256[] memory value) public {
        vm.assume(value.length <= 4);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoUintArray.selector, value);
        assertEquivalent(callData);
    }
}
