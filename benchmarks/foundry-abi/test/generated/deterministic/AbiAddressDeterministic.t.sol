// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiAddressDeterministicTest is AbiRoundtripBase {
    function testEchoAddressDeterministic0() public {
        address value = address(0);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoAddress.selector, value);
        assertEquivalent(callData);
    }

    function testEchoAddressDeterministic1() public {
        address value = address(0x1000000000000000000000000000000000000001);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoAddress.selector, value);
        assertEquivalent(callData);
    }

    function testEchoAddressDeterministic2() public {
        address value = address(0xFFfFfFffFFfffFFfFFfFFFFFffFFFffffFfFFFfF);
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoAddress.selector, value);
        assertEquivalent(callData);
    }
}
