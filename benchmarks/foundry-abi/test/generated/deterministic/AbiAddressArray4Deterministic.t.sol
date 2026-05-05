// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiAddressArray4DeterministicTest is AbiRoundtripBase {
    function testEchoAddressArray4Deterministic0() public {
        address[4] memory value = [address(0), address(0x1000000000000000000000000000000000000001), address(0xFFfFfFffFFfffFFfFFfFFFFFffFFFffffFfFFFfF), address(0)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoAddressArray4.selector, value);
        assertEquivalent(callData);
    }

    function testEchoAddressArray4Deterministic1() public {
        address[4] memory value = [address(0x1000000000000000000000000000000000000001), address(0xFFfFfFffFFfffFFfFFfFFFFFffFFFffffFfFFFfF), address(0), address(0x1000000000000000000000000000000000000001)];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoAddressArray4.selector, value);
        assertEquivalent(callData);
    }
}
