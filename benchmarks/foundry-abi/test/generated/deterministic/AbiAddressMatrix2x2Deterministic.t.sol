// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";
import {IAbiRoundtrip} from "../../../src/AbiRoundtripSol.sol";

contract AbiAddressMatrix2x2DeterministicTest is AbiRoundtripBase {
    function testEchoAddressMatrix2x2Deterministic0() public {
        address[2][2] memory value = [[address(0), address(0x1000000000000000000000000000000000000001)], [address(0xFFfFfFffFFfffFFfFFfFFFFFffFFFffffFfFFFfF), address(0)]];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoAddressMatrix2x2.selector, value);
        assertEquivalent(callData);
    }

    function testEchoAddressMatrix2x2Deterministic1() public {
        address[2][2] memory value = [[address(0x1000000000000000000000000000000000000001), address(0xFFfFfFffFFfffFFfFFfFFFFFffFFFffffFfFFFfF)], [address(0), address(0x1000000000000000000000000000000000000001)]];
        bytes memory callData = abi.encodeWithSelector(IAbiRoundtrip.echoAddressMatrix2x2.selector, value);
        assertEquivalent(callData);
    }
}
