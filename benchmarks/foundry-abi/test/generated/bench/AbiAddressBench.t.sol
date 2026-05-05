// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import {AbiRoundtripBase} from "../support/AbiRoundtripBase.sol";

contract AbiAddressBenchTest is AbiRoundtripBase {
    function testBenchEchoAddress() public {
        address value = address(0x2000000000000000000000000000000000000002);
        require(solBench.benchEchoAddress(value) == value, "sol value");
        require(feBench.benchEchoAddress(value) == value, "fe value");
    }
}
