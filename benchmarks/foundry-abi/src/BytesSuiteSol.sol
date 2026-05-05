// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

interface IBytesSuite {
    function echoBytes(bytes calldata value) external returns (bytes memory);
}

contract BytesSuiteSol is IBytesSuite {
    function echoBytes(bytes calldata value) external pure returns (bytes memory) {
        return value;
    }
}

contract BytesSolBenchCaller {
    IBytesSuite public immutable target;

    constructor(address target_) {
        target = IBytesSuite(target_);
    }

    function benchEchoBytes(bytes calldata value) external returns (bytes memory) {
        return target.echoBytes(value);
    }
}

contract BytesFeBenchCaller {
    IBytesSuite public immutable target;

    constructor(address target_) {
        target = IBytesSuite(target_);
    }

    function benchEchoBytes(bytes calldata value) external returns (bytes memory) {
        return target.echoBytes(value);
    }
}
