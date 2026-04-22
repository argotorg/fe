// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract Bench {
    mapping(uint256 => uint256) public data;

    function store(uint256 key, uint256 value) public {
        data[key] = value;
    }

    function load(uint256 key) public view returns (uint256) {
        return data[key];
    }

    function storeAndLoad(uint256 key, uint256 value) public returns (uint256) {
        data[key] = value;
        return data[key];
    }
}
