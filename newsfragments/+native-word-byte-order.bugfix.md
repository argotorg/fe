Native code now loads and stores `std::evm` memory words big-endian, as the EVM does. A custom error's selector was lost from native revert data, and an encoded string literal had a garbled length.
