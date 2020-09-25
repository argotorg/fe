contract Foo:
    pub bar: map<u256, u256>

    pub def read_bar(key: u256) -> u256:
        return self.bar[key]

    pub def write_bar(key: u256, value: u256):
        self.bar[key] = value