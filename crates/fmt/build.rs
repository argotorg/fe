fn main() {
    #[cfg(test)]
    println!("cargo:rerun-if-changed=./tests/fixtures");
}
